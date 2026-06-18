# =============================================================================
# 03_fuel_forecast.R
# Fuel Price Forecasting + Scenario Analysis
#
# Fits ARIMAX models for gasoline and diesel retail prices
# using WTI crude as primary regressor.
# Generates 4-week and 13-week forward forecasts with confidence intervals.
# Produces three scenario forecasts: Conservative / Base / Supply Disruption.
#
# Inputs:  data/cache/fuel_weekly.csv
# Outputs: data/cache/fuel_forecasts.csv
#          data/cache/fuel_scenarios.csv
#          data/cache/fuel_model_accuracy.csv
# =============================================================================

library(tidyverse)
library(lubridate)
library(tsibble)
library(fable)
library(fabletools)
library(feasts)

source("config.R")

# -----------------------------------------------------------------------------
# Load data
# -----------------------------------------------------------------------------

cat("Loading fuel data...\n")

fuel_weekly <- read_csv("data/cache/fuel_weekly.csv",
                         show_col_types = FALSE) %>%
  mutate(date = as.Date(date))

# Remove rows with NA in target variables
fuel_clean <- fuel_weekly %>%
  filter(!is.na(gasoline_retail), !is.na(diesel_retail), !is.na(wti_crude)) %>%
  mutate(week_index = yearweek(date))

cat(sprintf("✓ Loaded %d weeks of fuel data\n", nrow(fuel_clean)))

# -----------------------------------------------------------------------------
# Train / holdout split
# -----------------------------------------------------------------------------

HOLDOUT_WEEKS <- 13

all_weeks   <- sort(unique(fuel_clean$week_index))
n_weeks     <- length(all_weeks)
train_end   <- all_weeks[n_weeks - HOLDOUT_WEEKS]

train_ts    <- fuel_clean %>%
  filter(week_index <= train_end) %>%
  as_tsibble(index = week_index)

holdout_ts  <- fuel_clean %>%
  filter(week_index > train_end) %>%
  as_tsibble(index = week_index)

# -----------------------------------------------------------------------------
# Fit models — Gasoline
# -----------------------------------------------------------------------------

cat("\nFitting gasoline price models...\n")

fit_gasoline <- train_ts %>%
  model(
    snaive = SNAIVE(gasoline_retail),
    arima  = ARIMA(gasoline_retail, stepwise = TRUE),
    arimax = ARIMA(gasoline_retail ~ wti_lag1, stepwise = TRUE)
  )

# Evaluate on holdout
fc_gas_holdout <- fit_gasoline %>%
  forecast(new_data = holdout_ts)

accuracy_gas <- fc_gas_holdout %>%
  accuracy(holdout_ts) %>%
  select(.model, RMSE, MAE, MAPE) %>%
  mutate(target = "gasoline")

cat("Gasoline model accuracy (holdout):\n")
print(accuracy_gas)

# Select best model
best_gas_model <- accuracy_gas %>%
  slice_min(RMSE, n = 1) %>%
  pull(.model)

cat(sprintf("Best gasoline model: %s\n", best_gas_model))

# -----------------------------------------------------------------------------
# Fit models — Diesel
# -----------------------------------------------------------------------------

cat("\nFitting diesel price models...\n")

fit_diesel <- train_ts %>%
  model(
    snaive = SNAIVE(diesel_retail),
    arima  = ARIMA(diesel_retail, stepwise = TRUE),
    arimax = ARIMA(diesel_retail ~ wti_lag1, stepwise = TRUE)
  )

fc_diesel_holdout <- fit_diesel %>%
  forecast(new_data = holdout_ts)

accuracy_diesel <- fc_diesel_holdout %>%
  accuracy(holdout_ts) %>%
  select(.model, RMSE, MAE, MAPE) %>%
  mutate(target = "diesel")

cat("Diesel model accuracy (holdout):\n")
print(accuracy_diesel)

best_diesel_model <- accuracy_diesel %>%
  slice_min(RMSE, n = 1) %>%
  pull(.model)

cat(sprintf("Best diesel model: %s\n", best_diesel_model))

# -----------------------------------------------------------------------------
# Refit on full data
# -----------------------------------------------------------------------------

cat("\nRefitting on full dataset...\n")

full_ts <- fuel_clean %>% as_tsibble(index = week_index)

final_gasoline <- full_ts %>%
  model(
    snaive = SNAIVE(gasoline_retail),
    arima  = ARIMA(gasoline_retail, stepwise = TRUE),
    arimax = ARIMA(gasoline_retail ~ wti_lag1, stepwise = TRUE)
  )

final_diesel <- full_ts %>%
  model(
    snaive = SNAIVE(diesel_retail),
    arima  = ARIMA(diesel_retail, stepwise = TRUE),
    arimax = ARIMA(diesel_retail ~ wti_lag1, stepwise = TRUE)
  )

# -----------------------------------------------------------------------------
# Generate forward scenarios
# WTI crude trajectory drives three scenario paths
# -----------------------------------------------------------------------------

cat("\nGenerating scenario forecasts...\n")

last_date    <- max(fuel_clean$date)
last_wti     <- fuel_clean %>% slice_max(date, n = 1) %>% pull(wti_crude)
last_gas_inv <- fuel_clean %>% slice_max(date, n = 1) %>% pull(gas_inv_vs_avg)
last_dist_inv <- fuel_clean %>% slice_max(date, n = 1) %>% pull(distillate_inv_vs_avg)
last_summer  <- fuel_clean %>% slice_max(date, n = 1) %>% pull(summer_blend)
last_winter  <- fuel_clean %>% slice_max(date, n = 1) %>% pull(winter_heating)

build_scenario_future <- function(n_weeks, wti_multiplier, scenario_name) {
  future_dates <- seq.Date(last_date + 7, by = "week", length.out = n_weeks)
  future_wti   <- last_wti * (1 + wti_multiplier)

  tibble(
    date                 = future_dates,
    week_index           = yearweek(future_dates),
    scenario             = scenario_name,
    wti_crude            = future_wti,
    wti_lag1             = lag(c(last_wti, rep(future_wti, n_weeks - 1)), 1),
    wti_lag2             = lag(c(last_wti, last_wti, rep(future_wti, n_weeks - 2)), 2),
    gas_inv_vs_avg       = last_gas_inv,
    distillate_inv_vs_avg = last_dist_inv,
    summer_blend         = as.integer(month(future_dates) %in% 4:9),
    winter_heating       = as.integer(month(future_dates) %in% c(10:12, 1:2))
  ) %>%
    fill(wti_lag1, wti_lag2, .direction = "down") %>%
    as_tsibble(index = week_index)
}

scenarios <- list(
  conservative = build_scenario_future(FUEL_HORIZON_13W, SCENARIO_CONSERVATIVE, "Conservative"),
  base         = build_scenario_future(FUEL_HORIZON_13W, SCENARIO_BASE,         "Base"),
  disruption   = build_scenario_future(FUEL_HORIZON_13W, SCENARIO_DISRUPTION,   "Supply Disruption")
)

# Generate forecasts for each scenario
generate_scenario_forecasts <- function(model_fit, target_var, best_model) {
  map_dfr(names(scenarios), function(s) {
    fc <- model_fit %>%
      select(all_of(best_model)) %>%
      forecast(new_data = scenarios[[s]])

    fc %>%
      as_tibble() %>%
      mutate(
        scenario      = scenarios[[s]]$scenario[1],
        target        = target_var,
        forecast_date = as.Date(week_index),
        point_forecast = round(.mean, 3),
        lo_80 = round(quantile(.data[[target_var]], 0.10), 3),
        hi_80 = round(quantile(.data[[target_var]], 0.90), 3)
      ) %>%
      select(scenario, target, forecast_date, point_forecast, lo_80, hi_80)
  })
}

# Scenarios are defined by the WTI crude path, so the fan MUST be generated by the
# WTI-aware model ("arimax"). If the holdout-best model is snaive or arima (no WTI
# regressor), it ignores the scenario WTI multipliers and collapses all three paths
# to an identical forecast — which is what flattened the chart. best_*_model is still
# computed above for logging/accuracy reporting; it just doesn't drive the fan.
gas_scenarios    <- generate_scenario_forecasts(final_gasoline, "gasoline_retail", "arimax")
diesel_scenarios <- generate_scenario_forecasts(final_diesel,   "diesel_retail",   "arimax")

fuel_scenarios <- bind_rows(gas_scenarios, diesel_scenarios)
fuel_scenarios <- fuel_scenarios %>% mutate(point_forecast = as.numeric(point_forecast))

# -----------------------------------------------------------------------------
# Base case forecasts with confidence intervals (for main chart)
# -----------------------------------------------------------------------------

base_future_ts <- scenarios$base

fc_gas_base <- final_gasoline %>%
  select(all_of(best_gas_model)) %>%
  forecast(new_data = base_future_ts)

fc_diesel_base <- final_diesel %>%
  select(all_of(best_diesel_model)) %>%
  forecast(new_data = base_future_ts)

extract_fc <- function(fc_obj, target_name) {
  fc_obj %>%
    as_tibble() %>%
    mutate(
      target         = target_name,
      forecast_date  = as.Date(week_index),
      point_forecast = round(.mean, 3),
      lo_80 = round(quantile(.data[[target_name]], 0.10), 3),
      hi_80 = round(quantile(.data[[target_name]], 0.90), 3),
      lo_95 = round(quantile(.data[[target_name]], 0.025), 3),
      hi_95 = round(quantile(.data[[target_name]], 0.975), 3)
    ) %>%
    select(target, forecast_date, point_forecast, lo_80, hi_80, lo_95, hi_95)
}

fuel_forecasts <- bind_rows(
  extract_fc(fc_gas_base,    "gasoline_retail"),
  extract_fc(fc_diesel_base, "diesel_retail")
)

# -----------------------------------------------------------------------------
# Print scenario summary
# -----------------------------------------------------------------------------

cat("\n═══════════════════════════════════════\n")
cat("FUEL PRICE SCENARIOS (13-Week Avg)\n")
cat("═══════════════════════════════════════\n")

fuel_scenarios %>%
  group_by(scenario, target) %>%
  summarise(avg_price = round(mean(point_forecast, na.rm=TRUE), 3), .groups = "drop") %>%
  pivot_wider(names_from = target, values_from = avg_price) %>%
  print()

cat("═══════════════════════════════════════\n")

# Combine accuracy
model_accuracy <- bind_rows(accuracy_gas, accuracy_diesel) %>%
  mutate(
    best_gas_model    = best_gas_model,
    best_diesel_model = best_diesel_model
  )

# -----------------------------------------------------------------------------
# Write outputs
# -----------------------------------------------------------------------------

write_csv(fuel_forecasts,  "data/cache/fuel_forecasts.csv")
write_csv(fuel_scenarios,  "data/cache/fuel_scenarios.csv")
write_csv(model_accuracy,  "data/cache/fuel_model_accuracy.csv")

cat("\n✓ Fuel forecasting complete\n")
