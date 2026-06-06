# 02_ingest_grocery_data.R
library(tidyverse)
library(lubridate)
library(fredr)
library(zoo)

source("config.R")
fredr_set_key(FRED_API_KEY)

START_DATE    <- as.Date("2018-01-01")
END_DATE      <- Sys.Date()

monthly_dates <- seq.Date(
  from = floor_date(START_DATE, "month"),
  to   = floor_date(END_DATE, "month"),
  by   = "month"
)

pull_monthly <- function(series_id, series_name) {
  cat(sprintf("  Pulling %s (%s)...\n", series_name, series_id))
  tryCatch({
    fredr(series_id = series_id,
          observation_start = START_DATE,
          observation_end   = END_DATE,
          frequency         = "m") %>%
      select(date, value) %>%
      rename(!!series_name := value) %>%
      mutate(date = as.Date(date))
  }, error = function(e) {
    warning(sprintf("Failed: %s — %s", series_id, e$message))
    NULL
  })
}

cat("Pulling grocery and food price series...\n")

grocery_series <- list(
  pull_monthly("CPIFABSL",        "cpi_food_home"),
  pull_monthly("CPIUFDNS",        "cpi_food_away"),
  pull_monthly("CUSR0000SAF111",  "cpi_cereals"),
  pull_monthly("CUSR0000SAF112",  "cpi_meats"),
  pull_monthly("CUSR0000SAF113",  "cpi_dairy"),
  pull_monthly("CUSR0000SAF114",  "cpi_fruits_veg"),
  pull_monthly("PPIACO",          "ppi_food_mfg"),
  pull_monthly("PPIITM",          "ppi_trucking"),
  pull_monthly("MHHNGSP",         "natural_gas"),
  pull_monthly("GASDESW",         "diesel_monthly"),
  pull_monthly("DCOILWTICO",      "wti_monthly")
)

grocery_monthly <- reduce(compact(grocery_series), full_join, by = "date") %>%
  filter(date %in% monthly_dates) %>%
  arrange(date)

cat(sprintf("\n✓ Grocery dataset: %d months x %d series\n",
            nrow(grocery_monthly), ncol(grocery_monthly) - 1))

# -----------------------------------------------------------------------------
# Feature engineering — all in one clean mutate
# -----------------------------------------------------------------------------

grocery_monthly <- grocery_monthly %>%
  arrange(date) %>%
  mutate(
    cpi_food_home_yoy    = (cpi_food_home  / lag(cpi_food_home,  12) - 1) * 100,
    cpi_cereals_yoy      = (cpi_cereals    / lag(cpi_cereals,    12) - 1) * 100,
    cpi_meats_yoy        = (cpi_meats      / lag(cpi_meats,      12) - 1) * 100,
    cpi_dairy_yoy        = (cpi_dairy      / lag(cpi_dairy,      12) - 1) * 100,
    cpi_fruits_veg_yoy   = (cpi_fruits_veg / lag(cpi_fruits_veg, 12) - 1) * 100,
    cpi_food_home_mom    = (cpi_food_home  / lag(cpi_food_home,   1) - 1) * 100,
    diesel_3m_chg        = diesel_monthly - lag(diesel_monthly, 3),
    diesel_yoy_chg       = diesel_monthly - lag(diesel_monthly, 12),
    wti_3m_chg           = wti_monthly - lag(wti_monthly, 3),
    wti_yoy_chg          = wti_monthly - lag(wti_monthly, 12),
    ppi_food_mfg_yoy     = (ppi_food_mfg  / lag(ppi_food_mfg,  12) - 1) * 100,
    ppi_trucking_yoy     = (ppi_trucking  / lag(ppi_trucking,  12) - 1) * 100,
    nat_gas_3m_chg       = natural_gas - lag(natural_gas, 3),
    ag_input_combined    = (wti_yoy_chg * 0.7) + (nat_gas_3m_chg * 0.3)
  )

# -----------------------------------------------------------------------------
# GPPI scoring function
# -----------------------------------------------------------------------------

normalize_score <- function(x, window = 60) {
  n <- length(x)
  if (all(is.na(x))) return(rep(0, n))
  scores <- numeric(n)
  for (i in seq_along(x)) {
    if (is.na(x[i])) { scores[i] <- NA; next }
    lookback <- x[max(1, i - window):i]
    lookback <- lookback[!is.na(lookback)]
    if (length(lookback) < 10) { scores[i] <- 0; next }
    pct_rank <- mean(lookback <= x[i])
    scores[i] <- (pct_rank - 0.5) * 4
  }
  scores
}

# -----------------------------------------------------------------------------
# Compute GPPI scores
# -----------------------------------------------------------------------------

gppi_scores <- grocery_monthly %>%
  mutate(
    score_transportation = normalize_score(diesel_yoy_chg),
    score_agricultural   = normalize_score(ag_input_combined),
    score_manufacturing  = normalize_score(ppi_food_mfg_yoy),
    score_freight        = normalize_score(ppi_trucking_yoy),
    gppi_composite = (
      coalesce(score_transportation, 0) * GPPI_WEIGHTS$transportation +
      coalesce(score_agricultural,   0) * GPPI_WEIGHTS$agricultural   +
      coalesce(score_manufacturing,  0) * GPPI_WEIGHTS$manufacturing  +
      coalesce(score_freight,        0) * GPPI_WEIGHTS$freight
    ),
    gppi_signal = case_when(
      gppi_composite >  GPPI_HIGH_PRESSURE ~ "HIGH PRESSURE — Significant grocery inflation likely ahead",
      gppi_composite >  GPPI_MOD_PRESSURE  ~ "MODERATE PRESSURE — Food prices likely to rise modestly",
      gppi_composite >= GPPI_NEUTRAL_LOW   ~ "NEUTRAL — Food prices likely to track recent trend",
      TRUE                                  ~ "RELIEF BUILDING — Food price inflation likely to moderate"
    ),
    gppi_color = case_when(
      gppi_composite >  GPPI_HIGH_PRESSURE ~ "red",
      gppi_composite >  GPPI_MOD_PRESSURE  ~ "orange",
      gppi_composite >= GPPI_NEUTRAL_LOW   ~ "yellow",
      TRUE                                  ~ "green"
    )
  ) %>%
  select(date, starts_with("score_"), gppi_composite, gppi_signal, gppi_color,
         cpi_food_home_yoy, cpi_cereals_yoy, cpi_meats_yoy,
         cpi_dairy_yoy, cpi_fruits_veg_yoy,
         diesel_monthly, wti_monthly, ppi_food_mfg_yoy, ppi_trucking_yoy)

# -----------------------------------------------------------------------------
# Category outlooks
# -----------------------------------------------------------------------------

latest_grocery <- gppi_scores %>% filter(!is.na(cpi_cereals_yoy)) %>% slice_max(date, n = 1)

generate_category_outlook <- function(yoy_change, category_name) {
  if (is.na(yoy_change)) return(sprintf("%s prices: insufficient data.", category_name))
  direction <- case_when(
    yoy_change >  5  ~ "rising sharply",
    yoy_change >  2  ~ "rising moderately",
    yoy_change >  0  ~ "rising slightly",
    yoy_change > -2  ~ "holding steady",
    TRUE              ~ "easing"
  )
  driver <- case_when(
    !is.na(latest_grocery$diesel_3m_chg) &&
      as.numeric(latest_grocery$diesel_3m_chg) > 0.30 ~ "driven primarily by transportation costs",
    !is.na(latest_grocery$ppi_food_mfg_yoy) &&
      as.numeric(latest_grocery$ppi_food_mfg_yoy) > 3 ~ "reflecting upstream manufacturing cost pressure",
    TRUE ~ "tracking general inflation conditions"
  )
  sprintf("%s prices are %s year-over-year (%.1f%%), %s.",
          category_name, direction, yoy_change, driver)
}

category_outlooks <- tibble(
  category   = c("Cereals & Bakery", "Meat, Poultry & Fish", "Dairy", "Fruits & Vegetables"),
  yoy_change = c(
    as.numeric(latest_grocery$cpi_cereals_yoy),
    as.numeric(latest_grocery$cpi_meats_yoy),
    as.numeric(latest_grocery$cpi_dairy_yoy),
    as.numeric(latest_grocery$cpi_fruits_veg_yoy)
  )
) %>%
  mutate(
    outlook   = map2_chr(yoy_change, category, generate_category_outlook),
    direction = case_when(
      is.na(yoy_change)  ~ "— Unknown",
      yoy_change >  2    ~ "↑ Rising",
      yoy_change > -2    ~ "→ Stable",
      TRUE               ~ "↓ Easing"
    )
  )

# -----------------------------------------------------------------------------
# Console summary
# -----------------------------------------------------------------------------

cat("\n═══════════════════════════════════════\n")
cat("GROCERY PRICE PRESSURE INDEX (GPPI)\n")
cat("═══════════════════════════════════════\n")
cat(sprintf("  As of:          %s\n", latest_grocery$date))
cat(sprintf("  GPPI Score:     %.2f\n", coalesce(as.numeric(latest_grocery$gppi_composite), NA_real_)))
cat(sprintf("  Signal:         %s\n", latest_grocery$gppi_signal))
cat("═══════════════════════════════════════\n")

# -----------------------------------------------------------------------------
# Write outputs
# -----------------------------------------------------------------------------

dir.create("data/cache", recursive = TRUE, showWarnings = FALSE)
write_csv(grocery_monthly,   "data/cache/grocery_monthly.csv")
write_csv(gppi_scores,       "data/cache/gppi_scores.csv")
write_csv(category_outlooks, "data/cache/category_outlooks.csv")

cat("\n✓ Grocery data ingestion and GPPI calculation complete\n")

