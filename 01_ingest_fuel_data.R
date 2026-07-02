# 01_ingest_fuel_data.R
library(tidyverse)
library(lubridate)
library(fredr)
library(zoo)
source("config.R")
fredr_set_key(FRED_API_KEY)

START_DATE   <- as.Date("2018-01-01")
END_DATE     <- Sys.Date()
weekly_dates <- seq.Date(
  from = ceiling_date(START_DATE, "week", week_start = 1),
  to   = floor_date(END_DATE, "week", week_start = 1),
  by   = "week"
)

pull_weekly <- function(series_id, series_name) {
  cat(sprintf("  Pulling %s (%s)...\n", series_name, series_id))
  tryCatch({
    fredr(series_id = series_id,
          observation_start = START_DATE,
          observation_end   = END_DATE,
          frequency         = "w") %>%
      select(date, value) %>%
      rename(!!series_name := value) %>%
      mutate(date = as.Date(date))
  }, error = function(e) {
    warning(sprintf("Failed: %s — %s", series_id, e$message))
    NULL
  })
}

pull_monthly_to_weekly <- function(series_id, series_name, weekly_dates) {
  cat(sprintf("  Pulling %s (%s) [monthly -> weekly]...\n", series_name, series_id))
  tryCatch({
    monthly <- fredr(series_id = series_id,
                     observation_start = START_DATE,
                     observation_end   = END_DATE,
                     frequency         = "m") %>%
      select(date, value) %>%
      rename(!!series_name := value) %>%
      mutate(date = as.Date(date))
    daily_dates <- seq.Date(min(monthly$date), max(monthly$date), by = "day")
    tibble(date = daily_dates) %>%
      left_join(monthly, by = "date") %>%
      mutate(!!series_name := na.approx(.data[[series_name]], na.rm = FALSE, rule = 2)) %>%
      filter(date %in% weekly_dates) %>%
      select(date, all_of(series_name))
  }, error = function(e) {
    warning(sprintf("Failed: %s — %s", series_id, e$message))
    NULL
  })
}

pull_wti_weekly <- function(weekly_dates) {
  cat("  Pulling wti_crude (DCOILWTICO) [daily -> weekly]...\n")
  tryCatch({
    daily <- fredr(series_id = "DCOILWTICO",
                   observation_start = START_DATE,
                   observation_end   = END_DATE,
                   frequency         = "d") %>%
      select(date, wti_crude = value) %>%
      mutate(date = as.Date(date)) %>%
      filter(!is.na(wti_crude))
    daily %>%
      mutate(week = floor_date(date, "week", week_start = 1)) %>%
      group_by(week) %>%
      slice_max(date, n = 1) %>%
      ungroup() %>%
      select(date = week, wti_crude)
  }, error = function(e) {
    warning(sprintf("Failed WTI: %s", e$message))
    NULL
  })
}

cat("\nPulling weekly fuel series...\n")
weekly_series <- list(
  pull_weekly("GASREGW",    "gasoline_retail"),
  pull_weekly("GASDESW",    "diesel_retail"),
  pull_weekly("DCOILBRENTEU", "brent_crude"),
  pull_weekly("GASREGCOVW", "gas_days_supply"),
  pull_weekly("DHHNGSP",    "nat_gas_spot")
)

cat("\nPulling WTI crude (daily -> weekly)...\n")
wti_series <- pull_wti_weekly(weekly_dates)

cat("\nPulling monthly series (interpolated to weekly)...\n")
monthly_series <- list(
  pull_monthly_to_weekly("CPIENGSL",        "cpi_energy",         weekly_dates),
  pull_monthly_to_weekly("CUSR0000SETB01",  "cpi_gasoline",       weekly_dates),
  pull_monthly_to_weekly("UMCSENT",         "consumer_sentiment", weekly_dates),
  pull_monthly_to_weekly("CAPG211S",        "refinery_capacity",  weekly_dates)
)

cat("\nPulling spot prices for real 3-2-1 crack spread...\n")
pull_daily_to_weekly <- function(series_id, series_name, weekly_dates) {
  cat(sprintf("  Pulling %s (%s) [daily -> weekly]...\n", series_name, series_id))
  tryCatch({
    daily <- fredr(series_id = series_id,
                   observation_start = START_DATE,
                   observation_end   = END_DATE,
                   frequency         = "d") %>%
      select(date, value) %>%
      rename(!!series_name := value) %>%
      mutate(date = as.Date(date)) %>%
      filter(!is.na(.data[[series_name]]))
    daily %>%
      mutate(week = floor_date(date, "week", week_start = 1)) %>%
      group_by(week) %>%
      slice_max(date, n = 1) %>%
      ungroup() %>%
      select(date = week, all_of(series_name))
  }, error = function(e) {
    warning(sprintf("Failed %s: %s", series_id, e$message))
    NULL
  })
}

spot_series <- list(
  pull_daily_to_weekly("DGASNYH",  "gas_spot_nyh", weekly_dates),  # NY Harbor conventional gas $/gal daily->weekly
  pull_daily_to_weekly("DHOILNYH", "ho_spot_nyh",  weekly_dates)   # NY Harbor heating oil $/gal daily->weekly
)

all_series <- c(compact(weekly_series), list(wti_series), compact(monthly_series), compact(spot_series))
all_series <- compact(all_series)

fuel_weekly <- reduce(all_series, full_join, by = "date") %>%
  filter(date %in% weekly_dates) %>%
  arrange(date)

cat(sprintf("\n✓ Fuel dataset: %d weeks x %d series\n",
            nrow(fuel_weekly), ncol(fuel_weekly) - 1))

fuel_weekly <- fuel_weekly %>%
  arrange(date) %>%
  mutate(
    wti_lag1  = lag(wti_crude, 1),
    wti_lag2  = lag(wti_crude, 2),
    wti_lag4  = lag(wti_crude, 4),
    gas_crack_spread    = gasoline_retail - (wti_crude / 42),
    # Real 3-2-1 crack spread: ((2*gas_spot + 1*HO_spot) * 42 - 3*WTI) / 3, $/bbl
    # Falls back to retail proxy if spot prices unavailable
    diesel_crack_spread = if ("gas_spot_nyh" %in% names(.) && "ho_spot_nyh" %in% names(.)) {
      ifelse(!is.na(gas_spot_nyh) & !is.na(ho_spot_nyh) & !is.na(wti_crude),
             ((2 * gas_spot_nyh + 1 * ho_spot_nyh) * 42 - 3 * wti_crude) / 3,
             NA_real_)
    } else {
      diesel_retail - (wti_crude / 42)
    },
    week_of_year = as.integer(format(date, "%V")),
    gas_wow_chg    = gasoline_retail - lag(gasoline_retail, 1),
    gas_yoy_chg    = gasoline_retail - lag(gasoline_retail, 52),
    diesel_wow_chg = diesel_retail   - lag(diesel_retail, 1),
    diesel_yoy_chg = diesel_retail   - lag(diesel_retail, 52),
    wti_wow_chg    = wti_crude       - lag(wti_crude, 1),
    wti_4w_chg     = wti_crude       - lag(wti_crude, 4),
    summer_blend   = as.integer(month(date) %in% 4:9),
    winter_heating = as.integer(month(date) %in% c(10:12, 1:2))
  )

inv_seasonal_avg <- fuel_weekly %>%
  filter(date >= max(date) - years(5)) %>%
  group_by(week_of_year) %>%
  summarise(
    gas_inv_5yr_avg        = mean(gas_days_supply,  na.rm = TRUE),
    .groups = "drop"
  )

fuel_weekly <- fuel_weekly %>%
  left_join(inv_seasonal_avg, by = "week_of_year") %>%
  mutate(
    gas_inv_vs_avg         = gas_days_supply  - gas_inv_5yr_avg,
    distillate_inv_vs_avg  = NA_real_,
    crude_supply_signal    = "Normal",
    distillate_supply_signal = "Normal"
  )

latest <- fuel_weekly %>% slice_max(date, n = 1)
cat("\n═══════════════════════════════════════\n")
cat("CURRENT FUEL SNAPSHOT\n")
cat("═══════════════════════════════════════\n")
cat(sprintf("  As of:          %s\n", latest$date))
cat(sprintf("  Gasoline:       $%.3f/gal  (%+.3f WoW)\n",
            latest$gasoline_retail, coalesce(latest$gas_wow_chg, 0)))
cat(sprintf("  Diesel:         $%.3f/gal  (%+.3f WoW)\n",
            latest$diesel_retail, coalesce(latest$diesel_wow_chg, 0)))
cat(sprintf("  WTI Crude:      $%.2f/bbl  (%+.2f WoW)\n",
            coalesce(latest$wti_crude, 0), coalesce(latest$wti_wow_chg, 0)))
cat("═══════════════════════════════════════\n")

dir.create("data/cache", recursive = TRUE, showWarnings = FALSE)
write_csv(fuel_weekly, "data/cache/fuel_weekly.csv")
cat("\n✓ Fuel data ingestion complete\n")
cat(sprintf("  Saved: data/cache/fuel_weekly.csv (%d rows)\n", nrow(fuel_weekly)))
