# =============================================================================
# build_html.R  —  Jonny 5 HTML generator
# Fills jonny5_template.html (the banner page, with {{TOKENS}}) using the
# variables computed by rebuild_jonny5.R, then writes index.html.
#
# Usage:
#   source("build_html.R")          # sources rebuild_jonny5.R for you
# For a fresh-data run: source("run_report.R") first to refresh the caches,
# then source("build_html.R").
# =============================================================================

# 1. Compute all data variables (sets wd, loads caches, pulls PADD, etc.)
source("rebuild_jonny5.R")

TEMPLATE <- "jonny5_template.html"
OUTPUT   <- "index.html"
if (!file.exists(TEMPLATE)) stop("Missing ", TEMPLATE, " in ", getwd())

# 1b. Current WTI — Yahoo front-month (CL=F) via quantmod is near-real-time and
#     tracks the live market. FRED daily is the YoY baseline + fallback; the
#     weekly cache is the last resort.
fred_wti <- tryCatch(fredr("DCOILWTICO", observation_start = Sys.Date() - 430),
                     error = function(e) NULL)
fw <- if (!is.null(fred_wti)) { x <- fred_wti[!is.na(fred_wti$value), ]; x[order(x$date), ] } else NULL

curr_wti <- as.numeric(latest_fuel$wti_crude); wti_date <- latest_fuel$date   # cache fallback
if (!is.null(fw) && nrow(fw) > 0) {                                            # FRED daily
  curr_wti <- as.numeric(fw$value[nrow(fw)]); wti_date <- as.Date(fw$date[nrow(fw)])
}
yq <- tryCatch({                                                              # Yahoo live (best)
  if (!requireNamespace("quantmod", quietly = TRUE)) stop("no quantmod")
  quantmod::getQuote("CL=F")
}, error = function(e) NULL)
if (!is.null(yq) && !is.na(suppressWarnings(as.numeric(yq$Last))) && as.numeric(yq$Last) > 0) {
  curr_wti <- as.numeric(yq$Last)
  wti_date <- tryCatch(as.Date(yq[["Trade Time"]]), error = function(e) Sys.Date())
  if (length(wti_date) == 0 || is.na(wti_date)) wti_date <- Sys.Date()
}
# year-over-year vs ~1 year ago (from FRED history)
if (!is.null(fw) && nrow(fw) > 0) {
  ya <- fw[fw$date <= wti_date - 364, ]
  if (nrow(ya) > 0) wti_yoy <- (curr_wti - ya$value[nrow(ya)]) / ya$value[nrow(ya)] * 100
}
# rebuild the WTI "watch" sentence with the fresh value
watch1 <- sprintf("WTI crude at $%.2f/bbl — %s year over year. %s", curr_wti,
  ifelse(!is.na(wti_yoy), sprintf("%+.0f%%", wti_yoy), "data pending"),
  ifelse(!is.na(wti_yoy) && wti_yoy > 20, "Supply disruption scenario is not theoretical at current levels.",
  ifelse(!is.na(wti_yoy) && wti_yoy > 0, "Prices elevated vs last year — monitor for further movement.",
         "Prices below last year — favorable conditions.")))

# 2a. Crack spread — real 3-2-1 from cache -------------------------------------
# Real values start from ~Jun 2025 when DGASNYH/DHOILNYH were added
# Use date cutoff to exclude old proxy values
fuel_sorted       <- fuel_weekly[order(fuel_weekly$date), ]
crack_hist        <- fuel_sorted[!is.na(fuel_sorted$diesel_crack_spread) &
                                 fuel_sorted$date >= as.Date("2025-06-01"),
                                 c("date", "diesel_crack_spread")]
crack_spread_val  <- if (nrow(crack_hist) > 0) tail(crack_hist$diesel_crack_spread, 1) else NA_real_
if (is.na(crack_spread_val)) crack_spread_val <- round(curr_d - (curr_wti / 42), 2)

crack_spread_fmt  <- if (!is.na(crack_spread_val)) sprintf("$%.2f", crack_spread_val) else "data pending"

crack_note        <- if (!is.na(crack_spread_val)) {
  if (crack_spread_val > 35) {
    "Wide margins — refiners are running hard. Diesel supply is healthy for now."
  } else if (crack_spread_val > 15) {
    "Margins in normal range — no major refining-side pressure on diesel."
  } else {
    "Compressed margins — refiners may pull back, watch for diesel tightening."
  }
} else ""

# Sparkline data — last 10 weeks, fill latest NA with live computed value
spark_df          <- tail(fuel_sorted[, c("date", "diesel_crack_spread", "diesel_retail")], 10)
spark_df$diesel_crack_spread[is.na(spark_df$diesel_crack_spread)] <- crack_spread_val

# Chart tokens — crack_hist already filtered to real values above
crack_hist_labels   <- paste0('["', paste(format(crack_hist$date, "%Y-%m-%d"), collapse='","'), '"]')
crack_hist_values   <- paste0("[", paste(round(crack_hist$diesel_crack_spread, 2), collapse=","), "]")

# Data-driven insight: compare current to 4-week ago and 52-week average
crack_4w_ago   <- if (nrow(crack_hist) >= 5) crack_hist$diesel_crack_spread[nrow(crack_hist) - 4] else NA_real_
crack_52w_avg  <- if (nrow(crack_hist) >= 52) mean(tail(crack_hist$diesel_crack_spread, 52), na.rm=TRUE) else mean(crack_hist$diesel_crack_spread, na.rm=TRUE)
crack_mom_chg  <- if (!is.na(crack_4w_ago)) round(crack_spread_val - crack_4w_ago, 2) else NA_real_
crack_vs_avg   <- round(crack_spread_val - crack_52w_avg, 2)

crack_insight <- sprintf(
  "Current: $%.2f/bbl%s. %s the 52-week average ($%.2f/bbl). %s",
  crack_spread_val,
  if (!is.na(crack_mom_chg)) sprintf(" (%+.2f past 4 weeks)", crack_mom_chg) else "",
  if (crack_vs_avg > 0) sprintf("$%.2f above", abs(crack_vs_avg)) else sprintf("$%.2f below", abs(crack_vs_avg)),
  crack_52w_avg,
  crack_note
)

# Forecast y-axis zoom — bracket history min/max with small padding
forecast_ymin <- round(min(fuel_sorted$diesel_retail, na.rm=TRUE) - 0.10, 2)
forecast_ymax <- round(max(fuel_sorted$diesel_retail, na.rm=TRUE) + 0.10, 2)

# RDI signal — Real Disposable Income MoM change and direction
# Monthly series lags 6-8 weeks — use last non-NA observation
rdi_series  <- fuel_sorted[!is.na(fuel_sorted$real_disp_income), c("date", "real_disp_income")]
rdi_current <- if (nrow(rdi_series) >= 1) tail(rdi_series$real_disp_income, 1) else NA_real_
rdi_prev    <- if (nrow(rdi_series) >= 2) tail(rdi_series$real_disp_income, 2)[1] else NA_real_
rdi_mom     <- if (!is.na(rdi_current) && !is.na(rdi_prev)) round((rdi_current - rdi_prev) / rdi_prev * 100, 2) else NA_real_
rdi_level   <- if (!is.na(rdi_current)) {
  rdi_asof <- format(tail(rdi_series$date, 1), "%b %Y")
  sprintf("$%.0fB <span style='font-size:0.75rem;font-weight:400;color:rgba(255,255,255,0.45);'>as of %s</span>", rdi_current, rdi_asof)
} else "data pending"
rdi_change  <- if (!is.na(rdi_mom)) sprintf("%+.2f%% MoM", rdi_mom) else ""
rdi_signal  <- if (!is.na(rdi_mom)) {
  direction <- if (rdi_mom > 0.3)       "Rising"
               else if (rdi_mom < -0.3) "Softening"
               else                      "Flat"
  implication <- if (rdi_mom > 0.3)       "demand likely strengthening — reorder point may be too low."
                 else if (rdi_mom < -0.3) "demand headwinds building — watch for overstock risk."
                 else                      "demand conditions stable. Reorder math holds."
  sprintf("%s at $%.0fB (%s as of %s) — %s",
          direction, rdi_current, rdi_change,
          format(tail(rdi_series$date, 1), "%b %Y"),
          implication)
} else "Pending latest release."

# YoY margin commentary — RDI YoY vs diesel YoY
rdi_yoy <- if (nrow(rdi_series) >= 2) {
  # Find the observation closest to 1 year ago
  target_date <- tail(rdi_series$date, 1) - 365
  rdi_yr_ago  <- rdi_series$real_disp_income[which.min(abs(rdi_series$date - target_date))]
  round((rdi_current - rdi_yr_ago) / rdi_yr_ago * 100, 1)
} else NA_real_

# diesel_yoy_chg is $/gal change vs 52 weeks ago — convert to percent, sign preserved
diesel_yr_ago  <- curr_d - as.numeric(latest_fuel$diesel_yoy_chg)
diesel_yoy_pct <- if (!is.na(diesel_yr_ago) && diesel_yr_ago > 0) {
  round(as.numeric(latest_fuel$diesel_yoy_chg) / diesel_yr_ago * 100, 1)
} else NA_real_

rdi_yoy_fmt    <- if (!is.na(rdi_yoy))        sprintf("%+.1f%% YoY", rdi_yoy)        else "data pending"
diesel_yoy_fmt <- if (!is.na(diesel_yoy_pct)) sprintf("%+.1f%% YoY", diesel_yoy_pct) else "data pending"

margin_signal <- if (!is.na(rdi_yoy) && !is.na(diesel_yoy_pct)) {
  if      (rdi_yoy >= 0 && diesel_yoy_pct <= 0) "Margin Tailwind"
  else if (rdi_yoy >= 0 && diesel_yoy_pct >  0) "Mixed"
  else if (rdi_yoy <  0 && diesel_yoy_pct >  0) "Margin Squeeze"
  else                                            "Costs Easing"
} else "Pending"

margin_detail <- if (!is.na(rdi_yoy) && !is.na(diesel_yoy_pct)) {
  if      (rdi_yoy >= 0 && diesel_yoy_pct <= 0) "consumer wallets expanding while transportation costs fall."
  else if (rdi_yoy >= 0 && diesel_yoy_pct >  0) "income rising but transportation costs rising with it."
  else if (rdi_yoy <  0 && diesel_yoy_pct >  0) "consumer purchasing power falling while transportation costs rise."
  else                                            "watch whether income recovers before reordering aggressively."
} else ""

rdi_margin_commentary <- sprintf("Real income %s | Diesel %s. %s", rdi_yoy_fmt, diesel_yoy_fmt, margin_signal)

# Dual-axis chart — full RDI + diesel history from 2018, aligned on date
# Join RDI series with diesel from fuel_weekly on date
chart_df <- merge(
  rdi_series,
  fuel_sorted[!is.na(fuel_sorted$diesel_retail), c("date", "diesel_retail")],
  by = "date"
)
chart_df <- chart_df[order(chart_df$date), ]

rdi_hist_labels     <- paste0('["', paste(format(chart_df$date, "%Y-%m-%d"), collapse='","'), '"]')
rdi_hist_values     <- paste0("[", paste(round(chart_df$real_disp_income, 0), collapse=","), "]")
diesel_aligned_values <- paste0("[", paste(round(chart_df$diesel_retail, 3), collapse=","), "]")

# gas_inv_vs_avg: gasoline days supply vs 5-year average (distillate stub pending)
# crude_supply_signal: directional signal already computed in ingest
dist_vs_avg   <- as.numeric(latest_fuel$gas_inv_vs_avg)
crude_signal  <- as.character(latest_fuel$crude_supply_signal)

crude_vs_avg_txt <- if (!is.na(dist_vs_avg)) {
  if (dist_vs_avg < 0) sprintf("%.1f days below", abs(dist_vs_avg)) else sprintf("%.1f days above", dist_vs_avg)
} else "data pending"

supply_note <- if (!is.na(dist_vs_avg)) {
  if (dist_vs_avg < -1) {
    "Gasoline stocks below seasonal average — tighter supply conditions ahead."
  } else if (dist_vs_avg > 1) {
    "Gasoline stocks above seasonal average — adequate near-term buffer."
  } else {
    "Gasoline stocks near the seasonal average — neutral supply picture."
  }
} else ""


# 2. Derived values the template needs on top of rebuild's variables ----------
report_date <- format(Sys.Date(),      "%B %d %Y")   # publish date (the day you run it)
fuel_week   <- format(latest_fuel$date, "%B %d %Y")  # diesel / EIA data week
wti_asof    <- format(wti_date,         "%b %d")     # WTI as-of date, e.g. "Jun 12"

ps         <- padd_summary[order(padd_summary$value), ]       # cheapest -> dearest
region_min <- ps$region[1];          min_price <- ps$value[1]
region_max <- ps$region[nrow(ps)];   max_price <- ps$value[nrow(ps)]
price_for  <- function(r){ v <- padd_summary$value[padd_summary$region==r]; if(length(v)) v[1] else NA }
gulf_price <- price_for("Gulf Coast")
west_price <- price_for("West Coast")

annual_diff <- weekly_diff * 52
gppi_label  <- if (gppi_s > 1.5) "High Pressure" else
               if (gppi_s > 0.5) "Moderate Pressure" else
               if (gppi_s > -0.5) "Neutral" else "Relief Building"
gppi_month  <- format(latest_gppi$date, "%B")                  # "May"
yoy_txt <- if (!is.na(wti_yoy)) sprintf("%+.0f%%", wti_yoy) else "n/a"
fmt_int <- function(x) format(round(x), big.mark = ",", trim = TRUE)

# 3. Token -> value map (all strings) -----------------------------------------
vals <- c(
  REPORT_DATE = report_date,
  FUEL_WEEK   = fuel_week,
  WTI_DATE    = wti_asof,
  CURR_WTI    = sprintf("%.2f",  curr_wti),
  WTI_YOY     = yoy_txt,
  CURR_D      = sprintf("%.3f",  curr_d),
  BASE_D      = sprintf("%.3f",  base_d),
  DIS_D       = sprintf("%.3f",  dis_d),
  CONS_D      = sprintf("%.3f",  cons_d),
  CONS_DELTA  = sprintf("%+.3f", cons_d - curr_d),
  BASE_DELTA  = sprintf("%+.3f", base_d - curr_d),
  DIS_DELTA   = sprintf("%+.3f", dis_d  - curr_d),
  GPPI_S      = sprintf("%.2f",  gppi_s),
  GPPI_PCT    = sprintf("%.2f",  gppi_pct),
  GPPI_LABEL  = gppi_label,
  GPPI_MONTH  = gppi_month,
  SPREAD      = sprintf("%.2f",  spread),
  WEEKLY_DIFF = fmt_int(weekly_diff),
  ANNUAL_DIFF = fmt_int(annual_diff),
  REGION_MIN  = region_min,
  REGION_MAX  = region_max,
  MIN_PRICE   = sprintf("%.3f", min_price),
  MAX_PRICE   = sprintf("%.3f", max_price),
  GULF_PRICE  = sprintf("%.3f", gulf_price),
  WEST_PRICE  = sprintf("%.3f", west_price),
  WATCH1      = watch1,
  WATCH2      = watch2,
  WATCH3      = watch3,
  PADD_ROWS   = padd_rows,
  PADD_JS     = padd_prices_js,
  HIST_LABELS = hist_labels,
  HIST_VALUES = hist_values,
  FC_LABELS   = fc_labels_js,
  FC_BASE     = fc_base,
  FC_CONS     = fc_cons,
  FC_DIS      = fc_dis,
  GPPI_LABELS   = gppi_labels,
  GPPI_VALUES   = gppi_values,
  CRACK_SPREAD        = crack_spread_fmt,
  CRACK_NOTE          = crack_note,
  CRACK_INSIGHT       = crack_insight,
  CRACK_HIST_LABELS   = crack_hist_labels,
  CRACK_HIST_VALUES   = crack_hist_values,
  FORECAST_YMIN       = as.character(forecast_ymin),
  FORECAST_YMAX       = as.character(forecast_ymax),
  RDI_MARGIN_COMMENTARY   = rdi_margin_commentary,
  RDI_YOY                 = rdi_yoy_fmt,
  DIESEL_YOY              = diesel_yoy_fmt,
  MARGIN_SIGNAL           = margin_signal,
  MARGIN_DETAIL           = margin_detail,
  RDI_HIST_LABELS         = rdi_hist_labels,
  RDI_HIST_VALUES         = rdi_hist_values,
  DIESEL_ALIGNED_VALUES   = diesel_aligned_values,
  CRUDE_VS_AVG        = crude_vs_avg_txt,
  SUPPLY_NOTE         = supply_note
)

# 4. Fill the template --------------------------------------------------------
con  <- file(TEMPLATE, encoding = "UTF-8")
html <- paste(readLines(con, warn = FALSE), collapse = "\n")
close(con)
for (nm in names(vals)) html <- gsub(paste0("{{", nm, "}}"), vals[[nm]], html, fixed = TRUE)

# 5. Safety check: every placeholder must be filled ---------------------------
left <- regmatches(html, gregexpr("\\{\\{[A-Z0-9_]+\\}\\}", html))[[1]]
if (length(left) > 0)
  stop("Unfilled placeholders remain: ", paste(unique(left), collapse = ", "),
       "\n(add them to the vals map in build_html.R)")

# 6. Write index.html ---------------------------------------------------------
writeLines(html, OUTPUT, useBytes = TRUE)

cat(sprintf("\n[OK] Wrote %s  (%d bytes)\n", OUTPUT, file.info(OUTPUT)$size))
cat(sprintf("     published %s | WTI $%s (%s, as of %s) | diesel $%s (week of %s) | gppi %s (%s, %s)\n",
            report_date, vals[["CURR_WTI"]], yoy_txt, wti_asof, vals[["CURR_D"]], fuel_week,
            vals[["GPPI_S"]], gppi_label, gppi_month))
cat(sprintf("     spread=$%s  fleet=$%s/wk  cheapest=%s ($%s)  dearest=%s ($%s)\n",
            vals[["SPREAD"]], vals[["WEEKLY_DIFF"]], region_min, vals[["MIN_PRICE"]],
            region_max, vals[["MAX_PRICE"]]))
