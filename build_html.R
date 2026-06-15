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
  GPPI_LABELS = gppi_labels,
  GPPI_VALUES = gppi_values
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
