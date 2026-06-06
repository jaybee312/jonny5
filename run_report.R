# =============================================================================
# run_report.R
# Jonny 5 — Master Pipeline
#
# Pulls fresh data, runs models, regenerates jonny5.html
# Usage: Rscript run_report.R
# =============================================================================
library(tidyverse)
source("config.R")

start_time <- Sys.time()
cat("\n")
cat("═══════════════════════════════════════════════════\n")
cat("  JONNY 5 — DATA PIPELINE\n")
cat(sprintf("  Run started: %s\n", format(start_time, "%Y-%m-%d %H:%M:%S")))
cat("═══════════════════════════════════════════════════\n\n")

cat("Step 1/4: Ingesting fuel data from FRED...\n")
source("01_ingest_fuel_data.R")
cat("\n")

cat("Step 2/4: Ingesting grocery data and computing GPPI...\n")
source("02_ingest_grocery_data.R")
cat("\n")

cat("Step 3/4: Fitting fuel models and generating forecasts...\n")
source("03_fuel_forecast.R")
cat("\n")

cat("Step 4/4: Regenerating Jonny 5...\n")
source("generate_jonny5.R")
cat("\n")

end_time <- Sys.time()
elapsed  <- round(as.numeric(difftime(end_time, start_time, units="mins")), 1)
cat("═══════════════════════════════════════════════════\n")
cat("  PIPELINE COMPLETE\n")
cat(sprintf("  Elapsed: %s minutes\n", elapsed))
cat(sprintf("  Output:  jonny5.html\n"))
cat("═══════════════════════════════════════════════════\n\n")
