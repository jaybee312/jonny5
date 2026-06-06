# Load .env file if it exists
if (file.exists(".env")) {
  env_vars <- readLines(".env")
  for (v in env_vars) {
    if (nchar(trimws(v)) > 0 && !startsWith(v, "#")) {
      parts <- strsplit(v, "=")[[1]]
      if (length(parts) == 2) do.call(Sys.setenv, setNames(list(trimws(parts[2])), trimws(parts[1])))
    }
  }
}

# =============================================================================
# config.R
# Fuel & Grocery Price Forward Outlook
# Central Configuration
# =============================================================================

if (requireNamespace("dotenv", quietly = TRUE)) dotenv::load_dot_env()

# -----------------------------------------------------------------------------
# API Credentials
# -----------------------------------------------------------------------------

FRED_API_KEY <- Sys.getenv("FRED_API_KEY")
SHEETS_ID    <- Sys.getenv("GOOGLE_SHEETS_ID")

# -----------------------------------------------------------------------------
# FRED Series — Fuel Module
# -----------------------------------------------------------------------------

FUEL_SERIES <- list(
  gasoline_retail    = "GASREGW",       # Weekly retail regular gasoline ($/gal)
  diesel_retail      = "GASDESW",       # Weekly retail diesel ($/gal)
  wti_crude          = "DCOILWTICO",    # Weekly WTI crude spot price ($/bbl)
  crude_inventory    = "WCRSTUS1",      # Weekly US crude oil inventory (000 bbls)
  gasoline_inventory = "WGTSTUS1",      # Weekly US gasoline inventory (000 bbls)
  distillate_inventory = "WDISTUS1",    # Weekly US distillate inventory (000 bbls)
  cpi_energy         = "CPIENGSL",      # Monthly CPI energy
  cpi_gasoline       = "CUSR0000SETB01",# Monthly CPI gasoline all types
  consumer_sentiment = "UMCSENT",       # Monthly consumer sentiment
  refinery_util      = "OPUSNGAS"       # Monthly refinery utilization
)

# -----------------------------------------------------------------------------
# FRED Series — Grocery Module
# -----------------------------------------------------------------------------

GROCERY_SERIES <- list(
  cpi_food_home      = "CPIFABSL",      # Monthly CPI food at home
  cpi_food_away      = "CPIUFDNS",      # Monthly CPI food away from home
  cpi_cereals        = "CPICB01",       # Monthly CPI cereals and bakery
  cpi_meats          = "CPIUFDM",       # Monthly CPI meats poultry fish eggs
  cpi_dairy          = "CPIUFDAI",      # Monthly CPI dairy
  cpi_fruits_veg     = "CPIUFDFV",      # Monthly CPI fruits and vegetables
  ppi_food_mfg       = "PCUFD__FD__",   # Monthly PPI food manufacturing
  ppi_trucking       = "PCU484___484___",# Monthly PPI truck transportation
  natural_gas        = "MHHNGSP"        # Monthly Henry Hub natural gas price
)

# -----------------------------------------------------------------------------
# Forecast Config
# -----------------------------------------------------------------------------

FUEL_HORIZON_4W   <- 4    # Short-term fuel forecast (weeks)
FUEL_HORIZON_13W  <- 13   # Medium-term fuel forecast (weeks)
GROCERY_HORIZON   <- 3    # Grocery pressure projection (months)
HOLDOUT_PERIODS   <- 12   # Periods held out for model evaluation

# -----------------------------------------------------------------------------
# Scenario Config
# WTI price adjustment multipliers for scenario analysis
# -----------------------------------------------------------------------------

SCENARIO_CONSERVATIVE    <- -0.15  # -15% from current WTI (demand weakness)
SCENARIO_BASE            <-  0.00  # Current trajectory
SCENARIO_DISRUPTION      <-  0.25  # +25% from current WTI (supply shock)

# EIA STEO baseline anchors (updated manually from monthly STEO release)
EIA_GASOLINE_FORECAST_2026 <- 3.70   # $/gallon
EIA_DIESEL_FORECAST_2026   <- 4.80   # $/gallon

# -----------------------------------------------------------------------------
# GPPI Weights
# Component weights for Grocery Price Pressure Index
# Based on historical lead relationship to food CPI
# -----------------------------------------------------------------------------

GPPI_WEIGHTS <- list(
  transportation  = 0.35,   # Diesel / trucking costs — highest direct passthrough
  agricultural    = 0.25,   # WTI + natural gas as farm input costs
  manufacturing   = 0.25,   # PPI food manufacturing — upstream cost pressure
  freight         = 0.15    # PPI trucking — secondary freight signal
)

# GPPI scoring thresholds
GPPI_HIGH_PRESSURE   <-  1.5
GPPI_MOD_PRESSURE    <-  0.5
GPPI_NEUTRAL_LOW     <- -0.5

# -----------------------------------------------------------------------------
# Fleet Calculator Defaults
# -----------------------------------------------------------------------------

FLEET_DEFAULT_SIZE    <- 10     # Number of vehicles
FLEET_DEFAULT_MILES   <- 2500   # Weekly miles per vehicle
FLEET_DEFAULT_MPG     <- 7.5    # Average MPG (diesel truck default)

# -----------------------------------------------------------------------------
# Report Config
# -----------------------------------------------------------------------------

REPORT_TITLE    <- "Fuel & Grocery Price Forward Outlook"
REPORT_CADENCE  <- "Weekly fuel | Monthly grocery"
OUTPUT_DIR      <- "output/reports"
DATA_CACHE_DIR  <- "data/cache"

dir.create(OUTPUT_DIR,      recursive = TRUE, showWarnings = FALSE)
dir.create(DATA_CACHE_DIR,  recursive = TRUE, showWarnings = FALSE)

# -----------------------------------------------------------------------------
# Validation
# -----------------------------------------------------------------------------

if (FRED_API_KEY == "") {
  message("⚠ FRED_API_KEY not set — data ingestion will fail.\n",
          "  Set in .env: FRED_API_KEY=your_key_here\n",
          "  Free key: https://fred.stlouisfed.org/docs/api/api_key.html")
}

cat("✓ Config loaded\n")
