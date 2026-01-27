#==============================================================================
# 10_Build_Master_Annual_Panel.R
# 10/24/2026
# Purpose: Create a clean FACILITY-YEAR panel from RAW DATA.
#          Integrates "Corrected" Monthly construction (Sec 1-4) with
#          full feature engineering (Sec 5-10) and Annual Aggregation.
#
# Key Features:
#   - Precise Leak-Closure Classification (4 Specs: Primary, Narrow, Wide, Reg)
#   - Annual Aggregation: Stocks (Dec) vs Flows (Sum)
#   - Flow Changes: Calculated as Year-over-Year difference in Dec Stocks
#   - Texas FR Data: Wide-format issuer counts + Gaps/Transitions
#   - Policy Variables: post_1999 + Other Treated States
#
# Outputs:
#   1. tank_panel_unified.rds (Corrected Inventory)
#   2. facility_leak_behavior_annual.csv (Final Analysis Panel)
#   3. facility_leak_behavior_annual.rds (Fast-load version)
#
#==============================================================================
#==============================================================================
# 10_Build_Master_Annual_Panel.R
#
# Purpose: Create a clean FACILITY-YEAR panel from RAW DATA.
# Outputs:
#   1. tank_panel_unified.rds (Corrected Inventory)
#   2. facility_leak_behavior_annual.csv/rds (Final Analysis Panel)
#==============================================================================
script_start_time <- Sys.time()

# Load necessary libraries
suppressPackageStartupMessages({
  library(data.table)  # Core data manipulation
  library(lubridate)   # Date parsing and handling
  library(here)        # Project-relative file paths
  library(tidyverse)   # Used for various utilities later in script
  library(zoo)         # For na.locf (used later in script)
})

# Set threads for data.table for parallel processing
setDTthreads(14) 

# ==============================================================================
# SECTION 0: HELPER FUNCTIONS
# ==============================================================================

#' Coerce to Date
#' Safely converts IDate, character, or other types to standard Date class.
coerce_date <- function(x) {
  if (inherits(x, "Date")) return(x)
  as.Date(x, origin = "1970-01-01")
}

#' Clean ID Columns
#' Standardizes ID columns to uppercase, trimmed character strings.
clean_id <- function(x) toupper(trimws(as.character(x)))

#' Get Data Path
#' Constructs a flexible file path based on an environment variable or a default.
get_data_path <- function(...) {
  root <- Sys.getenv("UST_DATA_ROOT",
                     unset = file.path(Sys.getenv("HOME"), "UST", "Data"))
  file.path(root, ...)
}

#' Log Memory
#' Performance monitoring helper to log current memory usage.
log_memory <- function(label) {
  gc_info <- gc()
  cat(sprintf("  [Memory] %s: %.1f MB used\n", 
              label, sum(gc_info[, "used"]) / 1024))
}

#' Log Step
#' Logging helper for structured console output with indentation.
log_step <- function(message, indent = 0) {
  prefix <- paste(rep("  ", indent), collapse = "")
  cat(sprintf("%s%s\n", prefix, message))
}

#' Parse Texas LUST Dates
#' Robust date parsing for varied formats found in the TX LUST data.
parse_tx_lust_date <- function(x) {
  x_chr <- trimws(as.character(x))
  x_chr[x_chr %chin% c("", "NA", "NULL")] <- NA_character_
  suppressWarnings(as.Date(lubridate::parse_date_time( # Change to as.Date
    x_chr,
    orders = c("d-b-y", "d-b-Y", "m/d/y", "m/d/Y", "Y-m-d", "Ymd"),
    tz = "UTC"
  )))
}

cat("========================================\n")
cat("UNIFIED FACILITY-MONTH PANEL (Corrected)\n")
cat("========================================\n\n")

#==============================================================================
# SECTION 1: LOAD AND STANDARDIZE INPUT DATA
#
# Operations:
#   - Load all data sources
#   - Standardize column names and data types
#   - Preserve original dates in *_raw columns
#   - Create panel date bounds (1970-2025) without overwriting historical dates
#==============================================================================

#==============================================================================
# SECTION 1: LOAD AND HARMONIZE INPUT DATA
#
# Purpose: Systematic loading and harmonization of all input datasets
# 
# Subsections:
#   1.1 - Load all raw data files
#   1.2 - Create column mapping structures for harmonization
#   1.3 - Harmonize variables across datasets
#   1.4 - Standardize ID columns
#   1.4.1 - Create composite key (panel_id) and set keys [AUDIT ADDITION]
#==============================================================================

setDTthreads(14) # Ensure threads are set for this section if run interactively

cat("========================================\n")
cat("SECTION 1: DATA LOADING & HARMONIZATION\n")
cat("========================================\n\n")

section_start <- Sys.time()

#==============================================================================
# 1.1 LOAD ALL RAW DATA FILES
#==============================================================================
log_step("1.1 Loading raw data files...", 0)
load_start <- Sys.time()

# --- 1.1.1 MAIN DATA SOURCES (TANKS & LUST) ---

# Load Texas tank-months (facility-tank-month level)
# This is the primary source for Texas tank characteristics and activity
log_step("Loading Texas tank-months (facility-tank-month)...", 1)
tx_tank_months <- fread(here("Data","Processed", "texas_ust_facility_month_panel.csv"))
# install_date  & end_date  ---> is type IDate (based on context)

# Define raw column names to select
tx_tank_months_raw_cols <- c(
  "FACILITY_ID", "UST_ID", "YEAR", "MONTH", "CAPACITY",
  "month_date", "install_date", "end_date", 
  "single_walled", "double_walled", "is_gasoline", "is_diesel",
  "is_oil_kerosene", "is_jet_fuel", "is_other"
)

# Select only the columns we need (using raw column names)
tx_tank_months <- tx_tank_months[, .SD, .SDcols = tx_tank_months_raw_cols]

log_step(sprintf("Loaded: %s rows, %d columns", 
                format(nrow(tx_tank_months), big.mark=","),
                ncol(tx_tank_months)), 2)
log_step(sprintf("Columns: %s", 
                paste(head(names(tx_tank_months), 15), collapse=", ")), 2)


# Load EPA tank panel (facility-tank level)
# This is the primary source for all non-Texas states
log_step("Loading EPA tank panel (facility-tank)...", 1)
tank_panel_epa <- fread(
  here("Data", "Raw",  
       "EPA_Region_6_and_EPA_states_tank_panel.csv"),
  colClasses = c(facility_id = "character", tank_id = "character")
)

# Define raw column names to select
epa_tank_panel_raw_cols <- c(
  "facility_id", "tank_id", "state", "tank_installed_date", "tank_closed_date", 
  "capacity", "single_walled", "double_walled", "is_gasoline", "is_diesel",
  "is_oil_kerosene", "is_jet_fuel", "is_other", "county_name", "county_geoid"
)

# Select only the columns we need (using raw column names)
tank_panel_epa <- tank_panel_epa[, .SD, .SDcols = epa_tank_panel_raw_cols]
# tank_installed_date  & tank_closed_date---> is type IDate (based on context)

log_step(sprintf("Loaded: %s rows, %d columns", 
                format(nrow(tank_panel_epa), big.mark=","),
                ncol(tank_panel_epa)), 2)
log_step(sprintf("States: %s", 
                paste(head(unique(tank_panel_epa$state), 10), collapse=", ")), 2)


# Load EPA LUST data (facility-incident level)
# This is the leak data for non-Texas states
log_step("Loading EPA LUST data (facility-incident)...", 1)
lust_epa <- fread(
  here("Data", "Processed", "all_lust_data.csv"),
  colClasses = c(facility_id = "character")
)
#  facility_id report_date   nfa_date      state
#            <char>      <IDat>     <IDat>     <char>
log_step(sprintf("Loaded: %s rows, %d columns", 
                format(nrow(lust_epa), big.mark=","),
                ncol(lust_epa)), 2)


# Load Texas LUST data (facility-incident level)
# This is the leak data for Texas
log_step("Loading Texas LUST data (facility-incident)...", 1)
TX_LUST_SD <- fread(here("Data","Raw","TX_LUST.csv"))

# Clean TX LUST data immediately upon load
TX_LUST_SD[, `:=`(
  facility_id = clean_id(facility_id),
  LUST_id = trimws(as.character(LUST_id)),
  report_date = parse_tx_lust_date(report_date),
  nfa_date = parse_tx_lust_date(nfa_date)
)]
log_step(sprintf("Loaded: %s rows, %d columns (and cleaned)", 
                format(nrow(TX_LUST_SD), big.mark=","),
                ncol(TX_LUST_SD)), 2)


# --- 1.1.2 AUXILIARY & OPTIONAL DATA (FOR MERGES) ---

# Load Texas facility raw data (facility level - for county lookup)
# This file is used in Section 2 to map facility_id to a county name for TX facilities
log_step("Loading Texas facility raw data (for county lookup)...", 1)
tx_fac_raw <- fread(
  here("Data","Raw", "raw_pst_fac.csv"))
#  FACILITY_ID SITE_COUNTY
#           <int>      <char>

tx_fac_raw_cols <- c(
  "FACILITY_ID",
  "SITE_COUNTY"
)
tx_fac_raw <- tx_fac_raw[, .SD, .SDcols = tx_fac_raw_cols]
log_step(sprintf("Loaded: %s rows, %d columns", 
                format(nrow(tx_fac_raw), big.mark=","),
                ncol(tx_fac_raw)), 2)

# -- DYNAMIC FA_MONTHLY LOAD (SURGICAL UPDATE) --
# We must detect ALL issuer columns and pre-existing dummies dynamically.
fa_monthly <- fread(here("Data", "Raw", "fa_monthly.csv"))

# Define base columns to ALWAYS keep
fa_base_cols <- c("FACILITY_ID", "YEAR", "MONTH", "DETAIL_TYPE", "CATEGORY", 
                  "uses_private", "uses_self", "fr_covered", 
                  "transition_month", "multiple_contracts", "dropped_by_zurich") 

# Dynamically identify variable groups
fa_issuer_cols <- grep("^ISSUER_NAME", names(fa_monthly), value = TRUE)
fa_cat_dummies <- grep("^CATEGORY_", names(fa_monthly), value = TRUE)
fa_det_dummies <- grep("^DETAIL_TYPE_", names(fa_monthly), value = TRUE)
fa_cover_vars  <- grep("^COVER_", names(fa_monthly), value = TRUE)

# Combine and subset
fa_keep <- unique(c(fa_base_cols, fa_issuer_cols, fa_cat_dummies, fa_det_dummies, fa_cover_vars))
fa_keep <- intersect(fa_keep, names(fa_monthly)) # Safety check
fa_monthly <- fa_monthly[, .SD, .SDcols = fa_keep]

log_step(sprintf("FA monthly: %s rows (with %d issuer cols, %d dummies)", 
                 format(nrow(fa_monthly), big.mark=","), 
                 length(fa_issuer_cols), 
                 length(c(fa_cat_dummies, fa_det_dummies))), 1)


zurich_2012_lookup <- fread(here("Data", "Raw", "zurich_2012_lookup.csv"))
zurich_2012_lookup <- zurich_2012_lookup[, .SD, .SDcols = intersect(c("FACILITY_ID","had_zurich_2012"), names(zurich_2012_lookup))]



#==============================================================================
# 1.2 CREATE SYSTEMATIC COLUMN MAPPING STRUCTURES
#==============================================================================
log_step("\n1.2 Creating systematic harmonization mappings...", 0)
mapping_start <- Sys.time()

#------------------------------------------------------------------------------
# 1.2.1 TANK PANEL MAPPINGS (Texas → Common schema)
#------------------------------------------------------------------------------
log_step("Defining tank panel variable mappings...", 1)

# Texas tank-months → Common schema
tx_tank_months_mapping <- c(
  # Identifiers
  "FACILITY_ID" = "facility_id",
  "UST_ID" = "tank_id",
  
  # Time variables
  "YEAR" = "panel_year",
  "MONTH" = "panel_month",
  "month_date" = "date",
  
  # Tank dates (will create standardized versions)
  "install_date" = "tank_installed_date_raw",
  "end_date" = "tank_closed_date_raw",
  
  # Tank characteristics
  "CAPACITY" = "capacity",
  "single_walled" = "single_walled",
  "double_walled" = "double_walled",
  
  # Fuel type indicators
  "is_gasoline" = "is_gasoline",
  "is_diesel" = "is_diesel",
  "is_oil_kerosene" = "is_oil_kerosene",
  "is_jet_fuel" = "is_jet_fuel",
  "is_other" = "is_other"
)

# EPA tank panel → Common schema (already mostly standardized)
epa_tank_panel_mapping <- c(
  # Identifiers (no change needed)
  "facility_id" = "facility_id",
  "tank_id" = "tank_id",
  "state" = "state",
  
  # Tank dates (standardize naming)
  "tank_installed_date" = "tank_installed_date_raw",
  "tank_closed_date" = "tank_closed_date_raw",
  
  # Tank characteristics (no change needed)
  "capacity" = "capacity",
  "single_walled" = "single_walled",
  "double_walled" = "double_walled",
  
  # Fuel type indicators (no change needed)
  "is_gasoline" = "is_gasoline",
  "is_diesel" = "is_diesel",
  "is_oil_kerosene" = "is_oil_kerosene",
  "is_jet_fuel" = "is_jet_fuel",
  "is_other" = "is_other",
  
  # Geographic info
  "county_name" = "county_name",
  "county_geoid" = "county_geoid"
)

log_step("✓ Tank panel mappings created", 2)

#------------------------------------------------------------------------------
# 1.2.2 LUST DATA MAPPINGS (Texas → Common schema)
#------------------------------------------------------------------------------
log_step("Defining LUST data variable mappings...", 1)

# Texas LUST → Common schema
tx_lust_mapping <- c(
  "facility_id" = "facility_id",  # Already clean_id applied in load
  "LUST_id" = "lust_id",
  "report_date" = "report_date",
  "nfa_date" = "nfa_date"
)

# EPA LUST → Common schema (already standardized)
epa_lust_mapping <- c(
  "facility_id" = "facility_id",
  "state" = "state",
  "report_date" = "report_date",
  "nfa_date" = "nfa_date"
)

log_step("✓ LUST data mappings created", 2)

#------------------------------------------------------------------------------
# 1.2.3 AUXILIARY DATA MAPPINGS (Texas-specific)
#------------------------------------------------------------------------------
log_step("Defining auxiliary data variable mappings...", 1)

# Texas facility raw data → Common schema
tx_facility_mapping <- c(
  "FACILITY_ID" = "facility_id",
  "SITE_COUNTY" = "county_raw"
)

# FR monthly data → Common schema
fa_monthly_mapping <- c(
  "FACILITY_ID" = "facility_id",
  "YEAR" = "panel_year",
  "MONTH" = "panel_month",
  "DETAIL_TYPE" = "DETAIL_TYPE",    # Keep uppercase for FR-specific vars
  "CATEGORY" = "CATEGORY",
  "ISSUER_NAME" = "ISSUER_NAME"
)

# Zurich 2012 lookup → Common schema
zurich_2012_mapping <- c(
  "FACILITY_ID" = "facility_id",
  "had_zurich_2012" = "had_zurich_2012"
)

log_step("✓ Auxiliary data mappings created", 2)

#------------------------------------------------------------------------------
# 1.2.4 DEFINE VARIABLE TYPE SPECIFICATIONS
#------------------------------------------------------------------------------
log_step("Defining variable type specifications...", 1)

# Variables that should be character
character_vars <- c("facility_id", "tank_id", "state", "county_name", "county_geoid")

# Variables that should be integer
integer_vars <- c("panel_year", "panel_month", "single_walled", "double_walled",
                  "is_gasoline", "is_diesel", "is_oil_kerosene", "is_jet_fuel", "is_other")

# Variables that should be numeric
numeric_vars <- c("capacity")

# Date variables (raw lifecycle dates)
raw_date_vars <- c("tank_installed_date_raw", "tank_closed_date_raw")

# Date variables (panel-bounded dates - will be created)
panel_date_vars <- c("tank_panel_start_date", "tank_panel_end_date")

log_step("✓ Variable type specifications created", 2)

log_step(sprintf("✓ All mappings created in %.1f seconds", 
                difftime(Sys.time(), mapping_start, units="secs")), 1)

#==============================================================================
# 1.3 APPLY COLUMN RENAMING TO ALL DATASETS
#==============================================================================
log_step("\n1.3 Applying systematic column renaming...", 0)
rename_start <- Sys.time()

#------------------------------------------------------------------------------
# 1.3.1 RENAME TEXAS TANK PANEL COLUMNS
#------------------------------------------------------------------------------
log_step("Renaming Texas tank panel columns...", 1)

# Robust renaming: Only rename columns that exist in the mapping AND the data
rename_cols_tx <- intersect(names(tx_tank_months_mapping), names(tx_tank_months))
setnames(tx_tank_months, 
         old = rename_cols_tx, 
         new = unname(tx_tank_months_mapping[rename_cols_tx]))

log_step(sprintf("Renamed %d columns.", length(rename_cols_tx)), 2)
log_step(sprintf("Columns renamed: %s", paste(rename_cols_tx, collapse=", ")), 3)


# Add state column (constant for Texas)
tx_tank_months[, state := "Texas"]

log_step(sprintf("✓ Texas tank panel: %s rows", 
                format(nrow(tx_tank_months), big.mark=",")), 2)

#------------------------------------------------------------------------------
# 1.3.2 RENAME EPA TANK PANEL COLUMNS
#------------------------------------------------------------------------------
log_step("Renaming EPA tank panel columns...", 1)

# Robust renaming
rename_cols_epa <- intersect(names(epa_tank_panel_mapping), names(tank_panel_epa))
setnames(tank_panel_epa, 
         old = rename_cols_epa, 
         new = unname(epa_tank_panel_mapping[rename_cols_epa]))

log_step(sprintf("Renamed %d columns.", length(rename_cols_epa)), 2)
log_step(sprintf("Columns renamed: %s", paste(rename_cols_epa, collapse=", ")), 3)

log_step(sprintf("✓ EPA tank panel: %s rows", 
                format(nrow(tank_panel_epa), big.mark=",")), 2)

#------------------------------------------------------------------------------
# 1.3.3 RENAME LUST DATA COLUMNS
#------------------------------------------------------------------------------
log_step("Renaming LUST data columns...", 1)

# Robust renaming for TX LUST
rename_cols_lust <- intersect(names(tx_lust_mapping), names(TX_LUST_SD))
setnames(TX_LUST_SD,
         old = rename_cols_lust,
         new = unname(tx_lust_mapping[rename_cols_lust]))
         
# Add state column (constant for Texas)
TX_LUST_SD[, state := "Texas"]

log_step(sprintf("✓ Texas LUST: %s rows", 
                format(nrow(TX_LUST_SD), big.mark=",")), 2)

# EPA LUST (already standardized)
log_step(sprintf("✓ EPA LUST: %s rows (already standard)", 
                format(nrow(lust_epa), big.mark=",")), 2)

#------------------------------------------------------------------------------
# 1.3.4 RENAME AUXILIARY DATA COLUMNS
#------------------------------------------------------------------------------
log_step("Renaming auxiliary data columns...", 1)

# Texas facility raw
rename_cols_fac <- intersect(names(tx_facility_mapping), names(tx_fac_raw))
setnames(tx_fac_raw, 
         old = rename_cols_fac, 
         new = unname(tx_facility_mapping[rename_cols_fac]))
log_step(sprintf("✓ TX facility raw: %s rows", format(nrow(tx_fac_raw), big.mark=",")), 2)

# FA monthly
rename_cols_fa <- intersect(names(fa_monthly_mapping), names(fa_monthly))
setnames(fa_monthly, 
         old = rename_cols_fa, 
         new = unname(fa_monthly_mapping[rename_cols_fa]))
log_step(sprintf("✓ FA monthly: %s rows", format(nrow(fa_monthly), big.mark=",")), 2)

# Zurich lookup
rename_cols_zur <- intersect(names(zurich_2012_mapping), names(zurich_2012_lookup))
setnames(zurich_2012_lookup, 
         old = rename_cols_zur, 
         new = unname(zurich_2012_mapping[rename_cols_zur]))
log_step(sprintf("✓ Zurich lookup: %s rows", format(nrow(zurich_2012_lookup), big.mark=",")), 2)

log_step(sprintf("✓ All column renaming completed in %.1f seconds", 
                difftime(Sys.time(), rename_start, units="secs")), 1)

#==============================================================================
# 1.4 STANDARDIZE ID COLUMNS (OPTIMIZED)
#==============================================================================
log_step("\n1.4 Standardizing ID columns...", 0)
id_start <- Sys.time()

# Optimized clean_id that works on vectors
clean_id_vectorized <- function(x) {
  toupper(trimws(x))  # Remove as.character conversion overhead
}

# Texas tank panel - Standardize IDs by reference (efficient)
tx_tank_months[, `:=`(
  facility_id = clean_id_vectorized(facility_id),
  tank_id = clean_id_vectorized(tank_id)
)]
log_step("✓ Texas tank panel IDs standardized", 1)

# EPA tank panel - Standardize IDs by reference
tank_panel_epa[, `:=`(
  facility_id = clean_id_vectorized(facility_id),
  tank_id = clean_id_vectorized(tank_id),
  state = trimws(state)  # Already character
)]
log_step("✓ EPA tank panel IDs standardized", 1)

# LUST data (smaller datasets, still use by-reference)
lust_epa[, `:=`(
  facility_id = clean_id_vectorized(facility_id),
  state = trimws(state)
)]
log_step("✓ EPA LUST IDs standardized", 1)

# TX_LUST_SD facility_id was already cleaned during load
log_step("✓ Texas LUST IDs standardized (during load)", 1)

# Auxiliary data - Standardize IDs by reference
tx_fac_raw[, facility_id := clean_id_vectorized(facility_id)]
fa_monthly[, facility_id := clean_id_vectorized(facility_id)]
zurich_2012_lookup[, facility_id := clean_id_vectorized(facility_id)]
log_step("✓ Auxiliary data IDs standardized", 1)

log_step(sprintf("✓ ID standardization completed in %.1f seconds", 
                difftime(Sys.time(), id_start, units="secs")), 1)

#==============================================================================
# 1.4.1 CREATE COMPOSITE KEY (panel_id) AND SET KEYS [AUDIT ADDITION]
#==============================================================================
log_step("\n1.4.1 Creating composite 'panel_id' and setting keys...", 0)
key_start <- Sys.time()

# --- AUDIT FIX: Create a standardized composite key (facility_id + state) ---
# This is recommended in the audit [QUICK_REFERENCE_CHECKLIST.md, Priority 5]
# and matches the variable you create in downstream scripts [06_DiD_results_9_12.R]

# Define key columns for performance
panel_id_key <- c("facility_id", "state")
tank_key <- c("facility_id", "state", "tank_id")
panel_id_tank_key <- c("panel_id", "tank_id")

# --- Tank Datasets (Tank-level) ---
log_step("Applying panel_id to tank-level datasets...", 1)
tx_tank_months[, panel_id := paste(facility_id, state, sep = "_")]
setkeyv(tx_tank_months, c(panel_id_tank_key, "date")) # Key for time-series joins
log_step(sprintf("  ✓ tx_tank_months: %s unique panel_ids", 
                uniqueN(tx_tank_months$panel_id)), 2)

tank_panel_epa[, panel_id := paste(facility_id, state, sep = "_")]
setkeyv(tank_panel_epa, panel_id_tank_key)
log_step(sprintf("  ✓ tank_panel_epa: %s unique panel_ids", 
                uniqueN(tank_panel_epa$panel_id)), 2)

# --- LUST Datasets (Facility-level) ---
log_step("Applying panel_id to facility-level datasets...", 1)
lust_epa[, panel_id := paste(facility_id, state, sep = "_")]
setkeyv(lust_epa, c('panel_id', "report_date"))
log_step(sprintf("  ✓ lust_epa: %s unique panel_ids", 
                uniqueN(lust_epa$panel_id)), 2)

TX_LUST_SD[, panel_id := paste(facility_id, state, sep = "_")]
setkeyv(TX_LUST_SD, c('panel_id', "report_date"))
log_step(sprintf("  ✓ TX_LUST_SD: %s unique panel_ids", 
                uniqueN(TX_LUST_SD$panel_id)), 2)

# --- Auxiliary Datasets (Facility-level) ---
tx_fac_raw[, panel_id := paste(facility_id, "Texas", sep = "_")] # State is implicit
setkeyv(tx_fac_raw, 'panel_id')
log_step(sprintf("  ✓ tx_fac_raw: %s unique panel_ids", 
                uniqueN(tx_fac_raw$panel_id)), 2)

fa_monthly[, panel_id := paste(facility_id, "Texas", sep = "_")] # State is implicit
setkeyv(fa_monthly, c('panel_id', "panel_year", "panel_month"))
log_step(sprintf("  ✓ fa_monthly: %s unique panel_ids", 
                uniqueN(fa_monthly$panel_id)), 2)

zurich_2012_lookup[, panel_id := paste(facility_id, "Texas", sep = "_")] # State is implicit
setkeyv(zurich_2012_lookup, 'panel_id')
log_step(sprintf("  ✓ zurich_2012_lookup: %s unique panel_ids", 
                uniqueN(zurich_2012_lookup$panel_id)), 2)

log_step(sprintf("✓ panel_id created and keys set in %.1f seconds", 
                difftime(Sys.time(), key_start, units="secs")), 1)





#==============================================================================
# 1.5 PROCESS AND CLEAN DATE VARIABLES (TANK LIFECYCLE DATES)
#==============================================================================
log_step("\n1.5 Processing tank lifecycle date variables...", 0)
date_start <- Sys.time()

# This section is critical for creating two sets of dates:
# 1. `_raw` dates: The original, unaltered dates from the source data.
#    These are preserved for accurate age calculations.
# 2. `_panel_` dates: The dates bounded by the 1970-2025 panel window.
#    These are used to construct the panel backbone.
# This dual-date approach is praised by the audit (Finding 5.1).

#------------------------------------------------------------------------------
# 1.5.1 COERCE DATES TO STANDARD FORMAT
#------------------------------------------------------------------------------
log_step("Coercing dates to standard 'Date' class...", 1)

# Ensure all date columns (which may be IDate, char, etc.) are
# converted to the standard R Date class for safe comparison.
# This prevents join failures due to mismatched date types.

# Texas tank panel - raw lifecycle dates
tx_tank_months[, `:=`(
  tank_installed_date_raw = coerce_date(tank_installed_date_raw),
  tank_closed_date_raw = coerce_date(tank_closed_date_raw)
)]

# EPA tank panel - raw lifecycle dates
tank_panel_epa[, `:=`(
  tank_installed_date_raw = coerce_date(tank_installed_date_raw),
  tank_closed_date_raw = coerce_date(tank_closed_date_raw)
)]

# LUST data
lust_epa[, report_date := coerce_date(report_date)]
TX_LUST_SD[, report_date := coerce_date(report_date)]

log_step("✓ All key date columns coerced to 'Date' class", 2)

#------------------------------------------------------------------------------
# 1.5.2 IDENTIFY AND FLAG BAD INSTALLATION DATES
#------------------------------------------------------------------------------
log_step("Identifying bad installation dates...", 1)

# Bad install dates (NA, or nonsensical years) are critical to remove,
# as they make age calculation impossible.
# We flag them and set them to a "sentinel value" (9999-09-01) for later removal.

# Texas tank panel
tx_tank_months[, bad_install_date := as.integer(
  is.na(tank_installed_date_raw) | 
  year(tank_installed_date_raw) < 1900 | 
  year(tank_installed_date_raw) > 2025
)]
n_bad_tx <- sum(tx_tank_months$bad_install_date)
tx_tank_months[bad_install_date == 1, 
               tank_installed_date_raw := as.Date("9999-09-01")]

# EPA tank panel
tank_panel_epa[, bad_install_date := as.integer(
  is.na(tank_installed_date_raw) | 
  year(tank_installed_date_raw) < 1900 | 
  year(tank_installed_date_raw) > 2025
)]
n_bad_epa <- sum(tank_panel_epa$bad_install_date)
tank_panel_epa[bad_install_date == 1, 
               tank_installed_date_raw := as.Date("9999-09-01")]

log_step(sprintf("TX: %s tanks flagged with bad install dates (%.1f%%)", 
                format(n_bad_tx, big.mark=","),
                100 * n_bad_tx / nrow(tx_tank_months)), 2)
log_step(sprintf("EPA: %s tanks flagged with bad install dates (%.1f%%)", 
                format(n_bad_epa, big.mark=","),
                100 * n_bad_epa / nrow(tank_panel_epa)), 2)
log_step("✓ Bad installation dates flagged and set to 9999-09-01", 2)

#------------------------------------------------------------------------------
# 1.5.3 CLEAN CLOSURE DATES (OPTIONAL, CAN BE MISSING)
#------------------------------------------------------------------------------
log_step("Cleaning corrupted closure dates...", 1)
# Closure dates can be legitimately NA (tank is still active).
# We only clean dates that are present but nonsensical (e.g., year 1800).

# Texas tank panel
tx_tank_months[, bad_closure_date := as.integer(
  !is.na(tank_closed_date_raw) & (
    year(tank_closed_date_raw) < 1900 | 
    year(tank_closed_date_raw) > 2030
  )
)]
n_bad_close_tx <- sum(tx_tank_months$bad_closure_date)
if (n_bad_close_tx > 0) {
  log_step(sprintf("TX: %s tanks with corrupted closure dates (set to NA)", 
                  format(n_bad_close_tx, big.mark=",")), 2)
  tx_tank_months[bad_closure_date == 1, tank_closed_date_raw := NA_Date_]
}

# EPA tank panel
tank_panel_epa[, bad_closure_date := as.integer(
  !is.na(tank_closed_date_raw) & (
    year(tank_closed_date_raw) < 1900 | 
    year(tank_closed_date_raw) > 2030
  )
)]
n_bad_close_epa <- sum(tank_panel_epa$bad_closure_date)
if (n_bad_close_epa > 0) {
  log_step(sprintf("EPA: %s tanks with corrupted closure dates (set to NA)", 
                  format(n_bad_close_epa, big.mark=",")), 2)
  tank_panel_epa[bad_closure_date == 1, tank_closed_date_raw := NA_Date_]
}

log_step("✓ Corrupted (non-NA) closure dates cleaned", 2)

#==============================================================================
# 1.6 CREATE PANEL-BOUNDED DATE VARIABLES (1970-2025)
#==============================================================================
log_step("\n1.6 Creating panel-bounded date variables...", 0)
panel_date_start <- Sys.time()

# Define the fixed boundaries for the entire analysis panel
PANEL_START <- as.Date("1970-01-01")
PANEL_END <- as.Date("2025-12-31")

log_step(sprintf("Panel boundaries set: %s to %s", PANEL_START, PANEL_END), 1)

#------------------------------------------------------------------------------
# 1.6.1 CREATE TANK_PANEL_START_DATE
#------------------------------------------------------------------------------
log_step("Creating tank_panel_start_date...", 1)
# The panel start date is the *later* of the panel boundary (1970) or the
# actual install date. This prevents creating panel-months before 1970.
# Bad install dates are automatically set to PANEL_START.

# Texas tank panel
tx_tank_months[, tank_panel_start_date := fifelse(
  bad_install_date == 1 | as.Date(tank_installed_date_raw) < PANEL_START,
  PANEL_START,
  as.Date(tank_installed_date_raw) 
)]

# EPA tank panel
tank_panel_epa[, tank_panel_start_date := fifelse(
  bad_install_date == 1 | as.Date(tank_installed_date_raw) < PANEL_START,
  PANEL_START,
  as.Date(tank_installed_date_raw)
)]

# Validate
n_invalid_start_tx <- tx_tank_months[tank_panel_start_date < PANEL_START, .N]
n_invalid_start_epa <- tank_panel_epa[tank_panel_start_date < PANEL_START, .N]
if (n_invalid_start_tx == 0 & n_invalid_start_epa == 0) {
  log_step("✓ All tank_panel_start_date values >= 1970-01-01", 2)
} else {
  log_step(sprintf("⚠ WARNING: %d TX and %d EPA tanks have panel_start < 1970", 
                  n_invalid_start_tx, n_invalid_start_epa), 2)
}


#------------------------------------------------------------------------------
# 1.6.2 CREATE TANK_PANEL_END_DATE (CORRECTED - EXCLUDE PRE-PANEL CLOSURES)
#------------------------------------------------------------------------------
log_step("Creating tank_panel_end_date...", 1)

apply_end_logic <- function(dt) {
  # 1. Initialize: copy raw closed date
  dt[, tank_panel_end_date := as.Date(as.numeric(tank_closed_date_raw), origin = "1970-01-01")]
  
  # 2. Handle ACTIVE tanks (NA close date) -> Set to PANEL_END
  idx_na <- dt[is.na(tank_panel_end_date), which = TRUE]
  if(length(idx_na) > 0) {
    set(dt, i = idx_na, j = "tank_panel_end_date", value = PANEL_END)
  }
  
  # 3. FLAG tanks closed before panel (will be tracked in baseline, not panel)
  dt[, exclude_closed_before_panel := as.integer(
    !is.na(tank_closed_date_raw) & tank_closed_date_raw < PANEL_START
  )]
  
  # For these tanks, set end date to NA (they won't appear in panel)
  # They're already tracked in facility_baseline
  idx_early <- dt[exclude_closed_before_panel == 1, which = TRUE]
  if(length(idx_early) > 0) {
    set(dt, i = idx_early, j = "tank_panel_end_date", value = NA_Date_)
  }
  
  # 4. Handle LATE closures (Closed after 2025) -> Clamp to PANEL_END  
  idx_late <- dt[tank_panel_end_date > PANEL_END, which = TRUE]
  if(length(idx_late) > 0) {
    set(dt, i = idx_late, j = "tank_panel_end_date", value = PANEL_END)
  }
}

apply_end_logic(tx_tank_months)
apply_end_logic(tank_panel_epa)


#------------------------------------------------------------------------------
# 1.6.3 VALIDATE PANEL DATE LOGIC
#------------------------------------------------------------------------------
log_step("Validating panel date logic...", 1)

# Check for logical errors: panel_end must be >= panel_start
n_invalid_range_tx <- tx_tank_months[tank_panel_end_date < tank_panel_start_date, .N]
n_invalid_range_epa <- tank_panel_epa[tank_panel_end_date < tank_panel_start_date, .N]

if (n_invalid_range_tx > 0 || n_invalid_range_epa > 0) {
  log_step(sprintf("⚠ ERROR: %d TX and %d EPA tanks have end_date < start_date!", 
                  n_invalid_range_tx, n_invalid_range_epa), 2)
  
  # Fix by setting end = start for these cases
  tx_tank_months[tank_panel_end_date < tank_panel_start_date, 
                 tank_panel_end_date := tank_panel_start_date]
  tank_panel_epa[tank_panel_end_date < tank_panel_start_date, 
                 tank_panel_end_date := tank_panel_start_date]
  log_step("  Fixed by setting panel_end = panel_start", 2)
} else {
  log_step("✓ All tanks have panel_end >= panel_start", 2)
}

log_step(sprintf("✓ Panel date creation completed in %.1f seconds", 
                difftime(Sys.time(), panel_date_start, units="secs")), 1)

#==============================================================================
# 1.7 COMPREHENSIVE DATE TYPE STANDARDIZATION (Was 1.9)
#==============================================================================
# This section (mislabeled 1.9) is a final check to ensure all date columns
# are standard 'Date' class, not 'IDate', to prevent merge/join failures.
log_step("\n1.7 Ensuring all date columns are standard 'Date' class (not IDate)...", 0)
date_convert_start <- Sys.time()

# Convert all tank panel dates to Date class
tx_tank_months[, `:=`(
  date = as.Date(date),
  tank_installed_date_raw = as.Date(tank_installed_date_raw),
  tank_closed_date_raw = as.Date(tank_closed_date_raw),
  tank_panel_start_date = as.Date(tank_panel_start_date),
  tank_panel_end_date = as.Date(tank_panel_end_date)
)]

tank_panel_epa[, `:=`(
  tank_installed_date_raw = as.Date(tank_installed_date_raw),
  tank_closed_date_raw = as.Date(tank_closed_date_raw),
  tank_panel_start_date = as.Date(tank_panel_start_date),
  tank_panel_end_date = as.Date(tank_panel_end_date)
)]

# Convert LUST dates to Date class
lust_epa[, `:=`(
  report_date = as.Date(report_date),
  nfa_date = as.Date(nfa_date)
)]

TX_LUST_SD[, `:=`(
  report_date = as.Date(report_date),
  nfa_date = as.Date(nfa_date)
)]

cat(sprintf("✓ Date type standardization completed in %.1f seconds\n\n", 
            difftime(Sys.time(), date_convert_start, units="secs")))

#==============================================================================
# 1.8 SUMMARIZE DATE VARIABLE CREATION (Was 1.7)
#==============================================================================
log_step("\n1.8 Date variable summary...", 0)

# This summary shows the impact of the date cleaning and bounding.
# `install_range` shows the true historical range (e.g., "1950-01-01 to 2024-01-01")
# `panel_range` shows the bounded range (e.g., "1970-01-01 to 2025-12-31")

log_step("Texas tank panel date summary:", 1)
tx_date_summary <- tx_tank_months[, .(
  n_bad_install = sum(bad_install_date),
  n_not_closed = sum(is.na(tank_closed_date_raw)),
  n_closed = sum(!is.na(tank_closed_date_raw)),
  install_range = sprintf("%s to %s", 
                         min(tank_installed_date_raw[tank_installed_date_raw != as.Date("9999-09-01")], na.rm=TRUE),
                         max(tank_installed_date_raw[tank_installed_date_raw != as.Date("9999-09-01")], na.rm=TRUE)),
  panel_range = sprintf("%s to %s",
                       min(tank_panel_start_date, na.rm=TRUE),
                       max(tank_panel_end_date, na.rm=TRUE))
)]

log_step(sprintf("  Bad install dates: %s", format(tx_date_summary$n_bad_install, big.mark=",")), 2)
log_step(sprintf("  Tanks not closed: %s (%.1f%%)", 
                format(tx_date_summary$n_not_closed, big.mark=","), 2))
log_step(sprintf("  Install date range (valid): %s", tx_date_summary$install_range), 2)
log_step(sprintf("  Panel date range: %s", tx_date_summary$panel_range), 2)

log_step("EPA tank panel date summary:", 1)
epa_date_summary <- tank_panel_epa[, .(
  n_bad_install = sum(bad_install_date),
  n_not_closed = sum(is.na(tank_closed_date_raw)),
  n_closed = sum(!is.na(tank_closed_date_raw)),
  install_range = sprintf("%s to %s", 
                         min(tank_installed_date_raw[tank_installed_date_raw != as.Date("9999-09-01")], na.rm=TRUE),
                         max(tank_installed_date_raw[tank_installed_date_raw != as.Date("9999-09-01")], na.rm=TRUE)),
  panel_range = sprintf("%s to %s",
                       min(tank_panel_start_date, na.rm=TRUE),
                       max(tank_panel_end_date, na.rm=TRUE))
)]

log_step(sprintf("  Bad install dates: %s", format(epa_date_summary$n_bad_install, big.mark=",")), 2)
log_step(sprintf("  Tanks not closed: %s (%.1f%%)", 
                format(epa_date_summary$n_not_closed, big.mark=","), 2))
log_step(sprintf("  Install date range (valid): %s", epa_date_summary$install_range), 2)
log_step(sprintf("  Panel date range: %s", epa_date_summary$panel_range), 2)

log_step(sprintf("\n✓ Date processing completed in %.1f seconds", 
                difftime(Sys.time(), date_start, units="secs")), 1)


#==============================================================================
# 1.9 IDENTIFY AND REMOVE FACILITIES WITH INVALID DATES (EXPANDED)
#==============================================================================
log_step("\n1.9 Identifying facilities to exclude due to date issues...", 0)
exclude_start <- Sys.time()

# This section removes TWO types of facilities:
# 1. Facilities with bad install dates (age cannot be calculated)
# 2. Facilities that ONLY existed before 1970 (never active during panel period)

#------------------------------------------------------------------------------
# 1.9.1: IDENTIFY PROBLEM TANKS (EXPANDED CRITERIA)
#------------------------------------------------------------------------------
log_step("Identifying problem tanks with bad install dates OR pre-panel-only activity...", 1)

# Texas problem tanks - get full tank inventory
bad_tanks_tx <- tx_tank_months[
  bad_install_date == 1 | exclude_closed_before_panel == 1,  # EXPANDED
  .(
    facility_id,
    state,
    tank_id,
    panel_id,
    reason = fcase(
      bad_install_date == 1, "bad_install",
      exclude_closed_before_panel == 1, "pre_panel_closure",
      default = "unknown"
    ),
    install_year = year(tank_installed_date_raw),
    tank_installed_date_raw,
    tank_closed_date_raw,
    capacity,
    single_walled,
    double_walled,
    is_gasoline,
    is_diesel,
    is_oil_kerosene,
    is_jet_fuel,
    is_other
  )]

# Get unique tanks (collapse tank-months to tank level)
bad_tanks_tx <- unique(bad_tanks_tx, by = c("panel_id", "tank_id"))

# EPA problem tanks - get full tank inventory
bad_tanks_epa <- tank_panel_epa[
  bad_install_date == 1 | exclude_closed_before_panel == 1,  # EXPANDED
  .(
    facility_id,
    state,
    tank_id,
    panel_id,
    reason = fcase(
      bad_install_date == 1, "bad_install",
      exclude_closed_before_panel == 1, "pre_panel_closure",
      default = "unknown"
    ),
    install_year = year(tank_installed_date_raw),
    tank_installed_date_raw,
    tank_closed_date_raw,
    capacity,
    single_walled,
    double_walled,
    is_gasoline,
    is_diesel,
    is_oil_kerosene,
    is_jet_fuel,
    is_other
  )]

# Combine all problem tanks
bad_tanks_all <- rbindlist(list(bad_tanks_tx, bad_tanks_epa), use.names = TRUE, fill = TRUE)
bad_tanks_all <- unique(bad_tanks_all, by = c("panel_id", "tank_id"))

#------------------------------------------------------------------------------
# 1.9.2: IDENTIFY FACILITIES TO EXCLUDE (ONLY "DEAD" FACILITIES)
#------------------------------------------------------------------------------
log_step("Identifying facilities where ALL tanks are problematic (dead facilities)...", 1)

# Count total tanks per facility (from unified panel, which excludes bad facilities from earlier)
all_tanks_count <- rbindlist(list(
  tx_tank_months[, .(panel_id, tank_id)],
  tank_panel_epa[, .(panel_id, tank_id)]
), use.names = TRUE)
all_tanks_count <- unique(all_tanks_count, by = c("panel_id", "tank_id"))
all_tanks_count <- all_tanks_count[, .(total_tanks = .N), by = panel_id]

# Count problem tanks per facility
problem_tanks_count <- bad_tanks_all[, .(problem_tanks = .N), by = panel_id]

# Merge and identify facilities where ALL tanks are problematic
facility_problem_status <- merge(all_tanks_count, problem_tanks_count, 
                                  by = "panel_id", all.x = TRUE)
facility_problem_status[is.na(problem_tanks), problem_tanks := 0]
facility_problem_status[, all_tanks_bad := as.integer(problem_tanks == total_tanks)]

# Get facilities to exclude (dead facilities only)
bad_facilities <- facility_problem_status[all_tanks_bad == 1]

if (nrow(bad_facilities) > 0) {
  # Add state and facility_id for reporting
  bad_facilities <- merge(
    bad_facilities,
    unique(bad_tanks_all[, .(panel_id, facility_id, state)]),
    by = "panel_id"
  )
  
  # Get breakdown by reason
  reason_summary <- bad_tanks_all[panel_id %in% bad_facilities$panel_id, 
                                   .(n_tanks = .N), by = .(panel_id, reason)]
  reason_summary <- dcast(reason_summary, panel_id ~ reason, value.var = "n_tanks", fill = 0)
  bad_facilities <- merge(bad_facilities, reason_summary, by = "panel_id", all.x = TRUE)
  
  # Save detailed inventory
  fwrite(bad_tanks_all[panel_id %in% bad_facilities$panel_id], 
         here("Data", "Processed", "excluded_tanks_dead_facilities.csv"))
  fwrite(bad_facilities, 
         here("Data", "Processed", "excluded_facilities_dead_before_panel.csv"))
  
  log_step(sprintf("⚠ Identified %s DEAD facilities to exclude", 
                  format(nrow(bad_facilities), big.mark=",")), 2)
  log_step(sprintf("  Reason breakdown:"), 2)
  log_step(sprintf("    - All bad install: %s facilities", 
                  sum(bad_facilities$bad_install > 0 & bad_facilities$pre_panel_closure == 0)), 3)
  log_step(sprintf("    - All pre-panel: %s facilities", 
                  sum(bad_facilities$pre_panel_closure > 0 & bad_facilities$bad_install == 0)), 3)
  log_step(sprintf("    - Mixed: %s facilities", 
                  sum(bad_facilities$bad_install > 0 & bad_facilities$pre_panel_closure > 0)), 3)
  
  #---------------------------------------------------------------------------
  # 1.9.3: DISPLAY STATE-BY-STATE REMOVAL STATISTICS
  #---------------------------------------------------------------------------
  # Calculate facility counts by state BEFORE removal
  state_facility_counts_before <- rbindlist(list(
    tx_tank_months[, .(state = "Texas", n_facilities = uniqueN(panel_id))],
    tank_panel_epa[, .(n_facilities = uniqueN(panel_id)), by = state]
  ))
  
  # Calculate how many facilities we're removing per state
  removal_summary <- bad_facilities[, .N, by = state]
  setnames(removal_summary, "N", "n_removed")
  
  # Merge with before counts
  setkey(state_facility_counts_before, state)
  setkey(removal_summary, state)
  removal_summary <- removal_summary[state_facility_counts_before, on = .(state)]
  removal_summary[is.na(n_removed), n_removed := 0]
  
  # Calculate percentages
  removal_summary[, `:=`(
    pct_removed = 100 * n_removed / n_facilities,
    n_kept = n_facilities - n_removed
  )]
  
  # Sort by percent removed (descending)
  setorder(removal_summary, -pct_removed)
  
  log_step("\n  Dead facility removal by state:", 2)
  log_step("  ┌────────────────────┬──────────┬──────────┬──────────┬──────────┐", 2)
  log_step("  │ State              │ Before   │ Removed  │ Kept     │ % Drop   │", 2)
  log_step("  ├────────────────────┼──────────┼──────────┼──────────┼──────────┤", 2)
  
  for (i in seq_len(nrow(removal_summary))) {
    state_name <- sprintf("%-18s", removal_summary$state[i])
    before <- sprintf("%8s", format(removal_summary$n_facilities[i], big.mark=","))
    removed <- sprintf("%8s", format(removal_summary$n_removed[i], big.mark=","))
    kept <- sprintf("%8s", format(removal_summary$n_kept[i], big.mark=","))
    pct <- sprintf("%7.2f%%", removal_summary$pct_removed[i])
    
    log_step(sprintf("  │ %s │ %s │ %s │ %s │ %s │", 
                    state_name, before, removed, kept, pct), 2)
  }
  
  log_step("  └────────────────────┴──────────┴──────────┴──────────┴──────────┘", 2)
  
  # Overall summary
  total_before <- sum(removal_summary$n_facilities)
  total_removed <- sum(removal_summary$n_removed)
  total_kept <- sum(removal_summary$n_kept)
  total_pct <- 100 * total_removed / total_before
  
  log_step("", 2)
  log_step(sprintf("  TOTAL ACROSS ALL STATES:"), 2)
  log_step(sprintf("    Before:  %s facilities", format(total_before, big.mark=",")), 2)
  log_step(sprintf("    Removed: %s facilities (%.2f%%)", 
                  format(total_removed, big.mark=","), total_pct), 2)
  log_step(sprintf("    Kept:    %s facilities", format(total_kept, big.mark=",")), 2)
  
  #---------------------------------------------------------------------------
  # 1.9.4: REMOVE DEAD FACILITIES FROM DATASETS
  #---------------------------------------------------------------------------
  log_step("\nRemoving dead facilities from datasets...", 1)
  
  bad_panel_ids <- bad_facilities$panel_id
  
  tx_before <- uniqueN(tx_tank_months$panel_id)
  tx_rows_before <- nrow(tx_tank_months)
  epa_before <- uniqueN(tank_panel_epa$panel_id)
  epa_rows_before <- nrow(tank_panel_epa)
  
  # Remove from Texas data
  tx_tank_months <- tx_tank_months[!panel_id %in% bad_panel_ids]
  
  # Remove from EPA data
  tank_panel_epa <- tank_panel_epa[!panel_id %in% bad_panel_ids]
  
  tx_after_fac <- uniqueN(tx_tank_months$panel_id)
  epa_after_fac <- uniqueN(tank_panel_epa$panel_id)
  
  log_step(sprintf("  Texas:"), 2)
  log_step(sprintf("    Facilities removed: %s → %s (-%s)", 
                  format(tx_before, big.mark=","),
                  format(tx_after_fac, big.mark=","),
                  format(tx_before - tx_after_fac, big.mark=",")), 3)
  log_step(sprintf("    Tank-month rows removed: %s → %s (-%s)", 
                  format(tx_rows_before, big.mark=","),
                  format(nrow(tx_tank_months), big.mark=","),
                  format(tx_rows_before - nrow(tx_tank_months), big.mark=",")), 3)
  
  log_step(sprintf("  EPA (non-Texas):"), 2)
  log_step(sprintf("    Facilities removed: %s → %s (-%s)", 
                  format(epa_before, big.mark=","),
                  format(epa_after_fac, big.mark=","),
                  format(epa_before - epa_after_fac, big.mark=",")), 3)
  log_step(sprintf("    Tank rows removed: %s → %s (-%s)", 
                  format(epa_rows_before, big.mark=","),
                  format(nrow(tank_panel_epa), big.mark=","),
                  format(epa_rows_before - nrow(tank_panel_epa), big.mark=",")), 3)
  
} else {
  log_step("✓ No dead facilities require exclusion", 1)
}

log_step(sprintf("\n✓ Dead facility removal completed in %.1f seconds", 
                difftime(Sys.time(), exclude_start, units="secs")), 1)

#==============================================================================
# 1.10 CREATE FACILITY BASELINE (PRE-PANEL CLOSURES)
#==============================================================================
log_step("\n1.10 Creating facility baseline for pre-panel closures...", 0)
baseline_start <- Sys.time()

# Combine all tank data to get baseline counts
all_tanks_for_baseline <- rbindlist(list(
  tx_tank_months[, .(panel_id, facility_id, state, tank_id, 
                     tank_installed_date_raw, tank_closed_date_raw)],
  tank_panel_epa[, .(panel_id, facility_id, state, tank_id,
                     tank_installed_date_raw, tank_closed_date_raw)]
), use.names = TRUE)

# Get unique tanks only
all_tanks_for_baseline <- unique(all_tanks_for_baseline, by = c("panel_id", "tank_id"))

# Identify tanks closed BEFORE panel period (and with valid closure dates)
all_tanks_for_baseline[, closed_before_panel := as.integer(
  !is.na(tank_closed_date_raw) & 
  tank_closed_date_raw < PANEL_START &
  tank_closed_date_raw != as.Date("9999-09-01")
)]

# Aggregate to facility level (FIXED: as.numeric wrapper)
facility_baseline <- all_tanks_for_baseline[, .(
  n_tanks_closed_before_panel = sum(closed_before_panel, na.rm = TRUE),
  earliest_closure_date = suppressWarnings(min(
    ifelse(closed_before_panel == 1, as.numeric(tank_closed_date_raw), NA_real_), 
    na.rm = TRUE
  ))
), by = .(panel_id, facility_id, state)]

# Convert infinite to NA and numeric back to Date
facility_baseline[is.infinite(earliest_closure_date), earliest_closure_date := NA_real_]
facility_baseline[, earliest_closure_date := as.Date(earliest_closure_date, origin = "1970-01-01")]

log_step(sprintf("Facilities with pre-panel closures: %s", 
                 format(sum(facility_baseline$n_tanks_closed_before_panel > 0), big.mark=",")), 1)
log_step(sprintf("Total tanks closed before 1970: %s",
                 format(sum(facility_baseline$n_tanks_closed_before_panel), big.mark=",")), 1)

log_step(sprintf("✓ Facility baseline created in %.1f seconds", 
                 difftime(Sys.time(), baseline_start, units="secs")), 1)

#==============================================================================
# SECTION 1 COMPLETE
#==============================================================================
cat("\n========================================\n")
cat("SECTION 1 COMPLETE\n")
cat(sprintf("Total time: %.1f seconds\n", 
           difftime(Sys.time(), section_start, units="secs")))
cat("========================================\n\n")

cat("✓ Harmonized datasets ready for Section 2:\n")
cat("  - tx_tank_months (CLEANED)\n")
cat("  - tank_panel_epa (CLEANED)\n")
cat("  - lust_epa\n")
cat("  - TX_LUST_SD\n")
cat("  - tx_fac_raw\n")
cat("  - fa_monthly\n")
cat("  - zurich_2012_lookup\n\n")



#==============================================================================
# SECTION 2: CREATE UNIFIED TANK PANEL (CORRECTED)
#
# Purpose: To create a single, unified *tank-level inventory* (one row per tank)
#          by combining the EPA (tank-level) and Texas (tank-month-level) data.
#
# Operations:
#   - Collapse EPA data to be unique by (panel_id, tank_id)
#   - Collapse Texas tank-MONTH data *up* to be unique by (panel_id, tank_id)
#   - Look up and merge Texas county data (using tigris)
#   - rbindlist() the two inventories to create `tank_panel_unified`
#
# This `tank_panel_unified` object is the master *inventory* of all tanks
# that will be used in Section 4 to build the final *facility-month panel*.
#==============================================================================
cat("========================================\n")
cat("SECTION 2: Creating Unified Tank Panel\n")
cat("========================================\n\n")
start_time <- Sys.time()

# Define required columns for final schema
# AUDIT ADDITION: Added 'panel_id' to the required schema
needed_cols <- c(
  "panel_id", "facility_id", "state", "tank_id",
  "tank_installed_date_raw", "tank_closed_date_raw",
  "tank_panel_start_date", "tank_panel_end_date",
  "capacity", "single_walled", "double_walled",
  "is_gasoline", "is_diesel", "is_oil_kerosene", "is_jet_fuel", "is_other",
  "county_name", "county_geoid"
)


#------------------------------------------------------------------------------
# 2.1: COLLAPSE EPA DATA TO TANK LEVEL
#------------------------------------------------------------------------------
log_step("Collapsing EPA data to unique tank level...", 1)
epa_tank_level <- tank_panel_epa[
  state != "Texas", 
  .(
    # FIX: Wrap in as.numeric() to handle Inf returns
    tank_installed_date_raw = suppressWarnings(min(as.numeric(tank_installed_date_raw), na.rm = TRUE)),
    tank_closed_date_raw = suppressWarnings(max(as.numeric(tank_closed_date_raw), na.rm = TRUE)),
    
    tank_panel_start_date = suppressWarnings(min(as.numeric(tank_panel_start_date), na.rm = TRUE)),
    tank_panel_end_date = suppressWarnings(max(as.numeric(tank_panel_end_date), na.rm = TRUE)),
    
    # Tank characteristics
    capacity = suppressWarnings(max(capacity, na.rm = TRUE)),
    single_walled = as.integer(any(single_walled == 1, na.rm = TRUE)),
    double_walled = as.integer(any(double_walled == 1, na.rm = TRUE)),
    is_gasoline = as.integer(any(is_gasoline == 1, na.rm = TRUE)),
    is_diesel = as.integer(any(is_diesel == 1, na.rm = TRUE)),
    is_oil_kerosene = as.integer(any(is_oil_kerosene == 1, na.rm = TRUE)),
    is_jet_fuel = as.integer(any(is_jet_fuel == 1, na.rm = TRUE)),
    is_other = as.integer(any(is_other == 1, na.rm = TRUE)),
    
    # Geographic info
    county_name = na.omit(county_name)[1],
    county_geoid = as.character(na.omit(county_geoid)[1]),
    panel_id = panel_id[1]
  ), by = .(facility_id, tank_id, state)] 

# Clean infinite values and convert back to Date
for (v in c("tank_installed_date_raw", "tank_closed_date_raw",
            "tank_panel_start_date", "tank_panel_end_date")) {
  epa_tank_level[is.infinite(get(v)), (v) := NA_real_]
  # Convert numeric back to Date
  set(epa_tank_level, j = v, value = as.Date(epa_tank_level[[v]], origin = "1970-01-01"))
}
epa_tank_level[is.infinite(capacity), capacity := NA_real_]

# Fill any remaining NAs with panel bounds
epa_tank_level[is.na(tank_panel_start_date), tank_panel_start_date := as.Date("1970-01-01")]
epa_tank_level[is.na(tank_panel_end_date), tank_panel_end_date := as.Date("2025-12-31")]

# Ensure schema consistency
missing_cols_epa <- setdiff(needed_cols, names(epa_tank_level))
if (length(missing_cols_epa) > 0) {
  epa_tank_level[, (missing_cols_epa) := NA]
}
epa_tank_level <- epa_tank_level[, ..needed_cols]

log_step(sprintf("✓ EPA non-TX tanks collapsed: %s", format(nrow(epa_tank_level), big.mark=",")), 2)


#------------------------------------------------------------------------------
# 2.2: COLLAPSE TEXAS DATA TO TANK LEVEL
# (TX data starts as facility-tank-MONTH, need to collapse to facility-tank)
#------------------------------------------------------------------------------

# --- 2.2.1: Create TX County Lookup (Audit Finding 3.2) ---
log_step("Creating Texas county lookup...", 1)
clean_county <- function(x){
  if (is.null(x)) return(character())
  x <- toupper(trimws(as.character(x)))
  x <- gsub("[^A-Z ]", " ", x)
  x <- gsub("\\bCOUNTY\\b", "", x)
  x <- gsub("\\s+", " ", x)
  trimws(x)
}

tx_county_lookup <- NULL
if (nrow(tx_fac_raw) > 0) {
  tx_county_lookup <- tx_fac_raw[
    !is.na(county_raw) & county_raw != "",
    .(facility_id = facility_id,
      panel_id = panel_id,
      county_name_clean = clean_county(county_raw))
  ]
  tx_county_lookup <- unique(tx_county_lookup, by = "panel_id")
  
  # Try to get county GEOID from tigris (with graceful failure handling)
  site_down <- FALSE
  
  if (requireNamespace("tigris", quietly = TRUE)) {
    log_step("Attempting to download Census Bureau county data via tigris...", 2)
    
    tryCatch({
      suppressMessages({
        tx_cnty_sf <- tigris::counties(state = "TX", cb = TRUE, year = 2020, progress_bar = FALSE)
      })
      
      # If successful, create crosswalk
      cnty_xwalk <- data.table(
        county_name_clean = clean_county(tx_cnty_sf$NAME),
        county_geoid = as.character(tx_cnty_sf$GEOID),
        county_name = toupper(as.character(tx_cnty_sf$NAME))
      )
      setkey(cnty_xwalk, county_name_clean)
      setkey(tx_county_lookup, county_name_clean)
      tx_county_lookup <- cnty_xwalk[tx_county_lookup]
      
      log_step("✓ Census Bureau data downloaded successfully", 2)
      
    }, error = function(e) {
      log_step("---------------------------------------------------------------", 2)
      log_step("⚠ WARNING (Finding 3.2): Census Bureau website unavailable.", 2)
      log_step("  This is non-critical, but Texas GEOIDs will be NA.", 2)
      log_step(paste("  Error:", e$message), 2)
      log_step("---------------------------------------------------------------", 2)
      site_down <<- TRUE
    })
    
  } else {
    log_step("Note: 'tigris' package not available, skipping GEOID lookup", 2)
    site_down <- TRUE
  }
  
  # If download failed or tigris not available, set to NA
  if (site_down) {
    tx_county_lookup[, `:=`(
      county_geoid = NA_character_, 
      county_name = county_name_clean
    )]
  }
  
  # Final cleanup: collapse to facility level
  tx_county_lookup <- tx_county_lookup[, .(
    county_name = county_name[1], 
    county_geoid = county_geoid[1]
  ), by = .(panel_id, facility_id)]
  
} else {
  log_step("No TX facility raw data (tx_fac_raw) available, skipping county lookup", 1)
}

# --- 2.2.2: Collapse TX Tank-Months to Tank-Level ---
log_step("Collapsing TX tank-months to unique tank level...", 1)
tx_tank_level <- tx_tank_months[, .(
  # FIX: Wrap in as.numeric()
  tank_installed_date_raw = suppressWarnings(min(as.numeric(tank_installed_date_raw), na.rm = TRUE)),
  tank_closed_date_raw = suppressWarnings(max(as.numeric(tank_closed_date_raw), na.rm = TRUE)),
  
  tank_panel_start_date = suppressWarnings(min(as.numeric(tank_panel_start_date), na.rm = TRUE)),
  tank_panel_end_date = suppressWarnings(max(as.numeric(tank_panel_end_date), na.rm = TRUE)),
  
  # Tank characteristics
  capacity = suppressWarnings(max(capacity, na.rm = TRUE)),
  single_walled = as.integer(any(single_walled == 1, na.rm = TRUE)),
  double_walled = as.integer(any(double_walled == 1, na.rm = TRUE)),
  is_gasoline = as.integer(any(is_gasoline == 1, na.rm = TRUE)),
  is_diesel = as.integer(any(is_diesel == 1, na.rm = TRUE)),
  is_oil_kerosene = as.integer(any(is_oil_kerosene == 1, na.rm = TRUE)),
  is_jet_fuel = as.integer(any(is_jet_fuel == 1, na.rm = TRUE)),
  is_other = as.integer(any(is_other == 1, na.rm = TRUE)),
  panel_id = panel_id[1]
), by = .(facility_id, tank_id, state)]

# Clean infinite values
for (v in c("tank_installed_date_raw", "tank_closed_date_raw",
            "tank_panel_start_date", "tank_panel_end_date")) {
  tx_tank_level[is.infinite(get(v)), (v) := NA_Date_]
  tx_tank_level[[v]] <- as.Date(tx_tank_level[[v]])
}
tx_tank_level[is.infinite(capacity), capacity := NA_real_]

# Fill missing panel dates
tx_tank_level[is.na(tank_panel_start_date), tank_panel_start_date := as.Date("1970-01-01")]
tx_tank_level[is.na(tank_panel_end_date), tank_panel_end_date := as.Date("2025-12-31")]

# Attach county info
if (!is.null(tx_county_lookup) && nrow(tx_county_lookup) > 0) {
  # Use keys for efficient merge
  setkey(tx_tank_level, panel_id, facility_id)
  setkey(tx_county_lookup, panel_id, facility_id)
  tx_tank_level <- tx_county_lookup[tx_tank_level, on = .(panel_id, facility_id)]
} else {
  tx_tank_level[, `:=`(county_name = NA_character_, county_geoid = NA_character_)]
}

# Ensure schema consistency
missing_cols_tx <- setdiff(needed_cols, names(tx_tank_level))
if (length(missing_cols_tx) > 0) {
  tx_tank_level[, (missing_cols_tx) := NA]
}
tx_tank_level <- tx_tank_level[, ..needed_cols] # Reorder columns

log_step(sprintf("✓ TX tanks collapsed: %s", format(nrow(tx_tank_level), big.mark=",")), 2)

#------------------------------------------------------------------------------
# 2.3: COMBINE EPA AND TX INTO UNIFIED TANK PANEL
#------------------------------------------------------------------------------
log_step("Combining EPA and TX tank inventories...", 1)

# Your insight: fill=TRUE is suspicious. If the schemas are perfectly aligned
# (which we just forced them to be with `..needed_cols`), fill=TRUE is
# redundant and masks potential errors. We can safely remove it.
tank_panel_unified <- rbindlist(list(epa_tank_level, tx_tank_level), use.names = TRUE)

# Set the key for the new unified panel
setkeyv(tank_panel_unified, c("panel_id", "tank_id"))

# Section 2 Validation
cat("\n  Section 2 Validation:\n")
cat(sprintf("    Total unified tanks: %s\n", format(nrow(tank_panel_unified), big.mark=",")))
cat(sprintf("    Unique panel_ids: %s\n", uniqueN(tank_panel_unified$panel_id)))
cat(sprintf("    Unique tank_ids (panel_id + tank_id): %s\n", 
            uniqueN(tank_panel_unified[, .(panel_id, tank_id)])))
cat(sprintf("    Check: EPA + TX = Total? %s\n", 
            nrow(tank_panel_unified) == (nrow(epa_tank_level) + nrow(tx_tank_level))))
cat(sprintf("    Tanks per facility (mean): %.1f\n", 
            nrow(tank_panel_unified) / uniqueN(tank_panel_unified$panel_id)))

# Date range validation
cat(sprintf("    Panel date range: %s to %s\n",
            min(tank_panel_unified$tank_panel_start_date, na.rm=TRUE),
            max(tank_panel_unified$tank_panel_end_date, na.rm=TRUE)))

log_memory("After unification")
cat(sprintf("  Time elapsed: %.1f seconds\n\n", difftime(Sys.time(), start_time, units="secs")))




#==============================================================================
# SECTION 3: CREATE FACILITY-MONTH BACKBONE
#==============================================================================
cat("SECTION 3: Building facility-month backbone...\n")
start_time <- Sys.time()

# Calculate active ranges
facility_active <- tank_panel_unified[, .(
  start_date = min(tank_panel_start_date, na.rm=TRUE),
  end_date = max(tank_panel_end_date, na.rm=TRUE),
  county_name = na.omit(county_name)[1],
  county_geoid = as.character(na.omit(county_geoid)[1])
), by = .(facility_id, state, panel_id)]

# Expand to monthly
facility_active[, n_months := as.integer((year(end_date) - year(start_date)) * 12 + month(end_date) - month(start_date) + 1)]
panel_dates <- facility_active[rep(1:.N, n_months)][, date := seq(start_date[1], by="month", length.out=.N), by=.(panel_id)]
panel_dates[, `:=`(panel_year = year(date), panel_month = month(date))]

#==============================================================================
# SECTION 4: COMPUTE MONTHLY TANK COMPOSITION (FIXED FOR NON-EQUI JOIN)
#==============================================================================
cat("SECTION 4: Computing monthly tank composition...\n")
start_time <- Sys.time()

#------------------------------------------------------------------------------
# 4.1: PREPARE TANK DATA FOR JOINING
#------------------------------------------------------------------------------
cat("  Preparing tank data for monthly joins...\n")

# Create clean tank dataset with only what we need
tanks_for_join <- tank_panel_unified[, .(
  facility_id,
  state,
  tank_id,
  # Panel dates (already bounded to 1970-2025) - convert to numeric for join
  tank_start_num = as.numeric(as.Date(tank_panel_start_date)),
  tank_end_num = as.numeric(as.Date(tank_panel_end_date)),
  # Original install date for age calculation
  install_date_for_age = as.Date(fifelse(
    is.na(tank_installed_date_raw) | tank_installed_date_raw == as.Date("9999-09-01"),
    tank_panel_start_date,
    tank_installed_date_raw
  )),
  # Tank characteristics
  capacity,
  single_walled,
  double_walled,
  is_gasoline,
  is_diesel,
  is_oil_kerosene,
  is_jet_fuel,
  is_other
)]

cat(sprintf("    Tank records ready: %s\n", format(nrow(tanks_for_join), big.mark=",")))


#------------------------------------------------------------------------------
# 4.2: JOIN TANKS TO PANEL BACKBONE (OPTIMIZED)
#------------------------------------------------------------------------------
cat("  Joining tanks to panel backbone by state...\n")

states_to_process <- unique(panel_dates$state)
n_states <- length(states_to_process)

tank_month_list <- vector("list", n_states)

for (i in seq_along(states_to_process)) {
  st <- states_to_process[i]
  cat(sprintf("    [%2d/%2d] %s...", i, n_states, st))
  
  # Subset to current state
  backbone_st <- panel_dates[state == st]
  tanks_st <- tanks_for_join[state == st]
  
  if (nrow(backbone_st) == 0 || nrow(tanks_st) == 0) {
    cat(" skipped (no data)\n")
    next
  }
  
  # Convert dates to numeric ONCE for the entire state
  backbone_st[, date_num := as.numeric(date)]
  
  # Non-equi join - CRITICAL: specify which columns to keep from backbone
  active_tanks <- tanks_st[backbone_st, 
                           on = .(facility_id, 
                                  tank_start_num <= date_num,
                                  tank_end_num >= date_num),
                           nomatch = 0L,
                           allow.cartesian = TRUE]
  
  if (nrow(active_tanks) == 0) {
    cat(" no active tanks\n")
    next
  }
  
  # CRITICAL FIX: After join, backbone columns have original names
  # We need to extract date from the existing columns
  active_tanks[, `:=`(
    date = as.Date(tank_start_num, origin = "1970-01-01"),  # Use tank_start_num which now has date_num value
    panel_year = year(as.Date(tank_start_num, origin = "1970-01-01")),
    panel_month = month(as.Date(tank_start_num, origin = "1970-01-01"))
  )]
  
  # Calculate tank age using install_date_for_age
  active_tanks[, tank_age_years := as.numeric(
    difftime(date, install_date_for_age, units = "days")
  ) / 365.25]
  
  # Aggregate to facility-month level
  tank_month_list[[i]] <- active_tanks[, .(
    active_tanks = .N,
    avg_tank_age = mean(tank_age_years, na.rm = TRUE),
    min_tank_age = min(tank_age_years, na.rm = TRUE),
    max_tank_age = max(tank_age_years, na.rm = TRUE),
    total_capacity = sum(capacity, na.rm = TRUE),
    avg_capacity = mean(capacity, na.rm = TRUE),
    single_tanks = sum(single_walled, na.rm = TRUE),
    double_tanks = sum(double_walled, na.rm = TRUE),
    has_gasoline = as.integer(any(is_gasoline == 1, na.rm = TRUE)),
    has_diesel = as.integer(any(is_diesel == 1, na.rm = TRUE)),
    has_oil_kerosene = as.integer(any(is_oil_kerosene == 1, na.rm = TRUE)),
    has_jet_fuel = as.integer(any(is_jet_fuel == 1, na.rm = TRUE)),
    has_other = as.integer(any(is_other == 1, na.rm = TRUE))
  ), by = .(facility_id, state, date)]
  
  cat(sprintf(" %s rows\n", format(nrow(tank_month_list[[i]]), big.mark=",")))
  
  # Memory management
  rm(active_tanks, backbone_st, tanks_st)
  if (i %% 5 == 0) invisible(gc())
}


#------------------------------------------------------------------------------
# 4.3: COMBINE STATE RESULTS
#------------------------------------------------------------------------------
cat("  Combining state results...\n")

tank_composition <- rbindlist(
  tank_month_list[!sapply(tank_month_list, is.null)],
  use.names = TRUE
)

# Clean up
rm(tank_month_list, tanks_for_join)
invisible(gc())

cat(sprintf("    Combined: %s facility-months with active tanks\n", 
            format(nrow(tank_composition), big.mark=",")))

#------------------------------------------------------------------------------
# 4.4: MERGE BACK TO PANEL BACKBONE
#------------------------------------------------------------------------------
cat("  Merging tank composition to panel backbone...\n")

# Ensure date columns are proper Date class before final merge
tank_composition[, date := as.Date(date)]
panel_dates[, date := as.Date(date)]

# Merge tank stats back to full panel (including months with zero tanks)
setkey(tank_composition, facility_id, state, date)
setkey(panel_dates, facility_id, state, date)

monthly <- tank_composition[panel_dates, on = .(facility_id, state, date)]

# Fill missing values for facility-months with no active tanks
integer_fills <- c("active_tanks", "single_tanks", "double_tanks", 
                   "has_gasoline", "has_diesel", "has_oil_kerosene", 
                   "has_jet_fuel", "has_other")

numeric_fills <- c("avg_tank_age", "min_tank_age", "max_tank_age", 
                   "total_capacity", "avg_capacity")

for (col in integer_fills) {
  if (col %in% names(monthly)) {
    monthly[is.na(get(col)), (col) := 0L]
  }
}

for (col in numeric_fills) {
  if (col %in% names(monthly)) {
    monthly[is.na(get(col)), (col) := 0]
    # Clean infinites
    monthly[is.infinite(get(col)), (col) := NA_real_]
  }
}

#------------------------------------------------------------------------------
# 4.5: CREATE DERIVED COMPOSITION VARIABLES
#------------------------------------------------------------------------------
cat("  Creating derived composition variables...\n")

monthly[, `:=`(
  # Wall type shares (only calculate when tanks exist)
  single_share = fifelse(active_tanks > 0, single_tanks / active_tanks, 0),
  double_share = fifelse(active_tanks > 0, double_tanks / active_tanks, 0),
  
  # Capacity per tank (when tanks exist)
  capacity_per_tank = fifelse(active_tanks > 0, total_capacity / active_tanks, 0),
  
  # Facility has tanks flag
  has_tanks = as.integer(active_tanks > 0),
  has_single_walled = as.integer(single_tanks > 0),
  has_double_walled = as.integer(double_tanks > 0)
)]

#------------------------------------------------------------------------------
# 4.6: VALIDATION
#------------------------------------------------------------------------------
cat("\n  Section 4 Validation:\n")
cat(sprintf("    Total facility-months: %s\n", 
            format(nrow(monthly), big.mark=",")))
cat(sprintf("    Facility-months with tanks: %s (%.1f%%)\n", 
            format(sum(monthly$active_tanks > 0), big.mark=","),
            100 * mean(monthly$active_tanks > 0)))
cat(sprintf("    Mean active tanks (when > 0): %.2f\n", 
            mean(monthly[active_tanks > 0]$active_tanks)))
cat(sprintf("    Mean tank age (when tanks exist): %.1f years\n", 
            mean(monthly[active_tanks > 0]$avg_tank_age, na.rm = TRUE)))
cat(sprintf("    Mean capacity per facility: %s gallons\n", 
            format(round(mean(monthly[active_tanks > 0]$total_capacity, na.rm = TRUE)), 
                   big.mark=",")))

# Age distribution
age_summary <- monthly[active_tanks > 0 & !is.na(avg_tank_age), 
                       .(min_age = min(avg_tank_age),
                         q25 = quantile(avg_tank_age, 0.25),
                         median = median(avg_tank_age),
                         q75 = quantile(avg_tank_age, 0.75),
                         max_age = max(avg_tank_age))]

cat(sprintf("    Age distribution: min=%.1f, Q1=%.1f, median=%.1f, Q3=%.1f, max=%.1f\n",
            age_summary$min_age, age_summary$q25, age_summary$median, 
            age_summary$q75, age_summary$max_age))

# Wall type distribution
wall_summary <- monthly[active_tanks > 0, .(
  pct_single_only = 100 * mean(single_tanks > 0 & double_tanks == 0),
  pct_double_only = 100 * mean(double_tanks > 0 & single_tanks == 0),
  pct_mixed = 100 * mean(single_tanks > 0 & double_tanks > 0)
)]

cat(sprintf("    Wall types: %.1f%% single-only, %.1f%% double-only, %.1f%% mixed\n",
            wall_summary$pct_single_only, wall_summary$pct_double_only, 
            wall_summary$pct_mixed))

# Check for data quality issues
n_negative_age <- monthly[avg_tank_age < 0, .N]
n_very_old <- monthly[avg_tank_age > 80, .N]
n_huge_capacity <- monthly[total_capacity > 500000, .N]

if (n_negative_age > 0) {
  cat(sprintf("    ⚠ WARNING: %s facility-months have negative tank age\n", 
              format(n_negative_age, big.mark=",")))
}
if (n_very_old > 0) {
  cat(sprintf("    ⚠ Note: %s facility-months have tanks > 80 years old\n", 
              format(n_very_old, big.mark=",")))
}
if (n_huge_capacity > 0) {
  cat(sprintf("    ⚠ Note: %s facility-months have capacity > 500,000 gallons\n", 
              format(n_huge_capacity, big.mark=",")))
}

log_memory("After tank composition")
cat(sprintf("  Time elapsed: %.1f seconds\n\n", 
            difftime(Sys.time(), start_time, units = "secs")))

# Clean up temporary objects
rm(tank_composition, age_summary, wall_summary)
invisible(gc())



#==============================================================================
# SECTION 5: MERGE MONTHLY EVENTS (OPTIMIZED)
#==============================================================================
log_step("SECTION 5: Merging Monthly Events...", 0)

# 5.1 Prepare all event tables FIRST
all_leaks <- rbind(lust_epa[, .(panel_id, report_date)], TX_LUST_SD[, .(panel_id, report_date)])
all_leaks <- unique(all_leaks)
monthly_leaks <- all_leaks[, .(leak_incident = 1L), by = .(panel_id, date = floor_date(report_date, "month"))]

closures <- tank_panel_unified[!is.na(tank_closed_date_raw) & tank_closed_date_raw <= PANEL_END, 
                               .(tanks_closed = .N, 
                                 capacity_closed = sum(capacity, na.rm=TRUE),
                                 single_walled_closed = sum(single_walled, na.rm=TRUE)), 
                               by = .(panel_id, date = floor_date(tank_closed_date_raw, "month"))]

installs <- tank_panel_unified[!is.na(tank_installed_date_raw) & tank_installed_date_raw >= PANEL_START,
                               .(tanks_installed = .N,
                                 capacity_installed = sum(capacity, na.rm=TRUE),
                                 double_walled_installed = sum(double_walled, na.rm=TRUE)),
                               by = .(panel_id, date = floor_date(tank_installed_date_raw, "month"))]

# 5.2 Single combined merge using reduce
setkey(monthly, panel_id, date)
setkey(monthly_leaks, panel_id, date)
setkey(closures, panel_id, date)
setkey(installs, panel_id, date)

monthly <- monthly_leaks[monthly, on = .(panel_id, date)]
monthly <- closures[monthly, on = .(panel_id, date)]
monthly <- installs[monthly, on = .(panel_id, date)]

# Fill NAs in one operation
fill_cols <- c("leak_incident", "tanks_closed", "capacity_closed", "single_walled_closed",
               "tanks_installed", "capacity_installed", "double_walled_installed")
for(col in fill_cols) {
  if(col %in% names(monthly)) monthly[is.na(get(col)), (col) := 0L]
}

# 5.3 Replacements
monthly[, `:=`(
  replacement_event = as.integer(tanks_closed > 0 & tanks_installed > 0),
  single_to_double_replacement = as.integer(tanks_closed > 0 & tanks_installed > 0 & 
                                             single_walled_closed > 0 & double_walled_installed > 0)
)]

# 5.4 FR Data - remove date column before merge to avoid collision
fa_monthly[, date := NULL]  # Prevent date.x/date.y issue
monthly <- fa_monthly[monthly, on = .(panel_id, panel_year, panel_month)]
monthly <- zurich_2012_lookup[monthly, on = .(panel_id)]
# --- CRITICAL: Create dropped_by_zurich Variable ---
# Logic: Texas facility had Zurich in 2012 baseline, now in post-2012 period with no coverage
log_step("Creating 'dropped_by_zurich' treatment variable...", 0)
monthly[state == "Texas", dropped_by_zurich := as.integer(
  had_zurich_2012 == 1 & 
  panel_year >= 2012 & 
  is.na(CATEGORY)
)]
# Fill NAs with 0 (non-Texas or pre-2012 or never had Zurich)
monthly[is.na(dropped_by_zurich), dropped_by_zurich := 0L]
log_step(sprintf("  Facilities affected: %d unique panel_ids", 
                 uniqueN(monthly[dropped_by_zurich == 1]$panel_id)), 1)
# --- SURGICAL ADDITION: Save Intermediate Monthly Panel ---
# Required for Granularity Analysis (Seasonality, Bunching) in 01_Master_Descriptives.R
log_step("Saving intermediate monthly panel for granular analysis...", 0)
output_monthly <- here("Data", "Processed", "facility_leak_behavior_monthly_intermediate.rds")
saveRDS(monthly, output_monthly)
log_step(sprintf("✓ Saved Monthly RDS: %s", output_monthly), 1)


#==============================================================================
# SECTION 6: ANNUAL AGGREGATION
#==============================================================================
log_step("SECTION 6: Aggregating to Annual Panel...", 0)

# 6.1 End-of-Year Stocks (CRITICAL: Explicit Sort for .SD[.N])
setorder(monthly, panel_id, panel_year, panel_month)
december_obs <- monthly[, .SD[.N], by = .(panel_id, panel_year), 
                        .SDcols = c("active_tanks", "total_capacity", "avg_tank_age", 
                                    "single_tanks", "double_tanks", 
                                    "has_single_walled", "has_double_walled")]
setnames(december_obs, old=names(december_obs)[3:9], new=paste0(names(december_obs)[3:9], "_dec"))

# 6.2 Annual Flows & Summaries
annual <- monthly[, .(
  state = first(state),
  facility_id = first(facility_id),
  county_name = na.omit(county_name)[1],
  
  # Flows (Sums)
  n_leaks = sum(leak_incident, na.rm=TRUE),
  leak_year = as.integer(sum(leak_incident, na.rm=TRUE) > 0),
  n_closures = sum(tanks_closed, na.rm=TRUE),
  closure_year = as.integer(sum(tanks_closed, na.rm=TRUE) > 0),
  n_installs = sum(tanks_installed, na.rm=TRUE),
  n_retrofits = sum(replacement_event, na.rm=TRUE),
  retrofit_year = as.integer(sum(replacement_event, na.rm=TRUE) > 0),
  n_single_to_double = sum(single_to_double_replacement, na.rm=TRUE),
  single_to_double_year = as.integer(sum(single_to_double_replacement, na.rm=TRUE) > 0),
  
  # Sum Flows
  capacity_installed_year = sum(capacity_installed, na.rm=TRUE),
  capacity_closed_year = sum(capacity_closed, na.rm=TRUE),
  double_walled_installed_year = sum(double_walled_installed, na.rm=TRUE),
  
  # Mean Stocks
  active_tanks_mean = mean(active_tanks, na.rm=TRUE),
  total_capacity_mean = mean(total_capacity, na.rm=TRUE),
  avg_tank_age_mean = mean(avg_tank_age, na.rm=TRUE),
  
  # Fuel
  has_gasoline_year = max(has_gasoline, na.rm=TRUE),
  has_diesel_year = max(has_diesel, na.rm=TRUE),
  
  # FR Flags
  fr_coverage_share = if(any(!is.na(CATEGORY))) mean(!is.na(CATEGORY) & CATEGORY != "None", na.rm=TRUE) else 0,
  share_private = if("uses_private" %in% names(.SD)) mean(uses_private, na.rm=TRUE) else 0,
  share_state_fund = if("uses_state_fund" %in% names(.SD)) mean(uses_state_fund, na.rm=TRUE) else 0,
  share_self = if("uses_self" %in% names(.SD)) mean(uses_self, na.rm=TRUE) else 0,
  n_gap_months = if("coverage_gap_month" %in% names(.SD)) sum(coverage_gap_month, na.rm=TRUE) else 0,
  any_gap = if("coverage_gap_month" %in% names(.SD)) max(coverage_gap_month, na.rm=TRUE) else 0,
  n_transitions = if("transition_month" %in% names(.SD)) sum(transition_month, na.rm=TRUE) else 0,
  any_transition = if("transition_month" %in% names(.SD)) max(transition_month, na.rm=TRUE) else 0,
  
  modal_detail_type = {
    tbl <- table(DETAIL_TYPE)
    if(length(tbl)>0) names(which.max(tbl)) else NA_character_
  },
  
# Removed redundant dropped_by_zurich
  # Safe aggregation for had_zurich_2012 to avoid -Inf warning
  had_zurich_2012 = if(all(is.na(had_zurich_2012))) 0L else max(had_zurich_2012, na.rm=TRUE)  
), by = .(panel_id, panel_year)]

# 6.3 Merge Stocks
annual <- merge(annual, december_obs, by = c("panel_id", "panel_year"), all.x = TRUE)

# 6.4 Derived Flow Variables (Diff of End-of-Year Stocks)
setorder(annual, panel_id, panel_year) 
annual[, `:=`(
  capacity_change_year = total_capacity_dec - shift(total_capacity_dec, 1, type = "lag"),
  net_tank_change = active_tanks_dec - shift(active_tanks_dec, 1, type = "lag")
), by = panel_id]

annual[, `:=`(
  capacity_increased = as.integer(capacity_change_year > 0),
  capacity_decreased = as.integer(capacity_change_year < 0),
  is_motor_fuel = as.integer(has_gasoline_year == 1 | has_diesel_year == 1)
)]



#==============================================================================
# SECTION 7: ROBUST LEAK CLASSIFICATION (4 SPECS)
#==============================================================================
log_step("SECTION 7: Classifying Leaks (4 Robustness Specs)...", 0)

closures_exact <- tank_panel_unified[!is.na(tank_closed_date_raw), 
                                     .(panel_id, closure_date = tank_closed_date_raw)]
leaks_exact <- all_leaks[, .(panel_id, report_date)]

if(nrow(closures_exact) > 0 && nrow(leaks_exact) > 0) {
  setkey(closures_exact, panel_id)
  setkey(leaks_exact, panel_id)
  pairs <- leaks_exact[closures_exact, on = .(panel_id), allow.cartesian=TRUE]
  pairs[, diff := as.numeric(report_date - closure_date)]
  
pairs[, `:=`(
    is_rev_prim = diff >= 0 & diff <= 60, is_kno_prim = diff < -180, is_ind_prim = diff >= -180 & diff < 0,
    is_rev_nar = diff >= 0 & diff <= 30, is_kno_nar = diff < -365,
    is_rev_wid = diff >= 0 & diff <= 90, is_kno_wid = diff < -90,
    # Added Regulatory Spec:
    is_rev_reg = diff >= 0 & diff <= 45, is_kno_reg = diff < -180
  )]

  closure_flags <- pairs[, .(
    has_rev_prim = any(is_rev_prim), has_kno_prim = any(is_kno_prim), has_ind_prim = any(is_ind_prim),
    has_rev_nar = any(is_rev_nar), has_kno_nar = any(is_kno_nar),
    has_rev_wid = any(is_rev_wid), has_kno_wid = any(is_kno_wid),
    has_rev_reg = any(is_rev_reg), has_kno_reg = any(is_kno_reg)
  ), by = .(panel_id, closure_date)]
  
  closure_flags[, panel_year := year(closure_date)]
  
annual_flags <- closure_flags[, .(
    tank_closure_revealed = as.integer(any(has_rev_prim)),
    tank_closure_known_leak = as.integer(any(has_kno_prim)),
    tank_closure_indeterminate = as.integer(any(has_ind_prim)),
    tank_closure_revealed_narrow = as.integer(any(has_rev_nar)),
    tank_closure_known_leak_narrow = as.integer(any(has_kno_nar)),
    tank_closure_revealed_wide = as.integer(any(has_rev_wid)),
    tank_closure_known_leak_wide = as.integer(any(has_kno_wid)),
    # Added Regulatory Spec:
    tank_closure_revealed_reg = as.integer(any(has_rev_reg)),
    tank_closure_known_leak_reg = as.integer(any(has_kno_reg))
  ), by = .(panel_id, panel_year)]

  annual <- merge(annual, annual_flags, by = c("panel_id", "panel_year"), all.x=TRUE)
  cols_to_fill <- grep("tank_closure_", names(annual), value=TRUE)
  for(col in cols_to_fill) annual[is.na(get(col)), (col) := 0]
  annual[, tank_closure_clean := as.integer(closure_year == 1 & tank_closure_revealed == 0 & tank_closure_known_leak == 0)]
} else {
  annual[, tank_closure_revealed := 0]
}

#==============================================================================
# SECTION 8: WIDE ISSUER DATA
#==============================================================================
log_step("SECTION 8: Creating Wide Issuer Columns...", 0)
if ("ISSUER_NAME" %in% names(monthly)) {
  tx_issuers <- monthly[state == "Texas" & !is.na(ISSUER_NAME), .(panel_id, panel_year, ISSUER_NAME)]
  tx_issuers[, cleaned_issuer := str_sub(str_replace_all(toupper(ISSUER_NAME), "[^A-Z0-9]", ""), 1, 8)]
  issuer_counts <- tx_issuers[, .(n_months = .N), by = .(panel_id, panel_year, cleaned_issuer)]
  issuer_wide <- dcast(issuer_counts, panel_id + panel_year ~ cleaned_issuer, value.var = "n_months", fill = 0)
  setnames(issuer_wide, setdiff(names(issuer_wide), c("panel_id", "panel_year")), 
           paste0("TX_issuers_", setdiff(names(issuer_wide), c("panel_id", "panel_year"))))
  annual <- merge(annual, issuer_wide, by = c("panel_id", "panel_year"), all.x = TRUE)
  wide_cols <- grep("TX_issuers_", names(annual), value=TRUE)
  for(col in wide_cols) annual[is.na(get(col)), (col) := 0]
}

#==============================================================================
# SECTION 9: POLICY, TREATMENT & COVARAIATES
#==============================================================================
log_step("SECTION 9: Creating Policy & Cohort Variables...", 0)

treated_states <- c("Texas", "Wisconsin", "New Jersey", "Michigan", 
                    "Iowa", "Florida", "Arizona", "Connecticut")
treatment_years <- c(1999, 1998, 2010, 2014, 2000, 1999, 2006, 2010)

annual[, `:=`(
  post_1999 = as.integer(panel_year >= 1999),
  texas_treated = as.integer(state == "Texas"),
  first_observed = min(panel_year),
  treated_state_flag = as.integer(state %in% treated_states)
), by = panel_id]

annual[, `:=`(
  rel_year = fifelse(state == "Texas", panel_year - 1999, NA_integer_),
  treated = treated_state_flag,
  cohort = fifelse(first_observed < 2000, "Incumbent", "Entrant"),
  is_incumbent = as.integer(first_observed < 1999)
)]

annual[, treatment_year := treatment_years[match(state, treated_states)]]
annual[is.na(treatment_year), treatment_year := 0]

annual[, treatment_group := fcase(
  state == "Texas", "Texas",
  state %in% treated_states, "Other Treated",
  default = "Control"
)]

annual[, age_bins := cut(avg_tank_age_dec, 
                         breaks = c(seq(0, 35, 5), Inf),
                         include.lowest=TRUE, 
                         labels = c("0-5", "5-10", "10-15", "15-20", "20-25", "25-30", "30-35", "35+"))]

annual[, install_year := panel_year - floor(avg_tank_age_dec)]
annual[is.na(install_year), install_year := 1990]

annual[, `:=`(
  pre1998_install = as.integer(install_year < 1998),
  reg_vintage = fcase(install_year < 1988, "Pre-RCRA",
                      install_year >= 1988 & install_year <= 1998, "Transition",
                      install_year > 1998, "Post-Deadline",
                      default = "Unknown"),
  wall_type = fcase(has_double_walled_dec > 0 & has_single_walled_dec == 0, "Double-Walled",
                    has_single_walled_dec > 0 & has_double_walled_dec == 0, "Single-Walled",
                    has_double_walled_dec > 0 & has_single_walled_dec > 0, "Mixed",
                    default = "Unknown")
)]

#==============================================================================
# SECTION 10: SURVIVAL & DECOMPOSITION
#==============================================================================
log_step("SECTION 10: Creating Survival Variables...", 0)

setorder(annual, panel_id, panel_year)

# Exit Flag
annual[, last_year_id := max(panel_year), by = panel_id]
annual[, exit_flag := as.integer(panel_year == last_year_id & last_year_id < 2025)]
annual[, pulse_exit := exit_flag]

# Cumulative & Duration
annual[, `:=`(
  ever_leaked = as.integer(cumsum(leak_year) > 0),
  ever_closed = as.integer(cumsum(closure_year) > 0),
  ever_exited = as.integer(cumsum(exit_flag) > 0),
  cumulative_leaks = cumsum(n_leaks),
  cumulative_closures = cumsum(n_closures),
  cumulative_retrofits = cumsum(n_retrofits),
  has_previous_leak = as.integer(shift(cumsum(n_leaks), 1, fill=0) > 0),
  has_previous_closure = as.integer(shift(cumsum(n_closures), 1, fill=0) > 0),
  years_since_entry = panel_year - first_observed
), by = panel_id]

# First Event Timing
annual[, `:=`(
  event_first_leak = as.integer(leak_year == 1 & cumsum(leak_year) == 1),
  event_first_closure = as.integer(closure_year == 1 & cumsum(closure_year) == 1)
), by = panel_id]
annual[, year_of_first_leak := fifelse(any(leak_year > 0), panel_year[which(leak_year > 0)[1]], NA_integer_), by = panel_id]
annual[, years_since_first_leak := panel_year - year_of_first_leak]

# Decompositions
annual[, `:=`(
  exit_no_leak = as.integer(exit_flag == 1 & leak_year == 0),
  exit_with_leak = as.integer(exit_flag == 1 & leak_year == 1),
  retrofit_no_exit = as.integer(retrofit_year == 1 & exit_flag == 0),
  exit_no_retrofit = as.integer(exit_flag == 1 & retrofit_year == 0),
  both_exit_retrofit = as.integer(exit_flag == 1 & retrofit_year == 1)
)]

# Aliases
annual[, `:=`(
  lust_within_90d_of_closure = tank_closure_revealed,
  leak_found_by_closure = tank_closure_revealed,
  leak_not_found_by_exit = as.integer(exit_flag == 1 & leak_year == 0),
  active_tanks = active_tanks_dec,
  total_capacity = total_capacity_dec,
  avg_tank_age = avg_tank_age_dec,
  single_tanks = single_tanks_dec,
  double_tanks = double_tanks_dec,
  has_single_walled = has_single_walled_dec,
  has_double_walled = has_double_walled_dec
)]

#==============================================================================
# SECTION 11: SAVE OUTPUTS
#==============================================================================
log_step("SECTION 11: Saving Final Outputs...", 0)

output_csv <- here("Data", "Processed", "facility_leak_behavior_annual.csv")
fwrite(annual, output_csv)
log_step(sprintf("✓ Saved CSV: %s", output_csv), 1)

output_rds <- here("Data", "Processed", "facility_leak_behavior_annual.rds")
saveRDS(annual, output_rds)
log_step(sprintf("✓ Saved RDS: %s", output_rds), 1)

cat("\n========================================\n")
cat("PROCESSING COMPLETE\n")
cat(sprintf("Total Runtime: %.1f minutes\n", difftime(Sys.time(), script_start_time, units="mins")))
cat("========================================\n")

#==============================================================================
# DATA DICTIONARY GENERATION
#==============================================================================
log_step("Generating Data Dictionary...", 0)

data_dict <- data.table(
  variable_name = c(
    "panel_id", "facility_id", "state", "panel_year", "county_name", "county_geoid",
    "active_tanks_dec", "total_capacity_dec", "avg_tank_age_dec", "min_tank_age", "max_tank_age",
    "single_tanks_dec", "double_tanks_dec", "has_single_walled_dec", "has_double_walled_dec",
    "active_tanks_mean", "total_capacity_mean", "avg_tank_age_mean",
    "n_leaks", "leak_year", "n_closures", "closure_year", "n_installs", 
    "n_retrofits", "retrofit_year", "n_single_to_double", "single_to_double_year",
    "capacity_installed_year", "capacity_closed_year", "double_walled_installed_year",
    "capacity_change_year", "net_tank_change", "capacity_increased", "capacity_decreased",
    "tank_closure_revealed", "tank_closure_known_leak", "tank_closure_indeterminate", "tank_closure_clean",
    "tank_closure_revealed_narrow", "tank_closure_known_leak_narrow",
    "tank_closure_revealed_wide", "tank_closure_known_leak_wide",
    "tank_closure_revealed_reg", "tank_closure_known_leak_reg",
    "lust_within_90d_of_closure", "leak_found_by_closure", "leak_not_found_by_exit",
    "first_observed", "last_year_id", "years_since_entry", "exit_flag", "pulse_exit",
    "ever_leaked", "ever_closed", "ever_exited", "cumulative_leaks", "cumulative_closures", "cumulative_retrofits",
    "has_previous_leak", "has_previous_closure", "event_first_leak", "event_first_closure", "year_of_first_leak", "years_since_first_leak",
    "exit_no_leak", "exit_with_leak", "retrofit_no_exit", "exit_no_retrofit", "both_exit_retrofit",
    "fr_coverage_share", "share_private", "share_state_fund", "share_self",
    "n_gap_months", "any_gap", "n_transitions", "any_transition", "modal_detail_type",
    "had_zurich_2012", "dropped_by_zurich",
    "post_1999", "texas_treated", "treated_state_flag", "treated", "treatment_year", 
    "treatment_group", "rel_year",
    "cohort", "is_incumbent", "age_bins", "install_year", "pre1998_install", "reg_vintage", "wall_type",
    "has_gasoline_year", "has_diesel_year", "is_motor_fuel",
    "active_tanks", "total_capacity", "avg_tank_age", "single_tanks", "double_tanks", "has_single_walled", "has_double_walled"
  ),
  category = c(rep("Identifier", 6), rep("Stock (EOY)", 9), rep("Stock (Annual Mean)", 3),
    rep("Flow (Annual)", 12), rep("YoY Change", 4),
    rep("Leak Class (Primary)", 4), rep("Leak Class (Narrow)", 2), rep("Leak Class (Wide)", 2), rep("Leak Class (Regulatory)", 2),
    rep("Leak Class (Alias)", 3), rep("Duration", 5), rep("Cumulative", 3), rep("Cumulative", 3), rep("Lagged", 2), rep("Event Timing", 4),
    rep("Exit Type", 5), rep("FR Coverage", 9), rep("FR Treatment", 2),
    rep("Policy", 7), rep("Cohort", 2), rep("Vintage", 5), rep("Fuel Type", 3), rep("Stock (Alias)", 7)
  ),
  description = c(
    "Composite key", "Facility ID", "State", "Year", "County", "FIPS",
    "Active tanks (Dec)", "Capacity (Dec)", "Avg Age (Dec)", "Min Age", "Max Age",
    "Single tanks (Dec)", "Double tanks (Dec)", "Has single (Dec)", "Has double (Dec)",
    "Mean active tanks", "Mean capacity", "Mean age",
    "Leak count", "Leak dummy", "Closure count", "Closure dummy", "Install count",
    "Retrofit count", "Retrofit dummy", "Upgrade count", "Upgrade dummy",
    "Installed capacity", "Closed capacity", "DW installed count",
    "Capacity change", "Net tank change", "Cap Increase dummy", "Cap Decrease dummy",
    "Revealed (0-60d)", "Known (>180d)", "Indeterminate", "Clean closure",
    "Revealed (0-30d)", "Known (>365d)", "Revealed (0-90d)", "Known (>90d)",
    "Revealed (0-45d)", "Known (>180d - Reg)",
    "Alias Wide", "Alias Primary", "Exit clean",
    "First year", "Last year", "Tenure", "Exit dummy", "Exit pulse",
    "Ever leaked", "Ever closed", "Ever exited", "Cum leaks", "Cum closures", "Cum retrofits",
    "Prev leak", "Prev closure", "First leak event", "First closure event", "First leak year", "Yrs since first leak",
    "Clean exit", "Dirty exit", "Retrofit no exit", "Exit no retrofit", "Exit+Retrofit",
    "FR share", "Private share", "State fund share", "Self share",
    "Gap months", "Any gap", "Transitions", "Any transition", "Modal type",
    "Zurich 2012 baseline", "Dropped by Zurich (Treatment)",
    "Post-1999", "Texas dummy", "Treated state", "Treated dummy", "Reform year",
    "Treatment group", "Relative year",
    "Cohort", "Incumbent dummy", "Age bin", "Install year", "Pre-1998", "Vintage", "Wall type",
    "Gasoline", "Diesel", "Motor fuel",
    "Alias Active", "Alias Cap", "Alias Age", "Alias Single", "Alias Double", "Alias Has Single", "Alias Has Double"
  ),
  type = c(rep("character", 6), rep("numeric", 5), rep("integer", 4), rep("numeric", 3),
    rep("integer", 12), rep("numeric", 2), rep("integer", 2),
    rep("integer", 13), rep("integer", 17), rep("integer", 5),
    rep("numeric", 4), rep("integer", 4), "character", "integer", "integer",
    rep("integer", 5), "character", "integer", "character", "integer", "factor", "integer", "integer", "character", "character",
    rep("integer", 3), rep("numeric", 7)
  ),
  notes = c(rep("ID", 6), rep("Stock", 12), rep("Flow", 12), rep("Change", 4),
    rep("Leak", 13), rep("Survival", 17), rep("Exit", 5),
    rep("FR", 11), rep("Policy", 7), rep("Vintage", 7), rep("Fuel", 3), rep("Alias", 7)
  )
)

fwrite(data_dict, here("Data", "Processed", "facility_leak_behavior_annual_data_dictionary.csv"))
cat("✓ Data dictionary saved\n")