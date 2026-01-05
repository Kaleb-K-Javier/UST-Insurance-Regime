


#==============================================================================
# UNIFIED FACILITY-MONTH PANEL CONSTRUCTION (Corrected Version)
# 
# Purpose: Create a comprehensive facility-month panel combining EPA and Texas
#          tank data with leak incidents, replacements, and policy variables
#
# KEY IMPROVEMENTS IN THIS VERSION:
# 1. Proper date handling: Separates tank installation dates from panel dates
#    - Preserves original install dates in *_raw columns for age calculations
#    - Creates panel spanning 1970-2025 without overwriting historical dates
# 2. Memory efficiency: State-by-state processing prevents memory overflow
# 3. Data validation: Handles corrupted dates while preserving valid historical data
#
# Global Inputs:
#   - EPA tank panel (facility-tank level)
#   - Texas tank-months (facility-tank-month level) 
#   - EPA LUST data (facility-incident level)
#   - Texas LUST data (facility-incident level)
#   - Texas FR data (facility-month level)
#   - Texas facility raw data (facility level)
#
# Global Output:
#   - facility_leak_behavior_monthly.csv (facility-month level panel)
#
# Author: [Corrected version with proper date handling]
# Date: 2025
#==============================================================================
# Track total script runtime
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
tx_tank_months <- fread(get_data_path("Outputs", "texas_ust_facility_month_panel.csv"))
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
  here("Data", "Raw_do_not_write", "state_databases", 
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
TX_LUST_SD <- fread(get_data_path("TX_LUST.csv"))

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
  get_data_path("Outputs", "raw_pst_fac.csv"))
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


# Load optional FR data sources
# These are used in Section 10 for merging TX Financial Responsibility (insurance) data
log_step("Loading Texas FR data (optional)...", 1)
fa_monthly <- readRDS(get_data_path("Outputs", "panel_merge_staging", "fa_monthly.rds"))
setDT(fa_monthly)

fa_monthly_raw_cols <- c(
  "FACILITY_ID",
  "YEAR",
  "MONTH",
  "DETAIL_TYPE",
  "CATEGORY",
  "ISSUER_NAME"
)
fa_monthly <- fa_monthly[, .SD, .SDcols = fa_monthly_raw_cols]
log_step(sprintf("Loaded: %s rows, %d columns", 
                format(nrow(fa_monthly), big.mark=","),
                ncol(fa_monthly)), 2)


log_step("Loading Zurich 2012 lookup (optional)...", 1)
zurich_2012_lookup <- readRDS(get_data_path("Outputs", "panel_merge_staging", "zurich_2012_lookup.rds"))
setDT(zurich_2012_lookup)

zurich_2012_raw_cols <- c(
  "FACILITY_ID",
  "had_zurich_2012"
)
zurich_2012_lookup <- zurich_2012_lookup[, .SD, .SDcols = zurich_2012_raw_cols]
log_step(sprintf("Loaded: %s rows, %d columns", 
                format(nrow(zurich_2012_lookup), big.mark=","),
                ncol(zurich_2012_lookup)), 2)


log_step(sprintf("✓ All data loaded in %.1f seconds", 
                difftime(Sys.time(), load_start, units="secs")), 1)

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
# 1.6.2 CREATE TANK_PANEL_END_DATE
#------------------------------------------------------------------------------
log_step("Creating tank_panel_end_date...", 1)
# The panel end date is the *earlier* of the panel boundary (2025) or the
# actual close date. If the close date is NA, it defaults to the PANEL_END.
# We also floor any pre-1970 closures at the PANEL_START date.

# Texas tank panel
tx_tank_months[, tank_panel_end_date := fcase(
  is.na(tank_closed_date_raw), PANEL_END,
  as.Date(tank_closed_date_raw) < PANEL_START, PANEL_START,
  as.Date(tank_closed_date_raw) > PANEL_END, PANEL_END,
  default = as.Date(tank_closed_date_raw)
)]

# EPA tank panel
tank_panel_epa[, tank_panel_end_date := fcase(
  is.na(tank_closed_date_raw), PANEL_END,
  as.Date(tank_closed_date_raw) < PANEL_START, PANEL_START,
  as.Date(tank_closed_date_raw) > PANEL_END, PANEL_END,
  default = as.Date(tank_closed_date_raw)
)]

# Validate
n_invalid_end_tx <- tx_tank_months[tank_panel_end_date > PANEL_END, .N]
n_invalid_end_epa <- tank_panel_epa[tank_panel_end_date > PANEL_END, .N]
if (n_invalid_end_tx == 0 & n_invalid_end_epa == 0) {
  log_step("✓ All tank_panel_end_date values <= 2025-12-31", 2)
} else {
  log_step(sprintf("⚠ WARNING: %d TX and %d EPA tanks have panel_end > 2025", 
                  n_invalid_end_tx, n_invalid_end_epa), 2)
}

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
# 1.9 IDENTIFY AND REMOVE FACILITIES WITH INVALID DATES (Was 1.8)
#==============================================================================
log_step("\n1.9 Identifying facilities to exclude due to date issues...", 0)
exclude_start <- Sys.time()

# This is a critical cleaning step. We remove any *facility* that has
# even one *tank* with a bad installation date. This is an aggressive but
# clean approach, ensuring no facilities with imputed/sentinel ages
# contaminate the final analysis.

#------------------------------------------------------------------------------
# 1.9.1: IDENTIFY PROBLEM TANKS (TANK-LEVEL INVENTORY)
#------------------------------------------------------------------------------
log_step("Identifying problem tanks with bad install dates...", 1)

# Texas problem tanks - get full tank inventory
bad_tanks_tx <- tx_tank_months[
  bad_install_date == 1,  # Only tanks with bad install dates
  .(
    facility_id,
    state,
    tank_id,
    panel_id,
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
  bad_install_date == 1,
  .(
    facility_id,
    state,
    tank_id,
    panel_id,
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

# Remove duplicates
bad_tanks_all <- unique(bad_tanks_all, by = c("panel_id", "tank_id"))

if (nrow(bad_tanks_all) > 0) {
  # Save tank-level inventory for review
  output_path_tanks <- get_data_path("Outputs", "excluded_tanks_bad_dates.csv")
  fwrite(bad_tanks_all, output_path_tanks)
  
  log_step(sprintf("⚠ Identified %s tanks with invalid install dates", 
                  format(nrow(bad_tanks_all), big.mark=",")), 2)
  log_step(sprintf("  Saved tank inventory to: %s", output_path_tanks), 2)
  
  #---------------------------------------------------------------------------
  # 1.9.2: IDENTIFY AFFECTED FACILITIES
  #---------------------------------------------------------------------------
  log_step("Identifying affected facilities...", 1)
  
  bad_facilities <- unique(bad_tanks_all[, .(panel_id, facility_id, state)])
  n_bad_facilities <- nrow(bad_facilities)
  
  log_step(sprintf("  Facilities to remove: %s", 
                  format(n_bad_facilities, big.mark=",")), 2)
  
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
  
  #---------------------------------------------------------------------------
  # 1.9.3: DISPLAY STATE-BY-STATE REMOVAL STATISTICS
  #---------------------------------------------------------------------------
  log_step("\n  Facility removal by state:", 2)
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
  
  # Save facility-level summary
  output_path_facs <- get_data_path("Outputs", "excluded_facilities_bad_dates.csv")
  fwrite(bad_facilities, output_path_facs)
  log_step(sprintf("  Saved facility list to: %s", output_path_facs), 2)
  
  #---------------------------------------------------------------------------
  # 1.9.4: REMOVE BAD FACILITIES FROM DATASETS
  #---------------------------------------------------------------------------
  log_step("\nRemoving facilities from datasets...", 1)
  
  # Create removal keys (facility-level, not tank-level)
  # We can now use the panel_id we created in 1.4.1
  bad_panel_ids <- bad_facilities$panel_id
  
  tx_before <- uniqueN(tx_tank_months$panel_id)
  tx_rows_before <- nrow(tx_tank_months)
  epa_before <- uniqueN(tank_panel_epa$panel_id)
  epa_rows_before <- nrow(tank_panel_epa)
  
  # Remove from Texas data (all rows for bad facilities)
  # This is now much faster because panel_id is a key
  tx_tank_months <- tx_tank_months[!panel_id %in% bad_panel_ids]
  
  # Remove from EPA data (all rows for bad facilities)
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
  log_step("✓ No facilities require exclusion due to bad install dates", 1)
}

log_step(sprintf("\n✓ Date validation and facility removal completed in %.1f seconds", 
                difftime(Sys.time(), exclude_start, units="secs")), 1)


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
# (EPA data may have multiple rows per tank if it comes from state submissions)
#------------------------------------------------------------------------------
log_step("Collapsing EPA data to unique tank level...", 1)
epa_tank_level <- tank_panel_epa[
  state != "Texas", # Remove Texas data from EPA file
  .(
    # Keep the earliest/latest lifecycle dates (raw)
    tank_installed_date_raw = suppressWarnings(min(tank_installed_date_raw, na.rm = TRUE)),
    tank_closed_date_raw = suppressWarnings(max(tank_closed_date_raw, na.rm = TRUE)),
    
    # Keep the earliest/latest panel dates (already bounded in Section 1)
    tank_panel_start_date = suppressWarnings(min(tank_panel_start_date, na.rm = TRUE)),
    tank_panel_end_date = suppressWarnings(max(tank_panel_end_date, na.rm = TRUE)),
    
    # Tank characteristics (take max/any)
    capacity = suppressWarnings(max(capacity, na.rm = TRUE)),
    single_walled = as.integer(any(single_walled == 1, na.rm = TRUE)),
    double_walled = as.integer(any(double_walled == 1, na.rm = TRUE)),
    is_gasoline = as.integer(any(is_gasoline == 1, na.rm = TRUE)),
    is_diesel = as.integer(any(is_diesel == 1, na.rm = TRUE)),
    is_oil_kerosene = as.integer(any(is_oil_kerosene == 1, na.rm = TRUE)),
    is_jet_fuel = as.integer(any(is_jet_fuel == 1, na.rm = TRUE)),
    is_other = as.integer(any(is_other == 1, na.rm = TRUE)),
    
    # Geographic info (take first non-NA)
    county_name = na.omit(county_name)[1],
    county_geoid = as.character(na.omit(county_geoid)[1]),
    panel_id = panel_id[1] # panel_id is constant within group
), by = .(facility_id, tank_id, state)] # This ensures uniqueness

# Clean infinite values and ensure Date class
for (v in c("tank_installed_date_raw", "tank_closed_date_raw",
            "tank_panel_start_date", "tank_panel_end_date")) {
  epa_tank_level[is.infinite(get(v)), (v) := NA_Date_]
  epa_tank_level[[v]] <- as.Date(epa_tank_level[[v]])
}
epa_tank_level[is.infinite(capacity), capacity := NA_real_]

# Fill any remaining NAs with panel bounds
epa_tank_level[is.na(tank_panel_start_date), tank_panel_start_date := as.Date("1970-01-01")]
epa_tank_level[is.na(tank_panel_end_date), tank_panel_end_date := as.Date("2025-12-31")]

# Ensure schema consistency (add missing columns as NA)
missing_cols_epa <- setdiff(needed_cols, names(epa_tank_level))
if (length(missing_cols_epa) > 0) {
  epa_tank_level[, (missing_cols_epa) := NA]
}
epa_tank_level <- epa_tank_level[, ..needed_cols] # Reorder columns

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
  # Keep earliest/latest lifecycle dates (raw)
  tank_installed_date_raw = suppressWarnings(min(tank_installed_date_raw, na.rm = TRUE)),
  tank_closed_date_raw = suppressWarnings(max(tank_closed_date_raw, na.rm = TRUE)),
  
  # Keep earliest/latest panel dates (already bounded in Section 1)
  tank_panel_start_date = suppressWarnings(min(tank_panel_start_date, na.rm = TRUE)),
  tank_panel_end_date = suppressWarnings(max(tank_panel_end_date, na.rm = TRUE)),
  
  # Tank characteristics (take max/any from any month)
  capacity = suppressWarnings(max(capacity, na.rm = TRUE)),
  single_walled = as.integer(any(single_walled == 1, na.rm = TRUE)),
  double_walled = as.integer(any(double_walled == 1, na.rm = TRUE)),
  is_gasoline = as.integer(any(is_gasoline == 1, na.rm = TRUE)),
  is_diesel = as.integer(any(is_diesel == 1, na.rm = TRUE)),
  is_oil_kerosene = as.integer(any(is_oil_kerosene == 1, na.rm = TRUE)),
  is_jet_fuel = as.integer(any(is_jet_fuel == 1, na.rm = TRUE)),
  is_other = as.integer(any(is_other == 1, na.rm = TRUE)),
  panel_id = panel_id[1] # panel_id is constant
), by = .(facility_id, tank_id, state)] # This ensures uniqueness

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
#
# Operations:
#   - Find active date range for each facility
#   - Generate monthly sequence for 1970-2025 panel
#   - Create panel structure with all facility-months
#==============================================================================

#==============================================================================
# SECTION 3: CREATE FACILITY-MONTH BACKBONE (OPTIMIZED)
#==============================================================================
cat("SECTION 3: Building facility-month backbone...\n")
start_time <- Sys.time()

# Calculate facility active periods (unchanged)
facility_active <- tank_panel_unified[, .(
  start_date_raw = suppressWarnings(min(tank_installed_date_raw, na.rm = TRUE)),    
  end_date_raw = suppressWarnings(max(tank_closed_date_raw, na.rm = TRUE)),         
  start_date = suppressWarnings(min(tank_panel_start_date, na.rm = TRUE)),
  end_date = suppressWarnings(max(tank_panel_end_date, na.rm = TRUE)),
  county_name = na.omit(county_name)[1],
  county_geoid = as.character(na.omit(county_geoid)[1])
), by = .(facility_id, state)]

# Handle infinite values (unchanged)
facility_active[is.infinite(start_date_raw), start_date_raw := NA]
facility_active[is.infinite(end_date_raw), end_date_raw := NA]
facility_active[is.infinite(start_date), start_date := as.Date("1970-01-01")]
facility_active[is.infinite(end_date), end_date := as.Date("2025-12-31")]

facility_active[is.na(start_date) | start_date < as.Date("1970-01-01"), 
                start_date := as.Date("1970-01-01")]
facility_active[is.na(end_date) | end_date > as.Date("2025-12-31"), 
                end_date := as.Date("2025-12-31")]

n_invalid_ranges <- facility_active[end_date < start_date, .N]
if (n_invalid_ranges > 0) {
  cat(sprintf("  ⚠ WARNING: %d facilities have end_date < start_date (auto-corrected)\n", 
              n_invalid_ranges))
  facility_active[end_date < start_date, end_date := start_date]
}

n_facilities_pre_1970 <- sum(!is.na(facility_active$start_date_raw) & 
                             facility_active$start_date_raw < as.Date("1970-01-01"), na.rm=TRUE)
if (n_facilities_pre_1970 > 0) {
  cat(sprintf("  Note: %s facilities have tanks installed before 1970\n", 
              format(n_facilities_pre_1970, big.mark=",")))
}

cat(sprintf("  Facility date ranges computed: %s facilities\n", nrow(facility_active)))

#------------------------------------------------------------------------------
# OPTIMIZED: Pre-calculate month counts to avoid repeated seq() calls
#------------------------------------------------------------------------------
facility_active[, `:=`(
  start_floor = floor_date(start_date, "month"),
  end_floor = floor_date(end_date, "month")
)]

facility_active[, n_months := as.integer(
  (year(end_floor) - year(start_floor)) * 12 + 
  (month(end_floor) - month(start_floor)) + 1
)]

# Calculate total rows needed
total_rows <- sum(facility_active$n_months)
cat(sprintf("  Allocating space for %s facility-months...\n", 
            format(total_rows, big.mark=",")))

# Pre-allocate the full data.table
panel_dates <- data.table(
  facility_id = rep(facility_active$facility_id, facility_active$n_months),
  state = rep(facility_active$state, facility_active$n_months),
  county_name = rep(facility_active$county_name, facility_active$n_months),
  county_geoid = rep(facility_active$county_geoid, facility_active$n_months),
  date = as.Date(NA)
)

# Fill dates efficiently using vectorized operations
start_idx <- 1
for (i in seq_len(nrow(facility_active))) {
  n <- facility_active$n_months[i]
  end_idx <- start_idx + n - 1
  
  panel_dates[start_idx:end_idx, date := seq(
    facility_active$start_floor[i],
    by = "month",
    length.out = n
  )]
  
  start_idx <- end_idx + 1
  
  if (i %% 50000 == 0) {
    cat(sprintf("  Processed %d / %d facilities (%.1f%%)\n", 
                i, nrow(facility_active), 100*i/nrow(facility_active)))
  }
}

# Add time variables
panel_dates[, `:=`(
  panel_year = year(date),
  panel_month = month(date),
  month_key = sprintf("%d-%02d", year(date), month(date))
)]

setkey(panel_dates, facility_id, state, date)

# Validation (unchanged)
cat("\n  Section 3 Validation:\n")
cat(sprintf("    Total facility-months: %s\n", format(nrow(panel_dates), big.mark=",")))
cat(sprintf("    Unique facilities: %s\n", uniqueN(panel_dates$facility_id)))

min_date <- min(panel_dates$date, na.rm=TRUE)
max_date <- max(panel_dates$date, na.rm=TRUE)
cat(sprintf("    Date range: %s to %s\n", min_date, max_date))

if (year(max_date) > 2025 || year(min_date) < 1970) {
  cat("    WARNING: Date range extends beyond 1970-2025 bounds!\n")
}

cat(sprintf("    Avg months per facility: %.1f\n", nrow(panel_dates) / uniqueN(panel_dates$facility_id)))
log_memory("After backbone creation")
cat(sprintf("  Time elapsed: %.1f seconds\n\n", difftime(Sys.time(), start_time, units="secs")))
#==============================================================================
# SECTION 4: COMPUTE MONTHLY TANK COMPOSITION (OPTIMIZED)
#
# Operations:
#   - For each facility-month, identify active tanks
#   - Calculate tank ages using ORIGINAL installation dates
#   - Process in chunks by state to avoid memory overflow
#==============================================================================

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

panel <- tank_composition[panel_dates, on = .(facility_id, state, date)]

# Fill missing values for facility-months with no active tanks
integer_fills <- c("active_tanks", "single_tanks", "double_tanks", 
                   "has_gasoline", "has_diesel", "has_oil_kerosene", 
                   "has_jet_fuel", "has_other")

numeric_fills <- c("avg_tank_age", "min_tank_age", "max_tank_age", 
                   "total_capacity", "avg_capacity")

for (col in integer_fills) {
  if (col %in% names(panel)) {
    panel[is.na(get(col)), (col) := 0L]
  }
}

for (col in numeric_fills) {
  if (col %in% names(panel)) {
    panel[is.na(get(col)), (col) := 0]
    # Clean infinites
    panel[is.infinite(get(col)), (col) := NA_real_]
  }
}

#------------------------------------------------------------------------------
# 4.5: CREATE DERIVED COMPOSITION VARIABLES
#------------------------------------------------------------------------------
cat("  Creating derived composition variables...\n")

panel[, `:=`(
  # Wall type shares (only calculate when tanks exist)
  single_share = fifelse(active_tanks > 0, single_tanks / active_tanks, 0),
  double_share = fifelse(active_tanks > 0, double_tanks / active_tanks, 0),
  
  # Capacity per tank (when tanks exist)
  capacity_per_tank = fifelse(active_tanks > 0, total_capacity / active_tanks, 0),
  
  # Facility has tanks flag
  has_tanks = as.integer(active_tanks > 0)
)]

#------------------------------------------------------------------------------
# 4.6: VALIDATION
#------------------------------------------------------------------------------
cat("\n  Section 4 Validation:\n")
cat(sprintf("    Total facility-months: %s\n", 
            format(nrow(panel), big.mark=",")))
cat(sprintf("    Facility-months with tanks: %s (%.1f%%)\n", 
            format(sum(panel$active_tanks > 0), big.mark=","),
            100 * mean(panel$active_tanks > 0)))
cat(sprintf("    Mean active tanks (when > 0): %.2f\n", 
            mean(panel[active_tanks > 0]$active_tanks)))
cat(sprintf("    Mean tank age (when tanks exist): %.1f years\n", 
            mean(panel[active_tanks > 0]$avg_tank_age, na.rm = TRUE)))
cat(sprintf("    Mean capacity per facility: %s gallons\n", 
            format(round(mean(panel[active_tanks > 0]$total_capacity, na.rm = TRUE)), 
                   big.mark=",")))

# Age distribution
age_summary <- panel[active_tanks > 0 & !is.na(avg_tank_age), 
                     .(min_age = min(avg_tank_age),
                       q25 = quantile(avg_tank_age, 0.25),
                       median = median(avg_tank_age),
                       q75 = quantile(avg_tank_age, 0.75),
                       max_age = max(avg_tank_age))]

cat(sprintf("    Age distribution: min=%.1f, Q1=%.1f, median=%.1f, Q3=%.1f, max=%.1f\n",
            age_summary$min_age, age_summary$q25, age_summary$median, 
            age_summary$q75, age_summary$max_age))

# Wall type distribution
wall_summary <- panel[active_tanks > 0, .(
  pct_single_only = 100 * mean(single_tanks > 0 & double_tanks == 0),
  pct_double_only = 100 * mean(double_tanks > 0 & single_tanks == 0),
  pct_mixed = 100 * mean(single_tanks > 0 & double_tanks > 0)
)]

cat(sprintf("    Wall types: %.1f%% single-only, %.1f%% double-only, %.1f%% mixed\n",
            wall_summary$pct_single_only, wall_summary$pct_double_only, 
            wall_summary$pct_mixed))

# Check for data quality issues
n_negative_age <- panel[avg_tank_age < 0, .N]
n_very_old <- panel[avg_tank_age > 80, .N]
n_huge_capacity <- panel[total_capacity > 500000, .N]

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
# SECTION 5: MOTOR FUEL CLASSIFICATION
#==============================================================================
cat("SECTION 5: Adding motor fuel classification...\n")
start_time <- Sys.time()

mf_fac <- tank_panel_unified[, .(
  ever_gasoline = any(is_gasoline == 1, na.rm = TRUE),
  ever_diesel = any(is_diesel == 1, na.rm = TRUE),
  ever_oil_kerosene = any(is_oil_kerosene == 1, na.rm = TRUE),
  ever_jet_fuel = any(is_jet_fuel == 1, na.rm = TRUE),
  ever_other = any(is_other == 1, na.rm = TRUE)
), by = .(facility_id, state)]

mf_fac[, is_motor_fuel := (ever_gasoline | ever_diesel) &
         !ever_oil_kerosene & !ever_jet_fuel & !ever_other]

panel <- mf_fac[, .(facility_id, state, is_motor_fuel)][panel, on = .(facility_id, state)]

cat("\n  Section 5 Validation:\n")
cat(sprintf("    Motor fuel facilities: %s (%.1f%%)\n", 
            uniqueN(panel[is_motor_fuel == TRUE]$facility_id),
            100 * uniqueN(panel[is_motor_fuel == TRUE]$facility_id) / uniqueN(panel$facility_id)))
log_memory("After motor fuel classification")
cat(sprintf("  Time elapsed: %.1f seconds\n\n", difftime(Sys.time(), start_time, units="secs")))

#==============================================================================
# SECTION 6: TANK INSTALLATION AND CLOSURE EVENTS (CORRECTED)
#==============================================================================
cat("SECTION 6: Building installation/closure events...\n")
start_time <- Sys.time()

tank_panel_unified[, `:=`(
  tank_panel_start_date = as.Date(tank_panel_start_date),
  tank_panel_end_date = as.Date(tank_panel_end_date)
)]

#------------------------------------------------------------------------------
# 6.1: IDENTIFY INITIAL STOCK (CORRECTED - Count ALL tanks active at panel start)
#------------------------------------------------------------------------------
cat("  Identifying initial stock (pre-panel tanks)...\n")

# CRITICAL FIX: Initial stock should include:
# 1. Tanks with install_date < 1970 (historical tanks)
# 2. Tanks with panel_start_date = 1970-01-01 (tanks active at panel beginning)
# 3. ANY tank where panel_start_date is the first date in our panel

initial_stock <- tank_panel_unified[
  tank_panel_start_date == as.Date("1970-01-01"), .(
    initial_tanks = .N,
    initial_capacity = sum(capacity, na.rm = TRUE),
    initial_single_walled = sum(single_walled, na.rm = TRUE),
    initial_double_walled = sum(double_walled, na.rm = TRUE),
    initial_gasoline = sum(is_gasoline, na.rm = TRUE),
    initial_diesel = sum(is_diesel, na.rm = TRUE)
  ), by = .(facility_id, state)]

cat(sprintf("    Facilities with initial stock: %s\n", 
            format(nrow(initial_stock), big.mark=",")))
cat(sprintf("    Total initial stock tanks: %s\n", 
            format(sum(initial_stock$initial_tanks), big.mark=",")))

# Verify initial stock matches FIRST OBSERVED MONTH for each facility
first_month_check <- panel[, .SD[1], by = .(facility_id, state)]
first_month_check <- first_month_check[, .(facility_id, state, active_tanks)]

setkey(first_month_check, facility_id, state)
setkey(initial_stock, facility_id, state)

validation <- initial_stock[first_month_check, on = .(facility_id, state)]
validation[, discrepancy := initial_tanks - active_tanks]

n_mismatches <- validation[abs(discrepancy) > 0, .N]
if (n_mismatches > 0) {
  cat(sprintf("    ⚠ WARNING: %d facilities have initial_tanks != first month active_tanks\n", 
              n_mismatches))
  cat("    (Detailed analysis will occur after Section 6.4)\n")
} else {
  cat("    ✓ Initial stock matches first-month active tanks for all facilities\n")
}

#------------------------------------------------------------------------------
# 6.2: TRACK INSTALLATIONS (Only tanks installed WITHIN panel period)
#------------------------------------------------------------------------------
cat("  Aggregating installations (1970-2025 only)...\n")

inst <- tank_panel_unified[
  !is.na(tank_installed_date_raw) & 
  tank_installed_date_raw >= as.Date("1970-01-01") & 
  tank_installed_date_raw <= as.Date("2025-12-31"), .(
    tanks_installed = .N,
    capacity_installed = sum(capacity, na.rm = TRUE),
    single_walled_installed = sum(single_walled, na.rm = TRUE),
    double_walled_installed = sum(double_walled, na.rm = TRUE)
  ), by = .(facility_id, state,
           panel_year = year(tank_installed_date_raw),
           panel_month = month(tank_installed_date_raw))]

cat(sprintf("    Installation events: %s\n", 
            format(sum(inst$tanks_installed), big.mark=",")))

#------------------------------------------------------------------------------
# 6.3: TRACK CLOSURES
#------------------------------------------------------------------------------
cat("  Aggregating closures...\n")

clos <- tank_panel_unified[
  !is.na(tank_panel_end_date) & 
  tank_panel_end_date <= as.Date("2025-12-31"), .(
    tanks_closed = .N,
    capacity_closed = sum(capacity, na.rm = TRUE),
    single_walled_closed = sum(single_walled, na.rm = TRUE),
    double_walled_closed = sum(double_walled, na.rm = TRUE)
  ), by = .(facility_id, state,
           panel_year = year(tank_panel_end_date),
           panel_month = month(tank_panel_end_date))]

cat(sprintf("    Closure events: %s\n", 
            format(sum(clos$tanks_closed), big.mark=",")))

#------------------------------------------------------------------------------
# 6.4: MERGE EVENTS TO PANEL
#------------------------------------------------------------------------------
cat("  Merging events to panel...\n")

# Combine installation and closure events
setkey(inst, facility_id, state, panel_year, panel_month)
setkey(clos, facility_id, state, panel_year, panel_month)
events_combined <- merge(inst, clos, 
                         by = c("facility_id", "state", "panel_year", "panel_month"),
                         all = TRUE)

event_cols <- c("tanks_installed","capacity_installed","single_walled_installed","double_walled_installed",
                "tanks_closed","capacity_closed","single_walled_closed","double_walled_closed")
setnafill(events_combined, fill = 0L, cols = event_cols)

# Merge to panel
setkey(panel, facility_id, state, panel_year, panel_month)
panel <- events_combined[panel, on = .(facility_id, state, panel_year, panel_month)]

# Safer version:
if (nrow(panel) == 0) {
  cat("  WARNING: Panel is empty, creating from panel_dates...\n")
  panel <- copy(panel_dates)
}

for (v in event_cols) {
  if (!v %in% names(panel)) panel[, (v) := 0L]
  panel[is.na(get(v)), (v) := 0L]
}

# Merge initial stock
setkey(initial_stock, facility_id, state)
setkey(panel, facility_id, state)
panel <- initial_stock[panel, on = .(facility_id, state)]

# Fill NAs for facilities with no initial stock
initial_stock_cols <- c("initial_tanks","initial_capacity","initial_single_walled",
                        "initial_double_walled","initial_gasoline","initial_diesel")
for (v in initial_stock_cols) {
  if (!v %in% names(panel)) panel[, (v) := 0L]
  panel[is.na(get(v)), (v) := 0L]
}

#------------------------------------------------------------------------------
# 6.4A: POST-MERGE VALIDATION OF INITIAL STOCK
#------------------------------------------------------------------------------
cat("  Validating initial stock after event merge...\n")

if (exists("validation") && n_mismatches > 0) {
  problem_facilities <- validation[abs(discrepancy) > 0, .(facility_id, state, discrepancy)]
  setkey(problem_facilities, facility_id, state)
  
  first_month_events <- panel[problem_facilities, on = .(facility_id, state)][
    , .SD[1], by = .(facility_id, state)
  ][, .(facility_id, state, date, active_tanks, 
        tanks_closed, tanks_installed, 
        initial_tanks = discrepancy + active_tanks)]
  
  # Check: How many mismatches are explained by first-month closures?
  first_month_events[, explained_by_closure := (initial_tanks - active_tanks == tanks_closed)]
  
  cat(sprintf("    Mismatches explained by first-month closures: %d (%.1f%%)\n",
              sum(first_month_events$explained_by_closure, na.rm=TRUE),
              100 * mean(first_month_events$explained_by_closure, na.rm=TRUE)))
  
  # Show unexplained cases
  unexplained <- first_month_events[!explained_by_closure]
  if (nrow(unexplained) > 0) {
    cat(sprintf("    ⚠ Unexplained mismatches: %d facilities\n", nrow(unexplained)))
    cat("    Sample (first 5):\n")
    print(head(unexplained, 5))
  }
}

#------------------------------------------------------------------------------
# 6.5: CALCULATE CUMULATIVE STOCK (Initial + Net Changes)
#------------------------------------------------------------------------------
cat("  Calculating cumulative stock over time...\n")

setorder(panel, facility_id, state, date)

panel[, `:=`(
  # Net change each month
  net_tank_change = tanks_installed - tanks_closed,
  
  # Cumulative stock = initial stock + cumulative net changes
  cumulative_tanks = initial_tanks + cumsum(tanks_installed - tanks_closed),
  cumulative_capacity = initial_capacity + cumsum(capacity_installed - capacity_closed)
), by = .(facility_id, state)]

#------------------------------------------------------------------------------
# 6.6: REPLACEMENT INDICATORS (unchanged)
#------------------------------------------------------------------------------
panel[, `:=`(
  replacement_event = as.integer(tanks_closed > 0 & tanks_installed > 0),
  single_to_double_replacement = as.integer(single_walled_closed > 0 & double_walled_installed > 0),
  double_wall_replacement = as.integer(double_walled_installed > 0 & tanks_closed > 0),
  capacity_change = capacity_installed - capacity_closed,
  capacity_increased = as.integer(tanks_closed > 0 & tanks_installed > 0 & 
                                  (capacity_installed - capacity_closed) > 0),
  capacity_decreased = as.integer(tanks_closed > 0 & tanks_installed > 0 & 
                                  (capacity_installed - capacity_closed) < 0)
)]
#------------------------------------------------------------------------------
# 6.7: VALIDATION
#------------------------------------------------------------------------------
cat("\n  Section 6 Validation:\n")
cat(sprintf("    Facilities with pre-1970 tanks: %s\n", 
            format(nrow(initial_stock), big.mark=",")))  # ← COUNT FROM initial_stock
cat(sprintf("    Total initial stock (pre-1970): %s tanks\n", 
            format(sum(initial_stock$initial_tanks), big.mark=",")))
cat(sprintf("    Installation events (1970-2025): %s\n", 
            format(sum(panel$tanks_installed), big.mark=",")))
cat(sprintf("    Closure events: %s\n", 
            format(sum(panel$tanks_closed), big.mark=",")))
cat(sprintf("    Replacement events: %s\n", 
            format(sum(panel$replacement_event), big.mark=",")))
cat(sprintf("    Single-to-double upgrades: %s\n", 
            format(sum(panel$single_to_double_replacement), big.mark=",")))

# Cross-check: cumulative_tanks vs active_tanks
panel[, stock_discrepancy := cumulative_tanks - active_tanks]

n_discrepancies <- panel[abs(stock_discrepancy) > 1, .N]
pct_discrepancies <- 100 * n_discrepancies / nrow(panel)

if (n_discrepancies > 0) {
  cat(sprintf("    ℹ Stock calculation method differences: %s facility-months (%.2f%%)\n", 
              format(n_discrepancies, big.mark=","), pct_discrepancies))
  cat(sprintf("      - These reflect boundary effects and exact-date vs month-level differences\n"))
  cat(sprintf("      - Using Section 4 direct count (active_tanks) as ground truth\n"))
} else {
  cat("    ✓ Cumulative and direct counts match perfectly\n")
}

# Create reconciliation flag for diagnostics
panel[, stock_reconciliation_flag := as.integer(abs(stock_discrepancy) > 1)]
panel[, stock_for_analysis := active_tanks]  # Use Section 4's direct count
#==============================================================================
# SECTION 7: LUST INTEGRATION
#==============================================================================
cat("SECTION 7: Merging LUST data...\n")
start_time <- Sys.time()

# Aggregate EPA LUST by month
lust_epa_m <- lust_epa[!is.na(report_date), {
  report_dt <- as.Date(report_date)
  .(
    lust_detected = 1L,
    num_reports = .N,
    first_report_date = min(report_dt, na.rm = TRUE)
  )
}, by = .(facility_id, state, 
         panel_year = year(as.Date(report_date)), 
         panel_month = month(as.Date(report_date)))]

# Aggregate TX LUST by month
lust_tx_m <- if (nrow(TX_LUST_SD)) {
  TX_LUST_SD[, .(
    lust_detected = 1L,
    num_reports = .N,
    first_report_date = suppressWarnings(min(report_date, na.rm = TRUE))
  ), by = .(facility_id, state, 
           panel_year = year(report_date), 
           panel_month = month(report_date))]
} else data.table()

# Combine all LUST data
lust_all <- rbindlist(list(lust_epa_m, lust_tx_m), use.names = TRUE, fill = TRUE)

# Merge to panel
setkey(lust_all, facility_id, state, panel_year, panel_month)
setkey(panel, facility_id, state, panel_year, panel_month)
panel <- lust_all[panel, on = .(facility_id, state, panel_year, panel_month)]
panel[is.na(lust_detected), `:=`(lust_detected = 0L, num_reports = 0L)]
panel[, leak_incident := lust_detected]

# Use actual closure dates (not panel-bounded) for accurate leak timing analysis
closure_events <- tank_panel_unified[!is.na(tank_closed_date_raw), .(
  facility_id, 
  state,
  closed_date = as.Date(tank_closed_date_raw)
)]

leak_events <- rbindlist(list(
  lust_epa[, .(facility_id, state, report_date = as.Date(report_date))],
  if (nrow(TX_LUST_SD)) TX_LUST_SD[, .(facility_id, state, report_date = as.Date(report_date))] else NULL
), use.names = TRUE, fill = TRUE)

timing_monthly <- NULL
if (nrow(closure_events) && nrow(leak_events)) {
  setkey(closure_events, facility_id, state)
  setkey(leak_events, facility_id, state)

  pairs <- leak_events[closure_events, on = .(facility_id, state), allow.cartesian = TRUE]
  pairs <- unique(pairs, by = c("facility_id", "state", "report_date", "closed_date"))
  pairs[, `:=`(
    leak_within_90d = as.integer(report_date >= closed_date & report_date <= closed_date + 90),
    leak_before = as.integer(report_date < closed_date),
    leak_after_window = as.integer(report_date > closed_date + 90)
  )]
  
  leak_timing <- pairs[, .(
    lust_within_90d_of_closure = max(leak_within_90d, na.rm = TRUE),
    leak_not_found_by_exit = max(leak_before, na.rm = TRUE),
    leak_found_by_closure = max(leak_within_90d, na.rm = TRUE),
    leak_after_closure_window = max(leak_after_window, na.rm = TRUE)
  ), by = .(facility_id, state, closed_date)]
  
  leak_timing[, `:=`(
    panel_year = year(closed_date),
    panel_month = month(closed_date)
  )]
  
  timing_monthly <- leak_timing[, .(
    facility_id, state, panel_year, panel_month,
    lust_within_90d_of_closure, leak_not_found_by_exit,
    leak_found_by_closure, leak_after_closure_window
  )]
  
  setkey(timing_monthly, facility_id, state, panel_year, panel_month)
  panel <- timing_monthly[panel, on = .(facility_id, state, panel_year, panel_month)]
}

# Fill NA leak timing variables
for (v in c("lust_within_90d_of_closure","leak_not_found_by_exit",
            "leak_found_by_closure","leak_after_closure_window")) {
  if (!v %in% names(panel)) panel[, (v) := 0L]
  panel[is.na(get(v)), (v) := 0L]
}

cat("\n  Section 7 Validation:\n")
cat(sprintf("    Total leak incidents: %s\n", sum(panel$leak_incident)))
cat(sprintf("    Facilities with leaks: %s\n", uniqueN(panel[leak_incident == 1]$facility_id)))
cat(sprintf("    Leaks within 90d of closure: %s\n", sum(panel$lust_within_90d_of_closure)))
log_memory("After LUST merge")
cat(sprintf("  Time elapsed: %.1f seconds\n\n", difftime(Sys.time(), start_time, units="secs")))

#==============================================================================
# SECTION 8: TREATMENT AND POLICY VARIABLES
#==============================================================================
cat("SECTION 8: Adding treatment/policy variables...\n")
start_time <- Sys.time()

# Define treatment states and years
treated_states <- c("Texas","Wisconsin","New Jersey","Michigan","Iowa",
                   "Florida","Arizona","Connecticut")
treatment_years <- c(1999, 1998, 2010, 2014, 2000, 1999, 2006, 2010)
names(treatment_years) <- treated_states

control_states <- c("Maine","New Mexico","Arkansas","Oklahoma","Louisiana",
                   "Kansas","Montana","Idaho","South Dakota","Alabama",
                   "Minnesota","North Carolina","Illinois","Massachusetts",
                   "Ohio","Pennsylvania","Tennessee","Virginia")

# Add treatment variables
panel[, `:=`(
  treatment_year = treatment_years[state],
  treated = state %in% treated_states,
  texas_treated = as.integer(state == "Texas"),
  has_fr_requirements = as.integer(state == "Texas"),
  post_1999 = as.integer(panel_year >= 2000)
)]

panel[is.na(treatment_year), treatment_year := 9999L]

panel[, `:=`(
  post_treatment = as.integer(treated & panel_year >= treatment_year),
  rel_year = ifelse(treated, panel_year - treatment_year, NA_integer_),
  treatment_group = fcase(
    state == "Texas", "Texas",
    treated & state != "Texas", "Other Treated",
    default = "Control"
  ),
  period = fifelse(panel_year >= 2000, "Post-Policy (2000+)", "Pre-Policy (<2000)")
)]

panel[, `:=`(
  treatment_group = factor(treatment_group, levels = c("Texas","Other Treated","Control")),
  rel_year_factor = factor(rel_year)
)]

cat("\n  Section 8 Validation:\n")
cat(sprintf("    Texas facilities: %s\n", uniqueN(panel[state == "Texas"]$facility_id)))
cat(sprintf("    Treated state facilities: %s\n", uniqueN(panel[treated == TRUE]$facility_id)))
cat(sprintf("    Post-treatment obs: %s\n", sum(panel$post_treatment)))
log_memory("After treatment vars")
cat(sprintf("  Time elapsed: %.1f seconds\n\n", difftime(Sys.time(), start_time, units="secs")))

#==============================================================================
# SECTION 9: DERIVED VARIABLES AND SURVIVAL ANALYSIS
#==============================================================================
cat("SECTION 9: Creating derived variables...\n")
start_time <- Sys.time()

# Wall type indicators
panel[, `:=`(
  has_single_walled = as.integer(single_tanks > 0),
  has_double_walled = as.integer(double_tanks > 0)
)]

panel[, wall_type := fcase(
  has_single_walled > 0 & has_double_walled == 0, "Single-Walled",
  has_double_walled > 0 & has_single_walled == 0, "Double-Walled",
  has_single_walled > 0 & has_double_walled > 0, "Mixed",
  default = "Unknown"
)]

# Fuel type categories
panel[, fuel_type := fcase(
  has_gasoline == 1 & has_diesel == 1, "Both",  # From Section 4
  has_gasoline == 1, "Gasoline Only",
  has_diesel == 1, "Diesel Only",
  default = "Other"
)]

# Age bins
panel[, age_bins := cut(
  avg_tank_age, 
  breaks = c(seq(0, 35, by = 5), Inf),
  include.lowest = TRUE, 
  right = TRUE,
  labels = c(paste0(seq(0,30,5), "-", seq(5,35,5)), "35+")
)]
panel[, age_group := age_bins]

# Sort for cumulative calculations
setorder(panel, facility_id, state, date)

# Cumulative measures
panel[, `:=`(
  cumulative_closures = cumsum(tanks_closed),
  cumulative_replacements = cumsum(replacement_event),
  cumulative_single_to_double = cumsum(single_to_double_replacement),
  has_previous_closure = as.integer(cumsum(tanks_closed) > 0),
  has_previous_replacement = as.integer(cumsum(replacement_event) > 0),
  months_since_entry = seq_len(.N) - 1L
), by = .(facility_id, state)]

# Entry/Exit flags
panel[, entry_flag := as.integer(months_since_entry == 0L)]
panel[, last_active_month := suppressWarnings(max(months_since_entry[active_tanks > 0], na.rm = TRUE)), 
      by = .(facility_id, state)]
panel[is.infinite(last_active_month) | is.na(last_active_month), last_active_month := -1L]
panel[, exit_flag := as.integer(months_since_entry == last_active_month & active_tanks > 0L)]
panel[, last_active_month := NULL]

# Days since last closure
panel[, last_closure_date := {
  closure_dates <- fifelse(tanks_closed > 0, date, as.Date(NA))
  closure_dates <- as.Date(closure_dates, origin = "1970-01-01")
  zoo::na.locf(closure_dates, na.rm = FALSE)
}, by = .(facility_id, state)]

panel[, days_since_last_closure := as.integer(
  difftime(as.Date(date), as.Date(last_closure_date), units = "days")
)]

panel[days_since_last_closure < 0, days_since_last_closure := NA_integer_]
panel[, last_closure_date := NULL]

# Retrofit timing
panel[, retrofit_timing := fifelse(
  tanks_installed > 0 & !is.na(days_since_last_closure) & days_since_last_closure <= 365,
  days_since_last_closure, NA_integer_
)]

# Survival analysis variables
panel[, `:=`(
  first_leak_index = ifelse(any(leak_incident == 1L), which(leak_incident == 1L)[1], NA_integer_),
  first_leak_time = ifelse(any(leak_incident == 1L), 
                           months_since_entry[which(leak_incident == 1L)[1]], NA_integer_)
), by = .(facility_id, state)]

panel[, `:=`(
  time_to_first_leak = first_leak_time,
  time_to_event = first_leak_time,
  event_indicator = 0L
)]

panel[!is.na(first_leak_index),
      event_indicator := as.integer(seq_len(.N) == first_leak_index),
      by = .(facility_id, state)]

panel[, censored := 0L]
panel[is.na(first_leak_time) & months_since_entry == max(months_since_entry),
      censored := 1L, by = .(facility_id, state)]

cat("\n  Section 9 Validation:\n")
cat(sprintf("    Wall types - Single: %s, Double: %s, Mixed: %s\n",
            sum(panel$wall_type == "Single-Walled", na.rm=TRUE),
            sum(panel$wall_type == "Double-Walled", na.rm=TRUE),
            sum(panel$wall_type == "Mixed", na.rm=TRUE)))
cat(sprintf("    Facilities with previous closures: %s\n", 
            uniqueN(panel[has_previous_closure == 1]$facility_id)))
cat(sprintf("    Survival - Events: %s, Censored: %s\n",
            sum(panel$event_indicator), sum(panel$censored)))
log_memory("After derived vars")
cat(sprintf("  Time elapsed: %.1f seconds\n\n", difftime(Sys.time(), start_time, units="secs")))


#==============================================================================
# SECTION 10: TEXAS FR DATA MERGE (CORRECTED)
#==============================================================================
cat("SECTION 10: Merging Texas FR data...\n")
start_time <- Sys.time()

# Initialize FR columns
fr_cols <- c("DETAIL_TYPE","CATEGORY","ISSUER_NAME","multiple_contracts",
             "fr_covered","uses_private","uses_state_fund","uses_self",
             "coverage_gap_month","had_zurich_2012","dropped_by_zurich",
             "erp_reporting_month","transition_month")

# Set default values for all FR columns
panel[, `:=`(
  DETAIL_TYPE = NA_character_,
  CATEGORY = NA_character_,
  ISSUER_NAME = NA_character_,
  multiple_contracts = 0L,
  fr_covered = FALSE,
  uses_private = 0L,
  uses_state_fund = 0L,
  uses_self = 0L,
  coverage_gap_month = 0L,
  had_zurich_2012 = 0L,
  dropped_by_zurich = 0L,
  erp_reporting_month = 0L,
  transition_month = 0L
)]

if (!is.null(fa_monthly) && nrow(fa_monthly) > 0) {
  fm <- copy(fa_monthly)
  
  # Standardize column names if needed
  if ("FACILITY_ID" %in% names(fm) && !"facility_id" %in% names(fm)) {
    setnames(fm, "FACILITY_ID", "facility_id")
  }
  
  fm[, facility_id := clean_id(facility_id)]
  
  # Ensure proper types
  fm <- fm[, .(
    panel_year = as.integer(panel_year),
    panel_month = as.integer(panel_month),
    facility_id = as.character(facility_id),
    DETAIL_TYPE = trimws(as.character(DETAIL_TYPE)),
    CATEGORY = trimws(as.character(CATEGORY)),
    ISSUER_NAME = trimws(as.character(ISSUER_NAME))
  )]
  
  # Calculate multiple contracts
  fm[, multiple_contracts := as.integer(
    length(unique(na.omit(ISSUER_NAME))) > 1 | 
    length(unique(na.omit(DETAIL_TYPE))) > 1
  ), by = .(facility_id, panel_year, panel_month)]
  
  # Keep unique rows
  fm <- unique(fm, by = c("facility_id","panel_year","panel_month"))
  
  # Add Zurich flags if available
  if (!is.null(zurich_2012_lookup) && nrow(zurich_2012_lookup) > 0) {
    zl <- copy(zurich_2012_lookup)
    if ("FACILITY_ID" %in% names(zl) && !"facility_id" %in% names(zl)) {
      setnames(zl, "FACILITY_ID", "facility_id")
    }
    zl[, facility_id := clean_id(facility_id)]
    zl[, had_zurich_2012 := as.integer(had_zurich_2012)]
    
    setkey(zl, facility_id)
    setkey(fm, facility_id)
    fm <- zl[fm, on = .(facility_id)]
    fm[is.na(had_zurich_2012), had_zurich_2012 := 0L]
  } else {
    fm[, had_zurich_2012 := 0L]
  }
  
  # Create temporary merge key in both tables
  panel[, merge_key := paste(facility_id, panel_year, panel_month, sep = "_")]
  fm[, merge_key := paste(facility_id, panel_year, panel_month, sep = "_")]
  
  # Create lookup environment for fast matching (Texas only)
  tx_keys <- panel[state == "Texas", merge_key]
  fm_subset <- fm[merge_key %in% tx_keys]
  
  # Create lookup vectors
  setkey(fm_subset, merge_key)
  detail_lookup <- fm_subset$DETAIL_TYPE
  names(detail_lookup) <- fm_subset$merge_key
  
  category_lookup <- fm_subset$CATEGORY
  names(category_lookup) <- fm_subset$merge_key
  
  issuer_lookup <- fm_subset$ISSUER_NAME
  names(issuer_lookup) <- fm_subset$merge_key
  
  contracts_lookup <- fm_subset$multiple_contracts
  names(contracts_lookup) <- fm_subset$merge_key
  
  zurich_lookup <- fm_subset$had_zurich_2012
  names(zurich_lookup) <- fm_subset$merge_key
  
  # Apply lookups to Texas rows only
  panel[state == "Texas", `:=`(
    DETAIL_TYPE = detail_lookup[merge_key],
    CATEGORY = category_lookup[merge_key],
    ISSUER_NAME = issuer_lookup[merge_key],
    multiple_contracts = as.integer(contracts_lookup[merge_key]),
    had_zurich_2012 = as.integer(zurich_lookup[merge_key])
  )]
  
  # Clean up temporary column
  panel[, merge_key := NULL]
  
  # Set fr_covered flag based on whether FR data exists
  panel[state == "Texas", fr_covered := !is.na(DETAIL_TYPE)]
  
  cat(sprintf("  FR data merged for %s TX facility-months\n", 
              sum(panel$state == "Texas" & panel$fr_covered == TRUE, na.rm=TRUE)))
} else {
  cat("  No FA monthly data available, skipping FR merge\n")
}

cat("\n  Section 10 Validation:\n")
if (!is.null(fa_monthly) && nrow(fa_monthly) > 0) {
  cat(sprintf("    TX facilities with FR: %s\n", 
              uniqueN(panel[state == "Texas" & fr_covered == TRUE]$facility_id)))
  cat(sprintf("    Zurich dropouts: %s\n", 
              sum(panel$dropped_by_zurich, na.rm=TRUE)))
}
log_memory("After FR merge")
cat(sprintf("  Time elapsed: %.1f seconds\n\n", 
            difftime(Sys.time(), start_time, units="secs")))


#==============================================================================
# SECTION 11: FINAL CLEANUP AND QUALITY CHECKS
#==============================================================================
cat("SECTION 11: Final cleanup and validation...\n")
start_time <- Sys.time()

# Ensure proper data types
panel[, `:=`(
  facility_id = as.character(facility_id),
  state = as.character(state),
  county_name = as.character(county_name),
  county_geoid = as.character(county_geoid),
  month_key = sprintf("%d-%02d", panel_year, panel_month)
)]

# Clean infinite values
for (v in c("avg_tank_age","min_tank_age","max_tank_age")) {
  if (v %in% names(panel)) {
    panel[is.infinite(get(v)), (v) := NA_real_]
  }
}

# Final sort
setkey(panel, facility_id, state, date)

# Final validation
cat("\n  FINAL VALIDATION SUMMARY:\n")
cat("  ========================================\n")
cat(sprintf("  Total observations: %s\n", format(nrow(panel), big.mark=",")))
cat(sprintf("  Total facilities: %s\n", format(uniqueN(panel$facility_id), big.mark=",")))
cat(sprintf("  States: %d\n", uniqueN(panel$state)))
cat(sprintf("  Date range: %s to %s\n", 
            as.character(min(panel$date, na.rm=TRUE)), 
            as.character(max(panel$date, na.rm=TRUE))))
cat(sprintf("  Leak incidents: %s\n", format(sum(panel$leak_incident, na.rm=TRUE), big.mark=",")))
cat(sprintf("  Replacement events: %s\n", format(sum(panel$replacement_event, na.rm=TRUE), big.mark=",")))
cat(sprintf("  Single-to-double upgrades: %s\n", format(sum(panel$single_to_double_replacement, na.rm=TRUE), big.mark=",")))
cat("\n  Data quality checks:\n")
cat(sprintf("    Facilities with negative tank age: %d\n", 
            uniqueN(panel[avg_tank_age < 0]$facility_id)))
cat(sprintf("    Facilities with tank age > 50 years: %d\n",
            uniqueN(panel[avg_tank_age > 50]$facility_id)))
cat(sprintf("    Facilities with capacity > 100000: %d\n", 
            uniqueN(panel[total_capacity > 100000]$facility_id)))
cat(sprintf("    Missing county info: %d facilities\n", 
            uniqueN(panel[is.na(county_name)]$facility_id)))
cat(sprintf("  Time elapsed: %.1f seconds\n\n", difftime(Sys.time(), start_time, units="secs")))

#==============================================================================
# SECTION 12: SAVE OUTPUT
#==============================================================================
cat("SECTION 12: Saving output...\n")
output_file <- here("Data", "Processed", "facility_leak_behavior_monthly.csv")
fwrite(panel, output_file)

file_size_mb <- file.size(output_file) / (1024 * 1024)
cat(sprintf("✓ Saved: %s (%.1f MB)\n", output_file, file_size_mb))

# Final memory cleanup
rm(list = setdiff(ls(), "panel"))
invisible(gc())

# Calculate total runtime
script_end_time <- Sys.time()
cat("\n========================================\n")
cat("PROCESSING COMPLETE\n")
cat(sprintf("Script completed at: %s\n", format(script_end_time, "%Y-%m-%d %H:%M:%S")))
if (exists("script_start_time")) {
  actual_runtime <- difftime(script_end_time, script_start_time, units = "mins")
  cat(sprintf("Total runtime: %.1f minutes\n", as.numeric(actual_runtime)))
}
cat("========================================\n")
#==============================================================================