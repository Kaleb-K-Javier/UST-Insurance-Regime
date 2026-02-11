###############################################################################
# 01_clean_texas_inventory.R
# ==========================
# Texas UST Data Pipeline - Stage 1: Inventory & LUST Harmonization
#
# Purpose:
#   Generate cleaned, harmonized UST and LUST facility inventories that mirror
#   the schema of the Louisiana reference script (02_Clean_LA.R).
#
# Inputs:
#   Raw .txt files from Script 00 in: Data/Raw/state_databases/Texas/
#
# Outputs:
#   TX_Harmonized_UST_tanks.csv - Tank-level inventory (LA-compatible schema)
#   TX_Harmonized_LUST.csv      - LUST incident records (LA-compatible schema)
#
# Author: UST Research Pipeline
# Date: 2026-01
#
# Reference: Louisiana schema from 02_Clean_LA.R
###############################################################################

# ══════════════════════════════════════════════════════════════════════════════
# SECTION 0: Setup & Configuration
# ══════════════════════════════════════════════════════════════════════════════

rm(list = ls())

library(data.table)
library(lubridate)
library(stringr)
library(here)

# ── Path Configuration ───────────────────────────────────────────────────────
RAW_DIR    <- here("Data", "Raw", "state_databases", "Texas")
OUTPUT_DIR <- here("Data", "Raw", "state_databases", "Texas")

# Create output directory if needed
if (!dir.exists(OUTPUT_DIR)) {
  dir.create(OUTPUT_DIR, recursive = TRUE)
}

# ── Server Configuration ─────────────────────────────────────────────────────
onserver <- Sys.info()["nodename"] != "localhost"  # Adjust as needed
if (requireNamespace("parallel", quietly = TRUE)) {
  if (onserver) {
    setDTthreads(parallel::detectCores() - 2)
  } else {
    setDTthreads(4)
  }
}
message("data.table using ", getDTthreads(), " threads")

# ══════════════════════════════════════════════════════════════════════════════
# SECTION 1: Helper Functions
# ══════════════════════════════════════════════════════════════════════════════

#' Load and parse fixed-width file from TCEQ
#'
#' @param filepath Path to the file
#' @param col_widths Vector of column widths
#' @param col_names Vector of column names
#' @return data.table with parsed data
load_fixed_width <- function(filepath, col_widths, col_names) {
  if (!file.exists(filepath)) {
    stop("File not found: ", filepath)
  }
  
  message("Loading: ", basename(filepath))
  
  raw <- readLines(filepath, warn = FALSE)
  
  # Parse fixed-width format
  starts <- cumsum(c(1, col_widths[-length(col_widths)]))
  ends   <- cumsum(col_widths)
  
  split_row <- function(row) {
    mapply(substr, row, starts, ends, USE.NAMES = FALSE)
  }
  
  mat <- t(vapply(raw, split_row, 
                  FUN.VALUE = character(length(col_widths)),
                  USE.NAMES = FALSE))
  
  dt <- as.data.table(mat)
  setnames(dt, col_names)
  
  # Trim whitespace from all columns
  dt[, (col_names) := lapply(.SD, trimws), .SDcols = col_names]
  
  message("  Loaded ", nrow(dt), " records, ", ncol(dt), " columns")
  
  return(dt[])
}

#' Clean ID columns (standardize format)
#'
#' @param dt data.table
#' @param col_name Column name to clean
#' @return Modified data.table (by reference)
clean_id_column <- function(dt, col_name) {
  if (col_name %in% names(dt)) {
    dt[, (col_name) := trimws(as.character(get(col_name)))]
  }
  invisible(dt)
}


#' Classify tank substances using regex patterns
#' Mirrors the logic from the original monolithic script
#'
#' @param data data.table with 'substances' column
#' @return data.table with classification columns added
classify_substances <- function(data) {
  result <- copy(data)

  # Initialize classification columns
  result[, `:=`(
    is_gasoline      = 0L,
    is_diesel        = 0L,
    is_oil_kerosene  = 0L,
    is_jet_fuel      = 0L,
    is_other         = 1L  # Default to 1, set to 0 if any category matches
  )]

  # ── Classification Patterns ────────────────────────────────────────────────
  
  gasoline_patterns <- paste(c(
    "gas", "gasolin", "petrol", "motor fuel", "fuel oil", "e10", "e15", "e85",
    "e\\d+", "unleaded", "premium", "regular", "petroleum", "fuel", "conv[[:alpha:]]+",
    "mid", "plus", "supreme", "super", "octane", "87", "89", "91", "93", "\\bmtbe\\b"
  ), collapse = "|")

  diesel_patterns <- paste(c(
    "diesel", "dsl", "dl$", "\\bd\\b", "bio.?diesel", "red diesel", "off.?road diesel",
    "^d ", "\\bdl\\b", "ulsd", "\\bd[0-9]\\b"
  ), collapse = "|")

  oil_kerosene_patterns <- paste(c(
    "oil", "kerosene", "kerosine", "heating oil", "motor oil", "used oil",
    "waste oil", "lubricant", "lube", "heating", "hydraulic", "transmission",
    "crude", "heavy", "mineral", "vegetable", "\\bcrank"
  ), collapse = "|")

  jet_fuel_patterns <- paste(c(
    "jet", "aviation", "av gas", "avgas", "jp.?[45678]", "aircraft", "airplane"
  ), collapse = "|")

  # ── Apply Classifications ──────────────────────────────────────────────────
  result[, substances_lower := tolower(substances)]

  # Gasoline
  result[!is.na(substances_lower) & substances_lower != "",
         is_gasoline := as.integer(grepl(gasoline_patterns, substances_lower, perl = TRUE))]

  # Diesel (exclude DEF - Diesel Exhaust Fluid)
  result[!is.na(substances_lower) & substances_lower != "" &
         !grepl("def|diesel exhaust fluid", substances_lower, perl = TRUE),
         is_diesel := as.integer(grepl(diesel_patterns, substances_lower, perl = TRUE))]

  # Oil/Kerosene
  result[!is.na(substances_lower) & substances_lower != "",
         is_oil_kerosene := as.integer(grepl(oil_kerosene_patterns, substances_lower, perl = TRUE))]

  # Jet Fuel
  result[!is.na(substances_lower) & substances_lower != "",
         is_jet_fuel := as.integer(grepl(jet_fuel_patterns, substances_lower, perl = TRUE))]

  # Set "other" to 0 if any category matched
  result[is_gasoline == 1L | is_diesel == 1L | is_oil_kerosene == 1L | is_jet_fuel == 1L,
         is_other := 0L]

  # Clean up temp column
  result[, substances_lower := NULL]

  # ── Summary ────────────────────────────────────────────────────────────────
  message("\nSubstance classification summary:")
  summary_dt <- result[, .(
    total_records     = .N,
    gasoline_count    = sum(is_gasoline),
    diesel_count      = sum(is_diesel),
    oil_kerosene_count = sum(is_oil_kerosene),
    jet_fuel_count    = sum(is_jet_fuel),
    other_count       = sum(is_other),
    multi_category    = sum(is_gasoline + is_diesel + is_oil_kerosene + is_jet_fuel > 1)
  )]
  print(summary_dt)

  return(result)
}

#' Standardize Numeric IDs (Strip leading zeros and spaces)
#' Converts to integer then back to character to ensure "00123" == "123"
#'
#' @param dt data.table
#' @param col_name Column name to clean
#' @return Modified data.table
standardize_numeric_id <- function(dt, col_name) {
  if (col_name %in% names(dt)) {
    # Suppress warnings for NAs introduced by non-numeric values
    val <- suppressWarnings(as.integer(as.character(dt[[col_name]])))

    # Check if we created too many NAs (implies column wasn't actually numeric)
    if (mean(is.na(val)) > mean(is.na(dt[[col_name]])) + 0.5) {
      warning(paste("Column", col_name, "appears to be alphanumeric. Using strict trimming only."))
      dt[, (col_name) := gsub("^0+", "", trimws(as.character(get(col_name))))]
    } else {
      dt[, (col_name) := as.character(val)]
    }
  }
  invisible(dt)
}

#' Classify tank wall types
#' Uses Texas-specific flags: TANK_SINGLE, TANK_DOUBLE
#'
#' @param data data.table with tank construction flags
#' @return data.table with wall type classification columns
classify_tank_walls <- function(data) {
  result <- copy(data)
  
  # ── Texas-Specific Wall Type Logic ─────────────────────────────────────────
  # TANK_SINGLE and TANK_DOUBLE are Y/N flags from the UST file
  
  result[, `:=`(
    # Single-walled: TANK_SINGLE = 'Y'
    single_walled = as.integer(TANK_SINGLE == "Y"),
    
    # Double-walled: TANK_DOUBLE = 'Y'
    double_walled = as.integer(TANK_DOUBLE == "Y"),
    
    # Missing: Both are NA
    missing_walled = as.integer(is.na(TANK_SINGLE) & is.na(TANK_DOUBLE)),
    
    # Unknown: Both are explicitly 'N' (neither single nor double)
    unknown_walled = as.integer(TANK_SINGLE == "N" & TANK_DOUBLE == "N")
  )]
  
  # Handle NAs - set to 0 where NA
  wall_cols <- c("single_walled", "double_walled", "missing_walled", "unknown_walled")
  for (col in wall_cols) {
    result[is.na(get(col)), (col) := 0L]
  }
  
  # Consolidate missing and unknown for harmonization
  result[, unknown_walled := pmax(unknown_walled, missing_walled, na.rm = TRUE)]
  
  # ── Summary ────────────────────────────────────────────────────────────────
  message("\nTank wall classification summary:")
  summary_dt <- result[, .(
    total       = .N,
    single_wall = sum(single_walled),
    double_wall = sum(double_walled),
    unknown     = sum(unknown_walled)
  )]
  print(summary_dt)
  
  return(result)
}

#' Map Texas tank status to standardized categories
#'
#' @param status_code Raw Texas status string
#' @return Standardized status: "Open", "Closed", "Temporary", or NA
standardize_tank_status <- function(status_code) {
  status_upper <- toupper(trimws(status_code))
  
  # Texas status codes mapping
  case_when(
    status_upper %in% c("IN USE", "IN-USE", "ACTIVE") ~ "Open",
    status_upper %in% c("REMOVED", "CLOSED", "PERMANENTLY CLOSED", 
                        "REMOVED FROM GROUND", "CLOSURE IN PLACE") ~ "Closed",
    status_upper %in% c("TEMPORARILY CLOSED", "TEMP CLOSURE", 
                        "TEMPORARILY OUT OF SERVICE") ~ "Temporary",
    TRUE ~ NA_character_
  )
}

# ══════════════════════════════════════════════════════════════════════════════
# SECTION 2: File Specifications (from 00_download_texas.R)
# ══════════════════════════════════════════════════════════════════════════════

# ── PST_FAC.TXT Specification ────────────────────────────────────────────────
FAC_WIDTHS <- c(
  8, 15, 6, 60, 30, 10, 30, 1, 1, 1,
  4, 4, 50, 30, 2, 5, 4, 256, 35, 35,
  2, 5, 15, 15, 28, 60, 100, 50, 50, 30,
  2, 5, 4, 3, 7, 5, 3, 7, 5, 50,
  1, 10, 10, 15, 15, 28, 60, 35, 100, 1,
  10, 1, 30, 30
)

FAC_NAMES <- c(
  "FACILITY_ID", "ADD_ID", "AI", "FACILITY_NAME", "FACILITY_TYPE",
  "FACILITY_BEGIN_DATE", "FACILITY_STATUS", "FACILITY_EXEMPT_STATUS",
  "RECORDS_OFF_SITE", "UST_FR_REQUIRED", "NUM_ACTIVE_USTS", "NUM_ACTIVE_ASTS",
  "SITE_ADDRESS_DELIVERY", "SITE_CITY", "SITE_STATE_CODE", "SITE_ZIP",
  "SITE_ZIP_EXT", "SITE_LOCATION_DESC", "SITE_NEAREST_CITY", "SITE_COUNTY",
  "SITE_REGION", "SITE_LOCATION_ZIP", "CONTACT_FN", "CONTACT_MI", "CONTACT_LN",
  "CONTACT_TITLE", "CONTACT_ORG", "CONTACT_ADDR_DELIV", "CONTACT_ADDR_INTDELIV",
  "CONTACT_ADDR_CITY", "CONTACT_ADDR_STATE", "CONTACT_ADDR_ZIP",
  "CONTACT_ADDR_ZIP_EXT", "CONTACT_PHONE_AC", "CONTACT_PHONE_NUM",
  "CONTACT_PHONE_EXT", "CONTACT_FAX_AC", "CONTACT_FAX_NUM", "CONTACT_FAX_EXT",
  "CONTACT_EMAIL", "CONTACT_ADDR_DELIVERABLE", "APP_REC_DATE", "SIG_DATE",
  "SIG_FN", "SIG_MI", "SIG_LN", "SIG_TITLE", "SIG_ROLE", "SIG_COMPANY",
  "ENFORCEMENT_ACTION", "ENFORCEMENT_ACTION_DATE",
  "FACILITY_NOT_INSPECTABLE", "NOT_INSPECTABLE_REASON_1",
  "NOT_INSPECTABLE_REASON_2"
)

# ── PST_UST.TXT Specification ────────────────────────────────────────────────
UST_WIDTHS <- c(
  8, 8, 6, 10, 2, 10, 10, 8, 30, 10, 1, 30, 10, 1, 1,
  1, 1, 1, 1, 1, 1, 1, 1, 1,
  1, 1, 1, 1, 1, 1,
  1, 1, 1, 1, 1,
  1, 1, 1,
  1, 1, 1, 1,
  1,
  rep(1, 16),
  1, 1, 10
)

UST_NAMES <- c(
  "UST_ID", "FACILITY_ID_PAD", "AI", "TANK_ID", "COMPARTS",
  "INSTALL_DATE", "REG_DATE", "CAPACITY", "STATUS", "STATUS_DATE",
  "EMPTY", "REG_STATUS", "TANK_INT_PROT_DATE", "TANK_SINGLE", "TANK_DOUBLE",
  "PIP_SINGLE", "PIP_DOUBLE",
  "EXT_CONT_JACKET", "EXT_CONT_SYN_LNR", "EXT_CONT_VAULT",
  "EXT_CONT_PIP_JACKET", "EXT_CONT_PIP_SYN_LNR", "EXT_CONT_PIP_VAULT",
  "PIPE_TYPE",
  "TANK_MAT_STEEL", "TANK_MAT_FRP", "TANK_MAT_COMPOSITE",
  "TANK_MAT_CONCRETE", "TANK_MAT_JACKETED", "TANK_MAT_COATED",
  "PIP_MAT_STEEL", "PIP_MAT_FRP", "PIP_MAT_CONCRETE",
  "PIP_MAT_JACKETED", "PIP_MAT_FLEX",
  "PIP_VALVE_SHEAR", "PIP_SWING_JOINT", "PIP_FLEX_CONN",
  "CORR_TANK_CP", "CORR_PIPE_CP", "CORR_TANK_VARIANCE", "CORR_PIPE_VARIANCE",
  "TOS_COMPLY",
  paste0("UNUSED_FLAG_", 1:16),
  "TECH_COMPLY", "TANK_TESTED_FLAG", "INSTALL_SIG_DATE"
)

# ── PST_UST_COMPRT.TXT Specification ─────────────────────────────────────────
COMPRT_WIDTHS <- c(
  8, 8, 6, 10, 1, 8,
  30, 30, 30,
  rep(1, 30),
  30, 10
)

COMPRT_NAMES <- c(
  "UST_COMPRT_ID", "UST_ID", "FACILITY_ID", "TANK_ID", "COMPRT_ID",
  "CAPACITY",
  "SUBSTANCE_STORED_1", "SUBSTANCE_STORED_2", "SUBSTANCE_STORED_3",
  "DET_C_VAPOR", "DET_C_GW", "DET_C_SEC_CONT", "DET_C_ATG",
  "DET_C_INTERSTITIAL", "DET_C_MANUAL_WEEK", "DET_C_MANUAL_MONTH", "DET_C_SIR",
  "DET_P_VAPOR", "DET_P_GW", "DET_P_SEC_CONT", "DET_P_INTERSTITIAL",
  "DET_P_TT_MONTH", "DET_P_TT_ANNUAL", "DET_P_TT_TRIEN", "DET_P_LLD",
  "DET_P_SIR", "DET_P_SUCTION_EXEMPT",
  "SPILL_TIGHT_FILL", "SPILL_FACTORY_SPILL", "SPILL_SHUTOFF_VALVE",
  "SPILL_FLOW_RESTRICT", "SPILL_ALARM", "SPILL_NA",
  "COMPRT_DET_COMPLY", "PIP_DET_COMPLY", "SPILL_COMPLY",
  "COMPRT_DET_VARIANCE", "PIP_DET_VARIANCE", "SPILL_VARIANCE",
  "STAGE1_VAPOR_RECOVERY", "STAGE1_INSTALL_DATE"
)


# ══════════════════════════════════════════════════════════════════════════════
# SECTION 3: Load Raw Data
# ══════════════════════════════════════════════════════════════════════════════

message("\n", strrep("=", 79))
message("SECTION 3: Loading Raw Data")
message(strrep("=", 79), "\n")

# ── Load Facility File ───────────────────────────────────────────────────────
fac_path <- file.path(RAW_DIR, "pst_fac.txt")
fac <- load_fixed_width(fac_path, FAC_WIDTHS, FAC_NAMES)
# Facility IDs are often clean, but standardization is safer
standardize_numeric_id(fac, "FACILITY_ID")

# ── Load UST File ────────────────────────────────────────────────────────────
ust_path <- file.path(RAW_DIR, "pst_ust.txt")
ust <- load_fixed_width(ust_path, UST_WIDTHS, UST_NAMES)

# Extract FACILITY_ID first
ust[, FACILITY_ID := trimws(substr(FACILITY_ID_PAD, 1, 6))]

# Standardize IDs
standardize_numeric_id(ust, "UST_ID")
standardize_numeric_id(ust, "FACILITY_ID")

# ── Load Compartment File ────────────────────────────────────────────────────
comprt_path <- file.path(RAW_DIR, "pst_ust_comprt.txt")
compartment <- load_fixed_width(comprt_path, COMPRT_WIDTHS, COMPRT_NAMES)

# Standardize IDs to match UST table
standardize_numeric_id(compartment, "UST_ID")
standardize_numeric_id(compartment, "FACILITY_ID")

# DIAGOSTIC: Check intersection
common_ids <- length(intersect(ust$UST_ID, compartment$UST_ID))
message("Diagnostic: ", common_ids, " UST_IDs match between UST and Compartment files.")


# ══════════════════════════════════════════════════════════════════════════════
# SECTION 4: Filter Non-Federally Regulated Facilities
# ══════════════════════════════════════════════════════════════════════════════

message("\n", strrep("=", 79))
message("SECTION 4: Filtering Non-Federally Regulated Facilities")
message(strrep("=", 79), "\n")

# Identify federally-regulated facilities
# UST_FR_REQUIRED = 'Y' indicates federal regulation applies
fac[, UST_FR_REQUIRED := UST_FR_REQUIRED == "Y"]

n_total_fac <- nrow(fac)
n_fr_required <- sum(fac$UST_FR_REQUIRED, na.rm = TRUE)

message("Total facilities: ", format(n_total_fac, big.mark = ","))
message("FR-required facilities: ", format(n_fr_required, big.mark = ","))
message("Non-FR facilities (to be dropped): ", format(n_total_fac - n_fr_required, big.mark = ","))

# Keep only FR-required facilities
fac_fr <- fac[UST_FR_REQUIRED == TRUE]

# Get list of FR-required facility IDs for filtering other tables
fr_facility_ids <- fac_fr$FACILITY_ID

# Filter UST table
n_ust_before <- nrow(ust)
ust <- ust[FACILITY_ID %in% fr_facility_ids]
message("\nUST records: ", format(n_ust_before, big.mark = ","), " -> ", 
        format(nrow(ust), big.mark = ","))

# Filter compartment table
n_comprt_before <- nrow(compartment)
compartment <- compartment[FACILITY_ID %in% fr_facility_ids]
message("Compartment records: ", format(n_comprt_before, big.mark = ","), " -> ", 
        format(nrow(compartment), big.mark = ","))

# ══════════════════════════════════════════════════════════════════════════════
# SECTION 5: Process UST Data
# ══════════════════════════════════════════════════════════════════════════════

message("\n", strrep("=", 79))
message("SECTION 5: Processing UST Data")
message(strrep("=", 79), "\n")

# ── Parse Dates ──────────────────────────────────────────────────────────────
ust[, `:=`(
  INSTALL_DATE = mdy(INSTALL_DATE),
  REG_DATE     = mdy(REG_DATE),
  STATUS_DATE  = mdy(STATUS_DATE)
)]

# ── Parse Capacity ───────────────────────────────────────────────────────────
ust[, CAPACITY := as.numeric(gsub("[^0-9.]", "", CAPACITY))]

# ── Standardize Tank Status ──────────────────────────────────────────────────
# Create a mapping of Texas statuses to standardized categories
message("\nTank status distribution (raw):")
print(ust[, .N, by = STATUS][order(-N)])

ust[, tank_status := fcase(
  toupper(STATUS) %in% c("IN USE", "IN-USE", "ACTIVE"), "Open",
  toupper(STATUS) %in% c("REMOVED", "CLOSED", "PERMANENTLY CLOSED", 
                         "REMOVED FROM GROUND", "CLOSURE IN PLACE"), "Closed",
  toupper(STATUS) %in% c("TEMPORARILY CLOSED", "TEMP CLOSURE", 
                         "TEMPORARILY OUT OF SERVICE"), "Temporary",
  default = NA_character_
)]

message("\nTank status distribution (standardized):")
print(ust[, .N, by = tank_status][order(-N)])

# ── Classify Wall Types ──────────────────────────────────────────────────────
ust <- classify_tank_walls(ust)



# ══════════════════════════════════════════════════════════════════════════════
# SECTION 6: Processing Compartment Data (Substances)
# ══════════════════════════════════════════════════════════════════════════════

message("\n", strrep("=", 79))
message("SECTION 6: Processing Compartment Data (Substances)")
message(strrep("=", 79), "\n")

# Combine substance columns
compartment[, substances := {
  tmp <- paste(SUBSTANCE_STORED_1, SUBSTANCE_STORED_2, SUBSTANCE_STORED_3, sep = "; ")
  tmp <- gsub("^; |; $|; ; ", "", tmp)  # Clean up separators
  tmp <- gsub("^\\s*$", NA_character_, tmp)  # Empty to NA
  tmp
}]

# Aggregate to UST level (one row per UST_ID)
# Note: aggregating by UST_ID is sufficient; FACILITY_ID is redundant for the join key
compartment_agg <- compartment[, .(
  substances = paste(unique(na.omit(substances)), collapse = "; ")
), by = .(UST_ID)] 

# Classify substances
compartment_agg <- classify_substances(compartment_agg)

# Merge substance classifications back to UST data
# Note: We use UST_ID only.
substance_cols <- c("UST_ID", "is_gasoline", "is_diesel", 
                    "is_oil_kerosene", "is_jet_fuel", "is_other")

# Force character join just in case
ust[, UST_ID := as.character(UST_ID)]
compartment_agg[, UST_ID := as.character(UST_ID)]

ust <- merge(ust, compartment_agg[, ..substance_cols], by = "UST_ID", all.x = TRUE)

# Fill missing substance flags with 0
for (col in c("is_gasoline", "is_diesel", "is_oil_kerosene", "is_jet_fuel", "is_other")) {
  ust[is.na(get(col)), (col) := 0L]
}

# Diagnostic: Check populate rate
populated <- nrow(ust[is_gasoline == 1 | is_diesel == 1 | is_oil_kerosene == 1 | is_jet_fuel == 1 | is_other == 1])
message("Diagnostic: ", populated, " tanks have substance info populated after merge.")



# ══════════════════════════════════════════════════════════════════════════════
# SECTION 7: Merge Facility Information
# ══════════════════════════════════════════════════════════════════════════════

message("\n", strrep("=", 79))
message("SECTION 7: Merging Facility Information")
message(strrep("=", 79), "\n")

# Select relevant facility columns
fac_cols <- fac_fr[, .(FACILITY_ID, FACILITY_NAME, SITE_COUNTY)]

# Merge to UST data
ust <- merge(ust, fac_cols, by = "FACILITY_ID", all.x = TRUE)

message("Merged facility info to ", nrow(ust), " UST records")

# ══════════════════════════════════════════════════════════════════════════════
# SECTION 8: Create Harmonized Output (LA Schema)
# ══════════════════════════════════════════════════════════════════════════════

message("\n", strrep("=", 79))
message("SECTION 8: Creating Harmonized Output (LA-Compatible Schema)")
message(strrep("=", 79), "\n")

# ── Determine closure dates ──────────────────────────────────────────────────
ust[, is_closed_removed := tank_status == "Closed"]
ust[, tank_closed_date := fifelse(is_closed_removed == TRUE, STATUS_DATE, as.Date(NA))]

# ── Build harmonized output ──────────────────────────────────────────────────
# Schema matches Louisiana: 02_Clean_LA.R output columns
TX_Harmonized_UST_tanks <- ust[, .(
  # Identifiers
  facility_id   = as.character(FACILITY_ID),
  facility_name = str_to_title(trimws(gsub("[^[:alnum:] ]", "", FACILITY_NAME))),
  tank_id       = as.character(TANK_ID),
  state         = "TX",
  
  # Dates
  tank_installed_date = INSTALL_DATE,
  tank_closed_date    = tank_closed_date,
  
  # Status (Texas-localized codes preserved)
  tank_status = tank_status,
  
  # Capacity
  capacity = CAPACITY,
  
  # Wall Types (binary)
  single_walled  = as.integer(single_walled),
  double_walled  = as.integer(double_walled),
  unknown_walled = as.integer(unknown_walled),
  
  # Substances (binary)
  is_gasoline     = as.integer(is_gasoline),
  is_diesel       = as.integer(is_diesel),
  is_oil_kerosene = as.integer(is_oil_kerosene),
  is_jet_fuel     = as.integer(is_jet_fuel),
  is_other        = as.integer(is_other),
  
  # Location
  county_name = str_to_title(trimws(SITE_COUNTY))
)]

# ── Generate Summary Statistics ──────────────────────────────────────────────
message("\n", strrep("-", 60))
message("HARMONIZED UST INVENTORY SUMMARY")
message(strrep("-", 60))

message("\nTotal records: ", format(nrow(TX_Harmonized_UST_tanks), big.mark = ","))
message("Unique facilities: ", uniqueN(TX_Harmonized_UST_tanks$facility_id))
message("Unique tanks: ", uniqueN(TX_Harmonized_UST_tanks$tank_id))

message("\nTank Status:")
print(TX_Harmonized_UST_tanks[, .N, by = tank_status][order(-N)])

message("\nWall Type:")
message("  Single-walled: ", sum(TX_Harmonized_UST_tanks$single_walled))
message("  Double-walled: ", sum(TX_Harmonized_UST_tanks$double_walled))
message("  Unknown:       ", sum(TX_Harmonized_UST_tanks$unknown_walled))

message("\nSubstances:")
message("  Gasoline:     ", sum(TX_Harmonized_UST_tanks$is_gasoline))
message("  Diesel:       ", sum(TX_Harmonized_UST_tanks$is_diesel))
message("  Oil/Kerosene: ", sum(TX_Harmonized_UST_tanks$is_oil_kerosene))
message("  Jet Fuel:     ", sum(TX_Harmonized_UST_tanks$is_jet_fuel))
message("  Other:        ", sum(TX_Harmonized_UST_tanks$is_other))


# ══════════════════════════════════════════════════════════════════════════════
# SECTION 9: Processing LUST Data
# ══════════════════════════════════════════════════════════════════════════════

message("\n", strrep("=", 79))
message("SECTION 9: Processing LUST Data")
message(strrep("=", 79), "\n")

# Check if LUST file exists
lust_path <- file.path(RAW_DIR, "TX_LUST.csv")

if (file.exists(lust_path)) {
  TX_LUST_raw <- fread(lust_path)
  
  # Standardize column names
  if ("facility_id" %in% names(TX_LUST_raw)) {
    setnames(TX_LUST_raw, "facility_id", "FACILITY_ID")
  }
  
  standardize_numeric_id(TX_LUST_raw, "FACILITY_ID")
  
  # Parse Dates safely
  date_cols <- c("report_date", "nfa_date")
  for (col in date_cols) {
    if (col %in% names(TX_LUST_raw)) {
      # Try flexible parsing
      TX_LUST_raw[, (col) := lubridate::parse_date_time(get(col), 
                                                      orders = c("mdy", "ymd", "mdY", "Ymd"), quiet = TRUE)]
      TX_LUST_raw[, (col) := as.Date(get(col))]
    }
  }
  
  # Filter to FR-required facilities
  TX_LUST_raw <- TX_LUST_raw[FACILITY_ID %in% fr_facility_ids]
  
  # Create harmonized LUST output
  TX_Harmonized_LUST <- TX_LUST_raw[, .(
    facility_id = as.character(FACILITY_ID),
    lust_id     = as.character(LUST_id),
    report_date = as.Date(report_date),
    nfa_date    = as.Date(nfa_date),
    state       = "TX"
  )]
  
  message("Processed ", nrow(TX_Harmonized_LUST), " LUST records")
  message("Facilities with LUST: ", uniqueN(TX_Harmonized_LUST$facility_id))
  
} else {
  message("LUST file not found at: ", lust_path)
  # Create empty template
  TX_Harmonized_LUST <- data.table(
    facility_id = character(),
    lust_id     = character(),
    report_date = as.Date(character()),
    nfa_date    = as.Date(character()),
    state       = character()
  )
}

# ══════════════════════════════════════════════════════════════════════════════
# SECTION 10: Save Outputs
# ══════════════════════════════════════════════════════════════════════════════

message("\n", strrep("=", 79))
message("SECTION 10: Saving Outputs")
message(strrep("=", 79), "\n")

# Save harmonized UST inventory
ust_output_path <- file.path(OUTPUT_DIR, "TX_Harmonized_UST_tanks.csv")
fwrite(TX_Harmonized_UST_tanks, ust_output_path)
message("Saved: ", ust_output_path)

# Save harmonized LUST
lust_output_path <- file.path(OUTPUT_DIR, "TX_Harmonized_LUST.csv")
fwrite(TX_Harmonized_LUST, lust_output_path)
message("Saved: ", lust_output_path)

# Also save RDS for faster loading
saveRDS(TX_Harmonized_UST_tanks, file.path(OUTPUT_DIR, "TX_Harmonized_UST_tanks.rds"))
saveRDS(TX_Harmonized_LUST, file.path(OUTPUT_DIR, "TX_Harmonized_LUST.rds"))
message("Saved RDS versions")

# ══════════════════════════════════════════════════════════════════════════════
# SECTION 11: Validation Diagnostics
# ══════════════════════════════════════════════════════════════════════════════

message("\n", strrep("=", 79))
message("SECTION 11: Validation Diagnostics")
message(strrep("=", 79), "\n")

# Check for data quality issues
diagnostics <- list()

# 1. Missing critical fields
diagnostics$missing_facility_id <- sum(is.na(TX_Harmonized_UST_tanks$facility_id) | 
                                        TX_Harmonized_UST_tanks$facility_id == "")
diagnostics$missing_tank_id <- sum(is.na(TX_Harmonized_UST_tanks$tank_id) | 
                                    TX_Harmonized_UST_tanks$tank_id == "")
diagnostics$missing_install_date <- sum(is.na(TX_Harmonized_UST_tanks$tank_installed_date))
diagnostics$missing_status <- sum(is.na(TX_Harmonized_UST_tanks$tank_status))

# 2. Wall type consistency
diagnostics$both_single_and_double <- sum(
  TX_Harmonized_UST_tanks$single_walled == 1 & 
  TX_Harmonized_UST_tanks$double_walled == 1
)

# 3. No substance info
diagnostics$no_substance_info <- sum(
  TX_Harmonized_UST_tanks$is_gasoline == 0 &
  TX_Harmonized_UST_tanks$is_diesel == 0 &
  TX_Harmonized_UST_tanks$is_oil_kerosene == 0 &
  TX_Harmonized_UST_tanks$is_jet_fuel == 0 &
  TX_Harmonized_UST_tanks$is_other == 0
)

# Print diagnostics
message("Data Quality Diagnostics:")
message("  Missing facility_id:       ", diagnostics$missing_facility_id)
message("  Missing tank_id:           ", diagnostics$missing_tank_id)
message("  Missing install_date:      ", diagnostics$missing_install_date)
message("  Missing status:            ", diagnostics$missing_status)
message("  Both single & double wall: ", diagnostics$both_single_and_double)
message("  No substance info:         ", diagnostics$no_substance_info)

# Save diagnostics
diagnostics_dt <- as.data.table(diagnostics)
fwrite(diagnostics_dt, file.path(OUTPUT_DIR, "01_inventory_diagnostics.csv"))

message("\n", strrep("=", 79))
message("01_clean_texas_inventory.R COMPLETE")
message(strrep("=", 79), "\n")
