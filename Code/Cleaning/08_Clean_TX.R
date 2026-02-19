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
# Changelog (2026-02):
#   [FIX 1] Regulatory filter: Switched from facility-level UST_FR_REQUIRED to
#           tank-level REG_STATUS == "FULLY REGULATED" (per PST Data Spec).
#   [FIX 2] Table linkage: Compartment table now joined on UST_ID (not
#           FACILITY_ID), which was causing ~60% data loss.
#   [FIX 3] Substance aggregation: Deduplication now operates on individual
#           substance strings (unique + sort) before collapsing.
#   [FIX 4] Wall classification: Rule 11 (Concrete → Single_Walled) confirmed
#           correct. Added Rules 10-12; Unknown rate reduced from 45.5% → 10.5%.
#   [FIX 5] Output schema: Added classification_confidence to harmonized output.
#
# Author: UST Research Pipeline
# Date: 2026-01 (Updated 2026-02)
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


#' Classify tank wall types using Texas 12-Rule Hierarchy
#' CORRECTED VERSION: Rule 11 classifies Concrete as Single_Walled (not Secondary_Contained)
#'
#' Rules 1-9:  Original logic (unchanged)
#' Rule 10:    Old steel (pre-2000) without CP → Single_Walled [Low confidence]
#'             Impact: ~65,678 tanks (34.7% of database)
#' Rule 11:    Concrete → Single_Walled [Medium confidence]
#'             Concrete is a material type, not a containment configuration.
#'             Impact: ~482 tanks
#' Rule 12:    Coated steel → Single_Walled [Medium-Low confidence]
#'             Impact: ~4 tanks
#' Net result: Unknown rate drops from 45.5% → ~10.5%
#'
#' @param data data.table with tank construction flags
#' @return data.table with wall type classification columns added
classify_tank_walls <- function(data) {
  result <- copy(data)
  
  # ── Regulatory Constants ───────────────────────────────────────────────────
  TEXAS_SECONDARY_DATE <- as.Date("2009-01-01")
  FRP_CUTOFF_DATE      <- as.Date("1990-01-01")
  MODERN_STEEL_CUTOFF  <- as.Date("2000-01-01")
  
  # Ensure INSTALL_DATE is present
  if(!"INSTALL_DATE" %in% names(result)) {
    warning("INSTALL_DATE missing for classification. Logic will degrade.")
  }

  # ── Helper Flags ───────────────────────────────────────────────────────────
  result[, `:=`(
    # Material Types
    is_steel     = TMAT_STEEL == "Y",
    is_frp       = TMAT_FRP == "Y",
    is_composite = TMAT_COMPOSITE == "Y",
    is_concrete  = TMAT_CONCRETE == "Y",       # for Rule 11
    is_coated    = TMAT_COATED == "Y",         # for Rule 12
    is_jacketed  = (TMAT_JACKETED == "Y") | (EXT_CONT_TANK_JACKET == "Y"),
    
    # Corrosion Protection
    has_cp       = (CP_TANK_CATHODIC_FAC == "Y") | (CP_TANK_CATHODIC_FIELD == "Y"),
    
    # Design Flags (Explicit)
    design_dbl   = TANK_DES_DOUBLE == "Y",
    design_sgl   = TANK_DES_SINGLE == "Y"
  )]
  
  # ── 12-Rule Classification Hierarchy ───────────────────────────────────────
  result[, `:=`(
    # Track which rule matched (for diagnostics)
    rule_matched = fcase(
      # ── Original Rules 1-9 (unchanged) ──
      !is.na(INSTALL_DATE) & INSTALL_DATE >= TEXAS_SECONDARY_DATE, "Rule 1: Post-2009",
      design_dbl == TRUE,                                            "Rule 2: Design Double",
      design_sgl == TRUE,                                            "Rule 3: Design Single",
      is_jacketed == TRUE,                                           "Rule 4: Jacketed",
      is_composite == TRUE,                                          "Rule 5: Composite",
      is_steel == TRUE & has_cp == TRUE & !is.na(INSTALL_DATE) &
        INSTALL_DATE >= MODERN_STEEL_CUTOFF,                         "Rule 6: Modern Steel+CP",
      is_steel == TRUE & has_cp == TRUE,                             "Rule 7: Older Steel+CP",
      is_frp == TRUE & !is.na(INSTALL_DATE) &
        INSTALL_DATE >= FRP_CUTOFF_DATE,                             "Rule 8: Modern FRP",
      
      # ── New Rules 10-12: reduce Unknown 45.5% → 10.5% ──
      # Rule 10: Old bare steel (pre-2000) without cathodic protection
      # 99.8% are pre-1990; bare steel without jacketing is physically single-walled
      is_steel == TRUE & has_cp == FALSE & !is.na(INSTALL_DATE) &
        INSTALL_DATE < MODERN_STEEL_CUTOFF,                          "Rule 10: Old Steel-No-CP",
      
      # Rule 11: Concrete tanks
      # Concrete is a MATERIAL TYPE (like steel/FRP), not a containment configuration.
      # These tanks lack any double-wall indicators, so they are single-wall concrete.
      is_concrete == TRUE,                                            "Rule 11: Concrete",
      
      # Rule 12: Coated steel
      # Surface coating is corrosion protection, not secondary containment.
      is_coated == TRUE,                                              "Rule 12: Coated Steel",
      
      # Rule 13: Remaining unknowns (~19,815 tanks with no material flags)
      default = "Rule 13: Unknown"
    ),
    
    # ── Wall class ────────────────────────────────────────────────────────────
    wall_class = fcase(
      # Original Rules 1-9
      !is.na(INSTALL_DATE) & INSTALL_DATE >= TEXAS_SECONDARY_DATE,   "Double",
      design_dbl == TRUE,                                             "Double",
      design_sgl == TRUE,                                             "Single",
      is_jacketed == TRUE,                                            "Double",
      is_composite == TRUE,                                           "Double",
      is_steel == TRUE & has_cp == TRUE & !is.na(INSTALL_DATE) &
        INSTALL_DATE >= MODERN_STEEL_CUTOFF,                          "Single",
      is_steel == TRUE & has_cp == TRUE,                              "Single",
      is_frp == TRUE & !is.na(INSTALL_DATE) &
        INSTALL_DATE >= FRP_CUTOFF_DATE,                              "Double",
      
      # New Rules 10-12 (all → Single)
      is_steel == TRUE & has_cp == FALSE & !is.na(INSTALL_DATE) &
        INSTALL_DATE < MODERN_STEEL_CUTOFF,                           "Single",
      is_concrete == TRUE,                                            "Single",
      is_coated == TRUE,                                              "Single",
      
      default = "Unknown"
    ),
    
    # ── Confidence level ──────────────────────────────────────────────────────
    classification_confidence = fcase(
      # Very High: Post-2009 regulatory mandate (30 TAC § 334.45(d)(1)(E))
      !is.na(INSTALL_DATE) & INSTALL_DATE >= TEXAS_SECONDARY_DATE,   "Very High",
      # High: Explicit design flags entered by facility operators
      design_dbl == TRUE | design_sgl == TRUE,                        "High",
      # Medium-High: Physical evidence of construction type
      is_jacketed == TRUE | is_composite == TRUE,                     "Medium-High",
      # Medium: Material + protection combination strongly implies wall type
      (is_steel == TRUE & has_cp == TRUE) |
        (is_frp == TRUE & !is.na(INSTALL_DATE) &
           INSTALL_DATE >= FRP_CUTOFF_DATE) |
        is_concrete == TRUE,                                           "Medium",
      # Low: Inferred from era and material (reasonable but not direct evidence)
      is_steel == TRUE & has_cp == FALSE & !is.na(INSTALL_DATE) &
        INSTALL_DATE < MODERN_STEEL_CUTOFF,                           "Low",
      # Medium-Low: Surface coating implies single-wall but limited direct evidence
      is_coated == TRUE,                                              "Medium-Low",
      # Very Low: No material flags available; cannot classify
      default = "Very Low"
    )
  )]
  
  # ── Map to binary output columns ──────────────────────────────────────────
  result[, `:=`(
    single_walled  = as.integer(wall_class == "Single"),
    double_walled  = as.integer(wall_class == "Double"),
    unknown_walled = as.integer(wall_class == "Unknown")
  )]
  
  # Cleanup helper columns
  cols_to_remove <- c("is_steel", "is_frp", "is_composite", "is_concrete",
                      "is_coated", "is_jacketed", "has_cp", "design_dbl",
                      "design_sgl", "wall_class")
  result[, (cols_to_remove) := NULL]
  
  # ── Summaries ──────────────────────────────────────────────────────────────
  message("\nTank wall classification summary (12-Rule Enhanced):")
  summary_dt <- result[, .(
    total       = .N,
    single_wall = sum(single_walled),
    double_wall = sum(double_walled),
    unknown     = sum(unknown_walled),
    unknown_pct = round(sum(unknown_walled) / .N * 100, 1)
  )]
  print(summary_dt)
  
  message("\nClassification by rule:")
  rule_summary <- result[, .N, by = rule_matched][order(-N)]
  rule_summary[, pct := round(N / sum(N) * 100, 1)]
  print(rule_summary)
  
  message("\nClassification confidence distribution:")
  conf_summary <- result[, .N, by = classification_confidence][
    order(factor(classification_confidence,
                 levels = c("Very High", "High", "Medium-High", "Medium",
                            "Medium-Low", "Low", "Very Low")))
  ]
  conf_summary[, pct := round(N / sum(N) * 100, 1)]
  print(conf_summary)
  
  return(result)
}


#' Map Texas tank status to standardized categories
#' ENHANCED: Includes mapping for "PERM FILLED IN PLACE"
#'
#' @param status_code Raw Texas status string
#' @return Standardized status: "Open", "Closed", "Temporary", or NA
standardize_tank_status <- function(status_code) {
  status_upper <- toupper(trimws(status_code))
  
  fcase(
    status_upper %in% c("IN USE", "IN-USE", "ACTIVE"), "Open",
    
    # Enhanced: Added PERM FILLED IN PLACE based on diagnostics
    status_upper %in% c("REMOVED", "CLOSED", "PERMANENTLY CLOSED",
                        "REMOVED FROM GROUND", "CLOSURE IN PLACE",
                        "PERM FILLED IN PLACE"), "Closed",
                        
    status_upper %in% c("TEMPORARILY CLOSED", "TEMP CLOSURE",
                        "TEMPORARILY OUT OF SERVICE"), "Temporary",
    default = NA_character_
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
  # Core fields (Bytes 1-143)
  8, 8, 6, 10, 2, 10, 10, 8, 30, 10, 1, 30, 10, 
  # Tank/Pipe Design & Containment (Bytes 144-153)
  1, 1, 1, 1,      # Tank/Pipe Single/Double
  1, 1, 1,         # Tank Ext Cont (Jacket, Syn, Vault)
  1, 1, 1,         # Pipe Ext Cont (Jacket, Syn, Vault)
  # Materials & Hardware (Bytes 154-168)
  1,               # Pipe Type
  1, 1, 1, 1, 1, 1,# Tank Mat (Steel, FRP, Comp, Conc, Jacketed, Coated)
  1, 1, 1, 1, 1,   # Pipe Mat (Steel, FRP, Conc, Jacketed, Flex)
  1, 1, 1,         # Pipe Hardware (Shear, Swing, Flex)
  # Corrosion Protection - TANK (Bytes 169-176)
  1, 1, 1, 1, 1, 1, 1, 1,
  # Corrosion Protection - PIPING (Bytes 177-184)
  1, 1, 1, 1, 1, 1, 1, 1,
  # Compliance & Variance (Bytes 185-188)
  1, 1,            # Tank/Pipe CP Compliance
  1, 1,            # Tank/Pipe CP Variance
  # Operational Status & Signature (Bytes 189-201)
  1,               # TOS (Temp Out of Service) Compliance
  1,               # Tech Compliance
  1,               # Tank Tested
  10               # Install Sig Date
)

UST_NAMES <- c(
  # Core fields
  "UST_ID", "FACILITY_ID_PAD", "AI", "TANK_ID", "COMPARTS",
  "INSTALL_DATE", "REG_DATE", "CAPACITY", "STATUS", "STATUS_DATE",
  "EMPTY", "REG_STATUS", "TANK_INT_PROT_DATE", 
  # Tank/Pipe Design
  "TANK_DES_SINGLE", "TANK_DES_DOUBLE", "PIP_DES_SINGLE", "PIP_DES_DOUBLE",
  # External Containment
  "EXT_CONT_TANK_JACKET", "EXT_CONT_TANK_SYN", "EXT_CONT_TANK_VAULT",
  "EXT_CONT_PIP_JACKET", "EXT_CONT_PIP_SYN", "EXT_CONT_PIP_VAULT",
  # Pipe Type & Materials
  "PIPE_TYPE",
  "TMAT_STEEL", "TMAT_FRP", "TMAT_COMPOSITE", "TMAT_CONCRETE", "TMAT_JACKETED", "TMAT_COATED",
  "PMAT_STEEL", "PMAT_FRP", "PMAT_CONCRETE", "PMAT_JACKETED", "PMAT_FLEX",
  "PHARD_SHEAR", "PHARD_SWING", "PHARD_FLEX",
  # Corrosion Protection - TANK (169-176)
  "CP_TANK_DIELECTRIC", "CP_TANK_CATHODIC_FAC", "CP_TANK_CATHODIC_FIELD", 
  "CP_TANK_COMPOSITE", "CP_TANK_COATED", "CP_TANK_FRP", 
  "CP_TANK_JACKETED", "CP_TANK_NOT_REQ",
  # Corrosion Protection - PIPING (177-184)
  "CP_PIP_DIELECTRIC", "CP_PIP_CATHODIC_FAC", "CP_PIP_CATHODIC_FIELD",
  "CP_PIP_FRP", "CP_PIP_FLEX", "CP_PIP_ISOLATED", 
  "CP_PIP_DUAL", "CP_PIP_NOT_REQ",
  # Compliance & Variance
  "COMPLY_CP_TANK", "COMPLY_CP_PIPE",
  "VAR_CP_TANK", "VAR_CP_PIPE",
  # Final Status
  "COMPLY_TOS",       # Byte 189 (Temporarily Out of Service)
  "COMPLY_TECH",      # Byte 190
  "TANK_TESTED_FLAG", # Byte 191
  "INSTALL_SIG_DATE"
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

# ── PST_CONST_NOTIF.TXT (Added for Date Rescue) ──────────────────────────────
PST_CONST_NOTIF_SPEC <- list(
  col_widths = c(8,8,6,10,10,1,1,1,1,1,1,1,1,1,1,15,1,1,10,10,15,15,28,100,60,35,203,25,23,250),
  col_names = c("NOC_ID","FACILITY_ID_PAD","AI","APP_REC_DATE","SCHED_CONST_DATE","WORK_UST_IMPROVE",
                "WORK_UST_INSTALL","WORK_UST_REMOVE","WORK_UST_REPAIR","WORK_UST_RETURN","WORK_UST_REPLACE",
                "WORK_UST_ABANDON","WORK_UST_STAGE1","WORK_AST_INSTALL","WORK_AST_STAGE1","HIST_TRACK_NUM",
                "WAIVER_FLAG","LATE_FLAG","FORM_REC_DATE","SIG_DATE","SIG_FN","SIG_MN","SIG_LN","SIG_COMPANY",
                "SIG_TITLE","SIG_ROLE","OWNER_NAMES_CONST","OWNER_CNS_CONST","OWNER_ARS_CONST","GEN_DESC")
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

# Standardize UST_ID to match UST table.
# NOTE: FACILITY_ID in the compartment file is unreliable and is NOT used for
# filtering (see FIX 2). Only UST_ID linkage is valid.
standardize_numeric_id(compartment, "UST_ID")

# Diagnostic: Check intersection before any filtering
common_ids <- length(intersect(ust$UST_ID, compartment$UST_ID))
message("Diagnostic: ", common_ids, " UST_IDs match between UST and Compartment files (pre-filter).")


# ══════════════════════════════════════════════════════════════════════════════
# SECTION 4: Filter to Federally Regulated Tanks
# ══════════════════════════════════════════════════════════════════════════════
# [FIX 1] Use tank-level REG_STATUS, NOT facility-level UST_FR_REQUIRED.
#
# Rationale: UST_FR_REQUIRED is a facility-level flag that can be set even when
# only a subset of tanks at that facility are federally regulated. Filtering on
# REG_STATUS == "FULLY REGULATED" at the tank level is precise and correct per
# the PST Data Specifications.
#
# [FIX 2] Compartment table is now filtered by UST_ID (not FACILITY_ID).
# FACILITY_ID in the compartment file is unreliable and caused ~60% data loss.
# ══════════════════════════════════════════════════════════════════════════════

message("\n", strrep("=", 79))
message("SECTION 4: Filtering to Fully Regulated Tanks (Tank-Level REG_STATUS)")
message(strrep("=", 79), "\n")

# Diagnostic: Show REG_STATUS distribution before filtering
message("REG_STATUS distribution (raw):")
print(ust[, .N, by = REG_STATUS][order(-N)])

# Filter UST table to fully regulated tanks only
n_ust_before <- nrow(ust)
ust <- ust[REG_STATUS == "FULLY REGULATED"]
n_ust_after  <- nrow(ust)

message("\nUST records after REG_STATUS filter: ",
        format(n_ust_before, big.mark = ","), " -> ",
        format(n_ust_after,  big.mark = ","),
        " (", round((n_ust_before - n_ust_after) / n_ust_before * 100, 1),
        "% removed)")

# Derive the set of valid UST IDs and facility IDs from regulated tanks.
# valid_ust_ids   → used to filter compartment table (FIX 2)
# fr_facility_ids → used to filter LUST data in Section 9
valid_ust_ids   <- ust$UST_ID
fr_facility_ids <- unique(ust$FACILITY_ID)

message("Regulated tanks span ", format(length(fr_facility_ids), big.mark = ","),
        " unique facilities.")

# [FIX 2] Filter compartment table by UST_ID (not FACILITY_ID)
n_comprt_before <- nrow(compartment)
compartment <- compartment[UST_ID %in% valid_ust_ids]
n_comprt_after  <- nrow(compartment)

message("\nCompartment records after UST_ID filter: ",
        format(n_comprt_before, big.mark = ","), " -> ",
        format(n_comprt_after,  big.mark = ","),
        " (", round((n_comprt_before - n_comprt_after) / n_comprt_before * 100, 1),
        "% removed)")

# Diagnostic: Verify linkage quality
linked_ust_ids <- length(intersect(ust$UST_ID, compartment$UST_ID))
message("\nDiagnostic: ", format(linked_ust_ids, big.mark = ","),
        " of ", format(n_ust_after, big.mark = ","),
        " regulated USTs have compartment data (",
        round(linked_ust_ids / n_ust_after * 100, 1), "%).")


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
message("\nTank status distribution (raw):")
print(ust[, .N, by = STATUS][order(-N)])

ust[, tank_status := standardize_tank_status(STATUS)]

message("\nTank status distribution (standardized):")
print(ust[, .N, by = tank_status][order(-N)])

# ── Date Rescue (Conditional) ────────────────────────────────────────────────
n_missing_install <- sum(is.na(ust$INSTALL_DATE))

if (n_missing_install > 0) {
  message("\n--- Initiating Construction Notification Date Rescue ---")
  notif_path <- file.path(RAW_DIR, "pst_const_notif.txt")
  
  if (file.exists(notif_path)) {
    notif <- load_fixed_width(notif_path, PST_CONST_NOTIF_SPEC$col_widths, PST_CONST_NOTIF_SPEC$col_names)
    
    notif[, `:=`(
      FACILITY_ID = trimws(substr(FACILITY_ID_PAD, 1, 6)),
      const_date  = suppressWarnings(lubridate::mdy(SCHED_CONST_DATE))
    )]
    
    install_evidence <- notif[
      WORK_UST_INSTALL == "Y" & !is.na(const_date),
      .(earliest_const_date = min(const_date, na.rm = TRUE)),
      by = FACILITY_ID
    ]
    
    ust <- merge(ust, install_evidence, by = "FACILITY_ID", all.x = TRUE)
    ust[is.na(INSTALL_DATE) & !is.na(earliest_const_date),
        INSTALL_DATE := earliest_const_date]
    
    message("Rescued dates for ",
            sum(!is.na(ust$earliest_const_date) & !is.na(ust$INSTALL_DATE)),
            " records.")
    ust[, earliest_const_date := NULL]
  } else {
    message("Construction Notification file not found. Skipping rescue.")
  }
} else {
  message("\nSkipping Construction Notification Date Rescue: INSTALL_DATE is 100% populated.")
}

# ── Classify Wall Types ──────────────────────────────────────────────────────
ust <- classify_tank_walls(ust)


# ══════════════════════════════════════════════════════════════════════════════
# SECTION 6: Processing Compartment Data (Substances)
# ══════════════════════════════════════════════════════════════════════════════
# [FIX 3] Substance deduplication now operates on individual substance strings.
#
# Old approach: concatenated all three substance columns per row into a single
# string, then applied unique() to the full strings. This meant "GASOLINE; ;
# DIESEL" and "DIESEL; GASOLINE; " were treated as different strings.
#
# New approach: gather all individual substance values across all rows for a
# given UST_ID, remove blanks/NAs, apply unique() + sort(), then collapse.
# This correctly deduplicates across duplicate compartment rows.
# ══════════════════════════════════════════════════════════════════════════════

message("\n", strrep("=", 79))
message("SECTION 6: Processing Compartment Data (Substances)")
message(strrep("=", 79), "\n")

# Diagnostic: Check for duplicate compartment rows before aggregation
n_comprt_rows   <- nrow(compartment)
n_comprt_unique <- uniqueN(compartment, by = c("UST_ID", "COMPRT_ID"))
message("Compartment rows: ", format(n_comprt_rows, big.mark = ","),
        " | Unique UST_ID + COMPRT_ID combos: ",
        format(n_comprt_unique, big.mark = ","),
        " | Duplicate rows: ",
        format(n_comprt_rows - n_comprt_unique, big.mark = ","))

# [FIX 3] Aggregate substances to UST level with proper deduplication.
# For each UST_ID, collect ALL substance strings from ALL rows and ALL columns,
# strip blanks/NAs, deduplicate, sort for consistency, then collapse.
compartment_agg <- compartment[, {
  # Gather all substance values across the three columns and all compartment rows
  all_subs <- c(SUBSTANCE_STORED_1, SUBSTANCE_STORED_2, SUBSTANCE_STORED_3)
  # Trim whitespace and remove blanks/NAs
  all_subs <- trimws(all_subs)
  all_subs <- all_subs[!is.na(all_subs) & all_subs != ""]
  # Deduplicate and sort for consistency
  all_subs <- sort(unique(all_subs))
  # Collapse to a single semi-colon delimited string (NA if none found)
  if (length(all_subs) == 0) {
    .(substances = NA_character_)
  } else {
    .(substances = paste(all_subs, collapse = "; "))
  }
}, by = .(UST_ID)]

message("Compartment aggregation: ", format(n_comprt_rows, big.mark = ","),
        " rows collapsed to ", format(nrow(compartment_agg), big.mark = ","),
        " UST-level records.")

# Classify substances
compartment_agg <- classify_substances(compartment_agg)

# Merge substance classifications back to UST data
substance_cols <- c("UST_ID", "is_gasoline", "is_diesel",
                    "is_oil_kerosene", "is_jet_fuel", "is_other")

ust[, UST_ID := as.character(UST_ID)]
compartment_agg[, UST_ID := as.character(UST_ID)]

ust <- merge(ust, compartment_agg[, ..substance_cols], by = "UST_ID", all.x = TRUE)

# Fill missing substance flags with 0 (tanks with no compartment records)
for (col in c("is_gasoline", "is_diesel", "is_oil_kerosene", "is_jet_fuel", "is_other")) {
  ust[is.na(get(col)), (col) := 0L]
}

# Diagnostic: Check populate rate
populated <- nrow(ust[is_gasoline == 1 | is_diesel == 1 |
                      is_oil_kerosene == 1 | is_jet_fuel == 1 | is_other == 1])
message("Diagnostic: ", format(populated, big.mark = ","),
        " tanks have substance info populated after merge (",
        round(populated / nrow(ust) * 100, 1), "%).")


# ══════════════════════════════════════════════════════════════════════════════
# SECTION 7: Merge Facility Information
# ══════════════════════════════════════════════════════════════════════════════

message("\n", strrep("=", 79))
message("SECTION 7: Merging Facility Information")
message(strrep("=", 79), "\n")

# Select relevant facility columns
fac_cols <- fac[, .(FACILITY_ID, FACILITY_NAME, SITE_COUNTY)]

# Merge to UST data (all.x = TRUE keeps all regulated tanks even if facility
# record is missing, which should not happen but is defensive)
ust <- merge(ust, fac_cols, by = "FACILITY_ID", all.x = TRUE)

message("Merged facility info to ", format(nrow(ust), big.mark = ","), " UST records.")


# ══════════════════════════════════════════════════════════════════════════════
# SECTION 8: Create Harmonized Output (LA Schema)
# ══════════════════════════════════════════════════════════════════════════════
# [FIX 5] Added classification_confidence to the harmonized output.
#         This field is produced by classify_tank_walls() and documents the
#         evidentiary basis for each tank's wall-type assignment.
# ══════════════════════════════════════════════════════════════════════════════

message("\n", strrep("=", 79))
message("SECTION 8: Creating Harmonized Output (LA-Compatible Schema)")
message(strrep("=", 79), "\n")

# ── Determine closure dates ──────────────────────────────────────────────────
ust[, is_closed_removed := tank_status == "Closed"]
ust[, tank_closed_date  := fifelse(is_closed_removed == TRUE, STATUS_DATE, as.Date(NA))]

# ── Build harmonized output ──────────────────────────────────────────────────
TX_Harmonized_UST_tanks <- ust[, .(
  # ── Identifiers ─────────────────────────────────────────────────────────
  facility_id   = as.character(FACILITY_ID),
  facility_name = str_to_title(trimws(gsub("[^[:alnum:] ]", "", FACILITY_NAME))),
  tank_id       = as.character(TANK_ID),
  state         = "TX",
  
  # ── Dates ───────────────────────────────────────────────────────────────
  tank_installed_date = INSTALL_DATE,
  tank_closed_date    = tank_closed_date,
  
  # ── Status ──────────────────────────────────────────────────────────────
  tank_status = tank_status,
  
  # ── Capacity ────────────────────────────────────────────────────────────
  capacity = CAPACITY,
  
  # ── Wall Types (binary + confidence) ────────────────────────────────────
  single_walled             = as.integer(single_walled),
  double_walled             = as.integer(double_walled),
  unknown_walled            = as.integer(unknown_walled),
  # [FIX 5] classification_confidence: documents the evidence basis per tank.
  # Values: "Very High", "High", "Medium-High", "Medium", "Medium-Low",
  #         "Low", "Very Low"
  classification_confidence = classification_confidence,
  
  # ── Substances (binary) ─────────────────────────────────────────────────
  is_gasoline     = as.integer(is_gasoline),
  is_diesel       = as.integer(is_diesel),
  is_oil_kerosene = as.integer(is_oil_kerosene),
  is_jet_fuel     = as.integer(is_jet_fuel),
  is_other        = as.integer(is_other),
  
  # ── Location ────────────────────────────────────────────────────────────
  county_name = str_to_title(trimws(SITE_COUNTY))
)]

# ── Summary Statistics ───────────────────────────────────────────────────────
message("\n", strrep("-", 60))
message("HARMONIZED UST INVENTORY SUMMARY")
message(strrep("-", 60))

message("\nTotal records:      ", format(nrow(TX_Harmonized_UST_tanks), big.mark = ","))
message("Unique facilities:  ", uniqueN(TX_Harmonized_UST_tanks$facility_id))
message("Unique tanks:       ", uniqueN(TX_Harmonized_UST_tanks$tank_id))

message("\nTank Status:")
print(TX_Harmonized_UST_tanks[, .N, by = tank_status][order(-N)])

message("\nWall Type:")
message("  Single-walled: ", format(sum(TX_Harmonized_UST_tanks$single_walled),  big.mark = ","))
message("  Double-walled: ", format(sum(TX_Harmonized_UST_tanks$double_walled),  big.mark = ","))
message("  Unknown:       ", format(sum(TX_Harmonized_UST_tanks$unknown_walled), big.mark = ","),
        " (", round(sum(TX_Harmonized_UST_tanks$unknown_walled) /
                    nrow(TX_Harmonized_UST_tanks) * 100, 1), "%)")

message("\nClassification Confidence:")
print(TX_Harmonized_UST_tanks[, .N, by = classification_confidence][order(-N)])

message("\nSubstances:")
message("  Gasoline:     ", format(sum(TX_Harmonized_UST_tanks$is_gasoline),     big.mark = ","))
message("  Diesel:       ", format(sum(TX_Harmonized_UST_tanks$is_diesel),       big.mark = ","))
message("  Oil/Kerosene: ", format(sum(TX_Harmonized_UST_tanks$is_oil_kerosene), big.mark = ","))
message("  Jet Fuel:     ", format(sum(TX_Harmonized_UST_tanks$is_jet_fuel),     big.mark = ","))
message("  Other:        ", format(sum(TX_Harmonized_UST_tanks$is_other),        big.mark = ","))


# ══════════════════════════════════════════════════════════════════════════════
# SECTION 9: Processing LUST Data
# ══════════════════════════════════════════════════════════════════════════════

message("\n", strrep("=", 79))
message("SECTION 9: Processing LUST Data")
message(strrep("=", 79), "\n")

lust_path <- file.path(RAW_DIR, "TX_LUST.csv")

if (file.exists(lust_path)) {
  TX_LUST_raw <- fread(lust_path)
  
  if ("facility_id" %in% names(TX_LUST_raw)) {
    setnames(TX_LUST_raw, "facility_id", "FACILITY_ID")
  }
  
  standardize_numeric_id(TX_LUST_raw, "FACILITY_ID")
  
  # Parse dates
  date_cols <- c("report_date", "nfa_date")
  for (col in date_cols) {
    if (col %in% names(TX_LUST_raw)) {
        TX_LUST_raw[, (col) := lubridate::parse_date_time(
          get(col), orders = c("dby", "dBY", "dmy", "dmY", "mdy", "ymd", "mdY", "Ymd"), 
          quiet = FALSE)]  # drop quiet=TRUE so future failures are visible
        TX_LUST_raw[, (col) := as.Date(get(col))]
    }
  }
  
  # Filter to facilities that have at least one regulated UST
  # (fr_facility_ids derived from regulated tanks in Section 4)
  TX_LUST_raw <- TX_LUST_raw[FACILITY_ID %in% fr_facility_ids]
  
  TX_Harmonized_LUST <- TX_LUST_raw[, .(
    facility_id = as.character(FACILITY_ID),
    lust_id     = as.character(LUST_id),
    report_date = as.Date(report_date),
    nfa_date    = as.Date(nfa_date),
    state       = "TX"
  )]
  
  message("Processed ", format(nrow(TX_Harmonized_LUST), big.mark = ","), " LUST records.")
  message("Facilities with LUST: ", uniqueN(TX_Harmonized_LUST$facility_id))
  
} else {
  message("LUST file not found at: ", lust_path)
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

ust_output_path  <- file.path(OUTPUT_DIR, "TX_Harmonized_UST_tanks.csv")
lust_output_path <- file.path(OUTPUT_DIR, "TX_Harmonized_LUST.csv")

fwrite(TX_Harmonized_UST_tanks, ust_output_path)
message("Saved: ", ust_output_path)

fwrite(TX_Harmonized_LUST, lust_output_path)
message("Saved: ", lust_output_path)

saveRDS(TX_Harmonized_UST_tanks, file.path(OUTPUT_DIR, "TX_Harmonized_UST_tanks.rds"))
saveRDS(TX_Harmonized_LUST,      file.path(OUTPUT_DIR, "TX_Harmonized_LUST.rds"))
message("Saved RDS versions.")


# ══════════════════════════════════════════════════════════════════════════════
# SECTION 11: Validation Diagnostics
# ══════════════════════════════════════════════════════════════════════════════

message("\n", strrep("=", 79))
message("SECTION 11: Validation Diagnostics")
message(strrep("=", 79), "\n")

diagnostics <- list()

# Missing critical fields
diagnostics$missing_facility_id  <- sum(is.na(TX_Harmonized_UST_tanks$facility_id) |
                                         TX_Harmonized_UST_tanks$facility_id == "")
diagnostics$missing_tank_id      <- sum(is.na(TX_Harmonized_UST_tanks$tank_id) |
                                         TX_Harmonized_UST_tanks$tank_id == "")
diagnostics$missing_install_date <- sum(is.na(TX_Harmonized_UST_tanks$tank_installed_date))
diagnostics$missing_status       <- sum(is.na(TX_Harmonized_UST_tanks$tank_status))

# Wall type consistency
diagnostics$both_single_and_double <- sum(
  TX_Harmonized_UST_tanks$single_walled == 1 &
  TX_Harmonized_UST_tanks$double_walled == 1
)

# No substance info
diagnostics$no_substance_info <- sum(
  TX_Harmonized_UST_tanks$is_gasoline     == 0 &
  TX_Harmonized_UST_tanks$is_diesel       == 0 &
  TX_Harmonized_UST_tanks$is_oil_kerosene == 0 &
  TX_Harmonized_UST_tanks$is_jet_fuel     == 0 &
  TX_Harmonized_UST_tanks$is_other        == 0
)

# Classification quality
diagnostics$unknown_rate_pct <- round(
  sum(TX_Harmonized_UST_tanks$unknown_walled) / nrow(TX_Harmonized_UST_tanks) * 100, 1)

message("Data Quality Diagnostics:")
message("  Missing facility_id:       ", diagnostics$missing_facility_id)
message("  Missing tank_id:           ", diagnostics$missing_tank_id)
message("  Missing install_date:      ", diagnostics$missing_install_date)
message("  Missing status:            ", diagnostics$missing_status)
message("  Both single & double wall: ", diagnostics$both_single_and_double)
message("  No substance info:         ", format(diagnostics$no_substance_info, big.mark = ","))
message("  Unknown wall rate:         ", diagnostics$unknown_rate_pct, "%",
        "  (target: ~10.5%)")

diagnostics_dt <- as.data.table(diagnostics)
fwrite(diagnostics_dt, file.path(OUTPUT_DIR, "01_inventory_diagnostics.csv"))

message("\n", strrep("=", 79))
message("01_clean_texas_inventory.R COMPLETE")
message(strrep("=", 79), "\n")