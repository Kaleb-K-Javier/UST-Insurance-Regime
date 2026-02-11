###############################################################################
# 02_clean_texas_fr.R
# ====================
# Texas UST Data Pipeline - Stage 2: Financial Responsibility Panel
#
# Purpose:
#   Process the Financial Responsibility (FR) raw data into longitudinal panels
#   for econometric analysis.
#
# Inputs:
#   Raw pst_fin_assur.txt from Script 00 in: Data/Raw/state_databases/Texas/
#
# Outputs:
#   1. texas_fr_facility_month_panel.csv - Monthly resolution panel
#   2. texas_fr_facility_year_panel.csv  - December 31 snapshots
#   3. texas_fr_contracts_clean.csv      - Unique list of cleaned contracts
#
# Event Indicators:
#   - Insurer/provider switches
#   - Instrument type changes (Insurance <-> Self-Insurance <-> State Fund)
#   - Coverage gaps and ERP periods
#
# Author: UST Research Pipeline
# Date: 2026-01
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
OUTPUT_DIR <- here("Data", "Processed")

# Create directories
if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)

# ── Panel Configuration ──────────────────────────────────────────────────────
PANEL_START_DATE <- as.Date("1990-01-01")
PANEL_END_DATE   <- as.Date("2026-12-01")

# ── Server Configuration ─────────────────────────────────────────────────────
onserver <- Sys.info()["nodename"] != "localhost"
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

#' Load and parse fixed-width file
load_fixed_width <- function(filepath, col_widths, col_names) {
  if (!file.exists(filepath)) {
    stop("File not found: ", filepath)
  }
  
  message("Loading: ", basename(filepath))
  raw <- readLines(filepath, warn = FALSE)
  
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
  dt[, (col_names) := lapply(.SD, trimws), .SDcols = col_names]
  
  message("  Loaded ", nrow(dt), " records")
  return(dt[])
}

#' Clean ID column
clean_id_column <- function(dt, col_name) {
  if (col_name %in% names(dt)) {
    dt[, (col_name) := trimws(as.character(get(col_name)))]
  }
  invisible(dt)
}

#' Fix malformed dates (e.g., "02 01" -> 2001, "007" -> 2007)
fix_malformed_fa_dates <- function(dt, eff_col = "EFF_DATE", exp_col = "EXP_DATE") {
  
  fix_year <- function(d) {
    out <- d
    bad <- !is.na(out) & (year(out) < 1900 | year(out) > 2100)
    
    if (any(bad)) {
      y_bad <- year(out[bad])
      out[bad] <- as.Date(sprintf(
        "%04d-%02d-%02d",
        y_bad %% 100 + 2000,
        month(out[bad]),
        mday(out[bad])
      ))
    }
    out
  }
  
  dt_work <- copy(dt)
  
  # Ensure Date class
  if (!inherits(dt_work[[eff_col]], "Date")) {
    dt_work[, (eff_col) := as.Date(get(eff_col))]
  }
  if (!inherits(dt_work[[exp_col]], "Date")) {
    dt_work[, (exp_col) := as.Date(get(exp_col))]
  }
  
  # Tag bad rows
  dt_work[, `:=`(
    bad_eff = !is.na(get(eff_col)) & (year(get(eff_col)) < 1900 | year(get(eff_col)) > 2100),
    bad_exp = !is.na(get(exp_col)) & (year(get(exp_col)) < 1900 | year(get(exp_col)) > 2100)
  )]
  
  n_originally_bad <- dt_work[bad_eff == TRUE | bad_exp == TRUE, .N]
  
  # If only one is bad, assume 1-year term
  dt_work[bad_eff == TRUE & bad_exp == FALSE, (eff_col) := get(exp_col) - years(1)]
  dt_work[bad_exp == TRUE & bad_eff == FALSE, (exp_col) := get(eff_col) + years(1)]
  
  # Apply year fixing
  dt_work[bad_eff == TRUE, (eff_col) := fix_year(get(eff_col))]
  dt_work[bad_exp == TRUE, (exp_col) := fix_year(get(exp_col))]
  
  # Flag unrescued
  dt_work[, date_fix_flag := (bad_eff | bad_exp) & (is.na(get(eff_col)) | is.na(get(exp_col)))]
  dt_work[, c("bad_eff", "bad_exp") := NULL]
  
  n_still_bad <- dt_work[date_fix_flag == TRUE, .N]
  
  message("Date fixing: ", n_originally_bad, " malformed -> ", 
          n_originally_bad - n_still_bad, " fixed, ", n_still_bad, " unrescued")
  
  return(dt_work)
}

#' Collapse contract-level rows to facility-month level
#' @param dt data.table with all contracts for ONE facility in ONE year-month
#' @return single-row data.table with aggregated flags
collapse_to_facility_month <- function(dt) {
  dt <- copy(dt)
  n <- nrow(dt)
  
  # Aggregates
  agg <- list(
    DETAIL_TYPE     = paste(sort(unique(dt$DETAIL_TYPE)), collapse = "; "),
    CATEGORY        = paste(sort(unique(dt$CATEGORY)), collapse = "; "),
    ISSUER_NAME     = paste(sort(unique(dt$ISSUER_NAME)), collapse = " | "),
    max_COVER_OCC   = if (all(is.na(dt$COVER_OCC))) NA_real_ else max(dt$COVER_OCC, na.rm = TRUE),
    max_COVER_AGG   = if (all(is.na(dt$COVER_AGG))) NA_real_ else max(dt$COVER_AGG, na.rm = TRUE),
    total_COVER_OCC = sum(dt$COVER_OCC, na.rm = TRUE),
    total_COVER_AGG = sum(dt$COVER_AGG, na.rm = TRUE),
    premium_prepaid = any(dt$PREMIUM_PREPAID, na.rm = TRUE),
    proof_on_file   = any(dt$PROOF_OF_FA, na.rm = TRUE),
    fp_corr_met     = any(dt$FP_CORR_MET, na.rm = TRUE),
    tp_fa_met       = any(dt$TP_FA_MET, na.rm = TRUE),
    uses_private    = any(dt$USE_PRIVATE, na.rm = TRUE),
    uses_self       = any(dt$USE_SELF, na.rm = TRUE)
  )
  
  # Detect overlap/transition
  if (n == 1L) {
    transition_month   <- FALSE
    multiple_contracts <- FALSE
    active_ids         <- dt$FIN_ASSUR_ID[1]
  } else if (n == 2L) {
    ord <- order(dt$start_day)
    s <- as.numeric(dt$start_day[ord])
    e <- as.numeric(dt$end_day[ord])
    overlaps <- s[2] <= e[1]
    
    if (!overlaps) {
      transition_month   <- TRUE
      multiple_contracts <- FALSE
      active_ids         <- dt$FIN_ASSUR_ID[which.max(dt$start_day)]
    } else {
      transition_month   <- FALSE
      multiple_contracts <- TRUE
      active_ids         <- paste(sort(unique(dt$FIN_ASSUR_ID)), collapse = " | ")
    }
  } else {
    transition_month   <- FALSE
    multiple_contracts <- TRUE
    active_ids         <- paste(sort(unique(dt$FIN_ASSUR_ID)), collapse = " | ")
  }
  
  data.table(
    DETAIL_TYPE         = agg$DETAIL_TYPE,
    CATEGORY            = agg$CATEGORY,
    ISSUER_NAME         = agg$ISSUER_NAME,
    active_FIN_ASSUR_ID = active_ids,
    max_COVER_OCC       = agg$max_COVER_OCC,
    max_COVER_AGG       = agg$max_COVER_AGG,
    total_COVER_OCC     = agg$total_COVER_OCC,
    total_COVER_AGG     = agg$total_COVER_AGG,
    premium_prepaid     = agg$premium_prepaid,
    proof_on_file       = agg$proof_on_file,
    fp_corr_met         = agg$fp_corr_met,
    tp_fa_met           = agg$tp_fa_met,
    uses_private        = agg$uses_private,
    uses_self           = agg$uses_self,
    transition_month    = transition_month,
    multiple_contracts  = multiple_contracts,
    fr_covered          = TRUE
  )
}

# ══════════════════════════════════════════════════════════════════════════════
# SECTION 2: File Specification
# ══════════════════════════════════════════════════════════════════════════════

FA_WIDTHS <- c(
  8, 8, 6, 10, 30, 30, 1, 50,
  5, 3, 7, 5, 30, 10, 10, 30, 30,
  1, 1, 1, 1, 1
)

FA_NAMES <- c(
  "FIN_ASSUR_ID", "FACILITY_ID_PAD", "FACILITY_AI", "FORM_REC_DATE",
  "MECH_TYPE", "MECH_TYPE_OTHER", "MULTI_MECH_TYPES", "ISSUER_NAME",
  "ISSUER_PHONE_CTRY", "ISSUER_PHONE_AREA", "ISSUER_PHONE_NUM",
  "ISSUER_PHONE_EXT", "POLICY_MECH_NUM", "COVER_EFF", "COVER_EXP",
  "COVER_OCC", "COVER_AGG", "PREMIUM_PREPAID", "FP_CORR_MET",
  "TP_FA_MET", "PROOF_OF_FA", "MEETS_FLAG"
)

# ══════════════════════════════════════════════════════════════════════════════
# SECTION 3: Load Financial Assurance Data
# ══════════════════════════════════════════════════════════════════════════════

message("\n", strrep("=", 79))
message("SECTION 3: Loading Financial Assurance Data")
message(strrep("=", 79), "\n")

fa_path <- file.path(RAW_DIR, "pst_fin_assur.txt")
fa <- load_fixed_width(fa_path, FA_WIDTHS, FA_NAMES)

# Extract FACILITY_ID
fa[, FACILITY_ID := trimws(substr(FACILITY_ID_PAD, 1, 6))]
clean_id_column(fa, "FACILITY_ID")
clean_id_column(fa, "FIN_ASSUR_ID")

# ══════════════════════════════════════════════════════════════════════════════
# SECTION 4: Parse and Clean Dates
# ══════════════════════════════════════════════════════════════════════════════

message("\n", strrep("=", 79))
message("SECTION 4: Parsing and Cleaning Dates")
message(strrep("=", 79), "\n")

# Convert Y/N flags
flag_cols <- c("PREMIUM_PREPAID", "FP_CORR_MET", "TP_FA_MET", "PROOF_OF_FA", "MEETS_FLAG", "MULTI_MECH_TYPES")
for (col in flag_cols) {
  fa[, (col) := get(col) == "Y"]
}

# Parse dates
fa[, `:=`(
  EFF_DATE = mdy(COVER_EFF),
  EXP_DATE = mdy(COVER_EXP)
)]

# Fix malformed dates
fa <- fix_malformed_fa_dates(fa, "EFF_DATE", "EXP_DATE")

# Parse coverage amounts
fa[, `:=`(
  COVER_OCC_clean = gsub("[^0-9.]", "", COVER_OCC),
  COVER_AGG_clean = gsub("[^0-9.]", "", COVER_AGG)
)]
fa[, `:=`(
  COVER_OCC = as.numeric(fifelse(COVER_OCC_clean == "", NA_character_, COVER_OCC_clean)),
  COVER_AGG = as.numeric(fifelse(COVER_AGG_clean == "", NA_character_, COVER_AGG_clean))
)]
fa[, c("COVER_OCC_clean", "COVER_AGG_clean") := NULL]

# ══════════════════════════════════════════════════════════════════════════════
# SECTION 5: Map Mechanism Types to Categories
# ══════════════════════════════════════════════════════════════════════════════

message("\n", strrep("=", 79))
message("SECTION 5: Mapping Mechanism Types")
message(strrep("=", 79), "\n")

fa[, MECH_TYPE_upper := toupper(trimws(MECH_TYPE))]

# Lookup table for mechanism types
mech_lookup <- data.table(
  MECH_TYPE_upper = c(
    "INSURANCE OR RISK RETENTION", "FINANCIAL TEST", "LOCAL GOV FIN TEST",
    "LOCAL GOV GUARANTEE", "GUARANTEE", "LETTER OF CREDIT", "SURETY BOND",
    "TRUST FUND", "OTHER", ""
  ),
  DETAIL_TYPE = c(
    "Insurance Policy", "Self-Insurance", "Government Guarantee",
    "Government Guarantee", "Self-Insurance", "Self-Insurance", "Self-Insurance",
    "Self-Insurance", "Other or Unspecified", "Other or Unspecified"
  ),
  CATEGORY = c(
    "Insurance", "Self-Insurance", "Government Guarantee",
    "Government Guarantee", "Self-Insurance", "Self-Insurance", "Self-Insurance",
    "Self-Insurance", "Other or Unspecified", "Other or Unspecified"
  )
)

fa <- merge(fa, mech_lookup, by = "MECH_TYPE_upper", all.x = TRUE)

# Handle PST Remediation Fund (State Fund)
fa[MECH_TYPE_upper == "TRUST FUND" & 
   grepl("PST REMEDIATION FUND", MECH_TYPE_OTHER, ignore.case = TRUE),
   `:=`(DETAIL_TYPE = "State Insurance", CATEGORY = "State Fund")]

fa[MECH_TYPE_upper == "OTHER" & 
   grepl("PST REMEDIATION FUND", MECH_TYPE_OTHER, ignore.case = TRUE),
   `:=`(DETAIL_TYPE = "State Insurance", CATEGORY = "State Fund")]

# Fill unmatched
fa[is.na(DETAIL_TYPE), DETAIL_TYPE := "Other or Unspecified"]
fa[is.na(CATEGORY), CATEGORY := "Other or Unspecified"]

# Create convenience flags
fa[, `:=`(
  USE_PRIVATE = CATEGORY == "Insurance",
  USE_SELF    = CATEGORY == "Self-Insurance"
)]

message("\nMechanism type distribution:")
print(fa[, .N, by = .(CATEGORY, DETAIL_TYPE)][order(-N)])

fa[, MECH_TYPE_upper := NULL]

# ══════════════════════════════════════════════════════════════════════════════
# SECTION 6: Create Clean Contract Inventory
# ══════════════════════════════════════════════════════════════════════════════

message("\n", strrep("=", 79))
message("SECTION 6: Creating Clean Contract Inventory")
message(strrep("=", 79), "\n")

fa_contracts_clean <- fa[, .(
  FIN_ASSUR_ID,
  FACILITY_ID,
  EFF_DATE,
  EXP_DATE,
  MECH_TYPE,
  MECH_TYPE_OTHER,
  DETAIL_TYPE,
  CATEGORY,
  ISSUER_NAME,
  POLICY_MECH_NUM,
  COVER_OCC,
  COVER_AGG,
  PREMIUM_PREPAID,
  FP_CORR_MET,
  TP_FA_MET,
  PROOF_OF_FA,
  MEETS_FLAG,
  USE_PRIVATE,
  USE_SELF,
  date_fix_flag
)]

message("Clean contract inventory: ", nrow(fa_contracts_clean), " contracts")
message("Unique facilities: ", uniqueN(fa_contracts_clean$FACILITY_ID))

# ══════════════════════════════════════════════════════════════════════════════
# SECTION 7: Expand Contracts to Monthly Observations
# ══════════════════════════════════════════════════════════════════════════════

message("\n", strrep("=", 79))
message("SECTION 7: Expanding Contracts to Monthly Level")
message(strrep("=", 79), "\n")

# Filter valid contracts
fa_valid <- fa[!is.na(EFF_DATE) & !is.na(EXP_DATE) & EFF_DATE <= EXP_DATE]
message("Valid contracts for expansion: ", nrow(fa_valid))

# Expand each contract to monthly observations
fa_monthly_contract <- fa_valid[, {
  mo_seq <- seq(
    floor_date(EFF_DATE, "month"),
    floor_date(pmin(EXP_DATE, PANEL_END_DATE), "month"),
    by = "month"
  )
  
  month_starts <- floor_date(mo_seq, "month")
  month_ends   <- ceiling_date(mo_seq, "month") - days(1)
  
  .(
    YEAR      = year(mo_seq),
    MONTH     = month(mo_seq),
    EFF_DATE  = EFF_DATE,
    EXP_DATE  = EXP_DATE,
    start_day = pmax(EFF_DATE, month_starts),
    end_day   = pmin(EXP_DATE, month_ends),
    DETAIL_TYPE,
    CATEGORY,
    ISSUER_NAME,
    COVER_OCC,
    COVER_AGG,
    PREMIUM_PREPAID,
    PROOF_OF_FA,
    FP_CORR_MET,
    TP_FA_MET,
    USE_PRIVATE,
    USE_SELF
  )
}, by = .(FACILITY_ID, FIN_ASSUR_ID)]

message("Contract-month records: ", format(nrow(fa_monthly_contract), big.mark = ","))

# ══════════════════════════════════════════════════════════════════════════════
# SECTION 8: Collapse to Facility-Month Level
# ══════════════════════════════════════════════════════════════════════════════

message("\n", strrep("=", 79))
message("SECTION 8: Collapsing to Facility-Month Level")
message(strrep("=", 79), "\n")

# Collapse using the helper function
fa_monthly <- fa_monthly_contract[, 
  collapse_to_facility_month(.SD), 
  by = .(FACILITY_ID, YEAR, MONTH)
]

message("Facility-month records: ", format(nrow(fa_monthly), big.mark = ","))

# Verify uniqueness
stopifnot(anyDuplicated(fa_monthly[, .(FACILITY_ID, YEAR, MONTH)]) == 0)

# ══════════════════════════════════════════════════════════════════════════════
# SECTION 9: Create Full Panel with Gap Detection
# ══════════════════════════════════════════════════════════════════════════════

message("\n", strrep("=", 79))
message("SECTION 9: Creating Full Panel with Gap Detection")
message(strrep("=", 79), "\n")

# Get all facility IDs
all_facilities <- unique(fa$FACILITY_ID)

# Create complete facility-month skeleton
panel_skeleton <- CJ(
  FACILITY_ID = all_facilities,
  month_date  = seq(floor_date(PANEL_START_DATE, "month"),
                    floor_date(PANEL_END_DATE, "month"),
                    by = "month")
)
panel_skeleton[, `:=`(
  YEAR  = year(month_date),
  MONTH = month(month_date)
)]
panel_skeleton[, month_date := NULL]

message("Panel skeleton: ", format(nrow(panel_skeleton), big.mark = ","), " facility-months")

# Merge with FA data
setkey(panel_skeleton, FACILITY_ID, YEAR, MONTH)
setkey(fa_monthly, FACILITY_ID, YEAR, MONTH)

fa_panel <- merge(panel_skeleton, fa_monthly, 
                  by = c("FACILITY_ID", "YEAR", "MONTH"), 
                  all.x = TRUE)

# ── Calculate ERP (Extended Reporting Period) ────────────────────────────────
# ERP = 6 months after contract expiry where facility has tail coverage
message("\nCalculating ERP periods...")

# Get expired contracts
expired_contracts <- fa_contracts_clean[!is.na(EXP_DATE), .(
  FACILITY_ID,
  FIN_ASSUR_ID,
  EXP_DATE,
  erp_start = floor_date(EXP_DATE + months(1), "month"),
  erp_end   = floor_date(EXP_DATE + months(6), "month")
)]

# Expand to monthly ERP observations
erp_monthly <- expired_contracts[, {
  if (erp_start <= erp_end) {
    erp_months <- seq(erp_start, erp_end, by = "month")
    .(YEAR = year(erp_months), MONTH = month(erp_months), erp_coverage = TRUE)
  }
}, by = .(FACILITY_ID, FIN_ASSUR_ID)]

# Aggregate to facility-month
erp_facility_monthly <- erp_monthly[, .(
  erp_reporting_month = any(erp_coverage, na.rm = TRUE)
), by = .(FACILITY_ID, YEAR, MONTH)]

# Merge ERP
fa_panel <- merge(fa_panel, erp_facility_monthly,
                  by = c("FACILITY_ID", "YEAR", "MONTH"),
                  all.x = TRUE)
fa_panel[is.na(erp_reporting_month), erp_reporting_month := FALSE]

# ── Identify Coverage Gaps ───────────────────────────────────────────────────
fa_panel[is.na(fr_covered), fr_covered := FALSE]

fa_panel[, coverage_gap_month := (!fr_covered) & (!erp_reporting_month)]

# Set sentinel values for gap months
fa_panel[coverage_gap_month == TRUE, `:=`(
  ISSUER_NAME = "NO COVERAGE",
  DETAIL_TYPE = "NO COVERAGE",
  CATEGORY    = "NO COVERAGE"
)]

# ── Pre-1999: State Fund Era ─────────────────────────────────────────────────
# Before 1999, Texas had mandatory State Fund coverage
fa_panel[YEAR < 1999, `:=`(
  ISSUER_NAME = "State Fund",
  DETAIL_TYPE = "State Insurance",
  CATEGORY    = "State Fund",
  fr_covered  = TRUE,
  coverage_gap_month = FALSE
)]

message("\nCoverage gaps: ", fa_panel[coverage_gap_month == TRUE, .N], " facility-months")
message("ERP months: ", fa_panel[erp_reporting_month == TRUE, .N], " facility-months")

# ══════════════════════════════════════════════════════════════════════════════
# SECTION 10: Calculate Change Indicators
# ══════════════════════════════════════════════════════════════════════════════

message("\n", strrep("=", 79))
message("SECTION 10: Calculating Change Indicators")
message(strrep("=", 79), "\n")

setkey(fa_panel, FACILITY_ID, YEAR, MONTH)

# Calculate previous month values
fa_panel[, `:=`(
  prev_issuer       = shift(ISSUER_NAME, type = "lag"),
  prev_category     = shift(CATEGORY, type = "lag"),
  prev_detail_type  = shift(DETAIL_TYPE, type = "lag"),
  prev_coverage_gap = shift(coverage_gap_month, type = "lag")
), by = FACILITY_ID]

# Detect changes
fa_panel[, `:=`(
  issuer_changed = !is.na(prev_issuer) & !is.na(ISSUER_NAME) & ISSUER_NAME != prev_issuer,
  mech_changed   = !is.na(prev_category) & !is.na(CATEGORY) & CATEGORY != prev_category,
  detail_type_changed = !is.na(prev_detail_type) & !is.na(DETAIL_TYPE) & DETAIL_TYPE != prev_detail_type
)]

# Derived indicators
fa_panel[, `:=`(
  contract_changed = issuer_changed | mech_changed | detail_type_changed,
  
  # Transition to self-insurance
  changed_to_self_insure = mech_changed & 
    grepl("Self-Insurance", CATEGORY, ignore.case = TRUE) &
    !is.na(prev_category) &
    !grepl("Self-Insurance", prev_category, ignore.case = TRUE),
  
  # Transition to private insurance
  changed_to_insurance = mech_changed &
    grepl("^Insurance$", CATEGORY, ignore.case = TRUE) &
    !is.na(prev_category) &
    !grepl("^Insurance$", prev_category, ignore.case = TRUE),
  
  # Transition to/from coverage gap
  changed_to_no_coverage   = coverage_gap_month == TRUE & !is.na(prev_coverage_gap) & prev_coverage_gap == FALSE,
  changed_from_no_coverage = coverage_gap_month == FALSE & !is.na(prev_coverage_gap) & prev_coverage_gap == TRUE
)]

# Clean up lag columns
fa_panel[, c("prev_issuer", "prev_category", "prev_detail_type", "prev_coverage_gap") := NULL]

# ── Summary of Changes ───────────────────────────────────────────────────────
message("\nChange indicator summary:")
change_summary <- fa_panel[, .(
  total_facility_months   = .N,
  coverage_gaps           = sum(coverage_gap_month, na.rm = TRUE),
  erp_months              = sum(erp_reporting_month, na.rm = TRUE),
  issuer_changes          = sum(issuer_changed, na.rm = TRUE),
  mechanism_changes       = sum(mech_changed, na.rm = TRUE),
  contract_changes        = sum(contract_changed, na.rm = TRUE),
  transitions_to_self     = sum(changed_to_self_insure, na.rm = TRUE),
  transitions_to_insurance = sum(changed_to_insurance, na.rm = TRUE),
  transitions_to_gaps     = sum(changed_to_no_coverage, na.rm = TRUE),
  transitions_from_gaps   = sum(changed_from_no_coverage, na.rm = TRUE)
)]
print(change_summary)

# ══════════════════════════════════════════════════════════════════════════════
# SECTION 11: Create Facility-Year Panel (December 31 Snapshots)
# ══════════════════════════════════════════════════════════════════════════════

message("\n", strrep("=", 79))
message("SECTION 11: Creating Facility-Year Panel (Dec 31 Snapshots)")
message(strrep("=", 79), "\n")

# December snapshots
fa_year_panel <- fa_panel[MONTH == 12, .(
  FACILITY_ID,
  YEAR,
  
  # Contract characteristics at year-end
  ISSUER_NAME,
  DETAIL_TYPE,
  CATEGORY,
  active_FIN_ASSUR_ID,
  max_COVER_OCC,
  max_COVER_AGG,
  
  # Flags
  fr_covered,
  coverage_gap_month,
  erp_reporting_month,
  uses_private,
  uses_self,
  
  # Annual event counts (aggregate from monthly)
  annual_issuer_changes    = 0L,  # Will compute below
  annual_contract_changes  = 0L
)]

# Calculate annual event counts
annual_events <- fa_panel[, .(
  annual_issuer_changes   = sum(issuer_changed, na.rm = TRUE),
  annual_contract_changes = sum(contract_changed, na.rm = TRUE),
  annual_gap_months       = sum(coverage_gap_month, na.rm = TRUE),
  annual_coverage_months  = sum(fr_covered, na.rm = TRUE)
), by = .(FACILITY_ID, YEAR)]

fa_year_panel <- merge(
  fa_year_panel[, -c("annual_issuer_changes", "annual_contract_changes")],
  annual_events,
  by = c("FACILITY_ID", "YEAR"),
  all.x = TRUE
)

message("Facility-year records: ", format(nrow(fa_year_panel), big.mark = ","))

# ══════════════════════════════════════════════════════════════════════════════
# SECTION 12: Create Zurich 2012 Lookup
# ══════════════════════════════════════════════════════════════════════════════

message("\n", strrep("=", 79))
message("SECTION 12: Creating Zurich 2012 Lookup")
message(strrep("=", 79), "\n")

# Identify Zurich policies
fa_monthly_contract[, is_zurich := grepl(
  "ZURICH|ZURICH AMERICAN",
  toupper(trimws(ISSUER_NAME)),
  ignore.case = TRUE
)]

# Facilities with Zurich in 2012
zurich_2012_lookup <- fa_monthly_contract[
  YEAR == 2012 & is_zurich == TRUE,
  .(had_zurich_2012 = 1L),
  by = FACILITY_ID
]
zurich_2012_lookup <- unique(zurich_2012_lookup, by = "FACILITY_ID")

message("Facilities with Zurich in 2012: ", nrow(zurich_2012_lookup))

# ══════════════════════════════════════════════════════════════════════════════
# SECTION 13: Save Outputs
# ══════════════════════════════════════════════════════════════════════════════

message("\n", strrep("=", 79))
message("SECTION 13: Saving Outputs")
message(strrep("=", 79), "\n")

# 1. Facility-Month Panel
fwrite(fa_panel, file.path(OUTPUT_DIR, "texas_fr_facility_month_panel.csv"))
saveRDS(fa_panel, file.path(OUTPUT_DIR, "texas_fr_facility_month_panel.rds"))
message("Saved: texas_fr_facility_month_panel (.csv, .rds)")

# 2. Facility-Year Panel
fwrite(fa_year_panel, file.path(OUTPUT_DIR, "texas_fr_facility_year_panel.csv"))
saveRDS(fa_year_panel, file.path(OUTPUT_DIR, "texas_fr_facility_year_panel.rds"))
message("Saved: texas_fr_facility_year_panel (.csv, .rds)")

# 3. Clean Contract Inventory
fwrite(fa_contracts_clean, file.path(OUTPUT_DIR, "texas_fr_contracts_clean.csv"))
saveRDS(fa_contracts_clean, file.path(OUTPUT_DIR, "texas_fr_contracts_clean.rds"))
message("Saved: texas_fr_contracts_clean (.csv, .rds)")

# 4. Contract-Month Panel (for detailed analysis)
fwrite(fa_monthly_contract, file.path(OUTPUT_DIR, "texas_fr_contract_month_panel.csv"))
message("Saved: texas_fr_contract_month_panel.csv")

# 5. Zurich 2012 Lookup
fwrite(zurich_2012_lookup, file.path(OUTPUT_DIR, "zurich_2012_lookup.csv"))
message("Saved: zurich_2012_lookup.csv")

# ══════════════════════════════════════════════════════════════════════════════
# SECTION 14: Final Summary
# ══════════════════════════════════════════════════════════════════════════════

message("\n", strrep("=", 79))
message("FINAL SUMMARY")
message(strrep("=", 79))
message("\nOutputs created:")
message("  1. texas_fr_facility_month_panel - ", format(nrow(fa_panel), big.mark = ","), " records")
message("  2. texas_fr_facility_year_panel  - ", format(nrow(fa_year_panel), big.mark = ","), " records")
message("  3. texas_fr_contracts_clean      - ", format(nrow(fa_contracts_clean), big.mark = ","), " contracts")
message("  4. texas_fr_contract_month_panel - ", format(nrow(fa_monthly_contract), big.mark = ","), " records")
message("  5. zurich_2012_lookup            - ", format(nrow(zurich_2012_lookup), big.mark = ","), " facilities")

message("\nKey columns for linkage with inventory (01_clean_texas_inventory.R):")
message("  - FACILITY_ID (character, trimmed)")
message("  - YEAR (integer)")
message("  - MONTH (integer)")

message("\n", strrep("=", 79))
message("02_clean_texas_fr.R COMPLETE")
message(strrep("=", 79), "\n")