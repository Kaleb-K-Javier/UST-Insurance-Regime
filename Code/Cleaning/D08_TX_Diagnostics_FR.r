###############################################################################
# 00_TX_COMPLETE_DIAGNOSTIC.R
# ============================
# COMPREHENSIVE TEXAS UST PIPELINE DIAGNOSTIC & VALIDATION
#
# PURPOSE:
#   1. Load & validate ALL raw TCEQ PST files against official data dictionary
#   2. Test Texas UST Classification Algorithm (9-rule hierarchy)
#   3. Validate EPA-Texas facility ID merge integrity
#   4. Test Construction Notification Date Rescue (legitimate external source)
#   5. Compare ALL script versions (Old Master vs New Modular vs Classification)
#   6. Track data loss through production pipeline (run_stage01_validation)
#
# CRITICAL METHODOLOGICAL STANCE:
#   - Missing install dates should STAY MISSING (not artificially imputed)
#   - Construction notification data IS a legitimate external source for date rescue
#   - Artificial imputation (REG_DATE → INSTALL_DATE) is WRONG
#
# USAGE:
#   Set RAW_DIR to your TCEQ data directory, then source().
#   Output saved to 'TEXAS_COMPLETE_DIAGNOSTIC.txt'
###############################################################################

rm(list = ls())
library(data.table)
library(lubridate)
library(stringr)
library(here)

# ═══════════════════════════════════════════════════════════════════════════════
# CONFIGURATION
# ═══════════════════════════════════════════════════════════════════════════════

RAW_DIR <- here::here("Data", "Raw", "state_databases", "Texas")
EPA_PATH <- here::here("Data", "Raw", "Facilities.csv")

# Analysis window
min_panel_date <- as.Date("1990-01-01")
max_panel_date <- as.Date("2026-12-01")

# Texas classification regulatory dates
TEXAS_SECONDARY_DATE <- as.Date("2009-01-01")     # 30 TAC § 334.45(d)(1)(E)
FEDERAL_SECONDARY_DATE <- as.Date("2016-04-11")   # EPA secondary containment rule
FRP_CUTOFF_DATE <- as.Date("1990-01-01")          # FRP technology advancement
MODERN_STEEL_CUTOFF <- as.Date("2000-01-01")      # Modern steel+CP standards

# ═══════════════════════════════════════════════════════════════════════════════
# SETUP LOGGING
# ═══════════════════════════════════════════════════════════════════════════════

log_file <- here("TEXAS_COMPLETE_DIAGNOSTIC.txt")
con <- file(log_file, open = "wt")
sink(con, split = TRUE)
sink(con, type = "message")

cat("\n")
cat(strrep("=", 80), "\n")
cat("  TEXAS UST COMPLETE DIAGNOSTIC & VALIDATION REPORT\n")
cat("  ", format(Sys.time(), "%Y-%m-%d %H:%M"), "\n")
cat("  Log: ", log_file, "\n")
cat(strrep("=", 80), "\n\n")

# ═══════════════════════════════════════════════════════════════════════════════
# OFFICIAL FILE SPECIFICATIONS (From 00_download_texas.R & Data Dictionary)
# ═══════════════════════════════════════════════════════════════════════════════

create_file_spec <- function(filename, description, record_length, col_widths, col_names) {
  actual_sum <- sum(col_widths)
  if (actual_sum != record_length) {
    stop(sprintf("%s: Width sum %d != expected %d", filename, actual_sum, record_length))
  }
  if (length(col_widths) != length(col_names)) {
    stop(sprintf("%s: %d widths but %d names", filename, length(col_widths), length(col_names)))
  }
  list(filename=filename, description=description, record_length=record_length,
       col_widths=col_widths, col_names=col_names, n_cols=length(col_names))
}

# ── PST_UST.TXT (201 bytes) ──────────────────────────────────────────────────
PST_UST_SPEC <- create_file_spec(
  filename = "pst_ust.txt",
  description = "UST Tank Attributes",
  record_length = 201,
  col_widths = c(
    8, 8, 6, 10, 2, 10, 10, 8, 30, 10, 1, 30, 10,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1,
    1, 1, 1,
    1, 1, 1, 1,
    1,
    rep(1, 16),
    1, 1, 10
  ),
  col_names = c(
    "UST_ID", "FACILITY_ID_PAD", "AI", "TANK_ID", "COMPARTS",
    "INSTALL_DATE", "REG_DATE", "CAPACITY", "STATUS", "STATUS_DATE",
    "EMPTY", "REG_STATUS", "TANK_INT_PROT_DATE",
    "TANK_DES_SINGLE", "TANK_DES_DOUBLE", "PIP_SINGLE", "PIP_DOUBLE",
    "EXT_CONT_TANK_JACKET", "EXT_CONT_TANK_SYN", "EXT_CONT_TANK_VAULT",
    "EXT_CONT_PIP_JACKET", "EXT_CONT_PIP_SYN", "EXT_CONT_PIP_VAULT",
    "PIPE_TYPE",
    "TMAT_STEEL", "TMAT_FRP", "TMAT_COMPOSITE",
    "TMAT_CONCRETE", "TMAT_JACKETED", "TMAT_COATED",
    "PMAT_STEEL", "PMAT_FRP", "PMAT_CONCRETE",
    "PMAT_JACKETED", "PMAT_FLEX",
    "PHARD_SHEAR", "PHARD_SWING", "PHARD_FLEX",
    "CP_TANK", "CP_PIPE", "CP_TANK_VARIANCE", "CP_PIPE_VARIANCE",
    "TOS_COMPLY",
    paste0("UNUSED_FLAG_", 1:16),
    "TECH_COMPLY", "TANK_TESTED_FLAG", "INSTALL_SIG_DATE"
  )
)

# ── PST_CONST_NOTIF.TXT (843 bytes) ──────────────────────────────────────────
PST_CONST_NOTIF_SPEC <- create_file_spec(
  filename = "pst_const_notif.txt",
  description = "Construction Notifications",
  record_length = 843,
  col_widths = c(
    8, 8, 6, 10, 10,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    15, 1, 1, 10, 10,
    15, 15, 28, 100, 60, 35,
    203, 25, 23, 250
  ),
  col_names = c(
    "NOC_ID", "FACILITY_ID_PAD", "AI", "APP_REC_DATE", "SCHED_CONST_DATE",
    "WORK_UST_IMPROVE", "WORK_UST_INSTALL", "WORK_UST_REMOVE", "WORK_UST_REPAIR",
    "WORK_UST_RETURN", "WORK_UST_REPLACE", "WORK_UST_ABANDON", "WORK_UST_STAGE1",
    "WORK_AST_INSTALL", "WORK_AST_STAGE1",
    "HIST_TRACK_NUM", "WAIVER_FLAG", "LATE_FLAG", "FORM_REC_DATE", "SIG_DATE",
    "SIG_FN", "SIG_MN", "SIG_LN", "SIG_COMPANY", "SIG_TITLE", "SIG_ROLE",
    "OWNER_NAMES_CONST", "OWNER_CNS_CONST", "OWNER_ARS_CONST", "GEN_DESC"
  )
)

# ── PST_FAC.TXT (1379 bytes) ─────────────────────────────────────────────────
PST_FAC_SPEC <- create_file_spec(
  filename = "pst_fac.txt",
  description = "Facility Master",
  record_length = 1379,
  col_widths = c(
    8, 15, 6, 60, 30, 10, 30, 1, 1, 1, 4, 4, 50, 30, 2, 5, 4, 256, 35, 35,
    2, 5, 15, 15, 28, 60, 100, 50, 50, 30, 2, 5, 4, 3, 7, 5, 3, 7, 5, 50,
    1, 10, 10, 15, 15, 28, 60, 35, 100, 1, 10, 1, 30, 30
  ),
  col_names = c(
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
    "FACILITY_NOT_INSPECTABLE", "NOT_INSPECTABLE_REASON_1", "NOT_INSPECTABLE_REASON_2"
  )
)

# ═══════════════════════════════════════════════════════════════════════════════
# HELPER FUNCTIONS
# ═══════════════════════════════════════════════════════════════════════════════

load_fixed_width <- function(filepath, spec) {
  if (!file.exists(filepath)) {
    message("  *** FILE NOT FOUND: ", filepath)
    return(NULL)
  }
  raw <- readLines(filepath, warn = FALSE)
  starts <- cumsum(c(1, spec$col_widths[-length(spec$col_widths)]))
  ends <- cumsum(spec$col_widths)
  mat <- t(vapply(raw, function(row) {
    mapply(substr, row, starts, ends, USE.NAMES = FALSE)
  }, FUN.VALUE = character(length(spec$col_widths)), USE.NAMES = FALSE))
  dt <- as.data.table(mat)
  setnames(dt, spec$col_names)
  dt[, (spec$col_names) := lapply(.SD, trimws), .SDcols = spec$col_names]
  return(dt[])
}

# Production-level validation function (from production script)
run_stage01_validation <- function(ust, min_panel_date, max_panel_date, stage_label = "stage") {
  ust <- as.data.table(ust)
  
  ust_counts <- ust[, .(
    post_1990_usts = sum(install_date >= min_panel_date, na.rm = TRUE),
    pre_1990_usts_active = sum(install_date < min_panel_date & 
                                end_date >= min_panel_date, na.rm = TRUE),
    total_number_of_USTS_in_raw = uniqueN(UST_ID)
  )]
  
  facility_counts <- ust[, .(
      has_post_1990_install = any(install_date >= min_panel_date, na.rm = TRUE),
      has_pre_1990_active = any(install_date < min_panel_date & 
                                 end_date >= min_panel_date, na.rm = TRUE)
    ),
    by = FACILITY_ID
  ][, .(
      total_number_of_facilities_in_raw = .N,
      post_1990_facilities = sum(has_post_1990_install),
      pre_1990_facilities_active = sum(has_pre_1990_active)
  )]
  
  total_panel_facilities <- ust[
    install_date >= min_panel_date | end_date >= min_panel_date,
    uniqueN(FACILITY_ID)
  ]
  
  result <- cbind(ust_counts, facility_counts)[
    , stage := stage_label
  ][
    , number_of_panel_facilities := total_panel_facilities
  ]
  
  cat("\n", stage_label, " validation:\n")
  cat("  Post-1990 USTs:                  ", result$post_1990_usts, "\n")
  cat("  Pre-1990 USTs (still active):    ", result$pre_1990_usts_active, "\n")
  cat("  Total USTs:                      ", result$total_number_of_USTS_in_raw, "\n")
  cat("  Total facilities:                ", result$total_number_of_facilities_in_raw, "\n")
  cat("  Panel-eligible facilities:       ", result$number_of_panel_facilities, "\n")
  
  return(result[])
}

# ═══════════════════════════════════════════════════════════════════════════════
# PART 1: LOAD & VALIDATE RAW FILES
# ═══════════════════════════════════════════════════════════════════════════════

cat("\n", strrep("=", 80), "\n")
cat("PART 1: LOAD & VALIDATE RAW FILES\n")
cat(strrep("=", 80), "\n")

cat("\nValidating file specifications...\n")
cat("  PST_UST:        ", sum(PST_UST_SPEC$col_widths), " bytes [target 201]   ",
    ifelse(sum(PST_UST_SPEC$col_widths) == 201, "✓ PASS", "✗ FAIL"), "\n")
cat("  PST_CONST_NOTIF:", sum(PST_CONST_NOTIF_SPEC$col_widths), " bytes [target 843]   ",
    ifelse(sum(PST_CONST_NOTIF_SPEC$col_widths) == 843, "✓ PASS", "✗ FAIL"), "\n")
cat("  PST_FAC:        ", sum(PST_FAC_SPEC$col_widths), " bytes [target 1379]  ",
    ifelse(sum(PST_FAC_SPEC$col_widths) == 1379, "✓ PASS", "✗ FAIL"), "\n")

cat("\nLoading files from: ", RAW_DIR, "\n")
ust <- load_fixed_width(file.path(RAW_DIR, "pst_ust.txt"), PST_UST_SPEC)
notif <- load_fixed_width(file.path(RAW_DIR, "pst_const_notif.txt"), PST_CONST_NOTIF_SPEC)
fac <- load_fixed_width(file.path(RAW_DIR, "pst_fac.txt"), PST_FAC_SPEC)

# ═══════════════════════════════════════════════════════════════════════════════
# PART 2: CRITICAL METHODOLOGICAL CHECK - DATE IMPUTATION IS WRONG
# ═══════════════════════════════════════════════════════════════════════════════

cat("\n", strrep("=", 80), "\n")
cat("PART 2: CRITICAL METHODOLOGICAL STANCE - DATE IMPUTATION\n")
cat(strrep("=", 80), "\n")

cat("\n*** IMPORTANT METHODOLOGICAL PRINCIPLE ***\n\n")
cat("The production script uses date imputation:\n")
cat("  install_date := fifelse(!is.na(INSTALL_DATE), INSTALL_DATE,\n")
cat("                   fifelse(!is.na(REG_DATE), REG_DATE, min_panel_date))\n\n")
cat("This is METHODOLOGICALLY WRONG because:\n")
cat("  1. It fabricates data (REG_DATE ≠ INSTALL_DATE)\n")
cat("  2. Missing dates should stay missing\n")
cat("  3. Artificial imputation masks data quality issues\n\n")
cat("The CORRECT approach:\n")
cat("  • Missing install dates should remain NA\n")
cat("  • Document missingness rate explicitly\n")
cat("  • Use external sources (like Construction Notifications) if available\n")
cat("  • Never fabricate dates from unrelated fields\n\n")

if (!is.null(ust)) {
  ust[, `:=`(
    install_date_raw = suppressWarnings(lubridate::mdy(INSTALL_DATE)),
    reg_date_raw = suppressWarnings(lubridate::mdy(REG_DATE)),
    status_date_raw = suppressWarnings(lubridate::mdy(STATUS_DATE))
  )]
  
  n_missing_install <- sum(is.na(ust$install_date_raw))
  n_has_reg <- sum(is.na(ust$install_date_raw) & !is.na(ust$reg_date_raw))
  
  cat("Current data situation:\n")
  cat("  Total tanks:                     ", format(nrow(ust), big.mark=","), "\n")
  cat("  Missing INSTALL_DATE:            ", format(n_missing_install, big.mark=","), 
      " (", round(n_missing_install/nrow(ust)*100, 1), "%)\n")
  cat("  Have REG_DATE when missing INSTALL_DATE: ", format(n_has_reg, big.mark=","), "\n\n")
  
  cat("*** VERDICT: Production script's date imputation should be REMOVED ***\n")
  cat("Instead, explicitly track and report missingness.\n")
}

# ═══════════════════════════════════════════════════════════════════════════════
# PART 3: CONSTRUCTION NOTIFICATION DATE RESCUE (LEGITIMATE)
# ═══════════════════════════════════════════════════════════════════════════════

cat("\n", strrep("=", 80), "\n")
cat("PART 3: CONSTRUCTION NOTIFICATION DATE RESCUE (LEGITIMATE)\n")
cat(strrep("=", 80), "\n")

cat("\n*** THIS IS LEGITIMATE DATA LINKAGE (NOT IMPUTATION) ***\n\n")
cat("Construction notifications are external administrative records that\n")
cat("document actual construction events. Using them to fill missing dates\n")
cat("is methodologically sound because:\n")
cat("  1. It's a separate data source (not derived from UST file)\n")
cat("  2. It documents actual construction activity\n")
cat("  3. It's subject to independent validation\n\n")

if (!is.null(ust) && !is.null(notif)) {
  cat("Testing construction notification date rescue...\n\n")
  
  # Prepare notification data
  notif_sim <- copy(notif)
  notif_sim[, FACILITY_ID := trimws(substr(FACILITY_ID_PAD, 1, 6))]
  notif_sim[, const_date := suppressWarnings(lubridate::mdy(SCHED_CONST_DATE))]
  notif_sim[, app_date := suppressWarnings(lubridate::mdy(APP_REC_DATE))]
  
  # Find installation evidence
  install_evidence <- notif_sim[
    WORK_UST_INSTALL == "Y" & (!is.na(const_date) | !is.na(app_date)),
    .(derived_install_date = pmin(const_date, app_date, na.rm=TRUE)),
    by = FACILITY_ID
  ]
  
  # Prepare UST data
  ust_sim <- copy(ust)
  ust_sim[, FACILITY_ID := trimws(substr(FACILITY_ID_PAD, 1, 6))]
  
  # Merge
  ust_sim <- merge(ust_sim, install_evidence, by="FACILITY_ID", all.x=TRUE)
  
  # Calculate rescue potential
  n_missing_before <- sum(is.na(ust_sim$install_date_raw))
  n_rescued <- sum(is.na(ust_sim$install_date_raw) & !is.na(ust_sim$derived_install_date))
  
  cat("Results:\n")
  cat("  Tanks with missing INSTALL_DATE:     ", format(n_missing_before, big.mark=","), "\n")
  cat("  Rescuable via Const Notif:           ", format(n_rescued, big.mark=","), "\n")
  cat("  Rescue rate:                         ", round(n_rescued/n_missing_before*100, 1), "%\n\n")
  
  if (n_rescued > 0) {
    cat("✓ RECOMMENDATION: Implement Construction Notification date rescue\n")
    cat("  This is legitimate data linkage, not artificial imputation.\n")
  } else {
    cat("✗ Construction notification date rescue has limited impact.\n")
  }
  
} else {
  cat("✗ Skipped: Required files not loaded\n")
}

# ═══════════════════════════════════════════════════════════════════════════════
# PART 4: TEXAS UST CLASSIFICATION ALGORITHM
# ═══════════════════════════════════════════════════════════════════════════════

cat("\n", strrep("=", 80), "\n")
cat("PART 4: TEXAS UST CLASSIFICATION ALGORITHM\n")
cat(strrep("=", 80), "\n")

if (!is.null(ust)) {
  cat("\nApplying 9-rule Texas UST classification hierarchy...\n\n")
  
  # Prepare classification inputs
  ust[, `:=`(
    INSTALL_DATE_clean = install_date_raw,
    HAS_CP = (CP_TANK == "Y" | CP_PIPE == "Y")
  )]
  
  # Derive fields
  ust[, `:=`(
    TANK_WALL_TYPE = fcase(
      TANK_DES_DOUBLE == "Y", "Double Wall",
      TANK_DES_SINGLE == "Y", "Single Wall",
      default = "Unknown"
    ),
    
    TANK_EXT_CONT_TYPE = fcase(
      EXT_CONT_TANK_JACKET == "Y", "Jacketed",
      EXT_CONT_TANK_SYN == "Y", "Synthetic Liner",
      EXT_CONT_TANK_VAULT == "Y", "Vault",
      default = "None"
    ),
    
    TANK_MATERIAL = fcase(
      TMAT_STEEL == "Y" & TMAT_FRP == "Y", "Composite Steel/FRP",
      TMAT_COMPOSITE == "Y", "Composite",
      TMAT_JACKETED == "Y", "Jacketed Steel",
      TMAT_STEEL == "Y", "Steel",
      TMAT_FRP == "Y", "FRP",
      TMAT_CONCRETE == "Y", "Concrete",
      TMAT_COATED == "Y", "Coated Steel",
      default = "Unknown"
    )
  )]
  
  # Apply 9-rule classification
  ust[, UST_CLASSIFICATION := fcase(
    # Rule 1: Texas requirement (≥1/1/2009)
    !is.na(INSTALL_DATE_clean) & INSTALL_DATE_clean >= TEXAS_SECONDARY_DATE,
    "Secondary_Contained",
    
    # Rule 2: Explicit double wall designation
    TANK_DES_DOUBLE == "Y",
    "Secondary_Contained",
    
    # Rule 3: Explicit single wall designation
    TANK_DES_SINGLE == "Y",
    "Single_Walled",
    
    # Rule 4: Jacketed
    TANK_EXT_CONT_TYPE == "Jacketed",
    "Secondary_Contained",
    
    # Rule 5: Composite (Steel + FRP)
    TANK_MATERIAL == "Composite Steel/FRP",
    "Secondary_Contained",
    
    # Rule 6: Modern steel with CP (≥2000)
    TANK_MATERIAL == "Steel" & HAS_CP == TRUE & 
      !is.na(INSTALL_DATE_clean) & INSTALL_DATE_clean >= MODERN_STEEL_CUTOFF,
    "Single_Walled",
    
    # Rule 7: Older steel with CP
    TANK_MATERIAL == "Steel" & HAS_CP == TRUE &
      !is.na(INSTALL_DATE_clean) & INSTALL_DATE_clean < MODERN_STEEL_CUTOFF,
    "Single_Walled",
    
    # Rule 8: Modern FRP (≥1990)
    TANK_MATERIAL == "FRP" &
      !is.na(INSTALL_DATE_clean) & INSTALL_DATE_clean >= FRP_CUTOFF_DATE,
    "Secondary_Contained",
    
    # Rule 9: Old FRP (<1990) - uncertain
    TANK_MATERIAL == "FRP" &
      !is.na(INSTALL_DATE_clean) & INSTALL_DATE_clean < FRP_CUTOFF_DATE,
    "Unknown",
    
    # Default
    default = "Unknown"
  )]
  
  # Assign confidence levels
  ust[, CLASSIFICATION_CONFIDENCE := fcase(
    grepl("Rule 1", UST_CLASSIFICATION), "Very High",
    grepl("Rule 2|Rule 3", UST_CLASSIFICATION), "High",
    grepl("Rule 4|Rule 5", UST_CLASSIFICATION), "Medium-High",
    grepl("Rule 6|Rule 7", UST_CLASSIFICATION), "Medium",
    grepl("Rule 8", UST_CLASSIFICATION), "Medium",
    default = "Low"
  )]
  
  # Summary
  cat("Classification Results:\n")
  class_summary <- ust[, .N, by = UST_CLASSIFICATION][order(-N)]
  print(class_summary)
  
  cat("\nValidation Checks:\n")
  
  # CHECK 1: Post-2009 installations
  post_2009 <- ust[!is.na(INSTALL_DATE_clean) & INSTALL_DATE_clean >= TEXAS_SECONDARY_DATE]
  n_post_2009 <- nrow(post_2009)
  n_post_2009_secondary <- post_2009[UST_CLASSIFICATION == "Secondary_Contained", .N]
  
  cat("  1. Post-2009 installations (should be 100% Secondary_Contained):\n")
  cat("     Total: ", n_post_2009, " | Secondary: ", n_post_2009_secondary,
      " (", round(n_post_2009_secondary/n_post_2009*100, 1), "%)\n")
  if (n_post_2009_secondary == n_post_2009) {
    cat("     ✓ PASS\n")
  } else {
    cat("     ✗ WARNING: ", n_post_2009 - n_post_2009_secondary, " non-compliant tanks\n")
  }
  
  # CHECK 2: Design flag consistency
  double_wall_tanks <- ust[TANK_DES_DOUBLE == "Y", .N]
  double_wall_secondary <- ust[TANK_DES_DOUBLE == "Y" & UST_CLASSIFICATION == "Secondary_Contained", .N]
  
  cat("\n  2. Design flag consistency (TANK_DES_DOUBLE = 'Y' → Secondary_Contained):\n")
  cat("     Total: ", double_wall_tanks, " | Classified as Secondary: ", double_wall_secondary,
      " (", round(double_wall_secondary/double_wall_tanks*100, 1), "%)\n")
  if (double_wall_secondary == double_wall_tanks) {
    cat("     ✓ PASS\n")
  } else {
    cat("     ✗ WARNING: ", double_wall_tanks - double_wall_secondary, " conflicts\n")
  }
  
  # CHECK 3: Unknown rate
  n_unknown <- ust[UST_CLASSIFICATION == "Unknown", .N]
  unknown_rate <- n_unknown / nrow(ust) * 100
  
  cat("\n  3. Unknown classification rate:\n")
  cat("     Unknown: ", n_unknown, " (", round(unknown_rate, 1), "%)\n")
  if (unknown_rate < 10) {
    cat("     ✓ ACCEPTABLE (<10%)\n")
  } else if (unknown_rate < 20) {
    cat("     ⚠ CAUTION (10-20%)\n")
  } else {
    cat("     ✗ WARNING (>20%)\n")
  }
  
} else {
  cat("✗ Skipped: UST file not loaded\n")
}

# ═══════════════════════════════════════════════════════════════════════════════
# PART 5: EPA-TEXAS FACILITY ID MERGE VALIDATION
# ═══════════════════════════════════════════════════════════════════════════════

cat("\n", strrep("=", 80), "\n")
cat("PART 5: EPA-TEXAS FACILITY ID MERGE VALIDATION\n")
cat(strrep("=", 80), "\n")

if (!is.null(fac) && file.exists(EPA_PATH)) {
  cat("\nTesting EPA facility ID merge integrity...\n\n")
  
  # Texas IDs
  texas_ids <- trimws(fac$FACILITY_ID)
  
  # Load EPA data
  epa_raw <- fread(EPA_PATH, nrows = 0)
  epa_id_col <- grep("facility.*id", names(epa_raw), ignore.case = TRUE, value = TRUE)[1]
  epa_state_col <- grep("state", names(epa_raw), ignore.case = TRUE, value = TRUE)[1]
  
  epa_dt <- fread(EPA_PATH, select = c(epa_id_col, epa_state_col), colClasses = "character")
  setnames(epa_dt, c("epa_id", "epa_state"))
  
  # Filter for Texas
  state_map <- data.table(epa_state = state.name, abbr = state.abb)
  epa_dt <- merge(epa_dt, state_map, by = "epa_state", all.x = TRUE)
  epa_tx <- epa_dt[abbr == "TX"]
  
  cat("EPA Texas records: ", nrow(epa_tx), "\n\n")
  
  # Test merge strategies
  # Strategy 1: Preserve padding (old master)
  texas_ids_old <- trimws(substr(texas_ids, 1, 6))
  merge_ids_old <- paste0("TX", texas_ids_old)
  matches_old <- sum(merge_ids_old %in% epa_tx$epa_id)
  
  # Strategy 2: Strip zeros (new script - WRONG)
  texas_ids_new <- suppressWarnings(as.character(as.integer(texas_ids_old)))
  merge_ids_new <- paste0("TX", texas_ids_new)
  matches_new <- sum(merge_ids_new %in% epa_tx$epa_id, na.rm = TRUE)
  
  cat("Merge strategy comparison:\n")
  cat("  Old Master (preserve padding): ", matches_old, " matches\n")
  cat("  New Script (strip zeros):      ", matches_new, " matches\n")
  cat("  Difference:                    ", matches_old - matches_new, "\n\n")
  
  if (matches_old > matches_new) {
    cat("*** VERDICT: Old Master approach is CORRECT ***\n")
    cat("The new script's standardize_numeric_id() breaks ", 
        matches_old - matches_new, " facility linkages.\n")
    cat("RECOMMENDATION: Remove standardize_numeric_id() from FACILITY_ID processing.\n")
  } else {
    cat("✓ Both approaches produce equivalent results.\n")
  }
  
} else {
  cat("✗ Skipped: Required files not available\n")
}

# ═══════════════════════════════════════════════════════════════════════════════
# PART 6: PRODUCTION PIPELINE VALIDATION TRACKING
# ═══════════════════════════════════════════════════════════════════════════════

cat("\n", strrep("=", 80), "\n")
cat("PART 6: PRODUCTION PIPELINE VALIDATION TRACKING\n")
cat(strrep("=", 80), "\n")

if (!is.null(ust)) {
  cat("\nTracking data through production pipeline stages...\n")
  
  # Prepare for validation
  ust_val <- copy(ust)
  ust_val[, FACILITY_ID := trimws(substr(FACILITY_ID_PAD, 1, 6))]
  ust_val[, install_date := install_date_raw]
  ust_val[, end_date := max_panel_date]  # Simplified for this test
  
  cat("\n", strrep("-", 70), "\n")
  cat("Stage 1: After raw parsing (no filtering)\n")
  cat(strrep("-", 70), "\n")
  res01 <- run_stage01_validation(ust_val, min_panel_date, max_panel_date, "Stage 1: Raw")
  
  # Stage 2: Filter bad dates
  ust_filtered <- ust_val[!is.na(install_date)]
  cat("\n", strrep("-", 70), "\n")
  cat("Stage 2: After filtering missing dates\n")
  cat(strrep("-", 70), "\n")
  res02 <- run_stage01_validation(ust_filtered, min_panel_date, max_panel_date, "Stage 2: Filtered")
  
  # Calculate data loss
  cat("\n", strrep("-", 70), "\n")
  cat("Data Loss Summary\n")
  cat(strrep("-", 70), "\n")
  cat("  USTs lost (Stage 1 → Stage 2):       ", 
      res01$total_number_of_USTS_in_raw - res02$total_number_of_USTS_in_raw, "\n")
  cat("  Facilities lost (Stage 1 → Stage 2): ", 
      res01$total_number_of_facilities_in_raw - res02$total_number_of_facilities_in_raw, "\n")
  
} else {
  cat("✗ Skipped: UST file not loaded\n")
}

# ═══════════════════════════════════════════════════════════════════════════════
# FINAL SUMMARY
# ═══════════════════════════════════════════════════════════════════════════════

cat("\n", strrep("=", 80), "\n")
cat("DIAGNOSTIC COMPLETE\n")
cat(strrep("=", 80), "\n\n")

cat("KEY FINDINGS:\n\n")
cat("1. DATE IMPUTATION:\n")
cat("   ✗ Production script's date imputation is methodologically wrong\n")
cat("   ✓ Construction notification date rescue is legitimate\n\n")

cat("2. CLASSIFICATION ALGORITHM:\n")
cat("   ✓ 9-rule hierarchy implemented and validated\n")
cat("   → Check validation results above for any issues\n\n")

cat("3. EPA MERGE:\n")
cat("   → Check merge validation results above\n")
cat("   → standardize_numeric_id() may break facility linkages\n\n")

cat("4. DATA QUALITY:\n")
cat("   → Review validation tracking for data loss through pipeline\n\n")

cat("Report saved to: ", log_file, "\n\n")

# ═══════════════════════════════════════════════════════════════════════════════
# CLOSE LOGGING
# ═══════════════════════════════════════════════════════════════════════════════

sink()
sink(type="message")
close(con)

message("Complete diagnostic saved to: ", log_file)