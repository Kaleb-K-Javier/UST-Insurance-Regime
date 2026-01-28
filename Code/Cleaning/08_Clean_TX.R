###############################################################################
# tx_fr_panel.R   
###############################################################################


# ── Section 0: Setup ──────────────────────────────────────────────────────────
# Purpose: load core libraries, threads & I/O helpers, path utility, and ID-cleaning functions.
# Why: ensures consistent environment, parallelism & reliable file paths across server/local.
# End product: `clean_id_column()`, `get_data_path()`, and global options ready for subsequent sections.
rm(list = ls())
library(data.table)
library(lubridate)
library(maps)
library(stringr)
library(parallel) 
library(tidycensus) 
library(zoo) 
library(tidyverse)
library(here) 

# ── Path Configuration ───────────────────────────────────────────────────────

# 1. Output for Harmonized files (Where final UST/LUST CSVs go)
tx_output_dir <- here("Data", "Raw", "state_databases", "Texas")
if (!dir.exists(tx_output_dir)) dir.create(tx_output_dir, recursive = TRUE)

# 2. Staging Directories for Panel Merge (Where intermediate .rds/.csv files go)
# We place these in Data/Intermediate/Texas to keep the Raw folder clean
staging_root <- here("Data", "Intermediate", "Texas")

stage_dir  <- file.path(staging_root, "panel_merge_staging")
lust_dir   <- file.path(staging_root, "lust_merge_staging")
cens_dir   <- file.path(staging_root, "census_merge_staging")
output_figures <- here("Output", "Figures") # General outputs (repo uses top-level Output/)

# Create directories
dir.create(stage_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(lust_dir,  recursive = TRUE, showWarnings = FALSE)
dir.create(cens_dir,  recursive = TRUE, showWarnings = FALSE)
dir.create(output_figures, recursive = TRUE, showWarnings = FALSE)

# ── Helper for Legacy Calls ──────────────────────────────────────────────────
# This ensures existing calls like get_data_path("Outputs") still work 
# but point to the correct project root location
get_data_path <- function(...) {
  parts <- c(...)
  if (length(parts) == 0) return(here("Data"))

  # Normalize first path element so legacy code works:
  # - get_data_path("Raw", ...)       -> Data/Raw/...
  # - get_data_path("Processed", ...) -> Data/Processed/...
  # - get_data_path("Output(s)", ...) -> Output/...
  root <- as.character(parts[[1]])
  rest <- if (length(parts) > 1) parts[-1] else character()

  if (root %in% c("Output", "Outputs")) return(here("Output", rest))
  if (root == "Data")                  return(here(parts))

  # Default: everything else lives under Data/
  here("Data", parts)
}

# ── Server Configuration ─────────────────────────────────────────────────────
do_merge   <- FALSE          # set FALSE if you only want the staging artefacts
onserver   <- TRUE          # your existing flag

# Set data.table threads based on environment
if (onserver) {
  setDTthreads(detectCores() - 2)  # Use available cores minus 2
} else {
  setDTthreads(4)  # Use 4 threads for local development
}
message("data.table is using ", getDTthreads(), " threads.")

# ── Helper Functions ─────────────────────────────────────────────────────────

# Helper to ensure consistent ID cleaning
clean_id_column <- function(dt, col_name) {
  if (col_name %in% names(dt)) {
    dt[, (col_name) := trimws(as.character(get(col_name)))]
  }
  dt
}

# Helper for fast writing
quick_write <- function(dt, dir_path, nm) {
  fwrite(dt, file.path(dir_path, paste0(nm, ".csv")))     
  saveRDS(dt, file.path(dir_path, paste0(nm, ".rds")))     
  invisible(NULL)
}


# ── Fetch and Save ACS County-Level Time Series Data ─────────────────────────
# This entire section is removed as per the request to remove old ACS fetching
# and urban/rural variables. The new census data handling is done in Section 8.
# ───────────────────────────────────────────────────────────────────────────────

# Create output directories
dir.create(get_data_path("Output"), showWarnings = FALSE, recursive = TRUE)
dir.create(get_data_path("Output", "Figures"), showWarnings = FALSE, recursive = TRUE)
dir.create(get_data_path("Processed"), showWarnings = FALSE, recursive = TRUE)



# ── helper: temp-download + fixed-width splitter ─────────────────────────────
load_and_process_data <- function(url, col_widths, col_names, timeout = 600) {
  tf <- tempfile(); on.exit(unlink(tf))
  old_t <- getOption("timeout"); on.exit(options(timeout = old_t), add = TRUE)
  options(timeout = timeout)
  message("⇣  downloading ", basename(url))
  download.file(url, tf, method = "libcurl", quiet = TRUE)

  raw <- readLines(tf, warn = FALSE)

  split_row <- function(row, w) {
    starts <- cumsum(c(1, w[-length(w)]))
    ends   <- cumsum(w)
    mapply(substr, row, starts, ends, USE.NAMES = FALSE)
  }
  mat <- t(vapply(raw, split_row, FUN.VALUE = character(length(col_widths)),
                  w = col_widths))
  dt  <- as.data.table(mat); setnames(dt, col_names)
  # Trim whitespace from all columns (as they are all character at this point)
  dt[, (col_names) := lapply(.SD, trimws), .SDcols = col_names]
  message("✓  processed ", basename(url))
  dt[]
}

# Function to classify substances (user-provided, adapted for data.table)
classify_substances <- function(data) {
  # Create binary classification columns with default values
  result <- copy(data) # Ensure we work on a copy
  result[, `:=`(
    is_gasoline = 0,
    is_diesel = 0,
    is_oil_kerosene = 0,
    is_jet_fuel = 0,
    is_other = 1  # Default to 1, will set to 0 if any other category matches
  )]
  
  # Classification patterns
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
  
  # Convert substances to lowercase
  result[, substances_lower := tolower(substances)]
  
  # Apply classification patterns using vectorized operations
  result[!is.na(substances_lower) & substances_lower != "", 
         is_gasoline := as.integer(grepl(gasoline_patterns, substances_lower, perl = TRUE))]
  
  # Exclude DEF from diesel classification
  result[!is.na(substances_lower) & substances_lower != "" & 
         !grepl("def|diesel exhaust fluid", substances_lower, perl = TRUE), 
         is_diesel := as.integer(grepl(diesel_patterns, substances_lower, perl = TRUE))]
  
  result[!is.na(substances_lower) & substances_lower != "", 
         is_oil_kerosene := as.integer(grepl(oil_kerosene_patterns, substances_lower, perl = TRUE))]
  
  result[!is.na(substances_lower) & substances_lower != "", 
         is_jet_fuel := as.integer(grepl(jet_fuel_patterns, substances_lower, perl = TRUE))]
  
  # Set "other" to 0 if any other category matched
  result[is_gasoline == 1 | is_diesel == 1 | is_oil_kerosene == 1 | is_jet_fuel == 1, is_other := 0]
  
  # Remove temporary column
  result[, substances_lower := NULL]
  
  # Create a summary table of the classifications
  message("\nSubstance classification summary for processed tanks:")
  print(result[, .(
    total_records = .N,
    gasoline_count = sum(is_gasoline),
    diesel_count = sum(is_diesel),
    oil_kerosene_count = sum(is_oil_kerosene),
    jet_fuel_count = sum(is_jet_fuel),
    other_count = sum(is_other),
    multi_category_count = sum(is_gasoline + is_diesel + is_oil_kerosene + is_jet_fuel > 1)
  )])
  
  return(result)
}

clean_merge_keys <- function(dt, key_cols, to_upper = TRUE) {
  for (k in key_cols) if (k %in% names(dt)) {
    dt[[k]] <- trimws(as.character(dt[[k]]))
    if (to_upper) dt[[k]] <- toupper(dt[[k]])
  }
  dt
}

base <- "https://www.tceq.texas.gov/assets/public/admin/data/docs"

# ──────────────────────────────────────────────────────────────────────────────
# OPTIMIZED MONTHLY EXPANSION using Rolling Joins
# ──────────────────────────────────────────────────────────────────────────────
# Purpose: Expand contracts to monthly observations efficiently
# Method: Use data.table's rolling joins instead of individual contract expansion
# Note: Gap detection happens later when FA data is merged into the complete panel
# ──────────────────────────────────────────────────────────────────────────────

optimize_monthly_expansion <- function(fa_contracts) {
  
  # Step 1: Create master monthly grid for all relevant periods
  min_eff <- fa_contracts[!is.na(EFF_DATE), min(EFF_DATE)]
  max_exp <- fa_contracts[!is.na(EXP_DATE), max(EXP_DATE)]
  max_exp <- pmin(max_exp, as.Date("2026-12-01"))
  
  monthly_grid <- data.table(
    month_date = seq(
      floor_date(min_eff, "month"),
      floor_date(max_exp, "month"),
      by = "month"
    )
  )
  monthly_grid[, `:=`(
    YEAR = year(month_date),
    MONTH = month(month_date)
  )]
  
  # Step 2: Prepare contracts for rolling join
  fa_clean <- fa_contracts[
    !is.na(EFF_DATE) & !is.na(EXP_DATE) & EFF_DATE <= EXP_DATE
  ][, `:=`(
    month_start = floor_date(EFF_DATE, "month"),
    month_end = floor_date(pmin(EXP_DATE, as.Date("2026-12-01")), "month")
  )]
  
  # Step 3: Cross join each contract with relevant months using data.table efficiency
  # This replaces the individual seq() calls with a more efficient approach
  fa_expanded <- fa_clean[monthly_grid, 
    on = .(month_start <= month_date, month_end >= month_date),
    nomatch = 0L,  # Only keep matches
    .(
      # Contract identifiers
      x.FACILITY_ID, x.FIN_ASSUR_ID,
      
      # Time identifiers  
      i.YEAR, i.MONTH, i.month_date,
      
      # Original contract dates
      x.EFF_DATE, x.EXP_DATE,
      
      # Calculate day-level edges for this month
      start_day_in_month = pmax(x.EFF_DATE, floor_date(i.month_date, "month")),
      end_day_in_month = pmin(x.EXP_DATE, ceiling_date(i.month_date, "month") - 1),
      
      # Contract attributes
      x.DETAIL_TYPE, x.CATEGORY,
      x.COVER_OCC, x.COVER_AGG,
      x.ISSUER_NAME,
      x.PREMIUM_PREPAID, x.PROOF_OF_FA,
      x.FP_CORR_MET, x.TP_FA_MET,
      x.USE_PRIVATE_mapped, x.USE_SELF_mapped
    )
  ]
  
  # Step 4: Clean column names
  setnames(fa_expanded, 
           grep("^x\\.", names(fa_expanded), value = TRUE),
           gsub("^x\\.", "", grep("^x\\.", names(fa_expanded), value = TRUE)))
  setnames(fa_expanded,
           grep("^i\\.", names(fa_expanded), value = TRUE), 
           gsub("^i\\.", "", grep("^i\\.", names(fa_expanded), value = TRUE)))
  
  # Step 5: Add derived columns - all expanded months have coverage by definition
  fa_expanded[, `:=`(
    premium_prepaid = PREMIUM_PREPAID,
    proof_on_file = PROOF_OF_FA,
    fp_corr_met = FP_CORR_MET,
    tp_fa_met = TP_FA_MET,
    uses_private = USE_PRIVATE_mapped,
    uses_self = USE_SELF_mapped,
    fr_covered = TRUE  # All expanded contract months have coverage
  )]
  
  # Step 6: Calculate ERP periods for each contract
  # Note: This marks months AFTER contract expiry that still have ERP coverage
  fa_expanded[, erp_reporting_month := 
    between(
      make_date(YEAR, MONTH, 1),
      EXP_DATE + months(1),
      EXP_DATE + months(6)
    ),
    by = .(FACILITY_ID, FIN_ASSUR_ID)]
  
  # Clean up temporary columns
  fa_expanded[, month_date := NULL]
  
  return(fa_expanded)
  
} # end of optimize_monthly_expansion function


# ──────────────────────────────────────────────────────────────────────────────
# EXTRACTED FUNCTION: collapse_logic() - UPDATED
# ──────────────────────────────────────────────────────────────────────────────
# Purpose: Collapse contract-level rows to one facility-month row with aggregates
# Input: dt = all contracts for ONE facility in ONE year-month (data.table)
# Output: single-row data.table with aggregated flags + coverage amounts
# Note: Gap detection happens AFTER merging into complete panel, not here
# ──────────────────────────────────────────────────────────────────────────────

collapse_logic <- function(dt) {
  dt <- data.table::copy(dt)        # unlock .SD
  n  <- nrow(dt)                    # how many contracts this month?

  ## ---- group-level aggregates used in every branch -------------------------
  agg <- list(
    DETAIL_TYPE         = paste(sort(unique(dt$DETAIL_TYPE)), collapse = "; "),
    CATEGORY            = paste(sort(unique(dt$CATEGORY)),    collapse = "; "),
    ISSUER_NAME         = paste(sort(unique(dt$ISSUER_NAME)), collapse = " | "),
    max_COVER_OCC       = if (all(is.na(dt$COVER_OCC))) NA_real_
                          else max(dt$COVER_OCC, na.rm = TRUE),
    max_COVER_AGG       = if (all(is.na(dt$COVER_AGG))) NA_real_
                          else max(dt$COVER_AGG, na.rm = TRUE),
    total_COVER_OCC     = sum(dt$COVER_OCC, na.rm = TRUE),
    total_COVER_AGG     = sum(dt$COVER_AGG, na.rm = TRUE),
    premium_prepaid     = any(dt$premium_prepaid, na.rm = TRUE),
    proof_on_file       = any(dt$proof_on_file,   na.rm = TRUE),
    fp_corr_met         = any(dt$fp_corr_met,     na.rm = TRUE),
    tp_fa_met           = any(dt$tp_fa_met,       na.rm = TRUE),
    uses_private        = any(dt$uses_private,    na.rm = TRUE),
    uses_self           = any(dt$uses_self,       na.rm = TRUE)
    # REMOVED: coverage_gap_month - this is determined at panel level
  )

  # Branch 1: single contract
  if (n == 1L) {
    row <- dt[1L]
    row[, `:=`(
      transition_month    = FALSE,
      multiple_contracts  = FALSE,
      active_FIN_ASSUR_ID = FIN_ASSUR_ID,
      fr_covered          = TRUE,  # All expanded contract months have coverage by definition
      DETAIL_TYPE         = agg$DETAIL_TYPE,
      CATEGORY            = agg$CATEGORY,
      ISSUER_NAME         = agg$ISSUER_NAME,
      max_COVER_OCC       = agg$max_COVER_OCC,
      max_COVER_AGG       = agg$max_COVER_AGG,
      total_COVER_OCC     = agg$total_COVER_OCC,
      total_COVER_AGG     = agg$total_COVER_AGG,
      premium_prepaid     = agg$premium_prepaid,
      proof_on_file       = agg$proof_on_file,
      fp_corr_met         = agg$fp_corr_met,
      tp_fa_met           = agg$tp_fa_met,
      uses_private        = agg$uses_private,
      uses_self           = agg$uses_self
    )]
    return(row[])
  }

  # Branch 2: exactly two contracts, NO overlap (clean hand-off)
  ord  <- order(dt$start_day_in_month)
  sNum <- as.numeric(dt$start_day_in_month[ord])
  eNum <- as.numeric(dt$end_day_in_month[ord])
  overlaps <- any(na.omit(sNum[-1] <= cummax(eNum)[-n]))

  if (n == 2L && !overlaps) {
    idx <- which.max(dt$start_day_in_month)
    row <- dt[idx]
    row[, `:=`(
      transition_month    = TRUE,
      multiple_contracts  = FALSE,
      active_FIN_ASSUR_ID = FIN_ASSUR_ID,
      fr_covered          = TRUE,  # All expanded contract months have coverage by definition
      DETAIL_TYPE         = agg$DETAIL_TYPE,
      CATEGORY            = agg$CATEGORY,
      ISSUER_NAME         = agg$ISSUER_NAME,
      max_COVER_OCC       = agg$max_COVER_OCC,
      max_COVER_AGG       = agg$max_COVER_AGG,
      total_COVER_OCC     = agg$total_COVER_OCC,
      total_COVER_AGG     = agg$total_COVER_AGG,
      premium_prepaid     = agg$premium_prepaid,
      proof_on_file       = agg$proof_on_file,
      fp_corr_met         = agg$fp_corr_met,
      tp_fa_met           = agg$tp_fa_met,
      uses_private        = agg$uses_private,
      uses_self           = agg$uses_self
    )]
    return(row[])
  }

  # Branch 3: ≥2 overlapping contracts
  row <- dt[1L]
  row[, `:=`(
    transition_month    = FALSE,
    multiple_contracts  = TRUE,
    active_FIN_ASSUR_ID = paste(sort(unique(dt$FIN_ASSUR_ID)), collapse = " | "),
    fr_covered          = TRUE,  # All expanded contract months have coverage by definition
    DETAIL_TYPE         = agg$DETAIL_TYPE,
    CATEGORY            = agg$CATEGORY,
    ISSUER_NAME         = agg$ISSUER_NAME,
    max_COVER_OCC       = agg$max_COVER_OCC,
    max_COVER_AGG       = agg$max_COVER_AGG,
    total_COVER_OCC     = agg$total_COVER_OCC,
    total_COVER_AGG     = agg$total_COVER_AGG,
    premium_prepaid     = agg$premium_prepaid,
    proof_on_file       = agg$proof_on_file,
    fp_corr_met         = agg$fp_corr_met,
    tp_fa_met           = agg$tp_fa_met,
    uses_private        = agg$uses_private,
    uses_self           = agg$uses_self
  )]
  return(row[])
} # end of collapse_logic function


# ──  Functional version of malformed date fixing  ───────────────────────────
fix_malformed_fa_dates <- function(dt, eff_date_col = "EFF_DATE", exp_date_col = "EXP_DATE") {
  # Helper function: Vector-safe fixer for malformed years like "02 01" → 2001, "007" → 2007
  fix_year <- function(d) {
    # keep original class & NA's
    out <- d                       
    bad <- !is.na(out) & (year(out) < 1900 | year(out) > 2100)
    
    if (any(bad)) {
      y_bad <- year(out[bad])
      out[bad] <- as.Date(sprintf(
        "%04d-%02d-%02d",
        y_bad %% 100 + 2000,   # 0201 → 2001, 0022 → 2022, etc.
        month(out[bad]),
        mday(out[bad])
      ))
    }
    out
  }
  
  # Create working copy to avoid modifying original
  dt_work <- copy(dt)
  
  # Ensure date columns are Date objects, not character
  # Check if columns are Date class, if not convert them
  if (!inherits(dt_work[[eff_date_col]], "Date")) {
    warning(paste("Converting", eff_date_col, "from", class(dt_work[[eff_date_col]]), "to Date"))
    dt_work[, (eff_date_col) := as.Date(get(eff_date_col))]
  }
  
  if (!inherits(dt_work[[exp_date_col]], "Date")) {
    warning(paste("Converting", exp_date_col, "from", class(dt_work[[exp_date_col]]), "to Date"))
    dt_work[, (exp_date_col) := as.Date(get(exp_date_col))]
  }
  
  # 1. Tag rows with weird 2-digit or 5-digit years
  dt_work[, `:=`(
    bad_eff = !is.na(get(eff_date_col)) & 
              (year(get(eff_date_col)) < 1900 | year(get(eff_date_col)) > 2100),
    bad_exp = !is.na(get(exp_date_col)) & 
              (year(get(exp_date_col)) < 1900 | year(get(exp_date_col)) > 2100)
  )]
  
  # Count original bad dates for summary
  n_originally_bad <- dt_work[bad_eff == TRUE | bad_exp == TRUE, .N]
  
  # 2. If only one of the two dates is bad, assume a one-year term
  dt_work[bad_eff == TRUE & bad_exp == FALSE,
          (eff_date_col) := get(exp_date_col) - years(1L)]
  
  dt_work[bad_exp == TRUE & bad_eff == FALSE,
          (exp_date_col) := get(eff_date_col) + years(1L)]
  
  # 3. Apply year fixing to remaining bad dates
  dt_work[bad_eff == TRUE, 
          (eff_date_col) := fix_year(get(eff_date_col))]
  
  dt_work[bad_exp == TRUE, 
          (exp_date_col) := fix_year(get(exp_date_col))]
  
  # 4. Flag anything we still couldn't rescue for manual review
  dt_work[, date_fix_flag := (bad_eff | bad_exp) & 
                             (is.na(get(eff_date_col)) | is.na(get(exp_date_col)))]
  
  # 5. Clean up helper columns
  dt_work[, c("bad_eff", "bad_exp") := NULL]
  
  # Return summary info
  n_still_bad <- dt_work[date_fix_flag == TRUE, .N]
  n_fixed <- n_originally_bad - n_still_bad
  
  cat("Date fixing summary:\n")
  cat("  Rows with malformed dates originally:", n_originally_bad, "\n")
  cat("  Rows with malformed dates fixed:", n_fixed, "\n")
  cat("  Rows still flagged for manual review:", n_still_bad, "\n")
  
  return(dt_work)
} # end of fix_malformed_fa_dates function

# ── Section 1: Facility extract ─────────────────────────────────────────────
# Purpose: Download, parse, and clean the PST facility file to obtain static facility-level data.
# Why: Provides the universe of facilities and basic site attributes (county, ZIP, status) for later merges.
# End product: `fac` data.table keyed by FACILITY_ID, with type-cast fields and blank indicators.

message("Processing Section 1: Facility extract...")

# Define fixed-width layout (379 bytes, 54 columns) based on PST spec
fac_widths <- c(
   8,15,6,60,30,10,30,1,1,1,
   4,4,50,30,2,5,4,256,35,35,
   2,5,15,15,28,60,100,50,50,30,
   2,5,4,3,7,5,3,7,5,50,
   1,10,10,15,15,28,60,35,100,1,
   10,1,30,30
)

fac_names <- c(
  "FACILITY_ID","ADD_ID","AI","FACILITY_NAME","FACILITY_TYPE",
  "FACILITY_BEGIN_DATE","FACILITY_STATUS","FACILITY_EXEMPT_STATUS",
  "RECORDS_OFF_SITE","UST_FR_REQUIRED","NUM_ACTIVE_USTS","NUM_ACTIVE_ASTS",
  "SITE_ADDRESS_DELIVERY","SITE_CITY","SITE_STATE_CODE","SITE_ZIP",
  "SITE_ZIP_EXT","SITE_LOCATION_DESC","SITE_NEAREST_CITY","SITE_COUNTY",
  "SITE_REGION","SITE_LOCATION_ZIP","CONTACT_FN","CONTACT_MI","CONTACT_LN",
  "CONTACT_TITLE","CONTACT_ORG","CONTACT_ADDR_DELIV","CONTACT_ADDR_INTDELIV",
  "CONTACT_ADDR_CITY","CONTACT_ADDR_STATE","CONTACT_ADDR_ZIP",
  "CONTACT_ADDR_ZIP_EXT","CONTACT_PHONE_AC","CONTACT_PHONE_NUM",
  "CONTACT_PHONE_EXT","CONTACT_FAX_AC","CONTACT_FAX_NUM","CONTACT_FAX_EXT",
  "CONTACT_EMAIL","CONTACT_ADDR_DELIVERABLE","APP_REC_DATE","SIG_DATE",
  "SIG_FN","SIG_MI","SIG_LN","SIG_TITLE","SIG_ROLE","SIG_COMPANY",
  "ENFORCEMENT_ACTION","ENFORCEMENT_ACTION_DATE",
  "FACILITY_NOT_INSPECTABLE","NOT_INSPECTABLE_REASON_1",
  "NOT_INSPECTABLE_REASON_2"
)

# Sanity checks
stopifnot(length(fac_widths) == length(fac_names))
stopifnot(sum(fac_widths) == 1379)

# Load and parse
fac <- load_and_process_data(
  paste0(base, "/pst_fac.txt"), fac_widths, fac_names
)

# Persist raw load
saveRDS(fac, file = get_data_path("Outputs", "raw_pst_fac.rds"))
fwrite(fac, file = get_data_path("Outputs", "raw_pst_fac.csv"), row.names = FALSE)
# Clean key and flag blanks
fac <- clean_id_column(fac, "FACILITY_ID")

fac[, `:=`(
  NUM_ACTIVE_USTS_is_blank = trimws(NUM_ACTIVE_USTS) == "",
  NUM_ACTIVE_ASTS_is_blank = trimws(NUM_ACTIVE_ASTS) == ""
)]

# Rename long fields for consistency
rename_map <- c(
  FACILITY_EXEMPT_STATUS = "FACILITY_EXEMPT",
  RECORDS_OFF_SITE       = "OFFSITE"
)
present <- intersect(names(rename_map), names(fac))
if (length(present)) setnames(fac, present, rename_map[present])

# Type conversions
fac[, FACILITY_BEGIN_DATE := lubridate::mdy(trimws(FACILITY_BEGIN_DATE))]
fac[, `:=`(
  UST_FR_REQUIRED   = UST_FR_REQUIRED   == "Y",
  FACILITY_EXEMPT   = FACILITY_EXEMPT   == "Y",
  OFFSITE           = OFFSITE           == "Y",
  NUM_ACTIVE_USTS   = as.integer(NUM_ACTIVE_USTS),
  NUM_ACTIVE_ASTS   = as.integer(NUM_ACTIVE_ASTS),
  FACILITY_BEGIN_YR = year(FACILITY_BEGIN_DATE)
)]

# Keep only FR-required facilities


# Key for merges
setkey(fac, FACILITY_ID)



message("Section 1 complete: ", nrow(fac), " facilities loaded.")



# ── Section 2: Financial-Assurance extract & monthly / facility-month collapse ─
# Purpose: read raw FA records, fix malformed dates, expand each contract to its active months,
#   then collapse to one row per facility-month summarizing coverage, gaps, ERPs, and change flags.
# Why: to capture each facility’s insurance/self-insurance status over time.
# End product:
#   • `fa_monthly_contract` (contract-level × month series; key FACILITY_ID, FIN_ASSUR_ID, YEAR, MONTH)
#   • `fa_monthly` (facility × month summary; key FACILITY_ID, YEAR, MONTH)
# Usage: merged into main `panel` by FACILITY_ID, YEAR, MONTH in Section 7.
message("Processing Section 2: Financial-Assurance extract...")
# Updated based on the provided data dictionary for FINANCIAL ASSURANCE table
# ── Financial-Assurance column names ──────────────────────────────────────────

# Column widths for the PST Financial-Assurance extract
fa_widths <- c(
  8,  # FINANCIAL ASSURANCE ID
  8,  # FACILITY ID (padded)
  6,  # Facility ID (AI)
  10, # Form Received Date
  30, # Mechanism Type
  30, # Mechanism Type Other
  1,  # Multiple Mechanism Types flag
  50, # Issuer Name
  5,  # Issuer Phone – country code
  3,  # Issuer Phone – area code
  7,  # Issuer Phone Number
  5,  # Issuer Phone extension
  30, # Policy or Mechanism Number
  10, # Coverage Effective Begin Date
  10, # Coverage Expiration Date
  30, # Coverage Amount per Occurrence
  30, # Coverage Amount per Annual Aggregate
  1,  # Insurance Premium Pre-Paid flag
  1,  # First-Party Corrective-Action Met flag
  1,  # Third-Party FA Met flag
  1,  # Proof of Financial Assurance flag
  1   # Meets FA Requirements flag
)

# Sanity-check: sum should be 278 bytes
stopifnot(sum(fa_widths) == 278)


fa_names_core <- c(
  "FIN_ASSUR_ID",        # 1   Financial Assurance ID
  "FACILITY_ID",         # 2   Facility ID (Foreign Key)
  "FACILITY_AI",         # 3   Facility AI number
  "FORM_REC_DATE",       # 4   Form Received Date
  "MECH_TYPE",           # 5   Mechanism Type
  "MECH_TYPE_OTHER",     # 6   Mechanism Type – Other
  "MULTI_MECH_TYPES",    # 7   Multiple Mechanism Types flag (Y/N)
  "ISSUER_NAME",         # 8   Issuer Name
  "ISSUER_PHONE_CTRY",   # 9   Issuer Phone – country code
  "ISSUER_PHONE_AREA",   # 10  Issuer Phone – area code
  "ISSUER_PHONE_NUM",    # 11  Issuer Phone – number
  "ISSUER_PHONE_EXT",    # 12  Issuer Phone – extension
  "POLICY_MECH_NUM",     # 13  Policy or Mechanism Number
  "COVER_EFF",           # 14  Coverage Effective Begin Date
  "COVER_EXP",           # 15  Coverage Expiration Date
  "COVER_OCC",           # 16  Coverage Amount per Occurrence
  "COVER_AGG"            # 17  Coverage Amount per Annual Aggregate
)

fa_names <- c(
  fa_names_core,
  "PREMIUM_PREPAID",     # 18  Insurance Premium Pre-Paid flag
  "FP_CORR_MET",         # 19  First-Party Corrective-Action Met flag
  "TP_FA_MET",           # 20  Third-Party Financial-Assurance Met flag
  "PROOF_OF_FA",         # 21  Proof-of-Financial-Assurance flag
  "MEETS_FLAG"           # 22  Meets Financial-Assurance Requirements flag
)



# Load and process FA data
fa <- load_and_process_data(
  paste0(base,"/pst_fin_assur.txt"), fa_widths, fa_names)

setDT(fa)

# Convert flags and clean data  
fa[, PREMIUM_PREPAID := PREMIUM_PREPAID == "Y"]
fa[, FP_CORR_MET     := FP_CORR_MET     == "Y"]
fa[, TP_FA_MET       := TP_FA_MET       == "Y"]
fa[, PROOF_OF_FA     := PROOF_OF_FA     == "Y"]

saveRDS(fa, file = get_data_path("Outputs","raw_pst_fin_assur.rds"))
fa = clean_merge_keys(fa, "FACILITY_ID")

# Process coverage amounts and dates
fa[, `:=`(
  COVER_OCC_is_blank = trimws(COVER_OCC) == "",
  COVER_AGG_is_blank = trimws(COVER_AGG) == "",
  FACILITY_ID = trimws(substr(FACILITY_ID,1,6)),
  EFF_DATE = mdy(COVER_EFF),
  EXP_DATE = mdy(COVER_EXP),
  MULTI_MECH_TYPES = MULTI_MECH_TYPES == "Y",
  MEETS_FLAG = MEETS_FLAG == "Y"
)]


# 1. strip non-digits (e.g. commas), turn blank→NA
fa[, COVER_OCC_clean := na_if(trimws(gsub("[^0-9\\.]", "", COVER_OCC)), "")]
fa[, COVER_AGG_clean := na_if(trimws(gsub("[^0-9\\.]", "", COVER_AGG)), "")]

# 2. numeric coercion, safe
fa[, COVER_OCC := as.numeric(COVER_OCC_clean)]
fa[, COVER_AGG := as.numeric(COVER_AGG_clean)]

# 3. drop the helpers
fa[, c("COVER_OCC_clean", "COVER_AGG_clean") := NULL]

# Make sure dates are parsed before calling fix_malformed_fa_dates

# Fix malformed dates
fa <- fix_malformed_fa_dates(fa, "EFF_DATE", "EXP_DATE")

# Check results
bad_now <- fa[date_fix_flag == TRUE]
cat("🔎 After automatic repair, rows still bad:", nrow(bad_now), "\n")

# Apply mechanism type lookup table
fa[, MECH_TYPE_upper := toupper(trimws(MECH_TYPE))]



# Create lookup table
mech_type_lookup <- data.table(
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

# Merge lookup table
fa <- merge(fa, mech_type_lookup, by = "MECH_TYPE_upper", all.x = TRUE)

# Handle PST Remediation Fund special cases
fa[MECH_TYPE_upper == "TRUST FUND" & 
   !is.na(MECH_TYPE_OTHER) & 
   grepl("PST REMEDIATION FUND", MECH_TYPE_OTHER, ignore.case = TRUE), 
   `:=`(DETAIL_TYPE = "State Insurance", CATEGORY = "State Fund")]

fa[MECH_TYPE_upper == "OTHER" & 
   !is.na(MECH_TYPE_OTHER) & 
   grepl("PST REMEDIATION FUND", MECH_TYPE_OTHER, ignore.case = TRUE), 
   `:=`(DETAIL_TYPE = "State Insurance", CATEGORY = "State Fund")]

# Handle unmatched rows
fa[is.na(DETAIL_TYPE), DETAIL_TYPE := "Other or Unspecified"]
fa[is.na(CATEGORY), CATEGORY := "Other or Unspecified"]

# Create convenience flags
fa[, USE_PRIVATE := CATEGORY == "Insurance"]
fa[, USE_SELF    := CATEGORY == "Self-Insurance"]

# Create mechanism dummies
mech_levels <- mech_type_lookup$MECH_TYPE_upper
dummy_names <- paste0("MECH_", gsub("[^A-Z0-9]", "_", mech_levels))

fa[, (dummy_names) := lapply(
       mech_levels,
       function(lvl) as.integer(MECH_TYPE_upper == lvl)
     )]

# Clean up and set mapped flags
fa[, MECH_TYPE_upper := NULL]
fa[, `:=`(USE_PRIVATE_mapped = USE_PRIVATE,
          USE_SELF_mapped    = USE_SELF)]

# Simple monthly expansion - each contract becomes monthly rows
fa_monthly_contract <- fa[
  !is.na(EFF_DATE) & !is.na(EXP_DATE) & EFF_DATE <= EXP_DATE,
  {
    mo_seq <- seq(
      floor_date(EFF_DATE, "month"),
      floor_date(pmin(EXP_DATE, as.Date("2026-12-01")), "month"),
      by = "month"
    )
     # Calculate start/end days within each month
    month_starts <- floor_date(mo_seq, "month")
    month_ends <- ceiling_date(mo_seq, "month") - days(1)
    
    .(YEAR = year(mo_seq),
      MONTH = month(mo_seq),
      EFF_DATE = EFF_DATE,
      EXP_DATE = EXP_DATE,
      # Add the missing columns for collapse_logic
      start_day_in_month = pmax(EFF_DATE, month_starts),
      end_day_in_month = pmin(EXP_DATE, month_ends),
      DETAIL_TYPE = DETAIL_TYPE,
      CATEGORY = CATEGORY,
      ISSUER_NAME = ISSUER_NAME,
      COVER_OCC = COVER_OCC,
      COVER_AGG = COVER_AGG,
      premium_prepaid = PREMIUM_PREPAID,
      proof_on_file = PROOF_OF_FA,
      fp_corr_met = FP_CORR_MET,
      tp_fa_met = TP_FA_MET,
      uses_private = USE_PRIVATE_mapped,
      uses_self = USE_SELF_mapped,
      fr_covered = TRUE)
  },
  by = .(FACILITY_ID, FIN_ASSUR_ID)
]

# Collapse to facility-month
fa_monthly <- fa_monthly_contract[, collapse_logic(.SD), by = .(FACILITY_ID, YEAR, MONTH)]

# Create dummy-encoded columns for categorical variables
dummy_cols  <- c("ISSUER_NAME", "CATEGORY", "DETAIL_TYPE")


dummy_long <- melt(
  fa_monthly,
  id.vars      = c("FACILITY_ID", "YEAR", "MONTH"),
  measure.vars = dummy_cols,
  variable.name = "var",
  value.name    = "val"
)[!is.na(val) & val != ""]

fa_dummies <- dcast(
  dummy_long,
  FACILITY_ID + YEAR + MONTH ~ paste0(var, "_", gsub("[^A-Za-z0-9]", "_", val)),
  fun.aggregate = length,
  value.var     = "val"
)

# Merge dummies back
fa_monthly <- merge(
  fa_monthly,
  fa_dummies,
  by = c("FACILITY_ID", "YEAR", "MONTH"),
  all.x = TRUE
)

# Convert NA dummies to 0
dummy_cols <- names(fa_dummies)[-(1:3)]
fa_monthly[, (dummy_cols) := lapply(.SD, function(x) {
    x[is.na(x)] <- 0L
    x
  }), .SDcols = dummy_cols]

# Create Zurich 2012 lookup
fa_monthly_contract[, is_zurich := grepl(
  "ZURICH|ZURICH AMERICAN INSURANCE COMPANY",
  toupper(trimws(ISSUER_NAME)),
  ignore.case = TRUE
)]
fwrite(fa_monthly_contract, get_data_path("Processed", "fa_monthly_contracts.csv"))


zurich_2012_lookup <- fa_monthly_contract[
  YEAR == 2012 & is_zurich == TRUE,
  .(had_zurich_2012 = 1L),
  by = FACILITY_ID]

zurich_2012_lookup <- unique(zurich_2012_lookup, by = "FACILITY_ID")
fwrite(zurich_2012_lookup, get_data_path("Processed", "zurich_2012_lookup.csv"))
fwrite(fa_monthly, get_data_path("Processed", "fa_monthly.csv"))

# Sanity check
stopifnot(anyDuplicated(fa_monthly[, .(FACILITY_ID, YEAR, MONTH)]) == 0)

message("Section 2: Financial-Assurance extract complete.")

# Save processed data
fa_processed <- fa[, .(
  FIN_ASSUR_ID, FACILITY_ID, FORM_REC_DATE, EFF_DATE, EXP_DATE,
  MECH_TYPE, MECH_TYPE_OTHER, DETAIL_TYPE, CATEGORY, MULTI_MECH_TYPES,
  MEETS_FLAG, ISSUER_NAME, POLICY_MECH_NUM, COVER_OCC, COVER_AGG, 
  COVER_OCC_is_blank, COVER_AGG_is_blank, USE_PRIVATE_mapped, USE_SELF_mapped
)]

setnames(fa_processed, 
         c("FORM_REC_DATE", "MECH_TYPE_OTHER", "MULTI_MECH_TYPES", "POLICY_MECH_NUM",
           "USE_PRIVATE_mapped", "USE_SELF_mapped"), 
         c("FORM_DATE", "MECH_TYPE_OTHR", "MULTI_MECH", "POLICY_NUM",
           "USE_PRIVATE", "USE_SELF"), skip_absent = TRUE)

fwrite(fa_processed, get_data_path("Outputs", "texas_fr_processed.csv"))
saveRDS(fa_processed, get_data_path("Outputs", "texas_fr_processed.rds"))

setkey(fa_monthly, FACILITY_ID, YEAR, MONTH)

#quick graph for records request
## plot the number of unique faciliyt ids by year of fa_monthly
## make the graph clear and easy to read for records request/questions

library(ggplot2)
faciilyts_FR_by_year = fa_monthly[,.(
  unique_facilities = uniqueN(FACILITY_ID)
), by = YEAR]

#full year span 1990-2025
fa_year = data.table(YEAR =  seq(1990, 2025))
fa_year <- merge(fa_year, faciilyts_FR_by_year, by = "YEAR", all.x = TRUE)
fa_year[is.na(unique_facilities), unique_facilities := 0]

library(ggplot2)

#— Parameters for the shaded gap —#
gap_xmin <- 2000
gap_xmax <- 2006.99   # 2006-12-31
gap_label_y <- max(fa_year$unique_facilities, na.rm = TRUE) * 0.92

ggplot(fa_year, aes(x = YEAR, y = unique_facilities)) +
  # 1. grey band for missing years
  annotate(
    "rect",
    xmin = gap_xmin, xmax = gap_xmax,
    ymin = -Inf,   ymax =  Inf,
    fill = "grey85", alpha = 0.5
  ) +
  # (optional) mark the gap with text
  annotate(
    "text",
    x = (gap_xmin + gap_xmax) / 2,
    y = gap_label_y,
    label = "No data\n2000–2006",
    colour = "grey30",
    size = 3.5,
    lineheight = 0.9
  ) +
  # 2. the time-series itself
  geom_line(size = 0.8, colour = "#283747") +
  
  # 3. clearer labelling
  labs(
    title   = "Financial-Assurance Coverage by Year",
    subtitle = "Texas PST facilities, fiscal years 1988–2024",
    x       = "Fiscal year (ending September 30)",
    y       = "Number of facilities with\nfinancial-assurance records",
    caption = "Source: Texas Commission on Environmental Quality (PST registration files)"
  ) +
  
  # 4. academic-style theme tweaks
  scale_x_continuous(
    breaks = seq(min(fa_year$YEAR), max(fa_year$YEAR), by = 5),
    minor_breaks = NULL,
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.05))
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title      = element_text(face = "bold", hjust = 0.5, size = 14),
    plot.subtitle   = element_text(hjust = 0.5, margin = margin(b = 8)),
    axis.title.x    = element_text(margin = margin(t = 8)),
    axis.title.y    = element_text(margin = margin(r = 8)),
    panel.grid.minor = element_blank()
  )




# ── Section 3: Owner/Operator timelines ──────────────────────────────────────
# Purpose: load PST owner & operator extracts, build intervals, expand to months,
#   and aggregate per facility-month into concatenated ID/name/type strings.
# Why: to know who owns/operates each facility in each month, and detect changes/orphans.
# End product: `fac_contact_months` keyed by FACILITY_ID, YEAR, MONTH
# Usage: merged into `panel` by FACILITY_ID, YEAR, MONTH in Section 7.

library(data.table)
library(lubridate)

message("Processing Section 3: Owner & Operator timelines…")

# ───────────────────────── user-tunable ──────────────────────────────────────
analysis_end   <- as.Date("2026-12-01")   
analysis_start <- as.Date("1990-01-01")             # earliest month kept
# ─────────────────────────────────────────────────────────────────────────────
# ── libraries ────────────────────────────────────────────────────────────
library(data.table)
library(lubridate)

# ── generic contact-file loader built on load_and_process_data ───────────
read_contact <- function(file,
                         widths,
                         names,
                         date_col) {

  dt <- load_and_process_data(paste0(base, "/", file), widths, names)

  # light post-processing --------------------------------------------------
  set(dt, j = date_col,     value = as.IDate(dt[[date_col]], "%m/%d/%Y"))
  set(dt, j = "FACILITY_ID", value = trimws(dt$FACILITY_ID))

  unique(dt[])
}
# ── OWNER spec (record length = 682) ─────────────────────────────────────
owner_widths <- c(
  15,15,6,11,100,15,15,10,2,15, 10,15,15,28,60,100,
  50,50,30,2,5,4,15,3,5,3,7,5,5,3,7,5,50,1
)

owner_names <- c(
  "PRINC_ID","ADD_ID","FACILITY_ID","OWNER_CN",
  "OWNER_NAME_CO_LAST","OWNER_NAME_FIRST","OWNER_NAME_MIDDLE",
  "OWNER_EFF_BEGIN_DT","OWNER_TYPE","STATE_TAX_ID","CONTACT_ROLE",
  "CONTACT_FIRST_NM","CONTACT_MIDDLE_NM","CONTACT_LAST_NM",
  "CONTACT_TITLE","CONTACT_ORG_NM","MAIL_ADDR_DELIVERY",
  "MAIL_ADDR_INTERNAL","MAIL_CITY","MAIL_STATE","MAIL_ZIP",
  "MAIL_ZIP_PLUS4","MAIL_FOREIGN_POSTAL","MAIL_COUNTRY",
  "PHONE_COUNTRY","PHONE_AREA","PHONE_NUM","PHONE_EXT",
  "FAX_COUNTRY","FAX_AREA","FAX_NUM","FAX_EXT","EMAIL",
  "ADDR_DELIVERABLE_FLAG"
)

owners_raw <- read_contact(
  file     = "pst_own.txt",
  widths   = owner_widths,
  names    = owner_names,
  date_col = "OWNER_EFF_BEGIN_DT"
)

# ── OPERATOR spec (record length = 667) ──────────────────────────────────
operator_widths <- c(
  15,15,6,11,100,15,15,10,2,10, 15,15,28,60,100,
  50,50,30,2,5,4,15,3,5,3,7,5,5,3,7,5,50,1
)

operator_names <- c(
  "PRINC_ID","ADD_ID","FACILITY_ID","OPERATOR_CN",
  "OPERATOR_NAME_CO_LAST","OPERATOR_NAME_FIRST","OPERATOR_NAME_MIDDLE",
  "OPERATOR_EFF_BEGIN_DT","OPERATOR_TYPE","CONTACT_ROLE",
  "CONTACT_FIRST_NM","CONTACT_MIDDLE_NM","CONTACT_LAST_NM",
  "CONTACT_TITLE","CONTACT_ORG_NM","MAIL_ADDR_DELIVERY",
  "MAIL_ADDR_INTERNAL","MAIL_CITY","MAIL_STATE","MAIL_ZIP",
  "MAIL_ZIP_PLUS4","MAIL_FOREIGN_POSTAL","MAIL_COUNTRY",
  "PHONE_COUNTRY","PHONE_AREA","PHONE_NUM","PHONE_EXT",
  "FAX_COUNTRY","FAX_AREA","FAX_NUM","FAX_EXT","EMAIL",
  "ADDR_DELIVERABLE_FLAG"
)

operators_raw <- read_contact(
  file     = "pst_opr.txt",          # adjust if your filename differs
  widths   = operator_widths,
  names    = operator_names,
  date_col = "OPERATOR_EFF_BEGIN_DT"
)


view(operators_raw)

view(owners_raw)



# ───────────────── SECTION 3 : Owner / Operator monthly flags ───────────────
# assumes owners_raw, operators_raw, analysis_start, analysis_end already exist

## 1 ─ Build closed-open ownership & operator intervals -----------------------
owners_int <- owners_raw[
  order(FACILITY_ID, OWNER_EFF_BEGIN_DT)
][, .(
    OWNER_CN, OWNER_NAME_CO_LAST,
    START_DATE = OWNER_EFF_BEGIN_DT,
    END_DATE   = shift(OWNER_EFF_BEGIN_DT,
                       type = "lead",
                       fill = as.IDate("9999-12-31")) - 1L
  ), by = FACILITY_ID]

operators_int <- operators_raw[
  order(FACILITY_ID, OPERATOR_EFF_BEGIN_DT)
][, .(
    OPERATOR_CN,
    START_DATE = OPERATOR_EFF_BEGIN_DT,
    END_DATE   = shift(OPERATOR_EFF_BEGIN_DT,
                       type = "lead",
                       fill = as.IDate("9999-12-31")) - 1L
  ), by = FACILITY_ID]

setkey(owners_int,    FACILITY_ID, START_DATE, END_DATE)
setkey(operators_int, FACILITY_ID, START_DATE, END_DATE)

## 2 ─ Facility-month skeleton over full study window ------------------------
fac_months <- owners_raw[
  , .(dummy = 1), keyby = FACILITY_ID
][
  , .(panel_date = seq(analysis_start, analysis_end, by = "month")),
  by = FACILITY_ID]

setkey(fac_months, FACILITY_ID, panel_date)

## 3 ─ Non-equi joins to attach current owner(s) & operator(s) ---------------
fac_own <- owners_int[
  fac_months,
  on = .(FACILITY_ID,
         START_DATE <= panel_date,
         END_DATE   >= panel_date),
         .(x.FACILITY_ID,  x.OWNER_CN,i.panel_date,x.START_DATE,x.END_DATE),
  allow.cartesian = TRUE,
  nomatch         = 0L]



fac_opr <- operators_int[
  fac_months,
  on = .(FACILITY_ID,
         START_DATE <= panel_date,
         END_DATE   >= panel_date),
         .(x.FACILITY_ID, i.panel_date, x.OPERATOR_CN,x.START_DATE,x.END_DATE),
  allow.cartesian = TRUE,
  nomatch         = 0L]

#remove all the x. and i. prefixes from all cols

setnames(fac_own, gsub("^x\\.|^i\\.", "", names(fac_own)))
setnames(fac_opr, gsub("^x\\.|^i\\.", "", names(fac_opr)))

## 4 ─ Owner facility counts & multi-facility flag ---------------------------
own_sizes <- fac_own[
  , .(owner_facility_count = uniqueN(FACILITY_ID)),
  by = .(OWNER_CN, panel_date)]

fac_own  <- own_sizes[fac_own, on = .(OWNER_CN, panel_date)]

## 5 ─ Collapse to one row per FACILITY-month --------------------------------
owner_sets <- fac_own[
  , .(
      OWNER_SET              = paste(sort(unique(OWNER_CN)), collapse = ";"),
      owner_facility_count   = max(owner_facility_count),
      multi_fac_owner_flag   = as.integer(any(owner_facility_count > 1))
    ),
  by = .(FACILITY_ID, panel_date)]

operator_sets <- fac_opr[
  , .(OPERATOR_SET = paste(sort(unique(OPERATOR_CN)), collapse = ";")),
  by = .(FACILITY_ID, panel_date)]

## 6 ─ Combine + derive change / difference flags ----------------------------
fac_contact_months <- owner_sets[
  operator_sets, on = .(FACILITY_ID, panel_date)
]

setorder(fac_contact_months, FACILITY_ID, panel_date)

fac_contact_months[
  , ownership_change_flag :=
      as.integer(OWNER_SET != shift(OWNER_SET)),
  by = FACILITY_ID
][is.na(ownership_change_flag), ownership_change_flag := 0L]

fac_contact_months[
  , operator_diff_owner_flag :=
      as.integer(OPERATOR_SET != "" & OPERATOR_SET != OWNER_SET)
]

## 7 ─ Final YEAR / MONTH columns then drop panel_date -----------------------
fac_contact_months[
  , `:=`(YEAR  = year(panel_date),
         MONTH = month(panel_date))
][
  , panel_date := NULL][]   # the [] prints a preview

setcolorder(fac_contact_months,
            c("FACILITY_ID","YEAR","MONTH",
              "multi_fac_owner_flag","owner_facility_count",
              "ownership_change_flag","operator_diff_owner_flag"))

sum(fac_contact_months$multi_fac_owner_flag,na.rm = T) # check how many multi-facility owners

message("Section 3 complete → fac_contact_months ready for panel merge")

# ── Section 4: Self-Certification summary ────────────────────────────────────
# Purpose: read PST self-cert records, map to facility, expand to year’s 12 months,
#   and collapse to flags for FA_FLAG & TECH_FLAG per facility-month.
# Why: to track when facilities self-attest to meeting financial & technical standards.
# End product: `self_cert_monthly` keyed by FACILITY_ID, YEAR, MONTH
# Usage: merged into `panel` by FACILITY_ID, YEAR, MONTH in Section 7.
message("Processing Section 4: Self-Certification summary...")
# Updated based on the provided data dictionary for SELF-CERTIFICATION table
sc_widths <- c(
  8,  # SELF_CERT ID
  8,  # FACILITY ID (Foreign Key) -> FACILITY_ID_PAD
  6,  # Facility ID (AI)
  10, # Signature Date
  50, # Signature Name
  30, # Signature Title (Placeholder)
  30, # Signature Type Role (Placeholder)
  30, # Filing Status (Placeholder)
  1,  # Registration Self-Certification flag -> REG_FLAG
  1,  # Facility Fees Self-Certification flag -> FEE_FLAG
  1,  # Financial Assurance Self-Certification flag -> FA_FLAG
  1,  # Technical Standards Self-Certification flag -> TECH_FLAG
  10  # Delivery Certificate Expiration Date -> DEL_CERT_EXP
)
sc_names  <- c(
  "SC_ID", "FACILITY_ID_PAD", "AI", "SIGN_DATE",
  "SIGN_NAME", "UNUSED_SIGN_TITLE", "UNUSED_SIGN_TYPE_ROLE", "UNUSED_FILING_STATUS",
  "REG_FLAG", "FEE_FLAG", "FA_FLAG", "TECH_FLAG", "DEL_CERT_EXP"
)

self_cert <- load_and_process_data(
  paste0(base,"/pst_self_cert.txt"), sc_widths, sc_names)
saveRDS(self_cert, file = get_data_path("Outputs","raw_pst_self_cert.rds"))
clean_merge_keys(self_cert, "FACILITY_ID_PAD")
self_cert[, `:=`(
  FACILITY_ID = trimws(substr(FACILITY_ID_PAD,1,6)),
  SIGN_DATE = mdy(SIGN_DATE),
  FA_FLAG = FA_FLAG == "Y",
  TECH_FLAG = TECH_FLAG == "Y"
)]

# Expand self-certification to monthly level
self_cert[, `:=`(YEAR = year(SIGN_DATE), MONTH = month(SIGN_DATE))]
# Create monthly records for the entire year when certification was signed
self_cert_expanded <- self_cert[!is.na(YEAR), {
  months_in_year <- 1:12
  .(FACILITY_ID = FACILITY_ID, YEAR = YEAR, MONTH = months_in_year, 
    FA_FLAG = FA_FLAG, TECH_FLAG = TECH_FLAG)
}, by = .(SC_ID, SIGN_DATE)]

# Aggregate to facility-year-month level
self_cert_monthly <- self_cert_expanded[, .(
  FA_FLAG = any(FA_FLAG, na.rm = TRUE), 
  TECH_FLAG = any(TECH_FLAG, na.rm = TRUE)
), by = .(FACILITY_ID, YEAR, MONTH)]
message("Section 4: Self-Certification summary complete.")

setkey(self_cert_monthly, FACILITY_ID, YEAR, MONTH)

# ── Section 5: UST & compartment snapshot ───────────────────────────────────
# Purpose: load raw UST byte-map + compartment detection data,
#   clean IDs & dates, derive closure spells, tank wall/material flags, detection flags.
# Why: to get tank-level attributes and life-spans for later monthly aggregation.
# End product: cleaned `ust` (keyed by UST_ID) plus compartment det/class tables.

message("Processing Section 5: UST tank snapshot...")



###############################################################################
# ========== 5 A.  ADD MATERIAL + DETECTION COLUMNS TO THE RAW UST & COMP =====
###############################################################################



# Define UST file structure (bytes 1-201)
ust_widths <- c(
  8,  8,  6, 10,  2, 10, 10,  8, 30, 10,  1, 30, 10, 1, 1,     # 1-145
  rep(1, 44),                                                  # 146-189 1-byte flags
  1,                                                          # 190 TECH_COMPLY
  1,                                                          # 191 TANK_TESTED_FLAG
  10                                                          # 192-201 INSTALL_SIG_DATE
)

ust_names <- c(
  # Core fields (1-145)
  "UST_ID","FACILITY_ID_PAD","AI","TANK_ID","COMPARTS",
  "INSTALL_DATE","REG_DATE","CAPACITY","STATUS","STATUS_DATE",
  "EMPTY","REG_STATUS","TANK_INT_PROT_DATE","TANK_SINGLE","TANK_DOUBLE",
  # Tank/pipe material and configuration flags (146-189)
  "PIP_SINGLE","PIP_DOUBLE",
  "EXT_CONT_JACKET","EXT_CONT_SYN_LNR","EXT_CONT_VAULT",
  "EXT_CONT_PIP_JACKET","EXT_CONT_PIP_SYN_LNR","EXT_CONT_PIP_VAULT",
  "PIPE_TYPE",
  "TANK_MAT_STEEL","TANK_MAT_FRP","TANK_MAT_COMPOSITE","TANK_MAT_CONCRETE",
  "TANK_MAT_JACKETED","TANK_MAT_COATED",
  "PIP_MAT_STEEL","PIP_MAT_FRP","PIP_MAT_CONCRETE","PIP_MAT_JACKETED",
  "PIP_MAT_FLEX",
  "PIP_VALVE_SHEAR","PIP_SWING_JOINT","PIP_FLEX_CONN",
  "CORR_TANK_CP","CORR_PIPE_CP","CORR_TANK_VARIANCE","CORR_PIPE_VARIANCE",
  "TOS_COMPLY",
  paste0("UNUSED_FLAG_", 1:16),
  # Final fields (190-201)
  "TECH_COMPLY","TANK_TESTED_FLAG","INSTALL_SIG_DATE"
)

# Detection method columns
detect_cols <- c(
  # Compartment methods
  "DET_C_VAPOR","DET_C_GW","DET_C_SEC_CONT","DET_C_ATG",
  "DET_C_INTERSTITIAL","DET_C_MANUAL_WEEK","DET_C_MANUAL_MONTH","DET_C_SIR",
  # Piping methods
  "DET_P_VAPOR","DET_P_GW","DET_P_SEC_CONT","DET_P_INTERSTITIAL",
  "DET_P_TT_MONTH","DET_P_TT_ANNUAL","DET_P_TT_TRIEN","DET_P_LLD",
  "DET_P_SIR","DET_P_SUCTION_EXEMPT"
)

# Validation
stopifnot(length(ust_widths) == length(ust_names), sum(ust_widths) == 201)

# Load and process UST data
ust <- load_and_process_data(paste0(base,"/pst_ust.txt"), ust_widths, ust_names)

# Clean and standardize UST_ID (primary key)
ust <- clean_id_column(ust, "UST_ID")

ust[, UST_ID := toupper(UST_ID)]
setkey(ust, UST_ID)

# Verify uniqueness
message("UST_IDs: ", uniqueN(ust$UST_ID), " unique out of ", nrow(ust), " records")
stopifnot(uniqueN(ust$UST_ID) == nrow(ust))

glimpse(ust)

# Process dates and core fields in one operation
ust[, `:=`(
  # Extract facility ID
  FACILITY_ID = trimws(substr(FACILITY_ID_PAD, 1, 6)),
  
  # Parse dates
  INSTALL_DATE = mdy(INSTALL_DATE),
  REG_DATE = mdy(REG_DATE),
  STATUS_DATE = mdy(STATUS_DATE),
  TANK_INT_PROT_DATE = mdy(TANK_INT_PROT_DATE),
  
  # Convert core fields
  CAPACITY = as.numeric(CAPACITY),
  CAPACITY_is_blank = trimws(CAPACITY) == "",
  TANK_SINGLE = TANK_SINGLE == "Y",
  TANK_DOUBLE = TANK_DOUBLE == "Y",
  PIPE_TYPE = factor(PIPE_TYPE, 
                     levels = c("P", "S", "G"), 
                     labels = c("Pressurized", "Suction", "Gravity"))
)]

glimpse(ust)

# Process material flags efficiently
material_cols <- c("TANK_MAT_STEEL", "TANK_MAT_FRP", "TANK_MAT_COMPOSITE", 
                   "TANK_MAT_CONCRETE", "TANK_MAT_JACKETED", "TANK_MAT_COATED")

# Clean material flags and convert to logical
for(col in material_cols) {
  ust[is.na(get(col)) | get(col) == "", (col) := "N"]
}

# Create material type flags
ust[, `:=`(
  is_steel_tank = TANK_MAT_STEEL == "Y",
  is_fiberglass_tank = TANK_MAT_FRP == "Y",
  is_composite_tank = TANK_MAT_COMPOSITE == "Y",
  is_concrete_tank = TANK_MAT_CONCRETE == "Y",
  is_jacketed_tank = TANK_MAT_JACKETED == "Y",
  is_coated_tank = TANK_MAT_COATED == "Y"
)]

# Create unknown material flag
ust[, is_unknown_material_tank := !(is_steel_tank | is_fiberglass_tank | is_composite_tank | 
                                   is_concrete_tank | is_jacketed_tank | is_coated_tank)]

# Create wall type flags (single operation)
ust[, `:=`(
  single_walled = fifelse(TANK_SINGLE == TRUE, 1L, 0L),
  double_walled = fifelse(TANK_DOUBLE == TRUE, 1L, 0L),
  missing_walled = fifelse(is.na(TANK_SINGLE) & is.na(TANK_DOUBLE), 1L, 0L),
  unknown_walled = fifelse(TANK_SINGLE == FALSE & TANK_DOUBLE == FALSE, 1L, 0L)
)]

# Handle NA values in material flags
material_bool_cols <- c("is_steel_tank", "is_fiberglass_tank", "is_composite_tank", 
                        "is_concrete_tank", "is_jacketed_tank", "is_coated_tank", 
                        "is_unknown_material_tank")
for(col in material_bool_cols) {
  ust[is.na(get(col)), (col) := FALSE]
}

# Date imputation and clipping
min_panel_date <- as.Date("1990-01-01")
max_panel_date <- as.Date("2026-12-01")

##------------- Process status and closure dates -------------

#look at status
status_tally = ust[, .(tanks = uniqueN(UST_ID), Facilites = uniqueN(FACILITY_ID)), by = STATUS]
#clean and standardize status
status_tally[, STATUS := toupper(trimws(STATUS))]

# Create closure status flags using status_tally but dont include IN USE
closure_statuses <- status_tally$STATUS[c(1:2,4)]
ust[, is_closed_removed := toupper(trimws(STATUS)) %in% closure_statuses]
closed_ust_tally <- ust[is_closed_removed == TRUE, .(tanks = uniqueN(UST_ID), facilities = uniqueN(FACILITY_ID)), by = STATUS_DATE]

# Impute dates in single operation
ust[, `:=`(
  install_date = fifelse(
    !is.na(INSTALL_DATE),
    INSTALL_DATE,
    fifelse(
      !is.na(REG_DATE),
      REG_DATE,
      min_panel_date
    )
  ),
  end_date          = fifelse(is_closed_removed, STATUS_DATE, max_panel_date),
  end_imputed       = is_closed_removed & is.na(STATUS_DATE)
)]
closed_ust_tally <- ust[is_closed_removed == TRUE, .(tanks = uniqueN(UST_ID), facilities = uniqueN(FACILITY_ID)), by = .(end_date,STATUS_DATE)][order(end_date, STATUS_DATE)]
# Handle missing end dates and ensure logical date ordering
ust[is.na(end_date), end_date := max_panel_date]
nrow(ust)

ust[end_date < install_date, end_date := install_date]





message("USTs before filtering bad dates obs: ", nrow(ust))

# Filter valid tanks
ust <- ust[install_date <= end_date & !is.na(install_date) & !is.na(end_date)]
message("USTs after date processing and filtering: ", nrow(ust))
library(data.table)

#–– Function: run_stage01_validation ––
#' Run Stage 01 validation counts on a UST panel
#'
#' @param ust            data.table of raw UST records with columns:
#'                       INSTALL_DATE, REG_DATE, STATUS_DATE, is_closed_removed, UST_ID, FACILITY_ID
#' @param min_panel_date Date cutoff (e.g. as.Date("1990-01-01"))
#' @param max_panel_date Date cap for end_date imputation (e.g. as.Date("2026-12-01"))
#' @param stage_label    Character label for this run (default "stage_raw_cleaning")
#'
#' @return One‐row data.table with all validation counts and the stage label
run_stage01_validation <- function(ust,
                                   min_panel_date,
                                   max_panel_date,
                                   stage_label = "stage_raw_cleaning") {
  # ensure data.table
  ust <- as.data.table(ust)
  
  # 1) UST‐level counts
  ust_counts <- ust[, .(
    post_1990_usts              = sum(install_date >= min_panel_date),
    pre_1990_usts_active        = sum(install_date <  min_panel_date & end_date >= min_panel_date),
    total_number_of_USTS_in_raw = uniqueN(UST_ID)
  )]
  
  # 2) Facility‐level flags & counts (no double‐counting)
  facility_counts <- ust[, .(
      has_post_1990_install = any(install_date >= min_panel_date),
      has_pre_1990_active   = any(install_date <  min_panel_date & end_date >= min_panel_date)
    ),
    by = FACILITY_ID
  ][, .(
      total_number_of_facilities_in_raw = .N,
      post_1990_facilities              = sum(has_post_1990_install),
      pre_1990_facilities_active        = sum(has_pre_1990_active)
  )]
  
  # 3) True panel facility count
  total_panel_facilities <- ust[
    install_date >= min_panel_date |
    end_date       >= min_panel_date,
    uniqueN(FACILITY_ID)
  ]
  
  # 4) Combine into one summary row
  result <- cbind(ust_counts, facility_counts)[
    , stage := stage_label
  ][
    , number_of_panel_facilities := total_panel_facilities
  ]
  
  # 5) Print messages
  message("UST validation counts after ", stage_label, ":")
  message("  • Stage:                         ", result$stage)
  message("  • Post-1990 USTs:                ", result$post_1990_usts)
  message("  • Pre-1990 USTs still active:    ", result$pre_1990_usts_active)
  message("  • Total USTs in raw data:        ", result$total_number_of_USTS_in_raw)
  message("  • Total facilities in raw data:  ", result$total_number_of_facilities_in_raw)
  message("  • Post-1990 facilities:          ", result$post_1990_facilities)
  message("  • Pre-1990 facilities active:    ", result$pre_1990_facilities_active)
  message("  • Total facilities in panel:     ", result$number_of_panel_facilities)
  
  return(result[])
}

res01 <- run_stage01_validation(
  ust            = ust,
  min_panel_date = as.Date("1990-01-01"),
  max_panel_date = as.Date("2026-12-01"),
  stage_label    = "stage_raw_cleaning"
)




check_end_date = ust[,.(facs = uniqueN(FACILITY_ID), is_closed = sum(is_closed_removed)), by = end_date][order(end_date)]

# Fixed version - captures both sets you described
facilities_active_1990 <- ust[, .(
  has_ust_active_1990_plus = any(
    install_date >= min_panel_date |  # Set 1: Installed 1990 or later
    end_date >= min_panel_date        # Set 2: Still active 1990 or later  
  )
), by = FACILITY_ID][has_ust_active_1990_plus == TRUE]


nrow(facilities_active_1990)
facilities_active_1990 <- clean_id_column(facilities_active_1990, "FACILITY_ID")
message("Facilities with active USTs (1990+): ", nrow(facilities_active_1990))

# Apply early filtering to all datasets
datasets_to_filter <- list(
  list(data = quote(fac), id_col = "FACILITY_ID", name = "Facilities"),
  list(data = quote(fa), id_col = "FACILITY_ID", name = "FA records"),
  list(data = quote(fac_contact_months), id_col = "FACILITY_ID", name = "Contact months"),
  list(data = quote(self_cert), id_col = "FACILITY_ID", name = "Self-cert records"),
  list(data = quote(ust), id_col = "FACILITY_ID", name = "UST records"),
  list(data = quote(texas_compartment), id_col = "facility_id", name = "Compartment records")
)

message("Applying early filtering to all datasets...")
for(dataset_info in datasets_to_filter) {
  if(exists(as.character(dataset_info$data))) {
    dataset <- eval(dataset_info$data)
    original_count <- nrow(dataset)
    if(dataset_info$id_col %in% names(dataset)) {
      filtered_dataset <- dataset[get(dataset_info$id_col) %in% facilities_active_1990$FACILITY_ID]
      assign(as.character(dataset_info$data), filtered_dataset, envir = .GlobalEnv)
      message(dataset_info$name, " filtered from ", original_count, " to ", nrow(filtered_dataset))
    }
  }
}

# If you need to do it again later:
res02 <- run_stage01_validation(
  ust            = ust,      # e.g. after another cleaning step
  min_panel_date = as.Date("1990-01-01"),
  max_panel_date = as.Date("2026-12-01"),
  stage_label    = "stage_after_filter"
)


all_cols <- c("FACILITY_ID", "AI", "FACILITY_NAME", names(fac)[grepl("SITE_", names(fac))])
facility_data_for_record_request = fac[FACILITY_ID %in% facilities_active_1990$FACILITY_ID, 
                                      ..all_cols]                                        
fwrite(facility_data_for_record_request, 
       get_data_path("Outputs", "texas_facility_data_for_record_request.csv"))                                        


validateion_data <- bind_rows(
   res01,
   res02)

###############################################################################
# ========== 5 B. SUBSTANCE CLASSIFICATION & DETECTION PROCESSING =============
###############################################################################

# Process substance classification
# 1) Define Compartment file URL and local path
comp_url  <- "https://www.tceq.texas.gov/assets/public/admin/data/docs/pst_ust_comprt.txt"
comp_file <- file.path(base, "pst_ust_comprt.txt")


# 1) List all 30 raw flag names exactly as in the PST Compartment spec
raw_flags <- c(
  # Compartment release detection (8)
  "compartment_release_detection_vapor_monitoring",
  "compartment_release_detection_groundwater_monitoring",
  "compartment_release_detection_secondary_containment",
  "compartment_release_detection_automatic_tank_gauge",
  "compartment_release_detection_interstitial_monitoring",
  "compartment_release_detection_weekly_manual_tank_gauging",
  "compartment_release_detection_monthly_tank_gauging",
  "compartment_release_detection_sir_inventory_control",
  # Piping release detection (10)
  "piping_release_detection_vapor_monitoring",
  "piping_release_detection_groundwater_monitoring",
  "piping_release_detection_secondary_containment",
  "piping_release_detection_interstitial_monitoring",
  "piping_release_detection_monthly_tightness_test",
  "piping_release_detection_annual_tightness_test",
  "piping_release_detection_triennial_tightness_test",
  "piping_release_detection_auto_line_leak_detector",
  "piping_release_detection_sir_inventory_control",
  "piping_release_detection_exempt_system_suction",
  # Spill & overfill prevention equipment (6)
  "spill_overfill_tight_fill_fitting",
  "spill_overfill_factory_spill_container",
  "spill_overfill_delivery_shut_off_valve",
  "spill_overfill_flow_restrictor",
  "spill_overfill_alarm",
  "spill_overfill_na",
  # Compliance flags (3)
  "compartment_release_detection_compliance",
  "piping_release_detection_compliance",
  "spill_overfill_compliance",
  # Variance flags (3)
  "compartment_release_detection_variance",
  "piping_release_detection_variance",
  "spill_overfill_variance"
)
stopifnot(length(raw_flags) == 30)  # per PST spec :contentReference[oaicite:5]{index=5}

# 2) Build the fixed-width layout
comp_widths <- c(
  8,    # UST_COMPRT_ID
  8,    # UST_ID
  6,    # FACILITY_ID
  10,   # TANK_ID
  1,    # COMPRT_ID
  8,    # CAPACITY
  30,   # SUBSTANCE_STORED_1
  30,   # SUBSTANCE_STORED_2
  30,   # SUBSTANCE_STORED_3
  rep(1, length(raw_flags)),  # the 30 one-byte flags
  30,   # STAGE1_VAPOR_RECOVERY
  10    # STAGE1_INSTALL_DATE
)

comp_names <- c(
  "UST_COMPRT_ID", "UST_ID", "FACILITY_ID", "TANK_ID", "COMPRT_ID",
  "CAPACITY",
  "SUBSTANCE_STORED_1", "SUBSTANCE_STORED_2", "SUBSTANCE_STORED_3",
  raw_flags,
  "STAGE1_VAPOR_RECOVERY", "STAGE1_INSTALL_DATE"
)

# 3) Sanity check
stopifnot(
  length(comp_widths) == length(comp_names),
  sum(comp_widths)    == 201
)




# 4) Load using your existing helper
ust_compartments <- load_and_process_data(
   paste0(base, "/pst_ust_comprt.txt"),
   comp_widths,
   comp_names
)




setDT(ust_compartments)


texas_compartment_combined <- ust_compartments[
  , .(
      # 1) paste the three substance columns together
      substances = {
        tmp <- paste(SUBSTANCE_STORED_1,
                     SUBSTANCE_STORED_2,
                     SUBSTANCE_STORED_3,
                     sep = "; ")
        # 2) remove any "NA", leading/trailing separators
        tmp <- gsub("^NA; |; NA$|; NA; ", "", tmp)   # remove NA at start, end, or middle
        tmp[tmp == ""] <- NA                        # if all were NA, give a single NA
        tmp
      }
    ),
  by = .(FACILITY_ID, TANK_ID, UST_ID)
]

# View result
texas_compartment_combined

  # Classify substances
  texas_compartment_classified <- classify_substances(texas_compartment_combined)

  texas_compartment_classified = clean_id_column(texas_compartment_classified, "UST_ID")
texas_compartment_classified = clean_id_column(texas_compartment_classified, "FACILITY_ID")
  



  substance_cols <- c( "UST_ID", "is_gasoline", "is_diesel", 
                     "is_oil_kerosene", "is_jet_fuel", "is_other")
  texas_compartment_classified2 <- texas_compartment_classified[, ..substance_cols]
  

sum(texas_compartment_classified2[UST_ID %in% ust$UST_ID,is_gasoline], na.rm = TRUE)
sum(texas_compartment_classified2[UST_ID %in% ust$UST_ID,is_diesel], na.rm = TRUE)
# Merge substance classification
if("UST_ID" %in% names(texas_compartment_classified2)) {
  texas_compartment_classified2[, UST_ID := as.character(UST_ID)]
}


ust <- merge(ust, texas_compartment_classified2, by = c( "UST_ID"), all.x = TRUE)

sum(ust$is_gasoline, na.rm = TRUE)
sum(ust$is_diesel, na.rm = TRUE)


# Define which columns are your flags
flag_cols <- c("is_gasoline", "is_diesel", "is_oil_kerosene",
               "is_jet_fuel",   "is_other")

# 1) Coerce each flag to integer
ust[
  , (flag_cols) := lapply(.SD, as.integer),
  .SDcols = flag_cols
]

# 2) Create a logical column that is TRUE if any flag is set
ust[
  , has_substance_info := rowSums(.SD) > 0,
  .SDcols = flag_cols
]

###############################################################################
# Process detection methods
###############################################################################

# Rename detection columns in compartment data
det_rename <- c(
  compartment_release_detection_vapor_monitoring = "DET_C_VAPOR",
  compartment_release_detection_groundwater_monitoring = "DET_C_GW",
  compartment_release_detection_secondary_containment = "DET_C_SEC_CONT",
  compartment_release_detection_automatic_tank_gauge = "DET_C_ATG",
  compartment_release_detection_interstitial_monitoring = "DET_C_INTERSTITIAL",
  compartment_release_detection_weekly_manual_tank_gauging = "DET_C_MANUAL_WEEK",
  compartment_release_detection_monthly_tank_gauging = "DET_C_MANUAL_MONTH",
  compartment_release_detection_sir_inventory_control = "DET_C_SIR",
  piping_release_detection_vapor_monitoring = "DET_P_VAPOR",
  piping_release_detection_groundwater_monitoring = "DET_P_GW",
  piping_release_detection_secondary_containment = "DET_P_SEC_CONT",
  piping_release_detection_interstitial_monitoring = "DET_P_INTERSTITIAL",
  piping_release_detection_monthly_tightness_test = "DET_P_TT_MONTH",
  piping_release_detection_annual_tightness_test = "DET_P_TT_ANNUAL",
  piping_release_detection_triennial_tightness_test = "DET_P_TT_TRIEN",
  piping_release_detection_auto_line_leak_detector = "DET_P_LLD",
  piping_release_detection_sir_inventory_control = "DET_P_SIR",
  piping_release_detection_exempt_system_suction = "DET_P_SUCTION_EXEMPT"
)

present <- intersect(names(det_rename), names(ust_compartments))
setnames(ust_compartments, present, det_rename[present])
ust
# Classify detection methods
texas_compartment_det <- copy(ust_compartments)
texas_compartment_det[, (detect_cols) := lapply(.SD, function(x) x == "Y"), .SDcols = detect_cols]

# Create detection summary flags
texas_compartment_det[, `:=`(
  has_interstitial_det = DET_C_INTERSTITIAL | DET_C_SEC_CONT | DET_P_INTERSTITIAL | DET_P_SEC_CONT,
  has_electronic_det = DET_C_ATG | DET_P_LLD,
  has_manual_stat_det = DET_C_MANUAL_WEEK | DET_C_MANUAL_MONTH | DET_C_SIR | DET_P_SIR,
  has_any_det = rowSums(.SD) > 0
), .SDcols = detect_cols]

# Prepare detection data for merge
det_merge_cols <- c( "UST_ID", "has_interstitial_det", "has_electronic_det", 
                   "has_manual_stat_det", "has_any_det", detect_cols)
texas_compartment_det <- texas_compartment_det[, ..det_merge_cols]

# Ensure character types for merge
if("UST_ID" %in% names(texas_compartment_det)) {
  texas_compartment_det[, UST_ID := as.character(UST_ID)]
}

# Merge detection data
ust <- merge(ust, texas_compartment_det, by = c( "UST_ID"), all.x = TRUE)

# Fill missing detection flags
all_detection_cols <- c("has_interstitial_det", "has_electronic_det", 
                       "has_manual_stat_det", "has_any_det", detect_cols)
for(col in all_detection_cols) {
  ust[is.na(get(col)), (col) := FALSE]
}


### Count of USTs and Facilites that Should be in the panel: State 3 Left joined tank comparment data, no filtering 
  res03 <- run_stage01_validation(
    ust            = ust,
    min_panel_date = as.Date("1990-01-01"),
    max_panel_date = as.Date("2026-12-01"),
    stage_label    = "stage_after_classification"
  )

validateion_data <- bind_rows(
   validateion_data,
   res03)
sum(ust$is_gasoline, na.rm = TRUE)
sum(ust$is_diesel, na.rm = TRUE)


# Save processed UST data
saveRDS(ust, file = get_data_path("Outputs", "raw_pst_ust.rds"))
fwrite(ust, file = get_data_path("Outputs", "raw_pst_ust.csv"), row.names = FALSE)
setkey(ust,FACILITY_ID, UST_ID)

# 1. Create panel_months grid
# Use facility IDs from 'fac' as it's the basis for the main panel in Section 7
# Around line 1430, ensure panel is properly created:
min_panel_date <- as.Date("1990-01-01")
max_panel_date <- as.Date("2026-12-01")
panel_facility_ids <- unique(ust$FACILITY_ID) 
panel_months <- CJ(
  FACILITY_ID = panel_facility_ids,
  month_date = seq(floor_date(min_panel_date, "month"), floor_date(max_panel_date, "month"), by = "month")
)
panel_months[, YEAR := year(month_date)]
panel_months[, MONTH := month(month_date)]
setkey(panel_months, FACILITY_ID, month_date)

# Create the main panel from panel_months
panel <- panel_months[, .(FACILITY_ID, YEAR, MONTH)]

uniqueN(panel$FACILITY_ID) # Check how many unique facility IDs we have in the panel


message("Section 5 complete: UST processing finished with ", nrow(ust), " tanks")
# -----------------------------------------------------------------------------
# HARMONIZATION STEP: Create and Save TX_Harmonized_UST_tanks.csv
# -----------------------------------------------------------------------------
message("Creating Harmonized UST Tank dataset...")

# 1. Merge Facility Name and County from 'fac' (loaded in Section 1)
# Ensure fac is keyed or we use explicit 'by'
ust_harmonized <- merge(
  ust, 
  fac[, .(FACILITY_ID, FACILITY_NAME, SITE_COUNTY)], 
  by = "FACILITY_ID", 
  all.x = TRUE
)

# 2. Consolidate Wall Types
# Combine missing_walled and unknown_walled into a single unknown flag
ust_harmonized[, unknown_walled := pmax(unknown_walled, missing_walled, na.rm = TRUE)]

# 3. Select and Standardize Columns
TX_Harmonized_UST_tanks <- ust_harmonized[, .(
  facility_id = as.character(FACILITY_ID),
  facility_name = stringr::str_to_title(trimws(gsub("[^[:alnum:] ]", "", FACILITY_NAME))),
  tank_id = as.character(TANK_ID),
  state = "TX",
  
  # Dates: Use the raw parsed dates from Section 5
  tank_installed_date = INSTALL_DATE,
  
  # Logic: If status indicates closure (calculated in Section 5 via is_closed_removed), use STATUS_DATE
  tank_closed_date = fifelse(is_closed_removed, STATUS_DATE, as.Date(NA)),
  
  # Status: Keep the raw Texas status string as requested
  tank_status = STATUS,
  
  capacity = CAPACITY,
  
  # Wall Types (Binary)
  single_walled = as.integer(single_walled),
  double_walled = as.integer(double_walled),
  unknown_walled = as.integer(unknown_walled),
  
  # Substances (Binary) - calculated in Section 5B
  is_gasoline = as.integer(is_gasoline),
  is_diesel = as.integer(is_diesel),
  is_oil_kerosene = as.integer(is_oil_kerosene),
  is_jet_fuel = as.integer(is_jet_fuel),
  is_other = as.integer(is_other),
  
  # Location
  county_name = stringr::str_to_title(trimws(SITE_COUNTY))
)]

# 4. Save to State Database Folder
fwrite(TX_Harmonized_UST_tanks, file.path(tx_output_dir, "TX_Harmonized_UST_tanks.csv"))
message("Saved TX_Harmonized_UST_tanks.csv")


# ── Section 6: Monthly UST aggregations ─────────────────────────────────────
# Purpose: build full FACILITY×month grid (`panel_months`), non-equi join tanks to months,
#   then collapse to `ust_monthly` with counts, capacities, ages, wall-type proportions,
#   material/substance counts, detection & corrosion summaries, and upgrade events.
# Why: to quantify active tank inventory and attributes per facility per month.
# End product: `ust_monthly` keyed by FACILITY_ID, YEAR, MONTH
# Usage: merged into `panel` by FACILITY_ID, YEAR, MONTH in Section 7.

message("Processing Section 6: Creating monthly UST aggregations using UST_ID (non-equi join)...")

# 1. Use existing panel_months grid from Section 1
message("Using existing panel_months grid from Section 1...")

# 2. Define all detection and corrosion columns programmatically
detect_cols <- c(
  "DET_C_VAPOR", "DET_C_GW", "DET_C_SEC_CONT", "DET_C_ATG",
  "DET_C_INTERSTITIAL", "DET_C_MANUAL_WEEK", "DET_C_MANUAL_MONTH", "DET_C_SIR",
  "DET_P_VAPOR", "DET_P_GW", "DET_P_SEC_CONT", "DET_P_INTERSTITIAL",
  "DET_P_TT_MONTH", "DET_P_TT_ANNUAL", "DET_P_TT_TRIEN", "DET_P_LLD",
  "DET_P_SIR", "DET_P_SUCTION_EXEMPT"
)

corr_cols <- c("CORR_TANK_CP", "CORR_PIPE_CP", "CORR_TANK_VARIANCE", "CORR_PIPE_VARIANCE")

# 3. Pre-filter UST data to only relevant columns and date ranges
ust_cols_for_join <- c(
  "FACILITY_ID", "UST_ID", "install_date", "end_date", "CAPACITY", "STATUS", "STATUS_DATE", 
  "single_walled", "double_walled", "missing_walled", "unknown_walled",
  "is_steel_tank", "is_fiberglass_tank", "is_composite_tank", "is_concrete_tank", 
  "is_jacketed_tank", "is_coated_tank", "is_unknown_material_tank",
  "is_gasoline", "is_diesel", "is_oil_kerosene", "is_jet_fuel", "is_other",
  "has_interstitial_det", "has_electronic_det", "has_manual_stat_det", "has_any_det", 'is_closed_removed',
  detect_cols, corr_cols
)

# Ensure all selected columns exist in ust
ust_cols_for_join <- intersect(names(ust), ust_cols_for_join)

# Only keep USTs that could possibly be active during our panel period
ust_filtered <- ust[
  install_date <= max_panel_date | end_date >= min_panel_date,
  ..ust_cols_for_join
]

res04 <- run_stage01_validation(
  ust            = ust_filtered,
  min_panel_date = as.Date("1990-01-01"),
  max_panel_date = as.Date("2026-12-01"),
  stage_label    = "stage_after_ust_filtering_before_panel"
)

validateion_data <- bind_rows(
   validateion_data,
   res04)

message("Filtered UST data from ", nrow(ust), " to ", nrow(ust_filtered), " potentially active tanks")

# 4. Ensure optimal data types for join performance
ust_filtered[, `:=`(
  install_date = as.IDate(install_date),
  end_date = as.IDate(end_date),
  CAPACITY = as.numeric(CAPACITY)
)]
glimpse(ust_filtered)

# Convert character corrosion flags to logical before aggregation
char_corr_cols_to_logical <- c("CORR_TANK_CP", "CORR_PIPE_CP", "CORR_TANK_VARIANCE", "CORR_PIPE_VARIANCE")
for(col in char_corr_cols_to_logical) {
  if(col %in% names(ust_filtered) && is.character(ust_filtered[[col]])) {
    ust_filtered[, (col) := (get(col) == "Y")]
  } else if (col %in% names(ust_filtered) && !is.logical(ust_filtered[[col]])) {
    ust_filtered[, (col) := as.logical(as.character(get(col)) == "Y")]
  }
  if(col %in% names(ust_filtered)) ust_filtered[is.na(get(col)), (col) := FALSE]
}




# 5. Set optimal keys for the non-equi join
setkey(ust_filtered, FACILITY_ID, install_date, end_date)
setkey(panel_months, FACILITY_ID, month_date)

# 6. Perform optimized non-equi join to get active_tank_months
message("Performing optimized non-equi join...")
# ── Section 6: Monthly UST aggregations (pre-prefix removal) ─────────────────
message("Processing Section 6: Creating monthly UST aggregations…")



# (c) Expand each tank to facility×month, preserving x.* and i.* columns
active_tank_months <- ust_filtered[panel_months,
  on = .(
    FACILITY_ID,
    install_date <= month_date,
    end_date     >= month_date
  ),
  nomatch = 0L,
  allow.cartesian = TRUE,
  .(
    x.FACILITY_ID, x.UST_ID,
    i.YEAR, i.MONTH, i.month_date,
    x.install_date, x.end_date,x.STATUS_DATE,
    x.CAPACITY,

    x.single_walled, x.double_walled, x.missing_walled, x.unknown_walled,
    x.is_steel_tank, x.is_fiberglass_tank, x.is_composite_tank,
    x.is_concrete_tank, x.is_jacketed_tank, x.is_coated_tank,
    x.is_unknown_material_tank,

    x.is_gasoline, x.is_diesel, x.is_oil_kerosene, x.is_jet_fuel, x.is_other,
    x.has_interstitial_det, x.has_electronic_det, x.has_manual_stat_det, x.has_any_det,

    x.DET_C_VAPOR, x.DET_C_GW, x.DET_C_SEC_CONT, x.DET_C_ATG, x.DET_C_INTERSTITIAL,
    x.DET_C_MANUAL_WEEK, x.DET_C_MANUAL_MONTH, x.DET_C_SIR,

    x.DET_P_VAPOR, x.DET_P_GW, x.DET_P_SEC_CONT, x.DET_P_INTERSTITIAL, x.DET_P_TT_MONTH,
    x.DET_P_TT_ANNUAL, x.DET_P_TT_TRIEN, x.DET_P_LLD, x.DET_P_SIR, x.DET_P_SUCTION_EXEMPT,

    x.CORR_TANK_CP, x.CORR_PIPE_CP, x.CORR_TANK_VARIANCE, x.CORR_PIPE_VARIANCE,

    x.is_closed_removed
  )
]

# (d) Strip the x./i. prefixes in one go
old_names <- grep("^(x\\.|i\\.)", names(active_tank_months), value = TRUE)
new_names <- sub("^(?:x\\.|i\\.)", "", old_names)
setnames(active_tank_months, old = old_names, new = new_names)
setnames(active_tank_months, "STATUS_DATE", "tank_closed_date_raw")
## A.  Facilities derived from the raw / filtered tank table
fac_from_ust  <- unique(ust_filtered$FACILITY_ID)

## B.  Facilities that actually make it into the month-panel
fac_from_panel <- unique(active_tank_months$FACILITY_ID)

length(fac_from_ust)      # how many should be in the panel
length(fac_from_panel)    # how many you actually have


# 8. Pre-compute age calculation to avoid doing it in aggregation

active_tank_months[, `:=`(
  month_date   = as.IDate(month_date),   # ensure IDate
  install_date = as.IDate(install_date)  # ditto
)]

active_tank_months[
  , tank_age_months := {
      iv <- interval(install_date, month_date)
      full_months <- iv %/% months(1)
      fifelse(full_months >= 0, full_months, NA_integer_)
  }
]


########
res05 <- run_stage01_validation(
  ust            = active_tank_months,
  min_panel_date = as.Date("1990-01-01"),
  max_panel_date = as.Date("2026-12-01"),
  stage_label    = "stage_after_monthly_expansion"
)

validateion_data <- bind_rows(
   validateion_data,
   res05)

#save the ust-faciliyt-monthly panel
fwrite(active_tank_months, file = get_data_path("Outputs", "texas_ust_facility_month_panel.csv"))


sum(active_tank_months$is_gasoline, na.rm = TRUE) # Check how many gasoline tanks we have

# 9. Perform facility-month aggregation with all required variables
message("Performing facility-month aggregation...")

ust_monthly <- active_tank_months[, .(
  # Earliest install date and latest end date
  earliest_install_date = min(install_date, na.rm = TRUE),
  latest_end_date = max(end_date, na.rm = TRUE),

  # Core counts and capacity
  active_tank_count = .N,  # More efficient than uniqueN when UST_ID is unique per row
  total_capacity = sum(CAPACITY, na.rm = TRUE),
  
  # Age and proportions
  avg_tank_age_months = mean(tank_age_months, na.rm = TRUE),
  prop_single_wall = mean(single_walled, na.rm = TRUE),
  prop_double_wall = mean(double_walled, na.rm = TRUE),
  
  # Wall type counts
  num_sw_tanks = sum(single_walled, na.rm = TRUE),
  num_dw_tanks = sum(double_walled, na.rm = TRUE),
  num_missing_tanks = sum(missing_walled, na.rm = TRUE),
  num_unknown_tanks = sum(unknown_walled, na.rm = TRUE),
  
  # Material counts
  num_steel_tanks = sum(is_steel_tank, na.rm = TRUE),
  num_fiberglass_tanks = sum(is_fiberglass_tank, na.rm = TRUE),
  num_composite_tanks = sum(is_composite_tank, na.rm = TRUE),
  num_concrete_tanks = sum(is_concrete_tank, na.rm = TRUE),
  num_jacketed_tanks = sum(is_jacketed_tank, na.rm = TRUE),
  num_coated_tanks = sum(is_coated_tank, na.rm = TRUE),
  num_unknown_material_tanks = sum(is_unknown_material_tank, na.rm = TRUE),
  
  # Substance counts
  num_gasoline_tanks = sum(is_gasoline > 0, na.rm = TRUE),
  num_diesel_tanks = sum(is_diesel > 0, na.rm = TRUE),
  num_oil_kerosene_tanks = sum(is_oil_kerosene > 0, na.rm = TRUE),
  num_jet_fuel_tanks = sum(is_jet_fuel > 0, na.rm = TRUE),
  num_other_substance_tanks = sum(is_other > 0, na.rm = TRUE),
  total_tanks_with_substance_info = sum(is_gasoline > 0 | is_diesel > 0 | is_oil_kerosene > 0 | 
                                       is_jet_fuel > 0 | is_other > 0, na.rm = TRUE),
  
  # Detection summary counts
  num_interstitial_det = sum(has_interstitial_det, na.rm = TRUE),
  num_electronic_det = sum(has_electronic_det, na.rm = TRUE),
  num_manual_stat_det = sum(has_manual_stat_det, na.rm = TRUE),
  num_any_det = sum(has_any_det, na.rm = TRUE),
  
  # Individual detection method counts (compartment)
  num_DET_C_VAPOR = sum(DET_C_VAPOR, na.rm = TRUE),
  num_DET_C_GW = sum(DET_C_GW, na.rm = TRUE),
  num_DET_C_SEC_CONT = sum(DET_C_SEC_CONT, na.rm = TRUE),
  num_DET_C_ATG = sum(DET_C_ATG, na.rm = TRUE),
  num_DET_C_INTERSTITIAL = sum(DET_C_INTERSTITIAL, na.rm = TRUE),
  num_DET_C_MANUAL_WEEK = sum(DET_C_MANUAL_WEEK, na.rm = TRUE),
  num_DET_C_MANUAL_MONTH = sum(DET_C_MANUAL_MONTH, na.rm = TRUE),
  num_DET_C_SIR = sum(DET_C_SIR, na.rm = TRUE),
  
  # Individual detection method counts (piping)
  num_DET_P_VAPOR = sum(DET_P_VAPOR, na.rm = TRUE),
  num_DET_P_GW = sum(DET_P_GW, na.rm = TRUE),
  num_DET_P_SEC_CONT = sum(DET_P_SEC_CONT, na.rm = TRUE),
  num_DET_P_INTERSTITIAL = sum(DET_P_INTERSTITIAL, na.rm = TRUE),
  num_DET_P_TT_MONTH = sum(DET_P_TT_MONTH, na.rm = TRUE),
  num_DET_P_TT_ANNUAL = sum(DET_P_TT_ANNUAL, na.rm = TRUE),
  num_DET_P_TT_TRIEN = sum(DET_P_TT_TRIEN, na.rm = TRUE),
  num_DET_P_LLD = sum(DET_P_LLD, na.rm = TRUE),
  num_DET_P_SIR = sum(DET_P_SIR, na.rm = TRUE),
  num_DET_P_SUCTION_EXEMPT = sum(DET_P_SUCTION_EXEMPT, na.rm = TRUE),
  
  # Corrosion counts
  num_corr_tank_cp = sum(CORR_TANK_CP, na.rm = TRUE),
  num_corr_pipe_cp = sum(CORR_PIPE_CP, na.rm = TRUE),
  num_corr_tank_var = sum(CORR_TANK_VARIANCE, na.rm = TRUE),
  num_corr_pipe_var = sum(CORR_PIPE_VARIANCE, na.rm = TRUE),
   # **NEW**: count tanks that actually closed in this month
closed_tank_count = sum(is_closed_removed & 
                        floor_date(end_date, "month") == month_date, 
                        na.rm=TRUE)  
), by = .(FACILITY_ID, YEAR, MONTH)]


# 10. Handle edge cases and add derived flags
ust_monthly[, `:=`(
  # Handle NaN ages
  avg_tank_age_months = fifelse(is.nan(avg_tank_age_months), NA_real_, avg_tank_age_months),
  
  # Handle zero tank cases for proportions
  prop_single_wall = fifelse(active_tank_count == 0, NA_real_, prop_single_wall),
  prop_double_wall = fifelse(active_tank_count == 0, NA_real_, prop_double_wall),
  
  # Has tank flags (based on counts)
  has_gasoline_tank = as.integer(num_gasoline_tanks > 0),
  has_diesel_tank = as.integer(num_diesel_tanks > 0),
  has_oil_kerosene_tank = as.integer(num_oil_kerosene_tanks > 0),
  has_jet_fuel_tank = as.integer(num_jet_fuel_tanks > 0),
  has_other_substance_tank = as.integer(num_other_substance_tanks > 0),
  has_corr_deficiency = as.integer(num_corr_tank_var > 0 | num_corr_pipe_var > 0)
)]

# 11. Efficient double-wall upgrade detection
setkey(ust_monthly, FACILITY_ID, YEAR, MONTH)
ust_monthly[, `:=`(
  lag_dw_tanks = shift(num_dw_tanks, type = "lag"),
  lag_other_tanks = shift(num_sw_tanks + num_unknown_tanks + num_missing_tanks, type = "lag")
), by = FACILITY_ID]

ust_monthly[, dw_upgrade_event := as.integer(
  !is.na(lag_dw_tanks) & lag_dw_tanks < num_dw_tanks & 
  !is.na(lag_other_tanks) & lag_other_tanks > (num_sw_tanks + num_unknown_tanks + num_missing_tanks)
)]

# Clean up temporary columns
ust_monthly[, c("lag_dw_tanks", "lag_other_tanks") := NULL]
ust_monthly[is.na(dw_upgrade_event), dw_upgrade_event := 0L]

# # 12. Final cleanup and save
rm(ust_filtered, active_tank_months)  # Free memory
 gc()  # Force garbage collection

setkey(ust_monthly, FACILITY_ID, YEAR, MONTH)
message("Optimized ust_monthly built: ", format(nrow(ust_monthly), big.mark=","), " rows")

# 1. Last active month per facility
last_active <- ust_monthly[active_tank_count > 0,
  .(Last_YearMonth = max(make_date(YEAR, MONTH, 1)) ),
  by = FACILITY_ID
]

ust_monthly <- merge(ust_monthly, last_active,
                     by = "FACILITY_ID", all.x = TRUE)

# 1a. Extract the components
ust_monthly[
  , `:=`(
      closure_year  = year(Last_YearMonth),
      closure_month = month(Last_YearMonth)
  )
]

# 1b. Now you can safely refer to those new columns
ust_monthly[
  , is_active_pre_clos := as.integer(
      !is.na(Last_YearMonth) &
      (YEAR <  closure_year |
       (YEAR == closure_year & MONTH <= closure_month))
  )
]

# 3. Entry/exit events
setorder(ust_monthly, FACILITY_ID, YEAR, MONTH)
ust_monthly[, `:=`(
  is_facility_entry_event = as.integer(
    active_tank_count > 0 & shift(active_tank_count, fill = 0L) == 0L
  ),
  is_facility_exit_event  = as.integer(
    is_active_pre_clos == 0L & shift(is_active_pre_clos, fill = 1L) == 1L
  )
), by = FACILITY_ID]

# 4. Clean up any NAs
for(col in c("is_facility_entry_event","is_facility_exit_event")){
  ust_monthly[is.na(get(col)), (col) := 0L]
}

message("  → Done: ust_monthly now has closed_tank_count, inventory-closure & entry/exit flags.")
# Save for debugging
fwrite(ust_monthly, get_data_path("Outputs", "texas_facility_monthly_ust_aggregations_optimized.csv"))

uniqueN(ust_monthly$FACILITY_ID) # Check how many unique facility IDs we have in the monthly panel


message("Section 6: Monthly UST aggregations complete (optimized version).")

# ── Section 7: Main panel build & merging ────────────────────────────────────
# Purpose: start from skeleton panel of FACILITY_ID×YEAR×MONTH,
#   then left-merge in FA (`fa_monthly`), facility info (`fac`), contact months (`fac_contact_months`),
#   UST metrics (`ust_monthly`), self-cert (`self_cert_monthly`), LUST, Zurich flags, capacity & event deltas.
# Why: to assemble a unified monthly panel for all subsequent analysis.
# End product: `panel` keyed by FACILITY_ID, YEAR, MONTH ready to save.




# ... (previous sections of the script remain unchanged) ...

# ── Section 7: Staging & Conditional Panel Build ─────────────────────────────
# Purpose: Stage all intermediate data tables to disk for modularity and memory
#   management. Then, conditionally, read them back to perform all merges and
#   build the final analysis panel.
# Why: Allows for separate execution of data generation and panel assembly.
#   Reduces memory pressure by writing/reading/removing large objects.
# End product: Staged data in Outputs/ or a complete `panel` object saved to disk.

section7_start <- Sys.time()
message("Processing Section 7: Staging intermediate data...")

###############################################################################
# 7.1 QUICK-STAGE ALL PRE-MERGE TABLES TO DISK
###############################################################################
stage_dir <- get_data_path("Outputs", "panel_merge_staging")
if (!dir.exists(stage_dir)) dir.create(stage_dir, recursive = TRUE)

fac_info <- fac[ , .(
  FACILITY_ID, FACILITY_NAME,
  FACILITY_TYPE, FACILITY_STATUS,
  SITE_COUNTY, SITE_ZIP,
  NUM_ACTIVE_USTS_is_blank,
  NUM_ACTIVE_ASTS_is_blank
)]

datasets_to_stage <- list(
  panel               = panel,
  fa_monthly          = fa_monthly,
  fac_info            = fac_info,          # now it really exists
  fac_contact_months  = fac_contact_months,
  ust_monthly         = ust_monthly,
  zurich_2012_lookup  = zurich_2012_lookup,
  self_cert_monthly   = self_cert_monthly,
  fa_monthly_contract = fa_monthly_contract
)

# original loop with get() will now work
for (nm in names(datasets_to_stage)) {
  dt <- get(nm) # Get the object from the environment
  rds_path <- file.path(stage_dir, paste0(nm, ".rds"))
  fwrite(dt, file.path(stage_dir, paste0(nm, ".csv")))
  saveRDS(dt, rds_path)
  message("Staged ", nm, " -> ", basename(rds_path))
}

# Memory Hygiene
rm(list = names(datasets_to_stage))
rm(fac, fa, self_cert, ust, ust_compartments, texas_compartment_classified, texas_compartment_det) # remove large raw objects
gc(verbose = FALSE)
message("Cleaned pre-merge objects from memory.")



###############################################################################
# 7.2 LUST PROCESSING & STAGING
###############################################################################
message("Processing LUST data for staging...")

# Load and preprocess LUST data efficiently
TX_LUST_SD <- fread(get_data_path("Raw", "state_databases", "Texas", "TX_LUST.csv"))


if ("facility_id" %in% names(TX_LUST_SD)) setnames(TX_LUST_SD, "facility_id", "FACILITY_ID")
TX_LUST_SD <- clean_id_column(TX_LUST_SD, "FACILITY_ID")

# Parse Dates
TX_LUST_SD[, `:=`(
  report_date = dmy(report_date),
  nfa_date = dmy(nfa_date)
)]
# -----------------------------------------------------------------------------
# HARMONIZATION STEP: Create and Save TX_Harmonized_LUST.csv
# -----------------------------------------------------------------------------


message("Creating Harmonized LUST dataset...")
TX_Harmonized_LUST <- TX_LUST_SD[, .(
  facility_id = as.character(FACILITY_ID),
  lust_id = as.character(LUST_id),
  report_date = as.Date(report_date),
  nfa_date = as.Date(nfa_date),
  state = "TX"
)]


# Continue with Panel specific filtering...
TX_LUST_SD <- TX_LUST_SD[
  !is.na(report_date) &
  report_date >= as.Date("1990-01-01") &
  report_date <= as.Date("2026-12-31") &
  FACILITY_ID %in% facilities_active_1990$FACILITY_ID
]
message("LUST records filtered to ", nrow(TX_LUST_SD), " relevant records")

fwrite(TX_Harmonized_LUST, file.path(tx_output_dir, "TX_Harmonized_LUST.csv"))
message("Saved TX_Harmonized_LUST.csv")


expand_lust_optimized <- function(lust_dt) {
  lust_dt[!is.na(report_date), {
    start_month <- floor_date(report_date, "month")
    end_month <- floor_date(fifelse(is.na(nfa_date), as.Date("2026-12-31"), nfa_date), "month")
    if(start_month <= end_month) {
      month_seq <- seq(start_month, end_month, by = "month")
      .(YEAR = year(month_seq), MONTH = month(month_seq), lust_active = TRUE,
        is_pre2000 = report_date < as.Date("2000-01-01"),
        has_subsidy = report_date < as.Date("2000-01-01"),
        nfa_month = end_month)
    }
  }, by = .(FACILITY_ID, LUST_id)]
}

lust_report_months <- TX_LUST_SD[!is.na(report_date), .(
  FACILITY_ID, YEAR = year(report_date), MONTH = month(report_date), is_lust_report_month = 1L
)]
lust_long <- expand_lust_optimized(TX_LUST_SD)
lust_monthly_status <- lust_long[, .(
  lust_active = as.integer(any(lust_active)),
  lust_pre_2000_active = as.integer(any(lust_active & is_pre2000)),
  lust_post_2000_active = as.integer(any(lust_active & !is_pre2000)),
  has_subsidized_leak_active = as.integer(any(lust_active & has_subsidy)),
  lust_open_active = as.integer(any(lust_active & (nfa_month > make_date(YEAR, MONTH, 1)))),
  lust_closed_active = as.integer(any(lust_active & (nfa_month <= make_date(YEAR, MONTH, 1))))
), by = .(FACILITY_ID, YEAR, MONTH)]
facility_lust_summary <- TX_LUST_SD[, .(
  lust_ever = 1L, lust_pre_2000 = as.integer(any(report_date < "2000-01-01")),
  lust_post_2000 = as.integer(any(report_date >= "2000-01-01")),
  has_subsidized_leak = as.integer(any(report_date < "2000-01-01")),
  lust_ever_closed = as.integer(any(!is.na(nfa_date))),
  lust_ever_open = as.integer(any(is.na(nfa_date))),
  lust_after_closure_ever = as.integer(any(!is.na(nfa_date) & nfa_date < as.Date("2026-12-31") & nfa_date > report_date))
), by = FACILITY_ID]

# QUICK-STAGE LUST TABLES TO DISK
lust_stage_dir <- get_data_path("Outputs", "lust_merge_staging")
if (!dir.exists(lust_stage_dir)) dir.create(lust_stage_dir, recursive = TRUE)

lust_to_stage <- list(
  lust_report_months    = lust_report_months,
  lust_monthly_status   = lust_monthly_status,
  facility_lust_summary = facility_lust_summary
)

for (nm in names(lust_to_stage)) {
  dt <- get(nm)
  rds_path <- file.path(lust_stage_dir, paste0(nm, ".rds"))
  fwrite(dt, file.path(lust_stage_dir, paste0(nm, ".csv")))
  saveRDS(dt, rds_path)
  message("Staged ", nm, " -> ", basename(rds_path))
}

# Memory Hygiene
rm(list = names(lust_to_stage))
rm(TX_LUST_SD, lust_long)
gc(verbose = FALSE)
message("Cleaned LUST objects from memory.")

# ── Section 8: Census data processing & staging ──────────────────────────────
message("Processing Section 8: Building and staging census data...")
library(fredr); library(data.table)
###############################################################################
# 1. Meta: keep a tidy lookup of variable ↔ units for future reference
###############################################################################
units_map <- data.table(
  variable = c("total_pop", "median_hh_income", "gini_index"),
  units    = c("persons",  "US dollars (nominal-year)", "unitless index (0–1)")
)
expand_5yr_bin <- function(dt, end_year_col = "survey_end_year", value_cols) {
  dt_long <- melt(dt,id.vars= c("fips", end_year_col),measure.vars = value_cols,variable.name = "var",value.name= "value")
  dt_long[, year := as.integer(get(end_year_col))]
  dt_long <- dt_long[, .(year= rep((year - 4):year, each = .N / length(value_cols)),fips,var,value), by = .I]
  dcast(dt_long, fips + year ~ var, value.var = "value")
}
reshape_pop <- function(url, years) {
  cols <- c("STATE", "COUNTY", paste0("POPESTIMATE", years))
  dt   <- fread(url, select = cols, colClasses = "character", na.strings = "(X)")
  dt   <- dt[STATE == "48"]
  dt[, fips := sprintf("%02d%03d", as.integer(STATE), as.integer(COUNTY))]
  long <- melt(dt, id.vars = "fips", measure.vars = patterns("^POPESTIMATE"), variable.name = "year_str", value.name = "total_pop", na.rm = TRUE)
  long[, year := as.integer(sub("POPESTIMATE", "", year_str))]
  long[, .(fips, year, total_pop = as.numeric(total_pop))]
}
pop_2010_2020 <- reshape_pop(url= "https://www2.census.gov/programs-surveys/popest/datasets/2010-2020/counties/totals/co-est2020-alldata.csv",years = 2010:2020)
pop_2000_2009 <- reshape_pop(url= "https://www2.census.gov/programs-surveys/popest/datasets/2000-2009/counties/totals/co-est2009-alldata.csv",years = 2000:2009)
peek_icen_year <- function(yr, state_fips = "48", n_preview = 5) {
  stopifnot(yr %in% 1990:1999)
  url <- sprintf("https://www2.census.gov/programs-surveys/popest/tables/1990-2000/intercensal/st-co/stch-icen%d.txt",yr)
  raw <- fread(url, header = FALSE, colClasses = "character", showProgress = FALSE)
  setnames(raw, paste0("V", seq_len(ncol(raw))))
  message(sprintf("\nPreview of %s (first %d rows):", basename(url), n_preview)); print(head(raw, n_preview))
  tx <- raw[substr(V2, 1, 2) == state_fips & V3 == "0" & V4 == "1" & V5 == "1",.(fips= V2,year= yr,total_pop = as.numeric(V6))]
  message(sprintf("Returned %d county rows for Texas, %d", nrow(tx), yr)); return(tx)
}
pop_1998 <- peek_icen_year(1998)
pop_1999 <- peek_icen_year(1999)
pop_dt <- rbindlist(list(pop_1998, pop_1999, pop_2000_2009, pop_2010_2020),use.names = TRUE)[order(fips, year)]
message(sprintf("Pop table: %s rows, %s-%s", nrow(pop_dt),min(pop_dt$year), max(pop_dt$year)))
fredr_set_key("2d9a1f281713753ddce9c4250540c2aa")
tx_fips <- as.data.table(fips_codes)[state == "TX"]; tx_fips[ , fips := paste0(state_code, county_code)]; tx_fips <- unique(tx_fips[ , .(fips)])
if (!"48000" %in% tx_fips$fips) {tx_fips <- rbind(tx_fips, data.table(fips = "48000"))}
print(head(tx_fips, 6))
tx_fips[, series_id := paste0("MHITX", fips, "A052NCEN")]
tail(tx_fips)
get_mhi <- function(id, fips_code) {
  Sys.sleep(0.5)
  tryCatch({
    obs <- fredr_series_observations(series_id= id,observation_start = as.Date("1989-01-01"),units= "lin")
    obs <- as.data.table(obs)
    if (nrow(obs) == 0) return(NULL)
    obs[ , .(fips= fips_code,year= year(date),median_hh_income = value)]
  }, error = function(e) {
    warning(sprintf("Missing series %s (%s): %s", id, fips_code, e$message)); NULL
  })
}
income_dt <- rbindlist(Map(get_mhi, tx_fips$series_id, tx_fips$fips),use.names = TRUE, fill = TRUE)
income_dt[ , median_hh_income_units := "US dollars (nominal-year)"]
message("Fetched income data for ", uniqueN(income_dt[!is.na(median_hh_income), fips]), " counties.")
message("Years range from: ", min(income_dt$year, na.rm = TRUE), " to ", max(income_dt$year, na.rm = TRUE)); print(head(income_dt))
message("Downloading & expanding ACS Gini …")
acs_end_years <- 2010:2023
gini_list <- lapply(acs_end_years, function(ey) {
  acs_tbl <- get_acs(geography = "county",state= "TX",table= "B19083",year= ey,survey= "acs5",output= "wide")
  setDT(acs_tbl)
  acs_tbl[ , .(fips= GEOID,gini_index= B19083_001E,survey_end_year = ey)]
})
gini_wide <- expand_5yr_bin(rbindlist(gini_list),end_year_col = "survey_end_year",value_cols= "gini_index")
census_data_panel <- CJ(fips = unique(pop_dt$fips), year = 1998:2020)
census_data_panel <- merge(census_data_panel, pop_dt,    by = c("fips", "year"), all.x = TRUE)
census_data_panel <- merge(census_data_panel, income_dt, by = c("fips", "year"), all.x = TRUE)
census_data_panel <- merge(census_data_panel, gini_wide, by = c("fips", "year"), all.x = TRUE)
setnames(census_data_panel, "year", "YEAR")
census_data_panel[ , `:=`(total_pop_units= "persons",median_hh_income_units = "US dollars (nominal-year)",gini_index_units= "unitless index (0–1)")]
if (any(census_data_panel$total_pop < 10000, na.rm = TRUE) & any(census_data_panel$total_pop > 0, na.rm = TRUE)) {
  median_pop <- median(census_data_panel$total_pop, na.rm = TRUE)
  if (!is.na(median_pop) && median_pop < 10000) {
    warning("Median total_pop is low; values may be in thousands. Rescaling to persons.")
    census_data_panel[!is.na(total_pop), total_pop := total_pop * 1000]
  } else if (max(census_data_panel$total_pop, na.rm = TRUE) < 20000 && any(census_data_panel$total_pop > 0, na.rm = TRUE)) {
     warning("Maximum total_pop is low; values may be in thousands. Rescaling to persons.")
     census_data_panel[!is.na(total_pop), total_pop := total_pop * 1000]
  }
}
census_data_panel[order(fips, YEAR),gini_index := zoo::na.locf(gini_index, na.rm = FALSE), by = fips]
census_data_panel[order(fips, YEAR),gini_index := zoo::na.locf(gini_index, na.rm = FALSE, fromLast = TRUE), by = fips]
cpi_dt <- as.data.table(fredr(series_id= "CPIAUCSL",observation_start= as.Date("1998-01-01"),observation_end= as.Date("2023-12-01"),frequency= "a"))[ , .(YEAR = year(date), CPI = value) ]
cpi_2023 <- cpi_dt[YEAR == 2023, CPI]
cpi_dt[ , cpi_factor := cpi_2023 / CPI ]
census_data_panel <- merge(census_data_panel, cpi_dt[ , .(YEAR, cpi_factor)], by = "YEAR", all.x = TRUE)
census_data_panel[ , median_hh_income_real_2023 := median_hh_income * cpi_factor ]
census_data_panel[ ,median_hh_income_real_2023_units := "2023 dollars (CPI-U deflated)"]
units_map <- rbind(units_map,data.table(variable = "median_hh_income_real_2023",units= "2023 dollars (CPI-U deflated)"))
###############################################################################

# QUICK-STAGE CENSUS TABLES TO DISK
census_stage_dir <- get_data_path("Outputs", "census_merge_staging")
if (!dir.exists(census_stage_dir)) dir.create(census_stage_dir, recursive = TRUE)

census_to_stage <- list(
  pop_dt             = pop_dt,
  income_dt          = income_dt,
  gini_wide          = gini_wide,
  cpi_dt             = cpi_dt,
  census_data_panel  = census_data_panel,
  units_map          = units_map
)

for (nm in names(census_to_stage)) {
  dt <- get(nm)
  rds_path <- file.path(census_stage_dir, paste0(nm, ".rds"))
  fwrite(dt, file.path(census_stage_dir, paste0(nm, ".csv")))
  saveRDS(dt, rds_path)
  message("Staged ", nm, " -> ", basename(rds_path))
}

# Memory Hygiene
rm(list = names(census_to_stage))
gc(verbose = FALSE)
message("Cleaned census objects from memory.")

###############################################################################
# ── Section 9: Conditional Full Merge & Finalization ───────────────────────
###############################################################################

if (do_merge) {
  message("\n--- Full Merge Initiated (do_merge = TRUE) ---")

  # --- 1. Load staged data required for the panel build ---
  message("Loading staged data from disk...")
  panel_stage_dir <- get_data_path("Outputs", "panel_merge_staging")
  lust_stage_dir <- get_data_path("Outputs", "lust_merge_staging")
  census_stage_dir <- get_data_path("Outputs", "census_merge_staging")

  panel                 <- readRDS(file.path(panel_stage_dir, "panel.rds"))
  fa_monthly            <- readRDS(file.path(panel_stage_dir, "fa_monthly.rds"))
  fa_monthly_contract   <- readRDS(file.path(panel_stage_dir, "fa_monthly_contract.rds"))
  
  message("All staged data needed for initial merge loaded successfully.")

  # --- 2. Perform merges and processing sequentially ---
  message("Merging Financial Assurance data (fa_monthly)...")
  setkey(panel, FACILITY_ID, YEAR, MONTH)
  setkey(fa_monthly, FACILITY_ID, YEAR, MONTH)
  panel <- merge(panel, fa_monthly, by = c("FACILITY_ID", "YEAR", "MONTH"), all.x = TRUE)
  rm(fa_monthly); gc()

  # --- 2.1. Gap Detection, ERP Logic, and Change Indicators ---
  message("Processing gap detection and change indicators after FA merge...")
  panel[is.na(fr_covered), fr_covered := FALSE]

  message("Calculating ERP coverage...")
  expired_contracts <- fa_monthly_contract[!is.na(EXP_DATE), .(FACILITY_ID, FIN_ASSUR_ID, EXP_DATE, erp_start = floor_date(EXP_DATE + months(1), "month"), erp_end = floor_date(EXP_DATE + months(6), "month"))][!is.na(erp_start) & !is.na(erp_end)]
  expired_contracts <- unique(expired_contracts, by = c("FACILITY_ID", "FIN_ASSUR_ID"))
  erp_monthly <- expired_contracts[, {erp_months <- seq(erp_start, erp_end, by = "month"); .(YEAR = year(erp_months), MONTH = month(erp_months), erp_coverage = TRUE)}, by = .(FACILITY_ID, FIN_ASSUR_ID)]
  erp_facility_monthly <- erp_monthly[, .(erp_reporting_month = any(erp_coverage, na.rm = TRUE)), by = .(FACILITY_ID, YEAR, MONTH)]
  
  panel <- merge(panel, erp_facility_monthly, by = c("FACILITY_ID", "YEAR", "MONTH"), all.x = TRUE)
  panel[is.na(erp_reporting_month), erp_reporting_month := FALSE]
  rm(fa_monthly_contract, expired_contracts, erp_monthly, erp_facility_monthly); gc()

  panel[, `:=`(coverage_gap_month = (!fr_covered) & (!erp_reporting_month), ISSUER_NAME = fifelse((!fr_covered) & (!erp_reporting_month), "NO COVERAGE", ISSUER_NAME), DETAIL_TYPE = fifelse((!fr_covered) & (!erp_reporting_month), "NO COVERAGE", DETAIL_TYPE), CATEGORY = fifelse((!fr_covered) & (!erp_reporting_month), "NO COVERAGE", CATEGORY))]
  panel[YEAR < 1999, `:=`(ISSUER_NAME = "State Fund", DETAIL_TYPE = "State Insurance", CATEGORY = "State Fund")]
  
  setkey(panel, FACILITY_ID, YEAR, MONTH)
  panel[, `:=`(prev_issuer = shift(ISSUER_NAME, type = "lag"), prev_category = shift(CATEGORY, type = "lag"), prev_detail_type = shift(DETAIL_TYPE, type = "lag"), prev_fr_covered = shift(fr_covered, type = "lag"), prev_coverage_gap = shift(coverage_gap_month, type = "lag")), by = FACILITY_ID][, `:=`(issuer_changed = (!is.na(prev_issuer) & !is.na(ISSUER_NAME) & ISSUER_NAME != prev_issuer), mech_changed = (!is.na(prev_category) & !is.na(CATEGORY) & CATEGORY != prev_category), detail_type_changed = (!is.na(prev_detail_type) & !is.na(DETAIL_TYPE) & DETAIL_TYPE != prev_detail_type))][, `:=`(contract_changed = (issuer_changed | mech_changed | detail_type_changed), changed_to_self_insure = (mech_changed & grepl("Self-Insurance", CATEGORY, ignore.case = TRUE) & !is.na(prev_category) & !grepl("Self-Insurance", prev_category, ignore.case = TRUE)), changed_to_insurance = (mech_changed & grepl("Insurance", CATEGORY, ignore.case = TRUE) & !is.na(prev_category) & !grepl("Insurance", prev_category, ignore.case = TRUE)), changed_to_no_coverage = (coverage_gap_month == TRUE & !is.na(prev_coverage_gap) & prev_coverage_gap == FALSE), changed_from_no_coverage = (coverage_gap_month == FALSE & !is.na(prev_coverage_gap) & prev_coverage_gap == TRUE))]
  panel[, c("prev_issuer", "prev_category", "prev_detail_type", "prev_fr_covered", "prev_coverage_gap") := NULL]
  message("Gap detection and change indicators complete.")

  # --- 2.2. Batch Data Merges ---
  message("Performing batch data merges...")
  fac_info <- readRDS(file.path(panel_stage_dir, "fac_info.rds")); panel <- merge(panel, fac_info, by = "FACILITY_ID", all.x = TRUE); rm(fac_info); gc(); message("Merged facility information.")
  fac_contact_months <- readRDS(file.path(panel_stage_dir, "fac_contact_months.rds")); panel <- merge(panel, fac_contact_months, by = c("FACILITY_ID", "YEAR", "MONTH"), all.x = TRUE); rm(fac_contact_months); gc(); message("Merged contact data.")
  ust_monthly <- readRDS(file.path(panel_stage_dir, "ust_monthly.rds")); panel <- merge(panel, ust_monthly, by = c("FACILITY_ID", "YEAR", "MONTH"), all.x = TRUE); rm(ust_monthly); gc(); message("Merged UST metrics.")
  zurich_2012_lookup <- readRDS(file.path(panel_stage_dir, "zurich_2012_lookup.rds")); panel <- merge(panel, zurich_2012_lookup, by = "FACILITY_ID", all.x = TRUE); rm(zurich_2012_lookup); gc(); message("Merged Zurich lookup.")
  self_cert_monthly <- readRDS(file.path(panel_stage_dir, "self_cert_monthly.rds")); panel <- merge(panel, self_cert_monthly, by = c("FACILITY_ID", "YEAR", "MONTH"), all.x = TRUE); rm(self_cert_monthly); gc(); message("Merged self-certification.")

  # --- 2.3. Post-Merge Processing ---
  message("Adding FIPS codes and other derived variables...")
  data("fips_codes"); tx_county_fips <- as.data.table(fips_codes)[state == "TX", .(county_clean = toupper(gsub("\\s*County$", "", county)), fips = paste0(state_code, county_code))]; tx_county_fips <- unique(tx_county_fips, by = c("county_clean", "fips"))
  panel[, SITE_COUNTY_upper := toupper(SITE_COUNTY)]; panel <- merge(panel, tx_county_fips[, .(SITE_COUNTY_upper = county_clean, fips)], by = "SITE_COUNTY_upper", all.x = TRUE); panel[, SITE_COUNTY_upper := NULL]
  
  panel[, any_tank_active := active_tank_count > 0]
  panel[, `:=`(is_facility_entry_event = any_tank_active & !shift(any_tank_active, type="lag", fill=FALSE), is_facility_exit_event  = !any_tank_active & shift(any_tank_active, type="lag", fill=FALSE)), by = FACILITY_ID]
  
  panel[, `:=`(had_zurich_2012 = fifelse(is.na(had_zurich_2012), 0L, had_zurich_2012), dropped_by_zurich = had_zurich_2012 * as.integer(YEAR >= 2013))]
  
  change_metrics <- c("total_capacity", "num_interstitial_det", "num_electronic_det", "num_manual_stat_det", "num_steel_tanks", "num_fiberglass_tanks", "num_jacketed_tanks")
  lag_cols <- paste0("lag_", change_metrics); panel[, (lag_cols) := lapply(.SD, shift, type = "lag"), .SDcols = change_metrics, by = FACILITY_ID]
  delta_cols <- paste0(change_metrics, "_delta"); event_cols <- paste0(change_metrics, "_change_event")
  for (i in seq_along(change_metrics)) {
    orig_col <- change_metrics[i]; lag_col <- lag_cols[i]; delta_col <- delta_cols[i]; event_col <- event_cols[i]
    panel[, (delta_col) := fifelse(is.na(get(lag_col)), 0, get(orig_col) - get(lag_col))]; panel[, (event_col) := as.integer(!is.na(get(lag_col)) & get(orig_col) != get(lag_col))]
  }
  panel[, (lag_cols) := NULL]; setnames(panel, "total_capacity_delta", "capacity_delta_gal"); setnames(panel, "total_capacity_change_event", "capacity_change_event")
  
  # --- 2.4. LUST Data Merge ---
  message("Merging LUST data...")
  lust_report_months <- readRDS(file.path(lust_stage_dir, "lust_report_months.rds")); panel <- merge(panel, lust_report_months, by = c("FACILITY_ID", "YEAR", "MONTH"), all.x = TRUE); rm(lust_report_months); gc()
  lust_monthly_status <- readRDS(file.path(lust_stage_dir, "lust_monthly_status.rds")); panel <- merge(panel, lust_monthly_status, by = c("FACILITY_ID", "YEAR", "MONTH"), all.x = TRUE); rm(lust_monthly_status); gc()
  facility_lust_summary <- readRDS(file.path(lust_stage_dir, "facility_lust_summary.rds")); panel <- merge(panel, facility_lust_summary, by = "FACILITY_ID", all.x = TRUE); rm(facility_lust_summary); gc()
  
  lust_flags <- c("is_lust_report_month", "lust_active", "lust_pre_2000_active", "lust_post_2000_active", "has_subsidized_leak_active", "lust_open_active", "lust_closed_active", "lust_ever", "lust_pre_2000", "lust_post_2000", "has_subsidized_leak", "lust_ever_closed", "lust_ever_open", "lust_after_closure_ever")
  panel[, (lust_flags) := lapply(.SD, function(x) fifelse(is.na(x), 0L, x)), .SDcols = lust_flags]
  panel[, `:=`(closure_not_final_event = as.integer(lust_after_closure_ever == 1L)), by = FACILITY_ID]
  message("LUST indicators merged successfully.")

  # --- 2.5. Census Data Merge ---
  message("Merging Census data...")
  census_data_panel <- readRDS(file.path(census_stage_dir, "census_data_panel.rds"))
  if("fips" %in% names(panel) && "fips" %in% names(census_data_panel)) {
    if(!is.character(panel$fips)) panel[, fips := as.character(fips)]
    if(!is.character(census_data_panel$fips)) census_data_panel[, fips := as.character(fips)]
  }
  if("YEAR" %in% names(panel) && "YEAR" %in% names(census_data_panel)) {
    if(!is.integer(panel$YEAR)) panel[, YEAR := as.integer(YEAR)]
    if(!is.integer(census_data_panel$YEAR)) census_data_panel[, YEAR := as.integer(YEAR)]
  }
  panel <- merge(panel, census_data_panel, by = c("fips", "YEAR"), all.x = TRUE)
  rm(census_data_panel); gc()
  message("Updated census data merged to panel.")

  # --- 3. Final NA Fill and Save ---
  message("Applying final NA fill to panel...")
  # This loop handles all remaining NA values for key numeric/logical columns
  cols_to_fill_zero <- c(event_cols, delta_cols, lust_flags, "is_facility_entry_event", "is_facility_exit_event", "closure_not_final_event")
  cols_to_fill_zero <- intersect(cols_to_fill_zero, names(panel))
  for (col in cols_to_fill_zero) {
      panel[is.na(get(col)), (col) := 0L]
  }
  # Broader fill for any remaining logicals/numerics
  logical_cols <- names(panel)[sapply(panel, is.logical)]
  if (length(logical_cols) > 0) { for (col in logical_cols) panel[is.na(get(col)), (col) := FALSE] }
  numeric_cols <- names(panel)[sapply(panel, is.numeric)]
  if (length(numeric_cols) > 0) { for (col in numeric_cols) panel[is.na(get(col)) | is.nan(get(col)), (col) := 0] }

  message("Final row count: ", format(nrow(panel), big.mark = ","))
  message("Unique facilities: ", uniqueN(panel$FACILITY_ID))

  message("Saving final panel data...")
  fwrite(panel, get_data_path("Outputs", "texas_fr_panel.csv"))
  saveRDS(panel, get_data_path("Outputs", "texas_fr_panel.rds"))
  if (requireNamespace("fst", quietly = TRUE)) {
    fst::write_fst(panel, get_data_path("Outputs", "texas_fr_panel.fst"))
  }
  message("Panel data saved to ", get_data_path("Outputs"))

} else {
  message("\n--- Staging Complete. Skipping full merge (do_merge = FALSE) ---")
}

# Performance monitoring
section_end <- Sys.time()
message("Section 7/8/9 (Staging & Merge) completed in ", round(difftime(section_end, section7_start, units = "mins"), 2), " minutes")
message("Script finished.")

# ... (Section 11 and the column dictionary can follow




# Start timing for performance monitoring

# ###############################################################################
# # QUICK-STAGE ALL MERGE TABLES TO DISK  (place just before the first merge)
# ###############################################################################
# stage_dir <- get_data_path("Outputs", "panel_merge_staging")
# if (!dir.exists(stage_dir)) dir.create(stage_dir, recursive = TRUE)   # make once

# datasets_to_stage <- list(
#   fa_monthly            = fa_monthly,
#   erp_facility_monthly  = erp_facility_monthly,
#   fac_info              = fac[ , .(FACILITY_ID, FACILITY_NAME,
#                                    FACILITY_TYPE, FACILITY_STATUS,
#                                    SITE_COUNTY, SITE_ZIP,
#                                    NUM_ACTIVE_USTS_is_blank,
#                                    NUM_ACTIVE_ASTS_is_blank)],
#   fac_contact_months    = fac_contact_months,
#   ust_monthly           = ust_monthly,
#   zurich_2012_lookup    = zurich_2012_lookup,
#   self_cert_monthly     = self_cert_monthly
#   # add / comment–out here as needed
# )

# for (nm in names(datasets_to_stage)) {
#   dt <- datasets_to_stage[[nm]]
#   csv_path <- file.path(stage_dir, paste0(nm, ".csv"))
#   rds_path <- file.path(stage_dir, paste0(nm, ".rds"))

#   data.table::fwrite(dt, csv_path)   # fast text ☞ fread later :contentReference[oaicite:3]{index=3}
#   saveRDS(dt, rds_path)              # binary ☞ readRDS later  :contentReference[oaicite:4]{index=4}
#   message("Wrote ", nm, " → ",
#           basename(csv_path), " & ", basename(rds_path))
# }
# ###############################################################################
# # >>> proceed with setkey() / merge() calls here <<<
# ###############################################################################


# section7_start <- Sys.time()
# message("Processing Section 7: Building main panel and merging data...")

# uniqueN(panel$FACILITY_ID) # Check how many unique facility IDs we have in the panel
# # Merge Financial Assurance monthly data (fa_monthly)

# setkey(panel,      FACILITY_ID, YEAR, MONTH)   # index once
# setkey(fa_monthly, FACILITY_ID, YEAR, MONTH)


# panel <- merge(panel, fa_monthly, by = c("FACILITY_ID", "YEAR", "MONTH"), all.x = TRUE)

# uniqueN(panel$FACILITY_ID) # Check how many unique facility IDs we have after merging FA data


# message("Final row count: ", format(nrow(panel), big.mark = ","))
# message("Unique facilities: ", uniqueN(panel$FACILITY_ID))# ── Section 7.1: Gap Detection, ERP Logic, and Change Indicators ──────────────
# # Purpose: After merging FA data into the complete panel, identify coverage gaps,
# #   apply ERP logic, detect contract/issuer changes, and flag transitions.
# # Why: Gap detection requires the complete facility-month panel, not just FA data.
# # Input: panel with fa_monthly merged (some months will have no FA coverage)
# # Output: panel with gap flags, ERP indicators, and month-to-month change flags
# # ──────────────────────────────────────────────────────────────────────────────

# message("Processing gap detection and change indicators after FA merge...")

# # ── Step 1: Identify Coverage Gaps ───────────────────────────────────────────
# # Set fr_covered to FALSE for months with no FA data
# panel[is.na(fr_covered), fr_covered := FALSE]

# # ── Step 2: Calculate ERP (Extended Reporting Period) Coverage ───────────────
# message("Calculating ERP coverage...")

# # Optimized ERP calculation with pre-filtering
# expired_contracts <- fa_monthly_contract[
#   !is.na(EXP_DATE),
#   .(FACILITY_ID, FIN_ASSUR_ID, EXP_DATE,
#     erp_start = floor_date(EXP_DATE + months(1), "month"),
#     erp_end = floor_date(EXP_DATE + months(6), "month"))
# ][!is.na(erp_start) & !is.na(erp_end)]

# # Remove duplicates (same contract-facility combination)
# expired_contracts <- unique(expired_contracts, by = c("FACILITY_ID", "FIN_ASSUR_ID"))

# # Expand ERP periods to monthly observations
# erp_monthly <- expired_contracts[, {
#   erp_months <- seq(erp_start, erp_end, by = "month")
#   .(YEAR = year(erp_months),
#     MONTH = month(erp_months),
#     erp_coverage = TRUE)
# }, by = .(FACILITY_ID, FIN_ASSUR_ID)]

# # Aggregate ERP coverage by facility-month
# erp_facility_monthly <- erp_monthly[, .(
#   erp_reporting_month = any(erp_coverage, na.rm = TRUE)
# ), by = .(FACILITY_ID, YEAR, MONTH)]

# # Merge ERP coverage into panel
# panel <- merge(panel, erp_facility_monthly, by = c("FACILITY_ID", "YEAR", "MONTH"), all.x = TRUE)
# panel[is.na(erp_reporting_month), erp_reporting_month := FALSE]

# # ── Step 3: Final Gap Detection ──────────────────────────────────────────────
# # Combined gap detection and sentinel value setting
# panel[, `:=`(
#   coverage_gap_month = (!fr_covered) & (!erp_reporting_month),
#   # Set sentinel values for gap months in same operation
#   ISSUER_NAME = fifelse((!fr_covered) & (!erp_reporting_month), "NO COVERAGE", ISSUER_NAME),
#   DETAIL_TYPE = fifelse((!fr_covered) & (!erp_reporting_month), "NO COVERAGE", DETAIL_TYPE),
#   CATEGORY = fifelse((!fr_covered) & (!erp_reporting_month), "NO COVERAGE", CATEGORY)
# )]

# # 1) For any month before 1999, force the “State Fund” values
# panel[ YEAR < 1999, `:=`(
#   ISSUER_NAME = "State Fund",
#   DETAIL_TYPE = "State Insurance",
#   CATEGORY    = "State Fund"
# ) ]


# message("Coverage gaps identified: ", panel[coverage_gap_month == TRUE, .N], " facility-months")

# # ── Step 4: Contract/Issuer Change Detection (Optimized) ─────────────────────
# # Set keys once for optimal performance
# setkey(panel, FACILITY_ID, YEAR, MONTH)

# # Batch calculation of previous month values and change detection
# panel[, `:=`(
#   # Previous month values
#   prev_issuer = shift(ISSUER_NAME, type = "lag"),
#   prev_category = shift(CATEGORY, type = "lag"),
#   prev_detail_type = shift(DETAIL_TYPE, type = "lag"),
#   prev_fr_covered = shift(fr_covered, type = "lag"),
#   prev_coverage_gap = shift(coverage_gap_month, type = "lag")
# ), by = FACILITY_ID][, `:=`(
#   # Change detection in same operation
#   issuer_changed = (!is.na(prev_issuer) & !is.na(ISSUER_NAME) & ISSUER_NAME != prev_issuer),
#   mech_changed = (!is.na(prev_category) & !is.na(CATEGORY) & CATEGORY != prev_category),
#   detail_type_changed = (!is.na(prev_detail_type) & !is.na(DETAIL_TYPE) & DETAIL_TYPE != prev_detail_type)
# )][, `:=`(
#   # Derived indicators
#   contract_changed = (issuer_changed | mech_changed | detail_type_changed),
#   # Specific transition types
#   changed_to_self_insure = (mech_changed & 
#                            grepl("Self-Insurance", CATEGORY, ignore.case = TRUE) & 
#                            !is.na(prev_category) &
#                            !grepl("Self-Insurance", prev_category, ignore.case = TRUE)),
#   changed_to_insurance = (mech_changed &
#                          grepl("Insurance", CATEGORY, ignore.case = TRUE) & 
#                          !is.na(prev_category) &
#                          !grepl("Insurance", prev_category, ignore.case = TRUE)),
#   changed_to_no_coverage = (coverage_gap_month == TRUE & 
#                            !is.na(prev_coverage_gap) & 
#                            prev_coverage_gap == FALSE),
#   changed_from_no_coverage = (coverage_gap_month == FALSE & 
#                              !is.na(prev_coverage_gap) & 
#                              prev_coverage_gap == TRUE)
# )]

# # Clean up temporary columns
# panel[, c("prev_issuer", "prev_category", "prev_detail_type", 
#           "prev_fr_covered", "prev_coverage_gap") := NULL]

# # ── Step 5: Summary Statistics ───────────────────────────────────────────────
# message("Change indicator summary:")
# change_summary <- panel[, .(
#   total_facility_months = .N,
#   coverage_gaps = sum(coverage_gap_month, na.rm = TRUE),
#   erp_months = sum(erp_reporting_month, na.rm = TRUE),
#   issuer_changes = sum(issuer_changed, na.rm = TRUE),
#   mechanism_changes = sum(mech_changed, na.rm = TRUE),
#   contract_changes = sum(contract_changed, na.rm = TRUE),
#   transitions_to_self = sum(changed_to_self_insure, na.rm = TRUE),
#   transitions_to_insurance = sum(changed_to_insurance, na.rm = TRUE),
#   transitions_to_gaps = sum(changed_to_no_coverage, na.rm = TRUE),
#   transitions_from_gaps = sum(changed_from_no_coverage, na.rm = TRUE)
# )]

# print(change_summary)

# # ── Step 6: Validation Checks ────────────────────────────────────────────────
# inconsistent_erp <- panel[erp_reporting_month == TRUE & fr_covered == TRUE, .N]
# if (inconsistent_erp > 0) {
#   warning("Found ", inconsistent_erp, " months with both active coverage and ERP - this may indicate overlapping contracts")
# }

# gap_with_coverage <- panel[coverage_gap_month == TRUE & fr_covered == TRUE, .N]
# if (gap_with_coverage > 0) {
#   warning("Found ", gap_with_coverage, " months marked as gaps but with active coverage - check logic")
# }

# message("Gap detection and change indicators complete.")
# message("Coverage gaps: ", panel[coverage_gap_month == TRUE, .N], " facility-months")
# message("Contract changes: ", panel[contract_changed == TRUE, .N], " facility-months")
# message("Merged monthly financial assurance data (fa_monthly).") 

# # ── Batch Data Merges (Optimization 2) ──────────────────────────────────────
# message("Performing batch data merges...")

# # Define merge datasets for batch processing
# merge_datasets <- list(
#   list(data = fac[, .(FACILITY_ID, FACILITY_NAME, FACILITY_TYPE, FACILITY_STATUS, 
#                      SITE_COUNTY, SITE_ZIP, NUM_ACTIVE_USTS_is_blank, NUM_ACTIVE_ASTS_is_blank)], 
#        keys = "FACILITY_ID", name = "facility information"),
#   list(data = fac_contact_months, keys = c("FACILITY_ID", "YEAR", "MONTH"), name = "contact data"),
#   list(data = ust_monthly, keys = c("FACILITY_ID", "YEAR", "MONTH"), name = "UST metrics"),
#   list(data = zurich_2012_lookup, keys = "FACILITY_ID", name = "Zurich lookup"),
#   list(data = self_cert_monthly, keys = c("FACILITY_ID", "YEAR", "MONTH"), name = "self-certification")
# )

# for(merge_info in merge_datasets) {
#   panel <- merge(panel, merge_info$data, by = merge_info$keys, all.x = TRUE)
#   message("Merged ", merge_info$name)
# }

# # ── FIPS Code Mapping (Optimization 3) ──────────────────────────────────────
# message("Adding FIPS codes based on SITE_COUNTY...")
# data("fips_codes")
# tx_county_fips <- as.data.table(fips_codes)[
#   state == "TX", 
#   .(county_clean = toupper(gsub("\\s*County$", "", county)),
#     fips = paste0(state_code, county_code))
# ]
# tx_county_fips <- unique(tx_county_fips, by = c("county_clean", "fips"))

# panel[, SITE_COUNTY_upper := toupper(SITE_COUNTY)]
# panel <- merge(panel, tx_county_fips[, .(SITE_COUNTY_upper = county_clean, fips)],
#                by = "SITE_COUNTY_upper", all.x = TRUE)
# panel[, SITE_COUNTY_upper := NULL]

# # Handle unmapped counties
# unmapped_counties <- panel[is.na(fips) & !is.na(SITE_COUNTY), unique(SITE_COUNTY)]
# if (length(unmapped_counties) > 0) {
#   message("Warning: Unmapped Texas counties: ", paste(unmapped_counties, collapse = ", "))
# }

# # facility status flags
# panel[
#   , any_tank_active := active_tank_count > 0
# ]

# panel[
#   , `:=`(
#       is_facility_entry_event = any_tank_active & !shift(any_tank_active, type="lag", fill=FALSE),
#       is_facility_exit_event  = !any_tank_active & shift(any_tank_active, type="lag", fill=FALSE)
#   ),
#   by = FACILITY_ID
# ]

# # ── Owner/Operator Difference Calculation (Optimization 1) ──────────────────
# message("Calculating owner-operator differences...")

# # Memory-efficient calculation for large datasets
# if(nrow(panel) > 500000) {
#   # Process in chunks for very large datasets
#   chunk_size <- 100000
#   chunks <- split(seq_len(nrow(panel)), ceiling(seq_len(nrow(panel))/chunk_size))
  
#   for(i in seq_along(chunks)) {
#     chunk_idx <- chunks[[i]]
#     panel[chunk_idx, owner_operator_diff := {
#       mapply(function(o_names, op_names) {
#         if(is.na(o_names) || is.na(op_names) || o_names == "" || op_names == "") return(FALSE)
#         o_set <- unique(trimws(strsplit(o_names, " \\| ", fixed = TRUE)[[1]]))
#         op_set <- unique(trimws(strsplit(op_names, " \\| ", fixed = TRUE)[[1]]))
#         length(intersect(o_set, op_set)) == 0
#       }, owner_names_concatenated, operator_names_concatenated)
#     }]
#     if(i %% 5 == 0) message("Processed chunk ", i, " of ", length(chunks))
#   }
# } else {
#   # Standard processing for smaller datasets
#   panel[, owner_operator_diff := {
#     mapply(function(o_names, op_names) {
#       if(is.na(o_names) || is.na(op_names) || o_names == "" || op_names == "") return(FALSE)
#       o_set <- unique(trimws(strsplit(o_names, " \\| ", fixed = TRUE)[[1]]))
#       op_set <- unique(trimws(strsplit(op_names, " \\| ", fixed = TRUE)[[1]]))
#       length(intersect(o_set, op_set)) == 0
#     }, owner_names_concatenated, operator_names_concatenated)
#   }]
# }

# message("Calculated owner-operator difference.")

# # ── Zurich Processing ────────────────────────────────────────────────────────
# panel[, `:=`(
#   had_zurich_2012 = fifelse(is.na(had_zurich_2012), 0L, had_zurich_2012),
#   dropped_by_zurich = had_zurich_2012 * as.integer(YEAR >= 2013)
# )]

# # ── Efficient Change Calculations (Optimization 4) ──────────────────────────
# message("Computing change metrics...")

# # Define all change columns
# change_metrics <- c("total_capacity", "num_interstitial_det", "num_electronic_det", 
#                    "num_manual_stat_det", "num_steel_tanks", "num_fiberglass_tanks", 
#                    "num_jacketed_tanks")

# # Vectorized lag calculation
# lag_cols <- paste0("lag_", change_metrics)
# panel[, (lag_cols) := lapply(.SD, shift, type = "lag"), 
#       .SDcols = change_metrics, by = FACILITY_ID]

# # Vectorized delta and event calculation using Map
# delta_cols <- paste0(change_metrics, "_delta")
# event_cols <- paste0(change_metrics, "_change_event")

# for (i in seq_along(change_metrics)) {
#   orig_col <- change_metrics[i]
#   lag_col <- lag_cols[i]
#   delta_col <- delta_cols[i]
#   event_col <- event_cols[i]
  
#   panel[, (delta_col) := fifelse(is.na(get(lag_col)), 0, get(orig_col) - get(lag_col))]
#   panel[, (event_col) := as.integer(!is.na(get(lag_col)) & get(orig_col) != get(lag_col))]
# }

# # Clean up lag columns and handle special cases
# panel[, (lag_cols) := NULL]
# setnames(panel, "total_capacity_delta", "capacity_delta_gal")
# setnames(panel, "total_capacity_change_event", "capacity_change_event")

# # Handle remaining NAs efficiently
# delta_event_cols <- c(delta_cols, event_cols)
# for(col in delta_event_cols) {
#   if(col %in% names(panel)) panel[is.na(get(col)), (col) := 0L]
# }

# # ── Optimized LUST Processing (Optimization 5) ──────────────────────────────
# message("Processing LUST data...")

# # Load and preprocess LUST data efficiently
# TX_LUST_SD <- if(onserver) {
#   fread(get_data_path("TX_LUST.csv"))
# } else {
#   fread("C:/Users/kaleb/Box/UST-Insurance/Data/Raw_do_not_write/state_databases/Texas/TX_LUST.csv")
# }

# # Batch processing for LUST data
# if ("facility_id" %in% names(TX_LUST_SD)) setnames(TX_LUST_SD, "facility_id", "FACILITY_ID")
# TX_LUST_SD <- clean_id_column(TX_LUST_SD, "FACILITY_ID")

# # Efficient date processing and filtering
# TX_LUST_SD[, `:=`(
#   report_date = dmy(report_date),
#   nfa_date = dmy(nfa_date)
# )][
#   # Pre-filter for date range and facilities
#   !is.na(report_date) & 
#   report_date >= as.Date("1990-01-01") & 
#   report_date <= as.Date("2026-12-31") &
#   FACILITY_ID %in% facilities_active_1990$FACILITY_ID
# ]

# message("LUST records filtered to ", nrow(TX_LUST_SD), " relevant records")

# # Optimized LUST expansion
# expand_lust_optimized <- function(lust_dt) {
#   lust_dt[!is.na(report_date), {
#     start_month <- floor_date(report_date, "month")
#     end_month <- floor_date(fifelse(is.na(nfa_date), as.Date("2026-12-31"), nfa_date), "month")
    
#     if(start_month <= end_month) {
#       month_seq <- seq(start_month, end_month, by = "month")
#       .(YEAR = year(month_seq),
#         MONTH = month(month_seq),
#         lust_active = TRUE,
#         is_pre2000 = report_date < as.Date("2000-01-01"),
#         has_subsidy = report_date < as.Date("2000-01-01"),
#         nfa_month = end_month)
#     }
#   }, by = .(FACILITY_ID, LUST_id)]
# }

# # LUST report months
# lust_report_months <- TX_LUST_SD[!is.na(report_date), .(
#   FACILITY_ID, YEAR = year(report_date), MONTH = month(report_date),
#   is_lust_report_month = 1L
# )]

# # Expand and aggregate LUST data
# lust_long <- expand_lust_optimized(TX_LUST_SD)

# lust_monthly_status <- lust_long[, .(
#   lust_active = as.integer(any(lust_active)),
#   lust_pre_2000_active = as.integer(any(lust_active & is_pre2000)),
#   lust_post_2000_active = as.integer(any(lust_active & !is_pre2000)),
#   has_subsidized_leak_active = as.integer(any(lust_active & has_subsidy)),
#   lust_open_active = as.integer(any(lust_active & (nfa_month > make_date(YEAR, MONTH, 1)))),
#   lust_closed_active = as.integer(any(lust_active & (nfa_month <= make_date(YEAR, MONTH, 1))))
# ), by = .(FACILITY_ID, YEAR, MONTH)]

# facility_lust_summary <- TX_LUST_SD[, .(
#   lust_ever = 1L,
#   lust_pre_2000 = as.integer(any(report_date < "2000-01-01")),
#   lust_post_2000 = as.integer(any(report_date >= "2000-01-01")),
#   has_subsidized_leak = as.integer(any(report_date < "2000-01-01")),
#   lust_ever_closed = as.integer(any(!is.na(nfa_date))),
#   lust_ever_open = as.integer(any(is.na(nfa_date))),
#   lust_after_closure_ever = as.integer(any(!is.na(nfa_date) & nfa_date < as.Date("2026-12-31") & nfa_date > report_date))
# ), by = FACILITY_ID]

# # Batch LUST merges
# lust_datasets <- list(
#   list(data = lust_report_months, keys = c("FACILITY_ID", "YEAR", "MONTH")),
#   list(data = lust_monthly_status, keys = c("FACILITY_ID", "YEAR", "MONTH")),
#   list(data = facility_lust_summary, keys = "FACILITY_ID")
# )


# ###############################################################################
# # QUICK-STAGE LUST TABLES TO DISK  (insert right before the merges)
# ###############################################################################
# lust_stage_dir <- get_data_path("Outputs", "lust_merge_staging")
# if (!dir.exists(lust_stage_dir)) dir.create(lust_stage_dir, recursive = TRUE)

# lust_to_stage <- list(
#   lust_report_months    = lust_report_months,
#   lust_monthly_status   = lust_monthly_status,
#   facility_lust_summary = facility_lust_summary
# )

# for (nm in names(lust_to_stage)) {
#   dt <- lust_to_stage[[nm]]
#   csv_path <- file.path(lust_stage_dir, paste0(nm, ".csv"))
#   rds_path <- file.path(lust_stage_dir, paste0(nm, ".rds"))

#   data.table::fwrite(dt, csv_path)   # fast text
#   saveRDS(dt, rds_path)              # compact binary
#   message("Staged ", nm, " → ",
#           basename(csv_path), " & ", basename(rds_path))
# }
# ###############################################################################
# # >>> now proceed with the lust_datasets merge loop <<<
# ###############################################################################


# for(lust_info in lust_datasets) {
#   panel <- merge(panel, lust_info$data, by = lust_info$keys, all.x = TRUE)
# }

# # Batch NA handling for LUST flags
# lust_flags <- c("is_lust_report_month", "lust_active", "lust_pre_2000_active", 
#                "lust_post_2000_active", "has_subsidized_leak_active", 
#                "lust_open_active", "lust_closed_active", "lust_ever", 
#                "lust_pre_2000", "lust_post_2000", "has_subsidized_leak", 
#                "lust_ever_closed", "lust_ever_open", "lust_after_closure_ever")

# panel[, (lust_flags) := lapply(.SD, function(x) fifelse(is.na(x), 0L, x)), 
#       .SDcols = lust_flags]

# panel[, `:=`(
#   closure_not_final_event = as.integer(lust_after_closure_ever == 1L)
# ), by = FACILITY_ID]

# # And then clean up any NAs:
#  for(col in c("is_facility_entry_event","is_facility_exit_event","closure_not_final_event")){
#    panel[is.na(get(col)), (col) := 0L]
#  }

# message("LUST indicators merged successfully.")

# # ── Memory Management (Optimization 6) ──────────────────────────────────────
# if(nrow(panel) > 1000000) {
#   gc(verbose = FALSE)
#   message("Memory cleaned after major operations")
# }

# # Performance monitoring
# section7_end <- Sys.time()
# message("Section 7 completed in ", round(difftime(section7_end, section7_start, units = "mins"), 2), " minutes")

# message("Section 7 data processing complete.")

# # ── Section 8: Census data merge ─────────────────────────────────────────────
# # Purpose: read or (disabled) fetch ACS/FRED/county-PEP data, clean/populate pop, income, Gini,
# #   expand to yearly county panel and then merge to facility-month panel via county FIPS & YEAR.
# # Why: to attach local socioeconomic context to each facility-month.
# # End product: enriched `panel` with census covariates by fips & YEAR.
# message("Processing: Downloading updated census data for panel merge…")

# ###############################################################################
# # 1. Meta: keep a tidy lookup of variable ↔ units for future reference
# ###############################################################################
# units_map <- data.table(
#   variable = c("total_pop", "median_hh_income", "gini_index"),
#   units    = c("persons",  "US dollars (nominal-year)", "unitless index (0–1)")
# )

# # Helper function to expand 5-year ACS bins to every calendar year
# expand_5yr_bin <- function(dt, end_year_col = "survey_end_year", value_cols) {
#   dt_long <- melt(
#     dt,
#     id.vars      = c("fips", end_year_col),
#     measure.vars = value_cols,
#     variable.name = "var",
#     value.name    = "value"
#   )
#   dt_long[, year := as.integer(get(end_year_col))]
#   dt_long <- dt_long[, .(
#       year  = rep((year - 4):year, each = .N / length(value_cols)),
#       fips,
#       var,
#       value
#     ), by = .I]
#   dcast(dt_long, fips + year ~ var, value.var = "value")
# }
# ###############################################################################
# # Texas county totals, 1998-2020  |  U.S. Census Bureau Population Estimates
# ###############################################################################
# library(data.table)

# ## ---- helper to read wide CSV files (2000-09 & 2010-20) ----------------------
# reshape_pop <- function(url, years) {
#   cols <- c("STATE", "COUNTY", paste0("POPESTIMATE", years))
#   dt   <- fread(url, select = cols, colClasses = "character", na.strings = "(X)")
#   dt   <- dt[STATE == "48"]                               # Texas only
#   dt[, fips := sprintf("%02d%03d", as.integer(STATE), as.integer(COUNTY))]
#   long <- melt(dt, id.vars = "fips", measure.vars = patterns("^POPESTIMATE"),
#                variable.name = "year_str", value.name = "total_pop", na.rm = TRUE)
#   long[, year := as.integer(sub("POPESTIMATE", "", year_str))]
#   long[, .(fips, year, total_pop = as.numeric(total_pop))]
# }

# ## ---- Part 1: 2010-2020 wide file -------------------------------------------
# pop_2010_2020 <- reshape_pop(
#   url   = "https://www2.census.gov/programs-surveys/popest/datasets/2010-2020/counties/totals/co-est2020-alldata.csv",  # :contentReference[oaicite:2]{index=2}
#   years = 2010:2020
# )

# ## ---- Part 2: 2000-2009 wide file -------------------------------------------
# pop_2000_2009 <- reshape_pop(
#   url   = "https://www2.census.gov/programs-surveys/popest/datasets/2000-2009/counties/totals/co-est2009-alldata.csv",  # :contentReference[oaicite:3]{index=3}
#   years = 2000:2009
# )

# ## ---- Part 3: 1998-1999 intercensal TXT files -------------------------------
# ###############################################################################
# # QUICK PATCH ― read 1998 & 1999 intercensal files cleanly with **data.table**
# #   •  The `stch-icenYYYY.txt` files are *comma-delimited, no header*.
# #   •  Every record has 6 columns in the order shown below.
# #   •  AGEGRP==0 (= “all ages”), RACE==0, ORIGIN==0 give total population.
# #
# #  NOTE: The earlier error arose because we renamed columns *after* picking
# #        pop_col. Here we (i) add generic V1:V6 names _first_, (ii) pick
# #        pop_col, and (iii) never rename away the V* names we rely on.
# ###############################################################################
# ###############################################################################
# # peek_icen_year()  ―  robust “peek-and-clean” helper for 1990-1999 intercensal
# #
# #  • Works directly with the ST-CO txt files (no header, 6 columns).
# #  • Prints a quick preview so you can eyeball the raw rows.
# #  • Detects which column is the population count (always the last col, V6).
# #  • Keeps rows where STATE==48 (Texas), AGEGRP==0, RACE==1, ORIGIN==1
# #    → those are the bridged-race total-population lines.
# #
# #  NOTE: The earlier error arose because we renamed columns *after* picking
# #        pop_col. Here we (i) add generic V1:V6 names _first_, (ii) pick
# #        pop_col, and (iii) never rename away the V* names we rely on.
# ###############################################################################
# ###############################################################################
# # peek_icen_year()  ―  robust reader for 1990-1999 intercensal ST-CO files
# ###############################################################################
# library(data.table)

# peek_icen_year <- function(yr, state_fips = "48", n_preview = 5) {
#   stopifnot(yr %in% 1990:1999)

#   url <- sprintf(
#     "https://www2.census.gov/programs-surveys/popest/tables/1990-2000/intercensal/st-co/stch-icen%d.txt",
#     yr
#   )

#   # ---- read with generic V1…V6 headers --------------------------------------
#   raw <- fread(url, header = FALSE, colClasses = "character", showProgress = FALSE)
#   setnames(raw, paste0("V", seq_len(ncol(raw))))   # ensure V1:V6

#   message(sprintf("\nPreview of %s (first %d rows):", basename(url), n_preview))
#   print(head(raw, n_preview))

#   # ---- filter to Texas ------------------------------------------------------
#   tx <- raw[
#     substr(V2, 1, 2) == state_fips &  # V2 = 5-digit county FIPS
#     V3 == "0" &                       # AGEGRP 0  (all ages)
#     V4 == "1" &                       # RACE   1  (all races, bridged)
#     V5 == "1" ,                       # ORIGIN 1  (total origin)
#     .(fips      = V2,                 # already 5-digit
#       year      = yr,
#       total_pop = as.numeric(V6))
#   ]

#   message(sprintf("Returned %d county rows for Texas, %d", nrow(tx), yr))
#   return(tx)
# }

# # ---- run the two years ------------------------------------------------------
# pop_1998 <- peek_icen_year(1998)
# pop_1999 <- peek_icen_year(1999)

# # then bind with pop_2000_2009 and pop_2010_2020 as before:
# # pop_dt <- rbindlist(list(pop_1998, pop_1999, pop_2000_2009, pop_2010_2020))
# ###############################################################################
# ###############################################################################

# # afterward: rbindlist(list(pop_1998, pop_1999, pop_2000_2009, pop_2010_2020))
# ###############################################################################


# ## ---- stack & sort -----------------------------------------------------------
# pop_dt <- rbindlist(
#   list(pop_1998, pop_1999, pop_2000_2009, pop_2010_2020),
#   use.names = TRUE
# )[order(fips, year)]

# message(sprintf("Pop table: %s rows, %s-%s", nrow(pop_dt),
#                 min(pop_dt$year), max(pop_dt$year)))

# ###############################################################################
# # pop_dt is ready to left-join into census_data_panel on fips + year
# ###############################################################################

# # Median household income (SAIPE annual) 1998-2020
# ###############################################################################
# if (!requireNamespace("fredr", quietly = TRUE)) install.packages("fredr")
# library(fredr); library(data.table)
# # fredr_set_key("2d9a1f281713753ddce9c4250540c2aa")  # <-- set once in .Renviron
# fredr_set_key("2d9a1f281713753ddce9c4250540c2aa")  # <-- set once in .Renviron
# library(fredr)      # install.packages("fredr")
# library(data.table) # install.packages("data.table")

# # 1 ── full list of Texas county FIPS via tidycensus --------------------------
# if (!requireNamespace("tidycensus", quietly = TRUE)) install.packages("tidycensus")


# # 1 ── pull TX county codes and convert to data.table
# tx_fips <- as.data.table(fips_codes)[state == "TX"]   # filter after conversion
# tx_fips[ , fips := paste0(state_code, county_code)]   # create 5-digit code
# tx_fips <- unique(tx_fips[ , .(fips)])                # keep one row per fips

# # 2 ── add statewide aggregate 48000
# if (!"48000" %in% tx_fips$fips) {
#   tx_fips <- rbind(tx_fips, data.table(fips = "48000"))
# }

# print(head(tx_fips, 6))   # sanity-check: should show 48000 and first counties

# # 2 ── build correct series IDs ----------------------------------------------
# tx_fips[, series_id := paste0("MHITX", fips, "A052NCEN")]
# tail(tx_fips) # check that series_id is correct
# # 3 ── helper (fixed) ---------------------------------------------------------
# get_mhi <- function(id, fips_code) {
#   Sys.sleep(0.5)                           # ← rate-limit politeness
#   tryCatch({
#     obs <- fredr_series_observations(
#       series_id         = id,
#       observation_start = as.Date("1989-01-01"),
#       units             = "lin"
#     )

#     # ---- NEW: convert to data.table before using [ , .(…) ] -----------------
#     obs <- as.data.table(obs)

#     if (nrow(obs) == 0) return(NULL)

#     obs[ , .(
#       fips             = fips_code,
#       year             = year(date),
#       median_hh_income = value
#     )]

#   }, error = function(e) {
#     warning(sprintf("Missing series %s (%s): %s", id, fips_code, e$message))
#     NULL
#   })
# }

# # 4 ── loop & bind (unchanged) ------------------------------------------------
# income_dt <- rbindlist(
#   Map(get_mhi, tx_fips$series_id, tx_fips$fips),
#   use.names = TRUE, fill = TRUE
# )

# income_dt[ , median_hh_income_units := "US dollars (nominal-year)"]

# # Display a summary of the fetched data
# message("Fetched income data for ", uniqueN(income_dt[!is.na(median_hh_income), fips]), " counties.")
# message("Years range from: ", min(income_dt$year, na.rm = TRUE), " to ", max(income_dt$year, na.rm = TRUE))
# print(head(income_dt))
# ###############################################################################
# # sanity-check: all population variables already confirmed in “persons”
# # (see units_map); nothing to rescale.
# ###############################################################################
# library(data.table)      # make sure it’s loaded

# message("Downloading & expanding ACS Gini …")
# acs_end_years <- 2010:2023           # 2006-10 … 2019-23 5-yr files

# gini_list <- lapply(acs_end_years, function(ey) {

#   # pull as tibble …
#   acs_tbl <- get_acs(
#     geography = "county",
#     state     = "TX",
#     table     = "B19083",
#     year      = ey,
#     survey    = "acs5",
#     output    = "wide"               # still returns a tibble
#   )

#   # … then convert *in place* to data.table
#   setDT(acs_tbl)                     # or acs_dt <- as.data.table(acs_tbl)

#   acs_tbl[ , .(
#     fips            = GEOID,
#     gini_index      = B19083_001E,
#     survey_end_year = ey
#   )]
# })

# gini_wide <- expand_5yr_bin(
#   rbindlist(gini_list),
#   end_year_col = "survey_end_year",
#   value_cols   = "gini_index"
# )


# ###############################################################################
# # 3. Merge & attach unit columns
# ###############################################################################
# census_data_panel <- CJ(fips = unique(pop_dt$fips), year = 1998:2020)
# census_data_panel <- merge(census_data_panel, pop_dt,    by = c("fips", "year"), all.x = TRUE)
# census_data_panel <- merge(census_data_panel, income_dt, by = c("fips", "year"), all.x = TRUE)
# census_data_panel <- merge(census_data_panel, gini_wide, by = c("fips", "year"), all.x = TRUE)
# setnames(census_data_panel, "year", "YEAR")

# # ---- add unit columns (constant text) ---------------------------------------
# census_data_panel[ , `:=`(
#   total_pop_units        = "persons",
#   median_hh_income_units = "US dollars (nominal-year)",
#   gini_index_units       = "unitless index (0–1)"
# )]

# ###############################################################################
# # 4. Harmonise population units just in case ----------------------------------
# #    (All three PEP files are in *persons* per the file-layout PDFs, but
# #     this guard rescales if any future vintage is reported in thousands.)
# if (any(census_data_panel$total_pop < 10000, na.rm = TRUE) & any(census_data_panel$total_pop > 0, na.rm = TRUE)) { # Check if any pop value is small but positive
#   # More robust check: if median of non-NA population is very small, it might be in thousands.
#   # This avoids triggering on counties with genuinely small populations if most are large.
#   median_pop <- median(census_data_panel$total_pop, na.rm = TRUE)
#   if (!is.na(median_pop) && median_pop < 10000) { # Arbitrary threshold, adjust as needed
#     warning("Median total_pop is low; values may be in thousands. Rescaling to persons.")
#     census_data_panel[!is.na(total_pop), total_pop := total_pop * 1000]
#   } else if (max(census_data_panel$total_pop, na.rm = TRUE) < 20000 && any(census_data_panel$total_pop > 0, na.rm = TRUE)) { # If max pop is low
#      warning("Maximum total_pop is low; values may be in thousands. Rescaling to persons.")
#      census_data_panel[!is.na(total_pop), total_pop := total_pop * 1000]
#   }
# }


# ###############################################################################
# # 5. Forward- & backward-fill Gini as before, then save
# ###############################################################################
# census_data_panel[order(fips, YEAR),
#   gini_index := zoo::na.locf(gini_index, na.rm = FALSE), by = fips]
# census_data_panel[order(fips, YEAR),
#   gini_index := zoo::na.locf(gini_index, na.rm = FALSE, fromLast = TRUE), by = fips]


# ###############################################################################
# # CPI-U DEFLATOR  → convert nominal MN_MHI to 2023 dollars
# #   • Source: FRED series CPIAUCSL (1982-84 = 100) :contentReference[oaicite:0]{index=0}
# #   • Access via fredr (requires free FRED API key) :contentReference[oaicite:1]{index=1}
# #   • Method: annual average CPI, then multiply by CPI_2023 / CPI_year.

# # ---- 1. pull annual CPI 1998-2023 -------------------------------------------
# cpi_dt <- as.data.table(
#   fredr(
#     series_id        = "CPIAUCSL",
#     observation_start= as.Date("1998-01-01"),
#     observation_end  = as.Date("2023-12-01"),
#     frequency        = "a"     # annual average index
#   )
# )[ , .(YEAR = year(date), CPI = value) ]

# # ---- 2. build deflator (factor = CPI_2023 / CPI_year) -----------------------
# cpi_2023 <- cpi_dt[YEAR == 2023, CPI]
# cpi_dt[ , cpi_factor := cpi_2023 / CPI ]

# # ---- 3. merge & compute real income (2023 $) --------------------------------
# census_data_panel <- merge(
#   census_data_panel, cpi_dt[ , .(YEAR, cpi_factor)], 
#   by = "YEAR", all.x = TRUE
# )

# census_data_panel[ , median_hh_income_real_2023 := 
#                      median_hh_income * cpi_factor ]

# # ---- 4. append units metadata ----------------------------------------------
# census_data_panel[ ,
#   median_hh_income_real_2023_units := "2023 dollars (CPI-U deflated)"
# ]
# units_map <- rbind(
#   units_map,
#   data.table(variable = "median_hh_income_real_2023",
#              units    = "2023 dollars (CPI-U deflated)")
# )

# # optional: drop helper factor
# # census_data_panel[ , cpi_factor := NULL ]
# ###############################################################################


# saveRDS(census_data_panel, file = get_data_path("Outputs", "tx_census_1998_2020.rds"))
# message("Updated census panel created and saved to ", get_data_path("Outputs", "tx_census_1998_2020.rds"))
# # FIX: Removed library(tidyverse) and glimpse calls, converted to data.table or commented
# # library(tidyverse) # Already loaded if needed
# # glimpse(panel[YEAR ==2010]) # FIX: Converted from dplyr
# # nrow(panel)
# # nrow(panel[is.na(fips)])
# # glimpse(panel[YEAR == 2010 & is.na(fips)]) # FIX: Converted from dplyr
# # Left merge the census_data_panel into the panel by fips and YEAR
# # The census_data_panel (containing total_pop, median_hh_income, gini_index)
# # will be merged. Since the previous ACS load was removed, there are no conflicting columns to handle.

# # Ensure fips in panel is character to match census_data_panel$fips if it's character
# if("fips" %in% names(panel) && "fips" %in% names(census_data_panel)) {
#     if(is.factor(panel$fips)) panel[, fips := as.character(fips)]
#     if(!is.character(panel$fips)) panel[, fips := as.character(fips)] # Ensure panel$fips is character
#     if(is.factor(census_data_panel$fips)) census_data_panel[, fips := as.character(fips)]
#     if(!is.character(census_data_panel$fips)) census_data_panel[, fips := as.character(fips)] # Ensure census_data_panel$fips is character

#     if(class(panel$fips) != class(census_data_panel$fips)) {
#         warning(paste("Class mismatch for fips column: panel is", class(panel$fips), 
#                       "and census_data_panel is", class(census_data_panel$fips), 
#                       "- attempting conversion to character for both."))
#         panel[, fips := as.character(fips)]
#         census_data_panel[, fips := as.character(fips)]
#     }
# }
# # Ensure YEAR types match
# if("YEAR" %in% names(panel) && "YEAR" %in% names(census_data_panel)) {
#     if(class(panel$YEAR) != class(census_data_panel$YEAR)) {
#         warning(paste("Class mismatch for YEAR column: panel is", class(panel$YEAR),
#                       "and census_data_panel is", class(census_data_panel$YEAR),
#                       "- attempting conversion to integer for both."))
#         panel[, YEAR := as.integer(YEAR)]
#         census_data_panel[, YEAR := as.integer(YEAR)]
#     }
# }

# ###############################################################################
# # QUICK-STAGE CENSUS TABLES TO DISK  (drop in right after census_data_panel)
# ###############################################################################
# census_stage_dir <- get_data_path("Outputs", "census_merge_staging")
# if (!dir.exists(census_stage_dir)) dir.create(census_stage_dir, recursive = TRUE)  # creates nested dirs :contentReference[oaicite:0]{index=0}

# census_to_stage <- list(
#   pop_dt             = pop_dt,              # Pops 1998-2020
#   income_dt          = income_dt,           # SAIPE Median HH income
#   gini_wide          = gini_wide,           # 5-yr ACS Gini, expanded to years
#   cpi_dt             = cpi_dt,              # CPI factors (if you want them)
#   census_data_panel  = census_data_panel,   # the final, merged county-year panel
#   units_map          = units_map            # lookup of variable ↔ units
# )

# for (nm in names(census_to_stage)) {
#   dt <- census_to_stage[[nm]]
#   csv_path <- file.path(census_stage_dir, paste0(nm, ".csv"))
#   rds_path <- file.path(census_stage_dir, paste0(nm, ".rds"))
  
#   data.table::fwrite(dt, csv_path)   # fast multi-core writer :contentReference[oaicite:1]{index=1}
#   saveRDS(dt, rds_path)              # base-R single-object serializer :contentReference[oaicite:2]{index=2}
  
#   message("Staged ", nm, " → ",
#           basename(csv_path), " & ", basename(rds_path))
# }
# ###############################################################################
# # >>> proceed with your merge(panel, census_data_panel, ...) <<<
# ###############################################################################



# panel <- merge(panel, census_data_panel, by = c("fips", "YEAR"), all.x = TRUE)
# # FIX: Removed setnafill here. One final fill will be applied before saving.
# message("Updated census data merged to panel.")

# # FIX: Consolidated NA fill (moved here from individual merge steps, applied before saving)
# message("Applying final NA fill to panel...")
# if (utils::packageVersion("data.table") >= "1.13.0") {
#     logical_cols <- names(panel)[sapply(panel, is.logical)]
#     if (length(logical_cols) > 0) setnafill(panel, type = "const", fill = FALSE, cols = logical_cols)
    
#     numeric_cols <- names(panel)[sapply(panel, is.numeric)]
#     if (length(numeric_cols) > 0) setnafill(panel, type = "const", fill = 0, nan = 0, cols = numeric_cols)
# } else {
#     # Fallback for older data.table versions
#     logical_cols <- names(panel)[sapply(panel, is.logical)]
#     if (length(logical_cols) > 0) {
#       for (col in logical_cols) panel[is.na(get(col)), (col) := FALSE]
#     }
#     numeric_cols <- names(panel)[sapply(panel, is.numeric)]
#     if (length(numeric_cols) > 0) {
#       for (col in numeric_cols) panel[is.na(get(col)) | is.nan(get(col)), (col) := 0]
#     }
#   }


# # ── Section 9: Save panel ───────────────────────────────────────────────────
# # Purpose: write out final `panel` as CSV, RDS, FST and generate a column dictionary.
# # Why: persist the fully merged facility-month dataset for analysis and documentation.
# # End product: files in Outputs/ (texas_fr_panel.csv, .rds, .fst, column dictionary).
# message("Processing Section 9: Saving panel data...")
# fwrite(panel, get_data_path("Outputs", "texas_fr_panel.csv"))
# message("Panel data saved to ", get_data_path("Outputs", "texas_fr_panel.csv"), ", ", get_data_path("Outputs", "texas_fr_panel.rds"), " and ", get_data_path("Outputs", "texas_fr_panel.fst"))

# ── Sample output: Show complete panel structure for one facility ──────────
message("\n=== SAMPLE PANEL OUTPUT FOR ONE FACILITY ===")
sample_facility <- panel[!is.na(FACILITY_ID)][1, FACILITY_ID]
sample_data <- panel[FACILITY_ID == sample_facility][order(YEAR, MONTH)]
message("Facility ID: ", sample_facility)
message("Panel structure (", nrow(sample_data), " rows from ", min(sample_data$YEAR), "-", sprintf("%02d", min(sample_data$MONTH)), " to ", max(sample_data$YEAR), "-", sprintf("%02d", max(sample_data$MONTH)), "):")

###############################################################################
# Section 11: Pick Example Facilities for Visual Inspection via ust_monthly
###############################################################################

# 1. Compute first & last active month per facility
first_last_ust <- ust_monthly[active_tank_count > 0, .(
  first_active = min(make_date(YEAR, MONTH, 1)),
  last_active  = max(make_date(YEAR, MONTH, 1))
), by = FACILITY_ID]

# 2. Merge back onto ust_monthly
ust_monthly <- merge(
  ust_monthly,
  first_last_ust,
  by = "FACILITY_ID",
  all.x = TRUE
)

# 3. Create four binary (0/1) flags
ust_monthly[, `:=`(
  # Closed all tanks after 1999 but before 2026
  closed_after_1999_before_2026 = as.integer(
    last_active  > as.Date("1999-12-01") &
    last_active  < as.Date("2026-01-01")
  ),

  # Entered before 1990 and exited before 1999
  entered_before_1990_exited_before_1999 = as.integer(
    first_active < as.Date("1990-01-01") &
    last_active  < as.Date("1999-12-01")
  ),

  # Enters after 1999
  enters_after_1999 = as.integer(
    first_active > as.Date("1999-12-01")
  ),

  # Open before 1990 and still open after 1999
  open_before_1990_and_still_after_1999 = as.integer(
    first_active < as.Date("1990-01-01") &
    last_active  > as.Date("1999-12-01")
  )
)]

# 1) define your four flag columns
flag_cols <- c(
  "closed_after_1999_before_2026",
  "entered_before_1990_exited_before_1999",
  "enters_after_1999",
  "open_before_1990_and_still_after_1999"
)

# # 2) Count total flagged facility-months for each flag
# flag_month_counts <- ust_monthly[
#   , lapply(.SD, sum, na.rm = TRUE),
#   .SDcols = flag_cols
# ]
# print(flag_month_counts)

# check = ust_monthly[,.(obs = .N),by=.(Last_YearMonth)]

# exits_by_year = panel[is_facility_exit_event == 1, .N, by = YEAR]

# 3. Print out the examples
cat("
Example facilities for visual checks (from ust_monthly):
  1) closed after 1999 but before 2026: ", example1, "
  2) entered before 1990 and exited before 1999: ", example2, "
  3) entered after 1999: ", example3, "
  4) open before 1990 and still open after 1999: ", example4, "
")

# 4. Now inspect these in the full panel if desired:
    view(panel[FACILITY_ID == example1])
    view(panel[FACILITY_ID == example2])
    view(panel[FACILITY_ID == example3])
    view(panel[FACILITY_ID == example4])


# Create a lookup for column definitions
# This is a manual step based on the script's logic.
column_definitions_list <- list(
  # == Panel Core Identifiers ==
  FACILITY_ID = "character: Unique identifier for the facility.",
  YEAR = "integer: Year of the observation.",
  MONTH = "integer: Month of the observation (1-12).",

  # == Financial Assurance (from fa_monthly) ==
  ISSUER_NAME = "character: Concatenated list of financial mechanism issuer names active in the month. Values are ' | ' separated if multiple. 'NO COVERAGE' if coverage_gap_month is TRUE.",
  DETAIL_TYPE = "character: Concatenated list of financial mechanism detail types active in the month. Values are '; ' separated if multiple. 'NO COVERAGE' if coverage_gap_month is TRUE.",
  CATEGORY = "character: Concatenated list of financial mechanism categories active in the month. Values are '; ' separated if multiple. 'NO COVERAGE' if coverage_gap_month is TRUE.",
  active_FIN_ASSUR_ID = "character: Concatenated list of active financial assurance IDs (FIN_ASSUR_ID) in the month. Values are ' | ' separated if multiple.",
  max_COVER_OCC = "numeric: Maximum per-occurrence coverage amount from financial mechanisms active in the month. NA if no coverage or not applicable.",
  max_COVER_AGG = "numeric: Maximum annual aggregate coverage amount from financial mechanisms active in the month. NA if no coverage or not applicable.",
  total_COVER_OCC = "numeric: Sum of per-occurrence coverage amounts from all financial mechanisms active in the month. 0 if no coverage.",
  total_COVER_AGG = "numeric: Sum of annual aggregate coverage amounts from all financial mechanisms active in the month. 0 if no coverage.",
  premium_prepaid = "logical: TRUE if any active financial mechanism had its premium pre-paid.",
  proof_on_file = "logical: TRUE if proof of financial assurance was on file for any active mechanism.",
  fp_corr_met = "logical: TRUE if first-party corrective action requirements were met by any active mechanism.",
  tp_fa_met = "logical: TRUE if third-party financial assurance requirements were met by any active mechanism.",
  uses_private = "logical: TRUE if 'Insurance' category was active in the month (based on CATEGORY string).",
  uses_self = "logical: TRUE if 'Self-Insurance' category was active in the month (based on CATEGORY string).",
  fr_covered = "logical: TRUE if any financial responsibility coverage was active in the month (i.e., not a coverage gap).",
  transition_month = "logical: TRUE if the month contains a hand-off between exactly two non-overlapping financial mechanisms.",
  multiple_contracts = "logical: TRUE if multiple (potentially overlapping) financial mechanisms were active in the month.",
  erp_reporting_month = "logical: TRUE if the facility-month falls within the 6-month extended reporting period (ERP) after a contract expiry.",
  coverage_gap_month = "logical: TRUE if no FR contract covers the facility-month and it's outside any ERP tail.",
  issuer_changed = "logical: TRUE if the concatenated ISSUER_NAME string changed from the previous month.",
  mech_changed = "logical: TRUE if the concatenated CATEGORY string changed from the previous month.",
  contract_changed = "logical: TRUE if either issuer_changed or mech_changed is TRUE.",
  changed_to_self_insure = "logical: TRUE if CATEGORY changed to include 'Self-Insurance' and did not previously include it.",
  changed_to_insurance = "logical: TRUE if CATEGORY changed to include 'Insurance' and did not previously include it.",
  changed_to_no_coverage = "logical: TRUE if the facility-month transitioned into a 'coverage_gap_month' from a non-gap month.",

  # == Facility Information (from fac) ==
  FACILITY_NAME = "character: Name of the facility.",
  FACILITY_TYPE = "character: Type of the facility.",
  FACILITY_STATUS = "character: Status of the facility as reported in the facility extract.",
  SITE_COUNTY = "character: County where the facility is located.",
  SITE_ZIP = "character: ZIP code of the facility site.",
  NUM_ACTIVE_USTS_is_blank = "logical: TRUE if the original NUM_ACTIVE_USTS field was blank in the facility extract.",
  NUM_ACTIVE_ASTS_is_blank = "logical: TRUE if the original NUM_ACTIVE_ASTS field was blank in the facility extract.",
  fips = "character: 5-digit FIPS code for the facility's county.",

  # == Owner/Operator Information (from fac_contact_months) ==
  owner_ids_concatenated = "character: Semicolon-separated list of unique owner CNs for the facility-month.",
  owner_names_concatenated = "character: Pipe-separated list of unique owner names (CO_LAST) for the facility-month.",
  owner_types_concatenated = "character: Semicolon-separated list of unique owner types for the facility-month.",
  state_tax_ids_concatenated = "character: Semicolon-separated list of unique state tax IDs for owners in the facility-month.",
  any_owner_multi_facility = "logical: TRUE if any owner active in the facility-month owns multiple facilities.",
  num_distinct_owners = "integer: Number of distinct owners (by OWNER_CN) for the facility-month.",
  operator_ids_concatenated = "character: Semicolon-separated list of unique operator CNs for the facility-month.",
  operator_names_concatenated = "character: Pipe-separated list of unique operator names (CO_LAST) for the facility-month.",
  owner_operator_different = "logical: TRUE if the set of owner names and operator names do not fully overlap in the facility-month, or one is empty and the other is not.",
  orphaned_by_owner = "logical: TRUE if owner_ids_concatenated is NA or empty for the facility-month.",
  orphaned_by_operator = "logical: TRUE if operator_ids_concatenated is NA or empty for the facility-month.",

  # == UST Aggregations (from ust_monthly) ==
  total_capacity = "numeric: Sum of capacities (gallons) of all USTs active at the facility in the month.",
  active_tank_count = "integer: Number of unique USTs active at the facility in the month.",
  closed_tank_count = "integer: Number of USTs whose closure (due to 'REMOVED', 'CLOSED' etc. status in raw UST data) was effective in this facility-month.",
  prop_single_wall = "numeric: Proportion of active USTs in the month that are single-walled (based on TANK_SINGLE flag). 0-1 scale.",
  prop_double_wall = "numeric: Proportion of active USTs in the month that are double-walled (based on TANK_DOUBLE flag). 0-1 scale.",
  avg_tank_age_months = "numeric: Average age (in months from their install_date) of USTs active in the month.",
  num_sw_tanks = "integer: Number of active single-walled tanks (TANK_SINGLE='Y') in the facility-month.",
  num_dw_tanks = "integer: Number of active double-walled tanks (TANK_DOUBLE='Y') in the facility-month.",
  num_missing_tanks = "integer: Number of active tanks where both TANK_SINGLE and TANK_DOUBLE are NA in the facility-month.",
  num_unknown_tanks = "integer: Number of active tanks where TANK_SINGLE='N' and TANK_DOUBLE='N' in the facility-month.",
  dw_upgrade_event = "integer: 1 if there was an increase in num_dw_tanks and a corresponding decrease in (num_sw_tanks + num_unknown_tanks + num_missing_tanks) from the previous month, 0 otherwise.",
  num_steel_tanks = "integer: Number of active tanks made of steel in the facility-month.",
  num_fiberglass_tanks = "integer: Number of active tanks made of fiberglass (FRP) in the facility-month.",
  num_composite_tanks = "integer: Number of active tanks made of composite material in the facility-month.",
  num_concrete_tanks = "integer: Number of active tanks made of concrete in the facility-month.",
  num_jacketed_tanks = "integer: Number of active jacketed tanks in the facility-month.",
  num_coated_tanks = "integer: Number of active coated tanks in the facility-month.",
  num_unknown_material_tanks = "integer: Number of active tanks with unknown material in the facility-month.",
  # Substance counts (active tanks)
  num_gasoline_tanks = "integer: Number of active tanks storing gasoline in the facility-month.",
  num_diesel_tanks = "integer: Number of active tanks storing diesel in the facility-month.",
  num_oil_kerosene_tanks = "integer: Number of active tanks storing oil or kerosene in the facility-month.",
  num_jet_fuel_tanks = "integer: Number of active tanks storing jet fuel in the facility-month.",
  num_other_substance_tanks = "integer: Number of active tanks storing other substances in the facility-month.",
  total_tanks_with_substance_info = "integer: Total number of active tanks in the month for which substance information was available from compartment data.",
  # Detection summary counts (active tanks)
  num_interstitial_det = "integer: Number of active tanks with interstitial or secondary containment detection methods in the facility-month.",
  num_electronic_det = "integer: Number of active tanks with electronic detection methods (ATG, LLD) in the facility-month.",
  num_manual_stat_det = "integer: Number of active tanks with manual or statistical detection methods (manual gauging, SIR) in the facility-month.",
  num_any_det = "integer: Number of active tanks with any release detection method flagged in the facility-month.",
  # Individual detection method counts
  num_DEC_RP_VAPOR = "integer: Number of active tanks using compartment vapor monitoring in the facility-month.",
  num_DEC_RP_GW = "integer: Number of active tanks using compartment groundwater monitoring in the facility-month.",
  num_DEC_RP_SEC_CONT = "integer: Number of active tanks using compartment secondary containment monitoring in the facility-month.",
  num_DEC_RP_ATG = "integer: Number of active tanks using compartment automatic tank gauging in the facility-month.",
  num_DEC_RP_INTERSTITIAL = "integer: Number of active tanks using compartment interstitial monitoring in the facility-month.",
  num_DEC_RP_MANUAL_WEEK = "integer: Number of active tanks using weekly manual tank gauging for compartment in the facility-month.",
  num_DEC_RP_MANUAL_MONTH = "integer: Number of active tanks using monthly manual tank gauging for compartment in the facility-month.",
  num_DEC_RP_SIR = "integer: Number of active tanks using statistical inventory reconciliation for compartment in the facility-month.",
  num_DET_PIPE_VAPOR = "integer: Number of active tanks/piping systems using piping vapor monitoring in the facility-month.",
  num_DET_PIPE_GW = "integer: Number of active tanks/piping systems using piping groundwater monitoring in the facility-month.",
  num_DET_PIPE_SEC_CONT = "integer: Number of active tanks/piping systems using piping secondary containment monitoring in the facility-month.",
  num_DET_PIPE_INTERSTITIAL = "integer: Number of active tanks/piping systems using piping interstitial monitoring in the facility-month.",
  num_DET_PIPE_TT_MONTH = "integer: Number of active tanks/piping systems using monthly tightness testing for piping in the facility-month.",
  num_DET_PIPE_TT_ANNUAL = "integer: Number of active tanks/piping systems using annual tightness testing for piping in the facility-month.",
  num_DET_PIPE_TT_TRIEN = "integer: Number of active tanks/piping systems using triennial tightness testing for piping in the facility-month.",
  num_DET_PIPE_LLD = "integer: Number of active tanks/piping systems using automatic line leak detectors in the facility-month.",
  num_DET_PIPE_SIR = "integer: Number of active tanks/piping systems using statistical inventory reconciliation for piping in the facility-month.",
  num_DET_PIPE_SUCTION_EXEMPT = "integer: Number of active tanks/piping systems exempt due to suction piping in the facility-month.",

  # == Self-Certification (from self_cert_monthly) ==
  FA_FLAG = "logical: TRUE if self-certification (signed in the year of this month) indicated financial assurance requirements were met.",
  TECH_FLAG = "logical: TRUE if self-certification (signed in the year of this month) indicated technical standards were met.",

  # == LUST Information ==
  is_lust_report_month = "integer: 1 if a LUST incident was reported for the facility in this specific month, 0 otherwise.",
  lust_active = "logical: TRUE if a LUST incident was active (reported and not yet NFA) at the facility in the month.",
  lust_pre_2000_active = "logical: TRUE if an active LUST incident (reported before 2000-01-01) was ongoing in the month.",
  lust_post_2000_active = "logical: TRUE if an active LUST incident (reported on or after 2000-01-01) was ongoing in the month.",
  has_subsidized_leak_active = "logical: TRUE if an active LUST incident (reported before 2000-01-01, thus potentially subsidized) was ongoing in the month.",
  lust_open_active = "logical: TRUE if any LUST incident active in this month was still open (NFA date is in the future or NA).",
  lust_closed_active = "logical: TRUE if any LUST incident active in this month had its NFA date occur by this month.",
  lust_ever = "logical: TRUE if the facility ever had any LUST incident reported (cumulative).",
  lust_pre_2000 = "logical: TRUE if the facility ever had a LUST incident reported before 2000-01-01 (cumulative).",
  lust_post_2000 = "logical: TRUE if the facility ever had a LUST incident reported on or after 2000-01-01 (cumulative).",
  has_subsidized_leak = "logical: TRUE if the facility ever had a LUST incident reported before 2000-01-01 (cumulative, proxy for subsidy eligibility).",
  lust_ever_closed = "logical: TRUE if any LUST incident at the facility has ever received an NFA date (cumulative).",
  lust_ever_open = "logical: TRUE if any LUST incident at the facility was ever reported and currently has no NFA date or its NFA date is in the future (cumulative).",
  lust_after_closure_ever = "integer: 1 if the facility ever had a LUST incident reported after a previous NFA date for another LUST at the same facility, 0 otherwise (cumulative).",

  # == Facility Status Variables (derived in panel) ==
  is_tceq_reported_closed = "integer: 1 if FACILITY_STATUS from the facility extract is 'CLOSED', 0 otherwise.",
  is_tceq_reported_inactive = "integer: 1 if FACILITY_STATUS from the facility extract is 'INACTIVE', 0 otherwise.",
  is_tceq_reported_temporarily_out_of_service = "integer: 1 if FACILITY_STATUS from the facility extract is 'TEMPORARILY OUT OF SERVICE', 0 otherwise.",
  any_tank_active = "logical: TRUE if active_tank_count > 0 for the facility-month.",
  Closed_Inventory_YearMonth = "character: YYYY-MM of the last month the facility had active tanks based on panel's active_tank_count. NA if never active.",
  closure_year = "integer: Year extracted from Closed_Inventory_YearMonth. NA if never active or if Closed_Inventory_YearMonth is NA.",
  closure_month = "integer: Month extracted from Closed_Inventory_YearMonth. NA if never active or if Closed_Inventory_YearMonth is NA.",
  is_active_pre_inventory_closure = "integer: 1 if the facility-month is on or before the Closed_Inventory_YearMonth, 0 otherwise. 0 if never active or Closed_Inventory_YearMonth is NA.",
  is_facility_entry_event = "logical: TRUE for the first month a facility transitions from having no active tanks (any_tank_active=FALSE) to having active tanks (any_tank_active=TRUE).",
  is_facility_exit_event = "logical: TRUE for the first month a facility transitions from active to inactive pre-inventory closure.",
  closure_not_final_event = "integer: 1 if `lust_after_closure_ever` is 1, 0 otherwise. Indicates a LUST event occurred after a supposed NFA.",

  # == Zurich-Related Flags ==
  had_zurich_2012 = "integer: 1 if the facility had a Zurich insurance policy active at any point during CY 2012, 0 otherwise.",
  dropped_by_zurich = "integer: 1 if `had_zurich_2012` is 1 and the panel YEAR is >= 2013, 0 otherwise.",

  # == Capacity Change Metrics ==
  capacity_delta_gal = "numeric: Change in total_capacity (gallons) from the previous month. 0 for the first month of a facility.",
  capacity_change_event = "integer: 1 if capacity_delta_gal is not 0, 0 otherwise. 0 for the first month of a facility.",

  # == Device & Material Change Events ==
  num_interstitial_det_delta = "numeric: Change in num_interstitial_det from the previous month. 0 for the first month of a facility.",
  num_interstitial_det_change_event = "integer: 1 if num_interstitial_det changed from the previous month, 0 otherwise. 0 for the first month of a facility.",
  num_electronic_det_delta = "numeric: Change in num_electronic_det from the previous month. 0 for the first month of a facility.",
  num_electronic_det_change_event = "integer: 1 if num_electronic_det changed from the previous month, 0 otherwise. 0 for the first month of a facility.",
  num_manual_stat_det_delta = "numeric: Change in num_manual_stat_det from the previous month. 0 for the first month of a facility.",
  num_manual_stat_det_change_event = "integer: 1 if num_manual_stat_det changed from the previous month, 0 otherwise. 0 for the first month of a facility.",
  num_steel_tanks_delta = "numeric: Change in num_steel_tanks from the previous month. 0 for the first month of a facility.",
  num_steel_tanks_change_event = "integer: 1 if num_steel_tanks changed from the previous month, 0 otherwise. 0 for the first month of a facility.",
  num_fiberglass_tanks_delta = "numeric: Change in num_fiberglass_tanks from the previous month. 0 for the first month of a facility.",
  num_fiberglass_tanks_change_event = "integer: 1 if num_fiberglass_tanks changed from the previous month, 0 otherwise. 0 for the first month of a facility.",
  # Add active_tank_count definition
  active_tank_count = "integer: Number of unique USTs active at the facility in the month, calculated during UST monthly aggregation."
)

# Create data.table from the list for easy lookup or writing to CSV
column_dictionary_dt <- data.table(
  column_name = names(column_definitions_list),
  description = unlist(column_definitions_list)
)

# Optional: Add a generic definition for dummy columns if they are too numerous to list individually
# Note: The script dynamically creates these. You would need to identify them in the panel to add them here.
# For example, if all dummy columns start with "ISSUER_NAME__", "CATEGORY__", or "DETAIL_TYPE__":
# dummy_cols_in_panel <- names(panel)[grepl("^(ISSUER_NAME__|CATEGORY__|DETAIL_TYPE__)", names(panel))]
# for (d_col in dummy_cols_in_panel) {
#   if (!d_col %in% column_dictionary_dt$column_name) {
#     var_type <- sub("__.*", "", d_col)
#     val_slug <- sub(".*?__", "", d_col)
#     desc <- paste0("integer: 1 if an active financial mechanism in the month had ", var_type, " '", gsub("_", " ", val_slug), "', 0 otherwise.")
#     column_dictionary_dt <- rbind(column_dictionary_dt, data.table(column_name = d_col, description = desc))
#   }
# }

# Save the dictionary
fwrite(column_dictionary_dt, get_data_path("Outputs", "texas_fr_panel_column_dictionary.csv"))
message("Column dictionary saved to ", get_data_path("Outputs", "texas_fr_panel_column_dictionary.csv"))

# Final check: ensure all columns in the panel are in the dictionary (optional, for development)
# missing_from_dict <- setdiff(names(panel), column_dictionary_dt$column_name)
# if (length(missing_from_dict) > 0) {
#   warning(paste("Columns in panel missing from dictionary:", paste(missing_from_dict, collapse=", ")))
# }
# not_in_panel <- setdiff(column_dictionary_dt$column_name, names(panel))
# if (length(not_in_panel) > 0) {
#   warning(paste("Columns in dictionary not found in panel:", paste(not_in_panel, collapse=", ")))
#   # This might indicate definitions for columns that were dropped or renamed.
# }

message("\n=== SAMPLE PANEL OUTPUT FOR ONE FACILITY ===")
# ...existing code...

# ###############################################################################
# ## A. RAW UST COUNTS – tanks active ≥ 1990 at FR-required facilities
# ###############################################################################
# library(data.table)

# # 1.  keep only tanks whose life-span stretches past 1 Jan 1990
# ust_post90 <- ust[end_date >= as.Date("1990-01-01")]

# # 2.  keep only facilities that must carry UST financial-responsibility
# fac_fr     <- fac[UST_FR_REQUIRED == TRUE, .(FACILITY_ID)]

# # 3.  per-facility number of unique TANK_IDs
# raw_ust_cnt <- ust_post90[fac_fr,           # semi-join on FACILITY_ID
#    on = "FACILITY_ID"
#  ][, .(raw_usts_post90 = uniqueN(UST_ID)), # uniqueN() is fastest in data.table :contentReference[oaicite:0]{index=0}
#    by = FACILITY_ID]

# ###############################################################################
# ## B. PANEL COUNTS – same idea but from the finished monthly panel
# ###############################################################################
# # 1.  restrict to 1990+
# panel_1990 <- panel[YEAR >= 1990]

# # 2.  for each facility take the **maximum** reported active-tank count
# #     (that’s the size of the UST inventory that survives into the panel era)
# panel_ust_cnt <- panel_1990[,
#    .(panel_usts_post90 = max(active_tank_count, na.rm = TRUE)), 
#    by = FACILITY_ID]

# ###############################################################################
# ## C. COMPARE THE TWO ESTIMATES
# ###############################################################################
# compare_tbl <- merge(raw_ust_cnt, panel_ust_cnt, by = "FACILITY_ID", all = TRUE)
# compare_tbl[, diff := raw_usts_post90 - panel_usts_post90]

# ## quick summary
# summary_tbl <- compare_tbl[, .(
#   facilities           = .N,
#   matching             = sum(diff == 0, na.rm = TRUE),
#   mismatching          = sum(diff != 0, na.rm = TRUE),
#   mean_abs_difference  = mean(abs(diff), na.rm = TRUE)
# )]

# print(summary_tbl)
# print(head(compare_tbl[diff != 0], 20))   # peek at first mismatches

# ###############################################################################
# ## D. DID WE PUT FACILITY START DATE IN THE PANEL ?
# ###############################################################################

# rm(ust_post90, fac_fr, raw_ust_cnt, panel_1990, panel_ust_cnt)  # tidy up


# ## --- randomly draw 10 distinct facilities for spot-checking ---------------
# set.seed(42)                                # ⇐ makes the draw reproducible
# sample_fac_ids <- sample(unique(fac$FACILITY_ID), 10)

# cat("Random sample of 10 FACILITY_IDs:\n")
# print(sample_fac_ids)
# panel_RETAIL = panel[FACILITY_TYPE == "RETAIL", .(FACILITY_ID, YEAR, MONTH, FACILITY_NAME,ISSUER_NAME,CATEGORY,avg_tank_age_months ,SITE_COUNTY, fips, total_capacity, tank_count, active_tank_count, closed_tank_count,lust_active,lust_open_active,lust_closed_active)]

# ## optional: grab *all* rows for those facilities from any table you like
# #   (here I pull from the finished `panel` object)
# inspect_sample <- panel[FACILITY_ID %in% sample_fac_ids ]

# setkey(inspect_sample, FACILITY_ID, YEAR, MONTH)  # sort by facility and time
# view(inspect_sample)  # view in RStudio viewer
# # randomly pull 10 retail facilities for inspection
# sample_retail_fac_ids <- sample(unique(panel_RETAIL$FACILITY_ID), 10)

# inspect_retail_sample <- panel_RETAIL[FACILITY_ID %in% sample_retail_fac_ids ]

# setkey(inspect_retail_sample, FACILITY_ID, YEAR, MONTH)  # sort by facility and time
# view(inspect_retail_sample)  # view in RStudio viewer
# # count unique number  obs per  fips
# fips_obs = panel[, .(unique_obs = uniqueN(FACILITY_ID)), by = .(fips,SITE_COUNTY)]
# view(fips_obs)
# message("Validation checks passed (or warnings issued).")

# kims_b = ust[FACILITY_ID =='95972']
# view(kims_b)



# ###############################################################################
# # collapse_logic() — collapse contract-level rows to one facility-month row
# # --------------------------------------------------------------------------
# # Expects: `dt` = all contracts for ONE facility in ONE year–month
# # Returns: single-row data.table with aggregates + flags that downstream code
# #          (change-indicators, dummy expansion, etc.) relies on.
# #
# #  ➤ NEW (June 2024):
# #      • carries forward `coverage_gap_month` so later blocks can use it
# ###############################################################################
# collapse_logic <- function(dt) {

#   dt <- data.table::copy(dt)        # unlock .SD
#   n  <- nrow(dt)                    # how many contracts this month?

#   ## ---- group-level aggregates used in every branch -------------------------
#   agg <- list(
#     DETAIL_TYPE         = paste(sort(unique(dt$DETAIL_TYPE)), collapse = "; "),
#     CATEGORY            = paste(sort(unique(dt$CATEGORY)),    collapse = "; "),
#     ISSUER_NAME         = paste(sort(unique(dt$ISSUER_NAME)), collapse = " | "),
#     max_COVER_OCC       = if (all(is.na(dt$COVER_OCC))) NA_real_
#                           else max(dt$COVER_OCC, na.rm = TRUE),
#     max_COVER_AGG       = if (all(is.na(dt$COVER_AGG))) NA_real_
#                           else max(dt$COVER_AGG, na.rm = TRUE),
#     total_COVER_OCC     = sum(dt$COVER_OCC, na.rm = TRUE),
#     total_COVER_AGG     = sum(dt$COVER_AGG, na.rm = TRUE),
#     premium_prepaid     = any(dt$premium_prepaid, na.rm = TRUE),
#     proof_on_file       = any(dt$proof_on_file,   na.rm = TRUE),
#     fp_corr_met         = any(dt$fp_corr_met,     na.rm = TRUE),
#     tp_fa_met           = any(dt$tp_fa_met,       na.rm = TRUE),
#     uses_private        = any(dt$uses_private,    na.rm = TRUE),
#     uses_self           = any(dt$uses_self,       na.rm = TRUE),
#     coverage_gap_month  = any(dt$coverage_gap_month)   # ← NEW
#   )

#   ###########################################################################
#   # Branch 1 : single contract ----------------------------------------------
#   ###########################################################################
#   if (n == 1L) {
#     row <- dt[1L]
#     row[, `:=`(
#       transition_month    = FALSE,
#       multiple_contracts  = FALSE,
#       active_FIN_ASSUR_ID = FIN_ASSUR_ID,
#       fr_covered          = TRUE,            # by construction
#       DETAIL_TYPE         = agg$DETAIL_TYPE,
#       CATEGORY            = agg$CATEGORY,
#       ISSUER_NAME         = agg$ISSUER_NAME,
#       max_COVER_OCC       = agg$max_COVER_OCC,
#       max_COVER_AGG       = agg$max_COVER_AGG,
#       total_COVER_OCC     = agg$total_COVER_OCC,
#       total_COVER_AGG     = agg$total_COVER_AGG,
#       premium_prepaid     = agg$premium_prepaid,
#       proof_on_file       = agg$proof_on_file,
#       fp_corr_met         = agg$fp_corr_met,
#       tp_fa_met           = agg$tp_fa_met,
#       uses_private        = agg$uses_private,
#       uses_self           = agg$uses_self,
#       coverage_gap_month  = agg$coverage_gap_month     # ← NEW
#     )]
#     return(row[])
#   }

#   ###########################################################################
#   # Branch 2 : exactly two contracts, NO overlap  (clean hand-off) ----------
#   ###########################################################################
#   ord  <- order(dt$start_day_in_month)
#   sNum <- as.numeric(dt$start_day_in_month[ord])
#   eNum <- as.numeric(dt$end_day_in_month[ord])
#   overlaps <- any(na.omit(sNum[-1] <= cummax(eNum)[-n]))

#   if (n == 2L && !overlaps) {
#     idx <- which.max(dt$start_day_in_month)         # the one that begins
#     row <- dt[idx]
#     row[, `:=`(
#       transition_month    = TRUE,
#       multiple_contracts  = FALSE,
#       active_FIN_ASSUR_ID = FIN_ASSUR_ID,
#       fr_covered          = TRUE,
#       DETAIL_TYPE         = agg$DETAIL_TYPE,
#       CATEGORY            = agg$CATEGORY,
#       ISSUER_NAME         = agg$ISSUER_NAME,
#       max_COVER_OCC       = agg$max_COVER_OCC,
#       max_COVER_AGG       = agg$max_COVER_AGG,
#       total_COVER_OCC     = agg$total_COVER_OCC,
#       total_COVER_AGG     = agg$total_COVER_AGG,
#       premium_prepaid     = agg$premium_prepaid,
#       proof_on_file       = agg$proof_on_file,
#       fp_corr_met         = agg$fp_corr_met,
#       tp_fa_met           = agg$tp_fa_met,
#       uses_private        = agg$uses_private,
#       uses_self           = agg$uses_self,
#       coverage_gap_month  = agg$coverage_gap_month     # ← NEW
#     )]
#     return(row[])
#   }

#   ###########################################################################
#   # Branch 3 : ≥2 overlapping contracts -------------------------------------
#   ###########################################################################
#   row <- dt[1L]
#   row[, `:=`(
#     transition_month    = FALSE,
#     multiple_contracts  = TRUE,
#     active_FIN_ASSUR_ID = paste(sort(unique(dt$FIN_ASSUR_ID)), collapse = " | "),
#     fr_covered          = TRUE,
#     DETAIL_TYPE         = agg$DETAIL_TYPE,
#     CATEGORY            = agg$CATEGORY,
#     ISSUER_NAME         = agg$ISSUER_NAME,
#     max_COVER_OCC       = agg$max_COVER_OCC,
#     max_COVER_AGG       = agg$max_COVER_AGG,
#     total_COVER_OCC     = agg$total_COVER_OCC,
#     total_COVER_AGG     = agg$total_COVER_AGG,
#     premium_prepaid     = agg$premium_prepaid,
#     proof_on_file       = agg$proof_on_file,
#     fp_corr_met         = agg$fp_corr_met,
#     tp_fa_met           = agg$tp_fa_met,
#     uses_private        = agg$uses_private,
#     uses_self           = agg$uses_self,
#     coverage_gap_month  = agg$coverage_gap_month     # ← NEW
#   )]
#   return(row[])
# }


# ###############################################################################
# # 2.  Contract-monthly expansion WITH day-level edges (now keeps ISSUER_NAME) -
# ###############################################################################
# fa_monthly_contract <- fa[
#   !is.na(EFF_DATE) & !is.na(EXP_DATE) & EFF_DATE <= EXP_DATE,
#   {
#     mo_seq <- seq(
#       floor_date(EFF_DATE, "month"),
#       floor_date(pmin(EXP_DATE, as.Date("2026-12-01")), "month"),
#       by = "month"
#     )

#     first_in_mo <- pmax(EFF_DATE, floor_date(mo_seq, "month"))
#     last_in_mo  <- pmin(EXP_DATE, ceiling_date(mo_seq, "month") - 1)

#     .(
#       YEAR  = year(mo_seq),
#       MONTH = month(mo_seq),

#       ## <-- carry these through:
#       EFF_DATE = EFF_DATE,
#       EXP_DATE = EXP_DATE,

#       start_day_in_month = first_in_mo,
#       end_day_in_month   = last_in_mo,

#       DETAIL_TYPE, CATEGORY,
#       COVER_OCC,  COVER_AGG,
#       ISSUER_NAME,
#       premium_prepaid = PREMIUM_PREPAID,
#       proof_on_file   = PROOF_OF_FA,
#       fp_corr_met     = FP_CORR_MET,
#       tp_fa_met       = TP_FA_MET,
#       uses_private    = USE_PRIVATE_mapped,
#       uses_self       = USE_SELF_mapped,
#       fr_covered      = TRUE
#     )
#   },
#   by = .(FACILITY_ID, FIN_ASSUR_ID)
# ]