#==============================================================================
# 10_Build_Master_Annual_Panel_Refactored.R
# Purpose: Create a clean FACILITY-YEAR panel from HARMONIZED DATASETS.
#          Refactored for new pipeline schema (Facility-Tank Inventory).
#
# Inputs:
#   1. Master_Harmonized_UST_Tanks.csv
#   2. Master_Harmonized_LUST.csv
#   3. fa_monthly.csv (Texas FR)
#   4. zurich_2012_lookup.csv (Texas FR)
#
# Outputs:
#   1. facility_leak_behavior_annual.csv (Final Analysis Panel)
#   2. facility_leak_behavior_annual.rds (Fast-load version)
#   3. facility_leak_behavior_monthly_intermediate.rds
#==============================================================================

script_start_time <- Sys.time()

# Load necessary libraries
suppressPackageStartupMessages({
  library(data.table)  # Core data manipulation
  library(lubridate)   # Date parsing and handling
  library(here)        # Project-relative file paths
  library(tidyverse)   # Utilities
  library(zoo)         # For na.locf
  library(stringr)     # String manipulation
})

# Set threads for data.table
setDTthreads(14) 

# ==============================================================================
# SECTION 0: HELPER FUNCTIONS
# ==============================================================================

#' Coerce to Date
#' Safely converts various formats to standard Date class.
coerce_date <- function(x) {
  if (inherits(x, "Date")) return(x)
  # Handle IDate or character
  as.Date(x, origin = "1970-01-01")
}

#' Log Step
#' Logging helper for structured console output.
log_step <- function(message, indent = 0) {
  prefix <- paste(rep("  ", indent), collapse = "")
  cat(sprintf("%s%s\n", prefix, message))
}

#' Log Memory
log_memory <- function(label) {
  gc_info <- gc()
  cat(sprintf("  [Memory] %s: %.1f MB used\n", 
              label, sum(gc_info[, "used"]) / 1024))
}

#==============================================================================
# SECTION 1: LOAD HARMONIZED DATA
#==============================================================================
cat("========================================\n")
cat("SECTION 1: LOADING HARMONIZED DATA\n")
cat("========================================\n\n")

# 1.1 Load Master Tanks
log_step("Loading Master Harmonized Tanks...", 0)
master_tanks <- fread(here("Data", "Processed", "Master_Harmonized_UST_Tanks.csv"))

# 1.2 Load Master LUST
log_step("Loading Master Harmonized LUST...", 0)
master_lust <- fread(here("Data", "Processed", "Master_Harmonized_LUST.csv"))

# ==============================================================================
# SECTION 1.3: LOAD TEXAS FR DATA (CORRECTED)
# ==============================================================================
log_step("Loading Texas FR Auxiliary Data...", 0)

# FIX 1: Point to Data/Processed and use correct filename 'texas_fr_facility_month_panel.csv'
fa_monthly <- fread(here("Data", "Processed", "texas_fr_facility_month_panel.csv"))

# FIX 2: Remove 'dropped_by_zurich' from base cols (it is calculated later in this script)
fa_base_cols <- c("FACILITY_ID", "YEAR", "MONTH", "DETAIL_TYPE", "CATEGORY", 
                  "uses_private", "uses_self", "fr_covered", 
                  "transition_month", "multiple_contracts") 

# Dynamically identify variable groups
fa_issuer_cols <- grep("^ISSUER_NAME", names(fa_monthly), value = TRUE)
fa_cat_dummies <- grep("^CATEGORY_", names(fa_monthly), value = TRUE)
fa_det_dummies <- grep("^DETAIL_TYPE_", names(fa_monthly), value = TRUE)
fa_cover_vars  <- grep("COVER_", names(fa_monthly), value = TRUE)

# Combine and subset
fa_keep <- unique(c(fa_base_cols, fa_issuer_cols, fa_cat_dummies, fa_det_dummies, fa_cover_vars))
fa_keep <- intersect(fa_keep, names(fa_monthly)) # Safety check
fa_monthly <- fa_monthly[, .SD, .SDcols = fa_keep]

log_step(sprintf("FA monthly: %s rows loaded", format(nrow(fa_monthly), big.mark=",")), 1)

# FIX 1b: Load Zurich lookup from Processed
zurich_2012_lookup <- fread(here("Data", "Processed", "zurich_2012_lookup.csv"))

log_step("Data loading complete.", 1)


#==============================================================================
# SECTION 2: DATA CLEANING & PANEL PREP
#==============================================================================
cat("\n========================================\n")
cat("SECTION 2: CLEANING & FILTERING\n")
cat("========================================\n\n")

# ==============================================================================
# SECTION 2.1: STANDARDIZE IDs (CORRECTED)
# ==============================================================================
log_step("Creating composite panel_id...", 0)

# Master Tanks
master_tanks[, `:=`(
  facility_id = toupper(trimws(as.character(facility_id))),
  tank_id     = toupper(trimws(as.character(tank_id))),
  state       = toupper(trimws(as.character(state)))
)]
master_tanks[, panel_id := paste(facility_id, state, sep = "_")]

# Master LUST
master_lust[, `:=`(
  facility_id = toupper(trimws(as.character(facility_id))),
  state       = toupper(trimws(as.character(state)))
)]
master_lust[, panel_id := paste(facility_id, state, sep = "_")]

# FA Monthly - FIX 3: Do NOT strip leading zeros. Match Master Tanks logic exactly.
log_step("Standardizing FA Monthly IDs...", 1)
fa_monthly[, clean_id := toupper(trimws(as.character(FACILITY_ID)))] 
fa_monthly[, `:=`(
  facility_id = clean_id,
  panel_id    = paste(clean_id, "TX", sep = "_") 
)]
fa_monthly[, clean_id := NULL]

# Zurich Lookup - FIX 3b: Do NOT strip leading zeros.
log_step("Standardizing Zurich IDs...", 1)
zurich_2012_lookup[, clean_id := toupper(trimws(as.character(FACILITY_ID)))]
zurich_2012_lookup[, `:=`(
  facility_id = clean_id,
  panel_id    = paste(clean_id, "TX", sep = "_")
)]
zurich_2012_lookup[, clean_id := NULL]

# 2.2 Date Type Standardization
log_step("Standardizing date types...", 0)
master_tanks[, `:=`(
  tank_installed_date = coerce_date(tank_installed_date),
  tank_closed_date    = coerce_date(tank_closed_date)
)]
master_lust[, report_date := coerce_date(report_date)]

#==============================================================================
# SECTION 2.2b: FACILITY BIOGRAPHY (BEFORE ANY FILTERING)
# Must be computed from full master_tanks before Section 2.3 drops facilities.
# fac_max_install_date drives replacement classification in Section 5.2 --
# if a tank install that happened AFTER a closure gets dropped by 2.3, we
# would misclassify that replacement closure as permanent.
#==============================================================================

fac_bio <- master_tanks[, .(
  facility_first_install      = min(tank_installed_date, na.rm = TRUE),
  facility_last_install       = max(tank_installed_date, na.rm = TRUE),
  facility_first_closure      = if (any(!is.na(tank_closed_date)))
                                  min(tank_closed_date, na.rm = TRUE) else as.IDate(NA),
  facility_last_closure       = if (any(!is.na(tank_closed_date)))
                                  max(tank_closed_date, na.rm = TRUE) else as.IDate(NA),
  total_tanks_ever            = .N,
  total_tanks_closed_ever     = sum(!is.na(tank_closed_date)),
  total_tanks_open_ever       = sum(is.na(tank_closed_date)),
  had_pre1989_tanks           = as.integer(any(year(tank_installed_date) < 1989, na.rm = TRUE)),
all_tanks_post1988 = as.integer(  sum(!is.na(tank_installed_date)) > 0 &  all(year(tank_installed_date) >= 1989, na.rm = TRUE)),
  fac_max_install_date        = max(tank_installed_date, na.rm = TRUE)
), by = panel_id]

# Derived scalars
fac_bio[, `:=`(
  facility_first_install_yr = year(facility_first_install),
  facility_last_install_yr  = year(facility_last_install)
)]

fac_bio[, fac_reg_vintage := fcase(
  facility_first_install_yr < 1988,                    "Pre-RCRA",
  facility_first_install_yr %between% c(1988, 1998),   "Transition",
  facility_first_install_yr > 1998,                    "Post-Deadline",
  default = "Unknown"
)]

fac_bio[, fac_is_incumbent := as.integer(facility_first_install_yr < 1999)]

saveRDS(fac_bio, here("Data", "Analysis", "facility_biography.rds"))
log_step(sprintf("Saved: Data/Analysis/facility_biography.rds (%s facilities, pre-filter)",
  format(nrow(fac_bio), big.mark=",")), 1)
# Add this line immediately before Section 2.3
master_tanks_unfiltered <- copy(master_tanks)

# 2.3 Strict Filtering Rules
log_step("Applying strict facility filtering rules...", 0)
n_fac_orig <- uniqueN(master_tanks$panel_id)

# Rule 1: Drop facility if ANY tank is 'Closed' but missing closure date
problem_closed <- master_tanks[
  tolower(tank_status) == "closed" & is.na(tank_closed_date),
  .(panel_id)
]
ids_to_drop_closed <- unique(problem_closed$panel_id)

# Rule 2: Drop facility if ANY tank is 'Open' but missing install date
problem_open <- master_tanks[
  tolower(tank_status) == "open" & is.na(tank_installed_date),
  .(panel_id)
]
ids_to_drop_open <- unique(problem_open$panel_id)

ids_to_drop <- unique(c(ids_to_drop_closed, ids_to_drop_open))

if(length(ids_to_drop) > 0) {
  log_step(sprintf("  Dropping %s facilities due to missing dates on Open/Closed tanks", 
                   format(length(ids_to_drop), big.mark=",")), 1)
  master_tanks <- master_tanks[!panel_id %in% ids_to_drop]
}

n_fac_final <- uniqueN(master_tanks$panel_id)
log_step(sprintf("  Facilities Remaining: %s (%.1f%% retained)", 
                 format(n_fac_final, big.mark=","), 
                 100 * n_fac_final / n_fac_orig), 1)

# 2.4 Create Panel Bounded Dates (1970 - 2025)
log_step("Creating panel start/end date bounds...", 0)

PANEL_START <- as.IDate("1970-01-01") 
PANEL_END   <- as.IDate("2025-12-31")

# Create panel_start_date
master_tanks[, tank_panel_start_date := fifelse(
  is.na(tank_installed_date) | tank_installed_date < PANEL_START,
  PANEL_START,
  tank_installed_date
)]

# Create panel_end_date
master_tanks[, tank_panel_end_date := PANEL_END]
master_tanks[!is.na(tank_closed_date), tank_panel_end_date := pmin(tank_closed_date, PANEL_END)]

# Validation
master_tanks[, valid_panel_tank := tank_panel_end_date >= tank_panel_start_date]
active_panel_tanks <- master_tanks[valid_panel_tank == TRUE]

log_step(sprintf("  Tanks active within 1970-2025 window: %s", 
                 format(nrow(active_panel_tanks), big.mark=",")), 1)

#==============================================================================
# SECTION 2.5: FACILITY-LEVEL MAKE-MODEL CELL
# Classifies each facility by its portfolio characteristics as of Dec 1998.
# This is a SEPARATE classification from the tank-level cell in Step 3.5 of
# the harmonization script. The tank cell classifies the individual tank by
# immutable physical attributes. The facility cell classifies the facility
# by its portfolio-level premium burden at the reform date.
#==============================================================================
cat("\n========================================\n")
cat("SECTION 2.5: FACILITY-LEVEL MAKE-MODEL CELL\n")
cat("========================================\n\n")

log_step("Building facility-level make-model cells from 1998 portfolio snapshot...", 0)

REFORM_DATE <- as.IDate("1999-01-01")

# Tanks physically present at the reform date.
# Use tank_installed_date (exact); tank_panel_start_date is bounded to 1970.
tanks_at_reform <- active_panel_tanks[
  !is.na(tank_installed_date)                            &
  tank_installed_date  < REFORM_DATE                     &
  (is.na(tank_closed_date) | tank_closed_date >= REFORM_DATE)
]

log_step(sprintf("  Tanks at reform: %s across %s facilities",
  format(nrow(tanks_at_reform), big.mark=","),
  format(uniqueN(tanks_at_reform$panel_id), big.mark=",")), 1)

# Compute tank age at reform for oldest-tank calculation
tanks_at_reform[, tank_age_at_reform :=
  as.numeric(REFORM_DATE - tank_installed_date) / 365.25]

# Aggregate to facility level.
fac_mm <- tanks_at_reform[, .(
  n_tanks_at_reform = .N,

  # --- Portfolio Wall Composition ---
  # Unanimity rule: all agree = that type. Disagree = Mixed.
  # Unknown propagates if ANY tank is Unknown.
  fac_wall = fcase(
    any(mm_wall == "Unknown-Wall"),                                      "Unknown-Wall",
    all(mm_wall == "Single-Walled"),                                     "All-SW",
    all(mm_wall == "Double-Walled"),                                     "All-DW",
    default = "Mixed-Wall"
  ),

  # --- Portfolio Fuel Composition ---
  # Unknown only if ALL tanks are Unknown.
  fac_fuel = fcase(
    all(mm_fuel == "Unknown-Fuel"),                                      "Unknown-Fuel",
    all(mm_fuel == "Gasoline-Only"),                                     "Gasoline-Only",
    all(mm_fuel == "Diesel-Only"),                                       "Diesel-Only",
    default = "Mixed-Or-Other"
  ),

  # --- Oldest Tank Age Bin at Reform (3-year bins) ---
  # The oldest tank drives the facility's peak marginal premium.
  oldest_age_at_reform = max(tank_age_at_reform, na.rm = TRUE)

), by = panel_id]

# Bin the oldest tank age into 3-year intervals
fac_mm[, fac_oldest_age_bin := fcase(
  is.na(oldest_age_at_reform),                         NA_character_,
  oldest_age_at_reform < 3,                            "0-2yr",
  oldest_age_at_reform >= 3  & oldest_age_at_reform < 6,  "3-5yr",
  oldest_age_at_reform >= 6  & oldest_age_at_reform < 9,  "6-8yr",
  oldest_age_at_reform >= 9  & oldest_age_at_reform < 12, "9-11yr",
  oldest_age_at_reform >= 12 & oldest_age_at_reform < 15, "12-14yr",
  oldest_age_at_reform >= 15 & oldest_age_at_reform < 20, "15-19yr",
  oldest_age_at_reform >= 20,                          "20yr-Plus"
)]

# Facility-level cell identifier
fac_mm[, make_model_fac := fifelse(
  !is.na(fac_oldest_age_bin),
  paste(fac_wall, fac_fuel, fac_oldest_age_bin, sep = "_"),
  NA_character_
)]

log_step("  Facility make-model distribution:", 1)
print(fac_mm[, .N, by = .(fac_wall, fac_fuel)][order(-N)])
log_step(sprintf("  Unknown-Wall: %s (%.1f%%)  |  Unknown-Fuel: %s (%.1f%%)",
  format(fac_mm[fac_wall == "Unknown-Wall", .N], big.mark=","),
  100 * fac_mm[fac_wall == "Unknown-Wall", .N] / nrow(fac_mm),
  format(fac_mm[fac_fuel == "Unknown-Fuel", .N], big.mark=","),
  100 * fac_mm[fac_fuel == "Unknown-Fuel", .N] / nrow(fac_mm)), 1)
log_step("  Age bin distribution:", 1)
print(fac_mm[, .N, by = fac_oldest_age_bin][order(fac_oldest_age_bin)])

if (!dir.exists(here("Data", "Analysis"))) dir.create(here("Data", "Analysis"), recursive = TRUE)
saveRDS(fac_mm, here("Data", "Analysis", "facility_make_model.rds"))
log_step(sprintf("Saved: Data/Analysis/facility_make_model.rds (%s facilities)",
  format(nrow(fac_mm), big.mark=",")), 1)

rm(tanks_at_reform)
gc()

#==============================================================================
# SECTION 3: CREATE FACILITY-MONTH BACKBONE
#==============================================================================
cat("\n========================================\n")
cat("SECTION 3: BUILDING BACKBONE\n")
cat("========================================\n\n")

# Calculate active ranges for every facility
facility_ranges <- active_panel_tanks[, .(
  start_date = min(tank_panel_start_date, na.rm=TRUE),
  end_date   = max(tank_panel_end_date, na.rm=TRUE),
  county_name = first(na.omit(county_name)),
  county_fips = first(na.omit(county_fips))
), by = .(facility_id, state, panel_id)]

# Fast Backbone Creation (Vectorized)
facility_ranges[, start_m_abs := (year(start_date) - 1970L) * 12L + month(start_date)]
facility_ranges[, end_m_abs   := (year(end_date)   - 1970L) * 12L + month(end_date)]

log_step("Expanding facilities (Integer Method)...", 0)

panel_dates <- facility_ranges[, .(
    month_abs = start_m_abs:end_m_abs 
), by = .(panel_id, facility_id, state, county_name, county_fips)]

# Fast Date Recovery
min_m <- min(panel_dates$month_abs)
max_m <- max(panel_dates$month_abs)

date_lookup <- data.table(month_abs = min_m:max_m)
date_lookup[, `:=`(
    year  = 1970L + (month_abs - 1L) %/% 12L,
    month = (month_abs - 1L) %% 12L + 1L
)]
date_lookup[, date := as.IDate(sprintf("%04d-%02d-01", year, month))]

log_step("Mapping dates...", 0)
panel_dates <- merge(panel_dates, date_lookup, by = "month_abs", sort = FALSE)

panel_dates[, `:=`(panel_year = year, panel_month = month)]
panel_dates[, c("month_abs", "year", "month") := NULL]

log_step(sprintf("  Backbone created: %s facility-months", format(nrow(panel_dates), big.mark=",")), 1)

rm(facility_ranges, date_lookup)


#==============================================================================
# SECTION 4: COMPUTE MONTHLY TANK COMPOSITION
#==============================================================================
cat("\n========================================\n")
cat("SECTION 4: MONTHLY TANK COMPOSITION (NON-EQUI JOIN)\n")
cat("========================================\n\n")

# Prepare tanks for join
tanks_for_join <- active_panel_tanks[, .(
    panel_id, facility_id, state,
    tank_start_num = as.numeric(tank_panel_start_date),
    tank_end_num   = as.numeric(tank_panel_end_date),
    install_date_for_age = tank_installed_date,  # actual install date, NOT bounded to 1970
    capacity, single_walled, double_walled, 
    is_gasoline, is_diesel, is_oil_kerosene, is_jet_fuel, is_other
)]

# Process by State
states <- unique(panel_dates$state)
tank_month_list <- vector("list", length(states))

log_step("Performing non-equi joins by state...", 0)

for (i in seq_along(states)) {
    st <- states[i]

    backbone_st <- panel_dates[state == st]
    tanks_st    <- tanks_for_join[state == st]

    if(nrow(backbone_st) == 0 || nrow(tanks_st) == 0) next
    
    backbone_st[, date_num := as.numeric(date)]

    active <- tanks_st[backbone_st, 
                       on = .(panel_id, 
                              tank_start_num <= date_num, 
                              tank_end_num >= date_num),
                       nomatch = 0L, allow.cartesian = TRUE]
    
    active[, tank_age := as.numeric(date - install_date_for_age) / 365.25]

    tank_month_list[[i]] <- active[, .(
      active_tanks   = .N,
      avg_tank_age   = mean(tank_age, na.rm=TRUE),
      total_capacity = sum(capacity, na.rm=TRUE),
      single_tanks   = sum(single_walled, na.rm=TRUE),
      double_tanks   = sum(double_walled, na.rm=TRUE),
      has_gasoline   = max(is_gasoline, na.rm=TRUE),
      has_diesel     = max(is_diesel, na.rm=TRUE),
      has_oil_kerosene = max(is_oil_kerosene, na.rm=TRUE),
      has_jet_fuel   = max(is_jet_fuel, na.rm=TRUE),
      has_other      = max(is_other, na.rm=TRUE)
    ), by = .(panel_id, state, date)]

    if (i %% 5 == 0) cat(".")
}
cat("\n")

# Combine
tank_composition <- rbindlist(tank_month_list)
rm(tanks_for_join, tank_month_list, active)
gc()

# Merge back to backbone
log_step("Merging composition back to backbone...", 0)
monthly <- merge(panel_dates, tank_composition, 
                 by = c("panel_id", "state", "date"), 
                 all.x = TRUE)

# Fill NAs
monthly[is.na(active_tanks), `:=`(active_tanks=0, total_capacity=0, single_tanks=0, double_tanks=0)]

# Derived Vars
monthly[, `:=`(
    has_single_walled = as.integer(single_tanks > 0),
    has_double_walled = as.integer(double_tanks > 0),
    has_unknown_walled  = as.integer(active_tanks > (single_tanks + double_tanks))
)]

log_step("Section 4 Complete.", 1)

#==============================================================================
# SECTION 5: MERGE MONTHLY EVENTS
#==============================================================================
cat("\n========================================\n")
cat("SECTION 5: MERGING EVENTS (LUST, CLOSURES, INSTALLS)\n")
cat("========================================\n\n")

# 5.1 LUST Events
leaks_agg <- master_lust[!is.na(report_date), .(leak_incident = 1L), 
                         by = .(panel_id, date = floor_date(report_date, "month"))]
leaks_agg <- leaks_agg[, .(leak_incident = sum(leak_incident)), by = .(panel_id, date)]

# 5.2 Closures (TANK-LEVEL CLASSIFICATION BEFORE AGGREGATION)
# For each closed tank, determine if it was replaced or permanently removed.
# Logic: if the facility installed ANY tank after this tank's closure date,
# the facility continued investing --> this closure is a replacement.
# If no subsequent install exists, the facility was divesting --> permanent removal.
log_step("Classifying tank closures (replacement vs permanent)...", 0)

closed_tanks <- active_panel_tanks[!is.na(tank_closed_date), .(
  panel_id, tank_id, tank_closed_date, tank_installed_date,
  capacity, single_walled, double_walled
)]

# Use fac_max_install_date from facility biography (Section 2.6)
closed_tanks <- merge(closed_tanks, fac_bio[, .(panel_id, fac_max_install_date)],
                      by = "panel_id", all.x = TRUE)

# Classification: did the facility install anything AFTER this tank closed?
closed_tanks[, is_replacement := as.integer(
  !is.na(fac_max_install_date) & fac_max_install_date > tank_closed_date
)]
closed_tanks[is.na(is_replacement), is_replacement := 0L]

# Sub-classification: single-walled tank closed as part of replacement
closed_tanks[, is_sw_replacement := as.integer(
  is_replacement == 1L & single_walled == 1
)]

log_step(sprintf("  Total closures: %s | Replacement: %s (%.1f%%) | Permanent: %s (%.1f%%)",
  format(nrow(closed_tanks), big.mark=","),
  format(sum(closed_tanks$is_replacement), big.mark=","),
  100 * mean(closed_tanks$is_replacement),
  format(sum(closed_tanks$is_replacement == 0), big.mark=","),
  100 * mean(closed_tanks$is_replacement == 0)), 1)

# Aggregate to facility-month WITH closure type breakdown
closures_agg <- closed_tanks[, .(
  tanks_closed              = .N,
  tanks_closed_permanent    = sum(is_replacement == 0L),
  tanks_closed_replacement  = sum(is_replacement == 1L),
  capacity_closed           = sum(capacity, na.rm=TRUE),
  single_walled_closed      = sum(single_walled, na.rm=TRUE),
  sw_replacement_closures   = sum(is_sw_replacement, na.rm=TRUE)
), by = .(panel_id, date = floor_date(tank_closed_date, "month"))]

rm(closed_tanks)

# 5.3 Installs
installs_agg <- active_panel_tanks[!is.na(tank_installed_date), .(
  tanks_installed = .N,
  capacity_installed = sum(capacity, na.rm=TRUE),
  double_walled_installed = sum(double_walled, na.rm=TRUE)
), by = .(panel_id, date = floor_date(tank_installed_date, "month"))]

# 5.4 Merge All
monthly <- merge(monthly, leaks_agg, by=c("panel_id", "date"), all.x=TRUE)
monthly <- merge(monthly, closures_agg, by=c("panel_id", "date"), all.x=TRUE)
monthly <- merge(monthly, installs_agg, by=c("panel_id", "date"), all.x=TRUE)

# Fill NAs
fill_cols <- c("leak_incident", "tanks_closed", "tanks_closed_permanent",
               "tanks_closed_replacement", "capacity_closed", "single_walled_closed",
               "sw_replacement_closures",
               "tanks_installed", "capacity_installed", "double_walled_installed")
for(col in fill_cols) set(monthly, which(is.na(monthly[[col]])), col, 0)

# 5.5 Create Replacement/Retrofit Variables (from tank-level classification)
monthly[, `:=`(
  # A replacement event occurred this month (from tank-level forward-look, not same-month coincidence)
  replacement_event = as.integer(tanks_closed_replacement > 0),
  # Single-to-double upgrade: single-walled tank replaced AND double-walled tank installed
  single_to_double_replacement = as.integer(
    sw_replacement_closures > 0 & double_walled_installed > 0
  )
)]

# 5.6 Save Intermediate Monthly Panel (WITHOUT FR DATA to save memory)
log_step("Saving intermediate monthly panel...", 0)
output_monthly <- here("Data", "Processed", "facility_leak_behavior_monthly_intermediate.csv")
fwrite(monthly, output_monthly)
log_step(sprintf("✓ Saved Monthly RDS: %s", output_monthly), 1)



# =========================================================================
# SECTION 5.7: MEMORY CLEANUP
# =========================================================================
rm(leaks_agg, closures_agg, installs_agg, panel_dates)
gc()

# Shrink Master Files for Section 7
log_step("Shrinking Master Tables to bare minimum...", 0)
master_tanks_full <- copy(master_tanks_unfiltered)  # KEEP FULL VERSION FOR SECTION 7.5
# Keep tank_installed_date: Section 7 needs it to scope leak-closure pairs
# to the same generation of infrastructure
master_tanks <- master_tanks[!is.na(tank_closed_date), .(panel_id, tank_closed_date, tank_installed_date)]
master_lust <- master_lust[!is.na(report_date), .(panel_id, report_date)]

log_memory("Pre-Aggregation Cleanup")
gc()

#==============================================================================
# SECTION 6: ANNUAL AGGREGATION & LATE FR MERGE
#==============================================================================
cat("\n========================================\n")
cat("SECTION 6: ANNUAL AGGREGATION (LATE MERGE STRATEGY)\n")
cat("========================================\n\n")

log_step("Aggregating monthly panel to annual...", 0)

# Add YEAR column if not present
if(!"YEAR" %in% names(monthly)) monthly[, YEAR := year(date)]

# A. December Snapshot (Stocks)
setorder(monthly, panel_id, YEAR, date)
december <- monthly[, .SD[.N], by = .(panel_id, YEAR), 
                    .SDcols = c("active_tanks", "total_capacity", "avg_tank_age", 
                                "single_tanks", "double_tanks", "state", 
                                "county_name", "county_fips",
                                "has_single_walled", "has_double_walled")]

setnames(december, 
         old = c("active_tanks", "total_capacity", "avg_tank_age", "single_tanks", "double_tanks",
                 "has_single_walled", "has_double_walled"), 
         new = paste0(c("active_tanks", "total_capacity", "avg_tank_age", "single_tanks", "double_tanks",
                        "has_single_walled", "has_double_walled"), "_dec"))

# B. Annual Flows (CORRECTED)
annual_flows <- monthly[, .(
    facility_id = first(facility_id),

    # Flow Variables
    n_leaks       = sum(leak_incident, na.rm=TRUE),
    leak_year     = as.integer(sum(leak_incident, na.rm=TRUE) > 0),
    n_closures    = sum(tanks_closed, na.rm=TRUE),
    closure_year  = as.integer(sum(tanks_closed, na.rm=TRUE) > 0),
    # Closure type breakdown (from tank-level forward-look classification)
    n_closures_permanent    = sum(tanks_closed_permanent, na.rm=TRUE),
    n_closures_replacement  = sum(tanks_closed_replacement, na.rm=TRUE),
    permanent_closure_year  = as.integer(sum(tanks_closed_permanent, na.rm=TRUE) > 0),
    replacement_closure_year = as.integer(sum(tanks_closed_replacement, na.rm=TRUE) > 0),

    n_installs    = sum(tanks_installed, na.rm=TRUE),
    n_retrofits   = sum(replacement_event, na.rm=TRUE),
    retrofit_year = as.integer(sum(replacement_event, na.rm=TRUE) > 0),
    n_single_to_double = sum(single_to_double_replacement, na.rm=TRUE),
    single_to_double_year = as.integer(sum(single_to_double_replacement, na.rm=TRUE) > 0),

    # Capacity Flows
    capacity_installed_year = sum(capacity_installed, na.rm=TRUE),
    capacity_closed_year = sum(capacity_closed, na.rm=TRUE),
    double_walled_installed_year = sum(double_walled_installed, na.rm=TRUE),

    # Mean Stocks
    active_tanks_mean = mean(active_tanks, na.rm=TRUE),
    total_capacity_mean = mean(total_capacity, na.rm=TRUE),
    avg_tank_age_mean = mean(avg_tank_age, na.rm=TRUE),

    # Fuel Types (FIXED: Use 'any' to avoid -Inf and ensure integer type)
    has_gasoline_year = as.integer(any(has_gasoline == 1, na.rm=TRUE)),
    has_diesel_year   = as.integer(any(has_diesel == 1, na.rm=TRUE))
), by = .(panel_id, YEAR)]



# C. Merge Stocks and Flows
annual <- merge(annual_flows, december, by = c("panel_id", "YEAR"))

# D. Cleanup Monthly to free RAM
rm(monthly, december, annual_flows)
gc()

log_step(sprintf("Annual Panel Created: %s rows", format(nrow(annual), big.mark=",")), 1)
# =========================================================================
# 6.2 LATE MERGE: Texas FR Data (DECEMBER SNAPSHOT + ANNUAL FLOWS)
# =========================================================================
log_step("Preparing Texas FR data for annual merge...", 0)

# -------------------------------------------------------------------------
# A. December Snapshot (FR Status as of December 31st)
# -------------------------------------------------------------------------
# This captures: What was the facility's FR situation at year-end?

# Add YEAR/MONTH to fa_monthly for filtering
fa_monthly[, `:=`(YEAR = YEAR, MONTH = MONTH)]

# Get December observations (or last available month if December missing)
setorder(fa_monthly, panel_id, YEAR, MONTH)

fa_december <- fa_monthly[, .SD[.N], by = .(panel_id, YEAR), 
                          .SDcols = c("DETAIL_TYPE", "CATEGORY", 
                                      "uses_private", "uses_self", "fr_covered")]

# Rename to indicate these are end-of-year stocks
setnames(fa_december,
         old = c("DETAIL_TYPE", "CATEGORY", "uses_private", "uses_self", "fr_covered"),
         new = c("DETAIL_TYPE_dec", "CATEGORY_dec", "uses_private_dec", 
                 "uses_self_dec", "fr_covered_dec"))

# Add issuer name if available
issuer_col <- grep("^ISSUER_NAME", names(fa_monthly), value = TRUE)[1]
if(!is.null(issuer_col) && length(issuer_col) > 0) {
  fa_monthly[, ISSUER_NAME := get(issuer_col)]
  
  fa_december_issuer <- fa_monthly[, .SD[.N], by = .(panel_id, YEAR), 
                                   .SDcols = "ISSUER_NAME"]
  setnames(fa_december_issuer, "ISSUER_NAME", "ISSUER_NAME_dec")
  
  fa_december <- merge(fa_december, fa_december_issuer, by = c("panel_id", "YEAR"))
}

log_step("  December FR snapshot created", 1)

# -------------------------------------------------------------------------
# B. Annual Flows (FR Events During the Year)
# -------------------------------------------------------------------------
# This captures: What FR activity/changes occurred during the year?
fa_annual_flows <- fa_monthly[, .(
  # Use fr_covered flag directly — set by Script 02 to FALSE for gap months
  months_covered   = sum(fr_covered == TRUE,  na.rm = TRUE),
  months_with_data = .N,
  
  share_private = mean(uses_private, na.rm = TRUE),
  share_self    = mean(uses_self,    na.rm = TRUE),
  
  n_transitions = sum(transition_month, na.rm = TRUE),
  any_transition = as.integer(max(transition_month, na.rm = TRUE) > 0),
  # Use fr_covered == FALSE instead of CATEGORY == "None"
  n_gap_months  = sum(fr_covered == FALSE, na.rm = TRUE),
  
  modal_detail_type = {
    # Exclude gap-month sentinel values
    valid_types <- DETAIL_TYPE[!is.na(DETAIL_TYPE) & DETAIL_TYPE != "" & DETAIL_TYPE != "NO COVERAGE"]
    if (length(valid_types) > 0) names(which.max(table(valid_types))) else NA_character_
  },
  
  modal_category = {
    valid_cats <- CATEGORY[!is.na(CATEGORY) & CATEGORY != "NO COVERAGE"]
    if (length(valid_cats) > 0) names(which.max(table(valid_cats))) else NA_character_
  }
), by = .(panel_id, YEAR)]

# Create derived flow variables
fa_annual_flows[, `:=`(
  any_gap = as.integer(n_gap_months > 0),
  fr_coverage_share = months_covered / 12,
  fr_stable = as.integer(n_transitions == 0 & n_gap_months == 0)
)]

log_step("  Annual FR flows created", 1)

# -------------------------------------------------------------------------
# C. Merge FR Stocks and Flows to Annual Panel
# -------------------------------------------------------------------------
annual[, panel_year := YEAR]   # YEAR already exists from annual_flows / december

# Merge December snapshot
annual <- merge(annual, fa_december, 
                by.x = c("panel_id", "panel_year"), 
                by.y = c("panel_id", "YEAR"), 
                all.x = TRUE)

# Merge annual flows
annual <- merge(annual, fa_annual_flows, 
                by.x = c("panel_id", "panel_year"), 
                by.y = c("panel_id", "YEAR"), 
                all.x = TRUE)

log_step("  FR data merged to annual panel", 1)

# -------------------------------------------------------------------------
# D. Fill NAs for Non-Texas Facilities
# -------------------------------------------------------------------------

# December snapshot variables (stocks)
dec_vars <- c("DETAIL_TYPE_dec", "CATEGORY_dec", "uses_private_dec", 
              "uses_self_dec", "fr_covered_dec", "ISSUER_NAME_dec")

for(var in dec_vars) {
  if(var %in% names(annual)) {
    # For character vars, fill with "None" or NA
    if(is.character(annual[[var]])) {
      annual[is.na(get(var)), (var) := "None"]
    } else {
      # For numeric/binary, fill with 0
      annual[is.na(get(var)), (var) := 0]
    }
  }
}

# Annual flow variables
flow_vars <- c("months_covered", "months_with_data", "share_private", "share_self",
               "n_transitions", "any_transition", "n_gap_months", "any_gap",
               "fr_coverage_share", "fr_stable")

for(var in flow_vars) {
  if(var %in% names(annual)) {
    annual[is.na(get(var)), (var) := 0]
  }
}

# Modal variables
modal_vars <- c("modal_detail_type", "modal_category")
for(var in modal_vars) {
  if(var %in% names(annual)) {
    annual[is.na(get(var)), (var) := "None"]
  }
}

log_step("  NAs filled for non-Texas facilities", 1)

# -------------------------------------------------------------------------
# E. Merge Zurich Lookup
# -------------------------------------------------------------------------

annual <- merge(annual, unique(zurich_2012_lookup[, .(panel_id, had_zurich_2012)]), 
                by = "panel_id", all.x = TRUE)

# -------------------------------------------------------------------------
# F. Create Treatment Variable (Dropped by Zurich)
# -------------------------------------------------------------------------
# Definition: Had Zurich in 2012, Year >= 2012, and no coverage in December

annual[state == "TX", dropped_by_zurich := as.integer(
    had_zurich_2012 == 1 & 
    panel_year >= 2012 & 
    (is.na(CATEGORY_dec) | CATEGORY_dec == "None")
)]

annual[is.na(dropped_by_zurich), dropped_by_zurich := 0]
annual[is.na(had_zurich_2012), had_zurich_2012 := 0]

log_step("  Treatment variable created", 1)

# -------------------------------------------------------------------------
# G. Calculate Year-over-Year FR Changes
# -------------------------------------------------------------------------
setorder(annual, panel_id, panel_year)

# §6.2G — replace the fr_issuer_change line inside the := block with:

# Calculate YoY changes (guard issuer col separately)
annual[, `:=`(
  fr_coverage_change = as.integer(
    fr_covered_dec != shift(fr_covered_dec, 1, type = "lag")
  ),
  fr_type_change = as.integer(
    !is.na(DETAIL_TYPE_dec) & DETAIL_TYPE_dec != "None" &
    !is.na(shift(DETAIL_TYPE_dec, 1, type = "lag")) &
    shift(DETAIL_TYPE_dec, 1, type = "lag") != "None" &
    DETAIL_TYPE_dec != shift(DETAIL_TYPE_dec, 1, type = "lag")
  )
), by = panel_id]

# Issuer change — only compute if column exists
if ("ISSUER_NAME_dec" %in% names(annual)) {
  annual[, fr_issuer_change := as.integer(
    !is.na(ISSUER_NAME_dec) & ISSUER_NAME_dec != "None" &
    !is.na(shift(ISSUER_NAME_dec, 1, type = "lag")) &
    shift(ISSUER_NAME_dec, 1, type = "lag") != "None" &
    ISSUER_NAME_dec != shift(ISSUER_NAME_dec, 1, type = "lag")
  ), by = panel_id]
} else {
  annual[, fr_issuer_change := 0L]
}

annual[is.na(fr_coverage_change), fr_coverage_change := 0]
annual[is.na(fr_type_change),     fr_type_change     := 0]
annual[is.na(fr_issuer_change),   fr_issuer_change   := 0]

log_step("  YoY FR changes calculated", 1)

# -------------------------------------------------------------------------
# H. Final Cleanup of Texas Data (KEEP fa_monthly for Section 8)
# -------------------------------------------------------------------------
rm(fa_december, fa_annual_flows, zurich_2012_lookup)

# Keep fa_december_issuer if it exists
if(exists("fa_december_issuer")) rm(fa_december_issuer)

gc()

log_step("Annual FR merge complete.", 1)


# 6.3 Calculate YoY Changes
setorder(annual, panel_id, panel_year)
annual[, `:=`(
  capacity_change_year = total_capacity_dec - shift(total_capacity_dec, 1, type = "lag"),
  net_tank_change      = active_tanks_dec - shift(active_tanks_dec, 1, type = "lag")
), by = panel_id]

log_step("Annual aggregation complete.", 1)

#==============================================================================
# SECTION 7: ROBUST LEAK CLASSIFICATION
#==============================================================================
cat("\n========================================\n")
cat("SECTION 7: LEAK CLASSIFICATION\n")
cat("========================================\n\n")

# Carry tank_installed_date so we can scope pairs to same generation.
# A leak that occurred before a tank was installed cannot be attributed to
# that tank's closure -- it belongs to a prior generation of infrastructure.
closures_exact <- master_tanks[!is.na(tank_closed_date),
  .(panel_id, closure_date = tank_closed_date, tank_install_date = tank_installed_date)]
leaks_exact    <- master_lust[!is.na(report_date), .(panel_id, report_date)]

if(nrow(closures_exact) > 0 && nrow(leaks_exact) > 0) {
  
  setkey(closures_exact, panel_id)
  setkey(leaks_exact, panel_id)
  
  pairs <- leaks_exact[closures_exact, on = .(panel_id), allow.cartesian = TRUE]
  
  # GENERATION SCOPING: exclude leak-closure pairs from different generations.
  # A leak reported before this tank was installed cannot be caused by this tank.
  # This eliminates cross-generational contamination (e.g., a 1985 leak from
  # 1975-vintage infrastructure being paired with a 2005 closure of a 1997 tank).
  n_before <- nrow(pairs)
  pairs <- pairs[is.na(tank_install_date) | report_date >= tank_install_date]
  n_after <- nrow(pairs)
  log_step(sprintf("  Leak-closure pairs: %s total -> %s after generation scoping (dropped %s cross-generation)",
    format(n_before, big.mark=","), format(n_after, big.mark=","),
    format(n_before - n_after, big.mark=",")), 1)
  
  pairs[, diff := as.numeric(report_date - closure_date)]
  
  # Apply Classification Definitions (4 Specs)
  pairs[, `:=`(
    # Primary: 0 to 60 days after
    is_rev_prim = diff >= 0 & diff <= 60,
    is_kno_prim = diff < -180,
    is_ind_prim = diff >= -180 & diff < 0,
    
    # Narrow: 0 to 30 days
    is_rev_nar = diff >= 0 & diff <= 30,
    is_kno_nar = diff < -365,
    
    # Wide: 0 to 90 days
    is_rev_wid = diff >= 0 & diff <= 90,
    is_kno_wid = diff < -90,
    
    # Regulatory: 0 to 45 days
    is_rev_reg  = diff >= 0 & diff <= 45,
    is_kno_reg  = diff < -180
  )]
  
  pairs[, closure_year := year(closure_date)]
  
  closure_flags <- pairs[, .(
    has_rev_prim = any(is_rev_prim),
    has_kno_prim = any(is_kno_prim),
    has_ind_prim = any(is_ind_prim),
    has_rev_nar = any(is_rev_nar),
    has_kno_nar = any(is_kno_nar),
    has_rev_wid = any(is_rev_wid),
    has_kno_wid = any(is_kno_wid),
    has_rev_reg = any(is_rev_reg),
    has_kno_reg = any(is_kno_reg)
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
    tank_closure_revealed_reg = as.integer(any(has_rev_reg)),
    tank_closure_known_leak_reg = as.integer(any(has_kno_reg))
  ), by = .(panel_id, panel_year)]
  
  annual <- merge(annual, annual_flags, by = c("panel_id", "panel_year"), all.x = TRUE)
  
  cols_to_fill <- grep("tank_closure_", names(annual), value=TRUE)
  for(col in cols_to_fill) annual[is.na(get(col)), (col) := 0]
  
  annual[, tank_closure_clean := as.integer(
    closure_year == 1 & 
    tank_closure_revealed == 0 & 
    tank_closure_known_leak == 0
  )]
  
} else {
  annual[, `:=`(
    tank_closure_revealed = 0,
    tank_closure_known_leak = 0,
    tank_closure_clean = 0
  )]
}

log_step("Leak classification complete.", 1)

#==============================================================================
# SECTION 7.5: FACILITY BASELINE (PRE-PANEL CLOSURES)
#==============================================================================
cat("\n========================================\n")
cat("SECTION 7.5: FACILITY BASELINE\n")
cat("========================================\n\n")

log_step("Creating facility baseline for pre-panel closures...", 0)
baseline_start <- Sys.time()

if(!exists("master_tanks_full")) {
  log_step("⚠ master_tanks_full not found, skipping baseline", 1)
} else {
  pre_panel_closures <- master_tanks_full[
    !is.na(tank_closed_date) & 
    tank_closed_date < PANEL_START,
    .(
      n_tanks_closed_before_panel = .N,
      earliest_closure_date = min(tank_closed_date, na.rm = TRUE)
    ),
    by = panel_id
  ]
  
  annual <- merge(annual, pre_panel_closures, by = "panel_id", all.x = TRUE)
  annual[is.na(n_tanks_closed_before_panel), n_tanks_closed_before_panel := 0]
  
  log_step(sprintf("Facilities with pre-panel closures: %s", 
                   format(sum(annual[panel_year == min(panel_year)]$n_tanks_closed_before_panel > 0), big.mark=",")), 1)
  
  rm(master_tanks_full)
  gc()
}

log_step(sprintf("✓ Baseline created in %.1f seconds", 

                 difftime(Sys.time(), baseline_start, units="secs")), 1)



#==============================================================================
# SECTION 8: WIDE ISSUER DATA (TEXAS ONLY)
#==============================================================================
cat("\n========================================\n")
cat("SECTION 8: WIDE ISSUER DATA\n")
cat("========================================\n\n")

log_step("Creating wide issuer columns...", 0)

# Find issuer column(s) in fa_monthly
issuer_col <- grep("^ISSUER_NAME", names(fa_monthly), value = TRUE)[1]

if(!is.null(issuer_col) && length(issuer_col) > 0) {
  
  # Create clean issuer names (8 char limit)
  fa_monthly[, cleaned_issuer := str_sub(
    str_replace_all(toupper(get(issuer_col)), "[^A-Z0-9]", ""), 1, 8
  )]
  
  # Count months per issuer per facility-year
  issuer_counts <- fa_monthly[
    !is.na(cleaned_issuer) & cleaned_issuer != "",
    .(n_months = .N),
    by = .(panel_id, panel_year = YEAR, cleaned_issuer)
  ]
  
  # Cast to wide format
  issuer_wide <- dcast(
    issuer_counts,
    panel_id + panel_year ~ cleaned_issuer,
    value.var = "n_months",
    fill = 0
  )
  
  # Rename columns with TX_issuers_ prefix
  issuer_cols <- setdiff(names(issuer_wide), c("panel_id", "panel_year"))
  setnames(issuer_wide, issuer_cols, paste0("TX_issuers_", issuer_cols))
  
  # Merge into annual
  annual <- merge(annual, issuer_wide, by = c("panel_id", "panel_year"), all.x = TRUE)
  
  # Fill NAs with 0
  wide_cols <- grep("TX_issuers_", names(annual), value = TRUE)
  for(col in wide_cols) {
    annual[is.na(get(col)), (col) := 0]
  }
  
  log_step(sprintf("✓ Created %d issuer columns", length(wide_cols)), 1)
  
  rm(issuer_counts, issuer_wide)
  gc()
  
} else {
  log_step("⚠ ISSUER_NAME not found in fa_monthly, skipping wide issuer data", 1)
}

# Now we can safely remove fa_monthly
rm(fa_monthly)
gc()

#==============================================================================
# SECTION 9: POLICY, TREATMENT & COVARIATES
#==============================================================================
cat("\n========================================\n")
cat("SECTION 9: POLICY & COVARIATES\n")
cat("========================================\n\n")

log_step("Creating policy & cohort variables...", 0)

# -------------------------------------------------------------------------
# 9.0: MERGE FACILITY BIOGRAPHY & MAKE-MODEL CELL (time-invariant)
# -------------------------------------------------------------------------
# fac_bio from Section 2.6: actual facility birth dates, tank counts, vintage
# fac_mm from Section 2.5: portfolio wall/fuel composition at reform date
log_step("Merging facility biography and make-model cell...", 1)

annual <- merge(annual, fac_bio[, .(panel_id,
  facility_first_install, facility_first_install_yr,
  facility_last_install, facility_last_install_yr,
  total_tanks_ever, total_tanks_closed_ever, total_tanks_open_ever,
  had_pre1989_tanks, all_tanks_post1988,
  fac_reg_vintage, fac_is_incumbent)],
  by = "panel_id", all.x = TRUE)

annual <- merge(annual, fac_mm[, .(panel_id, make_model_fac,
  fac_wall, fac_fuel, fac_oldest_age_bin, n_tanks_at_reform)],
  by = "panel_id", all.x = TRUE)

# Define treated states and their treatment years
treated_states <- c("TX", "WI", "NJ", "MI", "IA", "FL", "AZ", "CT")
treatment_years <- c(1999, 1998, 2010, 2014, 2000, 1999, 2006, 2010)

# Create basic policy flags
annual[, `:=`(
  post_1999 = as.integer(panel_year >= 1999),
  texas_treated = as.integer(state == "TX"),
  treated_state_flag = as.integer(state %in% treated_states)
)]

# First/last observed years (panel appearance — useful for duration, NOT for vintage)
annual[, `:=`(
  first_observed = min(panel_year),
  last_observed = max(panel_year)
), by = panel_id]

# Treatment variables
annual[, treatment_year := treatment_years[match(state, treated_states)]]
annual[is.na(treatment_year), treatment_year := 0]

annual[, treatment_group := fcase(
  state == "TX", "Texas",
  state %in% treated_states, "Other Treated",
  default = "Control"
)]

# Relative year to treatment
annual[, rel_year := fifelse(
  treated_state_flag == 1,
  panel_year - treatment_year,
  NA_integer_
)]

# -------------------------------------------------------------------------
# 9.1: TIME-INVARIANT FACILITY ATTRIBUTES (from biography, not avg_tank_age)
# -------------------------------------------------------------------------
# install_year: actual facility birth year (time-invariant).
# The old version (panel_year - floor(avg_tank_age_dec)) was a time-varying
# fiction that shifted whenever tanks entered or exited the facility, making
# retrofitted facilities look "young" and contaminating sample restrictions.
annual[, install_year := facility_first_install_yr]

# Cohort: based on actual facility birth, not panel window appearance.
# The old version used first_observed (panel start) which conflated data
# availability with economic entry.
annual[, `:=`(
  cohort = fifelse(fac_is_incumbent == 1L, "Incumbent", "Entrant"),
  is_incumbent = fac_is_incumbent
)]

# Regulatory vintage: time-invariant, based on actual facility birth.
# The old version re-derived this each year from avg_tank_age, producing
# facilities that "jumped" between vintage categories over time.
annual[, `:=`(
  pre1998_install = as.integer(facility_first_install_yr < 1998),
  reg_vintage = fac_reg_vintage
)]

# -------------------------------------------------------------------------
# 9.2: TIME-VARYING COVARIATES (legitimately change over time)
# -------------------------------------------------------------------------
# Tank age bins: current age of active stock. This IS time-varying and that's
# correct — it describes the facility's risk profile in each year.
annual[, age_bins := cut(avg_tank_age_dec,
  breaks = c(0, 3, 6, 9, 12, 15, 18, 21, 24, Inf),
  include.lowest = TRUE,
  labels = c("0-2", "3-5", "6-8", "9-11", "12-14", "15-17", "18-20", "21-23", "24+")
)]


# Wall type: current composition (time-varying). Useful as DESCRIPTIVE OUTCOME
# (portfolio composition change) but NEVER as a sample restriction or control
# in DiD — using post-treatment wall_type as a filter creates differential
# attrition driven by the treatment itself. Use fac_wall (frozen at reform
# date, from Section 2.5) for sample restrictions instead.
annual[, wall_type := fcase(
  has_double_walled_dec > 0 & has_single_walled_dec == 0, "Double-Walled",
  has_single_walled_dec > 0 & has_double_walled_dec == 0, "Single-Walled",
  has_double_walled_dec > 0 & has_single_walled_dec > 0, "Mixed",
  default = "Unknown"
)]

log_step("✓ Policy variables created", 1)

#==============================================================================
# SECTION 10: SURVIVAL & DECOMPOSITION
#==============================================================================
cat("\n========================================\n")
cat("SECTION 10: SURVIVAL VARIABLES\n")
cat("========================================\n\n")

log_step("Creating survival & decomposition variables...", 0)

setorder(annual, panel_id, panel_year)

# Exit Flags
annual[, last_year_id := max(panel_year), by = panel_id]
annual[, exit_flag := as.integer(panel_year == last_year_id & last_year_id < 2025)]
annual[, pulse_exit := exit_flag]

# Cumulative & Duration Variables
annual[, `:=`(
  ever_leaked = as.integer(cumsum(leak_year) > 0),
  ever_closed = as.integer(cumsum(closure_year) > 0),
  ever_exited = as.integer(cumsum(exit_flag) > 0),
  cumulative_leaks = cumsum(n_leaks),
  cumulative_closures = cumsum(n_closures),
  cumulative_retrofits = cumsum(n_retrofits),
  years_since_entry = panel_year - first_observed
), by = panel_id]

# Lagged Events
annual[, `:=`(
  has_previous_leak = as.integer(shift(cumsum(n_leaks), 1, fill = 0) > 0),
  has_previous_closure = as.integer(shift(cumsum(n_closures), 1, fill = 0) > 0)
), by = panel_id]

# First Event Timing
annual[, `:=`(
  event_first_leak = as.integer(leak_year == 1 & cumsum(leak_year) == 1),
  event_first_closure = as.integer(closure_year == 1 & cumsum(closure_year) == 1)
), by = panel_id]

annual[, year_of_first_leak := fifelse(
  any(leak_year > 0),
  panel_year[which(leak_year > 0)[1]],
  NA_integer_
), by = panel_id]

annual[, years_since_first_leak := panel_year - year_of_first_leak]

# Exit Decompositions
annual[, `:=`(
  exit_no_leak = as.integer(exit_flag == 1 & leak_year == 0),
  exit_with_leak = as.integer(exit_flag == 1 & leak_year == 1),
  retrofit_no_exit = as.integer(retrofit_year == 1 & exit_flag == 0),
  exit_no_retrofit = as.integer(exit_flag == 1 & retrofit_year == 0),
  both_exit_retrofit = as.integer(exit_flag == 1 & retrofit_year == 1)
)]

# Leak Classification Aliases
annual[, `:=`(
  lust_within_90d_of_closure = tank_closure_revealed_wide ,
  leak_found_by_closure = tank_closure_revealed,
  leak_not_found_by_exit = as.integer(exit_flag == 1 & leak_year == 0)
)]

# Stock Variable Aliases
annual[, `:=`(
  active_tanks = active_tanks_dec,
  total_capacity = total_capacity_dec,
  avg_tank_age = avg_tank_age_dec,
  single_tanks = single_tanks_dec,
  double_tanks = double_tanks_dec,
  has_single_walled = has_single_walled_dec,
  has_double_walled = has_double_walled_dec
)]

log_step("✓ Survival variables created", 1)

#==============================================================================
# SECTION 10.5: ADDITIONAL DERIVED VARIABLES
#==============================================================================
log_step("Creating additional derived variables...", 0)

# Capacity change flags
annual[, `:=`(
  capacity_increased = as.integer(capacity_change_year > 0),
  capacity_decreased = as.integer(capacity_change_year < 0),
  is_motor_fuel = as.integer(has_gasoline_year == 1 | has_diesel_year == 1)
)]

log_step("✓ Derived variables created", 1)

#==============================================================================
# SECTION 11: SAVE OUTPUTS
#==============================================================================
cat("\n========================================\n")
cat("SECTION 11: SAVING OUTPUTS\n")
cat("========================================\n\n")

# Save CSV
output_csv <- here("Data", "Processed", "facility_leak_behavior_annual.csv")
fwrite(annual, output_csv)
log_step(sprintf("✓ Saved CSV: %s", output_csv), 1)

# # Save RDS
# output_rds <- here("Data", "Processed", "facility_leak_behavior_annual.rds")
# saveRDS(annual, output_rds)
# log_step(sprintf("✓ Saved RDS: %s", output_rds), 1)

#==============================================================================
# SECTION 12: DATA DICTIONARY GENERATION
#==============================================================================
cat("\n========================================\n")
cat("SECTION 12: GENERATING DATA DICTIONARY\n")
cat("========================================\n\n")

log_step("Generating data dictionary...", 0)

data_dict <- data.table(
  variable_name = c(
    # Identifiers
    "panel_id", "facility_id", "state", "panel_year", "county_name", "county_fips",
    
    # Stock Variables (December)
    "active_tanks_dec", "total_capacity_dec", "avg_tank_age_dec",
    "single_tanks_dec", "double_tanks_dec", 
    "has_single_walled_dec", "has_double_walled_dec",
    
    # Stock Variables (Annual Mean)
    "active_tanks_mean", "total_capacity_mean", "avg_tank_age_mean",
    
    # Flow Variables
    "n_leaks", "leak_year", "n_closures", "closure_year", "n_installs",
    "n_retrofits", "retrofit_year", "n_single_to_double", "single_to_double_year",
    "capacity_installed_year", "capacity_closed_year", "double_walled_installed_year",
    
    # Year-over-Year Changes
    "capacity_change_year", "net_tank_change", "capacity_increased", "capacity_decreased",
    
    # Leak Classification (Primary)
    "tank_closure_revealed", "tank_closure_known_leak", "tank_closure_indeterminate", "tank_closure_clean",
    
    # Leak Classification (Robustness)
    "tank_closure_revealed_narrow", "tank_closure_known_leak_narrow",
    "tank_closure_revealed_wide", "tank_closure_known_leak_wide",
    "tank_closure_revealed_reg", "tank_closure_known_leak_reg",
    
    # Leak Classification (Aliases)
    "lust_within_90d_of_closure", "leak_found_by_closure", "leak_not_found_by_exit",
    
    # Duration Variables
    "first_observed", "last_observed", "last_year_id", "years_since_entry",
    
    # Exit Variables
    "exit_flag", "pulse_exit",
    
    # Cumulative Variables
    "ever_leaked", "ever_closed", "ever_exited",
    "cumulative_leaks", "cumulative_closures", "cumulative_retrofits",
    
    # Lagged Variables
    "has_previous_leak", "has_previous_closure",
    
    # Event Timing
    "event_first_leak", "event_first_closure", "year_of_first_leak", "years_since_first_leak",
    
    # Exit Decomposition
    "exit_no_leak", "exit_with_leak", "retrofit_no_exit", "exit_no_retrofit", "both_exit_retrofit",
    
    # FR Coverage
    "fr_coverage_share", "share_private", "share_self",
    "n_transitions", "any_transition", "modal_detail_type",
    
    # FR Treatment
    "had_zurich_2012", "dropped_by_zurich",
    
    # Baseline
    "n_tanks_closed_before_panel", "earliest_closure_date",
    
    # Policy Variables
    "post_1999", "texas_treated", "treated_state_flag", "treatment_year", 
    "treatment_group", "rel_year",
    
    # Cohort Variables
    "cohort", "is_incumbent",
    
    # Vintage Variables
    "age_bins", "install_year", "pre1998_install", "reg_vintage", "wall_type",
    
    # Fuel Type
    "has_gasoline_year", "has_diesel_year", "is_motor_fuel",
    
    # Stock Aliases
    "active_tanks", "total_capacity", "avg_tank_age", 
    "single_tanks", "double_tanks", "has_single_walled", "has_double_walled"
  ),
  
  category = c(
    rep("Identifier", 6),
    rep("Stock (EOY)", 7),
    rep("Stock (Annual Mean)", 3),
    rep("Flow (Annual)", 12),
    rep("YoY Change", 4),
    rep("Leak Class (Primary)", 4),
    rep("Leak Class (Narrow)", 2),
    rep("Leak Class (Wide)", 2),
    rep("Leak Class (Regulatory)", 2),
    rep("Leak Class (Alias)", 3),
    rep("Duration", 4),
    rep("Exit", 2),
    rep("Cumulative", 6),
    rep("Lagged", 2),
    rep("Event Timing", 4),
    rep("Exit Type", 5),
    rep("FR Coverage", 6),
    rep("FR Treatment", 2),
    rep("Baseline", 2),
    rep("Policy", 6),
    rep("Cohort", 2),
    rep("Vintage", 5),
    rep("Fuel Type", 3),
    rep("Stock (Alias)", 7)
  ),
  
  description = c(
    "Composite key (facility_id + state)", "Facility ID", "State", "Year", "County name", "County FIPS",
    
    "Active tanks (December)", "Total capacity (December)", "Average tank age (December)",
    "Single-walled tanks (December)", "Double-walled tanks (December)",
    "Has single-walled tanks (December)", "Has double-walled tanks (December)",
    
    "Mean active tanks (annual)", "Mean capacity (annual)", "Mean tank age (annual)",
    
    "Number of leaks", "Leak occurred (binary)", "Number of closures", "Closure occurred (binary)",
    "Number of installations", "Number of retrofits", "Retrofit occurred (binary)",
    "Number of single→double upgrades", "Upgrade occurred (binary)",
    "Capacity installed", "Capacity closed", "Double-walled tanks installed",
    
    "YoY capacity change", "YoY net tank change", "Capacity increased", "Capacity decreased",
    
    "Revealed leak (0-60d)", "Known leak (>180d before)", "Indeterminate (-180 to 0d)", "Clean closure",
    "Revealed leak (0-30d)", "Known leak (>365d before)",
    "Revealed leak (0-90d)", "Known leak (>90d before)",
    "Revealed leak (0-45d - Regulatory)", "Known leak (>180d before - Regulatory)",
    
    "Alias for revealed (wide)", "Alias for revealed (primary)", "Exit without finding leak",
    
    "First year observed", "Last year observed", "Last year ID", "Years since entry",
    
    "Exit flag", "Exit pulse (alias)",
    
    "Ever had leak", "Ever closed tank", "Ever exited",
    "Cumulative leaks", "Cumulative closures", "Cumulative retrofits",
    
    "Had previous leak", "Had previous closure",
    
    "First leak event", "First closure event", "Year of first leak", "Years since first leak",
    
    "Exit without leak", "Exit with leak", "Retrofit without exit", "Exit without retrofit", "Both exit and retrofit",
    
    "FR coverage share", "Share using private insurance", "Share self-insured",
    "Number of transitions", "Any transition", "Modal detail type",
    
    "Had Zurich 2012", "Dropped by Zurich (treatment)",
    
    "Tanks closed before 1970", "Earliest pre-panel closure",
    
    "Post-1999 period", "Texas treatment", "Treated state flag", "Treatment year",
    "Treatment group", "Relative year to treatment",
    
    "Cohort (Incumbent/Entrant)", "Incumbent flag",
    
    "Age bins", "Install year", "Pre-1998 install", "Regulatory vintage", "Wall type",
    
    "Has gasoline", "Has diesel", "Motor fuel facility",
    
    "Alias for active_tanks_dec", "Alias for total_capacity_dec", "Alias for avg_tank_age_dec",
    "Alias for single_tanks_dec", "Alias for double_tanks_dec", 
    "Alias for has_single_walled_dec", "Alias for has_double_walled_dec"
  )
)

# Save data dictionary
# Append new variables from v2 rewrite (biography, closure types, make-model)
new_vars <- data.table(
  variable_name = c(
    # Facility Biography (time-invariant, from Section 2.6)
    "facility_first_install", "facility_first_install_yr",
    "facility_last_install", "facility_last_install_yr",
    "total_tanks_ever", "total_tanks_closed_ever", "total_tanks_open_ever",
    "had_pre1989_tanks", "all_tanks_post1988", "fac_is_incumbent",
    # Closure Type Breakdown (from tank-level classification, Section 5.2)
    "n_closures_permanent", "n_closures_replacement",
    "permanent_closure_year", "replacement_closure_year",
    # Facility Make-Model Cell (from Section 2.5)
    "make_model_fac", "fac_wall", "fac_fuel", "fac_oldest_age_bin", "n_tanks_at_reform"
  ),
  category = c(
    rep("Biography", 10),
    rep("Closure Type", 4),
    rep("Make-Model Cell", 5)
  ),
  description = c(
    "Earliest tank_installed_date at facility (time-invariant)",
    "Year of earliest install (time-invariant, replaces derived install_year)",
    "Latest tank_installed_date at facility",
    "Year of latest install",
    "Total tanks ever registered at facility",
    "Total tanks with a closure date",
    "Total tanks still open (no closure date)",
    "Facility had any tank installed before 1989",
    "All tanks at facility installed 1989 or later",
    "Facility birth before 1999 (from actual install, not panel appearance)",
    "Permanent tank closures (no subsequent install at facility)",
    "Replacement closures (facility installed another tank after)",
    "Any permanent closure this year (binary)",
    "Any replacement closure this year (binary)",
    "Facility make-model cell (wall_fuel_age at reform)",
    "Portfolio wall composition at reform: All-SW/All-DW/Mixed/Unknown",
    "Portfolio fuel at reform: Gasoline-Only/Diesel-Only/Mixed-Or-Other/Unknown",
    "Age bin of oldest tank at reform",
    "Count of tanks active at reform date"
  )
)
data_dict <- rbind(data_dict, new_vars)

dict_path <- here("Data", "Processed", "facility_leak_behavior_annual_data_dictionary.csv")
fwrite(data_dict, dict_path)
log_step(sprintf("✓ Data dictionary saved: %s", dict_path), 1)

#==============================================================================
# SCRIPT COMPLETE
#==============================================================================
cat("\n========================================\n")
cat("SCRIPT COMPLETE\n")
cat(sprintf("Total Runtime: %.1f minutes\n", 
            difftime(Sys.time(), script_start_time, units="mins")))
cat("========================================\n\n")

cat("✓ Outputs created:\n")
cat(sprintf("  1. %s\n", output_csv))
# cat(sprintf("  2. %s\n", output_rds))
cat(sprintf("  3. %s\n", output_monthly))
cat(sprintf("  4. %s\n", dict_path))
cat(sprintf("  5. %s\n", here("Data", "Analysis", "facility_make_model.rds")))
cat(sprintf("  6. %s\n", here("Data", "Analysis", "facility_biography.rds")))
cat("\n")