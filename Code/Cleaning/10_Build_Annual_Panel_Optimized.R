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

# 1.3 Load Texas FR Data (Auxiliary)
log_step("Loading Texas FR Auxiliary Data...", 0)
# FA Monthly
fa_monthly <- fread(here("Data", "Raw", "fa_monthly.csv"))
# Filter/Select FA Monthly columns dynamically as before
fa_issuer_cols <- grep("^ISSUER_NAME", names(fa_monthly), value = TRUE)
fa_keep <- unique(c("FACILITY_ID", "YEAR", "MONTH", "DETAIL_TYPE", "CATEGORY", 
                    "uses_private", "uses_self", "fr_covered", 
                    "transition_month", "multiple_contracts", "dropped_by_zurich",
                    fa_issuer_cols))
fa_monthly <- fa_monthly[, .SD, .SDcols = intersect(fa_keep, names(fa_monthly))]

# Zurich Lookup
zurich_2012_lookup <- fread(here("Data", "Raw", "zurich_2012_lookup.csv"))

log_step("Data loading complete.", 1)

head(fa_monthly[,FACILITY_ID])
# SECTION 2: DATA CLEANING & PANEL PREP
#==============================================================================
cat("\n========================================\n")
cat("SECTION 2: CLEANING & FILTERING\n")
cat("========================================\n\n")

# 2.1 Standardize IDs & Create Panel ID
# Logic: panel_id = facility_id + "_" + state
log_step("Creating composite panel_id...", 0)

# Master Tanks (Already standardized in 04_Master_Build, but robustly ensure consistency)
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

# --- CRITICAL FIX: Standardize Texas Auxiliary Files to Match Harmonized ID ---
# Logic: Harmonized Data stripped leading zeros for Texas. We must do the same here.
# Also: Suffix must be "_TX", not "_TEXAS".

# FA Monthly
log_step("Standardizing FA Monthly IDs (Stripping zeros + 'TX' suffix)...", 1)
fa_monthly[, clean_id := str_remove(toupper(trimws(as.character(FACILITY_ID))), "^0+")]
fa_monthly[, `:=`(
  facility_id = clean_id,
  panel_id    = paste(clean_id, "TX", sep = "_") 
)]
fa_monthly[, clean_id := NULL] # Cleanup

# Zurich Lookup
log_step("Standardizing Zurich IDs (Stripping zeros + 'TX' suffix)...", 1)
# Check names first if unsure: names(zurich_2012_lookup)
# Likely Fix: Change facility_id to FACILITY_ID
zurich_2012_lookup[, clean_id := str_remove(toupper(trimws(as.character(FACILITY_ID))), "^0+")]
zurich_2012_lookup[, `:=`(
  facility_id = clean_id,
  panel_id    = paste(clean_id, "TX", sep = "_")
)]
zurich_2012_lookup[, clean_id := NULL] # Cleanup


# 2.2 Date Type Standardization
log_step("Standardizing date types...", 0)
master_tanks[, `:=`(
  tank_installed_date = coerce_date(tank_installed_date),
  tank_closed_date    = coerce_date(tank_closed_date)
)]
master_lust[, report_date := coerce_date(report_date)]


# 2.3 Strict Filtering Rules (Per Instructions)
log_step("Applying strict facility filtering rules...", 0)
n_fac_orig <- uniqueN(master_tanks$panel_id)

# Rule 1: Drop facility if ANY tank is 'Closed' but missing closure date
# We check this by grouping by panel_id
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

# Create panel_start_date (Left censor at 1970)
# If install date is NA (and we kept it), default to Panel Start to be safe, 
# though filtering should have caught most active ones.
master_tanks[, tank_panel_start_date := fifelse(
  is.na(tank_installed_date) | tank_installed_date < PANEL_START,
  PANEL_START,
  tank_installed_date
)]

# Create panel_end_date (Right censor at 2025 or Closure)
master_tanks[, tank_panel_end_date := PANEL_END] # Default to active until 2025

# If closed, use closure date (clamped to 2025)
master_tanks[!is.na(tank_closed_date), tank_panel_end_date := pmin(tank_closed_date, PANEL_END)]

# Validation: Start <= End
# If a tank closed before 1970, End < Start. These are "dead" relative to panel.
master_tanks[, valid_panel_tank := tank_panel_end_date >= tank_panel_start_date]

# Exclude tanks that closed before the panel began
# (We keep them in 'master_tanks' for now if we need baseline stats, 
# but we filter them out for the backbone creation)
active_panel_tanks <- master_tanks[valid_panel_tank == TRUE]

log_step(sprintf("  Tanks active within 1970-2025 window: %s", 
                 format(nrow(active_panel_tanks), big.mark=",")), 1)


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
  # Carry forward geographic info
  county_name = first(na.omit(county_name)),
  county_fips = first(na.omit(county_fips))
), by = .(facility_id, state, panel_id)]


# =========================================================================
# FAST BACKBONE CREATION (Vectorized)
# =========================================================================

# 1. Convert Start/End Dates to "Absolute Month Integers" (Integers are fast)
#    (Formula: (Year - 1970) * 12 + Month)
facility_ranges[, start_m_abs := (year(start_date) - 1970L) * 12L + month(start_date)]
facility_ranges[, end_m_abs   := (year(end_date)   - 1970L) * 12L + month(end_date)]

# 2. Expand using C-optimized Integer Sequence
#    data.table optimizes 'x:y' inside 'j' significantly better than 'seq()'
#    We include county/state in 'by' to carry them forward automatically
log_step("Expanding facilities (Integer Method)...", 0)

panel_dates <- facility_ranges[, .(
    month_abs = start_m_abs:end_m_abs 
), by = .(panel_id, state, county_name, county_fips)]

# 3. Fast Date Recovery (Lookup Table Method)
#    Instead of running as.Date() on millions of rows, run it on ~700 unique months 
#    and join. This is orders of magnitude faster.
min_m <- min(panel_dates$month_abs)
max_m <- max(panel_dates$month_abs)

date_lookup <- data.table(month_abs = min_m:max_m)
date_lookup[, `:=`(
    year  = 1970L + (month_abs - 1L) %/% 12L,
    month = (month_abs - 1L) %% 12L + 1L
)]
# Create Date object once per unique month
date_lookup[, date := as.IDate(sprintf("%04d-%02d-01", year, month))]

# 4. Merge Date back to Panel
log_step("Mapping dates...", 0)
panel_dates <- merge(panel_dates, date_lookup, by = "month_abs", sort = FALSE)

# Cleanup columns to match your original schema
panel_dates[, `:=`(panel_year = year, panel_month = month)]
panel_dates[, c("month_abs", "year", "month") := NULL]

log_step(sprintf("  Backbone created: %s facility-months", format(nrow(panel_dates), big.mark=",")), 1)

rm(facility_ranges, date_lookup)
# =========================================================================
# SECTION 4: COMPUTE MONTHLY TANK COMPOSITION (CORRECTED)
# =========================================================================
cat("\n========================================\n")
cat("SECTION 4: MONTHLY TANK COMPOSITION (NON-EQUI JOIN)\n")
cat("========================================\n\n")

# 1. Prepare tanks for join (numeric dates for speed)
#    Ensure install_date is IDate for compatibility
tanks_for_join <- active_panel_tanks[, .(
    panel_id, facility_id, state,
    tank_start_num = as.numeric(tank_panel_start_date),
    tank_end_num   = as.numeric(tank_panel_end_date),
    install_date_for_age = as.IDate(fifelse(tank_installed_date < PANEL_START, PANEL_START, tank_installed_date)),
    capacity, single_walled, double_walled, 
    is_gasoline, is_diesel, is_oil_kerosene, is_jet_fuel, is_other
)]

# 2. Process by State
states <- unique(panel_dates$state)
tank_month_list <- vector("list", length(states))

log_step("Performing non-equi joins by state...", 0)

for (i in seq_along(states)) {
    st <- states[i]

    # Subset backbone and tanks
    backbone_st <- panel_dates[state == st]
    tanks_st    <- tanks_for_join[state == st]

    if(nrow(backbone_st) == 0 || nrow(tanks_st) == 0) next
    
    # Numeric date for join logic ONLY
    backbone_st[, date_num := as.numeric(date)]

    # The Join: Tanks active during the specific month
    # date_num is used for logic but dropped from result. 
    # 'date' (IDate) is preserved automatically.
    active <- tanks_st[backbone_st, 
                       on = .(panel_id, 
                              tank_start_num <= date_num, 
                              tank_end_num >= date_num),
                       nomatch = 0L, allow.cartesian = TRUE]

    # REMOVED: active[, date := as.Date(date_num...)] <-- CAUSE OF ERROR
    
    # Calc Age (using the preserved 'date' column)
    active[, tank_age := as.numeric(date - install_date_for_age) / 365.25]

    # Aggregation
    tank_month_list[[i]] <- active[, .(
      active_tanks   = .N,
      avg_tank_age   = mean(tank_age, na.rm=TRUE),
      total_capacity = sum(capacity, na.rm=TRUE),
      single_tanks   = sum(single_walled, na.rm=TRUE),
      double_tanks   = sum(double_walled, na.rm=TRUE),
      has_gasoline   = max(is_gasoline, na.rm=TRUE),
      has_diesel     = max(is_diesel, na.rm=TRUE)
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
# Deduplicate (multiple leaks in same month = 1 incident for binary flag, or sum?)
# Using sum for counts, binary for year flag later.
leaks_agg <- leaks_agg[, .(leak_incident = sum(leak_incident)), by = .(panel_id, date)]

# 5.2 Closures (from Master Tanks)
closures_agg <- master_tanks[!is.na(tank_closed_date), .(
  tanks_closed = .N,
  capacity_closed = sum(capacity, na.rm=TRUE)
), by = .(panel_id, date = floor_date(tank_closed_date, "month"))]

# 5.3 Installs (from Master Tanks)
installs_agg <- master_tanks[!is.na(tank_installed_date), .(
  tanks_installed = .N,
  capacity_installed = sum(capacity, na.rm=TRUE)
), by = .(panel_id, date = floor_date(tank_installed_date, "month"))]

# 5.4 Merge All
monthly <- merge(monthly, leaks_agg, by=c("panel_id", "date"), all.x=TRUE)
monthly <- merge(monthly, closures_agg, by=c("panel_id", "date"), all.x=TRUE)
monthly <- merge(monthly, installs_agg, by=c("panel_id", "date"), all.x=TRUE)

# Fill NAs
fill_cols <- c("leak_incident", "tanks_closed", "capacity_closed", "tanks_installed", "capacity_installed")
for(col in fill_cols) set(monthly, which(is.na(monthly[[col]])), col, 0)

# =========================================================================
# SECTION 5.5: FINALIZE MONTHLY (WITHOUT MERGING TEXAS YET)
# =========================================================================
# NOTE: We skip the Texas merge here to save RAM. We will do it in Section 6.

# 1. Immediate Memory Cleanup
#    Remove intermediate aggregations now that they are merged into 'monthly'
rm(leaks_agg, closures_agg, installs_agg)

# 2. Remove Redundant Panels
#    'active_panel_tanks' is a subset of 'master_tanks'.
#    'panel_dates' is the pre-merge version of 'monthly'.
rm(active_panel_tanks, panel_dates)
gc()

# 3. Shrink Master Files for Section 7
#    Section 7 ONLY needs 'panel_id' and specific dates.
#    We drop everything else to free up ~2GB+ of RAM.
log_step("Shrinking Master Tables to bare minimum...", 0)

# Keep only Closed tanks with valid dates
master_tanks <- master_tanks[!is.na(tank_closed_date), .(panel_id, tank_closed_date)]

# Keep only Leaks with valid dates
master_lust <- master_lust[!is.na(report_date), .(panel_id, report_date)]

# 4. Trigger Deep Garbage Collection
log_memory("Pre-Aggregation Cleanup")
gc()


# =========================================================================
# SECTION 6: ANNUAL AGGREGATION & LATE MERGE
# =========================================================================
cat("\n========================================\n")
cat("SECTION 6: ANNUAL AGGREGATION (LATE MERGE STRATEGY)\n")
cat("========================================\n\n")

# 6.1 Create Annual Backbone (Aggregation)
#    We aggregate the massive monthly panel down to facility-years.
log_step("Aggregating monthly panel to annual...", 0)

# Add YEAR column if not present
if(!"YEAR" %in% names(monthly)) monthly[, YEAR := year(date)]

# A. December Snapshot (Stocks: Capacity, Active Tanks)
#    We take the state of the facility in the last month of the year
setorder(monthly, panel_id, YEAR, date)
december <- monthly[, .SD[.N], by = .(panel_id, YEAR), 
                    .SDcols = c("active_tanks", "total_capacity", "avg_tank_age", 
                                "single_tanks", "double_tanks", "state", 
                                "county_name", "county_fips")]

# Rename columns for clarity (these are end-of-year stocks)
setnames(december, 
         old = c("active_tanks", "total_capacity", "avg_tank_age", "single_tanks", "double_tanks"), 
         new = paste0(c("active_tanks", "total_capacity", "avg_tank_age", "single_tanks", "double_tanks"), "_dec"))

# B. Annual Flows (Events: Leaks, Closures, etc.)
annual_flows <- monthly[, .(
    n_leaks      = sum(leak_incident, na.rm=TRUE),
    leak_year    = as.integer(sum(leak_incident, na.rm=TRUE) > 0),
    n_closures   = sum(tanks_closed, na.rm=TRUE),
    closure_year = as.integer(sum(tanks_closed, na.rm=TRUE) > 0),
    n_installs   = sum(tanks_installed, na.rm=TRUE)
), by = .(panel_id, YEAR)]

# C. Merge Stocks and Flows
annual <- merge(annual_flows, december, by = c("panel_id", "YEAR"))

# D. Cleanup Monthly to free RAM
#    CRITICAL: This releases the huge monthly dataset.
rm(monthly, december, annual_flows)
gc()

log_step(sprintf("Annual Panel Created: %s rows", format(nrow(annual), big.mark=",")), 1)


# 6.2 Late Merge: Texas FR Data (Aggregated to Annual)
log_step("Preparing Texas FR data for annual merge...", 0)

# A. Aggregate FA Monthly to Annual
#    Calculate coverage share: (Months with Coverage) / 12
#    We filter for valid rows, count them per year, and join.
fa_annual <- fa_monthly[
  !is.na(CATEGORY) & CATEGORY != "None",
  .(months_covered = .N),
  by = .(panel_id, YEAR)
]

# B. Merge FA Data to Annual Panel
annual <- merge(annual, fa_annual, by = c("panel_id", "YEAR"), all.x = TRUE)
annual[is.na(months_covered), months_covered := 0]
annual[, fr_coverage_share := months_covered / 12]

# C. Merge Zurich Lookup
#    Make sure Zurich IDs are cleaned (Section 2.1 logic)
#    Note: Section 2.1 already cleaned 'zurich_2012_lookup', so we just use it.
#    If the object was deleted in cleanup, reload it here or ensure it wasn't deleted.
#    (Based on the script flow, if you deleted it in Section 5.5, reload it below).
if(!exists("zurich_2012_lookup")) {
  zurich_2012_lookup <- fread(here("Data", "Raw", "zurich_2012_lookup.csv"))
  zurich_2012_lookup[, clean_id := str_remove(toupper(trimws(as.character(FACILITY_ID))), "^0+")]
  zurich_2012_lookup[, panel_id := paste(clean_id, "TX", sep = "_")]
}

annual <- merge(annual, unique(zurich_2012_lookup[, .(panel_id, had_zurich_2012)]), 
                by = "panel_id", all.x = TRUE)

# D. Create Treatment Variable (Dropped by Zurich)
#    Definition: Had Zurich 2012, Year >= 2012, and Zero Months of Coverage
annual[state == "TX", dropped_by_zurich := as.integer(
    had_zurich_2012 == 1 & YEAR >= 2012 & months_covered == 0
)]
annual[is.na(dropped_by_zurich), dropped_by_zurich := 0]
annual[is.na(had_zurich_2012), had_zurich_2012 := 0]

# E. Final Cleanup of Texas Data
rm(fa_monthly, fa_annual, zurich_2012_lookup)
gc()

log_step("Annual merge complete.", 1)


# 6.3 Calculate YoY Changes
setorder(annual, panel_id, YEAR)
annual[, `:=`(
  panel_year = YEAR,  # Rename for consistency with legacy code
  capacity_change_year = total_capacity_dec - shift(total_capacity_dec, 1, type = "lag"),
  net_tank_change      = active_tanks_dec - shift(active_tanks_dec, 1, type = "lag")
), by = panel_id]

#==============================================================================
# SECTION 7: ROBUST LEAK CLASSIFICATION
#==============================================================================
cat("\n========================================\n")
cat("SECTION 7: LEAK CLASSIFICATION\n")
cat("========================================\n\n")

# Prepare tables for matching
closures_exact <- master_tanks[!is.na(tank_closed_date), .(panel_id, closure_date = tank_closed_date)]
leaks_exact    <- master_lust[!is.na(report_date), .(panel_id, report_date)]

# Only proceed if we have data
if(nrow(closures_exact) > 0 && nrow(leaks_exact) > 0) {
  
  setkey(closures_exact, panel_id)
  setkey(leaks_exact, panel_id)
  
  # Cartesian join of all leaks to all closures within facility
  pairs <- leaks_exact[closures_exact, on = .(panel_id), allow.cartesian = TRUE]
  pairs[, diff := as.numeric(report_date - closure_date)]
  
  # Apply Classification Definitions
  pairs[, `:=`(
    # Primary: 0 to 60 days after
    is_rev_prim = diff >= 0 & diff <= 60,
    is_kno_prim = diff < -180,
    # Regulatory: 0 to 45 days
    is_rev_reg  = diff >= 0 & diff <= 45,
    is_kno_reg  = diff < -180
  )]
  
  # Summarize to Facility-Year level (based on closure year)
  pairs[, closure_year := year(closure_date)]
  
  closure_flags <- pairs[, .(
    tank_closure_revealed = as.integer(any(is_rev_prim)),
    tank_closure_known    = as.integer(any(is_kno_prim)),
    tank_closure_rev_reg  = as.integer(any(is_rev_reg))
  ), by = .(panel_id, panel_year = closure_year)]
  
  # Merge into annual panel
  annual <- merge(annual, closure_flags, by = c("panel_id", "panel_year"), all.x = TRUE)
  
  # Fill NAs with 0
  annual[is.na(tank_closure_revealed), tank_closure_revealed := 0]
  annual[is.na(tank_closure_known), tank_closure_known := 0]
  annual[is.na(tank_closure_rev_reg), tank_closure_rev_reg := 0]
  
} else {
  annual[, `:=`(tank_closure_revealed=0, tank_closure_known=0, tank_closure_rev_reg=0)]
}


#==============================================================================
# SECTION 8: SURVIVAL & POLICY VARIABLES
#==============================================================================
cat("\n========================================\n")
cat("SECTION 8: FINAL VARIABLES & SAVE\n")
cat("========================================\n\n")

# 8.1 Survival
setorder(annual, panel_id, panel_year)
annual[, `:=`(
  cumulative_leaks = cumsum(n_leaks),
  cumulative_closures = cumsum(n_closures),
  ever_leaked = as.integer(cumsum(leak_year) > 0),
  # Exit flag: if this is the last year we see them AND it's before 2025
  last_year = max(panel_year),
  first_year = min(panel_year)
), by = panel_id]

annual[, exit_flag := as.integer(panel_year == last_year & last_year < 2025)]

# 8.2 Policy Flags
annual[, `:=`(
  post_1999 = as.integer(panel_year >= 1999),
  texas_treated = as.integer(state == "TX"),
  is_incumbent = as.integer(first_year < 1999)
)]

# 8.3 Saving
output_csv <- here("Data", "Processed", "facility_leak_behavior_annual_panel.csv")
fwrite(annual, output_csv)
log_step(sprintf("Saved CSV: %s", output_csv), 1)

# output_rds <- here("Data", "Processed", "facility_leak_behavior_annual.rds")
# saveRDS(annual, output_rds)
# log_step(sprintf("Saved RDS: %s", output_rds), 1)

cat("\n========================================\n")
cat("SCRIPT COMPLETE\n")
cat(sprintf("Total Time: %.1f minutes\n", difftime(Sys.time(), script_start_time, units="mins")))
cat("========================================\n")