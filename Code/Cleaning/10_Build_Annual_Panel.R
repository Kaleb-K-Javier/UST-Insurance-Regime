#==============================================================================
# 10_Build_Master_Annual_Panel.R
#
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

suppressPackageStartupMessages({
  library(data.table)
  library(lubridate)
  library(here)
  library(zoo)
  library(stringr)
})

setDTthreads(14)

#==============================================================================
# SECTION 0: HELPER FUNCTIONS
#==============================================================================
coerce_date <- function(x) {
  if (inherits(x, "Date")) return(x)
  as.Date(x, origin = "1970-01-01")
}

clean_id <- function(x) toupper(trimws(as.character(x)))

get_data_path <- function(...) {

  root <- Sys.getenv("UST_DATA_ROOT", unset = file.path(Sys.getenv("HOME"), "UST", "Data"))
  file.path(root, ...)
}

log_step <- function(message, indent = 0) {
  prefix <- paste(rep("  ", indent), collapse = "")
  cat(sprintf("%s%s\n", prefix, message))
}

log_memory <- function(label) {
  gc_info <- gc()
  cat(sprintf("  [Memory] %s: %.1f MB used\n", label, sum(gc_info[, "used"]) / 1024))
}

parse_tx_lust_date <- function(x) {
  x_chr <- trimws(as.character(x))
  x_chr[x_chr %chin% c("", "NA", "NULL")] <- NA_character_
  suppressWarnings(as.Date(lubridate::parse_date_time(
    x_chr, orders = c("d-b-y", "d-b-Y", "m/d/y", "m/d/Y", "Y-m-d", "Ymd"), tz = "UTC"
  )))
}

cat("========================================\n")
cat("MASTER ANNUAL PANEL BUILDER\n")
cat("========================================\n\n")

#==============================================================================
# SECTION 1: LOAD AND HARMONIZE INPUT DATA
#==============================================================================
cat("SECTION 1: Loading and Harmonizing Data...\n")
section_start <- Sys.time()

# --- 1.1 LOAD RAW DATA ---
log_step("1.1 Loading raw data files...", 0)

# Texas tank-months
tx_tank_months <- fread(here("Data", "Processed", "texas_ust_facility_month_panel.csv"))
tx_cols <- c("FACILITY_ID", "UST_ID", "YEAR", "MONTH", "CAPACITY", "month_date", "install_date", "end_date",
             "single_walled", "double_walled", "is_gasoline", "is_diesel", "is_oil_kerosene", "is_jet_fuel", "is_other")
tx_tank_months <- tx_tank_months[, .SD, .SDcols = intersect(tx_cols, names(tx_tank_months))]
log_step(sprintf("TX tank-months: %s rows", format(nrow(tx_tank_months), big.mark=",")), 1)

# EPA tank panel
tank_panel_epa <- fread(here("Data", "Raw", "EPA_Region_6_and_EPA_states_tank_panel.csv"),
                        colClasses = c(facility_id = "character", tank_id = "character"))
epa_cols <- c("facility_id", "tank_id", "state", "tank_installed_date", "tank_closed_date", "capacity",
              "single_walled", "double_walled", "is_gasoline", "is_diesel", "is_oil_kerosene", 
              "is_jet_fuel", "is_other", "county_name", "county_geoid")
tank_panel_epa <- tank_panel_epa[, .SD, .SDcols = intersect(epa_cols, names(tank_panel_epa))]
log_step(sprintf("EPA tank panel: %s rows", format(nrow(tank_panel_epa), big.mark=",")), 1)

# LUST data
lust_epa <- fread(here("Data", "Processed", "all_lust_data.csv"), colClasses = c(facility_id = "character"))
log_step(sprintf("EPA LUST: %s rows", format(nrow(lust_epa), big.mark=",")), 1)

TX_LUST_SD <- fread(here("Data", "Raw", "TX_LUST.csv"))
TX_LUST_SD[, `:=`(facility_id = clean_id(facility_id), LUST_id = trimws(as.character(LUST_id)),
                  report_date = parse_tx_lust_date(report_date), nfa_date = parse_tx_lust_date(nfa_date))]
log_step(sprintf("TX LUST: %s rows", format(nrow(TX_LUST_SD), big.mark=",")), 1)

# Auxiliary data
tx_fac_raw <- fread(here("Data", "Raw", "raw_pst_fac.csv"))[, .(FACILITY_ID, SITE_COUNTY)]
fa_monthly <- fread(here("Data", "Raw", "fa_monthly.csv"))
fa_monthly <- fa_monthly[, .SD, .SDcols = intersect(c("FACILITY_ID","YEAR","MONTH","DETAIL_TYPE","CATEGORY","ISSUER_NAME"), names(fa_monthly))]
zurich_2012_lookup <- fread(here("Data", "Raw", "zurich_2012_lookup.csv"))
zurich_2012_lookup <- zurich_2012_lookup[, .SD, .SDcols = intersect(c("FACILITY_ID","had_zurich_2012"), names(zurich_2012_lookup))]

# --- 1.2-1.3 RENAME COLUMNS ---
log_step("1.2-1.3 Renaming and standardizing columns...", 0)

setnames(tx_tank_months, 
         old = c("FACILITY_ID", "UST_ID", "YEAR", "MONTH", "month_date", "install_date", "end_date", "CAPACITY"),
         new = c("facility_id", "tank_id", "panel_year", "panel_month", "date", "tank_installed_date_raw", "tank_closed_date_raw", "capacity"),
         skip_absent = TRUE)
tx_tank_months[, state := "Texas"]

setnames(tank_panel_epa, old = c("tank_installed_date", "tank_closed_date"),
         new = c("tank_installed_date_raw", "tank_closed_date_raw"), skip_absent = TRUE)

setnames(TX_LUST_SD, "LUST_id", "lust_id", skip_absent = TRUE)
TX_LUST_SD[, state := "Texas"]

setnames(tx_fac_raw, c("FACILITY_ID", "SITE_COUNTY"), c("facility_id", "county_raw"), skip_absent = TRUE)
setnames(fa_monthly, c("FACILITY_ID", "YEAR", "MONTH"), c("facility_id", "panel_year", "panel_month"), skip_absent = TRUE)
setnames(zurich_2012_lookup, "FACILITY_ID", "facility_id", skip_absent = TRUE)

# --- 1.4 STANDARDIZE IDs ---
log_step("1.4 Standardizing IDs and creating panel_id...", 0)

tx_tank_months[, `:=`(facility_id = clean_id(facility_id), tank_id = clean_id(tank_id))]
tank_panel_epa[, `:=`(facility_id = clean_id(facility_id), tank_id = clean_id(tank_id), state = trimws(state))]
lust_epa[, `:=`(facility_id = clean_id(facility_id), state = trimws(state))]
tx_fac_raw[, facility_id := clean_id(facility_id)]
fa_monthly[, facility_id := clean_id(facility_id)]
zurich_2012_lookup[, facility_id := clean_id(facility_id)]

# Create panel_id
tx_tank_months[, panel_id := paste(facility_id, state, sep = "_")]
tank_panel_epa[, panel_id := paste(facility_id, state, sep = "_")]
lust_epa[, panel_id := paste(facility_id, state, sep = "_")]
TX_LUST_SD[, panel_id := paste(facility_id, state, sep = "_")]
tx_fac_raw[, panel_id := paste(facility_id, "Texas", sep = "_")]
fa_monthly[, panel_id := paste(facility_id, "Texas", sep = "_")]
zurich_2012_lookup[, panel_id := paste(facility_id, "Texas", sep = "_")]

# Set keys
setkeyv(tx_tank_months, c("panel_id", "tank_id", "date"))
setkeyv(tank_panel_epa, c("panel_id", "tank_id"))
setkeyv(lust_epa, c("panel_id", "report_date"))
setkeyv(TX_LUST_SD, c("panel_id", "report_date"))
setkeyv(fa_monthly, c("panel_id", "panel_year", "panel_month"))

# --- 1.5 COERCE DATES (CRITICAL - MUST BE BEFORE fcase) ---
log_step("1.5 Coercing dates to Date class (CRITICAL)...", 0)

tx_tank_months[, `:=`(tank_installed_date_raw = coerce_date(tank_installed_date_raw),
                      tank_closed_date_raw = coerce_date(tank_closed_date_raw))]
tank_panel_epa[, `:=`(tank_installed_date_raw = coerce_date(tank_installed_date_raw),
                      tank_closed_date_raw = coerce_date(tank_closed_date_raw))]
lust_epa[, report_date := coerce_date(report_date)]
TX_LUST_SD[, report_date := coerce_date(report_date)]

# Flag bad install dates
tx_tank_months[, bad_install_date := as.integer(is.na(tank_installed_date_raw) | 
                                                  year(tank_installed_date_raw) < 1900 | year(tank_installed_date_raw) > 2025)]
tank_panel_epa[, bad_install_date := as.integer(is.na(tank_installed_date_raw) | 
                                                  year(tank_installed_date_raw) < 1900 | year(tank_installed_date_raw) > 2025)]
tx_tank_months[bad_install_date == 1, tank_installed_date_raw := as.Date("9999-09-01")]
tank_panel_epa[bad_install_date == 1, tank_installed_date_raw := as.Date("9999-09-01")]

log_step(sprintf("TX bad install dates: %s (%.1f%%)", format(sum(tx_tank_months$bad_install_date), big.mark=","),
                 100 * mean(tx_tank_months$bad_install_date)), 1)
log_step(sprintf("EPA bad install dates: %s (%.1f%%)", format(sum(tank_panel_epa$bad_install_date), big.mark=","),
                 100 * mean(tank_panel_epa$bad_install_date)), 1)

# Clean corrupted closure dates
tx_tank_months[!is.na(tank_closed_date_raw) & (year(tank_closed_date_raw) < 1900 | year(tank_closed_date_raw) > 2030),
               tank_closed_date_raw := NA_Date_]
tank_panel_epa[!is.na(tank_closed_date_raw) & (year(tank_closed_date_raw) < 1900 | year(tank_closed_date_raw) > 2030),
               tank_closed_date_raw := NA_Date_]

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

# --- 1.7 FINAL DATE TYPE STANDARDIZATION ---
log_step("1.7 Final date type standardization...", 0)
tx_tank_months[, `:=`(date = as.Date(date), tank_installed_date_raw = as.Date(tank_installed_date_raw),
                      tank_closed_date_raw = as.Date(tank_closed_date_raw), 
                      tank_panel_start_date = as.Date(tank_panel_start_date),
                      tank_panel_end_date = as.Date(tank_panel_end_date))]
tank_panel_epa[, `:=`(tank_installed_date_raw = as.Date(tank_installed_date_raw),
                      tank_closed_date_raw = as.Date(tank_closed_date_raw),
                      tank_panel_start_date = as.Date(tank_panel_start_date),
                      tank_panel_end_date = as.Date(tank_panel_end_date))]
lust_epa[, `:=`(report_date = as.Date(report_date), nfa_date = as.Date(nfa_date))]
TX_LUST_SD[, `:=`(report_date = as.Date(report_date), nfa_date = as.Date(nfa_date))]

# --- 1.8-1.9 REMOVE BAD FACILITIES ---
log_step("1.8-1.9 Removing facilities with bad dates...", 0)
bad_panel_ids <- unique(c(tx_tank_months[bad_install_date == 1, panel_id],
                          tank_panel_epa[bad_install_date == 1, panel_id]))

if (length(bad_panel_ids) > 0) {
  tx_before <- uniqueN(tx_tank_months$panel_id)
  epa_before <- uniqueN(tank_panel_epa$panel_id)
  tx_tank_months <- tx_tank_months[!panel_id %in% bad_panel_ids]
  tank_panel_epa <- tank_panel_epa[!panel_id %in% bad_panel_ids]
  log_step(sprintf("Removed %d facilities (TX: %d→%d, EPA: %d→%d)", length(bad_panel_ids),
                   tx_before, uniqueN(tx_tank_months$panel_id), 
                   epa_before, uniqueN(tank_panel_epa$panel_id)), 1)
}

cat(sprintf("✓ Section 1 complete in %.1f seconds\n\n", difftime(Sys.time(), section_start, units="secs")))

#==============================================================================
# SECTION 2: CREATE UNIFIED TANK PANEL
#==============================================================================
cat("SECTION 2: Creating Unified Tank Panel...\n")
start_time <- Sys.time()

needed_cols <- c("panel_id", "facility_id", "state", "tank_id",
                 "tank_installed_date_raw", "tank_closed_date_raw",
                 "tank_panel_start_date", "tank_panel_end_date",
                 "capacity", "single_walled", "double_walled",
                 "is_gasoline", "is_diesel", "is_oil_kerosene", "is_jet_fuel", "is_other",
                 "county_name", "county_geoid")

# --- 2.1 COLLAPSE EPA TO TANK LEVEL ---
log_step("2.1 Collapsing EPA to tank level...", 0)
epa_tank_level <- tank_panel_epa[state != "Texas", .(
  tank_installed_date_raw = suppressWarnings(min(tank_installed_date_raw, na.rm = TRUE)),
  tank_closed_date_raw = suppressWarnings(max(tank_closed_date_raw, na.rm = TRUE)),
  tank_panel_start_date = suppressWarnings(min(tank_panel_start_date, na.rm = TRUE)),
  tank_panel_end_date = suppressWarnings(max(tank_panel_end_date, na.rm = TRUE)),
  capacity = suppressWarnings(max(capacity, na.rm = TRUE)),
  single_walled = as.integer(any(single_walled == 1, na.rm = TRUE)),
  double_walled = as.integer(any(double_walled == 1, na.rm = TRUE)),
  is_gasoline = as.integer(any(is_gasoline == 1, na.rm = TRUE)),
  is_diesel = as.integer(any(is_diesel == 1, na.rm = TRUE)),
  is_oil_kerosene = as.integer(any(is_oil_kerosene == 1, na.rm = TRUE)),
  is_jet_fuel = as.integer(any(is_jet_fuel == 1, na.rm = TRUE)),
  is_other = as.integer(any(is_other == 1, na.rm = TRUE)),
  county_name = na.omit(county_name)[1],
  county_geoid = as.character(na.omit(county_geoid)[1]),
  panel_id = panel_id[1]
), by = .(facility_id, tank_id, state)]

# Clean infinites and ensure Date class
for (v in c("tank_installed_date_raw", "tank_closed_date_raw", "tank_panel_start_date", "tank_panel_end_date")) {
  epa_tank_level[is.infinite(get(v)), (v) := NA_Date_]
  epa_tank_level[[v]] <- as.Date(epa_tank_level[[v]])
}
epa_tank_level[is.infinite(capacity), capacity := NA_real_]
epa_tank_level[is.na(tank_panel_start_date), tank_panel_start_date := PANEL_START]
epa_tank_level[is.na(tank_panel_end_date), tank_panel_end_date := PANEL_END]

# Add missing columns
for (col in setdiff(needed_cols, names(epa_tank_level))) epa_tank_level[, (col) := NA]
epa_tank_level <- epa_tank_level[, ..needed_cols]
log_step(sprintf("EPA tanks: %s", format(nrow(epa_tank_level), big.mark=",")), 1)

# --- 2.2 COLLAPSE TX TO TANK LEVEL ---
log_step("2.2 Collapsing TX to tank level...", 0)

# TX county lookup
tx_county_lookup <- NULL
if (nrow(tx_fac_raw) > 0) {
  clean_county <- function(x) {
    x <- toupper(trimws(as.character(x)))
    x <- gsub("[^A-Z ]", " ", x); x <- gsub("\\bCOUNTY\\b", "", x); x <- gsub("\\s+", " ", x)
    trimws(x)
  }
  tx_county_lookup <- unique(tx_fac_raw[!is.na(county_raw) & county_raw != "",
                                         .(panel_id, facility_id, county_name = clean_county(county_raw))], by = "panel_id")
  tx_county_lookup[, county_geoid := NA_character_]
  
  # Try tigris for GEOID (graceful failure)
  if (requireNamespace("tigris", quietly = TRUE)) {
    tryCatch({
      suppressMessages(tx_cnty_sf <- tigris::counties(state = "TX", cb = TRUE, year = 2020, progress_bar = FALSE))
      cnty_xwalk <- data.table(county_name_clean = clean_county(tx_cnty_sf$NAME),
                               county_geoid = as.character(tx_cnty_sf$GEOID),
                               county_name_full = toupper(tx_cnty_sf$NAME))
      tx_county_lookup <- cnty_xwalk[tx_county_lookup, on = .(county_name_clean = county_name)]
      tx_county_lookup[, county_name := fifelse(!is.na(county_name_full), county_name_full, county_name_clean)]
      tx_county_lookup[, c("county_name_clean", "county_name_full") := NULL]
      log_step("✓ TX county GEOIDs loaded from Census", 2)
    }, error = function(e) log_step("⚠ Census Bureau unavailable, TX GEOIDs will be NA", 2))
  }
}

tx_tank_level <- tx_tank_months[, .(
  tank_installed_date_raw = suppressWarnings(min(tank_installed_date_raw, na.rm = TRUE)),
  tank_closed_date_raw = suppressWarnings(max(tank_closed_date_raw, na.rm = TRUE)),
  tank_panel_start_date = suppressWarnings(min(tank_panel_start_date, na.rm = TRUE)),
  tank_panel_end_date = suppressWarnings(max(tank_panel_end_date, na.rm = TRUE)),
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

# Clean infinites
for (v in c("tank_installed_date_raw", "tank_closed_date_raw", "tank_panel_start_date", "tank_panel_end_date")) {
  tx_tank_level[is.infinite(get(v)), (v) := NA_Date_]
  tx_tank_level[[v]] <- as.Date(tx_tank_level[[v]])
}
tx_tank_level[is.infinite(capacity), capacity := NA_real_]
tx_tank_level[is.na(tank_panel_start_date), tank_panel_start_date := PANEL_START]
tx_tank_level[is.na(tank_panel_end_date), tank_panel_end_date := PANEL_END]

# Attach county
if (!is.null(tx_county_lookup) && nrow(tx_county_lookup) > 0) {
  tx_tank_level <- tx_county_lookup[, .(panel_id, county_name, county_geoid)][tx_tank_level, on = .(panel_id)]
} else {
  tx_tank_level[, `:=`(county_name = NA_character_, county_geoid = NA_character_)]
}

# Add missing columns
for (col in setdiff(needed_cols, names(tx_tank_level))) tx_tank_level[, (col) := NA]
tx_tank_level <- tx_tank_level[, ..needed_cols]
log_step(sprintf("TX tanks: %s", format(nrow(tx_tank_level), big.mark=",")), 1)

# --- 2.3 COMBINE ---
log_step("2.3 Combining into unified tank panel...", 0)
tank_panel_unified <- rbindlist(list(epa_tank_level, tx_tank_level), use.names = TRUE)
setkeyv(tank_panel_unified, c("panel_id", "tank_id"))

cat(sprintf("✓ Unified tank panel: %s tanks, %s facilities\n", 
            format(nrow(tank_panel_unified), big.mark=","), format(uniqueN(tank_panel_unified$panel_id), big.mark=",")))
cat(sprintf("✓ Section 2 complete in %.1f seconds\n\n", difftime(Sys.time(), start_time, units="secs")))

# Save unified inventory
saveRDS(tank_panel_unified, here("Data", "Processed", "tank_panel_unified.rds"))
log_step("✓ Saved tank_panel_unified.rds", 0)


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
  
  had_zurich_2012 = max(had_zurich_2012, na.rm=TRUE),
  dropped_by_zurich = max(dropped_by_zurich, na.rm=TRUE)
  
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
    tank_closure_known_leak_wide = as.integer(any(has_kno_wid))
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