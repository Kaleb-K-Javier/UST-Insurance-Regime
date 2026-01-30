## Tennessee UST Dataset Creation
# Script: 12_Clean_TN.R
# Purpose: Harmonize TN UST/LUST data matching the LA/NM Architecture.
# Inputs:
#   1. State Tanks: 'ust_all-tn-compartments.xlsx'
#   2. State LUST: 'ust_all-tn-environmental-sites.xlsx'
#   3. EPA Geo: 'Facilities.csv' (Lat/Long)
#   4. Crosswalk: 'uszips.csv' (Zip -> County)

library(tidyverse)
library(data.table)
library(here)
library(janitor)
library(readxl)
library(stringr)
library(lubridate)

# 0. Setup ---------------------------------------------------------------------
state_abbr <- "TN"
tn_path    <- here("Data", "Raw", "state_databases", "Tennessee")
epa_path   <- here("Data", "Raw") # Contains Facilities.csv and uszips.csv
out_dir    <- here("Data", "Raw", "state_databases", "Tennessee")

if (!dir.exists(tn_path)) stop("TN data directory not found!")

# 1. Helper Functions ----------------------------------------------------------

# A. ID Cleaner (Matches Diagnostic Findings)
clean_ids <- function(x) {
  x <- as.character(x)
  x <- trimws(x)
  # EPA has "TN" prefix (e.g. TN10021), State does not (10021). Strip it.
  x <- gsub("^TN[- ]?", "", x, ignore.case = TRUE)
  return(x)
}

# B. Robust Tank Date Parser (Excel Serial & Text)
parse_tn_tank_date <- function(x) {
  num_dates <- suppressWarnings(as.numeric(x))
  d_out <- as.Date(num_dates, origin = "1899-12-30")
  
  needs_text <- !is.na(x) & is.na(d_out)
  if (any(needs_text)) {
    txt_vals <- x[needs_text]
    # TN tanks sometimes use YYYY-MM-DD
    d_parsed <- as.Date(parse_date_time(txt_vals, orders = c("ymd", "b-d-Y", "mdY"), quiet = TRUE))
    d_out[needs_text] <- d_parsed
  }
  return(d_out)
}

# C. LUST Date Parser (Specific to EnvSites: MMM-DD-YYYY)
parse_tn_lust_date <- function(x) {
  x <- trimws(x)
  as.Date(parse_date_time(x, orders = c("b-d-Y", "b-d-y", "mdY"), quiet = TRUE))
}

# D. Wall Classification (Tennessee Specific Algorithm)
classify_walls <- function(dt) {
  # Initialize Flags
  dt[, `:=`(double_walled=0, single_walled=0, unknown_walled=0)]
  
  # Standardize Inputs
  cat_lower <- tolower(dt$category_of_construction)
  mat_lower <- tolower(dt$tank_construction)
  install_date <- dt$tank_installed_date
  
  # Regulatory Date Constant: July 24, 2007 (Tenn. Comp. R. & Regs. 0400-18-01-.02(1)(c))
  TN_SECONDARY_MANDATE <- as.Date("2007-07-24")
  FRP_DOUBLE_STANDARD  <- as.Date("1990-01-01")
  
  # --- LOGIC BLOCK ---
  
  # 1. TENNESSEE SECONDARY CONTAINMENT DATE (Highest Priority)
  # If installed after mandate, must be secondary contained.
  dt[!is.na(install_date) & install_date >= TN_SECONDARY_MANDATE, double_walled := 1]
  
  # 2. EXPLICIT CATEGORY OF CONSTRUCTION (If not already double by date)
  # Only check rows not already classified as double
  dt[double_walled == 0 & grepl("double", cat_lower), double_walled := 1]
  dt[double_walled == 0 & grepl("single", cat_lower), single_walled := 1]
  
  # 3. SECONDARY CONTAINED TANK CONSTRUCTION TYPES (If still unclassified)
  # Composite and Poly Jacket are inherently double-walled
  dt[double_walled == 0 & single_walled == 0 & (
    grepl("composite", mat_lower) | 
    grepl("jacket", mat_lower)
  ), double_walled := 1]
  
  # 4. FIBERGLASS/PLASTIC (DATE-DEPENDENT)
  # Only applied to "Fiberglass Reinforced Plastic" if unclassified
  # Post-1990 -> Double; Pre-1990 or Null -> Unknown (Skip assignment)
  dt[double_walled == 0 & single_walled == 0 & grepl("fiberglass reinforced plastic", mat_lower) & 
       !is.na(install_date) & install_date >= FRP_DOUBLE_STANDARD, 
     double_walled := 1]
  
  # 5. SINGLE-WALLED STEEL INDICATORS
  # Basic Steel and StiP3
  dt[double_walled == 0 & single_walled == 0 & (
    mat_lower == "steel" | 
    grepl("stip3", mat_lower) | 
    grepl("cathodically protected steel", mat_lower)
  ), single_walled := 1]
  
  # 6. CONCRETE TANKS
  dt[double_walled == 0 & single_walled == 0 & grepl("concrete", mat_lower), single_walled := 1]
  
  # 7. UNKNOWN / INSUFFICIENT DATA
  # Any record that didn't hit a rule above gets marked Unknown
  dt[double_walled == 0 & single_walled == 0, unknown_walled := 1]
  
  return(dt)
}

# E. Substance Classification
classify_substance <- function(dt) {
  dt[, sub_lower := tolower(product)]
  dt[, `:=`(
    is_gasoline = as.integer(grepl("gasoline|gas|ethan|e10|e85|racing|avgas", sub_lower)),
    is_diesel   = as.integer(grepl("diesel|dsl|bio|kerosene", sub_lower)),
    is_oil_kerosene = as.integer(grepl("oil|lube|hydr|fluid", sub_lower)),
    is_jet_fuel = as.integer(grepl("jet|aviation|jp", sub_lower)),
    is_other    = 0
  )]
  dt[is_gasoline==0 & is_diesel==0 & is_oil_kerosene==0 & is_jet_fuel==0, is_other := 1]
  dt[, sub_lower := NULL]
  return(dt)
}

# 2. Load Data -----------------------------------------------------------------
message("Loading Tennessee Data...")

# A. Compartments (Tanks)
dt_tanks <- read_excel(file.path(tn_path, "ust_all-tn-compartments.xlsx"), col_types = "text") %>% 
  clean_names() %>% as.data.table()

# B. EnvSites (LUST)
dt_lust_state <- read_excel(file.path(tn_path, "ust_all-tn-environmental-sites.xlsx"), col_types = "text") %>% 
  clean_names() %>% as.data.table()

# C. EPA Facilities (For Lat/Long)
message("Loading EPA Facilities...")
if (file.exists(file.path(epa_path, "Facilities.csv"))) {
  dt_epa_fac <- fread(file.path(epa_path, "Facilities.csv"), colClasses = "character") %>% 
    clean_names() %>% as.data.table()
  
  # Filter for TN
  dt_geo <- dt_epa_fac[state == 'Tennessee', .(facility_id, latitude, longitude)]
  
  # CLEAN KEY: Strip "TN" (e.g. TN10021 -> 10021)
  dt_geo[, facility_id_clean := clean_ids(facility_id)]
  
  dt_geo <- dt_geo[, .(
    facility_id_clean,
    latitude = as.numeric(latitude),
    longitude = as.numeric(longitude)
  )]
  # Dedupe
  dt_geo <- dt_geo[!is.na(latitude) & !is.na(longitude)][!duplicated(facility_id_clean)]
} else {
  warning("Facilities.csv not found. Lat/Long will be NA.")
  dt_geo <- data.table(facility_id_clean=character(), latitude=numeric(), longitude=numeric())
}

# D. Zip Crosswalk (For County)
message("Loading Zip Crosswalk...")
if (file.exists(file.path(epa_path, "uszips.csv"))) {
  dt_zip <- fread(file.path(epa_path, "uszips.csv"), colClasses = "character") %>% clean_names()
  
  # Prepare Crosswalk: 5-digit Zip -> County Name
  dt_zip[, zip5 := str_sub(trimws(zip), 1, 5)]
  dt_zip <- dt_zip[, .(zip5, county_name)]
  dt_zip <- dt_zip[!duplicated(zip5)] # 1-to-1 mapping
} else {
  warning("uszips.csv not found. County names will be missing.")
  dt_zip <- data.table(zip5=character(), county_name=character())
}

# 3. Process Tanks -------------------------------------------------------------
message("Processing Tank Attributes...")

# IDs
dt_tanks[, `:=`(
  facility_id = clean_ids(facility_id_ust),
  tank_id = compartment_id,
  facility_name = facility_name
)]

# Dates
dt_tanks[, `:=`(
  tank_installed_date = parse_tn_tank_date(date_tank_installed),
  tank_closed_date    = parse_tn_tank_date(date_tank_closed)
)]
# Fallback logic: Use compartment close date if tank close date missing
dt_tanks[is.na(tank_closed_date), tank_closed_date := parse_tn_tank_date(date_compartment_closed)]

# Status Logic (Matches LA)
dt_tanks[, tank_status := fcase(
  grepl("Current|Temp", status, ignore.case=TRUE), "Open",
  grepl("Perm|Out of Use|Closed|Remov", status, ignore.case=TRUE), "Closed",
  default = "Closed"
)]
# Filter out "Temporary" if strict alignment with LA (LA removed them, keeping here for completeness or remove if desired)
# dt_tanks <- dt_tanks[tank_status != "Temporary"] 

# Classifications
dt_tanks <- classify_substance(dt_tanks)
dt_tanks <- classify_walls(dt_tanks)

# Capacity
dt_tanks[, capacity := as.numeric(gsub("[^0-9.]", "", compartment_capacity))]

# Extract Zip for Matching
dt_tanks[, zip5 := str_sub(trimws(facility_zip), 1, 5)]

# 4. Process LUST Data ---------------------------------------------------------
message("Processing LUST Data...")

dt_lust_clean <- dt_lust_state[, .(
  facility_id = clean_ids(facilityid),
  report_date = parse_tn_lust_date(discoverydate),
  nfa_date = as.Date(NA), # TN data lacks specific NFA dates
  lust_status = currentstatus
)]
dt_lust_clean <- dt_lust_clean[!is.na(facility_id) & facility_id != ""]

# 5. Merging -------------------------------------------------------------------
message("Merging Datasets...")

# A. Merge LUSTs (Left Join - One Facility can have multiple LUSTs)
tanks_lust <- merge(dt_tanks, dt_lust_clean, by = "facility_id", all.x = TRUE, allow.cartesian = TRUE)

# B. Calculate Leak Flags (Matches LA Logic)
tanks_lust[, `:=`(
  leak_after_closure = as.integer(!is.na(tank_closed_date) & !is.na(report_date) & report_date > tank_closed_date),
  # Without NFA dates, these flags default to 0
  leak_before_NFA_before_closure = 0, 
  leak_before_NFA_after_closure = 0,
  # No leak if no report found and tank is closed
  no_leak = as.integer(is.na(report_date) & !is.na(tank_closed_date))
)]

# 6. Aggregation ---------------------------------------------------------------
message("Aggregating to Unique Tank Level...")

final_df <- tanks_lust[, .(
  facility_name = first(facility_name),
  city = first(facility_city),
  zip_code = first(facility_zip),
  zip5 = first(zip5), # Keep for crosswalk
  
  tank_status = first(tank_status),
  tank_installed_date = first(tank_installed_date),
  tank_closed_date = first(tank_closed_date),
  capacity = mean(capacity, na.rm=TRUE),
  
  # Max flags for 1/0
  is_gasoline = max(is_gasoline, na.rm=TRUE),
  is_diesel = max(is_diesel, na.rm=TRUE),
  is_oil_kerosene = max(is_oil_kerosene, na.rm=TRUE),
  is_jet_fuel = max(is_jet_fuel, na.rm=TRUE),
  is_other = max(is_other, na.rm=TRUE),
  
  single_walled = max(single_walled, na.rm=TRUE),
  double_walled = max(double_walled, na.rm=TRUE),
  unknown_walled = max(unknown_walled, na.rm=TRUE),
  
  leak_after_closure = max(leak_after_closure, na.rm=TRUE),
  leak_before_NFA_before_closure = 0,
  leak_before_NFA_after_closure = 0,
  no_leak = min(no_leak, na.rm=TRUE)
), by = .(facility_id, tank_id)]

# 7. Final Geography Joins -----------------------------------------------------
message("Applying Geography (Lat/Long + County)...")

# A. Lat/Long from EPA (Left Join)
final_df <- merge(final_df, dt_geo, by.x = "facility_id", by.y = "facility_id_clean", all.x = TRUE)

# B. County from Zip Crosswalk (Left Join)
final_df <- merge(final_df, dt_zip, by = "zip5", all.x = TRUE)

# Final Cleanup
final_df[, state := state_abbr]
final_df[, street_address := NA_character_]
# Ensure correct order/selection of columns to match LA
cols_to_keep <- c(
  "facility_id", "facility_name", "tank_id", "state", "county_name",
  "tank_installed_date", "tank_closed_date", "tank_status",
  "leak_after_closure", "leak_before_NFA_before_closure", "leak_before_NFA_after_closure", "no_leak",
  "capacity", "single_walled", "double_walled", "unknown_walled",
  "is_gasoline", "is_diesel", "is_oil_kerosene", "is_jet_fuel", "is_other",
  "latitude", "longitude"
)

# Subset and fill any missing columns with NA if strict binding required
missing_cols <- setdiff(cols_to_keep, names(final_df))
if(length(missing_cols) > 0) final_df[, (missing_cols) := NA]

final_df <- final_df[, ..cols_to_keep]

# 8. Save ----------------------------------------------------------------------
outfile_tanks <- file.path(out_dir, paste0(state_abbr, "_Harmonized_UST_tanks.csv"))
outfile_lust  <- file.path(out_dir, paste0(state_abbr, "_Harmonized_LUST.csv"))

fwrite(final_df, outfile_tanks)
fwrite(dt_lust_clean, outfile_lust)

cat(paste0("\n------------------------------------------------\n"))
cat(paste0("TENNESSEE PROCESSING COMPLETE\n"))
cat(paste0("------------------------------------------------\n"))
cat(paste0("Output File:   ", outfile_tanks, "\n"))
cat(paste0("Total Tanks:   ", nrow(final_df), "\n"))
cat(paste0("Lat/Long:      ", round(mean(!is.na(final_df$latitude))*100,1), "% coverage\n"))
cat(paste0("County Names:  ", round(mean(!is.na(final_df$county_name))*100,1), "% coverage\n"))
cat(paste0("------------------------------------------------\n"))