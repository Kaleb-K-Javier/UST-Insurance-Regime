## New Mexico UST Dataset Creation
# Script: 02_Clean_NM.R
# Author: Validated Classification Logic (Updated Jan 2026)
# Description: Harmonizes NM UST/LUST data using EPA National file + State LUST files.
#              Fixes date parsing for Millisecond-based timestamps.

# 0. Setup ----------------------------------------------------------------
library(tidyverse)
library(data.table)
library(here)
library(lubridate)
library(janitor)
library(readxl)
library(stringr)

# 1. Helper Functions -----------------------------------------------------

# --- ROBUST EPA DATE PARSER (Handles Negative Pre-1970 Timestamps) ---
parse_epa_date <- function(x) {
  # 1. Convert to numeric (R automatically handles "1.23e+12" strings)
  num_val <- suppressWarnings(as.numeric(x))
  
  # Initialize output
  d_out <- as.Date(rep(NA, length(x)))
  
  # 2. Case A: Unix Milliseconds (Positive OR Negative)
  #    The diagnostic showed dates like -1.92e12 (approx 1909) and 1.5e12 (2018).
  #    We use abs() > 1e10 to safely capture everything outside the "seconds" or "Excel" range.
  #    1e10 ms = ~4 months (April 1970 or Sept 1969).
  is_millis <- !is.na(num_val) & abs(num_val) > 1e10
  
  if (any(is_millis)) {
    # Divide by 1000 to get seconds. POSIXct correctly handles negative seconds for pre-1970.
    d_out[is_millis] <- as.Date(as.POSIXct(num_val[is_millis] / 1000, origin = "1970-01-01"))
  }
  
  # 3. Case B: Excel Serial Days (Fallback)
  #    Your diagnostic showed 0 records of this type, but we keep it as a safety net.
  #    Range: 100 to 100,000 (Years 1900 to 2173)
  is_serial <- !is.na(num_val) & !is_millis & num_val > 100 & num_val < 100000
  
  if (any(is_serial)) {
    d_out[is_serial] <- as.Date(num_val[is_serial], origin = "1899-12-30")
  }
  
  # 4. Case C: Text Formats (Final Fallback)
  #    Your diagnostic showed 0 records, but good for robustness.
  needs_text <- is.na(d_out) & !is.na(x) & x != ""
  if (any(needs_text)) {
    d_parsed <- lubridate::parse_date_time(x[needs_text], 
                                           orders = c("ymd", "mdy", "Y-m-d", "m/d/Y", "dmy"), 
                                           quiet = TRUE)
    d_out[needs_text] <- as.Date(d_parsed)
  }
  
  return(d_out)
}

classify_substances <- function(data, substance_col_name = "substances") {
  result <- copy(data)
  result[, `:=`(is_gasoline = 0, is_diesel = 0, is_oil_kerosene = 0, is_jet_fuel = 0, is_other = 1)]
  
  if(substance_col_name %in% names(result)) {
    result[, sub_lower := tolower(get(substance_col_name))]
    result[grepl("gasoline|unleaded|petrol|mogas|ethanol|e85", sub_lower), is_gasoline := 1]
    result[grepl("diesel|biodiesel", sub_lower) & !grepl("def", sub_lower), is_diesel := 1]
    result[grepl("oil|kerosene|fluid|hydraulic", sub_lower), is_oil_kerosene := 1]
    result[grepl("jet|aviation|avgas", sub_lower), is_jet_fuel := 1]
    result[is_gasoline==1 | is_diesel==1 | is_oil_kerosene==1 | is_jet_fuel==1, is_other := 0]
    result[, sub_lower := NULL]
  }
  return(result)
}

classify_tank_walls <- function(data, tank_wall_col = "tank_wall_type") {
  result <- copy(data)
  if(!tank_wall_col %in% names(result)) {
    result[, `:=`(double_walled=0, single_walled=0, unknown_walled=1, missing_walled=1)]
    return(result)
  }
  result[, `:=`(
    double_walled = as.integer(grepl("Double Walled", get(tank_wall_col), ignore.case = TRUE)),
    single_walled = as.integer(grepl("Single Walled", get(tank_wall_col), ignore.case = TRUE)),
    unknown_walled = as.integer(grepl("Unknown", get(tank_wall_col), ignore.case=TRUE)),
    missing_walled = as.integer(is.na(get(tank_wall_col)) | get(tank_wall_col) == "")
  )]
  # Consolidate missing into unknown
  result[, unknown_walled := pmax(unknown_walled, missing_walled, na.rm = TRUE)]
  return(result)
}

standardize_capacity <- function(data, capacity_col = "capacity") {
  result <- copy(data)
  if(capacity_col %in% names(result)) {
    result[, `:=`(capacity = as.numeric(gsub("[^0-9.]", "", get(capacity_col))))]
  } else {
    result[, capacity := NA_real_]
  }
  return(result)
}

# 2. Load National Data ---------------------------------------------------
cat("\nLoading National EPA Data...\n")

ust_data_epa <- fread(here("Data","Raw","USTs.csv")) %>% janitor::clean_names() %>% as.data.table()
facilities_path <- here("Data","Raw","Facilities.csv")

if(file.exists(facilities_path)) {
  epa_facilities <- fread(facilities_path) %>% janitor::clean_names() %>% as.data.table()
  
  cat("  Joining Facility Locations...\n")
  cols_to_join <- c("facility_id", "name", "county", "latitude", "longitude")
  cols_present <- intersect(names(epa_facilities), cols_to_join)
  
  ust_data_epa <- merge(ust_data_epa, epa_facilities[, ..cols_present], by = "facility_id", all.x = TRUE)
} else {
  warning("Facilities.csv not found.")
}

# 3. Filter for New Mexico ------------------------------------------------
cat("\nFiltering for New Mexico...\n")
state_col <- grep("^state$", names(ust_data_epa), value = TRUE)
if(length(state_col) == 0) state_col <- "facility_state"
NM_UST <- ust_data_epa[grepl('New Mexico|NM', get(state_col), ignore.case = TRUE)]

# 4. Process Tank Data ----------------------------------------------------
cat("\nProcessing Tank Attributes...\n")

NM_UST_tanks <- NM_UST %>%
  select(facility_id, name, tank_id, installation_date, removal_date, 
         tank_status, county, substances, tank_wall_type, capacity, latitude, longitude) %>%
  rename(
    facility_name = name,
    tank_installed_date = installation_date,
    tank_closed_date = removal_date,
    county_name = county
  ) %>%
  mutate(
    facility_id = as.numeric(gsub("[A-Za-z]", "", facility_id)),
    tank_id = as.character(tank_id),
    state = "NM"
  ) %>%
  as.data.table()

# Clean Dates using Robust Parser (MS Fix)
NM_UST_tanks[, tank_installed_date := parse_epa_date(tank_installed_date)]
NM_UST_tanks[, tank_closed_date := parse_epa_date(tank_closed_date)]

# Harmonize Lat/Long
NM_UST_tanks[, latitude := as.numeric(latitude)]
NM_UST_tanks[, longitude := as.numeric(longitude)]

# Classify
NM_UST_tanks <- classify_substances(NM_UST_tanks, "substances")
NM_UST_tanks <- classify_tank_walls(NM_UST_tanks, "tank_wall_type")
NM_UST_tanks <- standardize_capacity(NM_UST_tanks, "capacity")

# 5. Load LUST Data -------------------------------------------------------
cat("\nLoading New Mexico LUST Data...\n")
nm_path <- here("Data","Raw","state_databases","New Mexico")
NM_LUST_files <- list.files(nm_path, full.names = TRUE)
rp_file <- grep("rp_nfa_amount", NM_LUST_files, value = TRUE, ignore.case = TRUE)
sl_file <- grep("sl_nfa_amount", NM_LUST_files, value = TRUE, ignore.case = TRUE)

if(length(rp_file) > 0 && length(sl_file) > 0) {
  NM_LUST_01 <- read_excel(rp_file[1]) %>% clean_names() %>% as.data.table()
  NM_LUST_02 <- read_excel(sl_file[1]) %>% clean_names() %>% as.data.table()
  
  cols_keep <- c("rid", "fid", "release_date", "nfa_date", "total_wp", "total_paid")
  safe_select <- function(dt, cols) { valid <- intersect(names(dt), cols); dt[, ..valid] }
  
  NM_LUST <- rbindlist(list(safe_select(NM_LUST_01, cols_keep), safe_select(NM_LUST_02, cols_keep)), fill=TRUE)
  
  NM_LUST_SD <- NM_LUST %>%
    rename(facility_id = fid, LUST_id = rid, report_date = release_date) %>%
    mutate(
      facility_id = as.numeric(facility_id),
      LUST_id = as.character(LUST_id),
      report_date = as.Date(report_date),
      nfa_date = as.Date(nfa_date),
      state = "NM"
    ) %>% as.data.table()
} else {
  NM_LUST_SD <- data.table(facility_id=numeric(), LUST_id=character(), report_date=as.Date(character()), nfa_date=as.Date(character()))
}

# 6. Join & Aggregation ---------------------------------------------------
cat("\nMerging and Aggregating...\n")
state_leak_data_collapsed <- NM_LUST_SD[, .(LUST_count = .N), by = .(facility_id, report_date, nfa_date)]
NM_UST_tanks_LUST <- merge(NM_UST_tanks, state_leak_data_collapsed, by="facility_id", all.x=TRUE, allow.cartesian=TRUE)

NM_UST_tanks_LUST[, `:=`(
  leak_after_closure = as.integer(!is.na(tank_closed_date) & !is.na(report_date) & report_date > tank_closed_date),
  no_leak = as.integer(is.na(report_date) & !is.na(tank_closed_date))
)]

# Final Aggregate
NM_UST_tanks_SD <- NM_UST_tanks_LUST[, .(
  facility_name = first(facility_name),
  tank_status = first(tank_status),
  leak_after_closure = max(leak_after_closure, na.rm=T),
  no_leak = max(no_leak, na.rm=T),
  single_walled = max(single_walled, na.rm=T),
  double_walled = max(double_walled, na.rm=T),
  unknown_walled = max(unknown_walled, na.rm=T),
  is_gasoline = max(is_gasoline, na.rm=T),
  is_diesel = max(is_diesel, na.rm=T),
  is_oil_kerosene = max(is_oil_kerosene, na.rm=T),
  is_jet_fuel = max(is_jet_fuel, na.rm=T),
  is_other = max(is_other, na.rm=T),
  capacity = mean(capacity, na.rm=T),
  latitude = first(latitude),
  longitude = first(longitude)
), by = .(facility_id, tank_id, tank_installed_date, tank_closed_date, county_name)]

NM_UST_tanks_SD[, state := "NM"]
NM_UST_tanks_SD[, facility_id := as.character(facility_id)]

# 7. Output ---------------------------------------------------------------
if (!dir.exists(nm_path)) dir.create(nm_path, recursive = TRUE)
fwrite(NM_UST_tanks_SD, file.path(nm_path, "NM_Harmonized_UST_tanks.csv"))
fwrite(NM_LUST_SD, file.path(nm_path, "NM_Harmonized_LUST.csv"))
cat("Done.\n")
