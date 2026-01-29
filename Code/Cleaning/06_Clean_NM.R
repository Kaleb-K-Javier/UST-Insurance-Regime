## New Mexico UST Dataset Creation
# Script: 02_Clean_NM.R
# Author: Validated Classification Logic (Updated Jan 2026)
# Description: Harmonizes NM UST/LUST data using EPA National file + State LUST files.

# 0. Setup ----------------------------------------------------------------
COUNTYVARIABLE <- "county"

library(tidyverse)
library(data.table)
library(here)
library(lubridate)
library(janitor)
library(readxl)
library(stringr)

# 1. Helper Functions -----------------------------------------------------

classify_substances <- function(data, substance_col_name = "substances") {
  result <- copy(data)
  result[, `:=`(is_gasoline = 0, is_diesel = 0, is_oil_kerosene = 0, is_jet_fuel = 0, is_other = 1)]
  
  if(substance_col_name %in% names(result)) {
    result[, sub_lower := tolower(get(substance_col_name))]
    
    gas_pat <- "gasoline|unleaded|petrol|mogas|ethanol|e85|alcohol enriched"
    dsl_pat <- "diesel|biodiesel"
    oil_pat <- "oil|kerosene|fluid|hydraulic"
    jet_pat <- "jet|aviation|avgas"
    
    result[grepl(gas_pat, sub_lower), is_gasoline := 1]
    result[grepl(dsl_pat, sub_lower) & !grepl("def", sub_lower), is_diesel := 1]
    result[grepl(oil_pat, sub_lower), is_oil_kerosene := 1]
    result[grepl(jet_pat, sub_lower), is_jet_fuel := 1]
    
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
    unknown_walled = as.integer(is.na(get(tank_wall_col)) | get(tank_wall_col) == "" | grepl("Unknown", get(tank_wall_col), ignore.case=TRUE)),
    missing_walled = as.integer(is.na(get(tank_wall_col)) | get(tank_wall_col) == "")
  )]
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

# 2. Load National Data (Permissive Strategy) -----------------------------
cat("\nLoading National EPA Data...\n")

# Load USTs - clean names immediately
ust_data_epa <- fread(here("Data","Raw","USTs.csv")) %>% 
  janitor::clean_names() %>% 
  as.data.table()

# Load Facilities - Only fetch ID, County, Lat, Long
facilities_path <- here("Data","Raw","Facilities.csv")
if(file.exists(facilities_path)) {
  epa_facilities <- fread(facilities_path) %>% 
    janitor::clean_names() %>% 
    as.data.table()
  
  # Join Facility Info (Strictly what you asked for)
  cat("  Joining Facility Locations...\n")
  cols_to_join <- c("facility_id", "county", "latitude", "longitude")
  cols_present <- intersect(names(epa_facilities), cols_to_join)
  
  ust_data_epa <- merge(ust_data_epa, 
                        epa_facilities[, ..cols_present], 
                        by = "facility_id", 
                        all.x = TRUE)
} else {
  warning("Facilities.csv not found. Geocoding will be missing.")
}

# 3. Filter for New Mexico ------------------------------------------------
cat("\nFiltering for New Mexico...\n")

# Determine state column name
state_col <- grep("^state$", names(ust_data_epa), value = TRUE)
if(length(state_col) == 0) state_col <- "facility_state"

NM_UST <- ust_data_epa[grepl('New Mexico|NM', get(state_col), ignore.case = TRUE)]

cat("NM Tanks filtered:", nrow(NM_UST), "\n")

# 4. Process Tank Data (ID Cleaning Step) ---------------------------------
cat("\nProcessing Tank Attributes...\n")

NM_UST_tanks <- NM_UST %>%
  select(
    facility_id, 
    tank_id, 
    installation_date, 
    removal_date, 
    tank_status, 
    county,
    substances, 
    tank_wall_type, 
    capacity,
    latitude,
    longitude
  ) %>%
  rename(
    tank_installed_date = installation_date,
    tank_closed_date = removal_date,
    county_name = county
  ) %>%
  mutate(
    # CRITICAL: Strip letters and make numeric to match LUST data
    facility_id = as.numeric(gsub("[A-Za-z]", "", facility_id)),
    tank_id = as.character(tank_id),
    state = "NM"
  ) %>%
  as.data.table()

# Clean Dates
NM_UST_tanks[, tank_installed_date := lubridate::parse_date_time(tank_installed_date, orders = c("ymd", "mdy", "mdy HMS"), quiet = TRUE)]
NM_UST_tanks[, tank_closed_date := lubridate::parse_date_time(tank_closed_date, orders = c("ymd", "mdy", "mdy HMS"), quiet = TRUE)]

NM_UST_tanks[, tank_installed_date := as.Date(tank_installed_date)]
NM_UST_tanks[, tank_closed_date := as.Date(tank_closed_date)]

# Harmonize Lat/Long
if("latitude" %in% names(NM_UST_tanks)) NM_UST_tanks[, latitude := as.numeric(latitude)] else NM_UST_tanks[, latitude := NA_real_]
if("longitude" %in% names(NM_UST_tanks)) NM_UST_tanks[, longitude := as.numeric(longitude)] else NM_UST_tanks[, longitude := NA_real_]

# Classify
NM_UST_tanks <- classify_substances(NM_UST_tanks, "substances")
NM_UST_tanks <- classify_tank_walls(NM_UST_tanks, "tank_wall_type")
NM_UST_tanks <- standardize_capacity(NM_UST_tanks, "capacity")

# 5. Load LUST Data (Targeted Files) --------------------------------------
cat("\nLoading New Mexico LUST Data...\n")

nm_path <- here("Data","Raw","state_databases","New Mexico")
NM_LUST_files <- list.files(nm_path, full.names = TRUE)

rp_file <- grep("rp_nfa_amount", NM_LUST_files, value = TRUE, ignore.case = TRUE)
sl_file <- grep("sl_nfa_amount", NM_LUST_files, value = TRUE, ignore.case = TRUE)

if(length(rp_file) > 0 && length(sl_file) > 0) {
  
  NM_LUST_01 <- read_excel(rp_file[1]) %>% clean_names() %>% as.data.table()
  NM_LUST_02 <- read_excel(sl_file[1]) %>% clean_names() %>% as.data.table()
  
  cols_keep <- c("rid", "fid", "release_date", "nfa_date", "total_wp", "total_paid")
  
  safe_select <- function(dt, cols) {
    valid <- intersect(names(dt), cols)
    dt[, ..valid]
  }
  
  l1 <- safe_select(NM_LUST_01, cols_keep)
  l2 <- safe_select(NM_LUST_02, cols_keep)
  
  NM_LUST <- rbindlist(list(l1, l2), fill=TRUE)
  NM_LUST <- NM_LUST[!is.na(rid) & !is.na(release_date)]
  
  NM_LUST_SD <- NM_LUST %>%
    rename(
      facility_id = fid, 
      LUST_id = rid, 
      report_date = release_date
    ) %>%
    mutate(
      # CRITICAL: Ensure Facility ID is numeric to match Tanks
      facility_id = as.numeric(facility_id),
      LUST_id = as.character(LUST_id),
      report_date = as.Date(report_date),
      nfa_date = as.Date(nfa_date),
      state = "NM"
    ) %>%
    as.data.table()
  
} else {
  warning("Critical LUST files (rp_nfa/sl_nfa) not found.")
  NM_LUST_SD <- data.table(facility_id=numeric(), LUST_id=character(), report_date=as.Date(character()), nfa_date=as.Date(character()), total_wp=numeric(), total_paid=numeric())
}

# 6. Join & Aggregation ---------------------------------------------------
cat("\nMerging and Aggregating...\n")

# Collapse LUSTs
state_leak_data_collapsed <- NM_LUST_SD[, .(
  LUST_count = .N,
  total_wp = sum(total_wp, na.rm = TRUE),
  total_paid = sum(total_paid, na.rm = TRUE)
), by = .(facility_id, report_date, nfa_date)]

# Numeric Join
NM_UST_tanks_LUST <- merge(NM_UST_tanks, state_leak_data_collapsed, by="facility_id", all.x=TRUE, allow.cartesian=TRUE)

NM_UST_tanks_LUST[, `:=`(
  leak_after_closure = as.integer(!is.na(tank_closed_date) & !is.na(report_date) & report_date > tank_closed_date),
  leak_before_NFA_before_closure = as.integer(!is.na(tank_closed_date) & !is.na(report_date) & report_date < tank_closed_date & (is.na(nfa_date) | nfa_date < tank_closed_date)),
  leak_before_NFA_after_closure = as.integer(!is.na(tank_closed_date) & !is.na(report_date) & report_date < tank_closed_date & !is.na(nfa_date) & nfa_date > tank_closed_date),
  no_leak = as.integer(is.na(report_date) & !is.na(tank_closed_date))
)]

# Final Aggregate
NM_UST_tanks_SD <- NM_UST_tanks_LUST[, .(
  # Note: facility_name missing from raw select, handled gracefully if needed
  tank_status = first(tank_status),
  
  leak_after_closure = max(leak_after_closure, na.rm=T),
  leak_before_NFA_before_closure = max(leak_before_NFA_before_closure, na.rm=T),
  leak_before_NFA_after_closure = max(leak_before_NFA_after_closure, na.rm=T),
  no_leak = max(no_leak, na.rm=T),
  
  single_walled = max(single_walled, na.rm=T),
  double_walled = max(double_walled, na.rm=T),
  unknown_walled = max(unknown_walled, missing_walled, na.rm=T),
  
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
NM_UST_tanks_SD[, facility_id := as.character(facility_id)] # Return to char for final CSV

# 7. Output ---------------------------------------------------------------
cat("\nSaving Final Files...\n")
if (!dir.exists(nm_path)) dir.create(nm_path, recursive = TRUE)

fwrite(NM_UST_tanks_SD, file.path(nm_path, "NM_Harmonized_UST_tanks.csv"))
fwrite(NM_LUST_SD, file.path(nm_path, "NM_Harmonized_LUST.csv"))

cat("Done.\n")
cat("Tank records:", nrow(NM_UST_tanks_SD), "\n")
cat("LUST records:", nrow(NM_LUST_SD), "\n")