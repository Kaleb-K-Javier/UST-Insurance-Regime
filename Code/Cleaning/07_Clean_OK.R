## Oklahoma UST Dataset Creation
# Script: 02_Clean_OK.R
# Author: Validated Classification Logic (Updated Jan 2026)
# Description: Harmonizes OK UST/LUST data.
# Key Features:
#   1. Robust Import: Dynamically finds Tank vs Release files.
#   2. ID Cleaning: Removes brackets '[]' from facility IDs.
#   3. Geocoding: Preserves native Latitude/Longitude columns.
#   4. Harmonization: Enforces strict column naming conventions.

# 0. Setup ----------------------------------------------------------------
COUNTYVARIABLE <- "county"

library(tidyverse)
library(data.table)
library(here)
library(lubridate)
library(janitor)
library(stringr)

# 1. Helper Functions -----------------------------------------------------

classify_substances <- function(data, substance_col_name = "tank_substance") {
  result <- copy(data)
  result[, `:=`(is_gasoline=0, is_diesel=0, is_oil_kerosene=0, is_jet_fuel=0, is_other=1)]
  
  if(substance_col_name %in% names(result)) {
    result[, sub_lower := tolower(get(substance_col_name))]
    
    gas_pat <- "gasoline|unleaded|petrol|ethanol|e85|e10|e15|e-10|e-85"
    dsl_pat <- "diesel|biodiesel|dyed diesel"
    oil_pat <- "oil|kerosene|used oil|waste oil|lube|hydraulic"
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

classify_tank_walls_OK <- function(data, tank_wall_col = "tank_construction") {
  result <- copy(data)
  
  vals <- toupper(result[[tank_wall_col]])
  
  # 1. Double Walled (Explicit in OK data)
  result[, is_double := grepl("DOUBLE WALLED|DOUBLE-WALLED|SECONDARILY CONTAINED|JACKETED", vals)]
  
  # 2. Single Walled (Explicit in OK data)
  result[, is_single := grepl("SINGLE WALLED|SINGLE-WALLED", vals)]
  
  # 3. Assign Harmonized Classes
  result[, `:=`(
    double_walled = as.integer(is_double),
    single_walled = as.integer(is_single),
    unknown_walled = as.integer(!is_double & !is_single),
    missing_walled = as.integer(is.na(vals) | vals == "")
  )]
  
  result[, `:=`(is_double = NULL, is_single = NULL)]
  return(result)
}

# 2. Load Data ------------------------------------------------------------
cat("\nLoading Oklahoma UST Data...\n")

ok_path <- here("Data","Raw","state_databases","Oklahoma")
ok_files <- list.files(ok_path, pattern = "\\.csv$", full.names = TRUE)

tank_file <- grep("Tank|PST_Tank", ok_files, value = TRUE, ignore.case = TRUE)
lust_file <- grep("Case|Release|PST_Case", ok_files, value = TRUE, ignore.case = TRUE)

if(length(tank_file) == 0 || length(lust_file) == 0) stop("Could not find Tank or Case CSV files in Oklahoma folder.")

# Permissive Load
OK_Tanks_Raw <- fread(tank_file[1], colClasses = "character") %>% 
  janitor::clean_names() %>% 
  as.data.table()

OK_LUST_Raw <- fread(lust_file[1], colClasses = "character") %>% 
  janitor::clean_names() %>% 
  as.data.table()

cat("Loaded", nrow(OK_Tanks_Raw), "tanks and", nrow(OK_LUST_Raw), "cases.\n")

# 3. Process Tank Data ----------------------------------------------------
cat("\nProcessing Tank Attributes...\n")

OK_UST_tanks <- OK_Tanks_Raw[toupper(tank_type) == "UST"]

OK_UST_tanks <- OK_UST_tanks[, .(
  facility_id = facility_number,
  facility_name = facility_name,
  tank_id = tank_number,
  tank_installed_date,
  tank_closed_date,
  tank_status,
  tank_substance,
  tank_construction,
  tank_capacity,
  latitude,
  longitude,
  facility_city
)]

# Clean IDs: Remove brackets [12345] -> 12345
OK_UST_tanks[, facility_id := gsub("\\[|\\]", "", facility_id)]
OK_UST_tanks[, tank_id := gsub("\\[|\\]", "", tank_id)]

# Dates
OK_UST_tanks[, tank_installed_date := lubridate::mdy(tank_installed_date, quiet = TRUE)]
OK_UST_tanks[, tank_closed_date := lubridate::mdy(tank_closed_date, quiet = TRUE)]

# Status Logic (CIU=Open, POU=Closed)
OK_UST_tanks[, tank_status_clean := case_when(
  tank_status == "CIU" ~ "Open",
  tank_status == "POU" ~ "Closed",
  tank_status == "TOU" ~ "Temporary",
  TRUE ~ "Closed"
)]

# Clean Capacity & Coords
OK_UST_tanks[, capacity := as.numeric(gsub("[^0-9.]", "", tank_capacity))]
OK_UST_tanks[, latitude := as.numeric(latitude)]
OK_UST_tanks[, longitude := as.numeric(longitude)]

# Classify
OK_UST_tanks <- classify_tank_walls_OK(OK_UST_tanks)
OK_UST_tanks <- classify_substances(OK_UST_tanks)

# 4. Process LUST Data ----------------------------------------------------
cat("\nProcessing LUST Data...\n")

OK_LUST_SD <- OK_LUST_Raw[, .(
  facility_id = facility_number,
  LUST_id = case_number,
  report_date = release_date,
  nfa_date = close_date,
  state = "OK"
)]

OK_LUST_SD[, facility_id := gsub("\\[|\\]", "", facility_id)]
OK_LUST_SD[, LUST_id := gsub("\\[|\\]", "", LUST_id)]
OK_LUST_SD[, report_date := lubridate::mdy(report_date, quiet = TRUE)]
OK_LUST_SD[, nfa_date := lubridate::mdy(nfa_date, quiet = TRUE)]

# 5. Merge & Aggregation --------------------------------------------------
cat("\nMerging and Aggregating...\n")

OK_UST_tanks_LUST <- merge(OK_UST_tanks, OK_LUST_SD, by="facility_id", all.x=TRUE, allow.cartesian=TRUE)

OK_UST_tanks_LUST[, `:=`(
  leak_after_closure = as.integer(!is.na(tank_closed_date) & !is.na(report_date) & report_date > tank_closed_date),
  leak_before_NFA_before_closure = as.integer(!is.na(tank_closed_date) & !is.na(report_date) & report_date < tank_closed_date & (is.na(nfa_date) | nfa_date < tank_closed_date)),
  leak_before_NFA_after_closure = as.integer(!is.na(tank_closed_date) & !is.na(report_date) & report_date < tank_closed_date & !is.na(nfa_date) & nfa_date > tank_closed_date),
  no_leak = as.integer(is.na(report_date) & !is.na(tank_closed_date))
)]

# Init County if missing (Using City as proxy placeholder if County not in raw)
if(!"county_name" %in% names(OK_UST_tanks_LUST)) OK_UST_tanks_LUST[, county_name := facility_city]

OK_UST_tanks_SD <- OK_UST_tanks_LUST[, .(
  facility_name = first(facility_name),
  tank_status = first(tank_status_clean),
  
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

OK_UST_tanks_SD[, state := "OK"]

# --- HARMONIZATION: Rename & Select Columns ---
# 1. Define the Strict List
required_columns <- c(
  "facility_id", "facility_name", "tank_id", "state", "tank_installed_date", "tank_closed_date", "tank_status",
  "leak_after_closure", "leak_before_NFA_before_closure", "leak_before_NFA_after_closure", "no_leak",
  "capacity", "single_walled", "double_walled", "unknown_walled",
  "is_gasoline", "is_diesel", "is_oil_kerosene", "is_jet_fuel", "is_other", "county_name"
)

# 2. Append Lat/Long (Essential for Geocoding, even if not in original list)
final_columns <- c(required_columns, "latitude", "longitude")

# 3. Filter the Dataset
# intersect ensures we don't crash if a column is missing (though our logic guarantees them)
cols_to_keep <- intersect(names(OK_UST_tanks_SD), final_columns)
OK_UST_tanks_SD <- OK_UST_tanks_SD[, ..cols_to_keep]

# 6. Output ---------------------------------------------------------------
cat("\nSaving Final Files...\n")
if (!dir.exists(ok_path)) dir.create(ok_path, recursive = TRUE)

fwrite(OK_UST_tanks_SD, file.path(ok_path, "OK_Harmonized_UST_tanks.csv"))
fwrite(OK_LUST_SD, file.path(ok_path, "OK_Harmonized_LUST.csv"))

cat("Done.\n")
cat("Tank records:", nrow(OK_UST_tanks_SD), "\n")
cat("LUST records:", nrow(OK_LUST_SD), "\n")