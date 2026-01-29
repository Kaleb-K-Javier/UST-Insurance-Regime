## New Jersey UST Dataset Creation
# Script: 02_Clean_NJ.R
# Author: Validated Classification Logic (Updated Jan 2026)
# Description: Harmonizes NJ UST/LUST data.
# Key Features:
#   1. Robust Import: Uses colClasses="character" to prevent type inference errors.
#   2. Specific Logic: Uses explicit 'tank_structure' columns (Single Wall vs Double Wall).
#   3. Date Parsing: Handles "1/1/1944 12:00:00 AM" format.
#   4. Geography: Converts confirmed NJ State Plane (EPSG:3424) to Lat/Long (EPSG:4326).

# 0. Setup ----------------------------------------------------------------
COUNTYVARIABLE <- "county"

library(tidyverse)
library(data.table)
library(here)
library(lubridate)
library(janitor)
library(stringr)
library(sf) # Required for EPSG:3424 -> EPSG:4326 conversion

# 1. Helper Functions -----------------------------------------------------

standardize_county_name <- function(county_name) {
  name <- tolower(county_name)
  name <- gsub(" county$| counties$| parish$| parishes$", "", name)
  name <- gsub("[^a-z ]", "", name)
  name <- trimws(gsub("\\s+", " ", name))
  name[name == ""] <- NA
  return(name)
}

# 2. Classification Logic -------------------------------------------------

classify_tank_walls_NJ <- function(data) {
  result <- copy(data)
  
  # NJ has a dedicated 'tank_structure' column
  val_struct <- toupper(result$tank_structure)
  val_const <- toupper(result$tank_construction) # Fallback
  
  # 1. Double Walled
  result[, is_double := grepl("DOUBLE WALL|SECONDARY CONTAINMENT|TRIPLE WALL", val_struct) | 
           grepl("DOUBLE WALL|SECONDARY CONTAINMENT", val_const)]
  
  # 2. Single Walled
  result[, is_single := !is_double & (grepl("SINGLE WALL", val_struct) | 
                                      grepl("BARE STEEL|COATED STEEL|FIBERGLASS", val_const))]
  
  # 3. Assign Harmonized Classes
  result[, `:=`(
    double_walled = as.integer(is_double),
    single_walled = as.integer(is_single),
    unknown_walled = as.integer(!is_double & !is_single),
    missing_walled = as.integer(is.na(tank_structure) & is.na(tank_construction))
  )]
  
  result[, `:=`(is_double = NULL, is_single = NULL)]
  
  cat("\nClassification Summary:\n")
  print(result[, .(total=.N, single=sum(single_walled), double=sum(double_walled), unknown=sum(unknown_walled))])
  
  return(result)
}

classify_substances <- function(data, substance_col_name = "tank_contents") {
  result <- copy(data)
  result[, `:=`(is_gasoline=0, is_diesel=0, is_oil_kerosene=0, is_jet_fuel=0, is_other=1)]
  
  if(substance_col_name %in% names(result)) {
    result[, sub_lower := tolower(get(substance_col_name))]
    
    gas_pat <- "gasoline|petrol|unleaded|premium|regular|gasohol|ethanol|e85"
    dsl_pat <- "diesel|bio.?diesel"
    oil_pat <- "oil|kerosene|lube|waste|heating"
    jet_pat <- "jet|aviation|jp"
    
    result[grepl(gas_pat, sub_lower), is_gasoline := 1]
    result[grepl(dsl_pat, sub_lower) & !grepl("def", sub_lower), is_diesel := 1]
    result[grepl(oil_pat, sub_lower), is_oil_kerosene := 1]
    result[grepl(jet_pat, sub_lower), is_jet_fuel := 1]
    
    result[is_gasoline==1 | is_diesel==1 | is_oil_kerosene==1 | is_jet_fuel==1, is_other := 0]
    result[, sub_lower := NULL]
  }
  return(result)
}

# 3. Load Data ------------------------------------------------------------
cat("\nLoading New Jersey UST Data...\n")

# Use 'preferred_id' as key. 
# Force char to match between tables and avoid "column not found" warnings
NJ_tanks_raw <- fread(here("Data","Raw","state_databases","New Jersey","New Jersey Tanks Data.csv"), 
                      colClasses = "character") %>% 
  janitor::clean_names() %>% as.data.table()

NJ_facility_raw <- fread(here("Data","Raw","state_databases","New Jersey","New Jersey Facility Data.csv"), 
                         colClasses = "character") %>% 
  janitor::clean_names() %>% as.data.table()

cat("Loaded", nrow(NJ_tanks_raw), "tanks and", nrow(NJ_facility_raw), "facilities.\n")

# 4. Join & Process (Geospatial Conversion) -------------------------------
cat("\nJoining and Processing...\n")

# Join Facility Info (Name, County, Coords) to Tanks
NJ_UST_tanks <- merge(
  NJ_tanks_raw,
  NJ_facility_raw[, .(preferred_id, facility_name = pi_name, county_name = county, 
                      x_coord = x, y_coord = y)],
  by = "preferred_id",
  all.x = TRUE
)

# --- GEOSPATIAL CONVERSION (NJ State Plane -> WGS84) ---
cat("Converting State Plane coordinates to Lat/Long using sf...\n")

# 1. Prepare unique locations
# Convert chars to numeric for sf. 
locs <- unique(NJ_UST_tanks[!is.na(x_coord) & !is.na(y_coord) & x_coord != "" & y_coord != "", 
                            .(facility_id = preferred_id, x = as.numeric(x_coord), y = as.numeric(y_coord))])

if(nrow(locs) > 0) {
  # 2. Create SF object (EPSG:3424 = NAD83 / New Jersey ftUS)
  sf_locs <- st_as_sf(locs, coords = c("x", "y"), crs = 3424)
  
  # 3. Transform to WGS84 (EPSG:4326)
  sf_trans <- st_transform(sf_locs, 4326)
  
  # 4. Extract Lat/Long
  coords <- st_coordinates(sf_trans)
  locs[, `:=`(longitude = coords[,1], latitude = coords[,2])]
  
  # 5. Merge back to main tank data
  NJ_UST_tanks <- merge(NJ_UST_tanks, locs[, .(facility_id, latitude, longitude)], 
                        by.x="preferred_id", by.y="facility_id", all.x=TRUE)
} else {
  NJ_UST_tanks[, `:=`(latitude = NA_real_, longitude = NA_real_)]
}

# Rename Standard Columns
NJ_UST_tanks <- NJ_UST_tanks %>%
  rename(
    facility_id = preferred_id,
    tank_id = tank_number,
    tank_installed_date = installed_date,
    tank_closed_date = out_of_service_date,
    capacity = tank_volume,
    substance = tank_contents
  ) %>%
  as.data.table()

# Clean Dates (MDY HMS format: "1/1/1944 12:00:00 AM")
NJ_UST_tanks[, tank_installed_date := as.Date(lubridate::mdy_hms(tank_installed_date, quiet=TRUE))]
NJ_UST_tanks[, tank_closed_date := as.Date(lubridate::mdy_hms(tank_closed_date, quiet=TRUE))]

# Status Logic
NJ_UST_tanks[, tank_status := case_when(
  tolower(tank_status_description) == "in-use" ~ "Open",
  tolower(tank_status_description) %in% c("abandoned in place", "out of service", "removed") ~ "Closed",
  TRUE ~ "Ignore"
)]
NJ_UST_tanks <- NJ_UST_tanks[tank_status != "Ignore"]

# Clean Capacity
NJ_UST_tanks[, capacity := as.numeric(gsub("[^0-9.]", "", capacity))]

# Classify
NJ_UST_tanks <- classify_tank_walls_NJ(NJ_UST_tanks)
NJ_UST_tanks <- classify_substances(NJ_UST_tanks)

# 5. Process LUST Data ----------------------------------------------------
cat("\nProcessing New Jersey LUST Data...\n")

lust_path <- here("Data", "Raw", "Releases.csv")

if(file.exists(lust_path)) {
  NJ_lust_raw <- fread(lust_path, colClasses = "character") %>% 
    janitor::clean_names() %>%
    filter(state == "New Jersey") %>%
    as.data.table()
  
  NJ_LUST_SD <- NJ_lust_raw[, .(
    facility_id = gsub("^NJ", "", facility_id),
    LUST_id = lust_id,
    report_date = reported_date,
    nfa_date = NA_character_,
    state = "NJ"
  )]
  
  # Date Parsing
  NJ_LUST_SD[, report_date := as.Date(lubridate::mdy_hm(report_date, quiet=TRUE))]
  
  # 6. Merge & Aggregation --------------------------------------------------
  cat("\nMerging and Aggregating...\n")
  
  NJ_UST_tanks_LUST <- merge(NJ_UST_tanks, NJ_LUST_SD, by="facility_id", all.x=TRUE, allow.cartesian=TRUE)
  
  NJ_UST_tanks_LUST[, `:=`(
    leak_after_closure = as.integer(!is.na(tank_closed_date) & !is.na(report_date) & report_date > tank_closed_date),
    no_leak = as.integer(is.na(report_date) & !is.na(tank_closed_date))
  )]
  
  # Init Lat/Long if missing to ensure columns exist
  if (!"latitude" %in% names(NJ_UST_tanks_LUST)) NJ_UST_tanks_LUST[, latitude := NA_real_]
  if (!"longitude" %in% names(NJ_UST_tanks_LUST)) NJ_UST_tanks_LUST[, longitude := NA_real_]
  
  NJ_UST_tanks_SD <- NJ_UST_tanks_LUST[, .(
    facility_name = first(facility_name),
    tank_status = first(tank_status),
    leak_after_closure = max(leak_after_closure, na.rm=T),
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
  
  NJ_UST_tanks_SD[, state := "NJ"]
  
  # Final Column Selection
  required_columns <- c(
    "facility_id", "facility_name", "tank_id", "state", "tank_installed_date", "tank_closed_date", "tank_status",
    "leak_after_closure", "no_leak",
    "capacity", "single_walled", "double_walled", "unknown_walled",
    "is_gasoline", "is_diesel", "is_oil_kerosene", "is_jet_fuel", "is_other", "county_name",
    "latitude", "longitude"
  )
  cols_to_keep <- intersect(names(NJ_UST_tanks_SD), required_columns)
  NJ_UST_tanks_SD <- NJ_UST_tanks_SD[, ..cols_to_keep]
  
  # 7. Save ---------------------------------------------------------------
  cat("\nSaving Final Files...\n")
  output_dir <- here("Data", "Raw", "state_databases", "New Jersey")
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  
  fwrite(NJ_UST_tanks_SD, file.path(output_dir, "NJ_Harmonized_UST_tanks.csv"))
  fwrite(NJ_LUST_SD, file.path(output_dir, "NJ_Harmonized_LUST.csv"))
  
  cat("Done.\n")
  cat("Tank records:", nrow(NJ_UST_tanks_SD), "\n")
  
} else {
  warning("LUST file 'Releases.csv' not found.")
}