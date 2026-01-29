## Louisiana UST Dataset Creation
# Script: 02_Clean_LA.R
# Author: Validated Classification Logic (Updated Jan 2026)
# Description: Harmonizes LA UST/LUST data for econometrics.
#
# Key Features:
#   1. Robust Import: Forces text import to capture "Y" flags in sparse columns.
#   2. Regulatory Mandate (Double): Implements "Dec 20, 2008" cutoff (LAC 33:XI.303.C.1).
#   3. Upgrade Logic (Single): Uses 'initial_cp_upgrade_date' as proof of Single-Walled status (40 CFR 280.21).
#   4. Expanded Definitions: Poly Jackets & Excavation Liners = Double Wall (EPA Def).
#   5. Robustness: Uses vectorized operations to prevent crashes on large datasets.

# 0. Setup ----------------------------------------------------------------
COUNTYVARIABLE <- "PARISH_DESC"

library(tidyverse)
library(data.table)
library(here)
library(lubridate)
library(janitor)
library(readxl)
library(stringr)

# 1. Helper Functions -----------------------------------------------------

# Vectorized function to standardize parish/county names
standardize_county_name <- function(county_name) {
  # tolower() is already vectorized
  name <- tolower(county_name)
  
  # gsub() is also vectorized
  name <- gsub(" county$| counties$| parish$| parishes$", "", name)
  name <- gsub("[^a-z ]", "", name)
  name <- trimws(gsub("\\s+", " ", name))
  
  # Vectorized way to handle empty strings
  name[name == ""] <- NA
  
  return(name)
}

# Robust Date Parser for Mixed Excel/Text formats
safe_parse_date <- function(date_vec) {
  # 1. Try parsing as numeric Excel serial date
  suppressWarnings({
    numeric_dates <- as.numeric(date_vec)
    parsed_dates <- janitor::excel_numeric_to_date(numeric_dates)
  })
  
  # 2. Identify failures (values that were not NA originally, but failed numeric parse)
  needs_string_parse <- is.na(parsed_dates) & !is.na(date_vec)
  
  # 3. Try string parsing for the remainder
  if(any(needs_string_parse)) {
    string_dates <- lubridate::parse_date_time(
      date_vec[needs_string_parse], 
      orders = c("ymd", "mdy", "dmy", "Y-m-d", "m/d/Y")
    )
    parsed_dates[needs_string_parse] <- as.Date(string_dates)
  }
  
  return(parsed_dates)
}

# 2. Core Logic: Classification -------------------------------------------

# Function to classify tank wall types (Robust Version with Upgrade Logic)
classify_tank_walls <- function(data) {
  result <- copy(data)
  
  # --- REGULATORY CONSTANTS ---
  # Citation: LAC 33:XI.303.C.1
  # "All new UST systems installed after December 20, 2008, shall have secondary containment."
  LA_SECONDARY_CONTAINMENT_DATE <- as.Date("2008-12-20") 
  
  # --- DEFINITION OF COLUMN GROUPS ---
  
  # Group A: Secondary Containment Indicators (Double Walled)
  # Citation: 40 CFR 280.20(d) & LAC 33:XI.303.C
  # - mct_poly_jacket: Meets EPA def of secondary containment (jacket creates interstitial space).
  # - mct_excavat_liner: Functional secondary containment if monitored.
  cols_double <- c("mct_double_walled", "mct_poly_jacket", "mct_excavat_liner")
  cols_double <- intersect(cols_double, names(result))
  
  # Group B: Single Wall / Material Indicators
  # Citation: 40 CFR 280.20(a) & (b)
  cols_materials <- c(
    "mct_asphalt_steel", "mct_fibrgls_plastic", "mct_comp_stl_fbrgls", 
    "mct_cathod_prot_stl", "mct_epox_coat_stl", "mct_lined_interior",
    "mct_stlasphalt_wocp", "mct_stlasphalt_nocp", "mct_cpsteel_ic", 
    "mct_cpsteel_sac_anode", "mct_cpsteel_int_line"
  )
  cols_materials <- intersect(cols_materials, names(result))
  
  if(length(cols_double) == 0 && length(cols_materials) == 0) {
    warning("Critical Warning: No construction columns found!")
    result[, `:=`(double_walled=0, single_walled=0, unknown_walled=1, missing_walled=1)]
    return(result)
  }

  # --- HELPER FUNCTION: Safely Check "Any Y" ---
  check_any_y <- function(dt, cols) {
    if(length(cols) == 0) return(rep(FALSE, nrow(dt)))
    # Reduce/lapply is safer than rowSums for data.table columns
    Reduce("|", lapply(dt[, ..cols], function(x) x %in% c("Y", "y", "Yes")))
  }

  # --- CALCULATE FLAGS ---
  
  # Flag A: Explicit Secondary Containment
  result[, explicit_double := check_any_y(result, cols_double)]
  
  # Flag B: Regulatory Mandate (The "2008 Rule")
  # Citation: LAC 33:XI.303.C.1
  result[, regulatory_double := !is.na(tank_installed_date) & tank_installed_date > LA_SECONDARY_CONTAINMENT_DATE]
  
  # Flag C: Material Presence (Evidence of Single Wall)
  # This now includes specific material flags OR the presence of an upgrade date
  result[, has_material_flag := check_any_y(result, cols_materials)]
  
  # Flag D: CP Upgrade Logic (The "Single Wall" Proof)
  # Citation: 40 CFR 280.21 - Upgrades apply to existing Single-Walled tanks.
  # If a tank has a CP upgrade date, it MUST be single-walled (originally).
  result[, has_upgrade_evidence := !is.na(initial_cp_upgrade_date)]
  
  # Combined Single Wall Evidence
  result[, evidence_of_existence := has_material_flag | has_upgrade_evidence]

  # --- HIERARCHICAL ASSIGNMENT ---
  result[, `:=`(
    # Priority 1: Double/Secondary if Explicitly marked OR Regulatory mandate
    # (Overrides upgrade evidence because a tank could be replaced post-2008)
    double_walled = as.integer(explicit_double | regulatory_double),
    
    # Priority 2: Single if NOT Double AND we have explicit Material OR Upgrade Data
    single_walled = as.integer(!(explicit_double | regulatory_double) & evidence_of_existence),
    
    # Priority 3: Unknown if neither
    unknown_walled = as.integer(!(explicit_double | regulatory_double) & !evidence_of_existence),
    missing_walled = as.integer(!(explicit_double | regulatory_double) & !evidence_of_existence)
  )]
  
  # Clean up temp cols
  result[, `:=`(explicit_double=NULL, regulatory_double=NULL, has_material_flag=NULL, has_upgrade_evidence=NULL, evidence_of_existence=NULL)]
  
  cat("\nTank wall classification summary:\n")
  print(result[, .(total=.N, single=sum(single_walled), double=sum(double_walled), unknown=sum(unknown_walled))])
  
  return(result)
}

# Function to classify substances
classify_substances <- function(data, substance_col_name = "substance") {
  result <- copy(data)
  result[, `:=`(is_gasoline=0, is_diesel=0, is_oil_kerosene=0, is_jet_fuel=0, is_other=1)]
  
  if(substance_col_name %in% names(result)) {
    result[, sub_lower := tolower(get(substance_col_name))]
    
    # Detailed regex patterns
    gas_pat <- paste(c("gas", "gasolin", "petrol", "motor fuel", "fuel oil", "e10", "e15", "e85", "e\\d+", "unleaded", "premium", "regular", "petroleum", "fuel", "conv[[:alpha:]]+", "mid", "plus", "supreme", "super", "octane", "87", "89", "91", "93", "\\bmtbe\\b"), collapse = "|")
    dsl_pat <- paste(c("diesel", "dsl", "dl$", "\\bd\\b", "bio.?diesel", "red diesel", "off.?road diesel", "^d ", "\\bdl\\b", "ulsd", "\\bd[0-9]\\b"), collapse = "|")
    oil_pat <- paste(c("oil", "kerosene", "kerosine", "heating oil", "motor oil", "used oil", "waste oil", "lubricant", "lube", "heating", "hydraulic", "transmission", "crude", "heavy", "mineral", "vegetable", "\\bcrank"), collapse = "|")
    jet_pat <- paste(c("jet", "aviation", "av gas", "avgas", "jp.?[45678]", "aircraft", "airplane"), collapse = "|")
    
    # Apply patterns
    result[!is.na(sub_lower), is_gasoline := as.integer(grepl(gas_pat, sub_lower, perl=TRUE))]
    result[!is.na(sub_lower) & !grepl("def|diesel exhaust fluid", sub_lower, perl=TRUE), is_diesel := as.integer(grepl(dsl_pat, sub_lower, perl=TRUE))]
    result[!is.na(sub_lower), is_oil_kerosene := as.integer(grepl(oil_pat, sub_lower, perl=TRUE))]
    result[!is.na(sub_lower), is_jet_fuel := as.integer(grepl(jet_pat, sub_lower, perl=TRUE))]
    
    # Handle "Other"
    result[, is_other := as.integer(is_gasoline + is_diesel + is_oil_kerosene + is_jet_fuel == 0)]
    result[, sub_lower := NULL]
  }
  
  cat("\nSubstance classification summary:\n")
  print(result[, .(total=.N, gas=sum(is_gasoline), diesel=sum(is_diesel), oil=sum(is_oil_kerosene), jet=sum(is_jet_fuel), other=sum(is_other))])
  
  return(result)
}

# 3. Load Raw Data (CRITICAL: Force Text Import) --------------------------
cat("\nLoading Louisiana UST and LUST data (Forcing Text Import)...\n")
la_file_path <- here("Data", "Raw", "state_databases", "Louisiana", "LA_record_request.xlsx")

if (!file.exists(la_file_path)) stop("File not found! Check path.")

# FIX: col_types = "text" prevents R from dropping "Y" flags in sparse columns.
LA_tanks_raw <- read_excel(la_file_path, sheet = "Tank Info by compartment", col_types = "text") %>%
  janitor::clean_names() %>%
  as.data.table()

LA_lust_raw <- read_excel(la_file_path, sheet = "LUST Sites by incident ID", col_types = "text") %>%
  janitor::clean_names() %>%
  as.data.table()

cat("Loaded", nrow(LA_tanks_raw), "tanks and", nrow(LA_lust_raw), "LUSTs.\n")

# 4. Process Tank Data ----------------------------------------------------
cat("\nProcessing tanks...\n")

# Select columns, including 'initial_cp_upgrade_date'
LA_UST_tanks <- LA_tanks_raw %>%
  select(
    master_ai_id, master_ai_name, parish_desc, subject_item_id,
    tank_capacity, install_date, tank_substance_code,
    current_status_code, current_status_desc, current_status_start_date,
    initial_cp_upgrade_date, # ADDED: Critical for Single Wall ID
    x_coord_standard_value, # ADDED: Longitude
    y_coord_standard_value, # ADDED: Latitude
    starts_with("mct_") 
  ) %>%
  rename(
    facility_id = master_ai_id,
    facility_name = master_ai_name,
    county_name = parish_desc,
    tank_id = subject_item_id,
    tank_installed_date = install_date,
    substance = tank_substance_code,
    status_code = current_status_code,
    status_desc = current_status_desc,
    tank_status_date = current_status_start_date,
    longitude = x_coord_standard_value, # Harmonized Name
    latitude = y_coord_standard_value   # Harmonized Name
  ) %>%
  as.data.table()

# Clean Capacity
LA_UST_tanks[, capacity := as.numeric(gsub("[^0-9.]", "", tank_capacity))]
LA_UST_tanks[, latitude := as.numeric(latitude)]   # Clean Text to Number
LA_UST_tanks[, longitude := as.numeric(longitude)] # Clean Text to Number

# Clean Dates (Safe Parse handles Excel Serials + Strings)
LA_UST_tanks[, tank_installed_date := safe_parse_date(tank_installed_date)]
LA_UST_tanks[, tank_status_date := safe_parse_date(tank_status_date)]
# ADDED: Parse Upgrade Date
LA_UST_tanks[, initial_cp_upgrade_date := safe_parse_date(initial_cp_upgrade_date)]

# Tank Status Logic
LA_UST_tanks[, tank_status := case_when(
  status_code == "A" ~ "Open",
  status_code %in% c("C", "C1", "I", "O", "R", "R1", "R2") ~ "Closed",
  status_code == "T" ~ "Temporary",
  TRUE ~ NA_character_
)]
LA_UST_tanks <- LA_UST_tanks[tank_status != "Temporary"]



# Determine Closure Date
LA_UST_tanks[, tank_closed_date := NA_Date_]
LA_UST_tanks[tank_status == "Closed", tank_closed_date := tank_status_date]

# Run Classifications
LA_UST_tanks <- classify_tank_walls(LA_UST_tanks)
LA_UST_tanks <- classify_substances(LA_UST_tanks, "substance")

# 5. Process LUST Data ----------------------------------------------------
cat("\nProcessing LUSTs...\n")
LA_LUST_SD <- LA_lust_raw %>%
  select(ai_number, incident_id, conf_rel_date, nfa_date) %>%
  rename(facility_id = ai_number, LUST_id = incident_id, report_date = conf_rel_date) %>%
  as.data.table()

LA_LUST_SD[, `:=`(
  report_date = safe_parse_date(report_date),
  nfa_date = safe_parse_date(nfa_date),
  state = "LA"
)]

# 6. Merge & Geography ----------------------------------------------------
cat("\nMerging and Geocoding...\n")
LA_UST_tanks[, facility_id := as.character(facility_id)]
LA_LUST_SD[, facility_id := as.character(facility_id)]

# Merge Tank and LUST data
LA_UST_tanks_LUST <- merge(LA_UST_tanks, LA_LUST_SD, by="facility_id", all.x=TRUE, allow.cartesian=TRUE)

# Create Leak Flags based on temporal overlap
LA_UST_tanks_LUST[, `:=`(
  leak_after_closure = as.integer(!is.na(tank_closed_date) & !is.na(report_date) & report_date > tank_closed_date),
  leak_before_NFA_before_closure = as.integer(!is.na(tank_closed_date) & !is.na(report_date) & report_date < tank_closed_date & (is.na(nfa_date) | nfa_date < tank_closed_date)),
  leak_before_NFA_after_closure = as.integer(!is.na(tank_closed_date) & !is.na(report_date) & report_date < tank_closed_date & !is.na(nfa_date) & nfa_date > tank_closed_date),
  no_leak = as.integer(is.na(report_date) & !is.na(tank_closed_date))
)]

# Aggregate to Facility-Tank Level
LA_UST_tanks_SD <- LA_UST_tanks_LUST[, .(
  facility_name = first(facility_name),
  tank_status = first(tank_status),
  leak_after_closure = max(leak_after_closure, na.rm=T),
  leak_before_NFA_before_closure = max(leak_before_NFA_before_closure, na.rm=T),
  leak_before_NFA_after_closure = max(leak_before_NFA_after_closure, na.rm=T),
  no_leak = max(no_leak, na.rm=T),
  
  # Wall Types
  single_walled = max(single_walled, na.rm=T),
  double_walled = max(double_walled, na.rm=T),
  unknown_walled = max(unknown_walled, missing_walled, na.rm=T),
  
  # Substances
  is_gasoline = max(is_gasoline, na.rm=T),
  is_diesel = max(is_diesel, na.rm=T),
  is_oil_kerosene = max(is_oil_kerosene, na.rm=T),
  is_jet_fuel = max(is_jet_fuel, na.rm=T),
  is_other = max(is_other, na.rm=T),
  
  capacity = mean(capacity, na.rm=T),
  latitude = first(latitude, na_rm=TRUE),   # ADDED
  longitude = first(longitude, na_rm=TRUE)  # ADDED
), by = .(facility_id, tank_id, tank_installed_date, tank_closed_date, county_name)]

# Geography: Standardize and Merge FIPS
LA_UST_tanks_SD[, standardized_county := standardize_county_name(county_name)]
fips_path <- here("Data", "Processed", "county_fips_codes.csv")

if(file.exists(fips_path)) {
  county_fips <- fread(fips_path)
  county_fips[, standardized_county := standardize_county_name(county_name)]
  
  # Filter for Louisiana (State FIPS 22)
  LA_UST_tanks_SD <- merge(LA_UST_tanks_SD, county_fips[state_fips==22], by="standardized_county", all.x=TRUE)
  LA_UST_tanks_SD[, standardized_county := NULL]
}

LA_UST_tanks_SD[, state := "LA"]

# Final Column Cleanup
required_columns <- c(
  "facility_id", "facility_name", "tank_id", "state", "tank_installed_date", "tank_closed_date", "tank_status",
  "leak_after_closure", "leak_before_NFA_before_closure", "leak_before_NFA_after_closure", "no_leak",
  "capacity", "single_walled", "double_walled", "unknown_walled",
  "is_gasoline", "is_diesel", "is_oil_kerosene", "is_jet_fuel", "is_other", "county_name", "county_fips",
  "latitude", "longitude" # ADDED
)
cols_to_keep <- intersect(names(LA_UST_tanks_SD), required_columns)
LA_UST_tanks_SD <- LA_UST_tanks_SD[, ..cols_to_keep]

# 7. Save -----------------------------------------------------------------
cat("\nSaving Final Files...\n")
output_dir <- here("Data", "Raw", "state_databases", "Louisiana")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

fwrite(LA_UST_tanks_SD, file.path(output_dir, "LA_Harmonized_UST_tanks.csv"))
fwrite(LA_LUST_SD, file.path(output_dir, "LA_Harmonized_LUST.csv"))

cat("Done.\n")
cat("Tank records:", nrow(LA_UST_tanks_SD), "\n")
cat("LUST records:", nrow(LA_LUST_SD), "\n")