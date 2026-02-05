## EPA Region UST Dataset Creation
# Script: 03_Clean_EPA_Regions.R
# Author: Validated Classification Logic (Updated Jan 2026)
# Purpose: 
#   1. Process ~42 "Standard States" from EPA national dataset (Geocoding + Harmonization).
#   2. Extract Lat/Long tables for the 8 "Custom States" (AR, LA, ME, MI, NJ, NM, OK, TX).

library(here)
library(data.table)
library(tidyverse)
library(lubridate)
library(janitor)
library(stringr)
library(tigris) 

# Enable caching for Tigris to speed up FIPS lookups
options(tigris_use_cache = TRUE)

# 0. Setup & Helper Functions --------------------------------------------------

# Ensure output root exists
root_output_dir <- here("Data", "Raw", "state_databases")
if (!dir.exists(root_output_dir)) dir.create(root_output_dir, recursive = TRUE)
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

# --- 2. Robust Substance Classifier ---
classify_substances <- function(data, substance_col_name = "substances") {
  result <- copy(data)
  result[, `:=`(
    is_gasoline = 0, is_diesel = 0, is_oil_kerosene = 0, is_jet_fuel = 0, is_other = 1
  )]
  
  # Regex Patterns
  gasoline_patterns <- paste(c(
    "\\bgasoline\\b", "\\bgas(?!\\s*(de-hy|condensate))\\b", "\\bmogas\\b", "\\bpetrol\\b", 
    "\\b(?:un)?leaded\\b", "\\bpremium\\b", "\\bregular\\b", "\\bmidgrade\\b", "\\bmid-grade\\b", 
    "\\bsuper\\b", "\\bsupreme\\b", "\\bplus\\b", "\\bconv(?:[[:alpha:]]+)?\\b",
    "\\be(?:-|\\s*)?(10|15|85|[0-9]+)\\b", "\\bethanol(?!.*fuel)\\b", "\\bgasohol\\b", 
    "\\brul\\b", "\\bpul\\b", "\\bpunl\\b", "\\bunl(?:ead)?\\b", "\\blead[- ]?free\\b",
    "\\boctane\\b", "\\b87\\b", "\\b89\\b", "\\b91\\b", "\\b93\\b",
    "\\bmtbe\\b", "\\bno[- ]?lead\\b", "\\bno[- ]?ethanol\\b", "\\bretasc\\b",
    "\\bracing(?:[- ]?(?:gas|fuel))?\\b", "\\brec[- ]?fuel\\b", "\\bstandard\\b",
    "\\b100[%]?\\s*(?:unleaded|gas)\\b", "\\bautomotive\\sfuel\\b",
    "(?<!av[\\s-]|aviation[\\s-]|jet[\\s-])\\bgas\\b", "(?<!jet|aviation)\\s+fuel"
  ), collapse = "|")
  
  diesel_patterns <- paste(c(
    "\\bdiesel\\b", "\\bdsl\\b", "\\bd$\\b", "\\bord\\b", "\\bdies\\b", "\\bdied fuel\\b",
    "\\b(?:auto|vehicular)[- ]?diesel\\b", "\\bulsd\\b", "\\bdistill\\b",
    "\\bbio[- ]?diesel\\b", "\\bb[- ]?([0-9]+(?:\\.[0-9]+)?)\\b", "\\bb100\\b", "\\bb99\\b",
    "\\bd[- ]?[0-9]\\b", "\\b#[1-6][- ]?(?:diesel)\\b", "\\bon[- ]?road\\b", 
    "\\bhigh sulp(?:h)?ur diesel\\b", "\\bheavy diesel\\b",
    "\\b(?:off|off[- ]?road)[- ]?d(?:iesel)?\\b", "\\boff[- ]?rd(?:[- ]?d(?:sl|iesel)?)?\\b", 
    "\\bdyed(?:[- ]?d(?:sl|iesel)?)?\\b", "\\bred[- ]?diesel\\b", "\\bfarm[- ]?diesel\\b",
    "(?<!def|fluid)\\s*\\bdiesel\\b"
  ), collapse = "|")
  
  oil_kerosene_patterns <- paste(c(
    "\\boil\\b", "\\blubrican(?:t|ting)\\b", "\\blube(?:\\s+oil)?\\b", 
    "\\b(?:motor|engine|used|waste|virgin|new|bulk|hydraulic|quench|cutting|hoist|gear|process|transmission|mineral)(?:[- ]?oil)?\\b",
    "\\bhyd(?:raul(?:ic)?)?(?:\\.|[- ]?fluid|\\.fluid|\\s)\\b", "\\btrans(?:mission)?(?:[- ]?fluid)?\\b",
    "\\banti[- ]?freeze\\b", "\\bcool(?:ant)?\\b", "\\bethylene[- ]?glycol\\b",
    "\\bkerosene\\b", "\\bkerosine\\b", "\\bk-1\\b",
    "\\bfuel[- ]?oil\\b", "\\b(?:heating|heat)[- ]?oil\\b", 
    "\\b#[1-6](\\s|\\/)?(fuel|heating)?[- ]?oil\\b",
    "\\bway[- ]?(?:oil|lube)\\b", "\\bdrain[- ]?oil\\b", "\\batf\\b", 
    "\\bcrude\\b", "\\bheavy\\b", "\\bsolutio?n\\b", "\\bslurry\\b", "\\bemulsion\\b",
    "\\bpower[- ]?steer(?:ing)?(?:[- ]?(?:fluid|oil))?\\b", "\\b10w(?:30|40|50)?\\b",
    "\\b30[- ]?(?:w|wt)\\b", "\\b40[- ]?(?:w|wt)\\b", "\\b5w(?:30|40)\\b"
  ), collapse = "|")
  
  jet_fuel_patterns <- paste(c(
    "\\bjet(?:[- ]?(?:a|fuel))?\\b", "\\bjp[- ]?[45678]\\b", "\\bturbo[- ]?fuel\\b",
    "\\baviation(?:[- ]?(?:fuel|gas))?\\b", "\\bav(?:[- ]?(?:gas|fuel))?\\b", 
    "\\bavgas\\b", "\\bav[- ]?gas\\b", "\\b100ll\\b", "\\b100\\s+oct\\b", "\\b80\\s+oct\\b",
    "\\baircraft\\b", "\\bairplane\\b", "\\bskychief\\b"
  ), collapse = "|")
  
  # Execution
  result[, substances_lower := tolower(get(substance_col_name))]
  
  result[!is.na(substances_lower) & substances_lower != "", 
         is_gasoline := as.integer(grepl(gasoline_patterns, substances_lower, perl = TRUE))]
  
  result[!is.na(substances_lower) & substances_lower != "" & 
           !grepl("def|diesel exhaust fluid", substances_lower, perl = TRUE), 
         is_diesel := as.integer(grepl(diesel_patterns, substances_lower, perl = TRUE))]
  
  result[!is.na(substances_lower) & substances_lower != "", 
         is_oil_kerosene := as.integer(grepl(oil_kerosene_patterns, substances_lower, perl = TRUE))]
  
  result[!is.na(substances_lower) & substances_lower != "", 
         is_jet_fuel := as.integer(grepl(jet_fuel_patterns, substances_lower, perl = TRUE))]
  
  result[, is_other := as.integer(is_gasoline + is_diesel + is_oil_kerosene + is_jet_fuel == 0)]
  result[, substances_lower := NULL]
  return(result)
}

# --- 3. Tank Wall Classifier ---
classify_tank_walls <- function(data, tank_wall_col = "tank_wall_type") {
  result <- copy(data)
  vals <- toupper(result[[tank_wall_col]])
  
  result[, `:=`(
    double_walled = as.integer(grepl("DOUBLE|TRIPLE|DBL|DUAL|SECONDAR|JACKET", vals, ignore.case = TRUE)),
    single_walled = as.integer(grepl("SINGLE|SGL", vals, ignore.case = TRUE) & 
                                 !grepl("UNKNOWN", vals, ignore.case = TRUE)),
    unknown_walled = as.integer(grepl("UNKNOWN", vals, ignore.case = TRUE)),
    missing_walled = as.integer(is.na(vals) | vals == "")
  )]
  
  # Diagnostic: If data was "", it is now missing_walled=1. 
  # We consolidate missing into unknown for the final robust dataset
  result[, unknown_walled := pmax(unknown_walled, missing_walled, na.rm = TRUE)]
  return(result)
}

standardize_capacity <- function(data, capacity_col = "capacity") {
  result <- copy(data)
  result[, `:=`(capacity = as.numeric(gsub("[^0-9.]", "", get(capacity_col))))]
  return(result)
}

# --- 4. Vectorized County Standardizer ---
# Critical Fix: Do NOT use if() on the vector. Use vectorized operations.
standardize_county_name <- function(county_name_vec) {
  name <- as.character(county_name_vec)
  name <- tolower(name)
  name <- gsub(" county$| counties$| parish$| parishes$", "", name)
  name <- gsub("[^a-z ]", "", name)
  name <- trimws(gsub("\\s+", " ", name))
  
  # Handle empty or NA strings
  name[name == "" | name == "na"] <- NA_character_
  return(name)
}

# 1. Load Data -----------------------------------------------------------------

cat("\nLoading raw EPA data (Permissive Mode)...\n")

# Loading as character prevents type inference crashes on mixed columns
epa_usts <- fread(here("Data", "Raw", "USTs.csv"), colClasses = "character") %>% 
  clean_names() %>% as.data.table()

epa_facilities <- fread(here("Data", "Raw", "Facilities.csv"), colClasses = "character") %>% 
  clean_names() %>% as.data.table()

epa_releases <- fread(here("Data", "Raw", "Releases.csv"), colClasses = "character") %>% 
  clean_names() %>% as.data.table()

cat("\nJoining Geocoding Data...\n")
fac_geo <- epa_facilities[, .(facility_id, facility_name = name, county, city, zip_code, latitude, longitude)]

# Master merge of UST + Geo
state_UST_full <- merge(
  epa_usts, 
  fac_geo, 
  by = "facility_id", 
  all.x = TRUE
)

# 2. State Partitioning --------------------------------------------------------

# States with custom scripts (we only extract Lat/Long for these)
custom_script_states <- c(
  "Arkansas",   # 01_Clean_AR.R
  "Louisiana",  # 02_Clean_LA.R
  "Maine",      # 03_Clean_ME.R
  "Michigan",   # 04_Clean_MI.R
  "New Jersey", # 05_Clean_NJ.R
  "New Mexico", # 06_Clean_NM.R
  "Oklahoma",   # 07_Clean_OK.R
  "Texas",      # 08_Clean_TX.R
  "Alabama",    # 10_Clean_AL.R
  "Tennessee",  # 11_Clean_TN.R
  "Colorado"    # 13_Clean_CO.R
)

all_states <- unique(state_UST_full$state)
# All other states get full harmonization here
states_to_process_full <- setdiff(all_states, custom_script_states)
if(!"Kansas" %in% states_to_process_full) states_to_process_full <- c(states_to_process_full, "Kansas")

# 3. Main Processing Function --------------------------------------------------

process_state_data <- function(state_name) {
  
  # Abbreviation Logic
  state_abbr <- state.abb[match(state_name, state.name)]
  if(is.na(state_abbr)) state_abbr <- toupper(substring(state_name, 1, 2))
  
  state_out_dir <- here("Data", "Raw", "state_databases", state_name)
  if (!dir.exists(state_out_dir)) dir.create(state_out_dir, recursive = TRUE)
  
  cat(paste0("\n--- Processing ", state_name, " (", state_abbr, ") ---\n"))
  
  # --- Step A: UST Data ---
  state_ust <- state_UST_full[state == state_name]
  
  state_ust[, `:=`(
    facility_id = as.character(facility_id),
    tank_id = as.character(tank_id),
    facility_name = stringr::str_to_title(trimws(gsub("[^[:alnum:] ]", "", facility_name))),
    
    # APPLY ROBUST DATE PARSER
    tank_installed_date = parse_epa_date(installation_date),
    tank_closed_date = parse_epa_date(removal_date),
    
    # Status: Matches "Closed", "Permanently Out", "Removed", "Abandoned"
    tank_status_clean = ifelse(grepl("Perm|Closed|Remov|Aband", tank_status, ignore.case = TRUE), "Closed", "Open")
  )]
  
  state_ust[tank_status_clean == "Open", tank_closed_date := NA]
  
  # Classifiers
  state_ust <- classify_substances(state_ust, "substances")
  state_ust <- classify_tank_walls(state_ust, "tank_wall_type")
  state_ust <- standardize_capacity(state_ust, "capacity")
  
  # --- Step B: LUST Data ---
  state_lust <- epa_releases[state == state_name]
  
  if(nrow(state_lust) > 0) {
    state_lust[, `:=`(
      facility_id = as.character(facility_id),
      LUST_id = as.character(lust_id),
      # APPLY ROBUST DATE PARSER
      report_date = parse_epa_date(reported_date),
      nfa_date = as.Date(NA),
      state = state_abbr
    )]
    
    LUST_Harmonized <- state_lust[, .(facility_id, LUST_id, report_date, nfa_date, state)]
    fwrite(LUST_Harmonized, file.path(state_out_dir, paste0(state_abbr, "_Harmonized_LUST.csv")))
    
    leak_counts <- state_lust[!is.na(report_date), .(LUST_count = .N), by = .(facility_id, report_date)]
  } else {
    leak_counts <- data.table(facility_id=character(), report_date=as.Date(character()), LUST_count=integer())
  }
  
  # --- Step C: Merge ---
  ust_with_leaks <- merge(state_ust, leak_counts, by = "facility_id", all.x = TRUE, allow.cartesian = TRUE)
  
  ust_with_leaks[, `:=`(
    leak_after_closure = as.integer(!is.na(tank_closed_date) & !is.na(report_date) & report_date > tank_closed_date),
    leak_before_NFA_before_closure = 0,
    leak_before_NFA_after_closure = 0,
    no_leak = as.integer(is.na(report_date) & !is.na(tank_closed_date))
  )]
  
  # --- Step D: Aggregate ---
  UST_Harmonized <- ust_with_leaks[, .(
    facility_name = first(facility_name),
    tank_status = first(tank_status_clean),
    capacity = mean(capacity, na.rm = TRUE),
    
    # Lat/Long from Facilities File
    latitude = first(as.numeric(latitude)),
    longitude = first(as.numeric(longitude)),
    
    leak_after_closure = max(leak_after_closure, na.rm=T),
    leak_before_NFA_before_closure = 0,
    leak_before_NFA_after_closure = 0,
    no_leak = max(no_leak, na.rm=T),
    
    single_walled = max(single_walled, na.rm=TRUE),
    double_walled = max(double_walled, na.rm=TRUE),
    unknown_walled = max(unknown_walled, na.rm=TRUE),
    
    is_gasoline = max(is_gasoline, na.rm=TRUE),
    is_diesel = max(is_diesel, na.rm=TRUE),
    is_oil_kerosene = max(is_oil_kerosene, na.rm=TRUE),
    is_jet_fuel = max(is_jet_fuel, na.rm=TRUE),
    is_other = max(is_other, na.rm=TRUE),
    
    county_name = first(county)
  ), by = .(facility_id, tank_id, tank_installed_date, tank_closed_date)]
  
  UST_Harmonized[, state := state_abbr]
  
  # --- Step E: FIPS ---
  UST_Harmonized[, standardized_county := standardize_county_name(county_name)]
  
  tryCatch({
    fips_lookup <- counties(state = state_name, cb = TRUE, progress_bar = FALSE)
    setDT(fips_lookup)
    fips_lookup[, standardized_county := standardize_county_name(NAME)]
    UST_Harmonized <- merge(UST_Harmonized, fips_lookup[, .(standardized_county, GEOID)], by = "standardized_county", all.x = TRUE)
    setnames(UST_Harmonized, "GEOID", "county_fips")
  }, error = function(e) {
    # If fetch fails, populate NA
    if(!"county_fips" %in% names(UST_Harmonized)) UST_Harmonized[, county_fips := NA_character_]
  })
  
  if("standardized_county" %in% names(UST_Harmonized)) UST_Harmonized[, standardized_county := NULL]
  
  # --- Step F: Save ---
  required_columns <- c(
    "facility_id", "facility_name", "tank_id", "state", "tank_installed_date", "tank_closed_date", "tank_status",
    "leak_after_closure", "leak_before_NFA_before_closure", "leak_before_NFA_after_closure", "no_leak",
    "capacity", "single_walled", "double_walled", "unknown_walled",
    "is_gasoline", "is_diesel", "is_oil_kerosene", "is_jet_fuel", "is_other", 
    "county_name", "county_fips", "latitude", "longitude"
  )
  
  for(col in required_columns) if(!col %in% names(UST_Harmonized)) UST_Harmonized[, (col) := NA]
  UST_Harmonized <- UST_Harmonized[, ..required_columns]
  
  fwrite(UST_Harmonized, file.path(state_out_dir, paste0(state_abbr, "_Harmonized_UST_tanks.csv")))
  cat(paste0("  Saved ", state_abbr, "_Harmonized_UST_tanks.csv\n"))
}

# 4. Excluded States GIS Extraction --------------------------------------------

process_excluded_gis <- function(state_name) {
  state_abbr <- state.abb[match(state_name, state.name)]
  if(is.na(state_abbr)) state_abbr <- toupper(substring(state_name, 1, 2))
  
  cat(paste0("\n--- Extracting GIS for Custom State: ", state_name, " (", state_abbr, ") ---\n"))
  state_out_dir <- here("Data", "Raw", "state_databases", state_name)
  if (!dir.exists(state_out_dir)) dir.create(state_out_dir, recursive = TRUE)
  
  state_data <- state_UST_full[state == state_name]
  
  # ID Cleaning Matching Local Scripts
  if (state_abbr == "MI") {
    state_data[, clean_id := gsub("^MI", "", facility_id)]
  } else if (state_abbr == "NJ") {
    state_data[, clean_id := gsub("^NJ", "", facility_id)]
  } else if (state_abbr == "NM") {
    state_data[, clean_id := gsub("NM", "", facility_id)] 
  } else if (state_abbr == "OK") {
    state_data[, clean_id := gsub("\\[|\\]", "", facility_id)]
  } else {
    # Default trim for others (AR, LA, ME, TX)
    state_data[, clean_id := gsub(paste0("^", state_abbr), "", facility_id)]
  }
  
  gis_extract <- unique(state_data[, .(
    facility_id = as.character(clean_id),
    state = state_abbr,
    latitude = as.numeric(latitude),
    longitude = as.numeric(longitude)
  )])
  
  # Save
  gis_extract <- gis_extract[!is.na(latitude) & !is.na(longitude)]
  fwrite(gis_extract, file.path(state_out_dir, paste0(state_abbr, "_Harmonized_latlong.csv")))
  cat(paste0("  Saved ", state_abbr, "_Harmonized_latlong.csv (", nrow(gis_extract), " records)\n"))
}

# 5. Execution -----------------------------------------------------------------

cat("\n=== Processing Standard EPA States ===\n")
states_to_process_full <- states_to_process_full[!is.na(states_to_process_full) & states_to_process_full != ""]

for (st in states_to_process_full) {
  tryCatch({ process_state_data(st) }, error = function(e) cat(paste0("Error processing ", st, ": ", e$message, "\n")))
}

cat("\n=== Extracting GIS for Custom States ===\n")
for (st in custom_script_states) {
  tryCatch({ process_excluded_gis(st) }, error = function(e) cat(paste0("Error extracting GIS for ", st, ": ", e$message, "\n")))
}

cat("\n\nScript Complete.\n")