###############################################################################
# Code/Helpers/utils_cleaning.R
# Purpose: Shared cleaning functions for all state-level UST scripts.
#          Contains the MOST ADVANCED regex logic extracted from 09_Clean_EPA.
###############################################################################

library(data.table)
library(stringr)

# 1. Advanced Substance Classifier (Source: 09_Clean_EPA_Region6.R) -----------
classify_substances <- function(data, substance_col_name = "substances") {
  # Create binary classification columns with default value
  result <- copy(data)
  
  # Ensure input column exists
  if (!substance_col_name %in% names(result)) {
    stop(paste("Column", substance_col_name, "not found in data."))
  }

  result[, `:=`(
    is_gasoline = 0,
    is_diesel = 0,
    is_oil_kerosene = 0,
    is_jet_fuel = 0,
    is_other = 1  # Default to 1, will set to 0 if any other category matches
  )]
  
  # Classification patterns for EPA data (FULL REGEX)
  gasoline_patterns <- paste(c(
    # Basic gasoline terms
    "\\bgasoline\\b", "\\bgas(?!\\s*(de-hy|condensate))\\b", "\\bmogas\\b", "\\bpetrol\\b", 
    # Grades and types
    "\\b(?:un)?leaded\\b", "\\bpremium\\b", "\\bregular\\b", "\\bmidgrade\\b", "\\bmid-grade\\b", 
    "\\bsuper\\b", "\\bsupreme\\b", "\\bplus\\b", "\\bconv(?:[[:alpha:]]+)?\\b",
    # Ethanol blends
    "\\be(?:-|\\s*)?(10|15|85|[0-9]+)\\b", "\\bethanol(?!.*fuel)\\b", "\\bgasohol\\b", 
    # Common abbreviations
    "\\brul\\b", "\\bpul\\b", "\\bpunl\\b", "\\bunl(?:ead)?\\b", "\\blead[- ]?free\\b",
    # Octanes and specific numbers
    "\\boctane\\b", "\\b87\\b", "\\b89\\b", "\\b91\\b", "\\b93\\b",
    # Additives and specialty terms
    "\\bmtbe\\b", "\\bno[- ]?lead\\b", "\\bno[- ]?ethanol\\b", "\\bretasc\\b",
    "\\bracing(?:[- ]?(?:gas|fuel))?\\b", "\\brec[- ]?fuel\\b", "\\bstandard\\b",
    "\\b100[%]?\\s*(?:unleaded|gas)\\b", "\\bautomotive\\sfuel\\b",
    # Exclude aviation gas and jet fuel using negative lookbehind
    "(?<!av[\\s-]|aviation[\\s-]|jet[\\s-])\\bgas\\b",
    "(?<!jet|aviation)\\s+fuel"
  ), collapse = "|")

  diesel_patterns <- paste(c(
    # Basic diesel terms
    "\\bdiesel\\b", "\\bdsl\\b", "\\bd$\\b", "\\bord\\b", "\\bdies\\b", "\\bdied fuel\\b",
    "\\b(?:auto|vehicular)[- ]?diesel\\b", "\\bulsd\\b", "\\bdistill\\b",
    # Biodiesel variants
    "\\bbio[- ]?diesel\\b", "\\bb[- ]?([0-9]+(?:\\.[0-9]+)?)\\b", "\\bb100\\b", "\\bb99\\b",
    # Diesel grades and types
    "\\bd[- ]?[0-9]\\b", "\\b#[1-6][- ]?(?:diesel)\\b", "\\bon[- ]?road\\b", 
    "\\bhigh sulp(?:h)?ur diesel\\b", "\\bheavy diesel\\b",
    # Off-road and dyed diesel
    "\\b(?:off|off[- ]?road)[- ]?d(?:iesel)?\\b", "\\boff[- ]?rd(?:[- ]?d(?:sl|iesel)?)?\\b", 
    "\\bdyed(?:[- ]?d(?:sl|iesel)?)?\\b", "\\bred[- ]?diesel\\b", "\\bfarm[- ]?diesel\\b",
    # Exclude terms that look like diesel but aren't
    "(?<!def|fluid)\\s*\\bdiesel\\b"
  ), collapse = "|")

  oil_kerosene_patterns <- paste(c(
    # Oil types
    "\\boil\\b", "\\blubrican(?:t|ting)\\b", "\\blube(?:\\s+oil)?\\b", 
    "\\b(?:motor|engine|used|waste|virgin|new|bulk|hydraulic|quench|cutting|hoist|gear|process|transmission|mineral)(?:[- ]?oil)?\\b",
    "\\bhyd(?:raul(?:ic)?)?(?:\\.|[- ]?fluid|\\.fluid|\\s)\\b", "\\btrans(?:mission)?(?:[- ]?fluid)?\\b",
    "\\banti[- ]?freeze\\b", "\\bcool(?:ant)?\\b", "\\bethylene[- ]?glycol\\b",
    # Kerosene types
    "\\bkerosene\\b", "\\bkerosine\\b", "\\bk-1\\b",
    # Fuel oil types  
    "\\bfuel[- ]?oil\\b", "\\b(?:heating|heat)[- ]?oil\\b", 
    "\\b#[1-6](\\s|\\/)?(fuel|heating)?[- ]?oil\\b",
    "\\bway[- ]?(?:oil|lube)\\b", "\\bdrain[- ]?oil\\b", "\\batf\\b", 
    "\\bcrude\\b", "\\bheavy\\b", "\\bsolutio?n\\b", "\\bslurry\\b", "\\bemulsion\\b",
    "\\bpower[- ]?steer(?:ing)?(?:[- ]?(?:fluid|oil))?\\b", "\\b10w(?:30|40|50)?\\b",
    "\\b30[- ]?(?:w|wt)\\b", "\\b40[- ]?(?:w|wt)\\b", "\\b5w(?:30|40)\\b"
  ), collapse = "|")

  jet_fuel_patterns <- paste(c(
    # Jet fuel terms
    "\\bjet(?:[- ]?(?:a|fuel))?\\b", "\\bjp[- ]?[45678]\\b", "\\bturbo[- ]?fuel\\b",
    # Aviation fuel terms
    "\\baviation(?:[- ]?(?:fuel|gas))?\\b", "\\bav(?:[- ]?(?:gas|fuel))?\\b", 
    "\\bavgas\\b", "\\bav[- ]?gas\\b", "\\b100ll\\b", "\\b100\\s+oct\\b", "\\b80\\s+oct\\b",
    "\\baircraft\\b", "\\bairplane\\b", "\\bskychief\\b"
  ), collapse = "|")  
  
  # Convert substances to lowercase
  result[, substances_lower := tolower(get(substance_col_name))]
  
  # Apply classification patterns using vectorized operations
  result[!is.na(substances_lower) & substances_lower != "", 
         is_gasoline := as.integer(grepl(gasoline_patterns, substances_lower, perl = TRUE))]
  
  # Exclude DEF from diesel classification
  result[!is.na(substances_lower) & substances_lower != "" & 
         !grepl("def|diesel exhaust fluid", substances_lower, perl = TRUE), 
         is_diesel := as.integer(grepl(diesel_patterns, substances_lower, perl = TRUE))]
  
  result[!is.na(substances_lower) & substances_lower != "", 
         is_oil_kerosene := as.integer(grepl(oil_kerosene_patterns, substances_lower, perl = TRUE))]
  
  result[!is.na(substances_lower) & substances_lower != "", 
         is_jet_fuel := as.integer(grepl(jet_fuel_patterns, substances_lower, perl = TRUE))]
  
  # Set "other" to 0 if any other category matched
  result[, is_other := as.integer(is_gasoline + is_diesel + is_oil_kerosene + is_jet_fuel == 0)]
  
  # Remove temporary column
  result[, substances_lower := NULL]
  
  return(result)
}

# 2. Advanced Tank Wall Classifier (Source: 09_Clean_EPA_Region6.R) -----------
classify_tank_walls <- function(data, tank_wall_col = "tank_wall_type") {
  result <- copy(data)
  
  if (!tank_wall_col %in% names(result)) {
     message(paste("Warning: Column", tank_wall_col, "not found. Creating empty wall columns."))
     result[, `:=`(double_walled=0, single_walled=0, unknown_walled=1, missing_walled=1)]
     return(result)
  }

  # Create binary variables for tank wall type
  result[, `:=`(
    double_walled = as.integer(grepl("DOUBLE|TRIPLE|DBL|DUAL|SECONDAR", toupper(get(tank_wall_col)), ignore.case = TRUE)),
    single_walled = as.integer(grepl("SINGLE", toupper(get(tank_wall_col)), ignore.case = TRUE) & 
                              !grepl("UNKNOWN", toupper(get(tank_wall_col)), ignore.case = TRUE)),
    unknown_walled = as.integer(grepl("UNKNOWN", toupper(get(tank_wall_col)), ignore.case = TRUE)),
    missing_walled = as.integer(is.na(get(tank_wall_col)) | get(tank_wall_col) == "")
  )]
  
  return(result)
}

# 3. Standardize Capacity (Source: 09_Clean_EPA_Region6.R) ------------------
standardize_capacity <- function(data, capacity_col = "capacity") {
  result <- copy(data)
  
  if (capacity_col %in% names(result)) {
    result[, `:=`(
      capacity = as.numeric(gsub("[^0-9.]", "", get(capacity_col)))
    )]
  } else {
    result[, capacity := NA_real_]
  }
  
  return(result)
}

# 4. Standardize County Name (Source: 09_Clean_EPA_Region6.R) ---------------
standardize_county_name <- function(county_name) {
  if(is.na(county_name) || county_name == "") return(NA)
  
  # Convert to lowercase
  name <- tolower(county_name)
  
  # Remove "county", "parish", etc.
  name <- gsub(" county$| counties$| parish$| parishes$", "", name)
  
  # Remove special characters and extra spaces
  name <- gsub("[^a-z ]", "", name)
  name <- trimws(gsub("\\s+", " ", name))
  
  return(name)
}

# 5. Facility Name Standardizer (New Addition) ------------------------------
clean_facility_name <- function(name_col) {
  # Uppercase
  val <- toupper(name_col)
  # Remove special chars (keep alphanumeric, space, hyphen, ampersand)
  val <- str_replace_all(val, "[^A-Z0-9\\-\\ &]", " ")
  # Squash whitespace
  val <- str_squish(val)
  # Basic standardization
  val <- str_replace_all(val, "\\bRD\\b", "ROAD")
  val <- str_replace_all(val, "\\bST\\b", "STREET")
  val <- str_replace_all(val, "\\bAVE\\b", "AVENUE")
  
  return(val)
}

# 6. Schema Enforcer (New Addition) -----------------------------------------
enforce_schema <- function(dt) {
  # Target Schema Definition
  required_cols <- c(
    "state", "facility_id", "tank_id", 
    "install_date", "closed_date", "tank_status", 
    "capacity", 
    "is_gasoline", "is_diesel", "is_oil_kerosene", "is_jet_fuel", "is_other",
    "double_walled", "single_walled", "unknown_walled", "missing_walled",
    "facility_name", "address", "city", "zip", "county", 
    "latitude", "longitude"
  )
  
  # Check for missing columns and create them as NA if needed
  missing_cols <- setdiff(required_cols, names(dt))
  if (length(missing_cols) > 0) {
    message(paste("Warning: Creating missing columns as NA:", paste(missing_cols, collapse=", ")))
    dt[, (missing_cols) := NA]
  }
  
  # Enforce Types (Basic safety)
  if("install_date" %in% names(dt)) dt[, install_date := as.Date(install_date)]
  if("closed_date" %in% names(dt))  dt[, closed_date := as.Date(closed_date)]
  
  # Return data with columns in correct order
  setcolorder(dt, required_cols)
  return(dt)
}