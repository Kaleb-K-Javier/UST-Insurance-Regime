## Louisiana UST Dataset Creation

# Define the county variable name in the raw data
# Note: Louisiana has parishes instead of counties, but 
# we'll still use county_name for consistency
COUNTYVARIABLE <- "PARISH_DESC"

library(here)
first_run = TRUE
#load packags ----
source(here('Code','00_global_packages.R'))

# Function to standardize county names
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

# Function to classify tank wall types
classify_tank_walls <- function(data, tank_wall_cols = c("MCT_DOUBLE_WALLED", "MCT_POLY_JACKET", "MCT_CPSTEEL_INT_LINE", "MCT_LINED_INTERIOR", "MCT_EXCAVAT_LINER")) {
  result <- copy(data)
  
  # Check which columns exist in the data
  available_cols <- tank_wall_cols[tank_wall_cols %in% names(result)]
  
  if(length(available_cols) == 0) {
    warning("None of the specified tank wall columns found in data!")
    # Create default values if no columns are found
    result[, `:=`(
      double_walled = 0,
      single_walled = 0,
      unknown_walled = 1,
      missing_walled = 1
    )]
    return(result)
  }
  
  # Create a new column that indicates if any of the specified columns has a "Y"
  result[, any_double_walled := 0]
  
  for(col in available_cols) {
    if(col %in% names(result)) {
      result[get(col) == "Y", any_double_walled := 1]
    }
  }
  
  # Create binary variables for tank wall type based on the combined check
  result[, `:=`(
    double_walled = as.integer(any_double_walled == 1),
    single_walled = as.integer(any_double_walled == 0 && all(!is.na(mget(available_cols)) & mget(available_cols) != "")),
    unknown_walled = as.integer(any_double_walled == 0 && any(is.na(mget(available_cols)) | mget(available_cols) == "")),
    missing_walled = as.integer(all(is.na(mget(available_cols))))
  )]
  
  # Remove temporary column
  result[, any_double_walled := NULL]
  
  # Create a summary table
  cat("\nTank wall classification summary:\n")
  print(result[, .(
    total_records = .N,
    single_walled_count = sum(single_walled, na.rm = TRUE),
    double_walled_count = sum(double_walled, na.rm = TRUE),
    unknown_walled_count = sum(unknown_walled, na.rm = TRUE),
    missing_walled_count = sum(missing_walled, na.rm = TRUE)
  )])
  
  return(result)
}

# Function to classify substances
classify_substances <- function(data, substance_col_name = "TANK_SUBSTANCE_CODE") {
  # Create binary classification columns with default value
  result <- copy(data)
  result[, `:=`(
    is_gasoline = 0,
    is_diesel = 0,
    is_oil_kerosene = 0,
    is_jet_fuel = 0,
    is_other = 1  # Default to 1, will set to 0 if any other category matches
  )]
  
  # Classification patterns
  gasoline_patterns <- paste(c(
    "gas", "gasolin", "petrol", "motor fuel", "fuel oil", "e10", "e15", "e85", 
    "e\\d+", "unleaded", "premium", "regular", "petroleum", "fuel", "conv[[:alpha:]]+", 
    "mid", "plus", "supreme", "super", "octane", "87", "89", "91", "93", "\\bmtbe\\b"
  ), collapse = "|")
  
  diesel_patterns <- paste(c(
    "diesel", "dsl", "dl$", "\\bd\\b", "bio.?diesel", "red diesel", "off.?road diesel", 
    "^d ", "\\bdl\\b", "ulsd", "\\bd[0-9]\\b"
  ), collapse = "|")
  
  oil_kerosene_patterns <- paste(c(
    "oil", "kerosene", "kerosine", "heating oil", "motor oil", "used oil", 
    "waste oil", "lubricant", "lube", "heating", "hydraulic", "transmission",
    "crude", "heavy", "mineral", "vegetable", "\\bcrank"
  ), collapse = "|")
  
  jet_fuel_patterns <- paste(c(
    "jet", "aviation", "av gas", "avgas", "jp.?[45678]", "aircraft", "airplane"
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
  
  # Create a summary table of the classifications
  cat("\nSubstance classification summary:\n")
  print(result[, .(
    total_records = .N,
    gasoline_count = sum(is_gasoline, na.rm = TRUE),
    diesel_count = sum(is_diesel, na.rm = TRUE),
    oil_kerosene_count = sum(is_oil_kerosene, na.rm = TRUE),
    jet_fuel_count = sum(is_jet_fuel, na.rm = TRUE),
    other_count = sum(is_other, na.rm = TRUE),
    multi_category_count = sum(is_gasoline + is_diesel + is_oil_kerosene + is_jet_fuel > 1, na.rm = TRUE)
  )])
  
  return(result)
}

# Function to standardize capacity
standardize_capacity <- function(data, capacity_col = "TANK_CAPACITY") {
  result <- copy(data)
  
  result[, `:=`(
    capacity = as.numeric(gsub("[^0-9.]", "", get(capacity_col)))
  )]
  
  return(result)
}

# Load Raw Data - Louisiana Excel file -----
cat("\nLoading Louisiana UST and LUST data from Excel...\n")

# Use readxl to read the Excel file
library(readxl)

# Define the file path
la_file_path <- here("Data","Raw","state_databases","Louisiana","LA_record_request.xlsx")

# Read the tank data from "Tanks info by compartment" sheet
LA_tanks_raw <- read_excel(la_file_path, sheet = "Tank Info by compartment") %>%
  janitor::clean_names() %>%
  as.data.table()

# Read the LUST data from "LUST Sites by incident ID" sheet
LA_lust_raw <- read_excel(la_file_path, sheet = "LUST Sites by incident ID") %>%
  janitor::clean_names() %>%
  as.data.table()

# Print summaries of loaded data
cat("\nLoaded", nrow(LA_tanks_raw), "tank records and", nrow(LA_lust_raw), "LUST records\n")
cat("\nTank data column names:", paste(names(LA_tanks_raw), collapse = ", "), "\n")
cat("\nLUST data column names:", paste(names(LA_lust_raw), collapse = ", "), "\n")

# Process Tank Data -----
cat("\nProcessing Louisiana tank data...\n")

# Select and rename columns according to the specified mappings
LA_UST_tanks <- LA_tanks_raw %>%
  select(
    master_ai_id,   # facility id
    master_ai_name, # facility name
    parish_desc,    # county_name
    subject_item_id, # tank_id
    tank_capacity,  # capacity
    install_date,   # tank_install_date
    tank_substance_code, # substance
    mct_double_walled, # Double wall tank indicator
    current_status_code, # status code
    current_status_desc, # status description
    current_status_start_date # status date
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
    tank_status_date = current_status_start_date
  ) %>%
  as.data.table()

# Determine tank status based on status codes
# Preserve specific logic
LA_UST_tanks[, tank_status := case_when(
  status_code == "A" ~ "Open",
  status_code %in% c("C", "C1", "I", "O", "R", "R1", "R2") ~ "Closed",
  status_code == "T" ~ "Temporary",
  TRUE ~ NA_character_
)]

# Force IDs to character
LA_UST_tanks[, facility_id := as.character(facility_id)]
LA_UST_tanks[, tank_id := as.character(tank_id)]

# Fix dates
LA_UST_tanks[, tank_status_date := as.Date(tank_status_date, origin = "1899-12-30")]
LA_UST_tanks[, tank_closed_date := NA_Date_] 
LA_UST_tanks[, tank_closed_date := tank_status_date]
setDT(LA_UST_tanks)
LA_UST_tanks[, tank_installed_date := lubridate::ymd(tank_installed_date)]
LA_UST_tanks[tank_status !="Closed", tank_closed_date := NA_Date_]

# Filter out Temporary tanks (status_code = "T")
LA_UST_tanks <- LA_UST_tanks[tank_status != "Temporary"]

# Apply standardization functions
cat("\nStandardizing capacity values...\n")
LA_UST_tanks <- standardize_capacity(LA_UST_tanks, "tank_capacity")

cat("\nClassifying tank wall types...\n")
LA_UST_tanks <- classify_tank_walls(LA_UST_tanks, "mct_double_walled")

cat("\nClassifying substance types...\n")
LA_UST_tanks <- classify_substances(LA_UST_tanks, "substance")

# Process LUST Data -----
cat("\nProcessing Louisiana LUST data...\n")

# Select and rename columns according to the specified mappings
LA_LUST_SD <- LA_lust_raw %>%
  select(
    ai_number,    # facility id
    incident_id,  # LUSTID
    conf_rel_date, # release_date
    nfa_date      # nfa date
  ) %>%
  rename(
    facility_id = ai_number,
    LUST_id = incident_id,
    report_date = conf_rel_date
  ) %>%
  as.data.table()

# Ensure character types for IDs and add State
LA_LUST_SD[, `:=`(
  facility_id = as.character(facility_id),
  LUST_id = as.character(LUST_id),
  state = "LA" # Added State Variable
)]

# Convert dates to proper format
LA_LUST_SD[, report_date := as.Date(lubridate::ymd_hms(report_date))]
LA_LUST_SD[, nfa_date := as.Date(lubridate::ymd_hms(nfa_date))]

# Join LUST data with tank data
cat("\nJoining LUST data with tank data...\n")

LA_UST_tanks_LUST <- merge(
  LA_UST_tanks, 
  LA_LUST_SD[, .(facility_id, LUST_id, report_date, nfa_date)],
  by = "facility_id",
  all.x = TRUE,
  allow.cartesian = TRUE
)

# Create leak indicators
LA_UST_tanks_LUST <- LA_UST_tanks_LUST %>%
  mutate(
    leak_after_closure = ifelse(!is.na(tank_closed_date) & !is.na(report_date) & 
                                  report_date > tank_closed_date, 1, 0),
    leak_before_NFA_before_closure = ifelse(!is.na(tank_closed_date) & !is.na(report_date) & 
                                              report_date < tank_closed_date & 
                                              (is.na(nfa_date) | nfa_date < tank_closed_date), 1, 0),
    leak_before_NFA_after_closure = ifelse(!is.na(tank_closed_date) & !is.na(report_date) & 
                                             report_date < tank_closed_date & 
                                             !is.na(nfa_date) & nfa_date > tank_closed_date, 1, 0),
    no_leak = ifelse(is.na(report_date) & !is.na(tank_closed_date), 1, 0)
  ) %>%
  as.data.table()

# Aggregate leak indicators at facility-tank level
LA_UST_tanks_SD <- LA_UST_tanks_LUST %>%
  group_by(facility_id, tank_id, tank_installed_date, tank_closed_date, county_name) %>%
  summarize(
    facility_name = first(facility_name),
    tank_status = first(tank_status), # Keep original logic
    leak_after_closure = sum(leak_after_closure, na.rm = TRUE),
    leak_before_NFA_before_closure = sum(leak_before_NFA_before_closure, na.rm = TRUE),
    leak_before_NFA_after_closure = sum(leak_before_NFA_after_closure, na.rm = TRUE),
    no_leak = sum(no_leak, na.rm = TRUE),
    # Take maximum value for binary flags
    is_gasoline = max(is_gasoline, na.rm = TRUE),
    is_diesel = max(is_diesel, na.rm = TRUE),
    is_oil_kerosene = max(is_oil_kerosene, na.rm = TRUE),
    is_jet_fuel = max(is_jet_fuel, na.rm = TRUE),
    is_other = max(is_other, na.rm = TRUE),
    single_walled = max(single_walled, na.rm = TRUE),
    double_walled = max(double_walled, na.rm = TRUE),
    # Consolidate missing into unknown
    unknown_walled = max(unknown_walled, missing_walled, na.rm = TRUE),
    capacity = mean(capacity, na.rm = TRUE)
  ) %>%
  as.data.table()

# Convert binary indicators to 0/1
LA_UST_tanks_SD[, `:=`(
  leak_after_closure = ifelse(leak_after_closure > 0, 1, 0),
  leak_before_NFA_before_closure = ifelse(leak_before_NFA_before_closure > 0, 1, 0),
  leak_before_NFA_after_closure = ifelse(leak_before_NFA_after_closure > 0, 1, 0),
  no_leak = ifelse(no_leak > 0, 1, 0),
  state = "LA",
  facility_name = stringr::str_to_title(trimws(gsub("[^[:alnum:] ]", "", facility_name))),
  facility_id = as.character(facility_id),
  tank_id = as.character(tank_id)
)]

# Add county FIPS codes
cat("\nAdding county FIPS codes...\n")

# Standardize county names
LA_UST_tanks_SD[, standardized_county := standardize_county_name(county_name)]

# Load county FIPS code reference data
county_fips <- fread(here("Data", "Processed", "county_fips_codes.csv"))

# Standardize county names in reference data
county_fips[, standardized_county := standardize_county_name(county_name)]

# Filter county FIPS data for Louisiana first
LA_county_fips <- county_fips[state_fips == 22]

# Join FIPS codes based on standardized county name using standard merge
LA_UST_tanks_SD <- merge(
  LA_UST_tanks_SD,
  LA_county_fips,
  by = "standardized_county",
  all.x = TRUE  # Keep all records from the tank data
)

# Clean up temp column
LA_UST_tanks_SD[, standardized_county := NULL]

# Define required columns order
required_columns <- c(
  "facility_id", "facility_name", "tank_id", "state", "tank_installed_date", "tank_closed_date", "tank_status",
  "leak_after_closure", "leak_before_NFA_before_closure", "leak_before_NFA_after_closure", "no_leak",
  "capacity", "single_walled", "double_walled", "unknown_walled",
  "is_gasoline", "is_diesel", "is_oil_kerosene", "is_jet_fuel", "is_other", "county_name"
)

cols_to_keep <- intersect(names(LA_UST_tanks_SD), c(required_columns, "county_fips"))
LA_UST_tanks_SD <- LA_UST_tanks_SD[, ..cols_to_keep]

# Save final datasets
cat("\nSaving final Louisiana datasets...\n")

# Create directory if it doesn't exist
output_dir <- here("Data","Raw","state_databases","Louisiana")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# Write tank data
fwrite(LA_UST_tanks_SD, file.path(output_dir, "LA_Harmonized_UST_tanks.csv"))

# Write LUST data
fwrite(LA_LUST_SD, file.path(output_dir, "LA_Harmonized_LUST.csv"))

cat("\nLouisiana data processing complete\n")
cat("Tank records:", nrow(LA_UST_tanks_SD), "\n")
cat("LUST records:", nrow(LA_LUST_SD), "\n")