## New Jersey UST Dataset Creation

# Define the county variable name in the raw data
COUNTYVARIABLE <- "county"

library(here)
first_run = TRUE
#load packages ----
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
classify_tank_walls <- function(data, tank_wall_col = "tank_structure") {
  result <- copy(data)
  
  # Create binary variables for tank wall type based on tank_structure column
  # For New Jersey, anything not single walled is considered double walled
  result[, `:=`(
    double_walled = as.integer(grepl("Double Wall|Secondary Containment|Triple Wall", get(tank_wall_col), ignore.case = TRUE)),
    single_walled = as.integer(grepl("Single Wall", get(tank_wall_col), ignore.case = TRUE)),
    unknown_walled = as.integer(is.na(get(tank_wall_col)) | get(tank_wall_col) == ""),
    missing_walled = as.integer(is.na(get(tank_wall_col)))
  )]
  
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
classify_substances <- function(data, substance_col_name = "tank_contents") {
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
standardize_capacity <- function(data, capacity_col = "tank_volume") {
  result <- copy(data)
  
  result[, `:=`(
    capacity = as.numeric(gsub("[^0-9.]", "", get(capacity_col)))
  )]
  
  return(result)
}

# State name standardization -----
# Create a mapping dictionary between abbreviations and full state names using R's built-in state data
data(state)
state_name_mapping <- setNames(state.name, state.abb)

# Add any missing territories or DC if needed
state_name_mapping["DC"] <- "District of Columbia"
state_name_mapping["PR"] <- "Puerto Rico"
state_name_mapping["VI"] <- "Virgin Islands"
state_name_mapping["GU"] <- "Guam"
state_name_mapping["AS"] <- "American Samoa"
state_name_mapping["MP"] <- "Northern Mariana Islands"

# Function to standardize state names and add abbreviations using data.table for efficiency
standardize_states <- function(dt) {
  if ("state" %in% names(dt)) {
    # Create a temporary copy of state column
    dt[, state_original := state]
    
    # Create reverse mapping for full state names to abbreviations
    reverse_mapping <- setNames(names(state_name_mapping), state_name_mapping)
    
    # Create a mapping table as a data.table for efficient joins
    mapping_dt <- data.table(
      abbr = names(state_name_mapping),
      full_name = unlist(state_name_mapping)
    )
    reverse_dt <- data.table(
      full_name = names(reverse_mapping),
      abbr = unlist(reverse_mapping)
    )
    
    # Convert two-letter codes to full names using joins
    # First identify which rows need conversion (where state is in abbr column)
    dt[state %in% mapping_dt$abbr, state := mapping_dt[.(state), full_name, on = "abbr"]]
    
    # Create state_abr column from original state
    dt[, state_abr := state_original]
    
    # For full names that don't have abbreviations yet, find their abbreviation
    dt[state %in% reverse_dt$full_name & !(state_abr %in% mapping_dt$abbr),
       state_abr := reverse_dt[.(state), abbr, on = "full_name"]]
    
    # Remove the temporary column
    dt[, state_original := NULL]
  }
  return(dt)
}

# Load Raw Data - New Jersey files -----
cat("\nLoading New Jersey UST data...\n")

# Load the New Jersey tank data
NJ_tanks_raw <- fread("C:/Users/kaleb/Box/UST-Insurance/Data/Raw_do_not_write/state_databases/New Jersey/New Jersey Tanks Data.csv") %>%
  janitor::clean_names() %>%
  as.data.table()

# Load the facility data which contains county information
NJ_facility_raw <- fread("C:/Users/kaleb/Box/UST-Insurance/Data/Raw_do_not_write/state_databases/New Jersey/New Jersey Facility Data.csv") %>%
  janitor::clean_names() %>%
  as.data.table()

# Load the EPA LUST data and filter for New Jersey
NJ_lust_raw <- fread(here("Data","Raw_do_not_write","epa_ust_data_1_8_2024","Releases_1_edited.csv")) %>%
  janitor::clean_names() %>%
  filter(state == "New Jersey") %>%
  as.data.table()

# Print summaries of loaded data
cat("\nLoaded", nrow(NJ_tanks_raw), "tank records,", nrow(NJ_facility_raw), "facility records, and", nrow(NJ_lust_raw), "LUST records\n")
cat("\nTank data column names:", paste(names(NJ_tanks_raw), collapse = ", "), "\n")
cat("\nFacility data column names:", paste(names(NJ_facility_raw), collapse = ", "), "\n")
cat("\nLUST data column names:", paste(names(NJ_lust_raw), collapse = ", "), "\n")

# Process Tank Data -----
cat("\nProcessing New Jersey tank data...\n")

# Add county information from facility data to tank data
# First ensure preferred_id is character type in both dataframes
NJ_facility_raw[, preferred_id := as.character(preferred_id)]
NJ_tanks_raw[, preferred_id := as.character(preferred_id)]
NJ_tanks_raw[, state:= "New Jersey"]
NJ_tanks_raw[, state_abbr := "NJ"]
# Join facility data to get county information
NJ_UST_tanks <- NJ_tanks_raw %>%
  left_join(
    NJ_facility_raw %>% select(preferred_id, county),
    by = "preferred_id"
  ) %>%
  as.data.table()

# Process tanks based on status description
cat("\nProcessing tank status...\n")

# Classify tank status based on Tank Status Description as per instructions
NJ_UST_tanks[, tank_status := case_when(
  # Open tanks
  tolower(tank_status_description) == "in-use" ~ "Open",
  # Closed tanks
  tolower(tank_status_description) %in% c("abandoned in place", "out of service", "removed") ~ "Closed",
  # Ignore these categories
  tolower(tank_status_description) %in% c("emergency backup generator tank", "other", "pending-noi", "sump") ~ "Ignore",
  TRUE ~ NA_character_
)]

# Filter out tanks with "Ignore" status
NJ_UST_tanks <- NJ_UST_tanks[tank_status != "Ignore"]

# Select and rename columns according to the required format
NJ_UST_tanks <- NJ_UST_tanks %>%
  select(
    preferred_id,         # facility_id
    tank_number,          # tank_id
    installed_date,       # tank_installed_date
    out_of_service_date,  # tank_closed_date (for closed tanks)
    county,               # county_name
    tank_volume,          # capacity
    tank_contents,        # substance
    tank_structure,       # tank wall type
    tank_status,           # processed status
    state,
    state_abbr
  ) %>%
  rename(
    facility_id = preferred_id,
    tank_id = tank_number,
    tank_installed_date = installed_date,
    tank_closed_date = out_of_service_date,
    county_name = county
  ) %>%
  as.data.table()

# Convert dates to proper format
NJ_UST_tanks[, tank_installed_date := as.Date(tank_installed_date, format = "%m/%d/%Y")]
NJ_UST_tanks[, tank_closed_date := as.Date(tank_closed_date, format = "%m/%d/%Y")]

# For closed tanks without a close date, set to NA and print a warning
missing_closed_dates <- sum(NJ_UST_tanks$tank_status == "Closed" & is.na(NJ_UST_tanks$tank_closed_date))
if(missing_closed_dates > 0) {
  cat("\nWARNING:", missing_closed_dates, "closed tanks have missing closure dates\n")
}

# Apply standardization functions
cat("\nStandardizing capacity values...\n")
NJ_UST_tanks <- standardize_capacity(NJ_UST_tanks, "tank_volume")

cat("\nClassifying tank wall types...\n")
NJ_UST_tanks <- classify_tank_walls(NJ_UST_tanks, "tank_structure")

cat("\nClassifying substance types...\n")
NJ_UST_tanks <- classify_substances(NJ_UST_tanks, "tank_contents")

# Add county FIPS codes - MOVED UP HERE
cat("\nAdding county FIPS codes...\n")

# Standardize county names
NJ_UST_tanks[, standardized_county := standardize_county_name(county_name)]

# Load county FIPS code reference data
county_fips <- fread(here("Data", "Processed", "county_fips_codes.csv"))

# Standardize county names in reference data
county_fips[, standardized_county := standardize_county_name(county_name)]
setDT(county_fips)
# Filter county FIPS data for New Jersey
NJ_county_fips <- county_fips[state_fips ==34 ]

# Join FIPS codes based on standardized county name
NJ_UST_tanks <- merge(
  NJ_UST_tanks,
  NJ_county_fips %>% select(standardized_county, county_fips_code) %>% as.data.table(),
  by = "standardized_county",
  all.x = TRUE
)

# Report on the match rate
cat("\nCounty FIPS code matching summary:\n")
cat("Total facilities:", uniqueN(NJ_UST_tanks$facility_id), "\n")
cat("Facilities with county FIPS codes:", sum(!is.na(NJ_UST_tanks$county_fips)), "\n")
cat("Match rate:", round(sum(!is.na(NJ_UST_tanks$county_fips)) / uniqueN(NJ_UST_tanks$facility_id) * 100, 2), "%\n")

# Clean up temp column
NJ_UST_tanks[, standardized_county := NULL]
NJ_UST_tanks[, county_fips := county_fips_code]
NJ_UST_tanks[,county_fips_code := NULL]

# Process LUST Data -----
cat("\nProcessing New Jersey LUST data...\n")

# Handle the "NJ" prefix in facility_id as per instructions
NJ_LUST_SD <- NJ_lust_raw %>%
  mutate(
    # Remove "NJ" prefix from facility_id to match tank data
    facility_id = gsub("^NJ", "", facility_id),
    LUST_id = lust_id,
    report_date = mdy_hm(reported_date),
    nfa_date = NA  # NFA date not available in raw data
  ) %>%
  select(facility_id, LUST_id, report_date, nfa_date) %>%
  as.data.table()

# Join LUST data with tank data
cat("\nJoining LUST data with tank data...\n")

NJ_LUST_SD_collapsed <- NJ_LUST_SD[, .(LUST_count = .N), by = .(facility_id, report_date,nfa_date)]

NJ_UST_tanks_LUST <- merge(
  NJ_UST_tanks, 
  NJ_LUST_SD_collapsed[, .(facility_id,LUST_count, report_date, nfa_date)],
  by = "facility_id",
  all.x = TRUE,
  allow.cartesian = TRUE
)

# Create leak indicators
NJ_UST_tanks_LUST <- NJ_UST_tanks_LUST %>%
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

# Aggregate leak indicators at facility-tank level - Now including county_fips
NJ_UST_tanks_SD <- NJ_UST_tanks_LUST %>%
  group_by(facility_id, tank_id, tank_installed_date, tank_closed_date, county_name, county_fips, state, state_abbr) %>%
  summarize(
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
    unknown_walled = max(unknown_walled, na.rm = TRUE),
    missing_walled = max(missing_walled, na.rm = TRUE),
    capacity = mean(capacity, na.rm = TRUE)
  ) %>%
  as.data.table()

# Convert binary indicators to 0/1 - Now including county_fips
NJ_UST_tanks_SD <- NJ_UST_tanks_SD[, .(
  leak_after_closure = ifelse(leak_after_closure > 0, 1, 0),
  leak_before_NFA_before_closure = ifelse(leak_before_NFA_before_closure > 0, 1, 0),
  leak_before_NFA_after_closure = ifelse(leak_before_NFA_after_closure > 0, 1, 0),
  no_leak = ifelse(no_leak > 0, 1, 0),
  is_gasoline = is_gasoline,
  is_diesel = is_diesel,
  is_oil_kerosene = is_oil_kerosene,
  is_jet_fuel = is_jet_fuel,
  is_other = is_other,
  single_walled = single_walled,
  double_walled = double_walled,
  unknown_walled = unknown_walled,
  missing_walled = missing_walled,
  capacity = capacity
), keyby = .(facility_id, tank_id, tank_installed_date, tank_closed_date, county_name, county_fips, state, state_abbr)]


# Save final datasets
cat("\nSaving final New Jersey datasets...\n")

# Create directory if it doesn't exist
dir_path <- here("Data", "Raw_do_not_write", "state_databases", "New Jersey")
if (!dir.exists(dir_path)) {
  dir.create(dir_path, recursive = TRUE)
}

# Write tank data
fwrite(NJ_UST_tanks_SD, here("Data", "Raw_do_not_write", "state_databases", "New Jersey", "NJ_UST_tanks.csv"))

# Write LUST data
fwrite(NJ_LUST_SD, here("Data", "Raw_do_not_write", "state_databases", "New Jersey", "NJ_UST_LUST.csv"))

cat("\nNew Jersey data processing complete\n")
cat("Tank records:", nrow(NJ_UST_tanks_SD), "\n")
cat("LUST records:", nrow(NJ_LUST_SD), "\n")

# Count closed tanks
num_closed_tanks_NJ = uniqueN(NJ_UST_tanks_SD[!is.na(tank_closed_date), paste0(facility_id, "-", tank_id)])
cat("Closed tanks:", num_closed_tanks_NJ, "\n")

# Count leaks
count_leak_after_closure = NJ_UST_tanks_SD[leak_after_closure == 1, uniqueN(paste0(facility_id, "-", tank_id))]
count_leak_before_NFA_before_closure = NJ_UST_tanks_SD[leak_before_NFA_before_closure == 1, uniqueN(paste0(facility_id, "-", tank_id))]
count_leak_before_NFA_after_closure = NJ_UST_tanks_SD[leak_before_NFA_after_closure == 1, uniqueN(paste0(facility_id, "-", tank_id))]
count_no_leak = NJ_UST_tanks_SD[no_leak == 1, uniqueN(paste0(facility_id, "-", tank_id))]

NJ_counts = data.frame(
  category = c("leak_after_closure", "leak_before_NFA_before_closure", "leak_before_NFA_after_closure", "no_leak"),
  count = c(count_leak_after_closure, count_leak_before_NFA_before_closure, count_leak_before_NFA_after_closure, count_no_leak)
)
NJ_counts$total = sum(NJ_counts$count)
NJ_counts$percent = (NJ_counts$count / NJ_counts$total) * 100

cat("\nNew Jersey leak statistics:\n")
print(NJ_counts)
