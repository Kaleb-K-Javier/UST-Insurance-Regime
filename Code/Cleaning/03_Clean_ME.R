## Maine UST Dataset Creation

# Define the county variable name in the raw data
COUNTYVARIABLE <- "county"

library(here)
first_run = TRUE
#load packages ----
source(here('Code','00_global_packages.R'))

# Function to standardize county names
standardize_county_name <- function(county_name) {
  if(is.na(county_name) || county_name == "") return(NA)
  name <- tolower(county_name)
  name <- gsub(" county$| counties$| parish$| parishes$", "", name)
  name <- gsub("[^a-z ]", "", name)
  name <- trimws(gsub("\\s+", " ", name))
  return(name)
}

# Function to classify tank wall types
classify_tank_walls <- function(data, tank_wall_col = "const_") {
  result <- copy(data)
  result[, `:=`(
    double_walled = as.integer(grepl("DOUBLE[-]?WALL|DBL[-]?WALL|DUAL[-]?WALL", 
                                    toupper(get(tank_wall_col)), ignore.case = TRUE)),
    single_walled = as.integer(!is.na(get(tank_wall_col)) & 
                              get(tank_wall_col) != "" & 
                              !grepl("DOUBLE[-]?WALL|DBL[-]?WALL|DUAL[-]?WALL", 
                                     toupper(get(tank_wall_col)), ignore.case = TRUE)),
    unknown_walled = 0,  # Maine doesn't have unknown wall types
    missing_walled = as.integer(is.na(get(tank_wall_col)) | get(tank_wall_col) == "")
  )]
  return(result)
}

# Function to classify substances
classify_substances <- function(data, substance_col_name = "prod_") {
  result <- copy(data)
  result[, `:=`(is_gasoline = 0, is_diesel = 0, is_oil_kerosene = 0, is_jet_fuel = 0, is_other = 1)]
  
  gasoline_patterns <- paste(c("gasoline unspecified", "leaded gasoline", "premium unleaded", "regular gasoline", "unleaded gasoline", "unleaded plus"), collapse = "|")
  diesel_patterns <- paste(c("diesel", "bio 1-74", "bio 75-99"), collapse = "|")
  oil_kerosene_patterns <- paste(c("#1 fuel oil - kerosene", "#2 fuel oil", "#5 fuel oil", "#6 fuel oil", "hydraulic oil", "lube oil", "oil - other - specified in report", "unspecified oil", "waste oil/used motor oil"), collapse = "|")
  jet_fuel_patterns <- paste(c("aviation gasoline", "jet fuel", "jp1", "jp4"), collapse = "|")
  
  result[, substances_lower := tolower(get(substance_col_name))]
  result[!is.na(substances_lower) & substances_lower != "", is_gasoline := as.integer(grepl(gasoline_patterns, substances_lower, perl = TRUE))]
  result[!is.na(substances_lower) & substances_lower != "" & !grepl("def|diesel exhaust fluid", substances_lower, perl = TRUE), is_diesel := as.integer(grepl(diesel_patterns, substances_lower, perl = TRUE))]
  result[!is.na(substances_lower) & substances_lower != "", is_oil_kerosene := as.integer(grepl(oil_kerosene_patterns, substances_lower, perl = TRUE))]
  result[!is.na(substances_lower) & substances_lower != "", is_jet_fuel := as.integer(grepl(jet_fuel_patterns, substances_lower, perl = TRUE))]
  result[, is_other := as.integer(is_gasoline + is_diesel + is_oil_kerosene + is_jet_fuel == 0)]
  result[, substances_lower := NULL]
  return(result)
}

# Function to standardize capacity
standardize_capacity <- function(data, capacity_col = "cap_") {
  result <- copy(data)
  result[, `:=`(capacity = as.numeric(gsub("[^0-9.]", "", get(capacity_col))))]
  return(result)
}

# Download and Load Raw Data - Maine files -----
cat("\nDownloading Maine UST data...\n")
temp_file <- tempfile()
download.file("https://www.maine.gov/dep/ftp/tanks/tanks_owner_all_registered_tanks.txt", destfile = temp_file, mode = "wb")

# Read the file directly - Force character cols
ME_tanks_raw <- fread(temp_file, sep = "*", header = TRUE, fill = TRUE, 
                      colClasses = c(registration_number = "character", tank_number = "character")) %>%
  janitor::clean_names() %>%
  as.data.table()

# LUST Data Loading -----
cat("\nDownloading and processing Maine LUST data...\n")
base_url <- "https://www.maine.gov/dep/ftp/hoss/"
tank_file         <- "report_tank_involved.txt"
spill_report_file <- "spill_report.txt"
spill_log_file    <- "spill_log.txt"

# [Keep existing safe_fread function - it is correct]
safe_fread <- function(file_url, file_name) {
  tryCatch({
    temp_file <- tempfile()
    download_url <- paste0(file_url, file_name)
    download.file(download_url, destfile = temp_file, mode = "wb", quiet = TRUE)
    all_lines <- readLines(temp_file, warn = FALSE)
    total_lines <- length(all_lines)
    
    if(file_name == "spill_report.txt" && total_lines >= 70340) {
      clean_file <- tempfile()
      writeLines(all_lines[c(1:70339, 70341:total_lines)], clean_file)
      dt <- fread(clean_file, sep = "*", header = FALSE, fill = TRUE, quote = "", data.table = TRUE, showProgress = FALSE, encoding = "Latin-1")
      return(dt)
    }
    dt <- fread(temp_file, sep = "*", header = FALSE, fill = TRUE, quote = "", data.table = TRUE, showProgress = FALSE, encoding = "Latin-1")
    return(dt)
  }, error = function(e) { return(data.table()) })
}

tank_raw_full         <- safe_fread(base_url, tank_file)
spill_report_raw_full <- safe_fread(base_url, spill_report_file)
spill_log_raw_full    <- safe_fread(base_url, spill_log_file)

tank_raw <- tank_raw_full[, .(l_spill_number = V1, ust_registered_flag = V5, UST_TANK_SITE_NUMBER = V6)]
spill_report_raw <- spill_report_raw_full[, .(l_spill_number = V1, modify_date = V4, report_status = V6, report_status_value = V7)]
spill_log_raw <- spill_log_raw_full[, .(l_spill_number = V1, log_reported_datetime = V16)]

# Filter and Merge Data
tank_federal <- tank_raw[ust_registered_flag == "T"]
spill_report_final <- spill_report_raw[report_status_value %in% c("Final Report", "FR")]

merged1 <- merge(tank_federal, spill_log_raw[, .(l_spill_number, log_reported_datetime)], by = "l_spill_number", all.x = TRUE)
merged2 <- merge(merged1, spill_report_final[, .(l_spill_number, modify_date)], by = "l_spill_number", all.x = TRUE)

ME_lust_raw <- merged2[, .(
  facility_id   = UST_TANK_SITE_NUMBER, 
  lust_id       = l_spill_number, 
  reported_date = log_reported_datetime, 
  nfa_date      = modify_date 
)] %>% clean_names() %>% as.data.table()

# Process Tank Data -----
cat("\nProcessing Maine tank data...\n")

# Select and rename columns according to the required format
# Force character IDs
ME_UST_tanks <- ME_tanks_raw %>%
  mutate(
    registration_number = as.character(registration_number),
    tank_number = as.character(tank_number)
  ) %>%
  select(
    registration_number,           
    site_name,                     
    tank_number,                   
    date_tank_installed,           
    tank_status_date,              
    tank_volume_in_gallons,        
    product_stored,                
    tank_material_label,           
    tank_status_label              
  ) %>%
  rename(
    facility_id = registration_number,
    facility_name = site_name,
    tank_id = tank_number,
    tank_installed_date = date_tank_installed,
    tank_closed_date = tank_status_date,
    capacity = tank_volume_in_gallons,
    tank_contents = product_stored,
    tank_structure = tank_material_label,
    status = tank_status_label
  ) %>%
  as.data.table()

# Add state columns
ME_UST_tanks[, state := "Maine"]
ME_UST_tanks[, state_abbr := "ME"]

# Convert dates 
ME_UST_tanks[, tank_installed_date := lubridate::mdy(tank_installed_date)]
ME_UST_tanks[, tank_closed_date := lubridate::mdy(tank_closed_date)]

# Process tanks based on status description (Specific Logic)
cat("\nProcessing tank status...\n")
ME_UST_tanks <- ME_UST_tanks[status != "PLANNED FOR INSTALLATION"]
ME_UST_tanks[, tank_status := "Open"]  # Default
ME_UST_tanks[grepl("Removed|Closed|Out of Service|Abandoned", status, ignore.case = TRUE), tank_status := "Closed"]

# Apply standardization functions
ME_UST_tanks <- standardize_capacity(ME_UST_tanks, "capacity")
ME_UST_tanks <- classify_tank_walls(ME_UST_tanks, "tank_structure")
ME_UST_tanks <- classify_substances(ME_UST_tanks, "tank_contents")

# Add county FIPS codes
ME_UST_tanks[, facility_id_census := paste0("ME", facility_id)]
census_file_path <- Sys.glob(here("Data", "Processed", "Census_Geography", "Maine_census_geography.csv"))
census_data <- fread(census_file_path[1])
census_data[, facility_id := as.character(facility_id)]

ME_UST_tanks <- merge(
  ME_UST_tanks,
  census_data[, .(facility_id,county_name ,county_geoid)],
  by.x = "facility_id_census",
  by.y = "facility_id",
  all.x = TRUE
)
setnames(ME_UST_tanks, "county_geoid", "county_fips")

# Process LUST Data -----
cat("\nProcessing Maine LUST data...\n")

ME_LUST_SD <- ME_lust_raw %>%
  mutate(
    facility_id = as.character(facility_id),
    LUST_id = as.character(lust_id),
    report_date = lubridate::mdy(reported_date),
    nfa_date = lubridate::mdy(nfa_date),
    state = "ME"
  ) %>%
  select(facility_id, LUST_id, report_date, nfa_date, state) %>%
  as.data.table()

# Join LUST data with tank data
ME_LUST_SD_collapsed <- ME_LUST_SD[, .(LUST_count = .N), by = .(facility_id, report_date, nfa_date)]
ME_UST_tanks[,facility_id:=as.character(facility_id)] # Ensure matching type for join
ME_UST_tanks_LUST <- merge(
  ME_UST_tanks, 
  ME_LUST_SD_collapsed[, .(facility_id, LUST_count, report_date, nfa_date)],
  by = "facility_id",
  all.x = TRUE,
  allow.cartesian = TRUE
)

# Create leak indicators
ME_UST_tanks_LUST <- ME_UST_tanks_LUST %>%
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
ME_UST_tanks_SD <- ME_UST_tanks_LUST %>%
  group_by(facility_id, tank_id, tank_installed_date, tank_closed_date, county_name, county_fips, state, state_abbr) %>%
  summarize(
    facility_name = first(facility_name),
    tank_status = first(tank_status), # Keep Specific Logic
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

# Convert binary indicators to 0/1 and clean final data
ME_UST_tanks_SD[, `:=`(
  leak_after_closure = ifelse(leak_after_closure > 0, 1, 0),
  leak_before_NFA_before_closure = ifelse(leak_before_NFA_before_closure > 0, 1, 0),
  leak_before_NFA_after_closure = ifelse(leak_before_NFA_after_closure > 0, 1, 0),
  no_leak = ifelse(no_leak > 0, 1, 0),
  facility_name = stringr::str_to_title(trimws(gsub("[^[:alnum:] ]", "", facility_name))),
  facility_id = as.character(facility_id),
  tank_id = as.character(tank_id)
)]

# Define required columns order
required_columns <- c(
  "facility_id", "facility_name", "tank_id", "state", "tank_installed_date", "tank_closed_date", "tank_status",
  "leak_after_closure", "leak_before_NFA_before_closure", "leak_before_NFA_after_closure", "no_leak",
  "capacity", "single_walled", "double_walled", "unknown_walled",
  "is_gasoline", "is_diesel", "is_oil_kerosene", "is_jet_fuel", "is_other", "county_name"
)

cols_to_keep <- intersect(names(ME_UST_tanks_SD), c(required_columns, "county_fips"))
ME_UST_tanks_SD <- ME_UST_tanks_SD[, ..cols_to_keep]

# Save final datasets
cat("\nSaving final Maine datasets...\n")
output_dir <- here("Data","Raw","state_databases","Maine")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

fwrite(ME_UST_tanks_SD, file.path(output_dir, "ME_Harmonized_UST_tanks.csv"))
fwrite(ME_LUST_SD, file.path(output_dir, "ME_Harmonized_LUST.csv"))