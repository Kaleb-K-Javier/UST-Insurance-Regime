## Michigan UST Dataset Creation

# Define the county variable name in the raw data
COUNTYVARIABLE <- "county_name"

library(here)
first_run = TRUE
#load packages ----
source(here('Code','00_global_packages.R'))
library(readxl)

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
classify_tank_walls <- function(data, tank_wall_col = "tank_wall_type") {
  result <- copy(data)
  result[, `:=`(
    double_walled = as.integer(grepl("Double Walled|Polyethylene Tank Jacket|Tank Jacket|Excavation Liner", 
                                   get(tank_wall_col), ignore.case = TRUE)),
    single_walled = as.integer((!grepl("Double Walled|Polyethylene Tank Jacket|Tank Jacket|Excavation Liner", 
                                     get(tank_wall_col), ignore.case = TRUE) &
                              !grepl("Unknown", get(tank_wall_col), ignore.case = TRUE) &
                              !is.na(get(tank_wall_col)) & 
                              get(tank_wall_col) != "")),
    unknown_walled = as.integer(grepl("Unknown", get(tank_wall_col), ignore.case = TRUE) & 
                              !is.na(get(tank_wall_col))),
    missing_walled = as.integer(is.na(get(tank_wall_col)) | get(tank_wall_col) == "")
  )]  
  return(result)
}

# Function to classify substances
classify_substances <- function(data, substance_col_name = "substance") {
  result <- copy(data)
  result[, `:=`(is_gasoline = 0, is_diesel = 0, is_oil_kerosene = 0, is_jet_fuel = 0, is_other = 1)]
  # ... [Regex Patterns Omitted for Brevity - assume standard MI patterns] ...
  gasoline_patterns <- "gasoline|unleaded|premium|regular|ethanol|e85"
  diesel_patterns <- "diesel|biodiesel"
  oil_kerosene_patterns <- "oil|kerosene|waste oil"
  jet_fuel_patterns <- "jet|aviation"
  
  result[, substances_lower := tolower(get(substance_col_name))]
  result[!is.na(substances_lower), is_gasoline := as.integer(grepl(gasoline_patterns, substances_lower))]
  result[!is.na(substances_lower), is_diesel := as.integer(grepl(diesel_patterns, substances_lower))]
  result[!is.na(substances_lower), is_oil_kerosene := as.integer(grepl(oil_kerosene_patterns, substances_lower))]
  result[!is.na(substances_lower), is_jet_fuel := as.integer(grepl(jet_fuel_patterns, substances_lower))]
  result[, is_other := as.integer(is_gasoline + is_diesel + is_oil_kerosene + is_jet_fuel == 0)]
  result[, substances_lower := NULL]
  return(result)
}

# Function to standardize capacity
standardize_capacity <- function(data, capacity_col = "tank_capacity") {
  result <- copy(data)
  result[, `:=`(capacity = as.numeric(gsub("[^0-9.]", "", get(capacity_col))))]
  return(result)
}

# Function to read Michigan Excel file sheets
read_michigan_sheets <- function(file_path) {
  sheet_names <- excel_sheets(file_path)
  all_sheets <- list()
  sheet_indices <- setdiff(1:length(sheet_names), c(6, 9))
  
  for (i in sheet_indices) {
    header_data <- read_excel(file_path, sheet = i, n_max = 2, col_names = FALSE)
    combined_header <- character(ncol(header_data))
    for (j in 1:ncol(header_data)) {
      header_part1 <- ifelse(!is.na(header_data[1, j]), as.character(header_data[1, j]), "")
      header_part2 <- ifelse(!is.na(header_data[2, j]), as.character(header_data[2, j]), "")
      combined_header[j] <- paste(header_part1, header_part2, sep = " ")
    }
    combined_header <- trimws(combined_header)
    sheet_data <- read_excel(file_path, sheet = i, skip = 3, col_names = FALSE)
    colnames(sheet_data) <- combined_header
    all_sheets[[i]] <- sheet_data
  }
  return(rbindlist(all_sheets, fill = TRUE))
}

# Load Raw Data - Michigan files -----
cat("\nLoading Michigan UST data...\n")
michigan_file_path <- here("Data","Raw","state_databases","Michigan","UTK Master List 3-3-25.xlsx")
michigan_raw <- read_michigan_sheets(michigan_file_path)

if (!is.null(michigan_raw) && nrow(michigan_raw) > 0) {
  cat("\nSuccessfully loaded Michigan raw data\n")
  
  michigan_raw = michigan_raw %>% janitor::clean_names()
  
  # Create column mapping
  mi_columns <- c(
    "facility_id" = "facility_id",
    "facility_name" = "facility_name", # Verify name in raw
    "facility_county" = "county_name", 
    "details_status" = "tank_status",
    "new_tank_id_number" = "tank_id",
    "tank_capacity" = "tank_capacity",
    "tank_instal_date" = "tank_installed_date",
    "tank_content" = "substance",
    "tank_removal_date" = "tank_remove_date",
    "tank_construction" = "tank_wall_type"
  )
  
  cols_present <- names(mi_columns)[names(mi_columns) %in% names(michigan_raw)]
  MI_UST_tanks <- michigan_raw[, ..cols_present]
  setnames(MI_UST_tanks, old = cols_present, new = mi_columns[cols_present])
  
  # Add state identifiers
  MI_UST_tanks[, `:=`(state = "Michigan", state_abbr = "MI")]
  
  # Filter out non-registered (Original Status Logic)
  MI_UST_tanks <- MI_UST_tanks[!grepl("NON-Registered Tank|Temporarily Out of Use", tank_status, ignore.case = TRUE)]
  
  # Force IDs to char
  MI_UST_tanks[, facility_id := as.character(facility_id)]
  MI_UST_tanks[, tank_id := as.character(tank_id)]
  
  # Dates
  MI_UST_tanks[, tank_installed_date := lubridate::mdy(tank_installed_date)]
  MI_UST_tanks[, tank_closed_date := lubridate::mdy(tank_remove_date)]
  MI_UST_tanks[, tank_remove_date := NULL]
  
  # Classify
  MI_UST_tanks <- standardize_capacity(MI_UST_tanks, "tank_capacity")
  MI_UST_tanks <- classify_tank_walls(MI_UST_tanks, "tank_wall_type")
  MI_UST_tanks <- classify_substances(MI_UST_tanks, "substance")
  
  # Process LUST Data -----
  cat("\nProcessing Michigan LUST data...\n")
  MI_lust_raw <- fread(here("Data", "Raw", "Releases.csv")) %>%
    janitor::clean_names() %>%
    filter(state == "Michigan") %>%
    as.data.table()
  
  MI_LUST_SD <- MI_lust_raw %>%
    mutate(
      facility_id = as.character(gsub("^MI", "", facility_id)),
      LUST_id = as.character(lust_id),
      report_date = mdy_hm(reported_date),
      nfa_date = as.Date(NA),
      state = "MI"
    ) %>%
    select(facility_id, LUST_id, report_date, nfa_date, state) %>%
    as.data.table()
  
  # Join
  MI_LUST_SD_collapsed <- MI_LUST_SD[, .(LUST_count = .N), by = .(facility_id, report_date, nfa_date)]
  MI_UST_tanks_LUST <- merge(
    MI_UST_tanks, 
    MI_LUST_SD_collapsed[, .(facility_id, LUST_count, report_date, nfa_date)],
    by = "facility_id",
    all.x = TRUE,
    allow.cartesian = TRUE
  )
  
  # Create leak indicators
  MI_UST_tanks_LUST <- MI_UST_tanks_LUST %>%
    mutate(
      leak_after_closure = ifelse(!is.na(tank_closed_date) & !is.na(report_date) & report_date > tank_closed_date, 1, 0),
      leak_before_NFA_before_closure = ifelse(!is.na(tank_closed_date) & !is.na(report_date) & report_date < tank_closed_date & (is.na(nfa_date) | nfa_date < tank_closed_date), 1, 0),
      leak_before_NFA_after_closure = ifelse(!is.na(tank_closed_date) & !is.na(report_date) & report_date < tank_closed_date & !is.na(nfa_date) & nfa_date > tank_closed_date, 1, 0),
      no_leak = ifelse(is.na(report_date) & !is.na(tank_closed_date), 1, 0)
    ) %>%
    as.data.table()
  
  # Aggregate
  MI_UST_tanks_SD <- MI_UST_tanks_LUST %>%
    group_by(facility_id, tank_id, tank_installed_date, tank_closed_date, county_name, state, state_abbr) %>%
    summarize(
      facility_name = first(facility_name),
      tank_status = first(tank_status), # Preserve filtered status
      leak_after_closure = sum(leak_after_closure, na.rm = TRUE),
      leak_before_NFA_before_closure = sum(leak_before_NFA_before_closure, na.rm = TRUE),
      leak_before_NFA_after_closure = sum(leak_before_NFA_after_closure, na.rm = TRUE),
      no_leak = sum(no_leak, na.rm = TRUE),
      is_gasoline = max(is_gasoline, na.rm = TRUE),
      is_diesel = max(is_diesel, na.rm = TRUE),
      is_oil_kerosene = max(is_oil_kerosene, na.rm = TRUE),
      is_jet_fuel = max(is_jet_fuel, na.rm = TRUE),
      is_other = max(is_other, na.rm = TRUE),
      single_walled = max(single_walled, na.rm = TRUE),
      double_walled = max(double_walled, na.rm = TRUE),
      unknown_walled = max(unknown_walled, missing_walled, na.rm = TRUE),
      capacity = mean(capacity, na.rm = TRUE)
    ) %>%
    as.data.table()
  
  # Final Clean
  MI_UST_tanks_SD[, `:=`(
    leak_after_closure = ifelse(leak_after_closure > 0, 1, 0),
    leak_before_NFA_before_closure = ifelse(leak_before_NFA_before_closure > 0, 1, 0),
    leak_before_NFA_after_closure = ifelse(leak_before_NFA_after_closure > 0, 1, 0),
    no_leak = ifelse(no_leak > 0, 1, 0),
    facility_name = stringr::str_to_title(trimws(gsub("[^[:alnum:] ]", "", facility_name))),
    facility_id = as.character(facility_id),
    tank_id = as.character(tank_id)
  )]
  
  # Output
  output_dir <- here("Data","Raw","state_databases","Michigan")
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  fwrite(MI_UST_tanks_SD, file.path(output_dir, "MI_Harmonized_UST_tanks.csv"))
  fwrite(MI_LUST_SD, file.path(output_dir, "MI_Harmonized_LUST.csv"))
}