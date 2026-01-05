## Michigan UST Dataset Creation

# Define the county variable name in the raw data
COUNTYVARIABLE <- "county_name"

library(here)
first_run = TRUE
#load packages ----
source(here('Code','00_global_packages.R'))

# Additional packages for Excel file handling
library(readxl)

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

# Function to classify tank wall types - adapted for Michigan's specific wall type descriptions
classify_tank_walls <- function(data, tank_wall_col = "tank_wall_type") {
  result <- copy(data)
  
# Create binary variables for tank wall type based on Michigan's specific descriptions
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
classify_substances <- function(data, substance_col_name = "substance") {
  # Create binary classification columns with default value
  result <- copy(data)
  result[, `:=`(
    is_gasoline = 0,
    is_diesel = 0,
    is_oil_kerosene = 0,
    is_jet_fuel = 0,
    is_other = 1  # Default to 1, will set to 0 if any other category matches
  )]
# Classification patterns for Michigan
gasoline_patterns <- paste(c(
  "\\bgasoline\\b", "\\bgasohol\\b", "\\bunleaded\\b", "\\bpremium\\b", "\\bregular\\b", 
  "\\be(10|15|85|\\d+)\\b", "\\bno[- ]?lead\\b", "\\brul\\b", "\\bpunl\\b", "\\bpul\\b",
  "\\bmogas\\b", "\\bpremuim\\b", "\\bsuper\\b", "\\boctane\\b", "\\b87\\b", "\\b89\\b", "\\b91\\b", "\\b93\\b",
  "\\bethanol[- ]?blend\\b", "\\brace[- ]?fuel\\b", "\\brec[- ]?(fuel|gas)\\b", "\\bsalvage[- ]?gasoline\\b",
  "\\bmtbe\\b", "\\bcleartex\\b", "\\bunl(ead)?\\b", "\\bleaded\\b", "\\blead[- ]?free\\b",
  # Exclude these from matching
  "(?<!av[\\s-]|aviation[\\s-]|jet[\\s-])\\bgas\\b(?!\\s*(de-hy|condensate))",
  "(?<!jet|aviation)\\s+fuel"
), collapse = "|")

diesel_patterns <- paste(c(
  "\\bdiesel\\b", "\\bbio[- ]?diesel\\b", "\\boff[- ]?road diesel\\b", "\\bdsl\\b", 
  "\\bulsd\\b", "\\bd[- ]?[0-9]\\b", "\\b#2 diesel\\b", "\\b#3 diesel\\b",
  "\\bhigh sulp(hur)? diesel\\b", "\\bheavy diesel\\b", "\\bdied fuel\\b",
  "\\bdiesel exhaust fluid\\b", "\\bdef\\b", "\\borv\\b", "\\bon road\\b", "\\bdistill\\b",
  "\\bd$\\b", "\\bord\\b", "\\bdies\\b"
), collapse = "|")

oil_kerosene_patterns <- paste(c(
  # Oil types
  "\\boil\\b", "\\blubrican(t|ting)\\b", "\\blube\\b", "\\bmotor[- ]?oil\\b", 
  "\\bengine[- ]?oil\\b", "\\bused[- ]?oil\\b", "\\bwaste[- ]?oil\\b", 
  "\\bhydraulic\\b", "\\bhyd(\\.|ralic|\\s)\\b", "\\btrans(mission)?\\b",
  "\\bquench\\b", "\\bhoist\\b", "\\bgear\\b", "\\bcrude\\b", "\\bheavy\\b",
  "\\bheating\\b", "\\bheat\\b", "\\bsalvage[- ]?oil\\b", "\\bmachine[- ]?oil\\b",
  "\\bway[- ]?oil\\b", "\\bway[- ]?lube\\b", "\\bpress[- ]?oil\\b", "\\bpetrol\\b",
  "\\bvirgin\\b", "\\bdrain[- ]?oil\\b", "\\bcutting[- ]?oil\\b", "\\baxle[- ]?lube\\b",
  "\\bbulk[- ]?oil\\b", "\\btorque[- ]?oil\\b", "\\bsteer[- ]?oil\\b", "\\bfresh[- ]?oil\\b",
  "\\bmineral[- ]?oil\\b", "\\bcore[- ]?oil\\b", "\\bform[- ]?oil\\b", "\\bsalvage[- ]?oil\\b",
  "\\bignition[- ]?oil\\b", "\\bresidual[- ]?oil\\b", "\\bsoluble[- ]?oil\\b", "\\bseco[- ]?oil\\b",
  # Kerosene types
  "\\bkerosene\\b", "\\bkerosine\\b", "\\bk-1\\b", "\\bkero\\b",
  # Fuel oil types  
  "\\bfuel[- ]?oil\\b", "\\bfurnace[- ]?oil\\b", "\\b#[1-6](\\s|\\/)?(fuel|heating)?[- ]?oil\\b",
  "\\bheating[- ]?oil\\b", "\\bheat[- ]?oil\\b", "\\bconsumptive\\s+use\\b",
  "\\bboiler[- ]?fuel\\b", "\\bfurn(ace)?[- ]?fuel\\b", "\\bheavy[- ]?fuel\\b",
  "\\bbunker[- ]?oil\\b", "\\bslop[- ]?oil\\b", "\\bconsumptive\\b",
  # Other types
  "\\batf\\b", "\\bdextron\\b", "\\bcool(ant)?\\b", "\\bantifreeze\\b", "\\banti[- ]?freeze\\b",
  "\\bprocess[- ]?oil\\b", "\\bsolution\\b", "\\bslurry\\b", "\\bemulsion\\b"
), collapse = "|")

jet_fuel_patterns <- paste(c(
  "\\bjet\\b", "\\bjet[- ]?a\\b", "\\bjeta\\b", "\\bjet[- ]?fuel\\b", 
  "\\baviation\\b", "\\bavgas\\b", "\\bav[- ]?gas\\b", "\\bav[- ]?fuel\\b",
  "\\baircraft\\b", "\\bairplane\\b", "\\bjp[- ]?[45678]\\b",
  "\\b100ll\\b", "\\b100\\s+oct\\b", "\\b80\\s+oct\\b", "\\bava?[- ]?fuel\\b",
  "\\bskychief\\b", "\\bair(craft)?[- ]?fuel\\b", "\\bav(\\.)?\\b"
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
standardize_capacity <- function(data, capacity_col = "tank_capacity") {
  result <- copy(data)
  
  result[, `:=`(
    capacity = as.numeric(gsub("[^0-9.]", "", get(capacity_col)))
  )]
  
  return(result)
}

# Function to read Michigan Excel file sheets with proper handling of merged headers
read_michigan_sheets <- function(file_path) {
  cat("\nReading Michigan Excel file:", file_path, "\n")
  
  # Get sheet names
  sheet_names <- excel_sheets(file_path)
  cat("Found", length(sheet_names), "sheets:", paste(sheet_names, collapse = ", "), "\n")
  
  # Initialize list to hold all data frames
  all_sheets <- list()
  
  # Process all sheets except 6 and 9
  sheet_indices <- setdiff(1:length(sheet_names), c(6, 9))
  
  for (i in sheet_indices) {
    sheet_name <- sheet_names[i]
    cat("\nProcessing sheet", i, ":", sheet_name, "\n")
    
    # Read the header row first (combined from rows 1 and 2)
    header_data <- read_excel(file_path, sheet = i, n_max = 2, col_names = FALSE)
    
    # Combine headers from row 1 and 2 to handle merged cells
    combined_header <- character(ncol(header_data))
    for (j in 1:ncol(header_data)) {
      if (!is.na(header_data[1, j])) {
        header_part1 <- as.character(header_data[1, j])
      }
      header_part2 <- ifelse(!is.na(header_data[2, j]), as.character(header_data[2, j]), "")
      combined_header[j] <- paste(header_part1, header_part2, sep = " ")
    }
    combined_header <- trimws(combined_header)
    
    # Now read the data starting from row 4 (skip header rows 1-2 and row 3 as instructed)
    # For sheet 1, there's no title in A3, but for consistency, we'll skip row 3 for all sheets
    sheet_data <- read_excel(file_path, sheet = i, skip = 3, col_names = FALSE)
    
    # Set the column names to our combined headers
    colnames(sheet_data) <- combined_header
    
    # Add to list
    all_sheets[[i]] <- sheet_data
    cat("  Sheet", i, "loaded:", nrow(sheet_data), "rows,", ncol(sheet_data), "columns\n")
  }
  
  # Combine all sheets
  combined_data <- rbindlist(all_sheets, fill = TRUE)
  cat("\nCombined data has", nrow(combined_data), "rows and", ncol(combined_data), "columns\n")
  
  return(combined_data)
}

# Load Raw Data - Michigan files -----
cat("\nLoading Michigan UST data...\n")

# Path to the Michigan Excel file
michigan_file_path <- "C:/Users/kaleb/Box/UST-Insurance/Data/Raw_do_not_write/state_databases/Michigan/UTK Master List 3-3-25.xlsx"

# Read the Michigan Excel file
michigan_raw <- tryCatch({
  read_michigan_sheets(michigan_file_path)
}, error = function(e) {
  cat("\nERROR: Failed to read Excel file. Details:", e$message, "\n")
  # Try alternative approach for .xlsb files if standard approach fails
  cat("\nAttempting alternative approach for .xlsb file...\n")
  
  # Some systems might need an alternative approach for .xlsb files
  # For this example, we'll assume readxl can handle it with additional parameters
  michigan_data <- list()
  sheet_indices <- c(1:5, 7:8)  # All sheets except 6 and 9
  
  for (i in sheet_indices) {
    sheet_data <- read_excel(michigan_file_path, sheet = i, skip = 3)
    michigan_data[[i]] <- sheet_data
  }
  
  rbindlist(michigan_data, fill = TRUE)
})

# If data was loaded successfully, proceed with processing
if (!is.null(michigan_raw) && nrow(michigan_raw) > 0) {
  cat("\nSuccessfully loaded Michigan raw data with", nrow(michigan_raw), "rows\n")
  
  # Process Tank Data -----
  cat("\nProcessing Michigan tank data...\n")
  
#   # Rename columns according to mapping in instructions
## RAW col names
# Display raw column names for reference


# Create mapping from raw column names to standardized column names
  michigan_raw = michigan_raw %>% janitor::clean_names()
print(colnames(michigan_raw))

# Find matching column names (allowing for some variation)
michigan_columns <- names(michigan_raw)
matched_cols <- list()
# Create column mapping
mi_columns <- c(
  "facility_id" = "facility_id",
  "facility_county" = "county_name", 
  "details_status" = "tank_status",
  "new_tank_id_number" = "tank_id",
  "tank_capacity" = "tank_capacity",
  "tank_instal_date" = "tank_installed_date",
  "tank_content" = "substance",
  "tank_removal_date" = "tank_remove_date",
  "tank_construction" = "tank_wall_type"
)


# Find columns in michigan_raw data
cols_to_select <- names(mi_columns)
cols_present <- cols_to_select[cols_to_select %in% michigan_columns]

# Select only those columns that exist in the dataset
MI_UST_tanks <- michigan_raw[, ..cols_present]

# Rename columns using data.table syntax
setnames(MI_UST_tanks, old = cols_present, new = mi_columns[cols_present])

# Report on missing columns
missing_cols <- setdiff(cols_to_select, cols_present)
if (length(missing_cols) > 0) {
  cat("\nWARNING: The following columns were not found in the dataset:\n")
  cat(paste(missing_cols, collapse = ", "), "\n")
}
  # Add state identifiers
  MI_UST_tanks[, `:=`(
    state = "Michigan",
    state_abbr = "MI"
  )]
  
  # Filter out non-registered tanks and temporarily out of use tanks
  MI_UST_tanks <- MI_UST_tanks[!grepl("NON-Registered Tank|Temporarily Out of Use", tank_status, ignore.case = TRUE)]
  
  # Convert dates to proper format
  MI_UST_tanks[, tank_installed_date := lubridate::mdy(tank_installed_date)]
  MI_UST_tanks[, tank_closed_date := lubridate::mdy(tank_remove_date)]
  MI_UST_tanks[, tank_remove_date := NULL]  # Remove original column
  
  # Apply standardization functions
  cat("\nStandardizing capacity values...\n")
  MI_UST_tanks <- standardize_capacity(MI_UST_tanks, "tank_capacity")
  
  cat("\nClassifying tank wall types...\n")
  MI_UST_tanks <- classify_tank_walls(MI_UST_tanks, "tank_wall_type")
  
  cat("\nClassifying substance types...\n")
  MI_UST_tanks <- classify_substances(MI_UST_tanks, "substance")
  
  # Remove duplicates based on facility_id and tank_id
  MI_UST_tanks <- unique(MI_UST_tanks, by = c("facility_id", "tank_id"))
  cat("\nAfter removing duplicates, Michigan tank data has", nrow(MI_UST_tanks), "rows\n")
  
  # Process LUST Data -----
  cat("\nProcessing Michigan LUST data...\n")
  
  # Load the EPA LUST data and filter for Michigan
  MI_lust_raw <- fread(here("Data", "Raw_do_not_write", "epa_ust_data_1_8_2024", "Releases_1_edited.csv")) %>%
    janitor::clean_names() %>%
    filter(state == "Michigan") %>%
    as.data.table()
  
  # Handle the "MI" prefix in facility_id as per instructions
  MI_LUST_SD <- MI_lust_raw %>%
    mutate(
      # Remove "MI" prefix from facility_id to match tank data
      facility_id = gsub("^MI", "", facility_id),
      LUST_id = lust_id,
      report_date = mdy_hm(reported_date),
      nfa_date = NA  # NFA date not available in raw data
    ) %>%
    select(facility_id, LUST_id, report_date, nfa_date) %>%
    as.data.table()
  
  # Join LUST data with tank data
  cat("\nJoining LUST data with tank data...\n")
  
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
  
  # Add county FIPS codes
  cat("\nAdding county FIPS codes...\n")
  
  # Standardize county names
  MI_UST_tanks_LUST[, standardized_county := standardize_county_name(county_name)]
  
  # Use tigris package to load county FIPS codes for Michigan
  if (!require(tigris)) install.packages("tigris")
  library(tigris)
  
  # Load counties from the Census Bureau
  counties <- counties(state = "MI", cb = TRUE)
  counties <- as.data.table(counties)
  
  # Extract just the columns we need
  fips_data <- counties[, .(
    county_name = NAME,
    county_fips = GEOID
  )]
  
  # Standardize county names in FIPS data
  fips_data[, standardized_county := standardize_county_name(county_name)]
  
  # Join FIPS codes based on standardized county name
  MI_UST_tanks_LUST <- merge.data.table(
    MI_UST_tanks_LUST,
    fips_data[, .(standardized_county, county_fips)],
    by = "standardized_county",
    all.x = TRUE
  )
  
  # Report on the match rate
  cat("\nCounty FIPS code matching summary:\n")
  cat("Total facilities:", uniqueN(MI_UST_tanks_LUST$facility_id), "\n")
  cat("Facilities with county FIPS codes:", sum(!is.na(MI_UST_tanks_LUST$county_fips)), "\n")
  cat("Match rate:", round(sum(!is.na(MI_UST_tanks_LUST$county_fips)) / uniqueN(MI_UST_tanks_LUST$facility_id) * 100, 2), "%\n")
  
  # Clean up temp column
  MI_UST_tanks_LUST[, standardized_county := NULL]
  
  # Aggregate leak indicators at facility-tank level
  MI_UST_tanks_SD <- MI_UST_tanks_LUST %>%
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
  
  # Convert binary indicators to 0/1
  MI_UST_tanks_SD <- MI_UST_tanks_SD[, .(
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
  cat("\nSaving final Michigan datasets...\n")
  
  # Create directory if it doesn't exist
  dir_path <- here("Data", "Raw_do_not_write", "state_databases", "Michigan")
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
  }
  
  # Write tank data
  fwrite(MI_UST_tanks_SD, here("Data", "Raw_do_not_write", "state_databases", "Michigan", "MI_UST_tanks.csv"))
  
  # Write LUST data
  fwrite(MI_LUST_SD, here("Data", "Raw_do_not_write", "state_databases", "Michigan", "MI_UST_LUST.csv"))
  
  # Summary stats
  cat("\nMichigan data processing complete\n")
  cat("Tank records:", nrow(MI_UST_tanks_SD), "\n")
  cat("LUST records:", nrow(MI_LUST_SD), "\n")
  
  # Count closed tanks
  num_closed_tanks_MI = uniqueN(MI_UST_tanks_SD[!is.na(tank_closed_date), paste0(facility_id, "-", tank_id)])
  cat("Closed tanks:", num_closed_tanks_MI, "\n")
  
  # Count leaks
  count_leak_after_closure = MI_UST_tanks_SD[leak_after_closure == 1, uniqueN(paste0(facility_id, "-", tank_id))]
  count_leak_before_NFA_before_closure = MI_UST_tanks_SD[leak_before_NFA_before_closure == 1, uniqueN(paste0(facility_id, "-", tank_id))]
  count_leak_before_NFA_after_closure = MI_UST_tanks_SD[leak_before_NFA_after_closure == 1, uniqueN(paste0(facility_id, "-", tank_id))]
  count_no_leak = MI_UST_tanks_SD[no_leak == 1, uniqueN(paste0(facility_id, "-", tank_id))]
  
  MI_counts = data.frame(
    category = c("leak_after_closure", "leak_before_NFA_before_closure", "leak_before_NFA_after_closure", "no_leak"),
    count = c(count_leak_after_closure, count_leak_before_NFA_before_closure, count_leak_before_NFA_after_closure, count_no_leak)
  )
  MI_counts$total = sum(MI_counts$count)
  MI_counts$percent = (MI_counts$count / MI_counts$total) * 100
  
  cat("\nMichigan leak statistics:\n")
  print(MI_counts)
} else {
  cat("\nERROR: Failed to load Michigan data. Check file path and format.\n")
}
