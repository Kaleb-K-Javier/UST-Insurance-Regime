## Creating EPA UST Region 6 Analysis Dataset

message("========== SCRIPT START: Creating EPA UST Region 6 Analysis Dataset ==========")

# load packags ----
message("Loading required packages...")
library(here)
source(here('Code','00_global_packages.R'))
library(readxl)
library(data.table)
library(lubridate)
library(stringr)
message("Packages loaded successfully.")

utils::globalVariables(c(
  "facility_id", "tank_id", "county_fips", "county_fips_code", "county_geoid", "county_name",
  "file_source", "facility_number", "tank_installed_date", "tank_closed_date", "state", "state_abb",
  "report_date", "nfa_date", "panel_id", "facility_tank_id", "panel_year", "month", "year",
  "columns_to_select", "lust_columns_to_select"
))

# Helper functions -----------------------------------------------------------------
message("Defining helper functions...")

standardize_column_names <- function(data) {
    # Facility ID variants
    if ("facility_number" %in% names(data)) setnames(data, "facility_number", "facility_id")
    if ("Facility_ID" %in% names(data)) setnames(data, "Facility_ID", "facility_id")
    # Tank ID variants
    if ("tank_number" %in% names(data)) setnames(data, "tank_number", "tank_id")
    if ("Tank_ID" %in% names(data)) setnames(data, "Tank_ID", "tank_id")
    # Installation date variants
    if ("installation_date" %in% names(data)) setnames(data, "installation_date", "tank_installed_date")
    if ("Tank_Installation_Date" %in% names(data)) setnames(data, "Tank_Installation_Date", "tank_installed_date")
    # Removal/closure date variants
    if ("removal_date" %in% names(data)) setnames(data, "removal_date", "tank_closed_date")
    if ("Tank_Removal_Date" %in% names(data)) setnames(data, "Tank_Removal_Date", "tank_closed_date")
    # County name variants
    if ("county" %in% names(data) && !"county_name" %in% names(data)) setnames(data, "county", "county_name")
    if ("CountyCAPS" %in% names(data) && !"county_name" %in% names(data)) setnames(data, "CountyCAPS", "county_name")
    # County FIPS/Geoid variants
    if ("county_fips" %in% names(data) && !"county_geoid" %in% names(data)) setnames(data, "county_fips", "county_geoid")
    if ("county_fips_code" %in% names(data) && !"county_geoid" %in% names(data)) setnames(data, "county_fips_code", "county_geoid")
    if ("county_fips" %in% names(data) && "county_geoid" %in% names(data)) data[, county_fips := NULL]
    if ("county_fips_code" %in% names(data) && "county_geoid" %in% names(data)) data[, county_fips_code := NULL]
    return(data)
}

convert_to_character <- function(data, date_cols) {
    cols_to_convert <- setdiff(names(data), date_cols)
    data[, (cols_to_convert) := lapply(.SD, as.character), .SDcols = cols_to_convert]
    return(data)
}

# New flexible date parser function
parse_flexible_date <- function(date_str) {
  if (inherits(date_str, "Date")) return(date_str)
  date_str <- str_trim(date_str)
  orders <- c(
    "ymd", "mdy", "dmy",
    "ymd HMS", "mdy HMS", "dmy HMS",
    "ymd HM", "mdy HM", "dmy HM",
    "Ymd", "mdY", "dmY",
    "Ymd HMS", "mdY HMS", "dmY HMS",
    "d-b-y", "d-b-Y" # <-- add these formats for e.g. 24-Mar-88
  )
  parsed <- parse_date_time(date_str, orders = orders, exact = FALSE, tz = "UTC")
  as_date(parsed)
}

# New Function: process_state_data ------------------------------------------------
process_state_data <- function(tank_files, lust_files) {
  # Create a mapping from state abbreviations to full state names
  state_names <- c(
    "AL" = "Alabama", "CT" = "Connecticut", "IL" = "Illinois", "KS" = "Kansas",
    "LA" = "Louisiana", "ME" = "Maine", "MA" = "Massachusetts", "MI" = "Michigan",
    "MN" = "Minnesota", "MT" = "Montana", "NJ" = "New Jersey", "NM" = "New Mexico",
    "NC" = "North Carolina", "OH" = "Ohio", "OK" = "Oklahoma", "PA" = "Pennsylvania", 
    "SD" = "South Dakota", "TN" = "Tennessee", "TX" = "Texas", "VA" = "Virginia", 
    "ID" = "Idaho", "AR" = "Arkansas", "DC" = "District of Columbia"
  )
  ### The GIS messed stuff up since only some of the obs had lat long then only some of
  ### th data had geoids. I neeed to go back to RAW data and just add the county name to the data
  tank_list <- list()
  lust_list <- list()
   #state = names(tank_files)[1]
  for(state in names(tank_files)) {
    ## Process UST (tank) Data
    dt_tank <- fread(tank_files[state], colClasses = "character") %>% as.data.table()
    file_state = tank_files[which(names(tank_files) == state)]
    dt_tank[,state := state]
    # Add file source column
    dt_tank[, file_source := file_state]
    
    # Special handling for New Mexico facility_number
    if(state == "NM" && "facility_number" %in% names(dt_tank)) {
      dt_tank[, facility_id := facility_number]
      dt_tank[, facility_number := NULL]
    }
    
    # Explicitly convert facility_id and tank_id to character if they exist
    if("facility_id" %in% names(dt_tank)) {
      dt_tank[, facility_id := as.character(facility_id)]
    }
    if("tank_id" %in% names(dt_tank)) {
      dt_tank[, tank_id := as.character(tank_id)]
    }
    
    dt_tank <- standardize_column_names(dt_tank)
    
    # Ensure facility_id and tank_id are character type after standardization
    dt_tank[, facility_id := as.character(facility_id)]
    dt_tank[, tank_id := as.character(tank_id)]
    
    if (state == "TX" && "county" %in% names(dt_tank)) {
      dt_tank[, county_name := county]
      dt_tank[, county := NULL]
    }
    
    # Replace current if/else blocks with:
    dt_tank[, `:=`(
       tank_installed_date = parse_flexible_date(tank_installed_date),
       tank_closed_date = parse_flexible_date(tank_closed_date)
    )]
    
    dt_tank <- convert_to_character(dt_tank, c("tank_installed_date","tank_closed_date"))
    
    # Select and standardize columns - create any missing columns with NA values
    columns_of_interest <- c("facility_id", "tank_id", "tank_installed_date", "tank_closed_date",
                           "leak_after_closure", "leak_before_NFA_before_closure", 
                           "leak_before_NFA_after_closure", "no_leak", "capacity",
                           "single_walled", "double_walled", "missing_walled", "unknown_walled",
                           "is_gasoline", "is_diesel", "is_oil_kerosene", "is_jet_fuel", "is_other",
                           "county_name", "county_geoid")
    
    # Create missing columns with NA values, ensure date columns are character
    for(col in columns_of_interest) {
      if(!(col %in% names(dt_tank))) {
        if (col %in% c("tank_installed_date", "tank_closed_date")) {
          dt_tank[, (col) := NA_character_]
        } else {
          dt_tank[, (col) := NA]
        }
      }
    }
    
    # Logging for missing columns
    missing_cols <- setdiff(columns_of_interest, names(dt_tank))
    if (length(missing_cols) > 0) {
      warning(sprintf("State %s: Missing columns after harmonization: %s", state, paste(missing_cols, collapse=", ")))
    }
    
    # Add state and file_source to our selection list
    columns_to_select <- c(columns_of_interest, "state", "file_source")
    
    # Select only the columns we want
    dt_tank_selected <- dt_tank[, ..columns_to_select]
    
    # Add the standardized data to our list
    tank_list[[state]] <- dt_tank_selected

    ## Process LUST Data
    dt_lust <- fread(lust_files[state], colClasses = "character")
    file_state = lust_files[which(names(lust_files) == state)]
    dt_lust[,state := state_names[which(names(state_names) == state)]]

    # Add file source column
    dt_lust[, file_source := file_state]
    
    # Special handling for New Mexico facility_number in LUST data
    if(state == "NM" && "facility_number" %in% names(dt_lust)) {
      dt_lust[, facility_id := facility_number]
      dt_lust[, facility_number := NULL]
    }
    
    # Explicitly convert facility_id to character if it exists
    if("facility_id" %in% names(dt_lust)) {
      dt_lust[, facility_id := as.character(facility_id)]
    }
    
    dt_lust <- standardize_column_names(dt_lust)
    
    # Ensure facility_id is character type after standardization
    dt_lust[, facility_id := as.character(facility_id)]
    
    # Replace current if/else blocks with:
    dt_lust[, `:=`(
       report_date = parse_flexible_date(report_date),
       nfa_date = parse_flexible_date(nfa_date)
    )]
    
    dt_lust <- convert_to_character(dt_lust, c("report_date","nfa_date"))
    
    # For LUST data, select key columns - slightly different set than tank data
    lust_columns_of_interest <- c("facility_id", "report_date", "nfa_date")
    
    # Create missing columns with NA values
    for(col in lust_columns_of_interest) {
      if(!(col %in% names(dt_lust))) {
        dt_lust[, (col) := NA]
      }
    }
    
    # Add state and file_source to our selection list
    lust_columns_to_select <- c(lust_columns_of_interest, "state", "file_source")
    
    # Select only the columns we want
    dt_lust_selected <- dt_lust[, ..lust_columns_to_select]
    
    # Add the standardized data to our list
    lust_list[[state]] <- dt_lust_selected
  }
  
  # Bind all state UST and LUST datasets respectively
  tank_data_all <- rbindlist(tank_list, use.names = TRUE, fill = TRUE)
  lust_data_all <- rbindlist(lust_list, use.names = TRUE, fill = TRUE)
  
  # Final check to ensure facility_id and tank_id are character
  if("facility_id" %in% names(tank_data_all)) {
    tank_data_all[, facility_id := as.character(facility_id)]
  }
  if("tank_id" %in% names(tank_data_all)) {
    tank_data_all[, tank_id := as.character(tank_id)]
  }
  
  if("facility_id" %in% names(lust_data_all)) {
    lust_data_all[, facility_id := as.character(facility_id)]
  }
  
  return(list(tank_data = tank_data_all, lust_data = lust_data_all))
}

# Data validation checks after harmonization
validate_harmonized_data <- function(dt, dt_name = "dataset") {
  key_vars <- c(
    "facility_id", "tank_id", "tank_installed_date", "tank_closed_date", "county_name", "county_geoid",
    "leak_after_closure", "leak_before_NFA_before_closure", "leak_before_NFA_after_closure", "no_leak",
    "single_walled", "double_walled", "missing_walled", "unknown_walled",
    "is_gasoline", "is_diesel", "is_oil_kerosene", "is_jet_fuel", "is_other", "capacity"
  )
  cat(sprintf("\nData validation summary for %s (missing values by variable):\n", dt_name))
  for (v in key_vars) {
    if (v %in% names(dt)) {
      n_missing <- sum(is.na(dt[[v]]))
      pct_missing <- round(100 * n_missing / nrow(dt), 2)
      cat(sprintf("%s: %d missing (%.2f%%)\n", v, n_missing, pct_missing))
      if (n_missing == nrow(dt)) warning(sprintf("All values missing for variable: %s in %s", v, dt_name))
      if (pct_missing > 50) warning(sprintf("High missingness (%.2f%%) for variable: %s in %s", pct_missing, v, dt_name))
    } else {
      warning(sprintf("Variable not found in %s: %s", dt_name, v))
    }
  }
}

# Function to compare harmonized data to raw state files
compare_harmonized_to_raw <- function(state_abbr, harmonized_dt, raw_file_path, key_vars = NULL) {
  cat(sprintf("\nComparing harmonized data to raw file for state: %s\n", state_abbr))
  if (!file.exists(raw_file_path)) {
    warning(sprintf("Raw file for %s not found: %s", state_abbr, raw_file_path))
    return()
  }
  raw_dt <- tryCatch({
    fread(raw_file_path, nThread = 1)
  }, error = function(e) {
    warning(sprintf("Could not read raw file for %s: %s", state_abbr, e$message))
    return(NULL)
  })
  if (is.null(raw_dt)) return()
  cat(sprintf("Raw file rows: %d, Harmonized rows: %d\n", nrow(raw_dt), nrow(harmonized_dt)))
  if (nrow(raw_dt) != nrow(harmonized_dt)) {
    warning(sprintf("Row count mismatch for %s: raw=%d, harmonized=%d", state_abbr, nrow(raw_dt), nrow(harmonized_dt)))
  }
  # Compare key variables
  if (is.null(key_vars)) key_vars <- intersect(names(raw_dt), names(harmonized_dt))
  missing_in_harmonized <- setdiff(key_vars, names(harmonized_dt))
  missing_in_raw <- setdiff(key_vars, names(raw_dt))
  if (length(missing_in_harmonized) > 0) {
    warning(sprintf("Variables missing in harmonized %s: %s", state_abbr, paste(missing_in_harmonized, collapse=", ")))
  }
  if (length(missing_in_raw) > 0) {
    warning(sprintf("Variables missing in raw %s: %s", state_abbr, paste(missing_in_raw, collapse=", ")))
  }
  cat("Comparison complete.\n")
}

message("Processing Region 6 tank and LUST data...")
# Apply the function to all tank and LUST datasets
tank_files <- c(
  'NM' = here("Data","Raw_do_not_write","state_databases","New Mexico","NM_UST_tanks.csv"),
  'TX' = here("Data","Raw_do_not_write","state_databases","Texas","TX_UST_tanks.csv"),
  'LA' = here("Data","Raw_do_not_write","state_databases","Louisiana","LA_UST_tanks.csv"),
  'AR' = here("Data","Raw_do_not_write","state_databases","Arkansas","Arkansas_facility_tank_data.csv"),
  'OK' = here("Data","Raw_do_not_write","state_databases","Oklahoma","OK_UST_tanks.csv")
)

lust_files <- c(
  'NM' = here("Data","Raw_do_not_write","state_databases","New Mexico","NM_LUST.csv"),
  'TX' = here("Data","Raw_do_not_write","state_databases","Texas","TX_LUST.csv"),
  'LA' = here("Data","Raw_do_not_write","state_databases","Louisiana","LA_LUST.csv"),
  'AR' = here("Data","Raw_do_not_write","state_databases","Arkansas","Arkansas_ust_lust_data.csv"),
  'OK' = here("Data","Raw_do_not_write","state_databases","Oklahoma","OK_LUST.csv")
)

item_1 = fread(tank_files["NM"])
item_2 = fread(tank_files["TX"])
head(item_1[,3])
head(item_2[,3])
processed_data <- process_state_data(tank_files, lust_files)

tank_data <- processed_data$tank_data %>% as.data.table()
lust_data <- processed_data$lust_data %>% as.data.table()
tank_data[,panel_id:=paste0(facility_id,"_",tank_id)]

glimpse(tank_data)
glimpse(lust_data)
# count the number of facilities and tanks in the data
epa_6_count = tank_data[, .(
    facilities = uniqueN(facility_id),
    tanks = uniqueN(panel_id)
),by = .(state,file_source)]
View(epa_6_count)

validate_harmonized_data(tank_data, "all_tank_data")
validate_harmonized_data(lust_data, "all_lust_data")

message(sprintf("Region 6 data processed: %d tank records, %d LUST records", nrow(tank_data), nrow(lust_data)))

# Compare harmonized data to raw files for Region 6 states
compare_harmonized_to_raw("NM", tank_data[state == "NM"], tank_files["NM"])
compare_harmonized_to_raw("TX", tank_data[state == "TX"], tank_files["TX"])
compare_harmonized_to_raw("LA", tank_data[state == "LA"], tank_files["LA"])
compare_harmonized_to_raw("AR", tank_data[state == "AR"], tank_files["AR"])
compare_harmonized_to_raw("OK", tank_data[state == "OK"], tank_files["OK"])

## New Section: Process EPA States Data ---------------------------------------
message("\n========== PROCESSING EPA STATES DATA ==========")
epa_states <- c(
  "Alabama", "Connecticut", "Illinois", "Kansas",
   "Maine", "Massachusetts", "Michigan",
  "Minnesota", "Montana", "New Jersey", "North Carolina",
  "Ohio",  "Pennsylvania", "South Dakota",
  "Tennessee", "Virginia", "Idaho"
)

# Construct file path vectors for EPA states tank and LUST files.
# Define state abbreviations mapping
state_abbr <- c(
  "Alabama" = "AL", "Connecticut" = "CT", "Illinois" = "IL", "Kansas" = "KS",
   "Maine" = "ME", "Massachusetts" = "MA", "Michigan" = "MI",
  "Minnesota" = "MN", "Montana" = "MT", "New Jersey" = "NJ", "North Carolina" = "NC",
  "Ohio" = "OH", "Pennsylvania" = "PA", "South Dakota" = "SD",
  "Tennessee" = "TN", "Virginia" = "VA", "Idaho" = "ID"
)

## some of these states still have issues. need to omit for now

# Create file paths with state abbreviations as names
epa_tank_files <- setNames(sapply(epa_states, function(s)
  here("Data","Raw_do_not_write","state_databases", s, paste0(s, "_UST_tanks.csv"))
), state_abbr[epa_states])


epa_lust_files <- setNames(sapply(epa_states, function(s)
  here("Data","Raw_do_not_write","state_databases", s, paste0(s, "_UST_LUST.csv"))
), state_abbr[epa_states])

# Process EPA states data using the shared process_state_data function.
message("Processing EPA states tank and LUST data...")
processed_data_epa <- process_state_data(epa_tank_files, epa_lust_files)
epa_tank_data <- processed_data_epa$tank_data %>% as.data.table()
epa_lust_data <- processed_data_epa$lust_data %>% as.data.table()
message(sprintf("EPA states data processed: %d tank records, %d LUST records", nrow(epa_tank_data), nrow(epa_lust_data)))
epa_tank_data[,panel_id:=paste0(facility_id,"_",tank_id)]

validate_harmonized_data(epa_tank_data, "epa_tank_data")
validate_harmonized_data(epa_lust_data, "epa_lust_data")

#tank_installed_date - epa 6 data

unique(epa_tank_data$state)

# Now the file contains two sets of UST and LUST datasets:
#     (a) tank_data and lust_data          -> Region 6 files (existing)
#     (b) epa_tank_data and epa_lust_data    -> EPA states data
names(tank_data) == names(epa_tank_data)
names(lust_data) == names(epa_lust_data)

message("Adding data source indicators and combining datasets...")
## add the data sample source as a col EPA for epa data and RG6 for region 6 data
tank_data[, data_source := "RG6"]
lust_data[, data_source := "RG6"]
epa_tank_data[, data_source := "EPA"]
epa_lust_data[, data_source := "EPA"]

# Ensure all facility_id and tank_id columns are character type before binding
message("Ensuring consistent data types before binding datasets...")
if("facility_id" %in% names(tank_data)) tank_data[, facility_id := as.character(facility_id)]
if("tank_id" %in% names(tank_data)) tank_data[, tank_id := as.character(tank_id)]

if("facility_id" %in% names(epa_tank_data)) epa_tank_data[, facility_id := as.character(facility_id)]
if("tank_id" %in% names(epa_tank_data)) epa_tank_data[, tank_id := as.character(tank_id)]

if("facility_id" %in% names(lust_data)) lust_data[, facility_id := as.character(facility_id)]
if("facility_id" %in% names(epa_lust_data)) epa_lust_data[, facility_id := as.character(facility_id)]

# Check for duplicate column names and fix them
tank_data_names <- names(tank_data)
epa_tank_data_names <- names(epa_tank_data)
lust_data_names <- names(lust_data)
epa_lust_data_names <- names(epa_lust_data)


tank_data = as.data.table(tank_data)
epa_tank_data = as.data.table(epa_tank_data)
lust_data = as.data.table(lust_data)
epa_lust_data = as.data.table(epa_lust_data)



message("Binding Region 6 and EPA data...")
all_tank_data = rbindlist(list(tank_data, epa_tank_data), use.names = TRUE)
all_lust_data = rbindlist(list(lust_data, epa_lust_data), use.names = TRUE)

# Final check to ensure facility_id is character type in combined datasets
all_tank_data[, facility_id := as.character(facility_id)]
all_tank_data[, tank_id := as.character(tank_id)]
all_lust_data[, facility_id := as.character(facility_id)]

validate_harmonized_data(all_tank_data, "all_tank_data")
validate_harmonized_data(all_lust_data, "all_lust_data")

message(sprintf("Combined dataset created: %d tank records, %d LUST records", nrow(all_tank_data), nrow(all_lust_data)))

message("Creating facility-tank IDs and counting closed tanks by state...")
#create a unique id for each tank-facility
all_tank_data[, facility_tank_id := paste0(facility_id, "_", tank_id,'_',state)]

unique(all_tank_data$state)


# Create a mapping dictionary between abbreviations and full state names using built‐in datasets
state_name_mapping <- setNames(state.name, state.abb)

# Add any missing territories or DC
state_name_mapping["DC"] <- "District of Columbia"
state_name_mapping["PR"] <- "Puerto Rico"
state_name_mapping["VI"] <- "Virgin Islands"
state_name_mapping["GU"] <- "Guam"
state_name_mapping["AS"] <- "American Samoa"
state_name_mapping["MP"] <- "Northern Mariana Islands"

# Create a reverse mapping: full state names to abbreviations
full_to_abbr <- setNames(names(state_name_mapping), state_name_mapping)

# New function to standardize states: convert two-letter abbreviations to full names 
# and add a column with the two-letter abbreviation.
standardize_states <- function(dt) {
    if ("state" %in% names(dt)) {
        dt[, state := trimws(state)]
        # Convert two-letter abbreviations to full names, leave full names unchanged.
        dt[, state := ifelse(nchar(state) == 2, state_name_mapping[state], state)]
        # Create a new column 'state_abb' with the two-letter abbreviation.
        dt[, state_abb := ifelse(state %in% names(full_to_abbr), full_to_abbr[state], NA_character_)]
    }
    return(dt)
}

# Apply state standardization to the combined tank dataset.
all_tank_data <- standardize_states(all_tank_data)
unique(all_tank_data$state)


all_tank_data[state == "", state := NA]
# Drop rows with missing state values
all_tank_data <- all_tank_data[!is.na(state)]


unique(all_tank_data$state)

states_lost_obs = all_tank_data[, .(
    total_obs = .N,
    missing_facility_id = sum(is.na(facility_id)),
    missing_install_date = sum(is.na(tank_installed_date))
), by = state]

## quickly count the number of closed tanks by state
### do this by counting if the tank_closed_date is not NA
closed_tanks_by_state = all_tank_data[, .(
    closed_tanks = sum(!is.na(tank_closed_date)),
    total_tanks = uniqueN(facility_tank_id),
    share_closed = sum(!is.na(tank_closed_date)) / uniqueN(facility_tank_id)
), by = state]


# Keep all states in the dataset - no filtering
# Retain original data assignment instead of creating filtered subset
S1_all_tank_data = all_tank_data
S1_all_lust_data = all_lust_data
unique(S1_all_lust_data$state)

#how missing is the facility id in the lust data
sum(is.na(S1_all_lust_data$facility_id)) / nrow(S1_all_lust_data) *100
### 8% of the data is missing facility id - drop those
S1_all_lust_data = S1_all_lust_data[!is.na(facility_id)]

## Save out lust data
fwrite(S1_all_lust_data, here("Data","Processed","all_lust_data.csv"))
fwrite(S1_all_tank_data, here("Data","Processed","all_tank_data.csv"))

message("========== SCRIPT COMPLETE ==========")

