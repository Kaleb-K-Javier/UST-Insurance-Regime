# New Mexico Dataset Creation
# 
#

#Issues section: NM EPA does not have wall data, need to get that data from the state. 
# Have to eamil that zack dude.


# Define the county variable name in the raw data
COUNTYVARIABLE <- "county"

library(here)
#first_run = FALSE
first_run = TRUE
#load packags ----
source(here('Code','00_global_packages.R'))
library(readxl)

# Add the substance classification function from EPA script
classify_substances <- function(data, substance_col_name = "substances") {
  # Create binary classification columns with default values
  result <- copy(data)
  result[, `:=`(
    is_gasoline = 0,
    is_diesel = 0,
    is_oil_kerosene = 0,
    is_jet_fuel = 0,
    is_other = 1  # Default to 1, will set to 0 if any other category matches
  )]
# Enhanced substance classification patterns for New Mexico data
gasoline_patterns <- paste(c(
  # Specific gasoline types in the data
  "\\bunleaded\\s*gasoline\\b", "\\bsuper\\s*unleaded\\b", "\\bunleaded\\s*plus\\b", 
  "\\bleaded\\s*gasoline\\b", "\\bgasoline\\s*unknown\\s*type\\b", "\\be85\\s*gasoline\\b",
  "\\balcohol\\s*enriched\\s*gasoline\\b",
  # Ethanol content patterns
  "\\bgasoline\\s*\\(containing\\s*<=10%\\s*ethanol\\)\\b", 
  "\\bgasoline\\s*containing\\s*>10%\\s*ethanol\\b",
  # Additional patterns to catch combinations
  "\\bpetrol\\b", "\\bmogas\\b"
), collapse = "|")

diesel_patterns <- paste(c(
  # Match exact diesel entries
  "\\bdiesel\\b", "\\bdiesel;\\b",
  # Biodiesel types
  "\\bbiodiesel\\b", "\\bbiodiesel\\s*mixture\\b"
), collapse = "|")

oil_kerosene_patterns <- paste(c(
  # Oil types found in data
  "\\bused\\s*oil\\b", "\\blubricating\\s*oil\\b", "\\bauto\\s*transmission\\s*fluid\\b",
  "\\bhydraulic\\s*fluid\\b",
  # Heating oils and kerosene
  "\\bkerosene\\b", "\\bheating\\s*oil\\b", "\\bfuel\\s*oil\\b"
), collapse = "|")

jet_fuel_patterns <- paste(c(
  # Aviation fuels
  "\\bjet\\s*fuel\\b", "\\baviation\\s*gas\\b", "\\bavgas\\b"
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
classify_tank_walls <- function(data, tank_wall_col = "tank_wall_type") {
  result <- copy(data)
  
  # Create binary variables for tank wall type based on NM data
  result[, `:=`(
    double_walled = as.integer(grepl("Double Walled|double walled|DOUBLE WALLED", get(tank_wall_col), ignore.case = TRUE)),
    single_walled = as.integer(grepl("Single Walled|single walled|SINGLE WALLED", get(tank_wall_col), ignore.case = TRUE) & 
                              !grepl("Unknown", get(tank_wall_col), ignore.case = TRUE)),
    # Most entries are empty strings, which should be treated as unknown
    unknown_walled = as.integer(is.na(get(tank_wall_col)) | get(tank_wall_col) == "" | 
                                grepl("Unknown", get(tank_wall_col), ignore.case = TRUE)),
    missing_walled = as.integer(is.na(get(tank_wall_col)) | get(tank_wall_col) == "")
  )]
  
  # Create a summary table
  cat("\nTank wall classification summary for New Mexico data:\n")
  print(result[, .(
    total_records = .N,
    single_walled_count = sum(single_walled, na.rm = TRUE),
    double_walled_count = sum(double_walled, na.rm = TRUE),
    unknown_walled_count = sum(unknown_walled, na.rm = TRUE),
    missing_walled_count = sum(missing_walled, na.rm = TRUE)
  )])
  
  return(result)
}

# Function to standardize capacity values

standardize_capacity <- function(data, capacity_col = "capacity") {
  result <- copy(data)
  
  result[, `:=`(
    capacity = as.numeric(gsub("[^0-9.]", "", get(capacity_col)))
  )]
  
  return(result)
}

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

# Get census geography data
census_files <- list.files(
  "C:/Users/kaleb/Box/UST-Insurance/Data/Processed/Census_Geography",
  pattern = "\\.csv$",
  full.names = TRUE
)

# Find New Mexico census data
nm_census_file <- census_files[grepl("New_Mexico_census_geography\\.csv$", census_files, ignore.case = TRUE)]

ust_data_epa_01 = fread(here("Data","Raw_do_not_write",'epa_ust_data_1_8_2024','USTs_4.csv'))%>% clean_names() %>% as.data.table()



epa_data <- fread("C:/Users/kaleb/Box/UST-Insurance/Data/Raw_do_not_write/epa_ust_data_1_8_2024/Facilities_edited_by_kj.csv")%>% clean_names() %>% as.data.table()

# Left join county information from epa_data to state_UST_full using facility_id
ust_data_epa_01 <- dplyr::left_join(ust_data_epa_01, epa_data[, c("facility_id", "county")], by = "facility_id") %>% 
  as.data.table()


NM_UST = ust_data_epa_01[grepl('New Mexico',state)]

## group by substance and count number of facilites
NM_UST_substance_count = NM_UST[, .(facility_count = .N), by = substances]
## save it to a file
fwrite(NM_UST_substance_count, here("Data","Raw_do_not_write","state_databases","New Mexico","NM_UST_substance_count.csv"))

## do the same for tank wall type
NM_UST_tank_wall_count = NM_UST[, .(facility_count = .N), by = tank_wall_type]
## save it to a file
fwrite(NM_UST_tank_wall_count, here("Data","Raw_do_not_write","state_databases","New Mexico","NM_UST_tank_wall_count.csv"))

View(NM_UST)
#list the NM LUST excel data files from the *_nfa_amount 
NM_LUST_files <- list.files(
  here("Data","Raw_do_not_write","state_databases","New Mexico"),
  pattern = "nfa",
  full.names = TRUE
)

# read each one in individually
NM_LUST_01 <- read_excel(NM_LUST_files[2]) %>% clean_names() %>% as.data.table()
NM_LUST_02 <- read_excel(NM_LUST_files[3])%>% clean_names() %>% as.data.table()

# now select the RID,FID Release_Date and NFA date and combine

NM_LUST_01 <- NM_LUST_01 %>% select(rid,fid,release_date,nfa_date,total_wp,total_paid)
NM_LUST_02 <- NM_LUST_02 %>% select(rid,fid,release_date,nfa_date,total_wp,total_paid)

NM_LUST = rbindlist(list(NM_LUST_01,NM_LUST_02))
# just select the facilityid and tank id, tank installed date and tank closed date
# Now including substances, tank_wall_type, and capacity columns

NM_UST_tanks = NM_UST %>% clean_names() %>%
    select(facility_id, tank_id, installation_date, removal_date, tank_status,county,
           substances, tank_wall_type, capacity) %>%
    #rename the date columns
    rename(tank_installed_date = installation_date,
           tank_closed_date = removal_date) %>%
           #remove NM from facility_id and make numeric
    mutate(facility_id = as.numeric(gsub("[A-Za-z]", "", facility_id)))

# Apply substance classification if data exists
if ("substances" %in% names(NM_UST_tanks) && 
    sum(!is.na(NM_UST_tanks$substances) & NM_UST_tanks$substances != "") > 0) {
  cat("\nClassifying substances for New Mexico\n")
  NM_UST_tanks <- classify_substances(NM_UST_tanks, "substances")
} else {
  # Add empty classification columns if no substance data
  NM_UST_tanks[, `:=`(
    is_gasoline = 0,
    is_diesel = 0,
    is_oil_kerosene = 0, 
    is_jet_fuel = 0,
    is_other = 1
  )]
  cat("\nNo substance data available. Using default classifications.\n")
}


# Apply tank wall classification if data exists
if ("tank_wall_type" %in% names(NM_UST_tanks) && 
    sum(!is.na(NM_UST_tanks$tank_wall_type) & NM_UST_tanks$tank_wall_type != "") > 0) {
  cat("\nClassifying tank walls for New Mexico\n")
  NM_UST_tanks <- classify_tank_walls(NM_UST_tanks, "tank_wall_type")
} else {
  # Add empty classification columns if no tank wall data
  NM_UST_tanks[, `:=`(
    single_walled = 0,
    double_walled = 0,
    unknown_walled = 1,
    missing_walled = 1
  )]
  cat("\nNo tank wall data available. Using default classifications.\n")
}

# Standardize capacity if data exists
if ("capacity" %in% names(NM_UST_tanks) && 
    sum(!is.na(NM_UST_tanks$capacity) & NM_UST_tanks$capacity != "") > 0) {
  cat("\nStandardizing capacity for New Mexico\n")
  NM_UST_tanks <- standardize_capacity(NM_UST_tanks, "capacity")
} else {
  # Add empty capacity column if no data
  NM_UST_tanks[, capacity := NA_real_]
  cat("\nNo capacity data available. Using NA for capacity.\n")
}

# left_join the two data sets and count 
## now identify the three sets of closed tanks
### 1. Closed tanks where a leak was found after closure
### 2. Closed tanks where a leak was found berfore closure and the NFA was issued before the closure date
### 3. Closed tanks where a leak was found berfore closure and the NFA was issued after the closure date
### 4. Closed tanks where a facility never had a leak ever
  NM_LUST <- NM_LUST[!is.na(rid) & !is.na(release_date)]

  # Count leaks by facility and date

state_leak_data_collapsed <- NM_LUST[, .(LUST_count = .N,
                            total_wp = sum(total_wp, na.rm = TRUE),
                             total_paid = sum(total_paid, na.rm = TRUE)), by = .(fid, release_date, nfa_date)]

  # Merge leak data with tank data



NM_UST_tanks_LUST = NM_UST_tanks %>%
    left_join(state_leak_data_collapsed, by = c("facility_id" = "fid")) %>%
    # make the dates date objects
    mutate(release_date = ymd_hms(release_date),
           NFA_date = ymd_hms(nfa_date),
           tank_installed_date = mdy_hms(tank_installed_date),
           tank_closed_date = mdy_hms(tank_closed_date)) %>%
    mutate(
        leak_after_closure = ifelse(!is.na(tank_closed_date) & release_date > tank_closed_date, 1, 0),
        leak_before_NFA_before_closure = ifelse(!is.na(tank_closed_date) & release_date < tank_closed_date & tank_closed_date > NFA_date, 1, 0),
        leak_before_NFA_after_closure = ifelse(!is.na(tank_closed_date) & release_date < tank_closed_date & tank_closed_date < NFA_date, 1, 0),
        no_leak = ifelse(is.na(release_date) & !is.na(tank_closed_date), 1, 0)
    ) %>% as.data.table()

fwrite(NM_UST_tanks_LUST, here("Data", "Raw_do_not_write", "state_databases", "New Mexico", "NM_UST_tanks_LUST_cost_flat.csv"))
#whats the total number of closed unique ust tanks -- 13035
NM_UST_tanks$id = paste0(NM_UST_tanks$facility_id, "-", NM_UST_tanks$tank_id)
NM_UST_tanks = NM_UST_tanks %>% as.data.table()
NM_UST_tanks[,tank_closed_date2:=mdy_hms(tank_closed_date)]
number_of_closed_tanks = uniqueN(NM_UST_tanks[!is.na(tank_closed_date2), id])


    # count the number of unique facility-tank_id pairs by each of the four categories
    #use data.table
    count_leak_after_closure <- NM_UST_tanks_LUST[
        leak_after_closure == 1,
        uniqueN(paste0(facility_id, "-", tank_id))
    ]

    count_leak_before_NFA_before_closure <- NM_UST_tanks_LUST[
        leak_before_NFA_before_closure == 1,
        uniqueN(paste0(facility_id, "-", tank_id))
    ]

    count_leak_before_NFA_after_closure <- NM_UST_tanks_LUST[
        leak_before_NFA_after_closure == 1,
        uniqueN(paste0(facility_id, "-", tank_id))
    ]

    count_no_leak <- NM_UST_tanks_LUST[
        no_leak == 1,
        uniqueN(paste0(facility_id, "-", tank_id))
    ]

    # Combine counts into a data frame
    counts <- data.frame(
      category = c("leak_after_closure", "leak_before_NFA_before_closure", "leak_before_NFA_after_closure", "no_leak"),
      count = c(count_leak_after_closure, count_leak_before_NFA_before_closure, count_leak_before_NFA_after_closure, count_no_leak)
    )

    # create the total number of obs by suming count
    counts$total = sum(counts$count)
    counts$percent = counts$count/counts$total
    # Print the counts data frame
    print(counts)
    print(number_of_closed_tanks)


## Standardize the column names and save to folder
## we are writing out two files. THe standarzied faciliyt-tank data and the LUST data

############################################################################################################
## Tank data
############################################################################################################

### first collapse the facility-tank-LUST data to just the facility-tank data, sum up the leaks and no leaks variables

NM_UST_tanks_SD = NM_UST_tanks_LUST %>%
 select(facility_id, tank_id, tank_installed_date, tank_closed_date, 
        leak_after_closure, leak_before_NFA_before_closure, leak_before_NFA_after_closure, no_leak,
        is_gasoline, is_diesel, is_oil_kerosene, is_jet_fuel, is_other,
        single_walled, double_walled, unknown_walled, missing_walled, capacity,county) %>%
 group_by(facility_id, tank_id, tank_installed_date, tank_closed_date,county) %>%
summarize(
  leak_after_closure = sum(leak_after_closure, na.rm = TRUE),
  leak_before_NFA_before_closure = sum(leak_before_NFA_before_closure, na.rm = TRUE),
  leak_before_NFA_after_closure = sum(leak_before_NFA_after_closure, na.rm = TRUE),
  no_leak = sum(no_leak, na.rm = TRUE),
  # Take maximum value for binary flags (1 wins over 0)
  is_gasoline = max(is_gasoline, na.rm = TRUE),
  is_diesel = max(is_diesel, na.rm = TRUE),
  is_oil_kerosene = max(is_oil_kerosene, na.rm = TRUE),
  is_jet_fuel = max(is_jet_fuel, na.rm = TRUE),
  is_other = max(is_other, na.rm = TRUE),
  single_walled = max(single_walled, na.rm = TRUE),
  double_walled = max(double_walled, na.rm = TRUE),
  unknown_walled = max(unknown_walled, na.rm = TRUE),
  missing_walled = max(missing_walled, na.rm = TRUE),
  capacity = mean(capacity, na.rm = TRUE)  # Use mean of numeric capacity
)

# using data.table change the sums to 1 if they are greater than 0
NM_UST_tanks_SD = NM_UST_tanks_SD %>% as.data.table()
NM_UST_tanks_SD= NM_UST_tanks_SD[,.(
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
), keyby=.(facility_id, tank_id, tank_installed_date, tank_closed_date,county)]

#drop NA facility numbers
NM_UST_tanks_SD = NM_UST_tanks_SD[!is.na(facility_id)]
glimpse(NM_UST_tanks_SD)
    NM_UST_tanks_SD[, facility_id := as.character(facility_id)]
glimpse(NM_UST_tanks_SD)

# Add census geography data if available
if (length(nm_census_file) > 0) {
  # COMMENTED OUT: This code uses the prior census geography work which has known issues
  # Once the GIS code is fixed, uncomment this section and adjust the merge logic accordingly
  # The current merge may use incorrect coordinates or boundary definitions
  
  # tryCatch({
  #   census_data <- fread(nm_census_file)
  #   
  #   # Convert facility IDs to character for joining
  #   census_data[, facility_id := as.character(facility_id)]
  #   
  #   glimpse(census_data)
  #   # Join with census data
  #   NM_UST_tanks_SD <- NM_UST_tanks_SD %>%
  #     left_join(census_data, by = "facility_id") %>%
  #     as.data.table()
  #   
  #   # Verify join success
  #   cat("\nAdded census geography data for New Mexico\n")
  #   cat("Unique county count:", uniqueN(NM_UST_tanks_SD$county), "\n")
  #   cat("Facilities with county data:", sum(!is.na(NM_UST_tanks_SD$county)), "\n")
  #   
  #   # Verify other census columns
  #   census_cols <- setdiff(names(census_data), c("facility_id", "county"))
  #   for (col in census_cols) {
  #     cat("Facilities with", col, "data:", sum(!is.na(NM_UST_tanks_SD[[col]])), "\n")
  #   }
  # }, error = function(e) {
  #   cat("\nError loading census data for New Mexico:", e$message, "\n")
  # })
  cat("\nCensus geography data merge commented out pending GIS code fixes\n")
} else {
  cat("\nNo census geography data found for New Mexico\n")
}


  # Standardize county names
  NM_UST_tanks_SD[, standardized_county := standardize_county_name(get(COUNTYVARIABLE))]
  
  cat("DEBUG: Loading county FIPS data from tigris Census package...\n")
  # Use tigris package to load county FIPS codes
  if (!require(tigris)) install.packages("tigris")
  library(tigris)

  # Load counties from the Census Bureau
  counties <- counties(state = "New Mexico", cb = TRUE)
  counties <- as.data.table(counties)

  # Extract just the columns we need
  fips_data <- counties[, .(
    county_name = NAME,
    county_fips_code = GEOID,
    state_fips = substr(GEOID, 1, 2),
    state_name = STATE_NAME
  )]

  # Add state abbreviation
  state_crosswalk <- data.table(state_name = state.name, state_abbr = state.abb)
  fips_data <- merge(fips_data, state_crosswalk, by = "state_name", all.x = TRUE)

  # Standardize county names
  fips_data[, standardized_county := standardize_county_name(county_name)]

  # Join FIPS codes based on standardized county name and state
  NM_UST_tanks_SD <- merge.data.table(
    NM_UST_tanks_SD,
    fips_data[, .(standardized_county, county_fips = county_fips_code)],
    by = "standardized_county",
    all.x = TRUE,
    allow.cartesian = TRUE
  )
  
  # Report on the match rate
  cat("\nCounty FIPS code matching summary:\n")
  cat("Total facilities:", uniqueN(NM_UST_tanks_SD$facility_id), "\n")
  cat("Facilities with county FIPS codes:", sum(!is.na(NM_UST_tanks_SD$county_fips)), "\n")
  cat("Match rate:", round(sum(!is.na(NM_UST_tanks_SD$county_fips)) / uniqueN(NM_UST_tanks_SD$facility_id) * 100, 2), "%\n")
  
  # Clean up temp column
  NM_UST_tanks_SD[, standardized_county := NULL]


# Define required columns order
required_columns = c(
  "facility_id", "tank_id", "tank_installed_date", "tank_closed_date",
  "leak_after_closure", "leak_before_NFA_before_closure", "leak_before_NFA_after_closure", "no_leak",
  "capacity", "single_walled", "double_walled", "missing_walled", "unknown_walled",
  "is_gasoline", "is_diesel", "is_oil_kerosene", "is_jet_fuel", "is_other",'county'
)

# Add county and census columns to the required column list if they exist
additional_columns = setdiff(names(NM_UST_tanks_SD), c(required_columns, "facility_id"))

# Reorder columns if county exists, put it first among additional columns
if ("county" %in% additional_columns) {
  setcolorder(NM_UST_tanks_SD, c(required_columns, "county", setdiff(additional_columns, "county")))
} else {
  setcolorder(NM_UST_tanks_SD, c(required_columns, additional_columns))
}

############################################################################################################
## LUST data
############################################################################################################

# the lust data should be the following columns
## facility_id
## LUST_id
## report_date
## nfa_date

NM_LUST_SD = NM_LUST %>% clean_names() %>%
    select(rid, fid, release_date, nfa_date) %>%
    rename(facility_id = fid,
           LUST_id = rid,
           report_date = release_date)
nrow(NM_UST_tanks_SD)
# write out the data
fwrite(NM_UST_tanks_SD, here("Data", "Raw_do_not_write", "state_databases", "New Mexico", "NM_UST_tanks.csv"))
fwrite(NM_LUST_SD, here("Data", "Raw_do_not_write", "state_databases", "New Mexico", "NM_LUST.csv"))
