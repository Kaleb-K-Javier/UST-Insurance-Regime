# Oklahoma Dataset Creation
# 
#

# Define the county variable name in the raw data
COUNTYVARIABLE <- "county"

library(here)
#first_run = FALSE
first_run = TRUE
#load packags ----
source(here('Code','00_global_packages.R'))



# tank_age
# corrosion_protection_tank
# corrosion_protection_piping
# overfill_protection
# piping_constuction
# tank_construction
# facility_history
# tank_status
# release_detection_piping
# release_detection_tank
# secondary_containment_tank
# secondary_containment_piping
# number_of_tanks
# tank_contents
# tank_capacity
# UST_pumping_system
# LUST_site

# Load Data -----

oklahoma_files <- list.files(
  here("Data","Raw_do_not_write","state_databases","Oklahoma"),
  pattern = "^OCC_PST.*\\.csv$",
  full.names = TRUE
)

OK_LUSt <- fread(
oklahoma_files[1]
)
OK_Tanks <- fread(
    oklahoma_files[2]
) 
nrow(OK_Tanks)
# count number of obs by tank substance

OK_Tanks %>% 
    count(`Tank Substance`, sort = TRUE)
  
  #


# Update tank selection to include substance, construction and capacity
OK_UST_tanks = OK_Tanks %>% clean_names() %>%
    select(facility_number, tank_number, tank_installed_date, tank_closed_date, tank_type, facility_zip_code,
           tank_status, tank_substance, tank_construction, tank_capacity) %>%
    filter(tank_type == "UST") %>% as.data.table()
view(OK_UST_tanks)
# Combine substance data to prepare for classification
OK_substances = OK_UST_tanks %>% 
  group_by( tank_substance) %>%
  count()
View(OK_substances)
# save out the substances data
fwrite(OK_substances, here("Data", "Raw_do_not_write", "state_databases", "Oklahoma", "OK_substances.csv"))

#save out construction tally
Ok_tanks_construction = OK_UST_tanks %>% group_by(tank_construction) %>% count(sort = TRUE)
fwrite(Ok_tanks_construction, here("Data", "Raw_do_not_write", "state_databases", "Oklahoma", "Ok_tanks_construction.csv"))

# Function to classify substances - using Texas detailed approach
classify_substances <- function(data, substance_col_name = "tank_substance") {
  # Create binary classification columns with default values
  result <- copy(data)
  result[, `:=`(
    is_gasoline = 0,
    is_diesel = 0,
    is_oil_kerosene = 0,
    is_jet_fuel = 0,
    is_other = 1  # Default to 1, will set to 0 if any other category matches
  )]
  
  # Classification patterns specifically for Oklahoma data
  gasoline_patterns <- paste(c(
    # Main gasoline types
    "\\bGasoline\\b", "\\bGasoline 100%\\b", 
    # Ethanol blends
    "\\bE-10\\b", "\\bE-15\\b", "\\bE-85\\b", 
    "\\bEthanol\\b", "\\bGasohol\\b"
  ), collapse = "|")
  
  diesel_patterns <- paste(c(
    # Main diesel types
    "\\bDiesel\\b", "\\bDyed Diesel\\b",
    # Biodiesel types
    "\\bBiodiesel\\b", "\\bBiodiesel <= 20%\\b", 
    "\\bBiodiesel for Blending\\b"
  ), collapse = "|")
  
  oil_kerosene_patterns <- paste(c(
    # Kerosene and heating oil
    "\\bKerosene\\b", "\\bHeating Oil\\b",
    # Motor and used oils
    "\\bMotor Oil\\b", "\\bUsed Oil\\b", "\\bWaste Oil\\b", 
    "\\bHydraulic Fluid\\b"
  ), collapse = "|")
  
  jet_fuel_patterns <- paste(c(
    "\\bJet Fuel\\b", "\\bAviation Fuel\\b"
  ), collapse = "|")
  
  # Convert substances to lowercase for case-insensitive matching
  result[, substances_lower := get(substance_col_name)]
  
  # Apply classification patterns using vectorized operations
  result[!is.na(substances_lower) & substances_lower != "", 
         is_gasoline := as.integer(grepl(gasoline_patterns, substances_lower, perl = TRUE))]
  
  # Exclude DEF from diesel classification
  result[!is.na(substances_lower) & substances_lower != "" & 
         !grepl("DEF \\(Diesel Exhaust Fluid\\)", substances_lower, perl = TRUE), 
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
  cat("\nSubstance classification summary for Oklahoma data:\n")
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
# Apply the substance classification
OK_substances_classified <- classify_substances(OK_UST_tanks)

# Check for unclassified substances
unclassified <- OK_substances_classified[is_gasoline + is_diesel + is_oil_kerosene + is_jet_fuel + is_other == 0]
if (nrow(unclassified) > 0) {
  warning(paste("Found", nrow(unclassified), "unclassified substances"))
  print(head(unclassified[, .(tank_substance, count = .N), by = tank_substance][order(-count)]))
}

OK_UST_tanks %>% group_by(tank_construction) %>% count(sort = TRUE)

classify_tank_walls <- function(data, tank_wall_col = "tank_construction") {
  result <- copy(data)
  
  # Create binary variables for tank wall type based on Oklahoma data
  result[, `:=`(
    double_walled = as.integer(
      grepl("Double Walled|Secondarily Contained / Jacketed", get(tank_wall_col), ignore.case = TRUE)
    ),
    single_walled = as.integer(
      grepl("Single Walled", get(tank_wall_col), ignore.case = TRUE)
    ),
    unknown_walled = as.integer(
      !grepl("Single Walled|Double Walled|Secondarily Contained / Jacketed", 
             get(tank_wall_col), ignore.case = TRUE) & !is.na(get(tank_wall_col))
    ),
    missing_walled = as.integer(
      is.na(get(tank_wall_col)) | get(tank_wall_col) == ""
    )
  )]
  
  # Create a summary table
  cat("\nTank wall classification summary for Oklahoma data:\n")
  print(result[, .(
    total_records = .N,
    single_walled_count = sum(single_walled, na.rm = TRUE),
    double_walled_count = sum(double_walled, na.rm = TRUE),
    unknown_walled_count = sum(unknown_walled, na.rm = TRUE),
    missing_walled_count = sum(missing_walled, na.rm = TRUE)
  )])
  
  return(result)
}

OK_UST_tanktype <- classify_tank_walls(OK_UST_tanks)

# # Create standardized wall type categories based on the specific Oklahoma groups
# OK_UST_tanks <- OK_UST_tanks %>%
#   mutate(
#     double_walled = case_when(
#       tank_construction %in% c("Double Walled", "Secondarily Contained / Jacketed") ~ 1,
#       TRUE ~ 0
#     ),
#     single_walled = case_when(
#       tank_construction == "Single Walled" ~ 1,
#       TRUE ~ 0
#     ),
#     unknown_walled = case_when(
#       !is.na(tank_construction) & 
#       !tank_construction %in% c("Single Walled", "Double Walled", "Secondarily Contained / Jacketed") ~ 1,
#       TRUE ~ 0
#     ),
#     missing_walled = case_when(
#       is.na(tank_construction) ~ 1,
#       TRUE ~ 0
#     )
#   )

# # Verify the wall type binary variables
# wall_type_summary = OK_UST_tanks %>% 
#   summarize(
#     total_records = n(),
#     single_walled_count = sum(single_walled),
#     double_walled_count = sum(double_walled),
#     missing_walled_count = sum(missing_walled),
#     unknown_walled_count = sum(unknown_walled)
#   )

# cat("Tank wall type summary:\n")
# print(wall_type_summary)

# Standardize capacity - keep numeric values instead of categories
OK_UST_tanks <- OK_UST_tanks %>%
  mutate(
    capacity_numeric = as.numeric(gsub("[^0-9.]", "", tank_capacity))
    # Remove the category conversion
  )

# Join the classified substance data back to main table
OK_UST_tanks <- OK_UST_tanks %>%
  left_join(
    OK_substances_classified %>% 
      select(facility_number, tank_number, is_gasoline, is_diesel, is_oil_kerosene, is_jet_fuel, is_other),
    by = c("facility_number", "tank_number")
  )


OK_UST_tanks <- OK_UST_tanks %>%
  left_join(
    OK_UST_tanktype %>% 
      select(facility_number, tank_number, single_walled, double_walled, unknown_walled, missing_walled),
    by = c("facility_number", "tank_number")
  )


# remove the brackets from OK_UST_tanks facility_number and make numeric
OK_UST_tanks$facility_number = gsub("\\[|\\]", "", OK_UST_tanks$facility_number)
OK_UST_tanks$facility_number = as.numeric(OK_UST_tanks$facility_number)

# make the facility number numeric in OK_LUSt and clean names
OK_LUSt = OK_LUSt %>% clean_names() %>%
    select(facility_number, case_number, case_type, release_date,close_date) %>% 
    filter(!is.na(facility_number)) %>% 
    mutate(facility_number = as.numeric(facility_number))

# left_join the two data sets and include the new variables
OK_UST_tanks_LUST = OK_UST_tanks %>%
    left_join(OK_LUSt, by = c("facility_number" = "facility_number")) %>%
    # make the dates date objects
    mutate(release_date = mdy(release_date),
           NFA_date = mdy(close_date),
           tank_installed_date = mdy(tank_installed_date),
           tank_closed_date = mdy(tank_closed_date)) %>%
   # make a new variable to id    Confirmed Release 1/0 in case_type
       mutate(CC = ifelse(case_type == "Confirmed Release", 1, 0)) %>%     
    mutate(
        leak_after_closure = ifelse(!is.na(tank_closed_date) & release_date > tank_closed_date & CC == 1, 1, 0),
        leak_before_NFA_before_closure = ifelse(!is.na(tank_closed_date) & release_date < tank_closed_date & tank_closed_date > NFA_date & CC == 1, 1, 0),
        leak_before_NFA_after_closure = ifelse(!is.na(tank_closed_date) & release_date < tank_closed_date & tank_closed_date < NFA_date & CC == 1, 1, 0),
        no_leak = ifelse(is.na(release_date) & !is.na(tank_closed_date) , 1, 0)
    ) %>% as.data.table()

### 
#whats the total number of closed unique ust tanks -- 27956
OK_UST_tanks$id = paste0(OK_UST_tanks$facility_number, "-", OK_UST_tanks$tank_number)
OK_UST_tanks = OK_UST_tanks %>% as.data.table()
OK_UST_tanks[,tank_closed_date:=mdy(tank_closed_date)]
number_of_closed_tanks = uniqueN(OK_UST_tanks[!is.na(tank_closed_date), id])
####

# count the number of unique facility-tank_id pairs by each of the four categories
#use data.table
count_leak_after_closure <- OK_UST_tanks_LUST[
    leak_after_closure == 1,
    uniqueN(paste0(facility_number, "-", tank_number))
]

count_leak_before_NFA_before_closure <- OK_UST_tanks_LUST[
    leak_before_NFA_before_closure == 1,
    uniqueN(paste0(facility_number, "-", tank_number))
]

count_leak_before_NFA_after_closure <- OK_UST_tanks_LUST[
    leak_before_NFA_after_closure == 1,
    uniqueN(paste0(facility_number, "-", tank_number))
]

count_no_leak <- OK_UST_tanks_LUST[
    no_leak == 1,
    uniqueN(paste0(facility_number, "-", tank_number))
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

############################################################################################################
## Tank data
############################################################################################################

### first collapse the facility-tank-LUST data to include the new variables
OK_UST_tanks_SD = OK_UST_tanks_LUST %>%
 select(facility_number, tank_number, tank_installed_date, tank_closed_date, 
        leak_after_closure, leak_before_NFA_before_closure, leak_before_NFA_after_closure, no_leak,
        is_gasoline, is_diesel, is_oil_kerosene, is_jet_fuel, is_other,
        single_walled, double_walled, unknown_walled, missing_walled, capacity_numeric) %>% # Replace capacity with capacity_numeric
 group_by(facility_number, tank_number, tank_installed_date, tank_closed_date) %>%
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
   capacity = mean(capacity_numeric, na.rm = TRUE) # Use mean of numeric capacity
 )

# using data.table change the sums to 1 if they are greater than 0
OK_UST_tanks_SD = OK_UST_tanks_SD %>% as.data.table()
OK_UST_tanks_SD = OK_UST_tanks_SD[, .(
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
  capacity = capacity # Keep numeric capacity
), keyby = .(facility_number, tank_number, tank_installed_date, tank_closed_date)]

# drop NA facility numbers
OK_UST_tanks_SD = OK_UST_tanks_SD[!is.na(facility_number)]

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

# Comment out the Oklahoma census geography data loading section
# Load the Oklahoma census geography data 
# oklahoma_census_geography = fread(here("Data", "Processed", "Census_Geography", "Oklahoma_census_geography.csv"))
# glimpse(oklahoma_census_geography)
# glimpse(OK_UST_tanks_SD)

# Rename facility_number to facility_id for consistency before adding zipcode data
setnames(OK_UST_tanks_SD, "facility_number", "facility_id")

# Add zipcode to county mapping using ZCTA relationship files
# Ensure zipcode is available in the processed data
if (!"facility_zip_code" %in% names(OK_UST_tanks_SD)) {
  # Add zipcode from the original tank data
  zipcode_data <- OK_UST_tanks %>%
    select(facility_number, tank_number, facility_zip_code) %>%
    distinct()
  
  # Rename facility_number to facility_id for consistency during merge
  setnames(zipcode_data, "facility_number", "facility_id")
  
  # Join zipcode to the standardized data
  OK_UST_tanks_SD <- OK_UST_tanks_SD %>%
    left_join(zipcode_data, by = c("facility_id",  "tank_number")) %>%
    as.data.table()
}

# Load necessary library
library(dplyr)

# Define the URL for the relationship file
url <- "https://www2.census.gov/geo/docs/maps-data/data/rel2020/zcta520/tab20_zcta520_county20_natl.txt"

# Download and read the relationship file using pipe delimiter
cw <- read.delim(url, sep = "|", header = TRUE, stringsAsFactors = FALSE, strip.white = TRUE)

# Convert numeric columns (areas) from character to numeric (trim any extra spaces)
cw <- cw %>%
  mutate(across(c(AREALAND_ZCTA5_20, AREAWATER_ZCTA5_20, 
                  AREALAND_COUNTY_20, AREAWATER_COUNTY_20,
                  AREALAND_PART, AREAWATER_PART),
                ~ as.numeric(trimws(.))))

# For each ZIP (GEOID_ZCTA5_20), select the county record with the largest overlapping land area
zip_to_county <- cw %>%
  group_by(GEOID_ZCTA5_20) %>%
  slice_max(order_by = AREALAND_PART, n = 1, with_ties = FALSE) %>%
  ungroup()

# Create a one-to-one ZIP-to-county crosswalk with the ZIP code, county GEOID, and county name
zip_to_county <- zip_to_county %>%
  select(GEOID_ZCTA5_20, GEOID_COUNTY_20, NAMELSAD_COUNTY_20) %>% as.data.table()

# Display the resulting crosswalk
View(zip_to_county)



# Ensure zipcode is character type in both datasets for proper joining
OK_UST_tanks_SD[, facility_zip_code := as.character(facility_zip_code)]
zip_to_county[, zipcode := as.character(GEOID_ZCTA5_20)]

# Join the county information to the tank data using zipcode
cat("\nJoining county information using zipcode data...\n")
OK_UST_tanks_SD <- OK_UST_tanks_SD %>%
  left_join(
    zip_to_county %>% 
      select(zipcode, county_fips = GEOID_COUNTY_20  ,  county_name=NAMELSAD_COUNTY_20),
    by = c('facility_zip_code' = "zipcode")
  ) %>%
  as.data.table()

View(OK_UST_tanks_SD)


# Report on the match rate
cat("\nZipcode to county match results:\n")
cat("Total facilities:", uniqueN(OK_UST_tanks_SD$facility_id), "\n")
cat("Facilities with zipcode data:", sum(!is.na(OK_UST_tanks_SD$facility_zip_code)), "\n")
cat("Facilities matched to county via zipcode:", 
    sum(!is.na(OK_UST_tanks_SD$county_fips)), "\n")
cat("Match rate:", 
    round(sum(!is.na(OK_UST_tanks_SD$facility_zip_code)) / 
            sum(!is.na(OK_UST_tanks_SD$facility_zip_code)) * 100, 2), 
    "%\n")

# Clean the facility_id in census data by removing brackets and making numeric
# oklahoma_census_geography[, facility_id := gsub("\\[|\\]", "", facility_id)]  # Remove brackets
# oklahoma_census_geography[, facility_id := as.numeric(facility_id)]  # Convert to numeric to match tank data

# Extract county and FIPS code columns from census data
# census_cols = setdiff(names(oklahoma_census_geography), c("facility_id"))

# Rename facility_number to facility_id for joining with census data
# setnames(OK_UST_tanks_SD, "facility_number", "facility_id")

# Print sample IDs from both datasets to verify format before joining
# cat("\nSample facility IDs before join:\n")
# cat("Tank data (first 5):", paste(head(OK_UST_tanks_SD$facility_id, 5), collapse=", "), "\n")
# cat("Census data (first 5):", paste(head(oklahoma_census_geography$facility_id, 5), collapse=", "), "\n")

# Join the census geography data to get county and FIPS information
# OK_UST_tanks_SD = OK_UST_tanks_SD %>%
#   left_join(oklahoma_census_geography, by = "facility_id") %>%
#   as.data.table()

# Verify the census join worked
# cat("\nAdded census geography data to tank data\n")
# for (col in census_cols) {
#   cat("Number of facilities with", col, "data:", sum(!is.na(OK_UST_tanks_SD[[col]])), "\n")
# }

# Ensure consistent column naming - rename county columns if needed
# if ("county" %in% names(OK_UST_tanks_SD)) {
#   cat("\nCounty data summary:\n")
#   cat("Unique county count:", uniqueN(OK_UST_tanks_SD$county), "\n")
#   cat("Facilities with county data:", sum(!is.na(OK_UST_tanks_SD$county)), "\n")
# }

# Ensure county_fips column is properly named
# if ("county_fips" %in% names(oklahoma_census_geography) && !("county_fips" %in% names(OK_UST_tanks_SD))) {
#   setnames(OK_UST_tanks_SD, "county_fips.y", "county_fips")
# } else if ("fips" %in% names(oklahoma_census_geography) && !("county_fips" %in% names(OK_UST_tanks_SD))) {
#   setnames(OK_UST_tanks_SD, "fips", "county_fips")
# }

# Update the message to clarify we're only using the zipcode approach
cat("\nUsing only zipcode to county mapping for geographic data\n")

# Add county FIPS code matching
# Get county information from the raw data if COUNTYVARIABLE is available

# Final verification that column names are as expected
cat("\nFinal Oklahoma UST tank data column names:\n")
print(names(OK_UST_tanks_SD)[1:min(length(names(OK_UST_tanks_SD)), length(required_columns) + 5)])  # Show first few columns

# Update all column names to standard format
setnames(OK_UST_tanks_SD, 
         c("tank_number", "tank_installed_date", "tank_closed_date", 
           "leak_after_closure", "leak_before_NFA_before_closure", "leak_before_NFA_after_closure", "no_leak"),
         c("tank_id", "tank_installed_date", "tank_closed_date",
           "leak_after_closure", "leak_before_NFA_before_closure", "leak_before_NFA_after_closure", "no_leak"))

# Define required columns order (similar to Arkansas file)
required_columns = c(
  "facility_id", "tank_id", "tank_installed_date", "tank_closed_date",
  "leak_after_closure", "leak_before_NFA_before_closure", "leak_before_NFA_after_closure", "no_leak",
  "capacity", "single_walled", "double_walled", "missing_walled", "unknown_walled",
  "is_gasoline", "is_diesel", "is_oil_kerosene", "is_jet_fuel", "is_other"
)

############################################################################################################
## LUST data
############################################################################################################

# the lust data should be the following columns
## facility_id
## LUST_id
## report_date
## nfa_date

OK_LUST_SD = OK_LUSt %>% clean_names() %>%
 select(facility_number, case_number, release_date, close_date) %>%
 rename(
   facility_id = facility_number,
   LUST_id = case_number,
   report_date = release_date,
   nfa_date = close_date
 )

# Verify there's no trailing whitespace in facility IDs
cat("\nChecking for whitespace in facility IDs:\n")
cat("Facility IDs with trailing whitespace in OK_UST_tanks_SD:", 
    sum(OK_UST_tanks_SD$facility_id != trimws(OK_UST_tanks_SD$facility_id)), "\n")
cat("Facility IDs with trailing whitespace in OK_LUST_SD:", 
    sum(OK_LUST_SD$facility_id != trimws(OK_LUST_SD$facility_id)), "\n")

# Add county and census columns to the required column list if they exist
additional_columns = setdiff(names(OK_UST_tanks_SD), required_columns)
OK_UST_tanks_SD[,'county':= county_name]
 OK_UST_tanks_SD[,'county_fips':= county_fips]


# Reorder columns if county exists, put it first among additional columns
if ("county" %in% additional_columns) {
  setcolorder(OK_UST_tanks_SD, c(required_columns, "county", setdiff(additional_columns, "county")))
} else {
  setcolorder(OK_UST_tanks_SD, c(required_columns, additional_columns))
}

# Check if OK_UST_tanks_SD has the required columns before writing
required_cols <- c("is_gasoline", "is_diesel", "is_oil_kerosene", "is_jet_fuel", "is_other")
missing_cols <- required_cols[!required_cols %in% names(OK_UST_tanks_SD)]

if (length(missing_cols) > 0) {
  # If columns are missing, add them from the original classified data
  cat("Adding missing substance classification columns:", paste(missing_cols, collapse=", "), "\n")
  
  # Create a mapping table with just facility_id, tank_id and the substance columns
  substance_mapping <- OK_UST_tanks %>%
    select(facility_id = facility_number, tank_id = tank_number, 
           any_of(c("is_gasoline", "is_diesel", "is_oil_kerosene", "is_jet_fuel", "is_other")))
  
  # Join the substance data to OK_UST_tanks_SD
  OK_UST_tanks_SD <- OK_UST_tanks_SD %>%
    left_join(substance_mapping, by = c("facility_id", "tank_id")) %>%
    as.data.table()
  
  # Fill any NA values with 0 in the substance columns
  for (col in required_cols) {
    if (col %in% names(OK_UST_tanks_SD)) {
      OK_UST_tanks_SD[is.na(get(col)), (col) := 0]
    } else {
      OK_UST_tanks_SD[, (col) := 0]
    }
  }
}

# write out the data
fwrite(OK_UST_tanks_SD, here("Data", "Raw_do_not_write", "state_databases", "Oklahoma", "OK_UST_tanks.csv"))
fwrite(OK_LUST_SD, here("Data", "Raw_do_not_write", "state_databases", "Oklahoma", "OK_LUST.csv"))

