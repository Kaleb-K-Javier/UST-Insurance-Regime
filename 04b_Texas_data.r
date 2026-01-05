## Texas UST Dataset Creation

# Define the county variable name in the raw data
COUNTYVARIABLE <- "SITE_LOCATION_COUNTY"

library(here)
#first_run = FALSE
first_run = TRUE
#load packags ----
source(here('Code','00_global_packages.R'))



##########
#https://www.tceq.texas.gov/compliance/enforcement/compliance-history/get_list.html - compliance data

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

# Load Raw Data -----
## should subset to UST Financial Assurance Required (Y =Yes, N = No) only the yes ones
# Load Texas UST and LUST data -----
texas_facility = fread(here("Data", "Raw_do_not_write", "state_databases", "Texas", "texas_facility.csv"))
texas_ust_data = fread(here("Data", "Raw_do_not_write", "state_databases", "Texas", "texas_ust.csv"))
texas_LUST = fread(here("Data", "Raw_do_not_write", "state_databases", "Texas", "texas_LUST.csv"))
texas_compartment = fread(here("Data", "Raw_do_not_write", "state_databases", "Texas", "texas_compartment.csv"))
## Standardize the column names and save to folder



## turn the begin date into a date object its mdy
texas_facility = texas_facility %>% mutate(FACILITY_BEGIN_DATE = mdy(FACILITY_BEGIN_DATE)) %>% 
mutate(beign_year = year(FACILITY_BEGIN_DATE)) %>% as.data.table()

#count the number of facilities with missing site_address and facility_begin_date by facility_type

address_missing = texas_facility[SITE_ADDRESS == "" | is.na(SITE_ADDRESS), .N, by = .(beign_year,FACILITY_TYPE)]
view(address_missing)
sum(address_missing$N)
## count the number of facilites by facility type
facility_type_count = texas_facility[, .N, by = .(FACILITY_TYPE)]
### make shares
facility_type_count[, share := N / sum(N)]
#
view(facility_type_count)



############################################################################################################
## Tank wall type binary variables
############################################################################################################

# Create binary variables for tank wall type
texas_ust_data = texas_ust_data %>% 
  mutate(
    single_walled = ifelse(TANK_DESIGN_SINGLE_WALL == "Y", 1, 0),
    double_walled = ifelse(TANK_DESIGN_DOUBLE_WALL == "Y", 1, 0),
    missing_walled = ifelse(is.na(TANK_DESIGN_SINGLE_WALL) & is.na(TANK_DESIGN_DOUBLE_WALL), 1, 0),
    unknown_walled = ifelse(TANK_DESIGN_SINGLE_WALL == "N" & TANK_DESIGN_DOUBLE_WALL == "N", 1, 0)
  )

# Verify the wall type binary variables
wall_type_summary = texas_ust_data %>% 
  summarize(
    total_records = n(),
    single_walled_count = sum(single_walled),
    double_walled_count = sum(double_walled),
    missing_walled_count = sum(missing_walled),
    unknown_walled_count = sum(unknown_walled)
  )

cat("Tank wall type summary:\n")
print(wall_type_summary)

############################################################################################################
## Substance classification
############################################################################################################


# Combine substance data from compartments
# First, create a combined substance column for each tank

texas_compartment_combined = texas_compartment %>% 
  group_by(facility_id, tank_id,ust_id) %>% 
  summarize(
    substances = paste(c(substance_stored_1, substance_stored_2, substance_stored_3), collapse = "; ")
  ) %>% 
  mutate(
    substances = gsub("; NA", "", gsub("NA; ", "", gsub("NA", "", substances)))  ) %>% 
  ungroup() %>% as.data.table()




  # Function to classify substances
  classify_substances <- function(data) {
    # Create binary classification columns with default values
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
    result[, substances_lower := tolower(substances)]
    
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
      gasoline_count = sum(is_gasoline),
      diesel_count = sum(is_diesel),
      oil_kerosene_count = sum(is_oil_kerosene),
      jet_fuel_count = sum(is_jet_fuel),
      other_count = sum(is_other),
      multi_category_count = sum(is_gasoline + is_diesel + is_oil_kerosene + is_jet_fuel > 1)
    )])
    
    return(result)
  }


# Apply substance classification
texas_compartment_classified <- classify_substances(texas_compartment_combined)

glimpse(texas_compartment_classified)

# Check for unclassified substances
unclassified <- texas_compartment_classified[is_gasoline + is_diesel + is_oil_kerosene + is_jet_fuel + is_other == 0]
if (nrow(unclassified) > 0) {
  warning(paste("Found", nrow(unclassified), "unclassified substances"))
  print(head(unclassified[, .(substances, count = .N), by = substances][order(-count)]))
}



############################################################################################################
## Tank data
############################################################################################################
view(texas_ust_data)

# Create minimal UST dataset with tank capacity and wall type
TX_UST_tanks = texas_ust_data %>% 
  clean_names() %>% 
  select(facility_id, tank_id,ust_id, tank_installation_date, tank_status_begin_date, tank_status, 
         tank_capacity, single_walled, double_walled, missing_walled, unknown_walled) %>% 
  rename(
    tank_installed_date = tank_installation_date
  ) %>% 
  mutate(tank_closed_date = tank_status_begin_date)

# tally the number of tanks by status
TX_UST_tanks %>% count(tank_status, sort = TRUE)
#now change the tank_closed_date to be the tank_status_begin_date if the tank_status does not have the word 'USE'
TX_UST_tanks = TX_UST_tanks %>% 
  mutate(
    tank_closed_date = ifelse(!grepl("USE", tank_status), tank_status_begin_date, NA)
  )

# Leak data

# Create minimal LUST dataset
TX_LUST_data = texas_LUST %>% 
  clean_names() %>% 
  select(pst_registration_id, lpst_id, reported_date, closure_date)
  
lust_fac = TX_LUST_data[, .(facility_id = unlist(strsplit(pst_registration_id, ";", fixed = TRUE))),
 by = .(lpst_id,pst_registration_id, reported_date, closure_date)]


#make both merge keys characters
TX_UST_tanks = TX_UST_tanks %>% mutate(facility_id = as.character(facility_id))
lust_fac = lust_fac %>% mutate(facility_id = as.character(facility_id))


# Convert compartment ID columns to character for joining
texas_compartment_classified = texas_compartment_classified %>% 
  mutate(facility_id = as.character(facility_id), tank_id = as.character(tank_id))
glimpse(texas_compartment_classified)
glimpse(TX_UST_tanks)
nrow(texas_compartment_classified)
nrow(TX_UST_tanks)
# Join substance classification data
TX_UST_tanks = TX_UST_tanks %>% 
  left_join(texas_compartment_classified %>% 
              select(facility_id, tank_id, is_gasoline, is_diesel, is_oil_kerosene, is_jet_fuel, is_other),
            by = c("tank_id",'facility_id'))
        
nrow(TX_UST_tanks)
TX_UST_tanks_LUST = TX_UST_tanks %>% 
    left_join(lust_fac, by = c("facility_id" = "facility_id")) %>% 
    # make the dates date objects
    mutate(release_date = dmy(reported_date),
           NFA_date = dmy(closure_date),
           tank_installed_date = mdy(tank_installed_date),
           tank_closed_date = mdy(tank_closed_date)) %>% 
    mutate(
        leak_after_closure = ifelse(!is.na(tank_closed_date) & release_date > tank_closed_date, 1, 0),
        leak_before_NFA_before_closure = ifelse(!is.na(tank_closed_date) & release_date < tank_closed_date & tank_closed_date > NFA_date, 1, 0),
        leak_before_NFA_after_closure = ifelse(!is.na(tank_closed_date) & release_date < tank_closed_date & tank_closed_date < NFA_date, 1, 0),
        no_leak = ifelse(is.na(release_date) & !is.na(tank_closed_date), 1, 0)
    ) %>% as.data.table()

### Checking what is the relationship between facility_type and substance types

#### left join on the FACILITY_TYPE to TX_UST_tanks
TX_UST_tanks = TX_UST_tanks %>% as.data.table()
texas_facility = texas_facility %>% as.data.table()
TX_UST_tanks = TX_UST_tanks %>% left_join(texas_facility %>% select(FACILITY_ID,FACILITY_TYPE) %>% mutate(FACILITY_ID = as.character(FACILITY_ID)), 
by = c("facility_id"="FACILITY_ID")) %>% as.data.table()

## do the cross_tab of is_gasoline, is_diesel, is_oil_kerosene, is_jet_fuel, is_other by facility_type

facilites_and_substance = TX_UST_tanks[, .(
  gasoline = sum(is_gasoline, na.rm = TRUE),
  diesel = sum(is_diesel, na.rm = TRUE),
  oil_kerosene = sum(is_oil_kerosene, na.rm = TRUE),
  jet_fuel = sum(is_jet_fuel, na.rm = TRUE),
  other = sum(is_other, na.rm = TRUE)
), by = .(FACILITY_TYPE)]

# Add totals and percent columns for each substance category
facilites_and_substance[, total := gasoline + diesel + oil_kerosene + jet_fuel + other]
facilites_and_substance[, gasoline_pct := gasoline / total]
facilites_and_substance[, diesel_pct := diesel / total]
facilites_and_substance[, oil_kerosene_pct := oil_kerosene / total]
facilites_and_substance[, jet_fuel_pct := jet_fuel / total]
facilites_and_substance[, other_pct := other / total]

View(facilites_and_substance)

# Graph showing the number of new gasoline/diesel facilities per year
# Find the earliest tank installation date for each facility with gas or diesel



# What's the total number of closed unique UST tanks --143087---NEW 143680
TX_UST_tanks$id = paste0(TX_UST_tanks$facility_id, "-", TX_UST_tanks$tank_id)
TX_UST_tanks = TX_UST_tanks %>% as.data.table()
TX_UST_tanks[, tank_closed_date := mdy(tank_closed_date)]
number_of_closed_tanks = uniqueN(TX_UST_tanks[!is.na(tank_closed_date), id])

    # count the number of unique facility-tank_id pairs by each of the four categories
    # use data.table
    count_leak_after_closure <- TX_UST_tanks_LUST[
        leak_after_closure == 1,
        uniqueN(paste0(facility_id, "-", tank_id))
    ]

    count_leak_before_NFA_before_closure <- TX_UST_tanks_LUST[
        leak_before_NFA_before_closure == 1,
        uniqueN(paste0(facility_id, "-", tank_id))
    ]

    count_leak_before_NFA_after_closure <- TX_UST_tanks_LUST[
        leak_before_NFA_after_closure == 1,
        uniqueN(paste0(facility_id, "-", tank_id))
    ]

    count_no_leak <- TX_UST_tanks_LUST[
        no_leak == 1,
        uniqueN(paste0(facility_id, "-", tank_id))
    ]

    # Combine counts into a data frame
    counts <- data.frame(
      category = c("leak_after_closure", "leak_before_NFA_before_closure", "leak_before_NFA_after_closure", "no_leak"),
      count = c(count_leak_after_closure, count_leak_before_NFA_before_closure, count_leak_before_NFA_after_closure, count_no_leak)
    )

    # create the total number of obs by summing count
    counts$total = sum(counts$count)
    counts$percent = counts$count / counts$total
    # Print the counts data frame
    print(counts)
    print(number_of_closed_tanks)


## Standardize the column names and save to folder

### First, collapse the facility-tank-LUST data to facility-tank level, summing up leaks and no leaks variables

TX_UST_tanks_SD = TX_UST_tanks_LUST %>% 
    select(facility_id, tank_id, tank_installed_date, tank_closed_date, leak_after_closure, 
           leak_before_NFA_before_closure, leak_before_NFA_after_closure, no_leak,
           tank_capacity, single_walled, double_walled, missing_walled, unknown_walled,
           is_gasoline, is_diesel, is_oil_kerosene, is_jet_fuel, is_other) %>% 
    group_by(facility_id, tank_id, tank_installed_date, tank_closed_date, tank_capacity, 
             single_walled, double_walled, missing_walled, unknown_walled,
             is_gasoline, is_diesel, is_oil_kerosene, is_jet_fuel, is_other) %>% 
    summarize(
        leak_after_closure = sum(leak_after_closure, na.rm = TRUE),
        leak_before_NFA_before_closure = sum(leak_before_NFA_before_closure, na.rm = TRUE),
        leak_before_NFA_after_closure = sum(leak_before_NFA_after_closure, na.rm = TRUE),
        no_leak = sum(no_leak, na.rm = TRUE)
    )
names(TX_UST_tanks_SD)
# Using data.table to change the sums to 1 if they are greater than 0
TX_UST_tanks_SD = TX_UST_tanks_SD %>% as.data.table()
TX_UST_tanks_SD = TX_UST_tanks_SD[, .(
    leak_after_closure = ifelse(leak_after_closure > 0, 1, 0),
    leak_before_NFA_before_closure = ifelse(leak_before_NFA_before_closure > 0, 1, 0),
    leak_before_NFA_after_closure = ifelse(leak_before_NFA_after_closure > 0, 1, 0),
    no_leak = ifelse(no_leak > 0, 1, 0),
    tank_capacity = tank_capacity,
    single_walled = single_walled,
    double_walled = double_walled,
    missing_walled = missing_walled,
    unknown_walled = unknown_walled,
    is_gasoline = is_gasoline,
    is_diesel = is_diesel,
    is_oil_kerosene = is_oil_kerosene,
    is_jet_fuel = is_jet_fuel,
    is_other = is_other
), keyby = .(facility_id, tank_id, tank_installed_date, tank_closed_date)]

# Drop NA facility numbers
TX_UST_tanks_SD = TX_UST_tanks_SD[!is.na(facility_id)]

# Make sure the table names are as follows for the essential columns

# Fill NA values with zeros for classification columns
cols_to_fill <- c("is_gasoline", "is_diesel", "is_oil_kerosene", "is_jet_fuel", "is_other",
                 "single_walled", "double_walled", "missing_walled", "unknown_walled")
for(col in cols_to_fill) {
  TX_UST_tanks_SD[is.na(get(col)), (col) := 0]
}

glimpse(TX_UST_tanks_SD)

############################################################################################################
## LUST data
############################################################################################################
nrow(TX_UST_tanks)

# The LUST data should include the following columns
## facility_id
## LUST_id
## report_date
## nfa_date

TX_LUST_SD = lust_fac %>% clean_names() %>%
    select(facility_id, lpst_id, reported_date, closure_date) %>% 
    rename(
        LUST_id = lpst_id,
        report_date = reported_date,
        nfa_date = closure_date
    )

############################################################################################################
## Add County Information and Census Geography Data to UST data
############################################################################################################

# First, extract county information from texas_facility table
county_data = texas_facility %>%
  select(FACILITY_ID,SITE_LOCATION_COUNTY ) %>%
  clean_names() %>%
  rename(county = !!names(.)[2]) %>%  # Dynamically rename the county column
  mutate(facility_id = as.character(facility_id)) %>%  # Convert facility_id to character
  as.data.table()

# Display summary of county data
cat("County data summary:\n")
cat("Unique facility count:", uniqueN(county_data$facility_id), "\n")
cat("Unique county count:", uniqueN(county_data$county), "\n")
cat("Facilities with county data:", sum(!is.na(county_data$county)), "\n")

# Join county data to the UST tanks data
TX_UST_tanks_SD = TX_UST_tanks_SD %>%
  left_join(county_data, by = "facility_id") %>%
  as.data.table()

# Verify the county join worked
cat("\nAdded county data to tank data\n")
cat("Number of facilities with county data:", sum(!is.na(TX_UST_tanks_SD$county)), "\n")

cat("\nCensus geography data merge commented out pending GIS code fixes\n")

# Add county FIPS code matching
# Standardize county names


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



TX_UST_tanks_SD[, standardized_county := standardize_county_name(county)]

cat("DEBUG: Loading county FIPS data from tigris Census package...\n")
# Use tigris package to load county FIPS codes
if (!require(tigris)) install.packages("tigris")
library(tigris)

# Load counties from the Census Bureau
counties <- counties(state = "TX", cb = TRUE)
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
TX_UST_tanks_SD <- merge(
  TX_UST_tanks_SD,
  fips_data[, .(standardized_county, county_fips = county_fips_code)],
  by = "standardized_county",
  all.x = TRUE
)
nrow(TX_UST_tanks_SD)

# Report on the match rate
cat("\nCounty FIPS code matching summary:\n")
cat("Total facilities:", uniqueN(TX_UST_tanks_SD$facility_id), "\n")
cat("Facilities with county FIPS codes:", sum(!is.na(TX_UST_tanks_SD$county_fips)), "\n")
cat("Match rate:", round(sum(!is.na(TX_UST_tanks_SD$county_fips)) / uniqueN(TX_UST_tanks_SD$facility_id) * 100, 2), "%\n")

# Clean up temp column
TX_UST_tanks_SD[, standardized_county := NULL]

# Rename tank_capacity to capacity to match EPA state data structure
setnames(TX_UST_tanks_SD, "tank_capacity", "capacity")

# Make sure the table names are as follows for the essential columns
# CHANGE: Keep tank_installed_date and tank_closed_date instead of installation_date and removal_date
# to match naming conventions in other state files

# Update required columns to use consistent naming
required_columns = c(
  "facility_id", "tank_id", "tank_installed_date", "tank_closed_date",
  "leak_after_closure", "leak_before_NFA_before_closure", "leak_before_NFA_after_closure", "no_leak",
  "capacity", "single_walled", "double_walled", "missing_walled", "unknown_walled",
  "is_gasoline", "is_diesel", "is_oil_kerosene", "is_jet_fuel", "is_other"
)
# Add county and census columns to the required column list if they exist
additional_columns = setdiff(names(TX_UST_tanks_SD), required_columns)

# Reorder columns to match the standard structure with county first among additional columns
setcolorder(TX_UST_tanks_SD, c(required_columns, "county", setdiff(additional_columns, "county")))

# Final verification that column names match
cat("\nFinal UST tank data column names:\n")
print(names(TX_UST_tanks_SD)[1:length(required_columns) + 1])  # +1 to include county

# Write out the data
fwrite(TX_UST_tanks_SD, here("Data", "Raw_do_not_write", "state_databases", "Texas", "TX_UST_tanks.csv"))
fwrite(TX_LUST_SD, here("Data", "Raw_do_not_write", "state_databases", "Texas", "TX_LUST.csv"))


glimpse(TX_UST_tanks_SD)
nrow(texas_ust_data)


# Calculate earliest installations for raw data (TX_UST_tanks)
earliest_installations <- TX_UST_tanks %>%
  filter((is_gasoline == 1 | is_diesel == 1) & !is.na(tank_installed_date)) %>%
  group_by(facility_id) %>%
  mutate(tank_installed_date = mdy(tank_installed_date)) %>%
  summarize(first_installation = min(tank_installed_date, na.rm = TRUE))
glimpse(earliest_installations)
# Calculate earliest installations for cleaned data (TX_UST_tanks_SD)
glimpse(TX_UST_tanks_SD)
earliest_installations_SD <- TX_UST_tanks_SD %>%
  filter((is_gasoline == 1 | is_diesel == 1) & !is.na(tank_installed_date)) %>%
  group_by(facility_id) %>%
  summarize(first_installation = min(tank_installed_date, na.rm = TRUE))

# Count facilities by year for raw data
facilities_by_year <- earliest_installations %>%
  mutate(installation_year = year((first_installation))) %>%
  filter(installation_year >= 1970 & installation_year <= 2023) %>%
  count(installation_year) %>%
  mutate(source = "Raw Data (TX_UST_tanks)")

# Count facilities by year for cleaned data
facilities_by_year_SD <- earliest_installations_SD %>%
  mutate(installation_year = year(first_installation)) %>%
  filter(installation_year >= 1970 & installation_year <= 2023) %>%
  count(installation_year) %>%
  mutate(source = "Cleaned Data (TX_UST_tanks_SD)")

# Combine the datasets
combined_facilities <- bind_rows(facilities_by_year, facilities_by_year_SD)

# Create the faceted plot
ggplot(combined_facilities %>% filter(installation_year > 1970), aes(x = installation_year, y = n,color = source)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = seq(1970, 2020, by = 5)) +
  #facet_grid(source ~ ., scales = "free_y") +
  theme_minimal() +
  labs(
    title = "Number of New Gasoline/Diesel Facilities by Year",
    x = "Year",
    y = "Number of New Facilities",
    caption = "Source: Texas UST Data"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#show me the number of new sw tanks by year from 1970 to 2020

# Count facilities by year for cleaned data (TX_UST_tanks)
facilities_by_year_sw <- TX_UST_tanks_SD %>%
  filter(single_walled == 1 & !is.na(tank_installed_date)) %>%
 # mutate(tank_installed_date = mdy(tank_installed_date)) %>%
  mutate(installation_year = year(tank_installed_date)) %>%
  filter(installation_year >= 1970 & installation_year <= 2023) %>%
  group_by(installation_year) %>% tally(n()) %>%
  ungroup() %>%
  mutate(source = "Cleaned Data (TX_UST_tanks)")


# plot the number of new sw tanks by year from 1970 to 2020
ggplot(facilities_by_year_sw %>% filter(installation_year > 1970), aes(x = installation_year, y = n)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = seq(1970, 2020, by = 5)) +
  theme_minimal() +
  labs(
    title = "Number of New Single-Walled Facilities by Year",
    x = "Year",
    y = "Number of New Facilities",
    caption = "Source: Texas UST Data"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


  ggplot(facilities_by_year_sw %>% filter(installation_year > 1988), aes(x = installation_year, y = n)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = seq(1970, 2020, by = 5)) +
  theme_minimal() +
  labs(
    title = "Number of New Single-Walled Facilities by Year",
    x = "Year",
    y = "Number of New Facilities",
    caption = "Source: Texas UST Data"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))