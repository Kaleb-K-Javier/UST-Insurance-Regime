## Epa  UST Dataset Creation

# Define the county variable name in the raw data
# For EPA data, the county variable name may differ between states
# This will be used as a default if available
COUNTYVARIABLE <- "county_name"

library(here)
first_run = FALSE
#first_run = TRUE
#load packags ----
library(tidyverse)
library(data.table)
library(here)
library(lubridate)
library(janitor)
library(ggplot2)
library(knitr)
library(kableExtra)
library(fixest)
library(ggpubr)
library(gghighlight)
library(did)
library(fastDummies)
library(fplot)
library(ggrepel)
library(patchwork)
library(DRDID)



# Add the substance classification function from Texas script - updated column names
classify_substances <- function(data, substance_col_name = "substances") {
  # Create binary classification columns with default value
  result <- copy(data)
  result[, `:=`(
    is_gasoline = 0,
    is_diesel = 0,
    is_oil_kerosene = 0,
    is_jet_fuel = 0,
    is_other = 1  # Default to 1, will set to 0 if any other category matches
  )]
  
# Classification patterns for EPA data
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
  
  # Create a summary table of the classifications
  cat("\nSubstance classification summary for", deparse(substitute(data)), ":\n")
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

# Function to classify tank wall types - updated to handle all EPA tank wall variations
classify_tank_walls <- function(data, tank_wall_col = "tank_wall_type") {
  result <- copy(data)
  
  # Create binary variables for tank wall type
  result[, `:=`(
    double_walled = as.integer(grepl("DOUBLE|TRIPLE|DBL|DUAL|SECONDAR", toupper(get(tank_wall_col)), ignore.case = TRUE)),
    single_walled = as.integer(grepl("SINGLE", toupper(get(tank_wall_col)), ignore.case = TRUE) & 
                              !grepl("UNKNOWN", toupper(get(tank_wall_col)), ignore.case = TRUE)),
    unknown_walled = as.integer(grepl("UNKNOWN", toupper(get(tank_wall_col)), ignore.case = TRUE)),
    missing_walled = as.integer(is.na(get(tank_wall_col)) | get(tank_wall_col) == "")
  )]
  
  # Create a summary table
  cat("\nTank wall classification summary for", deparse(substitute(data)), ":\n")
  print(result[, .(
    total_records = .N,
    single_walled_count = sum(single_walled, na.rm = TRUE),
    double_walled_count = sum(double_walled, na.rm = TRUE),
    unknown_walled_count = sum(unknown_walled, na.rm = TRUE),
    missing_walled_count = sum(missing_walled, na.rm = TRUE)
  )])
  
  return(result)
}
# Function to standardize capacity values - keep numeric values
standardize_capacity <- function(data, capacity_col = "capacity") {
  result <- copy(data)
  
  result[, `:=`(
    capacity = as.numeric(gsub("[^0-9.]", "", get(capacity_col)))
    # Remove the category creation
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



# Load Raw Data -----

### Load EPA leak data
leak_data_epa_01 = fread(here("Data","Raw_do_not_write","epa_ust_data_1_8_2024","Releases_1_edited.csv")) %>% 
  janitor::clean_names() %>%
  as.data.table()

### Load EPA UST data
state_UST_full = fread(here("Data","Raw_do_not_write","epa_ust_data_1_8_2024","USTs_4.csv")) %>% 
  clean_names() %>%
  as.data.table()

view(state_UST_full)

#group by and tally the number of tanks by substance
content = state_UST_full[, .(n = uniqueN(facility_id)), by = .(substances)]
# save this out to folder
fwrite(content, here("Data","Raw_do_not_write","epa_ust_data_1_8_2024","USTs_4_content.csv"))
tank_wall_type = state_UST_full[, .(n = uniqueN(facility_id)), by = .(tank_wall_type)]
tank_wall_type = tank_wall_type[order(-n)]$tank_wall_type
epa_data <- fread("C:/Users/kaleb/Box/UST-Insurance/Data/Raw_do_not_write/epa_ust_data_1_8_2024/Facilities_edited_by_kj.csv") %>% 
  janitor::clean_names() %>%
  as.data.table()

  view(epa_data)
# Left join county information from epa_data to state_UST_full using facility_id
state_UST_full <- dplyr::left_join(state_UST_full, epa_data[, c("facility_id", "county")], by = "facility_id") %>% 
  as.data.table()

# Get all census geography files
census_files <- list.files(
  "C:/Users/kaleb/Box/UST-Insurance/Data/Processed/Census_Geography",
  pattern = "\\.csv$",
  full.names = TRUE
)

# Create a mapping of state names to census files
census_file_mapping <- data.table(
  file_path = census_files,
  state_name = gsub("_census_geography.csv", "", basename(census_files))
)
census_file_mapping[, state_name := gsub("_", " ", state_name)]
print(census_file_mapping)




## inpsect pennsylvania data
PA = state_UST_full[state == "Pennsylvania"]
LA = state_UST_full[state == "Louisiana"]
View(LA)
## a bunch of states have not tank type info???
#         state technology_type     n type_counts
#  1:  Illinois         Unknown 11225           1
#  2: Louisiana         Unknown 12408           1 -- fixed  will use the record request data.
#  3:        MI   Double-Walled  2935           2
#  4:        MI         Unknown 13553           2
#  5:     Maine   Double-Walled   865           2
#  6:     Maine         Unknown  2948           2
#  7:        NJ         Unknown 16057           1
#  8:        NM   Double-Walled    16           2
#  9:        NM         Unknown  3551           2
# 10:        PA         Unknown 21467           1

#group by the states listed above and tank type
missing_state_list = c('Illinois','Louisiana','Michigan','Maine','New Jersey','Pennsylvania','New Mexico')
state_UST_full[state %in% missing_state_list, .(n = uniqueN(facility_id)), by = .(state, tank_wall_type)]

#            state tank_wall_type     n
#  1:     Illinois                28233 - Need to do record Request !!!!!!!!!!!!!!!!!!!!!!!!!!!! drop for now from wall figure
#  2:    Louisiana                16324-- fixed  will use the record request data.
#  3:        Maine                 5718
#  4:        Maine         Double  1168
#  5:     Michigan                26594
#  6:     Michigan         Double  4037
#  7:   New Mexico                 6047 - I'll email Zach to ask with the list of ids and the data feilds.
#  8:   New Jersey                31629 - fixed will use the public data
#  9: Pennsylvania                32436 - I cant fix atm, need to do record request or scrape. Looks like I can only get installed UST tank types?
# 10:     Michigan  Double Walled    11
# 11:   New Mexico  Double Walled    19
# 12:   New Mexico  Single Walled     1


# general cleaning function ----
process_state_data = function(state_name) {
  dir_path = here("Data", "Raw_do_not_write", "state_databases", state_name)
  if (!dir.exists(dir_path)) {
    dir.create(dir_path)
  }


  cat("\nProcessing data for", state_name, "\n")  
  state_leak_data = leak_data_epa_01[state == state_name, .(facility_id, lust_id, reported_date_converted = mdy_hm(reported_date), reported_date)]


  state_UST = state_UST_full[state == state_name]

  # Get additional tank details where available
  state_UST_tanks = state_UST %>% 
    select(facility_id, tank_id, installation_date, removal_date, tank_status, 
           substances, tank_wall_type, capacity, county) %>%   # Added county column if available
    rename(
      tank_installed_date = installation_date,
      tank_closed_date = removal_date,
      county_name = county # Rename to standard county_name
    ) %>% 
    as.data.table()

  # Classify tank substances if substances column exists and has data
  if ("substances" %in% names(state_UST_tanks) && 
      sum(!is.na(state_UST_tanks$substances) & state_UST_tanks$substances != "") > 0) {
    cat("\nClassifying substances for", state_name, "\n")
    state_UST_tanks <- classify_substances(state_UST_tanks, "substances")  # Using updated column name
  } else {
    # Add empty classification columns if no substance data
    state_UST_tanks[, `:=`(
      is_gasoline = 0,
      is_diesel = 0,
      is_oil_kerosene = 0, 
      is_jet_fuel = 0,
      is_other = 1
    )]
    cat("\nNo substance data available for", state_name, ". Using default classifications.\n")
  }

  # Classify tank walls if tank_wall_type column exists and has data
  if ("tank_wall_type" %in% names(state_UST_tanks) && 
      sum(!is.na(state_UST_tanks$tank_wall_type) & state_UST_tanks$tank_wall_type != "") > 0) {
    cat("\nClassifying tank walls for", state_name, "\n")
    state_UST_tanks <- classify_tank_walls(state_UST_tanks, "tank_wall_type")  # Using updated column name
  } else {
    # Add empty classification columns if no tank wall data
    state_UST_tanks[, `:=`(
      double_walled = 0,
      single_walled = 0,
      unknown_walled = 1,
      missing_walled = 1
    )]
    cat("\nNo tank wall data available for", state_name, ". Using default classifications.\n")
  }

  # Standardize capacity values if capacity column exists and has data
  if ("capacity" %in% names(state_UST_tanks) && 
      sum(!is.na(state_UST_tanks$capacity) & state_UST_tanks$capacity != "") > 0) {
    cat("\nStandardizing capacity values for", state_name, "\n")
    state_UST_tanks <- standardize_capacity(state_UST_tanks, "capacity")  # Using updated column name
  } else {
    # Add empty capacity column if no capacity data
    state_UST_tanks[, capacity := NA_real_]
    cat("\nNo capacity data available for", state_name, ". Using default values.\n")
  }
## drop NA values in the leak data data and LUST ID
  state_leak_data <- state_leak_data[!is.na(lust_id) & !is.na(reported_date_converted)]

  # Count leaks by facility and date
state_leak_data_collapsed <- state_leak_data[, .(LUST_count = .N), by = .(facility_id, reported_date_converted)]



  # Merge leak data with tank data
  state_UST_tanks_LUST = merge.data.table(state_UST_tanks, state_leak_data_collapsed, by = "facility_id", all.x = TRUE,
  allow.cartesian = TRUE)

glimpse(state_UST_tanks_LUST)
 
 # make the tank dates date time variables they are mdy_hms use lubridate
  state_UST_tanks_LUST = state_UST_tanks_LUST %>% 
    mutate(
      tank_installed_date = mdy_hms(tank_installed_date),
      tank_closed_date = mdy_hms(tank_closed_date)
    ) %>% 
    as.data.table()
glimpse(state_UST_tanks_LUST)


  # Create binary leak indicators
  state_UST_tanks_LUST = state_UST_tanks_LUST %>% 
    mutate(
      leak_after_closure = ifelse(!is.na(tank_closed_date) & reported_date_converted > tank_closed_date, 1, 0),
      leak_before_NFA_before_closure = ifelse(!is.na(tank_closed_date) & reported_date_converted < tank_closed_date & tank_closed_date > reported_date_converted, 1, 0),
      leak_before_NFA_after_closure = ifelse(!is.na(tank_closed_date) & reported_date_converted < tank_closed_date & tank_closed_date < reported_date_converted, 1, 0),
      no_leak = ifelse(is.na(reported_date_converted) & !is.na(tank_closed_date), 1, 0)
    ) %>% 
    as.data.table()

  state_UST_tanks[, id := paste0(facility_id, "-", tank_id)]
  state_UST_tanks[, tank_closed_date2 := mdy_hms(tank_closed_date)]
  num_closed_tanks = uniqueN(state_UST_tanks[!is.na(tank_closed_date2), id])

  count_leak_after_closure = state_UST_tanks_LUST[leak_after_closure == 1, uniqueN(paste0(facility_id, "-", tank_id))]
  count_leak_before_NFA_before_closure = state_UST_tanks_LUST[leak_before_NFA_before_closure == 1, uniqueN(paste0(facility_id, "-", tank_id))]
  count_leak_before_NFA_after_closure = state_UST_tanks_LUST[leak_before_NFA_after_closure == 1, uniqueN(paste0(facility_id, "-", tank_id))]
  count_no_leak = state_UST_tanks_LUST[no_leak == 1, uniqueN(paste0(facility_id, "-", tank_id))]

  state_counts = data.frame(
    category = c("leak_after_closure", "leak_before_NFA_before_closure", "leak_before_NFA_after_closure", "no_leak"),
    count = c(count_leak_after_closure, count_leak_before_NFA_before_closure, count_leak_before_NFA_after_closure, count_no_leak)
  )
  state_counts$total = sum(state_counts$count)
  state_counts$percent = (state_counts$count / state_counts$total) * 100

  cat("\nState:", state_name, "\n")
  print(state_counts)
  cat("\nNumber of closed tanks in", state_name, ":", num_closed_tanks, "\n")

state_UST_tanks_SD = state_UST_tanks_LUST %>% 
    select(facility_id, tank_id, tank_installed_date, tank_closed_date, tank_status, # <--- Added tank_status
           leak_after_closure, leak_before_NFA_before_closure, leak_before_NFA_after_closure, no_leak,
           is_gasoline, is_diesel, is_oil_kerosene, is_jet_fuel, is_other,
           single_walled, double_walled, unknown_walled, missing_walled, capacity, county_name) %>% 
    group_by(facility_id, tank_id, tank_installed_date, tank_closed_date, tank_status, county_name) %>% # <--- Added tank_status
    
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
    ) %>% 
    as.data.table()

  state_UST_tanks_SD = state_UST_tanks_SD[, .(
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
  ), keyby = .(facility_id, tank_id, tank_installed_date, tank_closed_date,county_name)]

  state_UST_tanks_SD = state_UST_tanks_SD[!is.na(facility_id)]

  # Define states with census geography data
  census_geography_states <- c("Louisiana", "Maine", "Massachusetts", "North Carolina", 
                              "Tennessee", "Minnesota", "Idaho", "Virginia")
  
  # Check if census geography data exists for this state
  if (state_name %in% census_geography_states) {
    # Use census geography data for specified states
    state_file_name <- gsub(" ", "_", state_name)
    census_file_path <- census_files[grepl(paste0("^", state_file_name, "_census_geography\\.csv$"), basename(census_files), ignore.case = TRUE)]
    
    if (length(census_file_path) > 0) {
      tryCatch({
        cat(paste0("\nLoading census geography data for ", state_name, "...\n"))
        census_data <- fread(census_file_path)
        
        # Convert facility_id to character for joining
        census_data[, facility_id := as.character(facility_id)]
        state_UST_tanks_SD[, facility_id := as.character(facility_id)]
        
        # Join with census data
        state_UST_tanks_SD <- merge(
          state_UST_tanks_SD,
          census_data[, .(facility_id, county_name, county_geoid)],
          by = "facility_id",
          all.x = TRUE,
          suffixes = c("", "_census")
        )
        
        # Use census county data where available
        state_UST_tanks_SD[!is.na(county_name_census), county_name := county_name_census]
        state_UST_tanks_SD[, county_name_census := NULL]
        
        # Verify join success
        cat(paste0("\nAdded census geography data for ", state_name, "\n"))
        cat("Unique county count:", uniqueN(state_UST_tanks_SD$county_name), "\n")
        cat("Facilities with county data:", sum(!is.na(state_UST_tanks_SD$county_name)), "\n")
        cat("Facilities with county_geoid data:", sum(!is.na(state_UST_tanks_SD$county_geoid)), "\n")
        
      }, error = function(e) {
        cat(paste0("\nError loading census data for ", state_name, ": ", e$message, "\n"))
      })
    } else {
      cat(paste0("\nNo census geography file found for ", state_name, "\n"))
    }
  } else {
    cat("\nSkipping census geography data for", state_name, "(not in census geography states list)\n")
  }

  # Standardize county names
  state_UST_tanks_SD[, standardized_county := standardize_county_name(county_name)]

# collapse data at facility_id standardize county name and county name level

if (state_name == "Connecticut") {
  cat("DEBUG: Special case for Connecticut - assigning New London county...\n")
  # For Connecticut, assign all records to New London county with FIPS 09110
  state_UST_tanks_SD[, `:=`(
    county_name = "New London",
    county_fips = "09110"
  )]
} else {
  cat("DEBUG: Loading county FIPS data from tigris Census package...\n")
  # Use tigris package to load county FIPS codes
  if (!require(tigris)) install.packages("tigris")
  library(tigris)

  # Load counties from the Census Bureau
  counties <- counties(state = state_name, cb = TRUE)
  counties <- as.data.table(counties)
  
  # Extract just the columns we need
  fips_data <- counties[, .(
    county_name = NAME,
    county_fips_code = GEOID,
    state_fips = substr(GEOID, 1, 2),
    state_name = STATE_NAME
  )]
  
  state_crosswalk <- data.table(state_name = state.name, state_abbr = state.abb)
  fips_data <- merge(fips_data, state_crosswalk, by = "state_name", all.x = TRUE)

  # Standardize county names
  fips_data[, standardized_county := standardize_county_name(county_name)]
  # Join FIPS codes based on standardized county name and state
  state_UST_tanks_SD <- merge.data.table(
    state_UST_tanks_SD,
    fips_data[, .(standardized_county, county_fips = county_fips_code)],
    by = "standardized_county",
    all.x = TRUE,
    allow.cartesian = TRUE
  )
}

# Report on the match rate

# Clean up temp column
if (state_name != "Connecticut") {
  state_UST_tanks_SD[, standardized_county := NULL]
}


  # Define required columns order (similar to other state files)
  required_columns = c(
    "facility_id", "tank_id", "tank_installed_date", "tank_closed_date",
    "leak_after_closure", "leak_before_NFA_before_closure", "leak_before_NFA_after_closure", "no_leak",
    "capacity", "single_walled", "double_walled", "missing_walled", "unknown_walled",
    "is_gasoline", "is_diesel", "is_oil_kerosene", "is_jet_fuel", "is_other"
  )
  
  # Add county and census columns to the required column list if they exist
  additional_columns = setdiff(names(state_UST_tanks_SD), required_columns)
  
  # Reorder columns if county exists, put it first among additional columns
  if ("county" %in% additional_columns) {
    setcolorder(state_UST_tanks_SD, c(required_columns, "county", setdiff(additional_columns, "county")))
  } else {
    setcolorder(state_UST_tanks_SD, c(required_columns, additional_columns))
  }

  fwrite(state_UST_tanks_SD, here("Data","Raw_do_not_write","state_databases", state_name, paste0(state_name,"_UST_tanks.csv")))

  state_leak_data_SD = state_leak_data %>% 
    mutate(
      LUST_id = lust_id,
      nfa_date = NA
    ) %>% 
    rename(report_date = reported_date_converted) %>% 
    select(facility_id, LUST_id, report_date, nfa_date) %>% 
    as.data.table()

  fwrite(state_leak_data_SD, here("Data","Raw_do_not_write","state_databases", state_name, paste0(state_name,"_UST_LUST.csv")))

  return(data.table(state = state_name, total_closed_tanks = num_closed_tanks))
} # end of function

#2. Processing EPA Data based on Criteria ----
#
# The EPA data is missing crucial info on the USTs. The most pressing I have identified are:
# 1. The Leak report date is missing from the EPAs - dropped states 4 (New Mexico[Fixed],North Dakota,Whyoming,Indiana, New Hampshire)
# 2. The Leak to tank ratio is unrealisticly small - dropped states 3 (OK [fixed],Nebraksa,California,Minnestoat)
# -- the total number of lost states due to both of these issues is 6 states ( Maine,Illinoise,Kentucky,Massachusetts,Virgina, Vermont)
# 19 States remain in  the EPA data we will process these 19 states for now and come back to the other states as needed.
### For each new state added I will create a seprate code file to process the data for that state. Since this data is from record requests.

# 2.1 What States that EPA Data has all the required columns ----


unique(state_UST_full$tank_status)

# 3. Analyze Missing Data Across States ----
analyze_missing_data <- function(start_year = 1986, end_year = 2022) {
  # Create data table to store results
  missing_data <- data.table(
    state = character(),
    total_tanks = integer(),
    missing_install_date = integer(),
    missing_install_percent = numeric(),
    closed_tanks = integer(),
    missing_closed_date = integer(),
    missing_closed_percent = numeric(),
    total_leaks = integer(),
    missing_leak_date = integer(),
    missing_leak_percent = numeric()
  )
  
  # Analyze each state
  for (state_name in unique(state_UST_full$state)) {
    cat("\nAnalyzing missing data for", state_name, "\n")
    
    # Get state UST data
    state_ust <- state_UST_full[state == state_name]
    
    # Parse dates
    state_ust[, installation_date_parsed := as.Date(mdy_hms(installation_date, quiet = TRUE))]
    state_ust[, removal_date_parsed := as.Date(mdy_hms(removal_date, quiet = TRUE))]
    
    # Filter data for the specified time period
    state_ust <- state_ust[
      (year(installation_date_parsed) >= start_year & year(installation_date_parsed) <= end_year) |
      (year(removal_date_parsed) >= start_year & year(removal_date_parsed) <= end_year) |
      (is.na(installation_date_parsed) & is.na(removal_date_parsed)) # Keep tanks with missing dates
    ]
    
    # Get state leak data
    state_leak <- leak_data_epa_01[state == state_name]
    
    # Parse leak dates
    state_leak[, reported_date_parsed := as.Date(mdy_hm(reported_date, quiet = TRUE))]
    
    # Filter leak data for the specified time period
    state_leak <- state_leak[
      (year(reported_date_parsed) >= start_year & year(reported_date_parsed) <= end_year) |
      is.na(reported_date_parsed) # Keep leaks with missing dates
    ]
    
    # Count total tanks
    total_tanks <- uniqueN(state_ust, by = c("facility_id", "tank_id"))
    
    # Count missing installation dates
    missing_install <- sum(is.na(state_ust$installation_date) | 
                           state_ust$installation_date == "", na.rm = TRUE)
    missing_install_pct <- ifelse(total_tanks > 0, missing_install / total_tanks * 100, 0)
    
    # Count closed tanks with missing closed dates - use proper column reference
    closed_tanks <- nrow(state_ust[state_ust$tank_status %in% c("Closed") | 
                                  grepl("^Closed", state_ust$tank_status, ignore.case = TRUE)])
    missing_closed <- sum((is.na(state_ust$removal_date) | state_ust$removal_date == "") & 
                          (state_ust$tank_status %in% c("Closed") | 
                           grepl("^Closed", state_ust$tank_status, ignore.case = TRUE)), 
                          na.rm = TRUE)
    missing_closed_pct <- ifelse(closed_tanks > 0, missing_closed / closed_tanks * 100, 0)
    
    # Count leaks with missing dates
    total_leaks <- nrow(state_leak)
    missing_leak_date <- sum(is.na(state_leak$reported_date) | state_leak$reported_date == "", na.rm = TRUE)
    missing_leak_pct <- ifelse(total_leaks > 0, missing_leak_date / total_leaks * 100, 0)
    
    # Add to results
    missing_data <- rbind(missing_data, data.table(
      state = state_name,
      total_tanks = total_tanks,
      missing_install_date = missing_install,
      missing_install_percent = missing_install_pct,
      closed_tanks = closed_tanks,
      missing_closed_date = missing_closed,
      missing_closed_percent = missing_closed_pct,
      total_leaks = total_leaks,
      missing_leak_date = missing_leak_date,
      missing_leak_percent = missing_leak_pct
    ))
  }
  
  return(missing_data)
}

# Function to analyze missingness through time - use proper column reference
analyze_time_missingness <- function(start_year = 1986, end_year = 2022) {
  # Container for time-based missingness data
  time_missing <- data.table()
  
  # Analyze each state
  for (state_name in unique(state_UST_full$state)) {
    cat("\nAnalyzing time-based missingness for", state_name, "\n")
    
    # Get state UST data
    state_ust <- state_UST_full[state == state_name]
    
    # Parse dates
    state_ust[, installation_date_parsed := as.Date(mdy_hms(installation_date, quiet = TRUE))]
    state_ust[, removal_date_parsed := as.Date(mdy_hms(removal_date, quiet = TRUE))]
    
    # Get state leak data
    state_leak <- leak_data_epa_01[state == state_name]
    state_leak[, reported_date_parsed := as.Date(mdy_hm(reported_date, quiet = TRUE))]
    
    # Analyze installation date missingness by year
    install_by_year <- data.table()
    for (yr in start_year:end_year) {
      # Filter to tanks that were active in this year
      year_tanks <- state_ust[
        (year(installation_date_parsed) <= yr & (is.na(removal_date_parsed) | year(removal_date_parsed) >= yr))
      ]
      
      total_tanks <- nrow(year_tanks)
      missing_install <- sum(is.na(year_tanks$installation_date) | year_tanks$installation_date == "", na.rm = TRUE)
      install_pct <- ifelse(total_tanks > 0, missing_install / total_tanks * 100, 0)
      
      install_by_year <- rbind(install_by_year, data.table(
        state = state_name,
        year = yr,
        metric = "installation_date",
        total_count = total_tanks,
        missing_count = missing_install,
        missing_percent = install_pct
      ))
    }
    
    # Analyze closure date missingness by year - use proper column reference
    closure_by_year <- data.table()
    for (yr in start_year:end_year) {
      # Filter to tanks closed in this year
      year_closed <- state_ust[
        year(removal_date_parsed) == yr | 
        ((state_ust$tank_status %in% c("Closed") | 
          grepl("^Closed", state_ust$tank_status, ignore.case = TRUE)) & 
         is.na(removal_date_parsed))
      ]
      
      total_closed <- nrow(year_closed)
      missing_closed <- sum(is.na(year_closed$removal_date) | year_closed$removal_date == "", na.rm = TRUE)
      closed_pct <- ifelse(total_closed > 0, missing_closed / total_closed * 100, 0)
      
      closure_by_year <- rbind(closure_by_year, data.table(
        state = state_name,
        year = yr,
        metric = "closure_date",
        total_count = total_closed,
        missing_count = missing_closed,
        missing_percent = closed_pct
      ))
    }
    
    # Analyze leak report date missingness by year
    leak_by_year <- data.table()
    for (yr in start_year:end_year) {
      # Filter to leaks reported in this year
      year_leaks <- state_leak[year(reported_date_parsed) == yr | is.na(reported_date_parsed)]
      
      total_leaks <- nrow(year_leaks)
      missing_leaks <- sum(is.na(year_leaks$reported_date) | year_leaks$reported_date == "", na.rm = TRUE)
      leak_pct <- ifelse(total_leaks > 0, missing_leaks / total_leaks * 100, 0)
      
      leak_by_year <- rbind(leak_by_year, data.table(
        state = state_name,
        year = yr,
        metric = "leak_date",
        total_count = total_leaks,
        missing_count = missing_leaks,
        missing_percent = leak_pct
      ))
    }
    
    # Combine all metrics
    time_missing <- rbind(time_missing, install_by_year, closure_by_year, leak_by_year)
  }
  
  return(time_missing)
}

# Function to visualize missing data
visualize_missing_data <- function(missing_data) {
  # Sort states by missing percentage for each category
  install_order <- missing_data[order(-missing_install_percent)]$state
  closed_order <- missing_data[order(-missing_closed_percent)]$state
  leak_order <- missing_data[order(-missing_leak_percent)]$state
  
  # Create installation date plot
  p1 <- ggplot(missing_data, aes(x = factor(state, levels = install_order), y = missing_install_percent)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    geom_text(aes(label = sprintf("%.1f%%", missing_install_percent)), 
              hjust = -0.1, size = 3, angle = 90) +
    labs(title = "Missing Installation Dates by State (1980-2022)",
         x = "State", y = "Percent Missing (%)") +
    theme_bw() +  # Changed to theme_bw for white background
    theme(
      panel.background = element_rect(fill = "white", color = "black"),
      plot.background = element_rect(fill = "white"),
      axis.text.x = element_text(angle = 90, hjust = 1)
    )
  
  # Create closed date plot (for tanks marked as closed)
  p2 <- ggplot(missing_data, aes(x = factor(state, levels = closed_order), y = missing_closed_percent)) +
    geom_bar(stat = "identity", fill = "darkred") +
    geom_text(aes(label = sprintf("%.1f%%", missing_closed_percent)), 
              hjust = -0.1, size = 3, angle = 90) +
    labs(title = "Missing Closure Dates for Closed Tanks by State (1980-2022)",
         x = "State", y = "Percent Missing (%)") +
    theme_bw() +  # Changed to theme_bw for white background
    theme(
      panel.background = element_rect(fill = "white", color = "black"),
      plot.background = element_rect(fill = "white"),
      axis.text.x = element_text(angle = 90, hjust = 1)
    )
  
  # Create leak date plot
  p3 <- ggplot(missing_data, aes(x = factor(state, levels = leak_order), y = missing_leak_percent)) +
    geom_bar(stat = "identity", fill = "darkgreen") +
    geom_text(aes(label = sprintf("%.1f%%", missing_leak_percent)), 
              hjust = -0.1, size = 3, angle = 90) +
    labs(title = "Missing Leak Report Dates by State (1980-2022)",
         x = "State", y = "Percent Missing (%)") +
    theme_bw() +  # Changed to theme_bw for white background
    theme(
      panel.background = element_rect(fill = "white", color = "black"),
      plot.background = element_rect(fill = "white"),
      axis.text.x = element_text(angle = 90, hjust = 1)
    )
  
  # Return list of plots
  return(list(install_plot = p1, closed_plot = p2, leak_plot = p3))
}

# Function to create cross-tabs of missing data
create_missing_data_crosstabs <- function(missing_data) {
  # Round percentages for better readability
  missing_data[, `:=`(
    missing_install_percent = round(missing_install_percent, 1),
    missing_closed_percent = round(missing_closed_percent, 1),
    missing_leak_percent = round(missing_leak_percent, 1)
  )]
  
  # Create install date cross-tab
  install_crosstab <- missing_data[, .(
    state, 
    total_tanks, 
    missing_install = missing_install_date,
    missing_percent = missing_install_percent
  )]
  setorder(install_crosstab, -missing_percent)
  
  # Create closed date cross-tab
  closed_crosstab <- missing_data[, .(
    state, 
    closed_tanks, 
    missing_closed = missing_closed_date,
    missing_percent = missing_closed_percent
  )]
  setorder(closed_crosstab, -missing_percent)
  
  # Create leak date cross-tab
  leak_crosstab <- missing_data[, .(
    state, 
    total_leaks, 
    missing_leak_dates = missing_leak_date,
    missing_percent = missing_leak_percent
  )]
  setorder(leak_crosstab, -missing_percent)
  
  return(list(
    install = install_crosstab,
    closed = closed_crosstab,
    leak = leak_crosstab
  ))
}

# Function to visualize missingness through time
visualize_time_missingness <- function(time_missing_data) {
  # List to store plots
  time_plots <- list()
  
  # Get unique states
  states <- unique(time_missing_data$state)
  
  # For each state, create a faceted plot showing all three metrics
  for (i in 1:ceiling(length(states)/4)) {
    # Select a batch of states (up to 4 per plot)
    start_idx <- (i-1)*4 + 1
    end_idx <- min(i*4, length(states))
    batch_states <- states[start_idx:end_idx]
    
    # Filter data for these states
    batch_data <- time_missing_data[state %in% batch_states]
    
    # Create plot
    p <- ggplot(batch_data, aes(x = year, y = missing_percent, color = metric, group = metric)) +
      geom_line() +
      geom_point(size = 0.5) +
      facet_wrap(~ state, scales = "free_y") +
      labs(title = paste("Missingness Over Time (Batch", i, ")"),
           x = "Year", y = "Percent Missing (%)", 
           color = "Metric") +
      scale_color_manual(values = c("installation_date" = "steelblue", 
                                   "closure_date" = "darkred", 
                                   "leak_date" = "darkgreen"),
                        labels = c("Installation Date", 
                                   "Closure Date", 
                                   "Leak Report Date")) +
      theme_bw() +  # Changed to theme_bw for white background
      theme(
        panel.background = element_rect(fill = "white", color = "black"),
        plot.background = element_rect(fill = "white"),
        strip.background = element_rect(fill = "white"),  # White background for facet labels
        legend.background = element_rect(fill = "white"),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
    
    # Add to list
    time_plots[[paste0("batch_", i)]] <- p
  }
  
  # Create a heatmap showing missingness by state and year for each metric separately
  for (metric_name in unique(time_missing_data$metric)) {
    metric_data <- time_missing_data[metric == metric_name]
    
    # Only include states and years with data
    metric_data <- metric_data[total_count > 0]
    
    if (nrow(metric_data) > 0) {
      # Create heatmap
      p_heatmap <- ggplot(metric_data, aes(x = year, y = state, fill = missing_percent)) +
        geom_tile() +
        scale_fill_gradient2(low = "white", mid = "yellow", high = "red",
                            midpoint = 50, limits = c(0, 100)) +
        labs(title = paste("Missingness Heatmap:", 
                          ifelse(metric_name == "installation_date", "Installation Dates",
                                ifelse(metric_name == "closure_date", "Closure Dates", "Leak Report Dates"))),
             x = "Year", y = "State", fill = "% Missing") +
        theme_bw() +  # Changed to theme_bw for white background
        theme(
          panel.background = element_rect(fill = "white", color = "black"),
          plot.background = element_rect(fill = "white"),
          legend.background = element_rect(fill = "white"),
          axis.text.x = element_text(angle = 90, hjust = 1, size = 8),
          axis.text.y = element_text(size = 8)
        )
      
      time_plots[[paste0("heatmap_", metric_name)]] <- p_heatmap
    }
  }
  
  return(time_plots)
}

# Function to create sampling criteria dataframe
create_sampling_criteria <- function(missing_data, time_missing_data) {
  # Create wide format dataframe with missingness statistics
  sampling_criteria <- missing_data[, .(
    state,
    total_tanks,
    closed_tanks,
    total_leaks,
    missing_install_percent,
    missing_closed_percent,
    missing_leak_percent
  )]
  
  # Calculate average missingness over time for each metric and state
  time_avg <- time_missing_data[, .(
    avg_missing_percent = mean(missing_percent, na.rm = TRUE)
  ), by = .(state, metric)]
  
  # Reshape to wide format
  time_avg_wide <- dcast(time_avg, state ~ metric, value.var = "avg_missing_percent")
  
  # Rename columns
  setnames(time_avg_wide, 
          c("closure_date", "installation_date", "leak_date"), 
          c("time_avg_missing_closed", "time_avg_missing_install", "time_avg_missing_leak"))
  
  # Calculate data quality metrics
  sampling_criteria <- merge(sampling_criteria, time_avg_wide, by = "state", all.x = TRUE)
  
  # Create overall quality score (lower is better)
  # Weight factors based on importance
  sampling_criteria[, quality_score := (
    0.4 * missing_install_percent + 
    0.3 * missing_closed_percent + 
    0.3 * missing_leak_percent
  )]
  
  # Add recommendation based on thresholds
  sampling_criteria[, recommendation := ifelse(
    missing_install_percent < 30 & missing_closed_percent < 40 & missing_leak_percent < 20,
    "Include",
    ifelse(
      missing_install_percent < 50 & missing_closed_percent < 60 & missing_leak_percent < 40,
      "Consider",
      "Exclude"
    )
  )]
  
  # Add data count metrics for evaluation
  sampling_criteria[, tanks_per_leak := total_tanks / pmax(total_leaks, 1)]
  
  # Sort by quality score
  setorder(sampling_criteria, quality_score)
  
  return(sampling_criteria)
}

# Function to get recommended states list based on criteria
get_recommended_states <- function(sampling_criteria, include_consider = FALSE) {
  if (include_consider) {
    recommended <- sampling_criteria[recommendation %in% c("Include", "Consider"), state]
  } else {
    recommended <- sampling_criteria[recommendation == "Include", state]
  }
  return(recommended)
}

# Run the enhanced analysis
if (first_run) {
  cat("\n\nAnalyzing missing data across states (1980-2022)...\n")
  missing_data <- analyze_missing_data(1980, 2022)
  
  # Create visualizations
  missing_plots <- visualize_missing_data(missing_data)
  
  # Create crosstabs
  missing_crosstabs <- create_missing_data_crosstabs(missing_data)
  
  # Print summary tables
  cat("\n\n=== Missing Installation Dates (1980-2022) ===\n")
  print(missing_crosstabs$install)
  
  cat("\n\n=== Missing Closure Dates for Closed Tanks (1980-2022) ===\n")
  print(missing_crosstabs$closed)
  
  cat("\n\n=== Missing Leak Report Dates (1980-2022) ===\n")
  print(missing_crosstabs$leak)
  
  # Analyze missing data through time
  cat("\n\nAnalyzing missingness through time...\n")
  time_missing_data <- analyze_time_missingness(1980, 2022)
  
  # Visualize time-based missingness
  cat("\n\nCreating time-based missingness visualizations...\n")
  time_plots <- visualize_time_missingness(time_missing_data)
  
  # Show time plots (first few plots only)
  if (length(time_plots) > 0) {
    for (i in 1:min(3, length(time_plots))) {
      print(time_plots[[i]])
    }
  }
  
  # Create sampling criteria dataframe
  cat("\n\nGenerating sampling criteria...\n")
  sampling_criteria <- create_sampling_criteria(missing_data, time_missing_data)
  
  # Print sampling criteria
  cat("\n\n=== Sampling Criteria for State Selection ===\n")
  print(sampling_criteria)
  
  # Get recommended states list
  recommended_states <- get_recommended_states(sampling_criteria, include_consider = TRUE)
  cat("\n\n=== Recommended States for Processing ===\n")
  print(recommended_states)
  
  # Compare with current states_list
  cat("\n\n=== Comparison with Current States List ===\n")
  cat("States in current list but not recommended:", setdiff(unique(state_UST_full$state), recommended_states), "\n")
  cat("Recommended states not in current list:", setdiff(recommended_states, unique(state_UST_full$state)), "\n")
  
  # Export sampling criteria to CSV
  fwrite(sampling_criteria, here("Data", "Processed", "state_sampling_criteria.csv"))
  cat("\nSampling criteria saved to:", here("Data", "Processed", "state_sampling_criteria.csv"), "\n")
}
# Replace the report template creation with updated version including time-based analysis
if (first_run) {
  # Create directory for report images if it doesn't exist
  report_img_dir <- here("Reports", "images")
  if (!dir.exists(report_img_dir)) {
    dir.create(report_img_dir, recursive = TRUE)
  }
  
  # Save plots to the images directory
  if (exists("missing_plots") && length(missing_plots) > 0) {
    ggsave(file.path(report_img_dir, "missing_install_dates.png"), missing_plots$install, width = 10, height = 8)
    ggsave(file.path(report_img_dir, "missing_closed_dates.png"), missing_plots$closed, width = 10, height = 8)
    ggsave(file.path(report_img_dir, "missing_leak_dates.png"), missing_plots$leak, width = 10, height = 8)
  }
  
  # Save time plots
  if (exists("time_plots") && length(time_plots) > 0) {
    time_plot_paths <- list()
    for (i in 1:length(time_plots)) {
      plot_name <- paste0("time_plot_", i, ".png")
      ggsave(file.path(report_img_dir, plot_name), time_plots[[i]], width = 10, height = 8)
      time_plot_paths[[i]] <- file.path(report_img_dir, plot_name)
    }
  }
  # Save plots with white backgrounds and higher DPI
  if (exists("missing_plots") && length(missing_plots) > 0) {
    ggsave(file.path(report_img_dir, "missing_install_dates.png"), missing_plots$install, 
           width = 10, height = 8, bg = "white", dpi = 300)
    ggsave(file.path(report_img_dir, "missing_closed_dates.png"), missing_plots$closed, 
           width = 10, height = 8, bg = "white", dpi = 300)
    ggsave(file.path(report_img_dir, "missing_leak_dates.png"), missing_plots$leak, 
           width = 10, height = 8, bg = "white", dpi = 300)
  }
  
  # Save time plots with white backgrounds and higher DPI
  if (exists("time_plots") && length(time_plots) > 0) {
    time_plot_paths <- list()
    for (i in 1:length(time_plots)) {
      plot_name <- paste0("time_plot_", i, ".png")
      ggsave(file.path(report_img_dir, plot_name), time_plots[[i]], 
             width = 10, height = 8, bg = "white", dpi = 300)
      time_plot_paths[[i]] <- file.path(report_img_dir, plot_name)
    }
  }
}

# Process selected states based on sampling criteria

if (first_run) {

#chekc to make sure the closed dates missing states is because of missing and not 
#raw data issues
# Extract states with 90% or more missing closed dates
high_missing_closed_states <- missing_data[missing_closed_percent >= 90, state]

#extract the states with > 80% missing installation dates
high_missing_install_states <- missing_data[missing_install_percent >= 80, state]
## how many of these two have the same states
common_states <- intersect(high_missing_closed_states, high_missing_install_states)
common_states
#now greater than .8  missing leak dates
high_missing_leak_states <- missing_data[missing_leak_percent >= 80, state]
#how many of these are common with the other two
common_states <- intersect(common_states, high_missing_leak_states)
common_leak_closed = intersect(high_missing_closed_states, high_missing_leak_states)
comon_leak_install = intersect(high_missing_install_states, high_missing_leak_states)

#Now the states that are not public fund states and not treatment states
not_public_fund_controls_ever <- c(
  "Alaska",
  "Delaware",
  "Hawaii",
  "Maryland", 
  "Oregon",
  "West Virginia",
  "Washington",
  ## remove terriroties
  "Puerto Rico",
"U.S. Virgin Islands",
"Guam",
"American Samoa",
"Commonwealth of the Northern Mariana Islands")

#now list the states that are treatment and I have not rebuilt their database from record request, so I need to use EPA
epa_treatment_state = c('New Jersey','Connecticut','Michigan')

#treatment states to exclude from the EPA data
## Reason1 I rebuild these seperately from record requests - need Record request form ARizona
## Reason2 The EPA data is missing key information - just arizona 
treatment_states_exclude = c('Wisconsin','Florida','Texas','Arizona')

# states to exclude because I rebuild from record requests not treatment
record_request_states = c('Arkansas','Okalahoma','New Mexico')


# so the states to process are all of the state not in  the following lists
## EPA data collection issues:
# high_missing_closed_states, high_missing_install_states, high_missing_leak_states, 
## Not a comparison state due to policy reasons
# not_public_fund_controls_ever, 
## I have rebuilt the database from record requests
# treatment_states_exclude, record_request_states

  # create the exclusion list using the above
  states_to_exclude = c(high_missing_closed_states, high_missing_install_states, high_missing_leak_states, 
                          not_public_fund_controls_ever, treatment_states_exclude, record_request_states)
  states_to_exclude = unique(states_to_exclude)
  states_to_exclude_dt = data.table(state = states_to_exclude)
  fwrite(states_to_exclude_dt, here("Data", "Processed", "states_to_exclude.csv"))
  
  # make it unique: 
  states_to_exclude = unique(states_to_exclude)
  
  # how many EPA data states does that leave me?
  states_to_process = setdiff(unique(state_UST_full$state), states_to_exclude)
  length(states_to_process)
  states_to_process = c(states_to_process, 'Kansas')
  
} else {
  # If not first_run, load the exclusion list from CSV
  states_to_exclude_dt = fread(here("Data", "Processed", "states_to_exclude.csv"),fill = TRUE)
  states_to_exclude = unique(states_to_exclude_dt$state)
  
  # derive states to process from the loaded exclusion list
  states_to_process = setdiff(unique(state_UST_full$state), states_to_exclude)
  states_to_process = c(states_to_process, 'Kansas')
}
# Process each state with the enhanced functions
total_closed_tanks = rbindlist(lapply(states_to_process, process_state_data))
process_state_data('Kansas')
process_state_data('Connecticut')
process_state_data('Louisiana')
# sort the data by total_closed_tanks
total_closed_tanks = total_closed_tanks[order(-total_closed_tanks)]

cat("\n\nSummary of all states processed:\n")
print(total_closed_tanks)
