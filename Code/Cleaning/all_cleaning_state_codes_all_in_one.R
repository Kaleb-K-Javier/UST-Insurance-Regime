## Arkansas UST Dataset Creation

# Define the county variable name in the raw data
COUNTYVARIABLE <- "CountyCAPS"

library(here)
# load packages ----
source(here('Code', '00_global_packages.R'))
library(tidyverse)
library(DBI)
library(odbc)
library(data.table)
library(here)


# Establish database connection
con <- dbConnect(
  odbc(),
  .connection_string = "Driver={Microsoft Access Driver (*.mdb, *.accdb)};Dbq=C:\\Users\\kaleb\\Box\\UST-Insurance\\Data\\Raw_do_not_write\\state_databases\\Arkansas\\TankStats_web.mdb;"
)

# Load required tables
rfacs_data <- as.data.table(dbReadTable(con, "TempRFACS_CountryNO"))
tankstats_ug <- as.data.table(dbReadTable(con, "TempTankStats_UG"))
lust_data <- as.data.table(dbReadTable(con, "TempRLUSTLOG1"))

# Clean and standardize facility IDs to remove whitespace
rfacs_data[, FACILITY_ID := trimws(FACILITY_ID)]
tankstats_ug[, RSTFacNbr := trimws(RSTFacNbr)]
lust_data[, RSTFacNbr := trimws(RSTFacNbr)]

# glimpse(rfacs_data)
# glimpse(tankstats_ug)
# glimpse(lust_data)

# Select and rename relevant columns for RFACS
rfacs_data <- rfacs_data[, .(Facility_ID = FACILITY_ID,
                              county_name = trimws(get(COUNTYVARIABLE)))]

# Select and rename relevant columns for TempTankStats_UG
tankstats_data <- tankstats_ug[, .(
  Facility_ID = RSTFacNbr,               # Facility ID
  Tank_ID = TankNbr,                    # Tank number
  Tank_Installation_Date = InstallDate, # Tank installation date
  Tank_Status_Change_Reason = TankStatusChangeRsnCode, # Reason code
  Tank_Status_Type = TankStatusTypeCode, # Status type
  Tank_Status_Date = TankStatusDate,     # Date associated with the status
  
  # Tank construction variables - include all relevant fields
  steel = ifelse(UT_MOC_SteelTF == TRUE, 1, 0),
  epoxy = ifelse(UT_MOC_EpoxyTF == TRUE, 1, 0),
  composite = ifelse(UT_MOC_CompositeTF == TRUE, 1, 0),
  fiberglass_plastic = ifelse(UT_MOC_FbrPlasticTF == TRUE, 1, 0),
  concrete = ifelse(UT_MOC_ConcreteTF == TRUE, 1, 0),
  int_lining = ifelse(UT_MOC_IntLiningTF == TRUE, 1, 0),
  excv_liner = ifelse(UT_MOC_ExcvLinerTF == TRUE, 1, 0),
  dbl_wall = ifelse(UT_MOC_DblWallTF == TRUE, 1, 0),
  poly_jacket = ifelse(UT_MOC_PolyJacketTF == TRUE, 1, 0),
  unknown_mat = ifelse(UT_MOC_UnknownTF == TRUE, 1, 0),
  
  capacity = Capacity,
  
  # Substance variables
  gasoline = ifelse(UT_SS_GasolineTF == TRUE, 1, 0),
  diesel = ifelse(UT_SS_DieselTF == TRUE, 1, 0),
  kerosene = ifelse(UT_SS_KeroseneTF == TRUE, 1, 0),
  used_oil = ifelse(UT_SS_UsedOilTF == TRUE, 1, 0),
  new_oil = ifelse(UT_SS_NewOilTF == TRUE, 1, 0),
  empty = ifelse(UT_SS_EmptyTF == TRUE, 1, 0),
  unknown = ifelse(UT_SS_UnknownTF == TRUE, 1, 0)
)]

# Convert logical nulls to 0 for binary variables
binary_cols <- c("steel", "epoxy", "composite", "fiberglass_plastic", "concrete", 
                "int_lining", "excv_liner", "dbl_wall", "poly_jacket", "unknown_mat",
                "gasoline", "diesel", "kerosene", "used_oil", "new_oil", "empty", "unknown")
for(col in binary_cols) {
  tankstats_data[is.na(get(col)), (col) := 0]
}

# Create standardized wall type variables according to the requirements
tankstats_data[, `:=`(
  # Double walled if either double wall or internal lining is present
  double_walled = ifelse(dbl_wall == 1 | int_lining == 1, 1, 0),
  
  # Unknown walled if unknown_mat is marked
  unknown_walled = unknown_mat,
  
  # Missing walled if none of the material types are recorded
  missing_walled = ifelse(
    steel + epoxy + composite + fiberglass_plastic + concrete + 
    int_lining + excv_liner + dbl_wall + poly_jacket + unknown_mat == 0, 
    1, 0
  ),
  
  # Single walled if not double walled, not unknown, and not missing
  single_walled = ifelse(
    dbl_wall == 0 & int_lining == 0 & unknown_mat == 0 & 
    (steel + epoxy + composite + fiberglass_plastic + concrete + 
     excv_liner + poly_jacket > 0),
    1, 0
  )
)]

# Map substance categories to match Texas format
tankstats_data[, `:=`(
  is_gasoline = gasoline,
  is_diesel = diesel,
  is_oil_kerosene = pmax(kerosene, used_oil, new_oil),  # 1 if any oil/kerosene type
  is_jet_fuel = 0,  # Assume no jet fuel, update if we have data about it
  is_other = ifelse(empty + unknown > 0, 1, 0) # Only use empty and unknown flags
)]

# If all substance types are 0, mark as other
tankstats_data[is_gasoline + is_diesel + is_oil_kerosene + is_jet_fuel == 0, is_other := 1]


# Select and rename relevant columns
ust_lust_table <- lust_data[, .(
  Facility_ID = RSTFacNbr,             # Facility ID
  LUST_ID = LUSTNoticeNbr,          # Unique identifier for the LUST
  LUSTDiscoveryMthdCode,               # Code for the LUST discovery method
  ReleaseStatus,                       # Release status (C: Confirmed, D: Denied, S: Suspected)
  ReleaseConfirmedDate,                # Date the release was confirmed
  NFAIssuedDate                        # Date of "No Further Action" issuance
)]

# Add a new column "leak_found_after_closure" to ust_lust_table
ust_lust_table[, leak_found_after_closure := grepl("CA", LUSTDiscoveryMthdCode, ignore.case = TRUE)]

# Add a new column "found_during_inspection" to ust_lust_table
ust_lust_table[, found_during_inspection := grepl("(ALT|INS)", LUSTDiscoveryMthdCode, ignore.case = TRUE)]


# Identify open tanks: Match "IU" (In Use) in TankStatusTypeCode
open_tanks <- tankstats_data[grepl("IU", Tank_Status_Type, ignore.case = TRUE), .(
  Facility_ID,
  Tank_ID,
  Tank_Installation_Date,
  Tank_Removal_Date = NA,                # Open tanks have no removal date
  # Add new variables
  capacity,
  single_walled,
  double_walled,
  missing_walled,
  unknown_walled,
  is_gasoline,
  is_diesel,
  is_oil_kerosene,
  is_jet_fuel,
  is_other
)]

# Identify closed tanks: Match "PO" (Permanently Out of Use) in TankStatusTypeCode
# AND "R" (Removed) in TankStatusChangeRsnCode
closed_tanks <- tankstats_data[
  grepl("PO", Tank_Status_Type, ignore.case = TRUE) & 
  grepl("R", Tank_Status_Change_Reason, ignore.case = TRUE), .(
    Facility_ID,
    Tank_ID,
    Tank_Installation_Date,
    Tank_Removal_Date = Tank_Status_Date,  # Use the status date as removal date
    # Add new variables
    capacity,
    single_walled,
    double_walled,
    missing_walled,
    unknown_walled,
    is_gasoline,
    is_diesel,
    is_oil_kerosene,
    is_jet_fuel,
    is_other
  )
]

# Combine open and closed tanks
facility_tank_data <- bind_rows(open_tanks, closed_tanks)

# Merge with RFACS to ensure all facility IDs are included if necessary
facility_tank_data <- merge(
  rfacs_data, 
  facility_tank_data, 
  by = "Facility_ID", 
  all.x = TRUE
)

# Save processed data for future use

# Print a summary of the resulting dataset
print(facility_tank_data)

# Disconnect from the database
dbDisconnect(con)

# how many leaks found by closure 
sum(ust_lust_table$leak_found_after_closure)
# how many found during inspection
sum(ust_lust_table$found_during_inspection)


## now identify the three sets of closed tanks
### 1. Closed tanks where a leak was found after closure
### 2. Closed tanks where a leak was found berfore closure and the NFA was issued before the closure date
### 3. Closed tanks where a leak was found berfore closure and the NFA was issued after the closure date

## do these using the LUST date and closure dates
## match a facilites LUST data to the tank data and then do the date checsk by faciliyt.
## LUST is at the facility level, so we are looking to find if any tank clsoures falls within the LUST dates

# ## more than one lust per facility check --- yes there is, idk how to handel this correctly atmd
# faciliy_lust_count = ust_lust_table[, .(count = .N), by = Facility_ID]
# View(faciliy_lust_count)

facility_tank_data <- unique(facility_tank_data)
ust_lust_table <- unique(ust_lust_table)
glimpse(facility_tank_data)
glimpse(ust_lust_table)

#lets make sure the data sets remove all the duplicates
## 

##
# Collapse LUST table to one row per Facility_ID and ReleaseConfirmedDate,
# counting the number of LUST events on that date
collapsed_ust_lust <- ust_lust_table[, .(LUST_count = .N), by = .(Facility_ID, ReleaseConfirmedDate,NFAIssuedDate)]

View(collapsed_ust_lust)
# Merge the facility_tank_data with the collapsed LUST dataset
facility_tank_data_LUST <- merge.data.table(
  facility_tank_data,
  collapsed_ust_lust,
  by = "Facility_ID",
  all.x = TRUE,
   allow.cartesian = TRUE
 )
#### Need to change how i do this across the whole
#### Due to the inconsistency of the reported lust date data
#### I'll do LUSTs to closures if any LUST is found within 
### 30days of reporting the tank closure date
### If I have NFA dates, if NFA is before closre then we have leak before closure.



nrow(ust_lust_table)
nrow(collapsed_ust_lust)
# # Merge the facility_tank_data with ust_lust_table
# facility_tank_data_LUST <- merge.data.table(
#   facility_tank_data,
#   ust_lust_table,
#   by = "Facility_ID",
#   all.x = TRUE,
# allow.cartesian = T)
# turn the date cols into date objects, using lubridate lust data is ymd  tank data is ymd as well
## use lubridate package

facility_tank_data_LUST[, ReleaseConfirmedDate := ymd(ReleaseConfirmedDate)]
facility_tank_data_LUST[, NFAIssuedDate := ymd(NFAIssuedDate)]
facility_tank_data_LUST[, Tank_Removal_Date := ymd(Tank_Removal_Date)]
facility_tank_data_LUST[, Tank_Installation_Date := ymd(Tank_Installation_Date)]





## some error is here-- I think its cuz we may have more than LUST event per facility

# Replace old leak logic with new columns
facility_tank_data_LUST[
  ,
  `:=`(
    leak_after_closure = ifelse(
      !is.na(Tank_Removal_Date) & ReleaseConfirmedDate > Tank_Removal_Date,
      1,
      0
    ),
    leak_before_NFA_before_closure = ifelse(
      !is.na(Tank_Removal_Date) & ReleaseConfirmedDate < Tank_Removal_Date & Tank_Removal_Date > NFAIssuedDate,
      1,
      0
    ),
    leak_before_NFA_after_closure = ifelse(
      !is.na(Tank_Removal_Date) & ReleaseConfirmedDate < Tank_Removal_Date & Tank_Removal_Date < NFAIssuedDate,
      1,
      0
    ),
    no_leak = ifelse(
      is.na(ReleaseConfirmedDate) & !is.na(Tank_Removal_Date),
      1,
      0
    )
  )
]

# Create an ID for unique facility-tank pairs
facility_tank_data_LUST[, id := paste0(Facility_ID, "-", Tank_ID)]

# Count for each category
count_leak_after_closure <- facility_tank_data_LUST[
  leak_after_closure == 1,
  uniqueN(id)
]
count_leak_before_NFA_before_closure <- facility_tank_data_LUST[
  leak_before_NFA_before_closure == 1,
  uniqueN(id)
]
count_leak_before_NFA_after_closure <- facility_tank_data_LUST[
  leak_before_NFA_after_closure == 1,
  uniqueN(id)
]
count_no_leak <- facility_tank_data_LUST[
  no_leak == 1,
  uniqueN(id)
]

# Combine results
counts <- data.table(
  category = c(
    "leak_after_closure",
    "leak_before_NFA_before_closure",
    "leak_before_NFA_after_closure",
    "no_leak"
  ),
  count = c(
    count_leak_after_closure,
    count_leak_before_NFA_before_closure,
    count_leak_before_NFA_after_closure,
    count_no_leak
  )
)

counts[, total := sum(count)]
counts[, percent := count / total]
print(counts)

# Total closed tanks
number_of_closed_tanks <- facility_tank_data_LUST[
  !is.na(Tank_Removal_Date), uniqueN(id)
]
print(number_of_closed_tanks)

### 1/20/2025 - make sure this closure tanked identifcation makes sense

# count the number of facilities with closed tanks and no LUST data as part of whole dat

#how many facilities have closed tanks and no LUST data

# Collapse facility-tank-lust data and sum up leaks
AR_UST_tanks_SD <- facility_tank_data_LUST[
  ,
  .(
    Facility_ID,
    Tank_ID,
    Tank_Installation_Date,
    Tank_Removal_Date,
    capacity,
    single_walled,
    double_walled,
    missing_walled,
    unknown_walled,
    is_gasoline,
    is_diesel,
    is_oil_kerosene,
    is_jet_fuel,
    is_other,
    leak_after_closure,
    leak_before_NFA_before_closure,
    leak_before_NFA_after_closure,
    no_leak,
    county_name
  )
][
  ,
  .(
    leak_after_closure = ifelse(sum(leak_after_closure, na.rm = TRUE) > 0, 1, 0),
    leak_before_NFA_before_closure = ifelse(sum(leak_before_NFA_before_closure, na.rm = TRUE) > 0, 1, 0),
    leak_before_NFA_after_closure = ifelse(sum(leak_before_NFA_after_closure, na.rm = TRUE) > 0, 1, 0),
    no_leak = ifelse(sum(no_leak, na.rm = TRUE) > 0, 1, 0),
    capacity = first(capacity),
    single_walled = max(single_walled, na.rm = TRUE),
    double_walled = max(double_walled, na.rm = TRUE),
    missing_walled = max(missing_walled, na.rm = TRUE),
    unknown_walled = max(unknown_walled, na.rm = TRUE),
    is_gasoline = max(is_gasoline, na.rm = TRUE),
    is_diesel = max(is_diesel, na.rm = TRUE),
    is_oil_kerosene = max(is_oil_kerosene, na.rm = TRUE),
    is_jet_fuel = max(is_jet_fuel, na.rm = TRUE),
    is_other = max(is_other, na.rm = TRUE)
  ),
  by = .(Facility_ID, Tank_ID, Tank_Installation_Date, Tank_Removal_Date,county_name)
]

# Rename columns to a standard set
setnames(
  AR_UST_tanks_SD,
  c("Facility_ID","Tank_ID","Tank_Installation_Date","Tank_Removal_Date"),
  c("facility_id","tank_id","tank_installed_date","tank_closed_date")
)

# Standardize the LUST table
AR_LUST_SD <- ust_lust_table[
  ,
  .(
    facility_id = Facility_ID,
    LUST_id = LUST_ID,
    report_date = ReleaseConfirmedDate,
    nfa_date = NFAIssuedDate
  )
]

# Verify there's no trailing whitespace in facility IDs
cat("\nChecking for whitespace in facility IDs:\n")
cat("Facility IDs with trailing whitespace in AR_UST_tanks_SD:", 
    sum(AR_UST_tanks_SD$facility_id != trimws(AR_UST_tanks_SD$facility_id)), "\n")
cat("Facility IDs with trailing whitespace in AR_LUST_SD:", 
    sum(AR_LUST_SD$facility_id != trimws(AR_LUST_SD$facility_id)), "\n")

############################################################################################################
## Add Census Geography Data to UST data - COMMENTED OUT UNTIL GIS CODE IS FIXED
############################################################################################################

# # Load the Arkansas census geography data
# arkansas_census_geography = fread("C:/Users/kaleb/Box/UST-Insurance/Data/Processed/Census_Geography/Arkansas_census_geography.csv")
# glimpse(arkansas_census_geography)
# glimpse(tankstats_ug)
# glimpse(AR_UST_tanks_SD)
# # Convert facility_id in census data to character to match other data frames
# arkansas_census_geography[, facility_id := as.character(facility_id)]
# 
# # Extract non-county columns from census data
# census_cols = setdiff(names(arkansas_census_geography), c("facility_id", "county"))
# 
# # Join the census geography data to the UST tanks data
# if (length(census_cols) > 0) {
#   AR_UST_tanks_SD = AR_UST_tanks_SD %>%
#     left_join(arkansas_census_geography, by = "facility_id") %>%
#     as.data.table()
#   
#   # Verify the census join worked
#   cat("\nAdded census geography data to tank data\n")
#   for (col in census_cols) {
#     cat("Number of facilities with", col, "data:", sum(!is.na(AR_UST_tanks_SD[[col]])), "\n")
#   }
#   
#   # If "county" is in the census data, print summary of county data
#   if ("county" %in% names(arkansas_census_geography)) {
#     cat("\nCounty data summary:\n")
#     cat("Unique county count:", uniqueN(AR_UST_tanks_SD$county), "\n")
#     cat("Facilities with county data:", sum(!is.na(AR_UST_tanks_SD$county)), "\n")
#   }
# }

cat("\nCensus geography data merge commented out pending GIS code fixes\n")

############################################################################################################
## Standardize county names and add FIPS codes
############################################################################################################

cat("\n============== COUNTY FIPS DEBUG START ==============\n")

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

cat("DEBUG: Loading county FIPS data from tigris Census package...\n")
# Use tigris package to load county FIPS codes
if (!require(tigris)) install.packages("tigris")
library(tigris)

# Load counties from the Census Bureau
counties <- counties(state = "AR", cb = TRUE)
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

cat("DEBUG: Loaded county FIPS data with", nrow(fips_data), "rows\n")
cat("DEBUG: County FIPS columns:", paste(names(fips_data), collapse=", "), "\n")
cat("DEBUG: Unique standardized counties in reference data:", uniqueN(fips_data$standardized_county), "\n")

# Check AR_UST_tanks_SD structure
cat("DEBUG: AR_UST_tanks_SD has", nrow(AR_UST_tanks_SD), "rows and", ncol(AR_UST_tanks_SD), "columns\n")

# Make sure county_name exists in AR_UST_tanks_SD
cat("DEBUG: Checking if county_name exists in AR_UST_tanks_SD...\n")
if (!"county_name" %in% names(AR_UST_tanks_SD)) {
  cat("DEBUG: county_name not found, merging from facility_tank_data...\n")
  # If county_name wasn't included in the original selection, bring it back from facility_tank_data
  AR_UST_tanks_SD <- merge(
    AR_UST_tanks_SD,
    unique(facility_tank_data[, .(Facility_ID, county_name)]),
    by.x = "facility_id",
    by.y = "Facility_ID",
    all.x = TRUE
  )

  cat("DEBUG: After merge, AR_UST_tanks_SD has", nrow(AR_UST_tanks_SD), "rows\n")
}

cat("DEBUG: county_name non-NA count:", sum(!is.na(AR_UST_tanks_SD$county_name)), "\n")

# Standardize county names in AR_UST_tanks_SD
cat("DEBUG: Standardizing county names in AR_UST_tanks_SD...\n")
AR_UST_tanks_SD[, standardized_county := standardize_county_name(county_name)]
cat("DEBUG: Unique standardized counties in AR_UST_tanks_SD:", uniqueN(AR_UST_tanks_SD$standardized_county), "\n")

# Join FIPS codes based on standardized county name
cat("DEBUG: Executing join operation using merge...\n")
AR_UST_tanks_SD <- merge(
  AR_UST_tanks_SD,
  fips_data[, .(standardized_county, county_fips = county_fips_code)],
  by = "standardized_county",
  all.x = TRUE
)
cat("DEBUG: Join complete\n")
cat("DEBUG: county_fips non-NA count after join:", sum(!is.na(AR_UST_tanks_SD$county_fips)), "\n")

cat("============== COUNTY FIPS DEBUG END ==============\n\n")

# Report on the match rate
cat("\nCounty FIPS code matching summary:\n")
cat("Total facilities:", uniqueN(AR_UST_tanks_SD$facility_id), "\n")
cat("Facilities with county FIPS codes:", sum(!is.na(AR_UST_tanks_SD$county_fips)), "\n")
cat("Match rate:", round(sum(!is.na(AR_UST_tanks_SD$county_fips)) / uniqueN(AR_UST_tanks_SD$facility_id) * 100, 2), "%\n")

# Display matches by county
cat("\nMatches by county:\n")
AR_UST_tanks_SD[!is.na(county_fips), .N, by = .(county_name, county_fips)][order(-N)]

# Define required columns order (similar to Texas file)
required_columns = c(
  "facility_id", "tank_id", "tank_installed_date", "tank_closed_date",
  "leak_after_closure", "leak_before_NFA_before_closure", "leak_before_NFA_after_closure", "no_leak",
  "capacity", "single_walled", "double_walled", "missing_walled", "unknown_walled",
  "is_gasoline", "is_diesel", "is_oil_kerosene", "is_jet_fuel", "is_other"
)

# Add county and FIPS columns to the list of columns to retain
additional_columns = c("county_name", "county_fips", setdiff(names(AR_UST_tanks_SD), 
                      c(required_columns, "county_name", "county_fips", "standardized_county")))

# Reorder columns with county_name and county_fips at the front of additional columns
setcolorder(AR_UST_tanks_SD, c(required_columns, "county_name", "county_fips", 
           setdiff(additional_columns, c("county_name", "county_fips"))))

# Remove temporary columns
AR_UST_tanks_SD[, standardized_county := NULL]

# Write out the final processed data
fwrite(AR_UST_tanks_SD, here('C:\\Users\\kaleb\\Box\\UST-Insurance\\Data\\Raw_do_not_write\\state_databases\\Arkansas\\', 'Arkansas_facility_tank_data.csv'))
fwrite(AR_LUST_SD, here('C:\\Users\\kaleb\\Box\\UST-Insurance\\Data\\Raw_do_not_write\\state_databases\\Arkansas\\', 'Arkansas_ust_lust_data.csv'))

# NOTE TO FUTURE CLAUDE:
# When the GIS code is fixed:
# 1. Uncomment the census geography section above
# 2. Adjust the county matching to either use the GIS-matched county data or combine both approaches
# 3. Update the column ordering to include any new census geography columns


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




## this measures upgrades for LA - INITIAL_CP_UPGRADE_DATE	SUBSEQUENT_CP_UPGRADE_DATE



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
la_file_path <- "C:/Users/kaleb/Box/UST-Insurance/Data/Raw_do_not_write/state_databases/Louisiana/LA_record_request.xlsx"

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
LA_UST_tanks[, tank_status := case_when(
  status_code == "A" ~ "Open",
  status_code %in% c("C", "C1", "I", "O", "R", "R1", "R2") ~ "Closed",
  status_code == "T" ~ "Temporary",
  TRUE ~ NA_character_
)]


# First, let's see what format the dates are in before conversion
cat("Sample tank_status_date values before conversion:\n")
print(head(LA_UST_tanks$tank_status_date, 10))

# Check if the date conversion worked correctly
cat("\nSample tank_status_date values after attempted conversion:\n")
print(head(LA_UST_tanks[, lubridate::is.Date(tank_status_date)], 10))
print(head(LA_UST_tanks$tank_status_date, 10))

# Fix the date conversion using the appropriate method for Excel dates
# If the dates are Excel serial numbers, use the janitor package or another approach
LA_UST_tanks[, tank_status_date := as.Date(tank_status_date, origin = "1899-12-30")]

# Now assign the properly converted dates to tank_closed_date
LA_UST_tanks[, tank_closed_date := NA_Date_] # Initialize with proper NA for Date type
LA_UST_tanks[, tank_closed_date := tank_status_date]


setDT(LA_UST_tanks)


LA_UST_tanks[, tank_installed_date := lubridate::ymd(tank_installed_date)]

LA_UST_tanks[tank_status !="Closed", tank_closed_date := NA_Date_]
# Filter out Temporary tanks (status_code = "T")
LA_UST_tanks <- LA_UST_tanks[tank_status != "Temporary"]

## group by tank_status and count
LA_UST_tanks[, .N, by = tank_status]

# Convert dates from Excel numeric format to proper R Date objects
# Excel dates are stored as days since 1899-12-30
# Use lubridate to parse dates in YMD format

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
    nfa_date     # nfa date
  ) %>%
  rename(
    facility_id = ai_number,
    LUST_id = incident_id,
    report_date = conf_rel_date
  ) %>%
  as.data.table()
 
 # Convert dates to proper format
LA_LUST_SD[, report_date := lubridate::ymd_hms(report_date)]
LA_LUST_SD[, nfa_date := lubridate::ymd_hms(nfa_date)]

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
LA_UST_tanks_SD <- LA_UST_tanks_SD[, .(
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
), keyby = .(facility_id, tank_id, tank_installed_date, tank_closed_date, county_name)]

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

# Report on the match rate
cat("\nCounty FIPS code matching summary:\n")
cat("Total facilities:", uniqueN(LA_UST_tanks_SD$facility_id), "\n")
cat("Facilities with county FIPS codes:", sum(!is.na(LA_UST_tanks_SD$county_fips)), "\n")
cat("Match rate:", round(sum(!is.na(LA_UST_tanks_SD$county_fips)) / uniqueN(LA_UST_tanks_SD$facility_id) * 100, 2), "%\n")

# Clean up temp column
LA_UST_tanks_SD[, standardized_county := NULL]

# Save final datasets
cat("\nSaving final Louisiana datasets...\n")

# Create directory if it doesn't exist
dir_path <- here("Data", "Raw_do_not_write", "state_databases", "Louisiana")
if (!dir.exists(dir_path)) {
  dir.create(dir_path, recursive = TRUE)
}



# Write tank data
fwrite(LA_UST_tanks_SD, here("Data", "Raw_do_not_write", "state_databases", "Louisiana", "LA_UST_tanks.csv"))

# Write LUST data
fwrite(LA_LUST_SD, here("Data", "Raw_do_not_write", "state_databases", "Louisiana", "LA_LUST.csv"))

cat("\nLouisiana data processing complete\n")
cat("Tank records:", nrow(LA_UST_tanks_SD), "\n")
cat("LUST records:", nrow(LA_LUST_SD), "\n")

# Count closed tanks
num_closed_tanks_LA = uniqueN(LA_UST_tanks_SD[!is.na(tank_closed_date), paste0(facility_id, "-", tank_id)])
cat("Closed tanks:", num_closed_tanks_LA, "\n")

# Count leaks
count_leak_after_closure = LA_UST_tanks_SD[leak_after_closure == 1, uniqueN(paste0(facility_id, "-", tank_id))]
count_leak_before_NFA_before_closure = LA_UST_tanks_SD[leak_before_NFA_before_closure == 1, uniqueN(paste0(facility_id, "-", tank_id))]
count_leak_before_NFA_after_closure = LA_UST_tanks_SD[leak_before_NFA_after_closure == 1, uniqueN(paste0(facility_id, "-", tank_id))]
count_no_leak = LA_UST_tanks_SD[no_leak == 1, uniqueN(paste0(facility_id, "-", tank_id))]

LA_counts = data.frame(
  category = c("leak_after_closure", "leak_before_NFA_before_closure", "leak_before_NFA_after_closure", "no_leak"),
  count = c(count_leak_after_closure, count_leak_before_NFA_before_closure, count_leak_before_NFA_after_closure, count_no_leak)
)
LA_counts$total = sum(LA_counts$count)
LA_counts$percent = (LA_counts$count / LA_counts$total) * 100

cat("\nLouisiana leak statistics:\n")
print(LA_counts)

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
classify_tank_walls <- function(data, tank_wall_col = "const_") {
  result <- copy(data)
  
  # Create binary variables for tank wall type based on const_ column
  # For Maine, we'll look for specific patterns in the construction field
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
classify_substances <- function(data, substance_col_name = "prod_") {
  # Create binary classification columns with default value
  result <- copy(data)
  result[, `:=`(
    is_gasoline = 0,
    is_diesel = 0,
    is_oil_kerosene = 0,
    is_jet_fuel = 0,
    is_other = 1  # Default to 1, will set to 0 if any other category matches
  )]
  
# Classification patterns based on Maine-specific contents list
gasoline_patterns <- paste(c(
  "gasoline unspecified", "leaded gasoline", "premium unleaded", 
  "regular gasoline", "unleaded gasoline", "unleaded plus"
), collapse = "|")

diesel_patterns <- paste(c(
  "diesel", "bio 1-74", "bio 75-99"
), collapse = "|")

oil_kerosene_patterns <- paste(c(
  "#1 fuel oil - kerosene", "#2 fuel oil", "#5 fuel oil", "#6 fuel oil", 
  "hydraulic oil", "lube oil", "oil - other - specified in report", 
  "unspecified oil", "waste oil/used motor oil"
), collapse = "|")

jet_fuel_patterns <- paste(c(
  "aviation gasoline", "jet fuel", "jp1", "jp4"
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
standardize_capacity <- function(data, capacity_col = "cap_") {
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

# Download and Load Raw Data - Maine files -----
cat("\nDownloading Maine UST data...\n")

# Create a temporary file to store the downloaded data
temp_file <- tempfile()

# Download the Maine tank data
download.file("https://www.maine.gov/dep/ftp/tanks/tanks_owner_all_registered_tanks.txt", 
              destfile = temp_file, mode = "wb")

# Read the file directly with fread using * as delimiter
ME_tanks_raw <- fread(temp_file, sep = "*", header = TRUE, fill = TRUE, 
                     colClasses = c(registration_number = "character", tank_number = "character")) %>%
  janitor::clean_names() %>%
  as.data.table()

# save out the raw data
if(first_run){
  dir.create(here("Data", "Raw_do_not_write", "state_databases", "Maine"), showWarnings = FALSE)
  fwrite(ME_tanks_raw, here("Data", "Raw_do_not_write", "state_databases", "Maine", "Maine_raw_tank_data.csv"))

  ## now save out the federally regulated USTs fed_regulated == Y
  ME_tanks_raw_fed_reg <- ME_tanks_raw[fed_regulated == "Y"]
  fwrite(ME_tanks_raw_fed_reg, here("Data", "Raw_do_not_write", "state_databases", "Maine", "Maine_raw_tank_data_fed_reg.csv"))
}

# Clean up the temporary file
# LUST Data Loading -----
cat("\nDownloading and processing Maine LUST data...\n")

# Define the base URL for the HOSS files
base_url <- "https://www.maine.gov/dep/ftp/hoss/"

# Define file names for the needed tables
tank_file         <- "report_tank_involved.txt"
spill_report_file <- "spill_report.txt"
spill_log_file    <- "spill_log.txt"

# Function to safely read * delimited files with known problematic line handling
safe_fread <- function(file_url, file_name) {
  tryCatch({
    temp_file <- tempfile()
    download_url <- paste0(file_url, file_name)
    cat(paste0("\nDownloading ", download_url, "...\n"))
    
    download.file(download_url, destfile = temp_file, mode = "wb", quiet = TRUE)
    
    # Read all lines from the file
    all_lines <- readLines(temp_file, warn = FALSE)
    total_lines <- length(all_lines)
    cat(paste0("File has ", total_lines, " lines total.\n"))
    
    # Check if this is the spill_report file with the known problematic line 70340
    if(file_name == "spill_report.txt" && total_lines >= 70340) {
      cat("Detected spill_report.txt with known problematic line 70340\n")
      cat("Creating a clean version with the problematic line removed...\n")
      
      # Create a temporary file without the problematic line
      clean_file <- tempfile()
      
      # Write all lines except the problematic one
      writeLines(all_lines[c(1:70339, 70341:total_lines)], clean_file)
      
      # Read the clean file
      dt <- fread(clean_file, sep = "*", header = FALSE, fill = TRUE, 
                 quote = "", data.table = TRUE, 
                 showProgress = FALSE, encoding = "Latin-1")
      
      cat(paste0("Successfully loaded ", file_name, " with ", nrow(dt), " rows and ", 
                ncol(dt), " columns (skipped line 70340).\n"))
      
      # Clean up
      file.remove(clean_file)
      return(dt)
    }
    
    # Standard process for other files or if spill_report doesn't have enough lines
    # Initialize variables to track progress and bad lines
    bad_lines <- integer(0)
    
    # First attempt - try reading the whole file
    cat(paste0("Attempting to read ", file_name, "...\n"))
    dt <- tryCatch({
      fread(temp_file, sep = "*", header = FALSE, fill = TRUE, 
            quote = "", data.table = TRUE, 
            showProgress = FALSE, encoding = "Latin-1")
    }, error = function(e) {
      cat(paste0("Standard read failed: ", e$message, "\n"))
      return(NULL)
    })
    
    # If the initial read fails, use incremental loading approach
    if (is.null(dt)) {
      cat("Switching to incremental loading to identify problematic rows...\n")
      
      # Create temporary file for clean data
      clean_file <- tempfile()
      
      # Write header line to clean file
      writeLines(all_lines[1], clean_file)
      
      # Process the file incrementally, skipping bad lines
      dt_chunks <- list()
      i <- 2  # Start after header
      
      while (i <= total_lines) {
        # Try to read a chunk of lines
        temp_chunk_file <- tempfile()
        chunk_lines <- min(i + 999, total_lines)
        
        # Write chunk to temp file
        writeLines(c(all_lines[1], all_lines[i:chunk_lines]), temp_chunk_file)
        
        # Try to read the chunk
        chunk_dt <- tryCatch({
          fread(temp_chunk_file, sep = "*", header = FALSE, fill = TRUE,
                quote = "", data.table = TRUE, showProgress = FALSE,
                encoding = "Latin-1", nThread = 1)
        }, error = function(e) {
          return(NULL)
        })
        
        # If chunk read failed, process lines one by one
        if (is.null(chunk_dt) && chunk_lines - i < 10) {
          # Process individual lines in the small problematic chunk
          for (j in i:chunk_lines) {
            single_line_file <- tempfile()
            writeLines(c(all_lines[1], all_lines[j]), single_line_file)
            
            single_dt <- tryCatch({
              fread(single_line_file, sep = "*", header = FALSE, fill = TRUE,
                   quote = "", data.table = TRUE, showProgress = FALSE,
                   encoding = "Latin-1", nThread = 1)
            }, error = function(e) {
              bad_lines <- c(bad_lines, j)
              cat(paste0("Line ", j, " is problematic and will be skipped.\n"))
              return(NULL)
            })
            
            if (!is.null(single_dt)) {
              # Append good line to clean file
              write.table(all_lines[j], clean_file, append = TRUE, 
                          row.names = FALSE, col.names = FALSE, quote = FALSE)
              dt_chunks <- c(dt_chunks, list(single_dt))
            }
            
            file.remove(single_line_file)
          }
        } else if (is.null(chunk_dt)) {
          # If larger chunk failed, reduce size and try again
          cat(paste0("Failed to read chunk from lines ", i, " to ", chunk_lines, ". Reducing chunk size.\n"))
          
          # Binary search approach to find problematic lines
          if (chunk_lines - i > 100) {
            # For large chunks, split in half and try again
            mid_point <- i + floor((chunk_lines - i) / 2)
            
            # First try the first half
            first_half_file <- tempfile()
            writeLines(c(all_lines[1], all_lines[i:mid_point]), first_half_file)
            
            first_half_dt <- tryCatch({
              fread(first_half_file, sep = "*", header = FALSE, fill = TRUE,
                   quote = "", data.table = TRUE, showProgress = FALSE,
                   encoding = "Latin-1", nThread = 1)
            }, error = function(e) {
              return(NULL)
            })
            
            if (!is.null(first_half_dt)) {
              dt_chunks <- c(dt_chunks, list(first_half_dt))
              # Append good chunk
              write.table(all_lines[i:mid_point], clean_file, append = TRUE, 
                          row.names = FALSE, col.names = FALSE, quote = FALSE,
                          sep = "\n")
            } else {
              # This half has issues, mark for more detailed processing
              cat(paste0("Issues in lines ", i, " to ", mid_point, ", will process in smaller chunks.\n"))
            }
            
            file.remove(first_half_file)
            
            # Then try the second half
            second_half_file <- tempfile()
            writeLines(c(all_lines[1], all_lines[(mid_point+1):chunk_lines]), second_half_file)
            
            second_half_dt <- tryCatch({
              fread(second_half_file, sep = "*", header = FALSE, fill = TRUE,
                   quote = "", data.table = TRUE, showProgress = FALSE,
                   encoding = "Latin-1", nThread = 1)
            }, error = function(e) {
              return(NULL)
            })
            
            if (!is.null(second_half_dt)) {
              dt_chunks <- c(dt_chunks, list(second_half_dt))
              # Append good chunk
              write.table(all_lines[(mid_point+1):chunk_lines], clean_file, append = TRUE, 
                          row.names = FALSE, col.names = FALSE, quote = FALSE,
                          sep = "\n")
            } else {
              # This half has issues, mark for more detailed processing
              cat(paste0("Issues in lines ", (mid_point+1), " to ", chunk_lines, ", will process in smaller chunks.\n"))
            }
            
            file.remove(second_half_file)
            
            # Move to next chunk
            i <- chunk_lines + 1
          } else {
            # For small chunks, move forward by one line and try again
            bad_lines <- c(bad_lines, i)
            cat(paste0("Line ", i, " appears problematic and will be skipped.\n"))
            i <- i + 1
          }
          next
        } else {
          # Chunk read successfully
          cat(paste0("Successfully read lines ", i, " to ", chunk_lines, ".\n"))
          dt_chunks <- c(dt_chunks, list(chunk_dt))
          # Append good chunk to clean file
          write.table(all_lines[i:chunk_lines], clean_file, append = TRUE, 
                      row.names = FALSE, col.names = FALSE, quote = FALSE,
                      sep = "\n")
        }
        
        i <- chunk_lines + 1
        file.remove(temp_chunk_file)
      }
      
      # Now read the clean file with all good lines
      if (length(dt_chunks) > 0) {
        dt <- rbindlist(dt_chunks, fill = TRUE)
        
        # Save list of bad lines
        if (length(bad_lines) > 0) {
          bad_lines_file <- paste0(temp_file, ".bad_lines")
          writeLines(as.character(bad_lines), bad_lines_file)
          cat(paste0("Identified ", length(bad_lines), " problematic lines. List saved to ", bad_lines_file, "\n"))
        }
      } else {
        cat("Failed to read any valid data from the file.\n")
        return(data.table())
      }
    }
    
    cat(paste0("Successfully loaded ", file_name, " with ", nrow(dt), " rows and ", 
              ncol(dt), " columns.\n"))
    
    if (length(bad_lines) > 0) {
      cat(paste0("Skipped ", length(bad_lines), " problematic lines.\n"))
    }
    
    return(dt)
  }, error = function(e) {
    cat(paste0("\nFailed to load ", file_name, ": ", e$message, "\n"))
    # Return an empty data.table to avoid breaking the workflow
    return(data.table())
  }, finally = {
    # Clean up temporary files
    if (exists("temp_file") && file.exists(temp_file)) file.remove(temp_file)
  })
}

# Load the LUST data files using the safe_fread function
cat("\nLoading LUST data files...\n")
tank_raw_full         <- safe_fread(base_url, tank_file)
spill_report_raw_full <- safe_fread(base_url, spill_report_file)
spill_log_raw_full    <- safe_fread(base_url, spill_log_file)

# Check if any files failed to load completely
if (nrow(tank_raw_full) == 0 || nrow(spill_report_raw_full) == 0 || nrow(spill_log_raw_full) == 0) {
  warning("One or more LUST data files may have failed to load completely.")
}

# Display summary of loaded data
cat("\nSummary of loaded LUST data:\n")
cat("Tank data:", nrow(tank_raw_full), "rows,", ncol(tank_raw_full), "columns\n")
cat("Spill report data:", nrow(spill_report_raw_full), "rows,", ncol(spill_report_raw_full), "columns\n")
cat("Spill log data:", nrow(spill_log_raw_full), "rows,", ncol(spill_log_raw_full), "columns\n")

# Select only the columns we want and rename them based on the comments:

# From tank_raw - selecting only columns marked "-- Want this one":
# - Column 1: L.SPILL_NUMBER
# - Column 5: UST_REGISTERED_FLAG
# - Column 6: UST_TANK_SITE_NUMBER
tank_raw <- tank_raw_full[, .(
  l_spill_number = V1,          # L.SPILL_NUMBER
  ust_registered_flag = V5,     # UST_REGISTERED_FLAG
  UST_TANK_SITE_NUMBER = V6     # UST_TANK_SITE_NUMBER
)]

# From spill_report_raw - selecting only columns marked "-- Want this one":
# - Column 1: L.SPILL_NUMBER
# - Column 4: MODIFY_DATE
# - Column 6: REPORT_STATUS
# - Column 7: REPORT_STATUS_VALUE
spill_report_raw <- spill_report_raw_full[, .(
  l_spill_number = V1,          # L.SPILL_NUMBER
  modify_date = V4,             # MODIFY_DATE
  report_status = V6,           # REPORT_STATUS
  report_status_value = V7      # REPORT_STATUS_VALUE
)]

# From spill_log_raw - selecting only columns marked "-- Want this one":
# - Column 1: L.SPILL_NUMBER
# - Column 16: LOG_REPORTED_DATETIME
spill_log_raw <- spill_log_raw_full[, .(
  l_spill_number = V1,          # L.SPILL_NUMBER
  log_reported_datetime = V16   # LOG_REPORTED_DATETIME
)]

# If desired, save the raw data files for future reference
if (first_run) {
  dir.create(here("Data", "Raw_do_not_write", "state_databases", "Maine", "LUST"), 
             recursive = TRUE, showWarnings = FALSE)
  fwrite(tank_raw, here("Data", "Raw_do_not_write", "state_databases", "Maine", "LUST", 
                       "tank_raw.csv"))
  fwrite(spill_report_raw, here("Data", "Raw_do_not_write", "state_databases", "Maine", "LUST", 
                               "spill_report_raw.csv"))
  fwrite(spill_log_raw, here("Data", "Raw_do_not_write", "state_databases", "Maine", "LUST", 
                            "spill_log_raw.csv"))
}


# ----------------------------------------
# Filter and Merge Data
# ----------------------------------------

# 1. Filter for federally regulated USTs in the tank table
#    (Assuming ust_registered_flag "T" indicates federally regulated)
tank_federal <- tank_raw[ust_registered_flag == "T"]

# 2. Filter Spill_Report.txt for Final Report entries
#    When report_status_value is "Final Report" or "FR", we use modify_date as the NFA date.
spill_report_final <- spill_report_raw[report_status_value %in% c("Final Report", "FR")]

# 3. Merge using the common key "l_spill_number"
# Merge tank data with spill_log to get the reported date (log_reported_datetime)
merged1 <- merge(
  tank_federal, 
  spill_log_raw[, .(l_spill_number, log_reported_datetime)], 
  by = "l_spill_number", 
  all.x = TRUE
)

# Merge the result with the final report data to get the NFA date (modify_date)
merged2 <- merge(
  merged1, 
  spill_report_final[, .(l_spill_number, modify_date)], 
  by = "l_spill_number", 
  all.x = TRUE
)
# ----------------------------------------
# Select and Rename the Variables Using data.table Syntax
# ----------------------------------------
# Final fields:
#   facility_id   from UST_TANK_SITE_NUMBER (Report_Tank_Involved.txt)
#   lust_id       from L.SPILL_NUMBER         (unique spill identifier)
#   reported_date from LOG_REPORTED_DATETIME   (Spill_Log.txt)
#   nfa_date      from MODIFY_DATE             (Spill_Report.txt, for Final Report)
ME_lust_raw <- merged2[, .(
  facility_id   = UST_TANK_SITE_NUMBER,  # this is the correct variable name from merged2
  lust_id       = l_spill_number,        # change L.SPILL_NUMBER to l_spill_number
  reported_date = log_reported_datetime, # change LOG_REPORTED_DATETIME to log_reported_datetime
  nfa_date      = modify_date            # change MODIFY_DATE to modify_date
)] %>% clean_names() %>% as.data.table()
# Build the final dataset selecting the fields:
#   facility_id: from ust_tank_site_number (the facility identifier)
#   lust_id: from l_spill_number (the spill/leak identifier)
#   reported_date: from log_reported_datetime
#   nfa_date: from modify_date (when the report is a Final Report)

# Save the raw LUST data
if(first_run){
  fwrite(ME_lust_raw, here("Data", "Raw_do_not_write", "state_databases", "Maine", "Maine_LUST_data.csv"))
}

# Print summaries of loaded data
cat("\nLoaded", nrow(ME_tanks_raw), "tank records and", nrow(ME_lust_raw), "LUST records\n")
cat("\nTank data column names:", paste(names(ME_tanks_raw), collapse = ", "), "\n")
cat("\nLUST data column names:", paste(names(ME_lust_raw), collapse = ", "), "\n")

# Process Tank Data -----
cat("\nProcessing Maine tank data...\n")

# Check the column names to identify facility_id, tank_id, installation_date, closure_date, etc.
# This will need to be adjusted based on the actual column names in the Maine data
cat("\nExamining column names in Maine tank data...\n")
print(names(ME_tanks_raw))

# Assuming column mappings based on typical naming conventions:
# facility_id -> fac_id or facility_id
# tank_id -> tank_id or tank_no
# installation_date -> install_date or installation_date
# closure_date -> closure_date, removal_date, or close_date
# For now, we'll use placeholder column names and adjust as needed

# Select and rename columns according to the required format
ME_UST_tanks <- ME_tanks_raw %>%
  mutate(
    registration_number = as.character(registration_number),
    tank_number = as.character(tank_number)
  ) %>%
  select(
    registration_number,           # facility_id
    tank_number,                   # tank_id
    date_tank_installed,           # tank_installed_date
    tank_status_date,              # tank_closed_date when tank_status indicates tank is closed
 #   county= NA,                        # county_name
    tank_volume_in_gallons,        # capacity
    product_stored,                # substance
    tank_material_label,           # tank wall type
    tank_status_label              # tank status
  ) %>%
  rename(
    facility_id = registration_number,
    tank_id = tank_number,
    tank_installed_date = date_tank_installed,
    tank_closed_date = tank_status_date,
 #   county_name = county,
    capacity = tank_volume_in_gallons,
    tank_contents = product_stored,
    tank_structure = tank_material_label,
    status = tank_status_label
  ) %>%
  as.data.table()

# Add state columns
ME_UST_tanks[, state := "Maine"]
ME_UST_tanks[, state_abbr := "ME"]

glimpse(ME_UST_tanks)

# Convert dates to proper format (assuming format in data, adjust as needed)
ME_UST_tanks[, tank_installed_date := lubridate::mdy(tank_installed_date)]
ME_UST_tanks[, tank_closed_date := lubridate::mdy(tank_closed_date)]

# Process tanks based on status description
cat("\nProcessing tank status...\n")
# drop all PLANNED FOR INSTALLATION tanks
ME_UST_tanks <- ME_UST_tanks[status != "PLANNED FOR INSTALLATION"]
# Classify tank status based on the status field
ME_UST_tanks[, tank_status := "Open"]  # Default to Open

# Then identify closed tanks based on specific conditions
ME_UST_tanks[grepl("Removed|Closed|Out of Service|Abandoned", status, ignore.case = TRUE), 
       tank_status := "Closed"]

# For closed tanks without a close date, set to NA and print a warning
missing_closed_dates <- sum(ME_UST_tanks$tank_status == "Closed" & is.na(ME_UST_tanks$tank_closed_date))
if(missing_closed_dates > 0) {
  cat("\nWARNING:", missing_closed_dates, "closed tanks have missing closure dates\n")
}

# Apply standardization functions
cat("\nStandardizing capacity values...\n")
ME_UST_tanks <- standardize_capacity(ME_UST_tanks, "capacity")

cat("\nClassifying tank wall types...\n")
ME_UST_tanks <- classify_tank_walls(ME_UST_tanks, "tank_structure")

cat("\nClassifying substance types...\n")
ME_UST_tanks <- classify_substances(ME_UST_tanks, "tank_contents")

# Add county FIPS codes
cat("\nAdding county FIPS codes...\n")

# Add "ME" to the beginning of facility_id if it's not already there
ME_UST_tanks[, facility_id_census := paste0("ME", facility_id)]

# Try to use census geography data for Maine 
cat("\nLooking for Maine census geography file...\n")

# Define path pattern for the Maine census geography file
census_file_path <- Sys.glob("C:/Users/kaleb/Box/UST-Insurance/Data/Processed/Census_Geography/Maine_census_geography.csv")

cat(paste0("\nLoading census geography data for Maine from ", census_file_path[1], "\n"))
census_data <- fread(census_file_path[1])

# Convert facility_id to character for joining
census_data[, facility_id := as.character(facility_id)]

# Join with census data based on facility_id
ME_UST_tanks <- merge(
  ME_UST_tanks,
  census_data[, .(facility_id,county_name ,county_geoid)],
  by.x = "facility_id_census",
  by.y = "facility_id",
  all.x = TRUE
)

# Rename county_geoid to county_fips
setnames(ME_UST_tanks, "county_geoid", "county_fips")

# Verify join success
cat("\nAdded census geography data for Maine\n")
cat("Total facilities:", uniqueN(ME_UST_tanks$facility_id), "\n")
cat("Facilities with county FIPS codes:", uniqueN(ME_UST_tanks[!is.na(county_fips), facility_id]), "\n")
cat("Match rate:", round(uniqueN(ME_UST_tanks[!is.na(county_fips), facility_id]) / uniqueN(ME_UST_tanks$facility_id) * 100, 2), "%\n")

# For records without county FIPS codes, they will remain NA
if(sum(is.na(ME_UST_tanks$county_fips)) > 0) {
  cat("\nWARNING:", sum(is.na(ME_UST_tanks$county_fips)), "records have missing county FIPS codes\n")
}

# Process LUST Data -----
cat("\nProcessing Maine LUST data...\n")

# Process the LUST data
ME_LUST_SD <- ME_lust_raw %>%
  mutate(
    facility_id = facility_id,  # Adjust as needed based on actual column name
    LUST_id = lust_id,          # Adjust as needed based on actual column name
    report_date = lubridate::mdy(reported_date),
    nfa_date = lubridate::mdy(nfa_date)               # NFA date might not be available
  ) %>%
  select(facility_id, LUST_id, report_date, nfa_date) %>%
  as.data.table()

# Join LUST data with tank data
cat("\nJoining LUST data with tank data...\n")

ME_LUST_SD_collapsed <- ME_LUST_SD[, .(LUST_count = .N), by = .(facility_id, report_date, nfa_date)]
glimpse(ME_LUST_SD_collapsed)
glimpse(ME_UST_tanks)   
ME_UST_tanks[,facility_id:=as.integer(facility_id)]
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

# Aggregate leak indicators at facility-tank level - including county_fips
ME_UST_tanks_SD <- ME_UST_tanks_LUST %>%
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
ME_UST_tanks_SD <- ME_UST_tanks_SD[, .(
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
cat("\nSaving final Maine datasets...\n")

# Create directory if it doesn't exist
dir_path <- here("Data", "Raw_do_not_write", "state_databases", "Maine")
if (!dir.exists(dir_path)) {
  dir.create(dir_path, recursive = TRUE)
}

# Write tank data
fwrite(ME_UST_tanks_SD, here("Data", "Raw_do_not_write", "state_databases", "Maine", "ME_UST_tanks.csv"))

# Write LUST data
fwrite(ME_LUST_SD, here("Data", "Raw_do_not_write", "state_databases", "Maine", "ME_LUST.csv"))

cat("\nMaine data processing complete\n")
cat("Tank records:", nrow(ME_UST_tanks_SD), "\n")
cat("LUST records:", nrow(ME_LUST_SD), "\n")

# Count closed tanks
num_closed_tanks_ME = uniqueN(ME_UST_tanks_SD[!is.na(tank_closed_date), paste0(facility_id, "-", tank_id)])
cat("Closed tanks:", num_closed_tanks_ME, "\n")

# Count leaks
count_leak_after_closure = ME_UST_tanks_SD[leak_after_closure == 1, uniqueN(paste0(facility_id, "-", tank_id))]
count_leak_before_NFA_before_closure = ME_UST_tanks_SD[leak_before_NFA_before_closure == 1, uniqueN(paste0(facility_id, "-", tank_id))]
count_leak_before_NFA_after_closure = ME_UST_tanks_SD[leak_before_NFA_after_closure == 1, uniqueN(paste0(facility_id, "-", tank_id))]
count_no_leak = ME_UST_tanks_SD[no_leak == 1, uniqueN(paste0(facility_id, "-", tank_id))]

ME_counts = data.frame(
  category = c("leak_after_closure", "leak_before_NFA_before_closure", "leak_before_NFA_after_closure", "no_leak"),
  count = c(count_leak_after_closure, count_leak_before_NFA_before_closure, count_leak_before_NFA_after_closure, count_no_leak)
)
ME_counts$total = sum(ME_counts$count)
ME_counts$percent = (ME_counts$count / ME_counts$total) * 100

cat("\nMaine leak statistics:\n")
print(ME_counts)

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

