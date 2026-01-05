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
