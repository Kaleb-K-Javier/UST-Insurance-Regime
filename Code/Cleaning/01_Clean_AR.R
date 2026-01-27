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
library(stringr)

# Establish database connection
# Updated path to Data/Raw/state_databases
db_path <- here("Data","Raw","state_databases","Arkansas","TankStats_web.mdb")
con <- dbConnect(
  odbc(),
  .connection_string = paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};Dbq=", db_path, ";")
)

# Load required tables
rfacs_data <- as.data.table(dbReadTable(con, "TempRFACS_CountryNO"))
tankstats_ug <- as.data.table(dbReadTable(con, "TempTankStats_UG"))
lust_data <- as.data.table(dbReadTable(con, "TempRLUSTLOG1"))

# Clean and standardize facility IDs to remove whitespace AND force to character
rfacs_data[, FACILITY_ID := as.character(trimws(FACILITY_ID))]
tankstats_ug[, RSTFacNbr := as.character(trimws(RSTFacNbr))]
tankstats_ug[, TankNbr := as.character(trimws(TankNbr))]
lust_data[, RSTFacNbr := as.character(trimws(RSTFacNbr))]

# Select and rename relevant columns for RFACS
# Added facility_name (LOC_NAME)
rfacs_data <- rfacs_data[, .(
  Facility_ID = FACILITY_ID,
  facility_name = LOC_NAME, 
  county_name = trimws(get(COUNTYVARIABLE))
)]

# Select and rename relevant columns for TempTankStats_UG
tankstats_data <- tankstats_ug[, .(
  Facility_ID = RSTFacNbr,                # Facility ID
  Tank_ID = TankNbr,                      # Tank number
  Tank_Installation_Date = InstallDate,   # Tank installation date
  Tank_Status_Change_Reason = TankStatusChangeRsnCode, # Reason code
  Tank_Status_Type = TankStatusTypeCode,  # Status type
  Tank_Status_Date = TankStatusDate,      # Date associated with the status
  
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
  Tank_Removal_Date = NA,                 # Open tanks have no removal date
  tank_status = "Open",                   # Explicit Status
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
    tank_status = "Closed",                # Explicit Status
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

# Disconnect from the database
dbDisconnect(con)

# how many leaks found by closure 
sum(ust_lust_table$leak_found_after_closure)
# how many found during inspection
sum(ust_lust_table$found_during_inspection)

facility_tank_data <- unique(facility_tank_data)
ust_lust_table <- unique(ust_lust_table)

# Collapse LUST table to one row per Facility_ID and ReleaseConfirmedDate
collapsed_ust_lust <- ust_lust_table[, .(LUST_count = .N), by = .(Facility_ID, ReleaseConfirmedDate,NFAIssuedDate)]

# Merge the facility_tank_data with the collapsed LUST dataset
facility_tank_data_LUST <- merge.data.table(
  facility_tank_data,
  collapsed_ust_lust,
  by = "Facility_ID",
  all.x = TRUE,
   allow.cartesian = TRUE
 )

# turn the date cols into date objects, using lubridate lust data is ymd  tank data is ymd as well
facility_tank_data_LUST[, ReleaseConfirmedDate := ymd(ReleaseConfirmedDate)]
facility_tank_data_LUST[, NFAIssuedDate := ymd(NFAIssuedDate)]
facility_tank_data_LUST[, Tank_Removal_Date := ymd(Tank_Removal_Date)]
facility_tank_data_LUST[, Tank_Installation_Date := ymd(Tank_Installation_Date)]

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
    county_name,
    facility_name,
    tank_status
  )
][
  ,
  .(
    facility_name = first(facility_name),
    tank_status = first(tank_status), # Keep the state specific status
    leak_after_closure = ifelse(sum(leak_after_closure, na.rm = TRUE) > 0, 1, 0),
    leak_before_NFA_before_closure = ifelse(sum(leak_before_NFA_before_closure, na.rm = TRUE) > 0, 1, 0),
    leak_before_NFA_after_closure = ifelse(sum(leak_before_NFA_after_closure, na.rm = TRUE) > 0, 1, 0),
    no_leak = ifelse(sum(no_leak, na.rm = TRUE) > 0, 1, 0),
    capacity = first(capacity),
    single_walled = max(single_walled, na.rm = TRUE),
    double_walled = max(double_walled, na.rm = TRUE),
    # Consolidate missing into unknown
    unknown_walled = max(unknown_walled, missing_walled, na.rm = TRUE),
    is_gasoline = max(is_gasoline, na.rm = TRUE),
    is_diesel = max(is_diesel, na.rm = TRUE),
    is_oil_kerosene = max(is_oil_kerosene, na.rm = TRUE),
    is_jet_fuel = max(is_jet_fuel, na.rm = TRUE),
    is_other = max(is_other, na.rm = TRUE)
  ),
  by = .(Facility_ID, Tank_ID, Tank_Installation_Date, Tank_Removal_Date, county_name)
]

# Rename columns to a standard set
setnames(
  AR_UST_tanks_SD,
  c("Facility_ID", "Tank_ID", "Tank_Installation_Date", "Tank_Removal_Date"),
  c("facility_id", "tank_id", "tank_installed_date", "tank_closed_date")
)

# Standardize the LUST table
AR_LUST_SD <- ust_lust_table[
  ,
  .(
    facility_id = as.character(Facility_ID),
    LUST_id = as.character(LUST_ID),
    report_date = ReleaseConfirmedDate,
    nfa_date = NFAIssuedDate,
    state = "AR"
  )
]

# Final Cleaning and Column Ordering
AR_UST_tanks_SD[, state := "AR"]
AR_UST_tanks_SD[, facility_name := stringr::str_to_title(trimws(gsub("[^[:alnum:] ]", "", facility_name)))]
# Ensure IDs are character
AR_UST_tanks_SD[, facility_id := as.character(facility_id)]
AR_UST_tanks_SD[, tank_id := as.character(tank_id)]

# Define required columns order
required_columns <- c(
  "facility_id", "facility_name", "tank_id", "state", "tank_installed_date", "tank_closed_date", "tank_status",
  "leak_after_closure", "leak_before_NFA_before_closure", "leak_before_NFA_after_closure", "no_leak",
  "capacity", "single_walled", "double_walled", "unknown_walled",
  "is_gasoline", "is_diesel", "is_oil_kerosene", "is_jet_fuel", "is_other", "county_name"
)

# Keep FIPS if it exists (from the GIS section if enabled), otherwise just required
cols_to_keep <- intersect(names(AR_UST_tanks_SD), c(required_columns, "county_fips"))
AR_UST_tanks_SD <- AR_UST_tanks_SD[, ..cols_to_keep]

# Create output directory
output_dir <- here("Data","Raw","state_databases","Arkansas")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# Write out the final processed data
fwrite(AR_UST_tanks_SD, file.path(output_dir, "AR_Harmonized_UST_tanks.csv"))
fwrite(AR_LUST_SD, file.path(output_dir, "AR_Harmonized_LUST.csv"))
