## Arkansas UST Dataset Creation
# Script: 01_Clean_AR.R
# Description: Harmonizes AR UST/LUST data for econometrics, aligned with LA standards.
#              Updates Jan 2026: Implements APC&EC Rule 12 classification logic.

# 0. Setup ----------------------------------------------------------------
COUNTYVARIABLE <- "CountyCAPS"

library(tidyverse)
library(data.table)
library(here)
library(lubridate)
library(janitor)
library(fixest)
library(did)
library(fastDummies)
library(stringr)

# Additional packages for Arkansas Access DB
library(DBI)
library(odbc)

# 1. Helper Functions -----------------------------------------------------

# Diagnostic remediation: max() returns -Inf when all values are NA.
# This function returns 0 in that case to prevent data corruption.
safe_max <- function(x, na.rm = TRUE) {
  if (all(is.na(x))) return(0)
  val <- suppressWarnings(max(x, na.rm = na.rm))
  if (is.infinite(val)) return(0)
  return(val)
}

# Standardize county names (Harmonization with LA script)
standardize_county_name <- function(county_name) {
  name <- tolower(county_name)
  name <- gsub(" county$| counties$| parish$| parishes$", "", name)
  name <- gsub("[^a-z ]", "", name)
  name <- trimws(gsub("\\s+", " ", name))
  name[name == ""] <- NA
  return(name)
}

# 2. Data Import ----------------------------------------------------------

# Establish database connection
db_path <- here("Data","Raw","state_databases","Arkansas","TankStats_web.mdb")
con <- dbConnect(
  odbc(),
  .connection_string = paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};Dbq=", db_path, ";")
)

# Load required tables
rfacs_data <- as.data.table(dbReadTable(con, "TempRFACS_CountryNO"))
tankstats_ug <- as.data.table(dbReadTable(con, "TempTankStats_UG"))
lust_data <- as.data.table(dbReadTable(con, "TempRLUSTLOG1"))

# Disconnect
dbDisconnect(con)

# Clean IDs
rfacs_data[, FACILITY_ID := as.character(trimws(FACILITY_ID))]
tankstats_ug[, RSTFacNbr := as.character(trimws(RSTFacNbr))]
tankstats_ug[, TankNbr := as.character(trimws(TankNbr))]
lust_data[, RSTFacNbr := as.character(trimws(RSTFacNbr))]

# 3. Process Facilities (Geography) ---------------------------------------

# Rename and standardize geography columns to match LA format
rfacs_data <- rfacs_data[, .(
  Facility_ID = FACILITY_ID,
  facility_name = LOC_NAME,
  address = LOC_ADDR,
  city = LOC_CITY,
  zip = LOC_ZIP,
  latitude = LOC_LATITUDE,   # Harmonized name
  longitude = LOC_LONGITUDE, # Harmonized name
  county_name = trimws(get(COUNTYVARIABLE))
)]

# Apply standardization
rfacs_data[, county_name := standardize_county_name(county_name)]

# 4. Process Tanks & Classification ---------------------------------------

tankstats_data <- tankstats_ug[, .(
  Facility_ID = RSTFacNbr,
  Tank_ID = TankNbr,
  Tank_Installation_Date = InstallDate,
  Tank_Status_Change_Reason = TankStatusChangeRsnCode,
  Tank_Status_Type = TankStatusTypeCode,
  Tank_Status_Date = TankStatusDate,
  
  # Construction flags
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
  
  # Substance flags
  gasoline = ifelse(UT_SS_GasolineTF == TRUE, 1, 0),
  diesel = ifelse(UT_SS_DieselTF == TRUE, 1, 0),
  kerosene = ifelse(UT_SS_KeroseneTF == TRUE, 1, 0),
  used_oil = ifelse(UT_SS_UsedOilTF == TRUE, 1, 0),
  new_oil = ifelse(UT_SS_NewOilTF == TRUE, 1, 0),
  empty = ifelse(UT_SS_EmptyTF == TRUE, 1, 0),
  unknown = ifelse(UT_SS_UnknownTF == TRUE, 1, 0)
)]

# Handle NAs in binary columns
binary_cols <- c("steel", "epoxy", "composite", "fiberglass_plastic", "concrete", 
                 "int_lining", "excv_liner", "dbl_wall", "poly_jacket", "unknown_mat",
                 "gasoline", "diesel", "kerosene", "used_oil", "new_oil", "empty", "unknown")
for(col in binary_cols) {
  tankstats_data[is.na(get(col)), (col) := 0]
}

# --- IMPLEMENT ARKANSAS CLASSIFICATION ALGORITHM (APC&EC Rule 12) ---
# Citation: APC&EC Rule 12.504 - July 1, 2007 cutoff
# Citation: 40 CFR 280.21 - Internal Lining is an upgrade for existing single-walled tanks

tankstats_data[, install_date_final := as.Date(Tank_Installation_Date)]
AR_SEC_CON_DATE <- as.Date("2007-07-01") 
FRP_CUTOFF      <- as.Date("1990-01-01") 

tankstats_data[, ust_classification := case_when(
  # 1. Regulatory Mandate (State Rule 12.504)
  !is.na(install_date_final) & install_date_final >= AR_SEC_CON_DATE ~ "Secondary_Contained",
  
  # 2. Explicit Double-Wall Indicators (Database Flags)
  dbl_wall == 1 ~ "Secondary_Contained",
  composite == 1 ~ "Secondary_Contained",   # ACT-100 is inherently double-walled or protected
  poly_jacket == 1 ~ "Secondary_Contained", # Jacket provides secondary barrier
  excv_liner == 1 ~ "Secondary_Contained",  # Liner acts as secondary containment
  
  # 3. Fiberglas/Plastic Inference (Date Dependent)
  fiberglass_plastic == 1 & !is.na(install_date_final) & install_date_final >= FRP_CUTOFF ~ "Secondary_Contained",
  
  # 4. Single Wall Indicators (Correction: lining/epoxy are upgrades, not containment)
  int_lining == 1 ~ "Single_Walled",
  epoxy == 1 ~ "Single_Walled",
  steel == 1 & dbl_wall == 0 ~ "Single_Walled",
  
  # 5. Default
  TRUE ~ "Unknown"
)]

# Map Classification to Binary Columns for Harmonization
tankstats_data[, `:=`(
  double_walled = ifelse(ust_classification == "Secondary_Contained", 1, 0),
  single_walled = ifelse(ust_classification == "Single_Walled", 1, 0),
  unknown_walled = ifelse(ust_classification == "Unknown", 1, 0),
  
  # Missing logic retained for compatibility, though largely superseded by "Unknown"
  missing_walled = ifelse(ust_classification == "Unknown", 1, 0)
)]

# Construct Harmonized Substances
tankstats_data[, `:=`(
  is_gasoline = gasoline,
  is_diesel = diesel,
  is_oil_kerosene = pmax(kerosene, used_oil, new_oil, na.rm=TRUE),
  is_jet_fuel = 0,
  is_other = ifelse(empty + unknown > 0, 1, 0)
)]
tankstats_data[is_gasoline + is_diesel + is_oil_kerosene + is_jet_fuel == 0, is_other := 1]

# Tank Status Logic (Relaxed per diagnostic findings)
open_tanks <- tankstats_data[grepl("IU|TO", Tank_Status_Type, ignore.case = TRUE), .(
  Facility_ID, Tank_ID, Tank_Installation_Date,
  Tank_Removal_Date = NA,
  tank_status = "Open",
  capacity, single_walled, double_walled, missing_walled, unknown_walled,
  is_gasoline, is_diesel, is_oil_kerosene, is_jet_fuel, is_other,
  ust_classification  # Kept for validation
)]

closed_tanks <- tankstats_data[grepl("PO", Tank_Status_Type, ignore.case = TRUE), .(
  Facility_ID, Tank_ID, Tank_Installation_Date,
  Tank_Removal_Date = Tank_Status_Date,
  tank_status = "Closed",
  capacity, single_walled, double_walled, missing_walled, unknown_walled,
  is_gasoline, is_diesel, is_oil_kerosene, is_jet_fuel, is_other,
  ust_classification # Kept for validation
)]

facility_tank_data <- bind_rows(open_tanks, closed_tanks)

# Merge with Geography
facility_tank_data <- merge(rfacs_data, facility_tank_data, by = "Facility_ID", all.x = TRUE)
facility_tank_data <- unique(facility_tank_data)

# 5. Process LUSTs --------------------------------------------------------

ust_lust_table <- lust_data[, .(
  Facility_ID = RSTFacNbr,             
  LUST_ID = LUSTNoticeNbr,          
  ReleaseConfirmedDate,              
  NFAIssuedDate                      
)]
ust_lust_table <- unique(ust_lust_table)

# 6. Merge & Leak Logic ---------------------------------------------------

# Collapse LUSTs for cartesian merge
collapsed_ust_lust <- ust_lust_table[, .(LUST_count = .N), by = .(Facility_ID, ReleaseConfirmedDate, NFAIssuedDate)]

facility_tank_data_LUST <- merge.data.table(
  facility_tank_data,
  collapsed_ust_lust,
  by = "Facility_ID",
  all.x = TRUE,
  allow.cartesian = TRUE
)

# Date formatting
facility_tank_data_LUST[, ReleaseConfirmedDate := ymd(ReleaseConfirmedDate)]
facility_tank_data_LUST[, NFAIssuedDate := ymd(NFAIssuedDate)]
facility_tank_data_LUST[, Tank_Removal_Date := ymd(Tank_Removal_Date)]
facility_tank_data_LUST[, Tank_Installation_Date := ymd(Tank_Installation_Date)]

# Leak Classification
facility_tank_data_LUST[, `:=`(
  leak_after_closure = ifelse(!is.na(Tank_Removal_Date) & ReleaseConfirmedDate > Tank_Removal_Date, 1, 0),
  leak_before_NFA_before_closure = ifelse(!is.na(Tank_Removal_Date) & ReleaseConfirmedDate < Tank_Removal_Date & Tank_Removal_Date > NFAIssuedDate, 1, 0),
  leak_before_NFA_after_closure = ifelse(!is.na(Tank_Removal_Date) & ReleaseConfirmedDate < Tank_Removal_Date & Tank_Removal_Date < NFAIssuedDate, 1, 0),
  no_leak = ifelse(is.na(ReleaseConfirmedDate) & !is.na(Tank_Removal_Date), 1, 0)
)]

# 7. Aggregation & Harmonization ------------------------------------------

# FIX: consolidate unknown_walled/missing_walled PRIOR to aggregation to avoid safe_max error
facility_tank_data_LUST[, unknown_consolidated := pmax(unknown_walled, missing_walled, na.rm=TRUE)]

AR_UST_tanks_SD <- facility_tank_data_LUST[, .(
  facility_name = first(facility_name),
  tank_status = first(tank_status),
  
  # Geography
  latitude = first(latitude),
  longitude = first(longitude),
  address = first(address),
  city = first(city),
  zip = first(zip),
  
  # Leak Flags
  leak_after_closure = ifelse(sum(leak_after_closure, na.rm = TRUE) > 0, 1, 0),
  leak_before_NFA_before_closure = ifelse(sum(leak_before_NFA_before_closure, na.rm = TRUE) > 0, 1, 0),
  leak_before_NFA_after_closure = ifelse(sum(leak_before_NFA_after_closure, na.rm = TRUE) > 0, 1, 0),
  no_leak = ifelse(sum(no_leak, na.rm = TRUE) > 0, 1, 0),
  
  capacity = first(capacity),
  
  # Construction (Using safe_max to handle potential all-NA groups)
  single_walled = safe_max(single_walled, na.rm = TRUE),
  double_walled = safe_max(double_walled, na.rm = TRUE),
  unknown_walled = safe_max(unknown_consolidated, na.rm = TRUE), # FIXED: using pre-calc column
  
  # Substances (Using safe_max)
  is_gasoline = safe_max(is_gasoline, na.rm = TRUE),
  is_diesel = safe_max(is_diesel, na.rm = TRUE),
  is_oil_kerosene = safe_max(is_oil_kerosene, na.rm = TRUE),
  is_jet_fuel = safe_max(is_jet_fuel, na.rm = TRUE),
  is_other = safe_max(is_other, na.rm = TRUE)
), by = .(Facility_ID, Tank_ID, Tank_Installation_Date, Tank_Removal_Date, county_name)]

# Standardize Columns
setnames(AR_UST_tanks_SD, 
         c("Facility_ID", "Tank_ID", "Tank_Installation_Date", "Tank_Removal_Date"),
         c("facility_id", "tank_id", "tank_installed_date", "tank_closed_date"))

AR_UST_tanks_SD[, state := "AR"]
AR_UST_tanks_SD[, facility_name := stringr::str_to_title(trimws(gsub("[^[:alnum:] ]", "", facility_name)))]
AR_UST_tanks_SD[, facility_id := as.character(facility_id)]
AR_UST_tanks_SD[, tank_id := as.character(tank_id)]

# Merge FIPS (if available)
fips_path <- here("Data", "Processed", "county_fips_codes.csv")
if(file.exists(fips_path)) {
  county_fips <- fread(fips_path)
  county_fips[, standardized_county := standardize_county_name(county_name)]
  # Filter for Arkansas (State FIPS 05)
  AR_UST_tanks_SD[, standardized_county := standardize_county_name(county_name)]
  AR_UST_tanks_SD <- merge(AR_UST_tanks_SD, county_fips[state_fips==5], by="standardized_county", all.x=TRUE)
  AR_UST_tanks_SD[, standardized_county := NULL]
  # Rename join collision if necessary
  if("county_name.x" %in% names(AR_UST_tanks_SD)) setnames(AR_UST_tanks_SD, "county_name.x", "county_name")
}

# LUST Harmonization
AR_LUST_SD <- ust_lust_table[, .(
  facility_id = as.character(Facility_ID),
  LUST_id = as.character(LUST_ID),
  report_date = ReleaseConfirmedDate,
  nfa_date = NFAIssuedDate,
  state = "AR"
)]

# Final Column Selection (Strict match to LA script)
required_columns <- c(
  "facility_id", "facility_name", "tank_id", "state", "tank_installed_date", "tank_closed_date", "tank_status",
  "leak_after_closure", "leak_before_NFA_before_closure", "leak_before_NFA_after_closure", "no_leak",
  "capacity", "single_walled", "double_walled", "unknown_walled",
  "is_gasoline", "is_diesel", "is_oil_kerosene", "is_jet_fuel", "is_other", 
  "county_name", "county_fips", "latitude", "longitude"
)

cols_to_keep <- intersect(names(AR_UST_tanks_SD), required_columns)
AR_UST_tanks_SD <- AR_UST_tanks_SD[, ..cols_to_keep]

# 8. Save -----------------------------------------------------------------
output_dir <- here("Data","Raw","state_databases","Arkansas")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

fwrite(AR_UST_tanks_SD, file.path(output_dir, "AR_Harmonized_UST_tanks.csv"))
fwrite(AR_LUST_SD, file.path(output_dir, "AR_Harmonized_LUST.csv"))