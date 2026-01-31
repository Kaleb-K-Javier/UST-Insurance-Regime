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

## Louisiana UST Dataset Creation
# Script: 02_Clean_LA.R
# Author: Validated Classification Logic (Updated Jan 2026)
# Description: Harmonizes LA UST/LUST data for econometrics.
#
# Key Features:
#   1. Robust Import: Forces text import to capture "Y" flags in sparse columns.
#   2. Regulatory Mandate (Double): Implements "Dec 20, 2008" cutoff (LAC 33:XI.303.C.1).
#   3. Upgrade Logic (Single): Uses 'initial_cp_upgrade_date' as proof of Single-Walled status (40 CFR 280.21).
#   4. Expanded Definitions: Poly Jackets & Excavation Liners = Double Wall (EPA Def).
#   5. Robustness: Uses vectorized operations to prevent crashes on large datasets.

# 0. Setup ----------------------------------------------------------------
COUNTYVARIABLE <- "PARISH_DESC"

library(tidyverse)
library(data.table)
library(here)
library(lubridate)
library(janitor)
library(readxl)
library(stringr)

# 1. Helper Functions -----------------------------------------------------

# Vectorized function to standardize parish/county names
standardize_county_name <- function(county_name) {
  # tolower() is already vectorized
  name <- tolower(county_name)
  
  # gsub() is also vectorized
  name <- gsub(" county$| counties$| parish$| parishes$", "", name)
  name <- gsub("[^a-z ]", "", name)
  name <- trimws(gsub("\\s+", " ", name))
  
  # Vectorized way to handle empty strings
  name[name == ""] <- NA
  
  return(name)
}

# Robust Date Parser for Mixed Excel/Text formats
safe_parse_date <- function(date_vec) {
  # 1. Try parsing as numeric Excel serial date
  suppressWarnings({
    numeric_dates <- as.numeric(date_vec)
    parsed_dates <- janitor::excel_numeric_to_date(numeric_dates)
  })
  
  # 2. Identify failures (values that were not NA originally, but failed numeric parse)
  needs_string_parse <- is.na(parsed_dates) & !is.na(date_vec)
  
  # 3. Try string parsing for the remainder
  if(any(needs_string_parse)) {
    string_dates <- lubridate::parse_date_time(
      date_vec[needs_string_parse], 
      orders = c("ymd", "mdy", "dmy", "Y-m-d", "m/d/Y")
    )
    parsed_dates[needs_string_parse] <- as.Date(string_dates)
  }
  
  return(parsed_dates)
}

# 2. Core Logic: Classification -------------------------------------------

# Function to classify tank wall types (Robust Version with Upgrade Logic)
classify_tank_walls <- function(data) {
  result <- copy(data)
  
  # --- REGULATORY CONSTANTS ---
  # Citation: LAC 33:XI.303.C.1
  # "All new UST systems installed after December 20, 2008, shall have secondary containment."
  LA_SECONDARY_CONTAINMENT_DATE <- as.Date("2008-12-20") 
  
  # --- DEFINITION OF COLUMN GROUPS ---
  
  # Group A: Secondary Containment Indicators (Double Walled)
  # Citation: 40 CFR 280.20(d) & LAC 33:XI.303.C
  # - mct_poly_jacket: Meets EPA def of secondary containment (jacket creates interstitial space).
  # - mct_excavat_liner: Functional secondary containment if monitored.
  cols_double <- c("mct_double_walled", "mct_poly_jacket", "mct_excavat_liner")
  cols_double <- intersect(cols_double, names(result))
  
  # Group B: Single Wall / Material Indicators
  # Citation: 40 CFR 280.20(a) & (b)
  cols_materials <- c(
    "mct_asphalt_steel", "mct_fibrgls_plastic", "mct_comp_stl_fbrgls", 
    "mct_cathod_prot_stl", "mct_epox_coat_stl", "mct_lined_interior",
    "mct_stlasphalt_wocp", "mct_stlasphalt_nocp", "mct_cpsteel_ic", 
    "mct_cpsteel_sac_anode", "mct_cpsteel_int_line"
  )
  cols_materials <- intersect(cols_materials, names(result))
  
  if(length(cols_double) == 0 && length(cols_materials) == 0) {
    warning("Critical Warning: No construction columns found!")
    result[, `:=`(double_walled=0, single_walled=0, unknown_walled=1, missing_walled=1)]
    return(result)
  }

  # --- HELPER FUNCTION: Safely Check "Any Y" ---
  check_any_y <- function(dt, cols) {
    if(length(cols) == 0) return(rep(FALSE, nrow(dt)))
    # Reduce/lapply is safer than rowSums for data.table columns
    Reduce("|", lapply(dt[, ..cols], function(x) x %in% c("Y", "y", "Yes")))
  }

  # --- CALCULATE FLAGS ---
  
  # Flag A: Explicit Secondary Containment
  result[, explicit_double := check_any_y(result, cols_double)]
  
  # Flag B: Regulatory Mandate (The "2008 Rule")
  # Citation: LAC 33:XI.303.C.1
  result[, regulatory_double := !is.na(tank_installed_date) & tank_installed_date > LA_SECONDARY_CONTAINMENT_DATE]
  
  # Flag C: Material Presence (Evidence of Single Wall)
  # This now includes specific material flags OR the presence of an upgrade date
  result[, has_material_flag := check_any_y(result, cols_materials)]
  
  # Flag D: CP Upgrade Logic (The "Single Wall" Proof)
  # Citation: 40 CFR 280.21 - Upgrades apply to existing Single-Walled tanks.
  # If a tank has a CP upgrade date, it MUST be single-walled (originally).
  result[, has_upgrade_evidence := !is.na(initial_cp_upgrade_date)]
  
  # Combined Single Wall Evidence
  result[, evidence_of_existence := has_material_flag | has_upgrade_evidence]

  # --- HIERARCHICAL ASSIGNMENT ---
  result[, `:=`(
    # Priority 1: Double/Secondary if Explicitly marked OR Regulatory mandate
    # (Overrides upgrade evidence because a tank could be replaced post-2008)
    double_walled = as.integer(explicit_double | regulatory_double),
    
    # Priority 2: Single if NOT Double AND we have explicit Material OR Upgrade Data
    single_walled = as.integer(!(explicit_double | regulatory_double) & evidence_of_existence),
    
    # Priority 3: Unknown if neither
    unknown_walled = as.integer(!(explicit_double | regulatory_double) & !evidence_of_existence),
    missing_walled = as.integer(!(explicit_double | regulatory_double) & !evidence_of_existence)
  )]
  
  # Clean up temp cols
  result[, `:=`(explicit_double=NULL, regulatory_double=NULL, has_material_flag=NULL, has_upgrade_evidence=NULL, evidence_of_existence=NULL)]
  
  cat("\nTank wall classification summary:\n")
  print(result[, .(total=.N, single=sum(single_walled), double=sum(double_walled), unknown=sum(unknown_walled))])
  
  return(result)
}

# Function to classify substances
classify_substances <- function(data, substance_col_name = "substance") {
  result <- copy(data)
  result[, `:=`(is_gasoline=0, is_diesel=0, is_oil_kerosene=0, is_jet_fuel=0, is_other=1)]
  
  if(substance_col_name %in% names(result)) {
    result[, sub_lower := tolower(get(substance_col_name))]
    
    # Detailed regex patterns
    gas_pat <- paste(c("gas", "gasolin", "petrol", "motor fuel", "fuel oil", "e10", "e15", "e85", "e\\d+", "unleaded", "premium", "regular", "petroleum", "fuel", "conv[[:alpha:]]+", "mid", "plus", "supreme", "super", "octane", "87", "89", "91", "93", "\\bmtbe\\b"), collapse = "|")
    dsl_pat <- paste(c("diesel", "dsl", "dl$", "\\bd\\b", "bio.?diesel", "red diesel", "off.?road diesel", "^d ", "\\bdl\\b", "ulsd", "\\bd[0-9]\\b"), collapse = "|")
    oil_pat <- paste(c("oil", "kerosene", "kerosine", "heating oil", "motor oil", "used oil", "waste oil", "lubricant", "lube", "heating", "hydraulic", "transmission", "crude", "heavy", "mineral", "vegetable", "\\bcrank"), collapse = "|")
    jet_pat <- paste(c("jet", "aviation", "av gas", "avgas", "jp.?[45678]", "aircraft", "airplane"), collapse = "|")
    
    # Apply patterns
    result[!is.na(sub_lower), is_gasoline := as.integer(grepl(gas_pat, sub_lower, perl=TRUE))]
    result[!is.na(sub_lower) & !grepl("def|diesel exhaust fluid", sub_lower, perl=TRUE), is_diesel := as.integer(grepl(dsl_pat, sub_lower, perl=TRUE))]
    result[!is.na(sub_lower), is_oil_kerosene := as.integer(grepl(oil_pat, sub_lower, perl=TRUE))]
    result[!is.na(sub_lower), is_jet_fuel := as.integer(grepl(jet_pat, sub_lower, perl=TRUE))]
    
    # Handle "Other"
    result[, is_other := as.integer(is_gasoline + is_diesel + is_oil_kerosene + is_jet_fuel == 0)]
    result[, sub_lower := NULL]
  }
  
  cat("\nSubstance classification summary:\n")
  print(result[, .(total=.N, gas=sum(is_gasoline), diesel=sum(is_diesel), oil=sum(is_oil_kerosene), jet=sum(is_jet_fuel), other=sum(is_other))])
  
  return(result)
}

# 3. Load Raw Data (CRITICAL: Force Text Import) --------------------------
cat("\nLoading Louisiana UST and LUST data (Forcing Text Import)...\n")
la_file_path <- here("Data", "Raw", "state_databases", "Louisiana", "LA_record_request.xlsx")

if (!file.exists(la_file_path)) stop("File not found! Check path.")

# FIX: col_types = "text" prevents R from dropping "Y" flags in sparse columns.
LA_tanks_raw <- read_excel(la_file_path, sheet = "Tank Info by compartment", col_types = "text") %>%
  janitor::clean_names() %>%
  as.data.table()

LA_lust_raw <- read_excel(la_file_path, sheet = "LUST Sites by incident ID", col_types = "text") %>%
  janitor::clean_names() %>%
  as.data.table()

cat("Loaded", nrow(LA_tanks_raw), "tanks and", nrow(LA_lust_raw), "LUSTs.\n")

# 4. Process Tank Data ----------------------------------------------------
cat("\nProcessing tanks...\n")

# Select columns, including 'initial_cp_upgrade_date'
LA_UST_tanks <- LA_tanks_raw %>%
  select(
    master_ai_id, master_ai_name, parish_desc, subject_item_id,
    tank_capacity, install_date, tank_substance_code,
    current_status_code, current_status_desc, current_status_start_date,
    initial_cp_upgrade_date, # ADDED: Critical for Single Wall ID
    x_coord_standard_value, # ADDED: Longitude
    y_coord_standard_value, # ADDED: Latitude
    starts_with("mct_") 
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
    tank_status_date = current_status_start_date,
    longitude = x_coord_standard_value, # Harmonized Name
    latitude = y_coord_standard_value   # Harmonized Name
  ) %>%
  as.data.table()

# Clean Capacity
LA_UST_tanks[, capacity := as.numeric(gsub("[^0-9.]", "", tank_capacity))]
LA_UST_tanks[, latitude := as.numeric(latitude)]   # Clean Text to Number
LA_UST_tanks[, longitude := as.numeric(longitude)] # Clean Text to Number

# Clean Dates (Safe Parse handles Excel Serials + Strings)
LA_UST_tanks[, tank_installed_date := safe_parse_date(tank_installed_date)]
LA_UST_tanks[, tank_status_date := safe_parse_date(tank_status_date)]
# ADDED: Parse Upgrade Date
LA_UST_tanks[, initial_cp_upgrade_date := safe_parse_date(initial_cp_upgrade_date)]

# Tank Status Logic
LA_UST_tanks[, tank_status := case_when(
  status_code == "A" ~ "Open",
  status_code %in% c("C", "C1", "I", "O", "R", "R1", "R2") ~ "Closed",
  status_code == "T" ~ "Temporary",
  TRUE ~ NA_character_
)]
LA_UST_tanks <- LA_UST_tanks[tank_status != "Temporary"]



# Determine Closure Date
LA_UST_tanks[, tank_closed_date := NA_Date_]
LA_UST_tanks[tank_status == "Closed", tank_closed_date := tank_status_date]

# Run Classifications
LA_UST_tanks <- classify_tank_walls(LA_UST_tanks)
LA_UST_tanks <- classify_substances(LA_UST_tanks, "substance")

# 5. Process LUST Data ----------------------------------------------------
cat("\nProcessing LUSTs...\n")
LA_LUST_SD <- LA_lust_raw %>%
  select(ai_number, incident_id, conf_rel_date, nfa_date) %>%
  rename(facility_id = ai_number, LUST_id = incident_id, report_date = conf_rel_date) %>%
  as.data.table()

LA_LUST_SD[, `:=`(
  report_date = safe_parse_date(report_date),
  nfa_date = safe_parse_date(nfa_date),
  state = "LA"
)]

# 6. Merge & Geography ----------------------------------------------------
cat("\nMerging and Geocoding...\n")
LA_UST_tanks[, facility_id := as.character(facility_id)]
LA_LUST_SD[, facility_id := as.character(facility_id)]

# Merge Tank and LUST data
LA_UST_tanks_LUST <- merge(LA_UST_tanks, LA_LUST_SD, by="facility_id", all.x=TRUE, allow.cartesian=TRUE)

# Create Leak Flags based on temporal overlap
LA_UST_tanks_LUST[, `:=`(
  leak_after_closure = as.integer(!is.na(tank_closed_date) & !is.na(report_date) & report_date > tank_closed_date),
  leak_before_NFA_before_closure = as.integer(!is.na(tank_closed_date) & !is.na(report_date) & report_date < tank_closed_date & (is.na(nfa_date) | nfa_date < tank_closed_date)),
  leak_before_NFA_after_closure = as.integer(!is.na(tank_closed_date) & !is.na(report_date) & report_date < tank_closed_date & !is.na(nfa_date) & nfa_date > tank_closed_date),
  no_leak = as.integer(is.na(report_date) & !is.na(tank_closed_date))
)]

# Aggregate to Facility-Tank Level
LA_UST_tanks_SD <- LA_UST_tanks_LUST[, .(
  facility_name = first(facility_name),
  tank_status = first(tank_status),
  leak_after_closure = max(leak_after_closure, na.rm=T),
  leak_before_NFA_before_closure = max(leak_before_NFA_before_closure, na.rm=T),
  leak_before_NFA_after_closure = max(leak_before_NFA_after_closure, na.rm=T),
  no_leak = max(no_leak, na.rm=T),
  
  # Wall Types
  single_walled = max(single_walled, na.rm=T),
  double_walled = max(double_walled, na.rm=T),
  unknown_walled = max(unknown_walled, missing_walled, na.rm=T),
  
  # Substances
  is_gasoline = max(is_gasoline, na.rm=T),
  is_diesel = max(is_diesel, na.rm=T),
  is_oil_kerosene = max(is_oil_kerosene, na.rm=T),
  is_jet_fuel = max(is_jet_fuel, na.rm=T),
  is_other = max(is_other, na.rm=T),
  
  capacity = mean(capacity, na.rm=T),
  latitude = first(latitude, na_rm=TRUE),   # ADDED
  longitude = first(longitude, na_rm=TRUE)  # ADDED
), by = .(facility_id, tank_id, tank_installed_date, tank_closed_date, county_name)]

# Geography: Standardize and Merge FIPS
LA_UST_tanks_SD[, standardized_county := standardize_county_name(county_name)]
fips_path <- here("Data", "Processed", "county_fips_codes.csv")

if(file.exists(fips_path)) {
  county_fips <- fread(fips_path)
  county_fips[, standardized_county := standardize_county_name(county_name)]
  
  # Filter for Louisiana (State FIPS 22)
  LA_UST_tanks_SD <- merge(LA_UST_tanks_SD, county_fips[state_fips==22], by="standardized_county", all.x=TRUE)
  LA_UST_tanks_SD[, standardized_county := NULL]
}

LA_UST_tanks_SD[, state := "LA"]

# Final Column Cleanup
required_columns <- c(
  "facility_id", "facility_name", "tank_id", "state", "tank_installed_date", "tank_closed_date", "tank_status",
  "leak_after_closure", "leak_before_NFA_before_closure", "leak_before_NFA_after_closure", "no_leak",
  "capacity", "single_walled", "double_walled", "unknown_walled",
  "is_gasoline", "is_diesel", "is_oil_kerosene", "is_jet_fuel", "is_other", "county_name", "county_fips",
  "latitude", "longitude" # ADDED
)
cols_to_keep <- intersect(names(LA_UST_tanks_SD), required_columns)
LA_UST_tanks_SD <- LA_UST_tanks_SD[, ..cols_to_keep]

# 7. Save -----------------------------------------------------------------
cat("\nSaving Final Files...\n")
output_dir <- here("Data", "Raw", "state_databases", "Louisiana")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

fwrite(LA_UST_tanks_SD, file.path(output_dir, "LA_Harmonized_UST_tanks.csv"))
fwrite(LA_LUST_SD, file.path(output_dir, "LA_Harmonized_LUST.csv"))

cat("Done.\n")
cat("Tank records:", nrow(LA_UST_tanks_SD), "\n")
cat("LUST records:", nrow(LA_LUST_SD), "\n")

## Maine UST Dataset Creation
# Script: 02_Clean_ME.R
# Author: Validated Classification Logic (Updated Jan 2026)
# Description: Harmonizes Maine UST/LUST data for econometrics.
#
# Key Features:
#   1. Robust Import: Uses fread(colClasses="character") to prevent data loss.
#   2. LUST Integrity: Uses strict delimiter counting (no 'fill' guessing) to handle malformed rows.
#   3. Specific Logic: Classifications based on explicit ME material labels.
#   4. Filtering: Restricts to Federally Regulated + Belowground tanks.
#   5. Geography: Robust Lat/Long handling (NA_real_) and FIPS merging.

# 0. Setup ----------------------------------------------------------------
COUNTYVARIABLE <- "county"

library(tidyverse)
library(data.table)
library(here)
library(lubridate)
library(janitor)
library(stringr)

# 1. Helper Functions -----------------------------------------------------

standardize_county_name <- function(county_name) {
  name <- tolower(county_name)
  name <- gsub(" county$| counties$| parish$| parishes$", "", name)
  name <- gsub("[^a-z ]", "", name)
  name <- trimws(gsub("\\s+", " ", name))
  name[name == ""] <- NA
  return(name)
}

# 2. Classification Logic -------------------------------------------------

classify_tank_walls_ME <- function(data) {
  result <- copy(data)
  
  # 1. Identify Double Walled (Secondary Containment)
  result[, is_double := grepl("DOUBLE[- ]?WALL|SEC(ONDARY)?\\s+CONTAIN|JACKETED", 
                              toupper(tank_structure), perl = TRUE)]
  
  # 2. Identify Single Walled
  result[, is_single := !is_double & grepl("SINGLE[- ]?WALL|BARE|ASPHALT|CATHODIC", 
                                           toupper(tank_structure), perl = TRUE)]
  
  # 3. Assign Harmonized Classes
  result[, `:=`(
    double_walled = as.integer(is_double),
    single_walled = as.integer(is_single),
    unknown_walled = as.integer(!is_double & !is_single),
    missing_walled = as.integer(is.na(tank_structure) | tank_structure == "")
  )]
  
  result[, `:=`(is_double = NULL, is_single = NULL)]
  return(result)
}

classify_substances <- function(data, substance_col_name = "substance") {
  result <- copy(data)
  result[, `:=`(is_gasoline=0, is_diesel=0, is_oil_kerosene=0, is_jet_fuel=0, is_other=1)]
  
  if(substance_col_name %in% names(result)) {
    result[, sub_lower := tolower(get(substance_col_name))]
    
    gas_pat <- "gasoline|petrol|unleaded|premium|regular|gasohol"
    dsl_pat <- "diesel|bio.?diesel"
    oil_pat <- "oil|kerosene|lube|waste|heating"
    jet_pat <- "jet|aviation|jp"
    
    result[grepl(gas_pat, sub_lower), is_gasoline := 1]
    result[grepl(dsl_pat, sub_lower) & !grepl("def", sub_lower), is_diesel := 1]
    result[grepl(oil_pat, sub_lower), is_oil_kerosene := 1]
    result[grepl(jet_pat, sub_lower), is_jet_fuel := 1]
    
    result[is_gasoline==1 | is_diesel==1 | is_oil_kerosene==1 | is_jet_fuel==1, is_other := 0]
    result[, sub_lower := NULL]
  }
  return(result)
}

# 3. Load Data (Robust Method) --------------------------------------------
cat("\nDownloading and Loading Maine UST Data...\n")

temp_file <- tempfile()
download.file("https://www.maine.gov/dep/ftp/tanks/tanks_owner_all_registered_tanks.txt", destfile = temp_file, mode = "wb")

# Force colClasses = "character" to prevent ANY data loss during import
ME_tanks_raw <- fread(temp_file, sep = "*", header = TRUE, fill = TRUE, 
                      colClasses = "character", quote = "") %>%
  janitor::clean_names() %>%
  as.data.table()

unlink(temp_file)

cat("Loaded", nrow(ME_tanks_raw), "tank records.\n")

# 4. Filter Data (Critical Step) ------------------------------------------
cat("\nApplying Filters...\n")

# 1. Belowground Only
ME_tanks_raw <- ME_tanks_raw[toupper(tank_above_below) == "BELOWGROUND"]

# 2. Federally Regulated Only
ME_tanks_raw <- ME_tanks_raw[toupper(fed_regulated) %in% c("Y", "YES")]

cat("Filtered to", nrow(ME_tanks_raw), "Federally Regulated USTs.\n")

# 5. Process Tank Data ----------------------------------------------------
cat("\nProcessing Tank Attributes...\n")

# Map Raw Names to Standard Names
ME_UST_tanks <- ME_tanks_raw[, .(
  facility_id = registration_number,
  facility_name = facility_name, 
  tank_id = tank_number,
  tank_installed_date = date_tank_installed,
  tank_closed_date = tank_status_date,
  capacity = tank_volume_in_gallons,
  substance = product_stored,
  tank_structure = tank_material_label,
  status = tank_status_label,
  facility_city,
  latitude,
  longitude
)]

# --- ROBUST LAT/LONG HANDLING ---
if (!"latitude" %in% names(ME_UST_tanks)) ME_UST_tanks[, latitude := NA_real_]
if (!"longitude" %in% names(ME_UST_tanks)) ME_UST_tanks[, longitude := NA_real_]

ME_UST_tanks[, latitude := as.numeric(latitude)]
ME_UST_tanks[, longitude := as.numeric(longitude)]

# Standardize Dates
ME_UST_tanks[, tank_installed_date := lubridate::mdy(tank_installed_date, quiet = TRUE)]
ME_UST_tanks[, tank_closed_date := lubridate::mdy(tank_closed_date, quiet = TRUE)]

# Tank Status Logic
ME_UST_tanks[, tank_status := "Closed"] # Default
ME_UST_tanks[toupper(status) == "ACTIVE", tank_status := "Open"]
ME_UST_tanks[toupper(status) == "PLANNED FOR INSTALLATION", tank_status := "Temporary"]
ME_UST_tanks <- ME_UST_tanks[tank_status != "Temporary"]

# Clean Numerics
ME_UST_tanks[, capacity := as.numeric(gsub("[^0-9.]", "", capacity))]

# Run Classifications
ME_UST_tanks <- classify_tank_walls_ME(ME_UST_tanks)
ME_UST_tanks <- classify_substances(ME_UST_tanks)

# 6. Process LUST Data (Strict Integrity Mode) ----------------------------
cat("\nProcessing LUST Data...\n")

# Function: STRICT Delimiter Checking (No filling/guessing)
safe_fread_strict <- function(url) {
  tf <- tempfile()
  download.file(url, destfile = tf, mode = "wb", quiet = TRUE)
  
  # Read as raw text lines
  all_lines <- readLines(tf, warn = FALSE)
  
  # Count separators ('*') in each line
  sep_counts <- stringr::str_count(all_lines, "\\*")
  
  # Determine the 'Mode' (most common) number of separators
  # This assumes the file is mostly correct
  expected_seps <- as.numeric(names(sort(table(sep_counts), decreasing=TRUE)[1]))
  
  # Filter lines: Keep only those with the expected delimiter count
  clean_lines <- all_lines[sep_counts == expected_seps]
  
  # Report dropped lines
  dropped <- length(all_lines) - length(clean_lines)
  if(dropped > 0) {
    cat(sprintf("  Dropped %d malformed rows from %s (Delimiter mismatch)\n", dropped, basename(url)))
  }
  
  # Parse the clean data
  # header=FALSE because Maine LUST files usually don't have headers
  dt <- fread(text = paste(clean_lines, collapse = "\n"), 
              sep = "*", header = FALSE, colClasses = "character", quote = "")
  
  unlink(tf)
  return(dt)
}

base_url <- "https://www.maine.gov/dep/ftp/hoss/"
tank_map      <- safe_fread_strict(paste0(base_url, "report_tank_involved.txt"))
spill_report  <- safe_fread_strict(paste0(base_url, "spill_report.txt"))
spill_log     <- safe_fread_strict(paste0(base_url, "spill_log.txt"))

# Filter for Final Reports (Col V7)
spill_final <- spill_report[V7 %in% c("Final Report", "FR")]

# Chain Merge
# Tank Map: V1=Spill, V6=FacilityID
# Spill Log: V1=Spill, V16=Report Date
# Spill Report: V1=Spill, V4=NFA Date
lust_merged <- merge(tank_map[, .(l_spill_number=V1, facility_id=V6)], 
                     spill_log[, .(l_spill_number=V1, report_date=V16)], 
                     by="l_spill_number", all.x=TRUE)

lust_merged <- merge(lust_merged, 
                     spill_final[, .(l_spill_number=V1, nfa_date=V4)], 
                     by="l_spill_number", all.x=TRUE)

# Final LUST Table
ME_LUST_SD <- lust_merged[, .(
  facility_id = facility_id,
  LUST_id = l_spill_number,
  report_date = report_date,
  nfa_date = nfa_date,
  state = "ME"
)]

# Robust Date Parsing for LUST
ME_LUST_SD[, report_date := lubridate::parse_date_time(report_date, orders = c("mdy", "ymd", "mdy HMS"), quiet = TRUE)]
ME_LUST_SD[, nfa_date := lubridate::parse_date_time(nfa_date, orders = c("mdy", "ymd", "mdy HMS"), quiet = TRUE)]

ME_LUST_SD[, report_date := as.Date(report_date)]
ME_LUST_SD[, nfa_date := as.Date(nfa_date)]

# 7. Merge & Aggregation --------------------------------------------------
cat("\nMerging and Aggregating...\n")

ME_UST_tanks[, facility_id := as.character(facility_id)]
ME_LUST_SD[, facility_id := as.character(facility_id)]

ME_UST_tanks_LUST <- merge(ME_UST_tanks, ME_LUST_SD, by="facility_id", all.x=TRUE, allow.cartesian=TRUE)

# Leak Flags
ME_UST_tanks_LUST[, `:=`(
  leak_after_closure = as.integer(!is.na(tank_closed_date) & !is.na(report_date) & report_date > tank_closed_date),
  leak_before_NFA_before_closure = as.integer(!is.na(tank_closed_date) & !is.na(report_date) & report_date < tank_closed_date & (is.na(nfa_date) | nfa_date < tank_closed_date)),
  leak_before_NFA_after_closure = as.integer(!is.na(tank_closed_date) & !is.na(report_date) & report_date < tank_closed_date & !is.na(nfa_date) & nfa_date > tank_closed_date),
  no_leak = as.integer(is.na(report_date) & !is.na(tank_closed_date))
)]

# Aggregate
ME_UST_tanks_SD <- ME_UST_tanks_LUST[, .(
  facility_name = first(facility_name),
  tank_status = first(tank_status),
  leak_after_closure = max(leak_after_closure, na.rm=T),
  leak_before_NFA_before_closure = max(leak_before_NFA_before_closure, na.rm=T),
  leak_before_NFA_after_closure = max(leak_before_NFA_after_closure, na.rm=T),
  no_leak = max(no_leak, na.rm=T),
  
  single_walled = max(single_walled, na.rm=T),
  double_walled = max(double_walled, na.rm=T),
  unknown_walled = max(unknown_walled, missing_walled, na.rm=T),
  
  is_gasoline = max(is_gasoline, na.rm=T),
  is_diesel = max(is_diesel, na.rm=T),
  is_oil_kerosene = max(is_oil_kerosene, na.rm=T),
  is_jet_fuel = max(is_jet_fuel, na.rm=T),
  is_other = max(is_other, na.rm=T),
  
  capacity = mean(capacity, na.rm=T),
  latitude = first(latitude),   
  longitude = first(longitude)  
), by = .(facility_id, tank_id, tank_installed_date, tank_closed_date)]

# 8. Geography ------------------------------------------------------------
cat("\nAdding Geography...\n")

census_file <- Sys.glob(here("Data", "Processed", "Census_Geography", "Maine_census_geography.csv"))

if(length(census_file) > 0) {
  census_data <- fread(census_file[1])
  if("facility_id" %in% names(census_data)) {
    census_data[, facility_id := as.character(facility_id)]
    census_data[, facility_id_clean := gsub("ME", "", facility_id)]
    
    ME_UST_tanks_SD <- merge(ME_UST_tanks_SD, census_data[, .(facility_id_clean, county_name, county_geoid)], 
                             by.x="facility_id", by.y="facility_id_clean", all.x=TRUE)
    setnames(ME_UST_tanks_SD, "county_geoid", "county_fips")
  }
} else {
  warning("Maine Census Geography file not found. County data will be missing.")
  ME_UST_tanks_SD[, county_name := NA]
  ME_UST_tanks_SD[, county_fips := NA]
}

ME_UST_tanks_SD[, state := "ME"]

# Final Column Selection
required_columns <- c(
  "facility_id", "facility_name", "tank_id", "state", "tank_installed_date", "tank_closed_date", "tank_status",
  "leak_after_closure", "leak_before_NFA_before_closure", "leak_before_NFA_after_closure", "no_leak",
  "capacity", "single_walled", "double_walled", "unknown_walled",
  "is_gasoline", "is_diesel", "is_oil_kerosene", "is_jet_fuel", "is_other", "county_name", "county_fips",
  "latitude", "longitude"
)
cols_to_keep <- intersect(names(ME_UST_tanks_SD), required_columns)
ME_UST_tanks_SD <- ME_UST_tanks_SD[, ..cols_to_keep]

# 9. Save -----------------------------------------------------------------
cat("\nSaving Final Files...\n")
output_dir <- here("Data", "Raw", "state_databases", "Maine")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

fwrite(ME_UST_tanks_SD, file.path(output_dir, "ME_Harmonized_UST_tanks.csv"))
fwrite(ME_LUST_SD, file.path(output_dir, "ME_Harmonized_LUST.csv"))

cat("Done.\n")
cat("Tank records:", nrow(ME_UST_tanks_SD), "\n")
cat("LUST records:", nrow(ME_LUST_SD), "\n")


## Michigan UST Dataset Creation
# Script: 02_Clean_MI.R
# Author: Validated Classification Logic (Updated Jan 2026)
# Description: Harmonizes Michigan UST/LUST data.
# Key Features:
#   1. Robust Import: Single-pass sheet reading with Base R header construction (Fixes dplyr crash).
#   2. Specific Logic: Classifies comma-separated construction descriptions.
#   3. Date Parsing: Strictly uses mdy() for Michigan's MM/DD/YYYY format.
#   4. Harmonization: Standardizes columns to match LA/ME schema.

# 0. Setup ----------------------------------------------------------------
COUNTYVARIABLE <- "county_name"

library(tidyverse)
library(data.table)
library(here)
library(lubridate)
library(janitor)
library(readxl)
library(stringr)
library(purrr)

# 1. Helper Functions -----------------------------------------------------

standardize_county_name <- function(county_name) {
  name <- tolower(county_name)
  name <- gsub(" county$| counties$| parish$| parishes$", "", name)
  name <- gsub("[^a-z ]", "", name)
  name <- trimws(gsub("\\s+", " ", name))
  name[name == ""] <- NA
  return(name)
}

safe_parse_date <- function(date_vec) {
  # Michigan uses MM/DD/YYYY
  suppressWarnings({
    parsed_dates <- lubridate::mdy(date_vec, quiet = TRUE)
  })
  return(as.Date(parsed_dates))
}

# 2. Classification Logic -------------------------------------------------

classify_tank_walls_MI <- function(data, tank_wall_col = "tank_wall_type") {
  result <- copy(data)
  
  # Normalize text
  vals <- toupper(result[[tank_wall_col]])
  
  # 1. Identify Double Walled (Secondary Containment)
  result[, is_double := grepl("DOUBLE WALLED|TANK JACKET|POLYETHYLENE TANK JACKET|EXCAVATION LINER|INTERSTITIAL", 
                              vals, perl = TRUE)]
  
  # 2. Identify Single Walled
  result[, is_single := !is_double & grepl("STEEL|FIBERGLASS|FRP|COMPOSITE|COATED|BARE|GALVANIZED|CONCRETE", 
                                           vals, perl = TRUE)]
  
  # 3. Assign Harmonized Classes
  result[, `:=`(
    double_walled = as.integer(is_double),
    single_walled = as.integer(is_single),
    unknown_walled = as.integer(!is_double & !is_single),
    missing_walled = as.integer(is.na(vals) | vals == "")
  )]
  
  result[, `:=`(is_double = NULL, is_single = NULL)]
  
  cat("\nClassification Summary:\n")
  print(result[, .(total=.N, single=sum(single_walled), double=sum(double_walled), unknown=sum(unknown_walled))])
  
  return(result)
}

classify_substances <- function(data, substance_col_name = "substance") {
  result <- copy(data)
  result[, `:=`(is_gasoline=0, is_diesel=0, is_oil_kerosene=0, is_jet_fuel=0, is_other=1)]
  
  if(substance_col_name %in% names(result)) {
    result[, sub_lower := tolower(get(substance_col_name))]
    
    gas_pat <- "gasoline|petrol|unleaded|premium|regular|gasohol|ethanol|e85"
    dsl_pat <- "diesel|bio.?diesel"
    oil_pat <- "oil|kerosene|lube|waste|heating"
    jet_pat <- "jet|aviation|jp"
    
    result[grepl(gas_pat, sub_lower), is_gasoline := 1]
    result[grepl(dsl_pat, sub_lower) & !grepl("def", sub_lower), is_diesel := 1]
    result[grepl(oil_pat, sub_lower), is_oil_kerosene := 1]
    result[grepl(jet_pat, sub_lower), is_jet_fuel := 1]
    
    result[is_gasoline==1 | is_diesel==1 | is_oil_kerosene==1 | is_jet_fuel==1, is_other := 0]
    result[, sub_lower := NULL]
  }
  return(result)
}

# 3. Load Data (Fixed: Robust Base R Header Construction) -----------------
cat("\nLoading Michigan UST Data...\n")

read_michigan_sheets <- function(file_path) {
  
  all_sheets <- excel_sheets(file_path)
  target_sheets <- all_sheets[-c(6, 9)]
  
  process_single_sheet <- function(sheet_name) {
    # 1. Read Headers (First 2 Rows)
    # .name_repair = "unique" prevents 'NA' names error in reading
    headers_df <- read_excel(file_path, sheet = sheet_name, n_max = 2, col_names = FALSE, .name_repair = "unique")
    
    # 2. Read Body (Skip 3 Rows) as Text
    body_df <- read_excel(file_path, sheet = sheet_name, skip = 3, col_names = FALSE, col_types = "text", .name_repair = "unique")
    
    if(nrow(body_df) > 0 && ncol(headers_df) > 0) {
      # Robust Header Construction (Base R apply avoids dplyr strictness)
      new_cols <- apply(headers_df, 2, function(x) paste(na.omit(x), collapse = " "))
      new_cols <- trimws(new_cols)
      
      # Ensure dimensions match (sometimes Excel reads extra empty columns)
      valid_cols <- length(new_cols)
      if(ncol(body_df) >= valid_cols) {
        body_df <- body_df[, 1:valid_cols]
        colnames(body_df) <- new_cols
        return(janitor::clean_names(body_df))
      }
    }
    return(NULL)
  }
  
  purrr::map_dfr(set_names(target_sheets), process_single_sheet, .id = "source_sheet")
}

michigan_file_path <- here("Data", "Raw", "state_databases", "Michigan", "UTK Master List 3-3-25.xlsx")

if (file.exists(michigan_file_path)) {
  michigan_raw <- read_michigan_sheets(michigan_file_path)
  
  # Ensure clean_names didn't create duplicate 'facility_id' if headers were messy
  michigan_raw <- as.data.table(michigan_raw)
  
  cat("Loaded", nrow(michigan_raw), "records.\n")
  
  # 4. Process Tank Data ----------------------------------------------------
  cat("\nProcessing Tank Attributes...\n")
  
  # Map columns
  MI_UST_tanks <- michigan_raw[, .(
    facility_id = facility_id,
    facility_name = facility_name,
    county_name = facility_county,
    tank_id = new_tank_id_number,
    tank_status = details_status,
    tank_capacity = tank_capacity,
    tank_installed_date = tank_instal_date,
    substance = tank_content,
    tank_remove_date = tank_removal_date,
    tank_wall_type = tank_construction
  )]
  
  # Clean IDs
  MI_UST_tanks[, facility_id := as.character(facility_id)]
  MI_UST_tanks[, tank_id := as.character(tank_id)]
  
  # Dates: Strict MDY Parsing
  MI_UST_tanks[, tank_installed_date := safe_parse_date(tank_installed_date)]
  MI_UST_tanks[, tank_closed_date := safe_parse_date(tank_remove_date)]
  
  # Status Logic
  MI_UST_tanks[, tank_status_clean := case_when(
    grepl("Removed|Closed", tank_status, ignore.case=TRUE) ~ "Closed",
    grepl("Active|In Use|Temporarily Out", tank_status, ignore.case=TRUE) ~ "Open",
    TRUE ~ "Closed"
  )]
  
  # Clean Numerics
  MI_UST_tanks[, capacity := as.numeric(gsub("[^0-9.]", "", tank_capacity))]
  
  # Run Classifications
  MI_UST_tanks <- classify_tank_walls_MI(MI_UST_tanks)
  MI_UST_tanks <- classify_substances(MI_UST_tanks)
  
  # 5. Process LUST Data ----------------------------------------------------
  cat("\nProcessing LUST Data...\n")
  
  lust_path <- here("Data", "Raw", "Releases.csv") 
  
  if(file.exists(lust_path)) {
    MI_lust_raw <- fread(lust_path, colClasses = "character") %>% 
      janitor::clean_names() %>%
      filter(state == "Michigan") %>%
      as.data.table()
    
    MI_LUST_SD <- MI_lust_raw[, .(
      facility_id = gsub("^MI", "", facility_id), 
      LUST_id = lust_id,
      report_date = reported_date,
      nfa_date = NA_character_,
      state = "MI"
    )]
    
    # Date Parsing for LUST
    MI_LUST_SD[, report_date := lubridate::mdy_hms(report_date, quiet=TRUE)]
    MI_LUST_SD[is.na(report_date), report_date := lubridate::mdy(report_date, quiet=TRUE)]
    MI_LUST_SD[, report_date := as.Date(report_date)]
    
    # 6. Merge & Aggregation ------------------------------------------------
    cat("\nMerging and Aggregating...\n")
    
    MI_UST_tanks_LUST <- merge(MI_UST_tanks, MI_LUST_SD, by="facility_id", all.x=TRUE, allow.cartesian=TRUE)
    
    MI_UST_tanks_LUST[, `:=`(
      leak_after_closure = as.integer(!is.na(tank_closed_date) & !is.na(report_date) & report_date > tank_closed_date),
      no_leak = as.integer(is.na(report_date) & !is.na(tank_closed_date))
    )]
    
    # --- HARMONIZATION: Initialize Lat/Long if Missing ---
    if (!"latitude" %in% names(MI_UST_tanks_LUST)) MI_UST_tanks_LUST[, latitude := NA_real_]
    if (!"longitude" %in% names(MI_UST_tanks_LUST)) MI_UST_tanks_LUST[, longitude := NA_real_]
    
    MI_UST_tanks_SD <- MI_UST_tanks_LUST[, .(
      facility_name = first(facility_name),
      tank_status = first(tank_status_clean),
      leak_after_closure = max(leak_after_closure, na.rm=T),
      no_leak = max(no_leak, na.rm=T),
      
      single_walled = max(single_walled, na.rm=T),
      double_walled = max(double_walled, na.rm=T),
      unknown_walled = max(unknown_walled, missing_walled, na.rm=T),
      
      is_gasoline = max(is_gasoline, na.rm=T),
      is_diesel = max(is_diesel, na.rm=T),
      is_oil_kerosene = max(is_oil_kerosene, na.rm=T),
      is_jet_fuel = max(is_jet_fuel, na.rm=T),
      is_other = max(is_other, na.rm=T),
      
      capacity = mean(capacity, na.rm=T),
      latitude = first(as.numeric(latitude)),
      longitude = first(as.numeric(longitude))
    ), by = .(facility_id, tank_id, tank_installed_date, tank_closed_date, county_name)]
    
    MI_UST_tanks_SD[, state := "MI"]
    
    # Final Columns Cleanup
    required_columns <- c(
      "facility_id", "facility_name", "tank_id", "state", "tank_installed_date", "tank_closed_date", "tank_status",
      "leak_after_closure", "no_leak",
      "capacity", "single_walled", "double_walled", "unknown_walled",
      "is_gasoline", "is_diesel", "is_oil_kerosene", "is_jet_fuel", "is_other", "county_name",
      "latitude", "longitude"
    )
    cols_to_keep <- intersect(names(MI_UST_tanks_SD), required_columns)
    MI_UST_tanks_SD <- MI_UST_tanks_SD[, ..cols_to_keep]
    
    # 7. Save ---------------------------------------------------------------
    cat("\nSaving Final Files...\n")
    output_dir <- here("Data", "Raw", "state_databases", "Michigan")
    if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
    
    fwrite(MI_UST_tanks_SD, file.path(output_dir, "MI_Harmonized_UST_tanks.csv"))
    fwrite(MI_LUST_SD, file.path(output_dir, "MI_Harmonized_LUST.csv"))
    
    cat("Done.\n")
    cat("Tank records:", nrow(MI_UST_tanks_SD), "\n")
    
  } else {
    warning("LUST file 'Releases.csv' not found.")
  }
} else {
  stop("Michigan Master Excel file not found.")
}


## New Jersey UST Dataset Creation
# Script: 02_Clean_NJ.R
# Author: Validated Classification Logic (Updated Jan 2026)
# Description: Harmonizes NJ UST/LUST data.
# Key Features:
#   1. Robust Import: Uses colClasses="character" to prevent type inference errors.
#   2. Specific Logic: Uses explicit 'tank_structure' columns (Single Wall vs Double Wall).
#   3. Date Parsing: Handles "1/1/1944 12:00:00 AM" format.
#   4. Geography: Converts confirmed NJ State Plane (EPSG:3424) to Lat/Long (EPSG:4326).

# 0. Setup ----------------------------------------------------------------
COUNTYVARIABLE <- "county"

library(tidyverse)
library(data.table)
library(here)
library(lubridate)
library(janitor)
library(stringr)
library(sf) # Required for EPSG:3424 -> EPSG:4326 conversion

# 1. Helper Functions -----------------------------------------------------

standardize_county_name <- function(county_name) {
  name <- tolower(county_name)
  name <- gsub(" county$| counties$| parish$| parishes$", "", name)
  name <- gsub("[^a-z ]", "", name)
  name <- trimws(gsub("\\s+", " ", name))
  name[name == ""] <- NA
  return(name)
}

# 2. Classification Logic -------------------------------------------------

classify_tank_walls_NJ <- function(data) {
  result <- copy(data)
  
  # NJ has a dedicated 'tank_structure' column
  val_struct <- toupper(result$tank_structure)
  val_const <- toupper(result$tank_construction) # Fallback
  
  # 1. Double Walled
  result[, is_double := grepl("DOUBLE WALL|SECONDARY CONTAINMENT|TRIPLE WALL", val_struct) | 
           grepl("DOUBLE WALL|SECONDARY CONTAINMENT", val_const)]
  
  # 2. Single Walled
  result[, is_single := !is_double & (grepl("SINGLE WALL", val_struct) | 
                                      grepl("BARE STEEL|COATED STEEL|FIBERGLASS", val_const))]
  
  # 3. Assign Harmonized Classes
  result[, `:=`(
    double_walled = as.integer(is_double),
    single_walled = as.integer(is_single),
    unknown_walled = as.integer(!is_double & !is_single),
    missing_walled = as.integer(is.na(tank_structure) & is.na(tank_construction))
  )]
  
  result[, `:=`(is_double = NULL, is_single = NULL)]
  
  cat("\nClassification Summary:\n")
  print(result[, .(total=.N, single=sum(single_walled), double=sum(double_walled), unknown=sum(unknown_walled))])
  
  return(result)
}

classify_substances <- function(data, substance_col_name = "tank_contents") {
  result <- copy(data)
  result[, `:=`(is_gasoline=0, is_diesel=0, is_oil_kerosene=0, is_jet_fuel=0, is_other=1)]
  
  if(substance_col_name %in% names(result)) {
    result[, sub_lower := tolower(get(substance_col_name))]
    
    gas_pat <- "gasoline|petrol|unleaded|premium|regular|gasohol|ethanol|e85"
    dsl_pat <- "diesel|bio.?diesel"
    oil_pat <- "oil|kerosene|lube|waste|heating"
    jet_pat <- "jet|aviation|jp"
    
    result[grepl(gas_pat, sub_lower), is_gasoline := 1]
    result[grepl(dsl_pat, sub_lower) & !grepl("def", sub_lower), is_diesel := 1]
    result[grepl(oil_pat, sub_lower), is_oil_kerosene := 1]
    result[grepl(jet_pat, sub_lower), is_jet_fuel := 1]
    
    result[is_gasoline==1 | is_diesel==1 | is_oil_kerosene==1 | is_jet_fuel==1, is_other := 0]
    result[, sub_lower := NULL]
  }
  return(result)
}

# 3. Load Data ------------------------------------------------------------
cat("\nLoading New Jersey UST Data...\n")

# Use 'preferred_id' as key. 
# Force char to match between tables and avoid "column not found" warnings
NJ_tanks_raw <- fread(here("Data","Raw","state_databases","New Jersey","New Jersey Tanks Data.csv"), 
                      colClasses = "character") %>% 
  janitor::clean_names() %>% as.data.table()

NJ_facility_raw <- fread(here("Data","Raw","state_databases","New Jersey","New Jersey Facility Data.csv"), 
                         colClasses = "character") %>% 
  janitor::clean_names() %>% as.data.table()

cat("Loaded", nrow(NJ_tanks_raw), "tanks and", nrow(NJ_facility_raw), "facilities.\n")

# 4. Join & Process (Geospatial Conversion) -------------------------------
cat("\nJoining and Processing...\n")

# Join Facility Info (Name, County, Coords) to Tanks
NJ_UST_tanks <- merge(
  NJ_tanks_raw,
  NJ_facility_raw[, .(preferred_id, facility_name = pi_name, county_name = county, 
                      x_coord = x, y_coord = y)],
  by = "preferred_id",
  all.x = TRUE
)

# --- GEOSPATIAL CONVERSION (NJ State Plane -> WGS84) ---
cat("Converting State Plane coordinates to Lat/Long using sf...\n")

# 1. Prepare unique locations
# Convert chars to numeric for sf. 
locs <- unique(NJ_UST_tanks[!is.na(x_coord) & !is.na(y_coord) & x_coord != "" & y_coord != "", 
                            .(facility_id = preferred_id, x = as.numeric(x_coord), y = as.numeric(y_coord))])

if(nrow(locs) > 0) {
  # 2. Create SF object (EPSG:3424 = NAD83 / New Jersey ftUS)
  sf_locs <- st_as_sf(locs, coords = c("x", "y"), crs = 3424)
  
  # 3. Transform to WGS84 (EPSG:4326)
  sf_trans <- st_transform(sf_locs, 4326)
  
  # 4. Extract Lat/Long
  coords <- st_coordinates(sf_trans)
  locs[, `:=`(longitude = coords[,1], latitude = coords[,2])]
  
  # 5. Merge back to main tank data
  NJ_UST_tanks <- merge(NJ_UST_tanks, locs[, .(facility_id, latitude, longitude)], 
                        by.x="preferred_id", by.y="facility_id", all.x=TRUE)
} else {
  NJ_UST_tanks[, `:=`(latitude = NA_real_, longitude = NA_real_)]
}

# Rename Standard Columns
NJ_UST_tanks <- NJ_UST_tanks %>%
  rename(
    facility_id = preferred_id,
    tank_id = tank_number,
    tank_installed_date = installed_date,
    tank_closed_date = out_of_service_date,
    capacity = tank_volume,
    substance = tank_contents
  ) %>%
  as.data.table()

# Clean Dates (MDY HMS format: "1/1/1944 12:00:00 AM")
NJ_UST_tanks[, tank_installed_date := as.Date(lubridate::mdy_hms(tank_installed_date, quiet=TRUE))]
NJ_UST_tanks[, tank_closed_date := as.Date(lubridate::mdy_hms(tank_closed_date, quiet=TRUE))]

# Status Logic
NJ_UST_tanks[, tank_status := case_when(
  tolower(tank_status_description) == "in-use" ~ "Open",
  tolower(tank_status_description) %in% c("abandoned in place", "out of service", "removed") ~ "Closed",
  TRUE ~ "Ignore"
)]
NJ_UST_tanks <- NJ_UST_tanks[tank_status != "Ignore"]

# Clean Capacity
NJ_UST_tanks[, capacity := as.numeric(gsub("[^0-9.]", "", capacity))]

# Classify
NJ_UST_tanks <- classify_tank_walls_NJ(NJ_UST_tanks)
NJ_UST_tanks <- classify_substances(NJ_UST_tanks)

# 5. Process LUST Data ----------------------------------------------------
cat("\nProcessing New Jersey LUST Data...\n")

lust_path <- here("Data", "Raw", "Releases.csv")

if(file.exists(lust_path)) {
  NJ_lust_raw <- fread(lust_path, colClasses = "character") %>% 
    janitor::clean_names() %>%
    filter(state == "New Jersey") %>%
    as.data.table()
  
  NJ_LUST_SD <- NJ_lust_raw[, .(
    facility_id = gsub("^NJ", "", facility_id),
    LUST_id = lust_id,
    report_date = reported_date,
    nfa_date = NA_character_,
    state = "NJ"
  )]
  
  # Date Parsing
  NJ_LUST_SD[, report_date := as.Date(lubridate::mdy_hm(report_date, quiet=TRUE))]
  
  # 6. Merge & Aggregation --------------------------------------------------
  cat("\nMerging and Aggregating...\n")
  
  NJ_UST_tanks_LUST <- merge(NJ_UST_tanks, NJ_LUST_SD, by="facility_id", all.x=TRUE, allow.cartesian=TRUE)
  
  NJ_UST_tanks_LUST[, `:=`(
    leak_after_closure = as.integer(!is.na(tank_closed_date) & !is.na(report_date) & report_date > tank_closed_date),
    no_leak = as.integer(is.na(report_date) & !is.na(tank_closed_date))
  )]
  
  # Init Lat/Long if missing to ensure columns exist
  if (!"latitude" %in% names(NJ_UST_tanks_LUST)) NJ_UST_tanks_LUST[, latitude := NA_real_]
  if (!"longitude" %in% names(NJ_UST_tanks_LUST)) NJ_UST_tanks_LUST[, longitude := NA_real_]
  
  NJ_UST_tanks_SD <- NJ_UST_tanks_LUST[, .(
    facility_name = first(facility_name),
    tank_status = first(tank_status),
    leak_after_closure = max(leak_after_closure, na.rm=T),
    no_leak = max(no_leak, na.rm=T),
    
    single_walled = max(single_walled, na.rm=T),
    double_walled = max(double_walled, na.rm=T),
    unknown_walled = max(unknown_walled, missing_walled, na.rm=T),
    
    is_gasoline = max(is_gasoline, na.rm=T),
    is_diesel = max(is_diesel, na.rm=T),
    is_oil_kerosene = max(is_oil_kerosene, na.rm=T),
    is_jet_fuel = max(is_jet_fuel, na.rm=T),
    is_other = max(is_other, na.rm=T),
    
    capacity = mean(capacity, na.rm=T),
    latitude = first(latitude),
    longitude = first(longitude)
  ), by = .(facility_id, tank_id, tank_installed_date, tank_closed_date, county_name)]
  
  NJ_UST_tanks_SD[, state := "NJ"]
  
  # Final Column Selection
  required_columns <- c(
    "facility_id", "facility_name", "tank_id", "state", "tank_installed_date", "tank_closed_date", "tank_status",
    "leak_after_closure", "no_leak",
    "capacity", "single_walled", "double_walled", "unknown_walled",
    "is_gasoline", "is_diesel", "is_oil_kerosene", "is_jet_fuel", "is_other", "county_name",
    "latitude", "longitude"
  )
  cols_to_keep <- intersect(names(NJ_UST_tanks_SD), required_columns)
  NJ_UST_tanks_SD <- NJ_UST_tanks_SD[, ..cols_to_keep]
  
  # 7. Save ---------------------------------------------------------------
  cat("\nSaving Final Files...\n")
  output_dir <- here("Data", "Raw", "state_databases", "New Jersey")
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  
  fwrite(NJ_UST_tanks_SD, file.path(output_dir, "NJ_Harmonized_UST_tanks.csv"))
  fwrite(NJ_LUST_SD, file.path(output_dir, "NJ_Harmonized_LUST.csv"))
  
  cat("Done.\n")
  cat("Tank records:", nrow(NJ_UST_tanks_SD), "\n")
  
} else {
  warning("LUST file 'Releases.csv' not found.")
}


## New Mexico UST Dataset Creation
# Script: 02_Clean_NM.R
# Author: Validated Classification Logic (Updated Jan 2026)
# Description: Harmonizes NM UST/LUST data using EPA National file + State LUST files.

# 0. Setup ----------------------------------------------------------------
COUNTYVARIABLE <- "county"

library(tidyverse)
library(data.table)
library(here)
library(lubridate)
library(janitor)
library(readxl)
library(stringr)

# 1. Helper Functions -----------------------------------------------------

classify_substances <- function(data, substance_col_name = "substances") {
  result <- copy(data)
  result[, `:=`(is_gasoline = 0, is_diesel = 0, is_oil_kerosene = 0, is_jet_fuel = 0, is_other = 1)]
  
  if(substance_col_name %in% names(result)) {
    result[, sub_lower := tolower(get(substance_col_name))]
    
    gas_pat <- "gasoline|unleaded|petrol|mogas|ethanol|e85|alcohol enriched"
    dsl_pat <- "diesel|biodiesel"
    oil_pat <- "oil|kerosene|fluid|hydraulic"
    jet_pat <- "jet|aviation|avgas"
    
    result[grepl(gas_pat, sub_lower), is_gasoline := 1]
    result[grepl(dsl_pat, sub_lower) & !grepl("def", sub_lower), is_diesel := 1]
    result[grepl(oil_pat, sub_lower), is_oil_kerosene := 1]
    result[grepl(jet_pat, sub_lower), is_jet_fuel := 1]
    
    result[is_gasoline==1 | is_diesel==1 | is_oil_kerosene==1 | is_jet_fuel==1, is_other := 0]
    result[, sub_lower := NULL]
  }
  return(result)
}

classify_tank_walls <- function(data, tank_wall_col = "tank_wall_type") {
  result <- copy(data)
  
  if(!tank_wall_col %in% names(result)) {
    result[, `:=`(double_walled=0, single_walled=0, unknown_walled=1, missing_walled=1)]
    return(result)
  }
  
  result[, `:=`(
    double_walled = as.integer(grepl("Double Walled", get(tank_wall_col), ignore.case = TRUE)),
    single_walled = as.integer(grepl("Single Walled", get(tank_wall_col), ignore.case = TRUE)),
    unknown_walled = as.integer(is.na(get(tank_wall_col)) | get(tank_wall_col) == "" | grepl("Unknown", get(tank_wall_col), ignore.case=TRUE)),
    missing_walled = as.integer(is.na(get(tank_wall_col)) | get(tank_wall_col) == "")
  )]
  return(result)
}

standardize_capacity <- function(data, capacity_col = "capacity") {
  result <- copy(data)
  if(capacity_col %in% names(result)) {
    result[, `:=`(capacity = as.numeric(gsub("[^0-9.]", "", get(capacity_col))))]
  } else {
    result[, capacity := NA_real_]
  }
  return(result)
}

# 2. Load National Data (Permissive Strategy) -----------------------------
cat("\nLoading National EPA Data...\n")

# Load USTs - clean names immediately
ust_data_epa <- fread(here("Data","Raw","USTs.csv")) %>% 
  janitor::clean_names() %>% 
  as.data.table()

# Load Facilities - Only fetch ID, County, Lat, Long
facilities_path <- here("Data","Raw","Facilities.csv")
if(file.exists(facilities_path)) {
  epa_facilities <- fread(facilities_path) %>% 
    janitor::clean_names() %>% 
    as.data.table()
  
  # Join Facility Info (Strictly what you asked for)
  cat("  Joining Facility Locations...\n")
  cols_to_join <- c("facility_id", "county", "latitude", "longitude")
  cols_present <- intersect(names(epa_facilities), cols_to_join)
  
  ust_data_epa <- merge(ust_data_epa, 
                        epa_facilities[, ..cols_present], 
                        by = "facility_id", 
                        all.x = TRUE)
} else {
  warning("Facilities.csv not found. Geocoding will be missing.")
}

# 3. Filter for New Mexico ------------------------------------------------
cat("\nFiltering for New Mexico...\n")

# Determine state column name
state_col <- grep("^state$", names(ust_data_epa), value = TRUE)
if(length(state_col) == 0) state_col <- "facility_state"

NM_UST <- ust_data_epa[grepl('New Mexico|NM', get(state_col), ignore.case = TRUE)]

cat("NM Tanks filtered:", nrow(NM_UST), "\n")

# 4. Process Tank Data (ID Cleaning Step) ---------------------------------
cat("\nProcessing Tank Attributes...\n")

NM_UST_tanks <- NM_UST %>%
  select(
    facility_id, 
    tank_id, 
    installation_date, 
    removal_date, 
    tank_status, 
    county,
    substances, 
    tank_wall_type, 
    capacity,
    latitude,
    longitude
  ) %>%
  rename(
    tank_installed_date = installation_date,
    tank_closed_date = removal_date,
    county_name = county
  ) %>%
  mutate(
    # CRITICAL: Strip letters and make numeric to match LUST data
    facility_id = as.numeric(gsub("[A-Za-z]", "", facility_id)),
    tank_id = as.character(tank_id),
    state = "NM"
  ) %>%
  as.data.table()

# Clean Dates
NM_UST_tanks[, tank_installed_date := lubridate::parse_date_time(tank_installed_date, orders = c("ymd", "mdy", "mdy HMS"), quiet = TRUE)]
NM_UST_tanks[, tank_closed_date := lubridate::parse_date_time(tank_closed_date, orders = c("ymd", "mdy", "mdy HMS"), quiet = TRUE)]

NM_UST_tanks[, tank_installed_date := as.Date(tank_installed_date)]
NM_UST_tanks[, tank_closed_date := as.Date(tank_closed_date)]

# Harmonize Lat/Long
if("latitude" %in% names(NM_UST_tanks)) NM_UST_tanks[, latitude := as.numeric(latitude)] else NM_UST_tanks[, latitude := NA_real_]
if("longitude" %in% names(NM_UST_tanks)) NM_UST_tanks[, longitude := as.numeric(longitude)] else NM_UST_tanks[, longitude := NA_real_]

# Classify
NM_UST_tanks <- classify_substances(NM_UST_tanks, "substances")
NM_UST_tanks <- classify_tank_walls(NM_UST_tanks, "tank_wall_type")
NM_UST_tanks <- standardize_capacity(NM_UST_tanks, "capacity")

# 5. Load LUST Data (Targeted Files) --------------------------------------
cat("\nLoading New Mexico LUST Data...\n")

nm_path <- here("Data","Raw","state_databases","New Mexico")
NM_LUST_files <- list.files(nm_path, full.names = TRUE)

rp_file <- grep("rp_nfa_amount", NM_LUST_files, value = TRUE, ignore.case = TRUE)
sl_file <- grep("sl_nfa_amount", NM_LUST_files, value = TRUE, ignore.case = TRUE)

if(length(rp_file) > 0 && length(sl_file) > 0) {
  
  NM_LUST_01 <- read_excel(rp_file[1]) %>% clean_names() %>% as.data.table()
  NM_LUST_02 <- read_excel(sl_file[1]) %>% clean_names() %>% as.data.table()
  
  cols_keep <- c("rid", "fid", "release_date", "nfa_date", "total_wp", "total_paid")
  
  safe_select <- function(dt, cols) {
    valid <- intersect(names(dt), cols)
    dt[, ..valid]
  }
  
  l1 <- safe_select(NM_LUST_01, cols_keep)
  l2 <- safe_select(NM_LUST_02, cols_keep)
  
  NM_LUST <- rbindlist(list(l1, l2), fill=TRUE)
  NM_LUST <- NM_LUST[!is.na(rid) & !is.na(release_date)]
  
  NM_LUST_SD <- NM_LUST %>%
    rename(
      facility_id = fid, 
      LUST_id = rid, 
      report_date = release_date
    ) %>%
    mutate(
      # CRITICAL: Ensure Facility ID is numeric to match Tanks
      facility_id = as.numeric(facility_id),
      LUST_id = as.character(LUST_id),
      report_date = as.Date(report_date),
      nfa_date = as.Date(nfa_date),
      state = "NM"
    ) %>%
    as.data.table()
  
} else {
  warning("Critical LUST files (rp_nfa/sl_nfa) not found.")
  NM_LUST_SD <- data.table(facility_id=numeric(), LUST_id=character(), report_date=as.Date(character()), nfa_date=as.Date(character()), total_wp=numeric(), total_paid=numeric())
}

# 6. Join & Aggregation ---------------------------------------------------
cat("\nMerging and Aggregating...\n")

# Collapse LUSTs
state_leak_data_collapsed <- NM_LUST_SD[, .(
  LUST_count = .N,
  total_wp = sum(total_wp, na.rm = TRUE),
  total_paid = sum(total_paid, na.rm = TRUE)
), by = .(facility_id, report_date, nfa_date)]

# Numeric Join
NM_UST_tanks_LUST <- merge(NM_UST_tanks, state_leak_data_collapsed, by="facility_id", all.x=TRUE, allow.cartesian=TRUE)

NM_UST_tanks_LUST[, `:=`(
  leak_after_closure = as.integer(!is.na(tank_closed_date) & !is.na(report_date) & report_date > tank_closed_date),
  leak_before_NFA_before_closure = as.integer(!is.na(tank_closed_date) & !is.na(report_date) & report_date < tank_closed_date & (is.na(nfa_date) | nfa_date < tank_closed_date)),
  leak_before_NFA_after_closure = as.integer(!is.na(tank_closed_date) & !is.na(report_date) & report_date < tank_closed_date & !is.na(nfa_date) & nfa_date > tank_closed_date),
  no_leak = as.integer(is.na(report_date) & !is.na(tank_closed_date))
)]

# Final Aggregate
NM_UST_tanks_SD <- NM_UST_tanks_LUST[, .(
  # Note: facility_name missing from raw select, handled gracefully if needed
  tank_status = first(tank_status),
  
  leak_after_closure = max(leak_after_closure, na.rm=T),
  leak_before_NFA_before_closure = max(leak_before_NFA_before_closure, na.rm=T),
  leak_before_NFA_after_closure = max(leak_before_NFA_after_closure, na.rm=T),
  no_leak = max(no_leak, na.rm=T),
  
  single_walled = max(single_walled, na.rm=T),
  double_walled = max(double_walled, na.rm=T),
  unknown_walled = max(unknown_walled, missing_walled, na.rm=T),
  
  is_gasoline = max(is_gasoline, na.rm=T),
  is_diesel = max(is_diesel, na.rm=T),
  is_oil_kerosene = max(is_oil_kerosene, na.rm=T),
  is_jet_fuel = max(is_jet_fuel, na.rm=T),
  is_other = max(is_other, na.rm=T),
  
  capacity = mean(capacity, na.rm=T),
  latitude = first(latitude),
  longitude = first(longitude)
), by = .(facility_id, tank_id, tank_installed_date, tank_closed_date, county_name)]

NM_UST_tanks_SD[, state := "NM"]
NM_UST_tanks_SD[, facility_id := as.character(facility_id)] # Return to char for final CSV

# 7. Output ---------------------------------------------------------------
cat("\nSaving Final Files...\n")
if (!dir.exists(nm_path)) dir.create(nm_path, recursive = TRUE)

fwrite(NM_UST_tanks_SD, file.path(nm_path, "NM_Harmonized_UST_tanks.csv"))
fwrite(NM_LUST_SD, file.path(nm_path, "NM_Harmonized_LUST.csv"))

cat("Done.\n")
cat("Tank records:", nrow(NM_UST_tanks_SD), "\n")
cat("LUST records:", nrow(NM_LUST_SD), "\n")

## Oklahoma UST Dataset Creation
# Script: 02_Clean_OK.R
# Author: Validated Classification Logic (Updated Jan 2026)
# Description: Harmonizes OK UST/LUST data.
# Key Features:
#   1. Robust Import: Dynamically finds Tank vs Release files.
#   2. ID Cleaning: Removes brackets '[]' from facility IDs.
#   3. Geocoding: Preserves native Latitude/Longitude columns.
#   4. Harmonization: Enforces strict column naming conventions.

# 0. Setup ----------------------------------------------------------------
COUNTYVARIABLE <- "county"

library(tidyverse)
library(data.table)
library(here)
library(lubridate)
library(janitor)
library(stringr)

# 1. Helper Functions -----------------------------------------------------

classify_substances <- function(data, substance_col_name = "tank_substance") {
  result <- copy(data)
  result[, `:=`(is_gasoline=0, is_diesel=0, is_oil_kerosene=0, is_jet_fuel=0, is_other=1)]
  
  if(substance_col_name %in% names(result)) {
    result[, sub_lower := tolower(get(substance_col_name))]
    
    gas_pat <- "gasoline|unleaded|petrol|ethanol|e85|e10|e15|e-10|e-85"
    dsl_pat <- "diesel|biodiesel|dyed diesel"
    oil_pat <- "oil|kerosene|used oil|waste oil|lube|hydraulic"
    jet_pat <- "jet|aviation|avgas"
    
    result[grepl(gas_pat, sub_lower), is_gasoline := 1]
    result[grepl(dsl_pat, sub_lower) & !grepl("def", sub_lower), is_diesel := 1]
    result[grepl(oil_pat, sub_lower), is_oil_kerosene := 1]
    result[grepl(jet_pat, sub_lower), is_jet_fuel := 1]
    
    result[is_gasoline==1 | is_diesel==1 | is_oil_kerosene==1 | is_jet_fuel==1, is_other := 0]
    result[, sub_lower := NULL]
  }
  return(result)
}

classify_tank_walls_OK <- function(data, tank_wall_col = "tank_construction") {
  result <- copy(data)
  
  vals <- toupper(result[[tank_wall_col]])
  
  # 1. Double Walled (Explicit in OK data)
  result[, is_double := grepl("DOUBLE WALLED|DOUBLE-WALLED|SECONDARILY CONTAINED|JACKETED", vals)]
  
  # 2. Single Walled (Explicit in OK data)
  result[, is_single := grepl("SINGLE WALLED|SINGLE-WALLED", vals)]
  
  # 3. Assign Harmonized Classes
  result[, `:=`(
    double_walled = as.integer(is_double),
    single_walled = as.integer(is_single),
    unknown_walled = as.integer(!is_double & !is_single),
    missing_walled = as.integer(is.na(vals) | vals == "")
  )]
  
  result[, `:=`(is_double = NULL, is_single = NULL)]
  return(result)
}

# 2. Load Data ------------------------------------------------------------
cat("\nLoading Oklahoma UST Data...\n")

ok_path <- here("Data","Raw","state_databases","Oklahoma")
ok_files <- list.files(ok_path, pattern = "\\.csv$", full.names = TRUE)

tank_file <- grep("Tank|PST_Tank", ok_files, value = TRUE, ignore.case = TRUE)
lust_file <- grep("Case|Release|PST_Case", ok_files, value = TRUE, ignore.case = TRUE)

if(length(tank_file) == 0 || length(lust_file) == 0) stop("Could not find Tank or Case CSV files in Oklahoma folder.")

# Permissive Load
OK_Tanks_Raw <- fread(tank_file[1], colClasses = "character") %>% 
  janitor::clean_names() %>% 
  as.data.table()

OK_LUST_Raw <- fread(lust_file[1], colClasses = "character") %>% 
  janitor::clean_names() %>% 
  as.data.table()

cat("Loaded", nrow(OK_Tanks_Raw), "tanks and", nrow(OK_LUST_Raw), "cases.\n")

# 3. Process Tank Data ----------------------------------------------------
cat("\nProcessing Tank Attributes...\n")

OK_UST_tanks <- OK_Tanks_Raw[toupper(tank_type) == "UST"]

OK_UST_tanks <- OK_UST_tanks[, .(
  facility_id = facility_number,
  facility_name = facility_name,
  tank_id = tank_number,
  tank_installed_date,
  tank_closed_date,
  tank_status,
  tank_substance,
  tank_construction,
  tank_capacity,
  latitude,
  longitude,
  facility_city
)]

# Clean IDs: Remove brackets [12345] -> 12345
OK_UST_tanks[, facility_id := gsub("\\[|\\]", "", facility_id)]
OK_UST_tanks[, tank_id := gsub("\\[|\\]", "", tank_id)]

# Dates
OK_UST_tanks[, tank_installed_date := lubridate::mdy(tank_installed_date, quiet = TRUE)]
OK_UST_tanks[, tank_closed_date := lubridate::mdy(tank_closed_date, quiet = TRUE)]

# Status Logic (CIU=Open, POU=Closed)
OK_UST_tanks[, tank_status_clean := case_when(
  tank_status == "CIU" ~ "Open",
  tank_status == "POU" ~ "Closed",
  tank_status == "TOU" ~ "Temporary",
  TRUE ~ "Closed"
)]

# Clean Capacity & Coords
OK_UST_tanks[, capacity := as.numeric(gsub("[^0-9.]", "", tank_capacity))]
OK_UST_tanks[, latitude := as.numeric(latitude)]
OK_UST_tanks[, longitude := as.numeric(longitude)]

# Classify
OK_UST_tanks <- classify_tank_walls_OK(OK_UST_tanks)
OK_UST_tanks <- classify_substances(OK_UST_tanks)

# 4. Process LUST Data ----------------------------------------------------
cat("\nProcessing LUST Data...\n")

OK_LUST_SD <- OK_LUST_Raw[, .(
  facility_id = facility_number,
  LUST_id = case_number,
  report_date = release_date,
  nfa_date = close_date,
  state = "OK"
)]

OK_LUST_SD[, facility_id := gsub("\\[|\\]", "", facility_id)]
OK_LUST_SD[, LUST_id := gsub("\\[|\\]", "", LUST_id)]
OK_LUST_SD[, report_date := lubridate::mdy(report_date, quiet = TRUE)]
OK_LUST_SD[, nfa_date := lubridate::mdy(nfa_date, quiet = TRUE)]

# 5. Merge & Aggregation --------------------------------------------------
cat("\nMerging and Aggregating...\n")

OK_UST_tanks_LUST <- merge(OK_UST_tanks, OK_LUST_SD, by="facility_id", all.x=TRUE, allow.cartesian=TRUE)

OK_UST_tanks_LUST[, `:=`(
  leak_after_closure = as.integer(!is.na(tank_closed_date) & !is.na(report_date) & report_date > tank_closed_date),
  leak_before_NFA_before_closure = as.integer(!is.na(tank_closed_date) & !is.na(report_date) & report_date < tank_closed_date & (is.na(nfa_date) | nfa_date < tank_closed_date)),
  leak_before_NFA_after_closure = as.integer(!is.na(tank_closed_date) & !is.na(report_date) & report_date < tank_closed_date & !is.na(nfa_date) & nfa_date > tank_closed_date),
  no_leak = as.integer(is.na(report_date) & !is.na(tank_closed_date))
)]

# Init County if missing (Using City as proxy placeholder if County not in raw)
if(!"county_name" %in% names(OK_UST_tanks_LUST)) OK_UST_tanks_LUST[, county_name := facility_city]

OK_UST_tanks_SD <- OK_UST_tanks_LUST[, .(
  facility_name = first(facility_name),
  tank_status = first(tank_status_clean),
  
  leak_after_closure = max(leak_after_closure, na.rm=T),
  leak_before_NFA_before_closure = max(leak_before_NFA_before_closure, na.rm=T),
  leak_before_NFA_after_closure = max(leak_before_NFA_after_closure, na.rm=T),
  no_leak = max(no_leak, na.rm=T),
  
  single_walled = max(single_walled, na.rm=T),
  double_walled = max(double_walled, na.rm=T),
  unknown_walled = max(unknown_walled, missing_walled, na.rm=T),
  
  is_gasoline = max(is_gasoline, na.rm=T),
  is_diesel = max(is_diesel, na.rm=T),
  is_oil_kerosene = max(is_oil_kerosene, na.rm=T),
  is_jet_fuel = max(is_jet_fuel, na.rm=T),
  is_other = max(is_other, na.rm=T),
  
  capacity = mean(capacity, na.rm=T),
  latitude = first(latitude),
  longitude = first(longitude)
), by = .(facility_id, tank_id, tank_installed_date, tank_closed_date, county_name)]

OK_UST_tanks_SD[, state := "OK"]

# --- HARMONIZATION: Rename & Select Columns ---
# 1. Define the Strict List
required_columns <- c(
  "facility_id", "facility_name", "tank_id", "state", "tank_installed_date", "tank_closed_date", "tank_status",
  "leak_after_closure", "leak_before_NFA_before_closure", "leak_before_NFA_after_closure", "no_leak",
  "capacity", "single_walled", "double_walled", "unknown_walled",
  "is_gasoline", "is_diesel", "is_oil_kerosene", "is_jet_fuel", "is_other", "county_name"
)

# 2. Append Lat/Long (Essential for Geocoding, even if not in original list)
final_columns <- c(required_columns, "latitude", "longitude")

# 3. Filter the Dataset
# intersect ensures we don't crash if a column is missing (though our logic guarantees them)
cols_to_keep <- intersect(names(OK_UST_tanks_SD), final_columns)
OK_UST_tanks_SD <- OK_UST_tanks_SD[, ..cols_to_keep]

# 6. Output ---------------------------------------------------------------
cat("\nSaving Final Files...\n")
if (!dir.exists(ok_path)) dir.create(ok_path, recursive = TRUE)

fwrite(OK_UST_tanks_SD, file.path(ok_path, "OK_Harmonized_UST_tanks.csv"))
fwrite(OK_LUST_SD, file.path(ok_path, "OK_Harmonized_LUST.csv"))

cat("Done.\n")
cat("Tank records:", nrow(OK_UST_tanks_SD), "\n")
cat("LUST records:", nrow(OK_LUST_SD), "\n")


###############################################################################
# tx_fr_panel.R   
###############################################################################


# ── Section 0: Setup ──────────────────────────────────────────────────────────
# Purpose: load core libraries, threads & I/O helpers, path utility, and ID-cleaning functions.
# Why: ensures consistent environment, parallelism & reliable file paths across server/local.
# End product: `clean_id_column()` and consistent `here()`-based paths ready for subsequent sections.
rm(list = ls())
library(data.table)
library(lubridate)
library(maps)
library(stringr)
library(parallel) 
library(tidycensus) 
library(zoo) 
library(tidyverse)
library(here) 

# ── Path Configuration ───────────────────────────────────────────────────────

# 1. Output for Harmonized files (Where final UST/LUST CSVs go)
tx_output_dir <- here("Data", "Raw", "state_databases", "Texas")
if (!dir.exists(tx_output_dir)) dir.create(tx_output_dir, recursive = TRUE)

# 2. Staging Directories for Panel Merge (Where intermediate .rds/.csv files go)
# We place these in Data/Intermediate/Texas to keep the Raw folder clean
staging_root <- here("Data", "Intermediate", "Texas")

stage_dir  <- file.path(staging_root, "panel_merge_staging")
lust_dir   <- file.path(staging_root, "lust_merge_staging")
cens_dir   <- file.path(staging_root, "census_merge_staging")
output_figures <- here("Output", "Figures") # Figures/tables only (NO data files)

# Create directories
dir.create(stage_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(lust_dir,  recursive = TRUE, showWarnings = FALSE)
dir.create(cens_dir,  recursive = TRUE, showWarnings = FALSE)
dir.create(output_figures, recursive = TRUE, showWarnings = FALSE)

# ── Server Configuration ─────────────────────────────────────────────────────
do_merge   <- FALSE          # set FALSE if you only want the staging artefacts
onserver   <- TRUE          # your existing flag

# Set data.table threads based on environment
if (onserver) {
  setDTthreads(detectCores() - 2)  # Use available cores minus 2
} else {
  setDTthreads(4)  # Use 4 threads for local development
}
message("data.table is using ", getDTthreads(), " threads.")

# ── Helper Functions ─────────────────────────────────────────────────────────

# Helper to ensure consistent ID cleaning
clean_id_column <- function(dt, col_name) {
  if (col_name %in% names(dt)) {
    dt[, (col_name) := trimws(as.character(get(col_name)))]
  }
  dt
}

# Helper for fast writing
quick_write <- function(dt, dir_path, nm) {
  fwrite(dt, file.path(dir_path, paste0(nm, ".csv")))     
  saveRDS(dt, file.path(dir_path, paste0(nm, ".rds")))     
  invisible(NULL)
}


# ── Fetch and Save ACS County-Level Time Series Data ─────────────────────────
# This entire section is removed as per the request to remove old ACS fetching
# and urban/rural variables. The new census data handling is done in Section 8.
# ───────────────────────────────────────────────────────────────────────────────

# Create output directories
dir.create(here("Output", "Figures"), showWarnings = FALSE, recursive = TRUE)
dir.create(here("Output", "Tables"),  showWarnings = FALSE, recursive = TRUE)
dir.create(here("Data", "Raw"),       showWarnings = FALSE, recursive = TRUE)
dir.create(here("Data", "Processed"), showWarnings = FALSE, recursive = TRUE)



# ── helper: temp-download + fixed-width splitter ─────────────────────────────
load_and_process_data <- function(url, col_widths, col_names, timeout = 600) {
  tf <- tempfile(); on.exit(unlink(tf))
  old_t <- getOption("timeout"); on.exit(options(timeout = old_t), add = TRUE)
  options(timeout = timeout)
  message("⇣  downloading ", basename(url))
  download.file(url, tf, method = "libcurl", quiet = TRUE)

  raw <- readLines(tf, warn = FALSE)

  split_row <- function(row, w) {
    starts <- cumsum(c(1, w[-length(w)]))
    ends   <- cumsum(w)
    mapply(substr, row, starts, ends, USE.NAMES = FALSE)
  }
  mat <- t(vapply(raw, split_row, FUN.VALUE = character(length(col_widths)),
                  w = col_widths))
  dt  <- as.data.table(mat); setnames(dt, col_names)
  # Trim whitespace from all columns (as they are all character at this point)
  dt[, (col_names) := lapply(.SD, trimws), .SDcols = col_names]
  message("✓  processed ", basename(url))
  dt[]
}

# Function to classify substances (user-provided, adapted for data.table)
classify_substances <- function(data) {
  # Create binary classification columns with default values
  result <- copy(data) # Ensure we work on a copy
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
  result[is_gasoline == 1 | is_diesel == 1 | is_oil_kerosene == 1 | is_jet_fuel == 1, is_other := 0]
  
  # Remove temporary column
  result[, substances_lower := NULL]
  
  # Create a summary table of the classifications
  message("\nSubstance classification summary for processed tanks:")
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

clean_merge_keys <- function(dt, key_cols, to_upper = TRUE) {
  for (k in key_cols) if (k %in% names(dt)) {
    dt[[k]] <- trimws(as.character(dt[[k]]))
    if (to_upper) dt[[k]] <- toupper(dt[[k]])
  }
  dt
}

base <- "https://www.tceq.texas.gov/assets/public/admin/data/docs"

# ──────────────────────────────────────────────────────────────────────────────
# OPTIMIZED MONTHLY EXPANSION using Rolling Joins
# ──────────────────────────────────────────────────────────────────────────────
# Purpose: Expand contracts to monthly observations efficiently
# Method: Use data.table's rolling joins instead of individual contract expansion
# Note: Gap detection happens later when FA data is merged into the complete panel
# ──────────────────────────────────────────────────────────────────────────────

optimize_monthly_expansion <- function(fa_contracts) {
  
  # Step 1: Create master monthly grid for all relevant periods
  min_eff <- fa_contracts[!is.na(EFF_DATE), min(EFF_DATE)]
  max_exp <- fa_contracts[!is.na(EXP_DATE), max(EXP_DATE)]
  max_exp <- pmin(max_exp, as.Date("2026-12-01"))
  
  monthly_grid <- data.table(
    month_date = seq(
      floor_date(min_eff, "month"),
      floor_date(max_exp, "month"),
      by = "month"
    )
  )
  monthly_grid[, `:=`(
    YEAR = year(month_date),
    MONTH = month(month_date)
  )]
  
  # Step 2: Prepare contracts for rolling join
  fa_clean <- fa_contracts[
    !is.na(EFF_DATE) & !is.na(EXP_DATE) & EFF_DATE <= EXP_DATE
  ][, `:=`(
    month_start = floor_date(EFF_DATE, "month"),
    month_end = floor_date(pmin(EXP_DATE, as.Date("2026-12-01")), "month")
  )]
  
  # Step 3: Cross join each contract with relevant months using data.table efficiency
  # This replaces the individual seq() calls with a more efficient approach
  fa_expanded <- fa_clean[monthly_grid, 
    on = .(month_start <= month_date, month_end >= month_date),
    nomatch = 0L,  # Only keep matches
    .(
      # Contract identifiers
      x.FACILITY_ID, x.FIN_ASSUR_ID,
      
      # Time identifiers  
      i.YEAR, i.MONTH, i.month_date,
      
      # Original contract dates
      x.EFF_DATE, x.EXP_DATE,
      
      # Calculate day-level edges for this month
      start_day_in_month = pmax(x.EFF_DATE, floor_date(i.month_date, "month")),
      end_day_in_month = pmin(x.EXP_DATE, ceiling_date(i.month_date, "month") - 1),
      
      # Contract attributes
      x.DETAIL_TYPE, x.CATEGORY,
      x.COVER_OCC, x.COVER_AGG,
      x.ISSUER_NAME,
      x.PREMIUM_PREPAID, x.PROOF_OF_FA,
      x.FP_CORR_MET, x.TP_FA_MET,
      x.USE_PRIVATE_mapped, x.USE_SELF_mapped
    )
  ]
  
  # Step 4: Clean column names
  setnames(fa_expanded, 
           grep("^x\\.", names(fa_expanded), value = TRUE),
           gsub("^x\\.", "", grep("^x\\.", names(fa_expanded), value = TRUE)))
  setnames(fa_expanded,
           grep("^i\\.", names(fa_expanded), value = TRUE), 
           gsub("^i\\.", "", grep("^i\\.", names(fa_expanded), value = TRUE)))
  
  # Step 5: Add derived columns - all expanded months have coverage by definition
  fa_expanded[, `:=`(
    premium_prepaid = PREMIUM_PREPAID,
    proof_on_file = PROOF_OF_FA,
    fp_corr_met = FP_CORR_MET,
    tp_fa_met = TP_FA_MET,
    uses_private = USE_PRIVATE_mapped,
    uses_self = USE_SELF_mapped,
    fr_covered = TRUE  # All expanded contract months have coverage
  )]
  
  # Step 6: Calculate ERP periods for each contract
  # Note: This marks months AFTER contract expiry that still have ERP coverage
  fa_expanded[, erp_reporting_month := 
    between(
      make_date(YEAR, MONTH, 1),
      EXP_DATE + months(1),
      EXP_DATE + months(6)
    ),
    by = .(FACILITY_ID, FIN_ASSUR_ID)]
  
  # Clean up temporary columns
  fa_expanded[, month_date := NULL]
  
  return(fa_expanded)
  
} # end of optimize_monthly_expansion function


# ──────────────────────────────────────────────────────────────────────────────
# EXTRACTED FUNCTION: collapse_logic() - UPDATED
# ──────────────────────────────────────────────────────────────────────────────
# Purpose: Collapse contract-level rows to one facility-month row with aggregates
# Input: dt = all contracts for ONE facility in ONE year-month (data.table)
# Output: single-row data.table with aggregated flags + coverage amounts
# Note: Gap detection happens AFTER merging into complete panel, not here
# ──────────────────────────────────────────────────────────────────────────────

collapse_logic <- function(dt) {
  dt <- data.table::copy(dt)        # unlock .SD
  n  <- nrow(dt)                    # how many contracts this month?

  ## ---- group-level aggregates used in every branch -------------------------
  agg <- list(
    DETAIL_TYPE         = paste(sort(unique(dt$DETAIL_TYPE)), collapse = "; "),
    CATEGORY            = paste(sort(unique(dt$CATEGORY)),    collapse = "; "),
    ISSUER_NAME         = paste(sort(unique(dt$ISSUER_NAME)), collapse = " | "),
    max_COVER_OCC       = if (all(is.na(dt$COVER_OCC))) NA_real_
                          else max(dt$COVER_OCC, na.rm = TRUE),
    max_COVER_AGG       = if (all(is.na(dt$COVER_AGG))) NA_real_
                          else max(dt$COVER_AGG, na.rm = TRUE),
    total_COVER_OCC     = sum(dt$COVER_OCC, na.rm = TRUE),
    total_COVER_AGG     = sum(dt$COVER_AGG, na.rm = TRUE),
    premium_prepaid     = any(dt$premium_prepaid, na.rm = TRUE),
    proof_on_file       = any(dt$proof_on_file,   na.rm = TRUE),
    fp_corr_met         = any(dt$fp_corr_met,     na.rm = TRUE),
    tp_fa_met           = any(dt$tp_fa_met,       na.rm = TRUE),
    uses_private        = any(dt$uses_private,    na.rm = TRUE),
    uses_self           = any(dt$uses_self,       na.rm = TRUE)
    # REMOVED: coverage_gap_month - this is determined at panel level
  )

  # Branch 1: single contract
  if (n == 1L) {
    row <- dt[1L]
    row[, `:=`(
      transition_month    = FALSE,
      multiple_contracts  = FALSE,
      active_FIN_ASSUR_ID = FIN_ASSUR_ID,
      fr_covered          = TRUE,  # All expanded contract months have coverage by definition
      DETAIL_TYPE         = agg$DETAIL_TYPE,
      CATEGORY            = agg$CATEGORY,
      ISSUER_NAME         = agg$ISSUER_NAME,
      max_COVER_OCC       = agg$max_COVER_OCC,
      max_COVER_AGG       = agg$max_COVER_AGG,
      total_COVER_OCC     = agg$total_COVER_OCC,
      total_COVER_AGG     = agg$total_COVER_AGG,
      premium_prepaid     = agg$premium_prepaid,
      proof_on_file       = agg$proof_on_file,
      fp_corr_met         = agg$fp_corr_met,
      tp_fa_met           = agg$tp_fa_met,
      uses_private        = agg$uses_private,
      uses_self           = agg$uses_self
    )]
    return(row[])
  }

  # Branch 2: exactly two contracts, NO overlap (clean hand-off)
  ord  <- order(dt$start_day_in_month)
  sNum <- as.numeric(dt$start_day_in_month[ord])
  eNum <- as.numeric(dt$end_day_in_month[ord])
  overlaps <- any(na.omit(sNum[-1] <= cummax(eNum)[-n]))

  if (n == 2L && !overlaps) {
    idx <- which.max(dt$start_day_in_month)
    row <- dt[idx]
    row[, `:=`(
      transition_month    = TRUE,
      multiple_contracts  = FALSE,
      active_FIN_ASSUR_ID = FIN_ASSUR_ID,
      fr_covered          = TRUE,  # All expanded contract months have coverage by definition
      DETAIL_TYPE         = agg$DETAIL_TYPE,
      CATEGORY            = agg$CATEGORY,
      ISSUER_NAME         = agg$ISSUER_NAME,
      max_COVER_OCC       = agg$max_COVER_OCC,
      max_COVER_AGG       = agg$max_COVER_AGG,
      total_COVER_OCC     = agg$total_COVER_OCC,
      total_COVER_AGG     = agg$total_COVER_AGG,
      premium_prepaid     = agg$premium_prepaid,
      proof_on_file       = agg$proof_on_file,
      fp_corr_met         = agg$fp_corr_met,
      tp_fa_met           = agg$tp_fa_met,
      uses_private        = agg$uses_private,
      uses_self           = agg$uses_self
    )]
    return(row[])
  }

  # Branch 3: ≥2 overlapping contracts
  row <- dt[1L]
  row[, `:=`(
    transition_month    = FALSE,
    multiple_contracts  = TRUE,
    active_FIN_ASSUR_ID = paste(sort(unique(dt$FIN_ASSUR_ID)), collapse = " | "),
    fr_covered          = TRUE,  # All expanded contract months have coverage by definition
    DETAIL_TYPE         = agg$DETAIL_TYPE,
    CATEGORY            = agg$CATEGORY,
    ISSUER_NAME         = agg$ISSUER_NAME,
    max_COVER_OCC       = agg$max_COVER_OCC,
    max_COVER_AGG       = agg$max_COVER_AGG,
    total_COVER_OCC     = agg$total_COVER_OCC,
    total_COVER_AGG     = agg$total_COVER_AGG,
    premium_prepaid     = agg$premium_prepaid,
    proof_on_file       = agg$proof_on_file,
    fp_corr_met         = agg$fp_corr_met,
    tp_fa_met           = agg$tp_fa_met,
    uses_private        = agg$uses_private,
    uses_self           = agg$uses_self
  )]
  return(row[])
} # end of collapse_logic function


# ──  Functional version of malformed date fixing  ───────────────────────────
fix_malformed_fa_dates <- function(dt, eff_date_col = "EFF_DATE", exp_date_col = "EXP_DATE") {
  # Helper function: Vector-safe fixer for malformed years like "02 01" → 2001, "007" → 2007
  fix_year <- function(d) {
    # keep original class & NA's
    out <- d                       
    bad <- !is.na(out) & (year(out) < 1900 | year(out) > 2100)
    
    if (any(bad)) {
      y_bad <- year(out[bad])
      out[bad] <- as.Date(sprintf(
        "%04d-%02d-%02d",
        y_bad %% 100 + 2000,   # 0201 → 2001, 0022 → 2022, etc.
        month(out[bad]),
        mday(out[bad])
      ))
    }
    out
  }
  
  # Create working copy to avoid modifying original
  dt_work <- copy(dt)
  
  # Ensure date columns are Date objects, not character
  # Check if columns are Date class, if not convert them
  if (!inherits(dt_work[[eff_date_col]], "Date")) {
    warning(paste("Converting", eff_date_col, "from", class(dt_work[[eff_date_col]]), "to Date"))
    dt_work[, (eff_date_col) := as.Date(get(eff_date_col))]
  }
  
  if (!inherits(dt_work[[exp_date_col]], "Date")) {
    warning(paste("Converting", exp_date_col, "from", class(dt_work[[exp_date_col]]), "to Date"))
    dt_work[, (exp_date_col) := as.Date(get(exp_date_col))]
  }
  
  # 1. Tag rows with weird 2-digit or 5-digit years
  dt_work[, `:=`(
    bad_eff = !is.na(get(eff_date_col)) & 
              (year(get(eff_date_col)) < 1900 | year(get(eff_date_col)) > 2100),
    bad_exp = !is.na(get(exp_date_col)) & 
              (year(get(exp_date_col)) < 1900 | year(get(exp_date_col)) > 2100)
  )]
  
  # Count original bad dates for summary
  n_originally_bad <- dt_work[bad_eff == TRUE | bad_exp == TRUE, .N]
  
  # 2. If only one of the two dates is bad, assume a one-year term
  dt_work[bad_eff == TRUE & bad_exp == FALSE,
          (eff_date_col) := get(exp_date_col) - years(1L)]
  
  dt_work[bad_exp == TRUE & bad_eff == FALSE,
          (exp_date_col) := get(eff_date_col) + years(1L)]
  
  # 3. Apply year fixing to remaining bad dates
  dt_work[bad_eff == TRUE, 
          (eff_date_col) := fix_year(get(eff_date_col))]
  
  dt_work[bad_exp == TRUE, 
          (exp_date_col) := fix_year(get(exp_date_col))]
  
  # 4. Flag anything we still couldn't rescue for manual review
  dt_work[, date_fix_flag := (bad_eff | bad_exp) & 
                             (is.na(get(eff_date_col)) | is.na(get(exp_date_col)))]
  
  # 5. Clean up helper columns
  dt_work[, c("bad_eff", "bad_exp") := NULL]
  
  # Return summary info
  n_still_bad <- dt_work[date_fix_flag == TRUE, .N]
  n_fixed <- n_originally_bad - n_still_bad
  
  cat("Date fixing summary:\n")
  cat("  Rows with malformed dates originally:", n_originally_bad, "\n")
  cat("  Rows with malformed dates fixed:", n_fixed, "\n")
  cat("  Rows still flagged for manual review:", n_still_bad, "\n")
  
  return(dt_work)
} # end of fix_malformed_fa_dates function

# ── Section 1: Facility extract ─────────────────────────────────────────────
# Purpose: Download, parse, and clean the PST facility file to obtain static facility-level data.
# Why: Provides the universe of facilities and basic site attributes (county, ZIP, status) for later merges.
# End product: `fac` data.table keyed by FACILITY_ID, with type-cast fields and blank indicators.

message("Processing Section 1: Facility extract...")

# Define fixed-width layout (379 bytes, 54 columns) based on PST spec
fac_widths <- c(
   8,15,6,60,30,10,30,1,1,1,
   4,4,50,30,2,5,4,256,35,35,
   2,5,15,15,28,60,100,50,50,30,
   2,5,4,3,7,5,3,7,5,50,
   1,10,10,15,15,28,60,35,100,1,
   10,1,30,30
)

fac_names <- c(
  "FACILITY_ID","ADD_ID","AI","FACILITY_NAME","FACILITY_TYPE",
  "FACILITY_BEGIN_DATE","FACILITY_STATUS","FACILITY_EXEMPT_STATUS",
  "RECORDS_OFF_SITE","UST_FR_REQUIRED","NUM_ACTIVE_USTS","NUM_ACTIVE_ASTS",
  "SITE_ADDRESS_DELIVERY","SITE_CITY","SITE_STATE_CODE","SITE_ZIP",
  "SITE_ZIP_EXT","SITE_LOCATION_DESC","SITE_NEAREST_CITY","SITE_COUNTY",
  "SITE_REGION","SITE_LOCATION_ZIP","CONTACT_FN","CONTACT_MI","CONTACT_LN",
  "CONTACT_TITLE","CONTACT_ORG","CONTACT_ADDR_DELIV","CONTACT_ADDR_INTDELIV",
  "CONTACT_ADDR_CITY","CONTACT_ADDR_STATE","CONTACT_ADDR_ZIP",
  "CONTACT_ADDR_ZIP_EXT","CONTACT_PHONE_AC","CONTACT_PHONE_NUM",
  "CONTACT_PHONE_EXT","CONTACT_FAX_AC","CONTACT_FAX_NUM","CONTACT_FAX_EXT",
  "CONTACT_EMAIL","CONTACT_ADDR_DELIVERABLE","APP_REC_DATE","SIG_DATE",
  "SIG_FN","SIG_MI","SIG_LN","SIG_TITLE","SIG_ROLE","SIG_COMPANY",
  "ENFORCEMENT_ACTION","ENFORCEMENT_ACTION_DATE",
  "FACILITY_NOT_INSPECTABLE","NOT_INSPECTABLE_REASON_1",
  "NOT_INSPECTABLE_REASON_2"
)

# Sanity checks
stopifnot(length(fac_widths) == length(fac_names))
stopifnot(sum(fac_widths) == 1379)

# Load and parse
fac <- load_and_process_data(
  paste0(base, "/pst_fac.txt"), fac_widths, fac_names
)

# Persist raw load (under Data/Raw/state_databases/Texas)
saveRDS(fac, file = here("Data", "Raw", "state_databases", "Texas", "raw_pst_fac.rds"))
fwrite(fac, file = here("Data", "Raw", "state_databases", "Texas", "raw_pst_fac.csv"), row.names = FALSE)
# Clean key and flag blanks
fac <- clean_id_column(fac, "FACILITY_ID")

fac[, `:=`(
  NUM_ACTIVE_USTS_is_blank = trimws(NUM_ACTIVE_USTS) == "",
  NUM_ACTIVE_ASTS_is_blank = trimws(NUM_ACTIVE_ASTS) == ""
)]

# Rename long fields for consistency
rename_map <- c(
  FACILITY_EXEMPT_STATUS = "FACILITY_EXEMPT",
  RECORDS_OFF_SITE       = "OFFSITE"
)
present <- intersect(names(rename_map), names(fac))
if (length(present)) setnames(fac, present, rename_map[present])

# Type conversions
fac[, FACILITY_BEGIN_DATE := lubridate::mdy(trimws(FACILITY_BEGIN_DATE))]
fac[, `:=`(
  UST_FR_REQUIRED   = UST_FR_REQUIRED   == "Y",
  FACILITY_EXEMPT   = FACILITY_EXEMPT   == "Y",
  OFFSITE           = OFFSITE           == "Y",
  NUM_ACTIVE_USTS   = as.integer(NUM_ACTIVE_USTS),
  NUM_ACTIVE_ASTS   = as.integer(NUM_ACTIVE_ASTS),
  FACILITY_BEGIN_YR = year(FACILITY_BEGIN_DATE)
)]

# Keep only FR-required facilities


# Key for merges
setkey(fac, FACILITY_ID)



message("Section 1 complete: ", nrow(fac), " facilities loaded.")



# ── Section 2: Financial-Assurance extract & monthly / facility-month collapse ─
# Purpose: read raw FA records, fix malformed dates, expand each contract to its active months,
#   then collapse to one row per facility-month summarizing coverage, gaps, ERPs, and change flags.
# Why: to capture each facility’s insurance/self-insurance status over time.
# End product:
#   • `fa_monthly_contract` (contract-level × month series; key FACILITY_ID, FIN_ASSUR_ID, YEAR, MONTH)
#   • `fa_monthly` (facility × month summary; key FACILITY_ID, YEAR, MONTH)
# Usage: merged into main `panel` by FACILITY_ID, YEAR, MONTH in Section 7.
message("Processing Section 2: Financial-Assurance extract...")
# Updated based on the provided data dictionary for FINANCIAL ASSURANCE table
# ── Financial-Assurance column names ──────────────────────────────────────────

# Column widths for the PST Financial-Assurance extract
fa_widths <- c(
  8,  # FINANCIAL ASSURANCE ID
  8,  # FACILITY ID (padded)
  6,  # Facility ID (AI)
  10, # Form Received Date
  30, # Mechanism Type
  30, # Mechanism Type Other
  1,  # Multiple Mechanism Types flag
  50, # Issuer Name
  5,  # Issuer Phone – country code
  3,  # Issuer Phone – area code
  7,  # Issuer Phone Number
  5,  # Issuer Phone extension
  30, # Policy or Mechanism Number
  10, # Coverage Effective Begin Date
  10, # Coverage Expiration Date
  30, # Coverage Amount per Occurrence
  30, # Coverage Amount per Annual Aggregate
  1,  # Insurance Premium Pre-Paid flag
  1,  # First-Party Corrective-Action Met flag
  1,  # Third-Party FA Met flag
  1,  # Proof of Financial Assurance flag
  1   # Meets FA Requirements flag
)

# Sanity-check: sum should be 278 bytes
stopifnot(sum(fa_widths) == 278)


fa_names_core <- c(
  "FIN_ASSUR_ID",        # 1   Financial Assurance ID
  "FACILITY_ID",         # 2   Facility ID (Foreign Key)
  "FACILITY_AI",         # 3   Facility AI number
  "FORM_REC_DATE",       # 4   Form Received Date
  "MECH_TYPE",           # 5   Mechanism Type
  "MECH_TYPE_OTHER",     # 6   Mechanism Type – Other
  "MULTI_MECH_TYPES",    # 7   Multiple Mechanism Types flag (Y/N)
  "ISSUER_NAME",         # 8   Issuer Name
  "ISSUER_PHONE_CTRY",   # 9   Issuer Phone – country code
  "ISSUER_PHONE_AREA",   # 10  Issuer Phone – area code
  "ISSUER_PHONE_NUM",    # 11  Issuer Phone – number
  "ISSUER_PHONE_EXT",    # 12  Issuer Phone – extension
  "POLICY_MECH_NUM",     # 13  Policy or Mechanism Number
  "COVER_EFF",           # 14  Coverage Effective Begin Date
  "COVER_EXP",           # 15  Coverage Expiration Date
  "COVER_OCC",           # 16  Coverage Amount per Occurrence
  "COVER_AGG"            # 17  Coverage Amount per Annual Aggregate
)

fa_names <- c(
  fa_names_core,
  "PREMIUM_PREPAID",     # 18  Insurance Premium Pre-Paid flag
  "FP_CORR_MET",         # 19  First-Party Corrective-Action Met flag
  "TP_FA_MET",           # 20  Third-Party Financial-Assurance Met flag
  "PROOF_OF_FA",         # 21  Proof-of-Financial-Assurance flag
  "MEETS_FLAG"           # 22  Meets Financial-Assurance Requirements flag
)



# Load and process FA data
fa <- load_and_process_data(
  paste0(base,"/pst_fin_assur.txt"), fa_widths, fa_names)

setDT(fa)

# Convert flags and clean data  
fa[, PREMIUM_PREPAID := PREMIUM_PREPAID == "Y"]
fa[, FP_CORR_MET     := FP_CORR_MET     == "Y"]
fa[, TP_FA_MET       := TP_FA_MET       == "Y"]
fa[, PROOF_OF_FA     := PROOF_OF_FA     == "Y"]

saveRDS(fa, file = here("Data", "Raw", "state_databases", "Texas", "raw_pst_fin_assur.rds"))
fa = clean_merge_keys(fa, "FACILITY_ID")

# Process coverage amounts and dates
fa[, `:=`(
  COVER_OCC_is_blank = trimws(COVER_OCC) == "",
  COVER_AGG_is_blank = trimws(COVER_AGG) == "",
  FACILITY_ID = trimws(substr(FACILITY_ID,1,6)),
  EFF_DATE = mdy(COVER_EFF),
  EXP_DATE = mdy(COVER_EXP),
  MULTI_MECH_TYPES = MULTI_MECH_TYPES == "Y",
  MEETS_FLAG = MEETS_FLAG == "Y"
)]


# 1. strip non-digits (e.g. commas), turn blank→NA
fa[, COVER_OCC_clean := na_if(trimws(gsub("[^0-9\\.]", "", COVER_OCC)), "")]
fa[, COVER_AGG_clean := na_if(trimws(gsub("[^0-9\\.]", "", COVER_AGG)), "")]

# 2. numeric coercion, safe
fa[, COVER_OCC := as.numeric(COVER_OCC_clean)]
fa[, COVER_AGG := as.numeric(COVER_AGG_clean)]

# 3. drop the helpers
fa[, c("COVER_OCC_clean", "COVER_AGG_clean") := NULL]

# Make sure dates are parsed before calling fix_malformed_fa_dates

# Fix malformed dates
fa <- fix_malformed_fa_dates(fa, "EFF_DATE", "EXP_DATE")

# Check results
bad_now <- fa[date_fix_flag == TRUE]
cat("🔎 After automatic repair, rows still bad:", nrow(bad_now), "\n")

# Apply mechanism type lookup table
fa[, MECH_TYPE_upper := toupper(trimws(MECH_TYPE))]



# Create lookup table
mech_type_lookup <- data.table(
  MECH_TYPE_upper = c(
    "INSURANCE OR RISK RETENTION", "FINANCIAL TEST", "LOCAL GOV FIN TEST",
    "LOCAL GOV GUARANTEE", "GUARANTEE", "LETTER OF CREDIT", "SURETY BOND",
    "TRUST FUND", "OTHER", ""
  ),
  DETAIL_TYPE = c(
    "Insurance Policy", "Self-Insurance", "Government Guarantee", 
    "Government Guarantee", "Self-Insurance", "Self-Insurance", "Self-Insurance",
    "Self-Insurance", "Other or Unspecified", "Other or Unspecified"
  ),
  CATEGORY = c(
    "Insurance", "Self-Insurance", "Government Guarantee",
    "Government Guarantee", "Self-Insurance", "Self-Insurance", "Self-Insurance",
    "Self-Insurance", "Other or Unspecified", "Other or Unspecified"
  )
)

# Merge lookup table
fa <- merge(fa, mech_type_lookup, by = "MECH_TYPE_upper", all.x = TRUE)

# Handle PST Remediation Fund special cases
fa[MECH_TYPE_upper == "TRUST FUND" & 
   !is.na(MECH_TYPE_OTHER) & 
   grepl("PST REMEDIATION FUND", MECH_TYPE_OTHER, ignore.case = TRUE), 
   `:=`(DETAIL_TYPE = "State Insurance", CATEGORY = "State Fund")]

fa[MECH_TYPE_upper == "OTHER" & 
   !is.na(MECH_TYPE_OTHER) & 
   grepl("PST REMEDIATION FUND", MECH_TYPE_OTHER, ignore.case = TRUE), 
   `:=`(DETAIL_TYPE = "State Insurance", CATEGORY = "State Fund")]

# Handle unmatched rows
fa[is.na(DETAIL_TYPE), DETAIL_TYPE := "Other or Unspecified"]
fa[is.na(CATEGORY), CATEGORY := "Other or Unspecified"]

# Create convenience flags
fa[, USE_PRIVATE := CATEGORY == "Insurance"]
fa[, USE_SELF    := CATEGORY == "Self-Insurance"]

# Create mechanism dummies
mech_levels <- mech_type_lookup$MECH_TYPE_upper
dummy_names <- paste0("MECH_", gsub("[^A-Z0-9]", "_", mech_levels))

fa[, (dummy_names) := lapply(
       mech_levels,
       function(lvl) as.integer(MECH_TYPE_upper == lvl)
     )]

# Clean up and set mapped flags
fa[, MECH_TYPE_upper := NULL]
fa[, `:=`(USE_PRIVATE_mapped = USE_PRIVATE,
          USE_SELF_mapped    = USE_SELF)]

# Simple monthly expansion - each contract becomes monthly rows
fa_monthly_contract <- fa[
  !is.na(EFF_DATE) & !is.na(EXP_DATE) & EFF_DATE <= EXP_DATE,
  {
    mo_seq <- seq(
      floor_date(EFF_DATE, "month"),
      floor_date(pmin(EXP_DATE, as.Date("2026-12-01")), "month"),
      by = "month"
    )
     # Calculate start/end days within each month
    month_starts <- floor_date(mo_seq, "month")
    month_ends <- ceiling_date(mo_seq, "month") - days(1)
    
    .(YEAR = year(mo_seq),
      MONTH = month(mo_seq),
      EFF_DATE = EFF_DATE,
      EXP_DATE = EXP_DATE,
      # Add the missing columns for collapse_logic
      start_day_in_month = pmax(EFF_DATE, month_starts),
      end_day_in_month = pmin(EXP_DATE, month_ends),
      DETAIL_TYPE = DETAIL_TYPE,
      CATEGORY = CATEGORY,
      ISSUER_NAME = ISSUER_NAME,
      COVER_OCC = COVER_OCC,
      COVER_AGG = COVER_AGG,
      premium_prepaid = PREMIUM_PREPAID,
      proof_on_file = PROOF_OF_FA,
      fp_corr_met = FP_CORR_MET,
      tp_fa_met = TP_FA_MET,
      uses_private = USE_PRIVATE_mapped,
      uses_self = USE_SELF_mapped,
      fr_covered = TRUE)
  },
  by = .(FACILITY_ID, FIN_ASSUR_ID)
]

# Collapse to facility-month
fa_monthly <- fa_monthly_contract[, collapse_logic(.SD), by = .(FACILITY_ID, YEAR, MONTH)]

# Create dummy-encoded columns for categorical variables
dummy_cols  <- c("ISSUER_NAME", "CATEGORY", "DETAIL_TYPE")


dummy_long <- melt(
  fa_monthly,
  id.vars      = c("FACILITY_ID", "YEAR", "MONTH"),
  measure.vars = dummy_cols,
  variable.name = "var",
  value.name    = "val"
)[!is.na(val) & val != ""]

fa_dummies <- dcast(
  dummy_long,
  FACILITY_ID + YEAR + MONTH ~ paste0(var, "_", gsub("[^A-Za-z0-9]", "_", val)),
  fun.aggregate = length,
  value.var     = "val"
)

# Merge dummies back
fa_monthly <- merge(
  fa_monthly,
  fa_dummies,
  by = c("FACILITY_ID", "YEAR", "MONTH"),
  all.x = TRUE
)

# Convert NA dummies to 0
dummy_cols <- names(fa_dummies)[-(1:3)]
fa_monthly[, (dummy_cols) := lapply(.SD, function(x) {
    x[is.na(x)] <- 0L
    x
  }), .SDcols = dummy_cols]

# Create Zurich 2012 lookup
fa_monthly_contract[, is_zurich := grepl(
  "ZURICH|ZURICH AMERICAN INSURANCE COMPANY",
  toupper(trimws(ISSUER_NAME)),
  ignore.case = TRUE
)]
fwrite(fa_monthly_contract, here("Data", "Processed", "fa_monthly_contracts.csv"))


zurich_2012_lookup <- fa_monthly_contract[
  YEAR == 2012 & is_zurich == TRUE,
  .(had_zurich_2012 = 1L),
  by = FACILITY_ID]

zurich_2012_lookup <- unique(zurich_2012_lookup, by = "FACILITY_ID")
fwrite(zurich_2012_lookup, here("Data", "Processed", "zurich_2012_lookup.csv"))
fwrite(fa_monthly, here("Data", "Processed", "fa_monthly.csv"))

# Sanity check
stopifnot(anyDuplicated(fa_monthly[, .(FACILITY_ID, YEAR, MONTH)]) == 0)

message("Section 2: Financial-Assurance extract complete.")

# Save processed data
fa_processed <- fa[, .(
  FIN_ASSUR_ID, FACILITY_ID, FORM_REC_DATE, EFF_DATE, EXP_DATE,
  MECH_TYPE, MECH_TYPE_OTHER, DETAIL_TYPE, CATEGORY, MULTI_MECH_TYPES,
  MEETS_FLAG, ISSUER_NAME, POLICY_MECH_NUM, COVER_OCC, COVER_AGG, 
  COVER_OCC_is_blank, COVER_AGG_is_blank, USE_PRIVATE_mapped, USE_SELF_mapped
)]

setnames(fa_processed, 
         c("FORM_REC_DATE", "MECH_TYPE_OTHER", "MULTI_MECH_TYPES", "POLICY_MECH_NUM",
           "USE_PRIVATE_mapped", "USE_SELF_mapped"), 
         c("FORM_DATE", "MECH_TYPE_OTHR", "MULTI_MECH", "POLICY_NUM",
           "USE_PRIVATE", "USE_SELF"), skip_absent = TRUE)

fwrite(fa_processed, here("Data", "Processed", "texas_fr_processed.csv"))
saveRDS(fa_processed, here("Data", "Processed", "texas_fr_processed.rds"))

setkey(fa_monthly, FACILITY_ID, YEAR, MONTH)

#quick graph for records request
## plot the number of unique faciliyt ids by year of fa_monthly
## make the graph clear and easy to read for records request/questions

library(ggplot2)
faciilyts_FR_by_year = fa_monthly[,.(
  unique_facilities = uniqueN(FACILITY_ID)
), by = YEAR]

#full year span 1990-2025
fa_year = data.table(YEAR =  seq(1990, 2025))
fa_year <- merge(fa_year, faciilyts_FR_by_year, by = "YEAR", all.x = TRUE)
fa_year[is.na(unique_facilities), unique_facilities := 0]

library(ggplot2)

#— Parameters for the shaded gap —#
gap_xmin <- 2000
gap_xmax <- 2006.99   # 2006-12-31
gap_label_y <- max(fa_year$unique_facilities, na.rm = TRUE) * 0.92

ggplot(fa_year, aes(x = YEAR, y = unique_facilities)) +
  # 1. grey band for missing years
  annotate(
    "rect",
    xmin = gap_xmin, xmax = gap_xmax,
    ymin = -Inf,   ymax =  Inf,
    fill = "grey85", alpha = 0.5
  ) +
  # (optional) mark the gap with text
  annotate(
    "text",
    x = (gap_xmin + gap_xmax) / 2,
    y = gap_label_y,
    label = "No data\n2000–2006",
    colour = "grey30",
    size = 3.5,
    lineheight = 0.9
  ) +
  # 2. the time-series itself
  geom_line(size = 0.8, colour = "#283747") +
  
  # 3. clearer labelling
  labs(
    title   = "Financial-Assurance Coverage by Year",
    subtitle = "Texas PST facilities, fiscal years 1988–2024",
    x       = "Fiscal year (ending September 30)",
    y       = "Number of facilities with\nfinancial-assurance records",
    caption = "Source: Texas Commission on Environmental Quality (PST registration files)"
  ) +
  
  # 4. academic-style theme tweaks
  scale_x_continuous(
    breaks = seq(min(fa_year$YEAR), max(fa_year$YEAR), by = 5),
    minor_breaks = NULL,
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.05))
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title      = element_text(face = "bold", hjust = 0.5, size = 14),
    plot.subtitle   = element_text(hjust = 0.5, margin = margin(b = 8)),
    axis.title.x    = element_text(margin = margin(t = 8)),
    axis.title.y    = element_text(margin = margin(r = 8)),
    panel.grid.minor = element_blank()
  )




# ── Section 3: Owner/Operator timelines ──────────────────────────────────────
# Purpose: load PST owner & operator extracts, build intervals, expand to months,
#   and aggregate per facility-month into concatenated ID/name/type strings.
# Why: to know who owns/operates each facility in each month, and detect changes/orphans.
# End product: `fac_contact_months` keyed by FACILITY_ID, YEAR, MONTH
# Usage: merged into `panel` by FACILITY_ID, YEAR, MONTH in Section 7.

library(data.table)
library(lubridate)

message("Processing Section 3: Owner & Operator timelines…")

# ───────────────────────── user-tunable ──────────────────────────────────────
analysis_end   <- as.Date("2026-12-01")   
analysis_start <- as.Date("1990-01-01")             # earliest month kept
# ─────────────────────────────────────────────────────────────────────────────
# ── libraries ────────────────────────────────────────────────────────────
library(data.table)
library(lubridate)

# ── generic contact-file loader built on load_and_process_data ───────────
read_contact <- function(file,
                         widths,
                         names,
                         date_col) {

  dt <- load_and_process_data(paste0(base, "/", file), widths, names)

  # light post-processing --------------------------------------------------
  set(dt, j = date_col,     value = as.IDate(dt[[date_col]], "%m/%d/%Y"))
  set(dt, j = "FACILITY_ID", value = trimws(dt$FACILITY_ID))

  unique(dt[])
}
# ── OWNER spec (record length = 682) ─────────────────────────────────────
owner_widths <- c(
  15,15,6,11,100,15,15,10,2,15, 10,15,15,28,60,100,
  50,50,30,2,5,4,15,3,5,3,7,5,5,3,7,5,50,1
)

owner_names <- c(
  "PRINC_ID","ADD_ID","FACILITY_ID","OWNER_CN",
  "OWNER_NAME_CO_LAST","OWNER_NAME_FIRST","OWNER_NAME_MIDDLE",
  "OWNER_EFF_BEGIN_DT","OWNER_TYPE","STATE_TAX_ID","CONTACT_ROLE",
  "CONTACT_FIRST_NM","CONTACT_MIDDLE_NM","CONTACT_LAST_NM",
  "CONTACT_TITLE","CONTACT_ORG_NM","MAIL_ADDR_DELIVERY",
  "MAIL_ADDR_INTERNAL","MAIL_CITY","MAIL_STATE","MAIL_ZIP",
  "MAIL_ZIP_PLUS4","MAIL_FOREIGN_POSTAL","MAIL_COUNTRY",
  "PHONE_COUNTRY","PHONE_AREA","PHONE_NUM","PHONE_EXT",
  "FAX_COUNTRY","FAX_AREA","FAX_NUM","FAX_EXT","EMAIL",
  "ADDR_DELIVERABLE_FLAG"
)

owners_raw <- read_contact(
  file     = "pst_own.txt",
  widths   = owner_widths,
  names    = owner_names,
  date_col = "OWNER_EFF_BEGIN_DT"
)

# ── OPERATOR spec (record length = 667) ──────────────────────────────────
operator_widths <- c(
  15,15,6,11,100,15,15,10,2,10, 15,15,28,60,100,
  50,50,30,2,5,4,15,3,5,3,7,5,5,3,7,5,50,1
)

operator_names <- c(
  "PRINC_ID","ADD_ID","FACILITY_ID","OPERATOR_CN",
  "OPERATOR_NAME_CO_LAST","OPERATOR_NAME_FIRST","OPERATOR_NAME_MIDDLE",
  "OPERATOR_EFF_BEGIN_DT","OPERATOR_TYPE","CONTACT_ROLE",
  "CONTACT_FIRST_NM","CONTACT_MIDDLE_NM","CONTACT_LAST_NM",
  "CONTACT_TITLE","CONTACT_ORG_NM","MAIL_ADDR_DELIVERY",
  "MAIL_ADDR_INTERNAL","MAIL_CITY","MAIL_STATE","MAIL_ZIP",
  "MAIL_ZIP_PLUS4","MAIL_FOREIGN_POSTAL","MAIL_COUNTRY",
  "PHONE_COUNTRY","PHONE_AREA","PHONE_NUM","PHONE_EXT",
  "FAX_COUNTRY","FAX_AREA","FAX_NUM","FAX_EXT","EMAIL",
  "ADDR_DELIVERABLE_FLAG"
)

operators_raw <- read_contact(
  file     = "pst_opr.txt",          # adjust if your filename differs
  widths   = operator_widths,
  names    = operator_names,
  date_col = "OPERATOR_EFF_BEGIN_DT"
)


view(operators_raw)

view(owners_raw)



# ───────────────── SECTION 3 : Owner / Operator monthly flags ───────────────
# assumes owners_raw, operators_raw, analysis_start, analysis_end already exist

## 1 ─ Build closed-open ownership & operator intervals -----------------------
owners_int <- owners_raw[
  order(FACILITY_ID, OWNER_EFF_BEGIN_DT)
][, .(
    OWNER_CN, OWNER_NAME_CO_LAST,
    START_DATE = OWNER_EFF_BEGIN_DT,
    END_DATE   = shift(OWNER_EFF_BEGIN_DT,
                       type = "lead",
                       fill = as.IDate("9999-12-31")) - 1L
  ), by = FACILITY_ID]

operators_int <- operators_raw[
  order(FACILITY_ID, OPERATOR_EFF_BEGIN_DT)
][, .(
    OPERATOR_CN,
    START_DATE = OPERATOR_EFF_BEGIN_DT,
    END_DATE   = shift(OPERATOR_EFF_BEGIN_DT,
                       type = "lead",
                       fill = as.IDate("9999-12-31")) - 1L
  ), by = FACILITY_ID]

setkey(owners_int,    FACILITY_ID, START_DATE, END_DATE)
setkey(operators_int, FACILITY_ID, START_DATE, END_DATE)

## 2 ─ Facility-month skeleton over full study window ------------------------
fac_months <- owners_raw[
  , .(dummy = 1), keyby = FACILITY_ID
][
  , .(panel_date = seq(analysis_start, analysis_end, by = "month")),
  by = FACILITY_ID]

setkey(fac_months, FACILITY_ID, panel_date)

## 3 ─ Non-equi joins to attach current owner(s) & operator(s) ---------------
fac_own <- owners_int[
  fac_months,
  on = .(FACILITY_ID,
         START_DATE <= panel_date,
         END_DATE   >= panel_date),
         .(x.FACILITY_ID,  x.OWNER_CN,i.panel_date,x.START_DATE,x.END_DATE),
  allow.cartesian = TRUE,
  nomatch         = 0L]



fac_opr <- operators_int[
  fac_months,
  on = .(FACILITY_ID,
         START_DATE <= panel_date,
         END_DATE   >= panel_date),
         .(x.FACILITY_ID, i.panel_date, x.OPERATOR_CN,x.START_DATE,x.END_DATE),
  allow.cartesian = TRUE,
  nomatch         = 0L]

#remove all the x. and i. prefixes from all cols

setnames(fac_own, gsub("^x\\.|^i\\.", "", names(fac_own)))
setnames(fac_opr, gsub("^x\\.|^i\\.", "", names(fac_opr)))

## 4 ─ Owner facility counts & multi-facility flag ---------------------------
own_sizes <- fac_own[
  , .(owner_facility_count = uniqueN(FACILITY_ID)),
  by = .(OWNER_CN, panel_date)]

fac_own  <- own_sizes[fac_own, on = .(OWNER_CN, panel_date)]

## 5 ─ Collapse to one row per FACILITY-month --------------------------------
owner_sets <- fac_own[
  , .(
      OWNER_SET              = paste(sort(unique(OWNER_CN)), collapse = ";"),
      owner_facility_count   = max(owner_facility_count),
      multi_fac_owner_flag   = as.integer(any(owner_facility_count > 1))
    ),
  by = .(FACILITY_ID, panel_date)]

operator_sets <- fac_opr[
  , .(OPERATOR_SET = paste(sort(unique(OPERATOR_CN)), collapse = ";")),
  by = .(FACILITY_ID, panel_date)]

## 6 ─ Combine + derive change / difference flags ----------------------------
fac_contact_months <- owner_sets[
  operator_sets, on = .(FACILITY_ID, panel_date)
]

setorder(fac_contact_months, FACILITY_ID, panel_date)

fac_contact_months[
  , ownership_change_flag :=
      as.integer(OWNER_SET != shift(OWNER_SET)),
  by = FACILITY_ID
][is.na(ownership_change_flag), ownership_change_flag := 0L]

fac_contact_months[
  , operator_diff_owner_flag :=
      as.integer(OPERATOR_SET != "" & OPERATOR_SET != OWNER_SET)
]

## 7 ─ Final YEAR / MONTH columns then drop panel_date -----------------------
fac_contact_months[
  , `:=`(YEAR  = year(panel_date),
         MONTH = month(panel_date))
][
  , panel_date := NULL][]   # the [] prints a preview

setcolorder(fac_contact_months,
            c("FACILITY_ID","YEAR","MONTH",
              "multi_fac_owner_flag","owner_facility_count",
              "ownership_change_flag","operator_diff_owner_flag"))

sum(fac_contact_months$multi_fac_owner_flag,na.rm = T) # check how many multi-facility owners

message("Section 3 complete → fac_contact_months ready for panel merge")

# ── Section 4: Self-Certification summary ────────────────────────────────────
# Purpose: read PST self-cert records, map to facility, expand to year’s 12 months,
#   and collapse to flags for FA_FLAG & TECH_FLAG per facility-month.
# Why: to track when facilities self-attest to meeting financial & technical standards.
# End product: `self_cert_monthly` keyed by FACILITY_ID, YEAR, MONTH
# Usage: merged into `panel` by FACILITY_ID, YEAR, MONTH in Section 7.
message("Processing Section 4: Self-Certification summary...")
# Updated based on the provided data dictionary for SELF-CERTIFICATION table
sc_widths <- c(
  8,  # SELF_CERT ID
  8,  # FACILITY ID (Foreign Key) -> FACILITY_ID_PAD
  6,  # Facility ID (AI)
  10, # Signature Date
  50, # Signature Name
  30, # Signature Title (Placeholder)
  30, # Signature Type Role (Placeholder)
  30, # Filing Status (Placeholder)
  1,  # Registration Self-Certification flag -> REG_FLAG
  1,  # Facility Fees Self-Certification flag -> FEE_FLAG
  1,  # Financial Assurance Self-Certification flag -> FA_FLAG
  1,  # Technical Standards Self-Certification flag -> TECH_FLAG
  10  # Delivery Certificate Expiration Date -> DEL_CERT_EXP
)
sc_names  <- c(
  "SC_ID", "FACILITY_ID_PAD", "AI", "SIGN_DATE",
  "SIGN_NAME", "UNUSED_SIGN_TITLE", "UNUSED_SIGN_TYPE_ROLE", "UNUSED_FILING_STATUS",
  "REG_FLAG", "FEE_FLAG", "FA_FLAG", "TECH_FLAG", "DEL_CERT_EXP"
)

self_cert <- load_and_process_data(
  paste0(base,"/pst_self_cert.txt"), sc_widths, sc_names)
saveRDS(self_cert, file = here("Data", "Raw", "state_databases", "Texas", "raw_pst_self_cert.rds"))
clean_merge_keys(self_cert, "FACILITY_ID_PAD")
self_cert[, `:=`(
  FACILITY_ID = trimws(substr(FACILITY_ID_PAD,1,6)),
  SIGN_DATE = mdy(SIGN_DATE),
  FA_FLAG = FA_FLAG == "Y",
  TECH_FLAG = TECH_FLAG == "Y"
)]

# Expand self-certification to monthly level
self_cert[, `:=`(YEAR = year(SIGN_DATE), MONTH = month(SIGN_DATE))]
# Create monthly records for the entire year when certification was signed
self_cert_expanded <- self_cert[!is.na(YEAR), {
  months_in_year <- 1:12
  .(FACILITY_ID = FACILITY_ID, YEAR = YEAR, MONTH = months_in_year, 
    FA_FLAG = FA_FLAG, TECH_FLAG = TECH_FLAG)
}, by = .(SC_ID, SIGN_DATE)]

# Aggregate to facility-year-month level
self_cert_monthly <- self_cert_expanded[, .(
  FA_FLAG = any(FA_FLAG, na.rm = TRUE), 
  TECH_FLAG = any(TECH_FLAG, na.rm = TRUE)
), by = .(FACILITY_ID, YEAR, MONTH)]
message("Section 4: Self-Certification summary complete.")

setkey(self_cert_monthly, FACILITY_ID, YEAR, MONTH)

# ── Section 5: UST & compartment snapshot ───────────────────────────────────
# Purpose: load raw UST byte-map + compartment detection data,
#   clean IDs & dates, derive closure spells, tank wall/material flags, detection flags.
# Why: to get tank-level attributes and life-spans for later monthly aggregation.
# End product: cleaned `ust` (keyed by UST_ID) plus compartment det/class tables.

message("Processing Section 5: UST tank snapshot...")



###############################################################################
# ========== 5 A.  ADD MATERIAL + DETECTION COLUMNS TO THE RAW UST & COMP =====
###############################################################################



# Define UST file structure (bytes 1-201)
ust_widths <- c(
  8,  8,  6, 10,  2, 10, 10,  8, 30, 10,  1, 30, 10, 1, 1,     # 1-145
  rep(1, 44),                                                  # 146-189 1-byte flags
  1,                                                          # 190 TECH_COMPLY
  1,                                                          # 191 TANK_TESTED_FLAG
  10                                                          # 192-201 INSTALL_SIG_DATE
)

ust_names <- c(
  # Core fields (1-145)
  "UST_ID","FACILITY_ID_PAD","AI","TANK_ID","COMPARTS",
  "INSTALL_DATE","REG_DATE","CAPACITY","STATUS","STATUS_DATE",
  "EMPTY","REG_STATUS","TANK_INT_PROT_DATE","TANK_SINGLE","TANK_DOUBLE",
  # Tank/pipe material and configuration flags (146-189)
  "PIP_SINGLE","PIP_DOUBLE",
  "EXT_CONT_JACKET","EXT_CONT_SYN_LNR","EXT_CONT_VAULT",
  "EXT_CONT_PIP_JACKET","EXT_CONT_PIP_SYN_LNR","EXT_CONT_PIP_VAULT",
  "PIPE_TYPE",
  "TANK_MAT_STEEL","TANK_MAT_FRP","TANK_MAT_COMPOSITE","TANK_MAT_CONCRETE",
  "TANK_MAT_JACKETED","TANK_MAT_COATED",
  "PIP_MAT_STEEL","PIP_MAT_FRP","PIP_MAT_CONCRETE","PIP_MAT_JACKETED",
  "PIP_MAT_FLEX",
  "PIP_VALVE_SHEAR","PIP_SWING_JOINT","PIP_FLEX_CONN",
  "CORR_TANK_CP","CORR_PIPE_CP","CORR_TANK_VARIANCE","CORR_PIPE_VARIANCE",
  "TOS_COMPLY",
  paste0("UNUSED_FLAG_", 1:16),
  # Final fields (190-201)
  "TECH_COMPLY","TANK_TESTED_FLAG","INSTALL_SIG_DATE"
)

# Detection method columns
detect_cols <- c(
  # Compartment methods
  "DET_C_VAPOR","DET_C_GW","DET_C_SEC_CONT","DET_C_ATG",
  "DET_C_INTERSTITIAL","DET_C_MANUAL_WEEK","DET_C_MANUAL_MONTH","DET_C_SIR",
  # Piping methods
  "DET_P_VAPOR","DET_P_GW","DET_P_SEC_CONT","DET_P_INTERSTITIAL",
  "DET_P_TT_MONTH","DET_P_TT_ANNUAL","DET_P_TT_TRIEN","DET_P_LLD",
  "DET_P_SIR","DET_P_SUCTION_EXEMPT"
)

# Validation
stopifnot(length(ust_widths) == length(ust_names), sum(ust_widths) == 201)

# Load and process UST data
ust <- load_and_process_data(paste0(base,"/pst_ust.txt"), ust_widths, ust_names)

# Clean and standardize UST_ID (primary key)
ust <- clean_id_column(ust, "UST_ID")

ust[, UST_ID := toupper(UST_ID)]
setkey(ust, UST_ID)

# Verify uniqueness
message("UST_IDs: ", uniqueN(ust$UST_ID), " unique out of ", nrow(ust), " records")
stopifnot(uniqueN(ust$UST_ID) == nrow(ust))

glimpse(ust)

# Process dates and core fields in one operation
ust[, `:=`(
  # Extract facility ID
  FACILITY_ID = trimws(substr(FACILITY_ID_PAD, 1, 6)),
  
  # Parse dates
  INSTALL_DATE = mdy(INSTALL_DATE),
  REG_DATE = mdy(REG_DATE),
  STATUS_DATE = mdy(STATUS_DATE),
  TANK_INT_PROT_DATE = mdy(TANK_INT_PROT_DATE),
  
  # Convert core fields
  CAPACITY = as.numeric(CAPACITY),
  CAPACITY_is_blank = trimws(CAPACITY) == "",
  TANK_SINGLE = TANK_SINGLE == "Y",
  TANK_DOUBLE = TANK_DOUBLE == "Y",
  PIPE_TYPE = factor(PIPE_TYPE, 
                     levels = c("P", "S", "G"), 
                     labels = c("Pressurized", "Suction", "Gravity"))
)]

glimpse(ust)

# Process material flags efficiently
material_cols <- c("TANK_MAT_STEEL", "TANK_MAT_FRP", "TANK_MAT_COMPOSITE", 
                   "TANK_MAT_CONCRETE", "TANK_MAT_JACKETED", "TANK_MAT_COATED")

# Clean material flags and convert to logical
for(col in material_cols) {
  ust[is.na(get(col)) | get(col) == "", (col) := "N"]
}

# Create material type flags
ust[, `:=`(
  is_steel_tank = TANK_MAT_STEEL == "Y",
  is_fiberglass_tank = TANK_MAT_FRP == "Y",
  is_composite_tank = TANK_MAT_COMPOSITE == "Y",
  is_concrete_tank = TANK_MAT_CONCRETE == "Y",
  is_jacketed_tank = TANK_MAT_JACKETED == "Y",
  is_coated_tank = TANK_MAT_COATED == "Y"
)]

# Create unknown material flag
ust[, is_unknown_material_tank := !(is_steel_tank | is_fiberglass_tank | is_composite_tank | 
                                   is_concrete_tank | is_jacketed_tank | is_coated_tank)]

# Create wall type flags (single operation)
ust[, `:=`(
  single_walled = fifelse(TANK_SINGLE == TRUE, 1L, 0L),
  double_walled = fifelse(TANK_DOUBLE == TRUE, 1L, 0L),
  missing_walled = fifelse(is.na(TANK_SINGLE) & is.na(TANK_DOUBLE), 1L, 0L),
  unknown_walled = fifelse(TANK_SINGLE == FALSE & TANK_DOUBLE == FALSE, 1L, 0L)
)]

# Handle NA values in material flags
material_bool_cols <- c("is_steel_tank", "is_fiberglass_tank", "is_composite_tank", 
                        "is_concrete_tank", "is_jacketed_tank", "is_coated_tank", 
                        "is_unknown_material_tank")
for(col in material_bool_cols) {
  ust[is.na(get(col)), (col) := FALSE]
}

# Date imputation and clipping
min_panel_date <- as.Date("1990-01-01")
max_panel_date <- as.Date("2026-12-01")

##------------- Process status and closure dates -------------

#look at status
status_tally = ust[, .(tanks = uniqueN(UST_ID), Facilites = uniqueN(FACILITY_ID)), by = STATUS]
#clean and standardize status
status_tally[, STATUS := toupper(trimws(STATUS))]

# Create closure status flags using status_tally but dont include IN USE
closure_statuses <- status_tally$STATUS[c(1:2,4)]
ust[, is_closed_removed := toupper(trimws(STATUS)) %in% closure_statuses]
closed_ust_tally <- ust[is_closed_removed == TRUE, .(tanks = uniqueN(UST_ID), facilities = uniqueN(FACILITY_ID)), by = STATUS_DATE]

# Impute dates in single operation
ust[, `:=`(
  install_date = fifelse(
    !is.na(INSTALL_DATE),
    INSTALL_DATE,
    fifelse(
      !is.na(REG_DATE),
      REG_DATE,
      min_panel_date
    )
  ),
  end_date          = fifelse(is_closed_removed, STATUS_DATE, max_panel_date),
  end_imputed       = is_closed_removed & is.na(STATUS_DATE)
)]
closed_ust_tally <- ust[is_closed_removed == TRUE, .(tanks = uniqueN(UST_ID), facilities = uniqueN(FACILITY_ID)), by = .(end_date,STATUS_DATE)][order(end_date, STATUS_DATE)]
# Handle missing end dates and ensure logical date ordering
ust[is.na(end_date), end_date := max_panel_date]
nrow(ust)

ust[end_date < install_date, end_date := install_date]





message("USTs before filtering bad dates obs: ", nrow(ust))

# Filter valid tanks
ust <- ust[install_date <= end_date & !is.na(install_date) & !is.na(end_date)]
message("USTs after date processing and filtering: ", nrow(ust))
library(data.table)

#–– Function: run_stage01_validation ––
#' Run Stage 01 validation counts on a UST panel
#'
#' @param ust            data.table of raw UST records with columns:
#'                       INSTALL_DATE, REG_DATE, STATUS_DATE, is_closed_removed, UST_ID, FACILITY_ID
#' @param min_panel_date Date cutoff (e.g. as.Date("1990-01-01"))
#' @param max_panel_date Date cap for end_date imputation (e.g. as.Date("2026-12-01"))
#' @param stage_label    Character label for this run (default "stage_raw_cleaning")
#'
#' @return One‐row data.table with all validation counts and the stage label
run_stage01_validation <- function(ust,
                                   min_panel_date,
                                   max_panel_date,
                                   stage_label = "stage_raw_cleaning") {
  # ensure data.table
  ust <- as.data.table(ust)
  
  # 1) UST‐level counts
  ust_counts <- ust[, .(
    post_1990_usts              = sum(install_date >= min_panel_date),
    pre_1990_usts_active        = sum(install_date <  min_panel_date & end_date >= min_panel_date),
    total_number_of_USTS_in_raw = uniqueN(UST_ID)
  )]
  
  # 2) Facility‐level flags & counts (no double‐counting)
  facility_counts <- ust[, .(
      has_post_1990_install = any(install_date >= min_panel_date),
      has_pre_1990_active   = any(install_date <  min_panel_date & end_date >= min_panel_date)
    ),
    by = FACILITY_ID
  ][, .(
      total_number_of_facilities_in_raw = .N,
      post_1990_facilities              = sum(has_post_1990_install),
      pre_1990_facilities_active        = sum(has_pre_1990_active)
  )]
  
  # 3) True panel facility count
  total_panel_facilities <- ust[
    install_date >= min_panel_date |
    end_date       >= min_panel_date,
    uniqueN(FACILITY_ID)
  ]
  
  # 4) Combine into one summary row
  result <- cbind(ust_counts, facility_counts)[
    , stage := stage_label
  ][
    , number_of_panel_facilities := total_panel_facilities
  ]
  
  # 5) Print messages
  message("UST validation counts after ", stage_label, ":")
  message("  • Stage:                         ", result$stage)
  message("  • Post-1990 USTs:                ", result$post_1990_usts)
  message("  • Pre-1990 USTs still active:    ", result$pre_1990_usts_active)
  message("  • Total USTs in raw data:        ", result$total_number_of_USTS_in_raw)
  message("  • Total facilities in raw data:  ", result$total_number_of_facilities_in_raw)
  message("  • Post-1990 facilities:          ", result$post_1990_facilities)
  message("  • Pre-1990 facilities active:    ", result$pre_1990_facilities_active)
  message("  • Total facilities in panel:     ", result$number_of_panel_facilities)
  
  return(result[])
}

res01 <- run_stage01_validation(
  ust            = ust,
  min_panel_date = as.Date("1990-01-01"),
  max_panel_date = as.Date("2026-12-01"),
  stage_label    = "stage_raw_cleaning"
)




check_end_date = ust[,.(facs = uniqueN(FACILITY_ID), is_closed = sum(is_closed_removed)), by = end_date][order(end_date)]

# Fixed version - captures both sets you described
facilities_active_1990 <- ust[, .(
  has_ust_active_1990_plus = any(
    install_date >= min_panel_date |  # Set 1: Installed 1990 or later
    end_date >= min_panel_date        # Set 2: Still active 1990 or later  
  )
), by = FACILITY_ID][has_ust_active_1990_plus == TRUE]


nrow(facilities_active_1990)
facilities_active_1990 <- clean_id_column(facilities_active_1990, "FACILITY_ID")
message("Facilities with active USTs (1990+): ", nrow(facilities_active_1990))

# Apply early filtering to all datasets
datasets_to_filter <- list(
  list(data = quote(fac), id_col = "FACILITY_ID", name = "Facilities"),
  list(data = quote(fa), id_col = "FACILITY_ID", name = "FA records"),
  list(data = quote(fac_contact_months), id_col = "FACILITY_ID", name = "Contact months"),
  list(data = quote(self_cert), id_col = "FACILITY_ID", name = "Self-cert records"),
  list(data = quote(ust), id_col = "FACILITY_ID", name = "UST records"),
  list(data = quote(texas_compartment), id_col = "facility_id", name = "Compartment records")
)

message("Applying early filtering to all datasets...")
for(dataset_info in datasets_to_filter) {
  if(exists(as.character(dataset_info$data))) {
    dataset <- eval(dataset_info$data)
    original_count <- nrow(dataset)
    if(dataset_info$id_col %in% names(dataset)) {
      filtered_dataset <- dataset[get(dataset_info$id_col) %in% facilities_active_1990$FACILITY_ID]
      assign(as.character(dataset_info$data), filtered_dataset, envir = .GlobalEnv)
      message(dataset_info$name, " filtered from ", original_count, " to ", nrow(filtered_dataset))
    }
  }
}

# If you need to do it again later:
res02 <- run_stage01_validation(
  ust            = ust,      # e.g. after another cleaning step
  min_panel_date = as.Date("1990-01-01"),
  max_panel_date = as.Date("2026-12-01"),
  stage_label    = "stage_after_filter"
)


all_cols <- c("FACILITY_ID", "AI", "FACILITY_NAME", names(fac)[grepl("SITE_", names(fac))])
facility_data_for_record_request = fac[FACILITY_ID %in% facilities_active_1990$FACILITY_ID, 
                                      ..all_cols]                                        
fwrite(facility_data_for_record_request, 
       here("Data", "Processed", "texas_facility_data_for_record_request.csv"))                                        


validateion_data <- bind_rows(
   res01,
   res02)

###############################################################################
# ========== 5 B. SUBSTANCE CLASSIFICATION & DETECTION PROCESSING =============
###############################################################################

# Process substance classification
# 1) Define Compartment file URL and local path
comp_url  <- "https://www.tceq.texas.gov/assets/public/admin/data/docs/pst_ust_comprt.txt"
comp_file <- file.path(base, "pst_ust_comprt.txt")


# 1) List all 30 raw flag names exactly as in the PST Compartment spec
raw_flags <- c(
  # Compartment release detection (8)
  "compartment_release_detection_vapor_monitoring",
  "compartment_release_detection_groundwater_monitoring",
  "compartment_release_detection_secondary_containment",
  "compartment_release_detection_automatic_tank_gauge",
  "compartment_release_detection_interstitial_monitoring",
  "compartment_release_detection_weekly_manual_tank_gauging",
  "compartment_release_detection_monthly_tank_gauging",
  "compartment_release_detection_sir_inventory_control",
  # Piping release detection (10)
  "piping_release_detection_vapor_monitoring",
  "piping_release_detection_groundwater_monitoring",
  "piping_release_detection_secondary_containment",
  "piping_release_detection_interstitial_monitoring",
  "piping_release_detection_monthly_tightness_test",
  "piping_release_detection_annual_tightness_test",
  "piping_release_detection_triennial_tightness_test",
  "piping_release_detection_auto_line_leak_detector",
  "piping_release_detection_sir_inventory_control",
  "piping_release_detection_exempt_system_suction",
  # Spill & overfill prevention equipment (6)
  "spill_overfill_tight_fill_fitting",
  "spill_overfill_factory_spill_container",
  "spill_overfill_delivery_shut_off_valve",
  "spill_overfill_flow_restrictor",
  "spill_overfill_alarm",
  "spill_overfill_na",
  # Compliance flags (3)
  "compartment_release_detection_compliance",
  "piping_release_detection_compliance",
  "spill_overfill_compliance",
  # Variance flags (3)
  "compartment_release_detection_variance",
  "piping_release_detection_variance",
  "spill_overfill_variance"
)
stopifnot(length(raw_flags) == 30)  # per PST spec :contentReference[oaicite:5]{index=5}

# 2) Build the fixed-width layout
comp_widths <- c(
  8,    # UST_COMPRT_ID
  8,    # UST_ID
  6,    # FACILITY_ID
  10,   # TANK_ID
  1,    # COMPRT_ID
  8,    # CAPACITY
  30,   # SUBSTANCE_STORED_1
  30,   # SUBSTANCE_STORED_2
  30,   # SUBSTANCE_STORED_3
  rep(1, length(raw_flags)),  # the 30 one-byte flags
  30,   # STAGE1_VAPOR_RECOVERY
  10    # STAGE1_INSTALL_DATE
)

comp_names <- c(
  "UST_COMPRT_ID", "UST_ID", "FACILITY_ID", "TANK_ID", "COMPRT_ID",
  "CAPACITY",
  "SUBSTANCE_STORED_1", "SUBSTANCE_STORED_2", "SUBSTANCE_STORED_3",
  raw_flags,
  "STAGE1_VAPOR_RECOVERY", "STAGE1_INSTALL_DATE"
)

# 3) Sanity check
stopifnot(
  length(comp_widths) == length(comp_names),
  sum(comp_widths)    == 201
)




# 4) Load using your existing helper
ust_compartments <- load_and_process_data(
   paste0(base, "/pst_ust_comprt.txt"),
   comp_widths,
   comp_names
)




setDT(ust_compartments)


texas_compartment_combined <- ust_compartments[
  , .(
      # 1) paste the three substance columns together
      substances = {
        tmp <- paste(SUBSTANCE_STORED_1,
                     SUBSTANCE_STORED_2,
                     SUBSTANCE_STORED_3,
                     sep = "; ")
        # 2) remove any "NA", leading/trailing separators
        tmp <- gsub("^NA; |; NA$|; NA; ", "", tmp)   # remove NA at start, end, or middle
        tmp[tmp == ""] <- NA                        # if all were NA, give a single NA
        tmp
      }
    ),
  by = .(FACILITY_ID, TANK_ID, UST_ID)
]

# View result
texas_compartment_combined

  # Classify substances
  texas_compartment_classified <- classify_substances(texas_compartment_combined)

  texas_compartment_classified = clean_id_column(texas_compartment_classified, "UST_ID")
texas_compartment_classified = clean_id_column(texas_compartment_classified, "FACILITY_ID")
  



  substance_cols <- c( "UST_ID", "is_gasoline", "is_diesel", 
                     "is_oil_kerosene", "is_jet_fuel", "is_other")
  texas_compartment_classified2 <- texas_compartment_classified[, ..substance_cols]
  

sum(texas_compartment_classified2[UST_ID %in% ust$UST_ID,is_gasoline], na.rm = TRUE)
sum(texas_compartment_classified2[UST_ID %in% ust$UST_ID,is_diesel], na.rm = TRUE)
# Merge substance classification
if("UST_ID" %in% names(texas_compartment_classified2)) {
  texas_compartment_classified2[, UST_ID := as.character(UST_ID)]
}


ust <- merge(ust, texas_compartment_classified2, by = c( "UST_ID"), all.x = TRUE)

sum(ust$is_gasoline, na.rm = TRUE)
sum(ust$is_diesel, na.rm = TRUE)


# Define which columns are your flags
flag_cols <- c("is_gasoline", "is_diesel", "is_oil_kerosene",
               "is_jet_fuel",   "is_other")

# 1) Coerce each flag to integer
ust[
  , (flag_cols) := lapply(.SD, as.integer),
  .SDcols = flag_cols
]

# 2) Create a logical column that is TRUE if any flag is set
ust[
  , has_substance_info := rowSums(.SD) > 0,
  .SDcols = flag_cols
]

###############################################################################
# Process detection methods
###############################################################################

# Rename detection columns in compartment data
det_rename <- c(
  compartment_release_detection_vapor_monitoring = "DET_C_VAPOR",
  compartment_release_detection_groundwater_monitoring = "DET_C_GW",
  compartment_release_detection_secondary_containment = "DET_C_SEC_CONT",
  compartment_release_detection_automatic_tank_gauge = "DET_C_ATG",
  compartment_release_detection_interstitial_monitoring = "DET_C_INTERSTITIAL",
  compartment_release_detection_weekly_manual_tank_gauging = "DET_C_MANUAL_WEEK",
  compartment_release_detection_monthly_tank_gauging = "DET_C_MANUAL_MONTH",
  compartment_release_detection_sir_inventory_control = "DET_C_SIR",
  piping_release_detection_vapor_monitoring = "DET_P_VAPOR",
  piping_release_detection_groundwater_monitoring = "DET_P_GW",
  piping_release_detection_secondary_containment = "DET_P_SEC_CONT",
  piping_release_detection_interstitial_monitoring = "DET_P_INTERSTITIAL",
  piping_release_detection_monthly_tightness_test = "DET_P_TT_MONTH",
  piping_release_detection_annual_tightness_test = "DET_P_TT_ANNUAL",
  piping_release_detection_triennial_tightness_test = "DET_P_TT_TRIEN",
  piping_release_detection_auto_line_leak_detector = "DET_P_LLD",
  piping_release_detection_sir_inventory_control = "DET_P_SIR",
  piping_release_detection_exempt_system_suction = "DET_P_SUCTION_EXEMPT"
)

present <- intersect(names(det_rename), names(ust_compartments))
setnames(ust_compartments, present, det_rename[present])
ust
# Classify detection methods
texas_compartment_det <- copy(ust_compartments)
texas_compartment_det[, (detect_cols) := lapply(.SD, function(x) x == "Y"), .SDcols = detect_cols]

# Create detection summary flags
texas_compartment_det[, `:=`(
  has_interstitial_det = DET_C_INTERSTITIAL | DET_C_SEC_CONT | DET_P_INTERSTITIAL | DET_P_SEC_CONT,
  has_electronic_det = DET_C_ATG | DET_P_LLD,
  has_manual_stat_det = DET_C_MANUAL_WEEK | DET_C_MANUAL_MONTH | DET_C_SIR | DET_P_SIR,
  has_any_det = rowSums(.SD) > 0
), .SDcols = detect_cols]

# Prepare detection data for merge
det_merge_cols <- c( "UST_ID", "has_interstitial_det", "has_electronic_det", 
                   "has_manual_stat_det", "has_any_det", detect_cols)
texas_compartment_det <- texas_compartment_det[, ..det_merge_cols]

# Ensure character types for merge
if("UST_ID" %in% names(texas_compartment_det)) {
  texas_compartment_det[, UST_ID := as.character(UST_ID)]
}

# Merge detection data
ust <- merge(ust, texas_compartment_det, by = c( "UST_ID"), all.x = TRUE)

# Fill missing detection flags
all_detection_cols <- c("has_interstitial_det", "has_electronic_det", 
                       "has_manual_stat_det", "has_any_det", detect_cols)
for(col in all_detection_cols) {
  ust[is.na(get(col)), (col) := FALSE]
}


### Count of USTs and Facilites that Should be in the panel: State 3 Left joined tank comparment data, no filtering 
  res03 <- run_stage01_validation(
    ust            = ust,
    min_panel_date = as.Date("1990-01-01"),
    max_panel_date = as.Date("2026-12-01"),
    stage_label    = "stage_after_classification"
  )

validateion_data <- bind_rows(
   validateion_data,
   res03)
sum(ust$is_gasoline, na.rm = TRUE)
sum(ust$is_diesel, na.rm = TRUE)


# Save processed UST data
saveRDS(ust, file = here("Data", "Raw", "state_databases", "Texas", "raw_pst_ust.rds"))
fwrite(ust, file = here("Data", "Raw", "state_databases", "Texas", "raw_pst_ust.csv"), row.names = FALSE)
setkey(ust,FACILITY_ID, UST_ID)

# 1. Create panel_months grid
# Use facility IDs from 'fac' as it's the basis for the main panel in Section 7
# Around line 1430, ensure panel is properly created:
min_panel_date <- as.Date("1990-01-01")
max_panel_date <- as.Date("2026-12-01")
panel_facility_ids <- unique(ust$FACILITY_ID) 
panel_months <- CJ(
  FACILITY_ID = panel_facility_ids,
  month_date = seq(floor_date(min_panel_date, "month"), floor_date(max_panel_date, "month"), by = "month")
)
panel_months[, YEAR := year(month_date)]
panel_months[, MONTH := month(month_date)]
setkey(panel_months, FACILITY_ID, month_date)

# Create the main panel from panel_months
panel <- panel_months[, .(FACILITY_ID, YEAR, MONTH)]

uniqueN(panel$FACILITY_ID) # Check how many unique facility IDs we have in the panel


message("Section 5 complete: UST processing finished with ", nrow(ust), " tanks")
# -----------------------------------------------------------------------------
# HARMONIZATION STEP: Create and Save TX_Harmonized_UST_tanks.csv
# -----------------------------------------------------------------------------
message("Creating Harmonized UST Tank dataset...")

# 1. Merge Facility Name and County from 'fac' (loaded in Section 1)
# Ensure fac is keyed or we use explicit 'by'
ust_harmonized <- merge(
  ust, 
  fac[, .(FACILITY_ID, FACILITY_NAME, SITE_COUNTY)], 
  by = "FACILITY_ID", 
  all.x = TRUE
)

# 2. Consolidate Wall Types
# Combine missing_walled and unknown_walled into a single unknown flag
ust_harmonized[, unknown_walled := pmax(unknown_walled, missing_walled, na.rm = TRUE)]

# 3. Select and Standardize Columns
TX_Harmonized_UST_tanks <- ust_harmonized[, .(
  facility_id = as.character(FACILITY_ID),
  facility_name = stringr::str_to_title(trimws(gsub("[^[:alnum:] ]", "", FACILITY_NAME))),
  tank_id = as.character(TANK_ID),
  state = "TX",
  
  # Dates: Use the raw parsed dates from Section 5
  tank_installed_date = INSTALL_DATE,
  
  # Logic: If status indicates closure (calculated in Section 5 via is_closed_removed), use STATUS_DATE
  tank_closed_date = fifelse(is_closed_removed, STATUS_DATE, as.Date(NA)),
  
  # Status: Keep the raw Texas status string as requested
  tank_status = STATUS,
  
  capacity = CAPACITY,
  
  # Wall Types (Binary)
  single_walled = as.integer(single_walled),
  double_walled = as.integer(double_walled),
  unknown_walled = as.integer(unknown_walled),
  
  # Substances (Binary) - calculated in Section 5B
  is_gasoline = as.integer(is_gasoline),
  is_diesel = as.integer(is_diesel),
  is_oil_kerosene = as.integer(is_oil_kerosene),
  is_jet_fuel = as.integer(is_jet_fuel),
  is_other = as.integer(is_other),
  
  # Location
  county_name = stringr::str_to_title(trimws(SITE_COUNTY))
)]

# 4. Save to State Database Folder
fwrite(TX_Harmonized_UST_tanks, file.path(tx_output_dir, "TX_Harmonized_UST_tanks.csv"))
message("Saved TX_Harmonized_UST_tanks.csv")


# ── Section 6: Monthly UST aggregations ─────────────────────────────────────
# Purpose: build full FACILITY×month grid (`panel_months`), non-equi join tanks to months,
#   then collapse to `ust_monthly` with counts, capacities, ages, wall-type proportions,
#   material/substance counts, detection & corrosion summaries, and upgrade events.
# Why: to quantify active tank inventory and attributes per facility per month.
# End product: `ust_monthly` keyed by FACILITY_ID, YEAR, MONTH
# Usage: merged into `panel` by FACILITY_ID, YEAR, MONTH in Section 7.

message("Processing Section 6: Creating monthly UST aggregations using UST_ID (non-equi join)...")

# 1. Use existing panel_months grid from Section 1
message("Using existing panel_months grid from Section 1...")

# 2. Define all detection and corrosion columns programmatically
detect_cols <- c(
  "DET_C_VAPOR", "DET_C_GW", "DET_C_SEC_CONT", "DET_C_ATG",
  "DET_C_INTERSTITIAL", "DET_C_MANUAL_WEEK", "DET_C_MANUAL_MONTH", "DET_C_SIR",
  "DET_P_VAPOR", "DET_P_GW", "DET_P_SEC_CONT", "DET_P_INTERSTITIAL",
  "DET_P_TT_MONTH", "DET_P_TT_ANNUAL", "DET_P_TT_TRIEN", "DET_P_LLD",
  "DET_P_SIR", "DET_P_SUCTION_EXEMPT"
)

corr_cols <- c("CORR_TANK_CP", "CORR_PIPE_CP", "CORR_TANK_VARIANCE", "CORR_PIPE_VARIANCE")

# 3. Pre-filter UST data to only relevant columns and date ranges
ust_cols_for_join <- c(
  "FACILITY_ID", "UST_ID", "install_date", "end_date", "CAPACITY", "STATUS", "STATUS_DATE", 
  "single_walled", "double_walled", "missing_walled", "unknown_walled",
  "is_steel_tank", "is_fiberglass_tank", "is_composite_tank", "is_concrete_tank", 
  "is_jacketed_tank", "is_coated_tank", "is_unknown_material_tank",
  "is_gasoline", "is_diesel", "is_oil_kerosene", "is_jet_fuel", "is_other",
  "has_interstitial_det", "has_electronic_det", "has_manual_stat_det", "has_any_det", 'is_closed_removed',
  detect_cols, corr_cols
)

# Ensure all selected columns exist in ust
ust_cols_for_join <- intersect(names(ust), ust_cols_for_join)

# Only keep USTs that could possibly be active during our panel period
ust_filtered <- ust[
  install_date <= max_panel_date | end_date >= min_panel_date,
  ..ust_cols_for_join
]

res04 <- run_stage01_validation(
  ust            = ust_filtered,
  min_panel_date = as.Date("1990-01-01"),
  max_panel_date = as.Date("2026-12-01"),
  stage_label    = "stage_after_ust_filtering_before_panel"
)

validateion_data <- bind_rows(
   validateion_data,
   res04)

message("Filtered UST data from ", nrow(ust), " to ", nrow(ust_filtered), " potentially active tanks")

# 4. Ensure optimal data types for join performance
ust_filtered[, `:=`(
  install_date = as.IDate(install_date),
  end_date = as.IDate(end_date),
  CAPACITY = as.numeric(CAPACITY)
)]
glimpse(ust_filtered)

# Convert character corrosion flags to logical before aggregation
char_corr_cols_to_logical <- c("CORR_TANK_CP", "CORR_PIPE_CP", "CORR_TANK_VARIANCE", "CORR_PIPE_VARIANCE")
for(col in char_corr_cols_to_logical) {
  if(col %in% names(ust_filtered) && is.character(ust_filtered[[col]])) {
    ust_filtered[, (col) := (get(col) == "Y")]
  } else if (col %in% names(ust_filtered) && !is.logical(ust_filtered[[col]])) {
    ust_filtered[, (col) := as.logical(as.character(get(col)) == "Y")]
  }
  if(col %in% names(ust_filtered)) ust_filtered[is.na(get(col)), (col) := FALSE]
}




# 5. Set optimal keys for the non-equi join
setkey(ust_filtered, FACILITY_ID, install_date, end_date)
setkey(panel_months, FACILITY_ID, month_date)

# 6. Perform optimized non-equi join to get active_tank_months
message("Performing optimized non-equi join...")
# ── Section 6: Monthly UST aggregations (pre-prefix removal) ─────────────────
message("Processing Section 6: Creating monthly UST aggregations…")



# (c) Expand each tank to facility×month, preserving x.* and i.* columns
active_tank_months <- ust_filtered[panel_months,
  on = .(
    FACILITY_ID,
    install_date <= month_date,
    end_date     >= month_date
  ),
  nomatch = 0L,
  allow.cartesian = TRUE,
  .(
    x.FACILITY_ID, x.UST_ID,
    i.YEAR, i.MONTH, i.month_date,
    x.install_date, x.end_date,x.STATUS_DATE,
    x.CAPACITY,

    x.single_walled, x.double_walled, x.missing_walled, x.unknown_walled,
    x.is_steel_tank, x.is_fiberglass_tank, x.is_composite_tank,
    x.is_concrete_tank, x.is_jacketed_tank, x.is_coated_tank,
    x.is_unknown_material_tank,

    x.is_gasoline, x.is_diesel, x.is_oil_kerosene, x.is_jet_fuel, x.is_other,
    x.has_interstitial_det, x.has_electronic_det, x.has_manual_stat_det, x.has_any_det,

    x.DET_C_VAPOR, x.DET_C_GW, x.DET_C_SEC_CONT, x.DET_C_ATG, x.DET_C_INTERSTITIAL,
    x.DET_C_MANUAL_WEEK, x.DET_C_MANUAL_MONTH, x.DET_C_SIR,

    x.DET_P_VAPOR, x.DET_P_GW, x.DET_P_SEC_CONT, x.DET_P_INTERSTITIAL, x.DET_P_TT_MONTH,
    x.DET_P_TT_ANNUAL, x.DET_P_TT_TRIEN, x.DET_P_LLD, x.DET_P_SIR, x.DET_P_SUCTION_EXEMPT,

    x.CORR_TANK_CP, x.CORR_PIPE_CP, x.CORR_TANK_VARIANCE, x.CORR_PIPE_VARIANCE,

    x.is_closed_removed
  )
]

# (d) Strip the x./i. prefixes in one go
old_names <- grep("^(x\\.|i\\.)", names(active_tank_months), value = TRUE)
new_names <- sub("^(?:x\\.|i\\.)", "", old_names)
setnames(active_tank_months, old = old_names, new = new_names)
setnames(active_tank_months, "STATUS_DATE", "tank_closed_date_raw")
## A.  Facilities derived from the raw / filtered tank table
fac_from_ust  <- unique(ust_filtered$FACILITY_ID)

## B.  Facilities that actually make it into the month-panel
fac_from_panel <- unique(active_tank_months$FACILITY_ID)

length(fac_from_ust)      # how many should be in the panel
length(fac_from_panel)    # how many you actually have


# 8. Pre-compute age calculation to avoid doing it in aggregation

active_tank_months[, `:=`(
  month_date   = as.IDate(month_date),   # ensure IDate
  install_date = as.IDate(install_date)  # ditto
)]

active_tank_months[
  , tank_age_months := {
      iv <- interval(install_date, month_date)
      full_months <- iv %/% months(1)
      fifelse(full_months >= 0, full_months, NA_integer_)
  }
]


########
res05 <- run_stage01_validation(
  ust            = active_tank_months,
  min_panel_date = as.Date("1990-01-01"),
  max_panel_date = as.Date("2026-12-01"),
  stage_label    = "stage_after_monthly_expansion"
)

validateion_data <- bind_rows(
   validateion_data,
   res05)

#save the ust-faciliyt-monthly panel
fwrite(active_tank_months, file = here("Data", "Processed", "texas_ust_facility_month_panel.csv"))


sum(active_tank_months$is_gasoline, na.rm = TRUE) # Check how many gasoline tanks we have

# 9. Perform facility-month aggregation with all required variables
message("Performing facility-month aggregation...")

ust_monthly <- active_tank_months[, .(
  # Earliest install date and latest end date
  earliest_install_date = min(install_date, na.rm = TRUE),
  latest_end_date = max(end_date, na.rm = TRUE),

  # Core counts and capacity
  active_tank_count = .N,  # More efficient than uniqueN when UST_ID is unique per row
  total_capacity = sum(CAPACITY, na.rm = TRUE),
  
  # Age and proportions
  avg_tank_age_months = mean(tank_age_months, na.rm = TRUE),
  prop_single_wall = mean(single_walled, na.rm = TRUE),
  prop_double_wall = mean(double_walled, na.rm = TRUE),
  
  # Wall type counts
  num_sw_tanks = sum(single_walled, na.rm = TRUE),
  num_dw_tanks = sum(double_walled, na.rm = TRUE),
  num_missing_tanks = sum(missing_walled, na.rm = TRUE),
  num_unknown_tanks = sum(unknown_walled, na.rm = TRUE),
  
  # Material counts
  num_steel_tanks = sum(is_steel_tank, na.rm = TRUE),
  num_fiberglass_tanks = sum(is_fiberglass_tank, na.rm = TRUE),
  num_composite_tanks = sum(is_composite_tank, na.rm = TRUE),
  num_concrete_tanks = sum(is_concrete_tank, na.rm = TRUE),
  num_jacketed_tanks = sum(is_jacketed_tank, na.rm = TRUE),
  num_coated_tanks = sum(is_coated_tank, na.rm = TRUE),
  num_unknown_material_tanks = sum(is_unknown_material_tank, na.rm = TRUE),
  
  # Substance counts
  num_gasoline_tanks = sum(is_gasoline > 0, na.rm = TRUE),
  num_diesel_tanks = sum(is_diesel > 0, na.rm = TRUE),
  num_oil_kerosene_tanks = sum(is_oil_kerosene > 0, na.rm = TRUE),
  num_jet_fuel_tanks = sum(is_jet_fuel > 0, na.rm = TRUE),
  num_other_substance_tanks = sum(is_other > 0, na.rm = TRUE),
  total_tanks_with_substance_info = sum(is_gasoline > 0 | is_diesel > 0 | is_oil_kerosene > 0 | 
                                       is_jet_fuel > 0 | is_other > 0, na.rm = TRUE),
  
  # Detection summary counts
  num_interstitial_det = sum(has_interstitial_det, na.rm = TRUE),
  num_electronic_det = sum(has_electronic_det, na.rm = TRUE),
  num_manual_stat_det = sum(has_manual_stat_det, na.rm = TRUE),
  num_any_det = sum(has_any_det, na.rm = TRUE),
  
  # Individual detection method counts (compartment)
  num_DET_C_VAPOR = sum(DET_C_VAPOR, na.rm = TRUE),
  num_DET_C_GW = sum(DET_C_GW, na.rm = TRUE),
  num_DET_C_SEC_CONT = sum(DET_C_SEC_CONT, na.rm = TRUE),
  num_DET_C_ATG = sum(DET_C_ATG, na.rm = TRUE),
  num_DET_C_INTERSTITIAL = sum(DET_C_INTERSTITIAL, na.rm = TRUE),
  num_DET_C_MANUAL_WEEK = sum(DET_C_MANUAL_WEEK, na.rm = TRUE),
  num_DET_C_MANUAL_MONTH = sum(DET_C_MANUAL_MONTH, na.rm = TRUE),
  num_DET_C_SIR = sum(DET_C_SIR, na.rm = TRUE),
  
  # Individual detection method counts (piping)
  num_DET_P_VAPOR = sum(DET_P_VAPOR, na.rm = TRUE),
  num_DET_P_GW = sum(DET_P_GW, na.rm = TRUE),
  num_DET_P_SEC_CONT = sum(DET_P_SEC_CONT, na.rm = TRUE),
  num_DET_P_INTERSTITIAL = sum(DET_P_INTERSTITIAL, na.rm = TRUE),
  num_DET_P_TT_MONTH = sum(DET_P_TT_MONTH, na.rm = TRUE),
  num_DET_P_TT_ANNUAL = sum(DET_P_TT_ANNUAL, na.rm = TRUE),
  num_DET_P_TT_TRIEN = sum(DET_P_TT_TRIEN, na.rm = TRUE),
  num_DET_P_LLD = sum(DET_P_LLD, na.rm = TRUE),
  num_DET_P_SIR = sum(DET_P_SIR, na.rm = TRUE),
  num_DET_P_SUCTION_EXEMPT = sum(DET_P_SUCTION_EXEMPT, na.rm = TRUE),
  
  # Corrosion counts
  num_corr_tank_cp = sum(CORR_TANK_CP, na.rm = TRUE),
  num_corr_pipe_cp = sum(CORR_PIPE_CP, na.rm = TRUE),
  num_corr_tank_var = sum(CORR_TANK_VARIANCE, na.rm = TRUE),
  num_corr_pipe_var = sum(CORR_PIPE_VARIANCE, na.rm = TRUE),
   # **NEW**: count tanks that actually closed in this month
closed_tank_count = sum(is_closed_removed & 
                        floor_date(end_date, "month") == month_date, 
                        na.rm=TRUE)  
), by = .(FACILITY_ID, YEAR, MONTH)]


# 10. Handle edge cases and add derived flags
ust_monthly[, `:=`(
  # Handle NaN ages
  avg_tank_age_months = fifelse(is.nan(avg_tank_age_months), NA_real_, avg_tank_age_months),
  
  # Handle zero tank cases for proportions
  prop_single_wall = fifelse(active_tank_count == 0, NA_real_, prop_single_wall),
  prop_double_wall = fifelse(active_tank_count == 0, NA_real_, prop_double_wall),
  
  # Has tank flags (based on counts)
  has_gasoline_tank = as.integer(num_gasoline_tanks > 0),
  has_diesel_tank = as.integer(num_diesel_tanks > 0),
  has_oil_kerosene_tank = as.integer(num_oil_kerosene_tanks > 0),
  has_jet_fuel_tank = as.integer(num_jet_fuel_tanks > 0),
  has_other_substance_tank = as.integer(num_other_substance_tanks > 0),
  has_corr_deficiency = as.integer(num_corr_tank_var > 0 | num_corr_pipe_var > 0)
)]

# 11. Efficient double-wall upgrade detection
setkey(ust_monthly, FACILITY_ID, YEAR, MONTH)
ust_monthly[, `:=`(
  lag_dw_tanks = shift(num_dw_tanks, type = "lag"),
  lag_other_tanks = shift(num_sw_tanks + num_unknown_tanks + num_missing_tanks, type = "lag")
), by = FACILITY_ID]

ust_monthly[, dw_upgrade_event := as.integer(
  !is.na(lag_dw_tanks) & lag_dw_tanks < num_dw_tanks & 
  !is.na(lag_other_tanks) & lag_other_tanks > (num_sw_tanks + num_unknown_tanks + num_missing_tanks)
)]

# Clean up temporary columns
ust_monthly[, c("lag_dw_tanks", "lag_other_tanks") := NULL]
ust_monthly[is.na(dw_upgrade_event), dw_upgrade_event := 0L]

# # 12. Final cleanup and save
rm(ust_filtered, active_tank_months)  # Free memory
 gc()  # Force garbage collection

setkey(ust_monthly, FACILITY_ID, YEAR, MONTH)
message("Optimized ust_monthly built: ", format(nrow(ust_monthly), big.mark=","), " rows")

# 1. Last active month per facility
last_active <- ust_monthly[active_tank_count > 0,
  .(Last_YearMonth = max(make_date(YEAR, MONTH, 1)) ),
  by = FACILITY_ID
]

ust_monthly <- merge(ust_monthly, last_active,
                     by = "FACILITY_ID", all.x = TRUE)

# 1a. Extract the components
ust_monthly[
  , `:=`(
      closure_year  = year(Last_YearMonth),
      closure_month = month(Last_YearMonth)
  )
]

# 1b. Now you can safely refer to those new columns
ust_monthly[
  , is_active_pre_clos := as.integer(
      !is.na(Last_YearMonth) &
      (YEAR <  closure_year |
       (YEAR == closure_year & MONTH <= closure_month))
  )
]

# 3. Entry/exit events
setorder(ust_monthly, FACILITY_ID, YEAR, MONTH)
ust_monthly[, `:=`(
  is_facility_entry_event = as.integer(
    active_tank_count > 0 & shift(active_tank_count, fill = 0L) == 0L
  ),
  is_facility_exit_event  = as.integer(
    is_active_pre_clos == 0L & shift(is_active_pre_clos, fill = 1L) == 1L
  )
), by = FACILITY_ID]

# 4. Clean up any NAs
for(col in c("is_facility_entry_event","is_facility_exit_event")){
  ust_monthly[is.na(get(col)), (col) := 0L]
}

message("  → Done: ust_monthly now has closed_tank_count, inventory-closure & entry/exit flags.")
# Save for debugging
fwrite(ust_monthly, here("Data", "Processed", "texas_facility_monthly_ust_aggregations_optimized.csv"))

uniqueN(ust_monthly$FACILITY_ID) # Check how many unique facility IDs we have in the monthly panel


message("Section 6: Monthly UST aggregations complete (optimized version).")

# ── Section 7: Main panel build & merging ────────────────────────────────────
# Purpose: start from skeleton panel of FACILITY_ID×YEAR×MONTH,
#   then left-merge in FA (`fa_monthly`), facility info (`fac`), contact months (`fac_contact_months`),
#   UST metrics (`ust_monthly`), self-cert (`self_cert_monthly`), LUST, Zurich flags, capacity & event deltas.
# Why: to assemble a unified monthly panel for all subsequent analysis.
# End product: `panel` keyed by FACILITY_ID, YEAR, MONTH ready to save.




# ... (previous sections of the script remain unchanged) ...

# ── Section 7: Staging & Conditional Panel Build ─────────────────────────────
# Purpose: Stage all intermediate data tables to disk for modularity and memory
#   management. Then, conditionally, read them back to perform all merges and
#   build the final analysis panel.
# Why: Allows for separate execution of data generation and panel assembly.
#   Reduces memory pressure by writing/reading/removing large objects.
# End product: Staged data in Outputs/ or a complete `panel` object saved to disk.

section7_start <- Sys.time()
message("Processing Section 7: Staging intermediate data...")

###############################################################################
# 7.1 QUICK-STAGE ALL PRE-MERGE TABLES TO DISK
###############################################################################
stage_dir <- here("Data", "Intermediate", "Texas", "panel_merge_staging")
if (!dir.exists(stage_dir)) dir.create(stage_dir, recursive = TRUE)

fac_info <- fac[ , .(
  FACILITY_ID, FACILITY_NAME,
  FACILITY_TYPE, FACILITY_STATUS,
  SITE_COUNTY, SITE_ZIP,
  NUM_ACTIVE_USTS_is_blank,
  NUM_ACTIVE_ASTS_is_blank
)]

datasets_to_stage <- list(
  panel               = panel,
  fa_monthly          = fa_monthly,
  fac_info            = fac_info,          # now it really exists
  fac_contact_months  = fac_contact_months,
  ust_monthly         = ust_monthly,
  zurich_2012_lookup  = zurich_2012_lookup,
  self_cert_monthly   = self_cert_monthly,
  fa_monthly_contract = fa_monthly_contract
)

# original loop with get() will now work
for (nm in names(datasets_to_stage)) {
  dt <- get(nm) # Get the object from the environment
  rds_path <- file.path(stage_dir, paste0(nm, ".rds"))
  fwrite(dt, file.path(stage_dir, paste0(nm, ".csv")))
  saveRDS(dt, rds_path)
  message("Staged ", nm, " -> ", basename(rds_path))
}

# Memory Hygiene
rm(list = names(datasets_to_stage))
rm(fac, fa, self_cert, ust, ust_compartments, texas_compartment_classified, texas_compartment_det) # remove large raw objects
gc(verbose = FALSE)
message("Cleaned pre-merge objects from memory.")



###############################################################################
# 7.2 LUST PROCESSING & STAGING
###############################################################################
message("Processing LUST data for staging...")

# Load and preprocess LUST data efficiently
TX_LUST_SD <- fread(here("Data", "Raw", "state_databases", "Texas", "TX_LUST.csv"))


if ("facility_id" %in% names(TX_LUST_SD)) setnames(TX_LUST_SD, "facility_id", "FACILITY_ID")
TX_LUST_SD <- clean_id_column(TX_LUST_SD, "FACILITY_ID")

# Parse Dates
TX_LUST_SD[, `:=`(
  report_date = dmy(report_date),
  nfa_date = dmy(nfa_date)
)]
# -----------------------------------------------------------------------------
# HARMONIZATION STEP: Create and Save TX_Harmonized_LUST.csv
# -----------------------------------------------------------------------------


message("Creating Harmonized LUST dataset...")
TX_Harmonized_LUST <- TX_LUST_SD[, .(
  facility_id = as.character(FACILITY_ID),
  lust_id = as.character(LUST_id),
  report_date = as.Date(report_date),
  nfa_date = as.Date(nfa_date),
  state = "TX"
)]


# Continue with Panel specific filtering...
TX_LUST_SD <- TX_LUST_SD[
  !is.na(report_date) &
  report_date >= as.Date("1990-01-01") &
  report_date <= as.Date("2026-12-31") &
  FACILITY_ID %in% facilities_active_1990$FACILITY_ID
]
message("LUST records filtered to ", nrow(TX_LUST_SD), " relevant records")

fwrite(TX_Harmonized_LUST, file.path(tx_output_dir, "TX_Harmonized_LUST.csv"))
message("Saved TX_Harmonized_LUST.csv")


expand_lust_optimized <- function(lust_dt) {
  lust_dt[!is.na(report_date), {
    start_month <- floor_date(report_date, "month")
    end_month <- floor_date(fifelse(is.na(nfa_date), as.Date("2026-12-31"), nfa_date), "month")
    if(start_month <= end_month) {
      month_seq <- seq(start_month, end_month, by = "month")
      .(YEAR = year(month_seq), MONTH = month(month_seq), lust_active = TRUE,
        is_pre2000 = report_date < as.Date("2000-01-01"),
        has_subsidy = report_date < as.Date("2000-01-01"),
        nfa_month = end_month)
    }
  }, by = .(FACILITY_ID, LUST_id)]
}

lust_report_months <- TX_LUST_SD[!is.na(report_date), .(
  FACILITY_ID, YEAR = year(report_date), MONTH = month(report_date), is_lust_report_month = 1L
)]
lust_long <- expand_lust_optimized(TX_LUST_SD)
lust_monthly_status <- lust_long[, .(
  lust_active = as.integer(any(lust_active)),
  lust_pre_2000_active = as.integer(any(lust_active & is_pre2000)),
  lust_post_2000_active = as.integer(any(lust_active & !is_pre2000)),
  has_subsidized_leak_active = as.integer(any(lust_active & has_subsidy)),
  lust_open_active = as.integer(any(lust_active & (nfa_month > make_date(YEAR, MONTH, 1)))),
  lust_closed_active = as.integer(any(lust_active & (nfa_month <= make_date(YEAR, MONTH, 1))))
), by = .(FACILITY_ID, YEAR, MONTH)]
facility_lust_summary <- TX_LUST_SD[, .(
  lust_ever = 1L, lust_pre_2000 = as.integer(any(report_date < "2000-01-01")),
  lust_post_2000 = as.integer(any(report_date >= "2000-01-01")),
  has_subsidized_leak = as.integer(any(report_date < "2000-01-01")),
  lust_ever_closed = as.integer(any(!is.na(nfa_date))),
  lust_ever_open = as.integer(any(is.na(nfa_date))),
  lust_after_closure_ever = as.integer(any(!is.na(nfa_date) & nfa_date < as.Date("2026-12-31") & nfa_date > report_date))
), by = FACILITY_ID]

# QUICK-STAGE LUST TABLES TO DISK
lust_stage_dir <- here("Data", "Intermediate", "Texas", "lust_merge_staging")
if (!dir.exists(lust_stage_dir)) dir.create(lust_stage_dir, recursive = TRUE)

lust_to_stage <- list(
  lust_report_months    = lust_report_months,
  lust_monthly_status   = lust_monthly_status,
  facility_lust_summary = facility_lust_summary
)

for (nm in names(lust_to_stage)) {
  dt <- get(nm)
  rds_path <- file.path(lust_stage_dir, paste0(nm, ".rds"))
  fwrite(dt, file.path(lust_stage_dir, paste0(nm, ".csv")))
  saveRDS(dt, rds_path)
  message("Staged ", nm, " -> ", basename(rds_path))
}

# Memory Hygiene
rm(list = names(lust_to_stage))
rm(TX_LUST_SD, lust_long)
gc(verbose = FALSE)
message("Cleaned LUST objects from memory.")

# ── Section 8: Census data processing & staging ──────────────────────────────
message("Processing Section 8: Building and staging census data...")
library(fredr); library(data.table)
###############################################################################
# 1. Meta: keep a tidy lookup of variable ↔ units for future reference
###############################################################################
units_map <- data.table(
  variable = c("total_pop", "median_hh_income", "gini_index"),
  units    = c("persons",  "US dollars (nominal-year)", "unitless index (0–1)")
)
expand_5yr_bin <- function(dt, end_year_col = "survey_end_year", value_cols) {
  dt_long <- melt(dt,id.vars= c("fips", end_year_col),measure.vars = value_cols,variable.name = "var",value.name= "value")
  dt_long[, year := as.integer(get(end_year_col))]
  dt_long <- dt_long[, .(year= rep((year - 4):year, each = .N / length(value_cols)),fips,var,value), by = .I]
  dcast(dt_long, fips + year ~ var, value.var = "value")
}
reshape_pop <- function(url, years) {
  cols <- c("STATE", "COUNTY", paste0("POPESTIMATE", years))
  dt   <- fread(url, select = cols, colClasses = "character", na.strings = "(X)")
  dt   <- dt[STATE == "48"]
  dt[, fips := sprintf("%02d%03d", as.integer(STATE), as.integer(COUNTY))]
  long <- melt(dt, id.vars = "fips", measure.vars = patterns("^POPESTIMATE"), variable.name = "year_str", value.name = "total_pop", na.rm = TRUE)
  long[, year := as.integer(sub("POPESTIMATE", "", year_str))]
  long[, .(fips, year, total_pop = as.numeric(total_pop))]
}
pop_2010_2020 <- reshape_pop(url= "https://www2.census.gov/programs-surveys/popest/datasets/2010-2020/counties/totals/co-est2020-alldata.csv",years = 2010:2020)
pop_2000_2009 <- reshape_pop(url= "https://www2.census.gov/programs-surveys/popest/datasets/2000-2009/counties/totals/co-est2009-alldata.csv",years = 2000:2009)
peek_icen_year <- function(yr, state_fips = "48", n_preview = 5) {
  stopifnot(yr %in% 1990:1999)
  url <- sprintf("https://www2.census.gov/programs-surveys/popest/tables/1990-2000/intercensal/st-co/stch-icen%d.txt",yr)
  raw <- fread(url, header = FALSE, colClasses = "character", showProgress = FALSE)
  setnames(raw, paste0("V", seq_len(ncol(raw))))
  message(sprintf("\nPreview of %s (first %d rows):", basename(url), n_preview)); print(head(raw, n_preview))
  tx <- raw[substr(V2, 1, 2) == state_fips & V3 == "0" & V4 == "1" & V5 == "1",.(fips= V2,year= yr,total_pop = as.numeric(V6))]
  message(sprintf("Returned %d county rows for Texas, %d", nrow(tx), yr)); return(tx)
}
pop_1998 <- peek_icen_year(1998)
pop_1999 <- peek_icen_year(1999)
pop_dt <- rbindlist(list(pop_1998, pop_1999, pop_2000_2009, pop_2010_2020),use.names = TRUE)[order(fips, year)]
message(sprintf("Pop table: %s rows, %s-%s", nrow(pop_dt),min(pop_dt$year), max(pop_dt$year)))
fredr_set_key("2d9a1f281713753ddce9c4250540c2aa")
tx_fips <- as.data.table(fips_codes)[state == "TX"]; tx_fips[ , fips := paste0(state_code, county_code)]; tx_fips <- unique(tx_fips[ , .(fips)])
if (!"48000" %in% tx_fips$fips) {tx_fips <- rbind(tx_fips, data.table(fips = "48000"))}
print(head(tx_fips, 6))
tx_fips[, series_id := paste0("MHITX", fips, "A052NCEN")]
tail(tx_fips)
get_mhi <- function(id, fips_code) {
  Sys.sleep(0.5)
  tryCatch({
    obs <- fredr_series_observations(series_id= id,observation_start = as.Date("1989-01-01"),units= "lin")
    obs <- as.data.table(obs)
    if (nrow(obs) == 0) return(NULL)
    obs[ , .(fips= fips_code,year= year(date),median_hh_income = value)]
  }, error = function(e) {
    warning(sprintf("Missing series %s (%s): %s", id, fips_code, e$message)); NULL
  })
}
income_dt <- rbindlist(Map(get_mhi, tx_fips$series_id, tx_fips$fips),use.names = TRUE, fill = TRUE)
income_dt[ , median_hh_income_units := "US dollars (nominal-year)"]
message("Fetched income data for ", uniqueN(income_dt[!is.na(median_hh_income), fips]), " counties.")
message("Years range from: ", min(income_dt$year, na.rm = TRUE), " to ", max(income_dt$year, na.rm = TRUE)); print(head(income_dt))
message("Downloading & expanding ACS Gini …")
acs_end_years <- 2010:2023
gini_list <- lapply(acs_end_years, function(ey) {
  acs_tbl <- get_acs(geography = "county",state= "TX",table= "B19083",year= ey,survey= "acs5",output= "wide")
  setDT(acs_tbl)
  acs_tbl[ , .(fips= GEOID,gini_index= B19083_001E,survey_end_year = ey)]
})
gini_wide <- expand_5yr_bin(rbindlist(gini_list),end_year_col = "survey_end_year",value_cols= "gini_index")
census_data_panel <- CJ(fips = unique(pop_dt$fips), year = 1998:2020)
census_data_panel <- merge(census_data_panel, pop_dt,    by = c("fips", "year"), all.x = TRUE)
census_data_panel <- merge(census_data_panel, income_dt, by = c("fips", "year"), all.x = TRUE)
census_data_panel <- merge(census_data_panel, gini_wide, by = c("fips", "year"), all.x = TRUE)
setnames(census_data_panel, "year", "YEAR")
census_data_panel[ , `:=`(total_pop_units= "persons",median_hh_income_units = "US dollars (nominal-year)",gini_index_units= "unitless index (0–1)")]
if (any(census_data_panel$total_pop < 10000, na.rm = TRUE) & any(census_data_panel$total_pop > 0, na.rm = TRUE)) {
  median_pop <- median(census_data_panel$total_pop, na.rm = TRUE)
  if (!is.na(median_pop) && median_pop < 10000) {
    warning("Median total_pop is low; values may be in thousands. Rescaling to persons.")
    census_data_panel[!is.na(total_pop), total_pop := total_pop * 1000]
  } else if (max(census_data_panel$total_pop, na.rm = TRUE) < 20000 && any(census_data_panel$total_pop > 0, na.rm = TRUE)) {
     warning("Maximum total_pop is low; values may be in thousands. Rescaling to persons.")
     census_data_panel[!is.na(total_pop), total_pop := total_pop * 1000]
  }
}
census_data_panel[order(fips, YEAR),gini_index := zoo::na.locf(gini_index, na.rm = FALSE), by = fips]
census_data_panel[order(fips, YEAR),gini_index := zoo::na.locf(gini_index, na.rm = FALSE, fromLast = TRUE), by = fips]
cpi_dt <- as.data.table(fredr(series_id= "CPIAUCSL",observation_start= as.Date("1998-01-01"),observation_end= as.Date("2023-12-01"),frequency= "a"))[ , .(YEAR = year(date), CPI = value) ]
cpi_2023 <- cpi_dt[YEAR == 2023, CPI]
cpi_dt[ , cpi_factor := cpi_2023 / CPI ]
census_data_panel <- merge(census_data_panel, cpi_dt[ , .(YEAR, cpi_factor)], by = "YEAR", all.x = TRUE)
census_data_panel[ , median_hh_income_real_2023 := median_hh_income * cpi_factor ]
census_data_panel[ ,median_hh_income_real_2023_units := "2023 dollars (CPI-U deflated)"]
units_map <- rbind(units_map,data.table(variable = "median_hh_income_real_2023",units= "2023 dollars (CPI-U deflated)"))
###############################################################################

# QUICK-STAGE CENSUS TABLES TO DISK
census_stage_dir <- here("Data", "Intermediate", "Texas", "census_merge_staging")
if (!dir.exists(census_stage_dir)) dir.create(census_stage_dir, recursive = TRUE)

census_to_stage <- list(
  pop_dt             = pop_dt,
  income_dt          = income_dt,
  gini_wide          = gini_wide,
  cpi_dt             = cpi_dt,
  census_data_panel  = census_data_panel,
  units_map          = units_map
)

for (nm in names(census_to_stage)) {
  dt <- get(nm)
  rds_path <- file.path(census_stage_dir, paste0(nm, ".rds"))
  fwrite(dt, file.path(census_stage_dir, paste0(nm, ".csv")))
  saveRDS(dt, rds_path)
  message("Staged ", nm, " -> ", basename(rds_path))
}

# Memory Hygiene
rm(list = names(census_to_stage))
gc(verbose = FALSE)
message("Cleaned census objects from memory.")

###############################################################################
# ── Section 9: Conditional Full Merge & Finalization ───────────────────────
###############################################################################

if (do_merge) {
  message("\n--- Full Merge Initiated (do_merge = TRUE) ---")

  # --- 1. Load staged data required for the panel build ---
  message("Loading staged data from disk...")
  panel_stage_dir <- here("Data", "Intermediate", "Texas", "panel_merge_staging")
  lust_stage_dir <- here("Data", "Intermediate", "Texas", "lust_merge_staging")
  census_stage_dir <- here("Data", "Intermediate", "Texas", "census_merge_staging")

  panel                 <- readRDS(file.path(panel_stage_dir, "panel.rds"))
  fa_monthly            <- readRDS(file.path(panel_stage_dir, "fa_monthly.rds"))
  fa_monthly_contract   <- readRDS(file.path(panel_stage_dir, "fa_monthly_contract.rds"))
  
  message("All staged data needed for initial merge loaded successfully.")

  # --- 2. Perform merges and processing sequentially ---
  message("Merging Financial Assurance data (fa_monthly)...")
  setkey(panel, FACILITY_ID, YEAR, MONTH)
  setkey(fa_monthly, FACILITY_ID, YEAR, MONTH)
  panel <- merge(panel, fa_monthly, by = c("FACILITY_ID", "YEAR", "MONTH"), all.x = TRUE)
  rm(fa_monthly); gc()

  # --- 2.1. Gap Detection, ERP Logic, and Change Indicators ---
  message("Processing gap detection and change indicators after FA merge...")
  panel[is.na(fr_covered), fr_covered := FALSE]

  message("Calculating ERP coverage...")
  expired_contracts <- fa_monthly_contract[!is.na(EXP_DATE), .(FACILITY_ID, FIN_ASSUR_ID, EXP_DATE, erp_start = floor_date(EXP_DATE + months(1), "month"), erp_end = floor_date(EXP_DATE + months(6), "month"))][!is.na(erp_start) & !is.na(erp_end)]
  expired_contracts <- unique(expired_contracts, by = c("FACILITY_ID", "FIN_ASSUR_ID"))
  erp_monthly <- expired_contracts[, {erp_months <- seq(erp_start, erp_end, by = "month"); .(YEAR = year(erp_months), MONTH = month(erp_months), erp_coverage = TRUE)}, by = .(FACILITY_ID, FIN_ASSUR_ID)]
  erp_facility_monthly <- erp_monthly[, .(erp_reporting_month = any(erp_coverage, na.rm = TRUE)), by = .(FACILITY_ID, YEAR, MONTH)]
  
  panel <- merge(panel, erp_facility_monthly, by = c("FACILITY_ID", "YEAR", "MONTH"), all.x = TRUE)
  panel[is.na(erp_reporting_month), erp_reporting_month := FALSE]
  rm(fa_monthly_contract, expired_contracts, erp_monthly, erp_facility_monthly); gc()

  panel[, `:=`(coverage_gap_month = (!fr_covered) & (!erp_reporting_month), ISSUER_NAME = fifelse((!fr_covered) & (!erp_reporting_month), "NO COVERAGE", ISSUER_NAME), DETAIL_TYPE = fifelse((!fr_covered) & (!erp_reporting_month), "NO COVERAGE", DETAIL_TYPE), CATEGORY = fifelse((!fr_covered) & (!erp_reporting_month), "NO COVERAGE", CATEGORY))]
  panel[YEAR < 1999, `:=`(ISSUER_NAME = "State Fund", DETAIL_TYPE = "State Insurance", CATEGORY = "State Fund")]
  
  setkey(panel, FACILITY_ID, YEAR, MONTH)
  panel[, `:=`(prev_issuer = shift(ISSUER_NAME, type = "lag"), prev_category = shift(CATEGORY, type = "lag"), prev_detail_type = shift(DETAIL_TYPE, type = "lag"), prev_fr_covered = shift(fr_covered, type = "lag"), prev_coverage_gap = shift(coverage_gap_month, type = "lag")), by = FACILITY_ID][, `:=`(issuer_changed = (!is.na(prev_issuer) & !is.na(ISSUER_NAME) & ISSUER_NAME != prev_issuer), mech_changed = (!is.na(prev_category) & !is.na(CATEGORY) & CATEGORY != prev_category), detail_type_changed = (!is.na(prev_detail_type) & !is.na(DETAIL_TYPE) & DETAIL_TYPE != prev_detail_type))][, `:=`(contract_changed = (issuer_changed | mech_changed | detail_type_changed), changed_to_self_insure = (mech_changed & grepl("Self-Insurance", CATEGORY, ignore.case = TRUE) & !is.na(prev_category) & !grepl("Self-Insurance", prev_category, ignore.case = TRUE)), changed_to_insurance = (mech_changed & grepl("Insurance", CATEGORY, ignore.case = TRUE) & !is.na(prev_category) & !grepl("Insurance", prev_category, ignore.case = TRUE)), changed_to_no_coverage = (coverage_gap_month == TRUE & !is.na(prev_coverage_gap) & prev_coverage_gap == FALSE), changed_from_no_coverage = (coverage_gap_month == FALSE & !is.na(prev_coverage_gap) & prev_coverage_gap == TRUE))]
  panel[, c("prev_issuer", "prev_category", "prev_detail_type", "prev_fr_covered", "prev_coverage_gap") := NULL]
  message("Gap detection and change indicators complete.")

  # --- 2.2. Batch Data Merges ---
  message("Performing batch data merges...")
  fac_info <- readRDS(file.path(panel_stage_dir, "fac_info.rds")); panel <- merge(panel, fac_info, by = "FACILITY_ID", all.x = TRUE); rm(fac_info); gc(); message("Merged facility information.")
  fac_contact_months <- readRDS(file.path(panel_stage_dir, "fac_contact_months.rds")); panel <- merge(panel, fac_contact_months, by = c("FACILITY_ID", "YEAR", "MONTH"), all.x = TRUE); rm(fac_contact_months); gc(); message("Merged contact data.")
  ust_monthly <- readRDS(file.path(panel_stage_dir, "ust_monthly.rds")); panel <- merge(panel, ust_monthly, by = c("FACILITY_ID", "YEAR", "MONTH"), all.x = TRUE); rm(ust_monthly); gc(); message("Merged UST metrics.")
  zurich_2012_lookup <- readRDS(file.path(panel_stage_dir, "zurich_2012_lookup.rds")); panel <- merge(panel, zurich_2012_lookup, by = "FACILITY_ID", all.x = TRUE); rm(zurich_2012_lookup); gc(); message("Merged Zurich lookup.")
  self_cert_monthly <- readRDS(file.path(panel_stage_dir, "self_cert_monthly.rds")); panel <- merge(panel, self_cert_monthly, by = c("FACILITY_ID", "YEAR", "MONTH"), all.x = TRUE); rm(self_cert_monthly); gc(); message("Merged self-certification.")

  # --- 2.3. Post-Merge Processing ---
  message("Adding FIPS codes and other derived variables...")
  data("fips_codes"); tx_county_fips <- as.data.table(fips_codes)[state == "TX", .(county_clean = toupper(gsub("\\s*County$", "", county)), fips = paste0(state_code, county_code))]; tx_county_fips <- unique(tx_county_fips, by = c("county_clean", "fips"))
  panel[, SITE_COUNTY_upper := toupper(SITE_COUNTY)]; panel <- merge(panel, tx_county_fips[, .(SITE_COUNTY_upper = county_clean, fips)], by = "SITE_COUNTY_upper", all.x = TRUE); panel[, SITE_COUNTY_upper := NULL]
  
  panel[, any_tank_active := active_tank_count > 0]
  panel[, `:=`(is_facility_entry_event = any_tank_active & !shift(any_tank_active, type="lag", fill=FALSE), is_facility_exit_event  = !any_tank_active & shift(any_tank_active, type="lag", fill=FALSE)), by = FACILITY_ID]
  
  panel[, `:=`(had_zurich_2012 = fifelse(is.na(had_zurich_2012), 0L, had_zurich_2012), dropped_by_zurich = had_zurich_2012 * as.integer(YEAR >= 2013))]
  
  change_metrics <- c("total_capacity", "num_interstitial_det", "num_electronic_det", "num_manual_stat_det", "num_steel_tanks", "num_fiberglass_tanks", "num_jacketed_tanks")
  lag_cols <- paste0("lag_", change_metrics); panel[, (lag_cols) := lapply(.SD, shift, type = "lag"), .SDcols = change_metrics, by = FACILITY_ID]
  delta_cols <- paste0(change_metrics, "_delta"); event_cols <- paste0(change_metrics, "_change_event")
  for (i in seq_along(change_metrics)) {
    orig_col <- change_metrics[i]; lag_col <- lag_cols[i]; delta_col <- delta_cols[i]; event_col <- event_cols[i]
    panel[, (delta_col) := fifelse(is.na(get(lag_col)), 0, get(orig_col) - get(lag_col))]; panel[, (event_col) := as.integer(!is.na(get(lag_col)) & get(orig_col) != get(lag_col))]
  }
  panel[, (lag_cols) := NULL]; setnames(panel, "total_capacity_delta", "capacity_delta_gal"); setnames(panel, "total_capacity_change_event", "capacity_change_event")
  
  # --- 2.4. LUST Data Merge ---
  message("Merging LUST data...")
  lust_report_months <- readRDS(file.path(lust_stage_dir, "lust_report_months.rds")); panel <- merge(panel, lust_report_months, by = c("FACILITY_ID", "YEAR", "MONTH"), all.x = TRUE); rm(lust_report_months); gc()
  lust_monthly_status <- readRDS(file.path(lust_stage_dir, "lust_monthly_status.rds")); panel <- merge(panel, lust_monthly_status, by = c("FACILITY_ID", "YEAR", "MONTH"), all.x = TRUE); rm(lust_monthly_status); gc()
  facility_lust_summary <- readRDS(file.path(lust_stage_dir, "facility_lust_summary.rds")); panel <- merge(panel, facility_lust_summary, by = "FACILITY_ID", all.x = TRUE); rm(facility_lust_summary); gc()
  
  lust_flags <- c("is_lust_report_month", "lust_active", "lust_pre_2000_active", "lust_post_2000_active", "has_subsidized_leak_active", "lust_open_active", "lust_closed_active", "lust_ever", "lust_pre_2000", "lust_post_2000", "has_subsidized_leak", "lust_ever_closed", "lust_ever_open", "lust_after_closure_ever")
  panel[, (lust_flags) := lapply(.SD, function(x) fifelse(is.na(x), 0L, x)), .SDcols = lust_flags]
  panel[, `:=`(closure_not_final_event = as.integer(lust_after_closure_ever == 1L)), by = FACILITY_ID]
  message("LUST indicators merged successfully.")

  # --- 2.5. Census Data Merge ---
  message("Merging Census data...")
  census_data_panel <- readRDS(file.path(census_stage_dir, "census_data_panel.rds"))
  if("fips" %in% names(panel) && "fips" %in% names(census_data_panel)) {
    if(!is.character(panel$fips)) panel[, fips := as.character(fips)]
    if(!is.character(census_data_panel$fips)) census_data_panel[, fips := as.character(fips)]
  }
  if("YEAR" %in% names(panel) && "YEAR" %in% names(census_data_panel)) {
    if(!is.integer(panel$YEAR)) panel[, YEAR := as.integer(YEAR)]
    if(!is.integer(census_data_panel$YEAR)) census_data_panel[, YEAR := as.integer(YEAR)]
  }
  panel <- merge(panel, census_data_panel, by = c("fips", "YEAR"), all.x = TRUE)
  rm(census_data_panel); gc()
  message("Updated census data merged to panel.")

  # --- 3. Final NA Fill and Save ---
  message("Applying final NA fill to panel...")
  # This loop handles all remaining NA values for key numeric/logical columns
  cols_to_fill_zero <- c(event_cols, delta_cols, lust_flags, "is_facility_entry_event", "is_facility_exit_event", "closure_not_final_event")
  cols_to_fill_zero <- intersect(cols_to_fill_zero, names(panel))
  for (col in cols_to_fill_zero) {
      panel[is.na(get(col)), (col) := 0L]
  }
  # Broader fill for any remaining logicals/numerics
  logical_cols <- names(panel)[sapply(panel, is.logical)]
  if (length(logical_cols) > 0) { for (col in logical_cols) panel[is.na(get(col)), (col) := FALSE] }
  numeric_cols <- names(panel)[sapply(panel, is.numeric)]
  if (length(numeric_cols) > 0) { for (col in numeric_cols) panel[is.na(get(col)) | is.nan(get(col)), (col) := 0] }

  message("Final row count: ", format(nrow(panel), big.mark = ","))
  message("Unique facilities: ", uniqueN(panel$FACILITY_ID))

  message("Saving final panel data...")
  fwrite(panel, here("Data", "Processed", "texas_fr_panel.csv"))
  saveRDS(panel, here("Data", "Processed", "texas_fr_panel.rds"))
  if (requireNamespace("fst", quietly = TRUE)) {
    fst::write_fst(panel, here("Data", "Processed", "texas_fr_panel.fst"))
  }
  message("Panel data saved to ", here("Data", "Processed"))

} else {
  message("\n--- Staging Complete. Skipping full merge (do_merge = FALSE) ---")
}

# Performance monitoring
section_end <- Sys.time()
message("Section 7/8/9 (Staging & Merge) completed in ", round(difftime(section_end, section7_start, units = "mins"), 2), " minutes")
message("Script finished.")

# ... (Section 11 and the column dictionary can follow




# Start timing for performance monitoring

# ###############################################################################
# # QUICK-STAGE ALL MERGE TABLES TO DISK  (place just before the first merge)
# ###############################################################################
# stage_dir <- get_data_path("Outputs", "panel_merge_staging")
# if (!dir.exists(stage_dir)) dir.create(stage_dir, recursive = TRUE)   # make once

# datasets_to_stage <- list(
#   fa_monthly            = fa_monthly,
#   erp_facility_monthly  = erp_facility_monthly,
#   fac_info              = fac[ , .(FACILITY_ID, FACILITY_NAME,
#                                    FACILITY_TYPE, FACILITY_STATUS,
#                                    SITE_COUNTY, SITE_ZIP,
#                                    NUM_ACTIVE_USTS_is_blank,
#                                    NUM_ACTIVE_ASTS_is_blank)],
#   fac_contact_months    = fac_contact_months,
#   ust_monthly           = ust_monthly,
#   zurich_2012_lookup    = zurich_2012_lookup,
#   self_cert_monthly     = self_cert_monthly
#   # add / comment–out here as needed
# )

# for (nm in names(datasets_to_stage)) {
#   dt <- datasets_to_stage[[nm]]
#   csv_path <- file.path(stage_dir, paste0(nm, ".csv"))
#   rds_path <- file.path(stage_dir, paste0(nm, ".rds"))

#   data.table::fwrite(dt, csv_path)   # fast text ☞ fread later :contentReference[oaicite:3]{index=3}
#   saveRDS(dt, rds_path)              # binary ☞ readRDS later  :contentReference[oaicite:4]{index=4}
#   message("Wrote ", nm, " → ",
#           basename(csv_path), " & ", basename(rds_path))
# }
# ###############################################################################
# # >>> proceed with setkey() / merge() calls here <<<
# ###############################################################################


# section7_start <- Sys.time()
# message("Processing Section 7: Building main panel and merging data...")

# uniqueN(panel$FACILITY_ID) # Check how many unique facility IDs we have in the panel
# # Merge Financial Assurance monthly data (fa_monthly)

# setkey(panel,      FACILITY_ID, YEAR, MONTH)   # index once
# setkey(fa_monthly, FACILITY_ID, YEAR, MONTH)


# panel <- merge(panel, fa_monthly, by = c("FACILITY_ID", "YEAR", "MONTH"), all.x = TRUE)

# uniqueN(panel$FACILITY_ID) # Check how many unique facility IDs we have after merging FA data


# message("Final row count: ", format(nrow(panel), big.mark = ","))
# message("Unique facilities: ", uniqueN(panel$FACILITY_ID))# ── Section 7.1: Gap Detection, ERP Logic, and Change Indicators ──────────────
# # Purpose: After merging FA data into the complete panel, identify coverage gaps,
# #   apply ERP logic, detect contract/issuer changes, and flag transitions.
# # Why: Gap detection requires the complete facility-month panel, not just FA data.
# # Input: panel with fa_monthly merged (some months will have no FA coverage)
# # Output: panel with gap flags, ERP indicators, and month-to-month change flags
# # ──────────────────────────────────────────────────────────────────────────────

# message("Processing gap detection and change indicators after FA merge...")

# # ── Step 1: Identify Coverage Gaps ───────────────────────────────────────────
# # Set fr_covered to FALSE for months with no FA data
# panel[is.na(fr_covered), fr_covered := FALSE]

# # ── Step 2: Calculate ERP (Extended Reporting Period) Coverage ───────────────
# message("Calculating ERP coverage...")

# # Optimized ERP calculation with pre-filtering
# expired_contracts <- fa_monthly_contract[
#   !is.na(EXP_DATE),
#   .(FACILITY_ID, FIN_ASSUR_ID, EXP_DATE,
#     erp_start = floor_date(EXP_DATE + months(1), "month"),
#     erp_end = floor_date(EXP_DATE + months(6), "month"))
# ][!is.na(erp_start) & !is.na(erp_end)]

# # Remove duplicates (same contract-facility combination)
# expired_contracts <- unique(expired_contracts, by = c("FACILITY_ID", "FIN_ASSUR_ID"))

# # Expand ERP periods to monthly observations
# erp_monthly <- expired_contracts[, {
#   erp_months <- seq(erp_start, erp_end, by = "month")
#   .(YEAR = year(erp_months),
#     MONTH = month(erp_months),
#     erp_coverage = TRUE)
# }, by = .(FACILITY_ID, FIN_ASSUR_ID)]

# # Aggregate ERP coverage by facility-month
# erp_facility_monthly <- erp_monthly[, .(
#   erp_reporting_month = any(erp_coverage, na.rm = TRUE)
# ), by = .(FACILITY_ID, YEAR, MONTH)]

# # Merge ERP coverage into panel
# panel <- merge(panel, erp_facility_monthly, by = c("FACILITY_ID", "YEAR", "MONTH"), all.x = TRUE)
# panel[is.na(erp_reporting_month), erp_reporting_month := FALSE]

# # ── Step 3: Final Gap Detection ──────────────────────────────────────────────
# # Combined gap detection and sentinel value setting
# panel[, `:=`(
#   coverage_gap_month = (!fr_covered) & (!erp_reporting_month),
#   # Set sentinel values for gap months in same operation
#   ISSUER_NAME = fifelse((!fr_covered) & (!erp_reporting_month), "NO COVERAGE", ISSUER_NAME),
#   DETAIL_TYPE = fifelse((!fr_covered) & (!erp_reporting_month), "NO COVERAGE", DETAIL_TYPE),
#   CATEGORY = fifelse((!fr_covered) & (!erp_reporting_month), "NO COVERAGE", CATEGORY)
# )]

# # 1) For any month before 1999, force the “State Fund” values
# panel[ YEAR < 1999, `:=`(
#   ISSUER_NAME = "State Fund",
#   DETAIL_TYPE = "State Insurance",
#   CATEGORY    = "State Fund"
# ) ]


# message("Coverage gaps identified: ", panel[coverage_gap_month == TRUE, .N], " facility-months")

# # ── Step 4: Contract/Issuer Change Detection (Optimized) ─────────────────────
# # Set keys once for optimal performance
# setkey(panel, FACILITY_ID, YEAR, MONTH)

# # Batch calculation of previous month values and change detection
# panel[, `:=`(
#   # Previous month values
#   prev_issuer = shift(ISSUER_NAME, type = "lag"),
#   prev_category = shift(CATEGORY, type = "lag"),
#   prev_detail_type = shift(DETAIL_TYPE, type = "lag"),
#   prev_fr_covered = shift(fr_covered, type = "lag"),
#   prev_coverage_gap = shift(coverage_gap_month, type = "lag")
# ), by = FACILITY_ID][, `:=`(
#   # Change detection in same operation
#   issuer_changed = (!is.na(prev_issuer) & !is.na(ISSUER_NAME) & ISSUER_NAME != prev_issuer),
#   mech_changed = (!is.na(prev_category) & !is.na(CATEGORY) & CATEGORY != prev_category),
#   detail_type_changed = (!is.na(prev_detail_type) & !is.na(DETAIL_TYPE) & DETAIL_TYPE != prev_detail_type)
# )][, `:=`(
#   # Derived indicators
#   contract_changed = (issuer_changed | mech_changed | detail_type_changed),
#   # Specific transition types
#   changed_to_self_insure = (mech_changed & 
#                            grepl("Self-Insurance", CATEGORY, ignore.case = TRUE) & 
#                            !is.na(prev_category) &
#                            !grepl("Self-Insurance", prev_category, ignore.case = TRUE)),
#   changed_to_insurance = (mech_changed &
#                          grepl("Insurance", CATEGORY, ignore.case = TRUE) & 
#                          !is.na(prev_category) &
#                          !grepl("Insurance", prev_category, ignore.case = TRUE)),
#   changed_to_no_coverage = (coverage_gap_month == TRUE & 
#                            !is.na(prev_coverage_gap) & 
#                            prev_coverage_gap == FALSE),
#   changed_from_no_coverage = (coverage_gap_month == FALSE & 
#                              !is.na(prev_coverage_gap) & 
#                              prev_coverage_gap == TRUE)
# )]

# # Clean up temporary columns
# panel[, c("prev_issuer", "prev_category", "prev_detail_type", 
#           "prev_fr_covered", "prev_coverage_gap") := NULL]

# # ── Step 5: Summary Statistics ───────────────────────────────────────────────
# message("Change indicator summary:")
# change_summary <- panel[, .(
#   total_facility_months = .N,
#   coverage_gaps = sum(coverage_gap_month, na.rm = TRUE),
#   erp_months = sum(erp_reporting_month, na.rm = TRUE),
#   issuer_changes = sum(issuer_changed, na.rm = TRUE),
#   mechanism_changes = sum(mech_changed, na.rm = TRUE),
#   contract_changes = sum(contract_changed, na.rm = TRUE),
#   transitions_to_self = sum(changed_to_self_insure, na.rm = TRUE),
#   transitions_to_insurance = sum(changed_to_insurance, na.rm = TRUE),
#   transitions_to_gaps = sum(changed_to_no_coverage, na.rm = TRUE),
#   transitions_from_gaps = sum(changed_from_no_coverage, na.rm = TRUE)
# )]

# print(change_summary)

# # ── Step 6: Validation Checks ────────────────────────────────────────────────
# inconsistent_erp <- panel[erp_reporting_month == TRUE & fr_covered == TRUE, .N]
# if (inconsistent_erp > 0) {
#   warning("Found ", inconsistent_erp, " months with both active coverage and ERP - this may indicate overlapping contracts")
# }

# gap_with_coverage <- panel[coverage_gap_month == TRUE & fr_covered == TRUE, .N]
# if (gap_with_coverage > 0) {
#   warning("Found ", gap_with_coverage, " months marked as gaps but with active coverage - check logic")
# }

# message("Gap detection and change indicators complete.")
# message("Coverage gaps: ", panel[coverage_gap_month == TRUE, .N], " facility-months")
# message("Contract changes: ", panel[contract_changed == TRUE, .N], " facility-months")
# message("Merged monthly financial assurance data (fa_monthly).") 

# # ── Batch Data Merges (Optimization 2) ──────────────────────────────────────
# message("Performing batch data merges...")

# # Define merge datasets for batch processing
# merge_datasets <- list(
#   list(data = fac[, .(FACILITY_ID, FACILITY_NAME, FACILITY_TYPE, FACILITY_STATUS, 
#                      SITE_COUNTY, SITE_ZIP, NUM_ACTIVE_USTS_is_blank, NUM_ACTIVE_ASTS_is_blank)], 
#        keys = "FACILITY_ID", name = "facility information"),
#   list(data = fac_contact_months, keys = c("FACILITY_ID", "YEAR", "MONTH"), name = "contact data"),
#   list(data = ust_monthly, keys = c("FACILITY_ID", "YEAR", "MONTH"), name = "UST metrics"),
#   list(data = zurich_2012_lookup, keys = "FACILITY_ID", name = "Zurich lookup"),
#   list(data = self_cert_monthly, keys = c("FACILITY_ID", "YEAR", "MONTH"), name = "self-certification")
# )

# for(merge_info in merge_datasets) {
#   panel <- merge(panel, merge_info$data, by = merge_info$keys, all.x = TRUE)
#   message("Merged ", merge_info$name)
# }

# # ── FIPS Code Mapping (Optimization 3) ──────────────────────────────────────
# message("Adding FIPS codes based on SITE_COUNTY...")
# data("fips_codes")
# tx_county_fips <- as.data.table(fips_codes)[
#   state == "TX", 
#   .(county_clean = toupper(gsub("\\s*County$", "", county)),
#     fips = paste0(state_code, county_code))
# ]
# tx_county_fips <- unique(tx_county_fips, by = c("county_clean", "fips"))

# panel[, SITE_COUNTY_upper := toupper(SITE_COUNTY)]
# panel <- merge(panel, tx_county_fips[, .(SITE_COUNTY_upper = county_clean, fips)],
#                by = "SITE_COUNTY_upper", all.x = TRUE)
# panel[, SITE_COUNTY_upper := NULL]

# # Handle unmapped counties
# unmapped_counties <- panel[is.na(fips) & !is.na(SITE_COUNTY), unique(SITE_COUNTY)]
# if (length(unmapped_counties) > 0) {
#   message("Warning: Unmapped Texas counties: ", paste(unmapped_counties, collapse = ", "))
# }

# # facility status flags
# panel[
#   , any_tank_active := active_tank_count > 0
# ]

# panel[
#   , `:=`(
#       is_facility_entry_event = any_tank_active & !shift(any_tank_active, type="lag", fill=FALSE),
#       is_facility_exit_event  = !any_tank_active & shift(any_tank_active, type="lag", fill=FALSE)
#   ),
#   by = FACILITY_ID
# ]

# # ── Owner/Operator Difference Calculation (Optimization 1) ──────────────────
# message("Calculating owner-operator differences...")

# # Memory-efficient calculation for large datasets
# if(nrow(panel) > 500000) {
#   # Process in chunks for very large datasets
#   chunk_size <- 100000
#   chunks <- split(seq_len(nrow(panel)), ceiling(seq_len(nrow(panel))/chunk_size))
  
#   for(i in seq_along(chunks)) {
#     chunk_idx <- chunks[[i]]
#     panel[chunk_idx, owner_operator_diff := {
#       mapply(function(o_names, op_names) {
#         if(is.na(o_names) || is.na(op_names) || o_names == "" || op_names == "") return(FALSE)
#         o_set <- unique(trimws(strsplit(o_names, " \\| ", fixed = TRUE)[[1]]))
#         op_set <- unique(trimws(strsplit(op_names, " \\| ", fixed = TRUE)[[1]]))
#         length(intersect(o_set, op_set)) == 0
#       }, owner_names_concatenated, operator_names_concatenated)
#     }]
#     if(i %% 5 == 0) message("Processed chunk ", i, " of ", length(chunks))
#   }
# } else {
#   # Standard processing for smaller datasets
#   panel[, owner_operator_diff := {
#     mapply(function(o_names, op_names) {
#       if(is.na(o_names) || is.na(op_names) || o_names == "" || op_names == "") return(FALSE)
#       o_set <- unique(trimws(strsplit(o_names, " \\| ", fixed = TRUE)[[1]]))
#       op_set <- unique(trimws(strsplit(op_names, " \\| ", fixed = TRUE)[[1]]))
#       length(intersect(o_set, op_set)) == 0
#     }, owner_names_concatenated, operator_names_concatenated)
#   }]
# }

# message("Calculated owner-operator difference.")

# # ── Zurich Processing ────────────────────────────────────────────────────────
# panel[, `:=`(
#   had_zurich_2012 = fifelse(is.na(had_zurich_2012), 0L, had_zurich_2012),
#   dropped_by_zurich = had_zurich_2012 * as.integer(YEAR >= 2013)
# )]

# # ── Efficient Change Calculations (Optimization 4) ──────────────────────────
# message("Computing change metrics...")

# # Define all change columns
# change_metrics <- c("total_capacity", "num_interstitial_det", "num_electronic_det", 
#                    "num_manual_stat_det", "num_steel_tanks", "num_fiberglass_tanks", 
#                    "num_jacketed_tanks")

# # Vectorized lag calculation
# lag_cols <- paste0("lag_", change_metrics)
# panel[, (lag_cols) := lapply(.SD, shift, type = "lag"), 
#       .SDcols = change_metrics, by = FACILITY_ID]

# # Vectorized delta and event calculation using Map
# delta_cols <- paste0(change_metrics, "_delta")
# event_cols <- paste0(change_metrics, "_change_event")

# for (i in seq_along(change_metrics)) {
#   orig_col <- change_metrics[i]
#   lag_col <- lag_cols[i]
#   delta_col <- delta_cols[i]
#   event_col <- event_cols[i]
  
#   panel[, (delta_col) := fifelse(is.na(get(lag_col)), 0, get(orig_col) - get(lag_col))]
#   panel[, (event_col) := as.integer(!is.na(get(lag_col)) & get(orig_col) != get(lag_col))]
# }

# # Clean up lag columns and handle special cases
# panel[, (lag_cols) := NULL]
# setnames(panel, "total_capacity_delta", "capacity_delta_gal")
# setnames(panel, "total_capacity_change_event", "capacity_change_event")

# # Handle remaining NAs efficiently
# delta_event_cols <- c(delta_cols, event_cols)
# for(col in delta_event_cols) {
#   if(col %in% names(panel)) panel[is.na(get(col)), (col) := 0L]
# }

# # ── Optimized LUST Processing (Optimization 5) ──────────────────────────────
# message("Processing LUST data...")

# # Load and preprocess LUST data efficiently
# TX_LUST_SD <- if(onserver) {
#   fread(get_data_path("TX_LUST.csv"))
# } else {
#   fread("C:/Users/kaleb/Box/UST-Insurance/Data/Raw_do_not_write/state_databases/Texas/TX_LUST.csv")
# }

# # Batch processing for LUST data
# if ("facility_id" %in% names(TX_LUST_SD)) setnames(TX_LUST_SD, "facility_id", "FACILITY_ID")
# TX_LUST_SD <- clean_id_column(TX_LUST_SD, "FACILITY_ID")

# # Efficient date processing and filtering
# TX_LUST_SD[, `:=`(
#   report_date = dmy(report_date),
#   nfa_date = dmy(nfa_date)
# )][
#   # Pre-filter for date range and facilities
#   !is.na(report_date) & 
#   report_date >= as.Date("1990-01-01") & 
#   report_date <= as.Date("2026-12-31") &
#   FACILITY_ID %in% facilities_active_1990$FACILITY_ID
# ]

# message("LUST records filtered to ", nrow(TX_LUST_SD), " relevant records")

# # Optimized LUST expansion
# expand_lust_optimized <- function(lust_dt) {
#   lust_dt[!is.na(report_date), {
#     start_month <- floor_date(report_date, "month")
#     end_month <- floor_date(fifelse(is.na(nfa_date), as.Date("2026-12-31"), nfa_date), "month")
    
#     if(start_month <= end_month) {
#       month_seq <- seq(start_month, end_month, by = "month")
#       .(YEAR = year(month_seq),
#         MONTH = month(month_seq),
#         lust_active = TRUE,
#         is_pre2000 = report_date < as.Date("2000-01-01"),
#         has_subsidy = report_date < as.Date("2000-01-01"),
#         nfa_month = end_month)
#     }
#   }, by = .(FACILITY_ID, LUST_id)]
# }

# # LUST report months
# lust_report_months <- TX_LUST_SD[!is.na(report_date), .(
#   FACILITY_ID, YEAR = year(report_date), MONTH = month(report_date),
#   is_lust_report_month = 1L
# )]

# # Expand and aggregate LUST data
# lust_long <- expand_lust_optimized(TX_LUST_SD)

# lust_monthly_status <- lust_long[, .(
#   lust_active = as.integer(any(lust_active)),
#   lust_pre_2000_active = as.integer(any(lust_active & is_pre2000)),
#   lust_post_2000_active = as.integer(any(lust_active & !is_pre2000)),
#   has_subsidized_leak_active = as.integer(any(lust_active & has_subsidy)),
#   lust_open_active = as.integer(any(lust_active & (nfa_month > make_date(YEAR, MONTH, 1)))),
#   lust_closed_active = as.integer(any(lust_active & (nfa_month <= make_date(YEAR, MONTH, 1))))
# ), by = .(FACILITY_ID, YEAR, MONTH)]

# facility_lust_summary <- TX_LUST_SD[, .(
#   lust_ever = 1L,
#   lust_pre_2000 = as.integer(any(report_date < "2000-01-01")),
#   lust_post_2000 = as.integer(any(report_date >= "2000-01-01")),
#   has_subsidized_leak = as.integer(any(report_date < "2000-01-01")),
#   lust_ever_closed = as.integer(any(!is.na(nfa_date))),
#   lust_ever_open = as.integer(any(is.na(nfa_date))),
#   lust_after_closure_ever = as.integer(any(!is.na(nfa_date) & nfa_date < as.Date("2026-12-31") & nfa_date > report_date))
# ), by = FACILITY_ID]

# # Batch LUST merges
# lust_datasets <- list(
#   list(data = lust_report_months, keys = c("FACILITY_ID", "YEAR", "MONTH")),
#   list(data = lust_monthly_status, keys = c("FACILITY_ID", "YEAR", "MONTH")),
#   list(data = facility_lust_summary, keys = "FACILITY_ID")
# )


# ###############################################################################
# # QUICK-STAGE LUST TABLES TO DISK  (insert right before the merges)
# ###############################################################################
# lust_stage_dir <- get_data_path("Outputs", "lust_merge_staging")
# if (!dir.exists(lust_stage_dir)) dir.create(lust_stage_dir, recursive = TRUE)

# lust_to_stage <- list(
#   lust_report_months    = lust_report_months,
#   lust_monthly_status   = lust_monthly_status,
#   facility_lust_summary = facility_lust_summary
# )

# for (nm in names(lust_to_stage)) {
#   dt <- lust_to_stage[[nm]]
#   csv_path <- file.path(lust_stage_dir, paste0(nm, ".csv"))
#   rds_path <- file.path(lust_stage_dir, paste0(nm, ".rds"))

#   data.table::fwrite(dt, csv_path)   # fast text
#   saveRDS(dt, rds_path)              # compact binary
#   message("Staged ", nm, " → ",
#           basename(csv_path), " & ", basename(rds_path))
# }
# ###############################################################################
# # >>> now proceed with the lust_datasets merge loop <<<
# ###############################################################################


# for(lust_info in lust_datasets) {
#   panel <- merge(panel, lust_info$data, by = lust_info$keys, all.x = TRUE)
# }

# # Batch NA handling for LUST flags
# lust_flags <- c("is_lust_report_month", "lust_active", "lust_pre_2000_active", 
#                "lust_post_2000_active", "has_subsidized_leak_active", 
#                "lust_open_active", "lust_closed_active", "lust_ever", 
#                "lust_pre_2000", "lust_post_2000", "has_subsidized_leak", 
#                "lust_ever_closed", "lust_ever_open", "lust_after_closure_ever")

# panel[, (lust_flags) := lapply(.SD, function(x) fifelse(is.na(x), 0L, x)), 
#       .SDcols = lust_flags]

# panel[, `:=`(
#   closure_not_final_event = as.integer(lust_after_closure_ever == 1L)
# ), by = FACILITY_ID]

# # And then clean up any NAs:
#  for(col in c("is_facility_entry_event","is_facility_exit_event","closure_not_final_event")){
#    panel[is.na(get(col)), (col) := 0L]
#  }

# message("LUST indicators merged successfully.")

# # ── Memory Management (Optimization 6) ──────────────────────────────────────
# if(nrow(panel) > 1000000) {
#   gc(verbose = FALSE)
#   message("Memory cleaned after major operations")
# }

# # Performance monitoring
# section7_end <- Sys.time()
# message("Section 7 completed in ", round(difftime(section7_end, section7_start, units = "mins"), 2), " minutes")

# message("Section 7 data processing complete.")

# # ── Section 8: Census data merge ─────────────────────────────────────────────
# # Purpose: read or (disabled) fetch ACS/FRED/county-PEP data, clean/populate pop, income, Gini,
# #   expand to yearly county panel and then merge to facility-month panel via county FIPS & YEAR.
# # Why: to attach local socioeconomic context to each facility-month.
# # End product: enriched `panel` with census covariates by fips & YEAR.
# message("Processing: Downloading updated census data for panel merge…")

# ###############################################################################
# # 1. Meta: keep a tidy lookup of variable ↔ units for future reference
# ###############################################################################
# units_map <- data.table(
#   variable = c("total_pop", "median_hh_income", "gini_index"),
#   units    = c("persons",  "US dollars (nominal-year)", "unitless index (0–1)")
# )

# # Helper function to expand 5-year ACS bins to every calendar year
# expand_5yr_bin <- function(dt, end_year_col = "survey_end_year", value_cols) {
#   dt_long <- melt(
#     dt,
#     id.vars      = c("fips", end_year_col),
#     measure.vars = value_cols,
#     variable.name = "var",
#     value.name    = "value"
#   )
#   dt_long[, year := as.integer(get(end_year_col))]
#   dt_long <- dt_long[, .(
#       year  = rep((year - 4):year, each = .N / length(value_cols)),
#       fips,
#       var,
#       value
#     ), by = .I]
#   dcast(dt_long, fips + year ~ var, value.var = "value")
# }
# ###############################################################################
# # Texas county totals, 1998-2020  |  U.S. Census Bureau Population Estimates
# ###############################################################################
# library(data.table)

# ## ---- helper to read wide CSV files (2000-09 & 2010-20) ----------------------
# reshape_pop <- function(url, years) {
#   cols <- c("STATE", "COUNTY", paste0("POPESTIMATE", years))
#   dt   <- fread(url, select = cols, colClasses = "character", na.strings = "(X)")
#   dt   <- dt[STATE == "48"]                               # Texas only
#   dt[, fips := sprintf("%02d%03d", as.integer(STATE), as.integer(COUNTY))]
#   long <- melt(dt, id.vars = "fips", measure.vars = patterns("^POPESTIMATE"),
#                variable.name = "year_str", value.name = "total_pop", na.rm = TRUE)
#   long[, year := as.integer(sub("POPESTIMATE", "", year_str))]
#   long[, .(fips, year, total_pop = as.numeric(total_pop))]
# }

# ## ---- Part 1: 2010-2020 wide file -------------------------------------------
# pop_2010_2020 <- reshape_pop(
#   url   = "https://www2.census.gov/programs-surveys/popest/datasets/2010-2020/counties/totals/co-est2020-alldata.csv",  # :contentReference[oaicite:2]{index=2}
#   years = 2010:2020
# )

# ## ---- Part 2: 2000-2009 wide file -------------------------------------------
# pop_2000_2009 <- reshape_pop(
#   url   = "https://www2.census.gov/programs-surveys/popest/datasets/2000-2009/counties/totals/co-est2009-alldata.csv",  # :contentReference[oaicite:3]{index=3}
#   years = 2000:2009
# )

# ## ---- Part 3: 1998-1999 intercensal TXT files -------------------------------
# ###############################################################################
# # QUICK PATCH ― read 1998 & 1999 intercensal files cleanly with **data.table**
# #   •  The `stch-icenYYYY.txt` files are *comma-delimited, no header*.
# #   •  Every record has 6 columns in the order shown below.
# #   •  AGEGRP==0 (= “all ages”), RACE==0, ORIGIN==0 give total population.
# #
# #  NOTE: The earlier error arose because we renamed columns *after* picking
# #        pop_col. Here we (i) add generic V1:V6 names _first_, (ii) pick
# #        pop_col, and (iii) never rename away the V* names we rely on.
# ###############################################################################
# ###############################################################################
# # peek_icen_year()  ―  robust “peek-and-clean” helper for 1990-1999 intercensal
# #
# #  • Works directly with the ST-CO txt files (no header, 6 columns).
# #  • Prints a quick preview so you can eyeball the raw rows.
# #  • Detects which column is the population count (always the last col, V6).
# #  • Keeps rows where STATE==48 (Texas), AGEGRP==0, RACE==1, ORIGIN==1
# #    → those are the bridged-race total-population lines.
# #
# #  NOTE: The earlier error arose because we renamed columns *after* picking
# #        pop_col. Here we (i) add generic V1:V6 names _first_, (ii) pick
# #        pop_col, and (iii) never rename away the V* names we rely on.
# ###############################################################################
# ###############################################################################
# # peek_icen_year()  ―  robust reader for 1990-1999 intercensal ST-CO files
# ###############################################################################
# library(data.table)

# peek_icen_year <- function(yr, state_fips = "48", n_preview = 5) {
#   stopifnot(yr %in% 1990:1999)

#   url <- sprintf(
#     "https://www2.census.gov/programs-surveys/popest/tables/1990-2000/intercensal/st-co/stch-icen%d.txt",
#     yr
#   )

#   # ---- read with generic V1…V6 headers --------------------------------------
#   raw <- fread(url, header = FALSE, colClasses = "character", showProgress = FALSE)
#   setnames(raw, paste0("V", seq_len(ncol(raw))))   # ensure V1:V6

#   message(sprintf("\nPreview of %s (first %d rows):", basename(url), n_preview))
#   print(head(raw, n_preview))

#   # ---- filter to Texas ------------------------------------------------------
#   tx <- raw[
#     substr(V2, 1, 2) == state_fips &  # V2 = 5-digit county FIPS
#     V3 == "0" &                       # AGEGRP 0  (all ages)
#     V4 == "1" &                       # RACE   1  (all races, bridged)
#     V5 == "1" ,                       # ORIGIN 1  (total origin)
#     .(fips      = V2,                 # already 5-digit
#       year      = yr,
#       total_pop = as.numeric(V6))
#   ]

#   message(sprintf("Returned %d county rows for Texas, %d", nrow(tx), yr))
#   return(tx)
# }

# # ---- run the two years ------------------------------------------------------
# pop_1998 <- peek_icen_year(1998)
# pop_1999 <- peek_icen_year(1999)

# # then bind with pop_2000_2009 and pop_2010_2020 as before:
# # pop_dt <- rbindlist(list(pop_1998, pop_1999, pop_2000_2009, pop_2010_2020))
# ###############################################################################
# ###############################################################################

# # afterward: rbindlist(list(pop_1998, pop_1999, pop_2000_2009, pop_2010_2020))
# ###############################################################################


# ## ---- stack & sort -----------------------------------------------------------
# pop_dt <- rbindlist(
#   list(pop_1998, pop_1999, pop_2000_2009, pop_2010_2020),
#   use.names = TRUE
# )[order(fips, year)]

# message(sprintf("Pop table: %s rows, %s-%s", nrow(pop_dt),
#                 min(pop_dt$year), max(pop_dt$year)))

# ###############################################################################
# # pop_dt is ready to left-join into census_data_panel on fips + year
# ###############################################################################

# # Median household income (SAIPE annual) 1998-2020
# ###############################################################################
# if (!requireNamespace("fredr", quietly = TRUE)) install.packages("fredr")
# library(fredr); library(data.table)
# # fredr_set_key("2d9a1f281713753ddce9c4250540c2aa")  # <-- set once in .Renviron
# fredr_set_key("2d9a1f281713753ddce9c4250540c2aa")  # <-- set once in .Renviron
# library(fredr)      # install.packages("fredr")
# library(data.table) # install.packages("data.table")

# # 1 ── full list of Texas county FIPS via tidycensus --------------------------
# if (!requireNamespace("tidycensus", quietly = TRUE)) install.packages("tidycensus")


# # 1 ── pull TX county codes and convert to data.table
# tx_fips <- as.data.table(fips_codes)[state == "TX"]   # filter after conversion
# tx_fips[ , fips := paste0(state_code, county_code)]   # create 5-digit code
# tx_fips <- unique(tx_fips[ , .(fips)])                # keep one row per fips

# # 2 ── add statewide aggregate 48000
# if (!"48000" %in% tx_fips$fips) {
#   tx_fips <- rbind(tx_fips, data.table(fips = "48000"))
# }

# print(head(tx_fips, 6))   # sanity-check: should show 48000 and first counties

# # 2 ── build correct series IDs ----------------------------------------------
# tx_fips[, series_id := paste0("MHITX", fips, "A052NCEN")]
# tail(tx_fips) # check that series_id is correct
# # 3 ── helper (fixed) ---------------------------------------------------------
# get_mhi <- function(id, fips_code) {
#   Sys.sleep(0.5)                           # ← rate-limit politeness
#   tryCatch({
#     obs <- fredr_series_observations(
#       series_id         = id,
#       observation_start = as.Date("1989-01-01"),
#       units             = "lin"
#     )

#     # ---- NEW: convert to data.table before using [ , .(…) ] -----------------
#     obs <- as.data.table(obs)

#     if (nrow(obs) == 0) return(NULL)

#     obs[ , .(
#       fips             = fips_code,
#       year             = year(date),
#       median_hh_income = value
#     )]

#   }, error = function(e) {
#     warning(sprintf("Missing series %s (%s): %s", id, fips_code, e$message))
#     NULL
#   })
# }

# # 4 ── loop & bind (unchanged) ------------------------------------------------
# income_dt <- rbindlist(
#   Map(get_mhi, tx_fips$series_id, tx_fips$fips),
#   use.names = TRUE, fill = TRUE
# )

# income_dt[ , median_hh_income_units := "US dollars (nominal-year)"]

# # Display a summary of the fetched data
# message("Fetched income data for ", uniqueN(income_dt[!is.na(median_hh_income), fips]), " counties.")
# message("Years range from: ", min(income_dt$year, na.rm = TRUE), " to ", max(income_dt$year, na.rm = TRUE))
# print(head(income_dt))
# ###############################################################################
# # sanity-check: all population variables already confirmed in “persons”
# # (see units_map); nothing to rescale.
# ###############################################################################
# library(data.table)      # make sure it’s loaded

# message("Downloading & expanding ACS Gini …")
# acs_end_years <- 2010:2023           # 2006-10 … 2019-23 5-yr files

# gini_list <- lapply(acs_end_years, function(ey) {

#   # pull as tibble …
#   acs_tbl <- get_acs(
#     geography = "county",
#     state     = "TX",
#     table     = "B19083",
#     year      = ey,
#     survey    = "acs5",
#     output    = "wide"               # still returns a tibble
#   )

#   # … then convert *in place* to data.table
#   setDT(acs_tbl)                     # or acs_dt <- as.data.table(acs_tbl)

#   acs_tbl[ , .(
#     fips            = GEOID,
#     gini_index      = B19083_001E,
#     survey_end_year = ey
#   )]
# })

# gini_wide <- expand_5yr_bin(
#   rbindlist(gini_list),
#   end_year_col = "survey_end_year",
#   value_cols   = "gini_index"
# )


# ###############################################################################
# # 3. Merge & attach unit columns
# ###############################################################################
# census_data_panel <- CJ(fips = unique(pop_dt$fips), year = 1998:2020)
# census_data_panel <- merge(census_data_panel, pop_dt,    by = c("fips", "year"), all.x = TRUE)
# census_data_panel <- merge(census_data_panel, income_dt, by = c("fips", "year"), all.x = TRUE)
# census_data_panel <- merge(census_data_panel, gini_wide, by = c("fips", "year"), all.x = TRUE)
# setnames(census_data_panel, "year", "YEAR")

# # ---- add unit columns (constant text) ---------------------------------------
# census_data_panel[ , `:=`(
#   total_pop_units        = "persons",
#   median_hh_income_units = "US dollars (nominal-year)",
#   gini_index_units       = "unitless index (0–1)"
# )]

# ###############################################################################
# # 4. Harmonise population units just in case ----------------------------------
# #    (All three PEP files are in *persons* per the file-layout PDFs, but
# #     this guard rescales if any future vintage is reported in thousands.)
# if (any(census_data_panel$total_pop < 10000, na.rm = TRUE) & any(census_data_panel$total_pop > 0, na.rm = TRUE)) { # Check if any pop value is small but positive
#   # More robust check: if median of non-NA population is very small, it might be in thousands.
#   # This avoids triggering on counties with genuinely small populations if most are large.
#   median_pop <- median(census_data_panel$total_pop, na.rm = TRUE)
#   if (!is.na(median_pop) && median_pop < 10000) { # Arbitrary threshold, adjust as needed
#     warning("Median total_pop is low; values may be in thousands. Rescaling to persons.")
#     census_data_panel[!is.na(total_pop), total_pop := total_pop * 1000]
#   } else if (max(census_data_panel$total_pop, na.rm = TRUE) < 20000 && any(census_data_panel$total_pop > 0, na.rm = TRUE)) { # If max pop is low
#      warning("Maximum total_pop is low; values may be in thousands. Rescaling to persons.")
#      census_data_panel[!is.na(total_pop), total_pop := total_pop * 1000]
#   }
# }


# ###############################################################################
# # 5. Forward- & backward-fill Gini as before, then save
# ###############################################################################
# census_data_panel[order(fips, YEAR),
#   gini_index := zoo::na.locf(gini_index, na.rm = FALSE), by = fips]
# census_data_panel[order(fips, YEAR),
#   gini_index := zoo::na.locf(gini_index, na.rm = FALSE, fromLast = TRUE), by = fips]


# ###############################################################################
# # CPI-U DEFLATOR  → convert nominal MN_MHI to 2023 dollars
# #   • Source: FRED series CPIAUCSL (1982-84 = 100) :contentReference[oaicite:0]{index=0}
# #   • Access via fredr (requires free FRED API key) :contentReference[oaicite:1]{index=1}
# #   • Method: annual average CPI, then multiply by CPI_2023 / CPI_year.

# # ---- 1. pull annual CPI 1998-2023 -------------------------------------------
# cpi_dt <- as.data.table(
#   fredr(
#     series_id        = "CPIAUCSL",
#     observation_start= as.Date("1998-01-01"),
#     observation_end  = as.Date("2023-12-01"),
#     frequency        = "a"     # annual average index
#   )
# )[ , .(YEAR = year(date), CPI = value) ]

# # ---- 2. build deflator (factor = CPI_2023 / CPI_year) -----------------------
# cpi_2023 <- cpi_dt[YEAR == 2023, CPI]
# cpi_dt[ , cpi_factor := cpi_2023 / CPI ]

# # ---- 3. merge & compute real income (2023 $) --------------------------------
# census_data_panel <- merge(
#   census_data_panel, cpi_dt[ , .(YEAR, cpi_factor)], 
#   by = "YEAR", all.x = TRUE
# )

# census_data_panel[ , median_hh_income_real_2023 := 
#                      median_hh_income * cpi_factor ]

# # ---- 4. append units metadata ----------------------------------------------
# census_data_panel[ ,
#   median_hh_income_real_2023_units := "2023 dollars (CPI-U deflated)"
# ]
# units_map <- rbind(
#   units_map,
#   data.table(variable = "median_hh_income_real_2023",
#              units    = "2023 dollars (CPI-U deflated)")
# )

# # optional: drop helper factor
# # census_data_panel[ , cpi_factor := NULL ]
# ###############################################################################


# saveRDS(census_data_panel, file = get_data_path("Outputs", "tx_census_1998_2020.rds"))
# message("Updated census panel created and saved to ", get_data_path("Outputs", "tx_census_1998_2020.rds"))
# # FIX: Removed library(tidyverse) and glimpse calls, converted to data.table or commented
# # library(tidyverse) # Already loaded if needed
# # glimpse(panel[YEAR ==2010]) # FIX: Converted from dplyr
# # nrow(panel)
# # nrow(panel[is.na(fips)])
# # glimpse(panel[YEAR == 2010 & is.na(fips)]) # FIX: Converted from dplyr
# # Left merge the census_data_panel into the panel by fips and YEAR
# # The census_data_panel (containing total_pop, median_hh_income, gini_index)
# # will be merged. Since the previous ACS load was removed, there are no conflicting columns to handle.

# # Ensure fips in panel is character to match census_data_panel$fips if it's character
# if("fips" %in% names(panel) && "fips" %in% names(census_data_panel)) {
#     if(is.factor(panel$fips)) panel[, fips := as.character(fips)]
#     if(!is.character(panel$fips)) panel[, fips := as.character(fips)] # Ensure panel$fips is character
#     if(is.factor(census_data_panel$fips)) census_data_panel[, fips := as.character(fips)]
#     if(!is.character(census_data_panel$fips)) census_data_panel[, fips := as.character(fips)] # Ensure census_data_panel$fips is character

#     if(class(panel$fips) != class(census_data_panel$fips)) {
#         warning(paste("Class mismatch for fips column: panel is", class(panel$fips), 
#                       "and census_data_panel is", class(census_data_panel$fips), 
#                       "- attempting conversion to character for both."))
#         panel[, fips := as.character(fips)]
#         census_data_panel[, fips := as.character(fips)]
#     }
# }
# # Ensure YEAR types match
# if("YEAR" %in% names(panel) && "YEAR" %in% names(census_data_panel)) {
#     if(class(panel$YEAR) != class(census_data_panel$YEAR)) {
#         warning(paste("Class mismatch for YEAR column: panel is", class(panel$YEAR),
#                       "and census_data_panel is", class(census_data_panel$YEAR),
#                       "- attempting conversion to integer for both."))
#         panel[, YEAR := as.integer(YEAR)]
#         census_data_panel[, YEAR := as.integer(YEAR)]
#     }
# }

# ###############################################################################
# # QUICK-STAGE CENSUS TABLES TO DISK  (drop in right after census_data_panel)
# ###############################################################################
# census_stage_dir <- get_data_path("Outputs", "census_merge_staging")
# if (!dir.exists(census_stage_dir)) dir.create(census_stage_dir, recursive = TRUE)  # creates nested dirs :contentReference[oaicite:0]{index=0}

# census_to_stage <- list(
#   pop_dt             = pop_dt,              # Pops 1998-2020
#   income_dt          = income_dt,           # SAIPE Median HH income
#   gini_wide          = gini_wide,           # 5-yr ACS Gini, expanded to years
#   cpi_dt             = cpi_dt,              # CPI factors (if you want them)
#   census_data_panel  = census_data_panel,   # the final, merged county-year panel
#   units_map          = units_map            # lookup of variable ↔ units
# )

# for (nm in names(census_to_stage)) {
#   dt <- census_to_stage[[nm]]
#   csv_path <- file.path(census_stage_dir, paste0(nm, ".csv"))
#   rds_path <- file.path(census_stage_dir, paste0(nm, ".rds"))
  
#   data.table::fwrite(dt, csv_path)   # fast multi-core writer :contentReference[oaicite:1]{index=1}
#   saveRDS(dt, rds_path)              # base-R single-object serializer :contentReference[oaicite:2]{index=2}
  
#   message("Staged ", nm, " → ",
#           basename(csv_path), " & ", basename(rds_path))
# }
# ###############################################################################
# # >>> proceed with your merge(panel, census_data_panel, ...) <<<
# ###############################################################################



# panel <- merge(panel, census_data_panel, by = c("fips", "YEAR"), all.x = TRUE)
# # FIX: Removed setnafill here. One final fill will be applied before saving.
# message("Updated census data merged to panel.")

# # FIX: Consolidated NA fill (moved here from individual merge steps, applied before saving)
# message("Applying final NA fill to panel...")
# if (utils::packageVersion("data.table") >= "1.13.0") {
#     logical_cols <- names(panel)[sapply(panel, is.logical)]
#     if (length(logical_cols) > 0) setnafill(panel, type = "const", fill = FALSE, cols = logical_cols)
    
#     numeric_cols <- names(panel)[sapply(panel, is.numeric)]
#     if (length(numeric_cols) > 0) setnafill(panel, type = "const", fill = 0, nan = 0, cols = numeric_cols)
# } else {
#     # Fallback for older data.table versions
#     logical_cols <- names(panel)[sapply(panel, is.logical)]
#     if (length(logical_cols) > 0) {
#       for (col in logical_cols) panel[is.na(get(col)), (col) := FALSE]
#     }
#     numeric_cols <- names(panel)[sapply(panel, is.numeric)]
#     if (length(numeric_cols) > 0) {
#       for (col in numeric_cols) panel[is.na(get(col)) | is.nan(get(col)), (col) := 0]
#     }
#   }


# # ── Section 9: Save panel ───────────────────────────────────────────────────
# # Purpose: write out final `panel` as CSV, RDS, FST and generate a column dictionary.
# # Why: persist the fully merged facility-month dataset for analysis and documentation.
# # End product: files in Outputs/ (texas_fr_panel.csv, .rds, .fst, column dictionary).
# message("Processing Section 9: Saving panel data...")
# fwrite(panel, get_data_path("Outputs", "texas_fr_panel.csv"))
# message("Panel data saved to ", get_data_path("Outputs", "texas_fr_panel.csv"), ", ", get_data_path("Outputs", "texas_fr_panel.rds"), " and ", get_data_path("Outputs", "texas_fr_panel.fst"))

# ── Sample output: Show complete panel structure for one facility ──────────
message("\n=== SAMPLE PANEL OUTPUT FOR ONE FACILITY ===")
sample_facility <- panel[!is.na(FACILITY_ID)][1, FACILITY_ID]
sample_data <- panel[FACILITY_ID == sample_facility][order(YEAR, MONTH)]
message("Facility ID: ", sample_facility)
message("Panel structure (", nrow(sample_data), " rows from ", min(sample_data$YEAR), "-", sprintf("%02d", min(sample_data$MONTH)), " to ", max(sample_data$YEAR), "-", sprintf("%02d", max(sample_data$MONTH)), "):")

###############################################################################
# Section 11: Pick Example Facilities for Visual Inspection via ust_monthly
###############################################################################

# 1. Compute first & last active month per facility
first_last_ust <- ust_monthly[active_tank_count > 0, .(
  first_active = min(make_date(YEAR, MONTH, 1)),
  last_active  = max(make_date(YEAR, MONTH, 1))
), by = FACILITY_ID]

# 2. Merge back onto ust_monthly
ust_monthly <- merge(
  ust_monthly,
  first_last_ust,
  by = "FACILITY_ID",
  all.x = TRUE
)

# 3. Create four binary (0/1) flags
ust_monthly[, `:=`(
  # Closed all tanks after 1999 but before 2026
  closed_after_1999_before_2026 = as.integer(
    last_active  > as.Date("1999-12-01") &
    last_active  < as.Date("2026-01-01")
  ),

  # Entered before 1990 and exited before 1999
  entered_before_1990_exited_before_1999 = as.integer(
    first_active < as.Date("1990-01-01") &
    last_active  < as.Date("1999-12-01")
  ),

  # Enters after 1999
  enters_after_1999 = as.integer(
    first_active > as.Date("1999-12-01")
  ),

  # Open before 1990 and still open after 1999
  open_before_1990_and_still_after_1999 = as.integer(
    first_active < as.Date("1990-01-01") &
    last_active  > as.Date("1999-12-01")
  )
)]

# 1) define your four flag columns
flag_cols <- c(
  "closed_after_1999_before_2026",
  "entered_before_1990_exited_before_1999",
  "enters_after_1999",
  "open_before_1990_and_still_after_1999"
)

# # 2) Count total flagged facility-months for each flag
# flag_month_counts <- ust_monthly[
#   , lapply(.SD, sum, na.rm = TRUE),
#   .SDcols = flag_cols
# ]
# print(flag_month_counts)

# check = ust_monthly[,.(obs = .N),by=.(Last_YearMonth)]

# exits_by_year = panel[is_facility_exit_event == 1, .N, by = YEAR]

# 3. Print out the examples
cat("
Example facilities for visual checks (from ust_monthly):
  1) closed after 1999 but before 2026: ", example1, "
  2) entered before 1990 and exited before 1999: ", example2, "
  3) entered after 1999: ", example3, "
  4) open before 1990 and still open after 1999: ", example4, "
")

# 4. Now inspect these in the full panel if desired:
    view(panel[FACILITY_ID == example1])
    view(panel[FACILITY_ID == example2])
    view(panel[FACILITY_ID == example3])
    view(panel[FACILITY_ID == example4])


# Create a lookup for column definitions
# This is a manual step based on the script's logic.
column_definitions_list <- list(
  # == Panel Core Identifiers ==
  FACILITY_ID = "character: Unique identifier for the facility.",
  YEAR = "integer: Year of the observation.",
  MONTH = "integer: Month of the observation (1-12).",

  # == Financial Assurance (from fa_monthly) ==
  ISSUER_NAME = "character: Concatenated list of financial mechanism issuer names active in the month. Values are ' | ' separated if multiple. 'NO COVERAGE' if coverage_gap_month is TRUE.",
  DETAIL_TYPE = "character: Concatenated list of financial mechanism detail types active in the month. Values are '; ' separated if multiple. 'NO COVERAGE' if coverage_gap_month is TRUE.",
  CATEGORY = "character: Concatenated list of financial mechanism categories active in the month. Values are '; ' separated if multiple. 'NO COVERAGE' if coverage_gap_month is TRUE.",
  active_FIN_ASSUR_ID = "character: Concatenated list of active financial assurance IDs (FIN_ASSUR_ID) in the month. Values are ' | ' separated if multiple.",
  max_COVER_OCC = "numeric: Maximum per-occurrence coverage amount from financial mechanisms active in the month. NA if no coverage or not applicable.",
  max_COVER_AGG = "numeric: Maximum annual aggregate coverage amount from financial mechanisms active in the month. NA if no coverage or not applicable.",
  total_COVER_OCC = "numeric: Sum of per-occurrence coverage amounts from all financial mechanisms active in the month. 0 if no coverage.",
  total_COVER_AGG = "numeric: Sum of annual aggregate coverage amounts from all financial mechanisms active in the month. 0 if no coverage.",
  premium_prepaid = "logical: TRUE if any active financial mechanism had its premium pre-paid.",
  proof_on_file = "logical: TRUE if proof of financial assurance was on file for any active mechanism.",
  fp_corr_met = "logical: TRUE if first-party corrective action requirements were met by any active mechanism.",
  tp_fa_met = "logical: TRUE if third-party financial assurance requirements were met by any active mechanism.",
  uses_private = "logical: TRUE if 'Insurance' category was active in the month (based on CATEGORY string).",
  uses_self = "logical: TRUE if 'Self-Insurance' category was active in the month (based on CATEGORY string).",
  fr_covered = "logical: TRUE if any financial responsibility coverage was active in the month (i.e., not a coverage gap).",
  transition_month = "logical: TRUE if the month contains a hand-off between exactly two non-overlapping financial mechanisms.",
  multiple_contracts = "logical: TRUE if multiple (potentially overlapping) financial mechanisms were active in the month.",
  erp_reporting_month = "logical: TRUE if the facility-month falls within the 6-month extended reporting period (ERP) after a contract expiry.",
  coverage_gap_month = "logical: TRUE if no FR contract covers the facility-month and it's outside any ERP tail.",
  issuer_changed = "logical: TRUE if the concatenated ISSUER_NAME string changed from the previous month.",
  mech_changed = "logical: TRUE if the concatenated CATEGORY string changed from the previous month.",
  contract_changed = "logical: TRUE if either issuer_changed or mech_changed is TRUE.",
  changed_to_self_insure = "logical: TRUE if CATEGORY changed to include 'Self-Insurance' and did not previously include it.",
  changed_to_insurance = "logical: TRUE if CATEGORY changed to include 'Insurance' and did not previously include it.",
  changed_to_no_coverage = "logical: TRUE if the facility-month transitioned into a 'coverage_gap_month' from a non-gap month.",

  # == Facility Information (from fac) ==
  FACILITY_NAME = "character: Name of the facility.",
  FACILITY_TYPE = "character: Type of the facility.",
  FACILITY_STATUS = "character: Status of the facility as reported in the facility extract.",
  SITE_COUNTY = "character: County where the facility is located.",
  SITE_ZIP = "character: ZIP code of the facility site.",
  NUM_ACTIVE_USTS_is_blank = "logical: TRUE if the original NUM_ACTIVE_USTS field was blank in the facility extract.",
  NUM_ACTIVE_ASTS_is_blank = "logical: TRUE if the original NUM_ACTIVE_ASTS field was blank in the facility extract.",
  fips = "character: 5-digit FIPS code for the facility's county.",

  # == Owner/Operator Information (from fac_contact_months) ==
  owner_ids_concatenated = "character: Semicolon-separated list of unique owner CNs for the facility-month.",
  owner_names_concatenated = "character: Pipe-separated list of unique owner names (CO_LAST) for the facility-month.",
  owner_types_concatenated = "character: Semicolon-separated list of unique owner types for the facility-month.",
  state_tax_ids_concatenated = "character: Semicolon-separated list of unique state tax IDs for owners in the facility-month.",
  any_owner_multi_facility = "logical: TRUE if any owner active in the facility-month owns multiple facilities.",
  num_distinct_owners = "integer: Number of distinct owners (by OWNER_CN) for the facility-month.",
  operator_ids_concatenated = "character: Semicolon-separated list of unique operator CNs for the facility-month.",
  operator_names_concatenated = "character: Pipe-separated list of unique operator names (CO_LAST) for the facility-month.",
  owner_operator_different = "logical: TRUE if the set of owner names and operator names do not fully overlap in the facility-month, or one is empty and the other is not.",
  orphaned_by_owner = "logical: TRUE if owner_ids_concatenated is NA or empty for the facility-month.",
  orphaned_by_operator = "logical: TRUE if operator_ids_concatenated is NA or empty for the facility-month.",

  # == UST Aggregations (from ust_monthly) ==
  total_capacity = "numeric: Sum of capacities (gallons) of all USTs active at the facility in the month.",
  active_tank_count = "integer: Number of unique USTs active at the facility in the month.",
  closed_tank_count = "integer: Number of USTs whose closure (due to 'REMOVED', 'CLOSED' etc. status in raw UST data) was effective in this facility-month.",
  prop_single_wall = "numeric: Proportion of active USTs in the month that are single-walled (based on TANK_SINGLE flag). 0-1 scale.",
  prop_double_wall = "numeric: Proportion of active USTs in the month that are double-walled (based on TANK_DOUBLE flag). 0-1 scale.",
  avg_tank_age_months = "numeric: Average age (in months from their install_date) of USTs active in the month.",
  num_sw_tanks = "integer: Number of active single-walled tanks (TANK_SINGLE='Y') in the facility-month.",
  num_dw_tanks = "integer: Number of active double-walled tanks (TANK_DOUBLE='Y') in the facility-month.",
  num_missing_tanks = "integer: Number of active tanks where both TANK_SINGLE and TANK_DOUBLE are NA in the facility-month.",
  num_unknown_tanks = "integer: Number of active tanks where TANK_SINGLE='N' and TANK_DOUBLE='N' in the facility-month.",
  dw_upgrade_event = "integer: 1 if there was an increase in num_dw_tanks and a corresponding decrease in (num_sw_tanks + num_unknown_tanks + num_missing_tanks) from the previous month, 0 otherwise.",
  num_steel_tanks = "integer: Number of active tanks made of steel in the facility-month.",
  num_fiberglass_tanks = "integer: Number of active tanks made of fiberglass (FRP) in the facility-month.",
  num_composite_tanks = "integer: Number of active tanks made of composite material in the facility-month.",
  num_concrete_tanks = "integer: Number of active tanks made of concrete in the facility-month.",
  num_jacketed_tanks = "integer: Number of active jacketed tanks in the facility-month.",
  num_coated_tanks = "integer: Number of active coated tanks in the facility-month.",
  num_unknown_material_tanks = "integer: Number of active tanks with unknown material in the facility-month.",
  # Substance counts (active tanks)
  num_gasoline_tanks = "integer: Number of active tanks storing gasoline in the facility-month.",
  num_diesel_tanks = "integer: Number of active tanks storing diesel in the facility-month.",
  num_oil_kerosene_tanks = "integer: Number of active tanks storing oil or kerosene in the facility-month.",
  num_jet_fuel_tanks = "integer: Number of active tanks storing jet fuel in the facility-month.",
  num_other_substance_tanks = "integer: Number of active tanks storing other substances in the facility-month.",
  total_tanks_with_substance_info = "integer: Total number of active tanks in the month for which substance information was available from compartment data.",
  # Detection summary counts (active tanks)
  num_interstitial_det = "integer: Number of active tanks with interstitial or secondary containment detection methods in the facility-month.",
  num_electronic_det = "integer: Number of active tanks with electronic detection methods (ATG, LLD) in the facility-month.",
  num_manual_stat_det = "integer: Number of active tanks with manual or statistical detection methods (manual gauging, SIR) in the facility-month.",
  num_any_det = "integer: Number of active tanks with any release detection method flagged in the facility-month.",
  # Individual detection method counts
  num_DEC_RP_VAPOR = "integer: Number of active tanks using compartment vapor monitoring in the facility-month.",
  num_DEC_RP_GW = "integer: Number of active tanks using compartment groundwater monitoring in the facility-month.",
  num_DEC_RP_SEC_CONT = "integer: Number of active tanks using compartment secondary containment monitoring in the facility-month.",
  num_DEC_RP_ATG = "integer: Number of active tanks using compartment automatic tank gauging in the facility-month.",
  num_DEC_RP_INTERSTITIAL = "integer: Number of active tanks using compartment interstitial monitoring in the facility-month.",
  num_DEC_RP_MANUAL_WEEK = "integer: Number of active tanks using weekly manual tank gauging for compartment in the facility-month.",
  num_DEC_RP_MANUAL_MONTH = "integer: Number of active tanks using monthly manual tank gauging for compartment in the facility-month.",
  num_DEC_RP_SIR = "integer: Number of active tanks using statistical inventory reconciliation for compartment in the facility-month.",
  num_DET_PIPE_VAPOR = "integer: Number of active tanks/piping systems using piping vapor monitoring in the facility-month.",
  num_DET_PIPE_GW = "integer: Number of active tanks/piping systems using piping groundwater monitoring in the facility-month.",
  num_DET_PIPE_SEC_CONT = "integer: Number of active tanks/piping systems using piping secondary containment monitoring in the facility-month.",
  num_DET_PIPE_INTERSTITIAL = "integer: Number of active tanks/piping systems using piping interstitial monitoring in the facility-month.",
  num_DET_PIPE_TT_MONTH = "integer: Number of active tanks/piping systems using monthly tightness testing for piping in the facility-month.",
  num_DET_PIPE_TT_ANNUAL = "integer: Number of active tanks/piping systems using annual tightness testing for piping in the facility-month.",
  num_DET_PIPE_TT_TRIEN = "integer: Number of active tanks/piping systems using triennial tightness testing for piping in the facility-month.",
  num_DET_PIPE_LLD = "integer: Number of active tanks/piping systems using automatic line leak detectors in the facility-month.",
  num_DET_PIPE_SIR = "integer: Number of active tanks/piping systems using statistical inventory reconciliation for piping in the facility-month.",
  num_DET_PIPE_SUCTION_EXEMPT = "integer: Number of active tanks/piping systems exempt due to suction piping in the facility-month.",

  # == Self-Certification (from self_cert_monthly) ==
  FA_FLAG = "logical: TRUE if self-certification (signed in the year of this month) indicated financial assurance requirements were met.",
  TECH_FLAG = "logical: TRUE if self-certification (signed in the year of this month) indicated technical standards were met.",

  # == LUST Information ==
  is_lust_report_month = "integer: 1 if a LUST incident was reported for the facility in this specific month, 0 otherwise.",
  lust_active = "logical: TRUE if a LUST incident was active (reported and not yet NFA) at the facility in the month.",
  lust_pre_2000_active = "logical: TRUE if an active LUST incident (reported before 2000-01-01) was ongoing in the month.",
  lust_post_2000_active = "logical: TRUE if an active LUST incident (reported on or after 2000-01-01) was ongoing in the month.",
  has_subsidized_leak_active = "logical: TRUE if an active LUST incident (reported before 2000-01-01, thus potentially subsidized) was ongoing in the month.",
  lust_open_active = "logical: TRUE if any LUST incident active in this month was still open (NFA date is in the future or NA).",
  lust_closed_active = "logical: TRUE if any LUST incident active in this month had its NFA date occur by this month.",
  lust_ever = "logical: TRUE if the facility ever had any LUST incident reported (cumulative).",
  lust_pre_2000 = "logical: TRUE if the facility ever had a LUST incident reported before 2000-01-01 (cumulative).",
  lust_post_2000 = "logical: TRUE if the facility ever had a LUST incident reported on or after 2000-01-01 (cumulative).",
  has_subsidized_leak = "logical: TRUE if the facility ever had a LUST incident reported before 2000-01-01 (cumulative, proxy for subsidy eligibility).",
  lust_ever_closed = "logical: TRUE if any LUST incident at the facility has ever received an NFA date (cumulative).",
  lust_ever_open = "logical: TRUE if any LUST incident at the facility was ever reported and currently has no NFA date or its NFA date is in the future (cumulative).",
  lust_after_closure_ever = "integer: 1 if the facility ever had a LUST incident reported after a previous NFA date for another LUST at the same facility, 0 otherwise (cumulative).",

  # == Facility Status Variables (derived in panel) ==
  is_tceq_reported_closed = "integer: 1 if FACILITY_STATUS from the facility extract is 'CLOSED', 0 otherwise.",
  is_tceq_reported_inactive = "integer: 1 if FACILITY_STATUS from the facility extract is 'INACTIVE', 0 otherwise.",
  is_tceq_reported_temporarily_out_of_service = "integer: 1 if FACILITY_STATUS from the facility extract is 'TEMPORARILY OUT OF SERVICE', 0 otherwise.",
  any_tank_active = "logical: TRUE if active_tank_count > 0 for the facility-month.",
  Closed_Inventory_YearMonth = "character: YYYY-MM of the last month the facility had active tanks based on panel's active_tank_count. NA if never active.",
  closure_year = "integer: Year extracted from Closed_Inventory_YearMonth. NA if never active or if Closed_Inventory_YearMonth is NA.",
  closure_month = "integer: Month extracted from Closed_Inventory_YearMonth. NA if never active or if Closed_Inventory_YearMonth is NA.",
  is_active_pre_inventory_closure = "integer: 1 if the facility-month is on or before the Closed_Inventory_YearMonth, 0 otherwise. 0 if never active or Closed_Inventory_YearMonth is NA.",
  is_facility_entry_event = "logical: TRUE for the first month a facility transitions from having no active tanks (any_tank_active=FALSE) to having active tanks (any_tank_active=TRUE).",
  is_facility_exit_event = "logical: TRUE for the first month a facility transitions from active to inactive pre-inventory closure.",
  closure_not_final_event = "integer: 1 if `lust_after_closure_ever` is 1, 0 otherwise. Indicates a LUST event occurred after a supposed NFA.",

  # == Zurich-Related Flags ==
  had_zurich_2012 = "integer: 1 if the facility had a Zurich insurance policy active at any point during CY 2012, 0 otherwise.",
  dropped_by_zurich = "integer: 1 if `had_zurich_2012` is 1 and the panel YEAR is >= 2013, 0 otherwise.",

  # == Capacity Change Metrics ==
  capacity_delta_gal = "numeric: Change in total_capacity (gallons) from the previous month. 0 for the first month of a facility.",
  capacity_change_event = "integer: 1 if capacity_delta_gal is not 0, 0 otherwise. 0 for the first month of a facility.",

  # == Device & Material Change Events ==
  num_interstitial_det_delta = "numeric: Change in num_interstitial_det from the previous month. 0 for the first month of a facility.",
  num_interstitial_det_change_event = "integer: 1 if num_interstitial_det changed from the previous month, 0 otherwise. 0 for the first month of a facility.",
  num_electronic_det_delta = "numeric: Change in num_electronic_det from the previous month. 0 for the first month of a facility.",
  num_electronic_det_change_event = "integer: 1 if num_electronic_det changed from the previous month, 0 otherwise. 0 for the first month of a facility.",
  num_manual_stat_det_delta = "numeric: Change in num_manual_stat_det from the previous month. 0 for the first month of a facility.",
  num_manual_stat_det_change_event = "integer: 1 if num_manual_stat_det changed from the previous month, 0 otherwise. 0 for the first month of a facility.",
  num_steel_tanks_delta = "numeric: Change in num_steel_tanks from the previous month. 0 for the first month of a facility.",
  num_steel_tanks_change_event = "integer: 1 if num_steel_tanks changed from the previous month, 0 otherwise. 0 for the first month of a facility.",
  num_fiberglass_tanks_delta = "numeric: Change in num_fiberglass_tanks from the previous month. 0 for the first month of a facility.",
  num_fiberglass_tanks_change_event = "integer: 1 if num_fiberglass_tanks changed from the previous month, 0 otherwise. 0 for the first month of a facility.",
  # Add active_tank_count definition
  active_tank_count = "integer: Number of unique USTs active at the facility in the month, calculated during UST monthly aggregation."
)

# Create data.table from the list for easy lookup or writing to CSV
column_dictionary_dt <- data.table(
  column_name = names(column_definitions_list),
  description = unlist(column_definitions_list)
)

# Optional: Add a generic definition for dummy columns if they are too numerous to list individually
# Note: The script dynamically creates these. You would need to identify them in the panel to add them here.
# For example, if all dummy columns start with "ISSUER_NAME__", "CATEGORY__", or "DETAIL_TYPE__":
# dummy_cols_in_panel <- names(panel)[grepl("^(ISSUER_NAME__|CATEGORY__|DETAIL_TYPE__)", names(panel))]
# for (d_col in dummy_cols_in_panel) {
#   if (!d_col %in% column_dictionary_dt$column_name) {
#     var_type <- sub("__.*", "", d_col)
#     val_slug <- sub(".*?__", "", d_col)
#     desc <- paste0("integer: 1 if an active financial mechanism in the month had ", var_type, " '", gsub("_", " ", val_slug), "', 0 otherwise.")
#     column_dictionary_dt <- rbind(column_dictionary_dt, data.table(column_name = d_col, description = desc))
#   }
# }

# Save the dictionary
fwrite(column_dictionary_dt, here("Data", "Processed", "texas_fr_panel_column_dictionary.csv"))
message("Column dictionary saved to ", here("Data", "Processed", "texas_fr_panel_column_dictionary.csv"))

# Final check: ensure all columns in the panel are in the dictionary (optional, for development)
# missing_from_dict <- setdiff(names(panel), column_dictionary_dt$column_name)
# if (length(missing_from_dict) > 0) {
#   warning(paste("Columns in panel missing from dictionary:", paste(missing_from_dict, collapse=", ")))
# }
# not_in_panel <- setdiff(column_dictionary_dt$column_name, names(panel))
# if (length(not_in_panel) > 0) {
#   warning(paste("Columns in dictionary not found in panel:", paste(not_in_panel, collapse=", ")))
#   # This might indicate definitions for columns that were dropped or renamed.
# }

message("\n=== SAMPLE PANEL OUTPUT FOR ONE FACILITY ===")
# ...existing code...

# ###############################################################################
# ## A. RAW UST COUNTS – tanks active ≥ 1990 at FR-required facilities
# ###############################################################################
# library(data.table)

# # 1.  keep only tanks whose life-span stretches past 1 Jan 1990
# ust_post90 <- ust[end_date >= as.Date("1990-01-01")]

# # 2.  keep only facilities that must carry UST financial-responsibility
# fac_fr     <- fac[UST_FR_REQUIRED == TRUE, .(FACILITY_ID)]

# # 3.  per-facility number of unique TANK_IDs
# raw_ust_cnt <- ust_post90[fac_fr,           # semi-join on FACILITY_ID
#    on = "FACILITY_ID"
#  ][, .(raw_usts_post90 = uniqueN(UST_ID)), # uniqueN() is fastest in data.table :contentReference[oaicite:0]{index=0}
#    by = FACILITY_ID]

# ###############################################################################
# ## B. PANEL COUNTS – same idea but from the finished monthly panel
# ###############################################################################
# # 1.  restrict to 1990+
# panel_1990 <- panel[YEAR >= 1990]

# # 2.  for each facility take the **maximum** reported active-tank count
# #     (that’s the size of the UST inventory that survives into the panel era)
# panel_ust_cnt <- panel_1990[,
#    .(panel_usts_post90 = max(active_tank_count, na.rm = TRUE)), 
#    by = FACILITY_ID]

# ###############################################################################
# ## C. COMPARE THE TWO ESTIMATES
# ###############################################################################
# compare_tbl <- merge(raw_ust_cnt, panel_ust_cnt, by = "FACILITY_ID", all = TRUE)
# compare_tbl[, diff := raw_usts_post90 - panel_usts_post90]

# ## quick summary
# summary_tbl <- compare_tbl[, .(
#   facilities           = .N,
#   matching             = sum(diff == 0, na.rm = TRUE),
#   mismatching          = sum(diff != 0, na.rm = TRUE),
#   mean_abs_difference  = mean(abs(diff), na.rm = TRUE)
# )]

# print(summary_tbl)
# print(head(compare_tbl[diff != 0], 20))   # peek at first mismatches

# ###############################################################################
# ## D. DID WE PUT FACILITY START DATE IN THE PANEL ?
# ###############################################################################

# rm(ust_post90, fac_fr, raw_ust_cnt, panel_1990, panel_ust_cnt)  # tidy up


# ## --- randomly draw 10 distinct facilities for spot-checking ---------------
# set.seed(42)                                # ⇐ makes the draw reproducible
# sample_fac_ids <- sample(unique(fac$FACILITY_ID), 10)

# cat("Random sample of 10 FACILITY_IDs:\n")
# print(sample_fac_ids)
# panel_RETAIL = panel[FACILITY_TYPE == "RETAIL", .(FACILITY_ID, YEAR, MONTH, FACILITY_NAME,ISSUER_NAME,CATEGORY,avg_tank_age_months ,SITE_COUNTY, fips, total_capacity, tank_count, active_tank_count, closed_tank_count,lust_active,lust_open_active,lust_closed_active)]

# ## optional: grab *all* rows for those facilities from any table you like
# #   (here I pull from the finished `panel` object)
# inspect_sample <- panel[FACILITY_ID %in% sample_fac_ids ]

# setkey(inspect_sample, FACILITY_ID, YEAR, MONTH)  # sort by facility and time
# view(inspect_sample)  # view in RStudio viewer
# # randomly pull 10 retail facilities for inspection
# sample_retail_fac_ids <- sample(unique(panel_RETAIL$FACILITY_ID), 10)

# inspect_retail_sample <- panel_RETAIL[FACILITY_ID %in% sample_retail_fac_ids ]

# setkey(inspect_retail_sample, FACILITY_ID, YEAR, MONTH)  # sort by facility and time
# view(inspect_retail_sample)  # view in RStudio viewer
# # count unique number  obs per  fips
# fips_obs = panel[, .(unique_obs = uniqueN(FACILITY_ID)), by = .(fips,SITE_COUNTY)]
# view(fips_obs)
# message("Validation checks passed (or warnings issued).")

# kims_b = ust[FACILITY_ID =='95972']
# view(kims_b)



# ###############################################################################
# # collapse_logic() — collapse contract-level rows to one facility-month row
# # --------------------------------------------------------------------------
# # Expects: `dt` = all contracts for ONE facility in ONE year–month
# # Returns: single-row data.table with aggregates + flags that downstream code
# #          (change-indicators, dummy expansion, etc.) relies on.
# #
# #  ➤ NEW (June 2024):
# #      • carries forward `coverage_gap_month` so later blocks can use it
# ###############################################################################
# collapse_logic <- function(dt) {

#   dt <- data.table::copy(dt)        # unlock .SD
#   n  <- nrow(dt)                    # how many contracts this month?

#   ## ---- group-level aggregates used in every branch -------------------------
#   agg <- list(
#     DETAIL_TYPE         = paste(sort(unique(dt$DETAIL_TYPE)), collapse = "; "),
#     CATEGORY            = paste(sort(unique(dt$CATEGORY)),    collapse = "; "),
#     ISSUER_NAME         = paste(sort(unique(dt$ISSUER_NAME)), collapse = " | "),
#     max_COVER_OCC       = if (all(is.na(dt$COVER_OCC))) NA_real_
#                           else max(dt$COVER_OCC, na.rm = TRUE),
#     max_COVER_AGG       = if (all(is.na(dt$COVER_AGG))) NA_real_
#                           else max(dt$COVER_AGG, na.rm = TRUE),
#     total_COVER_OCC     = sum(dt$COVER_OCC, na.rm = TRUE),
#     total_COVER_AGG     = sum(dt$COVER_AGG, na.rm = TRUE),
#     premium_prepaid     = any(dt$premium_prepaid, na.rm = TRUE),
#     proof_on_file       = any(dt$proof_on_file,   na.rm = TRUE),
#     fp_corr_met         = any(dt$fp_corr_met,     na.rm = TRUE),
#     tp_fa_met           = any(dt$tp_fa_met,       na.rm = TRUE),
#     uses_private        = any(dt$uses_private,    na.rm = TRUE),
#     uses_self           = any(dt$uses_self,       na.rm = TRUE),
#     coverage_gap_month  = any(dt$coverage_gap_month)   # ← NEW
#   )

#   ###########################################################################
#   # Branch 1 : single contract ----------------------------------------------
#   ###########################################################################
#   if (n == 1L) {
#     row <- dt[1L]
#     row[, `:=`(
#       transition_month    = FALSE,
#       multiple_contracts  = FALSE,
#       active_FIN_ASSUR_ID = FIN_ASSUR_ID,
#       fr_covered          = TRUE,            # by construction
#       DETAIL_TYPE         = agg$DETAIL_TYPE,
#       CATEGORY            = agg$CATEGORY,
#       ISSUER_NAME         = agg$ISSUER_NAME,
#       max_COVER_OCC       = agg$max_COVER_OCC,
#       max_COVER_AGG       = agg$max_COVER_AGG,
#       total_COVER_OCC     = agg$total_COVER_OCC,
#       total_COVER_AGG     = agg$total_COVER_AGG,
#       premium_prepaid     = agg$premium_prepaid,
#       proof_on_file       = agg$proof_on_file,
#       fp_corr_met         = agg$fp_corr_met,
#       tp_fa_met           = agg$tp_fa_met,
#       uses_private        = agg$uses_private,
#       uses_self           = agg$uses_self,
#       coverage_gap_month  = agg$coverage_gap_month     # ← NEW
#     )]
#     return(row[])
#   }

#   ###########################################################################
#   # Branch 2 : exactly two contracts, NO overlap  (clean hand-off) ----------
#   ###########################################################################
#   ord  <- order(dt$start_day_in_month)
#   sNum <- as.numeric(dt$start_day_in_month[ord])
#   eNum <- as.numeric(dt$end_day_in_month[ord])
#   overlaps <- any(na.omit(sNum[-1] <= cummax(eNum)[-n]))

#   if (n == 2L && !overlaps) {
#     idx <- which.max(dt$start_day_in_month)         # the one that begins
#     row <- dt[idx]
#     row[, `:=`(
#       transition_month    = TRUE,
#       multiple_contracts  = FALSE,
#       active_FIN_ASSUR_ID = FIN_ASSUR_ID,
#       fr_covered          = TRUE,
#       DETAIL_TYPE         = agg$DETAIL_TYPE,
#       CATEGORY            = agg$CATEGORY,
#       ISSUER_NAME         = agg$ISSUER_NAME,
#       max_COVER_OCC       = agg$max_COVER_OCC,
#       max_COVER_AGG       = agg$max_COVER_AGG,
#       total_COVER_OCC     = agg$total_COVER_OCC,
#       total_COVER_AGG     = agg$total_COVER_AGG,
#       premium_prepaid     = agg$premium_prepaid,
#       proof_on_file       = agg$proof_on_file,
#       fp_corr_met         = agg$fp_corr_met,
#       tp_fa_met           = agg$tp_fa_met,
#       uses_private        = agg$uses_private,
#       uses_self           = agg$uses_self,
#       coverage_gap_month  = agg$coverage_gap_month     # ← NEW
#     )]
#     return(row[])
#   }

#   ###########################################################################
#   # Branch 3 : ≥2 overlapping contracts -------------------------------------
#   ###########################################################################
#   row <- dt[1L]
#   row[, `:=`(
#     transition_month    = FALSE,
#     multiple_contracts  = TRUE,
#     active_FIN_ASSUR_ID = paste(sort(unique(dt$FIN_ASSUR_ID)), collapse = " | "),
#     fr_covered          = TRUE,
#     DETAIL_TYPE         = agg$DETAIL_TYPE,
#     CATEGORY            = agg$CATEGORY,
#     ISSUER_NAME         = agg$ISSUER_NAME,
#     max_COVER_OCC       = agg$max_COVER_OCC,
#     max_COVER_AGG       = agg$max_COVER_AGG,
#     total_COVER_OCC     = agg$total_COVER_OCC,
#     total_COVER_AGG     = agg$total_COVER_AGG,
#     premium_prepaid     = agg$premium_prepaid,
#     proof_on_file       = agg$proof_on_file,
#     fp_corr_met         = agg$fp_corr_met,
#     tp_fa_met           = agg$tp_fa_met,
#     uses_private        = agg$uses_private,
#     uses_self           = agg$uses_self,
#     coverage_gap_month  = agg$coverage_gap_month     # ← NEW
#   )]
#   return(row[])
# }


# ###############################################################################
# # 2.  Contract-monthly expansion WITH day-level edges (now keeps ISSUER_NAME) -
# ###############################################################################
# fa_monthly_contract <- fa[
#   !is.na(EFF_DATE) & !is.na(EXP_DATE) & EFF_DATE <= EXP_DATE,
#   {
#     mo_seq <- seq(
#       floor_date(EFF_DATE, "month"),
#       floor_date(pmin(EXP_DATE, as.Date("2026-12-01")), "month"),
#       by = "month"
#     )

#     first_in_mo <- pmax(EFF_DATE, floor_date(mo_seq, "month"))
#     last_in_mo  <- pmin(EXP_DATE, ceiling_date(mo_seq, "month") - 1)

#     .(
#       YEAR  = year(mo_seq),
#       MONTH = month(mo_seq),

#       ## <-- carry these through:
#       EFF_DATE = EFF_DATE,
#       EXP_DATE = EXP_DATE,

#       start_day_in_month = first_in_mo,
#       end_day_in_month   = last_in_mo,

#       DETAIL_TYPE, CATEGORY,
#       COVER_OCC,  COVER_AGG,
#       ISSUER_NAME,
#       premium_prepaid = PREMIUM_PREPAID,
#       proof_on_file   = PROOF_OF_FA,
#       fp_corr_met     = FP_CORR_MET,
#       tp_fa_met       = TP_FA_MET,
#       uses_private    = USE_PRIVATE_mapped,
#       uses_self       = USE_SELF_mapped,
#       fr_covered      = TRUE
#     )
#   },
#   by = .(FACILITY_ID, FIN_ASSUR_ID)
# ]

## EPA Region UST Dataset Creation
# Script: 03_Clean_EPA_Regions.R
# Author: Validated Classification Logic (Updated Jan 2026)
# Purpose: 
#   1. Process ~42 "Standard States" from EPA national dataset (Geocoding + Harmonization).
#   2. Extract Lat/Long tables for the 8 "Custom States" (AR, LA, ME, MI, NJ, NM, OK, TX).

library(here)
library(data.table)
library(tidyverse)
library(lubridate)
library(janitor)
library(stringr)
library(tigris) 

# Enable caching for Tigris
options(tigris_use_cache = TRUE)

# 0. Setup & Helper Functions --------------------------------------------------

# Ensure output root exists
root_output_dir <- here("Data", "Raw", "state_databases")
if (!dir.exists(root_output_dir)) dir.create(root_output_dir, recursive = TRUE)

# --- Classifiers ---

classify_substances <- function(data, substance_col_name = "substances") {
  result <- copy(data)
  result[, `:=`(
    is_gasoline = 0, is_diesel = 0, is_oil_kerosene = 0, is_jet_fuel = 0, is_other = 1
  )]
  
  # Patterns
  gasoline_patterns <- paste(c(
    "\\bgasoline\\b", "\\bgas(?!\\s*(de-hy|condensate))\\b", "\\bmogas\\b", "\\bpetrol\\b", 
    "\\b(?:un)?leaded\\b", "\\bpremium\\b", "\\bregular\\b", "\\bmidgrade\\b", "\\bmid-grade\\b", 
    "\\bsuper\\b", "\\bsupreme\\b", "\\bplus\\b", "\\bconv(?:[[:alpha:]]+)?\\b",
    "\\be(?:-|\\s*)?(10|15|85|[0-9]+)\\b", "\\bethanol(?!.*fuel)\\b", "\\bgasohol\\b", 
    "\\brul\\b", "\\bpul\\b", "\\bpunl\\b", "\\bunl(?:ead)?\\b", "\\blead[- ]?free\\b",
    "\\boctane\\b", "\\b87\\b", "\\b89\\b", "\\b91\\b", "\\b93\\b",
    "\\bmtbe\\b", "\\bno[- ]?lead\\b", "\\bno[- ]?ethanol\\b", "\\bretasc\\b",
    "\\bracing(?:[- ]?(?:gas|fuel))?\\b", "\\brec[- ]?fuel\\b", "\\bstandard\\b",
    "\\b100[%]?\\s*(?:unleaded|gas)\\b", "\\bautomotive\\sfuel\\b",
    "(?<!av[\\s-]|aviation[\\s-]|jet[\\s-])\\bgas\\b", "(?<!jet|aviation)\\s+fuel"
  ), collapse = "|")
  
  diesel_patterns <- paste(c(
    "\\bdiesel\\b", "\\bdsl\\b", "\\bd$\\b", "\\bord\\b", "\\bdies\\b", "\\bdied fuel\\b",
    "\\b(?:auto|vehicular)[- ]?diesel\\b", "\\bulsd\\b", "\\bdistill\\b",
    "\\bbio[- ]?diesel\\b", "\\bb[- ]?([0-9]+(?:\\.[0-9]+)?)\\b", "\\bb100\\b", "\\bb99\\b",
    "\\bd[- ]?[0-9]\\b", "\\b#[1-6][- ]?(?:diesel)\\b", "\\bon[- ]?road\\b", 
    "\\bhigh sulp(?:h)?ur diesel\\b", "\\bheavy diesel\\b",
    "\\b(?:off|off[- ]?road)[- ]?d(?:iesel)?\\b", "\\boff[- ]?rd(?:[- ]?d(?:sl|iesel)?)?\\b", 
    "\\bdyed(?:[- ]?d(?:sl|iesel)?)?\\b", "\\bred[- ]?diesel\\b", "\\bfarm[- ]?diesel\\b",
    "(?<!def|fluid)\\s*\\bdiesel\\b"
  ), collapse = "|")
  
  oil_kerosene_patterns <- paste(c(
    "\\boil\\b", "\\blubrican(?:t|ting)\\b", "\\blube(?:\\s+oil)?\\b", 
    "\\b(?:motor|engine|used|waste|virgin|new|bulk|hydraulic|quench|cutting|hoist|gear|process|transmission|mineral)(?:[- ]?oil)?\\b",
    "\\bhyd(?:raul(?:ic)?)?(?:\\.|[- ]?fluid|\\.fluid|\\s)\\b", "\\btrans(?:mission)?(?:[- ]?fluid)?\\b",
    "\\banti[- ]?freeze\\b", "\\bcool(?:ant)?\\b", "\\bethylene[- ]?glycol\\b",
    "\\bkerosene\\b", "\\bkerosine\\b", "\\bk-1\\b",
    "\\bfuel[- ]?oil\\b", "\\b(?:heating|heat)[- ]?oil\\b", 
    "\\b#[1-6](\\s|\\/)?(fuel|heating)?[- ]?oil\\b",
    "\\bway[- ]?(?:oil|lube)\\b", "\\bdrain[- ]?oil\\b", "\\batf\\b", 
    "\\bcrude\\b", "\\bheavy\\b", "\\bsolutio?n\\b", "\\bslurry\\b", "\\bemulsion\\b",
    "\\bpower[- ]?steer(?:ing)?(?:[- ]?(?:fluid|oil))?\\b", "\\b10w(?:30|40|50)?\\b",
    "\\b30[- ]?(?:w|wt)\\b", "\\b40[- ]?(?:w|wt)\\b", "\\b5w(?:30|40)\\b"
  ), collapse = "|")
  
  jet_fuel_patterns <- paste(c(
    "\\bjet(?:[- ]?(?:a|fuel))?\\b", "\\bjp[- ]?[45678]\\b", "\\bturbo[- ]?fuel\\b",
    "\\baviation(?:[- ]?(?:fuel|gas))?\\b", "\\bav(?:[- ]?(?:gas|fuel))?\\b", 
    "\\bavgas\\b", "\\bav[- ]?gas\\b", "\\b100ll\\b", "\\b100\\s+oct\\b", "\\b80\\s+oct\\b",
    "\\baircraft\\b", "\\bairplane\\b", "\\bskychief\\b"
  ), collapse = "|")
  
  result[, substances_lower := tolower(get(substance_col_name))]
  result[!is.na(substances_lower) & substances_lower != "", 
         is_gasoline := as.integer(grepl(gasoline_patterns, substances_lower, perl = TRUE))]
  result[!is.na(substances_lower) & substances_lower != "" & 
           !grepl("def|diesel exhaust fluid", substances_lower, perl = TRUE), 
         is_diesel := as.integer(grepl(diesel_patterns, substances_lower, perl = TRUE))]
  result[!is.na(substances_lower) & substances_lower != "", 
         is_oil_kerosene := as.integer(grepl(oil_kerosene_patterns, substances_lower, perl = TRUE))]
  result[!is.na(substances_lower) & substances_lower != "", 
         is_jet_fuel := as.integer(grepl(jet_fuel_patterns, substances_lower, perl = TRUE))]
  result[, is_other := as.integer(is_gasoline + is_diesel + is_oil_kerosene + is_jet_fuel == 0)]
  result[, substances_lower := NULL]
  return(result)
}

classify_tank_walls <- function(data, tank_wall_col = "tank_wall_type") {
  result <- copy(data)
  result[, `:=`(
    double_walled = as.integer(grepl("DOUBLE|TRIPLE|DBL|DUAL|SECONDAR|JACKET", toupper(get(tank_wall_col)), ignore.case = TRUE)),
    single_walled = as.integer(grepl("SINGLE|SGL", toupper(get(tank_wall_col)), ignore.case = TRUE) & 
                                 !grepl("UNKNOWN", toupper(get(tank_wall_col)), ignore.case = TRUE)),
    unknown_walled = as.integer(grepl("UNKNOWN", toupper(get(tank_wall_col)), ignore.case = TRUE)),
    missing_walled = as.integer(is.na(get(tank_wall_col)) | get(tank_wall_col) == "")
  )]
  # Consolidate missing into unknown
  result[, unknown_walled := pmax(unknown_walled, missing_walled, na.rm = TRUE)]
  return(result)
}

standardize_capacity <- function(data, capacity_col = "capacity") {
  result <- copy(data)
  result[, `:=`(capacity = as.numeric(gsub("[^0-9.]", "", get(capacity_col))))]
  return(result)
}

# --- FIX: Vectorized County Standardizer ---
standardize_county_name <- function(county_name_vec) {
  # Ensure input is character
  name <- as.character(county_name_vec)
  name <- tolower(name)
  name <- gsub(" county$| counties$| parish$| parishes$", "", name)
  name <- gsub("[^a-z ]", "", name)
  name <- trimws(gsub("\\s+", " ", name))
  
  # Handle empties and NAs
  name[name == "" | name == "na"] <- NA_character_
  return(name)
}

# 1. Load and Prep Raw Data ----------------------------------------------------

cat("\nLoading raw EPA data (Permissive Mode)...\n")

epa_usts <- fread(here("Data", "Raw", "USTs.csv"), colClasses = "character") %>% 
  clean_names() %>% as.data.table()

epa_facilities <- fread(here("Data", "Raw", "Facilities.csv"), colClasses = "character") %>% 
  clean_names() %>% as.data.table()

epa_releases <- fread(here("Data", "Raw", "Releases.csv"), colClasses = "character") %>% 
  clean_names() %>% as.data.table()

# Join Facility info (Name, County, LAT, LONG) to USTs
cat("\nJoining Facility info to USTs...\n")
fac_geo <- epa_facilities[, .(facility_id, facility_name = name, county, city, zip_code, latitude, longitude)]

state_UST_full <- merge(
  epa_usts, 
  fac_geo, 
  by = "facility_id", 
  all.x = TRUE
)

# 2. State Partitioning --------------------------------------------------------

custom_script_states <- c("Arkansas", "Louisiana", "Maine", "Michigan", "New Jersey", "New Mexico", "Oklahoma", "Texas")

all_states <- unique(state_UST_full$state)
states_to_process_full <- setdiff(all_states, custom_script_states)
# Ensure Kansas is processed here even if it was custom before
if(!"Kansas" %in% states_to_process_full) states_to_process_full <- c(states_to_process_full, "Kansas")

# 3. Main Processing Function (Full Harmonization) -----------------------------

process_state_data <- function(state_name) {
  
  state_abbr <- state.abb[match(state_name, state.name)]
  if(is.na(state_abbr)) state_abbr <- toupper(substring(state_name, 1, 2))
  
  state_out_dir <- here("Data", "Raw", "state_databases", state_name)
  if (!dir.exists(state_out_dir)) dir.create(state_out_dir, recursive = TRUE)
  
  cat(paste0("\n--- Processing ", state_name, " (", state_abbr, ") ---\n"))
  
  # --- Step A: UST Data Preparation ---
  state_ust <- state_UST_full[state == state_name]
  
  state_ust[, `:=`(
    facility_id = as.character(facility_id),
    tank_id = as.character(tank_id),
    facility_name = stringr::str_to_title(trimws(gsub("[^[:alnum:] ]", "", facility_name))),
    tank_installed_date = as.Date(lubridate::parse_date_time(installation_date, c("mdy", "ymd", "adb"), quiet=TRUE)),
    tank_closed_date = as.Date(lubridate::parse_date_time(removal_date, c("mdy", "ymd", "adb"), quiet=TRUE)),
    tank_status_clean = ifelse(grepl("Perm|Closed|Remov|Aband", tank_status, ignore.case = TRUE), "Closed", "Open")
  )]
  
  state_ust[tank_status_clean == "Open", tank_closed_date := NA]
  
  # Classifiers
  state_ust <- classify_substances(state_ust, "substances")
  state_ust <- classify_tank_walls(state_ust, "tank_wall_type")
  state_ust <- standardize_capacity(state_ust, "capacity")
  
  # --- Step B: LUST Data Preparation ---
  state_lust <- epa_releases[state == state_name]
  
  if(nrow(state_lust) > 0) {
    state_lust[, `:=`(
      facility_id = as.character(facility_id),
      LUST_id = as.character(lust_id),
      report_date = as.Date(lubridate::parse_date_time(reported_date, c("mdy", "ymd", "adb"), quiet=TRUE)),
      nfa_date = as.Date(NA),
      state = state_abbr
    )]
    
    LUST_Harmonized <- state_lust[, .(facility_id, LUST_id, report_date, nfa_date, state)]
    fwrite(LUST_Harmonized, file.path(state_out_dir, paste0(state_abbr, "_Harmonized_LUST.csv")))
    
    leak_counts <- state_lust[!is.na(report_date), .(LUST_count = .N), by = .(facility_id, report_date)]
  } else {
    leak_counts <- data.table(facility_id=character(), report_date=as.Date(character()), LUST_count=integer())
  }
  
  # --- Step C: Merge & Indicators ---
  ust_with_leaks <- merge(state_ust, leak_counts, by = "facility_id", all.x = TRUE, allow.cartesian = TRUE)
  
  ust_with_leaks[, `:=`(
    leak_after_closure = as.integer(!is.na(tank_closed_date) & !is.na(report_date) & report_date > tank_closed_date),
    leak_before_NFA_before_closure = 0,
    leak_before_NFA_after_closure = 0,
    no_leak = as.integer(is.na(report_date) & !is.na(tank_closed_date))
  )]
  
  # --- Step D: Aggregation ---
  UST_Harmonized <- ust_with_leaks[, .(
    facility_name = first(facility_name),
    tank_status = first(tank_status_clean),
    capacity = mean(capacity, na.rm = TRUE),
    
    latitude = first(as.numeric(latitude)),
    longitude = first(as.numeric(longitude)),
    
    leak_after_closure = max(leak_after_closure, na.rm=T),
    leak_before_NFA_before_closure = 0,
    leak_before_NFA_after_closure = 0,
    no_leak = max(no_leak, na.rm=T),
    
    single_walled = max(single_walled, na.rm=TRUE),
    double_walled = max(double_walled, na.rm=TRUE),
    unknown_walled = max(unknown_walled, na.rm=TRUE),
    
    is_gasoline = max(is_gasoline, na.rm=TRUE),
    is_diesel = max(is_diesel, na.rm=TRUE),
    is_oil_kerosene = max(is_oil_kerosene, na.rm=TRUE),
    is_jet_fuel = max(is_jet_fuel, na.rm=TRUE),
    is_other = max(is_other, na.rm=TRUE),
    
    county_name = first(county)
  ), by = .(facility_id, tank_id, tank_installed_date, tank_closed_date)]
  
  UST_Harmonized[, state := state_abbr]
  
  # --- Step E: Add FIPS ---
  # FIX: Vectorized call
  UST_Harmonized[, standardized_county := standardize_county_name(county_name)]
  
  tryCatch({
    fips_lookup <- counties(state = state_name, cb = TRUE, progress_bar = FALSE)
    setDT(fips_lookup)
    fips_lookup[, standardized_county := standardize_county_name(NAME)]
    UST_Harmonized <- merge(UST_Harmonized, fips_lookup[, .(standardized_county, GEOID)], by = "standardized_county", all.x = TRUE)
    setnames(UST_Harmonized, "GEOID", "county_fips")
  }, error = function(e) {
    message("  Warning: FIPS fetch failed for ", state_name)
    if(!"county_fips" %in% names(UST_Harmonized)) UST_Harmonized[, county_fips := NA_character_]
  })
  
  if("standardized_county" %in% names(UST_Harmonized)) UST_Harmonized[, standardized_county := NULL]
  
  # --- Step F: Save ---
  required_columns <- c(
    "facility_id", "facility_name", "tank_id", "state", "tank_installed_date", "tank_closed_date", "tank_status",
    "leak_after_closure", "leak_before_NFA_before_closure", "leak_before_NFA_after_closure", "no_leak",
    "capacity", "single_walled", "double_walled", "unknown_walled",
    "is_gasoline", "is_diesel", "is_oil_kerosene", "is_jet_fuel", "is_other", 
    "county_name", "county_fips", "latitude", "longitude"
  )
  
  for(col in required_columns) if(!col %in% names(UST_Harmonized)) UST_Harmonized[, (col) := NA]
  UST_Harmonized <- UST_Harmonized[, ..required_columns]
  
  fwrite(UST_Harmonized, file.path(state_out_dir, paste0(state_abbr, "_Harmonized_UST_tanks.csv")))
  cat(paste0("  Saved ", state_abbr, "_Harmonized_UST_tanks.csv\n"))
}


# 4. Excluded States GIS Extraction --------------------------------------------

process_excluded_gis <- function(state_name) {
  
  state_abbr <- state.abb[match(state_name, state.name)]
  if(is.na(state_abbr)) state_abbr <- toupper(substring(state_name, 1, 2))
  
  cat(paste0("\n--- Extracting GIS for Excluded State: ", state_name, " (", state_abbr, ") ---\n"))
  
  state_out_dir <- here("Data", "Raw", "state_databases", state_name)
  if (!dir.exists(state_out_dir)) dir.create(state_out_dir, recursive = TRUE)
  
  # Filter Data
  state_data <- state_UST_full[state == state_name]
  
  # Clean IDs to match local scripts
  if (state_abbr == "MI") {
    # Remove 'MI' prefix and pad to 8 digits with leading zeros
    state_data[, clean_id := stringr::str_pad(gsub("^MI", "", facility_id), width = 8, side = "left", pad = "0")]
    
  } else if (state_abbr == "NJ") {
    state_data[, clean_id := gsub("^NJ", "", facility_id)]
    
  } else if (state_abbr == "NM") {
    state_data[, clean_id := gsub("NM", "", facility_id)] 
    
  } else if (state_abbr == "OK") {
    state_data[, clean_id := gsub("^OK", "", gsub("\\[|\\]", "", facility_id))]
    
  } else if (state_abbr %in% c("AR", "LA", "ME", "TX")) {
    # NEW FIX: Remove state prefix for these states
    # e.g. "AR01000008" -> "01000008"
    # We use paste0("^", state_abbr) to dynamically match the prefix
    pattern <- paste0("^", state_abbr)
    state_data[, clean_id := gsub(pattern, "", facility_id)]
    
  } else {
    # Default trim
    state_data[, clean_id := trimws(facility_id)]
  }
  
  # Select and Aggregate
  gis_extract <- state_data[, .(
    facility_id = as.character(clean_id),
    tank_id = as.character(tank_id),
    state = state_abbr,
    latitude = latitude,
    longitude = longitude
  )]
  
  # Deduplicate
  gis_extract <- unique(gis_extract)
  
  fwrite(gis_extract, file.path(state_out_dir, paste0(state_abbr, "_Harmonized_latlong.csv")))
  cat(paste0("  Saved ", state_abbr, "_Harmonized_latlong.csv\n"))
}


# 5. Execution -----------------------------------------------------------------

cat("\n=== Processing Standard EPA States ===\n")
states_to_process_full <- states_to_process_full[!is.na(states_to_process_full) & states_to_process_full != ""]

for (st in states_to_process_full) {
  tryCatch({
    process_state_data(st)
  }, error = function(e) {
    cat(paste0("Error processing ", st, ": ", e$message, "\n"))
  })
}

cat("\n=== Extracting GIS for Custom States ===\n")
for (st in custom_script_states) {
  tryCatch({
    process_excluded_gis(st)
  }, error = function(e) {
    cat(paste0("Error extracting GIS for ", st, ": ", e$message, "\n"))
  })
}

cat("\n\nScript Complete.\n")


## Alabama UST Dataset Creation
# Script: 10_Clean_AL.R
# Author: Validated Classification Logic (Updated Jan 2026)
# Description: Harmonizes AL UST data for econometrics.
#
# Key Features:
#   1. ID Logic: Parses Permit Number "A-B-C" to link with Site ID "S[C]-[B]".
#   2. Regulatory Mandate: Implements Fed Secondary Containment (April 11, 2016).
#   3. Material Logic: Implements specific "Fiberglass Coated Steel" date cutoffs (1990).
#   4. Date Parsing: Handles Excel Serial dates robustly.

library(data.table)
library(tidyverse)
library(here)
library(janitor)
library(readxl)
library(stringr)
library(lubridate)

# 0. Setup ---------------------------------------------------------------------
state_abbr <- "AL"
al_path <- here("Data", "Raw", "state_databases", "Alabama")
out_dir <- here("Data", "Raw", "state_databases", "Alabama") # Keep in state folder for Master script

if (!dir.exists(al_path)) stop("Alabama data directory not found!")

# 1. Helper Functions ----------------------------------------------------------

# Vectorized Excel Date Parser
parse_excel_date <- function(x) {
  # Suppress warnings for non-numeric garbage
  num_val <- suppressWarnings(as.numeric(x))
  # Excel Origin: Dec 30, 1899
  as.Date(num_val, origin = "1899-12-30")
}

# 2. Load Data (Force Text) ----------------------------------------------------
message("Loading Alabama Data...")

# Tanks
dt_tanks <- read_excel(file.path(al_path, "UST_UTanks (2).xlsx"), col_types = "text") %>% 
  clean_names() %>% as.data.table()

# Sites (Geography)
dt_sites <- read_excel(file.path(al_path, "UST_Sites (2).xlsx"), col_types = "text") %>% 
  clean_names() %>% as.data.table()

# 3. Process Tank Attributes ---------------------------------------------------
message("Processing Tank Attributes...")

# --- A. Generate Link Keys ---
# Logic: Permit "16808-133-014739" -> Seq "133", Site "014739" -> Link "S14739-133"
# Use as.integer to strip leading zeros from the Site component
dt_tanks[, `:=`(
  part_seq  = as.integer(str_extract(permit_number, "(?<=-)[0-9]+(?=-)")),
  part_site = as.integer(str_extract(permit_number, "[0-9]+$"))
)]
dt_tanks[, site_link_id := paste0("S", part_site, "-", part_seq)]

# --- B. Parse Dates ---
dt_tanks[, `:=`(
  tank_installed_date = parse_excel_date(install_date),
  tank_closed_date    = parse_excel_date(removed_date)
)]

# --- C. Harmonize Status ---
dt_tanks[, tank_status := fcase(
  grepl("Current|Temp", tank_status, ignore.case=TRUE), "Open",
  grepl("Perm|Retire|Aband|Inert", tank_status, ignore.case=TRUE), "Closed",
  default = "Closed"
)]

# --- D. Harmonize Substance ---
# Combine raw and select for coverage
dt_tanks[, substance_lower := tolower(paste(petroleum_product_select, substance_stored, petroleum_product))]

dt_tanks[, `:=`(
  is_gasoline = as.integer(grepl("gasoline|gas|ethan|e85|e10|e-15|racing|avgas", substance_lower)),
  is_diesel   = as.integer(grepl("diesel|dsl|bio|b-99|b100|b-10|def", substance_lower)),
  is_oil_kerosene = as.integer(grepl("oil|kerosene|k1|lube|hydr|fluid", substance_lower)),
  is_jet_fuel = as.integer(grepl("jet|aviation|jp-4", substance_lower)),
  is_other    = 0
)]
dt_tanks[is_gasoline==0 & is_diesel==0 & is_oil_kerosene==0 & is_jet_fuel==0, is_other := 1]

# --- E. ALABAMA CLASSIFICATION ALGORITHM (Wall Type) ---
# Preparing fields
dt_tanks[, mat_upper := toupper(paste(tank_construction_material, tank_construction_material_select))]

# Regulatory Dates
fed_sec_date <- as.Date("2016-04-11") # Federal Requirement
frp_cutoff   <- as.Date("1990-01-01") # Industry standard for Double FRP
steel_cutoff <- as.Date("2000-01-01") # Modern Steel cutoff

dt_tanks[, wall_class := case_when(
  # 1. FEDERAL MANDATE (Post-2016)
  tank_installed_date >= fed_sec_date ~ "Double",
  
  # 2. EXPLICIT DOUBLE INDICATORS
  str_detect(mat_upper, "DOUBLE WALL|DBW|DBL|JACKET|2WALL|DOUBLEWALL") ~ "Double",
  
  # 3. COMPOSITE / FIBERGLASS COATED STEEL (Date Dependent)
  # Post-1990 = Double (Composite), Pre-1990 = Unknown (Might be coated single)
  str_detect(mat_upper, "FIBERGLASS COATED STEEL") & 
    !str_detect(mat_upper, "SINGLE") & 
    tank_installed_date >= frp_cutoff ~ "Double",
  
  str_detect(mat_upper, "FIBERGLASS COATED STEEL") & 
    !str_detect(mat_upper, "SINGLE") & 
    tank_installed_date < frp_cutoff ~ "Unknown",
  
  # 4. FRP (Date Dependent)
  str_detect(mat_upper, "FIBERGLASS REINFORCED PLASTIC") & 
    !str_detect(mat_upper, "SINGLE") & 
    tank_installed_date >= frp_cutoff ~ "Double",
  
  str_detect(mat_upper, "FIBERGLASS REINFORCED PLASTIC") & 
    !str_detect(mat_upper, "SINGLE") & 
    tank_installed_date < frp_cutoff ~ "Unknown",
  
  # 5. EXPLICIT SINGLE INDICATORS
  str_detect(mat_upper, "SINGLE|SGL") ~ "Single",
  
  # 6. BRAND NAMES
  str_detect(mat_upper, "STIP3|STI-P3") ~ "Single",
  str_detect(mat_upper, "ACT-100|ACT 100|ACT100|GLASTEEL|TITAN|PERMATANK|ELUTRON|TCI") ~ "Double",
  
  # 7. PLAIN STEEL (Date Analysis)
  # Post-2016 = Double (Rule 1 covers this, but explicit here)
  # Pre-2016 = Single (No state mandate for secondary containment on steel before Fed rule)
  str_detect(mat_upper, "^STEEL") & tank_installed_date < fed_sec_date ~ "Single",
  
  # 8. MISSING DATA (With Date)
  (is.na(mat_upper) | mat_upper == "NA NA") & tank_installed_date >= fed_sec_date ~ "Double",
  
  # 9. DEFAULT
  TRUE ~ "Unknown"
)]

# Convert to Flags
dt_tanks[, `:=`(
  double_walled = as.integer(wall_class == "Double"),
  single_walled = as.integer(wall_class == "Single"),
  unknown_walled = as.integer(wall_class == "Unknown")
)]

# --- F. Capacity ---
# AL data does not provide a clean capacity column. Set to NA to avoid bad parsing.
dt_tanks[, capacity := NA_real_]

# 4. Geography Merge -----------------------------------------------------------
message("Merging Geography...")

# Clean Sites
dt_sites_clean <- dt_sites[, .(
  site_link_id = number, 
  facility_name = name,
  latitude = as.numeric(latitude),
  longitude = as.numeric(longitude),
  address_raw = address,
  city = city,
  zip_code = zip,
  county_name = county
)]

# Left Join
harmonized <- merge(dt_tanks, dt_sites_clean, by = "site_link_id", all.x = TRUE)

# 5. LUST Placeholder ----------------------------------------------------------
# AL LUST files were not provided in this batch.
# We populate leak columns with 0/NA to allow merging into the Master Panel.
# NOTE: If EPA LUST data is added later, this section would be replaced by a merge.
message("Initializing Leak Columns (No Local LUST File)...")
harmonized[, `:=`(
  leak_after_closure = 0,
  leak_before_NFA_before_closure = 0,
  leak_before_NFA_after_closure = 0,
  no_leak = NA_integer_ # We don't know if they leaked or not without the file
)]

# 6. Final Formatting ----------------------------------------------------------
message("Finalizing Dataset...")

final_df <- harmonized[, .(
  facility_id = permit_number, # Unique Key
  tank_id = tank_identification_number,
  facility_name, 
  street_address = address_raw,
  city,
  zip_code,
  county_name,
  state = state_abbr,
  
  latitude, longitude,
  
  tank_status,
  tank_installed_date,
  tank_closed_date,
  
  capacity,
  
  is_gasoline, is_diesel, is_oil_kerosene, is_jet_fuel, is_other,
  
  single_walled, double_walled, unknown_walled,
  
  leak_after_closure, 
  leak_before_NFA_before_closure, 
  leak_before_NFA_after_closure, 
  no_leak
)]

# 7. Save ----------------------------------------------------------------------
outfile <- file.path(out_dir, paste0(state_abbr, "_Harmonized_UST_tanks.csv"))
fwrite(final_df, outfile)

cat(paste0("\n------------------------------------------------\n"))
cat(paste0("ALABAMA PROCESSING COMPLETE\n"))
cat(paste0("------------------------------------------------\n"))
cat(paste0("Output File:   ", outfile, "\n"))
cat(paste0("Total Tanks:   ", nrow(final_df), "\n"))
cat(paste0("Double Walled: ", round(mean(final_df$double_walled)*100,1), "%\n"))
cat(paste0("Single Walled: ", round(mean(final_df$single_walled)*100,1), "%\n"))
cat(paste0("Geocoded:      ", round(mean(!is.na(final_df$latitude))*100,1), "%\n"))
cat(paste0("------------------------------------------------\n"))

## Tennessee UST Dataset Creation
# Script: 12_Clean_TN.R
# Purpose: Harmonize TN UST/LUST data matching the LA/NM Architecture.
# Inputs:
#   1. State Tanks: 'ust_all-tn-compartments.xlsx'
#   2. State LUST: 'ust_all-tn-environmental-sites.xlsx'
#   3. EPA Geo: 'Facilities.csv' (Lat/Long)
#   4. Crosswalk: 'uszips.csv' (Zip -> County)

library(tidyverse)
library(data.table)
library(here)
library(janitor)
library(readxl)
library(stringr)
library(lubridate)

# 0. Setup ---------------------------------------------------------------------
state_abbr <- "TN"
tn_path    <- here("Data", "Raw", "state_databases", "Tennessee")
epa_path   <- here("Data", "Raw") # Contains Facilities.csv and uszips.csv
out_dir    <- here("Data", "Raw", "state_databases", "Tennessee")

if (!dir.exists(tn_path)) stop("TN data directory not found!")

# 1. Helper Functions ----------------------------------------------------------

# A. ID Cleaner (Matches Diagnostic Findings)
clean_ids <- function(x) {
  x <- as.character(x)
  x <- trimws(x)
  # EPA has "TN" prefix (e.g. TN10021), State does not (10021). Strip it.
  x <- gsub("^TN[- ]?", "", x, ignore.case = TRUE)
  return(x)
}

# B. Robust Tank Date Parser (Excel Serial & Text)
parse_tn_tank_date <- function(x) {
  num_dates <- suppressWarnings(as.numeric(x))
  d_out <- as.Date(num_dates, origin = "1899-12-30")
  
  needs_text <- !is.na(x) & is.na(d_out)
  if (any(needs_text)) {
    txt_vals <- x[needs_text]
    # TN tanks sometimes use YYYY-MM-DD
    d_parsed <- as.Date(parse_date_time(txt_vals, orders = c("ymd", "b-d-Y", "mdY"), quiet = TRUE))
    d_out[needs_text] <- d_parsed
  }
  return(d_out)
}

# C. LUST Date Parser (Specific to EnvSites: MMM-DD-YYYY)
parse_tn_lust_date <- function(x) {
  x <- trimws(x)
  as.Date(parse_date_time(x, orders = c("b-d-Y", "b-d-y", "mdY"), quiet = TRUE))
}

# D. Wall Classification (Tennessee Specific Algorithm)
classify_walls <- function(dt) {
  # Initialize Flags
  dt[, `:=`(double_walled=0, single_walled=0, unknown_walled=0)]
  
  # Standardize Inputs
  cat_lower <- tolower(dt$category_of_construction)
  mat_lower <- tolower(dt$tank_construction)
  install_date <- dt$tank_installed_date
  
  # Regulatory Date Constant: July 24, 2007 (Tenn. Comp. R. & Regs. 0400-18-01-.02(1)(c))
  TN_SECONDARY_MANDATE <- as.Date("2007-07-24")
  FRP_DOUBLE_STANDARD  <- as.Date("1990-01-01")
  
  # --- LOGIC BLOCK ---
  
  # 1. TENNESSEE SECONDARY CONTAINMENT DATE (Highest Priority)
  # If installed after mandate, must be secondary contained.
  dt[!is.na(install_date) & install_date >= TN_SECONDARY_MANDATE, double_walled := 1]
  
  # 2. EXPLICIT CATEGORY OF CONSTRUCTION (If not already double by date)
  # Only check rows not already classified as double
  dt[double_walled == 0 & grepl("double", cat_lower), double_walled := 1]
  dt[double_walled == 0 & grepl("single", cat_lower), single_walled := 1]
  
  # 3. SECONDARY CONTAINED TANK CONSTRUCTION TYPES (If still unclassified)
  # Composite and Poly Jacket are inherently double-walled
  dt[double_walled == 0 & single_walled == 0 & (
    grepl("composite", mat_lower) | 
    grepl("jacket", mat_lower)
  ), double_walled := 1]
  
  # 4. FIBERGLASS/PLASTIC (DATE-DEPENDENT)
  # Only applied to "Fiberglass Reinforced Plastic" if unclassified
  # Post-1990 -> Double; Pre-1990 or Null -> Unknown (Skip assignment)
  dt[double_walled == 0 & single_walled == 0 & grepl("fiberglass reinforced plastic", mat_lower) & 
       !is.na(install_date) & install_date >= FRP_DOUBLE_STANDARD, 
     double_walled := 1]
  
  # 5. SINGLE-WALLED STEEL INDICATORS
  # Basic Steel and StiP3
  dt[double_walled == 0 & single_walled == 0 & (
    mat_lower == "steel" | 
    grepl("stip3", mat_lower) | 
    grepl("cathodically protected steel", mat_lower)
  ), single_walled := 1]
  
  # 6. CONCRETE TANKS
  dt[double_walled == 0 & single_walled == 0 & grepl("concrete", mat_lower), single_walled := 1]
  
  # 7. UNKNOWN / INSUFFICIENT DATA
  # Any record that didn't hit a rule above gets marked Unknown
  dt[double_walled == 0 & single_walled == 0, unknown_walled := 1]
  
  return(dt)
}

# E. Substance Classification
classify_substance <- function(dt) {
  dt[, sub_lower := tolower(product)]
  dt[, `:=`(
    is_gasoline = as.integer(grepl("gasoline|gas|ethan|e10|e85|racing|avgas", sub_lower)),
    is_diesel   = as.integer(grepl("diesel|dsl|bio|kerosene", sub_lower)),
    is_oil_kerosene = as.integer(grepl("oil|lube|hydr|fluid", sub_lower)),
    is_jet_fuel = as.integer(grepl("jet|aviation|jp", sub_lower)),
    is_other    = 0
  )]
  dt[is_gasoline==0 & is_diesel==0 & is_oil_kerosene==0 & is_jet_fuel==0, is_other := 1]
  dt[, sub_lower := NULL]
  return(dt)
}

# 2. Load Data -----------------------------------------------------------------
message("Loading Tennessee Data...")

# A. Compartments (Tanks)
dt_tanks <- read_excel(file.path(tn_path, "ust_all-tn-compartments.xlsx"), col_types = "text") %>% 
  clean_names() %>% as.data.table()

# B. EnvSites (LUST)
dt_lust_state <- read_excel(file.path(tn_path, "ust_all-tn-environmental-sites.xlsx"), col_types = "text") %>% 
  clean_names() %>% as.data.table()

# C. EPA Facilities (For Lat/Long)
message("Loading EPA Facilities...")
if (file.exists(file.path(epa_path, "Facilities.csv"))) {
  dt_epa_fac <- fread(file.path(epa_path, "Facilities.csv"), colClasses = "character") %>% 
    clean_names() %>% as.data.table()
  
  # Filter for TN
  dt_geo <- dt_epa_fac[state == 'Tennessee', .(facility_id, latitude, longitude)]
  
  # CLEAN KEY: Strip "TN" (e.g. TN10021 -> 10021)
  dt_geo[, facility_id_clean := clean_ids(facility_id)]
  
  dt_geo <- dt_geo[, .(
    facility_id_clean,
    latitude = as.numeric(latitude),
    longitude = as.numeric(longitude)
  )]
  # Dedupe
  dt_geo <- dt_geo[!is.na(latitude) & !is.na(longitude)][!duplicated(facility_id_clean)]
} else {
  warning("Facilities.csv not found. Lat/Long will be NA.")
  dt_geo <- data.table(facility_id_clean=character(), latitude=numeric(), longitude=numeric())
}

# D. Zip Crosswalk (For County)
message("Loading Zip Crosswalk...")
if (file.exists(file.path(epa_path, "uszips.csv"))) {
  dt_zip <- fread(file.path(epa_path, "uszips.csv"), colClasses = "character") %>% clean_names()
  
  # Prepare Crosswalk: 5-digit Zip -> County Name
  dt_zip[, zip5 := str_sub(trimws(zip), 1, 5)]
  dt_zip <- dt_zip[, .(zip5, county_name)]
  dt_zip <- dt_zip[!duplicated(zip5)] # 1-to-1 mapping
} else {
  warning("uszips.csv not found. County names will be missing.")
  dt_zip <- data.table(zip5=character(), county_name=character())
}

# 3. Process Tanks -------------------------------------------------------------
message("Processing Tank Attributes...")

# IDs
dt_tanks[, `:=`(
  facility_id = clean_ids(facility_id_ust),
  tank_id = compartment_id,
  facility_name = facility_name
)]

# Dates
dt_tanks[, `:=`(
  tank_installed_date = parse_tn_tank_date(date_tank_installed),
  tank_closed_date    = parse_tn_tank_date(date_tank_closed)
)]
# Fallback logic: Use compartment close date if tank close date missing
dt_tanks[is.na(tank_closed_date), tank_closed_date := parse_tn_tank_date(date_compartment_closed)]

# Status Logic (Matches LA)
dt_tanks[, tank_status := fcase(
  grepl("Current|Temp", status, ignore.case=TRUE), "Open",
  grepl("Perm|Out of Use|Closed|Remov", status, ignore.case=TRUE), "Closed",
  default = "Closed"
)]
# Filter out "Temporary" if strict alignment with LA (LA removed them, keeping here for completeness or remove if desired)
# dt_tanks <- dt_tanks[tank_status != "Temporary"] 

# Classifications
dt_tanks <- classify_substance(dt_tanks)
dt_tanks <- classify_walls(dt_tanks)

# Capacity
dt_tanks[, capacity := as.numeric(gsub("[^0-9.]", "", compartment_capacity))]

# Extract Zip for Matching
dt_tanks[, zip5 := str_sub(trimws(facility_zip), 1, 5)]

# 4. Process LUST Data ---------------------------------------------------------
message("Processing LUST Data...")

dt_lust_clean <- dt_lust_state[, .(
  facility_id = clean_ids(facilityid),
  report_date = parse_tn_lust_date(discoverydate),
  nfa_date = as.Date(NA), # TN data lacks specific NFA dates
  lust_status = currentstatus
)]
dt_lust_clean <- dt_lust_clean[!is.na(facility_id) & facility_id != ""]

# 5. Merging -------------------------------------------------------------------
message("Merging Datasets...")

# A. Merge LUSTs (Left Join - One Facility can have multiple LUSTs)
tanks_lust <- merge(dt_tanks, dt_lust_clean, by = "facility_id", all.x = TRUE, allow.cartesian = TRUE)

# B. Calculate Leak Flags (Matches LA Logic)
tanks_lust[, `:=`(
  leak_after_closure = as.integer(!is.na(tank_closed_date) & !is.na(report_date) & report_date > tank_closed_date),
  # Without NFA dates, these flags default to 0
  leak_before_NFA_before_closure = 0, 
  leak_before_NFA_after_closure = 0,
  # No leak if no report found and tank is closed
  no_leak = as.integer(is.na(report_date) & !is.na(tank_closed_date))
)]

# 6. Aggregation ---------------------------------------------------------------
message("Aggregating to Unique Tank Level...")

final_df <- tanks_lust[, .(
  facility_name = first(facility_name),
  city = first(facility_city),
  zip_code = first(facility_zip),
  zip5 = first(zip5), # Keep for crosswalk
  
  tank_status = first(tank_status),
  tank_installed_date = first(tank_installed_date),
  tank_closed_date = first(tank_closed_date),
  capacity = mean(capacity, na.rm=TRUE),
  
  # Max flags for 1/0
  is_gasoline = max(is_gasoline, na.rm=TRUE),
  is_diesel = max(is_diesel, na.rm=TRUE),
  is_oil_kerosene = max(is_oil_kerosene, na.rm=TRUE),
  is_jet_fuel = max(is_jet_fuel, na.rm=TRUE),
  is_other = max(is_other, na.rm=TRUE),
  
  single_walled = max(single_walled, na.rm=TRUE),
  double_walled = max(double_walled, na.rm=TRUE),
  unknown_walled = max(unknown_walled, na.rm=TRUE),
  
  leak_after_closure = max(leak_after_closure, na.rm=TRUE),
  leak_before_NFA_before_closure = 0,
  leak_before_NFA_after_closure = 0,
  no_leak = min(no_leak, na.rm=TRUE)
), by = .(facility_id, tank_id)]

# 7. Final Geography Joins -----------------------------------------------------
message("Applying Geography (Lat/Long + County)...")

# A. Lat/Long from EPA (Left Join)
final_df <- merge(final_df, dt_geo, by.x = "facility_id", by.y = "facility_id_clean", all.x = TRUE)

# B. County from Zip Crosswalk (Left Join)
final_df <- merge(final_df, dt_zip, by = "zip5", all.x = TRUE)

# Final Cleanup
final_df[, state := state_abbr]
final_df[, street_address := NA_character_]
# Ensure correct order/selection of columns to match LA
cols_to_keep <- c(
  "facility_id", "facility_name", "tank_id", "state", "county_name",
  "tank_installed_date", "tank_closed_date", "tank_status",
  "leak_after_closure", "leak_before_NFA_before_closure", "leak_before_NFA_after_closure", "no_leak",
  "capacity", "single_walled", "double_walled", "unknown_walled",
  "is_gasoline", "is_diesel", "is_oil_kerosene", "is_jet_fuel", "is_other",
  "latitude", "longitude"
)

# Subset and fill any missing columns with NA if strict binding required
missing_cols <- setdiff(cols_to_keep, names(final_df))
if(length(missing_cols) > 0) final_df[, (missing_cols) := NA]

final_df <- final_df[, ..cols_to_keep]

# 8. Save ----------------------------------------------------------------------
outfile_tanks <- file.path(out_dir, paste0(state_abbr, "_Harmonized_UST_tanks.csv"))
outfile_lust  <- file.path(out_dir, paste0(state_abbr, "_Harmonized_LUST.csv"))

fwrite(final_df, outfile_tanks)
fwrite(dt_lust_clean, outfile_lust)

cat(paste0("\n------------------------------------------------\n"))
cat(paste0("TENNESSEE PROCESSING COMPLETE\n"))
cat(paste0("------------------------------------------------\n"))
cat(paste0("Output File:   ", outfile_tanks, "\n"))
cat(paste0("Total Tanks:   ", nrow(final_df), "\n"))
cat(paste0("Lat/Long:      ", round(mean(!is.na(final_df$latitude))*100,1), "% coverage\n"))
cat(paste0("County Names:  ", round(mean(!is.na(final_df$county_name))*100,1), "% coverage\n"))
cat(paste0("------------------------------------------------\n"))


## Colorado UST Dataset Creation
# Script: 13_Clean_CO.R
# Author: Validated Classification Logic (Updated Jan 2026)
# Description: Harmonizes CO UST/LUST data matching LA/TN Architecture.
#
# Key Features:
#   1. Regulatory Mandate (Double): Implements "Aug 1, 2008" cutoff (7 CCR 1101-14 § 2-2-1).
#   2. Material Logic: Maps "COMP"/"JKT" to Double; "STI-P3"/"Bare" to Single.
#   3. AST Exclusion: Filters out tanks with "AST:" in Tank Type
#   4. Geography Bridge: Backfills missing Tank Lat/Long using Release file coordinates.
#   5. Aggregation: Collapses LUST history to Facility-Tank level flags.

library(tidyverse)
library(data.table)
library(here)
library(lubridate)
library(janitor)
library(stringr)

# 0. Setup ----------------------------------------------------------------
STATE_ABBR <- "CO"
raw_dir    <- here("Data", "Raw", "state_databases", "Colorado")
out_dir    <- here("Data", "Raw", "state_databases", "Colorado")

if (!dir.exists(raw_dir)) stop("CO data directory not found!")

# 1. Helper Functions -----------------------------------------------------

# A. Robust Date Parser
safe_parse_date <- function(date_vec) {
  # Handles standard CO formats (M/D/YYYY) and potential ISO
  lubridate::parse_date_time(date_vec, orders = c("mdY", "Ymd", "dmY", "mdy"), quiet = TRUE) %>% 
    as.Date()
}

# B. ID Cleaner
clean_ids <- function(x) {
  x <- as.character(x)
  trimws(x)
}

# C. Classification Logic (CO Specific)
classify_co_walls <- function(dt) {
  # Initialize
  dt[, `:=`(double_walled=0, single_walled=0, unknown_walled=0)]
  
  # Constants
  CO_SECONDARY_DATE <- as.Date("2008-08-01")    # 7 CCR 1101-14 § 2-2-1
  FED_SECONDARY_DATE <- as.Date("2016-04-11")
  FRP_CUTOFF_DATE   <- as.Date("1990-01-01")
  
  # Prepare columns
  mat_clean  <- trimws(dt$tank_material)
  wall_clean <- trimws(dt$tank_wall_type)
  inst_date  <- dt$tank_installed_date
  
  # --- LOGIC BLOCK ---
  
  # 1. REGULATORY DATES (Priority 1)
  # CO requires secondary containment for all installs >= Aug 1, 2008
  dt[!is.na(inst_date) & inst_date >= CO_SECONDARY_DATE, double_walled := 1]
  
  # 2. EXPLICIT WALL TYPE (Rare in CO, but trust if present)
  dt[double_walled == 0 & wall_clean == "Double-Walled", double_walled := 1]
  dt[double_walled == 0 & single_walled == 0 & wall_clean == "Single-Walled", single_walled := 1]
  
  # 3. MATERIAL INDICATORS: INHERENTLY DOUBLE
  # Composite and Jacketed tanks are double-walled
  dt[double_walled == 0 & single_walled == 0 & (
    grepl("COMP|Composite", mat_clean, ignore.case=TRUE) |
    grepl("JKT|Jacketed", mat_clean, ignore.case=TRUE)
  ), double_walled := 1]
  
  # 4. FIBERGLASS (Date Dependent)
  # Post-1990 FRP is standard Double; Pre-1990 is ambiguous (Unknown)
  dt[double_walled == 0 & single_walled == 0 & grepl("FRP|Fiberglass", mat_clean, ignore.case=TRUE) &
       !is.na(inst_date) & inst_date >= FRP_CUTOFF_DATE,
     double_walled := 1]
  
  # 5. SINGLE WALL INDICATORS
  # Bare Steel, Asphalt Coated, Epoxy Coated, Cathodically Protected, STI-P3
  dt[double_walled == 0 & single_walled == 0 & (
    grepl("Asphalt|Bare|Epoxy|Cathodically|ST:|STI-P3", mat_clean, ignore.case=TRUE)
  ), single_walled := 1]
  
  # 6. UNKNOWN
  dt[double_walled == 0 & single_walled == 0, unknown_walled := 1]
  
  return(dt)
}

# D. Substance Classification
classify_substance <- function(dt) {
  dt[, sub_lower := tolower(product)]
  dt[, `:=`(
    is_gasoline = as.integer(grepl("gasoline|gas|unleaded|premium|regular|e-?10|e-?15|e-?85|mtbe", sub_lower)),
    is_diesel   = as.integer(grepl("diesel|dsl|bio|kerosene|heating oil", sub_lower)),
    is_oil_kerosene = as.integer(grepl("oil|lube|hydr|fluid|waste", sub_lower)),
    is_jet_fuel = as.integer(grepl("jet|aviation|av ?gas", sub_lower)),
    is_other    = 0
  )]
  dt[is_gasoline==0 & is_diesel==0 & is_oil_kerosene==0 & is_jet_fuel==0, is_other := 1]
  dt[, sub_lower := NULL]
  return(dt)
}

# 2. Load Raw Data --------------------------------------------------------
cat("\nLoading Colorado Data...\n")

# Use fread for robustness
f_tanks    <- file.path(raw_dir, "Regulated_Storage_Tanks_in_Colorado__Oil___Public_Safety_20250728.csv")
f_releases <- file.path(raw_dir, "Petroleum_Releases__Colorado_Oil___Public_Safety_20250728.csv")

# Load with character classes to preserve IDs
dt_tanks_raw    <- fread(f_tanks, colClasses = "character", na.strings=c("", "NA", "NULL")) %>% clean_names()
dt_releases_raw <- fread(f_releases, colClasses = "character", na.strings=c("", "NA", "NULL")) %>% clean_names()

cat("Loaded", nrow(dt_tanks_raw), "tanks and", nrow(dt_releases_raw), "releases.\n")

# 3. Process Tanks --------------------------------------------------------
cat("\nProcessing Tank Attributes...\n")

# drop all ASTs and LPGs

# A. Filter to only UST (Crucial CO Step)
# Exclude tanks where material starts with "AST:"
cat("Excluding", nrow(dt_tanks_raw[grepl("AST", tank_type)]), "Aboveground Storage Tanks (ASTs)...\n")
dt_tanks_raw_no_AST <- dt_tanks_raw[grepl("UST", tank_type)]


# A. Rename & Select
dt_tanks <- dt_tanks_raw_no_AST[, .(
  facility_id = clean_ids(facility_id),
  facility_name = facility_name,
  tank_id = clean_ids(tank_tag), # Use Tag as ID (unique within facility)
  tank_installed_date = safe_parse_date(installation_date),
  tank_closed_date = safe_parse_date(closure_date),
  status_raw = tank_status,
  capacity = as.numeric(gsub("[^0-9.]", "", capacity_gallons)),
  tank_material,
  tank_wall_type,
  product,
  county_name = county,
  # CO Tank file typically lacks Lat/Long, will merge from Release file later
  latitude = NA_real_,
  longitude = NA_real_
)]

# C. Status Logic
dt_tanks[, tank_status := fcase(
  grepl("Currently In Use", status_raw, ignore.case=TRUE), "Open",
  grepl("Closed|Permanently", status_raw, ignore.case=TRUE), "Closed",
  grepl("Temporarily|Pending", status_raw, ignore.case=TRUE), "Temporary",
  default = "Closed" # Default conservative
)]
dt_tanks <- dt_tanks[tank_status != "Temporary"]
dt_tanks


# D. Classifications
dt_tanks <- classify_co_walls(dt_tanks)
dt_tanks <- classify_substance(dt_tanks)

## print the number of closed tanks with missing closure date
num_missing_closure <- nrow(dt_tanks[tank_status == "Closed" & is.na(tank_closed_date)])
num_closure <- nrow(dt_tanks[tank_status == "Closed"])

num_missing_closure
cat("Closed Tanks with Missing Closure Date:", num_missing_closure, "out of", num_closure,"(",round(num_missing_closure/num_closure,2)*100,"%)" ,"\n")
install_years_of_closed_na = dt_tanks[tank_status == "Closed" & is.na(tank_closed_date),.N, by =.(year(tank_installed_date))][order(year)]
install_years_of_closed_na
dt_tanks[tank_status == "Closed" & is.na(tank_closed_date),.N, by =.(tank_status,status_raw)][order(N)]
bad_closures = dt_tanks[tank_status == "Closed" & is.na(tank_closed_date)]
dt_tanks_raw[facility_id == '20748']
dt_tanks_raw[,.N, by=.(tank_type)][order(-N)]

# 4. Process LUST (Releases) ----------------------------------------------
cat("\nProcessing LUST/Releases...\n")

# CO 'Petroleum Releases' file contains Event Dates and Closure Dates (NFA)
dt_lust <- dt_releases_raw[, .(
  facility_id = clean_ids(facility_id),
  lust_id = release_number,
  report_date = safe_parse_date(release_date),
  nfa_date = safe_parse_date(closure_date), # CO uses "Closure_Date" for the release event closure
  # Capture Geography from Release file if available
  rel_lat = as.numeric(latitude),
  rel_long = as.numeric(longitude)
)]

# 5. Geography Bridge -----------------------------------------------------
cat("\nBridging Geography from Release File to Tank File...\n")

# Create a Facility -> Lat/Long lookup from the Release file
# (Taking the first valid coordinate pair per facility)
geo_lookup <- dt_lust[!is.na(rel_lat) & !is.na(rel_long), .(
  latitude = first(rel_lat),
  longitude = first(rel_long)
), by = facility_id]

# Merge coordinates into Tanks
dt_tanks[, c("latitude", "longitude") := NULL] # Remove placeholders
dt_tanks <- merge(dt_tanks, geo_lookup, by="facility_id", all.x=TRUE)

# 6. Merge Tanks & LUSTs --------------------------------------------------
cat("\nMerging Tanks and LUST History...\n")

# Merge (One Facility -> Many LUSTs)
# Note: allow.cartesian=TRUE required as facilities have multiple tanks AND multiple releases
merged <- merge(dt_tanks, dt_lust[, .(facility_id, report_date, nfa_date)], 
                by="facility_id", all.x=TRUE, allow.cartesian=TRUE)

# 7. Calculate Leak Flags & Aggregate -------------------------------------
cat("\nCalculating Leak Flags & Aggregating...\n")

merged[, `:=`(
  # Leak after tank closure? (Zombie Tank Risk)
  leak_after_closure = as.integer(!is.na(tank_closed_date) & !is.na(report_date) & report_date > tank_closed_date),
  
  # Leak occurred while tank open, but NFA issued BEFORE tank closed
  leak_before_NFA_before_closure = as.integer(!is.na(tank_closed_date) & !is.na(report_date) & 
                                                report_date < tank_closed_date & 
                                                (is.na(nfa_date) | nfa_date < tank_closed_date)),
  
  # Leak occurred while tank open, but NFA issued AFTER tank closed
  leak_before_NFA_after_closure = as.integer(!is.na(tank_closed_date) & !is.na(report_date) & 
                                               report_date < tank_closed_date & 
                                               !is.na(nfa_date) & nfa_date > tank_closed_date),
  
  # No leak recorded
  no_leak = as.integer(is.na(report_date) & !is.na(tank_closed_date))
)]

# Aggregation to Facility-Tank Level
# Taking max() of flags to capture if *any* release at the facility triggers the condition for the tank
final_df <- merged[, .(
  facility_name = first(facility_name),
  county_name = first(county_name),
  tank_installed_date = first(tank_installed_date),
  tank_closed_date = first(tank_closed_date),
  tank_status = first(tank_status),
  capacity = mean(capacity, na.rm=TRUE),
  
  # Lat/Long (propagated from bridge)
  latitude = first(latitude),
  longitude = first(longitude),
  
  # Classification Flags (Constant per tank)
  single_walled = max(single_walled, na.rm=TRUE),
  double_walled = max(double_walled, na.rm=TRUE),
  unknown_walled = max(unknown_walled, na.rm=TRUE),
  
  is_gasoline = max(is_gasoline, na.rm=TRUE),
  is_diesel = max(is_diesel, na.rm=TRUE),
  is_oil_kerosene = max(is_oil_kerosene, na.rm=TRUE),
  is_jet_fuel = max(is_jet_fuel, na.rm=TRUE),
  is_other = max(is_other, na.rm=TRUE),
  
  # Aggregated Risk Flags
  leak_after_closure = max(leak_after_closure, na.rm=TRUE),
  leak_before_NFA_before_closure = max(leak_before_NFA_before_closure, na.rm=TRUE),
  leak_before_NFA_after_closure = max(leak_before_NFA_after_closure, na.rm=TRUE),
  no_leak = min(no_leak, na.rm=TRUE) # Only 1 if ALL history is clean (conservative)
  
), by = .(facility_id, tank_id)]

final_df[, state := STATE_ABBR]

# 8. Export ---------------------------------------------------------------
# Filter Columns to strict schema
cols_to_keep <- c(
  "facility_id", "facility_name", "tank_id", "state", "county_name",
  "tank_installed_date", "tank_closed_date", "tank_status",
  "leak_after_closure", "leak_before_NFA_before_closure", "leak_before_NFA_after_closure", "no_leak",
  "capacity", "single_walled", "double_walled", "unknown_walled",
  "is_gasoline", "is_diesel", "is_oil_kerosene", "is_jet_fuel", "is_other",
  "latitude", "longitude"
)

# Fill missing columns if any
missing <- setdiff(cols_to_keep, names(final_df))
if(length(missing)>0) final_df[, (missing) := NA]

final_df <- final_df[, ..cols_to_keep]

cat("\nSaving Harmonized Files...\n")
fwrite(final_df, file.path(out_dir, "CO_Harmonized_UST_tanks.csv"))
fwrite(dt_lust[, .(facility_id, lust_id, report_date, nfa_date, state="CO")], 
       file.path(out_dir, "CO_Harmonized_LUST.csv"))

cat("------------------------------------------------\n")
cat("COLORADO PROCESSING COMPLETE\n")
cat("Total UST Rows: ", nrow(final_df), "\n")
cat("Geo Coverage:   ", round(mean(!is.na(final_df$latitude))*100, 1), "%\n")
cat("county Coverage: ", round(mean(!is.na(final_df$county_name))*100, 1), "%\n")
cat("------------------------------------------------\n")