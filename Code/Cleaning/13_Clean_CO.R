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