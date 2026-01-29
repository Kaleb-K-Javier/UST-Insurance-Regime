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
