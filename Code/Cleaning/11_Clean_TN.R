## Tennessee UST Dataset Creation
# Script: 02_Clean_TN.R
# Author: Validated Classification Logic (Updated Jan 2026)
# Description: Harmonizes TN UST/LUST data using state files + EPA National database

# 0. Setup ----------------------------------------------------------------
COUNTYVARIABLE <- "county_name"

library(tidyverse)
library(data.table)
library(here)
library(lubridate)
library(janitor)
library(readxl)
library(stringr)

# 1. Helper Functions -----------------------------------------------------

# Vectorized function to standardize county names
standardize_county_name <- function(county_name) {
  name <- tolower(county_name)
  name <- gsub(" county$| counties$| parish$| parishes$", "", name)
  name <- gsub("[^a-z ]", "", name)
  name <- trimws(gsub("\\s+", " ", name))
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
      orders = c("ymd", "mdy", "dmy", "Y-m-d", "m/d/Y", "b-d-Y", "b-d-y"),
      quiet = TRUE
    )
    parsed_dates[needs_string_parse] <- as.Date(string_dates)
  }
  
  return(parsed_dates)
}

# 2. Core Logic: Classification -------------------------------------------

# Function to classify tank walls (Tennessee-specific logic)
classify_tank_walls <- function(data) {
  result <- copy(data)
  
  # Tennessee has explicit "Category Of Construction" field with Single/Double Wall
  # But only ~7K of 60K tanks have this field populated
  # Strategy: Use explicit flags when available, then construction materials
  
  # Initialize flags
  result[, `:=`(double_walled = 0, single_walled = 0, unknown_walled = 1, missing_walled = 1)]
  
  # PRIORITY 1: Explicit Category Of Construction
  if("category_of_construction" %in% names(result)) {
    result[!is.na(category_of_construction), `:=`(
      double_walled = as.integer(grepl("double", category_of_construction, ignore.case = TRUE)),
      single_walled = as.integer(grepl("single", category_of_construction, ignore.case = TRUE)),
      unknown_walled = 0,
      missing_walled = 0
    )]
  }
  
  # PRIORITY 2: Construction Material Indicators (for tanks without explicit category)
  if("tank_construction" %in% names(result)) {
    # Double-wall indicators
    double_patterns <- c(
      "polyethylene.*jacket", 
      "poly.*jacket",
      "double.*wall",
      "secondary.*containment"
    )
    double_regex <- paste(double_patterns, collapse = "|")
    
    # Single-wall indicators (steel without protection, basic FRP)
    # Note: Cathodically Protected Steel is single-wall with upgrade
    # Composite (Steel w/FRP) is typically double-wall
    result[unknown_walled == 1 & !is.na(tank_construction), `:=`(
      double_walled = as.integer(
        grepl(double_regex, tank_construction, ignore.case = TRUE) |
        grepl("composite.*steel.*frp", tank_construction, ignore.case = TRUE)
      ),
      single_walled = as.integer(
        grepl("^steel$|cathodically protected steel|^fiberglass", tank_construction, ignore.case = TRUE)
      )
    )]
    
    # Update unknown/missing flags
    result[, `:=`(
      unknown_walled = as.integer(double_walled == 0 & single_walled == 0),
      missing_walled = as.integer(double_walled == 0 & single_walled == 0)
    )]
  }
  
  cat("\nTank wall classification summary:\n")
  print(result[, .(total=.N, single=sum(single_walled), double=sum(double_walled), unknown=sum(unknown_walled))])
  
  return(result)
}

# Function to classify substances
classify_substances <- function(data, substance_col_name = "product") {
  result <- copy(data)
  result[, `:=`(is_gasoline=0, is_diesel=0, is_oil_kerosene=0, is_jet_fuel=0, is_other=1)]
  
  if(substance_col_name %in% names(result)) {
    result[, sub_lower := tolower(get(substance_col_name))]
    
    # Detailed regex patterns
    gas_pat <- paste(c("gas", "gasolin", "petrol", "motor fuel", "e10", "e15", "e85", "e\\d+", "unleaded", "premium", "regular", "ethanol blend", "racing fuel"), collapse = "|")
    dsl_pat <- paste(c("diesel", "dsl", "dl$", "\\bd\\b", "bio.?diesel", "uls diesel", "off.?road diesel"), collapse = "|")
    oil_pat <- paste(c("oil", "kerosene", "kerosine", "heating", "waste.*oil", "used.*oil", "new oil", "mineral spirits", "transmission", "gear oil", "power steering"), collapse = "|")
    jet_pat <- paste(c("jet", "aviation", "av gas", "avgas", "airplane"), collapse = "|")
    
    # Apply patterns
    result[!is.na(sub_lower), is_gasoline := as.integer(grepl(gas_pat, sub_lower, perl=TRUE))]
    result[!is.na(sub_lower), is_diesel := as.integer(grepl(dsl_pat, sub_lower, perl=TRUE))]
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

# 3. Load Tennessee State Data ---------------------------------------------
cat("\nLoading Tennessee UST data files...\n")
tn_path <- here("Data", "Raw", "state_databases", "Tennessee")

# Load Compartments (primary tank file)
TN_compartments_raw <- read_excel(
  file.path(tn_path, "ust_all-tn-compartments.xlsx"), 
  col_types = "text"
) %>%
  janitor::clean_names() %>%
  as.data.table()

# Load Facilities (for facility-level info)
TN_facilities_raw <- read_excel(
  file.path(tn_path, "ust_all-tn-facilities.xlsx"),
  col_types = "text"
) %>%
  janitor::clean_names() %>%
  as.data.table()

# Load Environmental Sites (LUST data)
TN_env_sites_raw <- read_excel(
  file.path(tn_path, "ust_all-tn-environmental-sites.xlsx"),
  col_types = "text"
) %>%
  janitor::clean_names() %>%
  as.data.table()

# Load Reimbursement Sites (financial data)
TN_reimb_sites_raw <- read_excel(
  file.path(tn_path, "ust_all-tn-reimbursement-sites.xlsx"),
  col_types = "text"
) %>%
  janitor::clean_names() %>%
  as.data.table()

cat("Loaded:", nrow(TN_compartments_raw), "compartments,", 
    nrow(TN_facilities_raw), "facilities,",
    nrow(TN_env_sites_raw), "environmental sites,",
    nrow(TN_reimb_sites_raw), "reimbursement sites\n")

# 4. Process Tank Data (Compartments) --------------------------------------
cat("\nProcessing tank data from Compartments file...\n")

TN_UST_tanks <- TN_compartments_raw %>%
  select(
    facility_id_ust,
    facility_name,
    facility_city,
    facility_zip,
    tank_id,
    compartment_id,
    date_tank_installed,
    date_tank_closed,
    date_compartment_closed,
    status,
    product,
    tank_construction,
    category_of_construction,
    compartment_capacity,
    how_tank_closed
  ) %>%
  rename(
    facility_id = facility_id_ust,
    tank_installed_date = date_tank_installed,
    tank_closed_date = date_tank_closed,
    compartment_closed_date = date_compartment_closed
  ) %>%
  as.data.table()

# Clean Capacity
TN_UST_tanks[, capacity := as.numeric(gsub("[^0-9.]", "", compartment_capacity))]

# Parse Dates
TN_UST_tanks[, tank_installed_date := safe_parse_date(tank_installed_date)]
TN_UST_tanks[, tank_closed_date := safe_parse_date(tank_closed_date)]
TN_UST_tanks[, compartment_closed_date := safe_parse_date(compartment_closed_date)]

# Use best available closure date (prefer compartment_closed_date if available)
TN_UST_tanks[, best_closed_date := ifelse(
  !is.na(compartment_closed_date), 
  compartment_closed_date, 
  tank_closed_date
)]
TN_UST_tanks[, best_closed_date := as.Date(best_closed_date, origin = "1970-01-01")]

# Tank Status Logic
TN_UST_tanks[, tank_status := case_when(
  grepl("currently in use", status, ignore.case = TRUE) ~ "Open",
  grepl("permanently out of use|closed|removed", status, ignore.case = TRUE) ~ "Closed",
  grepl("temporarily out of use", status, ignore.case = TRUE) ~ "Temporary",
  TRUE ~ NA_character_
)]

# Remove temporary tanks
TN_UST_tanks <- TN_UST_tanks[tank_status != "Temporary" | is.na(tank_status)]

# Run Classifications
TN_UST_tanks <- classify_tank_walls(TN_UST_tanks)
TN_UST_tanks <- classify_substances(TN_UST_tanks, "product")

# 5. Merge County Data from Environmental Sites ----------------------------
cat("\nMerging county data from Environmental Sites...\n")

# Extract county mapping
county_map <- TN_env_sites_raw[, .(facilityid, facilitycounty)] %>%
  unique() %>%
  rename(facility_id = facilityid, county_name = facilitycounty)

# Merge county data
TN_UST_tanks <- merge(TN_UST_tanks, county_map, by = "facility_id", all.x = TRUE)

cat("County coverage after merge:", 
    round(sum(!is.na(TN_UST_tanks$county_name)) / nrow(TN_UST_tanks) * 100, 1), "%\n")

# 6. Load EPA National Data for LUST Dates & Geography --------------------
cat("\nLoading EPA National Database for LUST dates and geography...\n")

# Load EPA LUSTs
epa_lust_path <- here("Data", "Raw", "LUSTs.csv")
if(file.exists(epa_lust_path)) {
  epa_lusts <- fread(epa_lust_path) %>% 
    janitor::clean_names() %>%
    as.data.table()
  
  # Filter for Tennessee
  TN_EPA_LUST <- epa_lusts[grepl('Tennessee|TN', state, ignore.case = TRUE)]
  
  cat("Loaded", nrow(TN_EPA_LUST), "EPA LUST records for Tennessee\n")
  
  # Process EPA LUST data
  TN_LUST_SD <- TN_EPA_LUST %>%
    select(facility_id, release_id, release_date, nfa_date) %>%
    rename(LUST_id = release_id, report_date = release_date) %>%
    mutate(
      facility_id = as.character(facility_id),
      LUST_id = as.character(LUST_id),
      report_date = as.Date(report_date),
      nfa_date = as.Date(nfa_date),
      state = "TN"
    ) %>%
    filter(!is.na(report_date)) %>%
    as.data.table()
  
} else {
  warning("EPA LUST file not found. LUST analysis will be limited.")
  TN_LUST_SD <- data.table(facility_id=character(), LUST_id=character(), 
                           report_date=as.Date(character()), nfa_date=as.Date(character()))
}

# Load EPA Facilities for Geography
epa_fac_path <- here("Data", "Raw", "Facilities.csv")
if(file.exists(epa_fac_path)) {
  epa_facilities <- fread(epa_fac_path) %>%
    janitor::clean_names() %>%
    as.data.table()
  
  # Filter for Tennessee and extract geography
  TN_EPA_geo <- epa_facilities[grepl('Tennessee|TN', facility_state, ignore.case = TRUE)] %>%
    select(facility_id, latitude, longitude) %>%
    mutate(
      facility_id = as.character(facility_id),
      latitude = as.numeric(latitude),
      longitude = as.numeric(longitude)
    ) %>%
    filter(!is.na(latitude) | !is.na(longitude)) %>%
    unique() %>%
    as.data.table()
  
  cat("Loaded geography for", nrow(TN_EPA_geo), "Tennessee facilities from EPA\n")
  
} else {
  warning("EPA Facilities file not found. Geography will be limited.")
  TN_EPA_geo <- data.table(facility_id=character(), latitude=numeric(), longitude=numeric())
}

# 7. Merge LUST Data -------------------------------------------------------
cat("\nMerging LUST data and creating leak flags...\n")

# Ensure facility_id is character for merge
TN_UST_tanks[, facility_id := as.character(facility_id)]

# Merge with LUST data
TN_UST_tanks_LUST <- merge(TN_UST_tanks, TN_LUST_SD, by="facility_id", all.x=TRUE, allow.cartesian=TRUE)

# Create Leak Flags based on temporal overlap
TN_UST_tanks_LUST[, `:=`(
  leak_after_closure = as.integer(!is.na(best_closed_date) & !is.na(report_date) & report_date > best_closed_date),
  leak_before_NFA_before_closure = as.integer(!is.na(best_closed_date) & !is.na(report_date) & report_date < best_closed_date & (is.na(nfa_date) | nfa_date < best_closed_date)),
  leak_before_NFA_after_closure = as.integer(!is.na(best_closed_date) & !is.na(report_date) & report_date < best_closed_date & !is.na(nfa_date) & nfa_date > best_closed_date),
  no_leak = as.integer(is.na(report_date) & !is.na(best_closed_date))
)]

# 8. Aggregate to Facility-Tank Level --------------------------------------
cat("\nAggregating to facility-tank level...\n")

TN_UST_tanks_SD <- TN_UST_tanks_LUST[, .(
  facility_name = first(facility_name),
  tank_status = first(tank_status),
  
  # Leak flags
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
  
  # Attributes
  capacity = mean(capacity, na.rm=T),
  county_name = first(county_name)
), by = .(facility_id, tank_id, tank_installed_date, best_closed_date)]

# Rename for consistency
setnames(TN_UST_tanks_SD, "best_closed_date", "tank_closed_date")

# 9. Merge Geography from EPA ----------------------------------------------
cat("\nMerging geography from EPA National Database...\n")

TN_UST_tanks_SD <- merge(TN_UST_tanks_SD, TN_EPA_geo, by = "facility_id", all.x = TRUE)

geo_coverage <- sum(!is.na(TN_UST_tanks_SD$latitude)) / nrow(TN_UST_tanks_SD) * 100
cat("Geography coverage after EPA merge:", round(geo_coverage, 1), "%\n")

# 10. Standardize County Names and Merge FIPS -----------------------------
cat("\nStandardizing county names and merging FIPS codes...\n")

TN_UST_tanks_SD[, standardized_county := standardize_county_name(county_name)]

fips_path <- here("Data", "Processed", "county_fips_codes.csv")
if(file.exists(fips_path)) {
  county_fips <- fread(fips_path)
  county_fips[, standardized_county := standardize_county_name(county_name)]
  
  # Filter for Tennessee (State FIPS 47)
  tn_fips <- county_fips[state_fips == 47]
  
  TN_UST_tanks_SD <- merge(TN_UST_tanks_SD, 
                           tn_fips[, .(standardized_county, county_fips, county_name)], 
                           by = "standardized_county", 
                           all.x = TRUE,
                           suffixes = c("_raw", "_standard"))
  
  # Use standardized county name
  TN_UST_tanks_SD[, county_name := county_name_standard]
  TN_UST_tanks_SD[, `:=`(standardized_county = NULL, county_name_raw = NULL, county_name_standard = NULL)]
  
  fips_coverage <- sum(!is.na(TN_UST_tanks_SD$county_fips)) / nrow(TN_UST_tanks_SD) * 100
  cat("FIPS coverage:", round(fips_coverage, 1), "%\n")
}

# Add state
TN_UST_tanks_SD[, state := "TN"]

# 11. Final Column Cleanup -------------------------------------------------
cat("\nFinal column cleanup and ordering...\n")

required_columns <- c(
  "facility_id", "facility_name", "tank_id", "state", 
  "tank_installed_date", "tank_closed_date", "tank_status",
  "leak_after_closure", "leak_before_NFA_before_closure", 
  "leak_before_NFA_after_closure", "no_leak",
  "capacity", "single_walled", "double_walled", "unknown_walled",
  "is_gasoline", "is_diesel", "is_oil_kerosene", "is_jet_fuel", "is_other", 
  "county_name", "county_fips", "latitude", "longitude"
)

cols_to_keep <- intersect(names(TN_UST_tanks_SD), required_columns)
TN_UST_tanks_SD <- TN_UST_tanks_SD[, ..cols_to_keep]

# 12. Save Output ----------------------------------------------------------
cat("\nSaving harmonized files...\n")

output_dir <- here("Data", "Raw", "state_databases", "Tennessee")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

fwrite(TN_UST_tanks_SD, file.path(output_dir, "TN_Harmonized_UST_tanks.csv"))
fwrite(TN_LUST_SD, file.path(output_dir, "TN_Harmonized_LUST.csv"))

# 13. Summary Statistics ---------------------------------------------------
cat("\n" , rep("=", 80), "\n", sep="")
cat("TENNESSEE UST HARMONIZATION COMPLETE\n")
cat(rep("=", 80), "\n", sep="")

cat("\nFinal Dataset Summary:\n")
cat("  Total tank records:", nrow(TN_UST_tanks_SD), "\n")
cat("  Unique facilities:", length(unique(TN_UST_tanks_SD$facility_id)), "\n")
cat("  LUST records:", nrow(TN_LUST_SD), "\n")

cat("\nTank Status:\n")
print(TN_UST_tanks_SD[, .N, by = tank_status])

cat("\nWall Type Distribution:\n")
print(TN_UST_tanks_SD[, .(
  Single = sum(single_walled),
  Double = sum(double_walled),
  Unknown = sum(unknown_walled)
)])

cat("\nSubstance Distribution:\n")
print(TN_UST_tanks_SD[, .(
  Gasoline = sum(is_gasoline),
  Diesel = sum(is_diesel),
  Oil_Kerosene = sum(is_oil_kerosene),
  Jet_Fuel = sum(is_jet_fuel),
  Other = sum(is_other)
)])

cat("\nLeak Flags:\n")
print(TN_UST_tanks_SD[, .(
  leak_after_closure = sum(leak_after_closure == 1, na.rm=T),
  leak_before_NFA_before_closure = sum(leak_before_NFA_before_closure == 1, na.rm=T),
  leak_before_NFA_after_closure = sum(leak_before_NFA_after_closure == 1, na.rm=T),
  no_leak = sum(no_leak == 1, na.rm=T)
)])

cat("\nData Completeness:\n")
cat("  Install Date:", round(sum(!is.na(TN_UST_tanks_SD$tank_installed_date))/nrow(TN_UST_tanks_SD)*100, 1), "%\n")
cat("  Closure Date (closed tanks):", 
    round(sum(!is.na(TN_UST_tanks_SD[tank_status == "Closed"]$tank_closed_date))/
          nrow(TN_UST_tanks_SD[tank_status == "Closed"])*100, 1), "%\n")
cat("  County:", round(sum(!is.na(TN_UST_tanks_SD$county_name))/nrow(TN_UST_tanks_SD)*100, 1), "%\n")
cat("  Geography (Lat/Long):", round(sum(!is.na(TN_UST_tanks_SD$latitude))/nrow(TN_UST_tanks_SD)*100, 1), "%\n")

cat("\nFiles saved to:", output_dir, "\n")
cat("  - TN_Harmonized_UST_tanks.csv\n")
cat("  - TN_Harmonized_LUST.csv\n")

cat("\nDone.\n")