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