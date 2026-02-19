## Pennsylvania UST Dataset Creation
# Script: 09_Clean_PA.R
# Author: Validated Classification Logic (Feb 2026)
# Description: Harmonizes PA UST/LUST data for econometrics.
#
# Key Features:
#   1. Data Fusion: Merges Active (Excel), Inactive (CSV), and GIS (Backups).
#   2. Linkage: Bridges EPA LUST data using `facility_linkage_table.csv` (via eFACTS ID).
#   3. Regulatory Mandate (Double): Implements Nov 10, 2007 cutoff (25 Pa. Code § 245.421).
#   4. Component Logic (Wall Type): Prioritizes "18Y" flag and explicit material codes.
#   5. Robustness: Parallel processing for large inactive file sets.

# 0. Setup ----------------------------------------------------------------
library(tidyverse)
library(data.table)
library(here)
library(lubridate)
library(janitor)
library(readxl)
library(stringr)
library(furrr)

# Define Paths
pa_base_path <- "C:/Users/kalebkja/PA_UST_Auction_Analysis/data/external/padep"
epa_path     <- here("Data", "Raw", "Releases.csv")
output_dir   <- here("Data", "Raw", "state_databases", "Pennsylvania")

if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# 1. Helper Functions -----------------------------------------------------

standardize_county_name <- function(county_name) {
  name <- tolower(county_name)
  name <- gsub(" county$| counties$", "", name)
  name <- gsub("[^a-z ]", "", name)
  name <- trimws(gsub("\\s+", " ", name))
  name[name == ""] <- NA
  return(name)
}

safe_parse_date <- function(date_vec) {
  parsed_dates <- rep(as.Date(NA), length(date_vec))
  
  suppressWarnings({
    numeric_indices <- !is.na(as.numeric(date_vec))
  })
  
  if (any(numeric_indices)) {
    parsed_dates[numeric_indices] <- janitor::excel_numeric_to_date(as.numeric(date_vec[numeric_indices]))
  }
  
  string_indices <- !numeric_indices & !is.na(date_vec)
  
  if (any(string_indices)) {
    parsed_strings <- lubridate::parse_date_time(
      date_vec[string_indices], 
      orders = c("ymd", "mdy", "dmy", "Y-m-d", "m/d/Y"),
      quiet = TRUE
    )
    parsed_dates[string_indices] <- as.Date(parsed_strings)
  }
  return(parsed_dates)
}

# 2. Core Logic: Classification -------------------------------------------

classify_tank_walls_pa <- function(data, comp_df) {
  result <- copy(data)
  
  # Join Wall Types derived from components
  result <- merge(result, comp_df, by = c("facility_id", "tank_id"), all.x = TRUE)
  
  # --- REGULATORY CONSTANTS ---
  PA_SECONDARY_MANDATE <- as.Date("2007-11-10")
  
  # --- HIERARCHICAL ASSIGNMENT ---
  result[, `:=`(
    comp_is_dw = as.integer(wall_type_comp == "DW"),
    
    # Priority 2: Regulatory Mandate
    regulatory_is_dw = as.integer(!is.na(tank_installed_date) & 
                                    tank_installed_date >= PA_SECONDARY_MANDATE & 
                                    (is.na(wall_type_comp) | wall_type_comp != "SW"))
  )]
  
  # Final Double Walled Logic
  result[, double_walled := as.integer(comp_is_dw == 1 | regulatory_is_dw == 1)]
  result[is.na(double_walled), double_walled := 0]
  
  # Single Walled Logic
  result[, single_walled := as.integer((wall_type_comp == "SW") | (double_walled == 0))]
  result[is.na(single_walled), single_walled := 0]
  
  # Unknown Logic
  result[is.na(tank_installed_date) & (is.na(wall_type_comp) | wall_type_comp == "UNKNOWN"), 
         `:=`(single_walled=0, double_walled=0, unknown_walled=1)]
  result[is.na(unknown_walled), unknown_walled := 0]
  
  result[, `:=`(wall_type_comp=NULL, comp_is_dw=NULL, regulatory_is_dw=NULL)]
  
  return(result)
}

classify_substances <- function(data) {
  result <- copy(data)
  result[, `:=`(is_gasoline=0, is_diesel=0, is_oil_kerosene=0, is_jet_fuel=0, is_other=1)]
  
  if("substance" %in% names(result)) {
    result[, sub_lower := tolower(substance)]
    
    gas_pat <- "gas|petrol|motor fuel|unleaded|premium|regular"
    dsl_pat <- "diesel|dsl|biodiesel"
    oil_pat <- "oil|kerosene|lube|waste"
    jet_pat <- "jet|aviation|avgas"
    
    result[grep(gas_pat, sub_lower), is_gasoline := 1]
    result[grep(dsl_pat, sub_lower), is_diesel := 1]
    result[grep(oil_pat, sub_lower), is_oil_kerosene := 1]
    result[grep(jet_pat, sub_lower), is_jet_fuel := 1]
    
    result[, is_other := as.integer(is_gasoline + is_diesel + is_oil_kerosene + is_jet_fuel == 0)]
    result[, sub_lower := NULL]
  }
  return(result)
}

# 3. Load Raw Data --------------------------------------------------------
cat("\nLoading Pennsylvania Data...\n")

# 3.1 Active Tanks (Excel)
active_files <- list.files(pa_base_path, pattern = "^tank_inventory_.*\\.xls$", full.names = TRUE)
if(length(active_files) == 0) stop("Active files missing")

active_dt <- map_df(active_files, function(f) {
  read_excel(f) %>% 
    clean_names() %>%
    select(
      facility_id = pf_other_id,          
      tank_id_raw = sequence_number,      
      tank_type = tank_code,
      tank_installed_date = date_installed, 
      capacity, 
      substance = substance_code,
      tank_status_code = status_code, 
      status_date = status_code_date_end,
      address = locad_pf_address_1, 
      city = locad_locad_pf_city, 
      zip_code = locad_pf_zip_code, 
      county_name = pf_county_name
    ) %>%
    mutate(
      source = "Active",
      # FORCE CHARACTER CONVERSION TO MATCH INACTIVE DATA
      tank_installed_date = as.character(tank_installed_date), 
      status_date = as.character(status_date)
    )
}) %>% as.data.table()

# 3.2 Inactive Tanks
inactive_path <- file.path(pa_base_path, "ssrs_inactive")
inactive_files <- list.files(inactive_path, pattern = "\\.csv$", full.names = TRUE)
if(length(inactive_files) == 0) warning("Inactive files missing")

plan(multisession, workers = max(1, parallel::detectCores() - 1))
inactive_dt <- future_map_dfr(inactive_files, function(f) {
  tryCatch({
    fread(f, colClasses = "character", na.strings = c("", "NA", "NULL"), showProgress = FALSE) %>%
      clean_names() %>%
      select(
        facility_id = other_id,         
        tank_id_raw = seq_number,       
        tank_type = tank_code,
        tank_installed_date = installed_date, 
        capacity, 
        substance = substance_code,
        tank_status_code = status_code, 
        status_date, 
        address = pf_address1,
        city = pf_city, 
        zip_code = pf_zip, 
        county_name = county
      ) %>%
      mutate(source = "Inactive")
  }, error = function(e) return(NULL))
}, .progress = TRUE) %>% as.data.table()
plan(sequential)

# 3.3 Combine
pa_tanks <- rbind(active_dt, inactive_dt, fill = TRUE)
pa_tanks <- pa_tanks[str_detect(toupper(tank_type), "UST")]

# 3.4 Clean Columns
pa_tanks[, `:=`(
  facility_id = as.character(facility_id),
  tank_installed_date = safe_parse_date(tank_installed_date),
  tank_closed_date = safe_parse_date(status_date), 
  capacity = as.numeric(capacity)
)]

# 3.5 Clean IDs
pa_tanks[, tank_id := case_when(
  source == "Active" ~ str_remove(tank_id_raw, "A$") %>% str_remove("^0+"),
  source == "Inactive" ~ str_extract(tank_id_raw, "\\d+$") %>% str_remove("^0+"),
  TRUE ~ tank_id_raw
)]

# 3.6 Status Logic
pa_tanks[, tank_status := case_when(
  tank_status_code %in% c("U", "T") ~ "Open",
  tank_status_code %in% c("C", "R", "A", "W") ~ "Closed",
  source == "Inactive" ~ "Closed",
  TRUE ~ "Closed"
)]
pa_tanks[tank_status == "Open", tank_closed_date := NA]

# 4. GIS & Components -----------------------------------------------------

# 4.1 GIS Data
cat("\nLoading GIS Coordinates...\n")
gis_files <- list.files(file.path(pa_base_path, "raw_csv_backups"), pattern = "raw_gis_.*\\.csv", full.names = TRUE)

if(length(gis_files) > 0) {
  gis_dt <- lapply(gis_files, function(f) {
    dt <- fread(f, colClasses = "character") %>% clean_names()
    if("attributes_facility_i" %in% names(dt)) {
      return(dt[, .(facility_id = attributes_facility_i, 
                    latitude = attributes_latitude, 
                    longitude = attributes_longitude)])
    } else { return(NULL) }
  }) %>% rbindlist()
  
  if(nrow(gis_dt) > 0) {
    gis_dt <- gis_dt[, `:=`(latitude = as.numeric(latitude), longitude = as.numeric(longitude))]
    gis_dt <- unique(gis_dt, by = "facility_id")
    pa_tanks <- merge(pa_tanks, gis_dt, by = "facility_id", all.x = TRUE)
  }
}
if(!"latitude" %in% names(pa_tanks)) pa_tanks[, `:=`(latitude = NA_real_, longitude = NA_real_)]

# 4.2 Component Attributes (Wall Type)
cat("\nProcessing Components for Wall Types...\n")
comp_file <- file.path(pa_base_path, "allattributes(in).csv")

if(file.exists(comp_file)) {
  # Fix: Read blindly then clean names
  comp_raw <- fread(comp_file, colClasses = "character") %>% clean_names()
  
  # Select columns now that names are cleaned/standardized
  comp_raw <- comp_raw[, .(fac_id, tank_name, attribute)]
  
  SECONDARY_CODES   <- c('1D', '1F', '1G', '1O', '1V')
  SINGLE_WALL_CODES <- c('1A', '1E', '1H', '1W', '1B', '1C', '1I', '1P', '1J')
  
  tank_walls <- comp_raw %>%
    mutate(
      facility_id = as.character(fac_id),
      tank_id = str_remove(tank_name, "^0+"),
      code = trimws(attribute)
    ) %>%
    group_by(facility_id, tank_id) %>%
    summarise(
      wall_type_comp = case_when(
        any(code == "18Y") ~ "DW",
        any(code %in% SECONDARY_CODES) ~ "DW",
        any(code %in% SINGLE_WALL_CODES) ~ "SW",
        TRUE ~ "UNKNOWN"
      ), .groups = "drop"
    ) %>% as.data.table()
  
  pa_tanks <- classify_tank_walls_pa(pa_tanks, tank_walls)
} else {
  warning("Component file missing. Wall types will rely on dates.")
  pa_tanks[, `:=`(double_walled=0, single_walled=0, unknown_walled=1)]
}

# 5. EPA LUST Linkage & Creation ------------------------------------------
cat("\nProcessing EPA LUST Data...\n")

linkage_file <- file.path(pa_base_path, "facility_linkage_table.csv")
pa_lust_clean <- data.table()

if(file.exists(linkage_file) & file.exists(epa_path)) {
  
  # 5.1 Linkage Table
  linkage <- fread(linkage_file, colClasses = "character") %>%
    clean_names() %>%
    select(facility_id, efacts_facility_id, site_id) %>%
    mutate(
      efacts_clean = na_if(trimws(efacts_facility_id), "NA"),
      site_clean   = na_if(trimws(site_id), "NA"),
      epa_numeric  = coalesce(efacts_clean, site_clean)
    ) %>%
    filter(!is.na(epa_numeric)) %>%
    select(facility_id, epa_numeric) %>%
    distinct()

  cat("Linkage table: ", nrow(linkage), "facilities with resolved EPA ID\n")

  # ── ATTACH EPA KEY TO TANKS ──────────────────────────────────────────────
  # Do this immediately after linkage is built so epa_key is always present
  # regardless of whether LUST processing below succeeds or fails.
  pa_tanks <- merge(pa_tanks,
                    linkage[, .(facility_id, epa_key = paste0("PA", epa_numeric))],
                    by = "facility_id", all.x = TRUE)
  cat("epa_key attached:", sum(!is.na(pa_tanks$epa_key)), "of", nrow(pa_tanks), "tanks matched\n")
  # ──────────────────
  
  # 5.2 Load Raw EPA
  epa_raw <- fread(epa_path, colClasses = "character") %>% 
    clean_names() %>%
    filter(state == "Pennsylvania")
  
  # 5.3 Clean & Standardize EPA Data
# 5.3 Clean & Standardize EPA Data
pa_lust_clean <- epa_raw %>%
  transmute(
    # Strip PA prefix to get raw numeric — matches our epa_numeric column
    epa_numeric     = str_remove(facility_id, "^PA") %>% trimws(),
    epa_facility_id = facility_id,
    report_date     = safe_parse_date(reported_date),
    nfa_date        = NA_Date_,
    lust_id         = lust_id,
    substance       = substance,
    status          = status,
    state           = "PA"
  ) %>%
  filter(!is.na(epa_numeric)) %>%
  # Join via epa_numeric -> resolves to PA DEP facility_id
  inner_join(linkage, by = "epa_numeric") %>%
  select(
    facility_id,
    lust_id,
    report_date,
    nfa_date,
    status,
    substance,
    state,
    epa_facility_id
  ) %>%
  as.data.table()

cat("PA LUST records matched:", nrow(pa_lust_clean), "\n")

  # 5.4 Merge into Tank Panel for Flags
  leak_agg <- pa_lust_clean[, .(
    first_leak_date = min(report_date, na.rm = TRUE),
    leak_count = .N
  ), by = facility_id]
  
  pa_tanks <- merge(pa_tanks, leak_agg, by = "facility_id", all.x = TRUE)
  
  pa_tanks[, `:=`(
    leak_after_closure = as.integer(!is.na(tank_closed_date) & !is.na(first_leak_date) & first_leak_date > tank_closed_date),
    no_leak = as.integer(is.na(first_leak_date))
  )]
  
} else {
  warning("EPA or Linkage file missing.")
  pa_tanks[, `:=`(leak_after_closure=0, no_leak=1)]
}

# 6. Final Cleanup & Save -------------------------------------------------
pa_tanks <- classify_substances(pa_tanks)
pa_tanks[, standardized_county := standardize_county_name(county_name)]
pa_tanks[, state := "PA"]

# Select Final Tank Columns
cols_to_keep <- c(
  "facility_id", "tank_id", "state", "epa_key",          # <-- add epa_key
  "tank_installed_date", "tank_closed_date", "tank_status",
  "leak_after_closure", "no_leak",
  "capacity", "single_walled", "double_walled", "unknown_walled",
  "is_gasoline", "is_diesel", "is_oil_kerosene", "is_jet_fuel", "is_other",
  "county_name", "latitude", "longitude"
)
cols_to_keep <- intersect(names(pa_tanks), cols_to_keep)
pa_final <- pa_tanks[, ..cols_to_keep]

cat("\nSaving Final Files...\n")
fwrite(pa_final, file.path(output_dir, "PA_Harmonized_UST_tanks.csv"))

if(nrow(pa_lust_clean) > 0) {
  fwrite(pa_lust_clean, file.path(output_dir, "PA_Harmonized_LUST.csv"))
  cat("Saved Harmonized LUST Data:", nrow(pa_lust_clean), "records.\n")
}

cat("Done. Final Tank Dimensions:", nrow(pa_final), "rows.\n")