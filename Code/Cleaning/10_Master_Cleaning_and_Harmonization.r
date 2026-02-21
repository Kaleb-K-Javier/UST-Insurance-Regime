## Master Dataset Assembly & Geolocation Backfill
# Script: 04_Master_Build.R
# Description: 
#   1. Aggregates all harmonized state UST & LUST files.
#   2. Applies Winning ID Logic (AL, MI, OK, TX, PA, etc.) to merge EPA Lat/Long.
#   3. Backfills missing County data using Zip Codes.
#   4. Generates a Comprehensive Data Quality Report (excluding leak flags).
#
# Merge key reference (confirmed via diagnostics):
#   TX, CO, LA, ME, NJ, NM, TN, AR  : paste0(state, facility_id)
#   MI                               : paste0("MI", strip_leading_zeros(facility_id))
#   AL                               : paste0("AL", numeric_chars_only(facility_id))
#   OK                               : paste0("OK[", facility_id, "]")
#   PA                               : epa_key column written by 09_Clean_PA.R
#                                      (built from site_id first, efacts fallback)

# 0. Setup ----------------------------------------------------------------
library(data.table)
library(tidyverse)
library(here)
library(stringr)

processed_dir <- here("Data", "Processed")
if (!dir.exists(processed_dir)) dir.create(processed_dir, recursive = TRUE)

# 1. Ingest Harmonized State Data -----------------------------------------
message("\n--- Step 1: Ingesting Harmonized State Files ---")

tank_files <- list.files(path = here("Data", "Raw", "state_databases"), 
                         pattern = "_Harmonized_UST_tanks.csv$", 
                         recursive = TRUE, full.names = TRUE)

lust_files <- list.files(path = here("Data", "Raw", "state_databases"), 
                         pattern = "_Harmonized_LUST.csv$", 
                         recursive = TRUE, full.names = TRUE)

load_state_file <- function(fpath) {
  fread(fpath, colClasses = "character", na.strings = c("", "NA", "NULL"))
}

master_tanks <- rbindlist(lapply(tank_files, load_state_file), fill = TRUE, use.names = TRUE)
master_lust  <- rbindlist(lapply(lust_files, load_state_file), fill = TRUE, use.names = TRUE)

# Extended Type Casting
cols_numeric <- c("latitude", "longitude", "capacity", 
                  "single_walled", "double_walled", "unknown_walled",
                  "is_gasoline", "is_diesel", "is_oil_kerosene", "is_jet_fuel", "is_other")

for (col in cols_numeric) {
  if (!col %in% names(master_tanks)) master_tanks[, (col) := NA]
}

master_tanks[, (cols_numeric) := lapply(.SD, function(x) suppressWarnings(as.numeric(x))), .SDcols = cols_numeric]
master_tanks[, tank_installed_date := as.Date(tank_installed_date)]
master_tanks[, tank_closed_date    := as.Date(tank_closed_date)]

# Standardize Status
master_tanks[, status_std := fcase(
  grepl("Closed|Remov|Aband|Perm", tank_status, ignore.case = TRUE), "Closed",
  default = "Open"
)]

message(paste0("Total Tanks Loaded: ", format(nrow(master_tanks), big.mark = ",")))

# --- Step 1.5: Standardize Coordinates (Mercator -> WGS84) ---
message("\n--- Step 1.5: Standardizing Coordinate Systems ---")

if (requireNamespace("sf", quietly = TRUE)) {
  mercator_idx <- which(
    abs(master_tanks$latitude) > 90 &
    !is.na(master_tanks$latitude) &
    !is.na(master_tanks$longitude)
  )
  
  if (length(mercator_idx) > 0) {
    message(paste("Detected", length(mercator_idx), "rows with Projected Coordinates. Converting..."))
    sub_geo  <- master_tanks[mercator_idx, .(latitude, longitude)]
    sf_obj   <- sf::st_as_sf(sub_geo, coords = c("longitude", "latitude"), crs = 3857)
    sf_trans <- sf::st_transform(sf_obj, crs = 4326)
    coords_new <- sf::st_coordinates(sf_trans)
    master_tanks[mercator_idx, longitude := coords_new[, 1]]
    master_tanks[mercator_idx, latitude  := coords_new[, 2]]
    message("Coordinate conversion complete.")
  } else {
    message("No projected coordinates detected.")
  }
} else {
  warning("Package 'sf' missing. Skipping coordinate standardization.")
}


# 2. EPA Geolocation Merge ------------------------------------------------
message("\n--- Step 2: Applying EPA Geolocation Merge ---")

epa_path <- here("Data", "Raw", "Facilities.csv")

if (file.exists(epa_path)) {
  
  raw_headers <- names(fread(epa_path, nrows = 0))
  id_col <- grep("facility.*id", raw_headers, ignore.case = TRUE, value = TRUE)[1]
  st_col <- grep("^state$",      raw_headers, ignore.case = TRUE, value = TRUE)[1]
  
  epa_dt <- fread(epa_path, select = c(id_col, st_col, "Latitude", "Longitude"),
                  colClasses = "character")
  setnames(epa_dt, c("epa_id", "epa_state_full", "lat_epa", "long_epa"))
  
  # FIX: extended state map handles DC and EPA quirks like "NewYork" (no space)
  state_map <- rbind(
    data.table(epa_state_full = state.name, state_abbr = state.abb),
    data.table(
      epa_state_full = c("NewYork", "Washington DC", "District of Columbia"),
      state_abbr     = c("NY",      "DC",            "DC")
    )
  )
  epa_dt <- merge(epa_dt, state_map, by = "epa_state_full", all.x = TRUE)
  
  # --- Build Merge IDs ---
  master_tanks[, merge_id := NA_character_]
  
  # A. Standard prefix concatenation: state + facility_id
  group_prefix <- c("TX", "CO", "LA", "ME", "NJ", "NM", "TN", "AR")
  master_tanks[state %in% group_prefix, merge_id := paste0(state, trimws(facility_id))]
  
  # B. Michigan: strip leading zeros from numeric facility_id
  master_tanks[state == "MI", merge_id := paste0("MI", str_remove(trimws(facility_id), "^0+"))]
  
  # C. Alabama: purge all non-numeric characters from permit number
  master_tanks[state == "AL", merge_id := paste0("AL", gsub("[^0-9]", "", facility_id))]
  
  # D. Oklahoma: EPA wraps numeric ID in brackets — OK[0100003]
  master_tanks[state == "OK", merge_id := paste0("OK[", trimws(facility_id), "]")]
  
  # E. Pennsylvania: uses epa_key column written by 09_Clean_PA.R
  #    09_Clean_PA.R builds epa_key as paste0("PA", coalesce(site_id, efacts_facility_id))
  #    (site_id first — confirmed correct by manual crosswalk verification)
  if ("epa_key" %in% names(master_tanks)) {
    master_tanks[state == "PA" & !is.na(epa_key) & epa_key != "", merge_id := epa_key]
  } else {
    warning("epa_key column missing from PA data. Rerun 09_Clean_PA.R before this script.")
  }
  
  # --- Execute Merge ---
  epa_lookup <- unique(
    epa_dt[!is.na(lat_epa) & !is.na(long_epa),
           .(merge_id = epa_id,
             lat_epa  = as.numeric(lat_epa),
             long_epa = as.numeric(long_epa))],
    by = "merge_id"
  )
  
  master_tanks <- merge(master_tanks, epa_lookup, by = "merge_id", all.x = TRUE)
  
  # Coalesce: native coordinates take priority, EPA fills gaps
  master_tanks[, latitude_final  := fcoalesce(latitude,  lat_epa)]
  master_tanks[, longitude_final := fcoalesce(longitude, long_epa)]
  
  # --- EPA Merge Diagnostic ---
  message("\n--- EPA Merge Diagnostic (by state) ---")
  diag_states <- c("TX", "CO", "LA", "ME", "NJ", "NM", "TN", "AR", "PA", "MI", "AL", "OK")
  epa_diag <- master_tanks[state %in% diag_states, .(
    total           = .N,
    native_lat      = sum(!is.na(latitude)),
    epa_matched     = sum(!is.na(lat_epa)),
    final_lat       = sum(!is.na(latitude_final)),
    pct_covered     = round(sum(!is.na(latitude_final)) / .N * 100, 1),
    merge_id_sample = merge_id[1]
  ), by = state][order(state)]
  print(epa_diag)
  
  epa_id_sample <- epa_dt[state_abbr %in% diag_states,
                           .(epa_id_sample = epa_id[1]), by = state_abbr]
  message("\nEPA ID format sample (crosscheck against merge_id_sample above):")
  print(epa_id_sample[order(state_abbr)])
  
  master_tanks[, c("merge_id", "lat_epa", "long_epa") := NULL]
  
} else {
  master_tanks[, latitude_final  := latitude]
  master_tanks[, longitude_final := longitude]
}

# 3. County & FIPS Backfill (Cascading) -----------------------------------
message("\n--- Step 3: Comprehensive County & FIPS Backfill ---")

if (!"county_fips" %in% names(master_tanks)) master_tanks[, county_fips := NA_character_]
master_tanks[, county_fips := as.character(county_fips)]

clean_county_name <- function(x) {
  x <- toupper(trimws(x))
  str_remove(x, "\\s+(COUNTY|PARISH|BOROUGH|CENSUS AREA)$")
}

has_zip_ref <- file.exists(here("Data", "Raw", "uszips.csv"))
has_sf      <- requireNamespace("sf",     quietly = TRUE) &&
               requireNamespace("tigris", quietly = TRUE)

# Ref 1: Name → FIPS via tigris
if (has_sf) {
  data("fips_codes", package = "tigris", envir = environment())
  setDT(fips_codes)
  name_fips_map <- fips_codes[, .(
    state      = state,
    clean_name = clean_county_name(county),
    ref_fips   = paste0(state_code, county_code)
  )]
}

# Ref 2: Zip → county/FIPS
if (has_zip_ref) {
  dt_zip <- fread(here("Data", "Raw", "uszips.csv"), colClasses = "character") %>% clean_names()
  dt_zip[, zip5 := str_sub(trimws(zip), 1, 5)]
  fips_col <- if ("county_fips" %in% names(dt_zip)) "county_fips" else NA
  if (!is.na(fips_col)) {
    zip_map <- unique(dt_zip[, .(zip5, zip_county = county_name, zip_fips = get(fips_col))])
  } else {
    zip_map <- unique(dt_zip[, .(zip5, zip_county = county_name)])
    zip_map[, zip_fips := NA_character_]
  }
}

master_tanks[, temp_clean_name := clean_county_name(county_name)]

# Pass 1: County name → FIPS
if (has_sf) {
  message("Pass 1: Filling FIPS via County Name match...")
  master_tanks[is.na(county_fips) & !is.na(temp_clean_name),
               county_fips := name_fips_map[.SD, ref_fips, on = .(state, clean_name = temp_clean_name)]]
}

# Pass 2: Zip code → county/FIPS
if (has_zip_ref) {
  message("Pass 2: Filling Missing Data via Zip Code...")
  if ("zip"      %in% names(master_tanks)) master_tanks[, zip5 := str_sub(trimws(zip),      1, 5)]
  if ("zip_code" %in% names(master_tanks)) master_tanks[, zip5 := str_sub(trimws(zip_code), 1, 5)]
  master_tanks <- merge(master_tanks, zip_map, by = "zip5", all.x = TRUE)
  master_tanks[is.na(county_name) | county_name == "", county_name := zip_county]
  master_tanks[is.na(county_fips) | county_fips == "", county_fips := zip_fips]
  master_tanks[, c("zip_county", "zip_fips") := NULL]
}

# Pass 3: Spatial join for anything still missing
missing_spatial <- which(
  (is.na(master_tanks$county_fips) | master_tanks$county_fips == "") &
  !is.na(master_tanks$latitude_final) &
  !is.na(master_tanks$longitude_final)
)

if (length(missing_spatial) > 0 && has_sf) {
  message(paste("Pass 3: Spatial Fill for", length(missing_spatial), "remaining records..."))
  us_sf  <- tigris::counties(cb = TRUE, resolution = "20m", year = 2021, class = "sf")
  us_sf  <- sf::st_transform(us_sf, 4326)
  us_sf  <- us_sf[, c("GEOID", "NAME")]
  pts    <- sf::st_as_sf(master_tanks[missing_spatial],
                         coords = c("longitude_final", "latitude_final"), crs = 4326)
  joined <- sf::st_join(pts, us_sf, join = sf::st_within)
  master_tanks[missing_spatial, county_name := joined$NAME]
  master_tanks[missing_spatial, county_fips := joined$GEOID]
  message("Spatial fill complete.")
}

master_tanks[, c("temp_clean_name", "zip5") := NULL]

pct_fips <- round(sum(!is.na(master_tanks$county_fips)) / nrow(master_tanks) * 100, 1)
message(paste0("Final FIPS Coverage: ", pct_fips, "%"))


# 4. Comprehensive Final Report -------------------------------------------
message("\n--- Step 4: Final Data Quality Report ---")

# FIX: NJ was in both tier1_controls and treated_states — resolved here.
# NJ has a custom cleaning script so belongs in Tier 1.
tier1_controls  <- c("AL", "AR", "CO", "LA", "ME", "NJ", "NM", "OK", "PA", "TN")
tier2_controls  <- c("ID", "KS", "KY", "MD", "MA", "MN", "NC", "ND", "OH", "VA")
treated_states  <- c("AZ", "CT", "FL", "IA", "MI", "WI")   # NJ removed
excluded_states <- c("WA", "OR")
target_state    <- c("TX")

master_tanks[, study_group := fcase(
  state %in% target_state,    "1. Target (TX)",
  state %in% tier1_controls,  "2. Control Tier 1 (Custom)",
  state %in% tier2_controls,  "3. Control Tier 2 (EPA Std)",
  state %in% treated_states,  "4. Excluded (Treated)",
  state %in% excluded_states, "5. Excluded (Other)",
  default = "6. Remainder"
)]

# LUST date missingness
master_lust[, report_date := as.Date(report_date)]
lust_stats <- master_lust[, .(
  total_lusts            = .N,
  n_missing_report_date  = sum(is.na(report_date))
), by = state]

# FIX: calc_missing() scoping bug.
# The old version captured lat_col as a full-table vector OUTSIDE the by= grouping,
# so sum(is.na(lat_col)) counted NAs across the entire master table while .N was
# only the group size — producing impossible values like 366%.
# Fix: reference the coordinate column by NAME inside .SDcols so data.table
# correctly subsets it per group.
calc_missing <- function(dt) {
  lat_col_name <- if ("latitude_final" %in% names(dt)) "latitude_final" else "latitude"
  
  dt[, .(
    total_tanks = .N,
    
    pct_closed_missing_date = {
      n_closed <- sum(status_std == "Closed", na.rm = TRUE)
      if (n_closed > 0)
        round(sum(status_std == "Closed" & is.na(tank_closed_date)) / n_closed * 100, 1)
      else
        NA_real_
    },
    
    pct_missing_install_date = round(sum(is.na(tank_installed_date)) / .N * 100, 1),
    
    # Scoping fix: .SD[[lat_col_name]] subsets correctly within each by= group
    pct_miss_lat = round(sum(is.na(.SD[[lat_col_name]])) / .N * 100, 1),
    
    pct_miss_county = round(
      sum(is.na(county_name) | county_name == "") / .N * 100, 1),
    
    pct_miss_capacity = round(sum(is.na(capacity)) / .N * 100, 1),
    
    pct_miss_tank_status = round(
      sum(is.na(tank_status) | tank_status == "") / .N * 100, 1),
    
    pct_miss_tank_substance = round(sum(
      (is.na(is_gasoline)     | is_gasoline     == 0) &
      (is.na(is_diesel)       | is_diesel       == 0) &
      (is.na(is_oil_kerosene) | is_oil_kerosene == 0) &
      (is.na(is_jet_fuel)     | is_jet_fuel     == 0) &
      (is.na(is_other)        | is_other        == 0)
    ) / .N * 100, 1),
    
    pct_miss_tank_type = round(sum(
      is.na(single_walled) |
      unknown_walled == 1  |
      (single_walled == 0 & double_walled == 0)
    ) / .N * 100, 1),
    
    pct_miss_fips = round(
      sum(is.na(county_fips) | county_fips == "") / .N * 100, 1)
    
  ), by = .(study_group, state), .SDcols = lat_col_name]
}

report <- calc_missing(master_tanks)

report <- merge(report, lust_stats, by = "state", all.x = TRUE)
report[is.na(total_lusts),           total_lusts           := 0]
report[is.na(n_missing_report_date), n_missing_report_date := 0]

setorder(report, study_group, pct_miss_lat, -total_tanks)
print(report)

# 5. Save -----------------------------------------------------------------
master_tanks[, latitude  := latitude_final]
master_tanks[, longitude := longitude_final]
master_tanks[, c("latitude_final", "longitude_final", "status_std") := NULL]

fwrite(master_tanks, file.path(processed_dir, "Master_Harmonized_UST_Tanks.csv"))
fwrite(master_lust,  file.path(processed_dir, "Master_Harmonized_LUST.csv"))
fwrite(report,       file.path(processed_dir, "Master_Data_Quality_Report.csv"))

message("Done.")