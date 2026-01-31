## 10_Master_Cleaning_and_Harmonization.R
## Purpose: 
## 1. Run individual cleaning scripts (optional toggle).
## 2. Load "Custom" State datasets (AR, LA, ME, MI, NJ, NM, OK, TX, TN, AL).
## 3. Merge EPA Lat/Long data ONLY for states that lack it locally.
## 4. Load "EPA Standard" datasets for the remaining 40 states.
## 5. Combine everything into Master Panel datasets.

library(here)
library(data.table)
library(tidyverse)
library(fs) 

# 0. Configuration -------------------------------------------------------------


# Directories
raw_db_dir <- here("Data", "Raw", "state_databases")
processed_dir <- here("Data", "Processed")

if (!dir.exists(processed_dir)) dir.create(processed_dir, recursive = TRUE)

# 2. Master UST Tank Aggregation -----------------------------------------------

message("\n=== Building Master UST Tank Dataset ===")

# The "Custom 10" - States we have specialized scripts for
custom_states_abbr <- c("AR", "LA", "ME", "MI", "NJ",  "TX")
custom_tanks_list <- list()

# States that ALREADY have Lat/Long merged in their local cleaning script
# We skip the second EPA merge pass for these.
# ADDED: TN and AL here to prevent double-merging
states_with_local_gis <- c("NM", "LA", "ME", "NJ", "OK", "TN", "AL") 

for (abbr in custom_states_abbr) {
  # Robust File Finding: Look for EXACT suffix to avoid 'v2' or backup files
  tank_file <- dir_ls(raw_db_dir, recurse = TRUE, regexp = paste0(abbr, "_Harmonized_UST_tanks.csv$"))
  
  if (length(tank_file) >= 1) {
    # If duplicates found, take the most recent or first
    dt <- fread(tank_file[1], colClasses = c("facility_id" = "character", "tank_id" = "character"))
    
    # Initialize Lat/Long as numeric to prevent logical coercion errors
    if (!"latitude" %in% names(dt)) dt[, latitude := NA_real_]
    if (!"longitude" %in% names(dt)) dt[, longitude := NA_real_]
    
    # Ensure numeric types
    dt[, `:=`(latitude = as.numeric(latitude), longitude = as.numeric(longitude))]
    
    # Check if we need to merge EPA GIS data (External Merge)
    if (abbr %in% states_with_local_gis) {
      message(paste0("  Processed ", abbr, " (Used LOCAL/INTERNAL GIS merge)"))
      
    } else {
      # Merge EPA GIS for states that need it (e.g. AR, TX, MI)
      gis_file <- dir_ls(raw_db_dir, recurse = TRUE, regexp = paste0(abbr, "_Harmonized_latlong.csv$"))
      
      if (length(gis_file) == 1) {
        message(paste0("  Merging External EPA GIS data for: ", abbr))
        gis_dt <- fread(gis_file, colClasses = c("facility_id" = "character"))
        
        # --- ID Harmonization Fixes ---
        if (abbr == "MI") {
          gis_dt[, facility_id := stringr::str_pad(facility_id, width=8, pad="0")]
        }
        if (abbr == "OK") {
          gis_dt[, facility_id := gsub("^OK", "", facility_id)]
        }
        
        # Perform Update Join
        m <- match(dt$facility_id, gis_dt$facility_id)
        dt[!is.na(m) & is.na(latitude), latitude := gis_dt$latitude[m[!is.na(m)]]]
        dt[!is.na(m) & is.na(longitude), longitude := gis_dt$longitude[m[!is.na(m)]]]
        
        matches <- sum(!is.na(m))
        message(paste0("    -> Matched GIS for ", matches, " facilities"))
      } else {
        message(paste0("  No EPA GIS file found for ", abbr))
      }
    }
    
    custom_tanks_list[[abbr]] <- dt
  } else {
    warning(paste0("Missing tank file for ", abbr))
  }
}

# --- B. Process "Standard EPA" States ---
# Find all Harmonized tank files
all_tank_files <- dir_ls(raw_db_dir, recurse = TRUE, regexp = "_Harmonized_UST_tanks.csv$")
# Identify files we haven't processed yet (The non-custom 40)
processed_filenames <- paste0(custom_states_abbr, "_Harmonized_UST_tanks.csv")
epa_tank_files <- all_tank_files[!basename(all_tank_files) %in% processed_filenames]

message(paste0("  Found ", length(epa_tank_files), " additional EPA state files."))

epa_tanks_list <- lapply(epa_tank_files, function(f) {
  dt <- fread(f, colClasses = c("facility_id" = "character", "tank_id" = "character"))
  return(dt)
})

# --- C. Combine All ---
message("  Combining all datasets...")
# Use fill=TRUE to handle columns that might be missing in some states
Master_UST_Tanks <- rbindlist(c(custom_tanks_list, epa_tanks_list), use.names = TRUE, fill = TRUE)

# Final Type Enforcement (Strict)
Master_UST_Tanks[, `:=`(
  tank_installed_date = as.Date(tank_installed_date),
  tank_closed_date = as.Date(tank_closed_date),
  facility_id = as.character(facility_id),
  tank_id = as.character(tank_id),
  latitude = as.numeric(latitude),
  longitude = as.numeric(longitude),
  capacity = as.numeric(capacity)
)]

# Save
outfile_tanks <- file.path(processed_dir, "Master_Harmonized_UST_Tanks.csv")
fwrite(Master_UST_Tanks, outfile_tanks)
message(paste0("SAVED: ", outfile_tanks))
message(paste0("Total Rows: ", nrow(Master_UST_Tanks)))
message(paste0("Unique States: ", uniqueN(Master_UST_Tanks$state)))

# 4. Master LUST Aggregation ---------------------------------------------------

message("\n=== Building Master LUST Dataset ===")

all_lust_files <- dir_ls(raw_db_dir, recurse = TRUE, regexp = "_Harmonized_LUST.csv$")
message(paste0("  Found ", length(all_lust_files), " LUST files."))

lust_list <- lapply(all_lust_files, function(f) {
  dt <- fread(f, colClasses = c("facility_id" = "character"))
  if ("LUST_id" %in% names(dt)) dt[, LUST_id := as.character(LUST_id)]
  if ("report_date" %in% names(dt)) dt[, report_date := as.Date(report_date)]
  if ("nfa_date" %in% names(dt)) dt[, nfa_date := as.Date(nfa_date)]
  return(dt)
})

Master_LUST <- rbindlist(lust_list, use.names = TRUE, fill = TRUE)

Master_LUST[, `:=`(
  report_date = as.Date(report_date),
  nfa_date = as.Date(nfa_date),
  facility_id = as.character(facility_id)
)]

outfile_lust <- file.path(processed_dir, "Master_Harmonized_LUST.csv")
fwrite(Master_LUST, outfile_lust)
message(paste0("SAVED: ", outfile_lust))
message(paste0("Total Rows: ", nrow(Master_LUST)))

message("\n=== Master Harmonization Complete ===")