## 10_Master_Cleaning_and_Harmonization.R
## Purpose: 
## 1. (Optional) Run all individual state cleaning scripts (01-09).
## 2. Load "Custom 8" State datasets (AR, LA, ME, MI, NJ, NM, OK, TX).
## 3. Merge EPA Lat/Long data into Custom 8 datasets (using files from Script 09).
## 4. Load "EPA Standard" State datasets.
## 5. Combine everything into Master Panel datasets.

library(here)
library(data.table)
library(tidyverse)
library(fs) # Excellent for file system handling

# 0. Configuration -------------------------------------------------------------

# Set to TRUE to re-run all cleaning scripts. Set to FALSE to just aggregate existing outputs.
RUN_CLEANING_SCRIPTS <- TRUE 

# Directories
raw_db_dir <- here("Data", "Raw", "state_databases")
processed_dir <- here("Data", "Processed")

if (!dir.exists(processed_dir)) dir.create(processed_dir, recursive = TRUE)

# 1. Run Individual Cleaning Scripts -------------------------------------------

if (RUN_CLEANING_SCRIPTS) {
  message("Starting Batch Execution of Cleaning Scripts...")
  
  scripts <- c(
    "01_Clean_AR.R", "02_Clean_LA.R", "03_Clean_ME.R", "04_Clean_MI.R",
    "05_Clean_NJ.R", "06_Clean_NM.R", "07_Clean_OK.R", #"08_Clean_TX.R",
    "09_Clean_EPA_States.R"
  )
  
  for (script in scripts) {
    script_path <- here("Code", "Cleaning", script) # Adjust path if scripts are in a subdir
    # Fallback if scripts are in root
    if (!file.exists(script_path)) script_path <- here(script)
    
    if (file.exists(script_path)) {
      message(paste0("Running: ", script))
      tryCatch({
        source(script_path, local = TRUE)
      }, error = function(e) {
        message(paste0("Error running ", script, ": ", e$message))
      })
    } else {
      warning(paste0("Script not found: ", script))
    }
  }
  message("Batch Execution Complete.")
}

# 2. Define State Categories ---------------------------------------------------

custom_states_abbr <- c("AR", "LA", "ME", "MI", "NJ", "NM", "OK", "TX")

# 3. Master UST Tank Aggregation -----------------------------------------------

message("\n=== Building Master UST Tank Dataset ===")
# --- A. Process "Custom 8" States (Add Lat/Long Placeholders) ---
custom_tanks_list <- list()

for (abbr in custom_states_abbr) {
  # Find the tank file
  tank_file <- dir_ls(raw_db_dir, recurse = TRUE, glob = paste0("*", abbr, "_Harmonized_UST_tanks.csv"))
  
  if (length(tank_file) == 1) {
    dt <- fread(tank_file, colClasses = c("facility_id" = "character", "tank_id" = "character"))
    
    # Initialize Lat/Long as NA 
    # NOTE: GIS merge disabled pending ID harmonization
    if (!"latitude" %in% names(dt)) dt[, latitude := NA_real_]
    if (!"longitude" %in% names(dt)) dt[, longitude := NA_real_]
    
    message(paste0("  Processed ", abbr, " (Lat/Long set to NA pending ID match fix)"))
    
    custom_tanks_list[[abbr]] <- dt
  } else {
    warning(paste0("Missing or multiple tank files for ", abbr))
  }
}


# --- B. Process "Standard EPA" States ---
# Find all files ending in _Harmonized_UST_tanks.csv
all_tank_files <- dir_ls(raw_db_dir, recurse = TRUE, glob = "*_Harmonized_UST_tanks.csv")

# Filter out the files we just processed in the Custom loop
# Extract filenames to compare
processed_files <- paste0(custom_states_abbr, "_Harmonized_UST_tanks.csv")
epa_tank_files <- all_tank_files[!basename(all_tank_files) %in% processed_files]

message(paste0("  Found ", length(epa_tank_files), " additional EPA state files."))

epa_tanks_list <- lapply(epa_tank_files, function(f) {
  fread(f, colClasses = c("facility_id" = "character", "tank_id" = "character"))
})

# --- C. Combine All ---
message("  Combining all datasets...")
Master_UST_Tanks <- rbindlist(c(custom_tanks_list, epa_tanks_list), use.names = TRUE, fill = TRUE)

# Final Type Enforcement
Master_UST_Tanks[, `:=`(
  tank_installed_date = as.Date(tank_installed_date),
  tank_closed_date = as.Date(tank_closed_date),
  facility_id = as.character(facility_id),
  tank_id = as.character(tank_id)
)]

# Save
outfile_tanks <- file.path(processed_dir, "Master_Harmonized_UST_Tanks.csv")
fwrite(Master_UST_Tanks, outfile_tanks)
message(paste0("SAVED: ", outfile_tanks))
message(paste0("Total Rows: ", nrow(Master_UST_Tanks)))
message(paste0("Unique States: ", uniqueN(Master_UST_Tanks$state)))

# 4. Master LUST Aggregation ---------------------------------------------------

message("\n=== Building Master LUST Dataset ===")

# Find ALL Harmonized LUST files (Custom + EPA)
all_lust_files <- dir_ls(raw_db_dir, recurse = TRUE, glob = "*_Harmonized_LUST.csv")

message(paste0("  Found ", length(all_lust_files), " LUST files to combine."))

lust_list <- lapply(all_lust_files, function(f) {
  dt <- fread(f, colClasses = c("facility_id" = "character", "LUST_id" = "character"))
  # Ensure dates are Date type immediately
  if ("report_date" %in% names(dt)) dt[, report_date := as.Date(report_date)]
  if ("nfa_date" %in% names(dt)) dt[, nfa_date := as.Date(nfa_date)]
  return(dt)
})

Master_LUST <- rbindlist(lust_list, use.names = TRUE, fill = TRUE)

# Final Type Enforcement
Master_LUST[, `:=`(
  report_date = as.Date(report_date),
  nfa_date = as.Date(nfa_date),
  facility_id = as.character(facility_id),
  LUST_id = as.character(LUST_id)
)]

# Save
outfile_lust <- file.path(processed_dir, "Master_Harmonized_LUST.csv")
fwrite(Master_LUST, outfile_lust)
message(paste0("SAVED: ", outfile_lust))
message(paste0("Total Rows: ", nrow(Master_LUST)))

message("\n=== Master Harmonization Complete ===")


# ==============================================================================
# DIAGNOSTIC SECTION: ID INSPECTION
# ==============================================================================
message("\n=======================================================")
message("STARTING MANUAL ID INSPECTION FOR CUSTOM STATES")
message("=======================================================")

# 1. Load the RAW EPA Facilities data once for ground-truth comparison
# We only read State and Facility_ID to keep it fast
message("Loading RAW EPA Facilities.csv for reference...")
raw_epa_ref <- fread(here("Data", "Raw", "Facilities.csv"), 
                     select = c("State", "Facility_ID"),
                     colClasses = "character")
setnames(raw_epa_ref, "State", "state")

# 2. Define the Custom 8 States
custom_states_abbr <- c("AR", "LA", "ME", "MI", "NJ", "NM", "OK", "TX")

# 3. Loop and Compare
for (st in custom_states_abbr) {
  cat(paste0("\n-------------------------------------------------------\n"))
  cat(paste0("STATE: ", st, "\n"))
  cat(paste0("-------------------------------------------------------\n"))
  
  # A. Load Local Harmonized Data (Source of Truth for your Panel)
  local_path <- dir_ls(raw_db_dir, recurse = TRUE, glob = paste0("*", st, "_Harmonized_UST_tanks.csv"))
  
  if(length(local_path) == 0) {
    cat("  [!] Missing Local Harmonized File\n")
    next
  }
  
  dt_local <- fread(local_path[1], select = "facility_id", colClasses = "character")
  ids_local <- unique(dt_local$facility_id)
  
  # B. Load Processed EPA GIS Data (Output from Script 09)
  # This represents what Script 09 *tried* to clean the IDs into
  gis_path <- dir_ls(raw_db_dir, recurse = TRUE, glob = paste0("*", st, "_Harmonized_latlong.csv"))
  
  if(length(gis_path) == 0) {
    cat("  [!] Missing Processed EPA GIS File (Script 09 Output)\n")
    ids_epa_processed <- character(0)
  } else {
    dt_gis <- fread(gis_path[1], select = "facility_id", colClasses = "character")
    ids_epa_processed <- unique(dt_gis$facility_id)
  }
  
  # C. Extract Raw EPA IDs for this state
  # This shows you what the ID looked like BEFORE Script 09 touched it
  # Note: EPA uses 2-letter codes mostly, but sometimes full names. We try matching strict abbr first.
  ids_epa_raw <- unique(raw_epa_ref[state == st]$Facility_ID)
  
  # D. Calculate Match Rate (Local vs Processed EPA)
  common <- intersect(ids_local, ids_epa_processed)
  match_rate <- round(length(common) / length(ids_local) * 100, 1)
  
  cat(paste0("  Local Facilities:     ", length(ids_local), "\n"))
  cat(paste0("  EPA (Processed):      ", length(ids_epa_processed), "\n"))
  cat(paste0("  MATCH RATE:           ", match_rate, "% (", length(common), " matches)\n"))
  
  # E. VISUAL COMPARISON TABLE
  # Create a side-by-side view of the first 15 IDs to spot patterns
  n_view <- 15
  
  # Pad vectors to same length for dataframe creation
  max_len <- max(length(ids_local), length(ids_epa_processed), length(ids_epa_raw))
  # Take head only
  vec_local <- c(head(ids_local, n_view), rep("", n_view - length(head(ids_local, n_view))))
  vec_proc  <- c(head(ids_epa_processed, n_view), rep("", n_view - length(head(ids_epa_processed, n_view))))
  vec_raw   <- c(head(ids_epa_raw, n_view), rep("", n_view - length(head(ids_epa_raw, n_view))))
  
  comparison_df <- data.table(
    `Local (Your Script)` = vec_local,
    `EPA Raw (Original)` = vec_raw,
    `EPA Processed (Script 09)` = vec_proc
  )
  
  print(comparison_df)
  
  # F. Show specific Mismatches if rate is low
  if (match_rate < 90) {
    cat("\n  [!] HIGH MISMATCH DETECTED. Showing Unmatched Local IDs vs Raw EPA IDs:\n")
    unmatched_local <- setdiff(ids_local, ids_epa_processed)
    
    # Attempt to find similar looking IDs in raw to give a hint
    # e.g. Local "123" vs Raw "00123"
    cat("  Top 5 Unmatched Local IDs:\n")
    print(head(unmatched_local, 5))
    
    cat("  ...checking character lengths...\n")
    cat(paste0("  Mean Length Local: ", round(mean(nchar(ids_local)), 1), "\n"))
    cat(paste0("  Mean Length Raw EPA: ", round(mean(nchar(ids_epa_raw)), 1), "\n"))
  }
}

message("\n=======================================================")
message("INSPECTION COMPLETE")
message("Use these patterns to update the ID cleaning logic in Script 09.")
message("=======================================================")