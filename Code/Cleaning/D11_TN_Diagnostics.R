# ==============================================================================
# 10_Diagnostic_TN_Full_Auditor.R
# Purpose: Comprehensive Audit of Tennessee UST Data.
# Scope: 
#   1. State Tank Data (Structure, Dates, Walls).
#   2. State LUST Data (New 'Discoverydate' field).
#   3. EPA National Data (Merge Logic Verification).
# Output: 'TN_Full_Audit_Report.txt'
# ==============================================================================

library(data.table)
library(tidyverse)
library(here)
library(janitor)
library(readxl)
library(lubridate)
library(stringr)

# 1. Setup ---------------------------------------------------------------------
tn_path  <- here("Data", "Raw", "state_databases", "Tennessee")
epa_file <- here("Data", "Raw", "Releases.csv") 
log_file <- file.path(tn_path, "TN_Full_Audit_Report.txt")

# Define files
files <- list(
  Compartments = "ust_all-tn-compartments.xlsx",
  EnvSites     = "ust_all-tn-environmental-sites.xlsx" 
)

# Start Logging
sink(log_file)
cat("================================================================================\n")
cat("TENNESSEE FULL DATA AUDIT (TANKS + LUST + EPA MERGE)\n")
cat("Generated: ", as.character(Sys.time()), "\n")
cat("================================================================================\n\n")

# 2. Load State Data -----------------------------------------------------------
cat("\n--- 1. LOADING STATE FILES ---\n")

data_list <- list()
for (name in names(files)) {
  fpath <- file.path(tn_path, files[[name]])
  if (file.exists(fpath)) {
    cat(paste0("Loading ", name, "... "))
    # Read as text to preserve IDs
    data_list[[name]] <- read_excel(fpath, col_types = "text") %>% clean_names() %>% as.data.table()
    cat("Success.\n")
  } else {
    cat("MISSING: ", files[[name]], "\n")
  }
}

# 3. EPA Merge Logic Test (The "Strip 'TN'" Check) -----------------------------
cat("\n\n================================================================================\n")
cat("PART 2: EPA MERGE LOGIC VERIFICATION\n")
cat("================================================================================\n")

if (!is.null(data_list$Compartments) && file.exists(epa_file)) {
  
  cat("Loading EPA Releases.csv (Minimal columns)...\n")
  epa_lust <- fread(epa_file, select = c("Facility_ID", "State"), colClasses = "character") %>% clean_names()
  
  # Filter EPA for TN
  state_col <- grep("state", names(epa_lust), value = TRUE)[1]
  epa_tn <- epa_lust[grepl("^TN$", get(state_col), ignore.case=TRUE)]
  
  # Prepare Lists
  tn_ids_all  <- unique(trimws(data_list$Compartments$facility_id_ust))
  tn_ids_all  <- tn_ids_all[!is.na(tn_ids_all) & tn_ids_all != ""]
  
  epa_ids_all <- unique(trimws(epa_tn$facility_id))
  epa_ids_all <- epa_ids_all[!is.na(epa_ids_all) & epa_ids_all != ""]
  
  # Sampling for Visual Check
  set.seed(42)
  tn_sample  <- sample(tn_ids_all, min(10, length(tn_ids_all)))
  epa_sample <- sample(epa_ids_all, min(10, length(epa_ids_all)))
  
  cat("\n>>> STRATEGY CHECK: STRIP 'TN' PREFIX\n")
  
  # Function to mimic the proposed cleaning logic
  clean_epa_id <- function(x) { gsub("^TN[- ]?", "", x, ignore.case=TRUE) }
  
  # Check TN Samples against Cleaned EPA
  cat("\nTest A: Checking 10 Random TN IDs against EPA (Cleaned)...\n")
  cat(sprintf("%-15s | %-10s\n", "TN ID", "Found in EPA?"))
  cat("-------------------------------\n")
  
  epa_lookup <- clean_epa_id(epa_ids_all)
  
  for(id in tn_sample) {
    found <- id %in% epa_lookup
    cat(sprintf("%-15s | %-10s\n", id, found))
  }
  
  # Check EPA Samples against TN
  cat("\nTest B: Checking 10 Random EPA IDs against TN...\n")
  cat(sprintf("%-15s | %-15s | %-10s\n", "Raw EPA ID", "Cleaned ID", "Found in TN?"))
  cat("-----------------------------------------------\n")
  
  for(id in epa_sample) {
    cleaned <- clean_epa_id(id)
    found <- cleaned %in% tn_ids_all
    cat(sprintf("%-15s | %-15s | %-10s\n", id, cleaned, found))
  }
  
  # Global Match Rate
  common <- intersect(tn_ids_all, epa_lookup)
  cat(paste0("\nGlobal Match Rate (Strip TN): ", length(common), " / ", length(tn_ids_all), 
             " (", round(length(common)/length(tn_ids_all)*100, 1), "%)\n"))
  
} else {
  cat("Skipping EPA Check (Files missing).\n")
}

# 4. New LUST File Audit -------------------------------------------------------
if (!is.null(data_list$EnvSites)) {
  dt <- data_list$EnvSites
  cat("\n\n================================================================================\n")
  cat("PART 3: NEW STATE LUST DATA AUDIT\n")
  cat("================================================================================\n")
  
  # Discovery Date
  cat("\n--- Discovery Date Analysis ---\n")
  if ("discoverydate" %in% names(dt)) {
    raw_dates <- dt$discoverydate[!is.na(dt$discoverydate)]
    cat("Sample Raw Dates: ", paste(head(raw_dates, 5), collapse=", "), "\n")
    
    # Test Parsing
    parsed <- parse_date_time(head(raw_dates, 10), orders = c("b-d-Y", "b-d-y"))
    cat("Parsing Test (MMM-DD-YYYY):\n")
    print(data.table(Raw=head(raw_dates, 5), Parsed=head(parsed, 5)))
  } else {
    cat("[!] 'discoverydate' column missing.\n")
  }
  
  # Status (Checking for NFA clues since we aren't using EPA NFA)
  cat("\n--- LUST Status Frequency ---\n")
  if ("currentstatus" %in% names(dt)) {
    print(dt[, .N, by = currentstatus][order(-N)])
  }
}

# 5. Tank Attributes Audit -----------------------------------------------------
if (!is.null(data_list$Compartments)) {
  dt <- data_list$Compartments
  cat("\n\n================================================================================\n")
  cat("PART 4: TANK ATTRIBUTES AUDIT\n")
  cat("================================================================================\n")
  
  cat("Total Tanks: ", nrow(dt), "\n")
  
  # Wall Type
  cat("\n--- Wall Type Indicators ---\n")
  cat("Category of Construction:\n")
  print(dt[, .N, by = category_of_construction][order(-N)])
  
  cat("\nTank Material:\n")
  print(dt[, .N, by = tank_construction][order(-N)])
  
  # Dates (Excel Serial Check)
  cat("\n--- Tank Install Date (Excel Serial Check) ---\n")
  raw_inst <- head(dt$date_tank_installed[!is.na(dt$date_tank_installed)], 10)
  cat("Sample Raw: ", paste(raw_inst, collapse=", "), "\n")
}

sink()
message("Diagnostic complete. Log saved to: ", log_file)