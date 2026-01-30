# ==============================================================================
# 10_Diagnostic_TN_Full_Combined_v3.R
# Purpose: Complete audit of TN UST data + EPA Merge Test + ZIP CROSSWALK TEST.
#   1. Structure: Glimpses/Heads of all raw files.
#   2. EPA Merge Test: ID Sampling (Strip 'TN').
#   3. ZIP Merge Test: Zip Sampling (First 5 Digits) vs uszips.csv.
#   4. Tank Data Audit: Wall types, dates, capacity, substance.
# Output: 'TN_Full_Diagnostic_Report.txt'
# ==============================================================================

library(data.table)
library(tidyverse)
library(here)
library(janitor)
library(readxl)
library(stringr)
library(lubridate)
library(knitr)

# 1. Setup & Paths -------------------------------------------------------------
tn_path  <- here("Data", "Raw", "state_databases", "Tennessee")
epa_file <- here("Data", "Raw", "Releases.csv") 
zip_file <- here("Data", "Raw", "uszips.csv")   # New Zip File Path
log_file <- file.path(tn_path, "TN_Full_Diagnostic_Report.txt")

# Define files map
files <- list(
  Facilities   = "ust_all-tn-facilities.xlsx",
  Compartments = "ust_all-tn-compartments.xlsx",
  EnvSites     = "ust_all-tn-environmental-sites.xlsx",
  ReimbSites   = "ust_all-tn-reimbursement-sites.xlsx"
)

# Start Logging
sink(log_file)
cat("================================================================================\n")
cat("TENNESSEE FULL DATA DIAGNOSTIC REPORT (V3 - WITH ZIP PROBE)\n")
cat("Generated: ", as.character(Sys.time()), "\n")
cat("================================================================================\n\n")

# 2. Load Data -----------------------------------------------------------------
cat("\n--- 1. LOADING STATE FILES ---\n")

data_list <- list()

for (name in names(files)) {
  fpath <- file.path(tn_path, files[[name]])
  if (file.exists(fpath)) {
    cat(paste0("Loading ", name, "... "))
    tryCatch({
      # Read as text to capture raw formats
      data_list[[name]] <- read_excel(fpath, col_types = "text") %>% 
        clean_names() %>% 
        as.data.table()
      cat("Success. Rows:", nrow(data_list[[name]]), "\n")
    }, error = function(e) {
      cat("Failed: ", e$message, "\n")
    })
  } else {
    cat("MISSING: ", files[[name]], "\n")
  }
}

# 3. Structure Inspection (Glimpse/Head) ---------------------------------------
cat("\n\n================================================================================\n")
cat("PART 2: DATA STRUCTURE INSPECTION\n")
cat("================================================================================\n")

for (name in names(data_list)) {
  dt <- data_list[[name]]
  cat(paste0("\n--- DATASET: ", name, " ---\n"))
  cat("Dimensions:", nrow(dt), "rows x", ncol(dt), "columns\n")
  cat("\nHead (First 5 Rows):\n")
  print(head(dt, 5))
  cat("\nColumn Names:\n")
  print(names(dt))
  cat("\n")
}

# 4. EPA Merge Logic Test (Enriched) -------------------------------------------
cat("\n\n================================================================================\n")
cat("PART 3: EPA MERGE LOGIC SAMPLER (TN vs EPA)\n")
cat("================================================================================\n")

if (!is.null(data_list$Compartments) && file.exists(epa_file)) {
  
  cat("Loading EPA Releases.csv (Minimal columns)...\n")
  epa_lust <- fread(epa_file, select = c("Facility_ID", "State"), colClasses = "character") %>% 
    clean_names()
  
  # Filter EPA for TN
  state_col <- grep("state", names(epa_lust), value = TRUE)[1]
  epa_tn <- epa_lust[grepl("^TN$", get(state_col), ignore.case=TRUE) | grepl("Tennessee", get(state_col), ignore.case=TRUE)]
  
  # 1. Get Pool of IDs
  tn_pool  <- unique(trimws(as.character(data_list$Compartments$facility_id_ust)))
  tn_pool  <- tn_pool[!is.na(tn_pool) & tn_pool != ""]
  
  epa_pool <- unique(trimws(as.character(epa_tn$facility_id)))
  epa_pool <- epa_pool[!is.na(epa_pool) & epa_pool != ""]
  
  cat(paste0("TN State Pool Size: ", length(tn_pool), "\n"))
  cat(paste0("EPA TN Pool Size:   ", length(epa_pool), "\n"))
  
  # 2. Select Samples
  set.seed(123)
  tn_sample  <- sample(tn_pool, min(10, length(tn_pool)))
  epa_sample <- sample(epa_pool, min(10, length(epa_pool)))
  
  # 3. Define Cleaning Logic
  clean_id_logic <- function(x) {
    x <- trimws(x)
    x <- gsub("^TN[- ]?", "", x, ignore.case = TRUE)
    return(x)
  }
  
  # 4. Raw vs Cleaned Output
  cat("\n--- RAW ID SAMPLER ---\n")
  cat("\n>>> A. TN STATE IDs (10 Samples):\n")
  for(id in tn_sample) {
    cat(sprintf("   Raw: '%-12s' -> Cleaned: '%-12s'\n", id, clean_id_logic(id)))
  }
  
  cat("\n>>> B. EPA NATIONAL IDs (10 Samples):\n")
  for(id in epa_sample) {
    cat(sprintf("   Raw: '%-12s' -> Cleaned: '%-12s'\n", id, clean_id_logic(id)))
  }
  
  # 5. Cross-Match Test
  cat("\n--- CROSS-MATCH TEST ---\n")
  epa_lookup <- clean_id_logic(epa_pool)
  tn_lookup  <- clean_id_logic(tn_pool)
  
  cat("\nChecking 10 TN Samples against EPA (Cleaned):\n")
  for(id in tn_sample) {
    cleaned <- clean_id_logic(id)
    found <- cleaned %in% epa_lookup
    cat(sprintf("   TN ID '%s' (%s) found in EPA? %s\n", id, cleaned, found))
  }
  
  cat("\nChecking 10 EPA Samples against TN (Cleaned):\n")
  for(id in epa_sample) {
    cleaned <- clean_id_logic(id)
    found <- cleaned %in% tn_lookup
    cat(sprintf("   EPA ID '%s' (%s) found in TN?  %s\n", id, cleaned, found))
  }
  
} else {
  cat("[!] Skipping EPA Test. Files missing.\n")
}

# 5. ZIP Code Merge Logic Test (NEW SECTION) -----------------------------------
cat("\n\n================================================================================\n")
cat("PART 3.5: ZIP CODE MERGE SAMPLER (TN vs USZIPS.CSV)\n")
cat("================================================================================\n")

if (!is.null(data_list$Compartments) && file.exists(zip_file)) {
  
  cat("Loading uszips.csv...\n")
  # Read as character to preserve leading zeros in Zips (e.g. '01234')
  dt_uszips <- fread(zip_file, select = c("zip", "county_name"), colClasses = "character") %>% 
    clean_names()
  
  # 1. Get Pool of Zips
  # From TN Facility Data
  tn_zip_pool <- unique(trimws(as.character(data_list$Compartments$facility_zip)))
  tn_zip_pool <- tn_zip_pool[!is.na(tn_zip_pool) & tn_zip_pool != ""]
  
  # From Crosswalk
  cw_zip_pool <- unique(trimws(as.character(dt_uszips$zip)))
  cw_zip_pool <- cw_zip_pool[!is.na(cw_zip_pool) & cw_zip_pool != ""]
  
  cat(paste0("TN State Unique Zips: ", length(tn_zip_pool), "\n"))
  cat(paste0("USZips Crosswalk Zips: ", length(cw_zip_pool), "\n"))
  
  # 2. Select Samples
  set.seed(999) 
  tn_zip_sample <- sample(tn_zip_pool, min(10, length(tn_zip_pool)))
  # Sample explicitly from TN range zips if possible, or random
  cw_zip_sample <- sample(cw_zip_pool, min(10, length(cw_zip_pool)))
  
  # 3. Define Cleaning Logic (First 5 Digits)
  clean_zip_logic <- function(x) {
    x <- trimws(as.character(x))
    # Extract first 5 digits only
    x <- str_sub(x, 1, 5)
    return(x)
  }
  
  # 4. Raw vs Cleaned Output
  cat("\n--- RAW ZIP SAMPLER ---\n")
  
  cat("\n>>> A. TN STATE ZIPS (10 Samples):\n")
  for(z in tn_zip_sample) {
    cat(sprintf("   Raw: '%-10s' -> Cleaned: '%-5s'\n", z, clean_zip_logic(z)))
  }
  
  cat("\n>>> B. CROSSWALK ZIPS (10 Samples):\n")
  for(z in cw_zip_sample) {
    cat(sprintf("   Raw: '%-10s' -> Cleaned: '%-5s'\n", z, clean_zip_logic(z)))
  }
  
  # 5. Match Test
  cat("\n--- ZIP MATCH TEST ---\n")
  
  cw_lookup <- clean_zip_logic(cw_zip_pool)
  
  cat("\nChecking 10 TN Zip Samples against Crosswalk:\n")
  matches <- 0
  for(z in tn_zip_sample) {
    cleaned <- clean_zip_logic(z)
    found <- cleaned %in% cw_lookup
    if(found) matches <- matches + 1
    
    # If found, print what county it maps to (sanity check)
    county_match <- ""
    if(found) {
      county_match <- paste0(" -> ", head(dt_uszips[clean_zip_logic(zip) == cleaned]$county_name, 1))
    }
    
    cat(sprintf("   TN Zip '%s' (%s) found? %s%s\n", z, cleaned, found, county_match))
  }
  
  cat(paste0("\nMatch Success Rate in Sample: ", matches, "/10\n"))
  
} else {
  cat("[!] Skipping Zip Test. 'uszips.csv' or TN Data missing.\n")
}

# 6. Tank Data Audit (Capacity & Substance) ------------------------------------
if (!is.null(data_list$Compartments)) {
  dt <- data_list$Compartments
  cat("\n\n================================================================================\n")
  cat("PART 4: TANK ATTRIBUTES AUDIT\n")
  cat("================================================================================\n")
  
  # A. Substance / Product
  cat("\n--- SUBSTANCE (Product) ---\n")
  if ("product" %in% names(dt)) {
    print(dt[, .N, by = product][order(-N)])
  }
  
  # B. Capacity
  cat("\n--- CAPACITY ---\n")
  if ("compartment_capacity" %in% names(dt)) {
    # Check for non-numeric characters
    dt[, cap_clean := as.numeric(gsub("[^0-9.]", "", compartment_capacity))]
    
    cat("Summary of Parsed Capacity:\n")
    print(summary(dt$cap_clean))
    
    cat("\nSample Raw vs Cleaned:\n")
    print(head(dt[!is.na(compartment_capacity), .(Raw=compartment_capacity, Clean=cap_clean)], 10))
    
    cat("\nTop 10 Most Common Capacities:\n")
    print(dt[, .N, by = cap_clean][order(-N)][1:10])
  }
  
  # C. Wall Types
  cat("\n--- WALL CONSTRUCTION ---\n")
  if ("tank_construction" %in% names(dt)) {
    print(dt[, .N, by = tank_construction][order(-N)])
  }
  
  cat("\n--- CONSTRUCTION CATEGORY ---\n")
  if ("category_of_construction" %in% names(dt)) {
    print(dt[, .N, by = category_of_construction][order(-N)])
  }
}

# 7. LUST Data Audit (Dates & Status) ------------------------------------------
if (!is.null(data_list$EnvSites)) {
  dt <- data_list$EnvSites
  cat("\n\n================================================================================\n")
  cat("PART 5: LUST DATA AUDIT\n")
  cat("================================================================================\n")
  
  # Discovery Date
  if ("discoverydate" %in% names(dt)) {
    cat("\n--- Discovery Date Analysis ---\n")
    raw_dates <- dt$discoverydate[!is.na(dt$discoverydate)]
    cat(paste0("Non-Missing Dates: ", length(raw_dates), " / ", nrow(dt), "\n"))
    
    cat("Sample Raw Values:\n")
    print(head(raw_dates, 10))
    
    # Test Parsing
    parsed <- parse_date_time(head(raw_dates, 10), orders = c("b-d-Y", "b-d-y"))
    print(data.table(Raw = head(raw_dates, 10), Parsed = parsed))
  }
  
  # Status
  cat("\n--- Current Status ---\n")
  if ("currentstatus" %in% names(dt)) {
    print(dt[, .N, by = currentstatus][order(-N)])
  }
}

sink()
message("Diagnostic complete. Log saved to: ", log_file)