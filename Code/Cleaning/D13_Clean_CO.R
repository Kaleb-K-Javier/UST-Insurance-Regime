# ==============================================================================
# D13_Diagnostic_CO_Enhanced.R
# Purpose: Deep Audit of CO OPS with EXPANDED TANK & PIPING ATTRIBUTES.
#          * Context: Previous audit showed 'tank_wall_type' is 98% Empty.
#          * Update: Added frequency tables for Material, Piping, and Detection.
#          * Output: Saves full log to 'CO_Enhanced_Diagnostic_Log.txt'.
# ==============================================================================

library(data.table)
library(tidyverse)
library(here)
library(janitor)
library(lubridate)

# 1. Setup & Config ------------------------------------------------------------
options(width = 300)
options(datatable.print.class = TRUE) 
options(datatable.print.keys = TRUE)

raw_dir  <- here("Data", "Raw", "state_databases", "Colorado")
log_file <- file.path(raw_dir, "CO_Enhanced_Diagnostic_Log.txt")

f_tanks    <- file.path(raw_dir, "Regulated_Storage_Tanks_in_Colorado__Oil___Public_Safety_20250728.csv")
f_releases <- file.path(raw_dir, "Petroleum_Releases__Colorado_Oil___Public_Safety_20250728.csv")

sink(log_file)
cat("================================================================================\n")
cat("COLORADO OPS ENHANCED DIAGNOSTIC REPORT\n")
cat("Generated: ", as.character(Sys.time()), "\n")
cat("================================================================================\n\n")

# 2. Loading -------------------------------------------------------------------
load_co_csv <- function(f) {
  if(!file.exists(f)) return(NULL)
  fread(f, colClasses = "character", na.strings = c("", "NA", "NULL")) %>% clean_names()
}

cat("--- 1. LOADING FILES ---\n")
dt_tanks    <- load_co_csv(f_tanks)
cat(sprintf("Regulated Tanks:    %s rows\n", format(nrow(dt_tanks), big.mark=",")))
dt_releases <- load_co_csv(f_releases)
cat(sprintf("Petroleum Releases: %s rows\n", format(nrow(dt_releases), big.mark=",")))

# 3. Enhanced Frequency Tables -------------------------------------------------
cat("\n\n================================================================================\n")
cat("PART 3: EXTENDED TANK & PIPING ATTRIBUTES\n")
cat("   * Note: 'tank_wall_type' was found to be empty; checking surrogates.\n")
cat("================================================================================\n")

if(!is.null(dt_tanks)) {
  
  # Helper to print freq table
  print_freq <- function(dt, col_name) {
    cat(sprintf("\n--- %s ---\n", toupper(col_name)))
    if(col_name %in% names(dt)) {
      print(dt[, .N, by = get(col_name)][order(-N)], nrows = Inf)
    } else {
      cat("[MISSING COLUMN]\n")
    }
  }

  # A. Tank Construction (The "Missing Wall Type" Fix)
  print_freq(dt_tanks, "tank_material")                    # Primary construction info
  print_freq(dt_tanks, "tank_corrosion_protection_method") # Corrosion info often proxies for material
  print_freq(dt_tanks, "tank_release_detection_method_primary") 

  # B. Piping Details (Crucial for Leak Risk)
  print_freq(dt_tanks, "piping_material")
  print_freq(dt_tanks, "piping_type")                      # e.g., Suction vs Pressure
  print_freq(dt_tanks, "piping_wall_type")                 # Check if this is also empty like tank_wall_type
  
  # C. Operational Details
  print_freq(dt_tanks, "overfill_prevention")
  print_freq(dt_tanks, "product")
  print_freq(dt_tanks, "tank_status")
}

# 4. Release Correlates --------------------------------------------------------
cat("\n\n================================================================================\n")
cat("PART 4: RELEASE CAUSES & STATUS\n")
cat("================================================================================\n")

if(!is.null(dt_releases)) {
  print_freq(dt_releases, "release_cause")
  print_freq(dt_releases, "release_source")                # Added: Source (Dispenser vs Tank)
  print_freq(dt_releases, "status")
}

# 5. Join Integrity & Dates ----------------------------------------------------
cat("\n\n================================================================================\n")
cat("PART 5: DATA INTEGRITY CHECKS\n")
cat("================================================================================\n")

# Dates
if("closure_date" %in% names(dt_tanks)) {
  dt_tanks[, d := parse_date_time(closure_date, orders = c("mdY", "Ymd", "dmY"), quiet = T)]
  cat(sprintf("Valid Tank Closure Dates: %s (%.1f%%)\n", 
              format(sum(!is.na(dt_tanks$d)), big.mark=","), 
              100 * sum(!is.na(dt_tanks$d))/nrow(dt_tanks)))
}

if("closure_date" %in% names(dt_releases)) {
  dt_releases[, d := parse_date_time(closure_date, orders = c("mdY", "Ymd", "dmY"), quiet = T)]
  cat(sprintf("Valid Release Closure Dates: %s (%.1f%%)\n", 
              format(sum(!is.na(dt_releases$d)), big.mark=","), 
              100 * sum(!is.na(dt_releases$d))/nrow(dt_releases)))
}

# Merge
if(!is.null(dt_tanks) && !is.null(dt_releases)) {
  t_ids <- unique(trimws(dt_tanks$facility_id))
  r_ids <- unique(trimws(dt_releases$facility_id))
  overlap <- length(intersect(t_ids, r_ids))
  
  cat(sprintf("\nMerge Key: 'facility_id'\n"))
  cat(sprintf("   Tanks Unique IDs:    %s\n", format(length(t_ids), big.mark=",")))
  cat(sprintf("   Releases Unique IDs: %s\n", format(length(r_ids), big.mark=",")))
  cat(sprintf("   Overlap Count:       %s\n", format(overlap, big.mark=",")))
  cat(sprintf("   Coverage:            %.1f%% of Releases have Tank Data\n", 
              100 * overlap / length(r_ids)))
}

sink()
message(sprintf("Enhanced Diagnostic Complete. Log: %s", log_file))