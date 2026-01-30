# ==============================================================================
# 99_Universal_State_Diagnostic.R
# Purpose: Run standardized diagnostics on the MASTER Harmonized Datasets.
# Input:   'Master_Harmonized_UST_Tanks.csv' & 'Master_Harmonized_LUST.csv'
# Scope:
#   1. Geography Completeness (Lat/Long, County).
#   2. Attribute Distribution (Walls, Substance, Capacity, Status).
#   3. Date Ranges (Install, Closed, Leak Report, NFA).
#   4. Merge Logic Probe (Sample 10 IDs vs EPA Raw).
# Output: 'Universal_Diagnostic_Report.txt'
# ==============================================================================

library(data.table)
library(tidyverse)
library(here)
library(fs)
library(stringr)
library(janitor)

# 1. Setup ---------------------------------------------------------------------
processed_dir <- here("Data", "Processed")
epa_file      <- here("Data", "Raw", "Releases.csv") 
log_file      <- here("Data", "Processed", "Universal_Diagnostic_Report.txt")

# A. Load Master Datasets
message("Loading Master UST Tanks...")
if(file.exists(file.path(processed_dir, "Master_Harmonized_UST_Tanks.csv"))) {
  master_tanks <- fread(file.path(processed_dir, "Master_Harmonized_UST_Tanks.csv"), 
                        colClasses = c("facility_id" = "character", "tank_id" = "character"))
} else {
  stop("Master_Harmonized_UST_Tanks.csv not found!")
}

message("Loading Master LUSTs...")
if(file.exists(file.path(processed_dir, "Master_Harmonized_LUST.csv"))) {
  master_lusts <- fread(file.path(processed_dir, "Master_Harmonized_LUST.csv"), 
                        colClasses = c("facility_id" = "character"))
} else {
  master_lusts <- NULL
  warning("Master_Harmonized_LUST.csv not found!")
}

# B. Load EPA Data ONCE for ID comparison
message("Loading EPA National Data for ID Probing...")
if(file.exists(epa_file)) {
  epa_national <- fread(epa_file, select = c("Facility_ID", "State"), colClasses = "character") %>% 
    clean_names()
} else {
  epa_national <- NULL
  warning("EPA Releases.csv not found. ID Probing will be skipped.")
}

# Start Logging
sink(log_file)
cat(paste0(strrep("=", 80), "\n"))
cat("UNIVERSAL STATE DATA DIAGNOSTIC REPORT (MASTER FILES)\n")
cat("Generated: ", as.character(Sys.time()), "\n")
cat(paste0(strrep("=", 80), "\n"))

# 2. Identify States -----------------------------------------------------------
states <- unique(master_tanks$state)
states <- states[!is.na(states) & states != ""]
states <- sort(states)

cat(paste0("\nFound ", length(states), " states in Master Dataset: ", paste(states, collapse=", "), "\n"))

# 3. Diagnostic Function -------------------------------------------------------
run_diagnostic <- function(abbr) {
  # ASCII Header
  cat("\n\n")
  cat(strrep("=", 60), "\n")
  cat(sprintf("STATE: %-5s\n", abbr))
  cat(strrep("=", 60), "\n")
  
  # A. Filter Data
  dt <- master_tanks[state == abbr]
  dl <- if(!is.null(master_lusts)) master_lusts[state == abbr] else NULL
  
  cat(paste0("  Total Tank Records: ", format(nrow(dt), big.mark=","), "\n"))
  if(!is.null(dl)) cat(paste0("  Total LUST Records: ", format(nrow(dl), big.mark=","), "\n"))
  
  # B. Geography Check
  cat("\n  --- 1. GEOGRAPHY COMPLETENESS ---\n")
  lat_miss <- mean(is.na(dt$latitude)) * 100
  cty_miss <- mean(is.na(dt$county_name) | dt$county_name == "") * 100
  
  cat(sprintf("  %-20s : %5.1f%%\n", "Missing Lat/Long", lat_miss))
  cat(sprintf("  %-20s : %5.1f%%\n", "Missing County", cty_miss))
  
  # C. Attribute Check
  cat("\n  --- 2. ATTRIBUTE DISTRIBUTIONS ---\n")
  
  # Tank Status
  cat("  [Tank Status]\n")
  if("tank_status" %in% names(dt)) {
    # Print table of status counts
    status_counts <- dt[, .N, by = tank_status][order(-N)]
    for(i in 1:nrow(status_counts)) {
      st_label <- ifelse(is.na(status_counts$tank_status[i]) | status_counts$tank_status[i] == "", "Missing/NA", status_counts$tank_status[i])
      cat(sprintf("    %-20s : %6s\n", st_label, format(status_counts$N[i], big.mark=",")))
    }
  }
  
  # Wall Types
  cat("\n  [Wall Type Summary]\n")
  if("single_walled" %in% names(dt)) {
    sw <- sum(dt$single_walled, na.rm=T)
    dw <- sum(dt$double_walled, na.rm=T)
    uw <- sum(dt$unknown_walled, na.rm=T)
    
    cat(sprintf("    %-10s : %6s\n", "Single", format(sw, big.mark=",")))
    cat(sprintf("    %-10s : %6s\n", "Double", format(dw, big.mark=",")))
    cat(sprintf("    %-10s : %6s\n", "Unknown", format(uw, big.mark=",")))
  }
  
  # Substance
  cat("\n  [Substance Summary]\n")
  if("is_gasoline" %in% names(dt)) {
    gas <- sum(dt$is_gasoline, na.rm=T)
    dsl <- sum(dt$is_diesel, na.rm=T)
    oil <- sum(dt$is_oil_kerosene, na.rm=T)
    jet <- sum(dt$is_jet_fuel, na.rm=T)
    oth <- sum(dt$is_other, na.rm=T)
    
    cat(sprintf("    %-10s : %6s\n", "Gasoline", format(gas, big.mark=",")))
    cat(sprintf("    %-10s : %6s\n", "Diesel", format(dsl, big.mark=",")))
    cat(sprintf("    %-10s : %6s\n", "Oil/Kero", format(oil, big.mark=",")))
    cat(sprintf("    %-10s : %6s\n", "Jet Fuel", format(jet, big.mark=",")))
    cat(sprintf("    %-10s : %6s\n", "Other", format(oth, big.mark=",")))
  }
  
  # Capacity
  cat("\n  [Capacity Statistics]\n")
  if("capacity" %in% names(dt)) {
    caps <- dt$capacity[dt$capacity > 0 & !is.na(dt$capacity)]
    if(length(caps) > 0) {
      cat(sprintf("    %-10s : %8.0f\n", "Mean", mean(caps)))
      cat(sprintf("    %-10s : %8.0f\n", "Median", median(caps)))
      cat(sprintf("    %-10s : %8s\n", "Valid Recs", format(length(caps), big.mark=",")))
    } else {
      cat("    [!] No valid capacity data found (>0).\n")
    }
  }
  
  # D. Date Check
  cat("\n  --- 3. DATE ANALYSIS ---\n")
  
  check_date_col <- function(date_vec, label) {
    valid <- date_vec[!is.na(date_vec)]
    n_valid <- length(valid)
    pct_valid <- (n_valid / length(date_vec)) * 100
    
    if (n_valid > 0) {
      min_d <- min(valid)
      max_d <- max(valid)
      cat(sprintf("    %-20s : %6s valid (%5.1f%%) | Range: %s to %s\n", 
                  label, format(n_valid, big.mark=","), pct_valid, min_d, max_d))
    } else {
      cat(sprintf("    %-20s :      0 valid (  0.0%%)\n", label))
    }
  }
  
  check_date_col(dt$tank_installed_date, "Tank Installed")
  check_date_col(dt$tank_closed_date, "Tank Closed")
  
  if(!is.null(dl)) {
    check_date_col(dl$report_date, "LUST Report")
    check_date_col(dl$nfa_date, "LUST NFA")
  }
  
  # E. ID Match Probe
  if(!is.null(epa_national)) {
    cat("\n  --- 4. EPA ID MATCH PROBE (10 SAMPLES) ---\n")
    
    # Get State Pool
    state_ids <- unique(trimws(dt$facility_id))
    state_ids <- state_ids[state_ids != ""]
    
    # Get EPA Pool for this state
    epa_state_ids <- epa_national[state == abbr]$facility_id
    
    if(length(state_ids) > 0 && length(epa_state_ids) > 0) {
      # Sample
      set.seed(99)
      sample_ids <- sample(state_ids, min(10, length(state_ids)))
      
      # Cleaning Logic (Standard Strip)
      clean_fn <- function(x) gsub("^[A-Z]{2}[- ]?|^0+", "", trimws(x), ignore.case=TRUE)
      
      clean_state_sample <- clean_fn(sample_ids)
      clean_epa_pool     <- clean_fn(epa_state_ids)
      
      # Test
      cat(sprintf("  %-15s | %-15s | %-10s\n", "Raw State ID", "Cleaned", "In EPA?"))
      cat(paste0("  ", strrep("-", 48), "\n"))
      for(i in 1:length(sample_ids)) {
        found <- clean_state_sample[i] %in% clean_epa_pool
        cat(sprintf("  %-15s | %-15s | %-10s\n", str_trunc(sample_ids[i], 15), str_trunc(clean_state_sample[i], 15), found))
      }
      
    } else {
      cat("  [!] Cannot run ID probe: No IDs found in one or both datasets.\n")
      cat(sprintf("      State IDs found: %d\n", length(state_ids)))
      cat(sprintf("      EPA IDs found:   %d\n", length(epa_state_ids)))
    }
  }
}

# 4. Execution Loop ------------------------------------------------------------
for(s in states) {
  tryCatch({
    run_diagnostic(s)
  }, error = function(e) {
    cat(paste0("\n  [!] Error processing ", s, ": ", e$message, "\n"))
  })
}

sink()
message("Universal Diagnostic Complete. Results saved to: ", log_file)