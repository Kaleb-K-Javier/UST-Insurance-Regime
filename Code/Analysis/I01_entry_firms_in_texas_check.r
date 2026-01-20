###############################################################################
# ISSUE #1 DIAGNOSTIC: Entry Pattern Analysis
# 
# Purpose: Diagnose why is_facility_entry_event shows few TRUE values in 2000+
# Run this AFTER the main panel script completes, or insert sections as needed
###############################################################################

library(data.table)
library(lubridate)

# ─── Configuration ───────────────────────────────────────────────────────────
# Adjust these paths based on your environment
onserver <- TRUE
get_data_path <- function(...) {
  root <- Sys.getenv("UST_DATA_ROOT",
                     unset = file.path(Sys.getenv("HOME"), "UST", "Data"))
  file.path(root, ...)
}

stage_dir <- get_data_path("Outputs", "panel_merge_staging")

###############################################################################
# PART 1: Check Raw UST Data for New Installations
###############################################################################
message("\n========== PART 1: Raw UST Installation Analysis ==========\n")

# Load raw UST data (need to re-download or use saved version)
base <- "https://www.tceq.texas.gov/assets/public/admin/data/docs"

# If ust_monthly exists in staging, use that
if (file.exists(file.path(stage_dir, "ust_monthly.rds"))) {
  ust_monthly <- readRDS(file.path(stage_dir, "ust_monthly.rds"))
  
  message("Loaded ust_monthly from staging")
  message("Total rows: ", format(nrow(ust_monthly), big.mark=","))
  message("Unique facilities: ", uniqueN(ust_monthly$FACILITY_ID))
  
  # Diagnostic 1: First active month per facility
  first_active <- ust_monthly[active_tank_count > 0, .(
    first_year = min(YEAR),
    first_month = min(MONTH[YEAR == min(YEAR)])
  ), by = FACILITY_ID]
  
  message("\n--- First Active Month Distribution ---")
  fac_by_first_year <- first_active[, .N, by = first_year][order(first_year)]
  setnames(fac_by_first_year, "N", "facilities_first_active")
  print(fac_by_first_year[first_year >= 1990 & first_year <= 2025])
  
  # Diagnostic 2: Entry events in ust_monthly (Section 6 definition)
  if ("is_facility_entry_event" %in% names(ust_monthly)) {
    entry_by_year_ust <- ust_monthly[, .(
      entry_events = sum(is_facility_entry_event, na.rm = TRUE)
    ), by = YEAR][order(YEAR)]
    
    message("\n--- Entry Events by Year (ust_monthly, Section 6 definition) ---")
    print(entry_by_year_ust[YEAR >= 1990 & YEAR <= 2025])
  }
}

###############################################################################
# PART 2: Check Final Panel Entry Events  
###############################################################################
message("\n========== PART 2: Final Panel Analysis ==========\n")

# Load final panel
panel_path <- get_data_path("Outputs", "texas_fr_panel.rds")
if (file.exists(panel_path)) {
  panel <- readRDS(panel_path)
  
  message("Loaded final panel")
  message("Total rows: ", format(nrow(panel), big.mark=","))
  message("Unique facilities: ", uniqueN(panel$FACILITY_ID))
  
  # Diagnostic 3: Entry events in final panel (Section 7 definition)
  if ("is_facility_entry_event" %in% names(panel)) {
    entry_by_year_panel <- panel[, .(
      entry_events = sum(is_facility_entry_event, na.rm = TRUE),
      total_fac_months = .N,
      fac_with_tanks = sum(active_tank_count > 0, na.rm = TRUE)
    ), by = YEAR][order(YEAR)]
    
    message("\n--- Entry Events by Year (Final Panel, Section 7 definition) ---")
    print(entry_by_year_panel[YEAR >= 1990 & YEAR <= 2025])
  }
  
  # Diagnostic 4: Compare first active year with entry event year
  message("\n--- Facilities Starting Post-2000: Entry Event Check ---")
  
  # Find facilities with first active month in 2000+
  first_active_panel <- panel[active_tank_count > 0, .(
    first_active_year = min(YEAR)
  ), by = FACILITY_ID]
  
  new_fac_2000_plus <- first_active_panel[first_active_year >= 2000]$FACILITY_ID
  message("Facilities with first active year 2000+: ", length(new_fac_2000_plus))
  
  # Check if entry was flagged for these facilities
  entry_flagged <- panel[FACILITY_ID %in% new_fac_2000_plus, .(
    has_entry_flag = any(is_facility_entry_event == 1, na.rm = TRUE)
  ), by = FACILITY_ID]
  
  message("Of these, with entry event flagged: ", sum(entry_flagged$has_entry_flag))
  message("Missing entry flag: ", sum(!entry_flagged$has_entry_flag))
  
  # Diagnostic 5: Sample problematic facilities (if any)
  if (sum(!entry_flagged$has_entry_flag) > 0) {
    missing_entry_facs <- entry_flagged[has_entry_flag == FALSE]$FACILITY_ID
    sample_facs <- head(missing_entry_facs, 5)
    
    message("\n--- Sample Facilities Without Entry Flag ---")
    for (fac_id in sample_facs) {
      fac_data <- panel[FACILITY_ID == fac_id, 
                        .(YEAR, MONTH, active_tank_count, is_facility_entry_event)][order(YEAR, MONTH)]
      
      first_active <- fac_data[active_tank_count > 0][1]
      message("\nFacility ", fac_id, ":")
      message("  First active: ", first_active$YEAR, "-", sprintf("%02d", first_active$MONTH))
      message("  First 24 months:")
      print(head(fac_data, 24))
    }
  }
  
  # Diagnostic 6: Check entry event definition is working
  message("\n--- Entry Event Logic Verification ---")
  
  # Manually recompute entry events for comparison
  panel[, any_tank_active_check := active_tank_count > 0]
  panel[, entry_check := any_tank_active_check & !shift(any_tank_active_check, type="lag", fill=FALSE), by = FACILITY_ID]
  
  # Compare
  match_count <- panel[is_facility_entry_event == entry_check, .N]
  mismatch_count <- panel[is_facility_entry_event != entry_check, .N]
  
  message("Entry event matches manual check: ", format(match_count, big.mark=","))
  message("Entry event mismatches: ", format(mismatch_count, big.mark=","))
  
  if (mismatch_count > 0) {
    message("\n  WARNING: Entry event logic may have issues!")
    sample_mismatch <- panel[is_facility_entry_event != entry_check][1:min(5, mismatch_count)]
    print(sample_mismatch[, .(FACILITY_ID, YEAR, MONTH, active_tank_count, 
                               is_facility_entry_event, entry_check)])
  }
  
  # Clean up
  panel[, c("any_tank_active_check", "entry_check") := NULL]
}

###############################################################################
# PART 3: Expected vs Actual Comparison
###############################################################################
message("\n========== PART 3: Expected vs Actual Entries ==========\n")

# Expected entries based on raw data analysis (from Python):
expected_entries <- data.table(
  year = 1990:2025,
  expected = c(390, 261, 223, 232, 289, 348, 425, 367, 350, 466,
               456, 382, 267, 231, 254, 213, 217, 168, 185, 121,
               161, 199, 243, 261, 280, 309, 319, 261, 232, 213,
               239, 251, 298, 290, 225, 98)
)

if (exists("entry_by_year_panel")) {
  comparison <- merge(expected_entries, entry_by_year_panel, 
                      by.x = "year", by.y = "YEAR", all.x = TRUE)
  comparison[is.na(entry_events), entry_events := 0]
  comparison[, difference := entry_events - expected]
  comparison[, pct_of_expected := round(entry_events / expected * 100, 1)]
  
  message("--- Comparison: Expected vs Actual Entry Events ---")
  print(comparison[year >= 2000 & year <= 2024])
  
  message("\nSummary:")
  message("  Total expected 2000-2024: ", sum(comparison[year >= 2000 & year <= 2024, expected]))
  message("  Total actual 2000-2024: ", sum(comparison[year >= 2000 & year <= 2024, entry_events], na.rm = TRUE))
}

###############################################################################
# PART 4: Save Diagnostic Outputs
###############################################################################
message("\n========== PART 4: Saving Diagnostics ==========\n")

output_dir <- get_data_path("Outputs", "diagnostics")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

if (exists("fac_by_first_year")) {
  fwrite(fac_by_first_year, file.path(output_dir, "facilities_by_first_active_year.csv"))
}
if (exists("entry_by_year_panel")) {
  fwrite(entry_by_year_panel, file.path(output_dir, "entry_events_by_year_panel.csv"))
}
if (exists("comparison")) {
  fwrite(comparison, file.path(output_dir, "entry_expected_vs_actual.csv"))
}

message("Diagnostic files saved to: ", output_dir)
message("\n========== DIAGNOSTIC COMPLETE ==========\n")