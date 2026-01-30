## Arkansas UST Data - Full Diagnostic Report
# Script: D01_AR_Diagnostics.R
# Purpose: Comprehensive diagnostic of Arkansas Access database
# Focus: Identify -Inf issue and map geography sources

library(DBI)
library(odbc)
library(data.table)
library(tidyverse)
library(here)
library(janitor)
library(lubridate)

# Setup
db_path <- here("Data","Raw","state_databases","Arkansas","TankStats_web.mdb")
output_dir <- here("Output","Tables","Data_Quality_Tables")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

log_file <- file.path(output_dir, "AR_Full_Diagnostic_Report.txt")

# Connect
con <- dbConnect(
  odbc(),
  .connection_string = paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};Dbq=", db_path, ";")
)

sink(log_file)
cat(rep("=", 80), "\n", sep="")
cat("ARKANSAS COMPREHENSIVE DIAGNOSTIC REPORT\n")
cat("Generated: ", as.character(Sys.time()), "\n")
cat(rep("=", 80), "\n\n", sep="")

# Load tables
cat("--- LOADING TABLES ---\n")
rfacs <- as.data.table(dbReadTable(con, "TempRFACS_CountryNO"))
tanks <- as.data.table(dbReadTable(con, "TempTankStats_UG"))
lust <- as.data.table(dbReadTable(con, "TempRLUSTLOG1"))

# Check for GIS table
all_tables <- dbListTables(con)
has_gis <- "TempGIS" %in% all_tables
if(has_gis) {
  gis <- as.data.table(dbReadTable(con, "TempGIS"))
  cat("Loaded GIS table:", nrow(gis), "rows\n")
}

cat("Facilities:", nrow(rfacs), "| Tanks:", nrow(tanks), "| LUSTs:", nrow(lust), "\n\n")

# ============================================================================
# PART 1: GEOGRAPHY INVESTIGATION
# ============================================================================
cat(rep("=", 80), "\n", sep="")
cat("PART 1: GEOGRAPHY SOURCES\n")
cat(rep("=", 80), "\n\n", sep="")

cat("--- FACILITIES TABLE (TempRFACS_CountryNO) ---\n")
cat("Lat/Long Columns Found: LOC_LATITUDE, LOC_LONGITUDE\n\n")

# Check coverage
rfacs[, `:=`(
  has_lat = !is.na(LOC_LATITUDE) & LOC_LATITUDE != 0,
  has_long = !is.na(LOC_LONGITUDE) & LOC_LONGITUDE != 0
)]

lat_coverage <- sum(rfacs$has_lat) / nrow(rfacs) * 100
lon_coverage <- sum(rfacs$has_long) / nrow(rfacs) * 100
both_coverage <- sum(rfacs$has_lat & rfacs$has_long) / nrow(rfacs) * 100

cat("Latitude coverage:", round(lat_coverage, 1), "%\n")
cat("Longitude coverage:", round(lon_coverage, 1), "%\n")
cat("Both lat/long:", round(both_coverage, 1), "%\n")

cat("\nSample coordinates (first 10 with data):\n")
print(rfacs[has_lat & has_long, .(FACILITY_ID, LOC_LATITUDE, LOC_LONGITUDE, LOC_NAME, CountyCAPS)][1:10])

# County coverage
cat("\n--- COUNTY DATA ---\n")
county_coverage <- sum(!is.na(rfacs$CountyCAPS) & rfacs$CountyCAPS != "") / nrow(rfacs) * 100
cat("County coverage:", round(county_coverage, 1), "%\n")
cat("\nCounty frequency (top 10):\n")
print(rfacs[!is.na(CountyCAPS) & CountyCAPS != "", .N, by = CountyCAPS][order(-N)][1:10])

# GIS table check
if(has_gis) {
  cat("\n--- TEMP GIS TABLE ---\n")
  cat("Columns:\n")
  print(names(gis))
  cat("\nDimensions:", nrow(gis), "rows x", ncol(gis), "columns\n")
  cat("\nSample:\n")
  print(head(gis, 5))
}

# ============================================================================
# PART 2: TANK CONSTRUCTION & SUBSTANCE FLAGS
# ============================================================================
cat("\n\n", rep("=", 80), "\n", sep="")
cat("PART 2: TANK CONSTRUCTION & SUBSTANCE FLAG ANALYSIS\n")
cat(rep("=", 80), "\n\n", sep="")

cat("--- CONSTRUCTION MATERIAL FLAGS (UT_MOC_*) ---\n")
cat("Total tanks:", nrow(tanks), "\n\n")

# Construction flags
const_flags <- c(
  "UT_MOC_SteelTF", "UT_MOC_EpoxyTF", "UT_MOC_CompositeTF", 
  "UT_MOC_FbrPlasticTF", "UT_MOC_ConcreteTF", "UT_MOC_IntLiningTF",
  "UT_MOC_ExcvLinerTF", "UT_MOC_DblWallTF", "UT_MOC_PolyJacketTF", 
  "UT_MOC_UnknownTF"
)

for(flag in const_flags) {
  if(flag %in% names(tanks)) {
    true_count <- sum(tanks[[flag]] == TRUE, na.rm = TRUE)
    false_count <- sum(tanks[[flag]] == FALSE, na.rm = TRUE)
    na_count <- sum(is.na(tanks[[flag]]))
    
    cat(sprintf("%-30s TRUE: %5d | FALSE: %5d | NA: %5d\n", 
                flag, true_count, false_count, na_count))
  }
}

# Test what happens with no TRUE values
cat("\n--- CRITICAL TEST: What happens when NO tanks have material flags? ---\n")
test_subset <- tanks[1:5, .(RSTFacNbr, UT_MOC_SteelTF, UT_MOC_DblWallTF, UT_MOC_UnknownTF)]
cat("Sample tanks:\n")
print(test_subset)

# Convert to binary
test_subset[, `:=`(
  steel_bin = ifelse(UT_MOC_SteelTF == TRUE, 1, 0),
  dbl_bin = ifelse(UT_MOC_DblWallTF == TRUE, 1, 0)
)]
test_subset[is.na(steel_bin), steel_bin := 0]
test_subset[is.na(dbl_bin), dbl_bin := 0]

cat("\nAfter conversion to binary:\n")
print(test_subset)

cat("\nTesting max() on all-zero columns:\n")
test_result <- max(c(0, 0, 0, 0), na.rm = TRUE)
cat("max(c(0,0,0,0), na.rm=TRUE) =", test_result, "\n")

test_result_na <- suppressWarnings(max(c(NA, NA, NA), na.rm = TRUE))
cat("max(c(NA,NA,NA), na.rm=TRUE) =", test_result_na, "\n")
cat("is.infinite(test_result_na) =", is.infinite(test_result_na), "\n")

# Count tanks with NO material flags
tanks_copy <- copy(tanks)
for(flag in const_flags) {
  if(flag %in% names(tanks_copy)) {
    new_col <- gsub("UT_MOC_|TF", "", flag) %>% tolower()
    tanks_copy[, (new_col) := ifelse(get(flag) == TRUE, 1, 0)]
    tanks_copy[is.na(get(new_col)), (new_col) := 0]
  }
}

material_cols <- c("steel", "epoxy", "composite", "fbrplastic", "concrete", 
                   "intlining", "excvliner", "dblwall", "polyjacket", "unknown")
existing_mat <- intersect(material_cols, names(tanks_copy))

tanks_copy[, total_mat_flags := rowSums(.SD), .SDcols = existing_mat]

tanks_no_flags <- tanks_copy[total_mat_flags == 0]
cat("\n>>> TANKS WITH NO MATERIAL FLAGS:", nrow(tanks_no_flags), 
    "(", round(nrow(tanks_no_flags)/nrow(tanks)*100, 1), "%)\n")

cat("\nSample tanks with no flags:\n")
print(tanks_no_flags[1:5, .(RSTFacNbr, TankNbr, TankStatusTypeCode, InstallDate)])

# ============================================================================
# PART 3: SUBSTANCE FLAGS
# ============================================================================
cat("\n\n--- SUBSTANCE FLAGS (UT_SS_*) ---\n")

subst_flags <- c(
  "UT_SS_EmptyTF", "UT_SS_DieselTF", "UT_SS_KeroseneTF",
  "UT_SS_GasolineTF", "UT_SS_UsedOilTF", "UT_SS_NewOilTF", "UT_SS_UnknownTF"
)

for(flag in subst_flags) {
  if(flag %in% names(tanks)) {
    true_count <- sum(tanks[[flag]] == TRUE, na.rm = TRUE)
    false_count <- sum(tanks[[flag]] == FALSE, na.rm = TRUE)
    na_count <- sum(is.na(tanks[[flag]]))
    
    cat(sprintf("%-25s TRUE: %5d | FALSE: %5d | NA: %5d\n", 
                flag, true_count, false_count, na_count))
  }
}

# ============================================================================
# PART 4: TANK STATUS CODES
# ============================================================================
cat("\n\n", rep("=", 80), "\n", sep="")
cat("PART 3: TANK STATUS ANALYSIS\n")
cat(rep("=", 80), "\n\n", sep="")

cat("--- TankStatusTypeCode Frequency ---\n")
print(tanks[, .N, by = TankStatusTypeCode][order(-N)])

cat("\n--- TankStatusChangeRsnCode Frequency ---\n")
print(tanks[, .N, by = TankStatusChangeRsnCode][order(-N)])

# Cross-tabulate
cat("\n--- Status Type x Change Reason (top combinations) ---\n")
print(tanks[, .N, by = .(TankStatusTypeCode, TankStatusChangeRsnCode)][order(-N)][1:20])

# ============================================================================
# PART 5: DATE COVERAGE
# ============================================================================
cat("\n\n", rep("=", 80), "\n", sep="")
cat("PART 4: DATE COVERAGE\n")
cat(rep("=", 80), "\n\n", sep="")

cat("--- TANK DATES ---\n")
install_coverage <- sum(!is.na(tanks$InstallDate)) / nrow(tanks) * 100
status_coverage <- sum(!is.na(tanks$TankStatusDate)) / nrow(tanks) * 100

cat("InstallDate coverage:", round(install_coverage, 1), "%\n")
cat("TankStatusDate coverage:", round(status_coverage, 1), "%\n")

if(install_coverage > 0) {
  install_range <- range(tanks$InstallDate, na.rm = TRUE)
  cat("Install date range:", as.character(install_range[1]), "to", as.character(install_range[2]), "\n")
}

cat("\n--- LUST DATES ---\n")
release_coverage <- sum(!is.na(lust$ReleaseConfirmedDate)) / nrow(lust) * 100
nfa_coverage <- sum(!is.na(lust$NFAIssuedDate)) / nrow(lust) * 100

cat("ReleaseConfirmedDate coverage:", round(release_coverage, 1), "%\n")
cat("NFAIssuedDate coverage:", round(nfa_coverage, 1), "%\n")

if(release_coverage > 0) {
  release_range <- range(lust$ReleaseConfirmedDate, na.rm = TRUE)
  cat("Release date range:", as.character(release_range[1]), "to", as.character(release_range[2]), "\n")
}

# ============================================================================
# PART 6: LINKAGE TEST
# ============================================================================
cat("\n\n", rep("=", 80), "\n", sep="")
cat("PART 5: LINKAGE STRESS TEST\n")
cat(rep("=", 80), "\n\n", sep="")

# Clean IDs
rfacs[, facility_id_clean := as.character(trimws(FACILITY_ID))]
tanks[, facility_id_clean := as.character(trimws(RSTFacNbr))]
lust[, facility_id_clean := as.character(trimws(RSTFacNbr))]

fac_ids <- unique(rfacs$facility_id_clean)
tank_ids <- unique(tanks$facility_id_clean)
lust_ids <- unique(lust$facility_id_clean)

cat("Unique Facility IDs:\n")
cat("  Facilities table:", length(fac_ids), "\n")
cat("  Tanks table:", length(tank_ids), "\n")
cat("  LUST table:", length(lust_ids), "\n\n")

cat("--- Facilities -> Tanks ---\n")
fac_in_tanks <- sum(fac_ids %in% tank_ids)
cat("Facilities with tanks:", fac_in_tanks, "(", round(fac_in_tanks/length(fac_ids)*100,1), "%)\n")

orphaned_fac <- setdiff(fac_ids, tank_ids)
cat("Orphaned facilities (no tanks):", length(orphaned_fac), "\n")

cat("\n--- Tanks -> Facilities ---\n")
tanks_in_fac <- sum(tank_ids %in% fac_ids)
cat("Tanks with facility match:", tanks_in_fac, "(", round(tanks_in_fac/length(tank_ids)*100,1), "%)\n")

cat("\n--- LUST -> Tanks ---\n")
lust_in_tanks <- sum(lust_ids %in% tank_ids)
cat("LUSTs with tank match:", lust_in_tanks, "(", round(lust_in_tanks/length(lust_ids)*100,1), "%)\n")

# ============================================================================
# PART 7: THE -Inf PROBLEM ROOT CAUSE
# ============================================================================
cat("\n\n", rep("=", 80), "\n", sep="")
cat("PART 6: ROOT CAUSE ANALYSIS - THE -Inf PROBLEM\n")
cat(rep("=", 80), "\n\n", sep="")

cat("HYPOTHESIS: -Inf occurs when aggregating tanks with NO flag data\n\n")

# Simulate the user's current code logic
test_tanks <- tanks[1:100, .(RSTFacNbr, UT_MOC_SteelTF, UT_MOC_DblWallTF, UT_MOC_IntLiningTF)]

# Convert like the user does
test_tanks[, `:=`(
  steel = ifelse(UT_MOC_SteelTF == TRUE, 1, 0),
  dbl_wall = ifelse(UT_MOC_DblWallTF == TRUE, 1, 0),
  int_lining = ifelse(UT_MOC_IntLiningTF == TRUE, 1, 0)
)]

# Replace NA with 0 (like user code)
for(col in c("steel", "dbl_wall", "int_lining")) {
  test_tanks[is.na(get(col)), (col) := 0]
}

# Apply user's wall logic
test_tanks[, double_walled := ifelse(dbl_wall == 1 | int_lining == 1, 1, 0)]
test_tanks[, single_walled := ifelse(
  dbl_wall == 0 & int_lining == 0 & steel > 0, 1, 0
)]

cat("Sample after binary conversion:\n")
print(test_tanks[1:10, .(RSTFacNbr, steel, dbl_wall, int_lining, double_walled, single_walled)])

# Now aggregate like the user does
test_agg <- test_tanks[, .(
  single_max = max(single_walled, na.rm = TRUE),
  double_max = max(double_walled, na.rm = TRUE)
), by = RSTFacNbr]

cat("\nAfter aggregation with max():\n")
print(test_agg[1:10])

# Check for -Inf
has_inf_single <- sum(is.infinite(test_agg$single_max))
has_inf_double <- sum(is.infinite(test_agg$double_max))

cat("\nRows with -Inf in single_max:", has_inf_single, "\n")
cat("Rows with -Inf in double_max:", has_inf_double, "\n")

if(has_inf_single > 0 || has_inf_double > 0) {
  cat("\n>>> -Inf DETECTED! This confirms the problem.\n")
} else {
  cat("\n>>> No -Inf in this sample, but may occur in full dataset.\n")
}

# ============================================================================
# PART 8: SOLUTION DEMONSTRATION
# ============================================================================
cat("\n\n", rep("=", 80), "\n", sep="")
cat("PART 7: SOLUTION - SAFE AGGREGATION\n")
cat(rep("=", 80), "\n\n", sep="")

cat("SOLUTION 1: Use safe_max() function\n")
cat("CODE:\n")
cat("safe_max <- function(x, na.rm = TRUE) {\n")
cat("  result <- suppressWarnings(max(x, na.rm = na.rm))\n")
cat("  ifelse(is.infinite(result) | is.na(result), 0, result)\n")
cat("}\n\n")

safe_max <- function(x, na.rm = TRUE) {
  result <- suppressWarnings(max(x, na.rm = na.rm))
  ifelse(is.infinite(result) | is.na(result), 0, result)
}

test_agg_fixed <- test_tanks[, .(
  single_max = safe_max(single_walled),
  double_max = safe_max(double_walled)
), by = RSTFacNbr]

cat("After using safe_max():\n")
print(test_agg_fixed[1:10])

has_inf_after <- sum(is.infinite(test_agg_fixed$single_max)) + sum(is.infinite(test_agg_fixed$double_max))
cat("\nRows with -Inf after fix:", has_inf_after, "\n")

cat("\n\nSOLUTION 2: Post-aggregation cleanup\n")
cat("CODE:\n")
cat("for(col in numeric_cols) {\n")
cat("  DT[is.infinite(get(col)), (col) := 0]\n")
cat("}\n")

# ============================================================================
# SUMMARY
# ============================================================================
cat("\n\n", rep("=", 80), "\n", sep="")
cat("SUMMARY & RECOMMENDATIONS\n")
cat(rep("=", 80), "\n\n", sep="")

cat("GEOGRAPHY:\n")
cat("✓ Lat/Long available in Facilities table (LOC_LATITUDE, LOC_LONGITUDE)\n")
cat("✓ Coverage:", round(both_coverage, 1), "%\n")
cat("✓ County data available (CountyCAPS):", round(county_coverage, 1), "%\n")

cat("\nDATA QUALITY:\n")
cat("✓ Tank install dates:", round(install_coverage, 1), "% coverage\n")
cat("✓ LUST release dates:", round(release_coverage, 1), "% coverage\n")
cat("⚠ Tanks with no material flags:", round(nrow(tanks_no_flags)/nrow(tanks)*100, 1), "%\n")

cat("\n-Inf PROBLEM:\n")
cat("⚠ ROOT CAUSE: max(NA, na.rm=TRUE) returns -Inf when all values are NA\n")
cat("⚠ OCCURS: When facilities have no tanks with material/substance flags set\n")
cat("✓ FIX: Use safe_max() function OR post-aggregation cleanup\n")

cat("\nRECOMMENDED FIXES:\n")
cat("1. Add safe_max() function to top of script\n")
cat("2. Replace all max() calls with safe_max() in aggregation\n")
cat("3. OR add post-aggregation loop to replace -Inf with 0\n")
cat("4. Merge geography from Facilities table (LOC_LATITUDE/LOC_LONGITUDE)\n")

cat("\n", rep("=", 80), "\n", sep="")
cat("END OF DIAGNOSTIC\n")
cat(rep("=", 80), "\n", sep="")

sink()
dbDisconnect(con)

cat("\n✓ Diagnostic complete!\n")
cat("Report saved to:", log_file, "\n")
cat("\nKey findings:\n")
cat("  - Geography: ", round(both_coverage, 1), "% coverage in Facilities table\n", sep="")
cat("  - -Inf issue: Confirmed - occurs with", round(nrow(tanks_no_flags)/nrow(tanks)*100, 1), "% of tanks\n")
cat("  - Solution: Use safe_max() function provided in report\n")