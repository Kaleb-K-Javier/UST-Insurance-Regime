# ==============================================================================
# SCRIPT 01: CREATE STRATIFIED SAMPLE FOR DEBUGGING (TASK 1)
# ==============================================================================
# Purpose: Create a small (e.g., 5%) sample of the full panel that is
#           stratified by the leak outcome, ensuring that debug models
#           will have "deaths" (leak events) to train on.
#
# Input:  facility_leak_behavior_monthly.csv (FULL dataset)
# Output: facility_leak_behavior_monthly_SAMPLE.csv (Small, stratified sample)
# ==============================================================================

library(data.table)
library(here)

set.seed(129837) # For reproducible sampling
SAMPLE_FRACTION <- 0.05 # 5%

# 1. Load the FULL panel data
cat("================================================================================\n")
cat("SCRIPT 01: CREATING STRATIFIED PANEL (FACILITY) SAMPLE\n")
cat("================================================================================\n")
cat("Loading full panel data (this may take a moment)...\n")
full_panel <- fread(here("Data", "Processed", "facility_leak_behavior_monthly.csv"),
                     select = c("facility_id", "state", "leak_incident")) # Only need key cols

# 2. Identify facilities by leak status
cat("Summarizing facilities by leak status...\n")
facility_summary <- full_panel[, .(
  had_leak = as.integer(any(leak_incident == 1, na.rm = TRUE))
), by = .(facility_id, state)]

leaking_facilities <- facility_summary[had_leak == 1]
non_leaking_facilities <- facility_summary[had_leak == 0]

cat(sprintf("Total facilities: %d\n", nrow(facility_summary)))
cat(sprintf("  - Leaking facilities: %d\n", nrow(leaking_facilities)))
cat(sprintf("  - Non-leaking facilities: %d\n", nrow(non_leaking_facilities)))

# 3. Sample from EACH group
cat(sprintf("Sampling %.0f%% from each group...\n", SAMPLE_FRACTION * 100))

# Sample 5% of leakers
sampled_leakers <- leaking_facilities[
  sample(.N, size = max(1, floor(.N * SAMPLE_FRACTION)))
]

# Sample 5% of non-leakers
sampled_nonleakers <- non_leaking_facilities[
  sample(.N, size = max(1, floor(.N * SAMPLE_FRACTION)))
]

# 4. Combine IDs and filter the full panel
sampled_ids <- rbindlist(list(sampled_leakers, sampled_nonleakers))
sampled_facility_keys <- sampled_ids[, .(facility_id, state)]

cat(sprintf("Total facilities in sample: %d\n", nrow(sampled_facility_keys)))

cat("Loading full dataset again to filter...\n")
full_panel_all_cols <- fread(here("Data", "Processed", "facility_leak_behavior_monthly.csv"))

setkey(full_panel_all_cols, facility_id, state)
setkey(sampled_facility_keys, facility_id, state)

panel_sample <- full_panel_all_cols[sampled_facility_keys, on = .(facility_id, state), nomatch = 0L]

# 5. Save the new sample file (OUTPUT 1)
output_path <- here("Data", "Processed", "facility_leak_behavior_monthly_SAMPLE.csv")
fwrite(panel_sample, output_path)

cat(sprintf("✓ Sample saved to: %s\n", output_path))
cat(sprintf("  - Total rows: %s\n", format(nrow(panel_sample), big.mark=",")))
cat(sprintf("  - Facilities with leaks: %s\n",
    format(uniqueN(panel_sample[leak_incident == 1]$facility_id), big.mark=",")))

cat("✓ SCRIPT 01 complete.\n\n")


# ==============================================================================
# SCRIPT 02: CREATE PRE-MERGED CLAIMS-PANEL SAMPLE (TASK 2)
# ==============================================================================
# Purpose: Create a single, pre-merged dataset by merging the FULL claims
#          data with the FULL panel data, saving ONLY the matches.
#          This is for testing the *other* script (e.g., 07_Script_B).
#
# Input:  all_cleaned_claims.csv (FULL dataset)
# Input:  facility_leak_behavior_monthly.csv (FULL dataset)
# Output: claims_panel_merged_matches_SAMPLE.csv (Pre-merged, matches only)
# ==============================================================================

cat("================================================================================\n")
cat("SCRIPT 02: CREATING PRE-MERGED MATCHED-CLAIMS (FROM FULL DATA)\n")
cat("================================================================================\n")

# 1. Load FULL claims data
cat("Loading full claims data...\n")
# Using the path from your successful debug run
claims_full <-  fread(here("Data", "Processed", "all_cleaned_claims.csv"))
cat(sprintf("  - Loaded %d total claims\n", nrow(claims_full)))

# 2. Load FULL panel data
cat("Loading FULL panel data (this may take a moment)...\n")
panel_full <- fread(here("Data", "Processed", "facility_leak_behavior_monthly.csv"))
cat(sprintf("  - Loaded %d facility-months in full panel\n", nrow(panel_full)))

# 3. Prepare claims data for merging (as in 07_Script_B)
#    Filter to valid costs FIRST to reduce merge size
claims_prepped <- claims_full[total_cost_2023 > 0 & !is.na(total_cost_2023)]
claims_prepped[, `:=`(
  claim_year = as.integer(claim_start_year),
  claim_month = as.integer(claim_start_month)
)]
cat(sprintf("  - Prepared %d claims with valid costs for merging\n", nrow(claims_prepped)))


# 4. *** FIX: MAP STATE ABBREVIATIONS TO FULL NAMES ***
cat("  - Mapping state abbreviations to full names for merge...\n")
state_name_map <- c(
  LA = "Louisiana",
  TN = "Tennessee",
  NM = "New Mexico",
  UT = "Utah",
  CO = "Colorado",
  WI = "Wisconsin",
  NC = "North Carolina"
)

# Convert the named vector to a data.table for joining
state_map_dt <- data.table(
  state_abbrev = names(state_name_map),
  state_full = state_name_map
)

# Perform an "update join"
# This looks up the abbreviation and puts the full name in the 'state' column
claims_prepped[state_map_dt, on = .(state = state_abbrev), state := i.state_full]

cat("  - State name mapping complete.\n")
cat("  - Example of mapped claims data:\n")
print(head(claims_prepped[, .(facility_id, state, claim_year, claim_month)]))


# 5. Prepare panel data for merging (as in 07_Script_B)
# (Was step 4, now step 5)
panel_prepped <- panel_full[, `:=`(
  panel_year = as.integer(panel_year),
  panel_month = as.integer(panel_month)
)]

# 6. Perform INNER JOIN (all.x = FALSE) to keep ONLY matches
# (Was step 5, now step 6)
cat("Performing inner join on FULL datasets to find all matches...\n")
cleanup_data_matches <- merge(
  claims_prepped,
  panel_prepped,
  by.x = c("facility_id", "state", "claim_year", "claim_month"),
  by.y = c("facility_id", "state", "panel_year", "panel_month"),
  all = FALSE # This performs an INNER JOIN
)

unique(cleanup_data_matches[, .(facility_id, state)]) # Show unique facilities matched
unique(claims_prepped[,.(facility_id, state )])
unique(panel_prepped[,.(facility_id, state )])

# 7. Save the new pre-merged sample file (OUTPUT 2)
# (Was step 6, now step 7)
output_merged_path <- here("Data", "Processed", "claims_panel_merged_matches_SAMPLE.csv")
fwrite(cleanup_data_matches, output_merged_path)

cat(sprintf("\n✓ Pre-merged sample (all matches from full data) saved to: %s\n", output_merged_path))
cat(sprintf("  - Total matched claim rows: %s\n", format(nrow(cleanup_data_matches), big.mark=",")))
cat("✓ SCRIPT 02 complete.\n\n")