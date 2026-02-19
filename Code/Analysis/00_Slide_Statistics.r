## Slide Statistics Calculator
## Purpose: Calculate placeholder values for presentation slides
## Input: Master_Harmonized_UST_Tanks.csv (from 04_Master_Build.R)

library(data.table)
library(here)
library(lubridate)

# Load processed master data
dt <- fread(here("Data", "Processed", "Master_Harmonized_UST_Tanks.csv"))

# Load claims data for cost statistics
claims <- fread(here("Data", "Processed", "all_cleaned_claims.csv"))

# ============================================================================
# STAT 1: Single-Walled Tank Prevalence (Motivation Slide)
# "Single-walled tanks (high-risk) persist: xx% of stock as of [YEAR?]"
# ============================================================================

# Calculate for multiple years to show persistence
dt[, installed_year := year(tank_installed_date)]
dt[, closed_year := year(tank_closed_date)]

# Function to get active tanks in a given year
get_active_tanks <- function(data, year) {
  data[
    installed_year <= year & 
    (is.na(closed_year) | closed_year > year)
  ]
}

# Function to calculate single-walled percentage by group
calc_single_walled <- function(tanks_data) {
  list(
    full_sample = tanks_data[, round(sum(single_walled == 1, na.rm = TRUE) / .N * 100, 1)],
    treatment_TX = tanks_data[state == "TX", round(sum(single_walled == 1, na.rm = TRUE) / .N * 100, 1)],
    control_tier1 = tanks_data[study_group == "2. Control Tier 1 (Custom)", 
                               round(sum(single_walled == 1, na.rm = TRUE) / .N * 100, 1)],
    control_tier2 = tanks_data[study_group == "3. Control Tier 2 (EPA Std)", 
                               round(sum(single_walled == 1, na.rm = TRUE) / .N * 100, 1)],
    control_combined = tanks_data[study_group %in% c("2. Control Tier 1 (Custom)", "3. Control Tier 2 (EPA Std)"), 
                                  round(sum(single_walled == 1, na.rm = TRUE) / .N * 100, 1)]
  )
}

# 1999: Year of TX policy shock
tanks_1999 <- get_active_tanks(dt, 1999)
stats_1999 <- calc_single_walled(tanks_1999)

# 2018: Recent data point (nearly 20 years post-policy)
tanks_2018 <- get_active_tanks(dt, 2018)
stats_2018 <- calc_single_walled(tanks_2018)

cat("\n=== STAT 1: Tank Wall Type Prevalence ===\n")
cat("\n--- 1999 (Pre-Policy) ---\n")
cat(sprintf("Full Sample: %.1f%% single-walled\n", stats_1999$full_sample))
cat(sprintf("  Treatment (TX): %.1f%%\n", stats_1999$treatment_TX))
cat(sprintf("  Control Tier 1: %.1f%%\n", stats_1999$control_tier1))
cat(sprintf("  Control Tier 2: %.1f%%\n", stats_1999$control_tier2))
cat(sprintf("  Control (Combined): %.1f%%\n", stats_1999$control_combined))

cat("\n--- 2018 (Post-Policy) ---\n")
cat(sprintf("Full Sample: %.1f%% single-walled\n", stats_2018$full_sample))
cat(sprintf("  Treatment (TX): %.1f%%\n", stats_2018$treatment_TX))
cat(sprintf("  Control Tier 1: %.1f%%\n", stats_2018$control_tier1))
cat(sprintf("  Control Tier 2: %.1f%%\n", stats_2018$control_tier2))
cat(sprintf("  Control (Combined): %.1f%%\n", stats_2018$control_combined))

cat("\n--- Change (1999-2018) ---\n")
cat(sprintf("Full Sample: %.1f pp reduction\n", stats_1999$full_sample - stats_2018$full_sample))
cat(sprintf("  Treatment (TX): %.1f pp reduction\n", stats_1999$treatment_TX - stats_2018$treatment_TX))
cat(sprintf("  Control (Combined): %.1f pp reduction\n", stats_1999$control_combined - stats_2018$control_combined))
cat(sprintf("  Diff-in-Diff: %.1f pp\n", 
            (stats_1999$treatment_TX - stats_2018$treatment_TX) - 
            (stats_1999$control_combined - stats_2018$control_combined)))


# ============================================================================
# STAT 2: Sample Size (Estimation Results Slide)
# "Sample: [Insert N] facilities observed quarterly (20XX-20XX)"
# ============================================================================

# Count unique facilities in analysis sample
# Define analysis sample: TX + Controls (exclude treated/excluded states)
analysis_states <- dt[study_group %in% c("1. Target (TX)", 
                                          "2. Control Tier 1 (Custom)", 
                                          "3. Control Tier 2 (EPA Std)")]

stat_n_facilities <- analysis_states[, uniqueN(facility_id)]

# Time period: Based on tank installation/closure dates
stat_year_start <- analysis_states[!is.na(tank_installed_date), min(year(tank_installed_date))]
stat_year_end <- analysis_states[!is.na(tank_closed_date), max(year(tank_closed_date))]

# Alternative: If you have panel data, this would be the observation period
# For now, use the range of tank lifecycle dates
stat_year_end_alt <- max(stat_year_end, 2024, na.rm = TRUE) # Assume data through ~2024

cat("\n=== STAT 2: Analysis Sample ===\n")
cat(sprintf("Sample: %s facilities observed (%d-%d)\n", 
            format(stat_n_facilities, big.mark = ","),
            stat_year_start, 
            stat_year_end_alt))

# Breakdown by state group
cat("\nBreakdown by Group:\n")
print(analysis_states[, .(N_Facilities = uniqueN(facility_id), 
                          N_Tanks = .N), 
                      by = study_group][order(study_group)])


# ============================================================================
# STAT 3: Additional Context Stats
# ============================================================================

cat("\n=== ADDITIONAL CONTEXT ===\n")

# Total tanks in TX (for "The Texas Experiment")
stat_tx_tanks <- dt[state == "TX", .N]
cat(sprintf("Total TX tanks in data: %s\n", format(stat_tx_tanks, big.mark = ",")))

# Average tank age in 1999
tanks_1999[, tank_age_1999 := 1999 - installed_year]
stat_avg_age_1999 <- tanks_1999[, round(mean(tank_age_1999, na.rm = TRUE), 1)]
cat(sprintf("Average tank age in 1999: %.1f years\n", stat_avg_age_1999))

# Closure rate post-policy (if you have closure dates)
tanks_post_policy <- dt[
  !is.na(closed_year) & 
  closed_year >= 1999 & 
  closed_year <= 2005 &
  state == "TX"
]
stat_tx_closures_post <- nrow(tanks_post_policy)
cat(sprintf("TX tank closures (1999-2005): %s\n", format(stat_tx_closures_post, big.mark = ",")))


# ============================================================================
# STAT 4: Median Cleanup Cost (Motivation Slide)
# "Median cleanup cost $> \$250{,}000$ per leak event"
# ============================================================================

cat("\n=== STAT 4: Cleanup Costs ===\n")

# Use inflation-adjusted costs (2023 dollars) when available, otherwise nominal
claims[, cost_for_analysis := ifelse(!is.na(total_cost_2023), total_cost_2023, total_cost)]

# Remove missing/zero costs
claims_valid <- claims[!is.na(cost_for_analysis) & cost_for_analysis > 0]

# Calculate median
stat_median_cost <- median(claims_valid$cost_for_analysis)

# Analytical approximation for SE of median (much faster than bootstrap)
# SE(median) ≈ 1.253 * SD / sqrt(n)  for normally distributed data
n <- nrow(claims_valid)
stat_median_cost_se <- 1.253 * sd(claims_valid$cost_for_analysis) / sqrt(n)

# 95% Confidence Interval using normal approximation
stat_median_cost_ci_lower <- stat_median_cost - 1.96 * stat_median_cost_se
stat_median_cost_ci_upper <- stat_median_cost + 1.96 * stat_median_cost_se

# Additional stats
stat_mean_cost <- mean(claims_valid$cost_for_analysis)
stat_n_claims <- nrow(claims_valid)

cat(sprintf("Median cleanup cost: $%s\n", format(round(stat_median_cost), big.mark = ",")))
cat(sprintf("  - Standard Error: $%s\n", format(round(stat_median_cost_se), big.mark = ",")))
cat(sprintf("  - 95%% CI: [$%s, $%s]\n", 
            format(round(stat_median_cost_ci_lower), big.mark = ","),
            format(round(stat_median_cost_ci_upper), big.mark = ",")))
cat(sprintf("  - Mean cost: $%s\n", format(round(stat_mean_cost), big.mark = ",")))
cat(sprintf("  - N claims: %s\n", format(stat_n_claims, big.mark = ",")))

# Check if > $250,000 for slide statement
stat_exceeds_250k <- stat_median_cost > 250000
cat(sprintf("\nSlide statement verification: Median > $250,000? %s\n", 
            ifelse(stat_exceeds_250k, "TRUE ✓", "FALSE (update slide)")))



# ============================================================================
# EXPORT: Create a text file with values for easy copy-paste
# ============================================================================

output_text <- sprintf("
==============================================
SLIDE PLACEHOLDER VALUES
Generated: %s
==============================================

MOTIVATION SLIDE (Environmental Problem):
  Median cleanup cost: $%s per leak event
    - Standard Error: $%s
    - 95%% CI: [$%s, $%s]
    - Mean cost: $%s
    - Based on %s claims
    - Exceeds $250K threshold: %s

MOTIVATION SLIDE (Technological Friction):
  
  === 1999 (Pre-Policy) ===
  Full Sample: %.1f%% single-walled
    - Treatment (TX): %.1f%%
    - Control Tier 1: %.1f%%
    - Control Tier 2: %.1f%%
    - Control (Combined): %.1f%%
  
  === 2018 (Post-Policy) ===
  Full Sample: %.1f%% single-walled
    - Treatment (TX): %.1f%%
    - Control Tier 1: %.1f%%
    - Control Tier 2: %.1f%%
    - Control (Combined): %.1f%%
  
  === Change (1999-2018) ===
  Full Sample: %.1f pp reduction
    - Treatment (TX): %.1f pp reduction
    - Control (Combined): %.1f pp reduction
    - Diff-in-Diff: %.1f pp
  
ESTIMATION RESULTS SLIDE (Sample Description):
  Sample: %s facilities observed (%d-%d)
  
ADDITIONAL STATS:
  - Total TX tanks: %s
  - Average tank age in 1999: %.1f years
  - TX closures post-policy (1999-2005): %s

==============================================
",
Sys.time(),
format(round(stat_median_cost), big.mark = ","),
format(round(stat_median_cost_se), big.mark = ","),
format(round(stat_median_cost_ci_lower), big.mark = ","),
format(round(stat_median_cost_ci_upper), big.mark = ","),
format(round(stat_mean_cost), big.mark = ","),
format(stat_n_claims, big.mark = ","),
ifelse(stat_exceeds_250k, "YES", "NO"),
stats_1999$full_sample, stats_1999$treatment_TX, stats_1999$control_tier1, 
stats_1999$control_tier2, stats_1999$control_combined,
stats_2018$full_sample, stats_2018$treatment_TX, stats_2018$control_tier1,
stats_2018$control_tier2, stats_2018$control_combined,
stats_1999$full_sample - stats_2018$full_sample,
stats_1999$treatment_TX - stats_2018$treatment_TX,
stats_1999$control_combined - stats_2018$control_combined,
(stats_1999$treatment_TX - stats_2018$treatment_TX) - (stats_1999$control_combined - stats_2018$control_combined),
format(stat_n_facilities, big.mark = ","), stat_year_start, stat_year_end_alt,
format(stat_tx_tanks, big.mark = ","),
stat_avg_age_1999,
format(stat_tx_closures_post, big.mark = ",")
)

# Save to file
writeLines(output_text, here("Output", "Slide_Stats.txt"))

cat("\n")
cat(output_text)
cat("\nStats saved to: Output/Slide_Stats.txt\n")


