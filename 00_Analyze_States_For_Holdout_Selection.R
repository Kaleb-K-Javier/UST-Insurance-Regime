

################################################################################
# STATE ANALYSIS FOR HOLDOUT SET SELECTION
################################################################################
#
# PURPOSE:
#   Analyze state characteristics to recommend defensible holdout sets
#   for multi-fold cross-validation at state level
#
# OUTPUT:
#   - State characteristics table
#   - Recommended holdout sets
#   - Diagnostic plots
#   - Balance checks
#
# USAGE:
#   Rscript 00_Analyze_States_For_Holdout_Selection.R
#
################################################################################

library(data.table)
library(here)
library(ggplot2)
library(cluster)
library(factoextra)
library(scales)

cat("\n")
cat(rep("=", 80), "\n", sep = "")
cat("STATE ANALYSIS FOR HOLDOUT SET SELECTION\n")
cat(rep("=", 80), "\n\n", sep = "")

# ==============================================================================
# CONFIGURATION
# ==============================================================================

CONFIG <- list(
  running_locally = FALSE,  # Set TRUE if using sample data
  
  # How many holdout sets to create?
  n_folds = 4,
  
  # Target: each fold should have ~20-25% of data
  target_test_pct = 0.20,
  
  # Output directory
  dir_output = here("Output", "Holdout_Analysis")
)

dir.create(CONFIG$dir_output, showWarnings = FALSE, recursive = TRUE)

# ==============================================================================
# LOAD DATA
# ==============================================================================

cat("Loading data...\n")

if (CONFIG$running_locally) {
  panel_path <- here("Data", "Processed", "From_Server", 
                     "facility_leak_behavior_monthly_SAMPLE.csv")
  cost_path <- here("Data", "Processed", "From_Server", 
                    "claims_panel_merged_matches_SAMPLE.csv")
} else {
  panel_path <- here("Data", "Processed", "facility_leak_behavior_monthly.csv")
  cost_path <- here("Data", "Processed", "claims_panel_merged_matches_SAMPLE.csv")
}

panel <- fread(panel_path)
cost_data <- fread(cost_path)

cat(sprintf("  - Panel: %s observations from %s facilities\n",
            format(nrow(panel), big.mark = ","),
            format(uniqueN(panel$facility_id), big.mark = ",")))
cat(sprintf("  - Cost data: %s claims\n\n", 
            format(nrow(cost_data), big.mark = ",")))

# ==============================================================================
# SCRIPT A: ANALYZE STATES FOR LEAK HAZARD
# ==============================================================================

cat(rep("=", 80), "\n", sep = "")
cat("SCRIPT A: LEAK HAZARD STATE ANALYSIS\n")
cat(rep("=", 80), "\n\n", sep = "")

# --- [UPDATED] Create policy regime variable ---
# Based on: Texas (1999), Michigan (1995-2015), Florida (1999),
# Iowa (2001), West Virginia (2001), Delaware (2012), Connecticut (2015)
panel[, rho := fcase(
  state == "Texas" & panel_year >= 1999, "RB",
  state == "Michigan" & panel_year >= 1995 & panel_year < 2016, "RB",
  state == "Florida" & panel_year >= 1999, "RB",
  state == "Iowa" & panel_year >= 2001, "RB",
  state == "West Virginia" & panel_year >= 2001, "RB",
  state == "Delaware" & panel_year >= 2012, "RB",
  state == "Connecticut" & panel_year >= 2015, "RB",
  default = "FF"
)]
# --- [END UPDATE] ---

# State-level summary
state_summary_A <- panel[, .(
  n_facilities = uniqueN(facility_id),
  n_observations = .N,
  pct_of_data = .N / nrow(panel) * 100,
  
  # Leak characteristics
  leak_rate = mean(leak_incident, na.rm = TRUE) * 100,
  n_leaks = sum(leak_incident, na.rm = TRUE),
  
  # Facility characteristics
  avg_tank_age = mean(avg_tank_age, na.rm = TRUE),
  avg_active_tanks = mean(active_tanks, na.rm = TRUE),
  pct_double_walled = mean(has_double_walled > 0, na.rm = TRUE) * 100,
  avg_capacity = mean(total_capacity, na.rm = TRUE),
  
  # Policy regime
  pct_months_RB = mean(rho == "RB") * 100,
  policy_regime = fcase(
    mean(rho == "RB") > 0.5, "Mostly RB",
    mean(rho == "RB") > 0, "Mixed",
    default = "All FF"
  )
), by = state][order(-n_facilities)]

cat("STATE CHARACTERISTICS (Script A):\n\n")
print(state_summary_A, nrows = 50)

# Save
fwrite(state_summary_A, here(CONFIG$dir_output, "state_characteristics_leak.csv"))
cat("\n✓ Saved: state_characteristics_leak.csv\n\n")

# Geographic regions (you may need to adjust)
region_map <- data.table(
  state = c("California", "Oregon", "Washington", "Nevada", "Arizona",
            "Texas", "Oklahoma", "Louisiana", "Arkansas", "New Mexico",
            "Florida", "Georgia", "Alabama", "Mississippi", "South Carolina", "North Carolina",
            "Tennessee", "Kentucky", "Virginia", "West Virginia",
            "Ohio", "Michigan", "Indiana", "Illinois", "Wisconsin", "Minnesota", "Iowa", "Missouri",
            "Pennsylvania", "New York", "New Jersey", "Delaware", "Maryland",
            "Massachusetts", "Connecticut", "Rhode Island", "Vermont", "New Hampshire", "Maine",
            "Kansas", "Nebraska", "South Dakota", "North Dakota", "Montana", "Wyoming", "Colorado", "Utah", "Idaho"),
  region = c(rep("West", 5),
             rep("South_Central", 5),
             rep("South_East", 6),
             rep("South", 4),
             rep("Midwest", 8),
             rep("Northeast", 5),
             rep("Northeast", 7),
             rep("Great_Plains", 9))
)

state_summary_A <- merge(state_summary_A, region_map, by = "state", all.x = TRUE)
state_summary_A[is.na(region), region := "Other"]

# Clustering for balanced folds
cat("Clustering states for balanced holdout sets...\n")

cluster_vars <- c("leak_rate", "avg_tank_age", "avg_active_tanks", 
                  "pct_double_walled", "pct_months_RB")

# Prepare for clustering
cluster_data_A <- state_summary_A[, .SD, .SDcols = c("state", cluster_vars)]
cluster_data_A <- cluster_data_A[complete.cases(cluster_data_A)]

# Scale
cluster_matrix_A <- scale(cluster_data_A[, .SD, .SDcols = cluster_vars])
rownames(cluster_matrix_A) <- cluster_data_A$state

# Cluster into n_folds groups
set.seed(42)
kmeans_result_A <- kmeans(cluster_matrix_A, centers = CONFIG$n_folds, nstart = 25)

state_summary_A[state %in% cluster_data_A$state, 
                cluster := kmeans_result_A$cluster[match(state, cluster_data_A$state)]]

cat(sprintf("  - Assigned states to %d clusters\n\n", CONFIG$n_folds))

# Print cluster summary
cat("CLUSTER SUMMARY:\n")
for (i in 1:CONFIG$n_folds) {
  states_in_cluster <- state_summary_A[cluster == i, state]
  cluster_n_facilities <- state_summary_A[cluster == i, sum(n_facilities)]
  cluster_pct <- state_summary_A[cluster == i, sum(pct_of_data)]
  cluster_policy <- state_summary_A[cluster == i, unique(policy_regime)]
  
  cat(sprintf("\nCluster %d (%d states, %.1f%% of data):\n", 
              i, length(states_in_cluster), cluster_pct))
  cat(sprintf("  States: %s\n", paste(states_in_cluster, collapse = ", ")))
  cat(sprintf("  Facilities: %s\n", format(cluster_n_facilities, big.mark = ",")))
  cat(sprintf("  Policy regimes: %s\n", paste(unique(cluster_policy), collapse = ", ")))
}

# ==============================================================================
# RECOMMEND HOLDOUT SETS (Script A)
# ==============================================================================

cat("\n")
cat(rep("=", 80), "\n", sep = "")
cat("RECOMMENDED HOLDOUT SETS (Script A)\n")
cat(rep("=", 80), "\n\n", sep = "")

# Strategy: Pick 1-2 large states per cluster to create balanced folds
recommended_sets_A <- list()

for (fold in 1:CONFIG$n_folds) {
  # For each fold, pick states from OTHER clusters
  other_clusters <- setdiff(1:CONFIG$n_folds, fold)
  
  # Pick 2 largest states from the "fold" cluster as holdout
  holdout_states <- state_summary_A[cluster == fold][order(-n_facilities)][1:2, state]
  holdout_states <- holdout_states[!is.na(holdout_states)]
  
  # Calculate statistics
  holdout_pct <- state_summary_A[state %in% holdout_states, sum(pct_of_data)]
  holdout_policy <- state_summary_A[state %in% holdout_states, unique(policy_regime)]
  holdout_regions <- state_summary_A[state %in% holdout_states, unique(region)]
  
  recommended_sets_A[[paste0("fold_", fold)]] <- list(
    states = holdout_states,
    pct_data = holdout_pct,
    policy_regimes = holdout_policy,
    regions = holdout_regions
  )
  
  cat(sprintf("FOLD %d (%.1f%% in test):\n", fold, holdout_pct))
  cat(sprintf("  Holdout: %s\n", paste(holdout_states, collapse = ", ")))
  cat(sprintf("  Regions: %s\n", paste(holdout_regions, collapse = ", ")))
  cat(sprintf("  Policy: %s\n\n", paste(holdout_policy, collapse = ", ")))
}

# ==============================================================================
# SCRIPT B: ANALYZE STATES FOR CLEANUP COSTS
# ==============================================================================

cat(rep("=", 80), "\n", sep = "")
cat("SCRIPT B: CLEANUP COST STATE ANALYSIS\n")
cat(rep("=", 80), "\n\n", sep = "")

# Which states report costs?
cost_by_state <- cost_data[!is.na(total_cost_2023), .(
  n_claims = .N,
  pct_of_claims = .N / nrow(cost_data[!is.na(total_cost_2023)]) * 100,
  
  # Cost characteristics
  median_cost = median(total_cost_2023),
  mean_cost = mean(total_cost_2023),
  sd_cost = sd(total_cost_2023),
  min_cost = min(total_cost_2023),
  max_cost = max(total_cost_2023),
  
  # Facility characteristics
  avg_age = mean(avg_tank_age, na.rm = TRUE),
  avg_tanks = mean(active_tanks, na.rm = TRUE),
  pct_double = mean(has_double_walled > 0, na.rm = TRUE) * 100
), by = state][order(-n_claims)]

cat("STATES REPORTING CLEANUP COSTS:\n\n")
print(cost_by_state)

# Add regions
cost_by_state <- merge(cost_by_state, region_map, by = "state", all.x = TRUE)
cost_by_state[is.na(region), region := "Other"]

# Save
fwrite(cost_by_state, here(CONFIG$dir_output, "state_characteristics_cost.csv"))
cat("\n✓ Saved: state_characteristics_cost.csv\n\n")

# Clustering (only among cost-reporting states)
if (nrow(cost_by_state) >= CONFIG$n_folds) {
  cat("Clustering cost-reporting states...\n")
  
  cluster_vars_B <- c("median_cost", "avg_age", "avg_tanks", "pct_double")
  
  cluster_data_B <- cost_by_state[, .SD, .SDcols = c("state", cluster_vars_B)]
  cluster_data_B <- cluster_data_B[complete.cases(cluster_data_B)]
  
  cluster_matrix_B <- scale(cluster_data_B[, .SD, .SDcols = cluster_vars_B])
  rownames(cluster_matrix_B) <- cluster_data_B$state
  
  n_folds_B <- min(CONFIG$n_folds, nrow(cluster_data_B))
  
  set.seed(42)
  kmeans_result_B <- kmeans(cluster_matrix_B, centers = n_folds_B, nstart = 25)
  
  cost_by_state[state %in% cluster_data_B$state,
                cluster := kmeans_result_B$cluster[match(state, cluster_data_B$state)]]
  
  cat(sprintf("  - Assigned %d states to %d clusters\n\n", 
              nrow(cluster_data_B), n_folds_B))
  
} else {
  cat("⚠ WARNING: Fewer cost-reporting states than desired folds!\n")
  cat(sprintf("  Only %d states report costs (wanted %d folds)\n\n", 
              nrow(cost_by_state), CONFIG$n_folds))
  n_folds_B <- nrow(cost_by_state)
  cost_by_state[, cluster := .I]
}

# ==============================================================================
# RECOMMEND HOLDOUT SETS (Script B)
# ==============================================================================

cat(rep("=", 80), "\n", sep = "")
cat("RECOMMENDED HOLDOUT SETS (Script B)\n")
cat(rep("=", 80), "\n\n", sep = "")

if (nrow(cost_by_state) < 2) {
  cat("⚠ ERROR: Only 1 state reports costs - cannot create holdout sets!\n\n")
  recommended_sets_B <- NULL
} else {
  recommended_sets_B <- list()
  
  for (fold in 1:n_folds_B) {
    # Pick 1 state from this cluster
    holdout_states <- cost_by_state[cluster == fold][order(-n_claims)][1, state]
    
    # If not enough states, skip
    if (is.na(holdout_states)) next
    
    # Calculate statistics
    holdout_pct <- cost_by_state[state %in% holdout_states, sum(pct_of_claims)]
    holdout_region <- cost_by_state[state %in% holdout_states, region]
    
    recommended_sets_B[[paste0("fold_", fold)]] <- list(
      states = holdout_states,
      pct_data = holdout_pct,
      regions = holdout_region
    )
    
    cat(sprintf("FOLD %d (%.1f%% of cost claims in test):\n", fold, holdout_pct))
    cat(sprintf("  Holdout: %s\n", paste(holdout_states, collapse = ", ")))
    cat(sprintf("  Region: %s\n\n", holdout_region))
  }
}

# ==============================================================================
# CREATE DIAGNOSTIC PLOTS
# ==============================================================================

cat(rep("=", 80), "\n", sep = "")
cat("CREATING DIAGNOSTIC PLOTS\n")
cat(rep("=", 80), "\n\n", sep = "")

# Plot 1: State sizes and leak rates (Script A)
p1 <- ggplot(state_summary_A, aes(x = n_facilities, y = leak_rate, 
                                   color = policy_regime, label = state)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_text(hjust = -0.1, vjust = 0, size = 3) +
  scale_x_log10(labels = comma) +
  labs(title = "Script A: State Characteristics",
       subtitle = "Size vs. Leak Rate by Policy Regime",
       x = "Number of Facilities (log scale)",
       y = "Leak Rate (%)",
       color = "Policy Regime") +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave(here(CONFIG$dir_output, "state_characteristics_A.png"), 
       p1, width = 12, height = 8)
cat("✓ Saved: state_characteristics_A.png\n")

# Plot 2: Cluster visualization (Script A)
if (!is.null(state_summary_A$cluster)) {
  p2 <- fviz_cluster(list(data = cluster_matrix_A, cluster = kmeans_result_A$cluster),
                     geom = "point", ellipse.type = "norm",
                     palette = "jco", ggtheme = theme_minimal(),
                     main = "Script A: State Clustering for Balanced Folds")
  
  ggsave(here(CONFIG$dir_output, "state_clusters_A.png"), 
         p2, width = 10, height = 8)
  cat("✓ Saved: state_clusters_A.png\n")
}

# Plot 3: Cost-reporting states (Script B)
if (nrow(cost_by_state) > 0) {
  p3 <- ggplot(cost_by_state, aes(x = reorder(state, n_claims), y = n_claims,
                                   fill = region)) +
    geom_col() +
    geom_text(aes(label = n_claims), hjust = -0.2, size = 3) +
    coord_flip() +
    labs(title = "Script B: States Reporting Cleanup Costs",
         x = "State", y = "Number of Claims", fill = "Region") +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  ggsave(here(CONFIG$dir_output, "cost_reporting_states_B.png"), 
         p3, width = 10, height = 8)
  cat("✓ Saved: cost_reporting_states_B.png\n")
}

# ==============================================================================
# SAVE RECOMMENDED SETS
# ==============================================================================

cat("\n")
cat(rep("=", 80), "\n", sep = "")
cat("SAVING RECOMMENDED HOLDOUT SETS\n")
cat(rep("=", 80), "\n\n", sep = "")

# Save as R object
holdout_recommendations <- list(
  script_A = list(
    state_summary = state_summary_A,
    recommended_sets = recommended_sets_A,
    n_folds = CONFIG$n_folds
  ),
  script_B = list(
    state_summary = cost_by_state,
    recommended_sets = recommended_sets_B,
    n_folds = n_folds_B
  ),
  analysis_date = Sys.time()
)

saveRDS(holdout_recommendations, 
        here(CONFIG$dir_output, "recommended_holdout_sets.rds"))

cat("✓ Saved: recommended_holdout_sets.rds\n")

# Also save as text for easy reading
sink(here(CONFIG$dir_output, "recommended_holdout_sets.txt"))

cat("RECOMMENDED HOLDOUT SETS\n")
cat("Generated:", as.character(Sys.time()), "\n\n")

cat(rep("=", 80), "\n")
cat("SCRIPT A: LEAK HAZARD\n")
cat(rep("=", 80), "\n\n")

cat("Use this configuration in your script:\n\n")
cat("holdout_sets <- list(\n")
for (i in seq_along(recommended_sets_A)) {
  fold_name <- names(recommended_sets_A)[i]
  states <- recommended_sets_A[[i]]$states
  cat(sprintf('  %s = c("%s")', fold_name, paste(states, collapse = '", "')))
  if (i < length(recommended_sets_A)) cat(",")
  cat("\n")
}
cat(")\n\n")

cat(rep("=", 80), "\n")
cat("SCRIPT B: CLEANUP COSTS\n")
cat(rep("=", 80), "\n\n")

if (!is.null(recommended_sets_B) && length(recommended_sets_B) > 0) {
  cat("Use this configuration in your script:\n\n")
  cat("holdout_sets <- list(\n")
  for (i in seq_along(recommended_sets_B)) {
    fold_name <- names(recommended_sets_B)[i]
    states <- recommended_sets_B[[i]]$states
    cat(sprintf('  %s = c("%s")', fold_name, paste(states, collapse = '", "')))
    if (i < length(recommended_sets_B)) cat(",")
    cat("\n")
  }
  cat(")\n\n")
} else {
  cat("⚠ Insufficient cost-reporting states for multi-fold validation\n\n")
}

sink()

cat("✓ Saved: recommended_holdout_sets.txt\n\n")

# ==============================================================================
# FINAL SUMMARY
# ==============================================================================

cat(rep("=", 80), "\n", sep = "")
cat("ANALYSIS COMPLETE\n")
cat(rep("=", 80), "\n\n", sep = "")

cat("Output files created in:", CONFIG$dir_output, "\n\n")

cat("Files:\n")
cat("  1. state_characteristics_leak.csv - Full state summary for Script A\n")
cat("  2. state_characteristics_cost.csv - Cost-reporting states for Script B\n")
cat("  3. recommended_holdout_sets.rds - Recommended sets (R object)\n")
cat("  4. recommended_holdout_sets.txt - Recommended sets (human-readable)\n")
cat("  5. state_characteristics_A.png - Visualization for Script A\n")
cat("  6. state_clusters_A.png - Clustering visualization\n")
cat("  7. cost_reporting_states_B.png - Cost-reporting states\n\n")

cat("NEXT STEPS:\n")
cat("  1. Review recommended_holdout_sets.txt\n")
cat("  2. Examine diagnostic plots\n")
cat("  3. Copy holdout_sets list into your Scripts A & B\n")
cat("  4. Run multi-fold validation\n\n")

cat("For Script A: %d folds recommended\n", CONFIG$n_folds)
if (!is.null(recommended_sets_B)) {
  cat("For Script B: %d folds available\n\n", length(recommended_sets_B))
} else {
  cat("For Script B: Limited folds due to sparse cost data\n\n")
}

cat(rep("=", 80), "\n")