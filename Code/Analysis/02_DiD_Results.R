#==============================================================================
# UNIVERSAL DiD ANALYSIS: Texas UST Insurance Reform
# Multi-Model Estimation with Mechanism Decomposition
#
# Location: Code/Analysis/02_DiD_Results.R
#
# This script unifies the Enhanced and Refactored analysis approaches:
#   Part A: Benchmarks (Naive 2WFE with Step-In Controls)
#   Part B: Survival & Selection Corrections (IPS Weighting)
#   Part C: Deaner & Ku (2024) Hazard Analysis
#   Part D: Mechanism Decompositions
#   Part E: Characterization & Heterogeneity
#   Part F: Robustness Models (Cloglog, Frailty)
#   Part G: Visualization
#
# Input:  Data/Processed/facility_leak_behavior_monthly.csv
# Output: Output/Tables/*.csv, *.tex, *.txt
#         Output/Figures/*.png
#
# References:
#   Deaner & Ku (2024): Causal Duration Analysis with Diff-in-Diff
#   Roodman et al. (2019): Fast and wild bootstrap inference
#   MacKinnon, Nielsen, Webb (2022): Cluster-robust inference
#
# REFACTORED: 2025-01-21
#   - Removed bootstrap_helper.R dependency
#   - Uses table_helper.R with Julia-based WCB (fwildclusterboot)
#   - Memory-efficient for 25M+ observations
#==============================================================================

#==============================================================================
# 1. SETUP & LOADING
#==============================================================================

# ----------------------------- BOOTSTRAP SETTINGS ----------------------------
# Master switch for bootstrap inference
# Set USE_BOOTSTRAP = FALSE for fast testing, TRUE for final production
USE_BOOTSTRAP <- FALSE   # <-- CHANGE TO TRUE FOR FINAL RUN
N_BOOTSTRAP   <- 100     # Bootstrap replications (999 for testing, 9999 for final)
# -----------------------------------------------------------------------------

# Load packages
library(data.table)
library(fixest)
library(survival)
library(coxme)
library(ggplot2)
library(kableExtra)
library(here)
library(broom)
library(sandwich)
library(lmtest)
library(scales)

options(scipen = 999)
set.seed(20250120 + 12345)  # Date-based seed convention
setDTthreads(14)

# Source table helper (contains all bootstrap + table functions)
source(here("Code", "Helpers", "table_helper.R"))

# Output paths
OUTPUT_TABLES <- here("Output", "Tables")
OUTPUT_FIGURES <- here("Output", "Figures")

# Create output directories
dir.create(OUTPUT_TABLES, recursive = TRUE, showWarnings = FALSE)
dir.create(OUTPUT_FIGURES, recursive = TRUE, showWarnings = FALSE)

# Custom theme for publication
theme_pub <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title = element_text(size = rel(1.1), face = "bold", margin = margin(0, 0, 10, 0)),
      plot.subtitle = element_text(size = rel(0.8), margin = margin(0, 0, 10, 0)),
      axis.title = element_text(face = "bold", size = rel(0.9)),
      legend.title = element_text(face = "bold", size = rel(0.9)),
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      panel.border = element_rect(fill = NA, color = "gray90"),
      strip.text = element_text(face = "bold")
    )
}
theme_set(theme_pub())

cat("====================================================================\n")
cat("UST Insurance Reform: Universal DiD Analysis\n")
cat(sprintf("Bootstrap: %s (B = %d)\n", 
            ifelse(USE_BOOTSTRAP, "ENABLED (Julia/WCB)", "DISABLED (HC2 fallback)"),
            N_BOOTSTRAP))
cat("====================================================================\n\n")

#==============================================================================
# 2. DATA LOADING & PREPARATION
#==============================================================================

cat("\n--- Loading Data ---\n")

# Load data
DATA_PATH <- here("Data", "Processed", "facility_leak_behavior_monthly.csv")

if (!file.exists(DATA_PATH)) {
  stop(sprintf("Data file not found: %s\nRun 10_Build_Master_Panel.R first.", DATA_PATH))
}

facility_data <- fread(DATA_PATH)
cat(sprintf("Loaded %s facility-months\n", format(nrow(facility_data), big.mark = ",")))

#------------------------------------------------------------------------------
# CONSTRAINT 1: Panel Window (1980-2023)
#------------------------------------------------------------------------------
cat("\n--- Applying Constraints ---\n")

facility_data <- facility_data[panel_year >= 1980 & panel_year <= 2023]
cat(sprintf("Constraint 1 (Window 1980-2023): %s rows\n", format(nrow(facility_data), big.mark = ",")))

#------------------------------------------------------------------------------
# CONSTRAINT 2: Zero-Duration Diagnostic
#------------------------------------------------------------------------------

zero_duration_ids <- facility_data[months_since_entry == 0 & active_tanks == 0, .(
  facility_id = unique(facility_id),
  state = unique(state)
)]

if (nrow(zero_duration_ids) > 0) {
  diag_file <- file.path(OUTPUT_TABLES, "Zero_Duration_Diagnostic.csv")
  fwrite(zero_duration_ids, diag_file)
  cat(sprintf("Found %d facilities with zero duration and no tanks. Diagnostic saved: %s\n",
              nrow(zero_duration_ids), basename(diag_file)))
}

#------------------------------------------------------------------------------
# CONSTRAINT 3: Variable Construction
#------------------------------------------------------------------------------

# Create identifiers
facility_data[, panel_id := paste0(state, "_", facility_id)]
facility_data[, year_month := paste0(panel_year, "_", sprintf("%02d", panel_month))]

# Create event_date if not present
if (!"event_date" %in% names(facility_data)) {
  if ("date" %in% names(facility_data)) {
    facility_data[, event_date := as.Date(date)]
  } else {
    facility_data[, event_date := as.Date(paste0(panel_year, "-", sprintf("%02d", panel_month), "-01"))]
  }
}

# Create install_year
facility_data[, install_year := panel_year - floor(avg_tank_age)]

# reg_vintage: Regulatory vintage (3 categories)
facility_data[, reg_vintage := fcase(
  install_year < 1988, "Pre-RCRA",
  install_year >= 1988 & install_year <= 1998, "Transition",
  install_year > 1998, "Post-Deadline",
  default = "Unknown"
)]

# age_bins: Age categories
if (!"age_bins" %in% names(facility_data) || all(is.na(facility_data$age_bins))) {
  facility_data[, age_bins := cut(
    avg_tank_age,
    breaks = c(0, 5, 10, 15, 20, 25, 30, Inf),
    labels = c("0-5", "6-10", "11-15", "16-20", "21-25", "26-30", "30+"),
    include.lowest = TRUE
  )]
}

# wall_type: Tank configuration
if (!"wall_type" %in% names(facility_data) || all(is.na(facility_data$wall_type))) {
  facility_data[, wall_type := fcase(
    has_single_walled > 0 & has_double_walled == 0, "Single-Walled",
    has_double_walled > 0 & has_single_walled == 0, "Double-Walled",
    has_single_walled > 0 & has_double_walled > 0, "Mixed",
    default = "Unknown"
  )]
}

# Treatment group labels
facility_data[, treatment_group := ifelse(texas_treated, "Texas", "Control")]
facility_data[, period := ifelse(post_1999, "Post-1999", "Pre-1999")]

# EPA deadline indicator
facility_data[, pre1998_install := as.integer(install_year < 1998)]

cat("Constraint 3: Variables constructed (reg_vintage, age_bins, wall_type, pre1998_install)\n")

# Define control states
control_states <- c(
  "Maine", "New Mexico", "Arkansas", "Oklahoma", "Louisiana",
  "Kansas", "Montana", "Idaho", "South Dakota", "Alabama",
  "Minnesota", "North Carolina", "Illinois", "Massachusetts",
  "Ohio", "Pennsylvania", "Tennessee", "Virginia"
)

# Filter to Texas + Control states, valid age
filtered_data <- facility_data[
  !is.na(avg_tank_age) &
    avg_tank_age >= 0 & avg_tank_age <= 50 &
    (state %in% control_states | state == "Texas")
]

cat(sprintf("After state/age filter: %s facility-months\n", format(nrow(filtered_data), big.mark = ",")))

#------------------------------------------------------------------------------
# CONSTRAINT 4: INCUMBENT FILTER (CRITICAL FOR DiD)
#------------------------------------------------------------------------------
cat("\n--- Applying Incumbent Filter ---\n")

n_before <- nrow(filtered_data)

# Identify Incumbents: Unique panel_ids active pre-1999
incumbent_ids <- unique(filtered_data[panel_year < 1999 & active_tanks > 0, panel_id])

cat(sprintf("Incumbent facilities (active pre-1999): %s\n", 
            format(length(incumbent_ids), big.mark = ",")))

# Keep only rows belonging to incumbent IDs
filtered_data <- filtered_data[panel_id %in% incumbent_ids]

n_dropped <- n_before - nrow(filtered_data)

cat(sprintf("Dropped %s rows from non-incumbent facilities\n", 
            format(n_dropped, big.mark = ",")))
cat(sprintf("Analysis sample (incumbents only): %s facility-months\n",
            format(nrow(filtered_data), big.mark = ",")))

# Final sample summary
cat(sprintf("\nFinal Analysis Sample:\n"))
cat(sprintf("  Total: %s facility-months\n", format(nrow(filtered_data), big.mark = ",")))
cat(sprintf("  Texas: %s\n", format(sum(filtered_data$texas_treated), big.mark = ",")))
cat(sprintf("  Control: %s\n", format(sum(!filtered_data$texas_treated), big.mark = ",")))
cat(sprintf("  Unique facilities: %s\n", format(uniqueN(filtered_data$panel_id), big.mark = ",")))

#==============================================================================
# PART A: BENCHMARKS (NAIVE 2WFE)
#==============================================================================

cat("\n")
cat("====================================================================\n")
cat("PART A: Naive 2WFE Benchmarks with Step-In Controls\n")
cat("====================================================================\n")
cat("\nNOTE: These models are BIASED for absorbing-state outcomes.\n")
cat("Reported as benchmark for comparison with corrected approaches.\n\n")

# Reference event date
ref_event_date <- as.Date("1998-12-01")

#------------------------------------------------------------------------------
# A.1: Naive Level Estimates (LUST, Exit, Retrofit) - Motor Fuel Only
#------------------------------------------------------------------------------
cat("--- A.1: Naive DiD on Levels (Motor Fuel Only) ---\n")

mf_data <- filtered_data[is_motor_fuel == TRUE]
cat(sprintf("Motor fuel sample: %s facility-months\n", format(nrow(mf_data), big.mark = ",")))
did_lust_naive <- feols(
  leak_incident ~ texas_treated:post_1999 | panel_id +  year_month,
  data = mf_data,
  cluster = "state",
  lean = TRUE,
  mem.clean = TRUE
)

did_exit_naive <- feols(
  exit_flag ~ texas_treated:post_1999 | panel_id + year_month,
  data = mf_data,
  cluster = "state",
  lean = TRUE,
  mem.clean = TRUE
)

did_retrofit_naive <- feols(
  replacement_event ~ texas_treated:post_1999 | panel_id + year_month,
  data = mf_data,
  cluster = "state",
  lean = TRUE,
  mem.clean = TRUE
)

# Save table using new helper
save_standard_did_table(
  models = list(did_lust_naive, did_exit_naive, did_retrofit_naive),
  headers = c("LUST", "Exit", "Retrofit"),
  base_name = "A1_Naive_2WFE_Benchmark",
  title = "Naive 2WFE DiD (Biased Benchmark, Motor Fuel Only)",
  treatment_var = "texas_treated:post_1999",
  cluster_var = "state",
  use_bootstrap = USE_BOOTSTRAP,
  n_reps = N_BOOTSTRAP,
  digits = 6
)



#------------------------------------------------------------------------------
# A.2: Step-In Control Specifications (LUST) - WITH TRIPLE INTERACTION
#------------------------------------------------------------------------------
cat("\n--- A.2: Step-In Controls with Motor Fuel Heterogeneity ---\n")

# A.2.1: Pooled (Benchmark)
did_lust_a21 <- feols(
  leak_incident ~ texas_treated * post_1999 | panel_id + year_month,
  data = filtered_data,
  cluster = "state",
  lean = TRUE,
  mem.clean = TRUE
)

# A.2.2: Baseline Triple Interaction
did_lust_a22 <- feols(
  leak_incident ~ texas_treated * post_1999 * is_motor_fuel | panel_id + year_month,
  data = filtered_data,
  cluster = "state",
  lean = TRUE,
  mem.clean = TRUE
)

# A.2.3: + Age Bins
did_lust_a23 <- feols(
  leak_incident ~ texas_treated * post_1999 * is_motor_fuel + age_bins | panel_id + year_month,
  data = filtered_data,
  cluster = "state",
  lean = TRUE,
  mem.clean = TRUE
)

# A.2.4: + EPA Deadline Controls
did_lust_a24 <- feols(
  leak_incident ~ texas_treated * post_1999 * is_motor_fuel + age_bins + pre1998_install * post_1999 | 
    panel_id + year_month,
  data = filtered_data,
  cluster = "state",
  lean = TRUE,
  mem.clean = TRUE
)

# A.2.5: + Regulatory Vintage
did_lust_a25 <- feols(
  leak_incident ~ texas_treated * post_1999 * is_motor_fuel + age_bins + reg_vintage | 
    panel_id + year_month,
  data = filtered_data,
  cluster = "state",
  lean = TRUE,
  mem.clean = TRUE
)

# Save triple interaction table using new helper
save_triple_did_table(
  pooled_model = did_lust_a21,
  triple_models = list(did_lust_a22, did_lust_a23, did_lust_a24, did_lust_a25),
  headers = c("Baseline", "MF HTE", "+Age", "+EPA Deadline", "+Reg Vintage"),
  base_name = "A2_LUST_StepIn_Results",
  title = "LUST DiD: Step-In Controls with MF Heterogeneity",
  base_term = "texas_treated:post_1999",
  interact_term = "texas_treated:post_1999:is_motor_fuelTRUE",
  cluster_var = "state",
  use_bootstrap = USE_BOOTSTRAP,
  n_reps = N_BOOTSTRAP,
  digits = 6
)

#==============================================================================
# PART B: SURVIVAL & SELECTION CORRECTIONS (IPS)
#==============================================================================

cat("\n")
cat("====================================================================\n")
cat("PART B: Selection-Corrected Analysis (IPS Weighting)\n")
cat("====================================================================\n")

#------------------------------------------------------------------------------
# B.1: Estimate Cox Model for Survival Weights
#------------------------------------------------------------------------------
cat("\n--- B.1: Estimating Survival Model for IPS Weights ---\n")

surv_data <- mf_data[, .(
  duration_months = max(months_since_entry, na.rm = TRUE),
  exit_event = max(exit_flag, na.rm = TRUE),
  avg_tank_age = mean(avg_tank_age, na.rm = TRUE),
  has_single_walled = max(has_single_walled, na.rm = TRUE),
  total_capacity = mean(total_capacity, na.rm = TRUE),
  texas_treated = max(texas_treated),
  state = first(state)
), by = panel_id]

surv_data <- surv_data[duration_months > 0 & !is.na(duration_months)]
cat(sprintf("Survival sample: %s facilities\n", format(nrow(surv_data), big.mark = ",")))

# Standardize covariates
surv_data[, `:=`(
  avg_tank_age_std = scale(avg_tank_age)[,1],
  log_capacity_std = scale(log(total_capacity + 1))[,1]
)]

# Cox model
cat("Estimating Cox model...\n")
surv_model <- tryCatch({
  coxph(
    Surv(duration_months, exit_event) ~ avg_tank_age_std + has_single_walled + 
      log_capacity_std + texas_treated,
    data = surv_data,
    control = coxph.control(iter.max = 50)
  )
}, error = function(e) {
  cat(sprintf("Cox model failed: %s\n", e$message))
  cat("Using simplified specification...\n")
  coxph(Surv(duration_months, exit_event) ~ avg_tank_age_std + texas_treated, 
        data = surv_data)
})

cat("Survival model estimated\n")
cat(sprintf("  Concordance: %.3f\n", surv_model$concordance["concordance"]))

#------------------------------------------------------------------------------
# B.2: Compute IPS Weights
#------------------------------------------------------------------------------
cat("\n--- B.2: Computing IPS Weights ---\n")

lp_lookup <- predict(surv_model, newdata = surv_data, type = "lp")
names(lp_lookup) <- surv_data$panel_id

mf_data[, surv_lp := lp_lookup[panel_id]]
mf_data[, ips_weight := exp(-surv_lp)]

# Stabilize weights
p1 <- quantile(mf_data$ips_weight, 0.01, na.rm = TRUE)
p99 <- quantile(mf_data$ips_weight, 0.99, na.rm = TRUE)
mf_data[, ips_weight := pmax(pmin(ips_weight, p99), p1)]
mf_data[is.na(ips_weight), ips_weight := 1]

cat(sprintf("IPS weights: median = %.3f, range = [%.3f, %.3f]\n",
            median(mf_data$ips_weight),
            min(mf_data$ips_weight),
            max(mf_data$ips_weight)))

#------------------------------------------------------------------------------
# B.3: IPS-Weighted Event Studies
#------------------------------------------------------------------------------
cat("\n--- B.3: IPS-Weighted Event Studies ---\n")

es_lust_ips <- feols(
  leak_incident ~ i(event_date, texas_treated, ref = ref_event_date) | panel_id + event_date,
  data = mf_data,
  weights = ~ips_weight,
  cluster = "state",
  lean = TRUE,
  mem.clean = TRUE
)

es_exit_ips <- feols(
  exit_flag ~ i(event_date, texas_treated, ref = ref_event_date) | panel_id + event_date,
  data = mf_data,
  weights = ~ips_weight,
  cluster = "state",
  lean = TRUE,
  mem.clean = TRUE
)

cat("IPS-weighted event studies estimated\n")

#==============================================================================
# PART C: DEANER & KU (2024) HAZARD ANALYSIS
#==============================================================================

cat("\n")
cat("====================================================================\n")
cat("PART C: Deaner & Ku (2024) Time-Average Hazard DiD\n")
cat("====================================================================\n")

epsilon <- 1e-8

#------------------------------------------------------------------------------
# C.1: Calculate Time-Average Hazards (H_bar)
#------------------------------------------------------------------------------
cat("\n--- C.1: Calculating Time-Average Hazards ---\n")

compute_hazard_data <- function(dt, outcome_var, group_vars = c("texas_treated", "year_month")) {
  haz_dt <- dt[, .(
    cumulative_rate = mean(get(outcome_var), na.rm = TRUE),
    n = .N
  ), by = group_vars]
  
  setkeyv(haz_dt, group_vars)
  haz_dt[, time_index := 1:.N, by = texas_treated]
  haz_dt[, baseline_rate := first(cumulative_rate), by = texas_treated]
  
  haz_dt[, H_bar := ifelse(
    time_index > 1,
    (1 / (time_index - 1)) * log((1 - baseline_rate + epsilon) / (1 - cumulative_rate + epsilon)),
    0
  )]
  
  haz_dt[, post_1999 := as.integer(year_month >= "1999_09")]
  
  return(haz_dt)
}

hazard_lust <- compute_hazard_data(mf_data, "leak_incident")
hazard_exit <- compute_hazard_data(mf_data, "exit_flag")
hazard_retrofit <- compute_hazard_data(mf_data, "replacement_event")
hazard_closure_detect <- compute_hazard_data(mf_data, "leak_found_by_closure")
hazard_operational <- compute_hazard_data(mf_data, "leak_not_found_by_exit")

cat("Time-average hazards calculated\n")

#------------------------------------------------------------------------------
# C.2: Main Deaner & Ku Regressions
#------------------------------------------------------------------------------
cat("\n--- C.2: Deaner & Ku DiD (Group-Level) ---\n")

did_dk_lust <- lm(H_bar ~ texas_treated * post_1999,
                  data = hazard_lust[time_index > 1],
                  weights = n)

did_dk_exit <- lm(H_bar ~ texas_treated * post_1999,
                  data = hazard_exit[time_index > 1],
                  weights = n)

did_dk_retrofit <- lm(H_bar ~ texas_treated * post_1999,
                      data = hazard_retrofit[time_index > 1],
                      weights = n)

dk_results <- data.table(
  Outcome = c("LUST", "Exit", "Retrofit"),
  Coefficient = c(
    coef(did_dk_lust)["texas_treated:post_1999"],
    coef(did_dk_exit)["texas_treated:post_1999"],
    coef(did_dk_retrofit)["texas_treated:post_1999"]
  ),
  SE = c(
    summary(did_dk_lust)$coefficients["texas_treated:post_1999", "Std. Error"],
    summary(did_dk_exit)$coefficients["texas_treated:post_1999", "Std. Error"],
    summary(did_dk_retrofit)$coefficients["texas_treated:post_1999", "Std. Error"]
  ),
  N_Cells = c(
    nrow(hazard_lust[time_index > 1]),
    nrow(hazard_exit[time_index > 1]),
    nrow(hazard_retrofit[time_index > 1])
  )
)

dk_results[, `:=`(
  t_stat = Coefficient / SE,
  p_val = 2 * pt(-abs(Coefficient / SE), df = N_Cells - 4),
  CI_Lower = Coefficient - 1.96 * SE,
  CI_Upper = Coefficient + 1.96 * SE
)]

# Save using simple CSV helper (lm models, optional bootstrap)
save_simple_csv_table(
  results_dt = dk_results,
  base_name = "C2_DeanerKu_Hazard_DiD",
  title = "Deaner & Ku (2024) Time-Average Hazard DiD",
  models = list(did_dk_lust, did_dk_exit, did_dk_retrofit),
  treatment_var = "texas_treated:post_1999",
  cluster_var = "state",
  use_bootstrap = USE_BOOTSTRAP,
  n_reps = N_BOOTSTRAP
)

#------------------------------------------------------------------------------
# C.3: Stratified by is_motor_fuel
#------------------------------------------------------------------------------
cat("\n--- C.3: Deaner & Ku Stratified by Motor Fuel ---\n")

hazard_lust_all <- compute_hazard_data(
  filtered_data,
  "leak_incident",
  group_vars = c("texas_treated", "year_month", "is_motor_fuel")
)

hazard_exit_all <- compute_hazard_data(
  filtered_data,
  "exit_flag",
  group_vars = c("texas_treated", "year_month", "is_motor_fuel")
)

did_dk_lust_strat <- lm(
  H_bar ~ texas_treated * post_1999 * is_motor_fuel,
  data = hazard_lust_all[time_index > 1],
  weights = n
)

did_dk_exit_strat <- lm(
  H_bar ~ texas_treated * post_1999 * is_motor_fuel,
  data = hazard_exit_all[time_index > 1],
  weights = n
)

dk_strat_results <- data.table(
  Outcome = c("LUST", "Exit"),
  Main_Effect = c(
    coef(did_dk_lust_strat)["texas_treated:post_1999"],
    coef(did_dk_exit_strat)["texas_treated:post_1999"]
  )
)

# Get triple interaction
triple_names <- c("texas_treated:post_1999:is_motor_fuelTRUE", 
                  "texas_treated:post_1999:is_motor_fuel")
for (tn in triple_names) {
  if (tn %in% names(coef(did_dk_lust_strat))) {
    dk_strat_results[, Triple_Interaction := c(
      coef(did_dk_lust_strat)[tn],
      coef(did_dk_exit_strat)[tn]
    )]
    break
  }
}

save_simple_csv_table(
  results_dt = dk_strat_results,
  base_name = "C3_DeanerKu_MotorFuel_Stratified",
  title = "Deaner & Ku Stratified by Motor Fuel"
)

#------------------------------------------------------------------------------
# C.4: Stratified by age_bins
#------------------------------------------------------------------------------
cat("\n--- C.4: Deaner & Ku Stratified by Age Bins ---\n")

hazard_lust_age <- compute_hazard_data(
  mf_data[!is.na(age_bins)],
  "leak_incident",
  group_vars = c("texas_treated", "year_month", "age_bins")
)

hazard_exit_age <- compute_hazard_data(
  mf_data[!is.na(age_bins)],
  "exit_flag",
  group_vars = c("texas_treated", "year_month", "age_bins")
)

did_dk_lust_age <- lm(
  H_bar ~ texas_treated * post_1999 * age_bins,
  data = hazard_lust_age[time_index > 1],
  weights = n
)

did_dk_exit_age <- lm(
  H_bar ~ texas_treated * post_1999 * age_bins,
  data = hazard_exit_age[time_index > 1],
  weights = n
)

dk_age_results <- data.table(
  Outcome = c("LUST", "Exit"),
  Main_Effect = c(
    coef(did_dk_lust_age)["texas_treated:post_1999"],
    coef(did_dk_exit_age)["texas_treated:post_1999"]
  )
)

age_bin_interactions <- grep("texas_treated:post_1999:age_bins", 
                             names(coef(did_dk_lust_age)), value = TRUE)
if (length(age_bin_interactions) > 0) {
  cat("\nAge Bin x Treatment Interactions (LUST):\n")
  for (abi in age_bin_interactions) {
    cat(sprintf("  %s: %.6f\n", abi, coef(did_dk_lust_age)[abi]))
  }
}

save_simple_csv_table(
  results_dt = dk_age_results,
  base_name = "C4_DeanerKu_AgeBin_Stratified",
  title = "Deaner & Ku Stratified by Age Bins"
)

#------------------------------------------------------------------------------
# C.5: Vintage Extension
#------------------------------------------------------------------------------
cat("\n--- C.5: Vintage-Based Hazard Analysis ---\n")

vintage_agg <- mf_data[, .(
  leak_rate = mean(leak_incident, na.rm = TRUE),
  exit_rate = mean(exit_flag, na.rm = TRUE),
  retrofit_rate = mean(replacement_event, na.rm = TRUE),
  n_obs = .N,
  n_facilities = uniqueN(panel_id)
), by = .(texas_treated, post_1999, reg_vintage)]

did_vintage_lust <- lm(leak_rate ~ texas_treated * post_1999 + reg_vintage,
                       data = vintage_agg,
                       weights = n_obs)

did_vintage_exit <- lm(exit_rate ~ texas_treated * post_1999 + reg_vintage,
                       data = vintage_agg,
                       weights = n_obs)

vintage_results <- data.table(
  Outcome = c("LUST", "Exit"),
  Coefficient = c(
    coef(did_vintage_lust)["texas_treated:post_1999"],
    coef(did_vintage_exit)["texas_treated:post_1999"]
  ),
  SE = c(
    summary(did_vintage_lust)$coefficients["texas_treated:post_1999", "Std. Error"],
    summary(did_vintage_exit)$coefficients["texas_treated:post_1999", "Std. Error"]
  )
)

save_simple_csv_table(
  results_dt = vintage_results,
  base_name = "C5_Vintage_Hazard_DiD",
  title = "Vintage-Based Hazard Analysis"
)

rm(vintage_agg)
gc()

#==============================================================================
# PART D: MECHANISM DECOMPOSITIONS
#==============================================================================

cat("\n")
cat("====================================================================\n")
cat("PART D: Mechanism Decompositions\n")
cat("====================================================================\n")

#------------------------------------------------------------------------------
# D.1: Exit vs Retrofit (Conditional on Closure)
#------------------------------------------------------------------------------
cat("\n--- D.1: Exit vs Retrofit (Conditional on Closure) ---\n")

mf_data[, any_closure := as.integer(tanks_closed > 0)]
closure_months <- mf_data[any_closure == 1]
cat(sprintf("Closure sample: %s facility-months\n", format(nrow(closure_months), big.mark = ",")))

closure_months[, `:=`(
  exit_no_retrofit = as.integer(exit_flag == 1 & replacement_event == 0),
  retrofit_no_exit = as.integer(replacement_event == 1 & exit_flag == 0),
  both_exit_retrofit = as.integer(exit_flag == 1 & replacement_event == 1),
  neither = as.integer(exit_flag == 0 & replacement_event == 0)
)]

did_exit_no_retrofit <- feols(
  exit_no_retrofit ~ texas_treated*post_1999 + age_bins + has_single_walled +
    active_tanks + total_capacity | panel_id + year_month,
  data = closure_months,
  cluster = "state",
  lean = TRUE,
  mem.clean = TRUE
)

did_retrofit_no_exit <- feols(
  retrofit_no_exit ~ texas_treated*post_1999 + age_bins + has_single_walled +
    active_tanks + total_capacity | panel_id + year_month,
  data = closure_months,
  cluster = "state",
  lean = TRUE,
  mem.clean = TRUE
)

did_both <- feols(
  both_exit_retrofit ~ texas_treated*post_1999 + age_bins + has_single_walled +
    active_tanks + total_capacity | panel_id + year_month,
  data = closure_months,
  cluster = "state",
  lean = TRUE,
  mem.clean = TRUE
)

did_neither <- feols(
  neither ~ texas_treated*post_1999 + age_bins + has_single_walled +
    active_tanks + total_capacity | panel_id + year_month,
  data = closure_months,
  cluster = "state",
  lean = TRUE,
  mem.clean = TRUE
)

save_standard_did_table(
  models = list(did_exit_no_retrofit, did_retrofit_no_exit, did_both, did_neither),
  headers = c("Exit (No Retrofit)", "Retrofit (No Exit)", "Both", "Neither"),
  base_name = "D1_Conditional_Closure_Behavior",
  title = "Conditional Closure Behavior: Exit vs Retrofit Decisions",
  treatment_var = "texas_treated:post_1999",
  cluster_var = "state",
  use_bootstrap = USE_BOOTSTRAP,
  n_reps = N_BOOTSTRAP
)

#------------------------------------------------------------------------------
# D.2: Exit-No-Leak vs Exit-With-Leak
#------------------------------------------------------------------------------
cat("\n--- D.2: Mechanism Decomposition (Exit-No-LUST) ---\n")

mf_data[, exit_no_lust := as.integer(exit_flag == 1 & leak_incident == 0)]
mf_data[, exit_with_lust := as.integer(exit_flag == 1 & leak_incident == 1)]

did_exit_no_lust <- feols(
  exit_no_lust ~ texas_treated*post_1999 | panel_id + year_month,
  data = mf_data,
  cluster = "state",
  lean = TRUE,
  mem.clean = TRUE
)

did_exit_with_lust <- feols(
  exit_with_lust ~ texas_treated*post_1999 | panel_id + year_month,
  data = mf_data,
  cluster = "state",
  lean = TRUE,
  mem.clean = TRUE
)

save_standard_did_table(
  models = list(did_lust_naive, did_exit_naive, did_exit_no_lust, did_exit_with_lust),
  headers = c("LUST", "Exit (All)", "Exit (No LUST)", "Exit (With LUST)"),
  base_name = "D2_Mechanism_Decomposition",
  title = "Mechanism Decomposition: LUST and Exit Channels",
  treatment_var = "texas_treated:post_1999",
  cluster_var = "state",
  use_bootstrap = USE_BOOTSTRAP,
  n_reps = N_BOOTSTRAP,
  digits = 6
)

# Create interpretation summary
decomp_summary <- data.table(
  Channel = c("LUST Detection", "Exit (All)", "Exit without LUST", "Exit with LUST"),
  Coefficient = c(
    coef(did_lust_naive)["texas_treated:post_1999"],
    coef(did_exit_naive)["texas_treated:post_1999"],
    coef(did_exit_no_lust)["texas_treated:post_1999"],
    coef(did_exit_with_lust)["texas_treated:post_1999"]
  )
)

decomp_summary[, Interpretation := fcase(
  Channel == "LUST Detection" & Coefficient < 0, "Policy reduces leak detection",
  Channel == "Exit without LUST" & Coefficient > 0, "Compositional change (risky exit)",
  Channel == "Exit with LUST" & Coefficient < 0, "Behavioral improvement",
  default = "Check significance"
)]

save_summary_table(
  results_dt = decomp_summary,
  base_name = "D2_Mechanism_Decomposition_Summary",
  title = "Mechanism Decomposition Interpretation",
  notes = c(
    "Coefficients from Texas x Post-1999 interaction",
    "Sign interpretation depends on policy mechanism"
  )
)

#------------------------------------------------------------------------------
# D.3: LUST Detection Mechanism
#------------------------------------------------------------------------------
cat("\n--- D.3: LUST Detection Mechanism ---\n")

did_lust_closure_detect <- feols(
  leak_found_by_closure ~ texas_treated*post_1999 + age_bins + has_single_walled |
    panel_id + year_month,
  data = mf_data,
  cluster = "state",
  lean = TRUE,
  mem.clean = TRUE
)

did_lust_operational <- feols(
  leak_not_found_by_exit ~ texas_treated*post_1999 + age_bins + has_single_walled |
    panel_id + year_month,
  data = mf_data,
  cluster = "state",
  lean = TRUE,
  mem.clean = TRUE
)

save_standard_did_table(
  models = list(did_lust_closure_detect, did_lust_operational),
  headers = c("Closure-Detected", "Operational"),
  base_name = "D3_LUST_Detection_Mechanism",
  title = "LUST Detection Mechanism: Closure vs Operational Discovery",
  treatment_var = "texas_treated:post_1999",
  cluster_var = "state",
  use_bootstrap = USE_BOOTSTRAP,
  n_reps = N_BOOTSTRAP,
  digits = 6
)

#==============================================================================
# PART E: CHARACTERIZATION & HETEROGENEITY
#==============================================================================

cat("\n")
cat("====================================================================\n")
cat("PART E: Characterization & Heterogeneity\n")
cat("====================================================================\n")

#------------------------------------------------------------------------------
# E.1: Detailed Retrofit Characterization
#------------------------------------------------------------------------------
cat("\n--- E.1: Retrofit Characterization ---\n")

retrofit_months <- mf_data[replacement_event == 1]
cat(sprintf("Retrofit sample: %s facility-months\n", format(nrow(retrofit_months), big.mark = ",")))

retrofit_months[, `:=`(
  retrofit_is_double_wall = as.integer(double_walled_installed > 0),
  capacity_change_pct = 100 * capacity_change / (capacity_closed + 1),
  net_tank_change = tanks_installed - tanks_closed,
  tank_count_increased = as.integer(tanks_installed > tanks_closed),
  tank_count_decreased = as.integer(tanks_installed < tanks_closed),
  equal_replacement = as.integer(tanks_installed == tanks_closed)
)]

did_dw_upgrade <- feols(
  retrofit_is_double_wall ~ texas_treated*post_1999 + age_bins + has_single_walled +
    active_tanks | panel_id + year_month,
  data = retrofit_months,
  cluster = "state",
  lean = TRUE,
  mem.clean = TRUE
)

did_capacity_change <- feols(
  capacity_change ~ texas_treated*post_1999 + age_bins + has_single_walled +
    active_tanks | panel_id + year_month,
  data = retrofit_months,
  cluster = "state",
  lean = TRUE,
  mem.clean = TRUE
)

did_capacity_increase <- feols(
  capacity_increased ~ texas_treated*post_1999 + age_bins + has_single_walled +
    active_tanks | panel_id + year_month,
  data = retrofit_months,
  cluster = "state",
  lean = TRUE,
  mem.clean = TRUE
)

did_capacity_decrease <- feols(
  capacity_decreased ~ texas_treated*post_1999 + age_bins + has_single_walled +
    active_tanks | panel_id + year_month,
  data = retrofit_months,
  cluster = "state",
  lean = TRUE,
  mem.clean = TRUE
)

did_net_tank_change <- feols(
  net_tank_change ~ texas_treated*post_1999 + age_bins + has_single_walled +
    active_tanks | panel_id + year_month,
  data = retrofit_months,
  cluster = "state",
  lean = TRUE,
  mem.clean = TRUE
)

save_standard_did_table(
  models = list(did_dw_upgrade, did_capacity_change, did_capacity_increase,
                did_capacity_decrease, did_net_tank_change),
  headers = c("DW Upgrade", "Capacity Delta", "Cap Up", "Cap Down", "Tank Delta"),
  base_name = "E1_Retrofit_Characterization",
  title = "Retrofit Characterization: Technology and Scale Adjustments",
  treatment_var = "texas_treated:post_1999",
  cluster_var = "state",
  use_bootstrap = USE_BOOTSTRAP,
  n_reps = N_BOOTSTRAP
)

#------------------------------------------------------------------------------
# E.2: Age Bin x Policy Interactions (HTE)
#------------------------------------------------------------------------------
cat("\n--- E.2: Age Bin x Policy Interactions (HTE) ---\n")

did_exit_age_hte <- feols(
  exit_flag ~ texas_treated*post_1999*age_bins | panel_id + year_month,
  data = mf_data,
  cluster = "state",
  lean = TRUE,
  mem.clean = TRUE
)

did_retrofit_age_hte <- feols(
  replacement_event ~ texas_treated*post_1999*age_bins | panel_id + year_month,
  data = mf_data,
  cluster = "state",
  lean = TRUE,
  mem.clean = TRUE
)

did_lust_age_hte <- feols(
  leak_incident ~ texas_treated*post_1999*age_bins | panel_id + year_month,
  data = mf_data,
  cluster = "state",
  lean = TRUE,
  mem.clean = TRUE
)

# Extract coefficients
lust_age_coefs <- coef(did_lust_age_hte)
lust_age_se <- se(did_lust_age_hte)
age_hte_names <- grep("texas_treated:post_1999:age_bins", names(lust_age_coefs), value = TRUE)

if (length(age_hte_names) > 0) {
  age_hte_results <- data.table(
    Age_Bin = gsub("texas_treated:post_1999:age_bins", "", age_hte_names),
    LUST_Coef = lust_age_coefs[age_hte_names],
    LUST_SE = lust_age_se[age_hte_names],
    Exit_Coef = coef(did_exit_age_hte)[age_hte_names],
    Exit_SE = se(did_exit_age_hte)[age_hte_names],
    Retrofit_Coef = coef(did_retrofit_age_hte)[age_hte_names],
    Retrofit_SE = se(did_retrofit_age_hte)[age_hte_names]
  )
  
  save_simple_csv_table(
    results_dt = age_hte_results,
    base_name = "E2_AgeBin_HTE",
    title = "Age Bin Heterogeneous Treatment Effects"
  )
}

#------------------------------------------------------------------------------
# E.3: Parallel Gradient Tests
#------------------------------------------------------------------------------
cat("\n--- E.3: Parallel Gradient Tests ---\n")

pooled_test_exit <- feols(
  exit_flag ~ texas_treated*age_bins | year_month,
  data = mf_data[wall_type == "Single-Walled" & post_1999 == 1],
  cluster = "state",
  lean = TRUE,
  mem.clean = TRUE
)

pooled_test_retro <- feols(
  replacement_event ~ texas_treated*age_bins | year_month,
  data = mf_data[wall_type == "Single-Walled" & post_1999 == 1],
  cluster = "state",
  lean = TRUE,
  mem.clean = TRUE
)

cat("\nJoint F-test for age bin x treatment interactions:\n")

exit_interact_names <- grep("texas_treated:age_bins", names(coef(pooled_test_exit)), value = TRUE)
retro_interact_names <- grep("texas_treated:age_bins", names(coef(pooled_test_retro)), value = TRUE)

if (length(exit_interact_names) > 0) {
  exit_joint_test <- tryCatch({
    wald(pooled_test_exit, exit_interact_names)
  }, error = function(e) {
    list(p = NA, stat = NA)
  })
  
  retro_joint_test <- tryCatch({
    wald(pooled_test_retro, retro_interact_names)
  }, error = function(e) {
    list(p = NA, stat = NA)
  })
  
  gradient_results <- data.table(
    Outcome = c("Exit", "Retrofit"),
    Joint_F_Stat = c(
      ifelse(is.list(exit_joint_test), exit_joint_test$stat, NA),
      ifelse(is.list(retro_joint_test), retro_joint_test$stat, NA)
    ),
    Joint_P = c(
      ifelse(is.list(exit_joint_test), exit_joint_test$p, NA),
      ifelse(is.list(retro_joint_test), retro_joint_test$p, NA)
    )
  )
  
  gradient_results[, Parallel_Gradients := Joint_P > 0.10]
  
  save_simple_csv_table(
    results_dt = gradient_results,
    base_name = "E3_Parallel_Gradient_Test",
    title = "Parallel Gradient Tests"
  )
  
  # Decision rule
  cat("\n=== Decision Rule for DCM Specification ===\n")
  if (all(gradient_results$Parallel_Gradients, na.rm = TRUE)) {
    cat("PARALLEL age gradients (Joint p > 0.10 for both outcomes)\n")
    cat("  -> DCM baseline specification is appropriate\n")
  } else {
    cat("NON-PARALLEL age gradients (Joint p < 0.10 for at least one outcome)\n")
    cat("  -> DCM should include age_bins x policy interaction\n")
  }
}

#==============================================================================
# PART F: ROBUSTNESS MODELS
#==============================================================================

cat("\n")
cat("====================================================================\n")
cat("PART F: Robustness Models\n")
cat("====================================================================\n")

#------------------------------------------------------------------------------
# F.1: Discrete Time Hazard (Cloglog)
#------------------------------------------------------------------------------
cat("\n--- F.1: Cloglog Hazard Models ---\n")

did_cloglog_lust <- feglm(
  leak_incident ~ texas_treated*post_1999 + age_bins | year_month,
  data = mf_data,
  family = binomial(link = "cloglog"),
  cluster = "state"
)

did_cloglog_exit <- feglm(
  exit_flag ~ texas_treated*post_1999 + age_bins | year_month,
  data = mf_data,
  family = binomial(link = "cloglog"),
  cluster = "state"
)

cat("Cloglog models estimated\n")

cloglog_results <- data.table(
  Outcome = c("LUST", "Exit"),
  Coefficient = c(
    coef(did_cloglog_lust)["texas_treated:post_1999"],
    coef(did_cloglog_exit)["texas_treated:post_1999"]
  ),
  SE = c(
    se(did_cloglog_lust)["texas_treated:post_1999"],
    se(did_cloglog_exit)["texas_treated:post_1999"]
  )
)

cloglog_results[, `:=`(
  Hazard_Ratio = exp(Coefficient),
  CI_Lower = exp(Coefficient - 1.96 * SE),
  CI_Upper = exp(Coefficient + 1.96 * SE)
)]

save_simple_csv_table(
  results_dt = cloglog_results,
  base_name = "F1_Cloglog_Hazard_DiD",
  title = "Cloglog Discrete Hazard DiD",
  models = list(did_cloglog_lust, did_cloglog_exit),
  treatment_var = "texas_treated:post_1999",
  cluster_var = "state",
  use_bootstrap = USE_BOOTSTRAP,
  n_reps = N_BOOTSTRAP
)

#------------------------------------------------------------------------------
# F.2: Frailty Models
#------------------------------------------------------------------------------
cat("\n--- F.2: Frailty Models (State-Level Random Effect via coxme) ---\n")

cat("Estimating Exit frailty model (coxme with state RE)...\n")
frailty_exit <- tryCatch({
  coxme(
    Surv(duration_months, exit_event) ~ texas_treated + 
      avg_tank_age_std + has_single_walled +
      (1 | state),
    data = surv_data
  )
}, error = function(e) {
  cat(sprintf("Exit frailty model failed: %s\n", e$message))
  NULL
})

if (!is.null(frailty_exit)) {
  frailty_results <- data.table(
    Outcome = "Exit",
    Coefficient = fixef(frailty_exit)["texas_treated"],
    Hazard_Ratio = exp(fixef(frailty_exit)["texas_treated"]),
    Frailty_Variance = VarCorr(frailty_exit)$state[1],
    Converged = TRUE
  )
  
  save_simple_csv_table(
    results_dt = frailty_results,
    base_name = "F2_Frailty_Hazard_DiD",
    title = "Frailty Hazard DiD (State-Level RE)"
  )
  
  cat(sprintf("  State-level frailty variance: %.4f\n", VarCorr(frailty_exit)$state[1]))
} else {
  cat("Frailty model did not converge. Skipping.\n")
}

#==============================================================================
# PART G: VISUALIZATION
#==============================================================================

cat("\n")
cat("====================================================================\n")
cat("PART G: Visualization\n")
cat("====================================================================\n")

#------------------------------------------------------------------------------
# G.1: Event Study Plots with WCB CIs
#------------------------------------------------------------------------------
cat("\n--- G.1: Event Study Plots ---\n")

# Save event study tables (will compute WCB if enabled)
es_lust_dt <- save_event_study_table(
  model = es_lust_ips,
  base_name = "G1_EventStudy_LUST",
  title = "LUST Event Study (IPS-Weighted)",
  outcome_label = "LUST (IPS-Weighted)",
  ref_date = ref_event_date,
  cluster_var = "state",
  use_bootstrap = USE_BOOTSTRAP,
  n_reps = N_BOOTSTRAP
)

es_exit_dt <- save_event_study_table(
  model = es_exit_ips,
  base_name = "G1_EventStudy_Exit",
  title = "Exit Event Study (IPS-Weighted)",
  outcome_label = "Exit (IPS-Weighted)",
  ref_date = ref_event_date,
  cluster_var = "state",
  use_bootstrap = USE_BOOTSTRAP,
  n_reps = N_BOOTSTRAP
)

# Combine for plotting
if (!is.null(es_lust_dt) && !is.null(es_exit_dt)) {
  es_plot_data <- rbindlist(list(es_lust_dt, es_exit_dt), fill = TRUE)
  
  if (nrow(es_plot_data) > 0) {
    es_plot <- ggplot(es_plot_data, aes(x = event_date, y = estimate, color = Outcome)) +
      geom_vline(xintercept = as.Date("1999-01-01"), linetype = "dashed", color = "gray30") +
      geom_hline(yintercept = 0, color = "black", size = 0.3) +
      geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = Outcome), alpha = 0.15) +
      geom_line(size = 1) +
      geom_point(size = 1.5) +
      scale_color_manual(values = c("LUST (IPS-Weighted)" = "#D55E00", "Exit (IPS-Weighted)" = "#0072B2")) +
      scale_fill_manual(values = c("LUST (IPS-Weighted)" = "#D55E00", "Exit (IPS-Weighted)" = "#0072B2")) +
      facet_wrap(~Outcome, scales = "free_y", ncol = 1) +
      labs(
        title = "Event Study: IPS-Weighted DiD Estimates",
        subtitle = sprintf("95%% %s Confidence Intervals", 
                           ifelse(USE_BOOTSTRAP, "Wild Cluster Bootstrap", "Clustered SE")),
        x = "Calendar Time",
        y = "Treatment Effect",
        caption = "Vertical line: January 1999 policy change"
      ) +
      theme_pub() +
      theme(legend.position = "none")
    
    ggsave(file.path(OUTPUT_FIGURES, "G1_Event_Study_IPS_WCB.png"),
           es_plot, width = 10, height = 8, dpi = 300, bg = "white")
    cat("Saved: G1_Event_Study_IPS_WCB.png\n")
  }
}

#------------------------------------------------------------------------------
# G.2: Hazard Trend Plots
#------------------------------------------------------------------------------
cat("\n--- G.2: Hazard Trend Plots ---\n")

hazard_plot_data <- rbindlist(list(
  hazard_lust[, .(
    date = as.Date(paste0(gsub("_", "-", year_month), "-01")),
    Group = ifelse(texas_treated, "Texas", "Control"),
    hazard = H_bar,
    outcome = "LUST"
  )],
  hazard_exit[, .(
    date = as.Date(paste0(gsub("_", "-", year_month), "-01")),
    Group = ifelse(texas_treated, "Texas", "Control"),
    hazard = H_bar,
    outcome = "Exit"
  )],
  hazard_retrofit[, .(
    date = as.Date(paste0(gsub("_", "-", year_month), "-01")),
    Group = ifelse(texas_treated, "Texas", "Control"),
    hazard = H_bar,
    outcome = "Retrofit"
  )]
), use.names = TRUE)

hazard_trends <- ggplot(hazard_plot_data, aes(x = date, y = hazard, color = Group)) +
  geom_line(size = 1.2) +
  geom_vline(xintercept = as.Date("1999-01-01"), linetype = "dashed", color = "black") +
  facet_wrap(~outcome, scales = "free_y", ncol = 1) +
  scale_color_manual(values = c("Texas" = "#D55E00", "Control" = "#0072B2")) +
  labs(
    title = "Time-Average Hazard Rates by Outcome",
    subtitle = "Deaner & Ku (2024) transformation",
    x = "Calendar Time",
    y = "Time-Average Hazard Rate",
    caption = "Vertical line: January 1999 policy change"
  ) +
  theme_pub()

ggsave(file.path(OUTPUT_FIGURES, "G2_Hazard_Trends.png"),
       hazard_trends, width = 12, height = 10, dpi = 300, bg = "white")
cat("Saved: G2_Hazard_Trends.png\n")

#------------------------------------------------------------------------------
# G.3: Age Bin HTE Coefficient Plot
#------------------------------------------------------------------------------
cat("\n--- G.3: Age Bin HTE Coefficient Plot ---\n")

if (exists("age_hte_results") && nrow(age_hte_results) > 0) {
  age_hte_long <- melt(
    age_hte_results,
    id.vars = "Age_Bin",
    measure.vars = patterns("_Coef$", "_SE$"),
    variable.name = "Outcome",
    value.name = c("Estimate", "SE")
  )
  
  age_hte_long[, Outcome := fcase(
    Outcome == 1, "LUST",
    Outcome == 2, "Exit",
    Outcome == 3, "Retrofit"
  )]
  
  age_hte_long[, `:=`(
    CI_Lower = Estimate - 1.96 * SE,
    CI_Upper = Estimate + 1.96 * SE
  )]
  
  age_hte_plot <- ggplot(age_hte_long, aes(x = Age_Bin, y = Estimate, color = Outcome)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), 
                  width = 0.2, position = position_dodge(0.5)) +
    geom_point(size = 3, position = position_dodge(0.5)) +
    scale_color_manual(values = c("LUST" = "#D55E00", "Exit" = "#0072B2", "Retrofit" = "#009E73")) +
    labs(
      title = "Heterogeneous Treatment Effects by Age Bin",
      subtitle = "Texas x Post-1999 x Age Bin Interaction Coefficients",
      x = "Tank Age Bin (years)",
      y = "Treatment Effect (relative to 0-5 bin)",
      caption = "Reference category: 0-5 years. Error bars: 95% CI."
    ) +
    theme_pub() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  ggsave(file.path(OUTPUT_FIGURES, "G3_AgeBin_HTE_Plot.png"),
         age_hte_plot, width = 10, height = 6, dpi = 300, bg = "white")
  cat("Saved: G3_AgeBin_HTE_Plot.png\n")
}

#------------------------------------------------------------------------------
# G.4: Retrofit Coefficient Plot
#------------------------------------------------------------------------------
cat("\n--- G.4: Retrofit Coefficient Plot ---\n")

retrofit_coefs <- data.table(
  outcome = c("Double-Wall", "Capacity Up", "Capacity Down", "Net Tank Delta"),
  estimate = c(
    coef(did_dw_upgrade)["texas_treated:post_1999"],
    coef(did_capacity_increase)["texas_treated:post_1999"],
    coef(did_capacity_decrease)["texas_treated:post_1999"],
    coef(did_net_tank_change)["texas_treated:post_1999"]
  ),
  se = c(
    se(did_dw_upgrade)["texas_treated:post_1999"],
    se(did_capacity_increase)["texas_treated:post_1999"],
    se(did_capacity_decrease)["texas_treated:post_1999"],
    se(did_net_tank_change)["texas_treated:post_1999"]
  )
)

retrofit_coefs[, `:=`(
  lower = estimate - 1.96 * se,
  upper = estimate + 1.96 * se,
  t_stat = estimate / se
)]

retrofit_coefs[, significant := abs(t_stat) > 1.96]

retrofit_coef_plot <- ggplot(retrofit_coefs, aes(x = outcome, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, color = "gray40") +
  geom_point(aes(color = significant), size = 4) +
  scale_color_manual(values = c("TRUE" = "#D55E00", "FALSE" = "gray50"),
                     labels = c("TRUE" = "p < 0.05", "FALSE" = "p >= 0.05")) +
  labs(
    title = "Retrofit Characterization: Treatment Effects",
    subtitle = "Texas x Post-1999 Coefficients (95% CI)",
    x = NULL,
    y = "Treatment Effect",
    color = "Significance"
  ) +
  theme_pub() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(file.path(OUTPUT_FIGURES, "G4_Retrofit_Coefficient_Plot.png"),
       retrofit_coef_plot, width = 8, height = 6, dpi = 300, bg = "white")
cat("Saved: G4_Retrofit_Coefficient_Plot.png\n")

#==============================================================================
# FINAL SUMMARY
#==============================================================================

cat("\n")
cat("====================================================================\n")
cat("ANALYSIS COMPLETE\n")
cat("====================================================================\n\n")

cat("Tables Created:\n")
cat(sprintf("  Output directory: %s\n\n", OUTPUT_TABLES))

cat("  Part A (Benchmarks):\n")
cat("    - A1_Naive_2WFE_Benchmark.csv/.tex/.txt\n")
cat("    - A2_LUST_StepIn_Results.csv/.tex/.txt\n")
cat("      (Baseline -> MF HTE -> +Age -> +EPA Deadline -> +Reg Vintage)\n\n")

cat("  Part C (Deaner & Ku):\n")
cat("    - C2_DeanerKu_Hazard_DiD.csv\n")
cat("    - C3_DeanerKu_MotorFuel_Stratified.csv\n")
cat("    - C4_DeanerKu_AgeBin_Stratified.csv\n")
cat("    - C5_Vintage_Hazard_DiD.csv\n\n")

cat("  Part D (Mechanisms):\n")
cat("    - D1_Conditional_Closure_Behavior.csv/.tex/.txt\n")
cat("    - D2_Mechanism_Decomposition.csv/.tex/.txt\n")
cat("    - D2_Mechanism_Decomposition_Summary.csv/.txt\n")
cat("    - D3_LUST_Detection_Mechanism.csv/.tex/.txt\n\n")

cat("  Part E (Heterogeneity):\n")
cat("    - E1_Retrofit_Characterization.csv/.tex/.txt\n")
cat("    - E2_AgeBin_HTE.csv\n")
cat("    - E3_Parallel_Gradient_Test.csv\n\n")

cat("  Part F (Robustness):\n")
cat("    - F1_Cloglog_Hazard_DiD.csv\n")
cat("    - F2_Frailty_Hazard_DiD.csv\n\n")

cat("  Part G (Event Studies):\n")
cat("    - G1_EventStudy_LUST_event_study.csv\n")
cat("    - G1_EventStudy_Exit_event_study.csv\n\n")

cat("Figures Created:\n")
cat(sprintf("  Output directory: %s\n\n", OUTPUT_FIGURES))
cat("    - G1_Event_Study_IPS_WCB.png\n")
cat("    - G2_Hazard_Trends.png\n")
cat("    - G3_AgeBin_HTE_Plot.png\n")
cat("    - G4_Retrofit_Coefficient_Plot.png\n\n")

cat("====================================================================\n")
cat("METHODOLOGICAL NOTES\n")
cat("====================================================================\n")
cat("
1. INCUMBENT FILTER: Only facilities active pre-1999 included (DiD requirement)

2. Part A Step-In Controls:
   A.2.1: Baseline (full sample, pooled)
   A.2.2: Motor fuel HTE (TX x Post x MF triple interaction)
   A.2.3: + Age bin controls
   A.2.4: + EPA deadline interaction (pre1998_install x post_1999)
   A.2.5: + Regulatory vintage (3 categories)

3. Part B: IPS weighting via Cox model (coxph with standardized covariates)

4. Part C: Deaner & Ku (2024) time-average hazard transformation

5. Part E: All HTE analysis uses age_bins (categorical)
   - E.2: Texas x Post x Age_Bins triple interaction
   - E.3: Joint F-test for parallel gradients

6. Part F: Frailty uses coxme (state-level RE)

7. All standard errors clustered at state level (19 clusters)

8. Bootstrap: Julia-based WCB via fwildclusterboot (when enabled)
   - Engine: WildBootTests.jl
   - Weights: Webb 6-point (optimal for G < 20)
   - Fallback: HC2 clustered SEs with t(G-1) inference
\n")

cat(sprintf("Bootstrap: %s (B = %d)\n", 
            ifelse(USE_BOOTSTRAP, "ENABLED", "DISABLED"),
            N_BOOTSTRAP))

if (!USE_BOOTSTRAP) {
  cat("NOTE: Bootstrap disabled. Set USE_BOOTSTRAP = TRUE for final run.\n")
}

cat("\n====================================================================\n")