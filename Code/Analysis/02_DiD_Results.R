#==============================================================================
# UNIVERSAL DiD ANALYSIS: Texas UST Insurance Reform
# Multi-Model Estimation with Mechanism Decomposition
#
# Location: Code/Analysis/02_DiD_Universal_Analysis.R
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
#==============================================================================

#==============================================================================
# 1. SETUP & LOADING
#==============================================================================

# ----------------------------- BOOTSTRAP SETTINGS ----------------------------
# IMPORTANT: Set B = 100 for testing, B = 9999 for final production run
# Change this value before running final analysis!
B_BOOTSTRAP <- 100  # <-- CHANGE TO 9999 FOR FINAL RUN
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

# Source helper functions
source(here("Code", "Helpers", "bootstrap_helper.R"))
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
cat(sprintf("Bootstrap replications: B = %d\n", B_BOOTSTRAP))
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
# Duration variable from panel script: months_since_entry (Section 9, line 2254)
#------------------------------------------------------------------------------

# Identify zero-duration rows for diagnostic
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

# reg_vintage: Regulatory vintage (3 categories - avoids collinearity with age_bins)
facility_data[, reg_vintage := fcase(
  install_year < 1988, "Pre-RCRA",
  install_year >= 1988 & install_year <= 1998, "Transition",
  install_year > 1998, "Post-Deadline",
  default = "Unknown"
)]

# age_bins: Age categories (use existing from panel script Section 9 if available)
if (!"age_bins" %in% names(facility_data) || all(is.na(facility_data$age_bins))) {
  facility_data[, age_bins := cut(
    avg_tank_age,
    breaks = c(0, 5, 10, 15, 20, 25, 30, Inf),
    labels = c("0-5", "6-10", "11-15", "16-20", "21-25", "26-30", "30+"),
    include.lowest = TRUE
  )]
}

# wall_type: Tank configuration (use existing from panel script Section 9 if available)
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

# EPA deadline indicator for step-in controls
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



# CONSTRAINT 4: INCUMBENT FILTER (CRITICAL FOR DiD)
# Facilities must have active tanks BEFORE treatment (pre-1999)
#------------------------------------------------------------------------------
cat("\n--- Applying Incumbent Filter ---\n")

# Store initial count for diagnostics
n_before <- nrow(filtered_data)

# 1. Identify Incumbents: Unique panel_ids active pre-1999
incumbent_ids <- unique(filtered_data[panel_year < 1999 & active_tanks > 0, panel_id])

cat(sprintf("Incumbent facilities (active pre-1999): %s\n", 
            format(length(incumbent_ids), big.mark = ",")))

# 2. Filter Direct: Keep only rows belonging to incumbent IDs
filtered_data <- filtered_data[panel_id %in% incumbent_ids]

# Diagnostics
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
  leak_incident ~ texas_treated*post_1999 | panel_id + year_month,
  data = mf_data,
  cluster = "state"
)

did_exit_naive <- feols(
  exit_flag ~ texas_treated*post_1999 | panel_id + year_month,
  data = mf_data,
  cluster = "state"
)

did_retrofit_naive <- feols(
  replacement_event ~ texas_treated*post_1999 | panel_id + year_month,
  data = mf_data,
  cluster = "state"
)

cat("Running WCB for naive models...\n")
wcb_a1_lust <- fast_wild_bootstrap(did_lust_naive, "texas_treated:post_1999", B = B_BOOTSTRAP)
wcb_a1_exit <- fast_wild_bootstrap(did_exit_naive, "texas_treated:post_1999", B = B_BOOTSTRAP)
wcb_a1_retro <- fast_wild_bootstrap(did_retrofit_naive, "texas_treated:post_1999", B = B_BOOTSTRAP)

save_universal_table(
  models = list(did_lust_naive, did_exit_naive, did_retrofit_naive),
  wcb_results = list(wcb_a1_lust, wcb_a1_exit, wcb_a1_retro),
  headers = c("LUST", "Exit", "Retrofit"),
  base_name = "A1_Naive_2WFE_Benchmark",
  title = "Naive 2WFE DiD (Biased Benchmark, Motor Fuel Only)",
  digits = 6
)


# ==============================================================================
# PART A.2: Step-In Control Specifications (LUST) - WITH TRIPLE INTERACTION
# ==============================================================================

#------------------------------------------------------------------------------
# A.2: Step-In Control Specifications (LUST)
# A.2.1: Baseline (full sample)
# A.2.2: Motor fuel HTE (triple interaction)
# A.2.3: + Age bin controls
# A.2.4: + EPA deadline interaction
# A.2.5: + Regulatory vintage (3 categories, avoids collinearity with age)
#------------------------------------------------------------------------------
cat("\n--- A.2: Step-In Controls with Motor Fuel Heterogeneity ---\n")

# ------------------------------------------------------------------------------
# 1. Estimate Models
# ------------------------------------------------------------------------------
# A.2.1: Pooled (Benchmark)
did_lust_a21 <- feols(
  leak_incident ~ texas_treated * post_1999 | panel_id + year_month,
  data = filtered_data,
  cluster = "state"
)

# A.2.2: Baseline Triple Interaction (Base Spec)
did_lust_a22 <- feols(
  leak_incident ~ texas_treated * post_1999 * is_motor_fuel | panel_id + year_month,
  data = filtered_data,
  cluster = "state"
)

# A.2.3: + Age Bins (Triple Interaction)
did_lust_a23 <- feols(
  leak_incident ~ texas_treated * post_1999 * is_motor_fuel + age_bins | panel_id + year_month,
  data = filtered_data,
  cluster = "state"
)

# A.2.4: + EPA Deadline Controls (Triple Interaction)
# Note: pre1998_install * post_1999 captures the deadline shock effect
did_lust_a24 <- feols(
  leak_incident ~ texas_treated * post_1999 * is_motor_fuel + age_bins + pre1998_install * post_1999 | 
                  panel_id + year_month,
  data = filtered_data,
  cluster = "state"
)

# A.2.5: + Regulatory Vintage (Triple Interaction)
did_lust_a25 <- feols(
  leak_incident ~ texas_treated * post_1999 * is_motor_fuel + age_bins + reg_vintage | 
                  panel_id + year_month,
  data = filtered_data,
  cluster = "state"
)

# ------------------------------------------------------------------------------
# 2. Run Wild Cluster Bootstraps
# ------------------------------------------------------------------------------
cat("Running WCB for A.2 models...\n")

# A.2.1: Standard Bootstrap (Pooled)
wcb_a21 <- fast_wild_bootstrap(did_lust_a21, "texas_treated:post_1999", B = B_BOOTSTRAP)

# Define coef names for triple interaction models
# Check your data: if is_motor_fuel is 0/1 numeric, remove "TRUE". If logical, keep "TRUE".
base_term <- "texas_treated:post_1999"
interact_term <- "texas_treated:post_1999:is_motor_fuelTRUE" 

# Helper wrapper to run triple bootstrap safely
run_triple_boot <- function(model) {
  fast_wild_bootstrap_triple(
    model, 
    base_coef = base_term, 
    interaction_coef = interact_term, 
    B = B_BOOTSTRAP
  )
}

wcb_a22 <- run_triple_boot(did_lust_a22)
wcb_a23 <- run_triple_boot(did_lust_a23)
wcb_a24 <- run_triple_boot(did_lust_a24)
wcb_a25 <- run_triple_boot(did_lust_a25)

# ------------------------------------------------------------------------------
# 3. Build the Table
# ------------------------------------------------------------------------------
# Helper to format "Coef (p-val)"
fmt_res <- function(beta, p) {
  stars <- ifelse(p < 0.01, "***", ifelse(p < 0.05, "**", ifelse(p < 0.1, "*", "")))
  sprintf("%.6f%s (%.4f)", beta, stars, p)
}

# Initialize data table
a2_table <- data.table(
  Effect = c(
    "Pooled Effect",
    "Non-Motor Fuel Effect (beta1)", 
    "MF Differential (beta3)", 
    "Motor Fuel Effect (beta1 + beta3)"
  )
)

# List of results to iterate over
results_list <- list(
  "Baseline" = list(type = "pooled", wcb = wcb_a21),
  "MF HTE" = list(type = "triple", wcb = wcb_a22),
  "+Age" = list(type = "triple", wcb = wcb_a23),
  "+EPA Deadline" = list(type = "triple", wcb = wcb_a24),
  "+Reg Vintage" = list(type = "triple", wcb = wcb_a25)
)

# Populate columns
for (model_name in names(results_list)) {
  res <- results_list[[model_name]]
  
  if (res$type == "pooled") {
    # Pooled Model: Only fills first row
    col_vals <- c(
      fmt_res(res$wcb$beta_obs, res$wcb$p_val),
      "--",
      "--",
      "--"
    )
  } else {
    # Triple Model: Fills bottom 3 rows
    w <- res$wcb
    col_vals <- c(
      "--",
      fmt_res(w$base$beta_obs, w$base$p_val),
      fmt_res(w$interact$beta_obs, w$interact$p_val),
      fmt_res(w$sum$beta_obs, w$sum$p_val)
    )
  }
  
  a2_table[, (model_name) := col_vals]
}

# ------------------------------------------------------------------------------
# 4. Save Output
# ------------------------------------------------------------------------------
print(a2_table)

fwrite(a2_table, file.path(OUTPUT_TABLES, "A2_LUST_StepIn_Results.csv"))

# Optional: Create a LaTeX version
kbl(a2_table, format = "latex", booktabs = TRUE, caption = "LUST DiD: Step-In Controls with MF Heterogeneity") %>%
  save_kable(file.path(OUTPUT_TABLES, "A2_LUST_StepIn_Results.tex"))

cat("✓ Saved A2_LUST_StepIn_Results.csv and .tex\n")


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

# Create facility-level survival data (motor fuel incumbents)
surv_data <- mf_data[, .(
  duration_months = max(months_since_entry, na.rm = TRUE),
  exit_event = max(exit_flag, na.rm = TRUE),
  avg_tank_age = mean(avg_tank_age, na.rm = TRUE),
  has_single_walled = max(has_single_walled, na.rm = TRUE),
  total_capacity = mean(total_capacity, na.rm = TRUE),
  texas_treated = max(texas_treated),
  state = first(state)
), by = panel_id]

# Remove facilities with zero duration
surv_data <- surv_data[duration_months > 0 & !is.na(duration_months)]
cat(sprintf("Survival sample: %s facilities\n", format(nrow(surv_data), big.mark = ",")))

# Standardize continuous covariates for numerical stability
surv_data[, `:=`(
  avg_tank_age_std = scale(avg_tank_age)[,1],
  log_capacity_std = scale(log(total_capacity + 1))[,1]
)]

# Cox model (simple specification)
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

# Get linear predictor for each facility
lp_lookup <- predict(surv_model, newdata = surv_data, type = "lp")
names(lp_lookup) <- surv_data$panel_id

# Merge linear predictor to panel
mf_data[, surv_lp := lp_lookup[panel_id]]

# Calculate IPS weights (inverse of hazard)
mf_data[, ips_weight := exp(-surv_lp)]

# Stabilize weights (trim extremes at 1st/99th percentile)
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
  cluster = "state"
)

es_exit_ips <- feols(
  exit_flag ~ i(event_date, texas_treated, ref = ref_event_date) | panel_id + event_date,
  data = mf_data,
  weights = ~ips_weight,
  cluster = "state"
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

# Function to compute hazard data
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

# Compute hazards for main outcomes (motor fuel)
hazard_lust <- compute_hazard_data(mf_data, "leak_incident")
hazard_exit <- compute_hazard_data(mf_data, "exit_flag")
hazard_retrofit <- compute_hazard_data(mf_data, "replacement_event")

# Compute hazards for LUST detection mechanisms
hazard_closure_detect <- compute_hazard_data(mf_data, "leak_found_by_closure")
hazard_operational <- compute_hazard_data(mf_data, "leak_not_found_by_exit")

cat("Time-average hazards calculated\n")

#------------------------------------------------------------------------------
# C.2: Main Deaner & Ku Regressions (LUST, Exit, Retrofit)
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

# Summary table
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

print(dk_results)
fwrite(dk_results, file.path(OUTPUT_TABLES, "C2_DeanerKu_Hazard_DiD.csv"))
cat("Saved: C2_DeanerKu_Hazard_DiD.csv\n")

#------------------------------------------------------------------------------
# C.3: Stratified by is_motor_fuel (Triple Interaction)
#------------------------------------------------------------------------------
cat("\n--- C.3: Deaner & Ku Stratified by Motor Fuel ---\n")

# Compute hazards for all facilities (motor fuel + non-motor fuel)
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

# Triple interaction model: Texas x Post x Motor Fuel
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

# Extract coefficients
dk_strat_results <- data.table(
  Outcome = c("LUST", "Exit"),
  Main_Effect = c(
    coef(did_dk_lust_strat)["texas_treated:post_1999"],
    coef(did_dk_exit_strat)["texas_treated:post_1999"]
  )
)

# Try to get triple interaction
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

print(dk_strat_results)
fwrite(dk_strat_results, file.path(OUTPUT_TABLES, "C3_DeanerKu_MotorFuel_Stratified.csv"))
cat("Saved: C3_DeanerKu_MotorFuel_Stratified.csv\n")

#------------------------------------------------------------------------------
# C.4: Stratified by age_bins
#------------------------------------------------------------------------------
cat("\n--- C.4: Deaner & Ku Stratified by Age Bins ---\n")

# Compute hazards stratified by age bins
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

# Triple interaction model: Texas x Post x Age Bins
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

# Extract main effect and interaction coefficients
dk_age_results <- data.table(
  Outcome = c("LUST", "Exit"),
  Main_Effect = c(
    coef(did_dk_lust_age)["texas_treated:post_1999"],
    coef(did_dk_exit_age)["texas_treated:post_1999"]
  )
)

# Get age bin interaction coefficients
age_bin_interactions <- grep("texas_treated:post_1999:age_bins", 
                              names(coef(did_dk_lust_age)), value = TRUE)
if (length(age_bin_interactions) > 0) {
  cat("\nAge Bin x Treatment Interactions (LUST):\n")
  for (abi in age_bin_interactions) {
    cat(sprintf("  %s: %.6f\n", abi, coef(did_dk_lust_age)[abi]))
  }
}

print(dk_age_results)
fwrite(dk_age_results, file.path(OUTPUT_TABLES, "C4_DeanerKu_AgeBin_Stratified.csv"))
cat("Saved: C4_DeanerKu_AgeBin_Stratified.csv\n")

#------------------------------------------------------------------------------
# C.5: Vintage Extension with Memory Management
#------------------------------------------------------------------------------
cat("\n--- C.5: Vintage-Based Hazard Analysis ---\n")

# Create vintage-aggregated dataset
vintage_agg <- mf_data[, .(
  leak_rate = mean(leak_incident, na.rm = TRUE),
  exit_rate = mean(exit_flag, na.rm = TRUE),
  retrofit_rate = mean(replacement_event, na.rm = TRUE),
  n_obs = .N,
  n_facilities = uniqueN(panel_id)
), by = .(texas_treated, post_1999, reg_vintage)]

# Save intermediate data
temp_vintage_file <- here("Data", "Processed", "temp_vintage_agg.csv")
fwrite(vintage_agg, temp_vintage_file)
cat(sprintf("Saved intermediate vintage data: %s\n", basename(temp_vintage_file)))

# Run vintage hazard models
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

print(vintage_results)
fwrite(vintage_results, file.path(OUTPUT_TABLES, "C5_Vintage_Hazard_DiD.csv"))

# Memory management
rm(vintage_agg)
gc()
cat("Vintage analysis complete, memory cleared\n")

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

# Create any_closure indicator
mf_data[, any_closure := as.integer(tanks_closed > 0)]

# Subset to facility-months with closures
closure_months <- mf_data[any_closure == 1]
cat(sprintf("Closure sample: %s facility-months\n", format(nrow(closure_months), big.mark = ",")))

# Create mutually exclusive outcomes
closure_months[, `:=`(
  exit_no_retrofit = as.integer(exit_flag == 1 & replacement_event == 0),
  retrofit_no_exit = as.integer(replacement_event == 1 & exit_flag == 0),
  both_exit_retrofit = as.integer(exit_flag == 1 & replacement_event == 1),
  neither = as.integer(exit_flag == 0 & replacement_event == 0)
)]

# Regressions with age_bins control
did_exit_no_retrofit <- feols(
  exit_no_retrofit ~ texas_treated*post_1999 + age_bins + has_single_walled +
                     active_tanks + total_capacity | panel_id + year_month,
  data = closure_months,
  cluster = "state"
)

did_retrofit_no_exit <- feols(
  retrofit_no_exit ~ texas_treated*post_1999 + age_bins + has_single_walled +
                     active_tanks + total_capacity | panel_id + year_month,
  data = closure_months,
  cluster = "state"
)

did_both <- feols(
  both_exit_retrofit ~ texas_treated*post_1999 + age_bins + has_single_walled +
                      active_tanks + total_capacity | panel_id + year_month,
  data = closure_months,
  cluster = "state"
)

did_neither <- feols(
  neither ~ texas_treated*post_1999 + age_bins + has_single_walled +
           active_tanks + total_capacity | panel_id + year_month,
  data = closure_months,
  cluster = "state"
)

cat("Running WCB for closure behavior models...\n")
wcb_d1a <- fast_wild_bootstrap(did_exit_no_retrofit, "texas_treated:post_1999", B = B_BOOTSTRAP)
wcb_d1b <- fast_wild_bootstrap(did_retrofit_no_exit, "texas_treated:post_1999", B = B_BOOTSTRAP)
wcb_d1c <- fast_wild_bootstrap(did_both, "texas_treated:post_1999", B = B_BOOTSTRAP)
wcb_d1d <- fast_wild_bootstrap(did_neither, "texas_treated:post_1999", B = B_BOOTSTRAP)

save_universal_table(
  models = list(did_exit_no_retrofit, did_retrofit_no_exit, did_both, did_neither),
  wcb_results = list(wcb_d1a, wcb_d1b, wcb_d1c, wcb_d1d),
  headers = c("Exit (No Retrofit)", "Retrofit (No Exit)", "Both", "Neither"),
  base_name = "D1_Conditional_Closure_Behavior",
  title = "Conditional Closure Behavior: Exit vs Retrofit Decisions"
)

#------------------------------------------------------------------------------
# D.2: Exit-No-Leak vs Exit-With-Leak
#------------------------------------------------------------------------------
cat("\n--- D.2: Mechanism Decomposition (Exit-No-LUST) ---\n")

# Create exit without leak indicator
mf_data[, exit_no_lust := as.integer(exit_flag == 1 & leak_incident == 0)]
mf_data[, exit_with_lust := as.integer(exit_flag == 1 & leak_incident == 1)]

did_exit_no_lust <- feols(
  exit_no_lust ~ texas_treated*post_1999 | panel_id + year_month,
  data = mf_data,
  cluster = "state"
)

did_exit_with_lust <- feols(
  exit_with_lust ~ texas_treated*post_1999 | panel_id + year_month,
  data = mf_data,
  cluster = "state"
)

cat("Running WCB for mechanism decomposition...\n")
wcb_d2a <- fast_wild_bootstrap(did_exit_no_lust, "texas_treated:post_1999", B = B_BOOTSTRAP)
wcb_d2b <- fast_wild_bootstrap(did_exit_with_lust, "texas_treated:post_1999", B = B_BOOTSTRAP)

save_universal_table(
  models = list(did_lust_naive, did_exit_naive, did_exit_no_lust, did_exit_with_lust),
  wcb_results = list(wcb_a1_lust, wcb_a1_exit, wcb_d2a, wcb_d2b),
  headers = c("LUST", "Exit (All)", "Exit (No LUST)", "Exit (With LUST)"),
  base_name = "D2_Mechanism_Decomposition",
  title = "Mechanism Decomposition: LUST and Exit Channels",
  digits = 6
)

# Interpretation summary
decomp_summary <- data.table(
  Channel = c("LUST Detection", "Exit (All)", "Exit without LUST", "Exit with LUST"),
  Coefficient = c(wcb_a1_lust$beta_obs, wcb_a1_exit$beta_obs, wcb_d2a$beta_obs, wcb_d2b$beta_obs),
  WCB_P = c(wcb_a1_lust$p_val, wcb_a1_exit$p_val, wcb_d2a$p_val, wcb_d2b$p_val)
)

decomp_summary[, Interpretation := fcase(
  Channel == "LUST Detection" & Coefficient < 0, "Policy reduces leak detection",
  Channel == "Exit without LUST" & Coefficient > 0, "Compositional change (risky exit)",
  Channel == "Exit with LUST" & Coefficient < 0, "Behavioral improvement",
  default = "Check significance"
)]

print(decomp_summary)
fwrite(decomp_summary, file.path(OUTPUT_TABLES, "D2_Mechanism_Decomposition_Summary.csv"))

#------------------------------------------------------------------------------
# D.3: LUST Detection Mechanism (Closure-Found vs Operational-Found)
#------------------------------------------------------------------------------
cat("\n--- D.3: LUST Detection Mechanism ---\n")

did_lust_closure_detect <- feols(
  leak_found_by_closure ~ texas_treated*post_1999 + age_bins + has_single_walled |
                           panel_id + year_month,
  data = mf_data,
  cluster = "state"
)

did_lust_operational <- feols(
  leak_not_found_by_exit ~ texas_treated*post_1999 + age_bins + has_single_walled |
                            panel_id + year_month,
  data = mf_data,
  cluster = "state"
)

cat("Running WCB for detection mechanism...\n")
wcb_d3a <- fast_wild_bootstrap(did_lust_closure_detect, "texas_treated:post_1999", B = B_BOOTSTRAP)
wcb_d3b <- fast_wild_bootstrap(did_lust_operational, "texas_treated:post_1999", B = B_BOOTSTRAP)

save_universal_table(
  models = list(did_lust_closure_detect, did_lust_operational),
  wcb_results = list(wcb_d3a, wcb_d3b),
  headers = c("Closure-Detected", "Operational"),
  base_name = "D3_LUST_Detection_Mechanism",
  title = "LUST Detection Mechanism: Closure vs Operational Discovery",
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

# Subset to retrofit events
retrofit_months <- mf_data[replacement_event == 1]
cat(sprintf("Retrofit sample: %s facility-months\n", format(nrow(retrofit_months), big.mark = ",")))

# Create retrofit characterization variables
retrofit_months[, `:=`(
  retrofit_is_double_wall = as.integer(double_walled_installed > 0),
  capacity_change_pct = 100 * capacity_change / (capacity_closed + 1),
  net_tank_change = tanks_installed - tanks_closed,
  tank_count_increased = as.integer(tanks_installed > tanks_closed),
  tank_count_decreased = as.integer(tanks_installed < tanks_closed),
  equal_replacement = as.integer(tanks_installed == tanks_closed)
)]

# Regressions with age_bins control
did_dw_upgrade <- feols(
  retrofit_is_double_wall ~ texas_treated*post_1999 + age_bins + has_single_walled +
                            active_tanks | panel_id + year_month,
  data = retrofit_months,
  cluster = "state"
)

did_capacity_change <- feols(
  capacity_change ~ texas_treated*post_1999 + age_bins + has_single_walled +
                    active_tanks | panel_id + year_month,
  data = retrofit_months,
  cluster = "state"
)

did_capacity_increase <- feols(
  capacity_increased ~ texas_treated*post_1999 + age_bins + has_single_walled +
                       active_tanks | panel_id + year_month,
  data = retrofit_months,
  cluster = "state"
)

did_capacity_decrease <- feols(
  capacity_decreased ~ texas_treated*post_1999 + age_bins + has_single_walled +
                       active_tanks | panel_id + year_month,
  data = retrofit_months,
  cluster = "state"
)

did_net_tank_change <- feols(
  net_tank_change ~ texas_treated*post_1999 + age_bins + has_single_walled +
                    active_tanks | panel_id + year_month,
  data = retrofit_months,
  cluster = "state"
)

cat("Running WCB for retrofit characterization...\n")
wcb_e1a <- fast_wild_bootstrap(did_dw_upgrade, "texas_treated:post_1999", B = B_BOOTSTRAP)
wcb_e1b <- fast_wild_bootstrap(did_capacity_change, "texas_treated:post_1999", B = B_BOOTSTRAP)
wcb_e1c <- fast_wild_bootstrap(did_capacity_increase, "texas_treated:post_1999", B = B_BOOTSTRAP)
wcb_e1d <- fast_wild_bootstrap(did_capacity_decrease, "texas_treated:post_1999", B = B_BOOTSTRAP)
wcb_e1e <- fast_wild_bootstrap(did_net_tank_change, "texas_treated:post_1999", B = B_BOOTSTRAP)

save_universal_table(
  models = list(did_dw_upgrade, did_capacity_change, did_capacity_increase,
                did_capacity_decrease, did_net_tank_change),
  wcb_results = list(wcb_e1a, wcb_e1b, wcb_e1c, wcb_e1d, wcb_e1e),
  headers = c("DW Upgrade", "Capacity Delta", "Cap Up", "Cap Down", "Tank Delta"),
  base_name = "E1_Retrofit_Characterization",
  title = "Retrofit Characterization: Technology and Scale Adjustments"
)

#------------------------------------------------------------------------------
# E.2: Age Bin x Policy Interactions (Heterogeneous Treatment Effects)
#------------------------------------------------------------------------------
cat("\n--- E.2: Age Bin x Policy Interactions (HTE) ---\n")

# Age bin x treatment interaction models
did_exit_age_hte <- feols(
  exit_flag ~ texas_treated*post_1999*age_bins | panel_id + year_month,
  data = mf_data,
  cluster = "state"
)

did_retrofit_age_hte <- feols(
  replacement_event ~ texas_treated*post_1999*age_bins | panel_id + year_month,
  data = mf_data,
  cluster = "state"
)

did_lust_age_hte <- feols(
  leak_incident ~ texas_treated*post_1999*age_bins | panel_id + year_month,
  data = mf_data,
  cluster = "state"
)

# Extract age bin interaction coefficients
cat("\nAge Bin HTE Coefficients (LUST):\n")
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
  print(age_hte_results)
  fwrite(age_hte_results, file.path(OUTPUT_TABLES, "E2_AgeBin_HTE.csv"))
  cat("Saved: E2_AgeBin_HTE.csv\n")
}

#------------------------------------------------------------------------------
# E.3: Parallel Gradient Tests (using age_bins)
#------------------------------------------------------------------------------
cat("\n--- E.3: Parallel Gradient Tests ---\n")

# Test: Texas x Age Bin interaction in post-period (single-walled only)
pooled_test_exit <- feols(
  exit_flag ~ texas_treated*age_bins | year_month,
  data = mf_data[wall_type == "Single-Walled" & post_1999 == 1],
  cluster = "state"
)

pooled_test_retro <- feols(
  replacement_event ~ texas_treated*age_bins | year_month,
  data = mf_data[wall_type == "Single-Walled" & post_1999 == 1],
  cluster = "state"
)

# Joint test of all age bin interactions
cat("\nJoint F-test for age bin x treatment interactions:\n")

# Get interaction coefficient names
exit_interact_names <- grep("texas_treated:age_bins", names(coef(pooled_test_exit)), value = TRUE)
retro_interact_names <- grep("texas_treated:age_bins", names(coef(pooled_test_retro)), value = TRUE)

if (length(exit_interact_names) > 0) {
  # Wald test for joint significance
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
  
  print(gradient_results)
  fwrite(gradient_results, file.path(OUTPUT_TABLES, "E3_Parallel_Gradient_Test.csv"))
  
  # Decision rule for DCM
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

print(cloglog_results)
fwrite(cloglog_results, file.path(OUTPUT_TABLES, "F1_Cloglog_Hazard_DiD.csv"))

#------------------------------------------------------------------------------
# F.2: Frailty Models - State-Level Random Effect (using coxme for stability)
#------------------------------------------------------------------------------
cat("\n--- F.2: Frailty Models (State-Level Random Effect via coxme) ---\n")

# Exit frailty model with state-level RE
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

# Extract results
if (!is.null(frailty_exit)) {
  frailty_results <- data.table(
    Outcome = "Exit",
    Coefficient = fixef(frailty_exit)["texas_treated"],
    Hazard_Ratio = exp(fixef(frailty_exit)["texas_treated"]),
    Frailty_Variance = VarCorr(frailty_exit)$state[1],
    Converged = TRUE
  )
  
  print(frailty_results)
  fwrite(frailty_results, file.path(OUTPUT_TABLES, "F2_Frailty_Hazard_DiD.csv"))
  
  cat(sprintf("  State-level frailty variance: %.4f\n", VarCorr(frailty_exit)$state[1]))
  cat("  (Larger variance = more unobserved state-level heterogeneity)\n")
} else {
  cat("Frailty model did not converge. Skipping.\n")
}

cat("Frailty models complete\n")

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

# Get WCB data for event studies
wcb_es_lust <- get_unified_wcb(es_lust_ips, "LUST (IPS-Weighted)", B = B_BOOTSTRAP)
wcb_es_exit <- get_unified_wcb(es_exit_ips, "Exit (IPS-Weighted)", B = B_BOOTSTRAP)

# Combine plot data
es_plot_data <- rbindlist(list(
  wcb_es_lust$plot_data,
  wcb_es_exit$plot_data
), fill = TRUE)

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
      subtitle = "95% Wild Cluster Bootstrap Confidence Intervals",
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
  # Reshape for plotting
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
  estimate = c(wcb_e1a$beta_obs, wcb_e1c$beta_obs, wcb_e1d$beta_obs, wcb_e1e$beta_obs),
  wcb_p = c(wcb_e1a$p_val, wcb_e1c$p_val, wcb_e1d$p_val, wcb_e1e$p_val)
)

# Get SEs from models for CI
retrofit_coefs[, se := c(
  se(did_dw_upgrade)["texas_treated:post_1999"],
  se(did_capacity_increase)["texas_treated:post_1999"],
  se(did_capacity_decrease)["texas_treated:post_1999"],
  se(did_net_tank_change)["texas_treated:post_1999"]
)]

retrofit_coefs[, `:=`(
  lower = estimate - 1.96 * se,
  upper = estimate + 1.96 * se,
  significant = wcb_p < 0.05
)]

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
    color = "Significance (WCB)"
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
cat("    - A2_LUST_StepIn_Controls.csv/.tex/.txt\n")
cat("      (Baseline -> MF HTE -> +Age -> +EPA Deadline -> +Reg Vintage)\n\n")

cat("  Part C (Deaner & Ku):\n")
cat("    - C2_DeanerKu_Hazard_DiD.csv\n")
cat("    - C3_DeanerKu_MotorFuel_Stratified.csv\n")
cat("    - C4_DeanerKu_AgeBin_Stratified.csv\n")
cat("    - C5_Vintage_Hazard_DiD.csv\n\n")

cat("  Part D (Mechanisms):\n")
cat("    - D1_Conditional_Closure_Behavior.csv/.tex/.txt\n")
cat("    - D2_Mechanism_Decomposition.csv/.tex/.txt\n")
cat("    - D2_Mechanism_Decomposition_Summary.csv\n")
cat("    - D3_LUST_Detection_Mechanism.csv/.tex/.txt\n\n")

cat("  Part E (Heterogeneity):\n")
cat("    - E1_Retrofit_Characterization.csv/.tex/.txt\n")
cat("    - E2_AgeBin_HTE.csv\n")
cat("    - E3_Parallel_Gradient_Test.csv\n\n")

cat("  Part F (Robustness):\n")
cat("    - F1_Cloglog_Hazard_DiD.csv\n")
cat("    - F2_Frailty_Hazard_DiD.csv (state-level RE via coxme)\n\n")

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
   A.2.1: Baseline (full sample)
   A.2.2: Motor fuel HTE (TX x Post x MF triple interaction)
   A.2.3: + Age bin controls
   A.2.4: + EPA deadline interaction (pre1998_install x post_1999)
   A.2.5: + Regulatory vintage (3 categories, avoids age collinearity)

3. Part B: IPS weighting via Cox model (coxph with standardized covariates)

4. Part C: Deaner & Ku (2024) time-average hazard transformation
   - C.4 uses age_bins for stratification (categorical, easier interpretation)

5. Part E: All HTE analysis uses age_bins (categorical)
   - E.2: Texas x Post x Age_Bins triple interaction
   - E.3: Joint F-test for parallel gradients

6. Part F: Frailty uses coxme (state-level RE) - faster/more stable than coxph+frailty()

7. All standard errors clustered at state level (19 clusters)

8. Wild cluster bootstrap p-values (Roodman et al. 2019) in all tables
\n")

cat(sprintf("Bootstrap replications: B = %d\n", B_BOOTSTRAP))
if (B_BOOTSTRAP < 9999) {
  cat("WARNING: Using reduced B for testing. Set B = 9999 for final run.\n")
}

cat("\n====================================================================\n")