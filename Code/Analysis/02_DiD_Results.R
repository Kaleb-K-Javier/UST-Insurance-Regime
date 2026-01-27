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

# --- LOAD ANNUAL DATA ---
DATA_PATH <- here("Data", "Processed", "facility_leak_behavior_annual.csv")
if (!file.exists(DATA_PATH)) stop("Data not found. Run 10_Build_Annual_Panel.R first.")
annual_data <- fread(DATA_PATH)

# --- APPLY CONSTRAINTS ---
# Filter 1: Window (1990-2015 captures pre-trend and long-run post)
annual_data <- annual_data[panel_year >= 1980 & panel_year <= 2025]

# Filter 2: Control States
control_states <- c("Maine", "New Mexico", "Arkansas", "Oklahoma", "Louisiana",
                    "Kansas", "Montana", "Idaho", "South Dakota", "Alabama",
                    "Minnesota", "North Carolina", "Illinois", "Massachusetts",
                    "Ohio", "Pennsylvania", "Tennessee", "Virginia")
annual_data <- annual_data[state == "Texas" | state %in% control_states]

# Filter 3: Incumbents Only (Using new build variable)
annual_data <- annual_data[cohort == "Incumbent"]

# --- DEFINE OUTCOMES & ALIASES ---
annual_data[, `:=`(
  treated = texas_treated,
  post = post_1999,
  did_term = texas_treated * post_1999,
  
  # Binary Outcomes (Integers)
  y_lust = as.integer(leak_year),
  y_exit = as.integer(exit_flag),
  y_retro = as.integer(retrofit_year)
)]

# Motor Fuel Subsample
mf_data <- annual_data[is_motor_fuel == 1]# Final sample summary
cat(sprintf("\nFinal Analysis Sample:\n"))
cat(sprintf("  Total: %s facility-months\n", format(nrow(annual_data), big.mark = ",")))
cat(sprintf("  Texas: %s\n", format(sum(annual_data$texas_treated), big.mark = ",")))
cat(sprintf("  Control: %s\n", format(sum(!annual_data$texas_treated), big.mark = ",")))
cat(sprintf("  Unique facilities: %s\n", format(uniqueN(annual_data$panel_id), big.mark = ",")))

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

cat(sprintf("Motor fuel sample: %s facility-months\n", format(nrow(mf_data), big.mark = ",")))
# --- NAIVE 2WFE BENCHMARKS ---
did_lust_naive <- feols(y_lust ~ did_term | panel_id + panel_year,
                        data = mf_data, cluster = "state", lean = FALSE)

did_exit_naive <- feols(y_exit ~ did_term | panel_id + panel_year,
                        data = mf_data, cluster = "state", lean = FALSE)

did_retro_naive <- feols(y_retro ~ did_term | panel_id + panel_year,
                         data = mf_data, cluster = "state", lean = FALSE)

# Save table using new helper
save_standard_did_table(
  models = list(did_lust_naive, did_exit_naive, did_retro_naive),
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

# Note: Using 'annual_data' (full sample) to allow comparison between
# Motor Fuel vs. Non-Motor Fuel via interaction.

# A.2.1: Pooled (Benchmark)
# Simple DiD on the full annual sample
did_lust_a21 <- feols(
  y_lust ~ texas_treated * post_1999 | panel_id + panel_year,
  data = annual_data,
  cluster = "state",
  lean = F,
  mem.clean = FALSE
)

# A.2.2: Baseline Triple Interaction
# Adds (Treat x Post x Is_Motor_Fuel) to test if effect is driven by gas stations
did_lust_a22 <- feols(
  y_lust ~ texas_treated * post_1999 * is_motor_fuel | panel_id + panel_year,
  data = annual_data,
  cluster = "state",
  lean = FALSE,
  mem.clean = FALSE
)

# A.2.3: + Age Bins
# Controls for tank age profile (older tanks leak more)
did_lust_a23 <- feols(
  y_lust ~ texas_treated * post_1999 * is_motor_fuel + age_bins | panel_id + panel_year,
  data = annual_data,
  cluster = "state",
  lean = FALSE,
  mem.clean = FALSE
)

# A.2.4: + EPA Deadline Controls
# Controls for the 1998 upgrade deadline effects (pre-1998 tanks behave differently)
did_lust_a24 <- feols(
  y_lust ~ texas_treated * post_1999 * is_motor_fuel + age_bins + pre1998_install * post_1999 | 
    panel_id + panel_year,
  data = annual_data,
  cluster = "state",
  lean = FALSE,
  mem.clean = FALSE
)

# A.2.5: + Regulatory Vintage
# Controls for specific regulatory cohorts (Pre-RCRA, Transition, Post-Deadline)
did_lust_a25 <- feols(
  y_lust ~ texas_treated * post_1999 * is_motor_fuel + age_bins + reg_vintage | 
    panel_id + panel_year,
  data = annual_data,
  cluster = "state",
  lean = FALSE,
  mem.clean = FALSE
)

# Save triple interaction table
# Note: interact_term is now "texas_treated:post_1999:is_motor_fuel" (no TRUE suffix)
save_triple_did_table(
  pooled_model = did_lust_a21,
  triple_models = list(did_lust_a22, did_lust_a23, did_lust_a24, did_lust_a25),
  headers = c("Baseline", "MF HTE", "+Age", "+EPA Deadline", "+Reg Vintage"),
  base_name = "A2_LUST_StepIn_Results",
  title = "LUST DiD: Step-In Controls with MF Heterogeneity",
  base_term = "texas_treated:post_1999",
  interact_term = "texas_treated:post_1999:is_motor_fuel", 
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

# Aggregate to facility level using 'Dec' variables from annual build
surv_data <- mf_data[, .(
  duration_years = max(years_since_entry, na.rm=TRUE),
  exit_event = max(exit_flag, na.rm=TRUE),
  # Use the FIRST observed December age as proxy for entry age
  start_age = first(avg_tank_age_dec), 
  # Average capacity over life
  capacity = mean(total_capacity_dec, na.rm=TRUE),
  # Ever single-walled
  single_walled = max(has_single_walled_dec, na.rm=TRUE),
  texas = max(texas_treated)
), by = panel_id]

# Filter valid durations
surv_data <- surv_data[duration_years > 0 & !is.na(duration_years)]
cat(sprintf("Survival sample: %s facilities\n", format(nrow(surv_data), big.mark = ",")))

# Standardize covariates (fixing variable names to match aggregation above)
surv_data[, `:=`(
  start_age_std = scale(start_age)[,1],
  log_capacity_std = scale(log(capacity + 1))[,1]
)]

# Cox model
cat("Estimating Cox model...\n")
surv_model <- tryCatch({
  coxph(
    # Updated to use duration_years and correct column names
    Surv(duration_years, exit_event) ~ start_age_std + single_walled + 
      log_capacity_std + texas,
    data = surv_data,
    control = coxph.control(iter.max = 50)
  )
}, error = function(e) {
  cat(sprintf("Cox model failed: %s\n", e$message))
  cat("Using simplified specification...\n")
  # Fallback specification
  coxph(Surv(duration_years, exit_event) ~ start_age_std + texas, 
        data = surv_data)
})

cat("Survival model estimated\n")
cat(sprintf("  Concordance: %.3f\n", surv_model$concordance["concordance"]))

#------------------------------------------------------------------------------
# B.2: Compute IPS Weights
#------------------------------------------------------------------------------
cat("\n--- B.2: Computing IPS Weights ---\n")

# Predict Linear Predictor from the Cox model (Facility-Level)
lp_lookup <- predict(surv_model, newdata = surv_data, type = "lp")
names(lp_lookup) <- surv_data$panel_id

# Map weights back to the full panel (Panel-Year Level)
mf_data[, surv_lp := lp_lookup[panel_id]]
mf_data[, ips_weight := exp(-surv_lp)]

# Stabilize weights (Winsorize 1st/99th percentiles)
p1 <- quantile(mf_data$ips_weight, 0.01, na.rm = TRUE)
p99 <- quantile(mf_data$ips_weight, 0.99, na.rm = TRUE)
mf_data[, ips_weight := pmax(pmin(ips_weight, p99), p1)]
mf_data[is.na(ips_weight), ips_weight := 1]

cat(sprintf("IPS weights: median = %.3f, range = [%.3f, %.3f]\n",
            median(mf_data$ips_weight, na.rm = TRUE),
            min(mf_data$ips_weight, na.rm = TRUE),
            max(mf_data$ips_weight, na.rm = TRUE)))

#------------------------------------------------------------------------------
# B.3: IPS-Weighted Event Studies (Annual Optimized)
#------------------------------------------------------------------------------
cat("\n--- B.3: IPS-Weighted Event Studies (Annual Optimized) ---\n")

# Reduce threads to safe level for memory
setFixest_nthreads(14)

# LUST Event Study
es_lust_ips <- feols(
  # Outcome: y_lust (Annual binary), Ref Year: 1998
  y_lust ~ i(panel_year, texas_treated, ref = 1998) | 
           panel_id + panel_year,
  data = mf_data,
  weights = ~ips_weight,
  cluster = "state",
  lean = FALSE,
  mem.clean = FALSE
)

# Exit Event Study
es_exit_ips <- feols(
  # Outcome: y_exit (Annual binary), Ref Year: 1998
  y_exit ~ i(panel_year, texas_treated, ref = 1998) | 
           panel_id + panel_year,
  data = mf_data,
  weights = ~ips_weight,
  cluster = "state",
  lean = FALSE,
  mem.clean = FALSE
)

cat("Yearly IPS-weighted event studies estimated (Sparse Mode).\n")

#==============================================================================
# PART C: DEANER & KU (2024) HAZARD ANALYSIS
#==============================================================================

cat("\n")
cat("====================================================================\n")
cat("PART C: Deaner & Ku (2024) Time-Average Hazard DiD\n")
cat("====================================================================\n")

epsilon <- 1e-8

#==============================================================================
# PART C: DEANER & KU (2024) HAZARD ANALYSIS (STATE-LEVEL)
#==============================================================================

cat("\n")
cat("====================================================================\n")
cat("PART C: Deaner & Ku (2024) Hazard DiD (State-Level Aggregation)\n")
cat("====================================================================\n")

epsilon <- 1e-8

#------------------------------------------------------------------------------
# --- C.1: Define Annual Hazard Aggregation Function (State-Level) ---
compute_annual_hazard <- function(dt, outcome_col, group_vars = c("state", "panel_year")) {
  # 1. Aggregate to (State x Year) cells
  # We preserve 'texas_treated' (constant within state)
  agg <- dt[, .(
    cum_rate = mean(get(outcome_col), na.rm = TRUE),
    n = .N,
    texas_treated = max(texas_treated)
  ), by = group_vars]
  
  # 2. Sort
  setorderv(agg, group_vars)
  
  # 3. Determine Entity Grouping (State vs State+Fuel)
  #    Everything in group_vars EXCEPT 'panel_year' defines the unit
  entity_vars <- setdiff(group_vars, "panel_year")
  
  # 4. Calculate Base Rate (First period for each state)
  agg[, `:=`(
    time_idx = 1:.N,
    base_rate = first(cum_rate)
  ), by = entity_vars]
  
  # 5. Deaner & Ku Transformation: H_bar
  #    (1 / t-1) * log( (1 - base) / (1 - current) )
  epsilon <- 1e-8
  agg[, H_bar := ifelse(time_idx > 1, 
                        (1 / (time_idx - 1)) * log((1 - base_rate + epsilon) / (1 - cum_rate + epsilon)), 
                        0)]
  
  # 6. Define Post Flag
  agg[, post := as.integer(panel_year >= 1999)]
  
  # Return only t > 1
  return(agg[time_idx > 1])
}

# --- C.2: Execute State-Level Regressions (LUST, Exit, Retrofit) ---

# 1. Compute State-Level Hazards
hazard_lust <- compute_annual_hazard(mf_data, "y_lust")
hazard_exit <- compute_annual_hazard(mf_data, "y_exit")
hazard_retrofit <- compute_annual_hazard(mf_data, "y_retro")

cat("State-level hazards calculated (N=19 states)\n")

# 2. Estimate 2WFE Models (Cluster = State)
#    Fixed Effects: State + Year
#    Interaction: texas_treated:post
did_dk_lust <- feols(H_bar ~ texas_treated:post | state + panel_year, 
                     data = hazard_lust,  cluster = "state")

did_dk_exit <- feols(H_bar ~ texas_treated:post | state + panel_year, 
                     data = hazard_exit,  cluster = "state")

did_dk_retrofit <- feols(H_bar ~ texas_treated:post | state + panel_year, 
                         data = hazard_retrofit,  cluster = "state")

# 3. Helper to Extract Results
get_dk_stats <- function(model, label, n_obs) {
  co <- coef(model)["texas_treated:post"]
  se <- se(model)["texas_treated:post"]
  t_stat <- co / se
  p_val <- pvalue(model)["texas_treated:post"]
  return(data.table(
    Outcome = label,
    Coefficient = co,
    SE = se,
    N_Cells = n_obs,
    t_stat = t_stat,
    p_val = p_val,
    CI_Lower = co - 1.96 * se,
    CI_Upper = co + 1.96 * se
  ))
}

dk_results <- rbind(
  get_dk_stats(did_dk_lust, "LUST", nrow(hazard_lust)),
  get_dk_stats(did_dk_exit, "Exit", nrow(hazard_exit)),
  get_dk_stats(did_dk_retrofit, "Retrofit", nrow(hazard_retrofit))
)

# 4. Save Results
save_simple_csv_table(
  results_dt = dk_results,
  base_name = "C2_DeanerKu_Hazard_DiD",
  title = "Deaner & Ku Hazard DiD (State FE + Clustered SE)",
  use_bootstrap = FALSE
)

#------------------------------------------------------------------------------
# C.3: Stratified by is_motor_fuel (State Level)
#------------------------------------------------------------------------------
cat("\n--- C.3: Deaner & Ku Stratified by Motor Fuel ---\n")

# Aggregate by State x Year x FuelType
hazard_lust_all <- compute_annual_hazard(
  annual_data,
  "y_lust",
  group_vars = c("state", "panel_year", "is_motor_fuel")
)

hazard_exit_all <- compute_annual_hazard(
  annual_data,
  "y_exit",
  group_vars = c("state", "panel_year", "is_motor_fuel")
)

# Triple Interaction Model
# We interact the DiD term (texas_treated:post) with is_motor_fuel
did_dk_lust_strat <- feols(H_bar ~ texas_treated:post*is_motor_fuel | state + panel_year, 
                           data = hazard_lust_all[time_idx > 1], 
                           weights = ~n, cluster = "state")

did_dk_exit_strat <- feols(H_bar ~ texas_treated:post*is_motor_fuel | state + panel_year, 
                           data = hazard_exit_all[time_idx > 1], 
                           weights = ~n, cluster = "state")

dk_strat_results <- data.table(
  Outcome = c("LUST", "Exit"),
  # The 'Main Effect' here is the baseline interaction (non-motor fuel if 0/1)
  # But with 3-way interaction syntax, interpretation depends on factor setup.
  # We extract the specific terms:
  Main_Interaction = c(
    coef(did_dk_lust_strat)["texas_treated:post"],
    coef(did_dk_exit_strat)["texas_treated:post"]
  ),
  Triple_Interaction = c(
    coef(did_dk_lust_strat)["texas_treated:post:is_motor_fuel"],
    coef(did_dk_exit_strat)["texas_treated:post:is_motor_fuel"]
  )
)

save_simple_csv_table(
  results_dt = dk_strat_results,
  base_name = "C3_DeanerKu_MotorFuel_Stratified",
  title = "Deaner & Ku Stratified (State FE + Clustered SE)",
  use_bootstrap = FALSE
)

#------------------------------------------------------------------------------
# C.4: Stratified by Age Bins
#------------------------------------------------------------------------------
cat("\n--- C.4: Deaner & Ku Stratified by Age Bins ---\n")

# Aggregate by State x Year x AgeBin
hazard_lust_age <- compute_annual_hazard(
  mf_data[!is.na(age_bins)],
  "y_lust",
  group_vars = c("state", "panel_year", "age_bins")
)

# Interaction Model
did_dk_lust_age <- feols(H_bar ~ texas_treated*post*age_bins | state + panel_year, 
                         data = hazard_lust_age[time_idx > 1], 
                         weights = ~n, cluster = "state")

# Extract interactions
coefs <- coef(did_dk_lust_age)
age_terms <- grep("texas_treated:post:age_bins", names(coefs), value = TRUE)

dk_age_results <- data.table(
  Term = age_terms,
  Coefficient = coefs[age_terms],
  SE = se(did_dk_lust_age)[age_terms],
  t_stat = coefs[age_terms] / se(did_dk_lust_age)[age_terms]
)

save_simple_csv_table(
  results_dt = dk_age_results,
  base_name = "C4_DeanerKu_AgeBin_Stratified",
  title = "Deaner & Ku Age Stratified (State FE + Clustered SE)",
  use_bootstrap = FALSE
)

#------------------------------------------------------------------------------
# C.5: Vintage Extension
#------------------------------------------------------------------------------
cat("\n--- C.5: Vintage-Based Hazard Analysis ---\n")

# Use State-Level aggregation for Vintage analysis as well
vintage_agg <- mf_data[, .(
  leak_rate = mean(y_lust, na.rm = TRUE),
  exit_rate = mean(y_exit, na.rm = TRUE),
  n_obs = .N,
  texas_treated = max(texas_treated),
  post = as.integer(panel_year >= 1999)
), by = .(state, reg_vintage,panel_year)] # State x Post x Vintage

# Weighted Regressions with Cluster State
did_vintage_lust_0 <- feols(leak_rate ~ texas_treated*post+reg_vintage | state + panel_year,
                          data = vintage_agg, weights = ~n_obs, cluster = "state")

did_vintage_lust <- feols(leak_rate ~ texas_treated*post*reg_vintage | state + panel_year,
                          data = vintage_agg, weights = ~n_obs, cluster = "state")

did_vintage_exit_0 <- feols(exit_rate ~ texas_treated*post + reg_vintage | state + panel_year,
                          data = vintage_agg, weights = ~n_obs, cluster = "state")


did_vintage_exit <- feols(exit_rate ~ texas_treated*post*reg_vintage | state + panel_year,
                          data = vintage_agg, weights = ~n_obs, cluster = "state")

vintage_results <- data.table(
  Outcome = c("LUST", "Exit"),
  # Extracting primary post-deadline interaction as example
  Coef_PostDeadline = c(
    coef(did_vintage_lust)["texas_treated:post:reg_vintagePost-Deadline"],
    coef(did_vintage_exit)["texas_treated:post:reg_vintagePost-Deadline"]
  )
)

save_simple_csv_table(
  results_dt = vintage_results,
  base_name = "C5_Vintage_Hazard_DiD",
  title = "Vintage-Based Hazard Analysis (State FE)",
  use_bootstrap = FALSE
)

#------------------------------------------------------------------------------
# C.6: Dynamic Deaner & Ku Event Studies (Fixest Implementation)
#------------------------------------------------------------------------------
cat("\n--- C.6: Estimating Dynamic DK Event Studies (Fixest) ---\n")

estimate_dk_es_fixest <- function(df, outcome_name) {
  # Dynamic Model: H_bar ~ Texas x Year | State + Year
  es_model <- feols(
    H_bar ~ i(panel_year, texas_treated, ref = 1998) | state + panel_year,
    data = df,
    weights = ~n,
    cluster = "state"
  )
  
  tidy_res <- broom::tidy(es_model, conf.int = TRUE)
  tidy_res <- data.table(tidy_res)
  tidy_res[, year := as.numeric(gsub("[^0-9]", "", term))]
  tidy_res[, Outcome := outcome_name]
  
  ref_row <- data.table(
    term = "Ref", estimate = 0, std.error = 0, statistic = 0, 
    p.value = 1, conf.low = 0, conf.high = 0, year = 1998, Outcome = outcome_name
  )
  return(rbind(tidy_res, ref_row))
}

# Run using State-Level Hazard Data
dk_es_lust_data <- estimate_dk_es_fixest(hazard_lust, "LUST")
dk_es_exit_data <- estimate_dk_es_fixest(hazard_exit, "Exit")
dk_es_retro_data <- estimate_dk_es_fixest(hazard_retrofit, "Retrofit")

cat("Dynamic Deaner & Ku models estimated via fixest (Clustered by State).\n")

rm(vintage_agg)
invisible(gc())


#------------------------------------------------------------------------------
# C.6: Dynamic Deaner & Ku Event Studies (Fixest Implementation)
#------------------------------------------------------------------------------
cat("\n--- C.6: Estimating Dynamic DK Event Studies (Fixest) ---\n")

# Function to estimate dynamic coefficients using fixest
estimate_dk_es_fixest <- function(df, outcome_name) {
  
  # Estimate 2WFE Event Study on Aggregated Data
  # Model: H_bar ~ Group FE + Year FE + (Treat x Year)
  # Interaction: i(panel_year, texas_treated, ref = 1998)
  
  es_model <- feols(
    H_bar ~ i(panel_year, texas_treated, ref = 1998) | texas_treated + panel_year,
    data = df,
    weights = ~n,       # Weighted by cell size
    cluster = "panel_year" # robust SEs (simple cluster since N is small)
  )
  
  # Extract Coefficients using broom (compatible with fixest)
  tidy_res <- broom::tidy(es_model, conf.int = TRUE)
  
  # The term names will look like "panel_year::1995:texas_treated"
  # We extract the year from this string
  tidy_res <- data.table(tidy_res)
  tidy_res[, year := as.numeric(gsub("[^0-9]", "", term))]
  tidy_res[, Outcome := outcome_name]
  
  # Fixest automatically drops the reference year (1998). 
  # We manually add it back for the plot.
  ref_row <- data.table(
    term = "Ref", estimate = 0, std.error = 0, statistic = 0, 
    p.value = 1, conf.low = 0, conf.high = 0, year = 1998, Outcome = outcome_name
  )
  
  return(rbind(tidy_res, ref_row))
}

# 1. Run for Main Variations (LUST, Exit, Retrofit)
dk_es_lust_data <- estimate_dk_es_fixest(hazard_lust, "LUST")
dk_es_exit_data <- estimate_dk_es_fixest(hazard_exit, "Exit")
dk_es_retro_data <- estimate_dk_es_fixest(hazard_retrofit, "Retrofit")

# 2. Run for Stratified Variation (Non-Motor Fuel from C.3)
# Note: C.3 created 'hazard_lust_all' with 'is_motor_fuel' column
if (exists("hazard_lust_all")) {
  # Subset to Non-MF rows
  haz_nonmf <- hazard_lust_all[is_motor_fuel == FALSE]
  # Ensure we have enough data
  if (nrow(haz_nonmf) > 10) {
    dk_es_nonmf_data <- estimate_dk_es_fixest(haz_nonmf, "LUST (Non-MF)")
  } else {
    dk_es_nonmf_data <- data.table()
  }
}

cat("Dynamic Deaner & Ku models estimated via fixest.\n")

#------------------------------------------------------------------------------
# G.5: Deaner & Ku Event Study Plots
#------------------------------------------------------------------------------
cat("\n--- G.5: Deaner & Ku Event Study Plots ---\n")

# --- PLOT 1: MAIN OUTCOMES (LUST, Exit, Retrofit) ---
dk_plot_data <- rbind(dk_es_lust_data, dk_es_exit_data, dk_es_retro_data)

if (nrow(dk_plot_data) > 0) {
  dk_es_plot <- ggplot(dk_plot_data, aes(x = year, y = estimate, color = Outcome)) +
    # Reference Lines
    geom_vline(xintercept = 1998.5, linetype = "dashed", color = "gray30") +
    geom_hline(yintercept = 0, color = "black", linewidth = 0.3) +
    
    # Confidence Intervals
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                  width = 0.2, alpha = 0.6, position = position_dodge(width = 0.3)) +
    
    # Points & Lines
    geom_point(size = 2, position = position_dodge(width = 0.3)) +
    geom_line(position = position_dodge(width = 0.3), alpha = 0.6) +
    
    # Styling
    scale_color_manual(values = c("LUST" = "#D55E00", "Exit" = "#0072B2", "Retrofit" = "#009E73")) +
    facet_wrap(~Outcome, scales = "free_y", ncol = 1) +
    
    labs(
      title = "Deaner & Ku (2024) Event Study: Main Outcomes",
      subtitle = "Dynamic DiD on Transformed Hazard (H_bar) [Estimated via fixest]",
      x = "Year",
      y = "Effect on Transformed Hazard",
      caption = "Reference Year: 1998. Weighted by cell size (n)."
    ) +
    theme_pub() +
    theme(legend.position = "none")
  
  ggsave(file.path(OUTPUT_FIGURES, "G5_DeanerKu_Event_Study_Main.png"),
         dk_es_plot, width = 10, height = 10, dpi = 300, bg = "white")
  cat("Saved: G5_DeanerKu_Event_Study_Main.png\n")
}

# --- PLOT 2: ROBUSTNESS COMPARISON (MF vs Non-MF) ---
if (exists("dk_es_nonmf_data") && nrow(dk_es_nonmf_data) > 0) {
  # Combine MF LUST (from above) with Non-MF LUST
  # Note: dk_es_lust_data was labeled "LUST" above. Let's rename for clarity in this plot.
  mf_subset <- copy(dk_es_lust_data)[, Outcome := "LUST (Motor Fuel)"]
  
  comp_plot_data <- rbind(mf_subset, dk_es_nonmf_data)
  
  dk_comp_plot <- ggplot(comp_plot_data, aes(x = year, y = estimate, color = Outcome)) +
    geom_vline(xintercept = 1998.5, linetype = "dashed", color = "gray30") +
    geom_hline(yintercept = 0, color = "black", linewidth = 0.3) +
    
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = Outcome), alpha = 0.15, color = NA) +
    geom_line(linewidth = 1) +
    geom_point(size = 1.5) +
    
    scale_color_manual(values = c("LUST (Motor Fuel)" = "#D55E00", "LUST (Non-MF)" = "gray40")) +
    scale_fill_manual(values = c("LUST (Motor Fuel)" = "#D55E00", "LUST (Non-MF)" = "gray40")) +
    
    labs(
      title = "Deaner & Ku (2024) Robustness: Fuel Type",
      subtitle = "Dynamic DiD Hazard Trends: Motor Fuel vs. Other Substances",
      x = "Year",
      y = "Effect on Transformed Hazard",
      caption = "Reference Year: 1998."
    ) +
    theme_pub() +
    theme(legend.position = "bottom")
  
  ggsave(file.path(OUTPUT_FIGURES, "G5_DeanerKu_Event_Study_FuelType.png"),
         dk_comp_plot, width = 10, height = 6, dpi = 300, bg = "white")
  cat("Saved: G5_DeanerKu_Event_Study_FuelType.png\n")
}


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
# ANALYSIS: Decisions made *given* a tank closure occurred in year t.
# SAMPLE: Facility-years where n_closures > 0.
# VARIABLES (Pre-calculated in 10_Build_Annual_Panel.R):
#   - exit_no_retrofit:   Exit w/o replacement (Clean Exit)
#   - retrofit_no_exit:   Replacement w/o exit (Upgrade/Stay in Business)
#   - both_exit_retrofit: Simultaneous exit and retrofit (High Churn)
#   - neither:            Closure w/o exit or replacement (Downsizing)

# 1. Define Sample
closure_years <- mf_data[n_closures > 0]
cat(sprintf("Closure sample: %s facility-years\n", format(nrow(closure_years), big.mark = ",")))

# 2. Define 'Neither' (Downsizing) manually as residual category
closure_years[, neither := as.integer(y_exit == 0 & y_retro == 0)]

# 3. Estimate Models
# Note: Using '_dec' variables for controls (state at end of year)
did_exit_no_retrofit <- feols(
  exit_no_retrofit ~ did_term + age_bins + has_single_walled_dec + 
    active_tanks_dec + total_capacity_dec | panel_id + panel_year,
  data = closure_years, cluster = "state", lean = FALSE
)

did_retrofit_no_exit <- feols(
  retrofit_no_exit ~ did_term + age_bins + has_single_walled_dec + 
    active_tanks_dec + total_capacity_dec | panel_id + panel_year,
  data = closure_years, cluster = "state", lean = FALSE
)

did_both <- feols(
  both_exit_retrofit ~ did_term + age_bins + has_single_walled_dec + 
    active_tanks_dec + total_capacity_dec | panel_id + panel_year,
  data = closure_years, cluster = "state", lean = FALSE
)

did_neither <- feols(
  neither ~ did_term + age_bins + has_single_walled_dec + 
    active_tanks_dec + total_capacity_dec | panel_id + panel_year,
  data = closure_years, cluster = "state", lean = FALSE
)

save_standard_did_table(
  models = list(did_exit_no_retrofit, did_retrofit_no_exit, did_both, did_neither),
  headers = c("Clean Exit", "Retrofit", "Churn", "Downsize"),
  base_name = "D1_Conditional_Closure_Behavior",
  title = "Conditional Closure Behavior: Exit vs Retrofit Decisions",
  treatment_var = "did_term", cluster_var = "state",
  use_bootstrap = USE_BOOTSTRAP, n_reps = N_BOOTSTRAP
)

#------------------------------------------------------------------------------
# D.2: Strategic vs. Responsive Exits (Risk Decomposition)
#------------------------------------------------------------------------------
cat("\n--- D.2: Mechanism Decomposition (Exit-No-LUST) ---\n")

# --- DEFINITIONS ---
# This section decomposes the aggregate "Exit" outcome into two distinct types
# based on whether a leak was reported in the same year as the exit.
#
# 1. STRATEGIC EXIT (Clean Exit): Outcome = 'exit_no_leak'
#    - Definition: The firm exits the market *without* a reported leak event.
#    - Mechanism: Pre-emptive avoidance of future liability or regulatory compliance costs.
#    - Interpretation: A positive coefficient suggests the policy pushed "risky but currently clean" firms out.
#
# 2. RESPONSIVE EXIT (Dirty Exit): Outcome = 'exit_with_leak'
#    - Definition: The firm exits the market *concurrently* with a reported leak.
#    - Mechanism: The exit is forced or triggered by the financial shock of discovery/cleanup.
#    - Interpretation: A negative coefficient implies the policy (insurance) reduced the financial shock 
#      of leaks, allowing firms to clean up and stay in business rather than exiting.

did_exit_no_lust <- feols(
  exit_no_leak ~ did_term | panel_id + panel_year,
  data = mf_data, cluster = "state", lean = FALSE
)

did_exit_with_lust <- feols(
  exit_with_leak ~ did_term | panel_id + panel_year,
  data = mf_data, cluster = "state", lean = FALSE
)

save_standard_did_table(
  models = list(did_lust_naive, did_exit_naive, did_exit_no_lust, did_exit_with_lust),
  headers = c("LUST (All)", "Exit (All)", "Strategic Exit", "Responsive Exit"),
  base_name = "D2_Mechanism_Decomposition",
  title = "Mechanism Decomposition: Strategic vs. Responsive Exits",
  treatment_var = "did_term", cluster_var = "state",
  use_bootstrap = USE_BOOTSTRAP, n_reps = N_BOOTSTRAP,
  digits = 6
)

# --- INTERPRETATION SUMMARY TABLE ---
decomp_summary <- data.table(
  Channel = c("LUST Detection", "Exit (All)", "Strategic Exit (No LUST)", "Responsive Exit (With LUST)"),
  Coefficient = c(
    coef(did_lust_naive)["did_term"],
    coef(did_exit_naive)["did_term"],
    coef(did_exit_no_lust)["did_term"],
    coef(did_exit_with_lust)["did_term"]
  )
)

decomp_summary[, Interpretation := fcase(
  Channel == "LUST Detection" & Coefficient < 0, "Policy reduces leak detection (Moral Hazard?)",
  Channel == "Exit (All)" & Coefficient > 0, "Net reduction in market size",
  Channel == "Strategic Exit (No LUST)" & Coefficient > 0, "Pre-emptive exit of risky capital",
  Channel == "Responsive Exit (With LUST)" & Coefficient < 0, "Insurance prevents exit after shock",
  default = "Check significance/Direction"
)]

save_summary_table(
  results_dt = decomp_summary,
  base_name = "D2_Mechanism_Decomposition_Summary",
  title = "Mechanism Decomposition Interpretation",
  notes = c("Coefficients from Texas x Post interaction (did_term)")
)

#------------------------------------------------------------------------------
# D.3: LUST Detection Mechanism (Discovery Channel)
#------------------------------------------------------------------------------
cat("\n--- D.3: LUST Detection Mechanism ---\n")
# ANALYSIS: Decomposing "LUST" based on *when* it was found relative to closure.
# WINDOW: "Primary Specification" from 10_Build_Annual_Panel.R (Section 7).
#   - Closure-Triggered (0-60d): Leak discovered within 60 days of closure.
#   - Operational: Leak discovered outside this window (Monitored/Routine).

did_lust_closure_detect <- feols(
  leak_found_by_closure ~ did_term + age_bins + has_single_walled_dec |
    panel_id + panel_year,
  data = mf_data, cluster = "state", lean = FALSE
)

did_lust_operational <- feols(
  leak_not_found_by_exit ~ did_term + age_bins + has_single_walled_dec |
    panel_id + panel_year,
  data = mf_data, cluster = "state", lean = FALSE
)

save_standard_did_table(
  models = list(did_lust_closure_detect, did_lust_operational),
  headers = c("Closure-Triggered", "Operational Discovery"),
  base_name = "D3_LUST_Detection_Mechanism",
  title = "LUST Detection Mechanism (Primary: 0-60 Days)",
  treatment_var = "did_term", cluster_var = "state",
  use_bootstrap = USE_BOOTSTRAP, n_reps = N_BOOTSTRAP
)

#------------------------------------------------------------------------------
# D.4: Robustness to Detection Windows (Narrow vs. Wide)
#------------------------------------------------------------------------------
cat("\n--- D.4: LUST Detection Robustness (Narrow vs Wide) ---\n")
# Testing sensitivity to the definition of "Closure-Triggered".
# Narrow = 0-30 days; Wide = 0-90 days.

did_lust_narrow <- feols(
  tank_closure_revealed_narrow ~ did_term + age_bins + has_single_walled_dec |
    panel_id + panel_year,
  data = mf_data, cluster = "state", lean = FALSE
)

did_lust_wide <- feols(
  tank_closure_revealed_wide ~ did_term + age_bins + has_single_walled_dec |
    panel_id + panel_year,
  data = mf_data, cluster = "state", lean = FALSE
)

save_standard_did_table(
  models = list(did_lust_narrow, did_lust_closure_detect, did_lust_wide),
  headers = c("Narrow (0-30d)", "Primary (0-60d)", "Wide (0-90d)"),
  base_name = "D4_LUST_Detection_Robustness",
  title = "Robustness: Sensitivity to Closure-Detection Window",
  treatment_var = "did_term", cluster_var = "state",
  use_bootstrap = USE_BOOTSTRAP, n_reps = N_BOOTSTRAP
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
# ANALYSIS: Analyzing the nature of tank replacements (Upsize vs Downsize vs Tech Upgrade).
# SAMPLE: Facility-years where a retrofit occurred (y_retro == 1).
# VARIABLES (From 10_Build_Annual_Panel.R):
#   - capacity_change_year: Net change in capacity (New - Old).
#   - net_tank_change: Net change in tank count (New - Old).

retrofit_years <- mf_data[y_retro == 1]
cat(sprintf("Retrofit sample: %s facility-years\n", format(nrow(retrofit_years), big.mark = ",")))

# Define Retrofit Types based on annual changes
retrofit_years[, `:=`(
  # Technology Upgrade: Did they install double-walled tanks?
  retrofit_is_double_wall = as.integer(double_walled_installed_year > 0),
  
  # Scale Adjustments: Direction of capacity change
  is_upsize = as.integer(capacity_change_year > 0),
  is_downsize = as.integer(capacity_change_year < 0),
  
  # Tank Count Adjustments
  net_tank_change = net_tank_change, # Already in annual build
  tank_count_increased = as.integer(net_tank_change > 0),
  tank_count_decreased = as.integer(net_tank_change < 0)
)]

# 1. Tech Upgrade (Double-Wall)
did_dw_upgrade <- feols(
  retrofit_is_double_wall ~ did_term + age_bins + has_single_walled_dec +
    active_tanks_dec | panel_id + panel_year,
  data = retrofit_years, cluster = "state", lean = FALSE
)

# 2. Capacity Magnitude Change (Continuous)
did_capacity_change <- feols(
  capacity_change_year ~ did_term + age_bins + has_single_walled_dec +
    active_tanks_dec | panel_id + panel_year,
  data = retrofit_years, cluster = "state", lean = FALSE
)

# 3. Probability of Upsizing
did_upsize <- feols(
  is_upsize ~ did_term + age_bins + has_single_walled_dec +
    active_tanks_dec | panel_id + panel_year,
  data = retrofit_years, cluster = "state", lean = FALSE
)

# 4. Probability of Downsizing
did_downsize <- feols(
  is_downsize ~ did_term + age_bins + has_single_walled_dec +
    active_tanks_dec | panel_id + panel_year,
  data = retrofit_years, cluster = "state", lean = FALSE
)

# 5. Net Tank Count Change
did_net_tank_change <- feols(
  net_tank_change ~ did_term + age_bins + has_single_walled_dec +
    active_tanks_dec | panel_id + panel_year,
  data = retrofit_years, cluster = "state", lean = FALSE
)

save_standard_did_table(
  models = list(did_dw_upgrade, did_capacity_change, did_upsize, did_downsize, did_net_tank_change),
  headers = c("DW Upgrade (Prob)", "Capacity Delta (Gal)", "Upsize (Prob)", "Downsize (Prob)", "Net Tank Change"),
  base_name = "E1_Retrofit_Characterization",
  title = "Retrofit Characterization: Technology and Scale Adjustments",
  treatment_var = "did_term", cluster_var = "state",
  use_bootstrap = USE_BOOTSTRAP, n_reps = N_BOOTSTRAP
)

#------------------------------------------------------------------------------
# E.2: Age Bin x Policy Interactions (HTE)
#------------------------------------------------------------------------------
cat("\n--- E.2: Age Bin x Policy Interactions (HTE) ---\n")
# ANALYSIS: Testing if the policy effect varies by tank age.
# MODEL: Outcome ~ did_term * age_bins

did_exit_age_hte <- feols(
  y_exit ~ did_term * age_bins | panel_id + panel_year,
  data = mf_data, cluster = "state", lean = FALSE
)

did_retrofit_age_hte <- feols(
  y_retro ~ did_term * age_bins | panel_id + panel_year,
  data = mf_data, cluster = "state", lean = FALSE
)

did_lust_age_hte <- feols(
  y_lust ~ did_term * age_bins | panel_id + panel_year,
  data = mf_data, cluster = "state", lean = FALSE
)

# Extract Coefficients for Plotting/Saving
lust_age_coefs <- coef(did_lust_age_hte)
lust_age_se <- se(did_lust_age_hte)
# Note: 'did_term' is 'texas_treated:post_1999', so interaction is 'did_term:age_bins...'
age_hte_names <- grep("did_term:age_bins", names(lust_age_coefs), value = TRUE)

if (length(age_hte_names) > 0) {
  age_hte_results <- data.table(
    Age_Bin = gsub("did_term:age_bins", "", age_hte_names),
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
# E.3: Parallel Gradient Tests (Single-Walled Validation)
#------------------------------------------------------------------------------
cat("\n--- E.3: Parallel Gradient Tests ---\n")
# PURPOSE: Validate the Discrete Choice Model (DCM) assumption.
# CHECK: Do Texas and Control states have similar age gradients in the POST period?
# SAMPLE: Single-Walled tanks in Post-1999 period only.

# Filter sample for test
gradient_sample <- mf_data[wall_type == "Single-Walled" & post_1999 == 1]

pooled_test_exit <- feols(
  y_exit ~ texas_treated * age_bins | panel_year,
  data = gradient_sample, cluster = "state", lean = FALSE
)

pooled_test_retro <- feols(
  y_retro ~ texas_treated * age_bins | panel_year,
  data = gradient_sample, cluster = "state", lean = FALSE
)

cat("\nJoint F-test for age bin x treatment interactions:\n")

exit_interact_names <- grep("texas_treated:age_bins", names(coef(pooled_test_exit)), value = TRUE)
retro_interact_names <- grep("texas_treated:age_bins", names(coef(pooled_test_retro)), value = TRUE)

if (length(exit_interact_names) > 0) {
  exit_joint_test <- tryCatch({
    wald(pooled_test_exit, exit_interact_names)
  }, error = function(e) list(p = NA, stat = NA))
  
  retro_joint_test <- tryCatch({
    wald(pooled_test_retro, retro_interact_names)
  }, error = function(e) list(p = NA, stat = NA))
  
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
    title = "Parallel Gradient Tests (DCM Assumption Check)"
  )
  
  # Decision Rule Output
  cat("\n=== Decision Rule for DCM Specification ===\n")
  if (all(gradient_results$Parallel_Gradients, na.rm = TRUE)) {
    cat("PARALLEL age gradients (Joint p > 0.10 for both outcomes)\n")
    cat(" -> DCM baseline specification is appropriate (gradients are shared).\n")
  } else {
    cat("NON-PARALLEL age gradients (Joint p < 0.10 for at least one outcome)\n")
    cat(" -> DCM must include 'age_bins x policy' interactions to capture state-specific gradients.\n")
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
# Checking if results hold under a functional form specifically designed for 
# discrete-time binary outcomes (Complementary Log-Log).

did_cloglog_lust <- feglm(
  y_lust ~ did_term + age_bins | panel_year,
  data = mf_data,
  family = binomial(link = "cloglog"),
  cluster = "state"
)

did_cloglog_exit <- feglm(
  y_exit ~ did_term + age_bins | panel_year,
  data = mf_data,
  family = binomial(link = "cloglog"),
  cluster = "state"
)

cat("Cloglog models estimated\n")

cloglog_results <- data.table(
  Outcome = c("LUST", "Exit"),
  Coefficient = c(
    coef(did_cloglog_lust)["did_term"],
    coef(did_cloglog_exit)["did_term"]
  ),
  SE = c(
    se(did_cloglog_lust)["did_term"],
    se(did_cloglog_exit)["did_term"]
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
  title = "Cloglog Discrete Hazard DiD (Annual)",
  models = list(did_cloglog_lust, did_cloglog_exit),
  treatment_var = "did_term",
  cluster_var = "state",
  use_bootstrap = USE_BOOTSTRAP,
  n_reps = N_BOOTSTRAP
)

#------------------------------------------------------------------------------
# F.2: Frailty Models
#------------------------------------------------------------------------------
cat("\n--- F.2: Frailty Models (State-Level Random Effect via coxme) ---\n")

# Using 'surv_data' created in Part B (Facility-level cross-section)
# Variables: duration_years, exit_event, start_age, single_walled, texas

cat("Estimating Exit frailty model (coxme with state RE)...\n")
frailty_exit <- tryCatch({
  coxme(
    Surv(duration_years, exit_event) ~ texas + 
      scale(start_age) + single_walled +
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
    Coefficient = fixef(frailty_exit)["texas"],
    Hazard_Ratio = exp(fixef(frailty_exit)["texas"]),
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
cat("\n--- G.1: Event Study Plots (Annual) ---\n")

# Helper function to extract coefficients from fixest annual event study
# (Handles term naming like "panel_year::1995:texas_treated")
extract_es <- function(model, label) {
  res <- broom::tidy(model, conf.int = TRUE)
  # Filter for interaction terms
  res <- res[grepl("panel_year", res$term), ]
  # Extract numeric year
  res$year <- as.numeric(gsub("[^0-9]", "", res$term))
  res$Outcome <- label
  return(data.table(res))
}

# Extract Data
es_plot_data <- rbindlist(list(
  extract_es(es_lust_ips, "LUST (IPS-Weighted)"),
  extract_es(es_exit_ips, "Exit (IPS-Weighted)")
))

if (nrow(es_plot_data) > 0) {
  es_plot <- ggplot(es_plot_data, aes(x = year, y = estimate, color = Outcome)) +
    # Policy Change Line (Between 1998 and 1999)
    geom_vline(xintercept = 1998.5, linetype = "dashed", color = "gray30") +
    geom_hline(yintercept = 0, color = "black", linewidth = 0.3) +
    # Confidence Intervals
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = Outcome), alpha = 0.15, color = NA) +
    geom_line(linewidth = 1) +
    geom_point(size = 1.5) +
    scale_color_manual(values = c("LUST (IPS-Weighted)" = "#D55E00", "Exit (IPS-Weighted)" = "#0072B2")) +
    scale_fill_manual(values = c("LUST (IPS-Weighted)" = "#D55E00", "Exit (IPS-Weighted)" = "#0072B2")) +
    facet_wrap(~Outcome, scales = "free_y", ncol = 1) +
    labs(
      title = "Event Study: IPS-Weighted DiD Estimates (Annual)",
      subtitle = sprintf("95%% %s Confidence Intervals", 
                         ifelse(USE_BOOTSTRAP, "Wild Cluster Bootstrap", "Clustered SE")),
      x = "Year",
      y = "Treatment Effect",
      caption = "Reference Year: 1998. Dashed line marks policy implementation."
    ) +
    theme_pub() +
    theme(legend.position = "none")
  
  ggsave(file.path(OUTPUT_FIGURES, "G1_Event_Study_IPS_Annual.png"),
         es_plot, width = 10, height = 8, dpi = 300, bg = "white")
  cat("Saved: G1_Event_Study_IPS_Annual.png\n")
}

#------------------------------------------------------------------------------
# G.2: Hazard Trend Plots (Annual)
#------------------------------------------------------------------------------
cat("\n--- G.2: Hazard Trend Plots (Annual) ---\n")

# Combine hazard datasets created in Part C
hazard_plot_data <- rbindlist(list(
  hazard_lust[, .(panel_year, Group = ifelse(texas_treated, "Texas", "Control"), hazard = H_bar, outcome = "LUST")],
  hazard_exit[, .(panel_year, Group = ifelse(texas_treated, "Texas", "Control"), hazard = H_bar, outcome = "Exit")],
  hazard_retrofit[, .(panel_year, Group = ifelse(texas_treated, "Texas", "Control"), hazard = H_bar, outcome = "Retrofit")]
), use.names = TRUE)

hazard_trends <- ggplot(hazard_plot_data, aes(x = panel_year, y = hazard, color = Group)) +
  geom_line(linewidth = 1.2) +
  geom_vline(xintercept = 1998.5, linetype = "dashed", color = "black") +
  facet_wrap(~outcome, scales = "free_y", ncol = 1) +
  scale_color_manual(values = c("Texas" = "#D55E00", "Control" = "#0072B2")) +
  labs(
    title = "Time-Average Hazard Rates by Outcome",
    subtitle = "Deaner & Ku (2024) transformation (Annual Aggregation)",
    x = "Year",
    y = "Time-Average Hazard Rate",
    caption = "Vertical line: Policy Implementation"
  ) +
  theme_pub()

ggsave(file.path(OUTPUT_FIGURES, "G2_Hazard_Trends_Annual.png"),
       hazard_trends, width = 12, height = 10, dpi = 300, bg = "white")
cat("Saved: G2_Hazard_Trends_Annual.png\n")

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
      subtitle = "Texas x Post x Age Bin Interaction Coefficients",
      x = "Tank Age Bin (years)",
      y = "Treatment Effect",
      caption = "Reference: 0-5 years. Error bars: 95% CI."
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
  outcome = c("Double-Wall", "Capacity Up", "Upsize Prob", "Downsize Prob", "Net Tank Delta"),
  estimate = c(
    coef(did_dw_upgrade)["did_term"],
    coef(did_capacity_change)["did_term"],
    coef(did_upsize)["did_term"],
    coef(did_downsize)["did_term"],
    coef(did_net_tank_change)["did_term"]
  ),
  se = c(
    se(did_dw_upgrade)["did_term"],
    se(did_capacity_change)["did_term"],
    se(did_upsize)["did_term"],
    se(did_downsize)["did_term"],
    se(did_net_tank_change)["did_term"]
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
    subtitle = "Texas x Post Coefficients (95% CI)",
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
# PART H: STANDARD SURVIVAL DID (COX PROPORTIONAL HAZARDS)
#==============================================================================

cat("\n")
cat("====================================================================\n")
cat("PART H: Standard Survival DiD (Time-to-Event Analysis)\n")
cat("====================================================================\n")

#------------------------------------------------------------------------------
# H.1: Survival Data Preparation (Risk Sets)
#------------------------------------------------------------------------------
cat("\n--- H.1: Defining Risk Sets (Time-to-First-Event) ---\n")

# Filter 1: Incumbents only (as requested) & Motor Fuel (Standard sample)
# Filter 2: Remove observations AFTER the first event (Risk set definition)
# Note: In counting process format, subjects leave the risk set after the event.

# --- Risk Set for First Leak ---
# Keep observation if: It is the year of the first leak OR no leak has occurred yet.
cox_lust_data <- mf_data[
  cohort == "Incumbent" & 
  (has_previous_leak == 0) # Exclude years after the first leak occurred
]

# --- Risk Set for First Closure ---
# Keep observation if: It is the year of first closure OR no closure has occurred yet.
cox_exit_data <- mf_data[
  cohort == "Incumbent" & 
  (has_previous_closure == 0)
]

cat(sprintf("Survival Analysis Samples (Incumbents):\n"))
cat(sprintf("  Time-to-First-Leak N: %s (Events: %s)\n", 
            format(nrow(cox_lust_data), big.mark=","),
            format(sum(cox_lust_data$event_first_leak), big.mark=",")))
cat(sprintf("  Time-to-First-Exit N: %s (Events: %s)\n", 
            format(nrow(cox_exit_data), big.mark=","),
            format(sum(cox_exit_data$event_first_closure), big.mark=",")))

#------------------------------------------------------------------------------
# H.2: Main Cox DiD Estimates
#------------------------------------------------------------------------------
cat("\n--- H.2: Estimating Cox DiD Models ---\n")

# Model Specification:
# Surv(Start, Stop, Event) ~ Treatment + Post + Treatment:Post + Strata(State)
# We use panel_year as time. Start = year-1, Stop = year.
# Standard errors clustered by State.

# 1. Time-to-First-Leak
cox_did_lust <- coxph(
  Surv(panel_year - 1, panel_year, event_first_leak) ~ 
    texas_treated * post_1999 + age_bins + has_single_walled_dec + 
    total_capacity_dec + strata(state), # Stratified baseline hazard by state
  data = cox_lust_data,
  cluster = state
)

# 2. Time-to-First-Closure
cox_did_exit <- coxph(
  Surv(panel_year - 1, panel_year, event_first_closure) ~ 
    texas_treated * post_1999 + age_bins + has_single_walled_dec + 
    total_capacity_dec + strata(state),
  data = cox_exit_data,
  cluster = state
)

# Extract Results
get_cox_res <- function(model, label) {
  res <- broom::tidy(model, exponentiate = FALSE, conf.int = TRUE)
  res <- data.table(res)
  target <- res[term == "texas_treated:post_1999"]
  data.table(
    Outcome = label,
    Coef = target$estimate,
    HR = exp(target$estimate), # Hazard Ratio
    SE = target$std.error,
    P_Val = target$p.value,
    CI_Lower_HR = exp(target$conf.low),
    CI_Upper_HR = exp(target$conf.high)
  )
}

cox_main_results <- rbind(
  get_cox_res(cox_did_lust, "Time-to-First-Leak"),
  get_cox_res(cox_did_exit, "Time-to-First-Closure")
)

save_simple_csv_table(
  results_dt = cox_main_results,
  base_name = "H2_Cox_Survival_DiD_Main",
  title = "Cox Proportional Hazards DiD (Stratified by State)",
  notes = c("Sample: Incumbent Motor Fuel Facilities.",
            "Controls: Age Bins, Wall Type, Capacity.",
            "HR > 1 indicates increased hazard (faster time to event).")
)

#------------------------------------------------------------------------------
# H.3: Dynamic Survival Event Study (Parallel Trends Test)
#------------------------------------------------------------------------------
cat("\n--- H.3: Estimating Dynamic Cox Models (Event Study) ---\n")

# To verify parallel trends in hazards, we interact Treatment with Year dummies.
# If Pre-Treatment coefficients are ~0 (HR ~ 1), hazards were parallel.

estimate_cox_es <- function(dt, event_col, outcome_name) {
  
  # Ensure event column is numeric for Surv()
  dt[, event_num := get(event_col)]
  
  # create factor for year, setting 1998 as reference
  dt[, year_fac := factor(panel_year)]
  dt[, year_fac := relevel(year_fac, ref = "1998")]
  
  # Formula: Interaction of Texas with every year
  # Note: Main effect of 'year' is absorbed by the baseline hazard (counting process)
  # Main effect of 'texas' is absorbed if we stratify, OR we can estimate it.
  # Here we include texas_treated + texas_treated:year_fac
  
  fmla <- as.formula(paste0(
    "Surv(panel_year - 1, panel_year, event_num) ~ ",
    "texas_treated * year_fac + strata(state) + cluster(state)"
  ))
  
  fit <- coxph(fmla, data = dt)
  
  # Extract Coefficients
  tidy_res <- broom::tidy(fit, conf.int = TRUE)
  dt_res <- data.table(tidy_res)
  
  # Filter for interactions (format: "texas_treated:year_fac1995")
  dt_res <- dt_res[grepl("texas_treated:year_fac", term)]
  
  # Extract Year
  dt_res[, year := as.numeric(gsub("texas_treated:year_fac", "", term))]
  dt_res[, Outcome := outcome_name]
  
  # Add Reference Year
  ref_row <- data.table(
    term = "Ref", estimate = 0, std.error = 0, statistic = 0, 
    p.value = 1, conf.low = 0, conf.high = 0, year = 1998, Outcome = outcome_name
  )
  
  return(rbind(dt_res, ref_row))
}

# Run Event Studies
# Warning: This can be computationally intensive. 
# We subset years to relevant window to ensure convergence/speed if needed.
# (Here we use the full 1990-2015 window filtered in Part 2)

cat("  Estimating dynamic Cox for LUST...\n")
cox_es_lust <- estimate_cox_es(cox_lust_data, "event_first_leak", "First Leak")

cat("  Estimating dynamic Cox for Exit...\n")
cox_es_exit <- estimate_cox_es(cox_exit_data, "event_first_closure", "First Closure")

#------------------------------------------------------------------------------
# H.4: Visualization (Cox Event Study)
#------------------------------------------------------------------------------
cat("\n--- H.4: Plotting Survival Event Studies ---\n")

cox_plot_data <- rbind(cox_es_lust, cox_es_exit)

# Filter for readable window (e.g. 1990-2010) to avoid noisy tails
cox_plot_data <- cox_plot_data[year >= 1990 & year <= 2010]

surv_es_plot <- ggplot(cox_plot_data, aes(x = year, y = estimate, color = Outcome)) +
  # Reference Lines
  geom_vline(xintercept = 1998.5, linetype = "dashed", color = "gray30") +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.3) +
  
  # Confidence Intervals
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = Outcome), alpha = 0.15, color = NA) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.5) +
  
  # Colors
  scale_color_manual(values = c("First Leak" = "#D55E00", "First Closure" = "#0072B2")) +
  scale_fill_manual(values = c("First Leak" = "#D55E00", "First Closure" = "#0072B2")) +
  
  facet_wrap(~Outcome, scales = "free_y", ncol = 1) +
  
  labs(
    title = "Survival DiD Event Study (Cox Proportional Hazards)",
    subtitle = "Log-Hazard Ratio (Texas vs Control) over Time",
    x = "Year",
    y = "Log-Hazard Difference",
    caption = "Reference Year: 1998. \nInterpretation: 0 implies parallel hazards. >0 implies higher hazard in Texas relative to controls."
  ) +
  theme_pub() +
  theme(legend.position = "none")

ggsave(file.path(OUTPUT_FIGURES, "H4_Survival_DiD_EventStudy.png"),
       surv_es_plot, width = 10, height = 8, dpi = 300, bg = "white")

cat("Saved: H4_Survival_DiD_EventStudy.png\n")


#==============================================================================
# FINAL SUMMARY
#==============================================================================

cat("\n")
cat("====================================================================\n")
cat("ANALYSIS COMPLETE\n")
cat("====================================================================\n\n")

cat("Analysis run on Annual Panel data (1990-2015).\n")
cat(sprintf("Bootstrap Mode: %s\n", ifelse(USE_BOOTSTRAP, "ENABLED", "DISABLED")))
cat(sprintf("Outputs saved to: %s\n", OUTPUT_FIGURES))


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