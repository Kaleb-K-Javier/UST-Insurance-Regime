# ==============================================================================
# CONSOLIDATED UST FACILITY BEHAVIOR ANALYSIS - REFACTORED v2.0
# Comprehensive Descriptive Statistics, Survival Analysis, and DCM Diagnostics
# 
# MAJOR CHANGES IN v2.0:
# - Extended time window: 1970-2023 (30-year pre-trends)
# - Cohort analysis: Incumbent (pre-2000) vs. Entrant (post-2000)
# - 1999 baseline standardization for all balance checks
# - Premium/claims data integration
# - Risk factor validation with regression analysis
# - Capital vintage analysis
# - LUST claims characterization
# - NEW: Discrete choice timing diagnostics (Section 4.5) for Dewey assessment
#
# Data Source: facility_leak_behavior_monthly.csv
# Output: 15 tables + 14 figures (academic + slides)
# ==============================================================================



# ==============================================================================
# 00_SETUP: LIBRARIES AND CONFIGURATION
# ==============================================================================

# Clear environment
rm(list = ls())
gc()

# Set options
options(scipen = 999, digits = 4)
set.seed(123456)

# Suppress startup messages
suppressPackageStartupMessages({
  # Core data manipulation
  library(data.table)
  
  # Visualization
  library(ggplot2)
  library(viridis)
  library(scales)
  library(patchwork)
  library(gridExtra)
  library(cowplot)
  
  # Tables
  library(gt)
  library(kableExtra)
  
  # Survival analysis
  library(survival)
  library(survminer)
  library(cmprsk)
  library(coxme)
  
  # Statistics and modeling
  library(fixest)      # NEW: For fixed effects models in risk validation
  library(broom)
  library(sandwich)
  library(lmtest)
  
  # Utilities
  library(here)
  library(lubridate)
  library(stringr)
  library(tidyverse)
  library(jsonlite)
  
  # DCM utilities (for diagnostics only)
  library(Matrix)
  library(numDeriv)
  library(ggridges)
  library(ggalluvial)
})

setDTthreads(10)  # Adjust based on your server's cores


# Verify configuration
cat(sprintf("data.table configured to use %d threads\n", getDTthreads()))
cat(sprintf("Available cores: %d\n", parallel::detectCores()))
options(datatable.optimize = 2)  # Enable all optimizations

# Record script start time
script_start_time <- Sys.time()

# Initialize runtime tracking
section_times <- list()
section_start_time <- Sys.time()

cat("\n", rep("=", 80), "\n", sep = "")
cat("CONSOLIDATED UST FACILITY BEHAVIOR ANALYSIS v2.0\n")
cat("Script started:", format(script_start_time, "%Y-%m-%d %H:%M:%S"), "\n")
cat(rep("=", 80), "\n\n", sep = "")

# Create output directories
dir.create(here("Output", "Tables"), recursive = TRUE, showWarnings = FALSE)
dir.create(here("Output", "Figures"), recursive = TRUE, showWarnings = FALSE)
dir.create(here("Output", "Figures", "Slides"), recursive = TRUE, showWarnings = FALSE)
dir.create(here("Output", "Captions"), recursive = TRUE, showWarnings = FALSE)
dir.create(here("Output", "Manifests"), recursive = TRUE, showWarnings = FALSE)
dir.create(here("Output", "Models"), recursive = TRUE, showWarnings = FALSE)

cat("Output directories created successfully.\n\n")

# ==============================================================================
# PUBLICATION THEMES AND COLOR PALETTES
# ==============================================================================

# Academic journal theme (for articles)
theme_academic <- function(base_size = 11, base_family = "sans") {
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      plot.title = element_text(size = rel(1.1), face = "bold", hjust = 0, margin = margin(0, 0, 10, 0)),
      plot.subtitle = element_text(size = rel(0.9), color = "gray30", hjust = 0, margin = margin(0, 0, 10, 0)),
      plot.caption = element_text(size = rel(0.7), color = "gray50", hjust = 1, margin = margin(10, 0, 0, 0)),
      axis.title = element_text(size = rel(0.95), face = "bold"),
      axis.text = element_text(size = rel(0.85)),
      axis.line = element_line(color = "gray80", linewidth = 0.5),
      legend.position = "bottom",
      legend.title = element_text(size = rel(0.9), face = "bold"),
      legend.text = element_text(size = rel(0.85)),
      legend.key.width = unit(1.5, "cm"),
      panel.grid.major.y = element_line(color = "gray90", linewidth = 0.25),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      strip.text = element_text(face = "bold", size = rel(0.9)),
      strip.background = element_rect(fill = "gray95", color = NA)
    )
}

# Beamer presentation theme (for slides)
theme_slides <- function(base_size = 18, base_family = "sans") {
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      plot.title = element_text(size = rel(1.2), face = "bold", hjust = 0.5, margin = margin(0, 0, 15, 0)),
      plot.subtitle = element_text(size = rel(0.9), hjust = 0.5, margin = margin(0, 0, 10, 0)),
      axis.title = element_text(size = rel(1.0), face = "bold"),
      axis.text = element_text(size = rel(0.9)),
      axis.line = element_line(color = "black", linewidth = 1),
      legend.position = "bottom",
      legend.title = element_text(size = rel(1.0), face = "bold"),
      legend.text = element_text(size = rel(0.95)),
      legend.key.width = unit(2, "cm"),
      panel.grid.major = element_line(color = "gray85", linewidth = 0.5),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      strip.text = element_text(face = "bold", size = rel(1.0)),
      strip.background = element_rect(fill = "gray90", color = NA)
    )
}

# Set default theme for academic outputs
theme_set(theme_academic())

# Color palettes (colorblind-safe, following Okabe-Ito)
treatment_colors <- c(
  "Texas" = "#D55E00",           # Orange
  "Other Treated" = "#009E73",   # Green
  "Control" = "#0072B2"          # Blue
)

period_colors <- c(
  "Pre-Policy" = "#56B4E9",      # Light blue
  "Post-Policy" = "#E69F00"      # Yellow-orange
)

# Viridis for continuous scales (colorblind-safe)
continuous_colors <- viridis::viridis(100)

cat("Publication themes and color palettes configured.\n\n")

# ==============================================================================
# GLOBAL ANALYSIS CONFIGURATION (NEW)
# ==============================================================================

cat("Setting global analysis parameters...\n")

# Treatment analysis mode
TREATMENT_ANALYSIS <- "texas_only"  # or "all_treated"

# Risk score method
RISK_SCORE_METHOD <- "empirical_hazard"  #"ml_predictions"  # or "empirical_hazard" or "simple_formula"

# Data integration toggles
USE_PREMIUM_DATA <- TRUE   # Mid-Continent rate filing data
USE_CLAIMS_DATA <- TRUE    # LUST cleanup cost data

# Dewey data integration assessment
ASSESS_DEWEY_TIMING <- TRUE   # Generate discrete choice timing diagnostics
DEWEY_START_YEAR <- 2014      # Earliest reliable Dewey data

if (TREATMENT_ANALYSIS == "texas_only") {
  cat("Analysis mode: Texas vs. Control only\n")
  treated_states_analysis <- "Texas"
  treatment_label <- "Texas"
} else if (TREATMENT_ANALYSIS == "all_treated") {
  cat("Analysis mode: All treated states vs. Control\n")
  treated_states_analysis <- c("Texas", "Wisconsin", "New Jersey", 
                                "Michigan", "Iowa", "Florida", 
                                "Arizona", "Connecticut")
  treatment_label <- "All Treated"
}

cat(sprintf("Treatment group: %s\n", treatment_label))
cat(sprintf("Risk score method: %s\n", RISK_SCORE_METHOD))
cat(sprintf("Premium integration: %s\n", ifelse(USE_PREMIUM_DATA, "ENABLED", "DISABLED")))
cat(sprintf("Claims integration: %s\n", ifelse(USE_CLAIMS_DATA, "ENABLED", "DISABLED")))
cat(sprintf("Dewey timing assessment: %s\n\n", ifelse(ASSESS_DEWEY_TIMING, "ENABLED", "DISABLED")))



# ==============================================================================
# HELPER FUNCTIONS
# ==============================================================================

# Section timing tracker
track_section_time <- function(section_name) {
  current_time <- Sys.time()
  elapsed <- as.numeric(difftime(current_time, section_start_time, units = "mins"))
  section_times[[section_name]] <<- elapsed
  section_start_time <<- current_time
  cat(sprintf("\n✓ %s completed in %.1f minutes\n\n", section_name, elapsed))
}

# Caption export function
export_caption <- function(table_num, title, caption, notes, model_link = NULL, id_link = NULL) {
  content <- paste0(
    "Title: ", title, "\n\n",
    "Caption: ", caption, "\n\n",
    "Notes: ", notes, "\n"
  )
  
  if (!is.null(model_link)) {
    content <- paste0(content, "\nModel: ", model_link, "\n")
  }
  
  if (!is.null(id_link)) {
    content <- paste0(content, "\nIdentification: ", id_link, "\n")
  }
  
  writeLines(content, here("Output", "Captions", paste0(table_num, "_caption.txt")))
}

# Table saving function (gt + CSV)
save_table <- function(gt_obj, data_obj, filename_base) {
  # Save gt object as HTML
  gt::gtsave(gt_obj, here("Output", "Tables", paste0(filename_base, ".html")))
  
  # Save data as CSV
  fwrite(data_obj, here("Output", "Tables", paste0(filename_base, ".csv")))
  
  # Save LaTeX version
  gt_obj %>%
    as_latex() %>%
    as.character() %>%
    writeLines(here("Output", "Tables", paste0(filename_base, ".tex")))
}

# Figure saving function (academic + slides)
save_figure <- function(plot_obj, filename_base, width = 10, height = 6, 
                        create_slides = TRUE) {
  # Academic version (PNG + PDF)
  ggsave(here("Output", "Figures", paste0(filename_base, ".png")),
         plot_obj, width = width, height = height, dpi = 300)
  ggsave(here("Output", "Figures", paste0(filename_base, ".pdf")),
         plot_obj, width = width, height = height)
  
  # Slides version (larger text)
  if (create_slides) {
    plot_slides <- plot_obj + theme_slides()
    ggsave(here("Output", "Figures", "Slides", paste0(filename_base, "_slides.png")),
           plot_slides, width = width * 1.2, height = height * 1.2, dpi = 300)
    ggsave(here("Output", "Figures", "Slides", paste0(filename_base, "_slides.pdf")),
           plot_slides, width = width * 1.2, height = height * 1.2)
  }
}

cat("Helper functions loaded.\n\n")

# ==============================================================================
# SECTION 01: DATA LOADING & COHORT DEFINITION
# ==============================================================================

cat(rep("=", 80), "\n", sep = "")
cat("SECTION 01: DATA LOADING & COHORT DEFINITION\n")
cat(rep("=", 80), "\n\n")

# Load facility-level panel data
cat("Loading facility-level panel data...\n")
facility_leak_behavior <- fread(here("Data", "Processed", "facility_leak_behavior_monthly.csv"))

# --- SURGICAL FIX: Reconstruct Composite Panel ID ---
# The 00 script dropped panel_id during backbone creation (Section 3).
# We must recreate it to ensure unique identification across states.
if (!"panel_id" %in% names(facility_leak_behavior)) {
  cat("  ⚠ Reconstructing missing 'panel_id' from 'facility_id' + 'state'...\n")
  facility_leak_behavior[, panel_id := paste(facility_id, state, sep = "_")]
}

cat(sprintf("Data loaded: %s facilities, %s observations\n",
            format(uniqueN(facility_leak_behavior$panel_id), big.mark = ","),
            format(nrow(facility_leak_behavior), big.mark = ",")))

# Define control and treated states
control_states <- c("Alabama", "Arkansas", "Colorado", "Delaware", 
                   "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", 
                   "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", 
                   "Massachusetts", "Minnesota", "Mississippi", "Missouri", 
                   "Montana", "Nebraska", "Nevada", "New Hampshire", 
                   "New Mexico", "New York", "North Carolina", "North Dakota", 
                   "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", 
                   "South Carolina", "South Dakota", "Tennessee", "Utah", 
                   "Vermont", "Virginia", "Washington", "West Virginia", 
                   "Wyoming")

treated_states <- c("Texas", "Wisconsin", "New Jersey", "Michigan", 
                   "Iowa", "Florida", "Arizona", "Connecticut")

# Create treatment indicators
facility_leak_behavior[, `:=`(
  treatment_group = fcase(
    state == "Texas", "Texas",
    state %in% treated_states, "Other Treated",
    state %in% control_states, "Control",
    default = NA_character_
  )
)]

cat("Treatment groups assigned.\n")
# ==============================================================================
# SECTION 01A: COHORT DEFINITION & IN-PLACE SAMPLE TAGGING (MEMORY OPTIMIZED)
# ==============================================================================
# Purpose: Define cohorts and flag analysis samples DIRECTLY in the main panel
#          to avoid creating massive duplicate objects in memory.
# ==============================================================================

track_section_time("Start Section 01A")

cat("Defining cohorts and tagging analysis samples (In-Place Operation)...\n")

# ------------------------------------------------------------------------------
# 1. Define Entry Timing (Incumbent vs. Entrant) - IN PLACE
# ------------------------------------------------------------------------------
# Calculate first appearance year for each facility directly in main table
facility_leak_behavior[, first_panel_year := min(panel_year), by = panel_id]

# Assign Cohort and installation year by reference (:=)
# This adds columns without copying the dataframe
facility_leak_behavior[, `:=`(
  cohort = fifelse(first_panel_year < 2000, "Incumbent", "Entrant"),
  
  cohort_type = fcase(
    first_panel_year < 2000 & is_motor_fuel == 1, "Incumbent Motor Fuel",
    first_panel_year < 2000 & is_motor_fuel == 0, "Incumbent Non-Motor Fuel",
    first_panel_year >= 2000 & is_motor_fuel == 1, "Entrant Motor Fuel",
    first_panel_year >= 2000 & is_motor_fuel == 0, "Entrant Non-Motor Fuel",
    default = "Unknown"
  ),
  
  # Approximate install year (for vintage charts)
  install_year = panel_year - floor(avg_tank_age)
)]

# ------------------------------------------------------------------------------
# 2. Tag Analysis Sample Rows (FLAGGING ONLY)
# ------------------------------------------------------------------------------
# instead of creating 'analysis_sample <- ...' (which duplicates memory),
# we create a boolean flag 'is_analysis_sample'.

cat("Tagging rows for Texas vs. Control analysis...\n")

# Pre-calculate validity conditions to keep logic clean
# 1. Geography: Texas OR Control States
condition_geo <- facility_leak_behavior$state == "Texas" | 
                 facility_leak_behavior$state %in% control_states

# 2. Time: 1970-2023 (Full window)
condition_time <- facility_leak_behavior$panel_year >= 1970 & 
                  facility_leak_behavior$panel_year <= 2023

# 3. Data Quality: Valid Tank Age (0-120) -- UPDATED
#    Increased ceiling to 120 to capture valid historical tanks >100 years old
condition_quality <- !is.na(facility_leak_behavior$avg_tank_age) & 
                     facility_leak_behavior$avg_tank_age >= 0 & 
                     facility_leak_behavior$avg_tank_age <= 120

# Apply flag
facility_leak_behavior[, is_analysis_sample := (condition_geo & condition_time & condition_quality)]

# Cleanup temporary vectors to free memory immediately
rm(condition_geo, condition_time, condition_quality)
gc()

# ------------------------------------------------------------------------------
# 3. Create 1999 Baseline Snapshot (Minimal Extract)
# ------------------------------------------------------------------------------
# We MUST create a separate object for the baseline because it is facility-level 
# (one row per ID), whereas the main panel is facility-month. 
# However, this object is small (~50k rows), so it won't crash the server.

cat("Creating 1999 Baseline Snapshot from tagged sample...\n")

baseline_1999 <- facility_leak_behavior[
  panel_year == 1999 &               # The baseline year
  is_analysis_sample == TRUE &       # Only valid analysis rows
  active_tanks > 0 &                 # Must be active
  cohort == "Incumbent",             # Only incumbents exist in 1999
  .SD[1],                            # Take 1st obs (unique facility)
  by = panel_id
]

# ------------------------------------------------------------------------------
# 4. Diagnostics & Summary (Using Flags)
# ------------------------------------------------------------------------------
# We calculate counts using the flags instead of nrow(analysis_sample)

n_analysis_ids <- uniqueN(facility_leak_behavior[is_analysis_sample == TRUE, panel_id])
n_incumbent_ids <- uniqueN(facility_leak_behavior[is_analysis_sample == TRUE & cohort == "Incumbent", panel_id])
n_motor_fuel_ids <- uniqueN(facility_leak_behavior[is_analysis_sample == TRUE & cohort == "Incumbent" & is_motor_fuel == 1, panel_id])

cat(sprintf("\nMEMORY-OPTIMIZED COHORT SUMMARY:\n"))
cat(sprintf("--------------------------------\n"))
cat(sprintf("  Analysis Universe (Tagged Rows): %s facilities\n", format(n_analysis_ids, big.mark = ",")))
cat(sprintf("  Incumbent Sample (Subset):       %s facilities\n", format(n_incumbent_ids, big.mark = ",")))
cat(sprintf("  Motor Fuel Incumbents:           %s facilities\n", format(n_motor_fuel_ids, big.mark = ",")))

cat(sprintf("\n  1999 Baseline Snapshot:\n"))
cat(sprintf("    Total Facilities:              %s\n", format(nrow(baseline_1999), big.mark = ",")))
cat(sprintf("    Texas (Treated):               %s\n", format(sum(baseline_1999$state == "Texas"), big.mark = ",")))

cat("\nNOTE: 'analysis_sample' object was NOT created to save RAM.")
cat("\n      Use 'facility_leak_behavior[is_analysis_sample == TRUE]' downstream.\n")

track_section_time("Section 01A: Cohort Definition")



cat(rep("=", 80), "\n", sep = "")
cat("SECTION 01B: PREMIUM & RISK DATA INTEGRATION\n")
cat(rep("=", 80), "\n\n")

# Initialize risk score variable
facility_leak_behavior[, risk_score := NA_real_]
# [REPLACE PREMIUM LOADING BLOCK IN SECTION 01B]
# ==============================================================================
# SECTION 01B: PREMIUM & RISK DATA INTEGRATION (MONTHLY MERGE FIX)
# ==============================================================================
# Purpose: Integrate external premium data using EXACT Year-Month keys.
#          Handles mid-year premium changes (e.g., policy renewals) by 
#          matching specific months rather than averaging over the year.
# ==============================================================================

track_section_time("Start Section 01B")

# Initialize risk score variable
facility_leak_behavior[, risk_score := NA_real_]

# ==============================================================================
# SECTION 01B: PREMIUM & RISK DATA INTEGRATION (BOUNDS + VALIDATION)
# ==============================================================================
# Purpose: Integrate external premium data with upper/lower bounds.
#          Validates if premium-rated tank counts match panel active tanks.
# ==============================================================================

track_section_time("Start Section 01B")

# Initialize risk score variable
facility_leak_behavior[, risk_score := NA_real_]

# ==============================================================================
# SECTION 01B: PREMIUM & RISK DATA INTEGRATION (FLATTENED)
# ==============================================================================
# Purpose: Integrate external premium data (Linear Execution).
# ==============================================================================

track_section_time("Start Section 01B")

cat("Loading Mid-Continent premium data...\n")

# Initialize risk score variable
facility_leak_behavior[, risk_score := NA_real_]

# ==============================================================================
# SECTION 01B: PREMIUM & RISK DATA INTEGRATION (NEW)
# ==============================================================================
# ------------------------------------------------------------------------------
# DATA DICTIONARY: MID-CONTINENT PREMIUM ESTIMATES
# ------------------------------------------------------------------------------
# SOURCE:
#   Reconstructed from Mid-Continent Casualty Company rate filings (OK-SERFF)
#   covering four regulatory periods: 2006-2014, 2014-2019, 2019-2021, and 2021+.
#
# METHODOLOGY:
#   Tank-level "bottom-up" rating engine derived from filed actuarial manuals.
#   Formula: Premium = Base_Rate * ILF * (1 + Sum(Secondary_Risk_Loads))
#
# KEY ASSUMPTIONS:
#   1. Base Rate Stability: A fixed base rate of $300/tank (500k/1M coverage)
#      is applied across all periods.
#   2. Default Coverage: Missing coverage limits default to the statutory
#      1M/1M requirement (ILF = 1.18).
#   3. Risk Factor Availability:
#      - Included: Tank Age, Wall Type, Piping Construction, Leak Detection.
#      - Excluded (Data Unavailable): Flood Zone, Marina, Aquifer proximity (set to 0).
#   4. Schedule Rating: Discretionary schedule rating is modeled as a bound
#      range (+/- 40% of calculated premium), not a specific point estimate.
#   5. Policy Structure: Premiums are calculated based on tanks active at the
#      contract effective date. Mid-term closures are not pro-rated in this
#      monthly snapshot.
#
# VARIABLES MERGED:
#   - annual_premium: The calculated "Base Premium" (before schedule adjustments).
#   - premium_min/max: The theoretical floor/ceiling based on +/- 40% schedule rating.
#   - premium_n_tanks: The count of tanks rated in the premium engine (for validation).
# ------------------------------------------------------------------------------



# ------------------------------------------------------------------------------
# 1. Configuration & File Loading
# ------------------------------------------------------------------------------
# Path config (User: kaleb)
data_dir <- "C:/Users/kalebkja/UST/rate_filings_backup/Rate FIllings/Mid-Continent Casualty Company ­– 23418"

# List files
prem_files <- list.files(
  path = data_dir,
  pattern = "^texas_midcontinent_facility_year_premium_.*\\.csv$", 
  full.names = TRUE
)

# Load and Bind
cat(sprintf("  Found %d premium files. Reading...\n", length(prem_files)))
premium_raw <- rbindlist(lapply(prem_files, fread), fill = TRUE)

cat(sprintf("  Raw premium records loaded: %s\n", format(nrow(premium_raw), big.mark = ",")))

# ------------------------------------------------------------------------------
# 2. Data Preparation
# ------------------------------------------------------------------------------
# Standardize Column Names
setnames(premium_raw, "YEAR", "panel_year", skip_absent = TRUE)
setnames(premium_raw, "MONTH", "panel_month", skip_absent = TRUE) 

# Construct panel_id "ID_Texas"
premium_raw[, panel_id := paste(FACILITY_ID, "Texas", sep = "_")]

# Deduplicate (Safety Step)
premium_clean <- unique(premium_raw, by = c("panel_id", "panel_year", "panel_month"))

# Select Columns (Including Bounds and Tank Count)
merge_data <- premium_clean[, .(
  panel_id, 
  panel_year, 
  panel_month, 
  annual_premium = base_premium,
  premium_min = sched_min,          # Lower bound
  premium_max = sched_max,          # Upper bound
  premium_n_tanks = number_of_tanks, # Count from rating engine
  merge_check = 1
)]
raw_count = sum(merge_data$merge_check)
# ------------------------------------------------------------------------------
# 3. Merge Execution
# ------------------------------------------------------------------------------
facility_leak_behavior <- merge(
  facility_leak_behavior,
  merge_data,
  by = c("panel_id", "panel_year", "panel_month"),
  all.x = TRUE
)
merge_count = sum(facility_leak_behavior$merge_check,na.rm = TRUE)


cat(sprintf("  Premium data integrated for %s  of possible facilities.\n", 
            format(((merge_count/raw_count) * 100), big.mark = ",")))

# ------------------------------------------------------------------------------
# 4. Tank Count Validation
# ------------------------------------------------------------------------------
cat("\n  VALIDATION: Panel Active Tanks vs. Premium Rated Tanks\n")

# Compare counts only where premium data exists
validation_set <- facility_leak_behavior[!is.na(annual_premium)]

validation_set[, tank_diff := active_tanks - premium_n_tanks]

# Metrics
n_total_merged <- nrow(validation_set)
n_perfect_match <- validation_set[tank_diff == 0, .N]
n_mismatch <- validation_set[tank_diff != 0, .N]
pct_match <- 100 * n_perfect_match / n_total_merged

cat(sprintf("    Total Merged Months: %s\n", format(n_total_merged, big.mark=",")))
cat(sprintf("    Perfect Matches:     %s (%.1f%%)\n", format(n_perfect_match, big.mark=","), pct_match))
cat(sprintf("    Mismatches:          %s\n", format(n_mismatch, big.mark=",")))

# Print mismatch table (Will be empty if no mismatches)
cat("    Mismatch Distribution (Panel - Premium):\n")
print(table(validation_set[tank_diff != 0, tank_diff]))

# Flag mismatches in main dataset (NA where no premium data exists)
facility_leak_behavior[, premium_tank_mismatch := (active_tanks != premium_n_tanks)]

# Cleanup
rm(premium_raw, premium_clean, merge_data, validation_set)
gc()

# Load GRF risk predictions if available
# 1. Load Risk Scores (From Script A)
if (RISK_SCORE_METHOD == "ml_predictions") {
  cat("\nLoading GRF risk predictions...\n")
  
  # --- FIX: Point to Output/Results where Script A saves ---
  grf_file <- here("Output", "Results", "leak_hazard_rates_DML_ipw.rds")
  
if (file.exists(grf_file)) {
    grf_predictions <- readRDS(grf_file)
    setDT(grf_predictions)  
      cat(sprintf("  GRF predictions loaded: %s records\n", 
                format(nrow(grf_predictions), big.mark = ",")))
    
    # Merge predictions
    facility_leak_behavior <- merge(
      facility_leak_behavior,
      grf_predictions[, .(panel_id, panel_year, predicted_leak_prob, 
                         predicted_cost, risk_score_grf)],
      by = c("panel_id", "panel_year"),
      all.x = TRUE
    )
    
    # Use GRF risk scores where available
    facility_leak_behavior[!is.na(risk_score_grf), risk_score := risk_score_grf]
    
    cat(sprintf("  Facilities with GRF risk scores: %s\n",
                format(uniqueN(facility_leak_behavior[!is.na(risk_score), .(panel_id)]), 
                       big.mark = ",")))
  } else {
    cat("  ⚠ GRF predictions not found, falling back to empirical hazard\n")
    RISK_SCORE_METHOD <- "empirical_hazard"
  }
}

# Fallback: Empirical hazard model
if (RISK_SCORE_METHOD == "empirical_hazard" & all(is.na(facility_leak_behavior$risk_score))) {
  cat("\nCalculating empirical hazard risk scores...\n")
  
  # Calculate leak rates by age × wall type
  hazard_rates <- facility_leak_behavior[
    panel_year >= 1990 & panel_year < 2000,
    .(empirical_leak_rate = mean(leak_incident, na.rm = TRUE),
      n_obs = .N),
    by = .(age_bin = cut(avg_tank_age, breaks = c(0, 10, 20, 30, 40, 200), 
                         include.lowest = TRUE),
           wall_type)
  ]
  
  # Assign risk scores based on empirical rates
  facility_leak_behavior[, age_bin := cut(avg_tank_age, 
                                          breaks = c(0, 10, 20, 30, 40, 200),
                                          include.lowest = TRUE)]
  
  facility_leak_behavior <- merge(
    facility_leak_behavior,
    hazard_rates[, .(age_bin, wall_type, empirical_leak_rate)],
    by = c("age_bin", "wall_type"),
    all.x = TRUE
  )
  
  facility_leak_behavior[!is.na(empirical_leak_rate) & is.na(risk_score), 
                        risk_score := empirical_leak_rate]
  
  cat(sprintf("  Risk scores assigned via empirical hazard: %s facilities\n",
              format(uniqueN(facility_leak_behavior[!is.na(risk_score), .(panel_id)]), 
                     big.mark = ",")))
}

cat(sprintf("\nFinal risk score coverage: %.1f%%\n", 
            100 * mean(!is.na(facility_leak_behavior$risk_score))))


# [REPLACE LINES 345-368]
# Load LUST claims data if available
# [REPLACE CLAIMS LOADING BLOCK IN SECTION 01B]
# [REPLACE THE MERGE BLOCK WITH THIS CORRECTED VERSION]
# [REPLACE THE ENTIRE CLAIMS INTEGRATION BLOCK WITH THIS]
if (USE_CLAIMS_DATA) {
  cat("\nLoading LUST claims data...\n")
  
  claims_file <- here("Data", "Processed", "all_cleaned_claims.csv")
  
  if (file.exists(claims_file)) {
    lust_claims <- fread(claims_file)
    
    # 1. FIX: Convert State Abbreviations to Full Names
    #    This ensures "TN" matches "Tennessee" in the main panel
    state_lookup <- setNames(state.name, state.abb)
    lust_claims[, state := state_lookup[state]]
    
    # 2. Standardize column names for merge
    #    Map: claim_start_year -> panel_year
    #         claim_start_month -> panel_month
    #         total_cost_2023   -> cleanup_cost (Target variable for analysis)
    setnames(lust_claims, 
             old = c("claim_start_year", "claim_start_month", "total_cost_2023"), 
             new = c("panel_year", "panel_month", "cleanup_cost"),
             skip_absent = TRUE)
    
    # 3. Merge using explicit Facility/State/Time keys
    #    Now that 'state' is full name in both, this will succeed
    facility_leak_behavior <- merge(
      facility_leak_behavior,
      lust_claims[, .(facility_id, state, panel_year, panel_month, cleanup_cost, total_cost)], 
      by = c("facility_id", "state", "panel_year", "panel_month"),
      all.x = TRUE
    )
    
    cat(sprintf("  Claims integrated: %s incidents merged via Facility/State/Month.\n", 
                format(nrow(lust_claims), big.mark = ",")))
                
    # Validation: Verify costs are actually present
    cat(sprintf("  Validation: %s rows in main panel now have cleanup costs.\n",
            format(sum(!is.na(facility_leak_behavior$cleanup_cost)), big.mark = ",")))

  } else {
    cat("  WARNING: Claims file not found at", claims_file, "\n")
  }
}
track_section_time("Section 01B: Data Integration")




# ==============================================================================
# SECTION 02: SAMPLE CONSTRUCTION (MEMORY OPTIMIZED)
# ==============================================================================
# Purpose: Calculate sample attrition statistics without creating intermediate
#          data copies. Uses sequential filtering logic.
# ==============================================================================

track_section_time("Start Section 02")

cat(rep("=", 80), "\n", sep = "")
cat("SECTION 02: SAMPLE CONSTRUCTION\n")
cat(rep("=", 80), "\n\n")

# 1. Initialize vectors to store counts
#    We will calculate these sequentially
steps <- c(
  "1. Full panel data (1970-2023)",
  "2. Texas + control states only",
  "3. Years 1970-2023",
  "4. Valid tank age (0-120 years)",  # Updated to match Section 01A
  "5. Active facilities (tanks > 0)",
  "6. Incumbents only (pre-2000 entry)",
  "7. Motor fuel incumbents"
)

n_facilities <- numeric(length(steps))
n_rows <- numeric(length(steps))

# 2. Step-by-Step Calculation (Logical ANDs)
#    We define boolean vectors for each condition to avoid subsetting the data frame.

# Step 1: Full Data
# Note: Assuming facility_leak_behavior is the starting point
n_facilities[1] <- uniqueN(facility_leak_behavior$panel_id)
n_rows[1] <- nrow(facility_leak_behavior)

# Step 2: States (Texas + Control)
# Define condition vector (TRUE/FALSE)
cond_2 <- facility_leak_behavior$state %in% c(control_states, "Texas")
n_facilities[2] <- uniqueN(facility_leak_behavior$panel_id[cond_2])
n_rows[2] <- sum(cond_2)

# Step 3: Years (1970-2023)
# Combine with previous condition using &
cond_3 <- cond_2 & (facility_leak_behavior$panel_year >= 1970 & facility_leak_behavior$panel_year <= 2023)
n_facilities[3] <- uniqueN(facility_leak_behavior$panel_id[cond_3])
n_rows[3] <- sum(cond_3)

# Step 4: Valid Age (0-120)
cond_4 <- cond_3 & (!is.na(facility_leak_behavior$avg_tank_age) & 
                    facility_leak_behavior$avg_tank_age >= 0 & 
                    facility_leak_behavior$avg_tank_age <= 120)
n_facilities[4] <- uniqueN(facility_leak_behavior$panel_id[cond_4])
n_rows[4] <- sum(cond_4)

# Step 5: Active Facilities (tanks > 0)
cond_5 <- cond_4 & (facility_leak_behavior$active_tanks > 0)
n_facilities[5] <- uniqueN(facility_leak_behavior$panel_id[cond_5])
n_rows[5] <- sum(cond_5)

# Step 6: Incumbents Only
# Note: 'cohort' column was created in Section 01A
cond_6 <- cond_5 & (facility_leak_behavior$cohort == "Incumbent")
n_facilities[6] <- uniqueN(facility_leak_behavior$panel_id[cond_6])
n_rows[6] <- sum(cond_6)

# Step 7: Motor Fuel Only
cond_7 <- cond_6 & (facility_leak_behavior$is_motor_fuel == 1)
n_facilities[7] <- uniqueN(facility_leak_behavior$panel_id[cond_7])
n_rows[7] <- sum(cond_7)

# 3. Assemble Table
sample_construction <- data.table(
  Step = steps,
  Facilities = n_facilities,
  Facility_Months = n_rows
)

sample_construction[, Retention_Rate := round(100 * Facilities / Facilities[1], 1)]

# 4. Create GT Table
t01_academic <- sample_construction %>%
  gt() %>%
  tab_header(
    title = "Table 1: Sample Construction and Attrition",
    subtitle = "From full panel to motor fuel incumbent analysis sample"
  ) %>%
  cols_label(
    Step = "Sample Restriction",
    Facilities = "Unique Facilities",
    Facility_Months = "Facility-Month Obs.",
    Retention_Rate = "Retention (%)"
  ) %>%
  fmt_number(columns = c(Facilities, Facility_Months), decimals = 0, sep_mark = ",") %>%
  fmt_number(columns = Retention_Rate, decimals = 1) %>%
  tab_source_note(
    "Notes: Sample restricted to Texas and control states (excludes other treated states).
     Time window extended to 1970-2023. Valid age restricted to 0-120 years.
     Incumbent cohort defined as first appearance before 2000."
  )

# 5. Save Outputs
save_table(t01_academic, sample_construction, "T01_sample_construction")

export_caption(
  "T01",
  "Sample Construction and Attrition",
  "Sequential sample restrictions showing retention of facilities and observations.",
  sprintf("Final motor fuel incumbent sample: %s facilities, %s facility-months. Primary attrition from motor fuel restriction (%.1f%% retention).",
          format(sample_construction[7, Facilities], big.mark = ","),
          format(sample_construction[7, Facility_Months], big.mark = ","),
          sample_construction[7, Retention_Rate]),
  "See 00_create_facility_month_year_panel_11_06.r for panel construction",
  "Incumbent restriction ensures facilities were exposed to pre-policy regime"
)

# Cleanup
rm(cond_2, cond_3, cond_4, cond_5, cond_6, cond_7)
gc()

cat("Table 1 saved.\n")
track_section_time("Section 02: Sample Construction")

# ==============================================================================
# SECTION 02B: AGGREGATION DIAGNOSTICS (MEMORY OPTIMIZED)
# ==============================================================================
# Purpose: Empirical justification for annual aggregation.
#          Tests seasonality of Leaks vs. Exits and consistency across years.
# ==============================================================================

track_section_time("Start Section 02B")

cat(rep("=", 80), "\n", sep = "")
cat("SECTION 02B: AGGREGATION DIAGNOSTICS\n")
cat(rep("=", 80), "\n\n")

# 1. In-Place Variable Creation
# ------------------------------------------------------------------------------
cat("Creating time/state flags in-place...\n")
# Create binary flags for analysis (lightweight logicals)
facility_leak_behavior[, `:=`(
  texas_treated = (state == "Texas"),
  post_policy   = as.integer(panel_year >= 1999)
)]

# 2. Seasonality Analysis (Average Pattern)
# ------------------------------------------------------------------------------
cat("Calculating average monthly patterns...\n")
# Aggregates to 12 rows (Month 1-12)
monthly_patterns <- facility_leak_behavior[
  is_analysis_sample == TRUE,
  .(
    leak_pct = 100 * mean(leak_incident, na.rm = TRUE),
    exit_pct = 100 * mean(tanks_closed > 0, na.rm = TRUE),
    n_obs = .N
  ), 
  by = .(panel_month)
]
setorder(monthly_patterns, panel_month)

# 3. Figure 2B: The Average Monthly Cycle
# ------------------------------------------------------------------------------
cat("Generating Figure 2B: Average Seasonality...\n")
monthly_long <- melt(monthly_patterns, id.vars = "panel_month", 
                     measure.vars = c("leak_pct", "exit_pct"),
                     variable.name = "Outcome", value.name = "Rate")

monthly_long[, Outcome := fcase(
  Outcome == "leak_pct", "Leaks (Stochastic)",
  Outcome == "exit_pct", "Exits (Fiscal/Seasonal)"
)]

p_seasonality_avg <- ggplot(monthly_long, aes(x = panel_month, y = Rate, color = Outcome)) +
  geom_line(linewidth = 1.2) + geom_point(size = 3) +
  geom_vline(xintercept = 12, linetype = "dashed", color = "gray50") +
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
  scale_color_manual(values = c("Leaks (Stochastic)" = "#D55E00", 
                                "Exits (Fiscal/Seasonal)" = "#0072B2")) +
  labs(title = "Figure 2B: Average Event Seasonality (All Years)",
       subtitle = "Exits cluster in December; Leaks are random.",
       x = "Month", y = "Event Rate (%)") +
  theme_academic() + theme(legend.position = "bottom")

save_figure(p_seasonality_avg, "F02b_seasonality_average", width = 10, height = 6)

# 4. Yearly Heterogeneity Analysis (Addressing "Bigger Jumps")
# ------------------------------------------------------------------------------
cat("Calculating year-specific December spikes...\n")

# Calculate monthly rates PER YEAR (memory efficient aggregation)
yearly_monthly_stats <- facility_leak_behavior[
  is_analysis_sample == TRUE,
  .(exit_rate = mean(tanks_closed > 0, na.rm=TRUE)), 
  by = .(panel_year, panel_month)
]

# Calculate "December Spike Factor" for each year: (Dec Rate) / (Avg Monthly Rate)
december_spikes <- yearly_monthly_stats[, .(
  dec_rate = exit_rate[panel_month == 12],
  avg_rate = mean(exit_rate, na.rm=TRUE)
), by = panel_year]

december_spikes[, spike_factor := dec_rate / avg_rate]
# Handle years with 0 exits/undefined rates to avoid Inf
december_spikes[!is.finite(spike_factor), spike_factor := 1.0] 

# 5. Figure 2C: The "Spike" History
# ------------------------------------------------------------------------------
cat("Generating Figure 2C: Yearly Seasonality Trends...\n")

p_seasonality_trends <- ggplot(december_spikes, aes(x = panel_year, y = spike_factor)) +
  geom_hline(yintercept = 1, linetype = "solid", color = "gray50") +
  geom_line(color = "#0072B2", linewidth = 1) +
  geom_point(aes(size = dec_rate), color = "#0072B2", alpha = 0.8) +
  # Highlight the Policy Change
  geom_vline(xintercept = 1999, linetype = "dashed", color = "red") +
  annotate("text", x = 1999, y = max(december_spikes$spike_factor, na.rm=T), 
           label = "Policy Change", hjust = -0.1, color = "red", size = 3) +
  labs(
    title = "Figure 2C: Consistency of Year-End Exit Clustering",
    subtitle = "Ratio of December Exits to Annual Average (Spike Factor > 1 indicates clustering)",
    x = "Year", y = "December Spike Factor (Dec / Avg)",
    caption = "Size of point indicates absolute exit rate magnitude."
  ) +
  theme_academic()

save_figure(p_seasonality_trends, "F02c_seasonality_trends_yearly", width = 10, height = 6)

# 6. Statistical Summary & Export
# ------------------------------------------------------------------------------
t_test <- t.test(december_spikes$spike_factor, mu = 1, alternative = "greater")

cat(sprintf("\nSeasonality Consistency Check:\n"))
cat(sprintf("  Mean December Spike: %.2fx\n", mean(december_spikes$spike_factor, na.rm=T)))
cat(sprintf("  Years with Dec Spike > 1.5x: %d of %d\n", 
            sum(december_spikes$spike_factor > 1.5, na.rm=T), nrow(december_spikes)))
cat(sprintf("  T-test p-value: %.4f\n", t_test$p.value))

export_caption(
  "F02b_F02c",
  "Event Seasonality and Aggregation Diagnostics",
  "Figure 2B shows the average monthly cycle; Figure 2C shows the consistency of the December spike over time.",
  sprintf("Exits consistently cluster in December across the sample period (Mean Spike Factor = %.2fx). This confirms that annual aggregation aligns with the firm's fiscal decision-making cycle, even in years with lower absolute exit volumes.", 
          mean(december_spikes$spike_factor, na.rm=T)),
  "Consistent December clustering justifies annual panel construction.",
  "Variation in spike magnitude (Figure 2C) reflects unobserved heterogeneity handled by Fixed Effects."
)

# Cleanup
rm(monthly_patterns, monthly_long, yearly_monthly_stats, december_spikes, t_test)
gc()

track_section_time("Section 02B: Aggregation Diagnostics")


# ==============================================================================
# SECTION 03: FACILITY CHARACTERISTICS (1999 BASELINE)
# ==============================================================================
# Purpose: Create baseline characteristics table using the main panel and flags.
#          Re-extracts baseline data to capture variables added in Section 01B.
# ==============================================================================

track_section_time("Start Section 03")

cat(rep("=", 80), "\n", sep = "")
cat("SECTION 03: FACILITY CHARACTERISTICS (1999 BASELINE)\n")
cat(rep("=", 80), "\n\n")

cat("Re-extracting 1999 baseline to include new premium/risk variables...\n")

# 1. Re-create baseline_1999 with ALL current columns
#    (The previous baseline_1999 in Section 01A missed premiums added in 01B)
baseline_1999_full <- facility_leak_behavior[
  panel_year == 1999 & 
  is_analysis_sample == TRUE & 
  active_tanks > 0 & 
  cohort == "Incumbent",
  .SD[1], # Take first observation per facility
  by = panel_id
]
# ==============================================================================
# SECTION 03: COVARIATE BALANCE (ENTROPY BALANCING + JOINT TESTS)
# ==============================================================================
# Purpose: 1. Check baseline balance (Raw).
#          2. Correct imbalance using Entropy Balancing (Weighted).
#          3. Report Normalized Differences and Joint F-Tests for both.
# ==============================================================================

track_section_time("Start Section 03")

if (requireNamespace("ebal", quietly = TRUE)) {
  
  cat("Calculating Entropy Balancing weights...\n")
  
  # --- FIX: Standardize Variable Names ---
  # Script 00 creates 'double_share' (0-1), analysis needs 'pct_double_walled' (0-100)
  if ("double_share" %in% names(baseline_1999_full)) {
    baseline_1999_full[, pct_double_walled := double_share * 100]
  } else if ("double_tanks" %in% names(baseline_1999_full) & "active_tanks" %in% names(baseline_1999_full)) {
    baseline_1999_full[, pct_double_walled := 100 * double_tanks / active_tanks]
  }
  
  # 1. Prepare Data
  #    Ensure all variables exist before subsetting
  balance_vars <- c("active_tanks", "avg_tank_age", "max_tank_age", 
                    "pct_double_walled", "is_motor_fuel")
  
  bal_data <- baseline_1999_full[
    !is.na(treatment_group) & treatment_group %in% c("Texas", "Control"),
    c("panel_id", "treatment_group", balance_vars),
    with = FALSE
  ]
  bal_data <- na.omit(bal_data)
  
  # 2. Entropy Balancing
  #    Target: Match 1st moments (means) of Control to Treated (Texas)
  D <- as.integer(bal_data$treatment_group == "Texas")
  X <- as.matrix(bal_data[, ..balance_vars])
  
  eb_out <- ebal::ebalance(Treatment = D, X = X)
  
  # 3. Assign Weights
  bal_data[, ebal_weight := 1.0] # Texas = 1
  bal_data[treatment_group == "Control", ebal_weight := eb_out$w] # Control = Weights
  
  # 4. Joint Orthogonality Tests (Raw & Weighted)
  cat("Running Joint Orthogonality Tests...\n")
  bal_data[, is_treated := as.integer(treatment_group == "Texas")]
  
  # Raw F-Test
  model_raw <- fixest::feols(
    as.formula(paste("is_treated ~", paste(balance_vars, collapse = " + "))),
    data = bal_data, vcov = "hetero"
  )
  f_test_raw <- fixest::wald(model_raw, keep = balance_vars)
  
  # Weighted F-Test (Using weights in regression)
  model_weighted <- fixest::feols(
    as.formula(paste("is_treated ~", paste(balance_vars, collapse = " + "))),
    data = bal_data, weights = ~ebal_weight, vcov = "hetero"
  )
  f_test_weighted <- fixest::wald(model_weighted, keep = balance_vars)
  
  # 5. Calculate Balance Stats (Raw & Weighted)
  calc_balance <- function(dt, vars, w_col = NULL) {
    res <- list()
    for (v in vars) {
      g1 <- dt[treatment_group == "Texas"]
      g2 <- dt[treatment_group == "Control"]
      
      # Raw Stats
      m1_raw <- mean(g1[[v]])
      m2_raw <- mean(g2[[v]])
      # Pooled SD (Unweighted) - Standard denominator
      sd_pool <- sqrt((var(g1[[v]]) + var(g2[[v]])) / 2)
      nd_raw <- (m1_raw - m2_raw) / sd_pool
      
      # Weighted Stats
      w1 <- if(is.null(w_col)) rep(1, nrow(g1)) else g1[[w_col]]
      w2 <- if(is.null(w_col)) rep(1, nrow(g2)) else g2[[w_col]]
      
      m1_w <- weighted.mean(g1[[v]], w1)
      m2_w <- weighted.mean(g2[[v]], w2)
      nd_w <- (m1_w - m2_w) / sd_pool # Keep denominator fixed
      
      res[[v]] <- data.table(
        Variable = v,
        Mean_Tx = m1_raw, 
        Mean_C_Raw = m2_raw,
        NormDiff_Raw = nd_raw,
        Mean_C_Weighted = m2_w,
        NormDiff_Weighted = nd_w
      )
    }
    rbindlist(res)
  }
  
  balance_table <- calc_balance(bal_data, balance_vars, "ebal_weight")
  
  # 6. Create Consolidated GT Table
  t02a_combined <- balance_table %>%
    gt() %>%
    tab_header(
      title = "Table 2A: Covariate Balance (Raw vs. Entropy Balanced)",
      subtitle = "Comparison of Texas vs. Control means before and after weighting"
    ) %>%
    cols_label(
      Variable = "Characteristic",
      Mean_Tx = "Texas (Target)",
      Mean_C_Raw = "Control (Raw)",
      NormDiff_Raw = "ND (Raw)",
      Mean_C_Weighted = "Control (Weighted)",
      NormDiff_Weighted = "ND (Weighted)"
    ) %>%
    tab_spanner(label = "Unweighted Sample", columns = c(Mean_C_Raw, NormDiff_Raw)) %>%
    tab_spanner(label = "Entropy Balanced", columns = c(Mean_C_Weighted, NormDiff_Weighted)) %>%
    fmt_number(columns = c(Mean_Tx, Mean_C_Raw, NormDiff_Raw, Mean_C_Weighted, NormDiff_Weighted), decimals = 3) %>%
    # Highlight imbalances in Raw (Red)
    tab_style(
      style = list(cell_text(color = "red", weight = "bold")),
      locations = cells_body(columns = NormDiff_Raw, rows = abs(NormDiff_Raw) > 0.1)
    ) %>%
    # Highlight balance in Weighted (Green)
    tab_style(
      style = list(cell_text(color = "green", weight = "bold")),
      locations = cells_body(columns = NormDiff_Weighted, rows = abs(NormDiff_Weighted) < 0.001)
    ) %>%
    # Add Joint Test Statistics to Footer
    tab_source_note(
      sprintf("Joint Orthogonality Test (Raw): F-stat = %.2f (p = %.3f).", 
              f_test_raw$stat, f_test_raw$p)
    ) %>%
    tab_source_note(
      sprintf("Joint Orthogonality Test (Weighted): F-stat = %.2f (p = %.3f).", 
              f_test_weighted$stat, f_test_weighted$p)
    ) %>%
    tab_source_note("Notes: ND = Normalized Difference. Values > 0.1 indicate imbalance. Joint test p > 0.05 indicates balance.")
  
  save_table(t02a_combined, balance_table, "T02a_balance_combined")
  cat("Table 2A (Combined) saved.\n")
  
  # 7. Merge weights back to main panel
  cat("Merging weights into main panel...\n")
  weights_to_merge <- bal_data[, .(panel_id, ebal_weight)]
  
  # Safety check to prevent duplicates
  if ("ebal_weight" %in% names(facility_leak_behavior)) {
    facility_leak_behavior[, ebal_weight := NULL]
  }
  
  facility_leak_behavior <- merge(
    facility_leak_behavior, 
    weights_to_merge, 
    by = "panel_id", 
    all.x = TRUE
  )
  
} else {
  cat("Package 'ebal' not available.\n")
}

track_section_time("Section 03: Covariate Balance")


# ------------------------------------------------------------------------------
# Table 2B: Wall Composition (Corrected for Unknowns)
# ------------------------------------------------------------------------------
cat("Generating Table 2B (Wall Composition with Unknowns)...\n")

wall_composition <- baseline_1999_full[is_motor_fuel == 1, .(
  facilities = .N,
  
  # Facility-level classification (based on dominant type or mixed flag)
  fac_all_sw = sum(wall_type == "Single-Walled", na.rm = TRUE),
  fac_all_dw = sum(wall_type == "Double-Walled", na.rm = TRUE),
  fac_mixed  = sum(wall_type == "Mixed", na.rm = TRUE),
  fac_unknown= sum(wall_type == "Unknown", na.rm = TRUE),
  
  # Tank-level aggregates
  total_tanks = sum(active_tanks, na.rm = TRUE),
  total_sw    = sum(single_tanks, na.rm = TRUE),
  total_dw    = sum(double_tanks, na.rm = TRUE),
  
  # Calculate "Other" as the residual: Total - (SW + DW)
  # This captures tanks where wall_type is missing/undefined in the raw data
  total_other = sum(active_tanks, na.rm=TRUE) - (sum(single_tanks, na.rm=TRUE) + sum(double_tanks, na.rm=TRUE)),
  
  # Average facility-level share (DW / Active)
  avg_pct_dw = 100 * mean(double_tanks / active_tanks, na.rm = TRUE)
), by = treatment_group]

# Calculate percentages (Now summing to 100%)
wall_composition[, `:=`(
  pct_fac_all_sw = 100 * fac_all_sw / facilities,
  pct_fac_all_dw = 100 * fac_all_dw / facilities,
  pct_fac_mixed  = 100 * fac_mixed / facilities,
  
  pct_tanks_sw    = 100 * total_sw / total_tanks,
  pct_tanks_dw    = 100 * total_dw / total_tanks,
  pct_tanks_other = 100 * total_other / total_tanks
)]

t02b_academic <- wall_composition[treatment_group %in% c("Texas", "Control")] %>%
  gt() %>%
  tab_header(
    title = "Table 2B: Wall Composition Analysis (1999 Baseline)",
    subtitle = "Facility-level vs. tank-level classification"
  ) %>%
  cols_label(
    treatment_group = "Group",
    facilities = "N Facilities",
    fac_all_sw = "All SW", pct_fac_all_sw = "%",
    fac_all_dw = "All DW", pct_fac_all_dw = "%",
    fac_mixed = "Mixed", pct_fac_mixed = "%",
    total_tanks = "Total Tanks",
    total_sw = "SW", pct_tanks_sw = "%",
    total_dw = "DW", pct_tanks_dw = "%",
    total_other = "Unk/Oth", pct_tanks_other = "%",
    avg_pct_dw = "Avg % DW"
  ) %>%
  tab_spanner(label = "Facility Classification", 
              columns = c(fac_all_sw, pct_fac_all_sw, fac_all_dw, pct_fac_all_dw, 
                          fac_mixed, pct_fac_mixed)) %>%
  tab_spanner(label = "Tank-Level Counts", 
              columns = c(total_tanks, total_sw, pct_tanks_sw, total_dw, pct_tanks_dw,
                          total_other, pct_tanks_other)) %>%
  fmt_number(columns = c(facilities, fac_all_sw, fac_all_dw, fac_mixed,
                         total_tanks, total_sw, total_dw, total_other), 
             decimals = 0, sep_mark = ",") %>%
  fmt_number(columns = c(pct_fac_all_sw, pct_fac_all_dw, pct_fac_mixed,
                         pct_tanks_sw, pct_tanks_dw, pct_tanks_other, avg_pct_dw),
             decimals = 1) %>%
  tab_source_note(
    "Notes: SW = Single-Walled, DW = Double-Walled. 'Unk/Oth' includes tanks with missing or undefined wall types. 
     Tank-level percentages now sum to 100%."
  )

save_table(t02b_academic, wall_composition, "T02b_wall_composition")

export_caption(
  "T02b",
  "Wall Composition Analysis",
  "Facility vs. tank-level wall type classification shows heterogeneity within facilities.",
  sprintf("Texas: %.1f%% facilities all-SW, %.1f%% all-DW. Tank-level: %.1f%% SW, %.1f%% DW, %.1f%% Unknown/Other. High rate of unknown wall types in Control group (%.1f%%) vs Texas (%.1f%%) suggests differential data quality.",
          wall_composition[treatment_group == "Texas", pct_fac_all_sw],
          wall_composition[treatment_group == "Texas", pct_fac_all_dw],
          wall_composition[treatment_group == "Texas", pct_tanks_sw],
          wall_composition[treatment_group == "Texas", pct_tanks_dw],
          wall_composition[treatment_group == "Texas", pct_tanks_other],
          wall_composition[treatment_group == "Control", pct_tanks_other],
          wall_composition[treatment_group == "Texas", pct_tanks_other]),
  "Differential measurement error (missing wall types) is a potential threat to validity if correlated with unobserved risk.",
  "Baseline imbalance in data quality is explicit."
)

cat("Table 2B saved.\n")


# ==============================================================================
# DATA SECTION FIGURES: D1-D4 + Table D1
# 
# PURPOSE: Teaching figures for the data section
# GOAL: Show coverage, variation, and overlap - NOT relationships
#
# DEPENDENCIES: Runs AFTER Section 01B (premium/claims already merged into
#               facility_leak_behavior). Uses existing objects:
#               - facility_leak_behavior (with annual_premium, cleanup_cost columns)
#               - control_states (defined in Section 01)
#               - claims_states defined here (subset with cleanup cost data)
#
# Insert AFTER Section 02 (Sample Construction)
# ==============================================================================

cat(rep("=", 80), "\n", sep = "")
cat("DATA SECTION: DESCRIPTIVE FIGURES (D1-D4) AND SUMMARY TABLE\n")
cat(rep("=", 80), "\n\n")

# Additional packages for maps
suppressPackageStartupMessages({
  library(maps)
})

# Define which control states have claims data
claims_states <- c("Louisiana", "Tennessee", "New Mexico", "Utah", "Colorado")

# ------------------------------------------------------------------------------
# EXTRACT PREMIUM AND CLAIMS SUMMARIES FROM EXISTING DATA
# (Premium/claims already merged in Section 01B - no reloading)
# ------------------------------------------------------------------------------

cat("Extracting premium and claims summaries from merged panel...\n")

# Premium summary (Texas only, where annual_premium exists)
premium_summary_data <- facility_leak_behavior[
  state == "Texas" & !is.na(annual_premium) & annual_premium > 0,
  .(
    panel_id,
    panel_year,
    per_tank_premium = annual_premium / pmax(active_tanks, 1),
    annual_premium,
    avg_tank_age,
    active_tanks,
    premium_n_tanks
  )
]

# Create age bins for premium data
premium_summary_data[, age_bin := cut(
  avg_tank_age,
  breaks = c(0, 5, 10, 15, 20, 25, 30, 35, Inf),
  labels = c("0-5", "5-10", "10-15", "15-20", "20-25", "25-30", "30-35", "35+"),
  include.lowest = TRUE
)]

premium_year_range <- range(premium_summary_data$panel_year, na.rm = TRUE)
cat(sprintf("  Premium data: %s facility-months (%d-%d)\n", 
            format(nrow(premium_summary_data), big.mark = ","),
            premium_year_range[1], premium_year_range[2]))

# Claims summary (control states, where cleanup_cost exists)
claims_summary_data <- facility_leak_behavior[
  state %in% claims_states & !is.na(cleanup_cost) & cleanup_cost > 0,
  .(
    panel_id,
    state,
    panel_year,
    cleanup_cost
  )
]

cat(sprintf("  Claims data: %s incidents\n", 
            format(nrow(claims_summary_data), big.mark = ",")))

# ------------------------------------------------------------------------------
# DATA COVERAGE STRUCTURE
# ------------------------------------------------------------------------------

cat("Building coverage structure...\n")

coverage_list <- list(
  data.table(state = "Texas", data_type = "Premium", 
             start_year = premium_year_range[1], end_year = premium_year_range[2]),
  data.table(state = "Louisiana", data_type = "Claims", start_year = 1995, end_year = 2023),
  data.table(state = "Tennessee", data_type = "Claims", start_year = 1998, end_year = 2023),
  data.table(state = "New Mexico", data_type = "Claims", start_year = 1996, end_year = 2023),
  data.table(state = "Utah", data_type = "Claims", start_year = 2000, end_year = 2023),
  data.table(state = "Colorado", data_type = "Claims", start_year = 1999, end_year = 2023),
  data.table(state = "Texas", data_type = "LUST", start_year = 1990, end_year = 2023),
  rbindlist(lapply(control_states, function(s) {
    data.table(state = s, data_type = "LUST", start_year = 1990, end_year = 2023)
  })),
  data.table(state = "Texas", data_type = "Admin", start_year = 1985, end_year = 2023),
  rbindlist(lapply(control_states, function(s) {
    data.table(state = s, data_type = "Admin", start_year = 1990, end_year = 2023)
  }))
)

coverage_dt <- rbindlist(coverage_list, fill = TRUE)
coverage_dt[, data_type := factor(data_type, levels = c("Premium", "Claims", "LUST", "Admin"))]



# ==============================================================================
# FIGURE D1: ANALYSIS SAMPLE COVERAGE (GAP ANALYSIS)
# ==============================================================================

cat("Creating Figure D1: Analysis Sample Coverage (Gap Analysis)...\n")

# ------------------------------------------------------------------------------
# 1. CONSTRUCT METADATA
# ------------------------------------------------------------------------------

# A. Base Layer: Admin / Facility Data (ALL Analysis States)
# This represents the "backbone" of the analysis (leak rates, entry/exit).
base_ranges <- facility_leak_behavior[
  is_analysis_sample == TRUE,
  .(
    start_year = min(panel_year),
    end_year = max(panel_year),
    data_type = "Admin / Fixed Fee (Facility Data)"
  ),
  by = state
]

# B. Overlay Layer 1: Market Premiums (Texas Only)
# Note: We ensure this only plots if data actually exists
prem_ranges <- data.table()
if (exists("premium_summary_data") && nrow(premium_summary_data) > 0) {
  if (!"state" %in% names(premium_summary_data)) premium_summary_data[, state := "Texas"]
  
  prem_ranges <- premium_summary_data[, .(
    start_year = min(panel_year),
    end_year = max(panel_year),
    data_type = "Market Premium (Cost Data)"
  ), by = state]
}

# C. Overlay Layer 2: Claims Data (Selected Controls)
claim_ranges <- data.table()
if (exists("claims_summary_data") && nrow(claims_summary_data) > 0) {
  if (!"state" %in% names(claims_summary_data)) {
    claims_summary_data <- merge(claims_summary_data, 
                                 unique(facility_leak_behavior[, .(panel_id, state)]),
                                 by = "panel_id", all.x = TRUE)
  }
  
  claim_ranges <- claims_summary_data[, .(
    start_year = min(panel_year),
    end_year = max(panel_year),
    data_type = "Claims (Cost Data)"
  ), by = state]
}

# Combine for Plotting
# We keep them separate to control layering in ggplot
all_ranges <- rbind(base_ranges, prem_ranges, claim_ranges, use.names = TRUE)

# ------------------------------------------------------------------------------
# 2. PANEL A: GEOGRAPHIC MAP (Analysis Roles)
# ------------------------------------------------------------------------------

# Define Roles based on "Best Available Data"
state_roles <- unique(base_ranges[, .(state)])

# Logic: If in Premium -> "Market Premium", If in Claims -> "Fixed Fee + Claims", Else -> "Fixed Fee (Admin Only)"
state_roles[, role := fcase(
  state %in% prem_ranges$state, "Market Premium (TX)",
  state %in% claim_ranges$state, "Fixed Fee + Claims Data",
  state %in% base_ranges$state, "Fixed Fee (Admin Data Only)",
  default = "Not Used"
)]

us_states_df <- map_data("state")
us_states_df$region <- str_to_title(us_states_df$region)
us_states_merged <- merge(us_states_df, state_roles, by.x = "region", by.y = "state", all.x = TRUE)
us_states_merged$role[is.na(us_states_merged$role)] <- "Not Used"
us_states_merged <- us_states_merged[order(us_states_merged$order), ]

role_colors <- c(
  "Market Premium (TX)" = "#D55E00",          # Orange
  "Fixed Fee + Claims Data" = "#0072B2",      # Blue
  "Fixed Fee (Admin Data Only)" = "gray70",   # Dark Gray
  "Not Used" = "gray95"
)

p_d1a_map <- ggplot(us_states_merged, aes(x = long, y = lat, group = group, fill = role)) +
  geom_polygon(color = "white", linewidth = 0.3) +
  coord_fixed(1.3) +
  scale_fill_manual(values = role_colors, name = "Data Availability") +
  labs(title = "A. Geographic Analysis Sample",
       subtitle = "Classification by highest level of cost data availability") +
  theme_void() +
  theme(plot.title = element_text(size = 12, face = "bold", hjust = 0),
        plot.subtitle = element_text(size = 10, hjust = 0),
        legend.position = "bottom",
        legend.title = element_text(face = "bold"),
        plot.margin = margin(10, 10, 10, 10))

save_figure(p_d1a_map, "FD01a_analysis_geo_coverage", width = 10, height = 6)

# ------------------------------------------------------------------------------
# 3. PANEL B: TEMPORAL COVERAGE TIMELINE (The Gaps)
# ------------------------------------------------------------------------------

# Filter & Order
plot_dt <- all_ranges[state %in% unique(base_ranges$state)]

# Ordering: Texas -> Claims States -> Admin Only
special_states <- c("Texas", unique(claim_ranges$state))
other_states <- setdiff(unique(base_ranges$state), special_states)
plot_order <- c(special_states, sort(other_states))
plot_dt[, state := factor(state, levels = rev(plot_order))]

# Plotting with Layers
# Layer 1: Gray Admin Bar (Background)
# Layer 2: Colored Cost Bars (Foreground, slightly thinner)

p_d1b_timeline <- ggplot() +
  # 1. Background: Admin Data (Shows the full potential panel)
  geom_segment(data = plot_dt[data_type == "Admin / Fixed Fee (Facility Data)"],
               aes(x = start_year, xend = end_year, y = state, yend = state, color = data_type),
               linewidth = 4, alpha = 0.4) +
  
  # 2. Foreground: Cost Data (Shows the gaps)
  geom_segment(data = plot_dt[data_type != "Admin / Fixed Fee (Facility Data)"],
               aes(x = start_year, xend = end_year, y = state, yend = state, color = data_type),
               linewidth = 2.5, alpha = 1) +
  
  scale_color_manual(
    values = c(
      "Admin / Fixed Fee (Facility Data)" = "gray50",
      "Market Premium (Cost Data)" = "#D55E00",
      "Claims (Cost Data)" = "#0072B2"
    ),
    name = "Data Type"
  ) +
  scale_x_continuous(breaks = seq(1970, 2025, by = 5)) +
  labs(title = "B. Temporal Analysis Sample & Data Gaps",
       subtitle = "Colored bars indicate periods where Cost Data overlays the Administrative Data backbone",
       x = "Year", y = NULL) +
  theme_academic() +
  theme(plot.title = element_text(size = 12, face = "bold"),
        axis.text.y = element_text(size = 8),
        legend.position = "bottom",
        panel.grid.major.x = element_line(color = "gray90"),
        panel.grid.major.y = element_blank())

save_figure(p_d1b_timeline, "FD01b_analysis_time_coverage", width = 10, height = 10)

# ------------------------------------------------------------------------------
# 4. COMBINE AND SAVE
# ------------------------------------------------------------------------------

p_d1_analysis <- p_d1a_map / p_d1b_timeline +
  plot_layout(heights = c(1, 1.5)) +
  plot_annotation(
    title = "Figure D1: Analysis Sample Coverage",
    subtitle = "Administrative facility data exists for all analysis states; Cost data is limited to specific subsets.",
    caption = "Notes: Gray bars in Panel B represent the 'Admin Data' backbone (facility entry/exit/leaks) available for all analysis states.\nColored overlays indicate periods where cost data is available: Orange for Texas Market Premiums (starting 2006),\nand Blue for cleanup costs in select control states. The gaps illustrate the data matching challenge.",
    theme = theme(plot.title = element_text(size = 14, face = "bold"),
                  plot.subtitle = element_text(size = 10),
                  plot.caption = element_text(hjust = 0, size = 9))
  )

save_figure(p_d1_analysis, "FD01_analysis_coverage_combined", width = 12, height = 14)
cat("  Figure D1 (Gap Analysis) saved.\n\n")


# ==============================================================================
# FIGURE D2: PREMIUM ENGINE REVERSE-ENGINEERING (TANK LEVEL)
# ==============================================================================

cat("Creating Figure D2 (Engine Characterization)...\n")

# ------------------------------------------------------------------
# 1. LOAD TANK-LEVEL OUTPUTS
# ------------------------------------------------------------------
rate_filing_dir <- here("rate_filings_backup", "Rate FIllings", "Mid-Continent Casualty Company ­– 23418")

csv_tank_files <- list.files(
  path = rate_filing_dir,
  pattern = "texas_midcontinent_tank_month_premium_.*\\.csv$",
  full.names = TRUE
)

if (length(csv_tank_files) > 0) {
  cat(sprintf("  Loading %d tank-premium files...\n", length(csv_tank_files)))
  all_tank_data <- rbindlist(lapply(csv_tank_files, fread), fill = TRUE)
} else {
  cat("  Looking in:", rate_filing_dir, "\n")
  stop("CRITICAL: Tank-level premium files not found. Check the path above.")
}

# ------------------------------------------------------------------
# 2. DATA PREP (MATCHING ENGINE LOGIC)
# ------------------------------------------------------------------

# A. Reconstruct Age (Engine uses INSTALL_DATE)
all_tank_data[, age_years := YEAR - year(as.IDate(INSTALL_DATE))]
all_tank_data[age_years < 0, age_years := 0]

# B. Reconstruct Technology Categories
all_tank_data[, tech_type := "Single-Walled/Steel (High Risk)"]
all_tank_data[is_reinforced_fiberglass == TRUE, tech_type := "Fiberglass (Medium Risk)"]
all_tank_data[is_double_walled_steel == TRUE | pip_double_walled == 1, 
              tech_type := "Double-Walled (Lowest Risk)"]

# C. Calculate Facility Scale
all_tank_data[, n_tanks_facility := .N, by = .(FACILITY_ID, YEAR, MONTH)]

# Filter valid data AND Time Range (Start 2006)
plot_dt <- all_tank_data[
  tank_premium > 0 & 
  tank_premium < 10000 & 
  YEAR >= 2006
]

# ------------------------------------------------------------------
# 3. PANEL A: THE AGE LOADING SCHEDULE (Step Function)
# ------------------------------------------------------------------
age_stats <- plot_dt[age_years <= 40, .(
  mean_prem = mean(tank_premium, na.rm = TRUE),
  N = .N
), by = age_years][order(age_years)]

coeff <- max(age_stats$mean_prem) / max(age_stats$N) * 0.8

p_d2a <- ggplot(age_stats, aes(x = age_years)) +
  geom_bar(aes(y = N * coeff), stat = "identity", fill = "gray90", width = 0.8) +
  geom_step(aes(y = mean_prem, color = "Realized Premium Schedule"), 
            linewidth = 1.2, direction = "mid") +
  geom_point(aes(y = mean_prem, color = "Realized Premium Schedule"), size = 2) +
  scale_y_continuous(
    name = "Mean Premium per Tank ($)",
    labels = scales::dollar,
    sec.axis = sec_axis(~ . / coeff, name = "Tank Count (Distribution)", labels = scales::comma)
  ) +
  scale_color_manual(values = c("Realized Premium Schedule" = "#D55E00")) +
  labs(x = "Tank Age (Years)") + # Removed Title/Subtitle
  theme_academic() +
  theme(legend.position = "none")

save_figure(p_d2a, "FD02a_engine_age_steps", width = 8, height = 6)

# ------------------------------------------------------------------
# 4. PANEL B: CONSTRUCTION LOADS (Additive Factors)
# ------------------------------------------------------------------
p_d2b <- ggplot(plot_dt, aes(x = tech_type, y = tank_premium)) +
  geom_violin(fill = "#D55E00", alpha = 0.1, color = NA) +
  geom_boxplot(width = 0.2, fill = "white", outlier.shape = NA) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 4, color = "#D55E00") +
  coord_cartesian(ylim = quantile(plot_dt$tank_premium, c(0.01, 0.99))) +
  scale_y_continuous(labels = scales::dollar) +
  labs(x = NULL, y = "Per-Tank Premium ($)") + # Removed Title/Subtitle
  theme_academic() +
  theme(axis.text.x = element_text(size = 9))

save_figure(p_d2b, "FD02b_engine_risk_loads", width = 8, height = 6)

# ------------------------------------------------------------------
# 5. PANEL C: SCALE NEUTRALITY CHECK
# ------------------------------------------------------------------
p_d2c <- ggplot(plot_dt, aes(x = factor(pmin(n_tanks_facility, 6)), y = tank_premium)) +
  geom_boxplot(fill = "#0072B2", alpha = 0.2, outlier.shape = NA) +
  stat_summary(fun = mean, geom = "line", aes(group = 1), color = "#0072B2", linewidth = 1) +
  stat_summary(fun = mean, geom = "point", size = 3, color = "#0072B2") +
  scale_y_continuous(labels = scales::dollar, limits = quantile(plot_dt$tank_premium, c(0.01, 0.99))) +
  scale_x_discrete(labels = c("1", "2", "3", "4", "5", "6+")) +
  labs(x = "Number of Tanks at Facility", y = "Per-Tank Premium ($)") + # Removed Title/Subtitle
  theme_academic()

save_figure(p_d2c, "FD02c_engine_scale_check", width = 8, height = 6)

# ------------------------------------------------------------------
# 6. PANEL D: RATE REVISIONS & AGE TREND (UPDATED)
# ------------------------------------------------------------------
comp_time <- plot_dt[, .(
  mean_prem = mean(tank_premium, na.rm = TRUE),
  mean_age = mean(age_years, na.rm = TRUE)
), by = YEAR][order(YEAR)]

filing_dates <- c(2006, 2014, 2019, 2021)
scale_fac <- 20

p_d2d <- ggplot(comp_time, aes(x = YEAR)) +
  # Filing Markers
  geom_vline(xintercept = filing_dates, linetype = "dotted", color = "gray50") +
  # Trends
  geom_line(aes(y = mean_age, color = "Avg Tank Age (Left)"), linewidth = 1.2) +
  geom_line(aes(y = mean_prem / scale_fac, color = "Avg Premium (Right)"), 
            linewidth = 1.2, linetype = "dashed") +
  scale_y_continuous(
    name = "Average Tank Age (Years)",
    sec.axis = sec_axis(~ . * scale_fac, name = "Mean Tank Premium ($)", labels = scales::dollar)
  ) +
  scale_color_manual(name = "", values = c("Avg Tank Age (Left)" = "#D55E00", 
                                           "Avg Premium (Right)" = "#0072B2")) +
  labs(x = "Year") + # Removed Title/Subtitle
  theme_academic() +
  theme(legend.position = "bottom")

save_figure(p_d2d, "FD02d_engine_filings", width = 8, height = 6)

# ------------------------------------------------------------------
# 7. COMBINE
# ------------------------------------------------------------------
p_final <- (p_d2a | p_d2b) / (p_d2c | p_d2d) +
  plot_layout(heights = c(1, 1)) 
  # Removed plot_annotation() completely

save_figure(p_final, "FD02_premium_engine_composite", width = 12, height = 10)
cat("Figure D2 (Engine Characterization) saved.\n")


# ==============================================================================
# FIGURE E: EVOLUTION OF AGE PRICING SCHEDULES (BY FILING)
# ==============================================================================

cat("Creating Figure E (Age Schedules by Filing)...\n")

if (!exists("all_tank_data")) stop("Please run the Figure D2 code block first.")

# 1. Define Filing Eras (using filtered data to respect 2006 start)
curve_dt <- all_tank_data[YEAR >= 2006]

curve_dt[, filing_era := fcase(
  YEAR < 2014 | (YEAR == 2014 & MONTH < 5),       "Filing 2006-2014",
  (YEAR == 2014 & MONTH >= 5) | (YEAR > 2014 & YEAR < 2019) | (YEAR == 2019 & MONTH == 1), "Filing 2014-2019",
  (YEAR == 2019 & MONTH >= 2) | YEAR == 2020,     "Filing 2019-2020",
  YEAR >= 2021,                                   "Filing 2021+"
)]

# 2. Aggregate
curve_stats <- curve_dt[age_years <= 40, .(
  mean_prem = mean(tank_premium, na.rm = TRUE),
  N = .N
), by = .(age_years, filing_era)][order(filing_era, age_years)]

# 3. Plot
p_E <- ggplot(curve_stats, aes(x = age_years, y = mean_prem, color = filing_era)) +
  geom_step(linewidth = 1.2, direction = "mid") +
  geom_point(size = 1.5, alpha = 0.6) +
  scale_color_viridis_d(option = "D", end = 0.85, name = "Rate Filing Period") +
  scale_y_continuous(labels = scales::dollar) +
  scale_x_continuous(breaks = seq(0, 40, 5)) +
  labs(x = "Tank Age (Years)", y = "Mean Premium per Tank ($)") + # Removed Title/Subtitle
  theme_academic() +
  theme(legend.position = "bottom",
        legend.title = element_text(face = "bold"),
        panel.grid.major.y = element_line(color = "gray90"))

# Save
save_figure(p_E, "FE_age_curve_by_filing", width = 10, height = 7)
cat("Figure E saved.\n")



# ==============================================================================
# FIGURE D3: CLAIMS DATA (Control States)
# ==============================================================================

cat("Creating Figure D3: Claims data description...\n")

# Calculate stats for annotation
claims_median <- median(claims_summary_data$cleanup_cost, na.rm = TRUE)
claims_mean <- mean(claims_summary_data$cleanup_cost, na.rm = TRUE)

# ------------------------------------------------------------------
# PANEL A: COST DISTRIBUTION (HISTOGRAM)
# ------------------------------------------------------------------
p_d3a_hist <- ggplot(claims_summary_data, aes(x = cleanup_cost)) +
  geom_histogram(bins = 40, fill = "#0072B2", color = "white", alpha = 0.8) +
  geom_vline(xintercept = claims_median, linetype = "dashed", color = "black", linewidth = 1) +
  geom_vline(xintercept = claims_mean, linetype = "dotted", color = "darkred", linewidth = 1) +
  
  # Annotation (Internal data-ink)
  annotate("text", x = claims_mean * 1.5, y = Inf, vjust = 2,
           label = sprintf("Median: $%s\nMean: $%s", 
                           format(round(claims_median), big.mark = ","),
                           format(round(claims_mean), big.mark = ",")),
           hjust = 0, size = 3, fontface = "italic") +
  
  scale_x_log10(labels = scales::dollar, breaks = c(10000, 50000, 100000, 500000, 1000000, 5000000)) +
  labs(x = "Total Cleanup Cost (2023 $, log scale)", y = "Count") + # Removed Titles
  theme_academic()

save_figure(p_d3a_hist, "FD03a_claims_dist", width = 10, height = 5)

# ------------------------------------------------------------------
# PANEL B: COST BY STATE (BOXPLOT)
# ------------------------------------------------------------------
p_d3b_state <- ggplot(claims_summary_data, 
                      aes(x = reorder(state, cleanup_cost, FUN = median), y = cleanup_cost)) +
  geom_boxplot(fill = "#0072B2", alpha = 0.7, outlier.size = 0.5) +
  scale_y_log10(labels = scales::dollar, breaks = c(10000, 100000, 1000000, 5000000)) +
  coord_flip() +
  labs(x = NULL, y = "Cleanup Cost (2023 $, log scale)") + # Removed Titles
  theme_academic()

save_figure(p_d3b_state, "FD03b_claims_by_state", width = 10, height = 6)

# ------------------------------------------------------------------
# COMBINE & SAVE (STACKED ROW-WISE)
# ------------------------------------------------------------------
# Stack vertically (/) so they share the horizontal X-axis dimension
p_d3_claims <- p_d3a_hist / p_d3b_state +
  plot_layout(heights = c(1, 1.2)) # Give slightly more space to the states

save_figure(p_d3_claims, "FD03_claims_data", width = 10, height = 10)
cat("  Figure D3 (Claims Data) saved.\n\n")

# ==============================================================================
# FIGURE D4: ANNUAL LEAK PROBABILITY (Final Polish)
# ==============================================================================

cat("Creating Figure D4: Annual leak probability...\n")

# ------------------------------------------------------------------
# 1. DATA PREP: AGGREGATE TO FACILITY-YEAR
# ------------------------------------------------------------------
leak_monthly <- facility_leak_behavior[
  is_analysis_sample == TRUE & active_tanks > 0
]

# Enforce Chronological Age Bin Ordering
age_levels <- c("0-5", "5-10", "10-15", "15-20", "20-25", "25-30", "30-35", "35-40", "40+")

leak_monthly[, age_bin_5yr := cut(
  avg_tank_age,
  breaks = c(0, 5, 10, 15, 20, 25, 30, 35, 40, Inf),
  labels = age_levels,
  include.lowest = TRUE, right = FALSE
)]

# Define Leak Type Variable
leak_var <- "leak_found_by_closure"
if (!leak_var %in% names(leak_monthly)) {
  leak_monthly[, leak_found_by_closure := as.integer(leak_incident == 1 & is_closed_removed == 1)]
}

# Aggregate to Annual Level
leak_annual <- leak_monthly[, .(
  n_months = .N,
  n_leaks_closure = sum(leak_incident == 1 & leak_found_by_closure == 1, na.rm = TRUE),
  n_leaks_oper    = sum(leak_incident == 1 & leak_found_by_closure == 0, na.rm = TRUE),
  avg_age_bin     = names(sort(table(age_bin_5yr), decreasing = TRUE))[1] 
), by = .(panel_id, panel_year, treatment_group)]

# Fix Age Factor Levels
leak_annual[, avg_age_bin := factor(avg_age_bin, levels = age_levels)]

# ------------------------------------------------------------------
# 2. DEFINE COLORS
# ------------------------------------------------------------------
type_colors <- c(
  "Found at Closure"      = "#882255",  # Deep Red/Wine
  "Operational Discovery" = "#117733"   # Forest Green
)

state_colors <- c(
  "Texas"   = "#D55E00", 
  "Control" = "#0072B2"
)

# ------------------------------------------------------------------
# 3. PANEL A: ANNUAL LEAK PROBABILITY (Stacked Bar)
# ------------------------------------------------------------------
leak_summary <- leak_annual[, .(
  exposure_years = sum(n_months) / 12,
  incidents_closure = sum(n_leaks_closure),
  incidents_oper    = sum(n_leaks_oper)
), by = .(treatment_group)]

leak_summary_long <- melt(leak_summary, 
  id.vars = c("treatment_group", "exposure_years"),
  measure.vars = c("incidents_closure", "incidents_oper"),
  value.name = "n_incidents"
)

leak_summary_long[, annual_prob := (n_incidents / exposure_years) * 100]
leak_summary_long[, type_label := fifelse(variable == "incidents_closure", 
                                          "Found at Closure", "Operational Discovery")]

p_d4a_bar <- ggplot(leak_summary_long, aes(x = treatment_group, y = annual_prob, fill = type_label)) +
  geom_col(position = "stack", width = 0.6, alpha = 0.9) +
  scale_fill_manual(values = type_colors, name = "Detection Method") +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(x = NULL, y = "Annual Leak Probability (%)") + 
  theme_academic() +
  theme(legend.position = "bottom")

save_figure(p_d4a_bar, "FD04a_leak_annual_prob", width = 8, height = 6)

# ------------------------------------------------------------------
# 4. PANEL B: AGE GRADIENT (5-Year Bins, Fixed Axis)
# ------------------------------------------------------------------
age_summary <- leak_annual[!is.na(avg_age_bin), .(
  exposure_years = sum(n_months) / 12,
  incidents_closure = sum(n_leaks_closure),
  incidents_oper    = sum(n_leaks_oper)
), by = .(avg_age_bin, treatment_group)]

age_long <- melt(age_summary, 
                 id.vars = c("avg_age_bin", "treatment_group", "exposure_years"),
                 measure.vars = c("incidents_closure", "incidents_oper"),
                 value.name = "n_incidents")

age_long[, annual_prob := (n_incidents / exposure_years) * 100]
age_long[, type_label := fifelse(variable == "incidents_closure", 
                                 "Found at Closure", "Operational Discovery")]

p_d4b_age <- ggplot(age_long[exposure_years > 100], 
                    aes(x = avg_age_bin, y = annual_prob, 
                        color = treatment_group, group = treatment_group)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  scale_color_manual(values = state_colors, name = "State Group") + 
  # SCALES FIXED: Forces direct comparison of magnitude between leak types
  facet_wrap(~type_label, ncol = 2, scales = "fixed") + 
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(x = "Tank Age (5-Year Bins)", y = "Annual Leak Probability (%)") +
  theme_academic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")

save_figure(p_d4b_age, "FD04b_leak_age_gradient_5yr", width = 10, height = 6)

# ------------------------------------------------------------------
# 5. COMBINE & SAVE
# ------------------------------------------------------------------
p_d4_combined <- p_d4a_bar / p_d4b_age +
  plot_layout(heights = c(1, 1.2))

save_figure(p_d4_combined, "FD04_leak_data_composite", width = 12, height = 10)
cat("Figure D4 (Final Polish) saved.\n")


# ==============================================================================
# TABLE D1: SUMMARY STATISTICS (Consolidated & Updated)
# ==============================================================================

cat("Creating Table D1: Summary statistics by dataset...\n")

# A. Premiums (Facility Annual)
prem_stats <- df_prem[, .(
  n_fac = uniqueN(panel_id),
  n_obs = .N,
  mean_age = mean(avg_tank_age, na.rm=TRUE),
  mean_tanks = mean(active_tanks, na.rm=TRUE),
  mean_cost = mean(annual_premium, na.rm=TRUE) # Facility Annual
)]

# B. Claims (Control States Backbone)
backbone_claims <- facility_leak_behavior[state %in% claims_states & is_analysis_sample == TRUE]
claim_stats <- list(
  n_fac = uniqueN(backbone_claims$panel_id),
  n_obs = nrow(backbone_claims),
  mean_age = mean(backbone_claims$avg_tank_age, na.rm=TRUE),
  mean_tanks = mean(backbone_claims$active_tanks, na.rm=TRUE),
  mean_cost = mean(claims_summary_data$cleanup_cost, na.rm=TRUE) # Per Incident
)

# C. Leaks (Annual Prob)
exposure_yrs <- nrow(leak_monthly) / 12
total_leaks <- sum(leak_monthly$leak_incident, na.rm=TRUE)
leak_stats_all <- list(
  n_fac = uniqueN(leak_monthly$panel_id),
  n_obs = nrow(leak_monthly),
  mean_age = mean(leak_monthly$avg_tank_age, na.rm=TRUE),
  mean_tanks = mean(leak_monthly$active_tanks, na.rm=TRUE),
  leak_metric = (total_leaks / exposure_yrs) * 100
)

# Table Construction
summary_table <- data.table(
  Variable = c("Facilities", "Facility-Months", "Mean Tank Age (Yrs)", "Mean Tanks per Facility", 
               "Annual Leak Prob. (%)", "Mean Annual Premium ($)", "Mean Cleanup Cost ($)"),
  
  `Premiums (TX)` = c(format(prem_stats$n_fac, big.mark = ","), format(prem_stats$n_obs, big.mark = ","),
                      sprintf("%.1f", prem_stats$mean_age), sprintf("%.1f", prem_stats$mean_tanks), "—",
                      sprintf("$%s", format(round(prem_stats$mean_cost), big.mark = ",")), "—"),
  
  `Claims (Control)` = c(format(claim_stats$n_fac, big.mark = ","), format(claim_stats$n_obs, big.mark = ","),
                         sprintf("%.1f", claim_stats$mean_age), sprintf("%.1f", claim_stats$mean_tanks), "—", "—",
                         sprintf("$%s", format(round(claim_stats$mean_cost), big.mark = ","))),
  
  `Analysis Sample (All)` = c(format(leak_stats_all$n_fac, big.mark = ","), format(leak_stats_all$n_obs, big.mark = ","),
                              sprintf("%.1f", leak_stats_all$mean_age), sprintf("%.1f", leak_stats_all$mean_tanks),
                              sprintf("%.2f%%", leak_stats_all$leak_metric), "—", "—")
)

t_d1_academic <- summary_table %>% gt() %>%
  tab_header(title = "Table D1: Summary Statistics by Data Source", subtitle = "Distinct samples used for Premium, Claims, and Risk estimation") %>%
  cols_label(Variable = "") %>% cols_align(align = "center", columns = -Variable) %>% cols_align(align = "left", columns = Variable) %>%
  tab_spanner(label = "Data Source / Sample Definition", columns = c("Premiums (TX)", "Claims (Control)", "Analysis Sample (All)")) %>%
  tab_source_note(source_note = md("**Notes:** '—' indicates metric not applicable. **Premiums:** Facility-level annual premiums. **Claims:** Realized cleanup costs per incident. **Analysis Sample:** Full panel used for leak probability estimation."))

save_table(t_d1_academic, summary_table, "TD01_summary_by_dataset")
cat("  Table D1 saved.\n\n")


# ==============================================================================
# EXPORT CAPTIONS
# ==============================================================================

export_caption("FD01", "Data Coverage", 
  "Geographic and temporal coverage of premium, claims, LUST, and administrative data.",
  "Panel A: US map showing Texas (premium data), control states with claims, and LUST coverage. Panel B: Timeline showing years of data availability by state and type.",
  "Data siloing is exogenous to facility behavior.",
  "Non-overlapping coverage motivates identification strategy."
)

export_caption("FD02", "Premium Data Description",
  "Distribution, age gradient, and time series of per-tank premiums in Texas.",
  "Panel A: log-scaled premium distribution. Panel B: premium-age gradient. Panel C: mean premium time series with 95% CI.",
  "Premium heterogeneity identifies pricing function parameters.",
  "Time series shows premium evolution over sample period."
)

export_caption("FD03", "Claims Data Description",
  "Distribution and state variation of cleanup costs in control states.",
  "Panel A: highly right-skewed distribution. Panel B: state-level boxplots.",
  "Cleanup cost heterogeneity identifies damage parameters.",
  "Log specification appropriate given extreme right skew."
)

export_caption("FD04", "Leak Data as Bridge Variable",
  "Leak rates observed nationwide provide common ground for cross-dataset inference.",
  "Panel A: facility-level leak rate distribution. Panel B: parallel age gradients across treatment groups.",
  "Leak rates bridge siloed datasets.",
  "Parallel age gradients support external validity."
)

# ==============================================================================
# SECTION COMPLETE
# ==============================================================================

track_section_time("Data Section Figures (D1-D4)")

cat(rep("=", 80), "\n", sep = "")
cat("DATA SECTION FIGURES COMPLETE\n")
cat("Outputs: FD01-FD04.png/pdf, TD01.html/tex/csv\n")
cat(rep("=", 80), "\n\n")

# Cleanup intermediate objects only - keep summary data for Results section
rm(coverage_dt, coverage_list, us_states_df, us_states_merged, state_coverage, 
   coverage_colors, premium_median, premium_ts, claims_median, claims_mean,
   leak_by_age, facility_leak_rates, leak_stats, n_facilities_leak, n_obs_leak,
   state_order)
invisible(gc())



# ==============================================================================
# REFACTORED RESULTS SECTION (R1 - R3) - CORRECTED
# ==============================================================================
# FIXES:
# 1. Type Mismatch: Forces facility_id to integer for merging.
# 2. Variable Consistency: Ensures age_bin exists before aggregation.
# 3. Object Safety: Hard stops if merges fail (preventing cascading errors).
# ==============================================================================

cat(rep("=",80), "\n", "RESULTS: PREMIUM / CLAIMS / GRADIENTS (R1-R3)", "\n", rep("=",80), "\n\n")

# ------------------------------------------------------------------------------
# 1. SETUP & COMMON SETTINGS
# ------------------------------------------------------------------------------
library(data.table)
library(fixest)
library(ggplot2)
library(gt)
library(scales)

# Explicit Factor Levels for Age
age_levels <- c("0-5","5-10","10-15","15-20","20-25","25-30","30-35","35-40","40+")

# ------------------------------------------------------------------------------
# 2. PREPARE FACILITY RISK METRICS (The Bridge)
# ------------------------------------------------------------------------------
cat("Step 1: Preparing facility-level risk metrics...\n")

# Ensure facility_leak_behavior exists
if (!exists("facility_leak_behavior")) stop("CRITICAL: 'facility_leak_behavior' missing.")

# Aggregate Risk from Main Panel
# FILTER: 2006+ only (to match premium data availability)
risk_panel <- facility_leak_behavior[
  is_analysis_sample == TRUE & active_tanks > 0 & panel_year >= 2006,
  .(
    n_months = .N,
    n_incidents = sum(leak_incident, na.rm = TRUE)
  ),
  by = .(panel_id, facility_id, state, panel_year)
]

# Calculate Annual Prob
risk_panel[, annual_leak_prob := n_incidents / (n_months / 12)]

# KEY FIX: Ensure facility_id is Integer for merging with Engine Data
# Some datasets import it as character ("00123") vs integer (123)
risk_panel[, facility_id_join := as.integer(as.character(facility_id))]

# Split for later use
risk_tx <- risk_panel[state == "Texas"]
risk_control <- risk_panel[state != "Texas"]

# ------------------------------------------------------------------------------
# 3. PREPARE PREMIUM DATA (Texas Tank-Year Level)
# ------------------------------------------------------------------------------
cat("Step 2: Aggregating Tank-Month premiums to Tank-Year...\n")

if (!exists("all_tank_data")) stop("CRITICAL: 'all_tank_data' (Rate Engine) missing.")

# Aggregate Tank-Month -> Tank-Year
# KEY FIX: Ensure FACILITY_ID is Integer
all_tank_data[, FACILITY_ID := as.integer(FACILITY_ID)]

texas_tank_years <- all_tank_data[
  tank_premium > 0 & YEAR >= 2006,
  .(
    mean_tank_premium = mean(tank_premium, na.rm = TRUE), 
    age_years = max(age_years, na.rm=TRUE),               
    tech_type = first(tech_type)
  ),
  by = .(FACILITY_ID, YEAR, UST_ID) 
]

# MERGE RISK INTO PREMIUMS
# This failed previously due to type mismatch. Now forced to integer.
cat("  Merging Premium Data with Risk Data...\n")
reg_data_premium <- merge(
  texas_tank_years,
  risk_tx[, .(facility_id_join, panel_year, annual_leak_prob)],
  by.x = c("FACILITY_ID", "YEAR"),
  by.y = c("facility_id_join", "panel_year"),
  all.x = FALSE # Inner Join: We need Risk + Premium
)

# VALIDATION: Stop if merge failed
if (nrow(reg_data_premium) == 0) {
  print(head(texas_tank_years$FACILITY_ID))
  print(head(risk_tx$facility_id_join))
  stop("CRITICAL: Premium-Risk merge resulted in 0 rows. Check ID formats.")
}

# Create Age Bins
reg_data_premium[, age_bin := cut(
  age_years, 
  breaks = c(0, 5, 10, 15, 20, 25, 30, 35, 40, Inf),
  labels = age_levels,
  include.lowest = TRUE, right = FALSE
)]

cat(sprintf("  Premium Analysis Sample: %s tank-years.\n", format(nrow(reg_data_premium), big.mark=",")))

# ------------------------------------------------------------------------------
# 4. PREPARE CLAIMS DATA (Control Costs)
# ------------------------------------------------------------------------------
cat("Step 3: Preparing Control State Claims...\n")

if (!exists("claims_summary_data")) stop("CRITICAL: 'claims_summary_data' missing.")

# Calculate Lifetime Risk for Control Facilities
lifetime_risk <- risk_control[, .(lifetime_annual_prob = mean(annual_leak_prob, na.rm=T)), by = panel_id]

# Merge Claims + Risk
reg_data_claims <- merge(
  claims_summary_data, 
  lifetime_risk,
  by = "panel_id",
  all.x = FALSE
)

# Filter 2006+ and Create Age Bins
# FIX: avg_tank_age must exist in claims_summary_data. If not, merge from main panel.
if (!"avg_tank_age" %in% names(reg_data_claims)) {
  # recover age from main panel (snapshot)
  age_lookup <- unique(facility_leak_behavior[, .(panel_id, panel_year, avg_tank_age)])
  reg_data_claims <- merge(reg_data_claims, age_lookup, by=c("panel_id", "panel_year"), all.x=TRUE)
}

reg_data_claims <- reg_data_claims[cleanup_cost > 0 & panel_year >= 2006 & !is.na(avg_tank_age)]

reg_data_claims[, age_bin := cut(
  avg_tank_age, 
  breaks = c(0, 5, 10, 15, 20, 25, 30, 35, 40, Inf),
  labels = age_levels,
  include.lowest = TRUE, right = FALSE
)]

cat(sprintf("  Claims Analysis Sample: %s incidents.\n", format(nrow(reg_data_claims), big.mark=",")))

# ------------------------------------------------------------------------------
# RESULT R1: PREMIUM ~ RISK (Tank Level)
# ------------------------------------------------------------------------------
cat("\nRunning Result R1: Premium Sensitivity...\n")

# Hard execution (will fail if data invalid)
m1 <- feols(log(mean_tank_premium) ~ annual_leak_prob, data=reg_data_premium, cluster="FACILITY_ID")
m2 <- feols(log(mean_tank_premium) ~ annual_leak_prob + age_years + I(age_years^2), data=reg_data_premium, cluster="FACILITY_ID")
m3 <- feols(log(mean_tank_premium) ~ annual_leak_prob + age_years + factor(tech_type) | YEAR, data=reg_data_premium, cluster="FACILITY_ID")

etable(m1, m2, m3, file = here("Output", "Tables", "TR01_premium_leak_regression.tex"), replace = TRUE)
cat("Table R1 saved.\n")

# Figure R1: Binned Scatter
# Robust Quantile Cuts (Unique)
breaks_r1 <- unique(quantile(reg_data_premium[annual_leak_prob > 0]$annual_leak_prob, probs = seq(0, 1, 0.1), na.rm=TRUE))

if (length(breaks_r1) >= 2) {
  reg_data_premium[, leak_bin := cut(annual_leak_prob, breaks = c(-Inf, breaks_r1), labels = FALSE)]
  
  binned_prem <- reg_data_premium[!is.na(leak_bin), .(
    mean_y = mean(mean_tank_premium, na.rm=T),
    se_y = sd(mean_tank_premium, na.rm=T)/sqrt(.N),
    mean_x = mean(annual_leak_prob, na.rm=T)
  ), by = leak_bin]
  
  p_r1 <- ggplot(binned_prem, aes(x = mean_x*100, y = mean_y)) +
    geom_point(color="#D55E00", size=3) +
    geom_errorbar(aes(ymin = mean_y - 1.96*se_y, ymax = mean_y + 1.96*se_y), width=0, color="#D55E00") +
    geom_smooth(method="lm", se=FALSE, color="gray40", linetype="dashed") +
    labs(title="Figure R1: Premium Response to Risk (Tank Level)", x="Annual Leak Probability (%)", y="Mean Tank Premium ($)") +
    theme_academic()
  
  save_figure(p_r1, "FR01_premium_leak_scatter", width=10, height=6)
  cat("Figure R1 saved.\n")
}

# ------------------------------------------------------------------------------
# RESULT R2: COST ~ RISK (Control)
# ------------------------------------------------------------------------------
cat("\nRunning Result R2: Cost Sensitivity...\n")

c1 <- feols(log(cleanup_cost) ~ lifetime_annual_prob, data=reg_data_claims, cluster="state")
c2 <- feols(log(cleanup_cost) ~ lifetime_annual_prob + avg_tank_age, data=reg_data_claims, cluster="state")

etable(c1, c2, file = here("Output", "Tables", "TR02_claims_leak_regression.tex"), replace = TRUE)
cat("Table R2 saved.\n")


# ==============================================================================
# RESULT R3: MULTI-STAGE GRADIENT VALIDATION (Improved Labels)
# ==============================================================================
# 1. Internal Validity (TX): Premium vs. Texas Physical Risk
# 2. Internal Validity (Control): Cost vs. Control Physical Risk
# 3. External Validity: Comparing Physical Risk Profiles
# ==============================================================================

cat("\nRunning Result R3: 3-Way Gradient Validation...\n")

# ------------------------------------------------------------------------------
# 1. DATA AGGREGATION (Unchanged Logic)
# ------------------------------------------------------------------------------

# A. PREMIUM GRADIENT (Texas Tank-Level)
grad_prem <- reg_data_premium[!is.na(age_bin), .(
  mean_premium = mean(mean_tank_premium, na.rm=TRUE)
), by = age_bin]

# B. COST GRADIENT (Control Claim-Level)
grad_cost <- reg_data_claims[!is.na(age_bin), .(
  mean_cost = mean(cleanup_cost, na.rm=TRUE)
), by = age_bin]

# C. RISK GRADIENTS (Facility-Level, Split by Group)
risk_split <- facility_leak_behavior[
  is_analysis_sample == TRUE & active_tanks > 0 & panel_year >= 2006
]
risk_split[, age_bin := cut(avg_tank_age, breaks=c(0,5,10,15,20,25,30,35,40,Inf), labels=age_levels, include.lowest=T, right=F)]

grad_risk <- risk_split[!is.na(age_bin), .(
  n_leaks = sum(leak_incident, na.rm=TRUE),
  n_years = .N / 12
), by = .(treatment_group, age_bin)]
grad_risk[, annual_prob := n_leaks / n_years]

grad_risk_tx <- grad_risk[treatment_group == "Texas", .(age_bin, risk_tx = annual_prob)]
grad_risk_ctrl <- grad_risk[treatment_group == "Control", .(age_bin, risk_ctrl = annual_prob)]

# D. MERGE MASTER TABLE
grad_master <- merge(grad_prem, grad_cost, by="age_bin", all=TRUE)
grad_master <- merge(grad_master, grad_risk_tx, by="age_bin", all=TRUE)
grad_master <- merge(grad_master, grad_risk_ctrl, by="age_bin", all=TRUE)
grad_master[, age_bin := factor(age_bin, levels = age_levels)]

# Save Summary Table
t_r3 <- grad_master[, .(
  `Age Bin` = age_bin,
  `Premium (TX)` = sprintf("$%s", format(round(mean_premium), big.mark=",")),
  `Risk (TX)` = sprintf("%.2f%%", risk_tx * 100),
  `Cost (Control)` = sprintf("$%s", format(round(mean_cost), big.mark=",")),
  `Risk (Control)` = sprintf("%.2f%%", risk_ctrl * 100)
)] %>% gt() %>%
  tab_header(title = "Table R3: Gradients by Region", subtitle = "Comparing internal pricing/risk dynamics")

save_table(t_r3, grad_master, "TR03_gradients_split")
cat("Table R3 saved.\n")

# ------------------------------------------------------------------------------
# 2. FIGURE R3A: TEXAS INTERNAL VALIDITY (Premium vs. Texas Risk)
# ------------------------------------------------------------------------------
# Does the Texas Premium track the Texas Risk profile?

scale_tx <- max(grad_master$mean_premium, na.rm=T) / max(grad_master$risk_tx, na.rm=T)

p_r3a <- ggplot(grad_master[!is.na(age_bin)], aes(x = age_bin)) +
  # Premium Bars
  geom_col(aes(y = mean_premium, fill = "Realized Premium ($)"), alpha = 0.7, width = 0.6) +
  # Risk Line (Scaled)
  geom_line(aes(y = risk_tx * scale_tx, group=1, color = "Observed Leak Frequency"), size = 1.2, linetype="dashed") +
  geom_point(aes(y = risk_tx * scale_tx, color = "Observed Leak Frequency"), size = 3) +
  
  scale_y_continuous(
    name = "Mean Tank Premium ($)", labels = scales::dollar,
    sec.axis = sec_axis(~ . / scale_tx, name = "Annual Leak Probability (Incidents/Year)", labels = scales::percent)
  ) +
  scale_fill_manual(values = c("Realized Premium ($)" = "#D55E00"), name = NULL) +
  scale_color_manual(values = c("Observed Leak Frequency" = "black"), name = "Secondary Axis") +
  labs(title = "Figure R3a: Internal Validity (Texas Pricing)",
       subtitle = "Actuarial premiums (Bars) closely track physical leak risk (Line)",
       x = "Tank Age (Years)") +
  theme_academic() + theme(legend.position = "bottom")

save_figure(p_r3a, "FR03a_internal_texas", width = 10, height = 6)
cat("Figure R3a saved.\n")

# ------------------------------------------------------------------------------
# 3. FIGURE R3B: CONTROL INTERNAL VALIDITY (Cost vs. Control Risk)
# ------------------------------------------------------------------------------
# Do Control Costs track Control Risk?

scale_ctrl <- max(grad_master$mean_cost, na.rm=T) / max(grad_master$risk_ctrl, na.rm=T)

p_r3b <- ggplot(grad_master[!is.na(age_bin)], aes(x = age_bin)) +
  # Cost Bars
  geom_col(aes(y = mean_cost, fill = "Avg Cleanup Cost ($)"), alpha = 0.7, width = 0.6) +
  # Risk Line (Scaled)
  geom_line(aes(y = risk_ctrl * scale_ctrl, group=1, color = "Observed Leak Frequency"), size = 1.2, linetype="dashed") +
  geom_point(aes(y = risk_ctrl * scale_ctrl, color = "Observed Leak Frequency"), size = 3) +
  
  scale_y_continuous(
    name = "Mean Cleanup Cost ($)", labels = scales::dollar,
    sec.axis = sec_axis(~ . / scale_ctrl, name = "Annual Leak Probability (Incidents/Year)", labels = scales::percent)
  ) +
  scale_fill_manual(values = c("Avg Cleanup Cost ($)" = "#0072B2"), name = NULL) +
  scale_color_manual(values = c("Observed Leak Frequency" = "black"), name = "Secondary Axis") +
  labs(title = "Figure R3b: Internal Validity (Control Damages)",
       subtitle = "Realized damages (Bars) scale with physical leak risk (Line)",
       x = "Tank Age (Years)") +
  theme_academic() + theme(legend.position = "bottom")

save_figure(p_r3b, "FR03b_internal_control", width = 10, height = 6)
cat("Figure R3b saved.\n")

# ------------------------------------------------------------------------------
# 4. FIGURE R3C: EXTERNAL VALIDITY (Risk Profile Comparison)
# ------------------------------------------------------------------------------
# Do Texas and Control states share the same age-risk gradient?

grad_long <- melt(
  grad_master[, .(age_bin, Texas = risk_tx, Control = risk_ctrl)],
  id.vars = "age_bin",
  variable.name = "Region",
  value.name = "Probability"
)

p_r3c <- ggplot(grad_long[!is.na(age_bin)], aes(x = age_bin, y = Probability, color = Region, group = Region)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  scale_y_continuous(
    labels = scales::percent,
    name = "Annual Leak Probability (Incidents/Year)"
  ) +
  scale_color_manual(
    values = c("Texas" = "#D55E00", "Control" = "#0072B2"),
    name = "Jurisdiction"
  ) +
  labs(title = "Figure R3c: External Validity (Physical Risk Profile)",
       subtitle = "Parallel age-risk gradients justify using Control damages as a proxy for Texas",
       x = "Tank Age (Years)",
       caption = "Metric: Annual probability of a leak incident per active facility.") +
  theme_academic() + theme(legend.position = "bottom")

save_figure(p_r3c, "FR03c_risk_comparison", width = 10, height = 6)
cat("Figure R3c saved.\n")

# Cleanup
rm(grad_master, grad_prem, grad_cost, grad_risk, grad_risk_tx, grad_risk_ctrl, grad_long, risk_split)
invisible(gc())

cat("\nRESULTS SECTION COMPLETE.\n")

# ==============================================================================
# SECTION 04: EVENT FREQUENCIES (1970-2023) - CORRECTED
# ==============================================================================
# Fix: Changed 'facility_exit' to 'exit_flag' to match panel creation script
# ==============================================================================

track_section_time("Start Section 04")

cat(rep("=", 80), "\n", sep = "")
cat("SECTION 04: EVENT FREQUENCIES (1970-2023)\n")
cat(rep("=", 80), "\n\n")

cat("Calculating annual event rates (extended time window)...\n")

# Calculate annual rates
# FIX: Use 'exit_flag' instead of 'facility_exit'
annual_rates <- facility_leak_behavior[
  is_analysis_sample == TRUE & 
  treatment_group %in% c("Texas", "Control"),
  .(
    n_facilities = uniqueN(panel_id),
    n_facility_months = .N,
    
    # Closure/exit events
    closure_rate = 100 * sum(tanks_closed > 0, na.rm = TRUE) / .N,
    exit_rate = 100 * sum(exit_flag, na.rm = TRUE) / .N,  # <--- FIXED HERE
    
    # Replacement events
    replacement_rate = 100 * sum(replacement_event, na.rm = TRUE) / .N,
    
    # Leak events
    leak_rate = 100 * sum(leak_incident, na.rm = TRUE) / .N
  ),
  by = .(treatment_group, panel_year)
]

# Figure 4A: Closure Rates (Individual) 
p_closure <- ggplot(
  annual_rates,
  aes(x = panel_year, y = closure_rate, color = treatment_group)
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2, alpha = 0.6) +
  geom_vline(xintercept = 1999.5, linetype = "dashed", color = "red", alpha = 0.7) +
  scale_color_manual(values = treatment_colors, name = "Group") +
  scale_x_continuous(breaks = seq(1970, 2023, 10)) +
  labs(
    title = "Tank Closure Rates (1970-2023)",
    x = "Year",
    y = "Closure Rate (%)",
    caption = "Red dashed line: 1999 policy change"
  ) +
  theme_academic() +
  theme(legend.position = "bottom")

save_figure(p_closure, "F04a_paneled_closure_rates", width = 10, height = 6)
cat("Figure 4A saved.\n")

# Figure 4B: Exit Rates (Individual) 
p_exit <- ggplot(
  annual_rates,
  aes(x = panel_year, y = exit_rate, color = treatment_group)
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2, alpha = 0.6) +
  geom_vline(xintercept = 1999.5, linetype = "dashed", color = "red", alpha = 0.7) +
  scale_color_manual(values = treatment_colors, name = "Group") +
  scale_x_continuous(breaks = seq(1970, 2023, 10)) +
  labs(
    title = "Facility Exit Rates (1970-2023)",
    x = "Year",
    y = "Exit Rate (%)",
    caption = "Red dashed line: 1999 policy change"
  ) +
  theme_academic() +
  theme(legend.position = "bottom")

save_figure(p_exit, "F04b_paneled_exit_rates", width = 10, height = 6)
cat("Figure 4B saved.\n")

# Figure 4C: Replacement Rates (Individual) 
p_replacement <- ggplot(
  annual_rates,
  aes(x = panel_year, y = replacement_rate, color = treatment_group)
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2, alpha = 0.6) +
  geom_vline(xintercept = 1999.5, linetype = "dashed", color = "red", alpha = 0.7) +
  scale_color_manual(values = treatment_colors, name = "Group") +
  scale_x_continuous(breaks = seq(1970, 2023, 10)) +
  labs(
    title = "Tank Replacement Rates (1970-2023)",
    x = "Year",
    y = "Replacement Rate (%)",
    caption = "Red dashed line: 1999 policy change"
  ) +
  theme_academic() +
  theme(legend.position = "bottom")

save_figure(p_replacement, "F04c_paneled_replacement_rates", width = 10, height = 6)
cat("Figure 4C saved.\n")

# Figure 4D: Leak Rates (Individual) 
p_leak <- ggplot(
  annual_rates,
  aes(x = panel_year, y = leak_rate, color = treatment_group)
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2, alpha = 0.6) +
  geom_vline(xintercept = 1999.5, linetype = "dashed", color = "red", alpha = 0.7) +
  scale_color_manual(values = treatment_colors, name = "Group") +
  scale_x_continuous(breaks = seq(1970, 2023, 10)) +
  labs(
    title = "Leak Incidence Rates (1970-2023)",
    x = "Year",
    y = "Leak Rate (%)",
    caption = "Red dashed line: 1999 policy change"
  ) +
  theme_academic() +
  theme(legend.position = "bottom")

save_figure(p_leak, "F04d_paneled_leak_rates", width = 10, height = 6)
cat("Figure 4D saved.\n")

# Figure 4: Combined patchwork (2×2 grid)
p_combined_rates <- (p_closure + p_exit) / (p_replacement + p_leak) +
  plot_layout(guides = "collect") +
  plot_annotation(
    title = "Figure 4: Event Frequencies Over Time (1970-2023)",
    subtitle = "Texas vs. Control comparison with 30-year pre-trends",
    caption = "Extended time window enables validation of parallel trends assumption.
               Dashed red line: 1999 Texas policy change to risk-based pricing.",
    theme = theme(plot.title = element_text(face = "bold", size = 14),
                  legend.position = "bottom")
  )

save_figure(p_combined_rates, "F04_combined_event_frequencies", width = 14, height = 10)
cat("Figure 4 (combined) saved.\n")

track_section_time("Section 04: Event Frequencies")

# ==============================================================================
# SECTION 4.5: DISCRETE CHOICE TIMING DIAGNOSTICS (CORRECTED)
# ==============================================================================
# Purpose: Assess if discrete events (Exit, Removal, Replacement) occur in
#          years where Dewey data (2014+) is available.
# FIXES:
# 1. Sample Consistency: Now strictly uses 'is_analysis_sample == TRUE'.
# 2. Variable Scoping: Calculates 'retrofit_possible' separately to avoid errors.
# 3. Variable Naming: Uses 'exit_flag' consistent with Section 00.
# ==============================================================================

track_section_time("Start Section 4.5")

cat(rep("=", 80), "\n", sep = "")
cat("SECTION 4.5: DISCRETE CHOICE TIMING DIAGNOSTICS\n")
cat("Assessing temporal concentration of discrete decisions for identification\n")
cat(rep("=", 80), "\n\n")

cat("Preparing discrete choice event data (Analysis Sample Only)...\n")

# 1. Extract raw event indicators for the ANALYSIS SAMPLE only
#    We subset first to ensure we aren't analyzing facilities dropped elsewhere.
dc_raw <- facility_leak_behavior[
  is_analysis_sample == TRUE & cohort == "Incumbent", 
  .(
    panel_id, 
    panel_year, 
    treatment_group,
    is_motor_fuel,
    exit_flag,          # From Section 00
    tanks_closed,
    replacement_event,
    wall_type,
    double_tanks
  )
]

# 2. Calculate Event Flags (handling scoping correctly)
#    We prioritize "Event happened this year" logic.
discrete_choices <- dc_raw[, .(
  # Exit: Facility exits market
  exit_event = max(exit_flag, na.rm = TRUE),
  
  # Removal: Any tank closed
  removal_event = max(tanks_closed > 0, na.rm = TRUE),
  
  # Replacement: Any tank replaced
  replacement_event = max(replacement_event, na.rm = TRUE),
  
  # Retrofit Logic:
  # Valid only if wall type is known.
  retrofit_possible = any(!is.na(wall_type) & wall_type != "Unknown"),
  
  # Event = Valid Wall Type AND increase in double-walled tanks
  retrofit_event = max(
    (!is.na(wall_type) & wall_type != "Unknown") & 
      (double_tanks > shift(double_tanks, 1, type = "lag", fill = 0)), 
    na.rm = TRUE
  ),
  
  # Pass through grouping vars
  treatment_group = first(treatment_group),
  is_motor_fuel = max(is_motor_fuel, na.rm = TRUE)
), by = .(panel_id, panel_year)]

# 3. Aggregate to Year-Level Counts
choice_counts_annual <- discrete_choices[, .(
  n_facilities = uniqueN(panel_id),
  n_exits = sum(exit_event == 1, na.rm = TRUE),
  n_removals = sum(removal_event == 1, na.rm = TRUE),
  n_replacements = sum(replacement_event == 1, na.rm = TRUE),
  n_retrofits = sum(retrofit_event == 1 & retrofit_possible, na.rm = TRUE) # Only count if possible
), by = .(panel_year, treatment_group)]

# 4. Calculate Cumulative Shares (Identification Power)
choice_counts_annual[, `:=`(
  cum_exits = cumsum(n_exits),
  cum_removals = cumsum(n_removals),
  cum_replacements = cumsum(n_replacements),
  cum_retrofits = cumsum(n_retrofits)
), by = treatment_group]

# Normalize to percentages of total observed events
choice_counts_annual[, `:=`(
  pct_exits_to_date = 100 * cum_exits / max(cum_exits, na.rm = TRUE),
  pct_removals_to_date = 100 * cum_removals / max(cum_removals, na.rm = TRUE),
  pct_replacements_to_date = 100 * cum_replacements / max(cum_replacements, na.rm = TRUE),
  pct_retrofits_to_date = 100 * cum_retrofits / max(cum_retrofits, na.rm = TRUE)
), by = treatment_group]

# ============================================================================
# Figure 8A: Annual Discrete Choice Counts
# ============================================================================

cat("Creating Figure 8A: Annual discrete choice counts...\n")

choice_counts_long <- melt(
  choice_counts_annual[treatment_group %in% c("Texas", "Control")],
  id.vars = c("panel_year", "treatment_group"),
  measure.vars = c("n_exits", "n_removals", "n_replacements", "n_retrofits"),
  variable.name = "choice_type",
  value.name = "count"
)

choice_counts_long[, choice_label := fcase(
  choice_type == "n_exits", "Facility Exits",
  choice_type == "n_removals", "Tank Removals (Any)",
  choice_type == "n_replacements", "Tank Replacements",
  choice_type == "n_retrofits", "Retrofits (SW -> DW)",
  default = as.character(choice_type)
)]

# Ensure DEWEY_START_YEAR is defined
if (!exists("DEWEY_START_YEAR")) DEWEY_START_YEAR <- 2014

p_choice_counts <- ggplot(
  choice_counts_long,
  aes(x = panel_year, y = count, color = treatment_group, group = treatment_group)
) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.5, alpha = 0.6) +
  geom_vline(xintercept = 1999.5, linetype = "dashed", color = "red", alpha = 0.7) +
  geom_vline(xintercept = DEWEY_START_YEAR, linetype = "dotted", color = "blue", alpha = 0.5) +
  annotate("text", x = DEWEY_START_YEAR, y = Inf, label = "Dewey data\navailable", 
           vjust = 1.1, hjust = -0.1, size = 3, color = "blue") +
  facet_wrap(~ choice_label, scales = "free_y", ncol = 2) +
  scale_color_manual(values = treatment_colors, name = "Group") +
  scale_x_continuous(breaks = seq(1970, 2023, 10)) +
  labs(
    title = "Annual Discrete Choice Counts (1970-2023)",
    subtitle = "Number of facilities making each decision type per year (Analysis Sample Only)",
    x = "Year",
    y = "Count (facilities)",
    caption = "Red dashed: 1999 policy change. Blue dotted: 2014 (Dewey data start).
               Counts reflect the exact sample used for regression analysis."
  ) +
  theme_academic() +
  theme(legend.position = "bottom")

save_figure(p_choice_counts, "F08a_discrete_choice_counts_annual", width = 14, height = 10)

cat("Figure 8A saved.\n")

# ============================================================================
# Figure 8B: Cumulative Decision Distribution
# ============================================================================

cat("Creating Figure 8B: Cumulative hazard curves...\n")

cum_hazard_long <- melt(
  choice_counts_annual[treatment_group %in% c("Texas", "Control")],
  id.vars = c("panel_year", "treatment_group"),
  measure.vars = c("pct_exits_to_date", "pct_removals_to_date", 
                   "pct_replacements_to_date", "pct_retrofits_to_date"),
  variable.name = "choice_type",
  value.name = "cum_pct"
)

cum_hazard_long[, choice_label := fcase(
  choice_type == "pct_exits_to_date", "Exits",
  choice_type == "pct_removals_to_date", "Removals",
  choice_type == "pct_replacements_to_date", "Replacements",
  choice_type == "pct_retrofits_to_date", "Retrofits",
  default = as.character(choice_type)
)]

p_cum_hazard <- ggplot(
  cum_hazard_long[treatment_group == "Texas"],
  aes(x = panel_year, y = cum_pct, color = choice_label, linetype = choice_label)
) +
  geom_line(linewidth = 1.2) +
  geom_hline(yintercept = c(50, 75, 90), linetype = "dotted", alpha = 0.3) +
  geom_vline(xintercept = 1999.5, linetype = "dashed", color = "red", alpha = 0.7) +
  geom_vline(xintercept = DEWEY_START_YEAR, linetype = "dotted", color = "blue", alpha = 0.5) +
  annotate("rect", xmin = DEWEY_START_YEAR, xmax = 2023, ymin = 0, ymax = 100,
           alpha = 0.1, fill = "blue") +
  annotate("text", x = (DEWEY_START_YEAR + 2023) / 2, y = 95, 
           label = "Dewey Data Period", 
           size = 3.5, color = "blue", fontface = "italic") +
  scale_color_viridis_d(name = "Decision Type", option = "plasma") +
  scale_linetype_manual(values = c(1, 2, 3, 4), name = "Decision Type") +
  scale_x_continuous(breaks = seq(1970, 2023, 10)) +
  scale_y_continuous(breaks = seq(0, 100, 25), limits = c(0, 100)) +
  labs(
    title = "Cumulative Distribution of Discrete Choices (Texas)",
    subtitle = "What percentage of total decisions occur by year X?",
    x = "Year",
    y = "Cumulative % of All Decisions",
    caption = "Shaded region: Dewey data (2014-2023). Steep slopes = high decision activity periods."
  ) +
  theme_academic() +
  theme(legend.position = "right")

save_figure(p_cum_hazard, "F08b_cumulative_decision_distribution", width = 12, height = 7)

cat("Figure 8B saved.\n")

# ============================================================================
# Figure 8C: Decision Intensity by Period
# ============================================================================

cat("Creating Figure 8C: Decision intensity by period...\n")

period_summary <- discrete_choices[, .(
  n_any_decision = sum(exit_event == 1 | removal_event == 1 | 
                         replacement_event == 1 | retrofit_event == 1, na.rm = TRUE),
  n_exits = sum(exit_event == 1, na.rm = TRUE),
  n_removals = sum(removal_event == 1, na.rm = TRUE),
  n_replacements = sum(replacement_event == 1, na.rm = TRUE),
  n_retrofits = sum(retrofit_event == 1 & retrofit_possible, na.rm = TRUE)
), by = .(treatment_group, 
          period = fcase(
            panel_year >= 2014, "2014-2023\n(Dewey Era)",
            panel_year >= 2010, "2010-2013\n(Pre-Dewey)",
            panel_year >= 2000, "2000-2009\n(Post-Policy)",
            panel_year >= 1990, "1990-1999\n(Pre-Policy)",
            default = "1970-1989\n(Early)"
          ))]

period_long <- melt(
  period_summary[treatment_group %in% c("Texas", "Control")],
  id.vars = c("treatment_group", "period"),
  measure.vars = c("n_exits", "n_removals", "n_replacements", "n_retrofits"),
  variable.name = "decision_type",
  value.name = "count"
)

period_long[, decision_label := fcase(
  decision_type == "n_exits", "Exits",
  decision_type == "n_removals", "Removals",
  decision_type == "n_replacements", "Replacements",
  decision_type == "n_retrofits", "Retrofits"
)]

period_long[, period := factor(period, levels = c(
  "1970-1989\n(Early)",
  "1990-1999\n(Pre-Policy)",
  "2000-2009\n(Post-Policy)",
  "2010-2013\n(Pre-Dewey)",
  "2014-2023\n(Dewey Era)"
))]

p_period_intensity <- ggplot(
  period_long,
  aes(x = period, y = count, fill = decision_label)
) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~ treatment_group, ncol = 1) +
  scale_fill_viridis_d(name = "Decision Type", option = "plasma") +
  labs(
    title = "Decision Intensity by Period",
    subtitle = "Total discrete choices per period—where is the action?",
    x = "Time Period",
    y = "Total Decisions (Cumulative)",
    caption = "Height shows total decisions within the Analysis Sample."
  ) +
  theme_academic() +
  theme(
    axis.text.x = element_text(size = 9),
    legend.position = "right"
  )

save_figure(p_period_intensity, "F08c_decision_intensity_by_period", width = 12, height = 8)

cat("Figure 8C saved.\n")
# ==============================================================================
# CORRECTED SECTION: Table 12 Generation
# ==============================================================================

cat("Creating Table 12: Decision timing summary statistics (CORRECTED)...\n")

# FIX: Wrap all median() calls in as.numeric() to prevent integer/double type conflicts
#      between groups with 1 observation vs groups with even numbers of observations.
timing_summary <- discrete_choices[, .(
  total_decisions = sum(exit_event == 1 | removal_event == 1 | 
                          replacement_event == 1 | retrofit_event == 1, na.rm = TRUE),
  
  # Exits
  n_exits = sum(exit_event == 1, na.rm = TRUE),
  median_exit_year = as.numeric(median(panel_year[exit_event == 1], na.rm = TRUE)),
  exits_2000_2009 = sum(exit_event == 1 & panel_year >= 2000 & panel_year <= 2009, na.rm = TRUE),
  exits_2010_2013 = sum(exit_event == 1 & panel_year >= 2010 & panel_year <= 2013, na.rm = TRUE),
  exits_2014_2023 = sum(exit_event == 1 & panel_year >= 2014, na.rm = TRUE),
  
  # Removals
  n_removals = sum(removal_event == 1, na.rm = TRUE),
  median_removal_year = as.numeric(median(panel_year[removal_event == 1], na.rm = TRUE)), # The specific crash point
  removals_2000_2009 = sum(removal_event == 1 & panel_year >= 2000 & panel_year <= 2009, na.rm = TRUE),
  removals_2010_2013 = sum(removal_event == 1 & panel_year >= 2010 & panel_year <= 2013, na.rm = TRUE),
  removals_2014_2023 = sum(removal_event == 1 & panel_year >= 2014, na.rm = TRUE),
  
  # Replacements
  n_replacements = sum(replacement_event == 1, na.rm = TRUE),
  median_replacement_year = as.numeric(median(panel_year[replacement_event == 1], na.rm = TRUE)),
  replacements_2000_2009 = sum(replacement_event == 1 & panel_year >= 2000 & panel_year <= 2009, na.rm = TRUE),
  replacements_2010_2013 = sum(replacement_event == 1 & panel_year >= 2010 & panel_year <= 2013, na.rm = TRUE),
  replacements_2014_2023 = sum(replacement_event == 1 & panel_year >= 2014, na.rm = TRUE),
  
  # Retrofits
  n_retrofits = sum(retrofit_event == 1 & retrofit_possible, na.rm = TRUE),
  median_retrofit_year = as.numeric(median(panel_year[retrofit_event == 1 & retrofit_possible], na.rm = TRUE)),
  retrofits_2000_2009 = sum(retrofit_event == 1 & retrofit_possible & 
                              panel_year >= 2000 & panel_year <= 2009, na.rm = TRUE),
  retrofits_2010_2013 = sum(retrofit_event == 1 & retrofit_possible & 
                              panel_year >= 2010 & panel_year <= 2013, na.rm = TRUE),
  retrofits_2014_2023 = sum(retrofit_event == 1 & retrofit_possible & 
                              panel_year >= 2014, na.rm = TRUE)
), by = treatment_group]

# Calculate percentages
timing_summary[, `:=`(
  pct_exits_2014_plus = 100 * exits_2014_2023 / n_exits,
  pct_removals_2014_plus = 100 * removals_2014_2023 / n_removals,
  pct_replacements_2014_plus = 100 * replacements_2014_2023 / n_replacements,
  pct_retrofits_2014_plus = 100 * retrofits_2014_2023 / n_retrofits
)]

# Reshape for display
timing_display <- rbind(
  timing_summary[treatment_group %in% c("Texas", "Control"), 
                 .(treatment_group, decision_type = "Exits",
                   total_n = n_exits, median_year = median_exit_year,
                   n_2000_2009 = exits_2000_2009, pct_2000_2009 = 100 * exits_2000_2009 / n_exits,
                   n_2010_2013 = exits_2010_2013, pct_2010_2013 = 100 * exits_2010_2013 / n_exits,
                   n_2014_2023 = exits_2014_2023, pct_2014_2023 = pct_exits_2014_plus)],
  
  timing_summary[treatment_group %in% c("Texas", "Control"), 
                 .(treatment_group, decision_type = "Removals",
                   total_n = n_removals, median_year = median_removal_year,
                   n_2000_2009 = removals_2000_2009, pct_2000_2009 = 100 * removals_2000_2009 / n_removals,
                   n_2010_2013 = removals_2010_2013, pct_2010_2013 = 100 * removals_2010_2013 / n_removals,
                   n_2014_2023 = removals_2014_2023, pct_2014_2023 = pct_removals_2014_plus)],
  
  timing_summary[treatment_group %in% c("Texas", "Control"), 
                 .(treatment_group, decision_type = "Replacements",
                   total_n = n_replacements, median_year = median_replacement_year,
                   n_2000_2009 = replacements_2000_2009, pct_2000_2009 = 100 * replacements_2000_2009 / n_replacements,
                   n_2010_2013 = replacements_2010_2013, pct_2010_2013 = 100 * replacements_2010_2013 / n_replacements,
                   n_2014_2023 = replacements_2014_2023, pct_2014_2023 = pct_replacements_2014_plus)],
  
  timing_summary[treatment_group %in% c("Texas", "Control"), 
                 .(treatment_group, decision_type = "Retrofits",
                   total_n = n_retrofits, median_year = median_retrofit_year,
                   n_2000_2009 = retrofits_2000_2009, pct_2000_2009 = 100 * retrofits_2000_2009 / n_retrofits,
                   n_2010_2013 = retrofits_2010_2013, pct_2010_2013 = 100 * retrofits_2010_2013 / n_retrofits,
                   n_2014_2023 = retrofits_2014_2023, pct_2014_2023 = pct_retrofits_2014_plus)]
)

# Create GT table
t12_academic <- timing_display %>%
  gt(groupname_col = "treatment_group") %>%
  tab_header(
    title = "Table 12: Temporal Distribution of Discrete Choices",
    subtitle = "Analysis Sample Only (Matched to Regression Population)"
  ) %>%
  tab_spanner(label = "2000-2009 (Post-Policy Wave)", 
              columns = c(n_2000_2009, pct_2000_2009)) %>%
  tab_spanner(label = "2010-2013 (Pre-Dewey)", 
              columns = c(n_2010_2013, pct_2010_2013)) %>%
  tab_spanner(label = "2014-2023 (Dewey Era)", 
              columns = c(n_2014_2023, pct_2014_2023)) %>%
  cols_label(
    decision_type = "Decision", total_n = "Total N", median_year = "Median Year",
    n_2000_2009 = "N", pct_2000_2009 = "%",
    n_2010_2013 = "N", pct_2010_2013 = "%",
    n_2014_2023 = "N", pct_2014_2023 = "%"
  ) %>%
  fmt_number(columns = c(total_n, n_2000_2009, n_2010_2013, n_2014_2023), 
             decimals = 0, sep_mark = ",") %>%
  fmt_number(columns = c(pct_2000_2009, pct_2010_2013, pct_2014_2023, median_year), 
             decimals = 1) %>%
  data_color(columns = pct_2014_2023, method = "numeric", 
             palette = "Reds", domain = c(0, 50)) %>%
  tab_source_note(
    "Notes: Highlighted column shows proportion of variation available for Dewey databases analysis.
     Low percentages (<20%) suggest limited identification power for causal inference on discrete choices."
  )

save_table(t12_academic, timing_display, "T12_decision_timing_summary")

export_caption(
  "T12",
  "Temporal Distribution of Discrete Choices",
  "When discrete decisions occur, informing value of external data integration (Dewey, 2014+).",
  sprintf("Majority of decisions occur 2000-2013. Texas exits: %.1f%% by 2013, only %.1f%% post-2014. Replacements show more post-2014 activity (%.1f%%). Median decision years cluster 2004-2008. Implications: (1) Main treatment effects realized by 2013, (2) Limited power for discrete choice analysis using Dewey data, (3) Post-2014 variation reflects long-run equilibrium. External data best for: station profitability, pricing pass-through, consumer welfare—NOT facility exit/investment decisions.",
          timing_display[treatment_group == "Texas" & decision_type == "Exits", 100 - pct_2014_2023],
          timing_display[treatment_group == "Texas" & decision_type == "Exits", pct_2014_2023],
          timing_display[treatment_group == "Texas" & decision_type == "Replacements", pct_2014_2023]),
  "Timing diagnostics show where identifying variation comes from and whether external data align with decision periods.",
  "If <20% post-2014, Dewey provides limited power for discrete choice effects. Better for equilibrium outcomes."
)

cat("Table 12 saved.\n")

# ============================================================================
# Console Summary Output
# ============================================================================

cat("\n", rep("=", 80), "\n", sep = "")
cat("DISCRETE CHOICE TIMING SUMMARY\n")
cat(rep("=", 80), "\n")

texas_timing <- timing_summary[treatment_group == "Texas"]

cat("\nTEXAS (Treatment Group):\n")
cat(sprintf("  Total Exits: %s (%.1f%% occur 2014+)\n",
            format(texas_timing$n_exits, big.mark = ","),
            texas_timing$pct_exits_2014_plus))
cat(sprintf("  Total Removals: %s (%.1f%% occur 2014+)\n",
            format(texas_timing$n_removals, big.mark = ","),
            texas_timing$pct_removals_2014_plus))
cat(sprintf("  Total Replacements: %s (%.1f%% occur 2014+)\n",
            format(texas_timing$n_replacements, big.mark = ","),
            texas_timing$pct_replacements_2014_plus))
cat(sprintf("  Total Retrofits: %s (%.1f%% occur 2014+)\n",
            format(texas_timing$n_retrofits, big.mark = ","),
            texas_timing$pct_retrofits_2014_plus))

cat("\nIMPLICATIONS FOR DEWEY DATA INTEGRATION:\n")

dewey_power_score <- mean(c(
  texas_timing$pct_exits_2014_plus,
  texas_timing$pct_removals_2014_plus,
  texas_timing$pct_replacements_2014_plus,
  texas_timing$pct_retrofits_2014_plus
), na.rm = TRUE)

if (dewey_power_score < 15) {
  cat("  ⚠ LOW IDENTIFICATION POWER (<15% of decisions post-2014)\n")
  cat("  -> Dewey unlikely to improve causal inference on discrete choices\n")
  cat("  -> Use for: equilibrium pricing, demand shocks, competition\n")
  cat("  -> NOT for: exit decisions, retrofit analysis, closure timing\n")
} else if (dewey_power_score < 30) {
  cat("  ⚠ MODERATE POWER (15-30% of decisions post-2014)\n")
  cat("  -> Dewey may add value for specific outcomes (replacements)\n")
  cat("  -> Useful for: profitability analysis, local competition\n")
  cat("  -> Limited for: exit analysis (insufficient variation)\n")
} else {
  cat("  ✓ SUFFICIENT POWER (>30% of decisions post-2014)\n")
  cat("  -> Dewey integration likely valuable\n")
  cat("  -> Recommended: station revenue, pricing, consumer welfare\n")
}

cat("\nRECOMMENDED DEWEY APPLICATIONS:\n")
cat("  1. SafeGraph Spend: Station revenue/transaction analysis\n")
cat("  2. Conv Store Transactions: Fuel sales volumes, pricing\n")
cat("  3. Cross-sectional: Equilibrium effects conditional on infrastructure\n")
cat("  4. Competition: Entry/exit spillovers in local markets\n")
cat("  5. NOT: Dynamic discrete choice (insufficient variation)\n")

cat("\n", rep("=", 80), "\n", sep = "")

cat("\n✓ Section 4.5 complete.\n")
track_section_time("Section 4.5: Discrete Choice Timing")




# ==============================================================================
# SECTION 12: FINAL SUMMARY AND MANIFEST
# ==============================================================================

cat(rep("=", 80), "\n", sep = "")
cat("SECTION 12: FINAL SUMMARY AND MANIFEST\n")
cat(rep("=", 80), "\n\n", sep = "")

# Calculate runtime before creating manifest
script_end_time <- Sys.time()
script_runtime <- difftime(script_end_time, script_start_time, units = "mins")

# Create comprehensive manifest
cat("Creating analysis manifest...\n")
manifest <- list(
  metadata = list(
    script_name = "06_descriptive_work_10_13_REFACTORED.r",
    version = "2.0",
    run_timestamp = script_start_time,
    total_runtime_minutes = as.numeric(script_runtime),
    section_runtimes = section_times,
    r_version = R.version.string,
    platform = Sys.info()["sysname"]
  ),
  
  configuration = list(
    treatment_analysis = TREATMENT_ANALYSIS,
    risk_score_method = RISK_SCORE_METHOD,
    premium_data = USE_PREMIUM_DATA,
    claims_data = USE_CLAIMS_DATA,
    dewey_assessment = ASSESS_DEWEY_TIMING,
    dewey_start_year = DEWEY_START_YEAR,
    time_window = c(1970, 2023),
    cohort_cutoff = 2000
  ),
  
  data_sources = list(
    primary = "facility_leak_behavior_monthly.csv",
    n_facilities = uniqueN(facility_leak_behavior$panel_id),
    n_observations = nrow(facility_leak_behavior),
    date_range = range(facility_leak_behavior$date, na.rm = TRUE),
    analysis_sample_n = nrow(analysis_sample),
    incumbent_sample_n = nrow(incumbent_sample),
    motor_fuel_sample_n = nrow(motor_fuel_sample),
    baseline_1999_n = nrow(baseline_1999)
  ),
  
  tables = list(
    T01 = list(file = "T01_sample_construction", description = "Sample construction (7 steps, extended to 1970)"),
    T02a = list(file = "T02a_facility_chars_texas_WIDE", description = "Balance Table (Texas vs Control, 1999 baseline)"),
    T02b = list(file = "T02b_wall_composition", description = "Wall composition analysis"),
    T02c = list(file = "T02c_premium_distribution", description = "Premium distribution (if available)"),
    T07 = list(file = "T07_leak_hazard_validation", description = "Leak hazard rate validation (regression)"),
    T08 = list(file = "T08_premium_gradient", description = "Premium gradient by age × wall type (if available)"),
    T09 = list(file = "T09_leak_incidence_regression", description = "Leak incidence regression (fixest)"),
    T10 = list(file = "T10_cleanup_cost_quantiles", description = "LUST cleanup cost quantiles"),
    T11 = list(file = "T11_predicted_leak_rates", description = "Predicted leak rates (marginal effects)"),
    T12 = list(file = "T12_decision_timing_summary", description = "Discrete choice timing summary (NEW)"),
    T13 = list(file = "T13_dewey_integration_assessment", description = "Dewey integration assessment (NEW)")
  ),
  
  figures = list(
    F04a = list(file = "F04a_paneled_closure_rates", description = "Tank closure rates (1970-2023)"),
    F04b = list(file = "F04b_paneled_exit_rates", description = "Facility exit rates (1970-2023)"),
    F04c = list(file = "F04c_paneled_replacement_rates", description = "Tank replacement rates (1970-2023)"),
    F04d = list(file = "F04d_paneled_leak_rates", description = "Leak incidence rates (1970-2023)"),
    F04 = list(file = "F04_combined_event_frequencies", description = "Combined event frequencies (2×2 grid)"),
    F05a = list(file = "F05a_installation_vintage_1999", description = "Installation vintage distribution (NEW)"),
    F05b = list(file = "F05b_vintage_evolution", description = "Capital vintage evolution (NEW)"),
    F05 = list(file = "F05_capital_vintage_combined", description = "Combined vintage analysis (NEW)"),
    F06 = list(file = "F06_risk_score_balance", description = "Risk score distribution balance (NEW)"),
    F08a = list(file = "F08a_discrete_choice_counts_annual", description = "Annual discrete choice counts (NEW)"),
    F08b = list(file = "F08b_cumulative_decision_distribution", description = "Cumulative decision distribution (NEW)"),
    F08c = list(file = "F08c_decision_intensity_by_period", description = "Decision intensity by period (NEW)")
  ),
  
  summary = list(
    n_tables = length(list.files(here("Output", "Tables"), pattern = "^T[0-9].*\\.csv$")),
    n_figures_main = length(list.files(here("Output", "Figures"), pattern = "^F[0-9].*\\.png$")),
    n_facilities_total = uniqueN(facility_leak_behavior$panel_id),
    n_facilities_analysis = uniqueN(analysis_sample$panel_id),
    n_facilities_incumbent = uniqueN(incumbent_sample$panel_id),
    n_facilities_motor_fuel = uniqueN(motor_fuel_sample$panel_id),
    n_observations_analysis = nrow(analysis_sample),
    date_range = range(analysis_sample$date, na.rm = TRUE),
    
    key_findings = list(
      texas_facilities = uniqueN(analysis_sample[treatment_group == "Texas", panel_id]),
      control_facilities = uniqueN(analysis_sample[treatment_group == "Control", panel_id]),
      total_leak_events = sum(analysis_sample$leak_incident, na.rm = TRUE),
      total_closure_events = sum(analysis_sample$tanks_closed > 0, na.rm = TRUE),
      total_exit_events = sum(analysis_sample$facility_exit, na.rm = TRUE)
    )
  )
)

# Export manifest as JSON
write_json(manifest, here("Output", "Manifests", "consolidated_analysis_manifest_v2.json"), 
           pretty = TRUE, auto_unbox = TRUE)

cat("Manifest exported to Output/Manifests/consolidated_analysis_manifest_v2.json\n\n")

# Print summary to console
cat(rep("=", 80), "\n", sep = "")
cat("ANALYSIS COMPLETE (v2.0 - REFACTORED)\n")
cat(rep("=", 80), "\n\n", sep = "")

cat("RUNTIME SUMMARY:\n")
cat("----------------\n")
cat(sprintf("Total script runtime: %.1f minutes\n\n", as.numeric(script_runtime)))

cat("Section breakdown:\n")
cat("------------------\n")
for (section_name in names(section_times)) {
  cat(sprintf("%-50s: %6.1f minutes\n", section_name, section_times[[section_name]]))
}

cat("\n")
cat("CONFIGURATION:\n")
cat("--------------\n")
cat(sprintf("Treatment analysis: %s\n", TREATMENT_ANALYSIS))
cat(sprintf("Risk score method: %s\n", RISK_SCORE_METHOD))
cat(sprintf("Premium data: %s\n", ifelse(USE_PREMIUM_DATA, "ENABLED", "DISABLED")))
cat(sprintf("Claims data: %s\n", ifelse(USE_CLAIMS_DATA, "ENABLED", "DISABLED")))
cat(sprintf("Dewey assessment: %s\n", ifelse(ASSESS_DEWEY_TIMING, "ENABLED", "DISABLED")))
cat(sprintf("Time window: %d-%d\n", 1970, 2023))
cat(sprintf("Cohort cutoff: %d\n", 2000))

cat("\n")
cat("SUMMARY:\n")
cat("--------\n")
cat(sprintf("Total facilities: %s\n", 
            format(manifest$summary$n_facilities_total, big.mark = ",")))
cat(sprintf("Analysis sample facilities: %s\n", 
            format(manifest$summary$n_facilities_analysis, big.mark = ",")))
cat(sprintf("Incumbent sample facilities: %s\n", 
            format(manifest$summary$n_facilities_incumbent, big.mark = ",")))
cat(sprintf("Motor fuel sample facilities: %s\n", 
            format(manifest$summary$n_facilities_motor_fuel, big.mark = ",")))
cat(sprintf("Analysis sample observations: %s\n", 
            format(manifest$summary$n_observations_analysis, big.mark = ",")))
cat(sprintf("Date range: %s to %s\n", 
            manifest$summary$date_range[1], manifest$summary$date_range[2]))
cat("\n")

cat("KEY FINDINGS:\n")
cat("-------------\n")
cat(sprintf("Texas facilities: %s\n", 
            format(manifest$summary$key_findings$texas_facilities, big.mark = ",")))
cat(sprintf("Control facilities: %s\n", 
            format(manifest$summary$key_findings$control_facilities, big.mark = ",")))
cat(sprintf("Total leak events: %s\n", 
            format(manifest$summary$key_findings$total_leak_events, big.mark = ",")))
cat(sprintf("Total closure events: %s\n", 
            format(manifest$summary$key_findings$total_closure_events, big.mark = ",")))
cat(sprintf("Total exit events: %s\n", 
            format(manifest$summary$key_findings$total_exit_events, big.mark = ",")))
cat("\n")

cat("OUTPUTS CREATED:\n")
cat("----------------\n")
cat(sprintf("Tables: %d\n", manifest$summary$n_tables))
cat(sprintf("Main figures: %d\n", manifest$summary$n_figures_main))
cat("Captions: Exported for all tables and figures\n")
cat("\n")

cat("NEW IN v2.0:\n")
cat("------------\n")
cat("✓ Extended time window (1970-2023) for 30-year pre-trends\n")
cat("✓ Cohort analysis (Incumbent vs. Entrant)\n")
cat("✓ 1999 baseline standardization\n")
cat("✓ Premium/claims data integration\n")
cat("✓ Risk factor validation (regression-based)\n")
cat("✓ Capital vintage analysis\n")
cat("✓ LUST claims characterization\n")
cat("✓ Discrete choice timing diagnostics (Section 4.5)\n")
cat("✓ Dewey integration assessment matrix\n")
cat("\n")

cat("OUTPUT LOCATIONS:\n")
cat("-----------------\n")
cat("Tables: Output/Tables/\n")
cat("Figures (Academic): Output/Figures/\n")
cat("Figures (Slides): Output/Figures/Slides/\n")
cat("Captions: Output/Captions/\n")
cat("Manifest: Output/Manifests/\n")
cat("\n")

cat(rep("=", 80), "\n", sep = "")
cat(sprintf("Script completed at: %s\n", format(script_end_time, "%Y-%m-%d %H:%M:%S")))
cat(sprintf("Total runtime: %.1f minutes\n", as.numeric(script_runtime)))
cat(rep("=", 80), "\n", sep = "")

# Final memory cleanup
rm(list = setdiff(ls(), c("facility_leak_behavior", "analysis_sample", "incumbent_sample",
                          "motor_fuel_sample", "baseline_1999", "manifest")))
invisible(gc())

cat("\nScript execution complete. All outputs saved.\n")
cat("\n=== REFACTORED VERSION 2.0 ===\n")
# ==============================================================================
# REPLACEMENT: TEXAS UST FR MARKET — ENHANCED & ROBUST (drop-in)
# ==============================================================================
# Purpose:
#  - Replaces previous FR_ANALYSIS_SECTION with robust fallbacks, name-mapping,
#    tank-weighted provider summaries, fixed switching denominators, CIs,
#    survival (KM), and sensitivity checks.
# Usage:
#  - Paste this *after* your 00_SETUP preamble (it will reuse get_data_path/output_dir).
#  - It tolerates a missing coverage_gap column and missing helper functions.
# ==============================================================================

# Clear plotting objects (keeps main environment intact)
rm(list = ls(pattern = "^p_|^provider_|^annual_switch|^plot_switch_"), envir = globalenv())
gc()

# --- Ensure path variables exist (reuse your get_data_path/output_dir if present) ---
if (!exists("get_data_path")) {
  get_data_path <- function(...) {
    root <- Sys.getenv("UST_DATA_ROOT", unset = file.path(Sys.getenv("HOME"), "UST", "Data"))
    file.path(root, ...)
  }
}
if (!exists("output_dir")) output_dir <- get_data_path("Outputs")
if (!exists("fig_dir"))    fig_dir    <- file.path(output_dir, "figures")
if (!dir.exists(fig_dir)) dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

# --- Load required packages quietly ---
suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(scales)
  library(stringr)
  library(lubridate)
  # optional but used if present
  requireNamespace("gt", quietly = TRUE)
  requireNamespace("ggridges", quietly = TRUE)
  requireNamespace("survival", quietly = TRUE)
  requireNamespace("survminer", quietly = TRUE)
})

message("Starting FR market analysis (robust replacement)...")

# --- Helper: fallback theme if not defined in preamble ---
if (!exists("theme_fr_market")) {
  theme_fr_market <- function(base_size = 12, base_family = "sans") {
    theme_minimal(base_size = base_size, base_family = base_family) +
      theme(
        plot.title = element_text(face = "bold", hjust = 0),
        plot.subtitle = element_text(color = "gray30"),
        legend.position = "bottom",
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "gray95", color = NA)
      )
  }
}

# --- Helper: table/figure saving (use preamble functions if available) ---
# If save_figure exists in the global env (your preamble), prefer it
if (exists("save_figure")) {
  save_fr_figure <- function(plot_obj, filename_base, width = 10, height = 6) {
    save_figure(plot_obj, filename_base, width = width, height = height, create_slides = TRUE)
  }
} else {
  save_fr_figure <- function(plot_obj, filename_base, width = 10, height = 6) {
    png_path <- file.path(fig_dir, paste0(filename_base, ".png"))
    pdf_path <- file.path(fig_dir, paste0(filename_base, ".pdf"))
    ggplot2::ggsave(png_path, plot_obj, width = width, height = height, dpi = 300)
    ggplot2::ggsave(pdf_path, plot_obj, width = width, height = height)
  }
}

# Fallback table savers
if (exists("save_table")) {
  save_table_wrapper <- function(gt_obj, data_obj, filename_base) save_table(gt_obj, data_obj, filename_base)
} else {
  save_table_wrapper <- function(gt_obj, data_obj, filename_base) {
    csv_path <- file.path(output_dir, paste0(filename_base, ".csv"))
    data.table::fwrite(data_obj, csv_path)
    if (requireNamespace("gt", quietly = TRUE) && inherits(gt_obj, "gt_tbl")) {
      try(gt::gtsave(gt_obj, file.path(output_dir, paste0(filename_base, ".html"))), silent = TRUE)
    }
  }
}

# Fallback fr_table_format (uses gt if available)
fr_table_format <- function(df, title = NULL) {
  if (requireNamespace("gt", quietly = TRUE)) {
    out <- gt::gt(df)
    if (!is.null(title)) out <- out %>% gt::tab_header(title = title)
    return(out)
  } else {
    return(as.data.frame(df))
  }
}

# Fallback tracking
if (!exists("track_section_time")) {
  track_section_time <- function(section_name) message("Completed: ", section_name)
}

# --- Read panel if not already in memory (preserve user's earlier approach) ---
if (!exists("panel")) {
  message("Loading panel from disk...")
  panel_path <- file.path(output_dir, "texas_fr_panel.csv")
  if (!file.exists(panel_path)) stop("texas_fr_panel.csv not found at: ", panel_path)
  panel <- fread(panel_path)
}

# --- Column mapping & required/optional checks ---
# required columns we must have (allow common alternative names)
col_map <- list(
  FACILITY_ID = c("FACILITY_ID","facility_id","FacilityID"),
  YEAR = c("YEAR","year"),
  MONTH = c("MONTH","month"),
  ISSUER_NAME = c("ISSUER_NAME","issuer_name","Issuer"),
  CATEGORY = c("CATEGORY","category"),
  ACTIVE_TANK_COUNT = c("active_tank_count","ACTIVE_TANK_COUNT","tank_count","n_tanks"),
  AVG_TANK_AGE_MONTHS = c("avg_tank_age_months","AVG_TANK_AGE_MONTHS","tank_age_months"),
  PROP_SINGLE_WALL = c("prop_single_wall","PROP_SINGLE_WALL","pct_single_wall"),
  PROP_DOUBLE_WALL = c("prop_double_wall","PROP_DOUBLE_WALL","pct_double_wall")
)

# optional columns (create safe defaults if missing)
optional_cols <- list(
  COVERAGE_GAP = c("coverage_gap_month","coverage_gap","COVERAGE_GAP_MONTH")
)

# map required
for (std in names(col_map)) {
  found <- intersect(col_map[[std]], names(panel))
  if (length(found) == 0) {
    stop(sprintf("Required column for '%s' not found in panel. Checked: %s", std, paste(col_map[[std]], collapse = ", ")))
  } else if (!(std %in% names(panel))) {
    setnames(panel, found[1], std)
  }
}

# map optional - if missing create FALSE
for (opt in names(optional_cols)) {
  found <- intersect(optional_cols[[opt]], names(panel))
  if (length(found) == 0) {
    # create safe default
    panel[, (opt) := FALSE]
    warning(sprintf("Optional column '%s' not found: created default FALSE column.", opt))
  } else if (!(opt %in% names(panel))) {
    setnames(panel, found[1], opt)
  }
}

# ensure date and ordering
if (!("date" %in% names(panel))) panel[, date := as.Date(sprintf("%d-%02d-01", YEAR, MONTH))]
setkey(panel, FACILITY_ID, date)

# --- Recompute FR_Type using your earlier logic (consistent) ---
oil_majors <- "EXXON|MOBIL|CHEVRON|TEXACO|SHELL|PHILLIPS|CONOCO|VALERO|SUNOCO|CITGO|MOTIVA"
panel[is.na(ISSUER_NAME) | ISSUER_NAME == "", ISSUER_NAME := "NO COVERAGE"]
panel[is.na(CATEGORY), CATEGORY := "No Coverage"]

panel[, FR_Type := fcase(
  CATEGORY == "State Fund" | ISSUER_NAME == "State Fund", "State Fund",
  CATEGORY == "NO COVERAGE" | COVERAGE_GAP == TRUE, "Uninsured/Gap",
  str_detect(toupper(CATEGORY), "INSURANCE"), "Private Insurance",
  (str_detect(toupper(CATEGORY), "SELF|GUARANTEE|TEST") & str_detect(toupper(ISSUER_NAME), oil_majors)), "Self-Insured (Oil Major)",
  str_detect(toupper(CATEGORY), "SELF|GUARANTEE|TEST"), "Self-Insured (Other)",
  default = "Other"
)]

# ------------------
# 3. 2023 snapshot and provider-level summaries (tank-weighted)
# ------------------
message("Computing 2023 cross-section and provider-level summaries (tank-weighted)...")
active_2023 <- panel[YEAR == 2023 & ACTIVE_TANK_COUNT > 0]
if (nrow(active_2023) == 0) stop("No active 2023 observations. Check panel filters.")

total_tanks_2023 <- active_2023[, sum(ACTIVE_TANK_COUNT, na.rm = TRUE)]

weighted_mean_safe <- function(x, w) {
  if (all(is.na(x))) return(NA_real_)
  if (sum(w, na.rm = TRUE) == 0) return(mean(x, na.rm = TRUE))
  w[is.na(w)] <- 0
  stats::weighted.mean(x, w, na.rm = TRUE)
}
weighted_quantile <- function(x, w, probs = c(0.25, 0.5, 0.75)) {
  ok <- !is.na(x) & !is.na(w) & w > 0
  if (sum(ok) == 0) return(rep(NA_real_, length(probs)))
  xo <- x[ok]; wo <- w[ok]; ord <- order(xo); xo <- xo[ord]; wo <- wo[ord]
  csum <- cumsum(wo)/sum(wo)
  sapply(probs, function(p) xo[which(csum >= p)[1]])
}

provider_summary <- active_2023[, .(
  n_facilities = .N,
  n_tanks = sum(ACTIVE_TANK_COUNT, na.rm = TRUE),
  tanks_share = sum(ACTIVE_TANK_COUNT, na.rm = TRUE) / total_tanks_2023,
  avg_age_mo = weighted_mean_safe(AVG_TANK_AGE_MONTHS, ACTIVE_TANK_COUNT),
  med_age_mo = as.numeric(weighted_quantile(AVG_TANK_AGE_MONTHS, ACTIVE_TANK_COUNT, probs = 0.5)),
  age_q1_mo = as.numeric(weighted_quantile(AVG_TANK_AGE_MONTHS, ACTIVE_TANK_COUNT, probs = 0.25)),
  age_q3_mo = as.numeric(weighted_quantile(AVG_TANK_AGE_MONTHS, ACTIVE_TANK_COUNT, probs = 0.75)),
  prop_sw = weighted_mean_safe(PROP_SINGLE_WALL, ACTIVE_TANK_COUNT),
  prop_dw = weighted_mean_safe(PROP_DOUBLE_WALL, ACTIVE_TANK_COUNT)
), by = .(FR_Type, ISSUER_NAME)]

setorder(provider_summary, -n_tanks)
fwrite(provider_summary, file.path(output_dir, "T_provider_summary_2023_weighted.csv"))

# Top 10 private insurers (tank-weighted)
top10_insurers <- provider_summary[FR_Type == "Private Insurance"][1:10]
paper_top10 <- top10_insurers[, .(ISSUER_NAME, n_tanks, tanks_share_pct = round(tanks_share*100,2),
                                  avg_age_yrs = round(avg_age_mo/12,1), prop_single_walled_pct = round(prop_sw*100,1))]

# Save top10 (CSV + optional GT)
gt_top10 <- fr_table_format(paper_top10, "Top 10 Private Insurers (2023) — Tank-weighted")
save_table_wrapper(gt_top10, paper_top10, "T_paper_top10_insurers_2023")

# ------------------
# 4. FR instrument composition & HHI
# ------------------
fr_composition <- provider_summary[, .(total_tanks_by_fr = sum(n_tanks)), by = FR_Type]
fr_composition[, fr_share := total_tanks_by_fr / sum(total_tanks_by_fr)]
fwrite(fr_composition, file.path(output_dir, "T_fr_instrument_share_2023.csv"))

hhi_by_fr <- provider_summary[, .(n_tanks = sum(n_tanks)), by = .(FR_Type, ISSUER_NAME)]
hhi_by_fr <- merge(hhi_by_fr, fr_composition[, .(FR_Type, total_tanks_by_fr)], by = "FR_Type")
hhi_by_fr[, share_within_fr := n_tanks / total_tanks_by_fr]
hhi_summary <- hhi_by_fr[, .(HHI = sum(share_within_fr^2)), by = FR_Type]
hhi_summary[, HHI_10000 := HHI * 10000]
fwrite(hhi_summary, file.path(output_dir, "T_hhi_by_fr_2023.csv"))

# ------------------
# 5. Figures: Age, Build, FR shares, Top insurers
# ------------------
message("Generating figures...")

# Age figure (top10 + self-insured groups)
plot_providers <- unique(c(top10_insurers$ISSUER_NAME, "Self-Insured (Oil Major)", "Self-Insured (Other)"))
age_plot_dt <- provider_summary[ISSUER_NAME %in% plot_providers | FR_Type %in% c("Self-Insured (Oil Major)", "Self-Insured (Other)")]
age_plot_dt[, `:=`(avg_age_yrs = avg_age_mo/12, q1_yrs = age_q1_mo/12, q3_yrs = age_q3_mo/12)]
age_plot_dt[, Plot_Label := ISSUER_NAME]

p_age <- ggplot(age_plot_dt, aes(x = reorder(Plot_Label, avg_age_yrs), y = avg_age_yrs, fill = avg_age_yrs)) +
  geom_col() + geom_errorbar(aes(ymin = q1_yrs, ymax = q3_yrs), width = 0.4, color = "gray20") +
  coord_flip() + scale_fill_viridis_c(option = "magma", direction = -1) +
  labs(title = "Risk Profile: Average Tank Age by FR Provider (2023)",
       subtitle = "Tank-weighted; bars = mean age (years); whiskers = weighted IQR",
       x = NULL, y = "Average Tank Age (years)") +
  theme_fr_market()
save_fr_figure(p_age, "F08_avg_tank_age_by_provider_weighted", width = 10, height = 7)

# Build composition stacked (providers)
build_stats <- age_plot_dt[, .(Plot_Label = Plot_Label, avg_prop_sw = prop_sw, avg_prop_dw = prop_dw, n_tanks = n_tanks)]
build_long <- melt(build_stats, id.vars = c("Plot_Label","n_tanks"), measure.vars = c("avg_prop_sw","avg_prop_dw"),
                   variable.name = "Tank_Type", value.name = "Share")
build_long[, Tank_Label := ifelse(Tank_Type == "avg_prop_sw", "Single-Walled (Higher Risk)", "Double-Walled (Lower Risk)")]
p_build <- ggplot(build_long, aes(x = reorder(Plot_Label, -n_tanks), y = Share, fill = Tank_Label)) +
  geom_col(position = "fill") + coord_flip() + scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_fill_manual(values = c("Single-Walled (Higher Risk)" = "#d73027", "Double-Walled (Lower Risk)" = "#4575b4")) +
  labs(title = "Portfolio Quality: Tank Build Composition (2023)",
       subtitle = "Tank-weighted share of Single-Walled vs Double-Walled",
       x = NULL, y = "Share of Provider's Tanks") + theme_fr_market()
save_fr_figure(p_build, "F09_tank_build_composition_weighted", width = 10, height = 7)

# FR instrument bar
p_fr_bar <- ggplot(fr_composition, aes(x = reorder(FR_Type, fr_share), y = fr_share, fill = FR_Type)) +
  geom_col(show.legend = FALSE) + coord_flip() + scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(title = "Market Composition by FR Instrument (2023)", y = "Share of Tanks", x = NULL) + theme_fr_market()
save_fr_figure(p_fr_bar, "F07_fr_instrument_composition_bar_2023", width = 9, height = 5)

# Top private insurers by share
top20 <- provider_summary[FR_Type == "Private Insurance"][1:20]
p_top_share <- ggplot(top20, aes(x = reorder(ISSUER_NAME, tanks_share), y = tanks_share, fill = tanks_share)) +
  geom_col(show.legend = FALSE) + coord_flip() + scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
  scale_fill_viridis_c() + labs(title = "Top Private Insurers by Tank Share (2023)", y = "Share of Tanks", x = NULL) + theme_fr_market()
save_fr_figure(p_top_share, "F06_top_private_insurers_by_share_2023", width = 10, height = 7)

# ------------------
# 6. Switching metrics (eligible-pair approach + CIs)
# ------------------
message("Calculating switching metrics (eligible-pair denominators + CIs)...")
setkey(panel, FACILITY_ID, date)
panel[, `:=`(prev_FR_Type = shift(FR_Type, 1L, type = "lag"),
             prev_ISSUER  = shift(ISSUER_NAME, 1L, type = "lag"),
             prev_date    = shift(date, 1L, type = "lag")),
      by = FACILITY_ID]

# Eligible transition: previous obs exists and within 62 days
panel[, eligible_transition := !is.na(prev_date) & (as.numeric(difftime(date, prev_date, units = "days")) <= 62)]
transitions <- panel[eligible_transition == TRUE]

# Event flags (compute sequentially, avoid referencing new columns inside same :=)
transitions[, switch_mechanism := as.integer(FR_Type != prev_FR_Type)]
transitions[, insured_pair := as.integer(prev_FR_Type == "Private Insurance" & FR_Type == "Private Insurance")]
transitions[, switch_insurer := as.integer(insured_pair == 1 & ISSUER_NAME != prev_ISSUER)]

# Annual aggregates
annual_mech <- transitions[, .(eligible_pairs = .N, n_switch_mech = sum(switch_mechanism, na.rm = TRUE)), by = YEAR]
annual_mech[, rate_mech := n_switch_mech / pmax(eligible_pairs, 1)]
annual_mech[, se_mech := sqrt(rate_mech * (1 - rate_mech) / pmax(eligible_pairs, 1))]
annual_mech[, `:=`(mech_lo = pmax(0, rate_mech - 1.96 * se_mech), mech_hi = pmin(1, rate_mech + 1.96 * se_mech))]

annual_insurer <- transitions[insured_pair == 1, .(insured_pairs = .N, n_switch_insurer = sum(switch_insurer, na.rm = TRUE)), by = YEAR]
all_years <- sort(unique(transitions$YEAR))
annual_insurer <- merge(data.table(YEAR = all_years), annual_insurer, by = "YEAR", all.x = TRUE)
annual_insurer[is.na(insured_pairs), `:=`(insured_pairs = 0, n_switch_insurer = 0)]
annual_insurer[, rate_insurer := ifelse(insured_pairs > 0, n_switch_insurer / insured_pairs, NA_real_)]
annual_insurer[, se_ins := ifelse(insured_pairs > 0, sqrt(rate_insurer * (1 - rate_insurer) / insured_pairs), NA_real_)]
annual_insurer[, `:=`(ins_lo = fifelse(is.na(se_ins), NA_real_, pmax(0, rate_insurer - 1.96 * se_ins)),
                      ins_hi = fifelse(is.na(se_ins), NA_real_, pmin(1, rate_insurer + 1.96 * se_ins)))]

# Combine and save
annual_switching <- merge(annual_mech[, .(YEAR, eligible_pairs, n_switch_mech, rate_mech, mech_lo, mech_hi)],
                          annual_insurer[, .(YEAR, insured_pairs, n_switch_insurer, rate_insurer, ins_lo, ins_hi)],
                          by = "YEAR", all = TRUE)
fwrite(annual_switching, file.path(output_dir, "T_annual_switching_eligible_pairs.csv"))

# Plot switching time series with ribbons (two series)
mech_plot_dt <- annual_mech[, .(YEAR, Rate = rate_mech, lo = mech_lo, hi = mech_hi, n_pairs = eligible_pairs, Metric = "Mechanism Switching (Between FR Types)")]
ins_plot_dt  <- annual_insurer[, .(YEAR, Rate = rate_insurer, lo = ins_lo, hi = ins_hi, n_pairs = insured_pairs, Metric = "Insurer Churn (Within Insurance Market)")]
plot_switch_long <- rbindlist(list(mech_plot_dt, ins_plot_dt), fill = TRUE)

ymax <- max(plot_switch_long$hi, na.rm = TRUE)
p_switch <- ggplot(plot_switch_long, aes(x = YEAR, y = Rate, color = Metric, fill = Metric)) +
  geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.15, color = NA) +
  geom_line(size = 1.1) + geom_point(size = 2) +
  geom_text(data = unique(plot_switch_long[, .(YEAR, Metric, n_pairs)]), aes(label = paste0("N=", n_pairs)), y = -0.02, size = 2.5, inherit.aes = FALSE) +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, ymax * 1.08)) +
  labs(title = "Market Friction: Annual Switching Rates (2000-2023)", subtitle = "Shaded bands = 95% binomial CIs; annotated N = eligible transitions", y = "Annual Switching Rate", x = "Year") +
  theme_fr_market()
save_fr_figure(p_switch, "F10_switching_rates_eligible_pairs_ci", width = 11, height = 6)

# ------------------
# 7. Survival analysis: time-to-first-mechanism-switch (Kaplan–Meier)
# ------------------
message("Running survival (KM) time-to-first-mechanism-switch...")
facility_spells <- panel[order(FACILITY_ID, date), .(
  first_date = min(date, na.rm = TRUE),
  first_fr = FR_Type[which.min(date)],
  last_date = max(date, na.rm = TRUE),
  switch_date = { idx <- which(FR_Type != FR_Type[1]); if (length(idx) == 0) as.Date(NA) else date[min(idx)] }
), by = FACILITY_ID]

facility_spells[, `:=`(
  event = as.integer(!is.na(switch_date)),
  end_date = fifelse(!is.na(switch_date), switch_date, last_date),
  time_days = as.numeric(difftime(end_date, first_date, units = "days")),
  time_months = time_days / 30.4375
)]

# drop negative / NA
km_dt <- facility_spells[!is.na(time_months) & time_months >= 0]

if (nrow(km_dt) > 0 && requireNamespace("survival", quietly = TRUE)) {
  surv_obj <- survival::Surv(time = km_dt$time_months, event = km_dt$event)
  fit_km <- survival::survfit(surv_obj ~ first_fr, data = km_dt)
  if (requireNamespace("survminer", quietly = TRUE)) {
    p_km <- survminer::ggsurvplot(fit_km, data = km_dt, risk.table = TRUE, pval = TRUE, conf.int = TRUE, ggtheme = theme_fr_market())$plot
  } else {
    p_km <- autoplot(fit_km) + theme_fr_market()
  }
  save_fr_figure(p_km, "F12_km_time_to_first_mechanism_switch_by_baseline_fr", width = 10, height = 7)
  fwrite(km_dt, file.path(output_dir, "T_facility_spell_time_to_switch.csv"))
} else {
  message("KM analysis skipped (no valid spells or survival packages missing).")
}

# ------------------
# 8. Age distribution (density ridges or box fallback)
# ------------------
active_2023[, age_years := AVG_TANK_AGE_MONTHS / 12]
if (requireNamespace("ggridges", quietly = TRUE)) {
  p_age_dist <- ggplot(active_2023[FR_Type %in% c("Private Insurance","Self-Insured (Oil Major)","Self-Insured (Other)","State Fund")],
                       aes(x = age_years, y = FR_Type, weight = ACTIVE_TANK_COUNT, fill = FR_Type)) +
    ggridges::geom_density_ridges(scale = 1, rel_min_height = 0.01, alpha = 0.8) +
    theme_fr_market() + labs(title = "Weighted Distribution of Facility Average Tank Age (2023)", x = "Age (years)", y = NULL)
} else {
  p_age_dist <- ggplot(active_2023, aes(x = FR_Type, y = age_years, weight = ACTIVE_TANK_COUNT, fill = FR_Type)) +
    geom_boxplot() + coord_flip() + theme_fr_market() + labs(title = "Age distribution (boxplot fallback)")
}
save_fr_figure(p_age_dist, "F11_age_distribution_by_fr_type_2023", width = 10, height = 6)

# ------------------
# 9. Sensitivity: eligibility windows (30 / 62 / 120 days)
# ------------------
message("Running sensitivity for eligibility windows (30/62/120 days)...")
elig_windows <- c(30, 62, 120)
sens_list <- list()
for (w in elig_windows) {
  colw <- paste0("eligible_w_", w)
  panel[, (colw) := (!is.na(prev_date) & (as.numeric(difftime(date, prev_date, units = "days")) <= w))]
  trans_w <- panel[get(colw) == TRUE]
  annual_w <- trans_w[, .(eligible_pairs = .N, n_switch_mech = sum(FR_Type != prev_FR_Type, na.rm = TRUE)), by = YEAR]
  annual_w[, rate := n_switch_mech / pmax(eligible_pairs, 1)]
  annual_w[, window := w]
  sens_list[[as.character(w)]] <- annual_w[, .(YEAR, window, rate)]
}
sens_dt <- rbindlist(sens_list)
fwrite(sens_dt, file.path(output_dir, "T_switching_sensitivity_windows.csv"))

p_sens <- ggplot(sens_dt[YEAR >= 2000 & YEAR <= 2023], aes(x = YEAR, y = rate, color = factor(window))) +
  geom_line(size = 1) + theme_fr_market() + labs(title = "Sensitivity: Mechanism Switch Rate by Eligibility Window", y = "Mechanism Switch Rate", x = "Year", color = "Window (days)")
save_fr_figure(p_sens, "F10b_switching_sensitivity_windows", width = 10, height = 6)

# ------------------
# 10. Save summary switching table for selected years & finish
# ------------------
switching_summary <- merge(
  annual_switching[, .(YEAR, eligible_pairs, n_switch_mech, rate_mech = rate_mech)],
  annual_switching[, .(YEAR, insured_pairs, n_switch_insurer, rate_insurer = rate_insurer)],
  by = "YEAR", all = TRUE
)
# pick anchor years for the short table
short_years <- c(2005, 2010, 2015, 2020, 2023)
short_table <- annual_switching[YEAR %in% short_years, .(YEAR, eligible_pairs, n_switch_mech, rate_mech = rate_mech, insured_pairs, n_switch_insurer, rate_insurer)]
# format percent columns for human CSV
short_table_print <- copy(short_table)
short_table_print[, `Mech Switch Rate (%)` := round(rate_mech * 100, 2)]
short_table_print[, `Insurer Churn Rate (%)` := round(rate_insurer * 100, 2)]
fwrite(short_table_print, file.path(output_dir, "T05_switching_summary.csv"))

track_section_time("FR market analysis (robust)")
message("Done. Figures saved to: ", fig_dir, " and tables saved to: ", output_dir)
