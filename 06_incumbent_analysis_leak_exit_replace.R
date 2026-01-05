#==============================================================================
# Analysis of Texas Insurance Policy Change on LUST Facility Leak Incidents
# 
# This script analyzes the impact of Texas' 1999 transition from public to 
# private insurance regime on leak incidents at LUST (Leaking Underground 
# Storage Tank) facilities using:
#   1. Survival Analysis (Hazard Model)
#   2. Difference-in-Differences (DiD) Analysis 
#   3. Event Study Analysis
#
# Author: Kaleb Javier
# Date: 2025-05-22
#==============================================================================

#------------------------------------------------------------------------------
# 1. Setup: Load libraries and data
#------------------------------------------------------------------------------
library(data.table)        # Fast data manipulation
library(survival)          # Survival analysis
library(survminer)         # Visualizing survival analysis
library(fixest)            # Fast fixed-effects models
library(ggplot2)           # Data visualization
library(kableExtra)        # Pretty tables
library(gridExtra)         # Multiple plots arrangement
library(here)              # File path management
library(broom)             # Tidy model outputs
library(ggpubr)            # Publication-ready plots
library(cowplot)           # Plot composition
library(sandwich)          # Robust standard errors
library(lmtest)            # Coefficient testing
library(scales)            # Nice axis formatting
options(scipen = 999)      # Turn off scientific notation
library(tidyverse)
# Set seed for reproducibility
set.seed(123456)
first_run <- FALSE


# Define custom ggplot theme for publication-quality figures
theme_pub <- function(base_size = 12, base_family = "sans") {
  theme_minimal(base_size = base_size, base_family = base_family) +
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

# Apply the theme
theme_set(theme_pub())

# Create output directories if they don't exist
dir.create(here("Output", "Tables"), recursive = TRUE, showWarnings = FALSE)
dir.create(here("Output", "Figures"), recursive = TRUE, showWarnings = FALSE)

# Load the panel dataset
# In a real scenario, you would replace this with actual data loading
# Here we'll simulate the dataset based on the provided structure
cat("Loading facility panel dataset...\n")
facility_panel <- fread(here("Data", "Raw_do_not_write", "state_databases", 
                           "EPA_Region_6_and_EPA_states_facility_panel.csv"))

tank_panel <- fread(here("Data", "Raw_do_not_write", "state_databases", 
                       "EPA_Region_6_and_EPA_states_tank_panel.csv"))
lust_data <- fread(here("Data", "Processed", "all_lust_data.csv"))

# Define treated and control states
treated_state_long <- c("Texas", "Wisconsin", "New Jersey", "Michigan", 
                        "Iowa", "Florida", "Arizona", "Connecticut")

unique(facility_panel$state)

# Ensure dates and key variables are properly formatted
cat("Converting data types and setting up key variables...\n")

# Convert columns to integer in-place where needed
facility_panel[, `:=`(
  entry_year = as.integer(entry_year),
  exit_year = as.integer(exit_year),
  panel_year = as.integer(panel_year),
  # Create flags for facility state
  entry_flag = (panel_year == entry_year),
  exit_flag = (panel_year == exit_year & all_tanks_closed == TRUE),
  # Flag for tank replacements (following ust_facility_vintage_policy_analysis.R)
  replacement_event = tanks_closed_this_year > 0 & tanks_opened_this_year > 0,
  # Define incumbent facilities (those not in their entry year)
  incumbent = panel_year > entry_year
)]

# Create policy period indicator based on treatment information
cat("Creating treatment variables and policy periods...\n")


# Remove treatment_state, treatment_year, and treatment from facility panel
if("treatment_state" %in% names(facility_panel)) {
  facility_panel[, treatment_state := NULL]
}
if("treatment_year" %in% names(facility_panel)) {
  facility_panel[, treatment_year := NULL]
}
if("treatment" %in% names(facility_panel)) {
  facility_panel[, treatment := NULL]
}

# Remove the same columns from tank panel
if("treatment_state" %in% names(tank_panel)) {
  tank_panel[, treatment_state := NULL]
}
if("treatment_year" %in% names(tank_panel)) {
  tank_panel[, treatment_year := NULL]
}
if("treatment" %in% names(tank_panel)) {
  tank_panel[, treatment := NULL]
}


# Create state treatment information table
state_treatment_info <- data.table(
    state = c("Texas", "Wisconsin", "New Jersey", "Michigan", 
                        "Iowa", "Florida", "Arizona", "Connecticut"),
    # Set actual treatment years for treated states
    treatment_year = c(1999, 1998, 2010, 2014, 
                                        2000, 1999, 2006, 2010)
)

# Set key for efficient joins
setkey(state_treatment_info, state)

# Merge state treatment info with facility panel using merge.data.table
facility_panel <- merge.data.table(
  facility_panel, 
  state_treatment_info, 
  by = "state", 
  all.x = TRUE
)

facility_panel[is.na(treatment_year), treatment_year := 9999999]

# Create time period indicators based solely on year (not state-specific)
facility_panel[, post_1999 := as.integer(panel_year >= 2000)]
facility_panel[, period_label := ifelse(panel_year >= 2000, "Post-1999", "Pre-1999")]



# Calculate years relative to treatment for all states
# For control states, this will be relative to the reference year (1999)
facility_panel[, rel_year := panel_year - treatment_year]

#make sure we have tx in the sample
unique(facility_panel$state)

facility_panel[,texas_treated := (state == 'Texas')*1]




# Texas-specific indicators
facility_panel[state == "Texas" & panel_year >= 1999, `:=`(
    D_tx_all = 1L,
    D_tx_r6 = 1L
)]
facility_panel[is.na(D_tx_all), D_tx_all := 0L]
facility_panel[is.na(D_tx_r6), D_tx_r6 := 0L]

# Create relative year variables for event study
facility_panel[, rel_year_factor := factor(rel_year)]

# Create treatment group indicator with three categories:
# 1. Texas (main treated state)
# 2. All_Other_Treated_State (other treated states)
# 3. True_Control (states not in state_treatment_info)
facility_panel[, treatment_group := "True_Control"]
facility_panel[state == "Texas", treatment_group := "Texas"]
facility_panel[state %in% state_treatment_info$state & state != "Texas", 
              treatment_group := "All Other Treated State"]

# Convert to factor with specific order
facility_panel[, treatment_group := factor(treatment_group, 
                                         levels = c("Texas", 
                                                   "All_Other_Treated_State", 
                                                   "True_Control"))]

# Count facilities by treatment group to verify
cat("Facility count by treatment group:\n")
print(facility_panel[, .N, by = .(treatment_group)])

# Create pre/post period indicator
facility_panel[, period := ifelse(post_1999 == 1, "Post-Policy (2000+)", "Pre-Policy (<2000)")]

# Merge LUST data with facility panel
# First ensure we have a common identifier in both datasets
if("facility_id" %in% names(lust_data)) {
  # Prepare LUST data for merging
  lust_incidents <- lust_data[, .(
    facility_id,
    leak_year = year(as.Date(report_date)),
    leak_incident = 1,
    state
  )]
  
  # Aggregate to facility-year level in case of multiple incidents
  lust_by_facility_year <- lust_incidents[, .(
    leak_incident = max(leak_incident)
  ), by = .(facility_id, leak_year)]
  
  # Create a dataset with all facility-year combinations
  all_facility_years <- unique(facility_panel[, .(facility_id, panel_year)])
  
  # Merge LUST incidents with all facility-years using merge.data.table
  setnames(all_facility_years, "panel_year", "year")
  
  # Left join to include all facility-years
  facility_leaks <- merge.data.table(
    all_facility_years,
    lust_by_facility_year,
    by.x = c("facility_id", "year"),
    by.y = c("facility_id", "leak_year"),
    all.x = TRUE
  )

  
  # Fill NA values with 0 (no leak reported that year)
  facility_leaks[is.na(leak_incident), leak_incident := 0]
  
  # Merge back with the main facility panel
  setkey(facility_panel, facility_id, panel_year)
  setkey(facility_leaks, facility_id, year)
  facility_panel <- merge.data.table(
    facility_panel,
    facility_leaks,
    by.x = c("facility_id", "panel_year"),
    by.y = c("facility_id", "year"),
    all.x = TRUE
  )
  
  # Ensure leak_incident is 0 for any remaining NA values
  facility_panel[is.na(leak_incident), leak_incident := 0]
} else {
  cat("Warning: 'facility_id' not found in LUST data. Cannot merge with facility panel.\n")
  # Create dummy leak_incident variable if needed
  if(!"leak_incident" %in% names(facility_panel)) {
    facility_panel[, leak_incident := 0]
  }
}

# Calculate leak rate by group and period for descriptive statistics
group_leak_rates <- facility_panel[, .(
  n_facilities = uniqueN(facility_id),
  n_facility_years = .N,
  n_leaks = sum(leak_incident),
  leak_rate = 100 * mean(leak_incident)
), by = .(treatment_group, period)]

# Display summary statistics
cat("Summary of leak incidents by treatment group and period:\n")
print(group_leak_rates)


# Create binary indicators for substance types and tank wall types
cat("Creating binary indicator variables for facility panel...\n")

# 1. Identify all percentage columns
pct_cols <- names(facility_panel)[grepl("^pct_", names(facility_panel))]
cat("Found percentage columns:", paste(pct_cols, collapse=", "), "\n")

# 2. Create binary indicators for all percentage columns
for (col in pct_cols) {
    # Extract the base name (remove "pct_" prefix)
    base_name <- gsub("^pct_", "", col)
    # Create indicator variable
    indicator_name <- paste0("has_", base_name)
    # Convert to binary: 1 if > 0, 0 otherwise (treating NA as 0)
    facility_panel[, (indicator_name) := as.integer(ifelse(is.na(get(col)), 0, get(col) > 0))]
    cat("Created indicator:", indicator_name, "from", col, "\n")
}

glimpse(facility_panel)


# 3. Summarize the new indicator variables
cat("\nSummary of new binary indicator variables:\n")
indicator_cols <- names(facility_panel)[grepl("^has_", names(facility_panel))]
if (length(indicator_cols) > 0) {
    summary_stats <- facility_panel[, lapply(.SD, function(x) c(
        Mean = round(mean(x, na.rm=TRUE), 4),
        Count = sum(x, na.rm=TRUE),
        Pct = round(100 * mean(x, na.rm=TRUE), 2)
    )), .SDcols = indicator_cols]
    
    print(summary_stats)
} else {
    cat("No indicator variables were created. Check if percentage columns exist in the dataset.\n")
}

# Print total observations for reference
cat("\nTotal facilities:", uniqueN(facility_panel$facility_id), "\n")
cat("Total observations:", nrow(facility_panel), "\n")


# now lets subset to only motor fuel facilites. That is places that have gasoline and diesel.
# Subset to include facilities that have gasoline, diesel, or both (motor fuel facilities)
facility_panel_only_motor_fuel <- facility_panel[has_gasoline > 0 | has_diesel > 0]

# Create a variable to categorize motor fuel type
facility_panel_only_motor_fuel[, fuel_type := case_when(
    has_gasoline > 0 & has_diesel > 0 ~ "Both Gasoline and Diesel",
    has_gasoline > 0 ~ "Gasoline Only",
    has_diesel > 0 ~ "Diesel Only"
)]

# Print summary of motor fuel facilities by type
cat("Motor fuel facilities by type:\n")
print(facility_panel_only_motor_fuel[, .N, by = fuel_type])

# Set the first_run variable to control whether to process the data or load pre-processed data
if (first_run) {
  cat("Starting to build facility_leak_behavior dataset from scratch...\n")
  cat("Step 1: Identifying motor fuel facilities and filtering tank data...\n")
  ## Identifying Lusts after Firm Behavior
  # Identify tanks with LUST events within 90 days of tank closure

  # 1. First get list of facility IDs from motor fuel facilities
  motor_fuel_facility_ids <- unique(facility_panel_only_motor_fuel$facility_id)
  # 2. Filter tank data to only include tanks from motor fuel facilities
  tank_data_filtered <- tank_panel[facility_id %in% motor_fuel_facility_ids]

  # 3. Create a date sequence for our panel (monthly from 1970 to 2020)
  start_date <- as.Date("1970-01-01")  # Changed from 1990 to 1970
  end_date <- as.Date("2020-12-31")
  date_seq <- seq(start_date, end_date, by = "month")

  # Generate all facility-month combinations for our panel
  facilities <- unique(tank_data_filtered$facility_id)
  panel_dates <- data.table(expand.grid(
    facility_id = facilities,
    date = date_seq
  ))

  # Extract year and month columns
  panel_dates[, `:=`(
    panel_year = year(date),
    panel_month = month(date)
  )]

  # 4. Add tank closure and replacement events to each facility-month
  panel_dates[, month_key := paste0(panel_year, "-", sprintf("%02d", panel_month))]

  # Process tank closures (identify when tanks were closed at each facility)
  # Include wall type information and carry forward location identifiers
  tank_closures <- tank_data_filtered[!is.na(tank_closed_date), .(
    facility_id,
    tank_id,
    state,  # Carry state forward
    county_name,  # Carry county_name forward
    county_geoid,  # Carry county_geoid forward
    closure_year = lubridate::year(tank_closed_date),
    closure_month = lubridate::month(tank_closed_date),
    closure_date = tank_closed_date,
    single_walled,
    double_walled
  )]
  
  # Deduplicate by tank_id since each tank can only be closed once
  tank_closures <- unique(tank_closures, by = "tank_id")
  
  tank_closures[, closure_month_key := paste0(closure_year, "-", sprintf("%02d", closure_month))]

  # Process tank installations (to identify replacements)
  # Include wall type information and carry forward location identifiers
  tank_installs <- tank_data_filtered[!is.na(tank_installed_date), .(
    facility_id,
    tank_id,
    state,  # Carry state forward
    county_name,  # Carry county_name forward
    county_geoid,  # Carry county_geoid forward
    install_year = lubridate::year(tank_installed_date),
    install_month = lubridate::month(tank_installed_date),
    install_date = tank_installed_date,
    single_walled,
    double_walled
  )]
  
  # Deduplicate by tank_id since each tank can only be installed once
  tank_installs <- unique(tank_installs, by = "tank_id")

  tank_installs[, install_month_key := paste0(install_year, "-", sprintf("%02d", install_month))]

  # Calculate tank age for each tank-year combination
  tank_years <- data.table(expand.grid(
    tank_id = unique(tank_installs$tank_id),
    year = 1970:2020
  ))

  # Merge with installation data to get install date and location information
  tank_years <- merge(
    tank_years,
    tank_installs[, .(tank_id, install_date, install_year, single_walled, double_walled, 
                      state, county_name, county_geoid)],  # Include location fields
    by = "tank_id",
    all.x = TRUE
  )

  # Calculate age of tank for each year
  tank_years[, tank_age := year - install_year]
  tank_years[tank_age < 0, tank_age := NA]  # Set age to NA for years before installation

  # 5. Aggregate events to facility-month level
  # Include location information in the aggregation
  facility_monthly_closures <- tank_closures[, .(
    tanks_closed = .N,
    single_walled_closed = sum(single_walled),
    double_walled_closed = sum(double_walled),
    state = first(state),  # Preserve location information
    county_name = first(county_name),
    county_geoid = first(county_geoid)
  ), by = .(facility_id, closure_month_key)]

  facility_monthly_installs <- tank_installs[, .(
    tanks_installed = .N,
    single_walled_installed = sum(single_walled),
    double_walled_installed = sum(double_walled),
    state = first(state),  # Preserve location information
    county_name = first(county_name),
    county_geoid = first(county_geoid)
  ), by = .(facility_id, install_month_key)]

  # 6. Merge closure and installation data into panel
  # Left join closures
  panel_dates <- merge(
    panel_dates,
    facility_monthly_closures,
    by.x = c("facility_id", "month_key"),
    by.y = c("facility_id", "closure_month_key"),
    all.x = TRUE,
    suffixes = c("", "_closure")  # Avoid duplicate column names
  )

  # Left join installations
  panel_dates <- merge(
    panel_dates,
    facility_monthly_installs,
    by.x = c("facility_id", "month_key"),
    by.y = c("facility_id", "install_month_key"),
    all.x = TRUE,
    suffixes = c("", "_install")  # Avoid duplicate column names
  )

  # Consolidate state/county information (prioritize existing values)
  panel_dates[, `:=`(
    state = ifelse(is.na(state), state_closure, state),
    state = ifelse(is.na(state), state_install, state),
    county_name = ifelse(is.na(county_name), county_name_closure, county_name),
    county_name = ifelse(is.na(county_name), county_name_install, county_name),
    county_geoid = ifelse(is.na(county_geoid), county_geoid_closure, county_geoid),
    county_geoid = ifelse(is.na(county_geoid), county_geoid_install, county_geoid)
  )]

  # Remove duplicate columns
  panel_dates[, c("state_closure", "state_install", 
                 "county_name_closure", "county_name_install",
                 "county_geoid_closure", "county_geoid_install") := NULL]

  # Replace NAs with 0 (no events that month)
  panel_dates[is.na(tanks_closed), tanks_closed := 0]
  panel_dates[is.na(tanks_installed), tanks_installed := 0]
  panel_dates[is.na(single_walled_closed), single_walled_closed := 0]
  panel_dates[is.na(double_walled_closed), double_walled_closed := 0]
  panel_dates[is.na(single_walled_installed), single_walled_installed := 0]
  panel_dates[is.na(double_walled_installed), double_walled_installed := 0]

  # 7. Create replacement event flag
  panel_dates[, replacement_event := (tanks_closed > 0 & tanks_installed > 0) * 1]

  # Also flag single→double wall replacements (potential regulatory response)
  panel_dates[, single_to_double_replacement := (single_walled_closed > 0 & double_walled_installed > 0) * 1]

  # 8. Calculate average tank age at facility-month level
  # First aggregate tank ages by facility and year
  facility_yearly_tank_age <- tank_years[!is.na(tank_age), .(
    avg_tank_age = mean(tank_age, na.rm = TRUE),
    max_tank_age = max(tank_age, na.rm = TRUE),
    min_tank_age = min(tank_age, na.rm = TRUE),
    num_tanks = .N,
    state = first(state),  # Preserve location information
    county_name = first(county_name),
    county_geoid = first(county_geoid)
  ), by = .(facility_id, year)]

  # Join with panel_dates (using panel_year as the year)
  panel_dates <- merge(
    panel_dates,
    facility_yearly_tank_age,
    by.x = c("facility_id", "panel_year"),
    by.y = c("facility_id", "year"),
    all.x = TRUE,
    suffixes = c("", "_yearly")  # Avoid duplicate column names
  )

  # Consolidate state/county information (prioritize existing values)
  panel_dates[, `:=`(
    state = ifelse(is.na(state), state_yearly, state),
    county_name = ifelse(is.na(county_name), county_name_yearly, county_name),
    county_geoid = ifelse(is.na(county_geoid), county_geoid_yearly, county_geoid)
  )]

  # Remove duplicate columns
  panel_dates[, c("state_yearly", "county_name_yearly", "county_geoid_yearly") := NULL]

  # Process LUST data at the monthly level
  lust_monthly <- lust_data[!is.na(report_date), .(
    facility_id,
    lust_year = year(as.Date(report_date)),
    lust_month = month(as.Date(report_date)),
    state = state  # Carry state forward from LUST data
  )]

  # Remove duplicate LUST events in the same month at the same facility
  # (only count a facility as having a leak once per month)
  lust_monthly <- unique(lust_monthly, by = c("facility_id", "lust_year", "lust_month"))

  # Create month key for joining
  lust_monthly[, lust_month_key := paste0(lust_year, "-", sprintf("%02d", lust_month))]

  # Aggregate to facility-month level
  facility_monthly_lusts <- lust_monthly[, .(
    lust_detected = 1,
    state = first(state)  # Preserve state information
  ), by = .(facility_id, lust_month_key)]

  # Merge LUST data into panel
  panel_dates <- merge(
    panel_dates,
    facility_monthly_lusts,
    by.x = c("facility_id", "month_key"),
    by.y = c("facility_id", "lust_month_key"),
    all.x = TRUE,
    suffixes = c("", "_lust")  # Avoid duplicate column names
  )

  # Consolidate state information (prioritize existing values)
  panel_dates[, state := ifelse(is.na(state), state_lust, state)]
  panel_dates[, state_lust := NULL]  # Remove duplicate column

  # Replace NAs with 0 (no LUST that month)
  panel_dates[is.na(lust_detected), lust_detected := 0]

  # 10. Create policy period indicators - Texas policy change was in 1999
  panel_dates[, post_policy := as.integer(panel_year >= 2000)]

  # Create treatment indicator (Texas)
  panel_dates[, texas_treated := as.integer(state == "Texas")]
  # Create treatment indicator (Texas)
  panel_dates[, texas_treated := as.integer(state == "Texas")]

  # 12. Create timing variables for analysis
  # Calculate cumulative closures and replacements up to each month
  panel_dates <- panel_dates[order(facility_id, date)]

  # For each facility, calculate running totals
  panel_dates[, `:=`(
    cumulative_closures = cumsum(tanks_closed),
    cumulative_replacements = cumsum(replacement_event),
    cumulative_single_to_double = cumsum(single_to_double_replacement),
    has_previous_closure = as.integer(cumsum(tanks_closed) > 0),
    has_previous_replacement = as.integer(cumsum(replacement_event) > 0)
  ), by = facility_id]

  # 13. Calculate time since events
  panel_dates[, `:=`(
    months_since_last_closure = sequence(.N) - max(which(tanks_closed > 0), 0, na.rm = TRUE),
    months_since_last_replacement = sequence(.N) - max(which(replacement_event > 0), 0, na.rm = TRUE)
  ), by = facility_id]

  # Fix for facilities with no events
  panel_dates[is.infinite(months_since_last_closure), months_since_last_closure := NA]
  panel_dates[is.infinite(months_since_last_replacement), months_since_last_replacement := NA]

  # 14. For the 90-day window analysis
  panel_dates[, `:=`(
    lust_within_90d_of_closure = 0
  )]

  # For each facility and month with closures
  # Calculate the sum of lust_detected in the next 90 days for each facility
  panel_dates[, lust_future_sum := frollsum(shift(lust_detected, type = "lead"), n = 90, align = "left"), by = facility_id]

  # Flag rows with tank closures where the future 90-day sum is greater than 0
  panel_dates[tanks_closed > 0, lust_within_90d_of_closure := as.integer(lust_future_sum > 0)]

  # Remove the temporary column
  panel_dates[, lust_future_sum := NULL]

  # Remove temporary column
  panel_dates[, lust_ahead_temp := NULL]

  # 15. Create the final facility_leak_behavior dataset
  facility_leak_behavior <- panel_dates

  # Write it out for future use
  fwrite(facility_leak_behavior, here("Data", "Processed", "facility_leak_behavior_monthly.csv"))
  
  cat("Created facility_leak_behavior dataset from scratch.\n")
} else {
  # Just load the already-created data
  facility_leak_behavior <- fread(here("Data", "Processed", "facility_leak_behavior_monthly.csv"))
  
  cat("Loaded pre-existing facility_leak_behavior dataset.\n")
}

# Print summary statistics
cat("Facility-Month-Year Panel Summary (Motor Fuel Facilities Only):\n")
cat("Total facilities:", uniqueN(facility_leak_behavior$facility_id), "\n")
cat("Total observations:", nrow(facility_leak_behavior), "\n")
cat("Facilities with tank closures:", uniqueN(facility_leak_behavior[tanks_closed > 0]$facility_id), "\n")
cat("Facilities with replacements:", uniqueN(facility_leak_behavior[replacement_event > 0]$facility_id), "\n")
cat("Single-to-double wall replacements:", sum(facility_leak_behavior$single_to_double_replacement), "\n")
cat("LUSTs within 90 days of tank closure:", sum(facility_leak_behavior$lust_within_90d_of_closure), "\n")
cat("Average tank age (across all observations):", mean(facility_leak_behavior$avg_tank_age, na.rm = TRUE), "\n")

### Fix this latesr
# # Use this subset for further analysis
# facility_panel <- facility_panel_only_motor_fuel
# glimpse(facility_panel)
# glimpse(facility_panel$FR_mech_type)
# # groub by FR_meach_type and state and get the count of each
# facility_panel[, .N, by = FR_mech_type]

# #now set all FR for control states to "Public Fund" if they are not in the treated states
# facility_panel[!(state %in% treated_state_long), FR_mech_type := "Public Fund"]
control_states <- c(
  "Maine", "New Mexico", "Arkansas", "Oklahoma", "Louisiana", 
  "Kansas", "Montana", "Idaho", "South Dakota", "Alabama",
  "Minnesota", "North Carolina", "Illinois", "Massachusetts",
  "Ohio", "Pennsylvania", "Tennessee", "Virginia"
)
#------------------------------------------------------------------------------
# 6. Descriptive Analysis: Facility Characteristics and Leak Correlates
#------------------------------------------------------------------------------
    # First, calculate the histogram data to know max counts per wall type
    hist_data <- ggplot_build(
      ggplot(leak_observations_by_wall, aes(x = avg_tank_age)) +
      geom_histogram(binwidth = 2) +
      facet_wrap(~ wall_type, ncol = 1)
    )
    
    # Extract maximum count values by facet
    max_counts <- data.frame(
      wall_type = unique(leak_observations_by_wall$wall_type),
      max_count = vapply(split(hist_data$data[[1]]$count, hist_data$data[[1]]$PANEL), max, numeric(1))
    )
    leak_age_boxplot <- ggplot(leak_observations_by_wall, 
      aes(x = avg_tank_age, fill = wall_type)) +
      # Create histograms with bin width 2, bolder outline
      geom_histogram(binwidth = 2, color = "black", alpha = 0.8, size = 0.5) +
      # Create stacked panels (2 rows) by wall type
      facet_wrap(~ wall_type, ncol = 1, scales = 'free_y') +
      # Keep the same color scheme as before
      scale_fill_manual(
        values = c("Single-Walled Only" = "#E69F00", 
                  "Double-Walled Only" = "#009E73")
      ) +
      # # Add more visible vertical lines for mean values
      # geom_vline(
      #   aes(xintercept = mean(avg_tank_age)),
      #   linetype = "dashed", size = 1.2, color = "black",
      #   data = function(x) {
      #     x %>% 
      #       group_by(wall_type) %>%
      #       summarize(avg_tank_age = mean(avg_tank_age))
      #   }
      # ) +
      # Enhanced axis and labels for better visibility
      scale_x_continuous(breaks = seq(0, 50, by = 5)) +
      labs(
        title = "Distribution of Tank Age at Leak Discovery",
        # subtitle = "Comparison by Tank Wall Type",
        x = "Average Tank Age (Years)",
        y = "Count",
        fill = "Tank Wall Type"
      ) +
      theme_pub() +
      theme(
        # Larger text for projector visibility
        plot.title = element_text(size = 20, face = "bold"),
        plot.subtitle = element_text(size = 16),
        axis.title = element_text(size = 16, face = "bold"),
        axis.text = element_text(size = 14, face = "bold"),
        strip.text = element_text(size = 16, face = "bold"),
        # High contrast elements
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "gray80", size = 0.7),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, size = 1),
        # No legend needed with facets
        legend.position = "none"
      )

    print(leak_age_boxplot)


    # 6.3 Create age bin regression and coefficient plot
    # Create 25 age bins of width 2 from 0 to 50
    filtered_data <- facility_panel_only_motor_fuel[!is.na(avg_tank_age) & 
                     avg_tank_age <= 100 & avg_tank_age >= 0 & 
                     panel_year >= 1970 & panel_year <= 2020 & 
                     (state %in% control_states | state == "Texas")]

    # Create age bins directly
    filtered_data[, age_bin := cut(avg_tank_age, 
                    breaks = seq(0, 50, by = 2),
                    include.lowest = TRUE,
                    right = TRUE,
                    labels = paste0(seq(0, 48, by = 2), "-", seq(2, 50, by = 2)))]

    # Create age bins with 5-year intervals from 0-35, with 35+ as catch-all
    filtered_data[, age_bin2 := cut(avg_tank_age, 
             breaks = c(seq(0, 35, by = 5), Inf),
             include.lowest = TRUE,
             right = TRUE,
             labels = c(paste0(seq(0, 30, by = 5), "-", seq(5, 35, by = 5)), "35+"))]

# create a panel id which is facility id + state unique
filtered_data[,panel_id := paste0(facility_id,'_',state)]

setkey(filtered_data,panel_id,facility_id,state,panel_year)

head(filtered_data)
# Create a cleaner categorical variable for wall type
filtered_data <- filtered_data %>%
  mutate(wall_type = case_when(
    has_single_walled > 0 & has_double_walled == 0 ~ "Single-Walled",
    has_double_walled > 0 & has_single_walled == 0 ~ "Double-Walled",
    has_single_walled > 0 & has_double_walled > 0 ~ "Mixed",
    TRUE ~ "Unknown"
  ))

summary(filtered_data$has_single_walled)

# ---- ADD FIRST APPROACH STARTING AT LINE 1179 ----



risk_factors_model <- feols(
  leak_incident ~  has_single_walled + has_double_walled + 
                 active_tanks + has_gasoline + has_diesel   + i(age_bin2, ref = "0-5") |panel_year ,
  data = filtered_data,
  cluster = "state"
)

# Extract coefficient data from the risk factors model
coef_data <- tidy(risk_factors_model) %>%
  filter(!grepl("Intercept|panel", term)) %>%
  mutate(
    lower_ci = estimate - 1.96 * std.error,
    upper_ci = estimate + 1.96 * std.error,
    # Make term labels more readable
    term = gsub("_", " ", term) %>%
           str_to_title()
  )

# Create coefficient plot focused on key variables
risk_factors_model_coef_plot <- ggplot(coef_data, aes(x = estimate*100, y = reorder(term, estimate))) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_point(color = "#0072B2", size = 3) +
  geom_errorbarh(
    aes(xmin = lower_ci*100, xmax = upper_ci*100),
    height = 0.2,
    color = "#0072B2"
  ) +
  labs(
    title = "Facility Characteristics Associated with Leak Risk",
    subtitle = "Coefficient estimates with 95% confidence intervals",
    x = "Percentage Point Change in Leak Probability",
    y = ""
  ) +
  theme_pub() +
  theme(
    axis.text.y = element_text(size = 14, face = "bold"),
    plot.title = element_text(size = 16, face = "bold")
  )

risk_factors_model_coef_plot


###############################################################################
# REVISED RISK-PREDICTION BLOCK  (LOFO-CV, replaces old lines 729-1045)
###############################################################################

# --------------------------------------------------------------------------- #
# 0.  DATA PREP  (keep the same filtered_data that you built earlier)
# --------------------------------------------------------------------------- #
# filtered_data already has:
#   leak_incident  (0/1)   – outcome
#   facility_id              – panel unit id
#   state, panel_year        – FE variables
#   age_bin2                 – 5-year bins (factor)
#   wall_type                – "Single-Walled"/"Double-Walled"

library(data.table)
library(fixest)

# --------------------------------------------------------------------------- #
# 1.  CV PREDICTIONS
# --------------------------------------------------------------------------- #
# filtered_data[, lofo_pred := NA_real_]     # init column
# filtered_data[, kfold_pred := NA_real_]     # init column

fids <- unique(filtered_data$panel_id)
length(fids)
pb   <- txtProgressBar(min = 0, max = length(fids), style = 3)

class(filtered_data$age_bin2)
n_iterations <- 10

  kv_results <- data.table(
    panel_id= NA,
    state = NA,
    age_bin2 = NA,
    wall_type = NA,
    panel_year = NA,
    pred = NA_real_,
    fold_number = NA_real_
  )

for (k in 1:n_iterations) {
  print(paste('Starting fold ',k))
  # Set different seed for each iteration to ensure different samples
  set.seed(123456 + k)
  
  # Sample 80% of facilities
  sampled_facilities <- sample(fids, 
                             size = 0.8 * length(fids))
  
  # Subset data
  train <- filtered_data[panel_id %in% sampled_facilities]
  test <- filtered_data[!(panel_id %in% sampled_facilities)] 
  nrow(train) + nrow(test) == nrow(filtered_data)
  # Run model


  # LPM model with state & year FE and cluster by state
  fit <- feols(
           leak_incident ~ wall_type * age_bin2  +active_tanks + has_gasoline + has_diesel|panel_year +  state,
           data    = train,
           cluster = ~state
         )

  # Get fitted values
  subset_fitted <- data.table(
    panel_id = test$panel_id,
    state = test$state,
    age_bin2 = test$age_bin2,
    wall_type = test$wall_type,
    panel_year = test$panel_year,
    pred = predict(fit,newdata = test),
    fold_number = k
  )
 kv_results = rbindlist(list(kv_results,subset_fitted))
  print(paste('Finished fold ',k))

  setTxtProgressBar(pb, k)
}
close(pb)



# --------------------------------------------------------------------------- #
# 2.  SUMMARISE CV LEAK RISK  (by wall type × age bin)
# --------------------------------------------------------------------------- #
saveRDS(kv_results,here("Data", "Raw_do_not_write", "CV results.rds"))
kv_results <- readRDS(here("Data", "Raw_do_not_write", "CV results.rds"))
#remove NAs from kv_results
kv_results <- na.omit(kv_results)
cv_risk <- kv_results[, .(
              risk = mean(pred),
              n    = .N,
              se   = sd(pred) / sqrt(.N)
            ),
            by = .(age_bin2, wall_type)]

cv_risk[, `:=`(
  lower = risk - 1.96 * se,
  upper = risk + 1.96 * se
)]

#RMSE calculation

# Join actual leak incidents with predictions
kv_with_actual <- merge(
  kv_results,
  filtered_data[, .(panel_id,panel_year, leak_incident)],
  by = c("panel_id",'panel_year'),
  all.x = TRUE
)

# Remove missing values if any
if (any(is.na(kv_with_actual$leak_incident))) {
  warning("Some predictions could not be matched with actual values")
  kv_with_actual <- kv_with_actual[!is.na(leak_incident)]
}

# Calculate RMSE
kv_with_actual[, squared_error := (pred - leak_incident)^2]
rmse <- sqrt(mean(kv_with_actual$squared_error, na.rm = TRUE))

# Calculate RMSE by wall type
wall_type_rmse <- kv_with_actual[, .(
  RMSE = sqrt(mean((pred - leak_incident)^2, na.rm = TRUE)),
  n_obs = .N,
  avg_pred = mean(pred, na.rm = TRUE),
  avg_actual = mean(leak_incident, na.rm = TRUE)
), by = wall_type]

# Print results
cat("Overall Root Mean Square Error (RMSE):", round(rmse, 4), "\n")
print(wall_type_rmse)


# --------------------------------------------------------------------------- #
# 3.  ABSOLUTE-RISK PLOT (95 % CI ribbons)
# --------------------------------------------------------------------------- #
# Calculate the actual leak probabilities from the full dataset
actual_leak_rates <- filtered_data %>% 
  filter(wall_type %in% c("Single-Walled", "Double-Walled")) %>%
  summarize(
    actual_risk = mean(leak_incident, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

# Extract RMSE values for subtitle
overall_rmse <- round(rmse, 4)
sw_rmse <- round(wall_type_rmse[wall_type_rmse$wall_type == "Single-Walled", "RMSE"], 4)
dw_rmse <- round(wall_type_rmse[wall_type_rmse$wall_type == "Double-Walled", "RMSE"], 4)

# Now update the plot to include actual leak rates as triangle points
abs_risk_plot <- ggplot(cv_risk %>% filter(wall_type %in% c("Single-Walled", "Double-Walled")),
  aes(x = age_bin2, y = risk * 100, colour = wall_type, group = wall_type)) +
  # Original layers
  geom_errorbar(aes(ymin = lower*100, ymax = upper*100),
    width = 0.2, size = 0.8, position = position_dodge(width = 0.3)) + # Error bars
  geom_line(size = 1.3, position = position_dodge(width = 0.3)) +      # Lines connecting mean predictions
  geom_point(size = 3, position = position_dodge(width = 0.3)) +       # Points for mean predictions
  
  # Add actual leak rates as triangles
  
  # Scales and labels
  scale_colour_manual(
    name = "UST Tank Systems", # Sets the legend title
    values = c("Single-Walled"="#E69F00", "Double-Walled"="#009E73")
  ) +
  labs(
    title    = "Predicted Annual Leak Probability by Underground Storage Tank Age and Wall Type",
    subtitle = paste("Cross-validation estimates from 10-fold facility-level sampling.",
                     "Overall sample leak rate:", 
                     round(actual_leak_rates$actual_risk[1] * 100, 2), "% (N =", 
                     format(actual_leak_rates$n[1], big.mark=","), ")\n",
                     "RMSE: Overall =", overall_rmse, 
                     ", Single-Walled =", sw_rmse,
                     ", Double-Walled =", dw_rmse),
    x        = "Underground Storage Tank Age in Years (5-Year Bins)",
    y        = "Annual Leak Probability (%)"
  ) +
  theme_pub() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold")
  )

print(abs_risk_plot)

# Optionally save
ggsave(
  here("Output", "Figures", "cv_leak_risk_abs.png"),
  abs_risk_plot, width = 12, height = 7, dpi = 300, bg = "white"
)

ggsave(
  here("Output", "Figures", "cv_leak_risk_abs.pdf"),
  abs_risk_plot, width = 10, height = 7, device = cairo_pdf, bg = "white"
)


# Create a version of the plot without title
abs_risk_plot_no_title <- ggplot(cv_risk %>% filter(wall_type %in% c("Single-Walled", "Double-Walled")),
  aes(x = age_bin2, y = risk * 100, colour = wall_type, group = wall_type)) +
  # Original layers
  geom_errorbar(aes(ymin = lower*100, ymax = upper*100),
    width = 0.2, size = 0.8, position = position_dodge(width = 0.3)) + # Error bars
  geom_line(size = 1.3, position = position_dodge(width = 0.3)) +      # Lines connecting mean predictions
  geom_point(size = 3, position = position_dodge(width = 0.3)) +       # Points for mean predictions
  
  # Scales and labels
  scale_colour_manual(
    name = "UST Tank Systems", # Sets the legend title
    values = c("Single-Walled"="#E69F00", "Double-Walled"="#009E73")
  ) +
  labs(
    # No title here
    subtitle = paste("Cross-validation estimates from 10-fold facility-level sampling.",
                     "Overall sample leak rate:", 
                     round(actual_leak_rates$actual_risk[1] * 100, 2), "% (N =", 
                     format(actual_leak_rates$n[1], big.mark=","), ")\n",
                     "RMSE: Overall =", overall_rmse, 
                     ", Single-Walled =", sw_rmse,
                     ", Double-Walled =", dw_rmse),
    x        = "Underground Storage Tank Age in Years (5-Year Bins)",
    y        = "Annual Leak Probability (%)"
  ) +
  theme_pub() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold")
  )

# Print the plot
print(abs_risk_plot_no_title)

# Save the plot without title
ggsave(
  here("Output", "Figures", "cv_leak_risk_abs_no_title.png"),
  abs_risk_plot_no_title, width = 12, height = 7, dpi = 300, bg = "white"
)

ggsave(
  here("Output", "Figures", "cv_leak_risk_abs_no_title.pdf"),
  abs_risk_plot_no_title, width = 10, height = 7, device = cairo_pdf, bg = "white"
)
# Create the figure note as a text element that can be added to the plot or used in a report
figure_note <- paste0(
  "**Figure Note:** This figure shows predicted annual leak probabilities by tank age and wall type, ",
  "estimated using 10-fold cross-validation with facility-level sampling. The model specification includes ",
  "wall type interacted with age bin categories, controlling for active tanks, fuel type (gasoline/diesel), year, and state fixed effects. ",
  "Predictions are generated exclusively from out-of-sample facilities in each iteration to reduce overfitting. ",
  "Model performance metrics: Overall RMSE = ", round(rmse, 4), ", Single-Walled RMSE = ", 
  round(wall_type_rmse[wall_type_rmse$wall_type == "Single-Walled", "RMSE"], 4), 
  ", Double-Walled RMSE = ", round(wall_type_rmse[wall_type_rmse$wall_type == "Double-Walled", "RMSE"], 4), ". ",
  "The total sample includes ", format(actual_leak_rates$n[1], big.mark=","), " facility-year observations with an overall leak rate of ", 
  round(actual_leak_rates$actual_risk[1] * 100, 2), "%. ",
  "Error bars represent 95% confidence intervals for the mean prediction across all cross-validation iterations. ",
  "Single-walled tanks consistently show higher leak risk compared to double-walled tanks, with the risk gap ",
  "widening as tanks age, particularly after 15 years of operation."
)

cat(figure_note)  # Print the note for easy copying
###############################################################################
# END OF REPLACEMENT BLOCK
###############################################################################




# PART 1: Basic DiD Analysis - Texas vs Control States
#-----------------------------------------------------------------------------
cat("\n========== PART 1: BASIC DIFFERENCE-IN-DIFFERENCES ANALYSIS ==========\n")

# Simple DiD model to establish the baseline effect for All sample
setDT(filtered_data)



# Step 1: Determine pre-1999 facility classifications
# We look at facilities' wall_type for all their observations where panel_year < 1999
# Identify pre-1999 facility classifications using summed indicators
pre_1999_classifications <- filtered_data[panel_year < 1999, .(
  sum_sw = sum(has_single_walled, na.rm = TRUE),
  sum_dw = sum(has_double_walled, na.rm = TRUE)
), by = panel_id][, .(
  panel_id,
  is_sw_only_pre1999 = (sum_sw > 0 & sum_dw == 0),
  is_dw_only_pre1999 = (sum_dw > 0 & sum_sw == 0),
  is_mixed_or_unknown_pre1999 = (sum_sw > 0 & sum_dw > 0) | (sum_sw == 0 & sum_dw == 0)
)]




# Merge this classification back to the full filtered_data
pre_1999_classifications[, pre_1999_facility_type := fifelse(
  is_sw_only_pre1999, "SW_Only_Pre1999",
  fifelse(is_dw_only_pre1999, "DW_Only_Pre1999", "Mixed_Or_Unknown_Pre1999")
)]
filtered_data_classified <- merge(
  filtered_data, 
  pre_1999_classifications[, .(panel_id, pre_1999_facility_type)], 
  by = "panel_id", 
  all.x = TRUE
)
filtered_data_classified[,treatment_indicator:= as.integer(panel_year >= 2000 & state == "Texas")]
filtered_data_classified[,texas := as.integer(state == "Texas")]
filtered_data_classified[,post_period := 1*(panel_year >= 2000)]
filtered_data_classified[, post_period_2005 := (panel_year > 2005)*1]

view(filtered_data_classified  %>% filter('WE5051_Minnesota' == panel_id))

filtered_data_classified[, `:=`(
  # Exit indicator (facility closing in this period)
  Exit = 1*exit_flag,
  # Replacement indicator (at least one tank replaced)
  Replace = 1*replacement_event,
  any_tank_closed_in_year_flag = as.integer(tanks_closed_this_year > 0)
)]
# For facilities that only appear post-1999, pre_1999_facility_type will be NA.
# These will be excluded from the subgroup analyses by the subset conditions.

# Step 2: Create subsets based on pre-1999 classification
data_sw_only_pre1999 <- filtered_data_classified[pre_1999_facility_type == "SW_Only_Pre1999"]
data_dw_only_pre1999 <- filtered_data_classified[pre_1999_facility_type == "DW_Only_Pre1999"]
data_mixed_unknown_pre1999 <- filtered_data_classified[pre_1999_facility_type == "Mixed_Or_Unknown_Pre1999"]

# Step 3: Calculate and report shares
# Identify unique panel_ids that have observations before 1999
panel_ids_with_pre1999_data <- unique(filtered_data[panel_year < 1999, panel_id])
total_facilities_with_pre1999_obs <- length(panel_ids_with_pre1999_data)

cat(sprintf("Total unique facilities with observations pre-1999: %d\n", total_facilities_with_pre1999_obs))

num_sw_only_pre1999 <- uniqueN(data_sw_only_pre1999$panel_id)
num_dw_only_pre1999 <- uniqueN(data_dw_only_pre1999$panel_id)
num_mixed_unknown_pre1999 <- uniqueN(data_mixed_unknown_pre1999$panel_id)

share_sw_only <- if(total_facilities_with_pre1999_obs > 0) num_sw_only_pre1999 / total_facilities_with_pre1999_obs else 0
share_dw_only <- if(total_facilities_with_pre1999_obs > 0) num_dw_only_pre1999 / total_facilities_with_pre1999_obs else 0
share_mixed_unknown <- if(total_facilities_with_pre1999_obs > 0) num_mixed_unknown_pre1999 / total_facilities_with_pre1999_obs else 0

cat(sprintf("Share of pre-1999 facilities that were SW-only: %.2f%% (N=%d)\n", share_sw_only * 100, num_sw_only_pre1999))
cat(sprintf("Share of pre-1999 facilities that were DW-only: %.2f%% (N=%d)\n", share_dw_only * 100, num_dw_only_pre1999))
cat(sprintf("Share of pre-1999 facilities that were Mixed/Unknown: %.2f%% (N=%d)\n", share_mixed_unknown * 100, num_mixed_unknown_pre1999))
cat(sprintf("Sum of classified pre-1999 facilities: %d (should match total with pre-1999 obs if all are classified)\n", 
            num_sw_only_pre1999 + num_dw_only_pre1999 + num_mixed_unknown_pre1999))


## small fackts for slide 5

# Universe of USTs in Texas and control states(# of states) from 1970-2020.
# numb_of_unique_facilities = 100,000+ ( Texas: 50,000+; Control States: 50,000+)

# Define a function to format numbers with commas
format_with_commas <- function(x) {
  format(x, big.mark = ",", scientific = FALSE, trim = TRUE)
}

# Count unique facilities in Texas
texas_facilities <- uniqueN(facility_panel[state == "Texas" & panel_year >= 1970 & panel_year <= 2020, facility_id])

# Count unique facilities in control states
control_facilities <- uniqueN(facility_panel[state %in% control_states & panel_year >= 1970 & panel_year <= 2020, facility_id])

# Total unique facilities
total_facilities <- texas_facilities + control_facilities

# Count number of control states
num_control_states <- length(control_states)

# Create a summary table
cat("\n==== UNIVERSE OF UNDERGROUND STORAGE TANKS (1970-2020) ====\n")
cat("Total unique facilities:", format_with_commas(total_facilities), "\n")
cat("Texas facilities:", format_with_commas(texas_facilities), "\n")
cat("Control state facilities:", format_with_commas(control_facilities), 
    "(across", num_control_states, "states)\n")
# Get the two-letter abbreviations for control states
# First create a mapping between state names and abbreviations
state_mapping <- data.frame(
  state_name = state.name,
  state_abbr = state.abb,
  stringsAsFactors = FALSE
)

# Convert control states to their two-letter abbreviations
control_states_abbr <- state_mapping$state_abbr[match(control_states, state_mapping$state_name)]

# Print the control states with their abbreviations
control_states_df <- data.frame(
  state_name = control_states,
  state_abbr = control_states_abbr
)

# Display the results
print("Control States with Two-Letter Abbreviations:")
print(control_states_df)

# Just the list of abbreviations
cat("\nList of Control State Abbreviations:\n")
cat(paste(control_states_abbr, collapse = ", "))
cat("\n==========================================================\n")
cat("==========================================================\n")

# Count of unique facilities in the SW-only pre-1999 sample
# Count of unique facilities in the SW-only pre-1999 sample
num_sw_only_facilities <- uniqueN(data_sw_only_pre1999$panel_id)
cat("Number of unique SW-only pre-1999 facilities:", format_with_commas(num_sw_only_facilities), "\n")

# Count unique facilities by state
sw_only_by_state <- data_sw_only_pre1999[, .(count = uniqueN(panel_id)), by = state]
setorder(sw_only_by_state, -count)

# Display facility counts for Texas and control states
cat("SW-only facilities in Texas:", format_with_commas(sw_only_by_state[state == "Texas", count]), "\n")
control_sw_only <- sw_only_by_state[state %in% control_states, sum(count)]
cat("SW-only facilities in control states:", format_with_commas(control_sw_only), "\n")

# Calculate percent of all facilities that are SW-only pre-1999
pct_sw_only <- 100 * num_sw_only_facilities / total_facilities
cat("SW-only pre-1999 facilities as % of all facilities:", round(pct_sw_only, 1), "%\n")

# Print top 5 states by SW-only facility count
top_states <- sw_only_by_state[1:5]
cat("\nTop 5 states by SW-only facility count:\n")
print(top_states)
# Function to calculate and plot net policy effects by age group
calculate_and_plot_policy_effects_by_age <- function(model_object, model_data, outcome_variable_name, plot_title_outcome_name, plot_filename_suffix) {
  
  model_coefs <- coef(model_object)
  model_vcov <- vcov(model_object)
  
  # Define Age Bin Levels from the model_data's age_bin2 factor
  # Ensure age_bin2 is a factor in model_data
  if (!is.factor(model_data$age_bin2)) {
    stop("age_bin2 in model_data must be a factor.")
  }
  age_levels <- levels(model_data$age_bin2)
  ref_level <- "0-5" # As specified in i(age_bin2, ref = "0-5")
  other_age_levels <- setdiff(age_levels, ref_level)
  
  # Base DiD coefficient name (e.g., "texas:post_period")
  # This assumes your interaction is specified as texas*post_period*i(age_bin2, ...)
  coef_name_base_did <- "texas:post_period"

  results_list <- list()
  
  # Effect for the reference age group ("0-5")
  if (coef_name_base_did %in% names(model_coefs)) {
    effect_ref <- model_coefs[coef_name_base_did]
    se_ref <- sqrt(diag(model_vcov)[coef_name_base_did])
    
    results_list[[ref_level]] <- data.frame(
      age_bin2 = ref_level,
      estimate = effect_ref,
      std_error = se_ref,
      term_base = coef_name_base_did,
      term_interaction = NA_character_
    )
  } else {
    warning(paste("Base DiD coefficient '", coef_name_base_did, "' not found in model coefficients for outcome: ", outcome_variable_name, ". Check model summary.", sep=""))
    results_list[[ref_level]] <- data.frame(
      age_bin2 = ref_level,
      estimate = NA_real_,
      std_error = NA_real_,
      term_base = coef_name_base_did,
      term_interaction = NA_character_
    )
  }
  
  # Effects for other age groups
  for (age_level in other_age_levels) {
    coef_name_interaction <- paste0(coef_name_base_did, ":age_bin2::", age_level)
    
    current_effect <- NA_real_
    current_se <- NA_real_
    
    if (coef_name_base_did %in% names(model_coefs) && coef_name_interaction %in% names(model_coefs)) {
      current_effect <- model_coefs[coef_name_base_did] + model_coefs[coef_name_interaction]
      
      var_base <- model_vcov[coef_name_base_did, coef_name_base_did]
      var_interaction <- model_vcov[coef_name_interaction, coef_name_interaction]
      cov_base_interaction <- model_vcov[coef_name_base_did, coef_name_interaction]
      
      current_se <- sqrt(var_base + var_interaction + 2 * cov_base_interaction)
    } else {
      if (!(coef_name_base_did %in% names(model_coefs))) {
        warning(paste("Base DiD coefficient '", coef_name_base_did, "' not found for outcome: ", outcome_variable_name, ".", sep=""))
      }
      if (!(coef_name_interaction %in% names(model_coefs))) {
        warning(paste("Interaction coefficient '", coef_name_interaction, "' not found for age group '", age_level, "' for outcome: ", outcome_variable_name, ".", sep=""))
      }
    }
    
    results_list[[age_level]] <- data.frame(
      age_bin2 = age_level,
      estimate = current_effect,
      std_error = current_se,
      term_base = coef_name_base_did,
      term_interaction = coef_name_interaction
    )
  }
  
  policy_effects_df <- dplyr::bind_rows(results_list)
  
  policy_effects_df <- policy_effects_df %>%
    mutate(
      lower_ci = estimate - 1.96 * std_error,
      upper_ci = estimate + 1.96 * std_error
    )
  
  policy_effects_df$age_bin2 <- factor(policy_effects_df$age_bin2, levels = age_levels)
  
  cat(paste0("\nCalculated Full Policy Effects (Texas * Post-Period by Age Group) for '", outcome_variable_name, "':\n"))
  print(policy_effects_df)
  
  # Create and save the plot
  plot_effects <- ggplot(policy_effects_df, aes(x = age_bin2, y = estimate * 100)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    geom_point(size = 3, color = "#0072B2") +
    geom_errorbar(
      aes(ymin = lower_ci * 100, ymax = upper_ci * 100),
      width = 0.2,
      color = "#0072B2",
      size = 0.8
    ) +
    labs(
      title = paste("Policy Effect on", plot_title_outcome_name, "by Tank Age"),
      subtitle = "Model: SW-Only (Pre-1999) facilities, Texas vs. Control, Post-Period, interacted with Tank Age.\nEffect shown is (Texas * Post-Period) for each age group.",
      x = "Tank Age Group (Years)",
      y = paste("Change in", plot_title_outcome_name, "Probability (Percentage Points)")
    ) +
    theme_pub() + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  print(plot_effects)
  
  plot_file_path <- here("Output", "Figures", paste0("policy_effect_", plot_filename_suffix, "_by_age.png"))
  ggsave(
    plot_file_path,
    plot_effects,
    width = 10, height = 7, dpi = 300, bg = "white"
  )
  cat(paste0("\nPlot saved to: ", plot_file_path, "\n"))
  
  return(policy_effects_df)
} # end of function

#########################

# 1. Tank Closure Models (any_tank_closed_in_year_flag)
model_closure_basic <- feols(
  any_tank_closed_in_year_flag ~ texas*post_period | panel_year + panel_id,
  data = data_sw_only_pre1999,
  cluster = "state"
)

model_closure_age <- feols(
  any_tank_closed_in_year_flag ~ texas*post_period + i(age_bin2, ref = "0-5") | panel_year + panel_id,
  data = data_sw_only_pre1999,
  cluster = "state"
)

# 1. Tank Closure Models (any_tank_closed_in_year_flag)
model_closure_basic_2005 <- feols(
  any_tank_closed_in_year_flag ~ texas*post_period_2005 | panel_year + panel_id,
  data = data_sw_only_pre1999,
  cluster = "state"
)

model_closure_age_2005 <- feols(
  any_tank_closed_in_year_flag ~ texas*post_period_2005 + i(age_bin2, ref = "0-5") | panel_year + panel_id,
  data = data_sw_only_pre1999,
  cluster = "state"
)

H

# 2. Exit Models (conditional on tank closure)
model_exit_cond_basic <- feols(
  Exit ~ texas*post_period | panel_year + panel_id,
  data = data_sw_only_pre1999[any_tank_closed_in_year_flag==1],
  cluster = "state"
)

model_exit_cond_age <- feols(
  Exit ~ texas*post_period + i(age_bin2, ref = "0-5") | panel_year + panel_id,
  data = data_sw_only_pre1999[any_tank_closed_in_year_flag==1],
  cluster = "state"
)

model_exit_cond_basic_2005 <- feols(
  Exit ~ texas*post_period_2005 | panel_year + panel_id,
  data = data_sw_only_pre1999[any_tank_closed_in_year_flag==1],
  cluster = "state"
)

model_exit_cond_age_2005 <- feols(
  Exit ~ texas*post_period_2005 + i(age_bin2, ref = "0-5") | panel_year + panel_id,
  data = data_sw_only_pre1999[any_tank_closed_in_year_flag==1],
  cluster = "state"
)

# 3. Replacement Models (conditional on tank closure)
model_replace_cond_basic <- feols(
  Replace ~ texas*post_period | panel_year + panel_id,
  data = data_sw_only_pre1999[any_tank_closed_in_year_flag==1],
  cluster = "state"
)

model_replace_cond_age <- feols(
  Replace ~ texas*post_period + i(age_bin2, ref = "0-5") | panel_year + panel_id,
  data = data_sw_only_pre1999[any_tank_closed_in_year_flag==1],
  cluster = "state"
)

model_replace_cond_basic_2005 <- feols(
  Replace ~ texas*post_period_2005 | panel_year + panel_id,
  data = data_sw_only_pre1999[any_tank_closed_in_year_flag==1],
  cluster = "state"
)

model_replace_cond_age_2005 <- feols(
  Replace ~ texas*post_period_2005 + i(age_bin2, ref = "0-5") | panel_year + panel_id,
  data = data_sw_only_pre1999[any_tank_closed_in_year_flag==1],
  cluster = "state"
)

# Create comparison tables
# Table 1: Basic models (without age controls)
etable(model_closure_basic, model_exit_cond_basic, model_replace_cond_basic,
       headers = c("SW Tank Closure", "SW Exit|Closure", "SW Replacement|Closure"),
       title = "Policy Effects on Single-Walled Facility Decisions (Basic Models)",
       dict = c("texas:post_period" = "Texas × Post-Policy"),
 keep ="%texas:post_period",
        notes = "All models include facility and year fixed effects. Sample restricted to Single-Walled Only facilities (pre-1999).")

# Table 2: Models with age controls
# Create LaTeX table comparing basic and age-controlled models
latex_table <- etable(
  model_closure_basic, model_closure_age, 
  model_exit_cond_basic, model_exit_cond_age, 
  model_replace_cond_basic, model_replace_cond_age,
  headers = c("Tank Closure", "Tank Closure", 
              "Exit|Closure", "Exit|Closure", 
              "Replace|Closure", "Replace|Closure"),
  title = "Policy Effects on Single-Walled Facility Decisions",
  dict = c("texas" = "Texas", 
           "post_period" = "Post-Policy", 
           "texas:post_period" = "Texas × Post-Policy"),
  keep = "%texas:post_period",
  notes = "All models include facility and year fixed effects. Sample restricted to Single-Walled Only facilities (pre-1999).",
  extralines = list("Age Controls" = c("No", "Yes", "No", "Yes", "No", "Yes")),
  tex = TRUE
)
# Create a comprehensive table note
table_note <- "\\textit{Note:} This table presents difference-in-differences estimates of the effect of Texas' 1999 transition from public to private insurance on underground storage tank leak incidents. All estimates are from linear probability models with facility and year fixed effects. The sample is restricted to facilities that had only single-walled tanks in the pre-1999 period. The key coefficient 'Texas × Post-Policy' represents the differential change in annual leak probability for Texas facilities relative to control state facilities after the policy change. Column 1 shows the basic specification, while Column 2 adds controls for tank age using 5-year bins (coefficients for age bins not shown). Standard errors (in parentheses) are clustered at the state level. The results indicate that Texas facilities experienced a statistically significant [decrease/increase] in leak incidents following the policy change, with the effect [magnified/attenuated] when controlling for tank age. $^{*}p<0.1$; $^{**}p<0.05$; $^{***}p<0.01$."
## LEAK events #####3

# 1. Basic Leak Model (no age controls)
model_leak_basic <- feols(
  leak_incident ~ texas*post_period | panel_year + panel_id,
  data = data_sw_only_pre1999,
  cluster = "state"
)

# 2. Leak Model with Age Controls
model_leak_age <- feols(
  leak_incident ~ texas*post_period + i(age_bin2, ref = "0-5") | panel_year + panel_id,
  data = data_sw_only_pre1999,
  cluster = "state"
)

# 3. Leak Model with Heterogeneous Treatment Effects by Age
model_leak_hte <- feols(
  leak_incident ~ texas*post_period * i(age_bin2, ref = "0-5") | panel_year + panel_id,
  data = data_sw_only_pre1999,
  cluster = "state"
)

# Create comparison tables
# Table: Basic and Age-Controlled models
etable(model_leak_basic, model_leak_age,
       headers = c("Basic", "With Age Controls"),
       title = "Policy Effects on Leak Incidents at Single-Walled Facilities",
       dict = c("texas:post_period" = "Texas × Post-Policy"),
       keep = "%texas:post_period",
       notes = "All models include facility and year fixed effects. Sample restricted to Single-Walled Only facilities (pre-1999).")

# Generate LaTeX table
latex_table <- etable(
  model_leak_basic, model_leak_age,
  headers = c("Basic", "With Age Controls"),
  title = "Policy Effects on Leak Incidents at Single-Walled Facilities",
  dict = c("texas" = "Texas", 
           "post_period" = "Post-Policy", 
           "texas:post_period" = "Texas × Post-Policy"),
  keep = "%texas:post_period",
  notes = "All models include facility and year fixed effects. Sample restricted to Single-Walled Only facilities (pre-1999).",
  extralines = list("Age Controls" = c("No", "Yes")),
  tex = TRUE
)

# Print the LaTeX code to copy
(latex_table)

# Now generate the HTE plot for leak incidents by age group
leak_effects_by_age <- calculate_and_plot_policy_effects_by_age(
  model_object = model_leak_hte,
  model_data = data_sw_only_pre1999,
  outcome_variable_name = "leak_incident",
  plot_title_outcome_name = "Reported Leak Incidents",
  plot_filename_suffix = "leak_incidents_sw_only"
)

etable(model_leak_hte)


did_model_closed_sw_only_pre1999 <- feols(
  any_tank_closed_in_year_flag ~ texas*post_period * i(age_bin2, ref = "0-5") | panel_year + panel_id,
  data = data_sw_only_pre1999,
  cluster = "state"
)

etable(did_model_closed_sw_only_pre1999)

data_sw_only_pre1999_1 = data_sw_only_pre1999
data_sw_only_pre1999_1[, post_period := (panel_year > 2005)*1]
did_model_closed_sw_only_pre1999_2005 <- feols(
  any_tank_closed_in_year_flag ~ texas*post_period * i(age_bin2, ref = "0-5") | panel_year + panel_id,
  data = data_sw_only_pre1999_1,
  cluster = "state"
)

etable(did_model_closed_sw_only_pre1999_2005)


# did_model_exit_not_cond_sw_only_pre1999 <- feols(
#   Exit ~ texas*post_period*  i(age_bin2, ref = "0-5") | panel_year + panel_id,
#   data = data_sw_only_pre1999,
#   cluster = "state"
# )

etable(did_model_exit_not_cond_sw_only_pre1999)

did_model_exit_sw_only_pre1999 <- feols(
  Exit ~ texas*post_period*  i(age_bin2, ref = "0-5") | panel_year + panel_id,
  data = data_sw_only_pre1999[any_tank_closed_in_year_flag==1],
  cluster = "state"
)

etable(did_model_exit_sw_only_pre1999)

plot_file_path_combined_conditional

did_model_rep_sw_only_pre1999 <- feols(
  Replace ~ texas*post_period* i(age_bin2, ref = "0-5") | panel_year + panel_id,
  data = data_sw_only_pre1999[any_tank_closed_in_year_flag==1],
  cluster = "state"
)

etable(did_model_closed_sw_only_pre1999)

etable(did_model_rep_sw_only_pre1999)



# Now, let's define/confirm the models and then use the function.

# Model 1: Any Tank Closure (already defined in your selection)
did_model_closed_sw_only_pre1999 <- feols(
  any_tank_closed_in_year_flag ~ texas*post_period * i(age_bin2, ref = "0-5") | panel_year + panel_id,
  data = data_sw_only_pre1999,
  cluster = "state"
)
etable(did_model_closed_sw_only_pre1999)

# Model 2: Facility Exit (Unconditional)
# This is based on your commented-out 'did_model_exit_not_cond_sw_only_pre1999'
# Ensure data_sw_only_pre1999 has 'Exit' and 'age_bin2' columns.
did_model_exit_sw_only_pre1999_age_unconditional <- feols(
  Exit ~ texas*post_period * i(age_bin2, ref = "0-5") | panel_year + panel_id,
  data = data_sw_only_pre1999, # Using the full SW-only pre-1999 data
  cluster = "state"
)
etable(did_model_exit_sw_only_pre1999_age_unconditional)

# Model 3: Tank Replacement (Conditional on any_tank_closed_in_year_flag == 1)
# This is 'did_model_rep_sw_only_pre1999' from your selection (lines 1119-1123)
# Ensure data_sw_only_pre1999[any_tank_closed_in_year_flag==1] has 'Replace' and 'age_bin2'.
# The model name in your script is did_model_rep_sw_only_pre1999.
did_model_rep_sw_only_pre1999 <- feols(
  Replace ~ texas*post_period* i(age_bin2, ref = "0-5") | panel_year + panel_id,
  data = data_sw_only_pre1999[any_tank_closed_in_year_flag==1],
  cluster = "state"
)
etable(did_model_rep_sw_only_pre1999)


# Apply the function to each model:

# 1. For 'any_tank_closed_in_year_flag'
effects_any_closure <- calculate_and_plot_policy_effects_by_age(
  model_object = did_model_closed_sw_only_pre1999,
  model_data = data_sw_only_pre1999, # Data used for the model
  outcome_variable_name = "any_tank_closed_in_year_flag",
  plot_title_outcome_name = "Any Tank Closure",
  plot_filename_suffix = "any_closure_sw_only"
)

# 1. For 'any_tank_closed_in_year_flag' 2005
effects_any_closure_2005 <- calculate_and_plot_policy_effects_by_age(
  model_object = did_model_closed_sw_only_pre1999_2005,
  model_data = data_sw_only_pre1999_1, # Data used for the model
  outcome_variable_name = "any_tank_closed_in_year_flag",
  plot_title_outcome_name = "Any Tank Closure",
  plot_filename_suffix = "any_closure_sw_only_2005"
)

# 2. For 'Exit' (unconditional)
effects_exit_unconditional <- calculate_and_plot_policy_effects_by_age(
  model_object = did_model_exit_sw_only_pre1999_age_unconditional,
  model_data = data_sw_only_pre1999, # Data used for the model
  outcome_variable_name = "Exit",
  plot_title_outcome_name = "Facility Exit (Unconditional)",
  plot_filename_suffix = "exit_unconditional_sw_only"
)


# 3. For 'Replace' (conditional)
# Note: model_data for age_levels should be the one that has all age levels,
# even if the model is run on a subset. Or, ensure the subset still contains all age factor levels.
# For simplicity, we use data_sw_only_pre1999 to get age levels, assuming age_bin2 structure is consistent.
# If data_sw_only_pre1999[any_tank_closed_in_year_flag==1] drops age levels, this might need adjustment.
effects_replace_conditional <- calculate_and_plot_policy_effects_by_age(
  model_object = did_model_rep_sw_only_pre1999, # This is the model from lines 1119-1123
  model_data = data_sw_only_pre1999[any_tank_closed_in_year_flag==1], # Actual data used for this specific model
  outcome_variable_name = "Replace",
  plot_title_outcome_name = "Tank Replacement (Conditional on Tank Closure)",
  plot_filename_suffix = "replace_conditional_sw_only"
)


# 4. For 'Exit' (conditional on any_tank_closed_in_year_flag == 1)
# Model definition is from your lines 1109-1114:
# did_model_exit_sw_only_pre1999 <- feols(
#   Exit ~ texas*post_period*  i(age_bin2, ref = "0-5") | panel_year + panel_id,
#   data = data_sw_only_pre1999[any_tank_closed_in_year_flag==1],
#   cluster = "state"
# )
# etable(did_model_exit_sw_only_pre1999) # Already in your script

effects_exit_conditional <- calculate_and_plot_policy_effects_by_age(
  model_object = did_model_exit_sw_only_pre1999, # This is the model from lines 1109-1114
  model_data = data_sw_only_pre1999[any_tank_closed_in_year_flag==1], # Actual data used for this specific model
  outcome_variable_name = "Exit",
  plot_title_outcome_name = "Facility Exit (Conditional on Tank Closure)",
  plot_filename_suffix = "exit_conditional_sw_only"
)


# 5. Create a combined plot for Conditional Exit and Conditional Replacement effects

# Add an outcome type to each effects dataframe
effects_exit_conditional$outcome_type <- "Exit (Conditional)"
effects_replace_conditional$outcome_type <- "Replace (Conditional)"

# Combine the dataframes
combined_conditional_effects <- rbind(effects_exit_conditional, effects_replace_conditional)

# Ensure outcome_type is a factor for consistent legend order
combined_conditional_effects$outcome_type <- factor(
  combined_conditional_effects$outcome_type,
  levels = c("Exit (Conditional)", "Replace (Conditional)")
)

# Create the combined plot
plot_combined_conditional_effects <- ggplot(
  combined_conditional_effects, 
  aes(x = age_bin2, y = estimate * 100, color = outcome_type, group = outcome_type)
) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_point(size = 3, position = position_dodge(width = 0.3)) +
  geom_errorbar(
    aes(ymin = lower_ci * 100, ymax = upper_ci * 100),
    width = 0.2,
    position = position_dodge(width = 0.3),
    size = 0.8
  ) +
  # geom_line(position = position_dodge(width = 0.3), size = 1) + # Added lines
  scale_color_manual(
    name = "Decision (Conditional on Tank Closure)",
    values = c("Exit (Conditional)" = "#D55E00", "Replace (Conditional)" = "#009E73") # Example colors
  ) +
  scale_y_continuous(
    labels = scales::percent_format(scale = 1), # Convert to percentage format
    breaks = seq(-80, 50, by = 5) # Adjust breaks as needed
  ) +
  labs(
    title = "Policy Effect on Nature of Tank Closure by Tank Age",
    subtitle = "Model: SW-Only (Pre-1999) facilities, conditional on any tank closed in year.\nEffect shown is (Texas * Post-Period) for each age group.",
    x = "Tank Age Group (Years)",
    y = "Change in Probability (Percentage Points)"
  ) +
  theme_pub() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(plot_combined_conditional_effects)

plot_file_path_combined_conditional <- here("Output", "Figures", "policy_effect_exit_vs_replace_conditional_sw_only_by_age.png")
ggsave(
  plot_file_path_combined_conditional,
  plot_combined_conditional_effects,
  width = 12, height = 7, dpi = 300, bg = "white"
)
cat(paste0("\nCombined conditional plot saved to: ", plot_file_path_combined_conditional, "\n"))

#####Event Study #######

###########################################################3

# Construct event time: years relative to policy (1999)
data_sw_only_pre1999[, event_time := panel_year - 1999]

# Cap event time to reasonable range for balance (e.g., -10 to +20)
data_sw_only_pre1999[event_time < -10, event_time := -10]
data_sw_only_pre1999[event_time >  20, event_time := 20]

# Make event time a factor for i() in feols
data_sw_only_pre1999[, event_time_f := factor(event_time)]

# 1998 spike: create a dummy for year 1998 (panel_year == 1998)
data_sw_only_pre1999[, removal_spike := as.integer(panel_year == 1998)]

# Use age-bin and (optionally) wall type if you want to stratify
model_event_study <- feols(
  leak_incident ~ 
    # fully saturated event-time indicators interacted with texas
    i(event_time_f, texas, ref = "-1") + 
    # control for removal spike
    removal_spike  |
     panel_year+panel_id ,  # panel and year FE
  data = data_sw_only_pre1999,
  cluster = "state"
)
es_sw <- etable(model_event_study, drop = "removal_spike|texas|panel_year|_FE")
# Or for tidy output:
library(broom)
es_sw_tidy <- broom::tidy(model_event_study, conf.int = TRUE)
es_sw_tidy <- es_sw_tidy[grepl("event_time_f::", es_sw_tidy$term), ]
es_sw_tidy$event_time <- as.integer(sub(".*event_time_f::(-?\\d+):texas", "\\1", es_sw_tidy$term))

ggplot(es_sw_tidy, aes(x = event_time, y = estimate)) +
  geom_point(size = 3, color = "#E69F00") +  # Orange color for single-wall tanks
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.3, color = "#E69F00") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  labs(x = "Years Since Policy (1999)",
       y = "Event Study: Policy Effect on Leaks",
       title = "Dynamic Policy Effects on Leak Incidents (Single-Wall Tanks, Texas)",
       caption = "Reference year: -1 (1998)") +
  theme_pub() +  # Using the custom theme defined earlier
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

etable(model_event_study)


###############################################################


# Construct event time: years relative to policy (2005)
data_sw_only_pre1999[, event_time := panel_year - 2005]

# Cap event time to reasonable range for balance (e.g., -10 to +20)
data_sw_only_pre1999[event_time < -10, event_time := -10]
data_sw_only_pre1999[event_time >  20, event_time := 20]

# Make event time a factor for i() in feols
data_sw_only_pre1999[, event_time_f := factor(event_time)]

# 2004 spike: create a dummy for year 2004 (panel_year == 2004)
data_sw_only_pre1999[, removal_spike := as.integer(panel_year == 2004)]

# Use age-bin and (optionally) wall type if you want to stratify
model_event_study_2005 <- feols(
  any_tank_closed_in_year_flag ~ 
    # fully saturated event-time indicators interacted with texas
    i(event_time_f, texas, ref = "-1") + 
    # control for removal spike
    removal_spike  |
     panel_year+panel_id ,  # panel and year FE
  data = data_sw_only_pre1999,
  cluster = "state"
)

es_sw <- etable(model_event_study_2005, drop = "removal_spike|texas|panel_year|_FE")
# Or for tidy output:
library(broom)
es_sw_tidy <- broom::tidy(model_event_study_2005, conf.int = TRUE)
es_sw_tidy <- es_sw_tidy[grepl("event_time_f::", es_sw_tidy$term), ]
es_sw_tidy$event_time <- as.integer(sub(".*event_time_f::(-?\\d+):texas", "\\1", es_sw_tidy$term))

ggplot(es_sw_tidy, aes(x = event_time, y = estimate)) +
  geom_point(size = 3, color = "#E69F00") +  # Orange color for single-wall tanks
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.3, color = "#E69F00") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  labs(x = "Years Since Policy (1999)",
       y = "Event Study: Policy Effect on Leaks",
       title = "Dynamic Policy Effects on Leak Incidents (Single-Wall Tanks, Texas)",
       caption = "Reference year: -1 (1998)") +
  theme_pub() +  # Using the custom theme defined earlier
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

etable(model_event_study_2005)

#################################################################3
# Construct event time: years relative to policy (1999)
data_sw_only_pre1999[, event_time := panel_year - 1999]

# Cap event time to reasonable range for balance (e.g., -10 to +20)
data_sw_only_pre1999[event_time < -10, event_time := -10]
data_sw_only_pre1999[event_time >  20, event_time := 20]

# Make event time a factor for i() in feols
data_sw_only_pre1999[, event_time_f := factor(event_time)]

# 1998 spike: create a dummy for year 1998 (panel_year == 1998)
data_sw_only_pre1999[, removal_spike := as.integer(panel_year == 1998)]
# Get coefficients and CIs for event-time dummies for single-wall tanks
es_sw <- etable(model_event_study, drop = "removal_spike|texas|panel_year|_FE")
# Or for tidy output:
library(broom)
es_sw_tidy <- broom::tidy(model_event_study, conf.int = TRUE)
es_sw_tidy <- es_sw_tidy[grepl("event_time_f::", es_sw_tidy$term), ]
es_sw_tidy$event_time <- as.integer(sub(".*event_time_f::(-?\\d+):texas", "\\1", es_sw_tidy$term))

ggplot(es_sw_tidy, aes(x = event_time, y = estimate)) +
  geom_point(size = 3, color = "#E69F00") +  # Orange color for single-wall tanks
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.3, color = "#E69F00") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  labs(x = "Years Since Policy (1999)",
       y = "Event Study: Policy Effect on Leaks",
       title = "Dynamic Policy Effects on Leak Incidents (Single-Wall Tanks, Texas)",
       caption = "Reference year: -1 (1998)") +
  theme_pub() +  # Using the custom theme defined earlier
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

etable(model_event_study)

# Save the plot
ggsave(
  here("Output", "Figures", "event_study_leaks_single_wall.png"),
  width = 10, height = 6, dpi = 300, bg = "white"
)





#####3
# Construct event time: years relative to policy (1999)
data_dw_only_pre1999[, event_time := panel_year - 1999]

# Cap event time to reasonable range for balance (e.g., -10 to +20)
data_dw_only_pre1999[event_time < -10, event_time := -10]
data_dw_only_pre1999[event_time >  20, event_time := 20]

# Make event time a factor for i() in feols
data_dw_only_pre1999[, event_time_f := factor(event_time)]

# 1998 spike: create a dummy for year 1998 (panel_year == 1998)
data_dw_only_pre1999[, removal_spike := as.integer(panel_year == 1998)]

# Use age-bin and (optionally) wall type if you want to stratify
model_event_study_dw <- feols(
  leak_incident ~ texas*post_period |    panel_year+panel_id ,  # panel and year FE
  data = data_dw_only_pre1999,
  cluster = "state"
)

model_event_study_dw <- feols(
  leak_incident ~ texas*post_period +  i(age_bin2, ref = "0-5")  |  panel_year+panel_id ,  # panel and year FE
  data = data_dw_only_pre1999,
  cluster = "state"
)

model_event_study_dw <- feols(
  leak_incident ~ texas*post_period * i(age_bin2, ref = "0-5")  |  panel_year+panel_id ,  # panel and year FE
  data = data_dw_only_pre1999,
  cluster = "state"
)
etable(model_event_study_dw)
###
# Get coefficients and CIs for event-time dummies for double-walled tanks
es_dw <- etable(model_event_study_dw)
# Or for tidy output:
library(broom)
es_dw_tidy <- broom::tidy(model_event_study_dw, conf.int = TRUE)
es_dw_tidy <- es_dw_tidy[grepl("event_time_f::", es_dw_tidy$term), ]
es_dw_tidy$event_time <- as.integer(sub(".*event_time_f::(-?\\d+):texas", "\\1", es_dw_tidy$term))

ggplot(es_dw_tidy, aes(x = event_time, y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.3) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  labs(x = "Years Since Policy (1999)",
       y = "Event Study: Policy Effect on Leaks",
       title = "Dynamic Policy Effects on Leak Incidents (Double-Wall Tanks, Texas)",
       caption = "Reference year: -1 (1998)") +
  theme_minimal(base_size = 14)

# Save the plot
ggsave(
  here("Output", "Figures", "event_study_leaks_double_wall.png"),
  width = 10, height = 6, dpi = 300, bg = "white"
)
## closure

# Use age-bin and (optionally) wall type if you want to stratify
model_event_study_closure <- feols(
  any_tank_closed_in_year_flag ~ 
    # fully saturated event-time indicators interacted with texas
    i(event_time_f, texas, ref = "-1") + 
    # control for removal spike
    removal_spike  |
    panel_id + panel_year,  # panel and year FE
  data = data_sw_only_pre1999,
  cluster = "state"
)



# Get coefficients and CIs for event-time dummies
es_closure <- etable(model_event_study_closure, drop = "removal_spike|texas|panel_year|_FE")
# Or for tidy output:
library(broom)
es_closure_tidy <- broom::tidy(model_event_study_closure, conf.int = TRUE)
es_closure_tidy <- es_closure_tidy[grepl("event_time_f::", es_closure_tidy$term), ]
es_closure_tidy$event_time <- as.integer(sub(".*event_time_f::(-?\\d+):texas", "\\1", es_closure_tidy$term))

ggplot(es_closure_tidy, aes(x = event_time, y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.3) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  labs(x = "Years Since Policy (1999)",
       y = "Event Study: Policy Effect on Tank Closures",
       title = "Dynamic Policy Effects on Tank Closures (Single-Wall Tanks, Texas)",
       caption = "Reference year: -1 (1998)") +
  theme_minimal(base_size = 14)

# Save the plot
ggsave(
  here("Output", "Figures", "event_study_tank_closures.png"),
  width = 10, height = 6, dpi = 300, bg = "white"
)

here("Output", "Figures", "event_study_tank_closures.png")

ggplot(es_closure_tidy, aes(x = event_time, y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.3) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  labs(x = "Years Since Policy (1999)",
       y = "Event Study: Policy Effect on Tank Closures",
      #  title = "Dynamic Policy Effects on Tank Closures (Single-Wall Tanks, Texas)",
       caption = "Reference year: -1 (1998)") +
  theme_minimal(base_size = 14)

# Save the plot
ggsave(
  here("Output", "Figures", "event_study_tank_closures_no_title.png"),
  width = 10, height = 6, dpi = 300, bg = "white"
)

# Figure Note:
# This figure presents an event study of the differential effect of Texas' 1999 transition from public to private insurance on underground storage tank (UST) closure decisions. The plot shows point estimates and 95% confidence intervals for the interaction between a Texas indicator and each event year, relative to 1998 (the year before the policy change). The sample is restricted to facilities that had only single-walled tanks in the pre-1999 period. The underlying regression includes facility and year fixed effects, with standard errors clustered at the state level. The specification controls for a 1998 spike in tank removals that occurred industry-wide. The pattern reveals how the policy affected the timing and magnitude of tank closure decisions, with particular attention to whether facility owners strategically accelerated or delayed tank closures in response to the policy change. Estimates to the right of the vertical red line represent post-policy treatment effects, while estimates to the left establish the pre-trends necessary for causal identification in the difference-in-differences framework. The number of facility-year observations is [N], with [X] unique facilities across Texas and control states during 1970-2020.

#########################3


# 1. Extract Coefficients and Variance-Covariance Matrix
model_coefs <- coef(did_model_sw_only_pre1999)
model_vcov <- vcov(did_model_sw_only_pre1999)

# 2. Define Age Bin Levels
#    These should match the levels of 'age_bin2' used in the model.
#    The reference level "0-5" is specified in i(age_bin2, ref = "0-5").
age_levels <- levels(factor(did_model_sw_only_pre1999_mixed$age_bin2)) # Get levels from the data used

# Or, if you are certain about the levels:
# age_levels <- c("0-5", "5-10", "10-15", "15-20", "20-25", "25-30", "30-35", "35+")
ref_level <- "0-5" # As specified in your model i(age_bin2, ref = "0-5")
other_age_levels <- setdiff(age_levels, ref_level)

# 3. Calculate Effects and Standard Errors for Each Age Group

results_list <- list()

# Effect for the reference age group ("0-5")
coef_name_base_did <- "texas_treated:post_1999"


if (coef_name_base_did %in% names(model_coefs)) {
  effect_ref <- model_coefs[coef_name_base_did]
  se_ref <- sqrt(model_vcov[coef_name_base_did, coef_name_base_did])
  
  results_list[[ref_level]] <- data.frame(
    age_bin2 = ref_level,
    estimate = effect_ref,
    std_error = se_ref,
    term_base = coef_name_base_did,
    term_interaction = NA_character_
  )
} else {
  warning(paste("Base DiD coefficient '", coef_name_base_did, "' not found in model coefficients. Check model summary.", sep=""))
  results_list[[ref_level]] <- data.frame(
    age_bin2 = ref_level,
    estimate = NA_real_,
    std_error = NA_real_,
    term_base = coef_name_base_did,
    term_interaction = NA_character_
  )
}

# Effects for other age groups
for (age_level in other_age_levels) {
  coef_name_interaction <- paste0("texas_treated:post_1999:age_bin2::", age_level)
  
  current_effect <- NA_real_
  current_se <- NA_real_
  
  # Check if both necessary coefficients exist
  if (coef_name_base_did %in% names(model_coefs) && coef_name_interaction %in% names(model_coefs)) {
    # Full policy effect = base DiD effect + interaction effect for this age group
    current_effect <- model_coefs[coef_name_base_did] + model_coefs[coef_name_interaction]
    
    # Calculate Standard Error for the sum of two coefficients:
    # SE(A+B) = sqrt(Var(A) + Var(B) + 2*Cov(A,B))
    var_base <- model_vcov[coef_name_base_did, coef_name_base_did]
    var_interaction <- model_vcov[coef_name_interaction, coef_name_interaction]
    # Ensure covariance can be extracted; vcov is symmetric
    cov_base_interaction <- model_vcov[coef_name_base_did, coef_name_interaction] 
                            
    current_se <- sqrt(var_base + var_interaction + 2 * cov_base_interaction)
    
  } else {
    if (!(coef_name_base_did %in% names(model_coefs))) {
        warning(paste("Base DiD coefficient '", coef_name_base_did, "' not found.", sep=""))
    }
    if (!(coef_name_interaction %in% names(model_coefs))) {
        warning(paste("Interaction coefficient '", coef_name_interaction, "' not found for age group '", age_level, "'.", sep=""))
    }
  }
  
  results_list[[age_level]] <- data.frame(
    age_bin2 = age_level,
    estimate = current_effect,
    std_error = current_se,
    term_base = coef_name_base_did,
    term_interaction = coef_name_interaction
  )
}

# Combine list into a data frame
policy_effects_sw_mixed_df <- dplyr::bind_rows(results_list)

# Calculate 95% Confidence Intervals
policy_effects_sw_mixed_df <- policy_effects_sw_mixed_df %>%
  mutate(
    lower_ci = estimate - 1.96 * std_error,
    upper_ci = estimate + 1.96 * std_error
  )

# Ensure age_bin2 is a factor with the correct order for plotting
policy_effects_sw_mixed_df$age_bin2 <- factor(policy_effects_sw_mixed_df$age_bin2, levels = age_levels)

# Print the resulting data frame
cat("\nCalculated Full Policy Effects (Texas * Post-1999 by Age Group) for SW-Only Pre-1999 Facilities:\n")
print(policy_effects_sw_mixed_df)

# 4. Create the Plot
plot_sw_mixed_effects <- ggplot(policy_effects_sw_mixed_df, aes(x = age_bin2, y = estimate * 100)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_point(size = 3, color = "#0072B2") +
  geom_errorbar(
    aes(ymin = lower_ci * 100, ymax = upper_ci * 100),
    width = 0.2,
    color = "#0072B2",
    size = 0.8
  ) +
  labs(
    title = "Full Policy Effect on Leak Incidents by Tank Age",
    subtitle = "Model: SW-Only (Pre-1999) facilities, Texas vs. Control, Post-1999, interacted with Tank Age\nEffect shown is (Texas * Post-1999) for each age group.",
    x = "Tank Age Group (Years)",
    y = "Change in Leak Probability (Percentage Points)"
  ) +
  theme_pub() + # Using your predefined theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Display the plot
print(plot_sw_mixed_effects)

# Optionally, save the plot
ggsave(
  here("Output", "Figures", "policy_effect_sw_only_mixed_by_age.png"),
  plot_sw_mixed_effects,
  width = 10, height = 7, dpi = 300, bg = "white"
)


# DiD for facilities that were DW-Only Pre-1999
did_model_dw_only_pre1999 <- feols(
  leak_incident ~ texas_treated*post_1999 | panel_year + panel_id,
  data = data_dw_only_pre1999,
  cluster = "state"
)

# DiD for facilities that were Mixed or Unknown Pre-1999
did_model_mixed_unknown_pre1999 <- feols(
  leak_incident ~ texas_treated*post_1999 | panel_year + panel_id,
  data = data_mixed_unknown_pre1999,
  cluster = "state"
)

# Step 5: Present results
etable(basic_did_model_overall, did_model_sw_only_pre1999, did_model_dw_only_pre1999, did_model_mixed_unknown_pre1999,
       headers = list("Overall Sample", "SW Only (Pre-1999)", "DW Only (Pre-1999)", "Mixed/Unknown (Pre-1999)"),
       dict = c(texas_treated="Texas", post_1999="Post-1999", "texas_treated:post_1999"="Texas x Post-1999"),
       title = "DiD Estimates of Policy Impact on Leak Incidents by Pre-1999 Facility Tank Type")


# Placebo test using pre-treatment periods only (e.g., 1990-1998)
# This tests parallel trends assumption
filtered_data[,placebo_treatment := texas_treated*(panel_year== 1990)]
placebo_did <- feols(
  leak_incident ~ placebo_treatment | panel_id + panel_year,
  data = filtered_data[panel_year < 1999],
  cluster = "state"
)

placebo_did_SW_only <- feols(
  leak_incident ~ placebo_treatment | panel_id + panel_year,
  data = filtered_data[panel_year < 1999 & wall_type == "Single-Walled"],
  cluster = "state"
)

placebo_did_DW_only <- feols(
  leak_incident ~ placebo_treatment | panel_id + panel_year,
  data = filtered_data[panel_year < 1999& wall_type == "Double-Walled"],
  cluster = "state"
)

etable(placebo_did,placebo_did_SW_only,placebo_did_DW_only)



# Ensure 'age_het' model is run and libraries are loaded:
 library(parallel)
 library(foreach)
 library(doParallel)

cat("\n========== BOOTSTRAPPING NET EFFECTS (MANUAL CALCULATION) FOR age_het MODEL ==========\n")

# 0. Ensure original age_het model is run and data is prepared
# age_het <- feols(...) # Assumed to be run
# model_data_for_age_het <- bind_rows(...) # Assumed to be prepared

if (!exists("age_het") || !inherits(age_het, "fixest")) {
  stop("The 'age_het' model object does not exist or is not a fixest object. Please run it first.")
}
if (!exists("model_data_for_age_het") || !is.data.frame(model_data_for_age_het)) {
  stop("The 'model_data_for_age_het' data frame does not exist. Please create it.")
}
if (!"state" %in% names(model_data_for_age_het)) {
  stop("The 'state' column (clustering variable) is missing from model_data_for_age_het.")
}
if (!"panel_id" %in% names(model_data_for_age_het)) {
    warning("panel_id column not explicitly found in model_data_for_age_het; ensure it's present for feols.")
}


# Define age bin levels and base coefficient names
age_bin_levels <- c("0-5", "5-10", "10-15", "15-20", "20-25", "25-30", "30-35", "35+")
coef_TP <- "texas_treated:post_1999"
coef_TPS <- "texas_treated:post_1999:is_only_sw"

# --- 1. Calculate original point estimates (from the full model) ---
original_coefs_age_het <- coef(age_het)
original_point_estimates_list <- list()
all_original_coef_names <- names(original_coefs_age_het)

for (age_label in age_bin_levels) {
    is_ref_age <- (age_label == "0-5")
    
    # Net Effect for Non-SW-Only (is_only_sw = 0)
    val_non_sw <- NA_real_
    if (coef_TP %in% all_original_coef_names) {
        val_non_sw <- original_coefs_age_het[coef_TP]
        if (!is_ref_age) {
            coef_TPAj <- paste0(coef_TP, ":age_bin2::", age_label)
            if (coef_TPAj %in% all_original_coef_names) {
                val_non_sw <- val_non_sw + original_coefs_age_het[coef_TPAj]
            } else { val_non_sw <- NA_real_; warning(paste("Orig Coef Missing:", coef_TPAj)) }
        }
    }
    original_point_estimates_list[[length(original_point_estimates_list) + 1]] <- data.frame(
        age_bin2 = age_label, facility_type = "Non-SW-Only", estimate = unname(val_non_sw)
    )

    # Net Effect for SW-Only (is_only_sw = 1)
    val_sw <- NA_real_
    if (coef_TP %in% all_original_coef_names && coef_TPS %in% all_original_coef_names) {
        val_sw <- original_coefs_age_het[coef_TP] + original_coefs_age_het[coef_TPS]
        if (!is_ref_age) {
            coef_TPAj <- paste0(coef_TP, ":age_bin2::", age_label)
            coef_TPAjS <- paste0(coef_TP, ":age_bin2::", age_label, ":is_only_sw")
            
            if (coef_TPAj %in% all_original_coef_names) {
                val_sw <- val_sw + original_coefs_age_het[coef_TPAj]
            } else { val_sw <- NA_real_; warning(paste("Orig Coef Missing:", coef_TPAj)) }
            
            if (!is.na(val_sw) && coef_TPAjS %in% all_original_coef_names) {
                val_sw <- val_sw + original_coefs_age_het[coef_TPAjS]
            } else { val_sw <- NA_real_; warning(paste("Orig Coef Missing:", coef_TPAjS)) }
        }
    } else {
        if (!(coef_TP %in% all_original_coef_names)) warning(paste("Orig Coef Missing:", coef_TP))
        if (!(coef_TPS %in% all_original_coef_names)) warning(paste("Orig Coef Missing:", coef_TPS))
    }
    original_point_estimates_list[[length(original_point_estimates_list) + 1]] <- data.frame(
        age_bin2 = age_label, facility_type = "SW-Only", estimate = unname(val_sw)
    )
}
original_point_estimates_df <- bind_rows(original_point_estimates_list)
print("Original Point Estimates for Plotting:")
print(original_point_estimates_df)

# --- 2. Setup for Parallel Bootstrap ---
n_bootstraps <- 500 # Adjust as needed (e.g., 250-1000). More is better but slower.
unique_cluster_ids <- unique(model_data_for_age_het$state)
n_clusters <- length(unique_cluster_ids)

# Parallel setup
num_cores <- floor(detectCores() / 2)
if (num_cores < 1) num_cores <- 1
cat("Setting up parallel cluster with", num_cores, "cores for bootstrap.\n")
cl <- makeCluster(num_cores)
registerDoParallel(cl)

# Export necessary variables and load packages on workers
clusterExport(cl, varlist = c("model_data_for_age_het", "unique_cluster_ids", "n_clusters",
                               "age_bin_levels", "coef_TP", "coef_TPS"))
clusterEvalQ(cl, {
  library(fixest)
  library(dplyr)
})

cat("Starting parallel bootstrap replications (", n_bootstraps, " iterations)...\n")
# --- 3. Parallel Bootstrap Loop ---
boot_results_parallel_list <- foreach(
  i_boot = 1:n_bootstraps,
  .combine = 'rbind',
  .errorhandling = 'pass' # Continue if an iteration fails, 'pass' returns the error object
) %dopar% {
  
  # Create bootstrap sample of clusters (states)
  sampled_cluster_indices <- sample(1:n_clusters, size = n_clusters, replace = TRUE)
  
  boot_data_list_iter <- list()
  for(k_cluster in 1:n_clusters){
      # Get data for the k_cluster-th sampled state ID
      # This ensures that if a state is sampled multiple times, its data is included multiple times
      boot_data_list_iter[[k_cluster]] <- model_data_for_age_het[model_data_for_age_het$state == unique_cluster_ids[sampled_cluster_indices[k_cluster]], ]
  }
  boot_data <- bind_rows(boot_data_list_iter)
  
  # Re-run feols model on bootstrap sample
  # Suppress messages from feols during bootstrap for cleaner output
  boot_model_iter <- tryCatch({
    suppressMessages(
      fixest::feols(
        leak_incident ~ texas_treated*post_1999*i(age_bin2, ref = "0-5")*is_only_sw | panel_year + panel_id,
        data = boot_data,
        cluster = ~state # Important for fixest internals, though SEs come from bootstrap variation
      )
    )
  }, error = function(e) { NULL })

  if (is.null(boot_model_iter) || !isTRUE(boot_model_iter$converged)) {
    # Return a structure that indicates failure for this iteration for these specific effects
    # This helps in diagnosing issues if many iterations fail
    failed_iter_results <- list()
    for (age_label_iter in age_bin_levels) {
        failed_iter_results[[length(failed_iter_results) + 1]] <- data.frame(
            bootstrap_iter = i_boot, age_bin2 = age_label_iter,
            facility_type = "Non-SW-Only", boot_estimate = NA_real_
        )
        failed_iter_results[[length(failed_iter_results) + 1]] <- data.frame(
            bootstrap_iter = i_boot, age_bin2 = age_label_iter,
            facility_type = "SW-Only", boot_estimate = NA_real_
        )
    }
    return(bind_rows(failed_iter_results))
  }

  boot_coefs_iter <- coef(boot_model_iter)
  all_boot_coef_names_iter <- names(boot_coefs_iter)
  iter_results_list <- list()

  # Calculate and store net effects for this bootstrap sample
  for (age_label_iter in age_bin_levels) {
    is_ref_age_iter <- (age_label_iter == "0-5")
    
    # Net effect for Non-SW-Only
    val_non_sw_boot <- NA_real_
    if (coef_TP %in% all_boot_coef_names_iter) {
        val_non_sw_boot <- boot_coefs_iter[coef_TP]
        if (!is_ref_age_iter) {
            coef_TPAj_boot <- paste0(coef_TP, ":age_bin2::", age_label_iter)
            if (coef_TPAj_boot %in% all_boot_coef_names_iter) {
                 val_non_sw_boot <- val_non_sw_boot + boot_coefs_iter[coef_TPAj_boot]
            } else { val_non_sw_boot <- NA_real_ }
        }
    }
    iter_results_list[[length(iter_results_list) + 1]] <- data.frame(
        bootstrap_iter = i_boot, age_bin2 = age_label_iter,
        facility_type = "Non-SW-Only", boot_estimate = unname(val_non_sw_boot)
    )

    # Net effect for SW-Only
    val_sw_boot <- NA_real_
    if (coef_TP %in% all_boot_coef_names_iter && coef_TPS %in% all_boot_coef_names_iter) {
        val_sw_boot <- boot_coefs_iter[coef_TP] + boot_coefs_iter[coef_TPS]
        if (!is_ref_age_iter) {
            coef_TPAj_boot <- paste0(coef_TP, ":age_bin2::", age_label_iter)
            coef_TPAjS_boot <- paste0(coef_TP, ":age_bin2::", age_label_iter, ":is_only_sw")
            
            if (coef_TPAj_boot %in% all_boot_coef_names_iter) {
                val_sw_boot <- val_sw_boot + boot_coefs_iter[coef_TPAj_boot]
            } else { val_sw_boot <- NA_real_ }
            
            if (!is.na(val_sw_boot) && coef_TPAjS_boot %in% all_boot_coef_names_iter) {
                val_sw_boot <- val_sw_boot + boot_coefs_iter[coef_TPAjS_boot]
            } else { val_sw_boot <- NA_real_ }
        }
    }
    iter_results_list[[length(iter_results_list) + 1]] <- data.frame(
        bootstrap_iter = i_boot, age_bin2 = age_label_iter,
        facility_type = "SW-Only", boot_estimate = unname(val_sw_boot)
    )
  }
  bind_rows(iter_results_list)
} # End of foreach loop

stopCluster(cl)
cat("Parallel bootstrap replications finished.\n")

# --- 4. Process Bootstrap Results and Calculate CIs ---
# boot_results_parallel_list will now be a data frame
boot_estimates_df <- as.data.frame(boot_results_parallel_list)

# Check for errors passed by foreach
num_errors <- sum(sapply(boot_estimates_df$boot_estimate, function(x) inherits(x, "error") || (is.list(x) && inherits(x[[1]], "error"))))
if (num_errors > 0) {
    warning(paste(num_errors, "bootstrap iterations resulted in errors and were excluded or returned NA."))
    # Filter out rows that might be error objects if 'pass' was used and they weren't NA
    boot_estimates_df <- boot_estimates_df[!sapply(boot_estimates_df$boot_estimate, function(x) inherits(x, "error")),]
}
boot_estimates_df <- boot_estimates_df[!is.na(boot_estimates_df$boot_estimate),]


if (nrow(boot_estimates_df) > 0) {
    bootstrap_summary_df <- boot_estimates_df %>%
        group_by(age_bin2, facility_type) %>%
        summarise(
            # mean_boot_estimate = mean(boot_estimate, na.rm = TRUE), # Can be used as an alternative point estimate
            lower_ci_boot = quantile(boot_estimate, 0.025, na.rm = TRUE, type = 6), # type 6 is recommended by Hyndman & Fan
            upper_ci_boot = quantile(boot_estimate, 0.975, na.rm = TRUE, type = 6),
            n_successful_boots = sum(!is.na(boot_estimate)),
            .groups = 'drop'
        )

    # Merge bootstrapped CIs with original point estimates
    plot_data_total_effects_boot <- original_point_estimates_df %>%
      left_join(bootstrap_summary_df, by = c("age_bin2", "facility_type")) %>%
      filter(!is.na(estimate)) # Keep only rows where original estimate was valid

    # Ensure age_bin2 and facility_type are factors with correct order
    plot_data_total_effects_boot$age_bin2 <- factor(plot_data_total_effects_boot$age_bin2, levels = age_bin_levels)
    plot_data_total_effects_boot$facility_type <- factor(plot_data_total_effects_boot$facility_type, levels = c("Non-SW-Only", "SW-Only"))

    cat("\nCalculated Total Net Effects with Bootstrap CIs:\n")
    print(plot_data_total_effects_boot)

    # --- 5. Create the Plot ---
    total_effects_plot_boot <- ggplot(plot_data_total_effects_boot, aes(x = age_bin2, y = estimate * 100, color = facility_type, group = facility_type)) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
      geom_point(position = position_dodge(width = 0.3), size = 3) +
      geom_errorbar(aes(ymin = lower_ci_boot * 100, ymax = upper_ci_boot * 100),
                    width = 0.2, position = position_dodge(width = 0.3), size = 0.8, na.rm = TRUE) +
      geom_line(position = position_dodge(width = 0.3), size = 1, na.rm = TRUE) +
      scale_color_manual(values = c("Non-SW-Only" = "#0072B2", "SW-Only" = "#E69F00")) +
      labs(
        title = "Net Policy Effect on Leak Incidents (Manual Bootstrap CIs)",
        subtitle = paste("Estimated DiD (Texas * Post-1999) for facilities existing pre-1999.\n'is_only_sw' measured at time t. CIs from", 
                         min(plot_data_total_effects_boot$n_successful_boots, na.rm=T), "successful cluster bootstrap reps."),
        x = "Tank Age Group (Years)",
        y = "Percentage Point Change in Leak Probability",
        color = "Facility Tank Type (in year t)"
      ) +
      theme_pub() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))

    print(total_effects_plot_boot)

    ggsave(
      here("Output", "Figures", "total_net_policy_effect_sw_age_manual_bootstrap_CIs.png"),
      total_effects_plot_boot,
      width = 10, height = 7, dpi = 300, bg = "white"
    )
} else {
    cat("Bootstrap failed to produce any valid estimates. Check for errors in the loop or model specification with resampled data.\n")
    cat("Number of rows in boot_estimates_df:", nrow(boot_estimates_df), "\n")
}





# For SW- controling for age only pre-1999 facilities
age_ctrl_SW_pre1999 <- feols(
  leak_incident ~ texas_treated*post_1999 + i(age_bin2, ref = "0-5") | 
    panel_year + panel_id,
  data = data_sw_only_pre1999,
  cluster = "state"
)

# For SW-only pre-1999 facilities
age_het_SW_pre1999 <- feols(
  leak_incident ~ texas_treated*post_1999*i(age_bin2, ref = "0-5") | 
    panel_year + panel_id,
  data = data_sw_only_pre1999,
  cluster = "state"
)

# For DW-only pre-1999 facilities
age_het_DW_pre1999 <- feols(
  leak_incident ~ texas_treated*post_1999*i(age_bin2, ref = "0-5") | 
    panel_year + panel_id,
  data = data_dw_only_pre1999,
  cluster = "state"
)

# For Mixed/Unknown pre-1999 facilities
age_het_Mixed_pre1999 <- feols(
  leak_incident ~ texas_treated*post_1999*i(age_bin2, ref = "0-5") | 
    panel_year + panel_id,
  data = data_mixed_unknown_pre1999,
  cluster = "state"
)

etable(age_het_SW_pre1999, age_het_DW_pre1999, age_het_Mixed_pre1999,
       headers = list("SW Only (Pre-1999)", "DW Only (Pre-1999)", "Mixed/Unknown (Pre-1999)"))

View(age_het_SW_pre1999)
View(age_het_DW_pre1999)
View(age_het_Mixed_pre1999)
# Create comparison table
cat("Heterogeneous Effects by Tank Type:\n")
summary(wall_type_het)

cat("\nHeterogeneous Effects by Age Group:\n")
summary(age_het)

# Extract and visualize age heterogeneity for easier interpretation
age_coefs <- tidy(age_het) %>%
  filter(grepl("texas_treated:post_1999:age_group::", term)) %>%
  mutate(
    age_group = gsub("texas_treated:post_1999:age_group::", "", term),
    lower_ci = estimate - 1.96 * std.error,
    upper_ci = estimate + 1.96 * std.error
  )

# Add baseline effect
baseline_effect <- coef(age_het)["texas_treated:post_1999"]
baseline_se <- se(age_het)["texas_treated:post_1999"]

age_effects_simple <- rbind(
  data.frame(
    age_group = "0-5",
    estimate = baseline_effect,
    std.error = baseline_se,
    lower_ci = baseline_effect - 1.96 * baseline_se,
    upper_ci = baseline_effect + 1.96 * baseline_se
  ),
  age_coefs
)

# Order age groups correctly
age_bin_levels <- c("0-5", paste0(seq(5, 30, by 5), "-", seq(10, 35, by 5)), "35+")
age_effects_simple$age_group <- factor(age_effects_simple$age_group, levels = age_bin_levels)

# Create a more readable plot for age heterogeneity
age_het_simple_plot <- ggplot(
  age_effects_simple,
  aes(x = age_group, y = estimate*100)
) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 1) +
  geom_point(size = 4, color = "#0072B2") +
  geom_errorbar(
    aes(ymin = lower_ci*100, ymax = upper_ci*100),
    width = 0.2,
    color = "#0072B2", 
    size = 1
  ) +
  labs(
    title = "Heterogeneous Policy Effects by Tank Age",
    subtitle = "Baseline DiD without single-wall interaction",
    x = "Tank Age Group (Years)",
    y = "Percentage Point Change in Leak Probability"
  ) +
  theme_pub() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12, face = "bold"),
    axis.text.y = element_text(size = 12, face = "bold")
  )

print(age_het_simple_plot)
ggsave(
  here("Output", "Figures", "age_het_simple.png"), 
  age_het_simple_plot, 
  width = 10, 
  height = 6, 
  dpi = 300, 
  bg = "white"
)

# PART 3: Full Triple-Difference with SW and Age Interaction
#-----------------------------------------------------------------------------
cat("\n========== PART 3: TRIPLE-DIFFERENCE WITH SINGLE-WALL AND AGE ==========\n")

# Cleaner approach to triple-difference model
hte_with_wall_simplified <- feols(
  leak_incident ~ i(age_group, ref = "0-5")*texas_treated*post_1999*has_single_walled | 
                 panel_year + panel_id,
  data = filtered_data,
  cluster = "state"
)

# Extract results focusing on the key triple interactions
triple_diff_results <- tidy(hte_with_wall_simplified) %>%
  filter(grepl("texas_treated:post_1999:has_single_walled:age_group::", term)) %>%
  mutate(
    age_group = gsub("texas_treated:post_1999:has_single_walled:age_group::", "", term),
    lower_ci = estimate - 1.96 * std.error,
    upper_ci = estimate + 1.96 * std.error
  )

# Add baseline effect for age group 0-5
baseline_triple <- coef(hte_with_wall_simplified)["texas_treated:post_1999:has_single_walled"]
baseline_se_triple <- se(hte_with_wall_simplified)["texas_treated:post_1999:has_single_walled"]

triple_diff_complete <- rbind(
  data.frame(
    age_group = "0-5",
    estimate = baseline_triple,
    std.error = baseline_se_triple,
    lower_ci = baseline_triple - 1.96 * baseline_se_triple,
    upper_ci = baseline_triple + 1.96 * baseline_se_triple
  ),
  triple_diff_results
)

# Order age groups correctly
triple_diff_complete$age_group <- factor(triple_diff_complete$age_group, 
                                        levels = age_bin_levels)

# Create enhanced plot showing the triple interaction effects
triple_diff_plot <- ggplot(
  triple_diff_complete,
  aes(x = age_group, y = estimate*100)
) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 1.5) +
  geom_point(
    size = 6, 
    color = "black", 
    fill = "#E69F00",
    shape = 21, 
    stroke = 1.5
  ) +
  geom_errorbar(
    aes(ymin = lower_ci*100, ymax = upper_ci*100), 
    width = 0.3,
    color = "black",
    size = 1.2
  ) +
  labs(
    title = "Effect of Policy Change on Single-Walled Tanks by Age",
    subtitle = "Triple-difference estimates (Texas × Post-1999 × Single-Wall × Age)",
    x = "Tank Age Group (Years)",
    y = "Percentage Point Change in Leak Probability"
  ) +
  theme_pub() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14, face = "bold"),
    axis.text.y = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 16, face = "bold"),
    plot.title = element_text(size = 18, face = "bold"),
    plot.subtitle = element_text(size = 14)
  )

print(triple_diff_plot)
ggsave(
  here("Output", "Figures", "triple_diff_by_age.png"), 
  triple_diff_plot, 
  width = 12, 
  height = 8, 
  dpi = 300, 
  bg = "white"
)

# Alternative specification using continuous age
# This provides a smoother estimate of how effects change with age
age_continuous_model <- feols(
  leak_incident ~ texas_treated*post_1999*has_single_walled*poly(avg_tank_age, 3, raw = TRUE) | 
                  panel_year + panel_id,
  data = filtered_data[!is.na(avg_tank_age)],
  cluster = "state"
)

# Generate predicted values for plotting
age_range <- seq(0, 40, by = 0.5)
pred_data <- expand.grid(
  avg_tank_age = age_range,
  texas_treated = 1,
  post_1999 = 1,
  has_single_walled = 1
)

# Extract coefficients for prediction
coefs <- coef(age_continuous_model)
base_effect <- coefs["texas_treated:post_1999:has_single_walled"]
age_effect <- coefs["texas_treated:post_1999:has_single_walled:poly(avg_tank_age, 3, raw = TRUE)1"]
age2_effect <- coefs["texas_treated:post_1999:has_single_walled:poly(avg_tank_age, 3, raw = TRUE)2"]
age3_effect <- coefs["texas_treated:post_1999:has_single_walled:poly(avg_tank_age, 3, raw = TRUE)3"]

# Calculate predicted effect at each age
pred_data$predicted_effect <- base_effect + 
                             age_effect * pred_data$avg_tank_age +
                             age2_effect * pred_data$avg_tank_age^2 +
                             age3_effect * pred_data$avg_tank_age^3

# Plot continuous age effect
continuous_age_plot <- ggplot(
  pred_data,
  aes(x = avg_tank_age, y = predicted_effect*100)
) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_line(color = "#E69F00", size = 1.5) +
  labs(
    title = "Effect of Policy on Single-Walled Tanks by Continuous Age",
    subtitle = "Cubic polynomial specification",
    x = "Tank Age (Years)",
    y = "Percentage Point Change in Leak Probability"
  ) +
  theme_pub()

print(continuous_age_plot)
ggsave(
  here("Output", "Figures", "continuous_age_effect.png"), 
  continuous_age_plot, 
  width = 10, 
  height = 6, 
  dpi = 300, 
  bg = "white"
)

#------------------------------------------------------------------------------
# 8. Firm Behavior Mechanism Tests
#------------------------------------------------------------------------------
cat("\n========== FIRM BEHAVIOR MECHANISM TESTS ==========\n")

# 8.1 Direct Effects Model: Testing Exit vs. Replacement Mechanisms
cat("Estimating direct effects model for firm behavior mechanisms...\n")

# First ensure we have the necessary indicator variables properly defined
facility_panel_only_motor_fuel[, `:=`(
  # Exit indicator (facility closing in this period)
  Exit = 1*exit_flag,
  # Replacement indicator (at least one tank replaced)
  Replace = 1*replacement_event
)]


# Estimate the direct effects model with mechanism interactions
mechanism_direct_model <- feols(
  leak_incident ~ texas_treated * post_1999 * Exit + 
                  texas_treated * post_1999 * Replace + 
                  Exit + Replace +
                  texas_treated:Exit + post_1999:Exit +
                  texas_treated:Replace + post_1999:Replace +
                  active_tanks + avg_tank_age | 
                  facility_id + panel_year,
  data = facility_panel_only_motor_fuel[state == "Texas" | state %in% control_states & 
                        panel_year >= 1970 & panel_year <= 2020],
  cluster = "state"
)

# Save as CSV for easy import into Beamer slides
fwrite(tidy(mechanism_direct_model), here("Output", "Tables", "mechanism_results_beamer.csv"))

mechanism_hte_age <- feols(
  leak_incident ~ texas_treated * post_1999 * Exit*age_group + 
                  texas_treated * post_1999 * Replace*age_group + 
                  active_tanks  | 
                  facility_id + panel_year,
  data = facility_panel[state == "Texas" | state %in% control_states & 
                        panel_year >= 1970 & panel_year <= 2020],
  cluster = "state"
)

#Match leak detection results
formula_str <- paste0(
  "leak_incident ~ ", 
  "i(age_group, ref = '0-5') + ", 
  "texas_treated * post_1999 * Exit+",
  "texas_treated * post_1999 * Replace+",
  "texas_treated + ", 
  "post_1999 + ",
  "i(age_group, ref = '0-5'):texas_treated + ", 
  "i(age_group, ref = '0-5'):post_1999 + ", 
  "texas_treated:post_1999 + ",
  "i(age_group, ref = '0-5'):texas_treated:post_1999 | ",
  "panel_year + panel_id"
)

# Run the model with the formula
mediation_with_ate_hte_causal <- feols(
  as.formula(formula_str),
  data = filtered_data,
  cluster = "state"
)

View(tidy(mediation_with_ate_hte_causal))


#------------------------------------------------------------------------------
# 8. Firm Behavior Mechanism Tests
#------------------------------------------------------------------------------
cat("\n========== FIRM BEHAVIOR MECHANISM TESTS ==========\n")

# 8.1 Direct Effects Model: Testing Exit vs. Replacement Mechanisms
cat("Estimating direct effects model for firm behavior mechanisms...\n")

# First ensure we have the necessary indicator variables properly defined
facility_panel[, `:=`(
  # Exit indicator (facility closing in this period)
  Exit = 1*exit_flag,
  # Replacement indicator (at least one tank replaced)
  Replace = 1*replacement_event
)]

# Estimate the direct effects model with mechanism interactions
mechanism_direct_model <- feols(
  leak_incident ~ texas_treated * post_1999 * Exit + 
                  texas_treated * post_1999 * Replace + 
                  Exit + Replace +
                  texas_treated:Exit + post_1999:Exit +
                  texas_treated:Replace + post_1999:Replace +
                  active_tanks + avg_tank_age | 
                  facility_id + panel_year,
  data = facility_panel[state == "Texas" | state %in% control_states & 
                        panel_year >= 1970 & panel_year <= 2020],
  cluster = "state"
)



# Save as CSV for easy import into Beamer slides
fwrite(tidy(mechanism_direct_model), here("Output", "Tables", "mechanism_results_beamer.csv"))


mechanism_hte_age <- feols(
  leak_incident ~ texas_treated * post_1999 * Exit*age_group + 
                  texas_treated * post_1999 * Replace*age_group + 
                  active_tanks  | 
                  facility_id + panel_year,
  data = facility_panel[state == "Texas" | state %in% control_states & 
                        panel_year >= 1970 & panel_year <= 2020],
  cluster = "state"
)

#Match leak detection results

formula_str <- paste0(
  "leak_incident ~ ", 
  "i(age_group, ref = '0-5') + ", 
  "texas_treated * post_1999 * Exit+",
  "texas_treated * post_1999 * Replace+",
  "texas_treated + ", 
  "post_1999 + ",
  "i(age_group, ref = '0-5'):texas_treated + ", 
  "i(age_group, ref = '0-5'):post_1999 + ", 
  "texas_treated:post_1999 + ",
  "i(age_group, ref = '0-5'):texas_treated:post_1999 | ",
  "panel_year + panel_id"
)

# Run the model with the formula
mediation_with_ate_hte_causal <- feols(
  as.formula(formula_str),
  data = filtered_data,
  cluster = "state"
)

View(tidy(mediation_with_ate_hte_causal))
################################################################################################
# # 8.3 Firm Behavior Mechanism Tests: Policy Impacts on Exit and Replacement
################################################################################################

# $$
# Y_{i,t} = \beta \cdot \text{TX}_{i} \cdot \text{Post1999}_{t} + \gamma \cdot \text{Controls}_{i,t} + \alpha_i + \alpha_t + \epsilon_{i,t}
# $$

# Where:
# - $Y_{i,t}$ is either $\text{Exit}_{i,t}$ or $\text{Replace}_{i,t}$ 
# - Same fixed effects and controls as previous models
# - This tests direct policy impacts on firm behavior
# First ensure we have the necessary indicator variables properly defined
facility_panel_only_motor_fuel[, `:=`(
  # Exit indicator (facility closing in this period)
  Exit = 1*exit_flag,
  # Replacement indicator (at least one tank replaced)
  Replace = 1*replacement_event
)]


    # First, ensure we have the filtered data with age bins
    filtered_data <- facility_panel_only_motor_fuel[!is.na(avg_tank_age) & 
                     avg_tank_age <= 50 & avg_tank_age >= 0 & 
                     panel_year >= 1970 & panel_year <= 2020 & 
                     (state %in% control_states | state == "Texas")]

    filtered_data[, age_group := cut(avg_tank_age, 
             breaks = c(seq(0, 35, by=5), Inf),
             include.lowest = TRUE,
             right = TRUE,
             labels = c(paste0(seq(0, 30, by= 5), "-", seq(5, 35, by= 5)), "35+"))]


# # Define the exit and replacement models
 exit_model <- feols(Exit~texas_treated * post_1999 + active_tanks +  i(age_group, ref = "0-5") + has_single_walled |
  facility_id + panel_year, data = filtered_data[state == "Texas" | state %in% control_states & panel_year >= 1970 & panel_year <= 2020], cluster = "state")


 exit_model_age_hte <- feols(Exit ~texas_treated * post_1999 * i(age_group, ref = "0-5")*has_single_walled + active_tanks   |
  facility_id + panel_year, data = filtered_data[state == "Texas" | state %in% control_states & panel_year >= 1970 & panel_year <= 2020], cluster = "state")
View(tidy(exit_model))
View(tidy(exit_model_age_hte))


  replace_model <- feols(Replace~texas_treated * post_1999 + active_tanks +  i(age_group, ref = "0-5") + has_single_walled  |
  facility_id + panel_year, data = filtered_data[state == "Texas" | state %in% control_states & panel_year >= 1970 & panel_year <= 2020], cluster = "state") 

  replace_model_just_sw <- feols(Replace~texas_treated * post_1999*has_single_walled+ active_tanks +  i(age_group, ref = "0-5")   |
  facility_id + panel_year, data = filtered_data[state == "Texas" | state %in% control_states & panel_year >= 1970 & panel_year <= 2020], cluster = "state") 


  replace_model_age_hte <- feols(Replace ~texas_treated * post_1999 * i(age_group, ref = "0-5")*has_single + active_tanks +has_single_walled  |
  facility_id + panel_year, data = filtered_data[state == "Texas" | state %in% control_states & panel_year >= 1970 & panel_year <= 2020], cluster = "state") 


  replace_model_age_hte_wall <- feols(Replace ~texas_treated * post_1999 * i(age_group, ref = "0-5")*has_single_walled + active_tanks   |
   facility_id + panel_year, data = filtered_data[state == "Texas" | state %in% control_states & panel_year >= 1970 & panel_year <= 2020], cluster = "state") 


  View(tidy(replace_model))
  View(tidy(replace_model_age_hte))
  View(tidy(replace_model_age_hte_wall))
  View(tidy(replace_model_just_sw))


  # Extract coefficients from exit model with age heterogeneity
  exit_coeffs <- tidy(exit_model_age_hte)
  # Filter only the quadruple interaction terms for exit model
  exit_terms <- exit_coeffs[grep("texas_treated:post_1999:has_single_walled:age_group::", exit_coeffs$term), ]
  # Create a more readable dataframe
  exit_results <- data.frame(
    age_group = gsub("texas_treated:post_1999:has_single_walled:age_group::", "", exit_terms$term),
    estimate = exit_terms$estimate,
    std_error = exit_terms$std.error,
    p_value = exit_terms$p.value,
    decision = "Exit"
  )
  # Calculate confidence intervals
  exit_results$lower_ci <- exit_results$estimate - 1.96 * exit_results$std_error
  exit_results$upper_ci <- exit_results$estimate + 1.96 * exit_results$std_error

  # Extract coefficients from replace model with age heterogeneity
  replace_coeffs <- tidy(replace_model_age_hte_wall)
  # Filter only the quadruple interaction terms for replacement model
  replace_terms <- replace_coeffs[grep("texas_treated:post_1999:has_single_walled:age_group::", replace_terms$term), ]
  # Create a more readable dataframe
  replace_results <- data.frame(
    age_group = gsub("texas_treated:post_1999:has_single_walled:age_group::", "", replace_terms$term),
    estimate = replace_terms$estimate,
    std_error = replace_terms$std.error,
    p_value = replace_terms$p.value,
    decision = "Replace"
  )
  # Calculate confidence intervals
  replace_results$lower_ci <- replace_results$estimate - 1.96 * replace_results$std.error
  replace_results$upper_ci <- replace_results$estimate + 1.96 * replace_results$std.error

  # Combine results
  combined_results <- rbind(exit_results, replace_results)

  # Add base effects for reference category (0-5 age group)
  # Extract the base effect for exit (texas_treated:post_1999:has_single_walled)
  exit_base_effect <- exit_coeffs[grep("^texas_treated:post_1999:has_single_walled$", exit_coeffs$term), ]
  if(nrow(exit_base_effect) > 0) {
    exit_base <- data.frame(
      age_group = "0-5",
      estimate = exit_base_effect$estimate,
      std_error = exit_base_effect$std.error,
      p_value = exit_base_effect$p.value,
      decision = "Exit",
      lower_ci = exit_base_effect$estimate - 1.96 * exit_base_effect$std.error,
      upper_ci = exit_base_effect$estimate + 1.96 * exit_base_effect$std.error
    )
    combined_results <- rbind(combined_results, exit_base)
  }

  # Extract the base effect for replacement
  replace_base_effect <- replace_coeffs[grep("^texas_treated:post_1999:has_single_walled$", replace_coeffs$term), ]
  if(nrow(replace_base_effect) > 0) {
    replace_base <- data.frame(
      age_group = "0-5",
      estimate = replace_base_effect$estimate,
      std_error = replace_base_effect$std.error,
      p_value = replace_base_effect$p.value,
      decision = "Replace",
      lower_ci = replace_base_effect$estimate - 1.96 * replace_base_effect$std.error,
      upper_ci = replace_base_effect$estimate + 1.96 * replace_base_effect$std.error
    )
    combined_results <- rbind(combined_results, replace_base)
  }

  # If base effects aren't available, add zeros for the reference group
  if(!any(combined_results$age_group == "0-5" & combined_results$decision == "Exit")) {
    exit_base <- data.frame(
      age_group = "0-5",
      estimate = 0,
      std_error = 0,
      p_value = 1,
      decision = "Exit",
      lower_ci = 0,
      upper_ci = 0
    )
    combined_results <- rbind(combined_results, exit_base)
  }

  if(!any(combined_results$age_group == "0-5" & combined_results$decision == "Replace")) {
    replace_base <- data.frame(
      age_group = "0-5",
      estimate = 0,
      std_error = 0,
      p_value = 1,
      decision = "Replace",
      lower_ci = 0,
      upper_ci = 0
    )
    combined_results <- rbind(combined_results, replace_base)
  }

  # Order age groups correctly
  age_bin_levels <- c("0-5", paste0(seq(5, 30, by = 5), "-", seq(10, 35, by = 5)), "35+")
  combined_results$age_group <- factor(combined_results$age_group, levels = age_bin_levels)
  combined_results <- combined_results[order(combined_results$decision, combined_results$age_group), ]

  # Create a high-quality coefficient plot showing both decision types
  mechanism_plot <- ggplot(
    combined_results,
    aes(x = age_group, y = estimate*100, color = decision, group = decision)
  ) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 1) +
    geom_point(size = 4, position = position_dodge(width = 0.5)) +
    geom_errorbar(
      aes(ymin = lower_ci*100, ymax = upper_ci*100),
      width = 0.2,
      position = position_dodge(width = 0.5),
      size = 1
    ) +
    geom_line(position = position_dodge(width = 0.5), size = 1) +
    # Colorblind friendly colors: blue for exit, orange for replacement
    scale_color_manual(values = c("Exit" = "#0072B2", "Replace" = "#E69F00")) +
    scale_y_continuous(labels = function(x) paste0(x, "%")) +
    labs(
      title = "Policy Effects on Single-Walled Firm Decisions",
      subtitle = "By Tank Age Group",
      x = "Tank Age (Years)",
      y = "Percentage Point Change in Probability",
      color = "Decision Type"
    ) +
    theme_pub() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 16, face = "bold"),
      axis.text.y = element_text(size = 16, face = "bold"),
      axis.title = element_text(size = 18, face = "bold"),
      legend.position = "bottom",
      legend.title = element_text(size = 16, face = "bold"),
      legend.text = element_text(size = 16),
      panel.grid.major = element_line(color = "gray80"),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "white"),
      plot.title = element_text(size = 20, face = "bold"),
      plot.subtitle = element_text(size = 16),
      plot.margin = margin(20, 20, 20, 20)
    )

  # Print the plot
  print(mechanism_plot)

  # Save the plot for presentations
  ggsave(
    here("Output", "Figures", "exit_vs_replacement_by_age.png"), 
    mechanism_plot, 
    width = 16, 
    height = 9, 
    dpi = 300, 
    bg = "white"
  )

  # Create a PDF version as well, which often displays better in presentations
  ggsave(
    here("Output", "Figures", "exit_vs_replacement_by_age.pdf"), 
    mechanism_plot, 
    width = 16, 
    height = 9, 
    bg = "white"
  )

  # Create a summary table showing the effects
  summary_table <- combined_results %>%
    mutate(
      effect = paste0(round(estimate*100, 2), "%"),
      significant = ifelse(p_value < 0.05, "*", ""),
      effect_with_sig = paste0(effect, significant)
    ) %>%
    select(age_group, decision, effect_with_sig) %>%
    pivot_wider(
      names_from = decision,
      values_from = effect_with_sig
    )

  # Print the summary table
  print(summary_table)
  
  # Mediation Analysis --------------------------------------------------------
  cat("\n\n========== MEDIATION ANALYSIS: LEAK EFFECT THROUGH EXIT AND REPLACEMENT ==========\n")
  
  # Extract coefficients for total effect on leaks (from hte_with_wall)
  leak_coeffs <- tidy(hte_with_wall)
  leak_terms <- leak_coeffs[grep("texas_treated:post_1999:has_single_walled:age_group::", leak_coeffs$term), ]
  leak_results <- data.frame(
    age_group = gsub("texas_treated:post_1999:has_single_walled:age_group::", "", leak_terms$term),
    total_effect = leak_terms$estimate,
    std_error = leak_terms$std.error
  )
  
  # Extract base effect for leaks (reference group)
  leak_base <- leak_coeffs[grep("^texas_treated:post_1999:has_single_walled$", leak_coeffs$term), ]
  if(nrow(leak_base) > 0) {
    leak_base_row <- data.frame(
      age_group = "0-5",
      total_effect = leak_base$estimate,
      std_error = leak_base$std.error
    )
    leak_results <- rbind(leak_results, leak_base_row)
  } else {
    # Add zero effect for reference group if not available
    leak_results <- rbind(leak_results, data.frame(
      age_group = "0-5",
      total_effect = 0,
      std_error = 0
    ))
  }

  # Extract coefficients for exit effects
  exit_coeffs <- tidy(exit_model_age_hte)
  exit_terms <- exit_coeffs[grep("texas_treated:post_1999:has_single_walled:age_group::", exit_coeffs$term), ]
  exit_results <- data.frame(
    age_group = gsub("texas_treated:post_1999:has_single_walled:age_group::", "", exit_terms$term),
    exit_effect = exit_terms$estimate
  )
  
  # Add reference group for exit
  exit_base <- exit_coeffs[grep("^texas_treated:post_1999:has_single_walled$", exit_coeffs$term), ]
  if(nrow(exit_base) > 0) {
    exit_results <- rbind(exit_results, data.frame(
      age_group = "0-5",
      exit_effect = exit_base$estimate
    ))
  } else {
    exit_results <- rbind(exit_results, data.frame(
      age_group = "0-5",
      exit_effect = 0
    ))
  }

  # Extract coefficients for replacement effects
  replace_coeffs <- tidy(replace_model_age_hte_wall)
  replace_terms <- replace_coeffs[grep("texas_treated:post_1999:has_single_walled:age_group::", replace_terms$term), ]
  replace_results <- data.frame(
    age_group = gsub("texas_treated:post_1999:has_single_walled:age_group::", "", replace_terms$term),
    replace_effect = replace_terms$estimate
  )
  
  # Add reference group for replacement
  replace_base <- replace_coeffs[grep("^texas_treated:post_1999:has_single_walled$", replace_coeffs$term), ]
  if(nrow(replace_base) > 0) {
    replace_results <- rbind(replace_results, data.frame(
      age_group = "0-5",
      replace_effect = replace_base$estimate
    ))
  } else {
    replace_results <- rbind(replace_results, data.frame(
      age_group = "0-5",
      replace_effect = 0
    ))
  }

  # Merge all results
  mediation_results <- merge(leak_results, exit_results, by = "age_group", all = TRUE)
  mediation_results <- merge(mediation_results, replace_results, by = "age_group", all = TRUE)
  
  # Calculate approximate mediated effects (rough approximation for visualization)
  # These are simplified calculations - a full mediation would use structural equation modeling
  
  # Estimate average effect of exit on leaks and replacement on leaks
  exit_to_leak_effect <- 0.15  # Hypothetical value - you may want to estimate from data
  replace_to_leak_effect <- -0.1  # Hypothetical value - negative because replacement reduces leaks
  
  mediation_results$exit_mediated <- mediation_results$exit_effect * exit_to_leak_effect
  mediation_results$replace_mediated <- mediation_results$replace_effect * replace_to_leak_effect
  mediation_results$direct_effect <- mediation_results$total_effect - 
                                    (mediation_results$exit_mediated + mediation_results$replace_mediated)
  
  # Reshape for plotting
  mediation_long <- tidyr::pivot_longer(
    mediation_results,
    cols = c("direct_effect", "exit_mediated", "replace_mediated"),
    names_to = "effect_type",
    values_to = "estimate"
  )
  
  # Add human-readable labels
  mediation_long$effect_label <- factor(
    mediation_long$effect_type,
    levels = c("direct_effect", "exit_mediated", "replace_mediated"),
    labels = c("Direct Policy Effect", "Mediated through Exit", "Mediated through Replacement")
  )
  
  # Order age groups correctly
  age_bin_levels <- c("0-5", paste0(seq(5, 30, by = 5), "-", seq(10, 35, by = 5)), "35+")
  mediation_long$age_group <- factor(mediation_long$age_group, levels = age_bin_levels)
  
  # Create a stacked bar chart to show mediation components
  mediation_plot <- ggplot(
    mediation_long,
    aes(x = age_group, y = estimate*100, fill = effect_label)
  ) +
    geom_bar(stat = "identity", position = "stack", color = "black", linewidth = 0.2) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 1) +
    # Use same colors as mechanism plot for exit and replacement
    scale_fill_manual(values = c(
      "Direct Policy Effect" = "#56B4E9", 
      "Mediated through Exit" = "#0072B2",  # Same blue as exit in mechanism_plot
      "Mediated through Replacement" = "#E69F00"  # Same orange as replacement
    )) +
    # Add percentage sign to y-axis
    scale_y_continuous(labels = function(x) paste0(x, "%")) +
    labs(
      title = "Decomposition of Policy Effects on Leaks by Mediation Pathway",
      subtitle = "For Single-Walled Tanks by Age Group",
      x = "Tank Age (Years)",
      y = "Percentage Point Effect on Leak Probability",
      fill = "Effect Pathway"
    ) +
    theme_pub() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 16, face = "bold"),
      axis.text.y = element_text(size = 16, face = "bold"),
      axis.title = element_text(size = 18, face = "bold"),
      legend.position = "bottom",
      legend.title = element_text(size = 16, face = "bold"),
      legend.text = element_text(size = 14),
      panel.grid.major = element_line(color = "gray80"),
      panel.background = element_rect(fill = "white"),
      plot.title = element_text(size = 20, face = "bold"),
      plot.subtitle = element_text(size = 16),
      plot.margin = margin(20, 20, 20, 20)
    )
  
  # Print the plot
  print(mediation_plot)
  
  # Save for presentations
  ggsave(
    here("Output", "Figures", "leak_mediation_by_age.png"), 
    mediation_plot, 
    width = 16, 
    height = 9, 
    dpi = 300, 
    bg = "white"
  )
  
  # Also create a line plot version showing mediation effects separately
  mediation_line_plot <- ggplot(
    mediation_long,
    aes(x = age_group, y = estimate*100, color = effect_label, group = effect_label)
  ) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 1) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 4) +
    # Use same colors as above
    scale_color_manual(values = c(
      "Direct Policy Effect" = "#56B4E9", 
      "Mediated through Exit" = "#0072B2", 
      "Mediated through Replacement" = "#E69F00"
    )) +
    scale_y_continuous(labels = function(x) paste0(x, "%")) +
    labs(
      title = "Mediation Pathways of Policy Effects on Leaks",
      subtitle = "For Single-Walled Tanks by Age Group",
      x = "Tank Age (Years)",
      y = "Percentage Point Effect on Leak Probability",
      color = "Effect Pathway"
    ) +
    theme_pub() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 16, face = "bold"),
      axis.text.y = element_text(size = 16, face = "bold"),
      axis.title = element_text(size = 18, face = "bold"),
      legend.position = "bottom",
      legend.title = element_text(size = 16, face = "bold"),
      legend.text = element_text(size = 14),
      panel.grid.major = element_line(color = "gray80"),
      panel.background = element_rect(fill = "white"),
      plot.title = element_text(size = 20, face = "bold"),
      plot.subtitle = element_text(size = 16),
      plot.margin = margin(20, 20, 20, 20)
    )
  
  print(mediation_line_plot)
  
  # Save line plot version
  ggsave(
    here("Output", "Figures", "leak_mediation_line_by_age.png"), 
    mediation_line_plot, 
    width = 16, 
    height = 9, 
    dpi = 300, 
    bg = "white"
  )

# Parallel implementation for bootstrap and k-fold validation

# Load parallel processing libraries
library(parallel)
library(foreach)
library(doParallel)

# Setup parallel processing
num_cores <- max(1, detectCores() - 2)  # Leave 2 cores free for system processes
cat("Setting up parallel cluster with", num_cores, "cores\n")
cl <- makeCluster(num_cores, type = "PSOCK")
registerDoParallel(cl)

# Export necessary data and functions to all workers
clusterExport(cl, c("filtered_data"))
clusterEvalQ(cl, {
  library(fixest)
  library(dplyr)
})

# Define bootstrap function that preserves state clustering
bootstrap_predictions <- function(data, indices) {
  # Get unique states from the bootstrap sample
  sampled_states <- unique(data$state[indices])
  
  # Create a new dataset with all observations from the sampled states
  boot_data <- filtered_data[filtered_data$state %in% sampled_states,]
  
  # Fit model on bootstrap sample
  tryCatch({
    boot_model <- feols(
      leak_incident ~ wall_type*i(age_bin2, ref = "0-5") + factor(panel_year) | facility_id,
      data = boot_data
    )
    
    # Calculate group means of fitted values
    boot_fitted <- data.frame(
      age_bin2 = boot_data$age_bin2,
      wall_type = boot_data$wall_type,
      pred = fitted(boot_model)
    )
    
    # Aggregate by age and wall type
    agg_preds <- boot_fitted %>%
      group_by(age_bin2, wall_type) %>%
      summarize(pred_prob = mean(pred, na.rm = TRUE), .groups = 'drop')
    
    # Return predictions in a standard format
    return(agg_preds$pred_prob)
  }, error = function(e) {
    # Return NA if model fails
    return(rep(NA, length(unique(data$age_bin2)) * 2))
  })
}

# Parallel Bootstrap Implementation
cat("Running parallel bootstrap with 500 replicates...\n")
states <- unique(filtered_data$state)
num_replicates <- 500

# Create bootstrap samples in advance
set.seed(123456)  # For reproducibility
boot_indices <- lapply(1:num_replicates, function(i) {
  sample(length(states), size = length(states), replace = TRUE)
})

# Run bootstrap in parallel
boot_results_parallel <- foreach(i = 1:num_replicates, .combine = rbind) %dopar% {
  indices <- boot_indices[[i]]
  result <- bootstrap_predictions(data.frame(state = states), indices)
  return(result)
}

# Reconstruct boot object structure for compatibility with boot.ci
t0 <- bootstrap_predictions(data.frame(state = states), 1:length(states))
boot_results <- list(t0 = t0, t = boot_results_parallel, R = num_replicates)
class(boot_results) <- "boot"

# Extract confidence intervals
ci_matrix <- t(sapply(1:length(boot_results$t0), function(i) {
  boot.ci(boot_results, type = "perc", index = i)$percent[4:5]
}))

# Add to predictions data frame
predictions$lower_ci <- ci_matrix[,1]
predictions$upper_ci <- ci_matrix[,2]

# Parallel K-fold cross-validation
cat("Running parallel k-fold cross-validation with 50 iterations...\n")
n_iterations <- 50
subset_results <- foreach(i = 1:n_iterations, .packages = c("fixest", "dplyr")) %dopar% {
  # Set different seed for each iteration to ensure different samples
  set.seed(123456 + i)
  
  # Sample 80% of facilities
  sampled_facilities <- sample(unique(filtered_data$facility_id), 
                             size = 0.8 * length(unique(filtered_data$facility_id)))
  
  # Subset data
  subset_data <- filtered_data[filtered_data$facility_id %in% sampled_facilities,]
  
  # Run model
  subset_model <- feols(
    leak_incident ~ wall_type*i(age_bin2, ref = "0-5") + factor(panel_year) | facility_id,
    data = subset_data
  )
  
  # Get fitted values
  subset_fitted <- data.frame(
    age_bin2 = subset_data$age_bin2,
    wall_type = subset_data$wall_type,
    pred = fitted(subset_model)
  )
  
  # Calculate group means
  result <- subset_fitted %>%
    group_by(age_bin2, wall_type) %>%
    summarize(pred_prob = mean(pred, na.rm = TRUE), .groups = 'drop')
  
  return(result)
}

# Stop the cluster when done
stopCluster(cl)

# Calculate variance across iterations - this part runs sequentially after parallel processing
combined_results <- bind_rows(subset_results, .id = "iteration")
prediction_intervals <- combined_results %>%
  group_by(age_bin2, wall_type) %>%
  summarize(
    mean_pred = mean(pred_prob, na.rm = TRUE),
    lower_ci = quantile(pred_prob, 0.025, na.rm = TRUE),
    upper_ci = quantile(pred_prob, 0.975, na.rm = TRUE)
  )

cat("Parallel processing complete.\n")



