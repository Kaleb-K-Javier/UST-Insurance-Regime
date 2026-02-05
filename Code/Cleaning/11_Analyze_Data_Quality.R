## 11_Analyze_Data_Quality_Tables.R
## Purpose: 
## 1. Load Master Harmonized datasets.
## 2. Generate "Wide-Form" frequency tables (State x Decade) for missingness.
## 3. Specific audit for "Pre-1999" cohort.
## 4. Audit Missing Closure Dates.
## 5. Save outputs as both CSV (for data) and TXT (for easy sharing/viewing).

library(here)
library(data.table)
library(tidyverse)
library(lubridate)
library(knitr) # Required for text table formatting

# 0. Setup ---------------------------------------------------------------------

processed_dir <- here("Data", "Processed")
output_dir    <- here("Output","Tables", "Data_Quality_Tables")

if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# --- Helper: Save Text Table ---
save_txt_table <- function(data, filename, title) {
  # Create full path
  file_path <- file.path(output_dir, filename)
  
  # Format the table using kable (Markdown pipe format is very readable)
  tbl_formatted <- kable(data, format = "pipe", align = "l")
  
  # Write Header and Table to file
  cat(paste0("================================================================================\n"), file = file_path)
  cat(paste0(title, "\n"), file = file_path, append = TRUE)
  cat(paste0("Generated: ", Sys.time(), "\n"), file = file_path, append = TRUE)
  cat(paste0("================================================================================\n\n"), file = file_path, append = TRUE)
  cat(tbl_formatted, file = file_path, append = TRUE, sep = "\n")
  
  message(paste0("  Saved TXT: ", filename))
}

# 1. Load Data -----------------------------------------------------------------

message("Loading Master Datasets...")
tanks <- fread(file.path(processed_dir, "Master_Harmonized_UST_Tanks.csv"))
lusts <- fread(file.path(processed_dir, "Master_Harmonized_LUST.csv"))

# 2. Pre-Processing & Cohort Definitions ---------------------------------------

message("Flagging Missingness and Defining Cohorts...")

# --- A. Tanks Processing ---
tanks[, `:=`(
  install_year = year(tank_installed_date),
  # Decade Binning (e.g., 1980 -> "1980s")
  decade = floor(year(tank_installed_date) / 10) * 10
)]

# Flagging specific missingness types
tanks[, `:=`(
  missing_wall_type = as.integer(is.na(single_walled) | (unknown_walled == 1) | (single_walled == 0 & double_walled == 0)),
  missing_substance = as.integer(is_gasoline == 0 & is_diesel == 0 & is_oil_kerosene == 0 & is_jet_fuel == 0 & is_other == 0),
  missing_capacity  = as.integer(is.na(capacity) | capacity == 0),
  missing_dates     = as.integer(is.na(tank_installed_date)),
  
  # NEW: Closure Date Missingness
  # Only relevant if tank_status is "Closed"
  missing_closure_date = ifelse(tank_status == "Closed" & is.na(tank_closed_date), 1, 0)
)]

# Define Samples
# 1. Main Sample: 1980 - 2023
sample_main <- tanks[install_year >= 1980 & install_year <= 2023]

# 2. Closed Sample: Only closed tanks in main sample (for closure audit)
sample_closed <- sample_main[tank_status == "Closed"]

# 3. Pre-1999 Sample: All tanks installed before 1999 (The "Old Fleet")
sample_pre99 <- tanks[install_year < 1999]

# --- B. LUST Processing ---
lusts[, `:=`(
  report_year = year(report_date),
  decade = floor(year(report_date) / 10) * 10,
  missing_nfa = as.integer(is.na(nfa_date))
)]

# LUST Sample: 1980 - 2023
lust_main <- lusts[report_year >= 1980 & report_year <= 2023]

# 3. Helper Function: Create Wide Frequency Tables -----------------------------

create_wide_table <- function(data, group_col, value_col) {
  # 1. Summarize
  summary_dt <- data[, .(
    pct_issue = round(mean(get(value_col), na.rm = TRUE) * 100, 1)
  ), by = c("state", group_col)]
  
  # 2. Pivot Wide (State x Group)
  wide_dt <- dcast(summary_dt, state ~ get(group_col), value.var = "pct_issue")
  
  # 3. Order rows by the most recent decade/group
  if(ncol(wide_dt) > 1) {
    last_col <- names(wide_dt)[ncol(wide_dt)]
    setorderv(wide_dt, last_col, -1) # Descending sort
  }
  
  return(wide_dt)
}

# 4. Generate Main Sample Tables (1980-2023) -----------------------------------

message("Generating Main Sample Tables (State x Decade)...")

# A. Wall Type Missingness
tbl_wall <- create_wide_table(sample_main, "decade", "missing_wall_type")
fwrite(tbl_wall, file.path(output_dir, "01_Main_Missing_Wall_by_Decade.csv"))
save_txt_table(tbl_wall, "01_Main_Missing_Wall_by_Decade.txt", "Missing Wall Type % by Decade (1980-2023)")

# B. Substance Missingness
tbl_subst <- create_wide_table(sample_main, "decade", "missing_substance")
fwrite(tbl_subst, file.path(output_dir, "02_Main_Missing_Substance_by_Decade.csv"))
save_txt_table(tbl_subst, "02_Main_Missing_Substance_by_Decade.txt", "Missing Substance % by Decade (1980-2023)")

# C. Capacity Missingness
tbl_cap <- create_wide_table(sample_main, "decade", "missing_capacity")
fwrite(tbl_cap, file.path(output_dir, "03_Main_Missing_Capacity_by_Decade.csv"))
save_txt_table(tbl_cap, "03_Main_Missing_Capacity_by_Decade.txt", "Missing Capacity % by Decade (1980-2023)")

# D. LUST NFA Missingness (Open Cases or Bad Data?)
tbl_lust <- create_wide_table(lust_main, "decade", "missing_nfa")
fwrite(tbl_lust, file.path(output_dir, "04_LUST_No_NFA_Date_by_Decade.csv"))
save_txt_table(tbl_lust, "04_LUST_No_NFA_Date_by_Decade.txt", "Missing NFA Date % by Decade (Leak Reports 1980-2023)")

# E. Missing Closure Date (Conditional on being Closed)
tbl_close <- create_wide_table(sample_closed, "decade", "missing_closure_date")
fwrite(tbl_close, file.path(output_dir, "06_Main_Missing_Closure_Date_by_Decade.csv"))
save_txt_table(tbl_close, "06_Main_Missing_Closure_Date_by_Decade.txt", "Missing Closure Date % (For Closed Tanks Only)")

# 5. Generate Pre-1999 Cohort Summary ------------------------------------------

message("Generating Pre-1999 Cohort Analysis...")

pre99_stats <- sample_pre99[, .(
  Total_Tanks_Pre99 = .N,
  Pct_Ms_Wall  = round(mean(missing_wall_type) * 100, 1),
  Pct_Ms_Subst = round(mean(missing_substance) * 100, 1),
  Pct_Ms_Cap   = round(mean(missing_capacity) * 100, 1),
  Pct_Ms_Date  = round(mean(missing_dates) * 100, 1),
  Pct_Ms_Close = round(mean(ifelse(tank_status=="Closed", missing_closure_date, NA), na.rm=TRUE) * 100, 1)
), by = state]

# Sort by Closure Date Missingness
setorder(pre99_stats, -Pct_Ms_Close)

fwrite(pre99_stats, file.path(output_dir, "05_Pre1999_Cohort_Quality.csv"))
save_txt_table(pre99_stats, "05_Pre1999_Cohort_Quality.txt", "Data Quality for Tanks Installed Pre-1999 (The Old Fleet)")

# 6. Global Summary (Quick View) -----------------------------------------------

cat("\n--- QUALITY SNAPSHOT: PRE-1999 COHORT ---\n")
print(head(pre99_stats[, .(state, Total_Tanks_Pre99, Pct_Ms_Close, Pct_Ms_Wall)], 15))

message(paste0("\nAll CSV and TXT tables saved to: ", output_dir))