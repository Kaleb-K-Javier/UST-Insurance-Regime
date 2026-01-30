# ==============================================================================
# 99_EPA_Raw_Probe.R
# Purpose: Deep dive into the RAW EPA data for non-custom states.
#          1. Generates detailed log of raw values (EPA_Raw_Probe_Log.txt)
#          2. Generates a summary ASCII table of missingness (EPA_Data_Summary_Table.txt)
# ==============================================================================

library(data.table)
library(tidyverse)
library(here)
library(janitor)
library(lubridate)
library(knitr) # For nice ASCII tables

# 1. Setup & Load --------------------------------------------------------------
# Custom states to EXCLUDE (Using both Full Names and Abbr)
custom_states <- c("Alabama", "Arkansas", "Louisiana", "Maine", "Michigan", 
                   "New Jersey", "New Mexico", "Oklahoma", "Tennessee", "Texas",
                   "AL", "AR", "LA", "ME", "MI", "NJ", "NM", "OK", "TN", "TX")

raw_dir <- here("Data", "Raw")
log_file <- here("Data", "Processed", "EPA_Raw_Probe_Log.txt")
table_file <- here("Data", "Processed", "EPA_Data_Summary_Table.txt")

# Load Raw Files (Character class to see true raw values)
message("Loading Raw EPA Files...")
usts_raw <- fread(file.path(raw_dir, "USTs.csv"), colClasses = "character") %>% clean_names()
facs_raw <- fread(file.path(raw_dir, "Facilities.csv"), colClasses = "character") %>% clean_names()
lust_raw <- fread(file.path(raw_dir, "Releases.csv"), colClasses = "character") %>% clean_names()

# 2. Filter for Target States --------------------------------------------------
all_states <- unique(usts_raw$state)
target_states <- setdiff(all_states, custom_states)
target_states <- target_states[!is.na(target_states) & target_states != ""]
target_states <- sort(target_states)

message(paste0("Analyzing ", length(target_states), " Standard EPA States..."))

# Subset Data
ust_tgt_all <- usts_raw[state %in% target_states]
fac_tgt_all <- facs_raw[state %in% target_states]
lust_tgt_all <- lust_raw[state %in% target_states]

# Initialize Summary Collector
summary_list <- list()

# 3. Helper Functions ----------------------------------------------------------

# Calc % Missing
pct_miss <- function(x) {
  if (length(x) == 0) return(100)
  m <- sum(x == "" | is.na(x))
  return(round(m / length(x) * 100, 0))
}

print_freq <- function(dt, col, label) {
  cat(paste0("\n  [", label, "]\n"))
  if(col %in% names(dt)) {
    freq <- dt[, .N, by = get(col)][order(-N)]
    print(head(freq, 10), row.names = FALSE)
    cat(sprintf("  >> Missing: %s%%\n", pct_miss(dt[[col]])))
  } else {
    cat("  [!] Column not found.\n")
  }
}

# 4. Generate Detailed Log & Collect Stats -------------------------------------
sink(log_file)
cat(paste0(strrep("=", 80), "\n"))
cat("RAW EPA DATA PROBE LOG (Detailed State Views)\n")
cat("Generated: ", as.character(Sys.time()), "\n")
cat(paste0(strrep("=", 80), "\n\n"))

# --- STATE LOOP ---
for (st in target_states) {
  cat("\n\n")
  cat(strrep("-", 80), "\n")
  cat(paste0("STATE: ", st, "\n"))
  cat(strrep("-", 80), "\n")
  
  # Subset
  ust_st <- ust_tgt_all[state == st]
  fac_st <- fac_tgt_all[state == st]
  lust_st <- lust_tgt_all[state == st]
  
  # --- COLLECT SUMMARY STATS ---
  
  # ID Format (Sample first non-empty ID)
  id_samp <- head(ust_st$facility_id[ust_st$facility_id != ""], 1)
  if(length(id_samp)==0) id_samp <- "NONE"
  
  # Geography (Use Facs file)
  ll_miss <- if(nrow(fac_st)>0) pct_miss(fac_st$latitude) else 100
  co_miss <- if(nrow(fac_st)>0) pct_miss(fac_st$county) else 100
  zip_miss <- if(nrow(fac_st)>0) pct_miss(fac_st$zip_code) else 100
  
  # Attributes
  wall_miss <- pct_miss(ust_st$tank_wall_type)
  sub_miss  <- pct_miss(ust_st$substances)
  stat_miss <- pct_miss(ust_st$tank_status)
  
  # Dates (UST)
  inst_miss <- pct_miss(ust_st$installation_date)
  rem_miss  <- pct_miss(ust_st$removal_date)
  
  # Dates (LUST) - NFA is tricky, looks for closed/nfa
  rep_miss <- if(nrow(lust_st)>0) pct_miss(lust_st$reported_date) else 100
  
  # Try to find NFA column
  nfa_col <- grep("closed|nfa|status_date", names(lust_st), value = TRUE)[1]
  nfa_miss <- if(!is.na(nfa_col) && nrow(lust_st)>0) pct_miss(lust_st[[nfa_col]]) else 100
  
  # Match Rate
  common <- length(intersect(ust_st$facility_id, fac_st$facility_id))
  match_rt <- if(nrow(ust_st)>0) round(common/nrow(ust_st)*100, 0) else 0
  
  # Store
  summary_list[[st]] <- data.table(
    State = st,
    ID_Example = id_samp,
    `LL%` = ll_miss,
    `Cnty%` = co_miss,
    `Zip%` = zip_miss,
    `Wall%` = wall_miss,
    `Sub%` = sub_miss,
    `Stat%` = stat_miss,
    `Inst%` = inst_miss,
    `Clsd%` = rem_miss,
    `L_Rep%` = rep_miss,
    `L_NFA%` = nfa_miss,
    `Match%` = match_rt
  )
  
  # --- PRINT DETAILED LOG ---
  cat(sprintf("Records: UST (%s), Facility (%s), LUST (%s)\n", 
              format(nrow(ust_st), big.mark=","), 
              format(nrow(fac_st), big.mark=","), 
              format(nrow(lust_st), big.mark=",")))
  
  cat("\n[Data Samples]\n")
  if(nrow(ust_st)>0) print(head(ust_st[, .(facility_id, installation_date, removal_date, tank_wall_type)], 3))
  
  cat("\n[Attribute Frequencies]\n")
  print_freq(ust_st, "tank_wall_type", "Tank Wall")
  print_freq(ust_st, "substances", "Substance")
  print_freq(ust_st, "tank_status", "Status")
  
  cat(sprintf("\n[Date Missingness]\n  Install: %s%%\n  Removal: %s%%\n  LUST Rep: %s%%\n  LUST NFA: %s%%\n", 
              inst_miss, rem_miss, rep_miss, nfa_miss))
  
  cat(sprintf("\n[Geography Missingness]\n  Lat/Long: %s%%\n  County: %s%%\n  Zip: %s%%\n", 
              ll_miss, co_miss, zip_miss))
}
sink()

# 5. Generate & Save ASCII Summary Table ---------------------------------------

# Combine list to data.table
summary_table <- rbindlist(summary_list)

# Generate Table Text
sink(table_file)
cat("=================================================================================================================\n")
cat("EPA RAW DATA MISSINGNESS SUMMARY (Standard States)\n")
cat("LL% = Missing Lat/Long | Wall% = Missing Wall Type | Inst% = Missing Install Date | Match% = UST IDs found in Facility File\n")
cat("=================================================================================================================\n\n")

# Use kable for nice ASCII formatting
print(kable(summary_table, format = "simple", align = "lcccccccccccc"))

cat(paste0("\n\n(Generated: ", Sys.time(), ")"))
sink()

message("---------------------------------------------------------")
message("1. Detailed Log:   ", log_file)
message("2. Summary Table:  ", table_file)
message("---------------------------------------------------------")