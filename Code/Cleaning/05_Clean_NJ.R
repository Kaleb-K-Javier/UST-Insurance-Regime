## New Jersey UST Dataset Creation

# Define the county variable name in the raw data
COUNTYVARIABLE <- "county"

first_run = TRUE
# load packages ----
library(tidyverse)
library(data.table)
library(here)
library(lubridate)
library(janitor)
library(ggplot2)
library(knitr)
library(kableExtra)
library(fixest)
library(ggpubr)
library(gghighlight)
library(did)
library(fastDummies)
library(fplot)
library(ggrepel)
library(patchwork)
library(DRDID)

# Function to standardize county names
standardize_county_name <- function(county_name) {
  if(is.na(county_name) || county_name == "") return(NA)
  name <- tolower(county_name)
  name <- gsub(" county$| counties$| parish$| parishes$", "", name)
  name <- gsub("[^a-z ]", "", name)
  name <- trimws(gsub("\\s+", " ", name))
  return(name)
}

# Function to classify tank wall types
classify_tank_walls <- function(data, tank_wall_col = "tank_structure") {
  result <- copy(data)
  result[, `:=`(
    double_walled = as.integer(grepl("Double Wall|Secondary Containment|Triple Wall", get(tank_wall_col), ignore.case = TRUE)),
    single_walled = as.integer(grepl("Single Wall", get(tank_wall_col), ignore.case = TRUE)),
    unknown_walled = as.integer(is.na(get(tank_wall_col)) | get(tank_wall_col) == ""),
    missing_walled = as.integer(is.na(get(tank_wall_col)))
  )]
  return(result)
}

# Function to classify substances
classify_substances <- function(data, substance_col_name = "tank_contents") {
  result <- copy(data)
  result[, `:=`(is_gasoline = 0, is_diesel = 0, is_oil_kerosene = 0, is_jet_fuel = 0, is_other = 1)]
  gasoline_patterns <- "gasoline|unleaded|premium|regular|ethanol|e85"
  diesel_patterns <- "diesel"
  oil_kerosene_patterns <- "oil|kerosene|waste oil"
  jet_fuel_patterns <- "jet|aviation"
  
  result[, substances_lower := tolower(get(substance_col_name))]
  result[!is.na(substances_lower), is_gasoline := as.integer(grepl(gasoline_patterns, substances_lower))]
  result[!is.na(substances_lower), is_diesel := as.integer(grepl(diesel_patterns, substances_lower))]
  result[!is.na(substances_lower), is_oil_kerosene := as.integer(grepl(oil_kerosene_patterns, substances_lower))]
  result[!is.na(substances_lower), is_jet_fuel := as.integer(grepl(jet_fuel_patterns, substances_lower))]
  result[, is_other := as.integer(is_gasoline + is_diesel + is_oil_kerosene + is_jet_fuel == 0)]
  result[, substances_lower := NULL]
  return(result)
}

# Function to standardize capacity
standardize_capacity <- function(data, capacity_col = "tank_volume") {
  result <- copy(data)
  result[, `:=`(capacity = as.numeric(gsub("[^0-9.]", "", get(capacity_col))))]
  return(result)
}

# Load Raw Data - New Jersey files -----
cat("\nLoading New Jersey UST data...\n")
NJ_tanks_raw <- fread(here("Data","Raw","state_databases","New Jersey","New Jersey Tanks Data.csv"), 
                      colClasses = c(preferred_id="character", tank_number="character")) %>%
  janitor::clean_names() %>% as.data.table()

NJ_facility_raw <- fread(here("Data","Raw","state_databases","New Jersey","New Jersey Facility Data.csv"), 
                         colClasses = c(preferred_id="character")) %>%
  janitor::clean_names() %>% as.data.table()

NJ_lust_raw <- fread(here("Data","Raw","Releases.csv")) %>%
  janitor::clean_names() %>%
  filter(state == "New Jersey") %>%
  as.data.table()

# Process Tank Data -----
cat("\nProcessing New Jersey tank data...\n")
NJ_tanks_raw[, state:= "New Jersey"]
NJ_tanks_raw[, state_abbr := "NJ"]

# Join facility data
NJ_UST_tanks <- NJ_tanks_raw %>%
  left_join(
    NJ_facility_raw %>% select(preferred_id, county, facility_name),
    by = "preferred_id"
  ) %>%
  as.data.table()

# Process tanks based on status description (Specific Logic)
cat("\nProcessing tank status...\n")
NJ_UST_tanks[, tank_status := case_when(
  tolower(tank_status_description) == "in-use" ~ "Open",
  tolower(tank_status_description) %in% c("abandoned in place", "out of service", "removed") ~ "Closed",
  tolower(tank_status_description) %in% c("emergency backup generator tank", "other", "pending-noi", "sump") ~ "Ignore",
  TRUE ~ NA_character_
)]
NJ_UST_tanks <- NJ_UST_tanks[tank_status != "Ignore"]

# Rename
NJ_UST_tanks <- NJ_UST_tanks %>%
  rename(
    facility_id = preferred_id,
    tank_id = tank_number,
    tank_installed_date = installed_date,
    tank_closed_date = out_of_service_date,
    county_name = county
  ) %>%
  as.data.table()

# Convert dates
NJ_UST_tanks[, tank_installed_date := as.Date(tank_installed_date, format = "%m/%d/%Y")]
NJ_UST_tanks[, tank_closed_date := as.Date(tank_closed_date, format = "%m/%d/%Y")]

# Classify
NJ_UST_tanks <- standardize_capacity(NJ_UST_tanks, "tank_volume")
NJ_UST_tanks <- classify_tank_walls(NJ_UST_tanks, "tank_structure")
NJ_UST_tanks <- classify_substances(NJ_UST_tanks, "tank_contents")

# Process LUST Data -----
cat("\nProcessing New Jersey LUST data...\n")
NJ_LUST_SD <- NJ_lust_raw %>%
  mutate(
    facility_id = as.character(gsub("^NJ", "", facility_id)),
    LUST_id = as.character(lust_id),
    report_date = as.Date(mdy_hm(reported_date)),
    nfa_date = as.Date(NA),
    state = "NJ"
  ) %>%
  select(facility_id, LUST_id, report_date, nfa_date, state) %>%
  as.data.table()

# Join
NJ_LUST_SD_collapsed <- NJ_LUST_SD[, .(LUST_count = .N), by = .(facility_id, report_date,nfa_date)]
NJ_UST_tanks_LUST <- merge(
  NJ_UST_tanks, 
  NJ_LUST_SD_collapsed[, .(facility_id,LUST_count, report_date, nfa_date)],
  by = "facility_id",
  all.x = TRUE,
  allow.cartesian = TRUE
)

# Indicators
NJ_UST_tanks_LUST <- NJ_UST_tanks_LUST %>%
  mutate(
    leak_after_closure = ifelse(!is.na(tank_closed_date) & !is.na(report_date) & report_date > tank_closed_date, 1, 0),
    leak_before_NFA_before_closure = ifelse(!is.na(tank_closed_date) & !is.na(report_date) & report_date < tank_closed_date & (is.na(nfa_date) | nfa_date < tank_closed_date), 1, 0),
    leak_before_NFA_after_closure = ifelse(!is.na(tank_closed_date) & !is.na(report_date) & report_date < tank_closed_date & !is.na(nfa_date) & nfa_date > tank_closed_date, 1, 0),
    no_leak = ifelse(is.na(report_date) & !is.na(tank_closed_date), 1, 0)
  ) %>%
  as.data.table()

# Aggregate
NJ_UST_tanks_SD <- NJ_UST_tanks_LUST %>%
  group_by(facility_id, tank_id, tank_installed_date, tank_closed_date, county_name, state, state_abbr) %>%
  summarize(
    facility_name = first(facility_name),
    tank_status = first(tank_status), # Preserve Specific Logic
    leak_after_closure = sum(leak_after_closure, na.rm = TRUE),
    leak_before_NFA_before_closure = sum(leak_before_NFA_before_closure, na.rm = TRUE),
    leak_before_NFA_after_closure = sum(leak_before_NFA_after_closure, na.rm = TRUE),
    no_leak = sum(no_leak, na.rm = TRUE),
    is_gasoline = max(is_gasoline, na.rm = TRUE),
    is_diesel = max(is_diesel, na.rm = TRUE),
    is_oil_kerosene = max(is_oil_kerosene, na.rm = TRUE),
    is_jet_fuel = max(is_jet_fuel, na.rm = TRUE),
    is_other = max(is_other, na.rm = TRUE),
    single_walled = max(single_walled, na.rm = TRUE),
    double_walled = max(double_walled, na.rm = TRUE),
    unknown_walled = max(unknown_walled, missing_walled, na.rm = TRUE),
    capacity = mean(capacity, na.rm = TRUE)
  ) %>%
  as.data.table()

# Final Clean
NJ_UST_tanks_SD[, `:=`(
  leak_after_closure = ifelse(leak_after_closure > 0, 1, 0),
  leak_before_NFA_before_closure = ifelse(leak_before_NFA_before_closure > 0, 1, 0),
  leak_before_NFA_after_closure = ifelse(leak_before_NFA_after_closure > 0, 1, 0),
  no_leak = ifelse(no_leak > 0, 1, 0),
  facility_name = stringr::str_to_title(trimws(gsub("[^[:alnum:] ]", "", facility_name))),
  facility_id = as.character(facility_id),
  tank_id = as.character(tank_id)
)]

# Output
output_dir <- here("Data","Raw","state_databases","New Jersey")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
fwrite(NJ_UST_tanks_SD, file.path(output_dir, "NJ_Harmonized_UST_tanks.csv"))
fwrite(NJ_LUST_SD, file.path(output_dir, "NJ_Harmonized_LUST.csv"))