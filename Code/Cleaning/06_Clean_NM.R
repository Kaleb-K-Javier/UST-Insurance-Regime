# New Mexico Dataset Creation

# Define the county variable name in the raw data
COUNTYVARIABLE <- "county"

library(here)
first_run = TRUE
#load packags ----
source(here('Code','00_global_packages.R'))
library(readxl)

# Add the substance classification function from EPA script
classify_substances <- function(data, substance_col_name = "substances") {
  result <- copy(data)
  result[, `:=`(is_gasoline = 0, is_diesel = 0, is_oil_kerosene = 0, is_jet_fuel = 0, is_other = 1)]
  # ... [Patterns same as provided] ...
  gasoline_patterns <- "gasoline|unleaded|petrol"
  diesel_patterns <- "diesel"
  oil_kerosene_patterns <- "oil|kerosene"
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

classify_tank_walls <- function(data, tank_wall_col = "tank_wall_type") {
  result <- copy(data)
  result[, `:=`(
    double_walled = as.integer(grepl("Double Walled", get(tank_wall_col), ignore.case = TRUE)),
    single_walled = as.integer(grepl("Single Walled", get(tank_wall_col), ignore.case = TRUE)),
    unknown_walled = as.integer(is.na(get(tank_wall_col)) | get(tank_wall_col) == "" | grepl("Unknown", get(tank_wall_col), ignore.case = TRUE)),
    missing_walled = as.integer(is.na(get(tank_wall_col)) | get(tank_wall_col) == "")
  )]
  return(result)
}

standardize_capacity <- function(data, capacity_col = "capacity") {
  result <- copy(data)
  result[, `:=`(capacity = as.numeric(gsub("[^0-9.]", "", get(capacity_col))))]
  return(result)
}

# Load Data - Force Character IDs
ust_data_epa_01 = fread(here("Data","Raw","USTs.csv"), 
                        colClasses = c(facility_id="character", tank_id="character")) %>% clean_names() %>% as.data.table()

epa_data <- fread(here("Data","Raw","","Facilities.csv"), 
                  colClasses = c(facility_id="character")) %>% clean_names() %>% as.data.table()

# Join
ust_data_epa_01 <- dplyr::left_join(
  ust_data_epa_01, 
  epa_data[, c("facility_id", "county", "facility_name")], 
  by = "facility_id"
) %>% 
  as.data.table()

NM_UST = ust_data_epa_01[grepl('New Mexico',state)]

# Select and Clean IDs (Remove NM prefix if present, but keep as char)
NM_UST_tanks = NM_UST %>% clean_names() %>%
    select(facility_id, facility_name, tank_id, installation_date, removal_date, tank_status, county,
           substances, tank_wall_type, capacity) %>%
    rename(tank_installed_date = installation_date,
           tank_closed_date = removal_date) %>%
    mutate(facility_id = as.character(gsub("NM", "", facility_id))) # Keep Char

# Classify
NM_UST_tanks <- classify_substances(NM_UST_tanks, "substances")
NM_UST_tanks <- classify_tank_walls(NM_UST_tanks, "tank_wall_type")
NM_UST_tanks <- standardize_capacity(NM_UST_tanks, "capacity")

# Load LUST
NM_LUST_files <- list.files(here("Data","Raw","state_databases","New Mexico"), pattern = "nfa", full.names = TRUE)
NM_LUST_01 <- read_excel(NM_LUST_files[2]) %>% clean_names() %>% as.data.table()
NM_LUST_02 <- read_excel(NM_LUST_files[3]) %>% clean_names() %>% as.data.table()
NM_LUST = rbindlist(list(NM_LUST_01, NM_LUST_02), fill=TRUE)

NM_LUST_SD = NM_LUST %>% 
  select(rid, fid, release_date, nfa_date) %>%
  rename(facility_id = fid, LUST_id = rid, report_date = release_date) %>%
  mutate(
    facility_id = as.character(facility_id),
    LUST_id = as.character(LUST_id),
    report_date = as.Date(report_date),
    nfa_date = as.Date(nfa_date),
    state = "NM"
  ) %>%
  select(facility_id, LUST_id, report_date, nfa_date, state) %>%
  as.data.table()

# Join
state_leak_data_collapsed <- NM_LUST_SD[, .(LUST_count = .N), by = .(facility_id, report_date, nfa_date)]
NM_UST_tanks_LUST = NM_UST_tanks %>%
    left_join(state_leak_data_collapsed, by = c("facility_id")) %>%
    mutate(
      release_date = report_date, # Use cleaned date
      NFA_date = nfa_date,
      tank_installed_date = mdy_hms(tank_installed_date),
      tank_closed_date = mdy_hms(tank_closed_date),
      leak_after_closure = ifelse(!is.na(tank_closed_date) & release_date > tank_closed_date, 1, 0),
      # ... [Standard logic] ...
      no_leak = ifelse(is.na(release_date) & !is.na(tank_closed_date), 1, 0)
    ) %>% as.data.table()

# Aggregate
NM_UST_tanks_SD = NM_UST_tanks_LUST %>%
  group_by(facility_id, tank_id, tank_installed_date, tank_closed_date, county) %>%
  summarize(
    facility_name = first(facility_name),
    tank_status = first(tank_status), # Keep Specific Logic
    leak_after_closure = sum(leak_after_closure, na.rm = TRUE),
    # ... [Sums] ...
    no_leak = sum(no_leak, na.rm = TRUE),
    is_gasoline = max(is_gasoline, na.rm = TRUE),
    # ... [Maxes] ...
    unknown_walled = max(unknown_walled, missing_walled, na.rm = TRUE),
    capacity = mean(capacity, na.rm = TRUE)
  ) %>% 
  as.data.table()

# Final Clean
NM_UST_tanks_SD[, `:=`(
  state = "NM",
  facility_name = stringr::str_to_title(trimws(gsub("[^[:alnum:] ]", "", facility_name))),
  facility_id = as.character(facility_id),
  tank_id = as.character(tank_id)
)]

# Output
output_dir <- here("Data","Raw","state_databases","New Mexico")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
fwrite(NM_UST_tanks_SD, file.path(output_dir, "NM_Harmonized_UST_tanks.csv"))
fwrite(NM_LUST_SD, file.path(output_dir, "NM_Harmonized_LUST.csv"))