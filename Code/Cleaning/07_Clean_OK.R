# Oklahoma Dataset Creation

# Define the county variable name in the raw data
COUNTYVARIABLE <- "county"

library(here)
first_run = TRUE
source(here('Code','00_global_packages.R'))

# Load Data - Force Character IDs
oklahoma_files <- list.files(here("Data","Raw","state_databases","Oklahoma"), pattern = "^OCC_PST.*\\.csv$", full.names = TRUE)

OK_LUSt <- fread(oklahoma_files[1], colClasses = c("Facility Number"="character", "Case Number"="character"))
OK_Tanks <- fread(oklahoma_files[2], colClasses = c("Facility Number"="character", "Tank Number"="character")) 

# Update tank selection
OK_UST_tanks = OK_Tanks %>% clean_names() %>%
    select(facility_number, facility_name, tank_number, tank_installed_date, tank_closed_date, tank_type, facility_zip_code,
           tank_status, tank_substance, tank_construction, tank_capacity) %>%
    filter(tank_type == "UST") %>% as.data.table()

# [Classify Functions Omitted - assume defined same as provided]
OK_substances_classified <- classify_substances(OK_UST_tanks)
OK_UST_tanktype <- classify_tank_walls(OK_UST_tanks)

OK_UST_tanks <- OK_UST_tanks %>%
  mutate(capacity_numeric = as.numeric(gsub("[^0-9.]", "", tank_capacity))) %>%
  left_join(OK_substances_classified, by = c("facility_number", "tank_number")) %>%
  left_join(OK_UST_tanktype, by = c("facility_number", "tank_number"))

# Clean IDs (remove brackets but keep char)
OK_UST_tanks[, facility_number := gsub("\\[|\\]", "", facility_number)]
OK_LUSt[, `Facility Number` := gsub("\\[|\\]", "", `Facility Number`)]

# Clean LUST
OK_LUST_SD = OK_LUSt %>% clean_names() %>%
  select(facility_number, case_number, release_date, close_date, case_type) %>%
  rename(facility_id = facility_number, LUST_id = case_number, report_date = release_date, nfa_date = close_date) %>%
  mutate(
    facility_id = as.character(facility_id),
    LUST_id = as.character(LUST_id),
    report_date = mdy(report_date),
    nfa_date = mdy(nfa_date),
    state = "OK"
  ) %>% as.data.table()

# Merge
OK_UST_tanks_LUST = OK_UST_tanks %>%
    left_join(OK_LUST_SD, by = c("facility_number" = "facility_id")) %>%
    mutate(
        tank_installed_date = mdy(tank_installed_date),
        tank_closed_date = mdy(tank_closed_date),
        CC = ifelse(case_type == "Confirmed Release", 1, 0),
        leak_after_closure = ifelse(!is.na(tank_closed_date) & report_date > tank_closed_date & CC == 1, 1, 0),
        # ... [Standard Logic] ...
        no_leak = ifelse(is.na(report_date) & !is.na(tank_closed_date) , 1, 0)
    ) %>% as.data.table()

# Aggregate
OK_UST_tanks_SD = OK_UST_tanks_LUST %>%
  group_by(facility_number, tank_number, tank_installed_date, tank_closed_date) %>%
  summarize(
    facility_name = first(facility_name),
    tank_status = first(tank_status), # Keep Specific Logic
    leak_after_closure = sum(leak_after_closure, na.rm = TRUE),
    # ... [Sums] ...
    no_leak = sum(no_leak, na.rm = TRUE),
    is_gasoline = max(is_gasoline, na.rm = TRUE),
    # ... [Maxes] ...
    unknown_walled = max(unknown_walled, missing_walled, na.rm = TRUE),
    capacity = mean(capacity_numeric, na.rm = TRUE)
  ) %>%
  as.data.table()

# Final Clean
OK_UST_tanks_SD[, `:=`(
  state = "OK",
  facility_name = stringr::str_to_title(trimws(gsub("[^[:alnum:] ]", "", facility_name))),
  facility_id = as.character(facility_number),
  tank_id = as.character(tank_number)
)]

# Output
output_dir <- here("Data","Raw","state_databases","Oklahoma")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
fwrite(OK_UST_tanks_SD, file.path(output_dir, "OK_Harmonized_UST_tanks.csv"))
# Filter columns for LUST output
OK_LUST_Final <- OK_LUST_SD[, .(facility_id, LUST_id, report_date, nfa_date, state)]
fwrite(OK_LUST_Final, file.path(output_dir, "OK_Harmonized_LUST.csv"))