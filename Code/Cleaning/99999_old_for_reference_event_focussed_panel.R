###############################################################################
# Script: 05_event_focused_panel.R
# Purpose: Create event-focused datasets that maintain time-varying characteristics
#          while significantly improving performance
###############################################################################

library(here)
library(data.table)
library(lubridate)
library(parallel)

# Start timing
script_start_time <- Sys.time()

# Performance optimizations
message("Setting performance optimizations...")
options(datatable.optimize = TRUE)
options(datatable.use.index = TRUE) 
options(datatable.auto.index = TRUE)
setDTthreads(max(1, round(detectCores() * 0.5)))

# Output file paths
message("Setting up output directories...")
output_dir <- here("Data/Event_Analysis")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
temp_dir <- here("Data/temp")
dir.create(temp_dir, recursive = TRUE, showWarnings = FALSE)

# Output files
event_panel_file <- file.path(output_dir, "event_panel.csv")
focused_panel_file <- file.path(output_dir, "focused_panel.csv")
tank_events_file <- file.path(output_dir, "tank_events.csv")

# Load data
message("Loading tank and LUST data...")
tank_cols <- c("facility_id", "tank_id", "state", "county_name", "county_geoid", 
               "tank_installed_date", "tank_closed_date", 
               "double_walled", "single_walled", "capacity",
               "is_gasoline", "is_diesel", "is_oil_kerosene", 
               "is_jet_fuel", "is_other")
tank_data <- fread(here("Data","Processed","all_raw_tank_data.csv"), select = tank_cols)
lust_cols <- c("facility_id", "state", "report_date", "nfa_date")
lust_data <- fread(here("Data","Processed","all_lust_data.csv"), select = lust_cols)

# Ensure consistent data types
tank_data[, `:=`(
  facility_id = as.character(facility_id),
  tank_id = as.character(tank_id),
  tank_installed_date = as.Date(tank_installed_date),
  tank_closed_date = as.Date(tank_closed_date)
)]
lust_data[, `:=`(
  facility_id = as.character(facility_id),
  report_date = as.Date(report_date),
  nfa_date = as.Date(nfa_date)
)]

# STEP 1: Create comprehensive event timeline
message("Creating comprehensive event timeline...")

# Tank installation events
tank_openings <- tank_data[!is.na(tank_installed_date), .(
  facility_id = facility_id,
  state = state,
  county_name = county_name,
  county_geoid = county_geoid,
  tank_id = tank_id,
  event_date = tank_installed_date,
  event_type = "tank_opening",
  double_walled = double_walled,
  single_walled = single_walled,
  capacity = as.numeric(capacity),
  is_gasoline = is_gasoline,
  is_diesel = is_diesel,
  is_oil_kerosene = is_oil_kerosene,
  is_jet_fuel = is_jet_fuel,
  is_other = is_other
)]

# Tank closure events
tank_closures <- tank_data[!is.na(tank_closed_date), .(
  facility_id = facility_id,
  state = state,
  county_name = county_name,
  county_geoid = county_geoid,
  tank_id = tank_id,
  event_date = tank_closed_date,
  event_type = "tank_closure"
)]

# LUST events
leak_detections <- lust_data[!is.na(report_date), .(
  facility_id = facility_id,
  state = state,
  event_date = report_date,
  event_type = "leak_detection"
)]

leak_remediations <- lust_data[!is.na(nfa_date), .(
  facility_id = facility_id,
  state = state,
  event_date = nfa_date,
  event_type = "leak_remediation"
)]

# Combine all events
all_events <- rbindlist(list(
  tank_openings, 
  tank_closures, 
  leak_detections, 
  leak_remediations
), use.names = TRUE, fill = TRUE)

# Add week information
all_events[, `:=`(
  year = year(event_date),
  month = month(event_date),
  week = week(event_date),
  week_key = paste0(year(event_date), "-W", sprintf("%02d", week(event_date)))
)]

# Sort events chronologically
setkey(all_events, facility_id, state, event_date)

# Save all tank events
fwrite(all_events, file = tank_events_file)

# STEP 2: Create change points (dates when facility characteristics change)
message("Identifying facility characteristic change points...")

# Get all dates when tank configuration changes (openings and closures)
change_points <- all_events[event_type %in% c("tank_opening", "tank_closure"), 
                          .(change_date = unique(event_date)), 
                          by = .(facility_id, state)]

# STEP 3: Create facility-level panel at each change point and for LUST events
message("Creating efficient panel with time-varying characteristics...")

# Combine change points with LUST events to ensure we have every important date
important_dates <- rbind(
  # Tank configuration change dates
  change_points[, .(facility_id, state, panel_date = change_date, date_type = "change_point")],
  
  # Leak detection dates
  all_events[event_type == "leak_detection", 
            .(facility_id, state, panel_date = event_date, date_type = "leak_detection")],
  
  # Remediation dates
  all_events[event_type == "leak_remediation", 
            .(facility_id, state, panel_date = event_date, date_type = "leak_remediation")]
)

# Add additional dates around important events for event study windows
message("Adding event study windows around important dates...")
panel_dates <- important_dates[, {
  # Create a list to store results for each panel date
  result_list <- list()
  
  # Process each panel date individually
  for (i in seq_along(panel_date)) {
    curr_date <- panel_date[i]
    curr_type <- date_type[i]
    
    # Set window size based on event type
    window_size <- ifelse(curr_type == "change_point", 12, 52)
    
    # Create date sequence for this specific panel date
    date_seq <- seq.Date(
      from = curr_date - 7*window_size,
      to = curr_date + 7*window_size,
      by = "week"
    )
    
    # Add to results
    result_list[[i]] <- data.table(
      panel_date = date_seq,
      original_event_date = curr_date,
      date_type = curr_type
    )
  }
  
  # Combine results and remove duplicates
  combined_results <- rbindlist(result_list)
  unique(combined_results, by = "panel_date")
  
}, by = .(facility_id, state)]

# Remove duplicate dates across events
panel_dates <- unique(panel_dates, by = c("facility_id", "state", "panel_date"))

# Add date information
panel_dates[, `:=`(
  year = year(panel_date),
  month = month(panel_date),
  week = week(panel_date),
  week_key = paste0(year(panel_date), "-W", sprintf("%02d", week(panel_date)))
)]

message("Panel created with time-varying characteristics at key dates")
message(sprintf("Total panel points: %d", nrow(panel_dates)))

# Function to calculate facility characteristics at a given date
calculate_facility_chars <- function(facilities_chunk, dates_chunk) {
  result <- dates_chunk[, {
    # Get current date and facility
    curr_date <- panel_date
    curr_facility <- facility_id
    curr_state <- state
    
    # Find all tanks that exist by this date
    active_tanks <- tank_data[
      facility_id == curr_facility &
      state == curr_state &
      tank_installed_date <= curr_date &
      (is.na(tank_closed_date) | tank_closed_date > curr_date)
    ]
    
    # Calculate characteristics based on active tanks
    if(nrow(active_tanks) > 0) {
      .(
        active_tank_count = nrow(active_tanks),
        double_walled_tanks = sum(active_tanks$double_walled == 1, na.rm = TRUE),
        single_walled_tanks = sum(active_tanks$single_walled == 1, na.rm = TRUE),
        gasoline_tanks = sum(active_tanks$is_gasoline == 1, na.rm = TRUE),
        diesel_tanks = sum(active_tanks$is_diesel == 1, na.rm = TRUE),
        oil_kerosene_tanks = sum(active_tanks$is_oil_kerosene == 1, na.rm = TRUE),
        jet_fuel_tanks = sum(active_tanks$is_jet_fuel == 1, na.rm = TRUE),
        other_fuel_tanks = sum(active_tanks$is_other == 1, na.rm = TRUE),
        avg_capacity = mean(as.numeric(active_tanks$capacity), na.rm = TRUE),
        total_capacity = sum(as.numeric(active_tanks$capacity), na.rm = TRUE),
        oldest_tank_age_days = as.numeric(difftime(curr_date, min(active_tanks$tank_installed_date), units = "days")),
        avg_tank_age_days = mean(as.numeric(difftime(curr_date, active_tanks$tank_installed_date, units = "days")))
      )
    } else {
      .(
        active_tank_count = 0L,
        double_walled_tanks = 0L,
        single_walled_tanks = 0L,
        gasoline_tanks = 0L,
        diesel_tanks = 0L,
        oil_kerosene_tanks = 0L,
        jet_fuel_tanks = 0L,
        other_fuel_tanks = 0L,
        avg_capacity = NA_real_,
        total_capacity = 0,
        oldest_tank_age_days = NA_real_,
        avg_tank_age_days = NA_real_
      )
    }
  }, by = .(facility_id, state, panel_date, year, week, week_key)]
  
  return(result)
}

# Process in parallel for performance
message("Calculating time-varying characteristics in parallel...")

# Split data for parallel processing
facility_chunks <- split(unique(panel_dates$facility_id), 
                        ceiling(seq_along(unique(panel_dates$facility_id))/500))

# Create cluster for parallel processing
if (exists("cl") && !is.null(cl)) {
  stopCluster(cl)
}
cl <- makeCluster(max(1, round(detectCores() * 0.75)))
clusterExport(cl, c("tank_data", "panel_dates", "calculate_facility_chars"), 
             envir = environment())
clusterEvalQ(cl, {
  library(data.table)
})

# Process each facility chunk
facility_chars_chunks <- parLapply(cl, facility_chunks, function(facilities) {
  # Get dates for these facilities
  dates_for_facilities <- panel_dates[facility_id %in% facilities]
  
  # Calculate characteristics
  result <- calculate_facility_chars(facilities, dates_for_facilities)
  
  # Return processed data
  return(result)
})

# Combine results
message("Combining parallel results...")
panel_with_chars <- rbindlist(facility_chars_chunks)

# STEP 4: Add leak status and event study indicators
message("Adding leak status and event study indicators...")

# Add indicators for leak events
facility_leaks <- all_events[event_type %in% c("leak_detection", "leak_remediation"), 
                           .(event_date, event_type),
                           by = .(facility_id, state)]

panel_with_leaks <- panel_with_chars[, {
  curr_date <- panel_date
  
  # Find all leaks active on this date (detected but not remediated)
  leaks_detected <- sum(facility_leaks[
    facility_id == .BY$facility_id &
    state == .BY$state &
    event_type == "leak_detection" &
    event_date <= curr_date
  , .N])
  
  leaks_remediated <- sum(facility_leaks[
    facility_id == .BY$facility_id &
    state == .BY$state &
    event_type == "leak_remediation" &
    event_date <= curr_date
  , .N])
  
  # Calculate active leaks
  active_leaks <- max(0, leaks_detected - leaks_remediated)
  
  # Calculate whether a new leak was detected that week
  new_leak_this_week <- sum(facility_leaks[
    facility_id == .BY$facility_id &
    state == .BY$state &
    event_type == "leak_detection" &
    event_date >= curr_date - 7 &
    event_date <= curr_date
  , .N])
  
  # Calculate whether a leak was remediated that week
  remediation_this_week <- sum(facility_leaks[
    facility_id == .BY$facility_id &
    state == .BY$state &
    event_type == "leak_remediation" &
    event_date >= curr_date - 7 &
    event_date <= curr_date
  , .N])
  
  # Find days to nearest tank closure before leak detection
  nearest_closure_days <- NA_real_
  if (new_leak_this_week > 0) {
    leak_date <- facility_leaks[
      facility_id == .BY$facility_id &
      state == .BY$state &
      event_type == "leak_detection" &
      event_date >= curr_date - 7 &
      event_date <= curr_date,
      min(event_date)
    ]
    
    # Find the most recent closure before this leak
    recent_closure <- all_events[
      facility_id == .BY$facility_id &
      state == .BY$state &
      event_type == "tank_closure" &
      event_date < leak_date,
      max(event_date)
    ]
    
    if (!is.na(recent_closure) && !is.infinite(recent_closure)) {
      nearest_closure_days <- as.numeric(difftime(leak_date, recent_closure, units = "days"))
    }
  }
  
  .(
    leak_count_cumulative = leaks_detected,
    remediation_count_cumulative = leaks_remediated,
    active_leaks = active_leaks,
    has_active_leak = as.integer(active_leaks > 0),
    new_leak_this_week = new_leak_this_week,
    remediation_this_week = remediation_this_week,
    days_since_nearest_closure = nearest_closure_days
  )
}, by = .(facility_id, state, panel_date, year, week, week_key, 
         active_tank_count, double_walled_tanks, single_walled_tanks, 
         gasoline_tanks, diesel_tanks, oil_kerosene_tanks, jet_fuel_tanks,
         other_fuel_tanks, avg_capacity, total_capacity, 
         oldest_tank_age_days, avg_tank_age_days)]

# Add tank event indicators
panel_with_events <- panel_with_leaks[, {
  curr_date <- panel_date
  
  # Find if any tanks were opened or closed that week
  tanks_opened <- sum(all_events[
    facility_id == .BY$facility_id &
    state == .BY$state &
    event_type == "tank_opening" &
    event_date >= curr_date - 7 &
    event_date <= curr_date
  , .N])
  
  tanks_closed <- sum(all_events[
    facility_id == .BY$facility_id &
    state == .BY$state &
    event_type == "tank_closure" &
    event_date >= curr_date - 7 &
    event_date <= curr_date
  , .N])
  
  # Check if this was a replacement (closure + opening within same week)
  replacement <- (tanks_opened > 0 & tanks_closed > 0)
  
  # Find days since first tank closure
  first_closure <- all_events[
    facility_id == .BY$facility_id &
    state == .BY$state &
    event_type == "tank_closure",
    min(event_date)
  ]
  
  # Find days since first leak
  first_leak <- all_events[
    facility_id == .BY$facility_id &
    state == .BY$state &
    event_type == "leak_detection",
    min(event_date)
  ]
  
  days_since_first_closure <- NA_real_
  if (!is.na(first_closure) && !is.infinite(first_closure)) {
    days_since_first_closure <- as.numeric(difftime(curr_date, first_closure, units = "days"))
  }
  
  days_since_first_leak <- NA_real_
  if (!is.na(first_leak) && !is.infinite(first_leak)) {
    days_since_first_leak <- as.numeric(difftime(curr_date, first_leak, units = "days"))
  }
  
  .(
    tanks_opened_this_week = tanks_opened,
    tanks_closed_this_week = tanks_closed,
    tank_replacement_this_week = as.integer(replacement),
    days_since_first_closure = days_since_first_closure,
    weeks_since_first_closure = floor(days_since_first_closure / 7),
    days_since_first_leak = days_since_first_leak,
    weeks_since_first_leak = floor(days_since_first_leak / 7)
  )
}, by = .(facility_id, state, county_name, county_geoid, panel_date, year, week, week_key,
         active_tank_count, double_walled_tanks, single_walled_tanks, 
         gasoline_tanks, diesel_tanks, oil_kerosene_tanks, jet_fuel_tanks,
         other_fuel_tanks, avg_capacity, total_capacity, 
         oldest_tank_age_days, avg_tank_age_days,
         leak_count_cumulative, remediation_count_cumulative, active_leaks,
         has_active_leak, new_leak_this_week, remediation_this_week,
         days_since_nearest_closure)]

# Calculate facility entry/exit status
message("Calculating entry and exit dates...")
facility_status <- panel_with_events[, .(
  first_appearance = min(panel_date),
  last_appearance = max(panel_date),
  first_tank_date = min(panel_date[active_tank_count > 0]),
  last_active_tanks = max(active_tank_count) > 0
), by = .(facility_id, state)]

# Add exit indicator for analytical purposes
facility_status[, facility_exit := as.integer(!last_active_tanks)]

# Merge back to panel
panel_final <- merge(
  panel_with_events,
  facility_status[, .(facility_id, state, facility_entry_date = first_tank_date, 
                    facility_exit = facility_exit)],
  by = c("facility_id", "state"),
  all.x = TRUE
)

# Add additional derived variables useful for analysis
panel_final[, `:=`(
  all_tanks_closed = (active_tank_count == 0),
  pct_double_walled = fifelse(active_tank_count > 0, 
                             double_walled_tanks / active_tank_count * 100, 
                             NA_real_),
  avg_tank_size = fifelse(active_tank_count > 0,
                         total_capacity / active_tank_count,
                         NA_real_),
  tanks_ever_closed = tanks_closed_this_week > 0 | all_tanks_closed,
  oldest_tank_age_years = oldest_tank_age_days / 365.25,
  avg_tank_age_years = avg_tank_age_days / 365.25
)]

# STEP 5: Write the final panel to disk
message("Writing final panel to disk...")
fwrite(panel_final, file = focused_panel_file)

# Clean up
stopCluster(cl)
gc()

# Execution time
script_end_time <- Sys.time()
execution_time <- difftime(script_end_time, script_start_time, units = "mins")
message(sprintf("Total execution time: %.2f minutes", as.numeric(execution_time)))
message("Event-focused panel with time-varying characteristics created successfully!")