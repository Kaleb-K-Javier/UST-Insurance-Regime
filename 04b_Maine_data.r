## Maine UST Dataset Creation

# Define the county variable name in the raw data
COUNTYVARIABLE <- "county"

library(here)
first_run = TRUE
#load packages ----
source(here('Code','00_global_packages.R'))

# Function to standardize county names
standardize_county_name <- function(county_name) {
  if(is.na(county_name) || county_name == "") return(NA)
  
  # Convert to lowercase
  name <- tolower(county_name)
  
  # Remove "county", "parish", etc.
  name <- gsub(" county$| counties$| parish$| parishes$", "", name)
  
  # Remove special characters and extra spaces
  name <- gsub("[^a-z ]", "", name)
  name <- trimws(gsub("\\s+", " ", name))
  
  return(name)
}

# Function to classify tank wall types
classify_tank_walls <- function(data, tank_wall_col = "const_") {
  result <- copy(data)
  
  # Create binary variables for tank wall type based on const_ column
  # For Maine, we'll look for specific patterns in the construction field
  result[, `:=`(
    double_walled = as.integer(grepl("DOUBLE[-]?WALL|DBL[-]?WALL|DUAL[-]?WALL", 
                                    toupper(get(tank_wall_col)), ignore.case = TRUE)),
    single_walled = as.integer(!is.na(get(tank_wall_col)) & 
                              get(tank_wall_col) != "" & 
                              !grepl("DOUBLE[-]?WALL|DBL[-]?WALL|DUAL[-]?WALL", 
                                    toupper(get(tank_wall_col)), ignore.case = TRUE)),
    unknown_walled = 0,  # Maine doesn't have unknown wall types
    missing_walled = as.integer(is.na(get(tank_wall_col)) | get(tank_wall_col) == "")
  )]
  
  # Create a summary table
  cat("\nTank wall classification summary:\n")
  print(result[, .(
    total_records = .N,
    single_walled_count = sum(single_walled, na.rm = TRUE),
    double_walled_count = sum(double_walled, na.rm = TRUE),
    unknown_walled_count = sum(unknown_walled, na.rm = TRUE),
    missing_walled_count = sum(missing_walled, na.rm = TRUE)
  )])
  
  return(result)
}

# Function to classify substances
classify_substances <- function(data, substance_col_name = "prod_") {
  # Create binary classification columns with default value
  result <- copy(data)
  result[, `:=`(
    is_gasoline = 0,
    is_diesel = 0,
    is_oil_kerosene = 0,
    is_jet_fuel = 0,
    is_other = 1  # Default to 1, will set to 0 if any other category matches
  )]
  
# Classification patterns based on Maine-specific contents list
gasoline_patterns <- paste(c(
  "gasoline unspecified", "leaded gasoline", "premium unleaded", 
  "regular gasoline", "unleaded gasoline", "unleaded plus"
), collapse = "|")

diesel_patterns <- paste(c(
  "diesel", "bio 1-74", "bio 75-99"
), collapse = "|")

oil_kerosene_patterns <- paste(c(
  "#1 fuel oil - kerosene", "#2 fuel oil", "#5 fuel oil", "#6 fuel oil", 
  "hydraulic oil", "lube oil", "oil - other - specified in report", 
  "unspecified oil", "waste oil/used motor oil"
), collapse = "|")

jet_fuel_patterns <- paste(c(
  "aviation gasoline", "jet fuel", "jp1", "jp4"
), collapse = "|")
  # Convert substances to lowercase
  result[, substances_lower := tolower(get(substance_col_name))]
  
  # Apply classification patterns using vectorized operations
  result[!is.na(substances_lower) & substances_lower != "", 
         is_gasoline := as.integer(grepl(gasoline_patterns, substances_lower, perl = TRUE))]
  
  # Exclude DEF from diesel classification
  result[!is.na(substances_lower) & substances_lower != "" & 
         !grepl("def|diesel exhaust fluid", substances_lower, perl = TRUE), 
         is_diesel := as.integer(grepl(diesel_patterns, substances_lower, perl = TRUE))]
  
  result[!is.na(substances_lower) & substances_lower != "", 
         is_oil_kerosene := as.integer(grepl(oil_kerosene_patterns, substances_lower, perl = TRUE))]
  
  result[!is.na(substances_lower) & substances_lower != "", 
         is_jet_fuel := as.integer(grepl(jet_fuel_patterns, substances_lower, perl = TRUE))]
  
  # Set "other" to 0 if any other category matched
  result[, is_other := as.integer(is_gasoline + is_diesel + is_oil_kerosene + is_jet_fuel == 0)]
  
  # Remove temporary column
  result[, substances_lower := NULL]
  
  # Create a summary table of the classifications
  cat("\nSubstance classification summary:\n")
  print(result[, .(
    total_records = .N,
    gasoline_count = sum(is_gasoline, na.rm = TRUE),
    diesel_count = sum(is_diesel, na.rm = TRUE),
    oil_kerosene_count = sum(is_oil_kerosene, na.rm = TRUE),
    jet_fuel_count = sum(is_jet_fuel, na.rm = TRUE),
    other_count = sum(is_other, na.rm = TRUE),
    multi_category_count = sum(is_gasoline + is_diesel + is_oil_kerosene + is_jet_fuel > 1, na.rm = TRUE)
  )])
  
  return(result)
}

# Function to standardize capacity
standardize_capacity <- function(data, capacity_col = "cap_") {
  result <- copy(data)
  
  result[, `:=`(
    capacity = as.numeric(gsub("[^0-9.]", "", get(capacity_col)))
  )]
  
  return(result)
}

# State name standardization -----
# Create a mapping dictionary between abbreviations and full state names using R's built-in state data
data(state)
state_name_mapping <- setNames(state.name, state.abb)

# Add any missing territories or DC if needed
state_name_mapping["DC"] <- "District of Columbia"
state_name_mapping["PR"] <- "Puerto Rico"
state_name_mapping["VI"] <- "Virgin Islands"
state_name_mapping["GU"] <- "Guam"
state_name_mapping["AS"] <- "American Samoa"
state_name_mapping["MP"] <- "Northern Mariana Islands"

# Function to standardize state names and add abbreviations using data.table for efficiency
standardize_states <- function(dt) {
  if ("state" %in% names(dt)) {
    # Create a temporary copy of state column
    dt[, state_original := state]
    
    # Create reverse mapping for full state names to abbreviations
    reverse_mapping <- setNames(names(state_name_mapping), state_name_mapping)
    
    # Create a mapping table as a data.table for efficient joins
    mapping_dt <- data.table(
      abbr = names(state_name_mapping),
      full_name = unlist(state_name_mapping)
    )
    reverse_dt <- data.table(
      full_name = names(reverse_mapping),
      abbr = unlist(reverse_mapping)
    )
    
    # Convert two-letter codes to full names using joins
    # First identify which rows need conversion (where state is in abbr column)
    dt[state %in% mapping_dt$abbr, state := mapping_dt[.(state), full_name, on = "abbr"]]
    
    # Create state_abr column from original state
    dt[, state_abr := state_original]
    
    # For full names that don't have abbreviations yet, find their abbreviation
    dt[state %in% reverse_dt$full_name & !(state_abr %in% mapping_dt$abbr),
       state_abr := reverse_dt[.(state), abbr, on = "full_name"]]
    
    # Remove the temporary column
    dt[, state_original := NULL]
  }
  return(dt)
}

# Download and Load Raw Data - Maine files -----
cat("\nDownloading Maine UST data...\n")

# Create a temporary file to store the downloaded data
temp_file <- tempfile()

# Download the Maine tank data
download.file("https://www.maine.gov/dep/ftp/tanks/tanks_owner_all_registered_tanks.txt", 
              destfile = temp_file, mode = "wb")

# Read the file directly with fread using * as delimiter
ME_tanks_raw <- fread(temp_file, sep = "*", header = TRUE, fill = TRUE, 
                     colClasses = c(registration_number = "character", tank_number = "character")) %>%
  janitor::clean_names() %>%
  as.data.table()

# save out the raw data
if(first_run){
  dir.create(here("Data", "Raw_do_not_write", "state_databases", "Maine"), showWarnings = FALSE)
  fwrite(ME_tanks_raw, here("Data", "Raw_do_not_write", "state_databases", "Maine", "Maine_raw_tank_data.csv"))

  ## now save out the federally regulated USTs fed_regulated == Y
  ME_tanks_raw_fed_reg <- ME_tanks_raw[fed_regulated == "Y"]
  fwrite(ME_tanks_raw_fed_reg, here("Data", "Raw_do_not_write", "state_databases", "Maine", "Maine_raw_tank_data_fed_reg.csv"))
}

# Clean up the temporary file
# LUST Data Loading -----
cat("\nDownloading and processing Maine LUST data...\n")

# Define the base URL for the HOSS files
base_url <- "https://www.maine.gov/dep/ftp/hoss/"

# Define file names for the needed tables
tank_file         <- "report_tank_involved.txt"
spill_report_file <- "spill_report.txt"
spill_log_file    <- "spill_log.txt"

# Function to safely read * delimited files with known problematic line handling
safe_fread <- function(file_url, file_name) {
  tryCatch({
    temp_file <- tempfile()
    download_url <- paste0(file_url, file_name)
    cat(paste0("\nDownloading ", download_url, "...\n"))
    
    download.file(download_url, destfile = temp_file, mode = "wb", quiet = TRUE)
    
    # Read all lines from the file
    all_lines <- readLines(temp_file, warn = FALSE)
    total_lines <- length(all_lines)
    cat(paste0("File has ", total_lines, " lines total.\n"))
    
    # Check if this is the spill_report file with the known problematic line 70340
    if(file_name == "spill_report.txt" && total_lines >= 70340) {
      cat("Detected spill_report.txt with known problematic line 70340\n")
      cat("Creating a clean version with the problematic line removed...\n")
      
      # Create a temporary file without the problematic line
      clean_file <- tempfile()
      
      # Write all lines except the problematic one
      writeLines(all_lines[c(1:70339, 70341:total_lines)], clean_file)
      
      # Read the clean file
      dt <- fread(clean_file, sep = "*", header = FALSE, fill = TRUE, 
                 quote = "", data.table = TRUE, 
                 showProgress = FALSE, encoding = "Latin-1")
      
      cat(paste0("Successfully loaded ", file_name, " with ", nrow(dt), " rows and ", 
                ncol(dt), " columns (skipped line 70340).\n"))
      
      # Clean up
      file.remove(clean_file)
      return(dt)
    }
    
    # Standard process for other files or if spill_report doesn't have enough lines
    # Initialize variables to track progress and bad lines
    bad_lines <- integer(0)
    
    # First attempt - try reading the whole file
    cat(paste0("Attempting to read ", file_name, "...\n"))
    dt <- tryCatch({
      fread(temp_file, sep = "*", header = FALSE, fill = TRUE, 
            quote = "", data.table = TRUE, 
            showProgress = FALSE, encoding = "Latin-1")
    }, error = function(e) {
      cat(paste0("Standard read failed: ", e$message, "\n"))
      return(NULL)
    })
    
    # If the initial read fails, use incremental loading approach
    if (is.null(dt)) {
      cat("Switching to incremental loading to identify problematic rows...\n")
      
      # Create temporary file for clean data
      clean_file <- tempfile()
      
      # Write header line to clean file
      writeLines(all_lines[1], clean_file)
      
      # Process the file incrementally, skipping bad lines
      dt_chunks <- list()
      i <- 2  # Start after header
      
      while (i <= total_lines) {
        # Try to read a chunk of lines
        temp_chunk_file <- tempfile()
        chunk_lines <- min(i + 999, total_lines)
        
        # Write chunk to temp file
        writeLines(c(all_lines[1], all_lines[i:chunk_lines]), temp_chunk_file)
        
        # Try to read the chunk
        chunk_dt <- tryCatch({
          fread(temp_chunk_file, sep = "*", header = FALSE, fill = TRUE,
                quote = "", data.table = TRUE, showProgress = FALSE,
                encoding = "Latin-1", nThread = 1)
        }, error = function(e) {
          return(NULL)
        })
        
        # If chunk read failed, process lines one by one
        if (is.null(chunk_dt) && chunk_lines - i < 10) {
          # Process individual lines in the small problematic chunk
          for (j in i:chunk_lines) {
            single_line_file <- tempfile()
            writeLines(c(all_lines[1], all_lines[j]), single_line_file)
            
            single_dt <- tryCatch({
              fread(single_line_file, sep = "*", header = FALSE, fill = TRUE,
                   quote = "", data.table = TRUE, showProgress = FALSE,
                   encoding = "Latin-1", nThread = 1)
            }, error = function(e) {
              bad_lines <- c(bad_lines, j)
              cat(paste0("Line ", j, " is problematic and will be skipped.\n"))
              return(NULL)
            })
            
            if (!is.null(single_dt)) {
              # Append good line to clean file
              write.table(all_lines[j], clean_file, append = TRUE, 
                          row.names = FALSE, col.names = FALSE, quote = FALSE)
              dt_chunks <- c(dt_chunks, list(single_dt))
            }
            
            file.remove(single_line_file)
          }
        } else if (is.null(chunk_dt)) {
          # If larger chunk failed, reduce size and try again
          cat(paste0("Failed to read chunk from lines ", i, " to ", chunk_lines, ". Reducing chunk size.\n"))
          
          # Binary search approach to find problematic lines
          if (chunk_lines - i > 100) {
            # For large chunks, split in half and try again
            mid_point <- i + floor((chunk_lines - i) / 2)
            
            # First try the first half
            first_half_file <- tempfile()
            writeLines(c(all_lines[1], all_lines[i:mid_point]), first_half_file)
            
            first_half_dt <- tryCatch({
              fread(first_half_file, sep = "*", header = FALSE, fill = TRUE,
                   quote = "", data.table = TRUE, showProgress = FALSE,
                   encoding = "Latin-1", nThread = 1)
            }, error = function(e) {
              return(NULL)
            })
            
            if (!is.null(first_half_dt)) {
              dt_chunks <- c(dt_chunks, list(first_half_dt))
              # Append good chunk
              write.table(all_lines[i:mid_point], clean_file, append = TRUE, 
                          row.names = FALSE, col.names = FALSE, quote = FALSE,
                          sep = "\n")
            } else {
              # This half has issues, mark for more detailed processing
              cat(paste0("Issues in lines ", i, " to ", mid_point, ", will process in smaller chunks.\n"))
            }
            
            file.remove(first_half_file)
            
            # Then try the second half
            second_half_file <- tempfile()
            writeLines(c(all_lines[1], all_lines[(mid_point+1):chunk_lines]), second_half_file)
            
            second_half_dt <- tryCatch({
              fread(second_half_file, sep = "*", header = FALSE, fill = TRUE,
                   quote = "", data.table = TRUE, showProgress = FALSE,
                   encoding = "Latin-1", nThread = 1)
            }, error = function(e) {
              return(NULL)
            })
            
            if (!is.null(second_half_dt)) {
              dt_chunks <- c(dt_chunks, list(second_half_dt))
              # Append good chunk
              write.table(all_lines[(mid_point+1):chunk_lines], clean_file, append = TRUE, 
                          row.names = FALSE, col.names = FALSE, quote = FALSE,
                          sep = "\n")
            } else {
              # This half has issues, mark for more detailed processing
              cat(paste0("Issues in lines ", (mid_point+1), " to ", chunk_lines, ", will process in smaller chunks.\n"))
            }
            
            file.remove(second_half_file)
            
            # Move to next chunk
            i <- chunk_lines + 1
          } else {
            # For small chunks, move forward by one line and try again
            bad_lines <- c(bad_lines, i)
            cat(paste0("Line ", i, " appears problematic and will be skipped.\n"))
            i <- i + 1
          }
          next
        } else {
          # Chunk read successfully
          cat(paste0("Successfully read lines ", i, " to ", chunk_lines, ".\n"))
          dt_chunks <- c(dt_chunks, list(chunk_dt))
          # Append good chunk to clean file
          write.table(all_lines[i:chunk_lines], clean_file, append = TRUE, 
                      row.names = FALSE, col.names = FALSE, quote = FALSE,
                      sep = "\n")
        }
        
        i <- chunk_lines + 1
        file.remove(temp_chunk_file)
      }
      
      # Now read the clean file with all good lines
      if (length(dt_chunks) > 0) {
        dt <- rbindlist(dt_chunks, fill = TRUE)
        
        # Save list of bad lines
        if (length(bad_lines) > 0) {
          bad_lines_file <- paste0(temp_file, ".bad_lines")
          writeLines(as.character(bad_lines), bad_lines_file)
          cat(paste0("Identified ", length(bad_lines), " problematic lines. List saved to ", bad_lines_file, "\n"))
        }
      } else {
        cat("Failed to read any valid data from the file.\n")
        return(data.table())
      }
    }
    
    cat(paste0("Successfully loaded ", file_name, " with ", nrow(dt), " rows and ", 
              ncol(dt), " columns.\n"))
    
    if (length(bad_lines) > 0) {
      cat(paste0("Skipped ", length(bad_lines), " problematic lines.\n"))
    }
    
    return(dt)
  }, error = function(e) {
    cat(paste0("\nFailed to load ", file_name, ": ", e$message, "\n"))
    # Return an empty data.table to avoid breaking the workflow
    return(data.table())
  }, finally = {
    # Clean up temporary files
    if (exists("temp_file") && file.exists(temp_file)) file.remove(temp_file)
  })
}

# Load the LUST data files using the safe_fread function
cat("\nLoading LUST data files...\n")
tank_raw_full         <- safe_fread(base_url, tank_file)
spill_report_raw_full <- safe_fread(base_url, spill_report_file)
spill_log_raw_full    <- safe_fread(base_url, spill_log_file)

# Check if any files failed to load completely
if (nrow(tank_raw_full) == 0 || nrow(spill_report_raw_full) == 0 || nrow(spill_log_raw_full) == 0) {
  warning("One or more LUST data files may have failed to load completely.")
}

# Display summary of loaded data
cat("\nSummary of loaded LUST data:\n")
cat("Tank data:", nrow(tank_raw_full), "rows,", ncol(tank_raw_full), "columns\n")
cat("Spill report data:", nrow(spill_report_raw_full), "rows,", ncol(spill_report_raw_full), "columns\n")
cat("Spill log data:", nrow(spill_log_raw_full), "rows,", ncol(spill_log_raw_full), "columns\n")

# Select only the columns we want and rename them based on the comments:

# From tank_raw - selecting only columns marked "-- Want this one":
# - Column 1: L.SPILL_NUMBER
# - Column 5: UST_REGISTERED_FLAG
# - Column 6: UST_TANK_SITE_NUMBER
tank_raw <- tank_raw_full[, .(
  l_spill_number = V1,          # L.SPILL_NUMBER
  ust_registered_flag = V5,     # UST_REGISTERED_FLAG
  UST_TANK_SITE_NUMBER = V6     # UST_TANK_SITE_NUMBER
)]

# From spill_report_raw - selecting only columns marked "-- Want this one":
# - Column 1: L.SPILL_NUMBER
# - Column 4: MODIFY_DATE
# - Column 6: REPORT_STATUS
# - Column 7: REPORT_STATUS_VALUE
spill_report_raw <- spill_report_raw_full[, .(
  l_spill_number = V1,          # L.SPILL_NUMBER
  modify_date = V4,             # MODIFY_DATE
  report_status = V6,           # REPORT_STATUS
  report_status_value = V7      # REPORT_STATUS_VALUE
)]

# From spill_log_raw - selecting only columns marked "-- Want this one":
# - Column 1: L.SPILL_NUMBER
# - Column 16: LOG_REPORTED_DATETIME
spill_log_raw <- spill_log_raw_full[, .(
  l_spill_number = V1,          # L.SPILL_NUMBER
  log_reported_datetime = V16   # LOG_REPORTED_DATETIME
)]

# If desired, save the raw data files for future reference
if (first_run) {
  dir.create(here("Data", "Raw_do_not_write", "state_databases", "Maine", "LUST"), 
             recursive = TRUE, showWarnings = FALSE)
  fwrite(tank_raw, here("Data", "Raw_do_not_write", "state_databases", "Maine", "LUST", 
                       "tank_raw.csv"))
  fwrite(spill_report_raw, here("Data", "Raw_do_not_write", "state_databases", "Maine", "LUST", 
                               "spill_report_raw.csv"))
  fwrite(spill_log_raw, here("Data", "Raw_do_not_write", "state_databases", "Maine", "LUST", 
                            "spill_log_raw.csv"))
}


# ----------------------------------------
# Filter and Merge Data
# ----------------------------------------

# 1. Filter for federally regulated USTs in the tank table
#    (Assuming ust_registered_flag "T" indicates federally regulated)
tank_federal <- tank_raw[ust_registered_flag == "T"]

# 2. Filter Spill_Report.txt for Final Report entries
#    When report_status_value is "Final Report" or "FR", we use modify_date as the NFA date.
spill_report_final <- spill_report_raw[report_status_value %in% c("Final Report", "FR")]

# 3. Merge using the common key "l_spill_number"
# Merge tank data with spill_log to get the reported date (log_reported_datetime)
merged1 <- merge(
  tank_federal, 
  spill_log_raw[, .(l_spill_number, log_reported_datetime)], 
  by = "l_spill_number", 
  all.x = TRUE
)

# Merge the result with the final report data to get the NFA date (modify_date)
merged2 <- merge(
  merged1, 
  spill_report_final[, .(l_spill_number, modify_date)], 
  by = "l_spill_number", 
  all.x = TRUE
)
# ----------------------------------------
# Select and Rename the Variables Using data.table Syntax
# ----------------------------------------
# Final fields:
#   facility_id   from UST_TANK_SITE_NUMBER (Report_Tank_Involved.txt)
#   lust_id       from L.SPILL_NUMBER         (unique spill identifier)
#   reported_date from LOG_REPORTED_DATETIME   (Spill_Log.txt)
#   nfa_date      from MODIFY_DATE             (Spill_Report.txt, for Final Report)
ME_lust_raw <- merged2[, .(
  facility_id   = UST_TANK_SITE_NUMBER,  # this is the correct variable name from merged2
  lust_id       = l_spill_number,        # change L.SPILL_NUMBER to l_spill_number
  reported_date = log_reported_datetime, # change LOG_REPORTED_DATETIME to log_reported_datetime
  nfa_date      = modify_date            # change MODIFY_DATE to modify_date
)] %>% clean_names() %>% as.data.table()
# Build the final dataset selecting the fields:
#   facility_id: from ust_tank_site_number (the facility identifier)
#   lust_id: from l_spill_number (the spill/leak identifier)
#   reported_date: from log_reported_datetime
#   nfa_date: from modify_date (when the report is a Final Report)

# Save the raw LUST data
if(first_run){
  fwrite(ME_lust_raw, here("Data", "Raw_do_not_write", "state_databases", "Maine", "Maine_LUST_data.csv"))
}

# Print summaries of loaded data
cat("\nLoaded", nrow(ME_tanks_raw), "tank records and", nrow(ME_lust_raw), "LUST records\n")
cat("\nTank data column names:", paste(names(ME_tanks_raw), collapse = ", "), "\n")
cat("\nLUST data column names:", paste(names(ME_lust_raw), collapse = ", "), "\n")

# Process Tank Data -----
cat("\nProcessing Maine tank data...\n")

# Check the column names to identify facility_id, tank_id, installation_date, closure_date, etc.
# This will need to be adjusted based on the actual column names in the Maine data
cat("\nExamining column names in Maine tank data...\n")
print(names(ME_tanks_raw))

# Assuming column mappings based on typical naming conventions:
# facility_id -> fac_id or facility_id
# tank_id -> tank_id or tank_no
# installation_date -> install_date or installation_date
# closure_date -> closure_date, removal_date, or close_date
# For now, we'll use placeholder column names and adjust as needed

# Select and rename columns according to the required format
ME_UST_tanks <- ME_tanks_raw %>%
  mutate(
    registration_number = as.character(registration_number),
    tank_number = as.character(tank_number)
  ) %>%
  select(
    registration_number,           # facility_id
    tank_number,                   # tank_id
    date_tank_installed,           # tank_installed_date
    tank_status_date,              # tank_closed_date when tank_status indicates tank is closed
 #   county= NA,                        # county_name
    tank_volume_in_gallons,        # capacity
    product_stored,                # substance
    tank_material_label,           # tank wall type
    tank_status_label              # tank status
  ) %>%
  rename(
    facility_id = registration_number,
    tank_id = tank_number,
    tank_installed_date = date_tank_installed,
    tank_closed_date = tank_status_date,
 #   county_name = county,
    capacity = tank_volume_in_gallons,
    tank_contents = product_stored,
    tank_structure = tank_material_label,
    status = tank_status_label
  ) %>%
  as.data.table()

# Add state columns
ME_UST_tanks[, state := "Maine"]
ME_UST_tanks[, state_abbr := "ME"]

glimpse(ME_UST_tanks)

# Convert dates to proper format (assuming format in data, adjust as needed)
ME_UST_tanks[, tank_installed_date := lubridate::mdy(tank_installed_date)]
ME_UST_tanks[, tank_closed_date := lubridate::mdy(tank_closed_date)]

# Process tanks based on status description
cat("\nProcessing tank status...\n")
# drop all PLANNED FOR INSTALLATION tanks
ME_UST_tanks <- ME_UST_tanks[status != "PLANNED FOR INSTALLATION"]
# Classify tank status based on the status field
ME_UST_tanks[, tank_status := "Open"]  # Default to Open

# Then identify closed tanks based on specific conditions
ME_UST_tanks[grepl("Removed|Closed|Out of Service|Abandoned", status, ignore.case = TRUE), 
       tank_status := "Closed"]

# For closed tanks without a close date, set to NA and print a warning
missing_closed_dates <- sum(ME_UST_tanks$tank_status == "Closed" & is.na(ME_UST_tanks$tank_closed_date))
if(missing_closed_dates > 0) {
  cat("\nWARNING:", missing_closed_dates, "closed tanks have missing closure dates\n")
}

# Apply standardization functions
cat("\nStandardizing capacity values...\n")
ME_UST_tanks <- standardize_capacity(ME_UST_tanks, "capacity")

cat("\nClassifying tank wall types...\n")
ME_UST_tanks <- classify_tank_walls(ME_UST_tanks, "tank_structure")

cat("\nClassifying substance types...\n")
ME_UST_tanks <- classify_substances(ME_UST_tanks, "tank_contents")

# Add county FIPS codes
cat("\nAdding county FIPS codes...\n")

# Add "ME" to the beginning of facility_id if it's not already there
ME_UST_tanks[, facility_id_census := paste0("ME", facility_id)]

# Try to use census geography data for Maine 
cat("\nLooking for Maine census geography file...\n")

# Define path pattern for the Maine census geography file
census_file_path <- Sys.glob("C:/Users/kaleb/Box/UST-Insurance/Data/Processed/Census_Geography/Maine_census_geography.csv")

cat(paste0("\nLoading census geography data for Maine from ", census_file_path[1], "\n"))
census_data <- fread(census_file_path[1])

# Convert facility_id to character for joining
census_data[, facility_id := as.character(facility_id)]

# Join with census data based on facility_id
ME_UST_tanks <- merge(
  ME_UST_tanks,
  census_data[, .(facility_id,county_name ,county_geoid)],
  by.x = "facility_id_census",
  by.y = "facility_id",
  all.x = TRUE
)

# Rename county_geoid to county_fips
setnames(ME_UST_tanks, "county_geoid", "county_fips")

# Verify join success
cat("\nAdded census geography data for Maine\n")
cat("Total facilities:", uniqueN(ME_UST_tanks$facility_id), "\n")
cat("Facilities with county FIPS codes:", uniqueN(ME_UST_tanks[!is.na(county_fips), facility_id]), "\n")
cat("Match rate:", round(uniqueN(ME_UST_tanks[!is.na(county_fips), facility_id]) / uniqueN(ME_UST_tanks$facility_id) * 100, 2), "%\n")

# For records without county FIPS codes, they will remain NA
if(sum(is.na(ME_UST_tanks$county_fips)) > 0) {
  cat("\nWARNING:", sum(is.na(ME_UST_tanks$county_fips)), "records have missing county FIPS codes\n")
}

# Process LUST Data -----
cat("\nProcessing Maine LUST data...\n")

# Process the LUST data
ME_LUST_SD <- ME_lust_raw %>%
  mutate(
    facility_id = facility_id,  # Adjust as needed based on actual column name
    LUST_id = lust_id,          # Adjust as needed based on actual column name
    report_date = lubridate::mdy(reported_date),
    nfa_date = lubridate::mdy(nfa_date)               # NFA date might not be available
  ) %>%
  select(facility_id, LUST_id, report_date, nfa_date) %>%
  as.data.table()

# Join LUST data with tank data
cat("\nJoining LUST data with tank data...\n")

ME_LUST_SD_collapsed <- ME_LUST_SD[, .(LUST_count = .N), by = .(facility_id, report_date, nfa_date)]
glimpse(ME_LUST_SD_collapsed)
glimpse(ME_UST_tanks)   
ME_UST_tanks[,facility_id:=as.integer(facility_id)]
ME_UST_tanks_LUST <- merge(
  ME_UST_tanks, 
  ME_LUST_SD_collapsed[, .(facility_id, LUST_count, report_date, nfa_date)],
  by = "facility_id",
  all.x = TRUE,
  allow.cartesian = TRUE
)

# Create leak indicators
ME_UST_tanks_LUST <- ME_UST_tanks_LUST %>%
  mutate(
    leak_after_closure = ifelse(!is.na(tank_closed_date) & !is.na(report_date) & 
                               report_date > tank_closed_date, 1, 0),
    leak_before_NFA_before_closure = ifelse(!is.na(tank_closed_date) & !is.na(report_date) & 
                                           report_date < tank_closed_date & 
                                           (is.na(nfa_date) | nfa_date < tank_closed_date), 1, 0),
    leak_before_NFA_after_closure = ifelse(!is.na(tank_closed_date) & !is.na(report_date) & 
                                          report_date < tank_closed_date & 
                                          !is.na(nfa_date) & nfa_date > tank_closed_date, 1, 0),
    no_leak = ifelse(is.na(report_date) & !is.na(tank_closed_date), 1, 0)
  ) %>%
  as.data.table()

# Aggregate leak indicators at facility-tank level - including county_fips
ME_UST_tanks_SD <- ME_UST_tanks_LUST %>%
  group_by(facility_id, tank_id, tank_installed_date, tank_closed_date, county_name, county_fips, state, state_abbr) %>%
  summarize(
    leak_after_closure = sum(leak_after_closure, na.rm = TRUE),
    leak_before_NFA_before_closure = sum(leak_before_NFA_before_closure, na.rm = TRUE),
    leak_before_NFA_after_closure = sum(leak_before_NFA_after_closure, na.rm = TRUE),
    no_leak = sum(no_leak, na.rm = TRUE),
    # Take maximum value for binary flags
    is_gasoline = max(is_gasoline, na.rm = TRUE),
    is_diesel = max(is_diesel, na.rm = TRUE),
    is_oil_kerosene = max(is_oil_kerosene, na.rm = TRUE),
    is_jet_fuel = max(is_jet_fuel, na.rm = TRUE),
    is_other = max(is_other, na.rm = TRUE),
    single_walled = max(single_walled, na.rm = TRUE),
    double_walled = max(double_walled, na.rm = TRUE),
    unknown_walled = max(unknown_walled, na.rm = TRUE),
    missing_walled = max(missing_walled, na.rm = TRUE),
    capacity = mean(capacity, na.rm = TRUE)
  ) %>%
  as.data.table()

# Convert binary indicators to 0/1
ME_UST_tanks_SD <- ME_UST_tanks_SD[, .(
  leak_after_closure = ifelse(leak_after_closure > 0, 1, 0),
  leak_before_NFA_before_closure = ifelse(leak_before_NFA_before_closure > 0, 1, 0),
  leak_before_NFA_after_closure = ifelse(leak_before_NFA_after_closure > 0, 1, 0),
  no_leak = ifelse(no_leak > 0, 1, 0),
  is_gasoline = is_gasoline,
  is_diesel = is_diesel,
  is_oil_kerosene = is_oil_kerosene,
  is_jet_fuel = is_jet_fuel,
  is_other = is_other,
  single_walled = single_walled,
  double_walled = double_walled,
  unknown_walled = unknown_walled,
  missing_walled = missing_walled,
  capacity = capacity
), keyby = .(facility_id, tank_id, tank_installed_date, tank_closed_date, county_name, county_fips, state, state_abbr)]

# Save final datasets
cat("\nSaving final Maine datasets...\n")

# Create directory if it doesn't exist
dir_path <- here("Data", "Raw_do_not_write", "state_databases", "Maine")
if (!dir.exists(dir_path)) {
  dir.create(dir_path, recursive = TRUE)
}

# Write tank data
fwrite(ME_UST_tanks_SD, here("Data", "Raw_do_not_write", "state_databases", "Maine", "ME_UST_tanks.csv"))

# Write LUST data
fwrite(ME_LUST_SD, here("Data", "Raw_do_not_write", "state_databases", "Maine", "ME_LUST.csv"))

cat("\nMaine data processing complete\n")
cat("Tank records:", nrow(ME_UST_tanks_SD), "\n")
cat("LUST records:", nrow(ME_LUST_SD), "\n")

# Count closed tanks
num_closed_tanks_ME = uniqueN(ME_UST_tanks_SD[!is.na(tank_closed_date), paste0(facility_id, "-", tank_id)])
cat("Closed tanks:", num_closed_tanks_ME, "\n")

# Count leaks
count_leak_after_closure = ME_UST_tanks_SD[leak_after_closure == 1, uniqueN(paste0(facility_id, "-", tank_id))]
count_leak_before_NFA_before_closure = ME_UST_tanks_SD[leak_before_NFA_before_closure == 1, uniqueN(paste0(facility_id, "-", tank_id))]
count_leak_before_NFA_after_closure = ME_UST_tanks_SD[leak_before_NFA_after_closure == 1, uniqueN(paste0(facility_id, "-", tank_id))]
count_no_leak = ME_UST_tanks_SD[no_leak == 1, uniqueN(paste0(facility_id, "-", tank_id))]

ME_counts = data.frame(
  category = c("leak_after_closure", "leak_before_NFA_before_closure", "leak_before_NFA_after_closure", "no_leak"),
  count = c(count_leak_after_closure, count_leak_before_NFA_before_closure, count_leak_before_NFA_after_closure, count_no_leak)
)
ME_counts$total = sum(ME_counts$count)
ME_counts$percent = (ME_counts$count / ME_counts$total) * 100

cat("\nMaine leak statistics:\n")
print(ME_counts)
