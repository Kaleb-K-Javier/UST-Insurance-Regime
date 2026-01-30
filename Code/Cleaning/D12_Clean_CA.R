# ==============================================================================
# D13_Diagnostic_CA_v4.R
# Purpose: Deep Audit of CA UST Data (Permitted Inventory + LUST History).
#          * ENCODING SAFE VERSION *
#          1. Load all 6 Tab-Delimited text files.
#          2. Print Full Glimpse (Head 20) & Structure.
#          3. Generate Full Frequency Tables.
#          4. Audit Lat/Long Availability (Spatial Match Feasibility).
#          5. Test "Rigorous" Address Matching (Strip N/S, Blvd/St).
# ==============================================================================

library(data.table)
library(tidyverse)
library(here)
library(janitor)
library(lubridate)
library(stringr)

# 1. Setup & Config ------------------------------------------------------------
# FORCE R TO SHOW EVERYTHING
options(width = 300)                # Prevent column wrapping
options(datatable.print.class = TRUE)
options(max.print = 2000)           # Allow large prints

# Paths
raw_dir  <- here("Data", "Raw", "state_databases", "California")
log_file <- file.path(raw_dir, "CA_Full_Diagnostic_Log.txt")

# Define File Map
files <- list(
  # Inventory (Key: CERSID)
  Tanks    = file.path(raw_dir, "permitted_ust_tanks.txt"),
  Sites    = file.path(raw_dir, "permitted_ust.txt"),
  
  # History (Key: GLOBAL_ID)
  GeoSites = file.path(raw_dir, "sites.txt"),
  Status   = file.path(raw_dir, "status_history.txt"),
  RegActs  = file.path(raw_dir, "regulatory_activities.txt"),
  Contacts = file.path(raw_dir, "contacts.txt")
)

# Start Logging
sink(log_file)
cat("================================================================================\n")
cat("CALIFORNIA DEEP DATA AUDIT (V4 - ENCODING SAFE)\n")
cat("Generated: ", as.character(Sys.time()), "\n")
cat("================================================================================\n\n")

# 2. Loading Function (Robust for CA Tab-Delimited) ----------------------------
load_ca_file <- function(fpath, name) {
  if(!file.exists(fpath)) {
    cat(sprintf("\n[MISSING] %s not found at: %s\n", name, fpath))
    return(NULL)
  }
  
  cat(sprintf("\n[LOADING] %s...", name))
  
  # CRITICAL: sep="\t", quote="" handles the unescaped quotes/commas in CA data
  dt <- fread(fpath, sep = "\t", quote = "", fill = TRUE, 
              colClasses = "character", na.strings = c("", "NA", "NULL")) %>% 
    clean_names()
  
  cat(sprintf(" Success. Rows: %s | Cols: %s\n", 
              format(nrow(dt), big.mark=","), ncol(dt)))
  return(dt)
}

# 3. Load Data -----------------------------------------------------------------
data_list <- list()
for (name in names(files)) {
  data_list[[name]] <- load_ca_file(files[[name]], name)
}

# 4. Deep Glimpse (Head 20) ----------------------------------------------------
cat("\n\n================================================================================\n")
cat("PART 2: RAW DATA GLIMPSE (FIRST 20 ROWS)\n")
cat("================================================================================\n")

for (name in names(data_list)) {
  dt <- data_list[[name]]
  if (!is.null(dt)) {
    cat(sprintf("\n>>> DATASET: %s <<<\n", name))
    
    # Print Column Names
    cat("Columns:\n")
    print(names(dt))
    
    # Print Head (20 rows, first 15 cols to fit screen)
    cat("\nFirst 20 Rows:\n")
    cols_to_show <- names(dt)[1:min(15, ncol(dt))]
    print(head(dt[, ..cols_to_show], 20))
  }
}

# 5. Full Frequency Tables (Key Attributes) ------------------------------------
cat("\n\n================================================================================\n")
cat("PART 3: FREQUENCY TABLES (KEY ATTRIBUTES)\n")
cat("================================================================================\n")

# A. PERMITTED TANKS (Inventory)
dt_tanks <- data_list$Tanks
if (!is.null(dt_tanks)) {
  cat("\n--- [Inventory] Tank Construction (Wall Type) ---\n")
  if("tank_pc_construction" %in% names(dt_tanks)) {
    print(dt_tanks[, .N, by = tank_pc_construction][order(-N)], nrows=Inf)
  }
  
  cat("\n--- [Inventory] Tank Contents ---\n")
  if("tank_contents" %in% names(dt_tanks)) {
    print(dt_tanks[, .N, by = tank_contents][order(-N)], nrows=Inf)
  }
  
  cat("\n--- [Inventory] Tank Status ---\n")
  if("tank_status" %in% names(dt_tanks)) {
    print(dt_tanks[, .N, by = tank_status][order(-N)], nrows=Inf)
  }
  
  cat("\n--- [Inventory] Installation Years (Parsed Sample) ---\n")
  if("tank_installation_date" %in% names(dt_tanks)) {
    # Quick parse test
    dates <- parse_date_time(head(dt_tanks$tank_installation_date, 50), orders = c("mdY HMS", "mdY"))
    print(table(year(dates)))
  }
}

# B. GEOTRACKER SITES (History)
dt_geo <- data_list$GeoSites
if (!is.null(dt_geo)) {
  cat("\n--- [History] Case Type (Use to filter LUST) ---\n")
  if("case_type" %in% names(dt_geo)) {
    print(dt_geo[, .N, by = case_type][order(-N)], nrows=Inf)
  }
  
  cat("\n--- [History] Status (LUST Cleanup Sites Only) ---\n")
  if("case_type" %in% names(dt_geo) && "status" %in% names(dt_geo)) {
    lust_only <- dt_geo[case_type == "LUST Cleanup Site"]
    print(lust_only[, .N, by = status][order(-N)], nrows=Inf)
  }
}

# 6. Spatial Availability Audit ------------------------------------------------
cat("\n\n================================================================================\n")
cat("PART 4: SPATIAL DATA AUDIT (LAT/LONG AVAILABILITY)\n")
cat("================================================================================\n")

check_coords <- function(dt, name) {
  cat(sprintf("\nDataset: %s\n", name))
  
  # Identify Lat/Long columns
  lat_col <- grep("latitude", names(dt), value=TRUE)[1]
  lon_col <- grep("longitude", names(dt), value=TRUE)[1]
  
  if(is.na(lat_col) || is.na(lon_col)) {
    cat("  [FAIL] Lat/Long columns not found.\n")
    return()
  }
  
  # Check Validity (Non-NA, Non-Zero)
  valid_mask <- !is.na(as.numeric(dt[[lat_col]])) & 
                !is.na(as.numeric(dt[[lon_col]])) & 
                as.numeric(dt[[lat_col]]) != 0
  
  valid_count <- sum(valid_mask)
  pct <- round(100 * valid_count / nrow(dt), 1)
  
  cat(sprintf("  Valid Coordinates: %s / %s (%s%%)\n", 
              format(valid_count, big.mark=","), 
              format(nrow(dt), big.mark=","), pct))
  
  # Check Bounds (Rough CA Box)
  in_bounds_mask <- valid_mask & 
                    as.numeric(dt[[lat_col]]) > 32 & 
                    as.numeric(dt[[lon_col]]) < -114
  
  cat(sprintf("  In CA Bounds:      %s\n", format(sum(in_bounds_mask), big.mark=",")))
}

if(!is.null(data_list$Sites))    check_coords(data_list$Sites, "Inventory (Permitted Facilities)")
if(!is.null(data_list$GeoSites)) check_coords(data_list$GeoSites, "History (GeoTracker Sites)")


# 7. Rigorous Address Match & ID Test ------------------------------------------
cat("\n\n================================================================================\n")
cat("PART 5: RIGOROUS MATCHING TEST (ID + SMART ADDRESS)\n")
cat("================================================================================\n")

dt_perm <- data_list$Sites # Inventory
dt_geo  <- data_list$GeoSites   # History

if (!is.null(dt_perm) && !is.null(dt_geo)) {
  
  # --- TEST A: Direct ID Match ---
  cat("\nTest A: Looking for Shared Columns...\n")
  if("global_id" %in% names(dt_perm)) {
    cat("  [SUCCESS] 'global_id' found in Permitted Inventory!\n")
    matches <- sum(dt_perm$global_id %in% dt_geo$global_id)
    cat(sprintf("  Exact ID Matches: %s / %s\n", format(matches, big.mark=","), nrow(dt_perm)))
  } else {
    cat("  [FAIL] 'global_id' NOT found in Permitted Inventory.\n")
  }
  
  # --- TEST B: Rigorous Address Match ---
  cat("\nTest B: Rigorous Address Normalization...\n")
  
  # Normalizer Function (Strips N/S, Blvd/St, Punctuation)
  normalize_addr <- function(x) {
    # FIX: Handle invalid characters by forcing ASCII
    x <- iconv(x, to = "ASCII", sub = " ") 
    x <- toupper(x)
    
    # Remove Directionals
    x <- gsub("\\b(N|S|E|W|NORTH|SOUTH|EAST|WEST)\\b", "", x)
    # Remove Suffixes
    x <- gsub("\\b(ROAD|RD|STREET|ST|AVENUE|AVE|BOULEVARD|BLVD|LANE|LN|DRIVE|DR|WAY|COURT|CT|PL|PLACE)\\b", "", x)
    # Remove Unit Numbers (STE 100, #5)
    x <- gsub("\\b(STE|SUITE|UNIT|APT|#)[0-9A-Z]*\\b", "", x)
    # Remove non-alphanumeric
    x <- gsub("[^A-Z0-9]", "", x)
    # Truncate
    return(str_sub(x, 1, 15))
  }
  
  # Prepare Keys
  cat("  Generating Clean Keys (This may take a moment)...\n")
  dt_perm[, clean_key := normalize_addr(address)]
  
  dt_geo[, full_addr := paste(street_number, street_name)]
  dt_geo[, clean_key := normalize_addr(full_addr)]
  
  # Execute Match
  matches_addr <- merge(dt_perm, dt_geo, by="clean_key")
  unique_matches <- unique(matches_addr, by="cersid") # One match per inventory site
  
  cat(sprintf("  Permitted Rows:      %s\n", format(nrow(dt_perm), big.mark=",")))
  cat(sprintf("  GeoTracker Rows:     %s\n", format(nrow(dt_geo), big.mark=",")))
  cat(sprintf("  Address Matches:     %s (%.1f%%)\n", 
              format(nrow(unique_matches), big.mark=","),
              100 * nrow(unique_matches) / nrow(dt_perm)))
  
  cat("\nSample Matches (Check Validity):\n")
  print(head(matches_addr[, .(Inventory=address, History=full_addr, Key=clean_key)], 10))
  
  cat("\nSample MISSING (Inventory sites that failed match):\n")
  missing <- dt_perm[!cersid %in% unique_matches$cersid]
  print(head(missing[, .(Business=business_name, Address=address, Key=clean_key)], 10))
}

sink()
message("Diagnostic Log Saved: ", log_file)