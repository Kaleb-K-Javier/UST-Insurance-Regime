## Alabama UST Dataset Creation
# Script: 10_Clean_AL.R
# Author: Validated Classification Logic (Updated Jan 2026)
# Description: Harmonizes AL UST data for econometrics.
#
# Key Features:
#   1. ID Logic: Parses Permit Number "A-B-C" to link with Site ID "S[C]-[B]".
#   2. Regulatory Mandate: Implements Fed Secondary Containment (April 11, 2016).
#   3. Material Logic: Implements specific "Fiberglass Coated Steel" date cutoffs (1990).
#   4. Date Parsing: Handles Excel Serial dates robustly.

library(data.table)
library(tidyverse)
library(here)
library(janitor)
library(readxl)
library(stringr)
library(lubridate)

# 0. Setup ---------------------------------------------------------------------
state_abbr <- "AL"
al_path <- here("Data", "Raw", "state_databases", "Alabama")
out_dir <- here("Data", "Raw", "state_databases", "Alabama") # Keep in state folder for Master script

if (!dir.exists(al_path)) stop("Alabama data directory not found!")

# 1. Helper Functions ----------------------------------------------------------

# Vectorized Excel Date Parser
parse_excel_date <- function(x) {
  # Suppress warnings for non-numeric garbage
  num_val <- suppressWarnings(as.numeric(x))
  # Excel Origin: Dec 30, 1899
  as.Date(num_val, origin = "1899-12-30")
}

# 2. Load Data (Force Text) ----------------------------------------------------
message("Loading Alabama Data...")

# Tanks
dt_tanks <- read_excel(file.path(al_path, "UST_UTanks (2).xlsx"), col_types = "text") %>% 
  clean_names() %>% as.data.table()

# Sites (Geography)
dt_sites <- read_excel(file.path(al_path, "UST_Sites (2).xlsx"), col_types = "text") %>% 
  clean_names() %>% as.data.table()

# 3. Process Tank Attributes ---------------------------------------------------
message("Processing Tank Attributes...")

# --- A. Generate Link Keys ---
# Logic: Permit "16808-133-014739" -> Seq "133", Site "014739" -> Link "S14739-133"
# Use as.integer to strip leading zeros from the Site component
dt_tanks[, `:=`(
  part_seq  = as.integer(str_extract(permit_number, "(?<=-)[0-9]+(?=-)")),
  part_site = as.integer(str_extract(permit_number, "[0-9]+$"))
)]
dt_tanks[, site_link_id := paste0("S", part_site, "-", part_seq)]

# --- B. Parse Dates ---
dt_tanks[, `:=`(
  tank_installed_date = parse_excel_date(install_date),
  tank_closed_date    = parse_excel_date(removed_date)
)]

# --- C. Harmonize Status ---
dt_tanks[, tank_status := fcase(
  grepl("Current|Temp", tank_status, ignore.case=TRUE), "Open",
  grepl("Perm|Retire|Aband|Inert", tank_status, ignore.case=TRUE), "Closed",
  default = "Closed"
)]

# --- D. Harmonize Substance ---
# Combine raw and select for coverage
dt_tanks[, substance_lower := tolower(paste(petroleum_product_select, substance_stored, petroleum_product))]

dt_tanks[, `:=`(
  is_gasoline = as.integer(grepl("gasoline|gas|ethan|e85|e10|e-15|racing|avgas", substance_lower)),
  is_diesel   = as.integer(grepl("diesel|dsl|bio|b-99|b100|b-10|def", substance_lower)),
  is_oil_kerosene = as.integer(grepl("oil|kerosene|k1|lube|hydr|fluid", substance_lower)),
  is_jet_fuel = as.integer(grepl("jet|aviation|jp-4", substance_lower)),
  is_other    = 0
)]
dt_tanks[is_gasoline==0 & is_diesel==0 & is_oil_kerosene==0 & is_jet_fuel==0, is_other := 1]

# --- E. ALABAMA CLASSIFICATION ALGORITHM (Wall Type) ---
# Preparing fields
dt_tanks[, mat_upper := toupper(paste(tank_construction_material, tank_construction_material_select))]

# Regulatory Dates
fed_sec_date <- as.Date("2016-04-11") # Federal Requirement
frp_cutoff   <- as.Date("1990-01-01") # Industry standard for Double FRP
steel_cutoff <- as.Date("2000-01-01") # Modern Steel cutoff

dt_tanks[, wall_class := case_when(
  # 1. FEDERAL MANDATE (Post-2016)
  tank_installed_date >= fed_sec_date ~ "Double",
  
  # 2. EXPLICIT DOUBLE INDICATORS
  str_detect(mat_upper, "DOUBLE WALL|DBW|DBL|JACKET|2WALL|DOUBLEWALL") ~ "Double",
  
  # 3. COMPOSITE / FIBERGLASS COATED STEEL (Date Dependent)
  # Post-1990 = Double (Composite), Pre-1990 = Unknown (Might be coated single)
  str_detect(mat_upper, "FIBERGLASS COATED STEEL") & 
    !str_detect(mat_upper, "SINGLE") & 
    tank_installed_date >= frp_cutoff ~ "Double",
  
  str_detect(mat_upper, "FIBERGLASS COATED STEEL") & 
    !str_detect(mat_upper, "SINGLE") & 
    tank_installed_date < frp_cutoff ~ "Unknown",
  
  # 4. FRP (Date Dependent)
  str_detect(mat_upper, "FIBERGLASS REINFORCED PLASTIC") & 
    !str_detect(mat_upper, "SINGLE") & 
    tank_installed_date >= frp_cutoff ~ "Double",
  
  str_detect(mat_upper, "FIBERGLASS REINFORCED PLASTIC") & 
    !str_detect(mat_upper, "SINGLE") & 
    tank_installed_date < frp_cutoff ~ "Unknown",
  
  # 5. EXPLICIT SINGLE INDICATORS
  str_detect(mat_upper, "SINGLE|SGL") ~ "Single",
  
  # 6. BRAND NAMES
  str_detect(mat_upper, "STIP3|STI-P3") ~ "Single",
  str_detect(mat_upper, "ACT-100|ACT 100|ACT100|GLASTEEL|TITAN|PERMATANK|ELUTRON|TCI") ~ "Double",
  
  # 7. PLAIN STEEL (Date Analysis)
  # Post-2016 = Double (Rule 1 covers this, but explicit here)
  # Pre-2016 = Single (No state mandate for secondary containment on steel before Fed rule)
  str_detect(mat_upper, "^STEEL") & tank_installed_date < fed_sec_date ~ "Single",
  
  # 8. MISSING DATA (With Date)
  (is.na(mat_upper) | mat_upper == "NA NA") & tank_installed_date >= fed_sec_date ~ "Double",
  
  # 9. DEFAULT
  TRUE ~ "Unknown"
)]

# Convert to Flags
dt_tanks[, `:=`(
  double_walled = as.integer(wall_class == "Double"),
  single_walled = as.integer(wall_class == "Single"),
  unknown_walled = as.integer(wall_class == "Unknown")
)]

# --- F. Capacity ---
# AL data does not provide a clean capacity column. Set to NA to avoid bad parsing.
dt_tanks[, capacity := NA_real_]

# 4. Geography Merge -----------------------------------------------------------
message("Merging Geography...")

# Clean Sites
dt_sites_clean <- dt_sites[, .(
  site_link_id = number, 
  facility_name = name,
  latitude = as.numeric(latitude),
  longitude = as.numeric(longitude),
  address_raw = address,
  city = city,
  zip_code = zip,
  county_name = county
)]

# Left Join
harmonized <- merge(dt_tanks, dt_sites_clean, by = "site_link_id", all.x = TRUE)

# 5. LUST Placeholder ----------------------------------------------------------
# AL LUST files were not provided in this batch.
# We populate leak columns with 0/NA to allow merging into the Master Panel.
# NOTE: If EPA LUST data is added later, this section would be replaced by a merge.
message("Initializing Leak Columns (No Local LUST File)...")
harmonized[, `:=`(
  leak_after_closure = 0,
  leak_before_NFA_before_closure = 0,
  leak_before_NFA_after_closure = 0,
  no_leak = NA_integer_ # We don't know if they leaked or not without the file
)]

# 6. Final Formatting ----------------------------------------------------------
message("Finalizing Dataset...")

final_df <- harmonized[, .(
  facility_id = permit_number, # Unique Key
  tank_id = tank_identification_number,
  facility_name, 
  street_address = address_raw,
  city,
  zip_code,
  county_name,
  state = state_abbr,
  
  latitude, longitude,
  
  tank_status,
  tank_installed_date,
  tank_closed_date,
  
  capacity,
  
  is_gasoline, is_diesel, is_oil_kerosene, is_jet_fuel, is_other,
  
  single_walled, double_walled, unknown_walled,
  
  leak_after_closure, 
  leak_before_NFA_before_closure, 
  leak_before_NFA_after_closure, 
  no_leak
)]

# 7. Save ----------------------------------------------------------------------
outfile <- file.path(out_dir, paste0(state_abbr, "_Harmonized_UST_tanks.csv"))
fwrite(final_df, outfile)

cat(paste0("\n------------------------------------------------\n"))
cat(paste0("ALABAMA PROCESSING COMPLETE\n"))
cat(paste0("------------------------------------------------\n"))
cat(paste0("Output File:   ", outfile, "\n"))
cat(paste0("Total Tanks:   ", nrow(final_df), "\n"))
cat(paste0("Double Walled: ", round(mean(final_df$double_walled)*100,1), "%\n"))
cat(paste0("Single Walled: ", round(mean(final_df$single_walled)*100,1), "%\n"))
cat(paste0("Geocoded:      ", round(mean(!is.na(final_df$latitude))*100,1), "%\n"))

cat(paste0("------------------------------------------------\n"))