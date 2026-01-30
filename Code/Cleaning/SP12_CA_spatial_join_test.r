# ==============================================================================
# 26_CA_Spatial_Address_Standardization_NoPostmastr.R
# Purpose: Hybrid Spatial/Semantic match without postmastr.
#          1. Pre-filter coordinates for NA values (Fixes st_as_sf error).
#          2. 75m Spatial Buffer to generate candidates.
#          3. Manual string normalization + tidygeocoder API standardization.
#          4. High-speed selection via data.table grouping.
# ==============================================================================

library(data.table)
library(tidyverse)
library(here)
library(sf)
library(tidygeocoder)
library(stringdist)

# 1. Setup & Robust Data Ingestion ---------------------------------------------
raw_dir <- here("Data", "Raw", "state_databases", "California")
proc_dir <- here("Data", "Processed")

load_ca <- function(f) {
  fread(f, sep = "\t", quote = "", fill = TRUE, colClasses = "character", 
        na.strings = c("", "NA", "NULL")) %>% 
    janitor::clean_names()
}

dt_inv  <- load_ca(file.path(raw_dir, "permitted_ust.txt"))
dt_lust <- load_ca(file.path(raw_dir, "sites.txt"))[case_type == "LUST Cleanup Site"]

# 2. Address Normalizer (Manual postmastr Alternative) -------------------------
message("Standardizing address strings via regex...")
standardize_manual <- function(x) {
  x <- toupper(x)
  # Standardize Directionals
  x <- gsub("\\bN\\.?\\b", "NORTH", x)
  x <- gsub("\\bS\\.?\\b", "SOUTH", x)
  x <- gsub("\\bE\\.?\\b", "EAST", x)
  x <- gsub("\\bW\\.?\\b", "WEST", x)
  # Standardize Common Road Types
  x <- gsub("\\bHWY\\b|\\bHIWAY\\b", "HIGHWAY", x)
  x <- gsub("\\bRD\\b", "ROAD", x)
  x <- gsub("\\bST\\b", "STREET", x)
  x <- gsub("\\bBLVD\\b", "BOULEVARD", x)
  x <- gsub("\\bAVE\\b", "AVENUE", x)
  # Strip punctuation and collapse whitespace
  x <- gsub("[[:punct:]]", " ", x)
  return(trimws(gsub("\\s+", " ", x)))
}

# 3. Spatial Candidate Search (75m) --------------------------------------------
message("Executing spatial neighborhood search...")

# CRITICAL: Strip NA/Zero coordinates to prevent st_as_sf crash
inv_geo <- dt_inv[!is.na(as.numeric(latitude)) & as.numeric(latitude) > 32]
lust_geo <- dt_lust[!is.na(as.numeric(latitude)) & as.numeric(latitude) > 32]

# Project to California Albers (Meters)
sf_inv <- st_as_sf(inv_geo, coords = c("longitude", "latitude"), crs = 4326) %>% st_transform(3310)

# Sample 100 for the test
set.seed(42)
dt_lust_sample <- lust_geo[sample(.N, 100)]
sf_lust_sample <- st_as_sf(dt_lust_sample, coords = c("longitude", "latitude"), crs = 4326) %>% st_transform(3310)

# Find all sites within 75m of each sample leak
hits <- st_is_within_distance(sf_lust_sample, sf_inv, dist = 75)

# Convert to data.table of candidates
dt_candidates <- data.table(
  lust_idx = rep(seq_along(hits), lengths(hits)),
  inv_idx  = unlist(hits)
)

if (nrow(dt_candidates) == 0) stop("No spatial candidates found within 75m.")

# Map raw data to candidates
dt_candidates[, `:=`(
  lust_id   = dt_lust_sample$global_id[lust_idx],
  lust_addr = standardize_manual(paste(dt_lust_sample$street_number[lust_idx], dt_lust_sample$street_name[lust_idx])),
  site_id   = inv_geo$cersid[inv_idx],
  site_addr = standardize_manual(inv_geo$address[inv_idx])
)]

# 4. Tidygeocoder External Fix -------------------------------------------------
message("Performing API address standardization...")

unique_addrs <- unique(c(dt_candidates$lust_addr, dt_candidates$site_addr))
std_map <- tibble(raw_addr = unique_addrs) %>%
  geocode(address = raw_addr, method = 'census', full_results = TRUE) %>%
  select(raw_addr, std_addr = matched_address) %>%
  as.data.table()

# Merge back
dt_candidates <- merge(dt_candidates, std_map, by.x = "lust_addr", by.y = "raw_addr", all.x = TRUE)
setnames(dt_candidates, "std_addr", "lust_std")
dt_candidates <- merge(dt_candidates, std_map, by.x = "site_addr", by.y = "raw_addr", all.x = TRUE)
setnames(dt_candidates, "std_addr", "site_std")

# 5. Semantic Scoring & Selection ----------------------------------------------
message("Calculating match scores and selecting winners...")

# Logic: Jaro-Winkler distance on standardized addresses
dt_candidates[, match_score := stringdist(lust_std, site_std, method = "jw")]

# Also flag house number match as an auxiliary indicator
dt_candidates[, house_match := ifelse(str_extract(lust_addr, "^[0-9]+") == str_extract(site_addr, "^[0-9]+"), 1, 0)]

# Select the best match per leak: Prioritize House Number then lowest String Distance
final_matches <- dt_candidates[order(lust_id, -house_match, match_score), .SD[1], by = lust_id]

# 6. Save Results --------------------------------------------------------------
fwrite(final_matches, file.path(proc_dir, "CA_Final_Hybrid_Match_100_NoPostmastr.csv"))
message("Processing complete. Probe saved.")

