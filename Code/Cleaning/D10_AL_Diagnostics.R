# ==============================================================================
# 10_Diagnostic_AL_Full.R
# Purpose: Complete audit of Alabama UST data.
#   1. Content Inspection: Frequency tables for Status, Substance, Construction.
#   2. Structure Check: Glimpses and sample rows.
#   3. Linkage Stress Test: Validating the Permit -> Site ID logic (S[Site]-[Seq]).
# Output: 'AL_Full_Diagnostic_Report.txt'
# ==============================================================================

library(data.table)
library(tidyverse)
library(here)
library(janitor)
library(readxl)
library(stringr)

# 1. Setup & Paths -------------------------------------------------------------
al_path <- here("Data", "Raw", "state_databases", "Alabama")
log_file <- file.path(al_path, "AL_Full_Diagnostic_Report.txt")

# Define files map
files <- list(
  IDs    = "FacilityIDs (1).xlsx",
  Owners = "UST_Owners (2).xlsx",
  Sites  = "UST_Sites (2).xlsx",
  Tanks  = "UST_UTanks (2).xlsx"
)

# Start Logging
sink(log_file)
cat("================================================================================\n")
cat("ALABAMA FULL DATA DIAGNOSTIC REPORT\n")
cat("Generated: ", as.character(Sys.time()), "\n")
cat("================================================================================\n\n")

# 2. Load Data -----------------------------------------------------------------
cat("\n--- 1. LOADING FILES ---\n")

data_list <- list()

for (name in names(files)) {
  fpath <- file.path(al_path, files[[name]])
  if (file.exists(fpath)) {
    cat(paste0("Loading ", name, " (", files[[name]], ")... "))
    tryCatch({
      # Read as text to capture raw formats (e.g., "01" vs "1")
      data_list[[name]] <- read_excel(fpath, col_types = "text") %>% 
        clean_names() %>% 
        as.data.table()
      cat("Success. Rows:", nrow(data_list[[name]]), "\n")
    }, error = function(e) {
      cat("Failed: ", e$message, "\n")
    })
  } else {
    cat("MISSING: ", files[[name]], "\n")
  }
}

# 3. Content Inspection (Structure & Frequencies) ------------------------------
cat("\n\n================================================================================\n")
cat("PART 2: CONTENT INSPECTION (STRUCTURE & CODES)\n")
cat("================================================================================\n")

# A. Glimpses
for (name in names(data_list)) {
  dt <- data_list[[name]]
  cat(paste0("\n--- DATASET: ", name, " ---\n"))
  glimpse(dt)
  cat("\nHead (First 5 Rows):\n")
  print(head(dt, 5))
}

# B. Tank Variable Frequencies (The "Rosetta Stone")
if (!is.null(data_list$Tanks)) {
  dt <- data_list$Tanks
  cat("\n--- TANK VARIABLE FREQUENCIES ---\n")
  
  # Helper to check and print freq table
  check_freq <- function(data, pattern, label) {
    cols <- grep(pattern, names(data), value = TRUE, ignore.case = TRUE)
    if (length(cols) > 0) {
      for (col in cols) {
        cat(paste0("\nFrequency Table: ", label, " (", col, ")\n"))
        print(data[, .N, by = get(col)][order(-N)])
      }
    }
  }
  
  check_freq(dt, "status", "STATUS")
  check_freq(dt, "content|substance|product", "SUBSTANCE")
  check_freq(dt, "construct|material|wall", "CONSTRUCTION")
  
  # Date Samples
  date_cols <- grep("date|install|close", names(dt), value = TRUE, ignore.case = TRUE)
  if(length(date_cols) > 0) {
    cat("\n--- DATE FORMAT SAMPLES (Top 10 non-missing) ---\n")
    for (col in date_cols) {
      cat(paste0("Sample of '", col, "':\n"))
      print(head(dt[[col]][!is.na(dt[[col]])], 10))
    }
  }
}

# C. Site Geography Check
if (!is.null(data_list$Sites)) {
  dt <- data_list$Sites
  cat("\n--- SITE GEOGRAPHY CHECK ---\n")
  
  geo_cols <- grep("lat|long", names(dt), value = TRUE, ignore.case = TRUE)
  for (col in geo_cols) {
    cat(paste0("Sample of '", col, "':\n"))
    print(head(dt[[col]], 10))
  }
}

# 4. Linkage Stress Test (Permit -> Site Logic) --------------------------------
cat("\n\n================================================================================\n")
cat("PART 3: LINKAGE STRESS TEST (PERMIT -> SITE ID)\n")
cat("================================================================================\n")
cat("Hypothesis: Permit 'A-B-C' maps to Site 'S[C_int]-[B_int]' (Stripping Leading Zeros)\n")
cat("Example: '10014-091-000386' -> 'S386-91'\n\n")

if (!is.null(data_list$Tanks) && !is.null(data_list$Sites)) {
  
  # Work with unique permits from Tanks
  map_test <- data.table(permit_number = unique(data_list$Tanks$permit_number))
  
  # A. PARSE PERMIT NUMBER
  # Split "16808-133-014739" into 3 parts
  map_test[, `:=`(
    part_acct = str_extract(permit_number, "^[0-9]+"),
    part_seq  = str_extract(permit_number, "(?<=-)[0-9]+(?=-)"),
    part_site = str_extract(permit_number, "[0-9]+$")
  )]
  
  # B. CONSTRUCT TARGET SITE ID (NO PADDING)
  # Logic: "S" + integer(part_site) + "-" + integer(part_seq)
  map_test[, target_site_id := paste0("S", as.integer(part_site), "-", as.integer(part_seq))]
  
  # C. CHECK AGAINST ACTUAL SITE FILE
  actual_site_ids <- unique(data_list$Sites$number)
  
  map_test[, match_found := target_site_id %in% actual_site_ids]
  
  # D. RESULTS
  success_count <- sum(map_test$match_found, na.rm = TRUE)
  total_count   <- nrow(map_test)
  success_rate  <- round(success_count / total_count * 100, 1)
  
  cat(paste0("Total Unique Permits in Tanks: ", total_count, "\n"))
  cat(paste0("Successfully Generated Valid Site ID: ", success_count, "\n"))
  cat(paste0("SUCCESS RATE: ", success_rate, "%\n"))
  
  # E. DIAGNOSE FAILURES
  if (success_rate < 100) {
    cat("\n--- FAILURE ANALYSIS ---\n")
    failures <- map_test[match_found == FALSE]
    
    cat("Top 10 Failed Mappings (Could not find generated ID in Sites file):\n")
    print(head(failures[, .(permit_number, target_site_id)], 10))
  }
  
  # F. GEOGRAPHY YIELD
  if (success_count > 0) {
    cat("\n--- GEOGRAPHY YIELD ---\n")
    valid_links <- map_test[match_found == TRUE]
    merged <- merge(valid_links, data_list$Sites, by.x = "target_site_id", by.y = "number", all.x = TRUE)
    
    # Check coords (non-NA, non-empty, non-zero)
    has_lat <- !is.na(merged$latitude) & merged$latitude != "" & merged$latitude != "0"
    lat_yield <- round(sum(has_lat) / nrow(merged) * 100, 1)
    
    cat(paste0("Of the matched facilities, ", lat_yield, "% have valid Latitude data.\n"))
    cat("Sample Coordinates:\n")
    print(head(merged[has_lat, .(permit_number, target_site_id, latitude, longitude)], 5))
  }
}
tank_file <- file.path(al_path, "UST_UTanks (2).xlsx")
outfile   <- file.path(al_path, "AL_Full_Codes.txt")

if (!file.exists(tank_file)) stop("Tank file not found!")

# 2. Load Data -----------------------------------------------------------------
message("Loading Tank File...")
# Read as text to ensure we catch every variation
dt <- read_excel(tank_file, col_types = "text") %>% 
  clean_names() %>% 
  as.data.table()

# 3. Extract Tables ------------------------------------------------------------
message("Extracting Frequency Tables...")

# Construction Codes (Full List)
const_table <- dt[, .N, by = .(tank_construction_material, tank_construction_material_select)]
setorder(const_table, -N)

# Substance Codes (Full List)
subst_table <- dt[, .N, by = .(petroleum_product, petroleum_product_select, substance_stored)]
setorder(subst_table, -N)

# 4. Write to Text File --------------------------------------------------------
message(paste0("Writing to ", outfile, "..."))

sink(outfile)

cat("================================================================================\n")
cat("ALABAMA FULL CODE DICTIONARY\n")
cat("Generated: ", as.character(Sys.time()), "\n")
cat("================================================================================\n\n")

cat("--------------------------------------------------------------------------------\n")
cat("TABLE 1: TANK CONSTRUCTION MATERIALS (Full List)\n")
cat("--------------------------------------------------------------------------------\n")
print(kable(const_table, format = "pipe", align = "l"))
cat("\n\n")

cat("--------------------------------------------------------------------------------\n")
cat("TABLE 2: SUBSTANCE / CONTENTS (Full List)\n")
cat("--------------------------------------------------------------------------------\n")
print(kable(subst_table, format = "pipe", align = "l"))
cat("\n\n")

sink()

message("Done. File saved.")