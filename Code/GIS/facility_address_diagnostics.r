##==============================================================================
## FACILITY NAME & ADDRESS VARIABLE AUDIT
## Purpose: Extract raw dataset structure (col names, head, glimpse) from all
##          state cleaning scripts to identify facility_name and address vars
## Output: Single consolidated log file with all raw data signatures
##==============================================================================

library(data.table)
library(tidyverse)
library(readxl)
library(janitor)
library(here)
library(DBI)
library(odbc)

# ──────────────────────────────────────────────────────────────────────────────
# SETUP
# ──────────────────────────────────────────────────────────────────────────────

out_dir <- here("Diagnostics")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

log_file <- file.path(out_dir, "Facility_Name_Address_Audit.txt")
con_log <- file(log_file, open = "wt")

# Helper: write to console AND file simultaneously
log_both <- function(...) {
  msg <- paste0(...)
  cat(msg, "\n")
  cat(msg, "\n", file = con_log)
}

# Helper: print data structure
print_raw_structure <- function(dt, label) {
  log_both("\n", strrep("─", 80))
  log_both("DATASET: ", label)
  log_both(strrep("─", 80))
  
  log_both("\n[DIMENSIONS]")
  log_both(sprintf("  Rows: %s | Columns: %s", format(nrow(dt), big.mark=","), ncol(dt)))
  
  log_both("\n[COLUMN NAMES]")
  log_both(paste(names(dt), collapse = ", "))
  
  log_both("\n[HEAD (first 6 rows)]")
  log_both(capture.output(print(head(dt, 6))))
  
  log_both("\n[GLIMPSE]")
  log_both(capture.output(dplyr::glimpse(as.data.frame(dt))))
}

# ──────────────────────────────────────────────────────────────────────────────
# ARKANSAS (01_Clean_AR.R)
# ──────────────────────────────────────────────────────────────────────────────

tryCatch({
  log_both("\n\n", strrep("═", 80))
  log_both("STATE: ARKANSAS")
  log_both(strrep("═", 80))
  
  db_path <- here("Data", "Raw", "state_databases", "Arkansas", "TankStats_web.mdb")
  if (file.exists(db_path)) {
    con <- dbConnect(odbc(),
                     .connection_string = paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};Dbq=", db_path, ";"))
    
    # Facilities table
    rfacs_data <- as.data.table(dbReadTable(con, "TempRFACS_CountryNO"))
    print_raw_structure(rfacs_data, "AR Facilities (TempRFACS_CountryNO)")
    
    # Tanks table
    tankstats_ug <- as.data.table(dbReadTable(con, "TempTankStats_UG"))
    print_raw_structure(tankstats_ug, "AR Tanks (TempTankStats_UG)")
    
    # LUST table
    lust_data <- as.data.table(dbReadTable(con, "TempRLUSTLOG1"))
    print_raw_structure(lust_data, "AR LUST (TempRLUSTLOG1)")
    
    dbDisconnect(con)
  } else {
    log_both("ERROR: Arkansas database not found at ", db_path)
  }
}, error = function(e) {
  log_both("ERROR (AR): ", e$message)
})

# ──────────────────────────────────────────────────────────────────────────────
# LOUISIANA (02_Clean_LA.R)
# ──────────────────────────────────────────────────────────────────────────────

tryCatch({
  log_both("\n\n", strrep("═", 80))
  log_both("STATE: LOUISIANA")
  log_both(strrep("═", 80))
  
  la_file_path <- here("Data", "Raw", "state_databases", "Louisiana", "LA_record_request.xlsx")
  if (file.exists(la_file_path)) {
    LA_tanks_raw <- read_excel(la_file_path, sheet = "Tank Info by compartment", col_types = "text") %>%
      janitor::clean_names() %>%
      as.data.table()
    print_raw_structure(LA_tanks_raw, "LA Tanks (Tank Info by compartment)")
    
    LA_lust_raw <- read_excel(la_file_path, sheet = "LUST Sites by incident ID", col_types = "text") %>%
      janitor::clean_names() %>%
      as.data.table()
    print_raw_structure(LA_lust_raw, "LA LUST (LUST Sites by incident ID)")
  } else {
    log_both("ERROR: Louisiana file not found at ", la_file_path)
  }
}, error = function(e) {
  log_both("ERROR (LA): ", e$message)
})

# ──────────────────────────────────────────────────────────────────────────────
# MAINE (03_Clean_ME.R)
# ──────────────────────────────────────────────────────────────────────────────

tryCatch({
  log_both("\n\n", strrep("═", 80))
  log_both("STATE: MAINE")
  log_both(strrep("═", 80))
  
  # Maine loads from URLs, attempt local cache if available
  me_path <- here("Data", "Raw", "state_databases", "Maine")
  if (dir.exists(me_path)) {
    me_files <- list.files(me_path, full.names = TRUE)
    log_both("Maine local files found: ", paste(basename(me_files), collapse = ", "))
    
    for (f in me_files[1:min(3, length(me_files))]) {
      tryCatch({
        dt <- fread(f, nrows = 100, colClasses = "character")
        print_raw_structure(dt, paste("ME -", basename(f)))
      }, error = function(e) {
        log_both("  Could not read ", basename(f), ": ", e$message)
      })
    }
  } else {
    log_both("Note: Maine data loads from URLs. Check 03_Clean_ME.R for URLs:")
    log_both("  - https://www.maine.gov/dep/ftp/hoss/...")
  }
}, error = function(e) {
  log_both("ERROR (ME): ", e$message)
})

# ──────────────────────────────────────────────────────────────────────────────
# MICHIGAN (04_Clean_MI.R)
# ──────────────────────────────────────────────────────────────────────────────

tryCatch({
  log_both("\n\n", strrep("═", 80))
  log_both("STATE: MICHIGAN")
  log_both(strrep("═", 80))
  
  michigan_file_path <- here("Data", "Raw", "state_databases", "Michigan", "UTK Master List 3-3-25.xlsx")
  if (file.exists(michigan_file_path)) {
    # Read all sheets
    sheet_names <- excel_sheets(michigan_file_path)
    log_both("Available sheets: ", paste(sheet_names, collapse = ", "))
    
    for (sheet in sheet_names[1:min(3, length(sheet_names))]) {
      tryCatch({
        dt <- read_excel(michigan_file_path, sheet = sheet, col_types = "text") %>%
          janitor::clean_names() %>%
          as.data.table()
        print_raw_structure(dt, paste("MI Sheet:", sheet))
      }, error = function(e) {
        log_both("  Could not read sheet ", sheet, ": ", e$message)
      })
    }
  } else {
    log_both("ERROR: Michigan file not found at ", michigan_file_path)
  }
}, error = function(e) {
  log_both("ERROR (MI): ", e$message)
})

# ──────────────────────────────────────────────────────────────────────────────
# NEW JERSEY (05_Clean_NJ.R)
# ──────────────────────────────────────────────────────────────────────────────

tryCatch({
  log_both("\n\n", strrep("═", 80))
  log_both("STATE: NEW JERSEY")
  log_both(strrep("═", 80))
  
  nj_path <- here("Data", "Raw", "state_databases", "New Jersey")
  
  # Tanks
  nj_tanks_file <- file.path(nj_path, "New Jersey Tanks Data.csv")
  if (file.exists(nj_tanks_file)) {
    NJ_tanks_raw <- fread(nj_tanks_file, colClasses = "character", nrows = 1000) %>%
      janitor::clean_names() %>%
      as.data.table()
    print_raw_structure(NJ_tanks_raw, "NJ Tanks Data")
  }
  
  # Facilities
  nj_fac_file <- file.path(nj_path, "New Jersey Facility Data.csv")
  if (file.exists(nj_fac_file)) {
    NJ_facility_raw <- fread(nj_fac_file, colClasses = "character", nrows = 1000) %>%
      janitor::clean_names() %>%
      as.data.table()
    print_raw_structure(NJ_facility_raw, "NJ Facility Data")
  }
}, error = function(e) {
  log_both("ERROR (NJ): ", e$message)
})

# ──────────────────────────────────────────────────────────────────────────────
# NEW MEXICO (06_Clean_NM.R)
# ──────────────────────────────────────────────────────────────────────────────

tryCatch({
  log_both("\n\n", strrep("═", 80))
  log_both("STATE: NEW MEXICO")
  log_both(strrep("═", 80))
  
  # Uses EPA national data
  ust_path <- here("Data", "Raw", "USTs.csv")
  facilities_path <- here("Data", "Raw", "Facilities.csv")
  
  if (file.exists(ust_path)) {
    epa_usts <- fread(ust_path, colClasses = "character", nrows = 1000) %>%
      janitor::clean_names() %>%
      as.data.table()
    print_raw_structure(epa_usts, "EPA USTs.csv (for NM)")
  }
  
  if (file.exists(facilities_path)) {
    epa_fac <- fread(facilities_path, colClasses = "character", nrows = 1000) %>%
      janitor::clean_names() %>%
      as.data.table()
    print_raw_structure(epa_fac, "EPA Facilities.csv (for NM)")
  }
  
  # Check for state-specific NM files
  nm_path <- here("Data", "Raw", "state_databases", "New Mexico")
  if (dir.exists(nm_path)) {
    nm_files <- list.files(nm_path, pattern = "\\.xlsx$|\\.csv$", full.names = TRUE)
    for (f in nm_files[1:min(3, length(nm_files))]) {
      tryCatch({
        if (grepl("\\.xlsx$", f)) {
          dt <- read_excel(f, col_types = "text") %>%
            janitor::clean_names() %>%
            as.data.table()
        } else {
          dt <- fread(f, colClasses = "character", nrows = 1000) %>%
            janitor::clean_names() %>%
            as.data.table()
        }
        print_raw_structure(dt, paste("NM -", basename(f)))
      }, error = function(e) {
        log_both("  Could not read ", basename(f), ": ", e$message)
      })
    }
  }
}, error = function(e) {
  log_both("ERROR (NM): ", e$message)
})

# ──────────────────────────────────────────────────────────────────────────────
# OKLAHOMA (07_Clean_OK.R)
# ──────────────────────────────────────────────────────────────────────────────

tryCatch({
  log_both("\n\n", strrep("═", 80))
  log_both("STATE: OKLAHOMA")
  log_both(strrep("═", 80))
  
  ok_path <- here("Data", "Raw", "state_databases", "Oklahoma")
  
  # Find tank and LUST files
  tank_files <- list.files(ok_path, pattern = "Tank|PST", full.names = TRUE, ignore.case = TRUE)
  lust_files <- list.files(ok_path, pattern = "Case|LUST|Release", full.names = TRUE, ignore.case = TRUE)
  
  for (f in tank_files[1:min(2, length(tank_files))]) {
    tryCatch({
      dt <- fread(f, colClasses = "character", nrows = 1000) %>%
        janitor::clean_names() %>%
        as.data.table()
      print_raw_structure(dt, paste("OK Tank -", basename(f)))
    }, error = function(e) {
      log_both("  Could not read ", basename(f), ": ", e$message)
    })
  }
  
  for (f in lust_files[1:min(2, length(lust_files))]) {
    tryCatch({
      dt <- fread(f, colClasses = "character", nrows = 1000) %>%
        janitor::clean_names() %>%
        as.data.table()
      print_raw_structure(dt, paste("OK LUST -", basename(f)))
    }, error = function(e) {
      log_both("  Could not read ", basename(f), ": ", e$message)
    })
  }
}, error = function(e) {
  log_both("ERROR (OK): ", e$message)
})

# ──────────────────────────────────────────────────────────────────────────────
# TEXAS (08_Clean_TX.R)
# ──────────────────────────────────────────────────────────────────────────────

tryCatch({
  log_both("\n\n", strrep("═", 80))
  log_both("STATE: TEXAS")
  log_both(strrep("═", 80))
  
  tx_path <- here("Data", "Raw", "state_databases", "Texas")
  
  # Check for raw CSV files
  tx_files <- list.files(tx_path, pattern = "\\.csv$|\\.txt$", full.names = TRUE)
  
  for (f in tx_files[1:min(4, length(tx_files))]) {
    tryCatch({
      dt <- fread(f, colClasses = "character", nrows = 1000) %>%
        janitor::clean_names() %>%
        as.data.table()
      print_raw_structure(dt, paste("TX -", basename(f)))
    }, error = function(e) {
      log_both("  Could not read ", basename(f), ": ", e$message)
    })
  }
  
  # Check for RDS files
  rds_files <- list.files(tx_path, pattern = "\\.rds$", full.names = TRUE)
  for (f in rds_files[1:min(2, length(rds_files))]) {
    tryCatch({
      dt <- readRDS(f) %>% as.data.table()
      print_raw_structure(dt, paste("TX -", basename(f)))
    }, error = function(e) {
      log_both("  Could not read ", basename(f), ": ", e$message)
    })
  }
}, error = function(e) {
  log_both("ERROR (TX): ", e$message)
})

# ──────────────────────────────────────────────────────────────────────────────
# ALABAMA (10_Clean_AL.R)
# ──────────────────────────────────────────────────────────────────────────────

tryCatch({
  log_both("\n\n", strrep("═", 80))
  log_both("STATE: ALABAMA")
  log_both(strrep("═", 80))
  
  al_path <- here("Data", "Raw", "state_databases", "Alabama")
  
  al_files <- list.files(al_path, pattern = "\\.xlsx$", full.names = TRUE)
  
  for (f in al_files[1:min(3, length(al_files))]) {
    tryCatch({
      sheet_names <- excel_sheets(f)
      for (sheet in sheet_names[1:min(2, length(sheet_names))]) {
        dt <- read_excel(f, sheet = sheet, col_types = "text") %>%
          janitor::clean_names() %>%
          as.data.table()
        print_raw_structure(dt, paste("AL -", basename(f), "- Sheet:", sheet))
      }
    }, error = function(e) {
      log_both("  Could not read ", basename(f), ": ", e$message)
    })
  }
}, error = function(e) {
  log_both("ERROR (AL): ", e$message)
})

# ──────────────────────────────────────────────────────────────────────────────
# TENNESSEE (11_Clean_TN.R)
# ──────────────────────────────────────────────────────────────────────────────

tryCatch({
  log_both("\n\n", strrep("═", 80))
  log_both("STATE: TENNESSEE")
  log_both(strrep("═", 80))
  
  tn_path <- here("Data", "Raw", "state_databases", "Tennessee")
  
  tn_files <- list.files(tn_path, pattern = "\\.xlsx$", full.names = TRUE)
  
  for (f in tn_files[1:min(4, length(tn_files))]) {
    tryCatch({
      sheet_names <- excel_sheets(f)
      for (sheet in sheet_names[1:min(1, length(sheet_names))]) {
        dt <- read_excel(f, sheet = sheet, col_types = "text") %>%
          janitor::clean_names() %>%
          as.data.table()
        print_raw_structure(dt, paste("TN -", basename(f), "- Sheet:", sheet))
      }
    }, error = function(e) {
      log_both("  Could not read ", basename(f), ": ", e$message)
    })
  }
}, error = function(e) {
  log_both("ERROR (TN): ", e$message)
})

# ──────────────────────────────────────────────────────────────────────────────
# COLORADO (13_Clean_CO.R)
# ──────────────────────────────────────────────────────────────────────────────

tryCatch({
  log_both("\n\n", strrep("═", 80))
  log_both("STATE: COLORADO")
  log_both(strrep("═", 80))
  
  co_path <- here("Data", "Raw", "state_databases", "Colorado")
  
  co_files <- list.files(co_path, pattern = "\\.csv$", full.names = TRUE)
  
  for (f in co_files[1:min(3, length(co_files))]) {
    tryCatch({
      dt <- fread(f, colClasses = "character", nrows = 1000) %>%
        janitor::clean_names() %>%
        as.data.table()
      print_raw_structure(dt, paste("CO -", basename(f)))
    }, error = function(e) {
      log_both("  Could not read ", basename(f), ": ", e$message)
    })
  }
}, error = function(e) {
  log_both("ERROR (CO): ", e$message)
})

# ──────────────────────────────────────────────────────────────────────────────
# PENNSYLVANIA (14_Clean_PA.r)
# ──────────────────────────────────────────────────────────────────────────────

tryCatch({
  log_both("\n\n", strrep("═", 80))
  log_both("STATE: PENNSYLVANIA")
  log_both(strrep("═", 80))
  
  pa_path <- here("Data", "Raw", "state_databases", "Pennsylvania")
  
  pa_files <- list.files(pa_path, pattern = "\\.xlsx$|\\.csv$", full.names = TRUE)
  
  for (f in pa_files[1:min(4, length(pa_files))]) {
    tryCatch({
      if (grepl("\\.xlsx$", f)) {
        sheet_names <- excel_sheets(f)
        for (sheet in sheet_names[1:min(1, length(sheet_names))]) {
          dt <- read_excel(f, sheet = sheet, col_types = "text") %>%
            janitor::clean_names() %>%
            as.data.table()
          print_raw_structure(dt, paste("PA -", basename(f), "- Sheet:", sheet))
        }
      } else {
        dt <- fread(f, colClasses = "character", nrows = 1000) %>%
          janitor::clean_names() %>%
          as.data.table()
        print_raw_structure(dt, paste("PA -", basename(f)))
      }
    }, error = function(e) {
      log_both("  Could not read ", basename(f), ": ", e$message)
    })
  }
}, error = function(e) {
  log_both("ERROR (PA): ", e$message)
})

# ──────────────────────────────────────────────────────────────────────────────
# EPA NATIONAL (09_Clean_EPA_Region6.R)
# ──────────────────────────────────────────────────────────────────────────────

tryCatch({
  log_both("\n\n", strrep("═", 80))
  log_both("EPA NATIONAL DATA")
  log_both(strrep("═", 80))
  
  epa_ust_path <- here("Data", "Raw", "USTs.csv")
  epa_fac_path <- here("Data", "Raw", "Facilities.csv")
  epa_rel_path <- here("Data", "Raw", "Releases.csv")
  
  if (file.exists(epa_ust_path)) {
    epa_usts <- fread(epa_ust_path, colClasses = "character", nrows = 500)
    print_raw_structure(epa_usts, "EPA USTs.csv (National)")
  }
  
  if (file.exists(epa_fac_path)) {
    epa_fac <- fread(epa_fac_path, colClasses = "character", nrows = 500)
    print_raw_structure(epa_fac, "EPA Facilities.csv (National)")
  }
  
  if (file.exists(epa_rel_path)) {
    epa_rel <- fread(epa_rel_path, colClasses = "character", nrows = 500)
    print_raw_structure(epa_rel, "EPA Releases.csv (National)")
  }
}, error = function(e) {
  log_both("ERROR (EPA): ", e$message)
})

# ──────────────────────────────────────────────────────────────────────────────
# SUMMARY
# ──────────────────────────────────────────────────────────────────────────────

log_both("\n\n", strrep("═", 80))
log_both("AUDIT COMPLETE")
log_both(strrep("═", 80))
log_both("Log file saved to: ", log_file)
log_both("Use this file to identify facility_name and address column names across states")

close(con_log)

message("✓ Audit complete. Log saved to: ", log_file)
