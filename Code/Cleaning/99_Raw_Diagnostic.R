# 01_state_head_probe.R
# Load safe/clean heads & glimpse for each file in Data/Raw/state_databases/*
# Output: Data/Processed/State_Heads_<timestamp>.txt
# AUDIT UPDATE:
#   - FIXED: Syntax error in print helper.
#   - AR: Iterates ALL tables in .mdb files.
#   - OK: Uses specific OCC_PST CSVs.
#   - Exclusions: Includes all custom states EXCEPT CA.

library(data.table)
library(tidyverse)
library(readxl)
library(DBI)
library(odbc)
library(janitor)
library(here)
library(glue)
library(tools)

options(width = 300)
out_dir <- here("Data", "Processed")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
out_file <- file.path(out_dir, paste0("State_Heads_", timestamp, ".txt"))

# ===============================================================================
# CONFIG: INJECTED FILE MAPPING
# ===============================================================================

state_file_paths_literal <- list(
  Arkansas = c(
    # Access DB - Will now print ALL tables
    here("Data", "Raw", "state_databases", "Arkansas", "TankStats_web.mdb"),
    here("Data", "Processed", "county_fips_codes.csv")
  ),
  
  Louisiana = c(
    here("Data", "Raw", "state_databases", "Louisiana", "LA_record_request.xlsx"),
    here("Data", "Processed", "county_fips_codes.csv")
  ),
  
  Maine = c(
    # Local
    here("Data", "Processed", "Census_Geography", "Maine_census_geography.csv"),
    # URLs found in cleaning script
    "https://www.maine.gov/dep/ftp/tanks/tanks_owner_all_registered_tanks.txt",
    "https://www.maine.gov/dep/ftp/hoss/report_tank_involved.txt",
    "https://www.maine.gov/dep/ftp/hoss/spill_report.txt",
    "https://www.maine.gov/dep/ftp/hoss/spill_log.txt"
  ),
  
  Michigan = c(
    here("Data", "Raw", "state_databases", "Michigan", "UTK Master List 3-3-25.xlsx"),
    here("Data", "Raw", "Releases.csv")
  ),
  
  `New Jersey` = c(
    here("Data", "Raw", "state_databases", "New Jersey", "New Jersey Tanks Data.csv"),
    here("Data", "Raw", "state_databases", "New Jersey", "New Jersey Facility Data.csv"),
    here("Data", "Raw", "Releases.csv")
  ),
  
  `New Mexico` = c(
    here("Data", "Raw", "USTs.csv"),
    here("Data", "Raw", "Facilities.csv"),
    # Directory scan backup
    here("Data", "Raw", "state_databases", "New Mexico")
  ),
  
  Oklahoma = c(
    # Specific files requested
    here("Data", "Raw", "state_databases", "Oklahoma", "OCC_PST_AllCases.csv"),
    here("Data", "Raw", "state_databases", "Oklahoma", "OCC_PST_AllTanks.csv")
  ),
  
  Texas = c(
    # Local
    here("Data", "Raw", "state_databases", "Texas", "TX_LUST.csv"),
    # URLs found in cleaning script
    "https://www.tceq.texas.gov/assets/public/admin/data/docs/pst_fac.txt",
    "https://www.tceq.texas.gov/assets/public/admin/data/docs/pst_fin_assur.txt",
    "https://www.tceq.texas.gov/assets/public/admin/data/docs/pst_self_cert.txt",
    "https://www.tceq.texas.gov/assets/public/admin/data/docs/pst_ust.txt",
    "https://www.tceq.texas.gov/assets/public/admin/data/docs/pst_ust_comprt.txt"
  ),
  
  Tennessee = c(
    here("Data", "Raw", "state_databases", "Tennessee", "ust_all-tn-compartments.xlsx"),
    here("Data", "Raw", "state_databases", "Tennessee", "ust_all-tn-environmental-sites.xlsx"),
    here("Data", "Raw", "Facilities.csv"),
    here("Data", "Raw", "uszips.csv")
  ),
  
  Alabama = c(
    here("Data", "Raw", "state_databases", "Alabama", "UST_UTanks (2).xlsx"),
    here("Data", "Raw", "state_databases", "Alabama", "UST_Sites (2).xlsx")
  ),
  
  Colorado = c(
    here("Data", "Raw", "state_databases", "Colorado", "Regulated_Storage_Tanks_in_Colorado__Oil___Public_Safety_20250728.csv"),
    here("Data", "Raw", "state_databases", "Colorado", "Petroleum_Releases__Colorado_Oil___Public_Safety_20250728.csv")
  )
)

epa_file_templates <- list(
  national = c(
    here("Data", "Raw", "USTs.csv"),
    here("Data", "Raw", "Facilities.csv"),
    here("Data", "Raw", "Releases.csv")
  ),
  
  # Exclude all states with custom scripts EXCEPT California
  excluded_states = c(
    "Arkansas", 
    "Louisiana", 
    "Maine", 
    "Michigan", 
    "New Jersey", 
    "New Mexico", 
    "Oklahoma", 
    "Texas",
    "Alabama",
    "Tennessee",
    "Colorado"
  )
)

# ===============================================================================
# LOADER HELPER FUNCTION
# ===============================================================================

load_file <- function(file_spec, state = NULL) {
  # Resolve templates
  path_resolved <- file_spec
  if (!is.null(state) && grepl("\\{state\\}", file_spec)) {
    path_resolved <- glue(file_spec)
  }
  
  # Check if URL
  is_url <- grepl("^https?://", path_resolved)
  
  # Download URLs to temp
  if (is_url) {
    temp_file <- tempfile(fileext = paste0(".", tools::file_ext(path_resolved)))
    if(tools::file_ext(path_resolved) == "") temp_file <- paste0(temp_file, ".txt")
    
    tryCatch({
      download.file(path_resolved, temp_file, mode = "wb", quiet = TRUE)
      path_resolved <- temp_file
    }, error = function(e) {
      return(list(dt = NULL, meta = list(path_resolved = path_resolved, error = paste("Download failed:", e$message))))
    })
  }
  
  # Handle Directory
  if (dir.exists(path_resolved) && !is_url) {
    files_in_dir <- list.files(path_resolved, full.names = TRUE, pattern = "\\.(csv|xlsx|xls|txt)$", ignore.case = TRUE)
    if (length(files_in_dir) > 0) {
      path_resolved <- files_in_dir[1] 
    } else {
      return(list(dt = NULL, meta = list(path_resolved = path_resolved, error = "Directory found but no data files (csv/xlsx) inside.")))
    }
  }
  
  ext <- tolower(tools::file_ext(path_resolved))
  meta <- list(
    path_resolved = path_resolved,
    file = basename(path_resolved),
    ext = ext,
    read_fn = NULL,
    access_tables = NULL,
    error = NULL,
    is_list = FALSE
  )
  
  dt <- NULL
  
  tryCatch({
    if (ext %in% c("csv", "txt")) {
      dt <- fread(path_resolved, colClasses = "character", na.strings = c("", "NA", "NULL"), nrows = 1000)
      meta$read_fn <- "fread"
      
    } else if (ext %in% c("xls", "xlsx")) {
      df <- read_excel(path_resolved, col_types = "text", n_max = 1000)
      dt <- as.data.table(df)
      meta$read_fn <- "read_excel"
      
    } else if (ext == "rds") {
      dt <- readRDS(path_resolved)
      if (!is.data.table(dt) && is.data.frame(dt)) dt <- as.data.table(dt)
      if (nrow(dt) > 1000) dt <- head(dt, 1000)
      meta$read_fn <- "readRDS"
      
    } else if (ext %in% c("mdb", "accdb")) {
      # UPDATED MDB LOGIC: LOAD ALL TABLES
      con_str <- sprintf("Driver={Microsoft Access Driver (*.mdb, *.accdb)};Dbq=%s", normalizePath(path_resolved))
      con <- tryCatch(dbConnect(odbc::odbc(), .connection_string = con_str), error = function(e) NULL)
      
      if (!is.null(con)) {
        tbls <- dbListTables(con)
        # Filter out system tables
        tbls <- grep("^MSys", tbls, invert = TRUE, value = TRUE)
        meta$access_tables <- tbls
        meta$read_fn <- "dbConnect/dbReadTable (All Tables)"
        
        if (length(tbls) > 0) {
          dt_list <- list()
          for(tbl in tbls) {
            # Read head of each table
            tryCatch({
              # Read query with limit to avoid loading huge tables
              q <- sprintf("SELECT TOP 100 * FROM [%s]", tbl)
              res <- dbGetQuery(con, q)
              dt_list[[tbl]] <- as.data.table(res)
            }, error = function(e) {
              dt_list[[tbl]] <- data.table(Error = paste("Failed to read:", e$message))
            })
          }
          dt <- dt_list
          meta$is_list <- TRUE
        }
        dbDisconnect(con)
      } else {
        meta$error <- "ODBC connect failed"
      }
      
    } else {
      meta$error <- paste("Unsupported extension:", ext)
    }
    
  }, error = function(e) {
    meta$error <- e$message
  })
  
  if (is_url && file.exists(path_resolved)) unlink(path_resolved)
  
  list(dt = dt, meta = meta)
}

# ===============================================================================
# PRINT HELPER
# ===============================================================================

print_table_details <- function(dt, name_suffix = "") {
  if (is.null(dt)) {
    cat(sprintf("  [NO DATA LOADED %s]\n", name_suffix))
    return()
  }
  
  # Handle error table from MDB load
  if ("Error" %in% names(dt) && ncol(dt) == 1) {
    cat(sprintf("  [ERROR %s]: %s\n", name_suffix, dt$Error[1]))
    return()
  }
  
  cat(sprintf("  Table: %s\n", name_suffix))
  cat(sprintf("  nrows: %s | ncols: %s\n", format(nrow(dt), big.mark=","), ncol(dt)))
  
  cat("  head:\n")
  print(head(dt, 6))
  cat("\n")
  
  cat("  column names (raw):\n")
  print(names(dt))
  cat("\n  column names (cleaned):\n")
  print(make_clean_names(names(dt)))
  cat("\n")
  
  cat("  glimpse:\n")
  tryCatch({
    glimpse(dt)
  }, error = function(e) {
    cat("   [glimpse failed]: ", e$message, "\n")
  })
  # FIXED SYNTAX ERROR HERE (removed dot concatenation)
  cat("\n", strrep("-", 40), "\n")
}

# ===============================================================================
# MAIN LOOP: STATE FILES
# ===============================================================================

sink(out_file)
cat(strrep("=", 120), "\n")
cat("STATE FILES: HEADS & GLIMPSE REPORT (Updated)\n")
cat("Generated: ", Sys.time(), "\n")
cat(strrep("=", 120), "\n\n")

for (state_name in names(state_file_paths_literal)) {
  file_specs <- state_file_paths_literal[[state_name]]
  
  cat("#############################\n")
  cat("State: ", state_name, "\n", sep = "")
  cat("Number of Inputs: ", length(file_specs), "\n")
  cat("#############################\n\n")
  
  idx <- 1
  for (file_spec in file_specs) {
    res <- load_file(file_spec, state = state_name)
    meta <- res$meta
    dt <- res$dt
    
    cat(sprintf("%d. %s\n", idx, meta$file))
    cat(sprintf("%d.1 %s\n", idx, toupper(meta$ext)))
    cat(sprintf("  Path: %s\n", meta$path_resolved))
    if (!is.null(meta$read_fn)) cat(sprintf("  Read method: %s\n", meta$read_fn))
    if (!is.null(meta$error)) cat(sprintf("  [ERROR]: %s\n", meta$error))
    cat("\n")
    
    if (meta$is_list) {
      # Iterate through all loaded tables (Access DB)
      table_names <- names(dt)
      for (tbl in table_names) {
        print_table_details(dt[[tbl]], name_suffix = tbl)
      }
    } else {
      # Single file
      print_table_details(dt)
    }
    
    cat("\n")
    idx <- idx + 1
  }
  cat("#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&#\n\n")
}

# ===============================================================================
# MAIN LOOP: EPA FILES
# ===============================================================================

cat("#############################\n")
cat("EPA National Files\n")
cat("Number of Files: ", length(epa_file_templates$national), "\n")
cat("Excluded States: ", paste(epa_file_templates$excluded_states, collapse=", "), "\n")
cat("#############################\n\n")

idx <- 1
for (file_spec in epa_file_templates$national) {
  res <- load_file(file_spec)
  meta <- res$meta
  dt <- res$dt
  
  cat(sprintf("%d. %s\n", idx, meta$file))
  cat(sprintf("%d.1 %s\n", idx, toupper(meta$ext)))
  print_table_details(dt)
  cat("\n\n")
  idx <- idx + 1
}

cat("#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&#\n\n")
cat("END OF REPORT\n")
sink()
message("Wrote head/glimpse report: ", out_file)