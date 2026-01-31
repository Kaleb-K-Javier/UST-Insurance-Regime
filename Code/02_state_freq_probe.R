# =============================================================================
# 02_state_freq_probe.R
# Purpose: Generate frequency tables, missingness reports, and type conversion
#          diagnostics for all state UST databases using comprehensive variable lists.
#
# Refactored to use state_variable_lists.R for exhaustive coverage.
# =============================================================================

library(data.table)
library(tidyverse)
library(readxl)
library(lubridate)
library(janitor)
library(DBI)
library(odbc)
library(here)
library(glue)
library(tools)

options(width = 300, scipen = 999)

# =============================================================================
# 0. SOURCE VARIABLE REGISTRY
# =============================================================================
source(here("Code", "state_variable_lists.R"))  
# Provides: STATE_VAR_REGISTRY, get_date_vars(), get_categorical_vars(), etc.

# =============================================================================
# 1. CONFIGURATION
# =============================================================================
out_dir <- here("Data", "Processed")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
out_file <- file.path(out_dir, paste0("State_Freqs_", timestamp, ".txt"))

# State abbreviation <-> Full name mapping
STATE_ABBREV <- c(
  AL = "Alabama", AK = "Alaska", AR = "Arkansas", CO = "Colorado",
  LA = "Louisiana", ME = "Maine", MI = "Michigan", NJ = "New Jersey",
  NM = "New Mexico", OK = "Oklahoma", TN = "Tennessee", TX = "Texas"
)

# File paths by state (using abbreviations to match STATE_VAR_REGISTRY)
STATE_FILE_PATHS <- list(
  AL = list(
    tanks = here("Data", "Raw", "state_databases", "Alabama", "UST_UTanks (2).xlsx"),
    sites = here("Data", "Raw", "state_databases", "Alabama", "UST_Sites (2).xlsx")
  ),
  AR = list(
    mdb = here("Data", "Raw", "state_databases", "Arkansas", "TankStats_web.mdb")
  ),
  CO = list(
    tanks = here("Data", "Raw", "state_databases", "Colorado", 
                 "Regulated_Storage_Tanks_in_Colorado__Oil___Public_Safety_20250728.csv"),
    releases = here("Data", "Raw", "state_databases", "Colorado",
                    "Petroleum_Releases__Colorado_Oil___Public_Safety_20250728.csv"),
    epa_usts = here("Data", "Raw", "USTs.csv"),
    epa_facilities = here("Data", "Raw", "Facilities.csv"),
    epa_releases = here("Data", "Raw", "Releases.csv")
  ),
  LA = list(
    lust = here("Data", "Raw", "state_databases", "Louisiana", "LA_record_request.xlsx")
  ),
  MI = list(
    tanks = here("Data", "Raw", "state_databases", "Michigan", "UTK Master List 3-3-25.xlsx"),
    releases = here("Data", "Raw", "Releases.csv")
  ),
  NJ = list(
    tanks = here("Data", "Raw", "state_databases", "New Jersey", "New Jersey Tanks Data.csv"),
    facilities = here("Data", "Raw", "state_databases", "New Jersey", "New Jersey Facility Data.csv"),
    releases = here("Data", "Raw", "Releases.csv")
  ),
  NM = list(
    usts = here("Data", "Raw", "USTs.csv"),
    facilities = here("Data", "Raw", "Facilities.csv"),
    inspections = here("Data", "Raw", "state_databases", "New Mexico", 
                       "2025 01 14 UST Compliance inspections_history 1990 - 2024.xlsx")
  ),
  OK = list(
    tanks = here("Data", "Raw", "state_databases", "Oklahoma", "OCC_PST_AllTanks.csv"),
    cases = here("Data", "Raw", "state_databases", "Oklahoma", "OCC_PST_AllCases.csv")
  ),
  TN = list(
    compartments = here("Data", "Raw", "state_databases", "Tennessee", "ust_all-tn-compartments.xlsx"),
    environmental_sites = here("Data", "Raw", "state_databases", "Tennessee", 
                               "ust_all-tn-environmental-sites.xlsx"),
    facilities = here("Data", "Raw", "Facilities.csv"),
    zip_crosswalk = here("Data", "Raw", "uszips.csv")
  ),
  TX = list(
    lust = here("Data", "Raw", "state_databases", "Texas", "TX_LUST.csv")
    # Note: Texas PST files are fixed-width, handled separately
  ),
  AK = list(
    lust = here("Data", "Raw", "state_databases", "Alaska", "LUST.csv")
  )
)

# =============================================================================
# 2. FILE LOADER (Handles Access, Excel, CSV, Fixed-Width)
# =============================================================================

load_file <- function(path, sheet = NULL, skip = 0, col_types = "text") {
  if (!file.exists(path)) {
    return(list(dt = NULL, error = paste("File not found:", path)))
  }
  
  ext <- tolower(tools::file_ext(path))
  dt <- NULL
  error <- NULL
  
  tryCatch({
    if (ext %in% c("csv", "txt")) {
      dt <- fread(path, colClasses = "character", na.strings = c("", "NA", "NULL", "N/A"),
                  skip = skip, fill = TRUE)
      
    } else if (ext %in% c("xls", "xlsx")) {
      # Read all as text to preserve raw values for diagnostics
      df <- read_excel(path, sheet = sheet, col_types = col_types, skip = skip, 
                       .name_repair = "minimal")
      dt <- as.data.table(df)
      
    } else if (ext == "rds") {
      obj <- readRDS(path)
      dt <- if (is.data.table(obj)) obj else as.data.table(obj)
      
    } else if (ext %in% c("mdb", "accdb")) {
      # Access database - return list of tables
      con_str <- sprintf("Driver={Microsoft Access Driver (*.mdb, *.accdb)};Dbq=%s", 
                         normalizePath(path))
      con <- dbConnect(odbc::odbc(), .connection_string = con_str)
      
      tbls <- dbListTables(con)
      tbls <- grep("^MSys|^~", tbls, invert = TRUE, value = TRUE)  # Exclude system tables
      
      dt <- list()
      for (tbl in tbls) {
        tryCatch({
          res <- dbGetQuery(con, sprintf("SELECT * FROM [%s]", tbl))
          dt[[tbl]] <- as.data.table(res)
        }, error = function(e) {
          dt[[tbl]] <<- data.table(.error = e$message)
        })
      }
      dbDisconnect(con)
      attr(dt, "is_mdb") <- TRUE
      
    } else {
      error <- paste("Unsupported file type:", ext)
    }
    
    # Clean names if single data.table
    if (is.data.table(dt)) {
      dt <- clean_names(dt)
    } else if (is.list(dt) && !is.null(attr(dt, "is_mdb"))) {
      dt <- lapply(dt, function(x) if (is.data.table(x)) clean_names(x) else x)
      attr(dt, "is_mdb") <- TRUE
    }
    
  }, error = function(e) {
    error <<- e$message
  })
  
  list(dt = dt, error = error, path = path, ext = ext)
}

# =============================================================================
# 3. DIAGNOSTIC FUNCTIONS BY VARIABLE TYPE
# =============================================================================

#' Compute basic stats for any column
basic_stats <- function(vals, col_name) {
  n_total <- length(vals)
  n_miss <- sum(is.na(vals) | vals == "" | vals == "NA" | vals == "NULL" | vals == "N/A")
  n_valid <- n_total - n_miss
  n_unique <- uniqueN(vals[!is.na(vals) & vals != ""])
  
  list(
    column = col_name,
    n_total = n_total,
    n_missing = n_miss,
    pct_missing = round(n_miss / n_total * 100, 2),
    n_valid = n_valid,
    n_unique = n_unique
  )
}

#' Date column diagnostics
diagnose_date <- function(vals, col_name) {
  stats <- basic_stats(vals, col_name)
  
  # Get non-missing values
  non_miss <- vals[!is.na(vals) & vals != "" & vals != "NA"]
  if (length(non_miss) == 0) {
    stats$date_info <- "No valid values to parse"
    return(stats)
  }
  
  # Sample for parsing tests
  sample_vals <- head(non_miss, min(500, length(non_miss)))
  stats$sample_values <- paste(head(sample_vals, 5), collapse = " | ")
  
  # Detect format patterns
  patterns <- list(
    excel_numeric = sum(grepl("^[0-9]{4,5}$", sample_vals)),
    ymd = sum(grepl("^[0-9]{4}[-/][0-9]{1,2}[-/][0-9]{1,2}", sample_vals)),
    mdy = sum(grepl("^[0-9]{1,2}[-/][0-9]{1,2}[-/][0-9]{2,4}", sample_vals)),
    mdy_hms = sum(grepl("^[0-9]{1,2}/[0-9]{1,2}/[0-9]{4}\\s+[0-9]", sample_vals)),
    iso = sum(grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}T", sample_vals)),
    text_month = sum(grepl("^[A-Za-z]{3}", sample_vals))
  )
  stats$format_detection <- patterns
  
  # Attempt parsing with multiple formats
  parse_orders <- c("ymd", "mdy", "dmy", "Ymd", "mdY", "ymd HMS", "mdy HMS", "ymd HM")
  parsed <- suppressWarnings(
    parse_date_time(sample_vals, orders = parse_orders, quiet = TRUE)
  )
  
  n_parsed <- sum(!is.na(parsed))
  stats$parse_success_rate <- round(n_parsed / length(sample_vals) * 100, 1)
  
  if (n_parsed > 0) {
    valid_dates <- parsed[!is.na(parsed)]
    stats$date_range <- list(
      min = min(valid_dates),
      max = max(valid_dates)
    )
    # Flag suspicious dates
    stats$pre_1970 <- sum(valid_dates < as.POSIXct("1970-01-01"))
    stats$post_2030 <- sum(valid_dates > as.POSIXct("2030-01-01"))
  }
  
  # Check for Excel serial dates
  if (patterns$excel_numeric > length(sample_vals) * 0.5) {
    excel_parsed <- suppressWarnings(
      as.Date(as.numeric(sample_vals), origin = "1899-12-30")
    )
    n_excel <- sum(!is.na(excel_parsed))
    if (n_excel > n_parsed) {
      stats$likely_excel_serial <- TRUE
      stats$excel_range <- list(
        min = min(excel_parsed, na.rm = TRUE),
        max = max(excel_parsed, na.rm = TRUE)
      )
    }
  }
  
  stats
}

#' Categorical column diagnostics (frequency table)
diagnose_categorical <- function(vals, col_name, max_levels = 75) {
  stats <- basic_stats(vals, col_name)
  
  # Build frequency table
  freq_dt <- data.table(value = vals)[, .N, by = value][order(-N)]
  setnames(freq_dt, c("Value", "Count"))
  freq_dt[, Pct := round(Count / sum(Count) * 100, 2)]
  
  stats$freq_table <- head(freq_dt, max_levels)
  stats$n_truncated <- max(0, nrow(freq_dt) - max_levels)
  
  # Detect potential issues
  stats$has_whitespace_variants <- any(grepl("^\\s|\\s$", freq_dt$Value[!is.na(freq_dt$Value)]))
  stats$has_case_variants <- {
    vals_clean <- tolower(trimws(freq_dt$Value))
    length(unique(vals_clean)) < nrow(freq_dt)
  }
  
  stats
}

#' Numeric column diagnostics
diagnose_numeric <- function(vals, col_name) {
  stats <- basic_stats(vals, col_name)
  
  # Attempt numeric conversion
  vals_clean <- gsub("[,$]", "", vals)  # Remove commas and dollar signs
  vals_num <- suppressWarnings(as.numeric(vals_clean))
  
  n_numeric <- sum(!is.na(vals_num))
  stats$numeric_conversion_rate <- round(n_numeric / max(stats$n_valid, 1) * 100, 1)
  
  if (n_numeric > 0) {
    stats$numeric_stats <- list(
      min = min(vals_num, na.rm = TRUE),
      max = max(vals_num, na.rm = TRUE),
      median = median(vals_num, na.rm = TRUE),
      mean = round(mean(vals_num, na.rm = TRUE), 2),
      sd = round(sd(vals_num, na.rm = TRUE), 2)
    )
    
    # Distribution bins
    stats$quartiles <- quantile(vals_num, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
    
    # Flag outliers (IQR method)
    q1 <- stats$quartiles[1]
    q3 <- stats$quartiles[3]
    iqr <- q3 - q1
    stats$n_outliers_low <- sum(vals_num < (q1 - 1.5 * iqr), na.rm = TRUE)
    stats$n_outliers_high <- sum(vals_num > (q3 + 1.5 * iqr), na.rm = TRUE)
  }
  
  # Check for non-numeric residuals
  failed_convert <- vals_clean[is.na(vals_num) & !is.na(vals) & vals != ""]
  if (length(failed_convert) > 0) {
    stats$non_numeric_examples <- head(unique(failed_convert), 10)
  }
  
  stats
}

#' Flag/Boolean column diagnostics
diagnose_flag <- function(vals, col_name) {
  stats <- basic_stats(vals, col_name)
  
  # Frequency of all values
  freq_dt <- data.table(value = vals)[, .N, by = value][order(-N)]
  stats$freq_table <- freq_dt
  
  # Detect boolean patterns
  vals_upper <- toupper(trimws(vals))
  true_patterns <- c("TRUE", "T", "Y", "YES", "1", "X")
  false_patterns <- c("FALSE", "F", "N", "NO", "0", "")
  
  n_true <- sum(vals_upper %in% true_patterns, na.rm = TRUE)
  n_false <- sum(vals_upper %in% false_patterns | is.na(vals_upper), na.rm = TRUE)
  n_other <- stats$n_total - n_true - n_false
  
  stats$boolean_breakdown <- list(
    n_true = n_true,
    n_false = n_false,
    n_other = n_other,
    pct_true = round(n_true / stats$n_total * 100, 2)
  )
  
  if (n_other > 0) {
    other_vals <- vals[!vals_upper %in% c(true_patterns, false_patterns, NA)]
    stats$non_boolean_examples <- head(unique(other_vals), 10)
  }
  
  stats
}

#' ID column diagnostics
diagnose_id <- function(vals, col_name) {
  stats <- basic_stats(vals, col_name)
  
  # Uniqueness
  stats$is_unique <- stats$n_unique == stats$n_valid
  stats$duplicate_count <- stats$n_valid - stats$n_unique
  
  if (stats$duplicate_count > 0) {
    dup_vals <- vals[duplicated(vals) & !is.na(vals)]
    stats$duplicate_examples <- head(unique(dup_vals), 10)
    stats$max_duplicates <- max(table(vals[!is.na(vals)]))
  }
  
  # Pattern detection
  sample_vals <- head(vals[!is.na(vals) & vals != ""], 100)
  stats$sample_values <- head(sample_vals, 5)
  
  # Check for embedded state prefixes
  stats$has_state_prefix <- any(grepl("^[A-Z]{2}[0-9]", sample_vals))
  
  # Numeric ID check
  stats$all_numeric <- all(grepl("^[0-9]+$", sample_vals))
  
  # Length distribution
  lengths <- nchar(sample_vals)
  stats$length_range <- list(min = min(lengths), max = max(lengths))
  stats$length_uniform <- min(lengths) == max(lengths)
  
  stats
}

# =============================================================================
# 4. ANALYSIS ORCHESTRATOR
# =============================================================================

#' Analyze a single dataset using variable registry
analyze_dataset <- function(dt, state_abbrev, source_key, sink_active = TRUE) {
  if (is.null(dt) || nrow(dt) == 0) return(invisible(NULL))
  if (".error" %in% names(dt)) {
    cat(sprintf("  [ERROR in table]: %s\n", dt$.error[1]))
    return(invisible(NULL))
  }
  
  # Get variable definitions for this state/source
  state_vars <- STATE_VAR_REGISTRY[[state_abbrev]]
  if (is.null(state_vars)) {
    cat(sprintf("  [WARN] No variable registry for state: %s\n", state_abbrev))
    return(invisible(NULL))
  }
  
  source_vars <- state_vars[[source_key]]
  if (is.null(source_vars)) {
    # Try fuzzy match on source key
    possible_keys <- names(state_vars)
    matched_key <- possible_keys[grepl(source_key, possible_keys, ignore.case = TRUE)][1]
    if (!is.na(matched_key)) {
      source_vars <- state_vars[[matched_key]]
    } else {
      cat(sprintf("  [WARN] No variable definitions for source: %s\n", source_key))
      return(invisible(NULL))
    }
  }
  
  # Get actual column names in data
  actual_cols <- names(dt)
  
  # Header
  cat(strrep("=", 90), "\n")
  cat(sprintf("STATE: %s (%s) | SOURCE: %s\n", 
              STATE_ABBREV[state_abbrev] %||% state_abbrev, state_abbrev, 
              source_vars$source %||% source_key))
  cat(sprintf("Rows: %s | Columns: %d\n", format(nrow(dt), big.mark = ","), ncol(dt)))
  cat(strrep("=", 90), "\n\n")
  
  # Process each variable type
  var_types <- c("id_vars", "date_vars", "categorical_vars", "flag_vars", 
                 "numeric_vars", "status_vars", "moc_flags", "substance_flags",
                 "release_det_flags", "cp_flags", "sop_flags", "pipe_moc_flags",
                 "pipe_type_flags", "pipe_rd_flags", "pipe_cp_flags",
                 "damage_flags", "abatement_flags", "remediation_flags",
                 "geo_vars", "address_vars", "name_vars", "count_vars")
  
  for (var_type in var_types) {
    target_vars <- source_vars[[var_type]]
    if (is.null(target_vars) || length(target_vars) == 0) next
    
    # Convert to clean_names format for matching
    target_vars_clean <- make_clean_names(target_vars)
    
    # Find matches in actual data
    matched_vars <- intersect(actual_cols, target_vars_clean)
    
    # Also try original names (some files don't get cleaned)
    matched_vars <- unique(c(matched_vars, intersect(actual_cols, target_vars)))
    
    if (length(matched_vars) == 0) next
    
    cat(strrep("-", 70), "\n")
    cat(sprintf("VARIABLE TYPE: %s (%d of %d found)\n", 
                toupper(gsub("_", " ", var_type)), length(matched_vars), length(target_vars)))
    cat(strrep("-", 70), "\n\n")
    
    for (col in matched_vars) {
      vals <- dt[[col]]
      
      # Select diagnostic based on variable type
      diag_result <- switch(
        var_type,
        id_vars = diagnose_id(vals, col),
        date_vars = diagnose_date(vals, col),
        numeric_vars = diagnose_numeric(vals, col),
        count_vars = diagnose_numeric(vals, col),
        flag_vars = diagnose_flag(vals, col),
        moc_flags = diagnose_flag(vals, col),
        substance_flags = diagnose_flag(vals, col),
        release_det_flags = diagnose_flag(vals, col),
        cp_flags = diagnose_flag(vals, col),
        sop_flags = diagnose_flag(vals, col),
        pipe_moc_flags = diagnose_flag(vals, col),
        pipe_type_flags = diagnose_flag(vals, col),
        pipe_rd_flags = diagnose_flag(vals, col),
        pipe_cp_flags = diagnose_flag(vals, col),
        damage_flags = diagnose_flag(vals, col),
        abatement_flags = diagnose_flag(vals, col),
        remediation_flags = diagnose_flag(vals, col),
        geo_vars = diagnose_numeric(vals, col),
        diagnose_categorical(vals, col)  # Default
      )
      
      # Print results
      print_diagnostic(diag_result, var_type)
    }
    
    # Report missing target variables
    missing_vars <- setdiff(target_vars_clean, actual_cols)
    missing_vars <- setdiff(missing_vars, matched_vars)  # Exclude already matched
    if (length(missing_vars) > 0 && length(missing_vars) <= 20) {
      cat(sprintf("\n  [NOT FOUND in data]: %s\n", paste(missing_vars, collapse = ", ")))
    } else if (length(missing_vars) > 20) {
      cat(sprintf("\n  [NOT FOUND]: %d variables (list truncated)\n", length(missing_vars)))
    }
    cat("\n")
  }
  
  invisible(NULL)
}

#' Print diagnostic results
print_diagnostic <- function(diag, var_type) {
  cat(sprintf("  COLUMN: %s\n", diag$column))
  cat(sprintf("    Total: %s | Missing: %s (%.1f%%) | Unique: %s\n",
              format(diag$n_total, big.mark = ","),
              format(diag$n_missing, big.mark = ","),
              diag$pct_missing,
              format(diag$n_unique, big.mark = ",")))
  
  # Type-specific output
  if (var_type == "date_vars" || !is.null(diag$date_range)) {
    if (!is.null(diag$sample_values)) {
      cat(sprintf("    Sample: %s\n", diag$sample_values))
    }
    if (!is.null(diag$parse_success_rate)) {
      cat(sprintf("    Parse Success: %.1f%%\n", diag$parse_success_rate))
    }
    if (!is.null(diag$date_range)) {
      cat(sprintf("    Range: %s to %s\n", diag$date_range$min, diag$date_range$max))
    }
    if (!is.null(diag$likely_excel_serial) && diag$likely_excel_serial) {
      cat("    [!] Likely Excel serial date format\n")
    }
    if (!is.null(diag$pre_1970) && diag$pre_1970 > 0) {
      cat(sprintf("    [!] %d dates before 1970\n", diag$pre_1970))
    }
    if (!is.null(diag$post_2030) && diag$post_2030 > 0) {
      cat(sprintf("    [!] %d dates after 2030\n", diag$post_2030))
    }
    if (!is.null(diag$format_detection)) {
      formats <- diag$format_detection[diag$format_detection > 0]
      if (length(formats) > 0) {
        cat(sprintf("    Detected formats: %s\n", 
                    paste(names(formats), "=", formats, collapse = ", ")))
      }
    }
    
  } else if (var_type %in% c("numeric_vars", "count_vars", "geo_vars")) {
    if (!is.null(diag$numeric_conversion_rate)) {
      cat(sprintf("    Numeric Parse: %.1f%%\n", diag$numeric_conversion_rate))
    }
    if (!is.null(diag$numeric_stats)) {
      s <- diag$numeric_stats
      cat(sprintf("    Range: [%s, %s] | Median: %s | Mean: %s\n",
                  format(s$min, big.mark = ","), format(s$max, big.mark = ","),
                  format(s$median, big.mark = ","), format(s$mean, big.mark = ",")))
    }
    if (!is.null(diag$non_numeric_examples)) {
      cat(sprintf("    Non-numeric values: %s\n", 
                  paste(head(diag$non_numeric_examples, 5), collapse = ", ")))
    }
    
  } else if (var_type == "id_vars") {
    if (!is.null(diag$is_unique)) {
      status <- if (diag$is_unique) "YES" else sprintf("NO (%d duplicates)", diag$duplicate_count)
      cat(sprintf("    Unique: %s\n", status))
    }
    if (!is.null(diag$sample_values)) {
      cat(sprintf("    Sample: %s\n", paste(diag$sample_values, collapse = ", ")))
    }
    if (!is.null(diag$has_state_prefix) && diag$has_state_prefix) {
      cat("    [!] Contains state prefix (e.g., TX, MI)\n")
    }
    
  } else if (grepl("flag", var_type)) {
    if (!is.null(diag$boolean_breakdown)) {
      b <- diag$boolean_breakdown
      cat(sprintf("    TRUE: %s (%.1f%%) | FALSE: %s | Other: %s\n",
                  format(b$n_true, big.mark = ","), b$pct_true,
                  format(b$n_false, big.mark = ","),
                  format(b$n_other, big.mark = ",")))
    }
    if (!is.null(diag$non_boolean_examples)) {
      cat(sprintf("    Non-boolean values: %s\n", 
                  paste(diag$non_boolean_examples, collapse = ", ")))
    }
    
  } else {
    # Categorical - print frequency table
    if (!is.null(diag$freq_table) && nrow(diag$freq_table) > 0) {
      # Compact format for small tables
      if (nrow(diag$freq_table) <= 15) {
        for (i in seq_len(nrow(diag$freq_table))) {
          row <- diag$freq_table[i]
          val_display <- if (is.na(row$Value) || row$Value == "") "<NA/Empty>" else 
                         substr(row$Value, 1, 50)
          cat(sprintf("      %s: %s (%.1f%%)\n", 
                      val_display, format(row$Count, big.mark = ","), row$Pct))
        }
      } else {
        # Top 10 + truncation note
        for (i in 1:min(10, nrow(diag$freq_table))) {
          row <- diag$freq_table[i]
          val_display <- if (is.na(row$Value) || row$Value == "") "<NA/Empty>" else 
                         substr(row$Value, 1, 50)
          cat(sprintf("      %s: %s (%.1f%%)\n", 
                      val_display, format(row$Count, big.mark = ","), row$Pct))
        }
        if (nrow(diag$freq_table) > 10) {
          cat(sprintf("      ... [%d more values]\n", nrow(diag$freq_table) - 10))
        }
      }
    }
    
    # Quality warnings
    if (!is.null(diag$has_whitespace_variants) && diag$has_whitespace_variants) {
      cat("    [!] Contains leading/trailing whitespace\n")
    }
    if (!is.null(diag$has_case_variants) && diag$has_case_variants) {
      cat("    [!] Contains case variants (potential duplicates)\n")
    }
  }
  
  cat("\n")
}

# =============================================================================
# 5. MAIN EXECUTION
# =============================================================================

run_all_diagnostics <- function(states = NULL) {
  # Default to all states in registry
  if (is.null(states)) {
    states <- names(STATE_VAR_REGISTRY)
  }
  
  sink(out_file)
  on.exit(sink(), add = TRUE)
  
  cat(strrep("#", 90), "\n")
  cat("# UST STATE DATABASE FREQUENCY & DIAGNOSTIC REPORT\n")
  cat(sprintf("# Generated: %s\n", Sys.time()))
  cat(sprintf("# States: %s\n", paste(states, collapse = ", ")))
  cat(strrep("#", 90), "\n\n")
  
  for (state_abbrev in states) {
    file_paths <- STATE_FILE_PATHS[[state_abbrev]]
    
    if (is.null(file_paths)) {
      cat(sprintf("\n[SKIP] No file paths configured for state: %s\n", state_abbrev))
      next
    }
    
    cat(sprintf("\n%s\n", strrep("*", 90)))
    cat(sprintf("*** PROCESSING STATE: %s (%s) ***\n", 
                STATE_ABBREV[state_abbrev] %||% state_abbrev, state_abbrev))
    cat(sprintf("%s\n\n", strrep("*", 90)))
    
    for (source_key in names(file_paths)) {
      path <- file_paths[[source_key]]
      
      cat(sprintf("\n>>> Loading: %s\n", basename(path)))
      
      result <- load_file(path)
      
      if (!is.null(result$error)) {
        cat(sprintf("    [ERROR] %s\n", result$error))
        next
      }
      
      dt <- result$dt
      
      # Handle Access databases (list of tables)
      if (is.list(dt) && !is.null(attr(dt, "is_mdb"))) {
        cat(sprintf("    [MDB] Found %d tables\n", length(dt)))
        
        for (tbl_name in names(dt)) {
          tbl_dt <- dt[[tbl_name]]
          if (!is.data.table(tbl_dt) || nrow(tbl_dt) == 0) next
          
          # Map MDB table names to source_key in registry
          # Arkansas tables: TempTankStats_UG -> tanks_ug, TempRFACS_CountryNO -> facilities
          mapped_key <- switch(
            tolower(tbl_name),
            "temptankstats_ug" = "tanks_ug",
            "temptankstats_ag" = "tanks_ag",
            "temprfacs_countryno" = "facilities",
            "temprlustlog1" = "lust",
            "tempgis" = "gis",
            "tempownerlist" = "owners",
            "tempeligcert1" = "eligibility",
            "tempinspectionslist2" = "inspections",
            tolower(tbl_name)  # Fallback
          )
          
          analyze_dataset(tbl_dt, state_abbrev, mapped_key)
        }
      } else if (is.data.table(dt)) {
        analyze_dataset(dt, state_abbrev, source_key)
      }
    }
  }
  
  cat("\n\n", strrep("=", 90), "\n")
  cat("END OF REPORT\n")
  cat(strrep("=", 90), "\n")
  
  message("Report written to: ", out_file)
}

# =============================================================================
# 6. RUN
# =============================================================================

# Execute for all states with configured file paths
run_all_diagnostics()

# Or run for specific states:
# run_all_diagnostics(c("AR", "TX", "CO"))
