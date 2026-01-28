###############################################################################
# DIAGNOSTIC: Identify Malformed CSV Rows
# Purpose: Find rows with delimiter issues (unquoted commas) in USTs.csv
###############################################################################

library(data.table)
library(here)

# 1. Setup Path ---------------------------------------------------------------
# Update this if you want to check Facilities.csv or Releases.csv instead
target_file <- here("Data", "Raw", "Facilities.csv")

if (!file.exists(target_file)) stop("File not found: ", target_file)

message("Target File: ", target_file)
message("File Size:   ", round(file.size(target_file) / 1e6, 2), " MB")

# 2. Stage 1: The "Tiny" Read (4 Rows) ----------------------------------------
message("\n--- Stage 1: Reading First 4 Rows ---")
dt_tiny <- tryCatch(
  fread(target_file, nrows = 4),
  error = function(e) { message("Error reading tiny chunk: ", e$message); return(NULL) }
)

if (!is.null(dt_tiny)) {
  expected_cols <- ncol(dt_tiny)
  message("Expected Columns (from header): ", expected_cols)
  print(names(dt_tiny))
}

# 3. Stage 2: The "Medium" Read (10,000 Rows) ---------------------------------
message("\n--- Stage 2: Reading First 10,000 Rows ---")
dt_med <- tryCatch(
  fread(target_file, nrows = 10000),
  error = function(e) { message("Error reading medium chunk: ", e$message); return(NULL) }
)

if (!is.null(dt_med)) {
  message("Medium Read Columns: ", ncol(dt_med))
  if (ncol(dt_med) != expected_cols) {
    warning("CRITICAL: Medium read has different column count than Tiny read!")
  } else {
    message("✓ Column count matches.")
  }
}

# 4. Stage 3: The "Full" Read (Attempt) ---------------------------------------
message("\n--- Stage 3: Reading Full File ---")
# Use fill=TRUE to prevent crashing, so we can inspect the mess
dt_full <- tryCatch(
  fread(target_file, fill = TRUE), 
  error = function(e) { message("Error reading full file: ", e$message); return(NULL) }
)

if (!is.null(dt_full)) {
  message("Full Read Rows:    ", nrow(dt_full))
  message("Full Read Columns: ", ncol(dt_full))
  
  # Check for NAs in critical columns (Facility ID usually shouldn't be NA)
  na_count <- sum(is.na(dt_full[[1]]))
  message("NAs in Column 1:   ", na_count, " (", round(na_count/nrow(dt_full)*100, 2), "%)")
}

# 5. The "Bad Row Hunter" (Deep Scan) -----------------------------------------
message("\n--- Stage 4: Deep Scan for Delimiter Issues ---")
message("Scanning file line-by-line to count separators (this may take a minute)...")

# Use Base R's count.fields to detect rows with wrong number of commas
# 'sep' is comma, 'quote' handles standard quotes
# We read the whole file to find inconsistencies
tryCatch({
  # Read just the field counts, not the data
  field_counts <- count.fields(target_file, sep = ",", quote = "\\#", skip = 0)
  
  # The header determines the "Truth"
  header_count <- field_counts[1]
  
  # Find rows that deviate
  bad_rows_indices <- which(field_counts != header_count)
  
  if (length(bad_rows_indices) == 0) {
    message("\n✓ NO STRUCTURE ISSUES DETECTED.")
    message("All ", length(field_counts), " rows have exactly ", header_count, " columns.")
  } else {
    message("\n!!! ISSUES DETECTED !!!")
    message("Found ", length(bad_rows_indices), " rows with mismatched column counts.")
    message("Expected: ", header_count, " columns.")
    
    # Show the first few culprits
    head_bad <- head(bad_rows_indices, 5)
    message("\nExamples of bad line numbers (1-based index): ", paste(head_bad, collapse = ", "))
    
    message("\n--- Inspecting the first bad row ---")
    bad_line_num <- head_bad[1]
    
    # Read specifically the bad line to show you the content
    bad_raw_text <- readLines(target_file, n = bad_line_num)[bad_line_num]
    print(bad_raw_text)
    
    message("\nDiagnosis:")
    actual_cnt <- field_counts[bad_line_num]
    if (actual_cnt > header_count) {
      message("Result: Row has ", actual_cnt, " columns (Too Many). Likely an unquoted comma.")
    } else {
      message("Result: Row has ", actual_cnt, " columns (Too Few). Likely a newline inside a quote or truncated line.")
    }
  }
}, error = function(e) {
  message("Deep scan failed: ", e$message)
})




################################################################################
# DEEP CSV DIAGNOSTIC & REPAIR TOOLKIT
# Purpose: Identify exact quote/delimiter issues and provide repair options
################################################################################

library(data.table)
library(stringr)
library(here)

# Configuration ----------------------------------------------------------------
target_file <- here("Data", "Raw", "USTs.csv")
output_dir <- here("Data", "Diagnostics")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# Helper Functions -------------------------------------------------------------

#' Check if a line has balanced quotes
check_quote_balance <- function(line) {
  # Count unescaped double quotes
  quotes <- str_count(line, '(?<!\\\\)"')
  list(
    balanced = (quotes %% 2 == 0),
    quote_count = quotes
  )
}

#' Analyze a specific line for issues
analyze_line <- function(line, expected_cols = 10) {
  # Basic stats
  comma_count <- str_count(line, ",")
  quote_info <- check_quote_balance(line)
  
  # Try to parse fields manually
  # Simple split (ignoring quotes for now)
  simple_fields <- str_split(line, ",", simplify = TRUE)
  
  # Check for specific patterns
  has_trailing_comma <- str_detect(line, ",$")
  has_empty_quoted <- str_detect(line, '""')
  has_unmatched_quote <- !quote_info$balanced
  
  # Look for problematic patterns
  problems <- c()
  if (comma_count != expected_cols - 1) {
    problems <- c(problems, sprintf("Wrong comma count: %d (expected %d)", 
                                   comma_count, expected_cols - 1))
  }
  if (has_unmatched_quote) {
    problems <- c(problems, sprintf("Unbalanced quotes: %d total", 
                                   quote_info$quote_count))
  }
  if (has_trailing_comma) {
    problems <- c(problems, "Trailing comma")
  }
  
  list(
    line = line,
    comma_count = comma_count,
    field_count = length(simple_fields),
    quote_count = quote_info$quote_count,
    quote_balanced = quote_info$balanced,
    problems = problems,
    has_empty_quoted = has_empty_quoted
  )
}

# Stage 1: Identify ALL Bad Rows ----------------------------------------------
message("=" , rep("=", 78), "=")
message("STAGE 1: COMPREHENSIVE BAD ROW IDENTIFICATION")
message("=" , rep("=", 78), "=\n")

message("Reading file to count fields...")
field_counts <- count.fields(target_file, sep = ",", quote = "\"", skip = 0)
expected_cols <- field_counts[1]  # Header defines expected

message(sprintf("Expected columns: %d", expected_cols))
message(sprintf("Total lines (including header): %d\n", length(field_counts)))

# Find all bad rows (excluding header)
bad_indices <- which(field_counts[-1] != expected_cols) + 1  # +1 to account for header
message(sprintf("Found %d problematic rows\n", length(bad_indices)))

# Save the bad row indices
bad_df <- data.frame(
  line_number = bad_indices,
  field_count = field_counts[bad_indices],
  expected = expected_cols,
  difference = field_counts[bad_indices] - expected_cols
)

write.csv(bad_df, file.path(output_dir, "bad_row_summary.csv"), row.names = FALSE)
message(sprintf("✓ Saved summary to: %s\n", file.path(output_dir, "bad_row_summary.csv")))

# Stage 2: Deep Analysis of Bad Rows ------------------------------------------
message("=" , rep("=", 78), "=")
message("STAGE 2: DETAILED ANALYSIS OF PROBLEMATIC ROWS")
message("=" , rep("=", 78), "=\n")

# Read the first 50 bad rows for detailed inspection
n_inspect <- min(50, length(bad_indices))
message(sprintf("Analyzing first %d bad rows in detail...\n", n_inspect))

# Read specific lines
con <- file(target_file, "r")
all_lines <- readLines(con)
close(con)

# Analyze each bad row
bad_analysis <- list()
for (i in 1:n_inspect) {
  line_num <- bad_indices[i]
  line_text <- all_lines[line_num]
  
  analysis <- analyze_line(line_text, expected_cols)
  analysis$line_number <- line_num
  bad_analysis[[i]] <- analysis
  
  if (i <= 5) {  # Print first 5 in detail
    message(sprintf("\n--- BAD ROW #%d (Line %d) ---", i, line_num))
    message(sprintf("Field Count: %d (expected %d)", 
                   field_counts[line_num], expected_cols))
    message(sprintf("Quote Count: %d (balanced: %s)", 
                   analysis$quote_count, analysis$quote_balanced))
    message("Problems:")
    for (prob in analysis$problems) {
      message(sprintf("  • %s", prob))
    }
    message("\nRaw line (first 200 chars):")
    message(substr(line_text, 1, 200))
    if (nchar(line_text) > 200) message("...")
  }
}

# Stage 3: Pattern Analysis ---------------------------------------------------
message("\n\n", rep("=", 80))
message("STAGE 3: PATTERN ANALYSIS")
message(rep("=", 80), "\n")

# Categorize issues
quote_issues <- sum(sapply(bad_analysis, function(x) !x$quote_balanced))
comma_issues <- sum(sapply(bad_analysis, function(x) x$comma_count != expected_cols - 1))
trailing_commas <- sum(sapply(bad_analysis, function(x) 
  str_detect(x$line, ",$")))

message(sprintf("Quote balance issues:  %d / %d (%.1f%%)", 
               quote_issues, n_inspect, 100 * quote_issues / n_inspect))
message(sprintf("Wrong comma count:     %d / %d (%.1f%%)", 
               comma_issues, n_inspect, 100 * comma_issues / n_inspect))
message(sprintf("Trailing commas:       %d / %d (%.1f%%)", 
               trailing_commas, n_inspect, 100 * trailing_commas / n_inspect))

# Stage 4: Quote Position Analysis --------------------------------------------
message("\n\n", rep("=", 80))
message("STAGE 4: QUOTE POSITION ANALYSIS")
message(rep("=", 80), "\n")

message("Analyzing quote positions in bad rows...\n")

for (i in 1:min(10, length(bad_analysis))) {
  line <- bad_analysis[[i]]$line
  line_num <- bad_analysis[[i]]$line_number
  
  # Find all quote positions
  quote_positions <- str_locate_all(line, '"')[[1]]
  
  if (nrow(quote_positions) > 0) {
    message(sprintf("\nLine %d: %d quotes at positions:", 
                   line_num, nrow(quote_positions)))
    message(paste(quote_positions[, "start"], collapse = ", "))
    
    # Show context around each quote
    for (j in 1:min(3, nrow(quote_positions))) {
      pos <- quote_positions[j, "start"]
      start_ctx <- max(1, pos - 20)
      end_ctx <- min(nchar(line), pos + 20)
      context <- substr(line, start_ctx, end_ctx)
      message(sprintf('  Quote %d context: ...%s...', j, context))
    }
  }
}

# Stage 5: Generate Repair Strategies -----------------------------------------
message("\n\n", rep("=", 80))
message("STAGE 5: REPAIR STRATEGY RECOMMENDATIONS")
message(rep("=", 80), "\n")

message("Based on the analysis, here are potential repair strategies:\n")

# Strategy 1: Remove orphan quotes
message("STRATEGY 1: Remove/Escape Orphan Quotes")
message("  • Applies when: Odd number of quotes in a line")
message("  • Action: Replace orphan \" with escaped \\\" or remove it")
message("  • Risk: Low - preserves data structure\n")

# Strategy 2: Add missing quote
message("STRATEGY 2: Add Missing Closing Quote")
message("  • Applies when: Field starts with quote but doesn't end with one")
message("  • Action: Add closing quote before next comma")
message("  • Risk: Medium - may misalign fields\n")

# Strategy 3: Quote the entire field
message("STRATEGY 3: Properly Quote Fields With Commas")
message("  • Applies when: Unquoted field contains comma")
message("  • Action: Wrap entire field in quotes")
message("  • Risk: Low - standard CSV fix\n")

# Strategy 4: Manual review
message("STRATEGY 4: Flag for Manual Review")
message("  • Applies when: Complex issues detected")
message("  • Action: Export to separate file for human review")
message("  • Risk: None - human judgment required\n")

# Export detailed report
report_file <- file.path(output_dir, "detailed_analysis.txt")
sink(report_file)
cat("DETAILED BAD ROW ANALYSIS\n")
cat(rep("=", 80), "\n\n")
for (i in 1:min(20, length(bad_analysis))) {
  cat(sprintf("\n--- ROW %d (Line %d) ---\n", i, bad_analysis[[i]]$line_number))
  cat(sprintf("Problems: %s\n", paste(bad_analysis[[i]]$problems, collapse = "; ")))
  cat(sprintf("Raw line:\n%s\n", bad_analysis[[i]]$line))
}
sink()

message(sprintf("\n✓ Detailed report saved to: %s", report_file))

# Final Summary ----------------------------------------------------------------
message("\n\n", rep("=", 80))
message("DIAGNOSTIC COMPLETE")
message(rep("=", 80), "\n")

message("Files created:")
message(sprintf("  1. %s - List of all bad rows", 
               file.path(output_dir, "bad_row_summary.csv")))
message(sprintf("  2. %s - Detailed analysis\n", 
               file.path(output_dir, "detailed_analysis.txt")))

message("Next steps:")
message("  1. Review the detailed analysis to understand the patterns")
message("  2. Look at the quote positions in Stage 4 output")
message("  3. Build a targeted repair script based on what you find\n")




################################################################################
# CONFIRM THE # CHARACTER ISSUE
################################################################################

library(here)

target_file <- here("Data", "Raw", "USTs.csv")

message("Testing count.fields() with and without comment.char...\n")

# The WRONG way (what we were doing)
message("1. With default comment.char = '#':")
field_counts_wrong <- count.fields(target_file, sep = ",", quote = '"', skip = 0)
expected <- field_counts_wrong[1]
bad_wrong <- which(field_counts_wrong[-1] != expected)
message(sprintf("   Bad rows detected: %d\n", length(bad_wrong)))

# The RIGHT way
message("2. With comment.char = '' (disabled):")
field_counts_right <- count.fields(target_file, sep = ",", quote = '"', 
                                   comment.char = "", skip = 0)
expected <- field_counts_right[1]
bad_right <- which(field_counts_right[-1] != expected)
message(sprintf("   Bad rows detected: %d\n", length(bad_right)))

if (length(bad_right) == 0) {
  message("✓✓✓ PROBLEM SOLVED! ✓✓✓")
  message("\nThe issue was count.fields() treating '#' as a comment character.")
  message("Your data is actually fine!")
  message("\nTo read the CSV properly, use:")
  message('  dt <- fread("USTs.csv")  # fread handles this correctly')
  message("  OR")
  message('  read.csv("USTs.csv", comment.char = "")')
} else {
  message(sprintf("\n⚠ Still have %d problematic rows to investigate", length(bad_right)))
  message("These need further diagnosis...")
  
  # Show first few
  message("\nFirst few bad lines:")
  lines <- readLines(target_file)
  for (i in 1:min(5, length(bad_right))) {
    line_num <- bad_right[i] + 1
    message(sprintf("\nLine %d:", line_num))
    message(substr(lines[line_num], 1, 150))
  }
}
