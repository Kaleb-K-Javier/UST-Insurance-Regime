#!/usr/bin/env Rscript
# ==============================================================================
# MASTER PROFILING RUNNER (v5.1 - Fixed Paths)
# ==============================================================================
# PURPOSE: 
#   Sets the run environment (Debug/Prod) and launches the pipeline.
#   Passes configuration to child scripts via Environment Variables.
# ==============================================================================

# 1. SET GLOBAL MODE HERE
# -----------------------
# Options: "DEBUG", "PROFILING", "PRODUCTION"
RUN_MODE <- "PROFILING"  

# 2. SETUP
# -----------------------
if (!require("here")) install.packages("here", repos = "https://cloud.r-project.org/")
library(here)

# Set the environment variable so Script A & B can read it
Sys.setenv(LEAK_PIPELINE_MODE = RUN_MODE)

cat(rep("=", 80), "\n", sep = "")
cat(sprintf("MASTER RUNNER STARTED | MODE: %s\n", RUN_MODE))
cat(rep("=", 80), "\n\n", sep = "")

# Create logs directory
dir.create(here("logs"), showWarnings = FALSE)

# 3. DEFINE SCRIPTS (Fixed Relative Paths)
# -----------------------
# Pointing to: ROOT/Code/Public_to_Private/Script_Name.R
scripts <- list(
  A = here("Code", "Public_to_Private", "07_Script_A_FINAL_WITH_MULTIFOLD.R"),
  B = here("Code", "Public_to_Private", "07_Script_B_FINAL_WITH_MULTIFOLD.R") 
)

# Verify files exist
for (s in scripts) {
  if (!file.exists(s)) stop(sprintf("CRITICAL ERROR: Script not found at %s", s))
}

# 4. EXECUTION FUNCTION
# -----------------------
run_pipeline_step <- function(name, script_path) {
  log_file <- here("logs", paste0(name, "_", tolower(RUN_MODE), ".log"))
  
  cat(sprintf(">> STARTING %s...\n", name))
  cat(sprintf("   Script: %s\n", basename(script_path)))
  cat(sprintf("   Log:    %s\n", basename(log_file)))
  
  start_t <- Sys.time()
  
  # Run Rscript and capture exit code
  # Note: quoting script_path ensures spaces in paths (like "Public_to_Private") work
  exit_code <- system2("Rscript", args = c(shQuote(script_path)), stdout = log_file, stderr = log_file)
  
  end_t <- Sys.time()
  duration <- round(difftime(end_t, start_t, units = "mins"), 2)
  
  if (exit_code == 0) {
    cat(sprintf("   ✓ SUCCESS. Runtime: %s mins\n\n", duration))
    return(duration)
  } else {
    cat(sprintf("   !!! FAILURE. Exit Code: %s\n", exit_code))
    cat(sprintf("   !!! Check log file: %s\n", log_file))
    stop("Pipeline halted due to script failure.")
  }
}

# 5. RUN PIPELINE
# -----------------------
total_start <- Sys.time()

# # Run A
dur_a <- run_pipeline_step("Script_A", scripts$A)

# Run B (Only runs if A succeeds)
dur_b <- run_pipeline_step("Script_B", scripts$B)

# 6. RESOURCE ESTIMATOR
# -----------------------
cat(rep("-", 80), "\n", sep = "")
cat("RESOURCE UTILIZATION ESTIMATES:\n")

analyze_log <- function(log_path) {
  if(!file.exists(log_path)) return()
  lines <- readLines(log_path)
  
  # Look for the Sparse Matrix size line we added in v25
  mem_line <- grep("Memory Size:", lines, value = TRUE)
  if(length(mem_line) > 0) {
    cat(sprintf("  %s: %s\n", basename(log_path), trimws(mem_line[1])))
  }
  
  # Look for GC max used
  gc_lines <- grep("Ncells", lines, value = TRUE)
  if(length(gc_lines) > 0) {
    cat(sprintf("  %s: GC logs found (Check log for 'max used' details)\n", basename(log_path)))
  }
}

analyze_log(here("logs", paste0("Script_A_", tolower(RUN_MODE), ".log")))
analyze_log(here("logs", paste0("Script_B_", tolower(RUN_MODE), ".log")))

total_end <- Sys.time()
cat(sprintf("\nTOTAL PIPELINE TIME: %.2f minutes\n", difftime(total_end, total_start, units = "mins")))
cat(rep("=", 80), "\n")