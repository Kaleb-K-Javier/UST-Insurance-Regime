#==============================================================================
# TABLE HELPER: Universal Table Export with Wild Cluster Bootstrap
# 
# Location: Code/Helpers/table_helper.R
#
# Creates publication-ready tables in three formats:
#   1. CSV  - Machine-readable (for QMD/RMarkdown integration)
#   2. TEX  - Polished LaTeX with WCB p-values injected
#   3. TXT  - Human-readable console output with WCB summary
#
# Dependencies:
#   - fixest (for etable, coef, se, pvalue, nobs, r2)
#   - data.table (for data.table, fwrite)
#   - here (for path management)
#   - Parent script must set OUTPUT_TABLES path
#
# Usage:
#   source("Code/Helpers/table_helper.R")
#   save_universal_table(
#     models = list(m1, m2, m3),
#     wcb_results = list(wcb1, wcb2, wcb3),
#     headers = c("Model 1", "Model 2", "Model 3"),
#     base_name = "A4_LUST_StepIn_Controls",
#     title = "LUST DiD: Step-In Control Specifications",
#     digits = 6
#   )
#==============================================================================

#------------------------------------------------------------------------------
# save_universal_table()
#
# Arguments:
#   models      - List of fixest model objects
#   wcb_results - List of WCB results from fast_wild_bootstrap() (same length as models)
#   headers     - Character vector of column headers (same length as models)
#   base_name   - Base filename (without extension)
#   title       - Title for table header
#   digits      - Number of decimal places (default 4; use 6 for LUST/closure rates)
#   output_dir  - Output directory path (default: OUTPUT_TABLES global or "Output/Tables")
#   treatment_var - Name of treatment coefficient (default: "texas_treated:post_1999")
#
# Returns:
#   data.table of coefficient summary (invisibly)
#
# Side Effects:
#   Creates three files: {base_name}.csv, {base_name}.tex, {base_name}.txt
#------------------------------------------------------------------------------
save_universal_table <- function(
    models,
    wcb_results,
    headers,
    base_name,
    title,
    digits = 4,
    output_dir = NULL,
    treatment_var = "texas_treated:post_1999"
) {
  
  # Resolve output directory
  if (is.null(output_dir)) {
    if (exists("OUTPUT_TABLES")) {
      output_dir <- OUTPUT_TABLES
    } else {
      output_dir <- here::here("Output", "Tables")
    }
  }
  
  # Ensure output directory exists
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  
  n_models <- length(models)
  
  # Validate inputs
  stopifnot(
    "models and wcb_results must have same length" = length(wcb_results) == n_models,
    "models and headers must have same length" = length(headers) == n_models
  )
  
  #---------------------------------------------------------------------------
  # 1. CONSOLE OUTPUT
  #---------------------------------------------------------------------------
  cat(sprintf("\n=== %s ===\n", title))
  print(fixest::etable(models, headers = headers, se.below = TRUE, fitstat = c("n", "r2")))
  
  #---------------------------------------------------------------------------
  # 2. CSV EXPORT (Machine-readable for QMD/RMarkdown)
  #---------------------------------------------------------------------------
  coef_dt <- data.table::data.table()
  
  for (i in seq_along(models)) {
    m <- models[[i]]
    wcb <- wcb_results[[i]]
    
    # Extract treatment coefficient
    all_coefs <- coef(m)
    all_se <- se(m)
    all_pval <- pvalue(m)
    
    # Check if treatment variable exists
    if (!treatment_var %in% names(all_coefs)) {
      warning(sprintf("Treatment variable '%s' not found in model %d. Using NA.", treatment_var, i))
      treat_coef <- NA_real_
      treat_se <- NA_real_
      treat_pval <- NA_real_
    } else {
      treat_coef <- all_coefs[[treatment_var]]
      treat_se <- all_se[[treatment_var]]
      treat_pval <- all_pval[[treatment_var]]
    }
    
    # Build row
    row_dt <- data.table::data.table(
      Model = headers[i],
      Coefficient = treat_coef,
      SE = treat_se,
      CI_Lower = treat_coef - 1.96 * treat_se,
      CI_Upper = treat_coef + 1.96 * treat_se,
      Clustered_P = treat_pval,
      WCB_P = wcb$p_val,
      WCB_Label = wcb$label,
      N = nobs(m),
      R2 = tryCatch(r2(m, type = "r2"), error = function(e) NA_real_)
    )
    
    coef_dt <- rbind(coef_dt, row_dt)
  }
  
  # Write CSV
  csv_file <- file.path(output_dir, paste0(base_name, ".csv"))
  data.table::fwrite(coef_dt, csv_file)
  
  #---------------------------------------------------------------------------
  # 3. LATEX EXPORT (with WCB p-values injected)
  #---------------------------------------------------------------------------
  tex_file <- file.path(output_dir, paste0(base_name, ".tex"))
  
  # Generate base LaTeX table
  fixest::etable(
    models,
    headers = headers,
    se.below = TRUE,
    fitstat = c("n", "r2"),
    digits = digits,
    tex = TRUE,
    title = title,
    file = tex_file,
    replace = TRUE
  )
  
  # Read and modify LaTeX file to inject WCB row
  tex_lines <- readLines(tex_file)
  
  # Find the treatment effect row (texas_treated:post_1999 or similar)
  # Escape special LaTeX characters in the pattern
  treat_pattern <- gsub(":", ".*", gsub("_", "\\\\_", treatment_var))
  treat_row_idx <- grep(treat_pattern, tex_lines)
  
  if (length(treat_row_idx) > 0) {
    # Get the SE row (should be right after treatment row)
    se_row_idx <- treat_row_idx[1] + 1
    
    # Create WCB p-value row
    wcb_pvals <- sapply(wcb_results, function(x) {
      sprintf("%.4f%s", x$p_val,
              ifelse(x$p_val < 0.01, "***",
                     ifelse(x$p_val < 0.05, "**",
                            ifelse(x$p_val < 0.1, "*", ""))))
    })
    
    wcb_row <- paste0("$[$WCB p-val$]$ & ", paste(wcb_pvals, collapse = " & "), " \\\\")
    
    # Insert WCB row after SE row
    tex_lines <- c(
      tex_lines[1:se_row_idx],
      wcb_row,
      tex_lines[(se_row_idx + 1):length(tex_lines)]
    )
    
    # Add footnote about WCB
    note_idx <- grep("\\\\end\\{tabular\\}", tex_lines)[1] - 1
    if (!is.na(note_idx) && note_idx > 0) {
      wcb_note <- sprintf(
        "\\multicolumn{%d}{l}{\\footnotesize WCB p-val = Wild cluster bootstrap p-value (Roodman et al. 2019, 9,999 reps)} \\\\",
        n_models + 1
      )
      
      tex_lines <- c(
        tex_lines[1:note_idx],
        wcb_note,
        tex_lines[(note_idx + 1):length(tex_lines)]
      )
    }
    
    # Write modified LaTeX
    writeLines(tex_lines, tex_file)
  }
  
  #---------------------------------------------------------------------------
  # 4. TXT EXPORT (Human-readable with WCB summary)
  #---------------------------------------------------------------------------
  txt_file <- file.path(output_dir, paste0(base_name, ".txt"))
  
  sink(txt_file)
  cat(paste0("=== ", title, " ===\n"))
  cat(paste0("Generated: ", Sys.time(), "\n\n"))
  
  # Print standard regression table
  print(fixest::etable(
    models,
    headers = headers,
    se.below = TRUE,
    fitstat = c("n", "r2"),
    digits = digits
  ))
  
  # Print WCB p-values section
  cat("\n\n")
  cat("====================================================================\n")
  cat("Wild Cluster Bootstrap P-Values (Roodman et al. 2019)\n")
  cat("====================================================================\n")
  cat(sprintf("Treatment Effect: %s\n\n", treatment_var))
  
  for (i in seq_along(models)) {
    cat(sprintf("  %-20s: %s (beta = %.*f)\n",
                headers[i],
                wcb_results[[i]]$label,
                digits,
                wcb_results[[i]]$beta_obs))
  }
  
  cat("\n")
  cat("--------------------------------------------------------------------\n")
  cat("Notes:\n")
  cat("  - WCB uses Rademacher weights (+1/-1 with equal probability)\n")
  cat("  - Significance: *** p<0.01, ** p<0.05, * p<0.10\n")
  cat("  - Standard errors clustered at state level\n")
  cat("====================================================================\n")
  sink()
  
  cat(sprintf("✓ Saved: %s (.csv, .tex, .txt)\n", base_name))
  
  invisible(coef_dt)
}

#------------------------------------------------------------------------------
# create_event_study_table()
#
# Specialized function for event study models with WCB confidence intervals.
# Exports CSV suitable for plotting in ggplot2.
#
# Arguments:
#   model       - fixest event study model
#   wcb_result  - Result from get_unified_wcb() with $plot_data
#   base_name   - Base filename
#   title       - Table title
#   ref_date    - Reference date (excluded period)
#   output_dir  - Output directory
#
# Returns:
#   data.table of event study coefficients (invisibly)
#------------------------------------------------------------------------------
create_event_study_table <- function(
    model,
    wcb_result,
    base_name,
    title,
    ref_date = as.Date("1998-12-01"),
    output_dir = NULL
) {
  
  if (is.null(output_dir)) {
    if (exists("OUTPUT_TABLES")) {
      output_dir <- OUTPUT_TABLES
    } else {
      output_dir <- here::here("Output", "Tables")
    }
  }
  
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  
  # Use WCB plot data if available
  if (!is.null(wcb_result$plot_data)) {
    es_dt <- wcb_result$plot_data
    es_dt[, `:=`(
      se_type = "WCB",
      ref_date = ref_date
    )]
  } else {
    # Fallback to standard errors
    es_dt <- data.table::as.data.table(broom::tidy(model, conf.int = TRUE))
    es_dt <- es_dt[grep("event_date", term)]
    es_dt[, event_date := as.Date(regmatches(term, regexpr("\\d{4}-\\d{2}-\\d{2}", term)))]
    es_dt[, se_type := "Clustered"]
  }
  
  # Add pre/post indicator
  es_dt[, period := ifelse(event_date < ref_date, "Pre", "Post")]
  
  # Write CSV
  csv_file <- file.path(output_dir, paste0(base_name, "_event_study.csv"))
  data.table::fwrite(es_dt, csv_file)
  
  cat(sprintf("✓ Saved: %s_event_study.csv\n", base_name))
  
  invisible(es_dt)
}

cat("✓ Table helper loaded (save_universal_table, create_event_study_table)\n")