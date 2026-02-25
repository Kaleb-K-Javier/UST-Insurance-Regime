#==============================================================================
# 02_DiD_Causal_Estimates.R
# Texas UST Insurance Reform — Causal Evidence from DiD
#
# PURPOSE:
#   All causal estimation for the JMP paper Section 3 ("Causal Evidence").
#   Loads analysis-ready .rds files from 01_Descriptive_Analysis.R and produces
#   publication-ready regression tables and event study figures.
#
# ARCHITECTURE: Three-Specification Mandate Confound Design
#   Spec A:  Facilities with 100% post-1988 tanks (mandate-free, HEADLINE)
#   Spec B:  Facilities with 100% pre-1988 tanks + mandate controls (robustness)
#   Pooled:  All facilities + mandate controls (comparison, interpreted cautiously)
#
#   Rationale: Texas 30 TAC Chapter 334 imposed staggered upgrade mandates on
#   pre-1988 tanks (1989–1993, five cohorts by vintage). Control states faced
#   only the federal 12/22/1998 deadline. This creates diverging pre-trends in
#   the pooled sample that have nothing to do with insurance reform. Spec A
#   isolates post-1988 tanks that never faced the TX phased mandate.
#
# PAPER OUTPUTS (JMP table/figure numbering):
#   Table 3:   Primary DiD — Spec A (Post-1988 Cohort) [HEADLINE]
#   Table 4:   Robustness — Spec B with Mandate Controls
#   Table 5:   Robustness — Pooled with Mandate Controls
#   Table 6:   Reported Leak DiD (Spec A headline)
#   Table B.4: Parallel Trends Validation (4-test battery)
#   Table B.5: MD-Excluded Robustness
#   Table B.6: mandate_window_3yr Sensitivity (Spec B)
#   Table F.1: Cloglog / Frailty Robustness
#   Figure 6A: Event Study — Spec A (Main Text) [HEADLINE]
#   Figure B-2: Event Study — Spec B + Mandate Control (Appendix)
#   Figure 7:  Leak Event Study
#
# ADDITIONAL MODELS (not restructured by spec — use existing samples):
#   Model 1B:  Facility Cox Survival
#   Model 2:   Tank-Level Cox HTE (Spec A/B via tank-level cohort_pre1988)
#   Model 3A:  Age at Closure (County FE) — cohort splits via run_cohort_did()
#   Model 3B:  Age at Closure (Facility FE, Spanning)
#   Model 4:   Revealed Leaks at Closure — cohort splits via run_cohort_did()
#   Model 5AB: Competing Risks (Cause-Specific Cox, Fine-Gray)
#
# STRUCTURE:
#   §1   Setup & Data Loading + Mandate Variable Verification
#   §2   Helper Functions
#   §3   Parallel Trends Validation (4-Test Battery, Table B.4)
#   §4   Table 3 — Primary DiD: Spec A (Closure/Exit/Replace) + Event Study
#   §5   Table 4 — Spec B Robustness (Mandate Controls Escalation)
#   §6   Table 5 — Pooled Robustness (Mandate Controls)
#   §7   Table 6 — Reported Leak DiD (Spec A/B/Pooled) + Event Study
#   §8   HTE — Wall Type × Treatment by Spec
#   §9   Model 1B — Facility Cox Survival
#   §10  Model 2  — Tank Cox HTE
#   §11  Model 3A — Age at Closure (County FE) + Event Study
#   §12  Model 3B — Age at Closure (Facility FE)
#   §13  Model 4  — Revealed Leaks at Closure
#   §14  Models 5A/5B — Competing Risks
#   §15  Appendix Robustness (MD, TN, mandate_window_3yr, CS stub)
#   §16  JMP Publication Tables (unified LaTeX)
#   §17  Script Summary
#
# INPUTS:
#   Data/Analysis/analysis_annual_data.rds
#   Data/Analysis/analysis_tank_inventory.rds
#   Data/Analysis/analysis_closed_tanks.rds
#   Data/Analysis/analysis_tanks_1999.rds
#   Data/Analysis/analysis_pre_period_closures.rds
#   Data/Analysis/analysis_metadata.rds
#
# PREREQUISITES: Run 01_Descriptive_Analysis.R first.
#
# Date: February 2026
#==============================================================================


#==============================================================================
# SECTION 1: SETUP & DATA LOADING
#==============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(fixest)
  library(survival)
  library(here)
  library(ggplot2)
  library(broom)
  library(stringr)
})

# Optional bootstrap
if (requireNamespace("fwildclusterboot", quietly = TRUE)) {
  library(fwildclusterboot)
  cat("fwildclusterboot loaded for wild cluster bootstrap.\n")
}

# ── Load metadata (constants from Script 01) ──
ANALYSIS_DIR <- here("Data", "Analysis")

if (!file.exists(file.path(ANALYSIS_DIR, "analysis_metadata.rds")))
  stop("Run 01_Descriptive_Analysis.R first.\n  Missing: ", ANALYSIS_DIR)

meta <- readRDS(file.path(ANALYSIS_DIR, "analysis_metadata.rds"))
TREATMENT_YEAR  <- meta$TREATMENT_YEAR
TREATMENT_DATE  <- meta$TREATMENT_DATE
PANEL_START     <- meta$PANEL_START
PANEL_END       <- meta$PANEL_END
ES_START        <- meta$ES_START
ES_END          <- meta$ES_END
STUDY_END_DATE  <- meta$STUDY_END_DATE
CONTROL_STATES  <- meta$CONTROL_STATES
OUTPUT_TABLES   <- meta$OUTPUT_TABLES
OUTPUT_FIGURES  <- meta$OUTPUT_FIGURES
FEDERAL_MANDATE_DATE <- meta$FEDERAL_MANDATE_DATE
MANDATE_CUTOFF_DATE  <- meta$MANDATE_CUTOFF_DATE
incumbent_ids        <- meta$incumbent_ids

# Bootstrap toggle
USE_BOOTSTRAP <- FALSE   # <-- CHANGE TO TRUE FOR FINAL RUN
N_BOOTSTRAP   <- 9999

# POST_YEAR: first full year under private insurance regime
POST_YEAR <- TREATMENT_YEAR  # 1999

dir.create(OUTPUT_TABLES,  recursive = TRUE, showWarnings = FALSE)
dir.create(OUTPUT_FIGURES, recursive = TRUE, showWarnings = FALSE)

# ── Publication theme ──
theme_pub <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title       = element_text(face = "bold", size = base_size + 2),
      plot.subtitle    = element_text(color = "grey40", size = base_size),
      panel.grid.minor = element_blank(),
      legend.position  = "bottom"
    )
}
theme_set(theme_pub())

cat("====================================================================\n")
cat("02_DiD_Causal_Estimates.R\n")
cat("  Three-Specification Mandate Confound Architecture\n")
cat("====================================================================\n")
cat(sprintf("Analysis: %d to %d | Treatment: %d\n",
            PANEL_START, PANEL_END, TREATMENT_YEAR))
cat(sprintf("Bootstrap: %s (%d reps)\n",
            ifelse(USE_BOOTSTRAP, "ENABLED", "DISABLED"), N_BOOTSTRAP))
cat(sprintf("Control states: %d (NJ excluded)\n", length(CONTROL_STATES)))
cat("====================================================================\n\n")

# ── Load analysis-ready datasets ──
cat("Loading analysis datasets from 01_Descriptive_Analysis.R...\n")

annual_data <- readRDS(file.path(ANALYSIS_DIR, "analysis_annual_data.rds"))
cat(sprintf("  annual_data:     %s rows, %s facilities\n",
            format(nrow(annual_data), big.mark = ","),
            format(uniqueN(annual_data$panel_id), big.mark = ",")))

tank_inventory <- readRDS(file.path(ANALYSIS_DIR, "analysis_tank_inventory.rds"))
cat(sprintf("  tank_inventory:  %s tanks\n",
            format(nrow(tank_inventory), big.mark = ",")))

closed_tanks <- readRDS(file.path(ANALYSIS_DIR, "analysis_closed_tanks.rds"))
cat(sprintf("  closed_tanks:    %s closures\n",
            format(nrow(closed_tanks), big.mark = ",")))

tanks_1999 <- readRDS(file.path(ANALYSIS_DIR, "analysis_tanks_1999.rds"))
cat(sprintf("  tanks_1999:      %s tanks\n",
            format(nrow(tanks_1999), big.mark = ",")))

pre_period_closures <- readRDS(file.path(ANALYSIS_DIR, "analysis_pre_period_closures.rds"))
cat(sprintf("  pre_period_cls:  %s closures\n",
            format(nrow(pre_period_closures), big.mark = ",")))

cat("All datasets loaded.\n\n")

# ──────────────────────────────────────────────────────────────────────────────
# 1.2  MANDATE VARIABLE VERIFICATION
# ──────────────────────────────────────────────────────────────────────────────
cat("--- Verifying mandate variables from 01_Descriptive_Analysis.R ---\n")

stopifnot("mandate_active"   %in% names(annual_data))
stopifnot("mandate_cohort"   %in% names(annual_data))
stopifnot("spec_A_eligible"  %in% names(annual_data))
stopifnot("spec_B_eligible"  %in% names(annual_data))

cat(sprintf("mandate_active facility-years: %s (%.1f%% of TX pre-1988)\n",
            format(sum(annual_data$mandate_active), big.mark = ","),
            100 * mean(annual_data$mandate_active[annual_data$texas_treated == 1 &
                                                   annual_data$spec_B_eligible == 1],
                       na.rm = TRUE)))

# Verify mandate_window_3yr exists; construct if missing
if (!"mandate_window_3yr" %in% names(annual_data)) {
  cat("  mandate_window_3yr not found — constructing...\n")
  annual_data[, mandate_window_3yr := as.integer(
    state == "TX" & spec_B_eligible == 1 &
      panel_year >= 1988L & panel_year <= 1994L
  )]
  cat(sprintf("  mandate_window_3yr facility-years: %s\n",
              format(sum(annual_data$mandate_window_3yr), big.mark = ",")))
} else {
  cat(sprintf("  mandate_window_3yr facility-years: %s\n",
              format(sum(annual_data$mandate_window_3yr), big.mark = ",")))
}

# ──────────────────────────────────────────────────────────────────────────────
# 1.3  CONSTRUCT SPEC A / SPEC B / POOLED SUBSETS
# ──────────────────────────────────────────────────────────────────────────────
cat("\n--- Constructing Spec A / Spec B / Pooled subsets ---\n")

specA_data   <- annual_data[spec_A_eligible == 1]
specB_data   <- annual_data[spec_B_eligible == 1]
pooled_data  <- annual_data  # alias for clarity

cat(sprintf("  Spec A (post-1988, mandate-free):  %s rows, %s facilities\n",
            format(nrow(specA_data), big.mark = ","),
            format(uniqueN(specA_data$panel_id), big.mark = ",")))
cat(sprintf("    TX: %s | Control: %s\n",
            format(uniqueN(specA_data[texas_treated == 1, panel_id]), big.mark = ","),
            format(uniqueN(specA_data[texas_treated == 0, panel_id]), big.mark = ",")))

cat(sprintf("  Spec B (pre-1988, mandate-exposed): %s rows, %s facilities\n",
            format(nrow(specB_data), big.mark = ","),
            format(uniqueN(specB_data$panel_id), big.mark = ",")))
cat(sprintf("    TX: %s | Control: %s\n",
            format(uniqueN(specB_data[texas_treated == 1, panel_id]), big.mark = ","),
            format(uniqueN(specB_data[texas_treated == 0, panel_id]), big.mark = ",")))

cat(sprintf("  Pooled (all):                       %s rows, %s facilities\n",
            format(nrow(pooled_data), big.mark = ","),
            format(uniqueN(pooled_data$panel_id), big.mark = ",")))

# Report PT validation from metadata if available
if (!is.null(meta$specA_pretrend_p)) {
  cat(sprintf("\n  PT validation from 01: Spec A p = %.4f | Pooled p = %.4f\n",
              meta$specA_pretrend_p, meta$pooled_pretrend_p))
}

cat("\n")

# SECTION 2: HELPER FUNCTIONS
#==============================================================================

#------------------------------------------------------------------------------
# 2.1 save_standard_did_table()
# Saves a list of feols models to .csv, .txt, and .tex
#------------------------------------------------------------------------------
save_standard_did_table <- function(models, headers, base_name, title,
                                     treatment_var, cluster_var = "state",
                                     use_bootstrap = FALSE, n_reps = 999,
                                     digits = 4) {

  stopifnot(length(models) == length(headers))

  results_list <- lapply(seq_along(models), function(i) {
    m   <- models[[i]]
    ct  <- summary(m)$coeftable

    tx_idx <- grep(treatment_var, rownames(ct), fixed = FALSE)
    if (length(tx_idx) == 0) {
      warning(sprintf("[Model %d] Treatment var '%s' not found in coeftable.",
                      i, treatment_var))
      return(data.frame(Model = headers[[i]], Estimate = NA, Std_Error = NA,
                        t_stat = NA, p_value_conv = NA,
                        p_value_webb = NA, p_value_rad = NA,
                        CI_low_webb = NA, CI_high_webb = NA, N_obs = NA))
    }
    row     <- ct[tx_idx[1], , drop = FALSE]
    tx_name <- rownames(ct)[tx_idx[1]]

    p_conv <- round(row[, "Pr(>|t|)"], 4)

    # Wild Cluster Bootstrap (Webb-6 primary, Rademacher cross-check)
    p_webb <- NA_real_; ci_webb_lo <- NA_real_; ci_webb_hi <- NA_real_
    p_rad  <- NA_real_

    if (use_bootstrap) {
      tryCatch({
        boot_webb <- fwildclusterboot::boottest(
          m, param = tx_name, clustid = cluster_var,
          B = n_reps, type = "webb", seed = 20260202
        )
        p_webb     <- round(boot_webb$p_val, 4)
        ci_webb_lo <- round(boot_webb$conf_int[[1]], digits)
        ci_webb_hi <- round(boot_webb$conf_int[[2]], digits)

        boot_rad <- fwildclusterboot::boottest(
          m, param = tx_name, clustid = cluster_var,
          B = n_reps, type = "rademacher", seed = 20260202
        )
        p_rad <- round(boot_rad$p_val, 4)
      }, error = function(e) {
        message(sprintf("  [Bootstrap model %d] Failed: %s", i, e$message))
      })
    }

    data.frame(
      Model        = headers[[i]],
      Estimate     = round(row[, "Estimate"],   digits),
      Std_Error    = round(row[, "Std. Error"], digits),
      t_stat       = round(row[, "t value"],    3),
      p_value_conv = p_conv,
      p_value_webb = p_webb,
      p_value_rad  = p_rad,
      CI_low_webb  = ci_webb_lo,
      CI_high_webb = ci_webb_hi,
      N_obs        = nobs(m),
      stringsAsFactors = FALSE
    )
  })

  results_dt <- rbindlist(results_list, fill = TRUE)

  csv_path <- file.path(OUTPUT_TABLES, paste0(base_name, ".csv"))
  fwrite(results_dt, csv_path)

  txt_path <- file.path(OUTPUT_TABLES, paste0(base_name, ".txt"))
  sink(txt_path)
  cat(title, "\n")
  cat(strrep("=", nchar(title)), "\n\n")
  if (use_bootstrap) {
    cat("Inference: Conventional cluster-robust SE + Wild Cluster Bootstrap\n")
    cat(sprintf("  Bootstrap: B = %d | Webb-6 (primary) + Rademacher (check)\n", n_reps))
    cat(sprintf("  Clusters:  %s (G = 19)\n\n", cluster_var))
  }
  cat("Summary Table:\n")
  print(as.data.frame(results_dt))
  cat("\n")
  for (i in seq_along(models)) {
    cat(sprintf("\n--- Full Output: %s ---\n", headers[[i]]))
    print(summary(models[[i]]))
  }
  sink()

  tex_path <- file.path(OUTPUT_TABLES, paste0(base_name, ".tex"))
  tryCatch(
    etable(models, title = title,
           headers = list("(Specification)" = setNames(
             as.list(rep(1, length(headers))), headers)),
           tex = TRUE, file = tex_path, digits = digits),
    error = function(e) {
      message(sprintf("  [LaTeX] etable failed for %s: %s", base_name, e$message))
    }
  )

  cat(sprintf("  ✓ Saved: %s (.csv / .txt / .tex)\n", base_name))
  invisible(results_dt)
}

#------------------------------------------------------------------------------
# 2.2 save_cox_results()
#------------------------------------------------------------------------------
save_cox_results <- function(models, headers, base_name, title) {

  stopifnot(length(models) == length(headers))

  results_list <- lapply(seq_along(models), function(i) {
    m  <- models[[i]]
    s  <- summary(m)
    ct <- s$coefficients
    ci <- s$conf.int

    data.frame(
      Model    = headers[[i]],
      Variable = rownames(ct),
      Coef     = round(ct[, "coef"],       4),
      HR       = round(ct[, "exp(coef)"],  4),
      SE       = round(ct[, "se(coef)"],   4),
      CI_low   = round(ci[, "lower .95"],  4),
      CI_high  = round(ci[, "upper .95"],  4),
      z        = round(ct[, "z"],          3),
      p_value  = round(ct[, "Pr(>|z|)"],  4),
      stringsAsFactors = FALSE
    )
  })

  results_dt <- rbindlist(results_list)

  csv_path <- file.path(OUTPUT_TABLES, paste0(base_name, ".csv"))
  fwrite(results_dt, csv_path)

  txt_path <- file.path(OUTPUT_TABLES, paste0(base_name, ".txt"))
  sink(txt_path)
  cat(title, "\n")
  cat(strrep("=", nchar(title)), "\n\n")
  cat("Summary Table (Hazard Ratios):\n")
  print(as.data.frame(results_dt))
  cat("\n")
  for (i in seq_along(models)) {
    cat(sprintf("\n--- Full Output: %s ---\n", headers[[i]]))
    print(summary(models[[i]]))
  }
  sink()

  cat(sprintf("  ✓ Saved: %s (.csv / .txt)\n", base_name))
  invisible(results_dt)
}

#------------------------------------------------------------------------------
# 2.3 extract_did() — Extract DiD coefficient from a feols model
#------------------------------------------------------------------------------
extract_did <- function(m, tvar = "did_term") {
  if (is.null(m)) return(list(beta = NA, se = NA, p = NA, n = NA))
  s  <- summary(m, cluster = ~state)
  ct <- coeftable(s)
  idx <- grep(tvar, rownames(ct), fixed = TRUE)[1]
  if (is.na(idx)) return(list(beta = NA, se = NA, p = NA, n = m$nobs))
  list(
    beta = ct[idx, "Estimate"],
    se   = ct[idx, "Std. Error"],
    p    = ct[idx, "Pr(>|t|)"],
    n    = m$nobs
  )
}

#------------------------------------------------------------------------------
# 2.4 stars_fn() — Significance stars for LaTeX
#------------------------------------------------------------------------------
stars_fn <- function(p) {
  if (is.na(p)) return("")
  if (p < 0.01) return("$^{***}$")
  if (p < 0.05) return("$^{**}$")
  if (p < 0.10) return("$^{*}$")
  return("")
}

#------------------------------------------------------------------------------
# 2.5 run_cohort_did() — Pooled|A|B|C|D cohort splits (utility)
# Retained for Model 3A, Model 4 which use cohort_pre1988 splits.
# Primary DiD models (§4–§7) use the Spec A/B/Pooled architecture instead.
#------------------------------------------------------------------------------
run_cohort_did <- function(formula_pooled, data, cohort_var = "cohort_pre1988",
                            treatment_var_name = "did_term",
                            cluster_var = "state",
                            fe_year_var = "panel_year",
                            add_cohort_year_fe = TRUE,
                            run_triple_diff = TRUE) {

  models  <- list()
  headers <- list()

  # --- POOLED ---
  m_pooled <- tryCatch(
    feols(formula_pooled, data = data,
          cluster = as.formula(paste0("~", cluster_var)), lean = FALSE),
    error = function(e) { message("  Pooled failed: ", e$message); NULL }
  )
  models[["Pooled"]]  <- m_pooled
  headers[["Pooled"]] <- "Pooled"

  # --- SPEC A: Post-1988 only ---
  data_A <- data[get(cohort_var) == 0]
  if (nrow(data_A) > 100) {
    m_A <- tryCatch(
      feols(formula_pooled, data = data_A,
            cluster = as.formula(paste0("~", cluster_var)), lean = FALSE),
      error = function(e) { message("  Spec A failed: ", e$message); NULL }
    )
    models[["A"]]  <- m_A
    headers[["A"]] <- "A: Post-1988"
  }

  # --- SPEC B: ≤1988 only ---
  data_B <- data[get(cohort_var) == 1]
  if (nrow(data_B) > 100) {
    m_B <- tryCatch(
      feols(formula_pooled, data = data_B,
            cluster = as.formula(paste0("~", cluster_var)), lean = FALSE),
      error = function(e) { message("  Spec B failed: ", e$message); NULL }
    )
    models[["B"]]  <- m_B
    headers[["B"]] <- "B: Pre-1988"
  }

  # --- SPEC C: Pooled + cohort × year FE ---
  if (add_cohort_year_fe) {
    fml_str <- deparse(formula_pooled, width.cutoff = 500)
    fml_C_str <- sub("\\|\\s*(.+)$",
                      paste0("| \\1 + ", cohort_var, "^", fe_year_var),
                      fml_str)
    fml_C <- as.formula(fml_C_str)
    m_C <- tryCatch(
      feols(fml_C, data = data,
            cluster = as.formula(paste0("~", cluster_var)), lean = FALSE),
      error = function(e) { message("  Spec C failed: ", e$message); NULL }
    )
    models[["C"]]  <- m_C
    headers[["C"]] <- "C: Pooled + Cohort×Year FE"
  }

  # --- SPEC D: Triple-difference ---
  if (run_triple_diff) {
    fml_str <- deparse(formula_pooled, width.cutoff = 500)
    fml_D_str <- sub(treatment_var_name,
                      paste0(treatment_var_name, " + ", treatment_var_name, ":",
                             cohort_var, " + post:", cohort_var),
                      fml_str)
    fml_D <- as.formula(fml_D_str)
    m_D <- tryCatch(
      feols(fml_D, data = data,
            cluster = as.formula(paste0("~", cluster_var)), lean = FALSE),
      error = function(e) { message("  Spec D failed: ", e$message); NULL }
    )
    models[["D"]]  <- m_D
    headers[["D"]] <- "D: Triple-Diff"
  }

  keep <- !sapply(models, is.null)
  list(models = models[keep], headers = headers[keep])
}


#==============================================================================
# SECTION 3: PARALLEL TRENDS VALIDATION (4-TEST BATTERY)
#==============================================================================
# Table B.4: Four pre-trend F-tests establishing the mandate confound and
# validating the Spec A sample as the clean identification strategy.
#==============================================================================

cat("\n========================================\n")
cat("SECTION 3: PARALLEL TRENDS VALIDATION\n")
cat("========================================\n\n")

cat("Four-test battery:\n")
cat("  1. Pooled, no controls     → Expected: REJECT (mandate contamination)\n")
cat("  2. Spec A, no controls     → Expected: NOT reject (clean sample)\n")
cat("  3. Spec B, no controls     → Expected: REJECT (mandate contamination)\n")
cat("  4. Spec B + mandate_active → Expected: Improves toward non-rejection\n\n")

# Use rel_year variable relative to treatment (1999)
# Pre-period window: restrict to years before treatment for F-test
# Use the event study specification, then test joint significance of pre-period coefs

pt_tests <- list()

# Test 1: Pooled, no controls
pt_tests[["pooled_no_ctrl"]] <- tryCatch(
  feols(closure_event ~ i(rel_year_1999, texas_treated, ref = -1) |
          panel_id + panel_year,
        data = annual_data[panel_year >= 1990 & panel_year <= 1997],
        cluster = ~state),
  error = function(e) { message("  PT test 1 failed: ", e$message); NULL }
)

# Test 2: Spec A, no controls (HEADLINE VALIDATION)
pt_tests[["specA_clean"]] <- tryCatch(
  feols(closure_event ~ i(rel_year_1999, texas_treated, ref = -1) |
          panel_id + panel_year,
        data = specA_data[panel_year >= 1990 & panel_year <= 1997],
        cluster = ~state),
  error = function(e) { message("  PT test 2 failed: ", e$message); NULL }
)

# Test 3: Spec B, no controls
pt_tests[["specB_no_ctrl"]] <- tryCatch(
  feols(closure_event ~ i(rel_year_1999, texas_treated, ref = -1) |
          panel_id + panel_year,
        data = specB_data[panel_year >= 1990 & panel_year <= 1997],
        cluster = ~state),
  error = function(e) { message("  PT test 3 failed: ", e$message); NULL }
)

# Test 4: Spec B + mandate_active
pt_tests[["specB_mandate"]] <- tryCatch(
  feols(closure_event ~ i(rel_year_1999, texas_treated, ref = -1) +
          mandate_active | panel_id + panel_year,
        data = specB_data[panel_year >= 1990 & panel_year <= 1997],
        cluster = ~state),
  error = function(e) { message("  PT test 4 failed: ", e$message); NULL }
)

# Extract F-test p-values
pt_results <- rbindlist(lapply(names(pt_tests), function(nm) {
  m <- pt_tests[[nm]]
  if (is.null(m)) return(data.table(spec = nm, f_stat = NA, p_value = NA,
                                     df1 = NA, interpretation = "FAILED"))
  w <- tryCatch(
    fixest::wald(m, "rel_year_1999"),
    error = function(e) NULL
  )
  if (is.null(w)) return(data.table(spec = nm, f_stat = NA, p_value = NA,
                                     df1 = NA, interpretation = "Wald failed"))
  data.table(
    spec           = nm,
    f_stat         = round(w$stat, 3),
    p_value        = round(w$p, 4),
    df1            = w$df1,
    interpretation = fifelse(w$p > 0.10, "NOT rejected", "REJECTED")
  )
}))

cat("\n=== PARALLEL TRENDS VALIDATION TABLE (Table B.4) ===\n")
print(pt_results)

# Save
fwrite(pt_results, file.path(OUTPUT_TABLES, "TableB4_Parallel_Trends_Validation.csv"))

# LaTeX version
tex_pt <- c(
  "\\begin{table}[htbp]",
  "\\centering",
  "\\caption{Parallel Trends Validation: Pre-Period F-Tests}",
  "\\label{tbl:parallel-trends}",
  "\\begin{tabular}{llccl}",
  "\\toprule",
  "Test & Sample & F-statistic & p-value & Result \\\\",
  "\\midrule"
)
for (i in seq_len(nrow(pt_results))) {
  r <- pt_results[i]
  tex_pt <- c(tex_pt, sprintf("%d & %s & %.3f & %.4f & %s \\\\",
                               i, r$spec, r$f_stat, r$p_value, r$interpretation))
}
tex_pt <- c(tex_pt,
  "\\bottomrule",
  "\\multicolumn{5}{p{0.9\\textwidth}}{\\footnotesize \\textit{Notes:}",
  "Joint F-test of pre-treatment event-study coefficients (1990--1997),",
  "reference year = 1998. Spec A restricts to facilities with 100\\% post-1988",
  "tanks (exempt from Texas 30 TAC Ch. 334 phased mandate). Spec B restricts",
  "to 100\\% pre-1988 tanks. Test 4 adds \\texttt{mandate\\_active} to absorb",
  "mandate-induced closure spikes.}",
  "\\end{tabular}",
  "\\end{table}"
)
writeLines(tex_pt, file.path(OUTPUT_TABLES, "TableB4_Parallel_Trends_Validation.tex"))
cat("✓ Saved: TableB4_Parallel_Trends_Validation (.csv / .tex)\n\n")

# Store key p-values for downstream use
specA_pretrend_p  <- pt_results[spec == "specA_clean",    p_value]
pooled_pretrend_p <- pt_results[spec == "pooled_no_ctrl", p_value]

if (!is.na(specA_pretrend_p) && specA_pretrend_p > 0.10) {
  cat("✓ Spec A passes pre-trend validation (p > 0.10) — primary identification confirmed.\n")
} else if (!is.na(specA_pretrend_p)) {
  cat("⚠ Spec A pre-trend p < 0.10 — investigate potential contamination.\n")
}


#==============================================================================
# SECTION 4: TABLE 3 — PRIMARY DiD (SPEC A: POST-1988 COHORT) [HEADLINE]
#==============================================================================
# Spec A: Facilities with 100% post-1988 tanks — never subject to Texas
# phased mandate. This is the clean identification sample.
#
# Columns: Closure(1-2) | Exit|Closure(3-4) | Replace|Closure(5-6)
# (1,3,5) = baseline TWFE; (2,4,6) = + mean_age_1998 control
#==============================================================================

cat("\n========================================\n")
cat("SECTION 4: TABLE 3 — PRIMARY DiD (SPEC A)\n")
cat("========================================\n\n")

cat("Specification: Y_it = αi + λt + β(TX × Post) + εit\n")
cat("Sample: Spec A — facilities with 100% post-1988 tanks\n")
cat("FE: Facility + Year | Cluster: State (19)\n\n")

# ── 4.1 Closure Probability ──
cat("--- 4.1 Closure Probability ---\n")

m_closure_A_base <- feols(
  closure_event ~ did_term | panel_id + panel_year,
  data = specA_data, cluster = ~state, lean = FALSE
)

m_closure_A_ctrl <- feols(
  closure_event ~ did_term + mean_age_1998 | panel_id + panel_year,
  data = specA_data, cluster = ~state, lean = FALSE
)

cat("Closure — Baseline:\n"); print(summary(m_closure_A_base))
cat("\nClosure — With mean_age_1998:\n"); print(summary(m_closure_A_ctrl))

# ── 4.2 Exit | Closure ──
cat("\n--- 4.2 Exit | Closure ---\n")

closure_A <- specA_data[closure_event == 1]
closure_A[, replace_indicator := as.integer(exit_flag == 0)]

cat(sprintf("Spec A facility-years with closures: %s\n",
            format(nrow(closure_A), big.mark = ",")))
cat(sprintf("  Exit rate: %.1f%% | Replace rate: %.1f%%\n",
            100 * mean(closure_A$exit_flag, na.rm = TRUE),
            100 * mean(closure_A$replace_indicator, na.rm = TRUE)))

m_exit_A_base <- tryCatch(
  feols(exit_flag ~ did_term | panel_id + panel_year,
        data = closure_A, cluster = ~state, lean = FALSE),
  error = function(e) { message("  Exit model failed: ", e$message); NULL }
)

m_exit_A_ctrl <- tryCatch(
  feols(exit_flag ~ did_term + mean_age_1998 | panel_id + panel_year,
        data = closure_A, cluster = ~state, lean = FALSE),
  error = function(e) { message("  Exit ctrl model failed: ", e$message); NULL }
)

if (!is.null(m_exit_A_base)) {
  cat("Exit|Closure — Baseline:\n"); print(summary(m_exit_A_base))
}
if (!is.null(m_exit_A_ctrl)) {
  cat("\nExit|Closure — With controls:\n"); print(summary(m_exit_A_ctrl))
}

# ── 4.3 Replace | Closure ──
cat("\n--- 4.3 Replace | Closure ---\n")

m_rep_A_base <- tryCatch(
  feols(replace_indicator ~ did_term | panel_id + panel_year,
        data = closure_A, cluster = ~state, lean = FALSE),
  error = function(e) { message("  Replace model failed: ", e$message); NULL }
)

m_rep_A_ctrl <- tryCatch(
  feols(replace_indicator ~ did_term + mean_age_1998 | panel_id + panel_year,
        data = closure_A, cluster = ~state, lean = FALSE),
  error = function(e) { message("  Replace ctrl model failed: ", e$message); NULL }
)

if (!is.null(m_rep_A_base)) {
  cat("Replace|Closure — Baseline:\n"); print(summary(m_rep_A_base))
}

# ── 4.4 Save Table 3 ──
table3_models <- list(
  m_closure_A_base, m_closure_A_ctrl,
  m_exit_A_base,    m_exit_A_ctrl,
  m_rep_A_base,     m_rep_A_ctrl
)
table3_headers <- c("Closure", "Closure + Age",
                     "Exit|Cls", "Exit|Cls + Age",
                     "Replace|Cls", "Replace|Cls + Age")

# Remove NULLs
keep3 <- !sapply(table3_models, is.null)
save_standard_did_table(
  models        = table3_models[keep3],
  headers       = table3_headers[keep3],
  base_name     = "Table3_Primary_DiD_SpecA",
  title         = "Table 3: Primary DiD — Spec A (Post-1988, Mandate-Free)",
  treatment_var = "did_term",
  cluster_var   = "state",
  use_bootstrap = USE_BOOTSTRAP,
  n_reps        = N_BOOTSTRAP,
  digits        = 6
)

# ── 4.5 EVENT STUDY — Figure 6A (Spec A, Main Text, HEADLINE) ──
cat("\n--- 4.5 Event Study: Spec A (Figure 6A) ---\n")

es_specA <- specA_data[panel_year >= ES_START & panel_year <= ES_END]
rel_min <- ES_START - TREATMENT_YEAR
rel_max <- ES_END   - TREATMENT_YEAR
es_specA[, rel_year_bin := pmax(pmin(rel_year_1999, rel_max), rel_min)]

model_es_specA <- feols(
  closure_event ~ i(rel_year_bin, texas_treated, ref = -1) |
    panel_id + panel_year,
  data = es_specA, cluster = ~state
)

cat("Event Study Spec A summary:\n")
print(summary(model_es_specA))

# Pre-trend F-test on event study
pre_coefs_A <- names(coef(model_es_specA))
pre_coefs_A <- pre_coefs_A[grepl("::-[2-9]|::-1[0-9]", pre_coefs_A)]
pre_pval_A  <- tryCatch(wald(model_es_specA, keep = pre_coefs_A)$p,
                          error = function(e) NA_real_)
cat(sprintf("Spec A Event Study pre-trend F-test: p = %s\n",
            ifelse(is.na(pre_pval_A), "N/A", round(pre_pval_A, 4))))

# Publication figure
png(file.path(OUTPUT_FIGURES, "JMP_Figure_6A_Event_Study_SpecA.png"),
    width = 2400, height = 1440, res = 200)
iplot(model_es_specA,
      main  = "",
      xlab  = "Years Relative to Treatment (1999)",
      ylab  = "Effect on Pr(Tank Closure)",
      col   = "#0072B2")
abline(h = 0, lty = 2, col = "gray40")
abline(v = -0.5, lty = 3, col = "red")
dev.off()
cat("✓ Saved: JMP_Figure_6A_Event_Study_SpecA.png\n")

es_coefs_A_dt <- as.data.table(broom::tidy(model_es_specA, conf.int = TRUE))
fwrite(es_coefs_A_dt, file.path(OUTPUT_TABLES, "Figure6A_ES_Coefficients_SpecA.csv"))

# Economic interpretation
beta_closure_A <- coef(m_closure_A_base)["did_term"]
cat(sprintf("\n--- Economic Interpretation (Spec A) ---\n"))
cat(sprintf("β(closure) = %.6f\n", beta_closure_A))
cat(sprintf("Mean closure rate (Spec A, pre-1999): %.4f\n",
            mean(specA_data[panel_year < TREATMENT_YEAR]$closure_event, na.rm = TRUE)))


#==============================================================================
# SECTION 5: TABLE 4 — SPEC B ROBUSTNESS (MANDATE CONTROLS ESCALATION)
#==============================================================================
# Spec B: Facilities with 100% pre-1988 tanks. These were subject to TX
# phased mandate (30 TAC Ch. 334). Three escalating mandate control levels:
#   (1) + mandate_active dummy
#   (2) + mandate_cohort × panel_year FE
#   (3) mandate_cohort-specific linear trends
#==============================================================================

cat("\n========================================\n")
cat("SECTION 5: TABLE 4 — SPEC B ROBUSTNESS\n")
cat("========================================\n\n")

cat("Sample: Spec B (100% pre-1988 tanks)\n")
cat("Mandate control escalation: active dummy → cohort×year FE → cohort trends\n\n")

# Level 1: + mandate_active dummy
m_specB_1 <- feols(
  closure_event ~ did_term + mandate_active | panel_id + panel_year,
  data = specB_data, cluster = ~state, lean = FALSE
)
cat("--- Spec B Level 1: + mandate_active ---\n")
print(summary(m_specB_1))

# Level 2: + mandate_cohort × panel_year FE
m_specB_2 <- feols(
  closure_event ~ did_term + mandate_active |
    panel_id + panel_year + mandate_cohort^panel_year,
  data = specB_data, cluster = ~state, lean = FALSE
)
cat("\n--- Spec B Level 2: + mandate_cohort^panel_year FE ---\n")
print(summary(m_specB_2))

# Level 3: mandate_cohort-specific trends (most conservative)
m_specB_3 <- tryCatch(
  feols(closure_event ~ did_term |
          panel_id + panel_year + mandate_cohort[panel_year],
        data = specB_data, cluster = ~state, lean = FALSE),
  error = function(e) {
    message("  Spec B Level 3 (cohort trends) failed: ", e$message)
    # Fallback: just use level 2
    NULL
  }
)
if (!is.null(m_specB_3)) {
  cat("\n--- Spec B Level 3: mandate_cohort-specific trends ---\n")
  print(summary(m_specB_3))
}

# Save Table 4
specB_models  <- list(m_specB_1, m_specB_2, m_specB_3)
specB_headers <- c("+ mandate_active",
                    "+ Cohort×Year FE",
                    "Cohort-Specific Trends")
keep_B <- !sapply(specB_models, is.null)

save_standard_did_table(
  models        = specB_models[keep_B],
  headers       = specB_headers[keep_B],
  base_name     = "Table4_SpecB_Mandate_Controls",
  title         = "Table 4: Spec B Robustness — Mandate Control Escalation",
  treatment_var = "did_term",
  cluster_var   = "state",
  use_bootstrap = USE_BOOTSTRAP,
  n_reps        = N_BOOTSTRAP,
  digits        = 6
)

# ── 5.2 EVENT STUDY — Figure B-2 (Spec B + mandate_active, Appendix) ──
cat("\n--- 5.2 Event Study: Spec B + mandate_active (Figure B-2) ---\n")

es_specB <- specB_data[panel_year >= ES_START & panel_year <= ES_END]
es_specB[, rel_year_bin := pmax(pmin(rel_year_1999, rel_max), rel_min)]

model_es_specB <- feols(
  closure_event ~ i(rel_year_bin, texas_treated, ref = -1) + mandate_active |
    panel_id + panel_year,
  data = es_specB, cluster = ~state
)

cat("Event Study Spec B summary:\n")
print(summary(model_es_specB))

# Pre-trend F-test
pre_coefs_B <- names(coef(model_es_specB))
pre_coefs_B <- pre_coefs_B[grepl("::-[2-9]|::-1[0-9]", pre_coefs_B)]
pre_pval_B  <- tryCatch(wald(model_es_specB, keep = pre_coefs_B)$p,
                          error = function(e) NA_real_)
cat(sprintf("Spec B Event Study pre-trend F-test: p = %s\n",
            ifelse(is.na(pre_pval_B), "N/A", round(pre_pval_B, 4))))

png(file.path(OUTPUT_FIGURES, "JMP_Figure_B2_Event_Study_SpecB.png"),
    width = 2400, height = 1440, res = 200)
iplot(model_es_specB,
      main  = "",
      xlab  = "Years Relative to Treatment (1999)",
      ylab  = "Effect on Pr(Tank Closure)",
      col   = "#D55E00")
abline(h = 0, lty = 2, col = "gray40")
abline(v = -0.5, lty = 3, col = "red")
dev.off()
cat("✓ Saved: JMP_Figure_B2_Event_Study_SpecB.png\n")

es_coefs_B_dt <- as.data.table(broom::tidy(model_es_specB, conf.int = TRUE))
fwrite(es_coefs_B_dt, file.path(OUTPUT_TABLES, "FigureB2_ES_Coefficients_SpecB.csv"))

# Cross-spec comparison
cat("\n--- Cross-Spec Comparison (Closure DiD) ---\n")
cross_spec <- data.table(
  Spec = c("A: Post-1988 (headline)", "B: + mandate_active",
           "B: + Cohort×Year FE",
           if (!is.null(m_specB_3)) "B: Cohort-Specific Trends"),
  beta = c(extract_did(m_closure_A_base)$beta,
           extract_did(m_specB_1)$beta,
           extract_did(m_specB_2)$beta,
           if (!is.null(m_specB_3)) extract_did(m_specB_3)$beta),
  se   = c(extract_did(m_closure_A_base)$se,
           extract_did(m_specB_1)$se,
           extract_did(m_specB_2)$se,
           if (!is.null(m_specB_3)) extract_did(m_specB_3)$se),
  p    = c(extract_did(m_closure_A_base)$p,
           extract_did(m_specB_1)$p,
           extract_did(m_specB_2)$p,
           if (!is.null(m_specB_3)) extract_did(m_specB_3)$p)
)
print(cross_spec)
fwrite(cross_spec, file.path(OUTPUT_TABLES, "Cross_Spec_Comparison_Closure.csv"))


#==============================================================================
# SECTION 6: TABLE 5 — POOLED ROBUSTNESS (MANDATE CONTROLS)
#==============================================================================

cat("\n========================================\n")
cat("SECTION 6: TABLE 5 — POOLED ROBUSTNESS\n")
cat("========================================\n\n")

cat("Sample: All facilities | Mandate control escalation\n")
cat("NOTE: Pooled estimate interpreted cautiously — mandate contamination only\n")
cat("partially absorbed even with controls.\n\n")

# Level 1: + mandate_active
m_pooled_1 <- feols(
  closure_event ~ did_term + mandate_active | panel_id + panel_year,
  data = pooled_data, cluster = ~state, lean = FALSE
)
cat("--- Pooled Level 1: + mandate_active ---\n")
print(summary(m_pooled_1))

# Level 2: + mandate_cohort × panel_year FE
m_pooled_2 <- feols(
  closure_event ~ did_term + mandate_active |
    panel_id + panel_year + mandate_cohort^panel_year,
  data = pooled_data, cluster = ~state, lean = FALSE
)
cat("\n--- Pooled Level 2: + mandate_cohort^panel_year FE ---\n")
print(summary(m_pooled_2))

# Level 3: Pooled no controls (diagnostic — shows contamination)
m_pooled_raw <- feols(
  closure_event ~ did_term | panel_id + panel_year,
  data = pooled_data, cluster = ~state, lean = FALSE
)

save_standard_did_table(
  models        = list(m_pooled_raw, m_pooled_1, m_pooled_2),
  headers       = c("No Controls (diagnostic)",
                     "+ mandate_active",
                     "+ Cohort×Year FE"),
  base_name     = "Table5_Pooled_Mandate_Controls",
  title         = "Table 5: Pooled Robustness — Mandate Control Escalation",
  treatment_var = "did_term",
  cluster_var   = "state",
  use_bootstrap = USE_BOOTSTRAP,
  n_reps        = N_BOOTSTRAP,
  digits        = 6
)

# Pooled event study (diagnostic only — not for main text)
cat("\n--- Pooled Event Study (Diagnostic) ---\n")
es_pooled <- pooled_data[panel_year >= ES_START & panel_year <= ES_END]
es_pooled[, rel_year_bin := pmax(pmin(rel_year_1999, rel_max), rel_min)]

model_es_pooled <- feols(
  closure_event ~ i(rel_year_bin, texas_treated, ref = -1) |
    panel_id + panel_year,
  data = es_pooled, cluster = ~state
)

png(file.path(OUTPUT_FIGURES, "Diagnostic_Event_Study_Pooled.png"),
    width = 2400, height = 1440, res = 200)
iplot(model_es_pooled,
      main = "DIAGNOSTIC: Pooled Event Study (Mandate-Contaminated)",
      xlab = "Years Relative to Treatment (1999)",
      ylab = "Effect on Pr(Tank Closure)",
      col  = "gray50")
abline(h = 0, lty = 2, col = "gray40")
abline(v = -0.5, lty = 3, col = "red")
dev.off()
cat("✓ Saved: Diagnostic_Event_Study_Pooled.png\n")


#==============================================================================
# SECTION 7: TABLE 6 — REPORTED LEAK DiD (SPEC A/B/POOLED) + EVENT STUDY
#==============================================================================
# Outcome: leak_year = 1 if facility reported any leak in year t
# Unconditional leak probability — full facility-year panel.
# Distinct from Model 4 (revealed leaks conditional on closure).
#==============================================================================

cat("\n========================================\n")
cat("SECTION 7: TABLE 6 — REPORTED LEAK DiD\n")
cat("========================================\n\n")

cat(sprintf("Baseline leak rate (pre-1999): %.3f%%\n",
            100 * mean(annual_data[panel_year < TREATMENT_YEAR]$leak_year, na.rm = TRUE)))

# ── Spec A (headline) ──
m_leak_A_base <- feols(
  leak_year ~ did_term | panel_id + panel_year,
  data = specA_data, cluster = ~state, lean = FALSE
)

m_leak_A_ctrl <- feols(
  leak_year ~ did_term + mean_age_1998 | panel_id + panel_year,
  data = specA_data, cluster = ~state, lean = FALSE
)

cat("--- Leak: Spec A Baseline ---\n"); print(summary(m_leak_A_base))
cat("\n--- Leak: Spec A + Age ---\n"); print(summary(m_leak_A_ctrl))

# ── Spec B + mandate controls ──
m_leak_B <- feols(
  leak_year ~ did_term + mandate_active | panel_id + panel_year,
  data = specB_data, cluster = ~state, lean = FALSE
)
cat("\n--- Leak: Spec B + mandate_active ---\n"); print(summary(m_leak_B))

# ── Pooled + mandate controls ──
m_leak_P <- feols(
  leak_year ~ did_term + mandate_active | panel_id + panel_year,
  data = pooled_data, cluster = ~state, lean = FALSE
)
cat("\n--- Leak: Pooled + mandate_active ---\n"); print(summary(m_leak_P))

save_standard_did_table(
  models        = list(m_leak_A_base, m_leak_A_ctrl, m_leak_B, m_leak_P),
  headers       = c("Spec A", "Spec A + Age",
                     "Spec B + Mandate", "Pooled + Mandate"),
  base_name     = "Table6_Reported_Leak",
  title         = "Table 6: Reported Leak Probability (Spec A/B/Pooled)",
  treatment_var = "did_term",
  cluster_var   = "state",
  use_bootstrap = USE_BOOTSTRAP,
  n_reps        = N_BOOTSTRAP
)

# ── Leak Event Study (Spec A, Figure 7) ──
cat("\n--- Leak Event Study (Spec A, Figure 7) ---\n")

es_leak_A <- specA_data[panel_year >= ES_START & panel_year <= ES_END]
es_leak_A[, rel_year_bin := pmax(pmin(rel_year_1999, rel_max), rel_min)]

model_es_leak_A <- feols(
  leak_year ~ i(rel_year_bin, texas_treated, ref = -1) |
    panel_id + panel_year,
  data = es_leak_A, cluster = ~state
)

pre_coefs_leak <- names(coef(model_es_leak_A))
pre_coefs_leak <- pre_coefs_leak[grepl("::-[2-9]|::-1[0-9]", pre_coefs_leak)]
pre_pval_leak  <- tryCatch(wald(model_es_leak_A, keep = pre_coefs_leak)$p,
                             error = function(e) NA_real_)
cat(sprintf("Leak ES pre-trend F-test (Spec A): p = %s\n",
            ifelse(is.na(pre_pval_leak), "N/A", round(pre_pval_leak, 4))))

png(file.path(OUTPUT_FIGURES, "JMP_Figure_7_Leak_Event_Study.png"),
    width = 2400, height = 1440, res = 200)
iplot(model_es_leak_A,
      main = "",
      xlab = "Years Relative to Treatment (1999)",
      ylab = "Effect on Pr(Reported Leak)",
      col  = "#D55E00")
abline(h = 0, lty = 2, col = "gray40")
abline(v = -0.5, lty = 3, col = "red")
dev.off()
cat("✓ Saved: JMP_Figure_7_Leak_Event_Study.png\n")

es_leak_dt <- as.data.table(broom::tidy(model_es_leak_A, conf.int = TRUE))
fwrite(es_leak_dt, file.path(OUTPUT_TABLES, "Figure7_Leak_ES_Coefficients.csv"))


#==============================================================================
# SECTION 8: HTE — WALL TYPE × TREATMENT BY SPEC
#==============================================================================
# Tests mechanism: if private insurance prices single-walled tanks more
# heavily, the closure response should be larger for high-pct_single_wall
# facilities.
#==============================================================================

cat("\n========================================\n")
cat("SECTION 8: HTE — WALL TYPE × TREATMENT\n")
cat("========================================\n\n")

# Determine wall-type column name (panel builder may use different names)
wall_col <- NULL
for (candidate in c("pct_single_wall", "has_single_walled", "any_single_walled")) {
  if (candidate %in% names(annual_data)) {
    wall_col <- candidate
    break
  }
}

if (!is.null(wall_col)) {
  cat(sprintf("Using wall-type variable: %s\n", wall_col))

  # Spec A: Wall type × treatment
  fml_hte_A <- as.formula(sprintf(
    "closure_event ~ did_term + did_term:%s + %s | panel_id + panel_year",
    wall_col, wall_col
  ))
  m_hte_wall_A <- feols(fml_hte_A, data = specA_data, cluster = ~state, lean = FALSE)
  cat("\n--- HTE Wall Type: Spec A ---\n"); print(summary(m_hte_wall_A))

  # Spec B: Wall type × treatment + mandate control
  fml_hte_B <- as.formula(sprintf(
    "closure_event ~ did_term + did_term:%s + %s + mandate_active | panel_id + panel_year",
    wall_col, wall_col
  ))
  m_hte_wall_B <- feols(fml_hte_B, data = specB_data, cluster = ~state, lean = FALSE)
  cat("\n--- HTE Wall Type: Spec B + mandate_active ---\n"); print(summary(m_hte_wall_B))

  save_standard_did_table(
    models        = list(m_hte_wall_A, m_hte_wall_B),
    headers       = c("Spec A: Wall×Treatment",
                       "Spec B: Wall×Treatment + Mandate"),
    base_name     = "HTE_Wall_Type_by_Spec",
    title         = "HTE: Wall Type × Treatment Effect by Specification",
    treatment_var = "did_term",
    cluster_var   = "state",
    use_bootstrap = USE_BOOTSTRAP,
    n_reps        = N_BOOTSTRAP
  )
} else {
  cat("⚠ No wall-type variable found in annual_data — skipping HTE.\n")
  cat("  Expected: pct_single_wall, has_single_walled, or any_single_walled\n")
}


#==============================================================================
# SECTION 9: MODEL 1B — FACILITY-LEVEL COX SURVIVAL
#==============================================================================

cat("\n========================================\n")
cat("SECTION 9: MODEL 1B — FACILITY SURVIVAL\n")
cat("========================================\n\n")

cat("Specification: h_i(t) = h_0(t) × exp(β × TX_i)\n")
cat("Outcome: Time from 1999 to first post-1999 tank closure\n\n")

# Build facility-level survival dataset from post-1999 panel
facility_surv <- annual_data[, .(
  texas   = first(texas_treated),
  state   = first(state),
  n_tanks = first(active_tanks_dec[panel_year == min(panel_year[panel_year >= 1999],
                                                     na.rm = TRUE)]),
  first_closure_post = fifelse(
    any(closure_event == 1 & panel_year >= TREATMENT_YEAR, na.rm = TRUE),
    min(panel_year[closure_event == 1 & panel_year >= TREATMENT_YEAR], na.rm = TRUE),
    NA_integer_
  ),
  last_obs = max(panel_year)
), by = panel_id]

facility_surv[, `:=`(
  surv_time = fifelse(
    !is.na(first_closure_post),
    first_closure_post - TREATMENT_YEAR,
    pmin(last_obs, PANEL_END) - TREATMENT_YEAR
  ),
  surv_event = as.integer(!is.na(first_closure_post))
)]

facility_surv <- facility_surv[!is.na(surv_time) & surv_time >= 0]

cat(sprintf("Facilities in survival: %s | Events: %s (%.1f%%)\n",
            format(nrow(facility_surv), big.mark = ","),
            format(sum(facility_surv$surv_event), big.mark = ","),
            100 * mean(facility_surv$surv_event)))

surv_obj_fac <- Surv(facility_surv$surv_time, facility_surv$surv_event)

model_1b <- coxph(surv_obj_fac ~ texas, data = facility_surv, cluster = state)
model_1b_ctrl <- coxph(surv_obj_fac ~ texas + log(n_tanks + 1),
                        data = facility_surv, cluster = state)

cat("--- Model 1B: Baseline ---\n"); print(summary(model_1b))
cat("\n--- Model 1B: With Controls ---\n"); print(summary(model_1b_ctrl))

hr_1b <- exp(coef(model_1b)["texas"])
cat(sprintf("\nHR (Texas): %.4f → %.0f%% %s likely to close\n",
            hr_1b, abs(hr_1b - 1) * 100,
            ifelse(hr_1b > 1, "more", "less")))

save_cox_results(
  models    = list(model_1b, model_1b_ctrl),
  headers   = c("Baseline", "With Controls"),
  base_name = "Model_1B_Facility_Survival",
  title     = "Model 1B: Time-to-First-Closure (Facility Cox)"
)


#==============================================================================
# SECTION 10: MODEL 2 — TANK-LEVEL COX HTE
#==============================================================================

cat("\n========================================\n")
cat("SECTION 10: MODEL 2 — TANK-LEVEL COX HTE\n")
cat("========================================\n\n")

tank_surv_data <- tank_inventory[!is.na(time_to_close) & time_to_close >= 0 &
                                  !is.na(age_at_treatment)]
cat(sprintf("Tanks in HTE analysis: %s\n", format(nrow(tank_surv_data), big.mark = ",")))

tank_surv_obj <- Surv(tank_surv_data$time_to_close, tank_surv_data$event_closure)

# ── SPEC A: Simple interactions ──
model_2a <- coxph(tank_surv_obj ~ texas, data = tank_surv_data, cluster = state)
model_2b_sw <- coxph(tank_surv_obj ~ texas + texas:single_walled + single_walled,
                      data = tank_surv_data, cluster = state)
model_2c_full <- coxph(
  tank_surv_obj ~ texas + texas:single_walled + texas:old_at_treatment +
    texas:old_vintage + single_walled + old_at_treatment + old_vintage,
  data = tank_surv_data, cluster = state
)

cat("Model 2A (Main):\n"); print(summary(model_2a))
cat("\nModel 2B (× Single-walled):\n"); print(summary(model_2b_sw))
cat("\nModel 2C (Full interactions):\n"); print(summary(model_2c_full))

save_cox_results(
  models    = list(model_2a, model_2b_sw, model_2c_full),
  headers   = c("Main Effect", "× Single-Walled", "Full Interactions"),
  base_name = "Model_2_Tank_HTE_Simple",
  title     = "Model 2: Tank-Level HTE — Simple Interactions"
)

# ── SPEC B: County-stratified Cox ──
tank_surv_strat <- tank_surv_data[!is.na(county_fips) & county_fips != ""]
tank_surv_obj_s <- Surv(tank_surv_strat$time_to_close, tank_surv_strat$event_closure)

cat(sprintf("\nTanks with county FIPS: %s\n",
            format(nrow(tank_surv_strat), big.mark = ",")))

model_2a_strat <- coxph(tank_surv_obj_s ~ texas + strata(county_fips),
                          data = tank_surv_strat, cluster = state)
model_2b_strat <- coxph(tank_surv_obj_s ~ texas * old_at_treatment + strata(county_fips),
                          data = tank_surv_strat, cluster = state)
model_2c_strat <- coxph(tank_surv_obj_s ~ texas * single_walled + strata(county_fips),
                          data = tank_surv_strat, cluster = state)
model_2d_strat <- coxph(tank_surv_obj_s ~ texas * vintage + strata(county_fips),
                          data = tank_surv_strat, cluster = state)

cat("County-strat Main:\n"); print(summary(model_2a_strat))
cat("\n× Age>20:\n"); print(summary(model_2b_strat))
cat("\n× Single-walled:\n"); print(summary(model_2c_strat))

save_cox_results(
  models    = list(model_2a_strat, model_2b_strat, model_2c_strat, model_2d_strat),
  headers   = c("Main Effect", "× Age>20", "× Single-Walled", "× Vintage"),
  base_name = "Model_2_Tank_HTE_County_Strata",
  title     = "Model 2: Tank-Level HTE — County-Stratified Cox"
)


#==============================================================================
# SECTION 11: MODEL 3A — AGE AT CLOSURE (COUNTY FE) + EVENT STUDY
#==============================================================================

cat("\n========================================\n")
cat("SECTION 11: MODEL 3A — AGE AT CLOSURE\n")
cat("========================================\n\n")

model_3a_data <- closed_tanks[!is.na(age_at_closure) & !is.na(county_fips_fac)]
cat(sprintf("Closed tanks with valid age & county: %s\n",
            format(nrow(model_3a_data), big.mark = ",")))

model_3a <- feols(
  age_at_closure ~ texas_post | county_fips_fac + closure_year,
  data = model_3a_data, cluster = ~state
)
cat("--- Model 3A: Baseline ---\n"); print(summary(model_3a))

has_capacity <- "capacity" %in% names(model_3a_data)
if (has_capacity) {
  model_3a_ctrl <- feols(
    age_at_closure ~ texas_post + single_walled + log(capacity + 1) |
      county_fips_fac + closure_year,
    data = model_3a_data, cluster = ~state
  )
  cat("\n--- Model 3A: With Controls ---\n"); print(summary(model_3a_ctrl))
  models_3a  <- list(model_3a, model_3a_ctrl)
  headers_3a <- c("Baseline", "With Controls")
} else {
  models_3a  <- list(model_3a)
  headers_3a <- c("Baseline")
}

save_standard_did_table(
  models = models_3a, headers = headers_3a,
  base_name = "Model_3A_Age_at_Closure", title = "Model 3A: Age at Closure (County FE)",
  treatment_var = "texas_post", cluster_var = "state",
  use_bootstrap = USE_BOOTSTRAP, n_reps = N_BOOTSTRAP
)

# Cohort splits (existing Pooled|A|B|C|D approach — retained for this model)
cat("\n--- Model 3A: Cohort Splits ---\n")
cohort_3a <- run_cohort_did(
  formula_pooled = age_at_closure ~ texas_post | county_fips_fac + closure_year,
  data = model_3a_data, cohort_var = "cohort_pre1988",
  treatment_var_name = "texas_post", fe_year_var = "closure_year"
)
for (nm in names(cohort_3a$models)) {
  cat(sprintf("--- [%s] ---\n", cohort_3a$headers[[nm]]))
  print(summary(cohort_3a$models[[nm]])); cat("\n")
}

save_standard_did_table(
  models = cohort_3a$models, headers = unlist(cohort_3a$headers),
  base_name = "Model_3A_Age_at_Closure_Cohort_Splits",
  title = "Model 3A: Age at Closure — Cohort Splits",
  treatment_var = "texas_post", cluster_var = "state",
  use_bootstrap = USE_BOOTSTRAP, n_reps = N_BOOTSTRAP
)

# Event study
cat("\n--- Model 3A Event Study ---\n")
model_3a_data[, `:=`(
  event_time   = closure_year - TREATMENT_YEAR,
  event_time_b = closure_year - TREATMENT_YEAR
)]
et_min_3a <- ES_START - TREATMENT_YEAR
et_max_3a <- ES_END   - TREATMENT_YEAR
model_3a_data[event_time_b < et_min_3a, event_time_b := et_min_3a]
model_3a_data[event_time_b > et_max_3a, event_time_b := et_max_3a]

model_3a_es <- feols(
  age_at_closure ~ i(event_time_b, texas, ref = -1) |
    county_fips_fac + closure_year,
  data = model_3a_data[closure_year >= ES_START & closure_year <= ES_END],
  cluster = ~state
)

png(file.path(OUTPUT_FIGURES, "Model_3A_ES_Age_at_Closure.png"),
    width = 2400, height = 1440, res = 200)
iplot(model_3a_es, main = "Age at Closure Event Study",
      xlab = "Years Relative to Treatment (1999)",
      ylab = "Differential Age (years)", col = "#D55E00")
abline(h = 0, lty = 2, col = "gray40")
dev.off()
cat("✓ Saved: Model_3A_ES_Age_at_Closure.png\n")

fwrite(as.data.table(broom::tidy(model_3a_es, conf.int = TRUE)),
       file.path(OUTPUT_TABLES, "Model_3A_ES_Coefficients.csv"))


#==============================================================================
# SECTION 12: MODEL 3B — AGE AT CLOSURE (FACILITY FE, SPANNING)
#==============================================================================

cat("\n========================================\n")
cat("SECTION 12: MODEL 3B — WITHIN-FACILITY\n")
cat("========================================\n\n")

span_counts <- closed_tanks[, .(
  n_pre  = sum(closure_year < TREATMENT_YEAR),
  n_post = sum(closure_year >= TREATMENT_YEAR)
), by = panel_id]

spanning_ids  <- span_counts[n_pre >= 1 & n_post >= 1, panel_id]
model_3b_data <- closed_tanks[panel_id %in% spanning_ids & !is.na(age_at_closure)]

cat(sprintf("Spanning facilities: %s (%.1f%% of all)\n",
            format(length(spanning_ids), big.mark = ","),
            100 * length(spanning_ids) / uniqueN(closed_tanks$panel_id)))

if (nrow(model_3b_data) > 0) {
  model_3b <- feols(age_at_closure ~ post | panel_id + closure_year,
                     data = model_3b_data, cluster = ~state)
  model_3b_did <- feols(age_at_closure ~ texas_post + post | panel_id + closure_year,
                         data = model_3b_data, cluster = ~state)

  cat("--- Post Effect (facility FE) ---\n"); print(summary(model_3b))
  cat("\n--- TX×Post (DiD within spanning) ---\n"); print(summary(model_3b_did))

  save_standard_did_table(
    models = list(model_3b), headers = c("Post Only"),
    base_name = "Model_3B_Post_Only", title = "Model 3B: Post Effect (Facility FE)",
    treatment_var = "post", cluster_var = "state",
    use_bootstrap = USE_BOOTSTRAP, n_reps = N_BOOTSTRAP
  )
  save_standard_did_table(
    models = list(model_3b_did), headers = c("TX×Post"),
    base_name = "Model_3B_TXxPost", title = "Model 3B: TX×Post (Facility FE)",
    treatment_var = "texas_post", cluster_var = "state",
    use_bootstrap = USE_BOOTSTRAP, n_reps = N_BOOTSTRAP
  )
} else {
  cat("⚠ No spanning facilities found — Model 3B skipped.\n")
}


#==============================================================================
# SECTION 13: MODEL 4 — REVEALED LEAKS AT CLOSURE
#==============================================================================

cat("\n========================================\n")
cat("SECTION 13: MODEL 4 — REVEALED LEAKS\n")
cat("========================================\n\n")

cat("Leak window definitions:\n")
cat("  Primary: tank_closure_revealed       (0-60d)\n")
cat("  Narrow:  tank_closure_revealed_narrow (0-30d)\n")
cat("  Wide:    tank_closure_revealed_wide   (0-90d)\n")
cat("  Reg:     tank_closure_revealed_reg    (0-45d)\n\n")

model_4_data <- annual_data[n_closures > 0 &
                              !is.na(tank_closure_revealed) &
                              !is.na(county_fips_fac)]

model_4_data[, `:=`(
  revealed_primary = as.integer(tank_closure_revealed       > 0),
  revealed_narrow  = as.integer(tank_closure_revealed_narrow > 0),
  revealed_wide    = as.integer(tank_closure_revealed_wide   > 0),
  revealed_reg     = as.integer(tank_closure_revealed_reg    > 0)
)]

cat(sprintf("Facility-years with closures: %s\n",
            format(nrow(model_4_data), big.mark = ",")))
cat(sprintf("  Revealed leak rate (primary): %.1f%%\n",
            100 * mean(model_4_data$revealed_primary, na.rm = TRUE)))

# LPM across windows
m4_primary <- feols(revealed_primary ~ did_term | county_fips_fac + panel_year,
                     data = model_4_data, cluster = ~state)
m4_narrow  <- feols(revealed_narrow  ~ did_term | county_fips_fac + panel_year,
                     data = model_4_data, cluster = ~state)
m4_wide    <- feols(revealed_wide    ~ did_term | county_fips_fac + panel_year,
                     data = model_4_data, cluster = ~state)
m4_reg     <- feols(revealed_reg     ~ did_term | county_fips_fac + panel_year,
                     data = model_4_data, cluster = ~state)

cat("Primary (0-60d):\n"); print(summary(m4_primary))

save_standard_did_table(
  models  = list(m4_primary, m4_narrow, m4_wide, m4_reg),
  headers = c("0-60d", "0-30d", "0-90d", "0-45d"),
  base_name = "Model_4_Revealed_Leaks_LPM",
  title = "Model 4: Revealed Leaks at Closure (LPM)",
  treatment_var = "did_term", cluster_var = "state",
  use_bootstrap = USE_BOOTSTRAP, n_reps = N_BOOTSTRAP
)

# Logit robustness
m4_logit_primary <- feols(revealed_primary ~ did_term, data = model_4_data,
                           cluster = ~state, family = "logit")
m4_logit_narrow  <- feols(revealed_narrow ~ did_term, data = model_4_data,
                           cluster = ~state, family = "logit")
m4_logit_wide    <- feols(revealed_wide ~ did_term, data = model_4_data,
                           cluster = ~state, family = "logit")
m4_logit_reg     <- feols(revealed_reg ~ did_term, data = model_4_data,
                           cluster = ~state, family = "logit")

save_standard_did_table(
  models  = list(m4_logit_primary, m4_logit_narrow, m4_logit_wide, m4_logit_reg),
  headers = c("0-60d", "0-30d", "0-90d", "0-45d"),
  base_name = "Model_4_Revealed_Leaks_Logit",
  title = "Model 4 (Robustness): Revealed Leaks — Logit",
  treatment_var = "did_term", cluster_var = "state",
  use_bootstrap = USE_BOOTSTRAP, n_reps = N_BOOTSTRAP
)

# Cohort splits (primary window only)
cat("\n--- Model 4: Cohort Splits ---\n")
cohort_4 <- run_cohort_did(
  formula_pooled = revealed_primary ~ did_term | county_fips_fac + panel_year,
  data = model_4_data, cohort_var = "cohort_pre1988",
  treatment_var_name = "did_term"
)
for (nm in names(cohort_4$models)) {
  cat(sprintf("[%s] ", cohort_4$headers[[nm]]))
  d <- extract_did(cohort_4$models[[nm]]); cat(sprintf("β=%.4f p=%.3f\n", d$beta, d$p))
}

save_standard_did_table(
  models = cohort_4$models, headers = unlist(cohort_4$headers),
  base_name = "Model_4_Revealed_Leaks_Cohort_Splits",
  title = "Model 4: Revealed Leaks — Cohort Splits",
  treatment_var = "did_term", cluster_var = "state",
  use_bootstrap = USE_BOOTSTRAP, n_reps = N_BOOTSTRAP
)


#==============================================================================
# SECTION 14: MODELS 5A/5B — COMPETING RISKS
#==============================================================================

cat("\n========================================\n")
cat("SECTION 14: MODELS 5A/5B — COMPETING RISKS\n")
cat("========================================\n\n")

cat("Competing events:\n")
cat("  Event 1: First-ever operational leak\n")
cat("  Event 2: First tank closure (competing)\n")
cat("  Event 0: Neither by study end (censored)\n\n")

compete_data <- annual_data[, .(
  texas          = first(texas_treated),
  state          = first(state),
  yr_first_leak  = first(na.omit(year_of_first_leak)),
  yr_first_close = fifelse(
    any(closure_event == 1, na.rm = TRUE),
    min(panel_year[closure_event == 1], na.rm = TRUE),
    NA_integer_
  ),
  last_obs = max(panel_year)
), by = panel_id]

compete_data[, event_type := fcase(
  !is.na(yr_first_leak) &
    (is.na(yr_first_close) | yr_first_leak <= yr_first_close), 1L,
  !is.na(yr_first_close) &
    (is.na(yr_first_leak)  | yr_first_close < yr_first_leak),  2L,
  default = 0L
)]

compete_data[, event_time := fcase(
  event_type == 1L, pmax(yr_first_leak  - TREATMENT_YEAR, 0),
  event_type == 2L, pmax(yr_first_close - TREATMENT_YEAR, 0),
  default = pmin(last_obs, PANEL_END) - TREATMENT_YEAR
)]
compete_data <- compete_data[!is.na(event_time) & event_time >= 0]

cat(sprintf("Facilities: %s | Leaked first: %s (%.1f%%) | Closed first: %s (%.1f%%)\n",
            format(nrow(compete_data), big.mark = ","),
            format(sum(compete_data$event_type == 1), big.mark = ","),
            100 * mean(compete_data$event_type == 1),
            format(sum(compete_data$event_type == 2), big.mark = ","),
            100 * mean(compete_data$event_type == 2)))

# 5A: Cause-Specific Cox
surv_5a_leak  <- Surv(compete_data$event_time, as.integer(compete_data$event_type == 1))
surv_5a_close <- Surv(compete_data$event_time, as.integer(compete_data$event_type == 2))

model_5a_leak  <- coxph(surv_5a_leak  ~ texas, data = compete_data, cluster = state)
model_5a_close <- coxph(surv_5a_close ~ texas, data = compete_data, cluster = state)

cat("CS-Cox (Leak):\n"); print(summary(model_5a_leak))
cat("\nCS-Cox (Closure):\n"); print(summary(model_5a_close))

save_cox_results(
  models  = list(model_5a_leak, model_5a_close),
  headers = c("Event: First Leak", "Event: First Closure"),
  base_name = "Model_5A_Cause_Specific_Cox",
  title = "Model 5A: Cause-Specific Cox"
)

# 5B: Fine-Gray
tryCatch({
  fg_data <- finegray(
    Surv(event_time, factor(event_type, levels = c(0, 1, 2))) ~ .,
    data = compete_data[, .(event_time, event_type, texas, state)]
  )
  model_5b <- coxph(Surv(fgstart, fgstop, fgstatus) ~ texas,
                     data = fg_data, weight = fgwt, cluster = state)
  cat("\nFine-Gray (subdistribution for leak):\n"); print(summary(model_5b))
  save_cox_results(
    models = list(model_5b), headers = c("Fine-Gray (Leak CIF)"),
    base_name = "Model_5B_Fine_Gray", title = "Model 5B: Fine-Gray"
  )
}, error = function(e) {
  cat(sprintf("⚠ Model 5B failed: %s\n", e$message))
})


#==============================================================================
# SECTION 15: APPENDIX ROBUSTNESS
#==============================================================================

cat("\n========================================\n")
cat("SECTION 15: APPENDIX ROBUSTNESS\n")
cat("========================================\n\n")

# ─────────────────────────────────────────────────────────────────────────────
# 15.1  MD-Excluded Robustness (Table B.5)
# MD never operated a state FA fund (ASTSWMO) — may not be valid control.
# ─────────────────────────────────────────────────────────────────────────────
cat("--- 15.1 MD-Excluded Robustness ---\n")

m_noMD_A <- feols(
  closure_event ~ did_term | panel_id + panel_year,
  data = specA_data[state != "MD"], cluster = ~state, lean = FALSE
)
m_noMD_B <- feols(
  closure_event ~ did_term + mandate_active | panel_id + panel_year,
  data = specB_data[state != "MD"], cluster = ~state, lean = FALSE
)

cat("Spec A (MD excluded):\n"); print(summary(m_noMD_A))
cat("\nSpec B + mandate (MD excluded):\n"); print(summary(m_noMD_B))

save_standard_did_table(
  models  = list(m_noMD_A, m_noMD_B),
  headers = c("Spec A (no MD)", "Spec B + Mandate (no MD)"),
  base_name = "TableB5_MD_Excluded",
  title = "Table B.5: MD-Excluded Robustness",
  treatment_var = "did_term", cluster_var = "state",
  use_bootstrap = USE_BOOTSTRAP, n_reps = N_BOOTSTRAP
)

# ─────────────────────────────────────────────────────────────────────────────
# 15.2  TN-Excluded from Leak Models (Table B.5b)
# TN has zero LUST records pre-2008. Restrict TN to ≥2008 in leak models.
# ─────────────────────────────────────────────────────────────────────────────
cat("\n--- 15.2 TN Leak Robustness ---\n")

tn_exclude <- if (!is.null(meta$tn_exclude_leak_models)) meta$tn_exclude_leak_models else TRUE

if (tn_exclude) {
  cat("TN flagged: excluding from leak-outcome models.\n")
  leak_noTN_data <- specA_data[state != "TN"]

  m_leak_noTN <- feols(
    leak_year ~ did_term | panel_id + panel_year,
    data = leak_noTN_data, cluster = ~state, lean = FALSE
  )
  cat("Leak Spec A (TN excluded):\n"); print(summary(m_leak_noTN))

  save_standard_did_table(
    models  = list(m_leak_A_base, m_leak_noTN),
    headers = c("Spec A (full)", "Spec A (no TN)"),
    base_name = "TableB5b_TN_Leak_Robustness",
    title = "Table B.5b: TN-Excluded Leak Robustness",
    treatment_var = "did_term", cluster_var = "state",
    use_bootstrap = USE_BOOTSTRAP, n_reps = N_BOOTSTRAP
  )
} else {
  cat("TN not flagged for exclusion — skipping.\n")
}

# ─────────────────────────────────────────────────────────────────────────────
# 15.3  mandate_window_3yr Sensitivity (Table B.6)
# Uses 3-year window (1988–1994) instead of point-in-time mandate_active.
# More conservative — absorbs anticipatory and aftermath effects.
# ─────────────────────────────────────────────────────────────────────────────
cat("\n--- 15.3 mandate_window_3yr Sensitivity ---\n")

m_specB_3yr <- feols(
  closure_event ~ did_term + mandate_window_3yr | panel_id + panel_year,
  data = specB_data, cluster = ~state, lean = FALSE
)
cat("Spec B + mandate_window_3yr:\n"); print(summary(m_specB_3yr))

m_pooled_3yr <- feols(
  closure_event ~ did_term + mandate_window_3yr | panel_id + panel_year,
  data = pooled_data, cluster = ~state, lean = FALSE
)
cat("\nPooled + mandate_window_3yr:\n"); print(summary(m_pooled_3yr))

save_standard_did_table(
  models  = list(m_specB_1, m_specB_3yr, m_pooled_1, m_pooled_3yr),
  headers = c("Spec B + active", "Spec B + 3yr window",
               "Pooled + active", "Pooled + 3yr window"),
  base_name = "TableB6_Mandate_Window_Sensitivity",
  title = "Table B.6: mandate_window_3yr Sensitivity",
  treatment_var = "did_term", cluster_var = "state",
  use_bootstrap = USE_BOOTSTRAP, n_reps = N_BOOTSTRAP
)

# ─────────────────────────────────────────────────────────────────────────────
# 15.4  Callaway-Sant'Anna Robustness Stub (Table B.7)
# For Spec B: treat mandate_year as staggered treatment adoption.
# Requires `did` package. Left as stub — uncomment for final run.
# ─────────────────────────────────────────────────────────────────────────────
cat("\n--- 15.4 Callaway-Sant'Anna Stub (Spec B) ---\n")

# ## Uncomment for CS estimation:
# if (requireNamespace("did", quietly = TRUE)) {
#   library(did)
#
#   # Merge facility-level mandate year from tanks_1999
#   specB_cs <- merge(
#     specB_data,
#     tanks_1999[, .(panel_id, tx_mandate_deadline)],
#     by = "panel_id", all.x = TRUE
#   )
#   specB_cs[, g_mandate := fifelse(
#     texas_treated == 1 & !is.na(tx_mandate_deadline),
#     as.integer(year(tx_mandate_deadline)),
#     0L  # never-treated
#   )]
#
#   cs_out <- att_gt(
#     yname  = "closure_event",
#     tname  = "panel_year",
#     idname = "panel_id",
#     gname  = "g_mandate",
#     data   = as.data.frame(specB_cs),
#     control_group = "nevertreated"
#   )
#   cs_agg <- aggte(cs_out, type = "simple")
#   cat("CS aggregate ATT:\n"); print(cs_agg)
# }

cat("CS stub implemented — uncomment and install `did` package for execution.\n")

# ─────────────────────────────────────────────────────────────────────────────
# 15.5  Regulatory Vintage Controls (existing robustness)
# ─────────────────────────────────────────────────────────────────────────────
cat("\n--- 15.5 Regulatory Vintage Controls ---\n")

if ("reg_vintage" %in% names(annual_data)) {
  annual_data[, reg_vintage := factor(reg_vintage,
                                       levels = c("Post-Deadline", "Transition", "Pre-RCRA"))]

  m_rob_vintage <- feols(
    closure_event ~ did_term + pre1998_install:post_1999 | panel_id + panel_year,
    data = annual_data, cluster = ~state
  )
  cat("Pooled + pre1998_install×post:\n"); print(summary(m_rob_vintage))

  save_standard_did_table(
    models  = list(m_pooled_raw, m_rob_vintage),
    headers = c("Pooled Baseline", "+ Vintage Control"),
    base_name = "Robustness_Regulatory_Vintage",
    title = "Robustness: Regulatory Vintage Controls",
    treatment_var = "did_term", cluster_var = "state",
    use_bootstrap = USE_BOOTSTRAP, n_reps = N_BOOTSTRAP
  )
} else {
  cat("reg_vintage not found — skipping.\n")
}

# Phased mandate exposure (if available)
if ("mandate_exposure" %in% names(annual_data)) {
  m_rob_exposure <- feols(
    closure_event ~ did_term + mandate_x_post | panel_id + panel_year,
    data = annual_data, cluster = ~state
  )
  cat("\n+ mandate_exposure×post:\n"); print(summary(m_rob_exposure))

  save_standard_did_table(
    models = list(m_pooled_raw, m_rob_exposure),
    headers = c("Pooled Baseline", "+ Phased Mandate Exposure"),
    base_name = "Robustness_Phased_Mandate",
    title = "Robustness: Phased Mandate Exposure",
    treatment_var = "did_term", cluster_var = "state",
    use_bootstrap = USE_BOOTSTRAP, n_reps = N_BOOTSTRAP
  )
}

# ─────────────────────────────────────────────────────────────────────────────
# 15.6  Cohort Splits Summary (cross-model comparison)
# ─────────────────────────────────────────────────────────────────────────────
cat("\n--- 15.6 Cross-Model Spec Comparison ---\n")

spec_summary <- data.table(
  Model = c("Closure (Spec A)", "Closure (Spec B + mandate)",
            "Closure (Pooled + mandate)", "Closure (Pooled raw)",
            "Leak (Spec A)", "Leak (Spec B + mandate)"),
  beta  = c(extract_did(m_closure_A_base)$beta,
            extract_did(m_specB_1)$beta,
            extract_did(m_pooled_1)$beta,
            extract_did(m_pooled_raw)$beta,
            extract_did(m_leak_A_base)$beta,
            extract_did(m_leak_B)$beta),
  se    = c(extract_did(m_closure_A_base)$se,
            extract_did(m_specB_1)$se,
            extract_did(m_pooled_1)$se,
            extract_did(m_pooled_raw)$se,
            extract_did(m_leak_A_base)$se,
            extract_did(m_leak_B)$se),
  p     = c(extract_did(m_closure_A_base)$p,
            extract_did(m_specB_1)$p,
            extract_did(m_pooled_1)$p,
            extract_did(m_pooled_raw)$p,
            extract_did(m_leak_A_base)$p,
            extract_did(m_leak_B)$p)
)
cat("Cross-Model Specification Summary:\n")
print(spec_summary)
fwrite(spec_summary, file.path(OUTPUT_TABLES, "Cross_Model_Spec_Summary.csv"))


#==============================================================================
# SECTION 16: JMP PUBLICATION TABLES — UNIFIED LaTeX
#==============================================================================

cat("\n========================================\n")
cat("SECTION 16: JMP PAPER TABLES\n")
cat("========================================\n\n")

# ─────────────────────────────────────────────────────────────────────────────
# JMP TABLE 3: Primary DiD — Spec A (Post-1988, Mandate-Free)
# 6 columns: Closure(1-2) | Exit|Closure(3-4) | Replace|Closure(5-6)
# ─────────────────────────────────────────────────────────────────────────────
cat("--- JMP Table 3: Spec A — Closure/Exit/Replace ---\n")

cols3 <- list(
  extract_did(m_closure_A_base),
  extract_did(m_closure_A_ctrl),
  extract_did(m_exit_A_base),
  extract_did(m_exit_A_ctrl),
  extract_did(m_rep_A_base),
  extract_did(m_rep_A_ctrl)
)

tex3 <- c(
  "\\begin{table}[htbp]",
  "\\centering",
  "\\caption{Policy Effects on Facility Decisions — Spec A (Post-1988 Cohort, Mandate-Free)}",
  "\\label{tbl:reg-closure}",
  "\\resizebox{\\textwidth}{!}{",
  "\\begin{tabular}{lcccccc}",
  "\\toprule",
  "\\textbf{Dependent Variables:} & \\multicolumn{2}{c}{\\textbf{Tank Closure}} & \\multicolumn{2}{c}{\\textbf{Exit $|$ Closure}} & \\multicolumn{2}{c}{\\textbf{Replace $|$ Closure}} \\\\",
  "\\cmidrule(lr){2-3} \\cmidrule(lr){4-5} \\cmidrule(lr){6-7}",
  "\\textbf{Model:} & (1) & (2) & (3) & (4) & (5) & (6) \\\\",
  "\\midrule",
  sprintf("Texas $\\times$ Post & %s%s & %s%s & %s%s & %s%s & %s%s & %s%s \\\\",
    sprintf("%.4f", cols3[[1]]$beta), stars_fn(cols3[[1]]$p),
    sprintf("%.4f", cols3[[2]]$beta), stars_fn(cols3[[2]]$p),
    sprintf("%.4f", cols3[[3]]$beta), stars_fn(cols3[[3]]$p),
    sprintf("%.4f", cols3[[4]]$beta), stars_fn(cols3[[4]]$p),
    sprintf("%.4f", cols3[[5]]$beta), stars_fn(cols3[[5]]$p),
    sprintf("%.4f", cols3[[6]]$beta), stars_fn(cols3[[6]]$p)),
  sprintf("& (%s) & (%s) & (%s) & (%s) & (%s) & (%s) \\\\",
    sprintf("%.4f", cols3[[1]]$se), sprintf("%.4f", cols3[[2]]$se),
    sprintf("%.4f", cols3[[3]]$se), sprintf("%.4f", cols3[[4]]$se),
    sprintf("%.4f", cols3[[5]]$se), sprintf("%.4f", cols3[[6]]$se)),
  "\\midrule",
  "Mean Age 1998 Control & No & Yes & No & Yes & No & Yes \\\\",
  "\\midrule",
  "\\textbf{Fixed-effects} & & & & & & \\\\",
  "Year FE & Yes & Yes & Yes & Yes & Yes & Yes \\\\",
  "Facility FE & Yes & Yes & Yes & Yes & Yes & Yes \\\\",
  "\\midrule",
  sprintf("Observations & %s & %s & %s & %s & %s & %s \\\\",
    format(cols3[[1]]$n, big.mark = ","), format(cols3[[2]]$n, big.mark = ","),
    format(cols3[[3]]$n, big.mark = ","), format(cols3[[4]]$n, big.mark = ","),
    format(cols3[[5]]$n, big.mark = ","), format(cols3[[6]]$n, big.mark = ",")),
  "\\bottomrule",
  "\\multicolumn{7}{p{0.95\\textwidth}}{\\footnotesize \\textit{Notes:}",
  "Difference-in-differences estimates using Spec A: facilities with 100\\% post-1988",
  "tanks, exempt from Texas 30 TAC Ch.\\ 334 phased mandate.",
  "Standard errors clustered at the state level in parentheses.",
  "$^{*}p<0.1$; $^{**}p<0.05$; $^{***}p<0.01$.}",
  "\\end{tabular}}",
  "\\end{table}"
)
writeLines(tex3, file.path(OUTPUT_TABLES, "JMP_Table_3_SpecA_Primary.tex"))
cat("✓ Saved: JMP_Table_3_SpecA_Primary.tex\n")

# ─────────────────────────────────────────────────────────────────────────────
# JMP TABLE 4: Spec B Robustness — Mandate Control Escalation
# ─────────────────────────────────────────────────────────────────────────────
cat("\n--- JMP Table 4: Spec B Robustness ---\n")

# Use etable for Spec B models (cleaner for escalation table)
specB_all <- list(m_specB_1, m_specB_2)
if (!is.null(m_specB_3)) specB_all <- c(specB_all, list(m_specB_3))

tryCatch(
  etable(specB_all,
         title = "Spec B Robustness: Mandate Control Escalation",
         tex = TRUE,
         file = file.path(OUTPUT_TABLES, "JMP_Table_4_SpecB_Robustness.tex")),
  error = function(e) message("  etable failed: ", e$message)
)
cat("✓ Saved: JMP_Table_4_SpecB_Robustness.tex\n")

# ─────────────────────────────────────────────────────────────────────────────
# JMP TABLE 6: Reported Leak
# ─────────────────────────────────────────────────────────────────────────────
cat("\n--- JMP Table 6: Reported Leak ---\n")

leak_cols <- list(
  extract_did(m_leak_A_base),
  extract_did(m_leak_A_ctrl),
  extract_did(m_leak_B),
  extract_did(m_leak_P)
)

tex6 <- c(
  "\\begin{table}[htbp]",
  "\\centering",
  "\\caption{Policy Effects on Reported Leak Rates}",
  "\\label{tbl:leak-results}",
  "\\begin{tabular}{lcccc}",
  "\\toprule",
  "& \\multicolumn{2}{c}{\\textbf{Spec A}} & \\textbf{Spec B} & \\textbf{Pooled} \\\\",
  "\\cmidrule(lr){2-3} \\cmidrule(lr){4-4} \\cmidrule(lr){5-5}",
  "& (1) & (2) & (3) & (4) \\\\",
  "\\midrule",
  sprintf("Texas $\\times$ Post & %s%s & %s%s & %s%s & %s%s \\\\",
    sprintf("%.4f", leak_cols[[1]]$beta), stars_fn(leak_cols[[1]]$p),
    sprintf("%.4f", leak_cols[[2]]$beta), stars_fn(leak_cols[[2]]$p),
    sprintf("%.4f", leak_cols[[3]]$beta), stars_fn(leak_cols[[3]]$p),
    sprintf("%.4f", leak_cols[[4]]$beta), stars_fn(leak_cols[[4]]$p)),
  sprintf("& (%s) & (%s) & (%s) & (%s) \\\\",
    sprintf("%.4f", leak_cols[[1]]$se), sprintf("%.4f", leak_cols[[2]]$se),
    sprintf("%.4f", leak_cols[[3]]$se), sprintf("%.4f", leak_cols[[4]]$se)),
  "\\midrule",
  "Mean Age 1998 & No & Yes & No & No \\\\",
  "Mandate Control & No & No & Yes & Yes \\\\",
  "\\midrule",
  "Facility FE & Yes & Yes & Yes & Yes \\\\",
  "Year FE & Yes & Yes & Yes & Yes \\\\",
  "\\midrule",
  sprintf("Observations & %s & %s & %s & %s \\\\",
    format(leak_cols[[1]]$n, big.mark = ","), format(leak_cols[[2]]$n, big.mark = ","),
    format(leak_cols[[3]]$n, big.mark = ","), format(leak_cols[[4]]$n, big.mark = ",")),
  "\\bottomrule",
  "\\multicolumn{5}{p{0.9\\textwidth}}{\\footnotesize \\textit{Notes:}",
  "Spec A: 100\\% post-1988 tanks (mandate-free). Spec B: 100\\% pre-1988 tanks",
  "with \\texttt{mandate\\_active} control. Pooled: all facilities with mandate control.",
  "Standard errors clustered at state level.",
  "$^{*}p<0.1$; $^{**}p<0.05$; $^{***}p<0.01$.}",
  "\\end{tabular}",
  "\\end{table}"
)
writeLines(tex6, file.path(OUTPUT_TABLES, "JMP_Table_6_Reported_Leak.tex"))
cat("✓ Saved: JMP_Table_6_Reported_Leak.tex\n")

# ─────────────────────────────────────────────────────────────────────────────
# JMP FIGURE 6: Combined Event Study (Spec A main + Spec B appendix ref)
# ─────────────────────────────────────────────────────────────────────────────
cat("\n--- JMP Figure 6: Combined reference ---\n")
cat("  Figure 6A (Spec A):  JMP_Figure_6A_Event_Study_SpecA.png\n")
cat("  Figure B-2 (Spec B): JMP_Figure_B2_Event_Study_SpecB.png\n")
cat("  Figure 7 (Leak):     JMP_Figure_7_Leak_Event_Study.png\n")


#==============================================================================
# SECTION 17: SCRIPT SUMMARY
#==============================================================================

cat("\n====================================================================\n")
cat("02_DiD_Causal_Estimates.R COMPLETE\n")
cat("  Three-Specification Mandate Confound Architecture\n")
cat("====================================================================\n\n")

cat("PRIMARY RESULTS (Spec A — Post-1988, Mandate-Free):\n")
cat("  Table 3:    Closure / Exit|Closure / Replace|Closure DiD\n")
cat("  Table 6:    Reported Leak DiD\n")
cat("  Figure 6A:  Closure Event Study\n")
cat("  Figure 7:   Leak Event Study\n\n")

cat("ROBUSTNESS (Spec B + Mandate Controls):\n")
cat("  Table 4:    Spec B — mandate control escalation\n")
cat("  Table 5:    Pooled — mandate control escalation\n")
cat("  Table B.4:  Parallel trends 4-test battery\n")
cat("  Table B.5:  MD-excluded\n")
cat("  Table B.5b: TN-excluded (leak models)\n")
cat("  Table B.6:  mandate_window_3yr sensitivity\n")
cat("  Figure B-2: Spec B event study\n\n")

cat("ADDITIONAL MODELS (existing architecture):\n")
cat("  Model 1B:   Facility Cox Survival\n")
cat("  Model 2:    Tank-Level Cox HTE\n")
cat("  Model 3A/B: Age at Closure (County/Facility FE)\n")
cat("  Model 4:    Revealed Leaks at Closure\n")
cat("  Model 5AB:  Competing Risks\n")
cat("  HTE:        Wall Type × Treatment by Spec\n\n")

cat(sprintf("Output:    %s\n", OUTPUT_TABLES))
cat(sprintf("Figures:   %s\n", OUTPUT_FIGURES))
cat(sprintf("Bootstrap: %s\n", ifelse(USE_BOOTSTRAP, "ENABLED", "DISABLED")))
cat(sprintf("Control states: %d (NJ excluded)\n", length(CONTROL_STATES)))
cat("====================================================================\n")