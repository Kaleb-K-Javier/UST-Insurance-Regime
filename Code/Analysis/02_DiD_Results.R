#==============================================================================
# 02_DiD_Causal_Estimates.R
# Texas UST Insurance Reform — Causal Evidence from DiD
#
# PURPOSE:
#   All causal estimation for the JMP paper Section 3 ("Causal Evidence").
#   Loads analysis-ready .rds files from 01_Descriptive_Analysis.R and produces
#   publication-ready regression tables and event study figures.
#
# PAPER OUTPUTS (JMP table/figure numbering):
#   Table 3:  Closure/Exit/Replace DiD (6-column, tbl-reg_closure)
#   Table 4:  Reported Leak DiD (tbl-leak_results)
#   Table 5:  Survival Analysis (Cox PH)
#   Table 6:  Revealed Leaks at Closure (LPM)
#   Table A3: Competing Risks (Cause-Specific Cox, Fine-Gray)
#   Table A4: Robustness — Regulatory Vintage & Mandate Controls
#   Table A5: Cohort Splits (Pooled|A|B|C|D)
#   Figure 6: Closure Event Study
#   Figure 7: Leak Event Study
#   Figure A1: Age-at-Closure Event Study
#
# STRUCTURE:
#   §1   Setup & Data Loading
#   §2   Helper Functions
#   §3   Model 1A — Closure DiD + Event Study
#   §4   Model 1C — Exit|Closure and Replace|Closure DiD [NEW]
#   §5   Model 1B — Facility Cox Survival
#   §6   Model 2  — Tank Cox HTE
#   §7   Model 3A — Age at Closure (County FE) + Event Study
#   §8   Model 3B — Age at Closure (Facility FE)
#   §9   Model 4  — Revealed Leaks at Closure
#   §10  Model 6  — Reported Leak DiD + Event Study [NEW]
#   §11  Models 5A/5B — Competing Risks
#   §12  Robustness — Regulatory Vintage & Phased Mandate
#   §13  JMP Publication Tables (unified multi-panel LaTeX)
#   §14  Script Summary
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

# Bootstrap toggle (override from metadata if desired)
USE_BOOTSTRAP <- FALSE   # <-- CHANGE TO TRUE FOR FINAL RUN
N_BOOTSTRAP   <- 9999

dir.create(OUTPUT_TABLES,  recursive = TRUE, showWarnings = FALSE)
dir.create(OUTPUT_FIGURES, recursive = TRUE, showWarnings = FALSE)

# ── Publication theme ──
theme_pub <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title      = element_text(face = "bold", size = base_size + 2),
      plot.subtitle   = element_text(color = "grey40", size = base_size),
      panel.grid.minor = element_blank(),
      legend.position  = "bottom"
    )
}
theme_set(theme_pub())

cat("====================================================================\n")
cat("02_DiD_Causal_Estimates.R\n")
cat("====================================================================\n")
cat(sprintf("Analysis: %d to %d | Treatment: %d\n",
            PANEL_START, PANEL_END, TREATMENT_YEAR))
cat(sprintf("Bootstrap: %s (%d reps)\n",
            ifelse(USE_BOOTSTRAP, "ENABLED", "DISABLED"), N_BOOTSTRAP))
cat(sprintf("Control states: %d\n", length(CONTROL_STATES)))
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

    # Match treatment variable (partial match allowed)
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
    tx_name <- rownames(ct)[tx_idx[1]]   # exact name used in model

    # Conventional clustered SE p-value
    p_conv <- round(row[, "Pr(>|t|)"], 4)

    # Wild Cluster Bootstrap (fwildclusterboot)
    # Per MacKinnon, Nielsen, Webb (2023): use Webb-6 weights as primary
    # when G <= ~20 clusters; Rademacher as cross-check
    p_webb <- NA_real_; ci_webb_lo <- NA_real_; ci_webb_hi <- NA_real_
    p_rad  <- NA_real_

    if (use_bootstrap) {
      tryCatch({
        boot_webb <- fwildclusterboot::boottest(
          m,
          param    = tx_name,
          clustid  = cluster_var,   # column name as character string
          B        = n_reps,
          type     = "webb",        # Webb-6 weights — recommended for G <= 20
          seed     = 20260202
        )
        p_webb    <- round(boot_webb$p_val,         4)
        ci_webb_lo <- round(boot_webb$conf_int[[1]], digits)
        ci_webb_hi <- round(boot_webb$conf_int[[2]], digits)

        boot_rad <- fwildclusterboot::boottest(
          m,
          param    = tx_name,
          clustid  = cluster_var,
          B        = n_reps,
          type     = "rademacher",  # Rademacher weights — robustness check
          seed     = 20260202
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

  # --- CSV ---
  csv_path <- file.path(OUTPUT_TABLES, paste0(base_name, ".csv"))
  fwrite(results_dt, csv_path)

  # --- TXT (human-readable with full summaries) ---
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

  # --- LaTeX via fixest::etable ---
  tex_path <- file.path(OUTPUT_TABLES, paste0(base_name, ".tex"))
  tryCatch(
    etable(models,
           title   = title,
           headers = list("(Specification)" = setNames(as.list(rep(1, length(headers))),
                                                       headers)),
           tex     = TRUE,
           file    = tex_path,
           digits  = digits),
    error = function(e) {
      message(sprintf("  [LaTeX] etable failed for %s: %s", base_name, e$message))
    }
  )

  cat(sprintf("  ✓ Saved: %s (.csv / .txt / .tex)\n", base_name))
  invisible(results_dt)
}

#------------------------------------------------------------------------------
# 2.2 save_cox_results()
# Saves a list of coxph models to .csv and .txt
#------------------------------------------------------------------------------
save_cox_results <- function(models, headers, base_name, title) {

  stopifnot(length(models) == length(headers))

  results_list <- lapply(seq_along(models), function(i) {
    m  <- models[[i]]
    s  <- summary(m)
    ct <- s$coefficients      # coef, exp(coef), se(coef), z, p
    ci <- s$conf.int          # lower/upper 95%

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
# 2.3 plot_event_study()
# Generic event study plotter from a fixest model estimated with i()
#------------------------------------------------------------------------------
plot_event_study <- function(es_model, title, subtitle = "",
                              filename, ref_year = -1,
                              ylab = "Treatment Effect Estimate",
                              colors = c("#0072B2")) {

  # Use broom::tidy for coefficient extraction
  coefs <- broom::tidy(es_model, conf.int = TRUE)

  # Parse event-time from term names produced by i()
  # fixest i() produces terms like "rel_year_1999::-5:texas_treated"
  # or "texas_treated::...:rel_year_1999" depending on order
  coefs <- as.data.table(coefs)
  coefs <- coefs[grepl("::", term)]
  coefs[, event_time := as.numeric(str_extract(term, "-?\\d+"))]
  coefs <- coefs[!is.na(event_time)]
  setorder(coefs, event_time)

  # Add omitted reference period
  ref_row <- data.table(term = "ref", estimate = 0, std.error = 0,
                        conf.low = 0, conf.high = 0, event_time = ref_year)
  coefs <- rbind(coefs[, .(term, estimate, std.error, conf.low,
                            conf.high, event_time)],
                 ref_row)
  setorder(coefs, event_time)

  # Pre-trend F-test (coefficients with event_time < ref_year)
  pre_vars <- coefs[event_time < ref_year & event_time != min(event_time), term]
  pre_pval <- tryCatch({
    wt <- wald(es_model, keep = pre_vars)
    wt$p
  }, error = function(e) NA_real_)

  pre_label <- ifelse(
    is.na(pre_pval), "Pre-trend F-test: N/A",
    sprintf("Pre-trend F-test: p = %.3f", pre_pval)
  )

  p <- ggplot(coefs, aes(x = event_time, y = estimate)) +
    geom_hline(yintercept = 0, color = "gray40", linewidth = 0.4) +
    geom_vline(xintercept = -0.5, linetype = "dashed",
               color = "firebrick", linewidth = 0.7) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
                alpha = 0.15, fill = colors[1]) +
    geom_line(color = colors[1], linewidth = 0.9) +
    geom_point(color = colors[1], size = 2) +
    labs(
      title    = title,
      subtitle = if (nchar(subtitle) > 0) subtitle else pre_label,
      x        = "Years Relative to Treatment (1999)",
      y        = ylab,
      caption  = "Reference year: 1998 (event time = −1). 95% CI shown."
    ) +
    theme_pub()

  ggsave(file.path(OUTPUT_FIGURES, filename),
         p, width = 10, height = 6, dpi = 300, bg = "white")

  cat(sprintf("  ✓ Saved: %s\n", filename))
  invisible(coefs)
}

#------------------------------------------------------------------------------
# 2.4 STANDARDIZED TREATMENT VARIABLE NAMES
# Used consistently throughout all models. Avoids ad-hoc naming.
#------------------------------------------------------------------------------
# Facility-year level (annual_data):
#   treated    = texas_treated  (1 if TX, 0 otherwise)
#   post       = post_1999      (1 if panel_year >= 1999)
#   did_term   = treated * post (interaction)
# Tank-level (tank_inventory, closed_tanks):
#   texas      = 1 if state == "TX"
#   post       = 1 if closure_year >= 1999  (for closed_tanks)
#   texas_post = texas * post               (for closed_tanks)
# Cox models: texas (cross-sectional, post is implicit in survival origin)

#------------------------------------------------------------------------------
# 2.5 run_cohort_did()
# Runs a feols specification across Pooled|A|B|C|D cohort splits.
# Returns a list of models + headers suitable for save_standard_did_table().
#
# Cohort definitions (1988 federal mandate cutoff):
#   Pooled: Full sample (no cohort restriction)
#   A:      Post-1988 tanks/facilities only (NOT subject to 1998 mandate)
#   B:      ≤1988 tanks/facilities only (SUBJECT to 1998 mandate)
#   C:      Pooled with cohort_pre1988 × panel_year FE
#   D:      Triple-difference: did_term + did_term × cohort_pre1988
#
# For tank-level models, cohort is direct (tank install date).
# For facility-level models, cohort is defined by facility composition:
#   A = all active tanks at 1999 installed after 12/22/1988
#   B = all active tanks at 1999 installed on/before 12/22/1988
#   C/D = full sample with cohort_pre1988 = majority of tanks are ≤1988
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
    feols(formula_pooled, data = data, cluster = as.formula(paste0("~", cluster_var)), lean = FALSE),
    error = function(e) { message("  Pooled failed: ", e$message); NULL }
  )
  models[["Pooled"]]  <- m_pooled
  headers[["Pooled"]] <- "Pooled"

  # --- SPEC A: Post-1988 only ---
  data_A <- data[get(cohort_var) == 0]
  if (nrow(data_A) > 100) {
    m_A <- tryCatch(
      feols(formula_pooled, data = data_A, cluster = as.formula(paste0("~", cluster_var)), lean = FALSE),
      error = function(e) { message("  Spec A failed: ", e$message); NULL }
    )
    models[["A"]]  <- m_A
    headers[["A"]] <- "A: Post-1988"
  } else {
    message("  Spec A skipped: N = ", nrow(data_A))
  }

  # --- SPEC B: ≤1988 only ---
  data_B <- data[get(cohort_var) == 1]
  if (nrow(data_B) > 100) {
    m_B <- tryCatch(
      feols(formula_pooled, data = data_B, cluster = as.formula(paste0("~", cluster_var)), lean = FALSE),
      error = function(e) { message("  Spec B failed: ", e$message); NULL }
    )
    models[["B"]]  <- m_B
    headers[["B"]] <- "B: Pre-1988"
  } else {
    message("  Spec B skipped: N = ", nrow(data_B))
  }

  # --- SPEC C: Pooled + cohort × year FE ---
  if (add_cohort_year_fe) {
    # Append cohort_pre1988^panel_year to the existing FE specification
    # Parse formula to inject additional FE term
    fml_str <- deparse(formula_pooled, width.cutoff = 500)
    # Inject cohort interaction FE: cohort_pre1988^panel_year
    fml_C_str <- sub("\\|\\s*(.+)$",
                      paste0("| \\1 + ", cohort_var, "^", fe_year_var),
                      fml_str)
    fml_C <- as.formula(fml_C_str)

    m_C <- tryCatch(
      feols(fml_C, data = data, cluster = as.formula(paste0("~", cluster_var)), lean = FALSE),
      error = function(e) { message("  Spec C failed: ", e$message); NULL }
    )
    models[["C"]]  <- m_C
    headers[["C"]] <- "C: Pooled + Cohort×Year FE"
  }

  # --- SPEC D: Triple-difference ---
  if (run_triple_diff) {
    fml_str <- deparse(formula_pooled, width.cutoff = 500)
    # Add cohort_pre1988 interactions to RHS
    # Replace "did_term" with "did_term + did_term:cohort_pre1988 + cohort_pre1988:post"
    fml_D_str <- sub(treatment_var_name,
                      paste0(treatment_var_name, " + ", treatment_var_name, ":",
                             cohort_var, " + post:", cohort_var),
                      fml_str)
    fml_D <- as.formula(fml_D_str)

    m_D <- tryCatch(
      feols(fml_D, data = data, cluster = as.formula(paste0("~", cluster_var)), lean = FALSE),
      error = function(e) { message("  Spec D failed: ", e$message); NULL }
    )
    models[["D"]]  <- m_D
    headers[["D"]] <- "D: Triple-Diff"
  }

  # Filter out NULLs
  keep <- !sapply(models, is.null)
  list(
    models  = models[keep],
    headers = headers[keep]
  )
}

# SECTION 3: MODEL 1A — FACILITY-YEAR DiD + EVENT STUDY
#==============================================================================

cat("\n========================================\n")
cat("SECTION 3: MODEL 1A — FACILITY DiD\n")
cat("========================================\n\n")

cat("Specification: Closure_Event_it = αi + λt + β(TX_i × Post_t) + εit\n")
cat("Outcome:  Any_Closure = 1 if facility closed ≥1 tank in year t\n")
cat("FE:       Facility (αi) + Year (λt)\n")
cat("Cluster:  State (19 clusters)\n\n")

# Baseline (POOLED)
model_1a <- feols(
  closure_event ~ did_term | panel_id + panel_year,
  data    = annual_data,
  cluster = ~state,
  lean    = FALSE
)

cat("--- Model 1A: Baseline (Pooled) ---\n")
print(summary(model_1a))

# With facility-size control (log tanks) — POOLED
model_1a_ctrl <- feols(
  closure_event ~ did_term + log(active_tanks_dec + 1) | panel_id + panel_year,
  data    = annual_data,
  cluster = ~state,
  lean    = FALSE
)

cat("\n--- Model 1A: With Controls (Pooled) ---\n")
print(summary(model_1a_ctrl))

# ─────────────────────────────────────────────────────────────────────────────
# Model 1A COHORT SPLITS: Pooled | A | B | C | D
# ─────────────────────────────────────────────────────────────────────────────
cat("\n--- Model 1A: Cohort Splits (1988 Federal Mandate) ---\n")
cat("Spec A: Post-1988 only (clean, no mandate confound)\n")
cat("Spec B: Pre-1988 only (composite mandate + insurance effect)\n")
cat("Spec C: Pooled + cohort×year FE\n")
cat("Spec D: Triple-difference (tests cohort heterogeneity)\n\n")

cohort_1a <- run_cohort_did(
  formula_pooled = closure_event ~ did_term | panel_id + panel_year,
  data           = annual_data,
  cohort_var     = "cohort_pre1988",
  treatment_var_name = "did_term"
)

# Print all cohort results
for (nm in names(cohort_1a$models)) {
  cat(sprintf("--- Model 1A [%s] ---\n", cohort_1a$headers[[nm]]))
  print(summary(cohort_1a$models[[nm]]))
  cat("\n")
}

# Save combined Pooled|A|B|C|D table
save_standard_did_table(
  models        = cohort_1a$models,
  headers       = unlist(cohort_1a$headers),
  base_name     = "Model_1A_Facility_Closure_Cohort_Splits",
  title         = "Model 1A: Closure Probability — Cohort Splits (Pooled|A|B|C|D)",
  treatment_var = "did_term",
  cluster_var   = "state",
  use_bootstrap = USE_BOOTSTRAP,
  n_reps        = N_BOOTSTRAP,
  digits        = 6
)

# Also save original 2-column baseline + controls table
save_standard_did_table(
  models        = list(model_1a, model_1a_ctrl),
  headers       = c("Baseline", "With Controls"),
  base_name     = "Model_1A_Facility_Closure_Probability",
  title         = "Model 1A: Treatment Effect on Annual Closure Probability",
  treatment_var = "did_term",
  cluster_var   = "state",
  use_bootstrap = USE_BOOTSTRAP,
  n_reps        = N_BOOTSTRAP,
  digits        = 6
)

#------------------------------------------------------------------------------
# Model 1A-ES: Event Study (1985–2018)
#------------------------------------------------------------------------------

cat("\n--- Model 1A-ES: Event Study ---\n")
cat(sprintf("Window: %d to %d (rel_year %d to %d)\n",
            ES_START, ES_END,
            ES_START - TREATMENT_YEAR, ES_END - TREATMENT_YEAR))

# Prepare event study data: bin extreme relative years
es_data <- annual_data[panel_year >= ES_START & panel_year <= ES_END]

# Bin rel_year at extremes so endpoints absorb tail variation
rel_min <- ES_START - TREATMENT_YEAR   # e.g. 1985-1999 = -14
rel_max <- ES_END   - TREATMENT_YEAR   # e.g. 2018-1999 = +19

es_data[, rel_year_bin := rel_year_1999]
es_data[rel_year_1999 <= rel_min, rel_year_bin := rel_min]
es_data[rel_year_1999 >= rel_max, rel_year_bin := rel_max]

# Estimate using fixest i() — reference period = -1 (year 1998)
model_1a_es <- feols(
  closure_event ~ i(rel_year_bin, texas_treated, ref = -1) |
    panel_id + panel_year,
  data    = es_data,
  cluster = ~state
)

cat("\n--- Model 1A-ES Summary ---\n")
print(summary(model_1a_es))

# Pre-trend F-test
pre_coefs <- names(coef(model_1a_es))
pre_coefs <- pre_coefs[grepl("::-[2-9]|::-1[0-9]", pre_coefs)]
pre_pval  <- tryCatch({
  wald(model_1a_es, keep = pre_coefs)$p
}, error = function(e) NA_real_)
cat(sprintf("\nPre-trends F-test (k < -1): p = %s\n",
            ifelse(is.na(pre_pval), "N/A", round(pre_pval, 4))))
if (!is.na(pre_pval) && pre_pval < 0.05)
  cat("⚠  Significant pre-trends — parallel trends may be violated.\n") else
  cat("✓  No significant pre-trends.\n")

# Plot using iplot (fixest native)
png(file.path(OUTPUT_FIGURES, "Model_1A_Event_Study_iplot.png"),
    width = 2400, height = 1440, res = 200)
iplot(model_1a_es,
      main  = "Model 1A Event Study: Annual Closure Probability",
      sub   = sprintf("Pre-trend F-test: p = %s | Reference: 1998",
                      ifelse(is.na(pre_pval), "N/A", round(pre_pval, 3))),
      xlab  = "Years Relative to Treatment (1999)",
      ylab  = "Treatment Effect (pp)",
      col   = "#0072B2")
abline(h = 0, lty = 2, col = "gray40")
dev.off()
cat("✓ Saved: Model_1A_Event_Study_iplot.png\n")

# Save event study coefficients
es_coefs_dt <- as.data.table(broom::tidy(model_1a_es, conf.int = TRUE))
fwrite(es_coefs_dt, file.path(OUTPUT_TABLES, "Model_1A_ES_Coefficients.csv"))


#==============================================================================
# SECTION 4: MODEL 1B — FACILITY-LEVEL COX SURVIVAL
#==============================================================================

cat("\n========================================\n")
cat("SECTION 4: MODEL 1B — FACILITY SURVIVAL\n")
cat("========================================\n\n")

cat("Specification: h_i(t) = h_0(t) × exp(β × TX_i)\n")
cat("Outcome:  Time from 1999 to first post-1999 tank closure\n")
cat("Method:   Cox proportional hazards\n\n")

# Build facility-level survival dataset from post-1999 panel
facility_surv <- annual_data[, .(
  texas   = first(texas_treated),
  state   = first(state),
  n_tanks = first(active_tanks_dec[panel_year == min(panel_year[panel_year >= 1999],
                                                     na.rm = TRUE)]),
  # First post-1999 closure year
  first_closure_post = fifelse(
    any(closure_event == 1 & panel_year >= TREATMENT_YEAR, na.rm = TRUE),
    min(panel_year[closure_event == 1 & panel_year >= TREATMENT_YEAR],
        na.rm = TRUE),
    NA_integer_
  ),
  last_obs = max(panel_year)
), by = panel_id]

# Survival time and event indicator
facility_surv[, `:=`(
  surv_time = fifelse(
    !is.na(first_closure_post),
    first_closure_post - TREATMENT_YEAR,
    pmin(last_obs, PANEL_END) - TREATMENT_YEAR
  ),
  surv_event = as.integer(!is.na(first_closure_post))
)]

facility_surv <- facility_surv[!is.na(surv_time) & surv_time >= 0]

cat(sprintf("Facilities in survival analysis: %s\n",
            format(nrow(facility_surv), big.mark = ",")))
cat(sprintf("  Events (first closure): %s (%.1f%%)\n",
            format(sum(facility_surv$surv_event), big.mark = ","),
            100 * mean(facility_surv$surv_event)))
cat(sprintf("  Censored:              %s (%.1f%%)\n\n",
            format(sum(1 - facility_surv$surv_event), big.mark = ","),
            100 * mean(1 - facility_surv$surv_event)))

# Baseline Cox
surv_obj_fac <- Surv(facility_surv$surv_time, facility_surv$surv_event)

model_1b <- coxph(
  surv_obj_fac ~ texas,
  data    = facility_surv,
  cluster = state
)

cat("--- Model 1B: Baseline ---\n")
print(summary(model_1b))

# With facility size control
model_1b_ctrl <- coxph(
  surv_obj_fac ~ texas + log(n_tanks + 1),
  data    = facility_surv,
  cluster = state
)

cat("\n--- Model 1B: With Controls ---\n")
print(summary(model_1b_ctrl))

# Interpretation
hr_1b <- exp(coef(model_1b)["texas"])
cat("\n--- Economic Interpretation ---\n")
cat(sprintf("Hazard Ratio (Texas): %.4f\n", hr_1b))
if (hr_1b > 1) {
  cat(sprintf("→ Texas facilities %.0f%% more likely to close a tank at any point\n",
              (hr_1b - 1) * 100))
  cat("→ Shorter expected time to first closure — consistent with reform\n")
} else {
  cat(sprintf("→ Texas facilities %.0f%% less likely to close a tank at any point\n",
              (1 - hr_1b) * 100))
}

save_cox_results(
  models   = list(model_1b, model_1b_ctrl),
  headers  = c("Baseline", "With Controls"),
  base_name = "Model_1B_Facility_Survival_First_Closure",
  title    = "Model 1B: Time-to-First-Closure (Facility-Level Cox)"
)


#==============================================================================
# SECTION 5: MODEL 2 — TANK-LEVEL COX HTE (TWO SPECIFICATIONS)
#==============================================================================

cat("\n========================================\n")
cat("SECTION 5: MODEL 2 — TANK-LEVEL COX HTE\n")
cat("========================================\n\n")

cat("Both specs share: Outcome = time from 1999 to tank closure\n\n")

# Prepare survival object
tank_surv_data <- tank_inventory[!is.na(time_to_close) & time_to_close >= 0 &
                                  !is.na(age_at_treatment)]
cat(sprintf("Tanks in HTE analysis: %s\n", format(nrow(tank_surv_data), big.mark = ",")))

tank_surv_obj <- Surv(tank_surv_data$time_to_close, tank_surv_data$event_closure)

#------------------------------------------------------------------------------
# SPEC A: Simple interactions (bug-fixed version of prior code)
# All tanks enter at 1999 → "post" is tautologically 1, so we estimate
# the cross-sectional treatment × risk factor interactions directly.
#------------------------------------------------------------------------------
cat("--- SPEC A: Texas × Risk Interactions (no strata) ---\n")

model_2a_specA <- coxph(
  tank_surv_obj ~ texas,
  data    = tank_surv_data,
  cluster = state
)

model_2b_specA <- coxph(
  tank_surv_obj ~ texas + texas:single_walled + single_walled,
  data    = tank_surv_data,
  cluster = state
)

model_2c_specA <- coxph(
  tank_surv_obj ~ texas + texas:single_walled + texas:old_at_treatment +
    texas:old_vintage + single_walled + old_at_treatment + old_vintage,
  data    = tank_surv_data,
  cluster = state
)

cat("Model 2A (Main effect only):\n"); print(summary(model_2a_specA))
cat("\nModel 2B (× Single-walled):\n"); print(summary(model_2b_specA))
cat("\nModel 2C (Full risk interactions):\n"); print(summary(model_2c_specA))

save_cox_results(
  models    = list(model_2a_specA, model_2b_specA, model_2c_specA),
  headers   = c("Main Effect", "× Single-Walled", "Full Interactions"),
  base_name = "Model_2_SpecA_Tank_HTE_Simple",
  title     = "Model 2 (Spec A): Tank-Level HTE — Simple Interactions"
)

#------------------------------------------------------------------------------
# SPEC B: Refactoring plan spec — county-stratified Cox
# Stratifying on county_fips absorbs local baseline hazard variation.
# This is the preferred specification per the refactoring plan.
#------------------------------------------------------------------------------
cat("\n--- SPEC B: County-Stratified Cox (Refactoring Plan Spec) ---\n")

# Require county_fips for stratification
tank_surv_strat <- tank_surv_data[!is.na(county_fips) & county_fips != ""]
tank_surv_obj_s <- Surv(tank_surv_strat$time_to_close, tank_surv_strat$event_closure)

cat(sprintf("Tanks with valid county FIPS: %s\n",
            format(nrow(tank_surv_strat), big.mark = ",")))

model_2a_specB <- coxph(
  tank_surv_obj_s ~ texas + strata(county_fips),
  data    = tank_surv_strat,
  cluster = state
)

model_2b_specB <- coxph(
  tank_surv_obj_s ~ texas * old_at_treatment + strata(county_fips),
  data    = tank_surv_strat,
  cluster = state
)

model_2c_specB <- coxph(
  tank_surv_obj_s ~ texas * single_walled + strata(county_fips),
  data    = tank_surv_strat,
  cluster = state
)

model_2d_specB <- coxph(
  tank_surv_obj_s ~ texas * vintage + strata(county_fips),
  data    = tank_surv_strat,
  cluster = state
)

cat("Model 2A-B (Main effect, county strata):\n"); print(summary(model_2a_specB))
cat("\nModel 2B-B (× Age > 20, county strata):\n"); print(summary(model_2b_specB))
cat("\nModel 2C-B (× Single-walled, county strata):\n"); print(summary(model_2c_specB))
cat("\nModel 2D-B (× Vintage cohort, county strata):\n"); print(summary(model_2d_specB))

save_cox_results(
  models    = list(model_2a_specB, model_2b_specB,
                   model_2c_specB, model_2d_specB),
  headers   = c("Main Effect", "× Age>20", "× Single-Walled", "× Vintage"),
  base_name = "Model_2_SpecB_Tank_HTE_County_Strata",
  title     = "Model 2 (Spec B): Tank-Level HTE — County-Stratified Cox"
)

# Interpretation (Spec B preferred)
cat("\n--- Economic Interpretation (Spec B, preferred) ---\n")
hr_main <- exp(coef(model_2a_specB)["texas"])
cat(sprintf("Main HR (Texas, county-stratified): %.4f\n", hr_main))

if ("texas:old_at_treatment" %in% names(coef(model_2b_specB))) {
  hr_age <- exp(coef(model_2b_specB)["texas:old_at_treatment"])
  cat(sprintf("HR interaction (Age > 20): %.4f\n", hr_age))
  if (hr_age > 1)
    cat("→ Old tanks close FASTER in Texas — consistent with risk-based sorting\n")
}

if ("texas:single_walled" %in% names(coef(model_2c_specB))) {
  hr_sw <- exp(coef(model_2c_specB)["texas:single_walled"])
  cat(sprintf("HR interaction (Single-walled): %.4f\n", hr_sw))
  if (hr_sw > 1)
    cat("→ Single-walled tanks close FASTER in Texas — risk-based pricing effect\n")
}


#==============================================================================
# SECTION 6: MODEL 3A — AGE AT CLOSURE (COUNTY FE) + EVENT STUDY
#==============================================================================

cat("\n========================================\n")
cat("SECTION 6: MODEL 3A — AGE AT CLOSURE\n")
cat("========================================\n\n")

cat("Specification: Age_j = μ_c + λ_t + β(TX_c × Post_t) + X_j'γ + ε_j\n")
cat("Outcome:  Age of tank when closed (years)\n")
cat("FE:       County (μ_c) + Closure Year (λ_t)\n")
cat("Cluster:  State\n\n")

model_3a_data <- closed_tanks[!is.na(age_at_closure) & !is.na(county_fips_fac)]
cat(sprintf("Closed tanks with valid age & county: %s\n",
            format(nrow(model_3a_data), big.mark = ",")))

# Baseline
model_3a <- feols(
  age_at_closure ~ texas_post | county_fips_fac + closure_year,
  data    = model_3a_data,
  cluster = ~state
)
cat("\n--- Model 3A: Baseline ---\n")
print(summary(model_3a))

# With controls
has_capacity <- "capacity" %in% names(model_3a_data)
if (has_capacity) {
  model_3a_ctrl <- feols(
    age_at_closure ~ texas_post + single_walled + log(capacity + 1) |
      county_fips_fac + closure_year,
    data    = model_3a_data,
    cluster = ~state
  )
  cat("\n--- Model 3A: With Controls ---\n")
  print(summary(model_3a_ctrl))
  models_3a   <- list(model_3a, model_3a_ctrl)
  headers_3a  <- c("Baseline", "With Controls")
} else {
  models_3a   <- list(model_3a)
  headers_3a  <- c("Baseline")
  cat("  (capacity column not found — skipping controls model)\n")
}

# Interpretation
beta_3a <- coef(model_3a)["texas_post"]
cat("\n--- Economic Interpretation ---\n")
cat(sprintf("β = %.3f years\n", beta_3a))
if (beta_3a < 0) {
  cat("→ Texas closed YOUNGER tanks post-treatment\n")
  cat("→ Consistent with marginal facility exits (full closure)\n")
} else {
  cat("→ Texas closed OLDER tanks post-treatment\n")
  cat("→ Consistent with selective culling of highest-risk capital\n")
}

save_standard_did_table(
  models        = models_3a,
  headers       = headers_3a,
  base_name     = "Model_3A_Age_at_Closure_County_FE",
  title         = "Model 3A: Age at Closure (County FE)",
  treatment_var = "texas_post",
  cluster_var   = "state",
  use_bootstrap = USE_BOOTSTRAP,
  n_reps        = N_BOOTSTRAP
)

# ─────────────────────────────────────────────────────────────────────────────
# Model 3A COHORT SPLITS: Pooled | A | B | C | D
# ─────────────────────────────────────────────────────────────────────────────
cat("\n--- Model 3A: Cohort Splits (1988 Federal Mandate) ---\n")

# For tank-level models, use tank's own cohort_pre1988
cohort_3a <- run_cohort_did(
  formula_pooled = age_at_closure ~ texas_post | county_fips_fac + closure_year,
  data           = model_3a_data,
  cohort_var     = "cohort_pre1988",
  treatment_var_name = "texas_post",
  fe_year_var    = "closure_year"
)

for (nm in names(cohort_3a$models)) {
  cat(sprintf("--- Model 3A [%s] ---\n", cohort_3a$headers[[nm]]))
  print(summary(cohort_3a$models[[nm]]))
  cat("\n")
}

save_standard_did_table(
  models        = cohort_3a$models,
  headers       = unlist(cohort_3a$headers),
  base_name     = "Model_3A_Age_at_Closure_Cohort_Splits",
  title         = "Model 3A: Age at Closure — Cohort Splits (Pooled|A|B|C|D)",
  treatment_var = "texas_post",
  cluster_var   = "state",
  use_bootstrap = USE_BOOTSTRAP,
  n_reps        = N_BOOTSTRAP
)

#------------------------------------------------------------------------------
# Model 3A-ES: Event Study for Age at Closure
#------------------------------------------------------------------------------
cat("\n--- Model 3A-ES: Event Study for Age at Closure ---\n")

model_3a_data[, `:=`(
  event_time   = closure_year - TREATMENT_YEAR,
  event_time_b = closure_year - TREATMENT_YEAR  # will be binned
)]

# Bin extremes
et_min_3a <- ES_START - TREATMENT_YEAR
et_max_3a <- ES_END   - TREATMENT_YEAR
model_3a_data[event_time_b < et_min_3a, event_time_b := et_min_3a]
model_3a_data[event_time_b > et_max_3a, event_time_b := et_max_3a]

model_3a_es <- feols(
  age_at_closure ~ i(event_time_b, texas, ref = -1) |
    county_fips_fac + closure_year,
  data    = model_3a_data[closure_year >= ES_START & closure_year <= ES_END],
  cluster = ~state
)

cat("--- Model 3A-ES Summary ---\n")
print(summary(model_3a_es))

# Save event study plot
png(file.path(OUTPUT_FIGURES, "Model_3A_ES_Age_at_Closure.png"),
    width = 2400, height = 1440, res = 200)
iplot(model_3a_es,
      main = "Model 3A Event Study: Age at Closure",
      sub  = "Reference: 1998. County + closure-year FE.",
      xlab = "Years Relative to Treatment (1999)",
      ylab = "Differential Age at Closure (years)",
      col  = "#D55E00")
abline(h = 0, lty = 2, col = "gray40")
dev.off()
cat("✓ Saved: Model_3A_ES_Age_at_Closure.png\n")

es_3a_dt <- as.data.table(broom::tidy(model_3a_es, conf.int = TRUE))
fwrite(es_3a_dt, file.path(OUTPUT_TABLES, "Model_3A_ES_Coefficients.csv"))


#==============================================================================
# SECTION 7: MODEL 3B — AGE AT CLOSURE (FACILITY FE, SPANNING SAMPLE)
#==============================================================================

cat("\n========================================\n")
cat("SECTION 7: MODEL 3B — WITHIN-FACILITY\n")
cat("========================================\n\n")

cat("Specification: Age_jt = αi + λt + β(Post_t) + εjt\n")
cat("Identification: Within-facility across pre/post periods\n")
cat("Sample: TX + Control facilities with closures in BOTH periods\n\n")

# Identify spanning facilities (closures before AND after 1999)
span_counts <- closed_tanks[, .(
  n_pre  = sum(closure_year < TREATMENT_YEAR),
  n_post = sum(closure_year >= TREATMENT_YEAR)
), by = panel_id]

spanning_ids   <- span_counts[n_pre >= 1 & n_post >= 1, panel_id]
model_3b_data  <- closed_tanks[panel_id %in% spanning_ids & !is.na(age_at_closure)]

cat(sprintf("Spanning facilities (TX + Control): %s (%.1f%% of all facilities)\n",
            format(length(spanning_ids), big.mark = ","),
            100 * length(spanning_ids) / uniqueN(closed_tanks$panel_id)))

cat(sprintf("Texas spanning:   %s\n",
            format(uniqueN(model_3b_data[texas == 1, panel_id]), big.mark = ",")))
cat(sprintf("Control spanning: %s\n",
            format(uniqueN(model_3b_data[texas == 0, panel_id]), big.mark = ",")))

cat(sprintf("Closure events in spanning sample: %s (%.1f%% of all closures)\n\n",
            format(nrow(model_3b_data), big.mark = ","),
            100 * nrow(model_3b_data) / nrow(closed_tanks)))

if (nrow(model_3b_data) == 0) {
  cat("⚠  No spanning facilities found — Model 3B cannot be estimated.\n")
} else {
  # Post coefficient = within-facility change in closure age after 1999.
  # Texas fixed effect is absorbed by facility FE, so the DiD comes from
  # comparing the post coefficient for TX vs control via interaction.
  model_3b <- feols(
    age_at_closure ~ post | panel_id + closure_year,
    data    = model_3b_data,
    cluster = ~state
  )

  # Full DiD version: texas_post within spanning facilities
  model_3b_did <- feols(
    age_at_closure ~ texas_post + post | panel_id + closure_year,
    data    = model_3b_data,
    cluster = ~state
  )

  cat("--- Model 3B: Post Effect (TX absorbed by facility FE) ---\n")
  print(summary(model_3b))
  cat("\n--- Model 3B-DiD: Explicit TX×Post within spanning facilities ---\n")
  print(summary(model_3b_did))

  cat("\nNote: In model_3b, the 'post' coefficient is the average within-facility\n")
  cat("      change in closure age post-1999 (both TX and Control).\n")
  cat("      In model_3b_did, texas_post isolates the TX-specific change.\n")

  # FIX: Split save calls — model_3b uses "post", model_3b_did uses "texas_post"
  save_standard_did_table(
    models        = list(model_3b),
    headers       = c("Post Only (Facility FE)"),
    base_name     = "Model_3B_Age_at_Closure_Post_Only",
    title         = "Model 3B: Age at Closure (Facility FE, Post Effect)",
    treatment_var = "post",
    cluster_var   = "state",
    use_bootstrap = USE_BOOTSTRAP,
    n_reps        = N_BOOTSTRAP
  )
  save_standard_did_table(
    models        = list(model_3b_did),
    headers       = c("TX×Post (DiD within Spanning)"),
    base_name     = "Model_3B_Age_at_Closure_TXxPost",
    title         = "Model 3B: Age at Closure (Facility FE, TX×Post DiD)",
    treatment_var = "texas_post",
    cluster_var   = "state",
    use_bootstrap = USE_BOOTSTRAP,
    n_reps        = N_BOOTSTRAP
  )
  cat("✓ Saved: Model_3B_Age_at_Closure_Facility_FE.*\n")
}


#==============================================================================
# SECTION 8: MODEL 4 — REVEALED LEAKS AT CLOSURE
#==============================================================================

cat("\n========================================\n")
cat("SECTION 8: MODEL 4 — REVEALED LEAKS\n")
cat("========================================\n\n")

cat("Research question: When tanks close, does the closure reveal contamination?\n")
cat("Outcome: Revealed leak indicator at facility-year (conditional on closure)\n")
cat("Using pre-built panel columns (validated by panel builder)\n\n")
cat("Leak window definitions:\n")
cat("  Primary:     tank_closure_revealed       (0–60 days)\n")
cat("  Narrow:      tank_closure_revealed_narrow (0–30 days)\n")
cat("  Wide:        tank_closure_revealed_wide   (0–90 days)\n")
cat("  Regulatory:  tank_closure_revealed_reg    (0–45 days)\n\n")

# Model 4 uses the facility-year panel, restricted to years with closures
model_4_data <- annual_data[n_closures > 0 &
                              !is.na(tank_closure_revealed) &
                              !is.na(county_fips_fac)]

# Create binary outcomes (any revealed leak in that facility-year)
model_4_data[, `:=`(
  revealed_primary = as.integer(tank_closure_revealed       > 0),
  revealed_narrow  = as.integer(tank_closure_revealed_narrow > 0),
  revealed_wide    = as.integer(tank_closure_revealed_wide   > 0),
  revealed_reg     = as.integer(tank_closure_revealed_reg    > 0)
)]

cat(sprintf("Facility-years with closures: %s\n",
            format(nrow(model_4_data), big.mark = ",")))
cat(sprintf("  Revealed leak rate (primary, 0-60d): %.1f%%\n",
            100 * mean(model_4_data$revealed_primary, na.rm = TRUE)))

#--- PRIMARY: Linear Probability Model (LPM) with county + year FE ---
cat("\n--- Model 4 PRIMARY: LPM (county FE + closure year FE) ---\n")

model_4_lpm_primary <- feols(
  revealed_primary ~ did_term | county_fips_fac + panel_year,
  data    = model_4_data,
  cluster = ~state
)
cat("Primary window (0-60d):\n"); print(summary(model_4_lpm_primary))

model_4_lpm_narrow <- feols(
  revealed_narrow ~ did_term | county_fips_fac + panel_year,
  data    = model_4_data,
  cluster = ~state
)
cat("\nNarrow window (0-30d):\n"); print(summary(model_4_lpm_narrow))

model_4_lpm_wide <- feols(
  revealed_wide ~ did_term | county_fips_fac + panel_year,
  data    = model_4_data,
  cluster = ~state
)
cat("\nWide window (0-90d):\n"); print(summary(model_4_lpm_wide))

model_4_lpm_reg <- feols(
  revealed_reg ~ did_term | county_fips_fac + panel_year,
  data    = model_4_data,
  cluster = ~state
)
cat("\nRegulatory window (0-45d):\n"); print(summary(model_4_lpm_reg))

save_standard_did_table(
  models        = list(model_4_lpm_primary, model_4_lpm_narrow,
                       model_4_lpm_wide,    model_4_lpm_reg),
  headers       = c("0-60d (Primary)", "0-30d (Narrow)",
                    "0-90d (Wide)",    "0-45d (Reg.)"),
  base_name     = "Model_4_Revealed_Leaks_LPM",
  title         = "Model 4: Revealed Leaks at Closure (LPM, County FE)",
  treatment_var = "did_term",
  cluster_var   = "state",
  use_bootstrap = USE_BOOTSTRAP,
  n_reps        = N_BOOTSTRAP
)

#--- ROBUSTNESS: Logistic Regression (no FE) ---
cat("\n--- Model 4 ROBUSTNESS: Logit (no FE) ---\n")

model_4_logit_primary <- feols(
  revealed_primary ~ did_term,
  data    = model_4_data,
  cluster = ~state,
  family  = "logit"
)
cat("Primary window (0-60d), logit:\n"); print(summary(model_4_logit_primary))

model_4_logit_narrow <- feols(
  revealed_narrow ~ did_term,
  data    = model_4_data,
  cluster = ~state,
  family  = "logit"
)
model_4_logit_wide <- feols(
  revealed_wide ~ did_term,
  data    = model_4_data,
  cluster = ~state,
  family  = "logit"
)
model_4_logit_reg <- feols(
  revealed_reg ~ did_term,
  data    = model_4_data,
  cluster = ~state,
  family  = "logit"
)

save_standard_did_table(
  models        = list(model_4_logit_primary, model_4_logit_narrow,
                       model_4_logit_wide,    model_4_logit_reg),
  headers       = c("0-60d (Primary)", "0-30d (Narrow)",
                    "0-90d (Wide)",    "0-45d (Reg.)"),
  base_name     = "Model_4_Revealed_Leaks_Logit_Robustness",
  title         = "Model 4 (Robustness): Revealed Leaks — Logit",
  treatment_var = "did_term",
  cluster_var   = "state",
  use_bootstrap = USE_BOOTSTRAP,
  n_reps        = N_BOOTSTRAP
)

# Interpretation
cat("\n--- Economic Interpretation ---\n")
beta_4 <- coef(model_4_lpm_primary)["did_term"]
cat(sprintf("β_LPM (primary window) = %.4f (%.2f pp)\n", beta_4, beta_4 * 100))
if (beta_4 > 0) {
  cat("→ Texas post-1999 closures MORE likely to reveal contamination\n")
  cat("→ Consistent with proactive risk mitigation: closing leaky tanks\n")
} else {
  cat("→ Texas post-1999 closures LESS likely to reveal contamination\n")
  cat("→ Inconsistent with risk-based selection hypothesis\n")
}

# ─────────────────────────────────────────────────────────────────────────────
# Model 4 COHORT SPLITS: Pooled | A | B | C | D (Primary window only)
# ─────────────────────────────────────────────────────────────────────────────
cat("\n--- Model 4: Cohort Splits (Primary 0-60d window) ---\n")

cohort_4 <- run_cohort_did(
  formula_pooled = revealed_primary ~ did_term | county_fips_fac + panel_year,
  data           = model_4_data,
  cohort_var     = "cohort_pre1988",
  treatment_var_name = "did_term"
)

for (nm in names(cohort_4$models)) {
  cat(sprintf("--- Model 4 [%s] ---\n", cohort_4$headers[[nm]]))
  print(summary(cohort_4$models[[nm]]))
  cat("\n")
}

save_standard_did_table(
  models        = cohort_4$models,
  headers       = unlist(cohort_4$headers),
  base_name     = "Model_4_Revealed_Leaks_Cohort_Splits",
  title         = "Model 4: Revealed Leaks — Cohort Splits (Pooled|A|B|C|D)",
  treatment_var = "did_term",
  cluster_var   = "state",
  use_bootstrap = USE_BOOTSTRAP,
  n_reps        = N_BOOTSTRAP
)


#==============================================================================
# SECTION 9: MODELS 5A/5B — OPERATIONAL LEAKS, COMPETING RISKS
#==============================================================================

cat("\n========================================\n")
cat("SECTION 9: MODELS 5A/5B — COMPETING RISKS\n")
cat("========================================\n\n")

cat("INCIDENCE (not recurrence): using event_first_leak + year_of_first_leak\n")
cat("Per Memo Section 6.2: leak_year measures ANY leak including re-occurrences;\n")
cat("event_first_leak measures the FIRST-EVER leak (tank transitions to 'leaker').\n")
cat("Competing events:\n")
cat("  Event 1: First-ever operational leak at facility (event of interest)\n")
cat("  Event 2: First tank closure at facility          (competing event)\n")
cat("  Event 0: Neither by study end                   (censored)\n\n")

# Build facility-level competing risks dataset
# Use pre-built panel columns (calculated by panel builder at facility level):
#   year_of_first_leak:    year of facility's first-ever leak (constant per facility)
#   event_first_closure:   1 in the year of the first closure (binary, by year)
#
# We need ONE row per facility (cross-section for Cox):
compete_data <- annual_data[, .(
  texas          = first(texas_treated),
  state          = first(state),
  # year_of_first_leak is constant across rows for a given facility
  yr_first_leak  = first(na.omit(year_of_first_leak)),  # NA if never leaked
  # First closure year (from flow variable)
  yr_first_close = fifelse(
    any(closure_event == 1, na.rm = TRUE),
    min(panel_year[closure_event == 1], na.rm = TRUE),
    NA_integer_
  ),
  last_obs = max(panel_year)
), by = panel_id]

# Competing event classification
# Event 1: first incidence leak before or concurrent with first closure
# Event 2: first closure before first incidence leak
# Event 0: neither (censored)
compete_data[, event_type := fcase(
  !is.na(yr_first_leak) &
    (is.na(yr_first_close) | yr_first_leak <= yr_first_close), 1L,
  !is.na(yr_first_close) &
    (is.na(yr_first_leak)  | yr_first_close < yr_first_leak),  2L,
  default = 0L
)]

# Survival time from 1999 (or from start of observation if before 1999)
# For facilities where event occurred before 1999: left-truncate at 1999
compete_data[, event_time := fcase(
  event_type == 1L,
    pmax(yr_first_leak  - TREATMENT_YEAR, 0),
  event_type == 2L,
    pmax(yr_first_close - TREATMENT_YEAR, 0),
  default = pmin(last_obs, PANEL_END) - TREATMENT_YEAR
)]

# Drop impossible survival times
compete_data <- compete_data[!is.na(event_time) & event_time >= 0]

# Report distribution
cat(sprintf("Facilities in competing risks analysis: %s\n",
            format(nrow(compete_data), big.mark = ",")))
cat(sprintf("  Event 1 (leaked first):  %s (%.1f%%)\n",
            format(sum(compete_data$event_type == 1), big.mark = ","),
            100 * mean(compete_data$event_type == 1)))
cat(sprintf("  Event 2 (closed first):  %s (%.1f%%)\n",
            format(sum(compete_data$event_type == 2), big.mark = ","),
            100 * mean(compete_data$event_type == 2)))
cat(sprintf("  Event 0 (censored):      %s (%.1f%%)\n\n",
            format(sum(compete_data$event_type == 0), big.mark = ","),
            100 * mean(compete_data$event_type == 0)))

#--- MODEL 5A: Cause-Specific Cox ---
cat("--- Model 5A: Cause-Specific Cox ---\n")
cat("Estimates hazard of each event type, treating the other as censoring.\n\n")

# 5A-1: Hazard of first LEAK (treating closure as censoring)
surv_5a_leak <- Surv(compete_data$event_time,
                     as.integer(compete_data$event_type == 1))
model_5a_leak <- coxph(
  surv_5a_leak ~ texas,
  data    = compete_data,
  cluster = state
)
cat("Cause-specific Cox — Event: First Leak (closure = censored):\n")
print(summary(model_5a_leak))

# 5A-2: Hazard of first CLOSURE (treating leak as censoring)
surv_5a_close <- Surv(compete_data$event_time,
                      as.integer(compete_data$event_type == 2))
model_5a_close <- coxph(
  surv_5a_close ~ texas,
  data    = compete_data,
  cluster = state
)
cat("\nCause-specific Cox — Event: First Closure (leak = censored):\n")
print(summary(model_5a_close))

save_cox_results(
  models    = list(model_5a_leak, model_5a_close),
  headers   = c("Event: First Leak (CS-Cox)", "Event: First Closure (CS-Cox)"),
  base_name = "Model_5A_Cause_Specific_Cox",
  title     = "Model 5A: Cause-Specific Cox (Competing Risks)"
)

#--- MODEL 5B: Fine-Gray Subdistribution Hazard ---
cat("\n--- Model 5B: Fine-Gray Competing Risks ---\n")
cat("Estimates subdistribution hazard for LEAK, accounting for closure.\n\n")

# Fine-Gray requires competing event coded as factor
tryCatch({
  compete_data[, event_factor := factor(event_type, levels = c(0, 1, 2))]

  # finegray() from survival package
  fg_formula <- as.formula(
    paste("Surv(event_time, event_factor) ~ texas + state")
  )
  fg_data <- finegray(
    Surv(event_time,
         factor(event_type, levels = c(0, 1, 2))) ~ .,
    data = compete_data[, .(event_time, event_type, texas, state)]
  )

  model_5b <- coxph(
    Surv(fgstart, fgstop, fgstatus) ~ texas,
    data   = fg_data,
    weight = fgwt,
    cluster = state
  )

  cat("Fine-Gray (subdistribution hazard for first leak):\n")
  print(summary(model_5b))

  save_cox_results(
    models    = list(model_5b),
    headers   = c("Fine-Gray (CIF for Leak)"),
    base_name = "Model_5B_Fine_Gray_Subdistribution",
    title     = "Model 5B: Fine-Gray Subdistribution Hazard (Competing Risks)"
  )

}, error = function(e) {
  cat(sprintf("⚠  Model 5B failed: %s\n", e$message))
  cat("   Check that cmprsk is installed and compete_data is properly structured.\n")
})

# Combined interpretation
cat("\n--- Economic Interpretation ---\n")
hr_5a_leak  <- tryCatch(exp(coef(model_5a_leak)["texas"]),  error = function(e) NA)
hr_5a_close <- tryCatch(exp(coef(model_5a_close)["texas"]), error = function(e) NA)
cat(sprintf("Cause-specific HR (Leak):    %.4f\n", hr_5a_leak))
cat(sprintf("Cause-specific HR (Closure): %.4f\n", hr_5a_close))
cat("If HR(Closure) > 1 and HR(Leak) < 1 or ≈ 1:\n")
cat("→ Texas facilities closed faster, but weren't necessarily leaking more\n")
cat("→ Supports proactive exit interpretation over operational failure\n")


#==============================================================================
# SECTION 10: ROBUSTNESS — REGULATORY VINTAGE & PHASED MANDATE CONTROLS
#==============================================================================

cat("\n========================================\n")
cat("SECTION 10: ROBUSTNESS CHECKS\n")
cat("========================================\n\n")

# ─────────────────────────────────────────────────────────────────────────────
# 17.1  Regulatory Vintage Controls (existing)
# ─────────────────────────────────────────────────────────────────────────────
cat("--- Robustness 1: Regulatory Vintage Controls (1998 Federal Mandate) ---\n")
cat("The Dec 22, 1998 federal mandate required all tanks installed ≤ 1988\n")
cat("to retrofit or close by the deadline. This confounds our 1999 TX reform.\n")
cat("We add pre1998_install × post_1999 interaction to absorb mandate effects.\n\n")

if ("reg_vintage" %in% names(annual_data)) {
  annual_data[, reg_vintage := factor(reg_vintage,
                                       levels = c("Post-Deadline", "Transition", "Pre-RCRA"))]

  model_rob_1a <- feols(
    closure_event ~ did_term + pre1998_install:post_1999 |
      panel_id + panel_year,
    data    = annual_data,
    cluster = ~state
  )
  cat("Model 1A + pre1998 mandate interaction:\n")
  print(summary(model_rob_1a))

  model_rob_1b <- feols(
    closure_event ~ did_term + reg_vintage:post_1999 |
      panel_id + panel_year,
    data    = annual_data,
    cluster = ~state
  )
  cat("\nModel 1A + reg_vintage × post interactions:\n")
  print(summary(model_rob_1b))

  save_standard_did_table(
    models        = list(model_1a, model_rob_1a, model_rob_1b),
    headers       = c("Baseline",
                      "+ 1998 Mandate Control",
                      "+ Reg. Vintage × Post"),
    base_name     = "Robustness_Regulatory_Vintage_Controls",
    title         = "Robustness: Regulatory Vintage Controls (1998 Federal Mandate)",
    treatment_var = "did_term",
    cluster_var   = "state",
    use_bootstrap = USE_BOOTSTRAP,
    n_reps        = N_BOOTSTRAP
  )
  cat("✓ Saved: Robustness_Regulatory_Vintage_Controls.*\n")

  beta_base <- coef(model_1a)["did_term"]
  beta_rob  <- coef(model_rob_1a)["did_term"]
  cat(sprintf("\nBaseline β: %.6f | After mandate control β: %.6f\n",
              beta_base, beta_rob))
  pct_change <- 100 * (beta_rob - beta_base) / abs(beta_base)
  cat(sprintf("Change: %.1f%% — ", pct_change))
  if (abs(pct_change) < 10)
    cat("Estimate stable; mandate confound unlikely to be large.\n") else
    cat("Estimate changes noticeably; mandate confound may matter.\n")

} else {
  cat("⚠  reg_vintage column not found in panel — skipping.\n")
}

# ─────────────────────────────────────────────────────────────────────────────
# 17.2  Texas Phased Mandate Exposure Control (NEW)
# ─────────────────────────────────────────────────────────────────────────────
cat("\n--- Robustness 2: Texas Phased Mandate Exposure ---\n")
cat("Texas had phased compliance deadlines (1989-1993) for pre-1988 tanks,\n")
cat("while control states all faced the federal 12/22/1998 deadline.\n")
cat("This means TX pre-1988 tanks were 5-9 years PAST their mandate deadline\n")
cat("at treatment, vs. control pre-1988 tanks that just hit theirs.\n")
cat("mandate_exposure controls for this differential timing.\n\n")

if ("mandate_exposure" %in% names(annual_data)) {
  # Model 1A + continuous mandate exposure control
  model_rob_2a <- feols(
    closure_event ~ did_term + mandate_x_post |
      panel_id + panel_year,
    data    = annual_data,
    cluster = ~state
  )
  cat("Model 1A + mandate_exposure × post:\n")
  print(summary(model_rob_2a))

  # Model 1A + mandate exposure + vintage controls (kitchen sink)
  model_rob_2b <- feols(
    closure_event ~ did_term + mandate_x_post + pre1998_install:post_1999 |
      panel_id + panel_year,
    data    = annual_data,
    cluster = ~state
  )
  cat("\nModel 1A + mandate_exposure × post + pre1998 × post:\n")
  print(summary(model_rob_2b))

  save_standard_did_table(
    models        = list(model_1a, model_rob_2a, model_rob_2b),
    headers       = c("Baseline",
                      "+ Phased Mandate Exposure",
                      "+ Phased + Pre1998×Post"),
    base_name     = "Robustness_Phased_Mandate_Controls",
    title         = "Robustness: Texas Phased Mandate Exposure Controls",
    treatment_var = "did_term",
    cluster_var   = "state",
    use_bootstrap = USE_BOOTSTRAP,
    n_reps        = N_BOOTSTRAP
  )
  cat("✓ Saved: Robustness_Phased_Mandate_Controls.*\n")

  beta_rob2 <- coef(model_rob_2a)["did_term"]
  cat(sprintf("\nBaseline β: %.6f | After phased mandate β: %.6f\n",
              beta_base, beta_rob2))
  pct_change2 <- 100 * (beta_rob2 - beta_base) / abs(beta_base)
  cat(sprintf("Change: %.1f%%\n", pct_change2))
} else {
  cat("⚠  mandate_exposure not found — skipping.\n")
}

# ─────────────────────────────────────────────────────────────────────────────
# 17.3  Cohort Splits Summary Table (NEW)
# ─────────────────────────────────────────────────────────────────────────────
cat("\n--- Robustness 3: Cohort Split Summary ---\n")
cat("Collecting Pooled|A|B|C|D estimates across all DiD models.\n\n")

# Gather cohort split results for a summary comparison
cohort_summary_list <- list()

# Model 1A splits
if (exists("cohort_1a") && !is.null(cohort_1a$models)) {
  for (nm in names(cohort_1a$models)) {
    m <- cohort_1a$models[[nm]]
    if (!is.null(m) && "did_term" %in% names(coef(m))) {
      ct <- summary(m)$coeftable
      idx <- grep("did_term", rownames(ct))[1]
      if (length(idx) > 0) {
        cohort_summary_list[[paste0("1A_", nm)]] <- data.table(
          Model = "1A: Closure Prob",
          Spec  = cohort_1a$headers[[nm]],
          Estimate = round(ct[idx, "Estimate"], 6),
          Std_Error = round(ct[idx, "Std. Error"], 6),
          p_value  = round(ct[idx, "Pr(>|t|)"], 4),
          N_obs    = nobs(m)
        )
      }
    }
  }
}

# Model 3A splits
if (exists("cohort_3a") && !is.null(cohort_3a$models)) {
  for (nm in names(cohort_3a$models)) {
    m <- cohort_3a$models[[nm]]
    if (!is.null(m) && "texas_post" %in% names(coef(m))) {
      ct <- summary(m)$coeftable
      idx <- grep("texas_post", rownames(ct))[1]
      if (length(idx) > 0) {
        cohort_summary_list[[paste0("3A_", nm)]] <- data.table(
          Model = "3A: Age at Closure",
          Spec  = cohort_3a$headers[[nm]],
          Estimate = round(ct[idx, "Estimate"], 4),
          Std_Error = round(ct[idx, "Std. Error"], 4),
          p_value  = round(ct[idx, "Pr(>|t|)"], 4),
          N_obs    = nobs(m)
        )
      }
    }
  }
}

# Model 4 splits
if (exists("cohort_4") && !is.null(cohort_4$models)) {
  for (nm in names(cohort_4$models)) {
    m <- cohort_4$models[[nm]]
    if (!is.null(m) && "did_term" %in% names(coef(m))) {
      ct <- summary(m)$coeftable
      idx <- grep("did_term", rownames(ct))[1]
      if (length(idx) > 0) {
        cohort_summary_list[[paste0("4_", nm)]] <- data.table(
          Model = "4: Revealed Leaks",
          Spec  = cohort_4$headers[[nm]],
          Estimate = round(ct[idx, "Estimate"], 6),
          Std_Error = round(ct[idx, "Std. Error"], 6),
          p_value  = round(ct[idx, "Pr(>|t|)"], 4),
          N_obs    = nobs(m)
        )
      }
    }
  }
}

if (length(cohort_summary_list) > 0) {
  cohort_summary <- rbindlist(cohort_summary_list)
  cat("Cohort Split Summary:\n")
  print(cohort_summary)
  fwrite(cohort_summary,
         file.path(OUTPUT_TABLES, "Robustness_Cohort_Split_Summary.csv"))
  cat("✓ Saved: Robustness_Cohort_Split_Summary.csv\n")
} else {
  cat("  No cohort split results available.\n")
}


#==============================================================================


#==============================================================================
# SECTION 11: MODEL 1C — EXIT|CLOSURE AND REPLACE|CLOSURE DiD [NEW]
#==============================================================================
# Paper: tbl-reg_closure, columns (3)-(6)
# Sample: Facility-years where n_closures > 0 (conditional on closure)
# Outcomes:
#   exit_flag          = 1 if facility exits permanently (all tanks closed)
#   replace_indicator  = 1 if facility closed a tank but stayed (replacement)
#
# This decomposes the closure margin into:
#   P(Closure) = P(Exit | Closure) × P(Closure) + P(Replace | Closure) × P(Closure)
#==============================================================================

cat("\n========================================\n")
cat("SECTION 11: MODEL 1C — EXIT/REPLACE | CLOSURE\n")
cat("========================================\n\n")

cat("Specification: Outcome_it = αi + λt + β(TX × Post) + γ·AgeBins + εit\n")
cat("Sample: Facility-years with at least one tank closure\n")
cat("Outcomes: exit_flag (permanent exit), replace_indicator (tank swap)\n\n")

# Subset to facility-years with closures
closure_data <- annual_data[closure_event == 1]
cat(sprintf("Facility-years with closures: %s\n",
            format(nrow(closure_data), big.mark = ",")))

# Create replace indicator: closure happened but facility didn't exit
closure_data[, replace_indicator := as.integer(exit_flag == 0)]

cat(sprintf("  Exit rate (conditional on closure): %.1f%%\n",
            100 * mean(closure_data$exit_flag, na.rm = TRUE)))
cat(sprintf("  Replace rate (conditional on closure): %.1f%%\n",
            100 * mean(closure_data$replace_indicator, na.rm = TRUE)))

# ── Exit | Closure ──
model_1c_exit_base <- feols(
  exit_flag ~ did_term | panel_id + panel_year,
  data    = closure_data,
  cluster = ~state,
  lean    = FALSE
)

model_1c_exit_ctrl <- feols(
  exit_flag ~ did_term + i(age_bins) | panel_id + panel_year,
  data    = closure_data,
  cluster = ~state,
  lean    = FALSE
)

cat("\n--- Exit | Closure (Baseline) ---\n")
print(summary(model_1c_exit_base))
cat("\n--- Exit | Closure (With Age Controls) ---\n")
print(summary(model_1c_exit_ctrl))

save_standard_did_table(
  models        = list(model_1c_exit_base, model_1c_exit_ctrl),
  headers       = c("Exit: Baseline", "Exit: Age Controls"),
  base_name     = "Model_1C_Exit_Given_Closure",
  title         = "Model 1C: Pr(Exit | Closure)",
  treatment_var = "did_term",
  cluster_var   = "state",
  use_bootstrap = USE_BOOTSTRAP,
  n_reps        = N_BOOTSTRAP
)

# ── Replace | Closure ──
model_1c_rep_base <- feols(
  replace_indicator ~ did_term | panel_id + panel_year,
  data    = closure_data,
  cluster = ~state,
  lean    = FALSE
)

model_1c_rep_ctrl <- feols(
  replace_indicator ~ did_term + i(age_bins) | panel_id + panel_year,
  data    = closure_data,
  cluster = ~state,
  lean    = FALSE
)

cat("\n--- Replace | Closure (Baseline) ---\n")
print(summary(model_1c_rep_base))
cat("\n--- Replace | Closure (With Age Controls) ---\n")
print(summary(model_1c_rep_ctrl))

save_standard_did_table(
  models        = list(model_1c_rep_base, model_1c_rep_ctrl),
  headers       = c("Replace: Baseline", "Replace: Age Controls"),
  base_name     = "Model_1C_Replace_Given_Closure",
  title         = "Model 1C: Pr(Replace | Closure)",
  treatment_var = "did_term",
  cluster_var   = "state",
  use_bootstrap = USE_BOOTSTRAP,
  n_reps        = N_BOOTSTRAP
)




#==============================================================================
# SECTION 12: MODEL 6 — REPORTED LEAK DiD + EVENT STUDY [NEW]
#==============================================================================
# Paper: tbl-leak_results
# Sample: Full facility-year panel (not conditional on closure)
# Outcome: leak_year (= 1 if facility reported any leak in year t)
#
# This is the UNCONDITIONAL leak probability — distinct from:
#   Model 4 (revealed leaks conditional on closure)
#   Model 5A/5B (competing risks: first leak vs first closure)
#==============================================================================

cat("\n========================================\n")
cat("SECTION 12: MODEL 6 — REPORTED LEAK DiD\n")
cat("========================================\n\n")

cat("Specification: Leak_it = αi + λt + β(TX × Post) + γ·AgeBins + εit\n")
cat("Outcome: leak_year = 1 if any leak reported at facility i in year t\n\n")

cat(sprintf("Baseline leak rate (pre-1999): %.3f%%\n",
            100 * mean(annual_data[panel_year < TREATMENT_YEAR]$leak_year, na.rm = TRUE)))

# ── DiD Estimation ──
model_6_base <- feols(
  leak_year ~ did_term | panel_id + panel_year,
  data    = annual_data,
  cluster = ~state,
  lean    = FALSE
)

model_6_ctrl <- feols(
  leak_year ~ did_term + i(age_bins) | panel_id + panel_year,
  data    = annual_data,
  cluster = ~state,
  lean    = FALSE
)

cat("--- Model 6: Reported Leak (Baseline) ---\n")
print(summary(model_6_base))
cat("\n--- Model 6: Reported Leak (With Age Controls) ---\n")
print(summary(model_6_ctrl))

save_standard_did_table(
  models        = list(model_6_base, model_6_ctrl),
  headers       = c("Baseline", "Age Controls"),
  base_name     = "Model_6_Reported_Leak",
  title         = "Model 6: Reported Leak Probability (Unconditional)",
  treatment_var = "did_term",
  cluster_var   = "state",
  use_bootstrap = USE_BOOTSTRAP,
  n_reps        = N_BOOTSTRAP
)

# ── Cohort Splits ──
cat("\n--- Model 6: Cohort Splits ---\n")
cohort_6 <- run_cohort_did(
  formula_pooled = leak_year ~ did_term | panel_id + panel_year,
  data           = annual_data,
  cohort_var     = "cohort_pre1988",
  treatment_var_name = "did_term"
)

for (nm in names(cohort_6$models)) {
  cat(sprintf("--- Model 6 [%s] ---\n", cohort_6$headers[[nm]]))
  print(summary(cohort_6$models[[nm]]))
  cat("\n")
}

save_standard_did_table(
  models        = cohort_6$models,
  headers       = unlist(cohort_6$headers),
  base_name     = "Model_6_Reported_Leak_Cohort_Splits",
  title         = "Model 6: Reported Leak — Cohort Splits (Pooled|A|B|C|D)",
  treatment_var = "did_term",
  cluster_var   = "state",
  use_bootstrap = USE_BOOTSTRAP,
  n_reps        = N_BOOTSTRAP
)

# ── Event Study ──
cat("\n--- Model 6-ES: Leak Event Study ---\n")

es_data_6 <- annual_data[panel_year >= ES_START & panel_year <= ES_END]
rel_min <- ES_START - TREATMENT_YEAR
rel_max <- ES_END   - TREATMENT_YEAR
es_data_6[, rel_year_bin := pmax(pmin(rel_year_1999, rel_max), rel_min)]

model_6_es <- feols(
  leak_year ~ i(rel_year_bin, texas_treated, ref = -1) |
    panel_id + panel_year,
  data    = es_data_6,
  cluster = ~state
)

cat("Model 6-ES summary:\n")
print(summary(model_6_es))

# Pre-trend F-test
pre_coefs_6 <- names(coef(model_6_es))
pre_coefs_6 <- pre_coefs_6[grepl("::-[2-9]|::-1[0-9]", pre_coefs_6)]
pre_pval_6  <- tryCatch(wald(model_6_es, keep = pre_coefs_6)$p,
                          error = function(e) NA_real_)
cat(sprintf("Pre-trends F-test: p = %s\n",
            ifelse(is.na(pre_pval_6), "N/A", round(pre_pval_6, 4))))

# Publication-quality event study plot
png(file.path(OUTPUT_FIGURES, "JMP_Figure_7_Leak_Event_Study.png"),
    width = 2400, height = 1440, res = 200)
iplot(model_6_es,
      main  = "",
      xlab  = "Years Relative to Treatment (1999)",
      ylab  = "Effect on Pr(Reported Leak)",
      col   = "#D55E00")
abline(h = 0, lty = 2, col = "gray40")
abline(v = -0.5, lty = 3, col = "red")
dev.off()
cat("Saved: JMP_Figure_7_Leak_Event_Study.png\n")

es_coefs_6_dt <- as.data.table(broom::tidy(model_6_es, conf.int = TRUE))
fwrite(es_coefs_6_dt, file.path(OUTPUT_TABLES, "Model_6_ES_Coefficients.csv"))


#==============================================================================
# SECTION 13: JMP PUBLICATION TABLES — UNIFIED MULTI-PANEL LaTeX
#==============================================================================
# These produce the exact tables for the paper, combining multiple models.
#==============================================================================

cat("\n========================================\n")
cat("SECTION 13: JMP PAPER TABLES\n")
cat("========================================\n\n")

# ─────────────────────────────────────────────────────────────────────────────
# JMP TABLE 3: Policy Effects on Facility Decisions (tbl-reg_closure)
# 6 columns: Closure(1-2) | Exit|Closure(3-4) | Replace|Closure(5-6)
# ─────────────────────────────────────────────────────────────────────────────
cat("--- JMP Table 3: Unified Closure/Exit/Replace ---\n")

# Collect coefficients
extract_did <- function(m, tvar = "did_term") {
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

stars_fn <- function(p) {
  if (is.na(p)) return("")
  if (p < 0.01) return("$^{***}$")
  if (p < 0.05) return("$^{**}$")
  if (p < 0.10) return("$^{*}$")
  return("")
}

# Extract from all 6 models
cols <- list(
  extract_did(model_1a),           # (1) Closure baseline
  extract_did(model_1a_ctrl),      # (2) Closure + age
  extract_did(model_1c_exit_base), # (3) Exit baseline
  extract_did(model_1c_exit_ctrl), # (4) Exit + age
  extract_did(model_1c_rep_base),  # (5) Replace baseline
  extract_did(model_1c_rep_ctrl)   # (6) Replace + age
)

tex3 <- c(
  "\\begin{table}[htbp]",
  "\\centering",
  "\\caption{Policy Effects on Single-Walled Facility Decisions}",
  "\\label{tbl:reg-closure}",
  "\\resizebox{\\textwidth}{!}{",
  "\\begin{tabular}{lcccccc}",
  "\\toprule",
  "\\textbf{Dependent Variables:} & \\multicolumn{2}{c}{\\textbf{Tank Closure}} & \\multicolumn{2}{c}{\\textbf{Exit $|$ Closure}} & \\multicolumn{2}{c}{\\textbf{Replace $|$ Closure}} \\\\",
  "\\cmidrule(lr){2-3} \\cmidrule(lr){4-5} \\cmidrule(lr){6-7}",
  "\\textbf{Model:} & (1) & (2) & (3) & (4) & (5) & (6) \\\\",
  "\\midrule",
  sprintf("Texas $\\times$ Post-Policy & %s%s & %s%s & %s%s & %s%s & %s%s & %s%s \\\\",
    sprintf("%.4f", cols[[1]]$beta), stars_fn(cols[[1]]$p),
    sprintf("%.4f", cols[[2]]$beta), stars_fn(cols[[2]]$p),
    sprintf("%.4f", cols[[3]]$beta), stars_fn(cols[[3]]$p),
    sprintf("%.4f", cols[[4]]$beta), stars_fn(cols[[4]]$p),
    sprintf("%.4f", cols[[5]]$beta), stars_fn(cols[[5]]$p),
    sprintf("%.4f", cols[[6]]$beta), stars_fn(cols[[6]]$p)),
  sprintf("& (%s) & (%s) & (%s) & (%s) & (%s) & (%s) \\\\",
    sprintf("%.4f", cols[[1]]$se),
    sprintf("%.4f", cols[[2]]$se),
    sprintf("%.4f", cols[[3]]$se),
    sprintf("%.4f", cols[[4]]$se),
    sprintf("%.4f", cols[[5]]$se),
    sprintf("%.4f", cols[[6]]$se)),
  "\\midrule",
  sprintf("Age Bin Controls & No & Yes & No & Yes & No & Yes \\\\"),
  "\\midrule",
  "\\textbf{Fixed-effects} & & & & & & \\\\",
  "Year FE & Yes & Yes & Yes & Yes & Yes & Yes \\\\",
  "Facility FE & Yes & Yes & Yes & Yes & Yes & Yes \\\\",
  "\\midrule",
  sprintf("Observations & %s & %s & %s & %s & %s & %s \\\\",
    format(cols[[1]]$n, big.mark = ","),
    format(cols[[2]]$n, big.mark = ","),
    format(cols[[3]]$n, big.mark = ","),
    format(cols[[4]]$n, big.mark = ","),
    format(cols[[5]]$n, big.mark = ","),
    format(cols[[6]]$n, big.mark = ",")),
  "\\bottomrule",
  "\\multicolumn{7}{p{0.95\\textwidth}}{\\footnotesize \\textit{Notes:}",
  "Difference-in-differences estimates of Texas's 1999 transition from public to",
  "private UST insurance on single-walled tank facilities operating before 1999.",
  "Columns (1)--(2): annual probability of closing any tank. Columns (3)--(4):",
  "probability of permanent facility exit, conditional on closure. Columns (5)--(6):",
  "probability of tank replacement (closure without exit), conditional on closure.",
  "All models include facility and year fixed effects.",
  "Standard errors clustered at the state level in parentheses.",
  "$^{*}p<0.1$; $^{**}p<0.05$; $^{***}p<0.01$.}",
  "\\end{tabular}}",
  "\\end{table}"
)

writeLines(tex3, file.path(OUTPUT_TABLES, "JMP_Table_3_Closure_Exit_Replace.tex"))
cat("Saved: JMP_Table_3_Closure_Exit_Replace.tex\n")

# ─────────────────────────────────────────────────────────────────────────────
# JMP TABLE 4: Reported Leak Results (tbl-leak_results)
# ─────────────────────────────────────────────────────────────────────────────
cat("\n--- JMP Table 4: Reported Leak ---\n")

leak_cols <- list(
  extract_did(model_6_base),
  extract_did(model_6_ctrl)
)

tex4 <- c(
  "\\begin{table}[htbp]",
  "\\centering",
  "\\caption{Policy Effects on Reported Leak Rates}",
  "\\label{tbl:leak-results}",
  "\\begin{tabular}{lcc}",
  "\\toprule",
  "\\textbf{Dependent Variable:} & \\multicolumn{2}{c}{\\textbf{Reported Leak (0/1)}} \\\\",
  "\\cmidrule(lr){2-3}",
  "\\textbf{Model:} & (1) & (2) \\\\",
  "\\midrule",
  sprintf("Texas $\\times$ Post-1999 & %s%s & %s%s \\\\",
    sprintf("%.4f", leak_cols[[1]]$beta), stars_fn(leak_cols[[1]]$p),
    sprintf("%.4f", leak_cols[[2]]$beta), stars_fn(leak_cols[[2]]$p)),
  sprintf("& (%s) & (%s) \\\\",
    sprintf("%.4f", leak_cols[[1]]$se),
    sprintf("%.4f", leak_cols[[2]]$se)),
  "\\midrule",
  "Age Bin Controls & No & Yes \\\\",
  "\\midrule",
  "\\textbf{Fixed-effects} & & \\\\",
  "Year FE & Yes & Yes \\\\",
  "Facility FE & Yes & Yes \\\\",
  "\\midrule",
  sprintf("Observations & %s & %s \\\\",
    format(leak_cols[[1]]$n, big.mark = ","),
    format(leak_cols[[2]]$n, big.mark = ",")),
  "\\bottomrule",
  "\\multicolumn{3}{p{0.85\\textwidth}}{\\footnotesize \\textit{Notes:}",
  "Difference-in-differences estimates of the effect of Texas's 1999 transition",
  "from public to private UST insurance on reported leak rates for single-walled",
  "tank facilities operating before 1999. The dependent variable equals one if",
  "facility $i$ reported any leak in year $t$. All models include facility and year",
  "fixed effects. Model (2) adds flexible age-bin controls.",
  "Standard errors clustered at the state level in parentheses.",
  "$^{*}p<0.1$; $^{**}p<0.05$; $^{***}p<0.01$.}",
  "\\end{tabular}",
  "\\end{table}"
)

writeLines(tex4, file.path(OUTPUT_TABLES, "JMP_Table_4_Reported_Leak.tex"))
cat("Saved: JMP_Table_4_Reported_Leak.tex\n")

# ─────────────────────────────────────────────────────────────────────────────
# JMP FIGURE 6: Closure Event Study (publication quality)
# ─────────────────────────────────────────────────────────────────────────────
cat("\n--- JMP Figure 6: Closure Event Study ---\n")

png(file.path(OUTPUT_FIGURES, "JMP_Figure_6_Closure_Event_Study.png"),
    width = 2400, height = 1440, res = 200)
iplot(model_1a_es,
      main  = "",
      xlab  = "Years Relative to Treatment (1999)",
      ylab  = "Effect on Pr(Tank Closure)",
      col   = "#0072B2")
abline(h = 0, lty = 2, col = "gray40")
abline(v = -0.5, lty = 3, col = "red")
dev.off()
cat("Saved: JMP_Figure_6_Closure_Event_Study.png\n")




#==============================================================================
# SECTION 14: SCRIPT SUMMARY
#==============================================================================

cat("\n====================================================================\n")
cat("02_DiD_Causal_Estimates.R COMPLETE\n")
cat("====================================================================\n\n")

cat("MODELS ESTIMATED:\n")
cat("  1A: Closure DiD + Event Study\n")
cat("  1C: Exit|Closure + Replace|Closure DiD [NEW]\n")
cat("  1B: Facility Cox Survival\n")
cat("   2: Tank-Level Cox HTE\n")
cat("  3A: Age at Closure (County FE) + Event Study\n")
cat("  3B: Age at Closure (Facility FE)\n")
cat("   4: Revealed Leaks at Closure (LPM + Logit)\n")
cat("   6: Reported Leak DiD + Event Study [NEW]\n")
cat(" 5AB: Competing Risks (Cause-Specific Cox, Fine-Gray)\n\n")

cat("JMP PAPER TABLES:\n")
cat("  Table 3: JMP_Table_3_Closure_Exit_Replace.tex\n")
cat("  Table 4: JMP_Table_4_Reported_Leak.tex\n")
cat("  Figure 6: JMP_Figure_6_Closure_Event_Study.png\n")
cat("  Figure 7: JMP_Figure_7_Leak_Event_Study.png\n\n")

cat(sprintf("Output directory: %s\n", OUTPUT_TABLES))
cat(sprintf("Figures directory: %s\n", OUTPUT_FIGURES))
cat(sprintf("Bootstrap: %s\n",
            ifelse(USE_BOOTSTRAP, "ENABLED (production)", "DISABLED (fast testing)")))
cat("====================================================================\n")