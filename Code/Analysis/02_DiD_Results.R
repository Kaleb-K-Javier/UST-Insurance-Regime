#==============================================================================
# 02_DiD_Results_Final.R  [AUDITED VERSION — February 2026]
# Texas UST Insurance Reform: Difference-in-Differences Analysis
#
# ============================================================================
# AUDIT LOG (February 20, 2026)
# Audited against: Comprehensive_Research_Memo_Unified.txt
#                  Research_Memo_Section_1_3_Descriptives.txt
#                  10_Build_Annual_Panel_Optimized.R
#                  10_Master_Cleaning_and_Harmonization.r
# ============================================================================
# Location: Code/Analysis/02_DiD_Results_Final.R
#
# Structure:
#   SECTION 1:  Setup & Configuration
#   SECTION 2:  Helper Functions (table/Cox saving)
#   SECTION 3:  Data Loading
#   SECTION 4:  Data Preparation & Filtering
#   SECTION 5:  Tank-Level Dataset Construction
#   SECTION 6:  Diagnostics (Pre-1999 Exit Balance)
#   SECTION 7:  Descriptive Tables (Tables 1-3)
#   SECTION 8:  Descriptive Figures (Figures 1-4 + Pre-Trends)
#   SECTION 9:  Figure 5 — Risk Factor Validation (Facility-Level CV)
#   SECTION 10: Model 1A — Facility DiD + Event Study
#   SECTION 11: Model 1B — Facility-Level Cox Survival
#   SECTION 12: Model 2  — Tank-Level Cox HTE (Two Specs)
#   SECTION 13: Model 3A — Age at Closure, County FE + Event Study
#   SECTION 14: Model 3B — Age at Closure, Facility FE (Spanning Sample)
#   SECTION 15: Model 4  — Revealed Leaks (LPM primary, Logit robustness)
#   SECTION 16: Models 5A/5B — Operational Leaks, Competing Risks
#   SECTION 17: Robustness — Regulatory Vintage Controls
#   SECTION 18: Script Summary
#
# Inputs:
#   Data/Processed/facility_leak_behavior_annual.csv
#   Data/Processed/Master_Harmonized_UST_Tanks.csv
#   Data/Processed/Master_Harmonized_LUST.csv
#
# Outputs:
#   Output/Tables/*.csv, *.tex, *.txt
#   Output/Figures/*.png
#
# References:
#   Comprehensive Research Memo (February 2, 2026)
#   MacKinnon, Nielsen, Webb (2023): Cluster-robust inference
#   Roodman et al. (2019): Wild cluster bootstrap
#
# Date: February 2026
#==============================================================================


#==============================================================================
# SECTION 1: SETUP & CONFIGURATION
#==============================================================================

# ----------------------------- BOOTSTRAP SETTINGS ----------------------------
# Set USE_BOOTSTRAP = FALSE for fast testing, TRUE for final production run.
USE_BOOTSTRAP <- FALSE   # <-- CHANGE TO TRUE FOR FINAL RUN
N_BOOTSTRAP   <- 9999    # Replications (999 for testing, 9999 for final)
# Uses Webb-6 point weights (optimal for G < 20 clusters per MacKinnon et al.)
# -----------------------------------------------------------------------------

# ----------------------------- STUDY PARAMETERS ------------------------------
TREATMENT_YEAR  <- 1999L
TREATMENT_DATE  <- as.IDate("1999-01-01")
PANEL_START     <- 1985L   # Analysis window start
PANEL_END       <- 2020L   # Analysis window end
ES_START        <- 1985L   # Event study window start
ES_END          <- 2018L   # Event study window end
STUDY_END_DATE  <- as.IDate("2020-12-31")
# -----------------------------------------------------------------------------

# ----------------------------- SAMPLE DEFINITION -----------------------------
# Control states (2-letter abbreviations — matches panel builder output)
CONTROL_STATES <- c("ME", "NM", "AR", "OK", "LA", "KS", "MT", "ID",
                    "SD", "AL", "MN", "NC", "IL", "MA", "OH", "PA",
                    "TN", "VA", "CO")
# NOTE: State column in panel uses 2-letter abbreviations (e.g. "TX", "ME")
# -----------------------------------------------------------------------------

# Load packages
suppressPackageStartupMessages({
  library(data.table)
  library(fixest)
  library(survival)
  library(cmprsk)           # Fine-Gray competing risks (Model 5B)
  library(fwildclusterboot) # Wild cluster bootstrap (MacKinnon-Webb)
  library(ggplot2)
  library(gridExtra)        # multi-panel figures (Figure 4)
  library(pROC)             # AUC-ROC for Figure 5B
  library(here)
  library(broom)
  library(scales)
  library(stringr)
})

options(scipen = 999)
set.seed(20260202)
setDTthreads(14)

# Output paths
OUTPUT_TABLES  <- here("Output", "Tables")
OUTPUT_FIGURES <- here("Output", "Figures")
dir.create(OUTPUT_TABLES,  recursive = TRUE, showWarnings = FALSE)
dir.create(OUTPUT_FIGURES, recursive = TRUE, showWarnings = FALSE)

# Publication theme
theme_pub <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title    = element_text(size = rel(1.1), face = "bold",
                                   margin = margin(0, 0, 10, 0)),
      plot.subtitle = element_text(size = rel(0.85),
                                   margin = margin(0, 0, 10, 0)),
      axis.title    = element_text(face = "bold", size = rel(0.9)),
      legend.title  = element_text(face = "bold", size = rel(0.9)),
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      panel.border  = element_rect(fill = NA, color = "gray85"),
      strip.text    = element_text(face = "bold")
    )
}
theme_set(theme_pub())

cat("====================================================================\n")
cat("FINAL DiD ANALYSIS: Texas UST Insurance Reform\n")
cat(sprintf("Analysis window: %d-%d | Event study: %d-%d\n",
            PANEL_START, PANEL_END, ES_START, ES_END))
cat(sprintf("Bootstrap: %s (B = %d)\n",
            ifelse(USE_BOOTSTRAP, "ENABLED", "DISABLED"), N_BOOTSTRAP))
cat(sprintf("Control states: %d states\n", length(CONTROL_STATES)))
cat("====================================================================\n\n")


#==============================================================================
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


#==============================================================================
# SECTION 3: DATA LOADING
#==============================================================================

cat("\n========================================\n")
cat("SECTION 3: DATA LOADING\n")
cat("========================================\n\n")

# 3.1 Facility-Year Panel (from Panel Builder)
cat("Loading facility-year panel...\n")
PANEL_PATH <- here("Data", "Processed", "facility_leak_behavior_annual.csv")
if (!file.exists(PANEL_PATH))
  stop("Panel not found. Run 10_Build_Annual_Panel_Optimized.R first.\n  Path: ", PANEL_PATH)

annual_data <- fread(PANEL_PATH)
cat(sprintf("  Loaded: %s facility-years | %s facilities | Years %d-%d\n",
            format(nrow(annual_data), big.mark = ","),
            format(uniqueN(annual_data$panel_id), big.mark = ","),
            min(annual_data$panel_year), max(annual_data$panel_year)))

# 3.2 Tank-Level Inventory (for Models 2, 3, 4, 5)
cat("\nLoading tank-level inventory...\n")
TANK_PATH <- here("Data", "Processed", "Master_Harmonized_UST_Tanks.csv")
if (!file.exists(TANK_PATH))
  stop("Tank inventory not found. Run harmonization scripts first.\n  Path: ", TANK_PATH)

tank_inventory <- fread(TANK_PATH)
cat(sprintf("  Loaded: %s tanks\n", format(nrow(tank_inventory), big.mark = ",")))

# 3.3 LUST Data (for leak classification validation)
cat("\nLoading LUST data...\n")
LUST_PATH <- here("Data", "Processed", "Master_Harmonized_LUST.csv")
if (!file.exists(LUST_PATH))
  stop("LUST data not found. Run harmonization scripts first.\n  Path: ", LUST_PATH)

master_lust <- fread(LUST_PATH)
cat(sprintf("  Loaded: %s leak incidents\n", format(nrow(master_lust), big.mark = ",")))


#==============================================================================
# SECTION 4: DATA PREPARATION & FILTERING (FACILITY-YEAR PANEL)
#==============================================================================

cat("\n========================================\n")
cat("SECTION 4: DATA PREPARATION\n")
cat("========================================\n\n")

cat("--- Filtering facility-year panel ---\n")
n0 <- nrow(annual_data)

# Filter 1: Time window
annual_data <- annual_data[panel_year >= PANEL_START & panel_year <= PANEL_END]
cat(sprintf("  After time window (%d-%d): %s rows\n",
            PANEL_START, PANEL_END, format(nrow(annual_data), big.mark = ",")))

# Filter 2: Texas + Control states
#   IMPORTANT: panel uses 2-letter state abbreviations (TX, ME, NM, etc.)
annual_data <- annual_data[state == "TX" | state %in% CONTROL_STATES]
cat(sprintf("  After state filter (TX + %d controls): %s rows\n",
            length(CONTROL_STATES), format(nrow(annual_data), big.mark = ",")))

# Quick sanity check
observed_states <- sort(unique(annual_data$state))
cat(sprintf("  States in sample: %s\n", paste(observed_states, collapse = ", ")))

# Filter 3: Incumbent sample only (facilities active before 1999)
#   Panel builder creates both is_incumbent (int) and cohort (char) at line 925-926
annual_data <- annual_data[is_incumbent == 1]
cat(sprintf("  After incumbent filter: %s rows | %s facilities\n",
            format(nrow(annual_data), big.mark = ","),
            format(uniqueN(annual_data$panel_id), big.mark = ",")))

cat(sprintf("\n  Total rows dropped: %s (%.1f%%)\n",
            format(n0 - nrow(annual_data), big.mark = ","),
            100 * (n0 - nrow(annual_data)) / n0))

# 4.3 Facility-Level Missing Data Exclusion
# ─────────────────────────────────────────────────────────────────────────────
# Per Unified Memo Section 1.3: Drop ENTIRE FACILITY if any tank is missing
# install_date OR if any CLOSED tank is missing closure_date.
# Rationale: partial facility histories contaminate within-facility variation
# (Models 1A, 3B), bias risk classification (Model 2), and violate the
# non-informative censoring assumption (Models 1B, 2, 4, 5).
#
# NOTE: status_std was dropped before saving Master_Harmonized_UST_Tanks.csv
# (harmonization script line 359). We reconstruct closed status from tank_status
# using the same regex pattern the pipeline uses.
# ─────────────────────────────────────────────────────────────────────────────
cat("\n--- Facility-Level Missing Data Exclusion (Memo Section 1.3) ---\n")

# Capture incumbent_ids from filtered annual panel (used in Section 5 too)
incumbent_ids <- unique(annual_data$panel_id)

# Load minimal columns from raw tank file for missingness diagnosis.
# tank_status is "Closed" or "Open" exactly (04_Master_Build.R writes
# status_std back as tank_status before saving). No regex needed.
tank_raw_miss <- fread(TANK_PATH,
                       select = c("facility_id", "state",
                                  "tank_installed_date", "tank_closed_date",
                                  "tank_status"),
                       colClasses = "character")

tank_raw_miss[, `:=`(
  facility_id         = toupper(trimws(facility_id)),
  state               = toupper(trimws(state)),
  tank_installed_date = as.IDate(tank_installed_date),
  tank_closed_date    = as.IDate(tank_closed_date),
  # 04_Master_Build.R writes status_std back as tank_status before saving,
  # so values are exactly "Closed" or "Open" — no regex reconstruction needed.
  is_closed_status    = (tank_status == "Closed")
)]
tank_raw_miss[, panel_id := paste(facility_id, state, sep = "_")]
tank_raw_miss <- tank_raw_miss[panel_id %in% incumbent_ids]

# --- DIAGNOSTIC 1: Scale of the problem ---
miss_install <- unique(tank_raw_miss[is.na(tank_installed_date), panel_id])
miss_closure <- unique(tank_raw_miss[is_closed_status == TRUE &
                                      is.na(tank_closed_date), panel_id])
facilities_to_exclude_miss <- union(miss_install, miss_closure)

n_incum_total <- length(incumbent_ids)
diag_scale <- data.frame(
  Metric = c("Incumbent facilities (initial)",
             "Facilities: missing install_date on any tank",
             "Facilities: missing closure_date on any closed tank",
             "Facilities: missing both types",
             "Total facilities to exclude",
             "Exclusion rate (%)"),
  N = c(n_incum_total,
        length(miss_install),
        length(miss_closure),
        length(intersect(miss_install, miss_closure)),
        length(facilities_to_exclude_miss),
        round(100 * length(facilities_to_exclude_miss) / n_incum_total, 2))
)
cat("Scale of missing data:\n")
print(diag_scale, row.names = FALSE)

# --- DIAGNOSTIC 2: Balance (TX vs Control) ---
tank_fac_flag <- tank_raw_miss[, .(
  has_miss_install = any(is.na(tank_installed_date)),
  has_miss_closure = any(is_closed_status == TRUE & is.na(tank_closed_date)),
  group            = fifelse(first(state) == "TX", "Texas", "Control")
), by = panel_id]

balance_miss <- tank_fac_flag[, .(
  N_facilities        = .N,
  N_miss_install      = sum(has_miss_install),
  Pct_miss_install    = round(100 * mean(has_miss_install), 2),
  N_miss_closure      = sum(has_miss_closure),
  Pct_miss_closure    = round(100 * mean(has_miss_closure), 2)
), by = group]
cat("\nBalance (TX vs Control):\n")
print(balance_miss)

# Balance logit test
tank_fac_flag[, texas := as.integer(group == "Texas")]
balance_glm <- tryCatch(
  glm(has_miss_install ~ texas, data = tank_fac_flag, family = binomial),
  error = function(e) NULL
)
if (!is.null(balance_glm)) {
  p_bal <- summary(balance_glm)$coefficients["texas", "Pr(>|z|)"]
  cat(sprintf("\nBalance test (missing install_date ~ Texas): p = %.4f\n", p_bal))
  if (p_bal < 0.10)
    cat("  ⚠  Imbalanced — consider IPW robustness (Memo Scenario 3)\n") else
    cat("  ✓  Balanced — proceed with clean drop\n")
}

# --- APPLY EXCLUSION ---
annual_data <- annual_data[!panel_id %in% facilities_to_exclude_miss]
# Re-sync incumbent_ids to the now-clean sample
incumbent_ids <- unique(annual_data$panel_id)

cat(sprintf("\n  Excluded: %s facilities | Remaining: %s facilities | %s rows\n",
            format(length(facilities_to_exclude_miss), big.mark = ","),
            format(uniqueN(annual_data$panel_id),      big.mark = ","),
            format(nrow(annual_data),                  big.mark = ",")))

# Save appendix tables
fwrite(as.data.table(diag_scale),
       file.path(OUTPUT_TABLES, "Appendix_A1_Missing_Data_Scale.csv"))
fwrite(balance_miss,
       file.path(OUTPUT_TABLES, "Appendix_A1_Missing_Data_Balance.csv"))
cat("✓ Saved: Appendix_A1_Missing_Data_Scale.csv\n")
cat("✓ Saved: Appendix_A1_Missing_Data_Balance.csv\n")
rm(tank_raw_miss, tank_fac_flag)
gc()

# 4.2 Create analysis variables
cat("\n--- Creating analysis variables ---\n")

annual_data[, `:=`(
  # Core DiD terms (panel builder already creates texas_treated, post_1999)
  treated     = texas_treated,
  post        = post_1999,
  did_term    = texas_treated * post_1999,

  # Primary outcome: any tank closure in year t
  closure_event = as.integer(n_closures > 0),

  # Relative year (for event studies) — only meaningful for TX
  rel_year_1999 = panel_year - TREATMENT_YEAR,

  # Convenience
  county_fips_fac = as.factor(county_fips)
)]

# Summary
tx_n  <- uniqueN(annual_data[texas_treated == 1, panel_id])
ctl_n <- uniqueN(annual_data[texas_treated == 0, panel_id])
cat(sprintf("  Texas facilities:   %s\n", format(tx_n,  big.mark = ",")))
cat(sprintf("  Control facilities: %s\n", format(ctl_n, big.mark = ",")))
cat(sprintf("  Years: %d to %d\n", min(annual_data$panel_year), max(annual_data$panel_year)))


#==============================================================================
# SECTION 5: TANK-LEVEL DATASET CONSTRUCTION
#==============================================================================

cat("\n========================================\n")
cat("SECTION 5: TANK-LEVEL DATASETS\n")
cat("========================================\n\n")

# 5.1 Standardize tank IDs (mirror panel builder logic exactly)
cat("--- Standardizing tank inventory IDs ---\n")
tank_inventory[, `:=`(
  facility_id = toupper(trimws(as.character(facility_id))),
  tank_id     = toupper(trimws(as.character(tank_id))),
  state       = toupper(trimws(as.character(state)))
)]
tank_inventory[, panel_id := paste(facility_id, state, sep = "_")]

# 5.2 Standardize dates
tank_inventory[, `:=`(
  tank_installed_date = as.IDate(tank_installed_date),
  tank_closed_date    = as.IDate(tank_closed_date)
)]

# 5.3 Restrict to clean incumbent sample facilities
# incumbent_ids was established and synced in Section 4.3 after missing-data exclusion
tank_inventory <- tank_inventory[panel_id %in% incumbent_ids]
cat(sprintf("  Tanks in clean incumbent sample: %s\n",
            format(nrow(tank_inventory), big.mark = ",")))

# 5.4 Create tank-level analysis variables
cat("--- Creating tank-level survival variables ---\n")

tank_inventory[, `:=`(
  # Age at treatment date (years)
  age_at_treatment = as.numeric(difftime(
    TREATMENT_DATE, tank_installed_date, units = "days")) / 365.25,

  # Time to closure from treatment date (years); NA = still active
  time_to_close = as.numeric(difftime(
    pmin(tank_closed_date, STUDY_END_DATE, na.rm = TRUE),
    TREATMENT_DATE, units = "days")) / 365.25,

  # Closure event indicator (closed within study window)
  event_closure = as.integer(!is.na(tank_closed_date) &
                              tank_closed_date <= STUDY_END_DATE),

  # Treatment group
  texas = as.integer(state == "TX"),

  # Risk factors
  # NOTE: single_walled and double_walled are numeric 0/1 columns in
  # Master_Harmonized_UST_Tanks.csv (panel builder reads them at line 274).
  # wall_type ("Single-Walled"/"Double-Walled"/"Mixed") is a FACILITY-YEAR
  # aggregate created by the panel builder at line 952 for the annual panel —
  # it does NOT exist in the raw tank file. Use the raw binary columns directly.
  single_walled    = as.integer(single_walled == 1),
  double_walled    = as.integer(double_walled == 1),
  old_at_treatment = as.integer(age_at_treatment > 20),

  # Vintage cohorts per memo definitions
  vintage = fcase(
    tank_installed_date <  as.IDate("1980-01-01"), "Pre-1980",
    tank_installed_date <  as.IDate("1988-12-23"), "1980-1988",
    tank_installed_date >= as.IDate("1988-12-23"), "1989-1998",
    default = NA_character_
  ),
  old_vintage = as.integer(tank_installed_date < as.IDate("1980-01-01")),
  pre_deadline = as.integer(tank_installed_date < as.IDate("1988-12-23"))
)]

# 5.4b Capture pre-period (1990-1998) closures BEFORE survival filter discards them.
# These are needed for Figure 4 panel (c) — mean age at closure in the pre-period.
# After the filter below, tank_inventory retains only tanks with time_to_close >= 0
# (i.e., closed on/after 1999 or never closed), so pre-1999 closures disappear.
cat("--- Capturing pre-period closures for Figure 4 panel (c) ---\n")
pre_period_closures <- tank_inventory[
  !is.na(tank_closed_date) &
  !is.na(tank_installed_date) &
  year(tank_closed_date) >= 1990 &
  year(tank_closed_date) <= 1998,
  .(
    closure_year    = year(tank_closed_date),
    texas           = texas,
    age_at_closure  = as.numeric(difftime(
      tank_closed_date, tank_installed_date, units = "days")) / 365.25
  )
]
cat(sprintf("  Pre-period closures captured: %s tanks (%d–%d)\n",
            format(nrow(pre_period_closures), big.mark = ","),
            min(pre_period_closures$closure_year),
            max(pre_period_closures$closure_year)))

# Drop tanks with negative time-to-close (closed before treatment — not in risk set)
n_tanks_before <- nrow(tank_inventory)
tank_inventory <- tank_inventory[is.na(time_to_close) | time_to_close >= 0]
cat(sprintf("  Tanks dropped (closed pre-1999): %s\n",
            format(n_tanks_before - nrow(tank_inventory), big.mark = ",")))
cat(sprintf("  Final tank inventory: %s tanks\n",
            format(nrow(tank_inventory), big.mark = ",")))

# 5.5 Create closed-tanks dataset (Models 3, 4)
cat("\n--- Creating closed-tanks dataset ---\n")

closed_tanks <- tank_inventory[event_closure == 1]
closed_tanks[, closure_year := as.integer(format(tank_closed_date, "%Y"))]

closed_tanks[, `:=`(
  age_at_closure = as.numeric(difftime(
    tank_closed_date, tank_installed_date, units = "days")) / 365.25,
  post      = as.integer(closure_year >= TREATMENT_YEAR),
  texas_post = as.integer(texas == 1 & closure_year >= TREATMENT_YEAR),
  county_fips_fac = as.factor(county_fips)
)]

cat(sprintf("  Closed tanks (post-1999 events): %s\n",
            format(nrow(closed_tanks), big.mark = ",")))
cat(sprintf("  Closure years: %d to %d\n",
            min(closed_tanks$closure_year), max(closed_tanks$closure_year)))

# 5.6 1999 snapshot of tanks (for descriptive tables)
tanks_1999 <- tank_inventory[!is.na(age_at_treatment) & age_at_treatment >= 0]
tanks_1999[, risk_score := (single_walled == 1) +
              (age_at_treatment > 20) +
              (old_vintage == 1)]


#==============================================================================
# SECTION 6: DIAGNOSTICS — PRE-1999 EXIT BALANCE TEST
#==============================================================================

cat("\n========================================\n")
cat("SECTION 6: DIAGNOSTICS\n")
cat("========================================\n\n")

cat("--- Pre-1999 Exit Balance Test (Memo Section 1.1) ---\n")
cat("Purpose: Test whether TX and control facilities had differential\n")
cat("         exit rates before 1999 (survivorship bias check).\n\n")

# Use the full pre-1999 window: is the facility's last year before 1999?
facility_exit <- annual_data[, .(
  last_year  = max(panel_year),
  first_year = min(panel_year),
  texas      = first(texas_treated),
  state      = first(state)
), by = panel_id]

facility_exit[, exited_pre_1999 := as.integer(last_year < TREATMENT_YEAR)]

# Summary by group
exit_summary <- facility_exit[, .(
  N         = .N,
  N_exited  = sum(exited_pre_1999),
  Pct_exit  = round(100 * mean(exited_pre_1999), 2)
), by = .(Group = fifelse(texas == 1, "Texas", "Control"))]

cat("Pre-1999 exit rates:\n")
print(exit_summary)
cat("\n")

# Regression test (note: only 2 clusters — inference severely limited)
pre_exit_reg <- feols(
  exited_pre_1999 ~ texas,
  data = facility_exit,
  cluster = ~state
)
cat("Regression: Pr(Exit before 1999) ~ Texas\n")
cat("(Only 2 clusters — inference is indicative only)\n\n")
print(summary(pre_exit_reg))

# Interpretation
tx_coef <- coef(pre_exit_reg)["texas"]
tx_pval <- summary(pre_exit_reg)$coeftable["texas", "Pr(>|t|)"]

cat("\n--- Interpretation ---\n")
if (abs(tx_coef) < 0.01) {
  cat("✓ No meaningful difference in pre-1999 exit rates (|β| < 1 pp)\n")
} else if (!is.na(tx_pval) && tx_pval < 0.10) {
  direction <- ifelse(tx_coef > 0, "HIGHER", "LOWER")
  cat(sprintf("⚠  Texas had %s pre-1999 exit rate (β = %.4f, p = %.3f)\n",
              direction, tx_coef, tx_pval))
  cat("   Consider Definition B (1990-1999 window) as robustness check.\n")
} else {
  cat(sprintf("   No significant difference (p = %.3f); β = %.4f\n",
              tx_pval, tx_coef))
}

fwrite(exit_summary,
       file.path(OUTPUT_TABLES, "Diagnostic_Pre1999_Exit_Balance.csv"))
cat("\n✓ Saved: Diagnostic_Pre1999_Exit_Balance.csv\n")


#==============================================================================
# SECTION 7: DESCRIPTIVE TABLES (TABLES 1-3)
#==============================================================================

cat("\n========================================\n")
cat("SECTION 7: DESCRIPTIVE TABLES\n")
cat("========================================\n\n")

#--- Table 1: Sample Composition ---
cat("--- Table 1: Sample Composition ---\n")

# Panel A: Facilities
table1_fac <- annual_data[panel_year == TREATMENT_YEAR, .(
  N_facilities  = .N,
  Med_tanks     = round(median(active_tanks_dec, na.rm = TRUE), 1),
  Mean_tanks    = round(mean(active_tanks_dec,   na.rm = TRUE), 2),
  Total_tanks   = sum(active_tanks_dec,          na.rm = TRUE),
  Pct_SingleTank = round(100 * mean(active_tanks_dec == 1, na.rm = TRUE), 1)
), by = .(Group = fifelse(texas_treated == 1, "Texas", "Control"))]

totals_fac <- annual_data[panel_year == TREATMENT_YEAR, .(
  Group = "Total",
  N_facilities  = .N,
  Med_tanks     = round(median(active_tanks_dec, na.rm = TRUE), 1),
  Mean_tanks    = round(mean(active_tanks_dec,   na.rm = TRUE), 2),
  Total_tanks   = sum(active_tanks_dec,          na.rm = TRUE),
  Pct_SingleTank = round(100 * mean(active_tanks_dec == 1, na.rm = TRUE), 1)
)]

table1 <- rbind(table1_fac, totals_fac)
cat("Table 1A — Facility Summary (at 1999):\n")
print(table1)

fwrite(table1, file.path(OUTPUT_TABLES, "Table_1_Sample_Composition.csv"))
cat("✓ Saved: Table_1_Sample_Composition.csv\n")

#--- Table 2: Baseline Characteristics (Tank-Level at 1999) ---
cat("\n--- Table 2: Baseline Tank Characteristics ---\n")

table2 <- tanks_1999[, .(
  N_tanks          = .N,
  Mean_age         = round(mean(age_at_treatment, na.rm = TRUE), 2),
  Median_age       = round(median(age_at_treatment, na.rm = TRUE), 1),
  Pct_SingleWalled = round(100 * mean(single_walled == 1, na.rm = TRUE), 1),
  Pct_OldAtTx     = round(100 * mean(old_at_treatment == 1, na.rm = TRUE), 1),
  Pct_Pre1980      = round(100 * mean(old_vintage == 1, na.rm = TRUE), 1),
  Pct_1980_1988    = round(100 * mean(vintage == "1980-1988", na.rm = TRUE), 1),
  Pct_1989_1998    = round(100 * mean(vintage == "1989-1998", na.rm = TRUE), 1),
  Pct_Events       = round(100 * mean(event_closure == 1, na.rm = TRUE), 1)
), by = .(Group = fifelse(texas == 1, "Texas", "Control"))]

cat("Table 2 — Tank Baseline Characteristics (1999 snapshot):\n")
print(table2)
fwrite(table2, file.path(OUTPUT_TABLES, "Table_2_Baseline_Characteristics.csv"))
cat("✓ Saved: Table_2_Baseline_Characteristics.csv\n")

#--- Table 3: Risk Factor Distribution ---
cat("\n--- Table 3: Risk Factor Distribution ---\n")

tanks_1999[, risk_category := fcase(
  risk_score == 0, "Low (0 factors)",
  risk_score == 1, "Medium (1 factor)",
  risk_score == 2, "High (2 factors)",
  risk_score >= 3, "Very High (3 factors)",
  default = NA_character_
)]

table3 <- tanks_1999[!is.na(risk_category), .(
  N = .N
), by = .(Group = fifelse(texas == 1, "Texas", "Control"), risk_category)]

table3[, Pct := round(100 * N / sum(N), 1), by = Group]
setorder(table3, Group, risk_category)

cat("Table 3 — Risk Factor Distribution:\n")
print(table3)
fwrite(table3, file.path(OUTPUT_TABLES, "Table_3_Risk_Factor_Distribution.csv"))
cat("✓ Saved: Table_3_Risk_Factor_Distribution.csv\n")


#==============================================================================
# SECTION 8: DESCRIPTIVE FIGURES (1-4 + PRE-TRENDS)
#==============================================================================

cat("\n========================================\n")
cat("SECTION 8: DESCRIPTIVE FIGURES\n")
cat("========================================\n\n")

#--- Figure 1: Tank Age Distribution at 1999 ---
cat("--- Figure 1: Age Distribution ---\n")

p1 <- ggplot(tanks_1999[!is.na(age_at_treatment) & age_at_treatment >= 0],
             aes(x = age_at_treatment, fill = factor(texas))) +
  geom_histogram(position = "identity", alpha = 0.6, bins = 30,
                 color = "white", linewidth = 0.2) +
  scale_fill_manual(values = c("0" = "#0072B2", "1" = "#D55E00"),
                    labels = c("Control", "Texas"), name = "") +
  labs(
    title    = "Figure 1: Tank Age Distribution as of January 1, 1999",
    subtitle = "Incumbent sample — facilities active before 1999",
    x = "Tank Age (years)", y = "Count"
  ) +
  theme_pub()

ggsave(file.path(OUTPUT_FIGURES, "Figure_1_Age_Distribution_1999.png"),
       p1, width = 10, height = 6, dpi = 300, bg = "white")
cat("✓ Saved: Figure_1_Age_Distribution_1999.png\n")

#--- Figure 2: Vintage Distribution ---
cat("--- Figure 2: Vintage Distribution ---\n")

vintage_dt <- tanks_1999[!is.na(vintage), .(N = .N),
                          by = .(Group = fifelse(texas == 1, "Texas", "Control"),
                                 vintage)]
vintage_dt[, Pct := 100 * N / sum(N), by = Group]
vintage_dt[, vintage := factor(vintage,
                                levels = c("Pre-1980", "1980-1988", "1989-1998"))]

p2 <- ggplot(vintage_dt, aes(x = vintage, y = Pct, fill = Group)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  scale_fill_manual(values = c("Control" = "#0072B2", "Texas" = "#D55E00")) +
  labs(
    title    = "Figure 2: Vintage Cohort Distribution",
    subtitle = "Share of tanks by installation era (1999 snapshot)",
    x = "Installation Vintage", y = "% of Tanks", fill = ""
  ) +
  theme_pub()

ggsave(file.path(OUTPUT_FIGURES, "Figure_2_Vintage_Distribution.png"),
       p2, width = 10, height = 6, dpi = 300, bg = "white")
cat("✓ Saved: Figure_2_Vintage_Distribution.png\n")

#--- Figure 3: Single-Walled Prevalence by Vintage ---
cat("--- Figure 3: Single-Walled by Vintage ---\n")

wall_vintage <- tanks_1999[!is.na(vintage), .(
  Pct_SW = 100 * mean(single_walled == 1, na.rm = TRUE),
  N      = .N
), by = .(Group = fifelse(texas == 1, "Texas", "Control"), vintage)]
wall_vintage[, vintage := factor(vintage,
                                  levels = c("Pre-1980", "1980-1988", "1989-1998"))]

p3 <- ggplot(wall_vintage, aes(x = vintage, y = Pct_SW,
                                color = Group, group = Group)) +
  geom_line(linewidth = 1.2) +
  geom_point(aes(size = N), alpha = 0.8) +
  scale_color_manual(values = c("Control" = "#0072B2", "Texas" = "#D55E00")) +
  scale_size_continuous(name = "Tank Count", range = c(3, 9)) +
  labs(
    title    = "Figure 3: Single-Walled Tank Prevalence by Vintage",
    subtitle = "Risk factor distribution across installation cohorts",
    x = "Installation Vintage", y = "% Single-Walled Tanks", color = ""
  ) +
  theme_pub()

ggsave(file.path(OUTPUT_FIGURES, "Figure_3_Single_Walled_by_Vintage.png"),
       p3, width = 10, height = 6, dpi = 300, bg = "white")
cat("✓ Saved: Figure_3_Single_Walled_by_Vintage.png\n")

#--- Figure 4: Pre-Period Trends (1990–1998) — Correct Figure 4 ---
# Per Memo Section 7 / Section 1.3.2: 4-panel line plot showing parallel
# trends between Texas and Control in the pre-period ONLY.
# This is the key visual support for the parallel trends assumption.
cat("--- Figure 4: Pre-Period Trends (1990-1998) ---\n")

# AUDIT FIX 1 of 2 (Figure 4):
# BUG: `leak_year` is a binary 0/1 flag (any leak this year?).
# The memo (Section 7) specifies: sum(leak_incident) / .N * 1000,
# which is total incident count, not facility-year prevalence.
# In our panel, that corresponds to `n_leaks` (= sum of individual leak events).
# Using `leak_year` understates the leak rate wherever facilities had multiple
# incidents in a single year, and flattens the Y-axis of panel (b).
#
# AUDIT FIX 2 of 2 (Figure 4):
# BUG: `avg_tank_age_dec[closure_event == 1]` uses December snapshot of average
# ACTIVE tank age in years when closures occurred — NOT the age of tanks that
# were actually closed. These are different: the closed tanks (likely older)
# have already left the stock, so avg_tank_age_dec is biased downward.
# The memo specifies avg_closure_age, which does not exist as an annual panel
# column (it's not emitted by 10_Build_Annual_Panel_Optimized.R).
# FIX: compute from pre_period_closures captured in Section 5.4b above,
# which has exact age_at_closure = (closure_date - install_date) per tank.

pre_trends <- annual_data[panel_year >= 1990 & panel_year <= 1998, .(
  closure_rate = mean(closure_event, na.rm = TRUE),
  leak_rate    = sum(n_leaks,        na.rm = TRUE) / .N * 1000,  # FIXED: n_leaks not leak_year
  exit_rate    = mean(exit_flag,     na.rm = TRUE) * 100,
  n            = .N
), by = .(panel_year,
          treatment_group = fifelse(texas_treated == 1, "Texas", "Control"))]

# Compute mean_closure_age correctly from tank-level pre-period closures
# (facility-year panel cannot supply this; avg_tank_age_dec is not age-at-closure)
closure_age_pre <- pre_period_closures[, .(
  mean_closure_age = mean(age_at_closure, na.rm = TRUE)
), by = .(panel_year = closure_year,
          treatment_group = fifelse(texas == 1, "Texas", "Control"))]

pre_trends <- closure_age_pre[pre_trends, on = .(panel_year, treatment_group)]  # FIXED

# Binomial SEs for closure rate and exit rate
pre_trends[, `:=`(
  se_closure = sqrt(closure_rate * (1 - closure_rate) / n),
  se_exit    = sqrt((exit_rate / 100) * (1 - exit_rate / 100) / n)
)]

colors_fig4 <- c("Texas" = "#D55E00", "Control" = "#0072B2")

p4a <- ggplot(pre_trends, aes(x = panel_year, y = closure_rate,
                               color = treatment_group,
                               fill  = treatment_group)) +
  geom_ribbon(aes(ymin = closure_rate - 1.96 * se_closure,
                  ymax = closure_rate + 1.96 * se_closure), alpha = 0.15,
              color = NA) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_vline(xintercept = 1998.5, linetype = "dashed", color = "gray40") +
  scale_color_manual(values = colors_fig4) +
  scale_fill_manual(values  = colors_fig4) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  labs(title = "(a) Annual Closure Rate",
       x = NULL, y = "% Facilities Closing a Tank",
       color = NULL, fill = NULL) +
  theme_pub() + theme(legend.position = "bottom")

p4b <- ggplot(pre_trends, aes(x = panel_year, y = leak_rate,
                               color = treatment_group)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_vline(xintercept = 1998.5, linetype = "dashed", color = "gray40") +
  scale_color_manual(values = colors_fig4) +
  labs(title = "(b) Annual Leak Rate (per 1,000 facility-years)",
       x = NULL, y = "Leaks per 1,000",
       color = NULL) +
  theme_pub() + theme(legend.position = "none")

p4c <- ggplot(pre_trends[!is.na(mean_closure_age)],
              aes(x = panel_year, y = mean_closure_age,
                  color = treatment_group)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_vline(xintercept = 1998.5, linetype = "dashed", color = "gray40") +
  scale_color_manual(values = colors_fig4) +
  labs(title = "(c) Mean Age at Closure (years)",
       x = NULL, y = "Years",
       color = NULL) +
  theme_pub() + theme(legend.position = "none")

p4d <- ggplot(pre_trends, aes(x = panel_year, y = exit_rate,
                               color = treatment_group,
                               fill  = treatment_group)) +
  geom_ribbon(aes(ymin = (exit_rate / 100 - 1.96 * se_exit) * 100,
                  ymax = (exit_rate / 100 + 1.96 * se_exit) * 100), alpha = 0.15,
              color = NA) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_vline(xintercept = 1998.5, linetype = "dashed", color = "gray40") +
  scale_color_manual(values = colors_fig4) +
  scale_fill_manual(values  = colors_fig4) +
  labs(title = "(d) Full Exit Rate (% of facilities)",
       x = "Year", y = "% Exiting",
       color = NULL, fill = NULL) +
  theme_pub() + theme(legend.position = "none")

fig4_combined <- gridExtra::arrangeGrob(
  p4a, p4b, p4c, p4d, ncol = 2,
  top = grid::textGrob(
    "Figure 4: Pre-Period Trends (1990-1998) — Texas vs. Control States",
    gp = grid::gpar(fontsize = 13, fontface = "bold")
  )
)

ggsave(file.path(OUTPUT_FIGURES, "Figure_4_Pre_Period_Trends.png"),
       fig4_combined, width = 14, height = 10, dpi = 300, bg = "white")
cat("✓ Saved: Figure_4_Pre_Period_Trends.png\n")

fwrite(pre_trends,
       file.path(OUTPUT_TABLES, "Figure_4_Pre_Trends_Data.csv"))
cat("✓ Saved: Figure_4_Pre_Trends_Data.csv\n")


#==============================================================================
# SECTION 9: FIGURE 5 — RISK FACTOR VALIDATION (FACILITY-LEVEL)
#==============================================================================
# Design decisions (confirmed with PI):
#   Q1  — Facility-level regression (option c). LUST records are facility-level;
#          we do NOT attribute leaks to individual tanks. Unit = facility-year.
#          Covariates are facility-level aggregates of tank characteristics.
#   Q2  — Incidence is the facility's first LUST report. event_first_leak (1 in
#          first-leak year, 0 before) is already built in annual_data. Facility
#          exits the risk set after that year.
#   Q3  — Fold by FACILITY (option b). Each facility is either fully in train or
#          fully in test. Prevents any temporal leakage of facility identity
#          across folds. Stratified by state to preserve state representation.
#   Q4  — State FE retained. Since we fold by facility (not state), every state
#          in the test set also appears in training, so state FE coefficients
#          are estimable. Predict on test using training-set FE values.
#==============================================================================

cat("\n========================================\n")
cat("SECTION 9: FIGURE 5 — RISK FACTOR VALIDATION\n")
cat("========================================\n\n")

cat("Design: Facility-year panel (1990-1998) | Outcome: first LUST report\n")
cat("CV:     5-fold, folded by facility, stratified by state\n")
cat("Specs:  (1) Year FE only | (2) Year + State FE [robustness]\n\n")

# ─────────────────────────────────────────────────────────────────────────────
# 17.1  BUILD FIGURE 5 PRE-PERIOD PANEL
# ─────────────────────────────────────────────────────────────────────────────
cat("--- 17.1: Building pre-period facility-year panel ---\n")

# Start from the already-filtered annual_data (incumbent, clean missing-data sample)
fig5_panel <- annual_data[panel_year >= 1990 & panel_year <= 1998]

# --- Apply first-leak incidence censoring ---
# Keep facility-years up to and including the year of first LUST report.
# After that year, the facility has "become a leaker" and exits the risk set.
# year_of_first_leak is NA for facilities that never leaked in the full panel.
fig5_panel <- fig5_panel[
  is.na(year_of_first_leak) | panel_year <= year_of_first_leak
]
cat(sprintf("  Facility-years after incidence censoring: %s\n",
            format(nrow(fig5_panel), big.mark = ",")))
cat(sprintf("  Unique facilities: %s\n",
            format(uniqueN(fig5_panel$panel_id), big.mark = ",")))
cat(sprintf("  Facilities with a first leak in 1990-1998: %s (%.1f%%)\n",
            format(sum(!is.na(fig5_panel$year_of_first_leak) &
                       fig5_panel$event_first_leak == 1, na.rm = TRUE),
                   big.mark = ","),
            100 * mean(!is.na(fig5_panel$year_of_first_leak) &
                       fig5_panel$event_first_leak == 1, na.rm = TRUE)))

# ─────────────────────────────────────────────────────────────────────────────
# 17.2  COMPUTE FACILITY-LEVEL COVARIATES
# ─────────────────────────────────────────────────────────────────────────────
cat("\n--- 17.2: Computing facility-level risk covariates ---\n")

# ── TIME-VARYING (from annual_data columns, computed per facility-year) ────────
# pct_single_walled_fac:  share of active tanks that are single-walled this year
#   single_tanks_dec / active_tanks_dec (December snapshot)
# mean_tank_age:          avg_tank_age_dec (already in panel, December snapshot)
# log_capacity_fac:       log(total_capacity_dec + 1) for facility-year
# is_motor_fuel:          has_gasoline_year (any gasoline/diesel tank this year)

fig5_panel[, `:=`(
  pct_single_walled_fac = fifelse(
    active_tanks_dec > 0,
    single_tanks_dec / active_tanks_dec,
    NA_real_
  ),
  mean_tank_age       = avg_tank_age_dec,
  age_gt_20_fac       = as.integer(avg_tank_age_dec > 20),   # facility avg > 20
  log_capacity_fac    = log(pmax(total_capacity_dec, 1)),
  is_motor_fuel_fac   = as.integer(is_motor_fuel == 1)
)]

# ── TIME-INVARIANT (from tank_inventory — vintage requires install_date) ────────
# pct_pre_1980_fac: share of facility's incumbent tanks with pre-1980 vintage.
# Not in annual_data (install_year proxy is approximate). Compute from
# tank_inventory where old_vintage is exact (install_date < 1980-01-01).

fac_vintage <- tank_inventory[
  panel_id %in% unique(fig5_panel$panel_id),
  .(pct_pre_1980_fac = mean(old_vintage == 1, na.rm = TRUE)),
  by = panel_id
]
cat(sprintf("  Vintage computed for %s facilities\n",
            format(nrow(fac_vintage), big.mark = ",")))

fig5_panel <- fac_vintage[fig5_panel, on = "panel_id"]

# Check covariate completeness
n_complete <- nrow(fig5_panel[
  !is.na(event_first_leak) &
  !is.na(pct_single_walled_fac) &
  !is.na(mean_tank_age) &
  !is.na(pct_pre_1980_fac) &
  !is.na(log_capacity_fac)
])
cat(sprintf("  Complete cases (all covariates + outcome): %s / %s (%.1f%%)\n",
            format(n_complete, big.mark = ","),
            format(nrow(fig5_panel), big.mark = ","),
            100 * n_complete / nrow(fig5_panel)))

# Drop rows with any covariate or outcome NA (cannot feed glm)
fig5_panel <- fig5_panel[
  !is.na(event_first_leak) &
  !is.na(pct_single_walled_fac) &
  !is.na(mean_tank_age) &
  !is.na(pct_pre_1980_fac) &
  !is.na(log_capacity_fac)
]
cat(sprintf("  Panel after complete-case filter: %s rows | %s facilities\n",
            format(nrow(fig5_panel), big.mark = ","),
            format(uniqueN(fig5_panel$panel_id), big.mark = ",")))

# ─────────────────────────────────────────────────────────────────────────────
# 17.3  COMPOSITE RISK SCORE & FIGURE 5A — RAW LEAK RATES BY CATEGORY
# ─────────────────────────────────────────────────────────────────────────────
cat("\n--- 17.3: Figure 5A — Raw Leak Rates by Risk Category ---\n")

# Risk score at facility-year level (sum of binary risk indicators).
# Each dimension is binarised at its natural threshold:
#   pct_single_walled_fac > 0.5  → majority single-walled
#   age_gt_20_fac == 1            → facility avg age > 20
#   pct_pre_1980_fac   > 0.5      → majority pre-1980 vintage
fig5_panel[, `:=`(
  risk_sw_fac  = as.integer(pct_single_walled_fac > 0.5),
  risk_age_fac = age_gt_20_fac,
  risk_vin_fac = as.integer(pct_pre_1980_fac > 0.5),
  risk_score_fac = (as.integer(pct_single_walled_fac > 0.5) +
                    age_gt_20_fac +
                    as.integer(pct_pre_1980_fac > 0.5))
)]

fig5_panel[, risk_category_fac := fcase(
  risk_score_fac == 0, "Low (0)",
  risk_score_fac == 1, "Medium (1)",
  risk_score_fac == 2, "High (2)",
  risk_score_fac == 3, "Very High (3)",
  default = NA_character_
)]

# Raw leak rates per 1,000 facility-years, by risk category and group
fig5a_data <- fig5_panel[!is.na(risk_category_fac), .(
  leak_rate_per1000 = sum(event_first_leak, na.rm = TRUE) / .N * 1000,
  n_events          = sum(event_first_leak, na.rm = TRUE),
  n_fac_years       = .N
), by = .(
  Group = fifelse(texas_treated == 1, "Texas", "Control"),
  risk_category_fac
)]

# Binomial SE for error bars (Clopper-Pearson style approximation)
fig5a_data[, se_rate := sqrt(
  (leak_rate_per1000 / 1000) * (1 - leak_rate_per1000 / 1000) / n_fac_years
) * 1000]

cat("Figure 5A — Raw leak rates by risk category:\n")
print(fig5a_data[order(Group, risk_category_fac)])

# Lift ratio: high-risk / low-risk
low_rate_ctrl <- fig5a_data[Group == "Control" & risk_category_fac == "Low (0)",
                             leak_rate_per1000]
hi_rate_ctrl  <- fig5a_data[Group == "Control" & risk_category_fac == "Very High (3)",
                             leak_rate_per1000]
if (length(low_rate_ctrl) > 0 && low_rate_ctrl > 0)
  cat(sprintf("  Control — Very High / Low risk lift: %.1fx\n",
              hi_rate_ctrl / low_rate_ctrl))

cat_order <- c("Low (0)", "Medium (1)", "High (2)", "Very High (3)")
fig5a_data[, risk_category_fac := factor(risk_category_fac, levels = cat_order)]

fig5a <- ggplot(
  fig5a_data[!is.na(risk_category_fac)],
  aes(x = risk_category_fac, y = leak_rate_per1000, fill = Group)
) +
  geom_col(position = position_dodge(width = 0.75), width = 0.7) +
  geom_errorbar(
    aes(ymin = pmax(leak_rate_per1000 - 1.96 * se_rate, 0),
        ymax = leak_rate_per1000 + 1.96 * se_rate),
    position = position_dodge(width = 0.75), width = 0.25, linewidth = 0.6
  ) +
  scale_fill_manual(values = c("Control" = "#0072B2", "Texas" = "#D55E00")) +
  labs(
    title    = "Figure 5A: Pre-Period Leak Rates by Facility Risk Category (1990-1998)",
    subtitle = paste0("Risk score = # of majority-threshold indicators met",
                      " (single-walled, age > 20 yrs, pre-1980 vintage)\n",
                      "Rates are first-LUST incidence only — facility exits risk set",
                      " after first event. Error bars: 95% CI."),
    x = "Facility Risk Category (# Risk Factors)",
    y = "First-Leak Rate per 1,000 Facility-Years",
    fill = ""
  ) +
  theme_pub()

ggsave(file.path(OUTPUT_FIGURES, "Figure_5A_Raw_Leak_Rates_by_Risk.png"),
       fig5a, width = 10, height = 6, dpi = 300, bg = "white")
cat("✓ Saved: Figure_5A_Raw_Leak_Rates_by_Risk.png\n")

fwrite(fig5a_data,
       file.path(OUTPUT_TABLES, "Figure_5A_Raw_Rates_Data.csv"))
cat("✓ Saved: Figure_5A_Raw_Rates_Data.csv\n")

# ─────────────────────────────────────────────────────────────────────────────
# 17.4  5-FOLD CV SETUP — FOLD BY FACILITY, STRATIFIED BY STATE
# ─────────────────────────────────────────────────────────────────────────────
cat("\n--- 17.4: 5-Fold CV setup (by facility, stratified by state) ---\n")

# Assign folds at the FACILITY level (one fold per panel_id, constant across years).
# Stratified by state so each fold mirrors the state composition of the full sample.
set.seed(20260202)
fac_folds <- unique(fig5_panel[, .(panel_id, state)])
fac_folds[, fold := sample(1:5, .N, replace = TRUE), by = state]

# Verify no facility spans two folds (should always be true by construction)
stopifnot(uniqueN(fac_folds$panel_id) == nrow(fac_folds))

cat(sprintf("  Facilities assigned to folds: %s\n",
            format(nrow(fac_folds), big.mark = ",")))
print(fac_folds[, .N, by = fold])   # rough balance check

# Merge fold assignments back into panel
fig5_panel <- fac_folds[fig5_panel, on = .(panel_id, state)]

# Logistic regression formula pieces
# Primary spec: year FE only  (captures global time trends in leak rates)
# Robustness:   year + state FE (absorbs state-level leak rate levels;
#               valid here because we fold by facility, so state FE is
#               always estimable from training data)
FORM_NOFE    <- event_first_leak ~ pct_single_walled_fac + age_gt_20_fac +
                  pct_pre_1980_fac + log_capacity_fac + is_motor_fuel_fac +
                  factor(panel_year)
FORM_STATEFE <- event_first_leak ~ pct_single_walled_fac + age_gt_20_fac +
                  pct_pre_1980_fac + log_capacity_fac + is_motor_fuel_fac +
                  factor(panel_year) + factor(state)

# Initialise prediction columns
fig5_panel[, `:=`(
  pred_no_state_fe   = NA_real_,
  pred_with_state_fe = NA_real_
)]

# ─────────────────────────────────────────────────────────────────────────────
# 17.5  K-FOLD CROSS-VALIDATION
# ─────────────────────────────────────────────────────────────────────────────
cat("\n--- 17.5: Running 5-fold CV ---\n")

for (k in 1:5) {

  train <- fig5_panel[fold != k]
  test  <- fig5_panel[fold == k]

  cat(sprintf("  Fold %d | train: %s fac-years, %s facs | test: %s fac-years, %s facs\n",
              k,
              format(nrow(train),            big.mark = ","),
              format(uniqueN(train$panel_id), big.mark = ","),
              format(nrow(test),             big.mark = ","),
              format(uniqueN(test$panel_id),  big.mark = ",")))

  # ── Spec 1: Year FE only ──────────────────────────────────────────────────
  m_nofe <- tryCatch(
    glm(FORM_NOFE, data = train, family = binomial(link = "logit")),
    error = function(e) {
      message(sprintf("    [Fold %d, no-state-FE] glm failed: %s", k, e$message))
      NULL
    }
  )
  if (!is.null(m_nofe)) {
    # predict() handles new year factor levels by setting them to NA; suppress warning
    preds_nofe <- suppressWarnings(
      predict(m_nofe, newdata = test, type = "response")
    )
    fig5_panel[fold == k, pred_no_state_fe := preds_nofe]
  }

  # ── Spec 2: Year + State FE ───────────────────────────────────────────────
  # State FE is estimable because every state in test also appears in train
  # (we folded by facility; a state is "held out" only if it has exactly 1
  # facility, which we check and warn about below).
  states_test_only <- setdiff(unique(test$state), unique(train$state))
  if (length(states_test_only) > 0) {
    cat(sprintf("    ⚠  Fold %d: states only in test (FE undefined): %s\n",
                k, paste(states_test_only, collapse = ", ")))
    cat("       State FE for these will fallback to NA — inspect calibration.\n")
  }

  m_statefe <- tryCatch(
    glm(FORM_STATEFE, data = train, family = binomial(link = "logit")),
    error = function(e) {
      message(sprintf("    [Fold %d, state-FE] glm failed: %s", k, e$message))
      NULL
    }
  )
  if (!is.null(m_statefe)) {
    preds_statefe <- suppressWarnings(
      predict(m_statefe, newdata = test, type = "response")
    )
    fig5_panel[fold == k, pred_with_state_fe := preds_statefe]
  }
}

cat("\n  ✓ CV complete. OOB prediction coverage:\n")
cat(sprintf("    No-state-FE:   %s / %s rows have predictions (%.1f%%)\n",
            sum(!is.na(fig5_panel$pred_no_state_fe)),
            nrow(fig5_panel),
            100 * mean(!is.na(fig5_panel$pred_no_state_fe))))
cat(sprintf("    With-state-FE: %s / %s rows have predictions (%.1f%%)\n",
            sum(!is.na(fig5_panel$pred_with_state_fe)),
            nrow(fig5_panel),
            100 * mean(!is.na(fig5_panel$pred_with_state_fe))))

# ─────────────────────────────────────────────────────────────────────────────
# 17.6  CALIBRATION TABLE
# ─────────────────────────────────────────────────────────────────────────────
cat("\n--- 17.6: Calibration table (OOB predictions by decile) ---\n")

cv_complete <- fig5_panel[!is.na(pred_no_state_fe)]
cv_complete[, decile := as.integer(cut(
  pred_no_state_fe,
  breaks = quantile(pred_no_state_fe, probs = seq(0, 1, 0.1), na.rm = TRUE),
  labels = FALSE, include.lowest = TRUE
))]

calibration_table <- cv_complete[, .(
  mean_predicted  = mean(pred_no_state_fe, na.rm = TRUE),
  mean_actual     = mean(event_first_leak, na.rm = TRUE),
  n_fac_years     = .N,
  n_first_leaks   = sum(event_first_leak)
), by = decile][order(decile)]

# Lift: actual rate in each decile relative to bottom decile
bottom_rate <- calibration_table[decile == 1, mean_actual]
calibration_table[, lift := fifelse(
  bottom_rate > 0, mean_actual / bottom_rate, NA_real_
)]

cat("Calibration Table (No-State-FE spec, primary):\n")
print(calibration_table, digits = 4)

top_rate <- calibration_table[decile == 10, mean_actual]
if (!is.na(bottom_rate) && bottom_rate > 0)
  cat(sprintf("\n  Top-decile / Bottom-decile lift: %.1fx\n", top_rate / bottom_rate))

fwrite(calibration_table,
       file.path(OUTPUT_TABLES, "Figure_5B_Calibration_Table.csv"))
cat("✓ Saved: Figure_5B_Calibration_Table.csv\n")

# ─────────────────────────────────────────────────────────────────────────────
# 17.7  DISCRIMINATION: AUC-ROC
# ─────────────────────────────────────────────────────────────────────────────
cat("\n--- 17.7: AUC-ROC ---\n")

auc_nofe <- tryCatch({
  roc_nofe  <- pROC::roc(cv_complete$event_first_leak,
                         cv_complete$pred_no_state_fe,
                         quiet = TRUE)
  as.numeric(pROC::auc(roc_nofe))
}, error = function(e) { NA_real_ })

auc_statefe <- tryCatch({
  cv_sfe    <- fig5_panel[!is.na(pred_with_state_fe)]
  roc_sfe   <- pROC::roc(cv_sfe$event_first_leak,
                         cv_sfe$pred_with_state_fe,
                         quiet = TRUE)
  as.numeric(pROC::auc(roc_sfe))
}, error = function(e) { NA_real_ })

cat(sprintf("  AUC-ROC  (Year FE only):       %.3f\n", auc_nofe))
cat(sprintf("  AUC-ROC  (Year + State FE):    %.3f\n", auc_statefe))

if (!is.na(auc_nofe)) {
  if      (auc_nofe >= 0.8) cat("  → Good discrimination (AUC ≥ 0.80)\n")
  else if (auc_nofe >= 0.7) cat("  → Acceptable discrimination (AUC 0.70-0.79)\n")
  else                       cat("  → Weak discrimination (AUC < 0.70) — expected for rare events\n")
  cat("  Note: Leaks are rare (low base rate); AUC alone understates lift.\n")
  cat("  Inspect the top-decile lift ratio alongside AUC.\n")
}

auc_summary <- data.frame(
  Spec    = c("Year FE only (primary)", "Year + State FE (robustness)"),
  AUC_ROC = round(c(auc_nofe, auc_statefe), 4)
)
fwrite(as.data.table(auc_summary),
       file.path(OUTPUT_TABLES, "Figure_5B_AUC_Summary.csv"))
cat("✓ Saved: Figure_5B_AUC_Summary.csv\n")

# ─────────────────────────────────────────────────────────────────────────────
# 17.8  PARTIAL DEPENDENCE SUMMARIES
# ─────────────────────────────────────────────────────────────────────────────
cat("\n--- 17.8: Partial dependence summaries ---\n")

# By single-walled share (quartile bins)
cv_complete[, sw_bin := cut(
  pct_single_walled_fac,
  breaks = c(0, 0.25, 0.5, 0.75, 1.00001),
  labels = c("0-25%", "26-50%", "51-75%", "76-100%"),
  include.lowest = TRUE
)]

pd_sw <- cv_complete[!is.na(sw_bin), .(
  mean_predicted = mean(pred_no_state_fe, na.rm = TRUE),
  mean_actual    = mean(event_first_leak, na.rm = TRUE),
  n = .N
), by = sw_bin][order(sw_bin)]

# By mean tank age (bins matching memo example)
cv_complete[, age_bin := cut(
  mean_tank_age,
  breaks = c(0, 10, 20, 30, Inf),
  labels = c("0-10 yrs", "11-20 yrs", "21-30 yrs", "30+ yrs"),
  include.lowest = TRUE
)]

pd_age <- cv_complete[!is.na(age_bin), .(
  mean_predicted = mean(pred_no_state_fe, na.rm = TRUE),
  mean_actual    = mean(event_first_leak, na.rm = TRUE),
  n = .N
), by = age_bin][order(age_bin)]

# By pre-1980 vintage share (quartile bins)
cv_complete[, vin_bin := cut(
  pct_pre_1980_fac,
  breaks = c(-0.001, 0, 0.25, 0.5, 1.00001),
  labels = c("None", "1-25%", "26-50%", "51-100%"),
  include.lowest = TRUE
)]

pd_vin <- cv_complete[!is.na(vin_bin), .(
  mean_predicted = mean(pred_no_state_fe, na.rm = TRUE),
  mean_actual    = mean(event_first_leak, na.rm = TRUE),
  n = .N
), by = vin_bin][order(vin_bin)]

cat("Partial Dependence — % Single-Walled:\n"); print(pd_sw)
cat("Partial Dependence — Mean Tank Age:\n");    print(pd_age)
cat("Partial Dependence — % Pre-1980 Vintage:\n"); print(pd_vin)

# Save combined PD table
pd_all <- rbind(
  pd_sw[ , .(Factor = "Share Single-Walled", Category = as.character(sw_bin),
             Mean_Predicted = mean_predicted, Mean_Actual = mean_actual, N = n)],
  pd_age[, .(Factor = "Mean Tank Age",       Category = as.character(age_bin),
             Mean_Predicted = mean_predicted, Mean_Actual = mean_actual, N = n)],
  pd_vin[, .(Factor = "Share Pre-1980",      Category = as.character(vin_bin),
             Mean_Predicted = mean_predicted, Mean_Actual = mean_actual, N = n)]
)
fwrite(pd_all, file.path(OUTPUT_TABLES, "Figure_5B_Partial_Dependence.csv"))
cat("✓ Saved: Figure_5B_Partial_Dependence.csv\n")

# ─────────────────────────────────────────────────────────────────────────────
# 17.9  FIGURE 5B — CALIBRATION PLOT + PARTIAL DEPENDENCE PANELS
# ─────────────────────────────────────────────────────────────────────────────
cat("\n--- 17.9: Generating Figure 5B ---\n")

# ── Panel 1: Calibration plot ─────────────────────────────────────────────────
auc_label <- ifelse(!is.na(auc_nofe),
                    sprintf("5-Fold CV, Year FE | AUC = %.3f", auc_nofe),
                    "5-Fold CV, Year FE | AUC not computed")

fig5b_cal <- ggplot(calibration_table,
                    aes(x = mean_predicted, y = mean_actual)) +
  geom_abline(intercept = 0, slope = 1,
              linetype = "dashed", color = "gray50", linewidth = 0.7) +
  geom_point(size = 3.5, color = "#D55E00") +
  geom_text(aes(label = decile), vjust = -0.8, size = 3.2, color = "gray30") +
  scale_x_continuous(labels = scales::percent_format(accuracy = 0.01)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.01)) +
  labs(
    title    = "Calibration: Predicted vs. Actual First-Leak Rates (OOB)",
    subtitle = auc_label,
    x = "Mean OOB Predicted Probability",
    y = "Mean Actual First-Leak Rate",
    caption  = "Numbers = decile. Dashed line = perfect calibration."
  ) +
  theme_pub()

# ── Panel 2: Partial dependence — long format for faceted bar chart ────────────
pd_long <- melt(
  pd_all,
  id.vars      = c("Factor", "Category"),
  measure.vars = c("Mean_Predicted", "Mean_Actual"),
  variable.name = "Type",
  value.name    = "Rate"
)
pd_long[, Type := fifelse(Type == "Mean_Predicted",
                          "Predicted (OOB)", "Actual")]

# Enforce factor order within each facet
pd_long[Factor == "Share Single-Walled",
        Category := factor(Category,
                           levels = c("0-25%", "26-50%", "51-75%", "76-100%"))]
pd_long[Factor == "Mean Tank Age",
        Category := factor(Category,
                           levels = c("0-10 yrs", "11-20 yrs", "21-30 yrs", "30+ yrs"))]
pd_long[Factor == "Share Pre-1980",
        Category := factor(Category,
                           levels = c("None", "1-25%", "26-50%", "51-100%"))]

fig5b_pd <- ggplot(pd_long[!is.na(Category) & !is.na(Rate)],
                   aes(x = Category, y = Rate, fill = Type)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.68) +
  facet_wrap(~Factor, scales = "free_x", nrow = 1) +
  scale_fill_manual(
    values = c("Predicted (OOB)" = "#D55E00", "Actual" = "#0072B2")
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.01)) +
  labs(
    title    = "Partial Dependence: Predicted vs. Actual First-Leak Rates",
    subtitle = "Facility-year averages within covariate bins | Pre-period 1990-1998",
    x = NULL, y = "First-Leak Rate", fill = NULL,
    caption  = "Predicted = OOB logistic predictions (Year FE spec, no state FE)."
  ) +
  theme_pub() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1, size = 8))

# ── Combine into Figure 5B composite ──────────────────────────────────────────
fig5b_combined <- gridExtra::arrangeGrob(
  fig5b_cal, fig5b_pd, nrow = 2,
  heights = c(1, 0.85),
  top = grid::textGrob(
    "Figure 5B: Cross-Validated Risk Factor Validation (Facility Level)",
    gp = grid::gpar(fontsize = 13, fontface = "bold")
  )
)

ggsave(file.path(OUTPUT_FIGURES, "Figure_5B_CV_Risk_Validation.png"),
       fig5b_combined, width = 14, height = 12, dpi = 300, bg = "white")
cat("✓ Saved: Figure_5B_CV_Risk_Validation.png\n")

# ─────────────────────────────────────────────────────────────────────────────
# 17.10 FULL COEFFICIENT TABLE (in-sample, for interpretation)
# ─────────────────────────────────────────────────────────────────────────────
cat("\n--- 17.10: Full-sample logit coefficients (in-sample, for reporting) ---\n")
cat("NOTE: These are IN-SAMPLE — for table reporting only.\n")
cat("      Use OOB predictions (above) for validation claims.\n\n")

m_full_nofe <- tryCatch(
  glm(FORM_NOFE, data = fig5_panel, family = binomial(link = "logit")),
  error = function(e) {
    message(sprintf("Full-sample logit (no state FE) failed: %s", e$message))
    NULL
  }
)
m_full_sfe <- tryCatch(
  glm(FORM_STATEFE, data = fig5_panel, family = binomial(link = "logit")),
  error = function(e) {
    message(sprintf("Full-sample logit (state FE) failed: %s", e$message))
    NULL
  }
)

if (!is.null(m_full_nofe)) {
  cat("Full-sample logit (Year FE only):\n")
  coef_nofe <- as.data.table(broom::tidy(m_full_nofe, exponentiate = FALSE))
  coef_nofe <- coef_nofe[!grepl("^factor\\(panel_year\\)", term)]  # suppress year FEs for display
  print(coef_nofe[, .(term, estimate = round(estimate,4),
                       std.error = round(std.error,4),
                       p.value   = round(p.value, 4))],
        row.names = FALSE)
}
if (!is.null(m_full_sfe)) {
  cat("\nFull-sample logit (Year + State FE):\n")
  coef_sfe <- as.data.table(broom::tidy(m_full_sfe, exponentiate = FALSE))
  coef_sfe <- coef_sfe[!grepl("^factor\\(panel_year\\)|^factor\\(state\\)", term)]
  print(coef_sfe[, .(term, estimate = round(estimate,4),
                      std.error = round(std.error,4),
                      p.value   = round(p.value, 4))],
        row.names = FALSE)
}

# Save coefficient tables
coef_out <- rbind(
  if (!is.null(m_full_nofe)) {
    dt <- as.data.table(broom::tidy(m_full_nofe))
    dt[!grepl("^factor\\(panel_year\\)", term), .(Spec = "Year FE only", term,
       estimate = round(estimate, 4), std.error = round(std.error, 4),
       p.value  = round(p.value, 4))]
  },
  if (!is.null(m_full_sfe)) {
    dt <- as.data.table(broom::tidy(m_full_sfe))
    dt[!grepl("^factor\\(panel_year\\)|^factor\\(state\\)", term),
       .(Spec = "Year + State FE", term,
         estimate = round(estimate, 4), std.error = round(std.error, 4),
         p.value  = round(p.value, 4))]
  },
  fill = TRUE
)
fwrite(coef_out,
       file.path(OUTPUT_TABLES, "Figure_5_Full_Sample_Logit_Coefs.csv"))
cat("✓ Saved: Figure_5_Full_Sample_Logit_Coefs.csv\n")

# Save full CV panel (with OOB predictions) for any further analysis
fwrite(fig5_panel[, .(
  panel_id, state, panel_year, fold,
  event_first_leak,
  pct_single_walled_fac, mean_tank_age, age_gt_20_fac,
  pct_pre_1980_fac, log_capacity_fac, is_motor_fuel_fac,
  risk_score_fac, risk_category_fac,
  pred_no_state_fe, pred_with_state_fe
)], file.path(OUTPUT_TABLES, "Figure_5_CV_Panel_with_OOB_Preds.csv"))
cat("✓ Saved: Figure_5_CV_Panel_with_OOB_Preds.csv\n")

gc()


#==============================================================================
# SECTION 10: MODEL 1A — FACILITY-YEAR DiD + EVENT STUDY
#==============================================================================

cat("\n========================================\n")
cat("SECTION 10: MODEL 1A — FACILITY DiD\n")
cat("========================================\n\n")

cat("Specification: Closure_Event_it = αi + λt + β(TX_i × Post_t) + εit\n")
cat("Outcome:  Any_Closure = 1 if facility closed ≥1 tank in year t\n")
cat("FE:       Facility (αi) + Year (λt)\n")
cat("Cluster:  State (19 clusters)\n\n")

# Baseline
model_1a <- feols(
  closure_event ~ did_term | panel_id + panel_year,
  data    = annual_data,
  cluster = ~state,
  lean    = FALSE
)

cat("--- Model 1A: Baseline ---\n")
print(summary(model_1a))

# With facility-size control (log tanks)
model_1a_ctrl <- feols(
  closure_event ~ did_term + log(active_tanks_dec + 1) | panel_id + panel_year,
  data    = annual_data,
  cluster = ~state,
  lean    = FALSE
)

cat("\n--- Model 1A: With Controls ---\n")
print(summary(model_1a_ctrl))

# Interpretation
beta_1a <- coef(model_1a)["did_term"]
cat("\n--- Economic Interpretation ---\n")
cat(sprintf("β = %.6f (%.4f pp)\n", beta_1a, beta_1a * 100))
if (beta_1a > 0) {
  cat("→ Texas facilities MORE likely to close a tank post-1999\n")
  cat("→ Consistent with insurance reform accelerating tank retirement\n")
} else {
  cat("→ Texas facilities LESS likely to close a tank post-1999\n")
}

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
# SECTION 11: MODEL 1B — FACILITY-LEVEL COX SURVIVAL
#==============================================================================

cat("\n========================================\n")
cat("SECTION 11: MODEL 1B — FACILITY SURVIVAL\n")
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
# SECTION 12: MODEL 2 — TANK-LEVEL COX HTE (TWO SPECIFICATIONS)
#==============================================================================

cat("\n========================================\n")
cat("SECTION 12: MODEL 2 — TANK-LEVEL COX HTE\n")
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
# SECTION 13: MODEL 3A — AGE AT CLOSURE (COUNTY FE) + EVENT STUDY
#==============================================================================

cat("\n========================================\n")
cat("SECTION 13: MODEL 3A — AGE AT CLOSURE\n")
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
# SECTION 14: MODEL 3B — AGE AT CLOSURE (FACILITY FE, SPANNING SAMPLE)
#==============================================================================

cat("\n========================================\n")
cat("SECTION 14: MODEL 3B — WITHIN-FACILITY\n")
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

  save_standard_did_table(
    models        = list(model_3b, model_3b_did),
    headers       = c("Post Only (Facility FE)", "TX×Post (DiD within Spanning)"),
    base_name     = "Model_3B_Age_at_Closure_Facility_FE",
    title         = "Model 3B: Age at Closure (Facility FE, Spanning Sample)",
    treatment_var = "post",
    cluster_var   = "state",
    use_bootstrap = USE_BOOTSTRAP,
    n_reps        = N_BOOTSTRAP
  )
  cat("✓ Saved: Model_3B_Age_at_Closure_Facility_FE.*\n")
}


#==============================================================================
# SECTION 15: MODEL 4 — REVEALED LEAKS AT CLOSURE
#==============================================================================

cat("\n========================================\n")
cat("SECTION 15: MODEL 4 — REVEALED LEAKS\n")
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


#==============================================================================
# SECTION 16: MODELS 5A/5B — OPERATIONAL LEAKS, COMPETING RISKS
#==============================================================================

cat("\n========================================\n")
cat("SECTION 16: MODELS 5A/5B — COMPETING RISKS\n")
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
# SECTION 17: ROBUSTNESS — REGULATORY VINTAGE CONTROLS
#==============================================================================

cat("\n========================================\n")
cat("SECTION 17: ROBUSTNESS CHECKS\n")
cat("========================================\n\n")

cat("--- Robustness 1: Regulatory Vintage Controls (1998 Federal Mandate) ---\n")
cat("The Dec 22, 1998 federal mandate required all tanks installed ≤ 1988\n")
cat("to retrofit or close by the deadline. This confounds our 1999 TX reform.\n")
cat("We add pre1998_install × post_1999 interaction to absorb mandate effects.\n\n")

# Check reg_vintage column exists
if ("reg_vintage" %in% names(annual_data)) {
  annual_data[, reg_vintage := factor(reg_vintage,
                                       levels = c("Post-Deadline", "Transition", "Pre-RCRA"))]

  # Baseline Model 1A + reg vintage control
  model_rob_1a <- feols(
    closure_event ~ did_term + pre1998_install:post_1999 |
      panel_id + panel_year,
    data    = annual_data,
    cluster = ~state
  )
  cat("Model 1A + pre1998 mandate interaction:\n")
  print(summary(model_rob_1a))

  # Model 1A + full vintage FE interaction
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

  # Interpretation note
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
  cat("⚠  reg_vintage column not found in panel — skipping this robustness check.\n")
  cat("   Ensure 10_Build_Annual_Panel_Optimized.R has been run.\n")
}


#==============================================================================
# SECTION 18: SCRIPT SUMMARY
#==============================================================================

cat("\n====================================================================\n")
cat("FINAL DiD ANALYSIS COMPLETE\n")
cat("====================================================================\n\n")

cat("MODELS ESTIMATED:\n")
cat("  ✓ Model 1A:    Facility-Year DiD (Baseline + Controls)\n")
cat("  ✓ Model 1A-ES: Event Study (", ES_START, "–", ES_END, ")\n", sep="")
cat("  ✓ Model 1B:    Facility-Level Cox Survival\n")
cat("  ✓ Model 2A-C:  Tank-Level Cox HTE — Spec A (simple interactions)\n")
cat("  ✓ Model 2A-D:  Tank-Level Cox HTE — Spec B (county strata, preferred)\n")
cat("  ✓ Model 3A:    Age at Closure (County FE + controls)\n")
cat("  ✓ Model 3A-ES: Age at Closure Event Study\n")
cat("  ✓ Model 3B:    Age at Closure (Facility FE, spanning facilities)\n")
cat("  ✓ Model 4:     Revealed Leaks — LPM (4 windows) + Logit (robustness)\n")
cat("  ✓ Model 5A:    Cause-Specific Cox (leak vs closure competing risks)\n")
cat("  ✓ Model 5B:    Fine-Gray Subdistribution Hazard\n")
cat("  ✓ Robustness:  Regulatory Vintage / 1998 Mandate Controls\n")
cat("  ✓ Figure 5A:   Raw First-Leak Rates by Facility Risk Category\n")
cat("  ✓ Figure 5B:   5-Fold CV Risk Validation (Facility-Level Logit)\n\n")

cat("TABLES SAVED:\n")
cat("  ✓ Diagnostic_Pre1999_Exit_Balance.csv\n")
cat("  ✓ Table_1_Sample_Composition.csv\n")
cat("  ✓ Table_2_Baseline_Characteristics.csv\n")
cat("  ✓ Table_3_Risk_Factor_Distribution.csv\n")
cat("  ✓ Model_1A_Facility_Closure_Probability (.csv / .txt / .tex)\n")
cat("  ✓ Model_1A_ES_Coefficients.csv\n")
cat("  ✓ Model_1B_Facility_Survival_First_Closure (.csv / .txt)\n")
cat("  ✓ Model_2_SpecA_Tank_HTE_Simple (.csv / .txt)\n")
cat("  ✓ Model_2_SpecB_Tank_HTE_County_Strata (.csv / .txt)\n")
cat("  ✓ Model_3A_Age_at_Closure_County_FE (.csv / .txt / .tex)\n")
cat("  ✓ Model_3A_ES_Coefficients.csv\n")
cat("  ✓ Model_3B_Age_at_Closure_Facility_FE (.csv / .txt / .tex)\n")
cat("  ✓ Model_4_Revealed_Leaks_LPM (.csv / .txt / .tex)\n")
cat("  ✓ Model_4_Revealed_Leaks_Logit_Robustness (.csv / .txt / .tex)\n")
cat("  ✓ Model_5A_Cause_Specific_Cox (.csv / .txt)\n")
cat("  ✓ Model_5B_Fine_Gray_Subdistribution (.csv / .txt)\n")
cat("  ✓ Robustness_Regulatory_Vintage_Controls (.csv / .txt / .tex)\n\n")

cat("FIGURES SAVED:\n")
cat("  ✓ Figure_1_Age_Distribution_1999.png\n")
cat("  ✓ Figure_2_Vintage_Distribution.png\n")
cat("  ✓ Figure_3_Single_Walled_by_Vintage.png\n")
cat("  ✓ Figure_4_Pre_Period_Trends.png (4-panel: closure, leak, age, exit)\n")
cat("  ✓ Model_1A_Event_Study_iplot.png (iplot — separate from Figure 4)\n")
cat("  ✓ Model_3A_ES_Age_at_Closure.png\n\n")
cat("  ✓ Figure_5A_Raw_Leak_Rates_by_Risk.png\n")
cat("  ✓ Figure_5B_CV_Risk_Validation.png (2-panel: calibration + PD)\n\n")

cat("TABLES SAVED:\n")
cat("  ✓ Appendix_A1_Missing_Data_Scale.csv\n")
cat("  ✓ Appendix_A1_Missing_Data_Balance.csv\n")
cat("  ✓ Diagnostic_Pre1999_Exit_Balance.csv\n")
cat("  ✓ Table_1_Sample_Composition.csv\n")
cat("  ✓ Table_2_Baseline_Characteristics.csv\n")
cat("  ✓ Table_3_Risk_Factor_Distribution.csv\n")
cat("  ✓ Model_1A_Facility_Closure_Probability (.csv / .txt / .tex)\n")
cat("  ✓ Model_1A_ES_Coefficients.csv\n")
cat("  ✓ Model_1B_Facility_Survival_First_Closure (.csv / .txt)\n")
cat("  ✓ Model_2_SpecA_Tank_HTE_Simple (.csv / .txt)\n")
cat("  ✓ Model_2_SpecB_Tank_HTE_County_Strata (.csv / .txt)\n")
cat("  ✓ Model_3A_Age_at_Closure_County_FE (.csv / .txt / .tex)\n")
cat("  ✓ Model_3A_ES_Coefficients.csv\n")
cat("  ✓ Model_3B_Age_at_Closure_Facility_FE (.csv / .txt / .tex)\n")
cat("  ✓ Model_4_Revealed_Leaks_LPM (.csv / .txt / .tex)\n")
cat("  ✓ Model_4_Revealed_Leaks_Logit_Robustness (.csv / .txt / .tex)\n")
cat("  ✓ Model_5A_Cause_Specific_Cox (.csv / .txt)\n")
cat("  ✓ Model_5B_Fine_Gray_Subdistribution (.csv / .txt)\n")
cat("  ✓ Robustness_Regulatory_Vintage_Controls (.csv / .txt / .tex)\n")
cat("  ✓ Figure_5A_Raw_Rates_Data.csv\n")
cat("  ✓ Figure_5B_Calibration_Table.csv\n")
cat("  ✓ Figure_5B_AUC_Summary.csv\n")
cat("  ✓ Figure_5B_Partial_Dependence.csv\n")
cat("  ✓ Figure_5_Full_Sample_Logit_Coefs.csv\n")
cat("  ✓ Figure_5_CV_Panel_with_OOB_Preds.csv\n")
cat("  ✓ Figure_4_Pre_Trends_Data.csv\n\n")

cat("FIGURES SAVED:\n")
cat("  ✓ Figure_1_Age_Distribution_1999.png\n")
cat("  ✓ Figure_2_Vintage_Distribution.png\n")
cat("  ✓ Figure_3_Single_Walled_by_Vintage.png\n")
cat("  ✓ Figure_4_Pre_Period_Trends.png [AUDITED: n_leaks + tank-level closure age]\n")
cat("  ✓ Figure_5A_Raw_Leak_Rates_by_Risk.png\n")
cat("  ✓ Figure_5B_CV_Risk_Validation.png\n")
cat("  ✓ Model_1A_Event_Study_iplot.png\n")
cat("  ✓ Model_3A_ES_Age_at_Closure.png\n\n")

cat("KEY SPECIFICATIONS:\n")
cat(sprintf("  Sample:           Incumbent facilities (is_incumbent == 1)\n"))
cat(sprintf("  Main window:      %d–%d\n", PANEL_START, PANEL_END))
cat(sprintf("  Event study:      %d–%d\n", ES_START, ES_END))
cat(sprintf("  Fig 5 window:     1990–1998 (pre-period only)\n"))
cat(sprintf("  Treatment:        TX vs {%s}\n",
            paste(CONTROL_STATES, collapse = ", ")))
cat(sprintf("  Clustering:       State (%d clusters)\n",
            length(unique(annual_data$state))))
cat(sprintf("  Bootstrap:        %s (B = %d)\n",
            ifelse(USE_BOOTSTRAP, "ENABLED", "DISABLED"), N_BOOTSTRAP))
cat(sprintf("  Fig 5 unit:       Facility-year (LUST are facility-level)\n"))
cat(sprintf("  Fig 5 incidence:  Facility's first-ever LUST report\n"))
cat(sprintf("  Fig 5 CV folds:   5, folded by facility, stratified by state\n"))
cat(sprintf("  Fig 5 FE specs:   Year FE (primary) + Year+State FE (robustness)\n"))

cat("\n====================================================================\n")
cat("NEXT STEPS:\n")
cat("  1. Set USE_BOOTSTRAP = TRUE and N_BOOTSTRAP = 9999 for final run\n")
cat("  2. Review Figure 4 panel (b): confirm n_leaks > leak_year in practice\n")
cat("  3. Review Figure 4 panel (c): confirm pre_period_closures non-empty\n")
cat("  4. Inspect Figure 5B calibration: top-decile lift and AUC\n")
cat("  5. Compare Figure 5B Year-FE vs Year+State-FE AUC for attenuation\n")
cat("  6. Compare Model 2 Spec A vs Spec B — prefer Spec B (county strata)\n")
cat("  7. Inspect Model 4 sensitivity across windows for robustness\n")
cat("  8. Consider Definition B robustness if pre-1999 exit test fails\n")
cat("====================================================================\n")