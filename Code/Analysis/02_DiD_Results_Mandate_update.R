#==============================================================================
# 02_DiD_Main.R
# Texas UST Insurance Reform -- Causal Evidence from DiD
#
# PURPOSE:
#   All causal estimation for the JMP paper Section 3 ("Causal Evidence").
#   Loads analysis-ready .rds files from 01_Descriptive_Analysis.R and produces
#   publication-ready regression tables and event study figures.
#
# ARCHITECTURE: Implicit Triple-Difference via Vintage-Cohort x Year FE
#
#   HEADLINE: Pooled sample with vintage_cohort x year FE + mandate_active
#     - vintage_cohort^panel_year absorbs cohort-specific temporal patterns
#       common to TX and control facilities within the same vintage bin
#       (federal deadline ramp-up, vintage-specific aging/obsolescence)
#     - mandate_active absorbs TX-specific phased mandate impulse (1989-1993)
#     - did_term (TX x Post) identified from within-vintage, cross-state
#       post-1999 divergence net of mandate impulse
#
#   ROBUSTNESS: Spec A (post-1988 only, no mandate FE needed)
#               Spec B (pre-1988 only, mandate controls escalation)
#
# ECONOMETRIC JUSTIFICATION (de Chaisemartin & d'Haultfoeuille 2023):
#   Interacting time FE with group indicators eliminates cross-group
#   contamination. The coefficient on the treatment variable is equivalent
#   to running TWFE restricted to each group separately. See also:
#   Gormley & Matsa (2014 RFS), Wooldridge (2021 extended TWFE).
#
# PAPER OUTPUTS:
#   Table 3:   HEADLINE -- Pooled + Vintage FE (Closure/Exit/Replace)
#   Table 4:   Robustness -- Spec A (Post-1988, Mandate-Free)
#   Table 5:   Robustness -- Spec B (Mandate Controls Escalation)
#   Table 6:   Reported Leak DiD (Pooled headline + Spec A/B)
#   Table 7:   HTE -- Age Bin x Treatment
#   Table 8:   HTE -- Wall Type x Treatment
#   Table B.4: Parallel Trends Validation (6-test battery)
#   Table B.5: MD-Excluded Robustness
#   Table B.6: mandate_window_3yr Sensitivity
#   Table F.1: Vintage Bin Coarseness Robustness
#   Figure 6:  Event Study -- Pooled + Vintage FE (Main Text) [HEADLINE]
#   Figure 6B: Event Study -- Spec A (Appendix)
#   Figure 6C: Event Study -- Spec B + Mandate (Appendix)
#   Figure 7:  Leak Event Study (Pooled + Vintage FE)
#   Figure 8:  HTE -- Age Bin Treatment Effects
#
# SURVIVAL MODELS: Section 12 contains a STUB pending methodological
#   clarification. See Cox PH questions at end of script.
#
# STRUCTURE:
#   S1   Setup, Data Loading, Variable Verification & Construction
#   S2   Helper Functions
#   S3   Parallel Trends Validation (6-Test Battery)
#   S4   Table 3 -- HEADLINE: Pooled + Vintage FE (Closure/Exit/Replace)
#   S5   Table 4 -- Spec A Robustness
#   S6   Table 5 -- Spec B Robustness (Mandate Controls Escalation)
#   S7   Table 6 -- Reported Leak DiD
#   S8   HTE -- Age Bin x Treatment (Table 7)
#   S9   HTE -- Wall Type x Treatment (Table 8)
#   S10  Additional Models (Age at Closure, Revealed Leaks, Competing Risks)
#   S11  Appendix Robustness
#   S12  Duration / Survival Models (STUB -- pending clarification)
#   S13  Diagnostic Data Export (for 03_DiD_Assumptions.R)
#   S14  JMP Publication Tables (Unified LaTeX)
#   S15  Script Summary
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
  library(kableExtra)
})

if (requireNamespace("fwildclusterboot", quietly = TRUE)) {
  library(fwildclusterboot)
  cat("fwildclusterboot loaded.\n")
}

options(scipen = 999)
set.seed(20260202)
setDTthreads(14)

# -- Load metadata --
ANALYSIS_DIR <- here("Data", "Analysis")
if (!file.exists(file.path(ANALYSIS_DIR, "analysis_metadata.rds")))
  stop("Run 01_Descriptive_Analysis.R first.\n  Missing: ", ANALYSIS_DIR)

meta <- readRDS(file.path(ANALYSIS_DIR, "analysis_metadata.rds"))
TREATMENT_YEAR       <- meta$treatment_year
TREATMENT_DATE       <- meta$treatment_date
POST_YEAR            <- meta$post_year
PANEL_START          <- meta$panel_start
PANEL_END            <- meta$panel_end
CONTROL_STATES       <- meta$control_states
ES_START             <- meta$es_start
ES_END               <- meta$es_end
STUDY_END_DATE       <- meta$study_end_date
OUTPUT_TABLES        <- meta$output_tables
OUTPUT_FIGURES       <- meta$output_figures
FEDERAL_MANDATE_DATE <- meta$federal_mandate_date
MANDATE_CUTOFF_DATE  <- meta$mandate_cutoff_date
incumbent_ids        <- meta$incumbent_ids
AGE_BIN_BREAKS       <- meta$age_bin_breaks
AGE_BIN_LABELS       <- meta$age_bin_labels
AGE_BIN_REF          <- meta$age_bin_ref

USE_BOOTSTRAP <- FALSE   # <-- CHANGE TO TRUE FOR FINAL RUN
N_BOOTSTRAP   <- 9999

dir.create(OUTPUT_TABLES,  recursive = TRUE, showWarnings = FALSE)
dir.create(OUTPUT_FIGURES, recursive = TRUE, showWarnings = FALSE)

# -- Publication theme --
COL_TX   <- "#D55E00"
COL_CTRL <- "#0072B2"

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


# -- Load analysis-ready datasets --
cat("Loading analysis datasets...\n")

annual_data       <- readRDS(file.path(ANALYSIS_DIR, "analysis_annual_data.rds"))
tank_inventory    <- readRDS(file.path(ANALYSIS_DIR, "analysis_tank_inventory.rds"))
closed_tanks      <- readRDS(file.path(ANALYSIS_DIR, "analysis_closed_tanks.rds"))
tanks_1999        <- readRDS(file.path(ANALYSIS_DIR, "analysis_tanks_1999.rds"))
pre_period_closures <- readRDS(file.path(ANALYSIS_DIR, "analysis_pre_period_closures.rds"))

cat(sprintf("  annual_data:     %s rows, %s facilities\n",
            format(nrow(annual_data), big.mark = ","),
            format(uniqueN(annual_data$panel_id), big.mark = ",")))
cat(sprintf("  tank_inventory:  %s tanks\n", format(nrow(tank_inventory), big.mark = ",")))
cat(sprintf("  closed_tanks:    %s closures\n", format(nrow(closed_tanks), big.mark = ",")))


# --------------------------------------------------------------------------
# 1.2  VARIABLE VERIFICATION & DEFENSIVE CONSTRUCTION
# --------------------------------------------------------------------------
# Verify required variables from 01; construct defensively if missing.
# This block ensures 02 runs even if 01 has not been fully updated.
# --------------------------------------------------------------------------

cat("\n--- 1.2: Variable verification ---\n")

stopifnot("texas_treated"   %in% names(annual_data))
stopifnot("post_1999"       %in% names(annual_data))
stopifnot("mandate_active"  %in% names(annual_data))
stopifnot("mandate_cohort"  %in% names(annual_data))
stopifnot("spec_A_eligible" %in% names(annual_data))
stopifnot("spec_B_eligible" %in% names(annual_data))
stopifnot("did_term"        %in% names(annual_data))
stopifnot("rel_year_1999"   %in% names(annual_data))

# -- Outcome aliases (defensive) --
if (!"any_closure" %in% names(annual_data))
  annual_data[, any_closure := closure_event]
if (!"any_leak" %in% names(annual_data))
  annual_data[, any_leak := leak_year]
if (!"exit_event" %in% names(annual_data))
  annual_data[, exit_event := exit_flag]
if (!"replace_event" %in% names(annual_data)) {
  annual_data[, replace_event := as.integer(
    closure_event == 1 & exit_flag == 0
  )]
}

# -- vintage_cell (binary: Pre-1988 vs Post-1988 for ALL facilities) --
if (!"vintage_cell" %in% names(annual_data)) {
  cat("  Constructing vintage_cell (Pre-1988 / Post-1988) for all facilities...\n")
  # Use spec eligibility: if exclusively post-1988 -> "Post-1988"
  # If any pre-1988 tanks -> "Pre-1988"
  annual_data[, vintage_cell := fifelse(
    spec_A_eligible == 1, "Post-1988", "Pre-1988"
  )]
  annual_data[, vintage_cell := factor(vintage_cell,
    levels = c("Post-1988", "Pre-1988"))]
  cat(sprintf("    Post-1988: %s facilities | Pre-1988: %s facilities\n",
    format(uniqueN(annual_data[vintage_cell == "Post-1988", panel_id]), big.mark = ","),
    format(uniqueN(annual_data[vintage_cell == "Pre-1988",  panel_id]), big.mark = ",")))
}

# -- vintage_cohort (6-bin for ALL facilities, TX and control alike) --
# This is the KEY variable for the implicit triple-diff.
# Uses TX mandate schedule cutpoints applied universally.
if (!"vintage_cohort" %in% names(annual_data)) {
  cat("  Constructing vintage_cohort (6-bin) for all facilities...\n")

  # Step 1: Compute at tank level using tanks_1999
  if (!"install_year" %in% names(tanks_1999))
    tanks_1999[, install_year := year(install_date)]

  tanks_1999[, vintage_cohort_tank := fcase(
    install_year < 1965,                          "Pre-1965",
    install_year >= 1965 & install_year <= 1974,  "1965-1974",
    install_year >= 1975 & install_year <= 1979,  "1975-1979",
    install_year >= 1980 & install_year <= 1984,  "1980-1984",
    install_year >= 1985 & install_year <= 1988,  "1985-1988",
    default =                                      "Post-1988"
  )]

  # Step 2: Aggregate to facility level via modal cohort
  fac_vintage <- tanks_1999[, .(
    vintage_cohort = names(which.max(table(vintage_cohort_tank)))
  ), by = panel_id]

  # Step 3: Merge to annual_data
  annual_data <- merge(annual_data, fac_vintage, by = "panel_id", all.x = TRUE)
  annual_data[is.na(vintage_cohort), vintage_cohort := "Post-1988"]
  annual_data[, vintage_cohort := factor(vintage_cohort,
    levels = c("Pre-1965", "1965-1974", "1975-1979",
               "1980-1984", "1985-1988", "Post-1988"))]

  cat("    Distribution (at treatment year):\n")
  print(annual_data[panel_year == TREATMENT_YEAR,
                    .N, by = vintage_cohort][order(vintage_cohort)])
}

# -- vintage_cohort_coarse (3-bin robustness) --
annual_data[, vintage_cohort_coarse := fcase(
  vintage_cohort %in% c("Pre-1965", "1965-1974", "1975-1979"), "Pre-1980",
  vintage_cohort %in% c("1980-1984", "1985-1988"),              "1980-1988",
  default =                                                      "Post-1988"
)]
annual_data[, vintage_cohort_coarse := factor(vintage_cohort_coarse,
  levels = c("Pre-1980", "1980-1988", "Post-1988"))]

# -- Cell count diagnostics --
cat("\n--- Vintage x Year Cell Diagnostics ---\n")
cell_counts <- annual_data[, .N, by = .(vintage_cohort, panel_year, texas_treated)]
sparse_cells <- cell_counts[N < 10]
if (nrow(sparse_cells) > 0) {
  cat(sprintf("  WARNING: %d sparse cells (N < 10) detected.\n", nrow(sparse_cells)))
  cat("  Consider coarsened bins (vintage_cohort_coarse) if SEs inflate.\n")
  print(sparse_cells[order(N)][1:min(10, nrow(sparse_cells))])
} else {
  cat("  OK: All vintage_cohort x year cells have N >= 10.\n")
}

# -- mandate_window_3yr (defensive) --
if (!"mandate_window_3yr" %in% names(annual_data)) {
  annual_data[, mandate_window_3yr := as.integer(
    state == "TX" & spec_B_eligible == 1 &
      panel_year >= 1988L & panel_year <= 1994L
  )]
}

# -- age_bin (canonical, for HTE) --
make_age_bin <- function(age_vec) {
  factor(
    cut(age_vec, breaks = AGE_BIN_BREAKS, labels = AGE_BIN_LABELS,
        right = FALSE, include.lowest = TRUE),
    levels = AGE_BIN_LABELS, ordered = FALSE
  )
}
if (!"age_bin" %in% names(annual_data)) {
  annual_data[, age_bin := make_age_bin(avg_tank_age)]
}
annual_data[, age_bin := relevel(age_bin, ref = AGE_BIN_REF)]

# -- Construct subsets --
specA_data  <- annual_data[spec_A_eligible == 1]
specB_data  <- annual_data[spec_B_eligible == 1]
pooled_data <- annual_data  # alias

# Event study bounds
rel_min <- ES_START - POST_YEAR
rel_max <- ES_END   - POST_YEAR

cat("\n====================================================================\n")
cat("02_DiD_Main.R -- Vintage FE Triple-Diff Architecture\n")
cat("====================================================================\n")
cat(sprintf("Analysis: %d to %d | Treatment: %d\n", PANEL_START, PANEL_END, TREATMENT_YEAR))
cat(sprintf("Bootstrap: %s (%d reps)\n",
            ifelse(USE_BOOTSTRAP, "ENABLED", "DISABLED"), N_BOOTSTRAP))
cat(sprintf("Spec A: %s facilities | Spec B: %s facilities | Pooled: %s\n",
            format(uniqueN(specA_data$panel_id), big.mark = ","),
            format(uniqueN(specB_data$panel_id), big.mark = ","),
            format(uniqueN(pooled_data$panel_id), big.mark = ",")))
cat("====================================================================\n\n")


#==============================================================================
# SECTION 2: HELPER FUNCTIONS
#==============================================================================

# 2.1 Save DiD table (CSV + TXT + LaTeX)
save_standard_did_table <- function(models, headers, base_name, title,
                                     treatment_var, cluster_var = "state",
                                     use_bootstrap = FALSE, n_reps = 999,
                                     digits = 4) {

  stopifnot(length(models) == length(headers))

  results_list <- lapply(seq_along(models), function(i) {
    m <- models[[i]]
    if (is.null(m)) return(NULL)
    ct <- summary(m)$coeftable
    tx_idx <- grep(treatment_var, rownames(ct), fixed = FALSE)
    if (length(tx_idx) == 0) {
      warning(sprintf("[Model %d] Treatment var '%s' not in coeftable.", i, treatment_var))
      return(data.frame(Model = headers[[i]], Estimate = NA, Std_Error = NA,
                        t_stat = NA, p_value_conv = NA, p_value_webb = NA,
                        CI_low_webb = NA, CI_high_webb = NA, N_obs = NA))
    }
    row <- ct[tx_idx[1], , drop = FALSE]
    tx_name <- rownames(ct)[tx_idx[1]]
    p_conv <- round(row[, "Pr(>|t|)"], 4)
    p_webb <- NA_real_; ci_lo <- NA_real_; ci_hi <- NA_real_

    if (use_bootstrap) {
      tryCatch({
        boot_w <- fwildclusterboot::boottest(
          m, param = tx_name, clustid = cluster_var,
          B = n_reps, type = "webb", seed = 20260202)
        p_webb <- round(boot_w$p_val, 4)
        ci_lo  <- round(boot_w$conf_int[[1]], digits)
        ci_hi  <- round(boot_w$conf_int[[2]], digits)
      }, error = function(e) message(sprintf("  [Boot %d] %s", i, e$message)))
    }

    data.frame(Model = headers[[i]],
               Estimate = round(row[, "Estimate"], digits),
               Std_Error = round(row[, "Std. Error"], digits),
               t_stat = round(row[, "t value"], 3),
               p_value_conv = p_conv, p_value_webb = p_webb,
               CI_low_webb = ci_lo, CI_high_webb = ci_hi,
               N_obs = nobs(m), stringsAsFactors = FALSE)
  })

  results_dt <- rbindlist(results_list[!sapply(results_list, is.null)], fill = TRUE)
  fwrite(results_dt, file.path(OUTPUT_TABLES, paste0(base_name, ".csv")))

  # TXT
  sink(file.path(OUTPUT_TABLES, paste0(base_name, ".txt")))
  cat(title, "\n", strrep("=", nchar(title)), "\n\n")
  print(as.data.frame(results_dt))
  for (i in seq_along(models)) {
    if (!is.null(models[[i]])) {
      cat(sprintf("\n--- %s ---\n", headers[[i]]))
      print(summary(models[[i]]))
    }
  }
  sink()

  # LaTeX
  keep <- !sapply(models, is.null)
  tryCatch(
    etable(models[keep], title = title, tex = TRUE,
           file = file.path(OUTPUT_TABLES, paste0(base_name, ".tex")),
           digits = digits),
    error = function(e) message(sprintf("  [LaTeX] %s", e$message))
  )

  cat(sprintf("  Saved: %s (.csv / .txt / .tex)\n", base_name))
  invisible(results_dt)
}

# 2.2 Save Cox results
save_cox_results <- function(models, headers, base_name, title) {
  results_list <- lapply(seq_along(models), function(i) {
    m <- models[[i]]
    if (is.null(m)) return(NULL)
    s <- summary(m)
    data.frame(Model = headers[[i]],
               HR = round(exp(coef(m))[1], 4),
               coef = round(coef(m)[1], 4),
               se = round(s$coefficients[1, "se(coef)"], 4),
               z = round(s$coefficients[1, "z"], 3),
               p = round(s$coefficients[1, "Pr(>|z|)"], 4),
               N = m$n, Events = m$nevent)
  })
  results_dt <- rbindlist(results_list[!sapply(results_list, is.null)])
  fwrite(results_dt, file.path(OUTPUT_TABLES, paste0(base_name, ".csv")))
  cat(sprintf("  Saved: %s.csv\n", base_name))
  invisible(results_dt)
}

# 2.3 Extract DiD coefficient
extract_did <- function(m, tvar = "did_term") {
  if (is.null(m)) return(list(beta = NA, se = NA, p = NA, n = NA))
  s  <- summary(m, cluster = ~state)
  ct <- coeftable(s)
  idx <- grep(tvar, rownames(ct), fixed = TRUE)[1]
  if (is.na(idx)) return(list(beta = NA, se = NA, p = NA, n = m$nobs))
  list(beta = ct[idx, "Estimate"], se = ct[idx, "Std. Error"],
       p = ct[idx, "Pr(>|t|)"], n = m$nobs)
}

# 2.4 Significance stars
stars_fn <- function(p) {
  if (is.na(p)) return("")
  if (p < 0.01) return("$^{***}$")
  if (p < 0.05) return("$^{**}$")
  if (p < 0.10) return("$^{*}$")
  ""
}

# 2.5 Publication event study plot
plot_event_study_pub <- function(model, title = "", subtitle = NULL,
                                 ylab = "Treatment Effect",
                                 xlab = "Years Relative to Treatment (1999)",
                                 ref_period = -1, treatment_line = -0.5,
                                 color_pre = "#4575B4", color_post = "#D73027",
                                 ci_alpha = 0.15, pre_trend_p = NULL,
                                 base_size = 13, filename = NULL,
                                 width_px = 2800, height_px = 1600) {

  ct <- as.data.table(broom::tidy(model, conf.int = TRUE))
  ct <- ct[grepl("rel_year|event_time", term)]
  ct[, rel_year := as.numeric(gsub(".*::(-?[0-9]+).*", "\\1", term))]
  ct <- ct[!is.na(rel_year)]

  ref_row <- data.table(term = "reference", estimate = 0, std.error = 0,
    statistic = NA, p.value = NA, conf.low = 0, conf.high = 0,
    rel_year = ref_period)
  ct <- rbind(ct, ref_row, fill = TRUE)
  setorder(ct, rel_year)
  ct[, period := fifelse(rel_year < 0, "Pre-treatment", "Post-treatment")]
  ct[rel_year == ref_period, period := "Reference"]

  if (!is.null(pre_trend_p) && is.null(subtitle)) {
    subtitle <- sprintf("Pre-trend F-test: p = %.3f | Reference: t = %d",
                         pre_trend_p, ref_period)
  }

  p <- ggplot(ct, aes(x = rel_year, y = estimate)) +
    annotate("rect", xmin = min(ct$rel_year) - 0.5, xmax = treatment_line,
             ymin = -Inf, ymax = Inf, fill = "grey90", alpha = 0.5) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.5) +
    geom_vline(xintercept = treatment_line, linetype = "solid",
               color = "grey30", linewidth = 0.6) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
                fill = ifelse(ct$rel_year < 0, color_pre, color_post),
                alpha = ci_alpha) +
    geom_point(aes(color = period), size = 2.5, shape = 19) +
    geom_line(color = "grey40", linewidth = 0.4, alpha = 0.6) +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high, color = period),
                  width = 0.25, linewidth = 0.5) +
    scale_color_manual(values = c("Pre-treatment" = color_pre,
                                   "Post-treatment" = color_post,
                                   "Reference" = "black"), guide = "none") +
    labs(title = title, subtitle = subtitle, x = xlab, y = ylab) +
    annotate("text", x = treatment_line + 0.3,
             y = max(ct$conf.high, na.rm = TRUE) * 0.92,
             label = "Reform ->", fontface = "italic",
             size = 3.5, hjust = 0, color = "grey30") +
    theme_minimal(base_size = base_size) +
    theme(plot.title = element_text(face = "bold", size = base_size + 3),
          plot.subtitle = element_text(color = "grey40", size = base_size - 1),
          panel.grid.minor = element_blank(), panel.grid.major.x = element_blank())

  if (!is.null(filename)) {
    ggsave(filename, plot = p, width = width_px / 200, height = height_px / 200,
           dpi = 200, bg = "white")
    ggsave(sub("\\.png$", ".pdf", filename), plot = p,
           width = width_px / 200, height = height_px / 200, device = cairo_pdf)
    cat(sprintf("  Saved: %s (.png + .pdf)\n", basename(filename)))
  }
  invisible(list(plot = p, data = ct))
}


#==============================================================================
# SECTION 3: PARALLEL TRENDS VALIDATION (6-TEST BATTERY)
#==============================================================================
# Expanded from 4-test to 6-test to include the new vintage FE specifications.
# Tests 1-4: Original battery. Tests 5-6: Pooled + vintage_cohort^year FE.
# Full diagnostics (HonestDiD, Roth, Bacon) are in 03_DiD_Assumptions.R.
#==============================================================================

cat("\n========================================\n")
cat("SECTION 3: PARALLEL TRENDS VALIDATION\n")
cat("========================================\n\n")

cat("Six-test battery:\n")
cat("  1. Pooled, no controls         -> Expected: REJECT\n")
cat("  2. Spec A, no controls         -> Expected: NOT reject\n")
cat("  3. Spec B, no controls         -> Expected: REJECT\n")
cat("  4. Spec B + mandate_active     -> Expected: Attenuated\n")
cat("  5. Pooled + vintage FE + mand  -> Expected: NOT reject [KEY TEST]\n")
cat("  6. Pooled + coarse vintage FE  -> Expected: NOT reject\n\n")

pre_window <- annual_data[panel_year >= 1990 & panel_year <= 1998]

run_pt_model <- function(dt, extra_rhs = NULL, extra_fe = NULL, label = "") {
  rhs <- "i(rel_year_1999, texas_treated, ref = -1)"
  if (!is.null(extra_rhs)) rhs <- paste(rhs, "+", extra_rhs)
  fe <- "panel_id + panel_year"
  if (!is.null(extra_fe)) fe <- paste(fe, "+", extra_fe)
  fml <- as.formula(paste("closure_event ~", rhs, "|", fe))
  m <- tryCatch(
    feols(fml, data = dt, cluster = ~state),
    error = function(e) { message(sprintf("  PT [%s] failed: %s", label, e$message)); NULL })
  if (is.null(m)) return(data.table(spec = label, f_stat = NA, p_value = NA, interpretation = "FAILED"))
  w <- tryCatch(fixest::wald(m, "rel_year_1999"), error = function(e) NULL)
  if (is.null(w)) return(data.table(spec = label, f_stat = NA, p_value = NA, interpretation = "Wald failed"))
  data.table(spec = label, f_stat = round(w$stat, 3), p_value = round(w$p, 4),
             interpretation = fifelse(w$p > 0.10, "NOT rejected", "REJECTED"))
}

pt_results <- rbindlist(list(
  run_pt_model(pre_window, label = "1. Pooled, no controls"),
  run_pt_model(pre_window[spec_A_eligible == 1], label = "2. Spec A, no controls"),
  run_pt_model(pre_window[spec_B_eligible == 1], label = "3. Spec B, no controls"),
  run_pt_model(pre_window[spec_B_eligible == 1], extra_rhs = "mandate_active",
               label = "4. Spec B + mandate_active"),
  run_pt_model(pre_window, extra_rhs = "mandate_active",
               extra_fe = "vintage_cohort^panel_year",
               label = "5. Pooled + vintage FE + mandate_active [KEY]"),
  run_pt_model(pre_window, extra_rhs = "mandate_active",
               extra_fe = "vintage_cohort_coarse^panel_year",
               label = "6. Pooled + coarse vintage FE")
))

cat("\n=== PARALLEL TRENDS VALIDATION (Table B.4) ===\n")
print(pt_results)

fwrite(pt_results, file.path(OUTPUT_TABLES, "TableB4_Parallel_Trends_Validation.csv"))

# LaTeX
tex_pt <- c(
  "\\begin{table}[htbp]", "\\centering",
  "\\caption{Parallel Trends Validation: Pre-Period F-Tests (1990--1997)}",
  "\\label{tbl:parallel-trends}",
  "\\begin{tabular}{rlccl}", "\\toprule",
  "\\textbf{Test} & \\textbf{Specification} & \\textbf{F} & \\textbf{p} & \\textbf{Result} \\\\",
  "\\midrule")
for (i in seq_len(nrow(pt_results))) {
  r <- pt_results[i]
  tex_pt <- c(tex_pt, sprintf("%d & %s & %.3f & %.4f & %s \\\\",
    i, gsub("_", "\\\\_", r$spec), r$f_stat, r$p_value, r$interpretation))
}
tex_pt <- c(tex_pt, "\\bottomrule",
  "\\multicolumn{5}{p{0.95\\textwidth}}{\\footnotesize \\textit{Notes:}",
  "Joint F-test of pre-treatment event-study coefficients, ref = $k=-1$.",
  "Test 5 is the key validation for the headline specification: pooled sample",
  "with vintage\\_cohort $\\times$ year FE absorbing cohort-specific temporal",
  "patterns and \\texttt{mandate\\_active} absorbing TX-specific mandate impulse.}",
  "\\end{tabular}", "\\end{table}")
writeLines(tex_pt, file.path(OUTPUT_TABLES, "TableB4_Parallel_Trends_Validation.tex"))
cat("  Saved: TableB4_Parallel_Trends_Validation (.csv / .tex)\n")

# Store key p-values
pooled_vfe_p <- pt_results[grepl("KEY", spec), p_value]
specA_p      <- pt_results[grepl("Spec A", spec), p_value]

if (length(pooled_vfe_p) > 0 && !is.na(pooled_vfe_p) && pooled_vfe_p > 0.10) {
  cat("\n  PASS: Pooled + vintage FE passes PT validation (p > 0.10).\n")
  cat("  Headline specification validated.\n")
} else {
  cat("\n  WARNING: Pooled + vintage FE PT test p <= 0.10. Investigate cell sparsity.\n")
}


#==============================================================================
# SECTION 4: TABLE 3 -- HEADLINE: POOLED + VINTAGE FE
#==============================================================================
# Specification: Y_it = alpha_i + delta_{v(i),t} + beta*(TX*Post) +
#                gamma*mandate_active + epsilon_it
# FE: facility + vintage_cohort^panel_year
# Cluster: state (20)
#==============================================================================

cat("\n========================================\n")
cat("SECTION 4: TABLE 3 -- HEADLINE (Pooled + Vintage FE)\n")
cat("========================================\n\n")

# -- 4.1 Closure Probability --
cat("--- 4.1 Closure ---\n")

m_closure_H1 <- feols(
  closure_event ~ did_term + mandate_active |
    panel_id + vintage_cohort^panel_year,
  data = pooled_data, cluster = ~state, lean = FALSE)

m_closure_H2 <- feols(
  closure_event ~ did_term + mandate_window_3yr |
    panel_id + vintage_cohort^panel_year,
  data = pooled_data, cluster = ~state, lean = FALSE)

m_closure_H3 <- tryCatch(
  feols(closure_event ~ did_term + mandate_active |
          panel_id + vintage_cohort^panel_year + vintage_cohort[panel_year],
        data = pooled_data, cluster = ~state, lean = FALSE),
  error = function(e) {
    cat(sprintf("  H3 (vintage trends) failed: %s\n", e$message))
    NULL
  })

cat("H1 (+ mandate_active):\n"); print(summary(m_closure_H1))
cat("\nH2 (+ mandate_window_3yr):\n"); print(summary(m_closure_H2))
if (!is.null(m_closure_H3)) { cat("\nH3 (+ vintage trends):\n"); print(summary(m_closure_H3)) }

# -- 4.2 Exit | Closure --
cat("\n--- 4.2 Exit | Closure ---\n")

closure_pool <- pooled_data[closure_event == 1]
closure_pool[, replace_indicator := as.integer(exit_flag == 0)]

m_exit_H1 <- tryCatch(
  feols(exit_flag ~ did_term + mandate_active |
          panel_id + vintage_cohort^panel_year,
        data = closure_pool, cluster = ~state, lean = FALSE),
  error = function(e) { message("  Exit H1 failed: ", e$message); NULL })

# -- 4.3 Replace | Closure --
cat("\n--- 4.3 Replace | Closure ---\n")

m_rep_H1 <- tryCatch(
  feols(replace_indicator ~ did_term + mandate_active |
          panel_id + vintage_cohort^panel_year,
        data = closure_pool, cluster = ~state, lean = FALSE),
  error = function(e) { message("  Replace H1 failed: ", e$message); NULL })

# -- 4.4 Save Table 3 --
table3_models <- list(m_closure_H1, m_closure_H2, m_closure_H3,
                      m_exit_H1, m_rep_H1)
table3_headers <- c("Closure (H1)", "Closure (H2: 3yr)",
                     "Closure (H3: trends)", "Exit|Cls", "Replace|Cls")
keep3 <- !sapply(table3_models, is.null)

save_standard_did_table(
  models = table3_models[keep3], headers = table3_headers[keep3],
  base_name = "Table3_Headline_Pooled_VintageFE",
  title = "Table 3: Headline -- Pooled + Vintage-Cohort x Year FE",
  treatment_var = "did_term", cluster_var = "state",
  use_bootstrap = USE_BOOTSTRAP, n_reps = N_BOOTSTRAP, digits = 6)

# -- 4.5 Event Study (Figure 6 -- HEADLINE) --
cat("\n--- 4.5 Event Study: Pooled + Vintage FE (Figure 6) ---\n")

es_pool <- pooled_data[panel_year >= ES_START & panel_year <= ES_END]
es_pool[, rel_year_bin := pmax(pmin(rel_year_1999, rel_max), rel_min)]

model_es_H <- feols(
  closure_event ~ i(rel_year_bin, texas_treated, ref = -1) + mandate_active |
    panel_id + vintage_cohort^panel_year,
  data = es_pool, cluster = ~state)

pre_coefs_H <- names(coef(model_es_H))
pre_coefs_H <- pre_coefs_H[grepl("::-[2-9]|::-1[0-9]", pre_coefs_H)]
pre_pval_H  <- tryCatch(wald(model_es_H, keep = pre_coefs_H)$p,
                          error = function(e) NA_real_)

es_H_fig <- plot_event_study_pub(
  model = model_es_H,
  title = "Effect of Insurance Privatization on Tank Closure Probability",
  subtitle = sprintf("Pooled + Vintage-Cohort x Year FE | Pre-trend p = %.3f",
                      ifelse(is.na(pre_pval_H), 0, pre_pval_H)),
  ylab = "Effect on Pr(Tank Closure)",
  ref_period = -1, pre_trend_p = pre_pval_H,
  color_pre = "#4575B4", color_post = "#D73027",
  filename = file.path(OUTPUT_FIGURES, "JMP_Figure_6_Event_Study_Headline.png"))

fwrite(as.data.table(broom::tidy(model_es_H, conf.int = TRUE)),
       file.path(OUTPUT_TABLES, "Figure6_ES_Coefficients_Headline.csv"))

# Economic interpretation
beta_H <- coef(m_closure_H1)["did_term"]
mean_pre <- mean(pooled_data[panel_year < TREATMENT_YEAR]$closure_event, na.rm = TRUE)
cat(sprintf("\n--- Economic Interpretation (Headline) ---\n"))
cat(sprintf("beta(closure) = %.6f | Pre-mean = %.4f | Pct change = %.1f%%\n",
            beta_H, mean_pre, 100 * beta_H / mean_pre))


#==============================================================================
# SECTION 5: TABLE 4 -- SPEC A ROBUSTNESS
#==============================================================================
# Confirms result not driven by pre-1988 tanks. No mandate FE needed.
#==============================================================================

cat("\n========================================\n")
cat("SECTION 5: TABLE 4 -- SPEC A ROBUSTNESS\n")
cat("========================================\n\n")

m_closure_A_base <- feols(
  closure_event ~ did_term | panel_id + panel_year,
  data = specA_data, cluster = ~state, lean = FALSE)

m_closure_A_ctrl <- feols(
  closure_event ~ did_term + mean_age_1998 | panel_id + panel_year,
  data = specA_data, cluster = ~state, lean = FALSE)

cat("Spec A baseline:\n"); print(summary(m_closure_A_base))
cat("\nSpec A + age:\n"); print(summary(m_closure_A_ctrl))

# Exit / Replace (conditional on closure)
closure_A <- specA_data[closure_event == 1]
closure_A[, replace_indicator := as.integer(exit_flag == 0)]

m_exit_A <- tryCatch(
  feols(exit_flag ~ did_term | panel_id + panel_year,
        data = closure_A, cluster = ~state, lean = FALSE),
  error = function(e) { message("  Exit A failed: ", e$message); NULL })

m_rep_A <- tryCatch(
  feols(replace_indicator ~ did_term | panel_id + panel_year,
        data = closure_A, cluster = ~state, lean = FALSE),
  error = function(e) { message("  Replace A failed: ", e$message); NULL })

table4_models <- list(m_closure_A_base, m_closure_A_ctrl, m_exit_A, m_rep_A)
table4_headers <- c("Closure", "Closure + Age", "Exit|Cls", "Replace|Cls")
keep4 <- !sapply(table4_models, is.null)

save_standard_did_table(
  models = table4_models[keep4], headers = table4_headers[keep4],
  base_name = "Table4_SpecA_Robustness",
  title = "Table 4: Spec A Robustness (Post-1988, Mandate-Free)",
  treatment_var = "did_term", cluster_var = "state",
  use_bootstrap = USE_BOOTSTRAP, n_reps = N_BOOTSTRAP, digits = 6)

# Event study (Appendix Figure 6B)
cat("\n--- Event Study: Spec A (Figure 6B) ---\n")
es_A <- specA_data[panel_year >= ES_START & panel_year <= ES_END]
es_A[, rel_year_bin := pmax(pmin(rel_year_1999, rel_max), rel_min)]

model_es_A <- feols(
  closure_event ~ i(rel_year_bin, texas_treated, ref = -1) | panel_id + panel_year,
  data = es_A, cluster = ~state)

pre_coefs_A <- names(coef(model_es_A))
pre_coefs_A <- pre_coefs_A[grepl("::-[2-9]|::-1[0-9]", pre_coefs_A)]
pre_pval_A  <- tryCatch(wald(model_es_A, keep = pre_coefs_A)$p, error = function(e) NA_real_)

plot_event_study_pub(
  model = model_es_A,
  title = "Spec A: Post-1988 Tanks (Mandate-Free)",
  ylab = "Effect on Pr(Tank Closure)",
  pre_trend_p = pre_pval_A,
  filename = file.path(OUTPUT_FIGURES, "JMP_Figure_6B_Event_Study_SpecA.png"))


#==============================================================================
# SECTION 6: TABLE 5 -- SPEC B ROBUSTNESS (MANDATE CONTROLS ESCALATION)
#==============================================================================

cat("\n========================================\n")
cat("SECTION 6: TABLE 5 -- SPEC B ROBUSTNESS\n")
cat("========================================\n\n")

m_specB_1 <- feols(
  closure_event ~ did_term + mandate_active | panel_id + panel_year,
  data = specB_data, cluster = ~state, lean = FALSE)

m_specB_2 <- feols(
  closure_event ~ did_term + mandate_active |
    panel_id + panel_year + mandate_cohort^panel_year,
  data = specB_data, cluster = ~state, lean = FALSE)

m_specB_3 <- tryCatch(
  feols(closure_event ~ did_term |
          panel_id + panel_year + mandate_cohort[panel_year],
        data = specB_data, cluster = ~state, lean = FALSE),
  error = function(e) { message("  Spec B L3 failed: ", e$message); NULL })

specB_models  <- list(m_specB_1, m_specB_2, m_specB_3)
specB_headers <- c("+ mandate_active", "+ Cohort x Year FE", "Cohort Trends")
keepB <- !sapply(specB_models, is.null)

save_standard_did_table(
  models = specB_models[keepB], headers = specB_headers[keepB],
  base_name = "Table5_SpecB_Mandate_Controls",
  title = "Table 5: Spec B Robustness -- Mandate Control Escalation",
  treatment_var = "did_term", cluster_var = "state",
  use_bootstrap = USE_BOOTSTRAP, n_reps = N_BOOTSTRAP, digits = 6)

# Event study (Appendix Figure 6C)
es_B <- specB_data[panel_year >= ES_START & panel_year <= ES_END]
es_B[, rel_year_bin := pmax(pmin(rel_year_1999, rel_max), rel_min)]

model_es_B <- feols(
  closure_event ~ i(rel_year_bin, texas_treated, ref = -1) + mandate_active |
    panel_id + panel_year,
  data = es_B, cluster = ~state)

plot_event_study_pub(
  model = model_es_B,
  title = "Spec B: Pre-1988 Tanks + Mandate Control",
  ylab = "Effect on Pr(Tank Closure)",
  filename = file.path(OUTPUT_FIGURES, "JMP_Figure_6C_Event_Study_SpecB.png"))


#==============================================================================
# SECTION 7: TABLE 6 -- REPORTED LEAK DiD
#==============================================================================

cat("\n========================================\n")
cat("SECTION 7: TABLE 6 -- REPORTED LEAK DiD\n")
cat("========================================\n\n")

# Headline: Pooled + vintage FE
m_leak_H <- feols(
  leak_year ~ did_term + mandate_active |
    panel_id + vintage_cohort^panel_year,
  data = pooled_data, cluster = ~state, lean = FALSE)

# Spec A
m_leak_A <- feols(
  leak_year ~ did_term | panel_id + panel_year,
  data = specA_data, cluster = ~state, lean = FALSE)

# Spec B + mandate
m_leak_B <- feols(
  leak_year ~ did_term + mandate_active | panel_id + panel_year,
  data = specB_data, cluster = ~state, lean = FALSE)

cat("Leak Headline (Pooled + VFE):\n"); print(summary(m_leak_H))
cat("\nLeak Spec A:\n"); print(summary(m_leak_A))
cat("\nLeak Spec B + mandate:\n"); print(summary(m_leak_B))

save_standard_did_table(
  models = list(m_leak_H, m_leak_A, m_leak_B),
  headers = c("Pooled + VFE (headline)", "Spec A", "Spec B + Mandate"),
  base_name = "Table6_Reported_Leak",
  title = "Table 6: Reported Leak Probability",
  treatment_var = "did_term", cluster_var = "state",
  use_bootstrap = USE_BOOTSTRAP, n_reps = N_BOOTSTRAP)

# Leak Event Study (Figure 7 -- Headline)
cat("\n--- Leak Event Study (Figure 7) ---\n")
es_leak <- pooled_data[panel_year >= ES_START & panel_year <= ES_END]
es_leak[, rel_year_bin := pmax(pmin(rel_year_1999, rel_max), rel_min)]

model_es_leak <- feols(
  leak_year ~ i(rel_year_bin, texas_treated, ref = -1) + mandate_active |
    panel_id + vintage_cohort^panel_year,
  data = es_leak, cluster = ~state)

pre_coefs_lk <- names(coef(model_es_leak))
pre_coefs_lk <- pre_coefs_lk[grepl("::-[2-9]|::-1[0-9]", pre_coefs_lk)]
pre_pval_lk  <- tryCatch(wald(model_es_leak, keep = pre_coefs_lk)$p,
                           error = function(e) NA_real_)

plot_event_study_pub(
  model = model_es_leak,
  title = "Effect of Insurance Privatization on Reported Leak Probability",
  ylab = "Effect on Pr(Reported Leak)",
  pre_trend_p = pre_pval_lk,
  filename = file.path(OUTPUT_FIGURES, "JMP_Figure_7_Leak_Event_Study.png"))


#==============================================================================
# SECTION 8: HTE -- AGE BIN x TREATMENT (Table 7)
#==============================================================================
# Maps age-specific heterogeneous treatment effects using canonical age bins.
# Interacts did_term with age_bin (ref = 0-4).
#==============================================================================

cat("\n========================================\n")
cat("SECTION 8: HTE -- AGE BIN x TREATMENT\n")
cat("========================================\n\n")

# Pooled + vintage FE, interacted with age_bin
m_hte_age_pool <- feols(
  closure_event ~ did_term + did_term:age_bin + age_bin + mandate_active |
    panel_id + vintage_cohort^panel_year,
  data = pooled_data, cluster = ~state, lean = FALSE)

cat("HTE Age Bin (Pooled + VFE):\n"); print(summary(m_hte_age_pool))

# Spec A
m_hte_age_A <- feols(
  closure_event ~ did_term + did_term:age_bin + age_bin | panel_id + panel_year,
  data = specA_data, cluster = ~state, lean = FALSE)

cat("\nHTE Age Bin (Spec A):\n"); print(summary(m_hte_age_A))

save_standard_did_table(
  models = list(m_hte_age_pool, m_hte_age_A),
  headers = c("Pooled + VFE", "Spec A"),
  base_name = "Table7_HTE_AgeBin",
  title = "Table 7: HTE -- Age Bin x Treatment",
  treatment_var = "did_term", cluster_var = "state",
  use_bootstrap = USE_BOOTSTRAP, n_reps = N_BOOTSTRAP)

# -- Figure 8: HTE Age Bin Coefficient Plot --
cat("\n--- Figure 8: HTE Age Bin Plot ---\n")

ct_hte <- as.data.table(broom::tidy(m_hte_age_pool, conf.int = TRUE))
ct_hte <- ct_hte[grepl("did_term:age_bin", term)]
ct_hte[, age_bin_label := gsub("did_term:age_bin", "", term)]
ct_hte[, age_bin_label := factor(age_bin_label, levels = AGE_BIN_LABELS)]

# Add base effect (did_term main) for reference = 0-4
base_row <- ct_hte[0]
base_coef <- coef(m_hte_age_pool)["did_term"]
base_se   <- summary(m_hte_age_pool)$coeftable["did_term", "Std. Error"]
base_row <- data.table(term = "did_term:age_bin0-4",
  estimate = base_coef, std.error = base_se,
  conf.low = base_coef - 1.96 * base_se,
  conf.high = base_coef + 1.96 * base_se,
  age_bin_label = factor("0-4", levels = AGE_BIN_LABELS))
ct_hte_plot <- rbind(base_row, ct_hte, fill = TRUE)

# Interaction effects = base + interaction
ct_hte_plot[age_bin_label != "0-4",
            `:=`(estimate = estimate + base_coef,
                 conf.low = conf.low + base_coef,
                 conf.high = conf.high + base_coef)]

p_hte <- ggplot(ct_hte_plot[!is.na(age_bin_label)],
                aes(x = age_bin_label, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_point(color = COL_TX, size = 3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width = 0.2, color = COL_TX) +
  labs(title = "Heterogeneous Treatment Effects by Tank Age",
       subtitle = "Total effect (base + interaction) by canonical age bin",
       x = "Tank Age Bin (years)", y = "Treatment Effect on Pr(Closure)") +
  theme_pub()

ggsave(file.path(OUTPUT_FIGURES, "JMP_Figure_8_HTE_AgeBin.png"),
       p_hte, width = 10, height = 6, dpi = 300, bg = "white")
ggsave(file.path(OUTPUT_FIGURES, "JMP_Figure_8_HTE_AgeBin.pdf"),
       p_hte, width = 10, height = 6, device = cairo_pdf)
cat("  Saved: JMP_Figure_8_HTE_AgeBin (.png + .pdf)\n")


#==============================================================================
# SECTION 9: HTE -- WALL TYPE x TREATMENT (Table 8)
#==============================================================================

cat("\n========================================\n")
cat("SECTION 9: HTE -- WALL TYPE x TREATMENT\n")
cat("========================================\n\n")

wall_col <- NULL
for (cand in c("pct_single_wall", "has_single_walled", "any_single_walled")) {
  if (cand %in% names(annual_data)) { wall_col <- cand; break }
}

if (!is.null(wall_col)) {
  cat(sprintf("Using: %s\n", wall_col))

  fml_hte_w <- as.formula(sprintf(
    "closure_event ~ did_term + did_term:%s + %s + mandate_active | panel_id + vintage_cohort^panel_year",
    wall_col, wall_col))

  m_hte_wall_H <- feols(fml_hte_w, data = pooled_data, cluster = ~state, lean = FALSE)
  cat("Wall HTE (Pooled + VFE):\n"); print(summary(m_hte_wall_H))

  fml_hte_wA <- as.formula(sprintf(
    "closure_event ~ did_term + did_term:%s + %s | panel_id + panel_year", wall_col, wall_col))
  m_hte_wall_A <- feols(fml_hte_wA, data = specA_data, cluster = ~state, lean = FALSE)

  save_standard_did_table(
    models = list(m_hte_wall_H, m_hte_wall_A),
    headers = c("Pooled + VFE", "Spec A"),
    base_name = "Table8_HTE_WallType",
    title = "Table 8: HTE -- Wall Type x Treatment",
    treatment_var = "did_term", cluster_var = "state",
    use_bootstrap = USE_BOOTSTRAP, n_reps = N_BOOTSTRAP)
} else {
  cat("  WARNING: No wall-type variable found. Skipping.\n")
}


#==============================================================================
# SECTION 10: ADDITIONAL MODELS
#==============================================================================
# Retained from prior architecture: Age at Closure, Revealed Leaks,
# Competing Risks. Now also estimated with vintage FE where applicable.
#==============================================================================

cat("\n========================================\n")
cat("SECTION 10: ADDITIONAL MODELS\n")
cat("========================================\n\n")

# -- 10.1 Model 3A: Age at Closure (County FE) --
cat("--- 10.1 Age at Closure ---\n")

model_3a_data <- closed_tanks[
  !is.na(age_at_closure) & !is.na(county_fips_fac) &
    closure_year >= PANEL_START & closure_year <= PANEL_END]

if ("texas_treated" %in% names(model_3a_data)) {
  model_3a_data[, texas := texas_treated]
} else if (!"texas" %in% names(model_3a_data)) {
  model_3a_data[, texas := as.integer(state == "TX")]
}
model_3a_data[, post := as.integer(closure_year >= POST_YEAR)]
model_3a_data[, texas_post := texas * post]

model_3a <- tryCatch(
  feols(age_at_closure ~ texas_post | county_fips_fac + closure_year,
        data = model_3a_data, cluster = ~state),
  error = function(e) { message("  Model 3A failed: ", e$message); NULL })

if (!is.null(model_3a)) {
  cat("Model 3A:\n"); print(summary(model_3a))
  save_standard_did_table(
    models = list(model_3a), headers = c("Age at Closure"),
    base_name = "Model_3A_Age_at_Closure",
    title = "Model 3A: Age at Closure (County FE)",
    treatment_var = "texas_post", cluster_var = "state",
    use_bootstrap = USE_BOOTSTRAP, n_reps = N_BOOTSTRAP)
}

# -- 10.2 Model 4: Revealed Leaks at Closure --
cat("\n--- 10.2 Revealed Leaks at Closure ---\n")

model_4_data <- annual_data[n_closures > 0 &
                              !is.na(tank_closure_revealed) &
                              !is.na(county_fips_fac)]
model_4_data[, revealed_primary := as.integer(tank_closure_revealed > 0)]

if (nrow(model_4_data) > 100) {
  m4_primary <- feols(revealed_primary ~ did_term | county_fips_fac + panel_year,
                       data = model_4_data, cluster = ~state)
  cat("Revealed leaks (primary):\n"); print(summary(m4_primary))
  save_standard_did_table(
    models = list(m4_primary), headers = "0-60d Window",
    base_name = "Model_4_Revealed_Leaks",
    title = "Model 4: Revealed Leaks at Closure",
    treatment_var = "did_term", cluster_var = "state",
    use_bootstrap = USE_BOOTSTRAP, n_reps = N_BOOTSTRAP)
}

# -- 10.3 Model 5: Competing Risks --
cat("\n--- 10.3 Competing Risks ---\n")

compete_data <- annual_data[, .(
  texas = first(texas_treated), state = first(state),
  yr_first_leak = first(na.omit(year_of_first_leak)),
  yr_first_close = fifelse(any(closure_event == 1, na.rm = TRUE),
    min(panel_year[closure_event == 1], na.rm = TRUE), NA_integer_),
  last_obs = max(panel_year)
), by = panel_id]

compete_data[, event_type := fcase(
  !is.na(yr_first_leak) & (is.na(yr_first_close) | yr_first_leak <= yr_first_close), 1L,
  !is.na(yr_first_close) & (is.na(yr_first_leak) | yr_first_close < yr_first_leak), 2L,
  default = 0L)]

compete_data[, event_time := fcase(
  event_type == 1L, pmax(yr_first_leak - POST_YEAR, 0),
  event_type == 2L, pmax(yr_first_close - POST_YEAR, 0),
  default = pmin(last_obs, PANEL_END) - POST_YEAR)]
compete_data <- compete_data[!is.na(event_time) & event_time >= 0]

surv_leak  <- Surv(compete_data$event_time, as.integer(compete_data$event_type == 1))
surv_close <- Surv(compete_data$event_time, as.integer(compete_data$event_type == 2))

m5a_leak  <- coxph(surv_leak  ~ texas, data = compete_data, cluster = state)
m5a_close <- coxph(surv_close ~ texas, data = compete_data, cluster = state)

cat("CS-Cox (Leak):\n"); print(summary(m5a_leak))
cat("\nCS-Cox (Closure):\n"); print(summary(m5a_close))

save_cox_results(
  models = list(m5a_leak, m5a_close),
  headers = c("Event: First Leak", "Event: First Closure"),
  base_name = "Model_5A_Cause_Specific_Cox",
  title = "Model 5A: Cause-Specific Cox")


#==============================================================================
# SECTION 11: APPENDIX ROBUSTNESS
#==============================================================================

cat("\n========================================\n")
cat("SECTION 11: APPENDIX ROBUSTNESS\n")
cat("========================================\n\n")

# 11.1 MD-Excluded
m_noMD_H <- feols(
  closure_event ~ did_term + mandate_active | panel_id + vintage_cohort^panel_year,
  data = pooled_data[state != "MD"], cluster = ~state, lean = FALSE)

m_noMD_A <- feols(
  closure_event ~ did_term | panel_id + panel_year,
  data = specA_data[state != "MD"], cluster = ~state, lean = FALSE)

save_standard_did_table(
  models = list(m_noMD_H, m_noMD_A),
  headers = c("Headline (no MD)", "Spec A (no MD)"),
  base_name = "TableB5_MD_Excluded",
  title = "Table B.5: MD-Excluded Robustness",
  treatment_var = "did_term", cluster_var = "state",
  use_bootstrap = USE_BOOTSTRAP, n_reps = N_BOOTSTRAP)

# 11.2 Vintage bin coarseness robustness (Table F.1)
cat("\n--- 11.2 Vintage Bin Coarseness ---\n")

m_coarse <- feols(
  closure_event ~ did_term + mandate_active |
    panel_id + vintage_cohort_coarse^panel_year,
  data = pooled_data, cluster = ~state, lean = FALSE)

m_binary <- feols(
  closure_event ~ did_term + mandate_active |
    panel_id + vintage_cell^panel_year,
  data = pooled_data, cluster = ~state, lean = FALSE)

save_standard_did_table(
  models = list(m_closure_H1, m_coarse, m_binary),
  headers = c("6-bin (headline)", "3-bin (coarse)", "2-bin (binary)"),
  base_name = "TableF1_Vintage_Bin_Robustness",
  title = "Table F.1: Vintage Bin Coarseness Robustness",
  treatment_var = "did_term", cluster_var = "state",
  use_bootstrap = USE_BOOTSTRAP, n_reps = N_BOOTSTRAP)

# 11.3 Vintage-cohort-specific treatment effects (homogeneity test)
cat("\n--- 11.3 Vintage-Specific Treatment Effects ---\n")

m_het_vintage <- feols(
  closure_event ~ did_term + did_term:vintage_cohort + mandate_active |
    panel_id + vintage_cohort^panel_year,
  data = pooled_data, cluster = ~state, lean = FALSE)

cat("Treatment x Vintage interactions (homogeneity test):\n")
print(summary(m_het_vintage))

# Wald test of joint nullity of interactions (H0: homogeneous beta)
vc_interact <- names(coef(m_het_vintage))
vc_interact <- vc_interact[grepl("did_term:vintage_cohort", vc_interact)]
if (length(vc_interact) > 0) {
  w_homog <- tryCatch(wald(m_het_vintage, keep = vc_interact), error = function(e) NULL)
  if (!is.null(w_homog)) {
    cat(sprintf("Homogeneity test: F = %.3f, p = %.4f\n", w_homog$stat, w_homog$p))
    if (w_homog$p > 0.10) cat("  PASS: Cannot reject homogeneous beta across vintages.\n")
  }
}

# 11.4 mandate_window_3yr Sensitivity
cat("\n--- 11.4 mandate_window_3yr Sensitivity ---\n")

m_specB_3yr <- feols(
  closure_event ~ did_term + mandate_window_3yr | panel_id + panel_year,
  data = specB_data, cluster = ~state, lean = FALSE)

save_standard_did_table(
  models = list(m_specB_1, m_specB_3yr),
  headers = c("Spec B + active", "Spec B + 3yr"),
  base_name = "TableB6_Mandate_Window_Sensitivity",
  title = "Table B.6: mandate_window_3yr Sensitivity",
  treatment_var = "did_term", cluster_var = "state",
  use_bootstrap = USE_BOOTSTRAP, n_reps = N_BOOTSTRAP)

#==============================================================================
# SECTION 12: DURATION / SURVIVAL MODELS (Cox DiD)
#==============================================================================
# This replaces the stub at the end of 02_DiD_Main.R
#
# METHODOLOGICAL RESOLUTION:
#   Q1: Use strata(vintage_cohort) for cohort-specific baseline hazards ✓
#   Q2: Use counting process format spanning full panel to preserve mandate_active ✓
#   Q3: Calendar time origin is the canonical DiD specification ✓
#==============================================================================

cat("\n========================================\n")
cat("SECTION 12: SURVIVAL MODELS (COX DiD)\n")
cat("========================================\n\n")

cat("Setting up counting process format for DiD survival analysis...\n")

# -----------------------------------------------------------------------------
# 12.1  COUNTING PROCESS FORMAT FOR DiD
# -----------------------------------------------------------------------------
# KEY INSIGHT: DiD requires observing pre-treatment and post-treatment periods.
# In survival models, this means using counting process (start-stop) format
# spanning the full panel, NOT truncating to post-1999 only.
#
# Each facility-year becomes an interval [tstart, tstop] where:
#   - mandate_active varies in pre-1999 intervals (1989-1993 for TX)
#   - did_term varies in post-1999 intervals (TX x Post)
# This preserves the full panel structure for DiD identification.
# -----------------------------------------------------------------------------

# Create a copy for survival analysis
pooled_data <- copy(annual_data)

# Create calendar time intervals (year-to-year)
pooled_data[, tstart := panel_year - 1]
pooled_data[, tstop  := panel_year]

# Create facility age intervals (for robustness check)
pooled_data[, age_start := avg_tank_age - 1]
pooled_data[, age_stop  := avg_tank_age]

# Drop invalid intervals
cox_data <- pooled_data[!is.na(tstart) & !is.na(tstop) & tstop > tstart]

cat(sprintf("  Counting process data: %s intervals from %s facilities\n",
            format(nrow(cox_data), big.mark = ","),
            format(uniqueN(cox_data$panel_id), big.mark = ",")))

# -----------------------------------------------------------------------------
# 12.2  HELPER FUNCTION FOR COX RESULTS
# -----------------------------------------------------------------------------

save_cox_results <- function(models, headers, base_name, title) {
  # Extract and format results
  results <- lapply(seq_along(models), function(i) {
    m <- models[[i]]
    if (is.null(m)) return(NULL)
    
    s <- summary(m)
    coef_table <- as.data.frame(s$coefficients)
    
    # Extract did_term coefficient
    if ("did_term" %in% rownames(coef_table)) {
      list(
        model = headers[i],
        coef = coef_table["did_term", "coef"],
        exp_coef = coef_table["did_term", "exp(coef)"],
        se = coef_table["did_term", "se(coef)"],
        z = coef_table["did_term", "z"],
        p = coef_table["did_term", "Pr(>|z|)"],
        n_events = s$nevent,
        n_obs = s$n
      )
    } else {
      NULL
    }
  })
  
  results <- Filter(Negate(is.null), results)
  
  if (length(results) > 0) {
    df <- data.table::rbindlist(results)
    fwrite(df, file.path(OUTPUT_TABLES, paste0(base_name, ".csv")))
    cat(sprintf("  Saved: %s.csv\n", base_name))
  }
}

# -----------------------------------------------------------------------------
# 12.3  MODEL A: CALENDAR TIME ORIGIN (DiD Canonical Specification)
# -----------------------------------------------------------------------------
# This is the Cox analogue of TWFE DiD:
#   - Baseline hazard = calendar time (like year FE in OLS)
#   - strata(vintage_cohort) = separate baseline per cohort
#   - did_term = TX x Post treatment effect
#   - mandate_active = TX-specific pre-treatment control (1989-1993)
# -----------------------------------------------------------------------------

cat("\nModel 12.1: Calendar Time Origin (TWFE Analogue)...\n")

m_cox_calendar <- tryCatch({
  coxph(
    Surv(tstart, tstop, closure_event) ~ 
      did_term + mandate_active + strata(vintage_cohort),
    data = cox_data,
    cluster = state
  )
}, error = function(e) {
  message("  ERROR: Cox (Calendar) failed: ", e$message)
  NULL
})

if (!is.null(m_cox_calendar)) {
  cat("\n--- Cox DiD: Calendar Time Origin ---\n")
  print(summary(m_cox_calendar))
  
  # Extract DiD effect
  coef_summary <- summary(m_cox_calendar)$coefficients
  if ("did_term" %in% rownames(coef_summary)) {
    hr <- exp(coef_summary["did_term", "coef"])
    p_val <- coef_summary["did_term", "Pr(>|z|)"]
    cat(sprintf("\n  DiD Treatment Effect:\n"))
    cat(sprintf("    Hazard Ratio: %.4f (p = %.4f)\n", hr, p_val))
    cat(sprintf("    Interpretation: TX facilities have %.1f%% %s closure hazard post-reform\n",
                abs((hr - 1) * 100),
                ifelse(hr > 1, "higher", "lower")))
  }
}

# -----------------------------------------------------------------------------
# 12.4  MODEL B: FACILITY AGE ORIGIN (Robustness Check)
# -----------------------------------------------------------------------------
# Alternative time origin based on facility age:
#   - Baseline hazard = facility age (lifecycle effects)
#   - Requires panel_year as covariate to control for calendar time
#   - Less aligned with DiD structure but useful robustness check
# -----------------------------------------------------------------------------

cat("\nModel 12.2: Facility Age Origin (Robustness)...\n")

# Filter to valid age intervals
cox_data_age <- cox_data[age_start >= 0 & age_stop > age_start & 
                         !is.na(age_start) & !is.na(age_stop)]

if (nrow(cox_data_age) > 0) {
  m_cox_age <- tryCatch({
    coxph(
      Surv(age_start, age_stop, closure_event) ~ 
        did_term + mandate_active + panel_year + strata(vintage_cohort),
      data = cox_data_age,
      cluster = state
    )
  }, error = function(e) {
    message("  ERROR: Cox (Age) failed: ", e$message)
    NULL
  })
  
  if (!is.null(m_cox_age)) {
    cat("\n--- Cox DiD: Age Time Origin ---\n")
    print(summary(m_cox_age))
    
    # Extract DiD effect
    coef_summary <- summary(m_cox_age)$coefficients
    if ("did_term" %in% rownames(coef_summary)) {
      hr <- exp(coef_summary["did_term", "coef"])
      p_val <- coef_summary["did_term", "Pr(>|z|)"]
      cat(sprintf("\n  DiD Treatment Effect:\n"))
      cat(sprintf("    Hazard Ratio: %.4f (p = %.4f)\n", hr, p_val))
    }
  }
} else {
  m_cox_age <- NULL
  cat("  WARNING: No valid age intervals for Cox age model\n")
}

# -----------------------------------------------------------------------------
# 12.5  SAVE RESULTS
# -----------------------------------------------------------------------------

save_cox_results(
  models = list(m_cox_calendar, m_cox_age),
  headers = c("Calendar Origin (TWFE Analogue)", "Age Origin (Robustness)"),
  base_name = "Table12_Cox_DiD_Models",
  title = "Table 12: Cox Proportional Hazards DiD"
)

cat("\n--- Section 12 Complete ---\n")
cat("  Calendar time origin: Main DiD specification\n")
cat("  Age origin: Robustness check with calendar controls\n")
cat("  Both models use strata(vintage_cohort) for cohort-specific baselines\n\n")

#==============================================================================
# SECTION 13: DIAGNOSTIC DATA EXPORT (for 03_DiD_Assumptions.R)
#==============================================================================

cat("\n========================================\n")
cat("SECTION 13: DIAGNOSTIC DATA EXPORT\n")
cat("========================================\n\n")

# Export headline event study model for HonestDiD
saveRDS(model_es_H, file.path(ANALYSIS_DIR, "headline_event_study_model.rds"))
cat("  Exported: headline_event_study_model.rds\n")

# Export Spec A event study for comparison
saveRDS(model_es_A, file.path(ANALYSIS_DIR, "specA_event_study_model.rds"))
cat("  Exported: specA_event_study_model.rds\n")

# Export headline DiD for Bacon
saveRDS(m_closure_H1, file.path(ANALYSIS_DIR, "headline_did_model.rds"))
cat("  Exported: headline_did_model.rds\n")

# PT results
saveRDS(pt_results, file.path(ANALYSIS_DIR, "pt_validation_results.rds"))
cat("  Exported: pt_validation_results.rds\n")

# Pooled data subset for Bacon (needs panel_id, panel_year, did_term, closure_event)
bacon_export <- pooled_data[, .(panel_id, panel_year, did_term, closure_event,
                                 texas_treated, vintage_cohort)]
saveRDS(bacon_export, file.path(ANALYSIS_DIR, "bacon_input_data.rds"))
cat("  Exported: bacon_input_data.rds\n")


#==============================================================================
# SECTION 14: JMP PUBLICATION TABLES (UNIFIED LaTeX)
#==============================================================================

cat("\n========================================\n")
cat("SECTION 14: JMP PUBLICATION TABLES\n")
cat("========================================\n\n")

# -- Table 3: Headline (Pooled + Vintage FE) --
cat("--- JMP Table 3: Headline Pooled + Vintage FE ---\n")

cols3 <- list(
  extract_did(m_closure_H1),
  extract_did(m_closure_H2),
  extract_did(m_exit_H1),
  extract_did(m_rep_H1)
)

tex3 <- c(
  "\\begin{table}[htbp]", "\\centering",
  "\\caption{Policy Effects on Facility Decisions -- Pooled + Vintage-Cohort $\\times$ Year FE}",
  "\\label{tbl:reg-closure}",
  "\\begin{tabular}{lcccc}", "\\toprule",
  " & \\multicolumn{2}{c}{\\textbf{Closure}} & \\textbf{Exit$|$Cls} & \\textbf{Replace$|$Cls} \\\\",
  "\\cmidrule(lr){2-3} \\cmidrule(lr){4-4} \\cmidrule(lr){5-5}",
  " & (1) & (2) & (3) & (4) \\\\", "\\midrule",
  sprintf("Texas $\\times$ Post & %s%s & %s%s & %s%s & %s%s \\\\",
    sprintf("%.4f", cols3[[1]]$beta), stars_fn(cols3[[1]]$p),
    sprintf("%.4f", cols3[[2]]$beta), stars_fn(cols3[[2]]$p),
    sprintf("%.4f", cols3[[3]]$beta), stars_fn(cols3[[3]]$p),
    sprintf("%.4f", cols3[[4]]$beta), stars_fn(cols3[[4]]$p)),
  sprintf("& (%s) & (%s) & (%s) & (%s) \\\\",
    sprintf("%.4f", cols3[[1]]$se), sprintf("%.4f", cols3[[2]]$se),
    sprintf("%.4f", cols3[[3]]$se), sprintf("%.4f", cols3[[4]]$se)),
  "\\midrule",
  "Mandate Control & \\texttt{active} & \\texttt{3yr} & \\texttt{active} & \\texttt{active} \\\\",
  "\\midrule", "\\textbf{Fixed Effects} & & & & \\\\",
  "Facility & Yes & Yes & Yes & Yes \\\\",
  "Vintage-Cohort $\\times$ Year & Yes & Yes & Yes & Yes \\\\",
  "\\midrule",
  sprintf("Observations & %s & %s & %s & %s \\\\",
    format(cols3[[1]]$n, big.mark = ","), format(cols3[[2]]$n, big.mark = ","),
    format(cols3[[3]]$n, big.mark = ","), format(cols3[[4]]$n, big.mark = ",")),
  "\\bottomrule",
  "\\multicolumn{5}{p{0.95\\textwidth}}{\\footnotesize \\textit{Notes:}",
  "Difference-in-differences estimates. Vintage-cohort $\\times$ year FE absorb",
  "cohort-specific temporal patterns (federal compliance ramp-up, aging trends).",
  "\\texttt{mandate\\_active} absorbs TX-specific phased mandate impulse (1989--1993).",
  "Cols (3)--(4) condition on closure occurring.",
  "Standard errors clustered at state level.",
  "$^{*}p<0.1$; $^{**}p<0.05$; $^{***}p<0.01$.}",
  "\\end{tabular}", "\\end{table}")
writeLines(tex3, file.path(OUTPUT_TABLES, "JMP_Table_3_Headline.tex"))
cat("  Saved: JMP_Table_3_Headline.tex\n")

# -- Table 6: Leak --
cat("\n--- JMP Table 6: Reported Leak ---\n")

leak_cols <- list(extract_did(m_leak_H), extract_did(m_leak_A), extract_did(m_leak_B))
tex6 <- c(
  "\\begin{table}[htbp]", "\\centering",
  "\\caption{Policy Effects on Reported Leak Rates}", "\\label{tbl:leak-results}",
  "\\begin{tabular}{lccc}", "\\toprule",
  " & \\textbf{Pooled + VFE} & \\textbf{Spec A} & \\textbf{Spec B} \\\\",
  "\\cmidrule(lr){2-2} \\cmidrule(lr){3-3} \\cmidrule(lr){4-4}",
  " & (1) & (2) & (3) \\\\", "\\midrule",
  sprintf("Texas $\\times$ Post & %s%s & %s%s & %s%s \\\\",
    sprintf("%.4f", leak_cols[[1]]$beta), stars_fn(leak_cols[[1]]$p),
    sprintf("%.4f", leak_cols[[2]]$beta), stars_fn(leak_cols[[2]]$p),
    sprintf("%.4f", leak_cols[[3]]$beta), stars_fn(leak_cols[[3]]$p)),
  sprintf("& (%s) & (%s) & (%s) \\\\",
    sprintf("%.4f", leak_cols[[1]]$se), sprintf("%.4f", leak_cols[[2]]$se),
    sprintf("%.4f", leak_cols[[3]]$se)),
  "\\midrule",
  "Vintage $\\times$ Year FE & Yes & No & No \\\\",
  "Mandate Control & Yes & No & Yes \\\\",
  "\\midrule",
  sprintf("Observations & %s & %s & %s \\\\",
    format(leak_cols[[1]]$n, big.mark = ","), format(leak_cols[[2]]$n, big.mark = ","),
    format(leak_cols[[3]]$n, big.mark = ",")),
  "\\bottomrule",
  "\\multicolumn{4}{p{0.9\\textwidth}}{\\footnotesize \\textit{Notes:}",
  "Standard errors clustered at state level.",
  "$^{*}p<0.1$; $^{**}p<0.05$; $^{***}p<0.01$.}",
  "\\end{tabular}", "\\end{table}")
writeLines(tex6, file.path(OUTPUT_TABLES, "JMP_Table_6_Reported_Leak.tex"))
cat("  Saved: JMP_Table_6_Reported_Leak.tex\n")

# Cross-spec summary
cat("\n--- Cross-Spec Summary ---\n")
spec_summary <- data.table(
  Model = c("Headline (Pooled + VFE)", "Spec A", "Spec B + mandate",
            "Headline Leak", "Spec A Leak"),
  beta = c(extract_did(m_closure_H1)$beta, extract_did(m_closure_A_base)$beta,
           extract_did(m_specB_1)$beta, extract_did(m_leak_H)$beta,
           extract_did(m_leak_A)$beta),
  se   = c(extract_did(m_closure_H1)$se, extract_did(m_closure_A_base)$se,
           extract_did(m_specB_1)$se, extract_did(m_leak_H)$se,
           extract_did(m_leak_A)$se),
  p    = c(extract_did(m_closure_H1)$p, extract_did(m_closure_A_base)$p,
           extract_did(m_specB_1)$p, extract_did(m_leak_H)$p,
           extract_did(m_leak_A)$p))
print(spec_summary)
fwrite(spec_summary, file.path(OUTPUT_TABLES, "Cross_Spec_Summary.csv"))


#==============================================================================
# SECTION 15: SCRIPT SUMMARY
#==============================================================================

cat("\n====================================================================\n")
cat("02_DiD_Main.R COMPLETE\n")
cat("  Vintage FE Triple-Diff Architecture\n")
cat("====================================================================\n\n")

cat("HEADLINE (Pooled + Vintage-Cohort x Year FE):\n")
cat("  Table 3:   Closure / Exit|Closure / Replace|Closure\n")
cat("  Table 6:   Reported Leak\n")
cat("  Figure 6:  Closure Event Study (headline)\n")
cat("  Figure 7:  Leak Event Study\n\n")

cat("ROBUSTNESS:\n")
cat("  Table 4:   Spec A (post-1988, mandate-free)\n")
cat("  Table 5:   Spec B (mandate controls escalation)\n")
cat("  Table 7:   HTE -- Age Bin\n")
cat("  Table 8:   HTE -- Wall Type\n")
cat("  Table B.4: Parallel Trends (6 tests)\n")
cat("  Table B.5: MD excluded\n")
cat("  Table B.6: mandate_window_3yr\n")
cat("  Table F.1: Vintage bin coarseness\n")
cat("  Figure 6B: Spec A event study\n")
cat("  Figure 6C: Spec B event study\n")
cat("  Figure 8:  HTE age bin plot\n\n")

cat("ADDITIONAL MODELS:\n")
cat("  Model 3A:  Age at Closure (County FE)\n")
cat("  Model 4:   Revealed Leaks at Closure\n")
cat("  Model 5A:  Competing Risks (Cause-Specific Cox)\n\n")

cat("SURVIVAL: STUB -- pending clarification (see below)\n\n")

cat(sprintf("Output:    %s\n", OUTPUT_TABLES))
cat(sprintf("Figures:   %s\n", OUTPUT_FIGURES))
cat(sprintf("Bootstrap: %s\n", ifelse(USE_BOOTSTRAP, "ENABLED", "DISABLED")))
cat("====================================================================\n")

