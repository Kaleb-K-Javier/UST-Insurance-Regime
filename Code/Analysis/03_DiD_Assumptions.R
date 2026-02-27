#==============================================================================
# 03_DiD_Assumptions.R
# Texas UST Insurance Reform -- Assumption Testing & Diagnostics
#
# PURPOSE:
#   All assumption-testing models, sensitivity analyses, and diagnostic
#   figures for the JMP paper appendix. Consumes exported model objects
#   and datasets from 02_DiD_Main.R.
#
# PAPER OUTPUTS:
#   Table B.4b:  Pre-Test Power Analysis (Roth 2022)
#   Table B.4c:  HonestDiD Sensitivity (Rambachan & Roth 2023)
#   Table B.7:   Callaway-Sant'Anna Group-Time ATT
#   Table B.8:   Compositional Balance -- Vintage Cohort x Treatment
#   Table B.9:   Cohort Exposure Summary
#   Figure B-3:  HonestDiD Sensitivity Plot
#   Figure B-4:  Bacon Decomposition Diagnostic
#   Figure B-5:  Compositional Balance Heatmap
#
# STRUCTURE:
#   S1  Setup & Package Loading (defensive)
#   S2  Data & Model Loading
#   S3  Roth (2022) Pre-Test Power Analysis
#   S4  Rambachan & Roth (2023) HonestDiD Sensitivity
#   S5  Bacon Decomposition Diagnostic
#   S6  Compositional Balance Tests
#   S7  Cohort Exposure Summary
#   S8  Callaway-Sant'Anna Robustness
#   S9  Script Summary
#
# PREREQUISITES: Run 02_DiD_Main.R first (exports model objects).
# Date: February 2026
#==============================================================================


#==============================================================================
# SECTION 1: SETUP & DEFENSIVE PACKAGE LOADING
#==============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(fixest)
  library(here)
  library(ggplot2)
  library(broom)
  library(kableExtra)
})

options(scipen = 999)
set.seed(20260202)

ANALYSIS_DIR   <- here("Data", "Analysis")
meta           <- readRDS(file.path(ANALYSIS_DIR, "analysis_metadata.rds"))
OUTPUT_TABLES  <- meta$output_tables
OUTPUT_FIGURES <- meta$output_figures
POST_YEAR      <- meta$post_year
ES_START       <- meta$es_start
ES_END         <- meta$es_end
PANEL_END      <- meta$panel_end

dir.create(OUTPUT_TABLES,  recursive = TRUE, showWarnings = FALSE)
dir.create(OUTPUT_FIGURES, recursive = TRUE, showWarnings = FALSE)

stars_fn <- function(p) {
  if (is.na(p)) return("")
  if (p < 0.01) return("$^{***}$")
  if (p < 0.05) return("$^{**}$")
  if (p < 0.10) return("$^{*}$")
  ""
}

theme_pub <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(plot.title = element_text(face = "bold", size = base_size + 2),
          plot.subtitle = element_text(color = "grey40"),
          panel.grid.minor = element_blank(),
          legend.position = "bottom")
}
theme_set(theme_pub())

# -- Defensive package loading --
HAS_HONESTDID  <- FALSE
HAS_PRETRENDS  <- FALSE
HAS_BACONDECOMP <- FALSE
HAS_DID        <- FALSE

tryCatch({
  if (!requireNamespace("HonestDiD", quietly = TRUE))
    remotes::install_github("asheshrambachan/HonestDiD")
  library(HonestDiD)
  HAS_HONESTDID <- TRUE
  cat("HonestDiD loaded.\n")
}, error = function(e) cat(sprintf("WARNING: HonestDiD unavailable: %s\n", e$message)))

tryCatch({
  if (!requireNamespace("pretrends", quietly = TRUE))
    remotes::install_github("jonathandroth/pretrends")
  library(pretrends)
  HAS_PRETRENDS <- TRUE
  cat("pretrends loaded.\n")
}, error = function(e) cat(sprintf("WARNING: pretrends unavailable: %s\n", e$message)))

tryCatch({
  if (!requireNamespace("bacondecomp", quietly = TRUE))
    install.packages("bacondecomp")
  library(bacondecomp)
  HAS_BACONDECOMP <- TRUE
  cat("bacondecomp loaded.\n")
}, error = function(e) cat(sprintf("WARNING: bacondecomp unavailable: %s\n", e$message)))

tryCatch({
  if (!requireNamespace("did", quietly = TRUE))
    install.packages("did")
  library(did)
  HAS_DID <- TRUE
  cat("did (Callaway-Sant'Anna) loaded.\n")
}, error = function(e) cat(sprintf("WARNING: did package unavailable: %s\n", e$message)))

cat(sprintf("\nPackage availability: HonestDiD=%s pretrends=%s bacondecomp=%s did=%s\n",
            HAS_HONESTDID, HAS_PRETRENDS, HAS_BACONDECOMP, HAS_DID))


#==============================================================================
# SECTION 2: DATA & MODEL LOADING
#==============================================================================

cat("\n========================================\n")
cat("SECTION 2: LOADING EXPORTED OBJECTS\n")
cat("========================================\n\n")

annual_data <- readRDS(file.path(ANALYSIS_DIR, "analysis_annual_data.rds"))
cat(sprintf("  annual_data: %s rows\n", format(nrow(annual_data), big.mark = ",")))

# Exported model objects from 02
model_es_H <- tryCatch(
  readRDS(file.path(ANALYSIS_DIR, "headline_event_study_model.rds")),
  error = function(e) { cat("  WARNING: headline ES model not found.\n"); NULL })

model_es_A <- tryCatch(
  readRDS(file.path(ANALYSIS_DIR, "specA_event_study_model.rds")),
  error = function(e) { cat("  WARNING: Spec A ES model not found.\n"); NULL })

bacon_input <- tryCatch(
  readRDS(file.path(ANALYSIS_DIR, "bacon_input_data.rds")),
  error = function(e) { cat("  WARNING: Bacon input not found.\n"); NULL })

pt_results <- tryCatch(
  readRDS(file.path(ANALYSIS_DIR, "pt_validation_results.rds")),
  error = function(e) { cat("  WARNING: PT results not found.\n"); NULL })

# If models not exported, re-estimate headline ES in-script
if (is.null(model_es_H)) {
  cat("  Re-estimating headline event study in-script...\n")

  # Ensure vintage_cohort exists
  if (!"vintage_cohort" %in% names(annual_data)) {
    tanks_1999 <- readRDS(file.path(ANALYSIS_DIR, "analysis_tanks_1999.rds"))
    if (!"install_year" %in% names(tanks_1999))
      tanks_1999[, install_year := year(install_date)]
    tanks_1999[, vintage_cohort_tank := fcase(
      install_year < 1965, "Pre-1965",
      install_year >= 1965 & install_year <= 1974, "1965-1974",
      install_year >= 1975 & install_year <= 1979, "1975-1979",
      install_year >= 1980 & install_year <= 1984, "1980-1984",
      install_year >= 1985 & install_year <= 1988, "1985-1988",
      default = "Post-1988")]
    fac_vintage <- tanks_1999[, .(
      vintage_cohort = names(which.max(table(vintage_cohort_tank)))
    ), by = panel_id]
    annual_data <- merge(annual_data, fac_vintage, by = "panel_id", all.x = TRUE)
    annual_data[is.na(vintage_cohort), vintage_cohort := "Post-1988"]
    annual_data[, vintage_cohort := factor(vintage_cohort,
      levels = c("Pre-1965", "1965-1974", "1975-1979",
                 "1980-1984", "1985-1988", "Post-1988"))]
  }

  if (!"mandate_active" %in% names(annual_data)) {
    annual_data[, mandate_active := as.integer(
      state == "TX" & spec_B_eligible == 1 &
        panel_year >= 1989L & panel_year <= 1993L)]
  }

  rel_min <- ES_START - POST_YEAR
  rel_max <- ES_END   - POST_YEAR
  es_pool <- annual_data[panel_year >= ES_START & panel_year <= ES_END]
  es_pool[, rel_year_bin := pmax(pmin(rel_year_1999, rel_max), rel_min)]

  model_es_H <- feols(
    closure_event ~ i(rel_year_bin, texas_treated, ref = -1) + mandate_active |
      panel_id + vintage_cohort^panel_year,
    data = es_pool, cluster = ~state)
  cat("  OK: Headline ES re-estimated.\n")
}


#==============================================================================
# SECTION 3: ROTH (2022) PRE-TEST POWER ANALYSIS (Table B.4b)
#==============================================================================

cat("\n========================================\n")
cat("SECTION 3: PRE-TEST POWER (Roth 2022)\n")
cat("========================================\n\n")

if (HAS_PRETRENDS && !is.null(model_es_H)) {
  ct_r <- coeftable(summary(model_es_H, cluster = ~state))
  coef_names <- rownames(ct_r)
  pre_names  <- coef_names[grepl("::-[2-9]$|::-1[0-9]$", coef_names)]
  post_names <- coef_names[grepl("::0$|::[1-9]$|::[1-9][0-9]$", coef_names)]

  if (length(pre_names) >= 2) {
    pre_beta <- ct_r[pre_names, "Estimate"]
    pre_se   <- ct_r[pre_names, "Std. Error"]
    sigma_pre <- vcov(model_es_H, cluster = ~state)[pre_names, pre_names]

    tryCatch({
      power_out <- pretrends::pretrends(
        betahat = pre_beta, sigma = sigma_pre,
        numPrePeriods = length(pre_names),
        deltatrue = 0,
        alpha = 0.05)

      cat(sprintf("Pre-test power (delta=0): %.3f\n", power_out$power))
      cat(sprintf("Conditional bias (pre-test pass): %.4f\n",
                  power_out$conditional_bias))

      power_dt <- data.table(
        Statistic = c("Power (delta=0)", "Conditional Bias (pre-test pass)",
                      "N pre-periods"),
        Value = c(sprintf("%.3f", power_out$power),
                  sprintf("%.4f", power_out$conditional_bias),
                  length(pre_names)))

      fwrite(power_dt, file.path(OUTPUT_TABLES, "TableB4b_PreTest_Power_Roth2022.csv"))

      tex_power <- c(
        "\\begin{table}[htbp]", "\\centering",
        "\\caption{Pre-Test Power Analysis (Roth 2022)}",
        "\\label{tbl:pretest-power}",
        "\\begin{tabular}{lc}", "\\toprule",
        "Statistic & Value \\\\", "\\midrule",
        sprintf("Power ($\\delta=0$, $\\alpha=0.05$) & %.3f \\\\", power_out$power),
        sprintf("Conditional Bias (pre-test pass) & %.4f \\\\",
                power_out$conditional_bias),
        sprintf("Pre-periods & %d \\\\", length(pre_names)),
        "\\bottomrule",
        "\\multicolumn{2}{p{0.8\\textwidth}}{\\footnotesize \\textit{Notes:}",
        "Power to detect violation of parallel trends under true effect $\\delta=0$.",
        "Headline specification (pooled + vintage-cohort $\\times$ year FE).",
        "Following Roth (2022, \\textit{AER: Insights}).}",
        "\\end{tabular}", "\\end{table}")
      writeLines(tex_power, file.path(OUTPUT_TABLES, "TableB4b_PreTest_Power_Roth2022.tex"))
      cat("  Saved: TableB4b (.csv / .tex)\n")

    }, error = function(e) cat(sprintf("  pretrends failed: %s\n", e$message)))
  } else {
    cat("  Insufficient pre-periods.\n")
  }
} else {
  cat("  [Skipped: HAS_PRETRENDS=%s, model=%s]\n", HAS_PRETRENDS, !is.null(model_es_H))
}


#==============================================================================
# SECTION 4: RAMBACHAN & ROTH (2023) HonestDiD (Table B.4c, Figure B-3)
#==============================================================================

cat("\n========================================\n")
cat("SECTION 4: HonestDiD SENSITIVITY\n")
cat("========================================\n\n")

if (HAS_HONESTDID && !is.null(model_es_H)) {
  ct_h <- coeftable(summary(model_es_H, cluster = ~state))
  coef_names_h <- rownames(ct_h)

  pre_h  <- coef_names_h[grepl("::-[2-9]$|::-1[0-9]$", coef_names_h)]
  post_h <- coef_names_h[grepl("::0$|::[1-9]$|::[1-9][0-9]$", coef_names_h)]

  n_pre_h  <- length(pre_h)
  n_post_h <- length(post_h)

  if (n_pre_h >= 2 && n_post_h >= 1) {
    beta_h  <- coef(model_es_H)
    sigma_h <- vcov(model_es_H, cluster = ~state)

    pre_idx_h  <- which(coef_names_h %in% pre_h)
    post_idx_h <- which(coef_names_h %in% post_h)

    pre_years_h  <- as.numeric(gsub(".*::(-?[0-9]+).*", "\\1", pre_h))
    post_years_h <- as.numeric(gsub(".*::(-?[0-9]+).*", "\\1", post_h))

    pre_order  <- order(pre_years_h)
    post_order <- order(post_years_h)

    pre_idx_sorted  <- pre_idx_h[pre_order]
    post_idx_sorted <- post_idx_h[post_order]

    all_idx <- c(pre_idx_sorted, post_idx_sorted)

    tryCatch({
      # Relative Magnitude
      honest_rm <- HonestDiD::createSensitivityResults_relativeMagnitudes(
        betahat        = beta_h[all_idx],
        sigma          = sigma_h[all_idx, all_idx],
        numPrePeriods  = n_pre_h,
        numPostPeriods = n_post_h,
        Mbarvec        = seq(0, 2, by = 0.25),
        l_vec          = basisVector(1, n_post_h))

      honest_rm_dt <- as.data.table(honest_rm)
      setnames(honest_rm_dt, c("Mbar", "lb", "ub", "method"))

      flci_rows <- honest_rm_dt[method == "FLCI"]
      if (nrow(flci_rows) > 0) {
        flci_rows[, excludes_zero := (lb > 0 | ub < 0)]
        breakdown_Mbar <- max(c(0, flci_rows[excludes_zero == TRUE, Mbar]), na.rm = TRUE)
      } else {
        breakdown_Mbar <- NA_real_
      }

      cat(sprintf("  Relative Magnitude breakdown Mbar* = %.2f\n", breakdown_Mbar))
      if (!is.na(breakdown_Mbar) && breakdown_Mbar >= 1.0) {
        cat("  ROBUST: post-violation must exceed pre-violation to overturn.\n")
      }

      # Smoothness
      breakdown_M_sd <- NA_real_
      tryCatch({
        honest_sd <- HonestDiD::createSensitivityResults(
          betahat = beta_h[all_idx], sigma = sigma_h[all_idx, all_idx],
          numPrePeriods = n_pre_h, numPostPeriods = n_post_h,
          Mvec = seq(0, 0.01, by = 0.001),
          l_vec = basisVector(1, n_post_h))

        honest_sd_dt <- as.data.table(honest_sd)
        setnames(honest_sd_dt, c("M", "lb", "ub", "method"))
        sd_flci <- honest_sd_dt[method == "FLCI"]
        if (nrow(sd_flci) > 0) {
          sd_flci[, excludes_zero := (lb > 0 | ub < 0)]
          breakdown_M_sd <- max(c(0, sd_flci[excludes_zero == TRUE, M]), na.rm = TRUE)
        }
        cat(sprintf("  Smoothness breakdown M* = %.4f\n", breakdown_M_sd))
      }, error = function(e) cat(sprintf("  Smoothness failed: %s\n", e$message)))

      # Save results
      honest_results <- data.table(
        restriction = c("Relative Magnitude (Mbar)", "Smoothness (DeltaSD)"),
        breakdown_value = c(sprintf("%.2f", breakdown_Mbar),
                            tryCatch(sprintf("%.4f", breakdown_M_sd),
                                     error = function(e) "N/A")),
        interpretation = c(
          fifelse(!is.na(breakdown_Mbar) & breakdown_Mbar >= 1.0,
                  "Robust (Mbar* >= 1)", "Moderate/Fragile"),
          tryCatch(fifelse(!is.na(breakdown_M_sd) & breakdown_M_sd > 0.005,
                           "Robust", "Moderate/Fragile"),
                   error = function(e) "N/A")))

      fwrite(honest_results, file.path(OUTPUT_TABLES, "TableB4c_HonestDiD_Sensitivity.csv"))

      # LaTeX
      tex_honest <- c(
        "\\begin{table}[htbp]", "\\centering",
        "\\caption{Sensitivity to Parallel Trends Violations (Rambachan \\& Roth 2023)}",
        "\\label{tbl:honest-did}",
        "\\begin{tabular}{lcc}", "\\toprule",
        "Restriction & Breakdown & Interpretation \\\\", "\\midrule",
        sprintf("Relative Magnitude ($\\bar{M}$) & %.2f & %s \\\\",
                breakdown_Mbar,
                fifelse(!is.na(breakdown_Mbar) & breakdown_Mbar >= 1.0,
                        "Robust", "Moderate")),
        sprintf("Smoothness ($\\Delta$SD) & %s & %s \\\\",
                tryCatch(sprintf("%.4f", breakdown_M_sd), error = function(e) "N/A"),
                tryCatch(fifelse(!is.na(breakdown_M_sd) & breakdown_M_sd > 0.005,
                                 "Robust", "Moderate"), error = function(e) "N/A")),
        "\\bottomrule",
        "\\multicolumn{3}{p{0.9\\textwidth}}{\\footnotesize \\textit{Notes:}",
        "Headline specification (pooled + vintage-cohort $\\times$ year FE).",
        "$\\bar{M}^*$: largest $\\bar{M}$ where robust CI excludes zero.",
        "Following Rambachan \\& Roth (2023, \\textit{ReStud}).}",
        "\\end{tabular}", "\\end{table}")
      writeLines(tex_honest, file.path(OUTPUT_TABLES, "TableB4c_HonestDiD_Sensitivity.tex"))
      cat("  Saved: TableB4c (.csv / .tex)\n")

      # -- Figure B-3: Sensitivity Plot --
      plot_df <- honest_rm_dt[method == "FLCI"]
      if (nrow(plot_df) == 0) plot_df <- honest_rm_dt[!duplicated(Mbar)]

      p_honest <- ggplot(plot_df, aes(x = Mbar)) +
        geom_ribbon(aes(ymin = lb, ymax = ub), fill = "#4575B4", alpha = 0.2) +
        geom_line(aes(y = lb), color = "#4575B4", linewidth = 0.8) +
        geom_line(aes(y = ub), color = "#4575B4", linewidth = 0.8) +
        geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
        {if (!is.na(breakdown_Mbar) && breakdown_Mbar > 0)
          geom_vline(xintercept = breakdown_Mbar, linetype = "dotted",
                     color = "#D73027", linewidth = 0.7)} +
        {if (!is.na(breakdown_Mbar) && breakdown_Mbar > 0)
          annotate("text", x = breakdown_Mbar + 0.05,
                   y = max(plot_df$ub, na.rm = TRUE) * 0.85,
                   label = sprintf("Mbar* = %.2f", breakdown_Mbar),
                   color = "#D73027", size = 4, hjust = 0)} +
        labs(title = "Sensitivity to Violations of Parallel Trends",
             subtitle = "Rambachan & Roth (2023): Relative Magnitude Restrictions",
             x = expression(bar(M)), y = "Robust CI") +
        theme_pub()

      ggsave(file.path(OUTPUT_FIGURES, "JMP_Figure_B3_HonestDiD_Sensitivity.png"),
             p_honest, width = 10, height = 6, dpi = 300, bg = "white")
      ggsave(file.path(OUTPUT_FIGURES, "JMP_Figure_B3_HonestDiD_Sensitivity.pdf"),
             p_honest, width = 10, height = 6, device = cairo_pdf)
      cat("  Saved: Figure B-3 (.png / .pdf)\n")

    }, error = function(e) cat(sprintf("  HonestDiD analysis failed: %s\n", e$message)))
  } else {
    cat(sprintf("  Insufficient coefficients: pre=%d, post=%d\n", n_pre_h, n_post_h))
  }
} else {
  cat("  [Skipped: HAS_HONESTDID=%s]\n", HAS_HONESTDID)
}


#==============================================================================
# SECTION 5: BACON DECOMPOSITION (Figure B-4)
#==============================================================================

cat("\n========================================\n")
cat("SECTION 5: BACON DECOMPOSITION\n")
cat("========================================\n\n")

if (HAS_BACONDECOMP && !is.null(bacon_input)) {
  tryCatch({
    bacon_df <- as.data.frame(bacon_input[, .(panel_id, panel_year,
                                               did_term, closure_event)])
    bacon_out <- bacon(closure_event ~ did_term,
                       data = bacon_df,
                       id_var = "panel_id", time_var = "panel_year")

    if (is.data.frame(bacon_out)) {
      bacon_dt <- as.data.table(bacon_out)
      cat(sprintf("  2x2 comparisons: %d\n", nrow(bacon_dt)))
      cat(sprintf("  Weighted average DD: %.6f\n",
                  sum(bacon_dt$estimate * bacon_dt$weight)))
      cat(sprintf("  Negative weights: %s\n",
                  fifelse(any(bacon_dt$weight < 0), "YES", "No")))

      fwrite(bacon_dt, file.path(OUTPUT_TABLES, "Bacon_Decomposition_Pooled.csv"))

      p_bacon <- ggplot(bacon_dt, aes(x = weight, y = estimate, color = type)) +
        geom_point(size = 2.5, alpha = 0.7) +
        geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
        labs(title = "Bacon Decomposition: Pooled TWFE",
             subtitle = "Goodman-Bacon (2021)",
             x = "Weight", y = "Component DD Estimate",
             color = "Comparison Type") +
        theme_pub()

      ggsave(file.path(OUTPUT_FIGURES, "JMP_Figure_B4_Bacon_Decomposition.png"),
             p_bacon, width = 10, height = 6, dpi = 300, bg = "white")
      cat("  Saved: Bacon decomposition (.csv / Figure B-4)\n")
    }
  }, error = function(e) {
    cat(sprintf("  Bacon failed: %s\n", e$message))
    cat("  (Likely unbalanced panel -- diagnostic only.)\n")
  })
} else {
  cat("  [Skipped: HAS_BACONDECOMP=%s, data=%s]\n", HAS_BACONDECOMP, !is.null(bacon_input))
}


#==============================================================================
# SECTION 6: COMPOSITIONAL BALANCE (Table B.8, Figure B-5)
#==============================================================================

cat("\n========================================\n")
cat("SECTION 6: COMPOSITIONAL BALANCE\n")
cat("========================================\n\n")

# Verify vintage_cohort exists
if ("vintage_cohort" %in% names(annual_data)) {

  # Cell counts: vintage_cohort x treatment group
  cell_balance <- annual_data[panel_year == meta$treatment_year,
    .N, by = .(vintage_cohort, texas_treated)]
  cell_balance[, pct := round(100 * N / sum(N), 1), by = texas_treated]

  cat("Vintage cohort composition at treatment year:\n")
  print(dcast(cell_balance, vintage_cohort ~ texas_treated,
              value.var = "pct", fill = 0))

  fwrite(cell_balance, file.path(OUTPUT_TABLES, "TableB8_Compositional_Balance.csv"))

  # LaTeX
  cell_wide <- dcast(cell_balance, vintage_cohort ~ texas_treated,
                     value.var = c("N", "pct"), fill = 0)
  # Rename for clarity
  old_n0 <- grep("^N_0$", names(cell_wide), value = TRUE)
  old_n1 <- grep("^N_1$", names(cell_wide), value = TRUE)
  old_p0 <- grep("^pct_0$", names(cell_wide), value = TRUE)
  old_p1 <- grep("^pct_1$", names(cell_wide), value = TRUE)

  tex_balance <- c(
    "\\begin{table}[htbp]", "\\centering",
    "\\caption{Compositional Balance: Vintage Cohort $\\times$ Treatment Group}",
    "\\label{tbl:comp-balance}",
    "\\begin{tabular}{lrrrr}", "\\toprule",
    " & \\multicolumn{2}{c}{Control} & \\multicolumn{2}{c}{Texas} \\\\",
    "\\cmidrule(lr){2-3} \\cmidrule(lr){4-5}",
    "Vintage Cohort & N & \\% & N & \\% \\\\", "\\midrule")

  for (i in seq_len(nrow(cell_wide))) {
    r <- cell_wide[i]
    tex_balance <- c(tex_balance, sprintf("%s & %s & %.1f & %s & %.1f \\\\",
      as.character(r$vintage_cohort),
      format(r[[old_n0]], big.mark = ","), r[[old_p0]],
      format(r[[old_n1]], big.mark = ","), r[[old_p1]]))
  }

  tex_balance <- c(tex_balance, "\\bottomrule",
    "\\multicolumn{5}{p{0.8\\textwidth}}{\\footnotesize \\textit{Notes:}",
    "Facility counts at treatment year (1998). Vintage cohorts defined by",
    "TX mandate schedule cutpoints applied uniformly to all states.}",
    "\\end{tabular}", "\\end{table}")
  writeLines(tex_balance, file.path(OUTPUT_TABLES, "TableB8_Compositional_Balance.tex"))
  cat("  Saved: TableB8 (.csv / .tex)\n")

  # -- Figure B-5: Heatmap --
  cell_balance[, group := fifelse(texas_treated == 1, "Texas", "Control")]

  p_heat <- ggplot(cell_balance, aes(x = group, y = vintage_cohort, fill = pct)) +
    geom_tile(color = "white") +
    geom_text(aes(label = sprintf("%.1f%%", pct)), size = 3.5) +
    scale_fill_gradient(low = "white", high = "#4575B4", name = "Share (%)") +
    labs(title = "Vintage Cohort Composition",
         x = NULL, y = "Vintage Cohort") +
    theme_pub() + theme(legend.position = "right")

  ggsave(file.path(OUTPUT_FIGURES, "JMP_Figure_B5_Compositional_Balance.png"),
         p_heat, width = 7, height = 5, dpi = 300, bg = "white")
  cat("  Saved: Figure B-5\n")

} else {
  cat("  WARNING: vintage_cohort not in annual_data. Skipping.\n")
}


#==============================================================================
# SECTION 7: COHORT EXPOSURE SUMMARY (Table B.9)
#==============================================================================

cat("\n========================================\n")
cat("SECTION 7: COHORT EXPOSURE SUMMARY\n")
cat("========================================\n\n")

if ("vintage_cohort" %in% names(annual_data)) {

  cohort_exposure <- annual_data[panel_year == meta$treatment_year, .(
    n_facilities = uniqueN(panel_id),
    n_tx         = uniqueN(panel_id[texas_treated == 1]),
    n_ctrl       = uniqueN(panel_id[texas_treated == 0]),
    mean_age     = round(mean(avg_tank_age, na.rm = TRUE), 1),
    pct_single   = round(100 * mean(has_single_walled == 1, na.rm = TRUE), 1)
  ), by = vintage_cohort]

  setorder(cohort_exposure, vintage_cohort)
  print(cohort_exposure)

  fwrite(cohort_exposure, file.path(OUTPUT_TABLES, "TableB9_Cohort_Exposure.csv"))
  cat("  Saved: TableB9_Cohort_Exposure.csv\n")
}


#==============================================================================
# SECTION 8: CALLAWAY-SANT'ANNA (Table B.7)
#==============================================================================

cat("\n========================================\n")
cat("SECTION 8: CALLAWAY-SANT'ANNA\n")
cat("========================================\n\n")

if (HAS_DID) {

  # For the pooled sample, treatment timing is uniform (1999 for TX, 0 for control).
  # CS estimation treats this as a standard 2-group design.
  # For Spec B, staggered mandate year can serve as group variable.

  tryCatch({
    # Prepare data
    cs_data <- as.data.frame(annual_data[, .(
      panel_id, panel_year, closure_event, texas_treated)])

    # Group variable: 1999 for TX, 0 for never-treated
    cs_data$g <- ifelse(cs_data$texas_treated == 1, 1999L, 0L)

    cs_out <- att_gt(
      yname  = "closure_event",
      tname  = "panel_year",
      idname = "panel_id",
      gname  = "g",
      data   = cs_data,
      control_group = "nevertreated")

    cs_agg <- aggte(cs_out, type = "simple")
    cat("CS aggregate ATT:\n"); print(cs_agg)

    cs_result <- data.table(
      Estimator = "Callaway-Sant'Anna",
      ATT = round(cs_agg$overall.att, 6),
      SE  = round(cs_agg$overall.se, 6),
      CI_low  = round(cs_agg$overall.att - 1.96 * cs_agg$overall.se, 6),
      CI_high = round(cs_agg$overall.att + 1.96 * cs_agg$overall.se, 6))

    fwrite(cs_result, file.path(OUTPUT_TABLES, "TableB7_Callaway_SantAnna.csv"))
    cat("  Saved: TableB7_Callaway_SantAnna.csv\n")

  }, error = function(e) cat(sprintf("  CS estimation failed: %s\n", e$message)))

} else {
  cat("  [Skipped: HAS_DID = FALSE]\n")
}


#==============================================================================
# SECTION 9: SCRIPT SUMMARY
#==============================================================================

cat("\n====================================================================\n")
cat("03_DiD_Assumptions.R COMPLETE\n")
cat("====================================================================\n\n")

cat("OUTPUTS:\n")
cat("  Table B.4b: Pre-test power (Roth 2022)\n")
cat("  Table B.4c: HonestDiD sensitivity\n")
cat("  Table B.7:  Callaway-Sant'Anna ATT\n")
cat("  Table B.8:  Compositional balance\n")
cat("  Table B.9:  Cohort exposure summary\n")
cat("  Figure B-3: HonestDiD sensitivity plot\n")
cat("  Figure B-4: Bacon decomposition\n")
cat("  Figure B-5: Compositional balance heatmap\n\n")

cat(sprintf("Packages: HonestDiD=%s pretrends=%s bacondecomp=%s did=%s\n",
            HAS_HONESTDID, HAS_PRETRENDS, HAS_BACONDECOMP, HAS_DID))
cat("====================================================================\n")
