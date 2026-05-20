################################################################################
# Code/Analysis/02c_Stepped_DiD.R
#
# Stepped DiD tables (OLS LPM + Cox PH), 7 columns each, wild cluster
# bootstrap SEs (score variant, Kline-Santos 2012).
# Active-at-treatment sample from birth-CEM matched panel, unweighted.
#
# Outputs:
#   Output/Tables/T_Stepped_DiD_OLS.tex
#   Output/Tables/T_Stepped_DiD_Cox.tex
#   Output/Tables/T_Stepped_DiD_Bootstrap_Diagnostics.csv
#   Output/Estimation_Results/Stepped_DiD_Fits_active_at_treatment.rds
#   logs/02c_Stepped_DiD_<TIMESTAMP>.log
################################################################################

suppressPackageStartupMessages({
  library(data.table)
  library(fixest)
  library(survival)
  library(here)
  library(glue)
  library(ggplot2)
})

# =============================================================================
# === LOGGING ===
# =============================================================================

dir.create(here::here("logs"), recursive = TRUE, showWarnings = FALSE)
.script_name <- "02c_Stepped_DiD"
.log_path <- here::here("logs", paste0(.script_name, "_",
                                        format(Sys.time(), "%Y%m%d_%H%M%S"), ".log"))
.log <- file(.log_path, open = "wt")
sink(.log, type = "output"); sink(.log, type = "message", append = TRUE)
on.exit({ sink(type = "output"); sink(type = "message"); close(.log) }, add = TRUE)
cat(sprintf("LOG START %s\nScript: %s\nR: %s\nWD: %s\n\n",
            .log_path, .script_name, R.version.string, getwd()))

# =============================================================================
# === SETUP ===
# =============================================================================

cat("=== SETUP ===\n")

source(here::here("Code", "Helpers", "reduced_form_utils.R"))

# Data root — defaults work on the analysis server (UCBARE2), where the
# canonical Data/Analysis/ lives INSIDE the repo. On the dev machine the
# canonical 4.6 GB matched_tanks_birth_cem.csv lives on the Z drive
# (Z is local-only — not mounted on the server), so for a local run
# override one or both via .Renviron / Sys.setenv() before sourcing:
#
#   Sys.setenv(UST_ANALYSIS_DIR = "Z:/ust_ins_move_to_github/Data/Analysis")
#   # — or, to iterate on the older Apr-14 prototype kept in the local repo —
#   Sys.setenv(UST_PANEL_FILE = "matched_tanks.csv")
#
# Column schemas are equivalent across the two panels for the 14 columns
# selected below.
ANALYSIS_DIR          <- Sys.getenv("UST_ANALYSIS_DIR",
                                    here::here("Data", "Analysis"))
PANEL_FILE            <- Sys.getenv("UST_PANEL_FILE",
                                    "matched_tanks_birth_cem.csv")
stopifnot(file.exists(file.path(ANALYSIS_DIR, PANEL_FILE)))
stopifnot(file.exists(file.path(ANALYSIS_DIR, "exact_base.csv")))

dir.create(OUTPUT_TABLES,                                    recursive = TRUE, showWarnings = FALSE)
dir.create(OUTPUT_FIGURES,                                   recursive = TRUE, showWarnings = FALSE)
dir.create(here::here("Output", "Estimation_Results"),       recursive = TRUE, showWarnings = FALSE)

BOOT_B    <- 9999L
BOOT_SEED <- 20260519L

cat(sprintf("ANALYSIS_DIR : %s\n", ANALYSIS_DIR))
cat(sprintf("BOOT_B       : %d\n", BOOT_B))
cat(sprintf("BOOT_SEED    : %d\n", BOOT_SEED))
cat(sprintf("REFORM_DATE  : %s (REFORM_DAYS = %.0f)\n\n", REFORM_DATE, REFORM_DAYS))

# =============================================================================
# === STEP 1 — LOAD DATA & BUILD SAMPLES ===
# =============================================================================

cat("=== STEP 1: LOAD DATA & BUILD SAMPLES ===\n")

log_step(sprintf("Loading %s...", PANEL_FILE))
.panel_header <- names(fread(file.path(ANALYSIS_DIR, PANEL_FILE), nrows = 0L))
.select_cols  <- c("tank_panel_id", "panel_id", "state", "texas_treated",
                    "install_yr_int", "make_model_noage", "cem_weight",
                    "panel_year", "closure_event",
                    "did_term", "mandate_release_det",
                    "mandate_spill_overfill", "mandate_integrity")
if ("first_year_churn" %in% .panel_header) {
  .select_cols <- c(.select_cols, "first_year_churn")
}
matched_tanks_birth_cem <- fread(
  file.path(ANALYSIS_DIR, PANEL_FILE),
  na.strings = c("", "NA"),
  select    = .select_cols
)
# Mirror 02b first_year_churn filter (present in May 12 Z-drive build)
if ("first_year_churn" %in% names(matched_tanks_birth_cem)) {
  matched_tanks_birth_cem <- matched_tanks_birth_cem[
    first_year_churn == 0L | is.na(first_year_churn)
  ]
  log_step("  Applied first_year_churn filter.")
} else {
  log_step("  first_year_churn not in this build — filter skipped.")
}
cat(sprintf("  matched_tanks_birth_cem: %s rows, %s tanks, %s facilities\n",
            fmt_n(nrow(matched_tanks_birth_cem)),
            fmt_n(uniqueN(matched_tanks_birth_cem$tank_panel_id)),
            fmt_n(uniqueN(matched_tanks_birth_cem$panel_id))))

log_step("Loading exact_base (select cols only)...")
exact_base <- fread(
  file.path(ANALYSIS_DIR, "exact_base.csv"),
  na.strings = c("", "NA"),
  select = c("tank_panel_id", "panel_id", "state", "texas_treated",
             "install_yr_int", "make_model_noage",
             "release_det_deadline_yr", "t_enter", "t_exit", "failure")
)
cat(sprintf("  exact_base: %s rows, %s tanks\n",
            fmt_n(nrow(exact_base)),
            fmt_n(uniqueN(exact_base$tank_panel_id))))

log_step("Building active-at-treatment sample...")
data_active <- build_active_at_treatment_sample(matched_tanks_birth_cem)

log_step("Building Cox split for active sample...")
cox_active <- build_active_cox_split(data_active, exact_base)

cat(sprintf("\nACTIVE SAMPLE SIZES:\n"))
cat(sprintf("  Rows (tank-years)  : %s\n",  fmt_n(nrow(data_active))))
cat(sprintf("  Unique tanks       : %s\n",  fmt_n(uniqueN(data_active$tank_panel_id))))
cat(sprintf("  Unique facilities  : %s\n",  fmt_n(uniqueN(data_active$panel_id))))
cat(sprintf("  Unique states      : %s\n",  fmt_n(uniqueN(data_active$state))))
cat(sprintf("  Closure events     : %s\n",  fmt_n(sum(data_active$closure_event == 1L))))
cat(sprintf("  TX treated tanks   : %s\n",  fmt_n(uniqueN(
  data_active[texas_treated == 1L, tank_panel_id]))))
cat(sprintf("\nCOX SPLIT:\n"))
cat(sprintf("  Episodes           : %s\n",  fmt_n(nrow(cox_active))))
cat(sprintf("  Unique tanks       : %s\n",  fmt_n(uniqueN(cox_active$tank_panel_id))))
cat(sprintf("  Failure events     : %s\n\n", fmt_n(sum(cox_active$failure))))

# =============================================================================
# === STEP 2 — FIT 7 OLS SPECIFICATIONS ===
# =============================================================================

cat("=== STEP 2: FIT 7 OLS SPECIFICATIONS ===\n")

ctrl_rhs <- "mandate_release_det + mandate_spill_overfill + mandate_integrity"

fit_ols <- function(fe_rhs) {
  fml <- as.formula(sprintf("closure_event ~ did_term + %s | %s",
                            ctrl_rhs, fe_rhs))
  feols(fml, data = data_active, cluster = ~state)
}

t0_ols <- proc.time()["elapsed"]
m_ols <- list(
  `1` = fit_ols("state + panel_year"),
  `2` = fit_ols("state + cell_id + panel_year"),
  `3` = fit_ols("panel_id + panel_year"),
  `4` = fit_ols("tank_panel_id + panel_year"),
  `5` = fit_ols("state + cell_vintage_year_fe"),
  `6` = fit_ols("tank_panel_id + cell_vintage_year_fe"),
  `7` = fit_ols("panel_id + cell_vintage_year_fe")
)
cat(sprintf("  OLS fits elapsed: %.0fs\n", proc.time()["elapsed"] - t0_ols))

cat("\n  Col | beta_did     | se_model     | N_obs\n")
cat("  ----|--------------|--------------|-------\n")
for (i in seq_along(m_ols)) {
  cat(sprintf("  %3d | %12.6f | %12.6f | %s\n",
              i,
              coef(m_ols[[i]])["did_term"],
              fixest::se(m_ols[[i]])["did_term"],
              fmt_n(nobs(m_ols[[i]]))))
}
cat("\n")

stopifnot(length(m_ols) == 7L)
stopifnot(all(sapply(m_ols, function(m) !is.null(coef(m)["did_term"]))))
stopifnot(all(sapply(m_ols, function(m) !is.na(coef(m)["did_term"]))))

# =============================================================================
# === STEP 3 — WILD CLUSTER BOOTSTRAP FOR OLS ===
# =============================================================================

cat("=== STEP 3: WILD CLUSTER BOOTSTRAP (OLS) ===\n")

boot_ols <- vector("list", 7L)
names(boot_ols) <- names(m_ols)
for (i in seq_along(m_ols)) {
  t0 <- proc.time()["elapsed"]
  boot_ols[[i]] <- run_wcb_ols(m_ols[[i]], data = data_active,
                                B = BOOT_B, seed = BOOT_SEED)
  elapsed <- proc.time()["elapsed"] - t0
  cat(sprintf("[%s] OLS boot col %d: SE_boot=%.5f CI=[%.5f,%.5f] p=%.3f  (%.0fs)\n",
              format(Sys.time(), "%H:%M:%S"), i,
              boot_ols[[i]]$SE_boot, boot_ols[[i]]$CI_lo,
              boot_ols[[i]]$CI_hi,  boot_ols[[i]]$p_boot, elapsed))
}
cat("\n")

stopifnot(length(boot_ols) == 7L)
stopifnot(all(sapply(boot_ols, function(b) is.finite(b$SE_boot))))
stopifnot(all(sapply(boot_ols, function(b) b$SE_boot > 0)))
stopifnot(all(sapply(boot_ols, function(b) b$B == BOOT_B)))

# =============================================================================
# === STEP 4 — FIT 4 COX SPECIFICATIONS ===
# =============================================================================

cat("=== STEP 4: FIT 4 COX SPECIFICATIONS ===\n")

fit_cox <- function(extra_covs = "", strata_var = NULL) {
  rhs_extra <- if (nzchar(extra_covs))   paste("+", extra_covs)               else ""
  rhs_strat <- if (!is.null(strata_var)) paste("+ strata(", strata_var, ")")  else ""
  fml <- as.formula(sprintf(
    "Surv(t_enter, t_exit, failure) ~ did_term + %s %s %s",
    ctrl_rhs, rhs_extra, rhs_strat))
  t0 <- proc.time()["elapsed"]
  m  <- coxph(fml, data = cox_active, cluster = state,
               ties = "efron", model = TRUE)
  elapsed <- proc.time()["elapsed"] - t0
  if (elapsed > 600) {
    warning(sprintf("Cox fit took %.0fs > 600s threshold", elapsed))
  }
  cat(sprintf("  col %s: beta=%.5f robust_se=%.5f n_events=%d (%.0fs)\n",
              deparse(sys.call()[[2]]),
              coef(m)["did_term"],
              summary(m)$coefficients["did_term", "robust se"],
              m$nevent, elapsed))
  m
}

# NOTE: dropped the prior col 2 (`factor(state) + factor(cell_id)`). With
# 1,083 cell_id levels and 728K episodes, `coxph` cannot invert the dense
# Hessian in reasonable wall time (>>1 hour). The cell-as-strata variant
# below is the strictly stronger econometric control (it lets the baseline
# hazard h_{0,c}(t) vary freely by cell rather than just shifting the
# log-hazard multiplicatively), so dropping the factor variant loses no
# identifying information. See T_Stepped_DiD_Cox.tex notes for the
# updated OLS-analogue mapping.
m_cox <- list(
  `1` = fit_cox("factor(state)"),
  `2` = fit_cox("factor(state)",                      strata_var = "cell_id"),
  `3` = fit_cox("factor(state) + factor(panel_year)", strata_var = "cell_id")
)
cat("\n")

stopifnot(length(m_cox) == 3L)
stopifnot(all(sapply(m_cox, function(m) !is.na(coef(m)["did_term"]))))
stopifnot(all(sapply(m_cox, function(m)
  is.finite(sqrt(diag(vcov(m)))["did_term"]))))
stopifnot(all(sapply(m_cox, function(m) identical(m$method, "efron"))))

# =============================================================================
# === STEP 5 — WILD SCORE BOOTSTRAP FOR COX ===
# =============================================================================

cat("=== STEP 5: WILD SCORE BOOTSTRAP (COX) ===\n")

boot_cox <- vector("list", 3L)
names(boot_cox) <- names(m_cox)
for (i in seq_along(m_cox)) {
  t0 <- proc.time()["elapsed"]
  boot_cox[[i]] <- run_wcb_cox(m_cox[[i]], data = cox_active,
                                B = BOOT_B, seed = BOOT_SEED)
  elapsed <- proc.time()["elapsed"] - t0
  cat(sprintf("[%s] Cox  boot col %d: SE_boot=%.5f CI=[%.5f,%.5f] p=%.3f  (%.0fs)\n",
              format(Sys.time(), "%H:%M:%S"), i,
              boot_cox[[i]]$SE_boot, boot_cox[[i]]$CI_lo,
              boot_cox[[i]]$CI_hi,  boot_cox[[i]]$p_boot, elapsed))
}
cat("\n")

stopifnot(length(boot_cox) == 3L)
stopifnot(all(sapply(boot_cox, function(b) is.finite(b$SE_boot))))
stopifnot(all(sapply(boot_cox, function(b) b$SE_boot > 0)))
stopifnot(all(sapply(boot_cox, function(b) b$B == BOOT_B)))

# =============================================================================
# === STEP 6 — BOOTSTRAP DIAGNOSTICS CSV ===
# =============================================================================

cat("=== STEP 6: BOOTSTRAP DIAGNOSTICS CSV ===\n")

boot_diag <- rbindlist(list(
  rbindlist(lapply(seq_along(boot_ols), function(i) data.table(
    model      = "OLS",
    col        = as.integer(names(boot_ols)[i]),
    coef       = coef(m_ols[[i]])["did_term"],
    se_model   = fixest::se(m_ols[[i]])["did_term"],
    se_boot    = boot_ols[[i]]$SE_boot,
    ci_lo_boot = boot_ols[[i]]$CI_lo,
    ci_hi_boot = boot_ols[[i]]$CI_hi,
    p_boot     = boot_ols[[i]]$p_boot,
    B          = boot_ols[[i]]$B,
    n_obs      = nobs(m_ols[[i]])
  ))),
  rbindlist(lapply(seq_along(boot_cox), function(i) data.table(
    model      = "Cox",
    col        = as.integer(names(boot_cox)[i]),
    coef       = coef(m_cox[[i]])["did_term"],
    se_model   = sqrt(diag(vcov(m_cox[[i]])))["did_term"],
    se_boot    = boot_cox[[i]]$SE_boot,
    ci_lo_boot = boot_cox[[i]]$CI_lo,
    ci_hi_boot = boot_cox[[i]]$CI_hi,
    p_boot     = boot_cox[[i]]$p_boot,
    B          = boot_cox[[i]]$B,
    n_obs      = nobs(m_cox[[i]])
  )))
))

stopifnot(nrow(boot_diag) == 10L)   # 7 OLS + 3 Cox
stopifnot(all(c("model","col","coef","se_model","se_boot",
                "ci_lo_boot","ci_hi_boot","p_boot","B","n_obs") %in% names(boot_diag)))

.diag_path <- file.path(OUTPUT_TABLES, "T_Stepped_DiD_Bootstrap_Diagnostics.csv")
fwrite(boot_diag, .diag_path)
cat(sprintf("  Written: T_Stepped_DiD_Bootstrap_Diagnostics.csv (%d rows)\n\n",
            nrow(boot_diag)))

# =============================================================================
# === STEP 7 — OLS TABLE (LaTeX) ===
# =============================================================================

cat("=== STEP 7: RENDER OLS TABLE ===\n")

.fmt4 <- function(x) ifelse(is.finite(x), sprintf("%.4f", x), "---")
.fmtn <- function(x) format(as.integer(x), big.mark = ",", scientific = FALSE)
.yn7  <- function(v) paste(ifelse(v, "Y", "---"), collapse = " & ")

# Extract OLS summary statistics
.coefs_ols <- sapply(m_ols, function(m) coef(m)["did_term"])
.ses_m_ols <- sapply(m_ols, function(m) fixest::se(m)["did_term"])
.ses_b_ols <- sapply(boot_ols, function(b) b$SE_boot)
.nobs_ols  <- sapply(m_ols, nobs)
.r2_ols    <- sapply(m_ols, function(m) unname(fixest::r2(m, "ar2")))

.ols_col_labels <- c(
  "{\\small\\textit{(state+yr)}}",
  "{\\small\\textit{(state+cell+yr)}}",
  "{\\small\\textit{(fac+yr)}}",
  "{\\small\\textit{(tank+yr)}}",
  "{\\small\\textit{(state+cell$\\times$yr)}}",
  "{\\small\\textit{(tank+cell$\\times$yr)}}",
  "{\\small\\textit{(fac+cell$\\times$yr)}}"
)

.ols_fe <- list(
  "State FE (explicit)"         = c(T,T,F,F,T,F,F),
  "Year FE"                     = c(T,T,T,T,F,F,F),
  "Cell FE"                     = c(F,T,F,F,F,F,F),
  "Facility FE"                 = c(F,F,T,F,F,F,T),
  "Tank FE"                     = c(F,F,F,T,F,T,F),
  "Cell $\\times$ year FE"      = c(F,F,F,F,T,T,T),
  "Mandate controls"            = c(T,T,T,T,T,T,T)
)

.ols_notes <- paste0(
  "OLS LPM with \\texttt{closure\\_event} as outcome. ",
  "Coefficient row shows $\\hat{\\beta}$; model row shows analytic cluster-robust SE ",
  "in parentheses (clustered at state, $G=18$); bootstrap row shows wild cluster ",
  "bootstrap SE in brackets (score-based variant, robust to high-dimensional FE; ",
  "Rademacher weights; $B=9{,}999$; bootstrap SE $=$ CI half-width / 1.96). ",
  "\\texttt{did\\_term} $=$ \\texttt{texas\\_treated} $\\times$ ",
  "$\\mathbf{1}\\{\\text{year} \\geq 1998\\}$. ",
  "Sample: active-at-treatment tanks (facility had $\\geq 1$ open or newly installed ",
  "tank on 1998-12-22), unweighted. ",
  "Mandate controls $=$ \\texttt{mandate\\_release\\_det}, ",
  "\\texttt{mandate\\_spill\\_overfill}, \\texttt{mandate\\_integrity}. ",
  "State-level absorption is enforced in every column: explicit $\\alpha_s$ in cols ",
  "1, 2, 5; implicit via the nested unit FE (facility / tank) in cols 3, 4, 6, 7. ",
  "Col~(7) is the main specification."
)

.ols_tex <- c(
  "% Auto-generated by Code/Analysis/02c_Stepped_DiD.R --- do not edit by hand",
  "\\begin{table}[!ht]",
  "\\centering",
  "\\caption{Stepped DiD: OLS Linear Probability Model of Tank Closure}",
  "\\label{tab:stepped_did_ols}",
  "\\footnotesize",
  "\\begin{tabular}{lccccccc}",
  "\\toprule",
  paste0(" & (1) & (2) & (3) & (4) & (5) & (6) & (7) \\\\"),
  paste0(" & ", paste(.ols_col_labels, collapse = " & "), " \\\\[2pt]"),
  "\\midrule",
  paste0("Texas reform ($\\hat{\\beta}$) & ",
         paste(.fmt4(.coefs_ols), collapse = " & "), " \\\\"),
  paste0(" & ",
         paste(sprintf("(%s)", .fmt4(.ses_m_ols)), collapse = " & "), " \\\\"),
  paste0(" & ",
         paste(sprintf("[%s]", .fmt4(.ses_b_ols)), collapse = " & "), " \\\\"),
  "\\midrule",
  unlist(lapply(names(.ols_fe), function(nm) {
    paste0(nm, " & ", .yn7(.ols_fe[[nm]]), " \\\\")
  })),
  "\\midrule",
  paste0("Observations & ",
         paste(.fmtn(.nobs_ols), collapse = " & "), " \\\\"),
  paste0("Adj.\\ $R^{2}$ & ",
         paste(.fmt4(.r2_ols), collapse = " & "), " \\\\"),
  paste0("Wild cluster $B$ & ",
         paste(rep("$9{,}999$", 7), collapse = " & "), " \\\\"),
  "\\bottomrule",
  paste0("\\multicolumn{8}{p{0.98\\textwidth}}{\\textit{Notes:} ", .ols_notes, "}"),
  "\\end{tabular}",
  "\\end{table}"
)

write_tex(.ols_tex, "T_Stepped_DiD_OLS.tex")
stopifnot(any(grepl("\\(1\\)", .ols_tex)))
stopifnot(any(grepl("\\(7\\)", .ols_tex)))
stopifnot(any(grepl("\\[", .ols_tex)))
stopifnot(any(grepl("wild cluster bootstrap", .ols_tex)))
stopifnot(any(grepl("score-based", .ols_tex)))

# =============================================================================
# === STEP 8 — COX TABLE (LaTeX, 3 cols) ===
# =============================================================================

cat("=== STEP 8: RENDER COX TABLE ===\n")

.coefs_cox   <- sapply(m_cox, function(m) coef(m)["did_term"])
.ses_m_cox   <- sapply(m_cox, function(m)
                  summary(m)$coefficients["did_term", "robust se"])
.ses_b_cox   <- sapply(boot_cox, function(b) b$SE_boot)
.nev_cox     <- sapply(m_cox, function(m) m$nevent)
.n_tanks_cox <- uniqueN(cox_active$tank_panel_id)

.yn3 <- function(v) paste(ifelse(v, "Y", "---"), collapse = " & ")

.cox_state_cov <- rep(T, 3)
.cox_cell_str  <- c(F, T, T)
.cox_year_cov  <- c(F, F, T)
.cox_mandate   <- rep(T, 3)

.cox_notes <- paste0(
  "Cox proportional-hazards model with two-episode splits at the reform date ",
  "(1998-12-22). Coefficient row shows $\\hat{\\beta}$ (log hazard ratio); ",
  "model row shows robust cluster-sandwich SE in parentheses ",
  "(Lin-Wei 1989; clustered at state, $G=18$); bootstrap row shows wild score ",
  "bootstrap SE in brackets (Kline-Santos 2012; Rademacher weights; $B=9{,}999$). ",
  "Ties handled by Efron's method. ",
  "Cox cols are reported in 3 distinct identifiable specifications mapped to the OLS ",
  "columns via the sub-header. The OLS-to-Cox mapping is many-to-one because Cox cannot ",
  "identify additional within-unit variation in a single-treated-state DiD design: ",
  "stratifying the partial likelihood on treatment-invariant units (state, facility, or tank) ",
  "zeros the score for $\\beta$. Also, including \\texttt{factor(cell\\_id)} as a multiplicative ",
  "covariate (one alternative for absorbing cell composition) is computationally infeasible ",
  "in \\texttt{coxph} at our cell cardinality ($\\sim$1{,}100 levels, $\\sim$728K episodes). ",
  "We use the strictly stronger \\texttt{strata(cell\\_id)} variant from col~(2) onward, which ",
  "lets the baseline hazard $h_{0,c}(t)$ vary freely by cell and subsumes the multiplicative-shifter ",
  "alternative. State-level absorption is enforced in every Cox column via \\texttt{factor(state)} ",
  "covariate, mirroring the same enforcement in OLS cols 2 and 5. ",
  "Cell-level stratification (Cox cols 2, 3) is identifiable because CEM-matched cells ",
  "contain both Texas and control tanks at risk simultaneously. ",
  "Sample: same active-at-treatment tanks as the OLS table. Cox col~(3) is the main specification."
)

.cox_tex <- c(
  "% Auto-generated by Code/Analysis/02c_Stepped_DiD.R --- do not edit by hand",
  "\\begin{table}[!ht]",
  "\\centering",
  "\\caption{Stepped DiD: Cox Proportional-Hazards Model of Tank Closure}",
  "\\label{tab:stepped_did_cox}",
  "\\footnotesize",
  "\\begin{tabular}{lccc}",
  "\\toprule",
  " & (1) & (2) & (3) \\\\",
  paste0("{\\small\\textit{(OLS analogue):}} & ",
         "{\\small\\textit{(1)}} & ",
         "{\\small\\textit{(2--6)}} & ",
         "{\\small\\textit{(7 --- main)}} \\\\[2pt]"),
  "\\midrule",
  paste0("Texas reform ($\\hat{\\beta}$, log HR) & ",
         paste(.fmt4(.coefs_cox), collapse = " & "), " \\\\"),
  paste0(" & ",
         paste(sprintf("(%s)", .fmt4(.ses_m_cox)), collapse = " & "), " \\\\"),
  paste0(" & ",
         paste(sprintf("[%s]", .fmt4(.ses_b_cox)), collapse = " & "), " \\\\"),
  "\\midrule",
  paste0("State covariate & ",   .yn3(.cox_state_cov), " \\\\"),
  paste0("Cell stratum & ",      .yn3(.cox_cell_str),  " \\\\"),
  paste0("Year covariate & ",    .yn3(.cox_year_cov),  " \\\\"),
  paste0("Mandate controls & ",  .yn3(.cox_mandate),   " \\\\"),
  "\\midrule",
  paste0("Tanks & ",
         paste(rep(.fmtn(.n_tanks_cox), 3), collapse = " & "), " \\\\"),
  paste0("Events & ",
         paste(.fmtn(.nev_cox), collapse = " & "), " \\\\"),
  paste0("Wild score $B$ & ",
         paste(rep("$9{,}999$", 3), collapse = " & "), " \\\\"),
  "\\bottomrule",
  paste0("\\multicolumn{4}{p{0.98\\textwidth}}{\\textit{Notes:} ", .cox_notes, "}"),
  "\\end{tabular}",
  "\\end{table}"
)

write_tex(.cox_tex, "T_Stepped_DiD_Cox.tex")
stopifnot(any(grepl("\\(1\\)", .cox_tex)))
stopifnot(any(grepl("\\(3\\)", .cox_tex)))
stopifnot(any(grepl("OLS analogue", .cox_tex)))
stopifnot(any(grepl("\\[", .cox_tex)))
stopifnot(any(grepl("wild score bootstrap", .cox_tex)))
stopifnot(any(grepl("Kline-Santos", .cox_tex)))

# =============================================================================
# === STEP 9 — SAVE FITTED-MODEL ARTIFACTS (.rds) ===
# =============================================================================

cat("=== STEP 9: SAVE RDS ===\n")

.rds_path <- file.path(here::here("Output", "Estimation_Results"),
                        "Stepped_DiD_Fits_active_at_treatment.rds")
saveRDS(
  list(
    m_ols     = m_ols,
    m_cox     = m_cox,
    boot_ols  = boot_ols,
    boot_cox  = boot_cox,
    sample_meta = list(
      n_rows       = nrow(data_active),
      n_tanks      = uniqueN(data_active$tank_panel_id),
      n_facilities = uniqueN(data_active$panel_id),
      n_states     = uniqueN(data_active$state),
      n_closures   = sum(data_active$closure_event == 1L),
      seed         = BOOT_SEED,
      B            = BOOT_B,
      cluster_var  = "state",
      sample_label = "active_at_treatment",
      weights      = "none",
      ticket       = "005",
      built_on     = Sys.time()
    )
  ),
  .rds_path
)
cat(sprintf("  Saved: %s\n\n", .rds_path))

# Quick reload check
.tmp <- readRDS(.rds_path)
stopifnot(all(c("m_ols","m_cox","boot_ols","boot_cox","sample_meta") %in% names(.tmp)))
stopifnot(length(.tmp$m_ols)    == 7L)
stopifnot(length(.tmp$m_cox)    == 3L)
stopifnot(length(.tmp$boot_ols) == 7L)
stopifnot(length(.tmp$boot_cox) == 3L)
rm(.tmp)

# =============================================================================
# === STEP 10 — SUMMARY BLOCK ===
# =============================================================================

cat("=== STEP 10: OUTPUT SUMMARY ===\n\n")

cat("RESULTS — OLS (did_term coefficient):\n")
cat(sprintf("  %s | %8s | %8s | %8s | %8s | %s\n",
            "Col", "beta", "SE_model", "SE_boot", "p_boot", "N_obs"))
for (i in seq_along(m_ols)) {
  cat(sprintf("  %3d | %8.5f | %8.5f | %8.5f | %8.3f | %s\n",
              i,
              coef(m_ols[[i]])["did_term"],
              fixest::se(m_ols[[i]])["did_term"],
              boot_ols[[i]]$SE_boot,
              boot_ols[[i]]$p_boot,
              fmt_n(nobs(m_ols[[i]]))))
}

cat("\nRESULTS — COX (did_term log hazard ratio):\n")
cat(sprintf("  %s | %8s | %8s | %8s | %8s | %s\n",
            "Col", "beta", "SE_model", "SE_boot", "p_boot", "N_events"))
for (i in seq_along(m_cox)) {
  cat(sprintf("  %3d | %8.5f | %8.5f | %8.5f | %8.3f | %s\n",
              i,
              coef(m_cox[[i]])["did_term"],
              summary(m_cox[[i]])$coefficients["did_term", "robust se"],
              boot_cox[[i]]$SE_boot,
              boot_cox[[i]]$p_boot,
              fmt_n(m_cox[[i]]$nevent)))
}

cat("\n--- OUTPUT SUMMARY ---\n")
cat(sprintf("  %s\n", file.path(OUTPUT_TABLES,  "T_Stepped_DiD_OLS.tex")))
cat(sprintf("  %s\n", file.path(OUTPUT_TABLES,  "T_Stepped_DiD_Cox.tex")))
cat(sprintf("  %s\n", file.path(OUTPUT_TABLES,  "T_Stepped_DiD_Bootstrap_Diagnostics.csv")))
cat(sprintf("  %s\n", .rds_path))
cat(sprintf("  %s\n", .log_path))
cat(sprintf("\nLOG END %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
