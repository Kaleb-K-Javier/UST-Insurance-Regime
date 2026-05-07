################################################################################
# 02a_DiD_FacBehavior.R
# Texas UST Insurance Reform -- Facility-Level Behavioral Response
#
# NARRATIVE FLOW:
#   Table 1  : Any tank closure? (6-col walk-in: full sample → reform-CEM)
#   ES Figure: Parallel trends test on any_closure (main paper)
#   Table 2  : What are these closures? (exit / upgrade / shrink, 3 cols)
#   Table 2b : Descriptive composition conditional on closure (endogenous)
#   Table 3  : What do replacements do to the portfolio? (row format, 3 cols)
#   LUST A   : Total leak discovery (3 cols)
#   LUST B   : Background vs inspection-triggered, all 4 windows (3 cols)
#
# COLUMN STRUCTURE (Tables 2 / 2b / 3 / LUST):
#   Col 1: Reform-CEM, CEM weights, facility + year FE, mandate controls
#   Col 2: Reform-CEM, CEM weights, make_model_fac^year FE (PRIMARY)
#   Col 3: Reform-CEM, CEM weights, fac_wall^fac_fuel^year FE (FE_3DIM robustness)
#
# PRIMARY ANALYSIS SAMPLE: Reform-CEM (mm_fac_reform)
#   All facilities alive Dec 22 1998, matched on reform-date snapshot.
#   No pre-1998 exiters. make_model_fac non-missing for all rows.
#
# EVENT STUDY SAMPLE: mm_fac_es (= mm_fac_reform, verified non-NA make_model_fac)
#
# SECTIONS:
#   B1   Setup and Constants
#   B2   Helper Functions
#   B3   Load Panel Data and Construct Variables
#   B4   Sample Construction and Diagnostics
#   B5   Table 1: Any Tank Closure (6-col walk-in + ES)
#   B6   Table 2: Closure Composition (exit / upgrade / shrink)
#        Table 2b: Descriptive Conditional Composition
#   B7   Table 3: Portfolio Composition Outcomes
#   B8   LUST Tables (total + background vs inspection-triggered)
#   B9   Event Study Figures
#   B10  HonestDiD Sensitivity
#   B11  HTE by Wall Type
#   B12  HTE by Oldest-Tank Age
#   B13  Export
################################################################################


################################################################################
# B1: SETUP AND CONSTANTS
################################################################################

suppressPackageStartupMessages({
  library(data.table)
  library(fixest)
  library(MatchIt)
  library(HonestDiD)
  library(ggplot2)
  library(scales)
  library(broom)
  library(here)
  library(did2s)
})

options(scipen = 999)
set.seed(20260202)
setDTthreads(14)

OUTPUT_TABLES  <- here("Output", "Tables")
OUTPUT_FIGURES <- here("Output", "Figures")
ANALYSIS_DIR   <- here("Data",   "Analysis")

dir.create(OUTPUT_TABLES,  recursive = TRUE, showWarnings = FALSE)
dir.create(OUTPUT_FIGURES, recursive = TRUE, showWarnings = FALSE)

# ---- Study parameters ----
POST_YEAR    <- 1999L
REFORM_DATE  <- as.IDate("1998-12-22")
REFORM_DAYS  <- as.numeric(as.Date("1998-12-22"))
PANEL_START  <- 1985L
PANEL_END    <- 2018L

# ---- Event study window ----
ES_PRE_MIN  <- -12L
ES_POST_MAX <-  15L

# ---- Mandate controls ----
MANDATE_CONTROLS_RHS <- paste(
  "+ any_mandate_release_det",
  "+ any_mandate_spill_overfill",
  "+ any_mandate_integrity"
)

# ---- Study states ----
CONTROL_STATES <- c(
  "AR", "CO", "ID", "KS", "KY",
  "LA", "MA", "MD", "ME", "MN", "MO", "NC",
  "OH", "OK", "SD", "TN", "VA"
)
STUDY_STATES <- c("TX", CONTROL_STATES)

# ---- Factor levels ----
AGE_BIN_LEVELS <- c("0-4yr", "5-9yr", "10-14yr", "15-19yr", "20-24yr", "25yr+")
WALL_LEVELS    <- c("All-SW", "Mixed", "All-DW")

# ---- Colors ----
COL_TX   <- "#D55E00"
COL_CTRL <- "#0072B2"

# ---- FE specification strings ----
# FE_NAIVE    : facility + year FE, no cell interaction. Used in Table 1 cols
#               1-3 (full sample) and Table 2/3/LUST col 1 (reform-CEM).
# FE_PRIMARY  : make_model_fac x year (3-dim cell: wall x fuel x age bin).
#               Primary specification. Used in col 2 of all tables.
# FE_3DIM     : fac_wall_reform x fac_fuel_reform x year (2-dim cell: wall x
#               fuel, drops age bin). Robustness spec. Used in col 3 of all
#               tables to test sensitivity to age-bin inclusion in cell def.
# FE_ES       : facility + year FE for event studies. Saturated cell-year FEs
#               are collinear with event-study indicators at a single treatment
#               date -- they absorb the level shift at 1999 within each cell,
#               leaving no variation for the treatment indicators to identify.
FE_NAIVE   <- "panel_id + panel_year"
FE_PRIMARY <- "panel_id + make_model_fac^panel_year"
FE_3DIM    <- "panel_id + fac_wall_reform^fac_fuel_reform^panel_year"
FE_ES      <- "panel_id + panel_year"
CLUSTER_VAR <- "~state"

# ---- Bootstrap constants ----
# Wild cluster bootstrap with Rademacher weights.
# Uses manual score bootstrap (run_boot_ols_score from 02b_DiD_Analysis.R)
# because boottest() and fwildclusterboot break when the FE specification
# includes cell^year interactions in fixest.
# Applied to: all static DiD primary specs (Tables 1-3, LUST A, LUST B)
# and to the primary any_closure ES model.
N_BOOT    <- 9999L
BOOT_SEED <- 42L

# ---- BJS imputation estimator ----
# Borusyak, Jaravel, Spiess (2024) two-stage imputation DiD.
# Stage 1: fit unit + time FEs on never-treated/pre-period observations only.
# Stage 2: impute counterfactual, regress residualized outcome on treatment.
# Reported as col 4 in all main tables. Provides efficiency gains over TWFE
# under heterogeneous treatment effects even with a single treatment date.
# Package: did2s. Formula interface matches feols syntax.
BJS_FIRST_STAGE  <- "~ 0 | panel_id + panel_year"
BJS_SECOND_STAGE <- "~ i(post_1999, texas_treated, ref = 0)"

# ---- Plain-language outcome labels ----
# Used in table notes and figure axis labels.
LABEL_ANY_CLOSURE  <- "Any Tank Closure (0/1)"
LABEL_FAC_EXIT     <- "Facility Closes Entirely (0/1)"
LABEL_UPGRADE      <- "Facility Upgrades a Tank: SW Removed, DW Installed (0/1)"
LABEL_SHRINK       <- "Facility Shrinks Tank Stock: Tank Removed, No Replacement (0/1)"
LABEL_LUST_TOTAL   <- "Any Leak Discovery This Year (0/1)"
LABEL_LUST_BACK    <- paste0("Background Leak: Discovered Outside ",
                              "Closure Inspection Window (0/1)")
LABEL_LUST_INSPECT <- paste0("Inspection-Triggered Leak: Discovered ",
                              "Within Closure Inspection Window (0/1)")

# ---- Acceptance checks ----
stopifnot(
  "POST_YEAR not in panel window" = POST_YEAR > PANEL_START & POST_YEAR <= PANEL_END,
  "ES window inconsistent"        = ES_PRE_MIN < 0L & ES_POST_MAX > 0L,
  "TX not in STUDY_STATES"        = "TX" %in% STUDY_STATES,
  "control states count"          = length(CONTROL_STATES) == 17L
)


################################################################################
# B2: HELPER FUNCTIONS
################################################################################

# ---- B2.1: Logging and formatting ----

log_step <- function(msg, indent = 0L) {
  cat(strrep("  ", indent), msg, "\n", sep = "")
}
fmt_n   <- function(n) format(n, big.mark = ",", scientific = FALSE)
fmt_pct <- function(x, digits = 1L) sprintf(paste0("%.", digits, "f%%"), x * 100)

stars_p <- function(p) {
  ifelse(p < 0.01, "***", ifelse(p < 0.05, "**", ifelse(p < 0.10, "*", "")))
}
fmt_est <- function(x, digits = 4L) {
  formatC(round(x, digits), digits = digits, format = "f")
}
fmt_se <- function(x, digits = 4L) {
  paste0("(", formatC(round(x, digits), digits = digits, format = "f"), ")")
}

# ---- B2.2: Model extraction ----

extract_did <- function(m, tvar = "did_term") {
  if (is.null(m)) {
    return(list(beta = NA_real_, se = NA_real_, p = NA_real_, n = NA_integer_))
  }
  # did2s objects use "ATT" as the coefficient name rather than did_term
  if (inherits(m, "did2s")) {
    ct  <- coeftable(m)
    idx <- grep("ATT|post_1999", rownames(ct))[1L]
  } else {
    ct  <- coeftable(m)
    idx <- grep(tvar, rownames(ct), fixed = TRUE)[1L]
  }
  if (is.na(idx))
    stop(sprintf("extract_did: '%s' not found.", tvar))
  list(
    beta = ct[idx, "Estimate"],
    se   = ct[idx, "Std. Error"],
    p    = ct[idx, "Pr(>|t|)"],
    n    = nobs(m)
  )
}

extract_coef_row <- function(m, pattern) {
  ct  <- coeftable(m)
  idx <- grep(pattern, rownames(ct))[1L]
  if (is.na(idx)) return(NULL)
  data.table(
    term = rownames(ct)[idx],
    est  = ct[idx, "Estimate"],
    se   = ct[idx, "Std. Error"],
    p    = ct[idx, "Pr(>|t|)"]
  )
}

# ---- B2.3: Output utilities ----

save_gg <- function(plot, filename, width, height) {
  ggsave(paste0(filename, ".png"), plot,
         width = width, height = height, dpi = 300, bg = "white")
  ggsave(paste0(filename, ".pdf"), plot,
         width = width, height = height, device = cairo_pdf)
  invisible(NULL)
}

write_tex <- function(lines, path) {
  writeLines(lines, con = path)
  log_step(sprintf("Wrote: %s", basename(path)))
  invisible(NULL)
}

mc_tex <- function(n, txt) sprintf("\\multicolumn{%d}{c}{%s}", n, txt)

# ---- B2.4: Event study (tidy + plot) ----

es_tidy <- function(m, ref_year = -1L) {
  ct <- as.data.table(broom::tidy(m, conf.int = TRUE))
  ct <- ct[grepl("rel_year_bin", term)]
  ct[, rel_year := as.integer(gsub(".*::(-?[0-9]+).*", "\\1", term))]
  ref_row <- data.table(
    term      = "ref",
    estimate  = 0,
    std.error = 0,
    conf.low  = 0,
    conf.high = 0,
    rel_year  = as.integer(ref_year)
  )
  ct <- rbind(ct, ref_row, fill = TRUE)
  setorder(ct, rel_year)
  ct[, period := fcase(
    rel_year <  ref_year, "Pre",
    rel_year == ref_year, "Ref",
    default              = "Post"
  )]
  ct[]
}

es_ggplot <- function(ct, ylab,
                      filename = NULL,
                      xlim_lo  = ES_PRE_MIN,
                      xlim_hi  = ES_POST_MAX,
                      width    = 10,
                      height   = 5.5) {
  plot_dt <- ct[rel_year %between% c(xlim_lo, xlim_hi)]
  p <- ggplot(plot_dt, aes(x = rel_year, y = estimate)) +
    annotate("rect",
             xmin = -Inf, xmax = -0.5,
             ymin = -Inf, ymax =  Inf,
             fill = "grey90", alpha = 0.5) +
    geom_hline(yintercept = 0,
               linetype = "dashed", color = "grey50", linewidth = 0.5) +
    geom_vline(xintercept = -0.5,
               color = "grey30", linewidth = 0.6) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
                alpha = 0.12, fill = "grey40") +
    geom_line(color = "grey40", linewidth = 0.4, alpha = 0.6) +
    geom_point(aes(color = period), size = 2.5) +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high, color = period),
                  width = 0.25, linewidth = 0.5) +
    scale_color_manual(
      values = c(Pre = COL_CTRL, Post = COL_TX, Ref = "black"),
      guide  = "none"
    ) +
    scale_x_continuous(
      breaks = seq(xlim_lo, xlim_hi, by = 2L),
      labels = as.character(seq(xlim_lo, xlim_hi, by = 2L))
    ) +
    labs(x = "Years Relative to Reform (tau = 0 is 1999)", y = ylab) +
    theme_pub()
  if (!is.null(filename)) save_gg(p, filename, width, height)
  invisible(list(plot = p, data = ct))
}

# ---- B2.5: ggplot theme ----

theme_pub <- function(base_size = 11) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title         = element_blank(),
      plot.subtitle      = element_blank(),
      plot.caption       = element_blank(),
      panel.grid.minor   = element_blank(),
      panel.grid.major.x = element_blank(),
      legend.position    = "bottom",
      legend.title       = element_blank(),
      axis.title         = element_text(size = base_size),
      axis.text          = element_text(size = base_size - 1L),
      strip.text         = element_text(face = "bold", size = base_size)
    )
}
theme_set(theme_pub())

# ---- B2.6: Model runners ----

# run_did_ols: primary spec (FE_PRIMARY), reform-CEM, CEM weights, mandates.
run_did_ols <- function(outcome, data, fe_spec = FE_PRIMARY) {
  feols(
    as.formula(paste(outcome, "~ did_term", MANDATE_CONTROLS_RHS, "|", fe_spec)),
    data    = data,
    weights = ~cem_weight,
    cluster = ~state
  )
}

# run_did_3dim: FE_3DIM robustness spec.
run_did_3dim <- function(outcome, data) {
  feols(
    as.formula(paste(outcome, "~ did_term", MANDATE_CONTROLS_RHS, "|", FE_3DIM)),
    data    = data,
    weights = ~cem_weight,
    cluster = ~state
  )
}

# run_did_bjs: Borusyak-Jaravel-Spiess imputation estimator.
# first_stage: FE formula for the pre-period/never-treated fit.
# second_stage: treatment indicator formula.
# treatment: column name of the binary treatment indicator (1 post-reform).
# Returns a did2s object which has a coeftable() method compatible with
# extract_did(). If did2s fails (e.g., thin cells), returns NULL silently.
run_did_bjs <- function(outcome, data,
                         first_stage  = BJS_FIRST_STAGE,
                         second_stage = BJS_SECOND_STAGE,
                         treatment    = "post_1999") {
  tryCatch(
    did2s::did2s(
      data          = data,
      yname         = outcome,
      first_stage   = as.formula(first_stage),
      second_stage  = as.formula(second_stage),
      treatment     = treatment,
      cluster_var   = "state"
    ),
    error = function(e) {
      warning(sprintf("BJS did2s failed for %s: %s", outcome, e$message))
      NULL
    }
  )
}

# run_did_naive_cem: reform-CEM sample, year FE only (col 1 of tables 2/3/LUST).
run_did_naive_cem <- function(outcome, data) {
  feols(
    as.formula(paste(outcome, "~ did_term", MANDATE_CONTROLS_RHS, "|", FE_NAIVE)),
    data    = data,
    weights = ~cem_weight,
    cluster = ~state
  )
}

# run_threecol: returns list(col1, col2, col3) for a given outcome on reform-CEM.
#   col1: year FE (naive CEM)
#   col2: make_model_fac^year FE (PRIMARY)
#   col3: fac_wall^fac_fuel^year FE (FE_3DIM robustness)
run_threecol <- function(outcome, data = mm_fac_reform) {
  list(
    col1 = run_did_naive_cem(outcome, data),
    col2 = run_did_ols(outcome,       data),
    col3 = run_did_3dim(outcome,      data),
    col4 = run_did_bjs(outcome,       data)
  )
}

# run_pretrend_test: joint Wald test on pre-period ES coefficients.
run_pretrend_test <- function(m, label, sample) {
  ct       <- coeftable(m)
  pre_pat  <- "rel_year_bin::(-[2-9]|-1[0-9]):texas_treated"
  pre_nms  <- grep(pre_pat, rownames(ct), value = TRUE)
  if (length(pre_nms) < 2L)
    return(data.table(label = label, sample = sample,
                      n_pre_coefs = length(pre_nms),
                      F_stat = NA_real_, p_val = NA_real_))
  wt <- tryCatch(wald(m, keep = pre_nms), error = function(e) NULL)
  if (is.null(wt))
    return(data.table(label = label, sample = sample,
                      n_pre_coefs = length(pre_nms),
                      F_stat = NA_real_, p_val = NA_real_))
  data.table(label       = label,
             sample      = sample,
             n_pre_coefs = length(pre_nms),
             F_stat      = round(wt$stat, 3L),
             p_val       = round(wt$p,    4L))
}

# run_boot_ols_score: Kline-Santos (2012) score wild cluster bootstrap.
# Works with any fixest FE specification including cell^year interactions
# because it never builds a model matrix over FE levels.
# Arguments:
#   model : fitted feols object
#   param : coefficient name to bootstrap (default "did_term")
#   B     : number of bootstrap draws
#   seed  : RNG seed
#   data  : the data.table passed to feols() -- must contain "state" column
# Returns list: p_boot, se_boot, ci_lo, ci_hi, t_stat
run_boot_ols_score <- function(model,
                                param = "did_term",
                                B     = N_BOOT,
                                seed  = BOOT_SEED,
                                data  = NULL) {
  set.seed(seed)
  if (is.null(data)) stop("run_boot_ols_score requires explicit data= argument.")

  ct  <- coeftable(model)
  idx <- which(rownames(ct) == param)[1L]
  if (is.na(idx)) {
    warning(sprintf("param '%s' not in model coeftable", param))
    return(list(p_boot=NA_real_, se_boot=NA_real_,
                ci_lo=NA_real_, ci_hi=NA_real_, t_stat=NA_real_))
  }
  coef_obs <- ct[idx, "Estimate"]
  se_obs   <- ct[idx, "Std. Error"]
  t_obs    <- coef_obs / se_obs

  obs_idx  <- fixest::obs(model)
  states   <- data[["state"]][obs_idx]
  clusters <- unique(states)
  G        <- length(clusters)

  wts_raw <- weights(model)
  wts     <- if (!is.null(wts_raw)) wts_raw[obs_idx] else NULL

  dm <- tryCatch({
    data_est <- data[obs_idx]
    as.data.frame(fixest::demean(model, data = data_est))
  }, error = function(e) {
    tryCatch(
      as.data.frame(fixest::demean(model)),
      error = function(e2) { warning(conditionMessage(e2)); NULL }
    )
  })
  if (is.null(dm)) {
    return(list(p_boot=NA_real_, se_boot=NA_real_,
                ci_lo=NA_real_, ci_hi=NA_real_, t_stat=NA_real_))
  }

  y_nm     <- as.character(formula(model)[[2L]])
  null_nms <- setdiff(names(dm), c(y_nm, param))

  if (!param %in% names(dm)) {
    warning(sprintf("param '%s' missing from demeaned frame", param))
    return(list(p_boot=NA_real_, se_boot=NA_real_,
                ci_lo=NA_real_, ci_hi=NA_real_, t_stat=NA_real_))
  }

  y_vec <- dm[[y_nm]]
  if (length(null_nms) > 0L) {
    X_null <- as.matrix(dm[null_nms])
    if (!is.null(wts)) {
      sw         <- sqrt(wts)
      fit        <- lm.fit(sw * X_null, sw * y_vec)
      resid_null <- fit$residuals / sw
    } else {
      fit        <- lm.fit(X_null, y_vec)
      resid_null <- fit$residuals
    }
  } else {
    resid_null <- y_vec
  }

  x_dm      <- dm[[param]]
  score_vec <- if (!is.null(wts)) wts * x_dm * resid_null else x_dm * resid_null

  valid          <- !is.na(score_vec) & !is.na(states)
  cluster_scores <- as.numeric(tapply(score_vec[valid], states[valid], sum))

  W          <- matrix(sample(c(-1L, 1L), B * G, replace = TRUE),
                       nrow = B, ncol = G)
  score_b    <- as.numeric(W %*% cluster_scores)
  var_b      <- sum(cluster_scores^2)
  t_boot     <- score_b / sqrt(var_b + 1e-10)
  boot_coefs <- t_boot * se_obs + coef_obs

  list(
    p_boot  = mean(abs(t_boot) >= abs(t_obs)),
    se_boot = sd(t_boot) * se_obs,
    ci_lo   = unname(quantile(boot_coefs, 0.025)),
    ci_hi   = unname(quantile(boot_coefs, 0.975)),
    t_stat  = t_obs
  )
}


################################################################################
# B3: LOAD PANEL DATA AND CONSTRUCT VARIABLES
################################################################################

cat("\n========================================\n")
cat("B3: LOAD PANEL DATA AND CONSTRUCT VARIABLES\n")
cat("========================================\n\n")

cat("B3.1: Loading matched panels and full facility panel...\n")

# Full facility panel — used for Table 1 cols 1-3 (full incumbent baseline)
facility_panel_full <- fread(
  file.path(ANALYSIS_DIR, "facility_panel.csv"),
  na.strings = c("", "NA", "N/A")
)
log_step(sprintf("facility_panel: %s rows | %s facilities",
  fmt_n(nrow(facility_panel_full)),
  fmt_n(uniqueN(facility_panel_full$panel_id))))

# Reform-CEM matched panel — PRIMARY ANALYSIS SAMPLE
fac_reform_cem_panel <- fread(
  file.path(ANALYSIS_DIR, "matched_facs_reform_cem.csv"),
  na.strings = c("", "NA", "N/A")
)
log_step(sprintf("Reform-CEM panel: %s rows | %s facilities | %s states",
  fmt_n(nrow(fac_reform_cem_panel)),
  fmt_n(uniqueN(fac_reform_cem_panel$panel_id)),
  uniqueN(fac_reform_cem_panel$state)))

# Assign treatment consistently
for (dt in list(facility_panel_full, fac_reform_cem_panel)) {
  dt[, texas_treated := fcase(
    state == "TX",              1L,
    state %in% CONTROL_STATES, 0L,
    default = NA_integer_
  )]
}

cat("\nB3.2: Verifying required columns in reform-CEM panel...\n")

required_cols <- c(
  "panel_id", "state", "panel_year", "texas_treated", "cem_weight",
  "did_term", "rel_year_es",
  "any_closure",
  "replacement_closure_year", "permanent_closure_year",
  "n_closures_replacement", "n_closures_permanent",
  "facility_exit",
  "exit_no_leak", "exit_with_leak",
  "exit_no_retrofit", "exit_with_retrofit",
  "n_installs", "n_dw_installs", "n_sw_installs",
  "net_tank_change", "capacity_change",
  "single_to_double_year",
  "n_sw_eoy", "n_dw_eoy", "pct_sw",
  "leak_year",
  "tank_closure_revealed",      "tank_closure_known_leak",
  "tank_closure_indeterminate", "tank_closure_clean",
  "tank_closure_revealed_narrow", "tank_closure_known_leak_narrow",
  "tank_closure_revealed_wide",   "tank_closure_known_leak_wide",
  "lust_within_90d_of_closure",
  "tank_closure_revealed_reg",    "tank_closure_known_leak_reg",
  "leak_found_by_closure",
  "make_model_fac", "fac_wall_reform", "fac_fuel_reform", "oldest_age_bin",
  "fac_is_incumbent", "first_install_yr", "had_pre1989_tanks", "all_post1989",
  "n_tanks_at_reform",
  "any_mandate_release_det", "any_mandate_spill_overfill",
  "any_mandate_integrity",
  "avg_tank_age"
)

missing_cols <- setdiff(required_cols, names(fac_reform_cem_panel))
if (length(missing_cols) > 0L)
  stop(sprintf("Reform-CEM panel missing: %s\n  Re-run 02b_Panel_Build.R.",
               paste(missing_cols, collapse = ", ")))
log_step(sprintf("All %d required columns present.", length(required_cols)))

cat("\nB3.3: Deriving outcome variables...\n")

# Apply to reform-CEM panel
fac_reform_cem_panel[, `:=`(
  retrofit_year        = replacement_closure_year,
  retrofit_no_exit     = as.integer(
    replacement_closure_year == 1L & facility_exit == 0L),
  double_walled_installed_year = as.integer(n_dw_installs > 0L),
  capacity_change_year = capacity_change,

  # LUST decomposition -- primary window (0-60d)
  lust_closure_induced = tank_closure_revealed,
  lust_standalone      = as.integer(
    leak_year == 1L & tank_closure_revealed == 0L),

  # Narrow (0-30d)
  lust_closure_induced_narrow = tank_closure_revealed_narrow,
  lust_standalone_narrow      = as.integer(
    leak_year == 1L & tank_closure_revealed_narrow == 0L),

  # Wide (0-90d)
  lust_closure_induced_wide = tank_closure_revealed_wide,
  lust_standalone_wide      = as.integer(
    leak_year == 1L & tank_closure_revealed_wide == 0L),

  # Regulatory (0-45d)
  lust_closure_induced_reg = tank_closure_revealed_reg,
  lust_standalone_reg      = as.integer(
    leak_year == 1L & tank_closure_revealed_reg == 0L)
)]

cat("\nB3.4: Deriving time-varying controls and rel_year alias...\n")

AGE_CTRL_BREAKS <- c(0, 3, 6, 9, 12, 15, 18, 21, 24, Inf)
AGE_CTRL_LABELS <- c("0-2", "3-5", "6-8", "9-11",
                      "12-14", "15-17", "18-20", "21-23", "24+")

fac_reform_cem_panel[, age_bin := factor(
  cut(avg_tank_age, breaks = AGE_CTRL_BREAKS, labels = AGE_CTRL_LABELS,
      right = FALSE, include.lowest = TRUE),
  levels = AGE_CTRL_LABELS
)]
fac_reform_cem_panel[, age_bin     := relevel(age_bin, ref = "0-2")]
fac_reform_cem_panel[, rel_year_bin := rel_year_es]

cat("\nB3.5: Verifying mandate controls zero post-1998...\n")
mandate_check <- fac_reform_cem_panel[
  panel_year > 1998L,
  .(n_rel = sum(any_mandate_release_det),
    n_spi = sum(any_mandate_spill_overfill),
    n_int = sum(any_mandate_integrity))
]
stopifnot(
  "mandate_release_det nonzero post-1998"    = mandate_check$n_rel == 0L,
  "mandate_spill_overfill nonzero post-1998" = mandate_check$n_spi == 0L,
  "mandate_integrity nonzero post-1998"      = mandate_check$n_int == 0L
)
log_step("Mandate post-1998 zero check: PASSED")
cat("\n")


################################################################################
# B4: SAMPLE CONSTRUCTION AND DIAGNOSTICS
################################################################################

cat("\n========================================\n")
cat("B4: SAMPLE CONSTRUCTION AND DIAGNOSTICS\n")
cat("========================================\n\n")

# ---- B4.1: Full incumbent sample (no CEM, no weights) ----
# Used for Table 1 cols 1-3.
# Establishes the unconditional baseline: "what does the simplest version
# look like?" No matching, no reweighting, all incumbent facilities.

cat("B4.1: Full incumbent sample (no CEM)...\n")

mm_fac_full <- facility_panel_full[
  fac_is_incumbent == 1L  &
  !is.na(texas_treated)   &
  !is.na(did_term)        &
  state %in% STUDY_STATES &
  panel_year %between% c(PANEL_START, PANEL_END)
]

# Derive any_closure if not present (may need to add from fac_stock logic)
if (!"any_closure" %in% names(mm_fac_full)) {
  mm_fac_full[, any_closure := as.integer(n_closures > 0L)]
}

leaked_full <- setdiff(unique(mm_fac_full$state), STUDY_STATES)
if (length(leaked_full) > 0L)
  stop(sprintf("Non-study states in full sample: %s",
               paste(leaked_full, collapse = ", ")))

n_fy_full  <- nrow(mm_fac_full)
n_fac_full <- uniqueN(mm_fac_full$panel_id)
n_tx_full  <- mm_fac_full[texas_treated == 1L, uniqueN(panel_id)]
n_ctl_full <- mm_fac_full[texas_treated == 0L, uniqueN(panel_id)]

log_step("Full incumbent sample (mm_fac_full):")
log_step(sprintf("  Facility-years : %s  (TX: %s | CTL: %s)",
  fmt_n(n_fy_full),
  fmt_n(mm_fac_full[texas_treated == 1L, .N]),
  fmt_n(mm_fac_full[texas_treated == 0L, .N])), 1L)
log_step(sprintf("  Facilities     : %s  (TX: %s | CTL: %s)",
  fmt_n(n_fac_full), fmt_n(n_tx_full), fmt_n(n_ctl_full)), 1L)

# ---- B4.2: Reform-CEM primary sample ----
# PRIMARY ANALYSIS SAMPLE throughout B5-B13.
# Alive Dec 22 1998 by construction; no pre-1998 exiters.

cat("B4.2: Reform-CEM primary sample...\n")

mm_fac_reform <- fac_reform_cem_panel[
  !is.na(texas_treated) &
  !is.na(did_term)      &
  panel_year %between% c(PANEL_START, PANEL_END)
]

leaked_reform <- setdiff(unique(mm_fac_reform$state), STUDY_STATES)
if (length(leaked_reform) > 0L)
  stop(sprintf("Non-study states in reform-CEM: %s",
               paste(leaked_reform, collapse = ", ")))

n_fy_reform    <- nrow(mm_fac_reform)
n_fac_reform   <- uniqueN(mm_fac_reform$panel_id)
n_tx_reform    <- mm_fac_reform[texas_treated == 1L, uniqueN(panel_id)]
n_ctl_reform   <- mm_fac_reform[texas_treated == 0L, uniqueN(panel_id)]
n_cells_reform <- mm_fac_reform[!is.na(make_model_fac), uniqueN(make_model_fac)]
n_fy_id_reform <- mm_fac_reform[!is.na(make_model_fac), .N]

log_step("Reform-CEM primary sample (mm_fac_reform):")
log_step(sprintf("  Facility-years : %s  (TX: %s | CTL: %s)",
  fmt_n(n_fy_reform),
  fmt_n(mm_fac_reform[texas_treated == 1L, .N]),
  fmt_n(mm_fac_reform[texas_treated == 0L, .N])), 1L)
log_step(sprintf("  Facilities     : %s  (TX: %s | CTL: %s)",
  fmt_n(n_fac_reform), fmt_n(n_tx_reform), fmt_n(n_ctl_reform)), 1L)
log_step(sprintf("  Cells          : %s  | identified fy: %s",
  fmt_n(n_cells_reform), fmt_n(n_fy_id_reform)), 1L)

# ---- B4.3: ES-aligned sample ----
# Reform-CEM restricted to non-NA make_model_fac.
# For reform-CEM this should equal mm_fac_reform (verified below).

cat("B4.3: ES-aligned sample (reform-CEM, non-NA make_model_fac)...\n")

mm_fac_es <- mm_fac_reform[!is.na(make_model_fac)]
n_fy_es   <- nrow(mm_fac_es)

stopifnot(
  "Reform-CEM has NA make_model_fac rows -- check Panel Build S15" =
    n_fy_es == n_fy_reform
)
log_step(sprintf("mm_fac_es: %s fy -- equals mm_fac_reform (verified)",
  fmt_n(n_fy_es)))

# ---- B4.4: Conditional subsamples ----

cat("B4.4: Conditional subsamples (reform-CEM)...\n")

mm_closure_sample <- mm_fac_reform[any_closure == 1L]

log_step(sprintf("Closure-year subsample: %s fy (TX: %s | CTL: %s)",
  fmt_n(nrow(mm_closure_sample)),
  fmt_n(mm_closure_sample[texas_treated == 1L, .N]),
  fmt_n(mm_closure_sample[texas_treated == 0L, .N])))

# ---- B4.5: Cell coverage diagnostics ----

cat("B4.5: Cell coverage diagnostics...\n")

cell_cov_check <- function(dt, label) {
  cyd <- dt[!is.na(make_model_fac), .(
    n_total  = .N,
    has_both = as.integer(
      sum(texas_treated == 1L) > 0L & sum(texas_treated == 0L) > 0L)
  ), by = .(make_model_fac, panel_year)]
  pct <- cyd[, round(sum(n_total * has_both) / sum(n_total) * 100, 1L)]
  log_step(sprintf("  %s: %.1f%% of fy in identified cells", label, pct))
  if (pct < 70)
    warning(sprintf("%s: only %.1f%% in identified cells (target 70%%).",
                    label, pct))
  cyd[, sample := label]
  cyd[]
}

cov_full   <- cell_cov_check(mm_fac_full,   "Full incumbent (no CEM)")
cov_reform <- cell_cov_check(mm_fac_reform, "Reform-CEM primary")
fwrite(rbind(cov_full, cov_reform),
       file.path(OUTPUT_TABLES, "Diag_B4_CellCoverage.csv"))

# ---- B4.6: Sample summary CSV ----

fwrite(
  data.table(
    sample          = c("full_incumbent", "reform_cem_primary", "es_aligned"),
    n_fy            = c(n_fy_full,   n_fy_reform,   n_fy_es),
    n_facilities    = c(n_fac_full,  n_fac_reform,  n_fac_reform),
    n_tx_fac        = c(n_tx_full,   n_tx_reform,   n_tx_reform),
    n_ctl_fac       = c(n_ctl_full,  n_ctl_reform,  n_ctl_reform),
    n_cells         = c(NA_integer_, n_cells_reform, n_cells_reform),
    has_cem_weights = c(FALSE, TRUE, TRUE),
    panel_start     = PANEL_START,
    panel_end       = PANEL_END
  ),
  file.path(OUTPUT_TABLES, "Diag_B4_SampleSummary.csv")
)
log_step("Wrote: Diag_B4_SampleSummary.csv")
cat("\n")


################################################################################
# B5: TABLE 1 — ANY TANK CLOSURE (6-COLUMN WALK-IN)
################################################################################

cat("\n========================================\n")
cat("B5: TABLE 1 — ANY TANK CLOSURE\n")
cat("========================================\n\n")

# Six-column walk-in showing the full spectrum from naive to primary spec.
#
# Cols 1-3: Full incumbent sample, no CEM, no weights.
#   Col 1: Naive TWFE (facility + year FE, no mandates)
#   Col 2: + mandate controls
#   Col 3: + cell-year FE (make_model_fac^year, still no weights)
#   Purpose: shows raw baseline and additive contribution of each ingredient
#   before matching enters.
#
# Cols 4-6: Reform-CEM sample, CEM weights.
#   Col 4: Year FE + mandate controls (no cell FE)
#   Col 5: + cell-year FE (FE_PRIMARY) -- PRIMARY SPECIFICATION
#   Col 6: + FE_3DIM (fac_wall^fac_fuel^year) -- robustness
#   Purpose: shows what matching adds (col 4 vs col 2) and what cell FE
#   adds within the matched sample (col 5 vs col 4).

cat("B5.1: Fitting six-column walk-in for any_closure...\n")

t1_models <- list(
  # Cols 1-3: full incumbent sample, no weights
  col1 = feols(
    any_closure ~ did_term | panel_id + panel_year,
    data    = mm_fac_full,
    cluster = ~state
  ),
  col2 = feols(
    as.formula(paste("any_closure ~ did_term",
                     MANDATE_CONTROLS_RHS, "| panel_id + panel_year")),
    data    = mm_fac_full,
    cluster = ~state
  ),
  col3 = feols(
    as.formula(paste("any_closure ~ did_term",
                     MANDATE_CONTROLS_RHS, "| panel_id + make_model_fac^panel_year")),
    data    = mm_fac_full,
    cluster = ~state
  ),
  # Cols 4-6: reform-CEM, CEM weights
  col4 = feols(
    as.formula(paste("any_closure ~ did_term",
                     MANDATE_CONTROLS_RHS, "| panel_id + panel_year")),
    data    = mm_fac_reform,
    weights = ~cem_weight,
    cluster = ~state
  ),
  col5 = run_did_ols("any_closure", mm_fac_reform),   # PRIMARY
  col6 = run_did_3dim("any_closure", mm_fac_reform),  # FE_3DIM robustness
  col7 = run_did_bjs("any_closure", mm_fac_reform)    # BJS imputation (Borusyak-Jaravel-Spiess)
)

# Bootstrap inference for primary spec (col5)
boot_t1_col5 <- run_boot_ols_score(
  model = t1_models$col5,
  param = "did_term",
  data  = mm_fac_reform
)

log_step("Table 1 (any_closure) walk-in:")
for (nm in names(t1_models)) {
  d <- extract_did(t1_models[[nm]])
  log_step(sprintf("  %s: beta = %+.4f  SE = %.4f  p = %.3f  N = %s",
    nm, d$beta, d$se, d$p, fmt_n(d$n)), 1L)
}

# ---- B5.2: Control mean ----
cm_any_closure <- round(
  mm_fac_reform[texas_treated == 0L & panel_year < POST_YEAR,
                mean(any_closure, na.rm = TRUE)], 6L)
log_step(sprintf("Control mean (any_closure, pre-reform, reform-CEM): %.5f",
  cm_any_closure))

# ---- B5.3: LaTeX table ----
cat("B5.3: Building Table 1 LaTeX...\n")

{
  vals     <- lapply(t1_models, extract_did)
  beta_str <- paste(sapply(vals, function(v)
    paste0(fmt_est(v$beta), stars_p(v$p))), collapse = " & ")
  se_str   <- paste(sapply(vals, function(v) fmt_se(v$se)), collapse = " & ")
  n_str    <- paste(sapply(vals, function(v) fmt_n(v$n)),   collapse = " & ")

  tex_t1 <- c(
    "\\begin{table}[htbp]",
    "\\centering",
    paste0("\\caption{Table 1: Do Facilities Close More Tanks? ",
           "Effect of the Texas Insurance Reform on Any Tank Closure Activity.}"),
    "\\label{tab:t1_any_closure}",
    "\\begin{tabular}{lccccccc}",
    "\\toprule",
    " & (1) & (2) & (3) & (4) & (5) & (6) & (7) \\\\",
    paste0(" & Naive & +Mandates & +Cell$\\times$Yr",
           " & CEM+Mandates & CEM+Cell$\\times$Yr & CEM+2D-Cell$\\times$Yr & BJS \\\\"),
    "\\midrule",
    paste0("TX $\\times$ Post & ", beta_str, " \\\\"),
    paste0("                  & ", se_str,   " \\\\"),
    paste0("Bootstrap $p$ (col 5) & \\multicolumn{4}{c}{} & ",
           formatC(round(boot_t1_col5$p_boot, 3L), digits = 3L, format = "f"),
           " & \\multicolumn{2}{c}{} \\\\"),
    paste0("$N$               & ", n_str,    " \\\\"),
    paste0("Control mean      & ", mc_tex(7L, fmt_est(cm_any_closure)), " \\\\"),
    "\\midrule",
    "Facility FE              & Yes & Yes & Yes & Yes & Yes & Yes & -- \\\\",
    "Year FE                  & Yes & Yes & Yes & Yes & Yes & Yes & -- \\\\",
    "Cell$\\times$Year FE     & No  & No  & 3D  & No  & 3D  & 2D  & -- \\\\",
    "CEM weights              & No  & No  & No  & Yes & Yes & Yes & Yes \\\\",
    "Mandate controls         & No  & Yes & Yes & Yes & Yes & Yes & Yes \\\\",
    paste0("Sample & ",
           mc_tex(3L, "Full incumbents (no matching)"),
           " & ",
           mc_tex(3L, "Reform-CEM matched"),
           " & Reform-CEM \\\\"),
    "\\bottomrule",
    "\\end{tabular}",
    "\\begin{minipage}{\\linewidth}",
    "\\small",
    paste0(
      "\\textit{Notes:} LPM coefficients. Dependent variable: ", LABEL_ANY_CLOSURE, ". ",
      "Cols (1)--(3) use the full sample of incumbent facilities with no ",
      "matching or reweighting. ",
      "Cols (4)--(6) use reform-survivor CEM matched facilities (alive ",
      "Dec 22 1998, matched on tank count $\\times$ SW count $\\times$ ",
      "fuel type $\\times$ oldest tank age bin). ",
      "3D Cell$\\times$Year FE: make\\_model\\_fac (wall$\\times$fuel$\\times$age) ",
      "$\\times$ year. ",
      "2D Cell$\\times$Year FE: fac\\_wall$\\times$fac\\_fuel$\\times$year ",
      "(drops age bin; robustness). ",
      "Col (5) is the primary specification used in all subsequent tables. ",
      "Col (7) reports the Borusyak, Jaravel, and Spiess (2024) two-stage ",
      "imputation estimator (BJS): stage 1 fits unit and year fixed effects on ",
      "untreated observations; stage 2 regresses the residualized outcome on ",
      "the treatment indicator. BJS is more efficient than TWFE under ",
      "heterogeneous treatment effects and provides an independent check on ",
      "the primary specification. ",
      "Bootstrap p-value reported for col (5) is from a wild cluster score ",
      "bootstrap (Kline-Santos 2012) with Rademacher weights, B = 9999, ",
      "clustering by state. ",
      "Cluster-robust SEs by state (17 clusters). ",
      "$^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$."
    ),
    "\\end{minipage}",
    "\\end{table}"
  )
  write_tex(tex_t1, file.path(OUTPUT_TABLES, "T1_AnyClosure.tex"))
}

# ---- B5.4: CSV ----
t1_dt <- rbindlist(lapply(names(t1_models), function(nm) {
  d <- extract_did(t1_models[[nm]])
  data.table(
    outcome   = "any_closure", col = nm,
    sample    = ifelse(nm %in% c("col1","col2","col3"),
                       "full_incumbent", "reform_cem"),
    fe        = c(col1="naive",col2="naive+mand",col3="primary_nomatch",
                  col4="naive_cem",col5="primary",col6="fe3dim",
                  col7="bjs_imputation")[[nm]],
    beta      = round(d$beta, 6L), se = round(d$se, 6L),
    p         = round(d$p, 4L),    stars = stars_p(d$p),
    n         = d$n,
    ctrl_mean = cm_any_closure
  )
}))
fwrite(t1_dt, file.path(OUTPUT_TABLES, "T1_AnyClosure.csv"))
log_step("Wrote: T1_AnyClosure.tex + T1_AnyClosure.csv")

# Global assignment
m_t1_primary <- t1_models$col5
cat("\n")


################################################################################
# B6: TABLE 2 — CLOSURE COMPOSITION + TABLE 2b — DESCRIPTIVE CONDITIONAL
################################################################################

cat("\n========================================\n")
cat("B6: TABLE 2 — CLOSURE COMPOSITION\n")
cat("========================================\n\n")

# Three outcomes, three columns, all on reform-CEM sample.
# Outcome definitions (plain language for reader):
#
#   facility_exit            -- Facility closes entirely: disappears from
#                               registry, all tanks gone, no future observations.
#
#   replacement_closure_year -- Facility upgrades a tank: at least one tank
#                               closure paired with a new tank installation in
#                               the same year, indicating a technology upgrade
#                               rather than exit.
#
#   permanent_closure_year   -- Facility shrinks its tank stock: at least one
#                               tank removed with no corresponding new
#                               installation. The facility continues operating
#                               but with a smaller portfolio. A facility can
#                               appear in both this and replacement_closure_year
#                               in the same year if it both upgraded one tank
#                               and permanently removed another.

cat("B6.1: Fitting closure composition models (3 outcomes x 3 cols)...\n")

t2_exit    <- run_threecol("facility_exit")
t2_upgrade <- run_threecol("replacement_closure_year")
t2_shrink  <- run_threecol("permanent_closure_year")

# Bootstrap inference for primary specs (col2)
boot_t2_exit    <- run_boot_ols_score(t2_exit$col2,    data = mm_fac_reform)
boot_t2_upgrade <- run_boot_ols_score(t2_upgrade$col2, data = mm_fac_reform)
boot_t2_shrink  <- run_boot_ols_score(t2_shrink$col2,  data = mm_fac_reform)

# Control means (reform-CEM, pre-reform controls)
cm_exit    <- round(mm_fac_reform[texas_treated == 0L & panel_year < POST_YEAR,
                                   mean(facility_exit,            na.rm = TRUE)], 6L)
cm_upgrade <- round(mm_fac_reform[texas_treated == 0L & panel_year < POST_YEAR,
                                   mean(replacement_closure_year, na.rm = TRUE)], 6L)
cm_shrink  <- round(mm_fac_reform[texas_treated == 0L & panel_year < POST_YEAR,
                                   mean(permanent_closure_year,   na.rm = TRUE)], 6L)

log_step("Table 2 results (primary spec, col2):")
for (item in list(
  list(nm="exit",    m=t2_exit$col2,    cm=cm_exit),
  list(nm="upgrade", m=t2_upgrade$col2, cm=cm_upgrade),
  list(nm="shrink",  m=t2_shrink$col2,  cm=cm_shrink)
)) {
  d <- extract_did(item$m)
  log_step(sprintf("  %-8s: %+.4f (%.4f) p=%.3f  ctrl_mean=%.5f",
    item$nm, d$beta, d$se, d$p, item$cm), 1L)
}

# ---- B6.2: Three-panel LaTeX table ----
cat("B6.2: Building Table 2 LaTeX...\n")

build_threepanel_closure_tex <- function(panels, caption, label_tex) {
  hdr <- paste0(
    " & (1) Year FE & (2) Cell$\\times$Yr & (3) 2D-Cell$\\times$Yr",
    " & (4) BJS \\\\"
  )

  panel_rows <- unlist(lapply(seq_along(panels), function(i) {
    p   <- panels[[i]]
    lbl <- LETTERS[i]
    vals <- lapply(p$models, extract_did)
    beta_str <- paste(sapply(vals, function(v)
      paste0(fmt_est(v$beta), stars_p(v$p))), collapse = " & ")
    se_str <- paste(sapply(vals, function(v) fmt_se(v$se)), collapse = " & ")
    n_str  <- paste(sapply(vals, function(v) fmt_n(v$n)),   collapse = " & ")
    boot_p_str <- if (!is.null(p$boot)) {
      formatC(round(p$boot$p_boot, 3L), digits = 3L, format = "f")
    } else "--"
    out <- c(
      paste0("\\multicolumn{5}{l}{\\textit{Panel ", lbl, ": ",
             p$label, "}} \\\\[0.3em]"),
      paste0("TX $\\times$ Post & ", beta_str, " \\\\"),
      paste0("                  & ", se_str,   " \\\\"),
      paste0("Bootstrap $p$ (col 2) & & ", boot_p_str,
             " & \\multicolumn{2}{c}{} \\\\"),
      paste0("$N$               & ", n_str,    " \\\\"),
      paste0("Control mean      & ", mc_tex(4L, fmt_est(p$cm)), " \\\\")
    )
    if (i < length(panels)) out <- c(out, "\\midrule")
    out
  }), recursive = FALSE)

  c(
    "\\begin{table}[htbp]",
    "\\centering",
    paste0("\\caption{", caption, "}"),
    paste0("\\label{tab:", label_tex, "}"),
    "\\begin{tabular}{lcccc}",
    "\\toprule",
    hdr,
    "\\midrule",
    panel_rows,
    "\\midrule",
    "Facility FE            & Yes & Yes & Yes & -- \\\\",
    "Year FE                & Yes & Yes & Yes & -- \\\\",
    "Cell$\\times$Year FE   & No  & 3D  & 2D  & -- \\\\",
    "CEM weights            & Yes & Yes & Yes & Yes \\\\",
    "Mandate controls       & Yes & Yes & Yes & Yes \\\\",
    "Sample & \\multicolumn{4}{c}{Reform-CEM matched} \\\\",
    "\\bottomrule",
    "\\end{tabular}",
    "\\begin{minipage}{\\linewidth}",
    "\\small",
    paste0(
      "\\textit{Notes:} LPM coefficients. All columns use reform-survivor ",
      "CEM matched facilities with CEM weights. ",
      "Col (4) reports the Borusyak, Jaravel, and Spiess (2024) two-stage ",
      "imputation estimator. ",
      "Bootstrap p-values for col (2) are from a wild cluster score bootstrap ",
      "(Kline-Santos 2012) with Rademacher weights, B = 9999. ",
      "\\textit{Panel A} (", LABEL_FAC_EXIT, "): ",
      "the facility disappears from the registry -- all tanks are gone and ",
      "no further observations exist. ",
      "\\textit{Panel B} (", LABEL_UPGRADE, "): ",
      "at least one tank closure is paired with a new tank installation, ",
      "indicating a technology upgrade. ",
      "\\textit{Panel C} (", LABEL_SHRINK, "): ",
      "at least one tank is closed with no corresponding new installation; ",
      "the facility continues operating with a smaller portfolio. ",
      "A facility-year can simultaneously appear in Panels B and C if it ",
      "both upgraded one tank and permanently removed another. ",
      "3D Cell$\\times$Year FE: make\\_model\\_fac $\\times$ year. ",
      "2D Cell$\\times$Year FE: fac\\_wall$\\times$fac\\_fuel$\\times$year. ",
      "Cluster-robust SEs by state (17 clusters). ",
      "$^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$."
    ),
    "\\end{minipage}",
    "\\end{table}"
  )
}

tex_t2 <- build_threepanel_closure_tex(
  panels = list(
    list(models = t2_exit,
         label  = paste0("Facility Closes Entirely -- ",
                         "\\textit{facility\\_exit}"),
         cm     = cm_exit,
         boot   = boot_t2_exit),
    list(models = t2_upgrade,
         label  = paste0("Facility Upgrades a Tank -- ",
                         "\\textit{replacement\\_closure\\_year}"),
         cm     = cm_upgrade,
         boot   = boot_t2_upgrade),
    list(models = t2_shrink,
         label  = paste0("Facility Shrinks Tank Stock -- ",
                         "\\textit{permanent\\_closure\\_year}"),
         cm     = cm_shrink,
         boot   = boot_t2_shrink)
  ),
  caption   = paste0(
    "Table 2: What Are These Closures? ",
    "Decomposition of Tank Closure Events into Exit, Upgrade, and Stock Reduction."
  ),
  label_tex = "t2_closure_composition"
)
write_tex(tex_t2, file.path(OUTPUT_TABLES, "T2_ClosureComposition.tex"))

# ---- B6.3: Table 2b — descriptive conditional composition ----
cat("B6.3: Table 2b — conditional-on-closure descriptive regression...\n")

# Conditioning on any_closure == 1. Endogenous -- descriptive only.
# Tells the reader: given that a facility chose to close a tank that year,
# what type of closure event was it?

t2b_exit    <- run_did_ols("facility_exit",            mm_closure_sample)
t2b_upgrade <- run_did_ols("replacement_closure_year", mm_closure_sample)
t2b_shrink  <- run_did_ols("permanent_closure_year",   mm_closure_sample)

# Bootstrap inference for descriptive specs
boot_t2b_exit    <- run_boot_ols_score(t2b_exit,    data = mm_closure_sample)
boot_t2b_upgrade <- run_boot_ols_score(t2b_upgrade, data = mm_closure_sample)
boot_t2b_shrink  <- run_boot_ols_score(t2b_shrink,  data = mm_closure_sample)

cm_cond_exit    <- round(mm_closure_sample[texas_treated == 0L & panel_year < POST_YEAR,
                                            mean(facility_exit,            na.rm = TRUE)], 6L)
cm_cond_upgrade <- round(mm_closure_sample[texas_treated == 0L & panel_year < POST_YEAR,
                                            mean(replacement_closure_year, na.rm = TRUE)], 6L)
cm_cond_shrink  <- round(mm_closure_sample[texas_treated == 0L & panel_year < POST_YEAR,
                                            mean(permanent_closure_year,   na.rm = TRUE)], 6L)

log_step("Table 2b (conditional on closure, primary spec):")
for (item in list(
  list(nm="exit",    m=t2b_exit,    cm=cm_cond_exit),
  list(nm="upgrade", m=t2b_upgrade, cm=cm_cond_upgrade),
  list(nm="shrink",  m=t2b_shrink,  cm=cm_cond_shrink)
)) {
  d <- extract_did(item$m)
  log_step(sprintf("  %-8s: %+.4f (%.4f) p=%.3f  ctrl_mean=%.5f",
    item$nm, d$beta, d$se, d$p, item$cm), 1L)
}

{
  t2b_items <- list(
    list(m=t2b_exit,    cm=cm_cond_exit,
         label="Facility Closes Entirely",
         boot=boot_t2b_exit),
    list(m=t2b_upgrade, cm=cm_cond_upgrade,
         label="Facility Upgrades a Tank",
         boot=boot_t2b_upgrade),
    list(m=t2b_shrink,  cm=cm_cond_shrink,
         label="Facility Shrinks Tank Stock",
         boot=boot_t2b_shrink)
  )
  t2b_rows <- sapply(t2b_items, function(o) {
    d <- extract_did(o$m)
    boot_p <- formatC(round(o$boot$p_boot, 3L), digits = 3L, format = "f")
    paste0(o$label, " & ",
           paste0(fmt_est(d$beta), stars_p(d$p)), " & ",
           fmt_se(d$se), " & ",
           boot_p, " & ",
           fmt_est(o$cm), " \\\\")
  })

  tex_t2b <- c(
    "\\begin{table}[htbp]",
    "\\centering",
    paste0("\\caption{Table 2b: What Type of Closure? Descriptive Composition of ",
           "Tank Closure Events Among Facilities That Closed at Least One Tank.}"),
    "\\label{tab:t2b_closure_conditional}",
    "\\begin{tabular}{lcccc}",
    "\\toprule",
    "Closure Type & TX $\\times$ Post & (SE) & Bootstrap $p$ & Control Mean \\\\",
    "\\midrule",
    t2b_rows,
    "\\midrule",
    paste0("$N$ (closing fac-years) & \\multicolumn{4}{c}{",
           fmt_n(nobs(t2b_exit)), "} \\\\"),
    "Cell$\\times$Year FE & \\multicolumn{4}{c}{3D (make\\_model\\_fac $\\times$ year)} \\\\",
    "CEM weights          & \\multicolumn{4}{c}{Yes} \\\\",
    "Mandate controls     & \\multicolumn{4}{c}{Yes} \\\\",
    "Sample               & \\multicolumn{4}{c}{Reform-CEM, any\\_closure $= 1$} \\\\",
    "\\bottomrule",
    "\\end{tabular}",
    "\\begin{minipage}{\\linewidth}",
    "\\small",
    paste0(
      "\\textit{Notes:} LPM coefficients. Sample restricted to reform-CEM ",
      "facility-years in which at least one tank closure occurred ",
      "(any\\_closure $= 1$). Each row describes the composition of closure ",
      "events: conditional on closing a tank that year, was the event a ",
      "full facility exit, a technology upgrade, or a tank stock reduction? ",
      "\\textit{These estimates are descriptive -- they condition on the ",
      "endogenous decision to close a tank and should not be interpreted as ",
      "causal effects of the reform on closure type.} For unconditional ",
      "causal estimates see Table 2. ",
      "Bootstrap p-values are from a wild cluster score bootstrap ",
      "(Kline-Santos 2012) with Rademacher weights, B = 9999, clustering by ",
      "state. ",
      "Cluster-robust SEs by state. ",
      "$^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$."
    ),
    "\\end{minipage}",
    "\\end{table}"
  )
  write_tex(tex_t2b, file.path(OUTPUT_TABLES, "T2b_ClosureConditional.tex"))
}

# ---- B6.4: CSV export ----
t2_dt <- rbindlist(lapply(
  list(
    list(out="facility_exit",            panel="A_exit",
         mods=t2_exit,    cm=cm_exit),
    list(out="replacement_closure_year", panel="B_upgrade",
         mods=t2_upgrade, cm=cm_upgrade),
    list(out="permanent_closure_year",   panel="C_shrink",
         mods=t2_shrink,  cm=cm_shrink)
  ),
  function(o) {
    rbindlist(lapply(names(o$mods), function(col) {
      d <- extract_did(o$mods[[col]])
      data.table(outcome=o$out, panel=o$panel, col=col,
                 beta=round(d$beta,6L), se=round(d$se,6L),
                 p=round(d$p,4L), stars=stars_p(d$p), n=d$n,
                 ctrl_mean=o$cm, sample="reform_cem_unconditional")
    }))
  }
))
fwrite(t2_dt, file.path(OUTPUT_TABLES, "T2_ClosureComposition.csv"))
log_step("Wrote: T2_ClosureComposition.tex + T2b_ClosureConditional.tex + T2_ClosureComposition.csv")

# Global assignments
m_t2_exit    <- t2_exit$col2
m_t2_upgrade <- t2_upgrade$col2
m_t2_shrink  <- t2_shrink$col2
cat("\n")


################################################################################
# B7: TABLE 3 — PORTFOLIO COMPOSITION OUTCOMES
################################################################################

cat("\n========================================\n")
cat("B7: TABLE 3 — PORTFOLIO COMPOSITION OUTCOMES\n")
cat("========================================\n\n")

# Unconditional DiD on portfolio outcomes. All reform-CEM facility-years.
# The reform affects portfolio composition through the replacement channel
# documented in Table 2 Panel B, but these regressions do not condition on
# replacement occurring and are therefore causally clean.
#
# Outcomes (rows of the table):
#   single_to_double_year        -- upgraded wall type this year (SW out, DW in)
#   double_walled_installed_year -- any DW tank installed this year
#   net_tank_change              -- change in total tank count (continuous)
#   capacity_change_year         -- change in total capacity, gallons (continuous)
#   n_sw_eoy                     -- SW tanks at end of year (continuous)
#   n_dw_eoy                     -- DW tanks at end of year (continuous)
#   pct_sw                       -- share of portfolio that is SW (0-1)

cat("B7.1: Fitting portfolio outcome models...\n")

portfolio_outcomes <- c(
  sw_to_dw        = "single_to_double_year",
  dw_installed    = "double_walled_installed_year",
  net_tanks       = "net_tank_change",
  capacity_change = "capacity_change_year",
  n_sw_eoy        = "n_sw_eoy",
  n_dw_eoy        = "n_dw_eoy",
  pct_sw          = "pct_sw"
)

portfolio_row_labels <- c(
  sw_to_dw        = "SW Tank Removed, DW Tank Installed (0/1)",
  dw_installed    = "Any DW Tank Installed This Year (0/1)",
  net_tanks       = "Net Change in Tank Count (continuous)",
  capacity_change = "Change in Total Capacity, gallons (continuous)",
  n_sw_eoy        = "Single-Wall Tanks at End of Year (continuous)",
  n_dw_eoy        = "Double-Wall Tanks at End of Year (continuous)",
  pct_sw          = "Share of Portfolio That Is Single-Wall (0--1)"
)

t3_models <- lapply(portfolio_outcomes, run_threecol)
names(t3_models) <- names(portfolio_outcomes)

# Bootstrap inference for primary specs (col2)
boot_t3 <- lapply(t3_models, function(mods)
  run_boot_ols_score(mods$col2, data = mm_fac_reform))
names(boot_t3) <- names(t3_models)

t3_cms <- sapply(portfolio_outcomes, function(y)
  round(mean(mm_fac_reform[texas_treated == 0L & panel_year < POST_YEAR,
                            get(y)], na.rm = TRUE), 6L))

log_step("Table 3 (primary spec, col2):")
for (nm in names(t3_models)) {
  d <- extract_did(t3_models[[nm]]$col2)
  log_step(sprintf("  %-16s: %+.4f (%.4f) p=%.3f  ctrl_mean=%.5f",
    nm, d$beta, d$se, d$p, t3_cms[[nm]]), 1L)
}

# ---- B7.2: Row-format LaTeX table ----
cat("B7.2: Building Table 3 LaTeX (row format)...\n")

{
  # Each outcome is three rows: estimate row + SE row + bootstrap-p row
  # Cols: outcome label | col1 | col2 | col3 | col4 BJS | ctrl mean
  t3_rows <- unlist(lapply(names(t3_models), function(nm) {
    mods <- t3_models[[nm]]
    vals <- lapply(mods, extract_did)
    beta_str <- paste(sapply(vals, function(v)
      paste0(fmt_est(v$beta), stars_p(v$p))), collapse = " & ")
    se_str <- paste(sapply(vals, function(v) fmt_se(v$se)), collapse = " & ")
    boot_p <- formatC(round(boot_t3[[nm]]$p_boot, 3L), digits = 3L, format = "f")
    c(
      paste0(portfolio_row_labels[[nm]], " & ",
             beta_str, " & ", fmt_est(t3_cms[[nm]]), " \\\\"),
      paste0(" & ", se_str, " &  \\\\"),
      paste0(" \\hspace{1em}\\textit{boot $p$ (col 2)} & & ",
             boot_p, " & \\multicolumn{2}{c}{} &  \\\\[0.3em]")
    )
  }), recursive = FALSE)

  n_obs_t3 <- fmt_n(nobs(t3_models[[1L]]$col2))

  tex_t3 <- c(
    "\\begin{table}[htbp]",
    "\\centering",
    paste0("\\caption{Table 3: What Do Replacements Do to the Portfolio? ",
           "Effect of the Texas Insurance Reform on Facility Portfolio Composition.}"),
    "\\label{tab:t3_portfolio}",
    "\\resizebox{\\textwidth}{!}{",
    "\\begin{tabular}{lccccc}",
    "\\toprule",
    paste0("Outcome & (1) Year FE & (2) Cell$\\times$Yr",
           " & (3) 2D-Cell$\\times$Yr & (4) BJS & Ctrl Mean \\\\"),
    "\\midrule",
    t3_rows,
    "\\midrule",
    paste0("$N$ & \\multicolumn{5}{c}{", n_obs_t3, "} \\\\"),
    "Facility FE            & Yes & Yes & Yes & -- & \\\\",
    "Year FE                & Yes & Yes & Yes & -- & \\\\",
    "Cell$\\times$Year FE   & No  & 3D  & 2D  & -- & \\\\",
    "CEM weights            & Yes & Yes & Yes & Yes & \\\\",
    "Mandate controls       & Yes & Yes & Yes & Yes & \\\\",
    "Sample & \\multicolumn{5}{c}{Reform-CEM matched, all facility-years} \\\\",
    "\\bottomrule",
    "\\end{tabular}",
    "}",
    "\\begin{minipage}{\\linewidth}",
    "\\small",
    paste0(
      "\\textit{Notes:} OLS coefficients. Unconditional estimates on all ",
      "reform-CEM facility-years. Binary outcomes (rows 1--2) estimated by LPM; ",
      "continuous outcomes (rows 3--7) by OLS. ",
      "The reform shifts portfolio composition through the replacement channel ",
      "documented in Table 2 Panel B, but these regressions do not condition on ",
      "replacement occurring and are causally interpretable. ",
      "3D Cell$\\times$Year FE: make\\_model\\_fac (wall$\\times$fuel$\\times$age) ",
      "$\\times$ year. ",
      "2D Cell$\\times$Year FE: fac\\_wall$\\times$fac\\_fuel$\\times$year ",
      "(robustness: drops age bin from cell definition). ",
      "Col (4) reports the Borusyak, Jaravel, and Spiess (2024) two-stage ",
      "imputation estimator. ",
      "Bootstrap p-values for col (2) are from a wild cluster score bootstrap ",
      "(Kline-Santos 2012) with Rademacher weights, B = 9999. ",
      "Cluster-robust SEs by state (17 clusters). ",
      "$^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$."
    ),
    "\\end{minipage}",
    "\\end{table}"
  )
  write_tex(tex_t3, file.path(OUTPUT_TABLES, "T3_Portfolio.tex"))
}

# ---- B7.3: CSV ----
t3_dt <- rbindlist(lapply(names(t3_models), function(nm) {
  rbindlist(lapply(names(t3_models[[nm]]), function(col) {
    d <- extract_did(t3_models[[nm]][[col]])
    data.table(
      outcome   = portfolio_outcomes[[nm]],
      row_label = portfolio_row_labels[[nm]],
      col       = col,
      beta      = round(d$beta, 6L), se = round(d$se, 6L),
      p         = round(d$p, 4L),    stars = stars_p(d$p),
      n         = d$n,
      ctrl_mean = t3_cms[[nm]]
    )
  }))
}))
fwrite(t3_dt, file.path(OUTPUT_TABLES, "T3_Portfolio.csv"))
log_step("Wrote: T3_Portfolio.tex + T3_Portfolio.csv")
cat("\n")


################################################################################
# B8: LUST TABLES
################################################################################

cat("\n========================================\n")
cat("B8: LUST TABLES\n")
cat("========================================\n\n")

# LUST TABLE A: Total leak discovery
#   Does the reform change the overall rate at which leaks are discovered?
#
# LUST TABLE B: Background vs Inspection-Triggered Decomposition
#
#   Panel A -- Background Leaks (lust_standalone_*):
#     Leak reported at a facility with NO tank closure within the window.
#     These leaks are discovered through routine monitoring, third-party
#     complaints, or incidental detection -- NOT triggered by a closure
#     inspection. An increase here means more genuine operational failures
#     are occurring or being detected through non-closure channels.
#
#   Panel B -- Inspection-Triggered Leaks (lust_closure_induced_*):
#     Leak reported at a facility WITHIN the inspection window around a tank
#     closure. Closures require a regulatory inspection; those inspections
#     reveal latent contamination that already existed underground. An
#     increase here reflects faster discovery of pre-existing leaks -- an
#     indirect environmental benefit of the reform, NOT evidence of more
#     leaking per se.
#
#   Four window definitions (rows within each panel):
#     Primary    (0-60d): main specification
#     Narrow     (0-30d): tighter, more conservative
#     Regulatory (0-45d): matches TX 45-day inspection rule
#     Wide       (0-90d): looser upper bound sensitivity check

cat("B8.1: LUST Table A — total leak discovery...\n")

t_lust_a <- run_threecol("leak_year")
cm_lust_total <- round(
  mm_fac_reform[texas_treated == 0L & panel_year < POST_YEAR,
                mean(leak_year, na.rm = TRUE)], 6L)

# Bootstrap inference for primary spec (col2)
boot_lust_a <- run_boot_ols_score(t_lust_a$col2, data = mm_fac_reform)

{
  d_la <- extract_did(t_lust_a$col2)
  log_step(sprintf("Total LUST (primary): %+.4f (%.4f) p=%.3f  ctrl_mean=%.5f",
    d_la$beta, d_la$se, d_la$p, cm_lust_total))
}

# LUST Table A LaTeX
{
  vals_la  <- lapply(t_lust_a, extract_did)
  beta_la  <- paste(sapply(vals_la, function(v)
    paste0(fmt_est(v$beta), stars_p(v$p))), collapse = " & ")
  se_la    <- paste(sapply(vals_la, function(v) fmt_se(v$se)), collapse = " & ")
  n_la     <- paste(sapply(vals_la, function(v) fmt_n(v$n)),   collapse = " & ")

  tex_lust_a <- c(
    "\\begin{table}[htbp]",
    "\\centering",
    paste0("\\caption{LUST Table A: Does the Reform Change the Rate of Leak ",
           "Discoveries? Effect of the Texas Insurance Reform on Total LUST Incidence.}"),
    "\\label{tab:lust_a_total}",
    "\\begin{tabular}{lcccc}",
    "\\toprule",
    " & (1) Year FE & (2) Cell$\\times$Yr & (3) 2D-Cell$\\times$Yr & (4) BJS \\\\",
    "\\midrule",
    paste0("TX $\\times$ Post & ", beta_la, " \\\\"),
    paste0("                  & ", se_la,   " \\\\"),
    paste0("Bootstrap $p$ (col 2) & & ",
           formatC(round(boot_lust_a$p_boot, 3L), digits = 3L, format = "f"),
           " & \\multicolumn{2}{c}{} \\\\"),
    paste0("$N$               & ", n_la,    " \\\\"),
    paste0("Control mean      & ", mc_tex(4L, fmt_est(cm_lust_total)), " \\\\"),
    "\\midrule",
    "Facility FE            & Yes & Yes & Yes & -- \\\\",
    "Year FE                & Yes & Yes & Yes & -- \\\\",
    "Cell$\\times$Year FE   & No  & 3D  & 2D  & -- \\\\",
    "CEM weights            & Yes & Yes & Yes & Yes \\\\",
    "Mandate controls       & Yes & Yes & Yes & Yes \\\\",
    "Sample & \\multicolumn{4}{c}{Reform-CEM matched} \\\\",
    "\\bottomrule",
    "\\end{tabular}",
    "\\begin{minipage}{\\linewidth}",
    "\\small",
    paste0(
      "\\textit{Notes:} LPM coefficients. Dependent variable: ",
      LABEL_LUST_TOTAL, ". ",
      "Total leak discovery includes leaks found through any channel ",
      "(routine monitoring, complaints, and closure inspections). ",
      "See LUST Table B for decomposition by discovery channel. ",
      "Col (4) reports the Borusyak, Jaravel, and Spiess (2024) two-stage ",
      "imputation estimator. ",
      "Bootstrap p-value for col (2) is from a wild cluster score bootstrap ",
      "(Kline-Santos 2012) with Rademacher weights, B = 9999. ",
      "Cluster-robust SEs by state (17 clusters). ",
      "$^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$."
    ),
    "\\end{minipage}",
    "\\end{table}"
  )
  write_tex(tex_lust_a, file.path(OUTPUT_TABLES, "T_LUST_A_Total.tex"))
}

cat("B8.2: LUST Table B — decomposition across four windows...\n")

# Window definitions
lust_windows <- list(
  list(label   = "Primary (0--60 day window)",
       sa_var  = "lust_standalone",
       ind_var = "lust_closure_induced"),
  list(label   = "Narrow (0--30 day window)",
       sa_var  = "lust_standalone_narrow",
       ind_var = "lust_closure_induced_narrow"),
  list(label   = "Regulatory (0--45 day window)",
       sa_var  = "lust_standalone_reg",
       ind_var = "lust_closure_induced_reg"),
  list(label   = "Wide (0--90 day window)",
       sa_var  = "lust_standalone_wide",
       ind_var = "lust_closure_induced_wide")
)

# Fit all models: 4 windows x 2 types x 4 cols = 32 models (4th col = BJS)
lust_b_data <- lapply(lust_windows, function(w) {
  sa_mods  <- run_threecol(w$sa_var)
  ind_mods <- run_threecol(w$ind_var)
  list(
    window   = w$label,
    sa_var   = w$sa_var,
    ind_var  = w$ind_var,
    sa_mods  = sa_mods,
    ind_mods = ind_mods,
    boot_sa  = run_boot_ols_score(sa_mods$col2,  data = mm_fac_reform),
    boot_ind = run_boot_ols_score(ind_mods$col2, data = mm_fac_reform),
    cm_sa    = round(mm_fac_reform[texas_treated == 0L & panel_year < POST_YEAR,
                                    mean(get(w$sa_var),  na.rm = TRUE)], 6L),
    cm_ind   = round(mm_fac_reform[texas_treated == 0L & panel_year < POST_YEAR,
                                    mean(get(w$ind_var), na.rm = TRUE)], 6L)
  )
})

log_step("LUST Table B (primary spec col2):")
for (w in lust_b_data) {
  d_sa  <- extract_did(w$sa_mods$col2)
  d_ind <- extract_did(w$ind_mods$col2)
  log_step(sprintf(
    "  %-32s  background: %+.4f (%.4f) p=%.3f | inspection: %+.4f (%.4f) p=%.3f",
    w$window,
    d_sa$beta,  d_sa$se,  d_sa$p,
    d_ind$beta, d_ind$se, d_ind$p), 1L)
}

# ---- B8.3: LUST Table B LaTeX ----
cat("B8.3: Building LUST Table B LaTeX...\n")

{
  # Helper: format one window row (beta + SE + bootstrap p)
  fmt_lust_row <- function(mods, cm, window_label, boot) {
    vals     <- lapply(mods, extract_did)
    beta_str <- paste(sapply(vals, function(v)
      paste0(fmt_est(v$beta), stars_p(v$p))), collapse = " & ")
    se_str   <- paste(sapply(vals, function(v) fmt_se(v$se)), collapse = " & ")
    boot_p   <- formatC(round(boot$p_boot, 3L), digits = 3L, format = "f")
    c(
      paste0(window_label, " & ", beta_str, " & ", fmt_est(cm), " \\\\"),
      paste0(" & ", se_str, " &  \\\\"),
      paste0(" \\hspace{0.5em}\\textit{boot $p$ (col 2)} & & ",
             boot_p, " & \\multicolumn{2}{c}{} &  \\\\[0.2em]")
    )
  }

  pa_rows <- unlist(lapply(lust_b_data, function(w)
    fmt_lust_row(w$sa_mods, w$cm_sa, w$window, w$boot_sa)), recursive = FALSE)

  pb_rows <- unlist(lapply(lust_b_data, function(w)
    fmt_lust_row(w$ind_mods, w$cm_ind, w$window, w$boot_ind)), recursive = FALSE)

  n_obs_lust <- fmt_n(nobs(lust_b_data[[1L]]$sa_mods$col2))

  tex_lust_b <- c(
    "\\begin{table}[htbp]",
    "\\centering",
    paste0("\\caption{LUST Table B: Background vs.\\ Inspection-Triggered Leak ",
           "Discoveries. Decomposition of LUST Incidence by Discovery Channel ",
           "and Inspection Window Definition.}"),
    "\\label{tab:lust_b_decomp}",
    "\\begin{tabular}{lccccc}",
    "\\toprule",
    paste0("Window & (1) Year FE & (2) Cell$\\times$Yr",
           " & (3) 2D-Cell$\\times$Yr & (4) BJS & Ctrl Mean \\\\"),
    "\\midrule",
    # Panel A header
    paste0("\\multicolumn{6}{l}{\\textit{Panel A: Background Leaks}} \\\\"),
    paste0("\\multicolumn{6}{l}{\\small\\textit{",
           "Leak reported with no tank closure within the window. ",
           "Reflects routine monitoring or complaint-driven discovery. ",
           "}} \\\\"),
    paste0("\\multicolumn{6}{l}{\\small\\textit{",
           "An increase here indicates more genuine operational failures ",
           "or increased non-closure detection.}} \\\\[0.3em]"),
    pa_rows,
    "\\midrule",
    # Panel B header
    paste0("\\multicolumn{6}{l}{\\textit{Panel B: Inspection-Triggered Leaks}} \\\\"),
    paste0("\\multicolumn{6}{l}{\\small\\textit{",
           "Leak reported within the window around a tank closure. ",
           "Closures require regulatory inspection; inspections reveal ",
           "}} \\\\"),
    paste0("\\multicolumn{6}{l}{\\small\\textit{",
           "latent contamination that already existed. An increase here ",
           "reflects faster discovery of pre-existing leaks -- an indirect ",
           "environmental benefit.}} \\\\[0.3em]"),
    pb_rows,
    "\\midrule",
    paste0("$N$ & \\multicolumn{5}{c}{", n_obs_lust, "} \\\\"),
    "Facility FE            & Yes & Yes & Yes & -- & \\\\",
    "Year FE                & Yes & Yes & Yes & -- & \\\\",
    "Cell$\\times$Year FE   & No  & 3D  & 2D  & -- & \\\\",
    "CEM weights            & Yes & Yes & Yes & Yes & \\\\",
    "Mandate controls       & Yes & Yes & Yes & Yes & \\\\",
    "Sample & \\multicolumn{5}{c}{Reform-CEM matched} \\\\",
    "\\bottomrule",
    "\\end{tabular}",
    "\\begin{minipage}{\\linewidth}",
    "\\small",
    paste0(
      "\\textit{Notes:} LPM coefficients. Each row reports estimates under a ",
      "different window definition for classifying a leak report as ",
      "inspection-triggered or background. ",
      "The window is the number of days between a tank closure and a leak ",
      "report at the same facility. ",
      "Panel A leaks have no closure within the window; Panel B leaks have ",
      "a closure within the window. ",
      "The 60-day primary window is the main specification; ",
      "30-day, 45-day, and 90-day windows test sensitivity to the threshold. ",
      "3D Cell$\\times$Year FE: make\\_model\\_fac $\\times$ year. ",
      "2D Cell$\\times$Year FE: fac\\_wall$\\times$fac\\_fuel$\\times$year. ",
      "Col (4) reports the Borusyak, Jaravel, and Spiess (2024) two-stage ",
      "imputation estimator. ",
      "Bootstrap p-values for col (2) are from a wild cluster score bootstrap ",
      "(Kline-Santos 2012) with Rademacher weights, B = 9999. ",
      "Cluster-robust SEs by state (17 clusters). ",
      "$^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$."
    ),
    "\\end{minipage}",
    "\\end{table}"
  )
  write_tex(tex_lust_b, file.path(OUTPUT_TABLES, "T_LUST_B_Decomp.tex"))
}

# ---- B8.4: CSV export ----
lust_dt <- rbindlist(lapply(lust_b_data, function(w) {
  rbindlist(lapply(list(
    list(type="background",  mods=w$sa_mods,  cm=w$cm_sa,  var=w$sa_var),
    list(type="inspection",  mods=w$ind_mods, cm=w$cm_ind, var=w$ind_var)
  ), function(o) {
    rbindlist(lapply(names(o$mods), function(col) {
      d <- extract_did(o$mods[[col]])
      data.table(
        window   = w$window, type = o$type,
        outcome  = o$var,    col  = col,
        beta     = round(d$beta, 6L), se = round(d$se, 6L),
        p        = round(d$p, 4L),    stars = stars_p(d$p),
        n        = d$n, ctrl_mean = o$cm
      )
    }))
  }))
}))

lust_a_dt <- rbindlist(lapply(names(t_lust_a), function(col) {
  d <- extract_did(t_lust_a[[col]])
  data.table(window="total", type="total", outcome="leak_year", col=col,
             beta=round(d$beta,6L), se=round(d$se,6L),
             p=round(d$p,4L), stars=stars_p(d$p), n=d$n,
             ctrl_mean=cm_lust_total)
}))

fwrite(rbind(lust_a_dt, lust_dt),
       file.path(OUTPUT_TABLES, "T_LUST_All.csv"))
log_step("Wrote: T_LUST_A_Total.tex + T_LUST_B_Decomp.tex + T_LUST_All.csv")

# Global assignments
m_lust_total   <- t_lust_a$col2
m_lust_back    <- lust_b_data[[1L]]$sa_mods$col2
m_lust_inspect <- lust_b_data[[1L]]$ind_mods$col2
cat("\n")


################################################################################
# B9: EVENT STUDY FIGURES
################################################################################

cat("\n========================================\n")
cat("B9: EVENT STUDY FIGURES\n")
cat("========================================\n\n")

# ES sample: mm_fac_es (reform-CEM, verified non-NA make_model_fac).
# FE: panel_id + panel_year (FE_ES). Cell-year FE is collinear with the
# single-date event study indicators and cannot be included.
# Weights: CEM weights. Mandate controls: always included.
#
# Main paper: any_closure (primary parallel trends test)
# Appendix:   facility_exit, replacement_closure_year,
#             permanent_closure_year, leak_year, lust_standalone

es_specs <- list(
  list(stem   = "AnyClosure",
       depvar = "any_closure",
       ylab   = "Effect on Any Tank Closure Probability",
       main   = TRUE),
  list(stem   = "FacilityExit",
       depvar = "facility_exit",
       ylab   = "Effect on Facility Exit Probability (Closes Entirely)",
       main   = FALSE),
  list(stem   = "Upgrade",
       depvar = "replacement_closure_year",
       ylab   = "Effect on Tank Upgrade Probability (SW Out, DW In)",
       main   = FALSE),
  list(stem   = "Shrink",
       depvar = "permanent_closure_year",
       ylab   = "Effect on Tank Stock Reduction Probability (No Replacement)",
       main   = FALSE),
  list(stem   = "LUSTTotal",
       depvar = "leak_year",
       ylab   = "Effect on Total Leak Discovery Probability",
       main   = FALSE),
  list(stem   = "LUSTBackground",
       depvar = "lust_standalone",
       ylab   = paste0("Effect on Background Leak Discovery Probability",
                       " (No Closure in 60-Day Window)"),
       main   = FALSE)
)

build_es_fml <- function(depvar) {
  as.formula(sprintf(
    "%s ~ i(rel_year_bin, texas_treated, ref = -1) %s | %s",
    depvar, MANDATE_CONTROLS_RHS, FE_ES
  ))
}

cat("B9.1: Fitting ES models on reform-CEM sample...\n")

es_models <- lapply(es_specs, function(spec) {
  log_step(sprintf("  ES (reform-CEM): %s", spec$stem))
  feols(
    build_es_fml(spec$depvar),
    data    = mm_fac_es,
    weights = ~cem_weight,
    cluster = ~state
  )
})
names(es_models) <- sapply(es_specs, `[[`, "stem")

# Bootstrap pre-trend joint test for any_closure ES
# We bootstrap each ES coefficient separately and report the
# bootstrap p-value for the joint pre-period test as a supplement
# to the Wald test already computed in B9.2.
boot_es_pretrend <- lapply(
  grep("rel_year_bin", names(coef(es_models[["AnyClosure"]])), value = TRUE),
  function(param) {
    run_boot_ols_score(
      model = es_models[["AnyClosure"]],
      param = param,
      data  = mm_fac_es
    )
  }
)
names(boot_es_pretrend) <- grep(
  "rel_year_bin", names(coef(es_models[["AnyClosure"]])), value = TRUE
)
# Save bootstrap p-values to CSV alongside ES coefficients
boot_es_dt <- data.table(
  term    = names(boot_es_pretrend),
  p_crve  = sapply(boot_es_pretrend, function(b) {
    ct  <- coeftable(es_models[["AnyClosure"]])
    idx <- which(rownames(ct) == names(boot_es_pretrend)[1L])[1L]
    ct[idx, "Pr(>|t|)"]
  }),
  p_boot  = sapply(boot_es_pretrend, `[[`, "p_boot"),
  se_boot = sapply(boot_es_pretrend, `[[`, "se_boot"),
  ci_lo   = sapply(boot_es_pretrend, `[[`, "ci_lo"),
  ci_hi   = sapply(boot_es_pretrend, `[[`, "ci_hi")
)
fwrite(boot_es_dt,
       file.path(OUTPUT_TABLES, "ES_AnyClosure_BootstrapPvals.csv"))

# BJS imputation event study for any_closure
# did2s::event_study() implements the BJS ES directly.
es_bjs_any_closure <- tryCatch(
  did2s::event_study(
    data          = mm_fac_es,
    yname         = "any_closure",
    idname        = "panel_id",
    tname         = "panel_year",
    gname         = "panel_year",   # all treated in 1999
    estimator     = "did2s",
    cluster_var   = "state"
  ),
  error = function(e) {
    warning(sprintf("BJS event_study failed: %s", e$message))
    NULL
  }
)

# Save BJS ES coefficients if estimation succeeded
if (!is.null(es_bjs_any_closure)) {
  bjs_coefs <- as.data.table(es_bjs_any_closure)
  bjs_coefs[, spec := "BJS_imputation"]
  fwrite(bjs_coefs,
         file.path(OUTPUT_TABLES, "ES_AnyClosure_BJS.csv"))
  log_step("Wrote: ES_AnyClosure_BJS.csv")
}

# ---- Static DiD wild cluster bootstrap consolidation ----
# Bootstrap p-values were computed per-section alongside each primary fit
# (boot_t1_col5, boot_t2_*, boot_t2b_*, boot_t3, boot_lust_a, boot_sa/ind in
# lust_b_data). Consolidate them all into a single CSV for cross-table reference.
log_step("Consolidating wild cluster bootstrap results across all tables...")
boot_static <- list(
  T1_AnyClosure   = boot_t1_col5,
  T2_FacExit      = boot_t2_exit,
  T2_Upgrade      = boot_t2_upgrade,
  T2_Shrink       = boot_t2_shrink,
  T2b_FacExit     = boot_t2b_exit,
  T2b_Upgrade     = boot_t2b_upgrade,
  T2b_Shrink      = boot_t2b_shrink,
  LUSTA_LeakYear  = boot_lust_a
)
for (nm in names(t3_models)) {
  boot_static[[paste0("T3_", nm)]] <- boot_t3[[nm]]
}
for (w in lust_b_data) {
  win_nm <- gsub("[^A-Za-z0-9]", "", w$window)
  boot_static[[paste0("LUSTB_SA_",  win_nm)]] <- w$boot_sa
  boot_static[[paste0("LUSTB_IND_", win_nm)]] <- w$boot_ind
}
boot_static_dt <- rbindlist(lapply(names(boot_static), function(nm) {
  b <- boot_static[[nm]]
  data.table(spec = nm, p_boot = b$p_boot, se_boot = b$se_boot,
             ci_lo = b$ci_lo, ci_hi = b$ci_hi, t_stat = b$t_stat)
}))
fwrite(boot_static_dt,
       file.path(OUTPUT_TABLES, "StaticDiD_BootstrapPvals.csv"))
log_step(sprintf("Wrote: StaticDiD_BootstrapPvals.csv (%d specs)",
                 nrow(boot_static_dt)))

# ---- B9.2: Pre-trend joint Wald tests ----
cat("B9.2: Pre-trend joint Wald tests...\n")

pretrend_dt <- rbindlist(lapply(es_specs, function(spec)
  run_pretrend_test(es_models[[spec$stem]], spec$stem, "reform_cem")))

log_step("Pre-trend F-tests (tau <= -2, ref = -1):")
for (i in seq_len(nrow(pretrend_dt))) {
  r <- pretrend_dt[i]
  log_step(sprintf("  %-22s  F = %6.3f  p = %.4f  (df = %d)",
    r$label,
    ifelse(is.na(r$F_stat), NA_real_, r$F_stat),
    ifelse(is.na(r$p_val),  NA_real_, r$p_val),
    r$n_pre_coefs), 1L)
}
fwrite(pretrend_dt, file.path(OUTPUT_TABLES, "ES_PreTrendTests.csv"))
log_step("Wrote: ES_PreTrendTests.csv")

# ---- B9.3: Save figures ----
cat("B9.3: Saving ES figures...\n")

for (spec in es_specs) {
  ct  <- es_tidy(es_models[[spec$stem]])
  fig <- es_ggplot(ct, ylab = spec$ylab, filename = NULL)$plot
  tag <- if (spec$main) "MainPaper" else "Appendix"
  save_gg(fig,
          file.path(OUTPUT_FIGURES,
                    sprintf("Figure_ES_%s_%s", spec$stem, tag)),
          width = 10, height = 5.5)
  log_step(sprintf("  Saved: Figure_ES_%s_%s", spec$stem, tag))
}

# ---- B9.4: Coefficient CSV ----
es_coefs_dt <- rbindlist(lapply(es_specs, function(spec) {
  ct <- es_tidy(es_models[[spec$stem]])
  ct[, `:=`(stem = spec$stem, depvar = spec$depvar,
             main_paper = spec$main, sample = "reform_cem")]
  ct
}), fill = TRUE)
fwrite(es_coefs_dt, file.path(OUTPUT_TABLES, "ES_Coefficients.csv"))
log_step(sprintf("Wrote: ES_Coefficients.csv (%s rows)", fmt_n(nrow(es_coefs_dt))))

# Global assignments for B10
m_es_any_closure <- es_models[["AnyClosure"]]
m_es_exit        <- es_models[["FacilityExit"]]
m_es_upgrade     <- es_models[["Upgrade"]]
m_es_lust_back   <- es_models[["LUSTBackground"]]
cat("\n")


################################################################################
# B10: HONESTDID SENSITIVITY
################################################################################

cat("\n========================================\n")
cat("B10: HONESTDID SENSITIVITY\n")
cat("========================================\n\n")

cat("B10.1: Defining HonestDiD input extraction...\n")

extract_hd_inputs <- function(m, ref = -1L) {
  b_full <- coef(m)
  V_full <- vcov(m, type = "clustered")

  es_pat <- "^rel_year_bin::(-?[0-9]+):texas_treated$"
  es_idx <- grep(es_pat, names(b_full))
  stopifnot("No rel_year_bin i() terms found" = length(es_idx) > 0L)

  b_es  <- b_full[es_idx]
  V_es  <- V_full[es_idx, es_idx, drop = FALSE]
  years <- as.integer(sub(es_pat, "\\1", names(b_es)))

  ord   <- order(years)
  b_es  <- b_es[ord]
  V_es  <- V_es[ord, ord, drop = FALSE]
  years <- years[ord]

  stopifnot("ref year should not be in b_es" = !ref %in% years)

  pre_idx  <- which(years < ref)
  post_idx <- which(years > ref)
  stopifnot("No pre-period coefficients"  = length(pre_idx)  > 0L)
  stopifnot("No post-period coefficients" = length(post_idx) > 0L)

  all_idx   <- c(pre_idx, post_idx)
  betahat   <- b_es[all_idx]
  sigma_raw <- V_es[all_idx, all_idx, drop = FALSE]

  ev        <- eigen(sigma_raw, symmetric = TRUE)
  ev$values <- pmax(ev$values, 0)
  sigma_psd <- ev$vectors %*% diag(ev$values) %*% t(ev$vectors)

  max_pre_slope <- if (length(pre_idx) >= 2L)
    max(abs(diff(b_es[pre_idx]))) else NA_real_

  list(
    betahat        = betahat,
    sigma          = sigma_psd,
    beta_pre       = b_es[pre_idx],
    beta_post      = b_es[post_idx],
    pre_years      = years[pre_idx],
    post_years     = years[post_idx],
    numPrePeriods  = length(pre_idx),
    numPostPeriods = length(post_idx),
    max_pre_slope  = max_pre_slope
  )
}

cat("B10.2: Running HonestDiD for three outcomes...\n")

HD_M_GRID_RM <- seq(0, 2,     by = 0.1)
HD_M_GRID_SD <- seq(0, 0.005, by = 0.0001)

hd_targets <- list(
  list(stem  = "AnyClosure",
       label = "Any Tank Closure",
       model = m_es_any_closure,
       col   = COL_TX),
  list(stem  = "FacilityExit",
       label = "Facility Exit",
       model = m_es_exit,
       col   = COL_CTRL),
  list(stem  = "LUSTBackground",
       label = "Background Leak Discovery",
       model = m_es_lust_back,
       col   = "#009E73")
)

make_l_vec <- function(n_post) rep(1 / n_post, n_post)

hd_results <- list()

for (tgt in hd_targets) {
  log_step(sprintf("HonestDiD: %s", tgt$stem))
  inp   <- extract_hd_inputs(tgt$model)
  l_vec <- make_l_vec(inp$numPostPeriods)

  point_avg <- as.numeric(l_vec %*% inp$beta_post)
  post_V    <- inp$sigma[
    (inp$numPrePeriods + 1L):(inp$numPrePeriods + inp$numPostPeriods),
    (inp$numPrePeriods + 1L):(inp$numPrePeriods + inp$numPostPeriods),
    drop = FALSE]
  se_avg    <- sqrt(as.numeric(t(l_vec) %*% post_V %*% l_vec))
  ci_lo_avg <- point_avg - 1.96 * se_avg
  ci_hi_avg <- point_avg + 1.96 * se_avg

  log_step(sprintf(
    "  Avg post ATT: %+.5f  SE=%.5f  95%%CI=[%+.5f, %+.5f]",
    point_avg, se_avg, ci_lo_avg, ci_hi_avg), 1L)

  sens_rm <- tryCatch(
    HonestDiD::createSensitivityResults_relativeMagnitudes(
      betahat        = inp$betahat,
      sigma          = inp$sigma,
      numPrePeriods  = inp$numPrePeriods,
      numPostPeriods = inp$numPostPeriods,
      l_vec          = l_vec,
      Mbarvec        = HD_M_GRID_RM,
      alpha          = 0.05
    ),
    error = function(e) { warning(e$message); NULL }
  )

  sens_sd <- tryCatch(
    HonestDiD::createSensitivityResults(
      betahat        = inp$betahat,
      sigma          = inp$sigma,
      numPrePeriods  = inp$numPrePeriods,
      numPostPeriods = inp$numPostPeriods,
      l_vec          = l_vec,
      Mvec           = HD_M_GRID_SD,
      alpha          = 0.05
    ),
    error = function(e) { warning(e$message); NULL }
  )

  bd_rm <- if (!is.null(sens_rm)) {
    rm_dt <- as.data.table(sens_rm)
    cross  <- rm_dt[lb <= 0 & ub >= 0, Mbar]
    if (length(cross) > 0L) min(cross) else NA_real_
  } else NA_real_

  bd_sd <- if (!is.null(sens_sd)) {
    sd_dt <- as.data.table(sens_sd)
    cross  <- sd_dt[lb <= 0 & ub >= 0, M]
    if (length(cross) > 0L) min(cross) else NA_real_
  } else NA_real_

  log_step(sprintf(
    "  Breakdown -- RM: %s | SD: %s",
    ifelse(is.na(bd_rm), "robust at all M", sprintf("%.2f",  bd_rm)),
    ifelse(is.na(bd_sd), "robust at all M", sprintf("%.5f",  bd_sd))), 1L)

  hd_results[[tgt$stem]] <- list(
    stem      = tgt$stem,  label     = tgt$label, col = tgt$col,
    inp       = inp,
    point_avg = point_avg, se_avg    = se_avg,
    ci_lo_avg = ci_lo_avg, ci_hi_avg = ci_hi_avg,
    sens_rm   = sens_rm,   sens_sd   = sens_sd,
    bd_rm     = bd_rm,     bd_sd     = bd_sd
  )
}

# ---- B10.3: Sensitivity figures ----
cat("B10.3: Saving HonestDiD figures...\n")

plot_hd_sensitivity <- function(sens_dt, M_col, bd_M, obs_slope,
                                ci_col, x_label, y_label,
                                orig_ci_lo, orig_ci_hi) {
  dt <- as.data.table(sens_dt)
  setnames(dt, M_col, "M")
  dt <- dt[!is.na(lb) & !is.na(ub)]
  if (nrow(dt) == 0L) return(NULL)

  y_lo  <- min(c(dt$lb, orig_ci_lo), na.rm = TRUE)
  y_hi  <- max(c(dt$ub, orig_ci_hi), na.rm = TRUE)
  y_pad <- diff(range(c(y_lo, y_hi))) * 0.12

  p <- ggplot(dt, aes(x = M)) +
    geom_hline(yintercept = 0, linetype = "dashed",
               color = "grey50", linewidth = 0.5) +
    geom_ribbon(aes(ymin = lb, ymax = ub),
                fill = ci_col, alpha = 0.20) +
    geom_line(aes(y = lb), color = ci_col, linewidth = 0.75) +
    geom_line(aes(y = ub), color = ci_col, linewidth = 0.75) +
    geom_point(data = data.table(M = 0, y = orig_ci_lo),
               aes(x = M, y = y), color = "black", size = 2.0) +
    geom_point(data = data.table(M = 0, y = orig_ci_hi),
               aes(x = M, y = y), color = "black", size = 2.0)

  if (is.finite(obs_slope) && obs_slope > 0) {
    p <- p +
      geom_vline(xintercept = obs_slope, linetype = "dashed",
                 color = "steelblue", linewidth = 0.55) +
      annotate("text",
               x = obs_slope * 1.04, y = y_hi - y_pad * 0.15,
               label = sprintf("Max pre-trend\n= %.4f", obs_slope),
               hjust = 0, size = 2.8, color = "steelblue", lineheight = 0.85)
  }
  if (is.finite(bd_M)) {
    p <- p +
      geom_vline(xintercept = bd_M, linetype = "dotted",
                 color = "grey30", linewidth = 0.6) +
      annotate("text",
               x = bd_M * 1.04, y = y_lo + y_pad * 0.4,
               label = sprintf("Breakdown M\n= %.4f", bd_M),
               hjust = 0, size = 2.8, color = "grey30", lineheight = 0.85)
  } else {
    p <- p +
      annotate("text",
               x = max(dt$M) * 0.65, y = y_lo + y_pad * 0.4,
               label = "CI excludes zero\nat all tested M",
               hjust = 0, size = 2.8, color = "grey30")
  }

  p +
    scale_x_continuous(labels = scales::number_format(accuracy = 0.001)) +
    scale_y_continuous(labels = scales::number_format(accuracy = 0.0001)) +
    coord_cartesian(ylim = c(y_lo - y_pad, y_hi + y_pad)) +
    labs(x = x_label, y = y_label) +
    theme_pub()
}

for (nm in names(hd_results)) {
  res <- hd_results[[nm]]
  if (!is.null(res$sens_rm)) {
    p_rm <- plot_hd_sensitivity(
      res$sens_rm, "Mbar", res$bd_rm, res$inp$max_pre_slope,
      res$col,
      "Mbar: Max PT Violation as Multiple of Pre-Trend",
      "Robust 95% CI for Average Post-Reform ATT",
      res$ci_lo_avg, res$ci_hi_avg
    )
    if (!is.null(p_rm)) {
      save_gg(p_rm,
              file.path(OUTPUT_FIGURES, sprintf("Figure_HonestDiD_%s_RM", nm)),
              width = 8, height = 5)
      log_step(sprintf("  Saved: Figure_HonestDiD_%s_RM", nm))
    }
  }
  if (!is.null(res$sens_sd)) {
    p_sd <- plot_hd_sensitivity(
      res$sens_sd, "M", res$bd_sd, res$inp$max_pre_slope,
      res$col,
      "M: Max Second-Difference of PT Violation (pp/year)",
      "Robust 95% CI for Average Post-Reform ATT",
      res$ci_lo_avg, res$ci_hi_avg
    )
    if (!is.null(p_sd)) {
      save_gg(p_sd,
              file.path(OUTPUT_FIGURES, sprintf("Figure_HonestDiD_%s_SD", nm)),
              width = 8, height = 5)
      log_step(sprintf("  Saved: Figure_HonestDiD_%s_SD", nm))
    }
  }
}

# ---- B10.4: Export ----
hd_summary_dt <- rbindlist(lapply(hd_results, function(res) {
  data.table(
    outcome       = res$label,
    stem          = res$stem,
    point_avg     = round(res$point_avg, 6L),
    se_avg        = round(res$se_avg,    6L),
    ci_lo_orig    = round(res$ci_lo_avg, 6L),
    ci_hi_orig    = round(res$ci_hi_avg, 6L),
    max_pre_slope = round(res$inp$max_pre_slope, 6L),
    breakdown_rm  = round(res$bd_rm, 3L),
    breakdown_sd  = round(res$bd_sd, 6L),
    n_pre         = res$inp$numPrePeriods,
    n_post        = res$inp$numPostPeriods
  )
}))
fwrite(hd_summary_dt, file.path(OUTPUT_TABLES, "T_HonestDiD_Summary.csv"))

fmt_break <- function(x, fmt) {
  if (is.na(x)) "$\\geq M_{\\max}$" else sprintf(fmt, x)
}
hd_tex_rows <- vapply(seq_len(nrow(hd_summary_dt)), function(i) {
  r <- hd_summary_dt[i]
  sprintf(
    "%s & %+.5f & [%+.5f, %+.5f] & %.4f & %s & %s \\\\",
    r$outcome, r$point_avg, r$ci_lo_orig, r$ci_hi_orig,
    r$max_pre_slope,
    fmt_break(r$breakdown_rm, "%.2f"),
    fmt_break(r$breakdown_sd, "%.5f")
  )
}, character(1))

tex_hd <- c(
  "\\begin{table}[htbp]",
  "\\centering",
  "\\caption{HonestDiD Sensitivity (Reform-CEM ES, Primary Sample)}",
  "\\label{tab:honestdid_fac}",
  "\\begin{tabular}{l c c c cc}",
  "\\toprule",
  paste0("Outcome & Avg post ATT & 95\\% CI",
         " & Max pre-trend & Breakdown $\\bar{M}$ ($\\Delta^{RM}$)",
         " & Breakdown $M$ ($\\Delta^{SD}$) \\\\"),
  "\\midrule",
  hd_tex_rows,
  "\\bottomrule",
  "\\end{tabular}",
  "\\begin{minipage}{\\linewidth}",
  "\\small",
  paste0(
    "\\textit{Notes:} Rambachan and Roth (2023) sensitivity analysis. ",
    "Estimand is the equal-weighted average of post-reform ATT coefficients ",
    "($\\tau \\geq 0$). ES estimated on reform-CEM sample. ",
    "Breakdown $\\bar{M}$: smallest restriction under which robust CI includes zero ",
    "($\\Delta^{RM}$ class). ",
    "Breakdown $M$: smallest slope-smoothness restriction under which CI includes zero ",
    "($\\Delta^{SD}$ class). ",
    "Values shown as $\\geq M_{\\max}$ indicate CI excludes zero at all tested levels."
  ),
  "\\end{minipage}",
  "\\end{table}"
)
write_tex(tex_hd, file.path(OUTPUT_TABLES, "T_HonestDiD_Summary.tex"))
log_step("Wrote: T_HonestDiD_Summary.csv + T_HonestDiD_Summary.tex")
cat("\n")


################################################################################
# B11: HTE BY WALL TYPE
################################################################################

cat("\n========================================\n")
cat("B11: HTE BY WALL TYPE\n")
cat("========================================\n\n")

cat("B11.1: Preparing wall-type HTE samples...\n")

expected_walls <- c("All-SW", "Mixed", "All-DW")

prep_wall_sample <- function(dt) {
  sub <- dt[fac_wall_reform %in% expected_walls]
  sub[, fac_wall_f := factor(fac_wall_reform,
                              levels = c("All-DW", "Mixed", "All-SW"))]
  sub[]
}

mm_wall <- prep_wall_sample(mm_fac_reform)

log_step(sprintf("Wall HTE sample: %s fy  (All-DW=%s | Mixed=%s | All-SW=%s)",
  fmt_n(nrow(mm_wall)),
  fmt_n(mm_wall[fac_wall_f == "All-DW", .N]),
  fmt_n(mm_wall[fac_wall_f == "Mixed",  .N]),
  fmt_n(mm_wall[fac_wall_f == "All-SW", .N])))

cat("B11.2: Fitting wall-type HTE models...\n")

wall_outcomes <- c(
  any_closure  = "any_closure",
  exit         = "facility_exit",
  upgrade      = "replacement_closure_year",
  shrink       = "permanent_closure_year",
  lust_total   = "leak_year",
  lust_back    = "lust_standalone"
)

build_wall_fml <- function(depvar) {
  as.formula(sprintf(
    "%s ~ did_term + did_term:fac_wall_f %s | %s",
    depvar, MANDATE_CONTROLS_RHS, FE_PRIMARY
  ))
}

wall_models <- lapply(wall_outcomes, function(y) {
  feols(build_wall_fml(y), data = mm_wall,
        weights = ~cem_weight, cluster = ~state)
})
names(wall_models) <- names(wall_outcomes)

cat("B11.3: Extracting wall HTE ATTs...\n")

extract_wall_atts <- function(m, outcome_label) {
  b <- coef(m)
  V <- vcov(m, type = "clustered")
  did_idx <- which(names(b) == "did_term")
  stopifnot("did_term not found" = length(did_idx) == 1L)

  rbindlist(lapply(c("All-DW", "Mixed", "All-SW"), function(wl) {
    if (wl == "All-DW") {
      att <- b[did_idx]; se <- sqrt(V[did_idx, did_idx])
      p   <- 2 * pnorm(-abs(att / se))
      return(data.table(outcome=outcome_label, wall=wl, is_ref=TRUE,
                        est=round(att,6L), se=round(se,6L),
                        ci_lo=round(att-1.96*se,6L), ci_hi=round(att+1.96*se,6L),
                        p=round(p,4L), stars=stars_p(p)))
    }
    int_nm  <- paste0("did_term:fac_wall_f", wl)
    int_idx <- which(names(b) == int_nm)
    if (length(int_idx) == 0L) {
      int_nm  <- paste0("fac_wall_f", wl, ":did_term")
      int_idx <- which(names(b) == int_nm)
    }
    if (length(int_idx) == 0L) {
      warning(sprintf("Interaction for %s not found in %s", wl, outcome_label))
      return(data.table(outcome=outcome_label, wall=wl, is_ref=FALSE,
                        est=NA_real_, se=NA_real_,
                        ci_lo=NA_real_, ci_hi=NA_real_,
                        p=NA_real_, stars=""))
    }
    att     <- b[did_idx] + b[int_idx]
    var_att <- V[did_idx,did_idx] + V[int_idx,int_idx] + 2*V[did_idx,int_idx]
    se      <- sqrt(max(var_att, 0))
    p       <- 2 * pnorm(-abs(att / se))
    data.table(outcome=outcome_label, wall=wl, is_ref=FALSE,
               est=round(att,6L), se=round(se,6L),
               ci_lo=round(att-1.96*se,6L), ci_hi=round(att+1.96*se,6L),
               p=round(p,4L), stars=stars_p(p))
  }))
}

outcome_labels_wall <- c(
  any_closure = "Any Tank Closure",
  exit        = "Facility Exit",
  upgrade     = "Tank Upgrade",
  shrink      = "Tank Stock Reduction",
  lust_total  = "Total Leak Discovery",
  lust_back   = "Background Leak Discovery"
)

wall_atts <- rbindlist(lapply(names(wall_models), function(nm)
  extract_wall_atts(wall_models[[nm]], outcome_labels_wall[[nm]])))

log_step("Wall HTE ATTs (primary spec):")
for (nm in names(outcome_labels_wall)) {
  sub <- wall_atts[outcome == outcome_labels_wall[[nm]]]
  log_step(sprintf("  %s:", outcome_labels_wall[[nm]]))
  for (i in seq_len(nrow(sub))) {
    r <- sub[i]
    log_step(sprintf("    %-8s: %+.4f (%.4f) p=%.3f",
      r$wall, r$est, r$se, r$p), 1L)
  }
}

# Forest plot
wall_atts[, wall := factor(wall, levels = c("All-DW", "Mixed", "All-SW"))]
wall_atts[, outcome_f := factor(outcome, levels = rev(unname(outcome_labels_wall)))]

wall_colors <- c("All-DW" = COL_CTRL, "Mixed" = "#E69F00", "All-SW" = COL_TX)

p_wall <- ggplot(
  wall_atts[!is.na(est)],
  aes(x = est, y = outcome_f,
      xmin = ci_lo, xmax = ci_hi,
      color = wall, group = wall)
) +
  geom_vline(xintercept = 0, linetype = "dashed",
             color = "grey50", linewidth = 0.5) +
  geom_pointrange(
    position = position_dodge(width = 0.55),
    size = 0.35, linewidth = 0.5
  ) +
  scale_color_manual(values = wall_colors, name = "Wall type") +
  labs(
    x = "ATT (Texas x Post), LPM coefficient",
    y = NULL
  ) +
  theme_pub() +
  theme(legend.position = "bottom", axis.text.y = element_text(size = 10))

save_gg(p_wall,
        file.path(OUTPUT_FIGURES, "Figure_HTE_WallType"),
        width = 9, height = 5.5)
log_step("Saved: Figure_HTE_WallType.png/.pdf")

fwrite(wall_atts, file.path(OUTPUT_TABLES, "T_HTE_WallType.csv"))
log_step("Wrote: T_HTE_WallType.csv")
cat("\n")


################################################################################
# B12: HTE BY OLDEST-TANK AGE
################################################################################

cat("\n========================================\n")
cat("B12: HTE BY OLDEST-TANK AGE\n")
cat("========================================\n\n")

cat("B12.1: Preparing age HTE samples...\n")

AGE_REF <- "0-4yr"
stopifnot("AGE_REF must be in AGE_BIN_LEVELS" = AGE_REF %in% AGE_BIN_LEVELS)

prep_age_sample <- function(dt) {
  sub <- dt[!is.na(oldest_age_bin)]
  sub[, age_bin_f := factor(oldest_age_bin, levels = AGE_BIN_LEVELS)]
  sub[, age_bin_f := relevel(age_bin_f, ref = AGE_REF)]
  sub[]
}

mm_age <- prep_age_sample(mm_fac_reform)

bin_counts <- mm_age[, .(
  n_fy = .N,
  n_tx  = sum(texas_treated == 1L),
  n_ctl = sum(texas_treated == 0L)
), by = age_bin_f][order(age_bin_f)]
log_step("Age bin counts (reform-CEM):")
print(bin_counts)

cat("B12.2: Fitting age HTE models...\n")

age_outcomes <- c(
  any_closure  = "any_closure",
  exit         = "facility_exit",
  upgrade      = "replacement_closure_year",
  lust_back    = "lust_standalone"
)

build_age_fml <- function(depvar) {
  as.formula(sprintf(
    "%s ~ did_term + did_term:age_bin_f %s | %s",
    depvar, MANDATE_CONTROLS_RHS, FE_PRIMARY
  ))
}

age_models <- lapply(age_outcomes, function(y)
  feols(build_age_fml(y), data = mm_age,
        weights = ~cem_weight, cluster = ~state))
names(age_models) <- names(age_outcomes)

cat("B12.3: Extracting age-bin ATTs...\n")

extract_age_atts <- function(m, outcome_label) {
  b <- coef(m)
  V <- vcov(m, type = "clustered")
  did_idx <- which(names(b) == "did_term")
  stopifnot("did_term not found" = length(did_idx) == 1L)

  rbindlist(lapply(AGE_BIN_LEVELS, function(bin) {
    if (bin == AGE_REF) {
      att <- b[did_idx]; se <- sqrt(V[did_idx, did_idx])
      p   <- 2 * pnorm(-abs(att / se))
      return(data.table(outcome=outcome_label, age_bin=bin, is_ref=TRUE,
                        est=round(att,6L), se=round(se,6L),
                        ci_lo=round(att-1.96*se,6L), ci_hi=round(att+1.96*se,6L),
                        p=round(p,4L), stars=stars_p(p)))
    }
    int_nm  <- paste0("did_term:age_bin_f", bin)
    int_idx <- which(names(b) == int_nm)
    if (length(int_idx) == 0L) {
      int_nm  <- paste0("age_bin_f", bin, ":did_term")
      int_idx <- which(names(b) == int_nm)
    }
    if (length(int_idx) == 0L) {
      warning(sprintf("Age interaction for bin %s not found", bin))
      return(data.table(outcome=outcome_label, age_bin=bin, is_ref=FALSE,
                        est=NA_real_, se=NA_real_,
                        ci_lo=NA_real_, ci_hi=NA_real_,
                        p=NA_real_, stars=""))
    }
    att     <- b[did_idx] + b[int_idx]
    var_att <- V[did_idx,did_idx] + V[int_idx,int_idx] + 2*V[did_idx,int_idx]
    se      <- sqrt(max(var_att, 0))
    p       <- 2 * pnorm(-abs(att / se))
    data.table(outcome=outcome_label, age_bin=bin, is_ref=FALSE,
               est=round(att,6L), se=round(se,6L),
               ci_lo=round(att-1.96*se,6L), ci_hi=round(att+1.96*se,6L),
               p=round(p,4L), stars=stars_p(p))
  }))
}

outcome_labels_age <- c(
  any_closure  = "Any Tank Closure",
  exit         = "Facility Exit",
  upgrade      = "Tank Upgrade",
  lust_back    = "Background Leak Discovery"
)

age_atts <- rbindlist(lapply(names(age_models), function(nm)
  extract_age_atts(age_models[[nm]], outcome_labels_age[[nm]])))
age_atts[, age_bin := factor(age_bin, levels = AGE_BIN_LEVELS)]

# Age-gradient line figure
age_colors <- c(
  "Any Tank Closure"        = COL_TX,
  "Facility Exit"           = COL_CTRL,
  "Tank Upgrade"            = "#E69F00",
  "Background Leak Discovery" = "#009E73"
)

p_age <- ggplot(
  age_atts[!is.na(est)],
  aes(x = age_bin, y = est, color = outcome, group = outcome)
) +
  geom_hline(yintercept = 0, linetype = "dashed",
             color = "grey50", linewidth = 0.5) +
  geom_ribbon(aes(ymin = ci_lo, ymax = ci_hi, fill = outcome),
              alpha = 0.10, color = NA) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2.8, shape = 16) +
  geom_errorbar(aes(ymin = ci_lo, ymax = ci_hi),
                width = 0.18, linewidth = 0.45) +
  scale_color_manual(values = age_colors, name = NULL) +
  scale_fill_manual(values  = age_colors, name = NULL, guide = "none") +
  scale_x_discrete(drop = FALSE) +
  labs(
    x = "Age of Oldest Tank at Reform Date",
    y = "ATT (Texas x Post), LPM coefficient"
  ) +
  theme_pub() +
  theme(legend.position = "bottom",
        axis.text.x     = element_text(angle = 30, hjust = 1))

save_gg(p_age,
        file.path(OUTPUT_FIGURES, "Figure_HTE_OldestAge"),
        width = 9, height = 5.5)
log_step("Saved: Figure_HTE_OldestAge.png/.pdf")

age_atts_out <- merge(age_atts,
  bin_counts[, .(age_bin = age_bin_f, n_fy, n_tx, n_ctl)],
  by = "age_bin", all.x = TRUE)
setorder(age_atts_out, outcome, age_bin)
fwrite(age_atts_out, file.path(OUTPUT_TABLES, "T_HTE_OldestAge.csv"))
log_step("Wrote: T_HTE_OldestAge.csv")
cat("\n")


################################################################################
# B12B: HTE BY TANK VINTAGE / MANDATE EXPOSURE
################################################################################
#
# Tests whether the reform's effect varies with the federal mandate exposure
# of the facility's tanks. The relevant cut is install year 1990:
#
#   Pre-1989 vintage (had_pre1989_tanks == 1): facility had at least one tank
#     installed in 1988 or earlier. These tanks are subject to the federal
#     UST corrective-action mandates: release detection (deadlines 1989-1993),
#     spill/overfill prevention (Dec 1994), and tank integrity (Dec 1998).
#     These facilities faced concurrent regulatory pressure during the
#     reform window and may have had stronger latent incentives to act.
#
#   All-post-1989 vintage (all_post1989 == 1): every tank at the facility
#     was installed in 1990 or later. These tanks were "new" under federal
#     rules and are exempt from the retrofit mandates above. Their reform
#     response isolates the insurance-channel effect from the mandate channel.
#
# A differential treatment effect by vintage indicates that mandate exposure
# either amplifies or substitutes for the insurance reform's behavioral push.

cat("\n========================================\n")
cat("B12B: HTE BY TANK VINTAGE / MANDATE EXPOSURE\n")
cat("========================================\n\n")

cat("B12B.1: Preparing vintage HTE samples...\n")

# Construct vintage group factor on reform-CEM sample
mm_vint <- copy(mm_fac_reform)
mm_vint[, vintage_group := fcase(
  had_pre1989_tanks == 1L,                       "Pre-1989",
  all_post1989      == 1L,                        "All-Post-1989",
  default = NA_character_
)]
mm_vint <- mm_vint[!is.na(vintage_group)]
mm_vint[, vintage_group := factor(vintage_group,
                                   levels = c("Pre-1989", "All-Post-1989"))]

n_pre  <- uniqueN(mm_vint[vintage_group == "Pre-1989",      panel_id])
n_post <- uniqueN(mm_vint[vintage_group == "All-Post-1989", panel_id])
log_step(sprintf("Vintage HTE sample: %s fy | %s pre-1989 facs | %s all-post-1989 facs",
  fmt_n(nrow(mm_vint)), fmt_n(n_pre), fmt_n(n_post)))

cat("B12B.2: Fitting vintage HTE models...\n")

vintage_outcomes <- c(
  any_closure = "any_closure",
  exit        = "facility_exit",
  upgrade     = "replacement_closure_year",
  shrink      = "permanent_closure_year",
  lust_total  = "leak_year",
  lust_back   = "lust_standalone"
)

build_vintage_fml <- function(depvar) {
  as.formula(sprintf(
    "%s ~ did_term + did_term:vintage_group %s | %s",
    depvar, MANDATE_CONTROLS_RHS, FE_PRIMARY
  ))
}

vintage_models <- lapply(vintage_outcomes, function(y) {
  feols(build_vintage_fml(y), data = mm_vint,
        weights = ~cem_weight, cluster = ~state)
})
names(vintage_models) <- names(vintage_outcomes)

cat("B12B.3: Extracting vintage HTE ATTs...\n")

# Reference level is "Pre-1989" (the mandate-exposed group).
# did_term            = ATT for Pre-1989
# did_term + interact = ATT for All-Post-1989 (mandate-exempt)
# interact term itself = differential effect (test of HTE)
extract_vintage_atts <- function(m, outcome_label) {
  b <- coef(m)
  V <- vcov(m, type = "clustered")
  did_idx <- which(names(b) == "did_term")
  stopifnot("did_term not found" = length(did_idx) == 1L)

  # Reference: Pre-1989
  att_pre <- b[did_idx]
  se_pre  <- sqrt(V[did_idx, did_idx])
  p_pre   <- 2 * pnorm(-abs(att_pre / se_pre))

  # All-Post-1989: did_term + interaction
  int_nm  <- "did_term:vintage_groupAll-Post-1989"
  int_idx <- which(names(b) == int_nm)
  if (length(int_idx) == 0L) {
    int_nm  <- "vintage_groupAll-Post-1989:did_term"
    int_idx <- which(names(b) == int_nm)
  }
  if (length(int_idx) == 0L) {
    warning(sprintf("Interaction term not found in %s", outcome_label))
    return(rbindlist(list(
      data.table(outcome=outcome_label, vintage="Pre-1989",      is_ref=TRUE,
                 est=round(att_pre,6L), se=round(se_pre,6L),
                 ci_lo=round(att_pre-1.96*se_pre,6L),
                 ci_hi=round(att_pre+1.96*se_pre,6L),
                 p=round(p_pre,4L), stars=stars_p(p_pre),
                 diff_est=NA_real_, diff_se=NA_real_, diff_p=NA_real_),
      data.table(outcome=outcome_label, vintage="All-Post-1989", is_ref=FALSE,
                 est=NA_real_, se=NA_real_, ci_lo=NA_real_, ci_hi=NA_real_,
                 p=NA_real_, stars="", diff_est=NA_real_,
                 diff_se=NA_real_, diff_p=NA_real_)
    )))
  }

  # Differential effect (mandate-exempt minus mandate-pressured)
  diff_est <- b[int_idx]
  diff_se  <- sqrt(V[int_idx, int_idx])
  diff_p   <- 2 * pnorm(-abs(diff_est / diff_se))

  # All-Post-1989 ATT level
  att_post <- att_pre + b[int_idx]
  var_post <- V[did_idx, did_idx] + V[int_idx, int_idx] +
                2 * V[did_idx, int_idx]
  se_post  <- sqrt(max(var_post, 0))
  p_post   <- 2 * pnorm(-abs(att_post / se_post))

  rbindlist(list(
    data.table(outcome=outcome_label, vintage="Pre-1989", is_ref=TRUE,
               est=round(att_pre,6L), se=round(se_pre,6L),
               ci_lo=round(att_pre-1.96*se_pre,6L),
               ci_hi=round(att_pre+1.96*se_pre,6L),
               p=round(p_pre,4L), stars=stars_p(p_pre),
               diff_est=round(diff_est,6L),
               diff_se =round(diff_se,6L),
               diff_p  =round(diff_p,4L)),
    data.table(outcome=outcome_label, vintage="All-Post-1989", is_ref=FALSE,
               est=round(att_post,6L), se=round(se_post,6L),
               ci_lo=round(att_post-1.96*se_post,6L),
               ci_hi=round(att_post+1.96*se_post,6L),
               p=round(p_post,4L), stars=stars_p(p_post),
               diff_est=round(diff_est,6L),
               diff_se =round(diff_se,6L),
               diff_p  =round(diff_p,4L))
  ))
}

outcome_labels_vintage <- c(
  any_closure = "Any Tank Closure",
  exit        = "Facility Exit",
  upgrade     = "Tank Upgrade",
  shrink      = "Tank Stock Reduction",
  lust_total  = "Total Leak Discovery",
  lust_back   = "Background Leak Discovery"
)

vintage_atts <- rbindlist(lapply(names(vintage_models), function(nm)
  extract_vintage_atts(vintage_models[[nm]], outcome_labels_vintage[[nm]])))

log_step("Vintage HTE ATTs (primary spec):")
for (nm in names(outcome_labels_vintage)) {
  sub <- vintage_atts[outcome == outcome_labels_vintage[[nm]]]
  log_step(sprintf("  %s:  diff (post1989 - pre1989) = %+.4f (%.4f) p=%.3f",
    outcome_labels_vintage[[nm]],
    sub$diff_est[1L], sub$diff_se[1L], sub$diff_p[1L]))
  for (i in seq_len(nrow(sub))) {
    r <- sub[i]
    log_step(sprintf("    %-14s: %+.4f (%.4f) p=%.3f",
      r$vintage, r$est, r$se, r$p), 1L)
  }
}

# B12B.4: Forest plot
vintage_atts[, vintage := factor(vintage,
                                  levels = c("Pre-1989", "All-Post-1989"))]
vintage_atts[, outcome_f := factor(outcome,
                                    levels = rev(unname(outcome_labels_vintage)))]

vintage_colors <- c("Pre-1989" = COL_TX, "All-Post-1989" = COL_CTRL)

p_vintage <- ggplot(
  vintage_atts[!is.na(est)],
  aes(x = est, y = outcome_f,
      xmin = ci_lo, xmax = ci_hi,
      color = vintage, group = vintage)
) +
  geom_vline(xintercept = 0, linetype = "dashed",
             color = "grey50", linewidth = 0.5) +
  geom_pointrange(
    position = position_dodge(width = 0.55),
    size = 0.35, linewidth = 0.5
  ) +
  scale_color_manual(values = vintage_colors,
                     name   = "Tank vintage at reform") +
  labs(
    x = "ATT (Texas x Post), LPM coefficient",
    y = NULL,
    subtitle = paste0(
      "Pre-1989 = facility has any tank installed before 1990 (mandate-exposed); ",
      "All-Post-1989 = every tank installed 1990+ (mandate-exempt)"
    )
  ) +
  theme_pub() +
  theme(legend.position = "bottom",
        axis.text.y = element_text(size = 10),
        plot.subtitle = element_text(size = 9, color = "grey30"))

save_gg(p_vintage,
        file.path(OUTPUT_FIGURES, "Figure_HTE_Vintage"),
        width = 9.5, height = 5.5)
log_step("Saved: Figure_HTE_Vintage.png/.pdf")

fwrite(vintage_atts, file.path(OUTPUT_TABLES, "T_HTE_Vintage.csv"))
log_step("Wrote: T_HTE_Vintage.csv")

# B12B.5: LaTeX table for vintage HTE
{
  vint_rows <- unlist(lapply(names(outcome_labels_vintage), function(nm) {
    sub  <- vintage_atts[outcome == outcome_labels_vintage[[nm]]]
    pre  <- sub[vintage == "Pre-1989"]
    post <- sub[vintage == "All-Post-1989"]
    diff_p_str <- formatC(round(sub$diff_p[1L], 3L), digits = 3L, format = "f")
    diff_stars <- stars_p(sub$diff_p[1L])
    c(
      paste0(outcome_labels_vintage[[nm]], " & ",
             paste0(fmt_est(pre$est),  stars_p(pre$p)),  " & ",
             paste0(fmt_est(post$est), stars_p(post$p)), " & ",
             paste0(fmt_est(sub$diff_est[1L]), diff_stars), " & ",
             diff_p_str, " \\\\"),
      paste0(" & ", fmt_se(pre$se),  " & ",
                    fmt_se(post$se), " & ",
                    fmt_se(sub$diff_se[1L]), " &  \\\\[0.3em]")
    )
  }), recursive = FALSE)

  tex_vintage <- c(
    "\\begin{table}[htbp]",
    "\\centering",
    paste0("\\caption{Heterogeneous Treatment Effects by Tank Vintage at Reform: ",
           "Mandate-Exposed (Pre-1989) vs.\\ Mandate-Exempt (All-Post-1989) Facilities.}"),
    "\\label{tab:hte_vintage}",
    "\\begin{tabular}{lcccc}",
    "\\toprule",
    " & Pre-1989 & All-Post-1989 & Difference & Diff $p$ \\\\",
    " & (mandate-exposed) & (mandate-exempt) & (post -- pre) & \\\\",
    "\\midrule",
    vint_rows,
    "\\midrule",
    paste0("$N$ (fac-years) & \\multicolumn{4}{c}{",
           fmt_n(nrow(mm_vint)), "} \\\\"),
    paste0("$N$ facilities  & ", fmt_n(n_pre), " & ", fmt_n(n_post),
           " & \\multicolumn{2}{c}{} \\\\"),
    "Cell$\\times$Year FE & \\multicolumn{4}{c}{3D (make\\_model\\_fac $\\times$ year)} \\\\",
    "CEM weights          & \\multicolumn{4}{c}{Yes} \\\\",
    "Mandate controls     & \\multicolumn{4}{c}{Yes} \\\\",
    "Sample               & \\multicolumn{4}{c}{Reform-CEM, vintage-classified} \\\\",
    "\\bottomrule",
    "\\end{tabular}",
    "\\begin{minipage}{\\linewidth}",
    "\\small",
    paste0(
      "\\textit{Notes:} LPM coefficients from a single regression per outcome ",
      "with a treatment $\\times$ vintage interaction. ",
      "\\textit{Pre-1989}: facility had at least one tank installed in 1988 or earlier ",
      "(\\texttt{had\\_pre1989\\_tanks} $= 1$). These tanks are subject to the ",
      "federal UST mandates: release detection (1989--1993), spill/overfill ",
      "prevention (Dec 1994), and tank integrity (Dec 1998). ",
      "\\textit{All-Post-1989}: every tank at the facility was installed in 1990 ",
      "or later (\\texttt{all\\_post1989} $= 1$); these tanks are exempt from ",
      "the retrofit mandates above. ",
      "The Pre-1989 column reports the ATT for mandate-exposed facilities (ref). ",
      "The All-Post-1989 column reports the ATT for mandate-exempt facilities. ",
      "The Difference column reports the interaction coefficient (mandate-exempt ",
      "minus mandate-exposed); a non-zero value indicates that mandate exposure ",
      "amplifies or substitutes for the insurance-reform channel. ",
      "Cluster-robust SEs by state. ",
      "$^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$."
    ),
    "\\end{minipage}",
    "\\end{table}"
  )
  write_tex(tex_vintage, file.path(OUTPUT_TABLES, "T_HTE_Vintage.tex"))
  log_step("Wrote: T_HTE_Vintage.tex")
}

cat("\n")


################################################################################
# B13: EXPORT
################################################################################

cat("\n========================================\n")
cat("B13: EXPORT\n")
cat("========================================\n\n")

cat("B13.1: Saving primary sample RDS...\n")
saveRDS(mm_fac_reform, file.path(ANALYSIS_DIR, "fac_mm_reform_primary.rds"))
saveRDS(mm_fac_full,   file.path(ANALYSIS_DIR, "fac_mm_full_incumbent.rds"))
saveRDS(mm_fac_es,     file.path(ANALYSIS_DIR, "fac_mm_es_aligned.rds"))
log_step(sprintf("fac_mm_reform_primary.rds  -- %s fy | %s facilities",
  fmt_n(nrow(mm_fac_reform)), fmt_n(uniqueN(mm_fac_reform$panel_id))))

cat("B13.2: Saving Table 1 model objects...\n")
saveRDS(t1_models$col5, file.path(ANALYSIS_DIR, "fac_t1_any_closure_primary.rds"))
log_step("  fac_t1_any_closure_primary.rds")

cat("B13.3: Saving Table 2 model objects...\n")
for (nm_pair in list(
  list(obj=m_t2_exit,    file="fac_t2_exit"),
  list(obj=m_t2_upgrade, file="fac_t2_upgrade"),
  list(obj=m_t2_shrink,  file="fac_t2_shrink")
)) {
  saveRDS(nm_pair$obj, file.path(ANALYSIS_DIR, paste0(nm_pair$file, ".rds")))
  log_step(sprintf("  %s.rds", nm_pair$file))
}

cat("B13.4: Saving Table 3 model objects...\n")
for (nm in names(t3_models)) {
  saveRDS(t3_models[[nm]]$col2,
          file.path(ANALYSIS_DIR, sprintf("fac_t3_%s_primary.rds", nm)))
}
log_step(sprintf("  %d Table 3 primary models saved", length(t3_models)))

cat("B13.5: Saving LUST model objects...\n")
saveRDS(m_lust_total,   file.path(ANALYSIS_DIR, "fac_lust_total.rds"))
saveRDS(m_lust_back,    file.path(ANALYSIS_DIR, "fac_lust_background.rds"))
saveRDS(m_lust_inspect, file.path(ANALYSIS_DIR, "fac_lust_inspection.rds"))
log_step("  fac_lust_total/background/inspection.rds")

cat("B13.6: Saving ES model objects...\n")
for (stem in names(es_models)) {
  saveRDS(es_models[[stem]],
          file.path(ANALYSIS_DIR, sprintf("fac_es_%s.rds", tolower(stem))))
}
log_step(sprintf("  %d ES models saved", length(es_models)))

cat("B13.7: Saving HTE model objects...\n")
for (nm in names(wall_models)) {
  saveRDS(wall_models[[nm]],
          file.path(ANALYSIS_DIR, sprintf("fac_hte_wall_%s.rds", nm)))
}
for (nm in names(age_models)) {
  saveRDS(age_models[[nm]],
          file.path(ANALYSIS_DIR, sprintf("fac_hte_age_%s.rds", nm)))
}
for (nm in names(vintage_models)) {
  saveRDS(vintage_models[[nm]],
          file.path(ANALYSIS_DIR, sprintf("fac_hte_vintage_%s.rds", nm)))
}
log_step(sprintf("  %d wall + %d age + %d vintage HTE models saved",
  length(wall_models), length(age_models), length(vintage_models)))

cat("B13.8: Scalar diagnostics CSV...\n")

scalars_dt <- data.table(
  scalar = c(
    "n_fy_full",        "n_fac_full",
    "n_fy_reform",      "n_fac_reform",
    "n_tx_reform",      "n_ctl_reform",
    "n_cells_reform",   "n_fy_es",
    "panel_start",      "panel_end",       "post_year",
    "t1_any_closure_beta", "t1_any_closure_se",  "t1_any_closure_p",
    "t2_exit_beta",        "t2_exit_se",         "t2_exit_p",
    "t2_upgrade_beta",     "t2_upgrade_se",       "t2_upgrade_p",
    "t2_shrink_beta",      "t2_shrink_se",        "t2_shrink_p",
    "lust_total_beta",     "lust_total_se",       "lust_total_p",
    "lust_back_beta",      "lust_back_se",        "lust_back_p",
    "hd_any_closure_bd_rm",  "hd_any_closure_bd_sd",
    "hd_exit_bd_rm",         "hd_exit_bd_sd",
    "hd_lust_back_bd_rm",    "hd_lust_back_bd_sd"
  ),
  value = c(
    n_fy_full,    n_fac_full,
    n_fy_reform,  n_fac_reform,
    n_tx_reform,  n_ctl_reform,
    n_cells_reform, n_fy_es,
    PANEL_START,  PANEL_END,  POST_YEAR,
    {d <- extract_did(t1_models$col5);  c(d$beta, d$se, d$p)},
    {d <- extract_did(m_t2_exit);       c(d$beta, d$se, d$p)},
    {d <- extract_did(m_t2_upgrade);    c(d$beta, d$se, d$p)},
    {d <- extract_did(m_t2_shrink);     c(d$beta, d$se, d$p)},
    {d <- extract_did(m_lust_total);    c(d$beta, d$se, d$p)},
    {d <- extract_did(m_lust_back);     c(d$beta, d$se, d$p)},
    hd_results$AnyClosure$bd_rm,    hd_results$AnyClosure$bd_sd,
    hd_results$FacilityExit$bd_rm,  hd_results$FacilityExit$bd_sd,
    hd_results$LUSTBackground$bd_rm, hd_results$LUSTBackground$bd_sd
  )
)
fwrite(scalars_dt, file.path(OUTPUT_TABLES, "Diag_FacBehavior_Scalars.csv"))
log_step("Wrote: Diag_FacBehavior_Scalars.csv")

cat("B13.9: Output manifest...\n")

collect_manifest <- function(dir, pattern) {
  fls <- list.files(dir, pattern = pattern, full.names = TRUE)
  if (length(fls) == 0L) return(data.table())
  data.table(
    filepath  = fls,
    filename  = basename(fls),
    directory = dir,
    ext       = tools::file_ext(fls),
    size_kb   = round(file.size(fls) / 1024, 1L)
  )
}

fac_prefixes <- paste(
  c("T1_","T2_","T2b_","T3_","T_LUST","ES_","T_HonestDiD",
    "T_HTE","Figure_ES","Figure_HTE","Figure_HonestDiD",
    "Diag_B4","Diag_FacBehavior"),
  collapse = "|"
)
manifest <- rbind(
  collect_manifest(OUTPUT_TABLES,  "\\.(tex|csv)$"),
  collect_manifest(OUTPUT_FIGURES, "\\.(png|pdf)$"),
  fill = TRUE
)
manifest <- manifest[grepl(fac_prefixes, filename, ignore.case = TRUE)]
fwrite(manifest, file.path(OUTPUT_TABLES, "Manifest_FacBehavior.csv"))
log_step(sprintf("Wrote: Manifest_FacBehavior.csv  (%d files)", nrow(manifest)))

cat("\n========================================\n")
cat("02a_DiD_FacBehavior.R COMPLETE\n")
cat("========================================\n\n")


# Do mandate controls differ systematically between 
# T