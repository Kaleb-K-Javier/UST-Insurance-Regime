################################################################################
# Code/Helpers/reduced_form_utils.R
#
# Shared constants, utilities, sample builders, and bootstrap wrappers for
# 02c-02f reduced-form scripts (stepped DiD, event studies, vintage HTE,
# HonestDiD).
#
# Usage: source(here::here("Code", "Helpers", "reduced_form_utils.R"))
#
# Contract: sourcing this file triggers NO file I/O and NO estimation.
################################################################################

stopifnot(requireNamespace("data.table", quietly = TRUE))
stopifnot(requireNamespace("fixest",     quietly = TRUE))
stopifnot(requireNamespace("survival",   quietly = TRUE))
stopifnot(requireNamespace("here",       quietly = TRUE))

# =============================================================================
# === CONSTANTS ===
# =============================================================================

REFORM_DATE    <- as.Date("1998-12-22")
REFORM_DAYS    <- as.numeric(REFORM_DATE)   # 10583 days since 1970-01-01 (R origin)
                                            # matches t_enter/t_exit scale in exact_base

CONTROL_STATES <- c(
  "AR", "CO", "ID", "KS", "KY",
  "LA", "MA", "MD", "ME", "MN", "MO", "NC",
  "OH", "OK", "SD", "TN", "VA"
)

OUTPUT_TABLES  <- here::here("Output", "Tables")
OUTPUT_FIGURES <- here::here("Output", "Figures")

# =============================================================================
# === UTILITY FUNCTIONS ===
# =============================================================================

log_step <- function(msg, indent = 0L) cat(strrep("  ", indent), msg, "\n", sep = "")
fmt_n    <- function(n) format(n, big.mark = ",", scientific = FALSE)

save_gg <- function(p, stem, width = 7, height = 5) {
  ggplot2::ggsave(file.path(OUTPUT_FIGURES, paste0(stem, ".pdf")),
                  p, width = width, height = height,
                  device = grDevices::cairo_pdf)
  ggplot2::ggsave(file.path(OUTPUT_FIGURES, paste0(stem, ".png")),
                  p, width = width, height = height,
                  dpi = 300, bg = "white")
  invisible(p)
}

write_tex <- function(lines, filename) {
  writeLines(lines, file.path(OUTPUT_TABLES, filename))
  message(sprintf("  Written: %s", filename))
}

# =============================================================================
# === SAMPLE BUILDERS ===
# =============================================================================

# Build the active-at-treatment sample from the birth-CEM matched panel.
#
# Retains facilities that had >= 1 tank open or newly installed on 1998-12-22.
# CEM weights are used only for cell-support filtering, NOT in regressions.
# Adds: cell_id, cell_vintage_year_fe, rel_year, pre89_cohort.
build_active_at_treatment_sample <- function(matched_panel) {
  d <- matched_panel[install_yr_int < 1999L & cem_weight > 0]

  tanks_open_1998      <- d[panel_year == 1998L & closure_event == 0L, unique(panel_id)]
  tanks_installed_1998 <- d[install_yr_int == 1998L, unique(panel_id)]
  facilities_active    <- union(tanks_open_1998, tanks_installed_1998)

  out <- d[panel_id %in% facilities_active]

  out[, cell_id              := .GRP, by = .(make_model_noage, install_yr_int)]
  out[, cell_vintage_year_fe := .GRP, by = .(panel_year, make_model_noage, install_yr_int)]
  out[, rel_year             := as.integer(panel_year) - 1998L]
  out[, pre89_cohort         := as.integer(install_yr_int <= 1988L)]

  stopifnot(nrow(out) > 0)
  stopifnot(data.table::uniqueN(out$state) >= 18L)
  stopifnot(data.table::uniqueN(out$panel_id) > 0L)
  stopifnot(all(out$cem_weight > 0))

  out[]
}

# Build two-episode Cox splits at the reform date for the active-at-treatment
# sample. Joins active_panel tank IDs back to exact_base_dt to inherit
# canonical t_enter / t_exit / failure (days since 1970-01-01 origin, matching
# REFORM_DAYS). Adds: did_term, panel_year (episode midpoint), mandate_*,
# cell_id.
build_active_cox_split <- function(active_panel, exact_base_dt,
                                   reform_cut = REFORM_DAYS) {
  active_ids <- unique(active_panel$tank_panel_id)
  base       <- exact_base_dt[tank_panel_id %in% active_ids]

  n_closures_base <- sum(base$failure)

  split_df <- data.table::as.data.table(survival::survSplit(
    formula = Surv(t_enter, t_exit, failure) ~ .,
    data    = as.data.frame(base),
    cut     = reform_cut,
    episode = "reform_ep"
  ))
  split_df <- split_df[t_exit > t_enter]

  split_df[, did_term := texas_treated * as.integer(reform_ep == 2L)]

  # Episode midpoint year: used for mandate controls and factor(panel_year) covariate
  split_df[, panel_year := as.integer(
    format(as.Date((t_enter + t_exit) / 2, origin = "1970-01-01"), "%Y")
  )]

  split_df[, mandate_release_det := as.integer(
    !is.na(release_det_deadline_yr) &
      panel_year >= 1989L &
      panel_year <= release_det_deadline_yr
  )]
  split_df[, mandate_spill_overfill := as.integer(
    !is.na(release_det_deadline_yr) &
      panel_year %in% 1993L:1994L
  )]
  split_df[, mandate_integrity := as.integer(
    !is.na(release_det_deadline_yr) &
      panel_year %in% 1996L:1998L
  )]

  # cell_id for use as factor covariate in Cox cols 6-7
  split_df[, cell_id := .GRP, by = .(make_model_noage, install_yr_int)]

  stopifnot(all(active_ids %in% split_df$tank_panel_id))
  stopifnot(sum(split_df$failure) == n_closures_base)

  split_df[]
}

# =============================================================================
# === BOOTSTRAP — INTERNAL WORKERS ===
# =============================================================================

# OLS score wild cluster bootstrap (analogous to Kline-Santos 2012).
# Lifted and adapted from 02b::run_boot_ols_score. Fully vectorized.
# REQUIREMENT: data must be the data.table passed to feols(), with "state" col.
# The model's obs() indices map into this table by row position.
.run_boot_ols_score <- function(model, param = "did_term",
                                B = 9999L, seed = 42L, data) {
  set.seed(seed)

  ct  <- fixest::coeftable(model)
  idx <- which(rownames(ct) == param)[1L]
  stopifnot("param not found in model coeftable" = !is.na(idx))

  coef_obs <- ct[idx, "Estimate"]
  se_obs   <- ct[idx, "Std. Error"]
  t_obs    <- coef_obs / se_obs

  obs_idx <- fixest::obs(model)
  states  <- data[["state"]][obs_idx]
  G       <- length(unique(states))

  wts_raw <- weights(model)
  wts     <- if (!is.null(wts_raw)) wts_raw[obs_idx] else NULL

  # Restrict data to estimation obs before demean to avoid cell x year FE
  # misalignment when fixest drops singletons internally.
  data_est <- data[obs_idx]
  dm       <- as.data.frame(fixest::demean(model, data = data_est))

  y_nm     <- as.character(formula(model)[[2L]])
  null_nms <- setdiff(names(dm), c(y_nm, param))
  stopifnot("param missing from demeaned frame" = param %in% names(dm))

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

  W       <- matrix(sample(c(-1L, 1L), B * G, replace = TRUE), nrow = B, ncol = G)
  score_b <- as.numeric(W %*% cluster_scores)
  var_b   <- sum(cluster_scores^2)
  t_boot  <- score_b / sqrt(var_b + 1e-10)

  p_boot     <- mean(abs(t_boot) >= abs(t_obs))
  se_boot    <- sd(t_boot) * se_obs
  boot_coefs <- t_boot * se_obs + coef_obs

  list(
    p_boot  = p_boot,
    se_boot = se_boot,
    ci_lo   = unname(quantile(boot_coefs, 0.025)),
    ci_hi   = unname(quantile(boot_coefs, 0.975)),
    t_stat  = t_obs
  )
}

# Cox score wild cluster bootstrap (Kline-Santos 2012).
# Lifted and adapted from 02b::run_boot_cox. Fully vectorized.
# Requires explicit data= because coxph stores Surv() in a model frame.
#
# NOTE for Ticket 007: t_boot_vec will need to become a boot_betas matrix
# (B x p) for linear-combo ATT bootstraps across cohort HTE parameters.
.run_boot_cox <- function(cox_model, term = "did_term",
                          B = 9999L, seed = 42L, data) {
  set.seed(seed)
  stopifnot("term not in model coefficients" = term %in% names(coef(cox_model)))

  states   <- data[["state"]]
  s        <- summary(cox_model)$coefficients
  coef_obs <- s[term, "coef"]
  se_obs   <- s[term, "robust se"]
  t_obs    <- coef_obs / se_obs

  null_fml <- stats::update(cox_model$formula,
                            stats::as.formula(paste(". ~ . -", term)))
  m_null   <- survival::coxph(null_fml, data = data,
                               cluster = state, ties = "efron", model = FALSE)

  # `naresid` is exported from {stats}, not {survival}; the survival package
  # uses it internally but does not re-export. 02b calls it bare via the
  # search path; we qualify with stats:: for explicitness.
  mart_resid  <- stats::naresid(m_null$na.action,
                                 residuals(m_null, type = "martingale"))
  score_vec   <- mart_resid * data[[term]]

  valid_rows     <- !is.na(score_vec) & !is.na(states)
  cluster_scores <- as.numeric(tapply(score_vec[valid_rows], states[valid_rows], sum))
  G              <- length(cluster_scores)

  W       <- matrix(sample(c(-1L, 1L), B * G, replace = TRUE), nrow = B, ncol = G)
  score_b <- as.numeric(W %*% cluster_scores)
  var_b   <- sum(cluster_scores^2)
  t_boot  <- score_b / sqrt(var_b + 1e-10)

  boot_coefs <- t_boot * se_obs + coef_obs

  list(
    p_boot     = mean(abs(t_boot) >= abs(t_obs)),
    se_boot    = sd(t_boot) * se_obs,
    ci_lo      = unname(quantile(boot_coefs, 0.025)),
    ci_hi      = unname(quantile(boot_coefs, 0.975)),
    t_obs      = t_obs,
    t_boot_vec = t_boot
  )
}

# =============================================================================
# === BOOTSTRAP — PUBLIC INTERFACE ===
# =============================================================================

# Wild cluster bootstrap (score variant) for OLS feols models.
# data= is REQUIRED: the data.table passed to feols(), with "state" column.
#
# NOTE for Ticket 007: extend .run_boot_ols_score to return a boot_betas
# matrix (B x p) when linear combos across HTE parameters are needed.
run_wcb_ols <- function(model, data, param = "did_term",
                        B = 9999L, seed = 20260519L) {
  raw     <- .run_boot_ols_score(model, param = param, B = B,
                                 seed = seed, data = data)
  se_boot <- raw$se_boot
  stopifnot(is.finite(se_boot) && se_boot > 0)
  list(
    SE_boot    = se_boot,
    CI_lo      = raw$ci_lo,
    CI_hi      = raw$ci_hi,
    p_boot     = raw$p_boot,
    B          = B,
    n_clusters = data.table::uniqueN(data[["state"]][fixest::obs(model)])
  )
}

# Wild score bootstrap for Cox coxph models.
# data= is REQUIRED: the data.table passed to coxph().
#
# NOTE for Ticket 007: see run_wcb_ols note re: boot_betas matrix extension.
run_wcb_cox <- function(model, data, param = "did_term",
                        B = 9999L, seed = 20260519L) {
  raw     <- .run_boot_cox(cox_model = model, term = param,
                            B = B, seed = seed, data = data)
  se_boot <- raw$se_boot
  stopifnot(is.finite(se_boot) && se_boot > 0)
  list(
    SE_boot    = se_boot,
    CI_lo      = raw$ci_lo,
    CI_hi      = raw$ci_hi,
    p_boot     = raw$p_boot,
    B          = B,
    n_clusters = data.table::uniqueN(data[["state"]])
  )
}

# =============================================================================
# === PUBLICATION TABLES + THREADING ===
# =============================================================================

# Enable multithreading for fixest + data.table (call once at script top).
rf_use_threads <- function() {
  nthr <- max(1L, parallel::detectCores())
  fixest::setFixest_nthreads(nthr)
  data.table::setDTthreads(nthr)
  cat(sprintf("threads: fixest=%d data.table=%d\n",
              fixest::getFixest_nthreads(), data.table::getDTthreads()))
  invisible(nthr)
}

# Clean labels for publication tables (NO code/variable names appear in output).
RF_DICT <- c(
  did_term      = "Reform $\\times$ Post",
  did_Z         = "\\quad $\\times$ subgroup ($=1$)",
  closure_event = "Tank closure",
  closure_share = "Closure share",
  any_closure   = "Any closure",
  facility_exit = "Facility exit",
  perm_share    = "Permanent closure",
  repl_share    = "Replacement",
  downsize       = "Downsize",
  consolidate    = "Consolidate",
  reconfigure_up = "Reconfigure-up",
  any_replace    = "Any replacement",
  cap_decrease   = "Capacity cut",
  tau           = "Closure share",
  Yhat0         = "Predicted baseline",
  cell_fac_year = "Mix $\\times$ year",
  G1_lt9k       = "Under 9k gal",
  G2_9to20k     = "9--20k gal",
  G3_20to30k    = "20--30k gal",
  G4_gt30k      = "Over 30k gal",
  panel_id      = "Facility",
  tank_panel_id = "Tank",
  cell_vintage_year_fe = "Cell $\\times$ year",
  panel_year    = "Year",
  state         = "State"
)

# Publication regression table via fixest::etable. Drops the nuisance controls
# (mandate dummies, the predicted-baseline control) and all FE/coef code names
# are renamed via RF_DICT. SEs are whatever each model carries (cluster ~ state).
pub_etable <- function(models, file, headers = NULL, title = NULL, notes = NULL,
                       drop = c("Yhat0", "mandate", "pct_sw", "avg_tank_age")) {
  fixest::etable(models, tex = TRUE, file = file, replace = TRUE,
                 dict = RF_DICT, drop = drop, headers = headers,
                 title = title, notes = notes, fitstat = ~ n,
                 digits = 4,
                 style.tex = fixest::style.tex("aer"), se.below = TRUE)
  message(sprintf("  Written (pub): %s", file))
}
