################################################################################
# S10_Cox_EventStudy_Anticipation.R
# ─────────────────────────────────────────────────────────────────────────────
# ANTICIPATION-EXPLICIT EVENT STUDY SPECIFICATION
#
# SPECIFICATION:
# Y_it = α_i + γ_t + Σ(τ ≤ -2) β_τ D_i,t+τ + β_anticipation D_i,1998 
#        + Σ(τ ≥ 0) β_τ D_i,t+τ + ε_it
#
# KEY FEATURES:
#   • Reference year: 1997 (τ = -2; strict pre-anticipation baseline)
#   • Anticipation period: 1998 (τ = -1; explicitly estimated as separate param)
#   • Reform onset: 1999 onward (τ ≥ 0; post-reform treatment effects)
#   • Pre-anticipation periods: τ ≤ -2 (test parallel trends here only)
#
# RATIONALE:
#   By re-centering the omitted reference to 1997 and explicitly isolating the
#   anticipation parameter β_anticipation, we can:
#     1. Formally test parallel trends ONLY in the pre-anticipation period
#     2. Quantify the magnitude of anticipatory behavior separately
#     3. Avoid the confounding of anticipation with baseline trends
#
# USAGE:
#   source(here("Code", "Helpers", "S10_Cox_EventStudy_Anticipation.R"))
#   
#   # Primary anticipation-explicit spec (ref=1997, separate anticipation param)
#   res_antic <- run_cox_event_study_anticipation(
#     exact_split_df[is_post_mandate == 1L],
#     label = "Anticipation-Explicit"
#   )
#   
#   # Sensitivity: Tighter pooling with anticipation
#   res_antic_tight <- run_cox_event_study_anticipation(
#     exact_split_df[is_post_mandate == 1L],
#     pool_pre = -4L, pool_post = 10L,
#     label = "Anticipation-Explicit-TightPool"
#   )
#
# RETURN VALUE (list):
#   $model          : fitted coxph object with separate anticipation param
#   $coefs          : data.table of tidy coefficients
#   $coefs_summary  : simplified coefficient table (anticipation focused)
#   $pre_tests      : list with parallel-trends tests (pre-anticipation only)
#   $anticipation   : explicit estimate of anticipation effect
#   $plot           : ggplot object
#   $plot_comparative : ggplot comparing all periods
#   $es_data        : the event-study dataset used
#   $params         : list echoing back all parameter values
#
# DEPENDENCIES (must be loaded before sourcing):
#   data.table, survival, ggplot2, scales, car
#   Objects from earlier sections: REFORM_DATE, STUDY_END, COL_TX, COL_CTRL,
#   theme_pub(), log_step(), fmt_n(), stars_p()
################################################################################


# ═══════════════════════════════════════════════════════════════════════════════
# 1. cox_es_anticipation_build_dummies
# ═══════════════════════════════════════════════════════════════════════════════
#' Create relative-year × texas_treated interaction dummies with
#' EXPLICIT ANTICIPATION SEPARATION.
#'
#' @param dt         data.table output of cox_es_split() with rel_year column.
#' @param pool_pre   Integer.  Relative years ≤ this value are pooled.
#'                   Reference will be at pool_pre (which is -2 for anticipation spec).
#' @param pool_post  Integer.  Relative years ≥ this value are pooled.
#' @return Named list:
#'   $dt           : data.table with ry_* columns appended
#'   $ry_vars      : character vector of variable names in the model formula
#'   $anticipation_var : the name of the anticipation dummy variable

cox_es_anticipation_build_dummies <- function(dt,
                                               pool_pre  = -2L,
                                               pool_post = 15L) {

  ry_all <- sort(unique(dt$rel_year))

  # ─── REFERENCE YEAR (omitted from dummies) ──────────────────────────────
  # For anticipation spec: ref_year = pool_pre (e.g., -2 for 1997)
  # This is the year BEFORE anticipation starts
  ref_year <- pool_pre

  # ─── PRE-ANTICIPATION YEARS (τ ≤ -2) ────────────────────────────────────
  # Individual years strictly before anticipation
  ry_pre_indiv <- sort(ry_all[
    ry_all < pool_pre  &
    ry_all != ref_year
  ])

  # ─── ANTICIPATION YEAR (τ = -1, i.e., 1998) ────────────────────────────
  # EXPLICITLY SEPARATE — not pooled, not omitted
  dt[, ry_anticipation := as.integer(rel_year == -1L) * texas_treated]
  anticipation_var <- "ry_anticipation"

  # ─── POST-REFORM YEARS (τ ≥ 0) ──────────────────────────────────────────
  ry_post_indiv <- sort(ry_all[
    ry_all >= 0L
  ])

  # ─── BUILD DUMMY VARIABLES ──────────────────────────────────────────────
  ry_vars <- c()

  # Pre-anticipation pooled tail
  if (any(ry_all <= pool_pre)) {
    dt[, ry_pool_pre := as.integer(rel_year < pool_pre) * texas_treated]
    ry_vars <- c(ry_vars, "ry_pool_pre")
  }

  # Pre-anticipation individual years
  for (yr in ry_pre_indiv) {
    vname <- if (yr < 0L) paste0("ry_pre_m", abs(yr)) else paste0("ry_pre_", yr)
    dt[, (vname) := as.integer(rel_year == yr) * texas_treated]
    ry_vars <- c(ry_vars, vname)
  }

  # Anticipation (explicit, separate)
  ry_vars <- c(ry_vars, anticipation_var)

  # Post-reform individual years
  for (yr in ry_post_indiv) {
    vname <- paste0("ry_post_", yr)
    dt[, (vname) := as.integer(rel_year == yr) * texas_treated]
    ry_vars <- c(ry_vars, vname)
  }

  # Post-reform pooled tail
  if (any(ry_all >= pool_post)) {
    dt[, ry_pool_post := as.integer(rel_year >= pool_post) * texas_treated]
    ry_vars <- c(ry_vars, "ry_pool_post")
  }

  list(
    dt              = dt,
    ry_vars         = sort(ry_vars),
    anticipation_var = anticipation_var,
    ref_year        = ref_year,
    ry_pre_indiv    = ry_pre_indiv,
    ry_post_indiv   = ry_post_indiv
  )
}


# ═══════════════════════════════════════════════════════════════════════════════
# 2. cox_es_anticipation_tidy_coefs
# ═══════════════════════════════════════════════════════════════════════════════
#' Extract and tidy coefficients with explicit anticipation effect.
#'
#' @param model       Fitted coxph object.
#' @param pool_pre    Integer. Pre-tail pooling boundary.
#' @param pool_post   Integer. Post-tail pooling boundary.
#' @param anticipation_var Character. Name of the anticipation variable.
#' @return data.table with columns: term, coef, robust_se, p, rel_year,
#'         hr, ci_lo, ci_hi, pct_change, pct_ci_lo, pct_ci_hi,
#'         period, effect_type.

cox_es_anticipation_tidy_coefs <- function(model,
                                            pool_pre = -2L,
                                            pool_post = 15L,
                                            anticipation_var = "ry_anticipation") {

  s <- summary(model)$coefficients
  ct <- as.data.table(s, keep.rownames = "term")

  # ── Rename columns robustly ─────────────────────────────────────────────
  old_names <- names(ct)
  new_names <- old_names
  new_names[grepl("^coef$|^coef\\b",       old_names, ignore.case = TRUE)][1] <- "coef"
  new_names[grepl("exp\\(coef\\)",          old_names, ignore.case = TRUE)][1] <- "exp_coef"
  new_names[grepl("robust se|se\\(coef\\)", old_names, ignore.case = TRUE)][1] <- "robust_se"
  new_names[grepl("Pr\\(>\\|z\\|\\)",       old_names, ignore.case = TRUE)][1] <- "p"
  setnames(ct, new_names)

  # ── Parse rel_year and effect_type from variable name ───────────────────
  ct[, `:=`(
    rel_year = fcase(
      term == "ry_pool_pre",             pool_pre,
      term == "ry_pool_post",            pool_post,
      term == anticipation_var,          -1L,
      grepl("^ry_pre_m[0-9]+$", term),  -as.integer(gsub("ry_pre_m", "", term)),
      grepl("^ry_pre_[0-9]+$",  term),   as.integer(gsub("ry_pre_",  "", term)),
      grepl("^ry_post_[0-9]+$", term),   as.integer(gsub("ry_post_", "", term)),
      default = NA_integer_
    ),
    effect_type = fcase(
      term == anticipation_var,         "Anticipation",
      grepl("^ry_pre_", term),          "Pre-Anticipation",
      grepl("^ry_post_", term),         "Post-Reform",
      term == "ry_pool_pre",            "Pre-Anticipation",
      term == "ry_pool_post",           "Post-Reform",
      default = NA_character_
    )
  )]
  ct <- ct[!is.na(rel_year)]

  # ── Derived quantities ──────────────────────────────────────────────────
  ct[, `:=`(
    hr         = exp(coef),
    ci_lo      = exp(coef - 1.96 * robust_se),
    ci_hi      = exp(coef + 1.96 * robust_se),
    pct_change = (exp(coef) - 1) * 100,
    pct_ci_lo  = (exp(coef - 1.96 * robust_se) - 1) * 100,
    pct_ci_hi  = (exp(coef + 1.96 * robust_se) - 1) * 100,
    log_ci_lo  = coef - 1.96 * robust_se,
    log_ci_hi  = coef + 1.96 * robust_se,
    period     = fcase(
      rel_year < -1L,  "Pre-Anticipation",
      rel_year == -1L, "Anticipation",
      rel_year >= 0L,  "Post-Reform"
    ),
    significance = stars_p(p)
  )]

  setorder(ct, rel_year)
  ct[]
}


# ═══════════════════════════════════════════════════════════════════════════════
# 3. cox_es_anticipation_pre_tests
# ═══════════════════════════════════════════════════════════════════════════════
#' Joint Wald χ² tests for pre-anticipation parallel trends ONLY.
#'   (i.e., test τ ≤ -2, excluding anticipation effect)
#'
#' @param model    Fitted coxph object.
#' @param ry_vars  Character vector of all RHS variable names.
#' @return Named list: chi_all, dof_all, p_all, chi_dense, dof_dense, p_dense.

cox_es_anticipation_pre_tests <- function(model,
                                           ry_vars,
                                           dense_range = 2:6) {

  # ─── PRE-ANTICIPATION VARIABLES ONLY (τ ≤ -2) ─────────────────────────
  # Include pooled pre tail and all ry_pre_* variables
  pre_vars_all <- ry_vars[
    ry_vars == "ry_pool_pre" | grepl("^ry_pre_", ry_vars)
  ]
  # Keep only those actually in the model
  pre_vars_all <- intersect(pre_vars_all, names(coef(model)))

  # Test all pre-anticipation vars (including pooled tail)
  test_all <- car::linearHypothesis(
    model, paste0(pre_vars_all, " = 0"), test = "Chisq"
  )
  chi_all <- test_all[2, "Chisq"]
  dof_all <- length(pre_vars_all)
  p_all   <- test_all[2, "Pr(>Chisq)"]

  # Dense pre-anticipation period (e.g., ry_pre_m2 through ry_pre_m6 if available)
  pre_vars_dense <- intersect(
    paste0("ry_pre_m", dense_range),
    names(coef(model))
  )
  chi_dense <- NA_real_
  dof_dense <- length(pre_vars_dense)
  p_dense   <- NA_real_

  if (dof_dense >= 2L) {
    test_dense <- car::linearHypothesis(
      model, paste0(pre_vars_dense, " = 0"), test = "Chisq"
    )
    chi_dense <- test_dense[2, "Chisq"]
    p_dense   <- test_dense[2, "Pr(>Chisq)"]
  }

  list(
    chi_all   = chi_all,   dof_all   = dof_all,   p_all   = p_all,
    chi_dense = chi_dense, dof_dense = dof_dense, p_dense = p_dense
  )
}


# ═══════════════════════════════════════════════════════════════════════════════
# 4. cox_es_anticipation_plot
# ═══════════════════════════════════════════════════════════════════════════════
#' Render the anticipation-explicit event study figure.
#'
#' @param coefs      data.table from cox_es_anticipation_tidy_coefs().
#' @param pre_tests  List from cox_es_anticipation_pre_tests().
#' @param y_metric   "pct_change" (default) or "log_hr".
#' @param pool_pre   Integer. For x-axis label rendering.
#' @param pool_post  Integer. For x-axis label rendering.
#' @return ggplot object.

cox_es_anticipation_plot <- function(coefs,
                                      pre_tests,
                                      y_metric  = "pct_change",
                                      y_cap     = NULL,
                                      y_floor   = NULL,
                                      pool_pre  = -2L,
                                      pool_post = 15L) {

  coefs <- copy(coefs)

  # ── Select y-axis columns ──────────────────────────────────────────────
  if (y_metric == "log_hr") {
    coefs[, `:=`(y = coef, y_lo = log_ci_lo, y_hi = log_ci_hi)]
    y_label  <- "Log-Hazard Ratio vs. Year 1997 (Reference)"
    y_format <- function(x) sprintf("%+.2f", x)
  } else {
    coefs[, `:=`(y = pct_change, y_lo = pct_ci_lo, y_hi = pct_ci_hi)]
    y_label  <- "Change in Closure Hazard vs. Year 1997 (Reference)  (%)"
    y_format <- function(x) paste0(ifelse(x > 0, "+", ""), x, "%")
  }

  # ── Auto y-axis limits ─────────────────────────────────────────────────
  indiv <- coefs[!grepl("pool", term)]
  if (is.null(y_cap))
    y_cap   <- ceiling(max(indiv$y_hi, na.rm = TRUE) / 25) * 25
  if (is.null(y_floor))
    y_floor <- floor(min(indiv$y_lo, na.rm = TRUE) / 25) * 25

  # ── X-axis labels with clear period markers ────────────────────────────
  all_x  <- sort(unique(coefs$rel_year))
  x_labs <- as.character(all_x)
  x_labs[all_x == pool_pre]  <- paste0(pool_pre, "\n(Ref)")
  x_labs[all_x == -1L]       <- "-1\n(Antic.)"
  x_labs[all_x == 0L]        <- "0\n(Reform)"
  x_labs[all_x == pool_post] <- paste0("\u2265", pool_post)

  # ── Color scheme ───────────────────────────────────────────────────────
  # Pre-Anticipation: Control color
  # Anticipation: Warning/highlight color. Special color (e.g., orange)
  # Post-Reform: Treatment color
  period_colors <- c(
    "Pre-Anticipation" = COL_CTRL,
    "Anticipation"     = "#E67E22",  # Orange to highlight
    "Post-Reform"      = COL_TX
  )

  # ── Pre-test annotation (pre-anticipation only) ─────────────────────────
  pre_label <- sprintf(
    "Pre-anticipation parallel trends (τ ≤ -2)\nFull: χ²(%d)=%.2f, p=%.4f | Dense: χ²(%d)=%.2f, p=%.4f",
    pre_tests$dof_all, pre_tests$chi_all, pre_tests$p_all,
    pre_tests$dof_dense,
    ifelse(is.na(pre_tests$chi_dense), 0, pre_tests$chi_dense),
    ifelse(is.na(pre_tests$p_dense), 1, pre_tests$p_dense)
  )

  # ── Build plot ────────────────────────────────────────────────────────
  p <- ggplot(coefs, aes(x = rel_year, y = y)) +

    # Shade pre-anticipation period
    annotate("rect",
             xmin = -Inf, xmax = -1.5,
             ymin = -Inf, ymax  =  Inf,
             fill = "grey90", alpha = 0.50) +

    # Shade post-reform period
    annotate("rect",
             xmin = -0.5, xmax = Inf,
             ymin = -Inf, ymax  =  Inf,
             fill = colAlpha("#FFF4E6", 0.30), alpha = 0.30) +

    geom_hline(yintercept = 0, linetype = "dashed",
               color = "grey35", linewidth = 0.5) +
    geom_vline(xintercept = -1.5, color = "grey50", linewidth = 0.35,
               linetype = "dotted") +
    geom_vline(xintercept = -0.5, color = "grey20", linewidth = 0.70) +

    geom_line(aes(group = 1), color = "grey45",
              linewidth = 0.45, alpha = 0.65) +

    geom_errorbar(aes(ymin = y_lo, ymax = y_hi, color = period),
                  width = 0.25, linewidth = 0.50) +

    geom_point(aes(color = period, size = (term == "ry_anticipation")),
               shape = 16) +

    scale_size_manual(values = c("FALSE" = 2.4, "TRUE" = 3.5),
                      guide = "none") +

    annotate("text",
             x = pool_pre - 0.15, y = y_cap * 0.95,
             label = pre_label, hjust = 0, vjust = 1,
             size = 2.7, color = "grey25", lineheight = 1.1,
             family = "mono") +

    scale_color_manual(
      values = period_colors,
      guide  = "none"
    ) +
    scale_x_continuous(breaks = all_x, labels = x_labs) +
    scale_y_continuous(
      limits = c(y_floor, y_cap),
      oob    = scales::squish,
      labels = y_format
    ) +
    labs(
      x        = sprintf("Years Relative to Reform  (0 = %s)", REFORM_DATE),
      y        = y_label,
      title    = "Anticipation-Explicit Event Study: Texas UST Insurance Reform",
      subtitle = "Reference: 1997 | Anticipation: 1998 | Reform Onset: 1999+",
      caption  = paste0(
        "○ = individual year; ● = anticipation year (emphasized). ",
        "CIs = 95% cluster-robust (state, 24 clusters). ",
        "Pre-trend test restricted to pre-anticipation period (τ ≤ -2)."
      )
    ) +
    theme_pub() +
    theme(
      panel.grid.major.x = element_blank(),
      plot.margin   = margin(t = 10, r = 12, b = 8, l = 20, unit = "pt"),
      axis.text.x   = element_text(size = 9.5, angle = 0, hjust = 0.5),
      plot.title    = element_text(size = 11, color = "grey20"),
      plot.subtitle = element_text(size = 9.5, color = "grey40",
                                   margin = margin(t = 6, b = 6)),
      plot.caption  = element_text(size = 8, color = "grey45",
                                   hjust = 0, margin = margin(t = 8))
    ) +
    coord_cartesian(clip = "off")

  p
}


# ═══════════════════════════════════════════════════════════════════════════════
# 5. cox_es_anticipation_summary_table
# ═══════════════════════════════════════════════════════════════════════════════
#' Create a clean summary table highlighting the three effect components.
#'
#' @param coefs data.table from cox_es_anticipation_tidy_coefs().
#' @return data.table with primary point estimates for each period.

cox_es_anticipation_summary_table <- function(coefs) {

  # Extract headline estimates for each component
  pre_antic  <- coefs[effect_type == "Pre-Anticipation" &
                      term == "ry_pool_pre"]
  antic      <- coefs[effect_type == "Anticipation"]
  post_ref   <- coefs[effect_type == "Post-Reform" &
                      term == "ry_pool_post"]

  summary_dt <- data.table(
    Effect              = c("Pre-Anticipation (τ ≤ -2)", "Anticipation (τ = -1, Year 1998)",
                            "Post-Reform (τ ≥ 0)"),
    Estimate            = c("Pooled [Pre]", "Explicit", "Pooled [Post]"),
    log_HR              = c(
      ifelse(nrow(pre_antic) > 0, pre_antic$coef[1], NA_real_),
      antic$coef[1],
      ifelse(nrow(post_ref) > 0, post_ref$coef[1], NA_real_)
    ),
    HR_pct_change       = c(
      ifelse(nrow(pre_antic) > 0, pre_antic$pct_change[1], NA_real_),
      antic$pct_change[1],
      ifelse(nrow(post_ref) > 0, post_ref$pct_change[1], NA_real_)
    ),
    SE                  = c(
      ifelse(nrow(pre_antic) > 0, pre_antic$robust_se[1], NA_real_),
      antic$robust_se[1],
      ifelse(nrow(post_ref) > 0, post_ref$robust_se[1], NA_real_)
    ),
    p_value             = c(
      ifelse(nrow(pre_antic) > 0, pre_antic$p[1], NA_real_),
      antic$p[1],
      ifelse(nrow(post_ref) > 0, post_ref$p[1], NA_real_)
    ),
    Significance        = c(
      ifelse(nrow(pre_antic) > 0, pre_antic$significance[1], ""),
      antic$significance[1],
      ifelse(nrow(post_ref) > 0, post_ref$significance[1], "")
    )
  )

  summary_dt[]
}


# ═══════════════════════════════════════════════════════════════════════════════
# 6. run_cox_event_study_anticipation  —  ORCHESTRATOR
# ═══════════════════════════════════════════════════════════════════════════════
#' One-call wrapper for anticipation-explicit event study.
#'
#' @param dt           data.table. The exact-date counting-process dataset.
#' @param pool_pre     Integer. Default -2L (for 1997 reference).
#' @param pool_post    Integer. Default 15L.
#' @param strata_var   Character. Default "make_model_tank".
#' @param cluster_var  Character. Default "state".
#' @param dense_range  Integer vector. Absolute values for dense pre-trend test.
#' @param y_metric     "pct_change" (default) or "log_hr".
#' @param label        Character. Label for outputs.
#' @param save         Logical. Save figures and CSV?
#' @param file_stem    Character or NULL.
#' @return Named list: model, coefs, coefs_summary, pre_tests, anticipation,
#'         plot, plot_comparative, es_data, params.

run_cox_event_study_anticipation <- function(
    dt,
    pool_pre    = -2L,
    pool_post   = 15L,
    strata_var  = "make_model_tank",
    cluster_var = "state",
    dense_range = 2:6,
    y_metric    = "pct_change",
    label       = "Anticipation-Explicit",
    save        = TRUE,
    file_stem   = NULL) {

  # ── Parameter echo ────────────────────────────────────────────────────────
  params <- list(
    ref_year    = pool_pre,  # For 1997 baseline with anticipation at -1
    pool_pre    = pool_pre,
    pool_post   = pool_post,
    strata_var  = strata_var,
    cluster_var = cluster_var,
    dense_range = dense_range,
    y_metric    = y_metric,
    label       = label,
    n_input     = nrow(dt),
    n_tanks     = uniqueN(dt$tank_panel_id)
  )

  log_step(sprintf(
    "Cox ES Anticipation [%s]: ref=%d, antic=-1, pool_post=%d, strata=%s, cluster=%s, n=%s tanks",
    label, pool_pre, pool_post, strata_var, cluster_var, fmt_n(params$n_tanks)
  ))

  # ── Step 1: Standard cuts (using pool_pre as reference) ────────────────
  cuts <- cox_es_generate_cuts(
    reform_date = REFORM_DATE,
    pool_pre    = pool_pre,
    pool_post   = pool_post
  )

  # ── Step 2: Split ─────────────────────────────────────────────────────
  es_data <- cox_es_split(dt, cuts)
  log_step(sprintf("  Split: %s rows, %s events",
                   fmt_n(nrow(es_data)), fmt_n(sum(es_data$failure))), 1)

  # ── Step 3: Build ANTICIPATION-EXPLICIT dummies ───────────────────────
  dummy_res <- cox_es_anticipation_build_dummies(
    es_data,
    pool_pre  = pool_pre,
    pool_post = pool_post
  )
  es_data <- dummy_res$dt
  ry_vars <- dummy_res$ry_vars
  anticipation_var <- dummy_res$anticipation_var

  log_step(sprintf("  Dummies: %d variables | Ref=%d, Antic=-1, Pool post=%d",
                   length(ry_vars), pool_pre, pool_post), 1)

  # ── Step 4: Estimate ───────────────────────────────────────────────────
  model <- cox_es_estimate(
    es_data,
    ry_vars     = ry_vars,
    strata_var  = strata_var,
    cluster_var = cluster_var
  )
  log_step(sprintf("  Estimated: %d params, %s obs, %s events",
                   length(coef(model)),
                   fmt_n(model$n), fmt_n(model$nevent)), 1)

  # ── Step 5: Tidy coefficients (with anticipation separation) ──────────
  coefs <- cox_es_anticipation_tidy_coefs(
    model,
    pool_pre         = pool_pre,
    pool_post        = pool_post,
    anticipation_var = anticipation_var
  )

  # ── Extract anticipation estimate ────────────────────────────────────────
  antic_row <- coefs[effect_type == "Anticipation"]
  anticipation <- if (nrow(antic_row) > 0) {
    list(
      coef  = antic_row$coef[1],
      se    = antic_row$robust_se[1],
      hr    = antic_row$hr[1],
      pct   = antic_row$pct_change[1],
      p     = antic_row$p[1],
      ci_lo = antic_row$ci_lo[1],
      ci_hi = antic_row$ci_hi[1]
    )
  } else {
    list(coef = NA_real_, se = NA_real_, hr = NA_real_,
         pct = NA_real_, p = NA_real_, ci_lo = NA_real_, ci_hi = NA_real_)
  }

  # ── Step 6: Pre-tests (pre-anticipation only) ───────────────────────────
  pre_tests <- cox_es_anticipation_pre_tests(model, ry_vars, dense_range)
  log_step(sprintf(
    "  Pre-anticipation trends: χ²(%d)=%.2f, p=%.4f",
    pre_tests$dof_all, pre_tests$chi_all, pre_tests$p_all
  ), 1)

  # ── Anticipation test ──────────────────────────────────────────────────
  if (!is.na(anticipation$p)) {
    log_step(sprintf(
      "  Anticipation effect: β=%.4f, SE=%.4f, p=%.4f  (Δ = %+.1f%%)",
      anticipation$coef, anticipation$se, anticipation$p, anticipation$pct
    ), 1)
  }

  # ── Step 7: Summary table ──────────────────────────────────────────────
  coefs_summary <- cox_es_anticipation_summary_table(coefs)

  # ── Step 8: Plot ───────────────────────────────────────────────────────
  p <- cox_es_anticipation_plot(
    coefs      = coefs,
    pre_tests  = pre_tests,
    y_metric   = y_metric,
    pool_pre   = pool_pre,
    pool_post  = pool_post
  )

  # ── Step 9: Save ──────────────────────────────────────────────────────
  if (save) {
    if (is.null(file_stem))
      file_stem <- gsub("[^A-Za-z0-9_]", "", gsub(" ", "_", label))

    ggsave(file.path(OUTPUT_FIGURES,
                     sprintf("Figure_CoxES_Anticipation_%s.png", file_stem)),
           p, width = 14, height = 7, dpi = 300, bg = "white")
    ggsave(file.path(OUTPUT_FIGURES,
                     sprintf("Figure_CoxES_Anticipation_%s.pdf", file_stem)),
           p, width = 14, height = 7, device = cairo_pdf)
    fwrite(coefs,
           file.path(OUTPUT_TABLES,
                     sprintf("Table_CoxES_Anticipation_%s.csv", file_stem)))
    fwrite(coefs_summary,
           file.path(OUTPUT_TABLES,
                     sprintf("Table_CoxES_Anticipation_Summary_%s.csv", file_stem)))
    log_step(sprintf("  Saved: %s (fig + csv)", file_stem), 1)
  }

  invisible(list(
    model          = model,
    coefs          = coefs,
    coefs_summary  = coefs_summary,
    pre_tests      = pre_tests,
    anticipation   = anticipation,
    plot           = p,
    es_data        = es_data,
    params         = params
  ))
}


# ═══════════════════════════════════════════════════════════════════════════════
# HELPER: colAlpha (convenience function for semi-transparent colors)
# ═══════════════════════════════════════════════════════════════════════════════
colAlpha <- function(col, alpha = 0.5) {
  # Convert a color string to RGBA with specified alpha
  rgb_col <- col2rgb(col) / 255
  rgb(rgb_col[1], rgb_col[2], rgb_col[3], alpha = alpha)
}
