################################################################################
# S10_Cox_EventStudy_Modular.R
# ─────────────────────────────────────────────────────────────────────────────
# Drop-in replacement for Section 10 of 02b_DiD_Survival.R.
#
# WHAT CHANGED:
#   The monolithic S10 block is decomposed into six pure-function helpers plus
#   one orchestrator.  Every design choice that was previously hard-coded is
#   now a parameter you can move at call time:
#
#     • sample (exact_split_df subset)
#     • reference year  (default -1)
#     • pooling endpoints  (pre-tail, post-tail)
#     • stratification variable  (default make_model_tank)
#     • cluster variable  (default state)
#     • y-axis metric  (pct_change or log_hr)
#     • plot aesthetics  (y-axis limits, dimensions)
#
# USAGE:
#   source("S10_Cox_EventStudy_Modular.R")
#
#   # ── Primary spec (reproduces original S10 output) ──────────────────────
#   res <- run_cox_event_study(exact_split_df)
#
#   # ── Move the reference year to -2 ──────────────────────────────────────
#   res2 <- run_cox_event_study(exact_split_df, ref_year = -2L)
#
#   # ── Tighter pooling ───────────────────────────────────────────────────
#   res3 <- run_cox_event_study(exact_split_df, pool_pre = -4L, pool_post = 10L)
#
#   # ── Single-walled subsample ───────────────────────────────────────────
#   res_sw <- run_cox_event_study(
#     exact_split_df[mm_wall == "Single-Walled"],
#     label = "Single-Walled"
#   )
#
#   # ── 3-dim cell robustness ─────────────────────────────────────────────
#   exact_split_df[, make_model_3dim := paste(mm_wall, mm_fuel,
#                                              mm_install_cohort, sep = "_")]
#   res_3d <- run_cox_event_study(
#     exact_split_df,
#     strata_var = "make_model_3dim",
#     label      = "3-dim cell (no capacity)"
#   )
#
#   # ── Plot on log-HR scale instead of % change ─────────────────────────
#   res_log <- run_cox_event_study(exact_split_df, y_metric = "log_hr")
#
# RETURN VALUE (list):
#   $model      : fitted coxph object
#   $coefs      : data.table of tidy coefficients (rel_year, hr, ci, pct, ...)
#   $pre_tests  : list with chi_all, p_all, chi_dense, p_dense, etc.
#   $plot       : ggplot object
#   $es_data    : the survSplit event-study dataset used for estimation
#   $params     : list echoing back all parameter values (for reproducibility)
#
# DEPENDENCIES (must be loaded before sourcing):
#   data.table, survival, ggplot2, scales, car
#   Objects from earlier sections: REFORM_DATE, STUDY_END, COL_TX, COL_CTRL,
#   theme_pub(), log_step(), fmt_n(), stars_p()
################################################################################


# ═══════════════════════════════════════════════════════════════════════════════
# 1.  cox_es_generate_cuts
# ═══════════════════════════════════════════════════════════════════════════════
#' Build the vector of exact reform-date anniversary cut points.
#'
#' @param reform_date  IDate or Date.  The statutory reform date.
#' @param pool_pre     Integer.  Most-negative relative year to estimate
#'                     individually; years further left are pooled.
#' @param pool_post    Integer.  Most-positive relative year to estimate
#'                     individually; years further right are pooled.
#' @param pad          Integer >= 1.  Extra years beyond the pooling endpoints
#'                     to include in the cut vector so that survSplit creates
#'                     episodes for the pooled tails.  Default 2.
#' @return Named list:
#'   $cuts_idate   : IDate vector of anniversary dates
#'   $cuts_numeric : numeric vector (days since 1970-01-01)
#'   $anchor_ep    : integer — the episode number that maps to rel_year = 0

cox_es_generate_cuts <- function(reform_date = REFORM_DATE,
                                  pool_pre    = -6L,
                                  pool_post   = 15L,
                                  pad         = 2L) {

  reform_date <- as.Date(reform_date)
  ref_yr <- as.integer(format(reform_date, "%Y"))
  ref_mo <- format(reform_date, "%m")
  ref_dy <- format(reform_date, "%d")

  # Range of relative years to generate cuts for:
  #   from (pool_pre - pad) to (pool_post + pad)
  # The pad ensures survSplit creates episodes BEYOND the pooling endpoints
  # so that the pooled-tail dummies have observations to work with.
  ry_lo <- pool_pre  - pad
  ry_hi <- pool_post + pad

  anniversaries <- as.IDate(
    vapply(ry_lo:ry_hi, function(offset) {
      paste(ref_yr + offset, ref_mo, ref_dy, sep = "-")
    }, character(1))
  )

  cuts_numeric <- as.numeric(anniversaries)

  # Anchor: the episode where the interval [reform_date, reform_date + 1yr)
  # begins.  This corresponds to rel_year = 0.
  # Episode numbering from survSplit:
  #   ep = 1 → before first cut
  #   ep = 2 → [cut_1, cut_2)
  #   ...
  # The reform_date itself is cut number (0 - ry_lo + 1) in the sorted vector.
  # The episode that STARTS at that cut = same index + 1.
  reform_cut_idx <- which(anniversaries == as.IDate(reform_date))
  anchor_ep      <- reform_cut_idx + 1L   # episode whose LEFT boundary is reform_date

  list(
    cuts_idate   = anniversaries,
    cuts_numeric = cuts_numeric,
    anchor_ep    = anchor_ep
  )
}


# ═══════════════════════════════════════════════════════════════════════════════
# 2.  cox_es_split
# ═══════════════════════════════════════════════════════════════════════════════
#' Split each tank's life at anniversary dates and map episodes → rel_year.
#'
#' @param dt         data.table with columns t_enter, t_exit, failure, and
#'                   texas_treated (the output of S5's survSplit at reform date,
#'                   or any counting-process dataset with those columns).
#' @param cuts       Output of cox_es_generate_cuts().
#' @return data.table with additional columns: es_episode, rel_year.

cox_es_split <- function(dt, cuts) {

  es_df <- survSplit(
    formula = Surv(t_enter, t_exit, failure) ~ .,
    data    = as.data.frame(dt),
    cut     = cuts$cuts_numeric,
    episode = "es_episode"
  )
  setDT(es_df)
  es_df <- es_df[t_exit > t_enter]

  # Map episode → rel_year using the anchor
  # anchor_ep corresponds to rel_year = 0, so:
  #   rel_year = es_episode - anchor_ep
  es_df[, rel_year := as.integer(es_episode) - cuts$anchor_ep]

  es_df[]
}


# ═══════════════════════════════════════════════════════════════════════════════
# 3.  cox_es_build_dummies
# ═══════════════════════════════════════════════════════════════════════════════
#' Create the relative-year × texas_treated interaction dummies with
#' pooled tails and an omitted reference year.
#'
#' @param dt         data.table output of cox_es_split().
#' @param ref_year   Integer.  Relative year omitted as reference (default -1).
#' @param pool_pre   Integer.  Relative years ≤ this value are pooled.
#' @param pool_post  Integer.  Relative years ≥ this value are pooled.
#' @return Named list:
#'   $dt       : data.table with ry_* columns appended (modified by reference)
#'   $ry_vars  : character vector of all variable names in the model formula

cox_es_build_dummies <- function(dt,
                                  ref_year  = -1L,
                                  pool_pre  = -6L,
                                  pool_post = 15L) {

  ry_all <- sort(unique(dt$rel_year))

  # Individual years: between the tails, excluding reference
  ry_indiv <- sort(ry_all[
    ry_all >  pool_pre  &
    ry_all <  pool_post &
    ry_all != ref_year
  ])

  # Pooled tails
  dt[, `:=`(
    ry_pool_pre  = as.integer(rel_year <= pool_pre)  * texas_treated,
    ry_pool_post = as.integer(rel_year >= pool_post) * texas_treated
  )]
  ry_vars <- c("ry_pool_pre", "ry_pool_post")

  # Individual-year dummies
  for (yr in ry_indiv) {
    vname <- if (yr < 0L) paste0("ry_m", abs(yr)) else paste0("ry_", yr)
    dt[, (vname) := as.integer(rel_year == yr) * texas_treated]
    ry_vars <- c(ry_vars, vname)
  }

  list(dt = dt, ry_vars = sort(ry_vars))
}


# ═══════════════════════════════════════════════════════════════════════════════
# 4.  cox_es_estimate
# ═══════════════════════════════════════════════════════════════════════════════
#' Fit the stratified Cox event-study model.
#'
#' @param dt          data.table with Surv columns and ry_* dummies.
#' @param ry_vars     Character vector of RHS variable names.
#' @param strata_var  Character.  Column name for stratification.
#' @param cluster_var Character.  Column name for cluster-robust SEs.
#' @param ties        Character.  Tie-handling method ("efron" or "breslow").
#' @return Fitted coxph object.

cox_es_estimate <- function(dt,
                             ry_vars,
                             strata_var  = "make_model_tank",
                             cluster_var = "state",
                             ties        = "efron") {

  fml <- as.formula(paste(
    "Surv(t_enter, t_exit, failure) ~",
    paste(ry_vars, collapse = " + "),
    "+ strata(", strata_var, ")"
  ))

  coxph(
    fml,
    data    = dt,
    cluster = dt[[cluster_var]],
    ties    = ties
  )
}


# ═══════════════════════════════════════════════════════════════════════════════
# 5.  cox_es_tidy_coefs
# ═══════════════════════════════════════════════════════════════════════════════
#' Extract, parse, and tidy the event-study coefficient table.
#'
#' @param model      Fitted coxph object from cox_es_estimate().
#' @param ref_year   Integer.  The omitted reference year (added back as HR=1).
#' @param pool_pre   Integer.  Pre-tail pooling boundary.
#' @param pool_post  Integer.  Post-tail pooling boundary.
#' @return data.table with columns: term, coef, robust_se, p, rel_year,
#'         hr, ci_lo, ci_hi, pct_change, pct_ci_lo, pct_ci_hi, period, pooled.

cox_es_tidy_coefs <- function(model,
                               ref_year  = -1L,
                               pool_pre  = -6L,
                               pool_post = 15L) {

  s <- summary(model)$coefficients
  ct <- as.data.table(s, keep.rownames = "term")

  # Rename robustly — column names differ by coxph version
  old_names <- names(ct)
  new_names <- old_names
  new_names[grepl("^coef$|^coef\\b",       old_names, ignore.case = TRUE)][1] <- "coef"
  new_names[grepl("exp\\(coef\\)",          old_names, ignore.case = TRUE)][1] <- "exp_coef"
  new_names[grepl("robust se|se\\(coef\\)", old_names, ignore.case = TRUE)][1] <- "robust_se"
  new_names[grepl("Pr\\(>\\|z\\|\\)",       old_names, ignore.case = TRUE)][1] <- "p"
  setnames(ct, new_names)

  # Parse rel_year from variable name
  ct[, rel_year := fcase(
    term == "ry_pool_pre",        pool_pre,
    term == "ry_pool_post",       pool_post,
    grepl("^ry_m[0-9]+$", term), -as.integer(gsub("ry_m", "", term)),
    grepl("^ry_[0-9]+$",  term),  as.integer(gsub("ry_",  "", term)),
    default = NA_integer_
  )]
  ct <- ct[!is.na(rel_year)]

  # Derived quantities
  ct[, `:=`(
    hr         = exp(coef),
    ci_lo      = exp(coef - 1.96 * robust_se),
    ci_hi      = exp(coef + 1.96 * robust_se),
    pct_change = (exp(coef) - 1) * 100,
    pct_ci_lo  = (exp(coef - 1.96 * robust_se) - 1) * 100,
    pct_ci_hi  = (exp(coef + 1.96 * robust_se) - 1) * 100,
    log_ci_lo  = coef - 1.96 * robust_se,
    log_ci_hi  = coef + 1.96 * robust_se,
    period     = fcase(rel_year <  0L, "Pre",
                       rel_year >= 0L, "Post"),
    pooled     = as.integer(term %in% c("ry_pool_pre", "ry_pool_post"))
  )]

  # Reference row (imposed at zero)
  ref_row <- data.table(
    term       = paste0("ref_year_m", abs(ref_year)),
    coef       = 0,     robust_se = 0,     p         = NA_real_,
    rel_year   = ref_year,
    hr         = 1,     ci_lo     = 1,     ci_hi     = 1,
    pct_change = 0,     pct_ci_lo = 0,     pct_ci_hi = 0,
    log_ci_lo  = 0,     log_ci_hi = 0,
    period     = "Ref",
    pooled     = 0L
  )
  ct <- rbind(ct, ref_row, fill = TRUE)
  setorder(ct, rel_year)

  ct[]
}


# ═══════════════════════════════════════════════════════════════════════════════
# 6.  cox_es_pre_tests
# ═══════════════════════════════════════════════════════════════════════════════
#' Joint Wald χ² tests for pre-period parallel trends.
#'
#' @param model       Fitted coxph object.
#' @param ry_vars     Character vector of all RHS variable names.
#' @param dense_range Integer vector (e.g., 2:6).  The absolute values of the
#'                    "dense" pre-period years to test separately.
#' @return Named list: chi_all, dof_all, p_all, chi_dense, dof_dense, p_dense.

cox_es_pre_tests <- function(model,
                              ry_vars,
                              dense_range = 2:6) {

  # All pre-period variables
  pre_vars_all <- ry_vars[
    ry_vars == "ry_pool_pre" | grepl("^ry_m[0-9]+$", ry_vars)
  ]
  # Keep only those actually present in the model
  pre_vars_all <- intersect(pre_vars_all, names(coef(model)))

  test_all <- car::linearHypothesis(
    model, paste0(pre_vars_all, " = 0"), test = "Chisq"
  )
  chi_all <- test_all[2, "Chisq"]
  dof_all <- length(pre_vars_all)
  p_all   <- test_all[2, "Pr(>Chisq)"]

  # Dense pre-period
  pre_vars_dense <- intersect(
    paste0("ry_m", dense_range),
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
# 7.  cox_es_plot
# ═══════════════════════════════════════════════════════════════════════════════
#' Render the event-study figure.
#'
#' @param coefs       data.table from cox_es_tidy_coefs().
#' @param pre_tests   List from cox_es_pre_tests().
#' @param y_metric    "pct_change" (default) or "log_hr".
#' @param y_cap       Numeric or NULL (auto).  Upper y-axis limit.
#' @param y_floor     Numeric or NULL (auto).  Lower y-axis limit.
#' @param pool_pre    Integer.  For x-axis label rendering.
#' @param pool_post   Integer.  For x-axis label rendering.
#' @param ref_year    Integer.  For subtitle annotation.
#' @param subtitle    Character or NULL.  Extra subtitle line.
#' @return ggplot object.

cox_es_plot <- function(coefs,
                         pre_tests,
                         y_metric   = "pct_change",
                         y_cap      = NULL,
                         y_floor    = NULL,
                         pool_pre   = -6L,
                         pool_post  = 15L,
                         ref_year   = -1L,
                         subtitle   = NULL) {

  coefs <- copy(coefs)

  # ── Select y-axis columns ──────────────────────────────────────────────

  if (y_metric == "log_hr") {
    coefs[, `:=`(y = coef, y_lo = log_ci_lo, y_hi = log_ci_hi)]
    y_label  <- sprintf(
      "Log-Hazard Ratio vs. Year %d", ref_year)
    y_format <- function(x) sprintf("%+.2f", x)
  } else {
    coefs[, `:=`(y = pct_change, y_lo = pct_ci_lo, y_hi = pct_ci_hi)]
    y_label  <- sprintf(
      "Change in Closure Hazard vs. Year %d  (%%)", ref_year)
    y_format <- function(x) paste0(ifelse(x > 0, "+", ""), x, "%")
  }

  # ── Auto y-axis limits from individual-year CIs ───────────────────────
  indiv <- coefs[pooled == 0]
  if (is.null(y_cap))
    y_cap   <- ceiling(max(indiv$y_hi, na.rm = TRUE) / 25) * 25
  if (is.null(y_floor))
    y_floor <- floor(min(indiv$y_lo, na.rm = TRUE) / 25) * 25

  # ── X-axis labels ─────────────────────────────────────────────────────
  all_x  <- sort(unique(coefs$rel_year))
  x_labs <- as.character(all_x)
  x_labs[all_x == pool_pre]  <- paste0("\u2264", pool_pre)
  x_labs[all_x == pool_post] <- paste0("\u2265", pool_post)

  # ── Pre-test annotation ───────────────────────────────────────────────
  pre_label <- sprintf(
    "Pre-period parallel-trends test\nFull pre (%d dof): \u03c7\u00b2=%.2f, p=%.4f\nDense (%d dof): \u03c7\u00b2=%.2f, p=%.4f",
    pre_tests$dof_all,   pre_tests$chi_all,   pre_tests$p_all,
    pre_tests$dof_dense,
    ifelse(is.na(pre_tests$chi_dense), 0, pre_tests$chi_dense),
    ifelse(is.na(pre_tests$p_dense),   1, pre_tests$p_dense)
  )

  # ── Build plot ────────────────────────────────────────────────────────
  p <- ggplot(coefs, aes(x = rel_year, y = y)) +

    annotate("rect",
             xmin = -Inf, xmax = -0.5,
             ymin = -Inf, ymax  =  Inf,
             fill = "grey90", alpha = 0.50) +

    geom_hline(yintercept = 0, linetype = "dashed",
               color = "grey35", linewidth = 0.5) +
    geom_vline(xintercept = -0.5, color = "grey20", linewidth = 0.65) +

    geom_line(aes(group = 1), color = "grey40",
              linewidth = 0.45, alpha = 0.65) +

    geom_errorbar(aes(ymin = y_lo, ymax = y_hi, color = period),
                  width = 0.30, linewidth = 0.45) +

    geom_point(data = coefs[pooled == 0],
               aes(color = period), size = 2.6, shape = 16) +

    geom_point(data = coefs[pooled == 1],
               aes(color = period), size = 3.2, shape = 18) +

    annotate("text",
             x = pool_pre + 0.3, y = y_cap * 0.97,
             label = pre_label, hjust = 0, vjust = 1,
             size = 2.8, color = "grey25", lineheight = 1.15) +

    scale_color_manual(
      values = c(Pre = COL_CTRL, Post = COL_TX, Ref = "black"),
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
      subtitle = subtitle,
      caption  = paste0(
        "\u25C6 = pooled tail; \u25CF = individual year. ",
        "CIs = 95% cluster-robust (state). ",
        "Ref year = ", ref_year, "."
      )
    ) +
    theme_pub() +
    theme(
      panel.grid.major.x = element_blank(),
      plot.margin   = margin(t = 8, r = 12, b = 8, l = 20, unit = "pt"),
      axis.text.x   = element_text(angle = 45, hjust = 1, vjust = 1),
      plot.subtitle = element_text(size = 9, color = "grey30"),
      plot.caption  = element_text(size = 8, color = "grey40",
                                   hjust = 0, margin = margin(t = 6))
    ) +
    coord_cartesian(clip = "off")

  p
}


# ═══════════════════════════════════════════════════════════════════════════════
# 8.  run_cox_event_study  —  ORCHESTRATOR
# ═══════════════════════════════════════════════════════════════════════════════
#' One-call wrapper: generates cuts, splits, builds dummies, estimates,
#' extracts coefficients, runs pre-tests, plots, and optionally saves.
#'
#' @param dt           data.table.  The exact-date counting-process dataset
#'                     (output of S5, i.e. exact_split_df or a subset).
#' @param ref_year     Integer.  Omitted reference year.  Default -1.
#' @param pool_pre     Integer.  Pre-tail pooling boundary.  Default -6.
#' @param pool_post    Integer.  Post-tail pooling boundary. Default 15.
#' @param strata_var   Character.  Column for Cox stratification.
#'                     Default "make_model_tank".
#' @param cluster_var  Character.  Column for cluster-robust SEs.
#'                     Default "state".
#' @param dense_range  Integer vector.  Absolute values of the "dense"
#'                     pre-period years for the focused Wald test.
#'                     Default 2:6.
#' @param y_metric     "pct_change" or "log_hr".
#' @param y_cap        Numeric or NULL.
#' @param y_floor      Numeric or NULL.
#' @param label        Character.  Label used in subtitle and file names.
#' @param save         Logical.  If TRUE, save figure + CSV to OUTPUT_*.
#' @param file_stem    Character or NULL.  File name stem for saved outputs.
#'                     If NULL and save=TRUE, derived from label.
#' @return Named list: model, coefs, pre_tests, plot, es_data, params.

run_cox_event_study <- function(dt,
                                 ref_year    = -1L,
                                 pool_pre    = -6L,
                                 pool_post   = 15L,
                                 strata_var  = "make_model_tank",
                                 cluster_var = "state",
                                 dense_range = 2:6,
                                 y_metric    = "pct_change",
                                 y_cap       = NULL,
                                 y_floor     = NULL,
                                 label       = "Primary",
                                 save        = TRUE,
                                 file_stem   = NULL) {

  # ── Parameter echo ──────────────────────────────────────────────────────
  params <- list(
    ref_year    = ref_year,
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
    "Cox ES [%s]: ref=%d, pool=[%d, %d], strata=%s, cluster=%s, n=%s tanks",
    label, ref_year, pool_pre, pool_post,
    strata_var, cluster_var, fmt_n(params$n_tanks)
  ))

  # ── Step 1: Cuts ────────────────────────────────────────────────────────
  cuts <- cox_es_generate_cuts(
    reform_date = REFORM_DATE,
    pool_pre    = pool_pre,
    pool_post   = pool_post
  )

  # ── Step 2: Split ───────────────────────────────────────────────────────
  es_data <- cox_es_split(dt, cuts)
  log_step(sprintf("  Split: %s rows, %s events",
                   fmt_n(nrow(es_data)), fmt_n(sum(es_data$failure))), 1)

  # ── Step 3: Dummies ─────────────────────────────────────────────────────
  dummy_res <- cox_es_build_dummies(
    es_data,
    ref_year  = ref_year,
    pool_pre  = pool_pre,
    pool_post = pool_post
  )
  es_data <- dummy_res$dt
  ry_vars <- dummy_res$ry_vars
  log_step(sprintf("  Dummies: %d variables (ref omitted: %d)",
                   length(ry_vars), ref_year), 1)

  # ── Step 4: Estimate ────────────────────────────────────────────────────
  model <- cox_es_estimate(
    es_data,
    ry_vars     = ry_vars,
    strata_var  = strata_var,
    cluster_var = cluster_var
  )
  log_step(sprintf("  Estimated: %d params, %s obs, %s events",
                   length(coef(model)),
                   fmt_n(model$n), fmt_n(model$nevent)), 1)

  # ── Step 5: Tidy coefficients ───────────────────────────────────────────
  coefs <- cox_es_tidy_coefs(
    model,
    ref_year  = ref_year,
    pool_pre  = pool_pre,
    pool_post = pool_post
  )

  # ── Step 6: Pre-tests ──────────────────────────────────────────────────
  pre_tests <- cox_es_pre_tests(model, ry_vars, dense_range)
  log_step(sprintf(
    "  Pre-test (full): \u03c7\u00b2(%d)=%.2f, p=%.4f | Dense: \u03c7\u00b2(%d)=%.2f, p=%.4f",
    pre_tests$dof_all, pre_tests$chi_all, pre_tests$p_all,
    pre_tests$dof_dense,
    ifelse(is.na(pre_tests$chi_dense), 0, pre_tests$chi_dense),
    ifelse(is.na(pre_tests$p_dense), 1, pre_tests$p_dense)
  ), 1)

  # ── Step 7: Plot ────────────────────────────────────────────────────────
  p <- cox_es_plot(
    coefs      = coefs,
    pre_tests  = pre_tests,
    y_metric   = y_metric,
    y_cap      = y_cap,
    y_floor    = y_floor,
    pool_pre   = pool_pre,
    pool_post  = pool_post,
    ref_year   = ref_year,
    subtitle   = label
  )

  # ── Step 8: Save ────────────────────────────────────────────────────────
  if (save) {
    if (is.null(file_stem))
      file_stem <- gsub("[^A-Za-z0-9_]", "", gsub(" ", "_", label))

    ggsave(file.path(OUTPUT_FIGURES,
                     sprintf("Figure_Cox_ES_%s.png", file_stem)),
           p, width = 13, height = 6.5, dpi = 300, bg = "white")
    ggsave(file.path(OUTPUT_FIGURES,
                     sprintf("Figure_Cox_ES_%s.pdf", file_stem)),
           p, width = 13, height = 6.5, device = cairo_pdf)
    fwrite(coefs,
           file.path(OUTPUT_TABLES,
                     sprintf("Table_CoxES_%s.csv", file_stem)))
    log_step(sprintf("  Saved: %s (fig + csv)", file_stem), 1)
  }

  # ── Clean up transient dummy columns from es_data ───────────────────────
  # (so returned dataset doesn't carry stale dummies if caller re-uses it)
  # The ry_* columns were added by reference in cox_es_build_dummies;
  # we intentionally leave them for transparency but warn if re-splitting.

  invisible(list(
    model     = model,
    coefs     = coefs,
    pre_tests = pre_tests,
    plot      = p,
    es_data   = es_data,
    params    = params
  ))
}


# ═══════════════════════════════════════════════════════════════════════════════
# 9.  cox_es_compare  —  Multi-spec comparison helper
# ═══════════════════════════════════════════════════════════════════════════════
#' Run multiple event studies with different parameters and return a tidy
#' comparison table of primary estimates and pre-test p-values.
#'
#' @param dt     data.table (exact_split_df or subset).
#' @param specs  Named list of lists, each containing parameter overrides
#'               for run_cox_event_study().  Example:
#'               list(
#'                 "Ref = -1"    = list(),
#'                 "Ref = -2"    = list(ref_year = -2L),
#'                 "Tight pool"  = list(pool_pre = -4L, pool_post = 10L)
#'               )
#' @param save   Logical.  Passed through to each run.
#' @return Named list:
#'   $results : list of run_cox_event_study() outputs (one per spec)
#'   $summary : data.table comparing key metrics across specs

cox_es_compare <- function(dt, specs, save = FALSE) {

  results <- list()
  rows    <- list()

  for (nm in names(specs)) {
    args        <- specs[[nm]]
    args$dt     <- dt
    args$label  <- nm
    args$save   <- save

    res <- do.call(run_cox_event_study, args)
    results[[nm]] <- res

    # Extract the pooled post-period coefficient as the headline
    post_row <- res$coefs[term == "ry_pool_post"]
    if (nrow(post_row) == 0) post_row <- res$coefs[rel_year == max(rel_year)]

    rows[[nm]] <- data.table(
      spec         = nm,
      ref_year     = res$params$ref_year,
      pool_pre     = res$params$pool_pre,
      pool_post    = res$params$pool_post,
      strata       = res$params$strata_var,
      n_tanks      = res$params$n_tanks,
      n_events     = res$model$nevent,
      post_hr      = post_row$hr[1],
      post_coef    = post_row$coef[1],
      post_se      = post_row$robust_se[1],
      post_p       = post_row$p[1],
      pre_p_all    = res$pre_tests$p_all,
      pre_p_dense  = res$pre_tests$p_dense
    )
  }

  list(
    results = results,
    summary = rbindlist(rows)
  )
}
