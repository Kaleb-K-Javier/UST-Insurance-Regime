# TICKET 007 — 02b refactor: vintage HTE (OLS + Cox) + forest figure
# Created: 2026-05-19
# Status: AWAITING_IMPLEMENTATION
# Attempt: 0
# Depends on: 005 (helper file + sample build + wild bootstrap wrappers)

═══════════════════════════════════════════════════
ECONOMIC MOTIVATION
═══════════════════════════════════════════════════
The static stepped DiD (Ticket 005) reports a single pooled ATT.
But the 1998 Texas reform plausibly affected tanks differently
depending on their installation vintage:
  - Pre-1989 tanks: installed before the EPA's December 1988 federal
    standard for new tanks. These are "old, regulatorily grandfathered"
    units that the 1998 state-financial-responsibility reform
    pressures hardest because they were already the marginal
    closure candidates.
  - Post-1988 tanks: installed under the federal standard, generally
    newer, less leak-prone, less exposed to the marginal-closure
    margin.

We test this in two complementary ways:

(1) A single interaction model that splits the ATT into:
    - the post-1988 cohort baseline ATT (β̂_did_term)
    - a pre-1989-cohort differential       (β̂_did:pre89)
    - a derived pre-1989 cohort ATT        (β̂_did + β̂_did:pre89)
    each reported with model SE, wild-bootstrap SE, and
    delta-method SE for the linear combination.

(2) A vintage forest plot that estimates a separate ATT for each
    installation year (1971..1998), with 1995 as the reference
    cohort. This visualizes the full vintage gradient and shows
    whether the binary pre89/post88 split captures the relevant
    heterogeneity or whether the effect is monotonically declining
    with newness.

We do both for OLS and Cox at the main specification (matching
col 7 of the stepped DiD: facility FE + cell × year — for Cox,
Option B: strata(facility) + factor(cell) + factor(year)).

═══════════════════════════════════════════════════
MATH
═══════════════════════════════════════════════════

Setup: same active-at-treatment sample, same cluster level (state),
same cell definitions as Ticket 005. Add:

  pre89_k := 1 if install_yr_int_k ≤ 1988, 0 otherwise

Eq. 1 — OLS HTE interaction:

    y_{kt} = β_post · did_term_{kt}
           + β_int  · did_term_{kt} · pre89_k
           + γ' · X_{kt}
           + φ_i + θ_{c, t} + ε_{kt}

  Coefficients:
    β_post       = post-1988-cohort ATT
    β_int        = pre-1989 differential
    β_pre89      = β_post + β_int (derived; not directly estimated)

  cluster = ~state.

  Implementation:
    closure_event ~ did_term + did_term:pre89_cohort
                  + mandate_release_det + mandate_spill_overfill + mandate_integrity
                  | panel_id + cell_vintage_year_fe

  pre89_cohort itself drops out because it is absorbed by panel_id
  (facility FE), since vintage is facility-invariant in this panel.

Eq. 2 — Cox HTE interaction (cell-stratified, state-as-covariate):

    h(t | k) = h_{0, c(k)}(t)
             · exp( β_post · did_term_k(t)
                  + β_int  · did_term_k(t) · pre89_k
                  + γ' · X_k(t)
                  + η_s · 1{state(k) = s} + η_t · 1{cal_year(t) = t} )

  Same data structure as Ticket 005 Cox (two-episode split at reform).
  pre89_k is time-invariant. The main spec mirrors Ticket 005 col (4):
  cell-level stratification (identifiable because cells contain both
  Texas and control tanks), state and year absorbed as covariates.
  See Ticket 005 spec for why strata(facility) / strata(tank) /
  strata(state) are NOT identifiable in this design.

Eq. 3 — Vintage forest (OLS):

    y_{kt} = sum_{v ≠ 1995} α_v · 1{install_yr_int_k = v} · did_term_{kt}
           + γ' · X_{kt}
           + φ_i + θ_{c, t} + ε_{kt}

  α_v = ATT for installation-year-v cohort, ref = 1995.

  Implementation:
    closure_event ~ i(install_yr_int_factor, did_term, ref = "1995")
                  + mandate_release_det + mandate_spill_overfill + mandate_integrity
                  | panel_id + cell_vintage_year_fe

  Note on fixest semantics: i(vintage, did_term, ref) interacts each
  vintage level with did_term and reports the FULL interaction
  coefficient per level (no base "did_term" term to add). The omitted
  level (1995) has coefficient 0 by construction.

Eq. 4 — Vintage forest (Cox):

    h(t | k) = h_{0, i(k)}(t)
             · exp( sum_{v ≠ 1995} α_v · texas_treated_k · 1{install_yr_int_k = v} · post_t
                  + γ' · X_k(t)
                  + η_c · 1{c(k) = c} + η_t · 1{cal_year(t) = t} )

  Same two-episode Cox split as Ticket 005. Vintage-by-treated
  interactions implemented as multiple did_term-style columns per
  vintage:
    did_term_vint_<v> := texas_treated · post · 1{install_yr_int == v}
  with the v = 1995 column omitted.

Eq. 5 — Delta-method SE for derived pre-89 ATT:

    Var(β̂_pre89) = Var(β̂_post) + Var(β̂_int) + 2 · Cov(β̂_post, β̂_int)
    SE(β̂_pre89)  = sqrt(Var(β̂_pre89))

  using the cluster-robust V̂ from feols (or sandwich-robust V̂ from coxph).

Eq. 6 — Wild-bootstrap SE for derived pre-89 ATT:

  For OLS: fwildclusterboot::boottest accepts a restriction vector R
  for linear hypotheses. To bootstrap β_post + β_int = 0, set
    R = c(did_term = 1, "did_term:pre89_cohort" = 1)
  via the param argument shape that fwildclusterboot supports
  (use a named numeric vector representing the restriction matrix).

  For Cox: wild-score bootstrap on a linear combination is not native
  to the existing run_boot_cox. Bootstrap the two component
  coefficients individually, then either:
    (a) Report the linear combination with delta-method SE (Eq. 5)
        only (acceptable — flag in table notes), OR
    (b) Reconstruct the bootstrap distribution of (β̂_post + β̂_int)
        from the saved per-iteration boot draws stored by
        run_boot_cox. Per Step 5 below, extend run_boot_cox to
        return boot_betas (B × p matrix) and compute SE from
        SD(boot_betas[, "did_term"] + boot_betas[, "did_term:pre89_cohort"]).
  We use option (b) — implementation note in Step 5.

═══════════════════════════════════════════════════
PSEUDOCODE
═══════════════════════════════════════════════════

═══ Step 0 — Extend shared helper file (small) ═══

EDIT FILE: Code/Helpers/reduced_form_utils.R

Append:

  vcov_lincom(model, lincom_vec)
    Input:  model       — fitted regression with vcov() method
            lincom_vec  — named numeric vector (length = # coefs)
                          representing the linear combination
                          (e.g. c(did_term = 1, "did_term:pre89_cohort" = 1))
    Output: list(est = num, se = num, ci_lo = num, ci_hi = num)
            using model's vcov.

    Operations:
      cn <- names(lincom_vec)
      stopifnot(all(cn %in% names(coef(model))))
      L <- numeric(length(coef(model)))
      names(L) <- names(coef(model))
      L[cn] <- lincom_vec[cn]
      V <- vcov(model)
      est <- sum(L * coef(model))
      se  <- sqrt(as.numeric(t(L) %*% V %*% L))
      list(est = est, se = se,
           ci_lo = est - 1.96 * se,
           ci_hi = est + 1.96 * se)

  Modify run_wcb_cox (from Ticket 005) to additionally return the
  raw bootstrap draws under the name `boot_betas` (a B × p matrix
  with col names = coef names). This enables linear-combo
  re-bootstrap without re-running. The existing 02b run_boot_cox
  already stores per-iteration draws internally; expose them in
  the return list.

═══ Step 1 — Create main script ═══

NEW FILE: Code/Analysis/02e_Vintage_HTE.R

Layout:
  1. Logging block
  2. Library loads (data.table, fixest, survival, fwildclusterboot,
                    ggplot2, here)
  3. source(reduced_form_utils.R)
  4. Load matched_tanks_birth_cem
  5. data_active <- build_active_at_treatment_sample(...)
  6. cox_active  <- build_active_cox_split(data_active)
  7. Log sample sizes (rows, tanks, # closures, # pre89 facilities,
                       # post88 facilities)

═══ Step 2 — Fit OLS HTE interaction (main spec only) ═══

  ctrl_rhs <- "mandate_release_det + mandate_spill_overfill + mandate_integrity"

  m_hte_ols <- feols(
    as.formula(sprintf(
      "closure_event ~ did_term + did_term:pre89_cohort + %s | panel_id + cell_vintage_year_fe",
      ctrl_rhs)),
    data = data_active, cluster = ~state)

  Log: coef and SE for did_term, did_term:pre89_cohort. Log N.

assert: "did_term"             %in% names(coef(m_hte_ols))
assert: "did_term:pre89_cohort" %in% names(coef(m_hte_ols))

═══ Step 3 — Wild cluster bootstrap on β_post, β_int, β_pre89 (OLS) ═══

  boot_ols_did   <- run_wcb_ols(m_hte_ols, param = "did_term",
                                 B = 9999L, seed = 20260519L)
  boot_ols_int   <- run_wcb_ols(m_hte_ols, param = "did_term:pre89_cohort",
                                 B = 9999L, seed = 20260519L)

  # Linear combination via fwildclusterboot::boottest with R argument
  boot_ols_pre89 <- {
    requireNamespace("fwildclusterboot")
    set.seed(20260519L)
    bt <- fwildclusterboot::boottest(
            object  = m_hte_ols,
            clustid = "state",
            B       = 9999L,
            type    = "rademacher",
            R       = c(did_term = 1, `did_term:pre89_cohort` = 1),
            seed    = 20260519L,
            impose_null = TRUE)
    ci_lo <- bt$conf_int[1]; ci_hi <- bt$conf_int[2]
    list(SE_boot = (ci_hi - ci_lo) / (2 * 1.96),
         CI_lo = ci_lo, CI_hi = ci_hi,
         p_boot = bt$p_val, B = 9999L, n_clusters = bt$N_G[1])
  }

  Log all three.

═══ Step 4 — Fit Cox HTE interaction (main spec mirror) ═══

  m_hte_cox <- coxph(
    Surv(t_enter, t_exit, failure) ~ did_term + did_term:pre89_cohort
      + mandate_release_det + mandate_spill_overfill + mandate_integrity
      + factor(state) + factor(panel_year) + strata(cell_id),
    data = cox_active, cluster = state, ties = "efron", model = TRUE)

  Log: coef + robust SE for did_term, did_term:pre89_cohort.

assert: same as OLS — both interaction coefficients estimated

═══ Step 5 — Wild score bootstrap on β_post, β_int, β_pre89 (Cox) ═══

  boot_cox_full <- run_wcb_cox(m_hte_cox, data = cox_active,
                                param = "did_term",
                                B = 9999L, seed = 20260519L)
  # boot_cox_full now returns boot_betas per the Step 0 extension

  # SE for the interaction (single coef)
  boot_cox_int <- list(
    SE_boot = sd(boot_cox_full$boot_betas[, "did_term:pre89_cohort"]),
    CI_lo   = quantile(boot_cox_full$boot_betas[, "did_term:pre89_cohort"], 0.025),
    CI_hi   = quantile(boot_cox_full$boot_betas[, "did_term:pre89_cohort"], 0.975),
    p_boot  = mean(abs(boot_cox_full$boot_betas[, "did_term:pre89_cohort"])
                   >= abs(coef(m_hte_cox)["did_term:pre89_cohort"])),
    B = 9999L, n_clusters = boot_cox_full$n_clusters
  )

  # SE for the linear combo via Eq. 6 option (b)
  linear_combo <- boot_cox_full$boot_betas[, "did_term"] +
                   boot_cox_full$boot_betas[, "did_term:pre89_cohort"]
  obs_combo <- coef(m_hte_cox)["did_term"] + coef(m_hte_cox)["did_term:pre89_cohort"]
  boot_cox_pre89 <- list(
    SE_boot = sd(linear_combo),
    CI_lo   = quantile(linear_combo, 0.025),
    CI_hi   = quantile(linear_combo, 0.975),
    p_boot  = mean(abs(linear_combo) >= abs(obs_combo)),
    B = 9999L, n_clusters = boot_cox_full$n_clusters
  )

  Log all three.

═══ Step 6 — Compute delta-method SEs for pre-89 ATT (both models) ═══

  lc_ols <- vcov_lincom(m_hte_ols,
                        c(did_term = 1, `did_term:pre89_cohort` = 1))
  lc_cox <- vcov_lincom(m_hte_cox,
                        c(did_term = 1, `did_term:pre89_cohort` = 1))

═══ Step 7 — Fit vintage forest models (OLS + Cox) ═══

OLS:
  data_active[, install_yr_factor := factor(install_yr_int)]
  m_vintage_ols <- feols(
    closure_event ~ i(install_yr_factor, did_term, ref = "1995")
                  + mandate_release_det + mandate_spill_overfill + mandate_integrity
                  | panel_id + cell_vintage_year_fe,
    data = data_active, cluster = ~state)

Cox:
  # Build per-vintage did_term columns in cox_active
  vintages <- sort(unique(cox_active$install_yr_int))
  vintages_no_ref <- setdiff(vintages, 1995L)
  for (v in vintages_no_ref) {
    var_name <- sprintf("did_term_vint_%d", v)
    cox_active[, (var_name) := did_term * as.integer(install_yr_int == v)]
  }
  cox_terms <- paste(sprintf("did_term_vint_%d", vintages_no_ref),
                     collapse = " + ")
  m_vintage_cox <- coxph(
    as.formula(sprintf(
      "Surv(t_enter, t_exit, failure) ~ %s + mandate_release_det + mandate_spill_overfill + mandate_integrity + factor(state) + factor(panel_year) + strata(cell_id)",
      cox_terms)),
    data = cox_active, cluster = state, ties = "efron", model = TRUE)

  Log: # vintage coefficients in each. No bootstrap on the vintage
  forest (asymptotic SE suffices — flag in figure caption).

═══ Step 8 — Build forest data.tables ═══

For OLS:
  vintage_ols_dt <- data.table(
    term     = names(coef(m_vintage_ols)),
    estimate = coef(m_vintage_ols),
    se       = se(m_vintage_ols)
  )[grepl("^install_yr_factor::\\d+:did_term$", term)]
  vintage_ols_dt[, install_yr := as.integer(sub(
    "^install_yr_factor::(\\d+):did_term$", "\\1", term))]
  # Append ref row at 1995
  vintage_ols_dt <- rbind(
    vintage_ols_dt[, .(install_yr, estimate, se,
                       ci_lo = estimate - 1.96 * se,
                       ci_hi = estimate + 1.96 * se,
                       is_ref = FALSE)],
    data.table(install_yr = 1995L, estimate = 0, se = 0,
               ci_lo = NA_real_, ci_hi = NA_real_, is_ref = TRUE)
  )[order(install_yr)]
  vintage_ols_dt[, pre89 := install_yr <= 1988L]

For Cox: analogous, parsing names matching "^did_term_vint_(\\d+)$"

═══ Step 9 — Render forest figures ═══

  plot_vintage_forest <- function(dt, y_label) {
    # ... matches the existing 02b vintage-forest style:
    # blue points for pre89, red points for post88, hollow diamond at ref,
    # vertical dotted line at install_yr = 1988.5, errorbars, ref text annotation
    # See lines 4216-4306 of 02b for the template.
    ggplot(dt, aes(x = install_yr, y = estimate)) +
      geom_hline(yintercept = 0, colour = "grey55",
                 linetype = "dashed", linewidth = 0.45) +
      geom_vline(xintercept = 1988.5, colour = "grey35",
                 linetype = "dotted", linewidth = 0.55) +
      geom_errorbar(data = dt[is_ref == FALSE],
                    aes(ymin = ci_lo, ymax = ci_hi,
                        colour = ifelse(pre89, "pre89", "post88")),
                    width = 0.35, linewidth = 0.6) +
      geom_point(data = dt[is_ref == FALSE],
                 aes(colour = ifelse(pre89, "pre89", "post88"),
                     fill   = ifelse(pre89, "pre89", "post88")),
                 size = 2.8, shape = 21, stroke = 1.1) +
      geom_point(data = dt[is_ref == TRUE],
                 colour = "grey40", fill = "white",
                 size = 3.2, shape = 23, stroke = 1.1) +
      annotate("text", x = 1995.3, y = 0.003,
               label = "ref.", hjust = 0, size = 2.8,
               colour = "grey40", family = "Times") +
      scale_colour_manual(
        values = c(pre89 = "#3A6BBF", post88 = "#BF3A3A"),
        labels = c(pre89  = "Existing tanks (pre-Dec 1988 EPA rule)",
                   post88 = "New tanks (post-Dec 1988 EPA rule)"),
        name = NULL) +
      scale_fill_manual(
        values = c(pre89 = "#3A6BBF", post88 = "#BF3A3A"),
        guide  = "none") +
      labs(x = "Installation year (vintage cohort)", y = y_label) +
      theme_classic(base_size = 11, base_family = "Times") +
      theme(legend.position = "bottom",
            axis.line  = element_line(colour = "black", linewidth = 0.4),
            axis.ticks = element_line(colour = "black", linewidth = 0.3),
            panel.grid.major.y = element_line(colour = "grey92", linewidth = 0.3),
            panel.grid.minor = element_blank(),
            plot.margin = margin(8, 12, 8, 8))
  }

  p_vintage_ols <- plot_vintage_forest(vintage_ols_dt, "ATT (LPM)")
  p_vintage_cox <- plot_vintage_forest(vintage_cox_dt, "ATT (log hazard ratio)")

  save_gg(p_vintage_ols, "Fig_Vintage_Forest_OLS", width = 11, height = 6)
  save_gg(p_vintage_cox, "Fig_Vintage_Forest_Cox", width = 11, height = 6)

═══ Step 10 — Render HTE summary table ═══

Hand-built two-block table:
  Block 1 — header row "OLS (LPM) | Cox (log HR)"
  Block 2 — three coefficient rows:
    Row "Post-1988 cohort ATT (β_post)":
      OLS: est_post_ols / (se_model_post_ols) / [se_boot_post_ols]
      Cox: est_post_cox / (se_model_post_cox) / [se_boot_post_cox]
    Row "Pre-1989 differential (β_int)":
      OLS: est_int_ols  / (se_model_int_ols)  / [se_boot_int_ols]
      Cox: est_int_cox  / (se_model_int_cox)  / [se_boot_int_cox]
    Row "Pre-1989 cohort ATT (β_pre89 = β_post + β_int)":
      OLS: est_pre89_ols / (se_lc_ols)        / [se_boot_pre89_ols]
      Cox: est_pre89_cox / (se_lc_cox)        / [se_boot_pre89_cox]

Each numeric cell uses the 3-row format from Ticket 005 (est /
(model SE) / [boot SE]). Adapt the LaTeX template from Step 7-8 of
Ticket 005.

Bottom rows:
  "Observations" / "Events" / "Wild bootstrap B"
  "Specification"
    OLS: facility FE + cell × year FE | mandate controls | cluster=state
    Cox: strata(facility) + factor(cell) + factor(year) | mandate controls | cluster=state

Notes section:
  "Heterogeneous-effect estimates for the 1998 Texas reform by
  installation-vintage cohort, at the main specification (col 7 of
  T_Stepped_DiD_*.tex). Coefficient row shows estimate β̂; model row
  shows analytic cluster-robust SE in parentheses; bootstrap row
  shows wild cluster (OLS) or wild score (Cox) bootstrap SE in
  brackets, B = 9,999, cluster = state. The pre-1989 cohort ATT
  is a derived linear combination (β_post + β_int); its bootstrap
  SE is computed by summing the per-iteration bootstrap draws of
  the two component coefficients (Cox) or via boottest's R-vector
  argument (OLS). 'Pre-1989' means installation year ≤ 1988
  (before the EPA December 1988 federal tank standard). Sample:
  active-at-treatment, unweighted."

Write to: Output/Tables/T_HTE_Vintage.tex

═══ Step 11 — Save fitted-model artifacts ═══

  saveRDS(
    list(
      m_hte_ols     = m_hte_ols,
      m_hte_cox     = m_hte_cox,
      m_vintage_ols = m_vintage_ols,
      m_vintage_cox = m_vintage_cox,
      boot = list(
        ols_post  = boot_ols_did,
        ols_int   = boot_ols_int,
        ols_pre89 = boot_ols_pre89,
        cox_post  = list(SE_boot = boot_cox_full$SE_boot,
                          CI_lo = boot_cox_full$CI_lo,
                          CI_hi = boot_cox_full$CI_hi,
                          p_boot = boot_cox_full$p_boot,
                          B = 9999L),
        cox_int   = boot_cox_int,
        cox_pre89 = boot_cox_pre89,
        cox_full  = boot_cox_full   # includes boot_betas matrix
      ),
      lc = list(ols = lc_ols, cox = lc_cox),
      vintage_ols_dt = vintage_ols_dt,
      vintage_cox_dt = vintage_cox_dt,
      sample_meta = list(
        n_rows         = nrow(data_active),
        n_tanks        = uniqueN(data_active$tank_panel_id),
        n_facilities   = uniqueN(data_active$panel_id),
        n_pre89_facs   = uniqueN(data_active[pre89_cohort == 1L, panel_id]),
        n_post88_facs  = uniqueN(data_active[pre89_cohort == 0L, panel_id]),
        seed           = 20260519L,
        B              = 9999L,
        sample_label   = "active_at_treatment",
        ticket         = "007",
        built_on       = Sys.time()
      )
    ),
    file.path(here::here("Output", "Estimation_Results"),
              "Vintage_HTE_Fits_active_at_treatment.rds")
  )

═══ Step 12 — Save CSVs ═══

(a) HTE coefficient summary CSV:
  fwrite(data.table(
    model       = rep(c("OLS", "Cox"), each = 3),
    quantity    = rep(c("post88_att", "pre89_differential", "pre89_att"), 2),
    estimate    = c(coef(m_hte_ols)["did_term"],
                    coef(m_hte_ols)["did_term:pre89_cohort"],
                    lc_ols$est,
                    coef(m_hte_cox)["did_term"],
                    coef(m_hte_cox)["did_term:pre89_cohort"],
                    lc_cox$est),
    se_model    = c(se(m_hte_ols)["did_term"],
                    se(m_hte_ols)["did_term:pre89_cohort"],
                    lc_ols$se,
                    sqrt(diag(vcov(m_hte_cox)))["did_term"],
                    sqrt(diag(vcov(m_hte_cox)))["did_term:pre89_cohort"],
                    lc_cox$se),
    se_boot     = c(boot_ols_did$SE_boot,
                    boot_ols_int$SE_boot,
                    boot_ols_pre89$SE_boot,
                    boot_cox_full$SE_boot,
                    boot_cox_int$SE_boot,
                    boot_cox_pre89$SE_boot),
    p_boot      = c(boot_ols_did$p_boot,
                    boot_ols_int$p_boot,
                    boot_ols_pre89$p_boot,
                    boot_cox_full$p_boot,
                    boot_cox_int$p_boot,
                    boot_cox_pre89$p_boot)
  ), file.path(OUTPUT_TABLES, "T_HTE_Vintage_Summary.csv"))

(b) Vintage forest CSV:
  fwrite(rbind(
    cbind(model = "OLS", vintage_ols_dt),
    cbind(model = "Cox", vintage_cox_dt)
  ), file.path(OUTPUT_TABLES, "T_Vintage_Forest_Coefficients.csv"))

═══ Step 13 — Print summary block & close ═══

Per CLAUDE.md output rules:
  Model | Quantity | β̂ | SE_model | SE_boot | p_boot
followed by OUTPUT SUMMARY footer.

═══════════════════════════════════════════════════
R-IMPLEMENTATION NOTES
═══════════════════════════════════════════════════
- Script path: Code/Analysis/02e_Vintage_HTE.R
- Sources Code/Helpers/reduced_form_utils.R
- Required packages: data.table, fixest, survival, fwildclusterboot,
  ggplot2, here
- here::here() for all paths
- Hard error propagation only
- Parallel: same as Ticket 005 — set BOOT_NTHREADS top of script,
  pass to boottest, register doParallel for run_wcb_cox
- The Cox vintage forest fit will have many parameters (one per
  vintage × did_term, plus factor(cell), factor(year)). Expect
  ~30-60 min on the server.
- Implementation note for fwildclusterboot's R argument syntax:
  pass R as a NAMED numeric vector with names matching coefficient
  names exactly (including the `:` in interaction terms). If the
  package version on the server differs in API, fall back to
  building the restriction matrix and passing it as R = matrix.
- Do NOT delete any vintage data columns added in Step 7 from
  cox_active before saving the .rds — they need to be reproducible
  on reload.

═══════════════════════════════════════════════════
DELIVERABLES — ENUMERATED
═══════════════════════════════════════════════════

(1) EDIT FILE: Code/Helpers/reduced_form_utils.R
    Adds:
      vcov_lincom(model, lincom_vec) → list
    Modifies:
      run_wcb_cox — now also returns `boot_betas` (B × p matrix
                    with col names = coef names) in its output list
    No new I/O on source.

(2) NEW FILE: Code/Analysis/02e_Vintage_HTE.R
    Per Steps 1-13. Runnable end-to-end from a fresh R session.

(3) Output/Tables/T_HTE_Vintage.tex
    Hand-built two-block, three-coefficient HTE summary table per
    Step 10. Booktabs. 3-row cell format. Notes section per Step 10
    wording. Must compile in standard article preamble.

(4) Output/Tables/T_HTE_Vintage_Summary.csv
    6 rows (3 OLS + 3 Cox), 5 columns: model, quantity, estimate,
    se_model, se_boot, p_boot. Quantity values: "post88_att",
    "pre89_differential", "pre89_att".

(5) Output/Tables/T_Vintage_Forest_Coefficients.csv
    Long-format. Columns: model (chr "OLS"/"Cox"), install_yr (int),
    estimate, se, ci_lo, ci_hi, is_ref (logical), pre89 (logical).
    Approx 2 × 28 = 56 rows depending on vintage range present.

(6) Output/Figures/Fig_Vintage_Forest_OLS.{pdf,png}
(7) Output/Figures/Fig_Vintage_Forest_Cox.{pdf,png}
    11in × 6in. Times font. Blue points pre89, red post88, hollow
    diamond at 1995 ref, dotted vertical line at x = 1988.5,
    error bars at 95%. Y-label "ATT (LPM)" or "ATT (log hazard
    ratio)". x-label "Installation year (vintage cohort)". Legend
    at bottom labeled "Existing tanks (pre-Dec 1988 EPA rule)" vs
    "New tanks (post-Dec 1988 EPA rule)".

(8) Output/Estimation_Results/Vintage_HTE_Fits_active_at_treatment.rds
    R list per Step 11. Required top-level names:
      m_hte_ols, m_hte_cox, m_vintage_ols, m_vintage_cox,
      boot (sublist with 7 sub-elements),
      lc (sublist with 2 sub-elements: ols, cox),
      vintage_ols_dt, vintage_cox_dt, sample_meta

(9) Log file: logs/02e_Vintage_HTE_<TIMESTAMP>.log

═══════════════════════════════════════════════════
ACCEPTANCE CRITERIA — MECHANICAL
═══════════════════════════════════════════════════

HELPER EXTENSIONS (1):
- [ ] vcov_lincom exists after sourcing
- [ ] run_wcb_cox return includes element `boot_betas` (matrix)
- [ ] All previously-added helper functions still exist (no regression)
- [ ] Sourcing has no I/O side effects

MAIN SCRIPT (2):
- [ ] 02e_Vintage_HTE.R exists; runs end-to-end
- [ ] No tryCatch returning NULL; no try(silent = TRUE)
- [ ] Uses here::here()

FITS:
- [ ] m_hte_ols is fixest with coef names including "did_term" and
      "did_term:pre89_cohort"
- [ ] m_hte_cox is coxph with same coef names
- [ ] m_vintage_ols is fixest with ≥ 20 install_yr_factor::*:did_term coefs
- [ ] m_vintage_cox is coxph with ≥ 20 did_term_vint_* coefs

BOOTSTRAP:
- [ ] All 7 boot sub-elements have finite SE_boot > 0
- [ ] B = 9999 in every bootstrap call
- [ ] boot_cox_full$boot_betas is a 9999 × p matrix with col names
- [ ] boot_cox_pre89$SE_boot ≈ sd(boot_betas[,"did_term"] + boot_betas[,"did_term:pre89_cohort"])
      to within 1e-12

DELIVERABLES (3)-(9):
- [ ] T_HTE_Vintage.tex exists and compiles
- [ ] T_HTE_Vintage_Summary.csv has 6 rows + header
- [ ] T_Vintage_Forest_Coefficients.csv has 2 × N_vintages rows + header
- [ ] Both forest figures exist as .pdf and .png
- [ ] .rds loads with all 9 named top-level elements
- [ ] Log contains "OUTPUT SUMMARY"

═══════════════════════════════════════════════════
ATTEMPT LOG
═══════════════════════════════════════════════════
[Filled by reviewer after each R1 attempt.]

### Attempt 1 — [DATE]
Transcript: 007_transcript.txt
Result: [PASS | TRANSLATION_FAIL | PSEUDOCODE_FAIL]
