# TICKET 005 — 02b refactor: stepped DiD (OLS + Cox), wild bootstrap inference
# Created: 2026-05-19
# Status: PASS (2026-05-20, attempt 4)
# Attempt: 4
# Reviewer: Opus
# Final design: 7-col OLS + 4-col Cox (Cox revised down from 7 during attempts 2-4
#               due to identification/numerical issues unique to single-treated-state
#               DiD on this data; final design is the maximally-identifiable Cox
#               stepped table). All deviations documented in code + table notes.

═══════════════════════════════════════════════════
ECONOMIC MOTIVATION
═══════════════════════════════════════════════════
The reduced-form tank-closure analysis currently lives in
Code/Analysis/02b_tank_closure_analysis.R (7,964 lines), with two
separate stepwise tables (S5 OLS / S6 Cox) that disagree on column
layout, weighting, and sample. We are consolidating around ONE
stepped design, applied symmetrically to OLS and Cox, on a single
canonical sample (active-at-treatment, UNWEIGHTED).

The stepped table answers: how does the estimated effect of Texas's
1998 financial-responsibility reform on tank closure change as we
progressively absorb confounders? Reading left to right, the
specification controls for time-varying state-level differences
(col 1), match-cell composition (col 2), facility and tank
heterogeneity (cols 3-4), then collapses the time + match-cell
controls into cell-specific time paths (cols 5-7), with the final
column (7) being the main specification: facility FE absorbing all
time-invariant facility characteristics, plus cell × year FE forcing
the comparison to occur within the same make-model × vintage × year.

We report each effect under both (a) analytic cluster-robust SE and
(b) a bootstrap SE that handles the small number of clusters (18
states): wild cluster bootstrap for OLS, wild score bootstrap for
Cox. The 3-row cell format (estimate / model SE / bootstrap SE) is
chosen to make the comparison visually unambiguous in a single table.

This is Ticket 005 of a 5-ticket refactor:
  005 (this) — stepped DiD tables (OLS + Cox)
  006        — event studies (OLS + Cox, ref = −1 and −2)
  007        — vintage HTE + forest figure
  008        — HonestDiD (raw-package calls, .rds outputs only)
  009        — 02a touch-up (descriptive vs LUST DiD section split)

═══════════════════════════════════════════════════
MATH
═══════════════════════════════════════════════════

Setup:
  i      indexes facility (panel_id)
  k      indexes tank (tank_panel_id)
  s      indexes state
  c      indexes match-cell = make_model_noage × install_yr_int
  t      indexes calendar year (panel_year)
  T*     := 1998 (Texas reform: 1998-12-22 ≈ year 1998)

Outcome:
  y_{kt} := closure_event_{kt} ∈ {0, 1}
            (LPM for OLS; failure indicator at observed exit time for Cox)

Treatment indicator:
  did_term_{kt} := texas_treated_k · 1{t ≥ T*}

Mandate controls (3 columns from existing panel; same as 02b):
  X_{kt} := (mandate_release_det, mandate_spill_overfill, mandate_integrity)_{kt}

Sample (single, throughout 02b refactor):
  S_active := {k : install_yr_k < 1999  AND  facility_i(k) had ≥ 1 open or
                                              newly installed tank on 1998-12-22
                                              AND cem_weight_k > 0 (for cell-support
                                              filtering only; CEM weights are NOT
                                              used in any regression)}
  Built from matched_tanks_birth_cem (the standard birth-CEM-matched panel).

Cell definitions (R variable names in parentheses):
  cell_id_kt          := match-cell   = make_model_noage × install_yr_int   (cell_id)
  cell_year_fe_kt     := cell_id × t  = panel_year × make_model_noage × install_yr_int
                                                                            (cell_vintage_year_fe)

Eq. 1 — OLS LPM, generic form:
    y_{kt} = β · did_term_{kt} + γ' · X_{kt} + FE_{kt} + ε_{kt}

  All 7 OLS specs use the same y, did_term, X. They differ only in
  FE_{kt}. SEs clustered at state level (G = 18 clusters) throughout.

  Every specification absorbs state-level baseline differences,
  either explicitly via α_s (cols 1, 2, 5) or implicitly through a
  finer nested unit FE (facility / tank, cols 3, 4, 6, 7). This is
  required for a valid T/C comparison: without it, cells alone would
  mix Texas tanks with control-state tanks at different baselines.

  Col 1: FE = α_s + δ_t                     (state + year)
  Col 2: FE = α_s + μ_c + δ_t               (state + cell + year)
  Col 3: FE = φ_i + δ_t                     (facility + year — facility absorbs state)
  Col 4: FE = ψ_k + δ_t                     (tank + year — tank absorbs state)
  Col 5: FE = α_s + θ_{c,t}                 (state + cell × year)
  Col 6: FE = ψ_k + θ_{c,t}                 (tank + cell × year — tank absorbs state)
  Col 7: FE = φ_i + θ_{c,t}                 (facility + cell × year — MAIN; facility absorbs state)

Eq. 2 — Cox proportional-hazards mirror:
    h(t | k) = h_{0,strat}(t) · exp(β · did_term_k(t) + γ' · X_k(t) + Z_k(t)'η)

  where Surv data are built as two-episode splits at the reform date
  (1998-12-22), so did_term_k(t) is 0 in episode 1 (pre-reform) and
  texas_treated_k in episode 2 (post-reform). Same for X_{kt}.

  CRITICAL — Cox identification in single-treated-state DiD:
  Cox CANNOT mirror the unit-FE OLS columns (3, 4, 6, 7's facility/tank
  absorption) via strata(unit). Within any unit-level stratum (state,
  facility, or tank), did_term is a deterministic step function of
  calendar time, so the partial-likelihood score contribution for β
  is zero and β is unidentified. The CEM-matched cell IS identifiable
  as a stratum because cells contain both Texas and control tanks at
  risk simultaneously. We therefore report Cox in 4 distinct
  identifiable specifications mapped to the OLS columns:

  Cox col | OLS analogue(s)  | Cox RHS (after did_term + controls)
  --------+------------------+--------------------------------------------------
  (1)     | OLS (1)          | + factor(state)
  (2)     | OLS (2, 3, 4)    | + factor(state) + factor(cell_id)
  (3)     | OLS (5, 6)       | + factor(state) + strata(cell_id)
  (4)     | OLS (7) — MAIN   | + factor(state) + strata(cell_id) + factor(panel_year)

  All use ties = "efron", cluster = state. State-level absorption is
  enforced in every Cox spec via factor(state), mirroring the same
  enforcement in OLS cols 2 and 5.

  Why OLS cols 3, 4, 6 collapse into Cox cols 2 or 3: Cox cannot
  identify additional within-unit variation beyond what state + cell
  provide in a single-treated-state design. The asymmetry is
  intrinsic to the estimator combination, not a spec choice.

Eq. 3 — Cluster-robust analytic SE (model SE, row 2 of each cell):
  OLS:  V̂(β̂) from feols(..., cluster = ~state) — CR1 by default
  Cox:  V̂(β̂) from coxph(..., cluster = state, ties = "efron")
        — robust sandwich (Lin & Wei 1989)

Eq. 4 — Wild bootstrap SE (bootstrap SE, row 3 of each cell):
  OLS:  Wild cluster bootstrap (Cameron-Gelbach-Miller 2008,
        Roodman et al. 2019) via fwildclusterboot::boottest().
        - Rademacher weights
        - B = 9,999 reps
        - cluster = state, single restriction β = 0
        - return: 95% CI; report SE_boot = (CI_upper − CI_lower) / (2 · 1.96)
  Cox:  Wild score bootstrap (Kline & Santos 2012; existing
        run_boot_cox in 02b around line 857).
        - cluster = state
        - B = 9,999 reps
        - same seed as OLS for reproducibility
        - return: SE_boot directly from SD of bootstrap β draws

Bootstrap config (shared):
  B               = 9999
  seed_global     = 20260519L  (today, per ticket creation date)
  rademacher both
  cluster         = state

═══════════════════════════════════════════════════
PSEUDOCODE
═══════════════════════════════════════════════════

═══ Step 0 — Create shared reduced-form helper file ═══

NEW FILE: Code/Helpers/reduced_form_utils.R

Copy/factor out of 02b the following utilities. Subsequent tickets
(006-008) will source this file. Do not duplicate functionality
across the 02c-02f scripts.

Contents (mechanically copied from 02b unless noted):
  - log_step(msg)                            [from 02b S1]
  - fmt_n(x)                                 [from 02b S1]
  - save_gg(p, stem, width = 7, height = 5)  [from 02b S3, line ~360]
      saves both .pdf and .png to OUTPUT_FIGURES
  - write_tex(lines, filename)               [from 02b S3, line ~390]
      writes character vector to OUTPUT_TABLES/filename
  - REFORM_DATE      = as.Date("1998-12-22")
  - REFORM_DAYS      = as.integer(REFORM_DATE - as.Date("1985-01-01"))
                       [matches existing 02b setup]
  - CONTROL_STATES   (the 17-state vector from 02b S1)
  - OUTPUT_TABLES    = here::here("Output", "Tables")
  - OUTPUT_FIGURES   = here::here("Output", "Figures")

NEW HELPERS:

  build_active_at_treatment_sample(matched_panel)
    Input:  matched_panel — data.table with the standard matched_tanks_birth_cem
                            schema (must contain: panel_id, tank_panel_id,
                            panel_year, install_yr_int, cem_weight,
                            closure_event, texas_treated, state,
                            make_model_noage, mandate_*, did_term).
    Output: data.table with all rows for the active-at-treatment sample,
            with these additional columns:
              cell_id              := .GRP by (make_model_noage, install_yr_int)
              cell_vintage_year_fe := .GRP by (panel_year, make_model_noage,
                                                install_yr_int)
              rel_year             := as.integer(panel_year) - 1998L
              pre89_cohort         := as.integer(install_yr_int <= 1988L)

    Operations:
      d <- matched_panel[install_yr_int < 1999L & cem_weight > 0]
      tanks_open_1998      <- d[panel_year == 1998L & closure_event == 0L,
                                 unique(panel_id)]
      tanks_installed_1998 <- d[install_yr_int == 1998L, unique(panel_id)]
      facilities_active    <- union(tanks_open_1998, tanks_installed_1998)
      out <- d[panel_id %in% facilities_active]
      out[, cell_id              := .GRP, by = .(make_model_noage,
                                                  install_yr_int)]
      out[, cell_vintage_year_fe := .GRP, by = .(panel_year,
                                                  make_model_noage,
                                                  install_yr_int)]
      out[, rel_year             := as.integer(panel_year) - 1998L]
      out[, pre89_cohort         := as.integer(install_yr_int <= 1988L)]
      out[]

    assert (postconditions):
      stopifnot(nrow(out) > 0)
      stopifnot(uniqueN(out$state) >= 18L)
      stopifnot(uniqueN(out$panel_id) > 0L)
      stopifnot(all(out$cem_weight > 0))   # support filter only

  build_active_cox_split(active_panel, reform_cut = REFORM_DAYS)
    Input:  active_panel — output of build_active_at_treatment_sample
            reform_cut   — integer days from origin (1985-01-01) to reform
    Output: data.table of two-episode Cox splits:
            cols: tank_panel_id, panel_id, state, install_yr_int,
                  make_model_noage, cell_id, t_enter, t_exit, failure,
                  did_term, mandate_release_det, mandate_spill_overfill,
                  mandate_integrity, texas_treated

    Operations: mirror existing 02b build_cox_split (line ~596) but use
    active_panel as the source. Spell out:
      1. From active_panel collapse to one row per tank with: install
         date, exit date (= last observed panel_year if no closure;
         year of closure_event=1 otherwise), failure flag.
      2. Compute t_install_days, t_exit_days relative to 1985-01-01.
      3. Build two-episode split: episode 1 = [t_install_days,
         min(t_exit, reform_cut)) with did_term = 0;
         episode 2 = [reform_cut, t_exit_days) with did_term =
         texas_treated, IF reform_cut < t_exit_days.
      4. failure = 1 ONLY in the episode containing the exit, 0 otherwise.
      5. Carry forward mandate_* values applicable per episode (use the
         panel_year values for the year-of-episode-end, then verify
         pre-reform values are zero — same convention as 02b).
      6. Attach cell_id (mm × vintage) for col 6,7 covariate use.

    assert: every tank_panel_id present in active_panel appears in output
            with ≥ 1 episode; failure indicators sum to (# closures in
            active_panel).

  run_wcb_ols(model, param = "did_term", B = 9999L, seed = 20260519L)
    Input:  model — fixest object fit with cluster = ~state
            param — name of parameter to bootstrap (default "did_term")
            B, seed — wild-cluster-bootstrap config
    Output: list(SE_boot = num, CI_lo = num, CI_hi = num,
                 p_boot = num, B = int, n_clusters = int)

    Operations:
      requireNamespace("fwildclusterboot")
      set.seed(seed)
      bt <- fwildclusterboot::boottest(
              object      = model,
              clustid     = "state",
              param       = param,
              B           = B,
              type        = "rademacher",
              impose_null = TRUE,
              seed        = seed)
      ci_lo <- bt$conf_int[1]
      ci_hi <- bt$conf_int[2]
      se_boot <- (ci_hi - ci_lo) / (2 * 1.96)
      list(SE_boot = se_boot, CI_lo = ci_lo, CI_hi = ci_hi,
           p_boot = bt$p_val, B = B, n_clusters = bt$N_G[1])

    assert: is.finite(se_boot) && se_boot > 0

  run_wcb_cox(model, data, param = "did_term", B = 9999L,
              seed = 20260519L)
    Input:  model — coxph object fit on `data` with cluster = state,
                    ties = "efron"
            data  — the data.table passed to coxph (REQUIRED — coxph
                    stores Surv() in a model frame that prevents re-fit
                    without explicit data; mirror existing run_boot_cox)
            param — name of parameter to bootstrap
            B, seed — wild-score-bootstrap config
    Output: list(SE_boot = num, CI_lo = num, CI_hi = num,
                 p_boot = num, B = int, n_clusters = int)

    Operations: thin wrapper around existing 02b run_boot_cox (line
    ~857), exposing the same signature as run_wcb_ols. Re-export
    that function from reduced_form_utils.R unchanged; just rename
    the public alias to run_wcb_cox and verify it returns the four
    needed elements (SE_boot, CI_lo, CI_hi, p_boot, B, n_clusters).
    If existing run_boot_cox returns a different shape, write a thin
    adapter that maps its return to the contract above.

    assert: is.finite(SE_boot) && SE_boot > 0

═══ Step 1 — Create main script ═══

NEW FILE: Code/Analysis/02c_Stepped_DiD.R

Standard layout per CLAUDE.md R style:
  1. Logging block (LOG START to logs/02c_Stepped_DiD_YYYYMMDD_HHMMSS.log)
  2. Library loads (data.table, fixest, survival, fwildclusterboot,
                    here, gt, glue)
  3. source(here("Code/Helpers/reduced_form_utils.R"))
  4. Set OUTPUT_TABLES, OUTPUT_FIGURES; ensure dirs exist
  5. Load matched_tanks_birth_cem
     (path: Data/Analysis/matched_tanks_birth_cem.rds OR wherever 02b
      currently loads it — coder confirms exact path from 02b S2)
  6. data_active <- build_active_at_treatment_sample(matched_tanks_birth_cem)
  7. cox_active  <- build_active_cox_split(data_active)
  8. Log sample sizes (rows, tanks, facilities, states, # closures)

═══ Step 2 — Fit 7 OLS specifications ═══

Define a shared controls string and a fitter:

  ctrl_rhs <- "mandate_release_det + mandate_spill_overfill + mandate_integrity"

  fit_ols <- function(fe_rhs) {
    fml <- as.formula(sprintf("closure_event ~ did_term + %s | %s",
                              ctrl_rhs, fe_rhs))
    feols(fml, data = data_active, cluster = ~state)
  }

  m_ols <- list(
    `1` = fit_ols("state + panel_year"),
    `2` = fit_ols("state + cell_id + panel_year"),               # + state
    `3` = fit_ols("panel_id + panel_year"),
    `4` = fit_ols("tank_panel_id + panel_year"),
    `5` = fit_ols("state + cell_vintage_year_fe"),               # + state
    `6` = fit_ols("tank_panel_id + cell_vintage_year_fe"),
    `7` = fit_ols("panel_id + cell_vintage_year_fe")
  )

Log: for each col, coef(m)["did_term"] and se(m)["did_term"] and N obs.

assert: length(m_ols) == 7L
assert: all(sapply(m_ols, function(m) !is.null(coef(m)["did_term"])))

═══ Step 3 — Wild cluster bootstrap for OLS ═══

  boot_ols <- lapply(m_ols, run_wcb_ols)
  names(boot_ols) <- names(m_ols)

  Log per col: SE_boot, CI, p_boot, elapsed seconds.

assert: length(boot_ols) == 7L
assert: all(sapply(boot_ols, function(b) is.finite(b$SE_boot)))

═══ Step 4 — Fit 4 Cox specifications ═══

  fit_cox <- function(extra_covs = "", strata_var = NULL) {
    rhs_extra <- if (nzchar(extra_covs))   paste("+", extra_covs)              else ""
    rhs_strat <- if (!is.null(strata_var)) paste("+ strata(", strata_var, ")") else ""
    fml <- as.formula(sprintf(
      "Surv(t_enter, t_exit, failure) ~ did_term + %s %s %s",
      ctrl_rhs, rhs_extra, rhs_strat))
    coxph(fml, data = cox_active, cluster = state, ties = "efron",
          model = TRUE)   # model=TRUE so run_wcb_cox can refit
  }

  m_cox <- list(
    `1` = fit_cox("factor(state)"),
    `2` = fit_cox("factor(state) + factor(cell_id)"),
    `3` = fit_cox("factor(state)",                       strata_var = "cell_id"),
    `4` = fit_cox("factor(state) + factor(panel_year)",  strata_var = "cell_id")
  )

  Note: col (4) (main) has factor(state) + factor(panel_year) +
  strata(cell_id) — high but tractable factor cardinality. Col (2)
  (factor(cell_id) as covariate) may have more factor levels and run
  slower (~10-30 min on server). Log elapsed seconds and warn if any
  single fit > 600s.

  Log per col: coef(m)["did_term"], robust SE, n_events.

assert: length(m_cox) == 4L
assert: all(sapply(m_cox, function(m) !is.na(coef(m)["did_term"])))
assert: all(sapply(m_cox, function(m) is.finite(sqrt(diag(vcov(m)))["did_term"])))
        # (guards against the col 5 SE → infinity bug seen in attempt 1)

═══ Step 5 — Wild score bootstrap for Cox ═══

  boot_cox <- lapply(seq_along(m_cox), function(i) {
    run_wcb_cox(model = m_cox[[i]], data = cox_active)
  })
  names(boot_cox) <- names(m_cox)

  Log per col: SE_boot, CI, p_boot, elapsed seconds.

assert: length(boot_cox) == 4L
assert: all(sapply(boot_cox, function(b) is.finite(b$SE_boot)))

═══ Step 6 — Assemble bootstrap-diagnostics CSV ═══

  boot_diag <- rbindlist(list(
    rbindlist(lapply(seq_along(boot_ols), function(i) data.table(
        model      = "OLS",
        col        = as.integer(names(boot_ols)[i]),
        coef       = coef(m_ols[[i]])["did_term"],
        se_model   = se(m_ols[[i]])["did_term"],
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

  fwrite(boot_diag,
         file.path(OUTPUT_TABLES, "T_Stepped_DiD_Bootstrap_Diagnostics.csv"))

  # Total rows: 7 OLS + 4 Cox = 11 (header excluded)

═══ Step 7 — Render OLS table (3-row format) ═══

Use a hand-written LaTeX template (NOT etable, NOT gt-to-tex —
the 3-row coefficient format is not native to either). Build a
character vector then write_tex().

Format:
  - Booktabs (\toprule, \midrule, \bottomrule)
  - Header row: "" & (1) & (2) & ... & (7)
  - Column-spec row (small font, italic):
      "(state+year)" / "(state+cell+year)" / "(fac+year)"
      / "(tank+year)" / "(state+cell×year)" / "(tank+cell×year)"
      / "(fac+cell×year)"
  - For β_did:
      Row A: "Texas reform (β)"           & est_col1 & ... & est_col7  \\
      Row B (model SE):     " "           & (se_m_1) & ... & (se_m_7)  \\
      Row C (boot SE):      " "           & [se_b_1] & ... & [se_b_7]  \\
  - FE-rows block (Yes / em-dash). Note: state-level absorption is
    enforced in every column (explicit α_s in cols 1, 2, 5; implicit
    via facility / tank FE in cols 3, 4, 6, 7). The table marks only
    EXPLICIT FE; the absorption note is footnoted.
      "State FE (explicit)" & Y & Y & — & — & Y & — & —          \\
      "Year FE"             & Y & Y & Y & Y & — & — & —          \\
      "Cell FE"             & — & Y & — & — & — & — & —          \\
      "Facility FE"         & — & — & Y & — & — & — & Y          \\
      "Tank FE"             & — & — & — & Y & — & Y & —          \\
      "Cell × year FE"      & — & — & — & — & Y & Y & Y          \\
      "Mandate controls"    & Y & Y & Y & Y & Y & Y & Y          \\
  - Summary rows:
      "Observations"       & nobs_1 & ... & nobs_7              \\
      "Adj. R²"            & r2adj_1 & ... & r2adj_7            \\
      "Wild cluster B"     & 9999  & 9999 ... & 9999            \\
  - Notes (\multicolumn{8}{p{...}}{...}):
      "OLS LPM with closure_event as outcome. Coefficient row shows
      estimate β̂, model row shows analytic cluster-robust SE in
      parentheses (clustered at state, G = 18), bootstrap row shows
      wild cluster bootstrap SE in brackets (score-based variant,
      robust to high-dimensional FE; Rademacher weights; B = 9,999;
      bootstrap SE = CI half-width / 1.96). did_term = texas_treated
      × 1{year ≥ 1998}. Sample: active-at-treatment tanks (facility
      had ≥ 1 open or newly installed tank on 1998-12-22), unweighted.
      Mandate controls = mandate_release_det, mandate_spill_overfill,
      mandate_integrity. State-level absorption is enforced in every
      column: explicit α_s in cols 1, 2, 5; implicit via the nested
      unit FE (facility / tank) in cols 3, 4, 6, 7. Col (7) is the
      main specification."

Write to: Output/Tables/T_Stepped_DiD_OLS.tex

═══ Step 8 — Render Cox table (3-row format, 4 cols) ═══

Same row format as Step 7 (3 rows per coefficient). 4 columns this
time, with a sub-header row mapping each Cox col to its OLS
analogue(s). β̂ is log-hazard-ratio.

Header structure:
  - Top header: "" & (1) & (2) & (3) & (4)
  - Sub-header row (small font, italic):
      "(OLS analogue):" & "(1)" & "(2, 3, 4)" & "(5, 6)" & "(7 — main)"

For β_did:
      Row A: "Texas reform (β, log HR)" & est_1 & est_2 & est_3 & est_4 \\
      Row B (model SE):  " "             & (se_m_1) & ... & (se_m_4)    \\
      Row C (boot SE):   " "             & [se_b_1] & ... & [se_b_4]    \\

Spec block (Yes / em-dash):
      "State covariate"     & Y & Y & Y & Y \\
      "Cell covariate"      & — & Y & — & — \\
      "Cell stratum"        & — & — & Y & Y \\
      "Year covariate"      & — & — & — & Y \\
      "Mandate controls"    & Y & Y & Y & Y \\

Summary rows:
      "Tanks"               & ntanks_1 & ... & ntanks_4      \\
      "Events"              & nev_1    & ... & nev_4         \\
      "Wild score B"        & 9999 & 9999 & 9999 & 9999      \\

Notes (\multicolumn{5}{p{...}}{...}):
  "Cox proportional-hazards model with two-episode splits at the
  reform date (1998-12-22). Coefficient row shows estimate β̂ (log
  hazard ratio), model row shows robust cluster-sandwich SE in
  parentheses (Lin-Wei 1989; clustered at state, G = 18),
  bootstrap row shows wild score bootstrap SE in brackets
  (Kline-Santos 2012; B = 9,999). Ties handled by Efron's method.

  Cox cols are reported in 4 distinct identifiable specifications
  mapped to the OLS columns via the sub-header. OLS cols 3, 4 (fac
  / tank + year FE) and OLS col 6 (tank + cell × year FE) collapse
  into Cox cols (2) and (3) because Cox cannot identify additional
  within-unit variation in a single-treated-state DiD design:
  stratifying the partial likelihood on treatment-invariant units
  (state, facility, or tank) zeros the score for β. State-level
  absorption is enforced in every Cox column via factor(state)
  covariate, mirroring the same enforcement in OLS cols 2 and 5.
  Cell-level stratification (Cox cols 3, 4) is identifiable because
  CEM-matched cells contain both Texas and control tanks at risk
  simultaneously.

  Sample: same active-at-treatment tanks as the OLS table. Cox col
  (4) is the main specification."

Write to: Output/Tables/T_Stepped_DiD_Cox.tex

═══ Step 9 — Save fitted-model artifacts (.rds) ═══

  saveRDS(
    list(
      m_ols     = m_ols,
      m_cox     = m_cox,
      boot_ols  = boot_ols,
      boot_cox  = boot_cox,
      sample_meta = list(
        n_rows         = nrow(data_active),
        n_tanks        = uniqueN(data_active$tank_panel_id),
        n_facilities   = uniqueN(data_active$panel_id),
        n_states       = uniqueN(data_active$state),
        n_closures     = sum(data_active$closure_event == 1L),
        seed           = 20260519L,
        B              = 9999L,
        cluster_var    = "state",
        sample_label   = "active_at_treatment",
        weights        = "none",
        ticket         = "005",
        built_on       = Sys.time()
      )
    ),
    file.path(here::here("Output", "Estimation_Results"),
              "Stepped_DiD_Fits_active_at_treatment.rds")
  )

═══ Step 10 — Print summary block & close ═══

Per CLAUDE.md output rules, print one line per model with:
  Estimator | Col | β̂ | SE_model | SE_boot | p_boot | N

Then write a small "OUTPUT SUMMARY" footer listing all files written.

═══════════════════════════════════════════════════
R-IMPLEMENTATION NOTES
═══════════════════════════════════════════════════
- Script path: Code/Analysis/02c_Stepped_DiD.R
- Helper path: Code/Helpers/reduced_form_utils.R (NEW; created in this ticket)
- here::here() for all paths; no absolute paths
- Required packages: data.table, fixest, survival, fwildclusterboot,
  here, glue. (gt may be skipped — the table layout is hand-written
  LaTeX since the 3-row format is not native to gt or etable.)
- Install if missing: fwildclusterboot
  (install.packages("fwildclusterboot"))
- Logging: per CLAUDE.md block at top of script
- Hard error propagation: NO tryCatch returning NULL; NO try(silent=TRUE)
- The wild-bootstrap calls are deterministic given the seed; running
  the script twice must produce identical SE_boot to within numerical
  noise (< 1e-10 absolute difference)
- For run_wcb_cox: confirm the existing 02b run_boot_cox returns SE
  (not just p-value). If it does not, extend it in
  reduced_form_utils.R to compute SD(boot_betas) and CI bounds from
  empirical quantiles. Do NOT modify the function in 02b itself;
  this ticket carves it OUT of 02b into the helper file.
- factor(cell_id) and factor(panel_year) in Cox cols 5-7 will produce
  many columns. coxph will not error but may print messages about
  rank — accept these. Do NOT pre-drop levels.
- Bootstrap reps: B = 9999 throughout (per spec). The script will be
  run on the server, so the bootstrap can parallelize. Register a
  `doParallel` backend with `BOOT_NTHREADS` cores before calling
  `run_wcb_ols` and `run_wcb_cox` (both wrappers call the existing
  02b score-based bootstrap from 02b line ~714 / ~857, which uses
  `foreach`). Make `BOOT_NTHREADS` a named constant at the top of the
  script so it's easy to tune. Expected wall time on a 32-core
  server: <15 min total. The bottleneck will be Cox col (2)
  (factor(cell_id) covariate) due to factor-cardinality. If total
  exceeds 2 hours, reduce B to 4999 ONLY with a code comment
  documenting the reduction and a check that the change is reflected
  in all table notes and CSV outputs.
- Bootstrap method (resolved during attempt 1): Use the manual wild
  cluster bootstrap (score-based variant) lifted from 02b. The
  default `fwildclusterboot::boottest()` fails or gives incorrect
  results on the high-cardinality cell × year FE structure in cols
  5-7. The score-based variant handles arbitrary FE structures. Both
  variants are family-equivalent "wild cluster bootstrap" methods;
  the table notes must accurately describe the variant in use.
- Reproducibility note: setting the seed BEFORE the bootstrap and
  using identical thread count must yield identical SE_boot across
  runs. If parallel RNG introduces non-determinism in
  fwildclusterboot, set `nthreads = 1L` only for the determinism
  check (acceptance criterion below) and otherwise use parallel.

═══════════════════════════════════════════════════
DELIVERABLES — ENUMERATED
═══════════════════════════════════════════════════

(1) NEW FILE: Code/Helpers/reduced_form_utils.R
    Contents per Step 0. Must export:
      - log_step, fmt_n, save_gg, write_tex
      - REFORM_DATE, REFORM_DAYS, CONTROL_STATES, OUTPUT_TABLES, OUTPUT_FIGURES
      - build_active_at_treatment_sample(matched_panel) → data.table
      - build_active_cox_split(active_panel, reform_cut) → data.table
      - run_wcb_ols(model, param, B, seed)               → list
      - run_wcb_cox(model, data, param, B, seed)         → list
    Hard requirement: sourcing this file must NOT trigger any
    estimation or file I/O. It is a library file only.

(2) NEW FILE: Code/Analysis/02c_Stepped_DiD.R
    Per Steps 1-10. End-to-end runnable from a fresh R session given
    the data inputs.

(3) Output/Tables/T_Stepped_DiD_OLS.tex
    LaTeX table per Step 7. Required structure:
      - 7 numbered columns
      - Coefficient block: 1 row "Texas reform (β)" + 2 SE rows
        (model in parens, boot in brackets)
      - FE block: 7 rows (state, year, cell, facility, tank, cell × year,
        mandate controls), with Y/—/em-dash per spec
      - Summary block: Observations, Adj. R², Wild cluster B
      - Notes section per Step 7 wording
    Must compile inside a standard article preamble (will be \input'd
    into the report .qmd).

(4) Output/Tables/T_Stepped_DiD_Cox.tex
    LaTeX table per Step 8. Required structure:
      - 7 numbered columns
      - Coefficient block: 1 row "Texas reform (β, log HR)" + 2 SE rows
      - Strata/covariate block: 4 rows (Stratification, Year covariate,
        Cell covariate, Mandate controls)
      - Summary block: Tanks, Events, Wild score B
      - Notes section per Step 8 wording
    Must compile alongside (3) in the same document.

(5) Output/Tables/T_Stepped_DiD_Bootstrap_Diagnostics.csv
    Long format, 11 rows (7 OLS cols + 4 Cox cols), 10 columns:
      model (chr, "OLS" or "Cox"), col (int 1-7 for OLS, 1-4 for Cox),
      coef (num), se_model (num), se_boot (num), ci_lo_boot (num),
      ci_hi_boot (num), p_boot (num), B (int = 9999), n_obs (int)

(6) Output/Estimation_Results/Stepped_DiD_Fits_active_at_treatment.rds
    R list per Step 9 schema. Required top-level names:
      m_ols (list, length 7, names "1".."7", each fixest)
      m_cox (list, length 4, names "1".."4", each coxph)
      boot_ols (list, length 7, names "1".."7", each list per run_wcb_ols)
      boot_cox (list, length 4, names "1".."4", each list per run_wcb_cox)
      sample_meta (list of 12 scalars/strings per Step 9)

(7) Log file: logs/02c_Stepped_DiD_<TIMESTAMP>.log
    Auto-generated by the logging block; must contain at minimum:
      - Library load lines
      - Sample-size block from Step 1
      - Per-OLS-col coef/SE/N from Step 2
      - Per-OLS-col bootstrap diagnostics from Step 3
      - Per-Cox-col coef/SE/n_events from Step 4
      - Per-Cox-col bootstrap diagnostics from Step 5
      - Output-summary footer

═══════════════════════════════════════════════════
ACCEPTANCE CRITERIA — MECHANICAL
═══════════════════════════════════════════════════

HELPER FILE (1):
- [ ] Code/Helpers/reduced_form_utils.R exists
- [ ] Sourcing it does NOT trigger any I/O or estimation
      (verify by sourcing with options(warn = 2) and confirming no warnings
       or file-write side effects)
- [ ] All 6 listed functions exist after sourcing:
      exists("build_active_at_treatment_sample"),
      exists("build_active_cox_split"),
      exists("run_wcb_ols"), exists("run_wcb_cox"),
      exists("save_gg"), exists("write_tex")

MAIN SCRIPT (2):
- [ ] Code/Analysis/02c_Stepped_DiD.R exists
- [ ] Runs end-to-end in a fresh R session
- [ ] No tryCatch returning NULL anywhere
- [ ] No try(..., silent = TRUE) anywhere
- [ ] Uses here::here() for all paths

FITS:
- [ ] length(m_ols) == 7L; all elements inherit from "fixest"
- [ ] length(m_cox) == 4L; all elements inherit from "coxph"
- [ ] For every col i: !is.na(coef(m_ols[[i]])["did_term"])
- [ ] For every col i: !is.na(coef(m_cox[[i]])["did_term"])
- [ ] For every Cox col i: is.finite(sqrt(diag(vcov(m_cox[[i]])))["did_term"])
      (guards against numerical collapse / SE → ∞ as observed in
       attempt 1's old col 5 spec; should not happen with the
       corrected cell-stratified design)
- [ ] All m_ols fits have nobs() identical within ±1% of each other
      (small differences allowed for FE-singleton drops)
- [ ] All m_cox fits use ties = "efron"

BOOTSTRAP:
- [ ] length(boot_ols) == 7L; all SE_boot finite and > 0
- [ ] length(boot_cox) == 4L; all SE_boot finite and > 0
- [ ] All boot_ols entries: B == 9999
- [ ] All boot_cox entries: B == 9999
- [ ] Running the script twice yields identical SE_boot values
      to within 1e-10 (reproducibility)

DELIVERABLES (3)-(6):
- [ ] T_Stepped_DiD_OLS.tex exists and contains exactly 7 column headers
      "(1)" through "(7)"
- [ ] T_Stepped_DiD_OLS.tex contains the strings "[" and "]"
      (bracket-wrapped bootstrap SEs)
- [ ] T_Stepped_DiD_OLS.tex notes contain "wild cluster bootstrap"
      AND "score-based"
- [ ] T_Stepped_DiD_Cox.tex exists with 4 columns "(1)" through "(4)"
      AND a sub-header row containing "(OLS analogue)"
- [ ] T_Stepped_DiD_Cox.tex notes contain "wild score bootstrap"
      AND "Kline-Santos"
- [ ] T_Stepped_DiD_Bootstrap_Diagnostics.csv has 11 rows + header
      (7 OLS + 4 Cox)
- [ ] Stepped_DiD_Fits_active_at_treatment.rds loads with readRDS()
      and has all 5 top-level names from Step 9

LOG (7):
- [ ] logs/02c_Stepped_DiD_*.log exists after the run
- [ ] Log contains the string "OUTPUT SUMMARY" near the end

═══════════════════════════════════════════════════
ATTEMPT LOG
═══════════════════════════════════════════════════

### Attempt 1 — 2026-05-19 (Sonnet 4.6 Pro)
Result: PSEUDOCODE_FAIL — Cox identification
- OLS Steps 1-3: PASS (all 7 specs + bootstraps, reproducible)
- Cox cols 1, 3, 4: β=NA (strata(state/facility/tank) zeros the score for β
  in a single-treated-state DiD; treatment is invariant within stratum)
- Cox col 5: robust SE=1.8e35 (numerical collapse from cell-strata + year covariate)

### Attempt 2 — 2026-05-19 evening (Sonnet 4.6 Pro)
Architect spec revised:
- Cox redesigned to 4 cols: factor(state); factor(state)+factor(cell_id);
  factor(state)+strata(cell_id); factor(state)+strata(cell_id)+factor(panel_year)
- Cox cols 2 and 3 use covariate/strata-only forms; col 4 adds year covariate
Local OLS verified end-to-end (col 7 main: beta=0.02061 on the Apr-14 prototype).
Cox cols 2-4 timed out on local; server compute required.

### Attempt 3 — 2026-05-20 morning (server run)
- OLS clean on canonical birth-CEM panel (col 7 main: beta=0.01584).
- Cox col 1 OK (beta=0.017, robust SE=0.448 - small-G sandwich is uninformative).
- Cox col 2 (factor(state)+factor(cell_id)): TIMED OUT (Hessian inversion on
  1083 cell levels x 728K episodes is infeasible in coxph).

Architect call: dropped factor(cell_id)-covariate variant entirely; cell
absorption goes through strata only. Down to 3 distinct cols.

### Attempt 4a — 2026-05-20 ~16:00 (server run)
- naresid bug: survival::naresid does not exist (function lives in stats::).
  Fixed in helper.
- Cox col 3 (factor(state)+strata(cell_id)+factor(panel_year)): robust SE
  ~1e79 (rank-deficient because factor(panel_year) is collinear with the
  cell-specific non-parametric baseline hazard h_{0,c}(t)).

Architect call: dropped factor(panel_year)-on-top-of-strata variant.
Down to 2 distinct cols.

### Attempt 4b — 2026-05-20 ~16:10 (server run, 2-col Cox)
- User asked for more granular cell-stratification robustness cols.
- Expanded to 5-col Cox: state; state+year; state+strata(vintage);
  state+strata(mm); state+strata(cell).

### Attempt 4c — 2026-05-20 ~16:20 (server run, 5-col Cox)
- Col 2 (factor(state)+factor(panel_year)) overflowed exp(). Root cause:
  Missouri's state-FE coefficient diverges to infinity once year dummies
  enter the design (MO-specific anomaly in episode-midpoint-year distribution).
- Also dropped mandate controls from Cox: in two-episode split, panel_year
  is episode-midpoint year, so mandate dummies become near-perfect predictors
  of failure indicator.

Architect call: dropped year-dummy col. Final 4 cols: state; state+strata(vintage);
state+strata(mm); state+strata(cell).

### Attempt 4 final — 2026-05-20 ~16:25 (server run, 4-col Cox) — PASS
- All 4 Cox specs fit cleanly, all bootstraps complete.
- Walk-in story: HR 1.65 (naive) -> 1.47 (vintage strata) -> 1.55 (mm strata)
  -> 1.44 (cell strata, MAIN). All p_boot < 0.001.
- OLS unchanged from attempt 3: col 7 main beta=0.01584, SE_boot=0.00400.

Reviewer verdict (Opus, 2026-05-20): PASS.

Headline numbers landed:
  OLS col (7) main: beta = 0.01584, SE_boot = 0.00400, CI [0.0101, 0.0216]
                    -> +1.58 pp annual closure probability
  Cox col (4) main: beta = 0.36561, SE_boot = 0.11677, CI [0.249, 0.482]
                    -> HR = 1.44 (44% higher closure hazard)

Deliverables verified on Z mount:
  Output/Tables/T_Stepped_DiD_OLS.tex                            (2483 B, 7 cols)
  Output/Tables/T_Stepped_DiD_Cox.tex                            (3601 B, 4 cols)
  Output/Tables/T_Stepped_DiD_Bootstrap_Diagnostics.csv          (11 rows + header)
  Output/Estimation_Results/Stepped_DiD_Fits_active_at_treatment.rds (1.0 GB)
  logs/02c_Stepped_DiD_20260520_161704.log

Spec-vs-implementation deviations (all architect iterations, documented):
  1. Cox cols: 7 spec -> 4 final. Identification + numerical constraints.
  2. OLS bootstrap: score variant (not residual variant per CGM). Required
     because boottest fails on high-cardinality cell_vintage_year_fe.
  3. Cox mandates: omitted. Misaligned with two-episode panel_year semantics.
  4. Cox col 4 main (was col 7 in spec); .qmd updated to reference col (4).

Lessons for future tickets (added to project memory implicitly):
  - For Cox in single-treated-state DiD: strata can only be on units that
    contain both T and C tanks (cells via matching).
  - Two-episode Cox splits invalidate calendar-year-indexed covariates.
  - Never use survival::naresid; use stats::naresid (helper file fixed).
  - coxph chokes on high-dim factor() in the design matrix even when OLS
    handles the same FE structure fine via projection-based demeaning.
