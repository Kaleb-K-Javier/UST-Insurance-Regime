# TICKET 006 — 02b refactor: event studies (OLS + Cox), ref = -1 and -2
# Created: 2026-05-19
# Status: AWAITING_IMPLEMENTATION (T005 dependencies all LANDED 2026-05-20)
# Attempt: 0
# Depends on: 005 (helper file exists, sample/Cox-split builders work, wild
#             bootstrap wrappers work, naresid bug fixed, mandate-in-Cox lesson
#             absorbed into Eq. 2 below)

═══════════════════════════════════════════════════
ECONOMIC MOTIVATION
═══════════════════════════════════════════════════
The static stepped DiD (Ticket 005) reports a single ATT per
specification but says nothing about (a) pre-trends — whether
treated and control facilities were on the same closure trajectory
before 1998 — or (b) the dynamic shape of the response (immediate
spike vs. gradual ramp-up vs. delayed onset).

Event-study regressions answer both. For each year τ relative to
1998, we estimate β_τ = E[y | TX, τ] − E[y | control, τ] − baseline,
with one ω in {-1, -2} chosen as the omitted reference category.
β_τ for τ < 0 should be statistically indistinguishable from zero
under the parallel-trends assumption; β_τ for τ ≥ 0 traces out the
post-reform dynamic effect.

We fit the event study at the main specification (facility FE +
cell × year FE, matching col 7 of the stepped table) for both OLS
and Cox. We report both ref = -1 (standard) and ref = -2
(robustness — the -1 observation is itself partially treated since
the reform date is 1998-12-22, very late in the calendar year, and
some closures triggered by anticipation may already register).

Event-study tables are saved as deliverables but NOT included in
the report .qmd (per user — the figures are sufficient for advisor
review; the tables exist as reference artifacts).

═══════════════════════════════════════════════════
MATH
═══════════════════════════════════════════════════

Setup: same active-at-treatment sample, same controls (mandate_*),
same cluster level (state), same cell definitions as Ticket 005.

Eq. 1 — OLS event study, generic form:

    y_{kt} = sum_{τ ≠ ω} β_τ · 1{rel_year_{kt} = τ} · texas_treated_k
            + γ' · X_{kt}
            + φ_i + θ_{c, t} + ε_{kt}

  where:
    rel_year_{kt} := panel_year_t − 1998
    ω             ∈ {-1, -2}  (the omitted "reference" period)
    φ_i           facility fixed effect
    θ_{c,t}       cell × year fixed effect (= cell_vintage_year_fe)

  Implemented via fixest's i() factor-interaction syntax:
    closure_event ~ i(rel_year, texas_treated, ref = ω)
                  + mandate_release_det
                  + mandate_spill_overfill
                  + mandate_integrity
                  | panel_id + cell_vintage_year_fe

  with cluster = ~state.

Eq. 2 — Cox event study, generic form:

    h(t | k) = h_{0, i(k)}(t)
             · exp( sum_{τ ≠ ω} β_τ · es_tau_τ_k(t)
                  + γ' · X_k(t)
                  + η_c · 1{c(k) = c}
                  + η_t · 1{cal_year(t) = t} )

  where es_tau_τ_k(t) = texas_treated_k · 1{rel_year(t) = τ} is the
  per-year treatment indicator, time-varying via the per-year-split
  Cox episode structure (one episode per (tank, calendar year) pair).

  Implemented via coxph on per-year-split data with es_tau_* dummies
  (existing 02b convention; the column the reference period gets is
  the one OMITTED from the formula):

    coxph(Surv(t_enter, t_exit, failure) ~
            es_tau_minus_14 + ... + es_tau_minus_2 + (omit -1)
                 + es_tau_0 + es_tau_1 + ... + es_tau_22
            + factor(state) + strata(cell_id),
          data = cox_es_active, cluster = state, ties = "efron")

  For ref = -2 the omitted dummy shifts accordingly.

  CRITICAL — Cox spec lessons from T005 (must be carried into T006):

  (a) The Cox ES MUST NOT stratify on panel_id, state, or tank_panel_id.
      Within any such stratum, every es_tau_τ dummy is a deterministic
      function of calendar time (1 only when rel_year(t) == τ for Texas
      units; always 0 for control units), so within-stratum risk sets
      contain at-risk observations with identical es_tau values and the
      partial-likelihood score for β_τ is zero. Identification requires
      strata that contain BOTH Texas and control tanks at risk — which
      cell_id provides by construction (matched cells). Use
      factor(state) as a covariate (not a stratum) to absorb state-level
      baseline differences. This mirrors the Cox spec in Ticket 005
      col (4) (main).

  (b) DO NOT include `factor(panel_year)` as a Cox covariate. In the
      static stepped table (T005 attempt 4c), `factor(state) +
      factor(panel_year)` without strata caused Missouri's state-FE
      coefficient to diverge to infinity (exp() overflow); and
      `factor(state) + strata(cell_id) + factor(panel_year)` was
      rank-deficient (robust SE ~1e79). The per-year-split data
      structure in the event study has more degrees of freedom than
      the two-episode split, so factor(panel_year) MIGHT be tractable
      in principle — but the es_tau_τ dummies themselves already
      capture calendar-year variation in the treatment effect (each
      τ corresponds to a specific calendar year), so factor(panel_year)
      is at best redundant and at worst rank-deficient. Omit it.

  (c) DO NOT include the mandate controls in the Cox ES. In T005 these
      were dropped from all Cox specs because the two-episode split
      makes `panel_year` correlated with failure time, turning the
      mandate dummies into near-perfect predictors of the failure
      indicator. The per-year-split has cleaner panel_year semantics,
      BUT the mandate dummies are still essentially calendar-year-by-
      state indicators that risk near-collinearity with the es_tau
      dummies (which are texas × calendar-year indicators). Safer
      to omit from Cox and document the OLS-vs-Cox asymmetry in the
      table notes, mirroring T005.

  (d) When extracting the Cox score for the bootstrap, use stats::naresid,
      NOT survival::naresid (the latter does not exist as an exported
      function). The helper file Code/Helpers/reduced_form_utils.R has
      this already correct from T005's fix.

Eq. 3 — Plotting convention:
  - x-axis: rel_year ∈ [min, max], typically [-14, +22]
  - y-axis: β̂_τ with 95% pointwise CI = β̂_τ ± 1.96 · SE_τ
  - reference period shown as a hollow shape at y = 0 (CI = NA)
  - vertical dotted line at x = ω − 0.5
  - colour by period: pre / event(0) / post
  - Times font, theme_classic; matches existing plot_es_clean style

═══════════════════════════════════════════════════
PSEUDOCODE
═══════════════════════════════════════════════════

═══ Step 0 — Extend shared helper file ═══

EDIT FILE: Code/Helpers/reduced_form_utils.R

Append (do NOT modify functions already added in Ticket 005):

  build_active_cox_es_split(active_panel, max_lead = 22L, max_lag = 14L)
    Input:  active_panel — output of build_active_at_treatment_sample
            max_lead     — int, largest post-reform rel_year (default 22)
            max_lag      — int, abs() of smallest pre-reform rel_year
                           (default 14; so years span [-14, +22])
    Output: data.table of per-year Cox splits with columns:
              tank_panel_id, panel_id, state, install_yr_int,
              make_model_noage, cell_id, t_enter, t_exit, failure,
              texas_treated, panel_year, rel_year,
              mandate_release_det, mandate_spill_overfill, mandate_integrity,
              es_tau_minus_14, ..., es_tau_minus_1, es_tau_0,
              es_tau_1, ..., es_tau_22

    Operations: mirror existing 02b build_cox_es_split (line ~647) but
    take active_panel as the source. Spell out:
      1. For each tank in active_panel: identify install date, exit date
         (= last observed panel_year if no closure; year of closure
         otherwise), failure flag.
      2. Build episodes split at every Jan-1 boundary from install year
         through max(panel_year). Each episode covers one calendar year.
         t_enter, t_exit in days from 1985-01-01.
      3. For each episode set panel_year = the calendar year the episode
         falls in; compute rel_year = panel_year - 1998.
      4. failure = 1 ONLY in the episode containing the actual exit;
         0 otherwise.
      5. Carry mandate_* values from active_panel by (tank, panel_year).
      6. Build es_tau_<τ> dummies as integer (0/1):
           es_tau_<τ>[i] = texas_treated[i] * (rel_year[i] == τ)
         for τ ∈ {-max_lag, ..., max_lead}, skipping NONE
         (omission of the reference period happens at fit time via
         which dummies are in the formula RHS).
      7. Attach cell_id (mm × vintage).

    assert: nrow(output) > nrow(active_panel)     # split expands rows
    assert: sum(output$failure) == sum(active_panel$closure_event == 1L)
    assert: all rel_year values fall in [-max_lag, max_lead]
    assert: attr(output, "es_vars") := names of all es_tau_* cols

  extract_es_coef_dt_ols(model, ref_period = -1L)
    Input:  fixest model with i(rel_year, texas_treated, ref = X) terms
            ref_period — the omitted τ (default -1)
    Output: data.table with columns: rel_year (int), estimate (num),
            se (num), period (chr in {pre, event, post}),
            ci_lo (num), ci_hi (num)
            Reference period is included with estimate = 0, se = 0.

    Operations: mirror existing 02b helper from S3 (line ~282-310);
    parse fixest coefficient names of form
      "rel_year::-3:texas_treated"  OR  "texas_treated:rel_year::-3"
    extract τ, build the data.table, append the (τ = ref_period, 0, 0)
    row, set period flags, compute 95% CI.

  extract_es_coef_dt_cox(model, ref_period = -1L)
    Input:  coxph model with es_tau_* terms
            ref_period — the omitted τ
    Output: same column schema as extract_es_coef_dt_ols
            (estimate is log-HR; SE is robust cluster sandwich)

    Operations: mirror existing 02b `es_tidy_cox` (line ~427).
    Parse coefficient names "es_tau_minus_<n>" → -n,
    "es_tau_<n>" → +n. Append reference row, build CI.

  plot_es_clean(es_dt, ref_period = -1L,
                x_label = "Years relative to Dec 22 1998",
                y_label = "Coefficient",
                col_pre = "#3A6BBF", col_post = "#BF3A3A",
                col_event = "#888888")
    Input:  es_dt — output of extract_es_coef_dt_{ols,cox}
            other args control axis labels and colours
    Output: ggplot object (Times font, theme_classic, no title/caption)

    Operations: copy verbatim from existing 02b plot_es_clean (line
    ~466-498); refactor signature to take a data.table directly
    instead of a model so the same function works for OLS and Cox
    outputs of extract_es_coef_dt_*.

After these additions, sourcing reduced_form_utils.R must STILL not
trigger any I/O or estimation.

═══ Step 1 — Create main script ═══

NEW FILE: Code/Analysis/02d_Event_Studies.R

Standard layout:
  1. Logging block → logs/02d_Event_Studies_<TIMESTAMP>.log
  2. Library loads (data.table, fixest, survival, ggplot2,
                    here, ggtext optional)
  3. source(here("Code/Helpers/reduced_form_utils.R"))
  4. Load matched_tanks_birth_cem (same path as Ticket 005)
  5. data_active  <- build_active_at_treatment_sample(matched_tanks_birth_cem)
  6. cox_es       <- build_active_cox_es_split(data_active,
                                                max_lead = 22L, max_lag = 14L)
  7. Log sample sizes (rows in each, tanks, # episodes, # events)

═══ Step 2 — Fit OLS event studies ═══

  ctrl_rhs <- "mandate_release_det + mandate_spill_overfill + mandate_integrity"
  es_fml_ols <- function(ref) {
    as.formula(sprintf(
      "closure_event ~ i(rel_year, texas_treated, ref = %dL) + %s | panel_id + cell_vintage_year_fe",
      ref, ctrl_rhs))
  }

  m_es_ols <- list(
    main_ref_m1 = feols(es_fml_ols(-1L), data = data_active, cluster = ~state),
    main_ref_m2 = feols(es_fml_ols(-2L), data = data_active, cluster = ~state)
  )

  Log per fit: ref period, N obs, # τ coefficients estimated,
  range of β̂_τ.

assert: length(m_es_ols) == 2L
assert: all(sapply(m_es_ols, function(m) inherits(m, "fixest")))

═══ Step 3 — Fit Cox event studies ═══

  es_vars <- attr(cox_es, "es_vars")

  build_es_fml_cox <- function(ref_tau) {
    # Drop the dummy corresponding to ref_tau from the RHS
    ref_name <- if (ref_tau < 0) {
      sprintf("es_tau_minus_%d", abs(ref_tau))
    } else {
      sprintf("es_tau_%d", ref_tau)
    }
    keep_vars <- setdiff(es_vars, ref_name)
    # CRITICAL (lessons from T005, see Eq. 2 above):
    #   - NO mandate controls (omit ctrl_rhs entirely for Cox)
    #   - NO factor(panel_year) on top of strata(cell_id)
    #     (rank-deficient against cell-specific baseline hazard;
    #      es_tau dummies already encode calendar-year variation)
    rhs <- paste(c(
      paste(keep_vars, collapse = " + "),
      "factor(state) + strata(cell_id)"
    ), collapse = " + ")
    as.formula(sprintf("Surv(t_enter, t_exit, failure) ~ %s", rhs))
  }

  m_es_cox <- list(
    main_ref_m1 = coxph(build_es_fml_cox(-1L), data = cox_es,
                        cluster = state, ties = "efron", model = TRUE),
    main_ref_m2 = coxph(build_es_fml_cox(-2L), data = cox_es,
                        cluster = state, ties = "efron", model = TRUE)
  )

  Log per fit: ref period, # events, # τ coefficients, range of β̂_τ.

assert: length(m_es_cox) == 2L
assert: all(sapply(m_es_cox, function(m) inherits(m, "coxph")))

═══ Step 4 — Build coefficient data.tables for plotting/CSV ═══

  es_dt <- list(
    ols_ref_m1 = extract_es_coef_dt_ols(m_es_ols$main_ref_m1, -1L),
    ols_ref_m2 = extract_es_coef_dt_ols(m_es_ols$main_ref_m2, -2L),
    cox_ref_m1 = extract_es_coef_dt_cox(m_es_cox$main_ref_m1, -1L),
    cox_ref_m2 = extract_es_coef_dt_cox(m_es_cox$main_ref_m2, -2L)
  )

assert: every element has columns rel_year, estimate, se, period, ci_lo, ci_hi
assert: reference period is present with estimate == 0 and se == 0

═══ Step 5 — Save coefficient CSVs ═══

  out_dt <- rbindlist(lapply(names(es_dt), function(nm) {
    parts <- strsplit(nm, "_ref_m")[[1]]
    cbind(model = parts[1], ref_period = -as.integer(parts[2]), es_dt[[nm]])
  }))
  fwrite(out_dt, file.path(OUTPUT_TABLES, "T_Event_Study_Coefficients_Long.csv"))

═══ Step 6 — Render LaTeX tables (artifacts, not for report) ═══

OLS table (etable-friendly format, 2 cols = 2 ref periods):

  etable(m_es_ols$main_ref_m1, m_es_ols$main_ref_m2,
         headers = c("ref = -1", "ref = -2"),
         tex = TRUE, digits = 4,
         file = file.path(OUTPUT_TABLES, "T_Event_Study_OLS.tex"))

Cox table: etable does NOT support coxph. Hand-build a small two-col
table with one row per τ (rel_year):

  - Header: " " & "ref = -1" & "ref = -2"
  - One row per τ from min(rel_year) to max(rel_year), reporting
    "β̂ (SE)" in each column (NA at the omitted ref period)
  - Bottom rows: "Events" / "Tanks" / "Bootstrap" (state — no bootstrap
    here; this is the asymptotic ES table)
  - Notes: "Cox PH event-study coefficients (log hazard ratios). SEs
    are robust cluster-sandwich (Lin-Wei 1989), clustered at state.
    Reference τ shown with --- in that column. Main specification
    matches col (4) of T_Stepped_DiD_Cox.tex: factor(state) +
    strata(cell_id). Mandate controls omitted (same rationale as
    in the stepped Cox table — see T_Stepped_DiD_Cox.tex notes).
    Sample: active-at-treatment tanks, unweighted."

  write_tex(c(...), "T_Event_Study_Cox.tex")

═══ Step 7 — Render figures ═══

For each of the 4 es_dt elements, build a plot:

  p_ols_m1 <- plot_es_clean(es_dt$ols_ref_m1, ref_period = -1L,
                             y_label = "Coefficient (LPM)")
  p_ols_m2 <- plot_es_clean(es_dt$ols_ref_m2, ref_period = -2L,
                             y_label = "Coefficient (LPM)")
  p_cox_m1 <- plot_es_clean(es_dt$cox_ref_m1, ref_period = -1L,
                             y_label = "Coefficient (log hazard ratio)")
  p_cox_m2 <- plot_es_clean(es_dt$cox_ref_m2, ref_period = -2L,
                             y_label = "Coefficient (log hazard ratio)")

Save:
  save_gg(p_ols_m1, "Fig_Event_Study_OLS_ref_m1", width = 10, height = 5)
  save_gg(p_ols_m2, "Fig_Event_Study_OLS_ref_m2", width = 10, height = 5)
  save_gg(p_cox_m1, "Fig_Event_Study_Cox_ref_m1", width = 10, height = 5)
  save_gg(p_cox_m2, "Fig_Event_Study_Cox_ref_m2", width = 10, height = 5)

(File suffix "_m1" means ref = -1 — "_m" for "minus"; chosen because
"-" is awkward in some shell contexts.)

═══ Step 8 — Save fitted-model .rds ═══

  saveRDS(
    list(
      m_es_ols    = m_es_ols,
      m_es_cox    = m_es_cox,
      es_dt       = es_dt,
      sample_meta = list(
        n_active_rows  = nrow(data_active),
        n_cox_episodes = nrow(cox_es),
        n_tanks        = uniqueN(data_active$tank_panel_id),
        n_facilities   = uniqueN(data_active$panel_id),
        n_closures     = sum(data_active$closure_event == 1L),
        es_max_lag     = 14L,
        es_max_lead    = 22L,
        ref_periods    = c(-1L, -2L),
        sample_label   = "active_at_treatment",
        weights        = "none",
        ticket         = "006",
        built_on       = Sys.time()
      )
    ),
    file.path(here::here("Output", "Estimation_Results"),
              "Event_Study_Fits_active_at_treatment.rds")
  )

═══ Step 9 — Print summary block & close ═══

For each of the 4 ES fits, log:
  Model | Ref | N coefs | min β̂_τ | max β̂_τ | mean SE
Followed by an OUTPUT SUMMARY footer listing files written.

═══════════════════════════════════════════════════
R-IMPLEMENTATION NOTES
═══════════════════════════════════════════════════
- Script path: Code/Analysis/02d_Event_Studies.R
- Sources Code/Helpers/reduced_form_utils.R (created in Ticket 005,
  extended in Step 0 of this ticket)
- Required packages: data.table, fixest, survival, ggplot2, here.
  No fwildclusterboot (no bootstrap in this ticket; the ES asymptotic
  SE is sufficient and bootstrap on a 36-coefficient ES is overkill).
- here::here() for all paths
- Hard error propagation: no tryCatch/try silencing
- Cox ES main spec is factor(state) + strata(cell_id) per T005 lessons.
  Each fit is ~10-30 min (~100K events split across ~1000 cell strata,
  each with its own non-parametric baseline hazard, ~36 es_tau coefficients
  to estimate). Two fits in this ticket → ~30-60 min total. Run on server.
- es_tau_<n> naming convention: existing 02b uses
  "es_tau_minus_<n>" for negative τ and "es_tau_<n>" for non-negative
  τ (no leading "plus_"). Match this exactly so downstream regex
  parsing in extract_es_coef_dt_cox works.
- The OLS plots reuse plot_es_clean (formerly in 02b) but the helper
  signature change to take a coefficient data.table (not a model)
  means callers must extract first. This is intentional — keeps the
  function dual-use across OLS and Cox.

═══════════════════════════════════════════════════
DELIVERABLES — ENUMERATED
═══════════════════════════════════════════════════

(1) EDIT FILE: Code/Helpers/reduced_form_utils.R
    Adds 4 new functions:
      build_active_cox_es_split
      extract_es_coef_dt_ols
      extract_es_coef_dt_cox
      plot_es_clean
    Sourcing still has no I/O / no estimation side effects.

(2) NEW FILE: Code/Analysis/02d_Event_Studies.R
    Per Steps 1-9. Runnable end-to-end from a fresh R session.

(3) Output/Tables/T_Event_Study_OLS.tex
    fixest etable output, 2 columns ("ref = -1", "ref = -2"), one row
    per estimated τ (the ref period appears as the omitted level
    automatically). Standard fixest "aer" style.
    NOT included in the report .qmd — saved as a reference artifact.

(4) Output/Tables/T_Event_Study_Cox.tex
    Hand-built two-column LaTeX table per Step 6 (one row per τ from
    min to max rel_year, "β̂ (SE)" cells, --- in the ref column),
    booktabs style. Notes per Step 6 wording.
    NOT included in the report .qmd.

(5) Output/Tables/T_Event_Study_Coefficients_Long.csv
    Long-format CSV. Columns:
      model (chr: "ols" or "cox")
      ref_period (int: -1 or -2)
      rel_year (int)
      estimate (num)
      se (num)
      period (chr: "pre", "event", or "post")
      ci_lo (num)
      ci_hi (num)
    Rows: 4 (ref_period × model) × (1 + range(rel_year)) ≈ 4 × 37 = 148

(6) Output/Figures/Fig_Event_Study_OLS_ref_m1.{pdf,png}
(7) Output/Figures/Fig_Event_Study_OLS_ref_m2.{pdf,png}
(8) Output/Figures/Fig_Event_Study_Cox_ref_m1.{pdf,png}
(9) Output/Figures/Fig_Event_Study_Cox_ref_m2.{pdf,png}
    All 10in × 5in. Times font, theme_classic, no title (added in
    .qmd caption). x-label "Years relative to Dec 22 1998", y-label
    "Coefficient (LPM)" for OLS or "Coefficient (log hazard ratio)"
    for Cox. Legend at bottom, no legend title.

(10) Output/Estimation_Results/Event_Study_Fits_active_at_treatment.rds
     R list per Step 8. Required top-level names:
       m_es_ols (list of 2 fixest)
       m_es_cox (list of 2 coxph)
       es_dt (list of 4 data.tables)
       sample_meta (list of 12)

(11) Log file: logs/02d_Event_Studies_<TIMESTAMP>.log
     Auto-generated. Must contain sample-size block, per-fit summary,
     and "OUTPUT SUMMARY" footer.

═══════════════════════════════════════════════════
ACCEPTANCE CRITERIA — MECHANICAL
═══════════════════════════════════════════════════

HELPER EXTENSIONS (1):
- [ ] After re-sourcing reduced_form_utils.R, these new functions exist:
      build_active_cox_es_split, extract_es_coef_dt_ols,
      extract_es_coef_dt_cox, plot_es_clean
- [ ] Sourcing produces no I/O and no estimation
- [ ] Functions added in Ticket 005 are still present (no regression)

MAIN SCRIPT (2):
- [ ] Code/Analysis/02d_Event_Studies.R exists and runs end-to-end
- [ ] No tryCatch returning NULL; no try(silent = TRUE)
- [ ] Uses here::here() for all paths

FITS:
- [ ] m_es_ols has length 2 with names "main_ref_m1", "main_ref_m2"
- [ ] m_es_cox has length 2 with the same names
- [ ] All fixest fits inherit from "fixest"
- [ ] All coxph fits inherit from "coxph"
- [ ] For each m_es_ols fit, length(coef(m)) ≥ 30 (event-time dummies)
- [ ] For each m_es_cox fit, sum(grepl("^es_tau_", names(coef(m)))) ≥ 30

COEFFICIENT DATA TABLES:
- [ ] All 4 es_dt elements have exactly these columns:
      rel_year, estimate, se, period, ci_lo, ci_hi
- [ ] Each contains a row with estimate == 0 and se == 0 at the
      respective ref_period
- [ ] period column values are exactly {"pre", "event", "post"}

DELIVERABLES (3)-(11):
- [ ] All 9 file paths from deliverables (3)-(10) exist after run
- [ ] T_Event_Study_OLS.tex compiles in a standard article preamble
- [ ] T_Event_Study_Cox.tex compiles in a standard article preamble
- [ ] T_Event_Study_Coefficients_Long.csv has columns matching schema
- [ ] All 4 figure stems exist as BOTH .pdf and .png
- [ ] .rds loads and contains all 4 named top-level elements
- [ ] Log contains "OUTPUT SUMMARY"

═══════════════════════════════════════════════════
ATTEMPT LOG
═══════════════════════════════════════════════════
[Filled by reviewer after each R1 attempt.]

### Attempt 1 — [DATE]
Transcript: 006_transcript.txt
Result: [PASS | TRANSLATION_FAIL | PSEUDOCODE_FAIL]
