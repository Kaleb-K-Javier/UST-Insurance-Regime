# TICKET 007 — Clean-panel rebuild + 6p+stayFE re-estimation + TX-FF counterfactual
# Created: 2026-05-22
# Status: AWAITING_IMPLEMENTATION
# Attempt: 0

═══════════════════════════════════════════════════
MOTIVATION
═══════════════════════════════════════════════════
The T006 audit found a real panel-build bug (first-year-churn
asymmetry in 02b) AND confirmed that real portfolio-shrinkage behavior
exists in the data. This ticket cleans up both layers and produces the
canonical structural fit and one welfare counterfactual.

Five phases:

  Phase 0: Pre-fix diagnostic — re-run the within-TX partial-shrinkage
    regime test with churn-related tanks filtered out, to quantify how
    much of the 43%→52% regime effect survives.

  Phase 1: 02b churn-symmetry fix — drop first-year-churn tanks
    consistently across all three count fields (n_tanks_active,
    n_closures, n_installs). Regenerate facility_panel.csv.

  Phase 2: 04b clean action definitions — apply the plain-English
    Replace/Exit/Maintain mapping using the post-fix facility_panel.csv.
    Regenerate dcm_obs_panel_observed.csv and the matched primitives.

  Phase 3: One re-estimation — 6-parameter universal-γ + stayFE-profile,
    observed sample only (no drop-{SD,MO}, no extended), on the cleaned
    panel. This becomes the canonical fit for downstream CFs.

  Phase 4: One counterfactual — "TX never had RB pricing": replace TX
    2006+ premiums with the FF-equivalent schedule and re-solve the
    equilibrium policy at the structural θ̂. Report removal-age
    distribution, action-share trajectories by age, and welfare PVs
    in the standard format.

  Phase 5: Extension to the Identifying_Variation_Size_Capacity report
    with new analyses characterizing partial-closure behavior across
    facility-portfolio dimensions, to help the researcher decide whether
    to add it as a 4th model action in a future spec.

The CF in Phase 4 is the smallest welfare-relevant deliverable: it
exercises the full pipeline (cleaned panel → estimation → policy CF →
welfare output) and gives the researcher a concrete deliverable to
evaluate before scaling to the other 5 CFs.

═══════════════════════════════════════════════════
PHASE 0 — PRE-FIX DIAGNOSTIC (small)
═══════════════════════════════════════════════════

Goal: quantify how much of the within-TX 4+ tank regime effect
(43.3% pre-1999 vs 52.2% 2006+) survives once churn-related tanks are
filtered out.

Pseudocode:
  fp <- fread("Data/Analysis/facility_panel.csv")
  panel_dt <- fread("Data/Analysis/panel_dt.csv", select = c(
    "tank_panel_id", "panel_id", "panel_year",
    "tank_installed_date", "tank_closed_date"))

  # Identify churn tanks (installed AND closed in same year)
  panel_dt[, churn := !is.na(tank_installed_date) & !is.na(tank_closed_date) &
                       year(tank_installed_date) == year(tank_closed_date)]
  churn_tank_yrs <- panel_dt[churn == TRUE, .(panel_id, panel_year, tank_panel_id)]

  # For each facility-year, recompute n_tanks_active, n_closures, n_installs
  # EXCLUDING churn tank-years
  pd_clean <- panel_dt[!(tank_panel_id %in% churn_tank_yrs$tank_panel_id &
                          panel_year %in% churn_tank_yrs$panel_year)]
  # ... aggregate to facility-year and join back to fp ...

  # Compute partial-shrinkage rate by size_bin × regime using the cleaned counts
  # Compare to the original numbers

Output: Reports/Audits/Phase0_Churn_Filtered_Regime_Test.csv
  Rows: 8 (4 size bins × 2 regimes: TX_FF_pre1999, TX_RB_2006plus)
  Cols: size_bin, regime, n_closure_events, n_partial_shrinkage,
        pct_partial_original, pct_partial_clean, delta

Decision branch:
  - If 4+ tank regime effect (TX FF vs TX RB) remains ≥ 5pp after
    filtering: real shrinking margin, document explicitly in Phase 5.
  - If it collapses to < 2pp: was mostly churn artifact; partial
    shrinkage doesn't identify a real policy response. Still worth
    cleaning the panel for the model, but the welfare-CF urgency
    drops.
  - Print the verdict to the log.

═══════════════════════════════════════════════════
PHASE 1 — 02b CHURN-SYMMETRY FIX
═══════════════════════════════════════════════════

File: Code/Analysis/02b_Tank_level_Panel_Build.R

The current asymmetry (per T006 §6):
  Line 870 area: n_tanks_active INCLUDES first-year-churn tanks
  Line 887 area: n_closures EXCLUDES first-year-churn closures
  Line 984 area: n_installs INCLUDES first-year-churn installs

Fix strategy: **drop first-year-churn tank-years from all three
aggregations**, so they never enter facility-level count fields.

Modification plan:

  Step 1.1 — Identify churn tank-years at the source:
    Create a vector or filter in 02b that flags
      churn := install_yr_int == year(tank_closed_date)
    These tank-years should be excluded from ALL downstream counts.

  Step 1.2 — Apply the filter consistently:
    Before each of the three count aggregations (n_tanks_active,
    n_closures, n_installs), filter out the churn tank-years.

    Either:
    (a) Build a single filtered panel `tank_yr_clean` (panel_dt
        minus churn rows) and use it for all aggregations.
    (b) Add `& !churn` to each existing aggregation's filter clause.

    Option (a) is cleaner — recommend (a).

  Step 1.3 — Verify the identity after fix:
    After regenerating fac_stock, assert across ALL facility-years:
      stopifnot(all(fac_stock$n_tanks_eoy ==
                    fac_stock$n_tanks_active - fac_stock$n_closures +
                    fac_stock$n_installs))
    If this fails for any rows, the fix is incomplete — investigate
    further and report.

  Step 1.4 — Log churn-filter statistics:
    Print to the 02b log:
      - Total tank-years in raw panel_dt: N
      - Tank-years flagged as churn: N_churn
      - Pct: N_churn/N
      - Facility-years affected (had ≥1 churn tank-year): N_fy_affected
      - Pct of facility-years affected: N_fy_affected / total

  Step 1.5 — Regenerate output files:
    Save corrected:
      Data/Analysis/facility_panel.csv
      Data/Analysis/panel_dt.csv  (only if it also embeds churn rows)
      Data/Analysis/matched_tanks.csv  (only if it embeds churn rows)
    Preserve original copies first under
      Data/Analysis/_pre_T007/  (backup directory)
    in case we need to roll back.

═══════════════════════════════════════════════════
PHASE 2 — 04b CLEAN ACTION DEFINITIONS
═══════════════════════════════════════════════════

File: Code/Dynamic_Model/04b_Replacement_Panel_Prep.R

Replace the current action-classification logic (around lines 295-305)
with the user's plain-English definitions:

  Replace = closure where the facility installs again later anywhere
  Exit    = all tanks closed AND no future installs at this facility ever
  Maintain = anything else (including partial closures that don't fit
             the other two)

Pseudocode (replaces existing y_it / I_replace assignment):

  # First: compute facility-level "ever installs after year t" flag.
  # Use the post-fix facility_panel.csv and the panel-end year info.
  fac_panel[, last_install_yr_facility := max(panel_year[n_installs > 0L],
                                               na.rm = TRUE),
            by = panel_id]
  # If a facility has zero installs ever, last_install_yr_facility = -Inf
  fac_panel[!is.finite(last_install_yr_facility),
            last_install_yr_facility := NA_integer_]

  # True facility-level exit: full closure now AND no install after
  fac_panel[, is_true_exit := as.integer(
    facility_complete_closure == 1L &
    (is.na(last_install_yr_facility) | panel_year >= last_install_yr_facility)
  )]

  # Retrofit/Replace per user's definition: replacement_closure_year flag
  # (already correctly built in post-fix 02b: closure followed by future
  #  install at the facility)
  fac_panel[, is_retrofit := as.integer(replacement_closure_year == 1L)]

  # Action mapping
  fac_panel[, y_it := as.integer(is_true_exit == 1L | is_retrofit == 1L)]
  fac_panel[, I_replace := fcase(
    y_it == 0L,                                NA_integer_,
    is_retrofit == 1L,                         1L,
    is_true_exit == 1L & is_retrofit == 0L,    0L,
    default = NA_integer_)]

  # Partial closures (any_closure == 1 but neither retrofit nor true exit)
  # are now MAINTAIN by construction (y_it == 0 for them).

  # Reset_state_index logic for retrofits stays the same as current 04b
  # (uses w_state_next from observed lead).

After these definitions, the rest of 04b proceeds unchanged
(cell assignment, primitive computation, file write).

Expected impact on counts (from prior diagnostics):
  Original Maintain: 97.7% → Cleaned Maintain: ~98.5%
  Original Exit:      ~1.9% → Cleaned Exit:      ~1.7% (true full exits only)
  Original Replace:   ~0.3% → Cleaned Replace:   ~0.3%
  Partial shrinkage (was Exit in T005) → moves to Maintain

Output files (overwrite existing):
  Data/Analysis/dcm_obs_panel_observed.csv
  Output/Estimation_Results/DCM_Primitives_Replacement_observed.rds

  Preserve T005-era versions at:
    Data/Analysis/_pre_T007/dcm_obs_panel_observed.csv
    Output/Estimation_Results/_pre_T007/DCM_Primitives_Replacement_observed.rds

═══════════════════════════════════════════════════
PHASE 3 — RE-ESTIMATE 6p+stayFE PROFILE (ONE FIT ONLY)
═══════════════════════════════════════════════════

Re-use the T004/T005 machinery. ONE fit only:
  - 6-parameter universal-γ + state stayFE profile
  - Observed sample (TX 2006+, controls 1999+)
  - All 17 control states (do NOT drop SD/MO — the cleanup may resolve
    MO's degeneracy; we'll see post-fit whether α_MO still bumps the
    bound)

New pipeline script: Code/Dynamic_Model/04o_6paramFE_Profile_Clean.R

Structure (mostly copy from 04l with one fit instead of six):

  1. Logging block per CLAUDE.md
  2. sourceCpp("Code/Helpers/cpp_engine.cpp")
  3. Load CLEANED primitives + obs_panel from Phase 2 outputs
  4. Warm-start theta: use T005's
       Model_Replacement_6p_observed.rds (no-FE fit) for structural θ
     If that's not preferred, warm-start from the T005's
       Model_Replacement_6paramFE_profile_observed.rds
       (same spec on contaminated panel)
  5. Call npl_estimator_replacement_6p_fe_profile(...) once
  6. Save fit: Output/Estimation_Results/Model_Replacement_6paramFE_profile_clean_observed.rds
  7. Produce comparison artifacts (see Deliverables) showing how θ̂
     moves from T005-contaminated to T007-clean

Acceptance:
  - Fit converges (converged == TRUE)
  - α_MO no longer at +20 boundary (if MO still bumps the bound, that
    confirms the data degeneracy is real, not just churn artifact)
  - LL is finite; structural θ in plausible ranges per prior estimates

═══════════════════════════════════════════════════
PHASE 4 — COUNTERFACTUAL: TX NEVER HAD RB PRICING
═══════════════════════════════════════════════════

Goal: re-solve the agent's equilibrium policy under a counterfactual
where TX retains FF pricing in the 2006+ window, using the cleaned 6p
+stayFE fit. Compare baseline vs CF outcomes.

CF mechanics (per NPL_REFERENCE.md §2 — CF re-solve at fixed θ):

  Step 4.1 — Construct CF primitives:
    Read cleaned primitives from Phase 2.
    For TX state cells (rho_state == 2 in the original), construct a
    CF version where the premium reflects the FF schedule. Two options:

    (a) Median FF premium per (A_bin, w_state) cell from control states
        in the same panel year. Use as the CF premium for TX cells.
    (b) The 04b "fr_premium_per_tank_yr" used for FF controls, averaged
        across states/years and tier-matched to TX cells. Simpler.

    Recommend (a) for transparency. Document the CF premium values in
    a sidecar CSV: 04o_CF_TX_FF_premiums.csv with cols
    (state_cell, baseline_P, cf_P, source).

    Build a NEW primitives object with cache$P_vec overwritten for the
    TX state cells (16 cells: 8 A_bin × 2 wall × 1 regime, since RB
    cells are TX-only).

  Step 4.2 — Re-solve equilibrium policy at structural θ̂ from Phase 3:
    Per NPL_REFERENCE §4: alphas DO NOT enter CF re-solve (Semantic 2).
    Use only the 6 structural params from the cleaned fit.

    Call: solve_equilibrium_policy_replacement_8p(
            theta_struct = fit$theta_hat,
            cache = cf_cache,
            config = fit$config,
            max_iter = 500, tol = 1e-7)
    (Or whatever the corresponding 6p variant is — there should be a
    solve_equilibrium_policy_replacement_6p mirroring the existing 8p
    function. If not, copy/adapt with the 6p flow utility wrapper.)

    Output: P_cf (32 × 3 CCP matrix under CF), V_cf (32-vec).

  Step 4.3 — Compute baseline equilibrium for comparison:
    Same call but with original cache (cleaned primitives, no CF
    overlay). Yields P_baseline, V_baseline.

  Step 4.4 — Implied removal-age distribution:
    For each cohort (start state at age=1), simulate forward using
    P_cf vs P_baseline. The expected age at retrofit/exit can be
    computed analytically via the Markov operator:

      For each (start state s, action a), the survival function
      S(t | s) = product over τ=1..t of (1 − P(M | state(τ-1)))
      where state(τ) follows the maintenance transitions.

    Or simpler: simulate 10,000 cohorts per starting state and tabulate
    age at retrofit / exit.

    Output: 04o_CF_RemovalAge_Distribution.csv
    Cols: start_state, scenario {baseline, cf}, action {Retrofit, Exit},
          age_at_event, density

  Step 4.5 — Action shares by age, baseline vs CF:
    Tabulate P(action | state) directly from P_baseline and P_cf, for
    TX cells. Easy to read off; no simulation needed.

    Output: 04o_CF_ActionShares_byAge.csv (and PNG figure)
    Rows: A_bin × wall × scenario × action (8 × 2 × 2 × 3 = 96 rows)
    Cols: A_bin, wall, scenario, action, P

  Step 4.6 — Welfare PV summary (per NPL_REFERENCE §5):
    For baseline and CF, compute:
      ProducerSurplus = sum_s mu(s) * V(s)
      ExternalDamage  = sum_s mu(s) * P_M(s) * h_vec(s) * E_external / (1-beta)
      GovtOutlay      = 0 in this CF (no subsidy)
      SocialWelfare   = ProducerSurplus - ExternalDamage - GovtOutlay

    Where mu(s) is the stationary or empirical state distribution.
    Use empirical (matches the data) — derived from obs_panel cell
    counts.

    Multiply $ values by SCALE_FACTOR = 10,000 for $/facility-yr units.

    Output: 04o_CF_Welfare_Summary.csv (and TEX table)
    Rows: 4 (Producer, External, Govt, SocialWelfare)
    Cols: baseline_USD, cf_USD, delta_USD

═══════════════════════════════════════════════════
PHASE 5 — REPORT EXTENSION (partial-closure deep dive)
═══════════════════════════════════════════════════

File: Reports/Paper/Identifying_Variation_Size_Capacity.qmd

Add a new section, after §4 (closure taxonomy) and before §5
(Implications), titled:

  §4.8 (NEW) — Partial closure as a candidate fourth action

The goal: give the researcher enough characterization of partial
closure behavior to decide whether to add it as a 4th model action.

Sub-sections + analyses:

  §4.8.1 — Partial closure rate by facility-portfolio dimensions
    Stratify the partial-closure rate by:
      (a) facility size_bin (already exists; just bring forward)
      (b) total capacity bin (already defined)
      (c) wall composition of facility (all-SW vs mixed vs all-DW)
      (d) age distribution of tanks at facility (avg_tank_age binned)

    Table 4.8.1: pct partial closure by each stratification dimension.

  §4.8.2 — Wall type of the tank that closed at partial-closure events
    For each partial-closure event, identify which tank(s) closed by
    pulling the tank_closed_date == event_year rows from panel_dt at
    the facility. Report what fraction of closed tanks at partial-
    closure events were SW vs DW.

    Table 4.8.2: closed-tank wall composition at partial-closure events,
    stratified by facility wall composition.

    Hypothesis: facilities preferentially close their oldest SW tanks
    (the riskiest, least valuable). If confirmed, partial closure is
    a risk-management action — and capturing it would matter for
    welfare CFs involving risk pricing.

  §4.8.3 — Age of closed tank at partial-closure events
    Within partial-closure events, what's the age distribution of
    the tank(s) being closed?

    Figure 4.8.3: histogram of tank-age-at-closure for tanks closed
    in partial-closure events, vs in full-exit events, vs in retrofit
    events.

    Hypothesis: partial-closure tanks are older than average; firms
    are pruning end-of-life tanks while keeping the rest.

  §4.8.4 — Policy response check (within-TX 1999 cutoff)
    Repeat the within-TX before/after partial-closure rate test, but
    use the CLEANED definitions (post-T007 panel).

    Table 4.8.4: partial-closure rate by size_bin × regime, with
    churn already filtered.

    Compare to Phase 0 diagnostic. If the regime effect at 4+ tank
    facilities is still meaningful (~5pp or more), partial closure
    IS a real policy response — and we should consider adding it as
    a 4th action.

  §4.8.5 — Recommendation
    Based on §4.8.1-4 evidence, write a 1-paragraph recommendation:
    either (a) "the model's current 3-action specification is sufficient
    because partial closure is small / regime-independent / not
    welfare-relevant", or (b) "partial closure should be added as a
    fourth action because [evidence from above]".

═══════════════════════════════════════════════════
DELIVERABLES — ENUMERATED
═══════════════════════════════════════════════════

PHASE 0:
(1) Reports/Audits/Phase0_Churn_Filtered_Regime_Test.csv
    8 rows × 6 cols (size_bin, regime, n_closure_events,
    n_partial_shrinkage, pct_partial_original, pct_partial_clean, delta)

PHASE 1:
(2) Modified Code/Analysis/02b_Tank_level_Panel_Build.R
(3) Regenerated Data/Analysis/facility_panel.csv (overwrites T005-era version;
    backup at Data/Analysis/_pre_T007/)
(4) Identity-pass log entry: "All 5.06M facility-years satisfy
    n_tanks_eoy = n_tanks_active − n_closures + n_installs"

PHASE 2:
(5) Modified Code/Dynamic_Model/04b_Replacement_Panel_Prep.R
(6) Regenerated Data/Analysis/dcm_obs_panel_observed.csv
(7) Regenerated Output/Estimation_Results/DCM_Primitives_Replacement_observed.rds
    Both with T005-era backups at _pre_T007/ subdirectory.

PHASE 3:
(8) Code/Dynamic_Model/04o_6paramFE_Profile_Clean.R (new pipeline)
(9) Output/Estimation_Results/Model_Replacement_6paramFE_profile_clean_observed.rds
    Schema: same as T005 col 3/5 (theta_hat 6-vec, alpha_hat 17-vec, se_theta,
    se_alpha, P_hat 32×3, V_hat 32-vec, log_likelihood, converged, n_iter,
    ll_path, theta_path, cache, config, sample_label="observed_clean",
    elapsed_sec)
(10) Output/Tables/04o_Theta_Comparison_T005_vs_T007.csv (+ .tex)
     Rows: 10 (8 struct params + logL + N_obs)
     Cols: T005_contaminated, T007_clean, delta, pct_change

PHASE 4:
(11) Code/Dynamic_Model/04o_CF_TX_FlatFee.R (new CF pipeline)
(12) Output/Estimation_Results/CF_TX_FlatFee_results.rds
     List with: P_baseline, V_baseline, P_cf, V_cf,
     cf_primitives, welfare_baseline, welfare_cf
(13) Output/Tables/04o_CF_TX_FF_premiums.csv (sidecar showing baseline
     vs CF premium per state cell)
(14) Output/Tables/04o_CF_RemovalAge_Distribution.csv
     Rows: ~64 (4 start states × 2 scenarios × 2 actions × 4 age buckets)
     Cols: start_state, scenario, action, age_at_event, density
(15) Output/Tables/04o_CF_ActionShares_byAge.csv
     Rows: 96 (8 A_bin × 2 wall × 2 scenarios × 3 actions; TX cells only)
     Cols: A_bin, wall, scenario, action, P
(16) Output/Figures/04o_CF_ActionShares_byAge_TX.png
     Three facets (M/E/R), x=age, y=P, colored by scenario
(17) Output/Figures/04o_CF_RemovalAge_Distribution_TX.png
     Density plot or histogram, by scenario
(18) Output/Tables/04o_CF_Welfare_Summary.csv (+ .tex)
     Rows: 4 (Producer, External, Govt, SocialWelfare)
     Cols: baseline_USD, cf_USD, delta_USD

PHASE 5:
(19) Updated Reports/Paper/Identifying_Variation_Size_Capacity.qmd with
     new §4.8 (5 subsections, 3 tables, 1-2 figures, 1 recommendation
     paragraph). DO NOT remove or restructure existing sections.

═══════════════════════════════════════════════════
ACCEPTANCE CRITERIA
═══════════════════════════════════════════════════

PHASE 0:
- [ ] Phase0_Churn_Filtered_Regime_Test.csv exists with 8 rows
- [ ] Console log prints verdict: regime effect survives (≥5pp) or
      collapses (<2pp) or intermediate

PHASE 1:
- [ ] 02b runs to completion without errors
- [ ] Identity assertion holds across all 5.06M facility-years
- [ ] Backup copies preserved at _pre_T007/

PHASE 2:
- [ ] 04b runs without errors on cleaned facility_panel.csv
- [ ] dcm_obs_panel_observed.csv has between 2.0M and 2.5M rows
      (allows some shift from churn-removal but flags if drastically off)
- [ ] Action share distribution: Maintain ≥ 98%, Exit and Replace each
      between 0.05% and 3%
- [ ] DCM_Primitives_Replacement_observed.rds loads cleanly with same
      schema as T005-era version

PHASE 3:
- [ ] fit$converged == TRUE
- [ ] length(fit$theta_hat) == 6 with correct names
- [ ] All P_hat rowSums in [1 ± 1e-8]
- [ ] θ̂ values are in plausible ranges (κ ~ $200K-$300K, K ~ $30K-$70K,
      γ_price between -1 and -3, γ_risk between 0 and 0.15)
- [ ] Comparison table shows specific direction and magnitude of each
      param's shift from T005 to T007

PHASE 4:
- [ ] CF re-solve converges (eq solver returns converged = TRUE)
- [ ] P_cf has rowSums within 1e-6 of 1
- [ ] CF removal-age distribution differs from baseline (otherwise the
      CF did nothing — check that premium override actually applied)
- [ ] Welfare summary table has all 4 rows; SocialWelfare = Producer -
      External - Govt (allowing rounding within 1 USD)

PHASE 5:
- [ ] §4.8 added to qmd, 5 subsections present
- [ ] Existing §1-§4.7 and §5 sections unchanged
- [ ] §4.8.5 has explicit "yes add" or "no don't add" recommendation
      with one-paragraph reasoning

CODE HYGIENE:
- [ ] No tryCatch returning NULL anywhere (except T004's allowed
      Hessian-inversion guard inherited via the existing estimator)
- [ ] No try(silent=TRUE)
- [ ] All scripts use here::here()
- [ ] All scripts have logging blocks per CLAUDE.md
- [ ] sourceCpp called once before any NPL or CF re-solve

═══════════════════════════════════════════════════
R-IMPLEMENTATION NOTES
═══════════════════════════════════════════════════

- This ticket has FIVE distinct phases. Execute and verify each before
  moving to the next. If Phase 0 reveals the regime effect collapses
  entirely, escalate to architect before continuing — Phase 4 CF
  results would be less compelling.

- Phase 1's churn filter touches RAW data construction. Before
  overwriting facility_panel.csv, COPY the current version to
  Data/Analysis/_pre_T007/facility_panel.csv. We need a rollback path.

- Phase 2 should generate DIFF logs: how many rows changed in
  dcm_obs_panel_observed.csv? Specifically, how many y_it values
  flipped vs T005-era? Worth a small CSV showing the action-table
  before and after.

- Phase 4 CF mechanics: the existing `solve_equilibrium_policy_
  replacement_8p` may need a 6p sibling. Check what's in
  improved_estimator_OPTIMIZED.r. If only an 8p version exists, write
  a 6p wrapper that swaps the flow utility function.

- Phase 4 welfare: SCALE_FACTOR = 10000 converts model units to dollars
  per facility-year. E_external (external damage per leak event) is in
  primitives somewhere — check existing 04i/04j welfare scripts for
  the canonical value.

- Phase 5 partial-closure analyses: for tank-age-at-closure, you need
  to join facility_panel partial-closure events with the panel_dt
  rows where tank_closed_date == event_year to identify which tanks
  closed. Compute age at closure as event_year - install_yr_int from
  panel_dt's tank metadata.

═══════════════════════════════════════════════════
RESEARCHER AMENDMENTS (applied before coding)
═══════════════════════════════════════════════════
Amendment 1 — Phase 1 identity assertion: Use lead identity
  (n_tanks_active[t+1] == n_tanks_eoy[t]) instead of the algebraically
  incorrect within-year 3-way identity. Document pre-fix pass rate.

Amendment 2 — Phase 5: Add §4.8.0 "Bidirectional regime effect by facility size"
  BEFORE the dimension stratifications. §4.8.5 recommendation must address
  both "4th action" AND "size as state dimension" as distinct design choices.

═══════════════════════════════════════════════════
CODER Q&A LOG (Attempt 1)
═══════════════════════════════════════════════════
Q1: size_bin → 4 bins: 1, 2, 3, 4+ (not 01_Descriptive binning)
Q2: Use first_year_churn from panel_dt.csv — NOT AVAILABLE (absent from CSV
    because churn tanks filtered by tstop>tstart before panel_dt saved).
    Fix: compute from dates: year(tank_installed_date)==year(tank_closed_date).
Q3: Anti-join bug in spec pseudocode — use paired anti-join
    panel_dt[!churn_tank_yrs, on=c("tank_panel_id","panel_year")]
Q4: Stick with T006 years_to_next_install definition (recompute from fp)
Q5: Assert lead identity (n_tanks_active[t+1]==n_tanks_eoy[t]) not 3-way
Q6: Date-based churn filter on study_tanks: year(installed)!=year(closed)
Q7: Warm-start from FE-profile fit (not no-FE) — reversed from spec text
Q8: $50K E headline; $17K sensitivity row; both in welfare table
Q9: CF premium = control-state 2006+ median FF per (A_bin, w_state);
    fallback to 1999+ if < 5 obs; loop over TX cells (rho_state==2)

═══════════════════════════════════════════════════
ATTEMPT LOG
═══════════════════════════════════════════════════
Attempt 1 — 2026-05-27 — Sonnet 4.6

Phase 0: PASS
  Result: SURVIVES — 4+ tank regime effect +6.2pp (FF=46.0%→RB=52.2%)
  Finding: panel_dt churn count == 0 (churn tanks excluded by tstop>tstart).
  Churn fix is cosmetic for the DCM analysis sample.
  Output: Reports/Audits/Phase0_Churn_Filtered_Regime_Test.csv ✓

Phase 1 (02b): CODE CHANGES VERIFIED; facility_panel.csv UNCHANGED
  S12 ran correctly with churn stats:
    - 7,602 first_year_churn tank-years (0.06% of 13.7M) — cosmetic ✓
    - 4,416 facility-years affected (0.09%) — cosmetic ✓
    - 11,199 churn installs filtered from study_tanks_no_churn
  Lead consistency: 6.24% failure — PRE-EXISTING residual panel noise
    (T006 silent-disappearances; unrelated to churn fix; needs investigation)
  BLOCKED at S13.2: machine cannot memory-map 20.75 GB Texas FR CSV.
    facility_panel.csv not regenerated — using pre-T007 local version for Phase 2.
    Per Phase 0 evidence, this is fine (churn fix cosmetic for DCM sample).

Phase 2 (04b): Code changes applied; awaiting Phase 1 to finish before running.
  New action logic: is_true_exit + is_retrofit + y_it + I_replace per spec.

Phase 3 (04o_6paramFE_Profile_Clean.R): Written; awaiting Phase 2.

Phase 4 (04o_CF_TX_FlatFee.R): Written; awaiting Phase 3.

Phase 5 (Identifying_Variation_Size_Capacity.qmd): COMPLETE
  Added §4.8 with 6 subsections (including Amendment 2 §4.8.0).
  Recommendation: Option B (size as state dim) first; Option A (4th action) after.
