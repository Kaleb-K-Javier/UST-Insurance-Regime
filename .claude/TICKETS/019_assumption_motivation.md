# TICKET 019 — Data-motivated assumptions for the portfolio/size DCM
# Created: 2026-06-08
# Status: REVIEWED-PASS (attempt 1, 2026-06-09) — see Attempt Log caveats (M3 Eq.4/Eq.6)
# Attempt: 1
# Type: READ-ONLY DESCRIPTIVE (no structural estimation; no .rds model outputs)

═══════════════════════════════════════════════════
ECONOMIC MOTIVATION
═══════════════════════════════════════════════════
The improved DCM adds (a) facility SIZE/composition to the state, (b) a faithful
REPLACE (consolidation) transition, and keeps maintain/exit/replace as the action
set (downsizing folded into maintain). A structural model is a claim about how
behavior varies ACROSS STATES, so each modeling choice must be motivated by a
STATE-CONDITIONAL data fact: holding (age, wall, regime) fixed, behavior/transition
varies with the dimension in question. Each fact = one FIGURE (the pattern) + one
DISCRETE-CHOICE / descriptive REGRESSION (the formal test, with the rest of the
state as controls). Feeds the talk's assumption slides (each bullet -> a button to
its evidence) and the identification appendix. NOTHING here estimates the structural
model; these are reduced-form descriptive mirrors of the CCPs/transitions.

Four analyses:
  M1  size belongs in the state            (binary logit, CCP ~ size | state)
  M2  action set: replace != downsize      (binary logit per margin, action ~ state x regime)
  M3  replace transition = consolidation   (Δcount/Δcap/Δwall by install vs no-install)
  M4  which tank is acted on               (CONDITIONAL logit; replaces biased 04ai benchmark)

Already built, CITE — do not rebuild: 04ag (size stickiness; frame on the SURVIVAL
curve, not the spell-pooled "never changed"), 04ah (within-facility homogeneity).
RETIRED: 04ai's age/capacity "vs random ~E[1/n]" numbers are tie-biased (most
facilities are age-homogeneous, so "oldest" is trivial) — superseded by M4's clogit.
04ai's single-walled result (79% vs 52% base) carries over and is reproduced by M4.

ASSUMED (state plainly in the paper, no data test): beta = 0.95; Type-1 EV choice
errors; scale-normalization sigma. These are conventional and not data-motivated.

═══════════════════════════════════════════════════
REGRESSION SPECIFICATIONS (the "MATH")
═══════════════════════════════════════════════════
Notation: facility-year i,t. State cell s_idx in 1..32 = (A_bin x w_state x rho_state).
size_bin in {1,2,3,4+} from boy_stock. regime = rho_state (1=FF, 2=RB).

Both M1 and M2 use the 04z 5-category true-action coding and the SAME estimator:
a separate BINARY LOGIT per non-maintain action margin (vs maintain), fixest::feglm
with clustered SE by state -> native clustered SE, NO bootstrap. (One-vs-maintain
binary logits, not a joint multinomial: clustered SEs come for free this way; a joint
multinomial would need a cluster bootstrap = overkill for descriptive evidence.)
"ALL MARGINS" = run it for every margin: Exit, Replace-upgrade, Replace-samewall,
Downsize. No joint LR/Wald test anywhere -- the per-coefficient ORs + clustered SE
ARE the evidence.

M1 (SIZE is the test; state held fixed by s_idx FE):
  Eq.1   logit P(action_it = k) = a_{s_idx} + b_k' * size_bin        for each margin k
  Report OR = exp(b_k) for size {2,3,4+} (vs size=1), clustered SE, per-coef p.
  Size gradient (ORs ordered / away from 1) = size shifts behavior at fixed state.

M2 (REGIME is the test; age/wall/size controlled; YEAR FE absorb secular time):
  Eq.2   logit P(action_it = k) = a_year + a(age,wall,size) + e_k*RB + f_k*(RB x SW)
  YEAR FE REQUIRED: RB = TX x post-1999, so without year FE e_k absorbs any secular
  trend in the action (e.g. the 1998 federal-deadline wave). Regime still identified:
  control states are FF in every year, so RB varies cross-state WITHIN year.
  Test: e_{Replace-upgrade} > 0 AND e_{Downsize} < 0 (opposite regime response);
        f_{Replace-upgrade} > 0 (replace targets SW). Report ORs + clustered SE.
  CAVEAT: RB is concentrated in the switching state(s) -- report n_states_RB. With RB
  in few clusters the clustered SE on the REGIME term is limited (few-treated-clusters);
  the causal regime claim defers to the DiD. Within-state terms cluster fine. (CRVE
  computes without failure -- this is exactly why no bootstrap is needed.)

M3 (transition characterization + simple tests):
  Population: facility-years that KEEP OPERATING (not facility_complete_closure) AND
              had any portfolio change: (n_installs > 0 OR any_closure == 1).
              This INCLUDES net-0 swaps (close 1 + install 1, count unchanged) -- the
              MODAL replace (~31% per T011-A7) that a net_tank_change<0 filter wrongly
              drops. Split: consolidation = (n_installs > 0); pure_shrink = (n_installs==0).
  Eq.4   dcap_pct_it      = h0 + h1 * consolidation_it + u          (feols, % cap change)
  Eq.5   net_tank_change_it = j0 + j1 * consolidation_it + u        (feols)
  Eq.6   logit P(single_to_double_year=1) = m0 + m1 * consolidation (feglm logit)
  All SEs CLUSTERED BY STATE (fixest cluster=~state).
  Report distributions (below) + (h1, j1, m1) with clustered SE. NO assumed sign on Δcapacity
  (it can be negative, zero, OR positive -- report the empirical spread).

M4 (CONDITIONAL logit — McFadden, strata = facility-year):
  Choice occasion = facility-year with EXACTLY one closure in a multi-tank facility
  (the 04ai target sample). Alternatives = the facility's active tanks.
  Eq.7   P(tank j closed | strata) = exp(x_j' beta) / sum_{k in strata} exp(x_k' beta)
         x_j = (tank_age_j, SW_j, capacity_j)     [within-facility variation only]
  strata(facility_year). Conditioning on the strata removes the facility FE and
  handles ties correctly (a same-age facility contributes no age variation).
  SE CLUSTERED BY STATE (robust grouped SE via + cluster(state); G=18).
  Report odds ratios exp(beta) for age (per year), SW (vs DW), capacity (per 1000 gal).

═══════════════════════════════════════════════════
PSEUDOCODE
═══════════════════════════════════════════════════
TWO scripts (isolate the heavy 3.9GB panel_dt read in M4):
  Code/Analysis/04aj_Assumption_Motivation.R   -> M1, M2, M3
  Code/Analysis/04ak_WhichTank_clogit.R        -> M4

------ 04aj : shared prep (M1 + M2) — true-action coding ------
Input:  dcm_obs_panel_observed.csv (panel_id, panel_year, s_idx, A_bin, w_state,
        rho_state, size_bin, boy_stock, state) + facility_panel.csv (04z join cols)
Prep (ONCE, reused by M1 and M2):
  - filter boy_stock >= 1; size_bin <- factor(pmin(boy_stock,4L),1:4,c("1","2","3","4+"))
  - true_event from 04z_Model_vs_Reality.R fcase (merge keys panel_id, panel_year;
    flags any_closure, facility_complete_closure, replacement_closure_year,
    permanent_closure_year, n_installs, single_to_double_year) ->
      {No-close, Exit, Replace-upgrade, Replace-samewall, Downsize}.
    DO NOT re-derive a different coding.
  - regime <- as.integer(rho_state == 2L)   # RB = 1
  - MARGINS <- c("Exit","Replace-upgrade","Replace-samewall","Downsize")

------ M1 — size gradient (SIZE is the test) ------
  - For each margin k in MARGINS: y_k <- as.integer(true_event == k);
      feglm(y_k ~ size_bin | s_idx, family = binomial(), cluster = ~state)
      -> OR=exp(coef), clustered SE, p for size {2,3,4+}.
  - FIGURE M1_CCP_by_Size.png: empirical share of EACH margin by (A_bin, size_bin),
      facet_grid(margin ~ w_state) [or margin ~ rho_state]; x=A_bin, color=size_bin,
      lines+points; y = % with axis SCALED TO THE DATA RANGE per facet (free y; do
      NOT force 0-100% -- shares are <2%). ALL four margins shown.
  - TABLE M1_Size_Logit.csv + .tex.
  assert: nrow > 2.0e6; size_bin 4 levels; all OR finite.

------ M2 — action set: regime contrast (REGIME is the test) ------
  - For each margin k in MARGINS: y_k <- as.integer(true_event == k);
      feglm(y_k ~ A_bin + w_state + size_bin + regime + regime:w_state | panel_year,
            family = binomial(), cluster = ~state)
      # panel_year FE via the fixest `|` slot. RB varies cross-state within year
      # (controls always FF), so the regime terms survive the year FE.
      -> extract regime (e_k) and regime:w_stateSW (f_k): OR, clustered SE, p.
  - n_states_RB <- uniqueN(state[regime == 1L]).
  - FIGURE M2_ActionShares_by_State_Regime.png: empirical share of {Exit,
    Replace-upgrade, Downsize} by A_bin, facet_grid(w_state ~ regime); exclude
    No-close; y-axis SCALED TO DATA RANGE (free y, NOT 0-100%).
  - TABLE M2_Action_Logit.csv + .tex.
  assert: report e_{Replace-upgrade} and e_{Downsize} signs (opposite-sign test;
    report both even if not opposite — not a hard stop).

------ 04aj : Section M3 — replace transition ------
Input:  facility_panel.csv
        cols: panel_id, panel_year, state, net_tank_change, capacity_change,
              n_installs, any_closure, single_to_double_year, facility_complete_closure,
              total_capacity, lag_capacity
Steps:
  - events <- facility_panel[facility_complete_closure == 0 &
                             (n_installs > 0 | any_closure == 1)]   # any change while operating
      # INCLUDES net-0 swaps; do NOT filter on net_tank_change<0.
  - consolidation <- as.integer(n_installs > 0); (pure_shrink = 1 - consolidation)
  - dcap_pct <- 100 * capacity_change / lag_capacity (guard lag_capacity>0);
      DO NOT assume sign (report negative, zero, and positive mass).
  - FIGURE M3_Transition_by_Action.png: two panels —
      (a) histogram/density of dcap_pct by event type (consolidation vs pure_shrink),
      (b) bar of net_tank_change distribution by event type; annotate %SW->DW.
  - DISTRIBUTION TABLE M3_Transition_Distribution.csv (rows = event x dcount-bin).
  - TESTS (Eq.4-6), SE CLUSTERED BY STATE via fixest:
      feols(dcap_pct ~ consolidation, cluster=~state);
      feols(net_tank_change ~ consolidation, cluster=~state);
      feglm(single_to_double_year ~ consolidation, binomial, cluster=~state).
      -> M3_Transition_Tests.csv (clustered SE).
  - TABLE M3_Transition_Summary.tex (slide-ready: by event type, median Δcount,
      median Δcap%, %SW->DW).
  assert: both event types non-empty; dcap_pct reported with min<0 and max>0
    possible (no sign filter).

------ 04ak : Section M4 — which tank (conditional logit) ------
Input:  Data/Analysis/panel_dt.csv
        select cols: tank_panel_id, panel_id, panel_year, state, mm_wall, capacity,
                     tank_age, closure_event
  R-NOTE: filter-first to avoid OOM (see 04ai): compute n_active, n_closed by
          (panel_id, panel_year); target = (n_active>=2 & n_closed==1); subset.
Steps:
  - sw <- grepl("single", mm_wall, ignore.case=TRUE)   # "Single-Walled"/"Double-Walled"
  - strata_id <- paste(panel_id, panel_year)
  - keep only target facility-years (multi-tank, single closure); ~19,744 strata.
  - REGRESSION: survival::clogit(closure_event ~ tank_age + sw +
      I(capacity/1000) + strata(strata_id) + cluster(state))
      -> robust SE clustered by state (G=18); clogit passes cluster() to coxph.
      FALLBACK if in-formula cluster() errors in this survival version: fit clogit
      WITHOUT cluster(), then cluster on state via sandwich::vcovCL(fit, cluster=state).
      Report whichever path succeeds; the SE must be state-clustered either way.
  - FIGURE M4_WhichTank_clogit.png: coefficient/odds-ratio plot (forest) for the
      three terms with 95% CI.
  - TABLE M4_WhichTank_clogit.csv + .tex (odds ratios).
  - Print: n_strata, n_obs, and confirm the SW odds ratio is > 1 (reproduces the
    04ai single-walled finding); age/capacity ORs are the corrected, tie-robust
    estimates.
  assert: n_strata between 19000 and 20500; all OR finite; clogit converged.

═══════════════════════════════════════════════════
DELIVERABLES (enumerated — files, rows, columns, types)
═══════════════════════════════════════════════════
FIGURES (Output/Figures/, PNG, dpi=150):
  M1_CCP_by_Size.png
  M2_ActionShares_by_State_Regime.png
  M3_Transition_by_Action.png
  M4_WhichTank_clogit.png

TABLES — each as CSV (Output/Tables/) AND a slide-ready .tex (same name .tex):
  M1_Size_Logit.csv
    rows: {size2,size3,size4+} x margin {Exit,Replace-upgrade,Replace-samewall,Downsize} (12)
    cols: margin[char], size_term[char], odds_ratio[num], coef[num], se[num], p[num], n[int]
    (se = state-clustered CRVE; NO joint-test row)
  M2_Action_Logit.csv
    rows: {Exit, Replace-upgrade, Replace-samewall, Downsize} x {regime_RB, regimeRB:SW} (8)
    cols: margin[char], term[char], odds_ratio[num], coef[num], se[num], p[num],
          n_states_RB[int]   (se = state-clustered CRVE; no bootstrap)
  M3_Transition_Distribution.csv
    rows: {consolidation, pure_shrink} x dcount_bin{0,-1,-2,<=-3}  (8)
    cols: event_type[char], dcount_bin[char], n[int], share[num],
          median_dcap_pct[num], p25_dcap[num], p75_dcap[num],
          share_cap_within10pct[num], share_SWtoDW[num]
  M3_Transition_Tests.csv
    rows: {dcap_pct~consol, net_tank_change~consol, SWtoDW~consol}  (3)
    cols: outcome[char], term[char], coef[num], se[num], p[num]   (se = state-clustered)
  M4_WhichTank_clogit.csv
    rows: {tank_age, SW, capacity_per_1000gal}                  (3)
    cols: term[char], odds_ratio[num], coef[num], se[num], p[num],
          n_strata[int], n_obs[int]

.tex CONVENTION (every table): centered, fit-to-slide, pre-escaped —
  \begin{center}\small\renewcommand{\arraystretch}{1.2}\begin{tabular}{...}
    ... \end{tabular}\end{center}  + a {\centering\scriptsize Notes...\par}.
  Escape: % -> \%, _ -> \_, & in cell text handled; NEVER a `$...$` whose closing
  `$` sits immediately before a digit (pandoc gotcha) — use `$\sim$\,2\%` form.
  Keep each .tex <= ~8 rows so it fits a slide.

═══════════════════════════════════════════════════
R-IMPLEMENTATION NOTES
═══════════════════════════════════════════════════
- Packages: data.table, ggplot2, here, scales; fixest (M1/M2/M3 feglm/feols +
  cluster); survival + sandwich (M4 clogit). nnet NOT needed. If a package is
  missing, install.packages it (do not silently skip an analysis).
- M1/M2 = separate BINARY logits per action margin via fixest::feglm(y_k ~ ...,
  family = binomial(), cluster = ~state): native state-clustered SE, NO bootstrap,
  one fit per margin. Report OR=exp(coef) + clustered SE + p. NO joint LR/Wald test --
  the per-coefficient ORs ARE the evidence. (A joint multinomial would need a cluster
  bootstrap for clustered SE = overkill here.) M3 tests likewise feols/feglm cluster=~state.
- M2 true_event coding MUST reuse 04z_Model_vs_Reality.R's fcase logic verbatim
  (merge keys panel_id, panel_year). Re-deriving a different coding = PSEUDOCODE_FAIL.
- M4: filter-first BEFORE any by-group ranking (the full 12.7M-row group-by OOMs;
  see 04ai). Read panel_dt with fread(select=...) only the 7 needed cols.
- Figures: theme_minimal(base_size=13); regime colors FF=#E76F51, RB=#2A9D8F; size
  bins sequential blues; percent y-axes via scales::percent_format(accuracy=0.1);
  bold titles, grey30 subtitles. Match 04ag/04ah/04ae styling. SCALE Y-AXES TO THE
  DATA RANGE (facet_grid(..., scales="free_y"); never force 0-100% when shares are
  <2%) so the pattern is visible.
- NO tryCatch returning NULL / no silent skips (CLAUDE.md). Hard errors surface.
- Logging: cat("=== SECTION ===") markers; the rstudioapi sink idiom BREAKS under
  Rscript — instead optionally sink() to a hardcoded logs/04aj_*.log path, or just
  rely on the runner capturing stdout. Loop/fit checkpoints via cat.
- These are RUN BY THE RESEARCHER VIA THE CODER RUNNER; outputs land in
  Output/Figures + Output/Tables only. No Output/Estimation_Results writes.

═══════════════════════════════════════════════════
ACCEPTANCE CRITERIA (binary — reviewer checks against code + output)
═══════════════════════════════════════════════════
- [ ] Two scripts created: 04aj_Assumption_Motivation.R (M1-M3), 04ak_WhichTank_clogit.R (M4)
- [ ] ALL SEs CLUSTERED BY STATE (G=18) via fixest CRVE (M1/M2/M3 feglm/feols
      cluster=~state) and M4 clogit cluster(state) (or sandwich::vcovCL fallback).
      NO bootstrap, NO plain lm/glm, NO model SEs.
- [ ] M1: binary LOGIT per margin (Exit/Replace-upgrade/Replace-samewall/Downsize)
      on size_bin with s_idx FE, clustered SE; reports size ORs per margin; NO joint
      test; figure shows ALL margins by size with DATA-RANGE (free-y) axes
- [ ] M2: binary LOGIT per margin on regime + RB:SW (state controls) WITH panel_year
      FE (fixest `| panel_year`), clustered SE, NO bootstrap; reports RB coef for
      Replace-upgrade and Downsize (opposite-sign test), RB:SW for Replace-upgrade,
      and n_states_RB
- [ ] M1/M2 true_event reproduces 04z's 5-category coding (Exit/Downsize/Replace-
      upgrade/Replace-samewall/No-close) via the same merge + fcase
- [ ] M3 events = operating AND (install OR closure), INCLUDING net-0 swaps (NO
      net_tank_change<0 filter); splits by install vs no-install; Δcapacity as % with
      NO sign filter (table shows negative, zero, and positive mass)
- [ ] M3 reports the three test coefficients (Eq.4-6)
- [ ] M4 uses survival::clogit with strata = facility-year AND cluster(state) for
      robust clustered SE; reports age/SW/capacity ORs; n_strata in [19000, 20500];
      SW odds ratio > 1 (reproduces 04ai SW finding)
- [ ] All 4 figures saved as PNG in Output/Figures/
- [ ] All 5 tables saved as BOTH .csv and slide-ready .tex in Output/Tables/, with
      % escaped as \% and no `$`-before-digit
- [ ] No tryCatch returning NULL; no silent analysis skips
- [ ] Scripts run end-to-end exit 0; logs/stdout captured

═══════════════════════════════════════════════════
ATTEMPT LOG
═══════════════════════════════════════════════════
ATTEMPT 1 — 2026-06-09 — VERDICT: PASS (Opus acting reviewer; verified from code +
output CSVs + run logs, not the R1 report).

FAITHFULNESS VERIFIED:
- fcase byte-identical to 04z_Model_vs_Reality.R:26-31 (same branch order, NA-fill
  loop, merge keys, all.x=FALSE); rename via named lookup AFTER fcase. Category
  counts sum to 2,281,442 = post-filter N. size_bin re-derived per spec.
- All SEs state-clustered: M1/M2/M3 fixest cluster=~state; M4 in-formula
  cluster(state) succeeded (G=18). No bootstrap, no plain glm, no joint tests.
- M2 has the panel_year FE (fixest `|` slot) per the 2026-06-09 spec amendment.
- M3 population = operating AND (install OR closure), net-0 swaps included, no
  sign filter on dcap_pct (positive mass present in the table).
- M4: n_strata = 19,744 (exactly 04ai's sample, in [19000,20500]); SW OR = 2.85 > 1
  reproduces the 04ai finding; all ORs finite; both scripts exit 0; 4 figures +
  5 CSV+.tex pairs on disk; .tex escapes OK, no $-before-digit, all <= 8 rows.

BLESSED DEVIATIONS (mechanical, no economics):
- D1 M2 interaction extracted by order-robust regex (fixest names factor first).
- D2 M4 clogit method="approximate": with exactly one event per stratum the
  Breslow and exact conditional likelihoods COINCIDE (denominator = sum over the
  strata risk set either way), so point estimates are identical and robust/
  clustered variance becomes available. The spec's sandwich fallback stays wired.
- D3 M4 NA/capacity>0 filter applied before strata construction — empirically
  inert (strata count matches 04ai exactly).

CAVEATS FOR THE ARCHITECT/RESEARCHER (spec-design, NOT implementation defects —
the code is faithful; these affect how M3/M2 numbers are USED later):
- C1 M3 Eq.4 contaminated by entry/growth years: the population definition
  (n_installs>0 OR any_closure==1) pulls in 106,601 net>0 facility-years (65% of
  events) = pure installs/expansions, all coded "consolidation"; 81,724/130,495
  consolidation rows lack lag_capacity (entry years). Hence dcap_pct~consolidation
  coef = +191.6 is GROWTH, not replace evidence. The CLEAN M3 evidence is the
  distribution table conditional on dcount<=0: consolidation medians +6.9/0/-9.7%
  vs pure-shrink -14/-39/-58% — capacity conserved under swap-type events.
  Possible follow-up: re-run Eq.4-5 on any_closure==1 only (swap vs shrink).
- C2 M3 Eq.6 is mechanically degenerate: share_SWtoDW = 0 in ALL pure-shrink rows
  (SW->DW requires an install by construction), so the logit separates
  (coef 16.5, p=0 is a tautology, not a finding). Do not slide this coefficient.
- C3 M2 Replace-upgrade quasi-separation: RB main OR 0.00017 and RB:SW OR 13,609
  are offsetting artifacts of a near-empty RB x DW x Replace-upgrade cell
  (upgrade requires owning SW tanks). The interpretable number is e+f = +0.81
  (OR ~2.25): RB raises SW-facility upgrade odds. The literal opposite-sign test
  on MAIN effects reads NO for this artifactual reason; the economic contrast
  (RB up on SW upgrades, down on downsize: e_Downsize = -0.486, p=0.009) holds.
- C4 M1 per-margin n differs (Replace-upgrade 1,922,503; Downsize 2,255,796):
  fixest drops s_idx FE cells with zero events for that margin. Standard FE-logit
  behavior; cite per-margin n, not the full panel N.
