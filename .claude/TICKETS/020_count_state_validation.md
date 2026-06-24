# TICKET 020 — Count-state (composition) model: identification budget + validation
# Created: 2026-06-09
# Status: REVIEWED-PASS (attempt 1, 2026-06-09) — ADDENDUM (E4x/E2x/Q) drafted below,
#         awaiting researcher approval before launch
# Attempt: 1
# Type: READ-ONLY DESCRIPTIVE (no structural estimation; no Output/Estimation_Results
#       writes; ONE intermediate data file under Data/Analysis/)

═══════════════════════════════════════════════════
ECONOMIC MOTIVATION
═══════════════════════════════════════════════════
Proposed model state = COUNT VECTOR n over composition cells (wall x age bin),
with flow utility depending on the state ONLY through low-dimensional indices:
  P_total(n) = sum_cells n_cell * p_cell      (Mid-Continent rate card, RB rows;
                                               flat fee * N for FF rows)
  H(n)       = 1 - prod_cells (1-h_cell)^n_cell   (facility hazard)
  N, Q       = tank count, total capacity
Parameters (~9) never see the cells — identification runs through index variation,
not per-cell saturation. Partial actions (downsize/replace) move the state by a
MARGINAL RULE (shed SW before DW, oldest bin first within wall; replace also adds
one DW age-0 tank), making transitions deterministic given n. Replace wall outcome
(upgrade vs same-wall) is NOT a choice: it is an accounting consequence of
composition (any-SW facility -> upgrade; all-DW facility -> same-wall).

This ticket answers, BEFORE any model code exists, the three researcher worries:
  W1 rare actions -> can the parameters be identified?        (E1)
  W2 big state space -> is index variation/separation there?  (E2, E5)
  W3 is the marginal-rule transition actually true?           (E3, E4)

CELL DEFINITION (canonical for the whole ticket):
  cell = (wall in {SW, DW}) x (model age bin 1..8, the existing 5-yr bins)
       = 16 cells. The rate card's 6 age bands are a coarsening (bands 1-5 = bins
  1-5; band 6 "25+" = bins 6-8), so p_cell is constant across bins 6-8 by
  construction. Hazard h_cell reads natively off the model bins.

DECISION LOGIC (pre-committed interpretation guidance — NOT hard gates; report
the numbers either way, researcher rules on borderline reads):
  E1: multi-replace facility-years (>=2 closures AND >=2 installs) < ~200
      -> two-part replace fixed cost F^R unidentified -> dropped ("lite" costs).
  E2: within-RB residual SD of P_total after (H, N, Q, year FE) < ~5% of P_total SD
      -> gamma_price separation rests ENTIRELY on the FF<->RB contrast; say so.
      Rate-card fit R^2 < ~0.8 -> 2-dim card misses real pricing (leak/piping
      loads matter) -> revisit cell definition before model build.
  E3: cell-exact shed-fidelity >= ~90% -> deterministic F_a;
      ~70-90% -> deterministic + documented noise;
      < ~70% -> use the E3 cross-tab as an ESTIMATED stochastic shedding kernel.
  E4: SW-skip rate (any-SW facility does same-wall replace) < ~10% -> no wall
      sub-choice needed; >= ~10% -> replace margin needs a wall choice (redesign).

═══════════════════════════════════════════════════
DEFINITIONS (the "MATH")
═══════════════════════════════════════════════════
BOY composition (decision-time, per T013 convention):
  tank j is in facility i's BOY(t) set  <=>  install_yr_int < t  AND
                                             (is.na(close_yr_int) OR close_yr_int >= t)
  BOY age_j(t) = t - install_yr_int;  age bin via the existing 5-yr binning
  (bin 1: 0-5, ..., bin 8: 35+ — MATCH 04b's A_bin breaks exactly; read them
  from 04b, do not invent).
  wall_j: SW if grepl("single", mm_wall, ignore.case=TRUE) else DW.
  n_cell(i,t) = #tanks in cell; N(i,t) = sum; Q(i,t) = sum capacity.
  CROSS-CHECK (wiring tripwire): N(i,t) must equal dcm_obs_panel_observed.csv
  boy_stock on merged rows. Report exact-match share; stopifnot(share > 0.95).

Within-year action detail (from panel_dt, year t):
  n_shed_cell(i,t)  = #tanks with closure_event==1 in year t, by their BOY(t) cell
  n_inst(i,t)       = #tanks with install_yr_int == t (enter BOY at t+1, bin 1)
  inst_dw_share     = share of those installs with wall = DW

Facility-year action coding (6 categories; extends 019's five):
  Exit       = facility_complete_closure == 1
  Replace    = any_closure==1 & n_installs>0 & !Exit
  Downsize   = any_closure==1 & n_installs==0 & !Exit
  Expansion  = any_closure==0 & n_installs>0            (NOT a model action;
               counted in E1 so the M3-C1 contamination never recurs)
  Maintain   = any_closure==0 & n_installs==0
  (any_closure/facility_complete_closure/n_installs from facility_panel, NA->0,
   merge keys panel_id+panel_year, exactly as 019.)

MARGINAL ORDER over occupied cells (cell-level, tie-free by construction):
  all SW cells from OLDEST bin to youngest, then all DW cells oldest to youngest.
  Predicted shed multiset for k sheds = first k tanks drawn in that order.

Rate-card simplified cell price (reuse 04a functions VERBATIM — age_factor_*,
construction_factor, ilf_factor_* from 04a_TX_Premium_All_1999_onwards.R).
ERA-TO-YEAR MAP (ALL eras in the panel MUST be used; modal era per calendar
year, from 04a's ERA_BOUNDS at 04a:83-86 — brackets switch mid-year):
  panel_year 1999-2013 -> era_2006   (filing window 1999-01-01..2014-04-30)
  panel_year 2014-2018 -> era_2014   (2014-05-01..2019-01-31)
  panel_year 2019-2020 -> era_2019   (2019-02-01..2021-04-30)
  panel_year 2021+     -> era_2021   (2021-05-01.. ; rating tables byte-identical
                                      to era_2019 per 04a:266 — 3 distinct cards)
  era_2019/2021 also carry deductible + coverage-form multipliers; at the
  spec's defaults (form A, ded $5k) both factors = 1.00 exactly — apply them
  explicitly at defaults so the formula matches 04a's code path.
  NOTE the age gradient CHANGES across eras (2014 extends loads to age 50+;
  2019 adds finer young-tank discounts) — this time-variation in the
  composition-price gradient is identifying variation; do not collapse eras.
  p_cell(era) = 300 * ILF_era(default NA->1m/1m) *
                (1 + age_load_era(bin midpoint age) + cons_load(wall))
  cons_load: DW = -0.20, SW = 0.00 (04a:186-192). Status/leak/piping neutral (0),
  deductible/coverage defaults — exactly 04a's defaults.
  P_total_RB(i,t) = sum n_cell * p_cell(era(t))
  P_total_FF(i,t) = fr_premium_per_tank_yr(state,t) * N(i,t)
    DENOMINATION (verified 2026-06-09 vs raw notes): the FF fee is PER TANK per
    year, state x year level (no within-facility or composition variation), so
    FF total scales with N. SIX states are automatic_zero (AL,IL,MN,OK,SD,VA;
    fee=0) and KS/MD are private_premium NA->0 per 02b: for ~8/18 states
    P_total_FF == 0 identically. REPORT the zero-fee state list + their share
    of FF facility-years in T020_E2_Premium_Regressions footnote; run R2_FF
    both on all FF rows AND on fee>0 FF rows. Within-FF, gamma_p*fee*N is
    collinear with any kappa1*N / phi*Q size term — E2's point; say it.
  P_index(i,t)    = RB row ? P_total_RB : P_total_FF   (RB = TX & panel_year>=1999;
                    match the dcm panel's rho_state on merged rows — do not re-derive)

Hazard: h_cell from DCM_Primitives_Replacement_observed.rds h_vec (length 32 =
  8 bins x 2 wall x 2 regime). Average over regime per (bin, wall); REPORT
  max |h_FF - h_RB| (expected ~0; hazard is physical). H(i,t) as defined above.

═══════════════════════════════════════════════════
PSEUDOCODE — FOUR SCRIPTS
═══════════════════════════════════════════════════
04al_BOY_Composition_Build.R   (the ONLY heavy panel_dt read; everything else
                                reads its output)
  - fread panel_dt.csv select = c(tank_panel_id, panel_id, panel_year, state,
      mm_wall, capacity, tank_age, closure_event, install_yr_int, close_yr_int)
  - build BOY membership per (tank, panel_year) per the definition above.
    R-NOTE: do NOT cross-join tanks x years. Iterate years (1999..max) or use
    non-equi joins per year; filter-first; this is the 12.7M-row OOM zone (04ai).
  - aggregate LONG: one row per (panel_id, panel_year, wall, age_bin) with
      n_boy[int], n_shed[int]  (+ facility-year rows: N, Q, n_inst, n_inst_dw)
  - save Data/Analysis/boy_composition_long.csv  AND
         Data/Analysis/boy_composition_fy.csv   (facility-year summary:
      panel_id, panel_year, state, N, Q, has_SW, n_shed_total, n_inst, n_inst_dw)
  - merge check vs dcm_obs boy_stock -> print match share; stopifnot > 0.95
  - print: rows, facility-years, years covered

04am_Identification_Budget.R   (E1 + E5; reads boy_composition_* + facility_panel
                                + dcm_obs for size_bin/rho_state)
  E1a action frequencies: 6-category coding; table action x size_bin {1,2,3,4+}
      + "All": n, rate_pct.
  E1b simultaneity grid: operating facility-years; cross-tab
      pmin(n_shed_total,3)x{0,1,2,3+} x pmin(n_inst,3)x{0,1,2,3+}: n, share.
      PRINT the headline: n facility-years with >=2 sheds AND >=2 installs.
  E1c events by identifying contrast: for action in {Exit, Downsize, Replace}:
      n_events and event rate by size_bin x has_SW (24 rows).
  E5  occupancy: composition key = concatenated 16-cell count string.
      n distinct compositions; coverage of top 10/100/500; n needed for 95%/99%
      of facility-years; share single-cell (ties to 04ah); boy_stock p50/p90/p99,
      share N>4, share N>6.
  FIGURES: E1 simultaneity heat-tile; E5 coverage curve (cum share vs rank, log x).

04an_Index_Separation.R        (E2; reads boy_composition_long + primitives rds +
                                state_fr_premium via facility_panel cols +
                                tx_midcont_premium_all_1999_onwards.csv)
  - build p_cell(era) table (16 cells x 4 eras) -> T020_E2_Cell_Prices.csv
  - build P_index, H, N, Q per facility-year.
  - correlations: pearson for all pairs of (P_index, H, N, Q), pooled / FF / RB.
  - regressions (fixest, cluster = ~state):
      R1: feols(P_total_RB ~ H + N + Q | panel_year)  on RB rows
      R2: feols(P_total_FF ~ H + N + Q | panel_year)  on FF rows (expect R^2 ~ 1;
          report honestly — FF premium is mechanically N * fee)
      R3: feols(P_index ~ regime + H + N + Q | panel_year)  pooled (the FF<->RB
          gap at matched composition)
      report per spec table: r2, resid_sd, resid_sd / sd(P), n.
  - rate-card fit check: TX 2006+ rows; actual = tx_mean_tank_premium *
      n_tanks_rated (from the 04a output csv); cor + R^2 of simplified
      P_total_RB vs actual, by era.
  - FIGURE: P_index vs H scatter at the composition level (point = distinct
      composition, size = n facility-years, color = regime FF/RB) — the
      "two pricing worlds" picture on the new state.

04ao_Transition_Fidelity.R     (E3 + E4; reads boy_composition_long + _fy +
                                facility_panel flags + dcm_obs rho_state)
  E3 population: Downsize and Replace facility-years with n_shed_total >= 1.
    For each event: predicted shed multiset (marginal order, k = n_shed_total)
    vs realized (n_shed_cell). Metrics per event:
      exact_match = predicted multiset == realized multiset
      L1_misplaced = sum_cells |pred - real| / 2     (tanks shed from wrong cell)
      wall_violation = any DW tank shed while >=1 SW tank kept
      ageband_violation = within either wall, shed from a younger bin while an
        older same-wall bin still had kept tanks
    Aggregate by action x size_bin (and by k_shed 1 vs 2+): n, match share,
    mean L1, violation shares.
    KERNEL (fallback object): long cross-tab predicted-cell x realized-cell
    shed counts (16x16, only sheds), pooled and by action.
  E4a replace wall 2x2: Replace events; BOY has_SW {0,1} x outcome
      {upgrade = single_to_double_year==1, samewall} : n, row shares.
      SW-skip rate = P(samewall | has_SW). PRINT it.
  E4b installs are DW: dw_share of installs by panel_year x group {TX, Control};
      figure: two lines over years.
  FIGURE E3: fidelity bars (match + violation shares) by size, facet
      Downsize | Replace.

═══════════════════════════════════════════════════
DELIVERABLES (enumerated — files, rows, columns, types)
═══════════════════════════════════════════════════
INTERMEDIATE (Data/Analysis/ — regenerable, do not commit):
  boy_composition_long.csv  rows ~ (facility-year x occupied cell);
    cols: panel_id[char], panel_year[int], state[char], wall[char SW/DW],
          age_bin[int 1-8], n_boy[int], n_shed[int]
  boy_composition_fy.csv    rows = facility-years;
    cols: panel_id[char], panel_year[int], state[char], N[int], Q[num],
          has_SW[int], n_shed_total[int], n_inst[int], n_inst_dw[int]

FIGURES (Output/Figures/, PNG, dpi=150):
  T020_E1_Simultaneity_Grid.png
  T020_E2_Premium_vs_Hazard.png
  T020_E3_Fidelity_bySize.png
  T020_E4_Install_DW_Share.png
  T020_E5_Coverage_Curve.png

TABLES (Output/Tables/; ALL as .csv; .tex ALSO for the four starred slide tables,
  019 .tex conventions: centered, \small, arraystretch, escaped %/_, no
  $-before-digit, <= ~8 rows):
  T020_E1_Action_Frequencies.csv *  rows action{Maintain,Exit,Downsize,Replace,
      Expansion} x size{1,2,3,4+,All} (25); cols action[char], size_bin[char],
      n[int], rate_pct[num]   (.tex = the "All" column block, 5 rows)
  T020_E1_Simultaneity.csv          rows sheds{0,1,2,3+} x installs{0,1,2,3+} (16);
      cols sheds_bin[char], installs_bin[char], n[int], share[num]
  T020_E1_Events_by_Index.csv       rows action{Exit,Downsize,Replace} x size{1,2,
      3,4+} x has_SW{0,1} (24); cols action[char], size_bin[char], has_SW[int],
      n_events[int], event_rate[num]
  T020_E2_Cell_Prices.csv           rows 16 cells x era{2006,2014,2019,2021} (64);
      cols wall[char], age_bin[int], era[char], p_cell[num]
  T020_E2_Index_Correlations.csv    rows sample{pooled,FF,RB} x pair{P-H,P-N,P-Q,
      H-N,H-Q,N-Q} (18); cols sample[char], pair[char], pearson[num], n[int]
  T020_E2_Premium_Regressions.csv * rows {R1_RB, R2_FF, R3_pooled} (3);
      cols spec[char], sample[char], r2[num], resid_sd[num],
      resid_sd_over_sdP[num], coef_regime[num, NA for R1/R2], se_regime[num, NA
      for R1/R2], n[int]   (SE state-clustered)
  T020_E2_RateCard_Fit.csv          rows era (<=4); cols era[char], n_fy[int],
      pearson[num], r2[num], mean_actual[num], mean_simplified[num]
  T020_E3_Shed_Fidelity.csv *       rows action{Downsize,Replace} x size{1,2,3,4+}
      x k_shed{1,2+} (<=16, drop empty); cols action[char], size_bin[char],
      k_shed[char], n_events[int], exact_match_share[num], mean_L1[num],
      wall_violation_share[num], ageband_violation_share[num]
      (.tex = collapsed action x size, <= 8 rows)
  T020_E3_Shedding_Kernel.csv       rows pred_cell x real_cell with n>0 (<=256);
      cols pred_wall[char], pred_bin[int], real_wall[char], real_bin[int],
      n[int], share_within_pred[num]
  T020_E4_Replace_Wall.csv *        rows has_SW{0,1} x outcome{upgrade,samewall}
      (4); cols has_SW[int], outcome[char], n[int], row_share[num]
  T020_E4_Install_Wall.csv          rows panel_year x group{TX,Control};
      cols panel_year[int], group[char], n_installs[int], dw_share[num]
  T020_E5_Occupancy.csv             rows ~10 metrics; cols metric[char], value[num]
      metrics: n_compositions, top10_cov, top100_cov, top500_cov, n_for_95pct,
      n_for_99pct, share_single_cell, share_N_gt4, share_N_gt6, p50_N, p90_N, p99_N

═══════════════════════════════════════════════════
R-IMPLEMENTATION NOTES
═══════════════════════════════════════════════════
- Packages: data.table, ggplot2, here, scales, fixest. install.packages if
  missing; no silent skips.
- 04al is the ONLY script allowed to read panel_dt.csv. fread(select=) the 10
  cols. Build BOY membership year-by-year (loop or non-equi join); NEVER
  materialize tank x year cross joins. Peak memory target < 8GB.
- Rate-card functions: COPY VERBATIM from 04a (age_factor_2006/2014/2019/2021,
  construction_factor, ilf_factor_*) and the era-year assignment. Re-deriving
  different factors = PSEUDOCODE_FAIL.
- A_bin breaks: read the exact binning from 04b_Replacement_Panel_Prep.R and
  reuse; do not invent breaks. (Reviewer will diff.)
- h_vec: readRDS DCM_Primitives_Replacement_observed.rds; stopifnot length 32,
  finite, in (0,1); print max regime discrepancy per (bin,wall).
- All regression SEs state-clustered (fixest cluster=~state). Descriptive shares
  reported with n; no inferential dressing on counts.
- No tryCatch returning NULL. Hard errors surface. cat("=== SECTION ===")
  markers. rstudioapi sink idiom breaks under Rscript — log to hardcoded
  logs/04a{l,m,n,o}_*.log or rely on runner stdout capture.
- Outputs land in Output/Figures, Output/Tables, Data/Analysis only. NO
  Output/Estimation_Results writes.
- Figures: theme_minimal(base_size=13); FF=#E76F51, RB=#2A9D8F; free-y where
  shares are small; match 04ag/04ah/M-series styling.

═══════════════════════════════════════════════════
ACCEPTANCE CRITERIA (binary — reviewer checks code + output)
═══════════════════════════════════════════════════
- [ ] Four scripts as named; only 04al touches panel_dt.csv
- [ ] BOY composition matches dcm_obs boy_stock on > 95% of merged rows
      (share printed; stopifnot present)
- [ ] Cells = existing 04b A_bin breaks x wall (16); breaks read from/equal to
      04b's, not re-invented
- [ ] Action coding = the 6-category scheme above, built from the SAME
      facility_panel flags as 019 (merge keys panel_id+panel_year, NA->0);
      Expansion counted separately and EXCLUDED from E3
- [ ] E1: frequencies (incl. Expansion), simultaneity grid with the >=2 & >=2
      headline printed, events by size x has_SW
- [ ] E2: p_cell built from VERBATIM 04a factors; P_total_FF = fee x N;
      correlations pooled/FF/RB; 3 regressions with state-clustered SE and
      honest R2_FF reporting; rate-card fit vs (mean_tank_premium x
      n_tanks_rated) for TX 2006+
- [ ] E3: predicted-vs-realized shed multisets at CELL level (no tank-level
      match test); exact-match, L1, wall/ageband violation decomposition, by
      action x size x k_shed; 16x16 kernel cross-tab saved
- [ ] E4: replace 2x2 with SW-skip rate printed; install DW share by year x group
- [ ] E5: occupancy metrics incl. n_for_95pct/99pct and boy_stock quantiles
- [ ] All 5 figures in Output/Figures; all 12 tables in Output/Tables; the 4
      starred tables also as slide-ready .tex (019 conventions)
- [ ] No tryCatch-NULL; scripts run end-to-end exit 0; logs/stdout captured

═══════════════════════════════════════════════════
ATTEMPT LOG
═══════════════════════════════════════════════════
ATTEMPT 1 — 2026-06-09 — VERDICT: PASS (Opus acting reviewer; verified from all
four scripts + output CSVs + run logs, independently of the R1 report).

FAITHFULNESS VERIFIED:
- Only 04al reads panel_dt (12.69M rows); year-loop membership, no cross join.
- AGE_BREAKS byte-identical to 04b:64-69 incl. the cut() idiom + NA->oldest.
- BOY vs boy_stock exact-match 0.9835 (>0.95, stopifnot present). Regime
  agreement vs rho_state = 1.00000. max|h_FF-h_RB| = 0.
- 04an rate-card factor bodies byte-identical to 04a (diffed); era map per the
  architect ruling; cf/df applied explicitly at defaults for 2019/2021.
- E3 greedy predicted-shed (cumsum trick) is a correct vectorization of the
  marginal order; violation definitions match spec; Expansion excluded.
- All 5 figures, 12 CSVs, 4 .tex present; 6-category action coding from the
  same facility_panel flags as 019; no tryCatch-NULL; exit 0 all scripts.

BLESSED DEVIATIONS:
- D1 R1_RB fit unclustered (RB = TX = single cluster; SE not a deliverable;
  architect-approved in-session). R3's se_regime clustered as specified.
- D2 kernel pairing = m-th predicted to m-th realized in marginal order
  (reasonable resolution of an under-specified cross-tab).
- D3 frame inconsistency (spec ambiguity, mine): 04am E1 uses the dcm_obs spine
  (2,282,735 fy; TX 2006+), 04an/04ao use the boyfy >=1999 spine (2,351,957 fy
  incl. TX 1999-2005). Benign for E2-E4 (physical/transition questions); E1 is
  on the estimation population as intended. Document, don't refit.

SPEC-SIDE GAP (NOT a translation fail — deliverable enumeration was stale):
- The zero-fee amendment (R2_FF re-run on fee>0 FF rows + zero-fee state list
  in the footnote) was not implemented; the spec's 3-row enumeration for
  T020_E2_Premium_Regressions contradicted the amendment text. ALSO: the .tex
  footnote says R2_FF "mechanically near 1" but actual R2_FF = 0.126 — the
  mechanical-~1 logic only holds WITHIN state; across states fee varies (incl.
  8 zero-fee states) and the spec has no state FE. Fix both in the addendum.

RESULTS vs DECISION RULES (architect read):
- E1: budget healthy. >=2 sheds & >=2 installs = 3,890 (>>200) -> F^R stays a
  candidate. Downsize has NO size-1 row (1-tank shed = Exit, definitional —
  good internal consistency). Expansion 0.93% ~ 3x downsize.
- E2: within-RB resid/sd(P) = 0.236 (>>0.05) — gamma_p has real within-RB
  variation. R3 regime gap $709.5 (SE 126.2). Rate-card fit r2 .43/.55/.64
  (<0.8 rule) BUT level offset (simplified ~$1,042 vs actual ~$781 in era_2006)
  points at denominator (n_tanks_rated != BOY N) and neutral-status pricing
  (TOU tanks carry -0.50/-0.75 loads we zero) — decompose via E2x before any
  cell redesign.
- E3: pooled exact-match 0.840, kernel diagonal 0.905, wall-viol 2.8%,
  ageband-viol 13.6% -> "deterministic + documented noise" band. CAVEAT: high
  Replace fidelity at k=2+ is partly trivial (size-2 facilities shedding 2 =
  shed-everything, match=1 by construction); the BINDING cells are k=1
  multi-tank (match 0.72-0.88) — still in band.
- E4: SW-skip 0.497 fired the rule ON ITS FACE but conflates (a) true SW-skip
  (bounded ~3-8% by E3 wall-viol), (b) shed-SW+install-SW (early years; DW
  installs reach ~100% only by 2015), (c) missing install wall. has_SW=0 rows
  are 99.5% samewall (mechanical direction confirmed). DECOMPOSE via E4x
  before accepting "wall sub-choice needed". single_to_double_year =
  (n_sw_replacement>0 & n_dw_installs>0) per 02b:1109.
- E5: occupancy strongly concentrated: 368 compositions cover 95% of facility-
  years; 86.6% single-cell; p99 N = 8. Computation is a non-issue.

═══════════════════════════════════════════════════
ADDENDUM — SUPERSEDED 2026-06-10: folded into TICKET 021 (clean assumption-
evidence rebuild). Do not run 04ap. Kept below for the record only.
═══════════════════════════════════════════════════
One script: 04ap_T020_Addendum.R. READ-ONLY; reads ONLY the existing
intermediates (boy_composition_long/fy), facility_panel flags, and the midcont
csv. No panel_dt read.

A1 (E4x) — decompose the 0.497 "SW-skip":
  Population: Replace events with has_SW==1 (n=5,802). Classify each into:
    shed_SW & inst_DW   (rule holds; facility-level flag may still read 0)
    shed_SW & inst_SW   (install-side failure — era story)
    shed_DW_only        (true SW-skip — the only design threat)
    (use boy_composition_long n_shed by wall; n_inst_dw vs n_inst for installs)
  TABLE T020_E4x_Skip_Decomposition.csv: rows class x yearbin{1999-2007,
  2008-2014, 2015+} (9); cols class[char], yearbin[char], n[int], share[num].
  Also print: share of "samewall"-flagged has_SW replaces that are actually
  shed_SW & inst_DW (the flag-vs-act gap).
  READ: true SW-skip < ~10% -> no wall sub-choice; install-side failures
  concentrated pre-2015 -> era-qualified install assumption (DW from year Y*).
  INSTITUTIONAL CONTEXT (verified state SW-install ban dates, researcher's
  email-verified sheet, Box "Double Wall and Enformcnet Email Verification.xlsx"
  read 2026-06-09; HARDCODE this vector in 04ap — server cannot see Box;
  use the email-verified date where it differs):
    MA 1989-01-01 | ID 2007-02-23 | AR 2007-07-01 | TN 2007-07-24 |
    AL 2007-08-06(email) | NC 2007-11-01 | MN 2007-12-22 | IL 2008-02-01 |
    OK 2008-07-01 | CO 2008-08-01 | LA 2008-12-20 | TX 2009-01-01 |
    ND 2009-01-01 | SD 2009-01-01 | MD 2009-01-12 | VA 2010-09-15 |
    OH 2011-05-16 | KY 2012-04-01 | KS 2013-07-01 | MO 2017-07-01 |
    ME 1991-09-16(email)
  NOTE the spread: NOT "all by ~2005" — most 2007-2009, but OH/KY/KS/MO ban
  2011-2017 (MO near panel end) and TX's 2009 ban is INSIDE the observed
  sample (TX 2006+). A1 therefore classifies install-side events PRE/POST the
  facility's STATE ban date (sharper than calendar year bins; keep the yearbin
  column too). Expected: shed_SW & inst_SW collapses post-ban per state;
  post-ban SW installs = wall-coding data-quality flag, not behavior.
  Model stays STATIONARY (install = DW always; no CF changes the bans); the
  assumption rides on the EMPIRICAL DW install share (high before most bans —
  bans largely codified practice), with the institutional dates as a footnote.
  CAVEAT for M2/slides: TX's 2009 ban means post-2009 TX "upgrades" are partly
  compliance; ban timing varies cross-state so year FE don't fully absorb it —
  the causal upgrade claim stays with the DiD.

A2 (E2x) — per-tank rate-card fit + zero-fee deliverables:
  - Per-tank fit: simplified per-tank mean = P_total_RB / N vs mean_tank_premium
    directly; cor + r2 by era -> T020_E2x_RateCard_Fit_PerTank.csv (cols era,
    n_fy, pearson, r2, mean_actual_pertank, mean_simplified_pertank). Kills the
    n_tanks_rated denominator channel; remaining gap = loads/status.
  - R2_FF on fee>0 FF rows (4th row appended to T020_E2_Premium_Regressions.csv,
    spec=R2_FF_feepos) + print the zero-fee state list and its share of FF
    facility-years; FIX the .tex footnote ("mechanically near 1" -> within-state
    statement + zero-fee note).
A3 (Q sensitivity) — recompute T020_E2_Index_Correlations excluding Q > p99.9
  (extra col pearson_qtrim); print the panel_id/year of the Q=2.0e9 row for the
  raw-data audit list.
A5 (researcher additions, 2026-06-10) — two cross-tabs deciding open design cells:
  - A5a G-move at downsize: among Downsize events, share whose capacity QUARTILE
    bin (04ag bins) changes BOY(t)->BOY(t+1) -> decides G frozen vs step-down
    kernel at D. TABLE T020_A5_G_Move_at_Downsize.csv (rows size_bin x moved{0,1};
    cols size_bin, moved, n, share).
  - A5b removed-count vs block size: among Downsize events, compare n_shed_total
    to the size of the top-priority occupied cell (oldest-SW-first rule):
    classify {less than block, exactly block, more than block}. Decides
    one-tank vs full-block vs chosen-count removal. TABLE
    T020_A5_Shed_vs_Block.csv (rows class x size_bin; cols class, size_bin, n, share).
  REMOVED (2026-06-10, researcher direction): the former A4 (re-scoring fidelity
  under the regression-estimated removal order) — the model uses the PLAIN stated
  rule (oldest SW first, then oldest DW), with M4/E3 as motivation; the estimated
  re-ranking is not a model object.
Acceptance: 4 new CSVs + 1 amended CSV + amended .tex; classes in A1 exhaustive
  & mutually exclusive (counts sum to 5,802); A5b classes exhaustive; exit 0.
