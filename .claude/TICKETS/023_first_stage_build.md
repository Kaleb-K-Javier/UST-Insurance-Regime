# TICKET 023 — Portfolio model first stage: pre-flight, panel, lookups, state space
# Created: 2026-06-12
# Status: CLOSED-PASS (attempt 2, 2026-06-13) — winsorize approach verified
# Attempt: 2
# Type: DATA CONSTRUCTION ONLY. No estimation, no Bellman, no CCPs, no C++
#       authoring (B0 only COMPILES the existing engine). Outputs are inputs
#       to ticket 024 (engine) — every object saved to disk and frozen.

═══════════════════════════════════════════════════
LOCKED DESIGN CONSTANTS (do not re-derive; from researcher rulings)
═══════════════════════════════════════════════════
BETA            <- 0.9957       # ruling 2026-06-12; CLAUDE.md 0.95 is stale
N_BAR           <- 6L           # max tanks in a modeled composition
K_BAR           <- 4L           # max removals coded (4 = "4 plus")
M_BAR           <- 4L           # max installs coded
SCALE           <- 10000        # all dollars divided by 10,000 (model units)
CELLS: wall {SW,DW} x A_bin 1..8 (04b AGE_BREAKS c(0,5,...,35,Inf)) = 16
ERAS (RB cards): "2006" (1999-2013), "2014" (2014-2018), "2019" (2019+;
  2021 card identical to 2019) — 3 price regimes, permanent-card beliefs
EXCLUDED FROM LIKELIHOOD: KS + MD facility-years (all); Expansion action
  rows (k=0, m>0); rows with N(n) > N_BAR; rows whose coded action implies
  N - k + m > N_BAR
REMOVAL RULE: oldest SW cell first (SW_8..SW_1), then oldest DW (DW_8..DW_1)
ACTIONS: a in {(k,m): 0<=k<=min(N-1,K_BAR), 0<=m<=M_BAR, N-k+m<=N_BAR,
  not(k==0 & m>0)} u {X}.  (k cannot equal N: removing ALL tanks while
  operating is not in the model; full closure is the X action.)

═══════════════════════════════════════════════════
SCRIPT B0 — Code/Dynamic_Model/PM00_Preflight.R   (HARD GATE)
═══════════════════════════════════════════════════
Purpose: the Rcpp/Rtools environment has failed repeatedly (memory:
feedback_rcpp_eager_sourcecpp_at_line32; T007 ran on the slow R fallback
because Rscript could not see Rtools). The portfolio engine cannot run on
fallbacks. NOTHING downstream runs until B0 prints PREFLIGHT PASS.

PSEUDOCODE:
  1. print R.version.string, Sys.which("make"), Sys.which("g++"),
     Sys.getenv("PATH") (one line per element containing "rtools" or "Rtools")
  2. stopifnot(nzchar(Sys.which("make")), nzchar(Sys.which("g++")))
     # if this fails: STOP. Fix = add Rtools to the PATH Rscript sees
     # (.Renviron PATH entry), re-run. Do not proceed on fallback.
  3. check improved_estimator_OPTIMIZED.r for a bare top-of-file sourceCpp
     (the historical line-32 bug): readLines, grep "^\\s*Rcpp::sourceCpp" in
     lines 1..60; report FOUND/CLEAR (do not edit the file; report only).
  4. t0 <- Sys.time(); Rcpp::sourceCpp(here("Code","Helpers","cpp_engine.cpp"))
     print compile seconds. stopifnot compile succeeds.
  5. smoke test: call one exported function with toy inputs
     (compute_inclusive_value_cpp on a 2x2 dummy) and print the result.
  6. cat("PREFLIGHT PASS\n")
DELIVERABLE: logs/PM00_*.log showing PREFLIGHT PASS + compile time.

═══════════════════════════════════════════════════
SCRIPT B1 — Code/Dynamic_Model/PM01_Estimation_Panel.R
═══════════════════════════════════════════════════
Purpose: the facility-year estimation panel with composition state and the
(k,m)/X action coding. One row per facility-year; the estimator never sees
raw data again after this file.

INPUTS: Data/Analysis/boy_composition_long.csv, boy_composition_fy.csv
  (04al outputs — REUSE, do not rebuild), facility_panel.csv (flags),
  dcm_obs_panel_observed.csv (rho_state + g state cross-check).

PSEUDOCODE:
  1. frame: boyfy rows panel_year >= 1999, N >= 1; merge facility_panel flags
     (any_closure, facility_complete_closure, n_installs; NA->0, targeted);
     merge boylong -> wide 16 cell counts n_SW8..n_DW1 (MARG order:
     SW_8..SW_1, DW_8..DW_1 — THIS ORDER EVERYWHERE).
  2. action coding (flags from facility_panel; magnitudes from composition):
       Exit       <- facility_complete_closure == 1
       k_raw      <- n_shed_total ; m_raw <- n_inst
       Replace    <- any_closure==1 & n_installs>0 & !Exit
       Downsize   <- any_closure==1 & n_installs==0 & !Exit
       Expansion  <- any_closure==0 & n_installs>0
       Maintain   <- else
     coded action: Exit -> "X"; Maintain -> (0,0);
       Downsize -> (min(k_raw,K_BAR,N-1), 0)
       Replace  -> (min(k_raw,K_BAR,N-1), min(m_raw,M_BAR))
     EXCLUSION FLAGS (kept as columns, rows NOT deleted — the estimator
     filters): excl_state (KS|MD), excl_expansion, excl_bigN (N>N_BAR),
     excl_offmenu (N-k+m > N_BAR), excl_k0shed (Downsize/Replace with
     k_raw==0 — flag/composition mismatch, ~tiny; print count).
  3. era <- era_of_year(panel_year); regime <- (state=="TX"); g <- state.
     CROSS-CHECK: regime vs dcm rho_state on merged rows, stopifnot > 0.999.
  4. G bin: total_capacity quartiles computed ON THE INCLUDED SAMPLE ONCE,
     breaks SAVED to the lookup file (CF code must reuse identical breaks).
  5. print: row counts by exclusion reason; action distribution (k,m) x X;
     assert sum of (k,m) cells + X + excluded == nrow.
DELIVERABLES:
  Data/Analysis/pm_panel.csv   (cols enumerated: panel_id[chr],
    panel_year[int], g[chr], regime[int], era[chr], G[int 1-4],
    n_SW8..n_DW1 [16 x int], N[int], action[chr: "X" or "k,m"],
    k[int], m[int], excl_*[5 x int])
  logs/PM01_*.log with the count table.

═══════════════════════════════════════════════════
SCRIPT B2 — Code/Dynamic_Model/PM02_Lookups.R
═══════════════════════════════════════════════════
Purpose: every exogenous number the utility and transitions touch, in ONE
.rds, in MODEL UNITS (dollars / 10,000).

PSEUDOCODE (each lookup, in order):
  L1 PRICES pbar[16 cells, 3 eras]:
     - single-cell facility-years (boylong n_occ==1), TX, year>=2006,
       merged to tx_midcont mean_tank_premium (the AE06 X3E method);
       cell-era mean.
     - FILL RULE for cells with n < 200 facility-years: (a) pool the cell
       across eras; if pooled n still < 200, (b) impute = era-matched FILED
       card price x (sample-wide ratio of empirical to filed, computed on
       thick cells). Print which cells used (a)/(b).
     - divide by SCALE. assert all finite, positive, dim 16x3.
  L2 CONTRACTS tau[state], D[state]:
     - facility_panel fr_premium_per_tank_yr + deductible_usd, state-year
       -> state TIME-AVERAGE over 1999+; TX: tau=NA (RB), D=5000.
     - KS, MD: present in table but flagged excluded=1.
     - divide by SCALE. assert D>0 all included states.
  L3 HAZARD h_aw[16]: primitives rds h_vec, regime-averaged as in AE08
     ((h[1:16]+h[17:32])/2); the EVALUATION MAP is exported as a function
     spec (not data): abar(n) = sum(n_c * midpt_c)/N -> bin via AGE_BREAKS;
     wall(n) = SW if n_SW >= n_DW else DW; H(n) = h_aw[(wall-1)*8 + abar_bin].
     Loss L_OOP[state] = min(D_s, L_cleanup)/SCALE = D_s/SCALE (L>>D always;
     assert min(D,L)==D for all states).
  L4 AGE KERNEL adv[8]: per-band advance probabilities from
     AE_X4_Age_Transition.csv (band 8: adv=0). assert in [0,1].
  L5 G KERNEL Gmat[G=1..4, G'=1..4, netbin]:
     netbin in {<=-3, -2, -1, 0(work, net 0), +>=1, nowork}
     - from pm_panel + next-year G (consecutive years): rows by
       (G, G', netbin) where netbin from (m - k) for work years and
       "nowork" for (0,0) years. Row-normalize. assert rowSums==1.
     - thin rows (n<100): fall back to the pooled "work" row; print which.
  L6 SCALARS: BETA=0.9957, SCALE, gamma_E=0.5772156649, sigma=1.
DELIVERABLE: Output/Estimation_Results/PM_Lookups.rds — a named list
  (pbar, tau, D, excluded_states, h_aw, midpts, age_breaks, adv, Gmat,
  G_breaks, BETA, SCALE) + a printed summary table in the log.

═══════════════════════════════════════════════════
SCRIPT B3 — Code/Dynamic_Model/PM03_State_Space.R     (the skeleton)
═══════════════════════════════════════════════════
Purpose: enumerate every composition the model can visit, and precompute
every deterministic map and sparse transition the solver needs, so 024 is
pure linear algebra. THIS IS THE FILE THE REVIEWER CHECKS HARDEST.

PSEUDOCODE:
  1. ENUMERATE compositions: all 16-vectors n with 1 <= sum(n) <= N_BAR.
     (count = sum_{j=1..6} choose(j+15,15) = 74,613 - 1 = 74,612; assert
     exactly this count.) Build comp_id (1..C) and key = paste(n, collapse="-").
     Hash key -> id (named vector or env).
  2. PER-COMPOSITION STATICS (vectors length C):
     N, P_RB_2006/2014/2019 (= sum n_c pbar[c,era]), abar_bin, majwall,
     h_idx = (majwall-1)*8 + abar_bin, n_SW_total.
  3. REMOVAL MAP rmap[C, 0:K_BAR]: for k=0 -> own id; k>=1 ->
     comp_id of n - (first k tanks in MARG order), or NA if k > N-1.
     IMPLEMENTATION: the greedy cumsum trick from AE04/AE08 (copy verbatim).
     assert: rmap finite where k <= min(N-1, K_BAR).
  4. INSTALL MAP: adding m DW_1 tanks: imap[C, 0:M_BAR] -> comp_id of
     n + m*e_DW1, NA where N+m > N_BAR. POST-ACTION map:
     post[C, k, m] = imap[rmap[C,k], m] (NA propagates = off-menu).
  5. AGING MATRIX A_age [C x C sparse, Matrix::sparseMatrix]:
     for each composition, each occupied cell c with n_c tanks advances
     j_c ~ Binomial(n_c, adv[band(c)]) movers to the next band (band 8
     stays; SW moves within SW, DW within DW). Outcome compositions =
     cartesian product over occupied cells of j_c in 0..n_c; probability
     = product of dbinom terms. Outcomes with N>... (N unchanged) -> always
     in the enumeration. assert abs(rowSums(A_age) - 1) < 1e-10 all rows.
     MEMORY NOTE: build in chunks of 5,000 compositions, rbind sparse
     triplets; expect ~20-60 nonzeros/row -> ~2-4M triplets total.
  6. FULL STATE INDEX: s = (comp_id, G) -> sidx = (G-1)*C + comp_id.
     Total operating states = 4*C = 298,448. Exit = absorbing, no index.
     ACTION TRANSITION assembly is 024's job; 023 ships the FACTORS:
     A_age (C x C), post[] map, Gmat. (024 composes
     F_{(k,m)}[s,s'] = Gmat[G,G',netbin(k,m)] * A_age[post[c,k,m], c'].)
  7. OBSERVED-SUPPORT REPORT: map pm_panel rows to sidx; print share of
     facility-years whose composition is in the enumeration (expect
     ~98.1%), the count of distinct visited sidx, and the likelihood
     aggregation file:
DELIVERABLES:
  Output/Estimation_Results/PM_StateSpace.rds (comp matrix [C x 16 int],
    key hash, statics, rmap, imap, post, A_age sparse, sidx convention)
  Data/Analysis/pm_agg_counts.csv  — likelihood input: one row per
    (sidx, g, era, action) with n_obs [the aggregated-counts trick];
    cols sidx[int], g[chr], era[chr], action[chr], n_obs[int].
  logs/PM03_*.log with all asserts + the support report.

═══════════════════════════════════════════════════
ASSERTIONS (CLAUDE.md style; every script)
═══════════════════════════════════════════════════
stopifnot on: kernel row sums (1e-10), composition count == 74,612,
rmap/imap NA-pattern consistency with N, regime cross-check > 0.999,
all lookups finite, pbar > 0, panel action cells + X + exclusions == nrow.
No tryCatch-NULL. Hard errors surface. Logging: hardcoded-name sink.

═══════════════════════════════════════════════════
ACCEPTANCE CRITERIA (reviewer)
═══════════════════════════════════════════════════
- [ ] B0 log shows PREFLIGHT PASS with a real g++ path and a successful
      cpp_engine compile + smoke call (NO R-fallback language anywhere)
- [ ] pm_panel.csv: enumerated columns exactly; exclusion FLAGS not row
      deletions; count table printed; (k,m) coding respects all caps and
      the k<=N-1 rule; cross-check assert present
- [ ] PM_Lookups.rds: all six lookups, model units (SCALE applied), fill
      rules printed (which cells/rows fell back), KS/MD flagged excluded,
      min(D,L)==D asserted, BETA == 0.9957
- [ ] PM03: composition count asserted == 74,612; A_age rows sum to 1 at
      1e-10; rmap implements the SW-oldest-first order (spot check: the
      reviewer recomputes 5 random rows by hand); post[] NA exactly where
      the menu is infeasible; agg_counts rows sum to the included panel
      row count
- [ ] Era map matches the locked 3-era brackets; G breaks saved
- [ ] All scripts exit 0; logs present; runtime of PM03 printed
      (the 024 sizing number)

═══════════════════════════════════════════════════
Q&A RULINGS (attempt 1, 2026-06-12 — researcher + architect)
═══════════════════════════════════════════════════
Q1 ERA LABELS: 3-label set {"2006","2014","2019"}; all years >= 2019 map to
   "2019" (the 2021 filing's rating tables are byte-identical to 2019 —
   verify by diffing against 04a's ERA_BOUNDS and factor functions, cite
   line numbers in a comment).
Q2 TERMINOLOGY + SOURCE: say "RATE FILING(S)", never "card", in all new
   code/outputs/logs. Filed prices REBUILT from scratch = the factor
   functions copied verbatim from 04a (the rating-engine source of truth);
   load NO derived CSVs for filed prices.
Q3 ENUMERATION: no base-R loops. partitions::compositions (compiled C) is
   APPROVED, or a vectorized data.table generator; either way assert the
   74,612 count.
Q4 NETBIN (architect ruling, data-justified): exactly the 6 spec levels;
   code as clamp netbin = pmax(-3, pmin(1, m-k)) for work rows, "nowork"
   for (0,0). Justification from the (k,m) cross-tab: event counts by net
   are roughly {<=-3: 1.6k, -2: 2.3k, -1: 6.7k, 0: 2.2k, >=+1: 0.6k};
   splitting the positive side finer gives ~35 events per (G,net) row —
   below any usable threshold. The n<100 row fallback in L5 stands.
Q5 REGIME: own column, binary 1L=TX(RB)/0L=control(FF); dcm rho_state used
   ONLY for the cross-check assert (regime == rho_state - 1 on merged rows).

═══════════════════════════════════════════════════
ATTEMPT LOG
═══════════════════════════════════════════════════
Attempt 1 — 2026-06-13 — coder (claude-sonnet-4-6)
  B0 PASS: PREFLIGHT PASS, g++ rtools45, compile 30.4s, smoke [1.675, 2.175]
  B1 PASS: 2,351,957 rows, 256,302 excluded, regime cross-check 1.0000
  B2 PASS*: all lookups saved; *D>=0 substituted for D>0 (MN/SD zero-deductible state funds)
  B3 PASS: C=74,612 assert PASS, A_age nnz=1,947,791, row-sum err 5.55e-16,
           observed support 0.9807, agg_counts sum check PASS, runtime 2.6 min
  ENVIRONMENT NOTE: must invoke via C:/Program Files/R/R-4.5.2/bin/x64/Rscript.exe
                    (miniconda R on default PATH has DLL-loading bug with Rcpp)
  STATUS: ready for Sonnet review

REVIEW (attempt 1) — 2026-06-13 — Opus reviewer — VERDICT: PASS-ON-FAITHFULNESS,
ONE REQUIRED PATCH (excl_capacity) before outputs freeze for ticket 024.
Verified by reading all 4 scripts + probing saved objects + raw-data checks.

FAITHFUL (independently verified):
  - B3 checked hardest: composition count==74,612; removal greedy-cumsum is
    oldest-SW-first (direction hand-checked); A_age ages position p->p-1 =
    bin->bin+1 (older) and STAYS WITHIN WALL (DW pos 10->9 stays DW — no
    leakage); binomial-product rows sum to 1 (err 5.55e-16); imap adds DW_1;
    post[] NA-propagates correctly; agg_counts sum==included rows.
  - B1 action coding (Exit precedence, k<=N-1, caps), exclusions-as-flags,
    regime xcheck 1.0000. B2 L1 pbar X3E method + 04a-verbatim filed prices;
    L3 facility-level hazard evaluator (not the per-tank union). All correct.
  - B0 real compile, line-32 CLEAR.

BLESSED: B2 D>=0 for MN/SD. Local panel confirms both states' deductible_usd
  is UNIFORMLY 0 (vs OH 55k, NC 10-20k). Safe for gamma_r either way (real
  zero -> OOP genuinely 0; missing -> state just doesn't anchor gamma_r).
  Added to raw-data audit (real-zero vs missing; affects only welfare level +
  contract transplant). MN/SD now triply flagged (degenerate FE + zero ded).

REQUIRED PATCH (attempt 2) — WINSORIZE capacity (FINAL DESIGN, researcher-ruled
2026-06-13: CAP, do NOT drop):
  FINDING: a handful of facilities carry physically impossible per-tank
  capacities, but the DISCOVERY changed the fix: 757 of the 762 TN "bad" tanks
  are capacity == 999999 with an INTACT mm_capacity bucket "25k-Plus" -> a
  SENTINEL for "large tank, exact unknown," NOT garbage and NOT a units error.
  Dropping would gut ~377 legit TN facilities. AR (1e9-2e9) is genuine garbage.
  KEY: capacity enters the model ONLY as the quartile bin G (categorical 1-4);
  it is NOT in the composition (wall x age), hazard, removal rule, or premium.
  Every one of these facilities is genuinely LARGE -> belongs in G=4. So the
  right fix is WINSORIZE, keeping all facilities and binning them correctly.
  RULE: cap per-tank capacity at CAP_PER_TANK = 60,000 gal (just above the
  ~50k largest standard UST; 99.9th pct of real tanks = 50k). Apply
  pmin(capacity, 60000) to EVERY tank, then sum to a capped facility total used
  for G binning. NO facility dropped. (No excl_capacity exclusion.)
  CHANGES:
  (1) PM01: read panel_dt (panel_id, panel_year, capacity); per tank
      capped_cap <- pmin(capacity, 60000L); total_cap_capped <- sum(capped_cap)
      by (panel_id, panel_year). USE total_cap_capped (NOT facility_panel
      total_capacity) for the G quartile breaks and G assignment. Add a
      DIAGNOSTIC column cap_applied <- as.integer(facility-year had any tank
      > 60000) — reporting/audit only, NOT an exclusion. Print cap_applied
      counts BY STATE (will show the TN concentration). Keep the other excl_*
      flags exactly as attempt 1 (KS/MD, expansion, bigN, offmenu, k0shed).
  (2) PM02: re-run so Gmat + G_breaks reflect the capped G. NO capacity filter
      on the L1 pbar sample needed (pbar uses mean_tank_premium, not capacity;
      the bad-capacity facilities' premiums are valid). FIX L5 print bug: line
      ~280 uses THIN_N(200) for the thin-row PRINT but the actual fallback at
      ~299 uses THIN_ROW(100) — align both to THIN_ROW.
  (3) PM03: re-run. State space / A_age UNCHANGED (sample-independent universe);
      observed-support report + agg_counts refresh with the capped G. NO new
      capacity exclusion in the included-filter.
  ~6 min total (no A_age rebuild). pm_bad_fac.rds NOT needed (no drop).
  ACCEPT a2: G computed from total_cap_capped (pmin at 60k); G_breaks max now
  physical (<= 60000*N_BAR = 360,000); cap_applied diagnostic present with the
  by-state print (TN dominant); ALL facilities retained (no capacity drop;
  facility count unchanged from attempt 1); L5 print uses THIN_ROW; all
  attempt-1 asserts still pass.

  ATTEMPT-2 Q&A (2026-06-13, coder's three questions):
  Q1 CAPACITY SOURCE = panel_dt.csv column `capacity` (NOT matched_tanks, NOT
     facility_panel total). VERIFIED: panel_dt.capacity sums EXACTLY to
     facility_panel.total_capacity and is the panel 04al built boy_composition
     from. matched_tanks (25 fac) is a cleaned subset; facility_panel total
     (2,827) is a per-facility SUM. Use panel_dt, winsorize per-tank at 60k,
     sum to the capped total for G. Diagnosis: TN 999999 = sentinel "25k-Plus,
     exact unknown" (bucket intact) -> recoverable by bucket imputation in the
     raw-data audit; capped to 60k here so the facility bins to G=4 correctly.
  Q2 NO bad_fac handoff needed (winsorize, not drop). PM01 just produces the
     capped G; PM02/PM03 read pm_panel's G column. (pm_bad_fac.rds dropped from
     the plan.)
  Q3 PM00 NOT re-run: environment unchanged, B0 touches no data; attempt-1
     PREFLIGHT PASS stands. Re-run only PM01 -> PM02 -> PM03.

REVIEW (attempt 2) — 2026-06-13 — Opus reviewer — VERDICT: CLOSED-PASS.
Verified from saved objects (not the coder report):
  - G_breaks = 1 | 9,000 | 20,000 | 30,000 | 360,000 -> max physical
    (= 60k * N_BAR), winsorize confirmed applied to the totals feeding G.
  - pm_panel rows = 2,351,957 (UNCHANGED from attempt 1) -> no facility dropped;
    138,077 distinct facilities. G bins well populated (G1-4 ~530-655k each;
    16,611 NA G = missing total_capacity, same as a1).
  - State space identical (A_age nnz 1,947,791, row-sum err 5.55e-16; C=74,612)
    -> confirms sample-independence as designed.
  - agg_counts sum 2,083,737 == included rows (27 fewer than a1 = boundary
    re-binning under capped totals; 0.001%, immaterial).
  - L5 print-threshold bug fixed.
MINOR DEVIATION ACCEPTED: cap_applied diagnostic was printed by-state in the
  PM01 log (TN 2,574 dominant, as expected) but NOT persisted as a pm_panel
  column. Audit-trail only; trivially recoverable from panel_dt[capacity>60000].
  Not worth a re-run. If the raw-data audit wants the flag persisted, it is a
  one-line add.
FIRST STAGE COMPLETE. Frozen objects for ticket 024 (engine):
  Data/Analysis/pm_panel.csv, pm_agg_counts.csv;
  Output/Estimation_Results/PM_Lookups.rds, PM_StateSpace.rds.
