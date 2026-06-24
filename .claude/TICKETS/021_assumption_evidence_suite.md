# TICKET 021 — Assumption Evidence Suite (clean rebuild for the model QMD)
# Created: 2026-06-10
# Status: REVIEWED-PASS (attempt 1, 2026-06-11) — see Attempt Log: 3 design
#         decisions landed + 1 new identification finding (X11 per-tank fit)
# Attempt: 1
# Type: READ-ONLY DESCRIPTIVE. Publication-grade exhibits. No estimation,
#       no Output/Estimation_Results writes.
# SUPERSEDES: the 04ap addendum in ticket 020 (items A1/A2/A3/A5 folded in here);
#       the M1-M4 / E1-E5 exhibits remain valid analyses but their FIGURES/TABLES
#       are superseded by the AE_ set for publication use.

═══════════════════════════════════════════════════
PURPOSE
═══════════════════════════════════════════════════
One exhibit per model assumption, built cleanly: every known artifact from
019/020 fixed AT THE SOURCE (not footnoted): the M3 population contamination
(install-only years), the E4 flag-vs-act mismatch (single_to_double_year),
the M2 quasi-separation (near-empty RB x DW x upgrade cell), the zero-fee FF
states, the rate-card denominator mismatch, the Q outlier. Exhibits feed a new
section of Reports/Paper/Scale_Incorporation_Model_Sketch.qmd (built by the
architect afterward; NOT part of this ticket).

═══════════════════════════════════════════════════
CANONICAL CHOICES (apply to EVERY analysis — no per-analysis drift)
═══════════════════════════════════════════════════
SAMPLE FRAME: facility-years in boy_composition_fy.csv, panel_year >= 1999,
  N >= 1, merged to facility_panel flags (NA->0) and dcm_obs (size_bin,
  rho_state where available). TX pre-2006 rows KEPT for physical/transition
  analyses, EXCLUDED (match dcm_obs spine) for CCP/regression analyses that
  condition on regime — each exhibit states its frame in its Notes.
ACTION CODING (6 categories, Exit precedence — identical everywhere):
  Exit = facility_complete_closure==1
  Replace = any_closure==1 & n_installs>0 & !Exit
  Downsize = any_closure==1 & n_installs==0 & !Exit
  Expansion = any_closure==0 & n_installs>0    (never pooled with model actions)
  Maintain = remainder
CELLS: wall {SW,DW} x 04b A_bin (8 five-yr bins) = 16; verbatim 04b breaks.
REMOVAL RULE (the model's stated rule, used wherever a prediction is needed):
  remove from the occupied cell highest in: SW oldest->youngest, then DW
  oldest->youngest.
REGRESSION SE: state-clustered (fixest CRVE) everywhere; G=18 noted.
WALL ACTS, NOT FLAGS: any upgrade/same-wall classification uses tank-level
  shed/install wall from boy_composition_long / install records — NEVER
  single_to_double_year.
STATE SW-INSTALL BAN DATES (hardcoded; email-verified sheet, read 2026-06-09):
  MA 1989-01-01 | ME 1991-09-16 | ID 2007-02-23 | AR 2007-07-01 | TN 2007-07-24
  | AL 2007-08-06 | NC 2007-11-01 | MN 2007-12-22 | IL 2008-02-01 | OK 2008-07-01
  | CO 2008-08-01 | LA 2008-12-20 | TX 2009-01-01 | ND 2009-01-01 | SD 2009-01-01
  | MD 2009-01-12 | VA 2010-09-15 | OH 2011-05-16 | KY 2012-04-01 | KS 2013-07-01
  | MO 2017-07-01
OUTPUT NAMES: Output/Tables/AE_*.csv (+ .tex for starred) ; Output/Figures/
  AE_*.png (dpi 300) AND AE_*.pdf (for the qmd).
PUBLICATION STYLE:
  Tables — SPECIAL-CHARACTER RULE (qmd/pandoc chokes on stray specials; fix by
    NOT EMITTING them rather than escaping cleverly):
    - cell text and headers: plain ASCII words and numbers ONLY. No %, _, $,
      &, #, ~, ^, <, > inside any cell or header. Write "pct" not "%",
      "per 1000 gal" not "/1,000gal", spell out names ("size 4 plus" not
      "4+ "... exception: "4+" is fine, plus sign is safe), use words for
      math ("x" for interaction is banned — restructure the row label instead).
    - NO math mode in .tex tables at all (no $...$ anywhere — this kills the
      $-before-digit pandoc bug by construction). Greek letters / math symbols
      belong in the qmd prose, not in table cells.
    - stars: plain asterisks appended to the number string (e.g. 2.85***).
    - numbers pre-formatted in R: fixed decimals, comma thousands. Never emit
      raw doubles.
    - structure: booktabs (\toprule/\midrule/\bottomrule), NO vertical rules,
      Panel A/B via a plain multicolumn text row; detailed notes go in the qmd
      Notes block, NOT in the .tex (the .tex contains ZERO caption/notes text).
  Figures — AER convention: NO title, NO subtitle, NO caption text baked into
    the image (ggplot title/subtitle/caption elements all blank/absent; the
    qmd figure caption carries all of it). Axis labels + legend only, plain
    ASCII. theme_minimal base_size 11; palette FF=#E76F51, RB=#2A9D8F,
    sequential blues for size; grey50 reference lines; legend bottom; 7in wide.

═══════════════════════════════════════════════════
EXHIBITS (analysis -> assumption it motivates)
═══════════════════════════════════════════════════
Scripts: Code/Analysis/AE01_Frame_Build.R (shared frame, reads intermediates
  + facility_panel + dcm_obs; the ONLY panel_dt read is in AE04 for the clogit)
  then AE02_State_Evidence.R, AE03_Transition_Evidence.R,
  AE04_WhichTank_and_Fidelity.R, AE05_Identification_Evidence.R.

---- X1. WHAT FACILITIES DO (motivates: the 4-action set; Expansion fencing) ----
  AE_X1_Action_Frequencies.csv* : action x size_bin{1,2,3,4+,All}; cols action,
    size_bin, n[int], rate_pct[num]. (.tex = All column, 5 rows, Panel A;
    Panel B = the simultaneity headline row: n facility-years >=2 sheds & >=2
    installs.)
  AE_X1_Simultaneity.csv : sheds{0,1,2,3+} x installs{0,1,2,3+}; n, share.

---- X2. SIZE SHIFTS BEHAVIOR AT FIXED STATE (motivates: counts in the state) ----
  Same logit design as 019-M1 (it was clean): per margin {Exit, Downsize,
  Replace} binary logit y_k ~ size_bin | s_idx, cluster=~state, dcm spine.
  AE_X2_Size_Logit.csv* : margin x size{2,3,4+} (9 rows); cols margin,
    size_term, odds_ratio, se, p, n. (n = per-margin estimation n.)
  AE_X2_CCP_by_Size.{png,pdf} : empirical share of each margin by (A_bin,
    size_bin), facet margin ~ wall, free y; publication style.

---- X3. THE PRICE SCHEDULE ACTS ON (WALL x AGE) (motivates: joint cells) ----
  AE_X3_RateCard.csv* : the 2006-era card as a 2x8 matrix (rows wall, cols age
    bin, $/tank/yr) + one row noting 2014/2019 re-filings. (.tex = the matrix.)
  AE_X3_Premium_vs_Hazard.{png,pdf} : P_index vs H(n) at composition level,
    color regime, size = facility-years (the two-worlds figure, restyled).

---- X4. CAPACITY IS A NEAR-FIXED TYPE (motivates: G fixed; frozen at M/R) ----
  AE_X4_Capacity_Stickiness.csv* : Panel A rows {one_year_stay, mean_spell_yrs,
    implied_never_change, actual_never_change} (from 04ag logic re-emitted);
    Panel B rows = G-move share AT DOWNSIZE events by size_bin (the open cell:
    BOY(t)->BOY(t+1) capacity-quartile move among Downsize events).
  DECISION READ: Panel B small (<~10%) -> G frozen everywhere; large -> G
    step-down kernel at D only.

---- X5. TANKS WITHIN A FACILITY ARE ALIKE (motivates: 16 coarse cells) ----
  AE_X5_Homogeneity.{png,pdf} + AE_X5_Homogeneity.csv : 04ah by-size shares
    re-emitted in pub style (wall-homog, age-homog, both, median age range).

---- X6. WHICH TANK GOES (motivates: the removal rule) ----
  Clogit identical to 019-M4 (it was clean; re-run for styling + one table):
  AE_X6_WhichTank.csv* : terms {age/yr, SW, capacity/1000gal}; odds_ratio, se,
    p, n_strata, n_obs. AE_X6_WhichTank_Forest.{png,pdf}.
---- X7. THE RULE PREDICTS REMOVALS (validates the rule + how-many question) ----
  Fidelity machinery from 04ao (plain rule), Downsize+Replace events:
  AE_X7_Removal_Fidelity.csv* : action x size x k{1,2+}; n, exact_match_share,
    wrong_wall_share, wrong_ageband_share. (.tex collapsed to action x size.)
  AE_X7_Shed_vs_Block.csv : Downsize events: n_shed vs size of top-priority
    cell: {less, exactly, more} x size_bin; n, share.   [decides one-tank vs
    block vs chosen-count removal]
  AE_X7_Kernel_Heat.{png,pdf} : predicted-cell x realized-cell heat map.

---- X8. REPLACE RECYCLES CAPACITY (motivates: n^R transition, no reset) ----
  POPULATION FIX (the M3 contamination, fixed at source): operating AND
  any_closure==1 ONLY. swap = n_installs>0; shrink = n_installs==0.
  AE_X8_Recycle.csv* : event_type x dcount_bin{0,-1,-2,<=-3}; n, share,
    median_dcap_pct, p25, p75, share_within10pct, share_shedSW_instDW
    (TANK-LEVEL act, not the flag).
  AE_X8_Recycle_Tests.csv : feols dcap_pct ~ swap and net_change ~ swap on the
    FIXED population, cluster=~state. (Eq.6 logit DROPPED — degenerate.)
  AE_X8_Capacity_Density.{png,pdf} : dcap density swap vs shrink (display trim
    noted in caption).

---- X9. NEW TANKS ARE DOUBLE-WALLED (motivates: e_{DW,1} install; era fact) ----
  AE_X9_Install_Decomposition.csv* : Replace events at has_SW facilities,
    TANK-LEVEL classes {shed_SW & inst_DW, shed_SW & inst_SW, shed_DW_only}
    x ban timing {pre-state-ban, post-state-ban}; n, share. Classes exhaustive
    + mutually exclusive; counts must sum to the has_SW replace-event total.
    PRINT: true-violation share (shed_DW_only) and post-ban inst_SW share
    (data-quality tripwire).
  AE_X9_Install_DW_Share.{png,pdf} : DW share of installs by year, TX vs
    Control; vertical tick at each state ban date (small rug, not labeled).

---- X10. THE REGIME MOVES THE MARGINS (motivates: downsize as an action;
           regime-relevant replace) ----
  QUASI-SEPARATION FIX (at source): no RB x wall interaction. Three clean
  regressions, dcm spine, year FE, cluster=~state:
    (a) Downsize margin, full sample:    y_D ~ A_bin + w + size + regime | yr
    (b) Replace margin, has_SW ONLY:     y_R ~ A_bin + size + regime | yr
        (on has_SW facilities replace ~= upgrade under the rule; single
         regime coefficient, no empty-cell artifact)
    (c) Exit margin, full sample:        y_X ~ A_bin + w + size + regime | yr
  AE_X10_Regime_Margins.csv* : rows {Downsize_all, Replace_hasSW, Exit_all};
    cols margin, sample, odds_ratio, se, p, n, n_states_RB.
  CAVEATS in Notes (qmd, not table): single treated state; TX 2009 ban
  partially overlaps the upgrade margin post-2009; causal claims -> DiD.

---- X11. PREMIUM VARIATION IDENTIFIES gamma_p (identification) ----
  AE_X11_Premium_Regressions.csv* : rows {RB, FF_all, FF_feepos, pooled};
    cols spec, r2, resid_sd, resid_sd_over_sdP, coef_regime, se_regime, n.
    Zero-fee state list + share printed and put in qmd Notes.
  AE_X11_RateCard_Fit_PerTank.csv : per-tank fit (P_total_RB/N vs
    mean_tank_premium), by era; n, pearson, r2, mean_actual, mean_simplified.
    [fixes the denominator channel; if r2 still low, the gap is loads/status —
    report, do not redesign cells in this ticket]
  AE_X11_Correlations.csv : index correlations pooled/FF/RB; EXTRA column
    pearson_qtrim excluding Q > p99.9; print panel_id/year of the Q=2.0e9 row.

---- X12. THE STATE SPACE IS COVERED (feasibility) ----
  AE_X12_Occupancy.csv : the E5 metrics re-emitted; AE_X12_Coverage.{png,pdf}.

═══════════════════════════════════════════════════
R-IMPLEMENTATION NOTES
═══════════════════════════════════════════════════
- Reuse Data/Analysis/boy_composition_{long,fy}.csv (04al outputs) — do NOT
  rebuild them. AE04 may fread panel_dt (select cols, filter-first) ONLY for
  the clogit sample.
- Rate-card p_cell + era map: copy the (already verbatim) block from
  04an_Index_Separation.R. h_cell from the primitives rds as in 04an.
- One shared frame builder (AE01) saves Data/Analysis/ae_frame.csv consumed by
  AE02/03/05; AE04 self-contained.
- fixest everywhere; no plain glm; no bootstrap; no joint tests; no
  tryCatch-NULL; cat section markers; logs/AE0x_*.log or runner capture.
- Every .tex: booktabs; numbers formatted in R (no raw 15-digit floats);
  stars from clustered p ONLY where the qmd table is a regression table.
- PNG dpi=300 AND PDF via one ggsave each.

═══════════════════════════════════════════════════
ACCEPTANCE CRITERIA (reviewer)
═══════════════════════════════════════════════════
- [ ] One action coding, one cell definition, one removal rule — byte-identical
      across scripts (defined once in AE01, sourced or copied verbatim)
- [ ] X8 population = closure-years only (NO install-only years anywhere in X8)
- [ ] X9 classes tank-level, exhaustive, sum to has_SW replace total; ban dates
      hardcoded as listed
- [ ] X10 contains NO regime x wall interaction; Replace margin estimated on
      has_SW subsample
- [ ] X11 includes FF_feepos row + zero-fee list; per-tank fit present;
      Q-trim sensitivity column present
- [ ] X4 Panel B (G-move at downsize) and X7 Shed_vs_Block present — the two
      open design cells get their deciding numbers
- [ ] All starred tables exist as booktabs .tex; all figures as PNG(300)+PDF
- [ ] .tex tables contain NO math mode and NO special characters (%, _, $, &,
      #, ~, ^) in any cell/header; numbers pre-formatted; no caption/notes
      text inside the .tex
- [ ] Figures contain NO title/subtitle/caption text in the image — axis
      labels and legend only (qmd captions carry titles, AER style)
- [ ] All SEs state-clustered; per-margin n reported; scripts exit 0

═══════════════════════════════════════════════════
ATTEMPT LOG
═══════════════════════════════════════════════════
ATTEMPT 1 — 2026-06-11 — VERDICT: PASS (Opus acting reviewer; verified from all
five scripts + output CSVs/.tex + a rendered-figure check, independently).

FAITHFULNESS VERIFIED:
- AE01 canonical frame: one size_bin (from N, 04b fcase), one action coding,
  key-uniqueness stopifnots, targeted NA->0, split rule (flags=facility_panel,
  magnitudes=composition) with the disagreement diagnostic (0.0003).
- ASCII rule enforced PROGRAMMATICALLY: assert_ascii_clean() stops on any
  forbidden char before any .tex write; independent scan of all 13 .tex = clean;
  no math mode anywhere; numbers pre-formatted.
- Figures: th_ae blanks title/subtitle/caption; rendered-PNG check confirms
  no baked titles; PNG dpi=300 + PDF for all 8 figures.
- X8 population = closure-years only; X9 classes tank-level, exhaustive
  (sum = 5,802), ban dates byte-match the spec list; X10 has NO regime x wall
  interaction, Replace on has_SW subsample; X11 has FF_feepos row, zero-fee
  list (KS MD MN OK SD VA; 40.2 pct of FF fy), per-tank fit, Q-trim column;
  X4 Panel B and X7 Shed_vs_Block present (the two open design cells).
- AE04 clogit = 04ak exact (n_strata 19,744 in band; SW OR 2.85); fidelity
  machinery = 04ao verbatim (pooled 0.840 / wrong-wall 0.028 reproduced).
- All 27 tables + 8 figure pairs on disk; all SEs state-clustered per spec
  (RB-only row unclustered per standing architect ruling); exit 0 x5.

BLESSED DEVIATIONS:
- D1 X5 homogeneity re-derived from facility_panel min/max-age + all_sw/all_dw
  (equivalent definitions) rather than re-running 04ah tank-level — numbers
  shift slightly (both-homog 96.3->64.5 by size vs 04ah 93.2->62.0) due to
  frame; acceptable re-emission, cite AE numbers in the qmd.
- D2 exhibit->script grouping chosen by coder (spec did not pin it).
- D3 X1 .tex Panel-B separator cosmetic (midrule placement) — compiles fine.

DESIGN DECISIONS LANDED (pre-committed reads):
- DD1 X4 Panel B: G-move at downsize = 0.42 overall (0.31-0.52 by size)
  -> LARGE -> capacity bin gets a STEP-DOWN KERNEL AT DOWNSIZE ONLY
  (frozen at Maintain/Replace per Panel A + X8).
- DD2 X7 Shed_vs_Block: removals are mostly LESS than the top-priority block
  at sizes 3/4+ (60-62 pct "less"; "exactly" 27-32 pct) -> downsize is a
  CHOSEN-COUNT (intensity) action: each k an alternative, cost k*c^D, still
  one parameter; not one-tank, not full-block.
- DD3 X9: true rule violation (shed DW only) = 1.3 pct -> NO wall sub-choice,
  CONFIRMED. Install-SW is a pre-ban phenomenon (31.1 pct of has_SW replaces
  pre-ban -> 2.7 pct post); post-ban inst-SW = 6.0 pct of post-ban events =
  data-quality note (likely missing wall on new installs), not behavior.

NEW IDENTIFICATION FINDING (researcher attention):
- X11 per-tank rate-card fit is POOR: r = 0.086 (era 2006), 0.353 (2014),
  0.598 (2019); simplified per-tank mean $386 vs actual $291. The earlier
  total-level fit (0.43-0.64) was mostly tank-COUNT correlation. The actual
  per-tank premium varies with status (TOU -0.50/-0.75), leak-history and
  piping loads that the 2-dim card zeroes. RECOMMENDATION for model build:
  define p_c EMPIRICALLY as the cell-mean of ACTUAL rated per-tank premiums
  by (wall x age bin x era) from the 04a engine output, instead of the
  theoretical card — keeps the 16-cell state, makes P_total(n) track reality
  at cell level by construction. Decide before the estimator spec.
- Minor data note: 69 size-1 "Downsize" events (definitionally near-impossible;
  flag/composition mismatch) — noise, excluded nowhere, document in qmd.
