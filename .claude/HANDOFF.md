# HANDOFF -- 2026-05-18 (Ticket 003 PASS + polish-direct qmd report)
# Written by: Opus (architect for T003; polish-direct for the qmd)
# Session type: Architect drafted Ticket 003; Sonnet-on-Pro implemented;
#               Opus reviewed PASS; then wrote Reports/Paper/
#               Dynamic_Model_Fit_Report.qmd directly under polish exception.

═══════════════════════════════════════════════════
CHECKPOINT: T003 PASS + polish-direct fit report (2026-05-18)
═══════════════════════════════════════════════════

Ticket 003 — Harmonized goodness-of-fit artifacts + 3-way model comparison
  Spec:        .claude/TICKETS/003_spec.md  (PASS on attempt 1)
  Transcript:  .claude/TICKETS/003_transcript.txt
  Coder:       Sonnet 4.6 via Anthropic Pro account (OAuth)
  Reviewer:    Opus (this session)

  Modified/created (T003 coder, R script):
    Code/Dynamic_Model/04k_Fit_Report_Artifacts.R     (new)

  T003 deliverables (11 files):
    Output/Figures/04k_Fit_4param_SW.png
    Output/Figures/04k_Fit_4param_DW.png
    Output/Figures/04k_Fit_8param_SW.png
    Output/Figures/04k_Fit_8param_DW.png
    Output/Figures/04k_Fit_8paramFE_SW.png
    Output/Figures/04k_Fit_8paramFE_DW.png
    Output/Tables/04k_PerCell_Fit_8paramFE_Wide.csv         (32 rows)
    Output/Tables/04k_FitQuality_8paramFE_byWallRegimeAction.csv (12 rows)
    Output/Tables/04k_Model_Comparison_3way.csv             (3 rows)
    Output/Tables/04k_Model_Comparison_3way.tex
    Output/Tables/04k_FE_Alphas_Compact.tex                 (18 body rows)

  Headline results:
    Spec         k    log L         AIC        BIC        LR vs nested   df
    4-param      4    -268,481      536,971    537,021    --             --
    8-param      8    -261,770      523,557    523,658    13,422.2        4
    8-param+FE   25   -251,613      503,277    503,593    20,314.1       17
    N = 2,282,735 (identical across rows). Both nested LRs reject at p~0.

Polish-direct (Opus, no R1 ticket):
  Reports/Paper/Dynamic_Model_Fit_Report.qmd  (new)
  Consumes the 11 T003 artifacts plus existing
  04g_Theta_Table_AM_with_CIs.tex, 04h_Theta_Table_8param_AM_SE.tex,
  04i_Theta_Table_8paramFE_AM_SE.tex, 04g_Per_Cell_Fit_Numbers_Wide.csv,
  04h_8param_PerCell_Fit_Wide.csv, 04g_Fit_Quality_byWallRegimeAction.csv,
  04h_8param_FitQuality_byWallRegimeAction.csv.

  Polish-exception eligibility verified:
    - Pure presentation: readLines/cat of pre-rendered .tex; kable of
      pre-computed CSVs; include_graphics of pre-rendered PNGs.
    - No estimation logic touched (no Bellman, no V inversion, no CCP
      update, no NPL outer loop, no equilibrium solver, no primitives,
      no tolerances).
    - Downstream consumer only -- the .rds fits are inputs to the
      11 T003 artifacts, which are inputs to this qmd. No .rds touched
      directly here.
    - User explicitly waived architect role for the qmd write-up.

Spec errors recorded for future tickets (see 003_spec.md Attempt Log):
  (1) FE estimator saves $Phat (no underscore), 4p/8p save $P_hat.
      Coder added load-time alias. Substantive fix would re-name in
      04i.R, but that triggers re-estimation (hours) -- deferred.
  (2) Step 5 ncol == 7 in spec was wrong (should be 8). Template-
      mirrored fix in code is correct.
  (3) r5()-rounded sum-tolerance: spec said 1e-6 on rounded columns,
      coder kept 1e-6 on raw and relaxed printed-table check to 2e-5.
      The right fix (preserves the real invariant).

Substantive observation (for future research, not a defect):
  FE alphas for MO (8.18 +/- 0.77) and SD (2.27 +/- 0.08) are much
  larger than the other 15 control-state alphas (which span roughly
  [-1.4, +0.8]). Consistent with near-zero observed exit/replace in
  those state cells. Footnoted in the qmd.

Open / next:
  - User to render the qmd: `quarto render Reports/Paper/Dynamic_Model
    _Fit_Report.qmd` from repo root. (Quarto handles longtable + kable
    + the .tex inputs.) Inspect resulting PDF.
  - If MO/SD alphas warrant deeper inspection, that is a new ticket --
    likely a data-coverage diagnostic rather than an estimator change.
  - 04i.R save-name fix (rename $Phat to $P_hat) is a housekeeping
    candidate -- queue alongside next re-estimation run, not on its own.

Resume: "Load CLAUDE.md. T003 PASS + Dynamic_Model_Fit_Report.qmd
        written under polish exception. Today: [task]"

  -- end T003 checkpoint --

═══════════════════════════════════════════════════
[Previous checkpoint preserved below]
═══════════════════════════════════════════════════

# HANDOFF -- 2026-05-14 (presentation polish)
# Written by: Opus (presentation-polish-direct mode, not architect)
# Session type: Slide deck refactor for 04_Risk_Based_Pricing_and_USTs

═══════════════════════════════════════════════════
CHECKPOINT: deck densification + structural TBD greyout (2026-05-14, fourth)
═══════════════════════════════════════════════════
Polish-direct per researcher: deck was too text-heavy, structural model
results aren't final, slide titles cluttering figure-only slides.

  Reports/Slides/04_Risk_Based_Pricing_and_USTs.qmd:
    Deletions:
      - "Contribution" slide (was Section 1, 4 bullet groups)
      - "LUST Event Study (3-panel Combined)" content slide -- moved to
        appendix per "i hate the panel version"

    Rewrites:
      - "What We Find" rewritten with new reduced-form findings
        (+1.58 pp ATT, 4x vintage HTE, LUST -1.27 pp, +53% replacement
        margin) and a greyed-out structural block with TBD placeholders
        (color gray!55).

    Reduced-form section split for breathing room:
      - "Sample and Estimand" + "Specification and Identifying Variation"
        (2 dense slides) -> 4 lighter slides: Sample / Estimand /
        Specification / Identifying Variation.

    Title stripping (figure-only slides now use empty `##`):
      - FR Compliance, Market Structure (HHI), Premium Levels (TX vs ctrl),
        We Can Predict Risk, Premiums Track Risk, Fleet Composition,
        Cleanup Cost Distribution, Event Study, Welfare Decomp, Subsidy
        Curve.

    Button repositioning to allow full-figure layouts where slides had
    a button below the figure:
      - We Can Predict Risk and Premiums Track Risk slides:
        figure now uses layout-full-figure (tikz overlay), button
        positioned via separate tikz overlay anchored to current page
        south-east corner. Should not clash with navbutton's inline
        tikz since they're independent picture envs.

    Discussion-bullet removal on table slides (researcher: "we dont need
    to discuss the result of table on the slide we can put that after
    each table"):
      - Static DiD: dropped the trailing caption sentence.
      - Heterogeneity: dropped the 3 beta-decomposition bullets.
      - LUST Discovery: dropped the intro sentence + 3 bullets;
        added a \navbutton[lust-es] to the appendix LUST ES.
      - What Do Closing Firms Do: dropped the 4 explanation bullets;
        kept the descriptive-not-causal one-line intro.

    Structural section TBD greyout (researcher: "no real numbers those
    model results are not final"):
      - Split "Identification and Estimation" -> "Identification"
        (table+protocol bullets) and "Estimates (Preliminary)"
        (just the 4-param table, wrapped in \color{gray!55} +
        "PRELIMINARY --- estimates not final" banner above).
      - "Welfare Decomposition" slide: title -> empty, "PRELIMINARY"
        banner at top, figure wrapped in \color{gray!55}.
      - "The Subsidy Curve" slide: same treatment.
      - Counterfactuals: dropped the "Key Finding" paragraph that
        described the result direction (used preliminary numbers).
      - Order unchanged -- structural section is still last as required.

    Appendix additions:
      - "Appendix: LUST Event Study -- Total Leak Discovery" with
        \hypertarget{lust-es} (target of the LUST DiD slide's button).
        Uses Fig_ES_LUST_Total.pdf (single panel).
      - "Appendix: LUST Event Study -- Decomposition" using
        Fig_ES_LUST_Combined.pdf (3-panel) as a fallback because Z
        disconnected mid-run before single-outcome Background and
        Inspection-triggered ES figures could be generated.

KNOWN PENDING:
  - Output/Figures/Fig_ES_LUST_Background.{pdf,png} -- NOT generated;
    Z drive disconnected during the rerun of 02c. Modified 02c writes
    these via the same plots-list loop once Z is reachable. Researcher
    "hates the panel version", so the single-outcome figures are the
    preferred replacement.
  - Fig_ES_LUST_Inspection.{pdf,png} -- same as above.
  - Until those exist, the Decomposition appendix slide uses the
    Combined 3-panel figure as a fallback.

═══════════════════════════════════════════════════
CHECKPOINT: LUST on alive-at-reform + stepped-DiD col cleanup (2026-05-14, third)
═══════════════════════════════════════════════════
Polish-direct (researcher pointed out LUST was still on the paper's CEM sample,
not alive-at-reform, and asked for cleanup of stepped DiD .tex):

  Code/Analysis/02c_Closure_Conditional_Enriched.R (EXTENDED)
    Added LUST DiD block AFTER the conditional-on-closing block. Reads
    leak_year + tank_closure_revealed from facility_panel.csv in the same
    `select=` (no second read of 3.9 GB file). Sample = incumbent
    (fac_is_incumbent==1, NOT conditioned on any_closure). Outcomes:
      leak_year                       Total leak discovery (0/1)
      lust_standalone (computed)      Background = leak_year==1 & tank_closure_revealed==0
      tank_closure_revealed           Inspection-triggered (Primary 0-60d)
    Same FE spec as conditional block: panel_id + make_model_fac^panel_year,
    cluster by state.
    Output: Output/Tables/T_LUST_Incumbent_Slide.tex
    Headline (cm = pre-reform control mean):
      Total                cm 1.73% -> TX -1.27pp***  (-73% relative)
      Background           cm 1.16% -> TX -0.76pp***
      Inspection-triggered cm 0.61% -> TX -0.51pp***
    N facility-years = 4,650,240 (after singleton drop from 4.65M).
    Reassuring: numbers track the paper CEM result closely (paper Total:
    cm 1.47%, beta -1.20pp). Robustness across samples.

  Output/Tables/T_DiD_Stepped_{Full,Pre89,Post88}.tex     (REWRITTEN)
  Output/Tables/T_HTE_DiD_Vintage.tex                     (REWRITTEN)
    Surgically dropped col 3 ("(3) + Controls" -- mandates without cell FE),
    renumbered col 4 ("(4) + Cell FE") to col 3. Files now have three data
    columns: Raw / +Fac FE / +Cell FE. Tabular alignment changed from
    lcccc to lccc. Reformatted by hand from existing fixest etable output;
    values transcribed exactly from the originals.

  Reports/Slides/04_Risk_Based_Pricing_and_USTs.qmd:
    - LUST slide: \input switched
      T_LUST_Slide.tex -> T_LUST_Incumbent_Slide.tex.
      Prose updated to the incumbent-sample numbers (73% relative drop,
      not 80%). "Same sample as the closure analysis" phrase added so it's
      clear LUST shares the alive-at-reform restriction.
    - The hand-built T_LUST_Slide.tex from earlier in the session is now
      orphaned but kept on disk in case the CEM-matched numbers are
      wanted for paper/appendix later.

ANSWER TO RESEARCHER'S DIRECT QUESTION:
  Event study (closure): already on alive-at-reform -- Fig_ES_Full.pdf is
    generated from data_C_active in the active-at-treatment closure analysis
    chunk. No re-run needed.
  LUST DiD: NOW on alive-at-reform via 02c. Earlier table used CEM-matched
    paper numbers.
  LUST event study: DONE in subsequent invocation (figures only -- no tables
    per researcher direction). Three outcomes (Total, Background,
    Inspection-triggered) fit with i(rel_year_es, texas_treated, ref=-1L),
    same FE spec as static DiD, on 4.6M-row incumbent sample. Output:
      Output/Figures/Fig_ES_LUST_Combined.{pdf,png}  (3-panel stacked)
      Output/Figures/Fig_ES_LUST_Total.{pdf,png}     (single panel)
    New content slide added between LUST DiD and conditional-on-closing,
    using the Combined 3-panel figure.

    INTERPRETATION CAVEAT: The LUST ES pre-trend is NOT flat -- TX leak
    rates were trending down toward control levels from -12 to -1 (driven
    by ongoing EPA mandate compliance), and then dropped further at t=0.
    Unlike the closure ES (Fig_ES_Full.pdf) which has a flat pre-trend
    (slope test p=0.177), the LUST static DiD coefficient may be partly
    capturing the pre-existing convergence trend rather than a discrete
    shock. The current Identification Validity slide makes the "flat
    pre-trend" claim for the CLOSURE outcome only; it does NOT cover LUST.
    Honest read: closure ATT is well-identified; LUST effect mixes
    pre-trend + discrete drop. Flag for researcher to address in Q&A
    or with a HonestDiD-style bound for LUST.

UNREFERENCED FILES the researcher moved locally (not in current deck, fine
to leave; available for appendix slides if needed):
  Output/Tables/T_DiD_Stepped_Pre89.tex, T_DiD_Stepped_Post88.tex
  Output/Tables/T_ES_Active_Main.tex, T_ES_Active_Ref2.tex
  Output/Figures/Fig_ES_Pre89.{png,pdf}, Fig_ES_Post88.{png,pdf}
  Output/Figures/Fig_ES_Combined_3Panel.{png,pdf}

═══════════════════════════════════════════════════
CHECKPOINT: 04 SLIDE DECK -- LUST + ENRICHED CONDITIONAL (2026-05-14, second invocation)
═══════════════════════════════════════════════════
Polish-direct (researcher explicitly waived architect role: "you are free to
edit ... and run the code"):

  Code/Analysis/02c_Closure_Conditional_Enriched.R  (NEW, sibling to 02a)
    Reads facility_panel.csv from Z (read-only), filters to
    fac_is_incumbent == 1 + study states + 1985-2018, restricts to
    any_closure == 1, runs DiD with did_term + mandate controls and FEs
    panel_id + make_model_fac^panel_year, cluster by state. SE clustered
    by 17 (or 18 including TX) states. No CEM matching -- researcher
    explicitly OK'd "all facilities alive at Dec 1998" as the sample.
    Outcomes: facility_complete_closure, permanent_closure_year,
    replacement_closure_year, any_dw_install_year (= n_dw_installs>0),
    single_to_double_year, net_tank_change, capacity_change.
    Writes Output/Tables/T2b_Enriched_Slide.tex
      (cols: outcome | pre-reform ctrl mean | TX x Post | SE).
    Headline:
      - close-all-tanks ctrl 70.9% (small-fac dominated) -> TX +6.5pp***
      - permanent      ctrl 88.3% -> TX -6.2pp***
      - replacement    ctrl 12.0% -> TX +6.3pp***   (+53% relative)
      - any DW install ctrl  4.1% -> TX -1.4pp*  (fewer greenfield DW)
      - SW->DW         ctrl  2.0% -> TX +0.8pp   (n.s.)
      - net tanks      ctrl -1.9   -> TX +0.02   (n.s.)
      - capacity (gal) ctrl -100k  -> TX -$1,397 (n.s.)
    N closing facility-years = 59,107 (after singleton drop from
    make_model_fac^panel_year FE; 229k pre-singleton).
    Note: facility_complete_closure (n_closures>0 & n_tanks_eoy==0) is
    the event-level "closes all tanks" var; do NOT use facility_exit
    (it is the last-observed-year indicator and is contaminated by
    panel truncation -- gave 69% ctrl mean before the swap).

  Output/Tables/T_LUST_Slide.tex   (NEW, hand-built from
    T_LUST_A_Total.tex + T_LUST_B_Decomp.tex 0-60d window;
    dropped broken BJS col4 and empty bootstrap-p rows)
  Output/Tables/T2b_ClosureConditional_Slide.tex (NEW, superseded
    by T2b_Enriched_Slide.tex above -- the older 3-row variant; can be
    deleted if not referenced anywhere)

  Reports/Slides/04_Risk_Based_Pricing_and_USTs.qmd:
    - Stripped ALL \begin{assumptionbox}{...}\end{assumptionbox} wrappers
      (6 occurrences); content kept as plain bold-label markdown. The
      researcher found them "super distracting."
    - Reduced-form section now includes two new slides between
      Heterogeneity and Identification Validity:
        (a) LUST Discovery (uses T_LUST_Slide.tex)
        (b) What Do Closing Firms Do? (uses T2b_Enriched_Slide.tex,
            with pre-reform ctrl mean made explicit in the prose
            so the TX*Post coefficient reads as a deviation from
            baseline closing-firm behavior).
    - Descriptive section: FR coverage composition added before
      market structure; market-structure swapped to Top5_Dominance_HHI;
      added Premium Levels TX vs ctrl; risk plot swapped to CV_CellRisk
      + GoF appendix button; added Premiums-Track-Risk slide +
      Premium_vs_EL appendix button; replaced Claims slide with
      Cleanup Cost Distribution.

  No edits to 02b_Tank_level_Panel_Build.R despite researcher's
  permission to do so. facility_panel.csv on Z already has every column
  needed (fac_is_incumbent + outcomes); a new sibling script (02c)
  was strictly cheaper than touching a 3000-line panel builder.

  Files NOT regenerated: T_LUST_A_Total.tex, T_LUST_B_Decomp.tex,
  T2b_ClosureConditional.tex (the originals from 02a). Paper uses
  those; slides use the new *_Slide.tex variants.

═══════════════════════════════════════════════════
CHECKPOINT: 04 SLIDE DECK + 4P WELFARE VISUALS (2026-05-14)
═══════════════════════════════════════════════════
Polish-direct invocations this session (per CLAUDE.md exception):
  Code/Dynamic_Model/04j_4p_Welfare.R
    Section 10: subsidy-curve plot reskinned (Berkeley palette, Times,
                $/facility units via SCALE_FACTOR, HEALTH_ONLY only, no title).
                Now writes .pdf + .png.
    Section 14: bar chart rewritten as TRUE stacked bar — components signed
                so they sum to dSocialWelfare (+dFirm, -dExternalPV, -dGovtPV);
                black diamond + horizontal segment mark net at top of stack.
                HEALTH_ONLY only, $/facility units, reader-facing labels
                ("Subsidy" / "Pigouvian" / "Mandate" -- no CF_C tags).
    Section 14b (NEW): LaTeX welfare summary table
                       Output/Tables/04j_Welfare_4p_Summary.tex
                       (Scenario | P_M | P_E | P_R | Firm Surplus
                        | Social Welfare | dW), HEALTH_ONLY rows only,
                       $/facility PV.
    No Bellman / V / CCP / equilibrium / theta / cache code touched.
    Smoke test passes (Section 7); baseline FirmSurplus = $307,724 (unchanged
    from prior run).

Reports/Slides/04_Risk_Based_Pricing_and_USTs.qmd:
  - Fixed math error: `$\sim$500,000` and `$\sim$60-90%` -> proper math spans
    (pandoc rule: closing $ cannot precede a digit).
  - Switched structural slides 22-24 from 8-param to 4-param (kappa, K,
    gamma_p, gamma_r); swapped table 04h_Theta_Table_8param_AM_SE.tex ->
    04f_Theta_Table_AM_SE.tex.
  - Model Setup slide: added \begin{assumptionbox}{Units} stating
    1 model unit = R = $10,000/yr/facility; flow utility now shown with
    leading `1` (= R) so the normalization is explicit.
  - Reduced-form section: split into two ID slides (Sample & Estimand,
    then Specification & Identifying Variation -- conditional parallel
    trends, FW identifying variation). Survivorship de-emphasized per
    researcher.
  - Event-study figure -> Fig_ES_Full.pdf (full-sample unweighted).
  - Static DiD -> T_DiD_Stepped_Full.tex (col 4 main).
  - NEW slide: Heterogeneity (DiD x Pre89 interaction) using
    T_HTE_DiD_Vintage.tex; bottom-right \navbutton (template macro at
    my-template.tex:263) jumps to appendix slide with Fig_Vintage_Forest.pdf
    (hypertarget vintage-forest, visibility=uncounted).
  - Toy-model section pulled from 02_IO_Workshop_Slides_Feb_10_UPDATED.qmd
    slides 7-18: section card + 5 prose slides (Agent's Decision, Dynamic
    Problem, Insurance Regimes, First Best, Three Facts) + 6 full-figure
    slides (Slide_Fig_2..7 all already in Output/Figures).
  - Welfare slide split into TWO: Welfare Decomposition (stacked bar
    + summary table) and The Subsidy Curve (full-figure). 04j outputs
    are .pdf now.
  - Counterfactuals slide reduced to 3 policies (Subsidy / Pigouvian /
    Mandate) with $E=$17,000 only.

CLAUDE.md:
  - Added "PRESENTATION-POLISH EXCEPTION" block documenting when Opus may
    edit and run R directly (no R1 ticket): pure presentation work
    only -- no estimation logic, downstream consumer of an existing .rds
    fit, user has waived architect role. Each use must be logged here.
  - Added "UNIT CONVENTION" block stating 1 model unit = $10K = R,
    set at 04b_Replacement_Panel_Prep.R SCALE_FACTOR, with the flow
    utility leading-`1` interpretation.

UNITS — TRIPLE-CHECKED THIS SESSION (do not re-derive without reading):
  - 04b_Replacement_Panel_Prep.R:243-247 divides L_vec by 10000.
  - improved_estimator_OPTIMIZED.r:2633 flow utility:
        u_M <- 1 + gp*P_vec - gr*hazard_loss
    Leading `1` is annual revenue R, normalized.
  - Therefore V, FirmSurplus, ExternalPV, GovtOutlayPV, SocialWelfare are
    in $10K-PV/facility. Multiply by SCALE_FACTOR=10000 for $/facility PV.
  - Baseline FirmSurplus 30.77 (units) = $307,700/facility PV. Sniff: with
    beta=0.95 and steady-state revenue ~$10K/yr, max PV ~ $200K + scrap
    option. Passes.

KNOWN WARNING:
  - R complains "font family not found in Windows font database" when
    saving Times-family ggplots. Output renders correctly (Cairo PDF
    available) but PNG may use default sans-serif. Beamer will embed
    the PDF, so slide deck is unaffected.

Z-DRIVE NOTE FOR FUTURE-YOU:
  - 04 deck references LOCAL paths (Output/Tables, Output/Figures) via
    here::here and `../../`. Outputs generated on Z (e.g. T_DiD_Stepped_Full,
    Fig_ES_Full, T_HTE_DiD_Vintage, Fig_Vintage_Forest) must be synced
    to local Output/ before knitting the deck. Closure analysis was
    still running at session close.

NEXT (when researcher resumes):
  - Sync Z -> local Output/ once closure analysis finishes.
  - Render the deck and eyeball; the slide count is now ~30 (5-per-section
    cap intentionally relaxed per researcher; will trim later).
  - T003 still queued (8p+FE + retrofit alphas) per memory.

---

# Below: previous handoff (Ticket 001 complete) preserved for context.
---

# HANDOFF -- 2026-05-13 (evening)
# Written by: Opus (architect, end-of-session)
# Session type: First multi-agent workflow run -- Ticket 001 complete

═══════════════════════════════════════════════════
STOPPING POINT
═══════════════════════════════════════════════════
Task in flight: NONE -- Ticket 001 (25-param FE estimation + welfare with
                Marcus E sensitivity) completed end-to-end at ~21:12.
                All deliverables landed. Headline numbers reviewed by
                researcher. Architect wrote four follow-up notes in
                session (not yet operationalized into tickets).

Why we stopped: Natural checkpoint. Researcher saw the deflated-gamma
                result, identified four substantive follow-ups, and
                wants to break before any more agent spend until the
                workflow guardrails (max_tokens cap, non-reasoning model,
                spend ceiling) are in place.

Cost summary:
  Ticket 001 agent spend (R1 via OpenRouter):  ~$12
  Compute spend (local Rscript):                $0
  Wall time (estimation only, not coding):     ~3 min
  Wall time (full workflow including coding):  ~4 hours (with retries
                                                + dependency discovery)

═══════════════════════════════════════════════════
COMPLETED THIS SESSION
═══════════════════════════════════════════════════
Architecture (Opus, this Claude Desktop session):
  .claude/TICKETS/001_spec.md                                   -- written
  .claude/run_coder.ps1                                         -- ASCII-cleaned,
                                                                  --append-system-prompt fix,
                                                                  retry-note refactor
  Memory: zdrive_data_root.md                                   -- updated with
                                                                  Z workflow rule + architect
                                                                  precondition checklist

Implementation (R1 via OpenRouter coder session):
  Code/Helpers/improved_estimator_OPTIMIZED.r   -- dead AR-base FE block deleted
                                                   (lines 3399-3623); column-name
                                                   patch (s_idx/y_it/I_replace) applied
                                                   via setnames inside
                                                   .build_counts_weights_8p_fe
  Code/Dynamic_Model/04i_8param_FE_and_Welfare.R -- silent tryCatch removed (.3);
                                                    E_GRID refactor with HEALTH_ONLY=$17K
                                                    and HEALTH_PLUS_UNMEASURED=$50K
                                                    (Marcus 2021 calibration);
                                                    compute_social_welfare_8p
                                                    parameterized over E; CF B re-solved
                                                    per E (m_FF, m_RB depend on E); QSD CSV
                                                    duplicates B for both E values;
                                                    figure faceted by E_label
  Code/Dynamic_Model/04h_Replacement_8param_Estimation.R -- RUN_OBSERVED/
                                                            RUN_EXTENDED_2000PLUS gates
                                                            added at top of file; all
                                                            extended-branch blocks wrapped
                                                            in if(RUN_EXTENDED_2000PLUS);
                                                            list-build + conditional-insert
                                                            pattern for combined tables
  Code/Dynamic_Model/04i_smoke_test.R           -- NEW: 9-step plumbing test
                                                   (cpp engine compile check, cache build,
                                                   1x likelihood eval, 1x CCP update);
                                                   all assertions passed

Estimation runs:
  04c_Replacement_Estimation.R                   -- ran; produced
    Output/Estimation_Results/Model_Replacement_Estimates_observed.rds
    Output/Estimation_Results/Model_Replacement_Estimates_extended.rds
  04h_Replacement_8param_Estimation.R (patched)  -- ran on observed only; produced
    Output/Estimation_Results/Model_Replacement_8param_observed.rds
  04i_8param_FE_and_Welfare.R (patched)          -- ran; produced
    Output/Estimation_Results/Model_Replacement_8paramFE_observed.rds (713 KB)
    Output/Tables/04i_Theta_Table_8paramFE_AM_SE.csv (25 params, AM-2002 SEs)
    Output/Tables/04i_Theta_Table_8paramFE_AM_SE.tex
    Output/Tables/04iFETableAllControls.csv (17 control alphas)
    Output/Tables/04iWelfareSocial03Style.csv (10 rows: 5 scenarios x 2 E)
    Output/Tables/04iWelfareSocialDecomp.csv (8 rows: 4 non-base x 2 E)
    Output/Tables/04iQSDmubyScenario.csv (QSD weights)
    Output/Tables/04i_Welfare_PerCell.csv (128 rows: 32 states x 4 CFs)
    Output/Tables/04i_Welfare_Aggregate.csv
    Output/Tables/04i_Welfare_Summary.tex
    Output/Tables/04i_Compare_8p_vs_8pFE.csv (8 rows)
    Output/Figures/04iWelfareSocialDecomposition.png (faceted by E)
    Output/Figures/04i_Welfare_DeltaV_byCell.png

═══════════════════════════════════════════════════
HEADLINE RESULTS
═══════════════════════════════════════════════════
FE WORKED PARTIALLY -- did not fully solve the gamma inflation:
  gamma_price_FF: -14.38 (8p no FE)  -->  -11.09 +/- 0.343 (8p+FE)   [-23% magnitude]
  gamma_price_RB:  -7.48              -->   -6.51 +/- 0.163          [-13% magnitude]
  gamma_risk_FF:    0.065             -->    0.088 +/- 0.005
  gamma_risk_RB:   -0.083             -->   -0.120 +/- 0.009
  kappa_SW:        23.68              -->   24.67 +/- 0.081
  kappa_DW:        21.18              -->   21.93 +/- 0.093
  K_SW:             5.81              -->    5.53 +/- 0.036
  K_DW:             4.33              -->    4.31 +/- 0.036

ONE FE OUTLIER -- alphaMO = 8.18 +/- 0.77 (an order of magnitude larger than
                  all other alphas). Likely few-obs identification issue.
                  Check obs_obs[state == "MO", .N] in follow-up.
                  alphaSD = 2.27 (also outlier, smaller).
                  alphaME = -1.39 (Maine has more exit/replace than structural
                                   model predicts; tight SE).

WELFARE -- all four CFs reduce social welfare at both E values:
  Scenario                  | dSW (E=$17K)  | dSW (E=$50K)
  --------------------------|---------------|---------------
  A: TX stays FF            |   -0.085      |   -0.059
  B: Pigouvian (internalize)|   -0.002      |   -0.005
  C: K_SW subsidy 50%       |   -0.703      |   -0.376
  D: K tax +50%             |   -0.045      |   -0.125
  (units: $10K per facility)

  Decomposition insights:
   - C: subsidy hurts because firms over-replace; firm surplus loss dominates
        the externality reduction. Suggests optimal subsidy may be ZERO or
        even negative (a TAX on replacement).
   - D: replacement tax INCREASES external damages because firms keep dirty
        tanks longer. Net negative through externality channel.
   - B: nearly zero because (a) E is small relative to L, (b) AvgP_R baseline
        is 0.0024 so there's barely anything to redirect via Pigouvian scaling.
   - A: drops welfare via more exits in TX (AvgP_E rises 0.019 -> 0.030).

═══════════════════════════════════════════════════
RESEARCHER'S FOUR FOLLOW-UP CONCERNS (see in-session notes 1-4 below)
═══════════════════════════════════════════════════
1. CF D (replacement tax) is conceptually mis-named. RB pricing IS the tax.
   Reframe CF D as either "RB premium upcharge on dirty tanks" or remove.

2. CF A (TX stays FF) interpretation is muddled. Result shows MORE exits in
   TX under "stays FF" because |gamma_FF| > |gamma_RB|, but the CF doesn't
   reset premiums -- only swaps coefficients. Need two-part CF: reset
   premiums AND swap regime coefficients.

3. Build subsidy-curve simulation: subsidy fraction in [0, 1] on x-axis,
   producer surplus + external damage + social welfare on y-axis. Find
   the optimal subsidy by scanning, not by point estimate at 50%.

4. Add alpha to retrofit (deferred from Ticket 001). If 25-param + retrofit-
   alpha STILL doesn't pull gamma_price_FF to a sensible magnitude, brainstorm
   intermediate models between 4-param (well-behaved) and 8-param (inflated).

PLUS researcher requests:
5. Welfare CFs A/B/C/D adapted to the 4-parameter model. The 4-param fit had
   reasonable parameter estimates; should be welfare-analyzed too with the
   same Marcus E sensitivity.

═══════════════════════════════════════════════════
WORKFLOW BUGS DISCOVERED (W1-W16) -- needs codification
═══════════════════════════════════════════════════
W1.  PS 5.1 default encoding breaks .ps1 with non-ASCII chars.
     Fix: keep .ps1 ASCII-only OR UTF-8 BOM. ADD CI check.
W2.  Claude Code CLI flag is --append-system-prompt not --system.
     STATUS: FIXED in run_coder.ps1.
W3.  OAuth credentials shadow OpenRouter env-var routing (initial run).
     Routing flipped on /loop wakeup. Fix: claude logout before runner.
W4.  CLAUDE.md logging block uses rstudioapi (fails under Rscript).
     Replace with hardcoded .SCRIPT_BASENAME pattern (already done in 04h/04i).
W5.  Spec referenced code by line range without inlining.
     Architect must inline lines from existing functions; coder
     budget cannot afford hunting.
W6.  Start-Transcript captures host output only, not Claude TUI -- log
     mostly empty. Need: --output-format stream-json with tee, or
     ~/.claude/projects/*/sessions/*.jsonl post-hoc.
W7.  Spec did not declare upstream dependency chain. 04h prereqs
     (04c outputs) absent. Architect precondition checklist:
     for every readRDS/fread in target script, verify file exists
     LOCAL + Z before writing spec; otherwise add producer as step.
     (Added to memory zdrive_data_root.md.)
W8.  "Delete code" instruction when "gate with flag" was correct.
     Default: top-of-file RUN_X <- FALSE, if(RUN_X){...} wraps,
     list-build pattern for combine steps.
W9.  R installation path / renv-aware invocation not declared.
     Add R_HOME, RSCRIPT_EXE, RENV_LIB to CLAUDE.md and memory.
W10. Coder context budget IS the binding constraint when R1 is the
     coder (64K context vs Opus's 200K). Add ENVIRONMENT block to
     every spec; architect pre-resolves all paths/versions/prereqs.
W11. Default coder model choice not informed by current market.
     R1 was arbitrary. Use llm-stats.com 2026 leaderboard:
       Kimi K2.5 ($0.60/$3.00, 262k, Arena 1,462) is the new default;
       DSV4-Flash-Max ($0.14/$0.28, 1M ctx) is cheap-tier;
       GLM-5 ($1.00/$3.20, Arena 1,581) is premium fallback.
W12. OpenRouter 402 on /loop wakeup; max_tokens=64000 reservation
     exceeded balance. Fix: $env:CLAUDE_CODE_MAX_OUTPUT_TOKENS = "8000"
     in run_coder.ps1. Keep OpenRouter balance >= $5.
W13. OAuth-vs-OpenRouter routing non-deterministic across /loop wakeups.
     Pre-launch: `claude logout` to force env-var path, or set
     CLAUDE_CONFIG_DIR to separate empty dir.
W14. Reasoning models burn 3-5x output tokens on hidden thinking.
     HARD RULE: never use a reasoning model in the coder slot.
     Forbidden: R1, Thinking variants, o1/o3/o4, anything with
     "Reasoning" in the name. Permitted: Kimi K2.5 non-thinking,
     DeepSeek V3/V4 non-reasoning, GLM-4.6/GLM-5 non-thinking,
     Qwen3 generalist.
W15. No spend ceiling on coder sessions. Use a separate per-key
     credit limit ($2 max) for the workflow key.
W16. max_tokens reservation compounds cost on reasoning models.
     Always cap at 8000 in env.

═══════════════════════════════════════════════════
NEXT SESSION -- WORKFLOW HARDENING (mandatory before Ticket 002)
═══════════════════════════════════════════════════
*** RESEARCHER DIRECTION (added end-of-session):
    - Tomorrow's coder slot is SONNET (not Kimi K2.5 / R1).
    - Tomorrow uses ANTHROPIC PRO ACCOUNT API (OAuth), NOT OpenRouter,
      NOT any token-buying / capped-key setup. The $12 R1 incident
      will not recur because routing is locked to the Pro account.
    - MODE A below is the chosen path. MODE B is documented for future
      reference only -- do not configure MODE B tomorrow. ***

Before any coder launch:

  1. Create NEW file .claude/run_coder_pro_api.ps1 -- do NOT edit the
     existing run_coder.ps1. The original is preserved for later cleanup
     and as a fallback if Pro routing ever fails.

     The new file is a copy of run_coder.ps1 with these differences:
       - Module header SYNOPSIS / DESCRIPTION say "Anthropic Pro account
         / OAuth routing" instead of "OpenRouter".
       - DELETE the entire "-- Model routing --" block (the five env-var
         sets at lines 35-39 of the original).
       - ADD $env:CLAUDE_CODE_MAX_OUTPUT_TOKENS = "8000" in the same place.
       - Banner text says "ANTHROPIC PRO SESSION" instead of "OPENROUTER
         R1 SESSION".
       - Yellow warning lines say "Sonnet" instead of "R1".
       - Optional Sonnet pin (only if you want to lock the version):
           $env:ANTHROPIC_DEFAULT_SONNET_MODEL = "claude-sonnet-4-5-20250929"
         (verify current Sonnet slug; e.g. claude-sonnet-4-6-* if released;
          remove this line to let Claude Code auto-select.)
       - Verify file is ASCII-only.

     Invocation tomorrow: .\.claude\run_coder_pro_api.ps1 -TicketID 002

     VERIFY before launch: banner at session start should NOT show
     "openrouter" anywhere. If it does, `claude logout` and retry.

  2. NO capped-key setup needed -- Pro account routing handles spend
     via the Anthropic console. Optional: set a usage alert in
     console.anthropic.com if you want a soft cap notification.

  3. (Documented for future, NOT for tomorrow) MODE B fallback if Pro
     account ever rate-limits or you want a third-party model:
         OpenRouter -> Sonnet via anthropic/claude-sonnet-4.5 slug
         with $2-capped OpenRouter key. See git history of this file
         for the configuration block.

  3. Update CLAUDE.md with:
     - "RUNTIME ENVIRONMENT" section: RSCRIPT_EXE = C:\Program Files\R\R-4.5.2\bin\Rscript.exe
       RENV_LIB = renv/library/windows/R-4.5/x86_64-w64-mingw32
       Invocation: & $RSCRIPT_EXE --no-save --no-restore <script.R>
     - "MODEL SELECTION" subsection: Kimi K2.5 default, DSV4-Flash-Max
       cheap, GLM-5 premium, forbidden = reasoning models.
     - DO NOT RUN list -> ASK BEFORE RUN list (researcher direction)

  4. Update .claude/agents/architect.md:
     - PSEUDOCODE QUALITY STANDARD: every spec opens with ENVIRONMENT
       block (Rscript path, renv lib, verified prereq manifest, lines
       from existing functions inlined verbatim).
     - HARD RULE: coder model must be non-reasoning.
     - Cost estimate at top of spec: token budget + dollar @ default model.

  5. Add memory file r_runtime_environment.md (sister to zdrive_data_root.md)
     pinning R-4.5.2 path and Rscript invocation pattern.

═══════════════════════════════════════════════════
NEXT SESSION -- TICKETS (in priority order)
═══════════════════════════════════════════════════
T002 (cheap, fast) -- Sample diagnostic for MO/SD outlier alphas.
     No estimation. Check obs counts per state. Decide whether to merge,
     drop, or footnote. Re-fit 8p+FE if state list changes.
     Estimated cost: ~$0.10 on Kimi K2.5.

T003 (cheap, fast) -- Add alpha to retrofit utility (deferred from T001).
     Maintain-only FE absorbed ~23% of gamma_price_FF inflation; if
     across-state replace-rate variation explains the rest, retrofit FE
     should pull gamma further toward sensible range.
     Spec: new estimator npl_estimator_replacement_8p_fe_maint_retrofit,
     42 params (8 structural + 17 maintain alphas + 17 retrofit alphas).
     Identification: PR/PE ratio constancy across g STILL imposed.
     Estimated cost: ~$0.30 on Kimi K2.5 + ~5 min estimation.

T004 (medium) -- Subsidy-curve simulation. K_SW subsidy fraction
     scan in [0, 1] (11 points). Plot producer surplus, external
     damage PV, social welfare vs subsidy fraction. Identify optimum.
     Computationally cheap (eq solves are fast once cache built).
     Estimated cost: ~$0.20 on Kimi K2.5.

T005 (medium) -- Reframe / replace CF D. Researcher direction: replacement
     tax is mis-stated; RB pricing IS the tax. Either:
       (a) drop CF D entirely
       (b) replace with "RB premium upcharge on dirty tanks" CF
       (c) keep K-tax but rename and note it's a stress test, not a policy
     Architect call after researcher specifies which.

T006 (medium) -- Two-part CF A. Currently CF A swaps gammas but not premiums.
     Reset TX premiums to controls-comparable FF schedule AND swap gammas.
     Need: a "what FF premium would TX have faced" series. Likely already
     in obs_obs columns or derivable from Mid-Continent rate filings on Z.

T007 (REVISED -- incorporates Notes 1-3 + CF M; CF S dropped) --
     4-parameter welfare code. New script 04j_4p_Welfare.R mirroring 04i
     but using the 4-parameter fit (Model_Replacement_Estimates_observed.rds).
     Same Marcus E sensitivity. CFs:
       - CF A:       DROP (no FF/RB distinction in 4p)
       - CF C:       K subsidy 50% -- KEPT
       - CF C_scan:  K subsidy curve scan over s in [-0.5, 1.0], 16 points,
                     PER NOTE 3. Produces 4-curve plot (producer surplus,
                     external PV, govt outlay, social welfare) vs subsidy.
       - CF P:       TRUE Pigouvian via additive premium surcharge h(s)*E
                     (REPLACES old gamma_risk scaling). Cleaner economic
                     mapping; available because premium enters utility
                     linearly through gamma_price.
       - CF M:       MANDATED tank closure at age 25 (NEW -- added by
                     researcher end-of-prior-session). At/after age threshold
                     (A_bin >= 6, age 25+), P_maintain = 0 is FORCED;
                     firms still choose exit-vs-replace via 2-action softmax
                     on v_E and v_R. Equilibrium re-solved with constraint
                     applied in Bellman inversion at every iteration.
                     Computed at both E values. Architect spec MUST inline
                     the modified Bellman pseudocode -- coder cannot infer it.
       - CF S:       DROPPED (per researcher direction; CF M is the stronger
                     regulatory-mandate lever with cleaner precedent).
     Side-by-side comparison: Output/Tables/04j_Welfare_4p_vs_8pFE_Compare.csv
       For each CF/E_label pair, both 4p ΔSW and 8p+FE ΔSW. If directionally
       agreeing on every row, welfare conclusion is robust to model spec.
       Critical paper artifact.
     Deliverables: 04j_4p_Welfare.R, Model_Welfare_4p_observed.rds,
                   04j_Welfare_4p_Social.csv, 04j_Welfare_4p_Decomp.csv,
                   04j_Welfare_4p_vs_8pFE_Compare.csv,
                   04j_4p_Welfare_BarChart.png, 04j_4p_Subsidy_Curve.png,
                   04j_4p_PremiumSurcharge.png.
     Estimated cost on Sonnet 4.6: $0.35-0.55. Compute: ~5 min.

T008 (large) -- If T003 retrofit-alpha still leaves gamma_price_FF inflated,
     brainstorm intermediate models between 4p and 8p:
       - 5p (4p + separate K for SW/DW)
       - 6p (5p + separate kappa for SW/DW)
       - 7p (6p + single gamma_price interacted with FF/RB)
     The 4p had reasonable estimates; the 8p inflates. The intermediate
     specs identify which extension breaks identification.

T009 (deferred from T001) -- Welfare QSD social-aggregation upgrade.
     Current CSV uses n_cell weights and chain-decomposition QSD.
     The proper version needs full chain-based QSD iteration at the
     COUNTERFACTUAL equilibrium, not just baseline. Re-check whether
     this is what 04i actually does (compute_weighted_qsd_replacement
     is called per scenario already, so this may already be correct).

═══════════════════════════════════════════════════
RESUME PROMPT
═══════════════════════════════════════════════════
"Load CLAUDE.md and .claude/HANDOFF.md. Ticket 001 completed --
 25-param FE estimation produced, Marcus E sensitivity welfare done.

 TOMORROW'S CODER IS SONNET via ANTHROPIC PRO ACCOUNT API (OAuth).
 NOT OpenRouter. NOT any token-buying / capped-key setup. The $12 R1
 incident will not recur because routing is locked to Pro account.
 Apply MODE A from WORKFLOW HARDENING block (Anthropic-direct only;
 MODE B is documented but NOT for tomorrow).

 Before any coder launch:
   - apply max_tokens cap ($env:CLAUDE_CODE_MAX_OUTPUT_TOKENS = '8000')
   - apply CLAUDE.md RUNTIME ENVIRONMENT block update
   - apply architect.md ENVIRONMENT-block requirement
   - DO NOT RUN list -> ASK BEFORE RUN list (per researcher direction)

 Priority for next session:
   T007 first (4p welfare, paper-critical: robustness check) --
   architect writes spec incorporating Notes 1-3 CFs (CF C kept,
   C_scan curve, CF P true Pigouvian via premium surcharge, CF S
   risk-targeted surcharge analog to 'RB pricing IS the tax').
   Then T002 (MO outlier diagnostic) and T003 (retrofit alpha)."

═══════════════════════════════════════════════════
KEY DECISIONS THIS SESSION
═══════════════════════════════════════════════════
- Maintain-only FE confirmed as Semantic 2 (nuisance, not structural).
  Equilibrium and counterfactuals use structural-only theta;
  FE absorbs measurement-equation residual variation.
- TX FE fixed at 0; alphas estimated for 17 control states (alphaAR..alphaVA).
- AM-2002 "profile-likelihood" SE = inverse-Hessian of NPL pseudo-likelihood
  at converged theta, equilibrium re-solved on structural sub-vector only.
  Equilibrium cache keyed on structural theta -- alpha perturbations skip
  re-solve. Same pattern as 04f/04h.
- Marcus 2021 E calibration: HEALTH_ONLY = $17K (Section VI, $48.98M annual
  infant-health damages / 2,893 leaks/yr). HEALTH_PLUS_UNMEASURED = $50K
  (approx 3x for unmeasured channels: adult cancer, ecological, long-run
  cognitive). NEITHER includes the $169.5K/leak cleanup cost (paid by state
  LUST trust funds, would double-count if L_vec includes liability).
- Dead AR-base FE block in improved_estimator (lines 3399-3623) deleted.
- 04h, 04i, 04c, smoke test all confirmed to log via hardcoded
  .SCRIPT_BASENAME (CLAUDE.md logging block updated; rstudioapi dropped).
- Workflow uses Sonnet 4.6 / Kimi K2.5 to play "R1 slot" -- the actual
  model behind the protocol can rotate, the protocol stays.
- DO NOT RUN list -> ASK BEFORE RUN list direction noted.

═══════════════════════════════════════════════════
SKILL AUDIT
═══════════════════════════════════════════════════
None built this session. Skills/ directory still empty.
Candidates for skill extraction after T002-T003 land:
  - architect_environment_block_template (the pre-resolved env preamble)
  - architect_dependency_chain_checker (verify-or-add-prereq-step rule)
  - coder_runner_guardrails (max_tokens cap + capped key + non-reasoning)
