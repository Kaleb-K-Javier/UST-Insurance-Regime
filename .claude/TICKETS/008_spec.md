# TICKET 008 — Research-team walkthrough qmd: 6-parameter NPL model + TX-FF counterfactual
# Created: 2026-05-22
# Status: AWAITING_IMPLEMENTATION
# Attempt: 0

═══════════════════════════════════════════════════
MOTIVATION
═══════════════════════════════════════════════════
Create a single self-contained Quarto document that walks the research
team through what the 6-parameter dynamic-discrete-choice (DCC) replacement
model is actually doing in code. Audience: co-authors / committee members
who want to understand what code runs without re-deriving it from the
estimator source files. They should be able to read the document
cover-to-cover and come away with:

  - The math (flow utilities, value function, choice probabilities)
  - The actual R/C++ code that implements each piece, with the
    nice-to-have helpers stripped out so only the structural meat
    remains
  - The fitted parameter table with $-unit conversions
  - In-sample fit diagnostics
  - The Texas-on-flat-fee counterfactual results (action shares, removal
    timing distribution, welfare numbers)

This is presentation work (polish-direct in spirit). NO estimation runs.
The qmd consumes T007 Phase 3 + Phase 4 outputs and reformats them with
annotated code chunks for human reading.

═══════════════════════════════════════════════════
SCOPE
═══════════════════════════════════════════════════

Inputs (read-only — must exist from T007 PASS):
  Output/Estimation_Results/Model_Replacement_6paramFE_profile_clean_observed.rds
  Output/Estimation_Results/CF_TX_FlatFee_results.rds
  Output/Tables/04o_CF_TX_FF_premiums.csv
  Output/Tables/04o_CF_RemovalAge_Distribution.csv
  Output/Tables/04o_CF_ActionShares_byAge.csv
  Output/Tables/04o_CF_Welfare_Summary.csv
  Output/Figures/04o_CF_ActionShares_byAge_TX.png
  Output/Figures/04o_CF_RemovalAge_Distribution_TX.png

Code source for the annotated chunks (read-only — extract relevant
functions verbatim, then annotate inline):
  Code/Helpers/improved_estimator_OPTIMIZED.r
  Code/Helpers/cpp_engine.cpp

Output:
  Reports/Paper/Model_Code_Walkthrough.qmd  (renders to PDF)

If T007 hasn't finished by the time this ticket starts, escalate
back to the architect. The qmd is built FROM T007's outputs.

═══════════════════════════════════════════════════
DOCUMENT STRUCTURE
═══════════════════════════════════════════════════

YAML preamble:
  Same convention as Dynamic_Model_Fit_Report.qmd and
  Identifying_Variation_Size_Capacity.qmd in this project:
    format: pdf, documentclass article, geometry margin=1in,
    fontsize 11pt, linestretch 1.15, toc TRUE, number-sections TRUE,
    keep-tex TRUE.
  Title: "Walkthrough: 6-parameter NPL Replacement Model and TX→FF Counterfactual"
  Subtitle: "Code, estimates, fit, and welfare results for research-team review"

Section 1 — Setup and model overview (1 page)
  Brief prose:
    - 3 actions {Maintain, Exit, Replace}, T1EV(0) shocks
    - 32 state cells (8 age bins × 2 wall × 2 regime)
    - β = 0.95, σ₂ = 1.0
    - Sample: TX 2006+ and controls 1999+, N = 2.28M facility-years
    - Action definitions used (post-T007 clean version):
        Replace = closure with future install at facility
        Exit    = full-closure facility with no future install
        Maintain = anything else
    - One-paragraph intro to NPL (Aguirregabiria-Mira 2002) — what the
      outer loop is doing and why it's used instead of Rust NFXP
    - Pointer to NPL_REFERENCE.md and Teaching_Notes_Dynamic_Models.md
      for deeper math

Section 2 — Flow utility specification (~1.5 pages)
  Math block (Eq. 1, 2, 3) showing the universal-γ 6-parameter
  utilities at state s for action j and group g:
    u_M(s, g; θ, α_g) = 1 + γ_price · P(s) − γ_risk · h(s) + α_g
    u_E(s; θ)         = κ_{w(s)}
    u_R(s, g; θ, α_g) = −K_{w(s)} + α_g    (stayFE on M and R)

  Then a single code chunk (`{r, eval=FALSE}`) showing the actual
  R implementation, extracted verbatim from improved_estimator_OPTIMIZED.r
  flow_utilities_replacement_6p (approx 20 lines). Add inline comments
  marking the four key sub-steps:
    # (1) Pull required parameters from theta and assert presence
    # (2) Build wall-specific kappa and K vectors (32-vec per state cell)
    # (3) Build universal gamma_price and gamma_risk SCALARS — no regime
    #     splits; these are preference primitives
    # (4) Assemble the 32x3 utility matrix; order matters
  Caption the code block "flow_utilities_replacement_6p — from
  improved_estimator_OPTIMIZED.r line ~XXXX" with the actual line number.

Section 3 — Bellman inversion and choice probabilities (~1.5 pages)
  Math: Eq 2.3.25 from NPL_REFERENCE.md — linear V inversion using
    V = (I − β M)^{−1} R
  with M and R defined per the textbook. Note that in this codebase,
  exit is absorbing so F_exit doesn't enter M (only F_M and F_R).

  Two code chunks (eval=FALSE), both annotated:

  (a) invert_value_function_replacement (from improved_estimator_OPTIMIZED.r,
      around line 2645). Show the ~20-line core:
        # (1) Floor CCPs to avoid log(0); use the per-cell P_M, P_E, P_R
        # (2) Build the R[s] reward vector (period payoff including
        #     sigma·(gamma_E − log P_j) bonus term)
        # (3) Build the M Markov operator from non-absorbing actions
        #     (Maintain and Replace only; Exit is absorbing)
        # (4) Solve V via sparse linear system

  (b) .compute_v_indices_6p (a 6-line wrapper that uses the 6p flow
      utility plus a single Bellman call). Show as:
        # (1) Build U from theta_struct
        # (2) Invert V given current P^k
        # (3) Compute choice-specific v_M, v_E, v_R for softmax
      Note v_E has no continuation term (Exit absorbing).

  Footnote: alpha (state FE) does NOT enter V; it is added at the
  measurement-equation step in Section 4. This is the Semantic-2
  convention per NPL_REFERENCE.md §4.

Section 4 — Profile-α Newton solver (~1.5 pages)
  Math: Eq. 3 from Teaching_Notes_Dynamic_Models.md §1.3 (the FOC for α_g
  given θ, P^k).

  One code chunk (eval=FALSE) showing the R version of the Newton solver
  for a single group g — extract from `.solve_alpha_g_foc` in
  improved_estimator_OPTIMIZED.r (this was added in T004 / T005, find
  the line range and show ~30-40 lines). Annotate the four key steps:
    # (1) Guard against zero-count groups (return ±bound with warning)
    # (2) Newton iterate: at each guess, compute P_stay = P_M + P_R
    #     under the current alpha
    # (3) Residual = sum_s n_stay - sum_s n_total · P_stay; derivative
    #     uses the logit-derivative identity (P_stay · P_E)
    # (4) Clamp to bound, stop on residual < tol

  Optional: also show the C++ version (4-5 line snippet from
  cpp_engine.cpp inside nll_replacement8pfe_profile_counts_cpp, the
  Newton loop). Cap at ~15 lines to keep the document readable.
  Frame as "for production speed the same logic is in C++". If C++
  is too noisy / long, skip it and reference the file.

Section 5 — NPL outer loop driver (~1 page)
  Math: NPL_REFERENCE.md §2 — the alternating θ-step and P-update,
  with convergence criterion on both.

  Code chunk (eval=FALSE) showing the NPL outer-loop body from
  npl_estimator_replacement_6p_fe_profile in improved_estimator_OPTIMIZED.r.
  Strip out the Hessian / SE machinery — just show the iteration core
  (~30 lines):
    # (1) Initialize P with structural-equilibrium policy or uniform
    # (2) Outer loop:
    #     (a) Inner optim over 8 structural θ via L-BFGS-B
    #         with bounds and the profile-likelihood objective
    #     (b) CCP update: recompute v's at new theta, solve alpha,
    #         form softmax, marginalize by g
    #     (c) Convergence check on both ||θ^k+1 - θ^k|| and ||P^k+1 - P^k||

  Note that the profile-α solver from Section 4 fires INSIDE the inner
  optim's likelihood evaluation — at every theta candidate L-BFGS-B
  tries.

Section 6 — Fitted parameter estimates (~1 page)
  Pull from Output/Estimation_Results/Model_Replacement_6paramFE_profile_clean_observed.rds.

  One table (rendered via readLines + cat of the existing .tex if
  available, or built fresh via kable) showing:
    rows: kappa_SW, kappa_DW, K_SW, K_DW, gamma_price, gamma_risk,
          log L, N_obs
    cols: estimate (in $ where applicable), SE_raw,
          95 percent CI lower, 95 percent CI upper
  Convert kappa and K to $ via × 10,000 (SCALE_FACTOR).

  Add a 3-sentence prose interpretation per parameter:
    - kappa interpretation in dollars
    - K interpretation in dollars
    - gamma_price sign and magnitude (firms care more about each dollar
      of premium than each dollar of revenue — risk aversion signature)
    - gamma_risk magnitude (firms internalize ~X percent of expected leak
      cost — externalization signature)

  Below the structural-param table, embed the FE alphas table from T007
  (whatever path it lives at). Quick paragraph noting that alphas are
  measurement-side intercepts and do NOT enter CF re-solve.

Section 7 — Goodness of fit (~1.5 pages)
  Pull the per-cell P_hat from the fit object and compare to empirical
  cell shares (computed from the cleaned obs panel).

  Three figures (already produced by T007 if it ran the fit artifact
  script; otherwise reproduce here):
    - SW Fit figure (action shares model-vs-empirical by age bin)
    - DW Fit figure (same)
    - Overall RMSE summary (kable of per-cell residual RMSE by
      wall × regime × action)

  Two-paragraph narrative noting what the model fits well and what it
  doesn't (e.g., DW Replace at older ages may show under-fit; cite the
  most visible cell-level discrepancies).

Section 8 — Texas counterfactual: never had RB pricing (~2 pages)
  This is the headline counterfactual deliverable.

  Brief prose intro (3-4 sentences):
    "We re-solve the agent's equilibrium policy at the structural
    theta-hat, holding the CF where TX's RB premium schedule is
    replaced by the FF-equivalent schedule (median FF premium from
    control states, 2006+, matched by age × wall). Per Semantic 2,
    the FE alphas are stripped from the CF re-solve — only the 6
    structural parameters enter the CF agent's choice problem."

  Subsection 8.1 — CF premium swap
    Show 04o_CF_TX_FF_premiums.csv as a kable. Reader can see at a glance
    how big the premium reduction is per (A_bin, wall) cell.

  Subsection 8.2 — Action-share trajectories
    Show 04o_CF_ActionShares_byAge_TX.png inline. Brief caption:
    "Solid lines: baseline (TX RB). Dashed lines: counterfactual (TX FF).
     Three facets show maintain/exit/replace shares by age bin and wall."

    One-paragraph interpretation: where do the shares move most? Is
    Replace rate lower under FF (firms less pressured to retrofit)?
    Is Exit rate higher (firms walking away under softer pricing)?

  Subsection 8.3 — Removal-age distribution
    Show 04o_CF_RemovalAge_Distribution_TX.png inline. Brief caption:
    "Density (or histogram) of tank-age at retrofit or exit under
     baseline vs CF, by wall and initial regime cohort."

    One-paragraph interpretation: do tanks live longer under FF? Are
    retrofits delayed? Is the avg removal age higher or lower under CF?

  Subsection 8.4 — Welfare summary
    Show 04o_CF_Welfare_Summary.csv as a kable. Rows: Producer Surplus,
    External Damage, Govt Outlay (=0 in this CF), Social Welfare.
    Cols: baseline_USD, cf_USD, delta_USD.

    Brief 3-sentence interpretation:
      - Producer surplus change (firms gain or lose?)
      - External damage change (society's leak losses go up or down?)
      - Net social welfare verdict

Section 9 — Caveats and next steps (~1 page)
  Brief bullet list:
    - Partial closure (portfolio shrinking) is currently Maintain in
      the model; in the data it's a regime-responsive margin at 4+ tank
      facilities. CF welfare numbers ignore this margin; see
      Identifying_Variation_Size_Capacity.qmd §4.8.
    - SD and MO state fixed effects bumped the boundary in earlier fits;
      cleaned panel may have resolved this — check (Section 6 alphas).
    - The CF holds primitives (h, F transitions) fixed; only the premium
      schedule changes. Welfare numbers do not include behavioral
      feedbacks on hazard.
    - This CF is one of six planned. The remaining five (controls→RB,
      Marcus full-cost, mandate, subsidy, γ_risk=1) reuse the same
      pipeline.
    - Pointer to the canonical estimation script:
      Code/Dynamic_Model/04o_6paramFE_Profile_Clean.R
    - Pointer to the canonical CF script:
      Code/Dynamic_Model/04o_CF_TX_FlatFee.R

═══════════════════════════════════════════════════
R-IMPLEMENTATION NOTES
═══════════════════════════════════════════════════

- All R code chunks for the SHOW-THE-CODE sections should use
  `{r, eval=FALSE, echo=TRUE}` — render the code as text but don't
  re-execute. Add brief inline comments per the annotation guidance
  in each section above.

- All R code chunks for the COMPUTE-AND-DISPLAY sections (tables,
  embedded figures) should use `{r, echo=FALSE, warning=FALSE,
  message=FALSE}` so the chunk body doesn't appear, only output.

- Image inclusions via knitr::include_graphics(here(...)).

- Tables via kable(..., format="latex", booktabs=TRUE) + kable_styling
  (font_size=9-10, scale_down only if needed). Avoid `escape=FALSE`
  unless absolutely needed (per the prior lessons — special characters
  in cells cause LaTeX parse errors).

- Code extraction: use readLines() on the source file and trim to the
  function body, then wrap in ```r ... ``` for verbatim display.
  Add a `## ANNOTATION: ...` comment marker on each key sub-step
  (the 4-step annotations called out in each section spec above).
  Do NOT modify the source file's logic; extract verbatim and add
  comments only.

- Skip the following "nice-to-have helpers" entirely (their existence
  can be noted in prose but their code is not shown):
    - Logging helpers / file IO wrappers
    - Bounds construction helpers
    - SE / Hessian inversion machinery
    - Convergence-trace printing code
    - The full counts-and-weights cache builder
      (.build_counts_weights_8p_fe) — too long; describe in 1 sentence

- Document length target: 12-15 pages of PDF. Aim for clarity, not
  completeness. If a section feels long, cut prose and let the code
  speak.

═══════════════════════════════════════════════════
DELIVERABLES
═══════════════════════════════════════════════════

(1) Reports/Paper/Model_Code_Walkthrough.qmd  (NEW, the main deliverable)
    9 sections per the structure above. Total of approximately:
      - 5-7 annotated R code chunks (eval=FALSE)
      - 1-2 C++ code snippets (optional, eval=FALSE)
      - 3-5 tables (parameter estimates, FE alphas, fit RMSE, CF premium swap,
                    CF welfare summary)
      - 2-3 figures (SW fit, DW fit, CF action shares, CF removal-age dist)

═══════════════════════════════════════════════════
ACCEPTANCE CRITERIA
═══════════════════════════════════════════════════

CONTENT:
- [ ] All 9 sections present in the qmd
- [ ] Math equations in sections 2-5 use display math syntax ($$...$$)
      and match the equations in Teaching_Notes_Dynamic_Models.md and
      NPL_REFERENCE.md (don't make up new notation)
- [ ] Section 2 contains the flow_utilities_replacement_6p code chunk
      with the four-step annotation comments
- [ ] Section 3 contains BOTH invert_value_function_replacement AND
      .compute_v_indices_6p code chunks, each with annotations
- [ ] Section 4 contains the .solve_alpha_g_foc code chunk with the
      four-step annotation
- [ ] Section 5 contains the NPL outer-loop body with annotations
- [ ] Section 6 has the parameter table with $-unit conversions
- [ ] Section 7 has the SW and DW fit figures embedded
- [ ] Section 8 has all four subsections (CF premiums, action shares,
      removal-age dist, welfare summary)
- [ ] Section 9 has bullet-list caveats with the 4-5 points specified

CODE EXTRACTION FIDELITY:
- [ ] Every R code chunk shown matches the corresponding lines in
      improved_estimator_OPTIMIZED.r verbatim (only added: inline
      comments and any indentation tweaks for readability)
- [ ] Line numbers cited in code-chunk captions match actual source
      file line numbers as of the current commit
- [ ] No silent modifications to the source code logic in the
      displayed chunks

QMD HYGIENE:
- [ ] All code chunks have eval=FALSE for SHOW-THE-CODE sections
- [ ] All data-display chunks have echo=FALSE
- [ ] No `escape = FALSE` on any kable call (unless absolutely needed,
      and if so, no special characters in cell contents or column names)
- [ ] Special characters in headers/cells use plain English words:
      % → pct, $ → USD in column labels (per the prior cleanup pattern)

RENDER:
- [ ] qmd renders to PDF successfully via
      `quarto render Reports/Paper/Model_Code_Walkthrough.qmd`
- [ ] PDF is between 10 and 18 pages
- [ ] All embedded figures and tables appear in the rendered PDF (none
      missing or with red ?? placeholders)

═══════════════════════════════════════════════════
ATTEMPT LOG
═══════════════════════════════════════════════════
[Filled by reviewer after each attempt. Leave blank until first attempt.]
