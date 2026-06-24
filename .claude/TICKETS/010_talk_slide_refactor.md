# TICKET 010 — JMP talk slide refactor (TALK_EDIT.qmd, draft pass 1)
# Created: 2026-05-28
# Status: AWAITING_IMPLEMENTATION
# Attempt: 0
# Assignee: Sonnet (presentation/markdown edits only — no R, no estimation)

═══════════════════════════════════════════════════
MOTIVATION
═══════════════════════════════════════════════════
Restructure the existing Beamer deck into the new JMP talk outline.
The current deck has a stale 2-action "toy model" theory block, 4-parameter
structural results, and only 3 counterfactuals. The new outline:

  - Replaces the lit-review slide with the new 4-block Related Literature
    slide and the new 7-slide Part 1 buy-in section
  - Restructures Part 2 (Data & Descriptive) around the new outline's job
    descriptions
  - Tightens Part 3 (Reduced Form) into 5 slides plus a text-only
    "bridge to structural" slide
  - Replaces the 4-parameter structural section with the 6-parameter +
    state-FE (alpha_g) specification used in Model_Code_Walkthrough.qmd
    and 04o_CF_TX_FlatFee.R
  - Lays out 4 counterfactuals (CF1 = TX-on-flat-fee, CF2 = subsidy,
    CF3 = Pigouvian, CF4 = age mandate)
  - Demotes the toy-model block, FR Compliance, and Market Structure HHI
    slides to appendix

This is a DRAFT pass. Talk length target: ~70 minutes. No hard slide cap.
The governing principle is "no slide does more than two things." Where the
current content tries to do three or four things on one slide, split it.

This is presentation work only. NO R execution. NO estimation runs.
NO edits to anything in Code/ or Output/. Sonnet edits markdown only.

═══════════════════════════════════════════════════
FILES
═══════════════════════════════════════════════════
EDIT THIS FILE:
  Reports/Slides/04_Risk_Based_Pricing_and_USTs_TALK_EDIT.qmd

DO NOT EDIT:
  Reports/Slides/04_Risk_Based_Pricing_and_USTs_REFACTORED.qmd   (canonical reference)

READ-ONLY REFERENCE (DO NOT CITE INSIDE SLIDES — just for context):
  Reports/Paper/Model_Code_Walkthrough.qmd
  Reports/Paper/Identifying_Variation_Size_Capacity.qmd
  Code/Dynamic_Model/04o_CF_TX_FlatFee.R
  CLAUDE.md  (unit conventions: 1 model unit = $10K/yr, SCALE_FACTOR)

DO NOT RENDER. Author will compile manually in RStudio.

═══════════════════════════════════════════════════
GLOBAL CONVENTIONS
═══════════════════════════════════════════════════

(1) Beamer formatting — preserve from current REFACTORED deck:
    - YAML header unchanged (title, author, format: beamer, template,
      aspectratio: 169, pdf-engine: xelatex, theme-bg-color)
    - Every content slide opens with `\BerkeleyLightMode`
    - Every section-divider slide uses `\BerkeleyMode` and the
      `::: {.layout-statement} ... :::` block
    - `\vfill`, `\vspace{...}`, `\centering`, `\hfill` usage matches the
      surrounding style in REFACTORED
    - All `\navbutton[...]{...}` tikz overlay blocks for appendix
      cross-references must be preserved verbatim where the linked
      content survives (see APPENDIX section below for the active set)

(2) Equation conventions — MUST match the actual estimator code in
    Code/Helpers/improved_estimator_OPTIMIZED.r and the math in
    Model_Code_Walkthrough.qmd §2. The signs and where alpha enters
    are load-bearing:

    Flow utilities (6-parameter + state FE specification):

      u^M(s, g) = 1 + gamma_price * P(s)  -  gamma_risk * h(s)  + alpha_g
      u^E(s)    = kappa_{w(s)}
      u^R(s, g) = -K_{w(s)}                                      + alpha_g

    Where:
      - gamma_price is the estimated parameter and is NEGATIVE
        (point estimate ~ -1.11 in the current fit)
      - gamma_risk is the estimated parameter and is POSITIVE
        (point estimate ~ +0.07)
      - The displayed equation uses "+ gamma_price * P" (NOT "- gamma_p * P");
        report the parameter value as the negative number it actually is
      - alpha_g enters BOTH Maintain and Replace (Semantic-2 convention),
        NOT Exit. alpha_g is "profiled out" — it appears in the likelihood
        only, NOT in the counterfactual re-solve. Make this explicit on
        the relevant slides.

    6 structural parameters:  kappa_SW, kappa_DW, K_SW, K_DW,
                              gamma_price, gamma_risk
    18 state fixed effects:   alpha_g, one per (state, regime) cell,
                              profiled out at each likelihood evaluation.
                              Drop alpha_TX = 0 normalization may apply
                              — check Model_Code_Walkthrough.qmd if
                              needed; this is a passing mention, not a
                              focus.

    State space: 32 cells = 8 age bins x 2 wall x 2 regime
    Discount factor: beta = 0.95
    T1EV scale: sigma_2 = 1.0 (normalized)

(3) Unit convention (verbatim from CLAUDE.md):
    - 1 model unit = annual net revenue R = $10,000 / yr / facility
    - All theta entries (kappa, K) are in this $10K scale
    - When converting to dollars on slides, multiply by 10,000

(4) Health damage placeholder:
    - Use E = $17,000 per release (Marcus 2021) throughout the deck.
    - Note: the only CF that is actually computed in 6p code right now
      (04o TX-FF) uses E = $50,000. ALL welfare numbers on slides are
      PLACEHOLDERS pending re-run at E = $17K. Wherever a specific
      welfare number would appear, write "[PLACEHOLDER: welfare value
      at E = $17K pending 6p re-run]" — do NOT scale the $50K results
      yourself.

(5) Placeholder convention:
    - For any specific numeric claim about counterfactuals 2, 3, or 4 in
      the 6p model: use "[PLACEHOLDER: ...]" with a short description of
      what number is missing.
    - For figures that haven't been regenerated for the 6p spec, include
      the existing 4p figure path and ADD a comment in the qmd source:
      `<!-- TODO: replace with 6p figure when available -->`
    - Where a table or figure is genuinely missing, write
      `[PLACEHOLDER: figure description]` inside a center block.

(6) Cross-references (navbuttons): for every appendix slide that survives,
    preserve the `\hypertarget{...}` and matching `\navbutton[...]{...}`
    overlay so the in-talk forward/back navigation still works. New
    navbuttons for appendix items added by this ticket are listed in the
    APPENDIX section below.

═══════════════════════════════════════════════════
SLIDE-BY-SLIDE SPEC
═══════════════════════════════════════════════════

Numbering below follows the new outline (S1..S30 main + appendix).
The current REFACTORED deck's slides are referenced as "old S<n>" by
their order in that file.

───────────────────────────────────────────────────
PART 1 — BUY-IN (S1-S7)
───────────────────────────────────────────────────
DELETE the current S1-S5 ("Motivation", "Setting", "Contribution",
"Empirical Strategy", "Main Findings") and REPLACE with the 7 slides
below. The user has approved S1-S7 content as drafted. Use the body text
exactly as written (light Beamer reformatting only).

────────
S1 — Strict liability disciplines hazardous industries — but breaks down
     when firms cannot be held fully accountable
────────
BODY (bulleted, plain prose):

- Strict liability is the primary legal instrument for environmental
  harm: the firm pays for what it causes, regardless of fault — the
  backbone of environmental governance across oil and gas (OPA 1990,
  33 U.S.C. § 2701), surface coal mining (SMCRA 1977, 30 U.S.C. § 1201),
  and hazardous substance releases across chemicals, petroleum storage,
  and manufacturing (CERCLA 1980, 42 U.S.C. § 9601; RCRA 1976)
- Works when firms can be held fully accountable — breaks down when they
  cannot (Shavell 1986, 2007)
- Shavell (2004): two instruments to restore accountability under strict
  liability:
    - Asset / bonding requirements
    - Compulsory liability insurance — where the premium itself does the
      deterrence work, *if* the insurer can contract on risk-relevant
      attributes
- Bold one-line closer: "This paper examines the design and efficacy of
  compulsory liability insurance as a financial responsibility instrument
  in environmental hazard settings."

────────
S2 — This paper
────────
- Underground storage tanks (USTs) — primary infrastructure for retail
  petroleum storage in the US
- ~550,000 active tanks at peak; ~140,000 facilities; the most common
  source of groundwater contamination in the US
- Unit of observation: facility-year; 9.8M tank-years spanning both
  insurance regimes
- Compulsory liability insurance is already the law (RCRA Subtitle I,
  1988 — $1M coverage floor)
- Insurance design varies across states: flat-fee public funds vs.
  risk-rated private markets

Subhead: "Three questions:"

- Q1 — Observability: Are observable firm and tank characteristics
  predictive of release risk and cleanup costs?
- Q2 — Behavioral response: How do firms respond under risk-based pricing
  relative to a flat-fee regime — on closure, replacement, and confirmed
  release outcomes?
- Q3 — Welfare: How much of the first-best welfare gain does risk-rated
  pricing capture, and what drives the residual gap?

────────
S3 — The firms: small, independent operators under a federal insurance
     mandate since 1988
────────
- Underground storage tanks store petroleum at gas stations, truck stops,
  fleet facilities, and depots
- ~550,000 active tanks at peak; ~140,000 regulated facilities across all
  50 states
- Predominantly single-walled steel, installed 1960s–1980s during the
  interstate highway build-out
- Small, single-location independent operators — the dominant
  organizational form in retail petroleum distribution
- Federal financial responsibility mandate in place since 1988 — a mature
  compulsory insurance market
- Strict liability applies to confirmed releases under CERCLA and RCRA

────────
S4 — The risk: corrosion is gradual, detection is imperfect, and private
     tort law cannot reach this harm
────────
- Single-walled steel corrodes from outside in — no secondary containment;
  failure is gradual not catastrophic
- Petroleum and benzene migrate silently through soil into groundwater —
  no visible event, no immediate victim
- By detection, the contamination plume may have traveled hundreds of feet
- Key risk drivers: tank age, wall construction, piping type, leak-detection
  technology
- Private tort law largely fails: diffuse victims, delayed injury, causation
  hard to establish
- The regulatory liability system is the operative policy instrument —
  there is no private law substitute

────────
S5 — The stakes: large skewed cleanup costs and a health externality
     outside the liability system's reach
────────
- ~550,000 confirmed releases in the EPA LUST database since 1988
- Cleanup costs per confirmed release: median $136k, mean $403k — right
  tail dominates expected loss
- Strict liability exposure: up to $1M per occurrence (RCRA); Superfund
  liability (CERCLA) for severe cases
- Benzene: IARC Group 1 carcinogen. MTBE: widespread groundwater
  contaminant
- Health damages: ~$17k per release (Marcus 2021) — generate no
  third-party insurance claim
    - Contamination unobservable, victims diffuse, causation hard to
      establish
    - Even a perfectly risk-rated premium cannot price what no claimant
      recovers

Bold payoff line at bottom: "Cleanup costs are large enough that
insurance design materially affects firm incentives. Health damages are
real but structurally unpriced — this is the source of the first-best
gap the welfare decomposition quantifies."

────────
S6 — The policy variation: flat-fee public funds vs. risk-rated private
     markets
────────
- 38 states operate public trust funds — flat per-tank annual fee
  regardless of risk
- Texas closed its petroleum storage tank fund **December 22, 1998** —
  26,000 facilities enter the private risk-rated market overnight
- The private market prices on exactly the attributes the theory
  identifies. Render the following 2x3 table (markdown pipe table or
  beamer tabular):

  | | Flat-fee public fund | Risk-rated private market |
  |---|---|---|
  | **Premium** | Flat per-tank fee | Varies with age, wall type, piping, leak-detection |
  | **Marginal cost of aging** | Zero | Positive |

- Clean policy shock: one state, one date, one clearly defined regime
  change
- 9.8M tank-years of EPA administrative panel data spanning both regimes

────────
S7 — Related literature (4 blocks)
────────
Format: four bold subsection headers each followed by a citation list
on one line (use `\hfill` or similar to right-align if it reads cleanly
in Beamer; otherwise just put the citations on the next line). Below
each block, an italicized PLACEHOLDER contribution arrow. The author
will write the actual contribution text — leave the placeholders as
italic text so they read clearly in the deck.

Block 1 — Empirical work on liability regimes and financial responsibility

  Shavell (1986, 2004, 2007); Yin, Kunreuther & White (2011); Yin, Pfaff
  & Kunreuther (2011); Yin, Pfaff & Kunreuther (2007); Boomhower (2019);
  Ho, Hsu, Cha & Rivera (2018)

  *[PLACEHOLDER: First firm-level panel analysis of insurance design —
  risk-rated vs. flat-fee — as a financial responsibility instrument;
  clean policy shock; structural welfare decomposition; distinguish from
  prior work on asset/bonding leg and prior UST work with weaker
  identification]*

Block 2 — Environmental economics and environmental risk

  Fowlie, Reguant & Ryan (2016); Muehlenbachs (2015); Blundell,
  Gowrisankaran & Langer (2020); Kellogg & Reguant (2021)

  *[PLACEHOLDER: Dynamic structural model of firm behavior under
  environmental regulation; welfare decomposition bounding gap to
  first-best; connects insurance design to structural welfare analysis
  in environmental settings]*

Block 3 — IO of selection markets: consequences of risk-based and
          cost-based pricing

  Einav, Finkelstein & Mahoney (2021); Einav, Finkelstein & Cullen
  (2010); Bundorf, Levin & Mahoney (2012); Handel, Hendel & Whinston
  (2015); Mahoney & Weyl (2017); Einav, Jenkins & Levin (2012); Nelson
  (2020); McWilliams, Hsu & Newhouse (2012); Brown et al. (2014); Wagner
  (2020); Landais et al. (2020); Liberman et al. (2020)

  *[PLACEHOLDER: Extends selection markets framework to an environmental
  liability setting where the pricing regime is exogenously assigned by
  regulation rather than determined by market competition — isolates the
  pure price-signal channel from selection and competition effects;
  welfare analysis of cost-based pricing in a setting where the social
  cost exceeds the insurable cost by construction]*

Block 4 — Economics of UST policy (nascent)

  Yin, Pfaff & Kunreuther (2011); Yin, Kunreuther & White (2011); Yin,
  Pfaff & Kunreuther (2007)

  *[PLACEHOLDER: Most comprehensive firm-level empirical analysis of UST
  regulatory setting; pairs reduced-form identification with structural
  welfare analysis; 9.8M tank-years vs. state-level panel in prior work]*

NOTE: this slide is dense. If it visibly overflows when rendered, split
into two slides (S7a: blocks 1-2; S7b: blocks 3-4) with the same title
plus "(cont.)" on the second.

───────────────────────────────────────────────────
PART 2 — DATA, RISK MEASUREMENT, INSURANCE MARKET (S8-S14)
───────────────────────────────────────────────────
Drop the old section divider (the `Data & Descriptive Evidence` Berkeley
section header) and insert a new one immediately before S8 with the
title: **"Data, Risk Measurement, and the Insurance Market"** using the
same `\BerkeleyMode` + `::: {.layout-statement}` pattern as the current
deck.

Part 2 content is the roughest section. Use existing slides where they
map, but follow the new ordering and the "no slide does more than two
things" rule. Several existing slides will move to APPENDIX (see
appendix section).

────────
S8 — The Data
────────
NEW SLIDE. Bullets:
- EPA UST administrative panel: facility- and tank-level records, ~50
  states, 1988-present
- Unit of observation: facility-year (one row per facility per year)
- Tank-level attributes: install year, capacity, wall construction,
  piping, leak-detection technology
- LUST database: confirmed releases with cleanup-cost dispositions
- Texas SERFF rate filings (Mid-Continent Casualty, 2006-2024) for
  premium reconstruction
- Sample: TX + 17 control states; 9.8M tank-years; alive-at-reform
  facilities

Footer line (italics, small): "Sample construction details and panel
build pipeline documented in Code/Panel_Build/02b-02f."

────────
S9 — Descriptive Statistics
────────
Move the existing descriptive-stats table here:
  \input{../../Output/Tables/T_Desc_Stats_Slide.tex}

Wrap in the same `vspace{0.15cm}` + `\begin{center}\scriptsize ... \end{center}`
pattern the old slide used. Title: "Sample Descriptive Statistics".

Add a one-line caption underneath in italics, centered: "Alive-at-reform
sample; Texas vs. control states on tank age, wall composition, and
vintage."

────────
S10 — Risk is Predictable: setup
────────
Reuse the existing "Risk Prediction: Setup" slide content (current
slide titled "Risk Prediction: Setup" in REFACTORED, around line 350).
Body is fine; title becomes "Risk is predictable: the age-hazard
gradient (setup)".

────────
S11 — Risk is Predictable: the age-hazard figure
────────
Pure figure slide. Use:
  \includegraphics[height=0.78\textheight,keepaspectratio]{../../Output/Figures/Figure_CV_CellRisk.png}

Title: "Single-walled tanks drive the age-hazard gradient"

Preserve the existing navbutton overlay to `cell-gof` (goodness-of-fit
appendix slide):
  \navbutton[cell-gof][fill=BerkeleyBlue, font=\tiny, inner sep=2pt]{Goodness of fit $\rightarrow$}

────────
S12 — The Insurance Market: Premium Construction
────────
Reuse the existing "Premium Construction" slide content verbatim. Title:
"The insurance market: how Texas premiums are built". This is a
formula-heavy slide — keep it as-is.

────────
S13 — The Insurance Market: Premiums Track Risk
────────
Reuse the existing "Premiums Track the Risk We Can Predict" figure slide
verbatim:
  \includegraphics[height=0.80\textheight,keepaspectratio]{../../Output/Figures/Figure_Premium_vs_Hazard_Raw.png}

Title: "Premiums track the risk we can predict"

Preserve the navbutton overlay to `premium-vs-el`:
  \navbutton[premium-vs-el][fill=BerkeleyBlue, font=\tiny, inner sep=2pt]{Predicted per-tank EL $\rightarrow$}

────────
S14 — Premium Levels: Texas vs. Control States
────────
Reuse the existing "Premium Levels: Texas vs. Control States" slide
verbatim:
  \includegraphics[height=0.78\textheight,keepaspectratio]{../../Output/Figures/04e_Premium_by_Year_Treatment.png}

Title: "Premium divergence: Texas vs. control states (1992-2020)"

Add a one-line subtitle in italics, centered under the figure: "Flat and
parallel before 1998; sharp Texas divergence after the reform. Sets up
the DiD without showing it."

────────
S15 — Cleanup Cost Distribution
────────
Reuse the existing "Cleanup Cost Distribution" slide verbatim:
  \includegraphics[height=0.78\textheight,keepaspectratio]{../../Output/Figures/Figure_cost_distribution_pooled.png}

Title: "Cleanup cost distribution"

Add one-line subtitle in italics, centered: "Median $136k; mean $403k.
Right tail dominates expected loss — and grounds the units of K and
kappa in the structural model."

───────────────────────────────────────────────────
PART 3 — REDUCED FORM (S16-S20)
───────────────────────────────────────────────────
Insert section divider: title "Reduced Form Evidence" (preserve the
existing `\BerkeleyMode` + layout-statement pattern from the current
deck).

────────
S16 — Research Design
────────
Reuse existing "Estimating Sample and Specification" slide content. The
two equations (static DiD and event study) stay verbatim. Title:
"Research design: DiD with facility FE and cell-by-year FE"

────────
S17 — Event Study
────────
Reuse existing "Dynamic Closure Response in Texas" slide. Title:
"Event study: closure response in Texas"

Pure figure:
  \includegraphics[height=0.78\textheight,keepaspectratio]{../../Output/Figures/Fig_ES_Full.pdf}

Add a one-line italic subtitle, centered: "Pre-trends flat; sharp break
at Dec 22, 1998; sustained post-treatment effect."

────────
S18 — ATT on Tank Closure
────────
Reuse existing "ATT on Tank Closure: +1.58 pp on a 2.0% Baseline" slide
content. Keep the table input verbatim:
  \input{../../Output/Tables/T_DiD_Stepped_Full.tex}

The caption line stays: "Cols add facility FE, then cell-by-year FE.
Col (3) is the headline."

────────
S19 — Heterogeneity: Pre-89 Vintage Drives the Effect
────────
Reuse existing "Heterogeneity: Pre-89 Vintage Drives the Effect" slide
content. Keep the table input verbatim:
  \input{../../Output/Tables/T_HTE_DiD_Vintage.tex}

Caption stays: "Pre-89 ATT = beta_0 + beta_1 ≈ +3.1 pp; post-88 ATT =
beta_0 ≈ +0.6 pp."

Preserve the navbutton overlay to `vintage-forest`.

────────
S20 — LUST and Composition Results (split into two slides if dense)
────────
Combine the existing "LUST Discovery: Both Channels Fall" and "What
Closing Firms Do: Composition Shifts to Replacement" slides into ONE
slide titled "Two complementary outcomes: LUST discovery falls,
composition shifts to replacement", with BOTH tables stacked.

  Top half:
    \input{../../Output/Tables/T_LUST_Incumbent_Slide.tex}

  Bottom half:
    \input{../../Output/Tables/T2b_Enriched_Slide.tex}

Preserve the navbutton overlay to `lust-es`.

IF the rendered slide visibly overflows: SPLIT into S20a (LUST) and S20b
(Composition), each with its own table. Author preference is one slide
if it fits; otherwise two is fine.

────────
S21 — The Bridge to Structural (TEXT ONLY — NEW SLIDE)
────────
NEW SLIDE. Pure text, no figure, no table. Body:

Title: "What the reduced form cannot tell us"

Opening line (bold): "The DiD identifies an average treatment effect.
Three questions it cannot answer:"

Numbered list:

1. **How large are the welfare effects in dollar terms?** The DiD gives
   us a closure-rate gap; it does not give us a present-value welfare
   number on which to compare policies.

2. **What would alternative policies — subsidy, Pigouvian surcharge,
   age mandate — actually do?** These policies were not implemented in
   Texas. Their welfare consequences are counterfactual.

3. **How far are we from first-best, and why?** RBP is a partial fix:
   premiums price what the insurer can claim, but health damages are
   structurally unpriced. Quantifying the residual wedge requires the
   structural primitives.

Closing line (bold): "Recovering the structural parameters (kappa, K,
gamma_p, gamma_r) is the only way to answer these. The structural model
is not an add-on — it is required."

───────────────────────────────────────────────────
PART 4 — STRUCTURAL MODEL AND COUNTERFACTUALS (S22-S31)
───────────────────────────────────────────────────
Insert section divider: title "Structural Model and Counterfactuals"
(`\BerkeleyMode` + layout-statement, same pattern as existing deck).

DELETE the entire current "Dynamic Model of Tank Closure" block from the
existing deck (the 8 slides between Main Findings and Data & Descriptive
Evidence — Agent's Decision, Dynamic Problem, Policy Lever, First Best,
Three Facts, SOC Exit, RB vs SOC, FF vs SOC, Combined DWL). These all
move to APPENDIX (see below).

────────
S22 — Model Intuition
────────
NEW SLIDE. Body:

Title: "A dynamic discrete-choice model of tank operation, exit, and
replacement"

- Each period, a facility chooses one of three actions: **Maintain**,
  **Exit**, or **Replace**
- The choice trades off current flow payoffs against the continuation
  value of future operation
- Framework: optimal stopping (Rust 1987); estimated by Nested
  Pseudo-Likelihood (Aguirregabiria & Mira 2007)

Subhead: "What the structural model buys over the reduced form:"

- Recovers primitives that govern the exit–replace–maintain decision:
  scrap value, replacement cost, price sensitivity, risk sensitivity
- Holds the economic environment fixed when simulating counterfactual
  policies
- Decomposes welfare into firm surplus, external damage, and government
  outlay

────────
S23 — Flow Utilities
────────
NEW SLIDE. Three equations stacked (use align* or three displayed eqs):

  u^M(s) = 1 + gamma_price * P(s) - gamma_risk * h(s) + alpha_g
  u^E(s) = kappa_{w(s)}
  u^R(s) = -K_{w(s)} + alpha_g

Below the equations, a "Variables" table (use a beamer tabular —
small font). Columns: Symbol / Definition / Source.

| Symbol | Definition | Source |
|---|---|---|
| 1 | Normalized annual net revenue R = $10k/yr/facility | Calibrated |
| P(s) | Per-period insurance premium in state s | SERFF rate filings |
| h(s) | Actuarial leak hazard in state s | First-release prediction model |
| kappa_{w} | Scrap value on exit, by wall type {SW, DW} | Estimated |
| K_{w} | Replacement cost for new DW tank, by wall type | Estimated |
| gamma_price | Price sensitivity (universal scalar) | Estimated |
| gamma_risk | Risk internalization (universal scalar) | Estimated |
| alpha_g | State x regime stay FE (profiled out) | Estimated, drops out of CF |

Footer line (bold): "6 structural parameters: kappa_SW, kappa_DW, K_SW,
K_DW, gamma_price, gamma_risk. Plus 18 alpha_g state fixed effects
profiled out at each likelihood evaluation."

Footer line (italics, small): "Semantic-2 convention: alpha_g enters
the measurement step only — it does NOT enter the counterfactual
re-solve."

────────
S24 — The Bellman Equation
────────
NEW SLIDE. Body:

Title: "The dynamic problem: current payoff vs. option value of
continued operation"

Display equation:

  V(s) = max { u^M(s) + beta * E[V(s') | s],
               kappa_{w(s)},
               -K_{w(s)} + beta * E[V(s_new)] }

Three labeled braces under the max if it fits cleanly (maintain / exit /
replace).

Subhead: "Three margins:"

- **Maintain:** earn flow utility today, preserve the option to operate
  next period
- **Exit:** liquidate — collect kappa_w and leave; absorbing, no
  continuation
- **Replace:** pay K_w today, restart with a new double-walled tank at
  age zero

Subhead: "How the insurance regime enters:"

- **Flat-fee:** P(s) is constant across age, so d/d(age) V through the
  premium ≈ 0 — firm has no price incentive to close aging tanks
- **Risk-based:** P(s) rises with age, so continuation value falls as
  the tank ages — exit becomes optimal sooner

Closing line: "The stopping rule: exit when kappa_w exceeds the
continuation value. Risk-based pricing lowers the threshold age at which
this condition binds."

────────
S25 — Identification
────────
NEW SLIDE. Body:

Title: "Each parameter is pinned by a distinct moment in the choice data"

Table:

| Parameter | Identified by |
|---|---|
| kappa_SW, kappa_DW | Unconditional exit rate P(Exit \| wall) — higher scrap value → more exit |
| K_SW, K_DW | Replace-vs-exit share among closing facilities, by wall type |
| gamma_price | Cross-cell response of choice shares to premium variation across (age, wall, regime) |
| gamma_risk | Residual age-exit gradient after premium is accounted for — hazard variation across cells |
| alpha_g | Group-level stay rate (profiled out — measurement only) |

Subhead: "Intuition for gamma_price and gamma_risk (the key
parameters):"

Two sources of variation move choice shares across cells:

1. **Premium variation** P(s): rises with age under RB, flat under FF —
   identifies gamma_price
2. **Hazard variation** h(s): rises with age and single-walled
   construction — identifies gamma_risk

Closing line: "Because premium and hazard are correlated within regime,
the cross-regime contrast — FF cells where only hazard varies vs. RB
cells where both vary — provides the cross-equation restriction that
separately pins gamma_price from gamma_risk."

Add a navbutton overlay (new):
  \navbutton[ph-scatter][fill=BerkeleyBlue, font=\tiny, inner sep=2pt]{Premium x hazard scatter $\rightarrow$}

(Target slide created in APPENDIX section.)

────────
S26 — Model Fit: Raw Data vs. Model
────────
NEW SLIDE. Figure-only slide showing model-implied vs. empirical action
shares. The user's outline calls for one or two panels max — use the SW
panel first; if it reads well alone, leave the DW for appendix.

  \includegraphics[height=0.78\textheight,keepaspectratio]{../../Output/Figures/04o_Fit_6p_SW.png}

Title: "Model fit: single-walled action shares by age and regime"

Italic caption (centered, small): "Solid lines: model-implied CCPs.
Open circles: empirical cell shares. 32 cells fit simultaneously across
three actions; SW panel shown. DW panel in appendix."

Add a navbutton overlay (new) to the DW fit appendix slide:
  \navbutton[fit-dw][fill=BerkeleyBlue, font=\tiny, inner sep=2pt]{DW fit $\rightarrow$}

Add a second navbutton overlay (new) to the all-cells residual appendix
slide:
  \navbutton[fit-resid][fill=BerkeleyBlue, font=\tiny, inner sep=2pt]{Per-cell residuals $\rightarrow$}

────────
S27 — Estimating Sample
────────
Reuse the existing "Structural Estimating Sample" slide content with
minor edits:

- Update the sample-size language to match Model_Code_Walkthrough.qmd:
  "N ≈ 2.28M facility-years after the T007 churn-symmetry cleanup"
- Update the sample composition to match: "Texas 2006+ (post-RB
  implementation) plus 17 control states 1999+"

Title: "Structural estimating sample"

Other content (unit of obs, choices, premium/hazard sources) stays.

────────
S28 — Parameter Estimates
────────
NEW SLIDE. Replace the existing 4-parameter table.

If a 6-parameter results table file exists (check
`Output/Tables/04l_Theta_Table_8paramFE_profile_AM_SE.csv` or
`Output/Tables/04o_*.tex` — note there is no 6p .tex in the current
file listing), include it. Otherwise create the table inline in
Beamer using the values from Model_Code_Walkthrough.qmd §5:

  | Parameter | Estimate ($ where applicable) |
  |---|---|
  | kappa_SW | $266,000 |
  | kappa_DW | $206,000 |
  | K_SW | $55,000 |
  | K_DW | $54,000 |
  | gamma_price | -1.11 |
  | gamma_risk | +0.069 |

Title: "Parameter estimates: 6-parameter NPL with profiled state FE"

Wrap in `\begin{center} ... \end{center}` with small font sizing.

Add italic footer (centered): "kappa and K back-transformed to dollars
via SCALE_FACTOR = 10,000. Plus 18 alpha_g stay FEs profiled out
(not shown). Standard errors via AM-2002 profile likelihood."

Author note as a qmd-level comment: `<!-- TODO: replace inline table
with .tex input from Output/Tables once 6p AM-SE table is generated -->`

────────
S29 — Estimates in Economic Terms
────────
Reuse the existing "Estimates in Economic Terms" slide structure but
update for 6p model. Body:

Title: "Estimates in economic terms"

Italic intro: "Reminder: kappa and K are reported in dollars
(SCALE = $10K per facility-year). gamma_price and gamma_risk are
unitless coefficients."

Bullets:

- **kappa_SW ≈ $266k; kappa_DW ≈ $206k.** Net scrap value on exit.
  DW is ~25% lower — partial-closure events absorbed into Maintain in
  the cleaned panel may explain part of this gap.

- **K_SW ≈ $55k; K_DW ≈ $54k.** Replacement cost for a new DW tank,
  roughly equal across the wall-type-of-tank-being-replaced. Wall type
  matters mainly through ongoing hazard and premium, not the
  installation cost itself.

- **gamma_price ≈ -1.11.** Negative as expected — higher premiums lower
  the maintain-flow payoff. Roughly 45% less negative than the
  contaminated-panel T005 fit (-2.03).

- **gamma_risk ≈ +0.07.** Positive but small. Firms internalize ~7% of
  the actuarially expected leak damage in their dynamic decisions; the
  remaining 93% is externalized. This is the basis for the CF welfare
  calculation.

────────
S30 — Counterfactual Setup
────────
NEW SLIDE. This is the longest and most equation-heavy slide. If it
visibly overflows after rendering, SPLIT into multiple slides as
described at the bottom of this section.

Title: "Using the Bellman to evaluate counterfactual policies"

Open with the baseline Bellman (RB):

  V^RB(s) = max { u^M(s) + beta * E[V^RB(s')|s],  kappa_w,
                  -K_w + beta * E[V^RB(s_new)] }

  u^M(s) = 1 + gamma_price * P(s) - gamma_risk * h(s) + alpha_g

Intro line (bold): "Each counterfactual modifies one term in u^M(s) or
the action set — then re-solves V and computes the stationary fleet
distribution."

CF1 — Counterfactual flat-fee (the reform never happens):

  u^M_FF(s) = 1 + gamma_price * P_bar - gamma_risk * h(s)

  *Replace P(s) with the median control-state flat-fee premium P_bar
  matched on (age, wall). Re-solve V. The complement of the
  reduced-form ATT — what was the welfare cost of NOT switching to
  risk-based pricing.*

CF2 — Subsidy (replacement cost halved):

  u^R_sub(s) = -(1 - s*) * K_w,   s* = 0.50

  *Government pays half of replacement cost. Welfare = firm-surplus
  gain − government outlay.*

CF3 — Pigouvian (internalize unpriced health damage):

  u^M_pig(s) = 1 + gamma_price * P(s) - gamma_risk * h(s) - h(s) * E,
  E = $17k

  *Add h(s) * E directly to the maintain cost — the planner's
  correction for the unpriced health externality. Welfare = damage
  reduction − firm-surplus loss.*

CF4 — Age mandate (force exit/replace for old tanks):

  u^M_man(s) = -infinity for age bin >= 6 (tanks 25+ years)

  *Eliminate the maintain option for old tanks. Welfare = damage
  reduction − firm-surplus loss.*

Welfare accounting (display equation, full-width):

  Delta W = sum_s pi(s) [ Delta V(s) − sum_t beta^t h(s_t) (L + E)
                          − Delta Govt Outlay ]

With three labeled braces underneath: "Delta Firm Surplus" / "Delta
External Damage (L + E)" / "Delta Govt Outlay (subsidy only)".

Italic closing note: "Important — state fixed effects do NOT enter the
counterfactual value function. They are a measurement correction
(Semantic-2), not structural preference heterogeneity, and correctly
drop out of the policy simulation."

SPLITTING GUIDANCE — if S30 overflows (likely): split into:
  S30a — Baseline Bellman + CFs 1, 2
  S30b — CFs 3, 4 + welfare accounting equation

────────
S31 — Counterfactual Results: Welfare Decomposition
────────
PLACEHOLDER SLIDE for 6p welfare results. Body:

Title: "Counterfactual welfare: change in firm surplus, external damage,
and outlay"

Use the existing 4p bar chart as a stand-in:
  \includegraphics[height=0.78\textheight,keepaspectratio]{../../Output/Figures/04j_4p_Welfare_BarChart.pdf}

Italic caption (centered, small): "*4-parameter model results shown.
[PLACEHOLDER: regenerate with 6-parameter model and 4 CFs at E = $17k.]*"

Add a qmd-level TODO comment: `<!-- TODO: replace with 6p welfare bar
chart once CFs 2-4 are run at E=$17k. CF1 (TX-on-FF) results exist
from 04o but at E=$50K; need re-run. -->`

────────
S32 — Counterfactual Welfare Summary Table
────────
PLACEHOLDER SLIDE. Body:

Title: "Counterfactual welfare summary"

Use the existing 4p summary table as a stand-in:
  \input{../../Output/Tables/04j_Welfare_4p_Summary.tex}

Italic caption (centered, small): "*4-parameter model, three CFs at
E = $17k. [PLACEHOLDER: regenerate 4-row table for 6p model with CFs
1-4 at E = $17k.]*"

Author commentary line (placeholder): "*[PLACEHOLDER: directional
takeaway after 6p re-run — does RB beat all four alternatives, or do
some alternatives beat RB? CF1 at $50K currently shows FF welfare-
improving; need $17K re-run.]*"

Add qmd-level TODO comment: `<!-- TODO: replace with 6p summary table
matching the bar chart on S31. -->`

────────
S33 — Takeaways
────────
NEW SLIDE. Body:

Title: "Takeaways"

1. **Risk is observable.** The age-hazard gradient is steep for single-
   walled tanks and flat for double-walled. Shavell's observability
   condition is satisfied in this market.

2. **Risk-based pricing shifts behavior.** +1.58 pp annual closure
   ATT; ~4x larger in the pre-1989 SW-heavy stock that the premium
   schedule actually prices. Composition shifts toward the replacement
   margin (12% → 18%). LUST discovery falls through both background and
   inspection-triggered channels.

3. **Welfare gap to first-best persists.** [PLACEHOLDER: directional
   claim about the four counterfactuals at E = $17k, pending 6p re-run.
   Even after the reform, the health-externality component remains
   structurally unpriced — actuarial premiums cannot price what no
   claimant recovers.]

───────────────────────────────────────────────────
APPENDIX
───────────────────────────────────────────────────
Place all appendix slides at the very end of the file, after S33. Each
appendix slide gets `{visibility="uncounted"}` after its title and a
`\hypertarget{...}` immediately after `\BerkeleyLightMode`, exactly
matching the existing appendix-slide pattern in REFACTORED.

Order:

A1 — Toy model intro / Agent's Decision (current deck's "The Agent's
     Decision" slide). Move verbatim. Add a brief footer line:
     "*Two-action stopping model used in early presentations — included
     for completeness; superseded by the three-action DCC in the main
     deck.*"

A2 — Toy model: Dynamic Problem (current "The Dynamic Problem" slide).
     Move verbatim.

A3 — Toy model: Policy Lever (current "The Policy Lever: Insurance
     Regimes" slide). Move verbatim.

A4 — Toy model: First Best (current "What's the First Best?" slide).
     Move verbatim.

A5 — Toy model: Three Facts (current "Three Facts to Keep in Mind"
     slide). Move verbatim.

A6 — Toy model: SOC Exit Threshold (figure-only — current "Social
     Optimum Exit Threshold" slide using Slide_Fig_2_SOC_Exit.pdf).
     Move verbatim.

A7 — Toy model: RB vs. SOC (current "Risk-Based vs. Social Optimum"
     slide using Slide_Fig_3_SOC_RB_Comparison.pdf). Move verbatim.

A8 — Toy model: FF vs. SOC (current "Uniform Premium vs. Social
     Optimum" slide using Slide_Fig_5_SOC_FF_Comparison.pdf). Move
     verbatim.

A9 — Toy model: Deadweight Loss (current "Deadweight Loss Across
     Regimes" slide using Slide_Fig_7_Combined_DWL.pdf). Move verbatim.

A10 — FR Compliance: TX is a Private-Insurance Market (current
      "FigureA_TX_FR_Coverage_Composition.pdf" slide). Move verbatim.

A11 — Market Structure: Concentrated and Stable (current
      "Figure_TX_FR_Top5_Dominance_HHI.pdf" slide). Move verbatim.

A12 — Cell-Level Goodness of Fit (current "Appendix: Cell-Level
      Goodness of Fit" slide). Keep `\hypertarget{cell-gof}` and the
      existing figure. Move verbatim.

A13 — Vintage Forest Plot (current "Appendix: ATT by Installation
      Vintage" slide). Keep `\hypertarget{vintage-forest}` and existing
      figure. Move verbatim.

A14 — Premium vs. Per-Tank Expected Loss (current "Appendix: Premium
      vs. Per-Tank Expected Loss" slide). Keep `\hypertarget{premium-vs-el}`
      and existing figure. Move verbatim.

A15 — LUST Event Study — Total Leak Discovery (current "Appendix: LUST
      Event Study --- Total Leak Discovery" slide). Keep
      `\hypertarget{lust-es}` and existing figure. Move verbatim.

A16 — Subsidy Curve (current "Appendix: Subsidy Curve" slide). Move
      verbatim.

A17 — NEW: Premium x Hazard Scatter (identification figure for
      gamma_price vs gamma_risk).

      Title: "Appendix: Premium x Hazard cell-level scatter {visibility=\"uncounted\"}"
      `\hypertarget{ph-scatter}{}`
      `\includegraphics[height=0.78\textheight,keepaspectratio]{../../Output/Figures/Slide_PH_Scatter.png}`

      Note: this figure DOES NOT YET EXIST as a standalone PNG. The
      content is rendered inside Reports/Paper/Identifying_Variation_Size_Capacity.qmd
      as fig-Ph-scatter (chunk label, lines ~145). Add a qmd-level
      comment: `<!-- TODO: extract fig-Ph-scatter from
      Identifying_Variation_Size_Capacity.qmd as a standalone PNG into
      Output/Figures/Slide_PH_Scatter.png; in the meantime, this slide
      will fail to render. Leave the include line as-is. -->`

A18 — NEW: 6p Fit, DW Panel.

      Title: "Appendix: model fit — DW panel {visibility=\"uncounted\"}"
      `\hypertarget{fit-dw}{}`
      `\includegraphics[height=0.78\textheight,keepaspectratio]{../../Output/Figures/04o_Fit_6p_DW.png}`

A19 — NEW: 6p All-Cells Residuals.

      Title: "Appendix: per-cell fit residuals (6p) {visibility=\"uncounted\"}"
      `\hypertarget{fit-resid}{}`
      `\includegraphics[height=0.78\textheight,keepaspectratio]{../../Output/Figures/04o_Fit_6p_AllCells_Residuals.png}`

A20 — NEW: 6p CF Overlay, SW (action shares baseline vs. CF1):

      Title: "Appendix: TX-on-FF counterfactual — SW action shares {visibility=\"uncounted\"}"
      `\hypertarget{cf-overlay-sw}{}`
      `\includegraphics[height=0.78\textheight,keepaspectratio]{../../Output/Figures/04o_Fit_6p_CF_Overlay_SW.png}`

A21 — NEW: 6p CF Overlay, DW:

      Title: "Appendix: TX-on-FF counterfactual — DW action shares {visibility=\"uncounted\"}"
      `\hypertarget{cf-overlay-dw}{}`
      `\includegraphics[height=0.78\textheight,keepaspectratio]{../../Output/Figures/04o_Fit_6p_CF_Overlay_DW.png}`

A22 — NEW: CF1 Removal-Age Distribution:

      Title: "Appendix: TX-on-FF counterfactual — removal-age distribution {visibility=\"uncounted\"}"
      `\hypertarget{cf-removal-age}{}`
      `\includegraphics[height=0.78\textheight,keepaspectratio]{../../Output/Figures/04o_CF_RemovalAge_Distribution_TX.png}`

═══════════════════════════════════════════════════
ACCEPTANCE CRITERIA
═══════════════════════════════════════════════════

Mechanical checks. Reviewer (the author) will inspect by opening the
qmd in RStudio and visually scanning, then rendering once at the end.

- [ ] File edited: Reports/Slides/04_Risk_Based_Pricing_and_USTs_TALK_EDIT.qmd
- [ ] File NOT edited: Reports/Slides/04_Risk_Based_Pricing_and_USTs_REFACTORED.qmd
- [ ] YAML header unchanged from the source file
- [ ] Old S1-S5 (Motivation, Setting, Contribution, Empirical Strategy,
      Main Findings) deleted; new S1-S7 inserted in their place with
      content matching this spec verbatim
- [ ] Old "Dynamic Model of Tank Closure" Berkeley section divider and
      its 8 child slides moved to APPENDIX as A1-A9
- [ ] Old "FR Compliance" and "Market Structure HHI" slides moved to
      APPENDIX as A10, A11
- [ ] Part 2 ordering matches S8-S15 in this spec
- [ ] Part 3 ordering matches S16-S21 in this spec; S21 (Bridge to
      Structural) is text-only with the three numbered questions
- [ ] Part 4 ordering matches S22-S33 in this spec
- [ ] All flow utility equations use the sign convention:
      u^M = 1 + gamma_price * P - gamma_risk * h + alpha_g
- [ ] Parameter values reported as: gamma_price = -1.11,
      gamma_risk = +0.069 (matching the .rds object)
- [ ] kappa and K reported in dollars (multiplied by SCALE_FACTOR)
- [ ] All references to E (health damage) use $17k, never $50k
- [ ] All 6p welfare numbers replaced with [PLACEHOLDER: ...] where
      6p results don't yet exist
- [ ] 4p welfare bar chart and table used as stand-ins on S31 and S32
      with explicit italic captions saying "4-parameter model;
      PLACEHOLDER pending 6p re-run"
- [ ] qmd-level TODO comments added wherever the spec calls for them
- [ ] Existing navbuttons (cell-gof, vintage-forest, premium-vs-el,
      lust-es) preserved verbatim on their slides
- [ ] New navbuttons added on S25 (-> ph-scatter), S26 (-> fit-dw,
      -> fit-resid)
- [ ] All appendix slides have `{visibility="uncounted"}` after their
      title and a `\hypertarget{...}` immediately after `\BerkeleyLightMode`
- [ ] No slides deleted outright (everything either kept in main flow
      or moved to appendix)

═══════════════════════════════════════════════════
EXPLICITLY OUT OF SCOPE
═══════════════════════════════════════════════════

- Running any R code
- Regenerating any figure (`Output/Figures/*`)
- Regenerating any table (`Output/Tables/*`)
- Re-running CF1 at E = $17k (this would require running 04o again with
  a different E parameter — author will do separately)
- Editing the canonical REFACTORED.qmd
- Editing Reports/Paper/Model_Code_Walkthrough.qmd or any file outside
  Reports/Slides/
- Writing the "contribution" text under the Related Literature blocks
  (those stay as italic [PLACEHOLDER: ...] for the author)
- Choosing which fit figure (04o_Fit_6p_SW.png vs. alternative) to use
  on S26 — spec locks this in as 04o_Fit_6p_SW.png

═══════════════════════════════════════════════════
ATTEMPT LOG
═══════════════════════════════════════════════════
[Filled by reviewer after each attempt. Leave blank until first attempt.]
