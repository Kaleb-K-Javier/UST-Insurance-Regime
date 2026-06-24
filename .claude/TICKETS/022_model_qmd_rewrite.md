# TICKET 022 — Portfolio-model QMD rewrite (model v4 + evidence in template)
# Created: 2026-06-12
# Status: READY (architect spec complete; researcher approved direction in chat)
# Attempt: 0
# Type: WRITING/PRESENTATION ONLY. No analysis, no estimation, no R beyond
#       rendering. Every number cited below already exists in Output/Tables/.
# Coder seat: Sonnet (medium effort is fine — this is transcription + careful
#       prose in a prescribed register, not design).

═══════════════════════════════════════════════════
TARGET + WHAT TO REPLACE
═══════════════════════════════════════════════════
File: Reports/Paper/Scale_Incorporation_Model_Sketch.qmd  (renders via
  `quarto render ... --to pdf`; xelatex; booktabs/float already in header).
KEEP unchanged: YAML header; "# Purpose" through the END of
  "## Consequence for the counterfactuals" (the 6p-model strains — part 1).
REPLACE everything after that (the old "# The scale-incorporation model",
  "# Identification sketch", "# Data requirement and open empirical checks",
  AND the current "# The assumption evidence suite") with THREE new parts:
  Part 2  "# The portfolio model"            (model v4 — Section A below)
  Part 3  "# The assumption evidence suite"  (12 exhibits in the TEMPLATE —
          Section B below; reuse existing prose WHERE marked, rewrite the rest)
  Part 4  "# What the counterfactuals need, and how far the data lets them go"
          (Section C below)
Old text is in git history; do not archive in-file.

═══════════════════════════════════════════════════
GLOBAL WRITING RULES (researcher-mandated; violations = rewrite)
═══════════════════════════════════════════════════
R1 BEHAVIORAL REGISTER: every assumption states in 1-2 sentences what it
   claims about how the OWNER behaves/chooses — or says outright "this is
   bookkeeping, not a claim about behavior." Model the tone on:
   "the shocks are the part of each option's payoff the owner sees and we
   don't — the owner is rational; the randomness is our ignorance."
R2 TANKS-FIRST LANGUAGE: facts stated about tanks/owners; "cells/categories"
   appear only as "the model's bookkeeping records this as ...".
R3 ASSUMPTION SECTIONS open with the assumption as CENTERED DISPLAY MATH
   ($$...$$), then 2-3 sentences: why the model needs it + how the exhibits
   below support it. Then: **Why** paragraph, **How it is computed** paragraph,
   exhibit(s), **What it says** paragraph (a specific number -> plain meaning
   -> what it licenses for the model).
R4 FIGURES: latex figure envs, [H], short caption (~6 words), then
   {\footnotesize\textit{Notes:} ...} block. NO titles inside images.
   TABLES: \input the .tex inside table envs with short \caption + Notes
   block. (The existing v2 section already does this — keep that pattern.)
R5 MATH RENDER-SAFE: plain $$ blocks; no \big, \boxed, \textbf inside math.
R6 NO new analysis. If a number is missing, STOP and report — do not compute.

═══════════════════════════════════════════════════
SECTION A — "# The portfolio model"  (write verbatim-faithful to this)
═══════════════════════════════════════════════════
A0 Opening para: the model exists for four counterfactuals (list them); it is
   judged by coverage of the margins they need, not by fit. One sentence:
   every design choice below traces to an exhibit in Part 3 or a
   counterfactual requirement in Part 4.

A1 State.  s = (n, G, rho).
   $$ s = (n, G, \rho), \qquad n \in \mathbb{N}_0^{16} $$
   n = tank counts over (wall x five-year age band); G = capacity quartile
   bin; rho = regime, fixed. Behavioral sentence: the owner thinks about
   tanks the way the insurer prices them and leaks happen (construction +
   age) and about scale in gallons. Bookkeeping sentence: tanks in one
   category are interchangeable (evidence: X5).

A2 First-stage objects (a bulleted list; all measured, none estimated):
   empirical cell prices pbar_c (X3E table); the FF CONTRACT LOOKUP — each
   control state's own (tau_s, D_s), time-averaged for stationarity (TX
   D = 5,000 assumed, registry has no field). KS AND MD ARE EXCLUDED FROM
   ESTIMATION (researcher ruling 2026-06-12; CONFIRMED against the raw notes,
   which quote the ASTSWMO surveys): both states' funds functioned as CLEANUP
   funds but did not satisfy federal financial responsibility — KS operators
   were required to buy private third-party liability coverage (K.S.A.
   65-34,117), and the ASTSWMO 2002 Table 2 footnote states MD owners "need
   private insurance or be self-insured." The operator-facing premium in both
   is private and unrecorded -> excluded. QMD FOOTNOTE (one sentence): the
   exclusion follows the ASTSWMO surveys; Kansas is a noted robustness
   candidate for re-inclusion (its fund deductible IS recorded — 3,000 +
   500 per tank, a deductible with a count slope) under the stated assumption
   that its private liability premium is composition-invariant and absorbed
   by the Kansas fixed effect. REQUIRED sentence: per-state contracts cost
   no parameters and no extra computation (the engine already solves per
   state for the FEs), and the variation SURVIVES the state fixed effects
   because contracts enter multiplied by within-state-varying quantities —
   tau_s as a slope on N(n), D_s as a slope on H(n); an additive FE absorbs
   levels, not slopes.
   IDENTIFICATION-THREATS PARAGRAPH (REQUIRED, 3-4 sentences, this content):
   the slope-based identification compares hazard- and count-responsiveness
   across states with different contracts, so it is exposed to anything that
   makes those responsiveness differences for other reasons. Three concrete
   threats to name: (i) contract terms may be set IN RESPONSE to the state's
   tank stock — a state with riskier old tanks choosing a higher deductible
   bends the slope comparison (reverse causality); (ii) enforcement and
   inspection intensity may co-vary with contract stringency, so behavior
   attributed to the deductible could be inspections; (iii) fund payout
   reliability differs across states — a slow- or unreliably-paying fund
   changes the owner's effective exposure beyond the stated D. Mitigations
   stated, not oversold: state FEs absorb level confounders, year FEs absorb
   common shocks, and the regime channel is cross-validated by the DiD;
   threats (i)-(iii) are why the welfare results lean hardest on the
   within-TX and regime-contrast variation, with the cross-state slopes as
   supporting evidence. Zero-fee states still contribute to gamma_r through
   their deductibles; the
   facility-level hazard function h^ML (leak data is facility-level — it
   CANNOT support per-tank rates; assumption stated as such); per-tank aging
   kernel Lambda (X4 age table); capacity kernel Gamma_G(. | G, m-k)
   (X4 transition table; NOTE: conditions on NET tank change m-k — to be
   re-estimated on that conditioning in the estimator first stage);
   the removal rule.

A3 Perceived quantities:
   $$ N(n)=\sum_c n_c \qquad P(n,\rho)=\begin{cases}\sum_c n_c \bar p_c & RB\\ \tau_s N(n) & FF\end{cases} $$
   $$ H(n) = h^{ML}(\bar a(n), w(n)) \qquad OOP(n) = H(n)\cdot \min\{D, L\} $$
   Prose: P is the actual average bill by construction (empirical prices);
   H is "a leak at my facility this year" read off the risk model at the
   portfolio's average-age band and majority wall — no finer than the data;
   OOP is the contract: the owner pays the deductible when a leak happens,
   the insurer/fund pays the rest (min is ~always D since L ~ $300K).

A4 Actions — the (k,m) menu:
   $$ \mathcal A(s) = \{(k,m): 0\le k\le N(n),\ 0\le m\le \bar m\} \cup \{X\} $$
   (0,0)=maintain; (k,0)=downsize; (k,m>=1)=replace/consolidate; X=exit;
   expansion outside the model (fenced, X1). Wing-menu explanation REQUIRED
   (researcher-approved language): the menu is like wings priced per wing —
   each (k,m) is just another option; two per-unit costs price every line;
   the model never predicts k or m, the owner chooses them.
   Evidence pointer: the (k,m) cross-tab (X7 menu table) — only 13.7% of
   replacements remove exactly one tank; the modal replacement removes 3
   and installs 2.
   Removal rule as centered math (researcher-approved working):
   $$ \text{the } k \text{ removed tanks are the oldest single-walled; if fewer than } k \text{ exist, the oldest double-walled fill the remainder} $$
   $$ n' = n - r(n,k) + m\, e_{DW,1} $$
   Installs are double-walled by law (one sentence + X9 pointer).

A5 Flow payoffs (SIGN CONVENTION paragraph REQUIRED: all money outflows
   carry explicit minus signs; gamma_p, gamma_r are POSITIVE dollars-to-
   utility exchange rates; gamma_p = gamma_r is then a testable claim that
   the owner treats a certain premium dollar and an expected out-of-pocket
   dollar identically — differences mean salience/risk-weighting):
   $$ u_{(k,m)}(s) = \varphi_G + \alpha_g - \gamma_p P(n',\rho) - \gamma_r\, OOP(n') - k c^{rem} - m c^{inst} $$
   $$ u_X(s) = \kappa_1 N(n), \qquad \kappa_0 := 0 $$
   kappa_0 := 0 = a units choice (one sentence).

A6 Laws of motion: removal/install map, then Lambda on survivors; G' from
   Gamma_G(.|G, m-k) (degenerate stay at (0,0)); exit absorbing; rho fixed.
   One sentence: "every object here is an exhibit in Part 3, not an
   assumption."

A7 Choice structure — the nest, FULLY written:
   $$ v_a(s) = u_a(s) + \beta \sum_{s'} F_a(s'|s) V(s'), \qquad \beta = 0.95 $$
   $$ IV(s) = \lambda \log \sum_{(k,m)\ne(0,0)} e^{v_{(k,m)}(s)/\lambda} $$
   $$ V(s) = \log( e^{v_{(0,0)}(s)} + e^{IV(s)} + e^{v_X(s)} ) $$
   $$ P((k,m)|s) = \frac{e^{IV(s)}}{e^{v_{(0,0)}}+e^{IV}+e^{v_X}} \times \frac{e^{v_{(k,m)}/\lambda}}{\sum_{(k',m')\ne(0,0)} e^{v_{(k',m')}/\lambda}} $$
   Behavioral register for the shocks (REQUIRED, researcher-approved
   language): each option carries a draw = what the owner knows that we
   don't (retirement, lease, contractor quote); the owner is rational, the
   randomness is our ignorance. lambda in (0,1]: one estimated number for
   how much the tank-work options share unobserved circumstances (the same
   contractor visit); lambda = 1 is the flat model — a special case the
   data can choose. Note the menu's PAYOFF structure already makes nearby
   (k,m) options close substitutes; lambda only carries the residual
   shared-circumstances channel.

A8 Parameters + estimation:
   $$ \theta = (\varphi_G, \gamma_p, \gamma_r, c^{rem}, c^{inst}, \kappa_1, \lambda, \{\alpha_g\}) $$
   $$ \hat\theta = \arg\max \sum_{i,t} \log P(a_{it}\mid s_{it};\theta) $$
   NPL fixed point; nested inclusive-value machinery already in the library.
   Identification map (one line each): gamma_p <- contract contrasts (regime
   gap $709 at matched composition; re-filings; cross-state fees);
   gamma_r <- hazard variation x deductible levels (within-TX D flat ->
   variation through H; cross-state D moves levels); gamma_p = gamma_r
   testable; c_rem, c_inst <- (k,m) frequencies; kappa_1 <- exit-size
   gradient (X2); lambda <- co-movement of tank-work options beyond observed
   payoffs. Eight economics parameters + FE on a state two orders of
   magnitude richer — parameters did not grow because removal identity is
   predictable and costs are per-unit.

A9 Untestable/conventional assumptions (compact list, behavioral register):
   shock distribution shape; beta = 0.95; owners' beliefs = the estimated
   hazard/aging processes; stationarity (no anticipation; ban dates outside
   the model); facility-level hazard resolution (data limitation, stated);
   linear utility; kappa_0 := 0 units.

A10 WELFARE WEDGE (write this derivation verbatim, half page):
   A leak at facility i costs society L + E: cleanup L plus external damages
   E beyond cleanup. Under contract (P, D) the OWNER bears min{D, L} = D;
   the INSURER/FUND bears L - D, which under risk-rating is priced back into
   P (the pool breaks even on the facility's own hazard) and under flat fees
   is socialized across all facilities; E is borne by nobody in the market.
   The owner's private marginal cost of own-hazard H(n):
     RB:  d/dH [ gamma_p P + gamma_r OOP ] -> facility internalizes D
          directly plus (L - D) through the actuarial premium channel
          (to the extent the card tracks H — cite the X3E flatness finding:
          the effective card is compressed, so pass-through is partial).
     FF:  the facility internalizes ONLY gamma_r * D * dH; (L - D) and E
          are fully external.
   First-best (CF2): set the facility's total private exposure equal to
   H(n) (L + E) at the margin. The wedge that policy can close:
     FF wedge per unit hazard = (L - D) + E ; RB wedge = E + the
     under-pass-through of (L - D) implied by the compressed card.
   End with: welfare tables report SocialWelfare = ProducerSurplus -
   ExternalDamage - GovtOutlay under each contract, with E calibrated
   (E = $50K convention) and a band.

═══════════════════════════════════════════════════
SECTION B — Part 3 evidence suite: per-subsection change list
═══════════════════════════════════════════════════
The current v2 section (in the file now) already has Why/How/exhibit/
What-it-says for 12 exhibits. REWORK each to the R3 template (centered
assumption equation first + 2-3 sentence support plan) and apply these
specific content changes:

B-X1 (what facilities do): keep; add ONE sentence on the multinomial
   question: the model is a single choice among all options; the evidence
   regressions are one-vs-rest binary logits because clustered SEs are
   native there and with 97.5% maintain the two are numerically near-
   identical for rare margins.
   Assumption equation: $$ \mathcal A(s) = \{(k,m)\} \cup \{X\} $$

B-X2 (size): keep; assumption equation $$ n \in s $$ (counts are in the
   state); "What it says" stays (0.48 / 27.1 / 3.6 numbers).

B-X5 (homogeneity): assumption = bookkeeping claim:
   $$ \text{tanks within a (wall, age band) category are interchangeable} $$
   Add the researcher-register meaning: the model's agent cannot tell two
   same-category tanks apart — and neither can the real owner's bill or
   risk, since price and hazard are constant within a category.

B-X4 (capacity): RETITLE "Capacity moves like Rust's mileage". Assumption:
   $$ G' \sim \Gamma_G(\cdot \mid G,\ m-k), \quad \text{degenerate at } (0,0) $$
   Wire BOTH new tables: AE_X4_Age_Transition.tex (the L1 kernel — stay
   0.725-0.828 by band, band 8 absorbing) AND AE_X4_G_Transition.tex.
   What-it-says: maintain diagonal 0.998; downsize stay 0.564; replace stay
   0.465 — capacity is fixed until the owner touches tanks; the kernel will
   be conditioned on net change m-k in the estimator (one sentence; remove-3-
   install-2 is not remove-4-install-1).

B-X6 (which tank): keep, tanks-first language pass. Assumption equation =
   the removal rule (A4's centered text math).

B-X7 (rule fidelity + menu): SPLIT into two subsections:
   B-X7a "The rule predicts which tanks leave" — fidelity table + heat map +
   NEW miss-cost table (AE_X7_Miss_Cost.tex). What-it-says MUST make the
   two-halves argument explicitly: (i) identity rule: 84% exact, wall part
   fails 2.8%; (ii) the misses are cheap: conditional on a miss, mean
   premium error $71/yr (p90 $142) on bills of $500-1,065 and hazard error
   0.14pp; unconditionally ~ $11/yr ~ 1-2% of the bill (card-based = upper
   bound; the empirical card is flatter). This closes the big-facility
   downsizing worry QUANTITATIVELY — say so.
   B-X7b "How many tanks: a chosen intensity" — AE_X7_KM_Menu.tex +
   Shed_vs_Block reference. Assumption equation:
   $$ k, m \ \text{are choices; only } r(n,k) \text{ is a rule} $$
   Wing-menu language here. Replace is not 1-for-1 (13.7%; modal 3-out-2-in).

B-X8 (recycling): keep but OPEN with its purpose (researcher flagged):
   this exhibit exists to kill the old reset-to-zero replace transition and
   pin "tanks out, tanks in, scale carried." Assumption equation:
   $$ n' = n - r(n,k) + m\, e_{DW,1} \quad (\text{no reset}) $$

B-X9 (DW installs): keep; assumption $$ \text{installs} = e_{DW,1} $$.

B-X10 (regime margins): keep; add one sentence that these are the margins
   the counterfactuals move (forward pointer to Part 4).

B-X3 (premiums): RESTRUCTURE as "What facilities actually pay".
   Order: (1) AE_X3_Premium_by_Age figure FIRST (the researcher's
   facility-level picture: TX single-walled totals rise ~$500 -> ~$1,065
   across age bands; flat-fee lines flat) with Notes explaining the
   single-category plotting device (86.6% of facility-years; mixed
   facilities have no single age — which is WHY the model prices by
   composition) AND the count-channel caveat (older portfolios hold ~2x
   more tanks; part of the total gradient is count, not per-tank price).
   (2) filed card table + empirical card table side by side with the
   flatness finding ($278-310 vs $319-443; wall gap $15-30 vs $71;
   candidate explanation: status/equipment discounts offset age loads;
   DW bands 7-8 thin (n=381/206) — crossover there is noise+selection,
   say so). (3) the regenerated two-worlds scatter (corrected H axis:
   "Facility hazard (ML model at composition)").
   Assumption equation: $$ P(n,\rho) = \sum_c n_c \bar p_c \ (RB), \quad \tau_s N(n) \ (FF) $$

B-X11 (identification): UPDATE NUMBERS to the regenerated (corrected-H)
   table: RB resid/sd(P) = 0.267, regime gap $708.8 (SE 127.5). Add one
   sentence that H here is the corrected facility-level construction.
   Keep the FF zero-fee notes (KS MD MN OK SD VA; 40.2%).

B-X12 (occupancy): keep as-is plus template header.
   $$ 368 \text{ compositions cover } 95\% \text{ of facility-years} $$

═══════════════════════════════════════════════════
SECTION C — Part 4: counterfactual coverage + resolution ledger
═══════════════════════════════════════════════════
For EACH CF: one paragraph "the surgery" (what object changes), one
paragraph "the margins it runs through and which exhibits cover them",
then its DATA-RESOLUTION sentence in italics (verbatim below).

CF1 TX stays flat-fee: surgery = transplant the WHOLE CONTRACT C = (tau, D),
  not just premiums; bounding via REAL control contracts: zero-fee state's
  contract (note: zero-fee contracts still carry deductibles — CF1-low is
  not "insurance vanishes"), highest-fee contract, mean contract. The
  deductible transplant moves the OOP term too — both channels priced.
  Resolution: "Bounded, not point-identified — Texas's counterfactual
  flat-fee contract is unobserved, so we report the range spanned by actual
  control-state contracts."
CF2 first-best: surgery = total private exposure equals H(n)(L+E) at the
  margin; uses the A10 wedge; the gamma_p vs gamma_r distinction means the
  INSTRUMENT matters (premium-channel vs deductible-channel dollars).
  Resolution: "E is calibrated, not estimated; welfare reported at the
  calibration and a band."
CF3 SW replacement subsidy: surgery = lower c_inst (or subsidize (k,m)
  removing SW). Runs through the menu; the with/without-lambda presentation
  (verbatim researcher-approved): "When the subsidy creates new
  replacements, the model must apportion them between firms that would have
  removed tanks anyway and firms that would have done nothing. One
  estimated parameter governs that split, so we report the subsidy's
  effects both at its estimated value and with it switched off — showing
  the reader how much of the conclusion rides on it."
CF4 age-A mandate: surgery = delete options keeping tanks in bands above A
  (state counts them, so feasible); compliance margins all present incl.
  the cheap one (k,0). Resolution: "The mandate's risk benefit is measured
  at the resolution of the leak data — facility-level — so the model
  credits removing an old tank with the facility's hazard improvement, not
  a tank-specific one. No estimate from this data can do better."

═══════════════════════════════════════════════════
MECHANICS + ACCEPTANCE
═══════════════════════════════════════════════════
- All .tex/figure files exist already: AE_X1..X12 plus AE_X3_Premium_by_Age,
  AE_X3_Empirical_Premium, AE_X4_Age_Transition, AE_X4_G_Transition,
  AE_X7_Miss_Cost, AE_X7_KM_Menu (Output/Tables, Output/Figures; X3 scatter
  and X11 tables are the REGENERATED corrected-H versions — cite those
  numbers, not the old ones).
- Render `quarto render Reports/Paper/Scale_Incorporation_Model_Sketch.qmd
  --to pdf` must exit clean; check the .log for Missing/Undefined.
- ACCEPTANCE: [ ] Parts 2-4 replace the old sketch/identification/evidence
  sections; Part 1 untouched. [ ] Every assumption subsection opens with
  centered display math + 2-3 sentence support plan. [ ] All 12+ exhibits
  present with figure Notes blocks and no in-image titles. [ ] theta
  includes lambda; the nest equations appear exactly as in A7. [ ] Sign
  convention paragraph present; OOP = H min{D,L} throughout. [ ] X11/X3
  numbers are the corrected-H ones (0.267 / $708.8). [ ] Miss-cost argument
  appears with the $71 / $11 numbers. [ ] (k,m) menu with 13.7% / modal
  3-out-2-in. [ ] CF section has all four resolution sentences verbatim.
  [ ] Renders clean.

═══════════════════════════════════════════════════
NEXT STEPS AFTER THIS TICKET (for the record; NOT in scope)
═══════════════════════════════════════════════════
023 Estimator first-stage spec: empirical pbar_c by (cell x era);
    Gamma_G(.|G, m-k); the FF CONTRACT LOOKUP (tau_s, D_s) per state,
    time-averaged; KS/MD EXCLUDED (researcher ruling — private-insurance
    contracts unrecorded); deductible_usd re-kept in panel prep; H-at-
    composition columns; (k,m) action coding in the estimation panel.
RAW-DATA AUDIT LIST (early-mid June 2026 work) now carries: MO/SD near-zero
    exits; the Q=2.0e9 capacity facility; KS/MD fund-vs-private coverage
    history by year (re-inclusion candidates if fund era confirmed); post-ban
    SW-install wall-coding (6 pct tripwire from X9).
024 Estimator implementation (NPL with nested IV; new config; engine reuse).
    Then CF1-CF4 per Section C.

═══════════════════════════════════════════════════
ATTEMPT LOG
═══════════════════════════════════════════════════
ATTEMPT 1 — 2026-06-12 — VERDICT: PASS (Opus acting reviewer; verified by
independent greps + targeted reads of the rendered qmd, not the coder report).
- Parts 2-4 in place, Part 1 untouched; 14 \input + 9 \includegraphics;
  centered assumption equations; behavioral register confirmed (wing-menu,
  "the randomness is our ignorance"); corrected-H numbers (0.267/$708.8);
  miss-cost ($71/$11); (k,m) menu (13.7%, 3-out-2-in); all FOUR CF resolution
  sentences verbatim (CF4 wraps lines — single-line grep false-negative);
  renders clean.
- TWO ARCHITECT PATCHES applied post-review (rulings postdate the coder's
  start; not coder errors): (1) beta 0.95 -> 0.9957 in A7 + assumptions list
  (researcher ruling: 0.9957, matching the canonical fit's config; CLAUDE.md
  beta=0.95 is STALE — fix at next CLAUDE.md rebuild); (2) KS/MD passage
  replaced with the ASTSWMO-confirmed language (cleanup funds fine, FR not
  satisfied, private premiums unrecorded; Kansas robustness candidate with
  its 3,000+500/tank deductible). Re-rendered clean (735KB).
