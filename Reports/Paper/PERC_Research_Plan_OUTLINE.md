# PERC Research Plan — Working Outline

**Working title:** *Internalizing Environmental Risk: Insurance Design and Firm
Behavior in Hazardous Industries*
**Author:** Kaleb K. Javier
**Purpose of this file:** a bullet skeleton you expand into the five PERC
sections in `PERC_Reserach_Plan.qmd`. Each bullet is a topic sentence or a fact
to defend — turn it into prose. Numbers and equations are pre-filled from the
current TALK_EDIT deck and the 6-parameter fit so you are not hunting for them.

> Source decks: `04_Risk_Based_Pricing_and_USTs_TALK_EDIT.qmd` (current numbers,
> 3-question framing, 6p structural) and `..._REFACTORED.qmd` (older 4p — do not
> re-copy its γ's). Skeleton respects the PERC page budgets in
> `Research Plan and Workshops 2025.pdf`.

---

## The take (one paragraph — say this out loud before writing)

Strict liability is the workhorse instrument for environmental harm, but it
under-deters when firms are effectively judgment-proof. Shavell's answer is a
financial-responsibility (FR) menu; one leg is **compulsory liability insurance**,
which only does the deterrence work *if the insurer can price on risk-relevant
attributes*. USTs are the cleanest setting to test that conditional: risk is
observable (age, wall), the harm is real and unpriced by tort, and Texas flips
**one facility population from a flat per-tank fee to risk-rated private premiums
overnight in December 1998**. The paper walks three questions in sequence —
**is risk observable (Q1), does pricing it change behavior (Q2), and how much of
the first-best gain does pricing capture (Q3)** — and the structural punchline is
that firms internalize only a small share of true leak damage, so even
*actuarially fair* risk-based pricing leaves the health externality structurally
unpriced. That residual wedge — not whether RB "beats" flat-fee on net — is the
contribution. *(The magnitude of that internalization is what the portfolio model
estimates — see milestones; no structural numbers are quoted in this plan yet.)*

**One honest framing note up front:** the *behavioral* results (Q1, Q2) are solid
and dollar-anchored. The *welfare ranking of regimes* (Q3) is **not yet estimated
on the portfolio model** — early single-tank runs hinted the ranking is not a
foregone conclusion (small risk internalization can leave flat-fee competitive on
measured welfare). Do not write the plan as "RB closes the gap and wins." Write it
as "pricing changes behavior, internalization looks small, and signing the welfare
wedge is the work of the fellowship." Estimating it is milestones **M3–M4**.

---

## Section budgets (PERC spec)

| # | Section | Pages | Status in `.qmd` |
|---|---------|-------|------------------|
| 1 | Purpose | 1–2 | drafted — tighten |
| 2 | Background | 3–5 | drafted — extend lit |
| 3 | Milestones | 2–3 | **empty — build from below** |
| 4 | Methods | 1–3 | **empty — build from below** |
| 5 | Data | 1–2 | drafted — add summary stats |

---

## 1. Purpose  *(1–2 pp)*

**Frame (one paragraph, already in draft — keep):**
- Strict liability assigns the cost of harm to the firm that causes it: OPA 1990,
  SMCRA 1977, CERCLA 1980, RCRA 1976.
- It under-deters when firms are judgment-proof — limited assets, corporate
  shielding, **imperfect detection** (Shavell 1986, 2007).
- Federal law layers FR requirements on top: bonding, minimum assets, or
  compulsory insurance. **How that insurance is priced** decides whether firms
  have any reason to reduce risk. ← the paper's lever.

**Setting (one paragraph — keep, tighten):**
- ~140,000 active retail UST facilities, ~550,000 tanks; LUSTs are the leading
  documented source of US groundwater contamination (EPA 2024).
- Cleanup is expensive and right-skewed: median > \$136k, mean > \$400k
  (Marcus 2021). Health damages ≈ \$17k/release and generate **no claim** —
  contamination is rarely detected in time to establish liability.
- Risk is **observable and verifiable**: age and wall construction (single vs.
  double) drive the annual release hazard.
- RCRA Subtitle I has required \$1M FR coverage since 1988. 38 states meet it
  with flat-fee public trust funds (premium does not condition on risk).
- **The experiment:** Texas closes its flat-fee fund [Dec 22, 1998 — see open
  items]; ~26,000 facilities enter risk-rated private insurance in a single day,
  premiums rising with age and wall. Driven by fund **insolvency**, not by any
  change in facility riskiness → the treatment is plausibly exogenous to risk.

**The three questions (the spine — state them as falsifiable claims):**
- **Q1 — Observability.** Is first-release risk predictable from the attributes
  an underwriter can verify? *Precondition:* if observables carry no actuarial
  content, the Texas premium schedule is noise, not signal.
- **Q2 — Behavior.** Does risk-based pricing change closure, replacement, and
  confirmed-release outcomes? *Sharp prediction from the model:* the response is
  **concentrated in old, single-walled facilities** where the premium gradient is
  steepest. A generic Texas shock would move all types uniformly — so the
  risk-profile-shaped heterogeneity *is* the identification test.
- **Q3 — Welfare.** How much of the first-best gain does risk-rated pricing
  capture, and what determines the residual? Decompose the response into a
  **price channel** (γ_price) and a **risk-internalization channel** (γ_risk);
  compare instruments (flat-fee, replacement subsidy, Pigouvian surcharge, age
  mandate) against the RB baseline. *Estimation in progress — this is the
  fellowship's core deliverable.*

**Close Purpose with the sequential logic (one sentence):** the market has the
information to price risk (Q1), pricing it changes what firms do (Q2), and the
behavioral change closes part — but, because γ_risk is small, **not most** — of
the gap to the social optimum (Q3).

---

## 2. Background  *(3–5 pp — biggest section; this is where you "earn" the reader)*

Organize as four literature threads + the contribution. Each thread = 1–2
paragraphs ending in "…and here is the gap this paper fills."

**(a) Liability theory + financial responsibility (the theoretical engine).**
- Strict liability + insurance can align private and social cost *iff* the
  insurer can price care/risk attributes (Shavell 1982, 1984, 1986, 2007;
  Polinsky 1980; Boyd & Ingberman 1997).
- Shavell's instrument-choice result is the paper's organizing idea: when risk is
  contractible, compulsory insurance (the **pricing** leg) dominates asset/bonding
  requirements because the premium itself carries the incentive; when it is not,
  insurance can be *worse* (moral hazard dilutes care).
- **Gap:** prior FR work is almost entirely the **asset/bonding** leg
  (Boomhower 2019 oil & gas bonding; Ho, Hsu, Cha & Rivera 2018). This is the
  first firm-level test of the **pure pricing** leg.

**(b) Insurance rating regimes / selection markets (the mechanism).**
- Uniform-premium state funds are the environmental analog of **community
  rating**: premiums invariant to observable risk → cross-subsidies that suppress
  precaution and crowd out risk-differentiated alternatives (Rothschild–Stiglitz
  1976; Pauly 1974; Cutler & Zeckhauser 2000).
- Moving to experience/risk rating generates two margins: **selection**
  (high-risk facilities exit or upgrade) and **moral hazard** (remaining
  facilities now face a return to risk reduction). Separating them needs
  within-facility panel variation.
- Empirical confirmation in other markets: Medicare Advantage risk adjustment
  (Brown et al. 2014; McWilliams et al. 2012); consumer credit risk-pricing
  (Einav, Jenkins & Levin 2012, 2013; Nelson 2025); selection-market IO toolkit
  (Einav, Finkelstein & Mahoney 2021; Einav, Finkelstein & Cullen 2010; Bundorf,
  Levin & Mahoney 2012; Handel, Hendel & Whinston 2015; Mahoney & Weyl 2017).
- **Gap:** rarely are both margins identified in one market; environmental
  settings almost never.

**(c) UST / environmental-damage policy (the setting's own literature).**
- Most UST work estimates **marginal damages** of leaks — hedonic and
  contamination studies (Marcus 2021; Zabel & Guignet 2012; Guignet et al.
  2013–2018; Walsh & Guignet 2017).
- **Gap:** this paper is complementary — it studies the **prevention/management
  policy**, not the damage valuation.

**(d) Closest prior work — Michigan (the comparison to beat).**
- Yin, Pfaff & Kunreuther (2007, 2011); Yin, Kunreuther & White (2011): switching
  from a community-rated fund to private coverage in Michigan cut small firms and
  reported leaks ~20%.
- **Gap:** those rely on aggregate/cross-sectional data — cannot control for
  unobserved facility heterogeneity or separate selection from moral hazard. This
  paper uses a **facility-level panel** with a matched control group and
  facility + cell×year FE (the modern environmental-liability design, cf.
  Boomhower 2019), plus a **structural welfare decomposition**.

**Contribution paragraph (write last, state plainly):**
- First firm-level empirical test of the **pricing channel** of compulsory
  liability insurance.
- Clean, near-instantaneous policy shock with a credible matched control sample.
- Pairs reduced-form causal estimates with a **dynamic structural model** that
  bounds the gap to first-best and prices counterfactual instruments that were
  never implemented.

**Dynamic-structural-under-regulation lineage (cite to place the method):**
Fowlie, Reguant & Ryan (2016); Muehlenbachs (2015); Blundell, Gowrisankaran &
Langer (2020); Kellogg & Reguant (2021).

---

## 3. Milestones  *(2–3 pp — "how will we know you did your job"; be tangible)*

> Live task list of record: **`Things to do in the Paper.md`** (repo root). This
> section organizes those into checkable milestones mapped to the three
> questions, plus a data-infrastructure track (D) that feeds all three. Mark each
> done / in-progress / remaining.

### Track D — Data & measurement infrastructure (feeds Q1–Q3)

- **D1 — Dewey linkage.** Link the facility database to the **Dewey** database to
  attach **store revenue, facility classification, and property values** to each
  facility. Backbone for revenue-into-the-model (M3) and richer HTE (M2).
- **D2 — GIS classification of every facility** (from GIS + Dewey):
  - urban / suburban / rural class;
  - local market-competition measure (rival stations nearby);
  - count of **oil/gas wells around** each facility;
  - **population within a radius** → for a correctly **population-weighted
    externality** in the welfare calc (M4);
  - Dewey GIS to bin stations into the **four** operator/location groups.
- **D3 — Claims data fix (priority).** Current claims are **aggregated, not
  individual** (a quick-and-dirty first pass) — rebuild to **claims level**, then
  file record requests to **more trust-fund states** for additional claims data.
- **D4 — Rate filings.** Follow up on the **TCEQ / TDI** rate-filing collection
  (carriers beyond Mid-Continent).
- **D5 — Data hygiene.** **SD and MO** have broken data (degenerate state FE /
  counts) — diagnose and fix; keep both on the state-check list.
- **D6 — Public leak-table micro data.** Pick this back up.

### M1 — Observability → the cross-subsidy fact (Q1)

- *Done:* CV-validated first-release hazard model (3.93M facility-years; AUC 0.80;
  cell-risk + GoF); lifetime cumulative ~1-in-4 leak fact (new S12).
- **Reframed headline deliverable:** with **claims-level** data (D3) + the hazard
  model, show **empirically how flat-fee public funds cross-subsidize risk** —
  high-risk facilities underpay, low-risk overpay — in the states where we have
  claims. Sharper Q1 payoff than the premium-vs-hazard scatter alone.
- Premium-vs-hazard / premium-vs-EL scatter stays as supporting evidence.

### M2 — Behavior / reduced form (Q2)

- *Done:* stepped DiD (closure **+1.58 pp** on 2.0%); event study (flat pre-trends,
  sharp break); vintage HTE (pre-89 **+3.1 pp**); LUST **−1.27 pp**; closure
  composition **12% → 18%** toward replacement.
- **Facility-level reduced form (open ticket — finish it).** Re-run the DiD and
  event study at the **facility** level, not tank level; confirm the result holds.
- **Stronger HTE on size**, plus HTE on **more risk dimensions** (uses D1/D2).
- **Expected-leak-risk as an outcome (Zac).** Use imputed leak risk from the
  prediction model as a DiD outcome → show Texas is **empirically safer** post
  (fleet expected-leak-risk falls), not just that tanks close.
- *Remaining inference:* wild-cluster bootstrap (state, G=18); finalize LUST event
  study; write up the conditional-on-closing endogeneity caveat.

### M3 — Structural model + revenue (Q3, stage 1)

- *Done:* the 298k-state portfolio **value solve** (T024 — correct kernel, C++
  matvec, P1 aging-backbone preconditioner). The single-tank fit is retired; the
  portfolio model makes size/portfolio native (the old 32-cell abstraction is gone).
- **Deliverable — the first portfolio estimates.** Run the v4 NPL estimator (024b,
  λ=1 flat MNL) → φ_G, γ_p, γ_r, c_rem, c_inst, κ_1 (+α_g FE); then free the nest
  λ (024f). *This is the structural milestone.*
- **Revenue into the model.** Bring Dewey-measured **store revenue** (D1) in via the
  subset-moments-into-CCP approach in `Docs/blp -- (...how to bring moments from a
  subset into CCP models...).pdf` → estimate ψ (replaces the placeholder φ_G);
  disciplines downsize/exit against real dollars.

### M4 — Counterfactuals & welfare (Q3, stage 2)

- Re-solve V + stationary fleet under CF1 flat-fee, CF2 replacement subsidy
  (s*=0.5), CF3 Pigouvian (+h·E), CF4 age mandate (25+).
- **Population-weighted externality** from the GIS population layer (D2) → a
  defensible E per release rather than a flat \$17k/\$50k.
- **Headline to pin down:** sign + magnitude of the **FF-vs-RB** social-welfare
  comparison; reconcile with the reduced-form direction; E sensitivity; optimal
  subsidy rate. *Most important and most uncertain deliverable.*

### M5 — Assemble the draft / second-workshop paper

- Convert M1–M2 into "Results"; structural + welfare into the back half;
  Conclusions on the unpriced-externality wedge. Publishable paper by Dec 31.

**Timeline (small Gantt to render):** initial workshop (M1–M2 core done, D1–D2
underway, M3 6p fit shown) → mid-residence (D3 claims fix, M2 facility-level +
size HTE, M3 revenue + size-state) → final workshop (M4 welfare) → Dec 31
(M5 draft).

---

## 4. Methods  *(1–3 pp — equations with every variable defined; threats to ID)*

State, for each piece: the equation, where the variation comes from, whether the
estimate is causal, and the threats. PERC explicitly asks for all four.

**4.1 Risk prediction (Q1) — predictive, not causal.**
- Estimand: annual first-release hazard in the never-yet-leaked risk set
  `h(t | x) = P(first confirmed release in year t | no prior release, x)`.
- Model: penalized (lasso) logistic regression; `x` = 3-year age bins fully
  interacted with wall construction, state FE, fuel and portfolio controls;
  inverse-frequency weights (0.70% base rate); 10-fold OOS CV; post-hoc
  recalibration to the empirical event rate.
- Variation: cross-sectional + within-state differences in age/wall and realized
  first releases, pre-treatment.
- Causal? **No** — this establishes actuarial content (observability), not an
  effect. `ĥ(x)` then *feeds* the structural hazard `h(a_t, W_i)`.
- Threats: detection bias (releases seen only when found), survivorship (leaked
  tanks leave the risk set), heterogeneous state reporting → state FE +
  recalibration; report GoF rather than claim unbiasedness.

**4.2 Difference-in-differences (Q2) — causal core.**
- Static: `Y_it = β·(Texas_f(i) × Post_t) + α_i + δ_ct + ε_it`.
- Event study: `Y_it = Σ_{τ≠−1} β_τ·(Texas_f(i) × 1[t−1998=τ]) + α_i + δ_ct + ε_it`.
- Variables: `Y_it` ∈ {tank closure 0/1, LUST discovery 0/1, replace-vs-exit
  composition}; `α_i` facility FE; `δ_ct` cell×year FE with
  `c = wall × fuel × capacity × install-cohort` (the dimensions the market
  prices); `Post_t = 1[t > 1998]`.
- Identifying variation: within one tank-vintage cell, in the same calendar year,
  β is the change in the Texas-vs-control closure-gap before vs. after reform.
- Causal? **Yes**, ATT under conditional parallel trends; event study tests
  pre-trends directly.
- The decisive test (write this prominently): the model predicts a
  **risk-profile-shaped** response (steepest for old SW); a generic Texas
  macro shock predicts a **uniform** shift. The observed vintage/wall gradient
  discriminates between them.
- Inference: cluster at **state (G = 18)**; **wild cluster bootstrap** (Kline &
  Santos score variant, B = 9,999, Rademacher, seed 20260519) for the few-cluster
  problem.
- Threats: few clusters (→ WCB); differential trends (→ event study); the
  replace-vs-exit composition sample is **endogenous** (conditional on closing) —
  flag it, don't over-interpret; simultaneous TX policy (→ cell×year FE absorb
  common shocks, control states absorb macro).

**4.3 Dynamic structural model (Q3) — the welfare engine.** *(portfolio "v4"
model — the single-tank fit is retired; portfolio parameters are a milestone to
estimate, M3 — no structural numbers quoted here yet)*

- **What changed and why.** The agent now tracks its **whole tank portfolio**
  (every tank by wall × age), not one representative tank. This is what makes
  premiums and hazard respond to the *actual* fleet and lets the model answer the
  age-A removal mandate and the SW-replacement subsidy. The old single-tank
  32-state abstraction mis-celled ~16% of closures; the portfolio model fixes it.
- **State** `s = (c, G)` over **~298,448 operating states**:
  - `c` = composition = counts of tanks across **16 categories** (2 wall × 8 age
    bands) holding 1–6 tanks → **74,612 distinct portfolios**
    (Σ_{N=1..6} C(N+15,15)).
  - `G` = discretized **total facility capacity** (4 bins) — scale, distinct from
    composition (counts drive risk/premium; capacity drives revenue).
  - 298,448 = 74,612 × 4 (vs the old **32**-state model — the richness *is* the
    state count: a deliberate capability/computation trade).
  - **Regime/contract indexes separate value functions** (the 17 environments
    below), not extra states inside one solve.
- **Actions = a unified intensity menu** `a = (k, m)`: remove `k` tanks (rule:
  oldest SW first, then oldest DW — 84% exact in data) and install `m` new
  **double-wall** tanks (install is always DW; no wall sub-choice; ban dates = one
  institutional sentence):
  - maintain `(0,0)`; **downsize** `(k,0)`; **replace/upgrade** `(k, m>0)` —
    *not 1-for-1* (only 13.7% are k=1; modal remove-3/install-2); **exit X**
    (absorbing). This is the answer to your "close-then-replace-or-not" question:
    the DDC handles it as one joint `(k,m)` choice set, not two stages.
- **Post-action timing (locked).** Premium and hazard are evaluated at the
  **post-action operating fleet** `n'` = `c` with `k` removed then `m` installed
  (pre-aging) → downsizing visibly cuts the premium *this* period.
- **Flow utilities** (model units = \$10k; σ=1; money out enters with an explicit
  minus; γ's are **positive** dollar→util exchange rates — v4 convention,
  replaces the single-tank signed-γ_price):
  - Work `(k,m)`: `u^W = φ_G + α_g − γ_p·P^e(n') − γ_r·H(n')·D^e − k·c_rem − m·c_inst`
  - Exit: `u^X = κ_1·N(s)`   (κ_0 := 0, location normalization)
  - `P^e(n')` = Σ n'_cell · p̄[cell, era] (RB/Texas) or τ_g·N(n') (FF/control);
    `H(n')` = **facility-level** ML hazard at (majority wall, avg-age bin) — never
    a 1−(1−h)^n union; `D^e` = deductible (TX \$5k; FF D_g, some 0). The risk term
    is **expected out-of-pocket** `H·D` — the dollar scale rests on premium **and**
    deductible.
- **Bellman:** `V(s) = max_{(k,m), X} [ u + β·E V(s') ]`, **β = 0.9957**, exit
  absorbing.
- **Calibrated outside (frozen first stage):** premium schedule `P` (empirical
  cell-mean per-tank — realized bills are *flatter* than the filed card), hazard
  `H` (Q1 model), capacity transition kernel `Γ_G` (empirical, conditioned on net
  change m−k), deductibles `D`, liability `L`.
- **Estimated inside (v4, λ=1 flat MNL):** θ = (φ_1..φ_4, γ_p, γ_r, c_rem, c_inst,
  κ_1) = **9 structural** params (enter CFs) + α_g state FE (~14, one per FF
  control state, TX=0; **Semantic-2: likelihood only, dropped at CF time**). NPL
  (Aguirregabiria–Mira; Hortaçsu–Joo 2023 eq 2.3.24–25). Free nest λ is a
  follow-on (024f).
- **Revenue enrichment (Option A — in progress, M3/D1).** φ_G is a placeholder for
  **measured revenue**: `u^W = ψ·R(G', z_ℓ, m_t) + α_g − γ_p·P − γ_r·H·D − k·c_rem
  − m·c_inst`, where `R` = measured per-period revenue (margin m_t × throughput Q
  from EIA level × inferred share; Sweeting-style), `ψ` = estimated util-per-\$
  weight. This anchors the scale in revenue and makes downsize/exit forgo
  *measured* dollars, identifying c_rem/c_inst against real money — not a free
  constant. γ_p, γ_r, ψ are estimated as **separate** dollar→util weights (how much
  firms react per premium-\$ vs expected-OOP-\$ vs revenue-\$); equality is a
  reportable restriction / optional CF, **not** the headline.
- **Computation (solved, T024).** 298k-state `(I−βM)V=R` per environment; M never
  formed (structured Kronecker matvec); P1 aging-backbone-preconditioned BiCGSTAB
  (152→10 iters). With CCPs fixed, `V` is **affine in θ**, so the NPL inner
  optimizer does **zero** solves (precompute a θ-free basis once per outer iter).
- **17 environments / one θ.** 3 RB era-cards (TX 2006/2014/2019) + 14 FF (τ,D)
  control contracts; θ **shared** across all 17 — identification is **cross-contract
  variation**, not a data split.
- **Identification (structural).** Write this the way the Intro-IO "identification
  vs. structural identification" digression asks: first the assumptions under which
  θ is correctly estimated, then a plain description of the data variation that
  pins down *each* parameter — and whether that variation is exogenous.
  - *Assumptions under which θ is correctly estimated.* **(a) Economic model:** the
    flow-utility functional form, the oldest-SW-first removal rule, install = DW,
    β = 0.9957, and a **frozen first stage** (premium schedule, hazard, capacity
    kernel Γ_G, deductible) taken as data — not estimated jointly. **(b) Error
    structure:** T1EV(σ=1) i.i.d. choice shocks, agents know their own contract and
    the transition law, flat MNL (λ=1) this milestone. (This is what a reduced-form
    paper buys with an exclusion restriction; here it is a property of the
    econometric model.)
  - *The variation that pins down each parameter — and is it exogenous?*
    - **γ_p (premium weight)** — choice-probability response to premium differences
      *holding composition fixed*. Main lever: the **Texas 1998 regime switch**
      (FF→RB; ≈\$710 matched-composition gap) + the 2006/2014/2019 re-filings;
      cross-state FF fees τ secondary. *Exogeneity:* the switch was driven by fund
      insolvency, plausibly exogenous to a facility's own risk (the DiD assumption);
      realized **cross-cell** gradients are flat and partly endogenous (optimal
      rating + selection), so γ_p **leans on the reform**, not the cross-section —
      said plainly.
    - **γ_r (expected-OOP weight)** — response to expected out-of-pocket H·D, from
      **cross-state deductible (D) × hazard** variation. *Exogeneity:* D is a
      state-contract feature; identified net of α_g and the premium, so it inherits
      the cross-contract comparability assumption.
    - **c_rem (per-tank removal cost)** — the **downsize intensity** (how many tanks
      k firms remove) against the revenue/premium dollars removal forgoes; relies on
      the measured revenue/premium being right.
    - **c_inst (per-tank install cost)** — the **replace-vs-downsize-vs-exit split**
      among closing facilities and the install count m.
    - **κ_1 (exit value × size)** — the **exit rate's gradient in portfolio size N**,
      net of operating value.
    - **φ_G → ψ (operating value / revenue weight)** — stay-vs-exit level by capacity;
      with revenue, how downsize/exit (which forgo *measured* revenue R) respond to R
      as it varies with own capacity, local demand/competition, and the fuel margin.
      *Exogeneity:* the time-series **margin** is plausibly exogenous to one facility;
      cross-sectional demand and the inferred revenue **share** are constructed, so ψ
      inherits the share-imputation assumption — identify off the revenue
      **innovation**, not the level, where possible (Sweeting).
    - **α_g** — not a structural object: a state-level **measurement** FE that absorbs
      level differences so they don't contaminate γ_p/γ_r; dropped at CF time.
    - **λ (nest, follow-on 024f)** — within- vs. across-nest substitution, off
      relative within-nest frequencies (not estimated this milestone).
- **Honesty pattern (carry into the writeup, per the digression).** State whether
  each identifying comparison is clean — *and say so when it isn't*. Weakest links
  to flag explicitly: the flat realized premium card (γ_p leans on the reform, not
  cross-cell gradients), the constructed revenue share (ψ), facility-level hazard
  (CF4's risk benefit is only as fine as the leak data), and the oldest-SW-first
  removal rule (84% exact; mean miss-cost ~\$11/yr). Each CF also carries its own
  "data-resolution" sentence in §4.4.

**4.4 Counterfactuals & welfare (Q3) — each policy re-solves the portfolio.**
- **CF1 — Texas stays flat-fee = a CONTRACT transplant** (not premium-only):
  replace **both** the premium schedule **and** the deductible with a control-state
  contract `C_ρ = (P, D)`. **Bounded, not point-identified** — run paired contracts
  (zero-fee / highest-fee / mean) to bracket it.
- **CF2 — SW-replacement subsidy:** cut the install cost, `c_inst → (1−s)·c_inst` —
  subsidize SW→DW upgrades; trace welfare vs the subsidy rate s.
- **CF3 — First-best / Pigouvian premium:** add the unpriced externality to the
  operating cost (`− γ_r·H(n')·E`, or price `H·(L+E)`) — internalizes the health
  damage no insurer prices.
- **CF4 — Age-A removal mandate:** force removal of tanks at age ≥ A (restrict the
  feasible menu to the remove-those-tanks actions).
- **Welfare accounting (PV, \$/facility):** ΔW = Δ firm surplus − Δ external damage
  − Δ govt outlay. **The wedge (re-derived):** the firm bears the **deductible D**
  (via OOP), the insurance **pool bears L−D**, and the **health externality E is
  fully external** — so even actuarially fair RB pricing internalizes only the
  priced slice. α (state FE) dropped at CF time (structural θ only).
- **Damage calibration:** L ≈ cleanup (≈\$300k pool cost; the firm sees only D);
  E ∈ {\$17k health (Marcus 2021), higher with unmeasured damages}, and a
  **population-weighted** E from the GIS layer (M4/D2) → a defensible per-release E
  rather than a flat number.
- **Honesty pattern:** each CF carries one explicit **data-resolution sentence**
  (CF1 bounded; CF2's E calibrated; CF3 leans on the estimated tank-work
  correlation; CF4 only as fine as facility-level leak data).

---

## 5. Data  *(1–2 pp — sources, summary stats, variable definitions)*

**Three collection efforts → 18 usable states** *(reconcile 18 vs. 19 — see open items)*:
- **State + EPA administrative records.** EPA LUST Finder (EPA 2017 UST
  Compendium) harmonizing state registries; tank-level install date, wall type,
  piping, leak-detection, and cleanup dispositions. 20 of 38 flat-fee states
  dropped for missing closure/install/wall fields; the 17 controls + TX have
  complete records.
- **Texas record requests.** TCEQ UST registry — uniquely granular FR insurance
  contract details and carrier-level coverage composition, ~1993–2021. Lets you
  attach a carrier and coverage to each TX facility.
- **TX insurance rate filings.** Mid-Continent Casualty SERFF filings (2006–2024)
  reconstruct the risk-based schedule at the tank level:
  `P_ijt = Base_t × ILF_it × (1 + Σ_k λ_{k,t} X_{ijt,k})`
  — `Base_t` per-tank base rate (4 filing periods); `ILF_it` increased-limit
  factor (fixed at the \$1M/\$1M federal minimum); `X` = age, wall, piping,
  leak-detection; `λ` filed credits/debits. *(Still collecting other carriers'
  manuals.)*
- **Trust-fund claims (6 of 17 control states).** Links LUST sites to insurance
  claims — a first-of-its-kind multistate LUST claim/remediation dataset.
- **ASTSWMO state-fund survey** — pins the flat-fee assessment levels for control
  states.

**Unit of analysis:** UST facility-year (~1990–2021).

**Summary-stats deliverables to drop in (most already on disk):**
- `T_Baseline_Characteristics_Slide.tex` — baseline at reform (wall, vintage,
  age, capacity), TX vs. controls.
- `Figure_cost_distribution_pooled.png` — cleanup-cost distribution (median \$136k,
  mean \$403k, fat right tail).
- `AE_X3_Premium_by_Age.pdf` — premium-by-age, TX (rising) vs. flat-fee (flat).
- `Fig_Baseline_TanksAtReform.png`, `Fig_Baseline_CapacityAtReform.png` — size:
  TX skews to 3–4-tank facilities; controls have more single-tank facilities.
- `04e_Premium_by_Year_Treatment.png` — premium divergence, flat/parallel
  pre-1998 then sharp TX break (sets up the DiD without showing it).
- `Figure_CV_CellRisk.png` (+ `_GoF`) — the age–hazard gradient by wall.

**Variable table to write (PERC asks for it):** for each of {closure, LUST
discovery, replacement, premium P, hazard ĥ, age, wall, capacity, fuel, regime}
give: what it measures, source, and how it is used (outcome / state / calibrated
input).

---

## Figure & table inventory (already on disk → drop straight in)

| Section | Artifact | Shows |
|---|---|---|
| Q1 | `Figure_CV_CellRisk.png`, `..._GoF.png` | age–hazard gradient + fit |
| Q1 | `Figure_Premium_vs_Hazard_Raw.png`, `Figure_Premium_vs_EL_Raw.png` | premiums track predicted risk |
| Data | `Figure_cost_distribution_pooled.png` | cleanup-cost distribution |
| Data | `04e_Premium_by_Year_Treatment.png` | premium divergence at reform |
| Data | `FigureA_TX_FR_Coverage_Composition.pdf`, `Figure_TX_FR_Top5_Dominance_HHI.pdf` | TX is a private, concentrated market |
| Q2 | `T_DiD_Stepped_Full.tex` | +1.58 pp headline |
| Q2 | `Fig_ES_Full.pdf`, `Fig_Vintage_Forest.pdf` | event study, vintage heterogeneity |
| Q2 | `T_HTE_DiD_Vintage.tex`, `T_LUST_Incumbent_Slide.tex`, `T2b_Enriched_Slide.tex` | heterogeneity, LUST, composition |
| Q3 | `04o_Fit_6p_SW.png`, `_DW.png`, `_AllCells_Residuals.png` | structural fit |
| Q3 | `04q_Identif_ExitShare.png`, `04q_Identif_ReplaceShare.png`, `AE09_OOP_by_Age_Pooled.png` | identification |
| Q3 | `04v_Welfare_BarChart_Clean.pdf` | counterfactual welfare components |

---

## Current numbers (reduced form only — structural is a milestone, not yet reportable)

**Reduced form (alive-at-reform, ≈9.8M tank-years) — established Q2 results:**
- Closure ATT **+1.58 pp** on 2.0% base; pre-89 vintage **+3.1 pp** (≈4×).
- LUST discovery **−1.27 pp** on 1.73% base.
- Closure composition: replacement **12% → 18%** (+53% rel.); exit volume flat.

**Structural (portfolio v4): estimation in progress — do not quote numbers yet.**
- Parameters (φ_G / ψ, γ_p, γ_r, c_rem, c_inst, κ_1; +α_g FE) and the CF1–4
  welfare decomposition are **deliverables M3–M4**, not yet reportable.
- The single-tank fits (4p / 6p) are **retired**; do not quote them as a portfolio
  result anywhere in the plan.

---

## Open items to reconcile before the workshop

1. **Reform date: Dec 22 vs Dec 23, 1998.** The `.qmd` and both decks say
   **Dec 22**; the project memory note says **Dec 23**. Pick one and use it
   everywhere. (Separately, keep three cutoffs distinct in the text: reform/treatment
   timing for the DiD, the **1999** FF→RB regime label for within-TX, and the
   **2006** premium-data window for the structural sample.)
2. **18 vs 19 states.** The Data prose says "19 states" then "18 study states,"
   and lists 19 abbreviations (AL AR ID IL KS LA MA ME MN MT NC NM OH OK PA SD TN
   TX VA). Settle the count and the list (note MO and SD flagged as degenerate
   state-FE states — SD is in the list, MO is not).
3. **Q-count framing.** The `.qmd` Purpose and TALK_EDIT both use **three** Q's
   (observability / behavior / welfare). The older REFACTORED deck used two. Use
   the three-question version consistently.
4. **Structural numbers are retired until the portfolio fit lands.** The
   single-tank fits (4p and 6p) are superseded by the portfolio v4 model — quote
   **no** structural estimate in the plan until 024b/024f produce the portfolio fit
   (M3). The plan presents the structural model as a method + a milestone, not as
   results.
5. **Welfare directional claim.** Do not assert a regime ranking in the plan; the
   FF-vs-RB welfare comparison is a deliverable (M4), not a finding.
6. **Section 4 typos in the current draft to fix when you paste prose:**
   "adminstrative," "crucurial," "natinal," "ASWATMO" → ASTSWMO, "Currelty,"
   "lnks," "faciliy."
```
