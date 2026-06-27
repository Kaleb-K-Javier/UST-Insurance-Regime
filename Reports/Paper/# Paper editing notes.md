# Paper editing notes 
### Goal is to outline paragarpy by paragary the rough setup/narrative orginzaion of the slides: Reports\Slides\04_Risk_Based_Pricing_and_USTs_TALK_EDIT.qmd Reports\Slides\04_Risk_Based_Pricing_and_USTs_TALK_EDIT.pdf 
### Reports\Slides\05_PERC_Talk.qmd and the perc goals report Reports\Paper\PERC_Research_Plan_PERC.qmd & Reports\Paper\PERC_Research_Plan_PERC.pdf
### Below is a rough outline of what I think the paper should have section wise. I am unceratin about the need for the thoertical model section but the rest are correct. Note this is for a job market paper in economies so the structures should be similar to other papers in the field.


## Introduction
 
**Cascade Order (v3):**
General Mechanism (¶1) → The Pricing Distortion (¶2) → The Setting and the Stakes (¶3) → The Mandate and the Design Choice (¶4) → The Texas Experiment (¶5) → Causal Results (¶6) → Structural Necessity Bridge (¶7) → Literature (¶8) → Roadmap (¶9)
 
**Voice:** Singular "I" throughout.
 
---
 
### ¶1 — THE GENERAL MECHANISM
 
**Top Bread (Controlling Claim):**
Compulsory liability insurance is the primary instrument through which environmental law makes hazardous firms internalize the risk of harm they cause — but whether it actually does so depends entirely on how the insurance is priced.
 
**Filling — What the paragraph must contain:**
- The logic of compulsory insurance as a deterrence instrument: strict liability makes polluters legally responsible for harm, but strict liability fails when firms are judgment-proof (assets < expected damages), when victims are diffuse, or when causation is unobservable. Financial responsibility mandates — requiring firms to carry insurance or post bonds — are the statutory response to these failures (Shavell 1986, 2004, 2007; OPA 1990, CERCLA 1980, RCRA 1976).
- The pricing condition: when firms satisfy their financial responsibility obligation through compulsory insurance, the premium becomes the operative price of environmental risk. If the premium rises with the probability of harm, the firm internalizes that risk through ongoing insurance costs. If the premium is invariant to risk, the financial responsibility mandate is satisfied on paper but delivers no deterrence.
- The two-sided observation (follow Boomhower's structure): uniform per-tank premiums ensure that hazardous firms carry coverage — that is the good. But coverage without risk pricing means the firm bears no financial cost from operating increasingly dangerous equipment — that is the distortion. The design of the premium, not the existence of the mandate, is what determines whether compulsory insurance functions as an environmental policy instrument.
**Bottom Bread (Econometric Bridge):**
This paper provides the first firm-level causal evidence on whether shifting compulsory insurance from a uniform per-tank premium to actuarially differentiated, risk-rated pricing changes what operators do with aging hazardous capital.
 
*[Transition forward: name the specific pricing distortion and its mechanism.]*
 
---
 
### ¶2 — THE PRICING DISTORTION
 
**Top Bread (Controlling Claim):**
A uniform per-tank premium sets the marginal cost of operating an additional aging, high-risk tank to zero, eliminating the financial incentive to retire it — a textbook moral hazard distortion that theory predicts and this paper confirms.
 
**Filling — What the paragraph must contain:**
- The precise statement of the distortion: under a uniform per-tank premium $\bar{P}$, the annual insurance cost is $\bar{P} \times N$ where $N$ is the number of tanks. The marginal cost of holding one more old, leak-prone tank is $\bar{P}$ — identical to the marginal cost of holding one new, low-risk tank. The operator faces no financial gradient across the risk distribution of her portfolio: $\partial C / \partial \text{age} = 0$.
- The moral hazard channel: because the premium is invariant to tank age and wall construction, the operator does not internalize the age-hazard gradient. The efficient action — retiring old single-walled tanks before they leak — is privately costless to avoid. This is the hazard suppression this paper identifies and quantifies.
- The adverse selection channel (one sentence, not the primary focus): uniform per-tank premiums cross-subsidize high-risk facilities at the expense of low-risk ones, which crowds risk-rated private alternatives out of the market and concentrates the riskiest operators in the pooled coverage — the environmental analog of the community-rating problem identified by Pauly (1974) and Rothschild and Stiglitz (1976).
- The theoretical fix: Shavell (1986, 2004, 2007) establishes that compulsory insurance restores deterrence if and only if premiums price observable risk-relevant characteristics. For underground petroleum storage tanks, tank age and wall construction type are exactly those characteristics — both are verifiable by an underwriter at policy inception, and both are strong predictors of release probability.
- The open empirical question: whether risk-rated pricing in practice disciplines high-risk operators through deterrence, drives them out through adverse selection, or some combination of both, has not been identified at the facility level.
**Bottom Bread (Econometric Bridge):**
Separating these margins and quantifying their welfare consequences requires a setting where the transition from uniform to risk-rated pricing can be observed for a large, well-measured population of facilities — a setting Texas provides.
 
*[Transition forward: establish the setting and the stakes.]*
 
---
 
### ¶3 — THE SETTING AND THE STAKES
 
**Top Bread (Controlling Claim):**
Underground petroleum storage tanks are the dominant source of groundwater contamination in the United States, the harm they cause is well-documented and costly, and the regulatory system — not private tort law — is the only operative instrument for controlling it.
 
**Filling — What the paragraph must contain:**
- Scale: approximately 140,000 active retail facilities and 550,000 active tanks nationwide (EPA 2024); approximately 1 in 4 facilities leaks over a 30-year operational life.
- Harm and cleanup cost: approximately 550,000 confirmed releases since 1988; median cleanup cost \$136,249, mean \$403,319 per site (author's trust-fund claims data, $N = 12{,}541$ sites across 6 states); right tail dominates expected loss.
- Health externality: benzene and petroleum migrate silently into groundwater; contamination reaches drinking-water wells; health damages approximately \$17,000 per release (Marcus 2021) — but these damages generate no insurance claim.
- Why tort law cannot reach it: contamination is slow, silent, and diffuse; by the time a plume is detected it may have traveled hundreds of feet; causation is hard to establish; victims are dispersed and often unaware. Private tort law does not operate here. The regulatory system is the only instrument.
- Why the harm is structurally externalized even under compulsory insurance: actuarial premiums price what insurers can claim — cleanup costs that flow through insurance. They cannot price the health externality $E$ because it generates no third-party claim. Even a perfectly designed risk-rated premium leaves $E$ uninternalized. This is why the welfare gap between risk-based pricing and the first-best Pigouvian instrument persists even after the reform.
**Bottom Bread (Econometric Bridge):**
This combination — predictable risk, compulsory coverage, externalized harm, and a regulatory instrument whose pricing design is a state-level policy choice — makes USTs the ideal laboratory for studying compulsory insurance design.
 
*[Transition forward: describe the mandate architecture and the design choice.]*
 
---
 
### ¶4 — THE MANDATE AND THE DESIGN CHOICE
 
**Top Bread (Controlling Claim):**
Federal law mandates that every UST operator carry financial responsibility coverage, but the mandate is silent on pricing design — and 38 states have satisfied it through uniform per-tank premiums that eliminate the risk-reduction incentive the mandate was intended to create.
 
**Filling — What the paragraph must contain:**
- The mandate: RCRA Subtitle I (1988) requires all UST operators to demonstrate \$1M minimum financial responsibility coverage. Compliance is not optional; the mandate is enforced through operating permits.
- The design choice: federal law leaves the pricing structure to states. States have chosen one of three approaches: no state fund (private market only), a uniform per-tank state trust fund, or risk-rated private insurance. Thirty-eight states chose the uniform per-tank trust fund model.
- What uniform per-tank pricing means in practice: every covered facility pays the same annual per-tank assessment regardless of tank age, wall type, or leak history. Operators are in full regulatory compliance. But compliance is decoupled from risk reduction — the mandate is satisfied without the deterrence.
- The policy relevance beyond Texas: this is not a historical curiosity. More than a dozen of the 38 fund states carry statutory sunset dates clustering through this decade (Minnesota 2022; Colorado 2023; Kansas/Nebraska 2024; Illinois, Missouri, New Hampshire 2025; California, South Carolina 2026). As these funds close, their facilities will face exactly the transition Texas underwent in 1998. This paper measures the welfare consequences of that transition.
**Bottom Bread (Econometric Bridge):**
Texas provides the clean identification of those consequences: its uniform per-tank fund closed overnight on December 22, 1998, for reasons unrelated to the riskiness of any individual facility, moving the entire state into risk-rated private insurance in a single day.
 
*[Transition forward: describe the experiment.]*
 
---
 
### ¶5 — THE TEXAS EXPERIMENT
 
**Top Bread (Controlling Claim):**
On December 22, 1998, the Texas Petroleum Storage Tank Fund closed due to aggregate fiscal insolvency, and approximately 26,000 active facilities entered risk-rated private insurance overnight — an externally forced, institution-driven shock to insurance pricing whose exogeneity comes from the fund's balance sheet, not from any change in Texas facility risk profiles.
 
**Filling — What the paragraph must contain:**
- Source of exogeneity stated precisely: the fund's insolvency was driven by aggregate claim volume across the entire fund, not by the riskiness of any individual facility. Facility-level risk characteristics — tank age, wall type, installation cohort — were unchanged at the reform date. The price of risk changed; the underlying risk did not.
- The pre-/post price signal contrast: before December 22, 1998, Texas facilities faced a uniform per-tank premium with $\partial P / \partial \text{age} = 0$. After, premiums are actuarially differentiated — they rise with tank age, wall construction type, and piping technology. The premium schedule closely tracks the actuarial risk gradient (details in Section 3).
- The control group: 17 states that retained uniform per-tank coverage throughout the study window and have complete administrative records on closure dates, installation dates, and wall construction — the exact characteristics the private market prices.
- The panel: 9.8 million tank-years, 1990–2021, across 18 study states.
- Identification strategy (one sentence): I estimate the causal effect using a difference-in-differences design with facility fixed effects and cell-by-year fixed effects, where cells are defined by wall type, fuel type, capacity, and installation cohort — the dimensions the private market prices and the dimensions along which the treatment effect should be heterogeneous.
**Bottom Bread (Econometric Bridge):**
The design delivers a within-cell, within-facility, before-and-after comparison of Texas facilities against observationally identical control-state facilities that never left uniform per-tank coverage.
 
*[Transition forward: state the results.]*
 
---
 
### ¶6 — THE CAUSAL RESULTS
 
**Top Bread (Controlling Claim):**
Risk-based pricing raises the annual tank closure rate by $\hat{\beta} = +1.58$ percentage points (s.e. = 0.40) on a pre-reform control mean of 2.0% — a 79% relative increase — and the effect concentrates precisely where the pricing theory predicts: among old, single-walled tanks where the risk-rated premium gradient is steepest.
 
**Filling — What the paragraph must contain:**
 
*Observability precondition (one sentence):* A penalized logistic regression on 3.93M pre-treatment facility-years confirms the precondition: the annual first-release hazard for single-walled tanks rises from 5.3 per 1,000 facility-years in the youngest age bin to 12.3 per 1,000 in the oldest, while remaining flat for double-walled tanks — observable characteristics carry strong actuarial content, validating the premium gradient.
 
*The four behavioral results:*
- **Closure ATT:** $\hat{\beta} = +1.58$ pp (s.e. = 0.40) on a 2.0% baseline, facility FE + cell×year FE. Event-study pre-trends flat, with $rel\_year = -1$ the omitted reference; sharp break at $rel\_year = 0$; sustained elevation through $rel\_year = +22$.
- **Vintage heterogeneity — the falsification test:** Pre-1989-vintage single-walled tanks show an ATT of $+3.1$ pp; post-1988 tanks show $+0.6$ pp — a 5× differential. A coincident Texas-wide economic shock predicts uniform closure increases across all tank types. The theory predicts concentration among old single-walled tanks where the premium gradient is steepest. The data match the theory, not the alternative.
- **Leak discovery:** Annual LUST discovery falls $-1.27$ pp (s.e. = 0.32) on a 1.73% base, with both routine-monitoring and closure-inspection channels declining — demonstrating that tank retirement, not strategic non-reporting, drives the environmental improvement.
- **Closure composition:** Among closing facilities, the replacement share rises $+6.3$ pp (s.e. = 1.3) and the permanent-exit share falls $-6.2$ pp (s.e. = 1.2) — risk-based pricing induces fleet upgrading, not only market contraction.
**Bottom Bread (Econometric Bridge):**
These results establish that risk-based pricing changes firm behavior and identify where the effect is largest; translating them into welfare-comparable magnitudes and evaluating counterfactual policies requires the structural model.
 
*[Transition forward: structural necessity bridge.]*
 
---
 
### ¶7 — STRUCTURAL NECESSITY BRIDGE
 
**Top Bread (Controlling Claim):**
The difference-in-differences design identifies the average causal effect of the 1998 reform, but three welfare-relevant questions lie structurally beyond its reach.
 
**Filling — What the paragraph must contain:**
Three gaps, each one sentence:
 
1. **Dollar-denominated welfare:** The DiD delivers a closure-rate gap in percentage points. Comparing alternative policies requires present-value welfare in dollars — a translation the reduced form cannot make.
2. **Counterfactual policies:** A replacement subsidy and an age mandate were never implemented in Texas. Their behavioral and welfare consequences are unobserved in any reduced-form design and require model-based simulation.
3. **The uninternalized externality wedge:** Actuarial premiums price the cleanup cost component that flows through insurance claims. The health externality $E \approx \$50{,}000$ per release generates no insurance claim and is therefore structurally unpriced even under risk-rated coverage. Quantifying how much of the first-best welfare gain risk-based pricing fails to capture requires the structural primitives — specifically the estimated price-sensitivity parameter $\hat{\gamma}_p = -2.163$ (s.e. = 0.205) and the risk-internalization parameter $\hat{\gamma}_r = +0.114$ (s.e. = 0.005), where $\hat{\gamma}_r \ll \hat{\gamma}_p$ is itself the quantitative statement of the residual wedge.
- State the regime ordering: $a^*_{\text{SOC}} < a^*_{\text{RB}} < a^*_{\text{UP}}$, where $a^*$ is the optimal tank-removal age threshold under each regime. Risk-based pricing moves behavior toward the social optimum; the unpriced externality $E$ prevents it from closing the gap.
**Bottom Bread (Econometric Bridge):**
I estimate a dynamic discrete-choice model of facility-level tank portfolio management across a state space of 298,448 portfolios by Nested Pseudo-Likelihood (Aguirregabiria and Mira 2007), and use the recovered parameters to re-solve firm behavior and compute welfare under four counterfactual policy instruments.
 
*[Transition forward: literature.]*
 
---
 
### ¶8 — LITERATURE POSITIONING
 
**Top Bread (Controlling Claim):**
This paper contributes to three literatures: the theory and empirics of environmental liability and financial responsibility, the empirical literature on hazardous capital stock management under financial assurance requirements, and the industrial organization of insurance markets and welfare decomposition.
 
**Filling — One to two sentences per strand. Position, do not summarize.**
 
- **Liability, financial responsibility, and UST policy:** Shavell (1986, 2004, 2007) is the theoretical foundation for compulsory insurance as a deterrence instrument. Yin, Kunreuther and White (2011); Yin, Pfaff and Kunreuther (2011, 2007) study UST fund design theoretically and with Michigan aggregate data. *This paper provides the first facility-level causal identification of how insurance pricing design — uniform versus risk-rated — changes environmental behavior, with a clean policy shock, within-facility panel identification, and a structural welfare decomposition.*
- **Hazardous capital stock management:** Boomhower (2019) finds that bonding requirements change well retirement and industry composition in oil and gas. Armitage (2026) documents judgment-proof dynamics in aging well transfers. *This paper differs because the behavioral margin here is almost entirely the extensive margin of tank removal — wall-type retrofitting is prohibitively costly under 40 CFR Part 280 grandfathering rules, a structural feature of the market documented in the data: only 0.003% of active Texas facilities ever retrofitted tank construction prior to terminal closure. The clean extensive margin makes identification sharper and the structural model simpler.*
- **IO of insurance markets and welfare decomposition:** Einav, Finkelstein and Cullen (2010); Einav, Finkelstein and Mahoney (2021); Bundorf, Levin and Mahoney (2012). A uniform per-tank premium is the environmental analog of community rating in health insurance — premiums invariant to observable risk create cross-subsidies and suppress efficient sorting. *This paper applies the welfare-decomposition logic from the health insurance selection literature to an environmental liability context: the uninternalized wedge here is not adverse selection on coverage but the health externality that no actuarial premium can price.*
- **UST damages literature:** Marcus (2021) estimates groundwater health damages from UST leaks; Guignet (2014, 2018), Walsh (2017), and Zabel (2012) estimate hedonic property-value impacts; Ho, Hsu, Cha and Rivera (2018) study UST remediation. *This paper is complementary: it studies the design of the insurance instrument meant to prevent the damages those papers measure.*
**Bottom Bread (Econometric Bridge):**
The paper's contribution is the combination — credible causal identification of behavioral responses and a structural welfare decomposition that places those responses on a policy-comparable scale — that neither the liability literature nor the hazardous capital literature has previously delivered in this setting.
 
*[Transition forward: roadmap.]*
 
---
 
### ¶9 — ROADMAP
 
**Top Bread:** Navigational only. Three to four sentences in prose.
 
**Filling:**
- Section 2: Regulatory setting and institutional background — qualitative defense of identification.
- Section 3: Data construction — three administrative sources, sample construction, key descriptive facts.
- Section 4: Reduced-form evidence — hazard model, DiD and event study on closure / leaks / composition, vintage heterogeneity as the primary falsification test.
- Section 5: Structural model — portfolio state space, nested logit payoffs, NPL estimation, parameter estimates and identification.
- Section 6: Counterfactual welfare — four policy comparisons and welfare decomposition.
- Section 7: Conclusion.
**Bottom Bread:** None required.
 
---
# PAPER SPINE (new draft)

**Flow source:** non-appendix slides of `Reports/Slides/04_Risk_Based_Pricing_and_USTs_TALK_EDIT.qmd`
(same flow as `PERC_Research_Plan_PERC.qmd` and `PERC_Reserach_Plan.qmd`).
**Structural section model file:** `Reports/Paper/PERC_Reserach_Plan.qmd` §"Dynamic Structural Model" (6-param).
**Numbers:** [placeholder] everywhere until the outline is locked, then fill.

**D1 resolved:** NO standalone theory/conceptual-framework section, AND no analytical/toy model at
all (user: "analytical model -> don't include"). The "response concentrates in old single-walled
tanks" prediction is motivated verbally in §4 from the premium-gradient logic (Shavell) — not from a
formal proposition. The welfare-accounting identity lives in §6 (structural).

**Style benchmarks:** Russo (Conservation Auctions) + Boomhower 2019 — lean Data with run-in heads,
unit defined inline, exact counts, strengths+limits per source, construction -> appendix; Results go
raw/non-parametric -> equation-first -> enumerated facts -> magnitude+interpretation -> robustness.

**Locked conventions:** singular "I"; headline ATT = +1.58 pp (s.e. 0.40) Col 4 (facility FE +
cell×year FE); event-study reference k = -1 (omit the year before the reform); 1995 LUST-Finder migration = administrative
artifact, kept separate from physical closures; precise capital-asset language (no "zombie tanks").
**State list (canonical):** TX + 17 controls = AL, AR, ID, IL, KS, LA, MA, ME, MN, MT, NC, NM, OH,
OK, PA, SD, TN, VA. (Old CO/KY/MD/MO dropped; AL/IL now in.)

| § | Section | Talk slides | Function |
|---|---------|-------------|----------|
| 1 | Introduction | 1–2 | Cascade ¶1–9 (drafted above) |
| 2 | Setting / Institutional Background | 3–10 | Qualitative defense of identification |
| 3 | Data | 11 | Lean: sources, 18 states, panel, descriptives |
| 4 | Reduced-Form Evidence | 12–22 | Observability + pricing + DiD + results |
| 5 | Dynamic Structural Model | 23–32 | PERC_Reserach_Plan 6-param version |
| 6 | Estimates, Counterfactuals & Welfare | 33–36 | Estimates + CF1–4 + welfare accounting |
| 7 | Conclusion | 37 | Takeaways |
| App | Appendix — Robustness & Data detail | — | Tank-level Cox, matched sample, inference, sample funnel |

---

## Section 2 — Setting / Institutional Background  [§ qualitative ID defense]
*Talk 3–10, ending at the reform. Boomhower §I model: asset -> risk mechanism -> market failure ->
policy instrument -> the specific shock. Every paragraph validates the design. Short direct sentences,
plain words. Numbers bracketed/[cite] until checked.*

#### ¶1 — What a tank is, and why the wall is fixed  [DRAFTED]
An underground storage tank holds motor fuel at a gas station or a commercial site. It sits in the
ground for decades. A single-walled tank is a bare shell. A double-walled tank adds a second wall and
a monitored space between the two. Single-walled tanks are cheaper to put in and they leak more. The
wall is set when the tank goes in the ground. To change it, the operator has to dig the tank out and
install a new one, which costs about as much as starting fresh. In the Texas data, only 0.003% of
active facilities ever changed a tank's wall before closing it. The wall is a fixed trait, not a dial
an operator turns.
*Goal: the asset, and that wall type is fixed and observable (irreversible at install).*
*Pull: install cost ≈ \$23k SW vs \$39k DW (GAO 1987) [cite]; retrofit share 0.003% (TX registry 1998–2021); SW/DW definition (40 CFR Part 280).*

#### ¶2 — Closing a tank is the only real lever  [DRAFTED]
An operator who faces a higher price for risk has few ways to respond. The cheap risk-reduction steps
were already done. Federal deadlines in 1993 and 1998 required leak detection, spill protection, and
corrosion control, and almost every tank met them by the reform. A partial wall upgrade does not help.
Under 40 CFR Part 280, any such change reclassifies the whole facility to current standards, and that
cost dwarfs any premium saving.[^vdr] Replacing a tank with a new double-walled one works but is expensive,
about \$55,000 a tank, and rare. That leaves one lever that actually cuts risk: pull the old tank and
close it. The hazard is already in the ground, and it comes out only when the old tanks do. So the
margin I study is closure, and the policy question is how to price keeping an old tank in service.
*Goal: why closure is the only behavioral margin (other actions exhausted or foreclosed; stock-management framing).*
*Pull: 1993/1998 federal deadlines; 40 CFR Part 280 reclassification trigger; DW replacement ≈ \$55k/tank [cite].*
*[^vdr]: This is vintage-differentiated regulation (Gruenspecht 1982, Stavins 2005). Exempting existing
capital from new standards extends the life of the oldest and most hazardous stock, because any
modification triggers the current standards and that cost dwarfs the gain. It is the same mechanism as
New Source Review in the Clean Air Act, where the threat of reclassification deterred investment at
grandfathered plants (Bushnell 2012, Heutel 2011). When new-capital rules cannot reach the existing
stock, retirement-focused instruments are the right complement (Sallee 2023, Jacobsen 2023).*

#### ¶3 — The typical site  [DRAFTED]
A typical site is one gas station with a handful of tanks. The country has about [140,000] active
facilities and [550,000] active tanks [cite EPA]. Sites differ in the fuel they sell and in how many
tanks they run. I use both later to see who responds to the reform.
*Goal: describe the typical site, and the variation in fuel and size that the §4.5 cuts use. (Operator-
level claims removed — not verified.)*
*Pull: facility/tank counts -> [cite EPA UST program]. TODO (coder session, later): characterize the
typical Texas operator from the operators data — rough measure only, not now.*

#### ¶4 — Risk is observable  [DRAFTED]
The risk a tank carries is not hidden. It rises with two things anyone can read off the tank: its age
and its wall. Old tanks leak more than new ones. Single-walled tanks leak more than double-walled ones.
An insurer can check both when it writes the policy. So it can charge a riskier tank more. This is the
fact the paper turns on, and I show it in §4.1.
*Goal: establish that risk is observable and priceable (age, wall) — the precondition for §4.1/§4.2.*

#### ¶5 — Why regulation, not the courts  [DRAFTED]
A leak is hard to catch and hard to pin on anyone. Fuel seeps into soil and groundwater. It can travel
for years before a drinking-water well or a basement reveals it. By then the source is often unclear,
and the people harmed are spread out and may not know. Private lawsuits cannot do much here. So the job
of preventing leaks falls to regulation, not to the courts.
*Goal: the harm is diffuse, so tort cannot reach it and regulation is the instrument.*

#### ¶6 — A leak costs two ways  [DRAFTED]
A leak creates two kinds of cost. The first is cleanup, digging out and treating the contaminated soil
and water. Cleanup is expensive, with a median around [\$136k] and a mean around [\$403k] per site
[cite claims data]. The second is harm to people nearby. Marcus (2021) shows that UST leaks damage
infant health, lowering birth weight among births near a leak [confirm exact outcome and estimate].
Insurance pays for cleanup, because cleanup files a claim. It does not pay for the health harm, because
that harm files no claim. So even a fairly priced premium leaves the health cost uncovered. That gap is
the wedge I return to in §6.
*Goal: split the harm into cleanup (insurable, L) and the health damage (uninternalized, E) — the welfare wedge.*
*Pull: cleanup median/mean -> claims data; health outcome -> Marcus 2021. Cite her causal health
estimate (the birth outcome), NOT a priced-damage dollar — the outcome is the real causal result.*

#### ¶7 — The mandate and the three ways states price it  [DRAFTED]
Federal law requires every tank operator to carry financial responsibility for cleanup. The rule is
RCRA Subtitle I, in force since [1988], with a minimum of [\$1 million] in coverage. Federal law does
not say how to price it. States chose one of three paths. Some let operators buy risk-rated private
insurance. Some set up a state trust fund that charges every tank the same flat fee. Some did neither.
Thirty-eight states ran a flat-fee fund. A flat fee meets the mandate but prices no risk. An old
single-walled tank pays the same as a new double-walled one.
*Goal: the federal mandate plus the three state designs, and that a flat fee prices no risk.*
*Pull: RCRA Subtitle I date + $1M minimum [cite]; count of flat-fee states.*

#### ¶8 — The Texas reform  [DRAFTED]
Texas ran a flat-fee fund until it ran out of money. On December 22, 1998 the state closed the fund
because it was insolvent, and about [26,000] facilities moved to risk-rated private insurance at once.
The fund failed for a budget reason, not because any one facility got riskier. So the price of risk
changed for reasons outside any operator's control. That is the shock I use. Texas went from a flat fee
to a risk-rated premium overnight, while the flat-fee control states did not.
*Goal: the exogenous shock — fund insolvency flips Texas from flat fee to risk-rated; controls do not.*
*Pull: reform date Dec 22 1998; facilities moved (~26,000) [cite TX fund records].*

## Section 3 — Data  [§ lean replication blueprint]
*Talk 11. Style = Boomhower/Russo: run-in heads, unit inline, exact counts, strengths+limits per
source, construction -> appendix. Closed variable loop for §3/§4 only; structural first-stage inputs
(revenue, hazard, deductible, transitions, removal rule) documented in §5, not here.
Paragraphs drafted as flowing prose; [placeholder] = number to fill once the outline is locked.*

**Opening (1 sentence).**
I construct a facility-year panel of the underground storage tank industry in Texas and 17 flat-fee
control states, 1990–2021, by merging four sources: a harmonized national tank registry, state
leak-incident records, Texas insurance-pricing data, and location and market measures from the 2000
Census and facility geography. Appendix [X] documents the construction.

### ¶1 — The panel  [DRAFTED]
The backbone of the panel is the EPA's LUST Finder, which harmonizes state UST registries into one
national schema [@EPA2017USTCompendium]. My unit of observation is the facility-year. A facility is a
retail or commercial site with a unique state registry number that operates one or more underground
tanks. I follow each facility from its first tank installation to its last closure or to the end of
2021. The panel covers 117,250 facilities across 18 states.
*Goal: define the panel and its unit of observation.*

### ¶2 — Study states and analysis sample  [DRAFTED]
The registry has gaps in the fields a causal design needs. Of the 38 states with a uniform-premium
trust fund, I keep the ones with complete closure dates, install dates, and wall type. That drops 20
states and leaves Texas and 17 controls. My sample is the tanks and facilities that were open on the
reform date, December 22, 1998. I follow each one forward, year by year. A tank that closes later
counts as a closure, not a dropped observation. So closure and exit are outcomes I measure, not cases
the sample loses. I match Texas units to control units so I compare like with like. The match is
coarsened exact matching. At the tank level I match on make-model and install year. At the facility
level I match on install year, facility size, and single-wall share. The cell-by-year fixed effects in
§4 control for these traits again.
*Goal: state which units are in the sample (active-at-treatment incumbents, birth-CEM matched) and how they are matched.*
*EDITOR NOTE (state-list membership, user to revisit): the count is settled by the verified headline
sample (18 states = TX + 17 controls, 20 of 38 fund states dropped). The exact membership is not: the
reduced-form code (01n) keeps CO (19 controls in the CV model), the PERC write-ups drop CO. Confirm
whether CO is one of the 17 RF controls.*

### ¶3 — Tank attributes  [DRAFTED]
Each registry record is a physical tank. It reports the tank's install date, closure date, wall type
(single-walled or double-walled), stored fuel, and capacity. These attributes set the leak hazard and
the insurance premium. They also define the make-model cells used in §4. From them I build each
facility's stock variables: active tank count, total capacity, average tank age, and single-walled
share. I define two fuel cuts for later. A gas station is any facility with a gasoline tank. A
motor-fuel-only facility is one whose tanks are all motor fuel. I also sort facilities into four size
groups by total capacity, G1 to G4.
*Goal: introduce the priced tank attributes, the fuel cuts, and the size bins used in §4.*

### ¶4 — Closure-side outcomes  [DRAFTED]
My primary outcome is tank closure. It equals one in the year a tank is permanently removed. At the
facility I track the whole portfolio. A facility can downsize, which means it closes some tanks but
stays open. It can also cut capacity, replace tanks, or exit. I split each closure into two kinds. A
replacement is a closure where the facility puts in a new tank. A permanent exit is a closure where it
does not. I use the facility's full install history to tell them apart, so I do not count a replacement
as an exit.
*Goal: define the closure and portfolio-margin outcomes at the tank and facility level.*

### ¶5 — Leak discovery  [DRAFTED]
Leak discovery comes from the state LUST incident files. Each record gives a confirmed release by
facility and report date. I split discoveries by how they are found. Some show up in routine
monitoring. Others show up when a tank is closed and inspected. I drop states without usable release
dates when I pick the study states. So I measure leak discovery for every state in the panel and do not
need a separate leak sample. One note on the dates. The 1995 move of state records into the LUST Finder
bunched install and closure dates near 1995. That bunching is an administrative artifact, not real
activity. I treat those dates as artifacts, and I set the event-study reference year ($rel\_year = -1$)
after it.
*Goal: define the leak outcome, its detection channels, and the 1995 hygiene note.*

### ¶6 — Location and market measures  [DRAFTED]
For the heterogeneity analysis I attach measures fixed at the reform that do not move with the policy.
From the 2000 Census I classify each facility as rural, low-population, low-income, or high-poverty.
From facility geography I build a market-thinness measure for gas retailers, the count of active
competitors within one mile in 1998. Because all of these are set before or at the reform, they do not
pick up the reform's own effect on the market.
*Goal: introduce the location and market covariates used in the §4.5 heterogeneity, fixed at treatment.*

### ¶7 — Insurance-pricing data  [DRAFTED]
The insurance data let me measure the price each facility pays for risk. For Texas I collected the
filed rate manuals of the largest UST insurer in the state. I obtained them through public-record
requests to the Texas Department of Insurance. I link these manuals to facilities through the Texas
Commission on Environmental Quality (TCEQ) database. That database records each facility's insurance
contract for about the past twenty years. Texas is unusual in keeping this detail. It is what lets me
rebuild an annual premium for each Texas facility. For the control states I use the ASTSWMO survey of
state-fund fees. The survey gives the flat per-tank fee each fund charged. The premium data have one
main limit. The Texas manuals cover the largest carrier. So the premium is the price under that
carrier's schedule, not the price across all carriers. I also collected trust-fund claims data from 6
of the 17 control states. These records link LUST sites to their cleanup claims. They are the basis
for the cleanup-cost distribution I show below.
*Goal: introduce the insurance-pricing data (TX premiums, control fees, multistate claims) and state the single-carrier limit.*

### ¶8 — Summary statistics  [DRAFTED]
Table [1] reports summary statistics for the study facilities at the reform date, with Texas and the
control states in separate columns. Panel A describes the capital stock. Texas facilities are larger
on average. They hold more three- and four-tank sites, while the control states hold more single-tank
sites (Figure [baseline-tanks]). Texas capacity is also shifted toward the larger bins (Figure
[baseline-capacity]). The average Texas facility holds [N] tanks and [N] gallons of capacity, against
[N] tanks and [N] gallons in the controls. Panel B reports pre-reform annual rates. The closure rate
is [N] percent and the leak-discovery rate is [N] percent, and the two groups track each other before
the reform. Figure [cost-distribution] shows the distribution of cleanup costs across LUST sites in
the six claims states. The median cleanup cost is [\$N] and the mean is [\$N], which reflects a long
right tail.
*Goal: walk the summary-statistics table — Texas larger at the reform, the two groups parallel on pre-reform outcomes.*

## Section 4 — Reduced-Form Evidence  [§ design-based causal core]
*Talk 12–22. Non-parametric variation before parameterized regressions. SE clustering stated in the
body. The vintage/wall risk-concentration cut is the falsification (§4.4 ¶3), run outside the cell×year
FE. No toy-model references. Each [N] placeholder is followed by a "Pull:" line naming the source
file/field. CV/observability sources are the outputs of Code/Analysis/Descrptive Facts/01n_CVValidation.R.*

### ¶0 — Roadmap  [DRAFTED]
This section presents the reduced-form evidence in five steps. I first show that the traits an insurer
can observe predict releases (§4.1). I then show how the Texas market prices those traits, and how the
Texas premium pulled away from the flat control fee at the reform (§4.2). I then state the design
(§4.3). Section 4.4 asks whether the policy worked: the average effect on tank closure, its timing, and whether
leaks fell. Section 4.5 asks who it worked on, where, and what facilities did in response.
*Goal: tell the reader the five steps, and the "does it work, then who/where/what" arc.*

### 4.1 — Observable risk and the age-hazard gradient

*Format note: one clear goal per paragraph (italic Goal line). Keep distinct goals in separate
paragraphs (do not merge "validates out of sample" with "recovers the risk map"). Pull lines tag
source files. No per-unit rates in prose (percent only); AUC is a supporting clause, not the headline.*

#### ¶1 — Risk rises with observable traits  [DRAFTED]
Risk-based pricing can only change behavior if the traits an insurer sees at underwriting actually
predict releases. They do, and the clearest case is wall construction crossed with age. Figure
[cell-risk] shows the chance a facility has a first confirmed release in a year, by tank age and wall
type. For single-walled facilities that annual chance climbs with age, from about [X] percent in the
youngest tanks to about [X] percent in the oldest. For double-walled facilities it stays low and
nearly flat, near [X] percent, across the same ages. The two pull apart as tanks get older. The
lifetime picture is starker. Over a 30-year life about [X] percent of single-walled facilities leak
at least once, against about [X] percent of double-walled facilities. Risk is not a surprise that
arrives at random. It is written in traits an underwriter can read when the policy is written.
*Goal: show release risk rises with traits insurers can observe (gradient and lifetime magnitude).*
*Pull: annual release chance by age x wall -> Output/Tables/Table_CV_CellRates_Observed.csv (obs_per1k
by has_single_walled, age_bin; percent = obs_per1k/10); fig -> Output/Figures/Figure_CV_CellRisk;
lifetime % SW vs DW over 30 yr -> Output/Tables/Table_CV_LifetimeLeakProb.csv (lifetime_leak_prob by
group, horizon=30).*

#### ¶2 — The model validates out of sample  [DRAFTED]
A single trait gradient is suggestive, but a premium rests on all the traits together, and on whether
the pattern holds for facilities the model has not seen. So I estimate each facility's annual
first-release probability from its full set of observable traits, then check the predictions out of
sample. The model answers the underwriter's question. For a facility that has never leaked, what is the
chance of a first release in the coming year? It uses only what an underwriter can see at the start of
the year. The traits are wall construction, tank age, portfolio size, total capacity, and fuel type,
with a state baseline. I fit the model on pre-reform years only, and every facility's predicted risk is
scored by a model fit on data that left it out, so nothing here is in-sample fit. Two facts show the
predictions are usable. Risk concentrates: the [10] percent of facilities the model ranks highest hold
[X] percent of next-year releases, about [Z] times what random screening would catch (Figure [lift]).
And the predictions are right in levels, not just in rank: average predicted risk tracks the observed
release rate across the whole range of the score (Figure [calibration]).
*Goal: show the full-trait model predicts releases out of sample (concentration and calibration).*
*Pull: top-10% lift + multiple -> Output/Tables/Table_CV_GoF_Summary.csv (Top10_pct_events; Z =
Top10_pct_events/10); lift fig -> Output/Figures/FigureA_CV_Lift_State; calibration fig ->
Output/Figures/Figure_CV_Calibration; sample N/events/base rate -> Table_CV_GoF_Summary.csv
(N_fac_years, N_events, Base_rate_pct).*

#### ¶3 — The model recovers the risk map insurers price  [DRAFTED]
The model does more than rank facilities. It reproduces the level of risk in each cell the market
actually prices on. When I compare its predicted release chance in each age-and-wall cell to the raw
chance in that cell, the points fall along the 45-degree line (Figure [cell-gof]), so the model
recovers the risk map rather than just sorting facilities along it. It also separates leakers from
non-leakers well out of sample (ROC AUC [AUC], precision-recall AUC [PR_AUC]). This predicted
probability is what a risk-based premium puts a price on. I reuse the same facility-level hazard as an
input to the structural model in §5. The lesson for the reform is plain. An
insurer can tell risky facilities from safe ones in advance, so a premium can be made to rise with
risk, and a flat per-tank fee chooses not to.
*Goal: show the model recovers the priced risk map (not just a ranking), and hand to §4.2.*
*Pull: cell-level fit fig -> Output/Figures/Figure_CV_CellRisk_GoF; AUC + PR-AUC ->
Output/Tables/Table_CV_GoF_Summary.csv (AUC, PR_AUC, Primary row).*

### 4.2 — Pricing observable risk

#### ¶1 — How Texas premiums are built  [DRAFTED]
After the fund closed, Texas facilities bought coverage from private insurers, and I observe what they
paid. I reconstruct each facility's annual premium from the filed rate manuals of the state's largest
UST insurer, linked to the facility through the TCEQ contract records. The schedule prices the same
traits that predict releases. The per-tank premium rises with tank age, and at any given age it is
higher for single-walled tanks than for double-walled tanks (Figure [premium-by-age]). In the control
states, where the public fund still sets the price, I use the ASTSWMO survey of fund fees. Those funds
charge a flat amount per tank that does not move with age or wall type.
*Goal: show how the Texas premium is built and that it rises with the risk traits, while the control
fee is flat.*
*Pull: TX premium by age x wall -> Output/Tables/AE_X3_Premium_by_Age.csv; fig ->
Output/Figures/AE_X3_Premium_by_Age.{png,pdf} (script Code/Analysis/AE07_Premium_by_Age.R). Empirical
premium build -> Code/Analysis/AE06_G_Transition_and_Empirical_Premiums.R +
Output/Tables/AE_X3_Empirical_Premium.csv. Control flat per-tank fee -> ASTSWMO fund-fee survey
(confirm file/field).*

#### ¶2 — Premiums track the risk we can predict  [DRAFTED]
The premium gradient is not arbitrary. It follows the risk map from §4.1. When I line up each
age-and-wall cell's premium against its predicted release probability, the two move together: cells
the model flags as higher risk carry higher premiums (Figure [actuarial-alignment]). The price an
insurer charges rises with the risk the same observables predict, which is what makes the Texas
schedule a risk-based price and not a relabeled flat fee.
*Goal: show the premium tracks predicted risk, so the gradient is actuarial, not arbitrary.*
*Pull: premium-vs-predicted-risk alignment fig -> Output/Figures/Figure_Actuarial_Alignment.{png,pdf}
+ table -> Output/Tables/Table_Actuarial_Alignment.csv (script Code/Analysis/06_Actuarial_Alignment.R);
alt premium-vs-hazard fig -> Output/Figures/AE_X3_Premium_vs_Hazard.{png,pdf}.*

#### ¶3 — Texas pulled away from the flat fee at the reform  [DRAFTED]
Before the reform, Texas and the control states both paid a flat per-tank fee, so their prices moved
together. At the 1998 reform the Texas price split off. Figure [premium-divergence] plots the average
per-tank premium in Texas against the control states from 1992 to 2020. The two track each other
before 1998 and pull apart after, as the Texas schedule starts to charge more for older single-walled
tanks while the control fee stays flat. Across matched facilities the premium gap at the switch is
about [$X]. This change in the price of risk, with the facilities themselves held fixed, is the
variation the difference-in-differences design uses in §4.3.
*Goal: show the TX-vs-control premium divergence at the reform, which is the identifying variation.*
*Pull: premium by year x treatment (TX vs control, 1992–2020) ->
Output/Tables/04e_Premium_by_Year_Treatment.csv; fig -> Output/Figures/04e_Premium_by_Year_Treatment.png;
matched premium gap at switch (~$710 in PERC) -> tbl-regime-margins (confirm source table/field).*

### 4.2.1 — The flat fee cross-subsidizes risk

#### ¶1 — The fair premium  [DRAFTED]
I can now compare what each facility pays under a flat fee to what its risk actually costs. The fair
premium for a facility is its leak risk times the cost of a cleanup. I take the leak risk from the §4.1
hazard model and the cleanup cost from incident-level claims in the states where I have them, net of the
deductible the facility would bear. The claims show that cleanup cost barely depends on a tank's age or
wall type, so I use a single pooled cleanup cost across facilities and let the whole risk gradient come
from how often a facility leaks.[^csmethod] The fair premium is what a facility's risk costs the fund
each year, and it is the vertical axis in the figures that follow. It runs from a few hundred dollars for
the safest facilities to over \$10,000 for the riskiest in Tennessee, and the spread is similar in every
state.
*Goal: define the fair premium (frequency × pooled severity) and the y-axis; the gradient is all frequency.*
*[^csmethod]: The leak frequency is the out-of-sample prediction from the §4.1 hazard model, a cross-
validated penalized logistic regression on tank age, wall type, fuel, and capacity. The cleanup cost is
the average claim paid net of the deductible, pooled across the states with usable claims data and
deflated to 2023 dollars, about \$377,838 per release. Severity (spec): from the multistate trust-fund
claims; PA dropped — its leak events are not coded in the panel, so its hazard collapses to near zero.*
*Pull: fair premium PP = lambda × S_bar net of deductible; hazard from §4.1, severity from incident-level
claims (CO/NM/PA/TN; LA borrows pooled S_bar). 2005 cross-section. TN spread few hundred → >\$10k.*

#### ¶2 — What facilities pay now  [DRAFTED]
Today these facilities pay almost nothing toward that risk. \autoref{fig-cross-paid} lines up each
state's facilities from safest to riskiest and marks what each one pays, its flat fund fee, against its
fair premium. The fee is a thin strip along the bottom. The share of its fair premium a facility
actually pays is small everywhere: 17 percent in Tennessee, 14 percent in Louisiana, 1 percent in
Colorado, and near zero in New Mexico, which levies no flat fee and funds its program through a
per-gallon gas tax. The rest, from 83 percent in Tennessee to 100 percent in New Mexico, is covered by
someone other than the facility that carries the risk.
*Goal: Figure 1 (Paid) — facilities pay a tiny share of their own risk.*
*Pull: Fig_CrossSub_Paid_Panel (fig-cross-paid). Share paid: TN 17%, LA 14%, CO 1%, NM ~0% (no flat fee,
gas-tax funded); subsidized remainder TN 83% → NM 100%.*

#### ¶3 — The break-even fee and the cross-subsidy  [DRAFTED]
A flat fee that paid for itself would still cross-subsidize risk. Set the flat fee at break-even, the
average fair premium, so the fund collects exactly what it pays out and needs no gas-tax help. In 2005
that fee is \$7,592 in Colorado, \$5,027 in New Mexico, \$4,160 in Tennessee, and \$990 in Louisiana.
\autoref{fig-cross-area} draws that break-even line across the facilities. The safe facilities sit below
it and overpay, the blue area. The risky facilities sit above it and pay less than they cost, the red
area. The two areas are equal, so the safe half's overpayments exactly fund the risky half's discount.
The transfer is 26 percent of premiums in New Mexico, 18 percent in Colorado, 18 percent in Louisiana,
and 16 percent in Tennessee. The split of facilities runs from 60 percent overpaying in Colorado to 77
percent in New Mexico, where a small, very risky tail is carried by a large safe majority. This is the
cross-subsidy a flat fee creates, even when it is priced to break even.
*Goal: Figure 3 (Area) — break-even fee τ + the overpay/subsidized transfer.*
*Pull: Fig_CrossSub_Area_Panel (fig-cross-area). τ 2005: CO \$7,592, NM \$5,027, TN \$4,160, LA \$990.
Transfer %prem: NM 26, CO 18, LA 18, TN 16. Overpay→subsidized split: CO 60→40, TN 53→47, LA 47→53, NM 77→23.*

#### ¶4 — Does it move over time  [DRAFTED]
The cross-subsidy moves over time, and its two readings move in opposite directions.
\autoref{fig-cross-time} follows Tennessee across 2000, 2005, 2010, and 2015. Read in dollars, the
transfer grows. The break-even fee climbs as the fleet ages, because older tanks leak more and the
average cleanup cost rises, and a larger average pulls the gaps between facilities up with it: the fee
rises from \$3,845 to \$5,068 in Tennessee, \$7,042 to \$8,728 in Colorado, and \$920 to \$1,137 in
Louisiana. Read as a share of premiums, the transfer shrinks, from 18 to 15 percent in Tennessee, 19 to
17 percent in Colorado, and 18 to 11 percent in Louisiana. What governs the share is not the level of
risk but its spread relative to the average, and that spread compresses as the fleet ages together. New
installations slow, the young low-risk end thins out, facilities grow more alike, and there is
proportionally less to redistribute. The two readings answer different questions. The dollar transfer is
the size of the hidden subsidy a flat fee creates, and it is rising, so the stakes of mispricing grow.
The share is how distortionary the fee is per dollar collected, and it is easing, because a maturing
fleet is a more homogeneous risk pool. Neither dominates, the flat fee moves more money every year even
as it becomes proportionally less unfair. New Mexico is too thin to read a trend, so I report its 2005
level only.
*Goal: Figure 4 (AreaTime) — the two readings (dollars grow / share thins), why each moves, kept apart.*
*Pull: Fig_CrossSub_AreaTime_TN (fig-cross-time; _CO/_LA/_NM exist per state, no panel). Fee 2000→2015:
CO 7,042→8,728, TN 3,845→5,068, LA 920→1,137. Share: LA 18→11, CO 19→17, TN 18→15. NM thin → 2005 only.*

#### ¶5 — Finer details of the figures  [DRAFTED]
A few choices shape these figures. Deductibles are large, so the fair premium uses cleanup cost net of
the deductible. New Mexico reports gross cost, so I net out its deductible. The figures cover four
states: Colorado, Louisiana, New Mexico, and Tennessee. Louisiana has no cleanup-cost data of its own,
so it borrows the pooled average cost across states. Pennsylvania is dropped because its hazard estimate
is degenerate. The cross-section is for 2005 unless noted, and the New Mexico panel is small, about 70
facilities, so I use it for the 2005 level only and not for any trend.
*Goal: the caveats behind the cross-subsidy figures.*
*Pull: D_NM net-out; FIG_STATES CO/LA/NM/TN (PA dropped, degenerate 01n hazard); NM ~70 facilities (2000),
2005 level only; claim-level severity in CO/NM/PA/TN, LA borrows pooled.*
### 4.3 — Design and identification

#### ¶1 — The closure regression  [DRAFTED]
I measure the effect of the reform on tank closure with a difference-in-differences regression:
$$\text{closure}_{it}=\beta\,\text{did}_{it}+\gamma'M_{it}+\alpha_f+\delta_{ct}+\varepsilon_{it}.$$
The outcome is one in the year tank $i$ is permanently removed. The term $\text{did}_{it}$ turns on
for Texas tanks from 1998, the year the fund closed (December 22, 1998). The facility fixed effect
$\alpha_f$ holds the site fixed, so the comparison is within a facility over time. The cell-by-year
fixed effect $\delta_{ct}$ sets the comparison group. A cell is a make and model: wall type, fuel,
capacity, and install cohort. So a Texas tank is compared only to control tanks of the same make and
model in the same year. The 1988 federal upgrade deadline is national, so it is common to treated and control
states and differences out of the difference-in-differences. The cell-by-year fixed effects absorb
cohort-specific mandate timing, and the mandate indicators $M_{it}$ are an additional and largely
redundant control, dropped in estimation for collinearity. No installation cohort is dropped.[^mandate]
The coefficient $\beta$ is the average change in a Texas tank's annual closure probability after the
reform, within its make-model-cohort and year. This $\beta$ is causal under one assumption. Without the
reform, Texas tanks and their matched controls in the same cell would have followed the same closure
path. The event study in the next subsection tests it. The headline sample is 9.76 million tank-years
across 117,250 facilities in 18 states.
*Goal: state the spec, the comparison the FEs make, the identifying assumption, and what β means.*
*[^mandate]: The federal deadline shares its December 1998 date with the Texas reform. That coincidence
is precisely why the common-shock differencing of the difference-in-differences, not a cohort drop, is
what isolates the Texas risk-based-pricing effect.*

#### ¶2 — The event study  [DRAFTED]
To see the timing, I replace the single post indicator with one coefficient per year relative to the
reform:
$$\text{closure}_{it}=\sum_{k\ne -1}\beta_k\,\mathbf 1\{k_{it}=k\}\,\text{TX}_f+\gamma'M_{it}+\alpha_f+\delta_{ct}+\varepsilon_{it},$$
where $k=t-1998$ is event time and I normalize to the year before the reform, $k=-1$. The pre-reform
coefficients are the parallel-trends test: if Texas and control tanks moved together before the
reform, the $\beta_k$ for $k<0$ sit near zero. The post-reform coefficients trace the path of the
response.
*Goal: give the dynamic specification and the parallel-trends test.*
*Reference k = -1 (we use -1 for all event studies).*

#### ¶3 — Carrying the design up to the facility  [DRAFTED]
A firm decides at the facility, not tank by tank, so I carry the same design up to the portfolio. I
build each facility's no-reform benchmark from how untreated tanks of the same make and model behaved,
$\hat Y^0_{ft}=\sum_c s_{fct}\,\hat\delta_{ct}$, where $s_{fct}$ is the facility's share of tanks in
cell $c$ and $\hat\delta_{ct}$ is the untreated closure rate in that cell. The facility regression
mirrors the tank one:
$$Y_{ft}=\beta\,\text{did}_{ft}+\mu\,\hat Y^0_{ft}+\gamma'M_{ft}+\alpha_f+\delta^{\text{mix}}_{m(f),t}+\varepsilon_{ft}.$$
It holds the facility fixed with $\alpha_f$, holds a portfolio-mix-by-year fixed effect
$\delta^{\text{mix}}_{m(f),t}$ fixed (the facility's tank mix interacted with the year, the facility
twin of the tank make-model-by-year cell), and keeps $\hat Y^0_{ft}$ as a control. The outcome
$Y_{ft}$ is any of the portfolio margins. As a check I also regress the imputation residual
$\tau_{ft}=\overline{\text{closure}}_{ft}-\hat Y^0_{ft}$ on the treatment, which gives the same answer.
*Goal: explain how the same identification moves from tank to facility, with the mix-by-year FE.*
*Pull: crosswalk Ŷ⁰/τ -> Data/Analysis/facility_cellfe_xwalk.csv (02k); facility spec ->
Code/Analysis/02j_Facility_Portfolio_DiD.R (panel_id + cell_fac_year = make_model_fac × year, 47 mixes; + Yhat0).*

#### ¶4 — Inference  [DRAFTED]
I cluster standard errors by state. Texas is the only treated state, so I compute valid standard errors
with a wild cluster bootstrap.
*Goal: inference in one line (state clustering + wild cluster bootstrap).*

### 4.4 — Does the policy work?
*Headline tank ATT (¶1), leak (¶6), facility ATT (¶4), and the §4.5 HTE magnitudes (size/vintage/fuel/
spatial) are filled from the landed verified tables (2026-06-26, read off Z:\...\Output\Tables). STILL
PENDING: ¶3 falsification (did×age×wall outside the cell×year FE) — T_Falsification_RiskConcentration
is not yet on the server.*

#### ¶1 — The headline effect on closure  [DRAFTED]
Risk-based pricing raised tank closures. After the reform, the annual chance that a Texas tank is
permanently removed rose by 1.58 percentage points (s.e. 0.40), on a pre-reform control mean of about
2 percent (Table [stepped-did]). That is close to an 80 percent increase, measured within
make-model-cohort cells and holding the facility fixed. The reform moved real capital.
*Goal: deliver the headline causal effect.*
*Pull: tank static ATT β=0.0158, SE 0.0040 -> Output/Tables/T_Stepped_DiD_OLS.tex col (7), the main
spec (fac + cell×year FE), wild bootstrap B=9,999 (02c_Stepped_DiD.R). [Confirm the ~2% control base —
not in this table.]*

#### ¶2 — The timing matches the reform  [DRAFTED]
The response lines up with the reform date. Figure [es-tank] plots the year-by-year closure gap
between Texas and control tanks. Before the reform the gap is flat and close to zero, so the two groups
were on parallel paths. At the reform the gap opens, and it stays open for years after. The effect is a
clean break at 1998, not the continuation of a pre-existing trend.
*Goal: show timing and parallel trends.*
*Pull: tank event study -> Output/Figures/Fig_ES_HTE_Pooled.* (Code/Analysis/02h_HTE_EventStudy_FirstPass.R).*

#### ¶3 — The response concentrates in old single-walled tanks  [DRAFTED]
The effect is the reform, not something else that hit Texas in 1998. A recession or an oil-price swing
would close tanks across the board. The reform instead repriced risk, so it should bite hardest on the
tanks whose price rose most, the old single-walled ones, and barely touch new double-walled tanks. To
test this I set aside the make-model fixed effect, which would otherwise absorb wall and age, and
interact the treatment with tank age and wall type. The closure response is [β] points larger for old
single-walled tanks and near zero for new double-walled tanks (Table [risk-conc]). It lands on the
tanks whose premium rose most under risk-based pricing, which a uniform statewide shock cannot produce.
*Goal: rule out a coincident 1998 shock by showing the response concentrates where the premium repriced.*
*Pull: did×age×wall with facility + year FE, outside the cell×year FE -> Output/Tables/T_Falsification_RiskConcentration.csv (new spec to run; extend Code/Analysis/02g or 02c).*

#### ¶4 — The effect holds at the facility  [DRAFTED]
The same result appears when I move from the tank to the facility. With the facility fixed and the
portfolio-mix-by-year baseline held fixed, the share of a facility's tanks that close rose by 1.62
percentage points after the reform (s.e. 0.46), and the chance a facility closed any tank rose by 1.83
points (s.e. 0.46) (Table [fac-att]). A second route, which imputes each facility's no-reform closure
share and regresses the residual on the treatment, gives a similar 1.18 points (s.e. 0.38).[^route] So
the reform changed behavior at the level a firm actually decides, the whole facility, not only the
single tank.
*Goal: show the closure effect holds at the portfolio level under the mix-by-year FE.*
*Pull: facility ATT -> Output/Tables/T_Facility_Portfolio_ATT.csv (02j; VERIFIED 2026-06-26): closure_share
+0.0162 (s.e. 0.0046, p=0.003); any_closure +0.0183 (s.e. 0.0046, p=0.001); route-B tau +0.0118 (s.e.
0.0038, p=0.006); n=3,443,560 facility-years.*
*[^route]: Route A is the direct fixed-effects estimate on the portfolio-mix-by-year cell. Route B imputes
the no-reform baseline from untreated tanks of the same make and model and uses a plain facility-and-year
fixed effect. The mix-by-year cell is what lifts Route A above Route B. The two bracket the facility-level
effect.*

#### ¶5 — The facility path  [DRAFTED]
The facility path tells the same story. Figure [es-fac] plots the facility-level closure gap by year,
with the mix-by-year baseline and the composition control held fixed. It is flat before the reform and
rises after, matching the tank-level path. The portfolio response and the tank response line up.
*Goal: show the facility-level closure timing.*
*Pull: facility closure event study -> Output/Figures/Fig_ES_Facility_Portfolio (02j). (The downsize
and replace event studies appear in §4.5.)*

#### ¶6 — Fewer leaks, not just more closures  [DRAFTED]
The reform also lowered leaks. Annual confirmed leak discovery in Texas fell by about 1.2 percentage
points (s.e. 0.003) after the reform, on a pre-reform control mean of about 1.5 percent (Table [leak-did]). This drop is the
environmental payoff, and it is not a mechanical artifact of closing more tanks. Closing a tank usually
triggers an inspection that can surface a hidden leak, which would push discovery up, yet discovery
fell. It fell through both the routine-monitoring channel and the closure-inspection channel (Table
[leak-decomp]), so the reform lowered the true rate of leaks, not just how they are found. The leak
event study is noisier than the closure one, so I read this result from the average effect rather than
the year-by-year path.
*Goal: show the environmental payoff (fewer leaks) and that it is not a detection artifact.*
*Pull: leak-discovery DiD β + base -> Output/Tables/T_LUST_All.csv / T_LUST_A_Total.tex; channel split
(routine-monitoring vs closure-inspection) -> Output/Tables/D3_LUST_Detection_Mechanism.csv /
T_LUST_B_Decomp.tex; event study (de-emphasized, noisy) -> Output/Figures/JMP_Figure_7_Leak_Event_Study.*
— report the average, not the path.*

### 4.5 — Who, where, and what
*RF FACILITY RESULTS — reference files (read in order), per the 2026-06-26 master prompt:
  1. `Reports/Paper/_RF_coefficient_interpretations.md` — MAIN SOURCE; §1 plain design, §2–§7 every
     coefficient as a plain sentence, §7 the per-figure ES causal verdicts.
  2. `Reports/Paper/_RF_outcome_plain_language.md` — the plain title/Notes/in-prose phrase for every
     outcome var; never lead with a bare var name.
  3. `Reports/Paper/RF_FE_and_Regressions_Recipe.md` — §8 do/don't wording guardrail (portfolio-mix×year
     FE, Yhat0 is a control), §9 file→content map. Recipe wins over the outline on the FE.
  4. `Reports/Paper/Reduced_Form_HTE_Facility_Results_Outline.md` — equations + sample; IGNORE its stale
     §6/§7 panel_year FE code.
  DATA formats: T_Facility_Portfolio_ATT.csv (outcome,beta,se,…); SizeHTE/VintageHTE_byMargin.csv
  (margin,dimension,level,is_reference,estimate,std_error,p_value — level total = ref + differential);
  T_Facility_HTE_byMargin.csv (margin,dimension,did_term,did_Z,… — group total = did_term + did_Z).
  Style: high-school reading level; lead the sorting story; consolidation = informative null; ES causal
  status per interp §7 (closing/exit CAUSAL, shrink/upgrade DESCRIPTIVE, consolidate NULL, replace
  BORDERLINE); state the one-treated-state SE caveat once. "facility" kept as the unit for paper
  consistency (glossary uses "station") — flagged to researcher.*

#### ¶0 — How I read the reform at the facility  [DRAFTED]
A firm decides at the facility, not tank by tank, so I carry the design up to the whole portfolio. The
comparison is the same plain idea as the tank result. A Texas facility is matched only to a non-Texas
facility with the same tank setup in the same year, so parallel trends hold conditional on portfolio mix.
I measure each portfolio action on every facility, not only the ones that close, so exit and adjustment
are both in view. With one treated state, the standard errors here are a cluster-robust first pass, and I
defer the wild-cluster bootstrap to the final tables, so I flag the borderline results below.
*Goal: the facility ID in one plain idea (unchanged from the tank headline) + the inference caveat, stated once.*
*Pull: recipe §8 guardrail — portfolio-mix × year FE (not plain year); Yhat0 is a control. Sample/eqs from the outline.*

#### ¶1 — What facilities did, on average  [DRAFTED]
The reform mainly made facilities retire tanks and shut down. The share of a facility's tanks closed in a
year rose by 1.6 points, the chance a facility shut down completely rose by 1.25 points, and the chance
its total fuel-storage capacity fell rose by 1.5 points. Among facilities that stayed open, the reform
pushed real adjustment, but only of certain kinds. A facility that shrank, meaning it ended the year with
fewer tanks and at least five percent less capacity, became 0.26 points more common (s.e. 0.09). One that
upgraded, trading several tanks for fewer but larger ones, became 0.16 points more common (s.e. 0.03),
and the share of tanks a facility swapped for new ones rose 0.29 points. One move did not respond at all.
Consolidation, dropping tanks while holding total capacity about steady, did not move, at −0.04 points
and not significant. So the contraction the reform produced is genuine capacity shedding and modernizing,
not a capacity-preserving reshuffle of the same tanks.
*Goal: the full average portfolio response in glossary phrases, with the consolidation informative null.*
*Pull (T_Facility_Portfolio_ATT.csv, VERIFIED 2026-06-26): closure_share +0.0162, any_closure +0.0183,
facility_exit +0.0125, cap_decrease +0.0148, perm_share +0.0133, downsize +0.0026 (s.e. 0.0009, p=0.008),
reconfigure_up +0.0016 (p<0.001), repl_share +0.0029, consolidate −0.0004 (p=0.24, NULL). Outcome phrases
per _RF_outcome_plain_language.md.*

#### ¶2 — Who responded: motor-fuel retailers  [DRAFTED]
The response is concentrated in motor-fuel retailers. At the tank level, facilities whose tanks are all
motor fuel close about 4.3 percentage points more than other facilities after the reform (p < 0.001).
Their total response is near 5 percentage points. Having any gasoline tank, which describes most
facilities, does not separate the response. That differential is about −0.5 points and is not
significant. At the facility level the pattern holds and runs across the whole portfolio. Gas stations
close about 1.8 percentage points more than other facilities (s.e. 0.16), and the gas-station
differential is positive on every margin I measure, including exit and replacement (Table [hte-tank],
Table [fac-hte]). Motor-fuel retailers carry the response.
*Goal: show who responded most — motor-fuel-only tanks and gas stations, the robust cuts.*
*Pull: motor-fuel-only and any-gasoline tank HTE -> Output/Tables/T_HTE_Tank_Pub.tex (02g; verified:
motor-fuel-only did×Z = +0.0431, p<0.001, total ≈ +0.050; any-gas did×Z = −0.0051, p=0.22). Facility
gas-station HTE -> Output/Tables/T_Facility_HTE_byMargin.csv (02j; VERIFIED 2026-06-26: closure_share
did×Z = +0.0182, s.e. 0.0016, p<0.001; exit +0.0069; replace +0.0089; downsize +0.0199 [old combined]).
OPEN: the facility number is +1.82 pp here vs the +1.9 pp in the session brief / +0.0190 earlier run —
flagged to researcher. CONFIRM the underlying tank variable: is the +4.3 pp cut "all tanks gasoline" or
"all tanks motor fuel"? Label kept as motor-fuel per researcher; variable is all-gasoline.*

#### ¶3 — Who responded: by size and by vintage  [DRAFTED]
The response also sorts by how big a facility is and how old its tanks are, and it sorts the same way on
both. I interact the treatment with the facility's total-capacity bin, four bins from small to large,
and with its tank vintage at the reform. Small facilities exit. The smallest bin, under 9,000 gallons,
raises its closure share by 6.3 points (s.e. 0.5) and its exit rate by 6.6 points (s.e. 0.5), and that
response falls toward zero across the larger bins (Table [size-hte]). Large facilities reinvest instead.
The largest bin, over 30,000 gallons, barely exits but downsizes 1.0 point and replaces tanks 0.7
points, each rising monotonically across the bins (p < 0.001 for every differential). Vintage sorts the
same way. The oldest tanks, installed before 1975, exit 2.7 points more than the youngest cohort
(s.e. 0.3) and do not reinvest, while the youngest cohort, 1989 to 1998, barely exits but downsizes 0.5
points (s.e. 0.04) and replaces tanks 0.7 points (s.e. 0.04) (Table [vintage-hte]). The marginal
facilities, small and old, leave the market. The viable ones, large and young, upgrade and stay.
*Goal: show the size and vintage sort — marginal facilities exit, viable facilities reinvest. (Downsize
numbers REFRESHED to the gallons-based split, Ticket 031.)*
*Pull (VERIFIED 2026-06-26, byMargin CSVs landed): size HTE -> T_Facility_SizeHTE_byMargin.csv (G1 closure
+0.0632 s.e. 0.0054, G1 exit +0.0658 s.e. 0.0053; G4 levels: gallons-downsize +0.0101, replace +0.0070,
exit ≈ 0; differentials p<0.001). Vintage HTE -> T_Facility_VintageHTE_byMargin.csv (pre-1975 exit diff
+0.0266 s.e. 0.0028; 1989-98 levels: downsize +0.0053 s.e. 0.0004, replace +0.0069 s.e. 0.0004,
reconfigure_up +0.0043). NB downsize fell (old combined: G4 +0.0121, young +0.0172) because the new
downsize is gallons-shedding only; young reinvestment now spreads across downsize/replace/reconfigure-up.*

#### ¶4 — Where it landed: place and market  [DRAFTED]
Where a facility sits decides whether it adjusts or disappears. I interact the treatment with location and
market measures fixed at the reform: rural, low-population, low-income, and high-poverty areas from the
2000 Census, and a measure of how few competing gas retailers sit within a mile in 1998. All are set
before or at the reform, so none picks up the reform's own effect on the market. Two patterns stand out.
Where demand is captive, facilities stay open. Rural facilities are 0.42 points less likely to shut down
(s.e. 0.03), and gas retailers with few nearby competitors are 1.64 points less likely (s.e. 0.09). Where
facilities serve poor areas, they shut down but do not reinvest. Facilities in low-income and high-poverty
areas are 0.22 and 0.18 points more likely to shut down (s.e. 0.04 each), yet they shrink and replace
less, with low-income facilities shrinking 0.38 points less (s.e. 0.02) and replacing 0.32 points less
(s.e. 0.01). So the reform retired tanks fastest in poor areas, with no reinvestment there, and slowest
where a single station holds a captive market.
*Goal: show where the effect concentrates, by place and by market thickness; poor areas lose stations for good.*
*Pull (T_Facility_HTE_byMargin.csv, VERIFIED 2026-06-26, gallons-based): facility_exit did×Z rural −0.0042
(s.e. 0.0003), thin_market −0.0164 (0.0009), low_income +0.0022 (0.0004), high_pov +0.0018 (0.0004).
downsize did×Z low_income −0.0038, high_pov −0.0035. repl did×Z low_income −0.0032 (0.0001), high_pov
−0.0026. All p<0.001 except high_pov exit (p≈0.0001). NB downsize did×Z REFRESHED (was −0.0086 pre-031).
Census-2000 cuts run locally (GIS lookups local-only); thin_market from facility geography.*

#### ¶5 — Which responses are causal  [DRAFTED]
The event studies show which of these I can read as causal. Each one plots the Texas-minus-control gap
year by year, and a flat path before 1998 is the honesty check. Closing tanks and shutting down pass it
cleanly. The closure path is flat and near zero before the reform, then breaks upward and stays up for
two decades (Figure [es-fac]), so the extensive margin is the credible core. The portfolio-adjustment
margins are weaker. The shrinking path runs positive after the reform but its pre-period is not perfectly
flat, so I present it as suggestive (Figure [es-downsize]). The upgrading path drifts upward through the
reform rather than breaking at it, so part of its rise continues a pre-existing trend (Figure
[es-reconfigure]). The replacement path is clearly higher after but has some elevated pre-reform years,
so it is borderline and needs the bootstrap before any causal claim (Figure [es-replace]). Consolidation
scatters around zero throughout, matching its null average (Figure [es-consolidate]). The honest summary
is that the extensive margin, closing tanks and shutting down, is causal, while the adjustment margins
are suggestive sorting and consolidation shows no effect.
*Goal: per-figure causal status (interp doc §7); don't overclaim.*
*Pull: Fig_ES_Facility_{Portfolio=closure CAUSAL, Downsize=descriptive, ReconfigureUp=descriptive/drifts
through reform, Replace=borderline/needs bootstrap, Consolidate=null}; Fig_ES_HTE_Pooled = tank closure causal.*

#### ¶6 — What the reduced form settles, and what it cannot  [DRAFTED]
Put together, the facility evidence tells one story. The reform sorted the market. It pushed small and old
facilities, and facilities in poor areas, to retire tanks and leave for good, and it pushed large and
young facilities, and gas stations with room to reinvest, to shrink, replace, and upgrade and stay. Rural
facilities and those with few competitors mostly stayed open. Consolidation, the one move that would have
changed nothing real, did not happen. The response concentrated where the premium rose most rather than
spreading evenly across Texas, so a statewide shock that merely coincided with 1998 cannot explain it.
What the reduced form cannot do is put these responses in dollars, price the policies Texas never ran, or
separate deterrence from selection. That is the work of the structural model in §5.
*Goal: land the sorting story, nod to the falsification, and hand to §5.*

## Section 5 — Dynamic Structural Model  [§ what the reduced form cannot do]
*DRAFTED 2026-06-26 from `Reports/Paper/Structural_Model_Current_Spec.md` (the current pooled-exposure
portfolio model; supersedes the single-tank 6p+FE model). Style matched to the PERC structural section
in `PERC_Reserach_Plan.qmd`: intuition-first, run-in heads, numbered equations, parameters defined in
order of appearance, the two-facility worked example, three-part identification. House style enforced
over the template: singular "I", no semicolons, no em-dashes, short sentences.
KEY CHANGE vs the PERC template = the flow utility. ONE pooled coefficient gamma_pool on TOTAL exposure
E = P + H·D (premium plus expected out-of-pocket), plus a regime-extra gamma_RB that fires only under
risk-based pricing. The premium-vs-OOP split is NOT separately identified, written as a finding.
THREE DEPARTURES flagged to researcher: (1) beta = 0.9957 labeled "annual" in the spec but the PERC
template calls 0.9957 monthly (~5%/yr); an annual 0.9957 implies a ~0.4% rate, so the label needs a
decision — prose says "fixed in advance" and states no implied rate. (2) Counterfactuals now KEEP the
environment FEs (Semantic-1: real persistent state traits), the REVERSE of the PERC plan which drops
alpha_g; written the new way with the artifact caveat. (3) Environment count = 17 (14 FF control + 3 TX
RB eras, MO folded out) does not square with the 17 RF controls / 18 states — reconcile.*

### 5.0 — Opening: bridge and agent  [DRAFTED]

#### ¶A — What the reduced form cannot do  [DRAFTED]
The reduced form shows that the 1998 reform changed closure behavior. It cannot value that change in
dollars, price a policy Texas never ran, or say how far the reform moved firms toward the social
optimum. To answer those questions I build a dynamic structural model of how a facility manages its tank
portfolio. Each year a facility chooses what to do with its tanks. It can keep operating them, remove
some, replace some, or shut down. It weighs the operating value of its tanks against the cost of the
risk they carry and the cost of acting. I estimate the model on Texas and the uniform-pricing control
states, then re-solve it under policies that were never tried.
*Goal: bridge from RF to structural — the three questions the design cannot answer.*

#### ¶B — The agent is the facility  [DRAFTED]
The agent is the facility, not the tank. A facility with several aging tanks does not decide tank by
tank. It decides how many of its tanks to remove as one choice. The state has to track the facility's
whole portfolio, not one representative tank. A model built on a single tank cannot tell a facility that
slowly retires an old fleet apart from one that replaces tanks one for one, and those two paths carry
different consequences for a removal mandate. Tracking the portfolio is what lets the model price the
partial-closure margin the reduced form found.
*Goal: fix the unit of analysis (facility portfolio, not tank) and why it matters.*

### 5.1 — Measured operating revenue (first stage)

#### ¶1 — Where the revenue measure comes from  [DRAFTED]
The model needs to know what a facility earns, not only what it pays for insurance. I build a
per-period operating-revenue measure for each facility from public data. The retail gasoline price and
gallons sold by state and year come from the EIA State Energy Data System. The state gasoline tax comes
from the FHWA "Tax Rates by Motor Fuel and State" series. The wholesale price refiners charge resellers
comes from the EIA refiner "sales for resale" prices, reported by region (PADD). The federal gasoline
tax is 18.4 cents a gallon. Facility size comes from the tank registry: I sum each facility's tank
capacity and sort facilities into four size groups. The series cover the 18 study states from 1999 to
2020.
*Goal: name the sources for the revenue measure.*
*Pull: EIA SEDS (retail price + consumption) + FHWA tax -> Data/Macro/seds_gasoline_state_year.csv (025);
EIA refiner sales-for-resale wholesale by PADD + margin -> Data/Macro/wholesale_margin_state_year.csv
(026); capacity -> four G bins from the UST panel; federal excise \$0.184.*

#### ¶2 — The operating-revenue measure  [DRAFTED]
Most of the retail price of gasoline is the cost of the fuel itself, so the retail price is a poor
measure of what a station earns. The right measure is the margin: the retail price minus the wholesale
price, the state tax, and the federal tax. The margin is the money a station makes on a gallon. A
facility's revenue is its margin times how much fuel it moves, and throughput scales with size. So I
measure revenue as capacity times the local fuel margin. Everything is in nominal dollars, to match the
nominal premium and deductible already in the model. I average the margin within three eras rather than
tracking every year, so a facility forecasts a steady local margin instead of chasing the year-to-year
swing.
*Goal: define the revenue measure (capacity × margin), nominal, era-averaged.*
*Pull: R = capbar_G × margin_{state,era} / SCALE (028 / Data/Macro/R_rev_long.csv); margin = retail −
wholesale − state tax − \$0.184, nominal; eras 1999–2013 / 2014–2018 / 2019+; study mean margin ≈ \$0.16/gal.*

#### ¶3 — How revenue enters the model  [DRAFTED]
The model used to carry one free number per size group for the operating payoff. Now that size profile
is tied to measured revenue. The term becomes ψ·R, where R is the measured revenue and ψ is a single
weight I estimate. ψ says how much a dollar of revenue moves a facility's choices, on the same footing
as the weights on the premium and the deductible. This follows Sweeting (2013): measure revenue outside
the dynamic model, then estimate only the weight. A constant linking storage capacity to gallons sold is
folded into ψ, so ψ is a weight, not a dollar amount. Because revenue enters the post-action payoff, a
facility that downsizes or exits gives up measured revenue right away, which ties the size margins to
real dollars. ψ is identified mainly across facilities, from a facility's size crossed with its state's
fuel margin, not from the national time path of prices.[^revfloor]
*Goal: how ψ·R replaces the free size constants (a restriction), what ψ means, how it is identified. Estimator internals -> appendix.*
*Pull: ψ·R replaces φ_1..φ_4 in the flow utility (029 / Code/Dynamic_Model/PM08_Estimator_v4.R); κ
(turnover) ≡ 1 absorbed by ψ; identification cross-sectional (capacity × within-state margin), not the
macro time series.*
*[^revfloor]: One state's early-period margin is bounded, not measured. North Carolina sits in the
Lower Atlantic, which the Gulf supplies through the Colonial Pipeline, but EIA reports wholesale prices
only for the whole East Coast region, which is pulled up by the Northeast. With no sub-region wholesale
price published, North Carolina's margin before 2014 is floored at a small positive value. It is one
cell of fifty-four, and the state fixed effect absorbs the rest.*
### 5.2 — State space  [DRAFTED]

#### ¶1 — Portfolio and capacity  [DRAFTED]
The portfolio $c$ records how many tanks a facility holds across 16 categories. The categories cross two
wall types, single-walled and double-walled, with eight five-year age bands. A facility holds between
one and six tanks. Counting every combination of tank counts across the categories gives 74,612
distinct portfolios. A separate term $G$ sorts each facility into one of four bins by total storage
capacity. I keep capacity apart from the portfolio because capacity drives revenue, while tank count and
age drive the premium and the hazard. Two facilities with the same tanks can still differ in scale.
Crossing portfolios with capacity bins gives a state space of $74{,}612 \times 4 = 298{,}448$ states.
*Goal: define the state (portfolio count × capacity bin) and the 298,448 count.*
*Pull: 16 cells = 2 wall × 8 age bands; 1–6 tanks → 74,612 portfolios × 4 capacity bins (spec §2).*

#### ¶2 — Two facilities  [DRAFTED]
Two facilities show what a state is. The first holds three tanks: two single-walled tanks in the oldest
band and one double-walled tank in the youngest, with capacity in the smallest bin. The second holds
four double-walled tanks in a middle band, with capacity in the largest bin. The two face different
premiums and different hazard even under the same contract, because premium and hazard depend on the
full portfolio, not on a summary like tank count or average age. All facility heterogeneity in the model
is where the facility sits in this state space.
*Goal: the two-facility worked example (PERC device); heterogeneity = position in the state space.*

#### ¶3 — Most facilities are simple  [DRAFTED]
Most facilities sit in a simple part of this space. About 82 percent of facility-years hold tanks of a
single wall type and age band, so the portfolio collapses to a count for them. The full richness of the
state is used only by the large, mixed facilities, which are also the ones whose partial-closure choices
the reduced form flagged.
*Goal: the state is exact for most data; full richness only for large mixed facilities.*
*Pull: ~82% homogeneous facility-years (spec §2).*

### 5.3 — Actions  [DRAFTED]

#### ¶1 — The action menu  [DRAFTED]
Each year a facility chooses an action $a = (k, m)$. It removes $k$ tanks and installs $m$ new ones.
Removal follows a fixed order: oldest tanks first, single-walled before double-walled, smallest first.
That order matches the order in 84 percent of removal events in the data, so I build it in as a rule
rather than estimate a separate choice over which tank leaves. Installation is restricted to
double-walled tanks, because federal and state rules ended new single-walled installation over the
sample.[^swban] There is no separate choice over wall type when a facility installs.
*Goal: the (k,m) action; the deterministic removal rule (84%); double-wall-only installs.*
*Pull: removal-order match 84% (spec §3/§7); single-wall install ban (spec §3; PERC fn swban).*

#### ¶2 — What the menu covers  [DRAFTED]
The menu covers the behaviors the reduced form measured. Maintaining is $(0,0)$. Downsizing is $(k,0)$,
removing tanks and operating with fewer. Replacing or upgrading is $(k, m>0)$, removing tanks and
installing new double-walled ones. A facility can also exit, which is absorbing. Removal and
installation are one joint choice, not two steps, so a facility's plan to replace tanks can shape how
many it removes in the same year. A two-step model would force a facility to decide whether to close
before it could decide whether to reopen, which is not how replacement looks in the data.
*Goal: map (k,m) to maintain/downsize/replace/exit; justify the joint (not sequential) choice.*

#### ¶3 — Post-action timing  [DRAFTED]
Premium and hazard are evaluated on the fleet left after the action, before that year's aging. A
facility that downsizes sees its premium fall in the same year it removes tanks, not later. This matches
how an insurer re-rates a facility after its tank count changes.
*Goal: post-action, pre-aging timing of premium and hazard.*

### 5.4 — Flow utility  [DRAFTED]
*The KEY change vs the PERC template. ONE pooled coefficient on total exposure, plus a regime-extra. The
premium-vs-OOP split is not identified, written as a finding (spec §4, §7, §10).*

#### ¶1 — The payoff  [DRAFTED]
A facility that does not exit gets a per-period payoff from operating the fleet $n'$ left after its
action:
$$u_a(s,e)=\phi_G+\alpha_e-\big(\gamma_{\text{pool}}+\gamma_{\text{RB}}\,\mathbf 1[\text{RB}_e]\big)\,E(n',e)-k\,c_{\text{rem}}-m\,c_{\text{inst}}.$$
Here $\phi_G$ is the operating value of the fleet, allowed to vary with the capacity bin $G$. $\alpha_e$
is a fixed effect for the facility's regulatory environment. $E(n',e)$ is the total risk exposure the
fleet carries. $c_{\text{rem}}$ and $c_{\text{inst}}$ are the per-tank costs of removing and installing.
$\gamma_{\text{pool}}$ and $\gamma_{\text{RB}}$ convert exposure into the same units as operating value,
and I return to them below.
*Goal: state the flow utility and name every term in order.*

#### ¶2 — Total risk exposure  [DRAFTED]
Total risk exposure is the sum of two costs a facility bears for holding risky tanks:
$$E(n',e)=P(n',e)+H(n')\,D_e.$$
$P(n',e)$ is the insurance premium the facility pays on the fleet $n'$ under its contract $e$. $H(n')$ is
the facility's hazard, the chance that any of its tanks leaks in the year. $D_e$ is the deductible the
facility pays out of pocket when a leak occurs, so the second term is the expected out-of-pocket cost of
a leak. Both pieces rise with older and single-walled tanks. The hazard $H$ comes from the facility-level
model in §4.1, the premium $P$ from the reconstructed schedule in §4.2, and the deductible $D_e$ from the
contract terms. All three are first-stage inputs, estimated outside the dynamic model and held fixed.
*Goal: define total exposure E = premium + hazard×deductible and tie each piece to its first stage.*

#### ¶3 — Why one coefficient, not two  [DRAFTED]
The model puts one coefficient $\gamma_{\text{pool}}$ on total exposure, not separate coefficients on the
premium and on the expected out-of-pocket cost. A dollar of premium and a dollar of expected
out-of-pocket loss enter the payoff the same way. I do this because the data cannot tell the two apart.
Under risk-based pricing the premium moves with the hazard, so the premium and the expected loss are
collinear. The premium also varies mostly across states, which the environment fixed effects absorb. And
the Texas premium data begin only in 2006, so there is no clean within-Texas window in which to separate
them. I report the pooled response to total exposure and treat the premium-versus-out-of-pocket split as
not identified in this data. That non-identification is itself a result. It says a facility responds to
the size of its risk bill, not to which channel delivers it.
*Goal: the central modeling choice — pooled exposure — stated as a finding, with the reason in the same ¶.*
*Pull: spec §4 (pooled), §7 (why pooled), §10 (caveat). Anchor: limitation + solution in one paragraph.*

#### ¶4 — The regime knob  [DRAFTED]
The second coefficient $\gamma_{\text{RB}}$ is an extra response that applies only under risk-based
pricing. It lets a dollar of exposure move behavior more, or less, when the price is risk-based than when
it is a flat fund fee. A positive $\gamma_{\text{RB}}$ means firms respond more strongly to exposure
under risk-based pricing, which is the sign that the pricing mechanism does work beyond the level of the
bill. A value near zero means a dollar is a dollar whatever the regime. In this sample risk-based pricing
means Texas, so $\gamma_{\text{RB}}$ also picks up anything specific to Texas. I read it as suggestive of
the mechanism, not as a clean causal estimate, and I say so again where it matters for the
counterfactuals.
*Goal: what gamma_RB measures (regime-extra response) and the RB≡Texas caveat up front.*

#### ¶5 — Fixed effects and operating value  [DRAFTED]
The environment fixed effects $\alpha_e$ are load-bearing. They absorb everything that differs across
states for reasons unrelated to exposure: geology, enforcement, the local economy, data coverage.
Holding them fixed is what leaves a clean within-state exposure signal for $\gamma_{\text{pool}}$ to read.
The operating value $\phi_G$ is the payoff from running a fleet at a given scale. In the estimated model
I replace the free $\phi_G$ intercepts with the measured-revenue term from §5.1, so operating value is
$\psi\,R$, where $R$ is the measured revenue of a facility of that size in that state and era and $\psi$
is the single weight that scales it. This ties operating value to measured revenue instead of free
intercepts, which came back nearly flat across capacity bins.
*Goal: alpha_e is load-bearing; connect phi_G to the §5.1 measured-revenue refinement (ψ·R).*

#### ¶6 — Exit value  [DRAFTED]
A facility that exits gets a one-time terminal value:
$$u_{\text{exit}}(s)=\kappa_1\,N(s).$$
$N(s)$ is the number of tanks the facility holds. $\kappa_1$ measures how exit value scales with size. I
normalize the constant part of exit value to zero, which pins the level of the value function, so
$\kappa_1$ measures only how exit value grows with the number of tanks.
*Goal: exit value κ_1·N and the κ_0:=0 normalization.*

### 5.5 — Bellman equation and estimation  [DRAFTED]

#### ¶1 — The value function  [DRAFTED]
Facilities are forward-looking. The value of a state solves
$$V(s)=\max\Big\{\max_{(k,m)}\big[u_a(s,e)+\beta\,\mathbb E[V(s')\mid s,a]\big],\;u_{\text{exit}}(s)\Big\}.$$
The discount factor $\beta=0.9957$ is fixed in advance rather than estimated. Exit is absorbing. Each
action carries an independent type-1 extreme value shock, which gives closed-form logit choice
probabilities given the state.
*Goal: the Bellman equation; β fixed; logit shocks.*
*FLAG: spec labels β=0.9957 "annual"; PERC template calls 0.9957 monthly (~5%/yr). Annual 0.9957 ⇒
~0.4% rate. Prose says "fixed in advance," states NO implied annual rate until the researcher resolves it.*

#### ¶2 — Two stages  [DRAFTED]
Estimation runs in two stages. The first stage builds the premium schedule, the hazard model, the
deductibles, the capacity transitions, and the revenue measure directly from the claims, premium, and
registry data, and holds them fixed. This is the standard two-stage approach in this literature. It lets
the data on prices and risk be taken as given before I ask what cost and preference parameters fit the
closure behavior observed under those prices.
*Goal: the two-stage split; first-stage inputs held fixed.*

#### ¶3 — Parameters and environments  [DRAFTED]
The second stage estimates $\theta=(\psi,\gamma_{\text{pool}},\gamma_{\text{RB}},c_{\text{rem}},c_{\text{inst}},\kappa_1)$,
with the environment fixed effects $\alpha_e$ estimated alongside. One parameter vector $\theta$ is
shared across all environments: 14 uniform-pricing control states and three Texas risk-based rate eras,
17 in all, with Texas the reference. Sharing one $\theta$ across environments that price risk differently
is the main source of identifying variation, which the next section takes up.
*Goal: the estimated θ; one θ shared across 17 environments (the identifying structure).*
*FLAG: 17 environments = 14 FF control + 3 TX RB eras (MO folded out, spec §2) does NOT square with the
RF's 17 controls / 18 states. Reconcile the control count before this goes final.*

#### ¶4 — Nested pseudo-likelihood  [DRAFTED]
I estimate $\theta$ by nested pseudo-likelihood (Aguirregabiria and Mira). The method avoids solving the
full dynamic program at every trial value of $\theta$. It starts from the choice probabilities seen in
the data, holds them fixed to turn the dynamic problem into a static one in which the value function is
linear in $\theta$, estimates $\theta$ by maximizing the pseudo-likelihood, then recomputes the model's
implied choice probabilities and repeats until $\theta$ and the probabilities settle together. At 298,448
states, solving the value function from scratch inside an optimizer would not be feasible. With the
choice probabilities fixed the value function solves from the linear inversion $V=(I-\beta M)^{-1}R$,
which I compute with an iterative solver (BiCGSTAB) preconditioned on the tank-aging structure rather
than by direct inversion. The estimating sample is aggregated facility-year choice counts, Texas 2006 to
2020 and the control states 1999 to 2020. The 2006 start for Texas is forced by when the premium data
begin.
*Goal: NPL mechanics, the AM linear inversion + BiCGSTAB, and the estimating sample/window.*

### 5.6 — Identification  [DRAFTED]
*Three-part structure from the PERC template: data-grounded assumptions / DDC-NPL inherited / per-
parameter empirical variation. "Why pooled" lives here too (spec §7).*

#### ¶0 — The three components  [DRAFTED]
Identification of $\theta$ rests on three things. The first is a set of modeling assumptions grounded in
patterns in the data. The second is the set of assumptions that comes with choosing a dynamic discrete
choice model estimated by NPL. The third is the actual comparison in the data that fixes the size of
each parameter once the first two hold. I take them in turn, and I am explicit that some parameters rest
on stronger variation than others.
*Goal: lay out the three identification components.*

#### ¶1 — Assumptions grounded in the data  [DRAFTED]
The removal order, oldest first and single-walled before double-walled, is the order in 84 percent of
removal events, so I build it in as a rule. Installation is double-walled only because the single-walled
install ban removed the alternative. The premium schedule, hazard, deductibles, capacity transitions, and
revenue are built from observed records and held fixed, because they are directly observed. The cost and
preference parameters in $\theta$ are not observed and must be estimated.
*Goal: the behavior/law-of-motion assumptions that come from the data.*

#### ¶2 — Assumptions from the model class  [DRAFTED]
These are standard for the model class, not special to tanks. Each action's shock is an independent draw
from a type-1 extreme value distribution, which gives the logit choice probabilities. Facilities have
rational expectations: each knows its contract and correctly anticipates how its own portfolio evolves.
The NPL mapping satisfies the regularity conditions needed for the iteration to converge to the
parameters that generated the data. Convergence diagnostics are reported with the fit.
*Goal: the DDC/NPL inherited assumptions (T1EV, rational expectations, NPL regularity).*

#### ¶3 — What pins each parameter  [DRAFTED]
Conditional on those assumptions, each parameter is pinned by a distinct comparison.
$\gamma_{\text{pool}}$, the response to total exposure, is identified from within-state variation in
exposure. At a fixed environment, facilities with older or single-walled tanks, or facing higher
deductibles, carry more exposure and downsize or exit more. The environment fixed effects are what make
this clean, because they strip out cross-state confounds and leave within-state variation. The age-free
levers that discipline it are wall type and the deductible. Single-walled versus double-walled at the
same age changes hazard, and the deductible changes out-of-pocket cost without being driven by age, so
both move exposure for reasons other than a tank getting older. The premium-versus-out-of-pocket split is
not identified: with separate coefficients the premium coefficient collapses to near zero, because
premium moves with hazard under risk-based pricing, because premium varies mostly across states that the
fixed effects absorb, and because Texas premiums begin only in 2006. I therefore report the pooled
response. $\gamma_{\text{RB}}$, the extra response under risk-based pricing, is identified from the
difference in the exposure-response slope between the uniform control states and risk-based Texas. Because
risk-based pricing is Texas in this sample, this comparison conflates the mechanism with Texas-specific
factors, so it is suggestive, not a clean estimate of the mechanism. $\psi$, the weight on operating
revenue, is identified across facilities, from a facility's size crossed with its state's fuel margin
(§5.1), not from the national time path of prices. $c_{\text{rem}}$ and $c_{\text{inst}}$ are identified
from how often facilities downsize and replace, and from the split among closing facilities between
downsizing, replacement, and exit. $\kappa_1$ is identified from how the full-exit rate varies with the
number of tanks, net of operating value.
*Goal: per-parameter empirical variation; carry the "why pooled" non-ID and the RB≡TX caveat here.*

#### ¶4 — Limits  [DRAFTED]
This strategy has clear limits. The premium-versus-out-of-pocket split is not identified, so I report the
pooled response. $\gamma_{\text{RB}}$ conflates risk-based pricing with Texas. The hazard model is only as
precise as the leak data. The removal rule matches 84 percent of events, leaving a small share it does
not.
*Goal: honest identification limits in one paragraph.*

*[^swban]: New single-walled installation was foreclosed by state requirements that new tanks and piping
be secondarily contained (double-walled), most following the federal secondary-containment rule added by
the Energy Policy Act of 2005. Effective dates run from 1989 in Massachusetts and 1991 in Maine through
2013 in Kansas, most concentrated in 2007 to 2009. (Condensed from PERC fn swban; verify before final.)*

## Section 6 — Estimates, Counterfactuals & Welfare  [§ policy on a welfare scale]
*Talk 33–36. DRAFTED as an outline; the headline pooled estimates (gamma_pool, gamma_RB) and the CF
welfare numbers are [pending] the converged pooled run + an SE pass. Each policy changes ONE term. Welfare
= producer surplus − external damage − govt outlay (PV). E ≈ $50k/release placeholder. NOTE the model
change: counterfactuals KEEP the environment FEs (Semantic-1), the reverse of the PERC plan which drops
alpha_g — write it the new way with the artifact caveat (spec §9, §10).*

### 6.1 — Estimates  [OUTLINE]
#### ¶1 — The estimated parameters  [OUTLINE]
Table [struct-estimates] reports the estimated parameters. [HEADLINE PENDING: $\gamma_{\text{pool}}$ and
$\gamma_{\text{RB}}$ from the converged pooled run.] The anchor in hand is the earlier two-coefficient
fit, in which the single exposure coefficient is 4.32, the removal cost is 3.85, the install cost is
2.04, and the exit-size gradient $\kappa_1$ is 0.93, with a log-likelihood of −291,648.5 across 17
environments. Operating value came back nearly flat across capacity bins under free intercepts, which is
part of why I tie it to measured revenue. All values are in model units of \$10,000 per facility-year.
*Goal: report θ. Lead with the pooled headline once it lands; FE-on anchor meanwhile.*
*Pull (spec §8): FE-on fit gamma=4.32, c_rem=3.85, c_inst=2.04, kappa_1=0.93, phi≈−0.55 flat, LL=−291,648.5,
17 envs, β=0.9957. HEADLINE gamma_pool/gamma_RB [pending pooled run]. SEs [pending] — gamma_RB significance
needed to read the regime result. Expect gamma_pool > 0; gamma_RB sign = the regime-mechanism result.*

#### ¶2 — What the estimates say  [PENDING]
[PENDING the pooled fit. The reading: $\gamma_{\text{pool}}>0$ means more exposure pushes facilities off
maintain toward downsizing and exit. The sign and size of $\gamma_{\text{RB}}$ is the regime-mechanism
result — whether a dollar of exposure bites harder under risk-based pricing. Report with the SE caveat.]
*Goal: interpret the headline once it lands; the regime-mechanism reading of gamma_RB.*

### 6.2 — Counterfactuals  [OUTLINE]
#### ¶1 — How a counterfactual works  [DRAFTED]
With $\theta$ estimated, I re-solve the model under policies Texas never ran. Each counterfactual changes
one term in the payoff and leaves the rest of the model unchanged, then re-solves the value function and
the resulting fleet. I keep the environment fixed effects in the counterfactual, because they stand for
real and persistent features of each state rather than a temporary effect, so the baseline reproduces
observed behavior and each policy is a clean change against it. Welfare is computed on the observed
distribution of facilities, with no reweighting.
*Goal: CF protocol — one term moves; FEs KEPT (Semantic-1); welfare on observed distribution.*
*FLAG: this REVERSES the PERC plan (which drops alpha_g in CFs). Per spec §9. Caveat (spec §10): part of
each FE may be data artifact, not behavior — acknowledge in the prose.*

#### ¶2 — Counterfactual 1: Texas under uniform pricing  [DRAFTED]
The first counterfactual asks what Texas would have done if it had stayed on uniform pricing. I replace
the Texas risk-based contract, both its premium and its deductible, with a representative uniform-state
contract and re-solve. I report it as a decomposition. The first piece changes only the contract and
keeps the risk-based response. The second piece changes the contract and also turns off the regime-extra
response $\gamma_{\text{RB}}$. The difference between the two isolates the risk-based-pricing mechanism,
separate from the change in the dollar bill. This is the headline risk-based-versus-uniform experiment.
*Goal: CF1 setup + the (a)/(b) decomposition that isolates the gamma_RB mechanism.*
*Pull: CF1 (spec §9). Welfare deltas [pending the pooled fit].*

#### ¶3 — Counterfactual 2: replacement subsidy  [DRAFTED]
The second counterfactual subsidizes new double-walled tanks by lowering the install cost, and sweeps the
subsidy across levels. I report how the replacement rate and welfare respond, and how the response
differs between the risk-based and uniform regimes. The subsidy adds a government outlay to the welfare
account.
*Goal: CF2 replacement subsidy (lower c_inst), swept; outlay enters welfare.*
*Pull: CF3 in spec §9 (renumbered to CF2 here for the reader; flag the relabel).*

#### ¶4 — Counterfactual 3: removal mandate  [DRAFTED]
The third counterfactual forces removal of single-walled tanks past age 20. A facility can comply by
downsizing or by upgrading to double-walled tanks. The mandate trades the firm's cost of forced early
action against the leak damage it averts. I compare its welfare to the gradual response the price
instruments produce.
*Goal: CF3 age-20 single-wall removal mandate; firm cost vs averted damage.*
*Pull: CF4 in spec §9 (renumbered to CF3 here). The Pigouvian / first-best CF is left OUT for now per
researcher (do not mention it, not even as deferred).*

### 6.3 — Welfare accounting  [DRAFTED]
#### ¶1 — The welfare identity and the wedge  [DRAFTED]
I score each counterfactual on one identity. Net social welfare is producer surplus minus external leak
damage minus government outlay, all in present value. Producer surplus is expected discounted facility
value. External damage is expected discounted leaks times the external cost per release $E$. Government
outlay is the subsidy, which enters only in the subsidy counterfactual. Premiums are a transfer inside
firm value, so they net out of social welfare. The external cost $E$ is a researcher input, set at a
placeholder of \$50,000 per release. This identity makes the wedge explicit. A facility pays only its
deductible out of pocket, the insurance pool covers the rest of the cleanup, and the health externality
$E$ falls outside both. So even a fairly priced risk-based premium internalizes only the insured part of
the social cost, and the gap to the first best is $E$.
*Goal: the welfare identity, premiums-net-out, and the deductible/pool/E wedge (the §6 payoff).*
*Pull: welfare = PS − external damage − outlay (spec §9). E_ext placeholder \$50k/release [researcher input].*

## Section 6 — Estimates, Counterfactuals & Welfare  [§ policy on a welfare scale]
*Talk 33–36. Each policy changes one term (bold). Welfare = producer surplus − external damage −
govt outlay (PV). E ≈ $50k/release, band reported; population-weighted in the final version.*
- 6.1 Each policy changes one term (slide 33) — CF1 TX-stays-flat-fee (bounded); CF2 SW→DW subsidy s∈[0,1]; CF3 Pigouvian +γ_r·H·E; CF4 age-A mandate
- 6.2 Welfare accounting (slide 34) — the uninternalized wedge (deductible vs pool vs E)
- 6.3 Counterfactual welfare results (slides 35–36) — Δ firm surplus / external damage / outlay; flat-fee vs risk-based sign+size [placeholder]; welfare-maximizing subsidy rate

## Section 7 — Conclusion  [§ bottom bread]
*Talk 37. Restate thesis in evolved language; synthesize reduced-form + structural; external
validity = the clustered fund sunsets; honest scaling boundaries.*

## Appendix — Robustness & Data Detail
*Tank-level Cox (counting-process at Dec 22 1998); make-model matched (CEM) sample as alt to
cell×year FE; full→analysis sample funnel; data quality by state; inference detail.
(No analytical/toy-model appendix — dropped per user.)*
