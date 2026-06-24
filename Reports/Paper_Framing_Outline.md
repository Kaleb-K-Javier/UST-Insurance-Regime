# Paper Framing & Bullet Outline
## *Internalizing Environmental Risk: Insurance Design and Firm Behavior in Hazardous Industries*

**Author:** Kaleb K. Javier

---

# 1. Introduction (§1)

## ¶1 — The problem (USTs, scale, human cost)

- USTs are the leading documented source of groundwater contamination in the U.S. (EPA 2024). EPA had confirmed **~528,000 leaks** by Sept 2015 — for scale, the National Priorities List has 1,323 Superfund sites (Marcus 2021).
- ~500,000 USTs currently operate nationwide, ~80% under gas stations, holding gasoline and diesel.
- **~60% of UST facilities experience at least one leak** during their operating life (Marcus 2021).
- **>75% of leaking sites involve groundwater contamination**; one gallon of leaked gasoline can render up to 1 million gallons of water undrinkable.
- **Half the U.S. population and nearly all rural residents rely on groundwater for drinking water**.
- Gestational exposure to a leaking UST raises low birthweight and preterm birth by **7–8%** (Marcus 2021), comparable in magnitude to the food-stamp program or E-ZPass adoption.
- Cleanup cost: **median $136k, mean $403k** (2023 USD, n = 12,541 pooled across six control states). Federal and state remediation outlays exceed $1B/year (Marcus 2021).

## ¶2 — Risk is heterogeneous, observable, and predictable

- **Heterogeneous.** At age 24+, single-walled tanks leak at 12.9 per 1,000 facility-years vs 6.5 for double-walled (~2× ratio). Within single-walled, the age-hazard gradient runs from 2.9 → 12.9 per 1k across age bins 3–5 through 24+ (~4.5× steeper risk with age). Within double-walled, the same gradient runs 3.1 → 6.5 (~2×; much flatter). Source: 3.93M pre-treatment facility-years, 27,455 first-release events.
- **Observable.** Tank age and wall construction are recorded at the facility level in state and EPA inventories and have been for 30+ years. This is the "insurer can observe care" condition that makes compulsory insurance work in Shavell (2004).
- **Predictable.** A hazard model conditioned on age × wall + state FE has clean out-of-sample performance and tracks the Mid-Continent rate-filing schedule along the same risk dimensions.
- **What the three facts buy the paper**:
  1. There is real differential risk for risk-rated pricing to reach.
  2. The owner's incentive can be interpreted as a response to a risk-adjusted price, not a nominal label.
  3. Empirical model and rate-filing schedule can be validated against each other.
  4. The welfare-ceiling question (how far the priced regime can take us) reduces to how much of full social damage the observable attributes capture.

## ¶3 — The legal and policy lever

- **Strict liability.** Under federal environmental law (CERCLA, RCRA Subtitle I) and parallel state law, UST owners are strictly liable for cleanup costs and third-party damages from releases. In plain terms: the owner pays regardless of fault — even if they followed every regulation and could not have prevented the leak. The EPA and state regulators enforce this regime through corrective-action orders and fines, and it is this underlying liability that makes insurance economically meaningful in the first place.
- **Federal Financial Responsibility rule (1988, 40 CFR §280 Subpart H)**: every UST owner must demonstrate $1M per-occurrence coverage so that funds are available when liability is triggered. Federal law does not specify the form of that coverage.
- **38 states**: public trust fund pools every tank in the state at the same per-tank annual fee, no conditioning on risk attributes.
- **Texas (post-1999) + 7 others + DC + territories**: private insurance market. Premiums condition on tank age, wall construction, piping, leak-detection technology, fuel volume, and operator claims history (Mid-Continent rate filings, SERFF 2006–2024).

The private-market premium schedule takes the form

$$
P_{ijt} = \text{Base}_t \times \text{ILF}_{it} \times \left(1 + \sum_k \lambda_{k,t} X_{ijt,k}\right),
$$

where $\text{Base}_t$ is the per-tank base rate in filing period $t$, $\text{ILF}_{it}$ is the increased-limit factor (held fixed at the federal-minimum $1M/$1M coverage), $X_{ijt,k}$ are the priced characteristics, and $\lambda_{k,t}$ are filed credits and debits.

- Under a flat fund, the marginal cost to an owner of operating a riskier tank is zero. Under risk-rated pricing it is positive and rising in observable risk.

## ¶4 — Research questions

- **Q1: How does switching the financial-responsibility regime from flat-fee public insurance to risk-rated private insurance change firm behavior on tank closure, replacement, and confirmed-release outcomes?**
- **Q2: How much of the first-best social welfare gain does the risk-rated regime capture, and what determines the residual gap to first-best?**

## ¶5 — Setting, data, design

- Texas's December 1998 closure of the Petroleum Storage Tank Remediation Fund (driven by fiscal insolvency, not environmental policy).
- ~9.8M tank-years, 1990–2021, linking EPA inventories + TCEQ admin microdata + 18 control states that retained public funds.
- Identification: make-model matching on the observable risk attributes private insurers price (age × wall × fuel × capacity × install cohort), with facility fixed effects and cell × year fixed effects:

$$
Y_{it} = \beta \cdot (\text{Texas}_{f(i)} \times \text{Post}_t) + \alpha_i + \delta_{c(i)t} + \varepsilon_{it}.
$$

## ¶6 — Results 1 (reduced form)

- Closure: **+1.58 pp** on a 2.0% baseline (~80% increase); Cox HR 1.25.
- Vintage heterogeneity: pre-1989 install cohort **+3.1 pp**, post-1988 +0.6 pp → 4× larger response in the high-risk single-walled stock.
- LUST discoveries: **−1.27 pp** on a 1.73% baseline.
- Composition: among closing facilities, replacement share rises from 12.0% → 18.3% (+53% relative); aggregate tank counts ~unchanged. The fleet is being upgraded, not shrunk.

## ¶7 — Results 2 (structural welfare)

- DDC model: $\hat\kappa \approx 26R$, $\hat K \approx 5R$, $\hat\gamma_p = -0.29$, $\hat\gamma_r = 0.16$.
- Three counterfactual extensions at $E = $17k/release:
  - Subsidy ($K \to 0.5K$): reduces welfare
  - Pigouvian health-damage surcharge: reduces welfare
  - Age-25 mandate: roughly neutral
- → Priced regime captures the welfare gain reachable through the actuarial channel. The residual is bounded by what doesn't generate claims.

## ¶8 — Contribution

- **First firm-level empirical test of switching the compulsory insurance contract from public to private provision** in an environmental-liability setting. The paper applies the Hart-Shleifer-Vishny (1997) make-or-buy framework to mandatory liability insurance for groundwater contamination from underground storage tanks: the public trust fund is in-house provision, the private market is contracted-out provision, and the reform identifies the welfare effect of switching.
- **First firm-level empirical test of the insurance-contract side** of the Shavell financial-responsibility literature. The paper isolates the pure pricing channel of a mandatory insurance instrument, distinguishing the analysis from Boomhower (2019) and Ho et al. (2018), which test the asset-requirement / bonding leg of the same theoretical framework.
- Using a sharp fiscal discontinuity as the identifying event resolves the omitted-variable concerns and aggregate-data limits in earlier UST work (Yin et al. 2007, 2011). The make-model matched comparison is methodologically a sibling to the industry-matched control design La Porta & Lopez-de-Silanes (1999) used to evaluate privatization in Mexico.
- Mulder (2022) is the closest sibling in the insurance-pricing direction but operates with both information and price channels in flood insurance; this paper's setting mutes the information channel and isolates the pure price effect.
- Pairs reduced-form firm-response evidence with a structural welfare decomposition that identifies the unpriced channel — third-party health damages outside the actuarial claims domain — bounding the welfare reach of insurance pricing.

## Integration of the three theoretical anchors

- **Shavell** establishes *which instrument class is appropriate* in hazardous industries with judgment-proof injurers — compulsory liability insurance when insurers can observe care.
- **Hart-Shleifer-Vishny** establishes *who should provide that instrument* under incomplete contracts — the public sector in-house or the private sector contracted out, with the answer depending on the tradeoff between contractible improvements and non-contractible residual quality.
- **Diamond and the welfare-reach literature** provide the *quantitative scaffold* for evaluating how much of first-best welfare the chosen provider delivers when the contract conditions on a subset of damage-relevant attributes.
- The Texas reform is the empirical setting where all three meet: a switch from public to private provision of compulsory insurance, with the conditioning set widened from a uniform fee to a risk-rated schedule. The structural model in §11–12 evaluates how much welfare the private regime captures relative to first-best at varying values of the unpriced health externality $E$.

---

# 2. Related Literature (§2)

## Branch 1 — Shavell policy-instrument tree (why pricing matters)

- Strict liability disciplines firms only when the marginal injurer faces marginal expected damage (Shavell 1982).
- Judgment-proof problem (Shavell 1986): when injurer assets fall below the social cost of harm, liability fails to internalize.
- Two policy responses (Shavell 2004): minimum asset/bond requirements, and compulsory liability insurance. The latter dominates when insurers can verify risk-relevant attributes.
- Insurance mandates as a substitute for fines and process-mandate regulation (Yin, Pfaff, Kunreuther 2011): an insurance instrument can deliver internalization that ex-post fines (limited by bankruptcy) and ex-ante process mandates (limited by monitoring capacity) cannot.
- Shavell anchors the *why* of the paper: risk-rated compulsory insurance is the canonical fix for hazardous industries with limited-liability injurers, and the empirical question is how well it works in practice.

## Branch 2 — Public vs Private Provision under Incomplete Contracts (who provides the instrument)

- Hart, Shleifer & Vishny (1997): apply the incomplete-contracts make-or-buy framework (Grossman & Hart 1986) to the question of when government should provide a service in-house versus contract it out. Private providers have stronger incentives to both cut costs and improve quality on the contractible dimensions, but may degrade non-contractible quality. Private provision dominates when the residual non-contractible quality is small relative to the contractible improvements.
- Grossman & Hart (1986), Hart & Moore (1990), Hart (1995): foundational incomplete-contracts framework. Ownership rights to residual decisions matter when contracts cannot fully specify outcomes.
- La Porta & Lopez-de-Silanes (1999): empirical study of privatization in Mexico using industry-matched control firms before and after. Methodologically closest precedent for the make-model matched design in this paper. Privatization raises operating income through cost reduction (64% productivity, 31% labor, 5% prices).
- Levin & Tadelis (2010): empirical study of what drives the U.S. city-services make-or-buy decision. Services with harder-to-write performance contracts and higher quality sensitivity are less likely to be privatized.
- Vickers & Yarrow (1991), Megginson & Netter (2001): standard reviews of the privatization literature on agency concerns, incentive structure, and empirical performance under public vs private ownership.
- Yin, Pfaff & Kunreuther (2011): closest direct precedent in the UST literature — theoretical case-study comparing the insurance-mandate instrument to ex-post fines and ex-ante process mandates.

**Mapping to this paper.** The Texas reform is the Hart-Shleifer-Vishny make-or-buy decision applied to the compulsory liability insurance contract. The public trust fund is in-house provision; the private market is contracted-out provision. The contractible quality dimension that private provision improves is risk-based pricing; the contractible cost-cutting margin is administrative efficiency; the non-contractible residual is the third-party health externality $E$ that does not generate claims.

## Branch 4 — Welfare arithmetic scaffold (how much of first-best does the chosen provider deliver)

- Diamond (1973): when the instrument conditions on a subset of damage-relevant attributes, the second-best policy is a demand-derivative-weighted average of personalized Pigouvian taxes. Used here as the *form* of the counterfactual exercise — "how far from first-best is the chosen regime if $E$ takes value XYZ?" — not as a central organizing principle.
- Jacobsen, Knittel, Sallee & van Benthem (2020): sufficient-statistic R² for the welfare reach of a restricted-conditioning policy.
- Knittel & Sandler (2018): empirical implementation of the Diamond framework for the gasoline tax targeting local air pollution.
- Goulder & Parry (2008): standard instrument-choice review.

## Branch 5 — Structural-IO siblings (methodological precedents for the welfare counterfactual)

- Fowlie, Reguant & Ryan (2016): dynamic model of firm behavior under environmental regulation in a concentrated industry (cement). Closest methodological cousin on the structural side.
- Blundell, Gowrisankaran & Langer (2020): structural model of firm abatement investment under regulatory scrutiny; welfare gains from dynamic vs static enforcement.
- Muehlenbachs (2015): dynamic model of cleanup in oil and gas; estimates sunk costs from firms' well-plugging decisions. Methodologically the closest single precedent for the tank-retirement / exit-timing problem this paper estimates in §11.

## Branch 3 — Empirical environmental insurance and financial assurance

- Boomhower (2019): TX oil & gas insurance/bonding requirement; identifies the judgment-proof channel via firm-size composition. Asset-requirement leg of the financial-responsibility menu.
- Ho, Hsu, Cha & Rivera (2018): bonding requirements for oil and gas wells in the U.S.; evaluates financial-assurance policy for environmental liability. Together with Boomhower, the asset-requirement / bonding leg — distinct from the insurance-contract leg this paper opens up.
- Mulder (2022): NFIP flood risk reclassification; identifies both an information channel (homeowners learn their risk) and a price channel (premiums rise). Closest empirical sibling in the insurance-pricing direction.
- Yin, Kunreuther & White (2011): Michigan's mid-1990s switch from public assurance fund to private market; state-level aggregate counts; ~20% leak reduction.
- Yin, Pfaff & Kunreuther (2011): theoretical and case-study argument that insurance mandates outperform ex-post fines and ex-ante process mandates for UST risk.
- Yin et al. (2007), Ringleb & Wiggins (1990), Alberini & Austin (2002): firm-size response to liability/regulation in hazardous industries.

## Branch 6 — IO of insurance markets and selection (how priced insurance markets work)

The switch from a uniform-fee public fund to a risk-rated private market is a textbook selection-markets transition. This literature gives the framework for analyzing welfare under each regime and the channels through which switching operates.

- Einav, Finkelstein & Mahoney (2021) — *IO of Selection Markets*, Handbook of IO Vol. 4. Canonical recent reference for IO methods applied to insurance markets under adverse selection. Section 6 covers the welfare effects of customized pricing — directly the question this paper asks at the firm side.
- Akerlof (1970), Rothschild & Stiglitz (1976): foundational asymmetric-information / selection-equilibria theory. Flat-fee pools sustain adverse-selection equilibria; risk-rated separating contracts can be welfare-improving.
- Pauly (1968): canonical moral-hazard tradeoff — full insurance dulls precaution. Directly relevant to whether owners under the flat-fee regime have any premium-linked incentive to reduce risk.
- Einav, Finkelstein & Cullen (2010), Einav & Finkelstein (2011 JEP): empirical implementation of the selection-markets framework — demand and cost curves, welfare under uniform vs customized pricing.
- Aspelund & Russo (2026): the most direct recent precedent for applying selection-markets IO methodology to environmental policy (USDA Conservation Reserve Program auctions). Demonstrates selection-markets methods extend cleanly into environmental settings.

**Mapping to this paper.** The public flat-fee fund is a pooling equilibrium in the Akerlof-RS sense: every facility pays the same premium regardless of risk, high-risk facilities subsidized by low-risk facilities, premium-related incentive to reduce risk is zero. The private risk-rated market is the canonical separating equilibrium response. The welfare effect of switching operates through both the Diamond instrument-targeting channel AND the selection-markets channel (breaking the cross-subsidy that suppressed precaution).

## Branch 7 — IO and environmental regulation (methodology landscape)

- Kellogg & Reguant (2021) — *Energy and Environmental Markets, IO, and Regulation*, Handbook of IO Vol. 5. The methodology landscape paper for the field. Section 2.5 covers regulation of environmental damage at production sites and of site decommissioning — directly relevant for UST closure.
- Goulder & Parry (2008): standard instrument-choice review in environmental policy.
- Structural-IO siblings (Fowlie-Reguant-Ryan, Blundell-Gowrisankaran-Langer, Muehlenbachs) are the dynamic-structural precedents for §11–12.

## Methodology backdrop note

- Welfare object here is firm precaution under mandatory coverage rather than consumer insurance valuation. The selection-markets framework (above) gives the IO methodology, but the welfare statement is about firm behavior under externality internalization, not consumer surplus from insurance.

## UST-specific literature

- Health and amenity damages: Marcus (2021) — gestational LBW; Zabel & Guignet (2012), Guignet & Walsh (2017), Guignet et al. (2018) — hedonic property-value impacts.
- Older empirical UST work (Yin et al. 2007, 2011) — superseded by the panel-data approach here.

---

# 3. Institutional Background (§3)

## §3.1 What USTs are, physical characteristics, regulatory lock-in
- Definition: tank + connected piping with ≥10% volume underground.
- Physical risk: corrosion of bare-metal/fiberglass tanks in direct contact with soil.
- Wall construction is binary and irreversible: single-walled vs double-walled. Retrofitting is a full tank replacement, not an upgrade.
- 40 CFR Part 280: vintage-differentiated standards. Pre-1988 tanks grandfathered; any modification triggers reclassification under current (more stringent) standards.
- Connection to vintage-differentiated regulation literature (Stavins 2005, Gruenspecht 1982, Bushnell 2012, Heutel 2011, Sallee & Jacobsen 2023): the regulatory environment makes the binary close-vs-operate choice the operative behavioral margin.
- Aggregate composition of active fleet 1985–2020: SW share fell from 83% (1985) to 73% (1998 deadline) to 63% (2020); turnover is slow.
- **Implication**: environmental risk in this setting is driven by the pace of incumbent SW retirement, not by new entrants. Stock-management problem, not new-flow regulation.

## §3.2 The legal framework: strict liability for releases

- **Strict liability under federal and state law.** Under the Resource Conservation and Recovery Act Subtitle I, the Comprehensive Environmental Response, Compensation, and Liability Act (Superfund), and parallel state environmental statutes, the owner or operator of an UST is strictly liable for the cost of cleaning up a release and for any third-party damages it causes.
- **What "strict liability" means in plain terms.** The owner must pay even if they did nothing wrong — even if they followed every regulation, hired licensed installers, conducted required inspections, and could not have prevented the leak. There is no negligence requirement and no defense based on care. A release happens; the owner is on the hook.
- **Enforcement.** The EPA and state environmental agencies enforce this regime through corrective-action orders (the owner is directed to remediate the site), cost-recovery actions (the agency cleans the site and bills the owner), and administrative or civil fines for noncompliance with the technical and reporting requirements that frame the liability.
- **Why this matters for the paper.** The strict-liability regime is the *reason* compulsory insurance is economically meaningful. If owners were not strictly liable, the financial cost of a release would be uncertain and contestable, and an insurance mandate would have no clear object. Because the liability *is* strict, every release is a financial event whose magnitude is bounded by cleanup cost and third-party damages — exactly the object insurers price.
- **What the law does *not* internalize directly.** Strict liability covers cleanup and third-party damages that can be pursued in court (property, sometimes water-supply replacement). It does *not* automatically deliver compensation for diffuse third-party health damages — the chronic, low-dose exposures Marcus (2021) documents — because the link between a particular release and a particular health outcome is difficult to establish in litigation. This is the legal foundation of the $L$-vs-$E$ wedge that runs through §4 and §13.

## §3.3 Federal compliance mandate and pre-treatment dynamics
- 1984 HSWA → 1988 EPA technical standards → December 22, 1998 federal compliance deadline.
- Annual facility-level closure rates spike sharply 1996–1998 due to federal mandate.
- Texas's staggered upgrade schedule (1989–1993) — pre-treatment confound for pre-1988 cohorts.

## §3.4 Financial Responsibility rule and state trust fund regime
- 1988 Financial Responsibility Rule (40 CFR §280 Subpart H): $1M per-occurrence + $1M/$2M aggregate coverage requirement.
- State trust funds emerged as insurer-of-last-resort for small operators when private market couldn't supply coverage in late 1980s.
- 38 states: active or phased-out funds.
- Defining feature: uniform per-tank fee, no risk conditioning. Pooling equilibrium crowds out risk-rated private alternatives for high-risk tanks.

## §3.5 Texas's 1999 policy transition
- Petroleum Storage Tank Remediation Fund (PSTRF), 1989.
- By 1995, actuarial assessments showed insolvency from accumulated legacy claims.
- HB 2587 (1995): sunset of PSTRF effective December 22, 1998 — same date as federal compliance deadline.
- All TX UST owners required to secure private insurance or qualify for self-insurance (asset test).
- Driving force was fiscal, not environmental.

## §3.6 Post-transition private market
- 2007–2021: 89.8% of TX retail USTs use private commercial insurance; 10.2% self-insure; <3% other.
- Transition was rapid and near-complete within first post-reform year.
- Market structure: concentrated and stable; top-5 carrier share + HHI roughly flat over 14 years.
- Premium reconstruction from Mid-Continent Casualty SERFF rate filings (2006–2024); conditioning variables include tank age bin, wall, piping, leak-detection technology, fuel volume, and operator claims history.

---

# 4. Theoretical Framework (§4)

*Plain-prose first; theory-named idea second.*

## §4.1 Strict liability + judgment-proof problem
- Most single-station UST operators have less in assets than a single release costs to clean up (qualitative; cite Boomhower 2019 for the parallel oil-and-gas evidence).
- Under pure strict liability, expected liability bounded by firm assets; care and entry decisions are undersupplied.
- Federal FR rule requires compulsory insurance coverage to restore the link between operating risky equipment and bearing its cost — *if* the premium reflects how risky the equipment actually is.
- *Theory framing*: this is Shavell's (1986, 2004) compulsory-insurance solution to the judgment-proof problem. The dominance of compulsory insurance over pure asset/bond requirements rests on the observable-care condition — which the heterogeneity / observability / predictability facts in §1 confirm holds in this setting.

## §4.2 Public vs private provision of the compulsory insurance contract

- The reform changes both *who provides* the financial-responsibility contract and *how that contract is priced*. The Hart-Shleifer-Vishny (1997) framework characterizes when switching from in-house provision (public trust fund) to contracted-out provision (private insurance) improves welfare under incomplete contracts.
- The H-S-V tension applied to the UST setting:
  - **Contractible quality the private provider can improve.** Risk-based pricing on observable, predictable attributes — tank age, wall construction, piping, leak-detection, claims history. The Texas rate filings document that private insurers actively price these dimensions; the public trust fund did not.
  - **Contractible cost-cutting margin.** Administrative efficiency of premium collection, underwriting, and claims processing.
  - **Non-contractible quality the private cost-cutting could degrade.** The third-party health externality $E$ that does not generate insurer claims because release-to-diagnosis causation is hard to establish in litigation.
- H-S-V predict that private provision dominates when the contractible quality improvements (risk-based pricing) are large relative to the non-contractible residual ($E$). The empirical comparison turns on the relative magnitudes — what the structural model in §11–12 quantifies.
- This framing makes the reform something more than a rating-regime change: it is the textbook make-or-buy decision applied to compulsory liability insurance for an environmental externality, with the welfare-reach question now interpretable in the H-S-V vocabulary.

## §4.3 How owners respond to the price signal
- Four behavioral margins from the earlier theoretical writeup (`LUST_and_Insurance_Javier`):
  1. **Entry**: which technology to install at facility creation. Under flat fund, entry composition unaffected by risk. Under risk-rated pricing, higher-risk entrants face higher lifetime cost → composition shifts toward safer technology.
  2. **Technology choice**: in this setting, retrofit is binary and prohibitively expensive (vintage-differentiated regulation). The technology margin operates at entry and replacement only.
  3. **Replacement**: incumbent decides whether to replace existing tank with new one (resets age, allows technology upgrade). Under flat fund, no premium incentive to replace. Under risk-rated pricing, aging tanks face rising premiums → replacement becomes more attractive.
  4. **Exit (closure)**: incumbent decides whether to continue operation. Optimal exit when flow profit ≤ flow cost. Under risk-rated pricing, $\partial$cost$/\partial$age > 0 → exit threshold falls (firms exit sooner).

## §4.4 Welfare-reach scaffold for the counterfactual exercise

- The H-S-V framing identifies *what* drives the public-vs-private comparison. Diamond (1973) and the welfare-reach literature provide the *quantitative scaffold* for evaluating how much of first-best welfare the chosen regime delivers.
- Under either provider, the chosen contract conditions on a subset of damage-relevant attributes. When that subset captures cleanup cost $L$ but leaves the health externality $E$ unpriced, the welfare gap to first-best is bounded by how much of $E$ varies independently of the priced dimensions — the Jacobsen, Knittel, Sallee & van Benthem (2020) restricted-instrument welfare-reach question.
- Used here as the *form* of the counterfactual exercise rather than as a central organizing principle: the structural model in §11–12 evaluates how welfare under the private regime compares to alternative policies at varying values of $E$, answering the Diamond-style question "how far from first-best are we if the externality takes value $E$?"
- This positions Diamond as the methodological vocabulary for the welfare arithmetic, not as a co-equal theoretical anchor — the central organizing principles are Shavell (instrument class) and Hart-Shleifer-Vishny (provider choice).

## §4.5 Testable predictions

| Margin | Theory predicts under RBP | Empirical specification | Where tested |
|---|---|---|---|
| Closure | Higher exit hazard; concentrated in priced-high-risk cells | Cox + DiD | §9.1 |
| Replacement | Higher replacement rate; concentrated in older single-walled | Conditional-on-closing decomposition | §9.3 |
| Leak detection | Lower confirmed releases (selection + precaution) | LUST DiD | §9.2 |
| Heterogeneity by operator capacity | Larger effect for high-capacity (multi-site, branded, self-insured) | Triple-interaction DiD; structural type fit | §11–12 |

## §4.6 Why a structural model is necessary

- The structural model is not an add-on; it is the mechanism that turns reduced-form causal estimates into a quantified welfare statement.
- The reduced-form design identifies whether firms respond to the switch in instrument class on the closure, replacement, and release margins. It does not by itself deliver a present-value welfare comparison between the priced regime and counterfactual policies.
- Three considerations make the structural step necessary:
  1. **Exit timing of asset retirement is dynamic.** The closure decision is a stopping problem whose value depends on the full sequence of future premium and risk states.
  2. **Welfare reach of the priced regime requires a model.** Translating estimated behavioral parameters into a present-value comparison between observed Texas outcomes and alternative policy regimes (subsidy, Pigouvian surcharge, mandate) requires counterfactual simulation.
  3. **The L-vs-E gap can only be bounded structurally.** Varying the unpriced health externality $E$ in the counterfactual exercise identifies the threshold values at which complementary instruments outside the actuarial channel become welfare-improving.
- Structural-IO precedents: Fowlie, Reguant & Ryan (2016), Blundell, Gowrisankaran & Langer (2020), and Muehlenbachs (2015).

---

# 5. Data (§5)

## §5.1 Source records
- EPA UST/LUST databases (federal-level inventory and confirmed-release reporting)
- Texas Commission on Environmental Quality (TCEQ) administrative microdata
- 18 control-state UST/LUST databases (assembled state-by-state)
- Mid-Continent Casualty SERFF rate filings (2006–2024) for premium reconstruction

## §5.2 Panel construction
- Tank-level panel built up to facility-level for some specs
- Time coverage: 1990–2021
- Unit of observation: tank-year (~9.8M observations); also facility-year for descriptive work

## §5.3 Study states and matched sample
- Make-model matching cells: age × wall × fuel × capacity × install cohort
- Texas tanks matched to control-state counterparts within cell

## §5.4 Outcome variables
- Annual closure indicator (tank exits the active fleet)
- Confirmed release ("LUST event") indicator
- Replacement indicator (closure → new tank install at same facility within 6 months)

\newpage

### Table 1 — Sample descriptive statistics

\begin{center}
\textit{[Insert tabular content from `T_Desc_Stats_Slide.tex`.]}
\end{center}

**Notes:** Sample is the make-model matched tank-year panel, 1990–2021. Columns 1–2 report means and standard deviations for Texas-installed tanks; columns 3–4 report the same for tanks in the 18 matched control states; column 5 reports the standardized difference. The sample is restricted to tanks alive at the reform date (December 22, 1998) and matched within cells defined by tank age bin × wall construction × fuel type × capacity quartile × install-year cohort. Pre-treatment balance across the priced risk attributes is preserved by construction; remaining differences in operator-level characteristics are absorbed by facility fixed effects in the estimation. Wall-type composition is reported separately because the schedule prices this dimension most heavily; the over-representation of single-walled tanks in Texas (relative to controls) drives the headline heterogeneity in §9.

---

\newpage

# 6. Risk Heterogeneity and the Actuarial Foundation (§6)

## §6.1 Release probability is predictable from observables

The hazard model estimates the annual first-release probability conditional on observed tank attributes:

$$
h(t \mid x_i) = P(\text{first confirmed release in year } t \mid \text{no prior release}, x_i),
$$

where $x_i$ contains 3-year tank-age bins fully interacted with wall construction, state fixed effects, and portfolio controls (fuel type, capacity, piping). Estimation uses penalized logistic regression with lasso variable selection and inverse-frequency weights for the 0.70% base event rate. Out-of-sample evaluation uses 10-fold cross-validation with post-hoc recalibration. Sample: 3.93M pre-treatment facility-years, 27,455 confirmed first-release events.

\newpage

### Figure 1 — Single-walled tanks drive the age–hazard gradient

\begin{center}
\textit{[Insert Figure: `Figure_CV_CellRisk.png`.]}
\end{center}

**Notes:** Predicted annual first-release hazard rate by tank age bin and wall construction, from the penalized logistic hazard model fit on 3.93M pre-treatment facility-years. The horizontal axis is tank age in three-year bins; the vertical axis is the predicted hazard per 1,000 facility-years. Single-walled tanks (orange) display a steep age gradient — from 2.9 per 1,000 at ages 3–5 to 12.9 per 1,000 at ages 24+, a ~4.5× rise. Double-walled tanks (blue) display a much flatter gradient — from 3.1 to 6.5, a ~2× rise. Shaded bands are out-of-sample 95% prediction intervals. The figure documents the two empirical facts the actuarial schedule responds to: hazard rises with age (steeply for SW, mildly for DW) and is higher for SW than DW at every age.

\newpage

### Figure 2 — Premiums track the risk the hazard model predicts

\begin{center}
\textit{[Insert Figure: `Figure_Premium_vs_Hazard_Raw.png`.]}
\end{center}

**Notes:** Cell-level Mid-Continent Casualty premium (vertical axis) plotted against the hazard-model–predicted first-release probability (horizontal axis), across all (age × wall × piping × leak-detection) cells in the Texas private-insurance schedule. Premium is the annual per-tank rate at federal-minimum $1M/$1M coverage; predicted hazard is the out-of-sample hazard from the model in §6.1. Marker shape distinguishes single- vs double-walled cells; color indicates piping-system age. The schedule prices age and wall in directions consistent with the empirical hazard gradient: premiums rise with predicted release probability, and the SW–DW separation in premiums mirrors the SW–DW separation in hazard. The figure does *not* claim that premiums equal marginal social damage; it documents that the priced wedge is real along the dimensions the model can also predict.

\newpage

## §6.3 Realized cleanup cost distribution

Pooled stats across six control states with usable claims records (n = 12,541, 2023 USD): median **$136,249**, mean **$403,319**, 75th percentile **$494,962**, 95th percentile **$1,737,650**.

\newpage

### Figure 3 — Distribution of realized cleanup costs

\begin{center}
\textit{[Insert Figure: `Figure_cost_distribution_pooled.png`.]}
\end{center}

**Notes:** Kernel density of realized cleanup costs across 12,541 confirmed releases in six control states (Colorado, Louisiana, New Mexico, Pennsylvania, Tennessee, Utah), inflated to 2023 USD. The horizontal axis is on a log scale. The orange dashed vertical line marks the pooled median ($136,249); the gold dot-dashed line marks the pooled mean ($403,319). The distribution is right-skewed: the median is roughly one-third of the mean, and the 95th percentile exceeds $1.7M. State-level summary statistics are reported in Appendix Table A.1; cross-state heterogeneity is modest in shape but meaningful in scale (Louisiana median $240k; New Mexico median $57k).

---

\newpage

# 7. The Make-Model Comparison Sample (§7)

## §7.1 The composition problem
- Naive TX vs control comparison contaminated by differential fleet composition (TX has more SW pre-reform, different age distribution).
- Need to compare apples-to-apples on the dimensions the schedule prices.

## §7.2 Constructing the matched sample
- Cells defined by tank age × wall × fuel × capacity × install cohort.
- Texas tanks matched to control-state counterparts within cell; unmatched tanks dropped.

## §7.3 Why tank-level measurement
- Closure decision is tank-level (a facility may close some tanks, replace others).
- Facility-level measurement averages across heterogeneous tank decisions.

## §7.4 Pre-treatment balance
- Pre-treatment trends in matched sample reported in Appendix B.
- Event-study pre-period flat (see Figure 4).

---

# 8. Empirical Framework (§8)

## §8.1 Estimation strategy

**Static OLS difference-in-differences.** The baseline specification is

$$
Y_{it} = \beta\,(\text{Texas}_{f(i)} \times \text{Post}_t) + \boldsymbol\gamma' M_{it} + \alpha_i + \delta_{c(i)\,t} + \varepsilon_{it},
$$

where $Y_{it}$ is the closure indicator for tank $i$ in year $t$, $\text{Texas}_{f(i)}$ indicates that facility $f(i)$ is in Texas, $\text{Post}_t = \mathbf{1}[t \geq 1999]$, $M_{it}$ is a vector of federal compliance-mandate controls (release-detection, spill-overfill, integrity-protection deadlines), $\alpha_i$ are tank or facility fixed effects, and $\delta_{c(i)\,t}$ are cell-by-year fixed effects with $c$ indexing make-model–vintage cells. Standard errors are clustered at the state level and supplemented with wild-cluster bootstrap (see §9.1).

**Cox proportional-hazards model.** The tank-level survival specification is

$$
h_i(t) = h_{0,s(i)}(t)\,\exp\bigl\{\beta\,(\text{Texas}_{f(i)} \times \text{Post}_t) + \boldsymbol\eta'\,\text{state-dummies}_{s(i)}\bigr\},
$$

where $h_{0,s(i)}(t)$ is the baseline hazard, optionally stratified on install year, make-model cell, or full make-model–vintage cell. Estimation uses Efron's method for ties; standard errors clustered at the state level and supplemented with wild-score bootstrap.

**Event study.** Dynamic effects are estimated by

$$
Y_{it} = \sum_{\tau \neq -1} \beta_\tau \,(\text{Texas}_{f(i)} \times \mathbf{1}[t - 1998 = \tau]) + \alpha_i + \delta_{c(i)\,t} + \varepsilon_{it}.
$$

The reference period is $\tau = -1$ (calendar year 1997).

## §8.2 Threats to identification
- **Federal mandate**: Dec 1998 compliance deadline affects both groups; identification rests on differential response within cells the schedule prices.
- **Texas-specific upgrade schedule (1989–1993)**: pre-treatment confound for pre-1988 cohorts; address via sample restriction and cohort-specific event studies.
- **Anticipation**: closure spike could precede 1998 if owners anticipate fund closure. Event-study pre-trends are flat.
- **Unobserved heterogeneity**: facility FE absorbs time-invariant; cell × year FE absorbs common shocks within risk cells.
- **Reverse causality**: fund closure driven by fiscal insolvency, not by trends in TX risk landscape.

---

# 9. Reduced-Form Results (§9)

## §9.1 Tank closure — stepped DiD

The headline closure result is a difference-in-differences estimate of $\beta$ in the static OLS specification of §8.1, applied to the make-model matched alive-at-treatment tank-year sample. Seven OLS specifications walk through fixed-effects structure, and four Cox specifications walk through baseline-hazard stratification. The main OLS specification is Column 7 (tank fixed effects plus cell × year fixed effects); the main Cox specification is Column 4 (state fixed effects plus stratification on the full make-model–vintage cell).

\newpage

### Table 2 — Stepped DiD on tank closure (OLS)

\begin{center}
\textit{[Insert tabular content from `T_Stepped_DiD_OLS.tex`.]}
\end{center}

| Column | Tank/Facility FE | Cell FE | Year FE | Cell × Year FE | Mandate controls |
|---|---|---|---|---|---|
| (1) | — | — | panel_year | — | Y |
| (2) | — | cell_id | panel_year | — | Y |
| (3) | panel_id (facility) | — | panel_year | — | Y |
| (4) | tank_panel_id | — | panel_year | — | Y |
| (5) | — | — | — | cell × year | Y |
| (6) | tank_panel_id | — | — | cell × year | Y |
| (7) **main** | panel_id (facility) | — | — | cell × year | Y |

**Notes:** Outcome is the annual tank-closure indicator. All specifications include state fixed effects when no facility/tank FE is present, and three federal-mandate dummies (release-detection, spill-overfill, integrity-protection deadlines) interacted with each tank's calendar-year compliance status. Standard errors are clustered at the state level; wild-cluster bootstrap p-values and 95% confidence intervals are reported in the second standard-error row. Sample is the alive-at-reform make-model matched tank-year panel. Column (7) is the headline specification: facility fixed effects absorb time-invariant operator characteristics, and cell × make-model–vintage × year fixed effects absorb any common shock affecting all tanks of the same observable risk profile in the same year. The point estimate of +1.58 pp in column (7) is the within-cell, within-facility differential response of Texas tanks to the reform.

\newpage

### Table 3 — Stepped DiD on tank closure (Cox proportional hazards)

\begin{center}
\textit{[Insert tabular content from `T_Stepped_DiD_Cox.tex`.]}
\end{center}

| Column | State control | Baseline hazard stratification |
|---|---|---|
| (1) | factor(state) | None |
| (2) | factor(state) | strata(install_yr_int) |
| (3) | factor(state) | strata(make_model_noage) |
| (4) **main** | factor(state) | strata(cell_id) — full make-model–vintage cell |

**Notes:** Estimation uses Efron's method for ties on the two-episode Cox split of the alive-at-reform sample. Baseline hazards are stratified at progressively finer granularity moving across columns. Cox specifications omit the three federal-mandate dummies present in the OLS table, because in the two-episode Cox split panel_year is the episode midpoint year and is mechanically correlated with the failure indicator, driving mandate-dummy coefficients to ±∞; the OLS table's panel_year is the calendar year and admits the controls cleanly. Standard errors are robust and clustered at the state level; wild-score bootstrap p-values and 95% confidence intervals are reported in the second standard-error row. Column (4) is the headline specification: the baseline hazard is allowed to vary nonparametrically across the full set of make-model–vintage cells, so identification of $\beta$ comes only from the within-cell differential timing of Texas tanks' exits.

\newpage

### Figure 4 — Dynamic closure response in Texas relative to matched controls

\begin{center}
\textit{[Insert Figure: `Fig_ES_Full.pdf`.]}
\end{center}

**Notes:** Event-study coefficients $\hat\beta_\tau$ from the dynamic specification in §8.1, plotted against event time $\tau = t - 1998$. The reference period is $\tau = -1$ (calendar year 1997). The outcome is the annual tank-closure indicator; the specification includes facility fixed effects and make-model–vintage × year fixed effects, with standard errors clustered at the state level. The vertical dashed line at $\tau = 0$ marks December 22, 1998 — the federal compliance deadline and the Texas Petroleum Storage Tank Remediation Fund closure date. Pre-reform coefficients are statistically indistinguishable from zero, supporting parallel trends in the closure outcome between Texas and matched control-state tanks. Post-reform coefficients rise sharply between 1999 and 2003 and stabilize at approximately 1.6 percentage points above the matched control baseline. The dynamic profile is consistent with risk-rated pricing operating immediately upon regime switch and persisting indefinitely.

\newpage

## §9.2 Confirmed releases (LUST discoveries)

The LUST discovery specification replaces $Y_{it}$ in the static OLS equation with an indicator for a confirmed release. Headline result: a **−1.27 pp decline on a 1.73% baseline**. Decomposition into background (steady-state operational) and inspection-triggered (regulatory audit) channels — both decline.

\newpage

### Table 4 — LUST discovery DiD

\begin{center}
\textit{[Insert tabular content from `T_LUST_Incumbent_Slide.tex`.]}
\end{center}

**Notes:** Outcome is the annual confirmed-release indicator at the tank level. Columns walk through fixed-effects structure as in Table 2. Sample is the make-model matched panel excluding tanks with prior confirmed releases (first-release hazard). Background-vs-inspection decomposition uses TCEQ inspection-event records linked to release notifications; the corresponding event-study figure is in Appendix C.

\newpage

## §9.3 Composition shift to replacement

Conditional on closing, the replacement share — the fraction of closed facilities that install a new tank within six months at the same site — rises from **12.0% to 18.3%** under the risk-rated regime (+53% relative). Aggregate active tank counts are essentially unchanged.

### Table 5 — Composition of closure decisions

\begin{center}
\textit{[Insert tabular content from `T2b_Enriched_Slide.tex`.]}
\end{center}

**Notes:** The estimating sample is the subsample of facility-years in which a closure event occurs. The outcome is an indicator for "closure with replacement" — a closure event followed by a new tank installation at the same facility within six months. The coefficient on Texas × Post measures the within-facility shift in the conditional probability that a closure is followed by a replacement rather than a permanent exit. Specifications include facility fixed effects and make-model–vintage × year fixed effects; standard errors clustered at the state level. Because the conditioning event (closure) is itself endogenous to the reform, the composition estimate should be interpreted alongside the unconditional closure result in Table 2 — the joint pattern (closure rises, replacement-conditional-on-closing rises) implies the reform shifts behavior toward upgrading rather than exiting.

\newpage

## §9.4 Heterogeneity by install vintage

### Table 6 — Closure heterogeneity by install vintage

\begin{center}
\textit{[Insert tabular content from `T_HTE_DiD_Vintage.tex`.]}
\end{center}

**Notes:** Outcome is the annual tank-closure indicator. The reported coefficients are $\hat\beta_0$ on Texas × Post (post-1988 install cohort baseline) and $\hat\beta_1$ on the triple-interaction with the pre-1989 install-cohort dummy. The implied pre-1989 ATT is $\hat\beta_0 + \hat\beta_1 \approx +3.1$ pp; the implied post-1988 ATT is $\hat\beta_0 \approx +0.6$ pp. Specifications include facility fixed effects and make-model–vintage × year fixed effects. The 4× larger response in the pre-1989 cohort — the single-walled stock the rate schedule prices most heavily — is the empirical signature of risk-rated pricing rather than a generic Texas economic shock.

\newpage

### Figure 5 — ATT on closure by install vintage

\begin{center}
\textit{[Insert Figure: `Fig_Vintage_Forest.pdf`.]}
\end{center}

**Notes:** Forest plot of difference-in-differences point estimates and 95% confidence intervals, separately by install-year cohort. The horizontal axis is the estimated ATT in percentage points; the vertical axis lists install-year cohorts from oldest to youngest. The vertical line at zero marks the null. Each row is a separate regression on the cohort-restricted subsample, with the same fixed-effects structure as Table 6 column (7). The pattern is monotone: older install cohorts (1970s–1980s, predominantly single-walled) show the largest closure response under risk-rated pricing; younger install cohorts (1990s–2000s, predominantly double-walled) show coefficients near zero. The pattern is consistent with the priced wedge being largest in the older single-walled stock.

---

# 10. Discussion of Reduced-Form Results (§10)

- The empirical pattern matches the predictions of pricing-induced firm response: closure rises, replacement-conditional-on-closing rises, releases fall, and the response concentrates in the cells the schedule prices most heavily.
- The concentration in pre-1989 SW is the empirical signature of the priced regime — generic Texas economic shocks would not selectively affect the cells the rate schedule weights.
- Magnitude is large in absolute terms (~80% closure increase) but the question of whether this captures the bulk of available welfare gains needs the structural model.

---

# 11. Dynamic Discrete Choice Model (§11)

## §11.1 Agent's problem

Each period, a facility chooses among three actions: maintain the existing tank, exit (close permanently), or replace (close and install a new double-walled tank). The Bellman equation for the conditional value function is

$$
V(s) = \max\bigl\{\,u_M(s) + \beta\, \mathbb{E}\!\left[V(s') \mid s, \text{maintain}\right],\; u_E(s),\; u_R(s) + \beta\, \mathbb{E}\!\left[V(s') \mid s, \text{replace}\right]\bigr\},
$$

where $s$ is the state, $\beta = 0.95$ is the annual discount factor, and $u_M, u_E, u_R$ are flow utilities of maintaining, exiting, and replacing.

## §11.2 State space and transitions
State: $s = (a, W, R, \varepsilon)$, where $a$ is the tank-age bin (9 levels), $W$ is wall construction (2 levels), $R$ is regime (2 levels: flat-fund vs risk-rated). With idiosyncratic logit shocks $\varepsilon$, the deterministic state has $9 \times 2 \times 2 = 36$ cells; the replacement-only model uses $8 \times 2 \times 2 = 32$ cells. Age advances deterministically while maintaining; wall is fixed (no retrofit); regime is fixed by state.

## §11.3 Flow payoffs — four-parameter specification

In the baseline four-parameter model, flow utilities are common across wall types and regimes:

$$
u_M(s) = 1 + \gamma_p \cdot P(a, W, R) - \gamma_r \cdot h(a, W) \cdot L,
$$

$$
u_E(s) = \kappa,
$$

$$
u_R(s) = -K,
$$

where $P(a, W, R)$ is the annual premium at state $s$, $h(a, W)$ is the annual first-release hazard, $L$ is the average cleanup cost per release, and the leading $1$ in $u_M$ is annual net revenue $R$ normalized to one model unit. The parameter vector is $\theta = (\kappa,\, K,\, \gamma_p,\, \gamma_r)$, with $\gamma_p$ measuring premium sensitivity (expected sign: negative) and $\gamma_r$ measuring the degree to which owners internalize the expected cleanup cost component of a release (expected sign: positive, with $\gamma_r = 1$ corresponding to full internalization of $L$).

## §11.4 Flow payoffs — eight-parameter specification (robustness)

The eight-parameter extension allows scrap value and replacement cost to vary by wall construction, and allows premium sensitivity and risk internalization to vary by regime:

$$
u_M(s) = 1 + \gamma_{p,R} \cdot P(a, W, R) - \gamma_{r,R} \cdot h(a, W) \cdot L,
$$

$$
u_E(s) = \kappa_W,
$$

$$
u_R(s) = -K_W.
$$

The parameter vector is $\theta = (\kappa_{SW},\, \kappa_{DW},\, K_{SW},\, K_{DW},\, \gamma_{p,FF},\, \gamma_{p,RB},\, \gamma_{r,FF},\, \gamma_{r,RB})$. The wall-specific scrap and replacement costs allow the model to fit the SW vs DW closure-vs-replacement composition separately; the regime-specific $\gamma$ parameters allow the model to express different degrees of cost internalization under flat-fund and risk-rated regimes. A further 25-parameter extension layers 17 state fixed effects on the maintenance flow utility to capture residual regional heterogeneity; reported in Appendix D.

## §11.5 Identification
- $\kappa$ identified from observed exit rate at each state.
- $K$ identified from observed replacement rate at each state.
- $\gamma_p$ identified from premium variation across cells and over rate-filing periods.
- $\gamma_r$ identified from hazard variation across cells (age × wall) holding premium fixed.

## §11.6 Estimation
- Nested Pseudo-Likelihood (Aguirregabiria–Mira / Hotz–Miller).
- Inner-loop CCP updates and Bellman iteration in C++ for speed.
- Tolerances: $\text{tol}_\theta = 10^{-8}$, $\text{tol}_P = 10^{-7}$, max NPL iterations 600.

## §11.7 Counterfactual scenarios
- **Subsidy**: $K \to 0.5 K$ (replacement cost halved by government grant).
- **Pigouvian surcharge**: add $h(a) \cdot E$ to the maintain flow cost, with $E = 17{,}000$ from Marcus (2021).
- **Mandate**: force the maintenance choice probability to zero for tanks aged $\geq 25$.
For each scenario: recompute optimal conditional choice probabilities, simulate the long-run fleet steady state, and compute present-value welfare separately for firm surplus, government outlay, and environmental damage.

---

# 12. Structural Results and Welfare (§12)

## §12.1 Parameter estimates

\newpage

### Table 7 — Structural parameter estimates (four-parameter model)

\begin{center}
\textit{[Insert tabular content from `04f_Theta_Table_AM_SE.tex`.]}
\end{center}

**Notes:** Nested Pseudo-Likelihood estimates of $\theta = (\kappa, K, \gamma_p, \gamma_r)$ for the four-parameter dynamic discrete-choice model in §11.3. Standard errors in parentheses are computed by the Aguirregabiria-Mira sandwich formula. Estimation sample is Texas facility-years over 1999–2018 (post-reform equilibrium), mapped onto the 32 (age × wall × regime) state cells. Parameters are reported in units of annual net revenue $R$; the calibration anchor $R \approx \$10{,}000$ per facility per year yields dollar translations as shown. The estimated $\hat\gamma_r = 0.16$ is small relative to the fully-internalized benchmark of 1, indicating that owners respond to risk-priced premiums as a price signal but do not behave as if they fully bear the expected cleanup cost.

\newpage

### Table 8 — Structural parameter estimates (eight-parameter model, robustness)

\begin{center}
\textit{[Insert tabular content from `04h_Theta_Table_8param_AM_SE.tex`.]}
\end{center}

**Notes:** Estimates of $\theta = (\kappa_{SW}, \kappa_{DW}, K_{SW}, K_{DW}, \gamma_{p,FF}, \gamma_{p,RB}, \gamma_{r,FF}, \gamma_{r,RB})$ for the eight-parameter extension in §11.4. The wall-specific $\kappa_W$ and $K_W$ allow the model to match SW vs DW closure and replacement rates separately; the regime-specific $\gamma_{\cdot,R}$ allow internalization to differ under flat-fund and risk-rated regimes. Reported alongside the four-parameter baseline (Table 7) to assess sensitivity of the welfare counterfactuals to functional-form flexibility.

\newpage

## §12.2 Model fit
- CCP fit by cell ([04g_Per_Cell_Fit_Numbers_Wide.csv]) and aggregate action-share fit by year and treatment status ([04e_Action_Shares_by_Year.png]).
- Pre/post action shares match data within ~1 percentage point in major cells.

## §12.3 Counterfactual welfare arithmetic

\newpage

### Figure 6 — Welfare decomposition across counterfactual policies

\begin{center}
\textit{[Insert Figure: `04j_4p_Welfare_BarChart.pdf`.]}
\end{center}

**Notes:** Present-value welfare change (per-tank, model units, $E = \$17{,}000$ per release) under three counterfactual extensions of the risk-rated regime, decomposed into three components: firm surplus (operating profit net of premium and replacement cost), environmental damage avoided (cleanup cost $L$ plus health damage $E$), and government outlay (subsidy payments or mandate-implementation cost). The status quo is the post-1999 Texas risk-rated regime; bars show the welfare change relative to that baseline. All three counterfactual policies — replacement subsidy ($K \to 0.5K$), Pigouvian health-damage surcharge added to maintenance cost, and age-25 closure mandate — fail to deliver positive welfare gains at the calibrated $E$. The decomposition makes visible *why*: the subsidy's government outlay outweighs its firm-surplus and damage-avoidance gains; the Pigouvian surcharge induces over-closure relative to the risk-rated optimum; the mandate is roughly neutral, redistributing from incumbents to the government without net welfare change.

\newpage

## §12.4 Limitations
- Static premium schedule (no insurer learning over time).
- Unobserved operator heterogeneity (4p model abstracts; 8p+FE extension partial fix).
- Marcus health-damage calibration assumes uniform per-release $E$ — spatial heterogeneity in damage not modeled in this paper.

---

# 13. Discussion (§13)

## Risk-rated pricing is a partial fix
- The priced regime delivers what theory says on the dimensions actuarial premium-setting can reach: closure of risky cells, replacement composition shift, leak reduction.
- The welfare reach is bounded by what shows up in claims data.

## Why the three counterfactuals fail at $17k/release
- Subsidy: doesn't condition on risk; subsidizes replacement across all cells, including those that would replace anyway.
- Pigouvian add-on: layers an additional charge but doesn't change the conditioning set of the schedule; the residual variance in damage is uncorrelated with the priced wedge.
- Mandate: too blunt; forces exit at age 25 regardless of underlying risk profile or owner's outside options.

## What would close the residual wedge
- Extending the actuarial conditioning set to spatial attributes (proximity to drinking-water wells, downgradient population density) — administratively novel but technically feasible.
- Layering a complementary regulatory instrument that conditions on the missing dimension — geographically-targeted retirement program, vulnerability-weighted retirement subsidy.

## Broader environmental-policy debate
- Connects to the long-running discussion of market mechanisms vs direct regulation for environmental risk (Goulder & Parry 2008, Kellogg & Reguant 2021).
- Risk-rated insurance is a market mechanism that works on the dimensions it can reach; it is not a substitute for direct regulation on the dimensions outside its actuarial domain.
- Bounded endorsement: insurance pricing should be the first-line policy where the conditioning set is rich enough to capture most of the social damage; complementary regulation is required where it isn't.

## Connection to Boomhower and Mulder
- Boomhower: bonding requirement that screens out judgment-proof firms. Different policy lever (asset requirement); different setting (oil & gas); same underlying judgment-proof problem.
- Mulder: NFIP shift toward risk classification with information channel. Closer policy lever (insurance pricing); different setting (floods); has information channel that this paper's setting mutes.
- Together with this paper: three empirical tests of different features of compulsory financial-responsibility instruments in different hazardous-capital settings.

---

# 14. Conclusion (§14)

- Risk-rated pricing of compulsory liability insurance moves firm behavior on the margins theory predicts, with magnitudes large enough to materially shift the fleet composition over a decade.
- The welfare reach of insurance pricing alone is bounded by what insurer claims data can observe and price — in this setting, primarily cleanup cost rather than health damage.
- Where the priced conditioning set captures the bulk of social damage, risk-rated insurance can serve as a first-line policy instrument. Where it does not, complementary regulatory instruments conditioning on the missing dimensions are required.
- Three siblings — Boomhower (asset-requirement leg), Mulder (price + information in floods), this paper (pure-pricing leg in USTs) — together suggest that the appropriate financial-responsibility instrument depends on which channel binds in the particular hazardous-capital setting.

---

# Appendix

## A. Data appendix
- Full sample to analysis sample (sample restriction flow)
- Balanced-in-missingness check
- State-by-state data quality
- Texas's unique federal mandate upgrade schedule
- State-by-state claims cost distributions

## B. Robustness
- 8-parameter and 8p+FE structural specifications
- Alternative control state selections
- Pre-1988 cohort sensitivity
- Alternative matching cell definitions

## C. Premium reconstruction
- Mid-Continent Casualty SERFF filing detail
- Increased-limit factor calibration
- Control-state premium proxy from fund records

## D. Structural model robustness
- Welfare counterfactuals at alternative $E$ values ($0, $5k, $17k, $50k)
- Sensitivity to discount factor, scrap-value normalization
- Wall-specific, regime-specific, and 25-parameter extensions

---

# Reference: theory-citation map

- **Shavell 1982, 1986, 2004** — why pricing matters (judgment-proof + compulsory insurance + observable care)
- **Diamond 1973** — welfare arithmetic when instrument conditions on subset of damage attributes
- **Knittel & Sandler 2018** — empirical sibling for elasticity-damage correlation in Diamond framework
- **Jacobsen, Knittel, Sallee, van Benthem 2020** — R² sufficient statistic for restricted-instrument welfare reach
- **Goulder & Parry 2008** — instrument-choice review
- **Fowlie, Reguant & Ryan 2016** — structural-IO sibling for welfare arithmetic under entry/exit + externality
- **Yin, Pfaff & Kunreuther 2011** — insurance-mandate-vs-other-instruments comparison for USTs
- **Yin, Kunreuther & White 2011** — Michigan UST aggregate precedent
- **Marcus 2021** — health damages from UST leaks; source of $E$ calibration
- **Boyd 1997** — retrospective insurance / financial assurance literature

# Reference: three-paper neighborhood

- **Boomhower (2019)**: TX oil & gas, bonding requirement, judgment-proof channel via firm-size composition. *Asset-requirement leg of compulsory FR.*
- **Mulder (2022)**: NFIP flood risk classification reform. Information + price channels. *Insurance-pricing leg with information dimension.*
- **This paper**: TX USTs, public-fund → private-insurance switch. *Pure pricing channel, no information channel.*

# Reference: writing style notes

- Diamond / second-best / sufficient-statistic vocabulary belongs in §4 (theory) and §13 (discussion). Avoid in intro and reduced-form sections; the reader expects theoretical names where they're doing interpretive work, not as set-dressing.
- Empirical claims about premium-vs-MSD mispricing come from the structural model in §12, not from descriptive comparisons of expected loss to premium. The intro and §6 establish only that the schedule tracks predicted hazard along the dimensions both condition on.
- Lead each section with institutional fact or behavioral finding; bring in theoretical concept as the closing interpretive frame, not the opening hook.
- "Strict liability" is the legal regime; in §3.2 it is explained in plain English (owner pays regardless of fault). In §4.1 it is connected to the Shavell framework as theory.
- Display equations are centered on their own lines (not embedded in bullets). Bullets are for institutional facts, lists of conditions, and tabular content; equations and figures get their own typographic real estate.
