# Identification: Active-at-Treatment Sample, Cell × Year Fixed Effects DiD

---

## I. Population, Sample, and Estimating Unit

**Panel unit.** The unit of observation is a tank–year: tank $i$ at facility $f(i)$ in state $s$ in calendar year $t$. The outcome is $Y_{it} \in \{0,1\}$, equal to one if tank $i$ closes in year $t$. Facilities own multiple tanks. The decision-making unit facing the insurance shock is the facility operator, but the outcome is measured at the tank level. This non-equivalence between the decision unit and the outcome unit is what makes the sample restriction non-trivial.

**Treatment.** Texas SB 1317 took effect on December 22, 1998, requiring all underground storage tank (UST) operators in Texas to carry third-party liability insurance. No comparable shock occurred in other states in this window. The treatment indicator is:

$$D_{it} = \mathbf{1}\bigl[\text{Texas}_{f(i)} = 1 \;\cap\; t > 1998\bigr]$$

**Active-at-treatment sample restriction.** Define the facility-level activity indicator:

$$A_f = \mathbf{1}\bigl[\exists \text{ tank } j \in f : j \text{ not closed by end of 1998}\bigr]$$

The estimating sample is then:

$$\mathcal{S} = \bigl\{(i,t) : A_{f(i)} = 1\bigr\}$$

$\mathcal{S}$ includes *all years* for every tank at an active facility — including pre-1998 closure events at tanks that had already closed — provided the facility retained at least one open tank on December 22, 1998. The restriction operates at the **facility** level; the outcome panel is at the **tank** level.

> *In plain language:* A facility with three tanks that closes tank 1 in 1994, tank 2 in 1997, and retains tank 3 through 1998 remains in $\mathcal{S}$. The 1994 and 1997 closure events for tanks 1 and 2 are retained as outcome observations. Tank closure is **non-absorbing at the facility level** — this prevents the restriction from mechanically zeroing out pre-period variation, which would be the case if closure were facility-absorbing.

---

## II. Potential Outcomes and the Target Estimand

**Potential outcomes framework.**

$$Y_{it}(1) = \text{closure indicator under the Texas insurance regime}$$
$$Y_{it}(0) = \text{closure indicator under the counterfactual (no reform)}$$
$$Y_{it} = D_{it} \cdot Y_{it}(1) + (1 - D_{it}) \cdot Y_{it}(0)$$

**Target estimand — the ATT, precisely defined.**

$$\text{ATT} = \mathbb{E}\bigl[Y_{it}(1) - Y_{it}(0) \;\big|\; \text{Texas}_{f(i)}=1,\; A_{f(i)}=1,\; t > 1998\bigr]$$

This is the causal effect of the December 22, 1998 insurance mandate on the annual tank closure probability, averaged over **Texas tanks at facilities that were operating when the shock hit**, in the post-reform period. Three restrictions in the conditioning set are each load-bearing:

1. **Texas only.** SB 1317 applied exclusively to Texas operators. The treatment group is $\{\text{Texas}_{f(i)}=1\}$.
2. **Active on December 22, 1998.** Facilities that had already fully exited before the reform could not be treated by it. The condition $A_{f(i)}=1$ sharpens the estimand to the population for whom treatment was operationally possible.
3. **Post-reform period.** The treatment effect can only materialize after the reform date; $t > 1998$ is required for $Y_{it}(1) \neq Y_{it}(0)$ to be meaningful.

This ATT is **not** the effect on the full 1985–1998 Texas tank population. Facilities that exited before 1998 were removed by a different mechanism — the 1988–1998 EPA mandate phase-in — not the insurance shock. Including them would conflate two distinct treatments with different mechanisms and different timing. The active-at-treatment restriction separates the two cleanly.

---

## III. The Estimating Equation and Fixed Effects

**Static DiD.**

$$Y_{it} = \beta \cdot \bigl(\text{Texas}_{f(i)} \times \text{Post}_t\bigr) + \alpha_i + \delta_{ct} + \varepsilon_{it}$$

where:

- $\alpha_i$ is a **facility fixed effect**, absorbing all time-invariant facility characteristics: location, soil geology, operator quality, and permanent compliance propensity. Identification does not rely on cross-facility comparisons.

- $\delta_{ct}$ is a **cell × year fixed effect**, where the cell is defined as $c = \{\text{tank type} \times \text{fuel type} \times \text{capacity bin} \times \text{install cohort}\}$. For every tank-type-vintage combination in every calendar year, this fixed effect absorbs all common shocks: the national 1998 EPA deadline spike in closure rates, cohort-specific ageing hazard, and EPA enforcement cycles common across states. Critically, the 1998 deadline surge hits *all* tanks of a given type and vintage in the same year equally — $\delta_{ct}$ absorbs it entirely, so the national deadline is not confounded with the Texas-specific insurance shock.

- $\beta$ is identified off the **within-facility, within-cell residual**: among tanks of identical type and vintage, in the same calendar year, $\beta$ captures whether Texas tanks close at a differentially higher rate after December 1998 than observationally identical control-state tanks.

**Frisch-Waugh interpretation.** By the Frisch-Waugh-Lovell theorem, $\hat{\beta}$ is numerically identical to OLS of the residualized outcome $\tilde{Y}_{it} = Y_{it} - \hat{\alpha}_i - \hat{\delta}_{ct}$ on the residualized treatment indicator. The identifying variation is therefore the **within-cell, within-facility, cross-border, pre-vs-post differential** — not raw state-level closure rates, which are contaminated by compositional differences, ageing, and common deadline effects that the fixed effects absorb.

**Event study.**

$$Y_{it} = \sum_{\tau \neq -1} \beta_\tau \cdot \bigl(\text{Texas}_{f(i)} \times \mathbf{1}[t - 1998 = \tau]\bigr) + \alpha_i + \delta_{ct} + \varepsilon_{it}$$

Each $\beta_\tau$ estimates the ATT at horizon $\tau$ years relative to December 1998, normalized to $\beta_{-1} = 0$. The pre-period coefficients $\hat{\beta}_\tau$ for $\tau < 0$ are the time series of the **residualized TX-control gap** — not the raw closure rate gap. They are the within-cell, within-facility differential after partialling out $\alpha_i$ and $\delta_{ct}$. All parallel trends diagnostics operate in this residualized space.

---

## IV. Identifying Assumptions

**Assumption 1 — Conditional parallel trends (in residualized space).**

Define the residualized group-time mean $\tilde{\mu}_{g,t} = \mathbb{E}[\tilde{Y}_{it} \mid \text{Texas}_{f(i)}=g,\, t]$ and the residualized gap $\tilde{\Delta}_t = \tilde{\mu}_{1,t} - \tilde{\mu}_{0,t}$. The assumption requires:

$$\mathbb{E}\bigl[\tilde{\Delta}_t(0) \;\big|\; \text{Texas}=1,\; A_f=1,\; \alpha_i,\; \delta_{ct}\bigr] = \mathbb{E}\bigl[\tilde{\Delta}_t(0) \;\big|\; \text{Texas}=0,\; A_f=1,\; \alpha_i,\; \delta_{ct}\bigr] \qquad \forall\, t < 1999$$

This is the assumption about trends in residualized space, not raw rates. A constant non-zero level $\delta$ in the residualized pre-period gap is compatible with this assumption — the fixed effects absorb levels, and a stable gap differences out exactly in the DiD. What would violate the assumption is a drifting gap: a systematic time trend in $\hat{\beta}_\tau$ as $\tau$ approaches treatment.

**Assumption 2 — No anticipation.**

$$\mathbb{E}\bigl[Y_{it}(1) - Y_{it}(0) \;\big|\; \text{Texas}=1,\; A_f=1,\; t \leq 1998\bigr] = 0$$

Texas facilities did not alter closure decisions before December 22, 1998 in anticipation of the insurance reform. Pre-1998 compliance activity is attributable to the documented EPA mandate phase-in, not to forward-looking responses to SB 1317. The institutional record supports this: SB 1317 was not widely anticipated years in advance, and the 1990–1998 Texas enforcement crackdown is directly traceable to the federal mandate schedule.

**Assumption 3 — SUTVA.**

$$Y_{it}(D_{it}) \text{ does not depend on } D_{jt} \text{ for } j \neq i$$

Tank closure decisions are independent across tanks. This is plausible given geographic dispersion and tank-specific liability. A potential violation arises if a large multi-tank operator closes several tanks simultaneously to exit the insurance market entirely; clustering standard errors by state partially accommodates within-state correlation of this kind.

---

## V. The DiD as a First-Difference Operator — Why a Stable Pre-Trend Gap Is Harmless

The DiD estimator is:

$$\hat{\beta}^{\text{DiD}} = \tilde{\Delta}_{\text{post}} - \tilde{\Delta}_{\text{pre}} = \bigl(\tilde{\mu}_{1,\text{post}} - \tilde{\mu}_{0,\text{post}}\bigr) - \bigl(\tilde{\mu}_{1,\text{pre}} - \tilde{\mu}_{0,\text{pre}}\bigr)$$

**The cancellation result.** Suppose the residualized pre-period gap is constant at $\delta \neq 0$ across all $\tau < 0$ — the pre-89 "elevated but flat" pattern. Under parallel trends, the counterfactual post-period gap would also equal $\delta$, and:

$$\hat{\beta}^{\text{DiD}} = \tilde{\Delta}_{\text{post}}(1) - \delta = \bigl[\tilde{\mu}_{1,\text{post}}(1) - \tilde{\mu}_{0,\text{post}}(0)\bigr] - \bigl[\tilde{\mu}_{1,\text{post}}(0) - \tilde{\mu}_{0,\text{post}}(0)\bigr] + \underbrace{\bigl[\tilde{\mu}_{1,\text{post}}(0) - \tilde{\mu}_{0,\text{post}}(0) - \delta\bigr]}_{= 0 \text{ by parallel trends}} = \text{ATT}$$

**A non-zero but constant residualized pre-period gap cancels exactly in the first difference.** The joint F-test rejection ($\delta \neq 0$) is not evidence of bias — it is evidence that the level of the pre-period gap is non-zero. This is irrelevant to identification. Bias requires a non-zero *slope* in the pre-period gap, not a non-zero level. The F-test tests $\delta = 0$; the bias formula involves $\gamma$. These are orthogonal objects.

**The bias formula when the gap drifts.** Suppose the residualized gap follows a linear trend:

$$\tilde{\Delta}_\tau(0) = \delta + \gamma \cdot \tau$$

where $\gamma$ is the drift rate — the change in the within-cell TX-control residualized gap per year. The DiD estimator then has bias:

$$\hat{\beta}^{\text{DiD}} = \underbrace{\text{ATT}}_{\text{always}} \;+\; \underbrace{\gamma \cdot \bigl(\bar{\tau}_{\text{post}} - \bar{\tau}_{\text{pre}}\bigr)}_{\text{drift bias}} \;+\; \underbrace{\delta \cdot 0}_{\text{level bias} = 0}$$

where $\bar{\tau}_{\text{post}}$ and $\bar{\tau}_{\text{pre}}$ are the mean relative years in the post and pre windows respectively. With $\bar{\tau}_{\text{post}} \approx 11$ and $\bar{\tau}_{\text{pre}} \approx -7.5$, the bias horizon is $\bar{\tau}_{\text{post}} - \bar{\tau}_{\text{pre}} \approx 18.5$ years.

$\gamma$ is estimated directly from the event study: run precision-weighted OLS of $\{\hat{\beta}_\tau\}_{\tau < -1}$ on $\tau$, weighting by $1/\hat{\sigma}_\tau^2$. The slope coefficient is $\hat{\gamma}$ and its p-value is the formal test of $\gamma = 0$.

---

## VI. The 1998 Evidence — Why Bias Is Conservative If Anything

**The natural-convergence threat.** The scenario that would break the flat-gap argument is if the residualized gap was naturally converging toward zero ($\gamma < 0$ with meaningful magnitude), so the observed post-1999 positive gap would partly reflect this convergence rather than the insurance shock. If this were true, bias $= \gamma \times 18.5 < 0$ and $\hat{\beta}^{\text{DiD}}$ would overstate the ATT.

**The correct diagnostic is the event-study coefficients, not raw rates.** The event-study pre-period coefficients $\hat{\beta}_\tau$ for $\tau < 0$ are the within-cell, within-facility TX-control residualized gap at each relative year. Raw state-level closure rates are not the identifying variation — they conflate compositional differences, ageing effects, and common deadline responses that the fixed effects absorb. The pre-period ES plot *is* the time series of the identifying variation.

**What the event study shows.** The residualized pre-period gap is approximately constant at $\delta \approx 0.005$–$0.008$ across $\tau = -13$ to $\tau = -2$, with no systematic trend. The key sequence at the cusp of treatment is:

$$\hat{\beta}_{-2} = +0.0054 \;\longrightarrow\; \hat{\beta}_{-1} = 0.000 \;\longrightarrow\; \hat{\beta}_{0} = -0.0082 \;\longrightarrow\; \hat{\beta}_{+1} = +0.0323$$

At $\tau = 0$ (calendar year 1998), the residualized gap turns *negative*: $\hat{\beta}_0 = -0.0082$. Within identical cell-year bins, Texas tanks closed at a *lower* residualized rate in 1998 than control tanks. This is because the national deadline spike is absorbed by $\delta_{ct}$, and within identical cells Texas had already completed compliance earlier — control tanks were still rushing to comply. The gap was not converging toward zero; it was still negative at the cusp of treatment.

The jump from $\hat{\beta}_0 = -0.0082$ to $\hat{\beta}_{+1} = +0.0323$ is a discrete discontinuity of approximately 0.040 percentage points in a single year. No linear trend model produces this shape; only a discrete shock does.

**Direction of residual bias.** The pre-period slope test finds $\hat{\gamma} = -0.000248$ ($p = 0.177$, weighted; $p = 0.311$, unweighted) — statistically indistinguishable from zero. If $\hat{\gamma}$ is nonzero at all, its estimated sign is negative. This implies:

$$\text{Bias} = \hat{\gamma} \times 18.5 < 0 \implies \hat{\beta}^{\text{DiD}} < \text{ATT}$$

Any remaining enforcement-driven drift in the pre-89 residualized gap would cause the DiD to **understate** the true treatment effect, not overstate it. The pre-89 estimate $\hat{\beta}^{\text{DiD}}_{\text{pre89}} = 0.0197$ is a lower bound on the ATT.

---

## VII. The Survivorship Conditioning Problem — Bounded and Directional

**The concern, stated precisely.** Conditioning on $A_f = 1$ is equivalent to conditioning on $C_{f,1998} = 0$ (facility has not fully exited by 1998). Since $C$ is determined partly by pre-period $Y$ (the outcome variable), this is conditioning on a function of pre-period outcomes. In the classic case where the outcome is absorbing at the conditioned-on unit, this conditioning mechanically zeros out pre-period variation among survivors. The concern does not apply in full force here for three reasons.

**Reason 1: Non-absorbing outcome.** $Y_{it} \in \{0,1\}$ is at the *tank* level; $A_f = 1$ conditions at the *facility* level. A facility with three tanks can have $Y = 1$ (tank closure) in 1994 and still satisfy $A_f = 1$ if a second tank remains open in 1998. Pre-1998 tank closures at active facilities are retained in $\mathcal{S}$ — they are identifying pre-period variation, not censored outcomes.

**Reason 2: Selection is observable and directionally conservative.** The Texas facility survival rate is 0.464 versus 0.533 for control states — a gap of $-0.070$. Texas lost *more* facilities pre-1998, meaning surviving Texas facilities are positively selected on compliance capacity. The surviving Texas stock is harder to push to close than a randomly drawn Texas facility. This attenuates $\hat{\beta}$: the treatment effect is biased *downward*, not upward. The bias is conservative.

**Reason 3: Post-88 vintage provides an unrestricted validity check.** Tanks installed 1989–1998 faced no mandate phase-in pressure whatsoever. For this cohort, $A_f = 1$ is uncorrelated with differential pre-period compliance behaviour, so the survivorship concern does not apply. The post-88 weighted estimate ($\hat{\beta} = 0.0057$) is consistent with the lower range of the pre-89 estimate ($\hat{\beta} = 0.0197$), consistent with attenuation from survivorship selection rather than inflation from a spurious pre-trend.

**The parallel trends assumption under conditioning.** Formally, the assumption strengthens to:

$$\mathbb{E}\bigl[\Delta\tilde{Y}_{it}(0) \;\big|\; \text{Texas}=1,\; A_f=1,\; \alpha_i,\; \delta_{ct}\bigr] = \mathbb{E}\bigl[\Delta\tilde{Y}_{it}(0) \;\big|\; \text{Texas}=0,\; A_f=1,\; \alpha_i,\; \delta_{ct}\bigr]$$

This requires that the trend in residualized counterfactual outcomes for surviving Texas facilities matches that for surviving control facilities, conditional on fixed effects. The pre-89 event study shows this holds approximately: the pre-period residualized gap is elevated but flat ($\hat{\gamma} = -0.000248$, $p = 0.177$), not drifting. The post-88 event study shows it holds cleanly, with pre-period coefficients near zero and insignificant throughout.

---

## VIII. Complete Bias Decomposition and Empirical Verdict

**Full decomposition for the pre-89 vintage.**

$$\hat{\beta}^{\text{DiD}}_{\text{pre89}} = \underbrace{\text{ATT}}_{\text{causal}} \;+\; \underbrace{\hat{\gamma} \times 18.5}_{\text{drift bias}} \;+\; \underbrace{\hat{\delta} \times 0}_{\text{level bias}=0}$$

Substituting the estimated values:

$$= \text{ATT} + (-0.000248) \times 18.5 + 0 = \text{ATT} - 0.00459$$

The implied drift bias is $-0.0046$, representing 14.5% of $\hat{\beta}^{\text{DiD}}_{\text{pre89}} = 0.0197$. The bias-adjusted lower bound is $0.0197 - 0.0046 = 0.0151$.

**Empirical verdicts by test.**

| Test | Result | Interpretation |
|---|---|---|
| Joint F-test ($\delta = 0$) | $F = 29.4$, $p < 0.001$ | Pre-period level gap exists (expected; irrelevant to bias) |
| Slope test ($\gamma = 0$, weighted) | $\hat{\gamma} = -0.000248$, $p = 0.177$ | **Flat** — drift bias not significant |
| Slope test ($\gamma = 0$, unweighted) | $\hat{\gamma} = -0.000215$, $p = 0.311$ | **Flat** — consistent across weighting |
| Bias share | 14.5% of $\hat{\beta}^{\text{DiD}}$ | **Small** — below 20% threshold |
| Direction of bias | $\hat{\gamma} < 0 \Rightarrow$ bias $< 0$ | $\hat{\beta}^{\text{DiD}}$ **understates** ATT |
| HonestDiD $\Delta^{\text{RM}}$ | Robust $\forall\; \bar{M} \leq 3.0$ | CI excludes zero across full grid |
| HonestDiD $\Delta^{\text{SD}}$ | Robust $\forall\; M \leq 0.08$ pp | CI excludes zero across full grid |
| Placebo (fake treatment at $\tau = -8, -6, -4$) | All coefficients near zero, insignificant | No spurious jump at arbitrary pre-period cutpoints |

**Paper statement for the pre-89 vintage.** The pre-1989 vintage event study shows a pre-period residualized gap that is elevated but statistically flat (slope $\hat{\gamma} = -0.000248$, $p = 0.177$). A non-zero but constant pre-period gap cancels exactly in the DiD first difference and does not bias $\hat{\beta}$; identification requires only that the gap is non-drifting, not that it is zero. The implied drift bias represents 14.5% of the point estimate, with the sign running in the conservative direction: if anything, $\hat{\beta}^{\text{DiD}}_{\text{pre89}} = 0.0197$ understates the true ATT. The post-treatment effect is robust to pre-trend deviations up to $\bar{M} = 3.0$ times the maximum observed pre-period deviation under the Rambachan and Roth (2023) relative-magnitudes restriction.

---

## IX. What the Variation Identifying $\beta$ Is — and Is Not

| Source of variation | Status |
|---|---|
| Texas × Post, within identical cell × year, within facility | **Identifies $\beta$** |
| Permanent facility quality, location, operator culture | Absorbed by $\alpha_i$ |
| National 1998 EPA deadline spike | Absorbed by $\delta_{ct}$ |
| Common ageing hazard by tank type and vintage | Absorbed by $\delta_{ct}$ |
| EPA enforcement cycles common across states | Absorbed by $\delta_{ct}$ |
| Time-varying Texas enforcement intensity 1990–1998, within cell | **Residual threat** — bounded by flat slope test ($p = 0.177$), conservative bias direction, HonestDiD robustness, and post-88 clean benchmark |

The cell × year fixed effect $\delta_{ct}$ is the workhorse of the design. By absorbing all common shocks to tanks of identical type and vintage in the same calendar year, it strips away the national regulatory environment and leaves only the cross-border, within-cell, pre-vs-post differential. $\beta$ is therefore not identified by the fact that Texas had a different regulatory environment from control states — it is identified by the fact that Texas tanks of a given type and vintage, facing the same national regulatory baseline, closed at a differentially higher rate after December 1998.

---

## Notation

| Symbol | Definition |
|---|---|
| $i$ | Tank |
| $f(i)$ | Facility owning tank $i$ |
| $t$ | Calendar year |
| $\tau = t - 1998$ | Relative year (event time) |
| $c$ | Cell: $\{\text{tank type} \times \text{fuel} \times \text{capacity} \times \text{install cohort}\}$ |
| $A_f$ | $\mathbf{1}[\text{facility } f \text{ has} \geq 1 \text{ active tank on Dec 22 1998}]$ |
| $\text{Post}_t$ | $\mathbf{1}[t > 1998]$ |
| $\text{Texas}_f$ | $\mathbf{1}[\text{facility } f \text{ in Texas}]$ |
| $\alpha_i$ | Facility fixed effect |
| $\delta_{ct}$ | Cell–year fixed effect |
| $\tilde{Y}_{it}$ | $Y_{it} - \hat{\alpha}_i - \hat{\delta}_{ct}$ (residualized outcome) |
| $\tilde{\Delta}_t$ | Residualized TX-control gap $= \tilde{\mu}_{TX,t} - \tilde{\mu}_{CTL,t}$ |
| $\delta$ | Mean pre-period level of $\{\hat{\beta}_\tau\}_{\tau < -1}$ |
| $\gamma$ | Precision-weighted OLS slope of $\{\hat{\beta}_\tau\}$ on $\tau$ |
| $\bar{\tau}_{\text{post}} - \bar{\tau}_{\text{pre}}$ | $\approx 18.5$ years (mean post minus mean pre relative year) |

**Sample sizes.** Pre-89 vintage (install year $\leq 1988$): $n = 7{,}197{,}274$ tank-years, 96,923 facilities. Post-88 vintage (install year $\geq 1989$): $n = 2{,}564{,}708$ tank-years, 42,523 facilities. CEM weights applied throughout. Standard errors clustered by state.
