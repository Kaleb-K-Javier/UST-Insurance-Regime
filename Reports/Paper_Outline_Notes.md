# Paper Outline — Internal Working Notes

*Not for sharing. Companion document to [Paper_Framing_Outline.md](Paper_Framing_Outline.md). Tracks project status, open TODOs, and explicit out-of-scope decisions.*

---

## What's in good shape (done / stable)

- Reduced-form results: closure, leaks, composition all estimated and stable across specifications
- Structural model: 4-parameter NPL estimates converged; 8p and 8p+FE robustness available
- Welfare counterfactuals: three CFs computed at $E = $17k/release
- Mid-Continent Casualty SERFF rate-filing reconstruction (2006–2024)
- Hazard model + CV cell rates (3.93M pre-treatment facility-years)
- Cleanup cost distribution (6-state pooled)

## What needs work before submission

### Risk-predictability summary statistic
- Consolidate AUC / Brier / calibration slope / pseudo-R² (McFadden + Tjur) into a single intro-ready paper block
- Add variance-decomposition: share of cross-tank hazard variance explained by (age × wall) alone vs (age × wall + state FE + portfolio controls + spatial covariates). The marginal contribution of spatial covariates is the empirical hand-off to the welfare-ceiling story.
- Target output: one-line summary of the form *"The hazard model conditioned on actuarial observables achieves an out-of-sample AUC of X with calibration slope Y; adding spatial covariates the rate-filing schedule does not condition on raises out-of-sample R² by Z pp."*
- Code: `Code/Analysis/Descrptive Facts/01n_CVValidation.R`, `Code/Analysis/06_Actuarial_Alignment.R`, `Code/Analysis/07_Risk_score_alg.r`
- Most pieces already exist in `Table_CV_*.csv` outputs — likely consolidation work, not new modeling.

### Heterogeneity proxies for operator capacity
- Replace the cash-endowment $y_i$ language from the older theoretical writeup with observable proxies in the actual paper
- Candidates: number of facilities operated by the same owner (size proxy); brand affiliation (corporate parent backing); self-insurance status (revealed-preference asset test passed)
- Boomhower (2019) uses firm size for the analogous oil-and-gas heterogeneity test — same move here
- Where it lands: §4.3 (theory) and a heterogeneity subsection in §9

### Consolidate the four-margin theoretical derivation
- The older `LUST_and_Insurance_Javier` writeup has the clean comparative-statics derivation for the four DWL components
- Pull that into §4 of the paper, reformulated with $\theta$ (operator type) rather than $y_i$ (cash endowment)

### JKSvB R² welfare diagnostic placement
- Decide whether to compute the cell-level R² of $\hat\phi_j$ (full social damage) on actuarial premium $C(a_j, W_j, R_j)$ across the regulated fleet
- Two versions: $L$-only (predicted cleanup cost on premium) and $L + E$ (predicted total social damage on premium, using Marcus calibration for $E$)
- The two R² values give a single-number summary of welfare reach
- Likely lives in §12 alongside the counterfactual bar chart, or as a §6 diagnostic

## What's explicitly out of scope

### Owner-equity / asset-constraint quantitative claim
- No firm-level financial data currently
- Boomhower has the parallel oil-and-gas evidence; in this paper, treat owner asset constraints as a qualitative claim and cite him

### Empirical claim that premiums are mispriced relative to MSD
- Don't try to make this descriptively (it requires assumptions about $E$ that the structural model handles)
- The structural model in §12 quantifies welfare under counterfactual premium structures — that's where the mispricing arithmetic lives

### Hendren-style insurance valuation framework
- Wrong margin (consumer WTP for coverage under selection, not firm precaution under mandatory coverage)
- Not load-bearing; one-sentence mention in §2 methodology backdrop is enough

### Induced-technology / innovation channel
- Empirically muted in this setting (~0% retrofit rate; wall construction is irreversible)
- Lectures 13–14 (Sallee innovation) don't add traction here

### General equilibrium / cross-state spillover effects
- Out of scope for current paper; potential follow-up

## Project memory cross-references

- [`project_risk_predictability_metrics_followup.md`](../../../.claude/projects/C--Users-kaleb-Documents-ust-ins-move-to-github/memory/project_risk_predictability_metrics_followup.md) — risk-model predictability stats consolidation
- [`wall_specific_kappa_K_followup.md`](../../../.claude/projects/C--Users-kaleb-Documents-ust-ins-move-to-github/memory/wall_specific_kappa_K_followup.md) — wall-specific structural extension
- [`regime_specific_params_followup.md`](../../../.claude/projects/C--Users-kaleb-Documents-ust-ins-move-to-github/memory/regime_specific_params_followup.md) — regime-specific structural extension
- [`project_t003_next_8pfe_retrofit_alphas.md`](../../../.claude/projects/C--Users-kaleb-Documents-ust-ins-move-to-github/memory/project_t003_next_8pfe_retrofit_alphas.md) — 8p+FE with retrofit alphas

## Decisions log

- **2026-05-21**: Reframed the paper around three-paper neighborhood (Boomhower / Mulder / Javier). Shavell is the *why pricing matters* anchor, not "we test leg 2 of Shavell 2004." Diamond + JKSvB give the welfare-reach vocabulary for the L-vs-E thesis but stay in §4 and §13.
- **2026-05-21**: Dropped Hendren-style insurance valuation framework — wrong margin.
- **2026-05-21**: Dropped owner-equity quantitative claim — no firm-level financial data.
- **2026-05-21**: Dropped premium-vs-MSD descriptive comparisons in §3 / §6. The structural model handles the welfare arithmetic.
- **2026-05-21**: Added §3.2 on strict liability under federal and state law (CERCLA + RCRA Subtitle I + parallel state law) — explains the legal architecture that makes the FR rule and insurance pricing meaningful.

## Two research questions (as they appear in §1 ¶4)

- **Q1:** How does switching the financial-responsibility regime from flat-fee public insurance to risk-rated private insurance change firm behavior on tank closure, replacement, and confirmed-release outcomes?
- **Q2:** How much of the first-best social welfare gain does the risk-rated regime capture, and what determines the residual gap to first-best?
