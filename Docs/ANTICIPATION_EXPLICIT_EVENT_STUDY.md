# Anticipation-Explicit Event Study: Specification & Interpretation

## Executive Summary

The **anticipation-explicit event study** is a methodological approach designed to formally model and test for anticipatory behavior when agents know a policy change is coming but it hasn't yet taken effect.

**Key Innovation**: Rather than treating anticipation as a "nuisance" pre-trend, we explicitly separate it into its own parameter $\beta_{\text{anticipation}}$ and shift the parallel-trends test to the truly *pre-anticipation* period.

---

## The Problem: Standard Event Study & Texas UST Reform

In the Texas UST insurance reform analysis:
- **Statutory reform date**: December 22, 1998
- **Observed pattern**: Tanks began closing in 1998, *before* the reform was implemented
- **Standard approach**: Would include 1998 in the "pre-period" for parallel-trends testing → **confounds anticipation with baseline trends**
- **Result**: May incorrectly reject parallel trends due to rational anticipatory behavior

---

## The Solution: Shifted Event Study with Explicit Anticipation

### Specification

Estimate a single Cox proportional hazard model:

$$Y_{it} = \alpha_i + \gamma_t + \sum_{\tau \le -2} \beta_\tau D_{i, t+\tau} + \beta_{\text{anticipation}} D_{i, 1998} + \sum_{\tau \ge 0} \beta_\tau D_{i, t+\tau} + \epsilon_{it}$$

Where:
- **Reference year**: $\tau = -2$ (1997) — the year *strictly prior to anticipation*
- **Anticipation period**: $\tau = -1$ (1998) — explicitly estimated, separate coefficient
- **Reform onset**: $\tau = 0, 1, 2, \ldots$ (1999 onward) — post-reform treatment effects
- **Pre-anticipation periods**: $\tau \le -2$ — used for parallel-trends test

### Relative Year Encoding

For Texas reform (statutory date = 1998-12-22):

| Calendar Year | Relative Year ($\tau$) | Role |
|:---:|:---:|:---|
| 1997 | $-2$ | **REFERENCE (omitted)** |
| 1998 | $-1$ | **ANTICIPATION (explicit)** |
| 1999 | $0$ | Reform onset (post-period) |
| 2000 | $1$ | Post-reform |
| $\ldots$ | $\ldots$ | $\ldots$ |
| 1996 | $-3$ | Pre-anticipation |
| 1995 | $-4$ | Pre-anticipation (pooled if $< -6$) |

---

## Key Features & Advantages

### 1. **Explicit Anticipation Parameter**
   - $\beta_{\text{anticipation}}$ quantifies the magnitude of behavioral response *before reform*
   - Can be tested against the null hypothesis $H_0: \beta_{\text{anticipation}} = 0$
   - Reported separately, not buried in pooled pre-period estimates

### 2. **Valid Parallel-Trends Test**
   - Only tests $\tau \le -2$ (pre-anticipation periods)
   - Avoids confounding the test with rational anticipatory behavior
   - Provides evidence that the pre-anticipation baseline is truly parallel

### 3. **Clear Period Decomposition**
   - Three distinct effect components:
     - **Pre-anticipation** ($\tau \le -2$): Baseline trends
     - **Anticipation** ($\tau = -1$): Forward-looking response
     - **Post-reform** ($\tau \ge 0$): Immediate & sustained effects after reform

### 4. **Single-Model Inference**
   - Estimates all effects in one Cox model
   - Cluster-robust standard errors across all parameters
   - Avoids multiple-testing issues from separate model runs

---

## Interpretation of Coefficients

### Hazard Ratios

For each relative-year parameter, we report:

$$\text{Hazard Ratio} = e^{\beta_\tau} = \frac{h_{\text{treated}}(t | \text{year} = \tau)}{h_{\text{control}}(t | \text{year} = \tau)}$$

Interpretation:
- **HR = 1.0**: No difference in closure hazard between TX and control states
- **HR > 1.0**: Treatment group has *higher* closure hazard (accelerated closures)
- **HR < 1.0**: Treatment group has *lower* closure hazard (delayed closures)

### Percent Change

$$\text{Percent Change} = (e^{\beta_\tau} - 1) \times 100\%$$

Example: If $\beta_{\text{anticipation}} = 0.40$, then:
$$e^{0.40} - 1 = 0.4918 \Rightarrow +49.2\%\text{ increase in annual closure hazard}$$

---

## Hypothesis Tests

### Pre-Anticipation Parallel Trends

**Wald Test**: $H_0: \beta_\tau = 0$ for all $\tau \le -2$

$$\chi^2 = \sum_{\tau \le -2} \left(\frac{\beta_\tau}{\text{SE}(\beta_\tau)}\right)^2$$

- **Null rejected** ($p < 0.05$): Evidence of pre-reform differential trends
- **Null not rejected** ($p > 0.05$): Support for parallel-trends assumption in pre-reform period

**Interpretation**: If this test passes at reasonable $p$-value (e.g., $p > 0.10$), we have confidence that:
- Treatment and control states followed parallel trends *before* anticipation began
- Observed post-reform effects are not driven by pre-existing divergence

### Anticipation Effect

**Individual $t$-test**: $H_0: \beta_{\text{anticipation}} = 0$

$$t = \frac{\beta_{\text{anticipation}}}{\text{SE}(\beta_{\text{anticipation}})}$$

- **Null rejected** ($p < 0.05$): Significant anticipatory behavior in 1998
- **Null not rejected** ($p > 0.05$): No detectable anticipation effect

---

## Expected Results: Texas UST Reform

### Likely Pattern

1. **Pre-Anticipation** ($\tau = -3, -4, \ldots$): $\beta_\tau \approx 0$ (parallel trends ✓)
2. **Anticipation** ($\tau = -1$, year 1998): $\beta_{\text{anticipation}} > 0$ (upward spike in closures)
3. **Post-Reform** ($\tau = 0, 1, 2, \ldots$): $\beta_\tau > 0$ (sustained elevated closures)

### Economic Interpretation

- **Anticipation**: Tank owners, anticipating stricter regulation, accelerated closures to avoid future compliance costs
- **Post-Reform**: Reform took effect Jan 1, 1999; effects accumulate over time
- **Combined story**: Policy *signaling* (anticipation) + *enforcement* (reform impact)

---

## Code Implementation

### Running the Analysis

```R
source(here("Code", "Helpers", "S10_Cox_EventStudy_Anticipation.R"))

# Primary specification: post-mandate tanks only
res_es_anticipation <- run_cox_event_study_anticipation(
  exact_split_df[is_post_mandate == 1L],
  label = "Primary-PostMandate-Anticipation"
)
```

### Accessing Results

```R
# Full coefficient table (includes pre-anticipation, anticipation, post-reform)
res_es_anticipation$coefs

# Summary table (headline estimates only)
res_es_anticipation$coefs_summary

# Anticipation effect in detail
res_es_anticipation$anticipation
# $coef, $se, $hr, $pct, $p, $ci_lo, $ci_hi

# Pre-anticipation parallel-trends test
res_es_anticipation$pre_tests
# $chi_all, $dof_all, $p_all

# Event study figure
res_es_anticipation$plot
```

### Output Files

The function saves:
- **`Figure_CoxES_Anticipation_*.png`**: Main event-study plot
- **`Table_CoxES_Anticipation_*.csv`**: Full coefficient table
- **`Table_CoxES_Anticipation_Summary_*.csv`**: Condensed summary

---

## Robustness Checks

The helper includes sensitivity analysis via parameters:

```R
# Sensitivity 1: Single-walled subsample (cleanest tank type)
res_sw <- run_cox_event_study_anticipation(
  exact_split_df[mm_wall == "Single-Walled" & is_post_mandate == 1L],
  label = "SingleWalled-Anticipation"
)

# Sensitivity 2: Tighter pooling
res_tight <- run_cox_event_study_anticipation(
  exact_split_df[is_post_mandate == 1L],
  pool_pre = -2L, pool_post = 10L,
  label = "TightPool-Anticipation"
)

# Sensitivity 3: Different stratification
exact_split_df[, make_model_3dim := paste(mm_wall, mm_fuel, mm_install_cohort, sep = "_")]
res_3d <- run_cox_event_study_anticipation(
  exact_split_df,
  strata_var = "make_model_3dim",
  label = "3dim-Anticipation"
)
```

---

## Comparison: Standard vs. Anticipation-Explicit

| Feature | Standard Event Study | Anticipation-Explicit |
|:---|:---:|:---:|
| Reference year | $\tau = -1$ (e.g., 1998) | $\tau = -2$ (1997) |
| Anticipation treatment | Pooled w/ pre-period | Separate explicit parameter |
| Parallel-trends test | All pre-periods | Pre-anticipation only ($\tau \le -2$) |
| Anticipation quantified? | No | **Yes, explicitly** |
| Model specification | Individual + pooled | Individual + pooled + explicit anticipation |

---

## References & Literature

### Seminal Papers on Anticipation Effects
- **Imbens & Wooldridge (2009)**: Discrete-choice modeling of discrete treatment timing
- **Callaway & Sant'Anna (2021)**: Difference-in-differences with heterogeneous treatment timing
- **Autor (2003)**: The "surprise" analysis of Earned Income Tax Credit
- **Shapiro (2003)**: Anticipation of tax changes (macro policy)

### Related Methods
- **Event-study design**: Standard; assumes no anticipation
- **Honest inference**: Allows for "mild" anticipation; provides conservative bounds
- **Time-varying treatment**: Alternative approach (may violate no-unmeasured-confounding)

---

## Questions & Troubleshooting

### Q: What if the parallel-trends test fails ($p < 0.05$)?
**A**: Pre-anticipation trends diverged between TX and control. Either:
- The pre-period is contaminated by other shocks (check calendar ≤ 1997)
- The matching/sample definition needs refinement
- Consider alternative identification strategies (robustness)

### Q: What if anticipation is not significant ($p > 0.10$)?
**A**: May indicate:
- Agents did not have advance notice of the reform
- Rational expectations theory does not apply to this policy context
- Costs of anticipatory action outweighed benefits
- **Implication**: Post-reform effect is a cleaner estimate of reform impact

### Q: How to choose pool_pre and pool_post?
**A**: Trade-off between precision and flexibility:
- **Tight pooling** ($pool_pre = -2, pool_post = 5$): Few parameters, tight CIs, bias if true effects vary
- **Loose pooling** ($pool_pre = -6, pool_post = 15$): Many parameters, wider CIs, flexibility
- **Default**: $pool_pre = -2$ (respect pre-anticipation window), $pool_post = 15$ (long post-period)

### Q: How to interpret the plot?
**A**: 
- **Gray shaded region** (left): Pre-anticipation period (parallel trends tested here)
- **Orange point** (center): Anticipation effect (emphasized, separate)
- **Right region**: Post-reform effects
- **Pre-test annotation**: Wald χ² and $p$-value for parallel trends

---

## Author Notes

This specification was developed to properly account for the documented evidence of anticipatory tank closures in 1998 in Texas, *prior to* the statutory Dec 22, 1998 reform implementation date.

**Key insight**: Rational agents responding in advance is not a threat to identification if we explicitly model their behavior. By shifting the reference baseline to the pre-anticipation year and isolating the anticipation parameter, we can:

1. Quantify how large anticipation is
2. Test whether pre-anticipation trends are truly parallel
3. Cleanly identify post-reform effects
4. Improve credibility of causal claims

---

**Updated**: March 2026  
**Helper File**: `Code/Helpers/S10_Cox_EventStudy_Anticipation.R`  
**Main Script**: `Code/Analysis/02b_DiD_Survival.R` (Section S10b)
