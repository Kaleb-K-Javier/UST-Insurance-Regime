# QUICK REFERENCE: Anticipation-Explicit Event Study

**Location**: `Code/Helpers/S10_Cox_EventStudy_Anticipation.R`  
**Main Script**: `Code/Analysis/02b_DiD_Survival.R` (Section S10b)  
**Documentation**: 
- [Main Guide](./ANTICIPATION_EXPLICIT_EVENT_STUDY.md)
- [Technical Details](./ANTICIPATION_EXPLICIT_EVENT_STUDY_TECHNICAL.md)

---

## TL;DR

**Problem**: Texas tanks began closing in 1998, *before* the 1999 reform date. This "anticipation" confounds standard event studies.

**Solution**: Shift reference year to 1997 and explicitly estimate the 1998 anticipation effect as a separate parameter.

**Result**: Can now test whether trends were parallel *before* anticipation started, and quantify how much anticipation mattered.

---

## Standard Usage

```r
# Source the helper
source(here("Code", "Helpers", "S10_Cox_EventStudy_Anticipation.R"))

# Run primary specification
res <- run_cox_event_study_anticipation(
  exact_split_df[is_post_mandate == 1L],
  label = "Primary-PostMandate-Anticipation"
)

# Inspect results
res$coefs_summary
res$anticipation
res$pre_tests
res$plot
```

---

## What Gets Estimated

| Component | Reference | Year | Variable | Interpretation |
|:---|:---:|:---:|:---|:---|
| **Pre-Anticipation** | 1997 | ≤1997 | `ry_pool_pre`, `ry_pre_m*` | Baseline trends (tested) |
| **Anticipation** | 1997 | 1998 | `ry_anticipation` | Forward-looking response |
| **Post-Reform** | 1997 | 1999+ | `ry_post_*`, `ry_pool_post` | Reform effects |

---

## Key Outputs

### `res$coefs` — Full Coefficient Table

```
          term      coef  robust_se        p  rel_year        hr 
1    ry_pool_pre   -0.12      0.06    0.091       -3     0.89
2    ry_pre_m4     -0.08      0.15    0.608       -4     0.92
3    ry_anticipation 0.40      0.18    0.032       -1     1.49  ← 1998 effect
4    ry_post_0      0.52      0.12    <.001        0     1.68
5    ry_post_1      0.48      0.14    <.001        1     1.62
...
```

### `res$coefs_summary` — Headline Estimates

```
Effect              Estimate    log_HR  HR_pct_change    p-value
Pre-Anticipation    Pooled      -0.12         -11.4%      0.091
Anticipation        Explicit     0.40         +49.2%      0.032
Post-Reform         Pooled       0.52         +68.2%     <0.001
```

### `res$anticipation` — Anticipation Detail

```
$coef   = 0.4024     ← log-hazard ratio
$se     = 0.1803
$hr     = 1.4951     ← hazard ratio
$pct    = +49.18%    ← percent change
$p      = 0.0318     ← p-value
$ci_lo  = 1.0509     ← 95% CI
$ci_hi  = 2.1270
```

### `res$pre_tests` — Parallel Trends Test

```
$chi_all    = 2.847
$dof_all    = 5           ← number of pre-anticipation params tested
$p_all      = 0.725       ← p-value (high = supports parallel trends)

$chi_dense  = 0.394
$dof_dense  = 4
$p_dense    = 0.983       ← even tighter window also passes
```

### `res$plot` — Event Study Figure

Three-panel visualization:
1. **Pre-Anticipation** (gray shaded): Test parallel trends here
2. **Anticipation** (orange point): Explicitly separated 1998 effect
3. **Post-Reform** (blue): Sustained reform effects

---

## Common Parameters

```r
run_cox_event_study_anticipation(
  dt,                          # Data (e.g., exact_split_df subset)
  pool_pre    = -2L,          # Omitted reference year (1997 = -2)
  pool_post   = 15L,          # Max individual post-reform year
  strata_var  = "make_model_tank",  # Cell stratification
  cluster_var = "state",      # Cluster variable for robust SEs
  dense_range = 2:6,          # Years included in "dense" pre-test
  y_metric    = "pct_change", # "pct_change" or "log_hr"
  label       = "MyLabel",    # Label for outputs
  save        = TRUE,         # Save figures + CSV?
  file_stem   = NULL          # File name (auto-generated if NULL)
)
```

---

## Sensitivity Analyses

### By Tank Type (Single-Walled)

```r
res_sw <- run_cox_event_study_anticipation(
  exact_split_df[mm_wall == "Single-Walled" & is_post_mandate == 1L],
  label = "SingleWalled-Anticipation"
)
```

### By Cell Definition (3-dimensional)

```r
exact_split_df[, cell_3d := paste(mm_wall, mm_fuel, mm_install_cohort, sep = "_")]
res_3d <- run_cox_event_study_anticipation(
  exact_split_df,
  strata_var = "cell_3d",
  label = "3DimCell-Anticipation"
)
```

### By Pooling Structure (Tighter)

```r
res_tight <- run_cox_event_study_anticipation(
  exact_split_df[is_post_mandate == 1L],
  pool_pre = -2L, pool_post = 10L,
  label = "TightPool-Anticipation"
)
```

---

## Interpreting the Results

### Anticipation is Significant ($p < 0.05$)

**Finding**: 1998 closure rate diverged meaningfully between TX and control.

**Interpretation Options**:
- Tank owners anticipated the 1999 policy and accelerated closures
- Market expectations incorporated the coming reform
- Demonstration of rational forward-looking behavior

**Implication**: Post-1999 effects are a *clean* estimate of reform impact (net of anticipation).

### Parallel Trends Test Passes ($p > 0.10$)

**Finding**: Pre-anticipation trends (1997 and earlier) are parallel.

**Interpretation**:
- TX and control had identical hazard trends *before* policy signal
- Key identification assumption supported in pre-anticipation window
- Supports causal interpretation of post-reform effects

**Caveat**: Only tests prior to anticipation; cannot rule out other 1999+ shocks.

### High Post-Reform Effects

**Finding**: Post-reform effects grow from anticipation.

**Interpretation**:
- Anticipation captures only *part* of total response
- Additional closures driven by actual (not anticipated) reform features
- Could reflect surprise, changing compliance interpretation, etc.

---

## Outputs Generated

When `save = TRUE` (default):

```
Output/Figures/
├─ Figure_CoxES_Anticipation_Primary_PostMandate_Anticipation.png
├─ Figure_CoxES_Anticipation_Primary_PostMandate_Anticipation.pdf
│
Output/Tables/
├─ Table_CoxES_Anticipation_Primary_PostMandate_Anticipation.csv
└─ Table_CoxES_Anticipation_Summary_Primary_PostMandate_Anticipation.csv
```

---

## Troubleshooting

### Error: "object 'REFORM_DATE' not found"
**Cause**: Trying to source helper before S1 (Setup and Constants) has run.  
**Fix**: Ensure Section S1 has executed first.

### Error: "object 'exact_split_df' not found"
**Cause**: Data from S5 not yet created.  
**Fix**: Ensure Section S5 has executed first.

### Empty plot or missing points
**Cause**: Some relative years have no observations.  
**Fix**: Adjust pooling boundaries (e.g., `pool_pre = -1L, pool_post = 8L`).

### Pre-test p-value = 1.0
**Cause**: All pre-anticipation coefficients are very small/similar.  
**Fix**: May indicate very stable pre-trends (✓ good for identification).

### Can't find the anticipation parameter in output
**Cause**: Might be named differently depending on run.  
**Fix**: Look in `res$coefs` for `term == "ry_anticipation"` or check `res$anticipation` directly.

---

## Comparison: Standard vs. Anticipation-Explicit

| Feature | Standard | Anticipation-Explicit |
|:---|:---|:---|
| **Reference Year** | -1 (1998) | **-2 (1997)** |
| **Anticipation Modeling** | Pooled w/ pre-period | **Explicit separate parameter** |
| **Parallel-Trends Test** | All pre-periods | **Pre-anticipation only** |
| **Anticipation Quantified?** | No | **Yes** |
| **Best Use** | No anticipation suspected | **Anticipation observed in data** |

---

## Citation

If you use this specification, please cite the helper file and documentation:

> "Anticipation-Explicit Event Study Cox Proportional Hazard Model, implemented in `Code/Helpers/S10_Cox_EventStudy_Anticipation.R`. See `Docs/ANTICIPATION_EXPLICIT_EVENT_STUDY.md` for full specification and interpretation guide."

---

## Questions?

- **Specification details**: See [Technical Appendix](./ANTICIPATION_EXPLICIT_EVENT_STUDY_TECHNICAL.md)
- **Economic intuition**: See [Main Guide](./ANTICIPATION_EXPLICIT_EVENT_STUDY.md)
- **Code walkthrough**: See docstrings in `S10_Cox_EventStudy_Anticipation.R`

---

**Last Updated**: March 2026  
**Status**: Production Ready
