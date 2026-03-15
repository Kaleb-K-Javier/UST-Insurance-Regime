# Technical Specification: Anticipation-Explicit Cox Event Study

## Introduction

This document provides the complete mathematical formulation of the anticipation-explicit event study Cox proportional hazard model implemented in `S10_Cox_EventStudy_Anticipation.R`.

---

## Data Structure: Counting Process Format

After exact-date reform splitting (Section S5), each tank $i$ is represented by one or more episodes $(i, j)$ where:

$$\text{Episode}_{ij} = (t^{\text{enter}}_{ij}, t^{\text{exit}}_{ij}, d_{ij})$$

- $t^{\text{enter}}_{ij}$: Entry time into the risk set (exact dates, in days)
- $t^{\text{exit}}_{ij}$: Exit time from the risk set (exact dates, in days)
- $d_{ij}$: Indicator for closure event ($d_{ij} = 1$ if tank closed)

Each tank's life is split at reform-date anniversary cutpoints to map episodes to relative years.

---

## Relative Year Assignment

Let the statutory reform date be $\mathcal{D}^* = $ December 22, 1998. The anniversary dates for relative year $\tau$ are:

$$\mathcal{D}_\tau = \text{Date}(\text{Dec 22}, \text{year}_\tau) \quad \text{where} \quad \text{year}_\tau = 1998 + \tau$$

For each episode $(t^{\text{enter}}, t^{\text{exit}})$:

$$\tau^* = \begin{cases}
\text{floor}\left(\frac{t^{\text{exit}} - \mathcal{D}^*}{365.25}\right) & \text{if episode *ends* after } \mathcal{D}^* \text{ (contributes to } \tau^* = 0, 1, 2, \ldots \text{)} \\
\text{floor}\left(\frac{t^{\text{enter}} - \mathcal{D}^*}{365.25}\right) & \text{otherwise}
\end{cases}$$

This ensures each row of risk time is assigned to exactly one relative year, reflecting when the tank was "exposed" during that year.

---

## Anticipation-Explicit Relative Year Dummies

Define a set of binary treatment-interaction variables:

### Pre-Anticipation Period ($\tau \le -2$)

**Pooled pre-tail** (years $< \tau_{\text{pool}}^{\text{pre}}$, e.g., $< -6$):

$$D_{i,\tau_{\text{pool}}^{\text{pre}}} = \begin{cases}
\mathbf{1}(\tau \le -6) \times \mathbf{1}(\text{TX}_i) & \\
\end{cases}$$

**Individual pre-anticipation years** ($-6 < \tau < -2$):

$$D_{i,\tau} = \mathbf{1}(\text{rel\_year} = \tau) \times \mathbf{1}(\text{TX}_i) \quad \text{for} \quad \tau \in \{-5, -4, -3\}$$

All pre-anticipation dummies are **included** in the model.

### Reference Year ($\tau = -2$, Year 1997)

$$D_{i,-2} = \mathbf{1}(\text{rel\_year} = -2) \times \mathbf{1}(\text{TX}_i)$$

**OMITTED from the model** (serves as reference, with coefficient $\beta_{-2} := 0$).

### Anticipation Period ($\tau = -1$, Year 1998)

$$D_{i,\text{antic}} = \mathbf{1}(\text{rel\_year} = -1) \times \mathbf{1}(\text{TX}_i)$$

**EXPLICITLY INCLUDED** as a separate parameter $\beta_{\text{antic}}$ (not pooled with any other period).

### Post-Reform Period ($\tau \ge 0$)

**Individual post-reform years** ($0 \le \tau \le \tau_{\text{pool}}^{\text{post}}$, e.g., $0 \le \tau \le 15$):

$$D_{i,\tau} = \mathbf{1}(\text{rel\_year} = \tau) \times \mathbf{1}(\text{TX}_i) \quad \text{for} \quad \tau \in \{0, 1, \ldots, 15\}$$

**Pooled post-tail** (years $\ge \tau_{\text{pool}}^{\text{post}}$, e.g., $\ge 15$):

$$D_{i,\tau_{\text{pool}}^{\text{post}}} = \mathbf{1}(\tau \ge 15) \times \mathbf{1}(\text{TX}_i)$$

All post-reform dummies are **included** in the model.

---

## The Specification

### Stratified Cox Proportional Hazard Model

For tank $i$ in cell $c$ (where cell = make/model/tank type) during episode $j$:

$$h_c(t | X) = h_{0,c}(t) \exp\left(\sum_{\tau \le -2, \tau \ne -2} \beta_\tau D_{i,\tau} + \beta_{\text{antic}} D_{i,\text{antic}} + \sum_{\tau \ge 0} \beta_\tau D_{i,\tau}\right)$$

Where:
- $h_{0,c}(t)$: Baseline hazard function for cell $c$ (unspecified in stratified Cox)
- $\beta_\tau$: Treatment effect coefficient for relative year $\tau$
- $\beta_{\text{antic}}$: Treatment effect coefficient for anticipation period (year 1998)

### Compact Form

$$h_c(t | \mathbf{D}_i) = h_{0,c}(t) \cdot \exp\left(\sum_{k \in \mathcal{K}} \beta_k D_{i,k}\right)$$

Where $\mathcal{K}$ is the set of included relative years:

$$\mathcal{K} = \{\tau_{\text{pre:pool}}, -5, -4, -3, \text{ahead}, 0, 1, \ldots, 15, \tau_{\text{post:pool}}\}$$

and $D_{i,\text{ahead}} := D_{i,\text{antic}}$ for clarity.

---

## Identification

### Core Identifying Assumptions

1. **No unmeasured confounding (conditional on strata)**:
   $$d_{ij} \perp\!\!\!\perp \text{TX}_i \mid \text{cell}_c, \text{rel\_year}, \text{risk time}$$
   
   *Interpretation*: Treatment assignment is independent of closure propensity within cell and year, conditional on time-at-risk.

2. **Stratified proportional hazards**:
   $$\frac{h_1(t)}{h_0(t)} = e^{\beta_\tau} \quad \text{(constant ratio across time within relative year)}$$

3. **Parallel trends (pre-anticipation)**:
   $$\beta_\tau = 0 \quad \text{for all} \quad \tau \le -2 \quad \Leftrightarrow \quad \sum_{\tau \le -2} \beta_\tau = 0$$
   
   *Interpretation*: TX and control states had identical closure hazard trends *before anticipation* (years ≤ 1997).

4. **Anticipation is isolated**:
   The 1998 effect is purely anticipatory, not driven by other 1998-specific shocks common to TX and control.

### Justification for Shifted Reference

By setting the reference to $\tau = -2$ (1997, *before* anticipation begins), we ensure:

- The parallel-trends test ($H_0: \beta_\tau = 0$ for all $\tau \le -2$) tests *true* pre-trends
- Anticipatory behavior ($\beta_{\text{antic}}$) is estimated *relative to the correct baseline*
- Post-reform effects ($\beta_\tau$ for $\tau \ge 0$) are not contaminated by unmodeled anticipation

---

## Inference

### Point Estimates

From the fitted Cox model, we extract maximum likelihood estimates:

$$\hat{\beta}_k \quad \text{and} \quad \widehat{\text{SE}}(\hat{\beta}_k)^2 = \text{Var}(\hat{\beta}_k)$$

Cluster-robust variance estimation (by state) adjusts for within-state correlation across tanks.

### Hazard Ratios

$$\widehat{\text{HR}}_\tau = e^{\hat{\beta}_\tau}$$

Interpretation: Relative closure hazard of TX vs. control in year $\tau$, *conditional on tank cell and survival to that year*.

### Percent Change

$$\widehat{\Delta}_\tau (\%) = (e^{\hat{\beta}_\tau} - 1) \times 100$$

Interpretation: Percentage change in annual closure rate during year $\tau$.

### Confidence Intervals (95%)

$$\text{CI}(\beta_\tau) = \left[\hat{\beta}_\tau - 1.96 \cdot \widehat{\text{SE}}(\hat{\beta}_\tau), \quad \hat{\beta}_\tau + 1.96 \cdot \widehat{\text{SE}}(\hat{\beta}_\tau)\right]$$

$$\text{CI}(\text{HR}_\tau) = \left[e^{\text{CI}(\beta_\tau)_L}, \quad e^{\text{CI}(\beta_\tau)_U}\right]$$

### Hypothesis Tests

#### Test 1: Pre-Anticipation Parallel Trends

$$H_0: \beta_\tau = 0 \quad \forall \tau \le -2 \quad \Longleftrightarrow \quad \mathbf{R} \boldsymbol{\beta} = \mathbf{0}$$

Where $\mathbf{R}$ is a contrast matrix selecting the pre-anticipation coefficients.

**Wald test statistic**:

$$W = (\mathbf{R}\hat{\boldsymbol{\beta}})^T (\mathbf{R} \Sigma_{\mathbf{\beta}} \mathbf{R}^T)^{-1} (\mathbf{R}\hat{\boldsymbol{\beta}}) \sim \chi^2_{|\mathcal{K}_{\text{pre}}|}$$

where $|\mathcal{K}_{\text{pre}}|$ is the number of pre-anticipation parameters tested.

#### Test 2: Anticipation Effect

$$H_0: \beta_{\text{antic}} = 0$$

**Wald test statistic**:

$$W_{\text{antic}} = \frac{\hat{\beta}_{\text{antic}}^2}{\widehat{\text{SE}}(\hat{\beta}_{\text{antic}})^2} \sim \chi^2_1$$

Equivalently, $z = \frac{\hat{\beta}_{\text{antic}}}{\widehat{\text{SE}}(\hat{\beta}_{\text{antic}})} \sim N(0,1)$ and two-sided $p$-value $= 2\Phi(-|z|)$.

#### Test 3: Dense Pre-Anticipation Trends

Subset of Test 1, focusing on relative years in the "dense" range (e.g., $\tau \in \{-6, -5, -4, -3\}$ if estimated individually):

$$H_0: \beta_\tau = 0 \quad \forall \tau \in \{-6, -5, -4, -3\}$$

Provides a sharper test of trends in the most recent pre-anticipation period.

---

## Data Organization in Code

After splitting via `cox_es_split()` and dummy creation via `cox_es_anticipation_build_dummies()`, the event-study dataset has the following additional columns:

### Relative Year (categorical)

| Column | Values | Meaning |
|:---|:---|:---|
| `rel_year` | $\ldots, -6, -5, \ldots, -1, 0, 1, \ldots$ | Relative year of episode |

### Treatment Interaction Dummies (binary)

| Column | Formula | Included? |
|:---|:---|:---:|
| `ry_pool_pre` | $\mathbf{1}(\tau < -2) \times \text{TX}$ | Yes (if $\tau < -2$ observed) |
| `ry_pre_m3` | $\mathbf{1}(\tau = -3) \times \text{TX}$ | Yes |
| `ry_pre_m4` | $\mathbf{1}(\tau = -4) \times \text{TX}$ | Yes |
| $\vdots$ | $\vdots$ | $\vdots$ |
| (nothing) | $\mathbf{1}(\tau = -2) \times \text{TX}$ | **No** (reference) |
| `ry_anticipation` | $\mathbf{1}(\tau = -1) \times \text{TX}$ | **Yes** (explicit) |
| `ry_post_0` | $\mathbf{1}(\tau = 0) \times \text{TX}$ | Yes |
| $\vdots$ | $\vdots$ | $\vdots$ |
| `ry_pool_post` | $\mathbf{1}(\tau \ge 15) \times \text{TX}$ | Yes (if $\tau \ge 15$ observed) |

---

## Cox Model Formula

In R/survival syntax:

```r
Surv(t_enter, t_exit, failure) ~ 
  ry_pool_pre + ry_pre_m3 + ry_pre_m4 + ry_pre_m5 + ry_pre_m6 +
  ry_anticipation +
  ry_post_0 + ry_post_1 + ... + ry_post_15 + ry_pool_post +
  strata(make_model_tank)
```

Fit via `coxph()` with cluster-robust variance estimates by state.

---

## Large-Sample Approximation

Under standard regularity conditions for Cox models (Andersen & Gill, 1982):

$$\sqrt{n} (\hat{\boldsymbol{\beta}} - \boldsymbol{\beta}^*) \xrightarrow{d} N(\mathbf{0}, \Sigma)$$

where $\Sigma$ is the asymptotic covariance, estimated by the Hessian and adjusted for clustering.

For our setting:
- $n = $ number of episodes (large after survSplit)
- Cluster-robust adjustment: "sandwich estimator" using state-level clustering

This justifies use of standard normal critical values for confidence intervals ($\pm 1.96 \times \text{SE}$).

---

## Comparison to Standard Event Study

### Standard (Reference = -1)

```r
Surv(t_enter, t_exit, failure) ~ 
  ry_pool_pre + ry_m2 + ry_m3 + ... +
  ry_0 + ry_1 + ... + ry_pool_post +
  strata(make_model_tank)
```

- Reference: $\tau = -1$ (1998)
- Parallel trends test: includes $\tau = -1$
- **Problem**: Tests confound anticipation with baseline trends

### Anticipation-Explicit (Reference = -2)

```r
Surv(t_enter, t_exit, failure) ~ 
  ry_pool_pre + ry_pre_m3 + ry_pre_m4 + ... +
  ry_anticipation +
  ry_post_0 + ry_post_1 + ... + ry_pool_post +
  strata(make_model_tank)
```

- Reference: $\tau = -2$ (1997)
- Anticipation: explicit separate parameter
- Parallel trends test: **only** $\tau \le -2$
- **Solution**: Isolates and tests anticipation correctly

---

## R Implementation Details

### Stratification & Clustering

```r
cox_es_estimate <- function(dt, ry_vars, 
                             strata_var = "make_model_tank",
                             cluster_var = "state", ...) {
  
  fml <- as.formula(paste(
    "Surv(t_enter, t_exit, failure) ~",
    paste(ry_vars, collapse = " + "),
    "+ strata(", strata_var, ")"
  ))
  
  coxph(fml, data = dt, 
        cluster = dt[[cluster_var]],  # Cluster-robust SEs
        ties = "efron")
}
```

**Key points**:
- `strata()`: Allows cell-specific baseline hazards (removes them from inference)
- `cluster`: Specifies clustering variable (state) for robust SE adjustment
- `ties = "efron"`: Handles tied event times (exact dates already unique in most cases)

### Coefficient Extraction

```r
s <- summary(model)$coefficients
ct <- as.data.table(s, keep.rownames = "term")

# Robustly locate the coefficient columns across coxph versions
old_names <- names(ct)
new_names <- old_names
new_names[grepl("^coef$|^coef\\b", old_names)] <- "coef"
new_names[grepl("robust se|se\\(coef\\)", old_names)] <- "robust_se"
new_names[grepl("Pr\\(>\\|z\\|\\)", old_names)] <- "p"
setnames(ct, new_names)
```

### Wald Tests

```r
# Pre-anticipation parallel trends
pre_vars <- c("ry_pool_pre", "ry_pre_m3", "ry_pre_m4", ...)
h0 <- paste0(pre_vars, " = 0")
test <- car::linearHypothesis(model, h0, test = "Chisq")
# Returns: Chisq, dof, p-value
```

---

## Extensions & Robustness

### 1. Alternative Stratification

Instead of make-model-tank cells:

```r
exact_split_df[, cell_3dim := paste(mm_wall, mm_fuel, mm_install_cohort, sep = "_")]
res <- run_cox_event_study_anticipation(
  exact_split_df,
  strata_var = "cell_3dim"
)
```

Tests whether results are robust to cell definition.

### 2. Subgroup Heterogeneity

```r
res_sw <- run_cox_event_study_anticipation(
  exact_split_df[mm_wall == "Single-Walled" & is_post_mandate == 1L]
)
```

Tests whether anticipation is uniform across tank types or heterogeneous.

### 3. Alternative Pooling

```r
res_tight <- run_cox_event_study_anticipation(
  exact_split_df,
  pool_pre = -2L, pool_post = 10L  # Tighter tail pooling
)
```

Tests whether results depend on tail-pooling choices (bias-variance tradeoff).

---

## References

### Methodological

1. **Andersen, P. K., & Gill, R. D. (1982).** "Cox's regression model for counting processes: a large sample study." *Annals of Statistics*, 10(4), 1100–1120.
   - Foundational asymptotics for Cox model with counting-process data.

2. **Therneau, T. M., & Grambsch, P. M. (2000).** *Modeling Survival Data: Extending the Cox Model*. Springer.
   - Standard reference for Cox modeling in R.

3. **Lin, D. Y., & Wei, L. J. (1989).** "The robust inference for the Cox proportional hazards model." *JASA*, 84(408), 1074–1078.
   - Cluster-robust variance for Cox models.

### Event Studies & Anticipation

4. **Imbens, G. W., & Wooldridge, J. M. (2009).** "Recent developments in the econometrics of program evaluation." *Journal of Economic Literature*, 47(1), 5–86.
   - Overview of causal inference; discusses anticipation.

5. **Callaway, B., & Sant'Anna, P. H. (2021).** "Difference-in-differences with multiple time periods." *Journal of Econometrics*, 225(2), 200–230.
   - Modern treatment of staggered adoption with anticipation.

6. **Shapiro, M. D. (2003).** "The user costs of household appliances: a new measure and applications." Unpublished Working Paper.
   - Classic example of modeling anticipation of tax changes.

---

## Notation Summary

| Symbol | Meaning |
|:---:|:---|
| $i$ | Tank index |
| $c$ | Cell index (make-model-tank) |
| $j$ | Episode (interval) index within tank |
| $t$ | Time (calendar days) |
| $\tau$ | Relative year (years relative to reform) |
| $\mathcal{D}^*$ | Statutory reform date (Dec 22, 1998) |
| $h_c(t)$ | Hazard function for cell $c$ |
| $h_{0,c}(t)$ | Baseline hazard for cell $c$ (unspecified) |
| $d_{ij}$ | Event indicator (1 = closure, 0 = censored) |
| $\beta_\tau$ | Coefficient for relative year $\tau$ |
| $\beta_{\text{antic}}$ | Coefficient for anticipation (τ = -1) |
| $D_{i,\tau}$ | Treatment × year interaction dummy |
| $\text{TX}_i$ | Treatment indicator (Texas = 1, Control = 0) |
| $\hat{\beta}_\tau$ | MLE of $\beta_\tau$ |
| $\text{HR}_\tau$ | Hazard ratio $= e^{\beta_\tau}$ |
| $\Sigma$ | Asymptotic covariance matrix |

---

**Version**: 1.0  
**Date**: March 2026  
**Author**: Analysis Team  
**File**: `Docs/ANTICIPATION_EXPLICIT_EVENT_STUDY_TECHNICAL.md`
