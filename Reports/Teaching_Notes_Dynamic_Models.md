# Teaching Notes — Dynamic Discrete-Choice Models

**Author:** Kaleb Javier
**First written:** 2026-05-18
**Purpose:** Self-reference notes for techniques, derivations, and literature
encountered while building the UST replacement model. Written so a future-me
(or a co-author) can pick it up cold and reconstruct what was done and why.

This is a *teaching* note, not a paper draft. Emphasis on the math being
explicit and the citations being reachable. Sections are independent —
read whichever one is relevant.

---

## Table of Contents

1. [Profile likelihood for additive group fixed effects in dynamic DCMs](#1-profile-likelihood-for-additive-group-fixed-effects-in-dynamic-dcms)
2. [Notation cheat sheet for the UST model](#appendix-a--notation-cheat-sheet-for-the-ust-model)

(more sections as topics come up — keep adding.)

---

## 1. Profile likelihood for additive group fixed effects in dynamic DCMs

### 1.1 The problem this solves

You have a dynamic discrete-choice model with structural parameters
$\theta \in \mathbb{R}^{p_\theta}$ (small, e.g. 8 in our case) and group-level
additive intercepts $\alpha_g$ for $g = 1, \ldots, G$ (potentially many, e.g.
17 control states in our case, with TX as baseline). The intercepts shift the
flow utility for one or more actions but are otherwise nuisance — you do not
care about their individual values in the welfare counterfactual.

The naive approach is to estimate $(\theta, \alpha_1, \ldots, \alpha_G)$
jointly by maximum likelihood, which is what our current 8p+FE specification
does. This works but has three issues:

1. **Optimizer dimensionality.** L-BFGS-B searches a $(p_\theta + G)$-dim
   surface. Adding more FE (e.g. retrofit alphas for an additional action,
   or interacting state × wall FEs) blows this up combinatorially.

2. **Boundary problems.** When a group has degenerate observed shares (e.g.
   Missouri has near-zero Exit and Replace activity), the joint optimizer
   pushes $\hat\alpha_{\text{MO}}$ toward its bound, producing inflated
   point estimates and standard errors that misreport the curvature.

3. **Ugly reporting.** The paper has to show 17 alphas that nobody reads
   and that are not the object of inference.

The profile-likelihood trick eliminates the $\alpha_g$ from the optimizer's
search space without changing the maximum likelihood estimator $\hat\theta$.
The $\alpha_g$ become a deterministic function of $\theta$, solved
analytically (or by a fast 1-D Newton solver) at each $\theta$ candidate
the optimizer evaluates.

### 1.2 Setup

Let actions be indexed $j \in \{M, E, R\}$ (Maintain, Exit, Replace). Each
observation belongs to a state cell $s \in \{1, \ldots, S\}$ and a group
$g \in \{1, \ldots, G\}$. For some specified subset of actions $\mathcal{J}_\alpha
\subseteq \{M, E, R\}$, the additive group intercept $\alpha_g$ enters the
flow utility:

$$
u_j(s, g; \theta, \alpha) =
\begin{cases}
  v_j(s; \theta) + \alpha_g & \text{if } j \in \mathcal{J}_\alpha \\
  v_j(s; \theta)            & \text{otherwise}
\end{cases}
$$

where $v_j(s; \theta)$ is the structural-parameter-only utility (post NPL
inner solve so $v_j$ already includes $\beta F_j V$ where applicable).

The T1EV-shock choice probability is

$$
P(j \mid s, g; \theta, \alpha_g) \;=\;
  \frac{\exp(u_j(s, g; \theta, \alpha_g) / \sigma)}{\sum_k \exp(u_k(s, g; \theta, \alpha_g) / \sigma)}.
$$

The aggregated log-likelihood over $(s, g)$ count cells is

$$
\ell(\theta, \alpha_1, \ldots, \alpha_G) \;=\;
  \sum_{s=1}^{S} \sum_{g=1}^{G} \sum_{j \in \{M,E,R\}}
    n_j(s, g) \cdot \log P(j \mid s, g; \theta, \alpha_g),
$$

where $n_j(s, g)$ is the observed count of action $j$ in cell $(s, g)$.

### 1.3 The FOC that lets you concentrate $\alpha_g$ out

Take $\partial \ell / \partial \alpha_g$. The derivative of $\log P(j \mid \cdot)$
w.r.t. $\alpha_g$ is $\sigma^{-1} \cdot (\mathbf{1}\{j \in \mathcal{J}_\alpha\} -
\sum_{k \in \mathcal{J}_\alpha} P(k \mid s, g))$ — i.e., for actions that have
$\alpha$ in their utility, the derivative is $1$ minus the within-cell sum of
$P$ over $\alpha$-using actions; for actions that don't, it's the negative of
that same sum.

Cleaning up, the FOC for $\alpha_g$ is

$$
\boxed{ \;
\sum_{s} \sum_{j \in \mathcal{J}_\alpha}
  \Big[\, n_j(s, g) \;-\; n_{\text{total}}(s, g) \cdot P(j \mid s, g; \theta, \alpha_g) \,\Big] \;=\; 0
\; }
$$

In words: **at the optimal $\alpha_g$, the model-predicted total count of
$\alpha$-using actions in group $g$ equals the observed total count of
those actions in group $g$.** The group-$g$ marginal of the choice variable
(over the actions where $\alpha$ enters) is matched exactly. This is the
sense in which $\alpha$ "residualizes out" the group-level variation — it
absorbs the group-$g$ marginal by construction.

**Special cases:**

- If $\mathcal{J}_\alpha = \{M\}$ (our current 8p+FE): the FOC enforces that
  the model-predicted Maintain count in group $g$ equals the observed Maintain
  count in group $g$.
- If $\mathcal{J}_\alpha = \{M, R\}$ (the proposed stay-shifter spec): the FOC
  enforces that the model-predicted total {Maintain + Replace} count in group
  $g$ equals the observed total {Maintain + Replace} count, which is the
  same as matching the observed Exit rate (since shares sum to 1).
- If $\mathcal{J}_\alpha = \{M, E, R\}$ (all actions): the FOC degenerates
  (it becomes $\sum_s [n_{\text{total}}(s,g) - n_{\text{total}}(s,g)] = 0$,
  trivially satisfied). $\alpha_g$ is not identified in this case — adding
  the same constant to every action's utility doesn't change probabilities.
  This is the level-normalization restriction familiar from logit.

### 1.4 Solving the FOC

For each $g$, the FOC above is a **one-dimensional nonlinear equation in
$\alpha_g$**, with $\theta$ and the current CCP matrix $P^k$ held fixed. The
LHS is strictly monotone decreasing in $\alpha_g$ (basic property of logit:
raising $\alpha_g$ raises $P(j \mid \cdot)$ for $j \in \mathcal{J}_\alpha$,
which raises the model-implied count, which lowers the difference).

So a 1-D Newton or bisection solver converges in 5–10 iterations to machine
precision. In C++ this is sub-millisecond per group; doing all $G = 17$
groups is microseconds total per likelihood evaluation.

There is no need to impose bounds on $\alpha_g$ — the FOC has a unique
solution on $(-\infty, \infty)$ whenever both $n_{j \in \mathcal{J}_\alpha}(\cdot, g) > 0$
and $n_{j \notin \mathcal{J}_\alpha}(\cdot, g) > 0$ for some $s$. If a group
has zero observations in any of the $\alpha$-using actions, $\alpha_g \to -\infty$
(or $+\infty$ for the reverse). This is the same data-degeneracy that
causes joint-estimation boundary problems, but profile-out at least lets
you detect it cleanly rather than reporting a fake-tight SE.

### 1.5 The profile likelihood

Define $\alpha_g(\theta) := $ the solution to the FOC above, for given $\theta$
and current CCP matrix. Then the profile log-likelihood is

$$
\ell^{\text{profile}}(\theta) \;:=\; \ell\big(\theta,\, \alpha_1(\theta), \ldots, \alpha_G(\theta)\big).
$$

The maximizer satisfies $\hat\theta^{\text{profile}} = \arg\max_\theta \ell^{\text{profile}}(\theta)$,
and by a standard result on profile likelihood,

$$
\hat\theta^{\text{profile}} \;=\; \hat\theta^{\text{joint}}
$$

— the profile-MLE numerically equals the joint MLE projected onto the
$\theta$ coordinates. The $\hat\alpha_g$ values from joint estimation are
recovered as $\alpha_g(\hat\theta^{\text{profile}})$. *Source:* this is a
theorem about M-estimators, see Murphy & Van der Vaart (2000, JASA, "On
Profile Likelihood") for the canonical treatment of asymptotic properties.

### 1.6 Standard errors

The asymptotic variance of $\hat\theta^{\text{profile}}$ is computed from
the profile observed-information matrix:

$$
\widehat{\text{Var}}(\hat\theta^{\text{profile}}) \;=\;
  \Big[ -\nabla^2 \ell^{\text{profile}}(\hat\theta) \Big]^{-1}.
$$

This is *not* the same thing as the $(\theta, \theta)$ block of the joint
Hessian. It accounts for the fact that $\alpha_g$ moves with $\theta$.
Numerically, the profile Hessian equals

$$
-\nabla^2 \ell^{\text{profile}}(\hat\theta) \;=\;
  H_{\theta\theta} - H_{\theta\alpha} H_{\alpha\alpha}^{-1} H_{\alpha\theta}
$$

evaluated at $(\hat\theta, \hat\alpha(\hat\theta))$ — i.e., the Schur
complement of the joint Hessian after partialling out the $\alpha$ block.
This is the same as the $(\theta, \theta)$ block of the *inverse* joint
information matrix, so it produces standard errors numerically identical
to what you would get from joint estimation. (This is the equivalence
proved e.g. in Cox & Reid 1987 *JRSSB*, "Parameter Orthogonality and
Approximate Conditional Inference.")

**Practical note:** in our existing 25-param 8p+FE code, the profile SE
calculation just means: form the full $25 \times 25$ Hessian at $(\hat\theta,
\hat\alpha)$, invert it, take the $(\theta, \theta)$ sub-block, take its
diagonal, take square roots. The numbers will match what a profile
implementation would emit. (This is also what the AM 2002 profile-likelihood
SE block in `improved_estimator_OPTIMIZED.r` does already, I believe — worth
checking.)

### 1.7 NPL outer loop with profile-likelihood inner

The full algorithm:

```
INPUT: data {n_j(s, g)}, primitives, theta_init
INITIALIZE: P^0 (Hotz-Miller frequency estimator)

FOR k = 0, 1, 2, ... until convergence:
    INNER OPTIMIZATION over theta only:
        theta^{k+1} = argmax_theta  ell_profile(theta | P^k)
                    where each LL evaluation does:
                        compute v_j(s; theta) from current P^k        # NPL inner V solve
                        FOR g = 1, ..., G:
                            solve 1-D FOC for alpha_g(theta)          # the profile bit
                        evaluate sum_{s,g,j} n_j(s,g) * log P(j | s, g; theta, alpha_g(theta))
                        return that sum

    CCP UPDATE:
        compute v_j(s; theta^{k+1}) from P^k
        solve alpha_g(theta^{k+1}) for each g
        P^{k+1}(s, g) = softmax over j of (v_j(s) + alpha_g * 1{j in J_alpha})

    OPTIONAL: marginalize P^{k+1}(s, g) over g via w_g(s) to get a (S × J)
        matrix for any downstream use that needs cell-level CCPs.

    CHECK CONVERGENCE:
        if ||theta^{k+1} - theta^k|| < tol_theta
        AND ||P^{k+1} - P^k|| < tol_P:
            BREAK
```

### 1.8 Why this is *not* the same as two-step estimation

A natural-sounding but **wrong** version of this idea: "first stage, compute
empirical group-level Maintain rates; second stage, estimate $\theta$ via NPL
on residualized data." That introduces first-stage estimation noise that
propagates through to $\theta$ with the wrong variance formula, and worse,
breaks the NPL fixed-point property because the "empirical" group rates are
not consistent with the structural model at any $\theta$.

The profile approach is fundamentally different: $\alpha_g(\theta)$ is solved
*against the model*, not against the data alone. The data enters only via
the counts $n_j(s, g)$ that show up in the FOC. The $\alpha_g$ at the optimum
is a model-implied object, not a sample average.

### 1.9 Connection to Chamberlain's conditional logit

For static (non-dynamic) panel logit with binary outcomes and additive group
fixed effects, **Chamberlain (1980)** showed that conditioning on the sum of
outcomes per group ($\sum_t y_{it} = T_i$) eliminates $\alpha_i$ exactly — the
conditional likelihood depends only on $\beta$ (the slopes), not on $\alpha_i$.

The profile-likelihood approach here is a *generalization* of that idea to
the dynamic-DCM, multinomial-action setting:

| Setting | Eliminate FE via | Cost |
|---|---|---|
| Static binary logit panel (Chamberlain 1980) | Condition on sufficient statistic $\sum_t y_{it}$ | Lose info on $\beta$'s identification from level shifts |
| Static multinomial logit panel | Generalizations exist but messy | Reduced efficiency |
| Dynamic logit, multinomial, group FE (us) | Profile-out via 1-D FOC per group | 17 fast Newton solves per LL eval |

In all three cases, the unifying theme is: the FE has a sufficient
statistic that lets you separate it from $\theta$. Conditional likelihood is
the "exact" version (cleanly removes FE from the LL formula). Profile
likelihood is the "approximate" version (formally numerical but
asymptotically equivalent to joint MLE; computationally easier than
conditional likelihood in the dynamic setting because the conditioning
event is hard to characterize when actions affect transitions).

### 1.10 Incidental-parameters problem — when do you need this?

The classical concern with jointly estimating many FEs alongside structural
parameters is the **incidental-parameters problem** (Neyman & Scott 1948,
*Econometrica*): when $N \to \infty$ but $T$ is fixed (short panel), the FE
estimates are noisy at any sample size, and that noise contaminates $\hat\theta$
even asymptotically, producing inconsistency or bias of order $O(T^{-1})$.

**Hahn & Newey (2004, *Econometrica*, "Jackknife and Analytical Bias
Reduction for Nonlinear Panel Models")** characterize this and propose
bias-correction methods. Their result is that for nonlinear FE panel models,
the bias is non-negligible when $T$ is small relative to $N$.

In our setting, this matters less than usual: we have $G = 17$ groups but
each group has many state cells $\times$ many years of data, so the
"per-group" sample size is large. The incidental-parameters bias should be
small. **Profile likelihood here is justified primarily on computational and
reporting grounds, not on bias grounds.**

But it's worth noting: if you ever extend the FE scheme to something with
many small groups (e.g. state × year FEs or county FEs), the
incidental-parameters problem starts mattering and you should think about
bias correction (Hahn-Newey, or Fernández-Val 2009, *J. Econometrics*,
"Fixed Effects Estimation of Structural Parameters and Marginal Effects in
Panel Probit Models").

### 1.11 Connection to demeaning in linear panel FE

For intuition: in a linear panel model $y_{it} = X_{it} \beta + \alpha_i +
\varepsilon_{it}$, the within transformation $y_{it} - \bar y_i = (X_{it} -
\bar X_i)\beta + (\varepsilon_{it} - \bar\varepsilon_i)$ eliminates $\alpha_i$
algebraically. You then OLS on demeaned data.

In the nonlinear (logit) case there is no analogous algebraic transformation
— logit choice probabilities don't separate additively in $\alpha_g$ the
way linear residuals do. The profile FOC

$$
\sum_s [n_j(s, g) - n_{\text{total}}(s, g) \cdot P(j \mid s, g; \theta, \alpha_g(\theta))] = 0
$$

is the nonlinear analog of "the residual sums to zero within each group."
You're not algebraically demeaning the data; you're choosing $\alpha_g$ so
the model fits the group marginal perfectly, leaving only within-group
variation for $\theta$ to explain. Hence "residualize the CCPs" — the
*model-implied* CCPs at $(\theta, \alpha_g(\theta))$ have been "residualized"
of the group-level marginal.

### 1.12 Literature

Citations I'm confident on; ones I'm hedging are flagged. Most of these
are in standard PhD field-course reading lists, so search by author + year
on Google Scholar to find PDFs.

**Foundational FE / conditional-likelihood:**

- **Andersen, E. (1970).** "Asymptotic Properties of Conditional
  Maximum-Likelihood Estimators." *Journal of the Royal Statistical Society
  Series B*, 32(2), 283–301. *(The original conditional MLE result.)*

- **Chamberlain, G. (1980).** "Analysis of Covariance with Qualitative
  Data." *Review of Economic Studies*, 47(1), 225–238. *(The "FE without
  estimating FE" trick for binary panel logit via conditional likelihood.
  Read this first if you only read one of these.)*

- **Chamberlain, G. (1984).** "Panel Data." Chapter 22 in Z. Griliches & M.
  Intriligator, eds., *Handbook of Econometrics, Vol. 2*. *(The textbook
  treatment.)*

**Profile likelihood (general statistical theory):**

- **Cox, D. R., & Reid, N. (1987).** "Parameter Orthogonality and
  Approximate Conditional Inference." *JRSSB*, 49(1), 1–18. *(Why profile
  Hessian = Schur complement of joint Hessian; orthogonality conditions for
  when profile SE is asymptotically correct without further adjustment.)*

- **Murphy, S. A., & Van der Vaart, A. W. (2000).** "On Profile Likelihood."
  *Journal of the American Statistical Association*, 95(450), 449–465.
  *(Modern reference for asymptotic properties of profile MLE.)*

**Dynamic discrete choice (the NPL / CCP world):**

- **Aguirregabiria, V., & Mira, P. (2002).** "Swapping the Nested
  Fixed-Point Algorithm: A Class of Estimators for Discrete Markov Decision
  Models." *Econometrica*, 70(4), 1519–1543. *(The NPL estimator paper. Read
  for the algorithm. The discussion of nuisance parameters is brief.)*

- **Aguirregabiria, V., & Mira, P. (2007).** "Sequential Estimation of
  Dynamic Discrete Games." *Econometrica*, 75(1), 1–53. *(Extension to
  games. Section on unobserved market heterogeneity is the closest treatment
  of "group-FE in NPL" that I know of.)*

- **Aguirregabiria, V., & Mira, P. (2010).** "Dynamic Discrete Choice
  Structural Models: A Survey." *Journal of Econometrics*, 156(1), 38–67.
  *(The survey. Has a section on unobserved heterogeneity. Good place to
  start if you have time for only one of the AM papers.)*

- **Arcidiacono, P., & Miller, R. A. (2011).** "Conditional Choice
  Probability Estimation of Dynamic Discrete Choice Models with Unobserved
  Heterogeneity." *Econometrica*, 79(6), 1823–1867. *(Uses EM for
  unobserved-type FE in CCP estimation. Different from profile-out — they
  treat $\alpha$ as a random variable to be integrated out, not a fixed
  parameter to be profiled out. The right reference if you ever want to
  switch from FE-as-fixed-parameter to FE-as-latent-type.)*

- **Arcidiacono, P., & Ellickson, P. B. (2011).** "Practical Methods for
  Estimation of Dynamic Discrete Choice Models." *Annual Review of
  Economics*, 3, 363–394. *(Survey covering CCP, NPL, EM, and bias
  correction. Good complement to AM 2010.)*

**Dynamic panel + FE (related but tangential):**

- **Honoré, B. E., & Kyriazidou, E. (2000).** "Panel Data Discrete Choice
  Models with Lagged Dependent Variables." *Econometrica*, 68(4), 839–874.
  *(Conditional-likelihood approach for dynamic binary choice with FE.)*

**Incidental-parameters bias correction:**

- **Hahn, J., & Newey, W. (2004).** "Jackknife and Analytical Bias
  Reduction for Nonlinear Panel Models." *Econometrica*, 72(4), 1295–1319.

- **Fernández-Val, I. (2009).** "Fixed Effects Estimation of Structural
  Parameters and Marginal Effects in Panel Probit Models." *Journal of
  Econometrics*, 150(1), 71–85.

- **Neyman, J., & Scott, E. L. (1948).** "Consistent Estimates Based on
  Partially Consistent Observations." *Econometrica*, 16(1), 1–32. *(The
  original incidental-parameters paper. Mostly of historical interest;
  Hahn-Newey is what you'd actually cite for bias correction.)*

**Textbook treatments:**

- **Aguirregabiria, V. (2021).** *Empirical Industrial Organization:
  Models, Methods, and Applications.* Chapter 2.3 is the source cited in
  `.claude/NPL_REFERENCE.md`. *(Has all the NPL machinery; discussion of
  FEs is in section 2.3.4. NOT confident on the exact publication year —
  there may also be a newer edition. Verify before citing in a paper.)*

- **Train, K. (2009).** *Discrete Choice Methods with Simulation*, 2nd ed.,
  Cambridge University Press. *(Standard reference for discrete-choice
  estimation. Has a chapter on panel data with FE.)*

### 1.13 Application to the UST replacement model

In our model:

- $S = 32$ state cells (8 age bins × 2 wall types × 2 regimes)
- $G = 18$ groups (TX baseline + 17 control states; TX has $\alpha = 0$)
- $J = \{M, E, R\}$ actions
- $p_\theta = 8$ structural parameters: $\kappa_{\text{SW}}, \kappa_{\text{DW}},
  K_{\text{SW}}, K_{\text{DW}}, \gamma_{\text{price,FF}}, \gamma_{\text{price,RB}},
  \gamma_{\text{risk,FF}}, \gamma_{\text{risk,RB}}$
- Current spec: $\mathcal{J}_\alpha = \{M\}$ (alpha in Maintain only)
- Proposed spec: $\mathcal{J}_\alpha = \{M, R\}$ (alpha shifts "stay-in-business"
  propensity)

Under the proposed M+R spec with profile-out:

1. Optimizer searches 8-dim $\theta$ surface only.
2. At each $\theta$ candidate inside the inner optim:
   - Compute $v_M(s; \theta), v_E(s; \theta), v_R(s; \theta)$ via the
     existing `.compute_v_indices_8p` (no change to V inversion).
   - For each $g = \text{AR}, \ldots, \text{VA}$ (17 groups):
     - Solve $\alpha_g$ such that
       $\sum_s [n_M(s,g) + n_R(s,g)] = \sum_s n_{\text{total}}(s,g)
       \cdot [P(M | s, g; \theta, \alpha_g) + P(R | s, g; \theta, \alpha_g)]$
     - Newton: 5–10 iters, sub-millisecond.
   - Substitute $\alpha_g(\theta)$ to compute LL contribution.
3. NPL outer loop wraps this as usual.

In CF welfare computation (per the Semantic-2 convention), the $\alpha_g$
values are discarded — the agent's equilibrium uses $\theta_{\text{struct}}$
only. The profile-out trick is purely a measurement-side reorganization;
it doesn't change the CF.

### 1.14 Practical implementation gotchas

Things that bit me (or are likely to) when implementing this:

1. **The NPL outer-loop convergence criterion now applies to $\theta$ only.**
   The $\alpha_g$'s are deterministic functions of $\theta$ and $P^k$ at every
   step — they don't have a "previous iteration" value to compare against
   between NPL iterations. So $\|\theta^{k+1} - \theta^k\| < \text{tol}_\theta$
   and $\|P^{k+1} - P^k\| < \text{tol}_P$ are the only stopping criteria.

2. **The CCP update step needs $\alpha_g(\theta^{k+1})$ evaluated *at the new
   $\theta^{k+1}$*, not at the previous $\theta^k$.** Easy to forget if you're
   thinking of $\alpha_g$ as "the value at last NPL iter."

3. **Numerical stability of the 1-D solver.** Use log-sum-exp tricks
   for the softmax inside the FOC. Initialize $\alpha_g$ at the previous
   NPL-iter's $\alpha_g$ value (warm start) for fast Newton convergence.

4. **Empty group cells.** If any $g$ has $n_{j \in \mathcal{J}_\alpha}(\cdot, g) = 0$,
   the FOC pushes $\alpha_g \to -\infty$ — undefined. In practice, set
   $\alpha_g$ to a large negative number (e.g. -20) and emit a warning.
   The data-degeneracy issue exists in joint estimation too but is louder
   in profile-out, which is good (catches it instead of hiding it in
   inflated SEs).

5. **Standard errors at the end.** Easiest: form the joint Hessian at
   $(\hat\theta, \hat\alpha(\hat\theta))$ using finite differences over the
   full 25-dim parameter vector, invert, take the $(\theta, \theta)$
   sub-block. This is numerically equivalent to the profile Hessian.
   Don't try to differentiate $\ell^{\text{profile}}(\theta)$ analytically —
   it requires the implicit-function-theorem derivative of $\alpha_g(\theta)$,
   which is doable but error-prone.

---

## Appendix A — Notation cheat sheet for the UST model

| Symbol | Meaning | Where in code |
|---|---|---|
| $\beta = 0.95$ | discount factor | `config$beta` |
| $\sigma_2 = 1.0$ | T1EV scale (single-tier) | `config$sigma2` |
| $V(s)$ | ex-ante value function, $S$-vector | `invert_value_function_replacement` |
| $u_j(s; \theta)$ | structural flow utility | `flow_utilities_replacement_8p` |
| $v_j(s; \theta, P)$ | choice-specific value, $u_j + \beta F_j V$ | `.compute_v_indices_8p` |
| $P(j \mid s)$ | CCP at state $s$ for action $j$ | `fit$P_hat[s, j]` |
| $F_j$ | $S \times S$ transition under action $j$ | `cache$F_maintain`, etc. |
| $\kappa_w$ | exit value, wall $w \in \{\text{SW}, \text{DW}\}$ | $u_E$, in 10K-scale |
| $K_w = \exp(K_{\log, w})$ | replacement cost | $u_R$, in 10K-scale |
| $\gamma_{\text{price}, r}$ | premium semi-elasticity, regime $r \in \{\text{FF}, \text{RB}\}$ | enters $u_M$ |
| $\gamma_{\text{risk}, r}$ | hazard semi-elasticity, regime $r$ | enters $u_M$ |
| $\alpha_g$ | state FE, $g \in \{\text{AR}, \ldots, \text{VA}\}$; TX = 0 | currently in $u_M$ only |
| 1 model unit | $\$10{,}000 / \text{yr} / \text{facility}$ | `SCALE_FACTOR` in 04b prep |

State indexing in `state_lut`: $s_{\text{idx}} \in \{1, \ldots, 32\}$ with
$(A_{\text{bin}}, w_{\text{state}}, \rho_{\text{state}})$ tuple. $w = 1$ is
SW, $w = 2$ is DW. $\rho = 1$ is FF, $\rho = 2$ is RB.

Group index: $g \in \{0, 1, \ldots, 17\}$ in 0-based C++ convention
(`graw`), with $g = 0$ as TX. In R, `alphacpp[g + 1L]` accesses the right
slot.
