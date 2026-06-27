# Structural Model — Current Specification (for the editor)
# As of 2026-06-26. This supersedes the single-tank 6p+FE model.
# Plain-language spec + equations + identification + CFs, ready to draft from.
# NOTE: numbers from the FE-on fit are flagged; the pooled-gamma headline
#       estimates are pending the converged pooled run (see "Estimates").

═══════════════════════════════════════════════════
1. ONE-PARAGRAPH OVERVIEW
═══════════════════════════════════════════════════
A facility that operates underground storage tanks (USTs) is a forward-looking
agent choosing, each year, what to do with its *portfolio* of tanks: keep operating
them, remove some, replace some, or shut down entirely. The model is a dynamic
discrete-choice model in which the state is the facility's tank portfolio (how many
tanks it has, of which wall type and age, and its total capacity) together with the
insurance/regulatory environment it faces. Firms trade off the operating value of
their tanks against the cost of the risk exposure those tanks carry (insurance
premiums plus the out-of-pocket cost of a leak) and the cost of acting. We estimate
the model on Texas (a risk-based-pricing state) and a set of uniform-pricing control
states, then use it to run policy counterfactuals.

═══════════════════════════════════════════════════
2. AGENT AND STATE SPACE
═══════════════════════════════════════════════════
Agent: the FACILITY (not the individual tank). The facility chooses for its whole
portfolio each period; this keeps the exit-correlation across a facility's tanks
structural rather than requiring a tank-level shock.

State s = (composition c, capacity bin G), within an environment e:
  - Composition c: the COUNT of tanks in each of 16 cells, where a cell =
    {wall type} x {age bin}. Wall = single-wall (SW) or double-wall (DW); age in
    8 five-year bins (0-5, 5-10, ..., 35+). Portfolios range from 1 to 6 tanks.
    -> 74,612 distinct compositions.
  - Capacity bin G in {1,2,3,4} (total facility capacity).
  - => 74,612 x 4 = 298,448 states.
  - Environment e (17 of them): 14 uniform-pricing control states (FF) + 3
    Texas risk-based "era" contracts (RB; 2006/2014/2019 rate regimes). Texas is
    the reference environment. (One state, MO, folded out for too few events.)

Discount factor beta = 0.9957 (annual; fixed a priori, not estimated).

Why a portfolio (count) state rather than one representative tank: facilities make
PARTIAL-closure decisions (remove some tanks, keep operating) that a single-tank
model cannot represent; the count state prices that margin. ~82% of facility-years
are homogeneous (all tanks one wall and age bin), so the state is exact for most of
the data and only the large heterogeneous facilities use its full richness.

═══════════════════════════════════════════════════
3. ACTIONS
═══════════════════════════════════════════════════
Each period the facility chooses an action a = (k, m): remove k tanks and install m
new (double-wall) tanks. This single intensity menu nests the economically distinct
actions:
  - Maintain  = (0, 0): keep all tanks, let them age one year.
  - Downsize  = (k, 0): remove k tanks, keep operating with fewer.
  - Replace   = (k, m>=1): remove k, install m new DW tanks (capacity-preserving
                consolidation / upgrade).
  - Exit      = shut the facility down (absorbing).
Which tank is removed follows a deterministic marginal-tank rule (oldest / single-
wall / smallest first), so the action set stays a count choice without tracking
which specific tank leaves. Installs are double-wall by regulation (not a choice).

═══════════════════════════════════════════════════
4. FLOW UTILITY  (the heart of the model)
═══════════════════════════════════════════════════
For a non-exit action a leading to resulting portfolio n' in environment e, the
per-period payoff is:

  u_a(s,e) = phi_{G}                      operating value (by capacity bin)
           + alpha_e                      environment fixed effect
           - ( gamma_pool + gamma_RB * 1[RB_e] ) * E(n', e)   risk-exposure cost
           - k * c_rem  -  m * c_inst     action costs

  Exit value:  u_exit = kappa_1 * N       (N = number of tanks; kappa_0 := 0)

where TOTAL RISK EXPOSURE is

  E(n', e) = P(n', e)        insurance premium the facility pays
           + H(n') * D_e     expected out-of-pocket cost of a leak
                             ( H = facility hazard = prob. any tank leaks;
                               D_e = deductible the facility bears )

Key modeling choices, in plain terms:
  - ONE pooled coefficient gamma_pool on TOTAL risk exposure (premium + expected
    out-of-pocket), NOT two separate coefficients on premium and on out-of-pocket.
    The data cannot separately identify the two (see Identification); a dollar of
    premium and a dollar of expected out-of-pocket are treated as equivalent.
  - gamma_RB is an EXTRA exposure response that applies only under risk-based
    pricing (Texas). It lets the per-dollar response differ between the risk-based
    and uniform regimes — the "is the risk-based pricing mechanism doing extra work"
    knob. gamma_RB > 0 means firms respond more strongly to exposure when pricing is
    risk-based; ~0 means a dollar is a dollar regardless of regime.
  - alpha_e (environment fixed effects) capture everything that differs across
    states for reasons unrelated to exposure (geology, enforcement, economy, data
    coverage). They are LOAD-BEARING: they isolate the within-state exposure signal.
  - phi_G is the operating (revenue) value, allowed to differ by capacity bin.
    [PLANNED REFINEMENT, in progress: replace the free phi_G intercepts with a single
     coefficient psi on MEASURED revenue, u contains psi * R(G, state, era), where R
     is built from gas-demand and margin data. This gives a disciplined revenue-
     scales-with-size channel instead of free intercepts. The rest of the model is
     unchanged. The editor can write the operating term as "phi_G (size-specific
     operating value)" with a footnote that the estimated version uses measured
     revenue psi*R.]
  - kappa_0 := 0 is a normalization (the absorbing exit value pins the location of
    the value function); kappa_1 then measures how exit value scales with size.

═══════════════════════════════════════════════════
5. LAW OF MOTION / TRANSITIONS
═══════════════════════════════════════════════════
  - Aging: under maintain, every tank advances one age bin (deterministic backbone).
  - Action a=(k,m): remove k tanks by the marginal-tank rule, install m DW tanks at
    age 0; capacity bin steps down only at downsize (frozen at maintain/replace).
  - Exit is absorbing (no continuation value).
The transition for an action is a convolution of the per-cell aging kernel with the
remove/install map; the editor does not need the matrix detail, only that the
portfolio evolves deterministically up to aging and the chosen action.

═══════════════════════════════════════════════════
6. VALUE FUNCTION AND ESTIMATION
═══════════════════════════════════════════════════
Standard dynamic discrete choice with i.i.d. Type-I extreme value shocks. The
ex-ante value solves the Aguirregabiria-Mira linear inversion

  V = ( I - beta * M )^{-1} R

with choice probabilities given by the logit over conditional value functions
v_a = u_a + beta * (continuation). Estimation is by Nested Pseudo-Likelihood (NPL):
iterate [solve V given current CCPs] and [update theta by maximizing the pseudo-
likelihood] to a fixed point. Exit is absorbing (drops out of the continuation).
The estimating sample is aggregated facility-year choice counts (TX 2006-2020 +
controls 1999-2020).

═══════════════════════════════════════════════════
7. IDENTIFICATION  (what pins each parameter, plainly)
═══════════════════════════════════════════════════
  - gamma_pool (response to total risk exposure): identified from WITHIN-state
    variation in exposure — at fixed state, facilities with older or single-wall
    tanks, or facing higher deductibles, carry more exposure and exit/downsize more.
    The state fixed effects are what make this clean: they remove cross-state
    confounds so the response comes from within-state variation, which is credible.
  - WHY POOLED, NOT SPLIT (premium vs out-of-pocket): we tried separate
    coefficients. The premium coefficient collapses to ~0 and is not separately
    identified, because (a) under risk-based pricing premium moves with hazard so the
    two are collinear, and (b) premium varies mostly across states, which the fixed
    effects absorb. Texas premium data only begins in 2006, so there is no clean
    within-Texas uniform-vs-risk-based benchmark to separate them. We therefore
    report the pooled response and treat premium-vs-out-of-pocket separation as not
    identified in this data — itself a finding.
  - gamma_RB (extra response under risk-based pricing): identified from the
    difference in the exposure-response slope between the uniform control states and
    risk-based Texas. CAVEAT TO STATE: in this sample "risk-based" = "Texas," so
    gamma_RB conflates the pricing mechanism with Texas-specific factors; it is
    suggestive, not a clean causal estimate of the mechanism.
  - phi_G / psi (operating value by size): cross-size variation in behavior. (With
    free intercepts phi_G came back nearly flat across capacity bins, which is part
    of the motivation for the measured-revenue refinement.)
  - c_rem, c_inst (per-tank removal / install costs): the frequency of downsizing
    and replacement actions.
  - kappa_1 (size gradient of exit value): how the full-exit rate varies with the
    number of tanks.
  - The clean, age-orthogonal levers that discipline the exposure response are WALL
    type (single vs double at the same age -> different hazard) and DEDUCTIBLE
    (varies across states, zero in some state funds): these move exposure without
    being driven by age, so they validate that the exposure response is genuine and
    not just an age-of-tank effect.

═══════════════════════════════════════════════════
8. ESTIMATES  (status + numbers)
═══════════════════════════════════════════════════
Converged FE-on fit (the prior two-coefficient parameterization; the empirical
anchor we have in hand):
    gamma (risk/out-of-pocket) = 4.32   (premium coefficient ~ 0, not separately
                                         identified -> motivates the pooled spec)
    c_rem   = 3.85,  c_inst = 2.04,  kappa_1 = 0.93
    phi_1..4 ~ -0.55 (nearly flat across capacity bins)
    log-likelihood = -291,648.5;  17 environments;  beta = 0.9957.
HEADLINE (pending): gamma_pool and gamma_RB from the converged POOLED run (same
    machinery; the pooled respec passed its gradient-correctness gate). Slot these in
    when the pooled fit lands. Expect gamma_pool > 0 (more exposure -> less maintain);
    gamma_RB sign is the regime-mechanism result.
All values are in model units (1 unit = $10,000 / year / facility).
Standard errors: not yet computed (point estimates); an SE pass is planned — note
    this, since gamma_RB's significance is needed for interpretation.

═══════════════════════════════════════════════════
9. COUNTERFACTUALS
═══════════════════════════════════════════════════
Each CF holds the estimated parameters fixed (including the environment fixed
effects — "Semantic-1": the FEs are real, persistent state characteristics, so they
stay in the counterfactual), changes one policy, and re-solves the equilibrium.
Welfare is computed on the observed state distribution (no population reweighting).

  CF1 — Texas under uniform pricing. Replace Texas's risk-based CONTRACT (premium
        and deductible together) with a representative uniform-state contract, and
        re-solve. Reported as a DECOMPOSITION:
          (a) contract change only (keep the risk-based response), vs
          (b) contract change + turn off the risk-based mechanism (gamma_RB).
        (b) minus (a) isolates the risk-based-pricing MECHANISM effect. This is the
        headline "risk-based vs uniform" experiment.
  CF3 — Replacement subsidy. Lower the install cost (subsidize new double-wall
        tanks), swept over subsidy levels; report the replacement-rate and welfare
        response, by regime.
  CF4 — Removal mandate. Single-wall tanks past age 20 must be removed (the firm may
        downsize or upgrade to satisfy it). Trades firm cost (forced early action)
        against averted leak damage.
  (CF2 — first-best / Pigouvian: deferred; needs population-weighted welfare.)

Welfare accounting (per CF, with a bar chart + table):
  Net social welfare = Producer surplus  -  External leak damage  -  Government outlay
    Producer surplus   = expected discounted facility value
    External damage    = expected discounted leaks x external cost per leak (E_ext)
    Government outlay   = subsidy paid (CF3 only)
  Premiums are internal to firm value (a transfer), so they net out of social welfare.
  E_ext (external $ per leak) is a researcher input; placeholder = $50k.

═══════════════════════════════════════════════════
10. CAVEATS THE WRITE-UP SHOULD STATE
═══════════════════════════════════════════════════
  - Premium-vs-out-of-pocket response is NOT separately identified; we report a
    pooled total-exposure response (a data limitation, stated as a finding).
  - gamma_RB conflates risk-based pricing with Texas-specific factors (RB = TX in
    sample); present it as suggestive, with the within-Texas pre-1999 benchmark
    unavailable (Texas was a petroleum-fee state fund pre-reform; no firm-level
    contract data before 2006).
  - Counterfactuals keep the state fixed effects (Semantic-1), so the baseline
    reproduces observed behavior and CF effects are clean perturbations; part of each
    FE may be data artifact rather than behavior — acknowledge.
  - Estimating window: Texas 2006-2020 (premium data begins 2006), controls
    1999-2020. The 2006 start is forced by data, not chosen.
  - Single-tank model relationship: this portfolio model supersedes the earlier
    single-tank specification, which could not represent partial closure.
