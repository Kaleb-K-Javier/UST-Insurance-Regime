# Structural Model — Current Specification (for the editor)
# As of 2026-06-26. This supersedes the single-tank 6p+FE model AND the earlier
# pooled-exposure portfolio spec (now a REJECTED spec — see §8/§10).
# Plain-language spec + equations + identification + CFs, ready to draft from.
# FINAL SPEC, in one line: operating term = psi * MEASURED revenue R(G,state,era);
#   risk exposure = TWO separate weights (gamma_p on premium, gamma_r on out-of-
#   pocket). gamma_r is identified (~4.32); gamma_p is NOT separately identified ->
#   money-metric reframe + a BRACKETED CF1 (§7-§9). Numbers from the FE-on anchor fit
#   are flagged; psi and the final two-gamma+revenue fit are pending the gated run
#   (tickets 028/029; see §8).

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

  u_a(s,e) = psi * R(G, state, era)       operating value (MEASURED revenue)
           + alpha_e                      environment fixed effect
           - gamma_p * P(n', e)           response to the insurance PREMIUM
           - gamma_r * H(n') * D_e        response to expected OUT-OF-POCKET cost
           - k * c_rem  -  m * c_inst     action costs

  Exit value:  u_exit = kappa_1 * N       (N = number of tanks; kappa_0 := 0)

The two risk terms together are the facility's TOTAL RISK EXPOSURE, but each dollar
channel carries its OWN behavioral weight:
  - P(n', e)      = insurance premium the facility pays in environment e
  - H(n') * D_e   = expected out-of-pocket cost of a leak
                    ( H = facility hazard = prob. any tank leaks;
                      D_e = deductible the facility bears; large, ~$5k-$55k )

Key modeling choices, in plain terms:
  - TWO separate exposure weights, NOT one pooled coefficient. gamma_p is the
    response to a dollar of insurance premium; gamma_r is the response to a dollar of
    expected out-of-pocket leak cost. We do NOT force them equal. (An earlier spec
    pooled them into a single coefficient on premium-plus-out-of-pocket; that spec is
    REJECTED — pooling diluted the response to ~0, see §8/§10.)
  - The OPERATING term is MEASURED revenue, psi * R(G, state, era), NOT free
    intercepts. R is built first-stage from fuel demand x margin scaled by facility
    capacity (representative capacity of bin G x the state-era fuel margin); psi is
    the single free weight translating a model-dollar of revenue into utility. This
    disciplines the "revenue scales with size" channel instead of estimating one free
    constant per capacity bin. (The earlier free per-bin intercepts phi_1..phi_4 came
    back flat with no size gradient and carried no revenue content — also rejected;
    see §8.) Timing: R uses the CURRENT period's capacity bin G; capacity evolution
    bites through the continuation value, not the within-period flow.
  - alpha_e (environment fixed effects) capture everything that differs across
    states for reasons unrelated to exposure (geology, enforcement, economy, data
    coverage). They are LOAD-BEARING: they isolate the within-state exposure signal.
    Texas is the reference (alpha_TX := 0), so Texas's operating level is purely
    psi * R_TX.
  - kappa_0 := 0 is a normalization (the absorbing exit value pins the location of
    the value function); kappa_1 then measures how exit value scales with size. In
    R = capacity x margin the turnover constant is normalized to 1, so psi absorbs
    turnover (a real-dollar revenue reading divides psi by true turnover).

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
  - gamma_r (response to expected OUT-OF-POCKET cost) — IDENTIFIED, ~4.32. Pinned by
    WITHIN-state variation in hazard x deductible: at a fixed state, facilities with
    older or single-wall tanks carry more hazard H, and the out-of-pocket term H*D_e
    moves with it; the deductible D_e is set by the state (large, zero in some state
    funds), orthogonal to a facility's own risk. The OOP channel therefore has a
    clean, age-orthogonal lever — wall type at fixed age -> different hazard; the
    state-set deductible -> different D — that a pure age effect cannot mimic. The
    state fixed effects remove cross-state confounds so the response is within-state.
  - gamma_p (response to the PREMIUM) — NOT SEPARATELY IDENTIFIED. Premium variation
    is almost entirely cross-state, which the state fixed effects absorb; the within-
    state premium variation that remains is collinear with portfolio composition
    (under risk-based pricing the premium moves with hazard). Texas premium data
    begins only in 2006, so there is no within-Texas uniform-vs-risk-based premium
    benchmark to break the collinearity. We therefore do NOT report a free gamma_p
    number; we report it as not separately identified — itself a finding about what
    this data can and cannot price.
  - MONEY-METRIC REFRAME (how one weight serves both channels): there is ONE marginal
    utility of money, identified off the out-of-pocket / deductible channel as
    gamma_r. Under risk-neutrality a dollar is a dollar, so the money-metric benchmark
    IMPOSES gamma_p = gamma_r on the premium. Because gamma_p is not separately
    identified, the premium counterfactual (CF1) is a BRACKET, not a point: gamma_p =
    0 (the premium is a pure transfer firms do not behaviorally respond to) at one end,
    gamma_p = gamma_r (full money-metric — a premium dollar moves behavior exactly
    like an out-of-pocket dollar) at the other. The true response lies inside the
    bracket. To TEST rather than impose the premium channel would require a within-
    regime premium change holding hazard fixed; a second treated state (Iowa's later
    fund -> risk-based transition) is the natural design, flagged as future work.
  - psi (operating value, revenue weight): identified from how operating-vs-exit
    behavior varies with size and with the state-era fuel margin that scales R. With
    free per-bin intercepts the operating profile came back flat (no size gradient, no
    revenue content), motivating forcing the capacity profile to measured revenue and
    estimating the single weight psi.
  - c_rem, c_inst (per-tank removal / install costs): the frequency of downsizing
    and replacement actions.
  - kappa_1 (size gradient of exit value): how the full-exit rate varies with the
    number of tanks.

═══════════════════════════════════════════════════
8. ESTIMATES  (status + numbers)
═══════════════════════════════════════════════════
Empirical anchor in hand (FE-on fit, two-coefficient operating + risk):
    gamma_r (out-of-pocket / risk) = 4.32   IDENTIFIED — the headline behavioral weight
    gamma_p (premium)              ~ 0       NOT separately identified (do NOT report
                                             as a number; see §7)
    c_rem = 3.85,  c_inst = 2.04,  kappa_1 = 0.93
    phi_1..4 ~ -0.55 (the OLD free intercepts; flat across capacity bins -> replaced
                      by psi*R below)
    log-likelihood = -291,648.5;  17 environments;  beta = 0.9957.
THE FINAL FIT (pending, gated): the two-gamma (gamma_p, gamma_r) + measured-revenue
    (psi*R) specification, FE-on. It puts the single revenue weight psi in place of
    phi_1..phi_4 and is the fit the paper reports. Gated on the revenue lookup
    R(G,state,era) landing (ticket 028) and the estimator swap (ticket 029); the
    counterfactual machinery is already built and validated against the model
    primitives. Slot in psi, the refreshed gamma_r, c_rem, c_inst, kappa_1, and the
    log-likelihood when that fit lands. Expect gamma_r > 0 (more out-of-pocket
    exposure -> less maintain) and psi > 0 (more revenue -> more operating).
REJECTED SPEC (report as a negative result, NOT the headline): a POOLED single
    coefficient on premium-plus-out-of-pocket. That fit CONVERGED but the pooled
    weight collapsed to ~0 (and its RB interaction to ~0): premium dominates the
    pooled exposure in magnitude yet is behaviorally inert (gamma_p ~ 0), so pooling
    drags the single coefficient to zero and DESTROYS the real out-of-pocket response
    (gamma_r = 4.32) as collateral. This is precisely why the model keeps the two
    channels separate (see §10).
All values are in model units (1 unit = $10,000 / year / facility).
Standard errors: not yet computed (point estimates); an SE pass is planned.

═══════════════════════════════════════════════════
9. COUNTERFACTUALS
═══════════════════════════════════════════════════
Each CF holds the estimated parameters fixed (including the environment fixed
effects — "Semantic-1": the FEs are real, persistent state characteristics, so they
stay in the counterfactual), changes one policy, and re-solves the equilibrium.
Welfare is computed on the observed state distribution (no population reweighting).

  CF1 — Texas under uniform pricing. Replace Texas's risk-based CONTRACT (premium
        and deductible together) with a representative uniform-state contract, and
        re-solve. Because the premium response gamma_p is not separately identified
        (§7), CF1 is reported as a BRACKET, not a point:
          (a) gamma_p = 0       — the premium change is a pure transfer; behavior
                                   responds only through the deductible / out-of-pocket
                                   channel (lower bound on the behavioral response).
          (b) gamma_p = gamma_r — full money-metric; a premium dollar moves behavior
                                   exactly like an out-of-pocket dollar (upper bound).
        The true effect lies between (a) and (b); report welfare and behavioral
        outcomes as the interval. This is the headline "risk-based vs uniform"
        experiment, now honest about the premium-channel uncertainty. (Implementation:
        the CF solver carries the gamma_p knob; (a) and (b) are two re-solves of the
        same contract swap.)
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
  - The PREMIUM response gamma_p is NOT separately identified (premium is cross-state
    -> FE-absorbed; within-state collinear with hazard). State this as a finding, and
    handle it through the money-metric reframe + the bracketed CF1 (§7, §9) rather
    than reporting a premium coefficient.
  - REJECTED pooled spec: pooling premium and out-of-pocket into one exposure
    coefficient diluted the weight to ~0 and destroyed the real out-of-pocket response
    (gamma_r = 4.32). Report this as a negative result that justifies the two-channel
    spec; do not present a pooled number as a headline.
  - The operating term is MEASURED revenue psi*R, not free intercepts; psi absorbs the
    turnover normalization (kappa := 1), so psi is a utility-per-model-dollar weight,
    not a real-dollar revenue elasticity until divided by true turnover.
  - "Risk-based" = "Texas" in this sample: any risk-based-vs-uniform contrast (incl.
    the CF1 contract swap) conflates the pricing mechanism with Texas-specific factors;
    present it as suggestive, with no within-Texas pre-1999 firm-level contract data
    (Texas was a petroleum-fee state fund pre-reform; premium data begins 2006).
  - Counterfactuals keep the state fixed effects (Semantic-1), so the baseline
    reproduces observed behavior and CF effects are clean perturbations; part of each
    FE may be data artifact rather than behavior — acknowledge.
  - Estimating window: Texas 2006-2020 (premium data begins 2006), controls
    1999-2020. The 2006 start is forced by data, not chosen.
  - Single-tank model relationship: this portfolio model supersedes the earlier
    single-tank specification, which could not represent partial closure.
