# NPL_REFERENCE.md
# Token-light math reference for the dynamic discrete-choice machinery used
# in this codebase. Load before any spec that touches flow utilities,
# V inversion, CCP updates, or counterfactual equilibrium.
# Source: chapter 2 of "Empirical Industrial Organization", sections 2.2-2.3.

================================================================================
SECTION 1 -- IDENTITIES WE RELY ON
================================================================================

T1EV inclusive value (corollary to lemma 2.2.2, when eps ~ T1EV(0)):

    E[ max_j { v_j + eps_j } ]  =  log( sum_j exp(v_j) )  +  gamma_E

  gamma_E = Euler-Mascheroni constant ~ 0.5772.
  If eps ~ T1EV(-gamma_E) [i.e. shifted to be mean-zero], drop the gamma_E.

Logit CCP (theorem 2.2.1):

    P_j(s)  =  exp( v_j(s) / sigma )  /  sum_k exp( v_k(s) / sigma )

AM CCP identity (eq 2.3.19, footnote 24):

    E[ eps_j | a_t = j, s ]  =  gamma_E - log P_j(s)        [T1EV(0) shocks]
    E[ eps_j | a_t = j, s ]  =        - log P_j(s)          [mean-zero shocks]

  This codebase uses the gamma_E form (location-zero T1EV). Choice probabilities
  identical under either convention; V levels differ by gamma_E / (1 - beta).

AM linear V-inversion (eq 2.3.25):

    V  =  ( I - beta * M )^{-1} * R

  where
    R[s]    =  sum_j P_j(s) * ( u_j(s)  +  sigma * ( gamma_E - log P_j(s) ) )
    M[s,s'] =  sum_j P_j(s) * F_j[s, s']
              (absorbing actions contribute 0 to M for those (s, j) rows)

================================================================================
SECTION 2 -- NPL OUTER LOOP (Aguirregabiria-Mira 2002, section 2.3.3)
================================================================================

Initialize:
  P^0 = Hotz-Miller frequency estimator
        (in this codebase: make_P0_mat_3action -- closures split E vs R by
        prior + observed frequencies; line ~2580 OPTIMIZED.r)

Iterate (k = 1, 2, ...):

  theta^k  =  argmax_theta  LL(theta | P^{k-1})              # inner step

  U^k      =  flow_utilities(theta^k)
  V^k      =  ( I - beta * M(P^{k-1}) )^{-1} * R(theta^k, P^{k-1})
  v_j^k    =  U_j^k  +  beta * F_j %*% V^k    for each j
  P^k      =  softmax over { v_j^k }                         # CCP update

  stop when  ||theta^k - theta^{k-1}|| < tol_theta
        AND  ||P^k - P^{k-1}||         < tol_P

Special cases:
  K = 1            ->  Hotz-Miller two-step estimator
  K -> infinity    ->  Rust NFP (equivalent at fixed point)

For counterfactual welfare (no estimation), the outer loop is dormant
(theta is fixed at the saved estimate); only the inner equilibrium solve
runs to convergence. That inner solve is exactly the same machinery with
theta held fixed.

================================================================================
SECTION 3 -- CODE MAP (improved_estimator_OPTIMIZED.r)
================================================================================

  Chapter symbol            ⇄  Function (line)                   Purpose
  ------------------------     ----------------------------       -------
  u_j(x; theta)             ⇄  flow_utilities_replacement (2626)     4p flow util
                            ⇄  flow_utilities_replacement_8p (3165)  8p flow util
                            ⇄  (8p+FE: structural part = 8p; alpha
                               added at likelihood step, line 3685)
  v_j(x; theta, P)          ⇄  v_M, v_E, v_R in update_ccps_*
                               replacement (2676-2678)
                               and update_ccps_replacement_8p (3192-3194)
  V(x; theta, P) [ex-ante]  ⇄  invert_value_function_replacement
                               (2645) -- implements eq 2.3.25 EXACTLY
                               (specialized to absorbing exit: F_exit
                               row is 0 in M; v_E = u_E with no
                               continuation term)
  P_j(x; theta)             ⇄  update_ccps_replacement[_8p] (2670, 3187)
  Psi(P) outer NPL step     ⇄  body of npl_estimator_replacement loop (2810+)
                               and npl_estimator_replacement_8p (3258+)
                               and npl_estimator_replacement_8p_fe (3755+)
  P^0 from Hotz-Miller      ⇄  make_P0_mat_3action (~2580)
  Equilibrium policy at
   fixed theta (CF use)     ⇄  solve_equilibrium_policy_replacement (2736)
                               (this is the NPL inner loop with theta held fixed)

================================================================================
SECTION 4 -- CONVENTIONS IN THIS CODEBASE
================================================================================

Shocks:        T1EV(0), mean = gamma_E. NOT mean-zero. The gamma_E term shows
               up explicitly in R[s] (line 2655) and in compute_inclusive_value.

Logit scale:   sigma = config$sigma2 (single-tier replacement model).
               In the nested-logit retrofit model (compute_inclusive_value_est),
               sigma1 is the outer-nest scale, sigma2 the inner.

Discount:      beta fixed a priori. Not identified from CCPs alone (chapter
               section 2.3.5). Default 0.9957 in current configs. NEVER
               hardcode -- always read from $config of the saved fit.

Absorbing:     Exit is absorbing. F_exit contributes 0 to M (no continuation
               from exited state). v_E = u_E (no beta * F_exit %*% V term).

Constrained
 action sets:  When a CF restricts the action set at some state s (e.g.,
               CF M mandate: P_M = 0 for A_bin >= 6), the math reduces to
               restricting the softmax denominator and the R[s] sum to
               feasible actions. The eps_floor inside invert_value_function
               _replacement (pmax against config$min_log_val) makes
               P_j = 0 numerically equivalent to a strict zero -- the
               infeasible action's contribution to R[s] and M[s,] is
               machine-precision zero (~1e-298). No code modification of
               the V-inversion function is needed; just pass in a P matrix
               with the right zeros.

Parameter
 sign convs:   See memory file param_sign_conventions.md.
               Briefly: gamma_price < 0 (in parameter), gamma_risk > 0
               (with minus in equation), kappa > 0, K = exp(K_log) > 0.
               Premium and hazard enter u_M only.

Fixed effects:
 (clarification on chapter terminology)
               The chapter's only FE discussion is in section 2.3.4
               (Arcidiacono-Miller 2011), which describes UNOBSERVED
               finite-type heterogeneity estimated via EM. The chapter
               calls time-invariant unobserved types "fixed effects."
               THAT IS NOT WHAT THIS CODEBASE'S 8p+FE MODEL IS.

               Our 8p+FE uses OBSERVED state-of-the-union g as the
               grouping (alpha_g indexed by an observed variable).
               The estimator optimizes alpha jointly with structural
               theta -- no EM, no hidden types.

               Crucially, this codebase makes a non-standard semantic
               choice: alpha enters the LIKELIHOOD (measurement equation)
               but NOT the agent's flow utility during CF equilibrium
               re-solves. CFs use structural theta only.
               (Researcher's "Semantic 2 / nuisance" choice; HANDOFF "KEY
               DECISIONS".)

               Implications:
                 1. The em_npl_estimator function in this codebase IS the
                    Arcidiacono-Miller hidden-type estimator. It has not
                    been run in any current pipeline. Distinct from 8p+FE.
                 2. A "Semantic 1" alternative -- alpha enters equilibrium,
                    so CFs solve 18 separate equilibria (one per g) -- is
                    a real modeling alternative, not in any current spec.
                 3. The paper should state the Semantic 2 assumption
                    explicitly.

================================================================================
SECTION 5 -- INVARIANT CHECKS (use when reviewing new dynamic-DCM code or specs)
================================================================================

For any spec or PR touching the dynamic-choice machinery, verify:

  [ ] V inversion uses ( I - beta * M )^{-1}, with M summing only over
      non-absorbing actions (exit absorbed: no F_exit term).

  [ ] R[s] formula includes sigma * ( gamma_E - log P_j(s) ) for each
      action with P_j(s) > 0. The gamma_E (not 0) is required by our
      T1EV(0) convention.

  [ ] CCP update uses softmax over the same v_j values that feed the
      V inversion, with the same sigma.

  [ ] Outer NPL loop stop criterion checks BOTH ||delta theta|| < tol_theta
      AND ||delta P|| < tol_P. One or the other alone is insufficient.

  [ ] Flow utility signs match memory/param_sign_conventions.md:
        + gamma_price * P_vec  (with gamma_price < 0)
        - gamma_risk * hazard_loss  (with gamma_risk > 0)
        u_E = +kappa, u_R = -K = -exp(K_log)
      Premium and hazard in u_M ONLY.

  [ ] When restricting the action set (CF M, retrofit-infeasibility, etc.),
      the restriction enters BOTH the softmax denominator AND the R[s]
      sum. The eps_floor trick handles both automatically; verify the
      restricted action is set to exactly 0 in the input P matrix.

  [ ] Welfare PV:
        producer surplus  = sum_s mu(s) * V(s)             # V is already a PV
        external damage   = sum_s mu(s) * P_M(s) * h_vec(s) * E_ext / (1-beta)
        govt outlay       = sum_s mu(s) * P_R(s) * K_BASELINE[s] * s_subsidy
                            / (1-beta)
                            (use K_BASELINE, not K_effective: the per-
                             replacement outlay is the SUBSIDY AMOUNT,
                             which is baseline_K * subsidy_fraction,
                             independent of what effective K the firm
                             responds to.)

  [ ] Cross-model CF comparisons (e.g., 4p vs 8p+FE): both use STRUCTURAL
      theta only. Alphas from 8p+FE fit do not enter CF equilibrium.

  [ ] beta read from $config of the saved fit, NOT hardcoded from CLAUDE.md.
      (CLAUDE.md may be stale; the fit's $config is canonical.)

  [ ] Cache primitives F_maintain, F_replace, h_vec, L_vec, hazard_loss,
      P_vec are length-32 (or 36 for the maintain-only model). Coder
      must verify dimensions before any matrix operation.

  [ ] sourceCpp(cpp_engine.cpp) is called BEFORE the first call to any
      equilibrium solve (the C++ paths short-circuit slow R fallbacks).
