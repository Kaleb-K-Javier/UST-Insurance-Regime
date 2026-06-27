# TICKET 024r — Portfolio counterfactual infrastructure + CF1/CF3/CF4
# Created: 2026-06-25
# Status: READY (build the machinery now; final RUN gated on the converged
#         pooled/revenue fit — the solver+welfare are theta-independent and
#         invariant to phi_G vs psi*R, so they can be built and gate-validated
#         in parallel while the revenue work finishes)
# Attempt: 0
# Type: CF BUILD. A reusable equilibrium re-solver for the 298k portfolio model
#       at FIXED theta, observed-distribution welfare accounting, and three
#       counterfactual policies. Reuses the validated estimation machinery
#       (kernel Mv, build_basis_env, bicgstab_pc + P1, feas_mask, post_cm) — NO
#       new Bellman math, NO change to estimation, tolerances, kernel, or
#       pm_matvec.cpp.
# Seat: advanced builder (Sonnet --effort max). Build phase-by-phase with gates.

═══════════════════════════════════════════════════
MATH GROUNDING (NPL_REFERENCE.md — cite when reviewing)
═══════════════════════════════════════════════════
- A CF = NPL outer loop DORMANT, theta fixed, inner equilibrium solved to a fixed
  point under a new policy (§2). Same machinery as estimation with theta held.
- V inversion = ( I - beta M )^{-1} R  (eq 2.3.25); R[s] carries
  sigma*(gamma_E - log P_j) for each feasible j; M sums only non-absorbing actions
  (exit absorbed, F_exit row 0). Reuse the existing kernel/bicgstab path — do NOT
  re-derive.
- Constrained action set (CF4 mandate, §4): set the forbidden action's P=0; the
  eps_floor makes its contribution machine-zero in BOTH the softmax denominator and
  R[s]. NO V-inversion change — just pass a feas_mask with the right zeros.
- Welfare PV forms (§5): producer surplus = sum_s mu(s) V(s); external and govt are
  mu-weighted per-period flows / (1-beta). Adapted to the portfolio below.

═══════════════════════════════════════════════════
KEY DESIGN DECISIONS (locked with researcher this session)
═══════════════════════════════════════════════════
D-FE  Semantic-1: state FEs are KEPT in the CF equilibrium (a DEPARTURE from the
      single-tank Semantic-2). Use theta_hat INCLUDING the profiled alpha_hat as
      fixed flow-utility components. Do NOT re-profile alphas in a CF (no data to
      profile against) and do NOT zero them. Holding alpha_hat fixed makes the
      BASELINE re-solve reproduce observed behavior, so CF deltas are clean
      perturbations. State the Semantic-1 choice in the paper.
D-MU  Welfare on the OBSERVED-sample state distribution mu(s), held FIXED across
      baseline and all CFs (isolates the policy's per-state effect; no population
      weighting — consistent with skipping first-best). Stationary-distribution
      welfare is a documented follow-on, NOT this ticket.
D-NET Net social welfare = ProducerSurplus - External - Govt (the single-tank T017
      convention; premium is internal to V, transfers cancel). E_ext is a
      researcher input (see OPEN INPUTS).
D-CF1 CF1 = FULL CONTRACT swap (premium AND deductible move together; the data has
      no variation to move one alone). Knob via the rb indicator: CF1a keeps rb=1
      (RB response), CF1b sets rb=0 (FF response). CF1b - CF1a = the RB mechanism.
D-CF4 CF4 = removal mandate (option b): an action is feasible iff its RESULTING
      composition post_cm[c,a] has NO (SW, age>A) cell. Both downsize and
      replace(SW->DW) satisfy it; exit is always feasible (never an empty set).

═══════════════════════════════════════════════════
INPUTS
═══════════════════════════════════════════════════
- Saved pooled fit rds: theta_hat (9 structural + profiled alphas), beta, config.
- Output/Estimation_Results/PM_StateSpace.rds, PM_Lookups.rds (kernel inputs).
- Data/Analysis/pm_agg_counts.csv -> build mu(s) (observed state frequencies).
- CF1 uniform contract: control flat-fee + control D (see OPEN INPUTS). A prior
  build exists at Output/Tables/04o_CF_TX_FF_premiums.csv (verify per-tank; reuse
  if applicable).
- E_ext (external $ per leak), L (cleanup $) — researcher (OPEN INPUTS).

═══════════════════════════════════════════════════
PHASE 1 — THE EQUILIBRIUM SOLVER (core; build + gate first)
═══════════════════════════════════════════════════
Build solve_equilibrium_portfolio(theta_hat, policy) returning {V_env, P_env} per
environment. `policy` is a per-env override bundle; absent fields fall back to the
baseline lookups:
    policy[e] = { prem_override = NULL|C-x-n_act matrix,
                  D_override    = NULL|scalar,
                  feas_override = NULL|C-x-n_act logical,
                  rb_override   = NULL|0/1,
                  theta_eff     = theta_hat or a modified copy (e.g. c_inst*(1-s)) }

Loop (warm-started from baseline P):
    repeat until max_e ||P_new[e] - P[e]|| < tol_P:
      for e in 1:n_env:
        basis_e <- build_basis_env(e, P[e], <CF prem/D/feas/rb from policy[e]>)
        V_e     <- assemble basis at policy[e]$theta_eff   # = V_const + sum_p W_p*theta_eff_p
        v_j     <- u_j(policy[e], theta_eff, alpha_hat) + beta * F_j V_e   # over feasible j
        P_new[e]<- softmax_j(v_j) restricted to feas        # infeasible -> 0
    return {V, P}

IMPLEMENTATION NOTES:
- build_basis_env must accept the policy overrides (premium matrix, D, feas_mask,
  rb). Refactor minimally to read overrides when present, else env_tbl/prem_ej/
  feas_mask as today. Keep the baseline path byte-identical when policy is empty.
- The c_inst subsidy (CF3) needs NO lookup override — it is theta_eff with
  c_inst -> c_inst*(1-s). The rb knob (CF1) flows into BOTH the rho for gamma_RB
  (= rb*(rho_gp+rho_gr)) inside build_basis_env AND the u_j term — pass rb_override
  through both.
- alpha_hat enters u_j as a fixed additive term per env (Semantic-1). Reuse PM08's
  utility assembly with alpha = alpha_hat (not profiled, not zeroed).
- Reuse kernel Mv + bicgstab_pc + P1 (Minv) exactly. Do NOT reimplement the solve.

GATE CF-A (the solver-correctness gate — must pass before any CF):
  Run solve_equilibrium_portfolio(theta_hat, EMPTY policy) = the BASELINE re-solve.
  [ ] Converges to a fixed point: max_e ||P_new - P|| < tol_P.
  [ ] At theta_hat, the assembled V passes the existing GATE B identity
      (V_basis == V_direct via a direct bicgstab solve) to < 1e-8 on >=1 env each
      regime. This proves the CF solver reuses the validated value-solve correctly.
  [ ] Baseline aggregate action shares (exit/replace/downsize/maintain) are sane and
      close to the fit's (report the numbers; small drift OK — it is the same fixed
      point the estimator converged to).

═══════════════════════════════════════════════════
PHASE 2 — WELFARE ACCOUNTING (observed mu; build + gate)
═══════════════════════════════════════════════════
mu(s): empirical frequency of state s=(comp c, G) in the estimation panel
(pm_agg_counts). Normalize to sum 1 over included states. Held fixed for all CFs.

compute_welfare(V, P, policy, E_ext, L) returns components (all PV, scalars; and a
by-regime breakdown FF vs RB):
  ProducerSurplus = sum_s mu(s) * V(s)
  External        = sum_s mu(s) * [ sum_j P_j(s) * H(post_cm(s,j)) ] * E_ext / (1-beta)
                    # leaks come from the tanks REMAINING after the action; exit ->
                    # empty comp -> H=0 (contributes 0 automatically). H = facility
                    # hazard = P(any tank leaks) at the post-action composition.
  Govt            = sum_s mu(s) * [ sum_j P_j(s) * m_j ] * c_inst_baseline * s_sub
                    / (1-beta)                         # CF3 only; 0 otherwise
  NetSocialWelfare= ProducerSurplus - External - Govt   # D-NET convention
Also report behavioral outcomes per CF (and by regime): exit rate, replace rate,
downsize rate, expected operating tank-years, premium revenue
(= sum_s mu(s) premium(s) * P_operating(s) / (1-beta)).

GATE CF-B (welfare identity + sanity):
  [ ] NetSocialWelfare == PS - External - Govt exactly (re-derive from the CSV).
  [ ] All shares in [0,1]; V, components finite; External >= 0; mu sums to 1.

═══════════════════════════════════════════════════
PHASE 3 — CF3 SUBSIDY (lightest; build first after welfare)
═══════════════════════════════════════════════════
Policy: theta_eff with c_inst -> c_inst*(1-s) for s in {0.25, 0.50, 0.75}; ALL envs.
Re-solve, compute welfare + outcomes by regime, vs baseline.
GATE CF-3:
  [ ] Replace rate WEAKLY INCREASES in s (monotone response; report the curve).
  [ ] Govt outlay > 0 and increasing in s.
  [ ] Report whether External falls (more replacement -> fewer old leaky tanks).

═══════════════════════════════════════════════════
PHASE 4 — CF1 TX-AS-FF (full contract + knob decomposition)
═══════════════════════════════════════════════════
Policy (TX/RB envs only; controls untouched):
  CF1a: prem_override = control flat fee, D_override = control D, rb_override = 1.
  CF1b: same contract, rb_override = 0  (FF response; gamma_RB drops out).
Baseline, CF1a, CF1b each re-solved; welfare + outcomes for all three.
DECOMPOSITION (the headline): CF1a - baseline = the PRICE/contract effect;
  CF1b - CF1a = the RB-pricing MECHANISM effect.
GATE CF-1:
  [ ] CF1b reproduces an FF-style response on TX (rb=0 path matches the gamma_pool-
      only utility; spot-check one TX state's v_j).
  [ ] Decomposition sums: (CF1b - baseline) == (CF1a - baseline) + (CF1b - CF1a).
  [ ] Caveat printed: gamma_RB carries the RB==TX between-state confound (CF1b-CF1a
      is "RB mechanism OR TX idiosyncrasy"); not clean causal. State in the paper.

═══════════════════════════════════════════════════
PHASE 5 — CF4 REMOVAL MANDATE (feas_mask lookup)
═══════════════════════════════════════════════════
Build feas_override: for each (c, a), FALSE iff post_cm[c,a] contains an
(SW, age>A) cell; TRUE otherwise. A=20, SW-only (DW exempt). ALL envs.
  - Derive from PM_StateSpace cell coding (wall x age bins) + post_cm. Maintain
    (k=0,m=0) becomes infeasible for any comp with an (SW,age>A) cell (it would age
    the SW tank forward). Exit always feasible (post_cm empty -> no offending cell).
  - Assert: every comp has >=1 feasible action (exit guarantees this).
Re-solve, welfare + outcomes by regime, vs baseline. Sweep A in {20, 25} if cheap.
GATE CF-4:
  [ ] No comp has an empty feasible set.
  [ ] Post-mandate equilibrium has ZERO mass on (SW, age>A) operating cells
      (the mandate binds).
  [ ] Welfare reads as the tradeoff: PS down (forced earlier action), External down
      (old SW tanks gone). Report both.

═══════════════════════════════════════════════════
PHASE 6 — DELIVERABLES (per CF: ONE bar chart + ONE table)
═══════════════════════════════════════════════════
Reuse 04v (clean welfare bar) + 04r (table) style. Per CF:
  - Decomposition BAR CHART: welfare components (PS, External, Govt, Net) for
    baseline vs CF (CF1: baseline / CF1a / CF1b three-way). Journal style
    (short title; QJE/AER \textit{Notes:} block; no chart-junk).
  - TABLE: behavioral outcomes (exit/replace/downsize) + welfare components +
    deltas, overall and by regime (FF vs RB). .tex + .csv.
Output to Output/Figures/ and Output/Tables/ with CF-prefixed names.

═══════════════════════════════════════════════════
OPEN RESEARCHER INPUTS (parameterize; do NOT hardcode a guess)
═══════════════════════════════════════════════════
1. E_ext — external $ per leak (and L cleanup $ if the pool split is added later).
   Build welfare PARAMETERIZED by E_ext; researcher supplies the value. The
   single-tank used the T017 $50k-delta family — confirm the portfolio value.
2. Leak-counting — default = expected post-action hazard sum_j P_j H(post_cm).
   Confirm this is the intended leak measure (vs pre-action, vs per-tank).
3. CF1 control contract — control-state MEAN flat fee + mean D (default), with the
   D=0 state-fund variant as a second run. Confirm choice + verify 04o reuse.
4. CF4 — A=20, SW-only (default); confirm + whether to sweep A.
5. CF3 — subsidy levels {.25,.5,.75} (default); confirm.

═══════════════════════════════════════════════════
REUSE / DO-NOT-TOUCH
═══════════════════════════════════════════════════
REUSE: build_basis_env, kernel Mv, bicgstab_pc, P1 (Minv), feas_mask, post_cm,
  the CCP-update path, env_tbl/prem_ej/haz_j lookups.
DO NOT TOUCH: estimation logic, tolerances, the kernel, pm_matvec.cpp, the NPL
  outer loop, PM08's PM08_POOLED/two-gamma paths.
FORWARD-COMPAT: when psi*R replaces phi_G, the solver is UNCHANGED (psi*R is just
  another flow-utility term build_basis_env already assembles). Build the solver and
  welfare AGNOSTIC to the revenue representation — they read theta_hat + the basis,
  not the specific flow-utility parameterization.

═══════════════════════════════════════════════════
ACCEPTANCE CRITERIA (reviewer)
═══════════════════════════════════════════════════
- [ ] GATE CF-A: baseline solver converges + GATE-B V identity < 1e-8; reuses the
      validated kernel/bicgstab (no reimplemented solve).
- [ ] Semantic-1 honored: alpha_hat fixed in the CF utility (not re-profiled, not
      zeroed); baseline reproduces observed shares.
- [ ] GATE CF-B welfare identity exact; GATE CF-3/CF-1/CF-4 each pass.
- [ ] CF1 decomposition additivity holds; RB==TX caveat printed.
- [ ] CF4 binds (zero (SW,age>A) operating mass) and no empty feasible sets.
- [ ] Welfare parameterized by E_ext (no hardcoded guess); mu = observed, fixed.
- [ ] One bar chart + one table per CF, journal style; .tex+.csv+.png/.pdf.
- [ ] Standalone R x64; runs exit 0; ONE monitor max; hard error propagation.
- [ ] DO-NOT-TOUCH set untouched; estimation paths byte-identical.

═══════════════════════════════════════════════════
BUILD ORDER
═══════════════════════════════════════════════════
Phase 1 (solver + GATE CF-A) -> Phase 2 (welfare + GATE CF-B) -> Phase 3 (CF3,
lightest, proves the pipeline) -> Phase 4 (CF1, the headline) -> Phase 5 (CF4) ->
Phase 6 (deliverables). Build the machinery now on the current pooled fit; swap in
the revenue fit when it lands (no solver change). Reviewer pass after each of the
CF phases or at the end.

═══════════════════════════════════════════════════
ATTEMPT LOG
═══════════════════════════════════════════════════
Attempt 1 (2026-06-25, builder Sonnet --effort max): wrote Code/Dynamic_Model/
  PM09_CF_Portfolio.R (1208 lines, Phases 1-6). Builder hit context limit before
  running; architect ran the gates-only test (CF_GATES_ONLY=1) on the local 3-env
  unconverged pooled fit.
  RESULTS:
    - GATE CF-A check 1 (baseline solver convergence): PASS (11 iters, max_dP=1.09e-8,
      converged). BUT slow: 1050s for 3 envs.
    - GATE CF-A check 2 (V_basis == V_direct identity): FAILED at 4.513e-7 on env AR
      vs the absolute 1e-8 threshold. DIAGNOSIS (architect): NOT a bug -- numerical
      accumulation (V_basis sums ~11 bicgstab solves vs 1 for V_direct; 4.5e-7 abs
      ~= 3e-9 RELATIVE on V~O(134); a structural omission would be O(1e-2)). alpha
      handling verified consistent (gate L768-772 == compute_R_env alpha_term).
      FIX APPLIED (architect): gate now uses RELATIVE tol 1e-6 (PM09 L780-790).
    - GATE CF-B (welfare identity): NOT REACHED (script stopped at CF-A check 2).
  CARRY-FORWARD (next builder attempt 2):
    (1) PERF: the CF fixed-point loop does NOT warm-start bicgstab across iters
        (~32s/solve vs ~3s warm in estimation -> 1050s/3env). Add x0 = previous-iter
        V to the bicgstab call in the solve loop (~L489). Full 17-env run is
        infeasible without this. Optional: PSOCK env-parallel like 024p.
    (2) Re-run gates-only -> confirm CF-A (relative) PASS + CF-B welfare identity.
    (3) Then full CFs (CF3/CF1/CF4) on the CONVERGED fit (set CF_FIT_PATH).
  Launch: .\.claude\run_builder_pro_api.ps1 -TicketID 024r -Attempt 2

Attempt 2 (2026-06-26, builder Sonnet --effort max): applied 8 edits (bicgstab
  x0 warm-start L493; CF solves init from P_base not P^0 L957/964/1010; CF3
  sequential warm-start; gate msg -> rel tol; CF_TAU_CTRL_FEE_POS_ONLY knob,
  default fee-positive per architect). Gates-only re-run (3-env local fit):
  CORRECTNESS -- ALL GATES PASS (exit 0):
    - GATE CF-A: PASS. check1 converge; check2 V_basis==V_direct rel FF 3.53e-9 /
      RB 1.77e-9 (abs ~4.6e-7, = attempt-1 value, confirms pure basis-accumulation
      not a bug -> the relative-tol fix was correct); check3 aggregate shares.
    - GATE CF-B: PASS. Net == PS-Ext-Govt; External 11.147 (>=0); finite; mu=1.
    - CF4 cell mask VERIFIED IN-CODE: SW_old cols 1-4 = wall SW, bins 8,7,6,5 =
      age>=20 (AGE_BREAKS [0,5,10,15,20,25,30,35,Inf]); 189,355 (c,a) forbidden;
      exit always feasible. The cell-index concern is closed.
  PERFORMANCE -- WARM-START DID NOT HELP: still ~90s/outer-iter (3 env), same as
    attempt 1 (~30s/solve vs estimation's ~3s). ROOT CAUSE (architect): build_minv_p1
    (L489) and build_cpp_op (L486) are rebuilt EVERY env EVERY outer iter. The P1
    preconditioner is the P-INDEPENDENT aging backbone (024d) -> the triangular
    factorization over 298k states is the bulk of the cost and is recomputed ~11x
    needlessly.
  NEXT (attempt 3 -- the perf fix; correctness is DONE):
    (1) HOIST build_minv_p1 OUT of the `for (it ...)` loop -> build Minv_e ONCE per
        env before the loop, reuse every iter. (Verify build_minv_p1 ignores its P
        arg -- P1 must be the aging backbone; if it uses P, that's a separate bug.)
        Target ~3 min / 3-env baseline, full 17-env run feasible.
    (2) Secondary: build_cpp_op is P-dependent (keep per-iter) but its A_age deep-copy
        is P-independent -> reuse a prebuilt A_age, swap only P_a weights.
    (3) Re-run gates (fast) to confirm unchanged PASS, then full CFs (CF3/CF1/CF4) on
        the CONVERGED fit (CF_FIT_PATH) + confirmed E_ext.
  STATUS: CF infrastructure MECHANICALLY VALIDATED (both gates PASS, CF4 mask
    verified). Only the preconditioner-hoist perf fix remains before the full run.

Attempt 3 (2026-06-26, builder Sonnet --effort max): HOIST FIX APPLIED (5 edits
  to PM09_CF_Portfolio.R; no other changes):
  DIAGNOSTIC: build_minv_p1 in PM08 also uses p_e[["0,0"]] (maintain CCP) —
    PM09 faithfully copies PM08. No separate bug; hoist-with-P_init is valid since
    P1 is an approximation and P_init (P_base for CFs) is close to the equilibrium.
  EDITS:
    (1) L474-486: Minv_envs precompute block hoisted BEFORE `for (it ...)` loop.
        Builds build_minv_p1(P_init[[e]]) ONCE per env; logs timing per-env.
    (2) L491: max_bicg_iters <- 0L initialization added before env loop.
    (3) L504: `Minv_e <- Minv_envs[[e]]` (was `build_minv_p1(P_envs[[e]])`).
    (4) L516: `max_bicg_iters <- max(max_bicg_iters, sol_e$iters)` after V update.
    (5) L525: convergence log now prints `max_bicg=%d` to monitor BiCGSTAB iters.
  SECONDARY (A_age deep-copy) and PSOCK parallel: DEFERRED to attempt 4.
  EXPECTED: ~3 min / 3-env gates run (vs ~16 min attempt 2); full 17-env CF set
    ~2 hr serial (acceptable for overnight server run).
  NEXT: run CF_GATES_ONLY=1 on the local 3-env fit to confirm timing target and
    unchanged PASS; then set CF_FIT_PATH to converged pooled fit + CF_E_EXT and
    run the full CF set on the server.
  Run command (gates-only):
    $env:CF_GATES_ONLY="1"; C:/Programs/R/R-4.5.2/bin/x64/Rscript.exe Code/Dynamic_Model/PM09_CF_Portfolio.R
  Run command (full):
    $env:CF_FIT_PATH="<converged-fit-path>"; $env:CF_E_EXT="5.0"; "C:/Program Files/R/R-4.5.2/bin/x64/Rscript.exe" Code/Dynamic_Model/PM09_CF_Portfolio.R
  RESULT (architect ran gates-only, local 3-env): HOIST BACKFIRED ON THE BASELINE.
    iter1 max_bicg=10 (fresh Minv from P_hm) but iter2 max_bicg=201 (~7.4 min) — the
    once-built preconditioner goes STALE as the baseline P swings off P_hm (max_dP=1.0),
    so BiCGSTAB explodes 10->201, WORSE than rebuild-every-iter. (CFs would be fine —
    they start from P_base, a small perturbation, so P barely moves.) Neither extreme
    works: rebuild-every-iter = slow build x11; hoist-once = stale -> 200+ iters on the
    swinging baseline. NOTE: PM09 run command had a path typo "C:/Programs/R/..." — the
    real path is "C:/Program Files/R/R-4.5.2/bin/x64/Rscript.exe" (fix in any wrapper).

Attempt 4 (2026-06-26, builder Sonnet --effort max): ADAPTIVE PRECONDITIONER —
  IMPLEMENTED BUT SUPERSEDED BEFORE RUNNING.
  EDITS (3 to solve_equilibrium_portfolio in PM09_CF_Portfolio.R):
    (1) Updated comment; added BICG_REFRESH_THRESH <- 30L and
        bicg_iters_prev <- integer(n_env) after hoisted build block.
    (2) Adaptive refresh block before Minv_e <- Minv_envs[[e]]: if
        bicg_iters_prev[e] > 30, rebuild Minv_envs[[e]] from P_envs[[e]].
    (3) bicg_iters_prev[e] <- sol_e$iters recorded after each env solve.
  CONFIRMED with researcher: P_envs[[e]] (not P_new) is the correct P for
    rebuild — it matches the A_x operator being solved this iter.
  SUPERSEDED: researcher ruled that adaptive thrash adds unnecessary complexity
    relative to the actual fix. The correct approach is:
    (a) revert to simple rebuild-every-iter (no hoist, no threshold tracking), AND
    (b) add PSOCK env-parallelism to amortize the rebuild cost. The parallel
        approach makes rebuild-every-iter affordable and is already validated in
        PM08 (024p Lever-B). Current PM09 still has the attempt-4 adaptive code;
        attempt-5 builder must revert it and replace with PSOCK.

Attempt 5 (PLANNED — the actual fix): REVERT ADAPTIVE + PSOCK ENV-PARALLEL.
  (1) REVERT: Remove all attempt-3/4 hoist+adaptive machinery from
      solve_equilibrium_portfolio. Specifically:
        - Remove the Minv_envs pre-build block (before `for (it ...)` loop).
        - Remove BICG_REFRESH_THRESH and bicg_iters_prev declarations.
        - Remove the adaptive refresh `if (bicg_iters_prev[e] > ...)` block.
        - Remove `bicg_iters_prev[e] <- sol_e$iters` line.
        - Restore plain: Minv_e <- build_minv_p1(P_envs[[e]]) INSIDE env loop.
      KEEP: warm-start x0 (V_envs[[e]]) + max_bicg logging — both from attempt 2,
        both correct and useful.
  (2) PSOCK ENV-PARALLELISM (024p Lever-B pattern; reference PM08_Estimator_v4.R
      PM08_NWORKERS block):
        - makeCluster(n_workers, type="PSOCK") near script top; stopCluster on.exit.
        - Knob: PM09_NWORKERS env var; default min(n_env, parallel::detectCores()-1, 8).
        - clusterExport: all statics (ss, lk, kernel, imap0, rmap0, GaT_list, k_vec,
          m_vec, act_keys, C, N_G, N_SIDX, beta, eps_prob, all_action_keys, n_act,
          n_act_p1, maint_idx, feas_mask, prem_ej, haz_j, env_tbl, alpha_hat,
          theta_hat, param_struct, use_pooled, n_env, gamma_E, ord, A_perm,
          uplo_flag, MARG_BIN).
        - clusterEvalQ: sourceCpp(pm_matvec.cpp) + source(pm_bellman_kernel.R) on
          each worker (XPtr not serializable; each worker needs its own compiled op).
          Also define bicgstab_pc, build_minv_p1, build_cpp_op, compute_R_env,
          ccp_update_cf (clusterExport or re-source) on each worker.
        - In solve_equilibrium_portfolio outer loop: replace sequential env for-loop
          with parLapply(cl, seq_len(n_env), function(e) { ... return list(V_e,
          P_new_e, bicg_iters) }). Collect results; extract V/P/bicg per env.
        - ccp_update_cf can run on each worker (it uses env-specific inputs). Return
          P_new alongside V from each worker call. No second pass needed.
        - Convergence check (max_dP across envs) remains in main process.
        - Validate: at n_workers=1, result must be bit-identical to serial path
          (use gates-only run with PM09_NWORKERS=1 vs default; diff should be 0).
  EXPECTED TIMING: baseline ~17 min serial -> ~2-3 min parallel (17 env, ~6-8 workers).
    Full CF set: ~2 hr serial -> ~15-20 min parallel. Acceptable for overnight server run.
  Run command (gates-only, local 3-env fit):
    $env:CF_GATES_ONLY="1"; "C:/Program Files/R/R-4.5.2/bin/x64/Rscript.exe" Code/Dynamic_Model/PM09_CF_Portfolio.R
  Run command (full, server):
    $env:CF_FIT_PATH="<path>"; $env:CF_E_EXT="5.0"; "C:/Program Files/R/R-4.5.2/bin/x64/Rscript.exe" Code/Dynamic_Model/PM09_CF_Portfolio.R
  GATES: same GATE CF-A/CF-B PASS as attempt 2 (correctness unchanged by perf work).

Attempt 4 (RAN — adaptive refresh; architect-observed):
  *** CORRECTION TO THE ATTEMPT-3/4 PREMISE: the P1 build is CHEAP. ***
  Log: "P1 built in 11.5s (0.68s/env)" for 17 envs — NOT the ~25s we both assumed.
  The build was never the bottleneck; the cost is the BiCGSTAB SOLVE, and a FRESH
  preconditioner keeps it ~14 iters vs ~200 stale. So attempts 3-4 (avoid rebuilding)
  optimized the wrong thing.
  ENV COUNT: 17 (the CF solves ALL state-space envs, not the fit's 3 — earlier
  "3-env" framing in this log is WRONG; it's always been a 17-env solve).
  WHAT HAPPENED: baseline from P^0 has max_dP=1.0 for iters 1-4 (P swings fully), so
  the reactive trigger (rebuild when PREV solve >30) THRASHES: iter3 refresh->bicg 14,
  iter4 no-refresh (prev=14<30) but operator moved to iter-3's P -> bicg 207, iter5
  refresh->14, ... always one step behind a fully-swinging P. iter5 max_dP drops to
  0.45 and it settles fast thereafter. NET: adaptive is WORSE than plain rebuild-
  every-iter on the baseline, for no gain (the avoided build is cheap). Run still
  finishes + PASSES (correctness perf-independent).

Attempt 5 (PLANNED — the actual fix): REVERT + PARALLELIZE.
  (1) REVERT hoist/adaptive (attempts 3-4) -> plain REBUILD-EVERY-ITER (build 0.68s/
      env is cheap; fresh preconditioner keeps BiCGSTAB ~14). Simplest, no thrash.
  (2) PSOCK env-parallelism = THE real lever: the 17 env solves per outer iter are
      independent -> parallelize across 16 server cores (024p Lever-B pattern; each
      worker rebuilds its own cpp op + Minv; XPtr not serializable). ~17min serial
      baseline -> ~2-3min; full 7-scenario CF set ~2hr -> ~15-20min. Validate
      parallel==serial (bit-identical) per 024p.
  (3) Keep warm-start x0 + max_bicg logging.
  Then full CFs on the converged fit (CF_FIT_PATH) + confirmed E_ext.
  LESSON: measure the cost of a step before optimizing it — attempts 3-4 optimized a
  0.68s/env operation under an assumed-25s premise.
