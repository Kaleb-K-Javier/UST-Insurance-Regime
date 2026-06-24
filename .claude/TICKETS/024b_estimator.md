# TICKET 024b — Portfolio model: v4 NPL estimator (lambda = 1 flat-MNL milestone)
# Created: 2026-06-19
# Status: READY (solve settled: P1 preconditioner PASS in 024d; math approved by
#         researcher 2026-06-19; verified vs Hortacsu-Joo 2023 eq 2.3.24/2.3.25 and
#         NPL_REFERENCE.md)
# Attempt: 0
# Type: STRUCTURAL ESTIMATOR. Builds the portfolio (count-state) dynamic discrete-
#       choice estimator on the FROZEN first stage + the FROZEN solve stack. Scope
#       of THIS ticket = lambda = 1 (flat MNL); free-lambda nest is a FOLLOW-ON
#       (024f). Deliverable = the first real portfolio parameter estimates.

═══════════════════════════════════════════════════
START HERE
═══════════════════════════════════════════════════
The value-solve is DONE and fast (024a kernel correct; 024c C++ matvec; 024d P1
aging-backbone preconditioner = 152->10 iters). This ticket wires the v4 economic
model onto that solve and runs NPL.

THE KEY COMPUTATIONAL FACT (verified against the book): with the CCPs P held FIXED
inside an NPL inner step and the flow utility LINEAR in the parameters, the value
function is AFFINE in theta:
    V^e(theta) = V^e_const + sum_p theta_p * W^e_p
So you PRECOMPUTE a small basis (one constant solve + one solve per parameter) ONCE
per (NPL-iter, environment), and then EVERY likelihood evaluation in the inner
optimizer is a cheap linear combination + softmax — NO solves during the parameter
search. This is the Aguirregabiria-Mira inner-step structure (Hortacsu-Joo 2023
eq 2.3.24-2.3.25; the inverse [I - beta P]^{-1} is fixed when P is fixed, and the
RHS w is affine in theta). Build the estimator AROUND this; do not re-solve per
optim eval.

Run EVERYTHING via standalone R-4.5.2 x64 (024c protocol E1-E5), LOCAL Windows
(this is where PM07 ran — R 4.5.2 ucrt):
  C:/Program Files/R/R-4.5.2/bin/x64/Rscript.exe

BUILD PROTOCOL — WRITE INCREMENTALLY, ONE PHASE PER RESPONSE (do NOT one-shot the
file). A prior session tried to emit the whole ~600-line PM08 in a single response
and hit the 32000 output-token ceiling (the runner does NOT cap tokens; this is
the per-turn limit). So: write Phase A to PM08_Estimator_v4.R, RUN it, confirm
GATE A, THEN append Phase B (Edit/append, not rewrite), run, confirm GATE B, and so
on through E. Each phase is ~100-150 lines, well under the limit, and the gates are
meant to be validated in order anyway. Never reproduce the whole script in one
message.

═══════════════════════════════════════════════════
RESOLVED PRE-LAUNCH Q&A (2026-06-19; architect — do NOT re-ask these)
═══════════════════════════════════════════════════
A prior coder session crashed mid-Q&A before writing code. The 8 R/data questions
are resolved here; build directly.

LOOKUP STRUCTURES (verified in PM02_Lookups.R):
  Q1 lk$tau : NAMED numeric vector, key = state abbrev (PM02:207). lk$tau["AR"].
     Model units (/SCALE). lk$tau["TX"] == NA (TX has no flat fee) — FF branch
     only; guard against NA.
  Q2 lk$D   : NAMED numeric vector, key = state (PM02:208). lk$D["TX"]==0.5
     ($5,000/10,000). Some FF states lk$D[g]==0 (zero-deductible funds — VALID).
  Q3 lk$pbar: 16x3 matrix, columns named "2006"/"2014"/"2019", rows unnamed in
     MARG order (PM02:146). Index by era string, same as ss$P_RB_all.
  REUSE (do NOT recompute premium/hazard at n' from scratch): index the EXISTING
  per-composition lookups at the post-action comp post_cm[c,(k,m)]:
     RB premium  = ss$P_RB_all[post_cm, era]    (== comp_mat %*% pbar, already built)
     FF premium  = lk$tau[g] * ss$N_vec[post_cm]
     hazard      = lk$h_aw[ ss$h_idx[post_cm] ] (h_idx encodes maj-wall x avg-age bin)
  So Phase A3 is mostly GATHERING existing ss lookups at post_cm, not new math.

Q4 TOLERANCES: use the spec values 1e-5 / 1e-5 / max_npl_iter=10. CLAUDE.md's
   1e-8/1e-7/600 are the OLD single-tank model — do NOT use them here.

Q5 PLATFORM + PARALLELISM: runs LOCAL Windows. RUN SERIAL for this ticket — do
   NOT parallelize. Serial cost ~10 basis solves x 17 env x ~3-5 outer ~ 1-2 hr,
   fine for a correctness milestone. PSOCK + shipping the 1.5GB state space +
   per-worker sourceCpp is bug surface for no correctness gain. Parallelism
   (parLapply/PSOCK on Windows) is a LATER optimization, NOT attempt 1.

Q6 INFEASIBLE ACTIONS = EXACTLY 0 via a feasibility mask (CORRECTNESS, not a
   nicety). Build a [n_states x n_work] mask from feasible_actions(N(c)) in Phase
   A. An infeasible (k,m) at c has rmap/imap = NA -> no valid post_cm -> u_j would
   read premium/hazard at a non-existent comp -> NaN into the softmax + CCP update
   over ALL states. Per NPL_REFERENCE S4/S5 a restricted action is exactly 0 in
   BOTH the softmax denominator AND R[s]. So: feasible work actions + exit (X and
   maintain always feasible) enter softmax/R/M/basis; infeasible excluded (exact
   0). eps_prob=1e-10 floor applies ONLY to FEASIBLE actions with zero observed
   count (log-safety). Do NOT inherit PM07's blanket-eps P_hm onto infeasible cells
   (fine for a timing benchmark, WRONG for the estimator).

Q7 SE: se = NULL acceptable for this milestone. You already compute per-cell
   analytic scores for the gradient, so OPG (sum of outer products of per-obs
   scores) is nearly free — include it IF trivial, else se=NULL with a note. Not
   a gate.

Q8 FOLD a degenerate FF env = operation (a) DROP: remove its agg_counts rows from
   the LL and create NO alpha-slot for it; theta stays shared across survivors
   (mirror the KS/MD exclusion). Do NOT code (b) merge-states (needs an economic
   decision not yet made). Report the excluded list in GATE A; if something other
   than the expected MN/SD trips the < ~30-exit threshold, SURFACE it.

Q9 STARTING theta^0 = ALL ZEROS (clean, reproducible; logit pseudo-LL converges
   from there with the analytic gradient). Do NOT warmstart from the single-tank
   8p+FE fit (different state space / action set; alpha,kappa meanings differ).
   Fallback ONLY if theta=0 converges slowly: seed gamma_p=-2, gamma_r=0.1 (sign-
   correct scale from the canonical 6p fit); leave alpha/kappa/phi/costs at 0.

Q10 OPTIM BOUNDS (L-BFGS-B, finite required): gamma_p,gamma_r in [-20,20]; phi_1..4,
   c_rem, c_inst, kappa_1, alpha_g all in [-50,50]. A param PINNING at a bound = a
   degeneracy signal -> REPORT it, do not silently accept. REQUIRED: log-sum-exp
   stabilized softmax / choice probs (subtract per-state row max) so wide-bound
   line-search probes cannot overflow exp().

Q11 OPG SE: INCLUDE it (per-obs scores already computed for the gradient, so
   sum_i score_i %o% score_i is one extra loop). Save as se_opg; label in config
   "conditional-on-final-P OPG (AM-corrected NPL variance is a later refinement)".
   Not a gate, but ship it.

Q12 era column: fread reads unquoted era as INTEGER 2006; pbar/P_RB_all columns
   are strings -> index ss$P_RB_all[post_cm, as.character(era)] for RB rows. GUARD:
   stopifnot(all(as.character(agg$era) %in% c("2006","2014","2019"))) (era_of_year
   PM02:21 only emits those 3; anything else = data error to surface). FF rows
   carry an era but IGNORE it (FF premium = tau_g*N is era-invariant); no filter.

Q13 data path: identical here("Data","Analysis","pm_agg_counts.csv").

Q14 P1 T_G = ENV-SPECIFIC: rebuild per (env, outer-iter) from P^e[["0,0"]] (match
   the preconditioner to THIS env's (I-beta M^e); keeps iters low as envs diverge).
   `ord`/`A_perm` are STATE-SPACE-ONLY -> build ONCE globally; only the diagonal
   rescale diag(p_maint^e[ord]) is per-env (cheap, reused across that env's basis
   solves). The preconditioner affects SPEED ONLY, never V^e (pinned by the true
   operator + residual gate). At outer iter 0 all envs share P^0=P_hm so it
   coincides with global; diverges at iter>=1. PM07 used global only as a 1-env
   benchmark.

Q15 OPG on aggregated counts: weighted-sum I_hat = sum_{(s,e,j)} n_obs *
   score %o% score is the INFORMATION estimate; VARIANCE is its INVERSE ->
   se = sqrt(diag(solve(I_hat))). Caveats to label: conditional-on-final-P AND
   unclustered (facility-year independence; ignores within-facility serial corr).

Q16 alpha Semantic-2 scope for PM08: alpha IS estimated here (in the optim vector,
   enters work-action LL, saved in fit$theta_hat). CF-exclusion is the CF ticket's
   job. Save a MACHINE-READABLE split: config$structural_params = c(phi_1..4,
   gamma_p, gamma_r, c_rem, c_inst, kappa_1) (the 9 that enter CF) + config$alpha_note
   (Semantic-2 prose), so the CF ticket selects the structural subset cleanly.

═══════════════════════════════════════════════════
FROZEN INPUTS (reuse; do NOT modify)
═══════════════════════════════════════════════════
- Code/Helpers/pm_bellman_kernel.R   — build_kernel(), Mv() (the F_j transition).
- Code/Helpers/pm_matvec.cpp         — 024c C++ matvec (pm_op_build/pm_op_mv).
- Code/Dynamic_Model/PM07_Preconditioner_Bakeoff.R — COPY the P1 preconditioner
  construction (triangular order `ord`/`A_perm`, the dtCMatrix T_G build, Minv_P1)
  and `bicgstab_pc()` VERBATIM. Also copy the pooled Hotz-Miller P_hm build (the
  initial CCP) verbatim from PM07/PM05.
- Output/Estimation_Results/PM_StateSpace.rds (comp_mat, A_age, rmap, imap, N_vec,
  P_RB_all, h_idx, MARG_BIN, N_SIDX), PM_Lookups.rds (pbar 16x3, tau, D, h_aw,
  Gmat, BETA, SCALE, adv, age_breaks, midpts), pm_agg_counts.csv (the likelihood
  counts: sidx, g, era, action, n_obs).
NOTHING in the model/solve is re-derived; M, the F_j, beta are exactly 024a/024d.

═══════════════════════════════════════════════════
THE v4 MODEL (researcher-locked 2026-06-19)
═══════════════════════════════════════════════════
States s = sidx in 1..298,448 = (composition c in 1..74,612) x (capacity bin G in
1..4). Actions at s: WORK actions a=(k,m) feasible per the kernel (maintain=(0,0),
downsize=(k,0), replace=(k,m>0)) + EXIT "X" (absorbing).

POST-ACTION OPERATING FLEET (the locked premium/hazard timing): for work action
(k,m) at composition c, the operating fleet THIS period is
    n' = c with k tanks removed (rmap[c,k+1]) then m installed (imap[.,m+1]),
         PRE-aging (aging is next period's transition F).
Premium and hazard are evaluated at n' (so downsizing visibly cuts the premium).
Implementation note: the post-action composition index is the remove->install of
the CURRENT comp (NOT the kernel's full remove->age->install transition). Build a
per-(c,k,m) post-action comp-id map once (Phase A); reuse rmap/imap semantics
WITHOUT the A_age step.

FLOW UTILITIES (model units = $10k; sigma = 1):
  Work (k,m):
    u^e_{(k,m)}(s) = phi_{G(s)}                      # 4 free capacity levels
                   + alpha_{g(e)}                     # state FE, TX==0, Semantic-2
                   - gamma_p * P^e(n')                # premium at post-action fleet
                   - gamma_r * H(n') * D^e            # OOP risk at post-action fleet
                   - k * c_rem  -  m * c_inst         # intensity costs
  Exit (absorbing):
    u^e_X(s) = kappa_1 * N(s)                         # kappa_0 := 0  (LOCATION NORM)

  where, for environment e:
    P^e(n') = sum_cells n'_cell * pbar[cell, era(e)]   if e is RB (Texas)
            = tau_{g(e)} * N(n')                        if e is FF (control state)
    H(n')   = h_aw[(wall(n')-1)*8 + abar_bin(n')]       (PM02 H-evaluator spec:
              abar = weighted-avg-age midpoint -> bin via age_breaks; wall = SW if
              n_SW>=n_DW else DW)
    D^e     = D["TX"]=0.5 (RB) or D[g(e)] (FF; some FF states D=0)
    N(.)    = tank count of the fleet.

  SIGN CONVENTIONS (NPL_REFERENCE S4 / param_sign_conventions): gamma_p < 0,
  gamma_r > 0 (minus in the equation), enter WORK utility ONLY; exit is kappa_1*N
  only. phi and alpha enter WORK utility only.

PARAMETERS theta:
  Structural (enter CF): phi_1..phi_4 (4), gamma_p, gamma_r, c_rem, c_inst,
    kappa_1  ->  9 total.
  State FE (Semantic-2: LIKELIHOOD only, NOT CF re-solves): alpha_g per control
    state, TX==0  ->  ~14 (one per FF environment).
  Location normalization: kappa_0 := 0 (exit intercept) -> identifies the 4 phi_G.

═══════════════════════════════════════════════════
THE 17 ENVIRONMENTS (contract settings; ONE shared theta across all)
═══════════════════════════════════════════════════
An environment = a distinct contract that sets the reward R; the structural theta
is SHARED across all 17 (this is identification via cross-contract variation, NOT
a data partition). Mapping from a data row's (g, era):
  - g == "TX"  -> RB environment keyed by era in {2006,2014,2019}  (3 envs;
    premium = comp %*% pbar[,era]; D = 0.5; alpha = 0).
  - else (control state) -> FF environment keyed by g  (premium = tau_g * N;
    D = D_g; alpha = alpha_g; era-invariant since tau is a state average).
Drop excluded states KS, MD (PM02 excluded_states). Confirm the realized env set
from the data: distinct (mapped) environments in pm_agg_counts; expect 17 (3 RB +
14 FF; MN/SD may collapse — if an FF env has < ~30 exit events or degenerate
support, FOLD it per the 024a note and log it). Build env_of_row(g, era) and the
inverse (env -> its premium rule, D, alpha-slot) in Phase A; ASSERT every agg_counts
row maps to exactly one environment.

═══════════════════════════════════════════════════
THE NPL LOOP  (Hortacsu-Joo 2023 S2.3.3; NPL_REFERENCE S1-S2; per environment)
═══════════════════════════════════════════════════
For each environment e, over the FULL 298k state space:
  R^e[s]  = sum_{j in work} P^e_j(s) (u^e_j(s;theta) + (gamma_E - log P^e_j(s)))
            + P^e_X(s) (u^e_X(s;theta) + (gamma_E - log P^e_X(s)))      # eq 2.3.24, sigma=1
  M^e     = sum_{j in work} P^e_j ⊙ F_j     # EXACTLY kernel Mv(kernel, P^e, .);
            exit absorbed -> contributes 0 to M (NPL_REFERENCE S4).
  V^e     = (I - beta M^e)^{-1} R^e          # P1-preconditioned BiCGSTAB (024d)
  v^e_j(s)= u^e_j(s;theta) + beta (F_j V^e)(s)   for work j ;  v^e_X(s)=u^e_X(s)  # eq 2.3.27
  Lambda^e_j(s) = softmax_j v^e_j(s)         # eq 2.3.26
Likelihood (OBSERVED cells only, from pm_agg_counts):
  LL(theta | P) = sum_{(s,e,j) in agg_counts} n^e_{s,j} * log Lambda^e_j(s)
NPL outer iteration K:
  theta^K = argmax_theta LL(theta | P^{K-1})         # inner optim (basis-accelerated)
  P^K_j(s) = Lambda^e_j(s; theta^K, P^{K-1})  for ALL s, all e   # CCP update
  STOP when ||theta^K - theta^{K-1}|| < tol_theta AND ||P^K - P^{K-1}|| < tol_P.
  (Book: K>3 buys little accuracy -> expect ~3-5 outer iters. K=1 == Hotz-Miller.)
Initial CCP P^0: the POOLED Hotz-Miller frequency CCP (PM07 P_hm, eps_prob
  smoothed) used for ALL environments at iter 0; environments diverge through the
  per-env updates thereafter.

═══════════════════════════════════════════════════
THE BASIS ACCELERATION  (the inner step does ZERO solves)
═══════════════════════════════════════════════════
Within the inner optim, P^{K-1} is FIXED so M^e and the entropy term are FIXED, and
u^e_j is LINEAR in theta. Hence (verified vs eq 2.3.25):
  R^e(theta) = R^e_const + sum_p theta_p * rho^e_p
    R^e_const = sum_j P^e_j (gamma_E - log P^e_j)                      # theta-free
    rho^e_p   = sum_j P^e_j * (d u^e_j / d theta_p)                    # design column
  V^e(theta) = V^e_const + sum_p theta_p * W^e_p
    V^e_const = (I - beta M^e)^{-1} R^e_const
    W^e_p     = (I - beta M^e)^{-1} rho^e_p                            # ONE solve each
Design columns d u^e_j / d theta_p (precompute Phase A, theta-free):
  d/d phi_G'   = 1{G(s)=G'}      (work only)        [4 columns]
  d/d gamma_p  = - P^e(n')       (work only)
  d/d gamma_r  = - H(n') * D^e   (work only)
  d/d c_rem    = - k             (work only)
  d/d c_inst   = - m             (work only)
  d/d kappa_1  = + N(s)          (EXIT only)
  d/d alpha_g  = + 1{e is FF env g}  (work only; appears in ONE env)
Per (NPL-iter, env) basis cost = 1 (const) + (#params active in env) solves
  ~ 1 + 9 (+1 alpha for FF) ~ 10-11 solves. ALL share the same M^e -> build the P1
  preconditioner ONCE per (NPL-iter, env), reuse across all basis RHS, warm-start.
Then per optim eval (NO solves):
  v^e_j(theta) = u^e_j(theta) + beta (F_j V^e(theta))
              = [u^e_j(theta)] + beta F_j V^e_const + sum_p theta_p * beta F_j W^e_p
  PRECOMPUTE beta*(F_j V^e_const) and beta*(F_j W^e_p) ONCE per (NPL-iter, env)
  (these are F_j applied to the basis vectors = a few matvecs), so each eval is a
  pure linear combination + softmax. Gradient d LL/d theta is then ANALYTIC and
  solve-free (logit score with the precomputed d v/d theta). NO adjoint needed at
  lambda=1.

═══════════════════════════════════════════════════
SCOPE: lambda = 1 (flat MNL) THIS TICKET
═══════════════════════════════════════════════════
All of the above is the FLAT multinomial logit (single sigma=1 scale, no nest).
The basis acceleration is EXACT here because the model is linear in theta. Freeing
the nest parameter lambda (which enters the inclusive value nonlinearly and breaks
basis-in-lambda) is FOLLOW-ON ticket 024f. Do NOT implement a nest here.

═══════════════════════════════════════════════════
PARALLELISM — SERIAL FOR THIS TICKET (Q5)
═══════════════════════════════════════════════════
RUN SERIAL. The 17 environments are independent (basis precompute, eval, CCP
update), so they parallelize cleanly LATER — but NOT in attempt 1. Serial cost
~1-2 hr is fine for the correctness milestone, and PSOCK + 1.5GB state-space
shipping + per-worker sourceCpp adds bug surface for zero correctness benefit.
Get gates B/C/D green serial; environment parallelism (parLapply/PSOCK on Windows)
is a follow-on optimization only if wall time becomes a problem.

═══════════════════════════════════════════════════
DELIVERABLE (phased; each phase has a hard validation gate)
═══════════════════════════════════════════════════
Code/Dynamic_Model/PM08_Estimator_v4.R  (single driver, phases A-E in order).
Saves Output/Estimation_Results/PM_v4_fit_lambda1_<date>.rds and a fit summary
.csv. logs/PM08_*.log (hardcoded sink; section markers; ONE monitor max).

PHASE A — STATIC ASSEMBLY (theta-free; validate then proceed)
  A0 FEASIBILITY MASK [n_states x n_work] from feasible_actions(N(c)) (Q6). Exit
     and maintain(0,0) always feasible. Infeasible work actions are EXACT 0
     everywhere downstream (softmax/R/M/basis); never eps_prob. All A2-A4
     per-action objects are defined on FEASIBLE cells only.
  A1 env_of_row(g,era); env table (premium rule, D, alpha-slot); ASSERT every
     agg_counts row maps to one env; report env list + per-env EXIT counts. Apply
     the Q8 fold: drop any FF env with < ~30 exits (rows out of LL, no alpha);
     report the dropped list; expect ~17 survivors (3 RB + 14 FF, MN/SD may fold).
  A2 post-action comp-id map post_cm[c,(k,m)] (remove->install of CURRENT comp via
     rmap/imap semantics, NO A_age step) for feasible (k,m); ASSERT each in 1..C.
  A3 per (env, state, feasible work-action) lookups AT n' by GATHERING existing ss
     objects at post_cm (Q1/Q3 reuse): RB prem = ss$P_RB_all[post_cm, era]; FF prem
     = lk$tau[g]*ss$N_vec[post_cm]; hazard = lk$h_aw[ss$h_idx[post_cm]]; plus N(s),
     G(s). (Do NOT recompute from rate cards.)
  A4 the theta-free DESIGN COLUMNS d u^e_j/d theta_p (above), per env, FEASIBLE
     cells only (infeasible -> 0 by the A0 mask).
  A5 P^0 pooled Hotz-Miller CCP (verbatim PM07) BUT masked: infeasible -> 0,
     re-normalize over feasible actions per state; per-env CCP container.
  A6 P1 preconditioner statics: `ord`, `A_perm`, uplo (verbatim PM07); assert
     triangularity (below/above-diag mass == 0).
  GATE A: dims + ASSERTS print; dropped-env list printed; spot-check 3 states'
     u-columns by hand vs formula; confirm masked P^0 rows sum to 1 over feasible.

PHASE B — BASIS + VALUE (one env, one test theta; validate vs direct solve)
  B1 build M^e = Mv(kernel, P^e, .) operator (C++ op) + P1 T_G for env e.
  B2 R^e_const, rho^e_p; solve V^e_const, W^e_p via bicgstab_pc + Minv_P1 (warm-
     started). Assemble V^e(theta_test) = V_const + sum theta_p W_p.
  B3 GATE B (correctness of the basis): independently form R^e(theta_test) and
     solve V_direct = (I-beta M^e)^{-1} R^e(theta_test) with the SAME solver;
     assert max|V^e(theta_test) - V_direct| < 1e-8. (This is the affine-in-theta
     proof in code.) Also residual of V_direct < 1e-8.

PHASE C — LIKELIHOOD + GRADIENT (one inner eval; validate gradient)
  C1 v^e_j(theta) and Lambda^e_j(theta) from the precomputed beta*F_j*basis pieces.
  C2 LL(theta|P) over agg_counts; analytic gradient d LL/d theta (logit score).
  C3 GATE C: central finite-difference gradient at theta_test (step 1e-5) vs
     analytic; assert max rel err < 1e-5 on every parameter.

PHASE D — NPL OUTER LOOP (run; validate K=1 and convergence)
  D1 inner optim: optim L-BFGS-B over theta given P^{K-1}, analytic gradient,
     bounds (gamma_p in [-20,20], gamma_r in [-20,20]; phi/c/kappa/alpha wide).
     Parallel basis precompute across envs each inner-loop restart of M (M fixed
     within the inner optim, so basis built ONCE per outer iter, before optim).
  D2 CCP update P^K over all states/envs; outer stop on BOTH tols
     (tol_theta=1e-5, tol_P=1e-5; max_npl_iter=10 — book says ~3 suffices).
  D3 GATE D: K=1 fit == Hotz-Miller two-step (report it); outer loop converges;
     print convergence trace (iter, LL, ||dtheta||, ||dP||).

PHASE E — REPORT + SAVE
  E1 print per OUTPUT RULES: Estimator | Sample | Converged T/F at iter N | LL |
     theta_hat (names+values, gammas with signs) | elapsed | saved path.
  E2 save fit rds (theta_hat, se if cheap via the analytic Hessian/OPG, P_hat,
     config incl beta/sigma/env-map/tols, convergence, LL) + summary csv.

═══════════════════════════════════════════════════
ACCEPTANCE CRITERIA (reviewer)
═══════════════════════════════════════════════════
- [ ] Kernel, pm_matvec.cpp, PM07 P1 code reused VERBATIM/UNMODIFIED; M, F_j, beta
      exactly 024a/024d; no Bellman/CCP math re-derived.
- [ ] Flow utility EXACTLY the v4 spec: premium/hazard at POST-ACTION n'; signs
      (gamma_p<0, gamma_r>0, work-only); exit = kappa_1*N, kappa_0:=0; 4 free phi;
      alpha TX==0, Semantic-2 (in LL, flagged as NOT entering CF).
- [ ] Every agg_counts row maps to exactly one of the (confirmed ~17) environments;
      env premium rule correct (RB: comp%*%pbar[,era]; FF: tau_g*N).
- [ ] R[s] uses (gamma_E - log P_j) per action with P_j>0 (T1EV(0) form); M sums
      WORK actions only (exit absorbed); softmax over the same v_j (NPL_REFERENCE S5).
- [ ] GATE B: basis V(theta_test) == direct solve to < 1e-8 (affine proof).
- [ ] GATE C: analytic gradient == finite-diff to < 1e-5 rel, every param.
- [ ] GATE D: K=1 == Hotz-Miller; outer NPL stops on BOTH ||dtheta||<tol_theta AND
      ||dP||<tol_P; convergence trace printed.
- [ ] No per-optim-eval solves (basis built once per outer iter); no tryCatch->NULL;
      standalone R-4.5.2 x64; ONE monitor max; runs exit 0.
- [ ] Output rules obeyed (no full matrices/CCP/V dumps); fit rds saved with config.

═══════════════════════════════════════════════════
R-NOTES
═══════════════════════════════════════════════════
- data.table / Matrix sparse throughout; sigma=1, gamma_E=0.5772156649, beta from
  lk$BETA (NOT hardcoded). eps_prob=1e-10 floor on P (log-safe), per PM07.
- Likelihood is over OBSERVED cells (agg_counts ~66k rows) — cheap; the CCP update
  Lambda is over ALL states (needed for next M^e). Keep these two scopes distinct.
- Warm-start every basis solve from the previous outer iter's V (M moves little
  between outer iters near convergence).
- Build the P1 preconditioner (T_G) ONCE per (NPL-iter, env); `ord`/`A_perm` once
  for the whole run. Transpose not needed (no adjoint at lambda=1).
- optim L-BFGS-B with the analytic gradient; warn on convergence!=0 (do not silently
  accept). Keep alpha in the optimized vector (Semantic-2 means alpha is in the LL,
  so it IS estimated here; it is dropped only at CF time, later).

═══════════════════════════════════════════════════
GATE DECISION (architect/researcher reads the fit)
═══════════════════════════════════════════════════
PASS = gates B/C/D green + a sensible fit (gamma_p<0, gamma_r>0, converged). Then:
  (1) researcher reviews signs/magnitudes vs the 6p single-tank fit;
  (2) 024f frees lambda (nest) — basis-in-lambda breaks, add the inclusive-value
      nonlinearity + a few extra solves for the lambda direction;
  (3) then CF1-4 + welfare on the portfolio fit (structural theta only; alpha
      dropped per Semantic-2). FAIL on any gate -> fix before running NPL.

═══════════════════════════════════════════════════
ATTEMPT LOG
═══════════════════════════════════════════════════
PRE-ATTEMPT-1 (2026-06-19): coder session read the ticket + kernel + PM07 +
  pm_matvec.cpp + agg_counts, asked 8 R/data questions, then CRASHED before
  writing any code. All 8 answers folded into "RESOLVED PRE-LAUNCH Q&A" above +
  Phase A (A0 feasibility mask, A1 fold-drop, A3 ss-reuse) + the PARALLELISM
  section (serial-first). No code was produced. Restart clean.

ATTEMPT 1 IMPLEMENTATION LOG (2026-06-22, Opus hands-on while coder out) — IN PROGRESS.
PM08_Estimator_v4.R written (single file, Phases A-E). Validated end-to-end on subsets;
full converged run NOT yet done. Fixes/changes made this session (all in PM08):
  (1) GATES B/C PASS: basis V==direct solve 1.2e-9 (affine-in-theta proof); analytic
      grad==finite-diff 3.7e-10. NPL machinery correct.
  (2) BUG dgeMatrix: comp_apply returns S4 dgeMatrix (A_age %*% Y = sparse x dense),
      which propagated into CCPs and broke pm_op_build ("Not a matrix") at iter 2.
      FIX: as.matrix() at ccp_update_env FjV_i + a guard in build_cpp_op. Value-preserving.
  (3) PROFILING: alpha_g (FF state FE) now CONCENTRATED OUT per-env (was jointly optimized,
      caused L-BFGS-B code-1 on 23 flat params). profile_alpha_e() solves the 1-D FOC
      (logit-intercept). Outer optim now 9 structural params; gradient exact by envelope
      theorem. Cuts optim from 134s->faster.
  (4) BUG (profiling): clamped Newton overshot (cc amplified ~1/(1-beta)) and STUCK at +-50
      -> on FULL 17 env, 10 alphas pinned, LL=-1.4e9. The 2-env smoke masked it.
      FIX: uniroot bracketing (score provably monotone). VALIDATED: 4-FF-env test ->
      alphas interior [-0.06,0.06], LL=-194587 (sane), no pins. uniroot==MLE standalone (1.9e-5).
  (5) fnscale = sum(n_obs) (mean-LL, O(1) gradients). Harmless; did NOT fix iter-1 cold-
      start code-1 (that's structural-param scaling; warm iters are code-0, fit still valid).
  (6) TEST KNOBS: PM08_TEST_NENV=N -> N FF + 1 RB subset (fast); PM08_MAXITER=N -> cap iters.
TIMING (full 17 env, 1 iter): basis 1381s (~23min) + inner optim (492s INFLATED by the
  broken Newton; ~1min once uniroot) -> ~25min/healthy pass; converged run ~2-2.5 hr.
  gamma_r=0.044 INTERIOR on full data (the 2-env pin at 20 was a subset artifact).
OPEN / NEXT: (a) inner-optim iter-1 cold-start still code-1 (structural scaling; parscale
  candidate; non-blocking, warm iters code-0); (b) run the FULL 17-env to convergence for
  real estimates (use the uniroot fix); (c) CF + figures plan drafted (engine = theta-fixed
  equilibrium solve reusing kernel+P1; CF1-4 + premium->size; welfare per NPL_REF S5;
  4 researcher decisions pending: mu empirical/stationary, fleet externality formula, CF
  set, E_ext). MODULARIZE PM08 -> pm_estimator_v4_lib.R before CF work.

[reviewer fills subsequent attempts]
