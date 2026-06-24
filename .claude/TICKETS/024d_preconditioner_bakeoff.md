# TICKET 024d — Portfolio model: aging-backbone preconditioner for the value solve
# Created: 2026-06-19
# Status: READY (follows 024c: C++ matvec is CORRECT but at the memory-bandwidth
#         floor — only 1.3x over R. The lever is ITERATION COUNT, not matvec cost.)
# Attempt: 0
# Type: NUMERICAL PRECONDITIONER + VALIDATION + RE-BENCHMARK. One focused job:
#       build a model-structured preconditioner that collapses BiCGSTAB iterations
#       on the FROZEN (I - beta M) V = R system, PROVE it does not change the
#       solution (oracle), and re-benchmark. NO estimator, NO NPL loop, NO change
#       to M / R / P / flow-utilities — all reused verbatim from PM05 / 024a.

═══════════════════════════════════════════════════
START HERE
═══════════════════════════════════════════════════
024c ported the structured matvec to C++ and PROVED it correct (cpp Mv == R Mv to
0.000e+00; G4 6.8e-12). But the C++ matvec is only 1.3x faster than R (450ms vs
600ms): the heavy step (A_age sparse x dense) was ALREADY compiled inside R's
Matrix package, so the bottleneck is memory bandwidth, not interpreter overhead.
The matvec is at its hardware floor. Therefore the path to feasibility is NOT a
cheaper matvec — it is FAR FEWER matvecs per solve: the unpreconditioned BiCGSTAB
takes ~152 iterations (each = 2 matvecs). This ticket cuts that with a
preconditioner built from the model's own structure.

NOTHING ABOUT THE MODEL CHANGES. The linear system (I - beta M) V = R, the
transition M, the reward R, the CCPs P, beta, and the kernel are all FROZEN and
reused verbatim from PM05_Solver_Bakeoff.R / pm_bellman_kernel.R. A preconditioner
is a pure numerical accelerator: it changes only HOW FAST you reach V, never WHAT
V is. Correctness is guarded end-to-end by the residual check and agreement with
the 024c/PM05 solution (the oracle). This is why no Bellman / CCP / flow-utility
math is touched here and none may be.

Run EVERYTHING via the standalone interpreter:
  C:/Program Files/R/R-4.5.2/bin/x64/Rscript.exe
(Reuse the already-validated C++ matvec from 024c — Code/Helpers/pm_matvec.cpp.
This ticket adds NO new C++; the preconditioner solves are pure R / Matrix.)

═══════════════════════════════════════════════════
THEORY — WEB-VERIFIED 2026-06-19 (cite in the Attempt Log)
═══════════════════════════════════════════════════
Three facts ground the design; all confirmed against the literature:

T1. (I - beta M) IS A NONSINGULAR M-MATRIX. M is nonnegative and sub-stochastic
    (PM05 measured max row sum = 1.000, and row sums are actually < 1 wherever
    exit has mass since Mv excludes the exit action "X"). With beta = 0.9957 < 1,
    (I - beta M) has nonpositive off-diagonals, positive diagonal, and is strictly
    diagonally dominant (row sums >= 1 - beta > 0). Standard discounted-MDP
    policy-evaluation structure (Bertsekas DP; Stewart, Computations with Markov
    Chains).

T2. SPLITTING / TRIANGULAR PRECONDITIONERS ARE SAFE FOR M-MATRICES. Meijerink &
    van der Vorst (1977) proved incomplete LU exists BREAKDOWN-FREE for any
    M-matrix and any sparsity pattern (Argonne, Purdue, Netlib Templates, UCLA).
    Regular splittings (Jacobi, Gauss-Seidel = triangular) converge for
    M-matrices (Varga). So a triangular preconditioner built from a sub-operator
    of M is well-posed by construction.

T3. THE MONOTONE-AGING BACKBONE IS ACYCLIC -> EXACTLY INVERTIBLE IN ONE SWEEP.
    Topological Value Iteration (Dai et al. 2011, cited 94+): decompose the
    transition graph into SCCs and back up in topological order; an ACYCLIC MDP
    is solved EXACTLY in one ordered sweep (Stanford CS221; CMU "Optimal Backup
    Order for Acyclic MDPs"). In our model AGING IS MONOTONE — a tank's age bin
    only increases (bin 8 absorbing, adv[8]=0), so the aging operator A_age is
    acyclic: under an age-graded ordering it is TRIANGULAR. The only edges that
    create cycles are the rare reset actions (install / replace / downsize), which
    carry ~2% of the work-action mass (maintain is ~98%). So:
       M  =  M_maintain (dominant, triangular)  +  M_reset (rare, breaks triangularity)
    Invert the triangular backbone exactly and cheaply; let Krylov clean up the
    small, beta-discounted reset perturbation.

═══════════════════════════════════════════════════
THE PRECONDITIONER (P1 — aging backbone)  [PRIMARY DELIVERABLE]
═══════════════════════════════════════════════════
Preconditioner  N_pc = I - beta * M_maintain , applied EXACTLY by triangular
substitution. M_maintain is the maintain action (k=0,m=0) only:
    M_maintain = diag(p_maint) %*% (I_4 (kron) A_age)
where p_maint = P_hm[["0,0"]]  (C x 4 maintain CCP, length-4C as.vector column-
major = sidx order), and Gmat[["nowork"]] = I_4 so capacity bin G is unchanged
(the 4 G-blocks are independent — block-diagonal in G).

WHY THIS IS CHEAP AND EXACT:
  - A_age is TRIANGULAR under the age-graded composition order (T3). Define
        aging_potential(c) = sum_cells comp_mat[c, ] * (8 - MARG_BIN)
    (MARG_BIN from PM03: bin 8 = oldest, bin 1 = youngest; potential = remaining
    youth). Aging advances >= 1 tank one bin older => strictly LOWERS potential.
    Order compositions by DECREASING aging_potential (youngest first); then every
    off-diagonal A_age edge points to a strictly-lower-potential comp = strictly
    to one side of the diagonal => A_age is triangular. Ties in potential cannot
    be connected by an A_age edge (an edge strictly lowers potential), so the
    within-level order is irrelevant and triangularity is robust. HARD-ASSERT it
    (below).
  - diag(p_maint) and (I_4 kron .) preserve triangularity, so N_pc is
    block-triangular (4 independent C x C triangular blocks, one per G).
  - Triangular solve = forward/back substitution, O(nnz(A_age)) ~ one matvec. NO
    factorization, NO fill-in, NO M ever formed. The permutation depends ONLY on
    the state space (compute ONCE, env- and NPL-iter-independent). The per-(NPL-
    iter, env) build is just rescaling A_age rows by p_maint[,G] — near-free.

This is exactly the property that makes P1 preferable to a generic ILU(0) here:
ILU(0) is also sound (T2) but needs the full M FORMED (PM05: 1.58 GB, 134 s) and
factored every (NPL-iter, env); P1 forms nothing and reuses A_age already in the
kernel. (ILU(0) is the documented FALLBACK if P1 misses target — see GATE.)

═══════════════════════════════════════════════════
INPUTS / REUSE  (do not rebuild; do not modify)
═══════════════════════════════════════════════════
- Code/Helpers/pm_bellman_kernel.R   — source; REUSE build_kernel(), Mv(),
  comp_apply(), netbin(). UNMODIFIED (the oracle).
- Code/Helpers/pm_matvec.cpp         — source via Rcpp::sourceCpp(); REUSE the
  024c C++ matvec for the operator A_x(). UNMODIFIED. (If sourcing fails, follow
  024c build protocol E1-E5; do NOT fall back silently to a slow path.)
- Output/Estimation_Results/PM_StateSpace.rds (comp_mat, A_age, rmap, imap,
  N_vec, N_G, P_RB_all, h_idx), PM_Lookups.rds (BETA, h_aw, D, Gmat).
- PM05_Solver_Bakeoff.R, lines 42-126: COPY VERBATIM the P_hm build and the
  R_g5 (RB-2006) construction and the A_x() operator. Header-comment the copy.
- PM05_Solver_Bakeoff.R, lines 334-391: COPY VERBATIM the bicgstab() baseline
  (unpreconditioned) for the head-to-head. Do NOT source(PM05) (it reruns the
  134 s M-form + LU OOM).

═══════════════════════════════════════════════════
DELIVERABLE
═══════════════════════════════════════════════════
Code/Dynamic_Model/PM06_Preconditioner_Bakeoff.R
  -> sources kernel + pm_matvec.cpp; builds P_hm, R_g5 (verbatim from PM05);
     builds the P1 preconditioner; runs unpreconditioned vs P1-preconditioned
     BiCGSTAB; validates against the oracle; benchmarks; prints recommendation.
  -> saves Output/Estimation_Results/PM_Precond_Bakeoff.rds.
  -> logs/PM06_*.log (hardcoded-name sink; cat section markers; ONE monitor max).

═══════════════════════════════════════════════════
PSEUDOCODE — PM06
═══════════════════════════════════════════════════
0. SETUP. source kernel + sourceCpp(pm_matvec.cpp); load 023 objects;
   kernel <- build_kernel(ss, lk); beta <- lk$BETA; C, N_G, N_SIDX = C*N_G.
   Build P_hm and R_g5 (RB-2006) and the C++-backed operator
       A_x(x) = x - beta * cpp_Mv(op, x)        # op = the 024c built operator
   VERBATIM from PM05. (Also build OH FF R_OH per 024c if available; OPTIONAL
   second RHS — RB-2006 is the required one.)

1. BUILD THE TRIANGULAR ORDER (once).
   1a. pot <- as.vector(ss$comp_mat %*% (8L - MARG_BIN))   # length C; MARG_BIN
       reconstructed as c(8:1, 8:1) (PM03 line 31) — or read from ss if stored.
   1b. ord <- order(pot, decreasing = TRUE); inv_ord[ord] <- seq_len(C).
   1c. A_perm <- ss$A_age[ord, ord]   # permuted aging operator (dgCMatrix)
   1d. HARD ASSERT TRIANGULARITY (the T3 premise — if this fails, STOP and report;
       the whole design rests on it):
         tri_lower_nnz <- sum(A_perm@i > <col-index of each entry>)   # entries
           strictly below the diagonal in the permuted order
         Implement cleanly: T_test <- A_perm; check
           isTriangular(as(T_test,"dtCMatrix"), upper=TRUE) OR
           max(|tril(A_perm, -1)|) == 0   (Matrix::tril); assert one orientation
           holds to 0 (exact zeros — these are structural).
       Report which orientation (upper vs lower) and nnz on each side.

2. BUILD THE P1 PRECONDITIONER APPLY  N_pc^{-1}.
   For each G in 1..4:
     p_maint_G <- P_hm[["0,0"]][, G]                 # length C
     T_G <- Diagonal(C) - beta * Diagonal(x = p_maint_G[ord]) %*% A_perm
            # triangular in the permuted order; mark as dtCMatrix (triangular)
     store T_G (4 of them).
   Minv_P1(r):  # r is length N_SIDX, column-major (G-blocks) = sidx order
     reshape r -> R_mat (C x 4)
     for G in 1..4:
       b  <- R_mat[ord, G]               # permute into triangular order
       yG <- Matrix::solve(T_G, b)       # triangular substitution (NOT full LU)
       Y_mat[ord, G] <- yG               # un-permute
     return as.vector(Y_mat)
   NOTE: ensure Matrix uses triangular solve (mark T_G triangular / use
   solve on a dtCMatrix); time the first solve to confirm it is ~one-matvec
   cheap, NOT factorizing.

3. PRECONDITIONED BiCGSTAB (right-preconditioned; standard, Barrett et al.
   "Templates"; do NOT improvise — this is the unpreconditioned PM05 bicgstab
   with two preconditioner solves added). Apply A_x via the C++ matvec.
     r = b - A_x(x);  rhat = r;  rho=alpha=w=1;  v=p=0
     repeat:
       rho1 = <rhat,r>;  (guard |rho1|<1e-100 -> restart rhat=r, as in PM05)
       bta  = (rho1/rho)*(alpha/w);  p = r + bta*(p - w*v)
       phat = Minv(p)                       # <-- preconditioner solve
       v    = A_x(phat)
       alpha= rho1/<rhat,v>;  (guard denom)
       s    = r - alpha*v
       if ||s||_inf < tol: x = x + alpha*phat; break
       shat = Minv(s)                       # <-- preconditioner solve
       t    = A_x(shat)
       w    = <t,s>/<t,t>;  (guard <t,t>)
       x    = x + alpha*phat + w*shat
       r    = s - w*t;  rho = rho1
       if ||r||_inf < tol: break
     (Minv = Minv_P1 for the preconditioned run; Minv = identity reproduces the
      PM05 baseline — assert it matches PM05's 152/145 iters as a self-check.)
   tol = 1e-9, max_it = 2000, x0 = 0 (cold) and x0 = V_cold*(1+0.01*rnorm) (warm,
   1% RELATIVE — same protocol as PM05).

4. VALIDATE (correctness is non-negotiable; a preconditioner must not move V):
   4a. RESIDUAL: ||A_x(V_pc) - R||_inf / ||R||_inf < 1e-8.
   4b. AGREEMENT WITH ORACLE: max| V_pc - V_baseline | < 1e-6, where V_baseline =
       the UNPRECONDITIONED BiCGSTAB solution computed in this same script (the
       024c/PM05-equivalent oracle). The preconditioned and baseline solutions
       must be the same vector.
   4c. G4 ANALYTIC (cold, x0=0): with P_maintain = {"0,0"=1, "X"=0} and R=r*1
       (r=3), preconditioned BiCGSTAB must give max|V - r/(1-beta)| < 1e-7.
       (Sanity: for the G4 system M = M_maintain exactly, so P1 is the EXACT
       inverse and should converge in 1 iteration — report the iter count; ~1
       confirms the triangular solve is correct.)

5. BENCHMARK + RECOMMENDATION (printed + saved):
   PRIMARY METRIC = iteration count (matvec-implementation-independent):
     baseline cold/warm iters  vs  P1 cold/warm iters.
   SECONDARY = wall time (uses the 024c C++ matvec) + Minv apply ms (median of 5).
   Per-(NPL-iter, env) projection for P1:
     precond_build_sec (the 4 T_G assembly) + n_evals * precond_solve_sec,
     n_evals=100, n_env=17. Report hr/NPL-iter. Compare to baseline 49.2 hr.
   PRINT the explicit recommendation per the DECISION RULE below.

═══════════════════════════════════════════════════
DECISION RULE (pre-committed)
═══════════════════════════════════════════════════
P1 PASS if: triangularity assert holds (step 1d), correctness holds (4a-4c), AND
  cold iters drop from ~152 to <= ~30 (target; ~10-20 expected). Then RECOMMEND
  P1 for 024b: build the 4 T_G once per (NPL-iter, env) [near-free], apply Minv_P1
  inside preconditioned BiCGSTAB, warm-start across optim evals.
P1 MISS if: cold iters still > ~50, OR the triangular solve is not ~one-matvec
  cheap, OR triangularity fails. Then report WHY and escalate to the ILU(0)
  fallback ticket (024e): form M once per (NPL-iter, env) [134 s, amortized],
  ILU(0) (breakdown-free by T2), apply two triangular solves. Do NOT silently
  build 024b on the unpreconditioned 49 hr/iter solve.

═══════════════════════════════════════════════════
ACCEPTANCE CRITERIA (reviewer)
═══════════════════════════════════════════════════
- [ ] pm_bellman_kernel.R, pm_matvec.cpp, cpp_engine.cpp, improved_estimator_
      OPTIMIZED.r ALL UNMODIFIED. No M / R / P / flow-utility / Bellman math
      changed — P_hm, R_g5, A_x copied verbatim from PM05.
- [ ] Triangularity of permuted A_age HARD-ASSERTED to exact zero on one side
      (step 1d); orientation + per-side nnz reported.
- [ ] Minv_P1 uses TRIANGULAR substitution (dtCMatrix solve), not full LU; first-
      solve time confirms ~one-matvec cost (no hidden factorization).
- [ ] Unpreconditioned baseline in-script reproduces PM05 iters (~152 cold) — the
      Minv=identity self-check.
- [ ] Correctness: residual < 1e-8 (4a); agreement with baseline V < 1e-6 (4b);
      G4 cold < 1e-7 and converges in ~1 iter (4c).
- [ ] Iteration counts (baseline vs P1, cold + warm) reported; P1 evaluated
      against the DECISION RULE; explicit 024b recommendation printed.
- [ ] PM_Precond_Bakeoff.rds saved (iters_baseline_cold/warm, iters_p1_cold/warm,
      minv_ms, precond_build_sec, resid_p1, agree_p1_baseline, g4_dev_p1,
      proj_hr_per_npl_iter_p1, triangular_orientation, recommendation).
- [ ] runs exit 0; standalone R-4.5.2 x64; ONE monitor max on any long run.

═══════════════════════════════════════════════════
R-NOTES
═══════════════════════════════════════════════════
- Matrix::tril / triu for the triangularity check; coerce T_G to "dtCMatrix"
  (triangular) so Matrix::solve does substitution, not LU. Verify with a timing.
- as.vector(P_hm[["0,0"]]) is column-major (G-blocks) = sidx order; keep the
  permutation acting on the C composition index WITHIN each G-block (the 4 blocks
  share ord but have block-specific diag(p_maint[,G])).
- Guard the BiCGSTAB scalars exactly as PM05 (hard error on true stall, never
  return NULL).
- No tryCatch-returning-NULL anywhere. The only acceptable guard is the kernel/
  cpp source preflight (which must hard-error, not fall back to slow R).
- Hardcoded-name sink to logs/PM06_*.log; cat section markers; ONE monitor max
  (024a died of 5 concurrent monitors — run foreground or ONE handle, read the
  log when done).

═══════════════════════════════════════════════════
GATE DECISION (architect reads the table, then rules)
═══════════════════════════════════════════════════
PASS (P1 cuts iters to target, correct, cheap apply) -> P1 is the 024b solve;
  write 024b (estimator) on preconditioned BiCGSTAB + warm-start + analytic NPL
  score gradient. FAIL -> 024e ILU(0) fallback before 024b. Either way 024b is
  written only after a preconditioner clears this gate.

═══════════════════════════════════════════════════
ATTEMPT LOG
═══════════════════════════════════════════════════
ATTEMPT 1 — 2026-06-19 — REVIEW (Opus architect/reviewer) — VERDICT: PASS.
Deliverable: Code/Dynamic_Model/PM07_Preconditioner_Bakeoff.R (renamed from PM06 —
  PM06 taken by 024c matvec driver; rds = PM_Precond_Bakeoff.rds). Ran exit 0,
  standalone R-4.5.2 x64. Verified independently from the code + log (not the
  coder summary):
  FAITHFULNESS: P_hm/R_g5/A_x copied verbatim from PM05 lines 42-126; single
    parameterized bicgstab_pc (right-precond, Minv applied at phat=Minv(p)/
    shat=Minv(s)); Minv=identity self-check; triangularity assert measures both
    tril/triu mass and DRIVES uplo (not hardcoded); T_G built as dtCMatrix
    (triangular substitution, not LU); G4 uses a DEDICATED p_maint=1.0
    preconditioner (Q4); ss$MARG_BIN with defensive check (Q6). Kernel,
    pm_matvec.cpp, cpp_engine.cpp, improved_estimator_OPTIMIZED.r all UNMODIFIED.
  NUMBERS (log PM07_20260619_185001):
    Triangularity: below-diag mass = 0.000e+00 (EXACT) -> A_perm UPPER triangular.
      pot range [0,42] (42 = 6 tanks x 7, all youngest). T3 premise holds exactly.
    Baseline self-check: 152 cold iters (== PM05 oracle) -> bicgstab_pc validated.
    P1: 10 cold / 8 warm iters (15.2x / 18.1x reduction).
    Correctness: resid_p1 8.276e-11 (<1e-8); agree_p1_baseline 3.005e-08 (<1e-6,
      same vector as unpreconditioned); G4 1.705e-12 (<1e-7) in EXACTLY 1 iter
      (analytic 697.674 = 3/(1-beta)).
    Cheap apply: first-solve 60ms vs median 50ms (1.2x, not 50x) -> no hidden LU.
    Projection: 2.812 hr/NPL-iter (vs 49.2 unpreconditioned) -> 17.5x.
  GATE RULING (architect): P1 PASS — aging-backbone (block-triangular) precond is
    the production solve for 024b. Direct LU dead (024s), C++ matvec at memory
    floor (024c), preconditioner is the lever (024d). 024b is now UNBLOCKED.
  CARRY-FORWARD into the 024b spec (do NOT lose):
    (1) FORWARD-SOLVE projection (2.8 hr/NPL-iter) counts ONE solve per optim eval.
        The likelihood GRADIENT adds solves. 024b MUST use the ADJOINT/costate
        method: one extra solve with M^T per eval gives ALL ~9 gradient components
        (NOT 9 separate dV/dtheta solves, NOT 18x numerical). Reuse the SAME P1
        preconditioner (M^T is also triangular under the reverse order) and warm-
        start. Keeps per-eval at ~2 solves.
    (2) Group-by-m matvec batching (commutation proven; ~3.4x, oracle-guarded)
        is still on the table — folding it in drops sec_p1_warm ~5.9s -> ~1.7s,
        projection ~0.9 hr/NPL-iter. Optional accelerator for 024b, not required.
    (3) Build the 4 T_G once per (NPL-iter, env); the permutation `ord` and
        `A_perm` are state-space-only -> compute ONCE for the whole estimation.
    (4) NPL: lambda=1 (flat MNL) FIRST, then free lambda (per 022 staging).
