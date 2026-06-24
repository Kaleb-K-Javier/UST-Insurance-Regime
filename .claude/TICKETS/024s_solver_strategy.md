# TICKET 024s — Portfolio model: production value-solve strategy (bake-off)
# Created: 2026-06-18
# Status: READY (follows 024a PASS-PORT; NPL_REFERENCE eq 2.3.25 in context)
# Attempt: 0
# Type: NUMERICAL BAKE-OFF. Pick the solve method 024b (the estimator) will use,
#       by MEASURING two candidates against the validated 024a kernel oracle.
#       No flow-utility spec, no NPL loop. Produces a RECOMMENDATION + numbers.

═══════════════════════════════════════════════════
START HERE
═══════════════════════════════════════════════════
024a built and PROVED a correct value-solve kernel (Code/Helpers/
pm_bellman_kernel.R): gates G1-G5 pass, the structured matvec matches an
explicit M@V at 1e-16 (machine precision), M is never formed (peak 94 MB). The
ONLY problem was speed: its Anderson fixed-point solve took 640s cold / 303s
warm per env (530-764 iters) because beta=0.9957 makes the contraction crawl.
This ticket does NOT touch the kernel's correctness pieces — it REUSES
pm_bellman_kernel.R's Mv() and comp_apply() as-is, and uses its Anderson
solve_V() ONLY as a correctness oracle. It implements and BENCHMARKS two faster
solve strategies, then recommends one for 024b. Do NOT re-ask 024a's resolved
questions (they're in 024a's RESOLVED Q&A). Run via
C:/Program Files/R/R-4.5.2/bin/x64/Rscript.exe.

═══════════════════════════════════════════════════
WHAT WE ARE CHOOSING, AND WHY TWO CANDIDATES
═══════════════════════════════════════════════════
The estimator solves (I - beta M) V = R many times: 17 environments, inside an
NPL loop whose inner optim calls the likelihood ~10^2 times. CRUCIAL STRUCTURE:
within the inner optim, P is FIXED, so M is FIXED — only R changes. So the ideal
method amortizes a one-time per-(NPL-iter, env) cost over many R solves.

S1 — DIRECT SPARSE FACTORIZATION (primary candidate; lowest risk):
  Form M explicitly (sparse), factorize A = (I - beta M) ONCE per (NPL-iter, env)
  with a built-in sparse LU, then each R is a cheap back-substitution. Robust
  (no hand-coded iteration), exact, amortizes perfectly. RISK: forming/
  factorizing a 298,448-square sparse matrix — memory + fill-in. 024a showed
  memory headroom, so this is now plausible; MEASURE it.

S2 — KRYLOV ON THE STRUCTURED MATVEC (fallback if S1 infeasible):
  BiCGSTAB on the linear operator A(x) = x - beta * Mv(x), reusing the kernel's
  memory-light Mv (never forms M). (I - beta M) is a diagonally-dominant
  M-matrix, cond ~ 1/(1-beta) ~ 233 -> Krylov expected ~20-50 iters (vs Anderson
  530+). RISK: per-iter matvec cost (0.42s in R) may need a C++ port to hit the
  target; measure the iteration count first to decide.

DECISION RULE (pre-committed): if S1 forms+factorizes in < ~120s and < ~24 GB
  with back-sub < ~2s, RECOMMEND S1 (simplest, best amortization, no C++).
  Else RECOMMEND S2, and state whether its R matvec is fast enough or 024b must
  port Mv to C++ (threshold: Krylov cold solve > ~10s -> needs C++).

═══════════════════════════════════════════════════
INPUTS / REUSE
═══════════════════════════════════════════════════
- Code/Helpers/pm_bellman_kernel.R: source it; REUSE build_kernel(), comp_apply(),
  Mv(), feasible_actions(), netbin(), and solve_V() (Anderson) as the ORACLE.
  Do NOT modify this file.
- Output/Estimation_Results/PM_StateSpace.rds, PM_Lookups.rds (as in 024a).
- The test (P, R) for one environment: build the SAME Hotz-Miller P (from
  pm_agg_counts, smoothed) and the SAME placeholder R that 024a's G5 used
  (env = RB-2006). Reuse 024a's G5 construction verbatim so the oracle V matches.

═══════════════════════════════════════════════════
DELIVERABLE
═══════════════════════════════════════════════════
Code/Dynamic_Model/PM05_Solver_Bakeoff.R (sources the kernel; builds M for S1,
  BiCGSTAB for S2; validates both vs the oracle; benchmarks; prints the
  recommendation). Saves Output/Estimation_Results/PM_Solver_Bakeoff.rds.

═══════════════════════════════════════════════════
PSEUDOCODE — PM05
═══════════════════════════════════════════════════
0. source kernel; load 023 objects; kernel <- build_kernel(ss, lk);
   beta <- lk$BETA. Build the G5 test (P, R) for env RB-2006 (reuse 024a G5).
   NO 640s ANDERSON ORACLE. Validation is RESIDUAL + ASSEMBLY based (the
   kernel's structured Mv, validated at 1e-16 in 024a G3, is the assembly
   reference; the residual ||A V - R|| is the solve check; G4 analytic is the
   known-answer anchor). Optionally cache an Anderson V as an extra cross-check
   (PM05_V_oracle.rds) but it is NOT required and NOT the validator.
   Define A(x) operator: A_x(x) = x - beta * as.vector(Mv(kernel, P, matrix(x, nrow=C))).

1. S1 — FORM M EXPLICITLY (one env), measure, factorize, back-sub:
   1a. For each WORK action a=(k,m) feasible at >=1 comp:
        CompKer_a = A_age ROW-gathered by rmap[,k+1] and COLUMN-relabeled by
          imap[,m+1] (build as a sparse C x C dgCMatrix; the column relabel is a
          permutation since imap is injective where defined; infeasible rows ->
          empty). F_a = kronecker(Gmat[[netbin(k,m)]], CompKer_a)   # sidx=(G-1)*C+c
          => F_a is (4C) x (4C) = N_SIDX^2, sparse.
        Weight: M_a = Diag(as.vector(P[[a]])) %*% F_a   # P[[a]] is C x 4 -> length-4C
        (as.vector column-major matches sidx).
       M = sum_a M_a   (sparse accumulate).
   1b. REPORT: nnz(M), object.size(M) GB, max row sum (must be <= 1: sub-stochastic).
       A = Diagonal(N_SIDX) - beta * M.
   1c. FACTORIZE once: fac <- Matrix::lu(A)  (sparse LU). MEASURE time + memory
       (gc peak). If it errors/OOMs, catch -> mark S1 INFEASIBLE with the reason
       (do NOT hard-fail the script; S2 still runs).
   1d. SOLVE: V_s1 <- solve(fac, R)  (back-sub). MEASURE back-sub time. Then
       MEASURE multi-RHS amortization: re-solve for 10 perturbed R's
       (R_i = R*(1 + 0.01*rnorm(N)), 1% RELATIVE) reusing fac; report mean
       back-sub sec (this is the per-optim-eval cost in 024b).
   1e. VALIDATE S1 (no Anderson oracle):
       (i) ASSEMBLY: max| M %*% v - as.vector(Mv(kernel,P,matrix(v,nrow=C))) |
           < 1e-10 for 3 random v -> the explicit kron-assembled M equals the
           024a-validated structured matvec (catches full-M assembly bugs).
       (ii) RESIDUAL: ||A %*% V_s1 - R||_inf / ||R||_inf < 1e-9 (LU exact).
       (iii) G4 analytic: M_g4 = kronecker(Gmat[["nowork"]], A_age); since
           Gmat[["nowork"]] = I_4, M_g4 is block-diagonal (4x A_age, ~7.8M nnz,
           cheaper than the RB-2006 M). Factorize A_g4 = I - beta*M_g4; solve for
           R = r*1 (r=3); assert max|V - r/(1-beta)| < 1e-7. (Skip if S1 was
           marked infeasible at 1c; note it.)

2. S2 — BiCGSTAB on the structured matvec (always run, for comparison):
   Operator: Ax(x) = x - beta * as.vector(Mv(kernel, P, matrix(x, nrow=C)))
   Implement standard BiCGSTAB (provided below) on Ax, b = R, tol 1e-9,
   max_it 2000, x0 = 0 (cold) and x0 = V_oracle-perturbed (warm).
   2a. VALIDATE S2: (i) RESIDUAL ||A_x(V_s2) - R||_inf/||R||_inf < 1e-8 (one Mv);
       (ii) AGREEMENT with S1 (if S1 feasible): max|V_s2 - V_s1| < 1e-6;
       (iii) G4 analytic via BiCGSTAB COLD (x0=0): max|V - r/(1-beta)| < 1e-7.
   2b. MEASURE: iters + wall sec, COLD (x0=0) and WARM (x0 = V_cold*(1+0.01*
       rnorm(N)), 1% RELATIVE perturbation; G4 check stays COLD).
   BiCGSTAB reference (do NOT improvise; standard van der Vorst 1992):
     r = b - Ax(x); rhat = r; rho=alpha=w=1; v=p=0
     repeat: rho1 = <rhat,r>; bta=(rho1/rho)*(alpha/w); p = r + bta*(p - w*v)
       v = Ax(p); alpha = rho1/<rhat,v>; s = r - alpha*v
       if ||s||<tol: x = x + alpha*p; break
       t = Ax(s); w = <t,s>/<t,t>; x = x + alpha*p + w*s; r = s - w*t
       rho = rho1; if ||r||<tol break
     (guard rho/w/<t,t> near-zero -> restart with rhat=r; HARD ERROR if it
     truly stalls, no NULL.)

3. BENCHMARK TABLE + RECOMMENDATION (printed + saved):
   columns: method, formed_M_nnz, form_sec, factorize_sec, factor_mem_gb,
     backsub_sec_mean (S1) | krylov_iters_cold/warm, krylov_sec_cold/warm (S2),
     max_dev_vs_oracle. Apply the DECISION RULE; print the explicit 024b
     recommendation (S1 vs S2, and S2's C++-needed verdict). Project a per-NPL-
     iter cost for the winner at 17 envs (factor-once + ~100 back-subs for S1;
     warm Krylov x ~100 evals for S2).

═══════════════════════════════════════════════════
R-NOTES
═══════════════════════════════════════════════════
- Matrix::kronecker / Matrix::Diagonal / sparse %*% for M assembly; keep
  everything dgCMatrix. as.vector(P[[a]]) is column-major (G-blocks) = sidx order.
- Matrix::lu on a 298k dgCMatrix uses CSparse LU. To reuse the factorization for
  multiple RHS, keep the lu() result and call solve(fac, R_i); confirm it does
  NOT re-factorize each call (time the 1st vs subsequent solves to verify).
  If Matrix re-factorizes, report it and the per-solve cost anyway.
- Wrap S1 factorize in a memory/error guard (the OOM is informative, not a
  failure): on error, record S1_infeasible=TRUE + message, continue to S2.
- No tryCatch returning NULL for the SOLVE results themselves; the only allowed
  catch is the S1 feasibility guard (which records a reason, not NULL silently).
- Hardcoded-name sink to logs/PM05_*.log. cat section markers.
- DO NOT spawn multiple background monitors on the run (024a's coder died of
  context exhaustion that way). Run PM05 in the foreground or ONE background
  handle; read the log file when done.

═══════════════════════════════════════════════════
ACCEPTANCE CRITERIA (reviewer)
═══════════════════════════════════════════════════
- [ ] pm_bellman_kernel.R UNMODIFIED (reused as oracle); no edits to 024a files
- [ ] S1: M formed with max row sum <= 1 (sub-stochastic); nnz + memory reported;
      factorize timed (or S1_infeasible recorded with reason, not a crash);
      validated by ASSEMBLY (M vs kernel Mv <1e-10) + RESIDUAL (<1e-9) + G4 (<1e-7)
- [ ] S2: BiCGSTAB validated by RESIDUAL (<1e-8) + S1-agreement (<1e-6 if S1 ok)
      + G4 cold (<1e-7); iters + cold/warm time reported
- [ ] NO dependence on a 640s Anderson oracle run (residual/assembly are the
      validators); 1% RELATIVE perturbations for warm/amortization tests
- [ ] PM_Solver_Bakeoff.rds saved with the benchmark table
- [ ] explicit 024b RECOMMENDATION printed per the DECISION RULE (S1 vs S2;
      C++-needed verdict for S2; projected per-NPL-iter cost at 17 envs)
- [ ] beta from lk$BETA; gamma_E/sigma literals; runs exit 0; ONE monitor max

═══════════════════════════════════════════════════
GATE DECISION (researcher/architect reads the table, then rules)
═══════════════════════════════════════════════════
-> Pick S1 or S2 for 024b. If S1: 024b factor-once per NPL-iter, back-sub per
   eval (no C++). If S2: 024b uses BiCGSTAB + (per the verdict) a C++ Mv port.
   Either way, 024b is written only after this recommendation is on record.

═══════════════════════════════════════════════════
ATTEMPT LOG
═══════════════════════════════════════════════════
[reviewer fills]

ATTEMPT 1 — 2026-06-18 — REVIEW (Opus) — VERDICT: PASS; decision = S2 + C++ port.
Verified from PM_Solver_Bakeoff.rds:
  S1 (direct sparse LU): INFEASIBLE. M = 131.3M nnz / 1.58 GB (max row sum 1.000,
    sub-stochastic OK), form 134s, but LU factorize OOM after 111s (fill-in blew
    past ~8 GB). The 298k fill-in risk I flagged is REAL — direct factorization
    is out, permanently. Good that we measured.
  S2 (BiCGSTAB on structured matvec): CORRECT — residual 8.46e-11, G4 6.82e-12
    (exact), cold 152 iters/136s, warm 145 iters/124s. Recommendation: S2.
  Projected R-only: 58.5 hr per NPL-iter (17 env x 100 evals x 124s) -> the C++
    matvec port is REQUIRED before the estimator.
RULING (architect): production solve = preconditioned BiCGSTAB on the structured
  matvec; direct factorization abandoned. NEXT TICKET (024c "fast solve", needs
  gate-5 approval for cpp_engine append):
  (1) Port Mv to C++ (append cpp_engine.cpp, full protocol); validate C++ Mv ==
      R Mv to ~1e-12 (the R kernel + 024a G3 are the oracle); 0.9s -> target
      ~15-30ms.
  (2) Add a PRECONDITIONER to cut the 152 iters: ILU(0) of (I-beta M) is the
      strong candidate — memory-BOUNDED (no full-LU fill-in blowup; M is 1.58GB,
      ILU(0) is ~same sparsity), and AMORTIZABLE (form once per (NPL-iter, env)
      since M is fixed in the inner optim, reuse for all evals). Block-Jacobi is
      the simpler fallback. Target: cut 152 -> ~20-40 iters.
  (3) Re-benchmark to target per-env solve < ~3s cold (and < ~1s with precond).
  THEN 024b (estimator). NOTE for 024b design: with ~17 env x (optim evals) x
  (NPL iters), the inner EVAL COUNT dominates -> 024b must use an efficient
  gradient (analytic NPL score, not 18x numerical) and amortize the precond
  per NPL-iter. Flag these in the 024b spec.
