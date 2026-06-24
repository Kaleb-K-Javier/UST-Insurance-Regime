# TICKET 024c — Portfolio model: C++ structured matvec + re-benchmark
# Created: 2026-06-18
# Status: READY (follows 024s: BiCGSTAB chosen, direct LU dead; C++ port required)
# Attempt: 0
# Type: C++ NUMERICAL PORT + VALIDATION + RE-BENCHMARK. Single focused job: make
#       the structured matvec fast in C++, prove it equals the R matvec, and
#       re-benchmark BiCGSTAB to confirm a feasible per-solve time. NO estimator,
#       NO preconditioner, NO NPL loop (those are 024b).

═══════════════════════════════════════════════════
START HERE
═══════════════════════════════════════════════════
024s decided the production solve = BiCGSTAB on the structured matvec (direct
sparse LU OOMs at 298k — dead). BiCGSTAB is CORRECT (residual 8.5e-11) at ~152
iters, but the R matvec (~0.9s) makes a solve 136s -> 58.5 hr/NPL-iter,
infeasible. This ticket ports ONLY the matvec to C++ (target ~15-30ms),
validates it against the R matvec (the oracle), and re-benchmarks. The R kernel
Code/Helpers/pm_bellman_kernel.R and its Mv() are the CORRECTNESS ORACLE — do
NOT modify them.

═══════════════════════════════════════════════════
C++ BUILD PROTOCOL — READ FIRST (we have repeatedly lost time to Rcpp/Rtools)
═══════════════════════════════════════════════════
These rules are NON-NEGOTIABLE and exist because every prior C++ attempt hit an
environment wall. Encode them; do not improvise.

E1 INTERPRETER: run EVERYTHING via the STANDALONE R, never miniconda R:
     C:/Program Files/R/R-4.5.2/bin/x64/Rscript.exe
   Miniconda R on the default PATH has a DLL-loading conflict with Rcpp (it
   loads the wrong libstdc++/Rlapack and sourceCpp fails or crashes). Confirmed
   in 023/024a. The standalone R finds Rtools via C:/Users/kaleb/.Renviron.

E2 TOOLCHAIN: Rtools45. Before ANY compile, assert the toolchain is visible:
     stopifnot(nzchar(Sys.which("make")), nzchar(Sys.which("g++")))
   If this fails: STOP, fix the PATH/.Renviron, re-run. NEVER fall back to an
   R-only path "to keep going" — a silent fallback is how we shipped slow code
   before. The whole point of this ticket is the C++ path working.

E3 ISOLATION: put the new C++ in a SEPARATE standalone file
     Code/Helpers/pm_matvec.cpp
   sourced via Rcpp::sourceCpp(). Do NOT touch Code/Helpers/cpp_engine.cpp
   (large, fragile, gate-5 protected) and do NOT touch improved_estimator_
   OPTIMIZED.r (it has a historical eager-sourceCpp at ~line 32 that halts file
   sourcing — see memory feedback_rcpp_eager_sourcecpp_at_line32). A fresh
   isolated .cpp compiles independently and cannot break the existing engine.

E4 LIBS: use Rcpp + RcppArmadillo (// [[Rcpp::depends(RcppArmadillo)]]) for
   arma::sp_mat (sparse A_age) and arma::mat (dense C x 4). Pass A_age to C++
   ONCE as a dgCMatrix -> arma::sp_mat (Rcpp::as), not rebuilt per call.

E5 BUILD-PREFLIGHT GATE (a tiny script that MUST print PASS before the real
   work): assert E1/E2, sourceCpp(pm_matvec.cpp), call a trivial exported probe
   (e.g. add two arma vectors), print compile seconds + result. If it fails,
   this ticket's job becomes fixing the environment, nothing else proceeds.
   Save logs/PM_cpp_preflight_*.log.

═══════════════════════════════════════════════════
THE C++ MATVEC (port the VALIDATED R algorithm exactly)
═══════════════════════════════════════════════════
Reproduce pm_bellman_kernel.R's Mv() in C++ — SAME algorithm, no new math:
  Mv(P, Vmat) returns C x 4 = sum over WORK actions a=(k,m) of
     Diag_c(P_a[,G]) (elementwise over C) applied to comp_apply(k,m, Vmat %*% t(Gmat_a))
  comp_apply(k,m, X):  # X is C x 4
     1. INSTALL input gather: Y = X[imap[,m+1], ] with NA imap rows -> 0
     2. AGE: Z = A_age %*% Y            (arma sp_mat * dense)
     3. REMOVE output gather: out = Z[rmap[,k+1], ] with NA rmap rows -> 0
  (D1/D2 conventions from 024a are FIXED; just port them. The 0-on-NA guards are
  load-bearing — 0*NA=NA in R/C++ alike; zero those rows explicitly.)
INPUTS passed from R to the C++ function (precompute once, pass as args):
  A_age (sp_mat), the per-action index vectors rmap[,k+1] & imap[,m+1] (1-based ->
  convert to 0-based in C++), the feasible (k,m) action list, Gmat_a (4x4) per
  action's netbin, and P as a list/array of C x 4 per action. Keep all indexing
  0-based inside C++, 1-based at the R boundary.

VALIDATION (the C++ Mv must equal the R Mv — the oracle):
  For 5 random Vmat and the G5 test P (RB-2006): assert
     max| cpp_Mv(P,V) - R_Mv(kernel,P,V) | < 1e-12.
  This is the gate: the C++ path is correct iff it matches the 024a-validated R
  matvec to machine precision. (Do not re-derive correctness from scratch; the R
  Mv already passed 024a G3 at 1e-16.)

═══════════════════════════════════════════════════
RE-BENCHMARK (the gate number for 024b)
═══════════════════════════════════════════════════
Wire the C++ Mv into the SAME BiCGSTAB from 024s (operator A_x(x) = x -
beta*cpp_Mv(...)). Re-solve env RB-2006 and one FF env (OH):
  - time one C++ Mv (median of 5); expect ~15-30ms (vs 0.9s R).
  - BiCGSTAB cold (x0=0) and warm (x0 = V*(1+0.01*rnorm), 1% relative): iters +
    wall sec. Residual < 1e-8; G4 analytic cold < 1e-7 (reuse 024s checks).
  - project per-NPL-iter cost at 17 env (warm-solve sec x 17 x 100 evals).
  Save Output/Estimation_Results/PM_Cpp_Benchmark.rds (mv_ms, bicgstab_iters_
  cold/warm, sec_cold/warm, resid, g4_dev, proj_hr_per_npl_iter).
TARGET: per-env cold solve < ~5s (i.e. C++ Mv ~20ms x ~150 iters ~3s). If hit,
  024b is feasible with this solve (and 024b will add a preconditioner +
  efficient gradient to cut further). If C++ Mv is not << R Mv, debug the C++
  (likely an unintended copy or per-call rebuild) before declaring done.

═══════════════════════════════════════════════════
ACCEPTANCE CRITERIA (reviewer)
═══════════════════════════════════════════════════
- [ ] BUILD-PREFLIGHT PASS logged: standalone R-4.5.2 x64, make+g++ found,
      pm_matvec.cpp compiled, probe ran. NO miniconda R, NO R-fallback language.
- [ ] New C++ lives in Code/Helpers/pm_matvec.cpp (standalone); cpp_engine.cpp
      and improved_estimator_OPTIMIZED.r UNTOUCHED; pm_bellman_kernel.R UNTOUCHED
- [ ] C++ Mv == R Mv to < 1e-12 on 5 random V (machine-precision match = correct)
- [ ] BiCGSTAB with C++ Mv: residual < 1e-8, G4 cold < 1e-7, iters+time reported
      cold & warm; PM_Cpp_Benchmark.rds saved
- [ ] C++ Mv median time reported and is << R's 0.9s (target ~15-30ms); per-NPL-
      iter projection printed
- [ ] runs exit 0; ONE monitor max on any long run (024a died of 5 monitors)

═══════════════════════════════════════════════════
GATE DECISION (architect reads the benchmark)
═══════════════════════════════════════════════════
Only the matvec is ported here; BiCGSTAB itself STAYS IN R (its per-iter vector
ops on length-298k are cheap vs the matvec). Two separate downstream levers, do
NOT conflate them:
  - SOLVER speed (per-solve sec): if the C++ benchmark still misses the per-NPL-
    iter target, a focused PRECONDITIONER step (ILU(0)/block-Jacobi, amortized
    per NPL-iter since M is fixed in the inner optim) precedes 024b. If the
    target is hit, skip it.
  - ESTIMATOR eval count (024b): the inner optim's number of likelihood calls is
    a SEPARATE lever — 024b must use an analytic NPL-score gradient, not 18x
    numerical for ~9 params. (See memory project-t024-bellman-kernel.)
PASS (C++ Mv fast + correct) -> proceed per the above.
FAIL (C++ Mv not << R, or build not robust) -> escalate; do NOT build 024b on a
  slow/fragile solve.

═══════════════════════════════════════════════════
ATTEMPT LOG
═══════════════════════════════════════════════════
[reviewer fills]
