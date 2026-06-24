# TICKET 024p — Basis-build speedup: batched matvec + PSOCK env-parallelism
# Created: 2026-06-22
# Status: READY
# Attempt: 0
# Type: PERFORMANCE. Two ORTHOGONAL, MULTIPLICATIVE speedups to the PM08 v4
#   basis build (the ~23-min bottleneck that dominates every estimation pass AND,
#   later, every CF equilibrium solve). NO change to the model, the math, or the
#   estimates — correctness is gated by the existing 1e-12 matvec oracle + GATES B/C.

═══════════════════════════════════════════════════
CONTEXT
═══════════════════════════════════════════════════
PM08_Estimator_v4.R per-pass cost = the BASIS BUILD: for each of 17 environments
it solves (1 + n_params) preconditioned-BiCGSTAB systems (I - beta M^e)x = rhs over
the 298,448-state space (~76 s/env, ~1381 s/pass cold). Phase A/B/C are one-time;
inner optim + CCP update are cheap. So the basis IS the run cost; speeding it speeds
everything (FE-on, FE-off, all CFs). Two orthogonal levers, do A then B (B's per-
worker matvec benefits from A):
  A batched matvec  ~3.5-4x (single-thread, C++)
  B PSOCK env-parallel basis build ~5-6x (8 cores)
  A x B ~ 20x:  basis ~23 min -> ~1-1.5 min;  full converged run ~2 hr -> ~15-25 min.

MACHINE SCOPE (measured 2026-06-22; BOTH this box AND the server run Windows ->
PSOCK everywhere, NO fork/mclapply):
  AMD Ryzen 9 6900HS: 8 PHYSICAL cores / 16 logical; 27.7 GB RAM.
  Basis solves are memory-bandwidth-bound -> use PHYSICAL cores (8), not logical.
  Per-worker footprint ~1.5 GB (shared static + one env's P / basis / C++ op).
  Budget: <= 8 workers x ~1.5 GB ~ 12 GB (fits 27.7 GB with other apps closed).
  17 envs / 8 workers -> max ceil(17/8)=3 envs on the busiest worker -> ~5-6x
  (load-bound, not 8x). Make worker count a parameter; default min(8, detectCores-ish).

Run via standalone R-4.5.2 x64 (024c protocol). Build/validate in SMALL files,
one per response (per the 024b build-protocol lesson; do NOT one-shot).

═══════════════════════════════════════════════════
LEVER A — BATCHED MATVEC  (C++ pm_op_mv; ~3.5-4x; DO FIRST)
═══════════════════════════════════════════════════
WHY: pm_op_mv currently does ONE sparse product (A_age %*% dense) PER WORK ACTION
(21 of them). But A_age acts on the COMPOSITION index while each action's Gmat (GaT)
acts on the CAPACITY index, and row-ops commute with the column (G) op (proven in
024c). So the expensive sparse product depends only on the INSTALL count m (the
install-gather imap[,m+1]) -> compute it ONCE per distinct m (<=5), not per action.

ALGEBRA (out_a for action a=(k,m); Vmat is C x N_G):
  original:  out_a = RemoveGather_k( A_age %*% InstallGather_m( Vmat %*% GaT_a ) )
  commute:   InstallGather_m and A_age (row ops) commute with %*% GaT_a (col op):
  batched:   Zbase_m = A_age %*% InstallGather_m(Vmat)        # 5 sparse products total
             out_a   = RemoveGather_k( Zbase_m %*% GaT_a )    # dense C x N_G * 4x4 + a row gather
             result += P_a (elementwise) out_a

IMPLEMENTATION (Code/Helpers/pm_matvec.cpp — append a NEW op/mv; keep the old
  pm_op_build/pm_op_mv intact as the within-file oracle until A validates):
  - In the op build: GROUP the actions by their m value. For each m-group store:
      g_in[m]  = the install gather (imap0[,m+1], 0-based, -1=NA) — shared in the group,
      and the per-action list { g_out (rmap0[,k+1]), GaT_a, P_a }.
  - In pm_op_mv (batched): for each m-group -> gather Ym = InstallGather(Vmat) by g_in
      (NA->0), Zbase = A_age * Ym (the ONE sparse product); then per action in the
      group -> Wa = Zbase * GaT_a (dense), out = RemoveGather_k(Wa) (NA->0),
      result += P_a % out. Same ARMA_NO_DEBUG / ARMA_64BIT_WORD flags as now.
VALIDATE A (the gate): batched pm_op_mv == original pm_op_mv (and == R Mv oracle in
  pm_bellman_kernel.R) to < 1e-12 on 5 random V and the G5 test P. (Reuse PM07's
  oracle harness.) THEN benchmark: time batched vs original Mv (expect ~3.5-4x) and
  re-run GATE B in PM08 (basis V == direct solve still 1e-9). Switch PM08's A_x to
  the batched op only after the 1e-12 gate passes.

═══════════════════════════════════════════════════
LEVER B — PSOCK ENV-PARALLEL BASIS BUILD  (~5-6x on 8 cores; DO SECOND)
═══════════════════════════════════════════════════
WHAT: the per-env loop in PM08's NPL section (build_basis_env + build_coeffs_env for
  e in 1:n_env) is EMBARRASSINGLY PARALLEL — the envs are independent. Parallelize
  ONLY that loop. Inner optim + CCP update stay in the parent (cheap, serial).
DESIGN (parallel::makeCluster type="PSOCK"):
  - Create the cluster ONCE (cl <- makeCluster(n_workers, type="PSOCK")); n_workers
    = min(getOption,8) cap, env-overridable PM08_NWORKERS. stopCluster on exit.
  - WORKER INIT (clusterEvalQ): each worker loads its OWN copy of the engine —
      library(Matrix); library(data.table); source(pm_bellman_kernel.R);
      Rcpp::sourceCpp(pm_matvec.cpp). CRITICAL: the C++ op is an XPtr (external
      pointer) and CANNOT be serialized across processes -> each worker must
      sourceCpp + call build_cpp_op ITSELF for its env(s). Do NOT export cpp ops.
  - EXPORT (clusterExport, once for statics + per-iter for the moving parts):
      static (once): kernel, ss, lk, beta, C, N_G, N_SIDX, n_act, k_vec, m_vec,
        act_keys, GaT_list, imap0, rmap0, ord, A_perm, uplo_flag, prem_ej, haz_j,
        feas_mask, env_tbl, param_struct/param_alpha/param_names, the build_*/Minv/
        bicgstab_pc functions, build_cpp_op.
      per outer iter: P_envs (or per-worker subset), bases_warm (warm starts).
  - DISPATCH: split 1:n_env into worker chunks; res <- parLapply(cl, chunks,
      function(es) lapply(es, function(e) list(basis=build_basis_env(e, P_envs[[e]],
      warm...), coeffs=build_coeffs_env(e, ...)))); flatten into bases_all/coeffs_all
      in the parent. Keep deterministic env->result ordering.
VALIDATE B: parallel bases_all/coeffs_all == the SERIAL build to < 1e-8 (per-vector
  max-abs) for a small env set; GATE B/C still pass; the full fit (theta_hat) from a
  parallel run matches a serial run to optimizer tol. Report wall-time vs serial.
MEMORY GUARD: print per-worker RSS or cap n_workers so n_workers x ~1.5GB < free RAM;
  if RAM is tight, fall back to fewer workers (do NOT OOM). 17 envs are not divisible
  by 8 -> accept the ceil(17/8)=3 load bottleneck (or balance: 6 workers x 3).

═══════════════════════════════════════════════════
ACCEPTANCE CRITERIA (reviewer)
═══════════════════════════════════════════════════
- [ ] A: batched pm_op_mv == original / R-Mv oracle to < 1e-12 (5 random V); Mv
      benchmark shows ~3.5-4x; PM08 GATE B still 1e-9; old op kept until A passes.
- [ ] B: parallel basis == serial basis < 1e-8; GATES B/C pass; parallel fit ==
      serial fit to optim tol; PSOCK (NOT fork); each worker sourceCpp's its own op;
      cluster stopped on exit; memory-guarded (no OOM at <=8 workers).
- [ ] Combined wall-time reported (basis sec serial -> batched -> batched+parallel)
      with the worker count and per-worker RAM.
- [ ] No model/math/estimate change: a full converged fit is numerically identical
      (to optim tol) to the pre-speedup fit. Standalone R-4.5.2 x64; runs exit 0;
      ONE monitor max; small-files build protocol.

═══════════════════════════════════════════════════
SEQUENCING / GATE
═══════════════════════════════════════════════════
A then B (B's per-worker matvec rides on A). After A: ~6 min basis. After B:
~1-1.5 min basis. Then re-time a full pass and update the week's run plan. This
ticket touches ONLY pm_matvec.cpp (append) + the PM08 basis-build section
(parallel wrapper) — NOT the kernel, the model, the profiling, or the estimates.

═══════════════════════════════════════════════════
ATTEMPT LOG
═══════════════════════════════════════════════════
[reviewer fills]
