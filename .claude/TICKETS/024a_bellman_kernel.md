# TICKET 024a — Portfolio model: Bellman value-solve kernel + feasibility gate
# Created: 2026-06-13
# Status: READY (architect spec; NPL_REFERENCE eq 2.3.25 in context; flat 4p
#         machinery at improved_estimator_OPTIMIZED.r:2643 matched)
# Attempt: 0
# Type: NUMERICAL KERNEL + VALIDATION + BENCHMARK. No estimation, no flow-utility
#       spec (024b), no NPL loop. Builds and PROVES the one primitive everything
#       downstream calls: solve_V(R, P, environment) at 298,448 states.
#       This is a HARD GATE: if the solve is not tractable, 024b does not proceed
#       until the fallback (named below) is chosen from the measured numbers.

═══════════════════════════════════════════════════
START HERE — IMPLEMENTATION-READY (a prior session stalled on a timeout)
═══════════════════════════════════════════════════
ALL R-implementation questions are ALREADY RESOLVED in the "RESOLVED Q&A"
section near the bottom of this spec. Do NOT re-ask them. Confirm you have read
the spec (including RESOLVED Q&A), then write code directly.
FIRST ACTION: a previous session crashed mid-work. Check for half-written
  Code/Helpers/pm_bellman_kernel.R and Code/Dynamic_Model/PM04_Kernel_Validate_
  Benchmark.R. If either exists, do NOT trust it — rebuild from this spec.
Run R via C:/Program Files/R/R-4.5.2/bin/x64/Rscript.exe (NOT miniconda R).

═══════════════════════════════════════════════════
WHY THIS TICKET EXISTS (read first)
═══════════════════════════════════════════════════
The estimator solves V = (I - beta*M)^{-1} R (NPL_REFERENCE eq 2.3.25) at
N_SIDX = 298,448 operating states, ~17 contract environments, inside an NPL loop.
beta = 0.9957 (near 1). Forming M explicitly (~10^8 nonzeros) and factorizing it
is likely infeasible in memory. This ticket builds the value solve WITHOUT ever
forming M, by exploiting the transition's product structure, then MEASURES it so
024b is designed against real numbers, not hope. Nothing in 024b is written until
this passes its correctness gates and reports its benchmark.

═══════════════════════════════════════════════════
TWO ARCHITECT DECISIONS LOCKED HERE (do not deviate)
═══════════════════════════════════════════════════
D1 — TIMING CONVENTION (remove -> age survivors -> install):
  At BOY(t) the facility holds composition n. It chooses (k,m). The k removed
  tanks leave; the SURVIVING tanks age over year t; the m newly installed tanks
  are brand new (band 1) at BOY(t+1) and DO NOT age in their install year.
  Therefore the composition transition for action (k,m) is:
     n  --remove k (rule)-->  n_rk = rmap[c,k]
        --age survivors-->     distribution A_age[n_rk, .]
        --install m DW band1--> shift each aged outcome c'' to imap[c'', m]
  => 023's precomputed `post` array (which installs BEFORE aging) is NOT used by
     this kernel. We compose from rmap (remove) + A_age (age) + imap (install).
  Rationale: a tank installed in year t is ~1 yr old at BOY(t+1) = band 1; it
  must not jump an age band in its first year. (Supersedes the ambiguous
  "removal/install map, then age" wording in the 022 QMD A6; reconcile the QMD
  prose in a later polish pass — note only, not this ticket.)

D2 — NEVER FORM M (structured matvec). V is indexed sidx = (G-1)*C + c,
  C = 74,612, G in 1..4. Represent V as a C x 4 matrix Vmat[c, G]. Each action's
  transition factors as F_a[(c,G),(c',G')] = CompKer_a[c,c'] * Gmat_a[G,G'].
  Then the action's expected-continuation operator is the C x 4 result:
     (F_a V)mat  =  CompKer_a %*% ( Vmat %*% t(Gmat_a) )
  where CompKer_a is A_age with a ROW-GATHER by rmap[.,k] (remove) and a
  COLUMN-SCATTER by imap[.,m] (install). We reuse the SINGLE 023 A_age
  (C x C sparse, ~1.95M nnz) for every action via reindexing — no per-action
  big matrices, no 298k x 298k matrix ever formed.

  CONVENTION LOCK (verified from first principles two ways: direct index sum and
  vec(A X B) = (B^T kron A) vec(X); do NOT alter):
    * Vmat is C x 4 with Vmat[c,G] = V[(G-1)*C + c]  -> in R: Vmat <- matrix(V,
      nrow = C) (COLUMN-MAJOR; byrow=FALSE). This matches 023 sidx=(G-1)*C+c.
    * The transpose is on Gmat (t(Gmat_a)), NOT Vmat-side, because Gmat is stored
      row=from/col=to (023) and the continuation contracts over the TO-index G'.
      Writing Gmat instead of t(Gmat) is WRONG (Gmat is not symmetric).
    * Flatten back column-major: V_out <- as.vector(R_mat).
    * Identity used: with sidx=(G-1)*C+c, F_a = Gmat kron CompKer and
      V = vec(Vmat); (Gmat kron CompKer) vec(Vmat) = vec(CompKer Vmat t(Gmat)).
    * GATE G3 is the catch-net for any transpose/reshape slip here: it forms M
      EXPLICITLY on the N<=2 block and requires the structured matvec to match to
      1e-11. A t(Gmat)<->Gmat flip or a byrow reshape FAILS G3 immediately.

═══════════════════════════════════════════════════
THE MATH (cite NPL_REFERENCE; this is what the kernel implements)
═══════════════════════════════════════════════════
Actions: a in WORK = {(k,m) feasible} (includes maintain (0,0)); plus exit X.
Exit is ABSORBING: contributes 0 to M, no continuation (NPL_REFERENCE sec 4).

M (per environment, given CCP matrix P over states x actions):
   M = sum_{a in WORK} Diag(P_a) F_a            # exit excluded
   (M is SUB-stochastic: row sums = 1 - P_X(s) <= 1)

Value solve (eq 2.3.25):  (I - beta M) V = R
   R[s] = sum_{a in WORK} P_a(s) (U_a(s) + sigma(gamma_E - log P_a(s)))
        +            P_X(s) (U_X(s) + sigma(gamma_E - log P_X(s)))
   (024a uses a PLACEHOLDER R to exercise the solver; the real R = 024b.)

Matvec used by the solver (the D2 structured form), for a given P and V:
   (M V)mat = sum_{a in WORK} Diag_c(P_a[,G]) ⊙ [ CompKer_a %*% (Vmat %*% t(Gmat_a)) ]
   implemented column-by-column in G; P_a[,G] is the length-C CCP of action a at
   capacity bin G. (⊙ = elementwise over the C dimension.)

Solver: (I - beta M) V = R via fixed-point with Anderson acceleration:
   V_{n+1} = R + beta (M V_n)        # contraction, modulus <= beta = 0.9957
   Anderson(depth=5) on the residual g(V) = R + beta M V - V.
   Plain fixed point needs ~log(tol)/log(beta) ~ 4,000 iters; Anderson should cut
   to tens. MEASURE both the accelerated iter count and the per-matvec time.

═══════════════════════════════════════════════════
INPUTS (frozen 023 objects; read-only)
═══════════════════════════════════════════════════
Output/Estimation_Results/PM_StateSpace.rds:
  comp_mat [C x 16], C, A_age [C x C dgCMatrix], rmap [C x (K_BAR+1)],
  imap [C x (M_BAR+1)], N_vec, h_idx, P_RB_all [C x 3], MARG_WALL/BIN, N_G, N_SIDX.
Output/Estimation_Results/PM_Lookups.rds:
  BETA (=0.9957), Gmat (named list of 4x4 by netbin), h_aw, pbar, tau, D, SCALE.
  NOTE: gamma_E and sigma are NOT stored in the RDS. Define them as LITERAL
  named constants at the top of the kernel: gamma_E <- 0.5772156649, sigma <- 1.0
  (sigma=1 is the scale normalization; gamma_E is the Euler-Mascheroni constant).
  ONLY beta is read from the file (lk$BETA); never hardcode beta.
  (024b may add gamma_E/sigma to the lookups for tidiness; not required here.)
PRECOMPUTED in PM_StateSpace.rds (use directly, do not recompute):
  P_RB_all [C x 3, cols "2006"/"2014"/"2019"] = comp_mat %*% pbar (RB P_index);
  N_vec (FF P_index = tau[state]*N_vec); h_idx [C, 1..16] -> H(n)=h_aw[h_idx];
  N_BAR=6, K_BAR=4, M_BAR=4; MARG_WALL/MARG_BIN (cols 1-8 SW bins 8..1, cols
  9-16 DW bins 8..1 -> removal = greedy left-to-right across cols 1..16).
  pm_agg_counts is a SEPARATE file: Data/Analysis/pm_agg_counts.csv
  (cols sidx,g,era,action,n_obs) — for the G5 Hotz-Miller P only.

═══════════════════════════════════════════════════
DELIVERABLES
═══════════════════════════════════════════════════
(1) Code/Helpers/pm_bellman_kernel.R  — SOURCED helper, NO side effects on load
    (no reads, no prints at source time; just function definitions). Functions:
      build_kernel(ss, lk) -> kernel object (precomputes per-action rmap/imap
        index vectors, Gmat_a selection by netbin(k,m), feasible-action lists,
        A_age once).
      comp_apply(kernel, k, m, Xmat)  -> C x ncol(Xmat): applies CompKer_(k,m)
        to a C x ncol matrix = A_age row-gathered by rmap[,k], then column
        result scattered by imap[,m]. (Maintain: k=m=0 -> plain A_age %*% Xmat.)
      Mv(kernel, P, Vmat) -> C x 4: the structured matvec sum over WORK actions.
      solve_V(kernel, R, P, beta, tol=1e-9, max_it=10000, anderson=5)
        -> list(V [length N_SIDX], iters, resid, converged). Anderson-accelerated
        fixed point on the D2 matvec. Hard error (no NULL) if not converged.
      netbin(k,m) -> one of c("-3","-2","-1","0","1","nowork") (nowork iff k=m=0;
        else pmax(-3,pmin(1,m-k))).
      feasible_actions(N) -> data.table(k,m) per the 023 menu rule
        (0<=k<=min(N-1,K_BAR), 0<=m<=M_BAR, N-k+m<=N_BAR, not(k==0&m>0)).

(2) Code/Dynamic_Model/PM04_Kernel_Validate_Benchmark.R — test+benchmark harness
    (sources the kernel, runs all gates, prints the report, writes logs/PM04_*.log).
    Saves Output/Estimation_Results/PM_Kernel_Benchmark.rds (the measured numbers
    024b is designed against).

═══════════════════════════════════════════════════
PSEUDOCODE — PM04 harness
═══════════════════════════════════════════════════
0. source pm_bellman_kernel.R; readRDS the two 023 objects; beta <- lk$BETA;
   stopifnot(beta == 0.9957); kernel <- build_kernel(ss, lk).

1. CORRECTNESS GATES (every one a stopifnot; hard-fail with a diagnostic):

  G1 mass conservation. For each feasible action (k,m): comp_apply(kernel,k,m,
     ones(C,1)) must equal 1 for every comp c that HAS that action feasible
     (rows sum to 1 — aging + deterministic remove/install conserve probability).
     Gmat rows already sum to 1 (023). Assert max|rowsum - 1| < 1e-12 over a
     sample of all action types {(0,0),(1,0),(2,0),(1,1),(3,2),(0,0)...} present.

  G2 determinism vs 023. For 20 random comps c and k in 1..min(N-1,K_BAR):
     the remove step rmap[c,k] must equal the greedy oldest-SW-first removal
     recomputed independently here (hand recompute from comp_mat[c,]); and the
     install imap[c'',m] must add exactly m to the DW_band1 cell. Assert exact
     integer match.

  G3 structured == explicit, on a SMALL block. Restrict to comps with N<=2
     (a few hundred comps). Build the FULL M explicitly for ONE environment on
     that sub-block (form F_a as dense/sparse from CompKer⊗Gmat, sum P_a F_a),
     and compare Mv(structured) to (M_explicit %*% V) for 5 random V. Assert
     max abs diff < 1e-11. (This is the ground-truth check on the matvec.)

  G4 ANALYTIC closed form. Set P = maintain-only (P_(0,0)=1, all other actions
     incl exit = 0 -> M = F_maintain, row-STOCHASTIC). Set R = r * ones, r = 3.0.
     Then (I - beta M)^{-1} R = r/(1-beta) * ones exactly. Run solve_V and assert
     max|V - r/(1-beta)| < 1e-7. (Clean correctness test of the whole solver.)

  G5 realistic R, well-posed. Build a PLACEHOLDER R from a trial theta (clearly
     commented NOT the 024b flow utility): per state, a bounded u like
     u_work ~ phi0 + gp*P_index(n,env) - gr*H(n)*D - k*cR - m*cI with trial
     (phi0=1, gp=-2, gr=0.1, cR=cI=0.5) and a Hotz-Miller P from pm_agg_counts
     (smoothed). Run solve_V; assert converged, all(is.finite(V)),
     all(abs(V) < 1e6); and residual ||(I-beta M)V - R||_inf / ||R||_inf < 1e-7
     (verified via one independent Mv call).

2. BENCHMARK (the gate output — the numbers 024b is built against):
  - time one Mv call (C x 4), median of 5.
  - time one full solve_V to tol 1e-9 (env = RB-2006); report iters + wall sec.
  - repeat for an FF env (e.g. OH) to confirm env-independence of cost.
  - PROJECT per-NPL-iter cost: 17 envs * solve_sec * (assume 1 factor-free solve
    per likelihood eval) — but NOTE the 024b amortization: with M FIXED across
    the inner optim, only R changes, so a converged V can WARM-START the next
    eval. Report BOTH cold-solve sec and warm-start sec (re-solve after a small
    R perturbation, measuring iters from a warm V0).
  - peak memory (gc()/object.size of kernel + working set).
  - save PM_Kernel_Benchmark.rds: list(mv_sec, solve_sec_cold, solve_iters_cold,
    solve_sec_warm, solve_iters_warm, n_env=17, peak_mem_gb, beta, tol).

3. REPORT (printed, also in the log): a 6-line summary + an explicit
   RECOMMENDATION for 024b: (i) R-level kernel adequate vs needs C++ port of Mv
   (threshold: if cold solve > ~30s or warm > ~5s, recommend C++); (ii) Anderson
   adequate vs needs BiCGSTAB; (iii) projected hours per NPL iteration at 17 envs.

═══════════════════════════════════════════════════
R-IMPLEMENTATION NOTES
═══════════════════════════════════════════════════
- comp_apply EXACT RECIPE (verified from first principles; install and remove
  are on OPPOSITE sides of the aging step — do NOT swap them):
  (CompKer_(k,m) x)[c] = sum_{c''} A_age[rmap[c,k], c''] * x[imap[c'',m]]
  =>  comp_apply(k, m, X):                 # X is C x ncol (the W = Vmat %*% t(Gmat))
        # 1. INSTALL = INPUT-side gather on X (BEFORE A_age):
        if (m == 0) Y <- X
        else { gi <- imap[, m+1L]; Y <- X[gi, , drop=FALSE]; Y[is.na(gi), ] <- 0 }
        # 2. AGE survivors:
        Z <- A_age %*% Y                    # C x ncol
        # 3. REMOVE = OUTPUT-side gather on Z (AFTER A_age):
        if (k == 0) out <- Z
        else { go <- rmap[, k+1L]; out <- Z[go, , drop=FALSE]
               out[is.na(go), ] <- 0 }      # ZERO infeasible rows (see NA trap)
        out
  CRITICAL NA TRAP (Q2): in R, 0 * NA = NA, NOT 0. So infeasible (comp,action)
  rows MUST be zeroed inside comp_apply (both the imap input gather and the rmap
  output gather), NOT left as NA to be "zeroed" by P_a=0 in Mv (that yields NA
  and poisons the whole solve). After comp_apply, assert !anyNA(out). Mv then
  combines P_a[,G] (=0 at infeasible cells) with a 0-valued comp_apply row -> 0.
  Maintain (k=m=0) reduces to out = A_age %*% X (just aging). The install NA->0
  guard is harmless: comps c'' with imap NA have N(c'')+m > N_BAR and are never
  reached by A_age from a feasible post-removal comp (N = N(c)-k <= N_BAR-m), so
  they carry zero A_age mass into any feasible state. imap is injective where
  defined (imap[c'',m] = c'' + m e_DW1) -> install is a permutation, not an
  aggregation; assert injectivity once.
- Equivalent MATRIX view (for G3's explicit oracle only): CompKer = A_age
  ROW-gathered by rmap[,k] (output side) and COLUMN-relabeled by imap[,m] (input
  side). The two views must agree (G3 enforces).
- Anderson acceleration: keep depth 5, regularize the least-squares with a small
  ridge (1e-10) for stability; cap max_it; HARD ERROR if not converged (no NULL).
- Hotz-Miller P for G5: from pm_agg_counts collapse to per-(comp,G,action)
  empirical shares; smooth with eps_prob; renormalize. Only needed for G3/G5.
- Feasible-action set must match 023 exactly (k<=N-1, caps, no (0,>0)); reuse the
  rule, do not re-invent. Exit X always feasible.
- NO tryCatch returning NULL anywhere. Hard errors surface. cat section markers.
  Hardcoded-name sink to logs/PM04_*.log.
- Run via C:/Program Files/R/R-4.5.2/bin/x64/Rscript.exe (the 023 env note).

═══════════════════════════════════════════════════
ACCEPTANCE CRITERIA (reviewer)
═══════════════════════════════════════════════════
- [ ] pm_bellman_kernel.R sources with NO side effects (no reads/prints on load)
- [ ] D1 timing implemented: comp transition = remove(rmap) -> age(A_age) ->
      install(imap); 023 `post` array NOT used in the kernel (grep-confirm)
- [ ] D2 structured matvec: M is NEVER formed at 298k scale (no 298k x 298k
      object created; grep-confirm no sparseMatrix/Matrix of dim N_SIDX^2)
- [ ] G1 mass conservation (rowsum 1, <1e-12) passes for all action types
- [ ] G2 remove/install determinism matches an independent recompute (exact int)
- [ ] G3 structured Mv == explicit M%*%V on the N<=2 block (<1e-11)
- [ ] G4 analytic V == r/(1-beta) for maintain-only, R=r*1 (<1e-7)
- [ ] G5 realistic solve converges, V finite & |V|<1e6, residual <1e-7
- [ ] BENCHMARK saved (PM_Kernel_Benchmark.rds) with mv_sec, cold/warm solve
      sec+iters, peak_mem; REPORT prints the 024b recommendation
- [ ] beta read from lookups (==0.9957), not hardcoded; sigma/gamma_E from L6
- [ ] script exits 0; logs present

═══════════════════════════════════════════════════
GATE DECISION (researcher reads the benchmark, then rules)
═══════════════════════════════════════════════════
PASS-GO: warm solve fast enough (projected NPL-iter hours acceptable) -> write
  024b against the R kernel.
PASS-PORT: correctness gates pass but timing too slow -> 024b first ports Mv +
  solve_V to C++ (append to cpp_engine.cpp, full protocol), reusing the validated
  R version as the correctness oracle.
FAIL: a correctness gate fails -> fix the kernel before any 024b work.

═══════════════════════════════════════════════════
RESOLVED Q&A (architect, verified against the frozen objects — treat as spec)
═══════════════════════════════════════════════════
Q1 rmap/imap: column 1 = k=0 (or m=0) NO-OP identity (== 1:C). Stored values are
   1-BASED comp_ids into comp_mat, or NA where infeasible. Use directly:
   rmap[, k+1L], imap[, m+1L]. (rmap has NA; imap[,1] == 1:C.)
Q2 NA TRAP: comp_apply MUST zero infeasible rows from BOTH gathers (imap input,
   rmap output) — 0*NA=NA in R, so P_a=0 does NOT rescue a NA row. assert
   !anyNA on the comp_apply output. (Now in the recipe above.)
Q3 P FORMAT: a named list of C x N_G (4) matrices, one per action; P[[a]][c,G] =
   CCP of action a at comp c, capacity bin G. WORK-action entries feed Mv; the
   "exit" entry feeds R only (never Mv). P_a[c,G] = 0 at infeasible (c,a); over
   all actions incl exit, each (c,G) row sums to 1.
Q4 Gmat[["nowork"]] is the 4x4 IDENTITY (capacity frozen on a no-work year) and
   is DISTINCT from Gmat[["0"]] (a real transition for net-0 WORK, e.g. k=m).
   Always route via Gmat[[ netbin(k,m) ]]; never substitute "0" for "nowork".
Q5 pm_agg_counts.csv action coding = "k,m" strings (e.g. "0,0","1,0","3,2") plus
   "X" for exit; era in {"2006","2014","2019"}. For G5 Hotz-Miller P: SUM n_obs
   over BOTH g and era by (sidx, action), normalize within sidx, smooth
   unobserved-but-feasible actions with eps_prob. Parse work via strsplit(",").
Q6 N(c) = ss$N_vec (precomputed, 1..6). Do NOT recompute rowSums.

═══════════════════════════════════════════════════
ATTEMPT LOG
═══════════════════════════════════════════════════
[reviewer fills]

ATTEMPT 1 — 2026-06-18 — REVIEW (Opus reviewer) — VERDICT: PASS-PORT.
CORRECTNESS: all 5 gates PASS, verified from logs/PM04_20260618_110514.log +
  PM_Kernel_Benchmark.rds + kernel grep:
  G1 mass conservation PASS; G2 determinism PASS (20 comps); G3 structured Mv ==
  explicit M@V at 1.1e-16..2.2e-16 (MACHINE PRECISION — matvec provably exact,
  Kronecker/transpose convention correct); G4 analytic V=697.674=3/(1-0.9957),
  dev 0 exactly; G5 realistic resid 1.05e-10. D1 compliant (no `post`); D2
  compliant (no 298k^2 matrix; peak mem 94 MB — M never formed).
BENCHMARK (the gate numbers): mv 0.42-0.58s; solve COLD 640s/764 iters,
  WARM 303s/529 iters; n_env=17; mem 0.094 GB; beta 0.9957; tol 1e-9.
  FAILS the ship threshold (cold>30s, warm>5s) by ~20-130x.
DIAGNOSIS: two separable causes. (1) ITERATION COUNT 530-764 — Anderson
  fixed-point is too weak for beta=0.9957 (contraction rate ~beta); this is the
  dominant problem. (2) MATVEC 0.42-0.58s — R-level sum over ~20 work actions.
024b PRESCRIPTION (do these FIRST, re-benchmark before building the estimator):
  (a) Replace Anderson fixed-point with a KRYLOV solver (BiCGSTAB or GMRES) on
      the SAME structured matvec. (I-beta M) is a diagonally-dominant M-matrix,
      cond ~ 1/(1-beta) ~ 233 -> Krylov expected ~20-50 iters (vs 530+), ~15x cut.
  (b) Port the structured matvec to C++ (append to cpp_engine.cpp, full protocol;
      keep the R version as the correctness oracle vs G3). 0.42s -> ~10-20ms.
  (c) Warm-start V across optim evals (M fixed in the inner loop); build any
      preconditioner once per NPL iter.
  TARGET to confirm in 024b step 0: cold < ~10s, warm < ~2s per env-solve.
  FALLBACK (plan B, document only): direct sparse factorize-once of (I-beta M)
      per NPL-iter per env. Memory headroom exists (forming M ~1-2 GB) but
      298k fill-in is the risk; use only if Krylov underperforms.
KERNEL STATUS: pm_bellman_kernel.R is CORRECT and stays as the oracle; 024b
  swaps the solver + C++ matvec, re-validates against G3/G4, re-benchmarks.
PROCESS NOTE (not a defect): the coder session died of context exhaustion from
  spawning 5 concurrent Monitors on one long run. For long scripts use ONE
  monitor or a single run_in_background + check. The script itself ran exit 0.
