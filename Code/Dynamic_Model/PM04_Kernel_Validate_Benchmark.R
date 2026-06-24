# PM04_Kernel_Validate_Benchmark.R
# Ticket 024a: correctness gates G1-G5 + benchmark for pm_bellman_kernel.R
# Run via: C:/Program Files/R/R-4.5.2/bin/x64/Rscript.exe

suppressPackageStartupMessages({
  library(Matrix)
  library(data.table)
  library(here)
})

# === LOGGING ===
.log_path <- here::here("logs", paste0("PM04_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".log"))
dir.create(dirname(.log_path), recursive = TRUE, showWarnings = FALSE)
.log <- file(.log_path, open = "wt")
sink(.log, type = "output"); sink(.log, type = "message", append = TRUE)
on.exit({ sink(type = "output"); sink(type = "message"); close(.log) }, add = TRUE)
cat(sprintf("LOG START %s\nScript: PM04_Kernel_Validate_Benchmark.R\nR: %s\nWD: %s\n\n",
    .log_path, R.version.string, getwd()))

# === SECTION: SOURCE KERNEL AND LOAD INPUTS ===
cat("=== SECTION: SOURCE KERNEL AND LOAD INPUTS ===\n")
source(here::here("Code", "Helpers", "pm_bellman_kernel.R"))

ss <- readRDS(here::here("Output", "Estimation_Results", "PM_StateSpace.rds"))
lk <- readRDS(here::here("Output", "Estimation_Results", "PM_Lookups.rds"))

beta <- lk$BETA
stopifnot(beta == 0.9957)
cat(sprintf("beta = %.4f [OK]\n", beta))

kernel <- build_kernel(ss, lk)
C      <- kernel$C
N_G    <- kernel$N_G
N_SIDX <- ss$N_SIDX
cat(sprintf("C = %d, N_G = %d, N_SIDX = %d\n", C, N_G, N_SIDX))
stopifnot(N_SIDX == C * N_G)

K_BAR <- kernel$K_BAR
M_BAR <- kernel$M_BAR
N_BAR <- kernel$N_BAR
cat(sprintf("K_BAR=%d, M_BAR=%d, N_BAR=%d\n", K_BAR, M_BAR, N_BAR))

# === SECTION: G1 — MASS CONSERVATION ===
cat("\n=== SECTION: G1 — MASS CONSERVATION ===\n")

# Test action types covering all categories
test_actions <- data.table(
  k = c(0L, 1L, 2L, 1L, 3L, 0L),
  m = c(0L, 0L, 0L, 1L, 2L, 0L)
)
test_actions <- unique(test_actions)

ones_vec <- matrix(1.0, nrow = C, ncol = 1L)

for (i in seq_len(nrow(test_actions))) {
  k <- test_actions$k[i]
  m <- test_actions$m[i]

  # Find comps for which this action is feasible (N-k+m<=N_BAR, k<=min(N-1,K_BAR))
  N_c      <- kernel$N_vec
  feasible_c <- which(
    k <= pmin(N_c - 1L, K_BAR) &
    N_c - k + m <= N_BAR &
    !(k == 0L & m > 0L)
  )
  if (length(feasible_c) == 0L) {
    cat(sprintf("  action (%d,%d): no feasible comps — skip\n", k, m))
    next
  }

  # Apply to ones — should give 1 at feasible rows
  out <- comp_apply(kernel, k, m, ones_vec)
  row_sums <- out[feasible_c, 1L]
  max_dev   <- max(abs(row_sums - 1.0))
  cat(sprintf("  action (%d,%d): %d feasible comps, max|rowsum-1| = %.3e\n",
              k, m, length(feasible_c), max_dev))
  stopifnot(max_dev < 1e-12)
}
cat("G1 PASS\n")

# === SECTION: G2 — DETERMINISM vs 023 ===
cat("\n=== SECTION: G2 — DETERMINISM vs 023 ===\n")

comp_mat  <- ss$comp_mat         # C x 16
MARG_WALL <- ss$MARG_WALL        # columns: 1..8 SW bins 8..1, 9..16 DW bins 8..1
MARG_BIN  <- ss$MARG_BIN

set.seed(42L)
n_test  <- 20L
c_test  <- sample(seq_len(C), n_test, replace = FALSE)

for (ci in c_test) {
  N_ci <- kernel$N_vec[ci]
  if (N_ci <= 1L) next   # need k>=1 for remove test

  k_try <- min(1L, min(N_ci - 1L, K_BAR))

  # --- Verify remove: rmap[ci, k_try+1] matches greedy oldest-SW-first removal ---
  stored_rc <- kernel$rmap[ci, k_try + 1L]

  # Independent greedy removal: remove k_try oldest SW tanks first (cols 1..8 left-to-right),
  # then oldest DW tanks if needed (cols 9..16)
  row_c <- as.integer(comp_mat[ci, ])   # length 16 counts
  remaining <- row_c
  removed   <- 0L
  # Greedy left-to-right across cols 1..16
  for (col_idx in 1L:16L) {
    if (removed >= k_try) break
    can_remove <- min(remaining[col_idx], k_try - removed)
    remaining[col_idx] <- remaining[col_idx] - can_remove
    removed <- removed + can_remove
  }
  # Find which comp row matches 'remaining'
  match_rows <- which(apply(comp_mat, 1L, function(r) all(as.integer(r) == remaining)))
  if (length(match_rows) == 0L) {
    # rmap NA means infeasible — both should be NA or valid
    stopifnot(is.na(stored_rc))
    next
  }
  stopifnot(length(match_rows) == 1L)
  stopifnot(!is.na(stored_rc))
  if (stored_rc != match_rows[1L]) {
    stop(sprintf("G2 FAIL: comp %d, k=%d: rmap gives %d but greedy gives %d",
                 ci, k_try, stored_rc, match_rows[1L]))
  }

  # --- Verify install: imap[ci, 1+1] adds 1 DW band-1 tank ---
  m_try <- 1L
  if (N_ci + m_try > N_BAR) next
  stored_ic <- kernel$imap[ci, m_try + 1L]
  if (is.na(stored_ic)) next
  row_post  <- as.integer(comp_mat[stored_ic, ])
  # DW band 1 is column 16 (DW bins 8..1, col 9=DW_bin8 ... col 16=DW_bin1)
  row_expected <- row_c
  row_expected[16L] <- row_expected[16L] + 1L
  stopifnot(all(row_post == row_expected))
}
cat(sprintf("G2 PASS (tested %d comps)\n", n_test))

# === SECTION: G3 — STRUCTURED == EXPLICIT (N<=2 block) ===
cat("\n=== SECTION: G3 — STRUCTURED Mv == EXPLICIT M@V ===\n")

# Restrict to comps with N<=2
idx_small <- which(kernel$N_vec <= 2L)
C_small   <- length(idx_small)
cat(sprintf("  N<=2 comps: %d\n", C_small))
stopifnot(C_small > 0L)

# Build a synthetic uniform P over feasible actions for the small block
# Use environment 1 (RB-2006); all G groups identical for this test
set.seed(7L)

# Build CCP P for the small block: uniform over feasible actions (incl exit)
build_uniform_P <- function(kernel, idx_sub) {
  C_sub <- length(idx_sub)
  N_vec_sub <- kernel$N_vec[idx_sub]

  # Collect all actions (including exit)
  all_acts <- copy(kernel$work_all)
  all_acts[, akey := paste0(k, ",", m)]
  exit_key <- "X"

  P_out <- list()

  # Work actions
  for (i in seq_len(nrow(all_acts))) {
    k <- all_acts$k[i]; m <- all_acts$m[i]
    P_out[[all_acts$akey[i]]] <- matrix(0.0, nrow = kernel$C, ncol = kernel$N_G)
  }
  P_out[["X"]] <- matrix(0.0, nrow = kernel$C, ncol = kernel$N_G)

  # For each comp in idx_sub, assign uniform CCP
  for (ji in seq_along(idx_sub)) {
    ci <- idx_sub[ji]
    N_ci <- kernel$N_vec[ci]
    fa   <- feasible_actions(N_ci, kernel$K_BAR, kernel$M_BAR, kernel$N_BAR)
    n_fa <- nrow(fa) + 1L   # +1 for exit
    for (i in seq_len(nrow(fa))) {
      akey <- paste0(fa$k[i], ",", fa$m[i])
      if (!akey %in% names(P_out)) P_out[[akey]] <- matrix(0.0, nrow=kernel$C, ncol=kernel$N_G)
      P_out[[akey]][ci, ] <- 1.0 / n_fa
    }
    P_out[["X"]][ci, ] <- 1.0 / n_fa
  }
  P_out
}

P_small <- build_uniform_P(kernel, idx_small)

# Build explicit M for the small block (sub-block only: C_small x C_small per G)
# F_a[(c,G),(c',G')] = CompKer_a[c,c'] * Gmat_a[G,G']
# Full state index for sub-block: sidx_sub = (G-1)*C + idx_small[c_local]
# For G3 we only need the sub-block: indices are (G-1)*C_small + c_local
# But the actual sidx in the full space is (G-1)*C + idx_small[c_local]
# We build M_full restricted to (idx_small, idx_small) per G block.

# Instead: build M_expl as (C_small*N_G) x (C_small*N_G) dense matrix
N_small_tot <- C_small * N_G

cat("  Building explicit M for small block...\n")
M_expl <- matrix(0.0, nrow = N_small_tot, ncol = N_small_tot)

for (i in seq_len(nrow(kernel$work_all))) {
  k <- kernel$work_all$k[i]; m <- kernel$work_all$m[i]
  akey <- paste0(k, ",", m)
  if (!akey %in% names(P_small)) next

  nb  <- netbin(k, m)
  Ga  <- kernel$Gmat[[nb]]   # 4x4

  # CompKer_a restricted to idx_small x idx_small
  # CompKer_a[c,c'] = sum_{c''} A_age[rmap[c,k], c''] * delta(imap[c'',m] == c')
  # For the small block, build explicitly
  # rmap row-gather on rows, imap col-relabel
  # Compute CompKer_a for full C, then restrict
  # Actually build as dense C_small x C_small
  CK_a <- matrix(0.0, nrow = C_small, ncol = C_small)

  # For each source c (cols in the CompKer sense = input side)
  # comp_apply(kernel, k, m, X): input-side is imap (install), output-side is rmap (remove)
  # CompKer_a[row=c, col=c'] means applying CompKer to e_{c'} gives the column
  # Let's build column by column: CK_a[,j] = comp_apply applied to e_{idx_small[j]}
  e_j <- matrix(0.0, nrow = kernel$C, ncol = 1L)
  for (j in seq_along(idx_small)) {
    e_j[] <- 0.0
    e_j[idx_small[j], 1L] <- 1.0
    col_full <- comp_apply(kernel, k, m, e_j)
    CK_a[, j] <- col_full[idx_small, 1L]
  }

  # P_a for sub-block: C_small x N_G
  P_a_sub <- P_small[[akey]][idx_small, ]   # C_small x 4

  # F_a restricted: (C_small*N_G) x (C_small*N_G)
  # sidx = (G-1)*C_small + c_local (local indexing)
  # F_a[sidx_row, sidx_col] = CK_a[c_row, c_col] * Ga[G_row, G_col]
  # = (Ga kron CK_a)[sidx_row, sidx_col]
  # F_a_full = kronecker(Ga, CK_a)  (standard kron convention: M kron N)
  # But sidx = (G-1)*C + c means the FIRST index varies over c (within each G block)
  # So matrix is laid out as blocks: [ G=1 rows ... | G=2 rows ... | ...]
  # kron(Ga, CK_a) gives exactly this: row block G_from, col block G_to = Ga[G_from,G_to]*CK_a
  F_a_small <- kronecker(Ga, CK_a)   # (C_small*4) x (C_small*4)

  # Diag(P_a): expand P_a_sub to a diagonal per (c,G)
  # State vector order: (G=1, c=1..C_small), (G=2, c=1..C_small), ...
  P_diag <- as.vector(t(P_a_sub))    # G-major then c: length C_small*4
  # Wait: sidx = (G-1)*C_small + c means: first C_small entries are G=1 c=1..C_small
  # P_a_sub[c, G] -> for sidx=(G-1)*C_small+c: need to arrange as vector in sidx order
  P_diag_correct <- numeric(N_small_tot)
  for (G in 1L:N_G) {
    P_diag_correct[((G-1L)*C_small + 1L):(G*C_small)] <- P_a_sub[, G]
  }

  M_expl <- M_expl + diag(P_diag_correct) %*% F_a_small
}

cat(sprintf("  M_expl built: dim %d x %d\n", nrow(M_expl), ncol(M_expl)))

# Compare structured Mv with M_expl %*% V on 5 random V
set.seed(13L)
for (rep in 1L:5L) {
  V_rand_sub <- rnorm(N_small_tot)
  Vmat_full  <- matrix(0.0, nrow = C, ncol = N_G)
  for (G in 1L:N_G) {
    Vmat_full[idx_small, G] <- V_rand_sub[((G-1L)*C_small+1L):(G*C_small)]
  }

  # Structured
  MV_struct_full <- Mv(kernel, P_small, Vmat_full)
  MV_struct_sub  <- numeric(N_small_tot)
  for (G in 1L:N_G) {
    MV_struct_sub[((G-1L)*C_small+1L):(G*C_small)] <- MV_struct_full[idx_small, G]
  }

  # Explicit
  MV_expl <- M_expl %*% V_rand_sub

  max_diff <- max(abs(MV_struct_sub - MV_expl))
  cat(sprintf("  rep %d: max|structured - explicit| = %.3e\n", rep, max_diff))
  stopifnot(max_diff < 1e-11)
}
cat("G3 PASS\n")

# === SECTION: G4 — ANALYTIC CLOSED FORM ===
cat("\n=== SECTION: G4 — ANALYTIC CLOSED FORM ===\n")

# P = maintain-only (0,0)=1, all others=0; R = r*ones, r=3.0
r_val <- 3.0
R_g4  <- rep(r_val, N_SIDX)

P_maintain <- list()
P_maintain[["0,0"]] <- matrix(1.0, nrow = C, ncol = N_G)
P_maintain[["X"]]   <- matrix(0.0, nrow = C, ncol = N_G)

V_g4 <- solve_V(kernel, R_g4, P_maintain, beta, tol = 1e-9, max_it = 10000L, anderson = 5L)
analytic_V <- r_val / (1 - beta)
max_dev_g4 <- max(abs(V_g4$V - analytic_V))
cat(sprintf("  analytic V = %.6f, solve max dev = %.3e (iters=%d)\n",
            analytic_V, max_dev_g4, V_g4$iters))
stopifnot(max_dev_g4 < 1e-7)
cat("G4 PASS\n")

# === SECTION: G5 — REALISTIC PLACEHOLDER ===
cat("\n=== SECTION: G5 — REALISTIC PLACEHOLDER R + HOTZ-MILLER P ===\n")

# Load pm_agg_counts
counts_dt <- fread(here::here("Data", "Analysis", "pm_agg_counts.csv"))
cat(sprintf("  pm_agg_counts: %d rows, cols: %s\n", nrow(counts_dt), paste(names(counts_dt), collapse=",")))

# Build Hotz-Miller P from counts: collapse over g and era by (sidx, action)
hm_dt <- counts_dt[, .(n_obs = sum(n_obs)), by = .(sidx, action)]

# Decode sidx -> (c, G) using sidx = (G-1)*C + c
hm_dt[, G  := as.integer(ceiling(sidx / C))]
hm_dt[, c  := as.integer(sidx - (G - 1L) * C)]
stopifnot(all(hm_dt$G >= 1L), all(hm_dt$G <= N_G))
stopifnot(all(hm_dt$c >= 1L), all(hm_dt$c <= C))

# Normalize within (c, G) with eps_prob smoothing
eps_prob <- 1e-10

# Initialize P lists from all feasible actions across all N
all_action_keys <- c(sapply(seq_len(nrow(kernel$work_all)),
                             function(i) paste0(kernel$work_all$k[i], ",", kernel$work_all$m[i])),
                      "X")

P_hm <- setNames(
  lapply(all_action_keys, function(ak) matrix(eps_prob, nrow = C, ncol = N_G)),
  all_action_keys
)

# Add observed counts
for (ri in seq_len(nrow(hm_dt))) {
  ci   <- hm_dt$c[ri]
  Gi   <- hm_dt$G[ri]
  act  <- hm_dt$action[ri]
  nobs <- hm_dt$n_obs[ri]
  if (!act %in% names(P_hm)) next
  P_hm[[act]][ci, Gi] <- P_hm[[act]][ci, Gi] + nobs
}

# Normalize rows per (c,G): sum over actions = 1
row_totals <- matrix(0.0, nrow = C, ncol = N_G)
for (ak in names(P_hm)) row_totals <- row_totals + P_hm[[ak]]
for (ak in names(P_hm)) P_hm[[ak]] <- P_hm[[ak]] / row_totals

# Verify: row sums = 1 (sample check)
check_tot <- matrix(0.0, nrow = C, ncol = N_G)
for (ak in names(P_hm)) check_tot <- check_tot + P_hm[[ak]]
stopifnot(max(abs(check_tot - 1.0)) < 1e-8)

# Placeholder R with trial theta (NOT the 024b flow utility — placeholder only)
# u_work ~ phi0 + gp*P_index(n,env) - gr*H(n)*D - k*cR - m*cI
phi0 <- 1.0; gp <- -2.0; gr <- 0.1; cR <- 0.5; cI <- 0.5

# Use P_RB_all col "2006" for P_index; h_aw[h_idx] for H(n); D from lk
P_idx_env <- ss$P_RB_all[, "2006"]   # length C
H_n       <- lk$h_aw[ss$h_idx]       # length C
D_val <- mean(lk$D)    # D has length 18 in lookups; use mean as placeholder scalar
cat(sprintf("  P_idx range: [%.4f, %.4f], H_n range: [%.4f, %.4f], D_val=%.4f\n",
            min(P_idx_env), max(P_idx_env), min(H_n), max(H_n), D_val))

R_g5 <- numeric(N_SIDX)
for (G in 1L:N_G) {
  sidx_G <- ((G-1L)*C + 1L):(G*C)   # state indices for this G block

  # Flow utility for each action at each comp
  u_exit <- rep(-10.0, C)   # placeholder exit utility

  # Compute R[c,G] = sum_a P_a[c,G]*(u_a + sigma*(gamma_E - log(P_a[c,G])))
  R_cG <- numeric(C)

  # Work actions
  for (i in seq_len(nrow(kernel$work_all))) {
    k <- kernel$work_all$k[i]; m <- kernel$work_all$m[i]
    akey <- paste0(k, ",", m)
    Pa_cG <- P_hm[[akey]][, G]
    u_cG  <- phi0 + gp * P_idx_env - gr * H_n * D_val - k * cR - m * cI
    entr  <- numeric(C)
    pos   <- Pa_cG > 0
    entr[pos] <- sigma * (gamma_E - log(Pa_cG[pos]))
    R_cG  <- R_cG + Pa_cG * (u_cG + entr)
  }

  # Exit action
  Pa_X <- P_hm[["X"]][, G]
  entr_X <- numeric(C)
  pos_X  <- Pa_X > 0
  entr_X[pos_X] <- sigma * (gamma_E - log(Pa_X[pos_X]))
  R_cG <- R_cG + Pa_X * (u_exit + entr_X)

  R_g5[sidx_G] <- R_cG
}

cat(sprintf("  R_g5 range: [%.4f, %.4f]\n", min(R_g5), max(R_g5)))

# Quick timing of one Mv call with G5 P before launching solve
t_mv_probe <- system.time(Mv(kernel, P_hm, matrix(rnorm(N_SIDX), nrow=C)))["elapsed"]
cat(sprintf("  One Mv call (probe before G5 solve): %.4f sec\n", t_mv_probe))

V_g5 <- solve_V(kernel, R_g5, P_hm, beta, tol = 1e-9, max_it = 10000L, anderson = 5L)
cat(sprintf("  G5 solve: converged=%s, iters=%d, resid=%.3e\n",
            V_g5$converged, V_g5$iters, V_g5$resid))
stopifnot(V_g5$converged)
stopifnot(all(is.finite(V_g5$V)))
stopifnot(all(abs(V_g5$V) < 1e6))

# Verify residual via independent Mv call
Vmat_g5 <- matrix(V_g5$V, nrow = C)
MVmat_g5 <- Mv(kernel, P_hm, Vmat_g5)
resid_vec <- as.vector(matrix(R_g5, nrow=C) + beta * MVmat_g5) - V_g5$V
rel_resid  <- max(abs(resid_vec)) / max(abs(R_g5))
cat(sprintf("  Independent residual check: inf-norm residual / ||R||_inf = %.3e\n", rel_resid))
stopifnot(rel_resid < 1e-7)
cat("G5 PASS\n")

# === SECTION: BENCHMARK ===
cat("\n=== SECTION: BENCHMARK ===\n")

# Benchmark Mv (one call, median of 5)
Vmat_bench <- matrix(rnorm(N_SIDX), nrow = C)
mv_times <- numeric(5L)
for (rep in 1L:5L) {
  t0 <- proc.time()["elapsed"]
  invisible(Mv(kernel, P_hm, Vmat_bench))
  mv_times[rep] <- proc.time()["elapsed"] - t0
}
mv_sec <- median(mv_times)
cat(sprintf("  Mv median time (5 reps): %.4f sec\n", mv_sec))

# Benchmark cold solve (RB-2006 env, G5 P and R)
cat("  Benchmarking cold solve (RB-2006)...\n")
t0_cold <- proc.time()["elapsed"]
V_cold <- solve_V(kernel, R_g5, P_hm, beta, tol = 1e-9, max_it = 10000L, anderson = 5L)
solve_sec_cold <- proc.time()["elapsed"] - t0_cold
solve_iters_cold <- V_cold$iters
cat(sprintf("  Cold solve: %.4f sec, %d iters\n", solve_sec_cold, solve_iters_cold))

# Warm-start: perturb R slightly, re-solve from V_cold
R_warm <- R_g5 + rnorm(N_SIDX) * 0.01
cat("  Benchmarking warm solve (small R perturbation)...\n")
t0_warm <- proc.time()["elapsed"]
# solve_V always starts from R/(1-beta); for warm-start, inject V0 by running g_fn steps
# Implement warm-start: we pass V_cold$V as initial guess via a one-shot wrapper
solve_V_warm <- function(kernel, R, P, beta, V0, tol = 1e-9, max_it = 10000L, anderson = 5L) {
  C   <- kernel$C
  N_G <- kernel$N_G
  N   <- C * N_G
  R_mat <- matrix(R, nrow = C)
  FP_fn <- function(v) {
    Vm <- matrix(v, nrow = C)
    as.vector(R_mat + beta * Mv(kernel, P, Vm))
  }
  V_vec  <- V0
  G_hist <- matrix(0.0, nrow = N, ncol = 0L)
  X_hist <- matrix(0.0, nrow = N, ncol = 0L)
  for (it in seq_len(max_it)) {
    Fv <- FP_fn(V_vec)
    gv <- Fv - V_vec
    resid_inf <- max(abs(gv))
    if (resid_inf < tol) return(list(V = V_vec, iters = it, resid = resid_inf, converged = TRUE))
    G_hist <- cbind(G_hist, gv)
    X_hist <- cbind(X_hist, V_vec)
    if (ncol(G_hist) > anderson + 1L) {
      keep   <- seq(ncol(G_hist) - anderson, ncol(G_hist))
      G_hist <- G_hist[, keep, drop = FALSE]
      X_hist <- X_hist[, keep, drop = FALSE]
    }
    m_k <- ncol(G_hist) - 1L
    if (m_k == 0L) {
      V_vec <- Fv
    } else {
      nh  <- ncol(G_hist)
      dG  <- G_hist[, 2L:nh, drop=FALSE] - G_hist[, 1L:(nh-1L), drop=FALSE]
      dX  <- X_hist[, 2L:nh, drop=FALSE] - X_hist[, 1L:(nh-1L), drop=FALSE]
      gram     <- crossprod(dG)
      rhs_neg  <- -as.vector(crossprod(dG, gv))
      max_diag <- max(diag(gram))
      if (max_diag < 1e-28) {
        V_vec <- Fv
      } else {
        ridge_val <- 1e-6 * max_diag + 1e-30
        gamma     <- solve(gram + ridge_val * diag(m_k), rhs_neg)
        V_step    <- Fv + as.vector((dX + dG) %*% gamma)
        if (anyNA(V_step) || !all(is.finite(V_step))) {
          G_hist <- matrix(0.0, nrow = N, ncol = 0L)
          X_hist <- matrix(0.0, nrow = N, ncol = 0L)
          V_vec  <- Fv
        } else {
          V_vec <- V_step
        }
      }
    }
  }
  stop(sprintf("solve_V_warm did not converge in %d iters; resid=%.3e", max_it, max(abs(FP_fn(V_vec)-V_vec))))
}

V_warm <- solve_V_warm(kernel, R_warm, P_hm, beta, V0 = V_cold$V,
                        tol = 1e-9, max_it = 10000L, anderson = 5L)
solve_sec_warm   <- proc.time()["elapsed"] - t0_warm
solve_iters_warm <- V_warm$iters
cat(sprintf("  Warm solve: %.4f sec, %d iters\n", solve_sec_warm, solve_iters_warm))

# Repeat for FF env (use P_maintain as a stand-in for FF — same timing, different R)
cat("  Benchmarking cold solve FF env (maintain-only P, constant R)...\n")
R_ff <- rep(2.0, N_SIDX)
t0_ff <- proc.time()["elapsed"]
V_ff  <- solve_V(kernel, R_ff, P_maintain, beta, tol = 1e-9, max_it = 10000L, anderson = 5L)
solve_sec_ff   <- proc.time()["elapsed"] - t0_ff
cat(sprintf("  FF cold solve: %.4f sec, %d iters\n", solve_sec_ff, V_ff$iters))

# Peak memory estimate
mem_kernel_mb <- as.numeric(object.size(kernel)) / 1e6
mem_Vmat_mb   <- as.numeric(object.size(Vmat_bench)) / 1e6
mem_P_mb      <- sum(sapply(P_hm, function(x) as.numeric(object.size(x)))) / 1e6
peak_mem_gb   <- (mem_kernel_mb + mem_Vmat_mb * 6 + mem_P_mb) / 1e3  # rough working set
cat(sprintf("  kernel: %.1f MB, P: %.1f MB, Vmat working: %.1f MB, peak_mem_gb~%.3f\n",
            mem_kernel_mb, mem_P_mb, mem_Vmat_mb * 6, peak_mem_gb))

# Projected NPL-iter cost
n_env        <- 17L
proj_cold_hr <- n_env * solve_sec_cold / 3600
proj_warm_hr <- n_env * solve_sec_warm / 3600
cat(sprintf("  Projected NPL-iter (17 envs): cold=%.3f hr, warm=%.3f hr\n",
            proj_cold_hr, proj_warm_hr))

# === SECTION: SAVE BENCHMARK ===
cat("\n=== SECTION: SAVE BENCHMARK ===\n")

benchmark <- list(
  mv_sec            = mv_sec,
  solve_sec_cold    = solve_sec_cold,
  solve_iters_cold  = solve_iters_cold,
  solve_sec_warm    = solve_sec_warm,
  solve_iters_warm  = solve_iters_warm,
  n_env             = n_env,
  peak_mem_gb       = peak_mem_gb,
  beta              = beta,
  tol               = 1e-9
)
out_path <- here::here("Output", "Estimation_Results", "PM_Kernel_Benchmark.rds")
saveRDS(benchmark, out_path)
cat(sprintf("Saved: %s\n", out_path))

# === SECTION: REPORT AND RECOMMENDATION ===
cat("\n=== SECTION: REPORT AND RECOMMENDATION ===\n")
cat("─────────────────────────────────────────────────────\n")
cat(sprintf("  C=%d, N_G=%d, N_SIDX=%d, beta=%.4f\n", C, N_G, N_SIDX, beta))
cat(sprintf("  Mv median:       %.4f sec\n", mv_sec))
cat(sprintf("  Cold solve:      %.4f sec / %d iters\n", solve_sec_cold, solve_iters_cold))
cat(sprintf("  Warm solve:      %.4f sec / %d iters\n", solve_sec_warm, solve_iters_warm))
cat(sprintf("  FF cold solve:   %.4f sec / %d iters\n", solve_sec_ff, V_ff$iters))
cat(sprintf("  Peak mem (est):  %.3f GB\n", peak_mem_gb))
cat(sprintf("  Proj cold/NPL:   %.3f hr  (17 envs x %.4f sec)\n", proj_cold_hr, solve_sec_cold))
cat(sprintf("  Proj warm/NPL:   %.3f hr  (17 envs x %.4f sec)\n", proj_warm_hr, solve_sec_warm))

cat("\n  RECOMMENDATION FOR 024b:\n")
# Threshold: cold > 30s or warm > 5s -> recommend C++ port
if (solve_sec_cold > 30 || solve_sec_warm > 5) {
  cat("  (i)  KERNEL: C++ port of Mv + solve_V RECOMMENDED (timing exceeds threshold)\n")
  cat("       Cold solve > 30s or warm > 5s — R-level kernel too slow for NPL loop.\n")
  cat("       Action: port Mv to Rcpp (append to cpp_engine.cpp, full protocol);\n")
  cat("               use this R version as correctness oracle in tests.\n")
} else {
  cat("  (i)  KERNEL: R-level kernel ADEQUATE (cold <= 30s and warm <= 5s)\n")
  cat("       Write 024b against the R kernel without C++ port.\n")
}

if (solve_iters_cold > 200) {
  cat("  (ii) SOLVER: BiCGSTAB may outperform Anderson — iters > 200; consider testing.\n")
} else {
  cat("  (ii) SOLVER: Anderson acceleration ADEQUATE (iters <= 200)\n")
}

cat(sprintf("  (iii) NPL-iter cost: %.3f hr/iter (warm); %.3f hr/iter (cold).\n",
            proj_warm_hr, proj_cold_hr))
if (proj_warm_hr > 1) {
  cat("        WARNING: projected NPL time > 1 hr/iter — review feasibility before 024b.\n")
} else {
  cat("        Projected NPL time is within acceptable range.\n")
}
cat("─────────────────────────────────────────────────────\n")

cat(sprintf("\nAll gates PASS. Script complete. Log: %s\n", .log_path))
