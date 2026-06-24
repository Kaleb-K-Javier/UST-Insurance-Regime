# PM07_Preconditioner_Bakeoff.R
# Ticket 024d: Aging-backbone preconditioner (P1) for the portfolio value solve.
# Builds a triangular preconditioner from the maintain-action backbone (M_maintain),
# validates against the oracle (unpreconditioned BiCGSTAB), benchmarks iteration
# counts, and prints a PASS/MISS recommendation for 024b.
#
# Theory references (web-verified 2026-06-19):
#   T1: (I - beta M) is an M-matrix (Bertsekas DP; Stewart, Computations with MC).
#   T2: ILU exists breakdown-free for M-matrices (Meijerink & van der Vorst 1977).
#   T3: Monotone aging backbone is acyclic -> triangular under age-graded ordering
#       (Dai et al. 2011; Stanford CS221; CMU "Optimal Backup Order for Acyclic MDPs").
#
# Run via: C:/Program Files/R/R-4.5.2/bin/x64/Rscript.exe
# NOTHING about the model changes. P_hm, R_g5, A_x copied verbatim from PM05.

suppressPackageStartupMessages({
  library(Matrix)
  library(data.table)
  library(here)
})

# === LOGGING ===
.log_path <- here::here("logs", paste0("PM07_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".log"))
dir.create(dirname(.log_path), recursive = TRUE, showWarnings = FALSE)
.log <- file(.log_path, open = "wt")
sink(.log, type = "output"); sink(.log, type = "message", append = TRUE)
on.exit({ sink(type = "output"); sink(type = "message"); close(.log) }, add = TRUE)
cat(sprintf("LOG START %s\nScript: PM07_Preconditioner_Bakeoff.R\nR: %s\nWD: %s\n\n",
    .log_path, R.version.string, getwd()))

# === SECTION: SOURCE KERNEL AND C++ MATVEC ===
cat("=== SECTION: SOURCE KERNEL AND C++ MATVEC ===\n")
source(here::here("Code", "Helpers", "pm_bellman_kernel.R"))
cat("  pm_bellman_kernel.R sourced [OK]\n")

cpp_path <- here::here("Code", "Helpers", "pm_matvec.cpp")
if (!file.exists(cpp_path))
  stop(sprintf("pm_matvec.cpp not found at: %s\nFollow 024c build protocol E1-E5.", cpp_path))
Rcpp::sourceCpp(cpp_path)
cat("  pm_matvec.cpp sourced [OK]\n")

# === SECTION: LOAD INPUTS ===
cat("\n=== SECTION: LOAD INPUTS ===\n")
ss <- readRDS(here::here("Output", "Estimation_Results", "PM_StateSpace.rds"))
lk <- readRDS(here::here("Output", "Estimation_Results", "PM_Lookups.rds"))

beta <- lk$BETA
stopifnot(beta == 0.9957)
cat(sprintf("  beta = %.4f [OK]\n", beta))

kernel <- build_kernel(ss, lk)
C      <- kernel$C
N_G    <- kernel$N_G
N_SIDX <- ss$N_SIDX
stopifnot(N_SIDX == C * N_G)
cat(sprintf("  C = %d, N_G = %d, N_SIDX = %d\n", C, N_G, N_SIDX))

stopifnot(length(ss$MARG_BIN) == 16L, all(ss$MARG_BIN %in% 1:8))
cat(sprintf("  MARG_BIN [%s] [OK]\n", paste(ss$MARG_BIN, collapse = ",")))
cat(sprintf("  gamma_E = %.10f, sigma = %.1f\n", gamma_E, sigma))

# === SECTION: P_hm AND R_g5 (verbatim PM05_Solver_Bakeoff.R lines 42-126) ===
cat("\n=== SECTION: P_hm AND R_g5 (verbatim PM05 lines 42-126) ===\n")
# --- begin verbatim PM05 lines 42-126 ---
counts_dt <- fread(here::here("Data", "Analysis", "pm_agg_counts.csv"))
cat(sprintf("  pm_agg_counts: %d rows, cols: %s\n",
            nrow(counts_dt), paste(names(counts_dt), collapse = ",")))

hm_dt <- counts_dt[, .(n_obs = sum(n_obs)), by = .(sidx, action)]
hm_dt[, G := as.integer(ceiling(sidx / C))]
hm_dt[, c := as.integer(sidx - (G - 1L) * C)]
stopifnot(all(hm_dt$G >= 1L), all(hm_dt$G <= N_G))
stopifnot(all(hm_dt$c >= 1L), all(hm_dt$c <= C))

eps_prob <- 1e-10

all_action_keys <- c(
  vapply(seq_len(nrow(kernel$work_all)),
         function(i) paste0(kernel$work_all$k[i], ",", kernel$work_all$m[i]),
         character(1L)),
  "X"
)

P_hm <- setNames(
  lapply(all_action_keys, function(ak) matrix(eps_prob, nrow = C, ncol = N_G)),
  all_action_keys
)

for (ri in seq_len(nrow(hm_dt))) {
  ci   <- hm_dt$c[ri]
  Gi   <- hm_dt$G[ri]
  act  <- hm_dt$action[ri]
  nobs <- hm_dt$n_obs[ri]
  if (!act %in% names(P_hm)) next
  P_hm[[act]][ci, Gi] <- P_hm[[act]][ci, Gi] + nobs
}

row_totals <- matrix(0.0, nrow = C, ncol = N_G)
for (ak in names(P_hm)) row_totals <- row_totals + P_hm[[ak]]
for (ak in names(P_hm)) P_hm[[ak]] <- P_hm[[ak]] / row_totals

check_tot <- matrix(0.0, nrow = C, ncol = N_G)
for (ak in names(P_hm)) check_tot <- check_tot + P_hm[[ak]]
stopifnot(max(abs(check_tot - 1.0)) < 1e-8)
cat(sprintf("  P_hm built: %d actions, row-sum check OK\n", length(P_hm)))

phi0 <- 1.0; gp <- -2.0; gr <- 0.1; cR <- 0.5; cI <- 0.5

P_idx_env <- ss$P_RB_all[, "2006"]
H_n       <- lk$h_aw[ss$h_idx]
D_val     <- mean(lk$D)
cat(sprintf("  P_idx [%.4f, %.4f], H_n [%.4f, %.4f], D_val=%.4f\n",
            min(P_idx_env), max(P_idx_env), min(H_n), max(H_n), D_val))

R_g5 <- numeric(N_SIDX)
for (G in 1L:N_G) {
  sidx_G <- ((G - 1L) * C + 1L):(G * C)
  u_exit <- rep(-10.0, C)
  R_cG   <- numeric(C)

  for (i in seq_len(nrow(kernel$work_all))) {
    k     <- kernel$work_all$k[i]; m <- kernel$work_all$m[i]
    akey  <- paste0(k, ",", m)
    Pa_cG <- P_hm[[akey]][, G]
    u_cG  <- phi0 + gp * P_idx_env - gr * H_n * D_val - k * cR - m * cI
    entr  <- numeric(C)
    pos   <- Pa_cG > 0
    entr[pos] <- sigma * (gamma_E - log(Pa_cG[pos]))
    R_cG  <- R_cG + Pa_cG * (u_cG + entr)
  }

  Pa_X   <- P_hm[["X"]][, G]
  entr_X <- numeric(C)
  pos_X  <- Pa_X > 0
  entr_X[pos_X] <- sigma * (gamma_E - log(Pa_X[pos_X]))
  R_cG   <- R_cG + Pa_X * (u_exit + entr_X)
  R_g5[sidx_G] <- R_cG
}
cat(sprintf("  R_g5 range: [%.4f, %.4f]\n", min(R_g5), max(R_g5)))
stopifnot(all(is.finite(R_g5)))

# R A_x (traceability; all BiCGSTAB runs use A_x_cpp)
A_x <- function(x) x - beta * as.vector(Mv(kernel, P_hm, matrix(x, nrow = C)))
# --- end verbatim PM05 lines 42-126 ---

# === SECTION: C++ OPERATOR BUILD (verbatim from PM06_Cpp_Matvec.R) ===
cat("\n=== SECTION: C++ OPERATOR BUILD ===\n")
imap0 <- kernel$imap - 1L
imap0[is.na(imap0)] <- -1L
storage.mode(imap0) <- "integer"

rmap0 <- kernel$rmap - 1L
rmap0[is.na(rmap0)] <- -1L
storage.mode(rmap0) <- "integer"

work_dt <- kernel$work_all
k_vec   <- as.integer(work_dt$k)
m_vec   <- as.integer(work_dt$m)
n_act   <- nrow(work_dt)

GaT_list <- vector("list", n_act)
for (i in seq_len(n_act)) {
  nb            <- netbin(k_vec[i], m_vec[i])
  GaT_list[[i]] <- t(kernel$Gmat[[nb]])
}

P_list_cpp <- vector("list", n_act)
for (i in seq_len(n_act)) {
  akey            <- paste0(k_vec[i], ",", m_vec[i])
  P_list_cpp[[i]] <- if (akey %in% names(P_hm)) P_hm[[akey]] else matrix(0.0, C, N_G)
}

cat(sprintf("  n_actions = %d,  nnz(A_age) = %d\n", n_act, length(kernel$A_age@x)))
t0_build <- proc.time()["elapsed"]
cpp_op <- pm_op_build(
  A_age    = kernel$A_age,
  imap0    = imap0,
  rmap0    = rmap0,
  k_vec    = k_vec,
  m_vec    = m_vec,
  GaT_list = GaT_list,
  P_list   = P_list_cpp,
  C        = C,
  N_G      = N_G
)
build_sec_cpp <- proc.time()["elapsed"] - t0_build
cat(sprintf("  C++ op built in %.2f sec\n", build_sec_cpp))

A_x_cpp <- function(x) x - beta * as.numeric(pm_op_mv(cpp_op, x))

# Assert A_x_cpp == A_x on 2 random vectors (PM07 self-contained; 024c proved to 1e-12)
cat("  Asserting A_x_cpp == A_x_R to 1e-12 on 2 random vectors...\n")
set.seed(13L)
for (vi in 1:2) {
  v_rand <- rnorm(N_SIDX)
  dev    <- max(abs(A_x_cpp(v_rand) - A_x(v_rand)))
  cat(sprintf("    v%d: max|A_x_cpp - A_x_R| = %.3e\n", vi, dev))
  stopifnot(dev < 1e-12)
}
cat("  A_x_cpp == A_x_R to 1e-12 [OK]\n")

# === SECTION: bicgstab_pc (right-preconditioned; Barrett et al. Templates) ===
cat("\n=== SECTION: bicgstab_pc DEFINITION ===\n")
# Right-preconditioned BiCGSTAB. Based verbatim on PM05 bicgstab() (lines 334-391)
# with two preconditioner applies added: phat = Minv(p), shat = Minv(s).
# With Minv = function(r) r (identity), phat = p and shat = s, recovering PM05 exactly.
bicgstab_pc <- function(Ax_fn, b, Minv, tol = 1e-9, max_it = 2000L, x0 = NULL) {
  n    <- length(b)
  x    <- if (!is.null(x0)) x0 else numeric(n)
  r    <- b - Ax_fn(x)
  rhat <- r
  rho  <- 1.0; alpha <- 1.0; w <- 1.0
  v    <- numeric(n); p <- numeric(n)

  for (it in seq_len(max_it)) {
    rho1 <- sum(rhat * r)

    if (abs(rho1) < 1e-100) {
      rhat <- r
      rho  <- 1.0; alpha <- 1.0; w <- 1.0
      v    <- numeric(n); p <- numeric(n)
      rho1 <- sum(rhat * r)
      if (abs(rho1) < 1e-100)
        stop(sprintf("BiCGSTAB stalled: rho1=%.3e at iter %d", rho1, it))
    }

    bta  <- (rho1 / rho) * (alpha / w)
    p    <- r + bta * (p - w * v)
    phat <- Minv(p)                      # right-preconditioner apply
    v    <- Ax_fn(phat)

    denom_alpha <- sum(rhat * v)
    if (abs(denom_alpha) < 1e-100)
      stop(sprintf("BiCGSTAB stalled: <rhat,v>=%.3e at iter %d", denom_alpha, it))

    alpha <- rho1 / denom_alpha
    s     <- r - alpha * v

    if (max(abs(s)) < tol) {
      x <- x + alpha * phat
      return(list(x = x, iters = it, converged = TRUE, resid = max(abs(s))))
    }

    shat <- Minv(s)                      # right-preconditioner apply
    t    <- Ax_fn(shat)

    denom_w <- sum(t * t)
    if (abs(denom_w) < 1e-100)
      stop(sprintf("BiCGSTAB stalled: <t,t>=%.3e at iter %d", denom_w, it))

    w   <- sum(t * s) / denom_w
    x   <- x + alpha * phat + w * shat
    r   <- s - w * t
    rho <- rho1

    resid_cur <- max(abs(r))
    if (resid_cur < tol)
      return(list(x = x, iters = it, converged = TRUE, resid = resid_cur))

    if (it %% 50L == 0L)
      cat(sprintf("  [%s] BiCGSTAB iter %d: ||r||_inf = %.3e\n",
                  format(Sys.time(), "%H:%M:%S"), it, resid_cur))
  }
  stop(sprintf("BiCGSTAB did not converge in %d iters; ||r||_inf = %.3e",
               max_it, max(abs(r))))
}
Minv_identity <- function(r) r
cat("  bicgstab_pc() defined [OK]\n")

# === SECTION: BUILD TRIANGULAR ORDER (step 1) ===
cat("\n=== SECTION: BUILD TRIANGULAR ORDER ===\n")
# T3: aging is monotone (bin only increases) => A_age is triangular under age-potential order.
# pot(c) = sum of remaining youth across all tanks in composition c.
MARG_BIN <- ss$MARG_BIN
pot      <- as.vector(ss$comp_mat %*% (8L - MARG_BIN))   # length C
cat(sprintf("  pot range: [%.0f, %.0f]\n", min(pot), max(pot)))

ord          <- order(pot, decreasing = TRUE)   # youngest-first (highest pot first)
inv_ord      <- integer(C)
inv_ord[ord] <- seq_len(C)                      # inv_ord[original_idx] = permuted_idx

A_perm <- ss$A_age[ord, ord]   # permuted aging operator; should be triangular
cat(sprintf("  A_perm: %d x %d, nnz = %d\n", nrow(A_perm), ncol(A_perm), length(A_perm@x)))

# HARD ASSERT TRIANGULARITY (T3 premise; if this fails the entire P1 design is invalid)
lower_off <- tril(A_perm, -1L)
upper_off <- triu(A_perm,  1L)
below_mass <- sum(abs(lower_off))
above_mass <- sum(abs(upper_off))
cat(sprintf("  Triangularity: below-diagonal mass = %.6e  |  above-diagonal mass = %.6e\n",
            below_mass, above_mass))

if (below_mass == 0 && above_mass > 0) {
  tri_orientation <- "upper"
  nnz_off <- length(upper_off@x)
  cat(sprintf("  ORIENTATION: A_perm is UPPER triangular\n"))
} else if (above_mass == 0 && below_mass > 0) {
  tri_orientation <- "lower"
  nnz_off <- length(lower_off@x)
  cat(sprintf("  ORIENTATION: A_perm is LOWER triangular\n"))
} else {
  stop(sprintf(
    paste0("TRIANGULARITY ASSERT FAILED (T3 violated).\n",
           "  below-diagonal mass = %.6e\n",
           "  above-diagonal mass = %.6e\n",
           "  The aging-backbone preconditioner design is invalid. Report to architect."),
    below_mass, above_mass))
}
nnz_diag <- sum(diag(A_perm) != 0)
cat(sprintf("  A_perm nnz: total = %d, diagonal = %d, off-diagonal (%s) = %d\n",
            length(A_perm@x), nnz_diag, tri_orientation, nnz_off))
uplo_flag <- if (tri_orientation == "upper") "U" else "L"

# === SECTION: BUILD P1 PRECONDITIONER (step 2) ===
cat("\n=== SECTION: BUILD P1 PRECONDITIONER ===\n")
# N_pc = I - beta * M_maintain  where M_maintain = diag(p_maint) %*% (I_4 kron A_age).
# In the permuted order, A_perm is triangular => T_G = I - beta * diag(p_maint_G[ord]) %*% A_perm
# is also triangular (scaling rows of a triangular matrix preserves triangularity).
# Stored as dtCMatrix so Matrix::solve() uses triangular substitution (not LU).
t0_precond_build <- proc.time()["elapsed"]

T_G_list <- vector("list", N_G)
for (G in seq_len(N_G)) {
  p_maint_G <- P_hm[["0,0"]][, G]                          # length C; maintain CCP for this G
  # T_G = I - beta * Diagonal(p_maint_G[ord]) %*% A_perm
  T_G_raw   <- as(
    Diagonal(C) - beta * Diagonal(x = p_maint_G[ord]) %*% A_perm,
    "CsparseMatrix"
  )
  # Construct dtCMatrix directly (triangularity asserted above; bypass Matrix's check)
  T_G_list[[G]] <- new("dtCMatrix",
                       i    = T_G_raw@i,
                       p    = T_G_raw@p,
                       x    = T_G_raw@x,
                       Dim  = T_G_raw@Dim,
                       uplo = uplo_flag,
                       diag = "N")
}

precond_build_sec <- proc.time()["elapsed"] - t0_precond_build
cat(sprintf("  P1 build: 4 T_G matrices (uplo='%s'), %.4f sec\n", uplo_flag, precond_build_sec))

# P1 preconditioner apply: solve T_G y = r_G for each G-block
Minv_P1 <- function(r) {
  R_mat <- matrix(r, nrow = C, ncol = N_G)   # reshape; column-major = G-blocks
  Y_mat <- matrix(0.0, nrow = C, ncol = N_G)
  for (G in seq_len(N_G)) {
    b             <- R_mat[ord, G]            # permute into triangular order
    yG            <- as.numeric(Matrix::solve(T_G_list[[G]], b))  # triangular substitution
    Y_mat[ord, G] <- yG                       # un-permute (ord maps permuted -> original)
  }
  as.vector(Y_mat)
}

# Time the first Minv_P1 call — confirms triangular substitution (not LU; LU would be ~seconds)
cat("  Timing first Minv_P1 call (hidden LU would be ~seconds; substitution is ~ms)...\n")
r_test      <- rnorm(N_SIDX)
t0_first    <- proc.time()["elapsed"]
invisible(Minv_P1(r_test))
first_solve_ms <- (proc.time()["elapsed"] - t0_first) * 1e3
cat(sprintf("  First solve: %.1f ms\n", first_solve_ms))

# Median of 5 subsequent calls
minv_times_ms <- numeric(5L)
for (ti in seq_len(5L)) {
  t0 <- proc.time()["elapsed"]
  invisible(Minv_P1(rnorm(N_SIDX)))
  minv_times_ms[ti] <- (proc.time()["elapsed"] - t0) * 1e3
}
minv_ms <- median(minv_times_ms)
cat(sprintf("  Minv_P1 times (ms): %s\n", paste(round(minv_times_ms, 1L), collapse = ", ")))
cat(sprintf("  Minv_P1 median:     %.1f ms\n", minv_ms))

# === SECTION: BASELINE BiCGSTAB (Minv = identity; self-check vs PM05 ~152 cold) ===
cat("\n=== SECTION: BASELINE BiCGSTAB (Minv = identity) ===\n")
cat("  Cold (x0 = 0)...\n")
t0_base_cold        <- proc.time()["elapsed"]
bicg_base_cold      <- bicgstab_pc(A_x_cpp, R_g5, Minv_identity, tol = 1e-9, max_it = 2000L)
sec_base_cold       <- proc.time()["elapsed"] - t0_base_cold
iters_baseline_cold <- bicg_base_cold$iters
V_baseline          <- bicg_base_cold$x
cat(sprintf("  Baseline cold: %d iters, %.4f sec\n", iters_baseline_cold, sec_base_cold))

# Hard assert: Minv=identity must match PM05 iteration range (oracle ~152)
stopifnot(iters_baseline_cold >= 120L && iters_baseline_cold <= 200L)
cat(sprintf("  Baseline cold iters = %d within [120, 200] (PM05 oracle: ~152) [OK]\n",
            iters_baseline_cold))

cat("  Warm (x0 = V_cold * (1 + 0.01*rnorm))...\n")
set.seed(77L)
x0_warm_base        <- V_baseline * (1 + 0.01 * rnorm(N_SIDX))
t0_base_warm        <- proc.time()["elapsed"]
bicg_base_warm      <- bicgstab_pc(A_x_cpp, R_g5, Minv_identity, tol = 1e-9, max_it = 2000L,
                                   x0 = x0_warm_base)
sec_base_warm       <- proc.time()["elapsed"] - t0_base_warm
iters_baseline_warm <- bicg_base_warm$iters
cat(sprintf("  Baseline warm: %d iters, %.4f sec\n", iters_baseline_warm, sec_base_warm))

# Validate baseline residual (used as oracle V for step 4b)
resid_baseline <- max(abs(A_x_cpp(V_baseline) - R_g5)) / max(abs(R_g5))
cat(sprintf("  Baseline residual: %.3e (tol 1e-8)\n", resid_baseline))
stopifnot(resid_baseline < 1e-8)
cat("  Baseline residual PASS\n")

# === SECTION: P1 PRECONDITIONED BiCGSTAB (step 3) ===
cat("\n=== SECTION: P1 PRECONDITIONED BiCGSTAB ===\n")
cat("  P1 cold (x0 = 0)...\n")
t0_p1_cold    <- proc.time()["elapsed"]
bicg_p1_cold  <- bicgstab_pc(A_x_cpp, R_g5, Minv_P1, tol = 1e-9, max_it = 2000L)
sec_p1_cold   <- proc.time()["elapsed"] - t0_p1_cold
iters_p1_cold <- bicg_p1_cold$iters
V_p1          <- bicg_p1_cold$x
cat(sprintf("  P1 cold: %d iters, %.4f sec\n", iters_p1_cold, sec_p1_cold))

cat("  P1 warm (x0 = V_p1 * (1 + 0.01*rnorm))...\n")
set.seed(77L)
x0_warm_p1    <- V_p1 * (1 + 0.01 * rnorm(N_SIDX))
t0_p1_warm    <- proc.time()["elapsed"]
bicg_p1_warm  <- bicgstab_pc(A_x_cpp, R_g5, Minv_P1, tol = 1e-9, max_it = 2000L,
                              x0 = x0_warm_p1)
sec_p1_warm   <- proc.time()["elapsed"] - t0_p1_warm
iters_p1_warm <- bicg_p1_warm$iters
cat(sprintf("  P1 warm: %d iters, %.4f sec\n", iters_p1_warm, sec_p1_warm))

# === SECTION: VALIDATE P1 (steps 4a, 4b, 4c) ===
cat("\n=== SECTION: VALIDATE P1 ===\n")

# 4a: Residual
resid_p1 <- max(abs(A_x_cpp(V_p1) - R_g5)) / max(abs(R_g5))
cat(sprintf("  4a Residual: ||A V_p1 - R||_inf / ||R||_inf = %.3e (tol 1e-8)\n", resid_p1))
stopifnot(resid_p1 < 1e-8)
cat("  4a PASS\n")

# 4b: Agreement with baseline oracle
agree_p1_baseline <- max(abs(V_p1 - V_baseline))
cat(sprintf("  4b Agreement: max|V_p1 - V_baseline| = %.3e (tol 1e-6)\n", agree_p1_baseline))
stopifnot(agree_p1_baseline < 1e-6)
cat("  4b PASS\n")

# 4c: G4 analytic — dedicated C++ op with p_maint=1 + dedicated preconditioner.
# When P_maintain = {"0,0"=1, "X"=0}, M = M_maintain exactly, so P1 is the exact
# inverse of the G4 operator => should converge in exactly 1 iteration.
cat("  4c G4 analytic (dedicated G4 op + G4 preconditioner with p_maint=1)...\n")

P_list_g4 <- lapply(seq_len(n_act), function(i) {
  akey <- paste0(k_vec[i], ",", m_vec[i])
  if (akey == "0,0") matrix(1.0, C, N_G) else matrix(0.0, C, N_G)
})
cpp_op_g4 <- pm_op_build(kernel$A_age, imap0, rmap0, k_vec, m_vec, GaT_list, P_list_g4, C, N_G)
A_x_g4    <- function(x) x - beta * as.numeric(pm_op_mv(cpp_op_g4, x))

# G4 preconditioner: p_maint = 1.0 everywhere => T_G_g4 = I - beta * A_perm (same for all G)
T_G_g4_raw <- as(Diagonal(C) - beta * A_perm, "CsparseMatrix")
T_G_g4     <- new("dtCMatrix",
                  i    = T_G_g4_raw@i,
                  p    = T_G_g4_raw@p,
                  x    = T_G_g4_raw@x,
                  Dim  = T_G_g4_raw@Dim,
                  uplo = uplo_flag,
                  diag = "N")

Minv_g4 <- function(r) {
  R_mat <- matrix(r, nrow = C, ncol = N_G)
  Y_mat <- matrix(0.0, nrow = C, ncol = N_G)
  for (G in seq_len(N_G)) {
    b             <- R_mat[ord, G]
    yG            <- as.numeric(Matrix::solve(T_G_g4, b))
    Y_mat[ord, G] <- yG
  }
  as.vector(Y_mat)
}

r_g4        <- 3.0
R_g4        <- rep(r_g4, N_SIDX)
analytic_g4 <- r_g4 / (1 - beta)

t0_g4_p1   <- proc.time()["elapsed"]
bicg_g4_p1 <- bicgstab_pc(A_x_g4, R_g4, Minv_g4, tol = 1e-9, max_it = 2000L, x0 = NULL)
sec_g4_p1  <- proc.time()["elapsed"] - t0_g4_p1
g4_dev_p1  <- max(abs(bicg_g4_p1$x - analytic_g4))
g4_iters_p1 <- bicg_g4_p1$iters
cat(sprintf("  analytic V = %.6f, max|V - V_analytic| = %.3e (tol 1e-7), iters = %d, sec = %.2f\n",
            analytic_g4, g4_dev_p1, g4_iters_p1, sec_g4_p1))
stopifnot(g4_dev_p1 < 1e-7)
if (g4_iters_p1 == 1L) {
  cat("  G4 converged in 1 iter — triangular substitution is exact inverse for G4 [CLEAN SIGNAL]\n")
} else {
  cat(sprintf("  NOTE: G4 converged in %d iters (expected 1); check T_G_g4 construction\n",
              g4_iters_p1))
}
cat("  4c PASS\n")

# === SECTION: BENCHMARK + RECOMMENDATION ===
cat("\n=== SECTION: BENCHMARK + RECOMMENDATION ===\n")

n_env   <- 17L
n_evals <- 100L
# Projection: per (NPL-iter, env): build T_G once + n_evals warm-start Krylov solves
proj_p1_per_iter_hr <- n_env * (precond_build_sec + n_evals * sec_p1_warm) / 3600

cat("─────────────────────────────────────────────────────────────\n")
cat("BENCHMARK SUMMARY\n")
cat(sprintf("  N_SIDX=%d  C=%d  N_G=%d  beta=%.4f\n", N_SIDX, C, N_G, beta))
cat(sprintf("  Triangularity:     A_perm is %s triangular [ASSERTED]\n", tri_orientation))
cat(sprintf("  A_perm nnz:        total=%d  diagonal=%d  off-diagonal(%s)=%d\n",
            length(A_perm@x), nnz_diag, tri_orientation, nnz_off))
cat(sprintf("  precond_build_sec: %.4f\n", precond_build_sec))
cat(sprintf("  Minv_P1 median:    %.1f ms\n", minv_ms))
cat(sprintf("  first_solve_ms:    %.1f ms  (vs median %.1f ms — large gap => hidden LU)\n",
            first_solve_ms, minv_ms))

cat("\n  Baseline (Minv=identity):\n")
cat(sprintf("    cold:  %d iters  %.4f sec\n", iters_baseline_cold, sec_base_cold))
cat(sprintf("    warm:  %d iters  %.4f sec\n", iters_baseline_warm, sec_base_warm))

cat("\n  P1 Preconditioned:\n")
cat(sprintf("    cold:  %d iters  %.4f sec\n", iters_p1_cold, sec_p1_cold))
cat(sprintf("    warm:  %d iters  %.4f sec\n", iters_p1_warm, sec_p1_warm))
cat(sprintf("    iter reduction (cold): %.1fx  (target: baseline/30 ~ %.1fx)\n",
            iters_baseline_cold / max(1L, iters_p1_cold),
            iters_baseline_cold / 30.0))
cat(sprintf("    iter reduction (warm): %.1fx\n",
            iters_baseline_warm / max(1L, iters_p1_warm)))

cat("\n  Validation:\n")
cat(sprintf("    resid_baseline    = %.3e (tol 1e-8)\n", resid_baseline))
cat(sprintf("    resid_p1          = %.3e (tol 1e-8)\n", resid_p1))
cat(sprintf("    agree_p1_baseline = %.3e (tol 1e-6)\n", agree_p1_baseline))
cat(sprintf("    g4_dev_p1         = %.3e (tol 1e-7), iters = %d (expected 1)\n",
            g4_dev_p1, g4_iters_p1))

cat(sprintf("\n  Projection (P1): %.3f hr/NPL-iter  (17 envs x (build + 100 warm-Krylov))\n",
            proj_p1_per_iter_hr))
cat(sprintf("  Baseline unpreconditioned (from 024c):   49.2 hr/NPL-iter\n"))

cat("\n─────────────────────────────────────────────────────────────\n")
cat("DECISION RULE AND RECOMMENDATION\n\n")

tri_ok     <- TRUE                           # asserted above; reaching here means PASS
resid_ok   <- resid_p1 < 1e-8
agree_ok   <- agree_p1_baseline < 1e-6
g4_ok      <- g4_dev_p1 < 1e-7
iters_ok   <- iters_p1_cold <= 50L          # target ~10-30; MISS if > 50
# First-solve time check: triangular substitution should be ~one-matvec (~few hundred ms max).
# A ratio first/median > 50 indicates Matrix did LU on first call.
solve_ok   <- (first_solve_ms / max(minv_ms, 1.0)) < 50.0

p1_pass <- tri_ok && resid_ok && agree_ok && g4_ok && iters_ok && solve_ok

if (p1_pass) {
  recommendation <- "P1 PASS — recommend P1 for 024b"
  cat("  RESULT: P1 PASS\n\n")
  cat(sprintf("  cold iters %d <= 50 [OK]\n", iters_p1_cold))
  cat("  Triangularity: PASS\n")
  cat("  Correctness 4a-4c: PASS\n")
  cat("  Triangular substitution cost: OK (no hidden LU)\n\n")
  cat("  RECOMMEND: Use P1 in 024b.\n")
  cat("  Design for 024b:\n")
  cat("    - Build 4 T_G once per (NPL-iter, env) [near-free; reuse A_perm + ord].\n")
  cat("    - Apply Minv_P1 inside preconditioned BiCGSTAB (right-preconditioned).\n")
  cat("    - Warm-start x0 across optim evals within the same (NPL-iter, env).\n")
  cat(sprintf("  Projected cost: %.3f hr/NPL-iter (vs 49.2 hr unpreconditioned).\n",
              proj_p1_per_iter_hr))
} else {
  recommendation <- "P1 MISS — escalate to ILU(0) fallback ticket 024e"
  cat("  RESULT: P1 MISS\n\n")
  if (!iters_ok)
    cat(sprintf("  FAIL: cold iters %d > 50 threshold\n", iters_p1_cold))
  if (!solve_ok)
    cat(sprintf("  FAIL: first_solve/median ratio %.1fx > 50 — likely hidden LU factorization\n",
                first_solve_ms / max(minv_ms, 1.0)))
  if (!resid_ok)
    cat(sprintf("  FAIL: resid_p1 %.3e >= 1e-8\n", resid_p1))
  if (!agree_ok)
    cat(sprintf("  FAIL: agree_p1_baseline %.3e >= 1e-6\n", agree_p1_baseline))
  if (!g4_ok)
    cat(sprintf("  FAIL: g4_dev_p1 %.3e >= 1e-7\n", g4_dev_p1))
  cat("\n  ESCALATE to 024e: ILU(0) fallback.\n")
  cat("  Form M once per (NPL-iter, env) [134 s, amortized over ~100 evals];\n")
  cat("  ILU(0) factorize (breakdown-free by T2); apply two triangular solves per iter.\n")
  cat("  Do NOT build 024b on the unpreconditioned 49 hr/iter solve.\n")
}
cat("─────────────────────────────────────────────────────────────\n")

# === SECTION: SAVE ===
cat("\n=== SECTION: SAVE ===\n")
out_path <- here::here("Output", "Estimation_Results", "PM_Precond_Bakeoff.rds")

bakeoff <- list(
  iters_baseline_cold      = iters_baseline_cold,
  iters_baseline_warm      = iters_baseline_warm,
  iters_p1_cold            = iters_p1_cold,
  iters_p1_warm            = iters_p1_warm,
  minv_ms                  = minv_ms,
  precond_build_sec        = precond_build_sec,
  resid_baseline           = resid_baseline,
  resid_p1                 = resid_p1,
  agree_p1_baseline        = agree_p1_baseline,
  g4_dev_p1                = g4_dev_p1,
  g4_iters_p1              = g4_iters_p1,
  proj_hr_per_npl_iter_p1  = proj_p1_per_iter_hr,
  triangular_orientation   = tri_orientation,
  recommendation           = recommendation,
  sec_base_cold            = sec_base_cold,
  sec_base_warm            = sec_base_warm,
  sec_p1_cold              = sec_p1_cold,
  sec_p1_warm              = sec_p1_warm,
  first_solve_ms           = first_solve_ms,
  nnz_A_perm               = length(A_perm@x),
  nnz_off_A_perm           = nnz_off
)

saveRDS(bakeoff, out_path)
cat(sprintf("  Saved: %s\n", out_path))

cat("\nPM07 COMPLETE\n")
