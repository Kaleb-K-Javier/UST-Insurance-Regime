# PM05_Solver_Bakeoff.R
# Ticket 024s: Sparse LU (S1) vs BiCGSTAB (S2) value-solve bake-off.
# Validates both via residual + assembly + G4 analytic (no Anderson oracle).
# Run via: C:/Program Files/R/R-4.5.2/bin/x64/Rscript.exe

suppressPackageStartupMessages({
  library(Matrix)
  library(data.table)
  library(here)
})

# === LOGGING ===
.log_path <- here::here("logs", paste0("PM05_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".log"))
dir.create(dirname(.log_path), recursive = TRUE, showWarnings = FALSE)
.log <- file(.log_path, open = "wt")
sink(.log, type = "output"); sink(.log, type = "message", append = TRUE)
on.exit({ sink(type = "output"); sink(type = "message"); close(.log) }, add = TRUE)
cat(sprintf("LOG START %s\nScript: PM05_Solver_Bakeoff.R\nR: %s\nWD: %s\n\n",
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
stopifnot(N_SIDX == C * N_G)
cat(sprintf("C = %d, N_G = %d, N_SIDX = %d\n", C, N_G, N_SIDX))

# gamma_E and sigma sourced from pm_bellman_kernel.R
cat(sprintf("gamma_E = %.10f, sigma = %.1f\n", gamma_E, sigma))

# === SECTION: BUILD G5 TEST (P_hm, R_g5) — EXACT REUSE OF 024a G5 ===
cat("\n=== SECTION: BUILD G5 TEST (P_hm, R_g5) ===\n")

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

# Placeholder R (RB-2006 env, verbatim from 024a G5)
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

# A(x) operator for BiCGSTAB: (I - beta M) x
A_x <- function(x) {
  x - beta * as.vector(Mv(kernel, P_hm, matrix(x, nrow = C)))
}

# === SECTION: S1 — FORM M EXPLICITLY ===
cat("\n=== SECTION: S1 — FORM M EXPLICITLY ===\n")
cat(sprintf("  Building M (%d x %d) from %d work actions...\n",
            N_SIDX, N_SIDX, nrow(kernel$work_all)))

t0_form <- proc.time()["elapsed"]
M <- NULL

for (i in seq_len(nrow(kernel$work_all))) {
  k    <- kernel$work_all$k[i]
  m    <- kernel$work_all$m[i]
  akey <- paste0(k, ",", m)

  if (!akey %in% names(P_hm)) {
    cat(sprintf("  action (%d,%d): not in P_hm, skip\n", k, m))
    next
  }

  # R_k: output-side row-gather by rmap[, k+1]  (R_k[i, rmap[i,k+1]] = 1)
  if (k == 0L) {
    R_k <- Diagonal(C)
  } else {
    rmap_col <- kernel$rmap[, k + 1L]
    valid_k  <- which(!is.na(rmap_col))
    R_k <- sparseMatrix(i = valid_k, j = rmap_col[valid_k],
                        x = 1.0, dims = c(C, C), repr = "C")
  }

  # G_m: input-side column-relabel by imap[, m+1]  (G_m[l, imap[l,m+1]] = 1)
  if (m == 0L) {
    G_m <- Diagonal(C)
  } else {
    imap_col <- kernel$imap[, m + 1L]
    valid_m  <- which(!is.na(imap_col))
    G_m <- sparseMatrix(i = valid_m, j = imap_col[valid_m],
                        x = 1.0, dims = c(C, C), repr = "C")
  }

  # CompKer_a = R_k %*% A_age %*% G_m
  if (k == 0L && m == 0L) {
    CompKer_a <- kernel$A_age
  } else if (k == 0L) {
    CompKer_a <- kernel$A_age %*% G_m
  } else if (m == 0L) {
    CompKer_a <- R_k %*% kernel$A_age
  } else {
    CompKer_a <- R_k %*% kernel$A_age %*% G_m
  }
  CompKer_a <- as(CompKer_a, "dgCMatrix")

  # F_a = kronecker(Ga, CompKer_a): N_SIDX x N_SIDX, sidx = (G-1)*C + c
  nb  <- netbin(k, m)
  Ga  <- as(kernel$Gmat[[nb]], "dgCMatrix")
  F_a <- as(Matrix::kronecker(Ga, CompKer_a), "dgCMatrix")

  # M_a = Diag(p_vec) %*% F_a; as.vector(P[[a]]) column-major = sidx order
  p_vec <- as.vector(P_hm[[akey]])
  M_a   <- as(Diagonal(x = p_vec) %*% F_a, "dgCMatrix")

  M <- if (is.null(M)) M_a else as(M + M_a, "dgCMatrix")

  if (i %% 5L == 0L || i == nrow(kernel$work_all)) {
    cat(sprintf("  [%s] action %d/%d (%d,%d) done; stored nnz(M) = %d\n",
                format(Sys.time(), "%H:%M:%S"), i, nrow(kernel$work_all),
                k, m, length(M@x)))
  }
}

M <- drop0(as(M, "dgCMatrix"))
form_sec <- proc.time()["elapsed"] - t0_form
cat(sprintf("  M formed in %.2f sec\n", form_sec))

# === SECTION: S1 — REPORT NNZ, MEMORY, ROW SUMS ===
cat("\n=== SECTION: S1 — REPORT NNZ, MEMORY, ROW SUMS ===\n")

nnz_M     <- length(M@x)
size_M_gb <- as.numeric(object.size(M)) / 1e9
row_sums_M  <- as.vector(Matrix::rowSums(M))
max_row_sum <- max(row_sums_M)
min_row_sum <- min(row_sums_M)
cat(sprintf("  nnz(M)          = %d\n",   nnz_M))
cat(sprintf("  object.size(M)  = %.3f GB\n", size_M_gb))
cat(sprintf("  max row sum     = %.8f  (must be <= 1, sub-stochastic)\n", max_row_sum))
cat(sprintf("  min row sum     = %.8f\n", min_row_sum))
stopifnot(max_row_sum <= 1 + 1e-8)

A_mat <- as(Diagonal(N_SIDX) - beta * M, "dgCMatrix")
cat(sprintf("  A = I - beta*M: nnz = %d\n", length(A_mat@x)))

# === SECTION: S1 — FACTORIZE ===
cat("\n=== SECTION: S1 — FACTORIZE ===\n")

S1_infeasible        <- FALSE
S1_infeasible_reason <- ""
fac            <- NULL
factorize_sec  <- NA_real_
factor_obj_mb  <- NA_real_

gc()
t0_fac <- proc.time()["elapsed"]
tryCatch({
  fac           <- Matrix::lu(A_mat)
  factorize_sec <- proc.time()["elapsed"] - t0_fac
  gc()
  factor_obj_mb <- as.numeric(object.size(fac)) / 1e6
  cat(sprintf("  S1 FACTORIZE OK: %.2f sec, fac object %.1f MB\n",
              factorize_sec, factor_obj_mb))
}, error = function(e) {
  S1_infeasible        <<- TRUE
  S1_infeasible_reason <<- conditionMessage(e)
  factorize_sec        <<- proc.time()["elapsed"] - t0_fac
  cat(sprintf("  S1 INFEASIBLE: %s (%.2f sec before error)\n",
              S1_infeasible_reason, factorize_sec))
})

# === SECTION: S1 — SOLVE AND AMORTIZE ===
cat("\n=== SECTION: S1 — SOLVE AND AMORTIZE ===\n")

V_s1              <- NULL
backsub_sec_first <- NA_real_
backsub_sec_mean  <- NA_real_

if (!S1_infeasible) {
  t0_bs1 <- proc.time()["elapsed"]
  V_s1   <- as.vector(Matrix::solve(fac, R_g5))
  backsub_sec_first <- proc.time()["elapsed"] - t0_bs1
  cat(sprintf("  First back-sub: %.4f sec\n", backsub_sec_first))

  set.seed(42L)
  backsub_times <- numeric(10L)
  for (ii in 1L:10L) {
    R_i  <- R_g5 * (1 + 0.01 * rnorm(N_SIDX))
    t0_i <- proc.time()["elapsed"]
    invisible(Matrix::solve(fac, R_i))
    backsub_times[ii] <- proc.time()["elapsed"] - t0_i
  }
  backsub_sec_mean <- mean(backsub_times)
  cat(sprintf("  10-RHS back-sub (1%% relative perturb): mean=%.4f sec, sd=%.4f sec\n",
              backsub_sec_mean, sd(backsub_times)))

  refac_flag <- if (backsub_sec_mean > 0.5 * factorize_sec)
    "WARN: possible re-factorize (back-sub >= 0.5x factorize time)" else "OK"
  cat(sprintf("  Re-factorize check: first=%.4f vs fac=%.4f -> %s\n",
              backsub_sec_first, factorize_sec, refac_flag))
}

# === SECTION: S1 — VALIDATE ===
cat("\n=== SECTION: S1 — VALIDATE ===\n")

resid_s1    <- NA_real_
max_dev_g4_s1 <- NA_real_

if (!S1_infeasible && !is.null(V_s1)) {

  # (i) Assembly: M %*% v vs Mv(kernel, P_hm, v)  for 3 random v
  cat("  (i) Assembly check:\n")
  set.seed(99L)
  for (rep in 1L:3L) {
    v_test        <- rnorm(N_SIDX)
    mv_explicit   <- as.vector(M %*% v_test)
    mv_structured <- as.vector(Mv(kernel, P_hm, matrix(v_test, nrow = C)))
    max_asm_dev   <- max(abs(mv_explicit - mv_structured))
    cat(sprintf("    rep %d: max|M*v - Mv(v)| = %.3e\n", rep, max_asm_dev))
    stopifnot(max_asm_dev < 1e-10)
  }
  cat("  Assembly PASS\n")

  # (ii) Residual: ||A V_s1 - R||_inf / ||R||_inf < 1e-9
  resid_s1 <- max(abs(as.vector(A_mat %*% V_s1) - R_g5)) / max(abs(R_g5))
  cat(sprintf("  (ii) Residual: ||AV-R||_inf/||R||_inf = %.3e (tol 1e-9)\n", resid_s1))
  stopifnot(resid_s1 < 1e-9)
  cat("  Residual PASS\n")

  # (iii) G4 analytic via S1
  cat("  (iii) G4 analytic via S1:\n")
  r_val         <- 3.0
  R_g4          <- rep(r_val, N_SIDX)
  analytic_V_g4 <- r_val / (1 - beta)

  # M_g4 = kronecker(Gmat[["nowork"]], A_age); nowork = I_4 -> block-diagonal
  G_nowork <- as(kernel$Gmat[["nowork"]], "dgCMatrix")
  M_g4     <- as(Matrix::kronecker(G_nowork, kernel$A_age), "dgCMatrix")
  A_g4     <- as(Diagonal(N_SIDX) - beta * M_g4, "dgCMatrix")
  cat(sprintf("    M_g4 nnz=%d, size=%.1f MB\n",
              length(M_g4@x), as.numeric(object.size(M_g4)) / 1e6))

  t0_fac_g4  <- proc.time()["elapsed"]
  fac_g4     <- Matrix::lu(A_g4)
  fac_g4_sec <- proc.time()["elapsed"] - t0_fac_g4

  V_g4_s1      <- as.vector(Matrix::solve(fac_g4, R_g4))
  max_dev_g4_s1 <- max(abs(V_g4_s1 - analytic_V_g4))
  cat(sprintf("    analytic V = %.6f, max|V-V_analytic| = %.3e (tol 1e-7), fac=%.2f sec\n",
              analytic_V_g4, max_dev_g4_s1, fac_g4_sec))
  stopifnot(max_dev_g4_s1 < 1e-7)
  cat("  G4 via S1 PASS\n")
  cat("S1 VALIDATION PASS\n")

} else {
  cat(sprintf("  S1 INFEASIBLE — validation skipped (reason: %s)\n", S1_infeasible_reason))
}

# === SECTION: S2 — BICGSTAB ===
cat("\n=== SECTION: S2 — BICGSTAB ===\n")

# Standard van der Vorst (1992) BiCGSTAB; inf-norm stopping; hard error on stall
bicgstab <- function(Ax_fn, b, tol = 1e-9, max_it = 2000L, x0 = NULL) {
  n   <- length(b)
  x   <- if (!is.null(x0)) x0 else numeric(n)
  r   <- b - Ax_fn(x)
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

    bta <- (rho1 / rho) * (alpha / w)
    p   <- r + bta * (p - w * v)
    v   <- Ax_fn(p)

    denom_alpha <- sum(rhat * v)
    if (abs(denom_alpha) < 1e-100)
      stop(sprintf("BiCGSTAB stalled: <rhat,v>=%.3e at iter %d", denom_alpha, it))

    alpha <- rho1 / denom_alpha
    s     <- r - alpha * v

    if (max(abs(s)) < tol) {
      x <- x + alpha * p
      return(list(x = x, iters = it, converged = TRUE, resid = max(abs(s))))
    }

    t <- Ax_fn(s)

    denom_w <- sum(t * t)
    if (abs(denom_w) < 1e-100)
      stop(sprintf("BiCGSTAB stalled: <t,t>=%.3e at iter %d", denom_w, it))

    w   <- sum(t * s) / denom_w
    x   <- x + alpha * p + w * s
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

# Cold solve
cat("  BiCGSTAB cold (x0 = 0)...\n")
t0_cold      <- proc.time()["elapsed"]
bicg_cold    <- bicgstab(A_x, R_g5, tol = 1e-9, max_it = 2000L, x0 = NULL)
krylov_sec_cold   <- proc.time()["elapsed"] - t0_cold
krylov_iters_cold <- bicg_cold$iters
V_s2 <- bicg_cold$x
cat(sprintf("  Cold: %.4f sec, %d iters\n", krylov_sec_cold, krylov_iters_cold))

# Warm solve (x0 = V_cold * (1 + 0.01 * rnorm), 1% relative)
cat("  BiCGSTAB warm (x0 = V_cold * (1 + 0.01*rnorm))...\n")
set.seed(77L)
x0_warm      <- V_s2 * (1 + 0.01 * rnorm(N_SIDX))
t0_warm      <- proc.time()["elapsed"]
bicg_warm    <- bicgstab(A_x, R_g5, tol = 1e-9, max_it = 2000L, x0 = x0_warm)
krylov_sec_warm   <- proc.time()["elapsed"] - t0_warm
krylov_iters_warm <- bicg_warm$iters
cat(sprintf("  Warm: %.4f sec, %d iters\n", krylov_sec_warm, krylov_iters_warm))

# === SECTION: S2 — VALIDATE ===
cat("\n=== SECTION: S2 — VALIDATE ===\n")

# (i) Residual
Ax_V_s2  <- A_x(V_s2)
resid_s2 <- max(abs(Ax_V_s2 - R_g5)) / max(abs(R_g5))
cat(sprintf("  (i) Residual: ||AV-R||_inf/||R||_inf = %.3e (tol 1e-8)\n", resid_s2))
stopifnot(resid_s2 < 1e-8)
cat("  S2 Residual PASS\n")

# (ii) S1 vs S2 agreement
max_agree_s1_s2 <- NA_real_
if (!S1_infeasible && !is.null(V_s1)) {
  max_agree_s1_s2 <- max(abs(V_s2 - V_s1))
  cat(sprintf("  (ii) S1 vs S2: max|V_s1 - V_s2| = %.3e (tol 1e-6)\n", max_agree_s1_s2))
  stopifnot(max_agree_s1_s2 < 1e-6)
  cat("  S1-S2 Agreement PASS\n")
}

# (iii) G4 analytic via BiCGSTAB — cold (x0 = 0)
cat("  (iii) G4 analytic via S2 (cold, x0=0):\n")
r_val_g4      <- 3.0
R_g4          <- rep(r_val_g4, N_SIDX)
analytic_V_g4 <- r_val_g4 / (1 - beta)

P_maintain <- list(
  "0,0" = matrix(1.0, nrow = C, ncol = N_G),
  "X"   = matrix(0.0, nrow = C, ncol = N_G)
)
A_x_g4 <- function(x) {
  x - beta * as.vector(Mv(kernel, P_maintain, matrix(x, nrow = C)))
}

t0_g4_s2      <- proc.time()["elapsed"]
bicg_g4       <- bicgstab(A_x_g4, R_g4, tol = 1e-9, max_it = 2000L, x0 = NULL)
t_g4_s2       <- proc.time()["elapsed"] - t0_g4_s2
max_dev_g4_s2 <- max(abs(bicg_g4$x - analytic_V_g4))
cat(sprintf("    analytic V=%.6f, max|V-V_analytic|=%.3e (tol 1e-7), iters=%d, sec=%.2f\n",
            analytic_V_g4, max_dev_g4_s2, bicg_g4$iters, t_g4_s2))
stopifnot(max_dev_g4_s2 < 1e-7)
cat("  G4 via S2 PASS\n")
cat("S2 VALIDATION PASS\n")

# === SECTION: BENCHMARK TABLE AND RECOMMENDATION ===
cat("\n=== SECTION: BENCHMARK TABLE AND RECOMMENDATION ===\n")

n_env   <- 17L
n_evals <- 100L   # inner optim evals per NPL iter

proj_s1_per_iter_hr <- if (!S1_infeasible && !is.na(factorize_sec) && !is.na(backsub_sec_mean))
  n_env * (factorize_sec + n_evals * backsub_sec_mean) / 3600 else NA_real_

proj_s2_per_iter_hr <- n_env * n_evals * krylov_sec_warm / 3600

cat("─────────────────────────────────────────────────────────────\n")
cat("BENCHMARK SUMMARY\n")
cat(sprintf("  N_SIDX=%d  C=%d  N_G=%d  beta=%.4f\n", N_SIDX, C, N_G, beta))

cat("\n  S1 (Sparse LU):\n")
if (!S1_infeasible) {
  cat(sprintf("    nnz(M)           = %d\n",     nnz_M))
  cat(sprintf("    object.size(M)   = %.3f GB\n", size_M_gb))
  cat(sprintf("    max row sum      = %.8f\n",   max_row_sum))
  cat(sprintf("    form_sec         = %.2f\n",   form_sec))
  cat(sprintf("    factorize_sec    = %.2f\n",   factorize_sec))
  cat(sprintf("    fac object       = %.1f MB\n", factor_obj_mb))
  cat(sprintf("    backsub_1st_sec  = %.4f\n",   backsub_sec_first))
  cat(sprintf("    backsub_mean_sec = %.4f  (10 1%%-relative-perturbed RHS)\n", backsub_sec_mean))
  cat(sprintf("    assembly_dev     = <1e-10 (PASS)\n"))
  cat(sprintf("    residual_s1      = %.3e\n",   resid_s1))
  cat(sprintf("    G4_dev_s1        = %.3e\n",   max_dev_g4_s1))
  cat(sprintf("    proj/NPL-iter    = %.3f hr  (17 envs x (fac + 100 back-subs))\n",
              proj_s1_per_iter_hr))
} else {
  cat(sprintf("    STATUS: INFEASIBLE — %s\n", S1_infeasible_reason))
}

cat("\n  S2 (BiCGSTAB on structured Mv):\n")
cat(sprintf("    iters_cold       = %d\n",     krylov_iters_cold))
cat(sprintf("    sec_cold         = %.4f\n",   krylov_sec_cold))
cat(sprintf("    iters_warm       = %d\n",     krylov_iters_warm))
cat(sprintf("    sec_warm         = %.4f\n",   krylov_sec_warm))
cat(sprintf("    residual_s2      = %.3e\n",   resid_s2))
if (!is.na(max_agree_s1_s2))
  cat(sprintf("    S1_vs_S2_max     = %.3e\n", max_agree_s1_s2))
cat(sprintf("    G4_dev_s2        = %.3e\n",   max_dev_g4_s2))
cat(sprintf("    proj/NPL-iter    = %.3f hr  (17 envs x 100 warm-Krylov)\n",
            proj_s2_per_iter_hr))

cat("\n─────────────────────────────────────────────────────────────\n")
cat("024b RECOMMENDATION (decision rule: S1 fac<120s, mem<24GB, back-sub<2s -> S1)\n\n")

s1_ok <- (!S1_infeasible &&
           !is.na(factorize_sec) && factorize_sec < 120 &&
           !is.na(size_M_gb)     && size_M_gb     < 24  &&
           !is.na(backsub_sec_mean) && backsub_sec_mean < 2)

if (s1_ok) {
  cat("  RECOMMEND: S1 — SPARSE LU\n")
  cat(sprintf("  fac=%.2fs < 120s | mem=%.3fGB < 24GB | back-sub=%.4fs < 2s\n",
              factorize_sec, size_M_gb, backsub_sec_mean))
  cat("  Simplest, exact, best amortization. No C++ required.\n")
  cat("  024b design: factorize A once per (NPL-iter, env); back-sub per optim eval.\n")
  cat(sprintf("  Projected cost: %.3f hr/NPL-iter (17 envs, ~100 evals).\n",
              proj_s1_per_iter_hr))
} else {
  cat("  RECOMMEND: S2 — BICGSTAB\n")
  if (S1_infeasible) {
    cat(sprintf("  S1 infeasible: %s\n", S1_infeasible_reason))
  } else {
    if (!is.na(factorize_sec) && factorize_sec >= 120)
      cat(sprintf("  S1 fac=%.2fs >= 120s threshold.\n", factorize_sec))
    if (!is.na(size_M_gb) && size_M_gb >= 24)
      cat(sprintf("  S1 mem=%.3fGB >= 24GB threshold.\n", size_M_gb))
    if (!is.na(backsub_sec_mean) && backsub_sec_mean >= 2)
      cat(sprintf("  S1 back-sub=%.4fs >= 2s threshold.\n", backsub_sec_mean))
  }
  if (krylov_sec_cold > 10) {
    cat("  C++ VERDICT: Mv port to C++ REQUIRED (cold Krylov > 10s in R).\n")
    cat("  Action: port Mv() to Rcpp before writing 024b.\n")
  } else {
    cat("  C++ VERDICT: R-level Mv ADEQUATE (cold Krylov <= 10s).\n")
    cat("  Action: write 024b in R without C++ port.\n")
  }
  cat(sprintf("  Projected cost: %.3f hr/NPL-iter (17 envs x 100 warm-Krylov).\n",
              proj_s2_per_iter_hr))
}
cat("─────────────────────────────────────────────────────────────\n")

# === SECTION: SAVE ===
cat("\n=== SECTION: SAVE ===\n")

bakeoff <- list(
  # S1
  S1_infeasible        = S1_infeasible,
  S1_infeasible_reason = S1_infeasible_reason,
  nnz_M                = nnz_M,
  size_M_gb            = size_M_gb,
  max_row_sum          = max_row_sum,
  form_sec             = form_sec,
  factorize_sec        = factorize_sec,
  factor_obj_mb        = factor_obj_mb,
  backsub_sec_first    = backsub_sec_first,
  backsub_sec_mean     = backsub_sec_mean,
  resid_s1             = resid_s1,
  max_dev_g4_s1        = max_dev_g4_s1,
  # S2
  krylov_iters_cold    = krylov_iters_cold,
  krylov_sec_cold      = krylov_sec_cold,
  krylov_iters_warm    = krylov_iters_warm,
  krylov_sec_warm      = krylov_sec_warm,
  resid_s2             = resid_s2,
  max_agree_s1_s2      = max_agree_s1_s2,
  max_dev_g4_s2        = max_dev_g4_s2,
  # Projection
  n_env                = n_env,
  n_evals              = n_evals,
  proj_s1_per_iter_hr  = proj_s1_per_iter_hr,
  proj_s2_per_iter_hr  = proj_s2_per_iter_hr,
  recommendation       = if (s1_ok) "S1" else "S2",
  # Metadata
  beta                 = beta,
  N_SIDX               = N_SIDX,
  C                    = C,
  N_G                  = N_G,
  tol                  = 1e-9,
  run_date             = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
)

out_path <- here::here("Output", "Estimation_Results", "PM_Solver_Bakeoff.rds")
saveRDS(bakeoff, out_path)
cat(sprintf("Saved: %s\n", out_path))

cat(sprintf("\nAll sections complete. Log: %s\n", .log_path))
