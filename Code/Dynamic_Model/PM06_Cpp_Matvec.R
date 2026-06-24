# PM06_Cpp_Matvec.R
# Ticket 024c: C++ structured matvec port + validation + BiCGSTAB re-benchmark.
# Run via: C:/Program Files/R/R-4.5.2/bin/x64/Rscript.exe

suppressPackageStartupMessages({
  library(Matrix)
  library(data.table)
  library(Rcpp)
  library(here)
})

# === LOGGING ===
.log_path <- here::here("logs", paste0("PM06_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".log"))
dir.create(dirname(.log_path), recursive = TRUE, showWarnings = FALSE)
.log <- file(.log_path, open = "wt")
sink(.log, type = "output"); sink(.log, type = "message", append = TRUE)
on.exit({ sink(type = "output"); sink(type = "message"); close(.log) }, add = TRUE)
cat(sprintf("LOG START %s\nScript: PM06_Cpp_Matvec.R\nR: %s\nWD: %s\n\n",
    .log_path, R.version.string, getwd()))

# === SECTION: E1/E2 — TOOLCHAIN ASSERTIONS ===
cat("=== SECTION: E1/E2 — TOOLCHAIN ASSERTIONS ===\n")
r_exe <- "C:/Program Files/R/R-4.5.2/bin/x64/Rscript.exe"
stopifnot(file.exists(r_exe))
cat(sprintf("  R binary (E1): %s [OK]\n", r_exe))
stopifnot(nzchar(Sys.which("make")), nzchar(Sys.which("g++")))
cat(sprintf("  make (E2): %s\n", Sys.which("make")))
cat(sprintf("  g++  (E2): %s\n", Sys.which("g++")))
cat("E1/E2 PASS\n")

# === SECTION: BUILD-PREFLIGHT GATE ===
cat("\n=== SECTION: BUILD-PREFLIGHT GATE ===\n")
preflight_log <- here::here("logs",
  paste0("PM_cpp_preflight_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".log"))

cpp_path <- here::here("Code", "Helpers", "pm_matvec.cpp")
stopifnot(file.exists(cpp_path))
cat(sprintf("  Compiling: %s\n", cpp_path))
t0_compile <- proc.time()["elapsed"]
Rcpp::sourceCpp(cpp_path)
compile_sec <- proc.time()["elapsed"] - t0_compile
cat(sprintf("  Compile time: %.1f sec\n", compile_sec))

probe_result <- pm_probe_arma(3.0, 4.0)
stopifnot(abs(probe_result - 7.0) < 1e-15)
cat(sprintf("  Probe pm_probe_arma(3,4) = %.1f [OK]\n", probe_result))

writeLines(c(
  "BUILD-PREFLIGHT PASS",
  sprintf("R: %s",            R.version.string),
  sprintf("make: %s",         Sys.which("make")),
  sprintf("g++: %s",          Sys.which("g++")),
  sprintf("compile_sec: %.1f", compile_sec),
  sprintf("probe: %.1f",       probe_result),
  sprintf("time: %s",          format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
), preflight_log)
cat(sprintf("  Preflight log: %s\n", preflight_log))
cat("BUILD-PREFLIGHT PASS\n")

# === SECTION: LOAD INPUTS ===
cat("\n=== SECTION: LOAD INPUTS ===\n")
source(here::here("Code", "Helpers", "pm_bellman_kernel.R"))  # gamma_E, sigma, build_kernel, Mv

ss <- readRDS(here::here("Output", "Estimation_Results", "PM_StateSpace.rds"))
lk <- readRDS(here::here("Output", "Estimation_Results", "PM_Lookups.rds"))

beta <- lk$BETA
stopifnot(beta == 0.9957)

kernel <- build_kernel(ss, lk)
C      <- kernel$C
N_G    <- kernel$N_G
N_SIDX <- ss$N_SIDX
stopifnot(N_SIDX == C * N_G)
cat(sprintf("C=%d  N_G=%d  N_SIDX=%d  beta=%.4f\n", C, N_G, N_SIDX, beta))

# === SECTION: BUILD P_hm (verbatim from PM05_Solver_Bakeoff.R lines 42-85) ===
cat("\n=== SECTION: BUILD P_hm ===\n")
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
cat(sprintf("  P_hm: %d actions, row-sum check OK\n", length(P_hm)))

# === SECTION: BUILD R_g5 (RB-2006, verbatim from PM05_Solver_Bakeoff.R lines 87-120) ===
cat("\n=== SECTION: BUILD R_g5 (RB-2006) ===\n")
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

# === SECTION: BUILD R_OH (OH FF environment) ===
cat("\n=== SECTION: BUILD R_OH (OH FF) ===\n")
# FF fee: flat per-tank annual fee tau_OH; deductible D_OH.
# gp*P_idx replaced by -tau_OH*N_vec (per-portfolio total fee in model units).
tau_OH <- lk$tau["OH"]
D_OH   <- lk$D["OH"]
cat(sprintf("  tau_OH=%.6f  D_OH=%.4f\n", tau_OH, D_OH))
stopifnot(is.finite(tau_OH), tau_OH > 0, is.finite(D_OH))

R_OH <- numeric(N_SIDX)
for (G in 1L:N_G) {
  sidx_G <- ((G - 1L) * C + 1L):(G * C)
  u_exit <- rep(-10.0, C)
  R_cG   <- numeric(C)
  for (i in seq_len(nrow(kernel$work_all))) {
    k     <- kernel$work_all$k[i]; m <- kernel$work_all$m[i]
    akey  <- paste0(k, ",", m)
    Pa_cG <- P_hm[[akey]][, G]
    u_cG  <- phi0 - tau_OH * ss$N_vec - gr * H_n * D_OH - k * cR - m * cI
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
  R_OH[sidx_G] <- R_cG
}
cat(sprintf("  R_OH range: [%.4f, %.4f]\n", min(R_OH), max(R_OH)))
stopifnot(all(is.finite(R_OH)))

# === SECTION: PREP C++ OPERATOR ===
cat("\n=== SECTION: PREP C++ OPERATOR ===\n")

# Convert imap/rmap to 0-based, NA -> -1L (load-bearing: mirrors R's NA guard in comp_apply)
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

# Precompute transposed Gmat per action (so C++ does Vmat * GaT, no per-call transpose)
GaT_list <- vector("list", n_act)
for (i in seq_len(n_act)) {
  nb          <- netbin(k_vec[i], m_vec[i])
  GaT_list[[i]] <- t(kernel$Gmat[[nb]])
}

# P list in work_all order (WORK actions only; "X" excluded from Mv per kernel spec)
P_list_cpp <- vector("list", n_act)
for (i in seq_len(n_act)) {
  akey <- paste0(k_vec[i], ",", m_vec[i])
  P_list_cpp[[i]] <- if (akey %in% names(P_hm)) P_hm[[akey]] else matrix(0.0, C, N_G)
}

cat(sprintf("  n_actions=%d  nnz(A_age)=%d\n", n_act, length(kernel$A_age@x)))

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
build_sec <- proc.time()["elapsed"] - t0_build
cat(sprintf("  Operator built in %.2f sec\n", build_sec))

# === SECTION: VALIDATION — C++ Mv == R Mv to 1e-12 ===
cat("\n=== SECTION: VALIDATION (C++ Mv vs R oracle, 5 random V) ===\n")
set.seed(42L)
max_devs <- numeric(5L)
for (vi in 1L:5L) {
  Vmat_r <- matrix(rnorm(C * N_G), nrow = C, ncol = N_G)
  r_out   <- as.vector(Mv(kernel, P_hm, Vmat_r))
  cpp_out <- as.numeric(pm_op_mv(cpp_op, as.vector(Vmat_r)))
  dev     <- max(abs(cpp_out - r_out))
  max_devs[vi] <- dev
  cat(sprintf("  V%d: max|cpp_Mv - R_Mv| = %.3e\n", vi, dev))
  stopifnot(dev < 1e-12)
}
cat(sprintf("  VALIDATION PASS: max over 5 draws = %.3e (tol 1e-12)\n", max(max_devs)))

# === SECTION: MATVEC TIMING ===
cat("\n=== SECTION: MATVEC TIMING ===\n")
set.seed(7L)
x_bench <- as.vector(matrix(rnorm(C * N_G), C, N_G))

mv_times_ms <- numeric(5L)
for (ti in 1L:5L) {
  t0 <- proc.time()["elapsed"]
  invisible(pm_op_mv(cpp_op, x_bench))
  mv_times_ms[ti] <- (proc.time()["elapsed"] - t0) * 1e3
}
mv_ms <- median(mv_times_ms)
cat(sprintf("  C++ Mv times (ms): %s\n", paste(round(mv_times_ms, 1), collapse = ", ")))
cat(sprintf("  C++ Mv median:     %.1f ms\n", mv_ms))

t0_r_mv <- proc.time()["elapsed"]
invisible(Mv(kernel, P_hm, matrix(x_bench, nrow = C)))
r_mv_ms <- (proc.time()["elapsed"] - t0_r_mv) * 1e3
cat(sprintf("  R   Mv:            %.1f ms\n", r_mv_ms))
cat(sprintf("  Speedup:           %.1fx\n", r_mv_ms / mv_ms))

if (mv_ms > 30) {
  cat("  WARN: C++ Mv median > 30ms; check for per-call arma rebuild or copy path.\n")
} else {
  cat("  C++ Mv is within target range (<=30ms).\n")
}

# === SECTION: BiCGSTAB (verbatim from PM05_Solver_Bakeoff.R lines 334-391) ===
cat("\n=== SECTION: BiCGSTAB definition ===\n")
# verbatim from PM05_Solver_Bakeoff.R:334-391
bicgstab <- function(Ax_fn, b, tol = 1e-9, max_it = 2000L, x0 = NULL) {
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
cat("  bicgstab() defined [OK]\n")

# C++ A_x operator
A_x_cpp <- function(x) x - beta * as.numeric(pm_op_mv(cpp_op, x))

# === SECTION: G4 ANALYTIC CHECK (C++ Mv, cold solve) ===
cat("\n=== SECTION: G4 ANALYTIC CHECK ===\n")
# Build C++ operator with P_maintain ("0,0" = 1 everywhere, rest = 0)
P_list_g4 <- lapply(seq_len(n_act), function(i) {
  akey <- paste0(k_vec[i], ",", m_vec[i])
  if (akey == "0,0") matrix(1.0, C, N_G) else matrix(0.0, C, N_G)
})
cpp_op_g4 <- pm_op_build(kernel$A_age, imap0, rmap0, k_vec, m_vec, GaT_list, P_list_g4, C, N_G)
A_x_g4 <- function(x) x - beta * as.numeric(pm_op_mv(cpp_op_g4, x))

r_g4          <- 3.0
R_g4          <- rep(r_g4, N_SIDX)
analytic_g4   <- r_g4 / (1 - beta)

t0_g4       <- proc.time()["elapsed"]
bicg_g4     <- bicgstab(A_x_g4, R_g4, tol = 1e-9, max_it = 2000L, x0 = NULL)
t_g4        <- proc.time()["elapsed"] - t0_g4
max_dev_g4  <- max(abs(bicg_g4$x - analytic_g4))
cat(sprintf("  analytic V=%.6f  max|V-V_analytic|=%.3e (tol 1e-7)  iters=%d  sec=%.2f\n",
            analytic_g4, max_dev_g4, bicg_g4$iters, t_g4))
stopifnot(max_dev_g4 < 1e-7)
cat("  G4 PASS\n")

# === SECTION: BICGSTAB RB-2006 (cold + warm) ===
cat("\n=== SECTION: BICGSTAB RB-2006 COLD + WARM ===\n")
cat("  Cold (x0=0)...\n")
t0_cold      <- proc.time()["elapsed"]
bicg_cold    <- bicgstab(A_x_cpp, R_g5, tol = 1e-9, max_it = 2000L, x0 = NULL)
sec_cold     <- proc.time()["elapsed"] - t0_cold
V_cold       <- bicg_cold$x
resid_cold   <- max(abs(A_x_cpp(V_cold) - R_g5)) / max(abs(R_g5))
cat(sprintf("  Cold: iters=%d  sec=%.2f  resid=%.3e\n",
            bicg_cold$iters, sec_cold, resid_cold))
stopifnot(resid_cold < 1e-8)
cat("  Residual PASS (cold)\n")

set.seed(77L)
x0_warm      <- V_cold * (1 + 0.01 * rnorm(N_SIDX))
cat("  Warm (x0 = V_cold*(1+0.01*rnorm))...\n")
t0_warm      <- proc.time()["elapsed"]
bicg_warm    <- bicgstab(A_x_cpp, R_g5, tol = 1e-9, max_it = 2000L, x0 = x0_warm)
sec_warm     <- proc.time()["elapsed"] - t0_warm
resid_warm   <- max(abs(A_x_cpp(bicg_warm$x) - R_g5)) / max(abs(R_g5))
cat(sprintf("  Warm: iters=%d  sec=%.2f  resid=%.3e\n",
            bicg_warm$iters, sec_warm, resid_warm))
stopifnot(resid_warm < 1e-8)
cat("  Residual PASS (warm)\n")

# === SECTION: BICGSTAB OH FF (cold) ===
cat("\n=== SECTION: BICGSTAB OH FF COLD ===\n")
t0_oh    <- proc.time()["elapsed"]
bicg_oh  <- bicgstab(A_x_cpp, R_OH, tol = 1e-9, max_it = 2000L, x0 = NULL)
sec_oh   <- proc.time()["elapsed"] - t0_oh
resid_oh <- max(abs(A_x_cpp(bicg_oh$x) - R_OH)) / max(abs(R_OH))
cat(sprintf("  OH cold: iters=%d  sec=%.2f  resid=%.3e\n",
            bicg_oh$iters, sec_oh, resid_oh))
stopifnot(resid_oh < 1e-8)
cat("  Residual PASS (OH)\n")

# === SECTION: PROJECTION ===
cat("\n=== SECTION: PROJECTION ===\n")
n_env   <- 17L
n_evals <- 100L
proj_hr <- n_env * n_evals * sec_warm / 3600
cat(sprintf("  Projection: %d envs x %d evals x %.2fs warm = %.3f hr/NPL-iter\n",
            n_env, n_evals, sec_warm, proj_hr))
if (proj_hr < 1.0) {
  cat("  TARGET HIT: projected cost < 1 hr/NPL-iter -> 024b feasible.\n")
} else {
  cat(sprintf("  WARN: projected %.3f hr/NPL-iter > 1 hr; consider preconditioner.\n", proj_hr))
}

# === SECTION: SUMMARY ===
cat("\n─────────────────────────────────────────────────────────────\n")
cat("024c BENCHMARK SUMMARY\n")
cat(sprintf("  C=%d  N_G=%d  N_SIDX=%d  beta=%.4f  n_actions=%d\n",
            C, N_G, N_SIDX, beta, n_act))
cat(sprintf("  ARMA_NO_DEBUG: ON (validation via 1e-12 gate above)\n"))
cat(sprintf("\n  C++ Mv:   %.1f ms median (R: %.1f ms, speedup %.1fx)\n",
            mv_ms, r_mv_ms, r_mv_ms / mv_ms))
cat(sprintf("  G4 dev:   %.3e (tol 1e-7) -> %s\n",
            max_dev_g4, if (max_dev_g4 < 1e-7) "PASS" else "FAIL"))
cat(sprintf("\n  RB-2006 cold: iters=%-4d  sec=%.2f  resid=%.3e\n",
            bicg_cold$iters, sec_cold, resid_cold))
cat(sprintf("  RB-2006 warm: iters=%-4d  sec=%.2f  resid=%.3e\n",
            bicg_warm$iters, sec_warm, resid_warm))
cat(sprintf("  OH  FF  cold: iters=%-4d  sec=%.2f  resid=%.3e\n",
            bicg_oh$iters, sec_oh, resid_oh))
cat(sprintf("\n  Proj: %.3f hr/NPL-iter (%d envs x %d evals x %.2fs warm)\n",
            proj_hr, n_env, n_evals, sec_warm))
cat("─────────────────────────────────────────────────────────────\n")

# === SECTION: SAVE ===
cat("\n=== SECTION: SAVE ===\n")
benchmark <- list(
  # Matvec
  mv_ms              = mv_ms,
  mv_times_ms        = mv_times_ms,
  r_mv_ms            = r_mv_ms,
  speedup            = r_mv_ms / mv_ms,
  # G4
  max_dev_g4         = max_dev_g4,
  # RB-2006
  bicgstab_iters_cold = bicg_cold$iters,
  sec_cold            = sec_cold,
  resid_cold          = resid_cold,
  bicgstab_iters_warm = bicg_warm$iters,
  sec_warm            = sec_warm,
  resid_warm          = resid_warm,
  # OH FF
  bicgstab_iters_oh   = bicg_oh$iters,
  sec_oh              = sec_oh,
  resid_oh            = resid_oh,
  # Validation
  max_dev_validation  = max(max_devs),
  # Projection
  n_env               = n_env,
  n_evals             = n_evals,
  proj_hr_per_npl_iter = proj_hr,
  # Meta
  C = C, N_G = N_G, N_SIDX = N_SIDX, beta = beta, n_actions = n_act,
  build_sec  = build_sec,
  compile_sec = compile_sec,
  run_date   = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
)

out_path <- here::here("Output", "Estimation_Results", "PM_Cpp_Benchmark.rds")
saveRDS(benchmark, out_path)
cat(sprintf("Saved: %s\n", out_path))
cat(sprintf("\nAll sections complete. Log: %s\n", .log_path))
