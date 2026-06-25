# PM08_Estimator_v4.R
# Ticket 024b: Portfolio model v4 NPL estimator (lambda=1 flat-MNL milestone)
# Phases A-E written incrementally; run after each gate.
# Run: C:/Program Files/R/R-4.5.2/bin/x64/Rscript.exe Code/Dynamic_Model/PM08_Estimator_v4.R

suppressPackageStartupMessages({
  library(Matrix)
  library(data.table)
  library(here)
  library(Rcpp)
})

# === LOGGING ===
.log_path <- here::here("logs", paste0("PM08_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".log"))
dir.create(dirname(.log_path), recursive = TRUE, showWarnings = FALSE)
.log <- file(.log_path, open = "wt")
sink(.log, type = "output"); sink(.log, type = "message", append = TRUE)
on.exit({ sink(type = "output"); sink(type = "message"); close(.log) }, add = TRUE)
cat(sprintf("LOG START %s\nScript: PM08_Estimator_v4.R\nR: %s\nWD: %s\n\n",
    .log_path, R.version.string, getwd()))

# === SECTION: SOURCE KERNEL AND C++ MATVEC ===
cat("=== SECTION: SOURCE KERNEL AND C++ MATVEC ===\n")
source(here::here("Code", "Helpers", "pm_bellman_kernel.R"))
cat("  pm_bellman_kernel.R sourced [OK]\n")
cpp_path <- here::here("Code", "Helpers", "pm_matvec.cpp")
if (!file.exists(cpp_path)) stop(sprintf("pm_matvec.cpp not found: %s", cpp_path))
Rcpp::sourceCpp(cpp_path)
cat("  pm_matvec.cpp sourced [OK]\n")
USE_BATCHED_MV <- !nzchar(Sys.getenv("PM08_NO_BATCHED"))   # default ON; set PM08_NO_BATCHED=1 to use old op
use_pooled     <- nzchar(Sys.getenv("PM08_POOLED"))         # PM08_POOLED=1 -> pooled total exposure + RB interaction (T024q)
cat(sprintf("  USE_BATCHED_MV: %s\n", USE_BATCHED_MV))
cat(sprintf("  USE_POOLED:     %s\n", use_pooled))

# === SECTION: LOAD INPUTS ===
cat("\n=== SECTION: LOAD INPUTS ===\n")
ss  <- readRDS(here::here("Output", "Estimation_Results", "PM_StateSpace.rds"))
lk  <- readRDS(here::here("Output", "Estimation_Results", "PM_Lookups.rds"))
agg <- fread(here::here("Data", "Analysis", "pm_agg_counts.csv"))
cat(sprintf("  pm_agg_counts: %d rows, cols: %s\n", nrow(agg), paste(names(agg), collapse = ",")))

stopifnot(all(as.character(agg$era) %in% c("2006", "2014", "2019")))
cat("  era in {2006,2014,2019} [OK]\n")

beta     <- lk$BETA
eps_prob <- 1e-10
stopifnot(beta == 0.9957)

kernel <- build_kernel(ss, lk)
C      <- kernel$C
N_G    <- kernel$N_G
N_SIDX <- ss$N_SIDX
stopifnot(N_SIDX == C * N_G)
cat(sprintf("  C=%d, N_G=%d, N_SIDX=%d, beta=%.4f\n", C, N_G, N_SIDX, beta))

n_act    <- nrow(kernel$work_all)
k_vec    <- as.integer(kernel$work_all$k)
m_vec    <- as.integer(kernel$work_all$m)
act_keys <- vapply(seq_len(n_act), function(i) paste0(k_vec[i], ",", m_vec[i]), character(1L))
cat(sprintf("  n_act=%d work actions: %s\n", n_act, paste(act_keys, collapse = " ")))

# === SECTION A0-A2: POST_CM AND FEASIBILITY MASK ===
cat("\n=== SECTION A0-A2: POST_CM AND FEASIBILITY MASK ===\n")
# post_cm[c, i] = comp ID after removing k_i tanks then installing m_i, NO A_age
# NA propagates automatically when rmap entry is NA (infeasible removal)
post_cm <- matrix(NA_integer_, nrow = C, ncol = n_act)
for (i in seq_len(n_act)) {
  k_i          <- k_vec[i]; m_i <- m_vec[i]
  after_remove <- kernel$rmap[, k_i + 1L]          # C-vector; NA if k_i infeasible
  valid        <- which(!is.na(after_remove))
  if (length(valid) > 0L)
    post_cm[valid, i] <- kernel$imap[cbind(after_remove[valid], rep(m_i + 1L, length(valid)))]
}
stopifnot(all(post_cm[!is.na(post_cm)] >= 1L & post_cm[!is.na(post_cm)] <= C))
cat(sprintf("  post_cm [%d x %d], NA frac = %.4f\n", C, n_act, mean(is.na(post_cm))))

feas_mask <- !is.na(post_cm)   # C x n_act logical
maint_idx <- which(k_vec == 0L & m_vec == 0L)
stopifnot(length(maint_idx) == 1L, all(feas_mask[, maint_idx]))
cat(sprintf("  feas_mask: maintain(0,0) always feasible [OK]\n"))

# Spot-check post_cm vs rmap/imap for 3 random comps
set.seed(42L)
for (sc in sample(seq_len(C), 3L)) {
  fa <- feasible_actions(ss$N_vec[sc], kernel$K_BAR, kernel$M_BAR, kernel$N_BAR)
  for (ri in seq_len(nrow(fa))) {
    ki <- fa$k[ri]; mi <- fa$m[ri]
    ai <- which(k_vec == ki & m_vec == mi)
    ar <- kernel$rmap[sc, ki + 1L]
    stopifnot(identical(post_cm[sc, ai], kernel$imap[ar, mi + 1L]))
  }
}
cat("  post_cm spot-check [OK]\n")

# === SECTION A1: ENVIRONMENTS ===
cat("\n=== SECTION A1: ENVIRONMENTS ===\n")
agg[, env_key := ifelse(g == "TX", paste0("RB_", era), as.character(g))]

exit_dt     <- agg[action == "X", .(n_exit = sum(n_obs)), by = env_key]
env_summary <- agg[, .(n_obs_total = sum(n_obs)), by = env_key]
env_summary <- merge(env_summary, exit_dt, by = "env_key", all.x = TRUE)
env_summary[is.na(n_exit), n_exit := 0L]
setorder(env_summary, env_key)
cat("  All environments (pre-fold):\n"); print(env_summary)

EXIT_FLOOR <- 30L
ff_drop <- env_summary[!grepl("^RB_", env_key) & n_exit < EXIT_FLOOR, env_key]
cat(sprintf("  Folded (< %d exits): %s\n",
            EXIT_FLOOR, if (length(ff_drop) > 0) paste(ff_drop, collapse = ", ") else "none"))

env_keep <- env_summary[!env_key %in% ff_drop, env_key]
.fast_test <- nzchar(Sys.getenv("PM08_TEST_NENV"))   # PM08_TEST_NENV=N -> N FF envs + 1 RB
if (.fast_test) {
  .nff <- max(1L, as.integer(Sys.getenv("PM08_TEST_NENV")))
  .rbk <- env_keep[grepl("^RB_", env_keep)]; .ffk <- env_keep[!grepl("^RB_", env_keep)]
  env_keep <- c(head(.ffk, .nff), head(.rbk, 1L))   # N FF (each has its own alpha) + 1 RB
  cat(sprintf("  *** FAST TEST MODE: subset to %d envs: %s ***\n",
              length(env_keep), paste(env_keep, collapse = ", ")))
}
agg_keep <- agg[env_key %in% env_keep]
n_env    <- length(env_keep)
rb_envs  <- env_keep[grepl("^RB_", env_keep)]
ff_envs  <- env_keep[!grepl("^RB_", env_keep)]
n_ff     <- length(ff_envs)
if (!.fast_test) stopifnot(length(rb_envs) == 3L)
cat(sprintf("  Survivors: %d envs (RB=%d, FF=%d)\n", n_env, length(rb_envs), n_ff))
cat(sprintf("  Env list: %s\n", paste(sort(env_keep), collapse = " ")))

# ASSERT every surviving row maps to exactly one env
stopifnot(all(agg_keep$env_key %in% env_keep))
cat(sprintf("  All %d agg_keep rows map to a survivor env [OK]\n", nrow(agg_keep)))

# Build env table
env_tbl <- data.table(
  env_idx    = seq_len(n_env),
  env_key    = env_keep,
  type       = ifelse(grepl("^RB_", env_keep), "RB", "FF"),
  g          = ifelse(grepl("^RB_", env_keep), "TX", env_keep),
  era_str    = ifelse(grepl("^RB_", env_keep), sub("^RB_", "", env_keep), NA_character_),
  alpha_slot = ifelse(grepl("^RB_", env_keep), NA_character_, env_keep)
)
d_vals   <- numeric(n_env)
tau_vals <- rep(NA_real_, n_env)
for (i in seq_len(n_env)) {
  d_vals[i]   <- if (env_tbl$type[i] == "RB") lk$D[["TX"]] else lk$D[[env_tbl$g[i]]]
  if (env_tbl$type[i] == "FF") tau_vals[i] <- lk$tau[[env_tbl$g[i]]]
}
env_tbl[, D   := d_vals]
env_tbl[, tau := tau_vals]
stopifnot(!anyNA(env_tbl$D))
stopifnot(!anyNA(env_tbl$tau[env_tbl$type == "FF"]))
cat("  env_tbl:\n"); print(env_tbl)

# Parameter names: 9 structural + n_ff alpha_g
param_struct <- if (use_pooled) {
  c("phi_1", "phi_2", "phi_3", "phi_4",
    "gamma_pool", "gamma_RB", "c_rem", "c_inst", "kappa_1")
} else {
  c("phi_1", "phi_2", "phi_3", "phi_4",
    "gamma_p", "gamma_r", "c_rem", "c_inst", "kappa_1")
}
param_alpha  <- paste0("alpha_", ff_envs)
param_names  <- c(param_struct, param_alpha)
n_param      <- length(param_names)
use_alpha    <- !nzchar(Sys.getenv("PM08_NO_ALPHA"))   # PM08_NO_ALPHA=1 -> state FEs fixed at 0 (no-FE model)
cat(sprintf("  n_param=%d (9 structural + %d alpha): %s\n",
            n_param, n_ff, paste(param_names, collapse = ", ")))
cat(sprintf("  STATE FEs: %s\n", if (use_alpha) "ON (alpha estimated)" else "OFF (PM08_NO_ALPHA: alpha fixed at 0)"))

# === SECTION A3: LOOKUPS AT n' ===
cat("\n=== SECTION A3: LOOKUPS AT n' ===\n")
# haz_j[c, i] = H(n') at post-action comp; env-independent
haz_j <- matrix(0.0, nrow = C, ncol = n_act)
for (i in seq_len(n_act)) {
  valid <- which(feas_mask[, i])
  if (length(valid) > 0L) haz_j[valid, i] <- lk$h_aw[ss$h_idx[post_cm[valid, i]]]
}
cat(sprintf("  haz_j [%d x %d], feasible range [%.4f, %.4f]\n",
            C, n_act, min(haz_j[feas_mask]), max(haz_j[feas_mask])))

# prem_ej: list n_env, each C x n_act (premium at post-action comp for env e, work action i)
prem_ej <- vector("list", n_env)
for (e in seq_len(n_env)) {
  pm <- matrix(0.0, nrow = C, ncol = n_act)
  ev <- env_tbl[e]
  for (i in seq_len(n_act)) {
    valid <- which(feas_mask[, i])
    if (length(valid) == 0L) next
    pc <- post_cm[valid, i]
    pm[valid, i] <- if (ev$type == "RB") {
      ss$P_RB_all[pc, ev$era_str]
    } else {
      ev$tau * ss$N_vec[pc]
    }
  }
  prem_ej[[e]] <- pm
}
cat(sprintf("  prem_ej: %d env x [%d x %d] [OK]\n", n_env, C, n_act))

# === SECTION A5: P^0 POOLED HOTZ-MILLER CCP (verbatim PM07) ===
cat("\n=== SECTION A5: P^0 POOLED HM CCP ===\n")
all_action_keys <- c(act_keys, "X")
hm_dt <- agg[, .(n_obs = sum(n_obs)), by = .(sidx, action)]
hm_dt[, G_idx := as.integer(ceiling(sidx / C))]
hm_dt[, c_idx := as.integer(sidx - (G_idx - 1L) * C)]
stopifnot(all(hm_dt$G_idx >= 1L & hm_dt$G_idx <= N_G))
stopifnot(all(hm_dt$c_idx >= 1L & hm_dt$c_idx <= C))

P_hm <- setNames(
  lapply(all_action_keys, function(ak) matrix(eps_prob, nrow = C, ncol = N_G)),
  all_action_keys
)
for (ri in seq_len(nrow(hm_dt))) {
  ci <- hm_dt$c_idx[ri]; Gi <- hm_dt$G_idx[ri]
  ak <- hm_dt$action[ri]; n  <- hm_dt$n_obs[ri]
  if (!ak %in% names(P_hm)) next
  P_hm[[ak]][ci, Gi] <- P_hm[[ak]][ci, Gi] + n
}
row_tot <- matrix(0.0, nrow = C, ncol = N_G)
for (ak in names(P_hm)) row_tot <- row_tot + P_hm[[ak]]
for (ak in names(P_hm)) P_hm[[ak]] <- P_hm[[ak]] / row_tot
check <- matrix(0.0, nrow = C, ncol = N_G)
for (ak in names(P_hm)) check <- check + P_hm[[ak]]
stopifnot(max(abs(check - 1.0)) < 1e-8)
cat(sprintf("  P_hm built: %d actions, row-sum OK\n", length(P_hm)))

# Apply feasibility mask: infeasible work entries -> exact 0, re-normalize
P_hm_masked <- lapply(P_hm, function(m) m + 0.0)
names(P_hm_masked) <- names(P_hm)
for (i in seq_len(n_act)) {
  infeas <- which(!feas_mask[, i])
  if (length(infeas) > 0L) P_hm_masked[[act_keys[i]]][infeas, ] <- 0.0
}
row_tot_masked <- matrix(0.0, nrow = C, ncol = N_G)
for (ak in names(P_hm_masked)) row_tot_masked <- row_tot_masked + P_hm_masked[[ak]]
stopifnot(all(row_tot_masked > 0))
for (ak in names(P_hm_masked)) P_hm_masked[[ak]] <- P_hm_masked[[ak]] / row_tot_masked
check_m <- matrix(0.0, nrow = C, ncol = N_G)
for (ak in names(P_hm_masked)) check_m <- check_m + P_hm_masked[[ak]]
stopifnot(max(abs(check_m - 1.0)) < 1e-8)
for (i in seq_len(n_act)) {
  infeas <- which(!feas_mask[, i])
  if (length(infeas) > 0L) stopifnot(all(P_hm_masked[[act_keys[i]]][infeas, ] == 0.0))
}
cat("  P_hm_masked: infeasible=0, rows sum to 1, infeasible check [OK]\n")

# Per-env CCP container: at iter 0 all envs share P_hm_masked
P_envs <- vector("list", n_env)
for (e in seq_len(n_env)) {
  P_envs[[e]] <- lapply(P_hm_masked, function(m) m + 0.0)
  names(P_envs[[e]]) <- names(P_hm_masked)
}
cat(sprintf("  P_envs: %d envs initialized to P_hm_masked [OK]\n", n_env))

# === SECTION A6: P1 PRECONDITIONER STATICS (verbatim PM07) ===
cat("\n=== SECTION A6: P1 PRECONDITIONER STATICS ===\n")
MARG_BIN <- ss$MARG_BIN
pot      <- as.vector(ss$comp_mat %*% (8L - MARG_BIN))
ord      <- order(pot, decreasing = TRUE)
A_perm   <- ss$A_age[ord, ord]
lower_off <- tril(A_perm, -1L); upper_off <- triu(A_perm, 1L)
below_mass <- sum(abs(lower_off)); above_mass <- sum(abs(upper_off))
cat(sprintf("  Triangularity: below=%.6e  above=%.6e\n", below_mass, above_mass))
if (below_mass == 0 && above_mass > 0) {
  uplo_flag <- "U"
} else if (above_mass == 0 && below_mass > 0) {
  uplo_flag <- "L"
} else {
  stop(sprintf("TRIANGULARITY ASSERT FAILED: below=%.6e above=%.6e", below_mass, above_mass))
}
cat(sprintf("  A_perm triangular (%s) [OK]\n", if (uplo_flag == "U") "upper" else "lower"))

# bicgstab_pc: right-preconditioned BiCGSTAB (verbatim PM07 lines 207-266)
bicgstab_pc <- function(Ax_fn, b, Minv, tol = 1e-9, max_it = 2000L, x0 = NULL) {
  n <- length(b)
  x <- if (!is.null(x0)) x0 else numeric(n)
  r <- b - Ax_fn(x); rhat <- r
  rho <- 1.0; alpha <- 1.0; w <- 1.0
  v <- numeric(n); p <- numeric(n)
  for (it in seq_len(max_it)) {
    rho1 <- sum(rhat * r)
    if (abs(rho1) < 1e-100) {
      rhat <- r; rho <- 1.0; alpha <- 1.0; w <- 1.0; v <- numeric(n); p <- numeric(n)
      rho1 <- sum(rhat * r)
      if (abs(rho1) < 1e-100) stop(sprintf("BiCGSTAB stalled: rho1=%.3e at iter %d", rho1, it))
    }
    bta  <- (rho1 / rho) * (alpha / w)
    p    <- r + bta * (p - w * v)
    phat <- Minv(p); v <- Ax_fn(phat)
    denom_alpha <- sum(rhat * v)
    if (abs(denom_alpha) < 1e-100) stop(sprintf("BiCGSTAB stalled: <rhat,v>=%.3e at iter %d", denom_alpha, it))
    alpha <- rho1 / denom_alpha
    s     <- r - alpha * v
    if (max(abs(s)) < tol) { x <- x + alpha * phat; return(list(x=x, iters=it, converged=TRUE, resid=max(abs(s)))) }
    shat <- Minv(s); t <- Ax_fn(shat)
    denom_w <- sum(t * t)
    if (abs(denom_w) < 1e-100) stop(sprintf("BiCGSTAB stalled: <t,t>=%.3e at iter %d", denom_w, it))
    w <- sum(t * s) / denom_w
    x <- x + alpha * phat + w * shat; r <- s - w * t; rho <- rho1
    resid_cur <- max(abs(r))
    if (resid_cur < tol) return(list(x=x, iters=it, converged=TRUE, resid=resid_cur))
    if (it %% 50L == 0L) cat(sprintf("  [%s] BiCGSTAB iter %d: ||r||_inf=%.3e\n",
                                     format(Sys.time(), "%H:%M:%S"), it, resid_cur))
  }
  stop(sprintf("BiCGSTAB did not converge in %d iters; ||r||_inf=%.3e", max_it, max(abs(r))))
}
cat("  bicgstab_pc() defined [OK]\n")

# build_minv_p1: build P1 Minv function for env e (env-specific maintain CCP per A3)
build_minv_p1 <- function(p_e) {
  T_G_list <- vector("list", N_G)
  for (G in seq_len(N_G)) {
    p_maint   <- p_e[["0,0"]][, G]
    T_G_raw   <- as(Diagonal(C) - beta * Diagonal(x = p_maint[ord]) %*% A_perm, "CsparseMatrix")
    T_G_list[[G]] <- new("dtCMatrix", i=T_G_raw@i, p=T_G_raw@p, x=T_G_raw@x,
                          Dim=T_G_raw@Dim, uplo=uplo_flag, diag="N")
  }
  function(r) {
    R_mat <- matrix(r, nrow = C, ncol = N_G)
    Y_mat <- matrix(0.0, nrow = C, ncol = N_G)
    for (G in seq_len(N_G)) {
      b <- R_mat[ord, G]
      Y_mat[ord, G] <- as.numeric(Matrix::solve(T_G_list[[G]], b))
    }
    as.vector(Y_mat)
  }
}
cat("  build_minv_p1() defined [OK]\n")

# C++ operator statics (precompute once; P_list is per-env-per-iter)
imap0 <- kernel$imap - 1L; imap0[is.na(imap0)] <- -1L; storage.mode(imap0) <- "integer"
rmap0 <- kernel$rmap - 1L; rmap0[is.na(rmap0)] <- -1L; storage.mode(rmap0) <- "integer"
GaT_list <- vector("list", n_act)
for (i in seq_len(n_act)) GaT_list[[i]] <- t(kernel$Gmat[[netbin(k_vec[i], m_vec[i])]])

# build_cpp_op: build C++ op encoding M^e for env e at current P_envs[[e]].
# Dispatches to batched (pm_op_build_b) or original (pm_op_build) via USE_BATCHED_MV.
build_cpp_op <- function(p_e) {
  P_list <- vector("list", n_act)
  for (i in seq_len(n_act)) {
    ak <- act_keys[i]
    # as.matrix guard: P_list must be base matrices for Rcpp::as<arma::mat>.
    P_list[[i]] <- as.matrix(if (ak %in% names(p_e)) p_e[[ak]] else matrix(0.0, C, N_G))
  }
  if (USE_BATCHED_MV) {
    pm_op_build_b(kernel$A_age, imap0, rmap0, k_vec, m_vec, GaT_list, P_list, C, N_G)
  } else {
    pm_op_build(kernel$A_age, imap0, rmap0, k_vec, m_vec, GaT_list, P_list, C, N_G)
  }
}
cat("  build_cpp_op() defined [OK]\n")

# === GATE A ===
cat("\n=== GATE A: VALIDATION ===\n")
cat(sprintf("  DIMS: C=%d N_G=%d N_SIDX=%d n_act=%d n_env=%d n_param=%d\n",
            C, N_G, N_SIDX, n_act, n_env, n_param))
cat(sprintf("  Dropped envs: %s\n",
            if (length(ff_drop) > 0) paste(ff_drop, collapse = ", ") else "none"))
cat(sprintf("  Survivor envs (%d): %s\n", n_env, paste(sort(env_keep), collapse = " ")))

# Spot-check u-columns for 3 random states
cat("  Spot-checking design lookups for 3 random states...\n")
set.seed(99L)
spot_sidx <- sample(seq_len(N_SIDX), 3L)
for (si in spot_sidx) {
  G_s <- as.integer(ceiling(si / C))
  c_s <- as.integer(si - (G_s - 1L) * C)
  fa  <- feasible_actions(ss$N_vec[c_s], kernel$K_BAR, kernel$M_BAR, kernel$N_BAR)
  for (e in c(1L, min(n_env, 2L))) {
    ev  <- env_tbl[e]
    for (ri in seq_len(min(2L, nrow(fa)))) {
      ki <- fa$k[ri]; mi <- fa$m[ri]
      ai <- which(k_vec == ki & m_vec == mi)
      pc <- post_cm[c_s, ai]
      if (is.na(pc)) next
      prem_formula <- if (ev$type == "RB") ss$P_RB_all[pc, ev$era_str] else ev$tau * ss$N_vec[pc]
      haz_formula  <- lk$h_aw[ss$h_idx[pc]]
      stopifnot(abs(prem_formula - prem_ej[[e]][c_s, ai]) < 1e-12)
      stopifnot(abs(haz_formula  - haz_j[c_s, ai])        < 1e-12)
    }
  }
}
cat("  u-column spot-check (prem + haz) [OK]\n")

# Masked P^0 rows sum to 1
check_sum <- matrix(0.0, nrow = C, ncol = N_G)
for (ak in names(P_envs[[1L]])) check_sum <- check_sum + P_envs[[1L]][[ak]]
cat(sprintf("  P_envs[[1]] max row-sum dev = %.3e (tol 1e-8)\n", max(abs(check_sum - 1.0))))
stopifnot(max(abs(check_sum - 1.0)) < 1e-8)

# Infeasible = exact 0 in P_envs[[1]]
for (i in seq_len(n_act)) {
  infeas <- which(!feas_mask[, i])
  if (length(infeas) > 0L) stopifnot(all(P_envs[[1L]][[act_keys[i]]][infeas, ] == 0.0))
}
cat("  Masked P^0: infeasible=0, row-sums=1 [OK]\n")

cat("\n=== GATE A COMPLETE ===\n")
cat(sprintf("  n_env=%d, n_param=%d, n_act=%d — Phase A static assembly OK\n",
            n_env, n_param, n_act))
cat("  Ready for Phase B.\n")

# ============================================================
# PHASE B — BASIS + VALUE (one env, one test theta)
# ============================================================
cat("\n=== PHASE B: BASIS + VALUE ===\n")

e_test  <- if (use_pooled) env_tbl[type == "RB", env_idx[1L]] else 1L   # pooled: RB env so gamma_RB gets real GATE C check
ev_test <- env_tbl[e_test]
cat(sprintf("  Test env %d: %s (type=%s)\n", e_test, ev_test$env_key, ev_test$type))

# B1: build C++ op (M^e) and P1 Minv for env e_test
cat("\n--- B1: Build cpp_op and Minv_P1 ---\n")
t0_b1    <- proc.time()["elapsed"]
cpp_op_e  <- build_cpp_op(P_envs[[e_test]])
mv_call_e <- if (USE_BATCHED_MV) pm_op_mv_b else pm_op_mv
A_x_e     <- function(x) x - beta * as.numeric(mv_call_e(cpp_op_e, x))
Minv_e    <- build_minv_p1(P_envs[[e_test]])
cat(sprintf("  B1 built: %.2f sec\n", proc.time()["elapsed"] - t0_b1))

# B2: compute R^e_const and rho^e_p (one vector per param), solve basis
cat("\n--- B2: R_const, rho_p, solve V_const and W_p ---\n")

# R^e_const[c,G] = sum_{all feasible j incl X} P^e_j(c,G) * (gamma_E - log P^e_j(c,G))
R_const_mat <- matrix(0.0, nrow = C, ncol = N_G)
for (ak in names(P_envs[[e_test]])) {
  Pa  <- P_envs[[e_test]][[ak]]
  pos <- Pa > 0
  entr <- matrix(0.0, nrow = C, ncol = N_G)
  entr[pos] <- gamma_E - log(Pa[pos])
  R_const_mat <- R_const_mat + Pa * entr
}
stopifnot(all(is.finite(R_const_mat)))
cat(sprintf("  R_const range: [%.4f, %.4f]\n", min(R_const_mat), max(R_const_mat)))

# rho^e_p: du^e_j/dtheta_p aggregated by P^e_j (C x N_G matrix per param)
rho_list <- setNames(vector("list", n_param), param_names)
Pa_X_e   <- P_envs[[e_test]][["X"]]   # C x N_G (exit CCP for env e_test)

# phi_G' (G'=1..4): rho[c, G'] = 1 - P_X[c, G']; zero elsewhere
for (G_prime in seq_len(N_G)) {
  rho <- matrix(0.0, nrow = C, ncol = N_G)
  rho[, G_prime] <- 1.0 - Pa_X_e[, G_prime]
  rho_list[[paste0("phi_", G_prime)]] <- rho
}

# gamma_p/gamma_pool intermediate: rho[c,G] = -sum_j work P_j[c,G] * prem_j[c]
rho_gp <- matrix(0.0, nrow = C, ncol = N_G)
for (i in seq_len(n_act))
  rho_gp <- rho_gp - P_envs[[e_test]][[act_keys[i]]] * prem_ej[[e_test]][, i]

# gamma_r/gamma_pool intermediate: rho[c,G] = -D_e * sum_j work P_j[c,G] * haz_j[c]
D_e    <- ev_test$D
rho_gr <- matrix(0.0, nrow = C, ncol = N_G)
for (i in seq_len(n_act))
  rho_gr <- rho_gr - D_e * P_envs[[e_test]][[act_keys[i]]] * haz_j[, i]

if (use_pooled) {
  rb <- (ev_test$type == "RB")
  rho_list[["gamma_pool"]] <- rho_gp + rho_gr
  rho_list[["gamma_RB"]]   <- rb * (rho_gp + rho_gr)
} else {
  rho_list[["gamma_p"]] <- rho_gp
  rho_list[["gamma_r"]] <- rho_gr
}

# c_rem: rho[c,G] = -sum_j work P_j[c,G] * k_j
rho_cr <- matrix(0.0, nrow = C, ncol = N_G)
for (i in seq_len(n_act))
  if (k_vec[i] > 0L) rho_cr <- rho_cr - k_vec[i] * P_envs[[e_test]][[act_keys[i]]]
rho_list[["c_rem"]] <- rho_cr

# c_inst: rho[c,G] = -sum_j work P_j[c,G] * m_j
rho_ci <- matrix(0.0, nrow = C, ncol = N_G)
for (i in seq_len(n_act))
  if (m_vec[i] > 0L) rho_ci <- rho_ci - m_vec[i] * P_envs[[e_test]][[act_keys[i]]]
rho_list[["c_inst"]] <- rho_ci

# kappa_1: rho[c,G] = P_X[c,G] * N_vec[c]  (exit only)
rho_list[["kappa_1"]] <- Pa_X_e * matrix(ss$N_vec, nrow = C, ncol = N_G)

# alpha_g: rho = (1 - P_X) for this FF env; exact 0 for all others
for (ff_g in ff_envs) {
  pn <- paste0("alpha_", ff_g)
  if (ev_test$type == "FF" && ev_test$g == ff_g) {
    rho_list[[pn]] <- 1.0 - Pa_X_e
  } else {
    rho_list[[pn]] <- matrix(0.0, nrow = C, ncol = N_G)
  }
}
cat(sprintf("  rho_list: %d params built [OK]\n", n_param))

# Solve V_const = (I - beta M^e)^{-1} R_const
cat("  Solving V_const (x0=NULL)...\n")
t0_vc   <- proc.time()["elapsed"]
sol_vc  <- bicgstab_pc(A_x_e, as.vector(R_const_mat), Minv_e, tol = 1e-9)
V_const <- sol_vc$x
cat(sprintf("  V_const: %d iters, resid=%.3e, %.2f sec\n",
            sol_vc$iters, sol_vc$resid, proc.time()["elapsed"] - t0_vc))
stopifnot(sol_vc$converged, max(abs(A_x_e(V_const) - as.vector(R_const_mat))) < 1e-8)
cat(sprintf("  V_const range: [%.4f, %.4f]\n", min(V_const), max(V_const)))

# Solve W_p = (I - beta M^e)^{-1} rho_p; skip zero-rho params
W_list <- setNames(vector("list", n_param), param_names)
cat("  Solving W_p per param (x0=NULL for each)...\n")
for (pn in param_names) {
  rho_v <- as.vector(rho_list[[pn]])
  if (max(abs(rho_v)) < 1e-15) {
    W_list[[pn]] <- numeric(N_SIDX)
    cat(sprintf("    %-14s rho=0 -> W=0 (skip)\n", pn))
    next
  }
  t0_wp  <- proc.time()["elapsed"]
  sol_wp <- bicgstab_pc(A_x_e, rho_v, Minv_e, tol = 1e-9)
  W_list[[pn]] <- sol_wp$x
  cat(sprintf("    %-14s %3d iters, resid=%.3e, %.2f sec\n",
              pn, sol_wp$iters, sol_wp$resid, proc.time()["elapsed"] - t0_wp))
}

# Test theta: sign-correct values (Q9/Q10; all alphas at 0)
theta_test <- setNames(rep(0.0, n_param), param_names)
theta_test["phi_1"] <- 0.5;  theta_test["phi_2"] <-  0.3
theta_test["phi_3"] <- 0.1;  theta_test["phi_4"] <- -0.1
if (use_pooled) {
  theta_test["gamma_pool"] <- 0.05;  theta_test["gamma_RB"] <- 0.02
} else {
  theta_test["gamma_p"] <- -2.0;  theta_test["gamma_r"] <- 0.1
}
theta_test["c_rem"]  <- 0.5;  theta_test["c_inst"]  <- 0.5
theta_test["kappa_1"] <- -1.0

# Assemble V(theta_test) = V_const + sum_p theta_p * W_p
V_basis_vec <- V_const
for (pn in param_names) V_basis_vec <- V_basis_vec + theta_test[[pn]] * W_list[[pn]]
cat(sprintf("  V_basis assembled: range [%.4f, %.4f]\n", min(V_basis_vec), max(V_basis_vec)))

# B3: GATE B — direct solve vs basis assembly
cat("\n--- B3: GATE B ---\n")
R_test_vec <- as.vector(R_const_mat)
for (pn in param_names) R_test_vec <- R_test_vec + theta_test[[pn]] * as.vector(rho_list[[pn]])
cat(sprintf("  R_test range: [%.4f, %.4f]\n", min(R_test_vec), max(R_test_vec)))

t0_dir   <- proc.time()["elapsed"]
sol_dir  <- bicgstab_pc(A_x_e, R_test_vec, Minv_e, tol = 1e-9)
V_direct <- sol_dir$x
cat(sprintf("  V_direct: %d iters, resid=%.3e, %.2f sec\n",
            sol_dir$iters, sol_dir$resid, proc.time()["elapsed"] - t0_dir))

resid_direct <- max(abs(A_x_e(V_direct) - R_test_vec))
gate_b_dev   <- max(abs(V_basis_vec - V_direct))
cat(sprintf("  V_direct residual:        %.3e (tol 1e-8)\n", resid_direct))
cat(sprintf("  GATE B max|V_basis - V_direct|: %.3e (tol 1e-8)\n", gate_b_dev))
stopifnot(resid_direct < 1e-8)
stopifnot(gate_b_dev   < 1e-8)
cat("  GATE B PASS\n")

# ============================================================
# PHASE C — LIKELIHOOD + GRADIENT (env e_test; validate gradient)
# ============================================================
cat("\n=== PHASE C: LIKELIHOOD + GRADIENT ===\n")

# --- C1: precompute basis pieces at observed states for env e_test ---
cat("\n--- C1: Precompute basis pieces at observed states ---\n")
agg_e  <- agg_keep[env_key == ev_test$env_key]
cat(sprintf("  agg_e: %d rows for env %s\n", nrow(agg_e), ev_test$env_key))

# Observed states: distinct sidx in agg_e
agg_e[, G_idx := as.integer(ceiling(sidx / C))]
agg_e[, c_idx := as.integer(sidx - (G_idx - 1L) * C)]
obs_states  <- unique(agg_e[, .(sidx, c_idx, G_idx)])
setorder(obs_states, sidx)
n_obs_states <- nrow(obs_states)
obs_c <- obs_states$c_idx
obs_G <- obs_states$G_idx
cat(sprintf("  n_obs_states=%d, obs_c range=[%d,%d], obs_G range=[%d,%d]\n",
            n_obs_states, min(obs_c), max(obs_c), min(obs_G), max(obs_G)))

# Map sidx -> row in obs_states (for LL assembly)
sidx_to_srow <- setNames(seq_len(n_obs_states), obs_states$sidx)

# coeff_v0: (n_act+1) x n_obs_states — theta-free part of v_j at obs states
# Row n_act+1 = exit action (no Bellman term)
n_act_p1    <- n_act + 1L   # n_act work + 1 exit
coeff_v0    <- matrix(0.0, nrow = n_act_p1, ncol = n_obs_states)

V_const_mat <- matrix(V_const, nrow = C, ncol = N_G)
for (i in seq_len(n_act)) {
  nb      <- netbin(k_vec[i], m_vec[i])
  FjVc_i  <- beta * comp_apply(kernel, k_vec[i], m_vec[i], V_const_mat %*% t(kernel$Gmat[[nb]]))
  coeff_v0[i, ] <- FjVc_i[cbind(obs_c, obs_G)]
}
# Exit row stays 0 (absorbing — no next-period term)

# coeff_p: list of n_param matrices, each (n_act+1) x n_obs_states
# coeff_p[[p]][j, s] = d v_j(theta)/d theta_p at obs state s
# = du_j/dtheta_p + beta*(F_j W_p)[obs_c[s], obs_G[s]]  for work j
# = du_X/dtheta_p                                          for exit (no Bellman)
cat("  Computing coeff_p for each param (n_act FjWp comp_apply calls each)...\n")
t0_cp   <- proc.time()["elapsed"]
rb_test <- (ev_test$type == "RB")   # rb flag for gamma_RB du in GATE C
coeff_p <- vector("list", n_param)
names(coeff_p) <- param_names
for (pn in param_names) {
  cp <- matrix(0.0, nrow = n_act_p1, ncol = n_obs_states)
  W_p_mat <- matrix(W_list[[pn]], nrow = C, ncol = N_G)
  for (i in seq_len(n_act)) {
    # beta*(F_j W_p) at obs states
    if (max(abs(W_list[[pn]])) < 1e-15) {
      FjWp_obs_i <- numeric(n_obs_states)
    } else {
      nb       <- netbin(k_vec[i], m_vec[i])
      FjWp_i   <- beta * comp_apply(kernel, k_vec[i], m_vec[i], W_p_mat %*% t(kernel$Gmat[[nb]]))
      FjWp_obs_i <- FjWp_i[cbind(obs_c, obs_G)]
    }
    # du_j/dtheta_p at each obs state
    du_dp_i <- numeric(n_obs_states)
    if      (pn == "phi_1")   du_dp_i[obs_G == 1L] <- 1.0
    else if (pn == "phi_2")   du_dp_i[obs_G == 2L] <- 1.0
    else if (pn == "phi_3")   du_dp_i[obs_G == 3L] <- 1.0
    else if (pn == "phi_4")   du_dp_i[obs_G == 4L] <- 1.0
    else if (pn == "gamma_p")    du_dp_i <- -prem_ej[[e_test]][cbind(obs_c, rep(i, n_obs_states))]
    else if (pn == "gamma_r")    du_dp_i <- -haz_j[cbind(obs_c, rep(i, n_obs_states))] * D_e
    else if (pn == "gamma_pool") du_dp_i <- -(prem_ej[[e_test]][cbind(obs_c, rep(i, n_obs_states))] + haz_j[cbind(obs_c, rep(i, n_obs_states))] * D_e)
    else if (pn == "gamma_RB")   du_dp_i <- -rb_test * (prem_ej[[e_test]][cbind(obs_c, rep(i, n_obs_states))] + haz_j[cbind(obs_c, rep(i, n_obs_states))] * D_e)
    else if (pn == "c_rem")   du_dp_i <- rep(-k_vec[i], n_obs_states)
    else if (pn == "c_inst")  du_dp_i <- rep(-m_vec[i], n_obs_states)
    # kappa_1, alpha_g: du_j/dp = 0 for work j (kappa_1 is exit-only; alpha in exit=0)
    # alpha for THIS FF env: du_j/dalpha = 1 for work
    else if (pn == paste0("alpha_", ev_test$g) && ev_test$type == "FF")
      du_dp_i <- rep(1.0, n_obs_states)
    cp[i, ] <- du_dp_i + FjWp_obs_i
  }
  # Exit row: only kappa_1 has non-zero du_X/dp = N_vec[c_s]
  if (pn == "kappa_1") cp[n_act_p1, ] <- ss$N_vec[obs_c]
  coeff_p[[pn]] <- cp
}
cat(sprintf("  coeff_p built: %.2f sec\n", proc.time()["elapsed"] - t0_cp))

# Feasibility mask at obs states: feas_obs[j, s] TRUE if action j feasible at c_s
feas_obs <- matrix(FALSE, nrow = n_act_p1, ncol = n_obs_states)
for (i in seq_len(n_act)) feas_obs[i, ] <- feas_mask[obs_c, i]
feas_obs[n_act_p1, ] <- TRUE   # exit always feasible

# Action key index: map agg_e action strings to row in coeff matrices
all_act_keys_p1 <- c(act_keys, "X")
act_row <- setNames(seq_len(n_act_p1), all_act_keys_p1)

# --- C2: LL and analytic gradient function ---
cat("\n--- C2: LL + analytic gradient ---\n")

ll_grad_e <- function(theta_vec) {
  # theta_vec: numeric n_param (named)
  # Returns list(LL, grad)

  # Assemble v_mat = (n_act_p1) x n_obs_states: v_j(theta) for each obs state
  v_mat <- coeff_v0
  for (p in seq_len(n_param)) v_mat <- v_mat + theta_vec[p] * coeff_p[[p]]

  # Log-sum-exp per state (column), respecting feasibility mask
  # Set infeasible entries to -Inf before softmax
  v_mat_masked <- v_mat
  v_mat_masked[!feas_obs] <- -Inf
  v_max <- apply(v_mat_masked, 2L, max)                          # n_obs_states
  exp_v <- exp(sweep(v_mat_masked, 2L, v_max, "-"))              # n_act_p1 x n_obs_states
  denom <- colSums(exp_v)                                        # n_obs_states
  log_denom <- log(denom) + v_max                                # log(sum exp v_j)

  LL_val <- 0.0
  grad   <- numeric(n_param)

  # agg_e rows: sidx -> srow; action -> act_row_idx
  s_rows <- sidx_to_srow[as.character(agg_e$sidx)]
  j_rows <- act_row[agg_e$action]
  valid  <- !is.na(s_rows) & !is.na(j_rows)

  # LL
  log_Lambda_vec <- v_mat[cbind(j_rows[valid], s_rows[valid])] - log_denom[s_rows[valid]]
  LL_val <- sum(agg_e$n_obs[valid] * log_Lambda_vec)

  # Lambda: n_act_p1 x n_obs_states
  Lambda <- sweep(exp_v, 2L, denom, "/")
  Lambda[!feas_obs] <- 0.0

  # Gradient: for each param p,
  # grad[p] = sum_{s,j} n_obs[s,j] * (coeff_p[j,s] - sum_{j'} Lambda[j',s] * coeff_p[j',s])
  # E_coeff_p[s] = sum_{j'} Lambda[j',s] * coeff_p[j',s]  (expected coefficient per state)
  s_rows_v <- s_rows[valid]
  j_rows_v <- j_rows[valid]
  n_obs_v  <- agg_e$n_obs[valid]

  for (p in seq_len(n_param)) {
    cp_p   <- coeff_p[[p]]                                       # n_act_p1 x n_obs_states
    E_cp_p <- colSums(Lambda * cp_p)                             # n_obs_states
    score  <- cp_p[cbind(j_rows_v, s_rows_v)] - E_cp_p[s_rows_v]
    grad[p] <- sum(n_obs_v * score)
  }

  list(LL = LL_val, grad = grad)
}

res_test <- ll_grad_e(theta_test)
cat(sprintf("  LL(theta_test) = %.4f\n", res_test$LL))
cat(sprintf("  Analytic grad: %s\n",
            paste(sprintf("%s=%.4f", param_names, res_test$grad), collapse = ", ")))

# --- C3: GATE C — central finite-difference gradient check ---
cat("\n--- C3: GATE C ---\n")
fd_step <- 1e-5
grad_fd <- numeric(n_param)
for (p in seq_len(n_param)) {
  th_hi      <- theta_test; th_hi[p] <- th_hi[p] + fd_step
  th_lo      <- theta_test; th_lo[p] <- th_lo[p] - fd_step
  LL_hi      <- ll_grad_e(th_hi)$LL
  LL_lo      <- ll_grad_e(th_lo)$LL
  grad_fd[p] <- (LL_hi - LL_lo) / (2.0 * fd_step)
}
cat(sprintf("  FD grad:       %s\n",
            paste(sprintf("%s=%.4f", param_names, grad_fd), collapse = ", ")))

# Relative error per param (use absolute scale for near-zero grads)
analytic_g <- res_test$grad
abs_ref    <- pmax(abs(analytic_g), abs(grad_fd), 1e-8)
rel_err    <- abs(analytic_g - grad_fd) / abs_ref
cat(sprintf("  Max rel err: %.3e  (tol 1e-5)\n", max(rel_err)))
cat(sprintf("  Per-param rel err: %s\n",
            paste(sprintf("%s=%.2e", param_names, rel_err), collapse = ", ")))
stopifnot(max(rel_err) < 1e-5)
cat("  GATE C PASS\n")

# ============================================================
# PHASE D — NPL OUTER LOOP
# ============================================================
cat("\n=== PHASE D: NPL OUTER LOOP ===\n")

# --- Helper: build basis for env e given its current CCP ---
build_basis_env <- function(e, p_e, V_const_warm = NULL, W_warm = NULL) {
  ev    <- env_tbl[e]
  D_e   <- ev$D
  Pa_X  <- p_e[["X"]]

  cpp_op  <- build_cpp_op(p_e)
  mv_call <- if (USE_BATCHED_MV) pm_op_mv_b else pm_op_mv
  A_x     <- function(x) x - beta * as.numeric(mv_call(cpp_op, x))
  Minv    <- build_minv_p1(p_e)

  # R_const
  R_const <- matrix(0.0, nrow = C, ncol = N_G)
  for (ak in names(p_e)) {
    Pa <- p_e[[ak]]; pos <- Pa > 0
    en <- matrix(0.0, nrow = C, ncol = N_G); en[pos] <- gamma_E - log(Pa[pos])
    R_const <- R_const + Pa * en
  }

  # rho_p
  rho <- setNames(vector("list", n_param), param_names)
  for (G_prime in seq_len(N_G)) {
    rp <- matrix(0.0, nrow = C, ncol = N_G)
    rp[, G_prime] <- 1.0 - Pa_X[, G_prime]
    rho[[paste0("phi_", G_prime)]] <- rp
  }
  rho_gp <- matrix(0.0, nrow = C, ncol = N_G)
  for (i in seq_len(n_act)) rho_gp <- rho_gp - p_e[[act_keys[i]]] * prem_ej[[e]][, i]
  rho_gr <- matrix(0.0, nrow = C, ncol = N_G)
  for (i in seq_len(n_act)) rho_gr <- rho_gr - D_e * p_e[[act_keys[i]]] * haz_j[, i]
  if (use_pooled) {
    rb_e <- (ev$type == "RB")
    rho[["gamma_pool"]] <- rho_gp + rho_gr
    rho[["gamma_RB"]]   <- rb_e * (rho_gp + rho_gr)
  } else {
    rho[["gamma_p"]] <- rho_gp
    rho[["gamma_r"]] <- rho_gr
  }
  rho_cr <- matrix(0.0, nrow = C, ncol = N_G)
  for (i in seq_len(n_act)) if (k_vec[i] > 0L) rho_cr <- rho_cr - k_vec[i] * p_e[[act_keys[i]]]
  rho[["c_rem"]] <- rho_cr
  rho_ci <- matrix(0.0, nrow = C, ncol = N_G)
  for (i in seq_len(n_act)) if (m_vec[i] > 0L) rho_ci <- rho_ci - m_vec[i] * p_e[[act_keys[i]]]
  rho[["c_inst"]] <- rho_ci
  rho[["kappa_1"]] <- Pa_X * matrix(ss$N_vec, nrow = C, ncol = N_G)
  for (ff_g in ff_envs) {
    pn <- paste0("alpha_", ff_g)
    rho[[pn]] <- if (ev$type == "FF" && ev$g == ff_g) 1.0 - Pa_X else matrix(0.0, nrow = C, ncol = N_G)
  }

  # Solve V_const (warm-start from previous iter if available)
  x0_vc   <- if (!is.null(V_const_warm)) V_const_warm else NULL
  sol_vc  <- bicgstab_pc(A_x, as.vector(R_const), Minv, tol = 1e-9, x0 = x0_vc)
  if (!sol_vc$converged) stop(sprintf("V_const solve failed for env %s", ev$env_key))
  Vc <- sol_vc$x

  # Solve W_p
  W <- setNames(vector("list", n_param), param_names)
  for (pn in param_names) {
    rv <- as.vector(rho[[pn]])
    if (max(abs(rv)) < 1e-15) { W[[pn]] <- numeric(N_SIDX); next }
    x0_wp  <- if (!is.null(W_warm)) W_warm[[pn]] else NULL
    sol_wp <- bicgstab_pc(A_x, rv, Minv, tol = 1e-9, x0 = x0_wp)
    if (!sol_wp$converged) stop(sprintf("W_p solve failed for %s env %s", pn, ev$env_key))
    W[[pn]] <- sol_wp$x
  }

  list(V_const = Vc, W_list = W, R_const = R_const, rho_list = rho)
}

# --- Helper: build coefficient tensors at observed states for env e ---
build_coeffs_env <- function(e, basis_e) {
  ev      <- env_tbl[e]
  D_e     <- ev$D
  rb_e    <- (ev$type == "RB")
  Vc      <- basis_e$V_const
  W       <- basis_e$W_list
  ae      <- agg_keep[env_key == ev$env_key]
  ae[, G_idx := as.integer(ceiling(sidx / C))]
  ae[, c_idx := as.integer(sidx - (G_idx - 1L) * C)]
  obs_s   <- unique(ae[, .(sidx, c_idx, G_idx)]); setorder(obs_s, sidx)
  n_obs_s <- nrow(obs_s)
  obs_c_e <- obs_s$c_idx; obs_G_e <- obs_s$G_idx

  # coeff_v0: (n_act_p1) x n_obs_s
  Vc_mat   <- matrix(Vc, nrow = C, ncol = N_G)
  cv0      <- matrix(0.0, nrow = n_act_p1, ncol = n_obs_s)
  for (i in seq_len(n_act)) {
    nb       <- netbin(k_vec[i], m_vec[i])
    FjVc_i  <- beta * comp_apply(kernel, k_vec[i], m_vec[i], Vc_mat %*% t(kernel$Gmat[[nb]]))
    cv0[i, ] <- FjVc_i[cbind(obs_c_e, obs_G_e)]
  }
  # exit row stays 0

  # coeff_p: list n_param of (n_act_p1) x n_obs_s matrices
  cp_list <- setNames(vector("list", n_param), param_names)
  for (pn in param_names) {
    cp <- matrix(0.0, nrow = n_act_p1, ncol = n_obs_s)
    Wp_mat <- matrix(W[[pn]], nrow = C, ncol = N_G)
    for (i in seq_len(n_act)) {
      FjWp_obs_i <- if (max(abs(W[[pn]])) < 1e-15) numeric(n_obs_s) else {
        nb     <- netbin(k_vec[i], m_vec[i])
        FjWp_i <- beta * comp_apply(kernel, k_vec[i], m_vec[i], Wp_mat %*% t(kernel$Gmat[[nb]]))
        FjWp_i[cbind(obs_c_e, obs_G_e)]
      }
      du_dp_i <- numeric(n_obs_s)
      if      (pn == "phi_1")    du_dp_i[obs_G_e == 1L] <- 1.0
      else if (pn == "phi_2")    du_dp_i[obs_G_e == 2L] <- 1.0
      else if (pn == "phi_3")    du_dp_i[obs_G_e == 3L] <- 1.0
      else if (pn == "phi_4")    du_dp_i[obs_G_e == 4L] <- 1.0
      else if (pn == "gamma_p")    du_dp_i <- -prem_ej[[e]][cbind(obs_c_e, rep(i, n_obs_s))]
      else if (pn == "gamma_r")    du_dp_i <- -haz_j[cbind(obs_c_e, rep(i, n_obs_s))] * D_e
      else if (pn == "gamma_pool") du_dp_i <- -(prem_ej[[e]][cbind(obs_c_e, rep(i, n_obs_s))] + haz_j[cbind(obs_c_e, rep(i, n_obs_s))] * D_e)
      else if (pn == "gamma_RB")   du_dp_i <- -rb_e * (prem_ej[[e]][cbind(obs_c_e, rep(i, n_obs_s))] + haz_j[cbind(obs_c_e, rep(i, n_obs_s))] * D_e)
      else if (pn == "c_rem")    du_dp_i <- rep(-k_vec[i], n_obs_s)
      else if (pn == "c_inst")   du_dp_i <- rep(-m_vec[i], n_obs_s)
      else if (pn == paste0("alpha_", ev$g) && ev$type == "FF") du_dp_i <- rep(1.0, n_obs_s)
      cp[i, ] <- du_dp_i + FjWp_obs_i
    }
    if (pn == "kappa_1") cp[n_act_p1, ] <- ss$N_vec[obs_c_e]
    cp_list[[pn]] <- cp
  }

  fo <- matrix(FALSE, nrow = n_act_p1, ncol = n_obs_s)
  for (i in seq_len(n_act)) fo[i, ] <- feas_mask[obs_c_e, i]
  fo[n_act_p1, ] <- TRUE

  list(coeff_v0 = cv0, coeff_p = cp_list, feas_obs = fo,
       sidx_to_srow = setNames(seq_len(n_obs_s), obs_s$sidx),
       agg_e = ae, n_obs_states = n_obs_s)
}

# --- Helper: CCP update over all C*N_G states for env e ---
ccp_update_env <- function(e, theta_k, basis_e) {
  ev    <- env_tbl[e]
  D_e   <- ev$D
  rb_e  <- (ev$type == "RB")
  alpha_e <- if (ev$type == "FF") theta_k[[paste0("alpha_", ev$g)]] else 0.0
  phi_G   <- c(theta_k[["phi_1"]], theta_k[["phi_2"]], theta_k[["phi_3"]], theta_k[["phi_4"]])

  V_mat <- matrix(basis_e$V_const, nrow = C, ncol = N_G)
  for (pn in param_names) V_mat <- V_mat + theta_k[[pn]] * matrix(basis_e$W_list[[pn]], nrow = C, ncol = N_G)

  v_max_mat  <- matrix(-Inf, nrow = C, ncol = N_G)
  v_mats     <- vector("list", n_act_p1)

  for (i in seq_len(n_act)) {
    if (!any(feas_mask[, i])) { v_mats[[i]] <- matrix(-Inf, nrow = C, ncol = N_G); next }
    nb    <- netbin(k_vec[i], m_vec[i])
    # as.matrix: comp_apply returns an S4 dgeMatrix (A_age %*% Y is sparse x dense);
    # coerce so P_new stays base matrices (else pm_op_build throws "Not a matrix" next iter).
    FjV_i <- beta * as.matrix(comp_apply(kernel, k_vec[i], m_vec[i], V_mat %*% t(kernel$Gmat[[nb]])))
    if (use_pooled) {
      E_i   <- prem_ej[[e]][, i] + haz_j[, i] * D_e
      u_vec <- (theta_k[["gamma_pool"]] + rb_e * theta_k[["gamma_RB"]]) * E_i +
                k_vec[i] * theta_k[["c_rem"]] + m_vec[i] * theta_k[["c_inst"]] - alpha_e
    } else {
      u_vec <- theta_k[["gamma_p"]] * prem_ej[[e]][, i] +
               theta_k[["gamma_r"]] * haz_j[, i] * D_e +
               k_vec[i] * theta_k[["c_rem"]] + m_vec[i] * theta_k[["c_inst"]] - alpha_e
    }
    u_mat <- sweep(matrix(phi_G, nrow = C, ncol = N_G, byrow = TRUE), 1L, u_vec, "-")
    v_ij  <- u_mat + FjV_i
    v_ij[!feas_mask[, i], ] <- -Inf
    v_mats[[i]] <- v_ij
    v_max_mat   <- pmax(v_max_mat, v_ij)
  }
  # exit
  v_X <- matrix(theta_k[["kappa_1"]] * ss$N_vec, nrow = C, ncol = N_G)
  v_mats[[n_act_p1]] <- v_X
  v_max_mat <- pmax(v_max_mat, v_X)

  denom <- matrix(0.0, nrow = C, ncol = N_G)
  exp_mats <- lapply(v_mats, function(vm) { em <- exp(vm - v_max_mat); em[vm <= -Inf] <- 0.0; em })
  for (j in seq_len(n_act_p1)) denom <- denom + exp_mats[[j]]

  P_new <- setNames(vector("list", length(all_action_keys)), all_action_keys)
  for (i in seq_len(n_act)) {
    lm <- exp_mats[[i]] / denom
    lm[!feas_mask[, i], ] <- 0.0
    lm[feas_mask[, i], ]  <- pmax(lm[feas_mask[, i], ], eps_prob)
    P_new[[act_keys[i]]]  <- lm
  }
  P_new[["X"]] <- pmax(exp_mats[[n_act_p1]] / denom, eps_prob)
  # re-normalize
  rt <- matrix(0.0, nrow = C, ncol = N_G)
  for (ak in names(P_new)) rt <- rt + P_new[[ak]]
  for (ak in names(P_new)) P_new[[ak]] <- P_new[[ak]] / rt
  P_new
}

# --- NPL outer loop ---
tol_theta    <- 1e-5
tol_P        <- 1e-5
max_npl_iter <- if (nzchar(Sys.getenv("PM08_MAXITER"))) as.integer(Sys.getenv("PM08_MAXITER")) else if (.fast_test) 3L else 10L  # PM08_MAXITER overrides (e.g. =1 to time one full pass)

lower_bounds <- setNames(rep(-50.0, n_param), param_names)
upper_bounds <- setNames(rep( 50.0, n_param), param_names)
if (use_pooled) {
  lower_bounds["gamma_pool"] <- -20.0; upper_bounds["gamma_pool"] <- 20.0
  lower_bounds["gamma_RB"]   <- -20.0; upper_bounds["gamma_RB"]   <- 20.0
} else {
  lower_bounds["gamma_p"] <- -20.0; upper_bounds["gamma_p"] <- 20.0
  lower_bounds["gamma_r"] <- -20.0; upper_bounds["gamma_r"] <- 20.0
}
.nll_scale <- sum(agg_keep$n_obs)   # optimize MEAN log-lik (O(1) gradients) so L-BFGS-B is well-scaled

theta_k    <- setNames(rep(0.0, n_param), param_names)
converged  <- FALSE
trace_dt   <- data.table(iter=integer(), LL=numeric(), dTheta=numeric(), dP=numeric())
bases_warm <- vector("list", n_env)   # warm-start storage

cat(sprintf("  Tolerances: tol_theta=%.0e, tol_P=%.0e, max_iter=%d\n",
            tol_theta, tol_P, max_npl_iter))
cat(sprintf("  Bounds: %s in [-20,20]; rest in [-50,50]\n",
            if (use_pooled) "gamma_pool/gamma_RB" else "gamma_p/gamma_r"))
cat(sprintf("  Starting theta: all zeros\n\n"))

# --- Profile a FF-env fixed effect alpha_g out via its 1-D FOC (concentrated MLE).
# alpha_g enters ONLY env e, linearly; the score d LL_e/d alpha_g is MONOTONE decreasing
# (derivative = -sum_s N_s Var_Lambda[c] <= 0), so the root is found by SAFE BRACKETING
# (uniroot) on [-50,50]. A raw clamped Newton overshoots when the basis amplifies the
# alpha coeff cc (~1/(1-beta)) and gets STUCK at a bound -> spurious +-50 pins + a broken
# envelope gradient. uniroot cannot get stuck. No sign change in the bracket => the env
# genuinely wants a boundary alpha (degenerate, like MO/SD) -> pin at that bound.
profile_alpha_e <- function(e, th_struct_named, ce) {
  if (!use_alpha) return(0.0)                          # PM08_NO_ALPHA: state FE fixed at 0 (no-FE model)
  ev  <- env_tbl[e]; ag <- paste0("alpha_", ev$g)
  cv0 <- ce$coeff_v0; cpe <- ce$coeff_p; fo <- ce$feas_obs
  ae  <- ce$agg_e;    stsr <- ce$sidx_to_srow
  vm_base <- cv0
  for (pn in param_struct) vm_base <- vm_base + th_struct_named[[pn]] * cpe[[pn]]
  vm_base[!fo] <- -Inf
  cc  <- cpe[[ag]]                                  # n_act_p1 x n_obs_s alpha coeff
  sr  <- stsr[as.character(ae$sidx)]; jr <- act_row[ae$action]
  ok  <- !is.na(sr) & !is.na(jr)
  n_ok <- ae$n_obs[ok]; sr_ok <- sr[ok]; jr_ok <- jr[ok]
  T_obs <- sum(n_ok * cc[cbind(jr_ok, sr_ok)])      # observed sufficient stat (constant)
  Ns   <- tapply(n_ok, sr_ok, sum); scols <- as.integer(names(Ns)); Nsv <- as.numeric(Ns)
  score <- function(alpha) {
    vm   <- vm_base + alpha * cc; vm[!fo] <- -Inf
    vmax <- apply(vm, 2L, max)
    expv <- exp(sweep(vm, 2L, vmax, "-")); expv[!fo] <- 0.0
    Lam  <- sweep(expv, 2L, colSums(expv), "/")
    T_obs - sum(Nsv * colSums(Lam * cc)[scols])
  }
  s_lo <- score(-50); s_hi <- score(50)             # decreasing: s_lo >= s_hi
  if (s_lo <= 0) return(-50.0)                       # root <= -50 (degenerate) -> pin low
  if (s_hi >= 0) return( 50.0)                       # root >= +50 (degenerate) -> pin high
  uniroot(score, lower = -50, upper = 50, f.lower = s_lo, f.upper = s_hi, tol = 1e-8)$root
}

# === SECTION: PSOCK CLUSTER (Ticket 024p Lever B) ===
cat("\n=== SECTION: PSOCK CLUSTER ===\n")
.pm08_nw_env <- as.integer(Sys.getenv("PM08_NWORKERS", unset = "0"))
n_workers <- if (.pm08_nw_env > 0L) .pm08_nw_env else
               min(8L, parallel::detectCores(logical = FALSE))
est_gb <- n_workers * 1.5
cat(sprintf("  PSOCK: %d workers, est %.1f GB\n", n_workers, est_gb))
if (est_gb > 20) stop(sprintf("worker RAM estimate %.1f GB exceeds 20 GB ceiling", est_gb))

cl <- parallel::makeCluster(n_workers, type = "PSOCK")
on.exit(parallel::stopCluster(cl), add = TRUE, after = FALSE)

.wk_kernel_path <- normalizePath(here::here("Code", "Helpers", "pm_bellman_kernel.R"))
.wk_cpp_path    <- normalizePath(cpp_path)
parallel::clusterExport(cl, c(".wk_kernel_path", ".wk_cpp_path", "USE_BATCHED_MV"),
                         envir = .GlobalEnv)
parallel::clusterEvalQ(cl, {
  suppressPackageStartupMessages({ library(Matrix); library(data.table); library(Rcpp) })
  source(.wk_kernel_path)
  Rcpp::sourceCpp(.wk_cpp_path)
  invisible(NULL)
})
cat("  Workers initialised (kernel + C++ loaded) [OK]\n")

.static_vars <- c(
  "beta", "C", "N_G", "N_SIDX", "n_act", "n_act_p1", "n_param",
  "eps_prob", "use_alpha", "use_pooled", "USE_BATCHED_MV",
  "kernel", "ss",
  "k_vec", "m_vec", "act_keys", "all_action_keys", "act_row",
  "env_tbl", "param_struct", "param_alpha", "param_names", "ff_envs",
  "imap0", "rmap0", "GaT_list",
  "prem_ej", "haz_j", "feas_mask", "post_cm",
  "ord", "A_perm", "uplo_flag",
  "agg_keep",
  "build_basis_env", "build_coeffs_env", "build_minv_p1", "build_cpp_op", "bicgstab_pc"
)
parallel::clusterExport(cl, .static_vars, envir = .GlobalEnv)
cat(sprintf("  Static vars exported (%d names) [OK]\n", length(.static_vars)))

# Round-robin chunk assignment: env w, w+n_wk, w+2*n_wk, ... -> worker w
.n_wk <- min(n_workers, n_env)
.env_chunks <- lapply(seq_len(.n_wk), function(w) seq(w, n_env, by = .n_wk))
.env_chunks <- Filter(length, .env_chunks)
cat(sprintf("  Env assignment (%d workers): sizes %s\n", .n_wk,
            paste(sapply(.env_chunks, length), collapse = "+")))

# === PM08_VALIDATE_B: in-process serial vs parallel basis check ===
# Run with PM08_VALIDATE_B=1 (+ small PM08_TEST_NENV for speed).
# Compares on identical in-memory P_envs so any deviation is a parallelism bug.
if (nzchar(Sys.getenv("PM08_VALIDATE_B"))) {
  cat("\n=== PM08_VALIDATE_B: serial vs parallel basis check ===\n")
  cat(sprintf("  Comparing all %d available envs on identical P_envs (warm=NULL)\n", n_env))

  cat("  Serial build...\n")
  .t0_ser <- proc.time()["elapsed"]
  .bases_ser <- vector("list", n_env)
  for (.e in seq_len(n_env)) .bases_ser[[.e]] <- build_basis_env(.e, P_envs[[.e]])
  cat(sprintf("  Serial: %.1f sec\n", proc.time()["elapsed"] - .t0_ser))

  cat("  Parallel build (same P_envs, no warm-starts)...\n")
  .val_chunks <- lapply(.env_chunks, function(es)
    lapply(es, function(e) list(e = e, p_e = P_envs[[e]], vc_warm = NULL, w_warm = NULL)))
  .t0_par <- proc.time()["elapsed"]
  .val_res <- parallel::parLapply(cl, .val_chunks, function(wk_chunk) {
    lapply(wk_chunk, function(item)
      list(e = item$e, basis = build_basis_env(item$e, item$p_e)))
  })
  cat(sprintf("  Parallel: %.1f sec\n", proc.time()["elapsed"] - .t0_par))
  .bases_par <- vector("list", n_env)
  for (.wr in .val_res) for (.ir in .wr) .bases_par[[.ir$e]] <- .ir$basis

  .max_dev <- 0.0
  for (.e in seq_len(n_env)) {
    .dv <- max(abs(.bases_ser[[.e]]$V_const - .bases_par[[.e]]$V_const))
    .dw <- max(sapply(param_names, function(pn)
      max(abs(.bases_ser[[.e]]$W_list[[pn]] - .bases_par[[.e]]$W_list[[pn]]))))
    cat(sprintf("  env %d (%s): V_const dev=%.3e  W_list dev=%.3e\n",
                .e, env_tbl$env_key[.e], .dv, .dw))
    .max_dev <- max(.max_dev, .dv, .dw)
  }
  cat(sprintf("  Max dev across all envs: %.3e (tol 1e-8)\n", .max_dev))
  stopifnot(.max_dev < 1e-8)
  cat("  PM08_VALIDATE_B PASS\n\n")
}

t_npl_start <- proc.time()["elapsed"]

for (npl_iter in seq_len(max_npl_iter)) {
  cat(sprintf("[%s] === NPL ITER %d ===\n", format(Sys.time(), "%H:%M:%S"), npl_iter))

  # Build basis for all envs — PSOCK parallel; warm-starts sliced per-worker to avoid broadcast
  cat(sprintf("  Building basis for %d envs (parallel, %d workers)...\n", n_env, n_workers))
  t0_basis <- proc.time()["elapsed"]
  .par_chunks <- lapply(.env_chunks, function(es) lapply(es, function(e) list(
    e       = e,
    p_e     = P_envs[[e]],
    vc_warm = if (!is.null(bases_warm[[e]])) bases_warm[[e]]$V_const else NULL,
    w_warm  = if (!is.null(bases_warm[[e]])) bases_warm[[e]]$W_list  else NULL
  )))
  .res_chunks <- parallel::parLapply(cl, .par_chunks, function(wk_chunk) {
    lapply(wk_chunk, function(item) {
      basis_e  <- build_basis_env(item$e, item$p_e, item$vc_warm, item$w_warm)
      coeffs_e <- build_coeffs_env(item$e, basis_e)
      list(e = item$e, basis = basis_e, coeffs = coeffs_e)
    })
  })
  bases_all  <- vector("list", n_env)
  coeffs_all <- vector("list", n_env)
  for (.wk in .res_chunks)
    for (.it in .wk) { bases_all[[.it$e]] <- .it$basis; coeffs_all[[.it$e]] <- .it$coeffs }
  cat(sprintf("  Basis built for all envs: %.1f sec\n", proc.time()["elapsed"] - t0_basis))
  bases_warm <- bases_all

  # Inner optim: PROFILE alpha_g out per FF env (1-D FOC), optimize the 9 structural
  # params only. Concentrated LL; structural gradient is exact by the envelope theorem
  # (alpha at its FOC => d LL/d alpha = 0 => no cross-term).
  .last_grad_s <- NULL; .last_alphas <- NULL
  fn_nll_s <- function(th_s) {
    th_full <- setNames(numeric(n_param), param_names)
    th_full[param_struct] <- th_s
    for (e in seq_len(n_env)) if (env_tbl[e]$type == "FF") {
      ag <- paste0("alpha_", env_tbl[e]$g)
      th_full[[ag]] <- profile_alpha_e(e, th_full, coeffs_all[[e]])
    }
    .last_alphas <<- th_full[param_alpha]
    LL_tot <- 0.0; grad_s <- setNames(numeric(length(param_struct)), param_struct)
    for (e in seq_len(n_env)) {
      ce <- coeffs_all[[e]]; cv0 <- ce$coeff_v0; cpe <- ce$coeff_p
      fo <- ce$feas_obs; ae <- ce$agg_e; stsr <- ce$sidx_to_srow
      vm <- cv0
      for (p in seq_len(n_param)) vm <- vm + th_full[[p]] * cpe[[p]]
      vm[!fo] <- -Inf
      vm_max  <- apply(vm, 2L, max); expv <- exp(sweep(vm, 2L, vm_max, "-")); expv[!fo] <- 0.0
      denom_e <- colSums(expv); log_den <- log(denom_e) + vm_max
      sr  <- stsr[as.character(ae$sidx)]; jr <- act_row[ae$action]
      ok  <- !is.na(sr) & !is.na(jr)
      LL_tot <- LL_tot + sum(ae$n_obs[ok] * (vm[cbind(jr[ok], sr[ok])] - log_den[sr[ok]]))
      Lam <- sweep(expv, 2L, denom_e, "/"); Lam[!fo] <- 0.0
      for (pn in param_struct) {
        cp_p <- cpe[[pn]]; E_cp <- colSums(Lam * cp_p)
        grad_s[pn] <- grad_s[pn] + sum(ae$n_obs[ok] * (cp_p[cbind(jr[ok], sr[ok])] - E_cp[sr[ok]]))
      }
    }
    .last_grad_s <<- -grad_s
    -LL_tot
  }
  gr_nll_s <- function(th_s) .last_grad_s

  t0_opt <- proc.time()["elapsed"]
  opt    <- optim(theta_k[param_struct], fn_nll_s, gr_nll_s, method = "L-BFGS-B",
                  lower = lower_bounds[param_struct], upper = upper_bounds[param_struct],
                  control = list(maxit = 300L, factr = 1e7, fnscale = .nll_scale))
  if (opt$convergence != 0)
    warning(sprintf("optim convergence code %d at NPL iter %d", opt$convergence, npl_iter))
  invisible(fn_nll_s(opt$par))   # set .last_alphas at the optimum
  theta_new <- setNames(numeric(n_param), param_names)
  theta_new[param_struct] <- opt$par
  theta_new[param_alpha]  <- .last_alphas
  LL_new <- -opt$value
  cat(sprintf("  Inner optim (alpha-profiled): LL=%.4f, convergence=%d, %.1f sec\n",
              LL_new, opt$convergence, proc.time()["elapsed"] - t0_opt))
  cat(sprintf("  Profiled alphas: range [%.4f, %.4f]\n", min(.last_alphas), max(.last_alphas)))

  # Pinning check
  pinned <- param_names[abs(theta_new - lower_bounds) < 1e-6 | abs(theta_new - upper_bounds) < 1e-6]
  if (length(pinned) > 0)
    cat(sprintf("  WARNING: params at bound: %s\n", paste(pinned, collapse = ", ")))

  # CCP update for all states/envs
  cat(sprintf("  Updating CCPs for %d envs...\n", n_env))
  t0_ccp   <- proc.time()["elapsed"]
  P_envs_new <- vector("list", n_env)
  for (e in seq_len(n_env)) P_envs_new[[e]] <- ccp_update_env(e, as.list(theta_new), bases_all[[e]])
  cat(sprintf("  CCP update: %.1f sec\n", proc.time()["elapsed"] - t0_ccp))

  # Convergence metrics
  dTheta <- max(abs(theta_new - theta_k))
  dP_max <- max(sapply(seq_len(n_env), function(e)
    max(sapply(names(P_envs_new[[e]]), function(ak)
      max(abs(P_envs_new[[e]][[ak]] - P_envs[[e]][[ak]]))))))

  trace_dt <- rbind(trace_dt, data.table(iter=npl_iter, LL=LL_new, dTheta=dTheta, dP=dP_max))
  cat(sprintf("[%s] iter %d: LL=%.4f dTheta=%.2e dP=%.2e\n",
              format(Sys.time(), "%H:%M:%S"), npl_iter, LL_new, dTheta, dP_max))
  cat(sprintf("  theta: %s\n",
              paste(sprintf("%s=%.4f", param_names[1:9], theta_new[1:9]), collapse=" ")))

  # Gate D K=1 report
  if (npl_iter == 1L) {
    cat("\n  === K=1 HOTZ-MILLER TWO-STEP ESTIMATES ===\n")
    for (pn in param_names) cat(sprintf("    %-14s = %10.4f\n", pn, theta_new[[pn]]))
    cat("  ===========================================\n\n")
  }

  # Update
  theta_k <- theta_new
  P_envs  <- P_envs_new

  if (dTheta < tol_theta && dP_max < tol_P) {
    converged <- TRUE
    cat(sprintf("  CONVERGED at iter %d (dTheta=%.2e < %.0e AND dP=%.2e < %.0e)\n",
                npl_iter, dTheta, tol_theta, dP_max, tol_P))
    break
  }
}

elapsed_total <- proc.time()["elapsed"] - t_npl_start
cat(sprintf("\n  NPL elapsed: %.1f sec (%.2f hr)\n", elapsed_total, elapsed_total/3600))

# D3: GATE D
cat("\n--- D3: GATE D ---\n")
cat("  Convergence trace:\n")
print(trace_dt)
stopifnot(nrow(trace_dt) >= 1L)   # at least K=1 completed
cat(sprintf("  Converged: %s\n", if (converged) "YES" else "NO (hit max_iter)"))
cat("  GATE D PASS\n")

# ============================================================
# SECTION: SAVE FIT
# ============================================================
cat("\n=== SECTION: SAVE FIT ===\n")

sample_label <- paste0(
  if (use_pooled) "pooled" else "two_gamma",
  if (use_alpha)  "_FE_on" else "_FE_off"
)
fit_path <- here::here("Output", "Estimation_Results",
  sprintf("Model_Portfolio_v4_%s_observed_%s.rds",
          sample_label, format(Sys.Date(), "%Y%m%d")))

fit_obj <- list(
  theta_hat   = theta_k,
  converged   = converged,
  n_iter      = nrow(trace_dt),
  LL          = if (nrow(trace_dt) > 0L) tail(trace_dt$LL, 1L) else NA_real_,
  trace_dt    = trace_dt,
  param_names = param_names,
  use_pooled  = use_pooled,
  beta        = beta,
  env_keys    = env_tbl$env_key,
  env_types   = env_tbl$type,
  D_vec       = env_tbl$D,
  tol_theta   = tol_theta,
  tol_P       = tol_P
)
saveRDS(fit_obj, fit_path)

cat(sprintf("Estimator: PM08_v4_%s | Sample: observed | Converged: %s at iter %d | LL: %.4f\n",
            sample_label, if (converged) "T" else "F", nrow(trace_dt), fit_obj$LL))
cat(sprintf("theta_hat: %s\n",
            paste(sprintf("%s=%.4f", param_names[seq_len(9L)], theta_k[seq_len(9L)]),
                  collapse = " ")))
cat(sprintf("Elapsed: %.1fs | Saved: %s\n", elapsed_total, fit_path))
cat("=== SAVE COMPLETE ===\n")
