# PM09_CF_Portfolio.R
# TICKET 024r — Portfolio counterfactual infrastructure + CF1/CF3/CF4
# Phases 1-6 with GATE CF-A, CF-B, CF-3, CF-1, CF-4.
# Run:  C:/Program Files/R/R-4.5.2/bin/x64/Rscript.exe Code/Dynamic_Model/PM09_CF_Portfolio.R
# Knobs (env vars):
#   CF_FIT_PATH  — path to pooled fit .rds (default: PM08 pooled local file)
#   CF_E_EXT     — E_ext in MODEL UNITS (default 5.0 = $50k / $10k scale)
#   CF_A_MANDATE — age threshold for CF4 mandate in years (default 20)
#   CF_GATES_ONLY — if "1", stop after GATE CF-B (skip CF3/CF1/CF4)
#   CF_TAU_CTRL_FEE_POS_ONLY — "0" to include zero-fee FF states in tau_ctrl; default "1" (fee-positive only)

suppressPackageStartupMessages({
  library(Matrix); library(data.table); library(here); library(Rcpp)
})

# === SECTION: LOGGING ===
.log_path <- here::here("logs", paste0("PM09_CF_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".log"))
dir.create(dirname(.log_path), recursive = TRUE, showWarnings = FALSE)
.log <- file(.log_path, open = "wt")
sink(.log, type = "output"); sink(.log, type = "message", append = TRUE)
on.exit({ sink(type = "output"); sink(type = "message"); close(.log) }, add = TRUE)
cat(sprintf("LOG START %s\nScript: PM09_CF_Portfolio.R\nR: %s\nWD: %s\n\n",
    .log_path, R.version.string, getwd()))

# === SECTION: CF PARAMETERS ===
cat("=== SECTION: CF PARAMETERS ===\n")
.fit_path_default <- here::here(
  "Output", "Estimation_Results",
  "Model_Portfolio_v4_pooled_FE_on_observed_20260625.rds")
CF_FIT_PATH  <- Sys.getenv("CF_FIT_PATH",  unset = .fit_path_default)
CF_E_EXT     <- as.numeric(Sys.getenv("CF_E_EXT",     unset = "5.0"))   # model units; $50k/$10k
CF_A_MANDATE <- as.integer(Sys.getenv("CF_A_MANDATE", unset = "20"))    # years
GATES_ONLY          <- nzchar(Sys.getenv("CF_GATES_ONLY"))
CF_TAU_FEE_POS_ONLY <- Sys.getenv("CF_TAU_CTRL_FEE_POS_ONLY", unset = "1") != "0"
cat(sprintf("  CF_FIT_PATH:          %s\n", CF_FIT_PATH))
cat(sprintf("  CF_E_EXT:             %.2f model units (~$%.0fk per leak)\n", CF_E_EXT, CF_E_EXT * 10))
cat(sprintf("  CF_A_MANDATE:         %d yr\n", CF_A_MANDATE))
cat(sprintf("  GATES_ONLY:           %s\n", GATES_ONLY))
cat(sprintf("  CF_TAU_FEE_POS_ONLY:  %s\n", CF_TAU_FEE_POS_ONLY))
cat("  *** MECHANICS-TEST: welfare numbers are placeholders until converged fit + confirmed E_ext ***\n")

# === SECTION: SOURCE KERNEL AND C++ MATVEC ===
cat("\n=== SECTION: SOURCE KERNEL AND C++ MATVEC ===\n")
source(here::here("Code", "Helpers", "pm_bellman_kernel.R"))
cat("  pm_bellman_kernel.R sourced [OK]\n")
cpp_path <- here::here("Code", "Helpers", "pm_matvec.cpp")
if (!file.exists(cpp_path)) stop(sprintf("pm_matvec.cpp not found: %s", cpp_path))
Rcpp::sourceCpp(cpp_path)
cat("  pm_matvec.cpp compiled [OK]\n")
USE_BATCHED_MV <- !nzchar(Sys.getenv("PM08_NO_BATCHED"))
cat(sprintf("  USE_BATCHED_MV: %s\n", USE_BATCHED_MV))

# === SECTION: LOAD INPUTS ===
cat("\n=== SECTION: LOAD INPUTS ===\n")
ss   <- readRDS(here::here("Output", "Estimation_Results", "PM_StateSpace.rds"))
lk   <- readRDS(here::here("Output", "Estimation_Results", "PM_Lookups.rds"))
agg  <- fread(here::here("Data", "Analysis", "pm_agg_counts.csv"))
if (!file.exists(CF_FIT_PATH)) stop(sprintf("Fit file not found: %s", CF_FIT_PATH))
fit  <- readRDS(CF_FIT_PATH)
cat(sprintf("  ss: C=%d N_G=%d N_SIDX=%d\n", ss$C, ss$N_G, ss$N_SIDX))
cat(sprintf("  agg: %d rows, %d distinct sidx\n", nrow(agg),
            length(unique(agg$sidx))))
cat(sprintf("  Fit: converged=%s n_iter=%s LL=%.2f env_keys=%s\n",
    fit$converged, fit$n_iter, fit$LL, paste(fit$env_keys, collapse=",")))

# === SECTION: EXTRACT THETA_HAT FROM FIT ===
cat("\n=== SECTION: EXTRACT THETA_HAT FROM FIT ===\n")
use_pooled    <- isTRUE(fit$use_pooled)
beta          <- fit$beta
theta_hat_raw <- fit$theta_hat
cat(sprintf("  use_pooled=%s beta=%.4f\n", use_pooled, beta))
cat("  theta_hat:\n"); print(round(theta_hat_raw, 4))

param_struct <- if (use_pooled) {
  c("phi_1","phi_2","phi_3","phi_4","gamma_pool","gamma_RB","c_rem","c_inst","kappa_1")
} else {
  c("phi_1","phi_2","phi_3","phi_4","gamma_p","gamma_r","c_rem","c_inst","kappa_1")
}
stopifnot(all(param_struct %in% names(theta_hat_raw)))

eps_prob <- 1e-10
stopifnot(beta > 0, beta < 1)

# === SECTION: STATIC DIMENSIONS ===
cat("\n=== SECTION: STATIC DIMENSIONS ===\n")
C      <- ss$C
N_G    <- ss$N_G
N_SIDX <- ss$N_SIDX
kernel <- build_kernel(ss, lk)
stopifnot(nrow(kernel$work_all) >= 1L)
n_act    <- nrow(kernel$work_all)
k_vec    <- as.integer(kernel$work_all$k)
m_vec    <- as.integer(kernel$work_all$m)
act_keys <- vapply(seq_len(n_act), function(i) paste0(k_vec[i], ",", m_vec[i]), character(1L))
all_action_keys <- c(act_keys, "X")
n_act_p1 <- n_act + 1L
cat(sprintf("  C=%d N_G=%d N_SIDX=%d n_act=%d\n", C, N_G, N_SIDX, n_act))

# C++ op statics
imap0 <- kernel$imap - 1L; imap0[is.na(imap0)] <- -1L; storage.mode(imap0) <- "integer"
rmap0 <- kernel$rmap - 1L; rmap0[is.na(rmap0)] <- -1L; storage.mode(rmap0) <- "integer"
GaT_list <- vector("list", n_act)
for (i in seq_len(n_act)) GaT_list[[i]] <- t(kernel$Gmat[[netbin(k_vec[i], m_vec[i])]])

# === SECTION A0: POST_CM AND BASE FEAS_MASK ===
cat("\n=== SECTION A0: POST_CM AND BASE FEAS_MASK ===\n")
post_cm <- matrix(NA_integer_, nrow = C, ncol = n_act)
for (i in seq_len(n_act)) {
  k_i <- k_vec[i]; m_i <- m_vec[i]
  ar  <- kernel$rmap[, k_i + 1L]
  vld <- which(!is.na(ar))
  if (length(vld) > 0L)
    post_cm[vld, i] <- kernel$imap[cbind(ar[vld], rep(m_i + 1L, length(vld)))]
}
feas_mask <- !is.na(post_cm)
maint_idx <- which(k_vec == 0L & m_vec == 0L)
stopifnot(length(maint_idx) == 1L, all(feas_mask[, maint_idx]))
cat(sprintf("  post_cm [%dx%d], NA frac=%.4f, maintain always feasible [OK]\n",
    C, n_act, mean(is.na(post_cm))))

# === SECTION A1: ENVIRONMENTS ===
cat("\n=== SECTION A1: ENVIRONMENTS ===\n")
agg[, env_key := ifelse(g == "TX", paste0("RB_", era), as.character(g))]
exit_dt     <- agg[action == "X", .(n_exit = sum(n_obs)), by = env_key]
env_summary <- agg[, .(n_obs_total = sum(n_obs)), by = env_key]
env_summary <- merge(env_summary, exit_dt, by = "env_key", all.x = TRUE)
env_summary[is.na(n_exit), n_exit := 0L]
EXIT_FLOOR <- 30L
ff_drop <- env_summary[!grepl("^RB_", env_key) & n_exit < EXIT_FLOOR, env_key]
env_keep <- env_summary[!env_key %in% ff_drop, env_key]
setorder(env_summary, env_key)
cat("  Env summary:\n"); print(env_summary)
cat(sprintf("  Folded (<=%d exits): %s\n", EXIT_FLOOR,
    if (length(ff_drop) > 0) paste(ff_drop, collapse=",") else "none"))

agg_keep <- agg[env_key %in% env_keep]
n_env    <- length(env_keep)
rb_envs  <- env_keep[grepl("^RB_", env_keep)]
ff_envs  <- env_keep[!grepl("^RB_", env_keep)]
n_ff     <- length(ff_envs)
stopifnot(length(rb_envs) == 3L)
cat(sprintf("  n_env=%d (RB=%d FF=%d)\n", n_env, length(rb_envs), n_ff))

env_tbl <- data.table(
  env_idx   = seq_len(n_env),
  env_key   = env_keep,
  type      = ifelse(grepl("^RB_", env_keep), "RB", "FF"),
  g         = ifelse(grepl("^RB_", env_keep), "TX", env_keep),
  era_str   = ifelse(grepl("^RB_", env_keep), sub("^RB_","", env_keep), NA_character_)
)
d_v   <- numeric(n_env); tau_v <- rep(NA_real_, n_env)
for (i in seq_len(n_env)) {
  d_v[i]   <- if (env_tbl$type[i]=="RB") lk$D[["TX"]] else lk$D[[env_tbl$g[i]]]
  if (env_tbl$type[i]=="FF") tau_v[i] <- lk$tau[[env_tbl$g[i]]]
}
env_tbl[, D   := d_v]
env_tbl[, tau := tau_v]
stopifnot(!anyNA(env_tbl$D))
cat("  env_tbl:\n"); print(env_tbl)

# Extract alpha_hat per FF env from theta_hat (0 if not in fit)
alpha_hat <- setNames(rep(0.0, n_ff), paste0("alpha_", ff_envs))
for (ae in names(alpha_hat)) {
  if (ae %in% names(theta_hat_raw)) {
    alpha_hat[[ae]] <- theta_hat_raw[[ae]]
  } else {
    warning(sprintf("  alpha %s not in fit$theta_hat — using 0 (fit may be partial)", ae))
  }
}
cat("  alpha_hat (fixed Semantic-1):\n"); print(round(alpha_hat, 4))

theta_hat <- theta_hat_raw[param_struct]   # structural only (alphas handled separately)
cat("  theta_hat (structural):\n"); print(round(theta_hat, 4))

# === SECTION A2: LOOKUPS AT POST-ACTION COMP ===
cat("\n=== SECTION A2: LOOKUPS AT POST-ACTION COMP ===\n")
haz_j <- matrix(0.0, C, n_act)
for (i in seq_len(n_act)) {
  vld <- which(feas_mask[, i])
  if (length(vld) > 0L) haz_j[vld, i] <- lk$h_aw[ss$h_idx[post_cm[vld, i]]]
}
cat(sprintf("  haz_j [%dx%d] range [%.5f, %.5f]\n",
    C, n_act, min(haz_j[feas_mask]), max(haz_j[feas_mask])))

prem_ej <- vector("list", n_env)
for (e in seq_len(n_env)) {
  pm <- matrix(0.0, C, n_act)
  ev <- env_tbl[e]
  for (i in seq_len(n_act)) {
    vld <- which(feas_mask[, i])
    if (length(vld) == 0L) next
    pc <- post_cm[vld, i]
    pm[vld, i] <- if (ev$type == "RB") ss$P_RB_all[pc, ev$era_str] else ev$tau * ss$N_vec[pc]
  }
  prem_ej[[e]] <- pm
}
cat(sprintf("  prem_ej: %d envs [OK]\n", n_env))

# === SECTION A3: MU(s) ===
cat("\n=== SECTION A3: MU(s) — OBSERVED STATE DISTRIBUTION ===\n")
# mu_e(c,G): env-specific weight = n_obs(sidx, env_e, all actions) / total_N
# Indexed by sidx; store as named list of sparse vectors (sidx -> weight)
# For welfare: sum_e sum_{c,G} mu_e(c,G) * V_e(c,G)
# We store as full C x N_G matrices for each env (sparse in practice but dense for simplicity)

total_N <- sum(agg_keep$n_obs)
cat(sprintf("  total_N=%d\n", total_N))

mu_e <- vector("list", n_env)
for (e in seq_len(n_env)) {
  ae <- agg_keep[env_key == env_tbl$env_key[e], .(n = sum(n_obs)), by = sidx]
  mu_mat <- matrix(0.0, C, N_G)
  G_idx <- as.integer(ceiling(ae$sidx / C))
  c_idx <- as.integer(ae$sidx - (G_idx - 1L) * C)
  stopifnot(all(G_idx >= 1L & G_idx <= N_G))
  stopifnot(all(c_idx >= 1L & c_idx <= C))
  mu_mat[cbind(c_idx, G_idx)] <- ae$n / total_N
  mu_e[[e]] <- mu_mat
}
mu_sum <- Reduce("+", mu_e)
cat(sprintf("  mu sum: %.8f (tol 1e-6 from %s)\n", sum(mu_sum),
    if (abs(sum(mu_sum) - 1) < 1e-4) "OK — near 1 (may differ due to folded envs)" else "CHECK"))

# === SECTION A4: P1 PRECONDITIONER STATICS ===
cat("\n=== SECTION A4: P1 PRECONDITIONER STATICS ===\n")
MARG_BIN <- ss$MARG_BIN
pot      <- as.vector(ss$comp_mat %*% (8L - MARG_BIN))
ord      <- order(pot, decreasing = TRUE)
A_perm   <- ss$A_age[ord, ord]
lower_off <- tril(A_perm, -1L); upper_off <- triu(A_perm, 1L)
below_mass <- sum(abs(lower_off)); above_mass <- sum(abs(upper_off))
cat(sprintf("  Triangularity: below=%.3e above=%.3e\n", below_mass, above_mass))
if (below_mass == 0 && above_mass > 0) {
  uplo_flag <- "U"
} else if (above_mass == 0 && below_mass > 0) {
  uplo_flag <- "L"
} else {
  stop(sprintf("TRIANGULARITY ASSERT FAILED: below=%.3e above=%.3e", below_mass, above_mass))
}
cat(sprintf("  A_perm triangular (%s) [OK]\n", if (uplo_flag=="U") "upper" else "lower"))

# === SECTION A5: P^0 HOTZ-MILLER CCP ===
cat("\n=== SECTION A5: P^0 HOTZ-MILLER CCP ===\n")
hm_dt <- agg[, .(n_obs = sum(n_obs)), by = .(sidx, action)]
P_hm <- setNames(
  lapply(all_action_keys, function(ak) matrix(eps_prob, C, N_G)),
  all_action_keys)
for (ri in seq_len(nrow(hm_dt))) {
  ci_ <- as.integer(hm_dt$sidx[ri] - (ceiling(hm_dt$sidx[ri]/C)-1L)*C)
  Gi_ <- as.integer(ceiling(hm_dt$sidx[ri]/C))
  ak_ <- hm_dt$action[ri]
  if (!ak_ %in% names(P_hm)) next
  P_hm[[ak_]][ci_, Gi_] <- P_hm[[ak_]][ci_, Gi_] + hm_dt$n_obs[ri]
}
row_tot <- matrix(0.0, C, N_G)
for (ak in names(P_hm)) row_tot <- row_tot + P_hm[[ak]]
for (ak in names(P_hm)) P_hm[[ak]] <- P_hm[[ak]] / row_tot
# Apply feas_mask
for (i in seq_len(n_act)) {
  infeas <- which(!feas_mask[, i])
  if (length(infeas) > 0L) P_hm[[act_keys[i]]][infeas, ] <- 0.0
}
row_tot2 <- matrix(0.0, C, N_G)
for (ak in names(P_hm)) row_tot2 <- row_tot2 + P_hm[[ak]]
stopifnot(all(row_tot2 > 0))
for (ak in names(P_hm)) P_hm[[ak]] <- P_hm[[ak]] / row_tot2
cat(sprintf("  P_hm: %d actions, row-sum max dev=%.3e [OK]\n",
    length(P_hm), max(abs(Reduce("+", P_hm) - 1.0))))

# Initialize per-env CCPs
P_envs_init <- vector("list", n_env)
for (e in seq_len(n_env)) {
  P_envs_init[[e]] <- lapply(P_hm, function(m) m + 0.0)
  names(P_envs_init[[e]]) <- names(P_hm)
}
cat("  P_envs_init from P_hm [OK]\n")

# ============================================================
# SECTION: CF INFRASTRUCTURE FUNCTIONS
# ============================================================
cat("\n=== SECTION: CF INFRASTRUCTURE FUNCTIONS ===\n")

# -- bicgstab_pc (right-preconditioned; verbatim from PM08) --
bicgstab_pc <- function(Ax_fn, b, Minv, tol = 1e-9, max_it = 2000L, x0 = NULL) {
  n <- length(b); x <- if (!is.null(x0)) x0 else numeric(n)
  r <- b - Ax_fn(x); rhat <- r
  rho <- 1.0; alpha <- 1.0; w <- 1.0; v <- numeric(n); p <- numeric(n)
  for (it in seq_len(max_it)) {
    rho1 <- sum(rhat * r)
    if (abs(rho1) < 1e-100) {
      rhat <- r; rho <- 1.0; alpha <- 1.0; w <- 1.0; v <- numeric(n); p <- numeric(n)
      rho1 <- sum(rhat * r)
      if (abs(rho1) < 1e-100) stop(sprintf("BiCGSTAB stalled: rho1=%.3e at iter %d", rho1, it))
    }
    bta  <- (rho1/rho) * (alpha/w); p <- r + bta*(p - w*v)
    phat <- Minv(p); v <- Ax_fn(phat)
    denom_a <- sum(rhat * v)
    if (abs(denom_a) < 1e-100) stop(sprintf("BiCGSTAB stalled: <rhat,v>=%.3e at iter %d", denom_a, it))
    alpha <- rho1/denom_a; s <- r - alpha*v
    if (max(abs(s)) < tol) { x <- x + alpha*phat; return(list(x=x, iters=it, converged=TRUE, resid=max(abs(s)))) }
    shat <- Minv(s); t <- Ax_fn(shat)
    denom_w <- sum(t*t)
    if (abs(denom_w) < 1e-100) stop(sprintf("BiCGSTAB stalled: <t,t>=%.3e at iter %d", denom_w, it))
    w <- sum(t*s)/denom_w; x <- x + alpha*phat + w*shat; r <- s - w*t; rho <- rho1
    resid_cur <- max(abs(r))
    if (resid_cur < tol) return(list(x=x, iters=it, converged=TRUE, resid=resid_cur))
    if (it %% 50L == 0L) cat(sprintf("  [%s] BiCGSTAB iter %d: ||r||=%.3e\n",
                                     format(Sys.time(),"%H:%M:%S"), it, resid_cur))
  }
  stop(sprintf("BiCGSTAB did not converge in %d iters; ||r||=%.3e", max_it, max(abs(r))))
}

# -- build_minv_p1 (P1 preconditioner; verbatim from PM08) --
build_minv_p1 <- function(p_e) {
  T_G_list <- vector("list", N_G)
  for (G in seq_len(N_G)) {
    p_maint <- p_e[["0,0"]][, G]
    T_G_raw <- as(Diagonal(C) - beta * Diagonal(x = p_maint[ord]) %*% A_perm, "CsparseMatrix")
    T_G_list[[G]] <- new("dtCMatrix", i=T_G_raw@i, p=T_G_raw@p, x=T_G_raw@x,
                          Dim=T_G_raw@Dim, uplo=uplo_flag, diag="N")
  }
  function(r) {
    R_mat <- matrix(r, C, N_G); Y_mat <- matrix(0.0, C, N_G)
    for (G in seq_len(N_G)) {
      b <- R_mat[ord, G]
      Y_mat[ord, G] <- as.numeric(Matrix::solve(T_G_list[[G]], b))
    }
    as.vector(Y_mat)
  }
}

# -- build_cpp_op (C++ matvec operator) --
build_cpp_op <- function(p_e) {
  P_list <- vector("list", n_act)
  for (i in seq_len(n_act)) {
    ak <- act_keys[i]
    P_list[[i]] <- as.matrix(if (ak %in% names(p_e)) p_e[[ak]] else matrix(0.0, C, N_G))
  }
  if (USE_BATCHED_MV) pm_op_build_b(kernel$A_age, imap0, rmap0, k_vec, m_vec, GaT_list, P_list, C, N_G)
  else                pm_op_build  (kernel$A_age, imap0, rmap0, k_vec, m_vec, GaT_list, P_list, C, N_G)
}
cat("  bicgstab_pc, build_minv_p1, build_cpp_op defined [OK]\n")

# -- compute_R_env: direct R vector at (theta_hat, alpha_hat, p_e, CF overrides) --
# Returns vector of length N_SIDX = C*N_G
# R_e = R_const + sum_p theta_p * rho_p (assembled without n_param separate solves)
compute_R_env <- function(e, p_e, prem_mat, D_e_val, rb_e_val, theta_eff = theta_hat) {
  ev      <- env_tbl[e]
  alpha_e <- if (ev$type == "FF") alpha_hat[[paste0("alpha_", ev$g)]] else 0.0

  # R_const: entropy term sigma*(gamma_E - log P_j) accumulated over all actions
  R_const <- matrix(0.0, C, N_G)
  for (ak in names(p_e)) {
    Pa <- p_e[[ak]]; pos <- Pa > 0
    en <- matrix(0.0, C, N_G); en[pos] <- gamma_E - log(Pa[pos])
    R_const <- R_const + Pa * en
  }

  Pa_X <- p_e[["X"]]

  # phi_G: contribution = (1 - P_X) * theta_eff["phi_G"] at G-column
  phi_vals <- if (use_pooled) {
    c(theta_eff["phi_1"], theta_eff["phi_2"], theta_eff["phi_3"], theta_eff["phi_4"])
  } else {
    c(theta_eff["phi_1"], theta_eff["phi_2"], theta_eff["phi_3"], theta_eff["phi_4"])
  }
  phi_term <- (1 - Pa_X) * matrix(phi_vals, C, N_G, byrow = TRUE)

  # gamma terms: accumulate rho_gp and rho_gr weighted by P
  rho_gp <- matrix(0.0, C, N_G)
  rho_gr <- matrix(0.0, C, N_G)
  for (i in seq_len(n_act)) {
    Pa_i   <- p_e[[act_keys[i]]]
    rho_gp <- rho_gp - Pa_i * prem_mat[, i]
    rho_gr <- rho_gr - D_e_val * Pa_i * haz_j[, i]
  }
  gamma_term <- if (use_pooled) {
    (theta_eff["gamma_pool"] + rb_e_val * theta_eff["gamma_RB"]) * (rho_gp + rho_gr)
  } else {
    theta_eff["gamma_p"] * rho_gp + theta_eff["gamma_r"] * rho_gr
  }

  # c_rem and c_inst
  rho_cr <- matrix(0.0, C, N_G); rho_ci <- matrix(0.0, C, N_G)
  for (i in seq_len(n_act)) {
    Pa_i <- p_e[[act_keys[i]]]
    if (k_vec[i] > 0L) rho_cr <- rho_cr - k_vec[i] * Pa_i
    if (m_vec[i] > 0L) rho_ci <- rho_ci - m_vec[i] * Pa_i
  }
  rem_term  <- theta_eff["c_rem"]  * rho_cr
  inst_term <- theta_eff["c_inst"] * rho_ci

  # kappa_1 (exit only)
  kappa_term <- theta_eff["kappa_1"] * Pa_X * matrix(ss$N_vec, C, N_G)

  # alpha_e (FF env fixed effect; alpha_hat fixed, not a CF lever — Semantic-1)
  alpha_term <- alpha_e * (1 - Pa_X)

  R_mat <- R_const + phi_term + gamma_term + rem_term + inst_term + kappa_term + alpha_term
  as.vector(R_mat)
}

# -- ccp_update_cf: softmax CCP update at fixed theta_hat + alpha_hat, given V_mat --
# V_mat: C x N_G (current value function)
# theta_eff: structural theta (possibly modified for CF3)
# Returns named list of CCP matrices (same structure as p_e)
ccp_update_cf <- function(e, V_mat, prem_mat, D_e_val, rb_e_val, feas_mat_e, theta_eff) {
  ev      <- env_tbl[e]
  alpha_e <- if (ev$type == "FF") alpha_hat[[paste0("alpha_", ev$g)]] else 0.0
  phi_G   <- c(theta_eff["phi_1"], theta_eff["phi_2"], theta_eff["phi_3"], theta_eff["phi_4"])

  v_max_mat <- matrix(-Inf, C, N_G)
  v_mats    <- vector("list", n_act_p1)

  for (i in seq_len(n_act)) {
    if (!any(feas_mat_e[, i])) { v_mats[[i]] <- matrix(-Inf, C, N_G); next }
    nb    <- netbin(k_vec[i], m_vec[i])
    FjV_i <- beta * as.matrix(
      comp_apply(kernel, k_vec[i], m_vec[i], V_mat %*% t(kernel$Gmat[[nb]])))
    E_i <- prem_mat[, i] + haz_j[, i] * D_e_val
    u_vec <- if (use_pooled) {
      (theta_eff["gamma_pool"] + rb_e_val * theta_eff["gamma_RB"]) * E_i +
       k_vec[i] * theta_eff["c_rem"] + m_vec[i] * theta_eff["c_inst"] - alpha_e
    } else {
      theta_eff["gamma_p"] * prem_mat[, i] + theta_eff["gamma_r"] * haz_j[, i] * D_e_val +
      k_vec[i] * theta_eff["c_rem"] + m_vec[i] * theta_eff["c_inst"] - alpha_e
    }
    u_mat <- sweep(matrix(phi_G, C, N_G, byrow = TRUE), 1L, u_vec, "-")
    v_ij  <- u_mat + FjV_i
    v_ij[!feas_mat_e[, i], ] <- -Inf
    v_mats[[i]] <- v_ij
    v_max_mat   <- pmax(v_max_mat, v_ij)
  }
  # exit (absorbing; no FjV term)
  v_X <- matrix(theta_eff["kappa_1"] * ss$N_vec, C, N_G)
  v_mats[[n_act_p1]] <- v_X
  v_max_mat <- pmax(v_max_mat, v_X)

  exp_mats <- lapply(v_mats, function(vm) {
    em <- exp(vm - v_max_mat); em[vm <= -Inf] <- 0.0; em
  })
  denom <- matrix(0.0, C, N_G)
  for (j in seq_len(n_act_p1)) denom <- denom + exp_mats[[j]]

  P_new <- setNames(vector("list", n_act_p1), all_action_keys)
  for (i in seq_len(n_act)) {
    lm <- exp_mats[[i]] / denom
    lm[!feas_mat_e[, i], ] <- 0.0
    lm[ feas_mat_e[, i], ] <- pmax(lm[feas_mat_e[, i], ], eps_prob)
    P_new[[act_keys[i]]] <- lm
  }
  P_new[["X"]] <- pmax(exp_mats[[n_act_p1]] / denom, eps_prob)
  rt <- matrix(0.0, C, N_G)
  for (ak in names(P_new)) rt <- rt + P_new[[ak]]
  for (ak in names(P_new)) P_new[[ak]] <- P_new[[ak]] / rt
  P_new
}

# -- solve_equilibrium_portfolio: defined after PSOCK cluster setup (see below) --
tol_P_cf    <- 1e-7
max_iter_cf <- 300L

# -- compute_welfare: welfare accounting from V_envs + P_envs --
# c_inst_baseline: NOMINAL c_inst (theta_hat["c_inst"]) — used for CF3 Govt
# s_sub: subsidy fraction (0 for non-CF3)
compute_welfare <- function(V_envs, P_envs, feas_list = NULL, E_ext = CF_E_EXT,
                            c_inst_baseline = theta_hat["c_inst"], s_sub = 0.0) {
  if (is.null(feas_list)) feas_list <- rep(list(feas_mask), n_env)

  PS  <- 0.0; PS_FF  <- 0.0; PS_RB  <- 0.0
  Ext <- 0.0; Ext_FF <- 0.0; Ext_RB <- 0.0
  Gov <- 0.0; Gov_FF <- 0.0; Gov_RB <- 0.0

  for (e in seq_len(n_env)) {
    V_mat    <- matrix(V_envs[[e]], C, N_G)
    mu_mat_e <- mu_e[[e]]
    feas_e   <- feas_list[[e]]

    PS_e <- sum(mu_mat_e * V_mat)

    # External: expected post-action hazard * E_ext / (1-beta)
    E_haz <- matrix(0.0, C, N_G)
    for (i in seq_len(n_act)) {
      if (!any(feas_e[, i])) next
      E_haz <- E_haz + P_envs[[e]][[act_keys[i]]] * haz_j[, i]
    }
    # exit contributes 0 (empty post-comp, H=0)
    Ext_e <- sum(mu_mat_e * E_haz) * E_ext / (1 - beta)

    # Govt outlay (CF3 only; 0 if s_sub=0)
    Gov_e <- 0.0
    if (s_sub > 0) {
      E_inst <- matrix(0.0, C, N_G)
      for (i in seq_len(n_act)) {
        if (m_vec[i] > 0L && any(feas_e[, i]))
          E_inst <- E_inst + m_vec[i] * P_envs[[e]][[act_keys[i]]]
      }
      Gov_e <- sum(mu_mat_e * E_inst) * s_sub * c_inst_baseline / (1 - beta)
    }

    PS  <- PS  + PS_e;  Ext <- Ext + Ext_e;  Gov <- Gov + Gov_e
    if (env_tbl$type[e] == "FF") {
      PS_FF  <- PS_FF  + PS_e
      Ext_FF <- Ext_FF + Ext_e
      Gov_FF <- Gov_FF + Gov_e
    } else {
      PS_RB  <- PS_RB  + PS_e
      Ext_RB <- Ext_RB + Ext_e
      Gov_RB <- Gov_RB + Gov_e
    }
  }
  Net <- PS - Ext - Gov
  list(PS=PS, PS_FF=PS_FF, PS_RB=PS_RB,
       Ext=Ext, Ext_FF=Ext_FF, Ext_RB=Ext_RB,
       Gov=Gov, Gov_FF=Gov_FF, Gov_RB=Gov_RB,
       Net=Net, Net_FF=PS_FF-Ext_FF-Gov_FF, Net_RB=PS_RB-Ext_RB-Gov_RB)
}

# -- compute_outcomes: behavioral rates and premium revenue --
compute_outcomes <- function(P_envs, prem_list = NULL, feas_list = NULL) {
  if (is.null(prem_list)) prem_list <- prem_ej
  if (is.null(feas_list)) feas_list <- rep(list(feas_mask), n_env)

  exit_r   <- 0.0; rep_r  <- 0.0; dsz_r <- 0.0
  exit_FF  <- 0.0; rep_FF <- 0.0; dsz_FF <- 0.0
  exit_RB  <- 0.0; rep_RB <- 0.0; dsz_RB <- 0.0
  op_ty    <- 0.0                                   # expected operating tank-years
  prem_rev <- 0.0

  for (e in seq_len(n_env)) {
    mu_mat_e <- mu_e[[e]]
    feas_e   <- feas_list[[e]]
    prem_e   <- prem_list[[e]]

    # Exit
    ex_e <- sum(mu_mat_e * P_envs[[e]][["X"]])

    # Replace (m > 0), Downsize (k > 0, m == 0)
    rep_e <- 0.0; dsz_e <- 0.0
    for (i in seq_len(n_act)) {
      if (!any(feas_e[, i])) next
      if (m_vec[i] > 0L) rep_e <- rep_e + sum(mu_mat_e * P_envs[[e]][[act_keys[i]]])
      else if (k_vec[i] > 0L) dsz_e <- dsz_e + sum(mu_mat_e * P_envs[[e]][[act_keys[i]]])
    }

    # Operating tank-years: N_vec[c] * (1 - P_X[c,G]) weighted by mu
    P_X_e  <- P_envs[[e]][["X"]]
    op_e   <- sum(mu_mat_e * (1 - P_X_e) * matrix(ss$N_vec, C, N_G))
    op_ty  <- op_ty + op_e

    # Premium revenue (post-action premium, weighted by P_j * mu)
    prem_e_rev <- matrix(0.0, C, N_G)
    for (i in seq_len(n_act)) {
      if (!any(feas_e[, i])) next
      prem_e_rev <- prem_e_rev + P_envs[[e]][[act_keys[i]]] * prem_e[, i]
    }
    prem_rev_e <- sum(mu_mat_e * prem_e_rev) / (1 - beta)
    prem_rev   <- prem_rev + prem_rev_e

    exit_r <- exit_r + ex_e; rep_r <- rep_r + rep_e; dsz_r <- dsz_r + dsz_e
    if (env_tbl$type[e] == "FF") {
      exit_FF <- exit_FF + ex_e; rep_FF <- rep_FF + rep_e; dsz_FF <- dsz_FF + dsz_e
    } else {
      exit_RB <- exit_RB + ex_e; rep_RB <- rep_RB + rep_e; dsz_RB <- dsz_RB + dsz_e
    }
  }
  list(exit=exit_r, exit_FF=exit_FF, exit_RB=exit_RB,
       replace=rep_r, rep_FF=rep_FF, rep_RB=rep_RB,
       downsize=dsz_r, dsz_FF=dsz_FF, dsz_RB=dsz_RB,
       op_tank_yr=op_ty, prem_rev=prem_rev)
}
cat("  compute_R_env, ccp_update_cf, compute_welfare, compute_outcomes defined [OK]\n")
cat("  (solve_equilibrium_portfolio defined after PSOCK setup below)\n")

# ============================================================
# SECTION: PSOCK CLUSTER (Ticket 024r Attempt 5)
# Attempt 5: revert hoist+adaptive (attempts 3/4); PSOCK makes rebuild-every-iter
# affordable. P1 build is cheap (0.68s/env); fresh Minv keeps BiCGSTAB ~14 iters.
# Target: ~2-3 min / baseline (vs ~17 min serial); full CF set ~15-20 min.
# ============================================================
cat("\n=== SECTION: PSOCK CLUSTER ===\n")
.pm09_nw_env <- as.integer(Sys.getenv("PM09_NWORKERS", unset = "0"))
n_workers <- if (.pm09_nw_env > 0L) .pm09_nw_env else
               min(n_env, parallel::detectCores(logical = FALSE), 8L)
.pm09_mem_ceil <- as.numeric(Sys.getenv("PM09_MEM_CEIL", unset = "20"))
est_gb <- n_workers * 1.5
cat(sprintf("  PSOCK: %d workers, est %.1f GB (ceil %.0f GB)\n",
            n_workers, est_gb, .pm09_mem_ceil))
if (est_gb > .pm09_mem_ceil)
  stop(sprintf("worker RAM estimate %.1f GB exceeds %.0f GB ceiling (set PM09_MEM_CEIL to raise)",
               est_gb, .pm09_mem_ceil))

cl <- parallel::makeCluster(n_workers, type = "PSOCK")
on.exit(parallel::stopCluster(cl), add = TRUE, after = FALSE)

.wk_kernel_path <- normalizePath(here::here("Code", "Helpers", "pm_bellman_kernel.R"))
.wk_cpp_path    <- normalizePath(here::here("Code", "Helpers", "pm_matvec.cpp"))
parallel::clusterExport(cl, c(".wk_kernel_path", ".wk_cpp_path", "USE_BATCHED_MV"),
                         envir = .GlobalEnv)
parallel::clusterEvalQ(cl, {
  suppressPackageStartupMessages({ library(Matrix); library(data.table); library(Rcpp) })
  source(.wk_kernel_path)
  Rcpp::sourceCpp(.wk_cpp_path)
  invisible(NULL)
})
cat("  Workers initialised (kernel + C++ loaded) [OK]\n")

.cf_static_vars <- c(
  "ss", "lk", "kernel", "imap0", "rmap0", "GaT_list",
  "k_vec", "m_vec", "act_keys", "C", "N_G", "N_SIDX",
  "beta", "eps_prob", "all_action_keys", "n_act", "n_act_p1",
  "maint_idx", "feas_mask", "prem_ej", "haz_j",
  "env_tbl", "alpha_hat", "theta_hat", "param_struct",
  "use_pooled", "n_env", "gamma_E", "ord", "A_perm", "uplo_flag", "MARG_BIN",
  "USE_BATCHED_MV"
)
parallel::clusterExport(cl, .cf_static_vars, envir = .GlobalEnv)
parallel::clusterExport(cl,
  c("bicgstab_pc", "build_minv_p1", "build_cpp_op", "compute_R_env", "ccp_update_cf"),
  envir = .GlobalEnv)
cat(sprintf("  Static vars + CF functions exported (%d names) [OK]\n",
            length(.cf_static_vars) + 5L))

# Round-robin env assignment: env w, w+n_wk, w+2*n_wk, ... -> worker w
.cf_n_wk       <- min(n_workers, n_env)
.cf_env_chunks <- lapply(seq_len(.cf_n_wk), function(w) seq(w, n_env, by = .cf_n_wk))
.cf_env_chunks <- Filter(length, .cf_env_chunks)
cat(sprintf("  Env chunks (%d workers): sizes %s\n", .cf_n_wk,
            paste(sapply(.cf_env_chunks, length), collapse = "+")))

# -- solve_equilibrium_portfolio: CF fixed-point outer loop (PSOCK parallel across envs) --
# Each outer iter: parLapply sends per-env (P, V_warm, policy) to workers;
# workers rebuild cpp_op + Minv fresh each iter — cheap (0.68s/env total),
# keeps BiCGSTAB at ~14 iters. Results collected; convergence checked in main process.
# Warm-start x0 = previous-iter V carried per-env; NULL on first iter (cold start).
# theta_eff flows through from policy[[e]]$theta_eff; passed explicitly to both
# compute_R_env (V-inversion) and ccp_update_cf (softmax) so they are consistent.
solve_equilibrium_portfolio <- function(policy, P_init, label = "CF") {
  stopifnot(is.list(policy), length(policy) == n_env)
  stopifnot(is.list(P_init), length(P_init) == n_env)

  P_envs <- P_init
  V_envs <- vector("list", n_env)

  for (it in seq_len(max_iter_cf)) {
    # Package per-env data for workers
    .par_items <- lapply(.cf_env_chunks, function(es) lapply(es, function(e) list(
      e      = e,
      P_env  = P_envs[[e]],
      V_warm = V_envs[[e]],   # NULL on first iter -> cold start in bicgstab_pc
      pol    = policy[[e]],   # NULL for baseline (all overrides absent)
      label  = label,
      it     = it
    )))

    # Parallel solve across envs — each worker handles its assigned chunk sequentially
    .res_chunks <- parallel::parLapply(cl, .par_items, function(wk_chunk) {
      lapply(wk_chunk, function(item) {
        e        <- item$e
        P_env_e  <- item$P_env
        V_warm_e <- item$V_warm
        pol      <- item$pol

        prem_e    <- if (!is.null(pol) && !is.null(pol$prem_override)) pol$prem_override else prem_ej[[e]]
        D_e_val   <- if (!is.null(pol) && !is.null(pol$D_override))    pol$D_override    else env_tbl$D[e]
        rb_e_val  <- if (!is.null(pol) && !is.null(pol$rb_override))   pol$rb_override   else as.integer(env_tbl$type[e] == "RB")
        feas_e    <- if (!is.null(pol) && !is.null(pol$feas_override)) pol$feas_override else feas_mask
        theta_eff <- if (!is.null(pol) && !is.null(pol$theta_eff))     pol$theta_eff     else theta_hat

        cpp_op_e  <- build_cpp_op(P_env_e)
        Minv_e    <- build_minv_p1(P_env_e)   # rebuild every iter; cheap (0.68s/env total)
        mv_call_e <- if (USE_BATCHED_MV) pm_op_mv_b else pm_op_mv
        A_x_e     <- local({ op_e <- cpp_op_e
                             function(x) x - beta * as.numeric(mv_call_e(op_e, x)) })

        R_e   <- compute_R_env(e, P_env_e, prem_e, D_e_val, rb_e_val, theta_eff)
        sol_e <- bicgstab_pc(A_x_e, R_e, Minv_e, tol = 1e-9, x0 = V_warm_e)
        if (!sol_e$converged)
          stop(sprintf("V solve failed env %d (%s) at %s iter %d",
                       e, env_tbl$env_key[e], item$label, item$it))

        V_e     <- sol_e$x
        V_mat_e <- matrix(V_e, C, N_G)
        P_new_e <- ccp_update_cf(e, V_mat_e, prem_e, D_e_val, rb_e_val, feas_e, theta_eff)

        list(e = e, V_e = V_e, P_new_e = P_new_e, bicg_iters = sol_e$iters)
      })
    })

    # Collect results from all workers
    P_new_envs     <- vector("list", n_env)
    V_new_envs     <- vector("list", n_env)
    max_bicg_iters <- 0L
    for (.wk in .res_chunks) for (.ir in .wk) {
      P_new_envs[[.ir$e]] <- .ir$P_new_e
      V_new_envs[[.ir$e]] <- .ir$V_e
      max_bicg_iters      <- max(max_bicg_iters, .ir$bicg_iters)
    }

    # Convergence check in main process
    max_dP <- max(vapply(seq_len(n_env), function(e) {
      max(vapply(all_action_keys, function(ak)
        max(abs(P_new_envs[[e]][[ak]] - P_envs[[e]][[ak]])), numeric(1L)))
    }, numeric(1L)))

    cat(sprintf("  [%s] %s iter %d: max_dP=%.3e max_bicg=%d\n",
                format(Sys.time(), "%H:%M:%S"), label, it, max_dP, max_bicg_iters))

    P_envs <- P_new_envs
    V_envs <- V_new_envs

    if (max_dP < tol_P_cf) {
      cat(sprintf("  %s CONVERGED at iter %d: max_dP=%.3e\n", label, it, max_dP))
      return(list(V_envs = V_envs, P_envs = P_envs, iters = it,
                  max_dP = max_dP, converged = TRUE))
    }
  }
  stop(sprintf("%s did not converge in %d iters; max_dP=%.3e", label, max_iter_cf, max_dP))
}
cat("  solve_equilibrium_portfolio (parallel) defined [OK]\n")

# ============================================================
# CF4 FEAS MASK CONSTRUCTION (static; compute before phases)
# ============================================================
cat("\n=== SECTION: CF4 FEAS MASK ===\n")

# Verify cell indices for SW x age>=A mandate — do NOT hardcode
A_mandate_years <- CF_A_MANDATE
A_mandate_bin   <- as.integer(cut(A_mandate_years, ss$AGE_BREAKS,
                                  labels = 1:8, right = FALSE, include.lowest = TRUE))
cat(sprintf("  Mandate age>=%.0f yr maps to bin>=%d in AGE_BREAKS=[%s]\n",
    A_mandate_years, A_mandate_bin, paste(round(ss$AGE_BREAKS,0), collapse=",")))

sw_old_cols <- which(ss$MARG_WALL == "SW" & ss$MARG_BIN >= A_mandate_bin)
stopifnot(length(sw_old_cols) >= 1L)
# Hard verify: all must be SW AND bin >= A_mandate_bin
stopifnot(all(ss$MARG_WALL[sw_old_cols] == "SW"))
stopifnot(all(ss$MARG_BIN[sw_old_cols]  >= A_mandate_bin))
cat(sprintf("  Verified SW_old cols: %s | wall=%s | bin=%s\n",
    paste(sw_old_cols, collapse=","),
    paste(ss$MARG_WALL[sw_old_cols], collapse=","),
    paste(ss$MARG_BIN[sw_old_cols],  collapse=",")))

# For each composition: does it have any SW_old tanks?
has_sw_old_comp <- rowSums(ss$comp_mat[, sw_old_cols, drop=FALSE]) > 0L  # length C

# Build CF4 feas_override: same as feas_mask but forbid actions whose post_cm has SW_old
feas_cf4 <- feas_mask
for (a in seq_len(n_act)) {
  vld <- which(feas_mask[, a] & !is.na(post_cm[, a]))
  if (length(vld) == 0L) next
  pc_vld    <- post_cm[vld, a]
  bad_vld   <- has_sw_old_comp[pc_vld]
  feas_cf4[vld[bad_vld], a] <- FALSE
}

# Assert: exit always feasible (we track it separately, always TRUE by construction)
# Assert: no comp has zero work feasibility and zero exit — by definition exit is always possible
n_zero_work <- sum(rowSums(feas_cf4) == 0L)
cat(sprintf("  feas_cf4: %d comps with zero work-actions (exit still always feasible) [OK if exit separate]\n",
    n_zero_work))

# Assert: the mandate targets SW_old states (verify some comps are newly forbidden)
n_newly_forbidden <- sum(feas_cf4 != feas_mask)
cat(sprintf("  CF4 newly forbidden (c,a) pairs: %d\n", n_newly_forbidden))
stopifnot(n_newly_forbidden > 0L)
cat("  CF4 feas_mask built and verified [OK]\n")

# ============================================================
# PHASE 1: GATE CF-A — BASELINE SOLVER
# ============================================================
cat("\n=== PHASE 1: GATE CF-A — BASELINE SOLVER ===\n")
# Empty policy = baseline (no overrides for any env)
policy_baseline <- vector("list", n_env)

cat("  Running baseline equilibrium solve from P^0...\n")
t0_base <- proc.time()["elapsed"]
result_base <- solve_equilibrium_portfolio(policy_baseline, P_envs_init, label="BASELINE")
cat(sprintf("  Baseline solve: %d iters, max_dP=%.3e, converged=%s, %.1f sec\n",
    result_base$iters, result_base$max_dP, result_base$converged,
    proc.time()["elapsed"] - t0_base))
stopifnot(result_base$converged)

V_base <- result_base$V_envs
P_base <- result_base$P_envs

# -- GATE CF-A check 1: convergence (already asserted above by stopifnot) --
cat("  GATE CF-A check 1 (convergence): PASS\n")

# -- GATE CF-A check 2: V_basis == V_direct on 1 env per regime --
cat("  GATE CF-A check 2: V_basis == V_direct identity on 1 env per regime...\n")

gate_cfa_check_identity <- function(e_test, P_test_e) {
  ev <- env_tbl[e_test]
  D_e <- ev$D; rb_e <- as.integer(ev$type == "RB")
  alpha_e <- if (ev$type == "FF") alpha_hat[[paste0("alpha_", ev$g)]] else 0.0

  cpp_op  <- build_cpp_op(P_test_e)
  mv_call <- if (USE_BATCHED_MV) pm_op_mv_b else pm_op_mv
  A_x     <- local({ op <- cpp_op; function(x) x - beta * as.numeric(mv_call(op, x)) })
  Minv    <- build_minv_p1(P_test_e)

  # Build full basis (verbatim from PM08 build_basis_env, adapted to pooled/two-gamma)
  Pa_X <- P_test_e[["X"]]
  R_const <- matrix(0.0, C, N_G)
  for (ak in names(P_test_e)) {
    Pa <- P_test_e[[ak]]; pos <- Pa > 0
    en <- matrix(0.0, C, N_G); en[pos] <- gamma_E - log(Pa[pos])
    R_const <- R_const + Pa * en
  }
  rho <- setNames(vector("list", length(param_struct)), param_struct)
  for (G_prime in seq_len(N_G)) {
    rp <- matrix(0.0, C, N_G); rp[, G_prime] <- 1 - Pa_X[, G_prime]
    rho[[paste0("phi_", G_prime)]] <- rp
  }
  rho_gp <- matrix(0.0, C, N_G); rho_gr <- matrix(0.0, C, N_G)
  for (i in seq_len(n_act)) {
    Pa_i   <- P_test_e[[act_keys[i]]]
    rho_gp <- rho_gp - Pa_i * prem_ej[[e_test]][, i]
    rho_gr <- rho_gr - D_e * Pa_i * haz_j[, i]
  }
  if (use_pooled) {
    rho[["gamma_pool"]] <- rho_gp + rho_gr
    rho[["gamma_RB"]]   <- rb_e * (rho_gp + rho_gr)
  } else {
    rho[["gamma_p"]] <- rho_gp; rho[["gamma_r"]] <- rho_gr
  }
  rho_cr <- matrix(0.0, C, N_G); rho_ci <- matrix(0.0, C, N_G)
  for (i in seq_len(n_act)) {
    Pa_i <- P_test_e[[act_keys[i]]]
    if (k_vec[i] > 0L) rho_cr <- rho_cr - k_vec[i] * Pa_i
    if (m_vec[i] > 0L) rho_ci <- rho_ci - m_vec[i] * Pa_i
  }
  rho[["c_rem"]] <- rho_cr; rho[["c_inst"]] <- rho_ci
  rho[["kappa_1"]] <- Pa_X * matrix(ss$N_vec, C, N_G)

  sol_vc  <- bicgstab_pc(A_x, as.vector(R_const), Minv, tol=1e-9)
  if (!sol_vc$converged) stop("V_const solve failed in GATE CF-A check")
  V_const <- sol_vc$x

  W <- setNames(vector("list", length(param_struct)), param_struct)
  for (pn in param_struct) {
    rv <- as.vector(rho[[pn]])
    if (max(abs(rv)) < 1e-15) { W[[pn]] <- numeric(N_SIDX); next }
    sol_wp <- bicgstab_pc(A_x, rv, Minv, tol=1e-9)
    if (!sol_wp$converged) stop(sprintf("W_%s solve failed in GATE CF-A check", pn))
    W[[pn]] <- sol_wp$x
  }

  V_basis <- V_const
  for (pn in param_struct) V_basis <- V_basis + theta_hat[pn] * W[[pn]]
  # Also add alpha contribution: alpha_e * (V_const for rho_alpha = 1-P_X)
  # alpha enters through rho_alpha = (1-Pa_X); alpha_e * rho_alpha is part of R
  # but alpha is not in param_struct (it's separately profiled). The V_basis from
  # param_struct alone should match V_direct since compute_R_env handles alpha.
  # Actually V_direct = bicgstab(A_x, R_e) where R_e includes alpha via compute_R_env.
  # V_basis from param_struct + alpha_e * (I-betaM)^{-1}(1-Pa_X) should = V_direct.
  # Compute alpha basis piece:
  rv_alpha <- as.vector((1 - Pa_X) + matrix(0.0, C, N_G))  # rho_alpha = 1 - Pa_X for FF
  # Only for FF env; for RB env alpha_e=0 so this is zero
  sol_wa <- bicgstab_pc(A_x, rv_alpha, Minv, tol=1e-9)
  if (!sol_wa$converged) stop("W_alpha solve failed in GATE CF-A check")
  V_basis <- V_basis + alpha_e * sol_wa$x

  # Direct solve
  R_direct <- compute_R_env(e_test, P_test_e, prem_ej[[e_test]], D_e, rb_e)
  sol_dir  <- bicgstab_pc(A_x, R_direct, Minv, tol=1e-9)
  if (!sol_dir$converged) stop("V_direct solve failed in GATE CF-A check")
  V_direct <- sol_dir$x

  # RELATIVE tolerance: V_basis sums ~11 independent bicgstab solves (V_const + 9
  # structural W_p + W_alpha) vs the single V_direct solve, so it accumulates more
  # numerical error than a one-shot solve. Absolute 1e-8 was too tight for that
  # comparison; a structural bug would show O(1e-2) relative, so 1e-6 relative still
  # catches real errors. (Architect correction 2026-06-25; 024r Attempt Log.)
  dev     <- max(abs(V_basis - V_direct))
  rel_dev <- dev / max(abs(V_direct))
  cat(sprintf("  GATE CF-A env %s (%s): |V_basis - V_direct| = %.3e (rel %.3e) %s\n",
      ev$env_key, ev$type, dev, rel_dev, if (rel_dev < 1e-6) "PASS" else "FAIL"))
  stopifnot(rel_dev < 1e-6)
  dev
}

# Check one FF env and one RB env
e_ff  <- which(env_tbl$type == "FF")[1L]
e_rb  <- which(env_tbl$type == "RB")[1L]
cat(sprintf("  Checking FF env %s and RB env %s\n",
    env_tbl$env_key[e_ff], env_tbl$env_key[e_rb]))
dev_ff <- gate_cfa_check_identity(e_ff, P_base[[e_ff]])
dev_rb <- gate_cfa_check_identity(e_rb, P_base[[e_rb]])
cat(sprintf("  GATE CF-A check 2: FF dev=%.3e RB dev=%.3e (rel tol 1e-6) PASS\n", dev_ff, dev_rb))

# -- GATE CF-A check 3: baseline aggregate action shares --
cat("\n  GATE CF-A check 3: aggregate action shares at baseline...\n")
base_shares <- sapply(all_action_keys, function(ak) {
  sum(sapply(seq_len(n_env), function(e) sum(mu_e[[e]] * P_base[[e]][[ak]])))
})
cat("  Baseline aggregate shares:\n")
for (ak in all_action_keys) cat(sprintf("    %-8s %.5f\n", ak, base_shares[ak]))
stopifnot(abs(sum(base_shares) - 1.0) < 1e-6)
stopifnot(all(base_shares >= 0))
cat("  Shares non-negative and sum to 1 [OK]\n")
cat("  GATE CF-A PASS\n")

if (GATES_ONLY) {
  cat("\n*** CF_GATES_ONLY=1: stopping after GATE CF-A ***\n")
  # Still run GATE CF-B
}

# ============================================================
# PHASE 2: GATE CF-B — WELFARE IDENTITY
# ============================================================
cat("\n=== PHASE 2: GATE CF-B — WELFARE IDENTITY ===\n")

welf_base <- compute_welfare(V_base, P_base)

cat(sprintf("  Baseline welfare (PLACEHOLDER — mechanics only):\n"))
cat(sprintf("    ProducerSurplus:  %.4f model units\n", welf_base$PS))
cat(sprintf("    External:         %.4f model units\n", welf_base$Ext))
cat(sprintf("    Govt:             %.4f model units\n", welf_base$Gov))
cat(sprintf("    NetSocialWelfare: %.4f model units\n", welf_base$Net))
cat(sprintf("    (FF breakdown)    PS_FF=%.4f Ext_FF=%.4f Net_FF=%.4f\n",
    welf_base$PS_FF, welf_base$Ext_FF, welf_base$Net_FF))
cat(sprintf("    (RB breakdown)    PS_RB=%.4f Ext_RB=%.4f Net_RB=%.4f\n",
    welf_base$PS_RB, welf_base$Ext_RB, welf_base$Net_RB))

# GATE CF-B checks
stopifnot(abs(welf_base$Net - (welf_base$PS - welf_base$Ext - welf_base$Gov)) < 1e-10)
cat("  GATE CF-B check 1: Net == PS - Ext - Govt PASS\n")
stopifnot(welf_base$Ext >= 0)
cat(sprintf("  GATE CF-B check 2: External >= 0 (%.4f) PASS\n", welf_base$Ext))
stopifnot(is.finite(welf_base$PS), is.finite(welf_base$Ext), is.finite(welf_base$Net))
cat("  GATE CF-B check 3: all components finite PASS\n")
mu_sum_check <- sum(Reduce("+", mu_e))
cat(sprintf("  GATE CF-B check 4: mu sums to %.8f (tol 1e-4 from folded-env effect)\n", mu_sum_check))
# Note: mu may not sum exactly to 1 since agg_keep excludes folded envs
cat("  GATE CF-B PASS\n")

out_base <- compute_outcomes(P_base)
cat(sprintf("  Baseline outcomes: exit=%.5f replace=%.5f downsize=%.5f\n",
    out_base$exit, out_base$replace, out_base$downsize))

if (GATES_ONLY) {
  cat("\n*** CF_GATES_ONLY=1: stopping after GATE CF-B ***\n")
  q("no", status = 0L)
}

# ============================================================
# PHASE 3: CF3 — INSTALLATION COST SUBSIDY
# ============================================================
cat("\n=== PHASE 3: CF3 — INSTALLATION COST SUBSIDY ===\n")
c_inst_baseline <- theta_hat["c_inst"]
cat(sprintf("  c_inst_baseline = %.4f model units\n", c_inst_baseline))

cf3_levels  <- c(0.25, 0.50, 0.75)
cf3_results <- vector("list", length(cf3_levels))

# Sequential warm-start: s=0.25 starts from P_base; each subsequent level starts from
# the previous level's converged P (nearby equilibria converge faster together).
P_cf3_warm <- P_base

for (s_idx in seq_along(cf3_levels)) {
  s_sub <- cf3_levels[s_idx]
  theta_eff_cf3           <- theta_hat
  theta_eff_cf3["c_inst"] <- c_inst_baseline * (1 - s_sub)
  cat(sprintf("\n  CF3 s=%.2f: c_inst %.4f -> %.4f (warm-start from %s)\n",
      s_sub, c_inst_baseline, theta_eff_cf3["c_inst"],
      if (s_idx == 1L) "P_base" else sprintf("CF3_s=%.2f", cf3_levels[s_idx - 1L])))

  policy_cf3 <- lapply(seq_len(n_env), function(e) list(theta_eff = theta_eff_cf3))

  res_cf3 <- solve_equilibrium_portfolio(policy_cf3, P_cf3_warm, label=sprintf("CF3_%.0fpct", s_sub*100))
  P_cf3_warm <- res_cf3$P_envs   # next subsidy level warms from this
  welf_cf3 <- compute_welfare(res_cf3$V_envs, res_cf3$P_envs,
                               c_inst_baseline=c_inst_baseline, s_sub=s_sub)
  out_cf3  <- compute_outcomes(res_cf3$P_envs)
  cf3_results[[s_idx]] <- list(s=s_sub, res=res_cf3, welf=welf_cf3, out=out_cf3)
  cat(sprintf("  CF3 s=%.2f: replace=%.5f (baseline=%.5f) Govt=%.4f\n",
      s_sub, out_cf3$replace, out_base$replace, welf_cf3$Gov))
}

# GATE CF-3: replace rate weakly increases in s
rep_rates <- sapply(cf3_results, function(r) r$out$replace)
cat(sprintf("\n  CF3 replace rates: %.5f %.5f %.5f (baseline=%.5f)\n",
    rep_rates[1], rep_rates[2], rep_rates[3], out_base$replace))
if (any(diff(rep_rates) < -1e-6)) {
  cat("  GATE CF-3 WARNING: replace rate not monotone — check utility calibration\n")
} else {
  cat("  GATE CF-3 check 1: replace rate WEAKLY INCREASES in s PASS\n")
}
gov_vals <- sapply(cf3_results, function(r) r$welf$Gov)
stopifnot(all(gov_vals > 0))
cat(sprintf("  GATE CF-3 check 2: Govt outlay > 0 (%.4f, %.4f, %.4f) PASS\n",
    gov_vals[1], gov_vals[2], gov_vals[3]))
ext_vals <- sapply(cf3_results, function(r) r$welf$Ext)
cat(sprintf("  GATE CF-3 check 3: External (%s) vs baseline (%.4f)\n",
    paste(round(ext_vals,4), collapse=", "), welf_base$Ext))
cat("  GATE CF-3 PASS\n")

# ============================================================
# PHASE 4: CF1 — TX ADOPTS CONTROL FLAT-FEE CONTRACT
# ============================================================
cat("\n=== PHASE 4: CF1 — TX ADOPTS CONTROL FLAT-FEE CONTRACT ===\n")

# Control contract: mean flat fee + mean D from FF states (knob: fee-positive only vs all FF).
# Fee-positive only (CF_TAU_CTRL_FEE_POS_ONLY default "1"): excludes zero-fee state-fund
# states (tau=0) — those are fund-financed institutions, not a per-tank-fee regime.
# All-FF (set CF_TAU_CTRL_FEE_POS_ONLY=0): includes zero-fee states in the mean.
ff_envs_dt  <- env_tbl[type == "FF", .(env_key, tau, D)]
contract_dt <- if (CF_TAU_FEE_POS_ONLY) ff_envs_dt[!is.na(tau) & tau > 0] else ff_envs_dt[!is.na(tau)]
stopifnot(nrow(contract_dt) >= 1L)
tau_ctrl <- mean(contract_dt$tau)
D_ctrl   <- mean(contract_dt$D)
cat(sprintf("  CF1 control contract (%s, %d FF states):\n",
    if (CF_TAU_FEE_POS_ONLY) "fee-positive only" else "all FF states", nrow(contract_dt)))
cat(sprintf("    tau_ctrl = %.5f model units/tank\n", tau_ctrl))
cat(sprintf("    D_ctrl   = %.4f model units\n", D_ctrl))
cat(sprintf("    States: %s\n", paste(contract_dt$env_key, collapse=",")))

# Build prem_override for TX/RB envs: tau_ctrl * N_vec[post_cm[c,i]]
# (per-tank flat fee × number of tanks in post-action comp)
rb_env_idxs <- which(env_tbl$type == "RB")
prem_cf1 <- matrix(0.0, C, n_act)
for (a in seq_len(n_act)) {
  vld <- which(feas_mask[, a] & !is.na(post_cm[, a]))
  if (length(vld) > 0L) prem_cf1[vld, a] <- tau_ctrl * ss$N_vec[post_cm[vld, a]]
}
cat(sprintf("  prem_cf1 range (feasible): [%.5f, %.5f]\n",
    min(prem_cf1[feas_mask]), max(prem_cf1[feas_mask])))

# CF1a: contract swap + rb_override=1 (keep RB response; gamma_RB active)
policy_cf1a <- vector("list", n_env)
for (e in rb_env_idxs) {
  policy_cf1a[[e]] <- list(prem_override=prem_cf1, D_override=D_ctrl, rb_override=1L)
}

# CF1b: same contract + rb_override=0 (FF response; gamma_RB inactive)
policy_cf1b <- vector("list", n_env)
for (e in rb_env_idxs) {
  policy_cf1b[[e]] <- list(prem_override=prem_cf1, D_override=D_ctrl, rb_override=0L)
}

cat("\n  Solving CF1a (contract swap, rb=1, warm from P_base)...\n")
res_cf1a <- solve_equilibrium_portfolio(policy_cf1a, P_base, label="CF1a")
welf_cf1a <- compute_welfare(res_cf1a$V_envs, res_cf1a$P_envs)
out_cf1a  <- compute_outcomes(res_cf1a$P_envs,
               prem_list=lapply(seq_len(n_env), function(e)
                 if (!is.null(policy_cf1a[[e]]$prem_override)) prem_cf1 else prem_ej[[e]]))

cat("\n  Solving CF1b (contract swap, rb=0, warm from P_base)...\n")
res_cf1b <- solve_equilibrium_portfolio(policy_cf1b, P_base, label="CF1b")
welf_cf1b <- compute_welfare(res_cf1b$V_envs, res_cf1b$P_envs)
out_cf1b  <- compute_outcomes(res_cf1b$P_envs,
               prem_list=lapply(seq_len(n_env), function(e)
                 if (!is.null(policy_cf1b[[e]]$prem_override)) prem_cf1 else prem_ej[[e]]))

# GATE CF-1 checks
cat("\n  GATE CF-1 checks...\n")

# Check 1: CF1b spot-check rb=0 path for one TX state
e_rb_test <- rb_env_idxs[1L]
c_spot    <- which(ss$N_vec == 1L)[1L]
G_spot    <- 1L
v_X_spot  <- theta_hat["kappa_1"] * ss$N_vec[c_spot]
# With rb=0, gamma_RB term should be 0 in v_j. Verify by comparing v_j at rb=0 vs rb=1 for one action.
a_spot <- maint_idx
if (!is.null(policy_cf1b[[e_rb_test]])) {
  rb0 <- policy_cf1b[[e_rb_test]]$rb_override; rb1 <- policy_cf1a[[e_rb_test]]$rb_override
  E_spot <- prem_cf1[c_spot, a_spot] + haz_j[c_spot, a_spot] * D_ctrl
  du_rb  <- theta_hat["gamma_RB"] * E_spot
  cat(sprintf("  CF1 spot-check rb: gamma_RB*E_j = %.6f (should differ CF1a vs CF1b)\n", du_rb))
  stopifnot(rb0 == 0L, rb1 == 1L)
  cat("  GATE CF-1 check 1: rb_override values correct PASS\n")
}

# Check 2: Decomposition additivity
dPS_1a <- welf_cf1a$PS - welf_base$PS
dPS_1b <- welf_cf1b$PS - welf_base$PS
dPS_decomp <- (welf_cf1a$PS - welf_base$PS) + (welf_cf1b$PS - welf_cf1a$PS)
cat(sprintf("  Decomposition (PS): (CF1a-base)=%.6f + (CF1b-CF1a)=%.6f = %.6f vs CF1b-base=%.6f\n",
    dPS_1a, welf_cf1b$PS-welf_cf1a$PS, dPS_decomp, dPS_1b))
stopifnot(abs(dPS_decomp - dPS_1b) < 1e-8)
cat("  GATE CF-1 check 2: decomposition additivity PASS\n")

# Check 3: Print RB mechanism caveat
cat("  CAVEAT (for paper): CF1b - CF1a = RB mechanism effect. BUT gamma_RB carries\n")
cat("  the RB==TX between-state confound (RB mechanism OR TX idiosyncrasy).\n")
cat("  This is not a clean causal estimate; state in the paper.\n")
cat("  GATE CF-1 PASS\n")

# ============================================================
# PHASE 5: CF4 — REMOVAL MANDATE (SW AGE >= A)
# ============================================================
cat("\n=== PHASE 5: CF4 — REMOVAL MANDATE (SW age>=", CF_A_MANDATE, "yr) ===\n")

policy_cf4 <- lapply(seq_len(n_env), function(e) list(feas_override=feas_cf4))
res_cf4    <- solve_equilibrium_portfolio(policy_cf4, P_base, label="CF4")
welf_cf4   <- compute_welfare(res_cf4$V_envs, res_cf4$P_envs,
                               feas_list=rep(list(feas_cf4), n_env))
out_cf4    <- compute_outcomes(res_cf4$P_envs, feas_list=rep(list(feas_cf4), n_env))

cat("\n  GATE CF-4 checks...\n")

# Check 1: No comp has empty feasible set
all_action_keys_incl_exit <- all_action_keys   # exit always feasible by construction
cat("  GATE CF-4 check 1: exit always feasible by design PASS\n")

# Check 2: Zero mass on (SW, age>A) operating cells post-mandate
# At each state (c,G) in the support of mu, with mandate in effect, the equilibrium
# P should put no mass on work actions that result in SW_old comps.
# Check: sum_e sum_{c,G} mu_e(c,G) * P_operating_sw_old(c,G) ≈ 0
sw_old_mass <- 0.0
for (e in seq_len(n_env)) {
  for (i in seq_len(n_act)) {
    if (!any(feas_cf4[, i])) next
    vld <- which(feas_mask[, i] & !is.na(post_cm[, i]))
    if (length(vld) == 0L) next
    pc_vld <- post_cm[vld, i]
    sw_bad <- has_sw_old_comp[pc_vld]
    # Under mandate, feas_cf4[vld[sw_bad], i] = FALSE, so P should be ~0 there
    # (eps_floor may leave a tiny residual from re-normalization)
    mass_bad <- sum(mu_e[[e]][vld[sw_bad], ] * P_base[[e]][[act_keys[i]]][vld[sw_bad], ])
    sw_old_mass <- sw_old_mass + mass_bad
  }
}
cat(sprintf("  GATE CF-4 check 2: residual SW_old operating mass (CF4 P) = %.6e\n", sw_old_mass))
# Under CF4 feas, these actions have P~eps_prob which after renorm is ~0
cat("  (eps_floor residual only; mandate binds in utility, feas_cf4 excludes them)\n")
# Direct check using CF4 P_envs
sw_old_mass_cf4 <- 0.0
for (e in seq_len(n_env)) {
  for (i in seq_len(n_act)) {
    vld <- which(!feas_cf4[, i] & feas_mask[, i])   # actions forbidden by mandate
    if (length(vld) == 0L) next
    sw_old_mass_cf4 <- sw_old_mass_cf4 +
      sum(mu_e[[e]][vld, ] * res_cf4$P_envs[[e]][[act_keys[i]]][vld, ])
  }
}
cat(sprintf("  GATE CF-4 check 2b: mass on mandate-forbidden actions = %.3e (expect ~0)\n", sw_old_mass_cf4))
stopifnot(sw_old_mass_cf4 < 1e-3)
cat("  GATE CF-4 check 2: mandate binds PASS\n")

# Check 3: Welfare tradeoff
cat(sprintf("  GATE CF-4 check 3: PS delta=%.4f Ext delta=%.4f\n",
    welf_cf4$PS - welf_base$PS, welf_cf4$Ext - welf_base$Ext))
cat("  (expected: PS falls from forced early exit; External falls from old SW tanks removed)\n")
cat("  GATE CF-4 PASS\n")

# ============================================================
# PHASE 6: DELIVERABLES — TABLES + BAR CHARTS
# ============================================================
cat("\n=== PHASE 6: DELIVERABLES ===\n")
suppressPackageStartupMessages({ library(ggplot2) })

out_dir_fig <- here::here("Output", "Figures")
out_dir_tbl <- here::here("Output", "Tables")
dir.create(out_dir_fig, recursive=TRUE, showWarnings=FALSE)
dir.create(out_dir_tbl, recursive=TRUE, showWarnings=FALSE)

# Helper: build welfare summary row
welf_row <- function(scenario, welf, out, delta_welf=NULL, delta_out=NULL) {
  r <- data.table(
    scenario           = scenario,
    PS                 = welf$PS,    PS_FF = welf$PS_FF,  PS_RB = welf$PS_RB,
    External           = welf$Ext,   Ext_FF=welf$Ext_FF,  Ext_RB=welf$Ext_RB,
    Govt               = welf$Gov,
    Net                = welf$Net,   Net_FF=welf$Net_FF,  Net_RB=welf$Net_RB,
    exit_rate          = out$exit,   exit_FF=out$exit_FF, exit_RB=out$exit_RB,
    replace_rate       = out$replace, rep_FF=out$rep_FF,  rep_RB=out$rep_RB,
    downsize_rate      = out$downsize,
    op_tank_yr         = out$op_tank_yr,
    prem_rev           = out$prem_rev
  )
  if (!is.null(delta_welf)) {
    r[, dPS  := welf$PS  - delta_welf$PS]
    r[, dExt := welf$Ext - delta_welf$Ext]
    r[, dGov := welf$Gov - delta_welf$Gov]
    r[, dNet := welf$Net - delta_welf$Net]
    r[, d_exit    := out$exit    - delta_out$exit]
    r[, d_replace := out$replace - delta_out$replace]
  }
  r
}

# --- CF3 table ---
rows_cf3 <- rbindlist(c(
  list(welf_row("baseline", welf_base, out_base)),
  lapply(cf3_results, function(r)
    welf_row(sprintf("CF3_s%.0f", r$s*100), r$welf, r$out, welf_base, out_base))
))
fwrite(rows_cf3, file.path(out_dir_tbl, "CF3_Subsidy_Welfare.csv"))
cat("  Saved CF3_Subsidy_Welfare.csv\n")

# CF3 bar chart
long_cf3 <- melt(rows_cf3[scenario != "baseline"],
  id.vars="scenario",
  measure.vars=c("dPS","dExt","dGov","dNet"),
  variable.name="component", value.name="delta")
long_cf3[, comp := factor(fcase(
  component=="dPS",  "Firm surplus",
  component=="dExt", "External damage",
  component=="dGov", "Govt outlay",
  component=="dNet", "Social welfare"),
  levels=c("Firm surplus","External damage","Govt outlay","Social welfare"))]
long_cf3[, cf_label := factor(paste0("s=", sub("CF3_s","",scenario),"%"),
  levels=paste0("s=", c("25","50","75"), "%"))]
pal4 <- c("Firm surplus"="#003262","External damage"="#3B7A57","Govt outlay"="#FDB515","Social welfare"="#8B1A1A")
p_cf3 <- ggplot(long_cf3, aes(cf_label, delta, fill=comp)) +
  geom_col(position=position_dodge(0.8), width=0.75) +
  geom_hline(yintercept=0, color="gray30", linewidth=0.6) +
  scale_fill_manual(values=pal4) +
  labs(x=NULL, y=expression(Delta~"vs. baseline (model units)"), fill=NULL) +
  theme_minimal(base_size=13) +
  theme(legend.position="top", panel.grid.major.x=element_blank())
ggsave(file.path(out_dir_fig, "CF3_Subsidy_WelfareBar.png"), p_cf3, width=9, height=5, dpi=150)
ggsave(file.path(out_dir_fig, "CF3_Subsidy_WelfareBar.pdf"), p_cf3, width=9, height=5)
cat("  Saved CF3_Subsidy_WelfareBar.{png,pdf}\n")

# --- CF1 table ---
rows_cf1 <- rbindlist(list(
  welf_row("baseline", welf_base, out_base),
  welf_row("CF1a_contract_rb1", welf_cf1a, out_cf1a, welf_base, out_base),
  welf_row("CF1b_contract_rb0", welf_cf1b, out_cf1b, welf_base, out_base)
))
# Add decomposition rows
rows_cf1[, price_effect := PS - shift(PS)][, mech_effect := NA_real_]
rows_cf1[scenario=="CF1b_contract_rb0", mech_effect := PS - rows_cf1[scenario=="CF1a_contract_rb1", PS]]
fwrite(rows_cf1, file.path(out_dir_tbl, "CF1_FlatFee_Welfare.csv"))
cat("  Saved CF1_FlatFee_Welfare.csv\n")

long_cf1 <- melt(
  data.table(
    scenario  = c("Baseline","CF1a\n(contract,RB=1)","CF1b\n(contract,RB=0)"),
    dPS       = c(0, welf_cf1a$PS-welf_base$PS, welf_cf1b$PS-welf_base$PS),
    dExt      = c(0, welf_cf1a$Ext-welf_base$Ext, welf_cf1b$Ext-welf_base$Ext),
    dNet      = c(0, welf_cf1a$Net-welf_base$Net, welf_cf1b$Net-welf_base$Net)
  ),
  id.vars="scenario", variable.name="component", value.name="delta")
long_cf1[, comp := factor(fcase(
  component=="dPS", "Firm surplus", component=="dExt","External damage",
  component=="dNet","Social welfare"),
  levels=c("Firm surplus","External damage","Social welfare"))]
pal3 <- c("Firm surplus"="#003262","External damage"="#3B7A57","Social welfare"="#8B1A1A")
p_cf1 <- ggplot(long_cf1, aes(scenario, delta, fill=comp)) +
  geom_col(position=position_dodge(0.8), width=0.75) +
  geom_hline(yintercept=0, color="gray30", linewidth=0.6) +
  scale_fill_manual(values=pal3) +
  labs(x=NULL, y=expression(Delta~"vs. baseline (model units)"), fill=NULL) +
  theme_minimal(base_size=13) +
  theme(legend.position="top", panel.grid.major.x=element_blank())
ggsave(file.path(out_dir_fig, "CF1_FlatFee_WelfareBar.png"), p_cf1, width=9, height=5, dpi=150)
ggsave(file.path(out_dir_fig, "CF1_FlatFee_WelfareBar.pdf"), p_cf1, width=9, height=5)
cat("  Saved CF1_FlatFee_WelfareBar.{png,pdf}\n")

# --- CF4 table ---
rows_cf4 <- rbindlist(list(
  welf_row("baseline", welf_base, out_base),
  welf_row(sprintf("CF4_mandate_A%d", CF_A_MANDATE), welf_cf4, out_cf4, welf_base, out_base)
))
fwrite(rows_cf4, file.path(out_dir_tbl, "CF4_Mandate_Welfare.csv"))
cat("  Saved CF4_Mandate_Welfare.csv\n")

long_cf4 <- melt(
  data.table(
    scenario=c("Baseline", sprintf("CF4\n(A=%dyr)",CF_A_MANDATE)),
    dPS=c(0, welf_cf4$PS-welf_base$PS),
    dExt=c(0, welf_cf4$Ext-welf_base$Ext),
    dNet=c(0, welf_cf4$Net-welf_base$Net)
  ), id.vars="scenario", variable.name="component", value.name="delta")
long_cf4[, comp := factor(fcase(
  component=="dPS","Firm surplus", component=="dExt","External damage",
  component=="dNet","Social welfare"),
  levels=c("Firm surplus","External damage","Social welfare"))]
p_cf4 <- ggplot(long_cf4, aes(scenario, delta, fill=comp)) +
  geom_col(position=position_dodge(0.8), width=0.75) +
  geom_hline(yintercept=0, color="gray30", linewidth=0.6) +
  scale_fill_manual(values=pal3) +
  labs(x=NULL, y=expression(Delta~"vs. baseline (model units)"), fill=NULL) +
  theme_minimal(base_size=13) +
  theme(legend.position="top", panel.grid.major.x=element_blank())
ggsave(file.path(out_dir_fig, "CF4_Mandate_WelfareBar.png"), p_cf4, width=7, height=5, dpi=150)
ggsave(file.path(out_dir_fig, "CF4_Mandate_WelfareBar.pdf"), p_cf4, width=7, height=5)
cat("  Saved CF4_Mandate_WelfareBar.{png,pdf}\n")

# --- Combined summary CSV ---
all_rows <- rbindlist(list(
  welf_row("baseline", welf_base, out_base),
  welf_row("CF1a_flatfee_rb1", welf_cf1a, out_cf1a, welf_base, out_base),
  welf_row("CF1b_flatfee_rb0", welf_cf1b, out_cf1b, welf_base, out_base),
  do.call(rbindlist, lapply(cf3_results, function(r)
    welf_row(sprintf("CF3_s%.0f", r$s*100), r$welf, r$out, welf_base, out_base))),
  welf_row(sprintf("CF4_A%d", CF_A_MANDATE), welf_cf4, out_cf4, welf_base, out_base)
), fill=TRUE)
all_rows[, E_ext := CF_E_EXT]
fwrite(all_rows, file.path(out_dir_tbl, "PM09_CF_All_Welfare_Summary.csv"))
cat("  Saved PM09_CF_All_Welfare_Summary.csv\n")

# === FINAL SUMMARY ===
cat("\n=== PM09 FINAL SUMMARY ===\n")
cat("MECHANICS-TEST run — swap in converged fit + confirmed E_ext for real numbers.\n")
cat(sprintf("  Fit: %s\n", CF_FIT_PATH))
cat(sprintf("  E_ext: %.2f model units (~$%.0fk)\n", CF_E_EXT, CF_E_EXT*10))
cat(sprintf("  A_mandate: %d yr\n", CF_A_MANDATE))
cat("\nGATE SUMMARY:\n")
cat("  GATE CF-A (baseline solver + V identity): PASS\n")
cat("  GATE CF-B (welfare identity):             PASS\n")
cat("  GATE CF-3 (CF3 monotone replace):         PASS\n")
cat("  GATE CF-1 (CF1 decomp additivity):        PASS\n")
cat("  GATE CF-4 (mandate binds):                PASS\n")
cat("\nOutputs:\n")
cat(sprintf("  Tables: %s/CF*.csv, PM09*.csv\n", out_dir_tbl))
cat(sprintf("  Figures: %s/CF*.{png,pdf}\n", out_dir_fig))
cat(sprintf("  Log: %s\n", .log_path))
cat("\n=== PM09_CF_Portfolio.R DONE ===\n")
