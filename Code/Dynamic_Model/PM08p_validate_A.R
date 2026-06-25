# PM08p_validate_A.R
# Ticket 024p Lever A gate: pm_op_mv_b (batched) == pm_op_mv (original) == R Mv oracle
# to < 1e-12 on 5 random V. Also benchmarks original vs batched (expect ~3.5-4x).
# Run: C:/Program Files/R/R-4.5.2/bin/x64/Rscript.exe Code/Dynamic_Model/PM08p_validate_A.R
# Exit 0 = PASS. Hard stop (stopifnot) on any deviation >= 1e-12.

suppressPackageStartupMessages({
  library(Matrix)
  library(data.table)
  library(here)
  library(Rcpp)
})

# === LOGGING ===
.log_path <- here::here("logs", paste0("PM08p_valA_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".log"))
dir.create(dirname(.log_path), recursive = TRUE, showWarnings = FALSE)
.log <- file(.log_path, open = "wt")
sink(.log, type = "output"); sink(.log, type = "message", append = TRUE)
on.exit({ sink(type = "output"); sink(type = "message"); close(.log) }, add = TRUE)
cat(sprintf("LOG START %s\nScript: PM08p_validate_A.R\nR: %s\nWD: %s\n\n",
    .log_path, R.version.string, getwd()))

# === SECTION: SOURCE KERNEL AND C++ ===
cat("=== SECTION: SOURCE KERNEL AND C++ ===\n")
source(here::here("Code", "Helpers", "pm_bellman_kernel.R"))
cat("  pm_bellman_kernel.R sourced [OK]\n")

cpp_path <- here::here("Code", "Helpers", "pm_matvec.cpp")
if (!file.exists(cpp_path)) stop(sprintf("pm_matvec.cpp not found: %s", cpp_path))
Rcpp::sourceCpp(cpp_path)
stopifnot(exists("pm_op_build"), exists("pm_op_build_b"),
          exists("pm_op_mv"),    exists("pm_op_mv_b"))
cat("  pm_matvec.cpp sourced; all 4 exports present [OK]\n")

# === SECTION: LOAD INPUTS ===
cat("\n=== SECTION: LOAD INPUTS ===\n")
ss  <- readRDS(here::here("Output", "Estimation_Results", "PM_StateSpace.rds"))
lk  <- readRDS(here::here("Output", "Estimation_Results", "PM_Lookups.rds"))
agg <- fread(here::here("Data", "Analysis", "pm_agg_counts.csv"))

beta     <- lk$BETA
eps_prob <- 1e-10
stopifnot(beta == 0.9957)

kernel <- build_kernel(ss, lk)
C      <- kernel$C
N_G    <- kernel$N_G
N_SIDX <- ss$N_SIDX
stopifnot(N_SIDX == C * N_G)
cat(sprintf("  C=%d, N_G=%d, N_SIDX=%d, beta=%.4f\n", C, N_G, N_SIDX, beta))

# === SECTION: BUILD P_hm (pooled HM CCP, verbatim PM07) ===
cat("\n=== SECTION: BUILD P_hm ===\n")
n_act    <- nrow(kernel$work_all)
k_vec    <- as.integer(kernel$work_all$k)
m_vec    <- as.integer(kernel$work_all$m)
act_keys <- vapply(seq_len(n_act), function(i) paste0(k_vec[i], ",", m_vec[i]), character(1L))
cat(sprintf("  n_act=%d, distinct m values: %s\n",
            n_act, paste(sort(unique(m_vec)), collapse = " ")))

all_action_keys <- c(act_keys, "X")
hm_dt <- agg[, .(n_obs = sum(n_obs)), by = .(sidx, action)]
hm_dt[, G_idx := as.integer(ceiling(sidx / C))]
hm_dt[, c_idx := as.integer(sidx - (G_idx - 1L) * C)]

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

# === SECTION: BUILD C++ OPS ===
cat("\n=== SECTION: BUILD C++ OPS ===\n")
imap0 <- kernel$imap - 1L; imap0[is.na(imap0)] <- -1L; storage.mode(imap0) <- "integer"
rmap0 <- kernel$rmap - 1L; rmap0[is.na(rmap0)] <- -1L; storage.mode(rmap0) <- "integer"

GaT_list <- vector("list", n_act)
for (i in seq_len(n_act)) GaT_list[[i]] <- t(kernel$Gmat[[netbin(k_vec[i], m_vec[i])]])

P_list <- vector("list", n_act)
for (i in seq_len(n_act)) {
  ak <- act_keys[i]
  P_list[[i]] <- if (ak %in% names(P_hm)) P_hm[[ak]] else matrix(0.0, C, N_G)
}

t0 <- proc.time()["elapsed"]
cpp_op_old <- pm_op_build(kernel$A_age, imap0, rmap0, k_vec, m_vec, GaT_list, P_list, C, N_G)
cat(sprintf("  pm_op_build  (old): %.2f sec\n", proc.time()["elapsed"] - t0))

t0 <- proc.time()["elapsed"]
cpp_op_new <- pm_op_build_b(kernel$A_age, imap0, rmap0, k_vec, m_vec, GaT_list, P_list, C, N_G)
cat(sprintf("  pm_op_build_b(new): %.2f sec\n", proc.time()["elapsed"] - t0))

# R oracle: Mv returns M*v as a C x N_G matrix (no beta scaling — matches pm_op_mv convention)
mv_R_oracle <- function(v) as.vector(Mv(kernel, P_hm, matrix(v, nrow = C, ncol = N_G)))

# === SECTION: GATE A — 1e-12 correctness check ===
cat("\n=== SECTION: GATE A — 1e-12 CHECK ===\n")
cat("  Testing 5 random V: max|old-new| and max|new-R| must both be < 1e-12\n")
set.seed(42L)
max_dev_old_new <- 0.0
max_dev_new_R   <- 0.0
for (vi in seq_len(5L)) {
  v_rand  <- rnorm(N_SIDX)
  mv_old  <- as.numeric(pm_op_mv(cpp_op_old,  v_rand))
  mv_new  <- as.numeric(pm_op_mv_b(cpp_op_new, v_rand))
  mv_R    <- mv_R_oracle(v_rand)

  dev_on  <- max(abs(mv_old - mv_new))
  dev_nR  <- max(abs(mv_new - mv_R))
  max_dev_old_new <- max(max_dev_old_new, dev_on)
  max_dev_new_R   <- max(max_dev_new_R,   dev_nR)
  cat(sprintf("  v%d: |old-new|=%.3e  |new-R|=%.3e\n", vi, dev_on, dev_nR))
}
cat(sprintf("  Max |old-new| = %.3e  (tol 1e-12)\n", max_dev_old_new))
cat(sprintf("  Max |new-R|   = %.3e  (tol 1e-12)\n", max_dev_new_R))
stopifnot(max_dev_old_new < 1e-12)
stopifnot(max_dev_new_R   < 1e-12)
cat("  GATE A PASS\n")

# === SECTION: BENCHMARK ===
cat("\n=== SECTION: BENCHMARK ===\n")
set.seed(99L)
v_bench <- rnorm(N_SIDX)
n_reps  <- 10L

# Warm up (avoids cold-cache timing artifacts on first call)
invisible(pm_op_mv(cpp_op_old,  v_bench))
invisible(pm_op_mv_b(cpp_op_new, v_bench))

t0 <- proc.time()["elapsed"]
for (r in seq_len(n_reps)) pm_op_mv(cpp_op_old, v_bench)
t_old <- (proc.time()["elapsed"] - t0) / n_reps

t0 <- proc.time()["elapsed"]
for (r in seq_len(n_reps)) pm_op_mv_b(cpp_op_new, v_bench)
t_new <- (proc.time()["elapsed"] - t0) / n_reps

speedup <- t_old / t_new
cat(sprintf("  pm_op_mv  (old, %d reps): %.4f sec/call\n", n_reps, t_old))
cat(sprintf("  pm_op_mv_b(new, %d reps): %.4f sec/call\n", n_reps, t_new))
cat(sprintf("  Speedup: %.2fx  (spec target: ~3.5-4x)\n", speedup))
if (speedup < 2.0)
  warning(sprintf("Speedup %.2fx is below 2x — check that A_age nnz and C are as expected", speedup))

cat("\n=== PM08p_validate_A COMPLETE: PASS ===\n")
cat("  pm_op_mv_b matches oracle to 1e-12. PM08 may now switch to USE_BATCHED_MV=TRUE.\n")
cat(sprintf("  Log: %s\n", .log_path))
