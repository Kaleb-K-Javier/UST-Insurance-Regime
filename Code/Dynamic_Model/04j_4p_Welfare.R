# ==============================================================================
# 04j_4p_Welfare.R
# ==============================================================================
# Four counterfactuals (CF C, CF C_scan, CF P, CF M) on the 4-parameter
# replacement model fit only. See .claude/TICKETS/002_spec.md.
# ==============================================================================

.PARENT_RENV_LIB <- "C:/Users/kaleb/Documents/ust_ins_move_to_github/renv/library/windows/R-4.5/x86_64-w64-mingw32"
if (dir.exists(.PARENT_RENV_LIB) && !(.PARENT_RENV_LIB %in% .libPaths())) {
  .libPaths(c(.PARENT_RENV_LIB, .libPaths()))
}
suppressPackageStartupMessages({
  library(data.table); library(Matrix); library(here)
  library(ggplot2);    library(gridExtra); library(Rcpp)
  library(scales)
})

.SCRIPT_BASENAME <- "04j_4p_Welfare"
.log_path <- here::here(
  "logs",
  paste0(.SCRIPT_BASENAME, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".log"))
dir.create(dirname(.log_path), recursive = TRUE, showWarnings = FALSE)
.log <- file(.log_path, open = "wt")
sink(.log, type = "output"); sink(.log, type = "message", append = TRUE)
on.exit({ sink(type = "output"); sink(type = "message"); close(.log) }, add = TRUE)
cat(sprintf("LOG START %s\nScript: %s\nR: %s\nWD: %s\n\n",
    .log_path, .SCRIPT_BASENAME, R.version.string, getwd()))


# ==============================================================================
# SECTION 1: SOURCE HELPERS
# ==============================================================================
cat("=== SECTION 1: SOURCE HELPERS ===\n")
source(here::here("Code", "Helpers", "improved_estimator_OPTIMIZED.r"))
Rcpp::sourceCpp(here::here("Code", "Helpers", "cpp_engine.cpp"))


# ==============================================================================
# CONSTANTS
# ==============================================================================
A_THRESH        <- 6L
SUBSIDY_SCAN_LO <- -0.5
SUBSIDY_SCAN_HI <-  0.99
SUBSIDY_N_PTS   <- 16L
SUBSIDY_BASE    <-  0.50
H_PIGOU         <-  1.0
E_GRID_DOLLARS  <- c(HEALTH_ONLY = 17000, HEALTH_PLUS_UNMEASURED = 50000)
SCALE_FACTOR    <- 10000
E_GRID          <- E_GRID_DOLLARS / SCALE_FACTOR
EQ_TOL          <- 1e-8
EQ_MAX_ITER     <- 1000L


# ==============================================================================
# SECTION 2: LOAD DATA
# ==============================================================================
cat("=== SECTION 2: LOAD DATA ===\n")
OUT_EST <- here::here("Output", "Estimation_Results")
OUT_TAB <- here::here("Output", "Tables")
OUT_FIG <- here::here("Output", "Figures")
dir.create(OUT_TAB, recursive = TRUE, showWarnings = FALSE)
dir.create(OUT_FIG, recursive = TRUE, showWarnings = FALSE)

fit_4p    <- readRDS(file.path(OUT_EST, "Model_Replacement_Estimates_observed.rds"))
prims_obs <- readRDS(file.path(OUT_EST, "DCM_Primitives_Replacement_observed.rds"))

cat("fit_4p names:", paste(names(fit_4p), collapse = ", "), "\n")

# --- theta ---
theta_base <- fit_4p$theta_raw %||% fit_4p$theta
if (is.null(names(theta_base))) {
  names(theta_base) <- c("kappa_exit", "K_log", "gamma_price", "gamma_risk")
}
stopifnot(all(c("kappa_exit", "K_log", "gamma_price", "gamma_risk") %in% names(theta_base)))
cat("theta_base:", paste(sprintf("%s=%.4f", names(theta_base), theta_base), collapse = ", "), "\n")

# --- config, beta, sigma2 ---
config_4p <- fit_4p$config
beta      <- config_4p$beta
sigma2    <- config_4p$sigma2
cat(sprintf("beta=%.4f  sigma2=%.4f\n", beta, sigma2))

K_baseline <- exp(theta_base[["K_log"]])
cat(sprintf("K_baseline=%.4f\n", K_baseline))

# --- cache (load or rebuild) ---
if ("cache" %in% names(fit_4p)) {
  cache_base <- fit_4p$cache
  cat("Cache: loaded from rds\n")
} else {
  cat("Cache: absent from rds -- rebuilding\n")
  obs_panel  <- data.table::fread(
    here::here("Data", "Analysis", "dcm_obs_panel_observed.csv"))
  cache_base <- create_estimation_cache_replacement(
    primitives = prims_obs,
    obs_panel  = obs_panel,
    config     = config_4p)
  cat("Cache: rebuilt OK\n")
}

# Attach state_lut to cache if absent
if (is.null(cache_base$state_lut)) {
  cache_base$state_lut <- data.table::as.data.table(prims_obs$state_lut)
}

state_lut <- cache_base$state_lut

# --- transition matrices ---
FM <- cache_base$F_maintain %||% cache_base$FM
FR <- cache_base$F_replace  %||% cache_base$FR
stopifnot(!is.null(FM), !is.null(FR))

# --- population weights for QSD ---
ncell_raw <- prims_obs$ncell_vec %||% prims_obs$n_cell_vec
if (!is.null(ncell_raw)) {
  pop_weights <- ncell_raw / sum(ncell_raw)
} else {
  cat("ncell_vec absent from prims -- rebuilding from obs_panel\n")
  if (!exists("obs_panel")) {
    obs_panel <- data.table::fread(
      here::here("Data", "Analysis", "dcm_obs_panel_observed.csv"))
  }
  cn    <- obs_panel[, .(n_cell = .N), by = s_idx][order(s_idx)]
  all_s <- data.table(s_idx = 1:32)
  cn    <- merge(all_s, cn, by = "s_idx", all.x = TRUE)
  cn[is.na(n_cell), n_cell := 0L]
  pop_weights <- cn$n_cell / sum(cn$n_cell)
}

chain_id     <- paste0("w", state_lut$w_state, "rho", state_lut$rho_state)
chain_labels <- unique(chain_id)


# ==============================================================================
# SECTION 3: PRE-FLIGHT CHECK -- A_BIN MAPPING
# ==============================================================================
cat("=== SECTION 3: PRE-FLIGHT CHECK ===\n")
cat("A_bin distribution (number of states per bin):\n")
print(state_lut[, .N, by = A_bin][order(A_bin)])
cat("\nA_bin range:", range(state_lut$A_bin), "\n")
cat(sprintf("Mandate (A_bin >= %d): %d of %d states\n",
    A_THRESH, sum(state_lut$A_bin >= A_THRESH), nrow(state_lut)))

if (!(min(state_lut$A_bin) == 1L && max(state_lut$A_bin) == 8L)) {
  stop("PRE-FLIGHT FAIL: A_bin range is not 1..8. Inspect state_lut before proceeding.")
}
if (nrow(state_lut) != 32L) {
  stop("PRE-FLIGHT FAIL: state_lut has unexpected row count (expected 32).")
}
if (sum(state_lut$A_bin >= A_THRESH) != 12L) {
  stop(sprintf(
    "PRE-FLIGHT FAIL: mandate covers %d states, expected 12 (A_bin in {6,7,8}).",
    sum(state_lut$A_bin >= A_THRESH)))
}
cat("PRE-FLIGHT: PASS\n")


# ==============================================================================
# SECTION 4: solve_equilibrium_policy_replacement_CFM
# ==============================================================================
cat("=== SECTION 4: DEFINE CFM SOLVER ===\n")

solve_equilibrium_policy_replacement_CFM <- function(theta, cache, config,
                                                      A_thresh = 6L,
                                                      max_iter = 1000L,
                                                      tol = 1e-8) {

  state_lut   <- cache$state_lut
  is_mandated <- state_lut$A_bin >= A_thresh

  P <- matrix(1/3, cache$n_states, 3,
              dimnames = list(NULL, c("maintain", "exit", "replace")))
  P[is_mandated, "maintain"] <- 0
  P[is_mandated, "exit"]     <- 0.5
  P[is_mandated, "replace"]  <- 0.5

  sigma     <- config$sigma2
  beta      <- cache$beta
  converged <- FALSE

  for (i in seq_len(max_iter)) {
    U <- flow_utilities_replacement(theta, cache)
    V <- invert_value_function_replacement(P, U, cache, config)

    v_M <- U[, "maintain"] + beta * as.numeric(cache$F_maintain %*% V)
    v_E <- U[, "exit"]
    v_R <- U[, "replace"]  + beta * as.numeric(cache$F_replace  %*% V)

    P_new <- matrix(0, cache$n_states, 3,
                    dimnames = list(NULL, c("maintain", "exit", "replace")))

    pre_idx <- which(!is_mandated)
    if (length(pre_idx) > 0L) {
      v_mat_pre <- cbind(v_M[pre_idx], v_E[pre_idx], v_R[pre_idx])
      v_max_pre <- pmax(v_M[pre_idx], v_E[pre_idx], v_R[pre_idx])
      z_pre     <- exp((v_mat_pre - v_max_pre) / sigma)
      P_new[pre_idx, ] <- z_pre / rowSums(z_pre)
    }

    mand_idx <- which(is_mandated)
    if (length(mand_idx) > 0L) {
      v_mat_m <- cbind(v_E[mand_idx], v_R[mand_idx])
      v_max_m <- pmax(v_E[mand_idx], v_R[mand_idx])
      z_m     <- exp((v_mat_m - v_max_m) / sigma)
      P_two   <- z_m / rowSums(z_m)
      P_new[mand_idx, "maintain"] <- 0
      P_new[mand_idx, "exit"]     <- P_two[, 1L]
      P_new[mand_idx, "replace"]  <- P_two[, 2L]
    }

    P_new[P_new > 0 & P_new < config$eps_prob] <- config$eps_prob
    row_sums <- rowSums(P_new)
    P_new    <- P_new / row_sums

    if (max(abs(P_new - P)) < tol) {
      P <- P_new; converged <- TRUE; break
    }
    P <- P_new
  }

  U <- flow_utilities_replacement(theta, cache)
  V <- invert_value_function_replacement(P, U, cache, config)
  list(P = P, V = V, converged = converged, n_iter = i,
       is_mandated = is_mandated)
}


# ==============================================================================
# SECTION 5: compute_weighted_qsd_replacement  (verbatim from 04i:348-378)
# ==============================================================================
cat("=== SECTION 5: DEFINE QSD HELPER ===\n")

compute_weighted_qsd_replacement <- function(P, FM, FR, chain_id, pop_weights,
                                             maxiter = 5000, tol = 1e-12) {
  S       <- nrow(P)
  pM      <- P[, "maintain"]
  pR      <- P[, "replace"]
  mu_full <- rep(0.0, S)

  for (cl in chain_labels) {
    idx         <- which(chain_id == cl)
    if (length(idx) == 0L) next
    chain_share <- sum(pop_weights[idx])
    if (chain_share < 1e-15) next

    Tsub <- as.matrix(
      Matrix::Diagonal(length(idx), x = pM[idx]) %*% FM[idx, idx] +
      Matrix::Diagonal(length(idx), x = pR[idx]) %*% FR[idx, idx]
    )
    mu_sub <- pop_weights[idx] / sum(pop_weights[idx])

    for (it in seq_len(maxiter)) {
      mu_new <- as.numeric(mu_sub %*% Tsub)
      s_tot  <- sum(mu_new)
      if (s_tot < 1e-15) { mu_sub <- rep(1 / length(idx), length(idx)); break }
      mu_new <- mu_new / s_tot
      if (max(abs(mu_new - mu_sub)) < tol) { mu_sub <- mu_new; break }
      mu_sub <- mu_new
    }
    mu_full[idx] <- mu_sub * chain_share
  }
  mu_full / sum(mu_full)
}


# ==============================================================================
# SECTION 6: compute_welfare_for_eq
# ==============================================================================
cat("=== SECTION 6: DEFINE WELFARE HELPER ===\n")

compute_welfare_for_eq <- function(eq, mu_pop, cache, E_scalar, beta,
                                   K_baseline, s_subsidy) {
  pM <- eq$P[, "maintain"]
  pE <- eq$P[, "exit"]
  pR <- eq$P[, "replace"]

  private_loss_flow <- pM * cache$h_vec * cache$L_vec
  external_dam_flow <- pM * cache$h_vec * E_scalar
  outlay_flow       <- pR * K_baseline  * s_subsidy

  firm_surplus   <- sum(mu_pop * eq$V)
  exp_priv_loss  <- sum(mu_pop * private_loss_flow)
  exp_ext_dam    <- sum(mu_pop * external_dam_flow)
  external_pv    <- exp_ext_dam / (1 - beta)
  govt_outlay_pv <- sum(mu_pop * outlay_flow) / (1 - beta)
  social_welfare <- firm_surplus - external_pv - govt_outlay_pv

  list(
    AvgP_M             = sum(mu_pop * pM),
    AvgP_E             = sum(mu_pop * pE),
    AvgP_R             = sum(mu_pop * pR),
    ExpPrivateLossFlow = exp_priv_loss,
    ExpExternalDamFlow = exp_ext_dam,
    FirmSurplus        = firm_surplus,
    ExternalPV         = external_pv,
    GovtOutlayPV       = govt_outlay_pv,
    SocialWelfare      = social_welfare
  )
}


# ==============================================================================
# SECTION 7: SMOKE TEST
# ==============================================================================
cat("=== SECTION 7: SMOKE TEST ===\n")

# 7a: baseline equilibrium convergence
eq_smoke <- solve_equilibrium_policy_replacement(
  theta_base, cache_base, config_4p,
  max_iter = EQ_MAX_ITER, tol = EQ_TOL)
cat(sprintf("Smoke [baseline]: converged=%s at iter %d\n",
    eq_smoke$converged, eq_smoke$n_iter))
stopifnot(isTRUE(eq_smoke$converged))

# 7b: CCPs close to saved fit (if $P available)
if (!is.null(fit_4p$P)) {
  delta_ccp <- max(abs(eq_smoke$P - fit_4p$P))
  cat(sprintf("Smoke [CCP delta vs saved]: %.2e (tol 1e-3)\n", delta_ccp))
  stopifnot(delta_ccp < 1e-3)
} else {
  cat("Smoke [CCP delta]: saved $P absent -- skipped\n")
}

# 7c: CFM solver assertions
eq_smoke_M <- solve_equilibrium_policy_replacement_CFM(
  theta_base, cache_base, config_4p,
  A_thresh = A_THRESH, max_iter = EQ_MAX_ITER, tol = EQ_TOL)
cat(sprintf("Smoke [CFM]: converged=%s at iter %d\n",
    eq_smoke_M$converged, eq_smoke_M$n_iter))
stopifnot(all(eq_smoke_M$P[eq_smoke_M$is_mandated, "maintain"] == 0))
stopifnot(all(abs(rowSums(eq_smoke_M$P) - 1) < 1e-8))
stopifnot(isTRUE(eq_smoke_M$converged))
cat("SMOKE TEST: PASS\n")


# ==============================================================================
# SECTION 8: BASELINE EQUILIBRIUM + WELFARE
# ==============================================================================
cat("=== SECTION 8: BASELINE ===\n")

eq_base <- solve_equilibrium_policy_replacement(
  theta_base, cache_base, config_4p,
  max_iter = EQ_MAX_ITER, tol = EQ_TOL)
cat(sprintf("Baseline: converged=%s at iter %d\n", eq_base$converged, eq_base$n_iter))
stopifnot(isTRUE(eq_base$converged))

mu_base <- compute_weighted_qsd_replacement(eq_base$P, FM, FR, chain_id, pop_weights)
wf_base <- list()
for (e_name in names(E_GRID)) {
  E_val <- E_GRID[[e_name]]
  wf    <- compute_welfare_for_eq(eq_base, mu_base, cache_base, E_val, beta,
                                   K_baseline, s_subsidy = 0)
  wf_base[[e_name]] <- wf
  cat(sprintf("=== BASELINE -- E_label = %s ===\n", e_name))
  cat(sprintf("Converged: T at iter %d\n", eq_base$n_iter))
  cat(sprintf("AvgP_M:        %.4f\n", wf$AvgP_M))
  cat(sprintf("AvgP_E:        %.4f\n", wf$AvgP_E))
  cat(sprintf("AvgP_R:        %.4f\n", wf$AvgP_R))
  cat(sprintf("FirmSurplus:   %.4f\n", wf$FirmSurplus))
  cat(sprintf("ExternalPV:    %.4f\n", wf$ExternalPV))
  cat(sprintf("GovtOutlayPV:  %.4f\n", wf$GovtOutlayPV))
  cat(sprintf("SocialWelfare: %.4f\n", wf$SocialWelfare))
  cat(sprintf("dSW vs base:   0.0000 (this IS the base)\n"))
}


# ==============================================================================
# SECTION 9: CF C -- 50% K SUBSIDY
# ==============================================================================
cat("=== SECTION 9: CF C ===\n")

theta_C             <- theta_base
theta_C[["K_log"]] <- theta_base[["K_log"]] + log(1 - SUBSIDY_BASE)

eq_C <- solve_equilibrium_policy_replacement(
  theta_C, cache_base, config_4p,
  max_iter = EQ_MAX_ITER, tol = EQ_TOL)
cat(sprintf("CF C: converged=%s at iter %d\n", eq_C$converged, eq_C$n_iter))
stopifnot(isTRUE(eq_C$converged))

mu_C <- compute_weighted_qsd_replacement(eq_C$P, FM, FR, chain_id, pop_weights)
wf_C <- list()
for (e_name in names(E_GRID)) {
  E_val <- E_GRID[[e_name]]
  wf    <- compute_welfare_for_eq(eq_C, mu_C, cache_base, E_val, beta,
                                   K_baseline, s_subsidy = SUBSIDY_BASE)
  wf_C[[e_name]] <- wf
  cat(sprintf("=== CF C -- E_label = %s ===\n", e_name))
  cat(sprintf("Converged: T at iter %d\n", eq_C$n_iter))
  cat(sprintf("AvgP_M:        %.4f\n", wf$AvgP_M))
  cat(sprintf("AvgP_E:        %.4f\n", wf$AvgP_E))
  cat(sprintf("AvgP_R:        %.4f\n", wf$AvgP_R))
  cat(sprintf("FirmSurplus:   %.4f\n", wf$FirmSurplus))
  cat(sprintf("ExternalPV:    %.4f\n", wf$ExternalPV))
  cat(sprintf("GovtOutlayPV:  %.4f\n", wf$GovtOutlayPV))
  cat(sprintf("SocialWelfare: %.4f\n", wf$SocialWelfare))
  cat(sprintf("dSW vs base:   %.4f\n", wf$SocialWelfare - wf_base[[e_name]]$SocialWelfare))
}


# ==============================================================================
# SECTION 10: CF C_SCAN -- SUBSIDY CURVE
# ==============================================================================
cat("=== SECTION 10: CF C_SCAN ===\n")

s_grid        <- seq(SUBSIDY_SCAN_LO, SUBSIDY_SCAN_HI, length.out = SUBSIDY_N_PTS)
scan_rows     <- list()
eq_Cscan_byS  <- vector("list", length(s_grid))

for (s_i in seq_along(s_grid)) {
  s_val              <- s_grid[s_i]
  theta_s            <- theta_base
  theta_s[["K_log"]] <- theta_base[["K_log"]] + log(1 - s_val)
  K_eff_s            <- exp(theta_s[["K_log"]])

  eq_s <- solve_equilibrium_policy_replacement(
    theta_s, cache_base, config_4p,
    max_iter = EQ_MAX_ITER, tol = EQ_TOL)
  eq_Cscan_byS[[s_i]] <- eq_s

  if (!isTRUE(eq_s$converged)) {
    warning(sprintf("CF C_scan: non-convergence at s=%.4f (iter %d)", s_val, eq_s$n_iter))
  }

  mu_s <- compute_weighted_qsd_replacement(eq_s$P, FM, FR, chain_id, pop_weights)

  for (e_name in names(E_GRID)) {
    E_val <- E_GRID[[e_name]]
    wf    <- compute_welfare_for_eq(eq_s, mu_s, cache_base, E_val, beta,
                                     K_baseline, s_subsidy = s_val)
    scan_rows[[length(scan_rows) + 1L]] <- data.table(
      E_label       = e_name,
      E_dollars     = E_GRID_DOLLARS[[e_name]],
      s             = s_val,
      K_eff         = K_eff_s,
      Converged     = isTRUE(eq_s$converged),
      AvgP_M        = wf$AvgP_M,
      AvgP_E        = wf$AvgP_E,
      AvgP_R        = wf$AvgP_R,
      FirmSurplus   = wf$FirmSurplus,
      ExternalPV    = wf$ExternalPV,
      GovtOutlayPV  = wf$GovtOutlayPV,
      SocialWelfare = wf$SocialWelfare
    )
  }
  cat(sprintf("[%s] CF C_scan s=%.4f: converged=%s iter=%d\n",
      format(Sys.time(), "%H:%M:%S"), s_val, eq_s$converged, eq_s$n_iter))
}

dt_scan <- rbindlist(scan_rows)
fwrite(dt_scan, file.path(OUT_TAB, "04j_Welfare_4p_CScan.csv"))

cat(sprintf("=== CF C_scan ===\n"))
cat("s_grid:", paste(round(s_grid, 4), collapse = " "), "\n")
cat(sprintf("All converged: %s\n", all(dt_scan$Converged)))
for (e_name in names(E_GRID)) {
  sub    <- dt_scan[E_label == e_name]
  best_s <- sub$s[which.max(sub$SocialWelfare)]
  cat(sprintf("argmax_s (SW @ %s): %.4f\n", e_name, best_s))
}

# CF C_scan figure (HEALTH_ONLY only; values converted to $/facility PV)
scan_long <- melt(
  dt_scan[E_label == "HEALTH_ONLY"],
  id.vars       = c("E_label", "E_dollars", "s"),
  measure.vars  = c("FirmSurplus", "ExternalPV", "GovtOutlayPV", "SocialWelfare"),
  variable.name = "Metric", value.name = "Value_unit")
scan_long[, Value_dollars := Value_unit * SCALE_FACTOR]
scan_long[, Metric := factor(Metric,
  levels = c("FirmSurplus", "ExternalPV", "GovtOutlayPV", "SocialWelfare"),
  labels = c("Firm surplus", "External damage (PV)",
             "Govt outlay (PV)", "Social welfare"))]
argmax_dt <- dt_scan[E_label == "HEALTH_ONLY",
                     .(s_best = s[which.max(SocialWelfare)])]

metric_palette <- c(
  "Firm surplus"          = "#003262",  # Berkeley Blue
  "External damage (PV)"  = "#3B7A57",  # forest green
  "Govt outlay (PV)"      = "#FDB515",  # California Gold
  "Social welfare"        = "#8B1A1A")  # dark red

p_scan <- ggplot(scan_long, aes(x = s, y = Value_dollars, color = Metric)) +
  geom_hline(yintercept = 0, color = "grey60", linetype = "dotted",
             linewidth = 0.4) +
  geom_line(linewidth = 0.9) +
  geom_vline(data = argmax_dt, aes(xintercept = s_best),
             linetype = "dashed", color = "black", linewidth = 0.5) +
  geom_text(data = argmax_dt,
            aes(x = s_best, y = Inf,
                label = sprintf("s* = %.2f", s_best)),
            vjust = 1.5, hjust = -0.1, color = "black", size = 3.5,
            inherit.aes = FALSE) +
  scale_color_manual(values = metric_palette, name = NULL) +
  scale_y_continuous(labels = scales::dollar_format(scale = 1e-3, suffix = "k")) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(x = "Replacement cost subsidy  s",
       y = "Present value, $ per facility (E = $17k)") +
  theme_classic(base_size = 11, base_family = "Times") +
  theme(legend.position    = "bottom",
        axis.line          = element_line(colour = "black", linewidth = 0.4),
        axis.ticks         = element_line(colour = "black", linewidth = 0.3),
        axis.text          = element_text(colour = "black"),
        panel.grid.major.y = element_line(colour = "grey92", linewidth = 0.3),
        panel.grid.minor   = element_blank(),
        plot.margin        = margin(8, 12, 8, 8))

ggsave(file.path(OUT_FIG, "04j_4p_Subsidy_Curve.pdf"), p_scan,
       width = 9, height = 5.5)
ggsave(file.path(OUT_FIG, "04j_4p_Subsidy_Curve.png"), p_scan,
       width = 9, height = 5.5, dpi = 200)
cat("Figure saved: 04j_4p_Subsidy_Curve.{pdf,png}\n")


# ==============================================================================
# SECTION 11: CF P -- PIGOUVIAN PREMIUM SURCHARGE
# ==============================================================================
cat("=== SECTION 11: CF P ===\n")

eq_P_by_E      <- list()
mu_P_by_E      <- list()
wf_P           <- list()
surcharge_by_E <- list()

for (e_name in names(E_GRID)) {
  E_val         <- E_GRID[[e_name]]
  surcharge_vec <- H_PIGOU * E_val * cache_base$h_vec /
                   abs(theta_base[["gamma_price"]])
  cache_pigou          <- cache_base
  cache_pigou$P_vec    <- cache_base$P_vec + surcharge_vec
  surcharge_by_E[[e_name]] <- surcharge_vec

  eq_P <- solve_equilibrium_policy_replacement(
    theta_base, cache_pigou, config_4p,
    max_iter = EQ_MAX_ITER, tol = EQ_TOL)
  eq_P_by_E[[e_name]] <- eq_P

  mu_P <- compute_weighted_qsd_replacement(eq_P$P, FM, FR, chain_id, pop_weights)
  mu_P_by_E[[e_name]] <- mu_P

  wf <- compute_welfare_for_eq(eq_P, mu_P, cache_base, E_val, beta,
                                K_baseline, s_subsidy = 0)
  wf_P[[e_name]] <- wf

  cat(sprintf("=== CF P -- E_label = %s ===\n", e_name))
  cat(sprintf("Converged: %s at iter %d\n", eq_P$converged, eq_P$n_iter))
  cat(sprintf("AvgP_M:        %.4f\n", wf$AvgP_M))
  cat(sprintf("AvgP_E:        %.4f\n", wf$AvgP_E))
  cat(sprintf("AvgP_R:        %.4f\n", wf$AvgP_R))
  cat(sprintf("FirmSurplus:   %.4f\n", wf$FirmSurplus))
  cat(sprintf("ExternalPV:    %.4f\n", wf$ExternalPV))
  cat(sprintf("GovtOutlayPV:  %.4f\n", wf$GovtOutlayPV))
  cat(sprintf("SocialWelfare: %.4f\n", wf$SocialWelfare))
  cat(sprintf("dSW vs base:   %.4f\n", wf$SocialWelfare - wf_base[[e_name]]$SocialWelfare))
}

# Surcharge diagnostic figure
surcharge_rows <- lapply(names(E_GRID), function(e_name) {
  data.table(
    E_label   = e_name,
    E_dollars = E_GRID_DOLLARS[[e_name]],
    s_idx     = state_lut$s_idx,
    wall      = state_lut$w_state,
    regime    = state_lut$rho_state,
    surcharge = surcharge_by_E[[e_name]]
  )
})
dt_surcharge <- rbindlist(surcharge_rows)
dt_surcharge[, group := paste0("W", wall, "_R", regime)]

p_surch <- ggplot(dt_surcharge,
    aes(x = factor(s_idx), y = surcharge, fill = group)) +
  geom_col() +
  facet_wrap(~ E_label, ncol = 1, scales = "free_y") +
  labs(title = "CF P: Pigouvian Surcharge by State",
       x = "State index", y = "Surcharge ($10K/facility)", fill = "Wall x Regime") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 6))
ggsave(file.path(OUT_FIG, "04j_PremiumSurcharge_byState.png"), p_surch,
       width = 10, height = 6, dpi = 150)
cat("Figure saved: 04j_PremiumSurcharge_byState.png\n")


# ==============================================================================
# SECTION 12: CF M -- MANDATED CLOSURE AT A_BIN >= 6
# ==============================================================================
cat("=== SECTION 12: CF M ===\n")

eq_M <- solve_equilibrium_policy_replacement_CFM(
  theta_base, cache_base, config_4p,
  A_thresh = A_THRESH, max_iter = EQ_MAX_ITER, tol = EQ_TOL)
cat(sprintf("CF M: converged=%s at iter %d\n", eq_M$converged, eq_M$n_iter))

stopifnot(all(eq_M$P[eq_M$is_mandated, "maintain"] == 0))
stopifnot(all(abs(rowSums(eq_M$P) - 1) < 1e-8))
stopifnot(isTRUE(eq_M$converged))

mu_M <- compute_weighted_qsd_replacement(eq_M$P, FM, FR, chain_id, pop_weights)
wf_M <- list()
for (e_name in names(E_GRID)) {
  E_val <- E_GRID[[e_name]]
  wf    <- compute_welfare_for_eq(eq_M, mu_M, cache_base, E_val, beta,
                                   K_baseline, s_subsidy = 0)
  wf_M[[e_name]] <- wf
  cat(sprintf("=== CF M -- E_label = %s ===\n", e_name))
  cat(sprintf("Converged: T at iter %d\n", eq_M$n_iter))
  cat(sprintf("AvgP_M:        %.4f\n", wf$AvgP_M))
  cat(sprintf("AvgP_E:        %.4f\n", wf$AvgP_E))
  cat(sprintf("AvgP_R:        %.4f\n", wf$AvgP_R))
  cat(sprintf("FirmSurplus:   %.4f\n", wf$FirmSurplus))
  cat(sprintf("ExternalPV:    %.4f\n", wf$ExternalPV))
  cat(sprintf("GovtOutlayPV:  %.4f\n", wf$GovtOutlayPV))
  cat(sprintf("SocialWelfare: %.4f\n", wf$SocialWelfare))
  cat(sprintf("dSW vs base:   %.4f\n", wf$SocialWelfare - wf_base[[e_name]]$SocialWelfare))
}

# QSD by state CSV (long format)
qsd_rows <- list()
for (e_name in names(E_GRID)) {
  for (sc in c("Baseline", "CF_C", "CF_M")) {
    mu_vec <- switch(sc,
      Baseline = mu_base,
      CF_C     = mu_C,
      CF_M     = mu_M)
    qsd_rows[[length(qsd_rows) + 1L]] <- data.table(
      s_idx    = state_lut$s_idx,
      Scenario = sc,
      mu       = mu_vec
    )
  }
}
dt_qsd <- unique(rbindlist(qsd_rows))   # CF M and CF C are E-independent
fwrite(dt_qsd, file.path(OUT_TAB, "04j_CFM_QSD_mu.csv"))
cat("Saved: 04j_CFM_QSD_mu.csv\n")


# ==============================================================================
# SECTION 13: ASSEMBLE OUTPUT CSVs
# ==============================================================================
cat("=== SECTION 13: ASSEMBLE CSVs ===\n")

social_rows <- list()
decomp_rows <- list()

scenarios <- list(
  Baseline = list(wf = wf_base, conv = eq_base$converged, n_iter = eq_base$n_iter),
  CF_C     = list(wf = wf_C,    conv = eq_C$converged,    n_iter = eq_C$n_iter),
  CF_P     = list(wf = wf_P,    conv = NULL,               n_iter = NULL),
  CF_M     = list(wf = wf_M,    conv = eq_M$converged,    n_iter = eq_M$n_iter)
)

for (e_name in names(E_GRID)) {
  E_dol <- E_GRID_DOLLARS[[e_name]]

  for (sc_name in names(scenarios)) {
    sc   <- scenarios[[sc_name]]
    wf   <- sc$wf[[e_name]]
    conv <- if (sc_name == "CF_P") eq_P_by_E[[e_name]]$converged else sc$conv

    social_rows[[length(social_rows) + 1L]] <- data.table(
      E_label            = e_name,
      E_dollars          = E_dol,
      Scenario           = sc_name,
      Converged          = isTRUE(conv),
      AvgP_M             = wf$AvgP_M,
      AvgP_E             = wf$AvgP_E,
      AvgP_R             = wf$AvgP_R,
      ExpPrivateLossFlow = wf$ExpPrivateLossFlow,
      ExpExternalDamFlow = wf$ExpExternalDamFlow,
      FirmSurplus        = wf$FirmSurplus,
      SocialWelfare      = wf$SocialWelfare,
      GovtOutlayPV       = wf$GovtOutlayPV
    )

    if (sc_name != "Baseline") {
      wf_b <- wf_base[[e_name]]
      decomp_rows[[length(decomp_rows) + 1L]] <- data.table(
        E_label       = e_name,
        E_dollars     = E_dol,
        Scenario      = sc_name,
        dFirm         = wf$FirmSurplus   - wf_b$FirmSurplus,
        dExternalPV   = wf$ExternalPV    - wf_b$ExternalPV,
        dGovtPV       = wf$GovtOutlayPV  - wf_b$GovtOutlayPV,
        dSocialWelfare = wf$SocialWelfare - wf_b$SocialWelfare
      )
    }
  }
}

dt_social <- rbindlist(social_rows)
dt_decomp <- rbindlist(decomp_rows)

fwrite(dt_social, file.path(OUT_TAB, "04j_Welfare_4p_Social.csv"))
fwrite(dt_decomp, file.path(OUT_TAB, "04j_Welfare_4p_Decomp.csv"))
cat("Saved: 04j_Welfare_4p_Social.csv\n")
cat("Saved: 04j_Welfare_4p_Decomp.csv\n")

cat("\nWelfare Social Table (all scenarios):\n")
print(dt_social)


# ==============================================================================
# SECTION 14: STACKED BAR -- WELFARE DECOMPOSITION (HEALTH_ONLY)
# ==============================================================================
# Sign convention (so components SUM to dSocialWelfare):
#   +dFirm                 (firm surplus change)
#   -dExternalPV           (damage reduction => positive welfare contribution)
#   -dGovtPV               (govt outlay => welfare cost)
# Net = sum of the three components = dSocialWelfare.
# Reader-facing scenario labels drop the "(CF X)" tags. E = $17k only.
cat("=== SECTION 14: BAR CHART ===\n")

scenario_labels <- c(CF_C = "Subsidy",
                     CF_P = "Pigouvian",
                     CF_M = "Mandate")
scenario_order  <- c("Subsidy", "Pigouvian", "Mandate")

dt_decomp_ho <- dt_decomp[E_label == "HEALTH_ONLY"][
  , .(Scenario,
      `Firm surplus`        =  dFirm,
      `External damage (-)` = -dExternalPV,
      `Govt outlay (-)`     = -dGovtPV,
      Net                   =  dSocialWelfare)
][, Scenario_disp := factor(scenario_labels[Scenario], levels = scenario_order)]

decomp_stack <- melt(
  dt_decomp_ho[, .(Scenario_disp, `Firm surplus`,
                   `External damage (-)`, `Govt outlay (-)`)],
  id.vars       = "Scenario_disp",
  variable.name = "Component", value.name = "Value_unit")
decomp_stack[, Value_dollars := Value_unit * SCALE_FACTOR]

net_dt <- dt_decomp_ho[, .(Scenario_disp,
                           Value_dollars = Net * SCALE_FACTOR)]

# Stack components above (+) and below (-) zero — split positives and negatives
decomp_stack[, sign := ifelse(Value_dollars >= 0, "pos", "neg")]

stack_palette <- c(
  "Firm surplus"        = "#003262",   # Berkeley Blue
  "External damage (-)" = "#3B7A57",   # forest green
  "Govt outlay (-)"     = "#FDB515")   # California Gold

p_bar <- ggplot(decomp_stack,
                aes(x = Scenario_disp, y = Value_dollars, fill = Component)) +
  geom_hline(yintercept = 0, color = "grey50", linewidth = 0.4) +
  geom_col(position = position_stack(), width = 0.55,
           color = "white", linewidth = 0.3) +
  geom_segment(data = net_dt,
               aes(x = as.numeric(Scenario_disp) - 0.32,
                   xend = as.numeric(Scenario_disp) + 0.32,
                   y = Value_dollars, yend = Value_dollars),
               inherit.aes = FALSE,
               color = "black", linewidth = 0.9) +
  geom_point(data = net_dt,
             aes(x = Scenario_disp, y = Value_dollars),
             inherit.aes = FALSE,
             shape = 18, size = 3.2, color = "black") +
  geom_text(data = net_dt,
            aes(x = Scenario_disp, y = Value_dollars,
                label = scales::dollar(Value_dollars, accuracy = 1)),
            inherit.aes = FALSE,
            vjust = ifelse(net_dt$Value_dollars >= 0, -0.9, 1.7),
            size = 3.4, family = "Times") +
  scale_fill_manual(values = stack_palette, name = NULL) +
  scale_y_continuous(labels = scales::dollar_format(scale = 1e-3, suffix = "k")) +
  labs(x = NULL,
       y = "Change vs baseline, $ per facility (PV, E = $17k)") +
  theme_classic(base_size = 11, base_family = "Times") +
  theme(legend.position    = "bottom",
        axis.line          = element_line(colour = "black", linewidth = 0.4),
        axis.ticks.y       = element_line(colour = "black", linewidth = 0.3),
        axis.ticks.x       = element_blank(),
        axis.text          = element_text(colour = "black"),
        panel.grid.major.y = element_line(colour = "grey92", linewidth = 0.3),
        panel.grid.minor   = element_blank(),
        plot.margin        = margin(8, 12, 8, 8))

ggsave(file.path(OUT_FIG, "04j_4p_Welfare_BarChart.pdf"), p_bar,
       width = 8.5, height = 5.5)
ggsave(file.path(OUT_FIG, "04j_4p_Welfare_BarChart.png"), p_bar,
       width = 8.5, height = 5.5, dpi = 200)
cat("Figure saved: 04j_4p_Welfare_BarChart.{pdf,png}\n")


# ==============================================================================
# SECTION 14b: LATEX WELFARE SUMMARY TABLE (HEALTH_ONLY)
# ==============================================================================
# One-row-per-scenario table in $/facility PV. Columns:
#   Scenario | P_M | P_E | P_R | Firm Surplus | Social Welfare | dWelfare
cat("=== SECTION 14b: LATEX WELFARE TABLE ===\n")

dt_tab <- dt_social[E_label == "HEALTH_ONLY", .(
  Scenario,
  AvgP_M, AvgP_E, AvgP_R,
  FirmSurplus_d   = FirmSurplus   * SCALE_FACTOR,
  SocialWelfare_d = SocialWelfare * SCALE_FACTOR)]
sw_base <- dt_tab[Scenario == "Baseline", SocialWelfare_d]
dt_tab[, dWelfare := SocialWelfare_d - sw_base]
dt_tab[, Scenario_disp := c(Baseline = "Baseline",
                             scenario_labels)[Scenario]]
dt_tab <- dt_tab[match(c("Baseline", "Subsidy", "Pigouvian", "Mandate"),
                       Scenario_disp)]

.fmt_pct  <- function(x) sprintf("%.1f\\%%", 100 * x)
.fmt_doll <- function(x) sprintf("\\$%s",
                                  formatC(round(x), format = "d",
                                          big.mark = ","))
.fmt_dwf  <- function(x) {
  s   <- if (x > 0) "+" else if (x < 0) "$-$" else ""
  mag <- formatC(round(abs(x)), format = "d", big.mark = ",")
  if (x == 0) "$\\pm$\\$0" else sprintf("%s\\$%s", s, mag)
}

tex_body <- vapply(seq_len(nrow(dt_tab)), function(i) {
  r <- dt_tab[i]
  sprintf("%s & %s & %s & %s & %s & %s & %s \\\\",
          r$Scenario_disp,
          .fmt_pct(r$AvgP_M), .fmt_pct(r$AvgP_E), .fmt_pct(r$AvgP_R),
          .fmt_doll(r$FirmSurplus_d),
          .fmt_doll(r$SocialWelfare_d),
          .fmt_dwf(r$dWelfare))
}, character(1L))

tex_out <- c(
  "\\begin{tabular}{lcccccc}",
  "\\toprule",
  " & $\\bar P(M)$ & $\\bar P(E)$ & $\\bar P(R)$ & Firm Surplus & Social Welfare & $\\Delta W$ \\\\",
  "\\midrule",
  tex_body,
  "\\bottomrule",
  "\\end{tabular}")

writeLines(tex_out, file.path(OUT_TAB, "04j_Welfare_4p_Summary.tex"))
cat("Saved: 04j_Welfare_4p_Summary.tex\n")
cat("\nWelfare summary (HEALTH_ONLY, $/facility PV):\n")
print(dt_tab)


# ==============================================================================
# SECTION 15: SAVE RDS + FINAL SUMMARY
# ==============================================================================
cat("=== SECTION 15: SAVE RDS ===\n")

A_bin_distribution <- state_lut[, .N, by = A_bin][order(A_bin)]

out_rds <- list(
  eq_base       = eq_base,
  eq_C          = eq_C,
  eq_P_byE      = eq_P_by_E,
  eq_M          = eq_M,
  eq_Cscan_byS  = eq_Cscan_byS,
  mu_base       = mu_base,
  mu_C          = mu_C,
  mu_P_byE      = mu_P_by_E,
  mu_M          = mu_M,
  theta_4p      = theta_base,
  beta          = beta,
  sigma2        = sigma2,
  E_GRID        = E_GRID,
  A_THRESH      = A_THRESH,
  A_bin_distribution = A_bin_distribution
)

rds_path <- file.path(OUT_EST, "Model_Welfare_4p_observed.rds")
saveRDS(out_rds, rds_path)
cat(sprintf("Saved: %s\n", rds_path))

cat("\n=== 04j COMPLETE ===\n")
cat(sprintf("Estimator: 4p welfare CFs | Sample: observed | Solves: 21\n"))
cat(sprintf("Outputs:\n"))
cat(sprintf("  %s\n", rds_path))
cat(sprintf("  %s\n", file.path(OUT_TAB, "04j_Welfare_4p_Social.csv")))
cat(sprintf("  %s\n", file.path(OUT_TAB, "04j_Welfare_4p_Decomp.csv")))
cat(sprintf("  %s\n", file.path(OUT_TAB, "04j_Welfare_4p_CScan.csv")))
cat(sprintf("  %s\n", file.path(OUT_TAB, "04j_CFM_QSD_mu.csv")))
cat(sprintf("  %s\n", file.path(OUT_TAB, "04j_Welfare_4p_Summary.tex")))
cat(sprintf("  %s\n", file.path(OUT_FIG, "04j_4p_Welfare_BarChart.pdf")))
cat(sprintf("  %s\n", file.path(OUT_FIG, "04j_4p_Subsidy_Curve.pdf")))
cat(sprintf("  %s\n", file.path(OUT_FIG, "04j_PremiumSurcharge_byState.png")))


