# Ticket 002 -- T007: 4-Parameter Welfare (CF redesign, 4p only)

Author: Opus (architect)
Date: 2026-05-14 (revised same-day for 4p-only scope)
Coder: Sonnet via Anthropic Pro account (run with .claude\run_coder_pro_api.ps1)
Math reference: .claude/NPL_REFERENCE.md (load before reading the CF M Bellman
                pseudocode; sections 1, 4, 5 are the relevant parts)

## Deliverable summary

Build `Code/Dynamic_Model/04j_4p_Welfare.R`. The script runs four
counterfactuals (CF C, CF C_scan, CF P, CF M) on the 4-parameter fit
ONLY. The 8-parameter + FE rerun of these CFs is deferred to a later
ticket (T003: retrofit-alpha, then re-run CFs on the 8p+FE+retrofit
model). No cross-model comparison CSV is produced today.

CFs (researcher-redesigned at end of Ticket 001):
  - CF C       K subsidy 50% (kept)
  - CF C_scan  K subsidy curve, s in [-0.5, 0.99], 16 points
  - CF P       Pigouvian via ADDITIVE premium surcharge h * E / |gamma_price|
               (NOT gamma_risk scaling -- the old CF B is replaced)
  - CF M       MANDATED closure at age threshold (A_bin >= 6, ages 26+)
               Equilibrium re-solved with P_M = 0 forced at mandated states.

CFs dropped:
  - CF A (TX stays FF)   no FF/RB distinction in 4p
  - CF S (risk surcharge)  CF M is the stronger regulatory lever
  - CF D (K tax)          "RB pricing IS the tax" (researcher direction)

==============================================================================
ENVIRONMENT
==============================================================================

R executable:  C:/Program Files/R/R-4.5.2/bin/Rscript.exe
Invocation:    & "C:/Program Files/R/R-4.5.2/bin/Rscript.exe" --no-save --no-restore Code/Dynamic_Model/04j_4p_Welfare.R
Working dir:   repo root (C:/Users/kaleb/Documents/ust_ins_move_to_github)
renv lib:      renv/library/windows/R-4.5/x86_64-w64-mingw32
Packages:      data.table, Matrix, here, ggplot2, gridExtra, Rcpp (for sourceCpp)

Inputs (verified to exist at spec time):

  Output/Estimation_Results/Model_Replacement_Estimates_observed.rds
    -- 4p fit (npl_estimator_replacement return). List with at minimum:
       $theta_raw or $theta (named: kappa_exit, K_log, gamma_price, gamma_risk),
       $config, $cache (if saved; otherwise rebuild from primitives),
       $P (32x3 CCP matrix), $V (length-32 value function),
       $converged, $log_likelihood. Coder MUST readRDS, inspect names()
       at first turn, and confirm theta names match the 4p convention.
       If theta is unnamed, assign c("kappa_exit","K_log","gamma_price","gamma_risk").

  Output/Estimation_Results/DCM_Primitives_Replacement_observed.rds
    -- primitives. Contains state_lut (with A_bin column), F_maintain,
       F_replace, h_vec, L_vec, P_vec, hazard_loss, n_cell_vec, ncell_vec.

  Data/Analysis/dcm_obs_panel_observed.csv
    -- only needed if cache must be rebuilt (see Q1 below).

  Code/Helpers/improved_estimator_OPTIMIZED.r   -- source() at top of 04j.
  Code/Helpers/cpp_engine.cpp                   -- sourceCpp() before any
                                                   equilibrium solve.

Outputs (this script writes):

  Output/Estimation_Results/Model_Welfare_4p_observed.rds
    Contents: list with eq_base, eq_C, eq_P_byE, eq_M, eq_Cscan_byS,
              mu_base, mu_C, mu_P_byE, mu_M, theta_4p, beta, sigma2,
              E_GRID, A_THRESH, A_bin_distribution.

  Output/Tables/04j_Welfare_4p_Social.csv        (5 scenarios x 2 E = 10 rows)
    Scenarios enumerated: {Baseline, CF_C, CF_C_optimal, CF_P, CF_M}
      CF_C_optimal is the argmax-of-SocialWelfare row from CF C_scan,
      computed independently at each E (so its s_used differs across E).
    Columns: E_label, E_dollars, Scenario, s_used, Converged, AvgP_M,
             AvgP_E, AvgP_R, ExpPrivateLossFlow, ExpExternalDamFlow,
             FirmSurplus, SocialWelfare, GovtOutlayPV
    s_used: NA for Baseline/CF_P/CF_M; SUBSIDY_BASE (0.50) for CF_C;
            argmax s* (E-specific) for CF_C_optimal.

  Output/Tables/04j_Welfare_4p_Decomp.csv        (4 non-base x 2 E = 8 rows)
    Scenarios enumerated: {CF_C, CF_C_optimal, CF_P, CF_M}
    Columns: E_label, E_dollars, Scenario, s_used, dFirm, dExternalPV, dGovtPV, dSocialWelfare

  Output/Tables/04j_Welfare_4p_CScan.csv         (16 s-points x 2 E = 32 rows)
    Columns: E_label, E_dollars, s, K_eff, Converged, AvgP_M, AvgP_E, AvgP_R,
             FirmSurplus, ExternalPV, GovtOutlayPV, SocialWelfare

  Output/Tables/04j_CFM_QSD_mu.csv               (32 rows x scenarios)
    Long format: s_idx, Scenario, mu

  Output/Figures/04j_4p_Welfare_BarChart.png     decomposition bars by scenario,
                                                 faceted by E_label
  Output/Figures/04j_4p_Subsidy_Curve.png        CF C_scan: 4 curves (firm,
                                                 external, govt, social) vs s,
                                                 faceted by E_label, with the
                                                 argmax of SW marked
  Output/Figures/04j_PremiumSurcharge_byState.png CF P diagnostic: surcharge
                                                 vector vs state, faceted by E_label

==============================================================================
CONSTANTS
==============================================================================

A_THRESH         <- 6L           # CF M binds at A_bin >= 6 (ages 26+)
SUBSIDY_SCAN_LO  <- -0.5         # CF C_scan: lower bound on subsidy fraction
SUBSIDY_SCAN_HI  <-  0.99        # CF C_scan: upper bound (s=1 would give K_eff=0)
SUBSIDY_N_PTS    <- 16L          # CF C_scan: 16 evenly-spaced points
SUBSIDY_BASE     <-  0.50        # CF C: point estimate 50% subsidy
H_PIGOU          <-  1.0         # CF P: full Pigouvian internalization
E_GRID_DOLLARS   <- c(HEALTH_ONLY = 17000, HEALTH_PLUS_UNMEASURED = 50000)
E_GRID           <- E_GRID_DOLLARS / 10000   # model units ($10K per facility)
SCALE_FACTOR     <- 10000        # display units: $10K per facility
EQ_TOL           <- 1e-8
EQ_MAX_ITER      <- 1000L

beta and sigma2 come from $config inside the saved rds; DO NOT
hardcode them. Read them as:

  config_4p <- fit_4p$config
  beta      <- config_4p$beta
  sigma2    <- config_4p$sigma2

==============================================================================
PRE-FLIGHT CHECK: A_BIN -> AGE MAPPING
==============================================================================

The CF M mandate is defined on A_bin >= 6 (ages 26+). This assumes
A_bin = 1..8 corresponds to 5-year bins 0-5, 5-10, ..., 35+. Coder
must verify this mapping at the very first step.

After loading primitives, BEFORE any equilibrium solve, print:

  state_lut <- prims_obs$state_lut
  cat("A_bin distribution (number of states per bin):\n")
  print(state_lut[, .N, by = A_bin][order(A_bin)])
  cat("\nA_bin range:", range(state_lut$A_bin), "\n")
  cat("Mandate (A_bin >= ", A_THRESH, "): ",
      sum(state_lut$A_bin >= A_THRESH),
      " of ", nrow(state_lut), " states\n", sep = "")

Expected: A_bin takes values 1..8, with 4 states per bin
(2 wall types x 2 regimes), total 32 states. Mandate covers
A_bin in {6, 7, 8} = 12 states.

If output deviates (e.g. A_bin starts at 0, or has values outside 1..8),
STOP and report. Do not proceed.

==============================================================================
SIGN CONVENTIONS (audited 2026-05-14)
==============================================================================

These hold for the 4-parameter model. Coder: trust the existing code's signs;
do not flip anything. CFs ADD to or SCALE existing cache entries; the
flow utility function signs stay as written.

  gamma_price:  estimated negative; in code as `+ gamma_price * P_vec`
                (the negative sign lives in the parameter)
  gamma_risk:   estimated positive; in code as `- gamma_risk * hazard_loss`
                (the negative sign lives in the equation)
  kappa_exit:   u_E = kappa_exit (positive scrap value)
  K = exp(K_log):   u_R = -K (K>0 cost)

Premium and hazard enter MAINTAIN UTILITY ONLY. Replace and exit are flat.

==============================================================================
FLOW UTILITY (verbatim from improved_estimator_OPTIMIZED.r:2626)
==============================================================================

  flow_utilities_replacement <- function(theta, cache) {
    K  <- exp(theta[["K_log"]])
    u_M <- 1 + theta[["gamma_price"]] * cache$P_vec -
               theta[["gamma_risk"]]  * cache$hazard_loss
    u_E <- rep.int(theta[["kappa_exit"]], cache$n_states)
    u_R <- rep.int(-K,                    cache$n_states)
    cbind(maintain = u_M, exit = u_E, replace = u_R)
  }

Cache contents the CFs touch:
  P_vec            length-32 numeric, baseline premium per state
                   ** CF P modifies this. **
  hazard_loss      length-32 numeric, h_vec * L_vec (unmodified by any CF)
  h_vec            length-32 numeric, per-period leak hazard
                   ** CF P uses this to build the surcharge. **
  L_vec            length-32 numeric, per-leak loss (unmodified)
  n_states         32L
  state_lut        data.table with rho_state, w_state, A_bin, s_idx
  F_maintain       32x32 sparse Matrix, transition under maintain
  F_replace        32x32 sparse Matrix, transition under replace (resets to A_bin=1)
  beta             scalar discount factor

==============================================================================
COUNTERFACTUAL SPECIFICATIONS
==============================================================================

------------------------------------------------------------------------------
BASELINE
------------------------------------------------------------------------------

theta_base <- theta_4p (from saved fit)
cache_base <- cache_4p (unmodified)
eq_base    <- solve_equilibrium_policy_replacement(
                theta_base, cache_base, config_4p,
                max_iter = EQ_MAX_ITER, tol = EQ_TOL)

Welfare evaluated at both E values via mu_base + V_base.

------------------------------------------------------------------------------
CF C -- K subsidy 50% (point estimate)
------------------------------------------------------------------------------

Modify K only.

  s             <- SUBSIDY_BASE                   # 0.50
  theta_CF_C    <- theta_base
  theta_CF_C[["K_log"]] <- theta_base[["K_log"]] + log(1 - s)

Solve eq_C with theta_CF_C on cache_base. Compute welfare at both E.

Govt outlay is positive (subsidy expense). Per state:
  per-state per-period outlay = P_R[s] * K_baseline * s
  where K_baseline = exp(theta_base[["K_log"]]) (PRE-CF), NOT exp(theta_CF_C[["K_log"]]).
  GovtPV = sum_s mu_C[s] * P_R[s] * K_baseline * s / (1 - beta)

------------------------------------------------------------------------------
CF C_scan -- subsidy curve
------------------------------------------------------------------------------

Loop CF C over s_grid = seq(SUBSIDY_SCAN_LO, SUBSIDY_SCAN_HI, length.out = SUBSIDY_N_PTS).

For each s in s_grid:
  theta_s <- theta_base
  theta_s[["K_log"]] <- theta_base[["K_log"]] + log(1 - s)
  K_eff_s <- exp(theta_s[["K_log"]])                       # = K_baseline * (1 - s)
  eq_s    <- solve_equilibrium_policy_replacement(
               theta_s, cache_base, config_4p,
               max_iter = EQ_MAX_ITER, tol = EQ_TOL)
  Record converged flag; if eq_s$converged is FALSE, log a warning and
  continue (do NOT halt -- the curve plot tolerates a missing point).

For each E in E_GRID, compute:
  mu_s         = compute_weighted_qsd_replacement(eq_s$P, ...)
  FirmSurplus  = sum_s mu_s[s] * V_s[s]
  ExternalPV   = sum_s mu_s[s] * P_M_s[s] * h_vec[s] * E  / (1 - beta)
  GovtOutlayPV = sum_s mu_s[s] * P_R_s[s] * K_baseline * s_subsidy / (1 - beta)
                 # ** K_BASELINE, NOT K_eff. The outlay is the subsidy PER
                 # replacement, which equals K_baseline * s. The firm responds
                 # at K_eff for the choice; the government pays s * K_baseline. **
                 # ** For s < 0 (a tax): GovtOutlayPV is negative, representing
                 # revenue collected. Same sign convention as CF C. **
  SocialWelfare = FirmSurplus - ExternalPV - GovtOutlayPV
                 # SW = producer surplus - externality - outlay
                 # Outlay enters with a minus sign because the social planner
                 # finances it via distortionary taxation (same convention 04i uses).

Plot 04j_4p_Subsidy_Curve.png: x = s, four curves (FirmSurplus, ExternalPV,
GovtOutlayPV, SocialWelfare) faceted by E_label. Mark the argmax of
SocialWelfare with a vertical line and an annotation.

Note on solver bounds: solve_equilibrium_policy_replacement does NOT
check theta bounds (those are estimator bounds, applied inside the optim
call). The scan therefore runs unbounded across s in [-0.5, 0.99].
K_eff = K_baseline * (1 - s) stays positive across this range:
  s = -0.5  -> K_eff = 1.50 * K_baseline
  s =  0.99 -> K_eff = 0.01 * K_baseline
No division by zero, no log of zero. OK to run as written.

------------------------------------------------------------------------------
CF P -- Pigouvian via additive premium surcharge
------------------------------------------------------------------------------

Modify the cache's P_vec, not theta. The surcharge converts external damage
into premium-equivalent units before the firm sees it through gamma_price.

DERIVATION:
  Without CF: marginal flow utility of maintain w.r.t. premium = gamma_price < 0.
  Full Pigouvian internalization wants the firm to value an extra dollar of
  premium-equivalent surcharge at -h * E (the per-period external damage
  expected from continuing to maintain).
  So solve gamma_price * delta_P = -h * E for delta_P:
    delta_P = -h * E / gamma_price = h * E / |gamma_price|   (since gamma_price < 0)
  With H_PIGOU = 1 (full internalization), the per-state surcharge is:
    surcharge[s] = H_PIGOU * E * cache$h_vec[s] / abs(theta_base[["gamma_price"]])

IMPLEMENTATION (per E in E_GRID, two runs):

  cache_pigou           <- cache_base                          # shallow copy
  surcharge_vec         <- H_PIGOU * E * cache_base$h_vec /
                           abs(theta_base[["gamma_price"]])    # length-32
  cache_pigou$P_vec     <- cache_base$P_vec + surcharge_vec
  # hazard_loss UNCHANGED. CF P only loads the gamma_price channel.

  eq_P_E <- solve_equilibrium_policy_replacement(
              theta_base, cache_pigou, config_4p,
              max_iter = EQ_MAX_ITER, tol = EQ_TOL)

  Record eq_P at this E. Welfare uses E in the EXTERNAL term but NOT in
  the firm's flow utility -- the firm only sees the premium surcharge.
  GovtOutlayPV = 0 (no subsidy in CF P; the premium is paid by the firm
                    to the insurer, which in social accounting is a transfer
                    among private agents, not a govt expense).

Two CF P runs total (one per E). Save the surcharge vector for the
diagnostic figure 04j_PremiumSurcharge_byState.png (small-multiples by
wall x regime, two facets for E).

------------------------------------------------------------------------------
CF M -- mandated tank closure at age threshold
------------------------------------------------------------------------------

Threshold: A_THRESH = 6L (binds at A_bin >= 6, i.e. ages 26+). Verified
in the PRE-FLIGHT CHECK above.

The constraint forces P_M(s) = 0 for all s with state_lut$A_bin[s] >= A_THRESH.
Firm chooses exit vs replace via 2-action softmax on (v_E, v_R) only.

Equilibrium MUST be re-solved with the constraint applied at EVERY iteration,
not post-hoc on a converged 3-action policy. Math reference: NPL_REFERENCE.md
Section 4 "Constrained action sets".

Equilibrium is E-INDEPENDENT (E does not enter the flow utility under CF M;
it enters only the planner's welfare accounting). Solve once, evaluate
welfare at both E.

INLINED PSEUDOCODE (R, drop-in solver for CF M):

  solve_equilibrium_policy_replacement_CFM <- function(theta, cache, config,
                                                        A_thresh = 6L,
                                                        max_iter = 1000L,
                                                        tol = 1e-8) {

    state_lut   <- cache$state_lut
    is_mandated <- state_lut$A_bin >= A_thresh    # length-32 logical

    # Initial CCPs: pre-mandate uniform; mandated 50/50 over (E, R).
    P <- matrix(1/3, cache$n_states, 3,
                dimnames = list(NULL, c("maintain", "exit", "replace")))
    P[is_mandated, "maintain"] <- 0
    P[is_mandated, "exit"]     <- 0.5
    P[is_mandated, "replace"]  <- 0.5

    sigma <- config$sigma2
    beta  <- cache$beta
    converged <- FALSE

    for (i in seq_len(max_iter)) {
      U <- flow_utilities_replacement(theta, cache)

      # ----------------------------------------------------------------
      # V inversion: invert_value_function_replacement is reusable AS-IS.
      # MATHEMATICAL JUSTIFICATION (NPL_REFERENCE.md Sec 4): the AM linear
      # inversion (eq 2.3.25 of the chapter) is
      #   V       = (I - beta * M)^-1 * R
      #   R[s]    = sum_j P_j(s) * (u_j(s) + sigma * (gamma_E - log P_j(s)))
      #   M[s,s'] = sum_j P_j(s) * F_j[s, s']
      # A constrained action set is mathematically equivalent to summing
      # only over feasible actions, i.e. setting P_M(s) = 0 in both R[s]
      # and M[s,] at mandated s. The function applies pmax against
      # config$min_log_val (~1e-300) before log, so the maintain
      # contributions at mandated s become
      #   R_M_contribution  ~ 1e-300 * (u_M + sigma * (gamma_E + 690)) ~ 1e-298
      #   M_M_contribution  ~ 1e-300 * F_maintain[s,]                   ~ 0
      # which is machine-precision equivalent to the exact zero-constraint
      # formula. No modification to invert_value_function_replacement needed.
      # ----------------------------------------------------------------
      V <- invert_value_function_replacement(P, U, cache, config)

      # Choice-specific values:
      v_M <- U[, "maintain"] + beta * as.numeric(cache$F_maintain %*% V)
      v_E <- U[, "exit"]
      v_R <- U[, "replace"]  + beta * as.numeric(cache$F_replace  %*% V)

      P_new <- matrix(0, cache$n_states, 3,
                      dimnames = list(NULL, c("maintain", "exit", "replace")))

      # Pre-mandate states: 3-action softmax (standard).
      pre_idx <- which(!is_mandated)
      if (length(pre_idx) > 0L) {
        v_mat_pre <- cbind(v_M[pre_idx], v_E[pre_idx], v_R[pre_idx])
        v_max_pre <- pmax(v_M[pre_idx], v_E[pre_idx], v_R[pre_idx])
        z_pre     <- exp((v_mat_pre - v_max_pre) / sigma)
        P_new[pre_idx, ] <- z_pre / rowSums(z_pre)
      }

      # Mandated states: 2-action softmax on (E, R); P_M forced to 0.
      mand_idx <- which(is_mandated)
      if (length(mand_idx) > 0L) {
        v_mat_m <- cbind(v_E[mand_idx], v_R[mand_idx])    # no v_M column
        v_max_m <- pmax(v_E[mand_idx], v_R[mand_idx])
        z_m     <- exp((v_mat_m - v_max_m) / sigma)
        P_two   <- z_m / rowSums(z_m)
        P_new[mand_idx, "maintain"] <- 0
        P_new[mand_idx, "exit"]     <- P_two[, 1L]
        P_new[mand_idx, "replace"]  <- P_two[, 2L]
      }

      # Floor and renormalize ONLY non-zero entries.
      P_new[P_new > 0 & P_new < config$eps_prob] <- config$eps_prob
      row_sums <- rowSums(P_new)
      P_new <- P_new / row_sums
      # P_M[mandated] stays exactly 0 because the floor mask excludes zeros.

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

CF M post-solve assertions (REQUIRED -- include in 04j):
  stopifnot(all(eq_M$P[eq_M$is_mandated, "maintain"] == 0))
  stopifnot(all(abs(rowSums(eq_M$P) - 1) < 1e-8))
  stopifnot(isTRUE(eq_M$converged))

Note: cache$state_lut may or may not be a direct member. If not, attach
it before calling: cache$state_lut <- prims_obs$state_lut.

==============================================================================
WELFARE COMPUTATION
==============================================================================

For each CF equilibrium (P, V), with E_ext_vec = rep(E_scalar, 32):

  1. QSD (stationary distribution under the CF policy).

     04i uses compute_weighted_qsd_replacement (defined inline in 04i,
     lines 348-378). Coder should COPY this function verbatim into 04j
     OR check if it's exported from improved_estimator_OPTIMIZED.r and
     call from there. If neither, inline the helper directly into 04j.

     Required inputs: P, F_maintain, F_replace, chain_id, pop_weights.
       chain_id: paste0("w", state_lut$w_state, "rho", state_lut$rho_state)
       pop_weights: prims_obs$ncell_vec normalized, or rebuilt from
                    obs_panel$s_idx counts (mirror 04i lines 291-295).

  2. Per-state per-period quantities (model units; 1 unit = $10K):
       PrivateLossFlow[s] = P[s,"maintain"] * h_vec[s] * L_vec[s]
       ExternalDamFlow[s] = P[s,"maintain"] * h_vec[s] * E_scalar
       Outlay[s]          = P[s,"replace"]  * K_baseline * s_subsidy
                            (only nonzero for CF C and CF C_scan; 0 elsewhere)

  3. QSD-weighted and PV:
       FirmSurplus    = sum_s mu[s] * V[s]                          # PV; V already discounted
       ExternalPV     = sum_s mu[s] * ExternalDamFlow[s] / (1 - beta)
       GovtOutlayPV   = sum_s mu[s] * Outlay[s]          / (1 - beta)
       PrivateLossPV  = sum_s mu[s] * PrivateLossFlow[s] / (1 - beta) (reporting only)
       SocialWelfare  = FirmSurplus - ExternalPV - GovtOutlayPV

  4. Baseline reference: same formulas with mu_base, eq_base$P, eq_base$V.
     dSW = SW_CF - SW_base. Same for dFirm, dExternalPV, dGovtPV.

Two welfare passes per E value. CF M and CF C and CF C_scan have
E-INDEPENDENT equilibria (E doesn't enter the firm's flow utility);
CF P has E-DEPENDENT equilibria. Code accordingly:
  - eq_base, eq_C, eq_M: solve once, welfare-eval at each E
  - eq_C_scan_by_s:      solve 16 times (E-independent eq), welfare at each E
  - eq_P_by_E:           solve twice (once per E)

Total equilibrium solves: 1 (base) + 1 (C) + 16 (C_scan) + 2 (P) + 1 (M) = 21.

==============================================================================
SCRIPT STRUCTURE (recommended)
==============================================================================

  Section 0:  header, libpath, log sink (use the .SCRIPT_BASENAME pattern;
              do NOT use rstudioapi)
  Section 1:  source improved_estimator_OPTIMIZED.r; sourceCpp cpp_engine.cpp
  Section 2:  load 4p fit; load primitives; extract theta_base, config_4p, beta,
              sigma2; rebuild cache if missing (see Q1)
  Section 3:  PRE-FLIGHT CHECK -- print A_bin distribution; assert expected
              shape; STOP if unexpected
  Section 4:  define solve_equilibrium_policy_replacement_CFM (inlined Bellman)
  Section 5:  define compute_weighted_qsd_replacement (copy from 04i or
              source from helper if available)
  Section 6:  define compute_welfare_for_eq(eq, mu_pop, cache, E_scalar,
                                             beta, K_baseline, s_subsidy)
  Section 7:  SMOKE TEST (see below). STOP on failure.
  Section 8:  BASELINE eq + welfare at both E
  Section 9:  CF C (K subsidy 50%) + welfare at both E
  Section 10: CF C_scan (16 solves; welfare at both E for each; save curve CSV +
              figure)
  Section 11: CF P (two solves -- one per E; welfare at the matching E only;
              save surcharge figure)
  Section 12: CF M (one solve; welfare at both E)
  Section 13: assemble all output CSVs
              -- BEFORE building social_rows/decomp_rows, compute the
                 CF_C_optimal summary per E:
                   for e_name in names(E_GRID):
                     sw_vec      <- sapply(wf_Cscan_byS[[e_name]], `[[`, "SocialWelfare")
                     s_star_idx  <- which.max(sw_vec)
                     s_star      <- s_grid[s_star_idx]
                     scan_summary[[e_name]] <- list(
                       wf     = wf_Cscan_byS[[e_name]][[s_star_idx]],
                       s_used = s_star)
                 -- Then in the scenarios loop, append CF_C_optimal after CF_C
                    using scan_summary[[e_name]]$wf and s_used = s_star.
                 -- s_used column: NA for Baseline/CF_P/CF_M; SUBSIDY_BASE
                    for CF_C; scan_summary[[e_name]]$s_used for CF_C_optimal.
  Section 14: build bar-chart figure (decomposition by scenario, faceted by E)
  Section 15: save Model_Welfare_4p_observed.rds; print final summary

==============================================================================
SMOKE TEST (REQUIRED -- run before full pipeline)
==============================================================================

After Section 6 (helpers defined), run a smoke test:

  - Build cache (if not loaded from fit). Solve BASELINE equilibrium with
    solve_equilibrium_policy_replacement (the standard 3-action version).
  - Verify converged = TRUE.
  - Verify max(abs(eq_base$P - fit_4p$P)) < 1e-3  (CCPs close to the
    saved fit; some tolerance because the fit's $P is post-NPL not
    necessarily a fixed point of solve_eq with the final theta).
    If the saved $P field is missing, skip this check and just verify
    convergence.

  - Run CFM with A_THRESH = 6L on theta_base, cache_base:
    * verify eq_M$P[is_mandated, "maintain"] is exactly 0 (every entry)
    * verify all(abs(rowSums(eq_M$P) - 1) < 1e-8)
    * verify eq_M$converged = TRUE

  - Print pass/fail for each check. If smoke test fails, STOP and report.
    Do not proceed to the full pipeline.

==============================================================================
OPEN R-IMPLEMENTATION QUESTIONS FOR THE RESEARCHER
==============================================================================

Q1. Is the cache saved inside Model_Replacement_Estimates_observed.rds?
    Check at runtime (names(fit_4p) -- look for "cache"). If absent,
    rebuild via:
      cache_4p <- create_estimation_cache_replacement(
                    primitives = prims_obs,
                    obs_panel  = fread(dcm_obs_panel_observed.csv),
                    config     = fit_4p$config)
    Report which path is taken in the first turn.

Q2. compute_weighted_qsd_replacement: is it exported from
    improved_estimator_OPTIMIZED.r, or only defined inline in 04i?
    Coder: grep for the function name in improved_estimator_OPTIMIZED.r;
    if absent, copy verbatim from 04i lines 348-378.

Q3. The geometric-PV formula (per-period x / (1 - beta)) assumes stationary
    behavior, which is what the QSD encodes. For CF M, the QSD must be
    computed UNDER the CFM policy (not baseline). Confirm before running.

Q4. For CF C_scan when s < 0 (a TAX, not a subsidy): GovtOutlayPV is
    negative, representing revenue collected by the government. Sign
    convention for the welfare formula:
      SW = FirmSurplus - ExternalPV - GovtOutlayPV
    When GovtOutlayPV < 0, the subtraction adds to SW -- i.e., revenue
    is treated as a positive contribution to social welfare. Same
    convention as 04i. Confirm before running.

(Do NOT ask economic or modeling questions. The four above are all
mechanical/implementation. If anything else is unclear, STOP and escalate.)

==============================================================================
OUTPUT FORMAT
==============================================================================

After each major CF section, print exactly:

  === CF <letter> -- E_label = <name> ===
  Converged: T/F at iter <N>
  AvgP_M:        <num>
  AvgP_E:        <num>
  AvgP_R:        <num>
  FirmSurplus:   <num>
  ExternalPV:    <num>
  GovtOutlayPV:  <num>
  SocialWelfare: <num>
  dSW vs base:   <num>

For CF C_scan, print only:
  === CF C_scan ===
  s_grid: <length-16 numeric>
  All converged: T/F
  argmax_s (SW @ HEALTH_ONLY):            <s>
  argmax_s (SW @ HEALTH_PLUS_UNMEASURED): <s>

No matrix dumps. No CCP printouts. No QSD printouts. No flow-utility dumps.

After all CFs complete, print the welfare social table (10 rows) to stdout
in tidy form via print(welfare_social), then save the CSVs.

==============================================================================
DO NOT
==============================================================================

- Do not modify improved_estimator_OPTIMIZED.r or cpp_engine.cpp.
  All new code lives inside 04j_4p_Welfare.R.
- Do not change beta or sigma2; read them from $config.
- Do not change EQ_TOL or EQ_MAX_ITER without escalating.
- Do not silently catch errors (no tryCatch returning NULL, no try(silent=TRUE)).
- Do not run 04i, 04h, 04c, or any other estimation script.
  Read the saved rds files only.
- Do not print full matrices, CCPs, V, or QSD weights to the console.
- Do not produce the 4p-vs-8p+FE comparison CSV. (Researcher direction:
  out of scope for this ticket.)
- Do not solve CF M, CF C, or CF C_scan equilibria TWICE per E.
  These equilibria are E-INDEPENDENT; solve once, welfare-eval twice.
- Do not source 04i to call its helpers. Copy helpers in if needed.

==============================================================================
ACCEPTANCE CRITERIA (numbered checklist for reviewer; no inference required)
==============================================================================

C1.  Constants: A_THRESH=6L, SUBSIDY_SCAN_LO=-0.5, SUBSIDY_SCAN_HI=0.99,
     SUBSIDY_N_PTS=16L, SUBSIDY_BASE=0.50, H_PIGOU=1.0, E_GRID_DOLLARS as
     spec'd, EQ_TOL=1e-8, EQ_MAX_ITER=1000L.
C2.  beta and sigma2 read from fit_4p$config (not hardcoded).
C3.  Pre-flight A_bin check runs BEFORE any equilibrium solve and
     hard-stops if A_bin range != 1..8 or n_states != 32 or mandate
     coverage != 12.
C4.  CF C modifies theta via theta[["K_log"]] += log(1 - SUBSIDY_BASE);
     GovtOutlayPV uses K_baseline = exp(theta_base[["K_log"]]) (pre-CF).
C5.  CF C_scan: 16 E-independent equilibrium solves over s in [-0.5, 0.99];
     GovtOutlayPV at each s uses K_baseline and s_val; non-convergence
     warns but does not halt.
C6.  CF P: surcharge_vec = H_PIGOU * E * cache$h_vec / |gamma_price|
     per E; cache_pigou$P_vec = cache$P_vec + surcharge_vec; hazard_loss
     UNCHANGED; GovtOutlayPV = 0; exactly two equilibrium solves total.
C7.  CF M solver matches the inlined pseudocode: pre-mandate 3-action
     softmax, mandated 2-action softmax on (E,R) only, P_M = 0 enforced
     at every iteration, eps_prob floor excludes hard zeros from being
     overwritten.
C8.  CF M post-solve assertions present and pass:
     all(eq_M$P[is_mandated, "maintain"] == 0);
     max|rowSums - 1| < 1e-8; eq_M$converged.
C9.  Welfare formula: SocialWelfare = FirmSurplus - ExternalPV - GovtOutlayPV.
C10. Total main-pipeline equilibrium solves = 21 (1 base + 1 C + 16 C_scan
     + 2 P + 1 M). Smoke-test solves are additive and permitted.
C11. All 8 output files produced at the spec'd paths.
C12. 04j_Welfare_4p_Social.csv: 10 rows, scenarios =
     {Baseline, CF_C, CF_C_optimal, CF_P, CF_M} x 2 E. Includes s_used
     column (NA for Baseline/CF_P/CF_M, 0.50 for CF_C, argmax s* for
     CF_C_optimal).
C13. 04j_Welfare_4p_Decomp.csv: 8 rows, scenarios =
     {CF_C, CF_C_optimal, CF_P, CF_M} x 2 E. Includes s_used column.
C14. CF_C_optimal: per E, s_used = argmax over s_grid of SocialWelfare
     from wf_Cscan_byS; the wf row pulled is the matching s_star entry.
C15. 04j_Welfare_4p_CScan.csv: 32 rows, 16 s x 2 E.
C16. 04j_CFM_QSD_mu.csv: long format with columns (s_idx, Scenario, mu).
C17. Model_Welfare_4p_observed.rds contains: eq_base, eq_C, eq_P_byE,
     eq_M, eq_Cscan_byS, mu_base, mu_C, mu_P_byE, mu_M, theta_4p, beta,
     sigma2, E_GRID, A_THRESH, A_bin_distribution.
C18. No silent error handling anywhere (no tryCatch returning NULL,
     no try(silent=TRUE)).
C19. Smoke test runs before main pipeline: baseline convergence, CCP
     delta vs saved fit (or skip if absent), CF M assertions. STOP on
     failure.

==============================================================================
ATTEMPT LOG
==============================================================================

(Reviewer fills this in after each attempt. Coder leaves empty.)

### Attempt 1 — 2026-05-14
Transcript: 002_transcript.txt (PowerShell transcript — empty per W6; code read directly)
Result: PSEUDOCODE_FAIL

Criteria:
- ✓ Constants: all values match spec (A_THRESH=6L, SUBSIDY_SCAN_LO=-0.5, SUBSIDY_SCAN_HI=0.99, N_PTS=16L, BASE=0.50, H_PIGOU=1.0, E_GRID, EQ_TOL=1e-8, EQ_MAX_ITER=1000L)
- ✓ beta / sigma2: read from fit_4p$config, not hardcoded
- ✓ Pre-flight check: prints A_bin distribution, hard-stops if range != 1..8, n_states != 32, or mandate != 12 states
- ✓ CF C: K_log += log(1-SUBSIDY_BASE); GovtOutlay uses K_baseline = exp(theta_base[["K_log"]]) (pre-CF)
- ✓ CF C_scan: 16 E-independent solves over [-0.5, 0.99]; GovtOutlay uses K_baseline and s_val; warning on non-convergence, no halt
- ✓ CF P: surcharge = H_PIGOU * E * h_vec / |gamma_price| per E; cache_pigou$P_vec modified; hazard_loss unchanged; GovtOutlay=0; one solve per E
- ✓ CF M solver: faithful to inlined pseudocode; 3-action pre-mandate / 2-action mandated softmax; floor/renormalize excludes exact zeros; post-loop U/V recompute; returns is_mandated
- ✓ CF M assertions: all three stopifnot lines present (P_M=0 at mandated, rowSums=1, converged)
- ✓ Welfare formula: SW = FirmSurplus - ExternalPV - GovtOutlayPV
- ✓ Equilibrium solve count: 1+1+16+2+1 = 21 main pipeline (correct); 2 smoke-test solves are additive and permitted by spec
- ✓ All 8 output files produced (rds, 4 CSVs, 3 figures)
- ✓ Social CSV columns match spec (12 columns)
- ✓ Decomp CSV columns match spec (7 columns)
- ✓ CScan CSV columns match spec (12 columns)
- ✓ QSD CSV columns match spec (s_idx, Scenario, mu)
- ✓ RDS contents: all 15 required fields present
- ✓ No silent error handling (no tryCatch returning NULL, no try(silent=TRUE))
- ✓ Smoke test: baseline convergence + CCP delta + CFM assertions — all present and run before pipeline
- ✗ Social CSV row count: spec says 10 rows (5 scenarios × 2 E). Code produces 8 rows (4 scenarios × 2 E). CF C_scan absent from scenarios list (Sections 13-15 only loop over Baseline/CF_C/CF_P/CF_M).
- ✗ Decomp CSV row count: spec says 8 rows (4 non-base × 2 E). Code produces 6 rows (3 non-base × 2 E). CF C_scan absent for same reason.

PSEUDOCODE_FAIL — spec says 5 scenarios (10 rows) in Social and 4 non-base (8 rows) in Decomp,
which counts CF C_scan as a scenario in both tables. But the spec does not specify which
s value to use as the representative CF C_scan summary row, and the only CF C_scan data
already written to 04j_Welfare_4p_CScan.csv (32 rows, all 16 s-points × 2 E).

Question for researcher → Opus:
  For 04j_Welfare_4p_Social.csv and Decomp.csv, should CF C_scan appear as a single
  summary row per E (and if so, at which s: the argmax s* of SocialWelfare, or SUBSIDY_BASE
  = 0.50)? Or did the spec's "5 scenarios / 4 non-base" count include CF C_scan in error,
  and the tables should have 8 / 6 rows as coded?

### Attempt 2 — correction prompt (2026-05-14, from Opus after Attempt 1 PSEUDOCODE_FAIL)
Architect resolved the ambiguity. The spec's "5 scenarios" was intentional;
CF C_scan must contribute a CF_C_optimal summary row using the argmax s* of
SocialWelfare, computed independently per E.

Specific narrow fix to Section 13 ONLY; no other code changes required.

Coder: in Section 13, BEFORE building social_rows / decomp_rows, add this
block (right after the `social_rows <- list(); decomp_rows <- list()` lines):

  scan_summary <- list()
  for (e_name in names(E_GRID)) {
    sw_vec     <- sapply(wf_Cscan_byS[[e_name]], `[[`, "SocialWelfare")
    s_star_idx <- which.max(sw_vec)
    s_star     <- s_grid[s_star_idx]
    scan_summary[[e_name]] <- list(
      wf     = wf_Cscan_byS[[e_name]][[s_star_idx]],
      s_used = s_star)
  }

Then change the `scenarios` list (currently 4 entries) to add CF_C_optimal
between CF_C and CF_P. Because scan_summary is E-indexed (not a flat wf
list), pull its wf inside the e_name loop:

  scenarios <- list(
    Baseline     = list(wf = wf_base, conv = eq_base$converged, n_iter = eq_base$n_iter),
    CF_C         = list(wf = wf_C,    conv = eq_C$converged,    n_iter = eq_C$n_iter),
    CF_C_optimal = list(wf = NULL,    conv = TRUE,              n_iter = NULL),  # wf pulled inside loop
    CF_P         = list(wf = wf_P,    conv = NULL,              n_iter = NULL),
    CF_M         = list(wf = wf_M,    conv = eq_M$converged,    n_iter = eq_M$n_iter)
  )

In the inner loop body, special-case CF_C_optimal to pull wf and s_used
from scan_summary, and add an s_used column to both data.tables:

  if (sc_name == "CF_C_optimal") {
    wf     <- scan_summary[[e_name]]$wf
    s_used <- scan_summary[[e_name]]$s_used
    conv   <- TRUE
  } else {
    wf     <- sc$wf[[e_name]]
    s_used <- if (sc_name == "CF_C") SUBSIDY_BASE else NA_real_
    conv   <- if (sc_name == "CF_P") eq_P_by_E[[e_name]]$converged else sc$conv
  }

  # Append s_used to the data.table:
  social_rows[[length(social_rows) + 1L]] <- data.table(
    E_label = e_name, E_dollars = E_dol, Scenario = sc_name,
    s_used  = s_used,
    Converged = isTRUE(conv),
    ... (rest unchanged)
  )

Same s_used addition to decomp_rows for non-Baseline scenarios.

Re-run the full script (cheap; 21 solves, ~5 min). Verify:
  nrow(dt_social) == 10
  nrow(dt_decomp) == 8
  unique(dt_social$Scenario) == c("Baseline", "CF_C", "CF_C_optimal", "CF_P", "CF_M")
  "s_used" %in% names(dt_social) && "s_used" %in% names(dt_decomp)

The CScan CSV (32 rows) and all figures stay as-is. The bar chart in
Section 14 will pick up the new CF_C_optimal scenario automatically through
the melt on dt_decomp -- no figure code change needed.
