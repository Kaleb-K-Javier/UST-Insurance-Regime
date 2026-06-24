# ==============================================================================
# Code/Analysis/T013_Transition_and_State_Leakage_Diagnostic.R
# READ-ONLY. The two master tripwires for state-coding errors that propagate
# 02b -> 04b -> 04 structural.
#
#   D1  Transition realization vs model F  (empirical P(s'|s,a) vs F_maintain /
#       F_replace; off-support mass = coding error). Also exit-absorbing &
#       replace-reset realization.
#   D2  Decision-time vs realization-time state leakage at closures
#       (computed from the saved T012_C4b routing table; no 3GB re-read).
#
# Inputs:  Data/Analysis/dcm_obs_panel_observed.csv
#          Output/Estimation_Results/Model_Replacement_6paramFE_profile_clean_observed.rds
#          Output/Tables/T012_C4b_ClosureRouting.csv
#          Code/Helpers/improved_estimator_OPTIMIZED.r  (for .get_F_mats)
# Outputs: Output/Tables/T013_*.csv ; Output/Figures/T013_*.png
# ==============================================================================

# ---- SECTION 1: LOGGING ------------------------------------------------------
.log_path <- here::here("logs", paste0(
  "T013_Transition_State_Leakage_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".log"))
dir.create(dirname(.log_path), recursive = TRUE, showWarnings = FALSE)
.log <- file(.log_path, open = "wt")
sink(.log, type = "output"); sink(.log, type = "message", append = TRUE)
on.exit({ sink(type = "output"); sink(type = "message"); close(.log) }, add = TRUE)
cat(sprintf("LOG START %s\nScript: T013\nR: %s\nWD: %s\n\n",
            .log_path, R.version.string, getwd()))

suppressPackageStartupMessages({ library(data.table); library(ggplot2); library(here) })
setDTthreads(0L)
OUT_TAB <- here::here("Output", "Tables"); OUT_FIG <- here::here("Output", "Figures")
dir.create(OUT_TAB, showWarnings = FALSE, recursive = TRUE)
dir.create(OUT_FIG, showWarnings = FALSE, recursive = TRUE)

# ---- SECTION 2: LOAD + MODEL F ----------------------------------------------
cat("=== SECTION 2: LOAD ===\n")
source(here::here("Code", "Helpers", "improved_estimator_OPTIMIZED.r"))
tryCatch(Rcpp::sourceCpp(here::here("Code", "Helpers", "cpp_engine.cpp")),
         error = function(e) cat("  (cpp fallback — not needed for this diagnostic)\n"))

fit <- readRDS(here::here("Output","Estimation_Results",
                          "Model_Replacement_6paramFE_profile_clean_observed.rds"))
cache <- fit$cache
lut   <- as.data.table(cache$state_lut)[, .(s_idx, A_bin, w_state, rho_state)]
Fm    <- .get_F_mats(cache)
FM_d  <- as.matrix(Fm$FM); FR_d <- as.matrix(Fm$FR)   # 32x32 — tiny, dense OK
stopifnot(all(dim(FM_d) == c(32L,32L)), all(dim(FR_d) == c(32L,32L)))
cat(sprintf("  F_maintain nonzero entries: %d | F_replace nonzero: %d\n",
            sum(FM_d > 1e-12), sum(FR_d > 1e-12)))

obs <- fread(here::here("Data","Analysis","dcm_obs_panel_observed.csv"),
             select = c("panel_id","panel_year","s_idx","y_it","I_replace"))
# labels come from state_lut only (s_idx is a bijection to A_bin/wall/regime)
lut_to <- copy(lut); setnames(lut_to, c("s_next","A_bin_to","w_state_to","rho_state_to"))
cat(sprintf("  obs: %s rows | %s facilities\n",
            format(nrow(obs), big.mark=","), format(uniqueN(obs$panel_id), big.mark=",")))

# ---- SECTION 3: BUILD (s_t, a_t, s_{t+1}) TRIPLES ---------------------------
cat("\n=== D1: TRANSITION REALIZATION vs MODEL F ===\n")
setorder(obs, panel_id, panel_year)
obs[, `:=`(s_next  = shift(s_idx,     1L, type = "lead"),
           yr_next = shift(panel_year, 1L, type = "lead")), by = panel_id]
obs[, consec := !is.na(yr_next) & yr_next == panel_year + 1L]
obs[, act := fcase(y_it == 0L,                                          "M",
                   y_it == 1L & (is.na(I_replace) | I_replace == 0L),  "E",
                   y_it == 1L & I_replace == 1L,                        "R")]

Fval <- function(Fd, s, sn) Fd[cbind(s, sn)]

# --- Maintain: should stay same wall+regime, age same or +1 ---
mt <- obs[act == "M" & consec & !is.na(s_next)]
mt[, fval := Fval(FM_d, s_idx, s_next)]
mt[, off  := fval <= 1e-12]
off_M <- mt[, mean(off)]
# Wall / regime / age crossing flags (model-forbidden under maintain)
mt2 <- merge(mt, lut, by = "s_idx", all.x = TRUE)
mt2 <- merge(mt2, lut_to, by = "s_next", all.x = TRUE)
dw_to_sw  <- mt2[w_state == 2L & w_state_to == 1L, .N]
sw_to_dw  <- mt2[w_state == 1L & w_state_to == 2L, .N]
regime_sw <- mt2[rho_state != rho_state_to, .N]
age_jump  <- mt2[A_bin_to > A_bin + 1L | A_bin_to < A_bin, .N]
cat(sprintf("  MAINTAIN transitions: %s | off-support mass = %.4f (%s rows)\n",
            format(nrow(mt), big.mark=","), off_M, format(mt[off==TRUE,.N], big.mark=",")))
cat(sprintf("    wall DW->SW: %s | wall SW->DW: %s | regime change: %s | age jump/back: %s\n",
            format(dw_to_sw, big.mark=","), format(sw_to_dw, big.mark=","),
            format(regime_sw, big.mark=","), format(age_jump, big.mark=",")))

# --- Replace: should reset to model's replace target (age bin 1, F_replace wall) ---
rt <- obs[act == "R" & consec & !is.na(s_next)]
rt[, fval := Fval(FR_d, s_idx, s_next)]
rt[, off  := fval <= 1e-12]
off_R <- rt[, mean(off)]
rt2 <- merge(rt, lut, by = "s_idx", all.x = TRUE)
rt2 <- merge(rt2, lut_to, by = "s_next", all.x = TRUE)
cat(sprintf("  REPLACE transitions: %s | off-support mass = %.4f (%s rows)\n",
            format(nrow(rt), big.mark=","), off_R, format(rt[off==TRUE,.N], big.mark=",")))
cat(sprintf("    post-replace age-bin distribution (should concentrate at model reset bin):\n"))
print(rt2[, .N, by = A_bin_to][order(A_bin_to)])

# --- Exit: should be absorbing (no consecutive next active row) ---
ex <- obs[act == "E"]
reentry <- ex[, mean(consec)]
cat(sprintf("  EXIT rows: %s | re-entry rate (active next yr, absorbing-violation) = %.4f (%s rows)\n",
            format(nrow(ex), big.mark=","), reentry, format(ex[consec==TRUE,.N], big.mark=",")))

# --- Top off-support maintain flows (this is where DW->SW etc. surface) ---
top_off <- mt2[off == TRUE, .N, by = .(s_idx, s_next, w_state, A_bin, rho_state,
                                       w_state_to, A_bin_to, rho_state_to)][order(-N)][1:min(.N,12)]
top_off[, `:=`(
  from = sprintf("A%d/%s/%s", A_bin, c("SW","DW")[w_state], c("FF","RB")[rho_state]),
  to   = sprintf("A%d/%s/%s", A_bin_to, c("SW","DW")[w_state_to], c("FF","RB")[rho_state_to]))]
cat("  Top off-support MAINTAIN flows (model forbids; data shows):\n")
print(top_off[, .(from, to, N)])

# Save D1 summary
d1 <- data.table(
  metric = c("maintain_off_support_mass","maintain_n_trans","maintain_DW_to_SW",
             "maintain_SW_to_DW","maintain_regime_change","maintain_age_jump",
             "replace_off_support_mass","replace_n_trans","exit_reentry_rate","exit_n"),
  value  = c(round(off_M,5), nrow(mt), dw_to_sw, sw_to_dw, regime_sw, age_jump,
             round(off_R,5), nrow(rt), round(reentry,5), nrow(ex)))
fwrite(d1, file.path(OUT_TAB, "T013_D1_TransitionRealization_Summary.csv"))
fwrite(top_off[, .(from, to, N)], file.path(OUT_TAB, "T013_D1_TopOffSupportFlows.csv"))

# ---- SECTION 4: D2 — CLOSURE CELL-ASSIGNMENT LEAKAGE (from T012 C4b) ---------
cat("\n=== D2: DECISION-TIME vs ASSIGNED WALL CELL (closure leakage) ===\n")
c4b_path <- file.path(OUT_TAB, "T012_C4b_ClosureRouting.csv")
if (file.exists(c4b_path)) {
  rt4 <- fread(c4b_path)   # cols: wall_comp_boy, action, obs_w1, obs_w2
  # decision-time (BOY) wall: all-DW -> DW; all-SW/mixed -> SW (worst-tank on BOY)
  rt4[, boy_wall := fifelse(wall_comp_boy == "all-DW", "DW", "SW")]
  tot <- rt4[, sum(obs_w1 + obs_w2)]
  # leaked = closures whose ASSIGNED cell wall != BOY decision-time wall
  leak_DWorigin_toSW <- rt4[boy_wall == "DW", sum(obs_w1)]          # DW facility -> SW cell
  leak_SWorigin_toDW <- rt4[boy_wall == "SW", sum(obs_w2)]          # SW/mixed -> DW cell
  leak_total <- leak_DWorigin_toSW + leak_SWorigin_toDW
  cat(sprintf("  Total closures: %s | mis-celled (assigned wall != decision-time wall): %s (%.1f%%)\n",
              format(tot, big.mark=","), format(leak_total, big.mark=","),
              100*leak_total/tot))
  cat(sprintf("    DW-origin closures routed to SW cells: %s\n", format(leak_DWorigin_toSW, big.mark=",")))
  cat(sprintf("    SW/mixed-origin closures routed to DW cells: %s\n", format(leak_SWorigin_toDW, big.mark=",")))

  # Contamination of the SW exit margin (kappa_SW) by DW-origin exits
  sw_cell_exits     <- rt4[action == "Exit", sum(obs_w1)]
  dw_origin_in_sw_E <- rt4[action == "Exit" & boy_wall == "DW", sum(obs_w1)]
  # Composition of the DW replace cell (K_DW): genuine all-DW vs mis-celled SW->DW retrofit
  dw_cell_repl      <- rt4[action == "Replace", sum(obs_w2)]
  genuine_dw_repl   <- rt4[action == "Replace" & wall_comp_boy == "all-DW", sum(obs_w2)]
  cat(sprintf("  SW-cell exit margin: %.1f%% contaminated by DW-origin exits (%s of %s)\n",
              100*dw_origin_in_sw_E/sw_cell_exits,
              format(dw_origin_in_sw_E, big.mark=","), format(sw_cell_exits, big.mark=",")))
  cat(sprintf("  DW-cell REPLACE pool: only %.1f%% are genuine all-DW decisions; %.1f%% are mis-celled SW->DW retrofits\n",
              100*genuine_dw_repl/dw_cell_repl, 100*(1-genuine_dw_repl/dw_cell_repl)))

  d2 <- data.table(
    metric = c("total_closures","mis_celled","mis_celled_pct",
               "DWorigin_to_SWcell","SWorigin_to_DWcell",
               "SW_exit_margin_DWcontam_pct","DWcell_replace_genuine_pct"),
    value  = c(tot, leak_total, round(100*leak_total/tot,2),
               leak_DWorigin_toSW, leak_SWorigin_toDW,
               round(100*dw_origin_in_sw_E/sw_cell_exits,2),
               round(100*genuine_dw_repl/dw_cell_repl,2)))
  fwrite(d2, file.path(OUT_TAB, "T013_D2_ClosureLeakage_Summary.csv"))
} else {
  cat("  SKIP D2 — run T012 first to produce T012_C4b_ClosureRouting.csv\n")
}

# ---- SECTION 5: FIGURE -------------------------------------------------------
if (nrow(top_off) > 0) {
  top_off[, lab := factor(sprintf("%s -> %s", from, to), levels = rev(sprintf("%s -> %s", from, to)))]
  ggsave(file.path(OUT_FIG, "T013_D1_TopOffSupportFlows.png"),
    ggplot(top_off, aes(lab, N)) + geom_col(fill = "#E76F51") + coord_flip() +
      labs(x = NULL, y = "Facility-year transitions",
           title = "Model-forbidden MAINTAIN transitions present in the data",
           subtitle = sprintf("Off-support mass = %.2f%% of maintain transitions. DW->SW = worst-tank artifact.",
                              100*off_M)) +
      theme_minimal(base_size = 11),
    width = 8, height = 5, dpi = 150)
}

# ---- SECTION 6: VERDICT ------------------------------------------------------
cat("\n=== T013 VERDICT ===\n")
cat(sprintf("D1 maintain off-support : %.3f%% (DW->SW: %s)\n", 100*off_M, format(dw_to_sw, big.mark=",")))
cat(sprintf("D1 replace  off-support : %.3f%%\n", 100*off_R))
cat(sprintf("D1 exit re-entry        : %.3f%% (should be ~0 if exit absorbing)\n", 100*reentry))
cat("=== T013 COMPLETE ===\n")
