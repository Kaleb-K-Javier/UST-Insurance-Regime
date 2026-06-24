# ==============================================================================
# Code/Analysis/T012_Portfolio_Mix_Diagnostic.R
# READ-ONLY diagnostic. No estimation, no model changes.
#
# Decides the state-space question for the 6p+FE replacement model:
#   C1  size stickiness         -> fixed-type vs transition kernel
#   C2  CCP-by-size within cell  -> does size belong in the state space
#   C3  composition beyond size  -> is "mixed" (worst-tank-coded SW) != all-SW
#   C4  wall-rule sensitivity    -> how much the worst-tank rule distorts kappa/K
#   C5  CF TX-level 45-deg check -> is Texas correctly leveled in the CF
#
# Inputs:  Data/Analysis/dcm_obs_panel_observed.csv
#          Data/Analysis/facility_panel.csv               (select cols)
#          Output/Estimation_Results/Model_Replacement_6paramFE_profile_clean_observed.rds
#          Output/Estimation_Results/CF_TX_FlatFee_results.rds
# Outputs: Output/Tables/T012_*.csv ; Output/Figures/T012_*.png
# ==============================================================================

# ---- SECTION 1: LOGGING ------------------------------------------------------
.log_path <- here::here("logs", paste0(
  "T012_Portfolio_Mix_Diagnostic_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".log"))
dir.create(dirname(.log_path), recursive = TRUE, showWarnings = FALSE)
.log <- file(.log_path, open = "wt")
sink(.log, type = "output"); sink(.log, type = "message", append = TRUE)
on.exit({ sink(type = "output"); sink(type = "message"); close(.log) }, add = TRUE)
cat(sprintf("LOG START %s\nScript: T012_Portfolio_Mix_Diagnostic.R\nR: %s\nWD: %s\n\n",
            .log_path, R.version.string, getwd()))

suppressPackageStartupMessages({
  library(data.table); library(ggplot2); library(here)
})
setDTthreads(0L)
OUT_TAB <- here::here("Output", "Tables")
OUT_FIG <- here::here("Output", "Figures")
dir.create(OUT_TAB, showWarnings = FALSE, recursive = TRUE)
dir.create(OUT_FIG, showWarnings = FALSE, recursive = TRUE)

bin4 <- function(x) fcase(x == 1L, "1", x == 2L, "2", x == 3L, "3",
                          x >= 4L, "4+", default = NA_character_)

# ---- SECTION 2: LOAD ---------------------------------------------------------
cat("=== SECTION 2: LOAD ===\n")
obs <- fread(here::here("Data", "Analysis", "dcm_obs_panel_observed.csv"))
cat(sprintf("  obs: %s rows | %s facilities\n",
            format(nrow(obs), big.mark = ","), format(uniqueN(obs$panel_id), big.mark = ",")))
stopifnot(all(c("panel_id","panel_year","s_idx","A_bin","w_state","rho_state",
                "y_it","I_replace") %in% names(obs)))

KEEP_FP <- c("panel_id","panel_year","texas_treated","n_tanks_active","n_tanks_eoy",
             "n_closures","any_closure","facility_complete_closure",
             "replacement_closure_year","all_sw","all_dw","mixed_wall")
fp <- fread(here::here("Data", "Analysis", "facility_panel.csv"), select = KEEP_FP)
cat(sprintf("  fp: %s rows | %s facilities\n",
            format(nrow(fp), big.mark = ","), format(uniqueN(fp$panel_id), big.mark = ",")))

# Wall composition (3-way) and BOY stock (gap-safe prior-year EOY)
fp[, wall_comp := fcase(all_dw == 1L, "all-DW", mixed_wall == 1L, "mixed",
                        all_sw == 1L, "all-SW", default = "unknown")]
setorder(fp, panel_id, panel_year)
prior <- fp[, .(panel_id, panel_year = panel_year + 1L, boy_stock = n_tanks_eoy)]
fp <- merge(fp, prior, by = c("panel_id","panel_year"), all.x = TRUE)
n_boy_fallback <- fp[is.na(boy_stock) & n_tanks_active > 0L, .N]
fp[is.na(boy_stock), boy_stock := n_tanks_active]
fp[, size_boy := bin4(boy_stock)]
cat(sprintf("  BOY fallback (no prior-year row): %s (%.2f%% of fp)\n",
            format(n_boy_fallback, big.mark = ","), 100 * n_boy_fallback / nrow(fp)))

# BOY wall composition (gap-safe prior-year). EOY composition is 'unknown' at a
# full closure (0 tanks), which silently drops EVERY exit event from a
# composition split; BOY retains the decision-time portfolio for exit years.
prior_wc <- fp[, .(panel_id, panel_year = panel_year + 1L, wall_comp_boy = wall_comp)]
fp <- merge(fp, prior_wc, by = c("panel_id","panel_year"), all.x = TRUE)
fp[is.na(wall_comp_boy), wall_comp_boy := wall_comp]

# ---- SECTION 3: C1 — SIZE TRANSITION MATRIX ---------------------------------
cat("\n=== C1: SIZE STICKINESS (EOY t -> EOY t+1) ===\n")
eoy <- fp[n_tanks_eoy > 0L, .(panel_id, panel_year, eoy_bin = bin4(n_tanks_eoy))]
nxt <- eoy[, .(panel_id, panel_year = panel_year - 1L, eoy_bin_next = eoy_bin)]
trans <- merge(eoy, nxt, by = c("panel_id","panel_year"))   # both t and t+1 active
trans <- trans[!is.na(eoy_bin) & !is.na(eoy_bin_next)]

lvl <- c("1","2","3","4+")
tmat <- trans[, .N, by = .(eoy_bin, eoy_bin_next)]
tmat[, prob := N / sum(N), by = eoy_bin]
tmat[, `:=`(eoy_bin = factor(eoy_bin, lvl), eoy_bin_next = factor(eoy_bin_next, lvl))]
setorder(tmat, eoy_bin, eoy_bin_next)
fwrite(tmat[order(eoy_bin, eoy_bin_next),
            .(from_bin = eoy_bin, to_bin = eoy_bin_next, N, prob = round(prob, 5))],
       file.path(OUT_TAB, "T012_C1_SizeTransitionMatrix.csv"))

diag_prob <- tmat[as.character(eoy_bin) == as.character(eoy_bin_next),
                  .(diag_prob = round(prob, 4)), by = .(bin = eoy_bin)]
pct_change <- 100 * trans[as.character(eoy_bin) != as.character(eoy_bin_next), .N] / nrow(trans)

# Disappearance (inactive next year) rate by bin — context for "exit vs resize"
last_yr <- fp[, max(panel_year)]
disappear <- merge(eoy[panel_year < last_yr, .(panel_id, panel_year, eoy_bin)],
                   nxt, by = c("panel_id","panel_year"), all.x = TRUE)
disap_rate <- disappear[, .(gone_next_yr_pct = round(100 * mean(is.na(eoy_bin_next)), 3),
                            n = .N), by = .(bin = factor(eoy_bin, lvl))][order(bin)]
cat("  Diagonal (stay-in-bin) probabilities:\n"); print(diag_prob)
cat(sprintf("  Active->active pairs changing bin: %.2f%%\n", pct_change))
cat("  Inactive-next-year rate by bin:\n"); print(disap_rate)
fwrite(disap_rate, file.path(OUT_TAB, "T012_C1_DisappearRate.csv"))

# ---- Build merged estimation-sample table (size + composition) --------------
# Use BOY composition for C3/C4 so full-exit events keep their decision-time wall mix.
fp_key <- fp[, .(panel_id, panel_year, size_boy, wall_comp = wall_comp_boy, n_tanks_active)]
m <- merge(obs[!is.na(premium)], fp_key, by = c("panel_id","panel_year"), all.x = TRUE)
m[is.na(size_boy),  size_boy  := "unknown"]
m[is.na(wall_comp), wall_comp := "unknown"]
cat(sprintf("\n  merged est-sample: %s rows | size unknown %.2f%% | wall_comp(BOY) unknown %.2f%%\n",
            format(nrow(m), big.mark = ","),
            100 * mean(m$size_boy == "unknown"), 100 * mean(m$wall_comp == "unknown")))

# Sanity: does BOY composition align with obs worst-tank w_state? (exits now retained)
xtab <- m[wall_comp != "unknown", .N, by = .(w_state, wall_comp)]
cat("  w_state (1=SW,2=DW) x wall_comp(BOY) crosstab:\n"); print(dcast(xtab, w_state ~ wall_comp, value.var = "N", fill = 0))

# ---- SECTION 4: C2 — CCP BY SIZE WITHIN CELL --------------------------------
cat("\n=== C2: CCP VARIATION BY SIZE WITHIN CELL ===\n")
NMIN <- 200L
sc <- m[size_boy != "unknown", .(
  n     = .N,
  P_M   = mean(y_it == 0L),
  P_E   = mean(y_it == 1L & (is.na(I_replace) | I_replace == 0L)),
  P_R   = mean(y_it == 1L & !is.na(I_replace) & I_replace == 1L),
  P_cl  = mean(y_it == 1L)
), by = .(s_idx, A_bin, w_state, rho_state, size_boy)]
fwrite(sc[order(s_idx, size_boy)], file.path(OUT_TAB, "T012_C2_CCP_by_Size_Cell.csv"))

# within-cell spread across size bins (cells where >=2 size bins clear NMIN)
sc_ok <- sc[n >= NMIN]
spread <- sc_ok[, .(
  n_bins      = .N,
  cell_n      = sum(n),
  spread_Pcl  = max(P_cl) - min(P_cl),
  spread_PE   = max(P_E)  - min(P_E),
  spread_PR   = max(P_R)  - min(P_R)
), by = .(s_idx, A_bin, w_state, rho_state)][n_bins >= 2L]
setorder(spread, -spread_Pcl)
wt <- function(x, w) sum(x * w) / sum(w)
cat(sprintf("  Cells with >=2 size bins (n>=%d): %d\n", NMIN, nrow(spread)))
cat(sprintf("  Within-cell spread in P_close  — obs-wt mean %.4f | max %.4f\n",
            wt(spread$spread_Pcl, spread$cell_n), max(spread$spread_Pcl)))
cat(sprintf("  Within-cell spread in P(Exit)  — obs-wt mean %.4f | max %.4f\n",
            wt(spread$spread_PE,  spread$cell_n), max(spread$spread_PE)))
cat(sprintf("  Within-cell spread in P(Replace)— obs-wt mean %.4f | max %.4f\n",
            wt(spread$spread_PR,  spread$cell_n), max(spread$spread_PR)))
cat("  Top-5 cells by P_close spread across size:\n")
print(head(spread[, .(s_idx, A_bin, w_state, rho_state,
                      spread_Pcl = round(spread_Pcl,4),
                      spread_PE  = round(spread_PE,4),
                      spread_PR  = round(spread_PR,4))], 5))
fwrite(spread, file.path(OUT_TAB, "T012_C2_WithinCell_Spread.csv"))

# ---- SECTION 5: C3 — COMPOSITION BEYOND SIZE (SW pool only) -----------------
cat("\n=== C3: COMPOSITION (all-SW vs mixed) WITHIN SIZE ===\n")
# SW-coded obs split by fp composition; compare all-SW vs mixed within (size,age,regime)
sw <- m[w_state == 1L & wall_comp %in% c("all-SW","mixed") & size_boy != "unknown"]
comp <- sw[, .(
  n    = .N,
  P_E  = mean(y_it == 1L & (is.na(I_replace) | I_replace == 0L)),
  P_R  = mean(y_it == 1L & !is.na(I_replace) & I_replace == 1L),
  P_cl = mean(y_it == 1L)
), by = .(size_boy, A_bin, rho_state, wall_comp)]
fwrite(comp[order(size_boy, A_bin, rho_state, wall_comp)],
       file.path(OUT_TAB, "T012_C3_Composition_within_Size.csv"))

cw <- dcast(comp[n >= NMIN], size_boy + A_bin + rho_state ~ wall_comp,
            value.var = c("n","P_cl","P_E","P_R"))
# keep strata where both comps clear NMIN
cw <- cw[!is.na(`n_all-SW`) & !is.na(n_mixed)]
if (nrow(cw) > 0) {
  cw[, d_Pcl := P_cl_mixed - `P_cl_all-SW`]
  cw[, d_PR  := P_R_mixed  - `P_R_all-SW`]
  cw[, d_PE  := P_E_mixed  - `P_E_all-SW`]
  cw[, wsum  := `n_all-SW` + n_mixed]
  cat(sprintf("  Strata with both all-SW & mixed (n>=%d each): %d\n", NMIN, nrow(cw)))
  cat(sprintf("  mixed - all-SW  (obs-wt mean):  dP_close %+.4f | dP_E %+.4f | dP_R %+.4f\n",
              wt(cw$d_Pcl, cw$wsum), wt(cw$d_PE, cw$wsum), wt(cw$d_PR, cw$wsum)))
  fwrite(cw, file.path(OUT_TAB, "T012_C3_Composition_Diff_Wide.csv"))
} else {
  cat("  No strata with both all-SW and mixed clearing NMIN — composition split too thin.\n")
}

# ---- SECTION 6: C4 — WALL-RULE SENSITIVITY ----------------------------------
cat("\n=== C4: WALL-RULE / COMPOSITION CONDITIONAL SHARES ===\n")
cl <- m[y_it == 1L & wall_comp != "unknown"]
cond <- cl[, .(
  n_closures = .N,
  P_R_cond   = mean(!is.na(I_replace) & I_replace == 1L),
  P_E_cond   = mean(is.na(I_replace)  | I_replace == 0L)
), by = wall_comp]
# combined SW pool (current worst-tank rule) = all-SW + mixed
sw_pool <- cl[wall_comp %in% c("all-SW","mixed"), .(
  wall_comp = "SW pool (all-SW+mixed)",
  n_closures = .N,
  P_R_cond   = mean(!is.na(I_replace) & I_replace == 1L),
  P_E_cond   = mean(is.na(I_replace)  | I_replace == 0L))]
cond <- rbind(cond, sw_pool)
cond[, `:=`(P_R_cond = round(P_R_cond, 4), P_E_cond = round(P_E_cond, 4))]
setorder(cond, wall_comp)
cat("  Conditional shares among closures by composition:\n"); print(cond)
fwrite(cond, file.path(OUT_TAB, "T012_C4_WallRule_ConditionalShares.csv"))

# Unconditional action rates by BOY composition (over ALL est-sample rows) —
# the direct read on how pooling 'mixed' into SW distorts the exit/replace margins.
uncond <- m[wall_comp != "unknown", .(
  n_obs = .N,
  P_E   = round(mean(y_it == 1L & (is.na(I_replace) | I_replace == 0L)), 5),
  P_R   = round(mean(y_it == 1L & !is.na(I_replace) & I_replace == 1L), 5),
  P_cl  = round(mean(y_it == 1L), 5)
), by = wall_comp]
sw_pool_u <- m[wall_comp %in% c("all-SW","mixed"), .(
  wall_comp = "SW pool (all-SW+mixed)", n_obs = .N,
  P_E  = round(mean(y_it == 1L & (is.na(I_replace) | I_replace == 0L)), 5),
  P_R  = round(mean(y_it == 1L & !is.na(I_replace) & I_replace == 1L), 5),
  P_cl = round(mean(y_it == 1L), 5))]
uncond <- rbind(uncond, sw_pool_u); setorder(uncond, wall_comp)
cat("  Unconditional action rates by composition:\n"); print(uncond)
fwrite(uncond, file.path(OUT_TAB, "T012_C4_Unconditional_byComposition.csv"))

# C4b — ROUTING: where do Exit vs Replace closures of each BOY composition land
# in the obs worst-tank w_state? Reveals whether DW exits are routed into SW
# cells (the mechanism behind the DW cells' artificial P(R|cl)=1).
route <- m[y_it == 1L & wall_comp != "unknown", .(n = .N),
           by = .(wall_comp_boy = wall_comp, obs_w_state = w_state,
                  action = fifelse(!is.na(I_replace) & I_replace == 1L, "Replace", "Exit"))]
route_wide <- dcast(route, wall_comp_boy + action ~ paste0("obs_w", obs_w_state),
                    value.var = "n", fill = 0)
cat("  Closure routing (BOY composition x action -> obs w_state cell):\n")
print(route_wide)
fwrite(route_wide, file.path(OUT_TAB, "T012_C4b_ClosureRouting.csv"))

mixed_share_of_sw_cl <- 100 * cl[wall_comp == "mixed", .N] /
                              cl[wall_comp %in% c("all-SW","mixed"), .N]
cat(sprintf("  Mixed share of all 'SW' closures: %.1f%% (this fraction contaminates kappa_SW/K_SW)\n",
            mixed_share_of_sw_cl))

# ---- SECTION 7: C5 — CF TX-LEVEL 45-DEG CHECK -------------------------------
cat("\n=== C5: CF TX-LEVEL CHECK (P_baseline re-solve vs fit$P_hat) ===\n")
fit_path <- here::here("Output","Estimation_Results",
                       "Model_Replacement_6paramFE_profile_clean_observed.rds")
cf_path  <- here::here("Output","Estimation_Results","CF_TX_FlatFee_results.rds")
if (file.exists(fit_path) && file.exists(cf_path)) {
  fit <- readRDS(fit_path); cf <- readRDS(cf_path)
  lut <- fit$cache$state_lut
  tx_s <- sort(lut[rho_state == 2L, s_idx])
  Ph <- fit$P_hat; Pb <- cf$P_baseline
  stopifnot(all(dim(Ph) == c(32L,3L)), all(dim(Pb) == c(32L,3L)))
  dd <- data.table(
    s_idx  = rep(tx_s, 3),
    action = rep(c("maintain","exit","replace"), each = length(tx_s)),
    fitted   = as.numeric(Ph[tx_s, ]),
    resolved = as.numeric(Pb[tx_s, ]))
  dd[, abs_diff := abs(fitted - resolved)]
  cat(sprintf("  TX cells: %d | max|fit - resolve| = %.3e | mean = %.3e\n",
              length(tx_s), max(dd$abs_diff), mean(dd$abs_diff)))
  print(dd[, .(max_abs = round(max(abs_diff), 8),
               mean_abs = round(mean(abs_diff), 8)), by = action])
  fwrite(dd[, .(s_idx, action, fitted = round(fitted,6),
                resolved = round(resolved,6), abs_diff = round(abs_diff,8))],
         file.path(OUT_TAB, "T012_C5_TXLevel_Check.csv"))
  TX_OK <- max(dd$abs_diff) < 1e-4
  cat(sprintf("  VERDICT: TX correctly leveled in CF = %s (threshold 1e-4)\n", TX_OK))
} else {
  cat("  SKIP — fit or CF rds missing.\n"); TX_OK <- NA
}

# ---- SECTION 8: FIGURES ------------------------------------------------------
cat("\n=== SECTION 8: FIGURES ===\n")
ggsave(file.path(OUT_FIG, "T012_C1_SizeTransition_Heatmap.png"),
  ggplot(tmat, aes(eoy_bin, eoy_bin_next, fill = prob)) +
    geom_tile() + geom_text(aes(label = sprintf("%.3f", prob)), size = 3.5) +
    scale_fill_gradient(low = "white", high = "#2A9D8F", limits = c(0,1)) +
    labs(x = "Size bin, year t", y = "Size bin, year t+1",
         title = "Facility size transition (EOY stock, active->active)",
         subtitle = sprintf("Diagonal = stay-in-bin. %.2f%% of active pairs change bin.", pct_change)) +
    theme_minimal(base_size = 11),
  width = 6.5, height = 5, dpi = 150)

if (exists("cond")) {
  ggsave(file.path(OUT_FIG, "T012_C4_CondShares_byComposition.png"),
    ggplot(melt(cond[wall_comp %in% c("all-SW","mixed","all-DW")],
                id.vars = "wall_comp", measure.vars = c("P_R_cond","P_E_cond")),
           aes(wall_comp, value, fill = variable)) +
      geom_col(position = "dodge") +
      scale_fill_manual(values = c(P_R_cond = "#2A9D8F", P_E_cond = "#E76F51"),
                        labels = c("P(Replace|cl)","P(Exit|cl)")) +
      labs(x = "Wall composition", y = "Conditional share", fill = NULL,
           title = "Conditional closure outcome by wall composition",
           subtitle = "Worst-tank rule lumps all-SW + mixed into the SW pool") +
      theme_minimal(base_size = 11),
    width = 7, height = 4.5, dpi = 150)
}

# ---- SECTION 9: DECISION SUMMARY --------------------------------------------
cat("\n=== T012 DECISION SUMMARY ===\n")
cat(sprintf("C1 size stickiness : min diagonal = %.3f | %.2f%% of pairs change bin\n",
            min(diag_prob$diag_prob), pct_change))
cat(sprintf("     -> fixed-type %s\n",
            if (min(diag_prob$diag_prob) >= 0.95 && pct_change < 5) "JUSTIFIED (skip kernel)"
            else "QUESTIONABLE (size moves enough to need a kernel or 4th action)"))
cat(sprintf("C2 size-in-state   : obs-wt within-cell P_close spread = %.4f | max %.4f\n",
            wt(spread$spread_Pcl, spread$cell_n), max(spread$spread_Pcl)))
if (exists("cw") && nrow(cw) > 0)
  cat(sprintf("C3 composition     : mixed-vs-allSW obs-wt dP_close %+.4f | dP_R %+.4f\n",
              wt(cw$d_Pcl, cw$wsum), wt(cw$d_PR, cw$wsum)))
cat(sprintf("C4 worst-tank rule : mixed = %.1f%% of SW closures; SW-pool P(R|cl)=%.3f vs all-SW alone=%.3f\n",
            mixed_share_of_sw_cl,
            cond[wall_comp == "SW pool (all-SW+mixed)", P_R_cond],
            cond[wall_comp == "all-SW", P_R_cond]))
cat(sprintf("C5 CF TX level     : %s\n",
            if (isTRUE(TX_OK)) "PASS (TX correctly leveled)" else
            if (is.na(TX_OK)) "SKIPPED" else "FAIL — investigate welfare levels"))
cat("\n=== T012 COMPLETE ===\n")
