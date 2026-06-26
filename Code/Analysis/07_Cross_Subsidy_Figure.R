################################################################################
# 07_Cross_Subsidy_Figure.R  —  PERC M1: Cross-subsidy under flat-fee trust fund
#
# Actuarially fair premium PP = lambda * S_bar vs break-even flat fee tau.
# Produces one pooled transfer-curve figure and two summary tables.
#
# INPUTS (via z_path):
#   Data/Processed/incident_level_claims.csv   — severity pool (CO,NM,PA,TN)
#   Data/Analysis/analysis_hazard_predictions_full.csv — lambda (CO,LA,NM,PA,TN)
#
# OUTPUTS (via here):
#   Output/Tables/cross_subsidy_severity_cells.csv
#   Output/Tables/cross_subsidy_state_summary.csv
#   Data/Analysis/cross_subsidy_facility.csv
#   Output/Figures/Fig_CrossSub_TransferCurve_Panel.{pdf,png}
#
# TICKET: 030  |  Attempt: 1  |  2026-06-25
################################################################################

suppressPackageStartupMessages({
  library(here)
  library(data.table)
  library(ggplot2)
  library(scales)
})

source(here::here("Code", "Helpers", "data_paths.R"))

options(scipen = 999)

# === LOGGING ===
.script_name <- "07_Cross_Subsidy_Figure"
.log_path <- here("logs", paste0(.script_name, "_",
                                  format(Sys.time(), "%Y%m%d_%H%M%S"), ".log"))
dir.create(dirname(.log_path), recursive = TRUE, showWarnings = FALSE)
.log <- file(.log_path, open = "wt")
sink(.log, type = "output")
sink(.log, type = "message", append = TRUE)
on.exit({
  sink(type = "output")
  sink(type = "message")
  close(.log)
}, add = TRUE)
cat(sprintf("LOG START %s\nScript: %s\nR: %s\nWD: %s\n\n",
    .log_path, .script_name, R.version.string, getwd()))

# ── Output directories ─────────────────────────────────────────────────────────
OUTPUT_FIGURES <- here("Output", "Figures")
OUTPUT_TABLES  <- here("Output", "Tables")
OUTPUT_DATA    <- here("Data", "Analysis")
for (d in c(OUTPUT_FIGURES, OUTPUT_TABLES, OUTPUT_DATA))
  dir.create(d, recursive = TRUE, showWarnings = FALSE)

# ── Figure theme (matches project convention from 06_Actuarial_Alignment.R) ──
theme_pub <- function(base_size = 11) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title       = element_text(face = "bold", size = rel(1.05),
                                      margin = margin(0, 0, 6, 0)),
      plot.subtitle    = element_text(color = "grey40", size = rel(0.83),
                                      margin = margin(0, 0, 8, 0)),
      plot.caption     = element_text(color = "grey50", size = rel(0.70),
                                      hjust = 0, margin = margin(8, 0, 0, 0)),
      axis.title       = element_text(face = "bold", size = rel(0.9)),
      panel.grid.minor = element_blank(),
      plot.background  = element_rect(fill = "white", color = NA)
    )
}

save_fig <- function(p, name, w = 8, h = 5) {
  ggsave(file.path(OUTPUT_FIGURES, paste0(name, ".png")),
         p, width = w, height = h, dpi = 300, bg = "white")
  ggsave(file.path(OUTPUT_FIGURES, paste0(name, ".pdf")),
         p, width = w, height = h, device = grDevices::cairo_pdf)
  cat(sprintf("  Saved: %s (.png + .pdf)\n", name))
}

# ── Constants ─────────────────────────────────────────────────────────────────
SEV_STATES <- c("CO", "NM", "PA", "TN")   # severity pool
FIG_STATES <- c("CO", "LA", "NM", "PA", "TN")  # figure states (have lambda)
D_NM       <- 10000  # NM is GROSS state; net = max(C - D_NM, 0)


# =============================================================================
cat("=== STEP 1: SEVERITY CLAIMS ===\n")
# =============================================================================

inc_path <- z_path("Data", "Processed", "incident_level_claims.csv")

# Column guard — cheap insurance against upstream rename
inc_hdr  <- fread(inc_path, nrows = 0)
req_cols <- c("panel_id", "state", "total_cost", "total_cost_2023",
              "age_bins", "has_single_walled", "has_double_walled")
stopifnot(
  "incident_level_claims.csv missing required columns" =
    all(req_cols %in% names(inc_hdr))
)
cat("Column guard PASS\n")

inc <- fread(inc_path,
             select = c("panel_id", "state", "total_cost", "total_cost_2023",
                        "age_bins", "has_single_walled", "has_double_walled"))
inc <- inc[state %in% SEV_STATES]
cat(sprintf("Rows in severity pool {%s}: %d\n",
            paste(SEV_STATES, collapse = ","), nrow(inc)))

# Drop rows where cost columns are NA (cannot compute net or deflated cost)
n_na_cost <- inc[is.na(total_cost) | is.na(total_cost_2023), .N]
if (n_na_cost > 0) {
  cat(sprintf("Dropping %d rows with NA total_cost or NA total_cost_2023\n", n_na_cost))
  print(inc[is.na(total_cost) | is.na(total_cost_2023), .N, by = state])
  inc <- inc[!is.na(total_cost) & !is.na(total_cost_2023)]
}

# Drop rows with negative costs (data anomalies; deflation ratio would flip sign)
n_neg_cost <- inc[total_cost < 0 | total_cost_2023 < 0, .N]
if (n_neg_cost > 0) {
  cat(sprintf("Dropping %d rows with negative total_cost or total_cost_2023\n", n_neg_cost))
  print(inc[total_cost < 0 | total_cost_2023 < 0,
            .(state, total_cost, total_cost_2023)])
  inc <- inc[total_cost >= 0 & total_cost_2023 >= 0]
}

# Eq.1 — net nominal cost
# CO/TN/PA: recorded total_cost is already (C-D)+  (NET states, per verified 2026-06-25)
# NM:       recorded total_cost is gross C          (GROSS state)
inc[, net_nom := fifelse(state == "NM",
                          pmax(total_cost - D_NM, 0),
                          total_cost)]

# Eq.1 — deflate net nominal to 2023 USD via CPI ratio in the data
# When net_nom == 0 (sub-deductible), set net_2023 := 0 directly to avoid 0/0
inc[, net_2023 := fifelse(net_nom == 0,
                           0,
                           net_nom * (total_cost_2023 / total_cost))]

# Catch any residual non-finite or negative net_2023 (should not occur after guards above)
nf_n <- inc[!is.finite(net_2023) | net_2023 < 0, .N]
if (nf_n > 0) {
  cat(sprintf("WARNING: %d unexpected non-finite or negative net_2023\n", nf_n))
  print(inc[!is.finite(net_2023) | net_2023 < 0,
            .(state, total_cost, total_cost_2023, net_nom, net_2023)][seq_len(min(nf_n, 10))])
}

# Wall type
inc[, wall := fifelse(has_single_walled == 1L, "single",
               fifelse(has_double_walled == 1L, "double", NA_character_))]

# Flat pooled S_bar uses ONLY net cost: wall/age are NOT needed, and NA wall/age
# must NOT drop rows (that previously excluded ALL of NM). Keep every row with a
# valid net_2023; report NA counts for transparency only.
n_na_age  <- inc[is.na(age_bins), .N]
n_na_wall <- inc[is.na(wall), .N]
cat(sprintf("NA age_bins: %d | NA wall: %d (NOT dropped; flat S_bar needs neither)\n",
            n_na_age, n_na_wall))

# Step 1 assertions
stopifnot(all(is.finite(inc$net_2023)))
stopifnot(all(inc$net_2023 >= 0))
stopifnot(!anyNA(inc$net_2023))
cat("Step 1 assertions PASS\n")

cat("N and N_zero by state (sub-deductible zeros kept):\n")
print(inc[, .(N = .N, N_zero = sum(net_2023 == 0)), by = state][order(state)])


# =============================================================================
cat("=== STEP 2: SEVERITY DESCRIPTIVE ===\n")
# =============================================================================
# Pooled S_bar is used in PP. Age-bin breakdown is printed for robustness only.

sev_by_age <- inc[, .(S = mean(net_2023), n = .N), by = age_bins][order(age_bins)]
cat("Severity by age bin (descriptive; S_bar used in PP, not cell means):\n")
print(sev_by_age)

S_bar <- inc[, mean(net_2023)]
cat(sprintf("Pooled S_bar [USED_SEVERITY = flat]: %.2f 2023-USD\n", S_bar))

fwrite(sev_by_age, file.path(OUTPUT_TABLES, "cross_subsidy_severity_cells.csv"))
cat("Written: Output/Tables/cross_subsidy_severity_cells.csv\n")


# =============================================================================
cat("=== STEP 3: HAZARD FREQUENCY (lambda) ===\n")
# =============================================================================
# pred_elnet_full = facility-year first-release probability from 01n

haz_path <- data_in("Data", "Analysis", "analysis_hazard_predictions_full.csv")  # local-first: avoids slow Z read on re-runs
haz <- fread(haz_path,
             select = c("panel_id", "panel_year", "state",
                        "pred_elnet_full", "has_single_walled"))
haz <- haz[state %in% FIG_STATES]
setnames(haz, "pred_elnet_full", "lambda")

stopifnot(all(haz$lambda >= 0 & haz$lambda <= 1))
stopifnot(!anyNA(haz$lambda))
stopifnot("Not all figure states present in hazard file" = all(FIG_STATES %in% unique(haz$state)))

cat("N by state:\n")
print(haz[, .N, by = state][order(state)])


# =============================================================================
cat("=== STEP 4: SNAPSHOT + FAIR PREMIUM ===\n")
# =============================================================================
# One row per facility: latest active panel year <= 2020.

snap <- haz[panel_year <= 2020][, .SD[which.max(panel_year)], by = panel_id]
cat(sprintf("Snapshot: %d facilities, %d states\n",
            nrow(snap), uniqueN(snap$state)))

# Wall (for output CSV)
snap[, wall := fifelse(has_single_walled == 1L, "single", "double")]

# Eq.4 — fair premium: flat pooled severity for all states (LA borrows S_bar)
snap[, S  := S_bar]
snap[, PP := lambda * S]

stopifnot(all(is.finite(snap$PP)))
stopifnot(all(snap$PP > 0))
stopifnot(uniqueN(snap$panel_id) == nrow(snap))
cat("Step 4 assertions PASS\n")

cat("PP summary by state:\n")
print(snap[, .(N    = .N,
               mean = round(mean(PP), 2),
               p10  = round(quantile(PP, 0.10), 2),
               p50  = round(quantile(PP, 0.50), 2),
               p90  = round(quantile(PP, 0.90), 2)),
           by = state][order(state)])


# =============================================================================
cat("=== STEP 5: BREAK-EVEN FEE, TRANSFERS, DECILES ===\n")
# =============================================================================

snap[, tau_s    := mean(PP), by = state]
snap[, transfer := PP - tau_s]
snap[, decile   := cut(frank(PP, ties.method = "first"), 10, labels = FALSE),
     by = state]

# Verify Sigma(transfer) == 0 within state to 1e-6 (exact by construction;
# floating-point rounding can make sum non-zero)
bal <- snap[, .(rel_err = abs(sum(transfer)) / sum(PP)), by = state]
stopifnot(all(bal$rel_err < 1e-6))
cat(sprintf("Transfer balance PASS (max rel err: %.2e)\n", max(bal$rel_err)))

# State summary (Eq.6)
state_sum <- snap[, {
  tau <- mean(PP)
  pos <- pmax(PP - tau, 0)
  .(N                 = .N,
    tau_breakeven     = tau,
    total_transfer    = sum(pos),
    transfer_pct_prem = sum(pos) / sum(PP),
    crossing_pct      = mean(PP <= tau),
    share_subsidized  = mean(PP > tau))
}, by = state]

# Pooled row: ALL facilities vs ONE grand-mean break-even fee (single-fund view,
# consistent with the figure). The within-state total = sum of the per-state rows.
grand_tau <- mean(snap$PP)
pool_pos  <- sum(pmax(snap$PP - grand_tau, 0))
pool_row <- data.table(
  state             = "POOLED",
  N                 = nrow(snap),
  tau_breakeven     = grand_tau,
  total_transfer    = pool_pos,
  transfer_pct_prem = pool_pos / sum(snap$PP),
  crossing_pct      = mean(snap$PP <= grand_tau),
  share_subsidized  = mean(snap$PP > grand_tau)
)
state_sum <- rbind(state_sum, pool_row)

fwrite(state_sum, file.path(OUTPUT_TABLES, "cross_subsidy_state_summary.csv"))
cat("Written: Output/Tables/cross_subsidy_state_summary.csv\n")

# Facility-level output (age_bins not in retrimmed flow; omitted)
fac_dt <- snap[, .(panel_id, state, panel_year, wall,
                   lambda, S, PP, tau_s, transfer, decile)]
fwrite(fac_dt, file.path(OUTPUT_DATA, "cross_subsidy_facility.csv"))
cat("Written: Data/Analysis/cross_subsidy_facility.csv\n")


# =============================================================================
cat("=== STEP 6: FIGURE — Pooled transfer curve ===\n")
# =============================================================================
# Grand-mean tau as the single flat-fee reference for the pooled figure.
# Per-state tau_s values are in cross_subsidy_state_summary.csv.

grand_tau <- mean(snap$PP)
cat(sprintf("Grand tau (pooled figure reference): %.2f 2023-USD\n", grand_tau))

# Sort by PP; centred percentile rank
plot_dt <- copy(snap)[order(PP)]
N_plot  <- nrow(plot_dt)
plot_dt[, pct := (seq_len(N_plot) - 0.5) / N_plot * 100]

# Annotation: pooled cross-subsidy stats
pr    <- state_sum[state == "POOLED"]
ann   <- sprintf(
  "Crossing: %.1f%%\nTransfer: %s\nTransfer / premiums: %.1f%%",
  pr$crossing_pct * 100,
  dollar(pr$total_transfer, accuracy = 1000, big.mark = ","),
  pr$transfer_pct_prem * 100
)

p_main <- ggplot(plot_dt, aes(x = pct, y = PP)) +
  # Blue fill: facilities that overpay the flat fee (PP < tau)
  geom_ribbon(
    data = plot_dt[PP <= grand_tau],
    aes(ymin = PP, ymax = grand_tau),
    fill = "#E6F1FB"
  ) +
  # Terracotta fill: facilities that underpay (PP > tau)
  geom_ribbon(
    data = plot_dt[PP >= grand_tau],
    aes(ymin = grand_tau, ymax = PP),
    fill = "#FAECE7"
  ) +
  geom_line(color = "#1a1a1a", linewidth = 0.55) +
  geom_hline(yintercept = grand_tau, linetype = "dashed",
             color = "#555555", linewidth = 0.45) +
  annotate("text",
           x = 3, y = max(plot_dt$PP) * 0.90,
           label = ann, hjust = 0, vjust = 1,
           size = 3.1, color = "#333333", lineheight = 1.35) +
  scale_x_continuous(
    "Facility percentile (ranked by PP, lowest to highest)",
    labels = function(x) paste0(x, "%"),
    expand = expansion(mult = c(0.01, 0.02))
  ) +
  scale_y_continuous(
    "Actuarially fair premium PP (2023 USD / yr)",
    labels = dollar_format(accuracy = 1, big.mark = ",")
  ) +
  labs(
    title    = "Cross-subsidy under a flat-fee trust fund",
    subtitle = sprintf(
      "Break-even flat fee τ = %s/yr (pooled mean PP). Blue: overpays (PP < τ). Terracotta: underpays (PP > τ). States: CO, LA, NM, PA, TN.",
      dollar(grand_tau, accuracy = 1)
    ),
    caption  = paste0(
      "λ = facility-year first-release hazard (01n elastic-net model). ",
      "Ś̅ = pooled mean severity net of deductible, 2023 USD (claims: CO, NM, PA, TN; LA excluded—facility totals). ",
      "PP = λ·Ś̅. Snapshot = latest active panel year ≤ 2020."
    )
  ) +
  theme_pub()

save_fig(p_main, "Fig_CrossSub_TransferCurve_Panel", w = 8, h = 5)


# =============================================================================
cat("=== STEP 9: SUMMARY ===\n")
# =============================================================================
cat(sprintf("USED_SEVERITY: flat  |  S_bar: %.2f 2023-USD\n", S_bar))
cat("\nState summary:\n")
print(state_sum[, .(
  state,
  N,
  tau           = round(tau_breakeven, 2),
  total_xfer    = round(total_transfer, 0),
  pct_prem      = round(transfer_pct_prem * 100, 2),
  crossing_pct  = round(crossing_pct * 100, 2)
)])

cat("\nDone.\n")
