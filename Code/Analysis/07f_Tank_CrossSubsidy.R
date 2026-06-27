################################################################################
# 07f_Tank_CrossSubsidy.R  —  PERC M1, TANK LEVEL: fair premium vs uniform premium
#
# Rebuild of the cross-subsidy picture at the TANK level (rates are priced per
# tank by age x wall, so facility-level risk is the wrong unit). One row per
# tank-year:
#   fair premium_t  = lambda_hat(age_bin, wall) * S_bar      (expected loss / tank-yr)
#   uniform premium  = mean fair premium over the tanks shown  (the break-even flat
#                      charge a single-rate fund would levy; "tau")
#   per-tank fee     = fr_premium_per_tank_yr                  (what the fund charges)
#
# We show who pays more vs less than the uniform premium (NOT "subsidy" language),
# and — the first-order point — how far the actual fee sits below fair cost.
#
# A confidence band on the fair-premium line propagates uncertainty in the
# severity estimate S_bar (nonparametric bootstrap of the claim rows).
#
# HAZARD SOURCE: cell mean of the 01n facility predictions, exactly the object the
#   structural model consumes (04b_Replacement_Panel_Prep.R:217). One swappable
#   block (STEP 2) — repoint HAZ_FILE to the de-confounded pricing hazard when it
#   lands without touching anything else.
#
# INPUTS (z_path / data_in):
#   Data/Processed/incident_level_claims.csv              severity pool (CO,NM,PA,TN)
#   Data/Analysis/analysis_hazard_predictions_full.csv    lambda cell hazard (01n)
#   Data/Analysis/panel_dt.csv                            tank-year spine
#   Data/Analysis/facility_panel.csv                      fr_premium_per_tank_yr
#
# OUTPUTS (here):
#   Output/Tables/tank_crosssub_cell_hazard.csv
#   Output/Tables/tank_crosssub_state_summary.csv
#   Data/Analysis/tank_crosssub_tankyears.csv
#   Output/Figures/Fig_TankCrossSub_Single_<ST>.{pdf,png}     single-year, per state
#   Output/Figures/Fig_TankCrossSub_Pooled_Panel.{pdf,png}    pooled all years, faceted
#   Output/Figures/Fig_TankPaidShare_Pooled_Panel.{pdf,png}   fee vs fair (price-too-low)
#
# TICKET: 033 (tank-level cross-subsidy rebuild) | Attempt: 1 | 2026-06-26
################################################################################

suppressPackageStartupMessages({
  library(here)
  library(data.table)
  library(ggplot2)
  library(scales)
})

source(here::here("Code", "Helpers", "data_paths.R"))

options(scipen = 999)
set.seed(20260626L)

# === LOGGING (hazard + tank reads can exceed 1 min) ==========================
.script_name <- "07f_Tank_CrossSubsidy"
.log_path <- here("logs", paste0(.script_name, "_",
                                 format(Sys.time(), "%Y%m%d_%H%M%S"), ".log"))
dir.create(dirname(.log_path), recursive = TRUE, showWarnings = FALSE)
.log <- file(.log_path, open = "wt")
sink(.log, type = "output"); sink(.log, type = "message", append = TRUE)
on.exit({ sink(type = "output"); sink(type = "message"); close(.log) }, add = TRUE)
cat(sprintf("LOG START %s\nScript: %s\nR: %s\nWD: %s\n\n",
            .log_path, .script_name, R.version.string, getwd()))

# ── Output dirs ──────────────────────────────────────────────────────────────
OUTPUT_FIGURES <- here("Output", "Figures")
OUTPUT_TABLES  <- here("Output", "Tables")
OUTPUT_DATA    <- here("Data", "Analysis")
for (d in c(OUTPUT_FIGURES, OUTPUT_TABLES, OUTPUT_DATA))
  dir.create(d, recursive = TRUE, showWarnings = FALSE)

# ── Constants ────────────────────────────────────────────────────────────────
FIG_STATES <- c("CO", "LA", "NM", "TN")        # tank figure states
SEV_STATES <- c("CO", "NM", "PA", "TN")        # severity pool (LA excluded: fac totals)
D_NM       <- 10000                            # NM is GROSS; net = max(C-D,0)
SNAP_YEAR  <- 2005L                            # single-year cross-section
N_BOOT     <- 1000L                            # severity bootstrap replicates
HAZARD_SCOPE <- "national"                     # "national" (one rate card) or "state"
HAZ_FILE   <- "analysis_hazard_predictions_full.csv"   # <- repoint when de-confounded hazard lands

# 01n canonical age bins (must match the hazard file's age_bin labels exactly)
AGE_BIN_BREAKS <- c(0, 3, 6, 9, 12, 15, 18, 21, 24, Inf)
AGE_BIN_LABELS <- c("0-2", "3-5", "6-8", "9-11", "12-14",
                    "15-17", "18-20", "21-23", "24+")

# Colours — over/under the uniform premium, plus the fee line
COL_UNDER <- "#FAECE7"   # pays MORE than uniform premium (fair > tau)
COL_OVER  <- "#E6F1FB"   # pays LESS than uniform premium (fair < tau)
COL_LINE  <- "#1a1a1a"
COL_BAND  <- "#9ecae1"   # severity CI band on the fair-premium line
COL_TAU   <- "#555555"
COL_FEE   <- "#2B2B2B"

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
      strip.text       = element_text(face = "bold"),
      plot.background   = element_rect(fill = "white", color = NA)
    )
}

# Lock-resilient save: PNG always; PDF wrapped so an open viewer can't halt the run.
save_fig <- function(p, name, w = 8, h = 5) {
  ggsave(file.path(OUTPUT_FIGURES, paste0(name, ".png")),
         p, width = w, height = h, dpi = 300, bg = "white")
  pdf_path <- file.path(OUTPUT_FIGURES, paste0(name, ".pdf"))
  tryCatch(
    ggsave(pdf_path, p, width = w, height = h, device = grDevices::cairo_pdf),
    error = function(e)
      cat(sprintf("  NOTE: PDF not written (%s): %s\n", basename(pdf_path),
                  conditionMessage(e)))
  )
  cat(sprintf("  Saved: %s (.png%s)\n", name,
              if (file.exists(pdf_path)) " + .pdf" else ""))
}


# =============================================================================
cat("=== STEP 1: SEVERITY S_bar + BOOTSTRAP CI ===\n")
# =============================================================================
# Fund-payable severity net of deductible, 2023 USD. Net rule (verified
# 2026-06-25): CO/TN/PA recorded cost is already (C-D)+; NM is gross -> net it.
# LA excluded (facility totals). Sub-deductible zeros kept.

inc <- fread(z_path("Data", "Processed", "incident_level_claims.csv"),
             select = c("panel_id", "state", "total_cost", "total_cost_2023"))
inc <- inc[state %in% SEV_STATES]
inc <- inc[!is.na(total_cost) & !is.na(total_cost_2023) &
           total_cost >= 0 & total_cost_2023 >= 0]

inc[, net_nom  := fifelse(state == "NM", pmax(total_cost - D_NM, 0), total_cost)]
inc[, net_2023 := fifelse(net_nom == 0, 0, net_nom * (total_cost_2023 / total_cost))]

stopifnot(all(is.finite(inc$net_2023)), all(inc$net_2023 >= 0), !anyNA(inc$net_2023))

S_bar <- inc[, mean(net_2023)]

# Nonparametric bootstrap of the claim rows -> S_bar sampling distribution.
n_inc  <- nrow(inc)
net_v  <- inc$net_2023
boot_S <- vapply(seq_len(N_BOOT),
                 function(b) mean(net_v[sample.int(n_inc, n_inc, replace = TRUE)]),
                 numeric(1))
S_lo <- as.numeric(quantile(boot_S, 0.025))
S_hi <- as.numeric(quantile(boot_S, 0.975))

cat(sprintf("Severity rows: %d (states %s)\n", n_inc, paste(SEV_STATES, collapse = ",")))
cat(sprintf("S_bar = %s  [95%% CI %s, %s]  (B=%d)\n",
            dollar(S_bar, accuracy = 1), dollar(S_lo, accuracy = 1),
            dollar(S_hi, accuracy = 1), N_BOOT))


# =============================================================================
cat("=== STEP 2: CELL HAZARD lambda_hat(age_bin, wall) ===\n")
# =============================================================================
# Cell mean of the 01n facility predictions — the same aggregation the DCM uses
# (04b:217). national: one rate-card schedule over all states; state: own state.
# Each TANK is later assigned its OWN cell's hazard, so wall/age vary per tank.

haz <- fread(data_in("Data", "Analysis", HAZ_FILE),
             select = c("state", "has_single_walled", "age_bin", "pred_elnet_full"))
stopifnot(all(haz$has_single_walled %in% c(0L, 1L)),
          !anyNA(haz$pred_elnet_full),
          all(haz$age_bin %in% AGE_BIN_LABELS))

by_keys <- if (HAZARD_SCOPE == "state")
  c("state", "age_bin", "has_single_walled") else c("age_bin", "has_single_walled")
cell <- haz[, .(lambda = mean(pred_elnet_full), n_cell = .N), by = by_keys]

# Expect a full grid: 9 age x 2 wall (x states if state-scoped)
exp_cells <- if (HAZARD_SCOPE == "state")
  length(AGE_BIN_LABELS) * 2L * uniqueN(haz$state) else length(AGE_BIN_LABELS) * 2L
if (nrow(cell) != exp_cells)
  cat(sprintf("  NOTE: %d cells present, %d expected (thin/absent cells)\n",
              nrow(cell), exp_cells))

fwrite(cell, file.path(OUTPUT_TABLES, "tank_crosssub_cell_hazard.csv"))
cat(sprintf("Cell hazard: scope=%s, %d cells. Saved tank_crosssub_cell_hazard.csv\n",
            HAZARD_SCOPE, nrow(cell)))
cat("Cell hazard SW vs DW by age (per 1000 tank-yr, national pool):\n")
print(dcast(haz[, .(lambda = mean(pred_elnet_full)), by = .(age_bin, has_single_walled)],
            age_bin ~ has_single_walled, value.var = "lambda")[
            match(AGE_BIN_LABELS, age_bin)][
            , .(age_bin, DW = round(`0` * 1000, 2), SW = round(`1` * 1000, 2))])


# =============================================================================
cat("=== STEP 3: TANK PANEL -> fair premium ===\n")
# =============================================================================
p <- fread(data_in("Data", "Analysis", "panel_dt.csv"),
           select = c("tank_panel_id", "panel_id", "state",
                      "tank_age", "mm_wall", "panel_year"))
p <- p[state %in% FIG_STATES & mm_wall %in% c("Single-Walled", "Double-Walled")]
p[, has_single_walled := fifelse(mm_wall == "Single-Walled", 1L, 0L)]
p[, age_bin := as.character(cut(tank_age, breaks = AGE_BIN_BREAKS,
                                labels = AGE_BIN_LABELS, right = FALSE,
                                include.lowest = TRUE))]
stopifnot(!anyNA(p$age_bin))

p <- merge(p, cell[, c(by_keys, "lambda"), with = FALSE], by = by_keys, all.x = TRUE)
n_nolam <- p[is.na(lambda), .N]
if (n_nolam > 0) {
  cat(sprintf("  WARNING: %d tank-years with no cell hazard match — dropping\n", n_nolam))
  p <- p[!is.na(lambda)]
}

p[, `:=`(fair_premium = lambda * S_bar,
         fair_lo      = lambda * S_lo,
         fair_hi      = lambda * S_hi)]
stopifnot(all(is.finite(p$fair_premium)), all(p$fair_premium > 0))
cat(sprintf("Tank-years: %s across %d states\n",
            format(nrow(p), big.mark = ","), uniqueN(p$state)))


# =============================================================================
cat("=== STEP 4: PER-TANK FEE ===\n")
# =============================================================================
# fr_premium_per_tank_yr is already per-tank: each tank inherits its facility's
# per-tank fee. NM has no per-tank fee (gas-tax financed) -> 0.
fp <- fread(data_in("Data", "Analysis", "facility_panel.csv"),
            select = c("panel_id", "panel_year", "fr_premium_per_tank_yr"))
p <- merge(p, fp, by = c("panel_id", "panel_year"), all.x = TRUE)
setnames(p, "fr_premium_per_tank_yr", "fee_tank")
p[is.na(fee_tank), fee_tank := 0]
cat("Median per-tank fee by state (0 = gas-tax funded):\n")
print(p[, .(med_fee = round(median(fee_tank), 1),
            med_fair = round(median(fair_premium), 0)), by = state][order(state)])


# =============================================================================
cat("=== STEP 5: UNIFORM PREMIUM tau + SUMMARY ===\n")
# =============================================================================
# tau = break-even uniform premium = mean fair premium over the tanks shown.
state_summary <- function(dt) dt[, {
  tau <- mean(fair_premium)
  .(N            = .N,
    uniform_prem = tau,
    share_under  = mean(fair_premium <  tau),    # pay LESS than uniform premium
    share_over   = mean(fair_premium >  tau),    # pay MORE than uniform premium
    spread_pct   = sum(pmax(fair_premium - tau, 0)) / sum(fair_premium),  # gap as % of premiums
    fee_pct_fair = sum(fee_tank) / sum(fair_premium))                     # fee as % of fair cost
}, by = state]

summ_pool <- state_summary(p)            # pooled all years
summ_snap <- state_summary(p[panel_year == SNAP_YEAR])
summ_pool[, scope := "pooled_all_years"]
summ_snap[, scope := paste0("single_", SNAP_YEAR)]
summ <- rbind(summ_snap, summ_pool)
fwrite(summ, file.path(OUTPUT_TABLES, "tank_crosssub_state_summary.csv"))
cat("State summary (pooled all years):\n")
print(summ_pool[, .(state, N, uniform_prem = round(uniform_prem, 0),
                    share_under = round(share_under, 3),
                    spread_pct = round(spread_pct, 3),
                    fee_pct_fair = round(fee_pct_fair, 3))])

fwrite(p[, .(tank_panel_id, panel_id, state, panel_year, tank_age, mm_wall,
             age_bin, lambda, fair_premium, fair_lo, fair_hi, fee_tank)],
       file.path(OUTPUT_DATA, "tank_crosssub_tankyears.csv"))
cat("Saved: Data/Analysis/tank_crosssub_tankyears.csv\n")


# =============================================================================
cat("=== STEP 6: FIGURES ===\n")
# =============================================================================
# Rank tanks low->high by fair premium; x = percentile. Area splits at the
# uniform premium tau. CI band = severity uncertainty on the fair-premium line.

rank_dt <- function(dt) {
  d <- copy(dt)[order(fair_premium)]
  d[, pct := (seq_len(.N) - 0.5) / .N * 100]
  d[]
}

# ── Fig A: single-year, one panel per state ─────────────────────────────────
build_single <- function(st) {
  d   <- rank_dt(p[state == st & panel_year == SNAP_YEAR])
  tau <- mean(d$fair_premium)
  s   <- summ_snap[state == st]
  ggplot(d, aes(pct, fair_premium)) +
    geom_ribbon(data = d[fair_premium <= tau], aes(ymin = fair_premium, ymax = tau),
                fill = COL_OVER) +
    geom_ribbon(data = d[fair_premium >= tau], aes(ymin = tau, ymax = fair_premium),
                fill = COL_UNDER) +
    geom_ribbon(aes(ymin = fair_lo, ymax = fair_hi), fill = COL_BAND, alpha = 0.45) +
    geom_line(color = COL_LINE, linewidth = 0.55) +
    geom_hline(yintercept = tau, linetype = "dashed", color = COL_TAU, linewidth = 0.45) +
    scale_x_continuous("Tanks, ranked low to high fair premium (percentile)",
                       labels = function(x) paste0(x, "%")) +
    scale_y_continuous("Fair premium ($ / tank-year, 2023)",
                       labels = dollar_format(accuracy = 1)) +
    labs(title = sprintf("%s: fair premium vs uniform premium, %d (tank level)", st, SNAP_YEAR),
         subtitle = sprintf("Uniform premium tau = %s/tank-yr. %.0f%% of tanks pay more than tau under one flat charge. Band = severity 95%% CI.",
                            dollar(tau, accuracy = 1), s$share_over * 100),
         caption = "Fair premium = (age x wall cell hazard, 01n) x mean net-of-deductible severity. Dashed = break-even uniform premium.") +
    theme_pub()
}
for (st in FIG_STATES) save_fig(build_single(st), sprintf("Fig_TankCrossSub_Single_%s", st), 8, 5)

# ── Fig B: pooled all tank-years, faceted by state (the headline) ───────────
pool_rank <- rbindlist(lapply(FIG_STATES, function(st) {
  d <- rank_dt(p[state == st]); d[, state := st]; d[, tau := mean(fair_premium)]; d
}))
tau_lab <- pool_rank[, .(tau = first(tau), ymax = max(fair_premium)), by = state]
p_pool <- ggplot(pool_rank, aes(pct, fair_premium)) +
  geom_ribbon(data = pool_rank[fair_premium <= tau], aes(ymin = fair_premium, ymax = tau),
              fill = COL_OVER) +
  geom_ribbon(data = pool_rank[fair_premium >= tau], aes(ymin = tau, ymax = fair_premium),
              fill = COL_UNDER) +
  geom_ribbon(aes(ymin = fair_lo, ymax = fair_hi), fill = COL_BAND, alpha = 0.45) +
  geom_line(color = COL_LINE, linewidth = 0.5) +
  geom_hline(data = tau_lab, aes(yintercept = tau),
             linetype = "dashed", color = COL_TAU, linewidth = 0.45) +
  facet_wrap(~ state, scales = "free", nrow = 1) +
  scale_x_continuous("Tank-years, ranked low to high fair premium (percentile)",
                     labels = function(x) paste0(x, "%")) +
  scale_y_continuous("Fair premium ($ / tank-year, 2023)", labels = dollar_format(accuracy = 1)) +
  labs(title = "Fair premium vs uniform premium, tank level (all years pooled)",
       subtitle = "Each tank-year priced by its own age x wall cell. Dashed = break-even uniform premium tau. Band = severity 95% CI.",
       caption = "Fair premium = (age x wall cell hazard, 01n) x mean net-of-deductible severity. Blue: pays less than tau. Terracotta: pays more.") +
  theme_pub()
save_fig(p_pool, "Fig_TankCrossSub_Pooled_Panel", 12, 4)

# ── Fig C: fee vs fair premium (the first-order point — price too low) ───────
# Black step = actual per-tank fee; line = fair premium; gap = how far below fair.
paid_dt <- rbindlist(lapply(FIG_STATES, function(st) {
  d <- rank_dt(p[state == st]); d[, state := st]; d
}))
p_paid <- ggplot(paid_dt, aes(pct, fair_premium)) +
  geom_ribbon(aes(ymin = fee_tank, ymax = fair_premium), fill = COL_OVER) +
  geom_line(color = COL_LINE, linewidth = 0.5) +
  geom_line(aes(y = fee_tank), color = COL_FEE, linewidth = 0.5) +
  facet_wrap(~ state, scales = "free", nrow = 1) +
  scale_x_continuous("Tank-years, ranked low to high fair premium (percentile)",
                     labels = function(x) paste0(x, "%")) +
  scale_y_continuous("$ / tank-year (2023)", labels = dollar_format(accuracy = 1)) +
  labs(title = "What the fund charges vs the fair premium (tank level)",
       subtitle = "Black = actual per-tank fee. Line = fair premium. The gap is the shortfall funded by gas-tax revenue.",
       caption = "Fee = fr_premium_per_tank_yr (NM gas-tax financed = 0). Fair premium as above.") +
  theme_pub()
save_fig(p_paid, "Fig_TankPaidShare_Pooled_Panel", 12, 4)


# =============================================================================
cat("=== STEP 7: SUMMARY ===\n")
# =============================================================================
cat(sprintf("S_bar = %s [%s, %s] | HAZARD_SCOPE = %s | SNAP_YEAR = %d\n",
            dollar(S_bar, accuracy = 1), dollar(S_lo, accuracy = 1),
            dollar(S_hi, accuracy = 1), HAZARD_SCOPE, SNAP_YEAR))
cat("Pooled-all-years uniform premium + fee-as-%-of-fair by state:\n")
print(summ_pool[, .(state, N, uniform_prem = round(uniform_prem, 0),
                    fee_pct_fair = round(fee_pct_fair, 3))])
cat("\nDone.\n")
