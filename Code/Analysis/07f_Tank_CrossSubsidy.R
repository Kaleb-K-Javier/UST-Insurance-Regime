################################################################################
# 07f_Tank_CrossSubsidy.R  —  PERC M1, TANK LEVEL: fair premium vs uniform premium
#
# Rebuild of the cross-subsidy picture at the TANK level (rates are priced per
# tank by age x wall, so facility-level risk is the wrong unit). One row per
# tank-year:
#   fair premium_t  = lambda_hat(cell) * S_bar              (expected loss / tank-yr)
#   uniform premium  = mean fair premium over the tanks shown  (break-even flat charge, "tau")
#   per-tank fee     = fr_premium_per_tank_yr                  (what the fund charges)
#
# Uniform-premium framing (NO "subsidy" language): who pays more vs less than the
# uniform premium, and how far the actual fee sits below fair cost.
#
# HAZARD = the de-confounded per-tank cell schedule from 01p_Pricing_Hazard.R
#   (dcm_cell_hazard_pricing.csv): detection-era trained, calendar-controlled,
#   priced at a fixed reference year + single-tank exposure. Age is the clean risk
#   dimension; wall is detection-confounded (DW>=SW is monitoring, not risk), so we
#   build BOTH versions and let the researcher choose:
#     WALL_MODES = "agewall" (per-tank by age x wall) and "ageonly" (wall collapsed).
#
# CI band = severity uncertainty (nonparametric bootstrap of the claim rows -> S_bar).
#
# INPUTS (z_path / data_in):
#   Data/Processed/incident_level_claims.csv     severity pool (CO,NM,PA,TN)
#   Data/Analysis/dcm_cell_hazard_pricing.csv    per-tank cell schedule (01p)
#   Data/Analysis/panel_dt.csv                   tank-year spine
#   Data/Analysis/facility_panel.csv             fr_premium_per_tank_yr
#
# OUTPUTS (here): per WALL_MODE m in {AgeWall, AgeOnly}
#   Output/Tables/tank_crosssub_state_summary.csv               (both modes stacked)
#   Data/Analysis/tank_crosssub_tankyears.csv                   (both fair cols)
#   Output/Figures/Fig_TankCrossSub_Single_<ST>_<m>.{pdf,png}   single-year, per state
#   Output/Figures/Fig_TankCrossSub_Pooled_Panel_<m>.{pdf,png}  pooled all years, faceted
#   Output/Figures/Fig_TankPaidShare_Pooled_<m>.{pdf,png}       fee vs fair (price-too-low)
#
# TICKET: 033 (tank-level cross-subsidy rebuild) | Attempt: 2 | 2026-06-26
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

# === LOGGING =================================================================
.script_name <- "07f_Tank_CrossSubsidy"
.log_path <- here("logs", paste0(.script_name, "_",
                                 format(Sys.time(), "%Y%m%d_%H%M%S"), ".log"))
dir.create(dirname(.log_path), recursive = TRUE, showWarnings = FALSE)
.log <- file(.log_path, open = "wt")
sink(.log, type = "output"); sink(.log, type = "message", append = TRUE)
on.exit({ sink(type = "output"); sink(type = "message"); close(.log) }, add = TRUE)
cat(sprintf("LOG START %s\nScript: %s\nR: %s\nWD: %s\n\n",
            .log_path, .script_name, R.version.string, getwd()))

OUTPUT_FIGURES <- here("Output", "Figures")
OUTPUT_TABLES  <- here("Output", "Tables")
OUTPUT_DATA    <- here("Data", "Analysis")
for (d in c(OUTPUT_FIGURES, OUTPUT_TABLES, OUTPUT_DATA))
  dir.create(d, recursive = TRUE, showWarnings = FALSE)

# ── Constants ────────────────────────────────────────────────────────────────
FIG_STATES   <- c("CO", "LA", "NM", "TN")
SEV_STATES   <- c("CO", "NM", "PA", "TN")        # LA excluded (facility totals)
D_NM         <- 10000
SNAP_YEAR    <- 2005L
N_BOOT       <- 1000L
HAZARD_SCOPE <- "national"                       # "national" (one rate card) or "state"
HAZ_CELL     <- "dcm_cell_hazard_pricing.csv"    # 01p de-confounded schedule
WALL_MODES   <- c(agewall = "AgeWall", ageonly = "AgeOnly")

AGE_BIN_LABELS <- c("0-2","3-5","6-8","9-11","12-14","15-17","18-20","21-23","24+")
AGE_BIN_BREAKS <- c(0, 3, 6, 9, 12, 15, 18, 21, 24, Inf)

COL_UNDER <- "#FAECE7"; COL_OVER <- "#E6F1FB"
COL_LINE  <- "#1a1a1a"; COL_BAND <- "#9ecae1"; COL_TAU <- "#555555"; COL_FEE <- "#2B2B2B"

theme_pub <- function(base_size = 11) {
  theme_minimal(base_size = base_size) +
    theme(plot.title    = element_text(face = "bold", size = rel(1.05), margin = margin(0,0,6,0)),
          plot.subtitle = element_text(color = "grey40", size = rel(0.83), margin = margin(0,0,8,0)),
          plot.caption  = element_text(color = "grey50", size = rel(0.70), hjust = 0, margin = margin(8,0,0,0)),
          axis.title    = element_text(face = "bold", size = rel(0.9)),
          panel.grid.minor = element_blank(), strip.text = element_text(face = "bold"),
          plot.background  = element_rect(fill = "white", color = NA))
}
save_fig <- function(p, name, w = 8, h = 5) {
  ggsave(file.path(OUTPUT_FIGURES, paste0(name, ".png")), p, width = w, height = h, dpi = 300, bg = "white")
  pdf_path <- file.path(OUTPUT_FIGURES, paste0(name, ".pdf"))
  tryCatch(ggsave(pdf_path, p, width = w, height = h, device = grDevices::cairo_pdf),
           error = function(e) cat(sprintf("  NOTE: PDF skipped (%s): %s\n",
                                           basename(pdf_path), conditionMessage(e))))
  cat(sprintf("  Saved: %s\n", name))
}


# =============================================================================
cat("=== STEP 1: SEVERITY S_bar + BOOTSTRAP CI ===\n")
# =============================================================================
inc <- fread(z_path("Data", "Processed", "incident_level_claims.csv"),
             select = c("panel_id", "state", "total_cost", "total_cost_2023"))
inc <- inc[state %in% SEV_STATES]
inc <- inc[!is.na(total_cost) & !is.na(total_cost_2023) &
           total_cost >= 0 & total_cost_2023 >= 0]
inc[, net_nom  := fifelse(state == "NM", pmax(total_cost - D_NM, 0), total_cost)]
inc[, net_2023 := fifelse(net_nom == 0, 0, net_nom * (total_cost_2023 / total_cost))]
stopifnot(all(is.finite(inc$net_2023)), all(inc$net_2023 >= 0), !anyNA(inc$net_2023))

S_bar  <- inc[, mean(net_2023)]
net_v  <- inc$net_2023; n_inc <- length(net_v)
boot_S <- vapply(seq_len(N_BOOT),
                 function(b) mean(net_v[sample.int(n_inc, n_inc, replace = TRUE)]), numeric(1))
S_lo <- as.numeric(quantile(boot_S, 0.025)); S_hi <- as.numeric(quantile(boot_S, 0.975))
cat(sprintf("S_bar = %s  [95%% CI %s, %s]  (rows=%d, B=%d)\n",
            dollar(S_bar, accuracy = 1), dollar(S_lo, accuracy = 1),
            dollar(S_hi, accuracy = 1), n_inc, N_BOOT))


# =============================================================================
cat("=== STEP 2: PER-TANK CELL SCHEDULE (from 01p) ===\n")
# =============================================================================
cell_path <- data_in("Data", "Analysis", HAZ_CELL)
if (!file.exists(cell_path))
  stop("Cell schedule not found: ", cell_path,
       "\n  Run 01p_Pricing_Hazard.R first (writes dcm_cell_hazard_pricing.csv).")
sched <- fread(cell_path)   # cols: state, age_bin, has_single_walled, lambda
stopifnot(all(c("state","age_bin","has_single_walled","lambda") %in% names(sched)))

sel_state <- if (HAZARD_SCOPE == "national") "NATIONAL" else NA
if (HAZARD_SCOPE == "national") {
  base <- sched[state == "NATIONAL"]
  if (nrow(base) == 0L) stop("No NATIONAL rows in schedule; re-run 01p or set HAZARD_SCOPE='state'.")
}

# age x wall lookup (national) and age-only lookup (wall collapsed = mean over wall)
cell_aw  <- (if (HAZARD_SCOPE == "national") base else sched
            )[, .(age_bin, has_single_walled, lambda)]
cell_age <- cell_aw[, .(lambda = mean(lambda)), by = age_bin]
cat("age x wall schedule (per 1000):\n")
print(dcast(cell_aw, age_bin ~ has_single_walled, value.var = "lambda")[
        match(AGE_BIN_LABELS, age_bin)][
        , .(age_bin, DW = round(`0`*1000,2), SW = round(`1`*1000,2))])


# =============================================================================
cat("=== STEP 3: TANK PANEL -> fair premium (both wall modes) ===\n")
# =============================================================================
p <- fread(data_in("Data", "Analysis", "panel_dt.csv"),
           select = c("tank_panel_id", "panel_id", "state", "tank_age", "mm_wall", "panel_year"))
p <- p[state %in% FIG_STATES & mm_wall %in% c("Single-Walled", "Double-Walled")]
# panel_dt is the tank spine; some figure states have no tank-level rows (e.g. NM is
# facility-level-only). Drop absent states so we don't emit empty per-state figures.
.absent <- setdiff(FIG_STATES, unique(p$state))
if (length(.absent))
  cat(sprintf("  NOTE: no tank-level rows for %s (absent from panel_dt) — excluded\n",
              paste(.absent, collapse = ", ")))
FIG_STATES <- intersect(FIG_STATES, unique(p$state))
p[, has_single_walled := fifelse(mm_wall == "Single-Walled", 1L, 0L)]
p[, age_bin := as.character(cut(tank_age, AGE_BIN_BREAKS, AGE_BIN_LABELS,
                                right = FALSE, include.lowest = TRUE))]
stopifnot(!anyNA(p$age_bin))

p <- merge(p, cell_aw,  by = c("age_bin","has_single_walled"), all.x = TRUE)
setnames(p, "lambda", "lambda_agewall")
p <- merge(p, cell_age, by = "age_bin", all.x = TRUE)
setnames(p, "lambda", "lambda_ageonly")
if (p[is.na(lambda_agewall) | is.na(lambda_ageonly), .N] > 0)
  stop("Tank-years with no cell-hazard match — schedule grid incomplete.")

for (m in names(WALL_MODES)) {
  lam <- paste0("lambda_", m)
  p[, paste0("fair_", m)    := get(lam) * S_bar]
  p[, paste0("fairlo_", m)  := get(lam) * S_lo]
  p[, paste0("fairhi_", m)  := get(lam) * S_hi]
}
cat(sprintf("Tank-years: %s | states %s\n", format(nrow(p), big.mark=","),
            paste(sort(unique(p$state)), collapse=",")))


# =============================================================================
cat("=== STEP 4: PER-TANK FEE ===\n")
# =============================================================================
fp <- fread(data_in("Data", "Analysis", "facility_panel.csv"),
            select = c("panel_id", "panel_year", "fr_premium_per_tank_yr"))
p <- merge(p, fp, by = c("panel_id","panel_year"), all.x = TRUE)
setnames(p, "fr_premium_per_tank_yr", "fee_tank")
p[is.na(fee_tank), fee_tank := 0]


# =============================================================================
cat("=== STEP 5: SUMMARY (per wall mode) ===\n")
# =============================================================================
state_summary <- function(dt, fc) dt[, {
  tau <- mean(get(fc))
  .(N = .N, uniform_prem = tau,
    share_over   = mean(get(fc) > tau),
    spread_pct   = sum(pmax(get(fc) - tau, 0)) / sum(get(fc)),
    fee_pct_fair = sum(fee_tank) / sum(get(fc)))
}, by = state]

summ <- rbindlist(lapply(names(WALL_MODES), function(m) {
  fc <- paste0("fair_", m)
  rbind(cbind(scope = paste0("single_", SNAP_YEAR), wall_mode = WALL_MODES[m],
              state_summary(p[panel_year == SNAP_YEAR], fc)),
        cbind(scope = "pooled_all_years",            wall_mode = WALL_MODES[m],
              state_summary(p, fc)))
}))
fwrite(summ, file.path(OUTPUT_TABLES, "tank_crosssub_state_summary.csv"))
cat("Pooled-all-years summary by mode:\n")
print(summ[scope == "pooled_all_years",
           .(wall_mode, state, N, uniform_prem = round(uniform_prem,0),
             fee_pct_fair = round(fee_pct_fair,3))])

fwrite(p[, .(tank_panel_id, panel_id, state, panel_year, tank_age, mm_wall, age_bin,
             lambda_agewall, lambda_ageonly, fair_agewall, fair_ageonly, fee_tank)],
       file.path(OUTPUT_DATA, "tank_crosssub_tankyears.csv"))
cat("Saved: Data/Analysis/tank_crosssub_tankyears.csv\n")


# =============================================================================
cat("=== STEP 6: FIGURES (both wall modes) ===\n")
# =============================================================================
rank_dt <- function(dt, fc) {
  d <- copy(dt)[order(get(fc))]; d[, pct := (seq_len(.N) - 0.5)/.N*100]; d[]
}

for (m in names(WALL_MODES)) {
  tag <- WALL_MODES[m]
  fc <- paste0("fair_", m); lo <- paste0("fairlo_", m); hi <- paste0("fairhi_", m)
  cat(sprintf("-- wall mode: %s --\n", tag))

  # Fig A: single-year, one panel per state
  for (st in FIG_STATES) {
    d   <- rank_dt(p[state == st & panel_year == SNAP_YEAR], fc)
    tau <- mean(d[[fc]])
    pA <- ggplot(d, aes(pct, .data[[fc]])) +
      geom_ribbon(data = d[get(fc) <= tau], aes(ymin = .data[[fc]], ymax = tau), fill = COL_OVER) +
      geom_ribbon(data = d[get(fc) >= tau], aes(ymin = tau, ymax = .data[[fc]]), fill = COL_UNDER) +
      geom_ribbon(aes(ymin = .data[[lo]], ymax = .data[[hi]]), fill = COL_BAND, alpha = 0.45) +
      geom_line(color = COL_LINE, linewidth = 0.55) +
      geom_hline(yintercept = tau, linetype = "dashed", color = COL_TAU, linewidth = 0.45) +
      scale_x_continuous("Tanks, ranked low to high fair premium (percentile)",
                         labels = function(x) paste0(x,"%")) +
      scale_y_continuous("Fair premium ($ / tank-year, 2023)", labels = dollar_format(accuracy = 1)) +
      labs(title = sprintf("%s: fair premium vs uniform premium, %d", st, SNAP_YEAR),
           subtitle = sprintf("Uniform premium tau = %s/tank-yr. Band = severity 95%% CI. [%s]",
                              dollar(tau, accuracy = 1), tag),
           caption = "Fair premium = per-tank cell hazard (01p, detection-de-confounded) x mean net-of-deductible severity.") +
      theme_pub()
    save_fig(pA, sprintf("Fig_TankCrossSub_Single_%s_%s", st, tag), 8, 5)
  }

  # Fig B: pooled all tank-years, faceted by state (headline)
  pool <- rbindlist(lapply(FIG_STATES, function(st) {
    d <- rank_dt(p[state == st], fc); d[, state := st]; d[, tau := mean(get(fc))]; d
  }))
  tau_lab <- pool[, .(tau = first(tau)), by = state]
  pB <- ggplot(pool, aes(pct, .data[[fc]])) +
    geom_ribbon(data = pool[get(fc) <= tau], aes(ymin = .data[[fc]], ymax = tau), fill = COL_OVER) +
    geom_ribbon(data = pool[get(fc) >= tau], aes(ymin = tau, ymax = .data[[fc]]), fill = COL_UNDER) +
    geom_ribbon(aes(ymin = .data[[lo]], ymax = .data[[hi]]), fill = COL_BAND, alpha = 0.45) +
    geom_line(color = COL_LINE, linewidth = 0.5) +
    geom_hline(data = tau_lab, aes(yintercept = tau), linetype = "dashed",
               color = COL_TAU, linewidth = 0.45) +
    facet_wrap(~ state, scales = "free", nrow = 1) +
    scale_x_continuous("Tank-years, ranked low to high fair premium (percentile)",
                       labels = function(x) paste0(x,"%")) +
    scale_y_continuous("Fair premium ($ / tank-year, 2023)", labels = dollar_format(accuracy = 1)) +
    labs(title = sprintf("Fair premium vs uniform premium, tank level (all years pooled) [%s]", tag),
         subtitle = "Each tank-year priced by its own age cell. Dashed = break-even uniform premium. Band = severity 95% CI.",
         caption = "Fair premium = per-tank cell hazard (01p) x mean net-of-deductible severity. Blue: pays less than tau. Terracotta: pays more.") +
    theme_pub()
  save_fig(pB, sprintf("Fig_TankCrossSub_Pooled_Panel_%s", tag), 12, 4)

  # Fig C: fee vs fair premium (the first-order point — price too low)
  paid <- rbindlist(lapply(FIG_STATES, function(st) { d <- rank_dt(p[state == st], fc); d[, state := st]; d }))
  pC <- ggplot(paid, aes(pct, .data[[fc]])) +
    geom_ribbon(aes(ymin = fee_tank, ymax = .data[[fc]]), fill = COL_OVER) +
    geom_line(color = COL_LINE, linewidth = 0.5) +
    geom_line(aes(y = fee_tank), color = COL_FEE, linewidth = 0.5) +
    facet_wrap(~ state, scales = "free", nrow = 1) +
    scale_x_continuous("Tank-years, ranked low to high fair premium (percentile)",
                       labels = function(x) paste0(x,"%")) +
    scale_y_continuous("$ / tank-year (2023)", labels = dollar_format(accuracy = 1)) +
    labs(title = sprintf("What the fund charges vs the fair premium (tank level) [%s]", tag),
         subtitle = "Black = actual per-tank fee. Line = fair premium. The gap is the gas-tax-funded shortfall.",
         caption = "Fee = fr_premium_per_tank_yr (NM gas-tax financed = 0).") +
    theme_pub()
  save_fig(pC, sprintf("Fig_TankPaidShare_Pooled_%s", tag), 12, 4)
}


# =============================================================================
cat("=== STEP 7: SUMMARY ===\n")
# =============================================================================
cat(sprintf("S_bar = %s [%s, %s] | scope=%s | snap=%d | modes: %s\n",
            dollar(S_bar, accuracy=1), dollar(S_lo, accuracy=1), dollar(S_hi, accuracy=1),
            HAZARD_SCOPE, SNAP_YEAR, paste(WALL_MODES, collapse=", ")))
cat("Done.\n")
