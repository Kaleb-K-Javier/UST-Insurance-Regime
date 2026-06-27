################################################################################
# 07g_Facility_CrossSubsidy.R  —  PERC M1, FACILITY LEVEL (de-confounded, matched to 07f)
#
# Facility-level companion to 07f. Each facility-year's fair premium is the SUM of
# its tanks' fair premiums (each tank priced by its own de-confounded age x wall cell
# from 01p), and its fee is the sum of its tanks' per-tank fees. This keeps the
# facility figure size-consistent (fair AND fee both scale with tank count) and on the
# same hazard object as the tank figure — unlike the old facility figure, which used a
# non-size-scaled facility release probability.
#
#   facility fair premium = sum_tanks [ lambda_hat(tank age x wall cell) * S_bar ]
#   uniform premium tau    = mean facility fair premium over the facilities shown
#   facility fee           = sum_tanks fr_premium_per_tank_yr  (= active_tanks * per-tank fee)
#
# Uniform-premium framing (no "subsidy"); severity-bootstrap CI band on the fair line.
# States: CO, LA, TN (NM absent from panel_dt — no tanks/tank-count, same as 07f).
#
# INPUTS (z_path / data_in):
#   Data/Processed/incident_level_claims.csv     severity pool (CO,NM,PA,TN)
#   Data/Analysis/dcm_cell_hazard_pricing.csv    per-tank cell schedule (01p)
#   Data/Analysis/panel_dt.csv                   tank-year spine (-> facility via panel_id)
#   Data/Analysis/facility_panel.csv             fr_premium_per_tank_yr
#
# OUTPUTS (here):
#   Output/Tables/fac_crosssub_state_summary.csv
#   Data/Analysis/fac_crosssub_facyears.csv
#   Output/Figures/Fig_FacCrossSub_Pooled_Panel.{pdf,png}
#   Output/Figures/Fig_FacCrossSub_Single_<ST>.{pdf,png}
#   Output/Figures/Fig_FacPaidShare_Pooled.{pdf,png}
#
# TICKET: 033 (M1 pricing pipeline, facility companion) | Attempt: 1 | 2026-06-27
################################################################################

suppressPackageStartupMessages({
  library(here); library(data.table); library(ggplot2); library(scales)
})
source(here::here("Code", "Helpers", "data_paths.R"))
options(scipen = 999); set.seed(20260627L)

# === LOGGING ===
.script_name <- "07g_Facility_CrossSubsidy"
.log_path <- here("logs", paste0(.script_name, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".log"))
dir.create(dirname(.log_path), recursive = TRUE, showWarnings = FALSE)
.log <- file(.log_path, open = "wt")
sink(.log, type = "output"); sink(.log, type = "message", append = TRUE)
on.exit({ sink(type = "output"); sink(type = "message"); close(.log) }, add = TRUE)
cat(sprintf("LOG START %s\nScript: %s\nR: %s\nWD: %s\n\n", .log_path, .script_name, R.version.string, getwd()))

OUTPUT_FIGURES <- here("Output", "Figures"); OUTPUT_TABLES <- here("Output", "Tables"); OUTPUT_DATA <- here("Data", "Analysis")
for (d in c(OUTPUT_FIGURES, OUTPUT_TABLES, OUTPUT_DATA)) dir.create(d, recursive = TRUE, showWarnings = FALSE)

# ── Constants (shared with 07f) ──────────────────────────────────────────────
FIG_STATES <- c("CO", "LA", "TN")
SEV_STATES <- c("CO", "NM", "PA", "TN")
D_NM <- 10000; SNAP_YEAR <- 2005L; N_BOOT <- 1000L
HAZARD_SCOPE <- "national"; HAZ_CELL <- "dcm_cell_hazard_pricing.csv"
AGE_BIN_LABELS <- c("0-2","3-5","6-8","9-11","12-14","15-17","18-20","21-23","24+")
AGE_BIN_BREAKS <- c(0, 3, 6, 9, 12, 15, 18, 21, 24, Inf)
COL_UNDER <- "#FAECE7"; COL_OVER <- "#E6F1FB"; COL_LINE <- "#1a1a1a"
COL_BAND <- "#9ecae1"; COL_TAU <- "#555555"; COL_FEE <- "#2B2B2B"

theme_pub <- function(base_size = 11) {
  theme_minimal(base_size = base_size) +
    theme(plot.title = element_text(face="bold", size=rel(1.05), margin=margin(0,0,6,0)),
          plot.subtitle = element_text(color="grey40", size=rel(0.83), margin=margin(0,0,8,0)),
          plot.caption = element_text(color="grey50", size=rel(0.70), hjust=0, margin=margin(8,0,0,0)),
          axis.title = element_text(face="bold", size=rel(0.9)),
          panel.grid.minor = element_blank(), strip.text = element_text(face="bold"),
          plot.background = element_rect(fill="white", color=NA))
}
save_fig <- function(p, name, w = 8, h = 5) {
  ggsave(file.path(OUTPUT_FIGURES, paste0(name, ".png")), p, width=w, height=h, dpi=300, bg="white")
  pdf_path <- file.path(OUTPUT_FIGURES, paste0(name, ".pdf"))
  tryCatch(ggsave(pdf_path, p, width=w, height=h, device=grDevices::cairo_pdf),
           error = function(e) cat(sprintf("  NOTE: PDF skipped (%s): %s\n", basename(pdf_path), conditionMessage(e))))
  cat(sprintf("  Saved: %s\n", name))
}

# =============================================================================
cat("=== STEP 1: SEVERITY S_bar + BOOTSTRAP CI ===\n")
# =============================================================================
inc <- fread(z_path("Data","Processed","incident_level_claims.csv"),
             select = c("panel_id","state","total_cost","total_cost_2023"))
inc <- inc[state %in% SEV_STATES][!is.na(total_cost) & !is.na(total_cost_2023) &
                                   total_cost >= 0 & total_cost_2023 >= 0]
inc[, net_nom  := fifelse(state == "NM", pmax(total_cost - D_NM, 0), total_cost)]
inc[, net_2023 := fifelse(net_nom == 0, 0, net_nom * (total_cost_2023 / total_cost))]
stopifnot(all(is.finite(inc$net_2023)), all(inc$net_2023 >= 0))
S_bar <- inc[, mean(net_2023)]; net_v <- inc$net_2023; n_inc <- length(net_v)
boot_S <- vapply(seq_len(N_BOOT), function(b) mean(net_v[sample.int(n_inc, n_inc, replace=TRUE)]), numeric(1))
S_lo <- as.numeric(quantile(boot_S, 0.025)); S_hi <- as.numeric(quantile(boot_S, 0.975))
cat(sprintf("S_bar = %s [95%% CI %s, %s] (rows=%d)\n",
            dollar(S_bar,accuracy=1), dollar(S_lo,accuracy=1), dollar(S_hi,accuracy=1), n_inc))

# =============================================================================
cat("=== STEP 2: PER-TANK CELL SCHEDULE (from 01p) ===\n")
# =============================================================================
cell_path <- data_in("Data","Analysis", HAZ_CELL)
if (!file.exists(cell_path))
  stop("Cell schedule not found: ", cell_path, "\n  Run 01p_Pricing_Hazard.R first.")
sched <- fread(cell_path)
stopifnot(all(c("state","age_bin","has_single_walled","lambda") %in% names(sched)))
cell_aw <- (if (HAZARD_SCOPE == "national") sched[state == "NATIONAL"] else sched
           )[, .(age_bin, has_single_walled, lambda)]
if (nrow(cell_aw) == 0L) stop("No cell rows for scope=", HAZARD_SCOPE)

# =============================================================================
cat("=== STEP 3: TANK -> per-tank fair premium + fee ===\n")
# =============================================================================
p <- fread(data_in("Data","Analysis","panel_dt.csv"),
           select = c("tank_panel_id","panel_id","state","tank_age","mm_wall","panel_year"))
p <- p[state %in% FIG_STATES & mm_wall %in% c("Single-Walled","Double-Walled")]
p[, has_single_walled := fifelse(mm_wall == "Single-Walled", 1L, 0L)]
p[, age_bin := as.character(cut(tank_age, AGE_BIN_BREAKS, AGE_BIN_LABELS, right=FALSE, include.lowest=TRUE))]
stopifnot(!anyNA(p$age_bin))
p <- merge(p, cell_aw, by = c("age_bin","has_single_walled"), all.x = TRUE)
if (p[is.na(lambda), .N] > 0) stop("Tank-years with no cell-hazard match.")

fp <- fread(data_in("Data","Analysis","facility_panel.csv"),
            select = c("panel_id","panel_year","fr_premium_per_tank_yr"))
p <- merge(p, fp, by = c("panel_id","panel_year"), all.x = TRUE)
setnames(p, "fr_premium_per_tank_yr", "fee_tank"); p[is.na(fee_tank), fee_tank := 0]

# =============================================================================
cat("=== STEP 4: AGGREGATE TANKS -> FACILITY-YEAR ===\n")
# =============================================================================
fac <- p[, .(n_tanks  = .N,
             fac_lambda = sum(lambda),          # expected # tank releases / facility-yr
             fee      = sum(fee_tank)),
         by = .(panel_id, state, panel_year)]
fac[, `:=`(fair   = fac_lambda * S_bar,
           fairlo = fac_lambda * S_lo,
           fairhi = fac_lambda * S_hi)]
stopifnot(all(is.finite(fac$fair)), all(fac$fair > 0))
cat(sprintf("Facility-years: %s | facilities: %s | states %s\n",
            format(nrow(fac), big.mark=","), format(uniqueN(fac$panel_id), big.mark=","),
            paste(sort(unique(fac$state)), collapse=",")))
cat("Median facility fair / fee / n_tanks by state:\n")
print(fac[, .(med_fair = round(median(fair)), med_fee = round(median(fee)),
              med_tanks = as.double(median(n_tanks))), by = state][order(state)])

# =============================================================================
cat("=== STEP 5: UNIFORM PREMIUM tau + SUMMARY ===\n")
# =============================================================================
fac_summary <- function(dt) dt[, {
  tau <- mean(fair)
  .(N = .N, uniform_prem = tau, mean_tanks = mean(n_tanks),
    share_over = mean(fair > tau),
    spread_pct = sum(pmax(fair - tau, 0)) / sum(fair),
    fee_pct_fair = sum(fee) / sum(fair))
}, by = state]
summ <- rbind(cbind(scope = paste0("single_", SNAP_YEAR), fac_summary(fac[panel_year == SNAP_YEAR])),
              cbind(scope = "pooled_all_years",            fac_summary(fac)))
fwrite(summ, file.path(OUTPUT_TABLES, "fac_crosssub_state_summary.csv"))
cat("Pooled-all-years facility summary:\n")
print(summ[scope == "pooled_all_years",
           .(state, N, uniform_prem = round(uniform_prem), mean_tanks = round(mean_tanks,2),
             share_over = round(share_over,3), fee_pct_fair = round(fee_pct_fair,3))])
fwrite(fac[, .(panel_id, state, panel_year, n_tanks, fac_lambda, fair, fairlo, fairhi, fee)],
       file.path(OUTPUT_DATA, "fac_crosssub_facyears.csv"))
cat("Saved: Data/Analysis/fac_crosssub_facyears.csv\n")

# =============================================================================
cat("=== STEP 6: FIGURES ===\n")
# =============================================================================
rank_dt <- function(dt) { d <- copy(dt)[order(fair)]; d[, pct := (seq_len(.N)-0.5)/.N*100]; d[] }

# A few mega multi-tank facilities have huge summed premiums; cap the y-VIEW (zoom,
# not drop) to the 98th percentile so the redistribution around tau stays readable.
YCAP <- ceiling(as.numeric(quantile(fac$fair, 0.98)) / 10000) * 10000
cat(sprintf("Display y-cap (98th pct facility fair): %s (top ~2%% extend beyond)\n", dollar(YCAP)))

# Fig A: single-year per state
for (st in FIG_STATES) {
  d <- rank_dt(fac[state == st & panel_year == SNAP_YEAR]); tau <- mean(d$fair)
  s <- summ[scope == paste0("single_", SNAP_YEAR) & state == st]
  pA <- ggplot(d, aes(pct, fair)) +
    geom_ribbon(data = d[fair <= tau], aes(ymin = fair, ymax = tau), fill = COL_OVER) +
    geom_ribbon(data = d[fair >= tau], aes(ymin = tau, ymax = fair), fill = COL_UNDER) +
    geom_ribbon(aes(ymin = fairlo, ymax = fairhi), fill = COL_BAND, alpha = 0.45) +
    geom_line(color = COL_LINE, linewidth = 0.55) +
    geom_hline(yintercept = tau, linetype = "dashed", color = COL_TAU, linewidth = 0.45) +
    scale_x_continuous("Facilities, ranked low to high fair premium (percentile)", labels = function(x) paste0(x,"%")) +
    scale_y_continuous("Fair premium ($ / facility-year, 2023)", labels = dollar_format(accuracy = 1)) +
    labs(title = sprintf("%s: facility fair premium vs uniform premium, %d", st, SNAP_YEAR),
         subtitle = sprintf("Uniform premium tau = %s/facility-yr (mean %.1f tanks). Band = severity 95%% CI.",
                            dollar(tau, accuracy = 1), s$mean_tanks),
         caption = "Facility fair premium = sum over tanks of (age x wall cell hazard, 01p) x mean net-of-deductible severity. Y capped at p98 (top ~2% exceed).") +
    coord_cartesian(ylim = c(0, YCAP)) +
    theme_pub()
  save_fig(pA, sprintf("Fig_FacCrossSub_Single_%s", st), 8, 5)
}

# Fig B: pooled all facility-years, faceted by state (headline)
pool <- rbindlist(lapply(FIG_STATES, function(st) { d <- rank_dt(fac[state == st]); d[, state := st]; d[, tau := mean(fair)]; d }))
tau_lab <- pool[, .(tau = first(tau)), by = state]
pB <- ggplot(pool, aes(pct, fair)) +
  geom_ribbon(data = pool[fair <= tau], aes(ymin = fair, ymax = tau), fill = COL_OVER) +
  geom_ribbon(data = pool[fair >= tau], aes(ymin = tau, ymax = fair), fill = COL_UNDER) +
  geom_ribbon(aes(ymin = fairlo, ymax = fairhi), fill = COL_BAND, alpha = 0.45) +
  geom_line(color = COL_LINE, linewidth = 0.5) +
  geom_hline(data = tau_lab, aes(yintercept = tau), linetype = "dashed", color = COL_TAU, linewidth = 0.45) +
  facet_wrap(~ state, nrow = 1) +
  scale_x_continuous("Facility-years, ranked low to high fair premium (percentile)", labels = function(x) paste0(x,"%")) +
  scale_y_continuous("Fair premium ($ / facility-year, 2023)", labels = dollar_format(accuracy = 1)) +
  coord_cartesian(ylim = c(0, YCAP)) +
  labs(title = "Facility fair premium vs uniform premium (all years pooled)",
       subtitle = "Each facility = sum of its tanks' fair premiums. Dashed = uniform premium. Band = severity 95% CI. Y capped at p98 (top ~2% exceed).",
       caption = "Fair premium = sum over tanks of (age x wall cell hazard, 01p) x mean net-of-deductible severity. Blue: pays less than tau. Terracotta: pays more.") +
  theme_pub()
save_fig(pB, "Fig_FacCrossSub_Pooled_Panel", 12, 4)

# Fig C: facility fee vs fair (price-too-low)
paid <- rbindlist(lapply(FIG_STATES, function(st) { d <- rank_dt(fac[state == st]); d[, state := st]; d }))
pC <- ggplot(paid, aes(pct, fair)) +
  geom_ribbon(aes(ymin = fee, ymax = fair), fill = COL_OVER) +
  geom_line(color = COL_LINE, linewidth = 0.5) +
  geom_line(aes(y = fee), color = COL_FEE, linewidth = 0.5) +
  facet_wrap(~ state, nrow = 1) +
  scale_x_continuous("Facility-years, ranked low to high fair premium (percentile)", labels = function(x) paste0(x,"%")) +
  scale_y_continuous("$ / facility-year (2023)", labels = dollar_format(accuracy = 1)) +
  coord_cartesian(ylim = c(0, YCAP)) +
  labs(title = "What the fund charges vs the fair premium (facility level)",
       subtitle = "Black = actual facility fee (sum of per-tank fees). Line = fair premium. The gap is the gas-tax-funded shortfall. Y capped at p98.",
       caption = "Fee = active_tanks x fr_premium_per_tank_yr.") +
  theme_pub()
save_fig(pC, "Fig_FacPaidShare_Pooled", 12, 4)

# =============================================================================
cat("=== STEP 7: SUMMARY ===\n")
# =============================================================================
cat(sprintf("S_bar = %s [%s, %s] | scope=%s | snap=%d | states %s\n",
            dollar(S_bar,accuracy=1), dollar(S_lo,accuracy=1), dollar(S_hi,accuracy=1),
            HAZARD_SCOPE, SNAP_YEAR, paste(FIG_STATES, collapse=",")))
cat("Done.\n")
