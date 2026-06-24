# ==============================================================================
# AE09_Identification_Gp_Gr.R -- researcher-directed (2026-06-16).
# Two identification exhibits for the portfolio (scale-incorporation) model.
#
#   FIG 1 (gamma_risk): per-state deductible D_s vs. mean facility hazard H(n).
#       OOP = gamma_r * H(n) * D_s, so the identifying variation is hazard
#       (within state, across compositions/age) crossed with the deductible
#       (across states). One point per state at (mean H, D_s); whiskers show
#       the within-state 10-90 hazard spread. D=0 states sit on the floor.
#
#   FIG 2 (gamma_price): mean total facility premium over age band, one line
#       per state, faceted by size, walls pooled. TX (risk-based) rises with
#       age; every flat-fee state's line is flat in age (premium = tau*N).
#       Faceting by size holds the count channel fixed.
#
# Built from PM_Lookups.rds + PM_StateSpace.rds + pm_panel.csv ONLY.
# READ-ONLY DESCRIPTIVE. No estimator object is created or modified.
# Scope: 16 states with usable contracts (TX + 15 FF); KS, MD excluded.
# ==============================================================================
suppressPackageStartupMessages({
  library(data.table); library(ggplot2); library(here); library(scales)
})

.log_path <- here::here("logs", paste0("AE09_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".log"))
dir.create(dirname(.log_path), recursive = TRUE, showWarnings = FALSE)
.log <- file(.log_path, open = "wt")
sink(.log, type = "output"); sink(.log, type = "message", append = TRUE)
on.exit({ sink(type = "output"); sink(type = "message"); close(.log) }, add = TRUE)
cat(sprintf("LOG START %s\nScript: AE09\nR: %s\nWD: %s\n\n",
            .log_path, R.version.string, getwd()))

FIG_DIR  <- here("Output", "Figures"); TAB_DIR <- here("Output", "Tables")
DATA_DIR <- here("Data", "Analysis");  RES_DIR <- here("Output", "Estimation_Results")
FF_COL <- "#E76F51"; RB_COL <- "#2A9D8F"
th_ae <- theme_minimal(base_size = 11) +
  theme(plot.title = element_blank(), plot.subtitle = element_blank(),
        plot.caption = element_blank(), legend.position = "bottom",
        panel.grid.minor = element_blank())
has_repel <- requireNamespace("ggrepel", quietly = TRUE)

# ------------------------------------------------------------------------------
# 0. Lookups + state space
# ------------------------------------------------------------------------------
cat("=== SECTION 0: load lookups + state space ===\n")
lk <- readRDS(file.path(RES_DIR, "PM_Lookups.rds"))
ss <- readRDS(file.path(RES_DIR, "PM_StateSpace.rds"))
SCALE   <- lk$SCALE
h_aw    <- lk$h_aw                       # length-16 cell hazard (MARG order)
D_vec   <- lk$D                          # named per-state deductible (model units)
tau_vec <- lk$tau                        # named per-state flat fee (model units); TX=NA
excl    <- lk$excluded_states            # KS, MD
ERAS    <- colnames(ss$P_RB_all)         # c("2006","2014","2019")
stopifnot(length(h_aw) == 16L, all(ERAS == c("2006", "2014", "2019")))
cat(sprintf("  excluded states: %s | eras: %s\n",
            paste(excl, collapse = ", "), paste(ERAS, collapse = ", ")))

# ------------------------------------------------------------------------------
# 1. pm_panel -> comp_id (vectorized key match against state-space enumeration)
# ------------------------------------------------------------------------------
cat("=== SECTION 1: pm_panel -> comp_id ===\n")
pm <- fread(file.path(DATA_DIR, "pm_panel.csv"))
pm[, panel_id := as.character(panel_id)]
CELL_COLS <- c(paste0("n_SW", 8:1), paste0("n_DW", 8:1))   # MARG order
pm_cell_mat <- as.matrix(pm[, ..CELL_COLS]); storage.mode(pm_cell_mat) <- "integer"
pm_keys <- do.call(paste, c(as.data.frame(pm_cell_mat), sep = "-"))
pm[, comp_id := match(pm_keys, ss$keys)]
cat(sprintf("  pm_panel rows: %s | composition in enumeration: %.4f\n",
            format(nrow(pm), big.mark = ","), mean(!is.na(pm$comp_id))))

# keep rows with a known composition and a usable contract (drop KS, MD)
pm <- pm[!is.na(comp_id) & !(g %chin% excl)]
cat(sprintf("  rows after dropping off-enumeration + excluded states: %s | states: %d\n",
            format(nrow(pm), big.mark = ","), uniqueN(pm$g)))

# ------------------------------------------------------------------------------
# 2. Per-facility-year statics: hazard, age band, size, premium
# ------------------------------------------------------------------------------
cat("=== SECTION 2: hazard, age band, size, premium ===\n")
pm[, h_idx    := ss$h_idx[comp_id]]
pm[, H        := h_aw[h_idx]]                       # cell hazard at maj-wall, avg-age band
pm[, abar_bin := ss$abar_bin[comp_id]]
pm[, size     := pmin(N, 4L)]
pm[, size_lab := factor(fcase(size == 1L, "1 tank", size == 2L, "2 tanks",
                              size == 3L, "3 tanks", default = "4+ tanks"),
                        levels = c("1 tank", "2 tanks", "3 tanks", "4+ tanks"))]
stopifnot(!anyNA(pm$H), !anyNA(pm$abar_bin))

# total premium (model units): TX = composition-weighted card; FF = tau * N
era_idx <- match(pm$era, ERAS); stopifnot(!anyNA(era_idx))
pm[, P_RB := ss$P_RB_all[cbind(comp_id, era_idx)]]
pm[, prem_usd := fifelse(g == "TX", P_RB, tau_vec[g] * N) * SCALE]
stopifnot(!anyNA(pm$prem_usd))
cat(sprintf("  premium $ range: [%.0f, %.0f] | H range: [%.4f, %.4f]\n",
            min(pm$prem_usd), max(pm$prem_usd), min(pm$H), max(pm$H)))

# ==============================================================================
# FIG 1 (gamma_risk): deductible vs. mean facility hazard, one point per state
# ==============================================================================
cat("=== FIG 1: deductible vs. hazard (gamma_risk) ===\n")
agg1 <- pm[, .(meanH = mean(H),
               q10   = as.numeric(quantile(H, 0.10)),
               q90   = as.numeric(quantile(H, 0.90)),
               nfy   = .N), by = .(state = g)]
agg1[, D_usd  := D_vec[state] * SCALE]
agg1[, regime := fifelse(state == "TX", "Risk-based (TX)", "Flat fee")]
setorder(agg1, -D_usd)
fwrite(agg1[, .(state, regime, meanH = round(meanH, 5),
                q10 = round(q10, 5), q90 = round(q90, 5),
                D_usd = round(D_usd, 0), nfy)],
       file.path(TAB_DIR, "AE09_Deductible_vs_Hazard.csv"))
cat("  per-state (H, D):\n"); print(agg1[, .(state, regime, meanH = round(meanH, 4),
                                             D_usd = round(D_usd, 0), nfy)])

p1 <- ggplot(agg1, aes(meanH, D_usd, color = regime)) +
  geom_segment(aes(x = q10, xend = q90, y = D_usd, yend = D_usd),
               linewidth = 0.4, alpha = 0.30) +
  geom_point(size = 2.4) +
  geom_point(data = agg1[regime == "Risk-based (TX)"], size = 4.0) +  # TX reference
  scale_color_manual(values = c("Risk-based (TX)" = RB_COL, "Flat fee" = FF_COL),
                     name = NULL) +
  scale_y_continuous(labels = dollar) +
  scale_x_continuous(labels = label_number(accuracy = 0.001)) +
  labs(x = "Mean facility hazard H(n)  (annual leak probability)",
       y = "Deductible D per state") +
  th_ae
p1 <- if (has_repel) {
  p1 + ggrepel::geom_text_repel(aes(label = state), size = 3, seed = 1,
                                show.legend = FALSE, max.overlaps = 30,
                                force = 2.5, box.padding = 0.5,
                                min.segment.length = 0, segment.alpha = 0.4)
} else {
  p1 + geom_text(aes(label = state), size = 3, vjust = -0.8, show.legend = FALSE)
}
ggsave(file.path(FIG_DIR, "AE09_Deductible_vs_Hazard.png"), p1, width = 7, height = 5, dpi = 300)
ggsave(file.path(FIG_DIR, "AE09_Deductible_vs_Hazard.pdf"), p1, width = 7, height = 5)
cat("  saved AE09_Deductible_vs_Hazard .png/.pdf + .csv\n")

# ==============================================================================
# FIG 2 (gamma_price): mean total premium over age, one line per state, by size
# ==============================================================================
cat("=== FIG 2: premium by age, by state, faceted by size (gamma_price) ===\n")
agg2 <- pm[, .(mean_prem = mean(prem_usd), n = .N),
           by = .(state = g, age_bin = abar_bin, size_lab)]
agg2 <- agg2[n >= 30]                               # suppress thin display cells
setorder(agg2, state, size_lab, age_bin)
fwrite(agg2[, .(state, size_lab, age_bin, mean_prem = round(mean_prem, 1), n)],
       file.path(TAB_DIR, "AE09_Premium_by_Age_State.csv"))

# palette: 15 FF states from the hue wheel; TX forced to a bold dark teal
ff_states <- sort(setdiff(unique(agg2$state), "TX"))
pal <- setNames(scales::hue_pal(l = 55)(length(ff_states)), ff_states)
pal["TX"] <- "#0B3D2E"                              # near-black teal, unmistakable
agg2[, is_tx := state == "TX"]

p2 <- ggplot(agg2, aes(age_bin, mean_prem, color = state, group = state)) +
  geom_line(aes(linewidth = is_tx)) +
  geom_point(size = 0.7) +
  facet_wrap(~size_lab, nrow = 1) +
  scale_color_manual(values = pal, name = NULL,
                     breaks = c("TX", ff_states)) +
  scale_linewidth_manual(values = c(`FALSE` = 0.5, `TRUE` = 1.5), guide = "none") +
  scale_x_continuous(breaks = AGE_BIN_BREAKS, labels = AGE_BIN_LABELS) +
  scale_y_continuous(labels = dollar) +
  guides(color = guide_legend(nrow = 2, byrow = TRUE,
                              override.aes = list(linewidth = 1.1))) +
  labs(x = "Tank age (years)",
       y = "Mean total facility premium per year") +
  th_ae
ggsave(file.path(FIG_DIR, "AE09_Premium_by_Age_State.png"), p2, width = 9.2, height = 4.2, dpi = 300)
ggsave(file.path(FIG_DIR, "AE09_Premium_by_Age_State.pdf"), p2, width = 9.2, height = 4.2)
cat("  saved AE09_Premium_by_Age_State .png/.pdf + .csv\n")


# ==============================================================================
# FIG 2b (gamma_price): mean total premium over age, one line per state, pooled sizes
# ==============================================================================
cat("=== FIG 2b: premium by age, by state, sizes pooled (gamma_price) ===\n")
agg2_pool <- pm[, .(mean_prem = mean(prem_usd), n = .N),
                by = .(state = g, age_bin = abar_bin)]
agg2_pool <- agg2_pool[n >= 30]                     # suppress thin display cells
setorder(agg2_pool, state, age_bin)
fwrite(agg2_pool[, .(state, age_bin, mean_prem = round(mean_prem, 1), n)],
       file.path(TAB_DIR, "AE09_Premium_by_Age_Pooled.csv"))

# palette: 15 FF states from the hue wheel; TX forced to a bold dark teal
ff_states <- sort(setdiff(unique(agg2_pool$state), "TX"))
pal <- setNames(scales::hue_pal(l = 55)(length(ff_states)), ff_states)
pal["TX"] <- "#0B3D2E"                              # near-black teal, unmistakable
agg2_pool[, is_tx := state == "TX"]

p2b <- ggplot(agg2_pool, aes(age_bin, mean_prem, color = state, group = state)) +
  geom_line(aes(linewidth = is_tx)) +
  geom_point(size = 0.9) +
  scale_color_manual(values = pal, name = NULL,
                     breaks = c("TX", ff_states)) +
  scale_linewidth_manual(values = c(`FALSE` = 0.5, `TRUE` = 1.5), guide = "none") +
  scale_x_continuous(breaks = AGE_BIN_BREAKS, labels = AGE_BIN_LABELS) +
  scale_y_continuous(labels = dollar) +
  guides(color = guide_legend(nrow = 2, byrow = TRUE,
                              override.aes = list(linewidth = 1.1))) +
  labs(x = "Tank age (years)",
       y = "Mean total facility premium per year") +
  th_ae

ggsave(file.path(FIG_DIR, "AE09_Premium_by_Age_Pooled.png"), p2b, width = 7.0, height = 4.8, dpi = 300)
ggsave(file.path(FIG_DIR, "AE09_Premium_by_Age_Pooled.pdf"), p2b, width = 7.0, height = 4.8)
cat("  saved AE09_Premium_by_Age_Pooled .png/.pdf + .csv\n")

# TX age slope, pooled sizes (sanity)
cat("  TX mean total premium by age band (pooled sizes):\n")
print(pm[g == "TX", .(mean_prem = round(mean(prem_usd), 0), n = .N),
         by = .(age_bin = abar_bin)][order(age_bin)])
cat("=== AE09 DONE ===\n")