# ==============================================================================
# AE09_OOP_by_Age.R  --  replacement identification exhibit for gamma_risk
#
# Replaces the single deductible-vs-hazard scatter (AE09_Deductible_vs_Hazard)
# with two figures that mirror the premium exhibit (AE09_Premium_by_Age_State)
# and show the SAME variation used to identify gamma_risk:
#
#   FIG 1a (AE09_OOP_by_Age_Size.pdf):
#       Mean OOP = D_s * H(n) by representative age band, one line per state,
#       faceted by facility size.  Directly parallel to the premium figure.
#
#   FIG 1b (AE09_OOP_by_Age_Pooled.pdf):
#       Same, sizes pooled.  Uses right-edge state labels instead of a
#       16-color legend.
#
# Identification story:
#   OOP = gamma_risk * H(n) * D_s.  The within-state slope (OOP rises with
#   tank age because H rises) crossed with the across-state spread in D_s
#   is what pins gamma_risk.  Zero-deductible states (MN, SD, OK, VA) sit
#   flat at OOP = 0 for every age band -- they contribute nothing to
#   identification.  Fading them makes this immediately legible.
#
# Data sources: PM_Lookups.rds, PM_StateSpace.rds, pm_panel.csv  (read-only).
# Drop-in companion to AE09_Identification_Gp_Gr.R; shares identical
# data-loading and panel-prep code so it can run independently or be
# sourced after the parent script if its objects are still in memory.
# ==============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(here)
  library(scales)
})

has_repel <- requireNamespace("ggrepel", quietly = TRUE)

# --------------------------------------------------------------------------
# Logging
# --------------------------------------------------------------------------
.log_path <- here::here(
  "logs",
  paste0("AE09_OOP_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".log")
)
dir.create(dirname(.log_path), recursive = TRUE, showWarnings = FALSE)
.log <- file(.log_path, open = "wt")
sink(.log, type = "output")
sink(.log, type = "message", append = TRUE)
on.exit(
  {
    sink(type = "output")
    sink(type = "message")
    close(.log)
  },
  add = TRUE
)
cat(sprintf(
  "LOG START %s\nScript: AE09_OOP_by_Age\nR: %s\nWD: %s\n\n",
  .log_path, R.version.string, getwd()
))

# --------------------------------------------------------------------------
# Paths and aesthetics  (consistent with AE09_Identification_Gp_Gr.R)
# --------------------------------------------------------------------------
# --------------------------------------------------------------------------
# Paths and aesthetics
# --------------------------------------------------------------------------
FIG_DIR  <- here("Output", "Figures")
TAB_DIR  <- here("Output", "Tables")
DATA_DIR <- here("Data", "Analysis")
RES_DIR  <- here("Output", "Estimation_Results")

# Add these lines to prevent silent ggsave/fwrite failures:
dir.create(FIG_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(TAB_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(DATA_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(RES_DIR, recursive = TRUE, showWarnings = FALSE)

TAB_DIR  <- here("Output", "Tables")
DATA_DIR <- here("Data", "Analysis")
RES_DIR  <- here("Output", "Estimation_Results")

RB_COL <- "#2A9D8F"   # risk-based (TX) teal
FF_COL <- "#E76F51"   # flat-fee orange  (kept for reference; palette built below)

th_ae <- theme_minimal(base_size = 11) +
  theme(
    plot.title    = element_blank(),
    plot.subtitle = element_blank(),
    plot.caption  = element_blank(),
    legend.position   = "bottom",
    panel.grid.minor  = element_blank()
  )

# ==============================================================================
# 0. Lookups + state space
# ==============================================================================
cat("=== SECTION 0: load lookups + state space ===\n")

lk    <- readRDS(file.path(RES_DIR, "PM_Lookups.rds"))
ss    <- readRDS(file.path(RES_DIR, "PM_StateSpace.rds"))
SCALE   <- lk$SCALE
h_aw    <- lk$h_aw           # length-16 cell hazard vector (MARG order)
D_vec   <- lk$D              # named per-state deductible  (model units)
excl    <- lk$excluded_states
ERAS    <- colnames(ss$P_RB_all)

stopifnot(length(h_aw) == 16L, all(ERAS == c("2006", "2014", "2019")))
cat(sprintf(
  "  excluded states: %s | eras: %s\n",
  paste(excl, collapse = ", "), paste(ERAS, collapse = ", ")
))

# States with D = 0 (contribute nothing to gamma_risk identification)
zero_d_states <- names(D_vec[D_vec == 0])
cat(sprintf("  zero-deductible states: %s\n", paste(sort(zero_d_states), collapse = ", ")))

# ==============================================================================
# 1. pm_panel -> comp_id  (identical to parent script)
# ==============================================================================
cat("=== SECTION 1: pm_panel -> comp_id ===\n")

pm <- fread(file.path(DATA_DIR, "pm_panel.csv"))
pm[, panel_id := as.character(panel_id)]

CELL_COLS <- c(paste0("n_SW", 8:1), paste0("n_DW", 8:1))
pm_cell_mat <- as.matrix(pm[, ..CELL_COLS])
storage.mode(pm_cell_mat) <- "integer"
pm_keys <- do.call(paste, c(as.data.frame(pm_cell_mat), sep = "-"))
pm[, comp_id := match(pm_keys, ss$keys)]
pm <- pm[!is.na(comp_id) & !(g %chin% excl)]

cat(sprintf(
  "  rows: %s | states: %d\n",
  format(nrow(pm), big.mark = ","), uniqueN(pm$g)
))

# ==============================================================================
# 2. Per-facility-year statics: hazard, age band, size, OOP
# ==============================================================================
cat("=== SECTION 2: hazard, age band, size, OOP ===\n")

pm[, h_idx    := ss$h_idx[comp_id]]
pm[, H        := h_aw[h_idx]]           # cell hazard (annual leak probability)
pm[, abar_bin := ss$abar_bin[comp_id]]  # representative age band (1-8)
pm[, size     := pmin(N, 4L)]
pm[, size_lab := factor(
  fcase(
    size == 1L, "1 tank",
    size == 2L, "2 tanks",
    size == 3L, "3 tanks",
    default = "4+ tanks"
  ),
  levels = c("1 tank", "2 tanks", "3 tanks", "4+ tanks")
)]

stopifnot(!anyNA(pm$H), !anyNA(pm$abar_bin))

# OOP = D_s * H(n), in USD
# D_vec[g] vectorises correctly because D_vec is a named numeric vector and g is character
pm[, D_usd   := D_vec[g] * SCALE]
pm[, OOP_usd := H * D_usd]

cat(sprintf(
  "  H range: [%.4f, %.4f] | D_usd range: [%.0f, %.0f] | OOP_usd range: [%.2f, %.2f]\n",
  min(pm$H), max(pm$H),
  min(pm$D_usd), max(pm$D_usd),
  min(pm$OOP_usd), max(pm$OOP_usd)
))

# --------------------------------------------------------------------------
# Shared colour palette  (same construction as parent script's FIG 2)
# --------------------------------------------------------------------------
all_states <- sort(unique(pm$g))
ff_states  <- setdiff(all_states, "TX")
pal <- setNames(scales::hue_pal(l = 55)(length(ff_states)), ff_states)
pal["TX"] <- "#0B3D2E"    # near-black teal, unmistakable

# Helper: aesthetic flags
# zero_ded  -- fades D=0 states (they contribute zero identification)
# line_type -- TX solid+bold, D>0 FF solid+thin, D=0 FF dashed+thin

# --------------------------------------------------------------------------
# Age-bin axis labels (bin index 1:8 -> 5-year age range)
# CONFIRM: adjust AGE_BIN_START if bin 1 isn't age 0-5
# --------------------------------------------------------------------------
AGE_BIN_WIDTH  <- 5
AGE_BIN_START  <- 0
AGE_BIN_BREAKS <- 1:8
AGE_BIN_LABELS <- sprintf(
  "%d-%d",
  AGE_BIN_START + (AGE_BIN_BREAKS - 1) * AGE_BIN_WIDTH,
  AGE_BIN_START + AGE_BIN_BREAKS * AGE_BIN_WIDTH
)

# ==============================================================================
# FIG 1a: OOP by age band, faceted by size
# ==============================================================================
cat("=== FIG 1a: OOP by age, faceted by size ===\n")

agg_sz <- pm[
  ,
  .(mean_oop = mean(OOP_usd), n = .N),
  by = .(state = g, age_bin = abar_bin, size_lab)
]
agg_sz <- agg_sz[n >= 30]   # suppress display cells with < 30 facility-years

agg_sz[, is_tx    := state == "TX"]
agg_sz[, zero_ded := state %in% zero_d_states]
agg_sz[, ltype := fcase(
  is_tx,    "tx",
  zero_ded, "zero_d",
  default = "pos_d"
)]

setorder(agg_sz, state, size_lab, age_bin)
fwrite(
  agg_sz[, .(state, size_lab, age_bin, mean_oop = round(mean_oop, 2), n)],
  file.path(TAB_DIR, "AE09_OOP_by_Age_Size.csv")
)

p1a <- ggplot(
  agg_sz,
  aes(age_bin, mean_oop, color = state, group = state)
) +
  geom_line(aes(linewidth = ltype, linetype = ltype)) +
  geom_point(aes(alpha = zero_ded), size = 0.7, show.legend = FALSE) +
  facet_wrap(~size_lab, nrow = 1) +
  # Colour: all states get hue-wheel, TX gets dark teal
  scale_color_manual(
    values = pal,
    name   = NULL,
    breaks = c("TX", ff_states)
  ) +
  # Line width: TX bold, others thin; controlled by ltype
  scale_linewidth_manual(
    values  = c(tx = 1.5, pos_d = 0.5, zero_d = 0.4),
    guide   = "none"
  ) +
  # Line type: D=0 states get dashed to signal "no identification"
  scale_linetype_manual(
    values  = c(tx = "solid", pos_d = "solid", zero_d = "22"),
    guide   = "none"
  ) +
  # Fade D=0 state points
  scale_alpha_manual(
    values  = c(`FALSE` = 1.0, `TRUE` = 0.30),
    guide   = "none"
  ) +
scale_x_continuous(breaks = AGE_BIN_BREAKS, labels = AGE_BIN_LABELS) +
  scale_y_continuous(labels = dollar) +
  guides(color = guide_legend(
    nrow = 2, byrow = TRUE,
    override.aes = list(linewidth = 1.1, linetype = "solid", alpha = 1)
  )) +
  labs(
    x = "Representative age band (5-year bins)",
    y = expression(paste("Mean OOP exposure  ", italic("D") %*% italic("H(n)"), "  per year"))
  ) +
  th_ae

ggsave(file.path(FIG_DIR, "AE09_OOP_by_Age_Size.png"), p1a,
       width = 9.2, height = 4.2, dpi = 300)
ggsave(file.path(FIG_DIR, "AE09_OOP_by_Age_Size.pdf"), p1a,
       width = 9.2, height = 4.2)
cat("  saved AE09_OOP_by_Age_Size .png/.pdf + .csv\n")

# ==============================================================================
# FIG 1b: OOP by age band, sizes pooled
# ==============================================================================
cat("=== FIG 1b: OOP by age, pooled sizes ===\n")

agg_pool <- pm[
  ,
  .(mean_oop = mean(OOP_usd), n = .N),
  by = .(state = g, age_bin = abar_bin)
]
agg_pool <- agg_pool[n >= 30]

agg_pool[, is_tx    := state == "TX"]
agg_pool[, zero_ded := state %in% zero_d_states]
agg_pool[, ltype := fcase(
  is_tx,    "tx",
  zero_ded, "zero_d",
  default = "pos_d"
)]

setorder(agg_pool, state, age_bin)
fwrite(
  agg_pool[, .(state, age_bin, mean_oop = round(mean_oop, 2), n)],
  file.path(TAB_DIR, "AE09_OOP_by_Age_Pooled.csv")
)

# Right-edge label data for ggrepel / fallback geom_text
# Place label at each state's rightmost available age bin

p1b <- ggplot(
  agg_pool,
  aes(age_bin, mean_oop, color = state, group = state)
) +
  geom_line(aes(linewidth = ltype, linetype = ltype)) +
  geom_point(aes(alpha = zero_ded), size = 0.9, show.legend = FALSE) +
  scale_color_manual(values = pal, guide = "none") +
  scale_linewidth_manual(
    values  = c(tx = 1.5, pos_d = 0.5, zero_d = 0.4),
    guide   = "none"
  ) +
  scale_linetype_manual(
    values  = c(tx = "solid", pos_d = "solid", zero_d = "22"),
    guide   = "none"
  ) +
  scale_alpha_manual(
    values  = c(`FALSE` = 1.0, `TRUE` = 0.30),
    guide   = "none"
  ) +
scale_x_continuous(breaks = AGE_BIN_BREAKS, labels = AGE_BIN_LABELS) +

  scale_y_continuous(labels = dollar) +
  labs(
    x = "Representative age band (5-year bins)",
    y = expression(paste("Mean OOP exposure  ", italic("D") %*% italic("H(n)"), "  per year"))
  ) +
  th_ae

# Right-edge state labels (ggrepel if available, plain geom_text otherwise)

# Callout for the D=0 cluster
# Locate the approximate y position of zero-D states (should be at or near 0)
p1b <- p1b +
  annotate(
    "text",
    x     = 1.5,
    y     = max(agg_pool$mean_oop) * 0.03,   # just above the x-axis
    label = paste0("D = 0: ", paste(sort(zero_d_states), collapse = ", ")),
    color = "grey50",
    size  = 2.5,
    hjust = 0
  )

ggsave(file.path(FIG_DIR, "AE09_OOP_by_Age_Pooled.png"), p1b,
       width = 7.0, height = 4.8, dpi = 300)
ggsave(file.path(FIG_DIR, "AE09_OOP_by_Age_Pooled.pdf"), p1b,
       width = 7.0, height = 4.8)
cat("  saved AE09_OOP_by_Age_Pooled .png/.pdf + .csv\n")

# --------------------------------------------------------------------------
# Sanity check: OOP by age band for TX and a high-D FF state
# --------------------------------------------------------------------------
cat("\n  OOP by age band (pooled sizes):\n")
print(agg_pool[order(state, age_bin),
               .(state, age_bin, mean_oop = round(mean_oop, 2), n)])

cat("=== AE09_OOP_by_Age DONE ===\n")