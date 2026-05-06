################################################################################
# 06_Actuarial_Alignment.R
#
# Tests whether the Mid-Continent premium schedule tracks actuarially implied
# expected losses across age cells. Three independent inputs:
#
#   (1) h_hat   — OOB hazard from 01n (TX + controls, pre-treatment),
#                 single-walled facilities only.
#                 Source: Data/Analysis/dcm_state_hazard_rates.csv (Z)
#
#   (2) L_hat   — Duan-smeared OLS predicted cleanup cost from 05,
#                 single-walled facilities only, re-aggregated to 01n's
#                 3-year age bins from row-level predictions.
#                 Source: Data/Analysis/dcm_predicted_cost_rowlevel.csv (Z)
#
#   (3) Premium — Cell-mean facility-month annualised premium from
#                 reconstructed Mid-Continent SERFF tank-month data,
#                 collapsed to facility-month and binned by avg_tank_age.
#                 Source: Rate FIllings/Mid-Continent Casualty Company .../
#
# Design notes (read before changing this script):
#   * The Texas Mid-Continent fleet is ~0% double-walled steel, so wall type
#     does not generate premium variation in this book. Alignment is on age
#     bins only, single-walled curve only, facility-level on both sides.
#   * All three age binnings are reconciled to 01n's 3-year master grid by
#     re-aggregating from row-level inputs (cost, premium) — no regression
#     is re-run here.
#   * Lambda (OLS through origin) is reported descriptively only. It mixes
#     underwriting markup with TX-vs-control-state cost-level differences
#     and cannot identify either separately. Spearman rho is the headline.
#
# OUTPUTS:
#   Output/Figures/Figure_Actuarial_Alignment.{png,pdf}
#   Output/Figures/Figure_Premium_Age_Evolution.{png,pdf}
#   Output/Tables/Table_Actuarial_Alignment.csv
#   Output/Tables/Table_Actuarial_Loading.csv
#   Output/Tables/Table_Premium_Cell_By_Period.csv
#
# EXECUTION ORDER:
#   01n_CVValidation.R  →  05_Claims_Analysis.R  →  12/13/14_Rate_*.R
#   →  06_Actuarial_Alignment.R  (this script)
################################################################################

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(scales)
  library(here)
})

source(here::here("Code", "Helpers", "data_paths.R"))

options(scipen = 999)

cat("=================================================================\n")
cat("06_Actuarial_Alignment.R  —  Premium vs. Expected Loss\n")
cat("=================================================================\n\n")

# ── 01n age-bin definition (master grid) ─────────────────────────────────────
# Must match make_age_bin() in 01n_CVValidation.R exactly.
AGE_BIN_BREAKS <- c(0, 3, 6, 9, 12, 15, 18, 21, 24, Inf)
AGE_BIN_LABELS <- c("0-2", "3-5", "6-8", "9-11", "12-14",
                    "15-17", "18-20", "21-23", "24+")

bin_age <- function(a) {
  factor(
    cut(a, breaks = AGE_BIN_BREAKS, labels = AGE_BIN_LABELS,
        right = FALSE, include.lowest = TRUE),
    levels = AGE_BIN_LABELS, ordered = FALSE
  )
}

# ── Output dirs ──────────────────────────────────────────────────────────────
OUTPUT_FIGURES <- here("Output", "Figures")
OUTPUT_TABLES  <- here("Output", "Tables")
for (d in c(OUTPUT_FIGURES, OUTPUT_TABLES))
  dir.create(d, recursive = TRUE, showWarnings = FALSE)

# ── Theme ────────────────────────────────────────────────────────────────────
theme_pub <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title       = element_text(face = "bold", size = rel(1.1),
                                      margin = margin(0, 0, 6, 0)),
      plot.subtitle    = element_text(color = "grey40", size = rel(0.85),
                                      margin = margin(0, 0, 8, 0)),
      plot.caption     = element_text(color = "grey50", size = rel(0.72),
                                      hjust = 0, margin = margin(8, 0, 0, 0)),
      axis.title       = element_text(face = "bold", size = rel(0.9)),
      legend.title     = element_text(face = "bold", size = rel(0.9)),
      legend.position  = "bottom",
      panel.grid.minor = element_blank(),
      panel.border     = element_rect(fill = NA, color = "gray85"),
      strip.text       = element_text(face = "bold")
    )
}
theme_set(theme_pub())

save_fig <- function(p, name, w = 8, h = 6) {
  ggsave(file.path(OUTPUT_FIGURES, paste0(name, ".png")),
         p, width = w, height = h, dpi = 300, bg = "white")
  ggsave(file.path(OUTPUT_FIGURES, paste0(name, ".pdf")),
         p, width = w, height = h, device = grDevices::cairo_pdf)
  cat(sprintf("  Saved: %s (.png + .pdf)\n", name))
}

PERIOD_COLORS <- c(
  "2006-2014" = "#D55E00",
  "2014-2019" = "#E69F00",
  "2019-2021" = "#009E73",
  "2021+"     = "#0072B2"
)


################################################################################
#### S1: Hazard (single-walled, 3-year bins from 01n) ##########################
################################################################################

cat("── S1: Hazard ────────────────────────────────────────────────────────\n")

hazard_path <- z_path("Data", "Analysis", "dcm_state_hazard_rates.csv")
if (!file.exists(hazard_path))
  stop("dcm_state_hazard_rates.csv not found on Z. Run 01n first.")

hazard_raw <- fread(hazard_path)

req_haz <- c("age_bin_int", "has_single_walled", "h_hat", "n_fac_years")
missing_haz <- setdiff(req_haz, names(hazard_raw))
if (length(missing_haz) > 0)
  stop("hazard file missing columns: ", paste(missing_haz, collapse = ", "))

hazard_sw <- hazard_raw[
  has_single_walled == 1L,
  .(age_bin_int,
    age_bin = factor(AGE_BIN_LABELS[age_bin_int], levels = AGE_BIN_LABELS),
    h_hat,
    n_fac_years)
]
setorder(hazard_sw, age_bin_int)

cat(sprintf("  Loaded %d SW hazard cells\n", nrow(hazard_sw)))
print(hazard_sw[, .(age_bin,
                    h_hat_per1k = round(h_hat * 1000, 2),
                    n_fac_years)])


################################################################################
#### S2: Cost (single-walled, re-binned from row-level to 3-year) ##############
################################################################################

cat("\n── S2: Cost ─────────────────────────────────────────────────────────\n")

cost_path <- z_path("Data", "Analysis", "dcm_predicted_cost_rowlevel.csv")
if (!file.exists(cost_path))
  stop("dcm_predicted_cost_rowlevel.csv not found on Z. Re-run 05.")

cost_raw <- fread(cost_path)

req_cost <- c("avg_tank_age", "has_single_walled", "predicted_cost_2023")
missing_cost <- setdiff(req_cost, names(cost_raw))
if (length(missing_cost) > 0)
  stop("cost file missing columns: ", paste(missing_cost, collapse = ", "))

cat(sprintf("  Loaded %s row-level cost predictions\n",
    format(nrow(cost_raw), big.mark = ",")))

cost_sw <- cost_raw[
  has_single_walled == 1L &
  !is.na(avg_tank_age) &
  !is.na(predicted_cost_2023) &
  predicted_cost_2023 > 0
]

cost_sw[, age_bin := bin_age(avg_tank_age)]

cost_cells <- cost_sw[!is.na(age_bin), .(
  L_hat    = mean(predicted_cost_2023,   na.rm = TRUE),
  L_median = median(predicted_cost_2023, na.rm = TRUE),
  L_sd     = sd(predicted_cost_2023,     na.rm = TRUE),
  n_claims = .N
), by = age_bin]
cost_cells[, age_bin := factor(age_bin, levels = AGE_BIN_LABELS)]
setorder(cost_cells, age_bin)

cat(sprintf("  SW cost cells (3-year bins): %d\n", nrow(cost_cells)))
print(cost_cells[, .(age_bin,
                     L_hat = dollar(round(L_hat, 0)),
                     n_claims)])


################################################################################
#### S3: Texas premium (facility-level, binned by avg_tank_age) ################
################################################################################

cat("\n── S3: Texas premium ────────────────────────────────────────────────\n")

# Path encoding: "Rate FIllings" directory name has a soft-hyphen in some
# variants. Try all three.
rate_dir_candidates <- c(
  here("Rate FIllings", "Mid-Continent Casualty Company ­– 23418"),
  here("Rate FIllings", "Mid-Continent Casualty Company - 23418"),
  here("Rate FIllings", "Mid-Continent Casualty Company – 23418")
)
RATE_DIR <- rate_dir_candidates[dir.exists(rate_dir_candidates)][1]
if (is.na(RATE_DIR))
  stop("Rate FIllings directory not found. Check path encoding.")

cat(sprintf("  Rate dir: %s\n", RATE_DIR))

tm_registry <- data.table(
  period_id     = c("P1", "P2", "P3", "P4"),
  filing_period = c("2006-2014", "2014-2019", "2019-2021", "2021+"),
  filename      = c(
    "texas_midcontinent_tank_month_premium_2006_to_04_2014.csv",
    "texas_midcontinent_tank_month_premium_2014_05_to_2019_01.csv",
    "texas_midcontinent_tank_month_premium_2019_2021.csv",
    "texas_midcontinent_tank_month_premium_2021_onwards.csv"
  )
)
tm_registry[, filepath := file.path(RATE_DIR, filename)]
tm_registry[, exists   := file.exists(filepath)]

cat("  File availability:\n")
print(tm_registry[, .(period_id, filing_period, exists)])

tm_list <- lapply(seq_len(nrow(tm_registry)), function(i) {
  row <- tm_registry[i]
  if (!row$exists) return(NULL)
  dt <- tryCatch(
    fread(row$filepath, showProgress = FALSE),
    error = function(e) {
      warning(sprintf("Failed to read %s: %s", row$filename, e$message))
      NULL
    }
  )
  if (is.null(dt)) return(NULL)
  dt[, filing_period := row$filing_period]
  dt[, period_id     := row$period_id]
  cat(sprintf("    %s: %s rows\n",
      row$filing_period, format(nrow(dt), big.mark = ",")))
  dt
})

tm_all <- rbindlist(Filter(Negate(is.null), tm_list),
                    use.names = TRUE, fill = TRUE)

if (nrow(tm_all) == 0)
  stop("No tank-month premium files loaded.")

# Detect monthly vs annual premium (BASE_RATE = $300/tank/year in all filings;
# tank_premium ~ $25 → monthly, ~ $300+ → annual)
prem_median <- median(tm_all$tank_premium, na.rm = TRUE)
cat(sprintf("\n  tank_premium median: $%.2f\n", prem_median))

if (prem_median < 100) {
  cat("  Detected MONTHLY premium — annualising x12\n")
  tm_all[, annual_tank_premium := tank_premium * 12]
} else {
  cat("  Detected ANNUAL premium\n")
  tm_all[, annual_tank_premium := tank_premium]
}

# Collapse tank-month → facility-month: avg_tank_age + sum of tank premiums.
# This is the unit that aligns with 01n (facility-mean-age) and 05 (facility
# claim-year). Sum-then-average preserves within-cell tank-count × age
# covariance that mean(tank_prem)*mean(n_tanks) would lose.
fac_month <- tm_all[
  !is.na(age_years) &
  !is.na(annual_tank_premium) &
  annual_tank_premium > 0,
  .(total_premium = sum(annual_tank_premium, na.rm = TRUE),
    avg_tank_age  = mean(age_years,           na.rm = TRUE),
    n_tanks       = .N),
  by = .(FACILITY_ID, YEAR, MONTH, filing_period, period_id)
]

cat(sprintf("\n  Facility-months: %s across %d periods\n",
    format(nrow(fac_month), big.mark = ","),
    uniqueN(fac_month$filing_period)))

fac_month[, age_bin := bin_age(avg_tank_age)]

# Cell-mean facility annual premium by age bin × period
prem_cells_by_period <- fac_month[!is.na(age_bin), .(
  mean_premium    = mean(total_premium,   na.rm = TRUE),
  median_premium  = median(total_premium, na.rm = TRUE),
  sd_premium      = sd(total_premium,     na.rm = TRUE),
  n_fac_months    = .N,
  mean_age        = mean(avg_tank_age,    na.rm = TRUE)
), by = .(filing_period, period_id, age_bin)]
prem_cells_by_period[, age_bin := factor(age_bin, levels = AGE_BIN_LABELS)]
setorder(prem_cells_by_period, period_id, age_bin)

fwrite(prem_cells_by_period,
       file.path(OUTPUT_TABLES, "Table_Premium_Cell_By_Period.csv"))
cat(sprintf("  Cell-period table saved: %d cells\n",
    nrow(prem_cells_by_period)))

# P1 cell premiums for the alignment figure
prem_p1 <- prem_cells_by_period[
  period_id == "P1",
  .(age_bin, annual_premium = mean_premium, n_fac_months)
]

if (nrow(prem_p1) == 0) {
  cat("  WARNING: P1 (2006-2014) data missing — falling back to earliest period.\n")
  earliest <- prem_cells_by_period[, min(as.character(period_id))]
  prem_p1 <- prem_cells_by_period[
    period_id == earliest,
    .(age_bin, annual_premium = mean_premium, n_fac_months)
  ]
}

cat("\n  P1 (2006-2014) cell premiums:\n")
print(prem_p1[order(age_bin),
              .(age_bin,
                annual_premium = dollar(round(annual_premium, 0)),
                n_fac_months)])


################################################################################
#### S4: Merge and compute expected loss #######################################
################################################################################

cat("\n── S4: Expected loss ────────────────────────────────────────────────\n")

# Expected loss = annual first-release probability × predicted cost per claim
# Both estimated outside Texas's premium book; both at the facility level;
# both restricted to single-walled.
haz_cost <- merge(
  hazard_sw[,  .(age_bin, h_hat, n_fac_years)],
  cost_cells[, .(age_bin, L_hat, n_claims)],
  by = "age_bin", all = FALSE
)
haz_cost[, expected_loss := h_hat * L_hat]
setorder(haz_cost, age_bin)

cat("  Expected loss by age bin:\n")
print(haz_cost[, .(age_bin,
                   h_hat_per1k   = round(h_hat * 1000, 2),
                   L_hat         = dollar(round(L_hat, 0)),
                   expected_loss = dollar(round(expected_loss, 0)))])

# Merge with P1 premium → final alignment table
align_dt <- merge(
  haz_cost,
  prem_p1[, .(age_bin, annual_premium, n_fac_months)],
  by = "age_bin", all = FALSE
)
setorder(align_dt, age_bin)

cat(sprintf("\n  Cells in alignment: %d\n", nrow(align_dt)))
if (nrow(align_dt) < 4)
  warning("Fewer than 4 cells in alignment — figure may not be credible.")

# Loading factor (descriptive only; see caveat in caption)
align_dt[, loading_factor := annual_premium / expected_loss]

# Headline statistics
spearman_rho <- cor(align_dt$expected_loss, align_dt$annual_premium,
                    method = "spearman", use = "complete.obs")
pearson_r    <- cor(align_dt$expected_loss, align_dt$annual_premium,
                    method = "pearson",   use = "complete.obs")

# OLS through origin (descriptive level shift)
lambda_ols <- coef(lm(annual_premium ~ 0 + expected_loss, data = align_dt))

cat(sprintf("\n  Spearman rho:  %.3f\n", spearman_rho))
cat(sprintf("  Pearson r:     %.3f\n", pearson_r))
cat(sprintf("  Lambda (OLS):  %.3f  (descriptive — see caption caveat)\n",
    lambda_ols))


################################################################################
#### S5: Alignment figure ######################################################
################################################################################

cat("\n── S5: Alignment figure ─────────────────────────────────────────────\n")

x_max <- max(align_dt$expected_loss, na.rm = TRUE) * 1.12
fit_line <- data.table(
  expected_loss  = seq(0, x_max, length.out = 200),
  annual_premium = lambda_ols * seq(0, x_max, length.out = 200)
)

annot_x <- min(align_dt$expected_loss, na.rm = TRUE)
annot_y <- max(align_dt$annual_premium, na.rm = TRUE) * 0.97

fig_align <- ggplot(align_dt,
                    aes(x = expected_loss, y = annual_premium)) +
  geom_line(data        = fit_line,
            inherit.aes = FALSE,
            aes(x = expected_loss, y = annual_premium),
            color       = "grey50",
            linetype    = "dashed",
            linewidth   = 0.75) +
  geom_point(size = 3.8, alpha = 0.92, color = "#0072B2") +
  geom_text(aes(label = as.character(age_bin)),
            vjust = -0.85, size = 2.8, color = "grey30") +
  annotate("label",
           x             = annot_x,
           y             = annot_y,
           hjust         = 0,
           vjust         = 1,
           size          = 3.1,
           fill          = "white",
           color         = "grey20",
           label.size    = 0.25,
           label.padding = unit(0.28, "lines"),
           label         = sprintf("Spearman ρ = %.2f", spearman_rho)) +
  scale_x_continuous(
    name   = "Model-Implied Expected Loss per Facility-Year (2023 USD)",
    labels = dollar_format(accuracy = 1),
    expand = expansion(mult = c(0.05, 0.12))) +
  scale_y_continuous(
    name   = "Mid-Continent Annual Facility Premium, 2006-2014 (USD)",
    labels = dollar_format(accuracy = 1),
    expand = expansion(mult = c(0.05, 0.08))) +
  labs(
    title    = "Premium Schedule Tracks Expected Loss Across Age Bins",
    subtitle = paste0(
      "Single-walled facilities only. Each point is one 3-year age bin. ",
      "Dashed line: OLS through origin (descriptive)."),
    caption  = paste0(
      "Notes: Expected loss = h_hat × L_hat. ",
      "h_hat = OOB hazard from 01n elastic net (TX + controls, ",
      "pre-treatment, single-walled). ",
      "L_hat = Duan-smeared OLS predicted cost from 05 (six-state trust ",
      "fund ledgers, single-walled, re-aggregated to 3-year bins from ",
      "row-level predictions). ",
      "Premium = facility-month total tank premium from reconstructed ",
      "Mid-Continent Casualty SERFF filing (2006-2014), averaged across ",
      "facility-months in each bin. Spearman ρ = ",
      round(spearman_rho, 2), ". ",
      "The OLS-through-origin slope mixes underwriting markup with ",
      "Texas-vs-control-state cost-level differences and cannot identify ",
      "either separately — interpret only as descriptive.")
  ) +
  theme_pub()

save_fig(fig_align, "Figure_Actuarial_Alignment", w = 7.5, h = 6.5)


################################################################################
#### S6: Period evolution figure ###############################################
################################################################################

cat("\n── S6: Period evolution figure ──────────────────────────────────────\n")

n_periods_avail <- uniqueN(prem_cells_by_period$filing_period)
cat(sprintf("  Filing periods available: %d\n", n_periods_avail))

if (n_periods_avail < 2) {
  cat("  WARNING: <2 periods — evolution figure skipped.\n")
} else {
  # Index to the bin closest to "10-15 years" — i.e., 12-14 in 01n's grid.
  NEUTRAL_BIN <- "12-14"

  neutral <- prem_cells_by_period[
    age_bin == NEUTRAL_BIN,
    .(filing_period, neutral_premium = mean_premium)
  ]

  evo_dt <- merge(prem_cells_by_period, neutral, by = "filing_period")
  evo_dt[, premium_index := mean_premium / neutral_premium]

  # Keep bins with non-trivial coverage in every available period
  bins_keep <- evo_dt[
    !is.na(premium_index) & n_fac_months >= 10,
    .(n_pds = uniqueN(filing_period)), by = age_bin
  ][n_pds == n_periods_avail, age_bin]

  evo_plot <- evo_dt[age_bin %in% bins_keep]
  evo_plot[, age_bin := factor(age_bin, levels = AGE_BIN_LABELS)]
  evo_plot[, filing_period := factor(filing_period,
                                      levels = c("2006-2014","2014-2019",
                                                 "2019-2021","2021+"))]

  young_bins <- c("0-2", "3-5")

  fig_evo <- ggplot(evo_plot,
                    aes(x     = age_bin,
                        y     = premium_index,
                        color = filing_period,
                        group = filing_period)) +
    geom_hline(yintercept = 1, linetype = "dotted",
               color = "grey55", linewidth = 0.5) +
    annotate("rect",
             xmin = 0.5, xmax = length(young_bins) + 0.5,
             ymin = -Inf, ymax = Inf,
             fill = "grey90", alpha = 0.4) +
    annotate("text",
             x = 1, y = Inf, vjust = 1.6, hjust = 0.5,
             size = 2.5, color = "grey40",
             label = "Young-tank\ndiscount zone") +
    geom_line(linewidth = 0.85) +
    geom_point(size = 2.4) +
    scale_color_manual(values = PERIOD_COLORS, name = "Filing Period") +
    scale_y_continuous(
      name   = sprintf("Premium Index (= 1.00 at %s bin)", NEUTRAL_BIN),
      labels = function(x) sprintf("%.2f×", x)) +
    scale_x_discrete(name = "Facility Mean Tank Age (years)") +
    labs(
      title    = "Mid-Continent Age Premium Gradient: Increasing Granularity Over Time",
      subtitle = paste0(
        "Cell-mean facility annual premium by 3-year age bin, indexed to ",
        NEUTRAL_BIN, " within each filing period. The 2019+ filing introduces ",
        "fine-grained discount steps for young tanks (shaded zone)."),
      caption  = paste0(
        "Source: reconstructed Mid-Continent Casualty SERFF filings, four ",
        "periods. Facility-month premium = sum of in-service tank premiums; ",
        "binned by facility mean tank age. Bins below 10 facility-months ",
        "in any period are dropped to suppress small-cell noise.")
    ) +
    theme_pub() +
    theme(axis.text.x = element_text(angle = 30, hjust = 1))

  save_fig(fig_evo, "Figure_Premium_Age_Evolution", w = 9, h = 6.5)
}


################################################################################
#### S7: Output tables #########################################################
################################################################################

cat("\n── S7: Output tables ────────────────────────────────────────────────\n")

align_out <- align_dt[, .(
  age_bin,
  h_hat_per1k     = round(h_hat * 1000, 2),
  L_hat_2023usd   = round(L_hat,         0),
  expected_loss   = round(expected_loss, 0),
  annual_premium  = round(annual_premium, 0),
  loading_factor  = round(loading_factor, 3),
  n_fac_years,
  n_claims,
  n_fac_months
)]
fwrite(align_out, file.path(OUTPUT_TABLES, "Table_Actuarial_Alignment.csv"))
cat(sprintf("  Saved: Table_Actuarial_Alignment.csv (%d rows)\n",
    nrow(align_out)))

load_summary <- data.table(
  spearman_rho   = round(spearman_rho, 3),
  pearson_r      = round(pearson_r,    3),
  lambda_ols     = round(lambda_ols,   3),
  lambda_mean    = round(mean(align_dt$loading_factor,   na.rm = TRUE), 3),
  lambda_median  = round(median(align_dt$loading_factor, na.rm = TRUE), 3),
  n_cells        = nrow(align_dt),
  premium_period = "2006-2014 (P1 baseline)",
  scope          = "Single-walled facilities only; Texas Mid-Continent fleet",
  note = paste0(
    "Loading factor = annual_premium / expected_loss. Mixes underwriting ",
    "markup with TX-vs-control-state cost-level differences. Cannot ",
    "identify either separately — interpret only as descriptive.")
)
fwrite(load_summary, file.path(OUTPUT_TABLES, "Table_Actuarial_Loading.csv"))
cat("  Saved: Table_Actuarial_Loading.csv\n")
cat("  Saved: Table_Premium_Cell_By_Period.csv (in S3)\n")


################################################################################
#### S8: Diagnostic checks #####################################################
################################################################################

cat("\n── S8: Diagnostic checks ────────────────────────────────────────────\n")

# CHECK 1: Expected loss roughly increasing in age
is_mono_el <- all(diff(align_dt$expected_loss) >= 0, na.rm = TRUE)
if (is_mono_el) {
  cat("  CHECK 1 PASS: Expected loss non-decreasing in age.\n")
} else {
  cat("  CHECK 1 NOTE: Expected loss not strictly monotone (acceptable —\n")
  cat("                may reflect within-bin composition noise).\n")
}

# CHECK 2: Premium roughly increasing in age
is_mono_pr <- all(diff(align_dt$annual_premium) >= 0, na.rm = TRUE)
if (is_mono_pr) {
  cat("  CHECK 2 PASS: Premium non-decreasing in age.\n")
} else {
  cat("  CHECK 2 NOTE: Premium not strictly monotone in age.\n")
}

# CHECK 3: Rank correlation positive
if (spearman_rho > 0) {
  cat(sprintf("  CHECK 3 PASS: Spearman rho = %.3f (positive).\n",
      spearman_rho))
} else {
  cat(sprintf("  CHECK 3 FAIL: Spearman rho = %.3f.\n", spearman_rho))
}

# CHECK 4: Cell count
cat(sprintf("  CHECK 4 INFO: %d alignment cells.\n", nrow(align_dt)))
if (nrow(align_dt) < 5)
  cat("  WARNING: <5 cells — figure may not be credible.\n")

# CHECK 5: Lambda plausibility (descriptive only)
lambda_mean <- mean(align_dt$loading_factor, na.rm = TRUE)
cat(sprintf("  CHECK 5 INFO: Mean loading factor = %.2f (descriptive).\n",
    lambda_mean))


################################################################################
#### Summary ###################################################################
################################################################################

cat("\n========================================================\n")
cat("06_Actuarial_Alignment.R COMPLETE\n\n")
cat(sprintf("  Alignment cells:   %d\n",   nrow(align_dt)))
cat(sprintf("  Spearman rho:      %.3f\n", spearman_rho))
cat(sprintf("  Pearson r:         %.3f\n", pearson_r))
cat(sprintf("  Lambda (OLS):      %.3f  (descriptive)\n", lambda_ols))
cat(sprintf("  Premium period:    2006-2014 (P1 baseline)\n"))
cat(sprintf("  Filing periods in evolution fig: %d\n", n_periods_avail))
cat("\n  Figures:\n")
cat("    Figure_Actuarial_Alignment      (main — Section 5.3)\n")
cat("    Figure_Premium_Age_Evolution    (period evolution)\n")
cat("\n  Tables:\n")
cat("    Table_Actuarial_Alignment.csv\n")
cat("    Table_Actuarial_Loading.csv\n")
cat("    Table_Premium_Cell_By_Period.csv\n")
cat("========================================================\n")
