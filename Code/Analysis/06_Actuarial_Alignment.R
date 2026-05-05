################################################################################
# 06_Actuarial_Alignment.R
#
# PURPOSE:
#   Section 5.3 — Tests whether the Mid-Continent premium schedule tracks
#   actuarially implied expected losses across rate cells, using three
#   entirely independent data sources:
#
#     (1) Predicted hazard rates   — OOB predictions from 01n_CVValidation.R
#                                    (control-state pre-treatment panel)
#     (2) Predicted cleanup costs  — Duan-smeared m4 fitted values from
#                                    05_Claims_Analysis.R
#                                    (six control-state trust fund ledgers)
#     (3) Premium schedule         — Real cell-mean premiums from reconstructed
#                                    Mid-Continent Casualty tank-month data
#                                    (SERFF rate filings, all four periods)
#
#   Also produces the filing-period evolution figure showing that the age
#   gradient became more granular — especially on the downward/discount side —
#   across the four filing periods.
#
# INPUTS (must exist before running):
#   Data/Analysis/dcm_state_hazard_rates.csv       from 01n
#   Output/Tables/dcm_loss_binary_wall.csv         from 05
#   Rate FIllings/Mid-Continent Casualty Company */
#     texas_midcontinent_tank_month_premium_2006_to_04_2014.csv
#     texas_midcontinent_tank_month_premium_2014_05_to_2019_01.csv
#     texas_midcontinent_tank_month_premium_2019_2021.csv
#     texas_midcontinent_tank_month_premium_2021_onwards.csv
#
# OUTPUTS:
#   Output/Figures/Figure_Actuarial_Alignment.png/.pdf
#   Output/Figures/Figure_Premium_Age_Evolution.png/.pdf
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

options(scipen = 999)

cat("=================================================================\n")
cat("06_Actuarial_Alignment.R\n")
cat("Premium Schedule vs. Actuarial Expected Loss\n")
cat("=================================================================\n\n")

# ── Output directories ────────────────────────────────────────────────────────
OUTPUT_FIGURES <- here("Output", "Figures")
OUTPUT_TABLES  <- here("Output", "Tables")
ANALYSIS_DIR   <- here("Data", "Analysis")

for (d in c(OUTPUT_FIGURES, OUTPUT_TABLES))
  dir.create(d, recursive = TRUE, showWarnings = FALSE)

# ── Colours and theme ─────────────────────────────────────────────────────────
COL_SW <- "#D55E00"
COL_DW <- "#0072B2"

PERIOD_COLORS <- c(
  "2006-2014" = "#D55E00",
  "2014-2019" = "#E69F00",
  "2019-2021" = "#009E73",
  "2021+"     = "#0072B2"
)

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

# ── Age bin ordering (5-year, matching 05_Claims_Analysis.R) ─────────────────
AGE_BIN_ORDER_5YR <- c("0-5", "5-10", "10-15", "15-20",
                        "20-25", "25-30", "30-35", "35+")

bin_age_years <- function(a) {
  fcase(
    is.na(a),   NA_character_,
    a <   5,    "0-5",
    a <  10,    "5-10",
    a <  15,    "10-15",
    a <  20,    "15-20",
    a <  25,    "20-25",
    a <  30,    "25-30",
    a <  35,    "30-35",
    a >= 35,    "35+"
  )
}


################################################################################
#### S1: Load Hazard Rates (01n output) ########################################
################################################################################

cat("\n── S1: Loading hazard rates ──────────────────────────────────────────\n")

hazard_path <- file.path(ANALYSIS_DIR, "dcm_state_hazard_rates.csv")
if (!file.exists(hazard_path))
  stop("dcm_state_hazard_rates.csv not found.\n",
       "Run 01n_CVValidation.R first and ensure the S11 addition is present.")

hazard_raw <- fread(hazard_path)
cat(sprintf("  Loaded: %d rows from dcm_state_hazard_rates.csv\n",
    nrow(hazard_raw)))

# Confirm required columns
req_haz <- c("age_bin_int", "has_single_walled", "h_hat", "h_hat_per1k", "n_fac_years")
missing_haz <- setdiff(req_haz, names(hazard_raw))
if (length(missing_haz) > 0)
  stop("hazard file missing columns: ", paste(missing_haz, collapse = ", "))

hazard_raw[, wall_label := fifelse(
  as.integer(has_single_walled) == 1L, "Single-Walled", "Double-Walled"
)]

# ── Crosswalk: 3-year age bins (01n) → 5-year age bins (05 / premiums) ───────
#
# 01n uses 9 bins with breaks at (0,3,6,9,12,15,18,21,24,Inf).
# age_bin_int: 1="0-2", 2="3-5", 3="6-8", 4="9-11", 5="12-14",
#              6="15-17", 7="18-20", 8="21-23", 9="24+"
#
# Mapping to 5-year bins:
#   int 1 (0-2)   → "0-5"
#   int 2 (3-5)   → "0-5"     ← note: 3-5 straddles the boundary; assign low
#   int 3 (6-8)   → "5-10"
#   int 4 (9-11)  → "10-15"   ← 9 straddles; assign to 10-15
#   int 5 (12-14) → "10-15"
#   int 6 (15-17) → "15-20"
#   int 7 (18-20) → "15-20"
#   int 8 (21-23) → "20-25"
#   int 9 (24+)   → "20-25"   (24+ facilities are mostly 24-30; bin at 20-25)
#
# Bins 25-30, 30-35, 35+ in the cost/premium data have no hazard counterpart
# because the 01n estimation sample (1990-entry cohort) caps out around 24 years
# by the pre-treatment window end. These cells are dropped from the alignment
# figure with a note.

crosswalk <- data.table(
  age_bin_int  = 1:9,
  age_bins_5yr = c("0-5", "0-5", "5-10", "10-15", "10-15",
                   "15-20", "15-20", "20-25", "20-25")
)

hazard_xw <- merge(hazard_raw, crosswalk, by = "age_bin_int")

# Aggregate 3-year cells to 5-year cells using facility-year weighted mean
hazard_5yr <- hazard_xw[, .(
  h_hat       = weighted.mean(h_hat,       w = n_fac_years, na.rm = TRUE),
  h_hat_per1k = weighted.mean(h_hat_per1k, w = n_fac_years, na.rm = TRUE),
  n_fac_years = sum(n_fac_years)
), by = .(age_bins_5yr, wall_label)]

hazard_5yr[, age_bins_5yr := factor(age_bins_5yr, levels = AGE_BIN_ORDER_5YR)]
setorder(hazard_5yr, wall_label, age_bins_5yr)

cat(sprintf("  Hazard cells after 5-year aggregation: %d\n", nrow(hazard_5yr)))
cat("  Cell summary:\n")
print(hazard_5yr[, .(age_bins_5yr, wall_label,
                     h_hat_per1k = round(h_hat_per1k, 2),
                     n_fac_years)])


################################################################################
#### S2: Load Cost Predictions (05 output) #####################################
################################################################################

cat("\n── S2: Loading cost predictions ──────────────────────────────────────\n")

loss_path <- file.path(OUTPUT_TABLES, "dcm_loss_binary_wall.csv")
if (!file.exists(loss_path))
  stop("dcm_loss_binary_wall.csv not found.\n",
       "Run 05_Claims_Analysis.R first and ensure the DCM output block is present.")

loss_raw <- fread(loss_path)
cat(sprintf("  Loaded: %d rows from dcm_loss_binary_wall.csv\n", nrow(loss_raw)))

# Confirm required columns
req_loss <- c("age_bins", "wall_binary", "L_hat", "n_claims")
missing_loss <- setdiff(req_loss, names(loss_raw))
if (length(missing_loss) > 0)
  stop("loss file missing columns: ", paste(missing_loss, collapse = ", "))

# Rename to match join keys
loss_dt <- copy(loss_raw)
setnames(loss_dt, c("age_bins", "wall_binary"), c("age_bins_5yr", "wall_label"))
loss_dt[, age_bins_5yr := factor(age_bins_5yr, levels = AGE_BIN_ORDER_5YR)]

cat(sprintf("  Cost cells available: %d\n", nrow(loss_dt)))
cat("  Cell summary:\n")
print(loss_dt[, .(age_bins_5yr, wall_label,
                  L_hat    = dollar(round(L_hat, 0)),
                  n_claims)])


################################################################################
#### S3: Load Real Premium Data (tank-month outputs from rate engines) #########
################################################################################

cat("\n── S3: Loading real premium data ─────────────────────────────────────\n")

# ── Path helper ───────────────────────────────────────────────────────────────
# Note: the Rate FIllings directory name contains a soft-hyphen character
# in the original scripts. Try both variants.
rate_dir_candidates <- c(
  here("Rate FIllings", "Mid-Continent Casualty Company \u00ad\u2013 23418"),
  here("Rate FIllings", "Mid-Continent Casualty Company - 23418"),
  here("Rate FIllings", "Mid-Continent Casualty Company \u2013 23418")
)
RATE_DIR <- rate_dir_candidates[dir.exists(rate_dir_candidates)][1]
if (is.na(RATE_DIR))
  stop("Rate FIllings directory not found. Check path encoding in Rate FIllings/.")

cat(sprintf("  Rate dir: %s\n", RATE_DIR))

# ── File registry ─────────────────────────────────────────────────────────────
tm_registry <- data.table(
  period_id    = c("P1", "P2", "P3", "P4"),
  filing_period = c("2006-2014", "2014-2019", "2019-2021", "2021+"),
  filename     = c(
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

missing_files <- tm_registry[exists == FALSE]
if (nrow(missing_files) > 0) {
  cat(sprintf(
    "  WARNING: %d filing period(s) missing. Alignment figure will use available periods only.\n",
    nrow(missing_files)))
}

# ── Load available files ──────────────────────────────────────────────────────
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
  stop("No tank-month premium files loaded. Check RATE_DIR and file names.")

cat(sprintf("  Total tank-months: %s across %d periods\n",
    format(nrow(tm_all), big.mark = ","),
    uniqueN(tm_all$filing_period)))

# ── Standardise wall type ─────────────────────────────────────────────────────
# is_double_walled_steel = 1 → Double-Walled
# All other construction types → Single-Walled
# Reinforced fiberglass gets 0.00 construction load (same as SW) so grouping
# it with SW is consistent with the premium schedule logic.

if ("is_double_walled_steel" %in% names(tm_all)) {
  tm_all[, wall_label := fcase(
    as.integer(is_double_walled_steel) == 1L, "Double-Walled",
    default = "Single-Walled"
  )]
} else {
  # Fallback: use double_walled flag if present
  if ("double_walled" %in% names(tm_all)) {
    tm_all[, wall_label := fcase(
      as.integer(double_walled) == 1L, "Double-Walled",
      default = "Single-Walled"
    )]
    cat("  NOTE: used double_walled flag (is_double_walled_steel not found)\n")
  } else {
    stop("Cannot identify wall type column in tank-month data.")
  }
}

# ── Bin age_years to 5-year bins ─────────────────────────────────────────────
if (!"age_years" %in% names(tm_all))
  stop("age_years column missing from tank-month data.")

tm_all[, age_bins_5yr := factor(bin_age_years(age_years),
                                levels = AGE_BIN_ORDER_5YR)]

# ── Detect premium frequency: annual vs monthly ───────────────────────────────
# BASE_RATE = $300/tank/year in all filings. If tank_premium median is ~$300-400
# the column is already annual (one rate applied per contract month record).
# If median is ~$25-35, the column is monthly and needs x12.
# We inspect the distribution and apply the appropriate multiplier.

prem_median <- median(tm_all$tank_premium, na.rm = TRUE)
cat(sprintf("\n  tank_premium median: $%.2f\n", prem_median))

if (prem_median < 100) {
  cat("  Detected MONTHLY premium — multiplying by 12 to annualise.\n")
  tm_all[, annual_tank_premium := tank_premium * 12]
} else {
  cat("  Detected ANNUAL premium — using tank_premium directly.\n")
  tm_all[, annual_tank_premium := tank_premium]
}

# ── Validity filter ───────────────────────────────────────────────────────────
n_before <- nrow(tm_all)
tm_all <- tm_all[
  !is.na(age_bins_5yr)         &
  !is.na(annual_tank_premium)  &
  annual_tank_premium > 0      &
  !is.na(wall_label)
]
cat(sprintf("  After validity filter: %s rows (dropped %s)\n",
    format(nrow(tm_all), big.mark = ","),
    format(n_before - nrow(tm_all), big.mark = ",")))

# ── Cell-mean premiums by filing period ───────────────────────────────────────
cell_premiums_by_period <- tm_all[, .(
  mean_premium    = mean(annual_tank_premium,   na.rm = TRUE),
  median_premium  = median(annual_tank_premium, na.rm = TRUE),
  sd_premium      = sd(annual_tank_premium,     na.rm = TRUE),
  n_tank_months   = .N,
  mean_age_years  = mean(age_years,             na.rm = TRUE)
), by = .(filing_period, period_id, age_bins_5yr, wall_label)]

setorder(cell_premiums_by_period, period_id, wall_label, age_bins_5yr)

fwrite(cell_premiums_by_period,
       file.path(OUTPUT_TABLES, "Table_Premium_Cell_By_Period.csv"))
cat(sprintf("\n  Cell-period table saved: %d cells\n",
    nrow(cell_premiums_by_period)))

# ── Cell-mean premiums for P1 (2006-2014) — alignment figure baseline ─────────
cell_premiums_p1 <- tm_all[
  period_id == "P1" & !is.na(age_bins_5yr),
  .(
    annual_premium = mean(annual_tank_premium, na.rm = TRUE),
    n_tank_months  = .N
  ),
  by = .(age_bins_5yr, wall_label)
]

if (nrow(cell_premiums_p1) == 0) {
  cat("  WARNING: P1 (2006-2014) data not available.\n")
  cat("  Falling back to earliest available period for alignment figure.\n")
  earliest_period <- tm_all[, min(period_id)]
  cell_premiums_p1 <- tm_all[
    period_id == earliest_period,
    .(annual_premium = mean(annual_tank_premium, na.rm = TRUE),
      n_tank_months  = .N),
    by = .(age_bins_5yr, wall_label)
  ]
  cat(sprintf("  Using period: %s\n",
      tm_registry[period_id == earliest_period, filing_period]))
}

cat("\n  P1 cell premiums (annualised per tank):\n")
print(cell_premiums_p1[order(wall_label, age_bins_5yr),
                        .(age_bins_5yr, wall_label,
                          annual_premium = dollar(round(annual_premium, 0)),
                          n_tank_months)])


################################################################################
#### S4: Merge and Compute Expected Loss #######################################
################################################################################

cat("\n── S4: Computing expected loss ───────────────────────────────────────\n")

# Merge hazard + cost on (age_bins_5yr, wall_label)
haz_cost <- merge(
  hazard_5yr[, .(age_bins_5yr, wall_label, h_hat, h_hat_per1k, n_fac_years)],
  loss_dt[,    .(age_bins_5yr, wall_label, L_hat, n_claims)],
  by = c("age_bins_5yr", "wall_label"),
  all = FALSE
)

cat(sprintf("  Cells after hazard-cost merge: %d\n", nrow(haz_cost)))

if (nrow(haz_cost) == 0)
  stop("Hazard-cost merge produced no rows. Check age bin label alignment.")

# Expected loss = annual first-release probability x expected cost per release
# h_hat is probability per facility-year (not per 1,000)
# L_hat is expected remediation cost in 2023 USD per confirmed release
# Result: expected environmental loss in 2023 USD per facility-year

haz_cost[, expected_loss     := h_hat * L_hat]
haz_cost[, expected_loss_1k  := h_hat_per1k * L_hat / 1000]

cat("  Expected loss by cell:\n")
print(haz_cost[order(wall_label, age_bins_5yr),
               .(age_bins_5yr, wall_label,
                 h_hat_per1k  = round(h_hat_per1k, 2),
                 L_hat        = dollar(round(L_hat, 0)),
                 expected_loss = dollar(round(expected_loss, 0)))])

# Merge with P1 premiums
align_dt <- merge(
  haz_cost,
  cell_premiums_p1[, .(age_bins_5yr, wall_label, annual_premium, n_tank_months)],
  by = c("age_bins_5yr", "wall_label"),
  all = FALSE
)

cat(sprintf("\n  Cells in final alignment table: %d\n", nrow(align_dt)))

if (nrow(align_dt) == 0)
  stop("Premium merge produced no rows. Check age bin labels in cell_premiums_p1.")

# Loading factor: premium / expected_loss
# lambda = 1: break-even pricing
# lambda > 1: includes profit, admin costs, reinsurance loading
align_dt[, loading_factor := annual_premium / expected_loss]

# Rank correlation — key statistic
spearman_rho <- cor(align_dt$expected_loss, align_dt$annual_premium,
                    method = "spearman", use = "complete.obs")
pearson_r    <- cor(align_dt$expected_loss, align_dt$annual_premium,
                    method = "pearson",   use = "complete.obs")

cat(sprintf("\n  Gradient alignment:\n"))
cat(sprintf("    Spearman rank correlation: %.3f\n", spearman_rho))
cat(sprintf("    Pearson correlation:       %.3f\n", pearson_r))
cat(sprintf("\n  Loading factor (premium / expected loss):\n"))
cat(sprintf("    Mean:    %.3f\n", mean(align_dt$loading_factor,   na.rm = TRUE)))
cat(sprintf("    Median:  %.3f\n", median(align_dt$loading_factor, na.rm = TRUE)))
cat(sprintf("    Range:   [%.3f, %.3f]\n",
    min(align_dt$loading_factor, na.rm = TRUE),
    max(align_dt$loading_factor, na.rm = TRUE)))

# OLS loading factor (no intercept — proportionality constraint)
lambda_ols <- coef(lm(annual_premium ~ 0 + expected_loss, data = align_dt))
cat(sprintf("    OLS (no-intercept): %.3f\n", lambda_ols))


################################################################################
#### S5: Figure 1 — Actuarial Alignment Scatter ################################
################################################################################

cat("\n── S5: Figure — Actuarial Alignment ─────────────────────────────────\n")

# Proportionality line through origin
x_max_align <- max(align_dt$expected_loss, na.rm = TRUE) * 1.12
fit_line <- data.table(
  expected_loss  = seq(0, x_max_align, length.out = 200),
  annual_premium = lambda_ols * seq(0, x_max_align, length.out = 200)
)

# Annotation position: top-left
annot_x <- min(align_dt$expected_loss, na.rm = TRUE)
annot_y <- max(align_dt$annual_premium, na.rm = TRUE) * 0.97

fig_align <- ggplot(align_dt,
                    aes(x     = expected_loss,
                        y     = annual_premium,
                        color = wall_label,
                        shape = wall_label)) +
  # Proportionality line
  geom_line(data       = fit_line,
            aes(x = expected_loss, y = annual_premium),
            inherit.aes = FALSE,
            color       = "grey50",
            linetype    = "dashed",
            linewidth   = 0.75) +
  # Data points
  geom_point(size = 3.8, alpha = 0.92) +
  # Age bin labels
  geom_text(aes(label = as.character(age_bins_5yr)),
            vjust       = -0.85,
            size        = 2.6,
            color       = "grey30",
            show.legend = FALSE) +
  # Correlation + lambda annotation
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
           label         = sprintf(
             "Spearman \u03c1 = %.2f\n\u03bb (OLS loading) = %.2f",
             spearman_rho, lambda_ols)) +
  scale_color_manual(
    values = c("Single-Walled" = COL_SW, "Double-Walled" = COL_DW),
    name   = "Wall Type") +
  scale_shape_manual(
    values = c("Single-Walled" = 19, "Double-Walled" = 17),
    name   = "Wall Type") +
  scale_x_continuous(
    name   = "Model-Implied Expected Loss per Facility-Year (2023 USD)",
    labels = dollar_format(accuracy = 1),
    expand = expansion(mult = c(0.05, 0.12))) +
  scale_y_continuous(
    name   = "Mid-Continent Annual Tank Premium, 2006-2014 Filing (USD)",
    labels = dollar_format(accuracy = 1),
    expand = expansion(mult = c(0.05, 0.08))) +
  labs(
    title   = "Premium Schedule Tracks Actuarial Expected Loss Across Rate Cells",
    subtitle = sprintf(
      "Each point is one (age bin \u00d7 wall type) cell. Dashed line: premium = \u03bb \u00d7 expected loss (\u03bb = %.2f).",
      lambda_ols),
    caption = paste0(
      "Notes: Expected loss = predicted release probability (cross-validated elastic net, ",
      "pre-treatment control states, 01n) \u00d7 predicted cleanup cost (Duan-smeared OLS, ",
      "six-state trust fund ledgers, 05). ",
      "Premium = cell-mean annual tank premium from Mid-Continent Casualty SERFF filing ",
      "(2006-2014), reconstructed from TCEQ financial responsibility records. ",
      "Data sources are independent. Spearman \u03c1 = ", round(spearman_rho, 2),
      ". Dashed line forced through origin (premium = \u03bb \u00d7 expected loss).")
  ) +
  theme_pub() +
  theme(legend.position = "bottom")

save_fig(fig_align, "Figure_Actuarial_Alignment", w = 7.5, h = 6.5)


################################################################################
#### S6: Figure 2 — Age Gradient Evolution Across Filing Periods ###############
################################################################################

cat("\n── S6: Figure — Premium Age Gradient Evolution ───────────────────────\n")

# We need at least two filing periods for a meaningful evolution figure
n_periods_available <- uniqueN(cell_premiums_by_period$filing_period)
cat(sprintf("  Filing periods available: %d\n", n_periods_available))

if (n_periods_available < 2) {
  cat("  WARNING: fewer than 2 periods available — evolution figure skipped.\n")
} else {

  # Index premiums to the neutral bin (10-15 years) within each period x wall type
  # This removes level differences across periods and shows gradient shape only
  NEUTRAL_BIN <- "10-15"

  neutral_prems <- cell_premiums_by_period[
    age_bins_5yr == NEUTRAL_BIN,
    .(filing_period, wall_label, neutral_premium = mean_premium)
  ]

  evo_dt <- merge(
    cell_premiums_by_period,
    neutral_prems,
    by = c("filing_period", "wall_label")
  )

  evo_dt[, premium_index := mean_premium / neutral_premium]

  # Restrict to bins that are populated in all available periods
  # to avoid gaps in the line chart
  bins_all_periods <- evo_dt[
    !is.na(premium_index) & n_tank_months >= 10,
    .(n_periods = uniqueN(filing_period)),
    by = .(age_bins_5yr, wall_label)
  ][n_periods == n_periods_available, .(age_bins_5yr, wall_label)]

  evo_plot <- merge(evo_dt, bins_all_periods,
                    by = c("age_bins_5yr", "wall_label"))

  evo_plot[, age_bins_5yr   := factor(age_bins_5yr, levels = AGE_BIN_ORDER_5YR)]
  evo_plot[, filing_period  := factor(filing_period,
                                      levels = c("2006-2014","2014-2019",
                                                 "2019-2021","2021+"))]

  # Flag which filing period introduced the young-tank discount granularity
  # (2019+ is the first to have sub-5-year steps; 0-5 bin captures them here)
  young_bins <- c("0-5", "5-10")

  fig_evo <- ggplot(evo_plot,
                    aes(x        = age_bins_5yr,
                        y        = premium_index,
                        color    = filing_period,
                        linetype = wall_label,
                        group    = interaction(filing_period, wall_label))) +
    # Reference line at index = 1
    geom_hline(yintercept = 1,
               linetype   = "dotted",
               color      = "grey55",
               linewidth  = 0.5) +
    # Shade the young-tank discount zone
    annotate("rect",
             xmin  = 0.5,
             xmax  = length(young_bins) + 0.5,
             ymin  = -Inf,
             ymax  = Inf,
             fill  = "grey90",
             alpha = 0.4) +
    annotate("text",
             x      = 1,
             y      = Inf,
             vjust  = 1.6,
             hjust  = 0.5,
             size   = 2.5,
             color  = "grey40",
             label  = "Young-tank\ndiscount zone") +
    geom_line(linewidth = 0.85) +
    geom_point(size = 2.4) +
    scale_color_manual(values = PERIOD_COLORS, name = "Filing Period") +
    scale_linetype_manual(
      values = c("Single-Walled" = "solid", "Double-Walled" = "dashed"),
      name   = "Wall Type") +
    scale_y_continuous(
      name   = "Premium Index (= 1.00 at 10-15 year bin)",
      labels = function(x) sprintf("%.2f\u00d7", x)) +
    scale_x_discrete(name = "Tank Age Bin (years)") +
    labs(
      title    = "Mid-Continent Age Premium Gradient: Increasing Granularity Over Time",
      subtitle = paste0(
        "Indexed to the 10-15 year bin within each filing period and wall type. ",
        "Each line is one filing period. Shaded area = young-tank bins where ",
        "the 2019+ filing introduced fine-grained discount steps absent from earlier schedules."),
      caption = paste0(
        "Notes: Cell-mean annual tank premiums from reconstructed Mid-Continent Casualty ",
        "rate filings (SERFF), four periods: 2006-2014, 2014-2019, 2019-2021, 2021+. ",
        "Indexed at 1.00 for the 10-15 year age bin within each period and wall type. ",
        "The 2006-2014 filing uses 6 age steps (maximum load at >25 years). ",
        "The 2014-2019 filing extends the upper tail to >50 years. ",
        "The 2019+ filing adds two-year increment credits for tanks under 10 years.")
    ) +
    theme_pub() +
    theme(
      axis.text.x      = element_text(angle = 30, hjust = 1),
      legend.position  = "bottom",
      legend.box       = "vertical",
      legend.spacing.y = unit(0.15, "cm")
    )

  save_fig(fig_evo, "Figure_Premium_Age_Evolution", w = 9, h = 6.5)
}


################################################################################
#### S7: Save Output Tables ####################################################
################################################################################

cat("\n── S7: Saving output tables ──────────────────────────────────────────\n")

# ── Full alignment table (appendix) ──────────────────────────────────────────
align_out <- align_dt[, .(
  age_bins_5yr,
  wall_label,
  h_hat_per1k      = round(h_hat_per1k,    2),
  L_hat_2023usd    = round(L_hat,           0),
  expected_loss    = round(expected_loss,   0),
  annual_premium   = round(annual_premium,  0),
  loading_factor   = round(loading_factor,  3),
  n_fac_years,
  n_claims,
  n_tank_months
)]
setorder(align_out, wall_label, age_bins_5yr)

fwrite(align_out, file.path(OUTPUT_TABLES, "Table_Actuarial_Alignment.csv"))
cat(sprintf("  Saved: Table_Actuarial_Alignment.csv (%d rows)\n", nrow(align_out)))

# ── Loading factor summary ────────────────────────────────────────────────────
load_summary <- data.table(
  spearman_rho       = round(spearman_rho, 3),
  pearson_r          = round(pearson_r,    3),
  lambda_ols         = round(lambda_ols,   3),
  lambda_mean        = round(mean(align_dt$loading_factor,   na.rm = TRUE), 3),
  lambda_median      = round(median(align_dt$loading_factor, na.rm = TRUE), 3),
  lambda_sw_mean     = round(
    mean(align_dt[wall_label == "Single-Walled", loading_factor], na.rm = TRUE), 3),
  lambda_dw_mean     = round(
    mean(align_dt[wall_label == "Double-Walled", loading_factor], na.rm = TRUE), 3),
  n_cells            = nrow(align_dt),
  premium_period     = "2006-2014 (P1 baseline)",
  note = paste0(
    "Loading factor = annual_premium / expected_loss. ",
    "Expected loss = h_hat (OOB predicted annual release probability, 01n) x ",
    "L_hat (Duan-smeared OLS cost prediction, 05). ",
    "Premium = cell-mean annual tank premium from Mid-Continent SERFF filing. ",
    "All three data sources are independent.")
)

fwrite(load_summary, file.path(OUTPUT_TABLES, "Table_Actuarial_Loading.csv"))
cat(sprintf("  Saved: Table_Actuarial_Loading.csv\n"))
cat(sprintf("  Saved: Table_Premium_Cell_By_Period.csv (written in S3)\n"))


################################################################################
#### S8: Diagnostic Checks #####################################################
################################################################################

cat("\n── S8: Diagnostic checks ─────────────────────────────────────────────\n")

# Check 1: SW premiums exceed DW premiums at every age bin
sw_dw_check <- dcast(
  align_dt[, .(age_bins_5yr, wall_label, annual_premium)],
  age_bins_5yr ~ wall_label,
  value.var = "annual_premium"
)
sw_dw_check[, sw_gt_dw := `Single-Walled` > `Double-Walled`]
if (all(sw_dw_check$sw_gt_dw, na.rm = TRUE)) {
  cat("  CHECK 1 PASS: Single-Walled premium > Double-Walled at all age bins.\n")
} else {
  cat("  CHECK 1 FAIL: Some age bins have SW premium <= DW premium.\n")
  print(sw_dw_check[sw_gt_dw == FALSE | is.na(sw_gt_dw)])
}

# Check 2: Expected loss monotonically increasing in age for SW
sw_hazard <- align_dt[wall_label == "Single-Walled"][order(age_bins_5yr)]
if (nrow(sw_hazard) > 1) {
  is_mono <- all(diff(sw_hazard$expected_loss) >= 0, na.rm = TRUE)
  if (is_mono) {
    cat("  CHECK 2 PASS: Single-Walled expected loss is non-decreasing in age.\n")
  } else {
    cat("  CHECK 2 NOTE: Single-Walled expected loss is not strictly monotone in age.\n")
    cat("                This may reflect within-bin composition differences (acceptable).\n")
    print(sw_hazard[, .(age_bins_5yr, expected_loss = round(expected_loss, 0))])
  }
}

# Check 3: Rank correlation direction
if (spearman_rho > 0) {
  cat(sprintf("  CHECK 3 PASS: Spearman rho = %.3f (positive gradient alignment).\n",
      spearman_rho))
} else {
  cat(sprintf("  CHECK 3 FAIL: Spearman rho = %.3f (gradient NOT aligned).\n",
      spearman_rho))
}

# Check 4: Loading factor plausibility (should be 1.0 - 3.0 for P&C insurance)
lambda_mean <- mean(align_dt$loading_factor, na.rm = TRUE)
if (lambda_mean >= 0.8 && lambda_mean <= 4.0) {
  cat(sprintf("  CHECK 4 PASS: Mean loading factor = %.2f (plausible range for P&C).\n",
      lambda_mean))
} else {
  cat(sprintf("  CHECK 4 WARNING: Mean loading factor = %.2f (outside expected range).\n",
      lambda_mean))
  cat("  Possible causes: unit mismatch in premium (monthly vs annual), ",
      "cost deflation error, or genuine pricing anomaly.\n")
}

# Check 5: Cell count
n_alignment_cells <- nrow(align_dt)
cat(sprintf("  CHECK 5 INFO: %d cells in alignment figure.\n", n_alignment_cells))
if (n_alignment_cells < 6) {
  cat("  WARNING: fewer than 6 cells — figure may not be credible.\n")
  cat("  Consider whether age bin crosswalk is dropping too many cells.\n")
}


################################################################################
#### Summary ###################################################################
################################################################################

cat("\n========================================================\n")
cat("06_Actuarial_Alignment.R COMPLETE\n\n")
cat(sprintf("  Alignment cells:      %d\n",         nrow(align_dt)))
cat(sprintf("  Spearman rho:         %.3f\n",       spearman_rho))
cat(sprintf("  Pearson r:            %.3f\n",       pearson_r))
cat(sprintf("  OLS loading (lambda): %.3f\n",       lambda_ols))
cat(sprintf("  Premium period:       2006-2014 (P1)\n"))
cat(sprintf("  Filing periods in evolution fig: %d\n",
    uniqueN(cell_premiums_by_period$filing_period)))
cat("\n  Figures:\n")
cat("    Figure_Actuarial_Alignment      (main — Section 5.3)\n")
cat("    Figure_Premium_Age_Evolution    (evolution — Section 5.3 or 2.6)\n")
cat("\n  Tables:\n")
cat("    Table_Actuarial_Alignment.csv   (appendix cell-level detail)\n")
cat("    Table_Actuarial_Loading.csv     (loading factor summary)\n")
cat("    Table_Premium_Cell_By_Period.csv (cell premiums by filing period)\n")
cat("========================================================\n")
