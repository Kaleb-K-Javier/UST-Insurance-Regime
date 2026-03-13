#==============================================================================
# 01h_SampleRestrictionFunnel.R
# Sample Restriction Funnel with Balance at Each Step
#
# New funnel (Steps 0–4) ends at mm_fac_primary.
# No restriction to single-tank or gasoline-only — those are cell FE
# dimensions, not sample filters.  Steps mirror the mm_fac_primary
# definition in 02a S4 exactly so readers can trace the regression sample.
#
# Figures:
#   Figure_Funnel_Attrition   — waterfall of facility counts through filters
#   Figure_Funnel_Balance     — balance metrics at each restriction step
#   Combined → Figure_Funnel_Combined
#
# Tables:
#   TableB2_Balance_By_Step    (CSV + LaTeX)
#==============================================================================

source(here::here("Code", "Analysis", "Descrptive Facts", "01a_Setup.R"))
open_log("01h_SampleRestrictionFunnel")
log_cat("=== 01h: SAMPLE RESTRICTION FUNNEL ===\n")

annual_data   <- load_interim("annual_data")
tanks_1998    <- load_interim("tanks_1998")
attrition_log <- load_interim("attrition_log")
fac_mm        <- load_interim("fac_mm")

# ─────────────────────────────────────────────────────────────────────────────
# Helpers
# ─────────────────────────────────────────────────────────────────────────────

# Wald pre-trend test using make-model cell FE
run_wald_p <- function(dt) {
  pre <- dt[panel_year %between% c(1990L, TREATMENT_YEAR) &
              !is.na(make_model_fac) &
              fac_wall != "Unknown-Wall" &
              fac_fuel != "Unknown-Fuel"]
  if (uniqueN(pre$panel_id) < 50) return(NA_real_)
  m <- feols(closure_event ~
               i(rel_year_1999, texas_treated, ref = -1) |
               panel_id + make_model_fac^panel_year,
             data = pre, cluster = ~state)
  w <- fixest::wald(m, "rel_year_1999")
  round(w$p, 4)
}

compute_balance <- function(dt, stage_label) {
  tx  <- dt[texas_treated == 1]
  ctl <- dt[texas_treated == 0]

  ids    <- unique(dt$panel_id)
  f_snap <- fac_mm[panel_id %in% ids]
  f_tx   <- f_snap[panel_id %in% tx$panel_id]
  f_ctl  <- f_snap[panel_id %in% ctl$panel_id]

  # Age gap: oldest tank age at reform (numeric) instead of avg tank age
  age_gap <- mean(f_tx$oldest_age_at_reform,  na.rm = TRUE) -
             mean(f_ctl$oldest_age_at_reform, na.rm = TRUE)

  # Wall gap: share All-SW (high-risk wall type)
  sw_gap  <- mean(f_tx$fac_wall  == "All-SW", na.rm = TRUE) -
             mean(f_ctl$fac_wall == "All-SW", na.rm = TRUE)

  wald_p  <- run_wald_p(dt)

  data.table(
    Stage              = stage_label,
    N_TX               = uniqueN(tx$panel_id),
    N_CTL              = uniqueN(ctl$panel_id),
    Mean_Age_TX        = round(mean(f_tx$oldest_age_at_reform,  na.rm = TRUE), 2),
    Mean_Age_CTL       = round(mean(f_ctl$oldest_age_at_reform, na.rm = TRUE), 2),
    Age_Gap            = round(age_gap, 2),
    SW_Share_TX        = round(mean(f_tx$fac_wall  == "All-SW", na.rm = TRUE), 3),
    SW_Share_CTL       = round(mean(f_ctl$fac_wall == "All-SW", na.rm = TRUE), 3),
    SW_Gap             = round(sw_gap, 3),
    Wald_PreTrend_p    = wald_p
  )
}

# ─────────────────────────────────────────────────────────────────────────────
# Funnel stages (Steps 0–4)
# ─────────────────────────────────────────────────────────────────────────────
log_cat("\n--- Building funnel stages ---\n")

# Stage 0: full analysis sample (post 01c filters)
fac_stage0 <- annual_data

# Stage 1: has reform-date classification
fac_stage1 <- annual_data[!is.na(make_model_fac)]

# Stage 2: + exclude Unknown-Wall
fac_stage2 <- annual_data[
  !is.na(make_model_fac) &
  fac_wall != "Unknown-Wall"
]

# Stage 3: + exclude Unknown-Fuel
fac_stage3 <- annual_data[
  !is.na(make_model_fac) &
  fac_wall != "Unknown-Wall" &
  fac_fuel != "Unknown-Fuel"
]

# Stage 4: make-model primary (fully classified, non-unknown)
fac_stage4 <- annual_data[
  !is.na(make_model_fac) &
  fac_wall != "Unknown-Wall" &
  fac_fuel != "Unknown-Fuel" &
  !is.na(fac_oldest_age_bin)
]

stage_labels <- c(
  "0. Full analysis sample",
  "1. + Has reform-date cell classification",
  "2. + Exclude Unknown-Wall",
  "3. + Exclude Unknown-Fuel",
  "4. Make-Model primary sample\n(mm_fac_primary)"
)

log_cat("  Computing balance at each stage (Wald tests may take a moment)...\n")
balance_tbl <- rbindlist(list(
  compute_balance(fac_stage0, stage_labels[1]),
  compute_balance(fac_stage1, stage_labels[2]),
  compute_balance(fac_stage2, stage_labels[3]),
  compute_balance(fac_stage3, stage_labels[4]),
  compute_balance(fac_stage4, stage_labels[5])
))
balance_tbl[, Step := 0:4]

print(balance_tbl[, .(Step, N_TX, N_CTL, Age_Gap, SW_Gap, Wald_PreTrend_p)])
save_table(balance_tbl, "TableB2_Balance_By_Step")

# ─────────────────────────────────────────────────────────────────────────────
# Panel A: Attrition waterfall
# ─────────────────────────────────────────────────────────────────────────────
log_cat("\n--- Figure: Attrition Waterfall ---\n")

attrition_dt <- balance_tbl[, .(
  Step, Stage,
  N_TX, N_CTL,
  Total = N_TX + N_CTL
)]
attrition_long <- melt(
  attrition_dt[, .(Step, Stage = gsub("\n", " ", Stage),
                   Texas = N_TX, Control = N_CTL)],
  id.vars = c("Step", "Stage"),
  variable.name = "Group", value.name = "N_Facilities"
)
attrition_long[, Stage := factor(Stage,
  levels = rev(gsub("\n", " ", stage_labels)))]

p_funnel_A <- ggplot(attrition_long,
                     aes(y = Stage, x = N_Facilities, fill = Group)) +
  geom_col(position = position_dodge(width = 0.7),
           width = 0.65, alpha = 0.9) +
  geom_text(aes(label = comma(N_Facilities)),
            position = position_dodge(width = 0.7),
            hjust = -0.1, size = 3.0, fontface = "bold") +
  scale_fill_manual(values = COL_PAIR) +
  scale_x_continuous(labels = comma_format(),
                     expand  = expansion(mult = c(0, 0.18))) +
  labs(
    title    = "A: Sample Attrition Through Restriction Steps",
    subtitle = "Each row removes one additional restriction. TX/CTL counts remain balanced.",
    x = "Number of Facilities", y = NULL, fill = NULL
  ) +
  theme(panel.grid.major.y = element_blank())

# ─────────────────────────────────────────────────────────────────────────────
# Panel B: Balance metrics by step
# ─────────────────────────────────────────────────────────────────────────────
log_cat("\n--- Figure: Balance By Step ---\n")

balance_long <- melt(
  balance_tbl[, .(Step,
    `Oldest-Tank Age Gap (TX-CTL, yrs)` = Age_Gap,
    `All-SW Share Gap (TX-CTL)`         = SW_Gap * 100,
    `Pre-Trend Wald p`                  = Wald_PreTrend_p)],
  id.vars    = "Step",
  variable.name = "Metric", value.name = "Value"
)
balance_long[, Stage_label := stage_labels[Step + 1]]

p_funnel_B <- ggplot(balance_long,
                     aes(x = Step, y = Value, group = Metric)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_hline(data = balance_long[Metric == "Pre-Trend Wald p"],
             aes(yintercept = 0.10), linetype = "dotted",
             color = "goldenrod3", linewidth = 0.7) +
  geom_line(aes(color = Metric), linewidth = 0.9) +
  geom_point(aes(color = Metric, shape = Metric), size = 3) +
  geom_text(aes(label = round(Value, 3)),
            vjust = -0.7, size = 2.5) +
  scale_x_continuous(breaks = 0:4,
    labels = paste0("Step ", 0:4)) +
  scale_color_manual(
    values = c("Oldest-Tank Age Gap (TX-CTL, yrs)" = "#E69F00",
               "All-SW Share Gap (TX-CTL)"         = "#56B4E9",
               "Pre-Trend Wald p"                  = "#009E73")) +
  scale_shape_manual(
    values = c("Oldest-Tank Age Gap (TX-CTL, yrs)" = 16,
               "All-SW Share Gap (TX-CTL)"         = 17,
               "Pre-Trend Wald p"                  = 15)) +
  facet_wrap(~ Metric, scales = "free_y", ncol = 1) +
  labs(
    title    = "B: Balance and Pre-Trend Quality by Restriction Step",
    subtitle = paste0("Dotted gold = p = 0.10 threshold. ",
                      "Restrictions should not worsen balance or p-value."),
    x = "Restriction Step", y = "Metric Value",
    color = NULL, shape = NULL
  ) +
  theme(legend.position = "none")

save_panels(
  panels          = list(A = p_funnel_A, B = p_funnel_B),
  base_name       = "Figure_Funnel",
  combined_name   = "Figure_Funnel_Combined",
  panel_width     = 9, panel_height = 5,
  combined_width  = 18, combined_height = 5,
  ncol            = 2,
  title    = "Sample Restriction Funnel: Attrition and Balance at Each Step",
  subtitle = paste0("Make-model primary sample builds incrementally through classification steps. ",
                    "Right panel shows restrictions improve or maintain pre-trend quality.")
)

# LaTeX table
write_tex(
  kbl(balance_tbl[, .(Stage, N_TX, N_CTL,
                       `Age Gap`  = Age_Gap,
                       `All-SW Gap` = SW_Gap,
                       `Wald p`   = Wald_PreTrend_p)],
      format = "latex", booktabs = TRUE, linesep = "", escape = FALSE,
      caption = paste0(
        "Sample restriction funnel. Each row adds one restriction to the ",
        "make-model primary sample. \\textit{Age Gap} = mean oldest-tank age ",
        "TX $-$ CTL (years) at reform. \\textit{All-SW Gap} = All-SW share ",
        "TX $-$ CTL. \\textit{Wald p} = p-value from parallel pre-trend ",
        "F-test using make-model cell-by-year FE."),
      label = "tab:funnel-balance") |>
    kable_styling(latex_options = c("scale_down", "hold_position"), font_size = 9),
  "TableB2_Balance_By_Step"
)

log_cat("=== 01h COMPLETE ===\n")
close_log("01h_SampleRestrictionFunnel")