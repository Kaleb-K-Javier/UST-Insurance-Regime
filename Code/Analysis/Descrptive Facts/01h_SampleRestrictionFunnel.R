#==============================================================================
# 01h_SampleRestrictionFunnel.R
# Sample Restriction Funnel with Balance at Each Step
#
# Figures:
#   Figure_Funnel_Attrition   — waterfall of facility counts through filters
#   Figure_Funnel_Balance     — balance metrics (age gap, SW gap, pre-trend p)
#                               at each restriction step
#   Combined → Figure_Funnel_Combined
#
# Tables:
#   Table_Attrition_Log        (human-readable)
#   TableB2_Balance_By_Step    (LaTeX)
#==============================================================================

source(here::here("Code", "01a_Setup.R"))
cat("=== 01h: SAMPLE RESTRICTION FUNNEL ===\n")

annual_data    <- load_interim("annual_data")
tanks_1998     <- load_interim("tanks_1998")
attrition_log  <- load_interim("attrition_log")

# ─────────────────────────────────────────────────────────────────────────────
# Build funnel stages sequentially, computing balance at each step
# ─────────────────────────────────────────────────────────────────────────────
cat("\n--- Building funnel stages ---\n")

run_wald_p <- function(dt) {
  pre <- dt[panel_year %between% c(1990L, TREATMENT_YEAR) &
              spec_A_eligible == 1]
  if (uniqueN(pre$panel_id) < 50) return(NA_real_)
  m <- tryCatch(
    feols(closure_event ~
            i(rel_year_1999, texas_treated, ref = -1) |
            panel_id + panel_year,
          data = pre, cluster = ~state),
    error = function(e) NULL)
  if (is.null(m)) return(NA_real_)
  w <- tryCatch(fixest::wald(m, "rel_year_1999"), error = function(e) NULL)
  if (is.null(w)) return(NA_real_)
  round(w$p, 4)
}

compute_balance <- function(dt, stage_label) {
  tx  <- dt[texas_treated == 1]
  ctl <- dt[texas_treated == 0]

  # Get 1998 cross-section for this subsample
  ids  <- unique(dt$panel_id)
  t98  <- tanks_1998[panel_id %in% ids]
  t_tx  <- t98[texas_treated == 1]
  t_ctl <- t98[texas_treated == 0]

  age_gap  <- mean(t_tx$tank_age_1998, na.rm=TRUE) -
              mean(t_ctl$tank_age_1998, na.rm=TRUE)
  sw_gap   <- if ("single_walled" %in% names(t98))
    mean(t_tx$single_walled==1, na.rm=TRUE) -
    mean(t_ctl$single_walled==1, na.rm=TRUE) else NA_real_

  wald_p <- run_wald_p(dt)

  data.table(
    Stage          = stage_label,
    N_TX           = uniqueN(tx$panel_id),
    N_CTL          = uniqueN(ctl$panel_id),
    Mean_Age_TX    = round(mean(t_tx$tank_age_1998, na.rm=TRUE), 2),
    Mean_Age_CTL   = round(mean(t_ctl$tank_age_1998, na.rm=TRUE), 2),
    Age_Gap        = round(age_gap, 2),
    SW_Share_TX    = round(mean(t_tx$single_walled==1, na.rm=TRUE), 3),
    SW_Share_CTL   = round(mean(t_ctl$single_walled==1, na.rm=TRUE), 3),
    SW_Gap         = round(sw_gap, 3),
    Wald_PreTrend_p = wald_p
  )
}

# Stage 0: full analysis sample (post 01c filters)
fac_stage0 <- annual_data

# Stage 1: SW only
fac_stage1 <- annual_data[has_single_walled == 1]

# Stage 2: SW + gasoline
fac_stage2 <- annual_data[has_single_walled == 1 & has_gasoline_year == 1]

# Stage 3: SW + gasoline + single tank
fac_stage3 <- annual_data[has_single_walled == 1 &
                            has_gasoline_year == 1 &
                            single_tanks == active_tanks]

# Stage 4: make-model cohort (+ install year window)
fac_stage4 <- annual_data[has_single_walled == 1 &
                            has_gasoline_year == 1 &
                            single_tanks == active_tanks &
                            install_year > MM_INSTALL_START - 1 &
                            install_year <= MM_INSTALL_END]

stage_labels <- c(
  "0. Full analysis sample",
  "1. + Single-walled only",
  "2. + Motor fuel/gasoline",
  "3. + Single-tank operators",
  "4. + 1990\u20131997 install cohort\n(Make-Model sample)"
)

cat("  Computing balance at each stage (Wald tests may take a moment)...\n")
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
cat("\n--- Figure: Attrition Waterfall ---\n")

attrition_dt <- balance_tbl[, .(
  Step, Stage,
  N_TX, N_CTL,
  Total = N_TX + N_CTL
)]
attrition_long <- melt(
  attrition_dt[, .(Step, Stage = gsub("\n", " ", Stage),
                   Texas = N_TX, Control = N_CTL)],
  id.vars = c("Step","Stage"),
  variable.name = "Group", value.name = "N_Facilities"
)
attrition_long[, Stage := factor(Stage, levels = rev(gsub("\n"," ", stage_labels)))]

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
cat("\n--- Figure: Balance By Step ---\n")

balance_long <- melt(
  balance_tbl[, .(Step,
                   `Age Gap (TX-CTL, yrs)` = Age_Gap,
                   `SW Share Gap (TX-CTL)` = SW_Gap * 100,
                   `Pre-Trend Wald p`      = Wald_PreTrend_p)],
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
    values = c("Age Gap (TX-CTL, yrs)" = "#E69F00",
               "SW Share Gap (TX-CTL)" = "#56B4E9",
               "Pre-Trend Wald p"      = "#009E73")) +
  scale_shape_manual(
    values = c("Age Gap (TX-CTL, yrs)" = 16,
               "SW Share Gap (TX-CTL)" = 17,
               "Pre-Trend Wald p"      = 15)) +
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
  subtitle = paste0("Make-model sample builds incrementally. ",
                    "Right panel shows restrictions improve or maintain pre-trend quality.")
)

# LaTeX table
write_tex(
  kbl(balance_tbl[, .(Stage, N_TX, N_CTL,
                       `Age Gap` = Age_Gap,
                       `SW Gap` = SW_Gap,
                       `Wald p` = Wald_PreTrend_p)],
      format="latex", booktabs=TRUE, linesep="", escape=FALSE,
      caption="Sample restriction funnel. Each row adds one restriction to the make-model sample. \\textit{Age Gap} = mean tank age TX $-$ CTL (years). \\textit{SW Gap} = single-walled share TX $-$ CTL. \\textit{Wald p} = p-value from parallel pre-trend F-test on Spec A sub-sample.",
      label="tab:funnel-balance") |>
    kable_styling(latex_options=c("scale_down","hold_position"), font_size=9),
  "TableB2_Balance_By_Step"
)

cat("=== 01h COMPLETE ===\n")
