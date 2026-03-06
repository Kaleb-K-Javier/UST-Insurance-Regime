#==============================================================================
# 01j_AgeHeterogeneity.R
# Age Heterogeneity Documentation (Make-Model Cohort)
#
# Key purpose: establish the WITHIN-COHORT age imbalance BEFORE the DiD section.
# TX is systematically younger within the 1990-1997 install cohort because
# it installed more tanks later in the mandate window (1994-1997).
# This is descriptive, not a robustness check.
#
# Figures:
#   Figure5_AgeDist_MakeModel:
#     PanelA — Age histogram at Dec 1998, TX vs Control (make-model only)
#     PanelB — Age ECDF (make-model only) with KS stat
#     PanelC — Age distribution by install cohort (bar, TX vs CTL)
#     Combined → Figure5_AgeDist_MakeModel_Combined
#
#   Figure_Survival_1990_1997:
#     PanelA — Kaplan-Meier survival of 1990-1997 cohort, TX vs CTL (raw data)
#     PanelB — Cumulative incidence (1 - S(t)), same cohort
#     Combined → Figure_Survival_1990_1997_Combined
#
#   Figure_AgeAtClosure (also referenced from 02_DiD H3 section):
#     PanelA — Age at closure distribution, TX vs Control (closed tanks)
#     PanelB — Age at closure CDF
#     Combined → Figure_AgeAtClosure_Combined
#
# Saves:
#   km_1990_1997.rds → used by 02_DiD for H3 contextual figure
#==============================================================================

source(here::here("Code", "01a_Setup.R"))
cat("=== 01j: AGE HETEROGENEITY ===\n")

tanks_1998   <- load_interim("tanks_1998")
closed_tanks <- load_interim("closed_tanks")
annual_data  <- load_interim("annual_data")

# Make-model tanks at 1998
mm_tanks_1998 <- tanks_1998[
  single_walled == 1 &
  state %in% c("TX", CONTROL_STATES) &
  !is.na(install_year) &
  install_year > MM_INSTALL_START - 1 &
  install_year <= MM_INSTALL_END
]
mm_tanks_1998[, Group := fifelse(state == "TX", "Texas", "Control")]

# Narrow install cohorts for Panel C (2-year bins within 1990-1997)
mm_tanks_1998[, install_bin := cut(install_year,
  breaks = c(1989, 1991, 1993, 1995, 1997),
  labels = c("1990\u201391","1992\u201393","1994\u201395","1996\u201397"),
  right  = TRUE)]

# ─────────────────────────────────────────────────────────────────────────────
# Figure 5: Age Distribution in Make-Model Sample
# ─────────────────────────────────────────────────────────────────────────────
cat("\n--- Figure 5: Age Distribution Make-Model ---\n")

mean_age_mm <- mm_tanks_1998[, .(mean_age = mean(tank_age_1998, na.rm=TRUE)),
                               by = Group]

p5_A <- ggplot(mm_tanks_1998[!is.na(tank_age_1998)],
               aes(x = tank_age_1998, fill = Group, color = Group)) +
  geom_density(alpha = 0.30, adjust = 1.3, linewidth = 0.8) +
  geom_vline(data = mean_age_mm,
             aes(xintercept = mean_age, color = Group),
             linetype = "dashed", linewidth = 0.8) +
  scale_fill_manual(values  = COL_PAIR) +
  scale_color_manual(values = COL_PAIR) +
  scale_x_continuous(breaks = seq(0, 10, 2)) +
  labs(
    title    = "A: Tank Age at Dec 1998 (Make-Model Cohort)",
    subtitle = "1990\u20131997 install cohort only. TX skews younger: installed later in cohort window.",
    x = "Tank Age (years)", y = "Density",
    fill = NULL, color = NULL
  )

p5_B <- ggplot(mm_tanks_1998[!is.na(tank_age_1998)],
               aes(x = tank_age_1998, color = Group)) +
  stat_ecdf(linewidth = 0.9, geom = "step") +
  scale_color_manual(values = COL_PAIR) +
  scale_x_continuous(breaks = seq(0, 10, 2)) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title    = "B: Cumulative Age Distribution (ECDF) — Make-Model Cohort",
    subtitle = "Vertical gap between lines = share of tanks in that age range with different TX/CTL composition.",
    x = "Tank Age (years)", y = "Cumulative Share",
    color = NULL
  )

# Bar chart: share of tanks by install bin × group
install_share <- mm_tanks_1998[!is.na(install_bin), .(N = .N),
                                by = .(install_bin, Group)]
install_share[, share := N / sum(N), by = Group]

p5_C <- ggplot(install_share,
               aes(x = install_bin, y = share, fill = Group)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.65, alpha = 0.9) +
  geom_text(aes(label = percent(share, accuracy=0.1)),
            position = position_dodge(width = 0.7),
            vjust = -0.4, size = 3) +
  scale_fill_manual(values = COL_PAIR) +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     limits = c(0, 0.50)) +
  labs(
    title    = "C: Install Sub-Cohort Composition (Make-Model Sample)",
    subtitle = "TX disproportionately installed tanks in 1994\u201397 (younger at treatment). Source of within-cohort age imbalance.",
    x = "Install Sub-Cohort", y = "Share of Group's Tanks",
    fill = NULL
  )

save_panels(
  panels          = list(A = p5_A, B = p5_B, C = p5_C),
  base_name       = "Figure5_AgeDist_MakeModel",
  combined_name   = "Figure5_AgeDist_MakeModel_Combined",
  panel_width     = 8, panel_height = 5,
  combined_width  = 24, combined_height = 5,
  ncol            = 3,
  title    = "Figure 5: Within-Cohort Age Imbalance in Make-Model Sample (1990\u20131997 Install Cohort)",
  subtitle = "TX younger by construction: heavier installation in later cohort sub-years motivates age heterogeneity tests (H1/H2)."
)

# ─────────────────────────────────────────────────────────────────────────────
# Figure: Kaplan-Meier Survival (1990–1997 cohort)
# ─────────────────────────────────────────────────────────────────────────────
cat("\n--- Figure: Kaplan-Meier Survival ---\n")

# Build survival data: use closed_tanks + tanks_1998 to determine entry/exit
mm_surv_tanks <- tanks_1998[
  !is.na(install_year) &
  install_year > MM_INSTALL_START - 1 &
  install_year <= MM_INSTALL_END &
  single_walled == 1 &
  state %in% c("TX", CONTROL_STATES)
]

mm_surv_tanks[, `:=`(
  Group        = fifelse(state == "TX", "Texas", "Control"),
  entry_date   = tank_installed_date,
  ref_date     = TREATMENT_DATE
)]

# Merge closure dates
closure_dates <- closed_tanks[, .(panel_id, tank_id = tank_id_harmonized,
                                   tank_closed_date)]
if (!"tank_id_harmonized" %in% names(closed_tanks)) {
  closure_dates <- closed_tanks[, .(panel_id, tank_closed_date)]
  mm_surv_tanks <- merge(mm_surv_tanks, closure_dates,
                         by = "panel_id", all.x = TRUE)
} else {
  mm_surv_tanks <- merge(mm_surv_tanks,
                         closed_tanks[, .(panel_id, tank_id_harmonized,
                                           tank_closed_date)],
                         by = c("panel_id","tank_id_harmonized"), all.x = TRUE)
}

mm_surv_tanks[, `:=`(
  time_since_install = as.numeric(
    fifelse(!is.na(tank_closed_date),
            tank_closed_date - tank_installed_date,
            as.IDate(paste0(PANEL_END, "-12-31")) - tank_installed_date)
  ) / 365.25,
  event_closure = as.integer(!is.na(tank_closed_date) &
                               tank_closed_date <= as.IDate(paste0(PANEL_END,"-12-31")))
)]
mm_surv_tanks <- mm_surv_tanks[time_since_install > 0]

# Kaplan-Meier fit
km_fit <- survfit(
  Surv(time_since_install, event_closure) ~ Group,
  data = mm_surv_tanks
)
km_dt <- as.data.table(broom::tidy(km_fit))
km_dt[, Group := gsub("Group=", "", strata)]

p_km_A <- ggplot(km_dt, aes(x = time, y = estimate, color = Group)) +
  geom_step(linewidth = 0.9) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = Group),
              alpha = 0.12, color = NA, stat = "identity") +
  geom_vline(xintercept = as.numeric(TREATMENT_DATE -
               as.IDate(paste0(MM_INSTALL_END,"-12-31"))) / 365.25,
             linetype = "dashed", color = "gray30", linewidth = 0.7) +
  annotate("text", x = 9, y = 0.95, hjust = 0, size = 2.6, color = "gray30",
           label = "Dec 1998\nreform") +
  scale_color_manual(values = COL_PAIR) +
  scale_fill_manual(values  = COL_PAIR) +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     limits = c(0, 1)) +
  labs(
    title    = "A: Kaplan-Meier Survival — 1990\u20131997 Install Cohort",
    subtitle = "Time axis = years since installation. Event = closure. Ribbon = 95% CI.",
    x = "Years Since Installation", y = "Survival Probability (tank still active)",
    color = NULL, fill = NULL
  )

p_km_B <- ggplot(km_dt, aes(x = time, y = 1 - estimate, color = Group)) +
  geom_step(linewidth = 0.9) +
  geom_ribbon(aes(ymin = 1 - conf.high, ymax = 1 - conf.low, fill = Group),
              alpha = 0.12, color = NA, stat = "identity") +
  scale_color_manual(values = COL_PAIR) +
  scale_fill_manual(values  = COL_PAIR) +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     limits = c(0, 1)) +
  labs(
    title    = "B: Cumulative Closure Probability — 1990\u20131997 Cohort",
    subtitle = "1 - S(t). Steeper TX curve post-reform = treatment effect.",
    x = "Years Since Installation", y = "Cumulative Closure Probability",
    color = NULL, fill = NULL
  )

save_panels(
  panels          = list(A = p_km_A, B = p_km_B),
  base_name       = "Figure_Survival_1990_1997",
  combined_name   = "Figure_Survival_1990_1997_Combined",
  panel_width     = 9, panel_height = 5,
  combined_width  = 18, combined_height = 5,
  ncol            = 2,
  title    = "Kaplan-Meier Survival: 1990\u20131997 Single-Walled Install Cohort",
  subtitle = "Raw data, no fixed effects. Provides context for DiD closure rate estimates."
)

# Save KM object for 02_DiD
save_interim(list(km_fit = km_fit, km_dt = km_dt,
                   mm_surv_tanks = mm_surv_tanks),
             "km_1990_1997")

# ─────────────────────────────────────────────────────────────────────────────
# Figure: Age at Closure (→ H3 in 02_DiD)
# ─────────────────────────────────────────────────────────────────────────────
cat("\n--- Figure: Age at Closure ---\n")

# Restrict to make-model cohort
mm_closed <- closed_tanks[
  state %in% c("TX", CONTROL_STATES) &
  !is.na(install_year) &
  install_year > MM_INSTALL_START - 1 &
  install_year <= MM_INSTALL_END &
  single_walled == 1
]
mm_closed[, Group := fifelse(state == "TX", "Texas", "Control")]

mean_closure_age <- mm_closed[, .(mean_cl_age = mean(age_at_closure, na.rm=TRUE)),
                               by = Group]

p_cl_A <- ggplot(mm_closed[!is.na(age_at_closure)],
                 aes(x = age_at_closure, fill = Group, color = Group)) +
  geom_density(alpha = 0.28, adjust = 1.3, linewidth = 0.8) +
  geom_vline(data = mean_closure_age,
             aes(xintercept = mean_cl_age, color = Group),
             linetype = "dashed", linewidth = 0.8) +
  scale_fill_manual(values  = COL_PAIR) +
  scale_color_manual(values = COL_PAIR) +
  labs(
    title    = "A: Age at Closure Density (Make-Model Cohort)",
    subtitle = "Dashed = group mean. TX closing younger (post-reform acceleration of young-tank removal).",
    x = "Tank Age at Closure (years)", y = "Density",
    fill = NULL, color = NULL
  )

p_cl_B <- ggplot(mm_closed[!is.na(age_at_closure)],
                 aes(x = age_at_closure, color = Group)) +
  stat_ecdf(linewidth = 0.9, geom = "step") +
  scale_color_manual(values = COL_PAIR) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title    = "B: Age at Closure — ECDF (Make-Model Cohort)",
    subtitle = "TX ECDF above CTL = TX closes at younger ages.",
    x = "Tank Age at Closure (years)", y = "Cumulative Share",
    color = NULL
  )

save_panels(
  panels          = list(A = p_cl_A, B = p_cl_B),
  base_name       = "Figure_AgeAtClosure",
  combined_name   = "Figure_AgeAtClosure_Combined",
  panel_width     = 8, panel_height = 5,
  combined_width  = 16, combined_height = 5,
  ncol            = 2,
  title    = "Age at Closure: Make-Model Cohort (1990\u20131997 Single-Walled)",
  subtitle = "Closures in any year. Provides descriptive context for H3 (age-at-closure DiD in 02_DiD)."
)

save_interim(mm_closed, "mm_closed_tanks_for_H3")

cat("=== 01j COMPLETE ===\n")
