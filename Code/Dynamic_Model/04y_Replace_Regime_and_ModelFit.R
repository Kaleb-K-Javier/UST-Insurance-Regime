# 04y_Replace_Regime_and_ModelFit.R
# Two replace-focused appendix figures:
#  (A) Replace decomposition by REGIME (FF vs RB, NO size split): model's replace
#      vs true upgrade vs shrink, empirical, colored by regime.
#  (B) Model-implied vs empirical replace CCP, by age x (wall, regime): does the
#      6p fit reproduce the empirical replace pattern? (from the per-cell fit table)

suppressPackageStartupMessages({ library(data.table); library(ggplot2); library(here) })
theme_set(theme_minimal(base_size = 12) +
  theme(legend.position = "top", plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(color = "grey30"), strip.text = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1)))
age_labs <- c("0-5","5-10","10-15","15-20","20-25","25-30","30-35","35+")
reg_pal  <- c(FF = "#E76F51", RB = "#2A9D8F")

# ---------------- (A) regime decomposition (empirical) ----------------
obs <- fread(here("Data","Analysis","dcm_obs_panel_observed.csv"),
             select = c("panel_id","panel_year","A_bin","rho_state","y_it","I_replace"))
fp  <- fread(here("Data","Analysis","facility_panel.csv"),
             select = c("panel_id","panel_year","any_closure","facility_complete_closure",
                        "replacement_closure_year"))
m <- merge(obs, fp, by = c("panel_id","panel_year"), all.x = FALSE)
for (c0 in c("any_closure","facility_complete_closure","replacement_closure_year")) m[is.na(get(c0)), (c0) := 0L]
m[, regime := ifelse(rho_state == 2L, "RB", "FF")]

# the three closure events, by where the model puts them
agg <- m[, .(n = .N,
             replace_upg = mean(y_it == 1L & I_replace == 1L),                                   # -> model Replace
             full_exit   = mean(y_it == 1L & I_replace == 0L),                                   # -> model Exit
             downsizing  = mean(any_closure == 1L & facility_complete_closure == 0L & replacement_closure_year == 0L)  # -> hidden in Maintain
            ), by = .(A_bin, regime)]
long <- melt(agg, id.vars = c("A_bin","regime","n"),
             measure.vars = c("replace_upg","full_exit","downsizing"),
             variable.name = "defn", value.name = "p")
long[, defn := factor(defn, levels = c("replace_upg","full_exit","downsizing"),
       labels = c("Upgrade  ->  model Replace","Full exit  ->  model Exit","Downsizing (kept operating)  ->  hidden in Maintain"))]
pA <- ggplot(long, aes(A_bin, p, color = regime)) +
  geom_line(aes(group = regime), linewidth = 0.8) + geom_point(aes(size = n), alpha = 0.85) +
  facet_wrap(~ defn, nrow = 1, scales = "free_y") +
  scale_x_continuous(breaks = 1:8, labels = age_labs) +
  scale_color_manual(values = reg_pal) + scale_size_area(max_size = 6, guide = "none") +
  labs(x = "Tank age", y = "Empirical probability", color = "Regime",
       title = "The three closure events by regime, and where the model puts them",
       subtitle = "Under RB the old-tank jump is mostly full exit (captured as Exit); downsizing (hidden in Maintain) is the smaller, uncaptured margin")
ggsave(here("Output","Figures","04y_Replace_Decomp_byRegime.png"), pA, width = 11.5, height = 4.3, dpi = 150)
cat("  saved 04y_Replace_Decomp_byRegime.png\n")

# ---------------- (B) model-implied vs empirical replace CCP ----------------
pc <- fread(here("Output","Tables","04o_PerCell_Fit_6p_Wide.csv"))
pc[, regime := factor(regime, levels = c("FF","RB"))]
pc[, wall := factor(wall, levels = c("SW","DW"))]
pB <- ggplot(pc, aes(x = age_bin)) +
  geom_line(aes(y = model_replace, color = "Model-implied"), linewidth = 0.9) +
  geom_point(aes(y = emp_replace, color = "Empirical", size = n_cell), alpha = 0.8) +
  facet_grid(wall ~ regime, scales = "free_y") +
  scale_x_continuous(breaks = 1:8, labels = age_labs) +
  scale_color_manual(values = c("Model-implied" = "#003262", "Empirical" = "#C1272D")) +
  scale_size_area(max_size = 5, guide = "none") +
  labs(x = "Tank age", y = "P(replace | state)", color = NULL,
       title = "Replace: does the 6p model capture the empirical pattern?",
       subtitle = "Model-implied CCP (line) vs empirical share (points, sized by cell N), by wall x regime")
ggsave(here("Output","Figures","04y_Replace_ModelFit.png"), pB, width = 10, height = 6, dpi = 150)
cat("  saved 04y_Replace_ModelFit.png\n=== 04y DONE ===\n")
