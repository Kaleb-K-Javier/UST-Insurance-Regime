# 04ad_ModelFit_byAction.R
# One model-fit figure per action (maintain / exit / replace): model-implied CCP
# (line) vs empirical share (points), faceted by wall x regime. y-axis = percent
# to 2 decimal places (e.g. 1.68%, 0.27%, 97.69%). Source: canonical per-cell fit
# table 04o_PerCell_Fit_6p_Wide.csv.

suppressPackageStartupMessages({ library(data.table); library(ggplot2); library(here); library(scales) })
pc <- fread(here("Output","Tables","04o_PerCell_Fit_6p_Wide.csv"))
pc[, regime := factor(regime, levels = c("FF","RB"))]
pc[, wall   := factor(wall,   levels = c("SW","DW"))]
age_labs <- c("0-5","5-10","10-15","15-20","20-25","25-30","30-35","35+")

fit_fig <- function(act, lab, fname) {
  d <- pc[, .(age_bin, wall, regime, n_cell,
              model = get(paste0("model_", act)), emp = get(paste0("emp_", act)))]
  p <- ggplot(d, aes(x = age_bin)) +
    geom_line(aes(y = model, color = "Model-implied"), linewidth = 0.9) +
    geom_point(aes(y = emp, color = "Empirical", size = n_cell), alpha = 0.8) +
    facet_grid(wall ~ regime, scales = "free_y") +
    scale_x_continuous(breaks = 1:8, labels = age_labs) +
    scale_y_continuous(labels = percent_format(accuracy = 0.01)) +
    scale_color_manual(values = c("Model-implied" = "#003262", "Empirical" = "#C1272D")) +
    scale_size_area(max_size = 5, guide = "none") +
    labs(x = "Tank age", y = sprintf("P(%s | state)", lab), color = NULL,
         title = sprintf("Model fit --- %s: model-implied vs. empirical", lab),
         subtitle = "Line = 6p model-implied CCP; points = empirical share (sized by cell N); by wall x regime") +
    theme_minimal(base_size = 12) +
    theme(legend.position = "top", plot.title = element_text(face = "bold"),
          plot.subtitle = element_text(color = "grey30"), strip.text = element_text(face = "bold"),
          axis.text.x = element_text(angle = 45, hjust = 1))
  ggsave(here("Output","Figures", fname), p, width = 9.5, height = 6, dpi = 150)
  cat(sprintf("  saved %s\n", fname))
}
fit_fig("maintain", "Maintain", "04ad_ModelFit_Maintain.png")
fit_fig("exit",     "Exit",     "04ad_ModelFit_Exit.png")
fit_fig("replace",  "Replace",  "04ad_ModelFit_Replace.png")
cat("=== 04ad DONE ===\n")
