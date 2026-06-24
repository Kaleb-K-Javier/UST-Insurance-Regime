# 04x_Binscatter_Fix.R
# Re-draw the tank-count vs capacity-change bin scatter from the saved joint table,
# with a y-range that shows EVERY median point (the old figure clipped +3/+4/+5).
# Source: Output/Tables/T011_A7_TankVsCapacity_Joint.csv

suppressPackageStartupMessages({ library(data.table); library(ggplot2); library(here) })

d <- fread(here("Output","Tables","T011_A7_TankVsCapacity_Joint.csv"))
lev <- c("<=-5","-4","-3","-2","-1","0","+1","+2","+3","+4",">=+5")
d[, bin := factor(delta_tanks_bin, levels = lev)]
d <- d[!is.na(bin)]

ymax <- max(d$median_cap_pct) + 40      # include every median point
ymin <- min(d$p25_cap_pct, d$median_cap_pct) - 20

p <- ggplot(d, aes(x = bin, y = median_cap_pct)) +
  geom_hline(yintercept = 0,   color = "gray40") +
  geom_hline(yintercept = -10, color = "gray60", linetype = "dashed") +
  geom_vline(xintercept = which(lev == "0"), color = "gray70", linetype = "dotted") +
  geom_linerange(aes(ymin = pmax(p25_cap_pct, ymin), ymax = pmin(p75_cap_pct, ymax)),
                 color = "#8B1A1A", alpha = 0.45, linewidth = 0.8) +
  geom_point(aes(size = n_events), color = "#8B1A1A") +
  geom_text(aes(label = sprintf("%.0f%%", median_cap_pct)),
            vjust = -0.9, size = 2.7, color = "gray25") +
  coord_cartesian(ylim = c(ymin, ymax)) +
  scale_size_area(max_size = 9, guide = "none") +
  labs(x = "Change in tank count at the event",
       y = "Capacity % change (median; bars = p25-p75)",
       title = "Tank-count change vs. capacity change at \"replace\" events",
       subtitle = "Median capacity change rises monotonically with tank-count change; point area = number of events") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(color = "grey30"),
        panel.grid.minor = element_blank())

ggsave(here("Output","Figures","04x_TankVsCapacity_BinScatter.png"), p, width = 10, height = 5.6, dpi = 150)
cat("  saved 04x_TankVsCapacity_BinScatter.png\n=== 04x DONE ===\n")
