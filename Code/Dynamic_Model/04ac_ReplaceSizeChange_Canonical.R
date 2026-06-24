# 04ac_ReplaceSizeChange_Canonical.R
# Recompute (from the canonical facility_panel, NOT old T011 outputs) the size
# change at upgrade/"replace" events: tank-count and capacity change, and their
# joint bin scatter. Replaces T011_A6 (histogram) and T011_A7 / 04x (bin scatter).

suppressPackageStartupMessages({ library(data.table); library(ggplot2); library(here) })
cat("=== 04ac REPLACE SIZE CHANGE (canonical) ===\n")

fp <- fread(here("Data","Analysis","facility_panel.csv"),
            select = c("replacement_closure_year","net_tank_change","capacity_change","lag_capacity"))
r <- fp[replacement_closure_year == 1L & !is.na(net_tank_change)]
cat(sprintf("upgrade/replace events: %d\n", nrow(r)))
r[, cap_pct := ifelse(lag_capacity > 0, 100 * capacity_change / lag_capacity, NA_real_)]

thm <- theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"), plot.subtitle = element_text(color = "grey30"),
        panel.grid.minor = element_blank())

# ---- (1) two-panel histogram: tank-count change and capacity change ----
rt <- r[, .(tb = pmin(pmax(net_tank_change, -5L), 5L))]
ph_t <- ggplot(rt, aes(tb)) +
  geom_bar(fill = "#4C78A8", width = 0.9) +
  geom_vline(xintercept = 0, color = "gray30") +
  scale_x_continuous(breaks = -5:5, labels = c("<=-5",-4:4,"5+")) +
  labs(x = "Change in tank count", y = "Replace events",
       title = "At \"replace\" events, facilities mostly shed tanks",
       subtitle = sprintf("Tank-count change at the %s upgrade events (canonical facility panel)", format(nrow(r), big.mark=","))) + thm
ggsave(here("Output","Figures","04ac_ReplaceTankChange_Hist.png"), ph_t, width = 9, height = 4.6, dpi = 150)
cat("  saved 04ac_ReplaceTankChange_Hist.png\n")

# ---- (2) bin scatter: capacity % change vs tank-count change ----
rb <- r[!is.na(cap_pct)]
rb[, tb := pmin(pmax(net_tank_change, -5L), 5L)]
agg <- rb[, .(n = .N, med = median(cap_pct), p25 = quantile(cap_pct, .25), p75 = quantile(cap_pct, .75)), by = tb][order(tb)]
ymax <- max(agg$med) + 40; ymin <- min(agg$p25, agg$med) - 20
p_bs <- ggplot(agg, aes(tb, med)) +
  geom_hline(yintercept = 0, color = "gray40") +
  geom_linerange(aes(ymin = pmax(p25, ymin), ymax = pmin(p75, ymax)), color = "#8B1A1A", alpha = 0.45, linewidth = 0.9) +
  geom_point(aes(size = n), color = "#8B1A1A") +
  geom_text(aes(label = sprintf("%.0f%%", med)), vjust = -0.9, size = 2.8, color = "gray25") +
  coord_cartesian(ylim = c(ymin, ymax)) +
  scale_x_continuous(breaks = -5:5, labels = c("<=-5",-4:4,"5+")) +
  scale_size_area(max_size = 9, guide = "none") +
  labs(x = "Change in tank count at the event", y = "Capacity % change (median; bars p25-p75)",
       title = "Tank-count change vs. capacity change at \"replace\" events",
       subtitle = "Losing one tank holds capacity roughly flat (consolidation); losing several cuts capacity (true shrink)") + thm
ggsave(here("Output","Figures","04ac_ReplaceTankVsCapacity_BinScatter.png"), p_bs, width = 10, height = 5.4, dpi = 150)
cat("  saved 04ac_ReplaceTankVsCapacity_BinScatter.png\n")

# ---- (3) capacity-change histogram at replace events (accompanies the tank-change one) ----
rc <- r[!is.na(capacity_change), .(capk = pmin(pmax(capacity_change, -20000), 20000) / 1000)]
ph_c <- ggplot(rc, aes(capk)) +
  geom_histogram(bins = 31, fill = "#E76F51", color = "white", linewidth = 0.1) +
  geom_vline(xintercept = 0, color = "gray30") +
  labs(x = "Capacity change (k gal)", y = "Replace events",
       title = "...but total capacity is largely maintained",
       subtitle = "Capacity change at the same upgrade events (clipped to +/-20k gal): big mass at zero --- consolidation") + thm
ggsave(here("Output","Figures","04ac_ReplaceCapChange_Hist.png"), ph_c, width = 9, height = 4.6, dpi = 150)
cat("  saved 04ac_ReplaceCapChange_Hist.png\n")

# ---- (4) size stickiness: net tank-count change across ALL facility-years ----
st <- fp[!is.na(net_tank_change)]
pct0 <- 100 * mean(st$net_tank_change == 0L)
sb <- st[, .(tb = pmin(pmax(net_tank_change, -3L), 3L))]
p_st <- ggplot(sb, aes(tb)) +
  geom_bar(fill = "#2A9D8F", width = 0.9) +
  scale_x_continuous(breaks = -3:3, labels = c("<=-3",-2:2,"3+")) +
  scale_y_continuous(labels = scales::label_number(scale_cut = scales::cut_short_scale())) +
  labs(x = "Net change in tank count", y = "Facility-years",
       title = "Facility size barely changes year to year",
       subtitle = sprintf("Net tank-count change across all facility-years: %.1f%% are zero --- size is a near-fixed attribute", pct0)) + thm
ggsave(here("Output","Figures","04ac_SizeStickiness.png"), p_st, width = 9, height = 4.6, dpi = 150)
cat(sprintf("  saved 04ac_SizeStickiness.png (%.1f%% zero)\n=== 04ac DONE ===\n", pct0))
