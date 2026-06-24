# T_Baseline_Characteristics_Slide.R
# Real values for the "baseline characteristics" slide table (Setting/Data section).
# Columns: Full sample (18 study states) | Texas | Controls (17 states).
# Blocks: (1) Coverage of the alive-at-reform sample; (2) tank attributes AT reform
#         (Dec 22, 1998); (3) average facility profile over the full study sample.
# Sources: Data/Analysis/exact_base.csv  (tank-level: wall, install yr, age, enter/exit)
#          Data/Analysis/facility_panel.csv (facility-year: counts, capacity, leaks)
# Output:  Output/Tables/T_Baseline_Characteristics_Slide.{tex,csv}

suppressPackageStartupMessages({ library(data.table); library(here) })
cat("=== BASELINE CHARACTERISTICS TABLE ===\n")

REFORM_DAY <- as.integer(as.Date("1998-12-22"))   # 10582 days since 1970 origin
cat(sprintf("Reform day (since 1970): %d\n", REFORM_DAY))

# ---- tank-level: attributes among tanks ACTIVE at reform ----
tk <- fread(here("Data","Analysis","exact_base.csv"),
  select = c("panel_id","texas_treated","mm_wall","install_yr_int",
             "release_det_deadline_yr","t_enter","t_exit","age_enter"))
tk <- tk[!is.na(texas_treated)]
tkr <- tk[t_enter <= REFORM_DAY & t_exit >= REFORM_DAY]           # active at reform
tkr[, age_reform := age_enter + (REFORM_DAY - t_enter) / 365.25]
tkr[, sw      := mm_wall == "Single-Walled"]
tkr[, pre89   := install_yr_int < 1989]
tkr[, leakdet := release_det_deadline_yr <= 1998]                 # past leak-detection deadline by reform
cat(sprintf("Tanks active at reform: %d (TX %d, Ctrl %d)\n",
            nrow(tkr), tkr[texas_treated==1,.N], tkr[texas_treated==0,.N]))

# ---- facility-year: coverage, profile, releases ----
fp <- fread(here("Data","Analysis","facility_panel.csv"),
  select = c("panel_id","panel_year","texas_treated","active_tanks","total_capacity",
             "avg_tank_age","n_tanks_at_reform","total_capacity_reform","fac_is_incumbent",
             "n_leaks","n_leak_incidents"))
fp <- fp[!is.na(texas_treated)]
fac_inc <- unique(fp[fac_is_incumbent == 1L | n_tanks_at_reform > 0L, .(panel_id, texas_treated)])
fp_inc  <- fp[panel_id %in% fac_inc$panel_id]
cat(sprintf("Incumbent (alive-at-reform) facilities: %d; facility-years: %d\n",
            nrow(fac_inc), nrow(fp_inc)))

col_stats <- function(tt) {
  sel <- function(d) if (tt == "Full") d else d[texas_treated == ifelse(tt == "Texas", 1L, 0L)]
  T <- sel(tkr); FAC <- sel(fac_inc); FYi <- sel(fp_inc); FYa <- sel(fp)
  data.table(
    facilities    = nrow(FAC),
    tanks_reform  = nrow(T),
    fac_years     = nrow(FYi),
    pct_sw        = 100 * mean(T$sw,      na.rm = TRUE),
    pct_pre89     = 100 * mean(T$pre89,   na.rm = TRUE),
    med_age       = median(T$age_reform,  na.rm = TRUE),
    tanks_med     = as.numeric(median(FYa$active_tanks, na.rm = TRUE)),
    tanks_p25     = as.numeric(quantile(FYa$active_tanks, 0.25, na.rm = TRUE)),
    tanks_p75     = as.numeric(quantile(FYa$active_tanks, 0.75, na.rm = TRUE)),
    cap_med       = median(FYa$total_capacity, na.rm = TRUE) / 1000,
    cap_p25       = as.numeric(quantile(FYa$total_capacity, 0.25, na.rm = TRUE)) / 1000,
    cap_p75       = as.numeric(quantile(FYa$total_capacity, 0.75, na.rm = TRUE)) / 1000,
    mean_age      = mean(FYa$avg_tank_age,   na.rm = TRUE),
    rel_rate      = 100 * mean(FYa$n_leak_incidents > 0L, na.rm = TRUE))
}
S <- list(Full = col_stats("Full"), Texas = col_stats("Texas"), Controls = col_stats("Controls"))
res <- rbindlist(S, idcol = "group")
print(res)
fwrite(res, here("Output","Tables","T_Baseline_Characteristics_Slide.csv"))

# ---- format helpers ----
cm  <- function(x) format(round(x), big.mark = ",")
M   <- function(x) sprintf("%.2fM", x / 1e6)
p0  <- function(x) sprintf("%.0f\\%%", x)
p1  <- function(x) sprintf("%.1f\\%%", x)
n1  <- function(x) sprintf("%.1f", x)
row3 <- function(lab, f, fmt) sprintf("\\quad %s & %s & %s & %s \\\\",
                                      lab, fmt(S$Full[[f]]), fmt(S$Texas[[f]]), fmt(S$Controls[[f]]))
iqr_row <- function(lab, m, lo, hi) { g <- function(k)
  sprintf("%.0f [%.0f--%.0f]", S[[k]][[m]], S[[k]][[lo]], S[[k]][[hi]])
  sprintf("\\quad %s & %s & %s & %s \\\\", lab, g("Full"), g("Texas"), g("Controls")) }

tex <- c(
  "\\begin{center}", "\\scriptsize", "\\setlength{\\tabcolsep}{6pt}",
  "\\renewcommand{\\arraystretch}{1.05}", "\\begin{tabular}{lrrr}", "\\hline",
  " & \\textbf{Full sample} & \\textbf{Texas} & \\textbf{Controls} \\\\", "\\hline",
  "\\multicolumn{4}{l}{\\textit{Coverage (alive-at-reform sample)}} \\\\",
  row3("Facilities",            "facilities",   cm),
  row3("Tanks at reform",       "tanks_reform", cm),
  sprintf("\\quad Facility-years & %s & %s & %s \\\\[4pt]", M(S$Full$fac_years), M(S$Texas$fac_years), M(S$Controls$fac_years)),
  "\\multicolumn{4}{l}{\\textit{At reform (Dec 22, 1998)}} \\\\",
  row3("Share single-walled",   "pct_sw",      p0),
  row3("Share pre-1989 vintage","pct_pre89",   p0),
  sprintf("\\quad Median tank age (years) & %s & %s & %s \\\\[4pt]", n1(S$Full$med_age), n1(S$Texas$med_age), n1(S$Controls$med_age)),
  "\\multicolumn{4}{l}{\\textit{Average facility profile (full sample)}} \\\\",
  iqr_row("Tanks per facility (med.\\ [IQR])", "tanks_med", "tanks_p25", "tanks_p75"),
  iqr_row("Total capacity, k gal (med.\\ [IQR])", "cap_med", "cap_p25", "cap_p75"),
  row3("Mean tank age (years)",    "mean_age",      n1),
  row3("Annual release rate",      "rel_rate",      p1),
  "\\hline", "\\end{tabular}", "\\end{center}")
writeLines(tex, here("Output","Tables","T_Baseline_Characteristics_Slide.tex"))

# ---- overlapping histograms at the treatment date (TX vs control) ----
suppressPackageStartupMessages(library(ggplot2))
fr <- unique(fp[fac_is_incumbent == 1L | n_tanks_at_reform > 0L,
  .(panel_id, texas_treated, n_tanks_at_reform, total_capacity_reform)], by = "panel_id")
fr <- fr[n_tanks_at_reform > 0L]
fr[, grp := ifelse(texas_treated == 1L, "Texas", "Controls")]
pal <- c("Texas" = "#FDB515", "Controls" = "#003262")
thm <- theme_minimal(base_size = 13) +
  theme(legend.position = "top", plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(color = "grey30"))

ft <- fr[, .(panel_id, grp, tb = pmin(n_tanks_at_reform, 8L))]
agg_t <- ft[, .N, by = .(grp, tb)][, prop := N / sum(N), by = grp]
p_tanks <- ggplot(agg_t, aes(tb, prop, fill = grp)) +
  geom_col(position = "identity", alpha = 0.5, width = 0.95) +
  scale_x_continuous(breaks = 1:8, labels = c(1:7, "8+")) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = pal) +
  labs(x = "Tanks per facility at reform (Dec 22, 1998)", y = "Share of facilities", fill = NULL,
       title = "Facility size at reform: Texas vs. controls",
       subtitle = "Tank count per incumbent facility (overlapping)") + thm
ggsave(here("Output","Figures","Fig_Baseline_TanksAtReform.png"), p_tanks, width = 8, height = 5, dpi = 150)

fc <- fr[total_capacity_reform > 0 & total_capacity_reform <= 60000]
fc[, capk := total_capacity_reform / 1000]
p_cap <- ggplot(fc, aes(capk, fill = grp, color = grp)) +
  geom_density(alpha = 0.3, linewidth = 0.8) +
  scale_fill_manual(values = pal) + scale_color_manual(values = pal) +
  labs(x = "Total capacity per facility at reform (k gal)", y = "Density", fill = NULL, color = NULL,
       title = "Facility capacity at reform: Texas vs. controls",
       subtitle = "Total tank capacity per incumbent facility, Dec 22 1998 (clipped at 60k gal)") + thm
ggsave(here("Output","Figures","Fig_Baseline_CapacityAtReform.png"), p_cap, width = 8, height = 5, dpi = 150)

cat("\nSaved .tex, .csv, and 2 histograms.\n=== DONE ===\n")
