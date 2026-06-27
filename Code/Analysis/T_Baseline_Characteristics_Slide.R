# T_Baseline_Characteristics_Slide.R
# Texas-vs-control BALANCE table, before (alive-at-reform) and after (birth-CEM matched).
# Shows the imbalance the matching removes. Backs Setting/Data section (tbl-summary, ¶8).
# Columns: Alive-at-reform {Texas, Control} | Matched {Texas, Control} | SMD {before, after}.
# Sources: Data/Analysis/exact_base.csv      (tank-level: wall, install yr, age, enter/exit)
#          Data/Analysis/facility_panel.csv  (facility-year: counts, capacity, age)
#          Data/Analysis/matched_tanks_birth_cem.csv  (headline tank-DiD matched panel; same
#            file 02c_Stepped_DiD.R reads. Matched facility set = unique(panel_id) after
#            cem_weight>0 & install_yr_int<1999 & first_year_churn==0  -> should be 117,250.)
# Output:  Output/Tables/T_Baseline_Characteristics_Slide.{tex,csv}
#          (the two figures Fig_Baseline_{Tanks,Capacity}AtReform.png are unchanged — raw
#           pre-match distributions §3 ¶8 cites.)

suppressPackageStartupMessages({ library(data.table); library(here) })
cat("=== BASELINE BALANCE TABLE (alive-at-reform vs birth-CEM matched) ===\n")

ANALYSIS_DIR <- Sys.getenv("UST_ANALYSIS_DIR", here("Data", "Analysis"))
REFORM_DAY   <- as.integer(as.Date("1998-12-22"))   # days since 1970 origin
cat(sprintf("Reform day (since 1970): %d\n", REFORM_DAY))

# ---- tank-level: attributes among tanks ACTIVE at reform ----
tk <- fread(file.path(ANALYSIS_DIR, "exact_base.csv"),
  select = c("panel_id", "texas_treated", "mm_wall", "install_yr_int",
             "t_enter", "t_exit", "age_enter"))
tk  <- tk[!is.na(texas_treated)]
tkr <- tk[t_enter <= REFORM_DAY & t_exit >= REFORM_DAY]          # active at reform
tkr[, age_reform := age_enter + (REFORM_DAY - t_enter) / 365.25]
tkr[, sw    := as.integer(mm_wall == "Single-Walled")]
tkr[, pre89 := as.integer(install_yr_int < 1989L)]
cat(sprintf("Tanks active at reform: %d\n", nrow(tkr)))

# ---- facility-year: profile rows ----
fp <- fread(file.path(ANALYSIS_DIR, "facility_panel.csv"),
  select = c("panel_id", "texas_treated", "active_tanks", "total_capacity",
             "avg_tank_age", "n_tanks_at_reform", "total_capacity_reform", "fac_is_incumbent"))
fp <- fp[!is.na(texas_treated)]

# ---- alive-at-reform incumbent facilities (the "before matching" sample) ----
fac_inc <- unique(fp[fac_is_incumbent == 1L | n_tanks_at_reform > 0L, .(panel_id, texas_treated)])
cat(sprintf("Alive-at-reform incumbent facilities: %d (TX %d, Ctrl %d)\n",
            nrow(fac_inc), fac_inc[texas_treated==1L,.N], fac_inc[texas_treated==0L,.N]))

# ---- matched facility set from the headline tank-DiD panel (the "after matching" sample) ----
cat("Reading matched_tanks_birth_cem.csv (4.6 GB; this is the slow step)...\n")
.mh   <- names(fread(file.path(ANALYSIS_DIR, "matched_tanks_birth_cem.csv"), nrows = 0L))
.msel <- intersect(c("panel_id","texas_treated","install_yr_int","cem_weight","first_year_churn"), .mh)
mt <- fread(file.path(ANALYSIS_DIR, "matched_tanks_birth_cem.csv"), select = .msel)
if ("cem_weight"       %in% names(mt)) mt <- mt[cem_weight > 0]
if ("install_yr_int"   %in% names(mt)) mt <- mt[install_yr_int < 1999L]
if ("first_year_churn" %in% names(mt)) mt <- mt[first_year_churn == 0L | is.na(first_year_churn)]
matched_fac <- unique(mt[, .(panel_id, texas_treated)])
cat(sprintf("Matched facilities: %d (TX %d, Ctrl %d)  [target 117,250]\n",
            nrow(matched_fac), matched_fac[texas_treated==1L,.N], matched_fac[texas_treated==0L,.N]))

# ---- one cell = stats for a given facility-id set ----
# Returns display stats (medians/IQRs/%) plus means+SDs for SMD.
cell_stats <- function(ids) {
  T  <- tkr[panel_id %in% ids]
  FY <- fp[panel_id %in% ids]
  q  <- function(x, p) as.numeric(quantile(x, p, na.rm = TRUE))
  list(
    n_fac     = length(ids),
    n_tanks   = nrow(T),
    pct_sw    = 100 * mean(T$sw, na.rm = TRUE),
    pct_pre89 = 100 * mean(T$pre89, na.rm = TRUE),
    med_age   = median(T$age_reform, na.rm = TRUE),
    tanks_med = as.numeric(median(FY$active_tanks, na.rm = TRUE)),
    tanks_p25 = q(FY$active_tanks, .25), tanks_p75 = q(FY$active_tanks, .75),
    cap_med   = median(FY$total_capacity, na.rm = TRUE) / 1000,
    cap_p25   = q(FY$total_capacity, .25) / 1000, cap_p75 = q(FY$total_capacity, .75) / 1000,
    mean_age  = mean(FY$avg_tank_age, na.rm = TRUE),
    # means / sds for SMD (var order: sw, pre89, age_reform, tanks, cap_k, avg_age)
    m = c(mean(T$sw,na.rm=TRUE), mean(T$pre89,na.rm=TRUE), mean(T$age_reform,na.rm=TRUE),
          mean(FY$active_tanks,na.rm=TRUE), mean(FY$total_capacity,na.rm=TRUE)/1000,
          mean(FY$avg_tank_age,na.rm=TRUE)),
    s = c(sd(T$sw,na.rm=TRUE), sd(T$pre89,na.rm=TRUE), sd(T$age_reform,na.rm=TRUE),
          sd(FY$active_tanks,na.rm=TRUE), sd(FY$total_capacity,na.rm=TRUE)/1000,
          sd(FY$avg_tank_age,na.rm=TRUE))
  )
}

A_tx <- cell_stats(fac_inc[texas_treated==1L, panel_id])
A_ct <- cell_stats(fac_inc[texas_treated==0L, panel_id])
M_tx <- cell_stats(matched_fac[texas_treated==1L, panel_id])
M_ct <- cell_stats(matched_fac[texas_treated==0L, panel_id])

# ---- standardized mean differences (TX vs Ctrl), before & after ----
smd_vec <- function(tx, ct) (tx$m - ct$m) / sqrt((tx$s^2 + ct$s^2) / 2)   # length 6
smd_before <- smd_vec(A_tx, A_ct)
smd_after  <- smd_vec(M_tx, M_ct)

# ---- assemble the table (formatted strings; one row per metric) ----
cm  <- function(x) format(round(x), big.mark = ",")
pc  <- function(x) sprintf("%.0f%%", x)
d1  <- function(x) sprintf("%.1f", x)
iqr <- function(m,lo,hi) sprintf("%.0f [%.0f-%.0f]", m, lo, hi)
sm  <- function(x) sprintf("%+.3f", x)

row_csv <- function(lab, fa, fb, fc, fd, sb, sa)
  data.table(metric=lab, alive_texas=fa, alive_control=fb,
             matched_texas=fc, matched_control=fd, smd_before=sb, smd_after=sa)

res <- rbindlist(list(
  row_csv("Facilities",              cm(A_tx$n_fac),  cm(A_ct$n_fac),  cm(M_tx$n_fac),  cm(M_ct$n_fac),  "", ""),
  row_csv("Tanks at reform",         cm(A_tx$n_tanks),cm(A_ct$n_tanks),cm(M_tx$n_tanks),cm(M_ct$n_tanks),"", ""),
  row_csv("Share single-walled",     pc(A_tx$pct_sw), pc(A_ct$pct_sw), pc(M_tx$pct_sw), pc(M_ct$pct_sw), sm(smd_before[1]), sm(smd_after[1])),
  row_csv("Share pre-1989 vintage",  pc(A_tx$pct_pre89),pc(A_ct$pct_pre89),pc(M_tx$pct_pre89),pc(M_ct$pct_pre89), sm(smd_before[2]), sm(smd_after[2])),
  row_csv("Median tank age (yrs)",   d1(A_tx$med_age),d1(A_ct$med_age),d1(M_tx$med_age),d1(M_ct$med_age), sm(smd_before[3]), sm(smd_after[3])),
  row_csv("Tanks per facility",      iqr(A_tx$tanks_med,A_tx$tanks_p25,A_tx$tanks_p75), iqr(A_ct$tanks_med,A_ct$tanks_p25,A_ct$tanks_p75),
                                     iqr(M_tx$tanks_med,M_tx$tanks_p25,M_tx$tanks_p75), iqr(M_ct$tanks_med,M_ct$tanks_p25,M_ct$tanks_p75), sm(smd_before[4]), sm(smd_after[4])),
  row_csv("Total capacity, k gal",   iqr(A_tx$cap_med,A_tx$cap_p25,A_tx$cap_p75), iqr(A_ct$cap_med,A_ct$cap_p25,A_ct$cap_p75),
                                     iqr(M_tx$cap_med,M_tx$cap_p25,M_tx$cap_p75), iqr(M_ct$cap_med,M_ct$cap_p25,M_ct$cap_p75), sm(smd_before[5]), sm(smd_after[5])),
  row_csv("Mean tank age (yrs)",     d1(A_tx$mean_age),d1(A_ct$mean_age),d1(M_tx$mean_age),d1(M_ct$mean_age), sm(smd_before[6]), sm(smd_after[6]))
))
cat("\n"); print(res)
fwrite(res, here("Output","Tables","T_Baseline_Characteristics_Slide.csv"))

# ---- LaTeX (two-level header; 4 data cols + 2 SMD cols) ----
tx <- function(s) gsub("%", "\\\\%", s)                       # escape % for LaTeX
trow <- function(r) sprintf("%s & %s & %s & %s & %s & %s & %s \\\\",
  r$metric, tx(r$alive_texas), tx(r$alive_control), tx(r$matched_texas), tx(r$matched_control),
  r$smd_before, r$smd_after)
tex <- c(
  "\\begin{center}", "\\footnotesize", "\\setlength{\\tabcolsep}{5pt}",
  "\\renewcommand{\\arraystretch}{1.08}", "\\begin{tabular}{l rr rr cc}", "\\hline",
  " & \\multicolumn{2}{c}{\\textbf{Alive-at-reform}} & \\multicolumn{2}{c}{\\textbf{Matched (birth-CEM)}} & \\multicolumn{2}{c}{\\textbf{SMD}} \\\\",
  "\\cline{2-3}\\cline{4-5}\\cline{6-7}",
  " & Texas & Control & Texas & Control & Before & After \\\\", "\\hline",
  vapply(seq_len(nrow(res)), function(i) trow(res[i]), character(1)),
  "\\hline", "\\end{tabular}", "\\end{center}")
writeLines(tex, here("Output","Tables","T_Baseline_Characteristics_Slide.tex"))
cat(sprintf("\nSMD before (TX vs Ctrl): %s\n", paste(sprintf("%+.3f", smd_before), collapse=" ")))
cat(sprintf("SMD after  (TX vs Ctrl): %s\n", paste(sprintf("%+.3f", smd_after),  collapse=" ")))

# ---- overlapping histograms at the treatment date (TX vs control) — UNCHANGED ----
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
