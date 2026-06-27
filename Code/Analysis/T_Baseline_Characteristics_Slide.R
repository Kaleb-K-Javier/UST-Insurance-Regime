# T_Baseline_Characteristics_Slide.R
# Texas-vs-control BALANCE table, before (alive-at-reform) and after (birth-CEM matched).
# Shows the imbalance the FACILITY-level birth-CEM match removes. Backs Setting/Data (tbl-summary, ¶8).
# Columns: Alive-at-reform {Texas, Control} | Matched {Texas, Control} | SMD {before, after}.
#   "Matched" = facility-level birth-CEM (fac_cem_matched==1 in facility_panel.csv) = the 02j /
#   facility-DiD sample (105,245 facilities). The match is WEIGHTED (fac_cem_weight), so the Matched
#   columns and the "after" SMD are fac_cem_weight-WEIGHTED; Alive columns are unweighted. A balance
#   table reports means (weighting medians is ill-defined), so rows 3-8 are means, not median[IQR].
# Sources: Data/Analysis/exact_base.csv (tank-level), Data/Analysis/facility_panel.csv (facility-year,
#   carries fac_cem_matched + fac_cem_weight). NO 4.6 GB matched-tank read needed.
# Output:  Output/Tables/T_Baseline_Characteristics_Slide.{tex,csv}  (figures unchanged).

suppressPackageStartupMessages({ library(data.table); library(here) })
cat("=== BASELINE BALANCE TABLE (alive-at-reform vs facility birth-CEM matched) ===\n")

ANALYSIS_DIR <- Sys.getenv("UST_ANALYSIS_DIR", here("Data", "Analysis"))
REFORM_DAY   <- as.integer(as.Date("1998-12-22"))

# ---- tank-level: attributes among tanks ACTIVE at reform ----
tk <- fread(file.path(ANALYSIS_DIR, "exact_base.csv"),
  select = c("panel_id", "texas_treated", "mm_wall", "install_yr_int", "t_enter", "t_exit", "age_enter"))
tk  <- tk[!is.na(texas_treated)]
tkr <- tk[t_enter <= REFORM_DAY & t_exit >= REFORM_DAY]
tkr[, age_reform := age_enter + (REFORM_DAY - t_enter) / 365.25]
tkr[, sw    := as.integer(mm_wall == "Single-Walled")]
tkr[, pre89 := as.integer(install_yr_int < 1989L)]

# ---- facility-year: profile + the facility CEM match ----
fp <- fread(file.path(ANALYSIS_DIR, "facility_panel.csv"),
  select = c("panel_id", "texas_treated", "active_tanks", "total_capacity", "avg_tank_age",
             "n_tanks_at_reform", "total_capacity_reform", "fac_is_incumbent",
             "fac_cem_matched", "fac_cem_weight"))
fp <- fp[!is.na(texas_treated)]

# attach the facility weight to each tank (constant within facility)
fw  <- unique(fp[, .(panel_id, fac_cem_weight)], by = "panel_id")
tkr <- merge(tkr, fw, by = "panel_id", all.x = TRUE)

# ---- the two samples ----
fac_inc     <- unique(fp[fac_is_incumbent == 1L | n_tanks_at_reform > 0L, .(panel_id, texas_treated)])
matched_fac <- unique(fp[fac_cem_matched == 1L, .(panel_id, texas_treated)])
cat(sprintf("Alive-at-reform incumbents: %d (TX %d, Ctrl %d)\n",
            nrow(fac_inc), fac_inc[texas_treated==1L,.N], fac_inc[texas_treated==0L,.N]))
cat(sprintf("Facility birth-CEM matched: %d (TX %d, Ctrl %d)\n",
            nrow(matched_fac), matched_fac[texas_treated==1L,.N], matched_fac[texas_treated==0L,.N]))

# ---- weighted moments ----
wmean <- function(x, w) { ok <- is.finite(x) & is.finite(w) & w > 0; sum(w[ok]*x[ok]) / sum(w[ok]) }
wsd   <- function(x, w) { ok <- is.finite(x) & is.finite(w) & w > 0
                          m <- sum(w[ok]*x[ok])/sum(w[ok]); sqrt(sum(w[ok]*(x[ok]-m)^2)/sum(w[ok])) }

# cell: stats for a facility-id set. weighted = TRUE -> use fac_cem_weight (matched columns).
cell_stats <- function(ids, weighted) {
  T  <- tkr[panel_id %in% ids]
  FY <- fp[panel_id %in% ids]
  wT <- if (weighted) T$fac_cem_weight  else rep(1, nrow(T))
  wF <- if (weighted) FY$fac_cem_weight else rep(1, nrow(FY))
  m <- c(sw    = wmean(T$sw, wT),        pre89 = wmean(T$pre89, wT),
         tage  = wmean(T$age_reform, wT), tanks = wmean(FY$active_tanks, wF),
         capk  = wmean(FY$total_capacity, wF) / 1000, fage = wmean(FY$avg_tank_age, wF))
  s <- c(sw    = wsd(T$sw, wT),          pre89 = wsd(T$pre89, wT),
         tage  = wsd(T$age_reform, wT),  tanks = wsd(FY$active_tanks, wF),
         capk  = wsd(FY$total_capacity, wF) / 1000, fage = wsd(FY$avg_tank_age, wF))
  q <- function(x, p) as.numeric(quantile(x, p, na.rm = TRUE))   # robust display for skewed vars
  list(n_fac = length(ids), n_tanks = nrow(T), m = m, s = s,
       tanks_med = median(FY$active_tanks, na.rm=TRUE), tanks_p25 = q(FY$active_tanks,.25), tanks_p75 = q(FY$active_tanks,.75),
       cap_med   = median(FY$total_capacity, na.rm=TRUE)/1000, cap_p25 = q(FY$total_capacity,.25)/1000, cap_p75 = q(FY$total_capacity,.75)/1000)
}

A_tx <- cell_stats(fac_inc[texas_treated==1L, panel_id],     weighted = FALSE)
A_ct <- cell_stats(fac_inc[texas_treated==0L, panel_id],     weighted = FALSE)
M_tx <- cell_stats(matched_fac[texas_treated==1L, panel_id], weighted = TRUE)
M_ct <- cell_stats(matched_fac[texas_treated==0L, panel_id], weighted = TRUE)

smd_vec    <- function(tx, ct) (tx$m - ct$m) / sqrt((tx$s^2 + ct$s^2) / 2)   # length 6
smd_before <- smd_vec(A_tx, A_ct)   # alive, unweighted
smd_after  <- smd_vec(M_tx, M_ct)   # matched, weighted

# ---- assemble (means; counts on rows 1-2) ----
cm  <- function(x) format(round(x), big.mark = ",")
pc  <- function(x) sprintf("%.0f%%", 100*x)
d1  <- function(x) sprintf("%.1f", x)
sm  <- function(x) sprintf("%+.3f", x)
iqr <- function(m,lo,hi) sprintf("%.0f [%.0f-%.0f]", m, lo, hi)
vi <- c(sw=1, pre89=2, tage=3, tanks=4, capk=5, fage=6)   # var -> index in m/s/smd

rowdt <- function(lab, fa, fb, fc, fd, sb, sa)
  data.table(metric=lab, alive_texas=fa, alive_control=fb,
             matched_texas=fc, matched_control=fd, smd_before=sb, smd_after=sa)
mrow <- function(lab, key, fmt) rowdt(lab,
  fmt(A_tx$m[key]), fmt(A_ct$m[key]), fmt(M_tx$m[key]), fmt(M_ct$m[key]),
  sm(smd_before[vi[key]]), sm(smd_after[vi[key]]))

res <- rbindlist(list(
  rowdt("Facilities",            cm(A_tx$n_fac),  cm(A_ct$n_fac),  cm(M_tx$n_fac),  cm(M_ct$n_fac),  "", ""),
  rowdt("Tanks at reform",       cm(A_tx$n_tanks),cm(A_ct$n_tanks),cm(M_tx$n_tanks),cm(M_ct$n_tanks),"", ""),
  mrow("Share single-walled",      "sw",    pc),
  mrow("Share pre-1989 vintage",   "pre89", pc),
  mrow("Mean tank age at reform",  "tage",  d1),
  rowdt("Tanks per facility (med [IQR])",
        iqr(A_tx$tanks_med,A_tx$tanks_p25,A_tx$tanks_p75), iqr(A_ct$tanks_med,A_ct$tanks_p25,A_ct$tanks_p75),
        iqr(M_tx$tanks_med,M_tx$tanks_p25,M_tx$tanks_p75), iqr(M_ct$tanks_med,M_ct$tanks_p25,M_ct$tanks_p75), "", ""),
  rowdt("Total capacity, k gal (med [IQR])",
        iqr(A_tx$cap_med,A_tx$cap_p25,A_tx$cap_p75), iqr(A_ct$cap_med,A_ct$cap_p25,A_ct$cap_p75),
        iqr(M_tx$cap_med,M_tx$cap_p25,M_tx$cap_p75), iqr(M_ct$cap_med,M_ct$cap_p25,M_ct$cap_p75), "", ""),
  mrow("Mean facility tank age",   "fage",  d1)
))
cat("\n"); print(res)
fwrite(res, here("Output","Tables","T_Baseline_Characteristics_Slide.csv"))

# ---- LaTeX (two-level header; 4 data cols + 2 SMD cols) ----
esc  <- function(s) gsub("%", "\\\\%", s)
trow <- function(r) sprintf("%s & %s & %s & %s & %s & %s & %s \\\\",
  r$metric, esc(r$alive_texas), esc(r$alive_control), esc(r$matched_texas), esc(r$matched_control),
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
cat(sprintf("\nSMD before (alive):   %s\n", paste(sprintf("%+.3f", smd_before), collapse=" ")))
cat(sprintf("SMD after  (matched): %s\n", paste(sprintf("%+.3f", smd_after),  collapse=" ")))

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
