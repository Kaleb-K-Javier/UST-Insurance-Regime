# 04ak_WhichTank_clogit.R
# Ticket 019 — M4: which tank is acted on (CONDITIONAL logit, McFadden).
# Replaces the tie-biased 04ai age/capacity "vs random ~E[1/n]" benchmark.
# Choice occasion = facility-year with EXACTLY one closure in a multi-tank facility.
# Alternatives = the facility's active tanks; strata(facility-year) removes the
# facility FE and handles ties correctly. SE CLUSTERED BY STATE (G=18).
# READ-ONLY DESCRIPTIVE. Reads only the 7 needed cols from the 3.9GB panel_dt.

suppressPackageStartupMessages({
  library(data.table); library(ggplot2); library(here)
})
need <- function(p) if (!requireNamespace(p, quietly = TRUE))
  install.packages(p, repos = "https://cloud.r-project.org")
need("survival"); need("sandwich"); need("scales")
suppressPackageStartupMessages({ library(survival); library(scales) })

cat("=== 04ak WHICH TANK (conditional logit) ===\n")
OUT_T <- here("Output", "Tables"); OUT_F <- here("Output", "Figures")
dir.create(OUT_T, recursive = TRUE, showWarnings = FALSE)
dir.create(OUT_F, recursive = TRUE, showWarnings = FALSE)
texesc <- function(x) gsub("%", "\\\\%", gsub("_", "\\\\_", x))

# ---- read ONLY the 7 needed cols (filter-first to avoid OOM, see 04ai) ----
dt <- fread(here("Data", "Analysis", "panel_dt.csv"),
            select = c("tank_panel_id", "panel_id", "panel_year", "state",
                       "mm_wall", "capacity", "tank_age", "closure_event"))
cat(sprintf("  tank-years read: %s\n", format(nrow(dt), big.mark = ",")))

dt <- dt[!is.na(tank_age) & !is.na(capacity) & capacity > 0 & !is.na(closure_event)]
dt[, sw := as.integer(grepl("single", mm_wall, ignore.case = TRUE))]   # "Single-Walled"/"Double-Walled"

# ---- cheap facility-year aggregates FIRST, then subset to the choice occasions ----
fy <- dt[, .(n_active = .N, n_closed = sum(closure_event)), by = .(panel_id, panel_year)]
tgt <- fy[n_active >= 2L & n_closed == 1L, .(panel_id, panel_year)]    # multi-tank, single closure
sub <- dt[tgt, on = .(panel_id, panel_year)]
sub[, strata_id := paste(panel_id, panel_year)]
n_strata <- uniqueN(sub$strata_id); n_obs <- nrow(sub)
cat(sprintf("  n_strata (choice occasions) = %s | n_obs (alternatives) = %s\n",
            format(n_strata, big.mark = ","), format(n_obs, big.mark = ",")))
stopifnot(n_strata >= 19000L, n_strata <= 20500L)

# ---- conditional logit: P(tank j closed | strata) ; cluster SE by state ----
# Each stratum has EXACTLY one event (single closure) => no within-stratum tied event
# times, so method="approximate" (Breslow) gives the SAME point estimates as exact while
# allowing robust/clustered variance. (The exact method supports neither in-formula
# cluster() nor score residuals -> sandwich, in this survival version.)
# Primary: in-formula cluster(state). FALLBACK (spec): if it errors, fit WITHOUT
# cluster() and cluster via sandwich::vcovCL. Both yield state-clustered SE.
terms_in  <- c("tank_age", "sw", "I(capacity/1000)")
terms_out <- c("tank_age", "SW", "capacity_per_1000gal")
cl_path <- "in-formula cluster()"
fit_cl <- tryCatch(
  clogit(closure_event ~ tank_age + sw + I(capacity / 1000) +
           strata(strata_id) + cluster(state), data = sub, method = "approximate"),
  error = function(e) { cl_path <<- "sandwich::vcovCL fallback"; cat(sprintf(
    "  in-formula cluster() unsupported (%s); using sandwich fallback\n", conditionMessage(e))); NULL })

if (cl_path == "in-formula cluster()") {
  fit <- fit_cl
  ct  <- summary(fit)$coefficients
  se_col <- if ("robust se" %in% colnames(ct)) "robust se" else "se(coef)"
  coef_v <- ct[terms_in, "coef"]; se_v <- ct[terms_in, se_col]; p_v <- ct[terms_in, "Pr(>|z|)"]
} else {
  need("sandwich")
  fit <- clogit(closure_event ~ tank_age + sw + I(capacity / 1000) +
                  strata(strata_id), data = sub, method = "approximate")
  vcl <- sandwich::vcovCL(fit, cluster = sub$state)
  coef_v <- coef(fit)[terms_in]
  se_v   <- sqrt(diag(vcl))[terms_in]
  z_v    <- coef_v / se_v
  p_v    <- 2 * pnorm(-abs(z_v))
}
if (fit$nevent != n_strata) warning(sprintf("clogit events %d != n_strata %d", fit$nevent, n_strata))
cat(sprintf("  clogit converged; SE path: %s\n", cl_path))

m4 <- data.table(
  term = terms_out,
  odds_ratio = exp(coef_v),
  coef = coef_v,
  se   = se_v,
  p    = p_v,
  n_strata = n_strata, n_obs = n_obs)
stopifnot(all(is.finite(m4$odds_ratio)))
fwrite(m4, file.path(OUT_T, "M4_WhichTank_clogit.csv"))
cat("  saved M4_WhichTank_clogit.csv\n"); print(m4)

sw_or <- m4[term == "SW", odds_ratio]
cat(sprintf("  SE path: %s | SW odds ratio = %.3f (>1 reproduces 04ai single-walled finding: %s)\n",
            cl_path, sw_or, ifelse(sw_or > 1, "YES", "NO")))

# ---- forest figure: OR + 95% CI for the three terms ----
zse <- m4$se   # robust SE on the coef scale
m4f <- data.table(term = factor(terms_out, levels = rev(terms_out)),
                  or = m4$odds_ratio,
                  lo = exp(m4$coef - 1.96 * zse),
                  hi = exp(m4$coef + 1.96 * zse))
pM4 <- ggplot(m4f, aes(or, term)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "grey50") +
  geom_errorbarh(aes(xmin = lo, xmax = hi), height = 0.18, color = "#2A9D8F", linewidth = 0.9) +
  geom_point(size = 3, color = "#264653") +
  geom_text(aes(label = sprintf("%.2f", or)), vjust = -1.0, size = 3.6, color = "grey25") +
  labs(x = "Odds ratio (95% CI)", y = NULL,
       title = "Which tank gets closed? Conditional logit within facility-year",
       subtitle = sprintf("Strata = facility-year (single-closure, multi-tank). State-clustered SE. n_strata = %s.",
                          format(n_strata, big.mark = ","))) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(color = "grey30"), panel.grid.minor = element_blank())
ggsave(file.path(OUT_F, "M4_WhichTank_clogit.png"), pM4, width = 8.5, height = 4.5, dpi = 150)
cat("  saved M4_WhichTank_clogit.png\n")

# ---- .tex ----
m4_tex <- c(
  "\\begin{center}\\small\\renewcommand{\\arraystretch}{1.25}",
  "\\begin{tabular}{lrrr}", "\\toprule",
  "\\textbf{The closed tank's\\ldots} & \\textbf{OR} & \\textbf{SE} & \\textbf{p} \\\\", "\\midrule",
  sprintf("age (per year) & %.3f & %.3f & %.3f \\\\",
          m4[term == "tank_age", odds_ratio], m4[term == "tank_age", se], m4[term == "tank_age", p]),
  sprintf("single-walled (vs double) & %.3f & %.3f & %.3f \\\\",
          m4[term == "SW", odds_ratio], m4[term == "SW", se], m4[term == "SW", p]),
  sprintf("capacity (per 1{,}000 gal) & %.3f & %.3f & %.3f \\\\",
          m4[term == "capacity_per_1000gal", odds_ratio], m4[term == "capacity_per_1000gal", se],
          m4[term == "capacity_per_1000gal", p]),
  "\\bottomrule", "\\end{tabular}", "\\end{center}",
  sprintf("\\vspace{0.1cm}{\\centering\\scriptsize McFadden conditional logit, strata $=$ facility-year (single-closure, multi-tank; $n_{\\text{strata}}{=}%s$). Conditioning removes the facility FE and is tie-robust. State-clustered robust SE ($G{=}18$). $\\text{OR}_{\\text{SW}}{>}1$ reproduces the 04ai single-walled finding; age/capacity ORs are the corrected estimates.\\par}",
          format(n_strata, big.mark = ",")))
writeLines(m4_tex, file.path(OUT_T, "M4_WhichTank_clogit.tex"))
cat("  saved M4_WhichTank_clogit.tex\n=== 04ak DONE ===\n")
