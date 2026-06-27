# TICKET 028 — Revenue lookup: capacity x state-era fuel margin -> PM_Lookups
# Created: 2026-06-25
# Status: AWAITING_IMPLEMENTATION
# Attempt: 0
# Type: DATA CONSTRUCTION ONLY. No estimation, no Bellman, no C++. Builds the revenue
#       lookup consumed by ticket 029. Ungated.
# Depends on: ticket 025 (SEDS price/consumption/tax) and ticket 026
#       (wholesale + PADD crosswalk -> wholesale_margin_state_year.csv). Run 025 -> 026 -> 028.

═══════════════════════════════════════════════════
ECONOMIC MOTIVATION
═══════════════════════════════════════════════════
Bring a measured operating-revenue term into the portfolio DCM so the operating
payoff is disciplined by real dollars instead of the free capacity-bin constants
phi_1..phi_4. The revenue object is per-period fuel gross margin scaled by facility
size: R = (representative capacity of the G bin) x (state-era fuel margin). It enters
the estimator (ticket 029) as +psi * R, REPLACING phi_1..phi_4. This ticket only
builds the lookup; it estimates nothing. Goal is to MEASURE psi, not to test equality.

═══════════════════════════════════════════════════
MATH
═══════════════════════════════════════════════════
Eq. 1  margin_{g,era} = mean_{t in era} margin_{g,t}        (era average; margin from 026)
Eq. 2  capbar_G       = mean over included facility-years in bin G of total_cap_capped
                        (gallons; per-tank winsorized at 60k, summed to facility-year —
                         the SAME object PM01 uses to build G)
Eq. 3  R[G, g, era]   = capbar_G * margin_{g,era} / SCALE     (model units)
       kappa (turnover) := 1. It is a single multiplier on the whole term, so psi and
       kappa enter only as the product psi*kappa; fixing kappa=1 is a free
       normalization (psi_hat absorbs turnover; divide by true turnover later for a
       real-dollar reading). EIA-pinned kappa_{g,era} = consumption/capacity is an
       optional later refinement, NOT in this ticket.
ERAS: "2006" = year 1999-2013, "2014" = 2014-2018, "2019" = 2019+
       (identical to PM01/PM02 era_of_year; do NOT redefine). SCALE = 10000.

═══════════════════════════════════════════════════
PSEUDOCODE
═══════════════════════════════════════════════════
Script: Code/Macro/M03_revenue_lookup.R   (025 -> M01, 026 -> M02, this -> M03)

Step 1 — Read the margin series (ticket 026 output):
  m <- fread("Data/Macro/wholesale_margin_state_year.csv")  # state, year, margin_usd_gal, ...
  USE THE NOMINAL column `margin_usd_gal` (NOT margin_real_usd_gal_2020). Rationale,
  pinned so the coder makes no call: every dollar object already in PM_Lookups
  (pbar, tau, D) is a NOMINAL time-average / SCALE; revenue must match that basis or
  psi mixes nominal and real dollars. Do not deflate.
  Restrict to study states, year in 1999-2020.
  assert: margin_usd_gal non-NA for all study states 1999-2020; report (not drop) any
  negative-margin state-years.

Step 2 — Era-average the margin (Eq. 1):
  era_of_year(y): y<=2013 -> "2006"; y<=2018 -> "2014"; else "2019".
  margin_era <- m[, .(margin = mean(margin_usd_gal)), by = .(state, era)].
  assert: every study state has all 3 eras present and finite.

Step 3 — capbar_G (Eq. 2):
  Read Data/Analysis/panel_dt.csv (panel_id, panel_year, capacity); panel_year>=1999.
  capped_cap = pmin(capacity, 60000L); total_cap_capped = sum(capped_cap) by
  (panel_id, panel_year)   -- IDENTICAL to PM01 lines 62-69.
  Read Data/Analysis/pm_panel.csv (panel_id, panel_year, g, era, G, excl_*).
  Join total_cap_capped onto pm_panel by (panel_id, panel_year).
  INCLUDED = all excl_* == 0 AND !is.na(G) AND total_cap_capped > 0.
  capbar_G <- INCLUDED[, mean(total_cap_capped), by = G][order(G)]  (length-4).
  capbar_G is GLOBAL (one representative capacity per G bin, pooled over all states
  and eras) and era-invariant — the era/state variation in R comes entirely from the
  margin. Do NOT compute capbar per state or per era.
  assert: 4 finite positive values, monotone increasing in G.

Step 4 — Build R lookup (Eq. 3) and attach to PM_Lookups.rds:
  states <- sort(unique(pm_panel$g)).
  R_rev <- array(NA_real_, dim = c(4, length(states), 3),
                 dimnames = list(G = 1:4, g = states, era = c("2006","2014","2019"))).
  For each (G, g, era): R_rev[G,g,era] <- capbar_G[G] * margin_era[state==g & era] / SCALE.
  KS/MD ONLY: excluded from the likelihood; if their margin is missing set R_rev to 0
  with a printed note (they never enter 029). Do NOT zero-fill any OTHER state — a
  missing margin for any non-excluded state is a real gap and must fail the assert
  below (surface it, do not paper over it).
  assert: R_rev finite and > 0 for every NON-excluded study state x 3 eras x 4 G.
  Load Output/Estimation_Results/PM_Lookups.rds; ADD element R_rev (do NOT alter any
  existing element); re-save. Also write Data/Macro/R_rev_long.csv (G,g,era,R).

Step 5 — Print summary (CLAUDE.md output rules): margin range by era; capbar_G;
  R_rev range by era; states with missing margin (KS/MD expected).

═══════════════════════════════════════════════════
R-IMPLEMENTATION NOTES
═══════════════════════════════════════════════════
- here(), data.table. Hard error propagation (no tryCatch->NULL).
- era_of_year MUST byte-match PM01/PM02; SCALE=10000L.
- Do NOT modify any frozen 023 object except ADDING R_rev to PM_Lookups.rds. Verify
  the existing elements are byte-unchanged (compare names() and a digest of
  pbar/tau/D before vs after the re-save).
- Companion doc: Code/Macro/README_M03.md (what it does + the kappa=1 normalization).
- Logging block per CLAUDE.md (>1 min).

═══════════════════════════════════════════════════
ACCEPTANCE CRITERIA
═══════════════════════════════════════════════════
- [ ] margin_{g,era}: all study states x 3 eras present and finite.
- [ ] capbar_G: length-4, finite, positive, monotone increasing in G.
- [ ] R_rev dim (4, n_state, 3); finite & >0 for all non-excluded states; KS/MD handled.
- [ ] PM_Lookups.rds gains R_rev with ALL prior elements byte-unchanged (digest check).
- [ ] R_rev_long.csv + README_M03.md present; log written; script exits 0.

═══════════════════════════════════════════════════
ATTEMPT LOG
═══════════════════════════════════════════════════
[blank until first attempt]
