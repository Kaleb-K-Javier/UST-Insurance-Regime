# TICKET 038 — Facility-level reduced-form portfolio analysis as a faithful roll-up of the tank closure analysis
# Created: 2026-06-27
# Status: AWAITING_IMPLEMENTATION
# Attempt: 0
# Type: REDUCED-FORM (DATA CODER seat, run_coder_pro_api.ps1). NEW standalone script.
#       Runs on SERVER (ucbare2, renv, no Z paths) — matched tank panel is server-only (4.6 GB).
#       Edit locally, commit+push; researcher git-pulls on server and runs; pulls outputs back via Z portal.
#       Does NOT touch 02b (locked foundation), 02j, or 02k. Leaves them in place.

═══════════════════════════════════════════════════
ECONOMIC MOTIVATION
═══════════════════════════════════════════════════
The approved tank closure analysis (02b) identifies the TX reform effect off a birth-CEM matched
tank sample with a clean closure event study. We want the FACILITY-level (station) portfolio story
to be a FAITHFUL AGGREGATION of that exact tank analysis — same stations, unweighted, no composition
baseline — rather than a separately-matched facility design (the old 02j/02k route with
matched_facs_birth_cem + Yhat0). The station is the decision unit for portfolio behavior (exit,
downsize, consolidate, reconfigure-up, replace), so we roll the matched tanks up to facility-years,
mirror the tank spec exactly, and read the portfolio margins on those same stations.

═══════════════════════════════════════════════════
LOCKED FOUNDATION — DO NOT CHANGE (read 02b only; do not edit it)
═══════════════════════════════════════════════════
Code/Analysis/02b_tank_closure_analysis.R, the UNWEIGHTED block (Sections 2–4, lines ~3959–4135):
  data_C        <- matched_tanks_birth_cem[install_yr_int < 1999L & cem_weight > 0]      (02b:3965)
  active-at-1998 = facilities with ≥1 open OR newly-installed tank in 1998                 (02b:3977–3980)
  data_C_active <- data_C[panel_id %in% facilities_active_1998]                            (02b:3982)
  cell FE       := .GRP by (panel_year, make_model_noage, install_yr_int)                  (02b:3987)
  did_term       = texas_treated * 1{panel_year >= 1999}                                   (matched-panel column; rebuild if absent)
  mandates       = mandate_release_det + mandate_spill_overfill + mandate_integrity        (state×year step flags; 02b:612–624)
  Static DiD step 4 (main): closure_event ~ did_term + <mandates> | panel_id + cell_vintage_year_fe   (02b:4032–4035)
  Event study (main, ref=-1): closure_event ~ i(rel_year, texas_treated, ref=-1) + <mandates>
                              | panel_id + cell_vintage_year_fe                            (02b:4095–4105, mES_full)
  cluster = ~state ; rel_year = panel_year - 1998 ; ES plots via es_one (02j:140, slide style; plot_es_clean in 02b is the same look).
Column facts (matched_tanks_birth_cem, tank×year): panel_id=facility, tank_panel_id=tank,
  closure_event (0/1), texas_treated (0/1, facility-constant), state, make_model_noage, install_yr_int,
  capacity (US gallons, numeric), the three mandate_* flags. first_year_churn filter applied at 02b:170.

═══════════════════════════════════════════════════
DESIGN (locked with researcher 2026-06-27)
═══════════════════════════════════════════════════
SAMPLE  = exactly the facilities × analysis-years of data_C_active (the matched tank sample rolled up).
          Unweighted. NEVER apply cem_weight. No Yhat0. No matched_facs_birth_cem, no fac_cem_* flags.

UNIT    = facility-year (panel_id, panel_year). One unweighted row per matched facility-year.

OUTCOMES (Decision 1 + ALL-BINARY revision 2026-06-27 per researcher):
  EVERY outcome is a 1/0 facility-year INDICATOR ("did X happen at this station this year"). No continuous
  shares: closure_share and repl_share are DROPPED, replaced by indicators (any_closure, any_replace). Why:
  the events are discrete tank moves (1, 2, 3 tanks); the "how many" treatment is a COUNT regression, deferred
  to a future ticket (FUTURE note below).
  any_closure    — PRIMARY closure mirror. Binary facility-year: as.integer(sum(closure_event) > 0) over the
                   facility's rows in data_C_active that year = "did this station have >=1 matched-tank closure
                   this year." The clean LPM parallel of the tank binary closure_event (station incidence vs
                   tank incidence); coefficient = pp change in P(station has a closure year). Facility size is
                   absorbed by panel_id + fixed composition FE, so the did identifies the differential change.
  Inherits the tank ES's at-risk row set exactly (same matched-tank rows as the tank analysis).
  The 6 portfolio margins are sourced from facility_panel.csv (the raw facility-year universe; NOT the
  CEM-matched facility object), inner/left-joined onto the matched facility-years on (panel_id, panel_year),
  because they require the facility's FULL portfolio (capacity changes, post-1999 installs/replacements)
  and DEGENERATE on the matched pre-1999 roster:
  facility_exit, downsize, consolidate, reconfigure_up (partition flags), any_replace (= 1{n_closures_replacement>0},
  replaces repl_share), cap_decrease  — Ticket-031 definitions, binarized.
  FUTURE (deferred, NOT in this ticket): COUNT regression for the discrete tank-move counts (n_closures,
  n_closures_replacement, tanks dropped) — Poisson/neg-bin FE (fixest::fepois) with the same did + FE, optional
  log(n_tanks_active) exposure offset. Own ticket later.

FIXED EFFECTS — build and COMPARE two composition specs, both from the matched roster (FIXED baseline,
  time-invariant per facility — mirrors the tank cell's time-invariance):
  A_exact     comp_A = sorted multiset of (make_model_noage, install_yr_int) over the facility's matched
                       tanks → cell_comp_year_fe_A := .GRP(panel_year, comp_A). Station analogue of the
                       tank cell make_model_noage × install_yr_int × year.
  B_coarsened comp_B = sorted multiset of (make_model_noage, install_band) where install_band bins
                       install_yr_int into 5-yr bands {<=1979, 1980–84, 1985–89, 1990–94, 1995–98};
                       make_model_noage kept EXACT → cell_comp_year_fe_B := .GRP(panel_year, comp_B).
  Report, for each version: # distinct compositions, # distinct comp×year cells, and # facility-years
  absorbed as singleton comp×year cells (size 1 → dropped by feols).
  FE in every regression = panel_id + cell_comp_year_fe_{A|B}. (panel_id is the facility id, already.)

SPEC (mirrors the tank exactly, every margin):
  static : <margin> ~ did_term + <mandates> | panel_id + cell_comp_year_fe_v          cluster ~state
  ES     : <margin> ~ i(rel_year, texas_treated, ref=-1) + <mandates> | panel_id + cell_comp_year_fe_v
  did_term = texas_treated * 1{panel_year >= 1999}. rel_year = panel_year - 1998. NO weights. NO Yhat0.

HTE (FE-version A only): interaction-only (did_term × Z), common FE = panel_id + cell_comp_year_fe_A,
  cluster ~state. Dims: size (cap_G on total_capacity_reform), vintage (fac_vintage, ref 1989–98),
  fuel (has_gasoline), spatial (gis_hte_vars.csv by panel_id: rural/low_pop/low_income/high_pov/thin_market).
  Spatial is LOCAL-ONLY → skip GRACEFULLY if gis_hte_vars.csv absent (server); core HTE still runs.

BOOTSTRAP (DEFERRED to the very end; never blocks deliverables): after ALL tables/figures are written,
  wild-cluster bootstrap by state on the STATIC did_term coefficient of each margin × each FE version,
  via fwildclusterboot::boottest (single treated cluster → the standard for this design), B = 9999.
  Write to a SEPARATE file. Do NOT bootstrap the event study. If fwildclusterboot is not installed,
  print a clear WARNING and SKIP (everything else is already saved) — this guarded optional-dependency
  skip is sanctioned for this section ONLY and is not an error-swallowing tryCatch.

═══════════════════════════════════════════════════
KEY INVARIANTS (do not deviate)
═══════════════════════════════════════════════════
I1  SAMPLE FIDELITY: reconstruct data_C_active by mirroring 02b:3965–3988 byte-for-byte (first_year_churn
    filter at load; install_yr_int<1999 & cem_weight>0; active-at-1998 union). Assert
    uniqueN(panel_id) and nrow match the figure printed by 02b for "Active — full" at run time (report both).
I2  UNWEIGHTED: no cem_weight anywhere. No weights= argument in any feols/boottest call.
I3  NO Yhat0: do not load 02k, do not construct or join any composition baseline / Yhat0 / tau column.
I4  CLOSURE outcome = any_closure = as.integer(sum(closure_event) > 0) by (panel_id, panel_year) over
    data_C_active — the matched tanks, NOT facility_panel's n_closures/n_tanks_active (a different, full-portfolio
    object). No continuous closure_share (dropped — every outcome is a 1/0 indicator).
I5  MARGINS = Ticket-031 definitions, REL_THRESH = 0.05. Before coding the partition, READ the existing
    build_margins() logic in 02j (added by Ticket 035) and PORT IT VERBATIM for facility_exit / downsize /
    consolidate / reconfigure_up / cap_decrease (verbatim) + any_replace (=1{n_closures_replacement>0}) so 02l is byte-consistent with the locked 02j
    definitions. Do not re-derive from scratch. Constants: REL_THRESH=0.05; Clag = total_capacity_dec −
    capacity_change; contraction gate = facility_exit==0 & net_tank_change<0 & Clag>0 & !is.na(capacity_change).
I6  did_term = texas_treated * as.integer(panel_year >= 1999L). Build it on the facility-year table; do not
    trust an inherited column name. mandates rolled up from data_C_active (state×year ⇒ constant within
    facility-year): take the unique value per (panel_id, panel_year) and ASSERT uniqueness.
I7  FE built PER version from the matched roster (sorted ⇒ order-invariant; collapse preserves multiplicity).
    cell_comp_year_fe_{A,B} := .GRP by (panel_year, comp_{A,B}). Same composition across years for a facility.
I8  CLUSTER = ~state for every static, ES, HTE, and bootstrap call. No exceptions.
I9  HARD ERRORS: no tryCatch(.,error→NULL), no try(silent=TRUE). The ONLY sanctioned soft path is the
    requireNamespace("fwildclusterboot") guard in the deferred bootstrap (skip-with-message), and a
    requireNamespace/file.exists guard for the LOCAL-ONLY gis_hte_vars.csv (skip spatial HTE with a message).
I10 ORDER: write ALL of {ATT csv, ES csv+figs, pub .tex, HTE csv} BEFORE the bootstrap section begins, so a
    slow/missing bootstrap cannot delay or break the main deliverables.
I11 OUTCOME LABELS: all outcomes are 1/0 facility-year indicators — label them plainly. RF_DICT already has
    any_closure, facility_exit, downsize, cap_decrease. ADD the MISSING keys (additive — do NOT change existing
    entries, which other scripts use): consolidate="Consolidate", reconfigure_up="Reconfigure-up",
    any_replace="Any replacement". Every emitted .tex / ES caption states the outcomes are indicators ("=1 if the
    station did X that year"). Plain language. (feedback_econ_journal_table_captions, feedback_writing_plain_language.)
    Label/caption text in the table-emitting code — NOT paper prose.

═══════════════════════════════════════════════════
PSEUDOCODE — Code/Analysis/02l_Facility_Rollup_Mirror.R
═══════════════════════════════════════════════════
Section 0 — Header (mirror 02j's header):
  data.table, fixest, here; source Code/Helpers/reduced_form_utils.R (rf_use_threads, RF_DICT, pub_etable,
  save_gg, OUTPUT_TABLES/OUTPUT_FIGURES path constants). NOTE: plot_es_clean is NOT in utils — the ES helper is
  es_one (copy from 02j:140; self-contained; needs ES_WIN copied from 02j). rf_use_threads(). Logging per
  CLAUDE.md (>1 min) using a FIXED script-name constant for the log path (NOT rstudioapi — fails under Rscript):
  SCRIPT_NAME <- "02l_Facility_Rollup_Mirror"; log to logs/<SCRIPT_NAME>_<ts>.log.
  ANALYSIS_DIR <- here("Data","Analysis"). Do NOT read UST_ANALYSIS_DIR.

Section 1 — Reconstruct the matched tank sample (mirror 02b:3965–3988):
  matched_tanks_birth_cem <- fread(ANALYSIS_DIR/matched_tanks_birth_cem.csv, na.strings=c("","NA"))
  apply first_year_churn==0 | is.na filter (02b:170).
  data_C <- matched_tanks_birth_cem[install_yr_int < 1999L & cem_weight > 0]
  tanks_open_1998      <- data_C[panel_year==1998L & closure_event==0L, unique(panel_id)]
  tanks_installed_1998 <- data_C[install_yr_int==1998L, unique(panel_id)]
  facilities_active_1998 <- union(tanks_open_1998, tanks_installed_1998)
  data_C_active <- data_C[panel_id %in% facilities_active_1998]
  cat: nrow(data_C_active), uniqueN(panel_id), uniqueN(tank_panel_id), uniqueN(panel_id where texas_treated==1).

Section 2 — Facility-year skeleton + closure outcomes (I4):
  cs <- data_C_active[, .(any_closure = as.integer(sum(closure_event, na.rm=TRUE) > 0),
                          n_tanks_fy = .N, n_tanks_fy_unique = uniqueN(tank_panel_id)),
                      by = .(panel_id, panel_year)]
  carry facility-constant + state×year fields by joining the unique (panel_id, panel_year) values from
  data_C_active: texas_treated, state, mandate_release_det, mandate_spill_overfill, mandate_integrity.
  ASSERT (I6) each mandate_* is unique within (panel_id, panel_year).
  fy <- cs[<carried fields>]; fy[, did_term := texas_treated * as.integer(panel_year >= 1999L)]
  fy[, rel_year := as.integer(panel_year) - 1998L]

Section 3 — Composition FE from the matched roster (I7):
  tc <- unique(data_C_active[, .(panel_id, tank_panel_id, make_model_noage, install_yr_int)], by="tank_panel_id")
  tc[, cell_A := paste(make_model_noage, install_yr_int, sep="@")]
  tc[, install_band := cut(install_yr_int, breaks=c(-Inf,1979,1984,1989,1994,1998),
                           labels=c("le79","80_84","85_89","90_94","95_98"))]
  tc[, cell_B := paste(make_model_noage, as.character(install_band), sep="@")]
  comp <- tc[, .(comp_A = paste(sort(cell_A), collapse="|"),
                 comp_B = paste(sort(cell_B), collapse="|")), by=panel_id]
  fy <- comp[fy, on="panel_id"]   # composition is facility-fixed (same across years)
  fy[, cell_comp_year_fe_A := .GRP, by=.(panel_year, comp_A)]
  fy[, cell_comp_year_fe_B := .GRP, by=.(panel_year, comp_B)]
  Diagnostics per version: uniqueN(comp_v), uniqueN cell_comp_year_fe_v, and singleton facility-years
    fy[, gsz_v := .N, by=.(panel_year, comp_v)]; n_singleton_v <- fy[gsz_v==1, .N]. cat all.

Section 4 — Portfolio margins from facility_panel.csv (I5; Decision 1):
  READ 02j build_margins() (02j:78-119) first; PORT ONLY the 6 portfolio margins. Do NOT port build_margins'
  closure_share/perm_share (02j:90-92, full-portfolio rate), post/did_term/rel_year, or cell_fac_year —
  closure_share + any_closure are the matched roll-up (Section 2) and the FE is comp_A/comp_B (Section 3). Then:
  fp <- fread(ANALYSIS_DIR/facility_panel.csv, select = the needed cols:
        panel_id, panel_year, facility_exit, net_tank_change, capacity_change, total_capacity_dec,
        n_closures, n_closures_replacement, total_capacity_reform, fac_vintage, has_gasoline,
        + any cols 02j's build_margins references)
  Build per (panel_id, panel_year): facility_exit (col), and per Ticket-031:
    Clag := total_capacity_dec - capacity_change ; rel := fifelse(Clag>0, capacity_change/Clag, NA)
    contraction := facility_exit==0L & net_tank_change<0L & Clag>0 & !is.na(capacity_change)
    consolidate    := as.integer(contraction & abs(rel) <= 0.05)
    downsize        := as.integer(contraction & rel <  -0.05)
    reconfigure_up  := as.integer(contraction & rel >   0.05)   ; NA→0L for all three
    any_replace     := as.integer(n_closures_replacement > 0L)   [1/0 indicator; NA→0L. Replaces continuous
                       repl_share. Source: facility_panel n_closures_replacement.]
    cap_decrease    := as.integer(capacity_decreased)   [VERBATIM 02j:115 — existing facility_panel column, NOT
                       as.integer(capacity_change<0)]
  ASSERT (031 I5): max(consolidate+downsize+reconfigure_up) <= 1; all three 0 where exit==1 | dN>=0 | is.na(dC).
  fy <- merge(fy, fp_margins, by=c("panel_id","panel_year"), all.x=TRUE)   # LEFT: keep any_closure full year range
  Report: # matched facility-years with no facility_panel match (should be ~0 within facility_panel's year
    coverage), and per-margin non-NA n + base rate.

Section 5 — Static DiD (all 7 margins × {A,B}):
  MARGINS <- c("any_closure","facility_exit","downsize","consolidate","reconfigure_up","any_replace","cap_decrease")
  # ALL 1/0 indicators. any_closure = PRIMARY closure mirror. (continuous closure_share/repl_share dropped;
  # counts deferred to a future count-regression ticket.)
  for v in c("A","B"): fe <- paste0("cell_comp_year_fe_", v)
    for m in MARGINS:
      dat <- fy[!is.na(get(m))]
      mod <- feols(<m> ~ did_term + mandate_release_det + mandate_spill_overfill + mandate_integrity
                   | panel_id + <fe>, data=dat, cluster=~state)
      store mod in models_static[[paste(m,v)]]; collect (margin, fe_version=v, beta=coef["did_term"],
        se=se["did_term"], p_value, n_obs=mod$nobs, n_fac=mod$fixef_sizes["panel_id"],
        n_singleton_dropped = nrow(dat) - mod$nobs)
  fwrite → T_Facility_Rollup_ATT.csv.
  STEPPED pub table for any_closure (the headline closure mirror; mirror 02b:4032–4076), each FE version: M1 ~did_term;
    M2 |panel_id; M3 +mandates|panel_id; M4 +mandates|panel_id+<fe>. pub_etable → T_DiD_Facility_Stepped_<A|B>.tex.
  Per-FE-version all-margins pub table: pub_etable(models_static for that v across MARGINS) →
    T_Facility_Rollup_ATT_Pub_<A|B>.tex (clean labels via RF_DICT).

Section 6 — Event studies + figures:
  ES_MARGINS <- c("any_closure","downsize","consolidate","any_replace"); reconfigure_up ES ONLY if
    the estimation sample has >=200 treated (texas_treated==1) post (rel_year>=0) events (else cat-skip,
    still in static+ATT csv). For v in {A,B}, m in ES_MARGINS:
    co <- es_one(<m>, ylab=<plain label>, stem=sprintf("Fig_ES_Facility_%s_%s", <Margin>, v), data=fy_es,
                 rhs="+ mandate_release_det + mandate_spill_overfill + mandate_integrity",
                 fe=paste0("panel_id + cell_comp_year_fe_", v))
      # es_one (copy 02j:140): fits i(rel_year,texas_treated,ref=-1)+rhs|fe over rel_year %between% ES_WIN,
      # saves the figure, RETURNS the coef data.table incl. the rel_year=-1 ref row (est=0,se=0).
    tag co with margin=<m>, fe_version=v, is_reference=(rel_year==-1L); rbind into es_coef_dt
      (margin, fe_version, rel_year, estimate=est, se, ci_lo, ci_hi, is_reference). fwrite → T_Facility_Rollup_ES_Coefs.csv.

Section 7 — HTE (FE-version A only; interaction-only; cluster ~state):
  HTE_MARGINS <- c("any_closure","facility_exit","downsize","consolidate","any_replace")
  size: cap_G := factor(cut(total_capacity_reform, CAP_BREAKS, labels=CAP_LABS), levels=CAP_LABS) with
    CAP_BREAKS=c(-Inf,9000,20000,30000,Inf), CAP_LABS=c("G1_lt9k","G2_9to20k","G3_20to30k","G4_gt30k") (02j:68-69).
    vintage: vintage := factor(fac_vintage); VREF="1989-1998" (02j:375); run vintage HTE only if VREF %in% levels(vintage).
  fuel: has_gasoline. spatial: GIS_HTE <- here("Output","GIS","gis_hte_vars.csv") (02j:58); if file.exists(GIS_HTE)
    join by panel_id and add rural/low_pop/
    low_income/high_pov/thin_market; else cat "GIS absent — skipping spatial HTE" and skip.
  For each dim, each m in HTE_MARGINS: feols(<m> ~ i(<dim>, did_term, ref=<ref>) + <mandates>
    | panel_id + cell_comp_year_fe_A, cluster=~state). Emit long rows via the 02j Step-6 coeftable idiom:
    (margin, dimension, level, is_reference, estimate, std_error, p_value, fe_version="A").
  fwrite → T_Facility_HTE.csv.

Section 8 — DEFERRED BOOTSTRAP (runs ONLY after Sections 5–7 have written their files; I10):
  if (!requireNamespace("fwildclusterboot", quietly=TRUE)) { cat WARNING "fwildclusterboot not installed —
    skipping deferred bootstrap; all other outputs already written"; } else {
    for v in {A,B}, m in MARGINS:
      mod <- models_static[[paste(m,v)]]
      bt  <- fwildclusterboot::boottest(mod, param="did_term", clustid=~state, B=9999L, type="rademacher")
      collect (margin, fe_version=v, beta=coef(mod)["did_term"], analytic_se=se(mod)["did_term"],
        wcb_p_value=bt$p_val, wcb_ci_lo=bt$conf_int[1], wcb_ci_hi=bt$conf_int[2], B=9999L,
        n_clusters=uniqueN(<estimation-sample state>))
    fwrite → T_Facility_Bootstrap_SEs.csv }

Section 9 — Console summary: per margin, betas for A vs B (+ p) and base rates; comp cardinalities +
  singleton counts; ES figure list; whether bootstrap ran.

═══════════════════════════════════════════════════
DELIVERABLES — ENUMERATED (rows × columns × types)
═══════════════════════════════════════════════════
1. Output/Tables/T_Facility_Rollup_ATT.csv
   ROWS: 7 margins × 2 FE versions = 14.  (margins: any_closure, facility_exit, downsize, consolidate,
     reconfigure_up, any_replace, cap_decrease)
   COLS (in order): margin(chr) | fe_version(chr "A"/"B") | beta(num, did_term) | se(num, cluster~state) |
     p_value(num) | n_obs(int facility-years) | n_fac(int) | n_singleton_dropped(int).
2. Output/Tables/T_Facility_Rollup_ES_Coefs.csv
   ROWS: one per (margin, fe_version, rel_year) for ES_MARGINS that ran (any_closure always; downsize/
     consolidate/any_replace; reconfigure_up only if ≥200 treated post events) × 2 FE × the rel_year grid.
   COLS: margin(chr) | fe_version(chr) | rel_year(int) | estimate(num) | se(num) | ci_lo(num) | ci_hi(num) |
     is_reference(lgl, TRUE at rel_year=-1).
3. Output/Figures/Fig_ES_Facility_<Margin>_<A|B>.{pdf,png}  (es_one slide style from 02j; one per ES margin×version).
4. Output/Tables/T_DiD_Facility_Stepped_<A|B>.tex  (any_closure headline, 4 stepped columns, per FE version;
   \textit{Notes:} defines any_closure and states all outcomes are 1/0 facility-year indicators, per I11).
   Output/Tables/T_Facility_Rollup_ATT_Pub_<A|B>.tex (7 margins as columns, per FE version, RF_DICT labels).
5. Output/Tables/T_Facility_HTE.csv
   ROWS: per (margin in HTE_MARGINS, dimension, level). dimensions: cap_G, vintage, has_gasoline, +spatial if present.
   COLS: margin(chr) | dimension(chr) | level(chr) | is_reference(lgl) | estimate(num) | std_error(num) |
     p_value(num) | fe_version(chr "A").
6. Output/Tables/T_Facility_Bootstrap_SEs.csv  (written LAST; absent only if package missing)
   ROWS: 7 margins × 2 FE versions = 14.
   COLS: margin(chr) | fe_version(chr) | beta(num) | analytic_se(num) | wcb_p_value(num) | wcb_ci_lo(num) |
     wcb_ci_hi(num) | B(int 9999) | n_clusters(int).

═══════════════════════════════════════════════════
R-IMPLEMENTATION NOTES
═══════════════════════════════════════════════════
- data.table + fixest only; snake_case; here()/OUTPUT_TABLES/OUTPUT_FIGURES; fwrite; save_gg.
- Reuse reduced_form_utils.R: rf_use_threads(), RF_DICT, pub_etable(), save_gg(). ES helper = es_one (copy from
  02j:140; plot_es_clean is NOT in utils — it lives in 02b and depends on extract_es_coef_dt, so don't source 02b).
- Reuse 02j's CAP_BREAKS/CAP_LABS, fac_vintage reference level (VREF), and the Step-6 coeftable→data.table
  →grepl/tstrsplit idiom for HTE long output (do not invent a new parser).
- comp strings: sort() before paste(collapse="|") so identical portfolios collide; duplicates preserved
  (multiset). Keep make_model_noage exact in BOTH versions; only install year is coarsened in B.
- n_singleton_dropped = nrow(non-NA estimation data) − mod$nobs. n_fac via mod$fixef_sizes["panel_id"].
- boottest needs the fixest object unmodified; keep models_static in memory through Section 8.
- Logging per CLAUDE.md (FIXED name, not rstudioapi). Loop checkpoints with timestamps for the long sections.
- HARD errors (I9) except the two guarded skips (fwildclusterboot present?, gis_hte_vars.csv present?).

═══════════════════════════════════════════════════
SERVER RUN (ucbare2; matched_tanks_birth_cem.csv + facility_panel.csv are server-only, in-repo Data/Analysis)
═══════════════════════════════════════════════════
  git pull origin main          # CODE only; Data/ is gitignored, panels already on server
  # from repo root so renv/.Rprofile activate (NO --vanilla); do NOT set UST_ANALYSIS_DIR:
  & "C:/Program Files/R/R-4.4.3/bin/x64/Rscript.exe" Code/Analysis/02l_Facility_Rollup_Mirror.R
  # fwildclusterboot prerequisite: if not installed, the bootstrap section self-skips with a warning and
  # all other outputs are still produced; install it later and re-run only Section 8 if the WCB SEs are wanted.
  Inspect Output/Tables + Output/Figures locally via Z:\C_Drive_Portal\.

═══════════════════════════════════════════════════
ACCEPTANCE CRITERIA
═══════════════════════════════════════════════════
- [ ] data_C_active reconstructed exactly per 02b:3965–3988 (first_year_churn filter; install<1999 & cem_weight>0;
      active-at-1998 union). Reported nrow + uniqueN(panel_id) match 02b's "Active — full" line.
- [ ] CLOSURE outcome = any_closure = as.integer(sum(closure_event)>0) by (panel_id, panel_year) over
      data_C_active rows — NOT facility_panel. Every outcome is a 1/0 indicator (no continuous closure_share/repl_share).
- [ ] UNWEIGHTED everywhere; no cem_weight; no Yhat0; no matched_facs_birth_cem / fac_cem_* / 02k.
- [ ] did_term = texas_treated*1{year>=1999}; mandates rolled up unique within facility-year (asserted); cluster ~state.
- [ ] Two composition FEs built from the matched roster (A exact, B 5-yr-band-coarsened); singleton facility-year
      counts reported for each.
- [ ] 6 portfolio margins ported from 02j build_margins / Ticket-031 (REL_THRESH=0.05; Clag=total_capacity_dec−
      capacity_change); partition mutually exclusive (max sum ≤1) and 0 where exit==1 | dN>=0 | is.na(dC).
- [ ] T_Facility_Rollup_ATT.csv: 14 rows, columns exactly as enumerated, all finite.
- [ ] T_Facility_Rollup_ES_Coefs.csv + Fig_ES_Facility_*_{A,B} produced (any_closure both versions at minimum;
      reconfigure_up ES gated ≥200 treated post events with printed skip otherwise).
- [ ] Stepped + per-version pub .tex produced (RF_DICT labels).
- [ ] T_Facility_HTE.csv produced (FE-A; spatial included iff gis_hte_vars.csv present, else printed-skip).
- [ ] ALL of {ATT, ES, .tex, HTE} written BEFORE the bootstrap section starts (I10).
- [ ] T_Facility_Bootstrap_SEs.csv: 14 rows, columns as enumerated — OR a clear WARNING + skip if
      fwildclusterboot absent (no hard failure of the script).
- [ ] No tryCatch→NULL / try(silent) except the two sanctioned requireNamespace/file.exists guards. Log written.
- [ ] Console summary printed (A-vs-B betas, base rates, comp cardinalities/singletons, bootstrap status).

═══════════════════════════════════════════════════
ATTEMPT LOG
═══════════════════════════════════════════════════
(none yet)
