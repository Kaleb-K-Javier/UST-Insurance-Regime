# Code/Macro/M02_wholesale_margin.R
# TICKET 026 — Attempt 2: sub-PADD1 probe + per-state best-available + universal floor
# Deliverables:
#   A) Data/Macro/state_region_crosswalk.csv   — 51-state PADD assignment
#   B) Data/Macro/wholesale_margin_state_year.csv — (state,year) margin 1994-2020

suppressPackageStartupMessages({
  library(here)
  library(data.table)
  library(httr)
  library(jsonlite)
})

# ══════════════════════════════════════════════════════════════════════
# LOGGING
# ══════════════════════════════════════════════════════════════════════
.log_path <- here::here("logs", paste0(
  "M02_wholesale_margin_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".log"))
dir.create(dirname(.log_path), recursive = TRUE, showWarnings = FALSE)
.log <- file(.log_path, open = "wt")
sink(.log, type = "output"); sink(.log, type = "message", append = TRUE)
on.exit({ sink(type = "output"); sink(type = "message"); close(.log) }, add = TRUE)
cat(sprintf("LOG START %s\nScript: %s\nR: %s\nWD: %s\n\n",
    .log_path, "Code/Macro/M02_wholesale_margin.R", R.version.string, getwd()))

# ══════════════════════════════════════════════════════════════════════
# CONSTANTS
# ══════════════════════════════════════════════════════════════════════
YEAR_MIN       <- 1994L
YEAR_MAX       <- 2020L
BASE_YEAR      <- 2020L
FEDERAL_EXCISE <- 0.184   # $/gal, flat since Oct 1993
MARGIN_FLOOR   <- 0.02    # universal backstop; applied after sub-PADD remap

STUDY_STATES <- c("TX","AR","CO","ID","KS","KY","LA","MA","MD","ME",
                  "MN","MO","NC","OH","OK","SD","TN","VA")

ALL_STATES <- c("AL","AK","AZ","AR","CA","CO","CT","DE","FL","GA",
                "HI","ID","IL","IN","IA","KS","KY","LA","ME","MD",
                "MA","MI","MN","MS","MO","MT","NE","NV","NH","NJ",
                "NM","NY","NC","ND","OH","OK","OR","PA","RI","SC",
                "SD","TN","TX","UT","VT","VA","WA","WV","WI","WY","DC")

stopifnot(length(ALL_STATES) == 51L)
stopifnot(all(STUDY_STATES %in% ALL_STATES))

# Era cuts — must byte-match PM01/PM02 and ticket 028
era_of_year <- function(y) {
  fifelse(y <= 2013L, "2006", fifelse(y <= 2018L, "2014", "2019"))
}

RAW_DIR <- here::here("Data", "Macro", "raw")
dir.create(RAW_DIR, recursive = TRUE, showWarnings = FALSE)

api_key <- Sys.getenv("EIA_API_KEY")
if (nchar(api_key) == 0L)
  stop(paste0(
    "No EIA_API_KEY in environment. Add to ~/.Renviron:\n",
    "  EIA_API_KEY=your_key_here\n",
    "Then restart R and re-run."))
cat(sprintf("  EIA_API_KEY present: YES\n\n"))

# ══════════════════════════════════════════════════════════════════════
# SECTION 1 — BUILD STATE → PADD CROSSWALK (skeleton)
# wholesale_area is per-state join key; PADD1 states may be updated in Section 2a.
# ══════════════════════════════════════════════════════════════════════
cat("=== SECTION 1: STATE → PADD CROSSWALK ===\n")

PADD_STATES <- list(
  `1` = c("CT","DE","DC","FL","GA","MA","MD","ME","NH","NJ",
           "NY","NC","PA","RI","SC","VA","VT","WV"),
  `2` = c("IL","IN","IA","KS","KY","MI","MN","MO","NE","ND",
           "OH","OK","SD","TN","WI"),
  `3` = c("AL","AR","LA","MS","NM","TX"),
  `4` = c("CO","ID","MT","UT","WY"),
  `5` = c("AK","AZ","CA","HI","NV","OR","WA")
)

xwalk <- rbindlist(lapply(names(PADD_STATES), function(p) {
  data.table(state = PADD_STATES[[p]], padd = as.integer(p))
}))
stopifnot(nrow(xwalk) == 51L)
stopifnot(setequal(xwalk$state, ALL_STATES))

hub_labels <- c(`1`="East Coast",`2`="Midwest",`3`="Gulf Coast",
                `4`="Rocky Mountain",`5`="West Coast")
xwalk[, padd_area_code    := paste0("R", padd, "0")]
xwalk[, wholesale_hub     := hub_labels[as.character(padd)]]
xwalk[, padd_sub          := NA_character_]  # set in Section 2a for remapped PADD1 states
xwalk[, wholesale_area    := padd_area_code] # per-state price join key; PADD1 may be updated
xwalk[, wholesale_series_id := NA_character_]
xwalk[, mapping_note      := fifelse(state %in% STUDY_STATES, "EIA official PADD", "")]

# Verify PINNED-2 study-state PADD assignments
pinned2 <- data.table(
  state    = c("MA","MD","ME","NC","VA",
               "KS","KY","MN","MO","OH","OK","SD","TN",
               "AR","LA","TX",
               "CO","ID"),
  exp_padd = c( 1L,  1L,  1L,  1L,  1L,
                2L,  2L,  2L,  2L,  2L,  2L,  2L,  2L,
                3L,  3L,  3L,
                4L,  4L)
)
chk <- merge(pinned2, xwalk[, .(state, padd)], by = "state", all.x = TRUE)
if (any(chk$padd != chk$exp_padd)) {
  bad <- chk[padd != exp_padd]
  stop(sprintf("PINNED-2 PADD mismatch for: %s", paste(bad$state, collapse=", ")))
}
cat("  PINNED-2 study-state PADD assignments verified.\n")

# PADD1 study-state → sub-PADD target (applied if that sub-PADD has resale data)
PADD1_TARGETS <- data.table(
  state    = c("MA","ME","MD","VA","NC"),
  padd_sub = c("1A","1A","1B","1B","1C")
)
cat(sprintf("  PADD1 study states targeted for sub-PADD remap: %s\n",
    paste(PADD1_TARGETS$state, collapse=", ")))

# ══════════════════════════════════════════════════════════════════════
# SECTION 2 — EIA API PROBE + PULL
# ══════════════════════════════════════════════════════════════════════
cat("=== SECTION 2: EIA API PROBE + PULL ===\n")

PRODUCT_CODE <- "EPM0"  # Total Gasoline (all grades)
PROCESS_CODE <- "PWG"   # Wholesale/Resale by Refiners and Gas Plants

# ── 2a: Probe sub-PADD1 candidate area codes directly ────────────────
# Use hardcoded EIA candidate codes (R11=1A, R12=1B, R13=1C — standard v2 format).
# The facet/duoarea/ metadata endpoint does not reliably enumerate sub-area codes
# for all product/process combinations, so we probe the data endpoint directly.
cat("  [2a] Probing sub-PADD1 candidate area codes (R11=1A, R12=1B, R13=1C) ...\n")

SUBPADD1_CANDIDATES <- data.table(
  area_code = c("R11", "R12", "R13"),
  padd_sub  = c("1A",  "1B",  "1C")
)
cat(sprintf("  Candidates: %s\n",
    paste(paste0(SUBPADD1_CANDIDATES$padd_sub, "=", SUBPADD1_CANDIDATES$area_code),
          collapse=", ")))

# ── 2b: Confirm Sales-for-Resale data exists at each candidate ────────
cat("  [2b] Probing each sub-PADD1 candidate for resale data ...\n")

probe_has_data <- function(area_code) {
  resp <- httr::GET(
    "https://api.eia.gov/v2/petroleum/pri/refmg/data/",
    query = list(
      api_key              = api_key,
      frequency            = "monthly",
      `data[0]`            = "value",
      `facets[duoarea][0]` = area_code,
      `facets[product][0]` = PRODUCT_CODE,
      `facets[process][0]` = PROCESS_CODE,
      start                = sprintf("%d-01", YEAR_MIN),
      end                  = sprintf("%d-12", YEAR_MAX),
      length               = 5L,
      offset               = 0L
    )
  )
  httr::stop_for_status(resp)
  body <- jsonlite::fromJSON(
    httr::content(resp, as = "text", encoding = "UTF-8"),
    simplifyVector = TRUE
  )
  n_total <- as.integer(body$response$total)
  cat(sprintf("    %s: total rows = %d %s\n", area_code, n_total,
              if (n_total > 0L) "[HAS DATA]" else "[NO DATA]"))
  n_total > 0L
}

subpadd1_usable <- data.table(area_code = character(), padd_sub = character())
for (i in seq_len(nrow(SUBPADD1_CANDIDATES))) {
  acode <- SUBPADD1_CANDIDATES$area_code[i]
  asub  <- SUBPADD1_CANDIDATES$padd_sub[i]
  if (probe_has_data(acode)) {
    subpadd1_usable <- rbind(subpadd1_usable,
      data.table(area_code = acode, padd_sub = asub))
  }
}
cat(sprintf("  Sub-PADD1 areas with resale data: %d\n", nrow(subpadd1_usable)))
if (nrow(subpadd1_usable) > 0L) print(subpadd1_usable)

# ── 2c: Remap PADD1 study states — per-state best-available ──────────
cat("  [2c] Remapping crosswalk (per-state best-available) ...\n")

if (nrow(subpadd1_usable) > 0L) {
  # For each PADD1 study state: use sub-PADD if its sub-area has data, else keep R10
  remap <- merge(PADD1_TARGETS, subpadd1_usable, by = "padd_sub", all.x = TRUE)
  remap_ok <- remap[!is.na(area_code)]

  for (i in seq_len(nrow(remap_ok))) {
    st    <- remap_ok$state[i]
    asub  <- remap_ok$padd_sub[i]
    acode <- remap_ok$area_code[i]
    xwalk[state == st, `:=`(
      padd_sub       = asub,
      wholesale_area = acode,
      wholesale_hub  = paste0("PADD ", asub),
      mapping_note   = sprintf(
        "EIA official PADD; sub-PADD %s series used (finer regional wholesale, attempt-2 fix)",
        asub)
    )]
    cat(sprintf("    %s -> %s (%s) [sub-PADD]\n", st, acode, asub))
  }

  no_remap <- PADD1_TARGETS$state[!PADD1_TARGETS$state %in% remap_ok$state]
  if (length(no_remap) > 0L)
    cat(sprintf("    %s -> R10 [sub-PADD not available; PADD1-wide fallback]\n",
        paste(no_remap, collapse=", ")))
} else {
  cat("  No sub-PADD1 resale data found; all PADD1 states stay on R10.\n")
  cat(sprintf("  Universal floor (pmax %.2f) will handle any negative margins.\n", MARGIN_FLOOR))
}

# ── 2d: Pull monthly wholesale for all distinct wholesale_area codes ──
cat("  [2d] Pulling monthly wholesale for all area codes ...\n")

all_areas <- sort(unique(xwalk$wholesale_area))
cat(sprintf("  Distinct area codes: %s\n", paste(all_areas, collapse=", ")))

pull_monthly_bulk <- function(area_codes) {
  q <- list(
    api_key              = api_key,
    frequency            = "monthly",
    `data[0]`            = "value",
    `facets[product][0]` = PRODUCT_CODE,
    `facets[process][0]` = PROCESS_CODE,
    start                = sprintf("%d-01", YEAR_MIN),
    end                  = sprintf("%d-12", YEAR_MAX),
    length               = 15000L,
    offset               = 0L
  )
  for (j in seq_along(area_codes))
    q[[sprintf("facets[duoarea][%d]", j - 1L)]] <- area_codes[j]

  resp <- httr::GET("https://api.eia.gov/v2/petroleum/pri/refmg/data/", query = q)
  httr::stop_for_status(resp)
  body <- jsonlite::fromJSON(
    httr::content(resp, as = "text", encoding = "UTF-8"),
    simplifyVector = TRUE
  )
  total_api <- as.integer(body$response$total)
  rows      <- as.data.table(body$response$data)
  cat(sprintf("  Pull: %d rows returned (API total=%d)\n", nrow(rows), total_api))

  # Paginate if the API returned fewer rows than total
  if (total_api > nrow(rows)) {
    all_chunks <- list(rows)
    offset_cur <- nrow(rows)
    while (offset_cur < total_api) {
      q$offset <- offset_cur
      r2 <- httr::GET("https://api.eia.gov/v2/petroleum/pri/refmg/data/", query = q)
      httr::stop_for_status(r2)
      b2    <- jsonlite::fromJSON(httr::content(r2, as="text", encoding="UTF-8"),
                                  simplifyVector = TRUE)
      chunk <- as.data.table(b2$response$data)
      all_chunks <- c(all_chunks, list(chunk))
      offset_cur <- offset_cur + nrow(chunk)
      cat(sprintf("  Paginated: +%d rows (cumulative %d / %d)\n",
          nrow(chunk), offset_cur, total_api))
      if (nrow(chunk) == 0L) break
    }
    rows <- rbindlist(all_chunks, fill = TRUE)
    cat(sprintf("  Final after pagination: %d rows\n", nrow(rows)))
  }
  rows
}

raw_w <- pull_monthly_bulk(all_areas)
cat("  Columns:", paste(names(raw_w), collapse=", "), "\n")
cat("  Sample rows:\n"); print(head(raw_w, 6))

SOURCE_WHOLESALE <- sprintf("EIA_refmg_API_%s_product=%s_process=%s",
                             format(Sys.Date(), "%Y-%m"), PRODUCT_CODE, PROCESS_CODE)

# ── 2e: Extract series IDs from response and update crosswalk ─────────
series_col <- if ("series" %in% names(raw_w)) "series" else NULL
if (!is.null(series_col)) {
  sid_map <- unique(raw_w[, .(wholesale_area = duoarea,
                               wholesale_series_id = get(series_col))])
  sid_map <- sid_map[, .(wholesale_series_id = wholesale_series_id[1L]),
                     by = wholesale_area]
  cat("  Series IDs by area:\n"); print(sid_map)
  xwalk <- merge(xwalk, sid_map, by = "wholesale_area", all.x = TRUE,
                 suffixes = c("_old", ""))
  if ("wholesale_series_id_old" %in% names(xwalk)) xwalk[, wholesale_series_id_old := NULL]
} else {
  xwalk[, wholesale_series_id := sprintf(
    "EIA-petroleum/pri/refmg-%s-%s-%s-monthly",
    wholesale_area, PRODUCT_CODE, PROCESS_CODE)]
  cat("  Series IDs constructed (no 'series' column in response).\n")
}

# ══════════════════════════════════════════════════════════════════════
# SECTION 3 — COLLAPSE MONTHLY → ANNUAL MEAN BY wholesale_area
# ══════════════════════════════════════════════════════════════════════
cat("=== SECTION 3: MONTHLY → ANNUAL ===\n")

stopifnot("duoarea" %in% names(raw_w),
          "period"  %in% names(raw_w),
          "value"   %in% names(raw_w))

raw_w[, value := suppressWarnings(as.numeric(value))]
raw_w[, year  := as.integer(substr(as.character(period), 1L, 4L))]
raw_w <- raw_w[year %in% YEAR_MIN:YEAR_MAX & duoarea %in% all_areas]

month_count <- raw_w[!is.na(value), .(n_months = .N), by = .(duoarea, year)]
thin_areas  <- month_count[n_months < 12L]
if (nrow(thin_areas) > 0L) {
  cat("  WARNING: area-years with < 12 monthly observations (na.rm used):\n")
  print(thin_areas)
} else {
  cat("  All area-years have 12 monthly observations.\n")
}

wholesale_ann <- raw_w[!is.na(value),
  .(wholesale_price_usd_gal = mean(value, na.rm = TRUE)),
  by = .(wholesale_area = duoarea, year)]

expected_ann_rows <- length(all_areas) * (YEAR_MAX - YEAR_MIN + 1L)
cat(sprintf("  Annual area-year rows: %d (expect %d for %d areas x %d years)\n",
    nrow(wholesale_ann), expected_ann_rows, length(all_areas),
    YEAR_MAX - YEAR_MIN + 1L))
cat(sprintf("  Wholesale price range: %.3f – %.3f $/gal\n",
    min(wholesale_ann$wholesale_price_usd_gal),
    max(wholesale_ann$wholesale_price_usd_gal)))

# ══════════════════════════════════════════════════════════════════════
# SECTION 4 — JOIN RETAIL + TAX FROM TICKET 025
# ══════════════════════════════════════════════════════════════════════
cat("=== SECTION 4: JOIN RETAIL + TAX ===\n")

seds_path <- here::here("Data", "Macro", "seds_gasoline_state_year.csv")
if (!file.exists(seds_path))
  stop(sprintf("Ticket 025 output not found: %s\nRun M01_SEDS_tax.R first.", seds_path))

seds <- fread(seds_path, select = c("state","year",
                                     "gas_price_retail_usd_gal",
                                     "gas_tax_state_usd_gal"))
seds <- seds[state %in% ALL_STATES & year %in% YEAR_MIN:YEAR_MAX]
cat(sprintf("  Loaded seds: %d rows, %d states, years %d-%d\n",
    nrow(seds), uniqueN(seds$state), min(seds$year), max(seds$year)))

# Join per-state info from crosswalk (including wholesale_area)
state_info <- xwalk[, .(state, padd, padd_sub, wholesale_area, wholesale_hub,
                         wholesale_series_id, mapping_note)]
margin_dt <- merge(seds, state_info, by = "state", all.x = TRUE)
stopifnot(nrow(margin_dt) == nrow(seds))

# Join annual wholesale by (wholesale_area, year) — each state uses its own area
margin_dt <- merge(margin_dt, wholesale_ann,
                   by = c("wholesale_area", "year"), all.x = TRUE)
cat(sprintf("  After wholesale join: %d rows, NA wholesale: %d\n",
    nrow(margin_dt), sum(is.na(margin_dt$wholesale_price_usd_gal))))

# ══════════════════════════════════════════════════════════════════════
# SECTION 5 — SANITY: retail - wholesale (taxes still included)
# ══════════════════════════════════════════════════════════════════════
cat("=== SECTION 5: SANITY CHECK ===\n")

san <- margin_dt[state %in% STUDY_STATES & year %in% 1999L:YEAR_MAX &
                 !is.na(gas_price_retail_usd_gal) & !is.na(wholesale_price_usd_gal)]
san[, raw_spread := gas_price_retail_usd_gal - wholesale_price_usd_gal]
san_area_yr <- san[, .(raw_spread = mean(raw_spread, na.rm = TRUE)),
                   by = .(wholesale_area, year)]

rng_lo <- min(san_area_yr$raw_spread, na.rm = TRUE)
rng_hi <- max(san_area_yr$raw_spread, na.rm = TRUE)
cat(sprintf("  retail − wholesale (taxes included): %.3f – %.3f $/gal (expect $0.20-1.00)\n",
    rng_lo, rng_hi))

if (any(san_area_yr$raw_spread < 0, na.rm = TRUE)) {
  bad <- san_area_yr[raw_spread < 0]
  stop(sprintf(
    "SANITY FAIL: retail < wholesale in %d area-years. First: area=%s year=%d spread=%.3f\n"  ,
    nrow(bad), bad$wholesale_area[1], bad$year[1], bad$raw_spread[1]))
}
if (any(san_area_yr$raw_spread > 1.50, na.rm = TRUE)) {
  bad <- san_area_yr[raw_spread > 1.50]
  stop(sprintf(
    "SANITY FAIL: retail − wholesale > $1.50 in %d area-years. First: area=%s year=%d spread=%.3f",
    nrow(bad), bad$wholesale_area[1], bad$year[1], bad$raw_spread[1]))
}
cat("  SANITY PASS: all area-year spreads in [0, 1.50] $/gal.\n")

# ══════════════════════════════════════════════════════════════════════
# SECTION 6 — COMPUTE MARGIN, RECOMPUTE CHECK, UNIVERSAL FLOOR
# ══════════════════════════════════════════════════════════════════════
cat("=== SECTION 6: MARGIN + FLOOR ===\n")

margin_dt[, federal_excise_usd_gal := FEDERAL_EXCISE]

# Step 1: raw margin (exact formula — recompute check done here)
margin_dt[, margin_usd_gal :=
    gas_price_retail_usd_gal -
    wholesale_price_usd_gal  -
    gas_tax_state_usd_gal    -
    federal_excise_usd_gal]

# Recompute identity check (pre-floor — this is the "exact" assertion)
margin_dt[, margin_check :=
    gas_price_retail_usd_gal -
    wholesale_price_usd_gal  -
    gas_tax_state_usd_gal    -
    federal_excise_usd_gal]
chk_rows <- margin_dt[!is.na(margin_usd_gal) & !is.na(margin_check)]
max_dev  <- max(abs(chk_rows$margin_usd_gal - chk_rows$margin_check), na.rm = TRUE)
stopifnot(max_dev < 1e-9)
cat(sprintf("  Recompute check (pre-floor): max deviation = %.2e PASS\n", max_dev))
margin_dt[, margin_check := NULL]

# Report pre-floor negatives (not dropped, per spec)
n_neg_raw <- sum(margin_dt$margin_usd_gal < 0, na.rm = TRUE)
cat(sprintf("  Pre-floor negative-margin rows: %d (reported, not dropped)\n", n_neg_raw))
if (n_neg_raw > 0L) {
  neg_study <- margin_dt[state %in% STUDY_STATES & !is.na(margin_usd_gal) &
                          margin_usd_gal < 0, .(state, year, wholesale_area, margin_usd_gal)]
  if (nrow(neg_study) > 0L) { cat("  Study-state pre-floor negatives:\n"); print(neg_study) }
}

# Step 2: universal floor backstop
margin_dt[, margin_floored := !is.na(margin_usd_gal) & margin_usd_gal < MARGIN_FLOOR]
margin_dt[, margin_usd_gal := pmax(margin_usd_gal, MARGIN_FLOOR)]

n_floored  <- sum(margin_dt$margin_floored, na.rm = TRUE)
n_non_na   <- sum(!is.na(margin_dt$margin_usd_gal))
cat(sprintf("  Floor applied (pmax %.2f): %d / %d rows (%.1f%%)\n",
    MARGIN_FLOOR, n_floored, n_non_na, 100 * n_floored / n_non_na))
if (n_floored > 0L) {
  floored_study <- margin_dt[margin_floored == TRUE & state %in% STUDY_STATES,
                              .(state, year, wholesale_area, margin_usd_gal)]
  if (nrow(floored_study) > 0L) {
    cat("  Floored rows in study states (post-floor values shown):\n")
    print(floored_study)
  }
}
cat(sprintf("  Post-floor margin range: %.3f – %.3f $/gal\n",
    min(margin_dt$margin_usd_gal, na.rm = TRUE),
    max(margin_dt$margin_usd_gal, na.rm = TRUE)))

# ══════════════════════════════════════════════════════════════════════
# SECTION 7 — CPI DEFLATOR (auxiliary real margin)
# ══════════════════════════════════════════════════════════════════════
cat("=== SECTION 7: CPI DEFLATOR ===\n")

fred_path <- here::here("Data", "Macro", "raw", "fred_cpiaucsl.csv")
if (!file.exists(fred_path)) {
  cat(sprintf("  Downloading FRED CPI-U (CPIAUCSL) to %s ...\n", fred_path))
  download.file("https://fred.stlouisfed.org/graph/fredgraph.csv?id=CPIAUCSL",
                fred_path, mode = "wb", quiet = FALSE)
  cat(sprintf("  Saved %.2f MB\n", file.size(fred_path) / 1e6))
} else {
  cat(sprintf("  Using cached CPI: %s\n", fred_path))
}

cpi_raw <- fread(fred_path)
setnames(cpi_raw, names(cpi_raw), c("date", "cpi"))
cpi_raw[, date := as.Date(date)]
cpi_raw[, year := as.integer(format(date, "%Y"))]
cpi_raw[, cpi  := suppressWarnings(as.numeric(cpi))]
cpi_raw <- cpi_raw[!is.na(cpi) & cpi > 0]

cpi_ann  <- cpi_raw[year %in% YEAR_MIN:YEAR_MAX, .(cpi_avg = mean(cpi)), by = year]
cpi_2020 <- cpi_ann[year == BASE_YEAR, cpi_avg]
stopifnot(length(cpi_2020) == 1L, is.finite(cpi_2020))
cpi_ann[, deflator_to_2020 := cpi_2020 / cpi_avg]
cat(sprintf("  CPI-U 2020 avg: %.3f; deflator range: %.3f – %.3f\n",
    cpi_2020, min(cpi_ann$deflator_to_2020), max(cpi_ann$deflator_to_2020)))

margin_dt <- merge(margin_dt, cpi_ann[, .(year, deflator_to_2020)], by = "year", all.x = TRUE)
margin_dt[, margin_real_usd_gal_2020 := margin_usd_gal * deflator_to_2020]
margin_dt[, deflator_to_2020 := NULL]

# ══════════════════════════════════════════════════════════════════════
# SECTION 8 — VALIDATE (study states 1999-2020)
# ══════════════════════════════════════════════════════════════════════
cat("=== SECTION 8: VALIDATION ===\n")

study_core <- margin_dt[state %in% STUDY_STATES & year %in% 1999L:YEAR_MAX]
na_margin  <- study_core[is.na(margin_usd_gal)]
if (nrow(na_margin) > 0L) {
  cat("  NA margin rows:\n"); print(na_margin[, .(state, year, wholesale_area)])
  stop(sprintf("VALIDATION FAIL: %d NA margin-years for study states 1999-2020.", nrow(na_margin)))
}
cat(sprintf("  No NA margins for study states 1999-2020 (n=%d). PASS\n", nrow(study_core)))

expected_n <- length(STUDY_STATES) * (YEAR_MAX - 1999L + 1L)
stopifnot(nrow(study_core) == expected_n)
cat(sprintf("  Row count: %d = %d states x 22 years. PASS\n",
    nrow(study_core), length(STUDY_STATES)))

study_xwalk <- xwalk[state %in% STUDY_STATES]
if (any(nchar(study_xwalk$mapping_note) == 0L))
  stop(sprintf("Empty mapping_note for: %s",
    paste(study_xwalk[nchar(mapping_note) == 0L, state], collapse=", ")))
cat("  All study states have non-empty mapping_note. PASS\n")

stopifnot(!anyNA(xwalk$wholesale_series_id))
stopifnot(uniqueN(xwalk[, .(state, wholesale_series_id)]) == 51L)
cat("  All 51 states have a unique wholesale_series_id. PASS\n")

# ══════════════════════════════════════════════════════════════════════
# SECTION 9 — ERA MATRIX VERIFICATION (Step D; 18 states x 3 eras)
# ══════════════════════════════════════════════════════════════════════
cat("=== SECTION 9: ERA MATRIX VERIFICATION ===\n")

study_core[, era := era_of_year(year)]
era_matrix <- study_core[,
  .(era_margin = mean(margin_usd_gal, na.rm = TRUE)),
  by = .(state, era)]
stopifnot(nrow(era_matrix) == length(STUDY_STATES) * 3L)

min_era_margin <- min(era_matrix$era_margin, na.rm = TRUE)
cat(sprintf("  State-era margin range: %.4f – %.4f $/gal\n",
    min_era_margin, max(era_matrix$era_margin, na.rm = TRUE)))

if (min_era_margin <= 0) {
  bad_cells <- era_matrix[era_margin <= 0]
  stop(sprintf("ERA-MARGIN FAIL: %d state-era cells <= 0:\n%s",
    nrow(bad_cells), paste(capture.output(print(bad_cells)), collapse="\n")))
}
cat(sprintf("  PASS: min state-era margin = %.4f > 0.\n", min_era_margin))

# NC: which path and three era values
nc_wholesale_area <- xwalk[state == "NC", wholesale_area]
nc_padd_sub       <- xwalk[state == "NC", padd_sub]
nc_floored_rows   <- sum(margin_dt[state == "NC", margin_floored], na.rm = TRUE)
nc_path <- if (!is.na(nc_padd_sub))
  sprintf("sub-PADD %s (area=%s)", nc_padd_sub, nc_wholesale_area) else
  sprintf("PADD1-wide R10 + floor (%d rows floored)", nc_floored_rows)
cat(sprintf("  NC path: %s\n", nc_path))
nc_eras <- era_matrix[state == "NC"][order(era)]
cat("  NC era margins:\n"); print(nc_eras)
stopifnot(all(nc_eras$era_margin > 0))
cat("  NC all eras > 0. PASS\n")

# Full 18x3 matrix (wide) for researcher review
era_wide <- dcast(era_matrix, state ~ era, value.var = "era_margin")
setcolorder(era_wide, c("state","2006","2014","2019"))
cat("  Full 18x3 state-era margin matrix ($/gal nominal, post-floor):\n")
print(era_wide, digits = 4L)

# ══════════════════════════════════════════════════════════════════════
# SECTION 10 — ASSEMBLE DELIVERABLE B
# ══════════════════════════════════════════════════════════════════════
cat("=== SECTION 10: ASSEMBLE DELIVERABLE B ===\n")

margin_dt[, source_wholesale := SOURCE_WHOLESALE]

out_b <- margin_dt[, .(
  state                    = state,
  year                     = year,
  wholesale_price_usd_gal  = wholesale_price_usd_gal,
  retail_price_usd_gal     = gas_price_retail_usd_gal,
  gas_tax_state_usd_gal    = gas_tax_state_usd_gal,
  federal_excise_usd_gal   = federal_excise_usd_gal,
  margin_usd_gal           = margin_usd_gal,
  margin_floored           = margin_floored,
  margin_real_usd_gal_2020 = margin_real_usd_gal_2020,
  source_wholesale         = source_wholesale
)]
setorder(out_b, state, year)
cat(sprintf("  Deliverable B: %d rows x %d cols\n", nrow(out_b), ncol(out_b)))

summ <- out_b[state %in% STUDY_STATES & year %in% 1999L:YEAR_MAX,
  .(min = min(margin_usd_gal), mean = mean(margin_usd_gal),
    max = max(margin_usd_gal), n_floored = sum(margin_floored)),
  by = state]
cat("  Study-state margin summary (1999-2020, $/gal nominal):\n"); print(summ)

# ══════════════════════════════════════════════════════════════════════
# SECTION 11 — ASSEMBLE DELIVERABLE A (crosswalk)
# ══════════════════════════════════════════════════════════════════════
cat("=== SECTION 11: ASSEMBLE DELIVERABLE A ===\n")

out_a <- xwalk[, .(
  state               = state,
  padd                = padd,
  padd_sub            = padd_sub,
  wholesale_series_id = wholesale_series_id,
  wholesale_hub       = wholesale_hub,
  mapping_note        = mapping_note
)]
setorder(out_a, state)
stopifnot(nrow(out_a) == 51L)
stopifnot(!anyNA(out_a$wholesale_series_id))
stopifnot(all(out_a[state %in% STUDY_STATES, nchar(mapping_note) > 0L]))

cat("  FULL CROSSWALK (printed for researcher review):\n")
print(out_a, nrows = 60L)

# ══════════════════════════════════════════════════════════════════════
# SECTION 12 — WRITE DELIVERABLES
# ══════════════════════════════════════════════════════════════════════
cat("=== SECTION 12: WRITE FILES ===\n")

out_dir <- here::here("Data", "Macro")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
path_a <- file.path(out_dir, "state_region_crosswalk.csv")
path_b <- file.path(out_dir, "wholesale_margin_state_year.csv")

fwrite(out_a, path_a)
fwrite(out_b, path_b)
cat(sprintf("  Saved A: %s (%d rows)\n", path_a, nrow(out_a)))
cat(sprintf("  Saved B: %s (%d rows)\n", path_b, nrow(out_b)))

cat("\n=== M02 COMPLETE ===\n")
cat(sprintf("  Source: %s\n", SOURCE_WHOLESALE))
cat(sprintf("  Sub-PADD1 used: %s\n",
    if (nrow(subpadd1_usable) > 0L)
      paste(paste0(subpadd1_usable$padd_sub, "=", subpadd1_usable$area_code), collapse=", ")
    else "none"))
cat(sprintf("  Rows floored (pmax %.2f): %d\n", MARGIN_FLOOR, n_floored))
cat(sprintf("  Min state-era margin: %.4f $/gal\n", min_era_margin))
cat(sprintf("  NC path: %s\n", nc_path))
cat(sprintf("  Log: %s\n", .log_path))
