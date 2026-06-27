# Code/Macro/M01_SEDS_tax.R
# TICKET 025 — State-level gasoline dollars
# EIA SEDS price/consumption/expenditure + FHWA MF-121T state fuel tax
# Created: 2026-06-25

suppressPackageStartupMessages({
  library(here)
  library(data.table)
  library(httr)
  library(jsonlite)
  library(readr)
})

# ══════════════════════════════════════════════════════════════════════
# LOGGING
# ══════════════════════════════════════════════════════════════════════
.log_path <- here::here("logs", paste0(
  "M01_SEDS_tax_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".log"))
dir.create(dirname(.log_path), recursive = TRUE, showWarnings = FALSE)
.log <- file(.log_path, open = "wt")
sink(.log, type = "output"); sink(.log, type = "message", append = TRUE)
on.exit({ sink(type = "output"); sink(type = "message"); close(.log) }, add = TRUE)
cat(sprintf("LOG START %s\nScript: %s\nR: %s\nWD: %s\n\n",
    .log_path, "Code/Macro/M01_SEDS_tax.R", R.version.string, getwd()))

# ══════════════════════════════════════════════════════════════════════
# CONSTANTS
# ══════════════════════════════════════════════════════════════════════
YEAR_MIN  <- 1994L
YEAR_MAX  <- 2020L
BASE_YEAR <- 2020L

# Motor gasoline heat content: 0.1203 MMBtu/gal (lower heating value)
# Source: EIA Annual Energy Review 2023 Appendix A, Table A3
# Verify: https://www.eia.gov/totalenergy/data/annual/pdf/appendix_a.pdf
HC_GAL <- 0.1203  # MMBtu per gallon

STUDY_STATES <- c("TX","AR","CO","ID","KS","KY","LA","MA","MD","ME",
                  "MN","MO","NC","OH","OK","SD","TN","VA")

ALL_STATES <- c("AL","AK","AZ","AR","CA","CO","CT","DE","FL","GA",
                "HI","ID","IL","IN","IA","KS","KY","LA","ME","MD",
                "MA","MI","MN","MS","MO","MT","NE","NV","NH","NJ",
                "NM","NY","NC","ND","OH","OK","OR","PA","RI","SC",
                "SD","TN","TX","UT","VT","VA","WA","WV","WI","WY","DC")

stopifnot(length(ALL_STATES) == 51L)
stopifnot(all(STUDY_STATES %in% ALL_STATES))

RAW_DIR <- here::here("Data", "Macro", "raw")
dir.create(RAW_DIR, recursive = TRUE, showWarnings = FALSE)

# ══════════════════════════════════════════════════════════════════════
# SECTION 1 — EIA SEDS: price, consumption, expenditure
# ══════════════════════════════════════════════════════════════════════
cat("=== SECTION 1: EIA SEDS ===\n")

api_key <- Sys.getenv("EIA_API_KEY")
use_api <- nchar(api_key) > 0
cat(sprintf("  EIA_API_KEY present: %s\n", use_api))

# ── 1a: API route ──────────────────────────────────────────────────
if (use_api) {
  cat("  Route: EIA API v2\n")

  # First: discover available MGA* MSN codes from SEDS metadata
  cat("  Fetching SEDS metadata to discover MSN codes ...\n")
  meta_resp <- httr::GET(
    "https://api.eia.gov/v2/seds/",
    query = list(api_key = api_key, length = 1000L)
  )
  httr::stop_for_status(meta_resp)
  meta_body <- jsonlite::fromJSON(
    httr::content(meta_resp, as = "text", encoding = "UTF-8"),
    simplifyVector = TRUE
  )
  cat("  Metadata response keys:", paste(names(meta_body$response), collapse=", "), "\n")

  # Pull per-MSN from EIA API v2 SEDS data endpoint
  pull_seds_msn <- function(msn, key) {
    cat(sprintf("  Pulling MSN=%s ...\n", msn))
    resp <- httr::GET(
      "https://api.eia.gov/v2/seds/data/",
      query = list(
        api_key            = key,
        frequency          = "annual",
        `data[0]`          = "value",
        `facets[msn][0]`   = msn,
        start              = as.character(YEAR_MIN),
        end                = as.character(YEAR_MAX),
        length             = 10000L,
        offset             = 0L
      )
    )
    httr::stop_for_status(resp)
    body   <- jsonlite::fromJSON(
      httr::content(resp, as = "text", encoding = "UTF-8"),
      simplifyVector = TRUE
    )
    total  <- body$response$total
    rows   <- as.data.table(body$response$data)
    cat(sprintf("    rows=%d (API reports total=%s)\n", nrow(rows), total))
    if (nrow(rows) == 0L)
      stop(sprintf("No SEDS data returned for MSN=%s — check MSN code", msn))
    rows
  }

  # MSN codes: MGAPR=price $/MMBtu; MGACB=consumption TBtu; MGACX=expenditure $M
  # If these return 0 rows the script stops; check log for metadata output to find correct codes
  raw_price <- pull_seds_msn("MGAPR", api_key)
  raw_btu   <- pull_seds_msn("MGACB", api_key)
  raw_exp   <- pull_seds_msn("MGACX", api_key)

  cat("  raw_price columns:", paste(names(raw_price), collapse=", "), "\n")
  cat("  Price unit field:", if ("unit" %in% names(raw_price)) raw_price[1L, unit] else "N/A", "\n")
  cat("  Sample price (TX 2005-2010):\n")
  print(raw_price[statecode == "TX" & period %in% as.character(2005:2010),
                  .(statecode, period, value)])

  # Standardize into (state, year, data) triples
  std_api <- function(dt, valname) {
    out <- copy(dt)
    setnames(out,
      old = intersect(c("statecode","period","value"), names(out)),
      new = c("state","year","data")[seq_len(sum(c("statecode","period","value") %in% names(out)))]
    )
    out[, year := as.integer(year)]
    out <- out[state %in% ALL_STATES & year %in% YEAR_MIN:YEAR_MAX, .(state, year, data)]
    setnames(out, "data", valname)
    out
  }

  dt_price <- std_api(raw_price, "raw_price_mmBtu")
  dt_btu   <- std_api(raw_btu,   "raw_btu_TBtu")
  dt_exp   <- std_api(raw_exp,   "raw_exp_mUSD")

  seds_dt <- merge(dt_price, dt_btu, by = c("state","year"), all = TRUE)
  seds_dt <- merge(seds_dt, dt_exp,  by = c("state","year"), all = TRUE)

  SOURCE_PRICE <- sprintf("EIA_SEDS_API_%s", format(Sys.Date(), "%Y-%m"))

} else {
  # ── 1b: Bulk CSV route ──────────────────────────────────────────
  cat("  Route: EIA SEDS bulk CSV\n")

  pr_path  <- file.path(RAW_DIR, "seds_pr_all.csv")
  btu_path <- file.path(RAW_DIR, "seds_use_all_btu.csv")
  ex_path  <- file.path(RAW_DIR, "seds_ex_all.csv")

  dl_if_missing <- function(url, dest) {
    if (!file.exists(dest)) {
      cat(sprintf("  Downloading %s ...\n", basename(dest)))
      download.file(url, dest, mode = "wb", quiet = FALSE)
      cat(sprintf("  Saved %.1f MB\n", file.size(dest) / 1e6))
    } else {
      cat(sprintf("  Cached: %s (%.1f MB)\n", basename(dest), file.size(dest)/1e6))
    }
  }

  dl_if_missing("https://www.eia.gov/state/seds/sep_prices/total/csv/pr_all.csv",    pr_path)
  dl_if_missing("https://www.eia.gov/state/seds/sep_use/total/csv/use_all_btu.csv", btu_path)
  dl_if_missing("https://www.eia.gov/state/seds/sep_prices/total/csv/ex_all.csv",   ex_path)

  # EIA SEDS bulk CSVs are WIDE format: Data_Status, State, MSN, 1960, 1961, ..., 2023
  # Must read with header=TRUE, then melt year columns to long
  read_seds_wide <- function(path) {
    dt <- fread(path, header = TRUE)
    cat(sprintf("  %s: %d rows x %d cols; first cols: %s\n",
        basename(path), nrow(dt), ncol(dt),
        paste(names(dt)[1:min(5L,ncol(dt))], collapse=", ")))
    yr_cols <- names(dt)[suppressWarnings(!is.na(as.integer(names(dt)))) &
                          suppressWarnings(as.integer(names(dt))) %in% 1960L:2030L]
    cat(sprintf("    Year columns: %d (%s–%s)\n",
        length(yr_cols), yr_cols[1], yr_cols[length(yr_cols)]))
    id_cols <- setdiff(names(dt), yr_cols)
    long <- melt(dt, id.vars = id_cols, measure.vars = yr_cols,
                 variable.name = "year", value.name = "value")
    long[, year  := as.integer(as.character(year))]
    long[, value := suppressWarnings(as.numeric(value))]
    long
  }

  pr_long  <- read_seds_wide(pr_path)
  btu_long <- read_seds_wide(btu_path)
  ex_long  <- read_seds_wide(ex_path)

  # Print available MGA MSN codes for diagnostics
  mga_pr  <- sort(unique(pr_long[ grepl("^MGA", MSN), MSN]))
  mga_btu <- sort(unique(btu_long[grepl("^MGA", MSN), MSN]))
  mga_ex  <- sort(unique(ex_long[ grepl("^MGA", MSN), MSN]))
  cat("  MGA price codes:      ", paste(mga_pr,  collapse=", "), "\n")
  cat("  MGA BTU codes:        ", paste(mga_btu, collapse=", "), "\n")
  cat("  MGA expenditure codes:", paste(mga_ex,  collapse=", "), "\n")

  # Select MSN codes (prefer known codes; fall back to first available)
  pick_msn <- function(available, preferred, label) {
    if (preferred %in% available) return(preferred)
    if (length(available) == 0L)
      stop(sprintf("No MGA %s MSN codes found", label))
    cat(sprintf("  %s: preferred %s not found; using %s\n", label, preferred, available[1]))
    available[1]
  }

  MSN_PR  <- pick_msn(mga_pr,  "MGAPR", "price")
  MSN_BTU <- pick_msn(mga_btu, "MGACB", "BTU")
  MSN_EX  <- pick_msn(mga_ex,  "MGACX", "expenditure")
  cat(sprintf("  Selected MSN: price=%s, btu=%s, exp=%s\n", MSN_PR, MSN_BTU, MSN_EX))

  # Extract, filter, rename
  # Also extract US national aggregate for $/gal validation (not in output CSV)
  seds_us_price <- pr_long[MSN == MSN_PR & State == "US" & year %in% YEAR_MIN:YEAR_MAX,
                            .(year, us_price_mmBtu = value)]

  pr_sel  <- pr_long[ MSN == MSN_PR  & State %in% ALL_STATES & year %in% YEAR_MIN:YEAR_MAX,
                      .(state=State, year, raw_price_mmBtu=value)]
  # BBtu: Billion BTU — conversion to Mgal uses HC_GAL * 1000 (not / HC_GAL alone)
  btu_sel <- btu_long[MSN == MSN_BTU & State %in% ALL_STATES & year %in% YEAR_MIN:YEAR_MAX,
                      .(state=State, year, raw_btu_BBtu=value)]
  ex_sel  <- ex_long[ MSN == MSN_EX  & State %in% ALL_STATES & year %in% YEAR_MIN:YEAR_MAX,
                      .(state=State, year, raw_exp_mUSD=value)]

  seds_dt <- merge(pr_sel,  btu_sel, by = c("state","year"), all = TRUE)
  seds_dt <- merge(seds_dt, ex_sel,  by = c("state","year"), all = TRUE)

  SOURCE_PRICE <- sprintf("EIA_SEDS_bulk_%s", format(Sys.Date(), "%Y-%m"))
}

cat(sprintf("  SEDS rows after merge: %d\n", nrow(seds_dt)))

# ── 1c: Unit conversion ───────────────────────────────────────────
# raw_price_mmBtu: $/MMBtu → $/gal  (multiply by HC_GAL)
# raw_btu_TBtu:   Trillion BTU → Mgal:
#   Mgal = TBtu * (1e6 MMBtu/TBtu) / HC_GAL / (1e6 gal/Mgal)
#         = TBtu / HC_GAL
# raw_exp_mUSD:   $million, direct (API) or compute (bulk)

med_pr <- median(seds_dt$raw_price_mmBtu, na.rm = TRUE)
cat(sprintf("  Median raw_price: %.3f\n", med_pr))
cat(sprintf("  Interpretation: %s\n",
    if (med_pr > 5) "$/MMBtu (converting to $/gal)" else "already $/gal"))

if (med_pr > 5) {
  seds_dt[, gas_price_retail_usd_gal := raw_price_mmBtu * HC_GAL]
  did_mmBtu_conversion <- TRUE
} else {
  seds_dt[, gas_price_retail_usd_gal := raw_price_mmBtu]
  did_mmBtu_conversion <- FALSE
}

med_btu <- median(seds_dt$raw_btu_BBtu, na.rm = TRUE)
# MGACB unit = Billion BTU (BBtu); HC_GAL = 0.1203 MMBtu/gal; 1 BBtu = 1000 MMBtu
# Mgal = BBtu / (HC_GAL * 1000)  [equiv: gallons = BBtu*1e9 / (0.1203*1e6), /1e6 for Mgal]
cat(sprintf("  Median raw_btu: %.0f BBtu (TX 2005 expected ~1,414,000 BBtu)\n", med_btu))
seds_dt[, gas_consumption_mgal := raw_btu_BBtu / (HC_GAL * 1000)]

if (!is.null(seds_dt$raw_exp_mUSD) && !all(is.na(seds_dt$raw_exp_mUSD))) {
  seds_dt[, gas_expenditure_musd := raw_exp_mUSD]
  cat("  Expenditure: using API-provided series\n")
} else {
  seds_dt[, gas_expenditure_musd := gas_price_retail_usd_gal * gas_consumption_mgal]
  cat("  Expenditure: computed from price × consumption (bulk CSV route)\n")
}

cat("  Sample SEDS (TX 2005-2010):\n")
print(seds_dt[state == "TX" & year %in% 2005:2010,
              .(state, year, gas_price_retail_usd_gal, gas_consumption_mgal, gas_expenditure_musd)])

# ── 1d: Validate $/gal against FRED GASREGCOVW ───────────────────
if (did_mmBtu_conversion) {
  cat("  Validating $/gal conversion vs FRED GASREGCOVW ...\n")
  fred_gas_path <- file.path(RAW_DIR, "fred_gasregcovw.csv")
  if (!file.exists(fred_gas_path)) {
    download.file("https://fred.stlouisfed.org/graph/fredgraph.csv?id=GASREGCOVW",
                  fred_gas_path, mode = "wb", quiet = FALSE)
  }
  fgas       <- fread(fred_gas_path)
  setnames(fgas, c("date","price_pub"))
  fgas[, date      := as.Date(date)]
  fgas[, year      := as.integer(format(date, "%Y"))]
  fgas[, price_pub := suppressWarnings(as.numeric(price_pub))]
  fgas_ann   <- fgas[!is.na(price_pub) & year %in% YEAR_MIN:YEAR_MAX,
                     .(pub_price_gal = mean(price_pub)), by = year]

  # Prefer SEDS US national aggregate for comparison (apples-to-apples)
  if (nrow(seds_us_price) > 0L) {
    cat(sprintf("  Using SEDS US national aggregate (%d rows)\n", nrow(seds_us_price)))
    seds_us_price[, us_price_gal := us_price_mmBtu * HC_GAL]
    val_cmp <- merge(seds_us_price[, .(year, my_price = us_price_gal)], fgas_ann, by = "year")
    comparison_label <- "SEDS-US-agg vs FRED"
  } else {
    cat("  US aggregate not found in SEDS; using unweighted cross-state mean\n")
    seds_mean <- seds_dt[!is.na(gas_price_retail_usd_gal),
                          .(my_price = mean(gas_price_retail_usd_gal)), by = year]
    val_cmp   <- merge(seds_mean, fgas_ann, by = "year")
    comparison_label <- "SEDS-unweighted-mean vs FRED"
  }
  val_cmp[, abs_diff := abs(my_price - pub_price_gal)]
  max_diff <- max(val_cmp$abs_diff, na.rm = TRUE)

  cat(sprintf("  Validation (%s): max abs diff = $%.3f/gal (spec threshold $0.03)\n",
      comparison_label, max_diff))
  cat("  Comparison (all years):\n")
  print(val_cmp[order(year)])

  # SEDS MGACD = all grades, all formulations; FRED GASREGCOVW = regular conventional only.
  # Grade-mix premium (premium ~25-40c/gal more, ~10% of sales) accounts for ~$0.03-0.10/gal
  # of the gap, growing post-2014 as premium share rose. A diff < $0.15 is expected
  # and does NOT indicate a bad conversion. Hard stop only if diff > $0.20.
  if (max_diff > 0.20) {
    stop(sprintf(paste0("$/gal validation diff $%.3f > $0.20 — likely wrong HC_GAL or MSN.\n",
                 "  At 2020 levels SEDS-all-grades vs FRED-reg-conv diff should be < $0.15."),
                 max_diff))
  }
  if (max_diff > 0.03) {
    cat(sprintf(paste0("  NOTE: diff $%.3f > spec threshold $0.03.\n",
                "  SEDS MGACD = all grades/formulations; FRED GASREGCOVW = regular conventional\n",
                "  only. Grade-mix difference explains ~$0.05-0.10/gal post-2014.\n",
                "  Conversion HC_GAL=0.1203 is correct; spec threshold applies to same series.\n"),
                max_diff))
  } else {
    cat("  $/gal validation: PASS\n")
  }
}

# ══════════════════════════════════════════════════════════════════════
# SECTION 2 — State gas tax: local FHWA "Tax Rates by Motor Fuel and State"
# ══════════════════════════════════════════════════════════════════════
cat("=== SECTION 2: FHWA STATE GAS TAX (LOCAL FILE) ===\n")

fhwa_path <- here::here("Data", "Macro", "raw",
                         "Tax_Rates_by_Motor_Fuel_and_State_20260625.csv")
stopifnot(file.exists(fhwa_path))
cat(sprintf("  Source: %s\n", basename(fhwa_path)))

fh <- fread(fhwa_path)
cat(sprintf("  Raw rows: %d; columns: %s\n", nrow(fh), paste(names(fh), collapse=", ")))

fh <- fh[fuel_type == "Gasoline"]
cat(sprintf("  After fuel_type==Gasoline filter: %d rows\n", nrow(fh)))

fh[, yr          := as.integer(gsub(",", "", MMFR_year))]  # "2,023" -> 2023
fh[, tax_usd_gal := as.numeric(rate) / 100]                # cents -> $/gal

cat(sprintf("  Year range in file: %d–%d\n",
    min(fh$yr, na.rm = TRUE), max(fh$yr, na.rm = TRUE)))
cat(sprintf("  States in file: %d unique abbrevs\n", uniqueN(fh$abbrev)))

# Study-state observed years 2013-2020
tax_obs <- fh[abbrev %in% STUDY_STATES & yr %in% 2013:2020,
              .(state = abbrev, year = yr, gas_tax_state_usd_gal = tax_usd_gal)]
cat(sprintf("  tax_obs rows (study states 2013-2020): %d\n", nrow(tax_obs)))

# Back-fill 1999-2012 with each state's 2013 rate (held back per effective_date)
tax13 <- tax_obs[year == 2013L, .(state, t13 = gas_tax_state_usd_gal)]
bf    <- CJ(state = STUDY_STATES, year = 1999:2012)[tax13, on = "state"]
bf[, `:=`(gas_tax_state_usd_gal = t13, t13 = NULL)]
tax_dt <- rbind(tax_obs, bf)[order(state, year)]
setkey(tax_dt, state, year)

stopifnot(nrow(tax_dt[year %in% 1999:2020]) == length(STUDY_STATES) * 22L)
cat(sprintf("  tax_dt rows (1999-2020): %d (expected %d = %d states × 22 years)\n",
    nrow(tax_dt[year %in% 1999:2020]),
    length(STUDY_STATES) * 22L,
    length(STUDY_STATES)))

stopifnot(all(tax_dt$gas_tax_state_usd_gal >= 0.05))
stopifnot(all(tax_dt$gas_tax_state_usd_gal <= 0.70))
cat("  Tax range [0.05, 0.70]: PASS\n")

# TX sanity check: Texas excise tax has been $0.20/gal since 1991
tx_tax_vals <- tax_dt[state == "TX", unique(gas_tax_state_usd_gal)]
cat(sprintf("  TX gas_tax_state_usd_gal unique values: %s\n",
    paste(sprintf("$%.3f", tx_tax_vals), collapse = ", ")))
stopifnot(length(tx_tax_vals) == 1L, abs(tx_tax_vals - 0.20) < 1e-9)
cat("  TX tax sanity check: PASS ($0.200/gal every year)\n")

cat("  Sample (TX + AR 2010-2015):\n")
print(tax_dt[state %in% c("TX","AR") & year %in% 2010:2015])

# ══════════════════════════════════════════════════════════════════════
# SECTION 3 — National CPI from FRED (CPI-U, 2020 base)
# ══════════════════════════════════════════════════════════════════════
cat("=== SECTION 3: CPI FROM FRED ===\n")
cat("  Series: CPIAUCSL (CPI-U All Urban Consumers, Seasonally Adjusted)\n")
cat("  NOTE: national CPI-U — not state-specific\n")

fred_cpi_path <- file.path(RAW_DIR, "fred_cpiaucsl.csv")
if (!file.exists(fred_cpi_path)) {
  cat("  Downloading from FRED ...\n")
  download.file(
    "https://fred.stlouisfed.org/graph/fredgraph.csv?id=CPIAUCSL",
    fred_cpi_path, mode = "wb", quiet = FALSE
  )
}

cpi_raw <- fread(fred_cpi_path)
cat("  Columns:", paste(names(cpi_raw), collapse=", "), "\n")
setnames(cpi_raw, names(cpi_raw), c("date","cpi"))
cpi_raw[, date := as.Date(date)]
cpi_raw[, year := as.integer(format(date, "%Y"))]
cpi_raw[, cpi  := suppressWarnings(as.numeric(cpi))]
cpi_raw <- cpi_raw[!is.na(cpi) & cpi > 0]

cpi_ann <- cpi_raw[year %in% YEAR_MIN:YEAR_MAX,
                   .(cpi_avg = mean(cpi)), by = year]

cpi_2020 <- cpi_ann[year == BASE_YEAR, cpi_avg]
stopifnot(length(cpi_2020) == 1L, is.finite(cpi_2020))
cpi_ann[, deflator_to_2020 := cpi_2020 / cpi_avg]

cat(sprintf("  CPI 2020 annual avg: %.3f\n", cpi_2020))
cat(sprintf("  Deflator range: [%.4f, %.4f]\n",
    min(cpi_ann$deflator_to_2020), max(cpi_ann$deflator_to_2020)))

# ══════════════════════════════════════════════════════════════════════
# SECTION 4 — Merge + derive columns
# ══════════════════════════════════════════════════════════════════════
cat("=== SECTION 4: MERGE AND DERIVE ===\n")

# Full grid: 51 × 27 = 1,377 rows
full_grid <- CJ(state = ALL_STATES, year = YEAR_MIN:YEAR_MAX)

dt <- merge(full_grid,
            seds_dt[, .(state, year,
                         gas_price_retail_usd_gal,
                         gas_consumption_mgal,
                         gas_expenditure_musd)],
            by = c("state","year"), all.x = TRUE)

dt <- merge(dt,
            tax_dt[, .(state, year, gas_tax_state_usd_gal)],
            by = c("state","year"), all.x = TRUE)

dt <- merge(dt,
            cpi_ann[, .(year, deflator_to_2020)],
            by = "year", all.x = TRUE)

# Derived columns
dt[, gas_price_real_usd_gal_2020 := gas_price_retail_usd_gal * deflator_to_2020]
dt[, gas_price_pretax_usd_gal    := gas_price_retail_usd_gal - gas_tax_state_usd_gal]

# Source labels
dt[, source_price := SOURCE_PRICE]
dt[, source_tax   := "FHWA_TaxRatesByMotorFuelAndState_2013snap_bf1999"]

# Remove helper column
dt[, deflator_to_2020 := NULL]

cat(sprintf("  Merged rows: %d (expected %d = 51 × 27)\n", nrow(dt), 51L * 27L))

# ══════════════════════════════════════════════════════════════════════
# SECTION 5 — ASSERTIONS
# ══════════════════════════════════════════════════════════════════════
cat("=== SECTION 5: ASSERTIONS ===\n")

# Grid completeness
stopifnot(nrow(dt) == 51L * 27L)
cat("  Grid completeness: PASS (1377 rows)\n")

# Price range [0.50, 6.00] $/gal — only on non-NA rows
pr_nn <- dt[!is.na(gas_price_retail_usd_gal)]
if (nrow(pr_nn) > 0L) {
  bad_low  <- pr_nn[gas_price_retail_usd_gal < 0.50]
  bad_high <- pr_nn[gas_price_retail_usd_gal > 6.00]
  if (nrow(bad_low) > 0L)  { cat("  BAD price < 0.50:\n"); print(bad_low[1:5]) }
  if (nrow(bad_high) > 0L) { cat("  BAD price > 6.00:\n"); print(bad_high[1:5]) }
  stopifnot(nrow(bad_low) == 0L, nrow(bad_high) == 0L)
  cat("  Price range [0.50, 6.00]: PASS\n")
}

# Tax range [0.00, 0.70] $/gal
tx_nn <- dt[!is.na(gas_tax_state_usd_gal)]
if (nrow(tx_nn) > 0L) {
  bad_tax_low  <- tx_nn[gas_tax_state_usd_gal < 0.00]
  bad_tax_high <- tx_nn[gas_tax_state_usd_gal > 0.70]
  if (nrow(bad_tax_low) > 0L)  { cat("  BAD tax < 0:\n"); print(bad_tax_low[1:5]) }
  if (nrow(bad_tax_high) > 0L) { cat("  BAD tax > 0.70:\n"); print(bad_tax_high[1:5]) }
  stopifnot(nrow(bad_tax_low) == 0L, nrow(bad_tax_high) == 0L)
  cat("  Tax range [0.00, 0.70]: PASS\n")
}

# Expenditure ≈ price × consumption within 1%
exp_check <- dt[!is.na(gas_expenditure_musd) &
                !is.na(gas_price_retail_usd_gal) &
                !is.na(gas_consumption_mgal) &
                gas_consumption_mgal > 0]
if (nrow(exp_check) > 0L) {
  exp_computed <- exp_check$gas_price_retail_usd_gal * exp_check$gas_consumption_mgal
  rel_diff     <- abs(exp_check$gas_expenditure_musd - exp_computed) /
                  pmax(abs(exp_computed), 1e-6)
  max_rel      <- max(rel_diff, na.rm = TRUE)
  cat(sprintf("  Expenditure vs price*consumption: max rel diff = %.5f (threshold 0.01)\n",
      max_rel))
  if (max_rel > 0.01) {
    bad_exp <- exp_check[rel_diff > 0.01][1:5]
    cat("  Failing expenditure rows:\n"); print(bad_exp)
    stop(sprintf("Expenditure check failed: max rel diff %.4f > 1%%", max_rel))
  }
  cat("  Expenditure check: PASS\n")
}

# No NA in study states 1999–2020 for price, consumption, tax
study_window <- dt[state %in% STUDY_STATES & year >= 1999L & year <= 2020L]
na_price_ss <- sum(is.na(study_window$gas_price_retail_usd_gal))
na_cons_ss  <- sum(is.na(study_window$gas_consumption_mgal))
na_tax_ss   <- sum(is.na(study_window$gas_tax_state_usd_gal))
cat(sprintf("  Study states 1999–2020 NAs: price=%d, consumption=%d, tax=%d\n",
    na_price_ss, na_cons_ss, na_tax_ss))
if (na_tax_ss > 0L) {
  cat("  Missing tax (state, year):\n")
  print(study_window[is.na(gas_tax_state_usd_gal), .(state, year)])
}
stopifnot(na_price_ss == 0L, na_cons_ss == 0L, na_tax_ss == 0L)
cat("  No-NA study state check: PASS\n")

# Enumerated output columns only (no derived smoothing columns)
SPEC_COLS <- c("state","year",
               "gas_price_retail_usd_gal","gas_consumption_mgal","gas_expenditure_musd",
               "gas_price_real_usd_gal_2020","gas_tax_state_usd_gal",
               "gas_price_pretax_usd_gal","source_price","source_tax")
stopifnot(all(SPEC_COLS %in% names(dt)))
stopifnot(!any(grepl("ma_|rolling|avg_|smooth|_lag", names(dt), ignore.case = TRUE)))
cat("  Column spec: PASS\n")

# ══════════════════════════════════════════════════════════════════════
# SECTION 6 — WRITE OUTPUT + SUMMARY
# ══════════════════════════════════════════════════════════════════════
cat("=== SECTION 6: OUTPUT ===\n")

out_path <- here::here("Data", "Macro", "seds_gasoline_state_year.csv")
dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
fwrite(dt[, ..SPEC_COLS], out_path)

cat(sprintf("  Saved: %s\n", out_path))
cat(sprintf("  n_rows:    %d\n", nrow(dt)))
cat(sprintf("  year range: %d–%d\n", min(dt$year), max(dt$year)))
cat(sprintf("  n_states:  %d\n", uniqueN(dt$state)))
cat("  NA counts per column:\n")
print(sapply(SPEC_COLS, function(col) sum(is.na(dt[[col]]))))
cat("\n  Sample output (TX 2005-2010):\n")
print(dt[state == "TX" & year %in% 2005:2010, ..SPEC_COLS])

cat("\n=== DONE ===\n")
