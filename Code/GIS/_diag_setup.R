#### _diag_setup.R ####
# Throwaway diagnostic: (1) which GIS/Dewey packages are installed locally,
# (2) facility-level geocoding coverage by state from the Master file,
# (3) extract the BLP micro-moments PDF to text. Safe to delete after.

suppressMessages(library(data.table))

#### 0) Package availability (local toolchain) ####
want <- c("duckdb","arrow","sf","tigris","FNN","geodist","stringdist","pdftools","here")
have <- want[want %in% rownames(installed.packages())]
cat("=== INSTALLED (local) ===\n"); print(have)
cat("=== MISSING (local) ===\n"); print(setdiff(want, have))
cat("libPaths:\n"); print(.libPaths())

#### 1) Facility-level geocoding coverage by state ####
path <- "Data/Processed/Master_Harmonized_UST_Tanks.csv"
dt <- fread(path, select = c("facility_id","state","latitude","longitude"))
dt[, lat := as.numeric(latitude)]; dt[, lon := as.numeric(longitude)]
fac <- dt[, .(geo = any(is.finite(lat) & is.finite(lon) &
                        lat %between% c(24,50) & lon %between% c(-125,-66))),
          by = .(facility_id, state)]
cov <- fac[, .(n_fac = .N, n_geo = sum(geo)), by = state][order(state)]
cov[, pct := round(100 * n_geo / n_fac, 1)]
cat("=== FACILITY-LEVEL GEOCODING COVERAGE BY STATE ===\n")
print(cov)
cat(sprintf("TOTAL: %s facilities | %s geocoded (%.1f%%)\n",
            format(sum(cov$n_fac), big.mark=","),
            format(sum(cov$n_geo), big.mark=","),
            100 * sum(cov$n_geo) / sum(cov$n_fac)))

#### 2) Extract the micro-moments PDF to text ####
pdf <- "Docs/blp -- how to bring in moments from subset into ccp discrete chioce models.pdf"
if ("pdftools" %in% have && file.exists(pdf)) {
  tx <- pdftools::pdf_text(pdf)
  writeLines(tx, "Docs/blp_micro_moments_extracted.txt")
  cat(sprintf("=== PDF extracted: %d pages -> Docs/blp_micro_moments_extracted.txt ===\n", length(tx)))
} else {
  cat("pdftools not installed locally; will extract PDF another way.\n")
}
