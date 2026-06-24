# PM00_Preflight.R — Rcpp/Rtools environment gate for the portfolio model
# TICKET 023 B0. Nothing downstream runs until this prints PREFLIGHT PASS.
suppressPackageStartupMessages({ library(here); library(Rcpp) })

.log_path <- here::here("logs", paste0("PM00_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".log"))
dir.create(dirname(.log_path), recursive = TRUE, showWarnings = FALSE)
.log <- file(.log_path, open = "wt")
sink(.log, type = "output"); sink(.log, type = "message", append = TRUE)
on.exit({ sink(type = "output"); sink(type = "message"); close(.log) }, add = TRUE)
cat(sprintf("LOG START %s\nScript: PM00_Preflight\nR: %s\nWD: %s\n\n",
            .log_path, R.version.string, getwd()))

# ==============================================================================
# 1. Print compiler environment
# ==============================================================================
cat("=== SECTION 1: compiler environment ===\n")
cat(sprintf("R version: %s\n", R.version.string))
cat(sprintf("make:      %s\n", Sys.which("make")))
cat(sprintf("g++:       %s\n", Sys.which("g++")))
path_entries <- strsplit(Sys.getenv("PATH"), .Platform$path.sep)[[1]]
rtools_entries <- path_entries[grepl("rtools|Rtools", path_entries, ignore.case = TRUE)]
cat(sprintf("Rtools PATH entries (%d):\n", length(rtools_entries)))
for (e in rtools_entries) cat(sprintf("  %s\n", e))

# ==============================================================================
# 2. Hard stop if compiler not found
# ==============================================================================
cat("=== SECTION 2: compiler check ===\n")
stopifnot(nzchar(Sys.which("make")), nzchar(Sys.which("g++")))
cat("COMPILER CHECK PASS\n")

# ==============================================================================
# 3. Check for bare top-of-file sourceCpp in improved_estimator_OPTIMIZED.r
#    (the historical line-32 bug — memory:feedback_rcpp_eager_sourcecpp_at_line32)
# ==============================================================================
cat("=== SECTION 3: improved_estimator bare sourceCpp check ===\n")
est_lines <- readLines(here("Code", "Helpers", "improved_estimator_OPTIMIZED.r"), n = 60L)
hit_idx <- grep("^\\s*Rcpp::sourceCpp", est_lines)
if (length(hit_idx) > 0L) {
  cat(sprintf("  WARNING: bare Rcpp::sourceCpp at line(s) %s — FOUND (do not edit here; report only)\n",
              paste(hit_idx, collapse = ", ")))
} else {
  cat("  bare sourceCpp check: CLEAR\n")
}

# ==============================================================================
# 4. Compile cpp_engine.cpp
# ==============================================================================
cat("=== SECTION 4: compile cpp_engine.cpp ===\n")
t0 <- Sys.time()
Rcpp::sourceCpp(here("Code", "Helpers", "cpp_engine.cpp"))
compile_sec <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
cat(sprintf("  cpp_engine compile time: %.1f seconds\n", compile_sec))
stopifnot(is.finite(compile_sec))
cat("  COMPILE OK\n")

# ==============================================================================
# 5. Smoke test: compute_inclusive_value_cpp on 2x2 dummy inputs
# ==============================================================================
cat("=== SECTION 5: smoke test compute_inclusive_value_cpp ===\n")
smoke_vm  <- c(0.5, 1.0)
smoke_vr  <- c(0.3, 0.8)
smoke_feas <- c(TRUE, TRUE)
smoke_result <- compute_inclusive_value_cpp(
  v_m       = smoke_vm,
  v_r       = smoke_vr,
  sigma2    = 1.0,
  feasible_r = smoke_feas,
  gamma_E   = 0.5772156649
)
cat(sprintf("  inputs:  v_m=(%s)  v_r=(%s)  feasible=(T,T)  sigma2=1  gamma_E=0.5772\n",
            paste(smoke_vm, collapse=","), paste(smoke_vr, collapse=",")))
cat(sprintf("  result:  (%s)\n", paste(round(smoke_result, 6), collapse=", ")))
stopifnot(length(smoke_result) == 2L, all(is.finite(smoke_result)))
cat("  SMOKE TEST PASS\n")

# ==============================================================================
# 6. Done
# ==============================================================================
cat("\nPREFLIGHT PASS\n")
