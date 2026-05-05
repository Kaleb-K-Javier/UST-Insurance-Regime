# ==============================================================================
# data_paths.R  —  Centralized data path resolution
# ==============================================================================
#
# WHAT THIS IS FOR:
#
# The project has two roots on this machine:
#
#   1. The Z drive (Z:/ust_ins_move_to_github/) is the canonical, read-only
#      mirror of the project on the server. It holds the up-to-date raw and
#      merged panels (claims, facility, tank-level, rate filings, etc.) that
#      upstream pipelines produce. We CAN read from Z, we CANNOT write to it.
#
#   2. The local repo (this working directory) is where we run analysis
#      scripts and write outputs. The local Data/ folder may have stale or
#      partial copies of what's on Z.
#
# RULE OF THUMB:
#   - Inputs (raw + merged panels, rate filings) -> use z_path()
#   - Outputs (figures, tables, derived RDS/CSV) -> use here()
#   - Derived files written earlier in this session and now read back ->
#     use here() (those live only locally)
#
# WHY centralized: hard-wiring "Z:/..." paths inside individual scripts
# scatters server assumptions across the codebase. Sourcing this one file
# at the top of any script that touches server inputs keeps the logic in
# one place — change DATA_ROOT_Z here if the mount ever moves.
#
# USAGE:
#   source(here::here("Code", "Helpers", "data_paths.R"))
#
#   reg_dt   <- fread(z_path("Data", "Processed", "all_cleaned_claims.csv"))
#   raw_fr   <- fread(z_path("Data", "Raw",       "state_fr_premium.csv"))
#   fwrite(my_output, here("Output", "Tables", "my_table.csv"))
#
#   # When a file might live on Z OR locally (e.g. you regenerated it
#   # locally and want to prefer the local copy if present):
#   panel <- fread(data_in("Data", "Analysis", "facility_panel.csv"))
#
# ==============================================================================

DATA_ROOT_Z <- "Z:/ust_ins_move_to_github"

#' Resolve a path on the Z drive (server, read-only) — falls back to local.
#'
#' On the laptop with Z: mounted, returns a Z-rooted path so we read fresh
#' canonical inputs from the server. On the server itself (or anywhere Z: is
#' not mounted), falls back to here::here(...) — the server already IS the
#' data source, so the project-relative path is correct there.
#'
#' This makes the same script portable across the laptop and the server with
#' no per-environment branching at the call site.
#'
#' @param ... path components passed to file.path (matches here() signature)
z_path <- function(...) {
  if (!dir.exists(DATA_ROOT_Z)) return(here::here(...))
  file.path(DATA_ROOT_Z, ...)
}

#' Resolve a path preferring local, falling back to Z.
#'
#' Returns the local path if the file exists there, otherwise the Z path.
#' Use this when a file MAY have been regenerated locally and you want the
#' fresh local copy when present, with Z as fallback.
#' @param ... path components
data_in <- function(...) {
  local_p <- here::here(...)
  if (file.exists(local_p)) return(local_p)
  z_path(...)
}
