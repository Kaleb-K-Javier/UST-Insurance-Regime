# HANDOFF — GIS + Dewey workstream
# Updated: 2026-06-24
# (design reference: Code/GIS/SCOPE_GIS_Dewey_Linkage.md)
#
# INFRA NOTE (2026-06, admin Gary, ucbare2; VERIFIED 2026-06-24): C: was nearly full
#   (shared ~50 users). The whole C:\Users\kalebkja profile is junctioned folder-by-folder
#   to D:\shares\Users\kalebkja\. VERIFIED junction targets:
#     C:\Users\kalebkja\dewey-downloads        -> D:\shares\Users\kalebkja\dewey-downloads
#     C:\Users\kalebkja\ust_ins_move_to_github -> D:\shares\Users\kalebkja\ust_ins_move_to_github
#       (this REPO physically lives on D:; `git rev-parse --show-toplevel` => D:/shares/...)
#     C:\Users\kalebkja\Car Subs Data          -> D:\shares\Users\kalebkja\Car Subs Data
#   D:\shares\Users\kalebkja\C_Drive_Portal\dewey-downloads is ANOTHER alias (junction) to
#   the SAME D:\shares\Users\kalebkja\dewey-downloads (old Z: mapping artifact, not a 2nd copy).
#   The C: paths work transparently (peak free: C: 235 GB, D: 2,583 GB). POLICY FLIPPED:
#   keep large data on D: (the old "C: only / D: not allowed" rule is dead). Scripts keep the
#   C: junction path (verified -> D:); the D: physical root above is the canonical truth.
#   Admin: Gary until 2026-07-01, then Eric.

═══════════════════════════════════════════════════
RESUME HERE (next session)
═══════════════════════════════════════════════════
Mode = ARCHITECT → CODER → REVIEWER (run_coder.ps1). Architect writes specs only.
NEXT ACTION: write **T019a** spec. ONE open decision to answer first:
  in-place (extend 10_Master EPA-merge block, reuse merge_id logic) vs standalone
  script for the address lookup. (Leaning in-place — no merge-ID drift.)
Then say "write the spec".

═══════════════════════════════════════════════════
WHAT'S BUILT (server: ucbare2, Windows, runs on C:)
═══════════════════════════════════════════════════
Code/GIS/dewey/  (python, runs via uv/uvx — no installs):
  dewey_pull_reduce.py  stream-and-reduce: download a chunk -> duckdb grid-filter
     to POIs NEAR a UST facility (GEOGRAPHY, not NAICS) -> write small -> delete raw.
     Peak disk = one chunk. Grid = 0.0025deg cells (+8 nbrs ~0.5km) from master lat/long.
  run_dewey_pull.ps1 / .sh, README_DEWEY_PULL.md, probe_system.ps1, requirements.txt
  Env (server): DEWEY_Z_ROOT=C:\Users\kalebkja\dewey-downloads (C: junction -> physically D:) ; DEWEY_RUN=uvx ;
     UST_MASTER_CSV=C:\Users\kalebkja\ust_ins_move_to_github\Data\Processed\Master_Harmonized_UST_Tanks.csv
     DEWEY_API_KEY = per-session (NOT persisted; set each shell, or set User env once).
  Confirmed: uv 0.10.12, uv run python 3.13, duckdb 1.5.3, dewey reachable.
  STORAGE: large data lives on D: now (junction keeps C: paths working — see INFRA NOTE up top).

Dataset registry (DATASETS in dewey_pull_reduce.py):
  safegraph_places       READY  project_id prj_ycsuakbe__fldr_b7faazxwmt47zdme8
  weekly_patterns_plus   READY  project_id prj_ycsuakbe__fldr_94nvmmhcmp9rrr6mc  (monthly windows)
  safegraph_spend        READY  project_id prj_ycsuakbe__fldr_fhx7b4q9kudjwmk4r  (monthly windows)
  attom_tax_assessor     PARKED/BLOCKED — reworked to batched project_ids (5%/day cap,
     MUST customize: non-residential land use + state batch <7.75M rows, ONE/day,
     STUDY STATES FIRST). Geocoded -> grid match. User can't download now. project_ids={} empty.
  (ATTOM Assessor History on D:\Dewey\ATTOM\Assessor History = NOT USED — has NULL lat/long.)
Run when ready:  uv run --with duckdb python Code\GIS\dewey\dewey_pull_reduce.py run <name>

═══════════════════════════════════════════════════
DATA FACTS
═══════════════════════════════════════════════════
Master = Data/Processed/Master_Harmonized_UST_Tanks.csv (tank-level; key facility_id+state).
  Has: facility_name, county_name/fips, latitude, longitude (97.5% geocoded; TX 96.9%),
       epa_key, fuel/wall flags, study_group. NO street address.
Address source = Data/Raw/Facilities.csv (EPA UST Finder; server only, gitignored).
  Cols incl: Facility_ID, Name, Address, City, County, State, Zip_Code, Latitude, Longitude,
  Coordinate_Source, Address_Match_Type, + EPA-derived (Private_Wells_1500ft, Within_SPA,
  SPA_PWS_FacilityID, Within_WHPA, WHPA_PWS_FacilityID, Population_1500ft, LandUse, ...).
  10_Master_Cleaning_and_Harmonization.r loads it but keeps ONLY Latitude/Longitude
  (EPA merge via per-state "winning-ID" logic, lines ~96-152).
  Address completeness in-file: 96.7% overall; ~100% most study states EXCEPT
  TX 84.0, AZ 48.8, CT 29.4, MT 85.0  -> those lean spatial-only / native fallback.

═══════════════════════════════════════════════════
DECISIONS LOCKED
═══════════════════════════════════════════════════
- Crosswalk KEEPS ALL candidate matches (auditable) + non-exclusive is_best flag.
  Match = spatial k-NN + name Jaro-Winkler + ADDRESS score. Hand-check as we go.
- Competition = id EDGE-LIST (facility_i, facility_j, dist_m) at {0.25, 0.5, 1 mi},
  ids only (join operator/brand/traffic downstream). Needs only lat/long.
- WELLS: build OURSELVES from raw well data (Marcus 600 m + a 1500 ft EPA-style version).
  Do NOT import EPA-derived well/SPA/WHPA columns — note EPA method as reference only.
- Operator 4-bin slide: SafeGraph BRAND + name/NAICS + facility fields. Data Axle DROPPED
  (year-bin sales). No CERS owner_type in EPA panel -> brand/name heuristics + hand-validate.
- Reduce filter = GEOGRAPHY (near-UST grid), never NAICS (USTs incl. fleet/muni/farm).
- _present_cols() in the script auto-drops keep_cols not in a file's schema (robust).

═══════════════════════════════════════════════════
TICKET BACKLOG (architect -> coder, R)
═══════════════════════════════════════════════════
  T019a  EPA address lookup -> Data/Processed/Facility_Address_Lookup.csv (facility_id,state);
         Step 1 = per-state address-coverage audit; + competition edge-list. NO Dewey. FIRST.
  T019b  Dewey crosswalk: facility <-> POI (placekey / id_store), ALL candidates kept,
         scored dist_m + name JW + address; is_best flag. Needs Places/WPP reduced dirs.
  T020   Foot-traffic + spend facility panels, joined on matched ids (Spend/WPP).
  T021   Operator 4-bin classification (the slide).
  T022   Urban/rural (RUCA + Census 2020 Urban Area) -> Code/GIS/03_urban_rural.R. NO Dewey.
  T023   Drinking-water wells + pollution, Marcus 600 m, built ourselves -> 04_wells.R. NO Dewey.
  (existing GIS scaffolding: Code/GIS/00_gis_config.R [EPSG:5070, STUDY_STATES, helpers],
   01_census_geography.R [tract/BG GEOID], 01_SafeGraph_Crosswalk.R [k-NN+JW matcher to reuse].)

═══════════════════════════════════════════════════
NOT YET COMMITTED
═══════════════════════════════════════════════════
Local repo (C:\Users\kaleb\...) holds the dewey tooling + SCOPE + this handoff (untracked/edited).
Server holds the user's edited copy (pull_reduce_local etc.). Reconcile via git before coder runs.
