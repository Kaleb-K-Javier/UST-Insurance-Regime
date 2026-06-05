# SCOPE — GIS + Dewey Facility Enrichment (UST Insurance project)
# Created: 2026-06-04  | Author: architect (Opus) | Status: SCOPING — approve before building
# Reference learned from: ca_gas_competition/Code/Checks/ca_gis_neighbors.R (1-mile neighbor method)

═══════════════════════════════════════════════════════════════════════
0. PURPOSE
═══════════════════════════════════════════════════════════════════════
Build five facility-level GIS / economic-context layers on the national EPA
UST panel, to (a) fill descriptive slides (e.g. the "Who operates USTs?" operator
table in Reports/Slides/04_Risk_Based_Pricing_and_USTs_TALK_EDIT.qmd) and
(b) feed Q1 (observability of risk from observables) with richer covariates —
operator type, urban/rural, local competition, drinking-water-well exposure
(the harm/externality channel), and POI economics.

THIS IS AN EXTENSION of the existing Code/GIS/ module framework, not a rebuild.
The CA project was studied ONLY to borrow its neighbor-counting recipe.

═══════════════════════════════════════════════════════════════════════
1. WHAT ALREADY EXISTS HERE (do not duplicate)
═══════════════════════════════════════════════════════════════════════
Code/GIS/00_gis_config.R       config: CRS_ALBERS=EPSG:5070 (distance), 4326 (storage),
                               STUDY_STATES (26: TX target + tiers), keys=(facility_id,state),
                               helpers load_master_coords(), coords_to_sf(),
                               write_gis_lookup(), log_gis(); outputs → Data/Processed/GIS/.
Code/GIS/01_census_geography.R tract + block-group GEOID (2010 & 2020) via TIGER point-in-poly.
                               → gis_01_census_geography.csv  (the merge key for RUCA/ACS/EJ).
Code/GIS/01_SafeGraph_Crosswalk.R  facility → SafeGraph Global Places, FNN k=5 k-NN,
                               Haversine dist, Jaro-Winkler name score, match_confidence.
                               → ust_safegraph_xwalk.csv (+ _geom.csv WKT for ≤100m).
                               THIS IS THE TASK-4/5 ANCHOR — already written.

BACKBONE  Data/Processed/Master_Harmonized_UST_Tanks.csv  (545 MB, tank-level)
  key cols: facility_id, facility_name, tank_id, state, tank_installed_date,
  tank_closed_date, tank_status, leak_* , capacity, single_walled/double_walled,
  is_gasoline/is_diesel/is_oil_kerosene/is_jet_fuel/is_other, county_name,
  county_fips, latitude, longitude, epa_key, study_group.
  → load_master_coords() already collapses this to facility points w/ CONUS coord gate.

STATUS: Data/Processed/GIS/ and Data/Raw/GIS/ are EMPTY — the modules are scaffolding
that has not been run/produced outputs. Part of this scope is to operationalize them.

═══════════════════════════════════════════════════════════════════════
2. KNOWN GAPS / FIXES IN THE EXISTING CODE (fold into the build)
═══════════════════════════════════════════════════════════════════════
F1. 01_SafeGraph_Crosswalk.R hard-codes SG_PARQUET_DIR to a stale LOCAL path
    "C:/Users/kalebkja/.../safegraph_places". Repoint to the Z: Dewey download
    (see §3) via 00_gis_config.R; SafeGraph Global Places is NOT yet on Z (only
    Infutor is) — it must be downloaded/customized first (§3, §6).
F2. 01_census_geography.R uses tryCatch(...) around tigris pulls — violates the
    project "hard propagation only" rule. Keep a single retry at most; let real
    errors surface. Same audit for any new module.
F3. lat/long coverage is uneven: raw coords exist for AR,LA,ME,NJ,NM,OK,AL,TN,CO;
    MI/TX historically lacked them. The Master file now carries latitude/longitude
    (EPA UST Finder merge), but TX is the TARGET state — a per-state coverage audit
    is a hard prerequisite for any distance task (neighbors, wells). See §9 Q1.
F4. EPA panel has NO CERS-style owner_type/facility_type field. Operator
    classification (Task 4) therefore leans on SafeGraph BRAND + facility_name
    heuristics + fuel flags, NOT a clean ownership column (§7.1).

═══════════════════════════════════════════════════════════════════════
3. DATA ARCHITECTURE  (Dewey is too big for local; query on the server)
═══════════════════════════════════════════════════════════════════════
Sizes (verified via Dewey MCP): Advan Weekly Patterns Plus 1,019 GB / 2.5 B rows;
SafeGraph Spend 93.7 M; SafeGraph Global Places 72.1 M; Cotality Property 87.2 M;
Infutor Consumer Profiles (already on Z) 65 GB / 365 shards.

THREE LAYERS — "download big → reduce on server → analyze small":
  A. ACQUIRE  (researcher, Dewey → Z:\C_Drive_Portal\dewey-downloads\<Partner>\<Dataset>\*.parquet)
     Use Dewey server-side "customize" to pre-filter to gas-station NAICS
     (447110/447190 + 2022: 457110/457120), 26 study states, needed cols/date-range
     BEFORE download. This is what turns 1 TB of Advan into tens of GB.
     (Only Infutor is staged today; SafeGraph Places/Spend, Advan WPP, Cotality must be pulled.)
  B. REDUCE  (R on the SERVER over Z: parquet — the existing crosswalk already shows
     the duckdb pattern). Formalize it into Code/Helpers/dewey_query.R: open a
     parquet glob lazily, push down WHERE/SELECT, COPY … TO a small parquet in
     Data/Processed/GIS/ (or Data/Dewey_Reduced/). Never materialize whole files.
  C. ANALYZE (R local) on the reduced extracts: spatial joins to facility points,
     build the facility-level lookups in Data/Processed/GIS/.

Reuse the duckdb recipe already in 01_SafeGraph_Crosswalk.R S3/S10 (read_parquet +
WHERE region IN (...) + memory_limit/threads). Generalize, don't re-invent.

═══════════════════════════════════════════════════════════════════════
4. THE NEIGHBOR RECIPE BORROWED FROM CA (Task 1)
═══════════════════════════════════════════════════════════════════════
CA's ca_gis_neighbors.R: project points to an equal-area CRS (meters), then
nb <- st_is_within_distance(pts, pts, 1609.34); neighbors = lengths(nb) - 1 (self).
Apply the SAME primitive here but: (i) CRS_ALBERS=5070 (national, not CA 3310),
(ii) run state-by-state to bound the O(n²) self-join at national scale (~140k pts),
(iii) emit a panel of radii, (iv) keep the within-state edge list for spillover work.

═══════════════════════════════════════════════════════════════════════
5. DEWEY DATASET INVENTORY (slugs / size / join key — verified)
═══════════════════════════════════════════════════════════════════════
 slug                         partner    rows/size           join → facility            task
 global-places-poi-geometry   SafeGraph  72.1 M              spatial+name (xwalk EXISTS) 4,5 anchor
 weekly-patterns-plus         Advan      2.54 B / 1,019 GB   PLACEKEY/ID_STORE via xwalk 5 foot traffic
 spend                        SafeGraph  93.7 M (mo 2019-26) PLACEKEY via xwalk          5 revenue
 property-data                Cotality   87.2 M              ADDRID (address graph, ⚠)   5 property value
 (Infutor consumer-profiles)  Infutor    65 GB ON Z          ADDRID                      5 demographics/bridge
NAICS gas: 447110,447190 (2017) / 457110,457120 (2022). Global Places carries BOTH
NAICS vintages + BRANDS + lat/long + POLYGON_WKT (footprint). Spend has RAW_TOTAL_SPEND,
RAW_NUM_TRANSACTIONS, MEDIAN_SPEND_PER_*; WPP has VISIT_COUNTS/VISITOR_COUNTS/MEDIAN_DWELL.
Cotality keys by ADDRID (NO lat/long) → see §7.2 risk; public parcel layer is the fallback.

5.1b REVENUE / PRICE / Q / CLASSIFICATION (verified 2026-06; all geocoded => same crosswalk)
 GOAL                   dataset (slug)                              key fields                          join
 ---------------------- ------------------------------------------- ----------------------------------- ------------------
 c-store + FUEL revenue PDI stores-information +                    STORE_ID, STORE_CHAIN_NAME,         spatial(lat/long)
                        transaction-items-daily-aggregation         CHAIN_SIZE, lat/long; TOTAL_REVENUE  -> STORE_ID
                        (Skupos ~80% of INDEPENDENT c-stores)       _AMOUNT, QUANTITY, TRANSACTION_COUNT
                        14.6B rows -> pull by matched STORE_ID      (store×day×NACS category, incl. FUEL)
 fuel PRICE (derived)   PDI fuel lines                              price = TOTAL_REVENUE_AMOUNT/QUANTITY STORE_ID
                        (NO posted pump-price feed in Dewey; EIA/state = free, OPIS/GasBuddy = purchase)
 Q (card swipes/store)  SafeGraph spend                             RAW_NUM_TRANSACTIONS(=swipes),       PLACEKEY
                                                                    RAW_TOTAL_SPEND, RAW_NUM_CUSTOMERS (monthly)
 Q (brand/industry)     ConsumerEdge daily-spend-breakout-by-brand, spend + txn counts by brand /        brand / NAICS×geo
                        industry-and-geography-cohort-breakout,     industry / ticker (same-store-sales)
                        daily-spend-breakout-by-symbol-ticker       100M+ cards, 13K+ merchants
 OPERATOR CLASS (slide) SafeGraph global-places-poi-geometry        BRANDS+NAICS+name (major/indep)      spatial->placekey
   (4-bin)              + PDI stores-information                    STORE_CHAIN_NAME/CHAIN_SIZE          spatial->STORE_ID
                        + master facility_name/fuel flags           gov/fleet name, diesel/oil-only      (have)
                        (Data Axle historical-business-data DROPPED: sales are year-bin, low value;
                         SafeGraph BRAND + PDI chain + facility name/fuel flags carry the slide)
 UNIFIED CROSSWALK: facility_id <-> {placekey(->Spend), advan_id_store(->WPP), attomid/apn(->ATTOM),
   pdi_store_id(->PDI)} — built once from the geocoded directories; heavy time-series pulled by matched id.

═══════════════════════════════════════════════════════════════════════
6. MICHELLE MARCUS REPLICATION (Task 3 — confirmed)
═══════════════════════════════════════════════════════════════════════
Marcus (2021), AEJ:Applied 13(1):72-104, verified quote: "I link leaking underground
storage tanks within **600 meters** to PWS wells." → headline radius 600 m, wells =
Public Water System (PWS) wells. Our facility-centric version: for each facility count
PWS wells within {300, 600, 1000, 1609 m} + nearest-well distance + any-within-600m.
National PWS source = EPA SDWIS / state DDW well point layers (NOT the CA-only OSWCR).
⚠ National PWS coordinate quality is the main feasibility risk for this task (§9 Q3).

WATER-WELL POLLUTION DATA — linkable, all FREE public (NOT Dewey):
- EPA SDWIS (Safe Drinking Water Information System): every public water system +
  its regulated-contaminant violations/monitoring (incl. VOCs e.g. benzene). This
  is Marcus (2021)'s drinking-water source. Link by PWS id / lat-long.
- Water Quality Portal (waterqualitydata.us = USGS NWIS + EPA STORET + states):
  premier US source of DISCRETE measured concentrations at monitoring locations
  incl. wells — petroleum analytes (benzene, toluene, ethylbenzene, xylene, MTBE,
  TPH) with lat/long + sample date. The actual measured-pollution layer.
- USGS National Ground-Water Monitoring Network (NGWMN): well water-quality + levels.
- EPA UCMR (Unregulated Contaminant Monitoring Rule): unregulated analytes at PWS
  (Marcus 2024 "Discovery of Unregulated Contaminants in Drinking Water" uses this).
CHAIN: UST/LUST (EPA LUST Finder, already have) --600 m--> PWS/drinking well (SDWIS)
--> measured contamination (WQP/NWIS, UCMR). All geocoded => spatial joins, no purchase.

═══════════════════════════════════════════════════════════════════════
7. TASK-4 / TASK-5 DESIGN
═══════════════════════════════════════════════════════════════════════
7.1 TASK 4 — OPERATOR CLASS (4 bins) → fills the talk's operator table
  Bins: Independent retail | Branded major | Fleet/non-retail | Government/other.
  No CERS owner_type here, so signals are: SafeGraph BRAND (from the existing xwalk),
  facility_name patterns, NAICS, and fuel flags. Waterfall (first match wins, facility level):
    1. name/NAICS = government/institutional ("CITY OF","COUNTY","STATE","USPS",
       "SCHOOL DIST","DOT", govt NAICS 92xx)                 → Government/other
    2. fleet/non-retail signal: cardlock/terminal/bulk name tokens, diesel/oil-only
       (is_gasoline==0), or no retail-fuel POI within 150 m   → Fleet/non-retail
    3. matched BRAND ∈ major-oil dictionary {Chevron,Texaco,Shell,Exxon,Mobil,BP,
       Arco,76,Phillips66,Conoco,Valero,Marathon,Sinclair,Sunoco,Citgo,Gulf,…}
                                                              → Branded major
    4. residual gasoline facility                            → Independent retail
  Brand dictionary → Code/Helpers/brand_dictionary.R (is_major_oil flag), researcher-reviewed.
  Output gis_05_operator_class.csv: facility_id, state, operator_class, brand_matched,
  match_confidence, rule_fired, n_poi_within_150m. NEEDS a hand-validation sample.

7.2 TASK 5 — POI ECONOMICS (all hang off the EXISTING xwalk's placekey)
  06_foot_traffic.R  Advan WPP → facility × month/qtr: visits, visitors, median_dwell.
  07_spend_revenue.R SafeGraph Spend (+ optional PDI c-store) → facility × month:
                     raw_total_spend, raw_num_transactions, median_spend_per_customer.
  08_property_value.R PRIMARY = ATTOM Tax Assessor (slug tax-assessor): GEOCODED
                     (LATITUDE/LONGITUDE) so facility->parcel is a spatial match like
                     the POIs. Yields ATTOMID (ATTOM parcel id) + PARCELNUMBER* (county
                     APN) + TAXASSESSEDVALUE*/TAXMARKETVALUE*. HUB: ATTOMID joins ATTOM
                     siblings (AVM modeled value, Recorder sales, Assessor History);
                     APN joins external parcel data (county assessor, Regrid, Cotality).
                     FALLBACK = Cotality property-data: ⚠ joins by ADDRID, not coords. Primary
                     robust path = PUBLIC parcel/assessor layer (facility point →
                     parcel polygon → assessed/market value + lot sqft); Cotality as
                     enrichment once an address→ADDRID bridge (via Infutor, on Z) validates.
  Outputs keyed (facility_id, state) via the xwalk's best placekey per facility.

═══════════════════════════════════════════════════════════════════════
8. BUILD PLAN (proposed GIS modules / tickets — extend the 0N numbering)
═══════════════════════════════════════════════════════════════════════
  module / script                         status   task  Dewey?  depends
  Code/Helpers/dewey_query.R              NEW      infra  engine  —          (formalize duckdb reduce)
  Code/Helpers/brand_dictionary.R         NEW      T4     —       —
  00/01 config+census+xwalk               EXISTS   —      —       —          (repoint path F1, fix F2)
  02_neighbor_density.R                   NEW      T1     no      master     (CA recipe, EPSG:5070, by state)
  03_urban_rural.R                        NEW      T2     no      01_census  (RUCA + Census 2020 UA)
  04_drinking_water_wells.R               NEW      T3     no      master     (PWS wells, 600 m Marcus)
  05_operator_class.R                     NEW      T4     yes*    xwalk      (*brand only; rest CERS-free)
  06_foot_traffic.R                       NEW      T5     yes     xwalk
  07_spend_revenue.R                      NEW      T5     yes     xwalk
  08_property_value.R                     NEW      T5     yes     xwalk/parcel
  + a Dewey ACQUISITION spec (what to customize+download to Z)  doc          (gates 05-08)

  WAVES: (0, no Dewey) dewey_query + 02 + 03 + 04 and the coord audit F3.
         (1, acquire) download/customize SafeGraph Places/Spend, Advan WPP, Cotality → Z.
         (2, Dewey) repoint+run xwalk → 05, 06, 07, 08.

Tickets: number from 019 upward (018 is the current max), one per module, each in the
project's spec_template.md format. NOT YET CREATED — awaiting decisions in §9.

═══════════════════════════════════════════════════════════════════════
9. OPEN DECISIONS (need researcher input before tickets are cut)
═══════════════════════════════════════════════════════════════════════
 Q1. Coordinate coverage: confirm per-state lat/long completeness in the Master file,
     ESPECIALLY TX (target). If TX coords are thin, distance tasks (T1, T3) are biased —
     do we backfill from EPA UST Finder / geocode addresses first?
 Q2. Urban/rural definition: USDA RUCA (gradient, tract-level, pairs with 01_census) as
     primary + Census 2020 Urban Area (binary) as secondary? (my recommendation)
 Q3. Wells (T3): national PWS source — EPA SDWIS vs stitched state DDW layers. Coordinate
     quality varies; confirm scope (all 26 states vs coords-available subset). Radii
     {300,600,1000,1609 m} OK; 600 m = Marcus headline.
 Q4. Operator bins (T4): approve the 4-bin waterfall + brand dictionary approach given no
     owner_type; how much hand-validation do you want before it goes in the slide?
 Q5. Dewey acquisition scope: national vs 26-state customize; confirm OK to stage
     SafeGraph Places/Spend (small) and Advan WPP gas-NAICS (tens of GB) onto Z.
 Q6. Property value (T5c): lean on public parcel layer as primary (robust) with Cotality
     as enrichment, given the ADDRID (non-spatial) join? 
 Q7. Time window for the Dewey panels (WPP 2017→, Spend 2019→): full, or trimmed toward
     the 1998 TX shock window (note: mobility/spend panels DO NOT reach 1998 — they are
     2017+/2019+ only, so they inform the modern cross-section, not the historical shock).

═══════════════════════════════════════════════════════════════════════
10. RISKS
═══════════════════════════════════════════════════════════════════════
- Modern-panel mismatch: Advan/Spend start 2017/2019; the insurance design shock is 1998.
  These layers describe today's facilities (descriptive slides, Q1 cross-section), not the
  historical event study. State this clearly wherever used.
- National PWS well coordinates (T3) and TX facility coordinates (F3/Q1) are the two
  feasibility risks; both are auditable up front in Wave 0.
- ADDRID (Cotality/Infutor) linkage is the T5c risk; public parcel de-risks it.
- Keep retired code in Archive/ per project rule; fix F1/F2 when adopting the modules.
