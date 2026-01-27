# Turning Monthly Panel into Annual



We need to make the facilyt-year panel.


My thoughts of the code by Sections of 10_Build_Master_panel.R 

Section 1

1.1
Loads and lists needed the data, still needed in the annual code
1.1.2 
- adds the county data 
- Load and standardized  zurich and Texas FR data

1.2  Maps variables names to panel data names

1.2.2 Maps LUST dataset names

1.2.3 Maps the FR texas data names

1.2.4 Maps panel variables to variable types

1.3 We apply the maps to the data sets

1.3.1 Maps texas tank panel data

1.3.2 Maps the EPA dataset

1.3.3 Maps the LUST datasets

1.3.4 Maps the Texas Datsets

1.4 Standarzied all the is across all the datasets so we can have the same merge keys later

1.4.1 Creates the panel id needed. Whichi is facility_id and state

1.5 Clean and standarize tank install and close dates across the datasets

1.5.1 Enfource the same date variable context

1.5.2 Fix bad install date variables

1.5.3 fix bad closure dates

1.6 Set the bounds of the panel dates

1.6.1 Use the install date with the panel start date to create the tanks PANEL_START date which is distninct from the install dates

1.6.2 Use the closed date with the panel start date to create the tanks PANEL_END date which is distninct from the install dates

1.6.3 Check for bad dates observatoins

1.7 Once again standarized all the dates variabse with as.Date() -- need to make sure this makes sens

1.8 Summary report of the date cleaning

1.9 Remove bad date tanks from panel and calulate the number of errors


Section 2 Panel creation

2.1 Creat the tank-level data set from EPA data

2.2 Create tank level data for Texasd data

2.3 combine the unified texas and epa data into one tank inventory dataset

Section 3 CREATE FACILITY-MONTH BACKBONE

QUickly use teh tank inventory data to create the faciliyt-month level backbond to do the panel data creatoin


Section 4 does the backboin joins to create panel data


4.1  Creates clean tank dataset with only what we need for memory issues.

4.2 does the tank inventoyr join to the backbone data set and then aggeragates by state for memory erason to build the tank characatersitc monthly panel.

4.3 Combines all the state datsets into one

4.4 merges all the indiviual counts back to the full panel

4.5 Makes some small tank facility charactersics variables

4.6 prints some summary stats

Section 5 Crudly identifes motor fuel only facilits.

section 6 

6.1 Creates inital tank stocks counts for facilites that were active at panel start and then identife tank closures and install eventes

6.2 Figures out the  installtion eventes within the panel

6.3 Figoures out the closure eventes within the panel

6.4 left joins on the install and closure events and type of evnet info

6.5 figures out changes in number of tanks in a month and th total tanks and faciliyt size


6.6 Identifes replacemtn events and classifies how that changegs the facility.

6.7 prints summary stats


Section 7 Lust data ading

Adds lust classificaitons to the panel but we should replace with the below :
----------------start of new section 7 ------------------------------------------
#==============================================================================
# SECTION 7: ROBUST EVENT CLASSIFICATION (Precision Method)
#==============================================================================
log_step("Classifying Leaks/Closures (Using Exact Tank Dates)...", 1)

#------------------------------------------------------------------------------
# 1. EXTRACT PRECISE EVENTS (The "Gold Standard" Data)
#------------------------------------------------------------------------------
# CLOSURES: Use 'tank_panel_unified' (from Section 2) for EXACT dates
# We aggregate to the facility-day level (if 3 tanks close on Jan 28, it's 1 event)
closure_events_exact <- tank_panel_unified[
  !is.na(tank_closed_date_raw) & 
  tank_closed_date_raw >= as.Date("1970-01-01") & 
  tank_closed_date_raw <= as.Date("2025-12-31"), 
  .(
    n_tanks_closed = .N
  ), 
  by = .(panel_id, state, closure_date = as.Date(tank_closed_date_raw))
]

# LEAKS: Use raw LUST data for EXACT dates
all_leaks_exact <- rbindlist(list(
  lust_epa[, .(panel_id, report_date = as.Date(report_date))],
  if (exists("TX_LUST_SD")) TX_LUST_SD[, .(panel_id, report_date = as.Date(report_date))] else NULL
), fill = TRUE)
all_leaks_exact <- unique(all_leaks_exact) # Remove dupes
all_leaks_exact[, leak_id := paste0(panel_id, "_", report_date)]

#------------------------------------------------------------------------------
# 2. CARTESIAN JOIN (Exact Dates)
#------------------------------------------------------------------------------
setkey(closure_events_exact, panel_id)
setkey(all_leaks_exact, panel_id)

# Join leaks to closures
event_pairs <- all_leaks_exact[closure_events_exact, on = .(panel_id), allow.cartesian = TRUE]

# Calculate PRECISE difference in days (No monthly rounding error)
event_pairs[, diff_days := as.integer(report_date - closure_date)]

#------------------------------------------------------------------------------
# 3. ROBUSTNESS WINDOW DEFINITIONS (Unchanged)
#------------------------------------------------------------------------------
specs <- list(
  primary = list(A_start=0, A_end=60, B_thresh=-180),
  narrow  = list(A_start=0, A_end=30, B_thresh=-365),
  wide    = list(A_start=0, A_end=90, B_thresh=-90),
  reg     = list(A_start=0, A_end=45, B_thresh=-180)
)

classify_behavior <- function(days, s) {
  fcase(
    days >= s$A_start & days <= s$A_end, "Closure-Revealed", 
    days < s$B_thresh,                   "Known-Leak",       
    days >= s$B_thresh & days < s$A_start, "Indeterminate",    
    default = NA_character_
  )
}

event_pairs[, `:=`(
  cat_primary = classify_behavior(diff_days, specs$primary),
  cat_narrow  = classify_behavior(diff_days, specs$narrow),
  cat_wide    = classify_behavior(diff_days, specs$wide),
  cat_reg     = classify_behavior(diff_days, specs$reg)
)]

#------------------------------------------------------------------------------
# 4. AGGREGATION & CLEAN RESET
#------------------------------------------------------------------------------
RESET_DAYS <- 365 * 5
relevant_pairs <- event_pairs[abs(diff_days) <= RESET_DAYS]

# Aggregate to Closure Event level (Exact Date)
closure_flags <- relevant_pairs[, .(
  has_revealed = max(as.integer(cat_primary == "Closure-Revealed"), na.rm=TRUE),
  has_known    = max(as.integer(cat_primary == "Known-Leak"), na.rm=TRUE),
  has_indeterm = max(as.integer(cat_primary == "Indeterminate"), na.rm=TRUE),
  is_multi_type = as.integer(uniqueN(cat_primary[!is.na(cat_primary)]) > 1)
), by = .(panel_id, closure_date)]

#------------------------------------------------------------------------------
# 5. MAP TO MONTHLY PANEL (The "Bridge")
#------------------------------------------------------------------------------
# Now we map these precise classifications back to the monthly panel buckets
closure_flags[, `:=`(
  panel_year = year(closure_date),
  panel_month = month(closure_date)
)]

# If a facility has multiple closure events in one month (e.g., Jan 5 and Jan 28),
# we take the "Maximum Risk" classification for the whole month.
# RENAMED VARIABLES: exit_ -> tank_closure_
monthly_flags <- closure_flags[, .(
  tank_closure_revealed     = max(has_revealed, na.rm=TRUE),
  tank_closure_known_leak   = max(has_known, na.rm=TRUE),
  tank_closure_indeterminate= max(has_indeterm, na.rm=TRUE),
  tank_closure_complex_multi= max(is_multi_type, na.rm=TRUE)
), by = .(panel_id, panel_year, panel_month)]

# Merge to Panel
setkey(panel, panel_id, panel_year, panel_month)
panel <- monthly_flags[panel, on = .(panel_id, panel_year, panel_month)]

#------------------------------------------------------------------------------
# 6. FINAL CLEANUP (Fill NAs)
#------------------------------------------------------------------------------
# Any month with 'tanks_closed > 0' that didn't match a flag is a CLEAN CLOSURE
panel[tanks_closed > 0, `:=`(
  tank_closure_revealed      = fifelse(is.na(tank_closure_revealed), 0L, tank_closure_revealed),
  tank_closure_known_leak    = fifelse(is.na(tank_closure_known_leak), 0L, tank_closure_known_leak),
  tank_closure_indeterminate = fifelse(is.na(tank_closure_indeterminate), 0L, tank_closure_indeterminate),
  tank_closure_complex_multi = fifelse(is.na(tank_closure_complex_multi), 0L, tank_closure_complex_multi)
)]

# Define Clean Closure (renamed from exit_clean)
panel[tanks_closed > 0, tank_closure_clean := as.integer(
  tank_closure_revealed == 0 & 
  tank_closure_known_leak == 0 & 
  tank_closure_indeterminate == 0
)]

# Define Leak-Related Aggregate (renamed from exit_any_leak_related)
panel[tanks_closed > 0, tank_closure_any_leak_related := as.integer(tank_closure_clean == 0)]

#------------------------------------------------------------------------------
# 7. SUMMARY
#------------------------------------------------------------------------------
log_step("Generating Classification Summary (Precision Method)...", 1)

panel[tanks_closed > 0, summary_cat := fcase(
  tank_closure_complex_multi == 1, "Multi-Event (Overlapping)",
  tank_closure_revealed == 1,      "Single: Closure-Revealed",
  tank_closure_known_leak == 1,    "Single: Known-Leak",
  tank_closure_indeterminate == 1, "Single: Indeterminate",
  tank_closure_clean == 1,         "Single: Clean Closure",
  default = "Unclassified"
)]

summary_table <- panel[tanks_closed > 0, .N, by = summary_cat][order(-N)]
summary_table[, pct := round(100 * N / sum(N), 2)]

cat("\n=== FIRM BEHAVIOR SUMMARY (Exact Date Method) ===\n")
print(summary_table)

log_step("✓ Classification complete.", 1)

----------------End of new section 7 ------------------------------------------



Section 8 -- Creates Treatament and Policy VAriables

Flags treated and control and treatment and control dates. these are wrong.
the treatment effective date for texas is 1998-12-21. so all of 1999 is treated
[ post_1999 := 1*(panel_year >= 1999)] -- need to fix that for texas

Section 9: DERIVED VARIABLES AND SURVIVAL ANALYSIS
 this makes a bunch of variables i think it needs to be checked by what we want to do in 
 Code\Analysis\01_Master_Descriptives.R
Code\Analysis\02_DiD_Results.R

we need to make this cchanges
-------------------Section 9 changes----------------------------
#==============================================================================
# SECTION 9B: STANDARDIZED SURVIVAL VARIABLES (Pulse / Ever / Event)
#==============================================================================
log_step("Creating standardized Pulse/Ever/Event variables...", 1)

# Sort strictly by ID and Date to ensure cumulative logic works
setorder(panel, facility_id, state, date)

#------------------------------------------------------------------------------
# 1. OUTCOME: LEAKS (The "Failure" Event)
#------------------------------------------------------------------------------
panel[, `:=`(
  # PULSE: Did a leak report happen strictly in this month? (Existing 'leak_incident')
  pulse_leak = as.integer(leak_incident == 1),
  
  # EVER: Has this facility EVER leaked up to this point? (Absorbing State)
  ever_leaked = as.integer(cumsum(leak_incident) > 0)
), by = .(facility_id, state)]

# EVENT: The transition from Clean -> Leaked (Time-to-First-Leak)
# Defined as: It is a leak pulse AND it is the first time 'ever_leaked' turned on.
panel[, event_first_leak := as.integer(pulse_leak == 1 & cumsum(pulse_leak) == 1), 
      by = .(facility_id, state)]


#------------------------------------------------------------------------------
# 2. OUTCOME: FACILITY EXIT (The "Market Departure" Event)
#------------------------------------------------------------------------------
# Note: 'exit_flag' was defined earlier as active_tanks going to 0.
panel[, `:=`(
  # PULSE: Did the facility exit the market this month?
  pulse_exit = as.integer(exit_flag == 1),
  
  # EVER: Is the facility currently "dead"? (Absorbing State)
  ever_exited = as.integer(cumsum(exit_flag) > 0)
), by = .(facility_id, state)]

# EVENT: Facility exits are usually terminal, so Pulse == First Event.
# We rename it for consistency in your survival code.
panel[, event_exit := pulse_exit]


#------------------------------------------------------------------------------
# 3. OUTCOME: ANY TANK CLOSURE (The "Partial Adjustment" Event)
#------------------------------------------------------------------------------
# Useful if you want to model "Time to First Tank Removal" (even if facility stays open)
panel[, `:=`(
  # PULSE: Did ANY tank close this month?
  pulse_closure = as.integer(tanks_closed > 0),
  
  # EVER: Has this facility EVER closed a tank?
  ever_closed_any = as.integer(cumsum(tanks_closed) > 0)
), by = .(facility_id, state)]

# EVENT: Time to First Closure (e.g., first time they downsized)
panel[, event_first_closure := as.integer(pulse_closure == 1 & cumsum(pulse_closure) == 1), 
      by = .(facility_id, state)]

#------------------------------------------------------------------------------
# SUMMARY & VALIDATION
#------------------------------------------------------------------------------
cat("\n=== SURVIVAL VARIABLE SUMMARY ===\n")
cat("1. Leaks (Failure):\n")
cat(sprintf("   - Pulses (Total Leaks): %s\n", sum(panel$pulse_leak)))
cat(sprintf("   - First Events (Unique Failures): %s\n", sum(panel$event_first_leak)))

cat("2. Exits (Market Departure):\n")
cat(sprintf("   - Exits (Terminal Events): %s\n", sum(panel$event_exit)))

cat("3. Closures (Partial/Any):\n")
cat(sprintf("   - Pulses (Months with Closures): %s\n", sum(panel$pulse_closure)))
cat(sprintf("   - First Events (First Downsizing): %s\n", sum(panel$event_first_closure)))

-------------------Section 9 changes----------------------------

Section 10: TEXAS FR DATA MERGE (CORRECTED)

Improtant adding of texas specific FR data. 
this data has within year changes because of  how contracts are just done. Need to be careful to identify and keep eveent- orddering variabls 
since these are contract-months dataset we need to summaize how the facililty contract motnhs changed over the year or didnt.

we  want to calculate the share of the year a firm
  multiple_contracts = 0L,
  fr_covered = FALSE,
  uses_private = 0L,
  uses_state_fund = 0L,
  uses_self = 0L,
  coverage_gap_month = 0L,
    transition_month = 0L
      erp_reporting_month = 0L,

Then these two are 1/0 variables for the year identifhing them being treated by this insurance dropping event.
  had_zurich_2012 = 0L,
These are character strings where we also wnat to know the share of the year or the number of contract monhts in a year(whatever is fastest/easiest) they are under each category,issueer  and detail type.
  DETAIL_TYPE = NA_character_,
  CATEGORY = NA_character_,
  ISSUER_NAME = NA_character_,


Section 11: some minor code validations. and clearning.
i am a bit worried we dont save out all the data vriables we crated want to maek sure that happens.

Section 12: saves the data

