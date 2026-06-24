# 02b Field Audit — Count Field Construction
_Generated: 2026-05-27 | Script: T006_Partial_Shrinkage_Audit.R_

## Count field sources

### n_tanks_active
- **Line(s)**: 870 (`n_tanks_active = .N`)
- **Source**: `tank_year_panel[state %in% STUDY_STATES]`
- **Filters**: STUDY_STATES only. Includes `mm_wall == 'Unknown-Wall'`,
  `make_model_tank == NA`, and `first_year_churn == 1` tanks.
- **Note**: This is `.N` — every tank-year row present for this facility-year.

### n_tanks_eoy
- **Line(s)**: 947 (`n_tanks_eoy = n_tanks_active - n_closures`)
- **Source**: Derived arithmetic — NOT aggregated directly from tank data.
- **Filters**: Inherits asymmetry from n_tanks_active and n_closures (see below).

### n_closures
- **Line(s)**: 887
- **Source**: `sum(closure_event[first_year_churn == 0L | is.na(first_year_churn)])` in `tank_year_panel`
- **Churn exclusion line(s)**: 887, 890, 894, 898, 902, 906, 1513
- **CRITICAL ASYMMETRY**: `first_year_churn == 1` tanks are counted in `n_tanks_active`
  (their row is present) but their `closure_event` is EXCLUDED from `n_closures`.
  Result: `n_tanks_eoy = n_tanks_active - n_closures` overcounts by the number of
  churn tanks at this facility-year (they are gone by EOY but not subtracted).

### n_sw_closures
- **Source**: `tank_year_panel`, filtered to `mm_wall == 'Single-Walled'` AND NOT first_year_churn.
- **Filters**: Consistent with n_closures (same churn exclusion).

### n_dw_closures
- **Source**: Same as n_sw_closures but `mm_wall == 'Double-Walled'`.
- **Filters**: Consistent.

### n_installs
- **Line(s)**: 984 (S12.3)
- **Source**: `study_tanks[!is.na(tank_installed_date)]`, grouped by `year(tank_installed_date)`.
- **DIFFERENT SOURCE**: `study_tanks` is the master tank file, NOT `tank_year_panel`.
  Requires non-NA install date. Includes Unknown-Wall tanks.
- **FIRST_YEAR_CHURN COUNTS HERE**: churn tank installs ARE counted in n_installs.
  Combined with the exclusion from n_closures, this is the direct driver of gap > 0:
  gap = (n_tanks_active - n_closures + n_installs) - n_tanks_eoy = n_installs
  (since n_tanks_eoy = n_tanks_active - n_closures by construction).
  For partial-shrinkage events where n_installs > 0 (current-year installs with no
  future installs), gap == n_installs > 0 by arithmetic identity.

### n_sw_installs
- **Line(s)**: 986
- **Filter line**: `n_sw_installs      = sum(mm_wall == "Single-Walled", na.rm = TRUE),`
- **Labels appear correct (Single-Walled for n_sw_installs)**

### n_dw_installs
- **Line(s)**: 985
- **Filter line**: `n_dw_installs      = sum(mm_wall == "Double-Walled", na.rm = TRUE),`

---

## Findings

### Finding 1 — first_year_churn asymmetry (HIGH SEVERITY — PANEL_BUG candidate)

**Lines involved**: n_tanks_active=870 | n_closures=887 | n_tanks_eoy=947 | n_installs=984

The count identity gap = n_installs (algebraic consequence when n_tanks_eoy is defined
as n_tanks_active - n_closures). For partial-shrinkage events, gap > 0 iff n_installs > 0
in the current year. This is not a noise issue — it is a direct consequence of the
first_year_churn exclusion being applied asymmetrically:
  - Churn installs counted in n_installs (S12.3)
  - Churn closures excluded from n_closures (S12.1)
  - Churn tanks included in n_tanks_active (S12.1)

**Fix (Option A)**: Exclude churn tanks from n_tanks_active.
  Change line 870 from `.N` to
  `sum(first_year_churn == 0L | is.na(first_year_churn), na.rm = TRUE)`

**Fix (Option B)**: Include churn closures in n_closures.
  Remove the `first_year_churn == 0L | is.na(first_year_churn)` filter on line 887.

**Fix (Option C)**: Exclude churn installs from n_installs.
  In S12.3 (line 984), filter out tanks where `year(tank_installed_date) == year(tank_closed_date)`.

All three fixes make the count identity self-consistent. Choose based on economic intent.

### Finding 2 — n_installs from different source table (MEDIUM SEVERITY)

`n_installs` aggregates from `study_tanks` (master file); `n_tanks_active` aggregates
from `tank_year_panel`. `tank_year_panel` excludes tanks with `expand_start > expand_end`
(impossible date ordering). `study_tanks` does not apply this filter. If any tanks have
install dates after close dates (data entry errors), they appear in n_installs but not
n_tanks_active — another source of count inconsistency.

### Finding 3 — n_sw_installs/n_dw_installs label swap (LOW SEVERITY)
Lines 986 and 985 — verify that n_sw_installs selects the intended wall type.
