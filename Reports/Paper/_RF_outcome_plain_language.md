# Reduced-form outcomes — plain-language statements (title / Notes / prose)
# For the editor. Use the SAME plain phrase in the table title, the \textit{Notes:} block,
# and the body text. Never lead with a bare variable name (downsize/consolidate/…).

## Facility-level outcomes (02j)

| variable | title phrase | Notes definition (one line) | in-prose phrase |
|---|---|---|---|
| `closure_share` | Share of a station's tanks closed | Of the tanks a facility had in operation that year, the share it closed (0–1). | the share of its tanks a station closed |
| `any_closure` | Closed at least one tank | Equals one if the facility closed any tank that year, zero otherwise. | whether a station closed any tank |
| `facility_exit` | Station shut down completely | Equals one in the year a facility closed its last tank and stopped operating. | whether the whole station shut down |
| `downsize` | Shrank (fewer tanks and less capacity) | Equals one if the facility ended the year with fewer tanks AND at least 5% less total fuel-storage capacity than the year before. | shrank — dropped tanks and lost capacity |
| `consolidate` | Consolidated (fewer tanks, same capacity) | Equals one if the facility ended the year with fewer tanks but kept total capacity within 5% of the year before (e.g. several small tanks for one larger one). | consolidated — fewer tanks, same total capacity |
| `reconfigure_up` | Upgraded (fewer tanks, more capacity) | Equals one if the facility ended the year with fewer tanks but more than 5% more total capacity than the year before. | upgraded — fewer but larger tanks |
| `perm_share` | Share of tanks permanently removed | Of the tanks in operation, the share closed and NOT replaced that year. | the share of tanks permanently removed |
| `repl_share` | Share of tanks replaced | Of the tanks in operation, the share closed and replaced with a new tank that year. | the share of tanks swapped for new ones |
| `cap_decrease` | Total capacity fell | Equals one if the facility's total fuel-storage capacity was lower than the year before (any decrease). | whether total storage capacity fell |

## Tank-level outcome (02g / 02h)

| variable | title phrase | Notes definition | in-prose phrase |
|---|---|---|---|
| `closure_event` | Tank closed | Equals one in the year a given tank was closed, zero otherwise. | whether an individual tank was closed |

## Event-study y-axis labels (replace the current jargon labels)

"Effect on the chance a station **shut down** / **closed a tank** / **shrank** / **consolidated** /
**upgraded** / **replaced a tank**" — matching the outcome above.

## Grouping variables (plain language for HTE titles/notes)

- **Size (`cap_G`):** station size by total tank capacity — G1 = under 9,000 gal (smallest, the
  comparison group), G2 = 9–20k, G3 = 20–30k, G4 = 30,000+ gal (largest).
- **Vintage:** how old the station's tanks were at the 1998 reform — comparison group is the newest
  (1989–98); older cohorts are 1985–88, 1975–84, Pre-1975.
- **Gas station:** sells gasoline. **Pure-gasoline:** all of the station's tanks are gasoline.
- **Rural / low population / low income / high poverty:** where the station is, measured at the 2000
  Census and fixed before the reform.
- **Thin market:** few competing gas stations within a mile, measured at the 1998 reform.
