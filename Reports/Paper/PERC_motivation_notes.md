# Motivation Research Notes — UST Leak Rates & State-Fund Sunsets

Two motivation questions, researched with citations (June 2026). Figures are
quotable; flagged caveats matter for honesty. Sources are primary (EPA, ASTSWMO,
state agencies, peer-reviewed PNAS / Marine & Petroleum Geology) wherever possible.

---

## Q1. How leak-prone are USTs, vs. oil & gas wells?

### The UST numbers (EPA, current)

- **Stock:** 534,189 active USTs at ~190,224 facilities (EPA, as of March 2026).
- **Flow:** 1,997 confirmed releases in Oct 2025–Mar 2026 (6 months) → **~4,000
  new confirmed releases per year** run-rate.
- **Implied annual release rate:** ~4,000 / 534,189 ≈ **0.7–0.8% per active
  tank-year** (≈ **2.1% per facility-year**). This matches the paper's own
  estimated first-release hazard (~0.70%/tank-year) — useful external validation.
- **Cumulative:** **583,313 confirmed releases since the 1984 program inception**;
  ~530,454 cleanups completed; ~52,859 remaining (EPA, March 2026).
  → *Cumulative confirmed releases (583k) exceed the entire current active stock
  (534k).* USTs are not a rare-tail hazard; loss of containment is routine.
- **Tanks ever regulated:** ~**1.5 million permanently closed** since program start
  (GAO) + 534k active ≈ **~2.0 million tanks ever**.
- **Cost context:** state assurance funds have paid ≈ **\$20 billion** since 2002
  to clean up LUST sites; combined they raise/spend ≈ **\$1 billion/year** on top
  of the federal LUST Trust Fund (EPA / ASTSWMO).

*Sources:*
- EPA, *UST Performance Measures* (semiannual, data through March 2026):
  https://www.epa.gov/ust/ust-performance-measures
- EPA, *Releases from Underground Storage Tanks* (581,000+ as of Sept 2025;
  ~528,000 cleaned up): https://www.epa.gov/ust/releases-underground-storage-tanks
- EPA, *State Financial Assurance Funds* (≈\$20B since 2002):
  https://www.epa.gov/ust/state-financial-assurance-funds

### The oil & gas well numbers (peer-reviewed)

Well "leaks" are measured as **barrier / wellbore-integrity failure** — not
identical to a UST "confirmed release," see caveat below. Ranges are wide and
definition-dependent:

- **Davies et al. (2014)**, *Marine and Petroleum Geology* — the canonical review:
  "The percentage of wells with **barrier element failure is between 1.9% and
  75%**" across global datasets. (Highly variable by region, age, definition.)
- **Jackson (2014)**, *PNAS* / "The integrity of oil and gas wells": historical
  well-failure rates run "from a **few percent** of wells with barrier failures
  to **>40%**."
- **Ingraffea et al. (2014)**, *PNAS* — Pennsylvania state inspection records,
  wells drilled 2000–2012: compromised cement and/or casing integrity in
  **0.7%–9.1%** of active wells; **unconventional (Marcellus) wells failed at
  ~1.6×–2.7× the rate** of conventional; ~6–7% for recent unconventional
  cohorts. (Cornell's press framing — "~4 in 10 will eventually leak in NE PA" —
  is a contested lifetime projection, not an observed rate; cite the 0.7–9.1%
  observed figure.)
- **Lackey et al. (2021)**, *PNAS* — three-state data: **14.1%** of PA wells
  tested before 2018 exhibited sustained casing pressure (SCP) or casing vent
  flow (CVF).
- **Industry caveat (King 2013, via Energy In Depth):** "true well integrity
  failure" (actual loss of containment to the environment) is "two to three
  orders of magnitude lower than single-barrier failure rates." → barrier failure
  ≠ environmental release.

*Sources:*
- Davies, R.J. et al. (2014). "Oil and gas wells and their integrity." *Marine
  and Petroleum Geology* 56: 239–254.
- Jackson, R.B. (2014). "The integrity of oil and gas wells." *PNAS* 111(30):
  10902–10903. (PMC4121783)
- Ingraffea, A.R. et al. (2014). "Assessment and risk analysis of casing and
  cement impairment in oil and gas wells in Pennsylvania, 2000–2012." *PNAS*
  111(30): 10955–10960.
- Lackey, G. et al. (2021). "Public data from three US states provide new
  insights into well integrity." *PNAS* 118(14): e2013894118.

### Boomhower (2019) — does the closest paper report a well spill rate?

Checked. **Boomhower (2019, AER 109(2)) does NOT report a clean per-well annual
spill/leak rate.** His environmental-risk outcomes are (i) **orphan wells**,
(ii) **water-protection rule violations**, and (iii) **well blowouts** — counts
and policy-induced *changes*, not a release hazard. He states plainly that
"environmental incidents are rare." What he does provide, useful for us:
- The contamination-*source* split via Kell (2011): in Texas (1983–2008) detected
  oil-and-gas groundwater contamination came **17% from orphan wells, 33%
  production activities, 43% waste management/disposal, 6% drilling/completion**;
  in Ohio, **22% / 21% / 14% / 40%**.
- Risk is concentrated in marginal operators: ~**100% of orphan wells, 95% of
  rule violations, 40% of blowouts** trace to the riskiest 20% of production;
  exiting firms had **42% more violations per well** than stayers.
- A universal bond mandate (TX 2001) cut the orphaning rate ~**50%**.

### The actual oil-and-gas groundwater contamination *rate* (Kell 2011)

The real per-well release rate Boomhower leans on comes from **Kell (2011)**,
GWPC two-state review:
- **Ohio: 185 documented groundwater contamination incidents in ~65,000 wells
  over 1983–2007 ≈ 0.28% of wells (cumulative, 25 yr).** Orphan wells = 41 of them.
- **Texas: 211 incidents over 1993–2008; only 10 traced to the well itself**
  (rest from pits, production, waste) — against Texas's far larger well count, a
  per-well rate well below 0.1%.

→ Documented oil-and-gas well groundwater contamination is **rare per well
(~0.1–0.3% cumulative)** — *orders of magnitude below* the integrity/barrier
failure rates above, because most barrier failures never produce a detected
groundwater release.

*Sources:*
- Boomhower, J. (2019). "Drilling Like There's No Tomorrow: Bankruptcy,
  Insurance, and Environmental Risk." *American Economic Review* 109(2): 391–426.
  (already in `UST_lit`: `boomhower_drilling_2019`)
- Kell, S. (2011). *State Oil and Gas Agency Groundwater Investigations and Their
  Role in Advancing Regulatory Reforms, A Two-State Review: Ohio and Texas.*
  Ground Water Protection Council. (Ohio 185 / ~65k wells; Texas 211, 10
  well-related — counts confirmed via ACS *Environmental Aspects of Hydraulic
  Fracturing*, 2015, and secondary reviews.)

### Making the units comparable (this is the point)

Match like with like. The clean comparison is **the share of buried units that
lose containment over their operating life** — UST lifetime cumulative release
vs. the well **integrity-failure** literature (which is already cumulative-over-
life). To get there, convert the UST **annual flow** into that cumulative/
lifetime unit. Two independent methods agree:

- **Method 1 — compound the per-tank annual hazard.** EPA's ~0.7%/tank-year, over
  a 25–30-year service life: `1 − (1 − 0.007)^25 ≈ 16%`; `^30 ≈ 19%`. → **~15–20%
  lifetime release probability per tank.** Conservative (holds the hazard flat;
  it actually *rises* with age).
- **Method 2 — EPA stock-flow cumulative.** 583,313 cumulative confirmed releases
  ÷ ~2.0M tanks ever regulated (1.5M closed + 534k active) ≈ **~25–29%.**
- **Reconcile:** Method 2 > Method 1 because today's 0.7%/yr is *down* from the
  higher rate the older bare-steel cohort actually lived through. Both bracket
  **~1 in 4 to 1 in 6 tanks leaks over its operating life.**

Now apples-to-apples — UST **~15–29% lifetime** (≈1 in 4–6) vs. oil-and-gas
**well integrity / barrier failure**:
- Ingraffea 2014 (PA, observed): **0.7–9.1%**
- Lackey 2021 (PA, sustained casing pressure / vent flow): **14.1%**
- Davies 2014 (global review, barrier failure): **1.9–75%**

→ **UST lifetime release rate is comparable to, and toward the upper end of, well
integrity-failure rates** — tanks lose containment at least as often as the wells
that anchor a large environmental-risk literature, yet draw far less attention.

**Measure caveat — don't over-claim, the three rates are not the same event:**
- Well *integrity / barrier failure* = a barrier breached; need not release to the
  environment (King 2013: true loss-of-containment is orders of magnitude rarer
  than barrier failure). This is the figure quoted above.
- UST *confirmed release* = an actual detected release (soil and/or groundwater).
- Oil-and-gas *documented groundwater contamination incident* (Kell 2011) = rarer
  still, ~0.1–0.3% of wells cumulatively — but a **stricter, groundwater-specific
  threshold**, so **NOT unit-matched** to a UST confirmed release. Use Kell only
  as context ("documented groundwater contamination specifically is rare for
  wells"), never as a 1-in-4-vs-0.1% headline.

The defensible slide claim is the cumulative-over-life comparison to well
integrity failure: **same order of magnitude, upper end.**

### From our own data (preferred — replaces the EPA-only number)

Integrating the **observed** annual first-leak hazard from `01n_CVValidation.R`
(18-state pre-treatment panel, 3.93M facility-years, age×wall cell rates in
`Table_CV_CellRates_Observed.csv`) over a tank's operating life, via a
synthetic-cohort life table `CI(L)=1−∏(1−h_age)`:

| Operating life | Single-wall | Double-wall | Fleet (comp-weighted) |
|---|---|---|---|
| 24 yr | 17.1% | 10.5% | 16.2% (1 in 6.2) |
| **30 yr** | **23.4%** | **13.9%** | **22.1% (1 in 4.5)** |
| 35 yr | 28.2% | 16.7% | 26.6% (1 in 3.8) |

→ Our own CV-validated hazard model lands on the EPA/GAO national "1 in 4"
figure *independently*, and decomposes it by wall type — tying the motivation to
the SW/DW risk gradient the structural model is built on. Reproduced by new
section **S12** in `01n_CVValidation.R` (writes `Table_CV_LifetimeLeakProb.csv`).
*Unit note:* our estimand is per **facility** (a facility may hold several
tanks); EPA's "1 in 4" is per **tank**. Different denominators landing at the
same magnitude — say "facility" on the slide.

### Slide line (1–2 sentences, pick one)

- **Our-data framing (strongest — own model + ties to wall type):**
  "In our 18-state panel, the age–wall hazard model implies **about 1 in 4
  facilities (≈23% single-walled, ≈14% double-walled) suffers a confirmed release
  over a 30-year tank life** — comparable to, and at the upper end of, oil-and-gas
  well integrity-failure rates (0.7–9.1% in PA; 14.1% sustained casing pressure;
  1.9–75% across studies)."
- **Cumulative framing (same construction as the well stat):**
  "About **1 in 4 underground storage tanks ever regulated has had a confirmed
  release** (583k releases across ~2.0M tanks; EPA/GAO) — on par with or above
  oil-and-gas well integrity-failure rates (0.7–9.1% in Pennsylvania, up to 75%
  across studies)."

*(Keep the Kell ~0.1–0.3% figure out of the headline — different/stricter measure;
see measure caveat above.)*

---

## Q2. How many state funds are set to sunset / wind down?

### The current landscape (EPA, 2026)

EPA classifies all states into three buckets:

- **36 states** have **active** financial assurance funds (pay for new + ongoing
  cleanups): AL, AR, CA, CO, GA, ID, IL, IN, KS, KY, LA, ME, MA, MI, MN, MS, MO,
  MT, NE, NV, NH, NM, NC, ND, OH, OK, PA, RI, SC, SD, TN, UT, VT, VA, WA, WY.
- **6 states have already PHASED OUT** their funds — pay only for releases
  reported before phase-out, owners now use private/risk-rated FR:
  **Arizona, Connecticut, Florida, New York, Texas, Wisconsin.**
  → *This is the key motivation fact: Texas is not unique. Six states have made
  the same flat-fee → private/risk-based transition the paper studies.*
- **8 states + DC + 5 territories** rely **entirely on private FR mechanisms**
  (no state fund): AK, DE, HI, IA, MD, NJ, OR, WV (+ DC, AS, CNMI, Guam, PR, VI).

*Source:* EPA, *State Financial Assurance Funds* (Map of State Financial
Assurance Funds, 2026):
https://www.epa.gov/ust/state-financial-assurance-funds

### Sunsets are an active, recurring pressure

- **California — the marquee current case.** The UST Cleanup Fund was scheduled
  to **sunset January 1, 2026**; in **2024 the Legislature extended it to
  January 1, 2036** (AB 753 lineage; prior 2014 extension had moved an earlier
  sunset to 2026). → The largest state fund in the country operates under a
  **statutory sunset that must be re-litigated roughly every decade** — renewal
  is political, not guaranteed. *(California is simultaneously mandating removal
  of all single-walled USTs by Dec 31, 2025 — a direct real-world analog to the
  paper's SW/DW risk distinction and the "age mandate" counterfactual.)*
  - Source: CA State Water Resources Control Board, *Single-Walled USTs / Timeline
    FAQs*: https://www.waterboards.ca.gov/ust/ — "In 2024, the California
    Legislature extended the sunset date of the UST Cleanup Fund from Jan. 1,
    2026, to Jan. 1, 2036."
- **Other funds carry statutory sunsets / claim-submission deadlines** that have
  already triggered wind-downs (ASTSWMO state-fund surveys document funds with
  release-reporting deadlines of 2004, 2006, etc.).

### Why the shift is structural, not one-off — ASTSWMO 2023

ASTSWMO's Financial Responsibility Task Force, *Sustainability of State Financial
Assurance Funds for the UST Programs* (Feb 22, 2023), identifies **four trends
that are eroding the revenue base** these funds depend on (most are funded by
per-gallon gasoline fees / tank fees):

1. Transition from gasoline to alternative-energy fuels (EVs).
2. Changes in how the public uses transportation.
3. The future of retail gas stations (consolidation / closure).
4. Political changes affecting UST program funding.

→ Funds tied to gasoline throughput face **declining revenue against aging tank
infrastructure and a long cleanup backlog**. The Task Force asked EPA OUST for a
deeper analysis "considering the potential for significant impact on state
programs, most particularly state financial assurance funds." This is the
evidence that the Texas-style transition is a **structural trajectory for the
remaining 36 fund states**, not a Texas idiosyncrasy.

*Source:* ASTSWMO (2023), *Sustainability of State Financial Assurance Funds for
the Underground Storage Tank Programs*:
https://astswmo.org/2023-sustainability-of-state-financial-assurance-funds-for-the-underground-storage-tank-programs/

### How to use it

- **One-liner for the intro:** "The Texas reform is not a historical curiosity:
  six states have already wound down their flat-fee funds, and the remaining 36 —
  including California, whose fund survives only on a recurring statutory sunset
  extended in 2024 to 2036 — face structurally declining gasoline-fee revenue
  (ASTSWMO 2023) against aging infrastructure. The flat-fee → risk-based
  transition this paper studies is the policy question many states are now
  approaching."
- **External validity hook:** the paper estimates the welfare consequences of
  exactly the transition a growing number of states must decide on.

---

## Open verifications / to double-check before citing in print

- Confirm the 6 phased-out states list against the EPA 2026 map at cite time
  (EPA updates this page; it was AZ/CT/FL/NY/TX/WI as of the April 2026 pull).
- The "~4,000 releases/yr" is a current run-rate from one 6-month period; if you
  want a stable annual figure, pull 3–5 years of EPA semiannual reports and
  average. Annual counts were much higher in the 1990s (verify the peak before
  asserting a number).
- Ingraffea's 0.7–9.1% is the **observed** PA inspection figure; do not cite the
  "4 in 10" projection as an observed rate.
- Pull the actual ASTSWMO PDF for direct quotes if you quote the four trends
  verbatim.
