# Reduced-form results — plain-language design + every coefficient interpreted
# For the editor. Numbers from the server run 2026-06-26 (Ticket 031). One sentence per coefficient.

> **One rule for the writer:** every result must be readable by a high-school student. Name the
> outcome in plain words (see `_RF_outcome_plain_language` glossary), state the number, then say
> what it means in stations-out-of-100 terms. A "percentage point (pp)" = a change out of 100
> stations (or out of 100 tanks for share outcomes).

---

## 1. What the analysis does (explain it this simply)

Texas switched its underground-fuel-tank insurance to **risk-based pricing on 22 December 1998**:
after that, a tank's insurance price depended on how risky it was. We ask what station owners did
in response.

The trick is to find out what the reform *caused*, not just what happened over time. So:

1. **Compare Texas to 17 other states** that did **not** change their rules. Anything that happened
   everywhere (a recession, a national trend) shows up in both groups and cancels out. What's left —
   the *extra* change in Texas — is the reform's effect. This is "difference-in-differences."
2. **Compare like with like.** We never compare a Texas station to a random other station. We
   compare it to a station **with the same kind of tank setup, in the same year**. (Technically, a
   "portfolio-type × year" fixed effect.) So the comparison is: two stations that look the same, same
   year, one in Texas and one not.
3. **Follow every station forward in time.** If a station shuts down, we see it shut down — we don't
   drop it. So "exit" is measured, not hidden.
4. **The event study** plots the Texas-minus-others gap **year by year**. If that gap is flat and near
   zero in the years *before* 1998 and then jumps *after*, the reform is the cause (there was no
   pre-existing trend pulling them apart). A flat "before" is the key honesty check.
5. We measure **many actions** a station can take, one at a time: close a tank, shut down entirely,
   shrink, consolidate, upgrade, replace tanks, lose capacity.

**Caveat to state once:** only Texas is treated, so the standard errors are a fast first pass
(clustered by state). The strongest results (closing, shutting down, and the size/age patterns) are
large; borderline ones need the wild-cluster bootstrap before a final table.

---

## 2. Headline effects of the reform (Portfolio ATT) — `T_Facility_Portfolio_ATT.csv`

Each number is the reform's average effect on that action (Texas vs. the comparison states).

- **Closed at least one tank (closure share): +0.0162 (p = .003).** The reform raised the share of a
  station's tanks closed each year by about 1.6 percentage points — for every 1,000 tanks, ~16 more
  closed per year than would have without the reform.
- **Closed any tank (any-closure): +0.0183 (p = .001).** A station was about 1.8 pp more likely to
  close at least one tank in a year.
- **Shut down completely (facility exit): +0.0125 (p = .014).** A station was about 1.25 pp more
  likely to close its last tank and stop operating.
- **Shrank — fewer tanks and less capacity (downsize): +0.0026 (p = .008).** A small but real rise:
  ~2.6 more stations per 1,000 genuinely contracted each year.
- **Consolidated — fewer tanks, same capacity (consolidate): −0.0004 (p = .24).** No effect. The
  reform did **not** push stations to drop tanks while keeping capacity. (Report as an informative null.)
- **Upgraded — fewer but bigger tanks (reconfigure-up): +0.0016 (p < .001).** A small rise: ~1.6 more
  per 1,000 traded several tanks for fewer, larger ones.
- **Permanently removed tanks (perm share): +0.0133 (p = .008).** The share of tanks removed and not
  replaced rose ~1.3 pp.
- **Replaced tanks (replacement share): +0.0029 (p < .001).** The share of tanks swapped for new ones
  rose ~0.3 pp.
- **Total capacity fell (cap-decrease): +0.0148 (p = .004).** A station was ~1.5 pp more likely to end
  the year with less total fuel storage.
- **Robustness (imputation route): closure +0.0118 (p = .006).** A second method that nets out tank-mix
  gives the same sign and a slightly smaller closing effect — the result is not an artifact of the
  comparison group.

**One-line takeaway:** the reform mainly made stations **close tanks and shut down**; among stations
that stayed open it pushed **genuine shrinking, replacing, and upgrading — but not capacity-preserving
consolidation.**

---

## 3. Who responds, by station size — `T_Facility_SizeHTE_byMargin.csv`

Size = total tank capacity. G1 = smallest (<9k gal, the comparison group), G4 = largest (30k+ gal).
Read each as: smallest-station effect, then how the largest differ. (Total for a bin = G1 row + that bin's row.)

- **Closing tanks:** smallest +0.0632; largest about +0.007 total (differential −0.0561, p < .001).
  Small stations close far more tanks; the largest barely move. **Closing is a small-station response.**
- **Shutting down:** smallest +0.0658; largest ≈ 0 (differential −0.0677, p < .001). **The smallest
  stations are ~6.6 pp more likely to shut down; the largest essentially never do because of the reform.**
- **Shrinking (downsize):** smallest −0.0050; largest +0.0101 total (differential +0.0151, p < .001).
  **Big stations shrink (shed capacity); small ones don't — they exit instead.**
- **Consolidating:** flat and tiny across all sizes (largest differential −0.0005). **No size pattern —
  consolidation isn't a response for anyone.**
- **Upgrading (reconfigure-up):** smallest +0.0020 (p < .001); fades to ~0 for the largest.
  **Upgrading to fewer, bigger tanks is concentrated in the smallest stations.**
- **Replacing tanks:** smallest −0.0014; largest +0.0070 total (differential +0.0084, p < .001).
  **Big stations replace tanks more; small ones replace less.**
- **Capacity falling:** smallest +0.0614; largest ≈ +0.008 total. **Capacity loss is concentrated at
  small stations** (mirrors closing).

**Takeaway:** **small stations exit; large stations adjust (shrink + replace) and survive.**

---

## 4. Who responds, by station age at the reform — `T_Facility_VintageHTE_byMargin.csv`

Age = how old the station's tanks were in 1998. Comparison group = newest (1989–98). Older cohorts:
1985–88, 1975–84, Pre-1975.

- **Closing tanks:** newest +0.0084; all ages similar (~+0.008 to +0.009). **Closing rose about the
  same regardless of age.**
- **Shutting down:** newest ≈ 0 (−0.0018, n.s.); oldest (Pre-1975) +0.0248 total (differential +0.0266,
  p < .001). **The oldest stations are ~2.5 pp more likely to shut down; the newest essentially don't.**
- **Shrinking (downsize):** newest +0.0053; oldest −0.0059 total (differential −0.0112, p < .001).
  **Newer stations shrink (adjust capacity); the oldest don't shrink — they exit.**
- **Consolidating:** tiny everywhere (newest +0.0006; oldest −0.0027). **No meaningful age pattern.**
- **Upgrading:** newest +0.0043 (p < .001); oldest −0.0022 total. **Newer stations upgrade to fewer,
  bigger tanks; the oldest don't.**
- **Replacing tanks:** newest +0.0069 (p < .001); oldest −0.0035 total. **Newer stations replace tanks;
  the oldest replace less.**
- **Capacity falling:** newest +0.0045 (n.s.); older cohorts ~+0.011. **Capacity loss is larger at
  older stations.**

**Takeaway:** **old stations exit; young stations reinvest (shrink, replace, upgrade).**

---

## 5. Who responds, by fuel and location — `T_Facility_HTE_byMargin.csv`

Read as: baseline group effect (`did_term`), then the extra effect for the named group (`did_Z`).

**Gas stations vs. non-gas (the fuel result):**
- **Closing:** non-gas +0.0051; gas stations **+0.0182 more** (p < .001) → total ~+0.0233. **Gas
  stations drive the closures.**
- **Shutting down:** gas stations +0.0069 more (p < .001). **Gas stations exit more.**
- **Shrinking:** gas stations +0.0088 more (p < .001). **Gas stations do the shrinking.**
- **Replacing:** gas stations +0.0089 more (p < .001). **Gas stations do the replacing.**
- **Consolidating:** gas +0.0023 (tiny). **Still negligible.**
- *Every portfolio action is concentrated in gas stations.*

**Rural and thin-market (few competitors) — these stay open:**
- **Shutting down:** rural −0.0042 (p < .001); thin-market **−0.0164** (p < .001). **Rural and
  low-competition stations are markedly *less* likely to shut down — captive demand keeps them open.**
- **Closing:** rural −0.0041; thin-market −0.0128 (both p < .001). **They also close fewer tanks.**

**Low-income and high-poverty neighborhoods — these exit but don't reinvest:**
- **Shutting down:** low-income +0.0022, high-poverty +0.0018 (both p < .001). **Stations in poorer
  areas are *more* likely to shut down.**
- **Shrinking:** low-income −0.0038, high-poverty −0.0035 (both p < .001). **and they shrink *less*.**
- **Replacing:** low-income −0.0032, high-poverty −0.0026 (both p < .001). **and replace *less*.**
- *Read together: poorer areas see permanent exit with no reinvestment.*

**Low population density:** small effects in the same direction as rural (closes/exits slightly less).

**Takeaway:** **gas stations drive every action; rural / low-competition stations stay open; poor
neighborhoods lose stations for good (exit up, reinvestment down).**

---

## 6. Tank-level fuel result — `T_HTE_GIS_FirstPass.csv` (02g)

- **Any-gasoline station:** non-gas baseline +0.0197; difference for gasoline-present **−0.0051
  (p = .22, not significant).** **Simply *having* a gasoline tank does not separate the response.**
- **Pure-gasoline station (all tanks gasoline):** mixed-fuel baseline +0.0068; difference **+0.0431
  (p < .001).** **Pure-gasoline tanks close about 4.3 pp more than mixed-fuel ones** — the response is
  concentrated in the cleanest motor-fuel-retail cell, where the insurance price gradient is steepest.

---

## 7. Event studies — what each one says (causal status)

The event study is the picture behind the average. Flat *before* 1998 = trustworthy; a jump *after* =
the reform.

- **Closing (`Fig_ES_Facility_Portfolio`): CAUSAL.** Flat and at zero before 1998, then a sharp jump up
  that stays positive for two decades. This is the clean, credible result — the anchor.
- **Pooled tank closing (`Fig_ES_HTE_Pooled`): CAUSAL (headline tank version).** Clear jump at the
  reform; the "before" is only mildly elevated and roughly flat.
- **Shrinking (`Fig_ES_Facility_Downsize`): DESCRIPTIVE.** Positive and steady after, but the "before"
  is not perfectly flat (a couple of years sit above zero) — present as suggestive, not causal.
- **Consolidating (`Fig_ES_Facility_Consolidate`): NULL.** Scatters around zero before and after — no
  effect, consistent with the average.
- **Upgrading (`Fig_ES_Facility_ReconfigureUp`): DESCRIPTIVE.** The series drifts upward *through* the
  reform — the rise started before 1998 — so the post-rise partly continues a pre-existing trend.
- **Replacing (`Fig_ES_Facility_Replace`): MODERATE / borderline.** Clearly higher after, but the
  "before" has some positive years — needs the bootstrap before any causal claim.

**One honest sentence for the paper:** the *extensive* margin — closing tanks and shutting down — is
causal; the *portfolio-adjustment* margins (shrink, upgrade, replace) are suggestive sorting, and
consolidation shows no effect.
