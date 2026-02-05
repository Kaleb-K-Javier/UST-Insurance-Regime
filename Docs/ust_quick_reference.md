# Quick Reference: UST Research Version 2 - Key Changes

## 🎯 SUPERVISOR GUIDANCE - KEY DECISIONS

✅ **Include 2015 federal updates** (operator training, inspections) - YES  
✅ **Include geographic restrictions** (near schools, water sources) - YES  
✅ **Include financial assurance/fund details** - YES, FR rules are important  
✅ **Document current regulations if can't find 1998 versions** - YES, with caveats  
✅ **Expand beyond 20 states** - NOT NOW, but create complete 50-state inventory table  

**Goal:** Understand the FULL state of UST regulation across the nation over time

---

## 🎯 PRIMARY OBJECTIVES

1. **Transform the data structure** from single-deadline table to multi-regulation observations
2. **Add command-and-control regulations** beyond the 1998 baseline (forced closures, double-wall mandates, etc.)
3. **Verify and fix** existing research gaps
4. **Complete research** for Texas and California

---

## 📊 NEW TABLE FORMAT AT A GLANCE

**OLD:**
```
State | Installation Cutoff | Compliance Deadline | Additional Phase-Out Dates
```

**NEW:**
```
State | Regulation Type | Affected Tanks/Firms Criteria | Effective Compliance Date | Non-Compliance Result | Primary Legal Citation | EPA Approval Status | Verification Status | Notes
```

**Key Change:** Each state now has MULTIPLE ROWS - one for each type of regulation

---

## 🔍 WHAT TO SEARCH FOR (12 Categories)

### High Priority (What supervisor specifically mentioned):
1. ✅ **Forced Removal Dates** - Like Massachusetts 2017, Maine 2008, Kentucky 2013
2. ✅ **Double-Wall Requirements** - When states required all NEW tanks to be double-walled
3. ✅ **California** - Supervisor mentioned but not researched yet
4. ✅ **Geographic Restrictions** - Near schools, water sources, wellhead protection
5. ✅ **Financial Assurance/Fund Details** - FR rules are important per supervisor

### Also Important:
6. **2015 Federal Requirements** - Operator training, walkthrough inspections (include these!)
7. Enhanced spill/overfill requirements (like Oklahoma's both/and requirement)
8. Release detection restrictions
9. Inspection requirements beyond federal baseline
10. Registration/permitting fees
11. Delivery prohibition mechanisms
12. Other innovative state regulations

---

## ⚠️ CRITICAL FIXES (Do These First)

### Fix #1: TEXAS (1-2 days)
**Problem:** Included in analysis but not fully researched  
**Action:** Complete full Texas research
- Find EPA approval status
- Document 1998 deadline adoption
- Find any Texas-specific requirements

### Fix #2: Verify Additional Deadlines (1 day)
**Problem:** Important dates cited but not independently verified  
**Action:** Access primary sources and confirm:
- ✅ Massachusetts 310 CMR 80.15 (Aug 7, 2017 single-wall removal)
- ✅ Maine 38 M.R.S.A. § 564(5) (Jan 1, 2008 pre-1985 tanks)
- ✅ Kentucky 401 KAR 42:030 (Dec 22, 2013 lined tanks)
- ✅ Kentucky 401 KAR 42:030 (Jan 1, 2012 unprotected steel)

### Fix #3: Federal Register Citations (1 day)
**Problem:** Some citations missing page numbers  
**Action:** Access Federal Register and complete:
- Kansas: 59 FR ___ (1994)
- Pennsylvania: 68 FR ___ (September 11, 2003)
- Verify all other dates

### Fix #4: Temporal Accuracy (2-3 days)
**Problem:** Current regulations cited, not 1998 versions  
**Action:** Verify which requirements existed in 1998 vs. added later
- Focus on: Oklahoma, Massachusetts, New Mexico regulations
- Note in table which requirements are post-1998

---

## 📋 EXAMPLE OF WHAT THE NEW DATA LOOKS LIKE

**Instead of this (old format):**
```
Massachusetts | ≤ Dec 22, 1988 | Dec 22, 1998 | Aug 7, 2017: all single-wall steel tanks must close
```

**You'll have this (new format):**

```
Row 1:
State: Massachusetts
Regulation Type: Upgrade Mandate - 1998 Federal Baseline  
Affected Tanks: All UST facilities with tanks installed on or before Dec 22 1988
Effective Compliance Date: 1998-12-22
Non-Compliance Result: Civil penalty $100-$25,000 per violation; delivery prohibition
Primary Legal Citation: 310 CMR 80.00
EPA Approval Status: State Program Approved - 1995-04-17
Verification Status: Cited - Not Yet Verified

Row 2:
State: Massachusetts
Regulation Type: Forced Closure - Construction-Based
Affected Tanks: All facilities with single-wall steel USTs (any installation date)
Effective Compliance Date: 2017-08-07
Non-Compliance Result: Tank must be permanently closed/removed; delivery prohibition; civil penalties
Primary Legal Citation: 310 CMR 80.15
EPA Approval Status: State Program Approved - 1995-04-17
Verification Status: Requires Verification

Row 3:
State: Massachusetts
Regulation Type: Inspection Requirement
Affected Tanks: All UST facilities
Effective Compliance Date: N/A - Every 3 years
Non-Compliance Result: Non-compliance subject to enforcement; delivery prohibition
Primary Legal Citation: 310 CMR 80.00
EPA Approval Status: State Program Approved - 1995-04-17
Verification Status: Cited - Not Yet Verified
```

---

## 🔎 SEARCH STRATEGY

For each state, run these searches:

**Set A: Forced Closures**
```
"[state] underground storage tank removal deadline"
"[state] UST phase out"  
"[state] single wall tank prohibition"
```

**Set B: Double-Wall**
```
"[state] secondary containment UST"
"[state] double wall tank requirement"
```

**Set C: Enhanced Standards**
```
"[state] UST overfill prevention"
"[state] UST release detection"
```

**Set D: State Agency Website**
- Go directly to state DEQ/DEP website
- Look for UST program page
- Check for fact sheets, guidance, compliance timelines

---

## 📚 VERIFICATION LEVELS

Mark each regulation with one of these:

1. **Verified - Primary Source** → You accessed the actual law/regulation
2. **Verified - Secondary Source** → Confirmed from EPA documents/state guidance
3. **Cited - Not Yet Verified** → From existing research, but you haven't independently confirmed
4. **Requires Verification** → Needs to be checked before submission

---

## 🎯 DELIVERABLES

### 1. Master Spreadsheet
**File:** `ust_state_regulations_enhanced.xlsx`
- Multiple rows per state
- All 9 columns populated
- Includes verification status

### 2. National Inventory Table (NEW REQUIREMENT)
**File:** Separate sheet in Excel OR `national_state_inventory.csv`
- All 50 states + DC listed
- Shows research status (Completed/In Progress/Not Yet Researched)
- EPA approval status documented
- Regulation count for completed states
- Framework for future expansion

### 3. Methodology Doc
**File:** `ust_research_methodology_v2.md`
- How you searched
- What sources you used
- Verification process

### 3. Findings Memo
**File:** `ust_findings_memo_v2.md`
- What changed from Version 1?
- What new regulations discovered?
- Which states have most regulations?
- What still needs work?

---

## ⏰ SUGGESTED TIMELINE

**Week 1:** Critical fixes (Texas, verification, citations, temporal accuracy)  
**Week 2:** California + systematic state searches  
**Week 3:** Verification + documentation  
**Week 4:** Buffer for difficult states

---

## 💡 KEY INSIGHTS FOR YOUR RESEARCH

1. **Not all regulations are equal** - Some applied in 1998, some added later
2. **Verification matters** - Supervisor needs publication-quality accuracy
3. **Multiple regulation types exist** - Don't just look for one type
4. **States vary significantly** - Some have many regulations, some have few
5. **Temporal dimension is critical** - When did requirements become effective?

---

## ❓ QUESTIONS - ANSWERED BY SUPERVISOR

~~Before starting, clarify:~~
~~1. Include post-1998 regulations (like 2015 federal update)?~~
~~2. Any other states beyond current 20?~~
~~3. Include geographic restrictions (near schools/water)?~~
~~4. What if historical 1998 regulations unavailable?~~
~~5. Include financial assurance or focus only on technical requirements?~~

**ALL QUESTIONS ANSWERED - See supervisor guidance at top of document**
- ✅ Yes to 2015 federal requirements
- ✅ Yes to geographic restrictions  
- ✅ Yes to financial assurance/fund details
- ✅ Document current regs with caveats if can't find 1998 versions
- ✅ Create 50-state inventory but focus on completing current 20 states

---

## 📞 WHEN TO ASK FOR HELP

Contact supervisor if:
- Can't access state regulations after reasonable effort
- Find conflicting information in sources
- Unclear if a regulation fits categories
- State has very unusual/unique structure (like Ohio)
- Need help interpreting legal language

---

## ✅ QUALITY CHECKLIST

Before submitting:
- [ ] Every state has at least one row (1998 baseline)
- [ ] Texas fully researched
- [ ] California fully researched  
- [ ] All "Requires Verification" items addressed
- [ ] All Federal Register citations complete
- [ ] Temporal accuracy verified (1998 vs. later)
- [ ] All non-compliance results documented
- [ ] Verification status accurate
- [ ] All citations have specific sections
- [ ] Excel file has all required sheets

---

## 🎓 REMEMBER

This research will be used in an **academic publication**. Accuracy and verifiability are more important than speed. When in doubt:

1. ✅ Verify rather than assume
2. ✅ Document your process
3. ✅ Flag items needing more research  
4. ✅ Distinguish confirmed vs. cited info

Your initial work was strong - this is about taking it from good to excellent.
