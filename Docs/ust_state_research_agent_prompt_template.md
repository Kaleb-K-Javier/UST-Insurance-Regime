# UST State Regulation Research Prompt Template

## Instructions for Use
Replace `[STATE]` with the target state name. Copy this entire prompt to a research agent (Claude, etc.) to generate comprehensive state-specific UST regulatory documentation.

---

# BEGIN PROMPT

You are conducting academic regulatory research for a PhD economics dissertation studying underground storage tank (UST) environmental insurance and policy. Your task is to comprehensively document all UST regulations, programs, and requirements for **[STATE]**.

## DELIVERABLES REQUIRED

### Deliverable 1: State Research Report (Markdown)
Create a detailed markdown document titled `[STATE]_UST_Regulatory_Research.md` containing:

1. **Executive Summary** - Key findings, unique features, critical dates
2. **EPA Program Approval Status** - Approval date, Federal Register citation (volume, page, date), or note if no approval
3. **Implementing Agency** - Current agency, historical predecessors, contact information
4. **Primary Legal Authority** - Statutes and administrative codes with specific section numbers
5. **Regulation Categories** (document each that applies):
   - 1998 Federal Baseline Implementation
   - 2015 Federal Update Adoption
   - Phased/Stratified Implementation Schedules (if any)
   - Forced Closure - Age-Based
   - Forced Closure - Construction-Based
   - Forced Upgrade - Technology Addition
   - New Installation Standards (Double-Wall, Spill/Overfill)
   - Release Detection Requirements/Restrictions
   - Operator Training Requirements
   - Inspection Requirements
   - Financial Assurance/State Fund Programs
   - Registration/Permitting Requirements
   - Geographic Restrictions (Wellhead Protection, Schools, Sensitive Areas)
6. **Penalties and Enforcement** - Civil, criminal, administrative penalties with amounts and citations
7. **Financial Programs** - Cleanup funds, loan/grant programs, eligibility requirements, coverage amounts
8. **Key Dates Timeline** - Chronological list of all regulatory effective dates
9. **Sources and Citations** - Complete bibliography with URLs and access dates

### Deliverable 2: Database Rows (Excel)
Create `[STATE]_database_rows.xlsx` with rows formatted to paste into the master database. Include sheets:

**Sheet 1: "Regulations"** - One row per regulation with columns:
- State
- Regulation Type
- Affected Tanks/Firms Criteria
- Effective Compliance Date
- Non-Compliance Result
- Primary Legal Citation
- EPA Approval Status
- Verification Status
- Notes

**Sheet 2: "National_Inventory_Row"** - Single row for national inventory update:
- State
- Research Status
- EPA Approval Status
- Regulations Documented
- Notes

---

## RESEARCH METHODOLOGY

### Step 1: Identify Primary Sources
Search for and access:
1. **State Environmental Agency UST Program Page** - Usually DEQ, DEP, or equivalent
2. **State Administrative Code** - Find the UST chapter/title
3. **State Statutes** - Enabling legislation for UST program
4. **EPA Region Documents** - State program approval notices
5. **Federal Register** - Search for "[STATE] underground storage tank program approval"

### Step 2: Document EPA Approval Status
Search: `"[STATE]" "underground storage tank" "program approval" site:federalregister.gov`

Find:
- Approval date (effective date, not publication date)
- Federal Register citation: [Volume] FR [Page]-[Page] ([Date])
- 40 CFR 282 section (if codified)
- If NO approval: document as "Federal Direct Implementation"

### Step 3: Document 1998 Federal Baseline Implementation
Find how [STATE] adopted the December 22, 1998 upgrade deadline:
- Did state adopt verbatim or have state-specific dates?
- **CRITICAL: Check for PHASED/STRATIFIED implementation** - Some states (like Texas) required older tanks to comply earlier:
  ```
  Example (Texas 30 TAC § 334.44):
  Pre-1964 tanks → 1989-12-22
  1965-1969 tanks → 1990-12-22
  1970-1974 tanks → 1991-12-22
  1975-1979 tanks → 1992-12-22
  1980-1988 tanks → 1993-12-22
  All remaining → 1998-12-22
  ```
- Document compliance options (cathodic protection, lining, closure)
- Document enforcement mechanisms (delivery prohibition, penalties)

### Step 4: Document 2015 Federal Update Adoption
Search: `"[STATE]" UST "operator training" OR "2015" OR "walkthrough inspection"`

Find:
- When state adopted 2015 federal requirements
- Operator training (Class A/B/C or state-specific designations)
- Walkthrough inspection requirements and frequency
- Secondary containment requirements for new/replaced equipment
- Whether state had earlier/more stringent requirements

### Step 5: Search for Forced Closure/Removal Mandates
Search terms:
```
"[STATE]" underground storage tank removal deadline
"[STATE]" UST phase out
"[STATE]" single wall tank prohibition
"[STATE]" tank closure mandate
"[STATE]" UST "must be closed" OR "shall be removed"
```

Document:
- **Age-based closures**: Tanks over X years must close (like Maine pre-1985 tanks)
- **Construction-based closures**: Single-wall, bare steel prohibitions (like Massachusetts 2017, California 2025)
- **Technology-based upgrades**: Lined tanks must add CP or close (like Kentucky 2013)

### Step 6: Search for Enhanced Technical Standards
Search terms:
```
"[STATE]" secondary containment UST requirement
"[STATE]" double wall tank requirement
"[STATE]" UST overfill prevention
"[STATE]" release detection method prohibited
"[STATE]" interstitial monitoring required
```

Document if state is MORE STRINGENT than federal:
- Federal allows either 90% alarm OR 95% shutoff; does state require BOTH?
- Does state prohibit specific release detection methods (vapor monitoring, SIR)?
- Does state require secondary containment earlier than federal October 2015?
- Does state prohibit ball float valves?

### Step 7: Search for Geographic Restrictions
Search terms:
```
"[STATE]" UST wellhead protection zone
"[STATE]" underground storage tank near schools
"[STATE]" UST source water protection
"[STATE]" UST aquifer recharge zone
"[STATE]" UST sensitive area requirement
```

Document:
- Enhanced requirements in drinking water protection zones
- Restrictions near schools, daycare, hospitals
- Aquifer recharge zone requirements (like Texas Edwards Aquifer)
- Distance-based restrictions or prohibitions

### Step 8: Document Financial Assurance Programs

**CRITICAL FOR DISSERTATION RESEARCH:** This section documents state fund programs AND whether states transitioned from public fund coverage to mandatory private insurance markets. This transition is a key policy variation for economic analysis.

#### 8A: Current/Historical State Fund Programs
Search terms:
```
"[STATE]" UST cleanup fund
"[STATE]" petroleum storage tank fund
"[STATE]" LUST trust fund
"[STATE]" UST financial responsibility
"[STATE]" tank removal grant OR loan
"[STATE]" petroleum storage tank environmental assurance fund
```

Document:
- Fund name and statutory authority
- Fund establishment date
- Funding mechanism (per-gallon fees, per-tank fees, general revenue)
- Coverage amounts (per occurrence, per site, annual aggregate)
- Deductibles and eligibility requirements
- Loan/grant programs for tank removal or upgrade (like California RUST)
- Connection between compliance and fund eligibility

#### 8B: EPA Approval as Financial Responsibility Mechanism
Search terms:
```
"[STATE]" state fund "financial responsibility" EPA approved
"[STATE]" UST fund "40 CFR 280" mechanism
"[STATE]" petroleum fund federal financial assurance
site:federalregister.gov "[STATE]" "state fund" "financial responsibility"
```

Document:
- Whether state fund was/is EPA-approved as valid FR mechanism under 40 CFR 280.97
- Date of EPA approval (Federal Register citation)
- Current status: still valid or withdrawn/expired

#### 8C: STATE FUND CLOSURE / TRANSITION TO PRIVATE INSURANCE
**This is economically critical.** A handful of states have discontinued their state fund as a valid federal FR mechanism, forcing tank owners to obtain private insurance.

Search terms:
```
"[STATE]" state fund "no longer" financial responsibility
"[STATE]" UST fund closed OR expired OR sunset
"[STATE]" petroleum fund transition private insurance
"[STATE]" UST "must obtain" insurance
"[STATE]" state fund "not acceptable" financial assurance
"[STATE]" tank fund reimbursement ended OR discontinued
"[STATE]" UST fund phase out
```

Document with EXACT DATES:
1. **Fund Closure/Sunset Date**: When did state fund stop accepting new claims or close entirely?
2. **FR Mechanism Withdrawal Date**: When did state fund stop being acceptable as federal financial responsibility demonstration? (This is the CRITICAL transition date)
3. **Private Insurance Mandate Date**: When were tank owners REQUIRED to obtain private insurance?
4. **Transition Legislation**: What bill/statute authorized the transition?
5. **Transition Timeline**: Was there a phase-in period?

**Known Examples to Reference:**
- **Texas (1998-12-22)**: PSTR Fund stopped being acceptable FR mechanism; owners must obtain private insurance per 30 TAC Chapter 37
- **Washington State**: Executed major structural transition (per prior research)
- **North Carolina**: Implemented partial phase-out

**Key Questions to Answer:**
- Does the state fund CURRENTLY serve as valid federal FR mechanism?
- If NO: When did it stop? What replaced it?
- If YES: Are there signs of upcoming transition (fund insolvency, legislative proposals)?
- What coverage amounts does private insurance require? (typically $500K-$1M per occurrence, $1M-$2M aggregate)

#### 8D: Private Insurance Market Structure (if applicable)
If state transitioned to private insurance requirement, document:
```
"[STATE]" UST insurance requirement
"[STATE]" petroleum liability insurance
"[STATE]" underground storage tank "pollution liability"
"[STATE]" UST coverage amounts required
```

Document:
- Required coverage amounts (per occurrence, annual aggregate)
- Acceptable insurance mechanisms (insurance policy, surety bond, letter of credit, etc.)
- Whether state-specific policy language required (like Texas 30 TAC Chapter 37)
- Major insurers in market (if identifiable)
- Premium/rate information (if available through rate filings)

### Step 9: Document Registration and Permitting
Find:
- Registration requirements and fees
- Permit requirements and renewal frequency
- Delivery prohibition/certificate mechanisms ("red tag" vs "green tag")
- Database systems (state tank registry names)

### Step 10: Document Penalties and Enforcement
Find specific penalty amounts:
- Civil penalties: $ per violation, per day, per tank
- Criminal penalties: fines and imprisonment
- Administrative penalties
- Enforcement procedures and timelines

---

## OUTPUT FORMAT SPECIFICATIONS

### Markdown Document Structure
```markdown
# [STATE] Underground Storage Tank Regulations
## Comprehensive Regulatory Research Report

**Research Date:** [DATE]
**Researcher:** [AI Agent]
**Version:** 1.0

---

## Executive Summary

[2-3 paragraph overview of key findings, unique features, critical compliance dates]

---

## 1. EPA Program Approval Status

| Field | Value |
|-------|-------|
| EPA Approval Status | [Approved/Not Approved/Federal Direct Implementation] |
| Approval Effective Date | [YYYY-MM-DD] |
| Federal Register Citation | [Vol] FR [Pages] ([Date]) |
| 40 CFR 282 Section | [Section number if applicable] |

[Additional context on approval history, any revisions]

---

## 2. Implementing Agency

**Current Agency:** [Name]
**Division/Bureau:** [Specific unit]
**Statutory Authority:** [Citation]

**Historical Predecessors:**
- [Agency] ([Years])
- [Agency] ([Years])

---

## 3. Primary Legal Authority

### Statutes
| Citation | Title | Key Provisions |
|----------|-------|----------------|
| [State Code §] | [Title] | [Brief description] |

### Administrative Code
| Citation | Title | Key Provisions |
|----------|-------|----------------|
| [Admin Code §] | [Title] | [Brief description] |

---

## 4. 1998 Federal Baseline Implementation

### Compliance Deadline
[Describe how state adopted the December 22, 1998 deadline]

### Phased Implementation Schedule (if applicable)
| Tank Installation Date | Compliance Deadline |
|------------------------|---------------------|
| [Date range] | [YYYY-MM-DD] |

### Tank Definition
[How state defines "existing tank" subject to upgrade requirements]

### Compliance Options
1. [Option with citation]
2. [Option with citation]

### Enforcement Mechanisms
[Delivery prohibition, penalties, etc.]

---

## 5. 2015 Federal Update Implementation

[Details on operator training, walkthrough inspections, secondary containment adoption]

---

## 6. State-Specific Requirements Beyond Federal Baseline

### [Requirement Category]
| Field | Value |
|-------|-------|
| Regulation Type | [Category] |
| Affected Tanks | [Criteria] |
| Effective Date | [YYYY-MM-DD] |
| Legal Citation | [Citation] |
| Non-Compliance Result | [Penalties/consequences] |

[Repeat for each state-specific requirement]

---

## 7. Financial Assurance Programs

### 7A. State Fund (if applicable)
| Field | Value |
|-------|-------|
| Fund Name | [Name] |
| Statutory Authority | [Citation] |
| Established | [YYYY-MM-DD] |
| Funding Source | [Description] |
| Coverage Amount | [Per occurrence/aggregate] |
| Deductible | [Amount by class] |
| Eligibility Requirements | [Requirements] |
| EPA-Approved as FR Mechanism | [Yes/No] |
| FR Approval Date | [YYYY-MM-DD or N/A] |
| FR Approval FR Citation | [Vol FR Pages (Date)] |

### 7B. State Fund Transition to Private Market (CRITICAL)
| Field | Value |
|-------|-------|
| Did Transition Occur? | [Yes/No/Partial] |
| State Fund FR Mechanism End Date | [YYYY-MM-DD - when fund stopped being acceptable for federal FR] |
| Fund Closure/Sunset Date | [YYYY-MM-DD - when fund stopped operations entirely] |
| Private Insurance Mandate Effective | [YYYY-MM-DD] |
| Transition Legislation | [Bill number, statute citation] |
| Transition Reason | [Insolvency/Legislative decision/Other] |

**If transition occurred, document timeline:**
| Date | Event | Citation |
|------|-------|----------|
| [Date] | [Key milestone in transition] | [Citation] |

### 7C. Private Insurance Requirements (if applicable)
| Field | Value |
|-------|-------|
| Per Occurrence Coverage | [$Amount] |
| Annual Aggregate Coverage | [$Amount] |
| Acceptable Mechanisms | [Insurance, surety bond, letter of credit, etc.] |
| State-Specific Policy Language Required | [Yes/No - citation if yes] |
| Legal Citation | [Citation] |

### 7D. Loan/Grant Programs
[Details on any tank removal or upgrade incentive programs]

---

## 8. Penalties and Enforcement

### Civil Penalties
| Violation Type | Penalty Range | Citation |
|----------------|---------------|----------|
| [Type] | $[Amount] per [day/violation/tank] | [Citation] |

### Criminal Penalties
[Details with citations]

### Enforcement Procedures
[Process description]

---

## 9. Key Dates Timeline

| Date | Event | Citation |
|------|-------|----------|
| [YYYY-MM-DD] | [Event description] | [Citation] |

---

## 10. Sources and Citations

### Primary Sources
1. [Citation with URL and access date]

### Secondary Sources
1. [Citation with URL and access date]

---

## Verification Notes

[Items requiring further verification, limitations, caveats]
```

### Excel Database Rows Format

**Regulations Sheet Columns (in order):**
1. State
2. Regulation Type (use standard categories)
3. Affected Tanks/Firms Criteria
4. Effective Compliance Date (YYYY-MM-DD or "N/A - Ongoing requirement")
5. Non-Compliance Result
6. Primary Legal Citation
7. EPA Approval Status
8. Verification Status (Verified - Primary Source / Verified - Secondary Source / Cited - Not Yet Verified / Requires Verification)
9. Notes

**Standard Regulation Type Categories:**
- Upgrade Mandate - 1998 Federal Baseline
- Upgrade Mandate - 1998 Phased Implementation
- Upgrade Mandate - 2015 Federal Update
- Forced Closure - Age-Based
- Forced Closure - Construction-Based
- Forced Upgrade - Technology Addition
- New Installation Standard - Double Wall
- New Installation Standard - Spill/Overfill
- Release Detection Restriction
- Operator Training Requirement
- Inspection Requirement
- Financial Assurance - State Fund (Active)
- Financial Assurance - State Fund Transition to Private Market
- Financial Assurance - Private Insurance Requirement
- Financial Assurance - Loan/Grant Program
- Registration/Permitting
- Geographic Restriction - Wellhead Protection
- Geographic Restriction - School/Daycare Proximity
- Geographic Restriction - Aquifer Protection
- Geographic Restriction - Sensitive Areas

---

## QUALITY CHECKLIST

Before finalizing deliverables, verify:

- [ ] EPA approval status documented with Federal Register citation
- [ ] All dates in YYYY-MM-DD format
- [ ] All legal citations include specific section numbers (not just chapter)
- [ ] Penalty amounts documented with statutory citations
- [ ] Phased implementation schedule checked (don't assume single 1998 deadline)
- [ ] 2015 federal update adoption date documented
- [ ] **Financial assurance transition checked** - Did state fund stop being FR mechanism?
- [ ] **If transition occurred**: FR end date, private insurance mandate date, transition legislation documented
- [ ] State fund EPA approval status documented (is it valid FR mechanism?)
- [ ] Grant/loan programs documented with amounts and eligibility
- [ ] Geographic restrictions checked (wellhead, schools, aquifers)
- [ ] Each regulation row has non-compliance consequences
- [ ] Verification status accurately reflects confidence level
- [ ] All sources cited with URLs and access dates

---

## IMPORTANT NOTES

1. **Academic Publication Standards Apply** - All citations must be verifiable
2. **Temporal Precision Required** - When did each requirement become effective?
3. **Look for State-Specific Innovations** - Don't assume state just adopted federal baseline
4. **Check for Phased Implementation** - Multiple compliance deadlines by tank age/type
5. **CRITICAL: Financial Assurance Transitions** - The transition from public state fund to private insurance market is a key policy variation. Document exact dates when state funds stopped being valid federal FR mechanisms. Known transitions: Texas (1998-12-22), Washington State, North Carolina (partial).
6. **Document What You Cannot Find** - Note gaps for follow-up research
7. **Primary Sources Preferred** - State administrative codes > agency websites > news articles

# END PROMPT
