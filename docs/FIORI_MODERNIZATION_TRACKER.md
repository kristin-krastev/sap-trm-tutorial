# Fiori Modernization Tracker - Cluster C: Analytics & Oversight

**Sprint:** Dec 15, 2025 - Jan 5, 2026  
**Last Updated:** December 15, 2025

---

## Apps in Scope

| App Code | App Name | Status | Priority |
|----------|----------|--------|----------|
| F5654 | Commodity Hedge Management Cockpit - Analytical View | 🔴 Not Started | High |
| F5656 | Commodity Hedge Management Cockpit - Overview | 🔴 Not Started | High |
| F5657 | Retrieve Hedge Constellation Details | 🔴 Not Started | High |
| F6003 | Monitor Overhedged Exposures | 🔴 Not Started | High |

---

## Issues Sorted by Difficulty (Easiest First)

Following proven approach from Trade Order Cockpit sprint:
- Start with **Issue 5** (easiest)
- Then **Issue 2**
- Progress through remaining issues by difficulty

---

## Issue 5: Standard SAP Fields (Company Code, Currency, etc.) ⭐ EASIEST

**Difficulty:** ⭐ Easy  
**Effort:** 1-2 hours per app  
**Risk:** Low  
**Pattern:** Well-established, standard associations

### What to Fix
Add text associations for standard SAP fields that have existing standard views:
- Company Code → Company Code Name
- Currency → Currency Name  
- Material → Material Description
- Plant → Plant Name
- Customer → Customer Name
- Country → Country Name

### Why It's Easy
✅ Standard associations already exist (e.g., `I_CompanyCode`, `I_Currency`)  
✅ Well-documented pattern  
✅ No custom logic needed  
✅ Low ATC risk  
✅ Quick testing

### Implementation Pattern
```abap
// CDS View
association [0..1] to I_CompanyCode as _CompanyCode 
  on $projection.CompanyCode = _CompanyCode.CompanyCode

{
  @ObjectModel.text.element: ['CompanyCodeName']
  key CompanyCode,
  
  @Semantics.text: true
  _CompanyCode.CompanyCodeName as CompanyCodeName,
}
```

### Apps to Check
- [ ] F5654 - Check for: CompanyCode, Currency, Material
- [ ] F5656 - Check for: CompanyCode, Currency, Plant
- [ ] F5657 - Check for: CompanyCode, Currency
- [ ] F6003 - Check for: CompanyCode, Currency

---

## Issue 2: Commodity-Specific ID Fields (DCS, Hedge Book) ⭐⭐ MEDIUM

**Difficulty:** ⭐⭐ Medium  
**Effort:** 2-3 hours per app  
**Risk:** Medium  
**Pattern:** Proven in Trade Order Cockpit

### What to Fix
Add text associations for commodity-specific fields:
- DCS (Derivative Contract Specification) → DCS Name
- Hedge Book → Hedge Book Description
- Delivery Period → Period Description
- Commodity → Commodity Description

### Why It's Medium
⚠️ Commodity-specific fields may have complex value helps  
⚠️ Text fields might not be in obvious places  
⚠️ May require exploring interface views  
✅ But pattern proven in Trade Order Cockpit

### Known Pattern from Trade Order Cockpit
```abap
// DCS Field
@ObjectModel.text.element: ['DerivativeContrSpecName']
key CmmdtyHedgePlanExposureDCSID,

@Semantics.text: true
DerivativeContrSpecName,

// Hedge Book Field  
@ObjectModel.text.element: ['CmmdtyHedgeBookDescription']
key CmmdtyHdgPlanExposureHedgeBook,

@Semantics.text: true
CmmdtyHedgeBookDescription,
```

### Apps to Check
- [ ] F5654 - DCS, Hedge Book likely present
- [ ] F5656 - DCS, Hedge Book likely present
- [ ] F5657 - Hedge Constellation fields
- [ ] F6003 - Exposure, Hedge fields

---

## Issue 3: Analytical Query groupBy and requestAtLeast ⭐⭐⭐ MEDIUM-HIGH

**Difficulty:** ⭐⭐⭐ Medium-High  
**Effort:** 1-2 hours per app  
**Risk:** Medium  
**Pattern:** Known from Trade Order Cockpit

### What to Fix
Ensure all text fields are in both:
1. `groupBy` array (makes them sortable dimensions)
2. `requestAtLeast` array (pre-loads them)

### Why It's Medium-High
⚠️ Must identify ALL text fields (don't miss any)  
⚠️ Order matters in some cases  
⚠️ Can impact performance if done incorrectly  
✅ But pattern is straightforward

### Implementation Pattern
```abap
@UI.presentationVariant: [{
  visualizations: [{ type: #AS_LINEITEM }],
  
  requestAtLeast: [ 
    'CompanyCode', 'CompanyCodeName',
    'CmmdtyHedgePlanExposureDCSID', 'DerivativeContrSpecName',
    'CmmdtyHdgPlanExposureHedgeBook', 'CmmdtyHedgeBookDescription',
    // ... all other ID and text fields
  ],
  
  groupBy: [ 
    'CompanyCode', 'CompanyCodeName',
    'CmmdtyHedgePlanExposureDCSID', 'DerivativeContrSpecName',
    'CmmdtyHdgPlanExposureHedgeBook', 'CmmdtyHedgeBookDescription',
    // ... all other ID and text fields
  ]
}]
```

### Known Issue
From Trade Order Cockpit: Text fields missing from `groupBy` caused "Sort by Name" option to not appear in column context menu.

### Apps to Check
- [ ] F5654 - Update presentationVariant
- [ ] F5656 - Update presentationVariant
- [ ] F5657 - Update presentationVariant (if analytical)
- [ ] F6003 - Update presentationVariant (if analytical)

---

## Issue 1: Custom/Complex Entity Relationships ⭐⭐⭐⭐ HARD

**Difficulty:** ⭐⭐⭐⭐ Hard  
**Effort:** 3-5 hours per app  
**Risk:** High  
**Pattern:** Requires investigation

### What to Fix
Fields that don't have standard associations:
- Custom Z/Y entities
- Complex join conditions
- Calculated fields
- Fields from nested associations

### Why It's Hard
❌ No standard associations available  
❌ May need to create custom associations  
❌ May need to modify interface views  
❌ Higher ATC risk  
❌ More testing required

### Implementation Approach
1. Identify custom entities
2. Check if text fields exist in interface views
3. Create associations if needed
4. Add text elements
5. Test thoroughly

### Apps to Check
- [ ] F5654 - Identify custom fields
- [ ] F5656 - Identify custom fields
- [ ] F5657 - Identify custom fields
- [ ] F6003 - Identify custom fields

---

## Issue 4: ATC Compliance Fixes ⭐⭐⭐ MEDIUM-HIGH

**Difficulty:** ⭐⭐⭐ Medium-High  
**Effort:** 30 min - 2 hours per app  
**Risk:** Low (if following patterns)  
**Pattern:** Known issues from Trade Order Cockpit

### What to Fix
Known ATC issues from previous sprint:

#### Issue 4a: Inconsistent Text Modeling
```
Error: Text fields referenced in @ObjectModel.text.element 
       are marked as @Semantics.text: false

Fix: Change to @Semantics.text: true
```

#### Issue 4b: Missing "use side effects"
```
Warning: "use side effects" should be added because 
         behavior is extensible

Fix: Add "use side effects;" to behavior definition
```

#### Issue 4c: Missing strict mode
```
Warning: Behavior definition should use strict ( 2 )

Fix: Add "strict ( 2 );" to behavior definition
```

### Prevention Strategy
✅ Run ATC after each change  
✅ Fix issues immediately  
✅ Don't accumulate ATC debt

### Apps to Check
- [ ] F5654 - Run ATC, fix issues
- [ ] F5656 - Run ATC, fix issues
- [ ] F5657 - Run ATC, fix issues
- [ ] F6003 - Run ATC, fix issues

---

## Recommended Work Order (Proven Approach)

### Phase 1: F5654 - Commodity Hedge Management Cockpit - Analytical View

**Day 1-2:**
1. ✅ **Issue 5:** Add standard field text associations (CompanyCode, Currency, etc.)
2. ✅ **Issue 2:** Add commodity field text associations (DCS, Hedge Book, etc.)
3. ✅ **Issue 3:** Update groupBy and requestAtLeast arrays
4. ✅ **Issue 4:** Run ATC and fix issues
5. ✅ Test in Service Binding Preview
6. ⚠️ **Issue 1:** Handle any custom fields (if found)

### Phase 2: F5656 - Commodity Hedge Management Cockpit - Overview

**Day 3-4:**
1. ✅ **Issue 5:** Add standard field text associations
2. ✅ **Issue 2:** Add commodity field text associations
3. ✅ **Issue 3:** Update groupBy and requestAtLeast
4. ✅ **Issue 4:** Run ATC and fix issues
5. ✅ Test in Service Binding Preview
6. ⚠️ **Issue 1:** Handle custom fields (if any)

### Phase 3: F5657 - Retrieve Hedge Constellation Details

**Day 5-6:**
- Follow same pattern as F5654/F5656
- Leverage learnings from first two apps

### Phase 4: F6003 - Monitor Overhedged Exposures

**Day 7-8:**
- Follow same pattern
- Should be fastest due to accumulated experience

---

## Field Discovery Checklist

### For Each App, Identify:

#### Standard SAP Fields (Issue 5)
- [ ] Company Code
- [ ] Currency
- [ ] Country
- [ ] Plant
- [ ] Material
- [ ] Customer
- [ ] Supplier
- [ ] Controlling Area
- [ ] Profit Center
- [ ] Cost Center

#### Commodity Fields (Issue 2)
- [ ] DCS (Derivative Contract Specification)
- [ ] Hedge Book
- [ ] Delivery Period
- [ ] Commodity Type
- [ ] Price Unit
- [ ] Quantity Unit
- [ ] Trading Partner
- [ ] Contract Type

#### Custom Fields (Issue 1)
- [ ] Z* or Y* fields with IDs
- [ ] Custom entities
- [ ] Calculated key fields

---

## Success Criteria Per App

### Functional Testing
- [ ] All ID fields have text associations
- [ ] Text displays correctly (ID with Name)
- [ ] "Sort by ID" appears in column menu
- [ ] "Sort by Name" appears in column menu
- [ ] Sorting by ID works
- [ ] Sorting by Name works
- [ ] Filters work correctly
- [ ] Value helps display correctly

### Technical Testing
- [ ] CDS view activates
- [ ] Metadata extension activates
- [ ] ATC check passes (zero errors)
- [ ] Service Binding Preview works
- [ ] No performance degradation
- [ ] No data inconsistencies

### Documentation
- [ ] Changes documented
- [ ] Field mappings recorded
- [ ] ATC fixes logged
- [ ] Testing results captured

---

## Known Gotchas (From Trade Order Cockpit)

### Gotcha 1: @Semantics.text: false
**Problem:** ATC error "Inconsistent Modeling of Text References"  
**Cause:** Text field has `@Semantics.text: false`  
**Fix:** Change to `@Semantics.text: true`

### Gotcha 2: Text Fields Not in groupBy
**Problem:** "Sort by Name" doesn't appear in column menu  
**Cause:** Text fields not in `groupBy` array  
**Fix:** Add text fields to `groupBy`

### Gotcha 3: Text Fields Not Pre-loaded
**Problem:** Performance issues or missing text  
**Cause:** Text fields not in `requestAtLeast`  
**Fix:** Add text fields to `requestAtLeast`

### Gotcha 4: Text Fields Visible as Separate Columns
**Problem:** Text shows as separate column instead of with ID  
**Cause:** Missing `@UI.dataFieldDefault: [{hidden: true}]`  
**Fix:** Hide text fields in metadata extension

---

## Time Estimates (Based on Trade Order Cockpit)

| Issue | Time per App | Notes |
|-------|--------------|-------|
| Issue 5 (Standard Fields) | 1-2 hours | Fast if 5-8 standard fields |
| Issue 2 (Commodity Fields) | 2-3 hours | Requires finding text fields |
| Issue 3 (groupBy/requestAtLeast) | 1 hour | Straightforward once fields known |
| Issue 4 (ATC Fixes) | 30 min - 1 hour | Quick if following patterns |
| Issue 1 (Custom Fields) | 3-5 hours | Only if custom fields exist |
| **Total per App** | **4-6 hours** | **Without custom fields** |
| **Total per App** | **7-11 hours** | **With custom fields** |

---

## Resources

### Documentation References
- `/workspace/docs/ux30_quick_reference.md` - Quick implementation guide
- `/workspace/docs/fiori_column_sorting_guide.md` - Detailed sorting guide
- `/workspace/docs/jira_summary.md` - Trade Order Cockpit learnings
- `/workspace/rap-rules.md` - General RAP guidelines

### Code Templates
- Standard field association template (in ux30_quick_reference.md)
- Commodity field pattern (from Trade Order Cockpit)
- Metadata extension template (in fiori_column_sorting_guide.md)

---

**Created:** December 15, 2025  
**Sprint:** Cluster C - Analytics & Oversight  
**Approach:** Proven pattern from Trade Order Cockpit  
**Start with:** Issue 5 → Issue 2 → Issue 3 → Issue 4 → Issue 1
