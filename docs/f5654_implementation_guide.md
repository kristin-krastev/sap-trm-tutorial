# F5654 - Commodity Hedge Management Cockpit - Analytical View
## UX 3.0 Implementation Guide

**App Code:** F5654  
**App Name:** Commodity Hedge Management Cockpit - Analytical View  
**Sprint:** Cluster C - Analytics & Oversight  
**Date:** December 15, 2025

---

## Prerequisites

### Information Needed from SAP System

Before starting implementation, gather:

1. **Consumption View Name:** `C_________________` (fill in actual name)
2. **Metadata Extension Name:** Same as view name  
3. **Interface View Name:** `I_________________` (if different from consumption)
4. **Package:** FIN_CMM_* (Commodity Management package)

### How to Find View Names

**Method 1: Via Fiori App**
```
1. Open F5654 in Fiori Launchpad (your SAP system)
2. F12 Developer Tools → Network tab
3. Filter for "odata" or "$metadata"
4. Find the service name and CDS view
```

**Method 2: Via Eclipse ADT**
```
1. Open Eclipse with ABAP Development Tools
2. Search for "F5654" or "Commodity Hedge Management Cockpit"
3. Look for consumption view (C_*) or service definition
4. Note the view name and package
```

---

## Step 0: Analysis Phase

### Current State Assessment

Run this checklist in Eclipse/ADT:

- [ ] Located consumption view (C_*)
- [ ] Located metadata extension
- [ ] Opened CDS view source code
- [ ] Identified all ID fields in SELECT list
- [ ] Identified which fields are keys
- [ ] Checked for existing text associations
- [ ] Checked @UI.presentationVariant
- [ ] Noted which fields already have text elements

### Field Inventory Template

Create a list like this:

| Field Name | Type | Has Text? | Text Field Name | Priority |
|------------|------|-----------|-----------------|----------|
| CompanyCode | Standard | ❌ No | CompanyCodeName | Issue 5 |
| Currency | Standard | ❌ No | CurrencyName | Issue 5 |
| CmmdtyHedgePlanExposureDCSID | Commodity | ❌ No | DerivativeContrSpecName | Issue 2 |
| CmmdtyHdgPlanExposureHedgeBook | Commodity | ❌ No | CmmdtyHedgeBookDescription | Issue 2 |
| ... | ... | ... | ... | ... |

---

## Step 1: Issue 5 - Standard SAP Fields (EASIEST)

**Time Estimate:** 1-2 hours  
**Difficulty:** ⭐ Easy

### Likely Standard Fields in F5654

Based on typical Commodity Hedge Management cockpits:

#### Company Code
```abap
// In CDS View - Add association
association [0..1] to I_CompanyCode as _CompanyCode 
  on $projection.CompanyCode = _CompanyCode.CompanyCode

// In SELECT list
{
  @ObjectModel.text.element: ['CompanyCodeName']
  key CompanyCode,
  
  @Semantics.text: true
  _CompanyCode.CompanyCodeName as CompanyCodeName,
  
  // ... rest of fields
}
```

#### Currency
```abap
// Add association
association [0..1] to I_Currency as _Currency
  on $projection.Currency = _Currency.Currency

// In SELECT list
{
  @ObjectModel.text.element: ['CurrencyName']
  Currency,
  
  @Semantics.text: true
  _Currency.CurrencyName as CurrencyName,
}
```

#### Material (if present)
```abap
association [0..1] to I_Product as _Product
  on $projection.Material = _Product.Product

{
  @ObjectModel.text.element: ['MaterialName']
  Material,
  
  @Semantics.text: true
  _Product.ProductName as MaterialName,
}
```

### Metadata Extension Updates (Issue 5)

```abap
@Metadata.layer: #CORE
annotate view C_<YourViewName> with
{
  // Company Code
  @UI.textArrangement: #TEXT_LAST
  @EndUserText.label: 'Company Code'
  CompanyCode;
  
  @Semantics.text: true
  @Consumption.filter.hidden: true
  @UI.dataFieldDefault: [{hidden: true}]
  CompanyCodeName;
  
  // Currency
  @UI.textArrangement: #TEXT_LAST
  @EndUserText.label: 'Currency'
  Currency;
  
  @Semantics.text: true
  @Consumption.filter.hidden: true
  @UI.dataFieldDefault: [{hidden: true}]
  CurrencyName;
  
  // Material (if applicable)
  @UI.textArrangement: #TEXT_LAST
  @EndUserText.label: 'Material'
  Material;
  
  @Semantics.text: true
  @Consumption.filter.hidden: true
  @UI.dataFieldDefault: [{hidden: true}]
  MaterialName;
}
```

---

## Step 2: Issue 2 - Commodity-Specific Fields (MEDIUM)

**Time Estimate:** 2-3 hours  
**Difficulty:** ⭐⭐ Medium

### Likely Commodity Fields in F5654

Based on Commodity Hedge Management domain:

#### DCS (Derivative Contract Specification)
```abap
// This text field might already exist in interface view
// Check interface view first!

{
  @ObjectModel.text.element: ['DerivativeContrSpecName']
  key CmmdtyHedgePlanExposureDCSID,
  
  @Semantics.text: true
  DerivativeContrSpecName,  // Check if this field exists
}
```

**If text field doesn't exist in SELECT:**
```abap
// Option 1: Add via association (if view exists)
association [0..1] to I_CommodityHedgeDCS as _DCS
  on $projection.CmmdtyHedgePlanExposureDCSID = _DCS.DCS_ID

{
  @ObjectModel.text.element: ['DerivativeContrSpecName']
  key CmmdtyHedgePlanExposureDCSID,
  
  @Semantics.text: true
  _DCS.DCS_Name as DerivativeContrSpecName,
}
```

#### Hedge Book
```abap
{
  @ObjectModel.text.element: ['CmmdtyHedgeBookDescription']
  key CmmdtyHdgPlanExposureHedgeBook,
  
  @Semantics.text: true
  CmmdtyHedgeBookDescription,  // Check if exists
}
```

#### Commodity Type (if present)
```abap
{
  @ObjectModel.text.element: ['CommodityTypeName']
  CommodityType,
  
  @Semantics.text: true
  CommodityTypeName,
}
```

### Metadata Extension Updates (Issue 2)

```abap
annotate view C_<YourViewName> with
{
  // DCS
  @UI.textArrangement: #TEXT_LAST
  @EndUserText.label: 'DCS'
  CmmdtyHedgePlanExposureDCSID;
  
  @Semantics.text: true
  @Consumption.filter.hidden: true
  @UI.dataFieldDefault: [{hidden: true}]
  DerivativeContrSpecName;
  
  // Hedge Book
  @UI.textArrangement: #TEXT_LAST
  @EndUserText.label: 'Hedge Book'
  CmmdtyHdgPlanExposureHedgeBook;
  
  @Semantics.text: true
  @Consumption.filter.hidden: true
  @UI.dataFieldDefault: [{hidden: true}]
  CmmdtyHedgeBookDescription;
  
  // Commodity Type (if applicable)
  @UI.textArrangement: #TEXT_LAST
  @EndUserText.label: 'Commodity Type'
  CommodityType;
  
  @Semantics.text: true
  @Consumption.filter.hidden: true
  @UI.dataFieldDefault: [{hidden: true}]
  CommodityTypeName;
}
```

---

## Step 3: Issue 3 - Update groupBy and requestAtLeast (MEDIUM-HIGH)

**Time Estimate:** 1 hour  
**Difficulty:** ⭐⭐⭐ Medium-High

### Locate Current presentationVariant

In metadata extension, find:
```abap
@UI.presentationVariant: [{
  visualizations: [{ type: #AS_LINEITEM }],
  requestAtLeast: [ ... ],
  groupBy: [ ... ]
}]
```

### Update Both Arrays

**IMPORTANT:** Add BOTH the ID field AND text field to BOTH arrays!

```abap
@UI.presentationVariant: [{
  visualizations: [{ type: #AS_LINEITEM }],
  
  requestAtLeast: [ 
    // Standard fields
    'CompanyCode', 'CompanyCodeName',
    'Currency', 'CurrencyName',
    'Material', 'MaterialName',  // if applicable
    
    // Commodity fields
    'CmmdtyHedgePlanExposureDCSID', 'DerivativeContrSpecName',
    'CmmdtyHdgPlanExposureHedgeBook', 'CmmdtyHedgeBookDescription',
    'CommodityType', 'CommodityTypeName',  // if applicable
    
    // Keep all existing fields
    'ExistingField1',
    'ExistingField2',
    // ... etc
  ],
  
  groupBy: [ 
    // Add SAME fields as requestAtLeast
    'CompanyCode', 'CompanyCodeName',
    'Currency', 'CurrencyName',
    'Material', 'MaterialName',  // if applicable
    'CmmdtyHedgePlanExposureDCSID', 'DerivativeContrSpecName',
    'CmmdtyHdgPlanExposureHedgeBook', 'CmmdtyHedgeBookDescription',
    'CommodityType', 'CommodityTypeName',  // if applicable
    
    // Keep all existing fields
    'ExistingField1',
    'ExistingField2',
    // ... etc
  ]
}]
```

### ⚠️ Critical: Don't Miss Any Text Fields

If you miss a text field in `groupBy`, "Sort by Name" won't appear!

**Checklist:**
- [ ] All ID fields from Step 1 (Issue 5) added to groupBy
- [ ] All text fields from Step 1 added to groupBy
- [ ] All ID fields from Step 2 (Issue 2) added to groupBy
- [ ] All text fields from Step 2 added to groupBy
- [ ] Same fields added to requestAtLeast
- [ ] No duplicates in either array

---

## Step 4: Issue 4 - ATC Checks and Fixes (MEDIUM-HIGH)

**Time Estimate:** 30 min - 1 hour  
**Difficulty:** ⭐⭐⭐ Medium-High

### Run ATC Check

In Eclipse/ADT:
```
1. Right-click on CDS view
2. Run As → ATC Check
3. Review findings
4. Fix issues
5. Re-run ATC
```

### Expected ATC Issues

#### Issue 4a: Inconsistent Text Modeling
```
Error: Text fields referenced in @ObjectModel.text.element 
       are marked as @Semantics.text: false

Fix in CDS View:
// BEFORE (wrong)
@Semantics.text: false
CompanyCodeName,

// AFTER (correct)
@Semantics.text: true
CompanyCodeName,
```

#### Issue 4b: Missing Strict Mode (if behavior definition exists)
```
Warning: Behavior definition should use strict ( 2 )

Fix in .bdef.asbdef file:
managed implementation in class zbp_<entity> unique;
strict ( 2 );  // ← Add this line
use side effects;  // ← Also add this

define behavior for ...
```

### ATC Checklist
- [ ] Zero errors
- [ ] Zero warnings (or only acceptable warnings)
- [ ] "Inconsistent Text Modeling" resolved
- [ ] All text fields have `@Semantics.text: true`
- [ ] No performance warnings
- [ ] No authorization warnings

---

## Step 5: Testing

### Test in Service Binding Preview

```
1. Open CDS view in Eclipse
2. Right-click → Open With → Data Preview
   OR
3. Find Service Definition → Open Service Binding → Preview
```

### Test Checklist

#### Visual Tests
- [ ] Text displays next to ID (e.g., "1000 (Company Name)")
- [ ] Format is "ID (Text)" not "Text (ID)"
- [ ] No separate text columns visible
- [ ] All enhanced fields show text

#### Sorting Tests
- [ ] Right-click column header
- [ ] "Sort Ascending" appears (by ID)
- [ ] "Sort Descending" appears (by ID)
- [ ] "Sort Ascending" appears (by Name) ← **KEY TEST**
- [ ] "Sort Descending" appears (by Name) ← **KEY TEST**
- [ ] Sorting by ID works correctly
- [ ] Sorting by Name works correctly

#### Functional Tests
- [ ] Filters work on ID fields
- [ ] Filters work on text fields
- [ ] Value helps open correctly
- [ ] Data loads without errors
- [ ] No performance degradation
- [ ] Pagination works

---

## Step 6: Documentation

### Create Implementation Log

Record in `/workspace/docs/f5654_implementation_log.md`:

```markdown
# F5654 Implementation Log

## Date: December XX, 2025

### Views Modified
- Consumption View: C_________________
- Metadata Extension: (same name)

### Fields Enhanced

#### Issue 5 - Standard Fields
- [ ] CompanyCode → CompanyCodeName
- [ ] Currency → CurrencyName
- [ ] Material → MaterialName (if applicable)

#### Issue 2 - Commodity Fields
- [ ] CmmdtyHedgePlanExposureDCSID → DerivativeContrSpecName
- [ ] CmmdtyHdgPlanExposureHedgeBook → CmmdtyHedgeBookDescription
- [ ] CommodityType → CommodityTypeName (if applicable)

### ATC Results
- Before: X errors, Y warnings
- After: 0 errors, 0 warnings

### Test Results
- Service Binding Preview: ✅ Pass
- Sort by ID: ✅ Works
- Sort by Name: ✅ Works
- Filters: ✅ Work
- Performance: ✅ No degradation

### Time Spent
- Analysis: ___ hours
- Implementation: ___ hours
- Testing: ___ hours
- Total: ___ hours
```

---

## Troubleshooting

### Problem: Text doesn't show next to ID
**Check:**
1. Is `@ObjectModel.text.element` on ID field?
2. Is text field in SELECT list?
3. Is `@UI.textArrangement` in metadata extension?
4. Is association returning data?

### Problem: "Sort by Name" doesn't appear
**Check:**
1. Is text field in `groupBy` array?
2. Is text field in `requestAtLeast` array?
3. Is view type analytical (not responsive table)?

### Problem: ATC error "Inconsistent Text Modeling"
**Fix:**
Change `@Semantics.text: false` to `@Semantics.text: true`

### Problem: Text field shows as separate column
**Fix:**
Add to metadata extension:
```abap
@UI.dataFieldDefault: [{hidden: true}]
TextFieldName;
```

---

## Next Steps After F5654

Once F5654 is complete:
1. Document learnings
2. Update time estimates
3. Move to F5656 (should be faster!)
4. Apply same pattern

---

**Ready to implement once you provide:**
- ✅ Consumption view name
- ✅ Current field list (optional)
- ✅ Access to SAP system/Eclipse

---

*Created: December 15, 2025*  
*App: F5654 - Commodity Hedge Management Cockpit - Analytical View*  
*Pattern: Issue 5 → Issue 2 → Issue 3 → Issue 4*
