# UX 3.0 Quick Reference Guide

**Purpose:** Fast reference for implementing UX 3.0 enhancements on Fiori Elements analytical apps

---

## The Pattern (5 Steps)

### Step 1: Add Text Field to CDS View
```abap
define root view C_YourView
  as select from I_YourView
  association [0..1] to I_CompanyCode as _CompanyCode 
    on $projection.CompanyCode = _CompanyCode.CompanyCode
{
  @ObjectModel.text.element: ['CompanyCodeName']
  key CompanyCode,
  
  @Semantics.text: true
  _CompanyCode.CompanyCodeName as CompanyCodeName,
}
```

### Step 2: Update Metadata Extension - Add Text Arrangement
```abap
annotate view C_YourView with
{
  @UI.textArrangement: #TEXT_LAST
  @EndUserText.label: 'Company Code'
  CompanyCode;
  
  @Semantics.text: true
  @Consumption.filter.hidden: true
  @UI.dataFieldDefault: [{hidden: true}]
  CompanyCodeName;
}
```

### Step 3: Add to groupBy Array
```abap
@UI.presentationVariant: [{
  groupBy: [ 
    'CompanyCode',
    'CompanyCodeName',  // ← ADD THIS
    ...
  ]
}]
```

### Step 4: Add to requestAtLeast Array
```abap
@UI.presentationVariant: [{
  requestAtLeast: [ 
    'CompanyCode',
    'CompanyCodeName',  // ← ADD THIS
    ...
  ]
}]
```

### Step 5: Run ATC Checks
- Ensure `@Semantics.text: true` (NOT false)
- Fix "Inconsistent Modeling of Text References" errors
- Check for performance warnings

---

## Common Mistakes ❌

### Mistake 1: Wrong Semantics
```abap
// ❌ WRONG
@Semantics.text: false
CompanyCodeName

// ✅ CORRECT
@Semantics.text: true
CompanyCodeName
```

### Mistake 2: Missing from groupBy
```abap
// ❌ WRONG - Only ID in groupBy
@UI.presentationVariant: [{
  groupBy: [ 'CompanyCode' ]
}]

// ✅ CORRECT - Both ID and text in groupBy
@UI.presentationVariant: [{
  groupBy: [ 'CompanyCode', 'CompanyCodeName' ]
}]
```

### Mistake 3: Text Field Visible in UI
```abap
// ❌ WRONG - Text field shows as separate column
CompanyCodeName;

// ✅ CORRECT - Text field hidden, shows with ID
@UI.dataFieldDefault: [{hidden: true}]
CompanyCodeName;
```

---

## Standard Associations Reference

| Field Type | ID Field | Text Field | Association View |
|------------|----------|------------|------------------|
| Company Code | `CompanyCode` | `CompanyCodeName` | `I_CompanyCode` |
| Currency | `Currency` | `CurrencyName` | `I_Currency` |
| Country | `Country` | `CountryName` | `I_Country` |
| Plant | `Plant` | `PlantName` | `I_Plant` |
| Customer | `Customer` | `CustomerName` | `I_Customer` |
| Supplier | `Supplier` | `SupplierName` | `I_Supplier` |
| Material | `Material` | `MaterialName` | `I_Material` |
| Product | `Product` | `ProductName` | `I_Product` |

---

## Commodity-Specific Fields

| Field Type | ID Field | Text Field |
|------------|----------|------------|
| DCS | `CmmdtyHedgePlanExposureDCSID` | `DerivativeContrSpecName` |
| Hedge Book | `CmmdtyHdgPlanExposureHedgeBook` | `CmmdtyHedgeBookDescription` |

---

## Testing Checklist ✓

### Quick Tests
- [ ] View activates
- [ ] ATC check passes
- [ ] Service Binding Preview loads
- [ ] Text shows next to ID (e.g., "1000 (Company Name)")
- [ ] Right-click column → See "Sort by ID" options
- [ ] Right-click column → See "Sort by Name" options

### Detailed Tests
- [ ] Sort by ID (ascending/descending)
- [ ] Sort by Name (ascending/descending)
- [ ] Filter by ID works
- [ ] Value help works
- [ ] No performance degradation

---

## ATC Check Focus Areas

1. **Text Reference Consistency**
   - Check: `@ObjectModel.text.element` matches `@Semantics.text: true`
   - Error: "Inconsistent Modeling of Text References"

2. **Performance**
   - Check: Text fields in `requestAtLeast`
   - Warning: Missing pre-load optimization

3. **UI Consistency**
   - Check: Text fields properly hidden
   - Warning: Duplicate data in UI

---

## Code Template

```abap
// ============================================
// CDS VIEW
// ============================================
define root view C_YourAnalyticalView
  as select from I_YourView
  
  association [0..1] to I_CompanyCode as _CompanyCode 
    on $projection.CompanyCode = _CompanyCode.CompanyCode
{
  // ID Field with text association
  @ObjectModel.text.element: ['CompanyCodeName']
  key CompanyCode,
  
  // Text Field
  @Semantics.text: true
  _CompanyCode.CompanyCodeName as CompanyCodeName,
  
  // ... other fields
}

// ============================================
// METADATA EXTENSION
// ============================================
@Metadata.layer: #CORE

@UI.presentationVariant: [{
  visualizations: [{ type: #AS_LINEITEM }],
  
  requestAtLeast: [ 
    'CompanyCode', 
    'CompanyCodeName',
    // ... other fields
  ],
  
  groupBy: [ 
    'CompanyCode', 
    'CompanyCodeName',
    // ... other fields
  ]
}]

annotate view C_YourAnalyticalView with
{
  // ID Field Annotations
  @UI.textArrangement: #TEXT_LAST
  @EndUserText.label: 'Company Code'
  CompanyCode;
  
  // Text Field Annotations (HIDE IT)
  @Semantics.text: true
  @Consumption.filter.hidden: true
  @UI.dataFieldDefault: [{hidden: true}]
  CompanyCodeName;
}
```

---

## Troubleshooting

### Problem: Text not showing next to ID
**Check:**
1. Is `@ObjectModel.text.element` on ID field?
2. Is text field in SELECT list?
3. Is `@UI.textArrangement` set?
4. Is association returning data?

### Problem: Can't sort by Name
**Check:**
1. Is text field in `groupBy` array?
2. Is text field in `requestAtLeast` array?
3. Is it an analytical table (not responsive)?

### Problem: ATC error about text modeling
**Check:**
1. Is `@Semantics.text: true` on text field?
2. Does text field name match `@ObjectModel.text.element`?

---

## Time Estimates

Per app with 5 ID fields:
- Analysis: 1 hour
- Implementation: 2-3 hours
- ATC fixes: 30 min - 1 hour
- Testing: 1 hour
- **Total: 4-6 hours per app**

---

**Last Updated:** December 15, 2025  
**Version:** 1.0  
**Based on:** Trade Order Cockpit implementation (Dec 2025)
