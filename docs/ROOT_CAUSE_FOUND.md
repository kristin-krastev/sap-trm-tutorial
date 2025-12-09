# Root Cause: Text Field Sorting Issue - SOLVED ✅

## Date
December 8, 2025

## Summary
**Issue:** DCS and Hedge Book columns only show "Sort by ID" option, missing "Sort by Name" in deployed Fiori apps  
**Root Cause:** Custom UI fragment `CustomColumns.fragment.xml` explicitly overrides `sortProperty` to only the ID field  
**Status:** IDENTIFIED - Solution documented below

---

## The Smoking Gun 🔍

### File Location
`CustomColumns.fragment.xml` (referenced in `manifest.json` as `GridTableColumnsExtension`)

### Problematic Code

#### DCS Column
```xml
<table:Column sortProperty="CmmdtyHedgePlanExposureDCSID" filterProperty="CmmdtyHedgePlanExposureDCSID">
    <Label text="{/#CounterdealRequestType/CmmdtyHedgePlanExposureDCSID/@sap:label}"/>
    <table:template>
        <HBox>
            <Link press="onNavLinkPress" 
                  text="{parts: [ 'CmmdtyHedgePlanExposureDCSID', 'DerivativeContrSpecName'], 
                         formatter: '.formatter.IDAndDescriptionFormatter'}"/>
        </HBox>
    </table:template>
</table:Column>
```

**Problem:** `sortProperty="CmmdtyHedgePlanExposureDCSID"` → Only ID field

#### Hedge Book Column
```xml
<table:Column sortProperty="CmmdtyHdgPlanExposureHedgeBook" filterProperty="CmmdtyHdgPlanExposureHedgeBook">
    <Label text="{/#CounterdealRequestType/CmmdtyHdgPlanExposureHedgeBook/@sap:label}"/>
    <table:template>
        <HBox>
            <Link press="onNavLinkPress" 
                  text="{parts: [ 'CmmdtyHdgPlanExposureHedgeBook', 'CmmdtyHedgeBookDescription'], 
                         formatter: '.formatter.IDAndDescriptionFormatter'}"/>
        </HBox>
    </table:template>
</table:Column>
```

**Problem:** `sortProperty="CmmdtyHdgPlanExposureHedgeBook"` → Only ID field

---

## Why This Causes the Issue

1. **Custom Fragment Overrides Standard Behavior**
   - Fiori Elements would normally generate sort options for BOTH ID and text fields automatically based on OData metadata
   - Custom column definition with explicit `sortProperty` **overrides** this automatic behavior
   - UI5 `sap.ui.table.Column` only accepts a **single string** for `sortProperty`, not multiple values

2. **Service Binding Preview vs. Deployed App**
   - **Preview:** Uses standard Fiori Elements rendering → Shows both sort options ✅
   - **Deployed App:** Uses custom fragment → Only shows ID sort option ❌

3. **Why Other Fields Work**
   - Fields like Company Code, Created By, Changed By are NOT in the custom fragment
   - They use standard Fiori Elements behavior → Multiple sort options work ✅

---

## Evidence Timeline

### What We Observed
1. ✅ OData metadata was perfect (`sap:text`, `sap:sortable` all correct)
2. ✅ CDS view annotations were correct (`@ObjectModel.text.element`, `@Semantics.text: true`)
3. ✅ Service Binding Preview showed both sort options
4. ❌ Deployed app only showed ID sort option
5. ❌ Issue appeared in BOTH Trade Order Cockpit AND Counter Deal Request apps

### The Clue
- `manifest.json` contained: `"GridTableColumnsExtension": { "fragmentName": "ibso.commodity.counterdeal.manage.ext.fragment.CustomColumns" }`
- This fragment was the missing piece!

---

## Solution Options

### ✅ Option 1: Remove Custom Columns (Safest)
**Recommendation:** Remove DCS and Hedge Book from `CustomColumns.fragment.xml`

**Pros:**
- Automatic sorting for ID and Name fields
- Less maintenance
- Standard Fiori Elements behavior

**Cons:**
- Lose custom Link navigation behavior
- Lose custom formatting via `IDAndDescriptionFormatter`

**When to use:** If navigation links are not critical business requirement

---

### ✅ Option 2: Accept Current Limitation (Pragmatic)
**Recommendation:** Keep as-is, document limitation

**Pros:**
- Zero risk to production
- Custom navigation links preserved
- Custom formatting preserved

**Cons:**
- Users can only sort by ID, not by Name

**When to use:** When custom links are important AND risk mitigation is priority (✅ Current decision)

---

### ⚠️ Option 3: Advanced Custom Implementation (High Risk)
**Recommendation:** Implement custom multi-property sorting

**What it requires:**
1. Modify `CustomColumns.fragment.xml` to add text fields as columns
2. Create custom controller extension
3. Implement `onColumnPress` handler for custom sort logic
4. Handle multi-property sorting programmatically
5. Extensive testing in DEV, QA, PRD

**Example pseudo-code:**
```javascript
// Controller extension needed
onColumnPress: function(oEvent) {
    var oColumn = oEvent.getParameter("column");
    var sColumnId = oColumn.getId();
    
    if (sColumnId.includes("DCS")) {
        // Custom logic to toggle between:
        // 1. Sort by CmmdtyHedgePlanExposureDCSID
        // 2. Sort by DerivativeContrSpecName
    } else if (sColumnId.includes("HedgeBook")) {
        // Custom logic to toggle between:
        // 1. Sort by CmmdtyHdgPlanExposureHedgeBook
        // 2. Sort by CmmdtyHedgeBookDescription
    }
}
```

**Pros:**
- Best of both worlds (navigation + sorting)

**Cons:**
- High implementation effort
- Requires JavaScript changes
- Risk of breaking existing functionality
- Requires extensive testing

**When to use:** When both features are critical business requirements AND sufficient testing resources available

---

## Technical Details

### UI5 Column sortProperty Limitation
From SAP UI5 documentation for `sap.ui.table.Column`:

```
sortProperty: string
Property name of the column for sorting.
```

**Key limitation:** Single string value only, not an array. This is a fundamental UI5 design constraint.

### Standard Fiori Elements Behavior (Without Custom Fragment)
When no custom fragment is used, Fiori Elements:
1. Reads OData metadata
2. Detects `sap:text` associations
3. Automatically generates sort options for both ID and text fields
4. Users see "Sort by [ID Field]" and "Sort by [Text Field]" in context menu

### Custom Fragment Behavior (Current Implementation)
When custom fragment defines columns:
1. Overrides Fiori Elements automatic generation
2. Uses explicit `sortProperty` from fragment
3. Only single sort option available
4. Requires manual implementation for multi-property sorting

---

## Recommendation for Production

**Selected Option:** Option 2 - Accept Current Limitation ✅

**Justification:**
1. ✅ Zero risk to production environment
2. ✅ Custom navigation links preserved (business value)
3. ✅ Custom formatting preserved (UX consistency)
4. ✅ Sort by ID still available (primary use case)
5. ✅ Team agreement to move forward

**Documentation:** This limitation should be documented in:
- User training materials
- Known limitations section
- Future enhancement backlog (if needed)

---

## Lessons Learned

1. **Custom UI Extensions Override Standard Behavior**
   - Always check `manifest.json` for fragments and extensions
   - Custom fragments can override OData metadata behavior

2. **Service Binding Preview ≠ Deployed App**
   - Preview uses standard rendering
   - Deployed app may have custom extensions

3. **Investigation Order for Fiori Issues**
   - ✅ Check OData metadata
   - ✅ Check CDS annotations
   - ✅ Check metadata extensions
   - ✅ **Check manifest.json for custom fragments** ← Critical step!
   - ✅ Check custom controllers and extensions

4. **UI5 Table Limitations**
   - `sortProperty` accepts single string only
   - Multi-property sorting requires custom implementation
   - Standard Fiori Elements handles this automatically

---

## Status
- [x] Root cause identified
- [x] Solution options documented
- [x] Recommendation made
- [x] Team decision: Accept as-is
- [x] Documentation complete

**Issue Closed:** December 8, 2025  
**Resolution:** Custom fragment limitation - accepted by business
