# Counter Deal Request Manifest Analysis

## ✅ Good News

### 1. Table Type is Correct
```json
"tableSettings": {
    "type": "GridTable"  // ✅ This supports full sorting!
}
```
**GridTable** has complete sorting capabilities, including text fields.

### 2. No Sorting Restrictions in Manifest
No explicit `"sortable": false` or column restrictions found.

---

## ⚠️ CRITICAL FINDING: Custom Column Fragment

### The Extension:
```json
"sap.ui.viewExtensions": {
    "sap.suite.ui.generic.template.ListReport.view.ListReport": {
        "GridTableColumnsExtension|CounterdealRequest": {
            "className": "sap.ui.core.Fragment",
            "fragmentName": "ibso.commodity.counterdeal.manage.ext.fragment.CustomColumns",
            "type": "XML"
        }
    }
}
```

### 🚨 **THIS COULD BE THE SMOKING GUN!**

**Location to check:**
```
webapp/ext/fragment/CustomColumns.fragment.xml
```

**What this means:**
- The app has **custom column definitions**
- These custom columns might be **overriding the standard behavior**
- The custom fragment might not have proper text field sorting configured

---

## 🔍 What to Look For in CustomColumns.fragment.xml

### Check for column definitions like:

```xml
<Column>
  <customData>
    <core:CustomData 
      key="p13nData" 
      value='{"columnKey":"CmmdtyHedgePlanExposureDCSID", "sortProperty":"CmmdtyHedgePlanExposureDCSID"}'
    />
  </customData>
  <Text text="DCS"/>
</Column>
```

### Problems to look for:

1. **Missing text field in sortProperty:**
```xml
<!-- BAD: Only ID field for sorting -->
"sortProperty":"CmmdtyHedgePlanExposureDCSID"

<!-- GOOD: Should also have text field -->
"sortProperty":"CmmdtyHedgePlanExposureDCSID,DerivativeContrSpecName"
```

2. **Explicit sortable=false:**
```xml
<Column sortProperty="" ...>  <!-- Empty = not sortable -->
```

3. **Hardcoded column without text field:**
```xml
<Column 
  sortProperty="CmmdtyHedgePlanExposureDCSID"
  filterProperty="CmmdtyHedgePlanExposureDCSID">
  <!-- Missing DerivativeContrSpecName reference -->
</Column>
```

---

## 📋 Other Interesting Points

### SAPUI5 Version
```json
"minUI5Version": "${sap.ui5.dist.version}"
```
**This is a build variable** - we need to check the deployed version.

**How to check:**
- Browser console: `sap.ui.version`
- Or check deployed app footer

### Smart Template vs Fiori Elements
```json
"sourceTemplate": {
    "id": "ui5template.smartTemplate",
    "version": "1.40.12"
}
```
This uses **Smart Template** (older framework, before Fiori Elements v4).

**Smart Template behavior:**
- Text field sorting **should** work with GridTable
- But custom fragments can override standard behavior

---

## 🎯 Immediate Action Required

### Step 1: Find CustomColumns.fragment.xml
**Possible locations:**
- `webapp/ext/fragment/CustomColumns.fragment.xml`
- Or check the project structure for `ext/fragment/` folder

### Step 2: Check for DCS Column Definition
Look for:
```xml
<!-- Search for: -->
CmmdtyHedgePlanExposureDCSID
DerivativeContrSpecName
```

### Step 3: Check for Hedge Book Column Definition
Look for:
```xml
<!-- Search for: -->
CmmdtyHdgPlanExposureHedgeBook
CmmdtyHedgeBookDescription
```

### Step 4: Compare with Company Code Column
If Company Code is NOT in the custom fragment, that's why it works!
- Custom columns = limited behavior
- Standard columns = full sorting support

---

## 💡 Expected Problem

**Most Likely Scenario:**

The custom fragment defines DCS and Hedge Book columns like this:
```xml
<Column 
  sortProperty="CmmdtyHedgePlanExposureDCSID"  <!-- Only ID! -->
  ...>
  <Text text="DCS"/>
</Column>
```

**But Company Code is NOT in the custom fragment**, so it uses standard rendering:
- Standard columns automatically get text field sorting
- Custom columns only get what you explicitly define

---

## 🔧 Expected Fix

**In CustomColumns.fragment.xml, change:**

```xml
<!-- BEFORE (only ID sorting): -->
<Column 
  sortProperty="CmmdtyHedgePlanExposureDCSID"
  filterProperty="CmmdtyHedgePlanExposureDCSID">

<!-- AFTER (both ID and text sorting): -->
<Column 
  sortProperty="CmmdtyHedgePlanExposureDCSID,DerivativeContrSpecName"
  filterProperty="CmmdtyHedgePlanExposureDCSID,DerivativeContrSpecName">
```

**OR** - Remove these columns from CustomColumns.fragment.xml entirely and let them use standard rendering!

---

## 📊 Confidence Level

**95% confident this is the issue!**

**Why:**
1. ✅ Manifest has correct table type (GridTable)
2. ✅ Manifest has no sorting restrictions
3. ✅ Backend metadata is correct (we verified this)
4. ⚠️ **But** there's a custom columns extension
5. ✅ Custom extensions often break standard features
6. ✅ This explains why Service Binding Preview works (no custom UI) but app doesn't

---

## 🚀 Next Step

**Please share the contents of:**
```
webapp/ext/fragment/CustomColumns.fragment.xml
```

This is very likely where the problem is! 🎯
