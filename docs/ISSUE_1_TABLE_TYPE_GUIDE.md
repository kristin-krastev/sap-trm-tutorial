# Issue 1: Table Type - Implementation Guide
**Date:** December 8, 2025  
**Apps:** F5658, F5655, F5659, F5665, F5666  
**Priority:** High (Foundational)  
**Effort:** Low to Medium

---

## Problem Summary

### Common Mistakes

#### ❌ Mistake 1: Responsive Table with Large Datasets
**Problem:**
- Responsive Table used when >200 items possible
- Results in poor usability and performance
- Scrolling becomes nearly impossible
- Comparison across rows/columns is difficult

**Impact:** High - Directly affects user productivity

---

#### ❌ Mistake 2: Grid Table without Condensed Layout
**Problem:**
- Grid Table used on List Report Page
- Condensed layout NOT enabled for desktop devices
- Poor information density
- Users see less data per screen

**Impact:** Medium - Affects information density and UX

---

## Decision Criteria

### ✅ Use Responsive Table When:

| Criteria | Description | Typical Use Case |
|----------|-------------|------------------|
| **Line Item Focus** | Focus on working with line items, not individual cells | Order items, document items |
| **Small Dataset** | ≤200 items total in table | Master data maintenance, worklists |
| **Flexible Content** | Need micro charts, progress indicators, semantic grouping | Status monitoring, dashboards |
| **Mobile Support** | Must work well on tablets/phones | Mobile approval apps |

**Best For:** Object Pages, mobile-first apps, visual/flexible content

---

### ✅ Use Grid Table When:

| Criteria | Description | Typical Use Case |
|----------|-------------|------------------|
| **Large Dataset** | Hundreds or thousands of items | List reports, search results |
| **Comparison Use Case** | Users need to compare rows/columns | Trading cockpits, monitoring apps |
| **Cell-Level Focus** | Operations on individual cells matter | Data entry, cell editing |
| **Spatial Relationships** | Relationship between cells is important | Financial data, analytical views |
| **Cross-Column Operations** | Operations across columns required | Summations, aggregations |

**Best For:** List Report Pages, large datasets, analytical use cases

---

### 🌳 Use Analytical Table When:
- Hierarchical/grouped data with aggregations
- Need sum, average, count at group level
- Drill-down/drill-up functionality required

---

### 🌲 Use Tree Table When:
- Hierarchical data with parent-child relationships
- Expandable/collapsible tree structure
- Bill of materials, org charts, category trees

---

## App Analysis: Recommended Table Types

### F5658 - Commodity Trader's Order Cockpit
**App Type:** List Report  
**Use Case:** Monitor and manage trade orders  
**Expected Data Volume:** Likely hundreds of orders  
**Key Activities:** Compare orders, filter, sort, select for action  

**Recommended Table Type:** 🏆 **Grid Table**

**Rationale:**
- ✅ Large dataset (trade orders can be numerous)
- ✅ Comparison critical (compare prices, quantities, status)
- ✅ List Report pattern
- ✅ Desktop-primary use case

**Additional Config:** Enable condensed layout for desktop

---

### F5655 - Observe and Monitor Deal Requests
**App Type:** List Report  
**Use Case:** Monitor deal request status and progress  
**Expected Data Volume:** Moderate to large (hundreds of requests)  
**Key Activities:** Monitor status, compare requests, drill into details  

**Recommended Table Type:** 🏆 **Grid Table**

**Rationale:**
- ✅ Monitoring requires seeing many items at once
- ✅ Comparison of deal requests is key use case
- ✅ List Report pattern
- ✅ Performance critical for large datasets

**Additional Config:** Enable condensed layout for desktop

---

### F5659 - Manage Commodity Counter Deal Request
**App Type:** List Report (Transactional/Draft)  
**Use Case:** Create and manage counter deal requests  
**Expected Data Volume:** Moderate (active requests being worked on)  
**Key Activities:** Edit requests, compare details, approve/reject  

**Recommended Table Type:** 🏆 **Grid Table**

**Rationale:**
- ✅ Need to compare multiple requests side-by-side
- ✅ Cell-level data (IDs, descriptions, amounts) is important
- ✅ Draft-enabled (multiple items in work)
- ✅ List Report pattern

**Additional Config:** Enable condensed layout for desktop

---

### F5665 - Monitor Hedge Constellation
**App Type:** List Report (Analytical/Monitoring)  
**Use Case:** Monitor hedge constellation status and effectiveness  
**Expected Data Volume:** Moderate to large  
**Key Activities:** Monitor status, compare constellations, analyze effectiveness  

**Recommended Table Type:** 🏆 **Grid Table** or **Analytical Table**

**Rationale:**
- ✅ Monitoring/analytical use case
- ✅ Comparison critical
- ✅ May need aggregations (consider Analytical Table)
- ✅ Performance for large datasets

**Decision Point:** 
- Use **Grid Table** if no aggregations needed
- Use **Analytical Table** if grouping/totals needed

**Additional Config:** Enable condensed layout for desktop

---

### F5666 - Manage Hedge Constellations Worklist
**App Type:** Worklist  
**Use Case:** Work through hedge constellations requiring attention  
**Expected Data Volume:** Small to moderate (active worklist items)  
**Key Activities:** Process worklist items, update status, navigate to details  

**Recommended Table Type:** 🔄 **Depends on Volume**

**Option A - Grid Table** (if >200 items possible):
- ✅ Worklists can grow large
- ✅ Quick scanning and comparison
- ✅ Better performance

**Option B - Responsive Table** (if <200 items guaranteed):
- ✅ Focus on individual worklist items
- ✅ May have visual indicators (status, priority)
- ✅ More flexible layout

**Recommendation:** 🏆 **Grid Table** (safer choice, better scalability)

**Additional Config:** Enable condensed layout for desktop

---

## Complete Change Checklist (When Changing Table Type)

**When you change from Responsive Table to Grid Table (or vice versa), you MUST check/update:**

### 🎯 Mandatory Updates

| # | Component | What to Check/Update | Impact if Skipped |
|---|-----------|---------------------|-------------------|
| 1 | **manifest.json** | Change `responsiveTable: true` to `gridTable: true` | Table won't change |
| 2 | **manifest.json** | Add `condensedTableLayout: true` (Grid Table only) | Poor information density |
| 3 | **Custom Fragments** | Update column type (`sap.m.Column` → `sap.ui.table.Column`) | Runtime errors |
| 4 | **Custom Data** | Move `sortProperty`/`filterProperty` (RT uses custom data, GT uses attributes) | Sorting/filtering broken |
| 5 | **Column IDs** | Reuse same stable IDs for columns | User variants lost |
| 6 | **Controller Code** | Update selection API (`getSelectedItems()` → `getSelectedIndices()`) | Actions break |
| 7 | **Controller Code** | Update column manipulation API | Dynamic features break |
| 8 | **Extension Columns** | Update manifest extension point name | Custom columns don't appear |
| 9 | **Cell Controls** | Verify all controls support Grid Table | Display issues |
| 10 | **Condensed Layout** | Verify all controls support condensed mode | Layout breaks |

### 📋 Verification Checklist

Before deploying table type change:

**Backend/Configuration:**
- [ ] Manifest.json updated with correct table type
- [ ] Condensed layout enabled (if Grid Table + no unsupported controls)
- [ ] Custom fragments updated (if present)
- [ ] Extension column manifest settings updated (if present)

**Custom Code:**
- [ ] Controller selection API updated
- [ ] Controller column manipulation API updated
- [ ] All table-related controller code reviewed
- [ ] Stable IDs preserved for all columns

**Controls & Content:**
- [ ] All cell controls are allowed in Grid Table
- [ ] All cell controls support condensed mode (if enabled)
- [ ] Single-line content used (no variable row heights)
- [ ] No ProgressIndicator or other unsupported controls

**Testing:**
- [ ] Table loads with data
- [ ] Selection works (single and multi-select)
- [ ] Sorting works (all columns)
- [ ] Filtering works (all filters)
- [ ] Actions work (row and table actions)
- [ ] Navigation works
- [ ] User variants still work
- [ ] System variants still work
- [ ] Export to Excel works
- [ ] Personalization persists
- [ ] Performance is acceptable with max data volume

---

## Implementation Guide

### Step 1: Check Current Table Type

#### Location in Code
**File:** `manifest.json` (in webapp folder or root)  
**Path:** `sap.ui.generic.app` → `pages` → `[PageName]` → `component` → `settings`

#### Example: Grid Table Configuration
```json
{
  "sap.ui.generic.app": {
    "pages": [
      {
        "entitySet": "C_YourEntitySet",
        "component": {
          "name": "sap.suite.ui.generic.template.ListReport",
          "list": true,
          "settings": {
            "gridTable": true,
            "condensedTableLayout": true,
            "multiSelect": true
          }
        }
      }
    ]
  }
}
```

#### Example: Responsive Table Configuration
```json
{
  "sap.ui.generic.app": {
    "pages": [
      {
        "entitySet": "C_YourEntitySet",
        "component": {
          "name": "sap.suite.ui.generic.template.ListReport",
          "list": true,
          "settings": {
            "responsiveTable": true,
            "multiSelect": true
          }
        }
      }
    ]
  }
}
```

---

### Step 2: Identify Current Configuration

**What to Look For:**

| Property | Table Type |
|----------|-----------|
| `"gridTable": true` | Grid Table ✅ |
| `"responsiveTable": true` | Responsive Table ✅ |
| `"analyticalTable": true` | Analytical Table ✅ |
| `"treeTable": true` | Tree Table ✅ |
| None specified | Default (usually Responsive) ⚠️ |

**Check for Condensed Layout:**
```json
"condensedTableLayout": true  // Should be true for Grid Tables on List Reports
```

---

### Step 3: Make Changes (If Needed)

#### Change from Responsive to Grid Table

**BEFORE:**
```json
"settings": {
  "responsiveTable": true,
  "multiSelect": true
}
```

**AFTER:**
```json
"settings": {
  "gridTable": true,
  "condensedTableLayout": true,
  "multiSelect": true
}
```

**Impact:** 
- Table renders as Grid Table (sap.ui.table.Table)
- Better performance for large datasets
- Condensed layout improves information density
- May affect custom column definitions

---

#### Enable Condensed Layout (If Missing)

**BEFORE:**
```json
"settings": {
  "gridTable": true,
  "multiSelect": true
}
```

**AFTER:**
```json
"settings": {
  "gridTable": true,
  "condensedTableLayout": true,
  "multiSelect": true
}
```

**Impact:**
- Reduces row height on desktop devices
- More rows visible per screen
- Better information density
- No impact on mobile (automatically adapts)

---

### Step 4: Handle Custom Fragments and Extensions (If Present)

#### ⚠️ Critical: Changing Table Type Requires Multiple Updates

When changing table type, you MUST update:
1. ✅ Manifest.json table settings
2. ✅ Custom column fragments (XML)
3. ✅ Controller code (JavaScript)
4. ✅ Extension column definitions
5. ✅ Custom data properties

#### A. Custom Column Fragments - API Differences

**Grid Table uses:** `sap.ui.table.Column`  
**Responsive Table uses:** `sap.m.Column`

##### Grid Table Column (sap.ui.table.Column)
```xml
<table:Column xmlns:table="sap.ui.table" 
              sortProperty="ProductID" 
              filterProperty="ProductID">
  <Label text="Product"/>
  <table:template>
    <Text text="{ProductID}"/>
  </table:template>
</table:Column>
```

**Key Features:**
- Built-in `sortProperty` and `filterProperty` attributes
- Uses `<table:template>` for cell content
- Single control per cell (keep single-line for consistent row height)

##### Responsive Table Column (sap.m.Column)
```xml
<Column xmlns="sap.m">
  <customData>
    <core:CustomData key="p13nData" 
                     value='\{"columnKey": "ProductID", 
                             "sortProperty": "ProductID", 
                             "filterProperty": "ProductID"}'/>
  </customData>
  <Text text="Product"/>
  <ColumnListItem>
    <Text text="{ProductID}"/>
  </ColumnListItem>
</Column>
```

**Key Features:**
- `sortProperty` and `filterProperty` provided as **custom data**
- Uses `<ColumnListItem>` for cell content
- Can have multiple controls per cell (flexible layout)

#### B. Custom Data Differences (Critical!)

##### Grid Table Custom Data
```xml
<table:Column>
  <table:customData>
    <core:CustomData key="p13nData"
      value='\{"columnKey": "DCS_ID", 
              "leadingProperty": "CmmdtyHedgePlanExposureDCSID", 
              "columnIndex": "2"}'/>
  </table:customData>
</table:Column>
```

**Note:** `sortProperty` and `filterProperty` are direct attributes on `<table:Column>`

##### Responsive Table Custom Data
```xml
<Column>
  <customData>
    <core:CustomData key="p13nData"
      value='\{"columnKey": "DCS_ID", 
              "leadingProperty": "CmmdtyHedgePlanExposureDCSID",
              "sortProperty": "CmmdtyHedgePlanExposureDCSID",
              "filterProperty": "CmmdtyHedgePlanExposureDCSID",
              "columnIndex": "2"}'/>
  </customData>
</Column>
```

**Note:** `sortProperty` and `filterProperty` MUST be in custom data for Responsive Table

---

#### C. Controller Code - API Differences

##### Selection API Differences

**Grid Table Selection:**
```javascript
// Get selected items
var oTable = this.byId("myGridTable");
var aSelectedIndices = oTable.getSelectedIndices();
var aSelectedContexts = aSelectedIndices.map(function(iIndex) {
    return oTable.getContextByIndex(iIndex);
});

// Get selected item count
var iSelectedCount = oTable.getSelectedIndices().length;

// Clear selection
oTable.clearSelection();

// Set selection
oTable.setSelectedIndex(5);
oTable.addSelectionInterval(0, 3); // Select rows 0-3
```

**Responsive Table Selection:**
```javascript
// Get selected items
var oTable = this.byId("myResponsiveTable");
var aSelectedItems = oTable.getSelectedItems();
var aSelectedContexts = aSelectedItems.map(function(oItem) {
    return oItem.getBindingContext();
});

// Get selected item count
var iSelectedCount = oTable.getSelectedItems().length;

// Clear selection
oTable.removeSelections(true);

// Set selection
var oItem = oTable.getItems()[5];
oTable.setSelectedItem(oItem);
```

##### Column Header Manipulation

**Grid Table:**
```javascript
// Get column
var oColumn = oTable.getColumns()[0];

// Change header text
oColumn.getLabel().setText("New Header");

// Show/hide column
oColumn.setVisible(false);

// Get sort property
var sSortProperty = oColumn.getSortProperty();
```

**Responsive Table:**
```javascript
// Get column
var oColumn = oTable.getColumns()[0];

// Change header text
oColumn.getHeader().setText("New Header");

// Show/hide column
oColumn.setVisible(false);

// Get sort property (from custom data)
var oCustomData = oColumn.data("p13nData");
var sSortProperty = oCustomData ? JSON.parse(oCustomData).sortProperty : "";
```

##### Cell Visibility/Content Manipulation

**Grid Table:**
```javascript
// Cells are in templates, update via binding
var oTemplate = oColumn.getTemplate();
oTemplate.setVisible("{= ${Status} === 'Active' }");
```

**Responsive Table:**
```javascript
// Cells are in ColumnListItems
var aItems = oTable.getItems();
aItems.forEach(function(oItem) {
    var oCells = oItem.getCells();
    oCells[0].setVisible(true);
});
```

---

#### D. Extension Columns - Manifest Differences (Fiori Elements V2)

##### Grid Table Extension Columns (Manifest)
```json
{
  "sap.ui.generic.app": {
    "pages": [
      {
        "entitySet": "C_YourEntitySet",
        "component": {
          "settings": {
            "gridTable": true,
            "tableSettings": {
              "type": "GridTable"
            }
          }
        },
        "pages": []
      }
    ]
  },
  "extends": {
    "extensions": {
      "sap.ui.controllerExtensions": {},
      "sap.ui.viewExtensions": {
        "sap.suite.ui.generic.template.ListReport.view.ListReport": {
          "ResponsiveTableColumnsExtension|C_YourEntitySet": {
            "type": "XML",
            "className": "sap.ui.core.Fragment",
            "fragmentName": "your.app.ext.fragment.CustomColumns"
          }
        }
      }
    }
  }
}
```

##### Responsive Table Extension Columns (Manifest)
```json
{
  "extends": {
    "extensions": {
      "sap.ui.viewExtensions": {
        "sap.suite.ui.generic.template.ListReport.view.ListReport": {
          "ResponsiveTableColumnsExtension|C_YourEntitySet": {
            "type": "XML",
            "className": "sap.ui.core.Fragment",
            "fragmentName": "your.app.ext.fragment.CustomColumns"
          }
        }
      }
    }
  }
}
```

**Note:** Extension point names differ between table types!

---

#### E. Stable IDs - Reuse for Extension Columns

**Important:** When updating custom columns, reuse the same stable IDs to preserve:
- User personalizations
- Saved variants
- Column order settings

**Example:**
```xml
<!-- OLD: Responsive Table -->
<Column id="myCustomColumn_DCS">
  <customData>
    <core:CustomData key="p13nData" value='\{"columnKey": "DCS_ID"}'/>
  </customData>
</Column>

<!-- NEW: Grid Table - REUSE SAME ID -->
<table:Column id="myCustomColumn_DCS">
  <table:customData>
    <core:CustomData key="p13nData" value='\{"columnKey": "DCS_ID"}'/>
  </table:customData>
</table:Column>
```

---

#### F. Allowed Controls in Grid Table

Grid Table supports most common controls, but has restrictions:

##### ✅ Allowed Controls (Examples)
- `sap.m.Text` (recommended - use `wrapping: false` for single-line)
- `sap.m.ObjectIdentifier`
- `sap.m.ObjectStatus`
- `sap.m.Link`
- `sap.m.Input` (for editable tables)
- `sap.m.CheckBox`
- `sap.m.Button`
- `sap.m.Icon`
- `sap.m.Label`
- `sap.m.HBox` / `sap.m.VBox` (simple layouts)

##### ⚠️ Restricted/Not Recommended
- Complex layouts (multiple lines)
- `sap.m.ProgressIndicator` (doesn't support condensed mode)
- Heavy controls (charts, complex forms)
- Controls that cause variable row heights

##### 🔍 Full List of Allowed Controls
See SAP Documentation: [Allowed Controls in Grid Table](https://ui5.sap.com/#/topic/148892ff9aea4a18b912829791e38f3e)

##### Best Practice: Single-Line Content
```xml
<!-- ✅ GOOD: Single-line, consistent row height -->
<table:Column>
  <table:template>
    <Text text="{ProductID}" wrapping="false"/>
  </table:template>
</table:Column>

<!-- ❌ BAD: Multi-line, variable row height -->
<table:Column>
  <table:template>
    <VBox>
      <Text text="{ProductID}"/>
      <Text text="{ProductName}"/>
    </VBox>
  </table:template>
</table:Column>
```

---

#### G. Variant Compatibility

**Good News:** User variants usually continue to work after table type changes! ✅

**Why?**
- Variants store column keys, not table-specific properties
- As long as column keys remain stable, variants are preserved

**Ensure Compatibility:**
1. ✅ Reuse stable IDs for columns
2. ✅ Keep same `columnKey` in custom data
3. ✅ Keep same field names/entity properties

**Test After Change:**
- Load existing variants (system and user variants)
- Verify columns appear correctly
- Verify filters still work
- Verify sort orders still work

---

#### H. Condensed Table Layout - Control Support Requirements

##### When to Enable Condensed Layout

**Enable `condensedTableLayout: true` for Grid Table on List Reports IF:**
- ✅ All controls in the table support condensed mode
- ✅ Desktop is primary use case
- ✅ Information density is important

##### Controls That DON'T Support Condensed Mode

⚠️ **Do NOT enable condensed layout if your table contains:**
- `sap.m.ProgressIndicator` ❌
- `sap.m.RatingIndicator` ❌
- `sap.suite.ui.microchart.*` (some chart types) ⚠️
- Complex custom controls (check documentation) ⚠️

##### Controls That DO Support Condensed Mode

✅ **Safe to use with condensed layout:**
- `sap.m.Text`
- `sap.m.Link`
- `sap.m.ObjectIdentifier`
- `sap.m.ObjectStatus`
- `sap.m.ObjectNumber`
- `sap.m.Input`
- `sap.m.CheckBox`
- `sap.m.Button`
- `sap.m.Icon`
- `sap.m.Label`

##### How Condensed Layout Works

**Desktop (Default):**
```json
"condensedTableLayout": true
```
- Applies `.sapUiSizeCompact` CSS class
- Reduces row height
- Reduces padding
- More rows visible per screen
- Better information density

**Mobile/Tablet:**
- Condensed layout is **automatically disabled**
- Uses comfortable/cozy mode regardless of setting
- Touch-friendly spacing maintained

##### Check Before Enabling

**Step 1:** Review all columns in your table

**Step 2:** Identify controls used in each column
```xml
<table:Column>
  <table:template>
    <!-- What control is here? -->
    <Text text="{field}"/>  <!-- ✅ Supports condensed -->
  </table:template>
</table:Column>
```

**Step 3:** Verify all controls support condensed mode

**Step 4:** If any control doesn't support condensed mode:
- **Option A:** Remove that control (use alternative)
- **Option B:** Don't enable condensed layout
- **Option C:** Use extension column to conditionally show control based on device

##### Example: Handling Progress Indicator

**❌ BAD: Progress Indicator with Condensed Layout**
```json
// manifest.json
"condensedTableLayout": true  // Will break ProgressIndicator display
```

```xml
<!-- Column with ProgressIndicator -->
<table:Column>
  <table:template>
    <ProgressIndicator percentValue="{CompletionPercent}"/>
  </table:template>
</table:Column>
```

**✅ GOOD: Use ObjectStatus Instead**
```xml
<table:Column>
  <table:template>
    <ObjectStatus 
      text="{CompletionPercent}%" 
      state="{= ${CompletionPercent} >= 100 ? 'Success' : 'Warning'}"/>
  </table:template>
</table:Column>
```

##### Reference Documentation

See SAP Fiori Design Guidelines:
- [Condensed Table Layout](https://experience.sap.com/fiori-design-web/grid-table/#condensed-table-layout)
- [Content Density](https://experience.sap.com/fiori-design-web/content-density/)

---

### Step 5: Testing Checklist

#### Functional Testing
- [ ] Table loads correctly with data
- [ ] Sorting works (all columns)
- [ ] Filtering works (all filter fields)
- [ ] Column reordering works (personalization)
- [ ] Column resizing works
- [ ] Multi-select works (if enabled)
- [ ] Actions work (row actions, table toolbar actions)
- [ ] Navigation works (click to navigate)
- [ ] Export to Excel works
- [ ] Table settings/personalization persists

#### Performance Testing
- [ ] Test with maximum expected data volume
- [ ] Scrolling is smooth (vertical and horizontal)
- [ ] Initial load time is acceptable
- [ ] Filtering/sorting response time is good

#### Responsive Testing
- [ ] Desktop: Condensed layout active (Grid Table only)
- [ ] Tablet: Layout adapts correctly
- [ ] Mobile: Layout adapts correctly (may switch to Responsive automatically)

#### Visual Testing
- [ ] Information density is appropriate
- [ ] Columns are readable (not too narrow)
- [ ] Text doesn't wrap inappropriately
- [ ] Cell content is properly aligned
- [ ] Icons and indicators display correctly

#### Integration Testing
- [ ] Custom columns still work (if present)
- [ ] Custom actions still work (if present)
- [ ] Variant management works
- [ ] Personalization works
- [ ] Draft handling works (if applicable)

---

## Common Issues and Solutions

### Issue: Table Doesn't Change After Manifest Update

**Cause:** Browser cache or CDN cache  
**Solution:**
1. Clear browser cache (Ctrl+Shift+Del)
2. Hard refresh (Ctrl+F5)
3. Clear app cache in Fiori Launchpad
4. Check CDN cache refresh time

---

### Issue: Custom Columns Break After Changing Table Type

**Cause:** Custom fragments use wrong column type  
**Solution:**
1. Identify custom fragments in manifest.json
2. Update fragments to use correct column type
3. Test thoroughly

**Grid Table Namespace:**
```xml
xmlns:table="sap.ui.table"
<table:Column>...</table:Column>
```

**Responsive Table Namespace:**
```xml
xmlns="sap.m"
<Column>...</Column>
```

---

### Issue: Condensed Layout Not Applied

**Possible Causes:**
1. `condensedTableLayout: true` not set in manifest
2. Custom CSS overriding layout
3. Theme doesn't support condensed layout (older themes)

**Solution:**
1. Verify manifest setting: `"condensedTableLayout": true`
2. Check for custom CSS: Search for `.sapUiSizeCompact` or table-related CSS
3. Verify theme version: Should be using current SAP theme

---

### Issue: Performance Worse After Changing to Grid Table

**Possible Causes:**
1. Too many columns defined
2. Complex cell templates
3. Too many visible rows configured

**Solution:**
1. Optimize columns: Hide less important columns by default
2. Simplify cell templates: Avoid complex formatters
3. Adjust visible rows: Default is usually optimal (20-50)

---

## Configuration Reference

### Grid Table - Full Configuration Example

```json
{
  "sap.ui.generic.app": {
    "pages": [
      {
        "entitySet": "C_CommodityTradeOrder",
        "component": {
          "name": "sap.suite.ui.generic.template.ListReport",
          "list": true,
          "settings": {
            "gridTable": true,
            "condensedTableLayout": true,
            "multiSelect": true,
            "tableSettings": {
              "type": "GridTable",
              "selectAll": true,
              "selectionLimit": 200
            },
            "dataLoadSettings": {
              "loadDataOnAppLaunch": "ifAnyFilterExist"
            }
          }
        }
      }
    ]
  }
}
```

### Responsive Table - Full Configuration Example

```json
{
  "sap.ui.generic.app": {
    "pages": [
      {
        "entitySet": "C_HedgeConstellationWorklist",
        "component": {
          "name": "sap.suite.ui.generic.template.ListReport",
          "list": true,
          "settings": {
            "responsiveTable": true,
            "multiSelect": false,
            "tableSettings": {
              "type": "ResponsiveTable",
              "mode": "SingleSelectMaster"
            },
            "dataLoadSettings": {
              "loadDataOnAppLaunch": "always"
            }
          }
        }
      }
    ]
  }
}
```

---

## Best Practices

### ✅ Do's

1. **Always enable condensed layout for Grid Tables on List Reports**
   ```json
   "condensedTableLayout": true
   ```

2. **Use Grid Table for List Reports with large datasets**
   - Default choice for List Report Pages
   - Better performance and usability

3. **Test with realistic data volumes**
   - Don't test with 10 rows if production has 1000
   - Performance characteristics change with scale

4. **Consider mobile use cases**
   - Grid Table adapts to mobile but isn't optimized for it
   - If mobile is primary, consider Responsive Table

5. **Document the decision**
   - Why did you choose this table type?
   - What are the expected data volumes?
   - What are the key use cases?

---

### ❌ Don'ts

1. **Don't use Responsive Table for large datasets (>200 items)**
   - Performance degrades significantly
   - User experience suffers

2. **Don't use Grid Table without condensed layout on desktop**
   - Wastes screen space
   - Poor information density

3. **Don't change table type without testing thoroughly**
   - May break custom fragments
   - May affect personalization
   - May impact performance differently

4. **Don't ignore mobile requirements**
   - Grid Table works on mobile but isn't optimal
   - Consider your user base

5. **Don't forget to update custom fragments**
   - Column types differ between table types
   - Will cause runtime errors

---

## Implementation Checklist (Per App)

### Phase 1: Analysis
- [ ] Open `manifest.json`
- [ ] Identify current table type
- [ ] Check if condensed layout is enabled (Grid Table only)
- [ ] Review expected data volumes
- [ ] Review key use cases
- [ ] Check for custom column fragments

### Phase 2: Decision
- [ ] Determine correct table type based on criteria
- [ ] Decide if change is needed
- [ ] Document rationale
- [ ] Get team approval (if significant change)

### Phase 3: Implementation
- [ ] Update `manifest.json` with correct table type
- [ ] Enable `condensedTableLayout` if Grid Table
- [ ] Update custom fragments if needed
- [ ] Save changes
- [ ] Build/deploy to DEV

### Phase 4: Testing
- [ ] Complete functional testing checklist
- [ ] Complete performance testing checklist
- [ ] Complete responsive testing checklist
- [ ] Complete visual testing checklist
- [ ] Complete integration testing checklist

### Phase 5: Documentation
- [ ] Document changes made
- [ ] Document test results
- [ ] Update tracker
- [ ] Create release notes (if applicable)

### Phase 6: Deployment
- [ ] Deploy to QA
- [ ] QA testing
- [ ] Deploy to PRD (after approval)
- [ ] Monitor for issues

---

## Quick Reference: Table Type Selection

```
                    START
                      |
        Is it a List Report Page?
                      |
            Yes ─────┴───── No (Object Page)
             |                     |
    How many items possible?    Need flexible
             |                  visual content?
         >200   <200              |
          |       |            Yes ─── Responsive
     Grid Table  |             |
          |      |            No ─── Grid or Responsive
          |      |                   (both work)
    Need aggregations/
    grouping?
          |
      Yes ─── Analytical Table
          |
      No ─── Grid Table
          |
    Enable condensedTableLayout: true
```

---

## Next Steps

1. **Identify Current State:** Check manifest.json for all 5 apps
2. **Create App Matrix:** Document current vs. recommended table type
3. **Prioritize Changes:** Which apps need changes most urgently?
4. **Plan Implementation:** Schedule DEV → QA → PRD rollout
5. **Execute:** Change one app at a time, test thoroughly
6. **Monitor:** Watch for issues after deployment

---

## Summary Recommendation for Your Apps

| App | Recommended Type | Rationale | Priority |
|-----|-----------------|-----------|----------|
| F5658 - Trader's Cockpit | Grid Table + Condensed | Large dataset, comparison critical | High |
| F5655 - Observe & Monitor | Grid Table + Condensed | Monitoring, large dataset | High |
| F5659 - Counter Deal Request | Grid Table + Condensed | Comparison, cell-level work | High |
| F5665 - Monitor Constellation | Grid Table or Analytical + Condensed | Monitoring, may need aggregations | High |
| F5666 - Worklist | Grid Table + Condensed | Scalability, better safe than sorry | Medium |

**All apps should likely use Grid Table with condensed layout enabled.**

This is the most common and appropriate choice for commodity trading applications with large datasets and comparison requirements.

---

**Ready to check your apps?** Let me know when you want to start the analysis! 🚀
