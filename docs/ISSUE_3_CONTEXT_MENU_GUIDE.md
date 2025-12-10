# Issue 3: Context Menu - Implementation Guide
**Date:** December 8, 2025  
**Apps:** F5658, F5655, F5659, F5665, F5666  
**Priority:** Medium  
**App Type:** Fiori Elements (All 5 apps)

---

## Problem Summary

### Common Issues with Context Menu

#### ❌ Issue 1: Wrong Rows Processed
**Problem:**
- Context menu action executed for **previously selected rows**, not the row where context menu was opened
- Example: Right-click on row 3, but action executes on rows 1 and 2 (previously selected)

**Impact:** High - Data integrity risk, user confusion, incorrect operations

---

#### ❌ Issue 2: Missing Actions
**Problem:**
- Not all selection-dependent actions shown in context menu
- Actions available in toolbar but missing from context menu
- Inconsistent UX across apps

**Impact:** Medium - Reduced efficiency, user frustration

---

#### ❌ Issue 3: No Context Menu
**Problem:**
- Context menu not available at all
- Despite having selection-dependent actions or navigation to detail page
- Users forced to use toolbar only

**Impact:** Medium - Poor UX, reduced productivity

---

## Expected Behavior (SAP Standard)

### ✅ Context Menu Should Offer:

1. **All toolbar actions requiring selection**
   - Single-select actions
   - Multi-select actions
   - In same order as toolbar

2. **"Open in New Tab or Window"** (if navigation possible)
   - Separator line before this option
   - Always at the bottom of menu

3. **Correct Row Context**
   - Execute for row where menu opened
   - OR execute for all selected rows (if menu row is selected)

---

## Solution for Fiori Elements Apps (All Your Apps)

### ✅ Good News: Built-In Support!

**Fiori Elements V2 supports context menu out of the box!** 🎉

All your apps use Smart Template (Fiori Elements V2), so context menu functionality is **automatically available** if you follow the programming guidelines.

---

## Assessment Checklist (For Each App)

### Step 1: Verify Context Menu Availability

**Test in DEV/QA:**
1. Open app List Report
2. Right-click on a table row
3. **Expected:** Context menu appears ✅
4. **If not:** Context menu missing ❌

### Step 2: Verify Actions Appear

**Check:**
1. Identify all toolbar actions with `requiresSelection: true`
2. Right-click on a row
3. **Expected:** All selection-dependent actions appear in context menu ✅
4. **If not:** Actions missing from context menu ❌

### Step 3: Verify "Open in New Tab"

**Check:**
1. Right-click on a row
2. **Expected:** "Open in New Tab or Window" option appears ✅
3. Click it
4. **Expected:** Object page opens in new tab ✅

### Step 4: Verify Correct Row Execution

**Test Scenario 1: Menu Row Not Selected**
1. Select row 1 and row 2
2. Right-click on row 3 (not selected)
3. Choose action from context menu
4. **Expected:** Action executes ONLY for row 3 ✅
5. **Wrong behavior:** Action executes for rows 1 and 2 ❌

**Test Scenario 2: Menu Row Is Selected**
1. Select row 1, row 2, and row 3
2. Right-click on row 2 (is selected)
3. Choose action from context menu
4. **Expected:** Action executes for ALL selected rows (1, 2, 3) ✅

---

## Common Violations (What NOT to Do)

### ❌ Violation 1: Using Native Table APIs

**Wrong:**
```javascript
// BAD: Using native table API
onCustomAction: function() {
    var oTable = this.byId("myTable");
    var aSelectedItems = oTable.getSelectedItems(); // ❌ WRONG!
    // This returns selected rows, not context menu row
}
```

**Correct:**
```javascript
// GOOD: Using Fiori Elements extensionAPI
onCustomAction: function(oEvent) {
    var oExtensionAPI = this.extensionAPI;
    var aContexts = oExtensionAPI.getSelectedContexts(); // ✅ CORRECT!
    // This returns correct contexts based on context menu logic
}
```

**Why Wrong:**
- Native API returns previously selected items
- Doesn't know which row context menu was opened on
- Causes wrong rows to be processed

**Critical:** Must call `getSelectedContexts()` **while context menu is still open!**

---

### ❌ Violation 2: Actions Not in Manifest

**Wrong:**
```javascript
// BAD: Adding actions via controller code
onInit: function() {
    var oToolbar = this.byId("toolbar");
    oToolbar.addContent(new Button({
        text: "My Action",
        press: this.onMyAction.bind(this)
    })); // ❌ WRONG! Not in context menu
}
```

**Correct:**
```json
// GOOD: Actions defined in manifest.json
{
  "sap.ui.controllerExtensions": {
    "sap.suite.ui.generic.template.ListReport.view.ListReport": {
      "controllerName": "your.app.ext.controller.ListReportExt",
      "sap.ui.generic.app": {
        "YourEntitySet": {
          "EntitySet": "YourEntitySet",
          "Actions": {
            "MyAction": {
              "id": "myActionButton",
              "text": "{@i18n>myAction}",
              "press": "onMyAction",
              "requiresSelection": true // ✅ Will appear in context menu!
            }
          }
        }
      }
    }
  }
}
```

**Why Wrong:**
- Fiori Elements doesn't know about dynamically added actions
- Won't appear in context menu
- Not managed by FE lifecycle

---

### ❌ Violation 3: Controlling Enabled State via Code

**Wrong:**
```javascript
// BAD: Controlling enabled state via controller
onSelectionChange: function() {
    var oButton = this.byId("myActionButton");
    var aSelectedItems = this.getSelectedItems();
    oButton.setEnabled(aSelectedItems.length > 0); // ❌ WRONG!
}
```

**Correct (V2 - Your Apps):**
```json
{
  "Actions": {
    "MyAction": {
      "id": "myActionButton",
      "text": "{@i18n>myAction}",
      "press": "onMyAction",
      "requiresSelection": true,
      "applicablePath": "IsEditable" // ✅ CORRECT! Based on row data
    }
  }
}
```

**Correct (V4):**
```json
{
  "Actions": {
    "MyAction": {
      "enabled": "{= ${IsEditable} === true }" // ✅ CORRECT! Expression binding
    }
  }
}
```

**Why Wrong:**
- Manual enable/disable logic doesn't work with context menu
- Context menu doesn't trigger your custom logic
- Use declarative approach instead

---

### ❌ Violation 4: Setting Actions Invisible

**Wrong:**
```javascript
// BAD: Hiding actions via controller
onInit: function() {
    var oButton = this.byId("myActionButton");
    oButton.setVisible(false); // ❌ WRONG! Hidden from context menu too
}
```

**Correct:**
- Use `applicablePath` to control when action is enabled
- Don't hide actions - let them appear disabled if not applicable
- Consistent UX across all rows

---

### ❌ Violation 5: Excluding Actions from Context Menu

**Don't do this:**
```javascript
// BAD: Trying to exclude actions
// Some developers try to customize which actions appear
// DON'T! Context menu should show all relevant actions
```

**Why Wrong:**
- Inconsistent UX across Fiori apps
- Users expect all toolbar actions in context menu
- SAP guideline: show all selection-dependent actions

---

## Implementation Checklist (Per App)

### Phase 1: Assessment (15 min per app)

- [ ] Open app in DEV/QA
- [ ] Identify all custom actions in manifest
- [ ] Check if actions have `requiresSelection: true`
- [ ] Test context menu availability (right-click on row)
- [ ] Test if all actions appear in context menu
- [ ] Test "Open in New Tab" functionality
- [ ] Test correct row execution (Scenario 1 and 2)
- [ ] Document violations found

### Phase 2: Code Review (30 min per app)

**Check Controller Extensions:**
- [ ] Search for native table API usage: `getSelectedItems()`, `getSelectedIndices()`
- [ ] Verify using extensionAPI: `extensionAPI.getSelectedContexts()`
- [ ] Check if `getSelectedContexts()` called while menu open (not after popup)

**Check Manifest:**
- [ ] Verify all actions defined in manifest `Actions` section
- [ ] Check `requiresSelection: true` for selection-dependent actions
- [ ] Verify `applicablePath` used (not manual enable/disable)
- [ ] No actions set invisible via code

**Check Custom Code:**
- [ ] No dynamic button additions via controller
- [ ] No manual enable/disable logic in selectionChange handlers
- [ ] No visibility manipulation

### Phase 3: Fix Violations (Variable per app)

**For Each Violation Found:**
1. Document current behavior
2. Implement correct approach
3. Test in DEV
4. Verify context menu works correctly
5. Move to next violation

### Phase 4: Testing (30 min per app)

**Functional Testing:**
- [ ] Context menu appears on right-click
- [ ] All selection-dependent actions appear
- [ ] "Open in New Tab" appears
- [ ] Actions enabled/disabled correctly based on row data
- [ ] Separator before "Open in New Tab"

**Scenario Testing:**
- [ ] Test Scenario 1: Menu on unselected row → Action executes for that row only
- [ ] Test Scenario 2: Menu on selected row → Action executes for all selected rows
- [ ] Test with 0 selections, 1 selection, multiple selections
- [ ] Test with Draft vs. Active Entity (if applicable)

**Navigation Testing:**
- [ ] "Open in New Tab" opens Object Page in new tab
- [ ] URL is correct
- [ ] Object Page loads with correct data
- [ ] Can navigate back/forward in new tab

---

## App-Specific Assessment

### F5658 - Commodity Trader's Order Cockpit

**Custom Actions Found (from manifest):**
```json
"Actions": {
  "ActionC_CmmdtyHedgeTradeOrderCockpitSwap": {
    "requiresSelection": true
  },
  "ActionC_CmmdtyHedgeTradeOrderCockpitForward": {
    "requiresSelection": true,
    "enabled": "{appView>/createForwardButtonEnabled}"
  },
  "ActionC_CmmdtyHedgeTradeOrderCockpitReload": {
    "requiresSelection": true
  },
  "ActionC_CmmdtyHedgeTradeOrderCockpitResults1": {
    // No requiresSelection - won't appear in context menu
  }
}
```

**Assessment Checklist:**
- [ ] Verify Swap action appears in context menu
- [ ] Verify Forward action appears in context menu
- [ ] Verify Reload action appears in context menu
- [ ] Verify Results1 does NOT appear (no requiresSelection)
- [ ] Check controller: `ListReportExt.controller.js`
- [ ] Verify using `extensionAPI.getSelectedContexts()`
- [ ] Test correct row execution for each action

**Potential Risk:** Has custom `enabled` binding for Forward action - verify works with context menu

---

### F5655 - Observe and Monitor Deal Requests

**Custom Extensions:**
- Has `ListReportExt` controller
- Need to check for custom actions

**Assessment Checklist:**
- [ ] Identify custom actions (if any)
- [ ] Test context menu availability
- [ ] Test "Open in New Tab" (navigation to Object Page)
- [ ] Check controller code for violations

---

### F5659 - Manage Commodity Counter Deal Request

**Custom Actions Found (from previous knowledge):**
- Has custom actions on Object Page (Add, Reset, Create Counterdeal)
- List Report may have actions too

**Assessment Checklist:**
- [ ] Check List Report for custom actions
- [ ] Test context menu on List Report
- [ ] Verify "Open in New Tab" (navigation to Object Page)
- [ ] Check controller code for violations
- [ ] Note: Draft-enabled app - test with Draft entities

---

### F5665 - Monitor Hedge Constellation

**Custom Extensions:**
- Has `ListReportExt` controller
- Need to check for custom actions

**Assessment Checklist:**
- [ ] Identify custom actions (if any)
- [ ] Test context menu availability
- [ ] Small dataset (17 rows) - easy to test all scenarios
- [ ] Check controller code for violations

---

### F5666 - Manage Hedge Constellations Worklist

**Custom Extensions:**
- Has `ListReportExt` controller
- Has `ObjectPageExt` controller with Application Log action

**Assessment Checklist:**
- [ ] Identify List Report custom actions (if any)
- [ ] Test context menu availability
- [ ] Verify "Open in New Tab"
- [ ] Check controller code for violations

---

## Code Examples

### ✅ Correct: Using extensionAPI

```javascript
// Controller extension for Fiori Elements V2
onMyCustomAction: function(oEvent) {
    // Get the extension API
    var oExtensionAPI = this.extensionAPI;
    
    // Get selected contexts (works correctly with context menu!)
    var aSelectedContexts = oExtensionAPI.getSelectedContexts();
    
    if (aSelectedContexts.length === 0) {
        MessageBox.warning("Please select at least one item.");
        return;
    }
    
    // Process selected contexts
    aSelectedContexts.forEach(function(oContext) {
        var sPath = oContext.getPath();
        var oData = oContext.getObject();
        
        // Your action logic here
        console.log("Processing: " + oData.YourField);
    });
}
```

### ✅ Correct: Manifest Action Definition

```json
{
  "sap.ui.controllerExtensions": {
    "sap.suite.ui.generic.template.ListReport.view.ListReport": {
      "controllerName": "your.app.ext.controller.ListReportExt",
      "sap.ui.generic.app": {
        "YourEntitySet": {
          "EntitySet": "YourEntitySet",
          "Actions": {
            "LockAction": {
              "id": "lockButton",
              "text": "{@i18n>lock}",
              "press": "onLockAction",
              "requiresSelection": true,
              "applicablePath": "IsLockable"
            },
            "UnlockAction": {
              "id": "unlockButton",
              "text": "{@i18n>unlock}",
              "press": "onUnlockAction",
              "requiresSelection": true,
              "applicablePath": "IsUnlockable"
            }
          }
        }
      }
    }
  }
}
```

### ✅ Correct: Calling Function Import

```javascript
onLockAction: function(oEvent) {
    var oExtensionAPI = this.extensionAPI;
    var aSelectedContexts = oExtensionAPI.getSelectedContexts();
    
    if (aSelectedContexts.length === 0) {
        return;
    }
    
    // Call OData function import
    var oModel = this.getView().getModel();
    var that = this;
    
    aSelectedContexts.forEach(function(oContext) {
        var oData = oContext.getObject();
        
        oModel.callFunction("/LockEntity", {
            method: "POST",
            urlParameters: {
                EntityID: oData.EntityID
            },
            success: function(oData) {
                MessageToast.show("Locked successfully");
                that.extensionAPI.refresh();
            },
            error: function(oError) {
                MessageBox.error("Lock failed");
            }
        });
    });
}
```

---

## Testing Scenarios

### Scenario 1: No Selection, Context Menu on Row

**Steps:**
1. Clear all selections
2. Right-click on row 5
3. Choose action from context menu

**Expected:**
- ✅ Action executes for row 5 only
- ✅ Row 5 data is processed

**Common Failure:**
- ❌ Nothing happens (action thinks no rows selected)

---

### Scenario 2: Selection Exists, Context Menu on Different Row

**Steps:**
1. Select rows 2 and 3
2. Right-click on row 7 (NOT selected)
3. Choose action from context menu

**Expected:**
- ✅ Action executes for row 7 only
- ✅ Row 7 data is processed
- ✅ Rows 2 and 3 are NOT processed

**Common Failure:**
- ❌ Action executes for rows 2 and 3 (wrong!)

---

### Scenario 3: Selection Exists, Context Menu on Selected Row

**Steps:**
1. Select rows 2, 3, and 5
2. Right-click on row 3 (IS selected)
3. Choose action from context menu

**Expected:**
- ✅ Action executes for ALL selected rows (2, 3, 5)
- ✅ All three rows processed

**Common Failure:**
- ❌ Action executes only for row 3

---

### Scenario 4: Open in New Tab

**Steps:**
1. Right-click on any row
2. Choose "Open in New Tab or Window"

**Expected:**
- ✅ New browser tab opens
- ✅ Object Page displays
- ✅ Correct entity data shown
- ✅ URL contains correct entity key

**Common Failure:**
- ❌ Option missing from context menu
- ❌ Opens in same tab instead

---

## Troubleshooting

### Issue: Context Menu Not Appearing

**Possible Causes:**
1. Table not using Smart Table control
2. Browser security settings blocking context menu
3. Custom code preventing default context menu

**Solution:**
- Verify using `sap.ui.comp.smarttable.SmartTable`
- Check browser (right-click should work on other elements)
- Remove any `onContextMenu` or `onBeforeOpenContextMenu` handlers

---

### Issue: Actions Missing from Context Menu

**Possible Causes:**
1. Actions not defined in manifest
2. Missing `requiresSelection: true`
3. Actions added dynamically via controller

**Solution:**
- Move all actions to manifest.json `Actions` section
- Add `requiresSelection: true` to each action
- Remove dynamic button creation from controller

---

### Issue: Wrong Rows Processed

**Possible Causes:**
1. Using native table APIs (`getSelectedItems()`)
2. Not using extensionAPI
3. Calling `getSelectedContexts()` after opening popup

**Solution:**
- Replace native APIs with `extensionAPI.getSelectedContexts()`
- Call `getSelectedContexts()` immediately in action handler
- Don't open popup before getting contexts

---

### Issue: "Open in New Tab" Missing

**Possible Causes:**
1. No Object Page defined for entity
2. Navigation not configured in manifest

**Solution:**
- Verify Object Page defined in `pages` section of manifest
- Check navigation route exists

---

## Best Practices

### ✅ Do's

1. **Always use extensionAPI**
   ```javascript
   var aContexts = this.extensionAPI.getSelectedContexts();
   ```

2. **Define actions in manifest**
   ```json
   "Actions": { "MyAction": { ... } }
   ```

3. **Use applicablePath for enable/disable**
   ```json
   "applicablePath": "IsEditable"
   ```

4. **Test all scenarios**
   - No selection + context menu
   - Selection + context menu on different row
   - Selection + context menu on selected row

5. **Keep context menu open**
   - Get contexts before opening any popup
   - Don't close context menu before processing

---

### ❌ Don'ts

1. **Don't use native table APIs**
   ```javascript
   // BAD!
   var aItems = oTable.getSelectedItems();
   ```

2. **Don't add actions via controller**
   ```javascript
   // BAD!
   oToolbar.addContent(new Button(...));
   ```

3. **Don't manually control enabled state**
   ```javascript
   // BAD!
   oButton.setEnabled(true/false);
   ```

4. **Don't hide actions**
   ```javascript
   // BAD!
   oButton.setVisible(false);
   ```

5. **Don't exclude actions from context menu**
   - Show all selection-dependent actions
   - Consistent UX across all apps

---

## Summary

### For Fiori Elements Apps (All Your Apps):

**Good News:** 🎉
- Context menu support is **built-in**
- **Automatic** if you follow guidelines
- No manual setup required

**Your Tasks:**
1. ✅ Verify context menu works in each app
2. ✅ Check for programming guideline violations
3. ✅ Fix violations if found
4. ✅ Test thoroughly

**Estimated Effort:** 1-2 hours per app (assessment + fixes)

---

## Next Steps

1. **Start with F5658** (most complex, has multiple actions)
2. Test context menu in DEV/QA
3. Review controller code for violations
4. Fix violations if found
5. Test all scenarios
6. Move to next app

**Ready to start assessment?** 🚀
