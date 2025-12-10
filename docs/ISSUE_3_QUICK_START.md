# Issue 3: Context Menu - Quick Start Guide
**For:** F5658, F5655, F5659, F5665, F5666  
**Estimated Time:** 1-2 hours per app

---

## 🎯 Quick Check (Do This First - 5 min per app)

### 1. Test Context Menu

**Open app → Right-click on table row**

✅ **Expected:** Context menu appears  
❌ **Problem:** No context menu

### 2. Check Actions Appear

**Identify toolbar actions → Right-click row**

✅ **Expected:** All selection-dependent actions in menu  
❌ **Problem:** Actions missing

### 3. Test "Open in New Tab"

**Right-click row → Look for option**

✅ **Expected:** "Open in New Tab or Window" at bottom  
❌ **Problem:** Option missing

### 4. Test Correct Row (CRITICAL!)

**Steps:**
1. Select row 1 and row 2
2. Right-click on row 3 (NOT selected)
3. Choose an action

✅ **Expected:** Action processes row 3 ONLY  
❌ **Problem:** Action processes rows 1 and 2 (WRONG!)

**If ❌ Problem occurs:** You have a violation! 🚨

---

## 🚨 Common Violations (What to Look For)

### Violation 1: Using Native Table APIs

**Search controller for:**
```javascript
.getSelectedItems()      // ❌ BAD
.getSelectedIndices()    // ❌ BAD
.getSelectedContexts()   // ❌ BAD (if not from extensionAPI)
```

**Should be:**
```javascript
this.extensionAPI.getSelectedContexts()  // ✅ GOOD
```

---

### Violation 2: Actions Not in Manifest

**Check controller for:**
```javascript
new Button({...})        // ❌ BAD - dynamic button
oToolbar.addContent(...) // ❌ BAD - adding to toolbar
```

**Should be in:**
```json
// manifest.json → Actions section
"Actions": {
  "MyAction": {
    "requiresSelection": true
  }
}
```

---

### Violation 3: Manual Enable/Disable

**Search controller for:**
```javascript
oButton.setEnabled(...)   // ❌ BAD - manual control
oButton.setVisible(...)   // ❌ BAD - hiding actions
```

**Should be:**
```json
// manifest.json
"applicablePath": "IsEditable"  // ✅ GOOD
```

---

## ⚡ Quick Assessment Per App

### F5658 - Trader's Cockpit

**Known Actions:**
- Swap (requiresSelection: true)
- Forward (requiresSelection: true)
- Reload (requiresSelection: true)

**Quick Test:**
- [ ] Right-click → All 3 actions appear?
- [ ] Right-click → "Open in New Tab" appears?
- [ ] Test Scenario 4 (select rows 1-2, right-click row 3)
- [ ] Check `ListReportExt.controller.js` for violations

**Estimated Risk:** Medium (has custom `enabled` binding)

---

### F5655 - Observe & Monitor

**Quick Test:**
- [ ] Right-click → Context menu appears?
- [ ] Right-click → "Open in New Tab" appears?
- [ ] Check `ListReportExt.controller.js` for violations

**Estimated Risk:** Low (likely no custom actions)

---

### F5659 - Counter Deal Request

**Quick Test:**
- [ ] Right-click → Context menu appears?
- [ ] Right-click → "Open in New Tab" appears?
- [ ] Test with Draft entities
- [ ] Check `ListReportExt.controller.js` for violations

**Estimated Risk:** Low to Medium

---

### F5665 - Monitor Constellation

**Quick Test:**
- [ ] Right-click → Context menu appears?
- [ ] Right-click → "Open in New Tab" appears?
- [ ] Easy to test (only 17 rows)
- [ ] Check `ListReportExt.controller.js` for violations

**Estimated Risk:** Low

---

### F5666 - Worklist

**Quick Test:**
- [ ] Right-click → Context menu appears?
- [ ] Right-click → "Open in New Tab" appears?
- [ ] Check `ListReportExt.controller.js` for violations

**Estimated Risk:** Low

---

## 🛠️ Quick Fix Pattern

**If you find native API usage:**

### BEFORE (Wrong):
```javascript
onMyAction: function() {
    var oTable = this.byId("myTable");
    var aItems = oTable.getSelectedItems(); // ❌
    // Process items
}
```

### AFTER (Correct):
```javascript
onMyAction: function() {
    var aContexts = this.extensionAPI.getSelectedContexts(); // ✅
    // Process contexts
}
```

---

## 📊 Decision Tree

```
Does context menu appear?
├─ NO → Context menu not enabled (unlikely for FE apps)
│       Check if SmartTable is used
│
└─ YES → Do all actions appear?
    ├─ NO → Actions missing
    │       → Check manifest: requiresSelection: true?
    │       → Check controller: Actions added dynamically?
    │
    └─ YES → Test Scenario 4 (wrong row bug)
        ├─ FAIL → Using native APIs!
        │         → Fix: Use extensionAPI.getSelectedContexts()
        │
        └─ PASS → ✅ All good!
```

---

## 🚀 Recommended Workflow

### Day 1: Assessment (All Apps - 2-3 hours)

**For each app (30 min):**
1. Quick test (5 min)
2. Code review (15 min)
3. Document findings (10 min)

**Output:** List of apps with/without violations

---

### Day 2: Fix High-Risk Apps (2-4 hours)

**Priority:**
1. **F5658** (most complex, multiple actions)
2. Apps with violations found
3. Apps with no violations (just verify)

---

### Day 3: Testing (1-2 hours)

**Test all scenarios for each app:**
- Scenario 1: No selection + context menu
- Scenario 2: Selection + context menu on different row (CRITICAL)
- Scenario 3: Selection + context menu on selected row
- Scenario 4: "Open in New Tab"

---

## 📝 Quick Checklist Template

**Copy this for each app:**

```
App: [App Name]
Date: [Date]
Tester: [Your Name]

Context Menu Tests:
[ ] Context menu appears on right-click
[ ] All toolbar actions with requiresSelection appear
[ ] "Open in New Tab or Window" appears
[ ] Separator before "Open in New Tab"

Scenario Tests:
[ ] Scenario 1: No selection → Menu on row → Action executes for that row
[ ] Scenario 2: Select rows 1-2 → Menu on row 3 → Action executes for row 3 ONLY
[ ] Scenario 3: Select rows 1-3 → Menu on row 2 → Action executes for rows 1-3
[ ] Scenario 4: "Open in New Tab" → New tab opens → Correct data

Code Review:
[ ] No native table APIs (getSelectedItems, getSelectedIndices)
[ ] Using extensionAPI.getSelectedContexts()
[ ] All actions in manifest (not in controller)
[ ] Using applicablePath (not setEnabled)
[ ] No actions hidden (no setVisible)

Status: ✅ PASS / ❌ FAIL
Violations Found: [List violations]
Fix Required: YES / NO
Estimated Effort: [Hours]
```

---

## 💡 Pro Tips

1. **Start with visual test** - If context menu works correctly, you may have no violations!
2. **Scenario 2 is critical** - This is where wrong-row bug appears
3. **Search for "getSelected"** - Quick way to find violations in code
4. **Test with real data** - Don't test with empty tables
5. **All your apps are FE V2** - Same guidelines apply to all

---

## 📚 Full Documentation

**Detailed Guide:** `/workspace/docs/ISSUE_3_CONTEXT_MENU_GUIDE.md`

**Contains:**
- Complete problem description
- All violation types with examples
- Code examples (correct and wrong)
- App-specific assessment plans
- Testing scenarios
- Troubleshooting guide

---

**Ready to start testing?** Pick an app and run the 5-minute quick test! 🔍
