# Issue 3: Context Menu - App-Specific Findings
**Date Started:** December 8, 2025  
**Status:** In Progress

---

## App Assessment Results

### ⚠️ F5658 - Commodity Trader's Order Cockpit

**Status:** ⚠️ DEFERRED - Requires Team Discussion  
**Date Assessed:** December 8, 2025

#### Findings:

**Architecture:**
- Uses **Tree Table** (not standard List Report grid table)
- Redirects to **SAP GUI** on row click (hybrid app)
- Actions: Create Swap, Create Forward, Refresh, Expand All
- Actions are grayed out when no selection

**Behavior:**
- Clicking on record → Redirects to SAP GUI
- Not standard Fiori Elements flow
- Likely hybrid Fiori/SAP GUI architecture

#### Assessment:

**Context Menu Relevance:** ⚠️ **UNCLEAR**

**Reasons for Deferral:**
1. **Tree Table** - Different from standard List Report tables
2. **SAP GUI Integration** - Hybrid architecture, not pure Fiori
3. **Different User Flow** - Click redirects to GUI, not standard navigation
4. **Unique Actions** - Create actions vs. standard row actions
5. **Grayed Out Logic** - May have custom enable/disable logic

#### Recommendation:

**DEFER for Team Discussion** 📋

**Questions for Team:**
1. Is this app intended to be pure Fiori or hybrid Fiori/GUI?
2. Is SAP GUI redirection intentional or legacy behavior?
3. Should users be able to right-click on tree items?
4. Are the "Create" actions row-specific or global?
5. Is context menu functionality desired for this app?
6. Should we modernize to pure Fiori first, then address context menu?

#### Potential Approaches (If Team Decides to Implement):

1. **Keep Hybrid Architecture:**
   - May not need context menu
   - SAP GUI handles row operations
   - Fiori handles high-level actions

2. **Modernize to Pure Fiori:**
   - Remove SAP GUI redirection
   - Implement standard Object Page
   - Then implement context menu per guidelines

3. **Add Context Menu to Tree Table:**
   - Different implementation than grid table
   - Would need custom coding
   - Higher effort

#### Next Steps:

- [ ] Present findings to team
- [ ] Team decision: Defer, Skip, or Implement?
- [ ] If Implement: Define requirements and approach
- [ ] If Skip: Document as "Not Applicable - Hybrid Architecture"

---

### 🔷 F5655 - Observe and Monitor Deal Requests

**Status:** 🔷 **HANDLED BY COLLEAGUE**  
**Date:** December 8, 2025

#### Note:
- This app is being handled by another team member
- Assessed for Issues 1 (Table Type) and 2 (Column Header Menu) only
- **Will be skipped for all future issues** (Issue 3 onwards)

#### Previous Assessment (Issues 1-2):
- ✅ Table Type: Grid Table with condensed layout - Optimal
- ✅ Column Header Menu: Investigated, documented

#### Future Issues:
- 🔷 Handled by colleague - No further assessment needed from this workstream

---

### 🔵 F5659 - Manage Commodity Counter Deal Request

**Status:** 🔵 Not Started  
**Date Assessed:** -

#### Known Info:
- Uses Grid Table with condensed layout ✅
- Draft-enabled transactional app
- Has custom column and cell extensions
- Standard navigation to Object Page expected
- Has multiple Object Page actions (Add, Reset, Create Counterdeal)

#### Assessment Plan:
1. Test context menu on List Report table
2. Test "Open in New Tab" functionality
3. Check List Report for custom actions
4. Review `ListReportExt` controller for violations
5. Test with Draft vs. Active entities
6. Test Scenario 2 (wrong row bug)

#### Expected Result:
- Context menu should work out of the box
- Need to verify if List Report has custom actions (we know Object Page does)

---

### 🔵 F5665 - Monitor Hedge Constellation

**Status:** 🔵 Not Started  
**Date Assessed:** -

#### Known Info:
- Uses Grid Table (no condensed - by design) ✅
- Small dataset (~17 rows) - Easy to test!
- Has `ListReportExt` controller
- Has custom column extensions
- Standard navigation to Object Page expected

#### Assessment Plan:
1. Test context menu availability
2. Test "Open in New Tab" functionality
3. Check for custom actions
4. Review controller for violations
5. Test Scenario 2 (wrong row bug)

#### Expected Result:
- Context menu should work out of the box
- Easy to test thoroughly (only 17 rows)
- Likely no custom actions (monitoring app)

---

### 🔵 F5666 - Manage Hedge Constellations Worklist

**Status:** 🔵 Not Started  
**Date Assessed:** -

#### Known Info:
- Uses Grid Table with condensed layout ✅
- Has `ListReportExt` and `ObjectPageExt` controllers
- Has custom column extensions
- Object Page has Application Log action
- Standard navigation to Object Page expected

#### Assessment Plan:
1. Test context menu on List Report
2. Test "Open in New Tab" functionality
3. Check for List Report custom actions
4. Review `ListReportExt` controller for violations
5. Test Scenario 2 (wrong row bug)

#### Expected Result:
- Context menu should work out of the box
- May have custom actions on List Report (to be determined)

---

## Summary

### Apps Assessed: 2 of 5 (1 Deferred, 1 Handled by Colleague)

| App | Status | Context Menu | Issues Found | Action Required |
|-----|--------|--------------|--------------|-----------------|
| F5658 | ⚠️ Deferred | Unknown | Hybrid architecture | Team discussion |
| F5655 | 🔷 Colleague | N/A | N/A | Skip for Issue 3+ |
| F5659 | 🔵 Pending | - | - | **Test next** |
| F5665 | 🔵 Pending | - | - | Test |
| F5666 | 🔵 Pending | - | - | Test |

### Scope for This Workstream:

**Active Apps:** 3 of 5
- F5659 - Manage Commodity Counter Deal Request
- F5665 - Monitor Hedge Constellation  
- F5666 - Manage Hedge Constellations Worklist

**Excluded Apps:** 2 of 5
- F5658 - Deferred (hybrid architecture, requires team discussion)
- F5655 - Handled by colleague (assessed for Issues 1-2 only)

### Next Actions:

1. **F5658:** Schedule team discussion about hybrid architecture
2. **F5655:** ~~Begin assessment~~ Handled by colleague ✅
3. **F5659:** Begin assessment (**START HERE** 🎯)
4. Continue with F5665, F5666

---

## Lessons Learned

### F5658 Insights:

1. **Not All Apps Are Standard Fiori Elements**
   - Some apps may be hybrid (Fiori + SAP GUI)
   - Different rules may apply
   - Always assess architecture first

2. **Tree Table vs. Grid Table**
   - Tree tables have different behavior
   - May have different context menu implementation
   - Requires separate consideration

3. **SAP GUI Integration**
   - Hybrid apps may redirect to GUI for details
   - May not follow standard Fiori navigation patterns
   - Context menu guidelines may not apply

4. **Team Discussion Important**
   - Not all issues apply to all apps
   - Architecture decisions affect applicability
   - Get team input before spending effort

---

**Next App to Test:** F5659 - Manage Commodity Counter Deal Request 🔍
