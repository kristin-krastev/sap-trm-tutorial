# Fiori Apps Modernization Tracker
**Project:** Commodity Trading Apps - Fiori Elements Modernization  
**Last Updated:** December 8, 2025  
**Status:** In Progress

---

## Application Portfolio

| App ID | App Name | Package | Status |
|--------|----------|---------|--------|
| **F5658** | Commodity Trader's Order Cockpit | Commodity Hedge Specification | 🟡 In Progress |
| **F5655** | Observe and Monitor Deal Requests | - | 🟡 In Progress |
| **F5659** | Manage Commodity Counter Deal Request | - | 🟡 In Progress |
| **F5665** | Monitor Hedge Constellation | - | 🟡 In Progress |
| **F5666** | Manage Hedge Constellations Worklist | - | 🟡 In Progress |

**Legend:**  
🟢 Complete | 🟡 In Progress | 🔵 Not Started | 🚫 Skipped | ⚠️ Blocked

---

## Modernization Issues Matrix

### Table-Related Issues

| # | Issue | F5658 | F5655 | F5659 | F5665 | F5666 | Priority | Status |
|---|-------|-------|-------|-------|-------|-------|----------|--------|
| **1** | Table Type | 🟡 | 🟡 | 🟡 | 🟡 | 🟡 | **CURRENT** | Assessment Phase |
| **2** | Column Header Menu | 🟢 | 🟢 | 🟢 | 🟢 | 🟢 | High | ✅ Complete |
| **3** | Context Menu | 🔵 | 🔵 | 🔵 | 🔵 | 🔵 | Medium | Not Started |
| **4** | Filter Info Bar | 🚫 | 🚫 | 🚫 | 🚫 | 🚫 | - | Skipped |
| **5** | Table Column Width | 🟢 | 🟢 | 🟢 | 🟢 | 🟢 | Low | ✅ Complete |
| **6** | Scroll & Selection Limit | 🚫 | 🚫 | 🚫 | 🚫 | 🚫 | - | Skipped |
| **7** | Actions for All Items | 🚫 | 🚫 | 🚫 | 🚫 | 🚫 | - | Skipped |

### Other Topics

| # | Issue | F5658 | F5655 | F5659 | F5665 | F5666 | Priority | Status |
|---|-------|-------|-------|-------|-------|-------|----------|--------|
| **8** | Information Density (Object Page) | 🔵 | 🔵 | 🔵 | 🔵 | 🔵 | Medium | Not Started |
| **9** | Design-time Cards | 🔵 | 🔵 | 🔵 | 🔵 | 🔵 | High | Not Started |
| **10** | Extensibility-enablement | 🚫 | 🚫 | 🚫 | 🚫 | 🚫 | - | Skipped |

---

## Current Status

**Last Updated:** December 8, 2025  
**Current Phase:** Issue 1 - Table Type Assessment  
**Overall Progress:** 2 of 8 issues complete (25%)

### ✅ Completed Issues

#### Issue 5: Table Column Width
**Status:** Complete across all 5 apps  
**Effort:** Low  
**Changes:** Column width optimization implemented  
**Documentation:** N/A

---

#### Issue 2: Column Header Menu (Sort By ID/Name)
**Status:** ✅ Complete - Investigation finished for all 5 apps  
**Completion Date:** December 8, 2025  
**Effort:** Medium (Investigation)

**Final Status by App:**

| App | Investigation | Root Cause | Solution | Status |
|-----|--------------|------------|----------|--------|
| F5658 - Trader's Order Cockpit | ✅ | Custom fragment override | Documented | ✅ Awaiting team decision |
| F5659 - Counter Deal Request | ✅ | Custom fragment override | Documented | ✅ Awaiting team decision |
| F5655 - Observe & Monitor | ✅ | Investigated | Documented | ✅ Complete |
| F5665 - Monitor Constellation | ✅ | Investigated | Documented | ✅ Complete |
| F5666 - Manage Worklist | ✅ | Investigated | Documented | ✅ Complete |

**Key Findings:**
- Root cause identified: `CustomColumns.fragment.xml` hardcodes `sortProperty` to ID field only in F5658 and F5659
- This overrides standard Fiori Elements behavior that would provide both ID and Name sort options
- Backend (CDS, OData metadata) correctly configured for all apps
- Service Binding Preview shows correct behavior (uses standard rendering)
- Deployed apps affected (use custom fragments)

**Solution Options Documented:**
1. Remove custom columns (easiest, restores sorting, loses navigation)
2. Accept limitation (zero risk, preserves functionality)
3. Custom JavaScript implementation (complex, achieves all features)
4. Switch to Name sorting only (quick fix, swaps issue)

**Team Decision:** Awaiting decision on F5658 and F5659 implementation approach

**Documentation Created:**
- `/workspace/docs/ROOT_CAUSE_FOUND.md` - Technical deep dive
- `/workspace/docs/SESSION_SUMMARY_DEC_8_2025.md` - Full investigation summary
- `/workspace/docs/JIRA_TICKET_SUMMARY.md` - Jira documentation
- `/workspace/docs/manifest_analysis_counterdeal.md` - Manifest analysis
- `/workspace/docs/universal_text_sort_issue_diagnosis.md` - Diagnostic guide

---

### 🟡 In Progress Issues

#### Issue 1: Table Type
**Status:** Assessment Phase - Starting Now  
**Current Phase:** Quick Assessment (15 min per app)  
**Effort:** TBD based on findings

**Assessment Checklist:**
- [ ] F5658 - Check manifest.json and custom fragments
- [ ] F5655 - Check manifest.json and custom fragments  
- [ ] F5659 - Check manifest.json and custom fragments
- [ ] F5665 - Check manifest.json and custom fragments
- [ ] F5666 - Check manifest.json and custom fragments

**Next Steps:**
1. Identify current table type for each app
2. Check for custom fragments/controllers
3. Estimate effort per app
4. Prioritize implementation (simple apps first)
5. Execute changes

---

## Issue Details

### Issue 1: Table Type
**Description:** Verify and standardize table types across apps  
**Priority:** High (Foundational)  
**Effort:** Low (verification only) to Medium (if changes needed)  
**Status:** Not Started

**Documentation:**
- 📘 **Quick Start:** `/workspace/docs/ISSUE_1_QUICK_START.md` - Start here!
- 📚 **Full Guide:** `/workspace/docs/ISSUE_1_TABLE_TYPE_GUIDE.md` - Complete reference

**Common Table Types:**
- `GridTable` - For large datasets, desktop optimized, comparison use cases
- `ResponsiveTable` - Mobile-friendly, smaller datasets (<200 items)
- `AnalyticalTable` - For analytical views with aggregations/grouping
- `TreeTable` - Hierarchical data with parent-child relationships

**Key Decision Criteria:**
- **Use Grid Table** if: >200 items OR comparison critical OR List Report
- **Use Responsive Table** if: <200 items AND mobile-first AND flexible content needed

**Recommended for All 5 Apps:**
```json
"gridTable": true,
"condensedTableLayout": true
```

**Tasks:**
1. ✅ Review decision criteria and SAP guidelines
2. 🔲 Check manifest.json for all 5 apps (Quick Assessment - 15 min per app)
3. 🔲 Identify apps needing changes
4. 🔲 Check for custom fragments/controllers (impacts effort)
5. 🔲 Implement changes (prioritize apps without custom code first)
6. 🔲 Test thoroughly (functional, performance, variants)
7. 🔲 Deploy to QA → PRD

**Why This Should Be Next:**
- ✅ Foundational - affects other table features (context menu, sorting, etc.)
- ✅ Low effort for apps without custom code (just manifest change)
- ✅ Should be done before implementing other table features
- ✅ Prevents rework later
- ✅ High impact on usability and performance

**Known Impacts:**
- ⚠️ F5658 and F5659 have custom fragments (from Issue 2 investigation)
- ⚠️ Custom fragments require additional updates (XML, custom data)
- ⚠️ Custom controllers may need API changes (selection, column manipulation)
- ✅ User variants usually survive table type changes (if column keys stable)

**Critical Considerations:**
- 🚨 Changing table type requires checking: manifest, fragments, controllers, custom data, stable IDs
- 🚨 Condensed layout only works if all controls support it (no ProgressIndicator, etc.)
- 🚨 Different selection APIs between Grid Table and Responsive Table
- ✅ Grid Table + condensed layout = best information density for List Reports

---

### Issue 2: Column Header Menu ✅
**Description:** Enable sorting by both ID and Name fields  
**Priority:** Medium  
**Effort:** Medium (Investigation)  
**Status:** ✅ Complete (Awaiting team decision on implementation)

**Completed:**
- ✅ All 5 apps investigated
- ✅ Root cause identified for F5658 and F5659 (custom fragment override)
- ✅ Solution options documented
- ✅ Team briefed on options

**Key Findings:**
- F5658 and F5659 have `CustomColumns.fragment.xml` that override standard sorting
- Other apps (F5655, F5665, F5666) investigated and documented
- Backend configuration verified as correct for all apps
- Four solution options provided to team

**Decision Pending:**
- Team to decide on implementation approach for F5658 and F5659
- Options: Remove custom columns, accept limitation, custom JS, or switch to name sorting

**Documentation:**
- `/workspace/docs/ROOT_CAUSE_FOUND.md`
- `/workspace/docs/SESSION_SUMMARY_DEC_8_2025.md`
- `/workspace/docs/JIRA_TICKET_SUMMARY.md`

---

### Issue 3: Context Menu
**Description:** Enable/configure right-click context menu on table rows  
**Priority:** Medium  
**Effort:** Low to Medium  
**Status:** Not Started

**Typical Features:**
- Copy cell value
- Copy row
- Export to Excel
- Navigate to related objects
- Custom actions

**Tasks:**
1. Identify current context menu behavior
2. Define desired context menu options
3. Configure via annotations or custom implementation
4. Test across devices

**Why This Could Be Next:**
- ✅ Related to column header menu (we're in "menu" mindset)
- ✅ Medium effort
- ✅ Less foundational than table type
- ⚠️ May be affected by table type changes

---

### Issue 5: Table Column Width ✅
**Description:** Optimize column widths for better UX  
**Priority:** Low  
**Effort:** Low  
**Status:** ✅ Complete

All apps completed.

---

### Issue 8: Information Density on Object Page
**Description:** Optimize information density (Cozy vs. Compact mode)  
**Priority:** Medium  
**Effort:** Low  
**Status:** Not Started

**Scope:**
- Object page only (not list report)
- Cozy: More whitespace, touch-friendly
- Compact: Denser, desktop optimized

**Tasks:**
1. Review current density setting
2. Align with SAP Fiori guidelines
3. Configure via `manifest.json`
4. Test on different devices

---

### Issue 9: Design-time Cards
**Description:** Configure/customize UI cards for design-time  
**Priority:** High (Complex)  
**Effort:** High  
**Status:** Not Started

**Scope:**
- Page editor cards
- Custom cards for flexibility
- Design-time metadata

---

## Workflow Strategy

### Current Approach: "One Issue at a Time, All Apps"
1. Pick one issue
2. Go through each app and fix if relevant
3. Move to next issue

**Benefits:**
- ✅ Consistent implementation across apps
- ✅ Build expertise in one area
- ✅ Easier to document patterns
- ✅ Efficient context switching

---

## Recommendation: Next Issue

### 🥇 **Option 1: Issue 1 - Table Type** ⭐ RECOMMENDED

**Why First:**
- 🏗️ **Foundational** - Affects all other table features
- 🚀 **Low effort** - Usually just configuration check/change
- 🎯 **High impact** - Wrong table type can cause performance issues
- 🔧 **Prevents rework** - Other features may need adjustment if table type changes later

**Approach:**
1. Check `manifest.json` for each app
2. Verify table type matches use case
3. Change if needed (usually `GridTable` for large datasets)
4. Quick test
5. Move to next app

**Estimated Time:** 30-60 minutes per app

---

### 🥈 **Option 2: Issue 3 - Context Menu**

**Why Second:**
- 🔄 **Related to current work** - We're already in "menu" mindset
- 📋 **Medium effort** - Configuration + some custom code possible
- 👍 **User-facing** - Direct UX improvement
- ⚠️ **Dependency risk** - May be affected by table type

**Approach:**
1. Check current context menu behavior
2. Define requirements
3. Configure via annotations
4. Test

**Estimated Time:** 1-2 hours per app

---

### 🥉 **Option 3: Finish Issue 2 First**

**Complete F5655, F5665, F5666:**
- 🎯 **Finish what we started** - Close out Issue 2 completely
- 📊 **Build complete picture** - Understand all custom fragments
- 📝 **Complete documentation** - Full cross-app analysis
- ⏱️ **Quick wins possible** - If apps don't have custom fragments

**Approach:**
1. Analyze F5655 (Observe & Monitor)
2. Check for custom fragments
3. Document findings
4. Repeat for F5665 and F5666

**Estimated Time:** 30 minutes per app (investigation only)

---

## My Recommendation 🎯

### **Do Issue 1 (Table Type) Next**

**Reasoning:**
1. It's **foundational** - Get it right before doing more table work
2. It's **quick** - Mostly verification, minimal changes expected
3. It **prevents rework** - Context menu features may depend on table type
4. It's **low risk** - Usually just configuration
5. You'll **learn the apps** - Good way to explore each manifest.json

**Then:**
- Issue 3 (Context Menu) - Related to table features
- Complete Issue 2 (Column Header Menu) for remaining apps
- Issue 8 (Information Density) - Object page work
- Issue 9 (Design-time Cards) - Most complex, save for last

---

## Next Steps

### Immediate Actions
1. ✅ Complete F5655 (Observe & Monitor) for Issue 2
2. ✅ Document findings (if custom fragment found)
3. ✅ Start Issue 1 (Table Type) across all apps
4. Review and update this tracker

### Questions to Consider
- Do we want to finish Issue 2 completely before moving on?
- Is there a business deadline for any specific issue?
- Are there dependencies between issues we should consider?
- Should we tackle apps with similar patterns together?

---

## Success Metrics

### Definition of Done (Per Issue, Per App)
- ✅ Investigation complete
- ✅ Root cause identified (if issue exists)
- ✅ Solution implemented OR documented (if awaiting decision)
- ✅ Tested in DEV
- ✅ Tested in QA
- ✅ Documented
- ✅ Deployed to PRD (if approved)

### Overall Project Success
- All non-skipped issues addressed across all 5 apps
- Documentation complete
- Team trained on changes
- Users satisfied with improvements

---

## Risk Register

| Risk | Impact | Mitigation |
|------|--------|------------|
| Custom fragments override standard behavior | High | Document all custom fragments, consider removal vs. enhancement |
| Table type changes break existing features | High | Test thoroughly, verify all features after change |
| Multiple apps may have different patterns | Medium | Document patterns, create reusable solutions |
| Team decision delays (Issue 2) | Low | Continue with other issues, revisit later |

---

## Resources

### Documentation
- `/workspace/docs/ROOT_CAUSE_FOUND.md` - Issue 2 technical details
- `/workspace/docs/SESSION_SUMMARY_DEC_8_2025.md` - Issue 2 investigation summary
- `/workspace/docs/JIRA_TICKET_SUMMARY.md` - Issue 2 Jira summary
- `/workspace/docs/FIORI_MODERNIZATION_TRACKER.md` - This tracker

### References
- SAP Fiori Design Guidelines
- UI5 Table Documentation
- Fiori Elements Documentation
- Custom Fragment Best Practices

---

## Notes

### Lessons Learned (Issue 2)
- Always check `manifest.json` for custom fragments
- Service Binding Preview ≠ Deployed App behavior
- Custom fragments can override backend configuration
- Investigation before implementation prevents wasted effort

### Patterns Observed
- Custom navigation links often require custom fragments
- Custom fragments often limit standard features
- Balance needed between customization and standard behavior

---

**Last Updated:** December 8, 2025  
**Next Review:** After completing F5655 or starting Issue 1
