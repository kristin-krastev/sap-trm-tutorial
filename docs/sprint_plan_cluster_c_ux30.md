# Sprint Plan: Cluster C - Analytics & Oversight - UX 3.0 Initiative

**Sprint Duration:** 3 weeks (Dec 15, 2025 - Jan 5, 2026)  
**Sprint Goal:** Complete UX 3.0 enhancements for 4 Analytics & Oversight apps + 2 ATC check tasks

---

## Sprint Overview

### Apps in Scope (Cluster C: Analytics & Oversight)
1. **F5654** - Commodity Hedge Management Cockpit - Analytical View
2. **F5656** - Commodity Hedge Management Cockpit - Overview
3. **F5657** - Retrieve Hedge Constellation Details
4. **F6003** - Monitor Overhedged Exposures

### ATC Tasks
1. **ATC Task 1** - Review and resolve ATC findings across all UX 3.0 apps
2. **ATC Task 2** - Implement best practices and quality checks documentation

---

## UX 3.0 Enhancement Pattern

Based on previous work (Trade Order Cockpit), the UX 3.0 pattern includes:

### 1. **Text Arrangement Implementation**
- Add text/name fields to CDS views
- Configure `@ObjectModel.text.element` associations on ID fields
- Set `@Semantics.text: true` for text fields
- Add `@UI.textArrangement: #TEXT_LAST` for display format

### 2. **Column Sorting Enhancement**
- Include text fields in `groupBy` array (makes them sortable dimensions)
- Include text fields in `requestAtLeast` array (ensures they're loaded)
- Enable "Sort by ID" AND "Sort by Name" options in column context menu

### 3. **ATC Compliance**
- Ensure `@Semantics.text: true` for all text fields (not `false`)
- Maintain consistency between `@ObjectModel.text.element` and `@Semantics.text`
- Resolve "Inconsistent Modeling of Text References" errors

### 4. **UI Annotations**
- Hide text fields from direct display: `@UI.dataFieldDefault: [{hidden: true}]`
- Hide text fields from filters: `@Consumption.filter.hidden: true`
- Proper labeling with `@EndUserText.label`

---

## Sprint Timeline (3 Weeks)

### Week 1 (Dec 15-21, 2025)
**Focus:** F5654 & F5656 - The two Cockpit apps

#### Days 1-2: F5654 - Analytical View
- Analyze current CDS view structure
- Identify ID fields that need text associations
- Implement text arrangement pattern
- Update metadata extensions
- Run ATC checks and fix issues
- Test in Service Binding Preview

#### Days 3-4: F5656 - Overview
- Analyze current CDS view structure
- Identify ID fields that need text associations
- Implement text arrangement pattern
- Update metadata extensions
- Run ATC checks and fix issues
- Test in Service Binding Preview

#### Day 5: Week 1 Review
- Document learnings from F5654 & F5656
- Identify common patterns
- Prepare for Week 2

---

### Week 2 (Dec 22-28, 2025) *Holiday Week*
**Focus:** F5657 & F6003 - Constellation & Monitoring apps

#### Days 1-2: F5657 - Retrieve Hedge Constellation Details
- Analyze current CDS view structure
- Identify ID fields that need text associations
- Implement text arrangement pattern
- Update metadata extensions
- Run ATC checks and fix issues
- Test in Service Binding Preview

#### Days 3-4: F6003 - Monitor Overhedged Exposures
- Analyze current CDS view structure
- Identify ID fields that need text associations
- Implement text arrangement pattern
- Update metadata extensions
- Run ATC checks and fix issues
- Test in Service Binding Preview

#### Day 5: Week 2 Review
- Document learnings from F5657 & F6003
- Cross-check all 4 apps for consistency
- Prepare ATC task scope

---

### Week 3 (Dec 29, 2025 - Jan 5, 2026) *Holiday Week*
**Focus:** ATC Tasks & Documentation

#### Days 1-2: ATC Task 1 - Cross-App ATC Review
- Run comprehensive ATC checks on all 4 apps
- Resolve any remaining ATC findings
- Focus on common patterns:
  - Inconsistent text modeling
  - Missing annotations
  - Performance issues
  - Code quality issues
- Document ATC resolution patterns

#### Days 3-4: ATC Task 2 - Best Practices Documentation
- Create ATC quality check guidelines for Cluster C apps
- Document common ATC issues and resolutions
- Create reusable code snippets
- Update team knowledge base
- Prepare training materials

#### Day 5: Sprint Closure
- Final testing of all 4 apps
- Sprint retrospective documentation
- Handoff documentation
- Prepare demo for stakeholders

---

## Common Fields to Enhance (Based on Previous Work)

### Standard SAP Fields
- **Company Code**: `CompanyCode` → `CompanyCodeName` (via `I_CompanyCode`)
- **Currency**: `Currency` → `CurrencyName` (via `I_Currency`)
- **Plant**: `Plant` → `PlantName` (via `I_Plant`)
- **Customer**: `Customer` → `CustomerName` (via `I_Customer`)
- **Supplier**: `Supplier` → `SupplierName` (via `I_Supplier`)

### Commodity-Specific Fields
- **DCS**: `CmmdtyHedgePlanExposureDCSID` → `DerivativeContrSpecName`
- **Hedge Book**: `CmmdtyHdgPlanExposureHedgeBook` → `CmmdtyHedgeBookDescription`
- **Delivery Period**: ID → Description
- **Commodity**: ID → Description

---

## Known ATC Issues & Solutions

### Issue 1: Inconsistent Modeling of Text References
**Error:** Text fields referenced in `@ObjectModel.text.element` are marked as `@Semantics.text: false`

**Solution:**
```abap
// WRONG:
@Semantics.text: false
DerivativeContrSpecName,

// CORRECT:
@Semantics.text: true
DerivativeContrSpecName,
```

### Issue 2: Missing Text Fields in Analytical Queries
**Error:** Text fields not included in `groupBy` array

**Solution:**
```abap
@UI.presentationVariant: [{
  groupBy: [ 
    'CmmdtyHedgePlanExposureDCSID',
    'DerivativeContrSpecName',  // Add text field here
    ...
  ]
}]
```

### Issue 3: Text Fields Not Pre-loaded
**Error:** Text fields not in `requestAtLeast` causing performance issues

**Solution:**
```abap
@UI.presentationVariant: [{
  requestAtLeast: [ 
    'CmmdtyHedgePlanExposureDCSID',
    'DerivativeContrSpecName',  // Add text field here
    ...
  ]
}]
```

---

## Testing Checklist (Per App)

### Development Testing
- [ ] CDS view activates without errors
- [ ] ATC checks pass (all categories)
- [ ] Metadata extension activates without errors
- [ ] Service Binding Preview loads successfully

### Functional Testing
- [ ] Text arrangement displays correctly (ID with Name)
- [ ] Column context menu shows "Sort by ID" options
- [ ] Column context menu shows "Sort by Name" options
- [ ] Sorting by ID works correctly
- [ ] Sorting by Name works correctly
- [ ] Filters work as expected
- [ ] Value helps display correctly

### Performance Testing
- [ ] View loads within acceptable time
- [ ] Sorting operations are responsive
- [ ] No N+1 query issues

---

## Success Criteria

### For Each App (F5654, F5656, F5657, F6003)
✅ All ID fields with text associations properly configured  
✅ Column sorting enabled for both ID and Name  
✅ ATC checks pass with zero errors  
✅ Text arrangement displays correctly  
✅ Service Binding Preview works as expected  
✅ Documentation created  

### For ATC Tasks
✅ Comprehensive ATC review completed across all 4 apps  
✅ All ATC findings resolved  
✅ Best practices documentation created  
✅ Team knowledge base updated  
✅ Reusable patterns documented  

---

## Risk Mitigation

### Holiday Coverage (2 weeks overlap)
- Focus complex work in Week 1
- Ensure knowledge transfer before holidays
- Have rollback plan ready
- Document all changes thoroughly

### Technical Risks
- **Risk:** Complex view hierarchies may require more analysis  
  **Mitigation:** Start with simplest app (F5656 Overview)
  
- **Risk:** Unknown ATC issues may surface  
  **Mitigation:** Leverage experience from Trade Order Cockpit
  
- **Risk:** Performance issues with additional fields  
  **Mitigation:** Use `requestAtLeast` efficiently, test early

---

## Reference Documentation

### Internal Docs
- `/workspace/docs/fiori_column_sorting_guide.md` - UX 3.0 implementation guide
- `/workspace/docs/jira_summary.md` - Previous work on Trade Order Cockpit
- `/workspace/docs/trade_order_cockpit_sort_investigation.md` - Detailed investigation
- `/workspace/rap-rules.md` - General RAP development guidelines

### Key Learnings from Previous Sprint
1. Always check `@Semantics.text` consistency
2. Text fields must be in `groupBy` for analytical tables
3. Use Service Binding Preview for quick validation
4. ATC checks should be run early and often
5. Document patterns as you discover them

---

## Deliverables

### Per App
1. Updated CDS Consumption View
2. Updated Metadata Extension
3. ATC check report (passing)
4. Test results documentation
5. Implementation notes

### Sprint-Level
1. Sprint summary document
2. Lessons learned
3. ATC best practices guide (updated)
4. Reusable code patterns library
5. Team training materials
6. Stakeholder demo materials

---

## Team Notes

- Sprint spans holiday period - plan accordingly
- Focus on completing apps early
- Document as you go
- Leverage patterns from Trade Order Cockpit work
- Early and frequent ATC checks
- Test in Service Binding Preview before requesting QA deployment

---

**Created:** December 15, 2025  
**Sprint Start:** December 15, 2025  
**Sprint End:** January 5, 2026  
**Sprint Velocity:** 4 apps + 2 ATC tasks over 3 weeks
