# Cluster B - Quick Reference Card

**Print this and keep at your desk! 📌**

---

## Your Apps (Priority Order)

1. **F5658** - Commodity Trader's Order Cockpit ⭐⭐⭐ CRITICAL
2. **F5655** - Observe and Monitor Deal Requests ⭐⭐
3. **F5659** - Manage Commodity Counter Deal Request ⭐⭐
4. **F5665** - Monitor Hedge Constellation ⭐
5. **F5666** - Manage Hedge Constellations Worklist ⭐

---

## The 10 Improvements Checklist

For each app, implement:

- [ ] 1. **Table Type** → Grid Table
- [ ] 2. **Column Header Menu** → Enable sort/filter
- [ ] 3. **Context Menu** → Quick actions
- [ ] 4. **Filter Info Bar** → Show active filters
- [ ] 5. **Table Column Width** → Optimize importance
- [ ] 6. **Scroll & Selection Limit** → Max 200 items
- [ ] 7. **Actions for All Items** → Bulk operations
- [ ] 8. **Information Density** → Object page optimization
- [ ] 9. **Design-time Cards** → Configure cards
- [ ] 10. **Extensibility** → Enable custom fields

---

## Time Estimates per App

| Phase | Hours | Days (@ 8h) |
|-------|-------|-------------|
| **Quick Wins** (1-4) | 8-12h | 1-2 days |
| **Core** (5-7) | 6-8h | 1 day |
| **Advanced** (8-10) | 4-6h | 0.5-1 day |
| **Testing** | 2-4h | 0.5 day |
| **TOTAL per app** | 20-30h | 2.5-4 days |

---

## Daily Routine Checklist

### Morning (30 min)
- [ ] Check JIRA for today's tasks
- [ ] Review yesterday's progress
- [ ] Daily standup
- [ ] Plan the day's 2-3 main tasks

### During Work (6-7 hours)
- [ ] Focus on one improvement at a time
- [ ] Test as you develop (don't batch)
- [ ] Document decisions/issues immediately
- [ ] Take screenshots (before/after)
- [ ] Commit code frequently with clear messages

### End of Day (30 min)
- [ ] Log time in JIRA with descriptions
- [ ] Update action plan checkboxes
- [ ] Note blockers for tomorrow
- [ ] Quick demo to colleague (if milestone)

---

## Week Structure

| Day | Focus | Time Log |
|-----|-------|----------|
| **Monday** | Analysis + Quick Win #1-2 | 6-8h |
| **Tuesday** | Quick Win #3-4 + Core #1 | 8h |
| **Wednesday** | Core #2-3 + Mid-week checkpoint | 7.5h |
| **Thursday** | Advanced #1-2 + Testing start | 8h |
| **Friday** | Advanced #3 + Testing + Demo | 6-7h |

---

## Code Template Quick Reference

### Metadata Extension Template
```abap
@Metadata.layer: #CUSTOMER
annotate view [YourView] with
{
  // 1. Grid Table
  @UI.lineItem: [{ position: 10, importance: #HIGH }]
  
  // 2. Filter Field
  @UI.selectionField: [{ position: 10 }]
  
  // 3. Column Width
  @UI.lineItem: [{ importance: #HIGH }]  // or #MEDIUM, #LOW
  
  // 4. Sort/Scroll
  @UI.presentationVariant: [{
    maxItems: 200,
    sortOrder: [{ by: 'Field', direction: #DESC }]
  }]
  
  // 5. Context Action
  @UI.lineItem: [{
    type: #FOR_ACTION,
    dataAction: 'ActionName'
  }]
  
  // 6. Object Page Facet
  @UI.facet: [{
    id: 'Info',
    type: #COLLECTION,
    label: 'Information'
  }]
  
  // 7. Extensibility
  @ObjectModel.extensibility: { enabled: true }
}
```

---

## Testing Checklist (per app)

### Functional ✅
- [ ] Table loads correctly
- [ ] Filters work (all fields)
- [ ] Sorting works (all columns)
- [ ] Context menu appears
- [ ] Actions execute successfully
- [ ] Bulk actions process correctly
- [ ] Object page displays properly

### Performance 🚀
- [ ] < 3 sec table load (200 items)
- [ ] < 1 sec filter response
- [ ] Smooth scrolling (no lag)

### Usability 👤
- [ ] Columns are appropriately sized
- [ ] Most important data visible first
- [ ] Actions are easy to find
- [ ] No horizontal scrolling needed

---

## JIRA Time Logging Format

```
Time Spent: [X]h
Work Description: [Specific accomplishment]
Remaining Estimate: [Y]h

Example:
Time Spent: 4h
Work Description: Implemented grid table and column header menu for F5658. Updated @UI.lineItem annotations. Unit tests passing.
Remaining Estimate: 12h
```

---

## Common JIRA Categories

- **Analysis & Design:** Requirements, planning, wireframes
- **Development:** Coding, annotation updates, behavior definitions
- **Testing:** Unit tests, integration tests, performance tests
- **Documentation:** Technical docs, user guides, comments
- **Deployment:** Transport, QA deployment, production release
- **Support:** Bug fixes, user questions, troubleshooting
- **Meetings:** Standup, demos, retrospectives, planning

---

## Performance Targets

| Metric | Target | Critical |
|--------|--------|----------|
| Table Load | < 3 sec | < 5 sec |
| Filter Response | < 1 sec | < 2 sec |
| Action Execution | < 2 sec | < 5 sec |
| Object Page Load | < 2 sec | < 4 sec |

---

## When to Ask for Help

🛑 **Immediately:**
- Security/authorization issues
- System down/inaccessible
- Data corruption risk
- Production outage

⏰ **After 2 hours:**
- Stuck on technical problem
- Unclear requirements
- Tool/environment issues

📅 **Next sync:**
- Nice-to-have improvements
- Process questions
- Clarification on priorities

---

## Key Contacts

| Role | Name | Contact | When to Reach |
|------|------|---------|---------------|
| Tech Lead | [Name] | [Slack/Email] | Technical blockers |
| Scrum Master | [Name] | [Slack/Email] | Process/timeline |
| Product Owner | [Name] | [Slack/Email] | Business questions |
| Colleague (Cluster A) | [Name] | [Slack] | Reference app questions |
| Colleague (Cluster C) | [Name] | [Slack] | Coordination |

---

## Sprint Goals Summary

### Sprint 5 (Weeks 1-2)
🎯 Complete F5658 + F5655 (2 apps)  
📊 Estimated: 80 hours

### Sprint 6 (Weeks 3-4)
🎯 Complete F5659 + Start F5665 (1.5 apps)  
📊 Estimated: 40 hours

### Sprint 7 (Weeks 5-6)
🎯 Complete F5665 + F5666 (1.5 apps)  
📊 Estimated: 40 hours

### Sprint 8 (Weeks 7-8)
🎯 Testing, UAT, Deployment  
📊 Estimated: 40 hours

**Total: 200 hours over 4 sprints**

---

## Success Metrics

✅ **All 5 apps completed**  
✅ **User satisfaction > 4/5**  
✅ **< 5 critical bugs per app**  
✅ **All performance targets met**  
✅ **On time delivery**  

---

## Motivational Milestones 🎉

- [ ] First app (F5658) completed → 🍕 Treat yourself!
- [ ] Sprint 5 demo successful → 🎊 
- [ ] All apps in QA → 🥳
- [ ] Production deployment → 🚀 Team celebration!
- [ ] Cluster B retrospective → 📚 Share lessons learned

---

## Emergency Commands

**Rollback last change:**
```bash
git reset --hard HEAD~1
```

**Check performance:**
```abap
" Use transaction: SAT, ST05
```

**Clear cache:**
```bash
" Use transaction: /nSMICM → Goto → HTTP Server Cache → Invalidate
```

---

## Files to Check Daily

1. `/workspace/docs/ux_improvements/cluster_b_action_plan.md`
2. JIRA board: [Link to your board]
3. Git branch: `cursor/improve-fiori-app-ux-for-commodity-hedging-c7d4`

---

## Before Going Home Each Day

- [ ] Code committed and pushed
- [ ] JIRA time logged
- [ ] Action plan updated
- [ ] Tomorrow's tasks identified
- [ ] Blockers documented

---

**You're doing great! One improvement at a time! 💪**

---

*Last updated: [Date]*  
*Next review: Weekly or as needed*
