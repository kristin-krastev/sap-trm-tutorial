# Issue 1: Table Type - Quick Start Guide
**For:** F5658, F5655, F5659, F5665, F5666  
**Estimated Time:** 30-60 minutes per app (verification only) | 2-4 hours per app (if changes needed)

---

## 🎯 Quick Decision

**All 5 commodity trading apps should likely use:**
```json
"gridTable": true,
"condensedTableLayout": true
```

**Why?** Large datasets, comparison critical, List Report pattern, desktop users.

---

## ⚡ Quick Check (Do This First)

### 1. Find the Manifest
Open: `webapp/manifest.json` (or `manifest.json` in your app root)

### 2. Look for This Section
```json
{
  "sap.ui.generic.app": {
    "pages": [
      {
        "entitySet": "YourEntitySet",
        "component": {
          "settings": {
            // Look here! 👇
            "responsiveTable": true  // or "gridTable": true
          }
        }
      }
    ]
  }
}
```

### 3. Check Current Config

| What You See | Current Table Type | Action Needed? |
|--------------|-------------------|----------------|
| `"gridTable": true` + `"condensedTableLayout": true` | Grid with Condensed ✅ | **None!** Perfect! |
| `"gridTable": true` (no condensed) | Grid without Condensed ⚠️ | **Add condensed layout** |
| `"responsiveTable": true` | Responsive Table ⚠️ | **Likely needs change** |
| Nothing specified | Default (usually Responsive) ⚠️ | **Likely needs change** |

---

## 🚨 When You MUST Check More

**If changing table type, you MUST also check:**

### Quick Impact Assessment

Answer these questions for each app:

1. **Do you have custom column fragments?**
   - Look in manifest for: `"viewExtensions"` or `"GridTableColumnsExtension"` or `"ResponsiveTableColumnsExtension"`
   - **If YES** → Will need to update XML fragments (2-4 hours work)

2. **Do you have custom controller code for table?**
   - Search code for: `getSelectedItems()`, `getSelectedIndices()`, table manipulation
   - **If YES** → Will need to update JavaScript controller (1-2 hours work)

3. **Do columns have ProgressIndicator or charts?**
   - Check custom fragments or standard columns
   - **If YES** → Cannot use condensed layout OR must replace controls

**If all answers are NO** → Quick change! Just update manifest. ✅

---

## 🛠️ Simple Change (No Custom Code)

### Change from Responsive to Grid Table

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

**Test:** Save, build, test in DEV. Done! ✅

---

## 🔧 Complex Change (Custom Code Present)

**If you have custom fragments/controllers, see:**
- `/workspace/docs/ISSUE_1_TABLE_TYPE_GUIDE.md` (full guide)
- Sections on custom fragments, controller API, custom data

---

## 📊 Expected Results for Your Apps

| App | Expected Current | Recommended Target | Estimated Effort |
|-----|-----------------|-------------------|------------------|
| F5658 - Trader's Cockpit | ? | Grid + Condensed | Medium (has custom fragments) |
| F5655 - Observe & Monitor | ? | Grid + Condensed | Check manifest first |
| F5659 - Counter Deal Request | Grid? | Grid + Condensed | Medium (has custom fragments) |
| F5665 - Monitor Constellation | ? | Grid + Condensed | Check manifest first |
| F5666 - Worklist | ? | Grid + Condensed | Check manifest first |

**Note:** F5658 and F5659 definitely have custom fragments (we found them in Issue 2 investigation!)

---

## 🚀 Recommended Workflow

### Phase 1: Quick Assessment (Today - 15 min per app)
1. Open manifest.json
2. Check current table type
3. Check for custom fragments
4. Document findings
5. Estimate effort for each app

### Phase 2: Simple Changes First (Day 2 - if any)
1. Apps with NO custom code
2. Just update manifest
3. Test in DEV
4. Quick wins! ✅

### Phase 3: Complex Changes (Day 3-4)
1. Apps WITH custom code (F5658, F5659 for sure)
2. Update fragments
3. Update controllers (if needed)
4. Thorough testing
5. Deploy to QA

---

## 📞 When You're Ready

**To start Phase 1 (Quick Assessment):**

Just share the manifest.json for each app (or I can guide you to find it), and I'll:
1. ✅ Identify current table type
2. ✅ Check for custom fragments
3. ✅ Recommend changes
4. ✅ Estimate effort
5. ✅ Create specific implementation plan

---

## 📚 Full Documentation

**Detailed Guide:** `/workspace/docs/ISSUE_1_TABLE_TYPE_GUIDE.md`

**Contains:**
- Complete decision criteria
- Step-by-step implementation
- Custom fragment examples
- Controller API changes
- Testing checklist
- Troubleshooting

---

## 💡 Pro Tips

1. **Start with apps that have NO custom fragments** - Quick wins build momentum
2. **Reuse stable IDs** - Preserves user variants
3. **Test with realistic data volumes** - 10 rows ≠ 1000 rows
4. **Condensed layout is desktop-only** - Mobile automatically adapts
5. **Variants usually survive table type changes** - If you keep column keys stable

---

**Ready to check your first manifest.json?** Just paste it and I'll analyze it! 🔍
