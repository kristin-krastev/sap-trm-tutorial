# F5656 - Issue 5 & Issue 10 Implementation Guide (BAS)

**Date:** December 16, 2025  
**App:** F5656 - Commodity Hedge Cockpit Overview  
**Changes:** Issue 5 (Column Width) + Issue 10 (Extensibility)

---

## 📋 Changes to Make

### Change 1: Issue 5 - Fix Column Width (View XML)
**File:** `webapp/view/Overview.view.xml`  
**Change:** Add `customizeConfig` property to SmartTable

### Change 2: Issue 10 - Enable Extensibility (Manifest)
**File:** `webapp/manifest.json`  
**Change:** Set `flexEnabled: true`

---

## 🚀 Step-by-Step Implementation in BAS

### Step 1: Open BAS and Navigate to Project

1. **Open SAP Business Application Studio**
2. **Open your workspace** with the F5656 app
3. **Locate the project folder:** 
   - Look for: `ibso.commodity.hedgecockpit.overview` or similar
   - Or the app with ID matching F5656

---

### Step 2: Create New Branch

**In BAS Terminal (or Source Control panel):**

```bash
# Make sure you're on main/master branch
git checkout main

# Pull latest changes
git pull origin main

# Create new branch for Issue 5 & 10
git checkout -b feature/f5656-column-width-and-extensibility
```

**Or using BAS UI:**
1. Click **Source Control** icon (left sidebar)
2. Click **"..."** menu → **Checkout to...**
3. Select **"Create new branch"**
4. Name: `feature/f5656-column-width-and-extensibility`

---

### Step 3: Make Change 1 - Fix Column Width (Issue 5)

#### File: `webapp/view/Overview.view.xml`

**Find this section (around line 15-25):**

```xml
<smartTable:SmartTable 
    customData:presentationVariantQualifier="DefaultOvw" 
    backgroundDesign="Solid" 
    id="idSmartTable"
    smartFilterId="smartFilterBar" 
    tableType="AnalyticalTable" 
    entitySet="Year" 
    useTablePersonalisation="true" 
    header="{i18n>headerTable}"
    showRowCount="false" 
    initialise="onSmartTableInitialise"
    ignoreFromPersonalisation="...">
```

**Add ONE line after `initialise="onSmartTableInitialise"`:**

```xml
<smartTable:SmartTable 
    customData:presentationVariantQualifier="DefaultOvw" 
    backgroundDesign="Solid" 
    id="idSmartTable"
    smartFilterId="smartFilterBar" 
    tableType="AnalyticalTable" 
    entitySet="Year" 
    useTablePersonalisation="true" 
    header="{i18n>headerTable}"
    showRowCount="false" 
    initialise="onSmartTableInitialise"
    customizeConfig="{'autoColumnWidth': {'*': {'truncateLabel': false}}}"
    ignoreFromPersonalisation="...">
```

**What you added:**
```xml
customizeConfig="{'autoColumnWidth': {'*': {'truncateLabel': false}}}"
```

**Save the file** (Ctrl+S or Cmd+S)

---

### Step 4: Make Change 2 - Enable Extensibility (Issue 10)

#### File: `webapp/manifest.json`

**Find this section in `sap.ui5`:**

```json
"sap.ui5": {
    "flexEnabled": false,
    "rootView": {
        ...
    }
}
```

**Change `false` to `true`:**

```json
"sap.ui5": {
    "flexEnabled": true,
    "rootView": {
        ...
    }
}
```

**Save the file** (Ctrl+S or Cmd+S)

---

### Step 5: Verify Changes

**Check what you changed:**

```bash
git status
```

**Should show:**
```
modified:   webapp/view/Overview.view.xml
modified:   webapp/manifest.json
```

**Review the exact changes:**

```bash
git diff webapp/view/Overview.view.xml
git diff webapp/manifest.json
```

**Verify:**
- ✅ Only added `customizeConfig` line to Overview.view.xml
- ✅ Only changed `flexEnabled: false` to `true` in manifest.json
- ✅ No other accidental changes

---

### Step 6: Commit Changes

**Stage the files:**

```bash
git add webapp/view/Overview.view.xml
git add webapp/manifest.json
```

**Commit with descriptive message:**

```bash
git commit -m "feat(f5656): Fix column width and enable extensibility

- Issue 5: Add customizeConfig to prevent column header truncation
  - Set autoColumnWidth with truncateLabel: false for all columns
  - Ensures full column headers are visible (e.g., 'Company' not 'Co...')
  
- Issue 10: Enable UI flexibility for extensibility
  - Set flexEnabled: true in manifest.json
  - Allows key users to adapt UI via UI Adaptation Editor
  - Enables personalization and custom columns

Closes: #Issue5 #Issue10"
```

---

### Step 7: Push Branch to Remote

**Push your new branch:**

```bash
git push origin feature/f5656-column-width-and-extensibility
```

**Or if BAS prompts you:**

```bash
git push --set-upstream origin feature/f5656-column-width-and-extensibility
```

---

### Step 8: Create Pull Request

#### Option A: Via GitHub UI (Easiest)

1. **Go to GitHub repository** in your browser
2. **You'll see a banner:** "feature/f5656-column-width-and-extensibility had recent pushes"
3. **Click "Compare & pull request"** button
4. **Fill in PR details:**

**Title:**
```
feat(f5656): Fix column width and enable extensibility (Issue 5 & 10)
```

**Description:**
```markdown
## Summary
Fixes column width truncation and enables UI extensibility for F5656 Commodity Hedge Cockpit Overview.

## Changes Made

### Issue 5: Column Width Fix
- **File:** `webapp/view/Overview.view.xml`
- **Change:** Added `customizeConfig` property to SmartTable
- **Impact:** Column headers now display fully (e.g., "Company" instead of "Co...")
- **Solution:** Uses official UX 3.0 guidance for Freestyle apps

### Issue 10: Extensibility Enablement
- **File:** `webapp/manifest.json`
- **Change:** Set `flexEnabled: true`
- **Impact:** Enables UI Adaptation Editor for key users
- **Benefit:** Allows personalization and custom extensions

## Testing Plan

### Issue 5 - Column Width
- [ ] Open F5656 app
- [ ] Verify "Company" column header shows full text (not "Co...")
- [ ] Verify all other column headers are fully visible
- [ ] Check table layout remains optimal

### Issue 10 - Extensibility
- [ ] Open F5656 app
- [ ] Check for "Adapt Filters" button
- [ ] Verify "Adapt UI" option in user menu (gear icon)
- [ ] Test UI adaptation capabilities
- [ ] Verify personalizations can be saved

## Technical Details

**App:** F5656 - Commodity Hedge Cockpit Overview  
**Type:** Freestyle SAPUI5 app with SmartTable  
**Entity Set:** Year  
**Backend:** C_CommodityHedgeCockpitYear (unchanged)

## Related Work
- Previous: Issue 1-3 completed
- Sprint: Cluster C - Analytics & Oversight UX 3.0

## Screenshots
_Will be added after testing_

## Checklist
- [x] Changes follow UX 3.0 guidance
- [x] No backend changes required
- [x] Minimal risk (UI-only changes)
- [ ] Tested in dev environment
- [ ] Ready for code review
```

5. **Select reviewers** (if required)
6. **Click "Create pull request"**

---

#### Option B: Via GitHub CLI (From BAS Terminal)

**If you have GitHub CLI installed:**

```bash
gh pr create --title "feat(f5656): Fix column width and enable extensibility (Issue 5 & 10)" --body "$(cat <<'EOF'
## Summary
Fixes column width truncation and enables UI extensibility for F5656.

## Changes Made

### Issue 5: Column Width Fix
- Added customizeConfig to SmartTable in Overview.view.xml
- Prevents column header truncation

### Issue 10: Extensibility Enablement  
- Set flexEnabled: true in manifest.json
- Enables UI Adaptation Editor

## Testing Plan
- Verify column headers show fully
- Test UI adaptation capabilities

EOF
)"
```

---

### Step 9: After PR is Created

**What happens next:**

1. **Automated Checks Run:**
   - Syntax validation
   - Linting
   - Build verification
   - Any CI/CD pipeline tests

2. **Code Review:**
   - Reviewers check your changes
   - May request modifications
   - Approve when satisfied

3. **Merge:**
   - Once approved, merge PR
   - Changes go to main branch
   - Get deployed to test system

---

## 🧪 Testing After Deployment

### Test Issue 5 (Column Width)

1. **Open F5656** in test system
2. **Check Company column header:**
   - ✅ Should show: "Company Code" (full text)
   - ❌ Should NOT show: "Co..." (truncated)
3. **Check DCS column:**
   - ✅ Should show: "DCS" (full text)
4. **Check all headers:** All should be fully visible

### Test Issue 10 (Extensibility)

1. **Open F5656** in test system
2. **Look for extensibility features:**
   - ✅ "Adapt Filters" button visible
   - ✅ User menu (top right) has "Adapt UI" option
3. **Test UI Adaptation:**
   - Click "Adapt UI"
   - Try adding/removing a column
   - Try rearranging columns
   - Save personalization
   - Refresh page → personalization persists ✅

---

## 🔄 If Changes Need Updates

**If reviewers request changes:**

```bash
# Make the requested changes to files

# Stage and commit
git add .
git commit -m "fix: Address review comments"

# Push to same branch
git push origin feature/f5656-column-width-and-extensibility

# PR will automatically update!
```

---

## 📊 Expected Results

### Before Changes:
- ❌ Column headers truncated ("Co...")
- ❌ No UI adaptation available

### After Changes:
- ✅ Column headers fully visible ("Company Code")
- ✅ UI Adaptation Editor enabled
- ✅ Users can personalize their view

---

## 🎯 Success Criteria

**Issue 5:**
- [x] Changes made to Overview.view.xml
- [ ] Column headers display fully
- [ ] No layout issues
- [ ] Table remains responsive

**Issue 10:**
- [x] Changes made to manifest.json
- [ ] flexEnabled set to true
- [ ] UI Adaptation Editor accessible
- [ ] Personalizations work correctly

**Overall:**
- [ ] PR created and reviewed
- [ ] Changes merged to main
- [ ] Deployed to test system
- [ ] User acceptance testing passed
- [ ] F5656 Issues 5 & 10 complete! ✅

---

## 📁 Files Changed Summary

| File | Change | Lines | Risk |
|------|--------|-------|------|
| `webapp/view/Overview.view.xml` | Add customizeConfig | +1 | Low |
| `webapp/manifest.json` | flexEnabled: true | ~1 | Low |
| **Total** | | **2 lines** | **Low** |

---

## 🚨 Troubleshooting

### Issue: Git push fails
**Solution:**
```bash
git pull origin main --rebase
git push origin feature/f5656-column-width-and-extensibility --force
```

### Issue: Can't find the files
**Solution:**
- Use BAS search (Ctrl+P) to find files by name
- Search for: "Overview.view.xml" or "manifest.json"

### Issue: Changes not showing after deployment
**Solution:**
- Clear browser cache (Ctrl+Shift+R)
- Check if PR was actually merged
- Verify deployment completed successfully

---

## ✅ Checklist for Implementation

**Before Starting:**
- [ ] BAS opened and workspace loaded
- [ ] Correct project identified (F5656 app)
- [ ] On main branch with latest changes

**During Implementation:**
- [ ] New branch created
- [ ] Overview.view.xml modified (customizeConfig added)
- [ ] manifest.json modified (flexEnabled: true)
- [ ] Changes verified with git diff
- [ ] Files staged and committed
- [ ] Branch pushed to remote

**After Push:**
- [ ] Pull request created
- [ ] PR description complete
- [ ] Reviewers assigned (if needed)
- [ ] CI/CD checks passing

**After Merge:**
- [ ] Changes deployed to test system
- [ ] Issue 5 tested (column width)
- [ ] Issue 10 tested (extensibility)
- [ ] User acceptance testing complete
- [ ] Issues 5 & 10 marked as complete

---

**Created:** December 16, 2025  
**App:** F5656 - Commodity Hedge Cockpit Overview  
**Sprint:** Cluster C - Analytics & Oversight  
**Status:** Ready for implementation
