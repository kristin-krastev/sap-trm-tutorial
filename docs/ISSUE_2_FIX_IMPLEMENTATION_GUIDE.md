# Issue 2: Column Header Menu - Fix Implementation Guide
**Date:** December 8, 2025  
**App:** F5659 - Manage Commodity Counter Deal Request  
**Solution:** Add sortProperty arrays to p13nData in CustomColumns fragment

---

## 🎯 **What We're Fixing**

**Problem:** DCS and Hedge Book columns only show "Sort by ID" option, missing "Sort by Name"

**Root Cause:** p13nData in CustomColumns.fragment.xml is missing `sortProperty` arrays

**Solution:** Add `sortProperty` with both ID and Name fields to p13nData

---

## 📝 **Exact Code Changes**

### File to Edit:
`ibso.commodity.counterdeal.manage/webapp/ext/fragment/CustomColumns.fragment.xml`

### Changes Needed:

#### **Change 1: DCS Column**

**FIND this (current - WRONG):**
```xml
<table:Column sortProperty="CmmdtyHedgePlanExposureDCSID" filterProperty="CmmdtyHedgePlanExposureDCSID">
    <Label text="{/#CounterdealRequestType/CmmdtyHedgePlanExposureDCSID/@sap:label}"/>
    <table:customData>
        <core:CustomData key="p13nData"
            value='\{"columnKey": "CmmdtyHedgePlanExposureDCSID", "leadingProperty":"CmmdtyHedgePlanExposureDCSID", "autoColumnWidth": true, "type":"string", "columnIndex": "2"}'/>
    </table:customData>
```

**REPLACE with this (fixed - CORRECT):**
```xml
<table:Column sortProperty="CmmdtyHedgePlanExposureDCSID" filterProperty="CmmdtyHedgePlanExposureDCSID">
    <Label text="{/#CounterdealRequestType/CmmdtyHedgePlanExposureDCSID/@sap:label}"/>
    <table:customData>
        <core:CustomData key="p13nData"
            value='\{"columnKey": "CmmdtyHedgePlanExposureDCSID", "leadingProperty":"CmmdtyHedgePlanExposureDCSID", "sortProperty": ["CmmdtyHedgePlanExposureDCSID", "DerivativeContrSpecName"], "autoColumnWidth": true, "type":"string", "columnIndex": "2"}'/>
    </table:customData>
```

**What changed:** Added `"sortProperty": ["CmmdtyHedgePlanExposureDCSID", "DerivativeContrSpecName"],`

---

#### **Change 2: Hedge Book Column**

**FIND this (current - WRONG):**
```xml
<table:Column sortProperty="CmmdtyHdgPlanExposureHedgeBook" filterProperty="CmmdtyHdgPlanExposureHedgeBook">
    <Label text="{/#CounterdealRequestType/CmmdtyHdgPlanExposureHedgeBook/@sap:label}"/>
    <table:customData>
        <core:CustomData key="p13nData"
            value='\{"columnKey": "CmmdtyHdgPlanExposureHedgeBook", "leadingProperty":"CmmdtyHdgPlanExposureHedgeBook", "autoColumnWidth": true, "type":"string", "columnIndex": "3"}'/>
    </table:customData>
```

**REPLACE with this (fixed - CORRECT):**
```xml
<table:Column sortProperty="CmmdtyHdgPlanExposureHedgeBook" filterProperty="CmmdtyHdgPlanExposureHedgeBook">
    <Label text="{/#CounterdealRequestType/CmmdtyHdgPlanExposureHedgeBook/@sap:label}"/>
    <table:customData>
        <core:CustomData key="p13nData"
            value='\{"columnKey": "CmmdtyHdgPlanExposureHedgeBook", "leadingProperty":"CmmdtyHdgPlanExposureHedgeBook", "sortProperty": ["CmmdtyHdgPlanExposureHedgeBook", "CmmdtyHedgeBookDescription"], "autoColumnWidth": true, "type":"string", "columnIndex": "3"}'/>
    </table:customData>
```

**What changed:** Added `"sortProperty": ["CmmdtyHdgPlanExposureHedgeBook", "CmmdtyHedgeBookDescription"],`

---

## 🛠️ **Step-by-Step Implementation in BAS**

### **Step 1: Open Business Application Studio**

1. Go to your BTP Cockpit
2. Navigate to **Business Application Studio**
3. Click on your **Dev Space** (or create one if needed)
4. Wait for Dev Space to start (Status: RUNNING)
5. Click **Open** to launch BAS

---

### **Step 2: Open the Project**

**Option A: If project already open in BAS:**
1. Look in left sidebar → **Explorer**
2. Find project: `ibso.commodity.counterdeal.manage`

**Option B: If project not open:**
1. Click **File** → **Open Workspace**
2. Navigate to your project folder
3. Select `ibso.commodity.counterdeal.manage`
4. Click **Open**

---

### **Step 3: Navigate to the Fragment File**

1. In **Explorer** (left sidebar), expand folders:
   ```
   ibso.commodity.counterdeal.manage
   └── webapp
       └── ext
           └── fragment
               └── CustomColumns.fragment.xml  ← This file!
   ```

2. Click on **CustomColumns.fragment.xml** to open it

---

### **Step 4: Make the Changes**

#### **Find DCS Column (around line 3-15):**

1. Use **Ctrl+F** (Find) to search for: `CmmdtyHedgePlanExposureDCSID`
2. Find the `<table:Column>` section for DCS
3. Locate the `<core:CustomData key="p13nData"` line
4. Find the `value='...'` part

#### **Edit DCS Column:**

**Current value attribute:**
```javascript
value='\{"columnKey": "CmmdtyHedgePlanExposureDCSID", "leadingProperty":"CmmdtyHedgePlanExposureDCSID", "autoColumnWidth": true, "type":"string", "columnIndex": "2"}'
```

**Change to:**
```javascript
value='\{"columnKey": "CmmdtyHedgePlanExposureDCSID", "leadingProperty":"CmmdtyHedgePlanExposureDCSID", "sortProperty": ["CmmdtyHedgePlanExposureDCSID", "DerivativeContrSpecName"], "autoColumnWidth": true, "type":"string", "columnIndex": "2"}'
```

**What to add:** Insert after `"leadingProperty":"CmmdtyHedgePlanExposureDCSID",`
```javascript
"sortProperty": ["CmmdtyHedgePlanExposureDCSID", "DerivativeContrSpecName"],
```

---

#### **Find Hedge Book Column (around line 16-28):**

1. Use **Ctrl+F** to search for: `CmmdtyHdgPlanExposureHedgeBook`
2. Find the `<table:Column>` section for Hedge Book
3. Locate the `<core:CustomData key="p13nData"` line

#### **Edit Hedge Book Column:**

**Current value attribute:**
```javascript
value='\{"columnKey": "CmmdtyHdgPlanExposureHedgeBook", "leadingProperty":"CmmdtyHdgPlanExposureHedgeBook", "autoColumnWidth": true, "type":"string", "columnIndex": "3"}'
```

**Change to:**
```javascript
value='\{"columnKey": "CmmdtyHdgPlanExposureHedgeBook", "leadingProperty":"CmmdtyHdgPlanExposureHedgeBook", "sortProperty": ["CmmdtyHdgPlanExposureHedgeBook", "CmmdtyHedgeBookDescription"], "autoColumnWidth": true, "type":"string", "columnIndex": "3"}'
```

**What to add:** Insert after `"leadingProperty":"CmmdtyHdgPlanExposureHedgeBook",`
```javascript
"sortProperty": ["CmmdtyHdgPlanExposureHedgeBook", "CmmdtyHedgeBookDescription"],
```

---

#### **Save the File:**

1. **Ctrl+S** or **File** → **Save**
2. Look for disk icon - should no longer show unsaved indicator

---

### **Step 5: Verify Your Changes**

**Quick Check:**

1. DCS column p13nData should now have:
   ```
   "sortProperty": ["CmmdtyHedgePlanExposureDCSID", "DerivativeContrSpecName"],
   ```

2. Hedge Book column p13nData should now have:
   ```
   "sortProperty": ["CmmdtyHdgPlanExposureHedgeBook", "CmmdtyHedgeBookDescription"],
   ```

3. Both arrays should be properly formatted (square brackets, comma-separated, quoted strings)

---

## 📦 **Commit Changes to Git**

### **Step 6: Stage Your Changes**

**Option A: Using BAS Git UI (Recommended):**

1. Click **Source Control** icon in left sidebar (looks like branch/tree icon)
2. You should see **1 changed file:** `CustomColumns.fragment.xml`
3. Hover over the file → Click **+** (plus icon) to stage
4. File moves to **Staged Changes** section

**Option B: Using Terminal:**

```bash
cd /home/user/projects/ibso.commodity.counterdeal.manage
git add webapp/ext/fragment/CustomColumns.fragment.xml
```

---

### **Step 7: Commit Your Changes**

**Using BAS Git UI:**

1. In **Source Control** panel, find the **Message** text box at top
2. Type commit message:
   ```
   Fix: Add sortProperty arrays to DCS and Hedge Book columns for multi-sort support
   
   - Added sortProperty array to DCS column p13nData (ID + Name)
   - Added sortProperty array to Hedge Book column p13nData (ID + Description)
   - Enables "Sort by Name" option in column context menu
   - Resolves Issue 2: Column Header Menu sorting limitation
   ```

3. Click **✓ Commit** button (or Ctrl+Enter)

**Using Terminal:**

```bash
git commit -m "Fix: Add sortProperty arrays to DCS and Hedge Book columns for multi-sort support

- Added sortProperty array to DCS column p13nData (ID + Name)
- Added sortProperty array to Hedge Book column p13nData (ID + Description)
- Enables 'Sort by Name' option in column context menu
- Resolves Issue 2: Column Header Menu sorting limitation"
```

---

### **Step 8: Push to Remote Repository**

**Using BAS Git UI:**

1. Click **...** (three dots) in Source Control panel
2. Select **Push**
3. Wait for push to complete (status bar shows progress)

**Using Terminal:**

```bash
git push origin your-branch-name
```

**Note:** Replace `your-branch-name` with your actual branch (e.g., `main`, `develop`, `feature/fix-sorting`)

---

## 🚀 **Deploy and Test**

### **Step 9: Deploy to DEV System**

**Deployment method depends on your setup:**

#### **Option A: Automatic Deployment (CI/CD)**

If you have CI/CD pipeline:
1. Push to Git triggers automatic build
2. Wait for pipeline to complete
3. Check deployment status in BTP Cockpit

#### **Option B: Manual Deployment from BAS**

1. Right-click on project root folder
2. Select **Deploy** or **Build and Deploy**
3. Choose target system (DEV)
4. Wait for deployment to complete

#### **Option C: MTA Build and Deploy**

```bash
# Build MTA
mbt build

# Deploy to Cloud Foundry
cf deploy mta_archives/your-app.mtar
```

#### **Option D: Using Fiori Tools**

1. Open **Command Palette** (Ctrl+Shift+P)
2. Type: **Fiori: Deploy**
3. Follow prompts to deploy to DEV

---

### **Step 10: Test the Fix**

**Once deployed to DEV:**

1. **Clear Browser Cache:**
   - Press **Ctrl+Shift+Delete**
   - Clear cached images and files
   - Close browser, reopen

2. **Open the App:**
   - Navigate to your Fiori Launchpad (DEV)
   - Open **Manage Commodity Counter Deal Request** (F5659)

3. **Test DCS Column:**
   - Right-click on any row in the DCS column
   - Check context menu
   - **Expected:** See both options:
     - Sort by Derivative Contract Specification (ID)
     - Sort by Derivative Contract Specification Name

4. **Test Hedge Book Column:**
   - Right-click on any row in the Hedge Book column
   - Check context menu
   - **Expected:** See both options:
     - Sort by Hedge Book (ID)
     - Sort by Hedge Book Description

5. **Test Sorting Functionality:**
   - Click "Sort by Name" for DCS
   - **Expected:** Table sorts by DCS Name (alphabetically)
   - Click "Sort by ID" for DCS
   - **Expected:** Table sorts by DCS ID (numerically/alphanumerically)

---

### **Step 11: Verify Results**

**✅ Success Indicators:**

- [ ] Both "Sort by ID" and "Sort by Name" appear in context menu
- [ ] Sorting by ID works correctly
- [ ] Sorting by Name works correctly
- [ ] No errors in browser console (F12)
- [ ] Table displays correctly
- [ ] All other columns still work

**❌ If It Doesn't Work:**

1. **Check browser console** (F12) for errors
2. **Verify JSON syntax** in p13nData (commas, brackets, quotes)
3. **Hard refresh** browser (Ctrl+F5)
4. **Check field names** match CDS view exactly
5. **Review deployment logs** for errors

---

## 🔄 **Apply to Other Apps**

### **If F5659 Fix Works:**

#### **Apply to F5658 - Trader's Cockpit** (if needed)

**File to edit:** Different app/fragment (check with team)

**Same changes:** Add sortProperty arrays to DCS and Hedge Book columns

**Follow same steps:**
1. Open project in BAS
2. Find CustomColumns fragment
3. Make same changes
4. Commit and push
5. Deploy and test

---

## 📋 **Troubleshooting**

### **Issue: File Not Found in BAS**

**Solution:**
1. Check you're in correct project
2. Verify path: `webapp/ext/fragment/CustomColumns.fragment.xml`
3. Check if fragment might be in different location
4. Search for file: Ctrl+P, type "CustomColumns"

---

### **Issue: Cannot Commit (Git Conflicts)**

**Solution:**
1. Pull latest changes first: `git pull`
2. Resolve any conflicts
3. Then commit your changes

---

### **Issue: Deployment Fails**

**Solution:**
1. Check deployment logs for specific error
2. Verify you have deployment permissions
3. Check target system is accessible
4. Retry deployment
5. Contact DevOps if issue persists

---

### **Issue: Changes Not Visible in App**

**Solution:**
1. Hard refresh browser (Ctrl+F5)
2. Clear browser cache completely
3. Check app version deployed (footer)
4. Verify correct DEV system
5. Check CDN cache (may take 5-15 min)
6. Try incognito/private browser window

---

### **Issue: JSON Syntax Error**

**Common mistakes:**

❌ **Wrong:** Missing comma
```javascript
"leadingProperty":"CmmdtyHedgePlanExposureDCSID" "sortProperty": [...]
```

✅ **Correct:** Comma after leadingProperty
```javascript
"leadingProperty":"CmmdtyHedgePlanExposureDCSID", "sortProperty": [...]
```

---

❌ **Wrong:** Missing square brackets
```javascript
"sortProperty": "CmmdtyHedgePlanExposureDCSID", "DerivativeContrSpecName"
```

✅ **Correct:** Array with square brackets
```javascript
"sortProperty": ["CmmdtyHedgePlanExposureDCSID", "DerivativeContrSpecName"]
```

---

❌ **Wrong:** Single quotes in JSON
```javascript
"sortProperty": ['CmmdtyHedgePlanExposureDCSID', 'DerivativeContrSpecName']
```

✅ **Correct:** Double quotes in JSON
```javascript
"sortProperty": ["CmmdtyHedgePlanExposureDCSID", "DerivativeContrSpecName"]
```

---

## ✅ **Validation Checklist**

### Before Commit:
- [ ] Both columns updated (DCS and Hedge Book)
- [ ] JSON syntax is valid (no missing commas, brackets, quotes)
- [ ] Field names match CDS view exactly
- [ ] File saved (Ctrl+S)
- [ ] No syntax errors in BAS (check problems panel)

### After Commit:
- [ ] Changes committed to Git
- [ ] Changes pushed to remote
- [ ] Commit message is clear and descriptive

### After Deployment:
- [ ] App deployed to DEV successfully
- [ ] No deployment errors in logs
- [ ] App accessible in DEV system

### After Testing:
- [ ] Context menu shows both sort options for DCS
- [ ] Context menu shows both sort options for Hedge Book
- [ ] Sorting by ID works
- [ ] Sorting by Name works
- [ ] No browser console errors
- [ ] Other columns still work correctly

---

## 📞 **Need Help?**

### **If Stuck:**

1. **BAS Issues:**
   - Check BAS status in BTP Cockpit
   - Restart Dev Space if needed
   - Check internet connection

2. **Git Issues:**
   - Ask colleague for help with Git workflow
   - Check Git configuration
   - Verify remote repository access

3. **Deployment Issues:**
   - Check with DevOps team
   - Verify deployment permissions
   - Review deployment documentation

4. **Testing Issues:**
   - Verify correct system (DEV)
   - Check with team if changes deployed
   - Try different browser

---

## 🎯 **Expected Timeline**

- **Making changes in BAS:** 5-10 minutes
- **Commit and push:** 2-5 minutes
- **Deployment:** 5-15 minutes (depends on method)
- **Testing:** 5-10 minutes
- **Total:** 20-40 minutes

---

## 🎉 **Success!**

**Once working, you'll see:**

**DCS Column Context Menu:**
- Sort by Derivative Contract Specification ↑
- Sort by Derivative Contract Specification ↓
- **Sort by Derivative Contract Specification Name** ↑ ← NEW!
- **Sort by Derivative Contract Specification Name** ↓ ← NEW!
- (other options)

**Hedge Book Column Context Menu:**
- Sort by Hedge Book ↑
- Sort by Hedge Book ↓
- **Sort by Hedge Book Description** ↑ ← NEW!
- **Sort by Hedge Book Description** ↓ ← NEW!
- (other options)

---

**Good luck with the implementation!** 🚀

Let me know if you have any questions during the process!
