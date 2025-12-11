# Issue 2 Fix - Quick Reference Card
**App:** F5659 - Counter Deal Request  
**File:** `webapp/ext/fragment/CustomColumns.fragment.xml`

---

## 🎯 Quick Summary

**Add this to p13nData for both columns:**

### DCS Column - Add:
```javascript
"sortProperty": ["CmmdtyHedgePlanExposureDCSID", "DerivativeContrSpecName"],
```

### Hedge Book Column - Add:
```javascript
"sortProperty": ["CmmdtyHdgPlanExposureHedgeBook", "CmmdtyHedgeBookDescription"],
```

---

## 📝 Exact Changes

### Change 1: DCS Column (around line 5)

**BEFORE:**
```xml
<core:CustomData key="p13nData"
    value='\{"columnKey": "CmmdtyHedgePlanExposureDCSID", "leadingProperty":"CmmdtyHedgePlanExposureDCSID", "autoColumnWidth": true, "type":"string", "columnIndex": "2"}'/>
```

**AFTER:**
```xml
<core:CustomData key="p13nData"
    value='\{"columnKey": "CmmdtyHedgePlanExposureDCSID", "leadingProperty":"CmmdtyHedgePlanExposureDCSID", "sortProperty": ["CmmdtyHedgePlanExposureDCSID", "DerivativeContrSpecName"], "autoColumnWidth": true, "type":"string", "columnIndex": "2"}'/>
```

---

### Change 2: Hedge Book Column (around line 18)

**BEFORE:**
```xml
<core:CustomData key="p13nData"
    value='\{"columnKey": "CmmdtyHdgPlanExposureHedgeBook", "leadingProperty":"CmmdtyHdgPlanExposureHedgeBook", "autoColumnWidth": true, "type":"string", "columnIndex": "3"}'/>
```

**AFTER:**
```xml
<core:CustomData key="p13nData"
    value='\{"columnKey": "CmmdtyHdgPlanExposureHedgeBook", "leadingProperty":"CmmdtyHdgPlanExposureHedgeBook", "sortProperty": ["CmmdtyHdgPlanExposureHedgeBook", "CmmdtyHedgeBookDescription"], "autoColumnWidth": true, "type":"string", "columnIndex": "3"}'/>
```

---

## ⚡ Quick Steps

1. **Open BAS** → Find project → Open `CustomColumns.fragment.xml`
2. **Find DCS column** (Ctrl+F: "CmmdtyHedgePlanExposureDCSID")
3. **Add sortProperty array** after leadingProperty (don't forget comma!)
4. **Find Hedge Book column** (Ctrl+F: "CmmdtyHdgPlanExposureHedgeBook")  
5. **Add sortProperty array** after leadingProperty
6. **Save** (Ctrl+S)
7. **Commit:** 
   ```
   git add webapp/ext/fragment/CustomColumns.fragment.xml
   git commit -m "Fix: Add sortProperty arrays for DCS and Hedge Book multi-sort"
   git push
   ```
8. **Deploy to DEV**
9. **Test:** Right-click columns → See both sort options ✅

---

## ✅ Success Check

**Expected Result:**
- Right-click DCS column → See "Sort by Name" option
- Right-click Hedge Book → See "Sort by Description" option
- Both sorting options work correctly

---

## 🚨 Common Mistakes

❌ **Missing comma after leadingProperty**
❌ **Single quotes instead of double quotes in JSON**
❌ **Missing square brackets around array**
❌ **Typo in field names**

✅ **Double-check JSON syntax!**

---

**Full Guide:** `/workspace/docs/ISSUE_2_FIX_IMPLEMENTATION_GUIDE.md`
