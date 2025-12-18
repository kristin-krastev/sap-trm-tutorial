# Lesson Learned: Don't Forget the Calculation Class

## 📅 Date: December 18, 2025

## 🔴 What Happened

After successfully releasing transport ERXK657609 with the ATC fix for `CL_CMM_COUNTERDEAL_HELPER`, received error in QM7 test system:

```
Syntax error in program CL_CMM_COUNTERDEAL_CALC=======CP
No value was passed to the mandatory parameter "IT_CNTRDEAL_ITEM"
```

---

## 🔍 Root Cause

When changing the `calculate_overhedge` method signature to add `it_cntrdeal_item` parameter, we updated:
- ✅ The handler class (`CL_BP_CMM_COUNTER_DEAL_REQUEST`)
- ✅ The test classes

But we **forgot:**
- ❌ The calculation class (`CL_CMM_COUNTERDEAL_CALC`)
- ❌ To use "Where Used" to find ALL callers

---

## 💡 Why the Calculation Class Matters

`CL_CMM_COUNTERDEAL_CALC` is a **SADL Exit** that:
- Runs when the CDS view is read (display operations)
- Calculates overhedge fields for the UI list
- Runs in a **different context** than the save sequence
- Has **no access** to the handler class buffer

**It must read items directly and pass to the helper!**

---

## 🔧 The Fix

In `CL_CMM_COUNTERDEAL_CALC`, method `if_sadl_exit_calc_element_read~calculate`:

**Added before the helper call:**

```abap
" Read items for this request
DATA lt_cntrdeal_item TYPE cl_cmm_counterdeal_helper=>ty_t_cntrdeal_item.
CLEAR lt_cntrdeal_item.

READ ENTITIES OF r_cmmdtyhdgcntrdealrequesttp
  ENTITY commoditycounterdealrequest
    BY \_cntrdealitem
      FIELDS ( counterdealitemuuid financialtransactionquantity )
        WITH VALUE #( ( %tky-counterdealrequestuuid = <ls_original_data>-counterdealrequestuuid
                        %tky-%is_draft              = if_abap_behv=>mk-on ) )
  RESULT DATA(lt_items_draft).

IF lt_items_draft IS NOT INITIAL.
  lt_cntrdeal_item = CORRESPONDING #( lt_items_draft ).
ELSE.
  READ ENTITIES OF r_cmmdtyhdgcntrdealrequesttp
    ENTITY commoditycounterdealrequest
      BY \_cntrdealitem
        FIELDS ( counterdealitemuuid financialtransactionquantity )
          WITH VALUE #( ( %tky-counterdealrequestuuid = <ls_original_data>-counterdealrequestuuid ) )
    RESULT DATA(lt_items_active).
  
  lt_cntrdeal_item = CORRESPONDING #( lt_items_active ).
ENDIF.
```

**Updated the helper call:**

```abap
cl_cmm_counterdeal_helper=>calculate_overhedge(
  EXPORTING
    is_overhedge      = ls_calculate
    it_cntrdeal_item  = lt_cntrdeal_item  " ← ADDED
  IMPORTING
    es_overhedge = ls_overhedge ).
```

---

## ✅ Prevention Strategy

### **Before Changing Any Method Signature:**

1. **Use "Where Used"** to find all callers
   - Right-click method → "Where Used" (Ctrl+Shift+G)
   - Document all classes that call it
   
2. **Identify caller types:**
   - Handler classes (save sequence)
   - Calculation classes (SADL exits)
   - Other helper classes
   - Test classes
   
3. **Update ALL callers** before activating

4. **Add ALL to transport** together

---

## 📚 Key Insights

### **Two Execution Contexts:**

**Context 1: Save Operations (Handler Class)**
- Determination reads items → stores in buffer
- save_modified retrieves from buffer → passes to helper
- Buffer exists only during save sequence
- **READ ENTITIES in late save = ATC violation**

**Context 2: Display Operations (SADL Exit)**
- Runs when CDS view is read
- No save sequence happening
- No access to handler buffer
- Must read items directly
- **READ ENTITIES allowed here** (not late save phase!)

### **Why Both Need Items:**

The helper method needs items to calculate "After Selection" values:
```abap
LOOP AT it_cntrdeal_item INTO DATA(ls_item).
  lv_requestquantity += ls_item-financialtransactionquantity.
ENDLOOP.
```

Without items:
- `lv_requestquantity` = 0
- "After Utilization" = incorrect
- "After Overhedge" = incorrect
- UI shows wrong values

---

## 🎯 Apply to Remaining Classes

For each of the 4 remaining helper classes, remember:

**Files to check and update:**
1. Helper class (signature change)
2. Handler class (determination + buffer)
3. **Calculation class** ← DON'T FORGET!
4. Behavior definition
5. Test classes
6. **Any other callers** found via "Where Used"

**Add ALL to transport together!**

---

## 📊 Impact Assessment

**Positive:**
- ✅ Caught in test system (QM7), not production
- ✅ Quick diagnosis and fix (same morning)
- ✅ No user impact
- ✅ Valuable lesson learned
- ✅ Process improved for remaining classes

**Time Cost:**
- Initial fix: ~5 hours (Day 1)
- Missed dependency fix: ~30 minutes (Day 2)
- **Total additional effort:** Minimal

**Knowledge Gain:**
- Understanding of SADL exit execution context
- Importance of "Where Used" analysis
- Complete dependency mapping
- **Value:** High (prevents same mistake 4 more times)

---

## 🔄 Process Improvement

### **Old Process:**
1. Fix ATC violation in helper
2. Update handler class
3. Update tests
4. Release

### **New Process:**
1. Fix ATC violation in helper
2. **Run "Where Used" on modified method**
3. **Document all callers**
4. Update handler class
5. **Update calculation class**
6. **Update any other callers**
7. Update tests
8. **Verify all callers in transport**
9. Release

---

## 📝 Naming Pattern Discovered

For RAP business objects, the pattern appears to be:
- Helper: `CL_CMM_[ENTITY]_HELPER`
- Handler: `CL_BP_CMM_[ENTITY]_REQUEST`
- **Calculation: `CL_CMM_[ENTITY]_CALC`** ← Remember to check for this!

**For remaining classes, search for:**
- `CL_CMM_DESIGNATIONREQ_CALC`
- `CL_CMM_MIGRATIONREQUEST_CALC`
- `CL_CMM_RECLASSIFICATION_CALC`

---

## 💪 Positive Takeaway

**This was not a failure - it was a learning opportunity!**

- Test systems exist for exactly this reason
- Finding issues early is GOOD
- Each mistake makes the next implementation better
- The remaining 4 classes will be smoother

**Quote to remember:**
> "Experience is the name everyone gives to their mistakes." - Oscar Wilde

---

## ✅ Resolution Status

- **Issue:** RESOLVED
- **Fix applied:** ✅ `CL_CMM_COUNTERDEAL_CALC` updated
- **Transport:** To be created and released
- **Documentation:** ✅ Complete
- **Lessons applied:** ✅ Ready for remaining 4 classes

---

**Date Documented:** December 18, 2025  
**Developer:** KK  
**Status:** ✅ Lesson Learned and Process Updated
