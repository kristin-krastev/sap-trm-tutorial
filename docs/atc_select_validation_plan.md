# Pragmatic Validation Plan: Can SELECT Replace READ ENTITIES?

**Date:** December 16, 2025  
**Approach:** Minimal-change validation for production code  
**Goal:** Test if SELECT works, report to team for decision

---

## 🎯 The Simple Plan

### Step 1: Run Existing Tests (Baseline) - 15 minutes

**Current State:** READ ENTITIES in line 21

**Action:**
```abap
" In SE80/Eclipse, run existing test class:
CLASS: TCL_CMM_RCLASSFCTN_HELPER
Method: Execute all tests (F9 or Ctrl+Shift+F10)
```

**Expected:** ✅ All tests pass (this is the baseline)

**Record:**
- Number of tests: 8
- All pass: ✅
- This proves current implementation works

---

### Step 2: Make SELECT Change (5 minutes)

**File:** CL_CMM_RECLASSIFICATION_HELPER  
**Method:** calculate_overhedge  
**Line:** 21-26

**CHANGE FROM:**
```abap
READ ENTITIES OF r_cmmdtyhdgrclassfctnrequesttp
  ENTITY cmmdtyreclassificationrequest
    BY \_rclassfctnitem
      FIELDS ( financialtransactionquantity )
        WITH VALUE #( ( %tky-reclassificationrequestuuid = is_overhedge-reclassificationrequestuuid ) )
  RESULT DATA(lt_rclassfctn_items).
```

**CHANGE TO:**
```abap
" Direct SELECT from items table
SELECT rclassfctn_itm_uuid as reclassificationitemuuid,
       reclassification_uuid as reclassificationrequestuuid,
       quantity as financialtransactionquantity
  FROM cmmt_reclass_itm
  WHERE reclassification_uuid = @is_overhedge-reclassificationrequestuuid
  INTO TABLE @DATA(lt_rclassfctn_items).
```

**Save** (don't activate yet!)

---

### Step 3: Run Tests Again (10 minutes)

**Action:** Run same test class

**Check:**
- ✅ All 8 tests still pass?
- ❌ Any tests fail?
- ⚠️ Any different behavior?

**Document results**

---

### Step 4: Add One Critical Test (30 minutes)

**If existing tests all pass**, add THIS test to catch edge cases:

**Add to test class:**
```abap
METHOD test_items_from_association FOR TESTING RAISING cx_static_check.
  " This specifically tests reading via association
  " Most likely to show difference between READ ENTITIES and SELECT
  
  DATA:
    ls_calculate        TYPE cl_cmm_reclassification_helper=>ty_is_overhedge,
    ls_overhedge        TYPE cl_cmm_reclassification_helper=>ty_es_overhedge,
    lt_hedging_area     TYPE STANDARD TABLE OF c_commodityhedgehedgingareatp,
    lt_rclassfctn_cmpst TYPE STANDARD TABLE OF i_cmmdtyhdgrclassfctnreqcmpst,
    lt_rclassfctn_item  TYPE STANDARD TABLE OF i_cmmdtyhdgrclassfctnitem,
    lt_fin_trans        TYPE STANDARD TABLE OF i_cmmdtyhdgfintransaction.

  " Setup test data with multiple items
  lt_hedging_area = VALUE #( ( hedgingareaidentification      = 1
                               hedgingareavalidfromdate       = sy-datum
                               hedgingareaversionstatus       = if_cmm_ha_hedging_area=>ha_status-released
                               hedgingareadedesignationmethod = '01'
                               hdggardedesignationmethodtext  = '01t' ) ).
  lo_environment->insert_test_data( lt_hedging_area ).

  ls_calculate-reclassificationrequestuuid    = 1.
  ls_calculate-reclassificationrequestdate    = sy-datum.
  ls_calculate-cmmdtyhedgeplnexposurequantity = 1000.
  ls_calculate-cmmdtyhdgplnexpsrquantityunit  = 'TO'.
  ls_calculate-cmmdtyhdgplnexpsrhedgingarea   = 1.

  lt_rclassfctn_cmpst = VALUE #( ( reclassificationrequestuuid = 1 ) ).
  lo_cds_environment->insert_test_data( lt_rclassfctn_cmpst ).

  " Create 3 items with clear values
  lt_rclassfctn_item = VALUE #( 
    ( reclassificationitemuuid     = 'ITEM1'
      reclassificationrequestuuid  = 1
      rclassfctnitemdealidentifier = 'DEAL1'
      rclassfctnitemcompanycode    = '1001' )
    ( reclassificationitemuuid     = 'ITEM2'
      reclassificationrequestuuid  = 1
      rclassfctnitemdealidentifier = 'DEAL2'
      rclassfctnitemcompanycode    = '1001' )
    ( reclassificationitemuuid     = 'ITEM3'
      reclassificationrequestuuid  = 1
      rclassfctnitemdealidentifier = 'DEAL3'
      rclassfctnitemcompanycode    = '1001' ) ).
  lo_cds_environment->insert_test_data( lt_rclassfctn_item ).

  lt_fin_trans = VALUE #( 
    ( fintransactiondealidentifier = 'DEAL1'
      fintransactioncompanycode    = '1001'
      financialtransactionquantity = 200 )
    ( fintransactiondealidentifier = 'DEAL2'
      fintransactioncompanycode    = '1001'
      financialtransactionquantity = 300 )
    ( fintransactiondealidentifier = 'DEAL3'
      fintransactioncompanycode    = '1001'
      financialtransactionquantity = 400 ) ).
  lo_cds_environment->insert_test_data( lt_fin_trans ).

  " Calculate - should sum all 3 items: 200 + 300 + 400 = 900
  cl_cmm_reclassification_helper=>calculate_overhedge(
    EXPORTING
      is_overhedge = ls_calculate
    IMPORTING
      es_overhedge = ls_overhedge ).

  " Expected: 1000 - 900 = 100
  cl_abap_unit_assert=>assert_equals(
    act  = ls_overhedge-rclassfctnreqexceededqtytext
    exp  = '100 TO'
    msg  = 'Should count all 3 items correctly' ).
ENDMETHOD.
```

---

### Step 5: Report Results (30 minutes)

**Create simple report:**

| Test | READ ENTITIES | SELECT | Conclusion |
|------|---------------|--------|------------|
| get_used_transactions | ✅ Pass | ✅/❌ | |
| handle_changedoc | ✅ Pass | ✅/❌ | |
| get_ha_method | ✅ Pass | ✅/❌ | |
| check_exposure | ✅ Pass | ✅/❌ | |
| perform_select | ✅ Pass | ✅/❌ | |
| get_latest_exposure | ✅ Pass | ✅/❌ | |
| calculate_overhedge | ✅ Pass | ✅/❌ | |
| get_date_before | ✅ Pass | ✅/❌ | |
| **NEW: items_from_assoc** | ✅ Pass | ✅/❌ | |

**Recommendation:**
- ✅ If all pass: SELECT is safe for this use case
- ❌ If any fail: Need proper refactoring

---

## 🔧 The SELECT Statement to Use

**Looking at the table structure** in the helper class methods:

```abap
" The items are stored in: cmmt_reclass_itm (active table)
" Field mapping:
"   reclassification_uuid → request UUID
"   quantity → item quantity

SELECT reclassification_uuid,
       reclassification_itm_uuid,
       quantity as financialtransactionquantity
  FROM cmmt_reclass_itm
  WHERE reclassification_uuid = @is_overhedge-reclassificationrequestuuid
  INTO TABLE @DATA(lt_rclassfctn_items).
  
" Map to expected structure if field names differ
" LOOP AT lt_rclassfctn_items ASSIGNING FIELD-SYMBOL(<item>).
"   <item>-reclassificationrequestuuid = <item>-reclassification_uuid.
" ENDLOOP.
```

---

## ⚠️ Important Considerations

### What SELECT Will Miss:

**1. Draft Items (if applicable):**
```abap
" Draft items in: cmmtd_rclass_itm (draft table)
" SELECT from active table won't see draft changes
```

**Solution if needed:**
```abap
" Read from BOTH tables
SELECT ... FROM cmmt_reclass_itm ...  " Active
UNION ALL
SELECT ... FROM cmmtd_rclass_itm ...  " Draft
WHERE draftentityoperationcode <> 'D'.  " Exclude deleted
```

**2. Just-Modified Items:**
- If item quantity changed in same transaction
- SELECT sees old value
- **Your existing test might not catch this!**

**3. Just-Deleted Items:**
- If item deleted in same transaction
- SELECT might still see it
- **Your existing test might not catch this!**

---

## 🧪 My Recommendation

**Minimum Viable Test:**

1. ✅ **Run existing tests** with SELECT (15 min)
2. ✅ **Add ONE critical test** - multiple items scenario (30 min)
3. ✅ **If all pass** → Report: "SELECT works, low risk"
4. ✅ **If any fail** → Report: "SELECT insufficient, need refactoring"

**Total time:** 1 hour to know the answer!

---

## 📋 Quick Implementation Tomorrow

**Morning (1 hour):**
1. Make SELECT change in ONE helper class (pilot)
2. Run existing test class
3. Add one multi-item test
4. Document results

**Report to team:**
```
Test Results: SELECT vs READ ENTITIES

Tested: CL_CMM_RECLASSIFICATION_HELPER
Existing tests: 8
New tests: 1
Total: 9 tests

Results with SELECT:
- Pass: X tests
- Fail: Y tests

Conclusion:
[ ] SELECT is safe - apply to all 4 classes (1 hour work)
[ ] SELECT has issues - recommend proper refactoring (1 day work)

Recommendation: [based on results]
```

---

## 🎯 This is the Right Approach!

**Why I like this:**
- ✅ **Data-driven decision** (not opinion-based)
- ✅ **Low risk** (test before changing)
- ✅ **Fast** (1 hour to know answer)
- ✅ **Production-safe** (minimal changes)
- ✅ **Team decision** (you provide data, they decide)

**Much better than my "perfect architecture" approach for live production code!** 👍

---

## 🚀 Ready for Tomorrow?

**I'll help you:**
1. ✅ Make the SELECT change
2. ✅ Run the tests
3. ✅ Add critical scenario (if needed)
4. ✅ Document results
5. ✅ Create recommendation report

**Then team decides:** Quick fix (SELECT) or proper refactoring?

**Sound good?** 😊