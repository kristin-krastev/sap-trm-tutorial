# ATC Fix: READ_IN_LATE_SAVE Violation - Complete Solution

**Issue:** RAP Contract Violation in CL_CMM_RECLASSIFICATION_HELPER=>CALCULATE_OVERHEDGE  
**Date:** December 15, 2025  
**Priority:** High (Priority 2)  
**Status:** Ready for Implementation (Pending Authorization)

---

## Problem Analysis

### Current Violation
**Location:** `CL_CMM_RECLASSIFICATION_HELPER` → `CALCULATE_OVERHEDGE` method, Line 21

**The Problem:**
```abap
READ ENTITIES OF r_cmmdtyhdgrclassfctnrequesttp
  ENTITY cmmdtyreclassificationrequest
    BY \_rclassfctnitem
      FIELDS ( financialtransactionquantity )
        WITH VALUE #( ( %tky-reclassificationrequestuuid = is_overhedge-reclassificationrequestuuid ) )
  RESULT DATA(lt_rclassfctn_items).
```

This READ ENTITIES is being called during the **late save phase**, which violates RAP contract rules.

### Why This is a Problem
1. ❌ Data consistency risk - reading partially committed data
2. ❌ Potential deadlocks
3. ❌ Violates RAP architectural contract
4. ❌ May fail on future SAP upgrades
5. ❌ Cannot be suppressed with pragma

---

## Recommended Solution: Refactor Helper Method

**Approach:** Remove READ from helper, pass data as parameter from caller (who reads in early phase)

**Effort:** ⏱️ 2-3 hours including testing  
**Risk:** ⚠️ Low-Medium (clean refactoring)  
**Files to Change:** 2 (helper class definition + implementation, behavior implementation class)

---

## Implementation Steps

### Step 1: Update Helper Method Signature

**File:** `CL_CMM_RECLASSIFICATION_HELPER` (Class Definition)

#### BEFORE (Current):
```abap
CLASS cl_cmm_reclassification_helper DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CLASS-METHODS calculate_overhedge
      IMPORTING
        is_overhedge        TYPE <type>
      EXPORTING
        es_overhedge        TYPE <type>.
        
ENDCLASS.
```

#### AFTER (Fixed):
```abap
CLASS cl_cmm_reclassification_helper DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CLASS-METHODS calculate_overhedge
      IMPORTING
        is_overhedge        TYPE <type>
        it_rclassfctn_items TYPE <table_type>  " ← ADD THIS PARAMETER
      EXPORTING
        es_overhedge        TYPE <type>.
        
ENDCLASS.
```

**Change:** Add `it_rclassfctn_items` as importing parameter

---

### Step 2: Update Helper Method Implementation

**File:** `CL_CMM_RECLASSIFICATION_HELPER` (Class Implementation)

#### BEFORE (Current - Line 21 violates contract):
```abap
METHOD calculate_overhedge.

  DATA:
    lv_items_quantity      TYPE ftr_quan,
    lv_exceeded_quantity   TYPE ftr_quan,
    lv_exceeded_quantity_t TYPE char_132.

  CLEAR: lv_items_quantity.

  cl_cmm_hedgereq_helper=>get_hedging_area_method(
    EXPORTING
      iv_hedging_area          = is_overhedge-cmmdtyhdgplnexpsrhedgingarea
      iv_valid_from            = is_overhedge-reclassificationrequestdate
    IMPORTING
      ev_hedgingareamethod     = es_overhedge-hedgingareadedesignationmethod
      ev_hedgingareamethodtext = es_overhedge-hdggardedesignationmethodtext ).

  "Get No. of decimal value for quantity unit
  DATA(lv_andec) = cl_cmm_hedgereq_helper=>get_andec( is_overhedge-cmmdtyhdgplnexpsrquantityunit ).

  " ❌ VIOLATION - Reading during late save
  READ ENTITIES OF r_cmmdtyhdgrclassfctnrequesttp
    ENTITY cmmdtyreclassificationrequest
      BY \_rclassfctnitem
        FIELDS ( financialtransactionquantity )
          WITH VALUE #( ( %tky-reclassificationrequestuuid = is_overhedge-reclassificationrequestuuid ) )
    RESULT DATA(lt_rclassfctn_items).

  LOOP AT lt_rclassfctn_items INTO DATA(ls_rclassfctn_items).
    lv_items_quantity += ls_rclassfctn_items-financialtransactionquantity.
  ENDLOOP.

  lv_exceeded_quantity = is_overhedge-cmmdtyhedgeplnexposurequantity - lv_items_quantity.
  WRITE lv_exceeded_quantity TO lv_exceeded_quantity_t DECIMALS lv_andec NO-SIGN LEFT-JUSTIFIED.

  IF lv_exceeded_quantity < 0.
    lv_exceeded_quantity_t = insert( val = lv_exceeded_quantity_t
                                     sub = '-' ).
    es_overhedge-rclassfctnreqexcdqtycritlty = 1.

  ENDIF.

  es_overhedge-rclassfctnreqexceededqtytext = |{ lv_exceeded_quantity_t } { is_overhedge-cmmdtyhdgplnexpsrquantityunit }|.
ENDMETHOD.
```

#### AFTER (Fixed - No READ, uses parameter):
```abap
METHOD calculate_overhedge.

  DATA:
    lv_items_quantity      TYPE ftr_quan,
    lv_exceeded_quantity   TYPE ftr_quan,
    lv_exceeded_quantity_t TYPE char_132.

  CLEAR: lv_items_quantity.

  cl_cmm_hedgereq_helper=>get_hedging_area_method(
    EXPORTING
      iv_hedging_area          = is_overhedge-cmmdtyhdgplnexpsrhedgingarea
      iv_valid_from            = is_overhedge-reclassificationrequestdate
    IMPORTING
      ev_hedgingareamethod     = es_overhedge-hedgingareadedesignationmethod
      ev_hedgingareamethodtext = es_overhedge-hdggardedesignationmethodtext ).

  "Get No. of decimal value for quantity unit
  DATA(lv_andec) = cl_cmm_hedgereq_helper=>get_andec( is_overhedge-cmmdtyhdgplnexpsrquantityunit ).

  " ✅ FIXED - Use passed items parameter instead of reading
  " Items are now passed from caller (who reads in early phase)
  LOOP AT it_rclassfctn_items INTO DATA(ls_rclassfctn_items).
    lv_items_quantity += ls_rclassfctn_items-financialtransactionquantity.
  ENDLOOP.

  lv_exceeded_quantity = is_overhedge-cmmdtyhedgeplnexposurequantity - lv_items_quantity.
  WRITE lv_exceeded_quantity TO lv_exceeded_quantity_t DECIMALS lv_andec NO-SIGN LEFT-JUSTIFIED.

  IF lv_exceeded_quantity < 0.
    lv_exceeded_quantity_t = insert( val = lv_exceeded_quantity_t
                                     sub = '-' ).
    es_overhedge-rclassfctnreqexcdqtycritlty = 1.

  ENDIF.

  es_overhedge-rclassfctnreqexceededqtytext = |{ lv_exceeded_quantity_t } { is_overhedge-cmmdtyhdgplnexpsrquantityunit }|.
ENDMETHOD.
```

**Changes:**
1. ❌ **Remove:** READ ENTITIES statement (lines 21-26)
2. ✅ **Update:** LOOP AT to use `it_rclassfctn_items` parameter instead of `lt_rclassfctn_items` local variable

---

### Step 3: Update All Callers of calculate_overhedge

**File:** Behavior Implementation Class (likely `LHC_RECLASSIFICATIONREQUEST` or similar)

#### BEFORE (Caller in Late Save - Causes Violation):
```abap
METHOD save_modified.
  " This runs in LATE SAVE phase
  
  " ... other code ...
  
  LOOP AT overhedges INTO DATA(ls_overhedge).
    cl_cmm_reclassification_helper=>calculate_overhedge(
      EXPORTING
        is_overhedge = ls_overhedge
      IMPORTING
        es_overhedge = ls_result ).
    
    " ... process result ...
  ENDLOOP.
  
ENDMETHOD.
```

#### AFTER (Move to Determination - Early Phase):
```abap
METHOD determine_overhedge_calculation.
  " This runs in EARLY phase (before save) - SAFE for READ ENTITIES
  
  " ✅ Read items in early phase (allowed by RAP contract)
  READ ENTITIES OF r_cmmdtyhdgrclassfctnrequesttp
    ENTITY cmmdtyreclassificationrequest
      BY \_rclassfctnitem
        FIELDS ( reclassificationrequestuuid financialtransactionquantity )
          WITH CORRESPONDING #( keys )
    RESULT DATA(lt_all_items).

  LOOP AT keys INTO DATA(ls_key).
    " Get overhedge data for this request
    READ ENTITIES OF r_cmmdtyhdgrclassfctnrequesttp
      ENTITY cmmdtyreclassificationrequest
        ALL FIELDS WITH VALUE #( ( %tky = ls_key-%tky ) )
      RESULT DATA(lt_overhedge).
    
    CHECK lt_overhedge IS NOT INITIAL.
    DATA(ls_overhedge) = lt_overhedge[ 1 ].
    
    " Filter items for this specific request
    DATA(lt_filtered_items) = FILTER #( lt_all_items 
      WHERE reclassificationrequestuuid = ls_overhedge-reclassificationrequestuuid ).
    
    " ✅ Call helper with items as parameter
    cl_cmm_reclassification_helper=>calculate_overhedge(
      EXPORTING
        is_overhedge        = ls_overhedge
        it_rclassfctn_items = lt_filtered_items  " ← Pass items here
      IMPORTING
        es_overhedge = DATA(ls_result) ).
    
    " Store calculated result back to entity
    MODIFY ENTITIES OF r_cmmdtyhdgrclassfctnrequesttp
      ENTITY cmmdtyreclassificationrequest
      UPDATE FIELDS ( 
        rclassfctnreqexceededqtytext 
        rclassfctnreqexcdqtycritlty 
        hedgingareadedesignationmethod 
        hdggardedesignationmethodtext )
      WITH VALUE #( ( 
        %tky = ls_key-%tky
        rclassfctnreqexceededqtytext = ls_result-rclassfctnreqexceededqtytext
        rclassfctnreqexcdqtycritlty = ls_result-rclassfctnreqexcdqtycritlty
        hedgingareadedesignationmethod = ls_result-hedgingareadedesignationmethod
        hdggardedesignationmethodtext = ls_result-hdggardedesignationmethodtext 
      ) ).
  ENDLOOP.
  
ENDMETHOD.

METHOD save_modified.
  " ✅ No longer needs to call calculate_overhedge
  " Calculation already done in determination phase
  
  " ... other late save operations ...
  
ENDMETHOD.
```

**Changes:**
1. ✅ **Add:** Determination method (runs before save)
2. ✅ **Move:** READ ENTITIES to determination (early phase)
3. ✅ **Add:** Pass `it_rclassfctn_items` parameter to helper
4. ✅ **Update:** Store calculated results back to entity
5. ✅ **Remove:** calculate_overhedge call from save_modified (if present)

---

### Step 4: Register Determination in Behavior Definition

**File:** Behavior Definition (`.bdef.asbdef` file)

#### Add to Behavior Definition:
```abap
define behavior for R_CMMDTYHDGRCLASSFCTNREQUESTTP alias ReclassificationRequest
persistent table <table_name>
lock master
authorization master ( instance )
etag master <etag_field>
{
  // ... existing fields and operations ...
  
  // ✅ ADD THIS DETERMINATION
  determination determine_overhedge_calculation on modify 
    { create; update; }
  
  // ... rest of definition ...
}
```

---

## Testing Plan

### 1. Unit Testing (30 min)

**Test Helper Method in Isolation:**
```abap
METHOD test_calculate_overhedge.
  " Given
  DATA(ls_overhedge) = VALUE <type>(
    cmmdtyhdgplnexpsrhedgingarea = '001'
    reclassificationrequestdate = sy-datum
    cmmdtyhedgeplnexposurequantity = 1000
    cmmdtyhdgplnexpsrquantityunit = 'TO' ).
    
  DATA(lt_items) = VALUE <table_type>(
    ( financialtransactionquantity = 300 )
    ( financialtransactionquantity = 400 ) ).
  
  " When
  cl_cmm_reclassification_helper=>calculate_overhedge(
    EXPORTING
      is_overhedge = ls_overhedge
      it_rclassfctn_items = lt_items
    IMPORTING
      es_overhedge = DATA(ls_result) ).
  
  " Then
  cl_abap_unit_assert=>assert_equals(
    act = ls_result-rclassfctnreqexceededqtytext
    exp = '300.000 TO'  " 1000 - (300 + 400) = 300
    msg = 'Overhedge calculation incorrect' ).
ENDMETHOD.
```

### 2. Integration Testing (1 hour)

**Scenario 1: Create Reclassification Request with Items**
- Create request with exposure quantity 1000
- Add items totaling 700
- Expected: Overhedge = 300, no criticality

**Scenario 2: Overhedge (Positive)**
- Create request with exposure quantity 1000
- Add items totaling 600
- Expected: Overhedge = 400, no criticality, positive number

**Scenario 3: Exceeded (Negative Overhedge)**
- Create request with exposure quantity 1000
- Add items totaling 1200
- Expected: Overhedge = -200, criticality = 1, negative sign

**Scenario 4: Update Items**
- Create request with items
- Add more items
- Expected: Overhedge recalculates correctly

**Scenario 5: Delete Items**
- Create request with items
- Delete some items
- Expected: Overhedge recalculates correctly

**Scenario 6: Zero Items**
- Create request with no items
- Expected: Overhedge = full exposure quantity

**Scenario 7: Concurrent Requests**
- Create multiple requests simultaneously
- Expected: All calculate correctly, no data corruption

### 3. Performance Testing (30 min)

- Test with 1 request, 10 items
- Test with 10 requests, 100 items each
- Test with 100 requests, 10 items each
- Compare performance vs. current implementation
- Expected: No significant degradation

### 4. ATC Check (5 min)

- Run ATC on modified classes
- Expected: No READ_IN_LATE_SAVE violation
- Expected: No new violations introduced

---

## Verification Checklist

Before considering the fix complete:

- [ ] **Code Changes Complete**
  - [ ] Helper method signature updated
  - [ ] Helper method implementation updated (READ removed)
  - [ ] Determination method created
  - [ ] Determination registered in behavior definition
  - [ ] All callers updated

- [ ] **Testing Complete**
  - [ ] Unit tests pass
  - [ ] Scenario 1: Create with items → Pass
  - [ ] Scenario 2: Overhedge positive → Pass
  - [ ] Scenario 3: Exceeded negative → Pass
  - [ ] Scenario 4: Update items → Pass
  - [ ] Scenario 5: Delete items → Pass
  - [ ] Scenario 6: Zero items → Pass
  - [ ] Scenario 7: Concurrent requests → Pass
  - [ ] Performance testing → No degradation

- [ ] **Quality Checks**
  - [ ] ATC check passes (no READ_IN_LATE_SAVE)
  - [ ] No new ATC violations
  - [ ] Code review completed
  - [ ] Documentation updated

- [ ] **Deployment**
  - [ ] Transport created
  - [ ] Changes deployed to DEV
  - [ ] Tested in DEV
  - [ ] Changes deployed to QA
  - [ ] User acceptance testing
  - [ ] Approved for production

---

## Alternative Solutions (If Needed)

### Alternative 1: Cache Pattern

**Use if:** Multiple places call helper, want to minimize caller changes

```abap
CLASS lhc_reclassification IMPLEMENTATION.
  " Cache at class level
  CLASS-DATA: gt_cached_items TYPE TABLE OF ...,
              gv_cache_valid TYPE abap_bool.

  METHOD determine_cache_items.
    " Read once in early phase
    READ ENTITIES OF r_cmmdtyhdgrclassfctnrequesttp
      ENTITY cmmdtyreclassificationrequest
        BY \_rclassfctnitem
          FIELDS ( financialtransactionquantity )
            WITH CORRESPONDING #( keys )
      RESULT gt_cached_items.
    
    gv_cache_valid = abap_true.
  ENDMETHOD.
  
  METHOD calculate_overhedge_wrapper.
    " Use cache instead of reading
    CHECK gv_cache_valid = abap_true.
    
    DATA(lt_items) = FILTER #( gt_cached_items
      WHERE reclassificationrequestuuid = is_overhedge-reclassificationrequestuuid ).
    
    cl_cmm_reclassification_helper=>calculate_overhedge(
      EXPORTING
        is_overhedge = is_overhedge
        it_rclassfctn_items = lt_items
      IMPORTING
        es_overhedge = es_overhedge ).
  ENDMETHOD.
  
  METHOD save_modified.
    " Clear cache
    CLEAR: gt_cached_items, gv_cache_valid.
  ENDMETHOD.
ENDCLASS.
```

**Pros:** Less caller changes  
**Cons:** More complex, cache management overhead

---

## Rollback Plan

If issues occur after deployment:

1. **Immediate:** Revert transport
2. **Temporary:** Suppress ATC check (if possible - though this one cannot be suppressed)
3. **Investigation:** Analyze failure scenario
4. **Fix:** Apply corrected solution
5. **Redeploy:** Test thoroughly before redeployment

---

## Summary

| Aspect | Details |
|--------|---------|
| **Problem** | READ ENTITIES in late save phase violates RAP contract |
| **Solution** | Pass items as parameter, move READ to early phase |
| **Effort** | 2-3 hours including testing |
| **Risk** | Low-Medium (clean refactoring) |
| **Files Changed** | 2-3 (helper class, behavior implementation, behavior definition) |
| **Lines Changed** | ~50 lines total |
| **Testing** | 7 scenarios + performance testing |
| **ATC Result** | Violation resolved ✅ |

---

## Authorization & Ownership

**Original Code Owner:** VOEROES (no longer on team)  
**Current Responsibility:** Your team (FIN_CMM_CMDTY_HEDGE_REQUESTS package)  
**Authorization Status:** Pending approval for implementation  
**Recommendation:** Senior developer should review before implementation

---

## Contact for Questions

- Package: FIN_CMM_CMDTY_HEDGE_REQUESTS
- Class: CL_CMM_RECLASSIFICATION_HELPER
- Method: CALCULATE_OVERHEDGE
- ATC Check: RAP Contract Check - READ_IN_LATE_SAVE
- Priority: 2 (High)

---

**Document Created:** December 15, 2025  
**Status:** Ready for Implementation (Pending Authorization)  
**Next Step:** Obtain approval from senior developer, then implement

---

## Quick Implementation Checklist

For quick reference when implementing:

```
✅ Step 1: Add parameter to helper method signature (3 lines)
✅ Step 2: Remove READ from helper, use parameter (delete 6 lines, modify 1 line)
✅ Step 3: Create determination method in behavior implementation (~30 lines)
✅ Step 4: Register determination in behavior definition (3 lines)
✅ Step 5: Test all scenarios (7 scenarios)
✅ Step 6: Run ATC check (verify violation resolved)
✅ Step 7: Deploy to DEV, test, promote to QA
```

**Estimated Time:** 2-3 hours  
**Estimated Lines Changed:** ~50 lines

---

**Ready to implement when authorized!** 🚀
