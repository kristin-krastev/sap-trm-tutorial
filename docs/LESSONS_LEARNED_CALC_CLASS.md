# 🎓 Lessons Learned: Calculation Class Fix (Dec 17, 2025)

## 📋 Summary
After successfully fixing the ATC violations in `CL_CMM_COUNTERDEAL_HELPER` and updating the handler class, we encountered a **runtime error in QM7** that initially looked like a transport issue but turned out to be a **propagation delay**.

---

## 🐛 The Issue

### **Error Seen in QM7:**
- **ST22 Dump:** Syntax error in program `CL_CMM_COUNTERDEAL_CALC=======CP`
- **Message:** "No value was passed to the mandatory parameter 'IT_CNTRDEAL_ITEM'"
- **Location:** When creating a Counter Deal Request in Fiori app

### **Root Cause:**
`CL_CMM_COUNTERDEAL_CALC` (a SADL exit/calculation class) was **not included in the original transport** and was calling the old signature of `calculate_overhedge`.

---

## ✅ The Fix

### **Step 1: Identify the Missing Dependency**
```
Problem: CL_CMM_COUNTERDEAL_CALC calls calculate_overhedge but wasn't updated
Solution: Update the calc class to pass it_cntrdeal_item parameter
```

### **Step 2: Update CL_CMM_COUNTERDEAL_CALC**

**Key Implementation Points:**
1. **Fetch items using READ ENTITIES** (allowed in SADL exits, unlike late save)
2. **Use batched READ ENTITIES** (avoid "EML in loop" ATC violation)
3. **Handle both draft and active entity instances**
4. **Group items by request UUID** for efficient lookup

**Final Working Code:**

```abap
METHOD if_sadl_exit_calc_element_read~calculate.

  DATA:
    ls_calculate     TYPE cl_cmm_counterdeal_helper=>ty_is_overhedge,
    ls_overhedge     TYPE cl_cmm_counterdeal_helper=>ty_es_overhedge,
    lt_original_data TYPE STANDARD TABLE OF c_cmmdtyhdgcntrdealrequesttp WITH DEFAULT KEY.

  lt_original_data = CORRESPONDING #( it_original_data ).

  " Step 1: Build map structure for items
  DATA: BEGIN OF ls_items_map,
          request_uuid TYPE sysuuid_x16,
          items        TYPE cl_cmm_counterdeal_helper=>ty_t_cntrdeal_item,
        END OF ls_items_map,
        lt_items_map LIKE STANDARD TABLE OF ls_items_map.

  DATA lt_request_uuids TYPE STANDARD TABLE OF sysuuid_x16.

  " Step 2: Collect all request UUIDs that need calculation
  LOOP AT lt_original_data INTO DATA(ls_data_temp)
    WHERE ( counterdealrequeststatus = if_cmm_cntrdeal_request=>cd_status-created
         OR counterdealrequeststatus = if_cmm_cntrdeal_request=>cd_status-mark_for_release
         OR counterdealrequeststatus = if_cmm_cntrdeal_request=>cd_status-tobereleased ).
    APPEND ls_data_temp-counterdealrequestuuid TO lt_request_uuids.
  ENDLOOP.

  " Step 3: Perform ONE batched READ ENTITIES for all requests
  IF lt_request_uuids IS NOT INITIAL.
    " Try draft first
    READ ENTITIES OF r_cmmdtyhdgcntrdealrequesttp
      ENTITY commoditycounterdealrequest BY \_cntrdealitem
        FIELDS ( counterdealitemuuid financialtransactionquantity counterdealrequestuuid )
        WITH VALUE #( FOR uuid IN lt_request_uuids ( %tky-counterdealrequestuuid = uuid %tky-%is_draft = if_abap_behv=>mk-on ) )
      RESULT DATA(lt_items_result).

    " Fallback to active if draft is empty
    IF lt_items_result IS INITIAL.
      READ ENTITIES OF r_cmmdtyhdgcntrdealrequesttp
        ENTITY commoditycounterdealrequest BY \_cntrdealitem
          FIELDS ( counterdealitemuuid financialtransactionquantity counterdealrequestuuid )
          WITH VALUE #( FOR uuid IN lt_request_uuids ( %tky-counterdealrequestuuid = uuid ) )
        RESULT lt_items_result.
    ENDIF.

    " Step 4: Group items by request UUID
    LOOP AT lt_request_uuids INTO DATA(lv_request_uuid).
      DATA(lt_items_for_request) = VALUE cl_cmm_counterdeal_helper=>ty_t_cntrdeal_item(
        FOR item IN lt_items_result WHERE ( counterdealrequestuuid = lv_request_uuid )
        ( counterdealitemuuid = item-counterdealitemuuid 
          financialtransactionquantity = item-financialtransactionquantity ) ).
      APPEND VALUE #( request_uuid = lv_request_uuid items = lt_items_for_request ) TO lt_items_map.
    ENDLOOP.
  ENDIF.

  " Step 5: Main processing loop - calculate for each request
  LOOP AT lt_original_data ASSIGNING FIELD-SYMBOL(<ls_original_data>).
    IF  ( <ls_original_data>-counterdealrequeststatus = if_cmm_cntrdeal_request=>cd_status-created
       OR <ls_original_data>-counterdealrequeststatus = if_cmm_cntrdeal_request=>cd_status-mark_for_release
       OR <ls_original_data>-counterdealrequeststatus = if_cmm_cntrdeal_request=>cd_status-tobereleased ).

      " Prepare input structure
      ls_calculate-counterdealrequestuuid         = <ls_original_data>-counterdealrequestuuid.
      ls_calculate-counterdealrequestdate         = <ls_original_data>-counterdealrequestdate.
      ls_calculate-commodityhedgeplanexposureid   = <ls_original_data>-commodityhedgeplanexposureid.
      ls_calculate-cmmdtyhedgeplnexposurequantity = <ls_original_data>-cmmdtyhedgeplnexposurequantity.
      ls_calculate-cmmdtyhdgplnexpsrquantityunit  = <ls_original_data>-cmmdtyhdgplnexpsrquantityunit.

      " Lookup items for this request
      READ TABLE lt_items_map INTO ls_items_map WITH KEY request_uuid = <ls_original_data>-counterdealrequestuuid.
      DATA(lt_cntrdeal_item) = COND #( WHEN sy-subrc = 0 THEN ls_items_map-items 
                                        ELSE VALUE cl_cmm_counterdeal_helper=>ty_t_cntrdeal_item( ) ).

      " Call helper with items parameter
      cl_cmm_counterdeal_helper=>calculate_overhedge(
        EXPORTING is_overhedge = ls_calculate it_cntrdeal_item = lt_cntrdeal_item
        IMPORTING es_overhedge = ls_overhedge ).

      " Map results back
      <ls_original_data>-cntrdealreqbfrutilizationtext  = ls_overhedge-cntrdealreqbfrutilizationtext.
      <ls_original_data>-cntrdealrequesttargetquotatext = ls_overhedge-cntrdealrequesttargetquotatext.
      <ls_original_data>-cntrdealbfrovrhedgecriticality = ls_overhedge-cntrdealbfrovrhedgecriticality.
      <ls_original_data>-cntrdealreqbeforeoverhedgetext = ls_overhedge-cntrdealreqbeforeoverhedgetext.
      <ls_original_data>-cntrdealaftovrhedgecriticality = ls_overhedge-cntrdealaftovrhedgecriticality.
      <ls_original_data>-cntrdealreqaftutilzncritlty    = ls_overhedge-cntrdealreqaftutilzncritlty.
      <ls_original_data>-cntrdealafterutilizationtext   = ls_overhedge-cntrdealafterutilizationtext.
      <ls_original_data>-cntrdealreqafteroverhedgetext  = ls_overhedge-cntrdealreqafteroverhedgetext.

    ELSE.
      " For released/completed requests, use historical data
      TRY.
          cl_cmm_hdg_req_hist_util=>get_entry( 
            EXPORTING iv_request_uuid = <ls_original_data>-counterdealrequestuuid 
            IMPORTING es_hr_utl_hist = DATA(ls_hr_utl_hist) ).
        CATCH cx_cmm_hedge_request.
      ENDTRY.

      <ls_original_data>-cntrdealreqbeforeoverhedgetext = ls_hr_utl_hist-hedge_request_over_quan.
      <ls_original_data>-cntrdealbfrovrhedgecriticality = ls_hr_utl_hist-hdg_request_over_quan_crtl.
      <ls_original_data>-cntrdealreqbfrutilizationtext  = ls_hr_utl_hist-hedge_request_hedged_quan.
      <ls_original_data>-cntrdealreqafteroverhedgetext  = ls_hr_utl_hist-hedge_request_over_quan_a.
      <ls_original_data>-cntrdealaftovrhedgecriticality = ls_hr_utl_hist-hdg_request_over_quan_crtl_a.
      <ls_original_data>-cntrdealafterutilizationtext   = ls_hr_utl_hist-hedge_request_hedged_quan_a.
      <ls_original_data>-cntrdealreqaftutilzncritlty    = ls_hr_utl_hist-hdg_req_hedged_quan_crtl_a.
      <ls_original_data>-cntrdealrequesttargetquotatext = ls_hr_utl_hist-hedge_request_mngmnt_quota.
    ENDIF.
  ENDLOOP.

  ct_calculated_data = CORRESPONDING #( lt_original_data ).

ENDMETHOD.
```

### **Step 3: Extend Helper Type to Include Request UUID**

In `CL_CMM_COUNTERDEAL_HELPER`, the type definition needed updating:

```abap
TYPES: BEGIN OF ty_cntrdeal_item,
         counterdealitemuuid         TYPE sysuuid_x16,
         financialtransactionquantity TYPE ftr_quan,
         counterdealrequestuuid      TYPE sysuuid_x16,  "← Added for calc class
       END OF ty_cntrdeal_item.
```

---

## ⚠️ Critical Discovery: System Propagation Delay

### **The Scare:**
After releasing the transport with the calc class fix, QM7 showed the **same ST22 error** immediately.

### **Initial Panic:**
- "Did the helper class not import?"
- "Do we need to create another transport?"
- "Did we break everything?"

### **The Reality:**
**It was just a propagation delay!** ⏱️

After ~5-10 minutes, the system:
- Completed program generation
- Refreshed ABAP runtime cache
- Updated dispatcher buffers
- Regenerated dependent objects

**Then everything worked perfectly!** ✅

---

## 🎯 Key Takeaways

### **1. SADL Exits Are Not Save Phase**
- SADL exit methods (`if_sadl_exit_calc_element_read~calculate`) run during **READ operations**
- They operate **outside the RAP save sequence**
- **READ ENTITIES is allowed** (unlike in late save)
- This means different rules than the handler/saver classes

### **2. Always Check Calculation/Exit Classes**
When refactoring a helper method signature, search for **ALL callers:**
```
☐ Handler classes (lhc_*)
☐ Saver classes (lsc_*)
☐ Calculation exit classes (CL_*_CALC, if_sadl_exit_*)
☐ Other helper classes
☐ Test classes
```

### **3. Batch EML Operations**
The calc class initially had "EML in loop" violations. Fixed by:
- Collecting all UUIDs first
- ONE batched READ ENTITIES for all requests
- Grouping results by UUID
- Using lookup in main loop

### **4. Transport Propagation Takes Time**
**Don't panic immediately after import!** 

Give the system 5-10 minutes to:
- Generate programs
- Clear caches
- Activate dependent objects

**Verification methods:**
- Syntax check classes in target system
- Check method signatures directly
- Run small test programs
- Wait a bit before declaring failure 😅

### **5. Type Definitions Need to Match Context**
Had to extend `ty_cntrdeal_item` to include `counterdealrequestuuid` because the calc class needs to group items by request (not needed in handler class context).

---

## 📋 Transport Checklist (Updated)

```
☐ Helper class (method signature change)
☐ Handler class (determination + buffer)
☐ Behavior definition (.bdef)
☐ Test classes (if any)
☐ Calculation/SADL exit classes ← DON'T FORGET THIS!
☐ Any other callers (grep search!)
```

---

## ⏱️ Timeline

**Yesterday (Dec 16):**
- Fixed ATC violations in helper class
- Updated handler class with determination
- Released transport ERXK657609
- All tests passed in ERX

**Today (Dec 17, Morning):**
- QM7 showed ST22 dump
- Identified missing calc class update
- Fixed `CL_CMM_COUNTERDEAL_CALC`
- Fixed "EML in loop" violations
- Extended helper type definition
- Released corrective transport

**Today (Dec 17, Afternoon):**
- Initial panic: same error appeared
- Verified transport import logs
- Waited ~5-10 minutes
- **Error disappeared - propagation complete!**
- ✅ **Application working in QM7**

---

## 🎓 For Next 4 Classes

**When fixing the other helper classes, remember:**

1. **Search for ALL callers** (not just handler/saver)
2. **Include calc/exit classes in transport** from the start
3. **After QM7 import, wait 5-10 minutes** before testing
4. **Batch EML operations** to avoid new ATC violations
5. **Extend types if needed** for different calling contexts

---

## ✅ Success Metrics

**Counterdeal Request - COMPLETE ✓**
- ✅ 5 ATC violations fixed (READ_IN_LATE_SAVE)
- ✅ Handler class refactored with determination
- ✅ Helper class signature updated
- ✅ Calc class updated and optimized
- ✅ No new ATC violations introduced
- ✅ Unit tests passing
- ✅ Transport released successfully
- ✅ **Application working in QM7**

**Remaining: 4 helper classes**
- CL_CMM_DESIGNATIONREQ_HELPER
- CL_CMM_MIGRATIONREQUEST_HELPER
- CL_CMM_RECLASSIFICATION_HELPER
- (1 more TBD)

---

**Documented by:** AI Assistant & Developer  
**Date:** December 17, 2025  
**Lesson:** Patience is a virtue in ABAP systems! 😅
