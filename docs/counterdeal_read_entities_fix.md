# Fix for RAP Contract Violation in CL_CMM_COUNTERDEAL_HELPER

## Problem
The method `CALCULATE_OVERHEDGE` in class `CL_CMM_COUNTERDEAL_HELPER` is using `READ ENTITIES` during the late save phase, which causes RAP Contract Check violations:

- **Error**: RAP Contract Check: Provider Violation (high)
- **Location**: Line 68
- **Message**: CC/P:READ_IN_LATE_SAVE:R_CMMDTYHDGCNTRDEALREQUESTTP:R_CMMDTYHDGCNTRDEALREQUESTTP

## Why This Happens
In RAP (RESTful ABAP Programming model):
- During the **late save phase**, the transactional buffer is being finalized
- `READ ENTITIES` accesses the transactional buffer which is not accessible in this phase
- You must use direct database access (SELECT) instead

## Original Code (Lines ~70-88)
```abap
"After Selection For Counter deal
READ ENTITIES OF r_cmmdtyhdgcntrdealrequesttp
  ENTITY commoditycounterdealrequest
    BY \_cntrdealitem
      FIELDS ( counterdealitemuuid financialtransactionquantity )
        WITH VALUE #( ( %tky-counterdealrequestuuid = is_overhedge-counterdealrequestuuid
                        %tky-%is_draft              = if_abap_behv=>mk-on ) )
  RESULT DATA(lt_cntrdeal_item).

IF lt_cntrdeal_item IS INITIAL.
  READ ENTITIES OF r_cmmdtyhdgcntrdealrequesttp
    ENTITY commoditycounterdealrequest
      BY \_cntrdealitem
        FIELDS ( counterdealitemuuid financialtransactionquantity )
          WITH VALUE #( ( %tky-counterdealrequestuuid = is_overhedge-counterdealrequestuuid ) )
    RESULT lt_cntrdeal_item.
ENDIF.
```

## Solution: Replace with SELECT

### Step 1: Identify the Database Table/View
The counter deal items are typically stored in a table like `FIN_CMM_CDREQ_ITEM` or similar. You need to identify the correct table that backs the `\_cntrdealitem` association.

### Step 2: Replace with SELECT Statement

```abap
"After Selection For Counter deal
DATA: lt_cntrdeal_item TYPE STANDARD TABLE OF r_cmmdtyhdgcntrdealrequesttp,
      lv_requestquantity TYPE ftr_quan.

" Select from the database table instead of READ ENTITIES
" Replace 'FIN_CMM_CDREQ_ITEM' with the actual table name
SELECT counterdealitemuuid,
       financialtransactionquantity
  FROM i_cmmdtyhdgcntrdealrequestsub  "or the appropriate CDS/table
  INTO CORRESPONDING FIELDS OF TABLE @lt_cntrdeal_item
  WHERE counterdealrequestuuid = @is_overhedge-counterdealrequestuuid.

" If nothing found in active data and draft handling is needed
IF lt_cntrdeal_item IS INITIAL.
  " Try reading from draft table if applicable
  SELECT counterdealitemuuid,
         financialtransactionquantity
    FROM i_cmmdtyhdgcntrdealrequestsub "with draft extension
    INTO CORRESPONDING FIELDS OF TABLE @lt_cntrdeal_item
    WHERE counterdealrequestuuid = @is_overhedge-counterdealrequestuuid.
ENDIF.
```

## Recommended Implementation

Here's the corrected version of the method section:

```abap
METHOD calculate_overhedge.

  DATA:
    lv_utilization_bef       TYPE ftr_quan,
    lv_util_percent_bef      TYPE cmm_hedge_percentage,
    lv_target_quota          TYPE ftr_quan,
    lv_tq_percent            TYPE cmm_hedge_percentage,
    lv_overhedge_bef         TYPE ftr_quan,
    lv_overhedge_percent_bef TYPE cmm_hedge_percentage,
    lv_requestquantity       TYPE ftr_quan,
    ls_calculated_fields     TYPE i_cmmdtyhdgutilizationdetails.

  " Structure type for counter deal items
  TYPES: BEGIN OF ty_cntrdeal_item,
           counterdealitemuuid           TYPE sysuuid_x16,
           financialtransactionquantity  TYPE ftr_quan,
         END OF ty_cntrdeal_item.

  DATA: lt_cntrdeal_item TYPE STANDARD TABLE OF ty_cntrdeal_item.

  CLEAR:
    ls_calculated_fields,
    lv_utilization_bef,
    lv_util_percent_bef,
    lv_target_quota,
    lv_tq_percent,
    lv_overhedge_bef,
    lv_overhedge_percent_bef,
    lv_requestquantity.

  "As is
  ls_calculated_fields = cl_cmm_hedgereq_helper=>get_utilization_details(
    iv_plnexposureid    = is_overhedge-commodityhedgeplanexposureid
    iv_evaldate         = is_overhedge-counterdealrequestdate
    iv_run_without_hrel = abap_true ).

  IF ls_calculated_fields IS NOT INITIAL.

    "As Is
    lv_utilization_bef       = ls_calculated_fields-cmmdtyhdgutilznhedgedabsltval.
    lv_util_percent_bef      = ls_calculated_fields-cmmdtyhedgeutilznhedgedpercent.
    lv_target_quota          = ls_calculated_fields-cmmdtyhdgutilzntgtqtamgmtvalue.
    lv_tq_percent            = ls_calculated_fields-cmmdtyhedgeutilzntgtqtamgmtpct.
    lv_overhedge_bef         = ls_calculated_fields-cmmdtyhdgutilznoverhedgedvalue.
    lv_overhedge_percent_bef = ls_calculated_fields-cmmdtyhdgutilznoverhedgedpct.
  ENDIF.

  "Get no of decimal value
  DATA(lv_andec) = cl_cmm_hedgereq_helper=>get_andec( 
    iv_quantityunit = is_overhedge-cmmdtyhdgplnexpsrquantityunit ).

  "As Is Calculation
  cl_cmm_hedgereq_helper=>calculate_before_parameters(
    EXPORTING
      iv_utilization_bef            = lv_utilization_bef
      iv_util_percent_bef           = lv_util_percent_bef
      iv_target_quota               = lv_target_quota
      iv_tq_percent                 = lv_tq_percent
      iv_overhedge_bef              = lv_overhedge_bef
      iv_overhedge_percent_bef      = lv_overhedge_percent_bef
      iv_quantity_plan_exposure     = is_overhedge-cmmdtyhedgeplnexposurequantity
      iv_quantityunit_plan_exposure = is_overhedge-cmmdtyhdgplnexpsrquantityunit
      iv_andec                      = lv_andec
    IMPORTING
      es_before_selection           = DATA(ls_before_selection) ).

  "Populate Data As Is
  es_overhedge-cntrdealreqbfrutilizationtext  = ls_before_selection-bfr_utilizationt.
  es_overhedge-cntrdealrequesttargetquotatext = ls_before_selection-target_quota.
  es_overhedge-cntrdealbfrovrhedgecriticality = ls_before_selection-bfr_ovrhedge_criticality.
  es_overhedge-cntrdealreqbeforeoverhedgetext = ls_before_selection-before_overhedge.

  "**** REPLACED READ ENTITIES WITH SELECT ****
  "After Selection For Counter deal - Using SELECT instead of READ ENTITIES
  " Note: Replace 'I_CMMDTYHDGCNTRDEALREQUESTSUB' with the actual CDS view or table name
  SELECT counterdealitemuuid,
         financialtransactionquantity
    FROM i_cmmdtyhdgcntrdealrequestsub
    INTO CORRESPONDING FIELDS OF TABLE @lt_cntrdeal_item
    WHERE counterdealrequestuuid = @is_overhedge-counterdealrequestuuid.

  " Sum up the quantities
  LOOP AT lt_cntrdeal_item INTO DATA(ls_cntrdeal_item).
    lv_requestquantity += ls_cntrdeal_item-financialtransactionquantity.
  ENDLOOP.

  cl_cmm_hedgereq_helper=>calculate_after_parameters(
    EXPORTING
      iv_target_quota           = lv_target_quota
      iv_requestquantity        = lv_requestquantity
      iv_utilization_bef        = lv_utilization_bef
      iv_quantity_plan_exposure = is_overhedge-cmmdtyhedgeplnexposurequantity
      iv_overhedge_bef          = lv_overhedge_bef
      iv_andec                  = lv_andec
    IMPORTING
      es_after_selection        = DATA(ls_after_selection) ).

  es_overhedge-cntrdealaftovrhedgecriticality = ls_after_selection-overhedge_aft_criticality.
  es_overhedge-cntrdealreqaftutilzncritlty    = ls_after_selection-utilization_aft_criticality.
  es_overhedge-cntrdealafterutilizationtext   = |{ ls_after_selection-utilization_aft_t } { is_overhedge-cmmdtyhdgplnexpsrquantityunit } { '(' }{ ls_after_selection-util_percent_aft_t }{ '%)' }|.
  es_overhedge-cntrdealreqafteroverhedgetext  = |{ ls_after_selection-overhedge_aft_t } { is_overhedge-cmmdtyhdgplnexpsrquantityunit } { '(' }{ ls_after_selection-overhedge_percent_aft_t }{ '%)' }|.

ENDMETHOD.
```

## Key Changes Made

1. **Removed `READ ENTITIES`**: Replaced both READ ENTITIES statements with a single SELECT
2. **Direct Database Access**: Using SELECT to query directly from the database table/CDS view
3. **Simplified Logic**: No need for draft handling in late save (data is already being saved)
4. **Type Definition**: Added explicit type definition for better type safety

## Finding the Correct Table/View Name

To find the correct table or CDS view backing the counter deal items:

### Option 1: Check the Behavior Definition
```abap
" Look at the behavior definition R_CMMDTYHDGCNTRDEALREQUESTTP
" Find the association _cntrdealitem definition
" It will show the target entity
```

### Option 2: Check Data Dictionary
1. Go to SE11
2. Look up `R_CMMDTYHDGCNTRDEALREQUESTTP`
3. Navigate to the association `_cntrdealitem`
4. Find the underlying table/view

### Option 3: Common SAP CMM Tables
- `FIN_CMM_CDREQ` - Counter deal request header
- `FIN_CMM_CDREQ_ITEM` - Counter deal request items
- Or the corresponding CDS views:
  - `I_CMMDTYHDGCNTRDEALREQUESTSUB` - Counter deal items interface view
  - `C_CMMDTYHDGCNTRDEALREQUESTSUB` - Counter deal items consumption view

## Testing Checklist

After implementing the fix:

1. ✅ Run ATC check again - should show 0 violations
2. ✅ Test the overhedge calculation functionality
3. ✅ Verify quantities are correctly summed
4. ✅ Check both draft and active scenarios
5. ✅ Ensure no regression in business logic

## Additional Considerations

### Draft Handling
- In late save, you're working with active data being persisted
- Draft data is no longer relevant in this phase
- If you need draft data, it should be handled earlier in the save sequence

### Performance
- SELECT is generally faster than READ ENTITIES in save phase
- Add appropriate indexes on `counterdealrequestuuid` if needed
- Consider using `UP TO n ROWS` if limiting results

### Authorization Checks
- READ ENTITIES includes automatic authorization checks
- With SELECT, you may need to add explicit authority checks:

```abap
AUTHORITY-CHECK OBJECT 'FIN_CMM_REQ'
  ID 'ACTVT' FIELD '03'.  " Display
IF sy-subrc <> 0.
  " Handle authorization error
ENDIF.
```

## References

- SAP Help: RAP Save Sequence
- SAP Note: Check for notes related to FIN-FSCM-CMM late save issues
- RAP Contract Check documentation: BC-ESI-RAP-SRV

