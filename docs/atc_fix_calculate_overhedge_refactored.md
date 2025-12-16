*"* Common Overhedge Calculator - Eliminates copy-paste across 4 helper classes
CLASS zcl_cmm_overhedge_calculator DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    
    TYPES:
      BEGIN OF ty_is_overhedge,
        reclassificationrequestuuid    TYPE sysuuid_x16,
        reclassificationrequestdate    TYPE cmm_cd_date,
        cmmdtyhedgeplnexposurequantity TYPE ftr_quan,
        cmmdtyhdgplnexpsrquantityunit  TYPE tpm_cty_uom,
        cmmdtyhdgplnexpsrhedgingarea   TYPE cmm_hedging_area,
      END OF ty_is_overhedge,
      
      BEGIN OF ty_es_overhedge,
        hedgingareadedesignationmethod TYPE cmm_ha_dedes_method,
        hdggardedesignationmethodtext  TYPE val_text,
        rclassfctnreqexcdqtycritlty    TYPE int1,
        rclassfctnreqexceededqtytext   TYPE char_132,
      END OF ty_es_overhedge,
      
      BEGIN OF ty_s_item,
        reclassificationrequestuuid  TYPE sysuuid_x16,
        financialtransactionquantity TYPE ftr_quan,
      END OF ty_s_item,
      
      ty_t_items TYPE STANDARD TABLE OF ty_s_item WITH DEFAULT KEY.

    " Main calculation method (no READ ENTITIES!)
    CLASS-METHODS calculate_overhedge
      IMPORTING
        !is_overhedge       TYPE ty_is_overhedge
        !it_rclassfctn_items TYPE ty_t_items
      EXPORTING
        !es_overhedge TYPE ty_es_overhedge
      RAISING
        cx_sadl_exit.

ENDCLASS.

CLASS zcl_cmm_overhedge_calculator IMPLEMENTATION.

  METHOD calculate_overhedge.
    " Common calculation logic used by all helper classes
    " NO READ ENTITIES - data passed as parameter
    
    DATA:
      lv_items_quantity      TYPE ftr_quan,
      lv_exceeded_quantity   TYPE ftr_quan,
      lv_exceeded_quantity_t TYPE char_132.

    CLEAR: lv_items_quantity.

    " Get hedging area method
    cl_cmm_hedgereq_helper=>get_hedging_area_method(
      EXPORTING
        iv_hedging_area          = is_overhedge-cmmdtyhdgplnexpsrhedgingarea
        iv_valid_from            = is_overhedge-reclassificationrequestdate
      IMPORTING
        ev_hedgingareamethod     = es_overhedge-hedgingareadedesignationmethod
        ev_hedgingareamethodtext = es_overhedge-hdggardedesignationmethodtext ).

    " Get decimal places for formatting
    DATA(lv_andec) = cl_cmm_hedgereq_helper=>get_andec( 
      is_overhedge-cmmdtyhdgplnexpsrquantityunit ).

    " ✅ Calculate total from PASSED items (no READ needed)
    LOOP AT it_rclassfctn_items INTO DATA(ls_item).
      lv_items_quantity += ls_item-financialtransactionquantity.
    ENDLOOP.

    " Calculate exceeded quantity
    lv_exceeded_quantity = is_overhedge-cmmdtyhedgeplnexposurequantity - lv_items_quantity.
    
    " Format with proper decimal places
    WRITE lv_exceeded_quantity TO lv_exceeded_quantity_t 
      DECIMALS lv_andec 
      NO-SIGN 
      LEFT-JUSTIFIED.

    " Handle negative (overhedged scenario)
    IF lv_exceeded_quantity < 0.
      lv_exceeded_quantity_t = insert( val = lv_exceeded_quantity_t
                                       sub = '-' ).
      es_overhedge-rclassfctnreqexcdqtycritlty = 1.
    ENDIF.

    " Format final result text
    es_overhedge-rclassfctnreqexceededqtytext = 
      |{ lv_exceeded_quantity_t } { is_overhedge-cmmdtyhdgplnexpsrquantityunit }|.
      
  ENDMETHOD.

ENDCLASS.
