*"* Enhanced test methods for calculate_overhedge
*"* Add these methods to existing TCL_CMM_RCLASSFCTN_HELPER class

  " Critical Test 1: Modified Items During Transaction
  METHOD test_calculate_with_modified_items FOR TESTING RAISING cx_static_check.
    " This test will FAIL if using SELECT, PASS if using READ ENTITIES
    
    DATA:
      ls_calculate        TYPE cl_cmm_reclassification_helper=>ty_is_overhedge,
      ls_overhedge        TYPE cl_cmm_reclassification_helper=>ty_es_overhedge,
      lt_hedging_area     TYPE STANDARD TABLE OF c_commodityhedgehedgingareatp,
      lt_rclassfctn_cmpst TYPE STANDARD TABLE OF i_cmmdtyhdgrclassfctnreqcmpst,
      lt_rclassfctn_item  TYPE STANDARD TABLE OF i_cmmdtyhdgrclassfctnitem,
      lt_fin_trans        TYPE STANDARD TABLE OF i_cmmdtyhdgfintransaction.

    " Setup hedging area
    lt_hedging_area = VALUE #( ( hedgingareaidentification      = 1
                                 hedgingareavalidfromdate       = sy-datum
                                 hedgingareaversionstatus       = if_cmm_ha_hedging_area=>ha_status-released
                                 hedgingareadedesignationmethod = '01'
                                 hdggardedesignationmethodtext  = '01t' ) ).
    lo_environment->insert_test_data( lt_hedging_area ).

    " Setup request
    ls_calculate-reclassificationrequestuuid    = 1.
    ls_calculate-reclassificationrequestdate    = sy-datum.
    ls_calculate-cmmdtyhedgeplnexposurequantity = 1000.  " Exposure: 1000 TO
    ls_calculate-cmmdtyhdgplnexpsrquantityunit  = 'TO'.
    ls_calculate-cmmdtyhdgplnexpsrhedgingarea   = 1.

    lt_rclassfctn_cmpst = VALUE #( ( reclassificationrequestuuid = 1 ) ).
    lo_cds_environment->insert_test_data( lt_rclassfctn_cmpst ).

    " Insert items with ORIGINAL values: 300 + 400 = 700
    lt_rclassfctn_item = VALUE #( 
      ( reclassificationitemuuid     = 'ITEM1'
        reclassificationrequestuuid  = 1
        rclassfctnitemdealidentifier = 'DEAL1'
        rclassfctnitemcompanycode    = '1001' )
      ( reclassificationitemuuid     = 'ITEM2'
        reclassificationrequestuuid  = 1
        rclassfctnitemdealidentifier = 'DEAL2'
        rclassfctnitemcompanycode    = '1001' ) ).
    lo_cds_environment->insert_test_data( lt_rclassfctn_item ).

    lt_fin_trans = VALUE #( 
      ( fintransactiondealidentifier = 'DEAL1'
        fintransactioncompanycode    = '1001'
        financialtransactionquantity = 300 )  " Original: 300
      ( fintransactiondealidentifier = 'DEAL2'
        fintransactioncompanycode    = '1001'
        financialtransactionquantity = 400 ) ). " Original: 400
    lo_cds_environment->insert_test_data( lt_fin_trans ).

    " CRITICAL: Now MODIFY item 1 from 300 to 500 in the test data
    " This simulates modification during transaction (before commit)
    lo_cds_environment->clear_doubles( ).
    
    " Re-insert with MODIFIED value
    lt_fin_trans = VALUE #( 
      ( fintransactiondealidentifier = 'DEAL1'
        fintransactioncompanycode    = '1001'
        financialtransactionquantity = 500 )  " MODIFIED: 300 → 500
      ( fintransactiondealidentifier = 'DEAL2'
        fintransactioncompanycode    = '1001'
        financialtransactionquantity = 400 ) ).
    lo_cds_environment->insert_test_data( lt_hedging_area ).
    lo_cds_environment->insert_test_data( lt_rclassfctn_cmpst ).
    lo_cds_environment->insert_test_data( lt_rclassfctn_item ).
    lo_cds_environment->insert_test_data( lt_fin_trans ).

    " When: Calculate overhedge
    cl_cmm_reclassification_helper=>calculate_overhedge(
      EXPORTING
        is_overhedge = ls_calculate
      IMPORTING
        es_overhedge = ls_overhedge ).

    " Then: Should see MODIFIED value (500 + 400 = 900)
    " Expected overhedge: 1000 - 900 = 100
    " If using SELECT and it sees old committed data (300 + 400 = 700):
    " Wrong overhedge would be: 1000 - 700 = 300
    
    cl_abap_unit_assert=>assert_equals(
      act  = ls_overhedge-rclassfctnreqexceededqtytext
      exp  = '100 TO'
      msg  = 'CRITICAL: Must see modified quantity (500), not original (300)' ).

    cl_abap_unit_assert=>assert_equals(
      act  = ls_overhedge-rclassfctnreqexcdqtycritlty
      exp  = 0
      msg  = 'Criticality should be 0 (positive overhedge)' ).
  ENDMETHOD.

  " Critical Test 2: Deleted Items During Transaction
  METHOD test_calculate_with_deleted_items FOR TESTING RAISING cx_static_check.
    " This test will FAIL if using SELECT, PASS if using READ ENTITIES
    
    DATA:
      ls_calculate        TYPE cl_cmm_reclassification_helper=>ty_is_overhedge,
      ls_overhedge        TYPE cl_cmm_reclassification_helper=>ty_es_overhedge,
      lt_hedging_area     TYPE STANDARD TABLE OF c_commodityhedgehedgingareatp,
      lt_rclassfctn_cmpst TYPE STANDARD TABLE OF i_cmmdtyhdgrclassfctnreqcmpst,
      lt_rclassfctn_item  TYPE STANDARD TABLE OF i_cmmdtyhdgrclassfctnitem,
      lt_fin_trans        TYPE STANDARD TABLE OF i_cmmdtyhdgfintransaction.

    " Setup
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

    " Insert 3 items initially: 300 + 400 + 200 = 900
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
        financialtransactionquantity = 300 )
      ( fintransactiondealidentifier = 'DEAL2'
        fintransactioncompanycode    = '1001'
        financialtransactionquantity = 400 )
      ( fintransactiondealidentifier = 'DEAL3'
        fintransactioncompanycode    = '1001'
        financialtransactionquantity = 200 ) ).
    lo_cds_environment->insert_test_data( lt_fin_trans ).

    " CRITICAL: Now simulate deletion of ITEM3 during transaction
    " Clear and re-insert WITHOUT item 3 (simulates deletion)
    lo_cds_environment->clear_doubles( ).
    
    " Re-insert only items 1 and 2 (item 3 deleted)
    lt_rclassfctn_item = VALUE #( 
      ( reclassificationitemuuid     = 'ITEM1'
        reclassificationrequestuuid  = 1
        rclassfctnitemdealidentifier = 'DEAL1'
        rclassfctnitemcompanycode    = '1001' )
      ( reclassificationitemuuid     = 'ITEM2'
        reclassificationrequestuuid  = 1
        rclassfctnitemdealidentifier = 'DEAL2'
        rclassfctnitemcompanycode    = '1001' ) ).
        
    lt_fin_trans = VALUE #( 
      ( fintransactiondealidentifier = 'DEAL1'
        fintransactioncompanycode    = '1001'
        financialtransactionquantity = 300 )
      ( fintransactiondealidentifier = 'DEAL2'
        fintransactioncompanycode    = '1001'
        financialtransactionquantity = 400 ) ).
        
    lo_environment->insert_test_data( lt_hedging_area ).
    lo_cds_environment->insert_test_data( lt_rclassfctn_cmpst ).
    lo_cds_environment->insert_test_data( lt_rclassfctn_item ).
    lo_cds_environment->insert_test_data( lt_fin_trans ).

    " When: Calculate
    cl_cmm_reclassification_helper=>calculate_overhedge(
      EXPORTING
        is_overhedge = ls_calculate
      IMPORTING
        es_overhedge = ls_overhedge ).

    " Then: Should count only 2 items (300 + 400 = 700)
    " Expected overhedge: 1000 - 700 = 300
    " If SELECT still sees deleted item 3: would show 100 (wrong!)
    
    cl_abap_unit_assert=>assert_equals(
      act  = ls_overhedge-rclassfctnreqexceededqtytext
      exp  = '300 TO'
      msg  = 'CRITICAL: Deleted item must NOT be counted' ).

    cl_abap_unit_assert=>assert_equals(
      act  = ls_overhedge-rclassfctnreqexcdqtycritlty
      exp  = 0
      msg  = 'Criticality should be 0 (positive overhedge)' ).
  ENDMETHOD.
