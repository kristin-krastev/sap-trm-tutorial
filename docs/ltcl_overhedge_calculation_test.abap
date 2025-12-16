*"* use this source file for your ABAP unit test classes
CLASS ltcl_overhedge_calculation DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    CLASS-DATA:
      environment     TYPE REF TO if_cds_test_environment,
      sql_environment TYPE REF TO if_osql_test_environment.

    CLASS-METHODS:
      class_setup,
      class_teardown.

    METHODS:
      setup,
      teardown,
      
      " Test Scenario 1: Simple Create
      test_simple_create FOR TESTING RAISING cx_static_check,
      
      " Test Scenario 2: Modified Items (CRITICAL)
      test_modified_items FOR TESTING RAISING cx_static_check,
      
      " Test Scenario 3: Deleted Items (CRITICAL)
      test_deleted_items FOR TESTING RAISING cx_static_check,
      
      " Test Scenario 4: Chained Determinations
      test_chained_determinations FOR TESTING RAISING cx_static_check,
      
      " Test Scenario 5: Concurrent Operations
      test_concurrent_requests FOR TESTING RAISING cx_static_check,
      
      " Test Scenario 6: Zero Items Edge Case
      test_zero_items FOR TESTING RAISING cx_static_check,
      
      " Test Scenario 7: Negative Overhedge
      test_negative_overhedge FOR TESTING RAISING cx_static_check.

ENDCLASS.

CLASS ltcl_overhedge_calculation IMPLEMENTATION.

  METHOD class_setup.
    " Create test environment for database tables
    sql_environment = cl_osql_test_environment=>create(
      i_dependency_list = VALUE #(
        ( 'Z_CMM_RECLASSFCTN_REQ' )     " Reclassification request table
        ( 'Z_CMM_RECLASSFCTN_ITEM' )    " Reclassification item table
        ( 'Z_CMM_HEDGE_PLAN_EXPOSURE' ) " Hedge plan exposure table
      )
    ).

    " Create CDS test environment
    environment = cl_cds_test_environment=>create(
      i_for_entity = 'R_CMMDTYHDGRCLASSFCTNREQUESTTP'
    ).
  ENDMETHOD.

  METHOD class_teardown.
    sql_environment->destroy( ).
    environment->destroy( ).
  ENDMETHOD.

  METHOD setup.
    " Clear test data before each test
    sql_environment->clear_doubles( ).
    environment->clear_doubles( ).
  ENDMETHOD.

  METHOD teardown.
    " Rollback any changes after each test
    ROLLBACK ENTITIES.
  ENDMETHOD.

  METHOD test_simple_create.
    " Test Case 1: Simple Create - Baseline
    " Purpose: Verify basic calculation works
    " Both READ ENTITIES and SELECT should work here
    
    " Given: Create reclassification request
    MODIFY ENTITIES OF r_cmmdtyhdgrclassfctnrequesttp
      ENTITY cmmdtyreclassificationrequest
      CREATE FIELDS ( cmmdtyhdgplnexpsrhedgingarea
                      reclassificationrequestdate
                      cmmdtyhedgeplnexposurequantity
                      cmmdtyhdgplnexpsrquantityunit )
      WITH VALUE #( ( %cid = 'REQ1'
                     cmmdtyhdgplnexpsrhedgingarea = '001'
                     reclassificationrequestdate = sy-datum
                     cmmdtyhedgeplnexposurequantity = 1000
                     cmmdtyhdgplnexpsrquantityunit = 'TO' ) )
      MAPPED DATA(mapped_req)
      FAILED DATA(failed_req)
      REPORTED DATA(reported_req).

    " Assert request created successfully
    cl_abap_unit_assert=>assert_initial(
      act = failed_req
      msg = 'Request creation should succeed' ).

    " Add items to request (total 700)
    MODIFY ENTITIES OF r_cmmdtyhdgrclassfctnrequesttp
      ENTITY cmmdtyreclassificationrequest
      CREATE BY \_rclassfctnitem
      FIELDS ( financialtransactionquantity )
      WITH VALUE #( ( %cid_ref = 'REQ1'
                     %target = VALUE #(
                       ( %cid = 'ITEM1'
                         financialtransactionquantity = 300 )
                       ( %cid = 'ITEM2'
                         financialtransactionquantity = 400 ) ) ) )
      MAPPED DATA(mapped_items)
      FAILED DATA(failed_items)
      REPORTED DATA(reported_items).

    " Commit to trigger save sequence
    COMMIT ENTITIES.

    " Then: Read result and verify overhedge calculation
    READ ENTITIES OF r_cmmdtyhdgrclassfctnrequesttp
      ENTITY cmmdtyreclassificationrequest
      FIELDS ( rclassfctnreqexceededqtytext
              rclassfctnreqexcdqtycritlty )
      WITH VALUE #( ( %cid_ref = 'REQ1' ) )
      RESULT DATA(lt_result).

    " Expected overhedge: 1000 - (300 + 400) = 300
    cl_abap_unit_assert=>assert_not_initial(
      act = lt_result
      msg = 'Result should exist after save' ).

    cl_abap_unit_assert=>assert_equals(
      act = lt_result[ 1 ]-rclassfctnreqexceededqtytext
      exp = |300.000 TO|
      msg = 'Overhedge should be 300 TO' ).

    cl_abap_unit_assert=>assert_equals(
      act = lt_result[ 1 ]-rclassfctnreqexcdqtycritlty
      exp = 0
      msg = 'Criticality should be 0 (no overhedge)' ).
  ENDMETHOD.

  METHOD test_modified_items.
    " Test Case 2: Modified Items - CRITICAL TEST
    " Purpose: Verify calculation sees MODIFIED values, not original
    " READ ENTITIES should see changes, SELECT might not!
    
    " Given: Create request with items
    MODIFY ENTITIES OF r_cmmdtyhdgrclassfctnrequesttp
      ENTITY cmmdtyreclassificationrequest
      CREATE FIELDS ( cmmdtyhdgplnexpsrhedgingarea
                      reclassificationrequestdate
                      cmmdtyhedgeplnexposurequantity
                      cmmdtyhdgplnexpsrquantityunit )
      WITH VALUE #( ( %cid = 'REQ1'
                     cmmdtyhdgplnexpsrhedgingarea = '001'
                     reclassificationrequestdate = sy-datum
                     cmmdtyhedgeplnexposurequantity = 1000
                     cmmdtyhdgplnexpsrquantityunit = 'TO' ) )
      CREATE BY \_rclassfctnitem
      FIELDS ( financialtransactionquantity )
      WITH VALUE #( ( %cid_ref = 'REQ1'
                     %target = VALUE #(
                       ( %cid = 'ITEM1'
                         financialtransactionquantity = 300 )
                       ( %cid = 'ITEM2'
                         financialtransactionquantity = 400 ) ) ) )
      MAPPED DATA(mapped)
      FAILED DATA(failed)
      REPORTED DATA(reported).

    " CRITICAL: Modify item 1 from 300 to 500 BEFORE save completes
    MODIFY ENTITIES OF r_cmmdtyhdgrclassfctnrequesttp
      ENTITY rclassfctnitem
      UPDATE FIELDS ( financialtransactionquantity )
      WITH VALUE #( ( %tky = mapped-rclassfctnitem[ 1 ]-%tky
                     financialtransactionquantity = 500 ) ).

    " When: Commit triggers save and calculation
    COMMIT ENTITIES.

    " Then: Overhedge should be 100 (1000 - 500 - 400)
    " NOT 300 (1000 - 300 - 400) ← SELECT might return this!
    READ ENTITIES OF r_cmmdtyhdgrclassfctnrequesttp
      ENTITY cmmdtyreclassificationrequest
      FIELDS ( rclassfctnreqexceededqtytext )
      WITH VALUE #( ( %cid_ref = 'REQ1' ) )
      RESULT DATA(lt_result).

    cl_abap_unit_assert=>assert_equals(
      act = lt_result[ 1 ]-rclassfctnreqexceededqtytext
      exp = |100.000 TO|
      msg = 'CRITICAL: Must see modified quantity (500), not original (300)' ).
  ENDMETHOD.

  METHOD test_deleted_items.
    " Test Case 3: Deleted Items - CRITICAL TEST
    " Purpose: Verify calculation does NOT count deleted items
    " READ ENTITIES should respect deletion, SELECT might still see them!
    
    " Given: Create request with 3 items
    MODIFY ENTITIES OF r_cmmdtyhdgrclassfctnrequesttp
      ENTITY cmmdtyreclassificationrequest
      CREATE FIELDS ( cmmdtyhdgplnexpsrhedgingarea
                      reclassificationrequestdate
                      cmmdtyhedgeplnexposurequantity
                      cmmdtyhdgplnexpsrquantityunit )
      WITH VALUE #( ( %cid = 'REQ1'
                     cmmdtyhdgplnexpsrhedgingarea = '001'
                     reclassificationrequestdate = sy-datum
                     cmmdtyhedgeplnexposurequantity = 1000
                     cmmdtyhdgplnexpsrquantityunit = 'TO' ) )
      CREATE BY \_rclassfctnitem
      FIELDS ( financialtransactionquantity )
      WITH VALUE #( ( %cid_ref = 'REQ1'
                     %target = VALUE #(
                       ( %cid = 'ITEM1'
                         financialtransactionquantity = 300 )
                       ( %cid = 'ITEM2'
                         financialtransactionquantity = 400 )
                       ( %cid = 'ITEM3'
                         financialtransactionquantity = 200 ) ) ) )
      MAPPED DATA(mapped)
      FAILED DATA(failed)
      REPORTED DATA(reported).

    " CRITICAL: Delete item 3 BEFORE save completes
    DELETE ENTITIES OF r_cmmdtyhdgrclassfctnrequesttp
      ENTITY rclassfctnitem
      WITH VALUE #( ( %tky = mapped-rclassfctnitem[ 3 ]-%tky ) ).

    " When: Commit triggers save and calculation
    COMMIT ENTITIES.

    " Then: Overhedge should be 300 (1000 - 300 - 400)
    " Should NOT count deleted item (200)
    " SELECT might still see it in database!
    READ ENTITIES OF r_cmmdtyhdgrclassfctnrequesttp
      ENTITY cmmdtyreclassificationrequest
      FIELDS ( rclassfctnreqexceededqtytext )
      WITH VALUE #( ( %cid_ref = 'REQ1' ) )
      RESULT DATA(lt_result).

    cl_abap_unit_assert=>assert_equals(
      act = lt_result[ 1 ]-rclassfctnreqexceededqtytext
      exp = |300.000 TO|
      msg = 'CRITICAL: Deleted item must NOT be counted' ).
  ENDMETHOD.

  METHOD test_chained_determinations.
    " Test Case 4: Chained Determinations - CRITICAL TEST
    " Purpose: Verify calculation sees values AFTER other determinations
    " Another determination might adjust quantities (e.g., unit conversion)
    
    " Given: Create request with items
    MODIFY ENTITIES OF r_cmmdtyhdgrclassfctnrequesttp
      ENTITY cmmdtyreclassificationrequest
      CREATE FIELDS ( cmmdtyhdgplnexpsrhedgingarea
                      reclassificationrequestdate
                      cmmdtyhedgeplnexposurequantity
                      cmmdtyhdgplnexpsrquantityunit )
      WITH VALUE #( ( %cid = 'REQ1'
                     cmmdtyhdgplnexpsrhedgingarea = '001'
                     reclassificationrequestdate = sy-datum
                     cmmdtyhedgeplnexposurequantity = 1000
                     cmmdtyhdgplnexpsrquantityunit = 'TO' ) )
      CREATE BY \_rclassfctnitem
      FIELDS ( financialtransactionquantity
              quantityunit )
      WITH VALUE #( ( %cid_ref = 'REQ1'
                     %target = VALUE #(
                       " Items created with different unit (e.g., KG)
                       ( %cid = 'ITEM1'
                         financialtransactionquantity = 300000
                         quantityunit = 'KG' )
                       ( %cid = 'ITEM2'
                         financialtransactionquantity = 400000
                         quantityunit = 'KG' ) ) ) )
      MAPPED DATA(mapped)
      FAILED DATA(failed)
      REPORTED DATA(reported).

    " CRITICAL: Another determination converts KG to TO
    " (Assume 1 TO = 1000 KG for this test)
    " So 300000 KG = 300 TO, 400000 KG = 400 TO
    " This conversion happens in determination phase
    
    " When: Commit triggers all determinations + calculation
    COMMIT ENTITIES.

    " Then: Calculation must see CONVERTED values (300 TO, 400 TO)
    " NOT original values (300000 KG, 400000 KG)
    READ ENTITIES OF r_cmmdtyhdgrclassfctnrequesttp
      ENTITY cmmdtyreclassificationrequest
      FIELDS ( rclassfctnreqexceededqtytext )
      WITH VALUE #( ( %cid_ref = 'REQ1' ) )
      RESULT DATA(lt_result).

    " If calculation sees converted values correctly:
    " Overhedge = 1000 - (300 + 400) = 300 TO
    " If it sees original values (wrong): would be incorrect
    cl_abap_unit_assert=>assert_contains(
      act = lt_result[ 1 ]-rclassfctnreqexceededqtytext
      exp = |300|
      msg = 'CRITICAL: Must see converted quantities, not original' ).
  ENDMETHOD.

  METHOD test_concurrent_requests.
    " Test Case 5: Concurrent Operations
    " Purpose: Verify proper transaction isolation
    " Each request must only see its own items
    
    " Given: Create two separate requests
    MODIFY ENTITIES OF r_cmmdtyhdgrclassfctnrequesttp
      ENTITY cmmdtyreclassificationrequest
      CREATE FIELDS ( cmmdtyhdgplnexpsrhedgingarea
                      reclassificationrequestdate
                      cmmdtyhedgeplnexposurequantity
                      cmmdtyhdgplnexpsrquantityunit )
      WITH VALUE #( 
        ( %cid = 'REQ_A'
          cmmdtyhdgplnexpsrhedgingarea = '001'
          reclassificationrequestdate = sy-datum
          cmmdtyhedgeplnexposurequantity = 1000
          cmmdtyhdgplnexpsrquantityunit = 'TO' )
        ( %cid = 'REQ_B'
          cmmdtyhdgplnexpsrhedgingarea = '002'
          reclassificationrequestdate = sy-datum
          cmmdtyhedgeplnexposurequantity = 2000
          cmmdtyhdgplnexpsrquantityunit = 'TO' ) )
      MAPPED DATA(mapped_reqs)
      FAILED DATA(failed_reqs)
      REPORTED DATA(reported_reqs).

    " Add items to Request A (total 700)
    MODIFY ENTITIES OF r_cmmdtyhdgrclassfctnrequesttp
      ENTITY cmmdtyreclassificationrequest
      CREATE BY \_rclassfctnitem
      FIELDS ( financialtransactionquantity )
      WITH VALUE #( ( %cid_ref = 'REQ_A'
                     %target = VALUE #(
                       ( %cid = 'ITEM_A1'
                         financialtransactionquantity = 300 )
                       ( %cid = 'ITEM_A2'
                         financialtransactionquantity = 400 ) ) ) ).

    " Add items to Request B (total 1700)
    MODIFY ENTITIES OF r_cmmdtyhdgrclassfctnrequesttp
      ENTITY cmmdtyreclassificationrequest
      CREATE BY \_rclassfctnitem
      FIELDS ( financialtransactionquantity )
      WITH VALUE #( ( %cid_ref = 'REQ_B'
                     %target = VALUE #(
                       ( %cid = 'ITEM_B1'
                         financialtransactionquantity = 800 )
                       ( %cid = 'ITEM_B2'
                         financialtransactionquantity = 900 ) ) ) ).

    " When: Commit both (simulates concurrent saves)
    COMMIT ENTITIES.

    " Then: Each request sees only its own items
    READ ENTITIES OF r_cmmdtyhdgrclassfctnrequesttp
      ENTITY cmmdtyreclassificationrequest
      FIELDS ( rclassfctnreqexceededqtytext )
      WITH VALUE #( ( %cid_ref = 'REQ_A' )
                   ( %cid_ref = 'REQ_B' ) )
      RESULT DATA(lt_results).

    " Request A: 1000 - 700 = 300
    cl_abap_unit_assert=>assert_contains(
      act = lt_results[ 1 ]-rclassfctnreqexceededqtytext
      exp = |300|
      msg = 'Request A should calculate based on its items only' ).

    " Request B: 2000 - 1700 = 300
    cl_abap_unit_assert=>assert_contains(
      act = lt_results[ 2 ]-rclassfctnreqexceededqtytext
      exp = |300|
      msg = 'Request B should calculate based on its items only' ).
  ENDMETHOD.

  METHOD test_zero_items.
    " Test Case 6: Zero Items Edge Case
    " Purpose: Verify calculation handles requests with no items
    
    " Given: Create request with NO items
    MODIFY ENTITIES OF r_cmmdtyhdgrclassfctnrequesttp
      ENTITY cmmdtyreclassificationrequest
      CREATE FIELDS ( cmmdtyhdgplnexpsrhedgingarea
                      reclassificationrequestdate
                      cmmdtyhedgeplnexposurequantity
                      cmmdtyhdgplnexpsrquantityunit )
      WITH VALUE #( ( %cid = 'REQ1'
                     cmmdtyhdgplnexpsrhedgingarea = '001'
                     reclassificationrequestdate = sy-datum
                     cmmdtyhedgeplnexposurequantity = 1000
                     cmmdtyhdgplnexpsrquantityunit = 'TO' ) )
      MAPPED DATA(mapped)
      FAILED DATA(failed)
      REPORTED DATA(reported).

    " When: Commit without adding items
    COMMIT ENTITIES.

    " Then: Overhedge should be full exposure (1000)
    READ ENTITIES OF r_cmmdtyhdgrclassfctnrequesttp
      ENTITY cmmdtyreclassificationrequest
      FIELDS ( rclassfctnreqexceededqtytext )
      WITH VALUE #( ( %cid_ref = 'REQ1' ) )
      RESULT DATA(lt_result).

    cl_abap_unit_assert=>assert_equals(
      act = lt_result[ 1 ]-rclassfctnreqexceededqtytext
      exp = |1000.000 TO|
      msg = 'With no items, overhedge should equal exposure' ).
  ENDMETHOD.

  METHOD test_negative_overhedge.
    " Test Case 7: Negative Overhedge (Exceeded)
    " Purpose: Verify criticality set correctly when overhedged
    
    " Given: Create request with items exceeding exposure
    MODIFY ENTITIES OF r_cmmdtyhdgrclassfctnrequesttp
      ENTITY cmmdtyreclassificationrequest
      CREATE FIELDS ( cmmdtyhdgplnexpsrhedgingarea
                      reclassificationrequestdate
                      cmmdtyhedgeplnexposurequantity
                      cmmdtyhdgplnexpsrquantityunit )
      WITH VALUE #( ( %cid = 'REQ1'
                     cmmdtyhdgplnexpsrhedgingarea = '001'
                     reclassificationrequestdate = sy-datum
                     cmmdtyhedgeplnexposurequantity = 1000
                     cmmdtyhdgplnexpsrquantityunit = 'TO' ) )
      CREATE BY \_rclassfctnitem
      FIELDS ( financialtransactionquantity )
      WITH VALUE #( ( %cid_ref = 'REQ1'
                     %target = VALUE #(
                       ( %cid = 'ITEM1'
                         financialtransactionquantity = 600 )
                       ( %cid = 'ITEM2'
                         financialtransactionquantity = 700 ) ) ) )
      MAPPED DATA(mapped)
      FAILED DATA(failed)
      REPORTED DATA(reported).

    " When: Commit (items total 1300, exceeds 1000)
    COMMIT ENTITIES.

    " Then: Overhedge should be -300 (negative)
    " Criticality should be 1 (exceeded)
    READ ENTITIES OF r_cmmdtyhdgrclassfctnrequesttp
      ENTITY cmmdtyreclassificationrequest
      FIELDS ( rclassfctnreqexceededqtytext
              rclassfctnreqexcdqtycritlty )
      WITH VALUE #( ( %cid_ref = 'REQ1' ) )
      RESULT DATA(lt_result).

    " Should show negative with minus sign
    cl_abap_unit_assert=>assert_contains(
      act = lt_result[ 1 ]-rclassfctnreqexceededqtytext
      exp = |-|
      msg = 'Should contain minus sign for exceeded quantity' ).

    cl_abap_unit_assert=>assert_contains(
      act = lt_result[ 1 ]-rclassfctnreqexceededqtytext
      exp = |300|
      msg = 'Absolute value should be 300' ).

    cl_abap_unit_assert=>assert_equals(
      act = lt_result[ 1 ]-rclassfctnreqexcdqtycritlty
      exp = 1
      msg = 'Criticality should be 1 when exceeded' ).
  ENDMETHOD.

ENDCLASS.
