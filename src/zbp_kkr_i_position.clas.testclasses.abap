*"* use this source file for your ABAP unit test classes
CLASS ltcl_position DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    CLASS-DATA:
      environment TYPE REF TO if_abap_behavior_test_environment,
      sql_environment TYPE REF TO if_osql_test_environment.

    CLASS-METHODS:
      class_setup,    " setup test double framework
      class_teardown. " clean up test double framework

    METHODS:
      setup,          " reset test doubles
      teardown.       " clean up test data

    METHODS:
      " Test methods for Position validations
      test_validate_dates_success FOR TESTING RAISING cx_static_check,
      test_validate_dates_missing_from FOR TESTING RAISING cx_static_check,
      test_validate_dates_invalid_range FOR TESTING RAISING cx_static_check,

      " Test methods for Instrument validation
      test_validate_instrument_success FOR TESTING RAISING cx_static_check,
      test_validate_instrument_not_found FOR TESTING RAISING cx_static_check,

      " Test methods for Cashflow validations
      test_validate_cf_amount_success FOR TESTING RAISING cx_static_check,
      test_validate_cf_amount_negative FOR TESTING RAISING cx_static_check,
      test_validate_cf_date_success FOR TESTING RAISING cx_static_check,
      test_validate_cf_date_missing FOR TESTING RAISING cx_static_check,
      test_validate_cf_currency FOR TESTING RAISING cx_static_check,

      " Test methods for ID determinations
      test_calculate_position_id FOR TESTING RAISING cx_static_check,
      test_calculate_cashflow_id FOR TESTING RAISING cx_static_check,

      " Test methods for draft handling
      test_create_draft FOR TESTING RAISING cx_static_check,
      test_activate_draft FOR TESTING RAISING cx_static_check,
      test_discard_draft FOR TESTING RAISING cx_static_check,

      " Test methods for associations
      test_create_position_with_cashflow FOR TESTING RAISING cx_static_check,
      test_add_cashflow_to_position FOR TESTING RAISING cx_static_check,

      " Test methods for authorization
      test_position_authorization FOR TESTING RAISING cx_static_check.

ENDCLASS.

CLASS ltcl_position IMPLEMENTATION.

  METHOD class_setup.
    " Create test doubles for the database tables
    sql_environment = cl_osql_test_environment=>create(
        i_dependency_list = VALUE #( ( 'ZTRMPOS' )
                                   ( 'ZTRMCF' )
                                   ( 'ZTRMINST' ) ) ).

    " Create test doubles for RAP business objects
    environment = cl_abap_testdouble=>create_test_environment(
        i_for_behavior_of = 'ZKKR_I_POSITION' ).
  ENDMETHOD.

  METHOD class_teardown.
    " Clean up test doubles
    sql_environment->destroy( ).
    environment->destroy( ).
  ENDMETHOD.

  METHOD setup.
    " Clear test doubles
    sql_environment->clear_doubles( ).
    environment->clear_doubles( ).
  ENDMETHOD.

  METHOD teardown.
  ENDMETHOD.

  METHOD test_validate_dates_success.
    " Create test data
    DATA(valid_from) = cl_abap_context_info=>get_system_date( ).
    DATA(valid_to) = valid_from + 365.

    " Prepare test position
    MODIFY ENTITIES OF zkkr_i_position
      ENTITY Position
        CREATE FIELDS ( ValidFrom ValidTo )
        WITH VALUE #( ( %cid = 'TEST_POS_01'
                       ValidFrom = valid_from
                       ValidTo = valid_to ) ).

    " Execute validation
    COMMIT ENTITIES
      RESPONSE OF zkkr_i_position
        FAILED DATA(failed)
        REPORTED DATA(reported).

    " Verify result
    cl_abap_unit_assert=>assert_initial( failed ).
    cl_abap_unit_assert=>assert_initial( reported ).
  ENDMETHOD.

  METHOD test_validate_dates_missing_from.
    " Prepare test position
    MODIFY ENTITIES OF zkkr_i_position
      ENTITY Position
        CREATE FIELDS ( ValidFrom ValidTo )
        WITH VALUE #( ( %cid = 'TEST_POS_01'
                       ValidFrom = '00000000'
                       ValidTo = '20241231' ) ).

    " Execute validation
    COMMIT ENTITIES
      RESPONSE OF zkkr_i_position
        FAILED DATA(failed)
        REPORTED DATA(reported).

    " Verify result
    cl_abap_unit_assert=>assert_not_initial( failed ).
    cl_abap_unit_assert=>assert_not_initial( reported ).
  ENDMETHOD.

  METHOD test_validate_dates_invalid_range.
    " Prepare test position
    MODIFY ENTITIES OF zkkr_i_position
      ENTITY Position
        CREATE FIELDS ( ValidFrom ValidTo )
        WITH VALUE #( ( %cid = 'TEST_POS_01'
                       ValidFrom = '20241231'
                       ValidTo = '20240101' ) ).

    " Execute validation
    COMMIT ENTITIES
      RESPONSE OF zkkr_i_position
        FAILED DATA(failed)
        REPORTED DATA(reported).

    " Verify result
    cl_abap_unit_assert=>assert_not_initial( failed ).
    cl_abap_unit_assert=>assert_not_initial( reported ).
  ENDMETHOD.

  METHOD test_validate_instrument_success.
    " Prepare test data
    sql_environment->insert_test_data(
      EXPORTING
        i_data = VALUE ztrmfinins_tab( ( instrument_id = 'INST001' ) ) ).

    " Prepare test position
    MODIFY ENTITIES OF zkkr_i_position
      ENTITY Position
        CREATE FIELDS ( InstrumentID )
        WITH VALUE #( ( %cid = 'TEST_POS_01'
                       InstrumentID = 'INST001' ) ).

    " Execute validation
    COMMIT ENTITIES
      RESPONSE OF zkkr_i_position
        FAILED DATA(failed)
        REPORTED DATA(reported).

    " Verify result
    cl_abap_unit_assert=>assert_initial( failed ).
    cl_abap_unit_assert=>assert_initial( reported ).
  ENDMETHOD.

  METHOD test_validate_instrument_not_found.
    " Prepare test position
    MODIFY ENTITIES OF zkkr_i_position
      ENTITY Position
        CREATE FIELDS ( InstrumentID )
        WITH VALUE #( ( %cid = 'TEST_POS_01'
                       InstrumentID = 'INVALID' ) ).

    " Execute validation
    COMMIT ENTITIES
      RESPONSE OF zkkr_i_position
        FAILED DATA(failed)
        REPORTED DATA(reported).

    " Verify result
    cl_abap_unit_assert=>assert_not_initial( failed ).
    cl_abap_unit_assert=>assert_not_initial( reported ).
  ENDMETHOD.

  METHOD test_validate_cf_amount_success.
    " Prepare test cashflow
    MODIFY ENTITIES OF zkkr_i_position
      ENTITY Position
        CREATE
        WITH VALUE #( ( %cid = 'TEST_POS_01' ) )
      ENTITY Position\_Cashflow
        CREATE
        WITH VALUE #( ( %cid_ref = 'TEST_POS_01'
                       %target = VALUE #( ( %cid = 'TEST_CF_01'
                                          Amount = '1000.00'
                                          Currency = 'EUR' ) ) ) ).

    " Execute validation
    COMMIT ENTITIES
      RESPONSE OF zkkr_i_position
        FAILED DATA(failed)
        REPORTED DATA(reported).

    " Verify result
    cl_abap_unit_assert=>assert_initial( failed ).
    cl_abap_unit_assert=>assert_initial( reported ).
  ENDMETHOD.

  METHOD test_validate_cf_amount_negative.
    " Prepare test cashflow
    MODIFY ENTITIES OF zkkr_i_position
      ENTITY Position
        CREATE
        WITH VALUE #( ( %cid = 'TEST_POS_01' ) )
      ENTITY Position\_Cashflow
        CREATE
        WITH VALUE #( ( %cid_ref = 'TEST_POS_01'
                       %target = VALUE #( ( %cid = 'TEST_CF_01'
                                          Amount = '-1000.00'
                                          Currency = 'EUR' ) ) ) ).

    " Execute validation
    COMMIT ENTITIES
      RESPONSE OF zkkr_i_position
        FAILED DATA(failed)
        REPORTED DATA(reported).

    " Verify result
    cl_abap_unit_assert=>assert_not_initial( failed ).
    cl_abap_unit_assert=>assert_not_initial( reported ).
  ENDMETHOD.

  METHOD test_validate_cf_date_success.
    " Create test data
    DATA(value_date) = cl_abap_context_info=>get_system_date( ).

    " Prepare test cashflow
    MODIFY ENTITIES OF zkkr_i_position
      ENTITY Position
        CREATE
        WITH VALUE #( ( %cid = 'TEST_POS_01' ) )
      ENTITY Position\_Cashflow
        CREATE
        WITH VALUE #( ( %cid_ref = 'TEST_POS_01'
                       %target = VALUE #( ( %cid = 'TEST_CF_01'
                                          ValueDate = value_date ) ) ) ).

    " Execute validation
    COMMIT ENTITIES
      RESPONSE OF zkkr_i_position
        FAILED DATA(failed)
        REPORTED DATA(reported).

    " Verify result
    cl_abap_unit_assert=>assert_initial( failed ).
    cl_abap_unit_assert=>assert_initial( reported ).
  ENDMETHOD.

  METHOD test_validate_cf_date_missing.
    " Prepare test cashflow
    MODIFY ENTITIES OF zkkr_i_position
      ENTITY Position
        CREATE
        WITH VALUE #( ( %cid = 'TEST_POS_01' ) )
      ENTITY Position\_Cashflow
        CREATE
        WITH VALUE #( ( %cid_ref = 'TEST_POS_01'
                       %target = VALUE #( ( %cid = 'TEST_CF_01'
                                          ValueDate = '00000000' ) ) ) ).

    " Execute validation
    COMMIT ENTITIES
      RESPONSE OF zkkr_i_position
        FAILED DATA(failed)
        REPORTED DATA(reported).

    " Verify result
    cl_abap_unit_assert=>assert_not_initial( failed ).
    cl_abap_unit_assert=>assert_not_initial( reported ).
  ENDMETHOD.

  METHOD test_validate_cf_currency.
    " Prepare test cashflow without currency
    MODIFY ENTITIES OF zkkr_i_position
      ENTITY Position
        CREATE
        WITH VALUE #( ( %cid = 'TEST_POS_01' ) )
      ENTITY Position\_Cashflow
        CREATE
        WITH VALUE #( ( %cid_ref = 'TEST_POS_01'
                       %target = VALUE #( ( %cid = 'TEST_CF_01'
                                          Amount = '1000.00'
                                          Currency = '' ) ) ) ).

    " Execute validation
    COMMIT ENTITIES
      RESPONSE OF zkkr_i_position
        FAILED DATA(failed)
        REPORTED DATA(reported).

    " Verify result
    cl_abap_unit_assert=>assert_not_initial( failed ).
    cl_abap_unit_assert=>assert_not_initial( reported ).
  ENDMETHOD.

  METHOD test_calculate_position_id.
    " Prepare test data
    sql_environment->insert_test_data(
      EXPORTING
        i_data = VALUE ztrmpos_tab( ( position_id = 'POS0000001' ) ) ).

    " Prepare test position
    MODIFY ENTITIES OF zkkr_i_position
      ENTITY Position
        CREATE
        WITH VALUE #( ( %cid = 'TEST_POS_01' ) ).

    " Execute determination
    COMMIT ENTITIES
      RESPONSE OF zkkr_i_position
        FAILED DATA(failed)
        REPORTED DATA(reported).

    " Read created position
    READ ENTITIES OF zkkr_i_position
      ENTITY Position
        FIELDS ( PositionID )
        WITH VALUE #( ( %cid = 'TEST_POS_01' ) )
      RESULT DATA(positions).

    " Verify result
    cl_abap_unit_assert=>assert_initial( failed ).
    cl_abap_unit_assert=>assert_initial( reported ).
    cl_abap_unit_assert=>assert_equals(
      act = positions[ 1 ]-PositionID
      exp = 'POS0000002' ).
  ENDMETHOD.

  METHOD test_calculate_cashflow_id.
    " Prepare test data
    sql_environment->insert_test_data(
      EXPORTING
        i_data = VALUE ztrmcf_tab( ( cashflow_id = 'CF0000001' ) ) ).

    " Prepare test position with cashflow
    MODIFY ENTITIES OF zkkr_i_position
      ENTITY Position
        CREATE
        WITH VALUE #( ( %cid = 'TEST_POS_01' ) )
      ENTITY Position\_Cashflow
        CREATE
        WITH VALUE #( ( %cid_ref = 'TEST_POS_01'
                       %target = VALUE #( ( %cid = 'TEST_CF_01'
                                          Amount = '1000.00'
                                          Currency = 'EUR'
                                          ValueDate = sy-datum ) ) ) ).

    " Execute determination
    COMMIT ENTITIES
      RESPONSE OF zkkr_i_position
        FAILED DATA(failed)
        REPORTED DATA(reported).

    " Read created cashflow
    READ ENTITIES OF zkkr_i_position
      ENTITY Cashflow
        FIELDS ( CashflowID )
        WITH VALUE #( ( %cid = 'TEST_CF_01' ) )
      RESULT DATA(cashflows).

    " Verify result
    cl_abap_unit_assert=>assert_initial( failed ).
    cl_abap_unit_assert=>assert_initial( reported ).
    cl_abap_unit_assert=>assert_equals(
      act = cashflows[ 1 ]-CashflowID
      exp = 'CF0000002' ).
  ENDMETHOD.

  METHOD test_create_draft.
    " Create draft position
    MODIFY ENTITIES OF zkkr_i_position
      ENTITY Position
        CREATE
        WITH VALUE #( ( %cid = 'TEST_POS_01'
                       InstrumentID = 'INST001'
                       ValidFrom = sy-datum
                       ValidTo = sy-datum + 365 ) )
        CREATE BY \_Cashflow
        WITH VALUE #( ( %cid_ref = 'TEST_POS_01'
                       %target = VALUE #( ( %cid = 'TEST_CF_01'
                                          Amount = '1000.00'
                                          Currency = 'EUR'
                                          ValueDate = sy-datum ) ) ) ).

    " Execute draft creation
    COMMIT ENTITIES
      RESPONSE OF zkkr_i_position
        FAILED DATA(failed)
        REPORTED DATA(reported).

    " Verify result
    cl_abap_unit_assert=>assert_initial( failed ).
    cl_abap_unit_assert=>assert_initial( reported ).
  ENDMETHOD.

  METHOD test_activate_draft.
    " Create and activate draft
    MODIFY ENTITIES OF zkkr_i_position
      ENTITY Position
        EXECUTE Edit
        WITH VALUE #( ( %cid = 'TEST_POS_01' ) )
        EXECUTE Activate
        WITH VALUE #( ( %cid = 'TEST_POS_01' ) ).

    " Execute activation
    COMMIT ENTITIES
      RESPONSE OF zkkr_i_position
        FAILED DATA(failed)
        REPORTED DATA(reported).

    " Verify result
    cl_abap_unit_assert=>assert_initial( failed ).
    cl_abap_unit_assert=>assert_initial( reported ).
  ENDMETHOD.

  METHOD test_discard_draft.
    " Create and discard draft
    MODIFY ENTITIES OF zkkr_i_position
      ENTITY Position
        EXECUTE Edit
        WITH VALUE #( ( %cid = 'TEST_POS_01' ) )
        EXECUTE Discard
        WITH VALUE #( ( %cid = 'TEST_POS_01' ) ).

    " Execute discard
    COMMIT ENTITIES
      RESPONSE OF zkkr_i_position
        FAILED DATA(failed)
        REPORTED DATA(reported).

    " Verify result
    cl_abap_unit_assert=>assert_initial( failed ).
    cl_abap_unit_assert=>assert_initial( reported ).
  ENDMETHOD.

  METHOD test_create_position_with_cashflow.
    " Create position with cashflow
    MODIFY ENTITIES OF zkkr_i_position
      ENTITY Position
        CREATE
        WITH VALUE #( ( %cid = 'TEST_POS_01'
                       InstrumentID = 'INST001'
                       ValidFrom = sy-datum
                       ValidTo = sy-datum + 365 ) )
        CREATE BY \_Cashflow
        WITH VALUE #( ( %cid_ref = 'TEST_POS_01'
                       %target = VALUE #( ( %cid = 'TEST_CF_01'
                                          Amount = '1000.00'
                                          Currency = 'EUR'
                                          ValueDate = sy-datum ) ) ) ).

    " Execute creation
    COMMIT ENTITIES
      RESPONSE OF zkkr_i_position
        FAILED DATA(failed)
        REPORTED DATA(reported).

    " Read created position with cashflow
    READ ENTITIES OF zkkr_i_position
      ENTITY Position
        BY \_Cashflow
        WITH VALUE #( ( %cid = 'TEST_POS_01' ) )
      RESULT DATA(cashflows).

    " Verify result
    cl_abap_unit_assert=>assert_initial( failed ).
    cl_abap_unit_assert=>assert_initial( reported ).
    cl_abap_unit_assert=>assert_equals(
      act = lines( cashflows )
      exp = 1 ).
  ENDMETHOD.

  METHOD test_add_cashflow_to_position.
    " Create position first
    MODIFY ENTITIES OF zkkr_i_position
      ENTITY Position
        CREATE
        WITH VALUE #( ( %cid = 'TEST_POS_01'
                       InstrumentID = 'INST001'
                       ValidFrom = sy-datum
                       ValidTo = sy-datum + 365 ) ).

    " Add cashflow to existing position
    MODIFY ENTITIES OF zkkr_i_position
      ENTITY Position\_Cashflow
        CREATE
        WITH VALUE #( ( %cid_ref = 'TEST_POS_01'
                       %target = VALUE #( ( %cid = 'TEST_CF_01'
                                          Amount = '1000.00'
                                          Currency = 'EUR'
                                          ValueDate = sy-datum ) ) ) ).

    " Execute creation
    COMMIT ENTITIES
      RESPONSE OF zkkr_i_position
        FAILED DATA(failed)
        REPORTED DATA(reported).

    " Read position with cashflow
    READ ENTITIES OF zkkr_i_position
      ENTITY Position
        BY \_Cashflow
        WITH VALUE #( ( %cid = 'TEST_POS_01' ) )
      RESULT DATA(cashflows).

    " Verify result
    cl_abap_unit_assert=>assert_initial( failed ).
    cl_abap_unit_assert=>assert_initial( reported ).
    cl_abap_unit_assert=>assert_equals(
      act = lines( cashflows )
      exp = 1 ).
  ENDMETHOD.

  METHOD test_position_authorization.
    " Create test position
    MODIFY ENTITIES OF zkkr_i_position
      ENTITY Position
        CREATE
        WITH VALUE #( ( %cid = 'TEST_POS_01'
                       InstrumentID = 'INST001'
                       ValidFrom = sy-datum
                       ValidTo = sy-datum + 365 ) ).

    " Check authorizations
    READ ENTITIES OF zkkr_i_position
      ENTITY Position
        ALL FIELDS WITH VALUE #( ( %cid = 'TEST_POS_01' ) )
      RESULT DATA(positions)
      FAILED DATA(failed)
      REPORTED DATA(reported).

    " Verify result
    cl_abap_unit_assert=>assert_initial( failed ).
    cl_abap_unit_assert=>assert_initial( reported ).
    cl_abap_unit_assert=>assert_equals(
      act = lines( positions )
      exp = 1 ).
  ENDMETHOD.

ENDCLASS.