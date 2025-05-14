"! @testing BDEF:ZKKR_I_POSITION
CLASS ltcl_position DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    CLASS-DATA:
      environment TYPE REF TO if_abap_behavior_test_environment,
      sql_environment TYPE REF TO if_osql_test_environment.

    DATA: cut TYPE REF TO lhc_position.  "Class Under Test

    CLASS-METHODS:
      class_setup,
      class_teardown.

    METHODS:
      setup,
      teardown,

      " Test methods for validations
      test_validate_dates FOR TESTING,
      test_validate_instrument FOR TESTING,
      test_cf_amount FOR TESTING,
      test_validate_cashflow_date FOR TESTING,

      " Test methods for determinations
      test_calculate_position_id FOR TESTING,
      test_calculate_cashflow_id FOR TESTING,
      test_cf_amt_val FOR TESTING RAISING cx_static_check,
      test_position_validation FOR TESTING RAISING cx_static_check,
      test_invalid_dates FOR TESTING RAISING cx_static_check,
      test_instrument_validation FOR TESTING RAISING cx_static_check,
      test_cf_date_val FOR TESTING RAISING cx_static_check.

ENDCLASS.

CLASS ltcl_position IMPLEMENTATION.

  METHOD class_setup.
    " Create SQL test environment for database tables
    sql_environment = cl_osql_test_environment=>create(
      i_dependency_list = VALUE #(
        ( 'ZTRMPOS' )
        ( 'ZTRMCF' )
        ( 'ZTRMINST' )
      )
    ).

    " Create behavior test environment
    environment = cl_abap_behavior_test_environment=>create(
      EXPORTING
        entity_type_name = 'ZKKR_I_POSITION'
    ).
  ENDMETHOD.

  METHOD class_teardown.
    sql_environment->destroy( ).
    environment->destroy( ).
  ENDMETHOD.

  METHOD setup.
    CREATE OBJECT cut FOR TESTING.
    sql_environment->clear_doubles( ).
    environment->clear_doubles( ).
  ENDMETHOD.

  METHOD teardown.
    ROLLBACK ENTITIES.
  ENDMETHOD.

  METHOD test_validate_dates.
    " Given
    DATA(valid_from) = cl_abap_context_info=>get_system_date( ).
    DATA(valid_to) = valid_from + 30.

    " Test case 1: Valid dates
    environment->create_entities(
      ENTITIES OF zkkr_i_position
      ENTITY Position
      SET FIELDS WITH VALUE #(
        ( %key-PositionID = 'TEST1'
          ValidFrom = valid_from
          ValidTo = valid_to ) ) ).

    " When
    TRY.
        environment->save( ).
        cl_abap_unit_assert=>fail( 'Exception expected' ).
      CATCH cx_root INTO DATA(expected_exception).
    ENDTRY.

    " Then
    environment->get_validation_messages(
      IMPORTING
        messages = DATA(validation_messages) ).

    cl_abap_unit_assert=>assert_initial( validation_messages ).

    " Test case 2: Invalid dates (ValidTo before ValidFrom)
    environment->create_entities(
      ENTITIES OF zkkr_i_position
      ENTITY Position
      SET FIELDS WITH VALUE #(
        ( %key-PositionID = 'TEST2'
          ValidFrom = valid_to
          ValidTo = valid_from ) ) ).

    " When
    TRY.
        environment->save( ).
        cl_abap_unit_assert=>fail( 'Exception expected' ).
      CATCH cx_root INTO DATA(expected_exception2).
    ENDTRY.

    " Then
    environment->get_validation_messages(
      IMPORTING
        messages = validation_messages ).

    cl_abap_unit_assert=>assert_not_initial( validation_messages ).
  ENDMETHOD.

  METHOD test_validate_instrument.
    " Given
    sql_environment->insert_test_data(
      i_data = VALUE ztrminst_tab(
        ( instrument_id = 'INST1' ) ) ).

    " Test case 1: Valid instrument
    environment->create_entities(
      ENTITIES OF zkkr_i_position
      ENTITY Position
      SET FIELDS WITH VALUE #(
        ( %key-PositionID = 'TEST1'
          InstrumentID = 'INST1' ) ) ).

    " When
    TRY.
        environment->save( ).
        cl_abap_unit_assert=>fail( 'Exception expected' ).
      CATCH cx_root INTO DATA(expected_exception).
    ENDTRY.

    " Then
    environment->get_validation_messages(
      IMPORTING
        messages = DATA(validation_messages) ).

    cl_abap_unit_assert=>assert_initial( validation_messages ).

    " Test case 2: Invalid instrument
    environment->create_entities(
      ENTITIES OF zkkr_i_position
      ENTITY Position
      SET FIELDS WITH VALUE #(
        ( %key-PositionID = 'TEST2'
          InstrumentID = 'INVALID' ) ) ).

    " When
    TRY.
        environment->save( ).
        cl_abap_unit_assert=>fail( 'Exception expected' ).
      CATCH cx_root INTO DATA(expected_exception2).
    ENDTRY.

    " Then
    environment->get_validation_messages(
      IMPORTING
        messages = validation_messages ).

    cl_abap_unit_assert=>assert_not_initial( validation_messages ).
  ENDMETHOD.

  METHOD test_cf_amount.
    " Given
    DATA: failed   TYPE RESPONSE FOR FAILED LATE zkkr_i_position,
          reported TYPE RESPONSE FOR REPORTED LATE zkkr_i_position,
          lt_cashflow TYPE TABLE FOR CREATE zkkr_i_position\\Cashflow.

    " Test Case 1: Valid cashflow (Amount > 0 and Currency filled)
    APPEND VALUE #( %tky = VALUE #( positionid = 'TEST01'
                                   cashflowid = 'CF001' )
                   %data = VALUE #( cashflow_amount = '1000.00'
                                   waers = 'EUR' ) ) TO lt_cashflow.

    cut->validateCashflowAmount(
      EXPORTING
        keys = lt_cashflow
      CHANGING
        failed   = failed
        reported = reported
    ).

    " Then
    cl_abap_unit_assert=>assert_initial(
      act = failed-cashflow
      msg = 'Validation should pass for valid amount and currency' ).

    " Test Case 2: Invalid amount (Amount <= 0)
    CLEAR: failed, reported, lt_cashflow.

    APPEND VALUE #( %tky = VALUE #( positionid = 'TEST01'
                                   cashflowid = 'CF002' )
                   %data = VALUE #( cashflow_amount = '0.00'
                                   waers = 'EUR' ) ) TO lt_cashflow.

    cut->validateCashflowAmount(
      EXPORTING
        keys = lt_cashflow
      CHANGING
        failed   = failed
        reported = reported
    ).

    " Then
    cl_abap_unit_assert=>assert_not_initial(
      act = failed-cashflow
      msg = 'Validation should fail for zero amount' ).

    cl_abap_unit_assert=>assert_true(
      act = xsdbool( line_exists( reported-cashflow[ %element-cashflow_amount = if_abap_behv=>mk-on ] ) )
      msg = 'Amount field should be marked as erroneous' ).

    " Test Case 3: Missing currency
    CLEAR: failed, reported, lt_cashflow.

    APPEND VALUE #( %tky = VALUE #( positionid = 'TEST01'
                                   cashflowid = 'CF003' )
                   %data = VALUE #( cashflow_amount = '1000.00'
                                   waers = '' ) ) TO lt_cashflow.

    cut->validateCashflowAmount(
      EXPORTING
        keys = lt_cashflow
      CHANGING
        failed   = failed
        reported = reported
    ).

    " Then
    cl_abap_unit_assert=>assert_not_initial(
      act = failed-cashflow
      msg = 'Validation should fail for missing currency' ).

    cl_abap_unit_assert=>assert_true(
      act = xsdbool( line_exists( reported-cashflow[ %element-waers = if_abap_behv=>mk-on ] ) )
      msg = 'Currency field should be marked as erroneous' ).
  ENDMETHOD.

  METHOD test_validate_cashflow_date.
    " First create a position
    environment->create_entities(
      ENTITIES OF zkkr_i_position
      ENTITY Position
      SET FIELDS WITH VALUE #(
        ( %cid = 'POS1'
          InstrumentID = 'INST01'
          ValidFrom = cl_abap_context_info=>get_system_date( ) ) ) ).

    " Then create cashflow for the position
    environment->create_entities(
      ENTITIES OF zkkr_i_position
      ENTITY Position\_Cashflow
      SET FIELDS WITH VALUE #(
        ( %cid_ref = 'POS1'
          %target = VALUE #( ( %cid = 'CF1'
                              ValueDate = cl_abap_context_info=>get_system_date( ) ) ) ) ) ).

    " When
    TRY.
        environment->save( ).
        cl_abap_unit_assert=>fail( 'Exception expected' ).
      CATCH cx_root INTO DATA(expected_exception).
    ENDTRY.

    " Then
    environment->get_validation_messages(
      IMPORTING
        messages = DATA(validation_messages) ).

    cl_abap_unit_assert=>assert_initial( validation_messages ).

    " Test case 2: Invalid value date (initial)
    environment->create_entities(
      ENTITIES OF zkkr_i_position
      ENTITY Position\_Cashflow
      SET FIELDS WITH VALUE #(
        ( %cid_ref = 'POS1'
          %target = VALUE #( ( %cid = 'CF2' ) ) ) ) ).

    " When
    TRY.
        environment->save( ).
        cl_abap_unit_assert=>fail( 'Exception expected' ).
      CATCH cx_root INTO DATA(expected_exception2).
    ENDTRY.

    " Then
    environment->get_validation_messages(
      IMPORTING
        messages = validation_messages ).

    cl_abap_unit_assert=>assert_not_initial( validation_messages ).
  ENDMETHOD.

  METHOD test_calculate_position_id.
    " Given
    sql_environment->insert_test_data(
      i_data = VALUE ztrmpos_tab(
        ( position_id = 'POS0000001' ) ) ).

    " When
    environment->create_entities(
      ENTITIES OF zkkr_i_position
      ENTITY Position
      SET FIELDS WITH VALUE #(
        ( %key-PositionID = '' ) ) ).

    TRY.
        environment->save( ).
        cl_abap_unit_assert=>fail( 'Exception expected' ).
      CATCH cx_root INTO DATA(expected_exception).
    ENDTRY.

    " Then
    environment->get_created_entities(
      IMPORTING
        entities = DATA(created_entities) ).

    READ ENTITIES OF zkkr_i_position
      ENTITY Position
      ALL FIELDS WITH VALUE #( ( %key = created_entities[ 1 ]-%key ) )
      RESULT DATA(positions).

    cl_abap_unit_assert=>assert_equals(
      exp = 'POS0000002'
      act = positions[ 1 ]-PositionID ).
  ENDMETHOD.

  METHOD test_calculate_cashflow_id.
    " Given
    sql_environment->insert_test_data(
      i_data = VALUE ztrmcf_tab(
        ( cashflow_id = 'CF0000001' ) ) ).

    " When
    environment->create_entities(
      ENTITIES OF zkkr_i_position
      ENTITY Cashflow
      SET FIELDS WITH VALUE #(
        ( %key-CashflowID = '' ) ) ).

    TRY.
        environment->save( ).
        cl_abap_unit_assert=>fail( 'Exception expected' ).
      CATCH cx_root INTO DATA(expected_exception).
    ENDTRY.

    " Then
    environment->get_created_entities(
      IMPORTING
        entities = DATA(created_entities) ).

    READ ENTITIES OF zkkr_i_position
      ENTITY Cashflow
      ALL FIELDS WITH VALUE #( ( %key = created_entities[ 1 ]-%key ) )
      RESULT DATA(cashflows).

    cl_abap_unit_assert=>assert_equals(
      exp = 'CF0000002'
      act = cashflows[ 1 ]-CashflowID ).
  ENDMETHOD.

  METHOD test_cf_amt_val.
    " Given
    DATA: failed   TYPE RESPONSE FOR FAILED LATE zkkr_i_position,
          reported TYPE RESPONSE FOR REPORTED LATE zkkr_i_position.

    " First create a position without PositionID (it's read-only)
    MODIFY ENTITIES OF zkkr_i_position
      ENTITY Position
        CREATE FIELDS ( InstrumentID ValidFrom )
        WITH VALUE #( ( %cid = 'POS1'
                       InstrumentID = 'INST01'
                       ValidFrom = cl_abap_context_info=>get_system_date( ) ) )
      MAPPED DATA(mapped_pos)
      FAILED DATA(failed_pos)
      REPORTED DATA(reported_pos).

    " Ensure position was created successfully
    cl_abap_unit_assert=>assert_initial(
      act = failed_pos
      msg = 'Position creation should succeed' ).

    " Test Case 1: Valid cashflow
    MODIFY ENTITIES OF zkkr_i_position
      ENTITY Position
        CREATE BY \_Cashflow
        FIELDS ( Amount Currency )
        WITH VALUE #(
          ( %tky = mapped_pos-position[ 1 ]-%tky  "Use the mapped key
            %target = VALUE #(
              ( %cid = 'CF1'
                Amount = '1000.00'
                Currency = 'EUR' ) ) ) )
      MAPPED DATA(mapped_cf)
      FAILED DATA(failed_cf)
      REPORTED DATA(reported_cf).

    " Then - Valid case
    cl_abap_unit_assert=>assert_initial(
      act = failed_cf-cashflow
      msg = 'Validation should pass for valid amount and currency' ).

    " Test Case 2: Invalid amount
    MODIFY ENTITIES OF zkkr_i_position
      ENTITY Position
        CREATE BY \_Cashflow
        FIELDS ( Amount Currency )
        WITH VALUE #(
          ( %tky = mapped_pos-position[ 1 ]-%tky  "Use the mapped key
            %target = VALUE #(
              ( %cid = 'CF2'
                Amount = '0.00'
                Currency = 'EUR' ) ) ) )
      MAPPED DATA(mapped_cf2)
      FAILED DATA(failed_cf2)
      REPORTED DATA(reported_cf2).

    " Then - Invalid amount
    cl_abap_unit_assert=>assert_not_initial(
      act = failed_cf2-cashflow
      msg = 'Validation should fail for zero amount' ).

    " Test Case 3: Missing currency
    MODIFY ENTITIES OF zkkr_i_position
      ENTITY Position
        CREATE BY \_Cashflow
        FIELDS ( Amount Currency )
        WITH VALUE #(
          ( %tky = mapped_pos-position[ 1 ]-%tky  "Use the mapped key
            %target = VALUE #(
              ( %cid = 'CF3'
                Amount = '1000.00'
                Currency = '' ) ) ) )
      MAPPED DATA(mapped_cf3)
      FAILED DATA(failed_cf3)
      REPORTED DATA(reported_cf3).

    " Then - Missing currency
    cl_abap_unit_assert=>assert_not_initial(
      act = failed_cf3-cashflow
      msg = 'Validation should fail for missing currency' ).

  ENDMETHOD.

  METHOD test_position_validation.
    " Given
    DATA: lt_position   TYPE TABLE OF ztrmpos,
          lt_instrument TYPE TABLE OF ztrminst,
          failed        TYPE RESPONSE FOR FAILED LATE zkkr_i_position,
          reported      TYPE RESPONSE FOR REPORTED LATE zkkr_i_position.

    " Prepare test data
    lt_instrument = VALUE #( ( instrument_id = 'INST01'
                              instrument_type = 'BOND'
                              currency = 'EUR' ) ).

    lt_position = VALUE #( ( position_id = 'TEST01'
                            instrument_id = 'INST01'
                            valid_from = cl_abap_context_info=>get_system_date( )
                            valid_to = cl_abap_context_info=>get_system_date( ) + 365 ) ).

    " Insert test data
    sql_environment->insert_test_data( lt_instrument ).
    sql_environment->insert_test_data( lt_position ).

    " When
    cut->validateDates(
      EXPORTING
        keys = VALUE #( ( %tky = VALUE #( PositionID = lt_position[ 1 ]-position_id )
                         %is_draft = if_abap_behv=>mk-off ) )
      CHANGING
        failed   = failed
        reported = reported
    ).

    " Then
    cl_abap_unit_assert=>assert_initial(
        act = failed-position
        msg = 'Position validation should pass for valid dates' ).
  ENDMETHOD.

  METHOD test_invalid_dates.
    " Given
    DATA: lt_position   TYPE TABLE OF ztrmpos,
          lt_instrument TYPE TABLE OF ztrminst,
          failed        TYPE RESPONSE FOR FAILED LATE zkkr_i_position,
          reported      TYPE RESPONSE FOR REPORTED LATE zkkr_i_position,
          lt_create     TYPE TABLE FOR CREATE zkkr_i_position\\Position,
          lt_created    TYPE TABLE FOR READ RESULT zkkr_i_position\\Position.

    " Prepare test data with invalid date range (valid_to before valid_from)
    lt_position = VALUE #( ( position_id = 'TEST02'
                            instrument_id = 'INST02'
                            valid_from = cl_abap_context_info=>get_system_date( ) + 10
                            valid_to = cl_abap_context_info=>get_system_date( ) ) ).

    " Create test position in test environment
    lt_create = VALUE #( ( %cid = 'TEST02'
                          InstrumentID = lt_position[ 1 ]-instrument_id
                          ValidFrom = lt_position[ 1 ]-valid_from
                          ValidTo = lt_position[ 1 ]-valid_to ) ).

    MODIFY ENTITIES OF zkkr_i_position
      ENTITY Position
        CREATE FIELDS ( InstrumentID ValidFrom ValidTo )
        WITH lt_create
      MAPPED DATA(mapped)
      FAILED DATA(create_failed)
      REPORTED DATA(create_reported).

    " Get the created position
    READ ENTITIES OF zkkr_i_position
      ENTITY Position
        ALL FIELDS
        WITH VALUE #( ( %tky = mapped-position[ 1 ]-%tky ) )
      RESULT lt_created.

    " When
    cut->validateDates(
      EXPORTING
        keys = VALUE #( ( %tky = lt_created[ 1 ]-%tky
                         %is_draft = if_abap_behv=>mk-off ) )
      CHANGING
        failed   = failed
        reported = reported
    ).

    " Then
    cl_abap_unit_assert=>assert_not_initial(
        act = failed-position
        msg = 'Position validation should fail for invalid date range' ).
  ENDMETHOD.

  METHOD test_instrument_validation.
    " Given
    DATA: lt_instrument TYPE TABLE OF ztrminst,
          failed        TYPE RESPONSE FOR FAILED LATE zkkr_i_position,
          reported      TYPE RESPONSE FOR REPORTED LATE zkkr_i_position,
          lt_create     TYPE TABLE FOR CREATE zkkr_i_position\\Position,
          lt_created    TYPE TABLE FOR READ RESULT zkkr_i_position\\Position.

    " Prepare test data - valid instrument
    lt_instrument = VALUE #( ( client = sy-mandt          "Add client field
                              instrument_id = 'INST03'
                              instrument_type = 'BOND'
                              currency = 'EUR'
                              instrument_descr = 'Test Bond'
                              nominal_amount = '1000.00'
                              created_by = sy-uname
                              created_at = cl_abap_context_info=>get_system_time( )
                            ) ).

    " Insert test instrument
    sql_environment->insert_test_data( lt_instrument ).

    " Create test position with valid instrument
    lt_create = VALUE #(
      ( %cid = 'TEST_VALID'
        InstrumentID = 'INST03'  " Valid instrument
        ValidFrom = cl_abap_context_info=>get_system_date( ) )
    ).

    " Create test position
    MODIFY ENTITIES OF zkkr_i_position
      ENTITY Position
        CREATE FIELDS ( InstrumentID ValidFrom )
        WITH lt_create
      MAPPED DATA(mapped)
      FAILED DATA(create_failed)
      REPORTED DATA(create_reported).

    " Check if creation was successful
    cl_abap_unit_assert=>assert_initial(
        act = create_failed-position
        msg = 'Position creation with valid instrument should succeed' ).

    " Test validation of valid instrument
    IF mapped-position IS NOT INITIAL.
      cut->validateInstrument(
        EXPORTING
          keys = VALUE #( ( %tky = mapped-position[ 1 ]-%tky
                           %is_draft = if_abap_behv=>mk-off ) )
        CHANGING
          failed   = failed
          reported = reported
      ).

      " Then - Valid instrument should pass
      cl_abap_unit_assert=>assert_initial(
          act = failed-position
          msg = 'Validation should pass for existing instrument' ).
    ENDIF.

    " Test 2: Invalid Instrument
    " -------------------------
    CLEAR: failed, reported, lt_create.

    " Create test position with non-existent instrument
    lt_create = VALUE #(
      ( %cid = 'TEST_INVALID'
        InstrumentID = 'NONEXIST'  " Invalid instrument
        ValidFrom = cl_abap_context_info=>get_system_date( ) )
    ).

    " Create test position
    MODIFY ENTITIES OF zkkr_i_position
      ENTITY Position
        CREATE FIELDS ( InstrumentID ValidFrom )
        WITH lt_create
      MAPPED DATA(mapped_invalid)
      FAILED DATA(create_failed_invalid)
      REPORTED DATA(create_reported_invalid).

    " Test validation of invalid instrument
    IF mapped_invalid-position IS NOT INITIAL.
      cut->validateInstrument(
        EXPORTING
          keys = VALUE #( ( %tky = mapped_invalid-position[ 1 ]-%tky
                           %is_draft = if_abap_behv=>mk-off ) )
        CHANGING
          failed   = failed
          reported = reported
      ).

      " Then - Invalid instrument should fail
      cl_abap_unit_assert=>assert_not_initial(
          act = failed-position
          msg = 'Validation should fail for non-existent instrument' ).

      " Verify error message
      IF reported-position IS NOT INITIAL.
        READ TABLE reported-position INTO DATA(reported_line) INDEX 1.
        cl_abap_unit_assert=>assert_equals(
            act = reported_line-%element-InstrumentID
            exp = if_abap_behv=>mk-on
            msg = 'Error should be reported on InstrumentID element' ).
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD test_cf_date_val.
    " Given
    DATA: failed   TYPE RESPONSE FOR FAILED LATE zkkr_i_position,
          reported TYPE RESPONSE FOR REPORTED LATE zkkr_i_position.

    " First create a position without PositionID (it's read-only)
    MODIFY ENTITIES OF zkkr_i_position
      ENTITY Position
        CREATE FIELDS ( InstrumentID ValidFrom )
        WITH VALUE #( ( %cid = 'POS1'
                       InstrumentID = 'INST01'
                       ValidFrom = cl_abap_context_info=>get_system_date( ) ) )
      MAPPED DATA(mapped_pos)
      FAILED DATA(failed_pos)
      REPORTED DATA(reported_pos).

    " Ensure position was created successfully
    cl_abap_unit_assert=>assert_initial(
      act = failed_pos
      msg = 'Position creation should succeed' ).

    " Test Case 1: Valid future date
    DATA(future_date) = cl_abap_context_info=>get_system_date( ) + 1.

    MODIFY ENTITIES OF zkkr_i_position
      ENTITY Position
        CREATE BY \_Cashflow
        FIELDS ( ValueDate )
        WITH VALUE #(
          ( %tky = mapped_pos-position[ 1 ]-%tky
            %target = VALUE #(
              ( %cid = 'CF1'
                ValueDate = future_date ) ) ) )
      MAPPED DATA(mapped_cf)
      FAILED DATA(failed_cf)
      REPORTED DATA(reported_cf).

    " Then - Valid case
    cl_abap_unit_assert=>assert_initial(
      act = failed_cf-cashflow
      msg = 'Validation should pass for future date' ).

    " Test Case 2: Invalid past date
    DATA(past_date) = cl_abap_context_info=>get_system_date( ) - 1.

    MODIFY ENTITIES OF zkkr_i_position
      ENTITY Position
        CREATE BY \_Cashflow
        FIELDS ( ValueDate )
        WITH VALUE #(
          ( %tky = mapped_pos-position[ 1 ]-%tky
            %target = VALUE #(
              ( %cid = 'CF2'
                ValueDate = past_date ) ) ) )
      MAPPED DATA(mapped_cf2)
      FAILED DATA(failed_cf2)
      REPORTED DATA(reported_cf2).

    " Then - Past date should fail
    cl_abap_unit_assert=>assert_not_initial(
      act = failed_cf2-cashflow
      msg = 'Validation should fail for past date' ).

  ENDMETHOD.

ENDCLASS.