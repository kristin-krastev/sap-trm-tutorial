"! @testing BDEF:ZKKR_I_POSITION
CLASS ltcl_position DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    CLASS-DATA:
      sql_environment TYPE REF TO if_osql_test_environment.

    DATA:
      cut TYPE REF TO lhc_position.  "Class Under Test

    CLASS-METHODS:
      class_setup,
      class_teardown.

    METHODS:
      setup,
      teardown,
      test_position_validation FOR TESTING RAISING cx_static_check,
      test_invalid_dates FOR TESTING RAISING cx_static_check.

ENDCLASS.

CLASS ltcl_position IMPLEMENTATION.

  METHOD class_setup.
    " Create SQL test environment for database tables
    sql_environment = cl_osql_test_environment=>create(
      i_dependency_list = VALUE #(
        ( 'ZTRMPOS' )
        ( 'ZTRMINST' )
      )
    ).
  ENDMETHOD.

  METHOD class_teardown.
    sql_environment->destroy( ).
  ENDMETHOD.

  METHOD setup.
    CREATE OBJECT cut FOR TESTING.
  ENDMETHOD.

  METHOD teardown.
    sql_environment->clear_doubles( ).
    ROLLBACK ENTITIES.
  ENDMETHOD.

  METHOD test_position_validation.
    " Given
    DATA: lt_position   TYPE TABLE OF ztrmpos,
          lt_instrument TYPE TABLE OF ztrminst,
          failed       TYPE RESPONSE FOR FAILED LATE zkkr_i_position,
          reported     TYPE RESPONSE FOR REPORTED LATE zkkr_i_position.

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
          failed       TYPE RESPONSE FOR FAILED LATE zkkr_i_position,
          reported     TYPE RESPONSE FOR REPORTED LATE zkkr_i_position,
          lt_create    TYPE TABLE FOR CREATE zkkr_i_position\\Position,
          lt_created   TYPE TABLE FOR READ RESULT zkkr_i_position\\Position.

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

ENDCLASS.