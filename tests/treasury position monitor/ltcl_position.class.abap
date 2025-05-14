CLASS ltcl_position DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    CLASS-DATA:
      environment TYPE REF TO if_cds_test_environment,
      environment_database TYPE REF TO if_osql_test_environment.

    CLASS-DATA:
      lt_position TYPE TABLE OF ztrmpos,           "Changed from zkkr_i_position
      lt_cashflow TYPE TABLE OF ztrmcf,           "Changed from zkkr_i_cashflow
      lt_instrument TYPE TABLE OF ztrminst,
      lt_position_draft TYPE TABLE OF zkkr_dpos,  "Renamed for clarity
      lt_cashflow_draft TYPE TABLE OF zkkr_dcf.   "Renamed for clarity

    CLASS-METHODS:
      class_setup,
      class_teardown,
      setup,
      test_pos_val_success.

    DATA:
      lo_cut TYPE REF TO lhc_position.  "Class Under Test

  METHOD setup.
    CREATE OBJECT lo_cut FOR TESTING.

    " Prepare test data
    lt_instrument = VALUE #(
      ( InstrumentID = 'INST001'
        InstrumentType = 'BOND'
        Currency = 'EUR'
        CreatedBy = sy-uname
        CreatedAt = cl_abap_context_info=>get_system_timestamp( )
        LastChangedBy = sy-uname
        LastChangedAt = cl_abap_context_info=>get_system_timestamp( )
      )
    ).

    lt_position = VALUE #(
      ( PositionID = 'POS000001'
        InstrumentID = 'INST001'
        ValidFrom = cl_abap_context_info=>get_system_date( )
        ValidTo = cl_abap_context_info=>get_system_date( ) + 365
        CreatedBy = sy-uname
        CreatedAt = cl_abap_context_info=>get_system_timestamp( )
        LastChangedBy = sy-uname
        LastChangedAt = cl_abap_context_info=>get_system_timestamp( )
      )
    ).

    " Insert test data
    environment->insert_test_data( lt_position ).
    environment_database->insert_test_data( lt_instrument ).
  ENDMETHOD.

  METHOD test_pos_val_success.
    " Given
    lt_position = VALUE #( (
        PositionID = 'POS000002'
        InstrumentID = 'INST001'
        ValidFrom = cl_abap_context_info=>get_system_date( )
        ValidTo = cl_abap_context_info=>get_system_date( ) + 365
    ) ).
    environment->insert_test_data( lt_position ).

    DATA: failed   TYPE RESPONSE FOR FAILED LATE zkkr_i_position,
          reported TYPE RESPONSE FOR REPORTED LATE zkkr_i_position.

    " When
    lo_cut->validateDates(
      EXPORTING
        keys = VALUE #( (
            PositionID = lt_position[ 1 ]-PositionID
            %is_draft = if_abap_behv=>mk-off
        ) )
      CHANGING
        failed   = failed
        reported = reported ).

    " Then
    cl_abap_unit_assert=>assert_initial(
        act = failed-position
        msg = 'Position validation should pass for valid dates' ).
  ENDMETHOD.

// ... existing code ...