CLASS ltcl_market_data DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    CLASS-DATA:
      environment TYPE REF TO if_cl_test_environment.

    CLASS-METHODS:
      class_setup,
      class_teardown.

    METHODS:
      setup,
      teardown,

      " Test methods for market data validation
      test_valid_fx_data FOR TESTING,
      test_invalid_fx_format FOR TESTING,
      test_invalid_market_type FOR TESTING,
      test_negative_price FOR TESTING,

      " Test methods for date validation
      test_valid_dates FOR TESTING,
      test_past_valid_from FOR TESTING,
      test_invalid_date_range FOR TESTING,

      " Test methods for price range validation
      test_fx_price_within_range FOR TESTING,
      test_fx_price_exceeds_range FOR TESTING,
      test_ir_price_within_range FOR TESTING,
      test_ir_price_exceeds_range FOR TESTING,

      " Test methods for confidence score
      test_bloomberg_confidence FOR TESTING,
      test_reuters_confidence FOR TESTING,
      test_manual_confidence FOR TESTING,

      " Test methods for actions
      test_validate_action FOR TESTING,
      test_reject_action FOR TESTING,

      " Helper methods
      create_test_data,
      cleanup_test_data.

    DATA:
      cut TYPE REF TO zbp_trm_market_data.

ENDCLASS.

CLASS ltcl_market_data IMPLEMENTATION.

  METHOD class_setup.
    environment = cl_test_environment=>create(
      i_package_name = 'Z_TRM_RISK'
    ).
  ENDMETHOD.

  METHOD class_teardown.
    environment->destroy( ).
  ENDMETHOD.

  METHOD setup.
    cut = NEW zbp_trm_market_data( ).
    create_test_data( ).
  ENDMETHOD.

  METHOD teardown.
    cleanup_test_data( ).
  ENDMETHOD.

  METHOD test_valid_fx_data.
    " Given
    DATA(ls_market_data) = VALUE z_trm_market_data(
      market_data_uuid = '1234567890'
      market_data_type = 'FX'
      instrument_id = 'EURUSD'
      price_value = '1.1234'
      price_currency = 'USD'
      valid_from = cl_abap_context_info=>get_system_date( )
      data_source = 'BLOOMBERG'
    ).

    " When
    cut->validateMarketData(
      EXPORTING
        keys = VALUE #( ( %key = ls_market_data ) )
    ).

    " Then
    cl_abap_unit_assert=>assert_initial(
      act = failed-marketdata
      msg = 'Validation should succeed for valid FX data'
    ).
  ENDMETHOD.

  METHOD test_invalid_fx_format.
    " Given
    DATA(ls_market_data) = VALUE z_trm_market_data(
      market_data_uuid = '1234567890'
      market_data_type = 'FX'
      instrument_id = 'EUR'  " Invalid format
      price_value = '1.1234'
      price_currency = 'USD'
    ).

    " When
    cut->validateMarketData(
      EXPORTING
        keys = VALUE #( ( %key = ls_market_data ) )
    ).

    " Then
    cl_abap_unit_assert=>assert_not_initial(
      act = failed-marketdata
      msg = 'Validation should fail for invalid FX format'
    ).
  ENDMETHOD.

  METHOD test_bloomberg_confidence.
    " Given
    DATA(ls_market_data) = VALUE z_trm_market_data(
      market_data_uuid = '1234567890'
      data_source = 'BLOOMBERG'
    ).

    " When
    cut->calculateConfidenceScore(
      EXPORTING
        keys = VALUE #( ( %key = ls_market_data ) )
    ).

    " Then
    cl_abap_unit_assert=>assert_equals(
      act = ls_market_data-confidence_score
      exp = '0.95'
      msg = 'Bloomberg data should have 0.95 confidence score'
    ).
  ENDMETHOD.

  METHOD test_validate_action.
    " Given
    DATA(ls_market_data) = VALUE z_trm_market_data(
      market_data_uuid = '1234567890'
      market_data_type = 'FX'
      instrument_id = 'EURUSD'
      price_value = '1.1234'
      data_source = 'BLOOMBERG'
    ).

    " When
    cut->validateData(
      EXPORTING
        keys = VALUE #( ( %key = ls_market_data ) )
      IMPORTING
        result = DATA(result)
    ).

    " Then
    READ TABLE result INTO DATA(ls_result) INDEX 1.
    cl_abap_unit_assert=>assert_equals(
      act = ls_result-%param-IsValidated
      exp = 'X'
      msg = 'Data should be marked as validated'
    ).
  ENDMETHOD.

  METHOD create_test_data.
    " Insert test data
    INSERT INTO z_trm_market_data VALUES @(
      VALUE #(
        market_data_uuid = '1234567890'
        market_data_type = 'FX'
        instrument_id = 'EURUSD'
        price_value = '1.1234'
        price_currency = 'USD'
        valid_from = cl_abap_context_info=>get_system_date( )
        data_source = 'BLOOMBERG'
      )
    ).
  ENDMETHOD.

  METHOD cleanup_test_data.
    " Clean up test data
    DELETE FROM z_trm_market_data WHERE market_data_uuid = '1234567890'.
  ENDMETHOD.

ENDCLASS.