CLASS ltcl_var_calculator_base DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA:
      mo_calculator TYPE REF TO zcl_trm_var_calculator_base.

    METHODS:
      setup,
      test_validate_config FOR TESTING,
      test_validate_market_data FOR TESTING,
      test_calculate_returns FOR TESTING,
      test_currency_conversion FOR TESTING.
ENDCLASS.

CLASS ltcl_var_calculator_base IMPLEMENTATION.

  METHOD setup.
    " Create test double since base class is abstract
    mo_calculator ?= cl_abap_testdouble=>create( 'ZCL_TRM_VAR_CALCULATOR_BASE' ).
  ENDMETHOD.

  METHOD test_validate_config.
    DATA: ls_config TYPE zif_trm_var_calculator=>ty_var_config.

    " Test invalid confidence level
    ls_config = VALUE #(
      config_id = '1234567890ABCDEF'
      confidence_level = '0.85'
      time_horizon = 10
      historical_window = 250
      calculation_method = 'HIST'
      base_currency = 'USD' ).

    TRY.
      mo_calculator->zif_trm_var_calculator~validate_input(
        is_config = ls_config
        it_market_data = VALUE #( ) ).
      cl_abap_unit_assert=>fail( 'Exception expected for invalid confidence level' ).
    CATCH cx_static_check INTO DATA(lx_error).
      cl_abap_unit_assert=>assert_bound( lx_error ).
    ENDTRY.

    " Test invalid time horizon
    ls_config-confidence_level = '0.95'.
    ls_config-time_horizon = 300.

    TRY.
      mo_calculator->zif_trm_var_calculator~validate_input(
        is_config = ls_config
        it_market_data = VALUE #( ) ).
      cl_abap_unit_assert=>fail( 'Exception expected for invalid time horizon' ).
    CATCH cx_static_check INTO lx_error.
      cl_abap_unit_assert=>assert_bound( lx_error ).
    ENDTRY.

    " Test valid configuration
    ls_config-time_horizon = 10.

    TRY.
      mo_calculator->zif_trm_var_calculator~validate_input(
        is_config = ls_config
        it_market_data = VALUE #( ) ).
      cl_abap_unit_assert=>assert_initial( lx_error ).
    CATCH cx_static_check INTO lx_error.
      cl_abap_unit_assert=>fail( 'No exception expected for valid config' ).
    ENDTRY.
  ENDMETHOD.

  METHOD test_validate_market_data.
    DATA: lt_market_data TYPE zif_trm_var_calculator=>tt_market_data,
          ls_config     TYPE zif_trm_var_calculator=>ty_var_config.

    " Prepare valid config
    ls_config = VALUE #(
      config_id = '1234567890ABCDEF'
      confidence_level = '0.95'
      time_horizon = 10
      historical_window = 250
      calculation_method = 'HIST'
      base_currency = 'USD' ).

    " Test incomplete market data
    lt_market_data = VALUE #( (
      instrument_id = 'STOCK1'
      price_date = '20240101'
      price_value = 100
      currency = '' ) ).

    TRY.
      mo_calculator->zif_trm_var_calculator~validate_input(
        is_config = ls_config
        it_market_data = lt_market_data ).
      cl_abap_unit_assert=>fail( 'Exception expected for incomplete market data' ).
    CATCH cx_static_check INTO DATA(lx_error).
      cl_abap_unit_assert=>assert_bound( lx_error ).
    ENDTRY.

    " Test valid market data
    lt_market_data = VALUE #( (
      instrument_id = 'STOCK1'
      price_date = '20240101'
      price_value = 100
      currency = 'USD' ) ).

    TRY.
      mo_calculator->zif_trm_var_calculator~validate_input(
        is_config = ls_config
        it_market_data = lt_market_data ).
      cl_abap_unit_assert=>assert_initial( lx_error ).
    CATCH cx_static_check INTO lx_error.
      cl_abap_unit_assert=>fail( 'No exception expected for valid market data' ).
    ENDTRY.
  ENDMETHOD.

  METHOD test_calculate_returns.
    DATA: lt_market_data TYPE zif_trm_var_calculator=>tt_market_data,
          lt_returns    TYPE STANDARD TABLE OF ty_return.

    " Prepare test data
    lt_market_data = VALUE #(
      ( instrument_id = 'STOCK1' price_date = '20240101' price_value = 100 currency = 'USD' )
      ( instrument_id = 'STOCK1' price_date = '20240102' price_value = 110 currency = 'USD' )
      ( instrument_id = 'STOCK1' price_date = '20240103' price_value = 105 currency = 'USD' ) ).

    " Calculate returns
    lt_returns = mo_calculator->calculate_returns( lt_market_data ).

    " Verify number of returns
    cl_abap_unit_assert=>assert_equals(
      exp = 2
      act = lines( lt_returns )
      msg = 'Wrong number of returns calculated' ).

    " Verify return values
    READ TABLE lt_returns WITH KEY return_date = '20240102' INTO DATA(ls_return).
    cl_abap_unit_assert=>assert_subrc( exp = 0 msg = 'Return not found' ).
    cl_abap_unit_assert=>assert_equals(
      exp = log( 110 / 100 )
      act = ls_return-return_value
      msg = 'Wrong return value calculated' ).
  ENDMETHOD.

  METHOD test_currency_conversion.
    DATA: ls_config TYPE zif_trm_var_calculator=>ty_var_config.

    " Initialize calculator with USD base currency
    ls_config-base_currency = 'USD'.
    mo_calculator->zif_trm_var_calculator~initialize( ls_config ).

    " Test same currency conversion
    DATA(lv_result) = mo_calculator->convert_to_base_currency(
      iv_amount = 100
      iv_currency = 'USD' ).

    cl_abap_unit_assert=>assert_equals(
      exp = 100
      act = lv_result
      msg = 'Wrong conversion for same currency' ).

    " Note: More tests would be added when currency conversion is implemented
  ENDMETHOD.

ENDCLASS.