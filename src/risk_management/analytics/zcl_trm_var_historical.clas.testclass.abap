CLASS ltcl_var_historical DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA:
      mo_calculator TYPE REF TO zcl_trm_var_historical.

    METHODS:
      setup,
      test_calculate_var FOR TESTING,
      test_calculate_percentile FOR TESTING,
      test_var_with_empty_data FOR TESTING,
      test_var_single_instrument FOR TESTING,
      test_var_multiple_instruments FOR TESTING.
ENDCLASS.

CLASS ltcl_var_historical IMPLEMENTATION.

  METHOD setup.
    " Create calculator instance
    CREATE OBJECT mo_calculator.

    " Initialize with test configuration
    DATA(ls_config) = VALUE zif_trm_var_calculator=>ty_var_config(
      config_id = cl_system_uuid=>create_uuid_x16_static( )
      confidence_level = '0.95'
      time_horizon = 10
      historical_window = 250
      calculation_method = 'HIST'
      base_currency = 'USD' ).

    mo_calculator->zif_trm_var_calculator~initialize( ls_config ).
  ENDMETHOD.

  METHOD test_calculate_var.
    DATA: lt_market_data TYPE zif_trm_var_calculator=>tt_market_data.

    " Prepare test data with known price movements
    lt_market_data = VALUE #(
      ( instrument_id = 'STOCK1' price_date = '20240101' price_value = 100 currency = 'USD' )
      ( instrument_id = 'STOCK1' price_date = '20240102' price_value = 95  currency = 'USD' )
      ( instrument_id = 'STOCK1' price_date = '20240103' price_value = 105 currency = 'USD' )
      ( instrument_id = 'STOCK1' price_date = '20240104' price_value = 98  currency = 'USD' )
      ( instrument_id = 'STOCK1' price_date = '20240105' price_value = 102 currency = 'USD' ) ).

    " Calculate VaR
    DATA(ls_result) = mo_calculator->zif_trm_var_calculator~calculate_var(
      iv_portfolio_id = 'TEST_PORT'
      it_market_data  = lt_market_data ).

    " Verify result
    cl_abap_unit_assert=>assert_not_initial(
      act = ls_result-var_amount
      msg = 'VaR amount should not be zero' ).

    cl_abap_unit_assert=>assert_equals(
      exp = 'HIST'
      act = ls_result-method
      msg = 'Wrong calculation method' ).

    cl_abap_unit_assert=>assert_equals(
      exp = '0.95'
      act = ls_result-confidence_level
      msg = 'Wrong confidence level' ).
  ENDMETHOD.

  METHOD test_calculate_percentile.
    DATA: lt_values TYPE STANDARD TABLE OF p LENGTH 15 DECIMALS 2.

    " Prepare test data
    lt_values = VALUE #( ( 10 ) ( 20 ) ( 30 ) ( 40 ) ( 50 ) ).

    " Test 95th percentile
    DATA(lv_result) = mo_calculator->calculate_percentile(
      it_values = lt_values
      iv_percentile = '0.95' ).

    " For 5 values, 95th percentile should be the first value (lowest)
    cl_abap_unit_assert=>assert_equals(
      exp = 10
      act = lv_result
      msg = 'Wrong 95th percentile calculation' ).

    " Test empty table
    TRY.
      mo_calculator->calculate_percentile(
        it_values = VALUE #( )
        iv_percentile = '0.95' ).
      cl_abap_unit_assert=>fail( 'Exception expected for empty values' ).
    CATCH cx_static_check INTO DATA(lx_error).
      cl_abap_unit_assert=>assert_bound( lx_error ).
    ENDTRY.
  ENDMETHOD.

  METHOD test_var_with_empty_data.
    TRY.
      mo_calculator->zif_trm_var_calculator~calculate_var(
        iv_portfolio_id = 'TEST_PORT'
        it_market_data  = VALUE #( ) ).
      cl_abap_unit_assert=>fail( 'Exception expected for empty market data' ).
    CATCH cx_static_check INTO DATA(lx_error).
      cl_abap_unit_assert=>assert_bound( lx_error ).
    ENDTRY.
  ENDMETHOD.

  METHOD test_var_single_instrument.
    DATA: lt_market_data TYPE zif_trm_var_calculator=>tt_market_data.

    " Prepare test data with consistent 5% daily losses
    lt_market_data = VALUE #(
      ( instrument_id = 'STOCK1' price_date = '20240101' price_value = 100.00 currency = 'USD' )
      ( instrument_id = 'STOCK1' price_date = '20240102' price_value = 95.00  currency = 'USD' )
      ( instrument_id = 'STOCK1' price_date = '20240103' price_value = 90.25  currency = 'USD' )
      ( instrument_id = 'STOCK1' price_date = '20240104' price_value = 85.74  currency = 'USD' )
      ( instrument_id = 'STOCK1' price_date = '20240105' price_value = 81.45  currency = 'USD' ) ).

    " Calculate VaR
    DATA(ls_result) = mo_calculator->zif_trm_var_calculator~calculate_var(
      iv_portfolio_id = 'TEST_PORT'
      it_market_data  = lt_market_data ).

    " VaR should be approximately 5% of current value
    cl_abap_unit_assert=>assert_number_between(
      lower  = 3.5  " Allow some variation due to log returns
      upper  = 4.5
      number = ls_result-var_amount
      msg    = 'VaR amount outside expected range' ).
  ENDMETHOD.

  METHOD test_var_multiple_instruments.
    DATA: lt_market_data TYPE zif_trm_var_calculator=>tt_market_data.

    " Prepare test data with two instruments
    lt_market_data = VALUE #(
      " First instrument with upward trend
      ( instrument_id = 'STOCK1' price_date = '20240101' price_value = 100.00 currency = 'USD' )
      ( instrument_id = 'STOCK1' price_date = '20240102' price_value = 102.00 currency = 'USD' )
      ( instrument_id = 'STOCK1' price_date = '20240103' price_value = 104.04 currency = 'USD' )
      " Second instrument with downward trend
      ( instrument_id = 'STOCK2' price_date = '20240101' price_value = 50.00  currency = 'USD' )
      ( instrument_id = 'STOCK2' price_date = '20240102' price_value = 47.50  currency = 'USD' )
      ( instrument_id = 'STOCK2' price_date = '20240103' price_value = 45.13  currency = 'USD' ) ).

    " Calculate VaR
    DATA(ls_result) = mo_calculator->zif_trm_var_calculator~calculate_var(
      iv_portfolio_id = 'TEST_PORT'
      it_market_data  = lt_market_data ).

    " Verify result
    cl_abap_unit_assert=>assert_not_initial(
      act = ls_result-var_amount
      msg = 'VaR amount should not be zero' ).

    " VaR should be less than the sum of individual VaRs due to diversification
    cl_abap_unit_assert=>assert_true(
      act = ls_result-var_amount < 10  " Combined loss should be less than worst case
      msg = 'Diversification effect not reflected in VaR' ).
  ENDMETHOD.

ENDCLASS.