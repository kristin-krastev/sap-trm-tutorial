CLASS ltcl_es_calculator DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA:
      mo_calculator TYPE REF TO zcl_trm_es_calculator.

    METHODS:
      setup,
      test_calculate_es FOR TESTING,
      test_tail_losses FOR TESTING,
      test_weighted_average FOR TESTING,
      test_es_vs_var FOR TESTING,
      test_es_with_empty_data FOR TESTING.
ENDCLASS.

CLASS ltcl_es_calculator IMPLEMENTATION.

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

  METHOD test_calculate_es.
    DATA: lt_market_data TYPE zif_trm_var_calculator=>tt_market_data.

    " Prepare test data with known loss pattern
    lt_market_data = VALUE #(
      ( instrument_id = 'STOCK1' price_date = '20240101' price_value = 100 currency = 'USD' )
      ( instrument_id = 'STOCK1' price_date = '20240102' price_value = 95  currency = 'USD' )
      ( instrument_id = 'STOCK1' price_date = '20240103' price_value = 90  currency = 'USD' )
      ( instrument_id = 'STOCK1' price_date = '20240104' price_value = 92  currency = 'USD' )
      ( instrument_id = 'STOCK1' price_date = '20240105' price_value = 88  currency = 'USD' ) ).

    " Calculate ES
    DATA(ls_result) = mo_calculator->calculate_expected_shortfall(
      iv_portfolio_id = 'TEST_PORT'
      it_market_data  = lt_market_data ).

    " Verify result
    cl_abap_unit_assert=>assert_not_initial(
      act = ls_result-es_amount
      msg = 'ES amount should not be zero' ).

    cl_abap_unit_assert=>assert_true(
      act = ls_result-es_amount >= ls_result-var_amount
      msg = 'ES should be greater than or equal to VaR' ).
  ENDMETHOD.

  METHOD test_tail_losses.
    DATA: lt_returns TYPE tt_returns.

    " Prepare test return data
    lt_returns = VALUE #(
      ( instrument_id = 'STOCK1' return_date = '20240101' return_value = -0.05 )
      ( instrument_id = 'STOCK1' return_date = '20240102' return_value = -0.03 )
      ( instrument_id = 'STOCK1' return_date = '20240103' return_value = -0.02 )
      ( instrument_id = 'STOCK1' return_date = '20240104' return_value = 0.01 ) ).

    " Calculate tail losses with -0.03 threshold
    DATA(lt_tail_losses) = mo_calculator->calculate_tail_losses(
      it_returns = lt_returns
      iv_var_threshold = '-0.03' ).

    " Verify number of tail losses
    cl_abap_unit_assert=>assert_equals(
      exp = 1
      act = lines( lt_tail_losses )
      msg = 'Wrong number of tail losses identified' ).

    " Verify weights sum to 1
    DATA(lv_total_weight) = REDUCE f( INIT sum = 0
                                     FOR ls_loss IN lt_tail_losses
                                     NEXT sum = sum + ls_loss-weight ).

    cl_abap_unit_assert=>assert_equals(
      exp = 1
      act = lv_total_weight
      msg = 'Tail loss weights do not sum to 1' ).
  ENDMETHOD.

  METHOD test_weighted_average.
    DATA: lt_losses TYPE tt_tail_losses.

    " Prepare test loss data with equal weights
    lt_losses = VALUE #(
      ( return_date = '20240101' loss_value = 5 weight = '0.5' )
      ( return_date = '20240102' loss_value = 3 weight = '0.5' ) ).

    " Calculate weighted average
    DATA(lv_es) = mo_calculator->calculate_weighted_average( lt_losses ).

    " Verify result (should be 4 = 5*0.5 + 3*0.5)
    cl_abap_unit_assert=>assert_equals(
      exp = 4
      act = lv_es
      msg = 'Wrong weighted average calculation' ).
  ENDMETHOD.

  METHOD test_es_vs_var.
    DATA: lt_market_data TYPE zif_trm_var_calculator=>tt_market_data.

    " Prepare test data with severe tail events
    lt_market_data = VALUE #(
      ( instrument_id = 'STOCK1' price_date = '20240101' price_value = 100 currency = 'USD' )
      ( instrument_id = 'STOCK1' price_date = '20240102' price_value = 98  currency = 'USD' )
      ( instrument_id = 'STOCK1' price_date = '20240103' price_value = 95  currency = 'USD' )
      ( instrument_id = 'STOCK1' price_date = '20240104' price_value = 85  currency = 'USD' )  " Severe loss
      ( instrument_id = 'STOCK1' price_date = '20240105' price_value = 80  currency = 'USD' ) ).  " Another severe loss

    " Calculate ES
    DATA(ls_result) = mo_calculator->calculate_expected_shortfall(
      iv_portfolio_id = 'TEST_PORT'
      it_market_data  = lt_market_data ).

    " ES should be significantly higher than VaR due to severe tail events
    cl_abap_unit_assert=>assert_true(
      act = ls_result-es_amount >= ls_result-var_amount * '1.2'
      msg = 'ES not sufficiently higher than VaR for severe tail events' ).
  ENDMETHOD.

  METHOD test_es_with_empty_data.
    TRY.
      mo_calculator->calculate_expected_shortfall(
        iv_portfolio_id = 'TEST_PORT'
        it_market_data  = VALUE #( ) ).
      cl_abap_unit_assert=>fail( 'Exception expected for empty market data' ).
    CATCH cx_static_check INTO DATA(lx_error).
      cl_abap_unit_assert=>assert_bound( lx_error ).
    ENDTRY.
  ENDMETHOD.

ENDCLASS.