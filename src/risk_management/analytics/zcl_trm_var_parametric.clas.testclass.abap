CLASS ltcl_var_parametric DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA:
      mo_calculator TYPE REF TO zcl_trm_var_parametric.

    METHODS:
      setup,
      test_calculate_var FOR TESTING,
      test_covariance_matrix FOR TESTING,
      test_normal_var FOR TESTING,
      test_portfolio_weights FOR TESTING,
      test_correlation_matrix FOR TESTING.
ENDCLASS.

CLASS ltcl_var_parametric IMPLEMENTATION.

  METHOD setup.
    " Create calculator instance
    CREATE OBJECT mo_calculator.

    " Initialize with test configuration
    DATA(ls_config) = VALUE zif_trm_var_calculator=>ty_var_config(
      config_id = cl_system_uuid=>create_uuid_x16_static( )
      confidence_level = '0.95'
      time_horizon = 10
      historical_window = 250
      calculation_method = 'PARAM'
      base_currency = 'USD' ).

    mo_calculator->zif_trm_var_calculator~initialize( ls_config ).
  ENDMETHOD.

  METHOD test_calculate_var.
    DATA: lt_market_data TYPE zif_trm_var_calculator=>tt_market_data.

    " Prepare test data with known volatility pattern
    lt_market_data = VALUE #(
      ( instrument_id = 'STOCK1' price_date = '20240101' price_value = 100 currency = 'USD' )
      ( instrument_id = 'STOCK1' price_date = '20240102' price_value = 102 currency = 'USD' )
      ( instrument_id = 'STOCK1' price_date = '20240103' price_value = 101 currency = 'USD' )
      ( instrument_id = 'STOCK1' price_date = '20240104' price_value = 103 currency = 'USD' )
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
      exp = 'PARAM'
      act = ls_result-method
      msg = 'Wrong calculation method' ).
  ENDMETHOD.

  METHOD test_covariance_matrix.
    DATA: lt_returns TYPE STANDARD TABLE OF ty_return.

    " Prepare test return data for two assets
    lt_returns = VALUE #(
      ( instrument_id = 'STOCK1' return_date = '20240101' return_value = '0.02' )
      ( instrument_id = 'STOCK1' return_date = '20240102' return_value = '-0.01' )
      ( instrument_id = 'STOCK2' return_date = '20240101' return_value = '-0.01' )
      ( instrument_id = 'STOCK2' return_date = '20240102' return_value = '0.02' ) ).

    " Calculate covariance matrix
    DATA(lt_covariance) = mo_calculator->calculate_covariance_matrix( lt_returns ).

    " Verify matrix dimensions
    cl_abap_unit_assert=>assert_equals(
      exp = 2
      act = lines( lt_covariance )
      msg = 'Wrong covariance matrix dimension' ).

    " Verify diagonal elements (variances) are positive
    READ TABLE lt_covariance INTO DATA(ls_row) INDEX 1.
    cl_abap_unit_assert=>assert_true(
      act = ls_row-variance > 0
      msg = 'Variance should be positive' ).
  ENDMETHOD.

  METHOD test_normal_var.
    " Test VaR calculation using normal distribution
    DATA(lv_confidence) = '0.95'.
    DATA(lv_volatility) = '0.15'.  " 15% annual volatility
    DATA(lv_position) = 1000000.   " $1M position

    DATA(lv_var) = mo_calculator->calculate_normal_var(
      iv_confidence_level = lv_confidence
      iv_volatility      = lv_volatility
      iv_position_value  = lv_position
      iv_time_horizon    = 10 ).

    " Verify VaR is reasonable (should be around 7.5% for 95% confidence)
    cl_abap_unit_assert=>assert_number_between(
      lower  = 70000   " 7% of position
      upper  = 80000   " 8% of position
      number = lv_var
      msg    = 'VaR outside expected range' ).
  ENDMETHOD.

  METHOD test_portfolio_weights.
    DATA: lt_positions TYPE STANDARD TABLE OF ty_position.

    " Prepare test position data
    lt_positions = VALUE #(
      ( instrument_id = 'STOCK1' position_value = 600000 )  " 60%
      ( instrument_id = 'STOCK2' position_value = 400000 ) ).  " 40%

    " Calculate weights
    DATA(lt_weights) = mo_calculator->calculate_portfolio_weights( lt_positions ).

    " Verify weights sum to 1
    DATA(lv_total) = REDUCE f( INIT sum = 0
                              FOR ls_weight IN lt_weights
                              NEXT sum = sum + ls_weight-weight ).

    cl_abap_unit_assert=>assert_equals(
      exp = 1
      act = lv_total
      msg = 'Portfolio weights do not sum to 1' ).
  ENDMETHOD.

  METHOD test_correlation_matrix.
    DATA: lt_returns TYPE STANDARD TABLE OF ty_return.

    " Prepare test return data for perfectly negatively correlated assets
    lt_returns = VALUE #(
      ( instrument_id = 'STOCK1' return_date = '20240101' return_value = '0.02' )
      ( instrument_id = 'STOCK1' return_date = '20240102' return_value = '-0.02' )
      ( instrument_id = 'STOCK2' return_date = '20240101' return_value = '-0.02' )
      ( instrument_id = 'STOCK2' return_date = '20240102' return_value = '0.02' ) ).

    " Calculate correlation matrix
    DATA(lt_correlation) = mo_calculator->calculate_correlation_matrix( lt_returns ).

    " Verify correlation coefficient is close to -1
    READ TABLE lt_correlation INTO DATA(ls_row) INDEX 1.
    cl_abap_unit_assert=>assert_number_between(
      lower  = '-1.0'
      upper  = '-0.9'
      number = ls_row-correlation
      msg    = 'Correlation coefficient outside expected range' ).
  ENDMETHOD.

ENDCLASS.