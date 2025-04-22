CLASS ltcl_sensitivity_calc DEFINITION FINAL
  FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA:
      mo_sensitivity_calc TYPE REF TO zcl_trm_sensitivity_calc,
      mt_positions       TYPE ztrm_tt_positions,
      mt_market_data    TYPE ztrm_tt_market_data.

    METHODS:
      setup,
      test_fx_delta FOR TESTING,
      test_ir_delta FOR TESTING,
      test_equity_gamma FOR TESTING,
      test_option_vega FOR TESTING,
      test_multiple_factors FOR TESTING.

ENDCLASS.

CLASS ltcl_sensitivity_calc IMPLEMENTATION.

  METHOD setup.
    " Create calculator instance
    mo_sensitivity_calc = NEW #(
      iv_confidence_level = '0.95'
      iv_time_horizon    = 10 ).

    " Setup test positions
    mt_positions = VALUE #(
      " FX Forward position
      ( position_id     = '1'
        instrument_type = 'FORWARD'
        risk_type      = 'FX_RATE'
        nominal_amount = 1000000
        maturity_date  = sy-datum + 90 )  " 90 days forward

      " Interest Rate Swap position
      ( position_id     = '2'
        instrument_type = 'SWAP'
        risk_type      = 'INTEREST_RATE'
        nominal_amount = 5000000
        maturity_date  = sy-datum + 365 )  " 1 year swap

      " Equity Option position
      ( position_id     = '3'
        instrument_type = 'OPTION'
        risk_type      = 'EQUITY_PRICE'
        nominal_amount = 100000
        strike_price   = 100
        maturity_date  = sy-datum + 30 ) ). " 30 days option

    " Setup test market data
    mt_market_data = VALUE #(
      " FX Rate
      ( factor_type    = 'FX_RATE'
        market_price   = '1.1000'    " EUR/USD rate
        volatility     = '0.10'      " 10% volatility
        interest_rate  = '0.02' )    " 2% interest rate

      " Interest Rate
      ( factor_type    = 'INTEREST_RATE'
        market_price   = '1.00'
        interest_rate  = '0.03' )    " 3% interest rate

      " Equity Price
      ( factor_type    = 'EQUITY_PRICE'
        market_price   = '105.00'    " Current stock price
        volatility     = '0.20'      " 20% volatility
        interest_rate  = '0.02' ) ). " 2% risk-free rate
  ENDMETHOD.

  METHOD test_fx_delta.
    " Given
    DATA(ls_fx_position) = mt_positions[ position_id = '1' ].
    DATA(ls_fx_market_data) = mt_market_data[ factor_type = 'FX_RATE' ].

    " When
    DATA(lv_delta) = mo_sensitivity_calc->calculate_delta(
      is_position    = ls_fx_position
      is_market_data = ls_fx_market_data ).

    " Then
    cl_abap_unit_assert=>assert_not_initial(
      act = lv_delta
      msg = 'FX Delta should not be zero' ).

    " Delta should be approximately the nominal amount for a forward
    cl_abap_unit_assert=>assert_number_between(
      lower  = 900000   " Allow for some numerical precision differences
      upper  = 1100000
      number = lv_delta
      msg    = 'FX Forward delta should be close to nominal amount' ).
  ENDMETHOD.

  METHOD test_ir_delta.
    " Given
    DATA(ls_ir_position) = mt_positions[ position_id = '2' ].
    DATA(ls_ir_market_data) = mt_market_data[ factor_type = 'INTEREST_RATE' ].

    " When
    DATA(lv_delta) = mo_sensitivity_calc->calculate_delta(
      is_position    = ls_ir_position
      is_market_data = ls_ir_market_data ).

    " Then
    cl_abap_unit_assert=>assert_not_initial(
      act = lv_delta
      msg = 'Interest Rate Delta should not be zero' ).

    " For a swap, delta should reflect the present value of basis point
    cl_abap_unit_assert=>assert_number_between(
      lower  = -100000  " Typical PVBP range for 5M notional
      upper  = 100000
      number = lv_delta
      msg    = 'Interest Rate Swap delta should be in reasonable range' ).
  ENDMETHOD.

  METHOD test_equity_gamma.
    " Given
    DATA(ls_equity_position) = mt_positions[ position_id = '3' ].
    DATA(ls_equity_market_data) = mt_market_data[ factor_type = 'EQUITY_PRICE' ].

    " When
    DATA(lv_gamma) = mo_sensitivity_calc->calculate_gamma(
      is_position    = ls_equity_position
      is_market_data = ls_equity_market_data ).

    " Then
    cl_abap_unit_assert=>assert_not_initial(
      act = lv_gamma
      msg = 'Equity Option Gamma should not be zero' ).

    " Gamma should be positive for a vanilla option
    cl_abap_unit_assert=>assert_true(
      act = lv_gamma > 0
      msg = 'Equity Option gamma should be positive' ).
  ENDMETHOD.

  METHOD test_option_vega.
    " Given
    DATA(ls_option_position) = mt_positions[ position_id = '3' ].
    DATA(ls_equity_market_data) = mt_market_data[ factor_type = 'EQUITY_PRICE' ].

    " When
    DATA(lv_vega) = mo_sensitivity_calc->calculate_vega(
      is_position    = ls_option_position
      is_market_data = ls_equity_market_data ).

    " Then
    cl_abap_unit_assert=>assert_not_initial(
      act = lv_vega
      msg = 'Option Vega should not be zero' ).

    " Vega should be positive for a vanilla option
    cl_abap_unit_assert=>assert_true(
      act = lv_vega > 0
      msg = 'Option vega should be positive' ).
  ENDMETHOD.

  METHOD test_multiple_factors.
    " When
    DATA(lt_sensitivities) = mo_sensitivity_calc->calculate_sensitivities(
      it_positions    = mt_positions
      it_market_data = mt_market_data ).

    " Then
    " Should have sensitivity entries for each position-risk factor combination
    cl_abap_unit_assert=>assert_equals(
      exp = 3  " One for each test position
      act = lines( lt_sensitivities )
      msg = 'Should calculate sensitivities for all positions' ).

    " Check that each sensitivity has valid values
    LOOP AT lt_sensitivities ASSIGNING FIELD-SYMBOL(<ls_sensitivity>).
      " Delta should always be calculated
      cl_abap_unit_assert=>assert_not_initial(
        act = <ls_sensitivity>-delta
        msg = |Delta missing for factor { <ls_sensitivity>-factor_type }| ).

      " Gamma should be calculated for non-linear instruments
      IF <ls_sensitivity>-factor_type = 'EQUITY_PRICE'.
        cl_abap_unit_assert=>assert_not_initial(
          act = <ls_sensitivity>-gamma
          msg = 'Gamma missing for equity option' ).
      ENDIF.

      " Vega should only be non-zero for options
      IF <ls_sensitivity>-factor_type = 'EQUITY_PRICE'.
        cl_abap_unit_assert=>assert_not_initial(
          act = <ls_sensitivity>-vega
          msg = 'Vega missing for equity option' ).
      ELSE.
        cl_abap_unit_assert=>assert_equals(
          exp = 0
          act = <ls_sensitivity>-vega
          msg = |Vega should be zero for { <ls_sensitivity>-factor_type }| ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.