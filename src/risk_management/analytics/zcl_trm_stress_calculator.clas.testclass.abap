CLASS ltcl_stress_calculator DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA:
      mo_calculator TYPE REF TO zcl_trm_stress_calculator.

    METHODS:
      setup,
      test_add_scenario FOR TESTING,
      test_apply_absolute_shock FOR TESTING,
      test_apply_relative_shock FOR TESTING,
      test_multiple_scenarios FOR TESTING,
      test_empty_portfolio FOR TESTING,
      test_currency_conversion FOR TESTING.
ENDCLASS.

CLASS ltcl_stress_calculator IMPLEMENTATION.

  METHOD setup.
    " Create calculator instance with USD as base currency
    CREATE OBJECT mo_calculator
      EXPORTING
        iv_base_currency = 'USD'.
  ENDMETHOD.

  METHOD test_add_scenario.
    DATA: ls_scenario TYPE zcl_trm_stress_calculator=>ty_stress_scenario.

    " Test invalid scenario
    TRY.
      mo_calculator->add_scenario( ls_scenario ).
      cl_abap_unit_assert=>fail( 'Exception expected for invalid scenario' ).
    CATCH cx_static_check INTO DATA(lx_error).
      cl_abap_unit_assert=>assert_bound( lx_error ).
    ENDTRY.

    " Test valid scenario
    ls_scenario = VALUE #(
      scenario_id = cl_system_uuid=>create_uuid_x16_static( )
      scenario_name = 'Market Crash'
      market_shocks = VALUE #( (
        risk_factor_id = 'EQUITY'
        shock_type = 'RELATIVE'
        shock_value = -30
        currency = 'USD' ) ) ).

    TRY.
      mo_calculator->add_scenario( ls_scenario ).
    CATCH cx_static_check INTO lx_error.
      cl_abap_unit_assert=>fail( 'No exception expected for valid scenario' ).
    ENDTRY.
  ENDMETHOD.

  METHOD test_apply_absolute_shock.
    DATA: lt_positions TYPE tt_portfolio_positions,
          lt_shocks   TYPE tt_market_shocks.

    " Prepare test position
    lt_positions = VALUE #( (
      position_id = '1'
      instrument_id = 'BOND1'
      quantity = 1000
      market_value = 100
      currency = 'USD' ) ).

    " Prepare absolute shock (-10)
    lt_shocks = VALUE #( (
      risk_factor_id = 'BOND1'
      shock_type = 'ABSOLUTE'
      shock_value = -10
      currency = 'USD' ) ).

    " Apply shock
    DATA(lt_stressed) = mo_calculator->apply_market_shocks(
      it_positions = lt_positions
      it_market_shocks = lt_shocks ).

    " Verify result
    READ TABLE lt_stressed INDEX 1 INTO DATA(ls_result).
    cl_abap_unit_assert=>assert_equals(
      exp = 90  " 100 - 10
      act = ls_result-market_value
      msg = 'Wrong absolute shock application' ).
  ENDMETHOD.

  METHOD test_apply_relative_shock.
    DATA: lt_positions TYPE tt_portfolio_positions,
          lt_shocks   TYPE tt_market_shocks.

    " Prepare test position
    lt_positions = VALUE #( (
      position_id = '1'
      instrument_id = 'EQUITY1'
      quantity = 1000
      market_value = 100
      currency = 'USD' ) ).

    " Prepare relative shock (-20%)
    lt_shocks = VALUE #( (
      risk_factor_id = 'EQUITY1'
      shock_type = 'RELATIVE'
      shock_value = -20
      currency = 'USD' ) ).

    " Apply shock
    DATA(lt_stressed) = mo_calculator->apply_market_shocks(
      it_positions = lt_positions
      it_market_shocks = lt_shocks ).

    " Verify result
    READ TABLE lt_stressed INDEX 1 INTO DATA(ls_result).
    cl_abap_unit_assert=>assert_equals(
      exp = 80  " 100 * (1 - 0.2)
      act = ls_result-market_value
      msg = 'Wrong relative shock application' ).
  ENDMETHOD.

  METHOD test_multiple_scenarios.
    DATA: lt_positions TYPE tt_portfolio_positions,
          ls_scenario1 TYPE zcl_trm_stress_calculator=>ty_stress_scenario,
          ls_scenario2 TYPE zcl_trm_stress_calculator=>ty_stress_scenario.

    " Prepare test position
    lt_positions = VALUE #( (
      position_id = '1'
      instrument_id = 'EQUITY1'
      quantity = 1000
      market_value = 100
      currency = 'USD' ) ).

    " Prepare scenarios
    ls_scenario1 = VALUE #(
      scenario_id = cl_system_uuid=>create_uuid_x16_static( )
      scenario_name = 'Mild Stress'
      market_shocks = VALUE #( (
        risk_factor_id = 'EQUITY1'
        shock_type = 'RELATIVE'
        shock_value = -10
        currency = 'USD' ) ) ).

    ls_scenario2 = VALUE #(
      scenario_id = cl_system_uuid=>create_uuid_x16_static( )
      scenario_name = 'Severe Stress'
      market_shocks = VALUE #( (
        risk_factor_id = 'EQUITY1'
        shock_type = 'RELATIVE'
        shock_value = -30
        currency = 'USD' ) ) ).

    " Add scenarios
    mo_calculator->add_scenario( ls_scenario1 ).
    mo_calculator->add_scenario( ls_scenario2 ).

    " Run stress test
    DATA(lt_results) = mo_calculator->run_stress_test(
      iv_portfolio_id = 'TEST_PORT'
      it_positions = lt_positions ).

    " Verify results
    cl_abap_unit_assert=>assert_equals(
      exp = 2
      act = lines( lt_results )
      msg = 'Wrong number of stress test results' ).

    " Verify impacts
    LOOP AT lt_results INTO DATA(ls_result).
      CASE ls_result-impact_percent.
        WHEN -10.
          cl_abap_unit_assert=>assert_equals(
            exp = -10
            act = ls_result-impact_amount
            msg = 'Wrong impact for mild stress' ).
        WHEN -30.
          cl_abap_unit_assert=>assert_equals(
            exp = -30
            act = ls_result-impact_amount
            msg = 'Wrong impact for severe stress' ).
        WHEN OTHERS.
          cl_abap_unit_assert=>fail( 'Unexpected impact percentage' ).
      ENDCASE.
    ENDLOOP.
  ENDMETHOD.

  METHOD test_empty_portfolio.
    DATA: lt_positions TYPE tt_portfolio_positions,
          ls_scenario TYPE zcl_trm_stress_calculator=>ty_stress_scenario.

    " Prepare scenario
    ls_scenario = VALUE #(
      scenario_id = cl_system_uuid=>create_uuid_x16_static( )
      scenario_name = 'Test Scenario'
      market_shocks = VALUE #( (
        risk_factor_id = 'EQUITY1'
        shock_type = 'RELATIVE'
        shock_value = -10
        currency = 'USD' ) ) ).

    mo_calculator->add_scenario( ls_scenario ).

    " Run stress test with empty portfolio
    DATA(lt_results) = mo_calculator->run_stress_test(
      iv_portfolio_id = 'TEST_PORT'
      it_positions = lt_positions ).

    " Verify result
    READ TABLE lt_results INDEX 1 INTO DATA(ls_result).
    cl_abap_unit_assert=>assert_equals(
      exp = 0
      act = ls_result-base_value
      msg = 'Base value should be zero for empty portfolio' ).

    cl_abap_unit_assert=>assert_equals(
      exp = 0
      act = ls_result-impact_amount
      msg = 'Impact should be zero for empty portfolio' ).
  ENDMETHOD.

  METHOD test_currency_conversion.
    DATA: lt_positions TYPE tt_portfolio_positions,
          ls_scenario TYPE zcl_trm_stress_calculator=>ty_stress_scenario.

    " Prepare test positions in different currencies
    lt_positions = VALUE #(
      ( position_id = '1'
        instrument_id = 'BOND1'
        quantity = 1000
        market_value = 100
        currency = 'USD' )
      ( position_id = '2'
        instrument_id = 'BOND2'
        quantity = 1000
        market_value = 85
        currency = 'EUR' ) ).

    " Prepare scenario
    ls_scenario = VALUE #(
      scenario_id = cl_system_uuid=>create_uuid_x16_static( )
      scenario_name = 'Currency Stress'
      market_shocks = VALUE #( (
        risk_factor_id = 'BOND1'
        shock_type = 'RELATIVE'
        shock_value = -10
        currency = 'USD' ) ) ).

    mo_calculator->add_scenario( ls_scenario ).

    " Run stress test
    DATA(lt_results) = mo_calculator->run_stress_test(
      iv_portfolio_id = 'TEST_PORT'
      it_positions = lt_positions ).

    " Verify result
    READ TABLE lt_results INDEX 1 INTO DATA(ls_result).
    cl_abap_unit_assert=>assert_equals(
      exp = 'USD'
      act = ls_result-currency
      msg = 'Result should be in base currency (USD)' ).
  ENDMETHOD.

ENDCLASS.