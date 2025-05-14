CLASS zcl_trm_stress_calculator DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES:
      BEGIN OF ty_stress_scenario,
        scenario_id    TYPE sysuuid_x16,
        scenario_name  TYPE string,
        market_shocks TYPE tt_market_shocks,
      END OF ty_stress_scenario,

      BEGIN OF ty_market_shock,
        risk_factor_id   TYPE string,
        shock_type      TYPE string,  " ABSOLUTE or RELATIVE
        shock_value     TYPE p LENGTH 15 DECIMALS 2,
        currency        TYPE waers,
      END OF ty_market_shock,
      tt_market_shocks TYPE STANDARD TABLE OF ty_market_shock WITH KEY risk_factor_id,

      BEGIN OF ty_stress_result,
        result_id       TYPE sysuuid_x16,
        scenario_id     TYPE sysuuid_x16,
        portfolio_id    TYPE string,
        calc_date      TYPE d,
        base_value     TYPE p LENGTH 15 DECIMALS 2,
        stressed_value TYPE p LENGTH 15 DECIMALS 2,
        impact_amount  TYPE p LENGTH 15 DECIMALS 2,
        impact_percent TYPE p LENGTH 15 DECIMALS 2,
        currency       TYPE waers,
      END OF ty_stress_result,
      tt_stress_results TYPE STANDARD TABLE OF ty_stress_result WITH KEY result_id.

    METHODS:
      constructor
        IMPORTING
          iv_base_currency TYPE waers DEFAULT 'USD',

      add_scenario
        IMPORTING
          is_scenario TYPE ty_stress_scenario
        RAISING
          cx_static_check,

      run_stress_test
        IMPORTING
          iv_portfolio_id TYPE string
          it_positions    TYPE tt_portfolio_positions
        RETURNING
          VALUE(rt_results) TYPE tt_stress_results
        RAISING
          cx_static_check.

  PROTECTED SECTION.
    METHODS:
      apply_market_shocks
        IMPORTING
          it_positions     TYPE tt_portfolio_positions
          it_market_shocks TYPE tt_market_shocks
        RETURNING
          VALUE(rt_stressed_positions) TYPE tt_portfolio_positions,

      calculate_portfolio_value
        IMPORTING
          it_positions      TYPE tt_portfolio_positions
        RETURNING
          VALUE(rv_value)   TYPE p LENGTH 15 DECIMALS 2
        RAISING
          cx_static_check.

  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_portfolio_position,
        position_id    TYPE string,
        instrument_id  TYPE string,
        quantity      TYPE p LENGTH 15 DECIMALS 2,
        market_value  TYPE p LENGTH 15 DECIMALS 2,
        currency      TYPE waers,
      END OF ty_portfolio_position,
      tt_portfolio_positions TYPE STANDARD TABLE OF ty_portfolio_position WITH KEY position_id.

    DATA:
      mt_scenarios     TYPE STANDARD TABLE OF ty_stress_scenario,
      mv_base_currency TYPE waers.

ENDCLASS.

CLASS zcl_trm_stress_calculator IMPLEMENTATION.

  METHOD constructor.
    mv_base_currency = iv_base_currency.
  ENDMETHOD.

  METHOD add_scenario.
    " Validate scenario
    IF is_scenario-scenario_id IS INITIAL OR
       is_scenario-scenario_name IS INITIAL OR
       is_scenario-market_shocks IS INITIAL.
      RAISE EXCEPTION TYPE cx_static_check
        EXPORTING
          textid = cx_static_check=>general_error
          text   = 'Invalid stress scenario definition'.
    ENDIF.

    " Add to scenarios table
    APPEND is_scenario TO mt_scenarios.
  ENDMETHOD.

  METHOD run_stress_test.
    DATA: ls_result TYPE ty_stress_result.

    " Calculate base portfolio value
    DATA(lv_base_value) = calculate_portfolio_value( it_positions ).

    " Run each scenario
    LOOP AT mt_scenarios INTO DATA(ls_scenario).
      " Apply market shocks
      DATA(lt_stressed_positions) = apply_market_shocks(
        it_positions     = it_positions
        it_market_shocks = ls_scenario-market_shocks ).

      " Calculate stressed portfolio value
      DATA(lv_stressed_value) = calculate_portfolio_value( lt_stressed_positions ).

      " Calculate impact
      DATA(lv_impact_amount) = lv_stressed_value - lv_base_value.
      DATA(lv_impact_percent) = COND #( WHEN lv_base_value <> 0
                                       THEN lv_impact_amount / lv_base_value * 100
                                       ELSE 0 ).

      " Prepare result
      APPEND VALUE #(
        result_id       = cl_system_uuid=>create_uuid_x16_static( )
        scenario_id     = ls_scenario-scenario_id
        portfolio_id    = iv_portfolio_id
        calc_date      = sy-datum
        base_value     = lv_base_value
        stressed_value = lv_stressed_value
        impact_amount  = lv_impact_amount
        impact_percent = lv_impact_percent
        currency       = mv_base_currency
      ) TO rt_results.
    ENDLOOP.
  ENDMETHOD.

  METHOD apply_market_shocks.
    rt_stressed_positions = it_positions.

    " Apply shocks to each position
    LOOP AT rt_stressed_positions ASSIGNING FIELD-SYMBOL(<ls_position>).
      " Find applicable shocks
      LOOP AT it_market_shocks INTO DATA(ls_shock)
        WHERE risk_factor_id = <ls_position>-instrument_id.

        CASE ls_shock-shock_type.
          WHEN 'ABSOLUTE'.
            <ls_position>-market_value = <ls_position>-market_value + ls_shock-shock_value.
          WHEN 'RELATIVE'.
            <ls_position>-market_value = <ls_position>-market_value *
              ( 1 + ls_shock-shock_value / 100 ).
        ENDCASE.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.

  METHOD calculate_portfolio_value.
    " Sum up position values in base currency
    LOOP AT it_positions INTO DATA(ls_position).
      IF ls_position-currency = mv_base_currency.
        rv_value = rv_value + ls_position-market_value.
      ELSE.
        " Convert to base currency
        " TODO: Implement proper currency conversion
        rv_value = rv_value + ls_position-market_value.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.