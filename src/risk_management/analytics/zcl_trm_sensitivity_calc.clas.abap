CLASS zcl_trm_sensitivity_calc DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES:
      BEGIN OF ty_risk_factor,
        factor_id   TYPE sysuuid_x16,
        factor_type TYPE string,
        delta       TYPE f,
        gamma       TYPE f,
        vega        TYPE f,
      END OF ty_risk_factor,
      tt_risk_factors TYPE STANDARD TABLE OF ty_risk_factor WITH KEY factor_id.

    METHODS:
      constructor
        IMPORTING
          iv_confidence_level TYPE ztrm_confidence_level
          iv_time_horizon    TYPE i,

      calculate_sensitivities
        IMPORTING
          it_positions     TYPE ztrm_tt_positions
          it_market_data  TYPE ztrm_tt_market_data
        RETURNING
          VALUE(rt_sensitivities) TYPE tt_risk_factors
        RAISING
          cx_static_check,

      calculate_delta
        IMPORTING
          is_position    TYPE ztrm_s_position
          is_market_data TYPE ztrm_s_market_data
        RETURNING
          VALUE(rv_delta) TYPE f,

      calculate_gamma
        IMPORTING
          is_position    TYPE ztrm_s_position
          is_market_data TYPE ztrm_s_market_data
        RETURNING
          VALUE(rv_gamma) TYPE f,

      calculate_vega
        IMPORTING
          is_position    TYPE ztrm_s_position
          is_market_data TYPE ztrm_s_market_data
        RETURNING
          VALUE(rv_vega) TYPE f.

  PRIVATE SECTION.
    DATA:
      mv_confidence_level TYPE ztrm_confidence_level,
      mv_time_horizon    TYPE i.

    METHODS:
      get_risk_factor_shift
        IMPORTING
          iv_factor_type     TYPE string
        RETURNING
          VALUE(rv_shift) TYPE f,

      calculate_position_value
        IMPORTING
          is_position    TYPE ztrm_s_position
          is_market_data TYPE ztrm_s_market_data
        RETURNING
          VALUE(rv_value) TYPE f,

      apply_market_data_shift
        IMPORTING
          is_market_data     TYPE ztrm_s_market_data
          iv_shift          TYPE f
        RETURNING
          VALUE(rs_shifted) TYPE ztrm_s_market_data.

ENDCLASS.

CLASS zcl_trm_sensitivity_calc IMPLEMENTATION.

  METHOD constructor.
    mv_confidence_level = iv_confidence_level.
    mv_time_horizon = iv_time_horizon.
  ENDMETHOD.

  METHOD calculate_sensitivities.
    DATA: ls_sensitivity TYPE ty_risk_factor.

    LOOP AT it_positions ASSIGNING FIELD-SYMBOL(<ls_position>).
      LOOP AT it_market_data ASSIGNING FIELD-SYMBOL(<ls_market_data>)
        WHERE factor_type = <ls_position>-risk_type.

        " Calculate sensitivities for each risk factor
        ls_sensitivity = VALUE #(
          factor_id   = cl_system_uuid=>create_uuid_x16_static( )
          factor_type = <ls_market_data>-factor_type
          delta      = calculate_delta(
            is_position    = <ls_position>
            is_market_data = <ls_market_data> )
          gamma      = calculate_gamma(
            is_position    = <ls_position>
            is_market_data = <ls_market_data> )
          vega       = calculate_vega(
            is_position    = <ls_position>
            is_market_data = <ls_market_data> ) ).

        APPEND ls_sensitivity TO rt_sensitivities.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.

  METHOD calculate_delta.
    DATA(lv_shift) = get_risk_factor_shift( is_market_data-factor_type ).

    " Calculate base value
    DATA(lv_base_value) = calculate_position_value(
      is_position    = is_position
      is_market_data = is_market_data ).

    " Calculate shifted value
    DATA(ls_shifted_data) = apply_market_data_shift(
      is_market_data = is_market_data
      iv_shift      = lv_shift ).

    DATA(lv_shifted_value) = calculate_position_value(
      is_position    = is_position
      is_market_data = ls_shifted_data ).

    " Calculate delta as first derivative
    rv_delta = ( lv_shifted_value - lv_base_value ) / lv_shift.
  ENDMETHOD.

  METHOD calculate_gamma.
    DATA(lv_shift) = get_risk_factor_shift( is_market_data-factor_type ).

    " Calculate values at -shift, base, and +shift
    DATA(ls_down_shift) = apply_market_data_shift(
      is_market_data = is_market_data
      iv_shift      = -lv_shift ).

    DATA(lv_down_value) = calculate_position_value(
      is_position    = is_position
      is_market_data = ls_down_shift ).

    DATA(lv_base_value) = calculate_position_value(
      is_position    = is_position
      is_market_data = is_market_data ).

    DATA(ls_up_shift) = apply_market_data_shift(
      is_market_data = is_market_data
      iv_shift      = lv_shift ).

    DATA(lv_up_value) = calculate_position_value(
      is_position    = is_position
      is_market_data = ls_up_shift ).

    " Calculate gamma as second derivative
    rv_gamma = ( lv_up_value - 2 * lv_base_value + lv_down_value ) /
              ( lv_shift * lv_shift ).
  ENDMETHOD.

  METHOD calculate_vega.
    " Only calculate vega for options
    IF is_position-instrument_type <> 'OPTION'.
      rv_vega = 0.
      RETURN.
    ENDIF.

    DATA(lv_vol_shift) = '0.01'.  " 1% volatility shift

    " Calculate base value
    DATA(lv_base_value) = calculate_position_value(
      is_position    = is_position
      is_market_data = is_market_data ).

    " Calculate value with shifted volatility
    DATA(ls_shifted_data) = is_market_data.
    ls_shifted_data-volatility = ls_shifted_data-volatility + lv_vol_shift.

    DATA(lv_shifted_value) = calculate_position_value(
      is_position    = is_position
      is_market_data = ls_shifted_data ).

    " Calculate vega as sensitivity to volatility
    rv_vega = ( lv_shifted_value - lv_base_value ) / lv_vol_shift.
  ENDMETHOD.

  METHOD get_risk_factor_shift.
    " Define standard shifts for different risk factors
    CASE iv_factor_type.
      WHEN 'FX_RATE'.
        rv_shift = '0.0001'.  " 1 pip for FX
      WHEN 'INTEREST_RATE'.
        rv_shift = '0.0001'.  " 1 basis point for rates
      WHEN 'EQUITY_PRICE'.
        rv_shift = '0.01'.    " 1% for equity prices
      WHEN 'COMMODITY_PRICE'.
        rv_shift = '0.01'.    " 1% for commodity prices
      WHEN OTHERS.
        rv_shift = '0.01'.    " Default 1% shift
    ENDCASE.
  ENDMETHOD.

  METHOD calculate_position_value.
    " Implement position valuation logic based on instrument type
    CASE is_position-instrument_type.
      WHEN 'SPOT'.
        rv_value = is_position-nominal_amount * is_market_data-market_price.
      WHEN 'FORWARD'.
        " Simple forward pricing
        DATA(lv_time_to_maturity) = CONV i(
          is_position-maturity_date - sy-datum ).
        rv_value = is_position-nominal_amount *
                  ( is_market_data-market_price *
                    exp( is_market_data-interest_rate *
                         lv_time_to_maturity / 365 ) ).
      WHEN 'OPTION'.
        " Black-Scholes pricing for options
        " Note: This is a simplified implementation
        rv_value = zcl_trm_option_pricer=>calculate_price(
          is_position    = is_position
          is_market_data = is_market_data ).
      WHEN OTHERS.
        rv_value = 0.
    ENDCASE.
  ENDMETHOD.

  METHOD apply_market_data_shift.
    rs_shifted = is_market_data.

    CASE is_market_data-factor_type.
      WHEN 'FX_RATE' OR 'EQUITY_PRICE' OR 'COMMODITY_PRICE'.
        rs_shifted-market_price = is_market_data-market_price *
                                ( 1 + iv_shift ).
      WHEN 'INTEREST_RATE'.
        rs_shifted-interest_rate = is_market_data-interest_rate + iv_shift.
      WHEN OTHERS.
        " Apply multiplicative shift by default
        rs_shifted-market_price = is_market_data-market_price *
                                ( 1 + iv_shift ).
    ENDCASE.
  ENDMETHOD.

ENDCLASS.