INTERFACE zif_trm_var_calculator
  PUBLIC.

  TYPES:
    BEGIN OF ty_var_config,
      config_id         TYPE sysuuid_x16,
      confidence_level TYPE ztrm_confidence_level,
      time_horizon     TYPE int4,
      historical_window TYPE int4,
      calculation_method TYPE ztrm_var_method,
      base_currency    TYPE waers,
    END OF ty_var_config,

    BEGIN OF ty_var_result,
      result_id        TYPE sysuuid_x16,
      config_id        TYPE sysuuid_x16,
      calculation_date TYPE d,
      portfolio_id     TYPE string,
      var_amount      TYPE p LENGTH 15 DECIMALS 2,
      currency        TYPE waers,
      confidence_level TYPE ztrm_confidence_level,
      time_horizon    TYPE int4,
      method          TYPE ztrm_var_method,
    END OF ty_var_result,

    BEGIN OF ty_market_data,
      instrument_id   TYPE string,
      price_date     TYPE d,
      price_value    TYPE p LENGTH 15 DECIMALS 2,
      currency       TYPE waers,
    END OF ty_market_data,

    tt_market_data TYPE STANDARD TABLE OF ty_market_data WITH KEY instrument_id price_date.

  METHODS:
    initialize
      IMPORTING
        is_config TYPE ty_var_config
      RAISING
        cx_static_check,

    calculate_var
      IMPORTING
        iv_portfolio_id TYPE string
        it_market_data  TYPE tt_market_data
      RETURNING
        VALUE(rs_result) TYPE ty_var_result
      RAISING
        cx_static_check,

    validate_input
      IMPORTING
        is_config      TYPE ty_var_config
        it_market_data TYPE tt_market_data
      RAISING
        cx_static_check.

ENDINTERFACE.