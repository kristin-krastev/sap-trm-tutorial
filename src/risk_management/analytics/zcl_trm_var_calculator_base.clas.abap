CLASS zcl_trm_var_calculator_base DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES: zif_trm_var_calculator.

    METHODS:
      constructor.

  PROTECTED SECTION.
    DATA:
      ms_config        TYPE zif_trm_var_calculator=>ty_var_config,
      mv_is_initialized TYPE abap_bool.

    METHODS:
      check_initialization
        RAISING
          cx_static_check,

      calculate_returns
        IMPORTING
          it_market_data        TYPE zif_trm_var_calculator=>tt_market_data
        RETURNING
          VALUE(rt_returns)     TYPE tt_returns
        RAISING
          cx_static_check,

      convert_to_base_currency
        IMPORTING
          iv_amount           TYPE p LENGTH 15 DECIMALS 2
          iv_currency        TYPE waers
        RETURNING
          VALUE(rv_amount)   TYPE p LENGTH 15 DECIMALS 2
        RAISING
          cx_static_check.

  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_return,
        instrument_id TYPE string,
        return_date  TYPE d,
        return_value TYPE p LENGTH 15 DECIMALS 6,
      END OF ty_return,
      tt_returns TYPE STANDARD TABLE OF ty_return WITH KEY instrument_id return_date.

ENDCLASS.

CLASS zcl_trm_var_calculator_base IMPLEMENTATION.

  METHOD constructor.
    mv_is_initialized = abap_false.
  ENDMETHOD.

  METHOD zif_trm_var_calculator~initialize.
    " Validate configuration
    validate_input(
      is_config      = is_config
      it_market_data = VALUE #( ) ).

    " Store configuration
    ms_config = is_config.
    mv_is_initialized = abap_true.
  ENDMETHOD.

  METHOD zif_trm_var_calculator~validate_input.
    " Check confidence level
    IF is_config-confidence_level NOT BETWEEN '0.90' AND '0.99'.
      RAISE EXCEPTION TYPE cx_static_check
        EXPORTING
          textid = cx_static_check=>general_error
          text   = 'Confidence level must be between 90% and 99%'.
    ENDIF.

    " Check time horizon
    IF is_config-time_horizon NOT BETWEEN 1 AND 250.
      RAISE EXCEPTION TYPE cx_static_check
        EXPORTING
          textid = cx_static_check=>general_error
          text   = 'Time horizon must be between 1 and 250 days'.
    ENDIF.

    " Check historical window
    IF is_config-historical_window NOT BETWEEN 100 AND 1000.
      RAISE EXCEPTION TYPE cx_static_check
        EXPORTING
          textid = cx_static_check=>general_error
          text   = 'Historical window must be between 100 and 1000 days'.
    ENDIF.

    " Check market data if provided
    IF it_market_data IS NOT INITIAL.
      " Check for required fields
      LOOP AT it_market_data ASSIGNING FIELD-SYMBOL(<ls_market_data>).
        IF <ls_market_data>-instrument_id IS INITIAL OR
           <ls_market_data>-price_date IS INITIAL OR
           <ls_market_data>-price_value IS INITIAL OR
           <ls_market_data>-currency IS INITIAL.
          RAISE EXCEPTION TYPE cx_static_check
            EXPORTING
              textid = cx_static_check=>general_error
              text   = 'Market data contains incomplete records'.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.

  METHOD check_initialization.
    IF mv_is_initialized = abap_false.
      RAISE EXCEPTION TYPE cx_static_check
        EXPORTING
          textid = cx_static_check=>general_error
          text   = 'Calculator not initialized. Call initialize() first'.
    ENDIF.
  ENDMETHOD.

  METHOD calculate_returns.
    DATA: lt_sorted_data TYPE SORTED TABLE OF zif_trm_var_calculator=>ty_market_data
                        WITH NON-UNIQUE KEY instrument_id price_date.

    " Sort market data by instrument and date
    lt_sorted_data = it_market_data.

    " Calculate returns for each instrument
    LOOP AT lt_sorted_data ASSIGNING FIELD-SYMBOL(<ls_current>)
      GROUP BY ( instrument_id = <ls_current>-instrument_id ).

      DATA(lv_prev_price) = 0.
      DATA(lv_prev_date) = '00000000'.

      LOOP AT GROUP <ls_current> ASSIGNING FIELD-SYMBOL(<ls_price>).
        IF lv_prev_price > 0.
          " Calculate logarithmic return
          DATA(lv_return) = log( <ls_price>-price_value / lv_prev_price ).

          " Store return
          APPEND VALUE #(
            instrument_id = <ls_price>-instrument_id
            return_date  = <ls_price>-price_date
            return_value = lv_return
          ) TO rt_returns.
        ENDIF.

        lv_prev_price = <ls_price>-price_value.
        lv_prev_date  = <ls_price>-price_date.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.

  METHOD convert_to_base_currency.
    " TODO: Implement currency conversion using SAP standard functions
    " For now, we assume same currency
    IF iv_currency = ms_config-base_currency.
      rv_amount = iv_amount.
    ELSE.
      " In real implementation, call currency conversion function
      " rv_amount = convert_currency( ... )
      rv_amount = iv_amount.
    ENDIF.
  ENDMETHOD.

  METHOD zif_trm_var_calculator~calculate_var.
    " This is an abstract method - must be implemented by concrete classes
    RAISE EXCEPTION TYPE cx_static_check
      EXPORTING
        textid = cx_static_check=>general_error
        text   = 'Method must be implemented by concrete calculation class'.
  ENDMETHOD.

ENDCLASS.