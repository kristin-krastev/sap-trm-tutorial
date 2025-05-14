CLASS zcl_trm_var_historical DEFINITION
  PUBLIC
  INHERITING FROM zcl_trm_var_calculator_base
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      zif_trm_var_calculator~calculate_var REDEFINITION.

  PROTECTED SECTION.
    METHODS:
      calculate_percentile
        IMPORTING
          it_values          TYPE STANDARD TABLE
          iv_percentile      TYPE f
        RETURNING
          VALUE(rv_result)   TYPE p LENGTH 15 DECIMALS 2
        RAISING
          cx_static_check.

  PRIVATE SECTION.
    TYPES:
      tt_amounts TYPE STANDARD TABLE OF p LENGTH 15 DECIMALS 2 WITH EMPTY KEY.

ENDCLASS.

CLASS zcl_trm_var_historical IMPLEMENTATION.

  METHOD zif_trm_var_calculator~calculate_var.
    " Check initialization
    check_initialization( ).

    " Calculate returns from historical data
    DATA(lt_returns) = calculate_returns( it_market_data ).

    " Calculate simulated portfolio values
    DATA: lt_simulated_values TYPE tt_amounts.

    " Get current portfolio value (assuming last price is current)
    DATA(lv_current_value) = 0.
    LOOP AT it_market_data ASSIGNING FIELD-SYMBOL(<ls_current>)
      GROUP BY <ls_current>-instrument_id.

      " Get the latest price for each instrument
      DATA(lt_prices) = VALUE zif_trm_var_calculator=>tt_market_data( ).
      LOOP AT GROUP <ls_current> ASSIGNING FIELD-SYMBOL(<ls_price>).
        APPEND <ls_price> TO lt_prices.
      ENDLOOP.
      SORT lt_prices BY price_date DESCENDING.
      READ TABLE lt_prices INDEX 1 ASSIGNING FIELD-SYMBOL(<ls_latest>).

      " Add to portfolio value in base currency
      lv_current_value = lv_current_value +
        convert_to_base_currency(
          iv_amount   = <ls_latest>-price_value
          iv_currency = <ls_latest>-currency ).
    ENDLOOP.

    " Apply returns to current value to get simulated values
    LOOP AT lt_returns ASSIGNING FIELD-SYMBOL(<ls_return>)
      GROUP BY <ls_return>-return_date.

      " Calculate simulated value for this date
      DATA(lv_simulated_value) = lv_current_value *
        exp( REDUCE f( INIT val = 0
                      FOR ret IN GROUP <ls_return>
                      NEXT val = val + ret-return_value ) ).

      " Store simulated value
      APPEND lv_simulated_value TO lt_simulated_values.
    ENDLOOP.

    " Sort simulated values ascending
    SORT lt_simulated_values.

    " Calculate VaR at confidence level
    DATA(lv_var_value) = calculate_percentile(
      it_values     = lt_simulated_values
      iv_percentile = CONV #( ms_config-confidence_level ) ).

    " Calculate VaR as difference from current value
    DATA(lv_var_amount) = lv_current_value - lv_var_value.

    " Prepare result
    rs_result = VALUE #(
      result_id        = cl_system_uuid=>create_uuid_x16_static( )
      config_id        = ms_config-config_id
      calculation_date = sy-datum
      portfolio_id     = iv_portfolio_id
      var_amount      = lv_var_amount
      currency        = ms_config-base_currency
      confidence_level = ms_config-confidence_level
      time_horizon    = ms_config-time_horizon
      method          = 'HIST' ).

  ENDMETHOD.

  METHOD calculate_percentile.
    " Check if values table is empty
    IF it_values IS INITIAL.
      RAISE EXCEPTION TYPE cx_static_check
        EXPORTING
          textid = cx_static_check=>general_error
          text   = 'No values provided for percentile calculation'.
    ENDIF.

    " Calculate position
    DATA(lv_count) = lines( it_values ).
    DATA(lv_position) = CONV i( round( val = ( 1 - iv_percentile ) * lv_count
                                     dec = 0 ) ).

    " Ensure position is within bounds
    IF lv_position < 1.
      lv_position = 1.
    ELSEIF lv_position > lv_count.
      lv_position = lv_count.
    ENDIF.

    " Get value at calculated position
    READ TABLE it_values INDEX lv_position INTO rv_result.
  ENDMETHOD.

ENDCLASS.