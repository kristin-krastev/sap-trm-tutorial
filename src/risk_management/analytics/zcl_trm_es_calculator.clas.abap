CLASS zcl_trm_es_calculator DEFINITION
  PUBLIC
  INHERITING FROM zcl_trm_var_calculator_base
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      calculate_expected_shortfall
        IMPORTING
          iv_portfolio_id TYPE string
          it_market_data  TYPE zif_trm_var_calculator=>tt_market_data
        RETURNING
          VALUE(rs_result) TYPE ty_es_result
        RAISING
          cx_static_check.

  PROTECTED SECTION.
    METHODS:
      calculate_tail_losses
        IMPORTING
          it_returns           TYPE tt_returns
          iv_var_threshold     TYPE f
        RETURNING
          VALUE(rt_tail_losses) TYPE tt_tail_losses,

      calculate_weighted_average
        IMPORTING
          it_losses          TYPE tt_tail_losses
        RETURNING
          VALUE(rv_es_value) TYPE f.

  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_es_result,
        result_id        TYPE sysuuid_x16,
        config_id        TYPE sysuuid_x16,
        calculation_date TYPE d,
        portfolio_id     TYPE string,
        es_amount       TYPE p LENGTH 15 DECIMALS 2,
        var_amount      TYPE p LENGTH 15 DECIMALS 2,
        currency        TYPE waers,
        confidence_level TYPE ztrm_confidence_level,
        time_horizon    TYPE int4,
        method          TYPE ztrm_var_method,
      END OF ty_es_result,

      BEGIN OF ty_tail_loss,
        return_date TYPE d,
        loss_value TYPE f,
        weight     TYPE f,
      END OF ty_tail_loss,
      tt_tail_losses TYPE STANDARD TABLE OF ty_tail_loss WITH KEY return_date.

ENDCLASS.

CLASS zcl_trm_es_calculator IMPLEMENTATION.

  METHOD calculate_expected_shortfall.
    " First calculate VaR as the threshold
    DATA(ls_var_result) = zif_trm_var_calculator~calculate_var(
      iv_portfolio_id = iv_portfolio_id
      it_market_data  = it_market_data ).

    " Calculate returns from market data
    DATA(lt_returns) = calculate_returns( it_market_data ).

    " Identify losses beyond VaR threshold
    DATA(lt_tail_losses) = calculate_tail_losses(
      it_returns       = lt_returns
      iv_var_threshold = CONV #( ls_var_result-var_amount ) ).

    " Calculate ES as weighted average of tail losses
    DATA(lv_es_value) = calculate_weighted_average( lt_tail_losses ).

    " Prepare result
    rs_result = VALUE #(
      result_id        = cl_system_uuid=>create_uuid_x16_static( )
      config_id        = ms_config-config_id
      calculation_date = sy-datum
      portfolio_id     = iv_portfolio_id
      es_amount       = lv_es_value
      var_amount      = ls_var_result-var_amount
      currency        = ms_config-base_currency
      confidence_level = ms_config-confidence_level
      time_horizon    = ms_config-time_horizon
      method          = 'ES_' && ls_var_result-method ).
  ENDMETHOD.

  METHOD calculate_tail_losses.
    DATA: lv_total_weight TYPE f VALUE 0.

    " Identify returns beyond VaR threshold
    LOOP AT it_returns INTO DATA(ls_return)
      WHERE return_value < iv_var_threshold.

      " Calculate loss and weight for this observation
      APPEND VALUE #(
        return_date = ls_return-return_date
        loss_value = abs( ls_return-return_value )
        weight     = 1 ) TO rt_tail_losses.

      lv_total_weight = lv_total_weight + 1.
    ENDLOOP.

    " Normalize weights
    IF lv_total_weight > 0.
      LOOP AT rt_tail_losses ASSIGNING FIELD-SYMBOL(<ls_loss>).
        <ls_loss>-weight = <ls_loss>-weight / lv_total_weight.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.

  METHOD calculate_weighted_average.
    DATA: lv_total_weighted_loss TYPE f VALUE 0.

    " Calculate weighted average of tail losses
    LOOP AT it_losses INTO DATA(ls_loss).
      lv_total_weighted_loss = lv_total_weighted_loss +
        ( ls_loss-loss_value * ls_loss-weight ).
    ENDLOOP.

    rv_es_value = lv_total_weighted_loss.
  ENDMETHOD.

ENDCLASS.