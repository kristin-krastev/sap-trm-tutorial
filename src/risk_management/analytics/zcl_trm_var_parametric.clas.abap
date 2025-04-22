CLASS zcl_trm_var_parametric DEFINITION
  PUBLIC
  INHERITING FROM zcl_trm_var_calculator_base
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      zif_trm_var_calculator~calculate_var REDEFINITION.

  PROTECTED SECTION.
    METHODS:
      calculate_covariance_matrix
        IMPORTING
          it_returns          TYPE STANDARD TABLE
        RETURNING
          VALUE(rt_covariance) TYPE tt_covariance_matrix,

      calculate_correlation_matrix
        IMPORTING
          it_returns           TYPE STANDARD TABLE
        RETURNING
          VALUE(rt_correlation) TYPE tt_correlation_matrix,

      calculate_normal_var
        IMPORTING
          iv_confidence_level TYPE zif_trm_var_calculator=>ty_confidence_level
          iv_volatility      TYPE f
          iv_position_value  TYPE f
          iv_time_horizon    TYPE i
        RETURNING
          VALUE(rv_var)      TYPE f,

      calculate_portfolio_weights
        IMPORTING
          it_positions      TYPE STANDARD TABLE
        RETURNING
          VALUE(rt_weights) TYPE tt_portfolio_weights.

  PRIVATE SECTION.
    METHODS:
      get_normal_quantile
        IMPORTING
          iv_confidence_level TYPE zif_trm_var_calculator=>ty_confidence_level
        RETURNING
          VALUE(rv_quantile) TYPE f.

ENDCLASS.

CLASS zcl_trm_var_parametric IMPLEMENTATION.

  METHOD zif_trm_var_calculator~calculate_var.
    " Calculate returns from market data
    DATA(lt_returns) = calculate_returns( it_market_data ).

    " Calculate covariance matrix
    DATA(lt_covariance) = calculate_covariance_matrix( lt_returns ).

    " Get portfolio positions and calculate weights
    DATA(lt_positions) = get_portfolio_positions( iv_portfolio_id ).
    DATA(lt_weights) = calculate_portfolio_weights( lt_positions ).

    " Calculate portfolio variance
    DATA(lv_portfolio_variance) = calculate_portfolio_variance(
      it_covariance = lt_covariance
      it_weights    = lt_weights ).

    " Calculate portfolio volatility (standard deviation)
    DATA(lv_volatility) = sqrt( lv_portfolio_variance ).

    " Calculate total portfolio value
    DATA(lv_portfolio_value) = REDUCE f( INIT sum = 0
                                       FOR ls_pos IN lt_positions
                                       NEXT sum = sum + ls_pos-position_value ).

    " Calculate VaR using normal distribution
    DATA(lv_var) = calculate_normal_var(
      iv_confidence_level = ms_config-confidence_level
      iv_volatility      = lv_volatility
      iv_position_value  = lv_portfolio_value
      iv_time_horizon    = ms_config-time_horizon ).

    " Prepare result
    rs_result = VALUE #(
      var_amount = lv_var
      method = 'PARAM'
      confidence_level = ms_config-confidence_level
      calculation_date = sy-datum
      portfolio_id = iv_portfolio_id ).
  ENDMETHOD.

  METHOD calculate_covariance_matrix.
    DATA: lt_mean_returns TYPE STANDARD TABLE OF ty_mean_return.

    " Calculate mean returns for each instrument
    SELECT instrument_id,
           AVG( return_value ) AS mean_return
      FROM @it_returns AS returns
      GROUP BY instrument_id
      INTO TABLE @lt_mean_returns.

    " Calculate covariances
    LOOP AT it_returns INTO DATA(ls_return1).
      LOOP AT it_returns INTO DATA(ls_return2)
        WHERE return_date = ls_return1-return_date.

        " Get mean returns
        READ TABLE lt_mean_returns WITH KEY
          instrument_id = ls_return1-instrument_id
          INTO DATA(ls_mean1).

        READ TABLE lt_mean_returns WITH KEY
          instrument_id = ls_return2-instrument_id
          INTO DATA(ls_mean2).

        " Calculate covariance term
        DATA(lv_covar_term) = ( ls_return1-return_value - ls_mean1-mean_return ) *
                             ( ls_return2-return_value - ls_mean2-mean_return ).

        " Update covariance matrix
        COLLECT VALUE ty_covariance(
          instrument_id1 = ls_return1-instrument_id
          instrument_id2 = ls_return2-instrument_id
          covariance = lv_covar_term
          count = 1 ) INTO rt_covariance.
      ENDLOOP.
    ENDLOOP.

    " Calculate final covariance values
    LOOP AT rt_covariance ASSIGNING FIELD-SYMBOL(<ls_covar>).
      <ls_covar>-covariance = <ls_covar>-covariance / ( <ls_covar>-count - 1 ).
    ENDLOOP.
  ENDMETHOD.

  METHOD calculate_correlation_matrix.
    " Get covariance matrix
    DATA(lt_covariance) = calculate_covariance_matrix( it_returns ).

    " Calculate standard deviations
    DATA: lt_std_dev TYPE STANDARD TABLE OF ty_std_dev.

    LOOP AT lt_covariance INTO DATA(ls_var)
      WHERE instrument_id1 = instrument_id2.
      APPEND VALUE ty_std_dev(
        instrument_id = ls_var-instrument_id1
        std_dev      = sqrt( ls_var-covariance )
      ) TO lt_std_dev.
    ENDLOOP.

    " Calculate correlations
    LOOP AT lt_covariance INTO DATA(ls_covar).
      " Get standard deviations
      READ TABLE lt_std_dev WITH KEY
        instrument_id = ls_covar-instrument_id1
        INTO DATA(ls_std1).

      READ TABLE lt_std_dev WITH KEY
        instrument_id = ls_covar-instrument_id2
        INTO DATA(ls_std2).

      " Calculate correlation coefficient
      APPEND VALUE ty_correlation(
        instrument_id1 = ls_covar-instrument_id1
        instrument_id2 = ls_covar-instrument_id2
        correlation   = ls_covar-covariance / ( ls_std1-std_dev * ls_std2-std_dev )
      ) TO rt_correlation.
    ENDLOOP.
  ENDMETHOD.

  METHOD calculate_normal_var.
    " Get normal distribution quantile for confidence level
    DATA(lv_quantile) = get_normal_quantile( iv_confidence_level ).

    " Calculate VaR
    " VaR = Position * Volatility * Sqrt(Time) * Normal_Quantile
    rv_var = iv_position_value * iv_volatility *
             sqrt( iv_time_horizon / 252 ) * " Annualization factor
             lv_quantile.
  ENDMETHOD.

  METHOD calculate_portfolio_weights.
    DATA: lv_total_value TYPE f.

    " Calculate total portfolio value
    lv_total_value = REDUCE f( INIT sum = 0
                              FOR ls_pos IN it_positions
                              NEXT sum = sum + ls_pos-position_value ).

    " Calculate weights
    LOOP AT it_positions INTO DATA(ls_position).
      APPEND VALUE ty_portfolio_weight(
        instrument_id = ls_position-instrument_id
        weight = ls_position-position_value / lv_total_value
      ) TO rt_weights.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_normal_quantile.
    CASE iv_confidence_level.
      WHEN '0.90'.
        rv_quantile = '1.28'.  " 90% confidence
      WHEN '0.95'.
        rv_quantile = '1.645'. " 95% confidence
      WHEN '0.99'.
        rv_quantile = '2.326'. " 99% confidence
      WHEN OTHERS.
        RAISE EXCEPTION TYPE cx_static_check
          MESSAGE ID 'TRM_VAR'
          MESSAGE NUMBER '001'
          WITH iv_confidence_level.
    ENDCASE.
  ENDMETHOD.

ENDCLASS.