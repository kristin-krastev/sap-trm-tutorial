CLASS zcl_volatility_impact_analysis DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_volatility_types.
    ALIASES: ts_price_history FOR zif_volatility_types~ts_price_history,
             tt_price_history FOR zif_volatility_types~tt_price_history,
             ts_daily_returns FOR zif_volatility_types~ts_daily_returns,
             tt_daily_returns FOR zif_volatility_types~tt_daily_returns,
             ts_volatility_point FOR zif_volatility_types~ts_volatility_point,
             tt_volatility_surface FOR zif_volatility_types~tt_volatility_surface,
             ts_term_structure_point FOR zif_volatility_types~ts_term_structure_point,
             tt_term_structure FOR zif_volatility_types~tt_term_structure,
             ts_correlation_point FOR zif_volatility_types~ts_correlation_point,
             tt_correlation_matrix FOR zif_volatility_types~tt_correlation_matrix,
             tt_commodity_ids FOR zif_volatility_types~tt_commodity_ids.

    METHODS:
      constructor,

      calculate_volatility_surface
        IMPORTING
          iv_commodity_id        TYPE string
          iv_reference_date     TYPE datum
        RETURNING
          VALUE(rt_vol_surface) TYPE tt_volatility_surface,

      analyze_term_structure
        IMPORTING
          iv_commodity_id        TYPE string
          iv_reference_date     TYPE datum
        RETURNING
          VALUE(rt_term_struct) TYPE tt_term_structure,

      calculate_correlation_matrix
        IMPORTING
          it_commodity_ids      TYPE tt_commodity_ids
          iv_reference_date     TYPE datum
        RETURNING
          VALUE(rt_correlation) TYPE tt_correlation_matrix.

  PRIVATE SECTION.
    METHODS:
      get_historical_prices
        IMPORTING
          iv_commodity_id   TYPE string
          iv_reference_date TYPE datum
          iv_days_back     TYPE i
        RETURNING
          VALUE(rt_prices) TYPE tt_price_history,

      calculate_daily_returns
        IMPORTING
          it_prices        TYPE tt_price_history
        RETURNING
          VALUE(rt_returns) TYPE tt_daily_returns,

      calculate_rolling_volatility
        IMPORTING
          it_returns           TYPE tt_daily_returns
          iv_window_size      TYPE i
        RETURNING
          VALUE(rv_volatility) TYPE f.

ENDCLASS.

CLASS zcl_volatility_impact_analysis IMPLEMENTATION.

  METHOD constructor.
    " Initialize any necessary attributes
  ENDMETHOD.

  METHOD calculate_volatility_surface.
    DATA: lt_prices TYPE tt_price_history,
          lt_returns TYPE tt_daily_returns,
          lv_vol_30d TYPE f,
          lv_vol_90d TYPE f.

    " Get historical prices for the last 90 days
    lt_prices = get_historical_prices(
      iv_commodity_id   = iv_commodity_id
      iv_reference_date = iv_reference_date
      iv_days_back     = 90 ).

    " Calculate daily returns
    lt_returns = calculate_daily_returns( lt_prices ).

    " Calculate rolling volatilities for different windows
    lv_vol_30d = calculate_rolling_volatility(
      it_returns      = lt_returns
      iv_window_size = 30 ).

    lv_vol_90d = calculate_rolling_volatility(
      it_returns      = lt_returns
      iv_window_size = 90 ).

    " Build volatility surface based on calculated volatilities
    " This is a simplified example - in practice, you would use more sophisticated methods
    APPEND VALUE #( strike_price = 1 maturity = 30 volatility = lv_vol_30d ) TO rt_vol_surface.
    APPEND VALUE #( strike_price = 1 maturity = 90 volatility = lv_vol_90d ) TO rt_vol_surface.
  ENDMETHOD.

  METHOD analyze_term_structure.
    DATA: lt_prices TYPE tt_price_history,
          lt_returns TYPE tt_daily_returns,
          lv_volatility TYPE f.

    " Get historical prices for a longer period (e.g., 360 days) for term structure analysis
    lt_prices = get_historical_prices(
      iv_commodity_id   = iv_commodity_id
      iv_reference_date = iv_reference_date
      iv_days_back     = 360 ).

    " Calculate daily returns
    lt_returns = calculate_daily_returns( lt_prices ).

    " Calculate volatilities for different tenors
    DO 12 TIMES.
      DATA(lv_tenor) = sy-index * 30.  " Monthly tenors
      lv_volatility = calculate_rolling_volatility(
        it_returns      = lt_returns
        iv_window_size = lv_tenor ).

      APPEND VALUE #( tenor = lv_tenor volatility = lv_volatility )
        TO rt_term_struct.
    ENDDO.
  ENDMETHOD.

  METHOD calculate_correlation_matrix.
    DATA: lt_prices_1 TYPE tt_price_history,
          lt_prices_2 TYPE tt_price_history,
          lt_returns_1 TYPE tt_daily_returns,
          lt_returns_2 TYPE tt_daily_returns.

    " Loop through commodity pairs to calculate correlations
    LOOP AT it_commodity_ids INTO DATA(lv_commodity_1).
      LOOP AT it_commodity_ids INTO DATA(lv_commodity_2).
        " Skip if same commodity
        CHECK lv_commodity_1 <> lv_commodity_2.

        " Get historical prices for both commodities
        lt_prices_1 = get_historical_prices(
          iv_commodity_id   = lv_commodity_1
          iv_reference_date = iv_reference_date
          iv_days_back     = 90 ).

        lt_prices_2 = get_historical_prices(
          iv_commodity_id   = lv_commodity_2
          iv_reference_date = iv_reference_date
          iv_days_back     = 90 ).

        " Calculate daily returns
        lt_returns_1 = calculate_daily_returns( lt_prices_1 ).
        lt_returns_2 = calculate_daily_returns( lt_prices_2 ).

        " Calculate correlation between the return series
        " Note: This is a simplified correlation calculation
        DATA(lv_correlation) = 0.  " Implement actual correlation calculation

        APPEND VALUE #(
          commodity_id_1 = lv_commodity_1
          commodity_id_2 = lv_commodity_2
          correlation   = lv_correlation
        ) TO rt_correlation.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_historical_prices.
    " Fetch historical price data from database
    SELECT commodity_id,
           trading_date,
           price,
           currency,
           volume
      FROM ztcomm_price_history
      INTO TABLE @rt_prices
      WHERE commodity_id = @iv_commodity_id
        AND trading_date <= @iv_reference_date
        AND trading_date >= @iv_reference_date - @iv_days_back
      ORDER BY trading_date DESCENDING.
  ENDMETHOD.

  METHOD calculate_daily_returns.
    DATA: lv_previous_price TYPE f,
          lv_return TYPE f.

    LOOP AT it_prices INTO DATA(ls_price).
      IF lv_previous_price IS NOT INITIAL.
        lv_return = ( ls_price-price - lv_previous_price ) / lv_previous_price.
        APPEND VALUE #(
          commodity_id  = ls_price-commodity_id
          trading_date = ls_price-trading_date
          return_value = lv_return
        ) TO rt_returns.
      ENDIF.
      lv_previous_price = ls_price-price.
    ENDLOOP.
  ENDMETHOD.

  METHOD calculate_rolling_volatility.
    DATA: lv_sum_squared_returns TYPE f,
          lv_mean_return TYPE f,
          lv_count TYPE i.

    " Calculate mean return
    LOOP AT it_returns INTO DATA(ls_return).
      lv_count = lv_count + 1.
      IF lv_count > iv_window_size.
        EXIT.
      ENDIF.
      lv_mean_return = lv_mean_return + ls_return-return_value.
    ENDLOOP.
    lv_mean_return = lv_mean_return / iv_window_size.

    " Calculate sum of squared deviations
    lv_count = 0.
    LOOP AT it_returns INTO ls_return.
      lv_count = lv_count + 1.
      IF lv_count > iv_window_size.
        EXIT.
      ENDIF.
      lv_sum_squared_returns = lv_sum_squared_returns +
        ( ls_return-return_value - lv_mean_return ) ** 2.
    ENDLOOP.

    " Calculate annualized volatility
    rv_volatility = sqrt( lv_sum_squared_returns / ( iv_window_size - 1 ) ) *
      sqrt( 252 ).  " Annualization factor for daily data
  ENDMETHOD.

ENDCLASS.