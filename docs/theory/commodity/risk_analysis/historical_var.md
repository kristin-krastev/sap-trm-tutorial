# Historical Value at Risk (VaR) Implementation

## Overview
Historical VaR calculates potential losses based on historical price movements. In commodity trading, this is particularly important due to the high volatility and seasonal patterns in commodity prices.

## Implementation Example

```abap
CLASS zcl_commodity_var_calculator DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES:
      BEGIN OF ty_price_history,
        trading_date TYPE d,
        price       TYPE p LENGTH 15 DECIMALS 2,
        volume      TYPE p LENGTH 15 DECIMALS 3,
      END OF ty_price_history,
      tt_price_history TYPE STANDARD TABLE OF ty_price_history WITH KEY trading_date.

    METHODS:
      calculate_historical_var
        IMPORTING
          iv_confidence_level TYPE decfloat16
          iv_holding_period  TYPE i
          it_price_history   TYPE tt_price_history
        RETURNING
          VALUE(rv_var)      TYPE decfloat34
        RAISING
          zcx_var_calculation_error.

  PRIVATE SECTION.
    METHODS:
      calculate_returns
        IMPORTING
          it_price_history     TYPE tt_price_history
        RETURNING
          VALUE(rt_returns)    TYPE tt_price_history,

      sort_returns_ascending
        IMPORTING
          it_returns           TYPE tt_price_history
        RETURNING
          VALUE(rt_sorted)     TYPE tt_price_history.
ENDCLASS.

CLASS zcl_commodity_var_calculator IMPLEMENTATION.
  METHOD calculate_historical_var.
    " Validate inputs
    IF iv_confidence_level NOT BETWEEN '0.9' AND '0.99' OR
       iv_holding_period <= 0.

      RAISE EXCEPTION TYPE zcx_var_calculation_error
        EXPORTING
          textid = zcx_var_calculation_error=>invalid_parameters.
    ENDIF.

    " Calculate daily returns
    DATA(lt_returns) = calculate_returns( it_price_history ).

    " Sort returns in ascending order
    DATA(lt_sorted_returns) = sort_returns_ascending( lt_returns ).

    " Find the percentile based on confidence level
    DATA(lv_observations) = lines( lt_sorted_returns ).
    DATA(lv_index) = CONV i( lv_observations * ( 1 - iv_confidence_level ) ).

    " Get the VaR value at the specified confidence level
    READ TABLE lt_sorted_returns INDEX lv_index
      INTO DATA(ls_var_return).

    " Scale VaR for holding period using square root of time rule
    rv_var = abs( ls_var_return-price ) * sqrt( iv_holding_period ).

  ENDMETHOD.

  METHOD calculate_returns.
    " Calculate logarithmic returns
    DATA: lv_previous_price TYPE decfloat34.

    LOOP AT it_price_history INTO DATA(ls_price).
      IF lv_previous_price IS NOT INITIAL.
        APPEND VALUE #(
          trading_date = ls_price-trading_date
          price       = log( ls_price-price / lv_previous_price )
          volume      = ls_price-volume
        ) TO rt_returns.
      ENDIF.

      lv_previous_price = ls_price-price.
    ENDLOOP.
  ENDMETHOD.

  METHOD sort_returns_ascending.
    rt_sorted = it_returns.
    SORT rt_sorted BY price ASCENDING.
  ENDMETHOD.
ENDCLASS.

" Usage Example:
DATA(lo_var_calc) = NEW zcl_commodity_var_calculator( ).

" Sample price history data
DATA: lt_price_history TYPE zcl_commodity_var_calculator=>tt_price_history.

" Populate with last 252 trading days (1 year)
SELECT trading_date,
       price,
       volume
  FROM ztcomm_price_history
  WHERE commodity_id = 'CRUDE_OIL'
    AND trading_date >= @lv_year_ago
  INTO TABLE @lt_price_history.

TRY.
    " Calculate 10-day VaR at 95% confidence level
    DATA(lv_var) = lo_var_calc->calculate_historical_var(
      iv_confidence_level = '0.95'
      iv_holding_period  = 10
      it_price_history   = lt_price_history
    ).

    " VaR result represents the potential loss at 95% confidence
    " over a 10-day period based on historical price movements

CATCH zcx_var_calculation_error INTO DATA(lx_error).
    " Handle calculation errors
    MESSAGE lx_error TYPE 'E'.
ENDTRY.
```

## Key Components

### 1. Data Requirements
- Historical price data (minimum 1 year recommended)
- Daily trading volumes
- Clean data without gaps or anomalies

### 2. Calculation Steps
1. Calculate daily logarithmic returns
2. Sort returns in ascending order
3. Find the percentile based on confidence level
4. Scale for holding period using square root of time rule

### 3. Configuration Parameters
- Confidence Level (typically 95% or 99%)
- Holding Period (typically 1-10 days)
- Historical Window (typically 252 trading days)

### 4. Risk Considerations
- Assumes historical patterns will repeat
- May underestimate risk in rapidly changing markets
- Needs regular backtesting and validation

## Integration with SAP TRM

### CDS Views
```sql
@AbapCatalog.sqlViewName: 'ZVCOMMODITYVAR'
@AbapCatalog.compiler.compareFilter: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Commodity VaR Analysis'
define view Z_COMMODITY_VAR_ANALYSIS
  as select from ztcomm_price_history as History
{
  key commodity_id,
      trading_date,
      price,
      volume,

      // Calculate rolling VaR
      @EndUserText.label: '1-Day 95% VaR'
      cast(
        abs( percentile_cont(0.05) within group (order by
          ln(price / lag(price) over (order by trading_date))
        ) ) as abap.dec(15,5)
      ) as var_95_1d,

      // Scale to 10-day VaR
      @EndUserText.label: '10-Day 95% VaR'
      cast(
        abs( percentile_cont(0.05) within group (order by
          ln(price / lag(price) over (order by trading_date))
        ) ) * sqrt(10) as abap.dec(15,5)
      ) as var_95_10d
}
where trading_date >= add_days(current_date, -252)
group by commodity_id, trading_date, price, volume;
```

### Best Practices
1. Use CDS views for real-time VaR monitoring
2. Implement proper error handling
3. Regular backtesting of VaR results
4. Document assumptions and limitations
5. Consider multiple VaR methodologies