# Parametric Value at Risk (VaR) Implementation

## Overview
Parametric VaR assumes returns follow a normal distribution and uses analytical formulas based on the mean and standard deviation of returns. While this is computationally efficient, it may not capture the fat-tailed nature of commodity returns as well as Historical or Monte Carlo methods.

## Implementation Example

```abap
CLASS zcl_parametric_var_calc DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES:
      BEGIN OF ty_distribution_params,
        mean       TYPE decfloat34,
        std_dev    TYPE decfloat34,
        position   TYPE decfloat34,  " Current position value
        z_score    TYPE decfloat34,  " Z-score for confidence level
      END OF ty_distribution_params.

    METHODS:
      calculate_parametric_var
        IMPORTING
          is_params          TYPE ty_distribution_params
          iv_holding_period  TYPE i
        RETURNING
          VALUE(rv_var)      TYPE decfloat34
        RAISING
          zcx_var_calculation_error,

      get_z_score
        IMPORTING
          iv_confidence_level TYPE decfloat16
        RETURNING
          VALUE(rv_z_score)   TYPE decfloat34
        RAISING
          zcx_var_calculation_error,

      estimate_parameters
        IMPORTING
          it_price_history        TYPE zcl_commodity_var_calculator=>tt_price_history
          iv_confidence_level     TYPE decfloat16
          iv_position_value      TYPE decfloat34
        RETURNING
          VALUE(rs_params)        TYPE ty_distribution_params.

ENDCLASS.

CLASS zcl_parametric_var_calc IMPLEMENTATION.
  METHOD calculate_parametric_var.
    " Validate inputs
    IF is_params-std_dev <= 0 OR
       iv_holding_period <= 0.

      RAISE EXCEPTION TYPE zcx_var_calculation_error
        EXPORTING
          textid = zcx_var_calculation_error=>invalid_parameters.
    ENDIF.

    " Calculate VaR using the analytical formula
    " VaR = Position * (z-score * volatility * sqrt(time) - mean * time)
    rv_var = abs( is_params-position * (
      is_params-z_score * is_params-std_dev * sqrt( iv_holding_period ) -
      is_params-mean * iv_holding_period
    ) ).

  ENDMETHOD.

  METHOD get_z_score.
    CASE iv_confidence_level.
      WHEN '0.90'.
        rv_z_score = '1.28155'.  " 90% confidence
      WHEN '0.95'.
        rv_z_score = '1.64485'.  " 95% confidence
      WHEN '0.99'.
        rv_z_score = '2.32635'.  " 99% confidence
      WHEN OTHERS.
        RAISE EXCEPTION TYPE zcx_var_calculation_error
          EXPORTING
            textid = zcx_var_calculation_error=>invalid_confidence_level.
    ENDCASE.
  ENDMETHOD.

  METHOD estimate_parameters.
    DATA: lt_returns TYPE TABLE OF decfloat34,
          lv_sum    TYPE decfloat34,
          lv_sum_sq TYPE decfloat34.

    " Calculate log returns
    LOOP AT it_price_history INTO DATA(ls_price) FROM 2.
      DATA(lv_prev_idx) = sy-tabix - 1.
      READ TABLE it_price_history INDEX lv_prev_idx INTO DATA(ls_prev).

      DATA(lv_return) = log( ls_price-price / ls_prev-price ).
      APPEND lv_return TO lt_returns.
      lv_sum = lv_sum + lv_return.
    ENDLOOP.

    " Calculate mean (annualized)
    rs_params-mean = ( lv_sum / lines( lt_returns ) ) * 252.

    " Calculate standard deviation (annualized)
    DATA(lv_mean_daily) = lv_sum / lines( lt_returns ).
    LOOP AT lt_returns INTO DATA(lv_ret).
      lv_sum_sq = lv_sum_sq + ( lv_ret - lv_mean_daily ) ** 2.
    ENDLOOP.
    rs_params-std_dev = sqrt( lv_sum_sq / ( lines( lt_returns ) - 1 ) ) * sqrt( 252 ).

    " Set position value and z-score
    rs_params-position = iv_position_value.
    rs_params-z_score = get_z_score( iv_confidence_level ).
  ENDMETHOD.
ENDCLASS.

" Usage Example:
DATA(lo_param_calc) = NEW zcl_parametric_var_calc( ).

" Get historical data
SELECT trading_date,
       price,
       volume
  FROM ztcomm_price_history
  WHERE commodity_id = 'CRUDE_OIL'
    AND trading_date >= add_days( @sy-datum, -252 )
  INTO TABLE @DATA(lt_price_history).

" Current position value (example: 1000 barrels at current price)
DATA(lv_position_value) = CONV decfloat34( 1000 * lt_price_history[ lines( lt_price_history ) ]-price ).

TRY.
    " Estimate parameters with 95% confidence
    DATA(ls_params) = lo_param_calc->estimate_parameters(
      it_price_history    = lt_price_history
      iv_confidence_level = '0.95'
      iv_position_value   = lv_position_value
    ).

    " Calculate 10-day VaR
    DATA(lv_var) = lo_param_calc->calculate_parametric_var(
      is_params         = ls_params
      iv_holding_period = 10
    ).

CATCH zcx_var_calculation_error INTO DATA(lx_error).
    MESSAGE lx_error TYPE 'E'.
ENDTRY.
```

## Key Components

### 1. Distribution Parameters
- Mean return (annualized)
- Standard deviation (annualized)
- Position value
- Z-score for confidence level

### 2. Calculation Steps
1. Calculate daily log returns
2. Estimate mean and standard deviation
3. Annualize parameters
4. Apply analytical VaR formula

### 3. Configuration Parameters
- Confidence Level (with corresponding z-scores)
- Holding Period
- Position Size

### 4. Risk Considerations
- Assumes normal distribution of returns
- May underestimate tail risks
- Computationally efficient
- Easy to decompose and attribute risk

## Integration with SAP TRM

### CDS Views
```sql
@AbapCatalog.sqlViewName: 'ZVPARAMVAR'
@AbapCatalog.compiler.compareFilter: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Parametric VaR Analysis'
define view Z_PARAMETRIC_VAR_ANALYSIS
  as select from ztcomm_price_history as History
{
  key commodity_id,
      trading_date,
      price,
      volume,

      // Daily parameters
      @EndUserText.label: 'Daily Mean Return'
      cast(
        avg(
          ln(price / lag(price) over (order by trading_date))
        ) as abap.dec(15,5)
      ) as daily_mean,

      @EndUserText.label: 'Daily Volatility'
      cast(
        sqrt(
          avg(
            power(
              ln(price / lag(price) over (order by trading_date)) -
              avg(ln(price / lag(price) over (order by trading_date))),
              2
            )
          )
        ) as abap.dec(15,5)
      ) as daily_std_dev,

      // Annualized parameters
      @EndUserText.label: 'Annual Mean Return'
      cast(
        avg(
          ln(price / lag(price) over (order by trading_date))
        ) * 252 as abap.dec(15,5)
      ) as annual_mean,

      @EndUserText.label: 'Annual Volatility'
      cast(
        sqrt(
          avg(
            power(
              ln(price / lag(price) over (order by trading_date)) -
              avg(ln(price / lag(price) over (order by trading_date))),
              2
            )
          ) * 252
        ) as abap.dec(15,5)
      ) as annual_std_dev
}
where trading_date >= add_days(current_date, -252)
group by commodity_id, trading_date, price, volume;
```

### Best Practices
1. Use for quick risk assessments
2. Combine with other VaR methods for validation
3. Monitor normality assumptions
4. Regular backtesting
5. Consider using with delta-normal approach for portfolios