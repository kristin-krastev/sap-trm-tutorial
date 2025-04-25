# Monte Carlo Value at Risk (VaR) Implementation

## Overview
Monte Carlo VaR uses simulation to generate multiple potential price paths based on statistical properties of the commodity prices. This method is particularly valuable for commodities with complex price behaviors or when dealing with non-linear positions like options.

## Implementation Example

```abap
CLASS zcl_monte_carlo_var_calc DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES:
      BEGIN OF ty_simulation_params,
        mean_return    TYPE decfloat34,
        volatility     TYPE decfloat34,
        time_steps    TYPE i,
        num_sims      TYPE i,
        initial_price TYPE decfloat34,
      END OF ty_simulation_params,

      BEGIN OF ty_mc_result,
        simulation_id TYPE i,
        final_value  TYPE decfloat34,
      END OF ty_mc_result,
      tt_mc_results TYPE STANDARD TABLE OF ty_mc_result WITH KEY simulation_id.

    METHODS:
      calculate_monte_carlo_var
        IMPORTING
          is_params          TYPE ty_simulation_params
          iv_confidence_level TYPE decfloat16
          iv_holding_period  TYPE i
        RETURNING
          VALUE(rv_var)      TYPE decfloat34
        RAISING
          zcx_var_calculation_error,

      estimate_parameters
        IMPORTING
          it_price_history     TYPE zcl_commodity_var_calculator=>tt_price_history
        RETURNING
          VALUE(rs_parameters) TYPE ty_simulation_params.

  PRIVATE SECTION.
    METHODS:
      generate_random_normal
        RETURNING
          VALUE(rv_random) TYPE decfloat34,

      simulate_price_path
        IMPORTING
          is_params           TYPE ty_simulation_params
        RETURNING
          VALUE(rv_end_price) TYPE decfloat34.

ENDCLASS.

CLASS zcl_monte_carlo_var_calc IMPLEMENTATION.
  METHOD calculate_monte_carlo_var.
    " Validate inputs
    IF iv_confidence_level NOT BETWEEN '0.9' AND '0.99' OR
       iv_holding_period <= 0 OR
       is_params-num_sims <= 0.

      RAISE EXCEPTION TYPE zcx_var_calculation_error
        EXPORTING
          textid = zcx_var_calculation_error=>invalid_parameters.
    ENDIF.

    DATA: lt_simulation_results TYPE tt_mc_results,
          ls_result            TYPE ty_mc_result.

    " Run simulations
    DO is_params-num_sims TIMES.
      ls_result-simulation_id = sy-index.
      ls_result-final_value = simulate_price_path( is_params ).
      APPEND ls_result TO lt_simulation_results.
    ENDDO.

    " Sort results for percentile calculation
    SORT lt_simulation_results BY final_value ASCENDING.

    " Find VaR at specified confidence level
    DATA(lv_index) = CONV i( is_params-num_sims * ( 1 - iv_confidence_level ) ).
    READ TABLE lt_simulation_results INDEX lv_index
      INTO DATA(ls_var_result).

    " Calculate VaR as the difference from initial price
    rv_var = abs( is_params-initial_price - ls_var_result-final_value ).

  ENDMETHOD.

  METHOD simulate_price_path.
    DATA(lv_current_price) = is_params-initial_price.
    DATA(lv_dt) = CONV decfloat34( iv_holding_period ) / is_params-time_steps.

    " Simulate price path using geometric Brownian motion
    DO is_params-time_steps TIMES.
      DATA(lv_random) = generate_random_normal( ).

      lv_current_price = lv_current_price * exp(
        ( is_params-mean_return - ( is_params-volatility ** 2 ) / 2 ) * lv_dt +
        is_params-volatility * sqrt( lv_dt ) * lv_random
      ).
    ENDDO.

    rv_end_price = lv_current_price.
  ENDMETHOD.

  METHOD generate_random_normal.
    " Box-Muller transform for normal distribution
    DATA: lv_u1 TYPE decfloat34,
          lv_u2 TYPE decfloat34.

    " Generate uniform random numbers between 0 and 1
    CALL FUNCTION 'RANDOM_DOUBLE'
      IMPORTING
        random_double = lv_u1.

    CALL FUNCTION 'RANDOM_DOUBLE'
      IMPORTING
        random_double = lv_u2.

    " Transform to normal distribution
    rv_random = sqrt( -2 * log( lv_u1 ) ) * cos( 2 * acos( -1 ) * lv_u2 ).
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

    " Calculate mean
    rs_parameters-mean_return = lv_sum / lines( lt_returns ).

    " Calculate volatility
    LOOP AT lt_returns INTO DATA(lv_ret).
      lv_sum_sq = lv_sum_sq + ( lv_ret - rs_parameters-mean_return ) ** 2.
    ENDLOOP.
    rs_parameters-volatility = sqrt( lv_sum_sq / ( lines( lt_returns ) - 1 ) ).

    " Set simulation parameters
    rs_parameters-time_steps = 252.     " Daily steps for a year
    rs_parameters-num_sims = 10000.     " Number of simulations
    rs_parameters-initial_price = ls_price-price.  " Latest price
  ENDMETHOD.
ENDCLASS.

" Usage Example:
DATA(lo_mc_calc) = NEW zcl_monte_carlo_var_calc( ).

" Get historical data for parameter estimation
SELECT trading_date,
       price,
       volume
  FROM ztcomm_price_history
  WHERE commodity_id = 'CRUDE_OIL'
    AND trading_date >= add_days( @sy-datum, -252 )
  INTO TABLE @DATA(lt_price_history).

" Estimate parameters from historical data
DATA(ls_params) = lo_mc_calc->estimate_parameters( lt_price_history ).

TRY.
    " Calculate 10-day VaR at 95% confidence level
    DATA(lv_var) = lo_mc_calc->calculate_monte_carlo_var(
      is_params          = ls_params
      iv_confidence_level = '0.95'
      iv_holding_period  = 10
    ).

CATCH zcx_var_calculation_error INTO DATA(lx_error).
    MESSAGE lx_error TYPE 'E'.
ENDTRY.
```

## Key Components

### 1. Simulation Parameters
- Mean return (drift)
- Volatility
- Number of time steps
- Number of simulations
- Initial price

### 2. Calculation Steps
1. Estimate parameters from historical data
2. Generate multiple price paths using geometric Brownian motion
3. Calculate portfolio value for each path
4. Find the percentile corresponding to confidence level

### 3. Configuration Parameters
- Confidence Level (typically 95% or 99%)
- Holding Period (typically 1-10 days)
- Number of Simulations (typically 10,000+)

### 4. Risk Considerations
- More computationally intensive than Historical VaR
- Better at handling non-linear positions
- Can incorporate changing volatility and correlations
- Allows for stress testing of parameters

## Integration with SAP TRM

### CDS Views
```sql
@AbapCatalog.sqlViewName: 'ZVMCVARANALYSIS'
@AbapCatalog.compiler.compareFilter: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Monte Carlo VaR Analysis'
define view Z_MC_VAR_ANALYSIS
  as select from ztcomm_price_history as History
{
  key commodity_id,
      trading_date,
      price,
      volume,

      // Calculate parameters
      @EndUserText.label: 'Rolling Volatility'
      cast(
        sqrt(
          avg(
            power(
              ln(price / lag(price) over (order by trading_date)),
              2
            )
          ) * 252
        ) as abap.dec(15,5)
      ) as annual_volatility,

      @EndUserText.label: 'Mean Return'
      cast(
        avg(
          ln(price / lag(price) over (order by trading_date))
        ) * 252 as abap.dec(15,5)
      ) as annual_return
}
where trading_date >= add_days(current_date, -252)
group by commodity_id, trading_date, price, volume;
```

### Best Practices
1. Use parallel processing for large simulations
2. Cache parameter estimates for frequent calculations
3. Implement proper error handling
4. Regular validation of model assumptions
5. Consider multiple risk factors for complex commodities