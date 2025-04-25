# Stress Testing Implementation for Commodity Risk

## Overview
Stress testing examines portfolio behavior under extreme but plausible scenarios. For commodities, this is particularly important due to supply chain disruptions, geopolitical events, and weather impacts.

## Implementation Example

```abap
CLASS zcl_commodity_stress_test DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES:
      BEGIN OF ty_stress_scenario,
        scenario_id   TYPE string,
        description  TYPE string,
        price_shock  TYPE decfloat34,    " Percentage change
        vol_shock    TYPE decfloat34,    " Volatility multiplier
        correlation_shock TYPE decfloat34, " Correlation adjustment
        supply_disruption TYPE abap.bool,
        duration_days    TYPE i,
      END OF ty_stress_scenario,
      tt_scenarios TYPE STANDARD TABLE OF ty_stress_scenario WITH KEY scenario_id,

      BEGIN OF ty_stress_result,
        scenario_id      TYPE string,
        position_value   TYPE decfloat34,
        var_impact      TYPE decfloat34,
        liquidity_impact TYPE decfloat34,
        total_loss      TYPE decfloat34,
      END OF ty_stress_result,
      tt_results TYPE STANDARD TABLE OF ty_stress_result WITH KEY scenario_id.

    METHODS:
      run_stress_test
        IMPORTING
          it_scenarios       TYPE tt_scenarios
          iv_position_value TYPE decfloat34
          iv_commodity_id   TYPE string
        RETURNING
          VALUE(rt_results) TYPE tt_results
        RAISING
          zcx_stress_test_error,

      get_historical_scenarios
        RETURNING
          VALUE(rt_scenarios) TYPE tt_scenarios,

      get_hypothetical_scenarios
        RETURNING
          VALUE(rt_scenarios) TYPE tt_scenarios.

  PRIVATE SECTION.
    METHODS:
      calculate_scenario_impact
        IMPORTING
          is_scenario        TYPE ty_stress_scenario
          iv_position_value TYPE decfloat34
          iv_commodity_id   TYPE string
        RETURNING
          VALUE(rs_result)   TYPE ty_stress_result.

ENDCLASS.

CLASS zcl_commodity_stress_test IMPLEMENTATION.
  METHOD run_stress_test.
    LOOP AT it_scenarios INTO DATA(ls_scenario).
      APPEND calculate_scenario_impact(
        is_scenario       = ls_scenario
        iv_position_value = iv_position_value
        iv_commodity_id   = iv_commodity_id
      ) TO rt_results.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_historical_scenarios.
    " Historical scenarios based on past events
    APPEND VALUE #(
      scenario_id = 'GULF_WAR_1990'
      description = 'Oil price shock during Gulf War'
      price_shock = '0.65'   " 65% price increase
      vol_shock   = '2.0'    " Double volatility
      correlation_shock = '0.8'
      supply_disruption = abap_true
      duration_days    = 90
    ) TO rt_scenarios.

    APPEND VALUE #(
      scenario_id = 'COVID_2020'
      description = 'COVID-19 demand shock'
      price_shock = '-0.30'  " 30% price decrease
      vol_shock   = '3.0'    " Triple volatility
      correlation_shock = '0.9'
      supply_disruption = abap_true
      duration_days    = 120
    ) TO rt_scenarios.

    APPEND VALUE #(
      scenario_id = 'UKRAINE_2022'
      description = 'Ukraine conflict impact'
      price_shock = '0.40'   " 40% price increase
      vol_shock   = '2.5'    " 2.5x volatility
      correlation_shock = '0.85'
      supply_disruption = abap_true
      duration_days    = 180
    ) TO rt_scenarios.
  ENDMETHOD.

  METHOD get_hypothetical_scenarios.
    " Hypothetical extreme scenarios
    APPEND VALUE #(
      scenario_id = 'MAJOR_SUPPLY_DISRUPT'
      description = 'Major supply chain disruption'
      price_shock = '1.00'   " 100% price increase
      vol_shock   = '3.0'    " Triple volatility
      correlation_shock = '0.95'
      supply_disruption = abap_true
      duration_days    = 60
    ) TO rt_scenarios.

    APPEND VALUE #(
      scenario_id = 'MARKET_CRASH'
      description = 'Global market crash'
      price_shock = '-0.50'  " 50% price decrease
      vol_shock   = '4.0'    " 4x volatility
      correlation_shock = '0.99'
      supply_disruption = abap_false
      duration_days    = 30
    ) TO rt_scenarios.

    APPEND VALUE #(
      scenario_id = 'CLIMATE_EVENT'
      description = 'Severe climate event'
      price_shock = '0.75'   " 75% price increase
      vol_shock   = '2.5'    " 2.5x volatility
      correlation_shock = '0.80'
      supply_disruption = abap_true
      duration_days    = 45
    ) TO rt_scenarios.
  ENDMETHOD.

  METHOD calculate_scenario_impact.
    " Initialize result
    rs_result-scenario_id = is_scenario-scenario_id.

    " Calculate direct price impact
    DATA(lv_price_impact) = iv_position_value * is_scenario-price_shock.

    " Calculate VaR adjustment using increased volatility
    DATA(lo_var_calc) = NEW zcl_parametric_var_calc( ).

    " Get current market data
    SELECT SINGLE FROM ztcomm_price_history
      FIELDS price,
             annual_volatility
      WHERE commodity_id = @iv_commodity_id
        AND trading_date = @sy-datum
      INTO @DATA(ls_market_data).

    " Adjust VaR for stress scenario
    DATA(ls_stress_params) = VALUE zcl_parametric_var_calc=>ty_distribution_params(
      mean       = 0  " Conservative assumption
      std_dev    = ls_market_data-annual_volatility * is_scenario-vol_shock
      position   = iv_position_value
      z_score    = lo_var_calc->get_z_score( '0.99' )  " Use 99% confidence in stress
    ).

    DATA(lv_stress_var) = lo_var_calc->calculate_parametric_var(
      is_params        = ls_stress_params
      iv_holding_period = is_scenario-duration_days
    ).

    " Calculate liquidity impact based on correlation shock
    DATA(lv_liquidity_impact) = iv_position_value *
      ( 1 - exp( -1 * is_scenario-correlation_shock * is_scenario-duration_days / 252 ) ).

    " Populate result
    rs_result-position_value   = iv_position_value.
    rs_result-var_impact      = lv_stress_var.
    rs_result-liquidity_impact = lv_liquidity_impact.
    rs_result-total_loss      = lv_price_impact + lv_stress_var + lv_liquidity_impact.
  ENDMETHOD.
ENDCLASS.

" Usage Example:
DATA(lo_stress_test) = NEW zcl_commodity_stress_test( ).

" Get both historical and hypothetical scenarios
DATA(lt_scenarios) = lo_stress_test->get_historical_scenarios( ).
APPEND LINES OF lo_stress_test->get_hypothetical_scenarios( ) TO lt_scenarios.

" Run stress tests for crude oil position
DATA(lv_position_value) = '1000000.00'. " $1M position
DATA(lt_results) = lo_stress_test->run_stress_test(
  it_scenarios      = lt_scenarios
  iv_position_value = lv_position_value
  iv_commodity_id   = 'CRUDE_OIL'
).

" Display results
LOOP AT lt_results INTO DATA(ls_result).
  WRITE: / 'Scenario:', ls_result-scenario_id,
         / 'Total Potential Loss:', ls_result-total_loss,
         / 'VaR Impact:', ls_result-var_impact,
         / 'Liquidity Impact:', ls_result-liquidity_impact,
         / '-------------------'.
ENDLOOP.
```

## Key Components

### 1. Scenario Types
- **Historical Scenarios**
  - Based on actual past events
  - Known market behavior
  - Validated impacts

- **Hypothetical Scenarios**
  - Extreme but plausible events
  - Forward-looking risks
  - Emerging threats

### 2. Stress Parameters
- Price shocks
- Volatility multipliers
- Correlation changes
- Supply disruptions
- Duration of stress

### 3. Impact Assessment
- Direct price impact
- VaR adjustments
- Liquidity effects
- Total potential loss

## Integration with SAP TRM

### CDS Views
```sql
@AbapCatalog.sqlViewName: 'ZVSTRESSTEST'
@AbapCatalog.compiler.compareFilter: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Stress Test Analysis'
define view Z_STRESS_TEST_ANALYSIS
  as select from ztcomm_stress_results as Results
    inner join ztcomm_stress_scenarios as Scenarios
      on Results.scenario_id = Scenarios.scenario_id
{
  key Results.scenario_id,
      Scenarios.description,
      Results.test_date,
      Results.commodity_id,
      Results.position_value,

      @EndUserText.label: 'Price Impact'
      Results.price_impact,

      @EndUserText.label: 'VaR Impact'
      Results.var_impact,

      @EndUserText.label: 'Liquidity Impact'
      Results.liquidity_impact,

      @EndUserText.label: 'Total Loss'
      Results.total_loss,

      @EndUserText.label: 'Impact Severity'
      case
        when Results.total_loss > Results.position_value * 0.5 then 'HIGH'
        when Results.total_loss > Results.position_value * 0.2 then 'MEDIUM'
        else 'LOW'
      end as impact_severity,

      @EndUserText.label: 'Duration (Days)'
      Scenarios.duration_days,

      @EndUserText.label: 'Supply Disruption'
      Scenarios.supply_disruption
}
where Results.test_date >= add_days(current_date, -90);
```

### Best Practices
1. Regular scenario review and updates
2. Combination of historical and hypothetical scenarios
3. Consider multiple risk factors
4. Document scenario assumptions
5. Regular stakeholder review of results