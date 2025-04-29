# Expected Shortfall Technical Implementation Guide

## Architecture Overview

### Core Components

1. **ES Calculator Class (`zcl_trm_es_calculator`)**
   - Inherits from base risk calculator
   - Implements ES-specific calculations
   - Handles multiple calculation methods

2. **Supporting Classes**
   - Historical ES (`zcl_trm_es_historical`)
   - Parametric ES (`zcl_trm_es_parametric`)
   - Monte Carlo ES (`zcl_trm_es_monte_carlo`)

## Implementation Details

### 1. Base ES Calculator

```abap
CLASS zcl_trm_es_calculator DEFINITION
  PUBLIC
  INHERITING FROM zcl_trm_risk_calc_base
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      calculate_es
        IMPORTING
          iv_portfolio_id TYPE ztrm_portfolio_id
          it_market_data  TYPE ztrm_tt_market_data
        RETURNING
          VALUE(rs_result) TYPE ztrm_s_es_result
        RAISING
          cx_static_check,

      calculate_tail_losses
        IMPORTING
          it_returns       TYPE ztrm_tt_returns
          iv_var_threshold TYPE f
        RETURNING
          VALUE(rt_losses) TYPE ztrm_tt_tail_losses.

  PROTECTED SECTION.
    METHODS:
      calculate_weighted_average
        IMPORTING
          it_tail_losses TYPE ztrm_tt_tail_losses
        RETURNING
          VALUE(rv_es)  TYPE f.

ENDCLASS.
```

### 2. Historical ES Implementation

```abap
METHOD calculate_historical_es.
  " First calculate historical VaR
  DATA(ls_var_result) = mo_var_calculator->calculate_var(
    iv_portfolio_id = iv_portfolio_id
    it_market_data  = it_market_data ).

  " Identify losses beyond VaR
  DATA(lt_tail_losses) = calculate_tail_losses(
    it_returns       = mt_returns
    iv_var_threshold = ls_var_result-var_amount ).

  " Calculate ES as weighted average of tail losses
  DATA(lv_es) = calculate_weighted_average( lt_tail_losses ).

  " Return result
  rs_result = VALUE #(
    portfolio_id     = iv_portfolio_id
    es_amount       = lv_es
    confidence_level = ms_config-confidence_level
    calculation_date = sy-datum
    method          = ztrm_es_method-historical ).
ENDMETHOD.
```

### 3. Parametric ES Implementation

```abap
METHOD calculate_parametric_es.
  DATA: lv_alpha TYPE f,
        lv_phi   TYPE f,
        lv_z     TYPE f.

  " Calculate standard normal parameters
  lv_alpha = ms_config-confidence_level.
  lv_z = get_normal_quantile( lv_alpha ).
  lv_phi = get_normal_density( lv_z ).

  " Calculate portfolio volatility
  DATA(lv_volatility) = calculate_portfolio_volatility(
    it_returns = mt_returns
    it_weights = mt_weights ).

  " Calculate ES using parametric formula
  " ES = VaR + (φ(Φ⁻¹(α))/(1-α)) × σ
  DATA(lv_es) = ls_var_result-var_amount +
    ( lv_phi / ( 1 - lv_alpha ) ) * lv_volatility.

  " Return result
  rs_result = VALUE #(
    portfolio_id     = iv_portfolio_id
    es_amount       = lv_es
    confidence_level = lv_alpha
    calculation_date = sy-datum
    method          = ztrm_es_method-parametric ).
ENDMETHOD.
```

## Data Structures

### 1. Configuration Types

```abap
TYPES: BEGIN OF ty_es_config,
  config_id         TYPE sysuuid_x16,
  confidence_level TYPE ztrm_confidence_level,
  calculation_method TYPE ztrm_es_method,
  historical_window TYPE int4,
  base_currency    TYPE waers,
END OF ty_es_config.
```

### 2. Result Types

```abap
TYPES: BEGIN OF ty_es_result,
  result_id        TYPE sysuuid_x16,
  portfolio_id     TYPE ztrm_portfolio_id,
  es_amount       TYPE p LENGTH 15 DECIMALS 2,
  confidence_level TYPE ztrm_confidence_level,
  calculation_date TYPE d,
  method          TYPE ztrm_es_method,
END OF ty_es_result.
```

## Performance Optimization

### 1. Efficient Tail Loss Calculation

```abap
METHOD calculate_tail_losses.
  " Sort returns in ascending order
  DATA(lt_sorted_returns) = it_returns.
  SORT lt_sorted_returns BY return_value ASCENDING.

  " Calculate number of tail observations
  DATA(lv_tail_count) = CONV i(
    lines( lt_sorted_returns ) * ( 1 - ms_config-confidence_level ) ).

  " Extract tail losses efficiently
  rt_losses = VALUE #(
    FOR i = 1 UNTIL i > lv_tail_count
    ( lt_sorted_returns[ i ] ) ).
ENDMETHOD.
```

### 2. Memory Management

```abap
METHOD cleanup_calculations.
  " Clear internal tables after calculation
  CLEAR: mt_returns,
         mt_weights,
         mt_covariance.

  " Free memory for large objects
  FREE mt_market_data.
ENDMETHOD.
```

## Error Handling

### 1. Input Validation

```abap
METHOD validate_es_input.
  " Check confidence level
  IF ms_config-confidence_level NOT BETWEEN '0.90' AND '0.99'.
    RAISE EXCEPTION TYPE zcx_trm_risk_calc
      EXPORTING
        textid = zcx_trm_risk_calc=>invalid_confidence_level.
  ENDIF.

  " Check historical data sufficiency
  IF lines( it_market_data ) < ms_config-historical_window.
    RAISE EXCEPTION TYPE zcx_trm_risk_calc
      EXPORTING
        textid = zcx_trm_risk_calc=>insufficient_history.
  ENDIF.
ENDMETHOD.
```

## Testing Framework

### 1. Unit Test Example

```abap
CLASS ltcl_es_calculator DEFINITION
  FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA:
      mo_es_calc TYPE REF TO zcl_trm_es_calculator,
      mt_test_data TYPE ztrm_tt_market_data.

    METHODS:
      setup,
      test_historical_es FOR TESTING,
      test_parametric_es FOR TESTING,
      test_tail_losses FOR TESTING.
ENDCLASS.
```

## Reporting Integration

### 1. CDS View for ES Results

```sql
@Analytics.dataCategory: #CUBE
define view Z_TRM_ES_ANALYTICS as select from tfm_es_results
  association [1..1] to I_Currency as _Currency
    on $projection.Currency = _Currency.Currency
{
  key result_id,
  portfolio_id,
  @Semantics.amount.currencyCode: 'Currency'
  es_amount,
  @Semantics.currencyCode: true
  currency,
  confidence_level,
  calculation_date,
  method,

  /* Associations */
  _Currency
}
```

### 2. ALV Report Integration

```abap
METHOD display_es_results.
  DATA: lt_fieldcat TYPE slis_t_fieldcat_alv.

  " Build field catalog
  lt_fieldcat = VALUE #(
    ( fieldname = 'PORTFOLIO_ID' seltext_m = 'Portfolio' )
    ( fieldname = 'ES_AMOUNT' seltext_m = 'ES Amount' )
    ( fieldname = 'CURRENCY' seltext_m = 'Currency' )
    ( fieldname = 'CONFIDENCE_LEVEL' seltext_m = 'Conf. Level' )
    ( fieldname = 'METHOD' seltext_m = 'Method' ) ).

  " Display results
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_structure_name = 'ZTRM_ES_RESULT'
      it_fieldcat     = lt_fieldcat
    TABLES
      t_outtab        = mt_results.
ENDMETHOD.
```