INTERFACE zif_volatility_types
  PUBLIC.

  TYPES: BEGIN OF ts_price_history,
           commodity_id  TYPE string,
           trading_date TYPE datum,
           price        TYPE f,
           currency     TYPE waers,
           volume       TYPE f,
         END OF ts_price_history.

  TYPES: tt_price_history TYPE STANDARD TABLE OF ts_price_history WITH KEY commodity_id trading_date.

  TYPES: BEGIN OF ts_daily_returns,
           commodity_id  TYPE string,
           trading_date TYPE datum,
           return_value TYPE f,
         END OF ts_daily_returns.

  TYPES: tt_daily_returns TYPE STANDARD TABLE OF ts_daily_returns WITH KEY commodity_id trading_date.

  TYPES: BEGIN OF ts_volatility_point,
           strike_price TYPE f,
           maturity    TYPE i,
           volatility  TYPE f,
         END OF ts_volatility_point.

  TYPES: tt_volatility_surface TYPE STANDARD TABLE OF ts_volatility_point WITH KEY strike_price maturity.

  TYPES: BEGIN OF ts_term_structure_point,
           tenor      TYPE i,
           volatility TYPE f,
         END OF ts_term_structure_point.

  TYPES: tt_term_structure TYPE STANDARD TABLE OF ts_term_structure_point WITH KEY tenor.

  TYPES: BEGIN OF ts_correlation_point,
           commodity_id_1 TYPE string,
           commodity_id_2 TYPE string,
           correlation   TYPE f,
         END OF ts_correlation_point.

  TYPES: tt_correlation_matrix TYPE STANDARD TABLE OF ts_correlation_point
         WITH KEY commodity_id_1 commodity_id_2.

  TYPES: tt_commodity_ids TYPE STANDARD TABLE OF string WITH KEY table_line.

ENDINTERFACE.