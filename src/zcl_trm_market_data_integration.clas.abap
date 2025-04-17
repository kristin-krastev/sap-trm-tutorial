CLASS zcl_trm_market_data_integration DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.

    METHODS:
      constructor,

      update_market_rates
        IMPORTING
          it_market_rates TYPE STANDARD TABLE
        RETURNING
          VALUE(rv_success) TYPE abap_bool,

      calculate_position_value
        IMPORTING
          iv_position_id TYPE tfm_position_id
        RETURNING
          VALUE(rv_value) TYPE tfm_amount
        RAISING
          cx_tfm_calculation_error.

  PRIVATE SECTION.
    DATA:
      mo_transaction_api TYPE REF TO cl_tfm_transaction_api,
      mo_position_api   TYPE REF TO cl_tfm_position_api,
      mo_market_api     TYPE REF TO cl_tfm_market_data_api.

    METHODS:
      initialize_apis,
      validate_market_data
        IMPORTING
          it_market_rates TYPE STANDARD TABLE
        RETURNING
          VALUE(rv_valid) TYPE abap_bool.

ENDCLASS.

CLASS zcl_trm_market_data_integration IMPLEMENTATION.

  METHOD constructor.
    initialize_apis( ).
  ENDMETHOD.

  METHOD initialize_apis.
    " Initialize API instances
    mo_transaction_api = cl_tfm_transaction_api=>get_instance( ).
    mo_position_api   = cl_tfm_position_api=>get_instance( ).
    mo_market_api     = cl_tfm_market_data_api=>get_instance( ).
  ENDMETHOD.

  METHOD update_market_rates.
    " Validate incoming market data
    IF validate_market_data( it_market_rates ) = abap_false.
      rv_success = abap_false.
      RETURN.
    ENDIF.

    " Update market rates in Treasury
    TRY.
        " Start update in new LUW
        mo_market_api->start_update( ).

        " Process each market rate
        LOOP AT it_market_rates INTO DATA(ls_rate).
          " Update rate in Treasury
          mo_market_api->update_market_data(
            EXPORTING
              is_market_data = ls_rate
            IMPORTING
              ev_success    = DATA(lv_update_success)
          ).

          " If update fails, exit with error
          IF lv_update_success = abap_false.
            mo_market_api->rollback( ).
            rv_success = abap_false.
            RETURN.
          ENDIF.
        ENDLOOP.

        " Commit changes
        mo_market_api->save( ).
        rv_success = abap_true.

      CATCH cx_tfm_market_error INTO DATA(lx_market_error).
        " Handle market data errors
        mo_market_api->rollback( ).
        rv_success = abap_false.
    ENDTRY.
  ENDMETHOD.

  METHOD calculate_position_value.
    TRY.
        " Get position details
        DATA(ls_position) = mo_position_api->get_position(
          iv_position_id = iv_position_id
        ).

        " Get current market rate
        DATA(lv_market_rate) = mo_market_api->get_market_price(
          iv_instrument_id = ls_position-instrument_id
          iv_date         = sy-datum
        ).

        " Calculate position value
        rv_value = ls_position-quantity * lv_market_rate.

        " Update position value in database
        mo_position_api->update_position_value(
          iv_position_id = iv_position_id
          iv_value      = rv_value
        ).

      CATCH cx_tfm_position_error INTO DATA(lx_position_error).
        " Handle position-related errors
        RAISE EXCEPTION TYPE cx_tfm_calculation_error
          EXPORTING
            previous = lx_position_error.

      CATCH cx_tfm_market_error INTO DATA(lx_market_error).
        " Handle market data errors
        RAISE EXCEPTION TYPE cx_tfm_calculation_error
          EXPORTING
            previous = lx_market_error.
    ENDTRY.
  ENDMETHOD.

  METHOD validate_market_data.
    rv_valid = abap_true.

    " Basic validation of market rates
    LOOP AT it_market_rates INTO DATA(ls_rate).
      " Check for required fields
      IF ls_rate-instrument_id IS INITIAL OR
         ls_rate-rate_value IS INITIAL OR
         ls_rate-valid_from IS INITIAL.
        rv_valid = abap_false.
        EXIT.
      ENDIF.

      " Validate rate value is positive
      IF ls_rate-rate_value <= 0.
        rv_valid = abap_false.
        EXIT.
      ENDIF.

      " Check valid_from is not in the past
      IF ls_rate-valid_from < sy-datum.
        rv_valid = abap_false.
        EXIT.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD if_oo_adt_classrun~main.
    " Example usage in ABAP Console
    TRY.
        " Create test market rates
        DATA: lt_market_rates TYPE STANDARD TABLE OF tfm_market_rate.

        " Add sample market rate
        APPEND VALUE #(
          instrument_id = 'BOND001'
          rate_value   = '1.25'
          valid_from   = sy-datum
          source       = 'REUTERS'
        ) TO lt_market_rates.

        " Update market rates
        DATA(lv_success) = update_market_rates( lt_market_rates ).

        IF lv_success = abap_true.
          out->write( 'Market rates updated successfully' ).
        ELSE.
          out->write( 'Failed to update market rates' ).
        ENDIF.

        " Calculate position value
        DATA(lv_position_value) = calculate_position_value( 'POS001' ).
        out->write( |Position value: { lv_position_value }| ).

      CATCH cx_tfm_calculation_error INTO DATA(lx_calc_error).
        out->write( lx_calc_error->get_text( ) ).
    ENDTRY.
  ENDMETHOD.

ENDCLASS.