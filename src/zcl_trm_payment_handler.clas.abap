CLASS zcl_trm_payment_handler DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      constructor,

      create_payment
        IMPORTING
          is_payment_data TYPE z_trm_payment_trans
        RETURNING
          VALUE(rv_payment_id) TYPE tfm_payment_id
        RAISING
          cx_tfm_payment_error,

      process_payment
        IMPORTING
          iv_payment_id TYPE tfm_payment_id
        RAISING
          cx_tfm_payment_error,

      cancel_payment
        IMPORTING
          iv_payment_id TYPE tfm_payment_id
          iv_reason     TYPE string
        RAISING
          cx_tfm_payment_error.

  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS:
      validate_payment_data
        IMPORTING
          is_payment_data TYPE z_trm_payment_trans
        RAISING
          cx_tfm_payment_error,

      update_payment_status
        IMPORTING
          iv_payment_id TYPE tfm_payment_id
          iv_status    TYPE tfm_pay_status
        RAISING
          cx_tfm_payment_error.
ENDCLASS.

CLASS zcl_trm_payment_handler IMPLEMENTATION.

  METHOD constructor.
    " Initialize any necessary resources
  ENDMETHOD.

  METHOD create_payment.
    " Validate payment data
    validate_payment_data( is_payment_data ).

    " Generate payment ID
    DATA(lv_timestamp) = cl_abap_context_info=>get_system_time( ).
    rv_payment_id = |PAY{ lv_timestamp }|.

    " Set initial status
    DATA(ls_payment) = is_payment_data.
    ls_payment-payment_id = rv_payment_id.
    ls_payment-status = 'PEND'.  " Pending

    " Insert into database
    INSERT z_trm_payment_trans FROM ls_payment.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_tfm_payment_error
        MESSAGE ID 'TFM_PAYMENT'
        NUMBER '001'
        WITH rv_payment_id.
    ENDIF.
  ENDMETHOD.

  METHOD process_payment.
    " Read payment data
    SELECT SINGLE *
      FROM z_trm_payment_trans
      WHERE payment_id = @iv_payment_id
      INTO @DATA(ls_payment).

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_tfm_payment_error
        MESSAGE ID 'TFM_PAYMENT'
        NUMBER '002'
        WITH iv_payment_id.
    ENDIF.

    " Check if payment can be processed
    IF ls_payment-status <> 'PEND'.
      RAISE EXCEPTION TYPE cx_tfm_payment_error
        MESSAGE ID 'TFM_PAYMENT'
        NUMBER '003'
        WITH iv_payment_id ls_payment-status.
    ENDIF.

    " Update status to processing
    update_payment_status(
      iv_payment_id = iv_payment_id
      iv_status    = 'PROC'
    ).

    " Implement payment processing logic here
    " ...

    " Update status to completed
    update_payment_status(
      iv_payment_id = iv_payment_id
      iv_status    = 'COMP'
    ).
  ENDMETHOD.

  METHOD cancel_payment.
    " Read payment data
    SELECT SINGLE status
      FROM z_trm_payment_trans
      WHERE payment_id = @iv_payment_id
      INTO @DATA(lv_status).

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_tfm_payment_error
        MESSAGE ID 'TFM_PAYMENT'
        NUMBER '002'
        WITH iv_payment_id.
    ENDIF.

    " Check if payment can be cancelled
    IF lv_status = 'COMP' OR lv_status = 'FAIL'.
      RAISE EXCEPTION TYPE cx_tfm_payment_error
        MESSAGE ID 'TFM_PAYMENT'
        NUMBER '004'
        WITH iv_payment_id lv_status.
    ENDIF.

    " Update status to failed
    update_payment_status(
      iv_payment_id = iv_payment_id
      iv_status    = 'FAIL'
    ).
  ENDMETHOD.

  METHOD validate_payment_data.
    " Check mandatory fields
    IF is_payment_data-amount <= 0.
      RAISE EXCEPTION TYPE cx_tfm_payment_error
        MESSAGE ID 'TFM_PAYMENT'
        NUMBER '005'
        WITH is_payment_data-amount.
    ENDIF.

    " Validate currency
    SELECT SINGLE @abap_true
      FROM tcurc
      WHERE waers = @is_payment_data-currency
      INTO @DATA(lv_currency_valid).

    IF lv_currency_valid <> abap_true.
      RAISE EXCEPTION TYPE cx_tfm_payment_error
        MESSAGE ID 'TFM_PAYMENT'
        NUMBER '006'
        WITH is_payment_data-currency.
    ENDIF.

    " Validate payment type
    " Add more validation as needed
  ENDMETHOD.

  METHOD update_payment_status.
    UPDATE z_trm_payment_trans
      SET status = @iv_status
      WHERE payment_id = @iv_payment_id.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_tfm_payment_error
        MESSAGE ID 'TFM_PAYMENT'
        NUMBER '007'
        WITH iv_payment_id iv_status.
    ENDIF.
  ENDMETHOD.

ENDCLASS.