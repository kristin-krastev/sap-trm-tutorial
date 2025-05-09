CLASS ltcl_position DEFINITION DEFERRED FOR TESTING.
CLASS lhc_position DEFINITION INHERITING FROM cl_abap_behavior_handler FRIENDS ltcl_position.
  PRIVATE SECTION.
    METHODS:
      validateDates FOR VALIDATE ON SAVE
        IMPORTING keys FOR Position~validateDates,

      validateInstrument FOR VALIDATE ON SAVE
        IMPORTING keys FOR Position~validateInstrument,

      validateCashflowAmount FOR VALIDATE ON SAVE
        IMPORTING keys FOR Position~validateCashflowAmount,

      validateCashflowDate FOR VALIDATE ON SAVE
        IMPORTING keys FOR Position~validateCashflowDate,

      calculatePositionID FOR DETERMINE ON SAVE
        IMPORTING keys FOR Position~calculatePositionID,

      calculateCashflowID FOR DETERMINE ON SAVE
        IMPORTING keys FOR Cashflow~calculateCashflowID,

      get_instance_authorizations FOR INSTANCE AUTHORIZATION
        IMPORTING keys REQUEST requested_authorizations FOR Position RESULT result.

ENDCLASS.

CLASS lhc_Position IMPLEMENTATION.

  METHOD validateDates.
    " Read the position data
    READ ENTITIES OF zkkr_i_position IN LOCAL MODE
      ENTITY Position
        FIELDS ( ValidFrom ValidTo ) WITH CORRESPONDING #( keys )
      RESULT DATA(positions).

    LOOP AT positions INTO DATA(position).
      " Check if ValidFrom is filled
      IF position-ValidFrom IS INITIAL.
        APPEND VALUE #( %tky = position-%tky ) TO failed-position.
        APPEND VALUE #( %tky = position-%tky
                       %msg = new_message_with_text(
                         severity = if_abap_behv_message=>severity-error
                         text     = 'Valid-from date is mandatory' )
                       %element-ValidFrom = if_abap_behv=>mk-on ) TO reported-position.
        CONTINUE.
      ENDIF.

      " Check if ValidTo is filled and after ValidFrom
      IF position-ValidTo IS NOT INITIAL AND position-ValidTo < position-ValidFrom.
        APPEND VALUE #( %tky = position-%tky ) TO failed-position.
        APPEND VALUE #( %tky = position-%tky
                       %msg = new_message_with_text(
                         severity = if_abap_behv_message=>severity-error
                         text     = 'Valid-to date must be after valid-from date' )
                       %element-ValidTo = if_abap_behv=>mk-on ) TO reported-position.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD validateInstrument.
    " Read the position data
    READ ENTITIES OF zkkr_i_position IN LOCAL MODE
      ENTITY Position
        FIELDS ( InstrumentID ) WITH CORRESPONDING #( keys )
      RESULT DATA(positions).

    " Check if instruments exist - using the correct table name ZTRMINST
    SELECT FROM ztrminst
      FIELDS instrument_id
      FOR ALL ENTRIES IN @positions
      WHERE instrument_id = @positions-InstrumentID
      INTO TABLE @DATA(valid_instruments).

    LOOP AT positions INTO DATA(position).
      " Check if instrument exists
      IF NOT line_exists( valid_instruments[ instrument_id = position-InstrumentID ] ).
        APPEND VALUE #( %tky = position-%tky ) TO failed-position.
        APPEND VALUE #( %tky = position-%tky
                       %msg = new_message_with_text(
                         severity = if_abap_behv_message=>severity-error
                         text     = |Instrument { position-InstrumentID } does not exist| )
                       %element-InstrumentID = if_abap_behv=>mk-on ) TO reported-position.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD validateCashflowAmount.
    " Read the cashflow data directly
    READ ENTITIES OF zkkr_i_position IN LOCAL MODE
      ENTITY Cashflow
        FIELDS ( Amount Currency ) WITH CORRESPONDING #( keys )
      RESULT DATA(cashflows).

    LOOP AT cashflows INTO DATA(cashflow).
      IF cashflow-Amount <= 0.
        APPEND VALUE #( %tky = cashflow-%tky ) TO failed-cashflow.
        APPEND VALUE #( %tky = cashflow-%tky
                       %msg = new_message_with_text(
                         severity = if_abap_behv_message=>severity-error
                         text     = 'Amount must be greater than zero' )
                       %element-Amount = if_abap_behv=>mk-on ) TO reported-cashflow.
      ENDIF.

      IF cashflow-Currency IS INITIAL.
        APPEND VALUE #( %tky = cashflow-%tky ) TO failed-cashflow.
        APPEND VALUE #( %tky = cashflow-%tky
                       %msg = new_message_with_text(
                         severity = if_abap_behv_message=>severity-error
                         text     = 'Currency is mandatory' )
                       %element-Currency = if_abap_behv=>mk-on ) TO reported-cashflow.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD validateCashflowDate.
    " Read the cashflow data directly using the association
    READ ENTITIES OF zkkr_i_position IN LOCAL MODE
      ENTITY Cashflow
        FIELDS ( ValueDate ) WITH CORRESPONDING #( keys )
      RESULT DATA(cashflows).

    LOOP AT cashflows INTO DATA(cashflow).
      IF cashflow-ValueDate IS INITIAL.
        APPEND VALUE #( %tky = cashflow-%tky ) TO failed-cashflow.  " Changed to failed-cashflow
        APPEND VALUE #( %tky = cashflow-%tky
                       %msg = new_message_with_text(
                         severity = if_abap_behv_message=>severity-error
                         text     = 'Value date is mandatory' )
                       %element-ValueDate = if_abap_behv=>mk-on ) TO reported-cashflow.  " Changed to reported-cashflow
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD calculatePositionID.
    " Read the position data
    READ ENTITIES OF zkkr_i_position IN LOCAL MODE
      ENTITY Position
        FIELDS ( PositionID ) WITH CORRESPONDING #( keys )
      RESULT DATA(positions).

    " Get last used position number
    SELECT SINGLE
      FROM ztrmpos
      FIELDS MAX( position_id ) AS last_id
      INTO @DATA(last_position_id).

    " Handle initial value
    IF last_position_id IS INITIAL.
      last_position_id = 0.
    ENDIF.

    " Read positions that need ID assignment
    READ ENTITIES OF zkkr_i_position IN LOCAL MODE
      ENTITY Position
        FIELDS ( PositionID )
        WITH CORRESPONDING #( keys )
        RESULT DATA(positions_to_update)
        FAILED DATA(read_failed).

    DATA updates TYPE TABLE FOR UPDATE zkkr_i_position\\Position.

    " Process positions without ID
    LOOP AT positions_to_update REFERENCE INTO DATA(position_ref).
      IF position_ref->PositionID IS NOT INITIAL.
        CONTINUE.
      ENDIF.

      " Generate new position ID
      last_position_id += 1.
      DATA(new_position_id) = |POS{ last_position_id ALIGN = RIGHT PAD = '0' WIDTH = 10 }|.

      " Collect updates
      APPEND VALUE #( %tky = position_ref->%tky
                     PositionID = new_position_id ) TO updates.
    ENDLOOP.

    " Update positions
    MODIFY ENTITIES OF zkkr_i_position IN LOCAL MODE
      ENTITY Position UPDATE FROM updates.
  ENDMETHOD.

  METHOD calculateCashflowID.
    " Read the cashflow data
    READ ENTITIES OF zkkr_i_position IN LOCAL MODE
      ENTITY Cashflow
        FIELDS ( CashflowID ) WITH CORRESPONDING #( keys )
      RESULT DATA(cashflows).

    " Get last used cashflow number
    SELECT SINGLE
      FROM ztrmcf
      FIELDS MAX( cashflow_id ) AS last_id
      INTO @DATA(last_cashflow_id).

    " Handle initial value
    IF last_cashflow_id IS INITIAL.
      last_cashflow_id = 0.
    ENDIF.

    " Read cashflows that need ID assignment
    READ ENTITIES OF zkkr_i_position IN LOCAL MODE
      ENTITY Cashflow
        FIELDS ( CashflowID )
        WITH CORRESPONDING #( keys )
        RESULT DATA(cashflows_to_update)
        FAILED DATA(read_failed).

    DATA updates TYPE TABLE FOR UPDATE zkkr_i_position\\Cashflow.

    " Process cashflows without ID
    LOOP AT cashflows_to_update REFERENCE INTO DATA(cashflow_ref).
      IF cashflow_ref->CashflowID IS NOT INITIAL.
        CONTINUE.
      ENDIF.

      " Generate new cashflow ID
      last_cashflow_id += 1.
      DATA(new_cashflow_id) = |CF{ last_cashflow_id ALIGN = RIGHT PAD = '0' WIDTH = 10 }|.

      " Collect updates
      APPEND VALUE #( %tky = cashflow_ref->%tky
                     CashflowID = new_cashflow_id ) TO updates.
    ENDLOOP.

    " Update cashflows
    MODIFY ENTITIES OF zkkr_i_position IN LOCAL MODE
      ENTITY Cashflow UPDATE FROM updates.
  ENDMETHOD.

  METHOD get_instance_authorizations.
    " Set default authorizations
    LOOP AT keys ASSIGNING FIELD-SYMBOL(<key>).
      APPEND VALUE #( %tky = <key>-%tky
                     %update = if_abap_behv=>auth-allowed
                     %delete = if_abap_behv=>auth-allowed
                     %action-Edit = if_abap_behv=>auth-allowed ) TO result.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.