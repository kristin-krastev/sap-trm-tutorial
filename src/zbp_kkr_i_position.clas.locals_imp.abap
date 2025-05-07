CLASS lhc_Position DEFINITION INHERITING FROM cl_abap_behavior_handler.
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

      get_instance_features FOR INSTANCE FEATURES
        IMPORTING keys REQUEST requested_features FOR Position RESULT result.

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

    " Check if instruments exist
    SELECT FROM ztrmfinins
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
    " Read the cashflow data
    READ ENTITIES OF zkkr_i_position IN LOCAL MODE
      ENTITY Position BY \_Cashflow
        FIELDS ( Amount Currency ) WITH CORRESPONDING #( keys )
      RESULT DATA(cashflows).

    LOOP AT cashflows INTO DATA(cashflow).
      " Check if amount is positive
      IF cashflow-Amount <= 0.
        APPEND VALUE #( %tky = cashflow-%tky ) TO failed-position.
        APPEND VALUE #( %tky = cashflow-%tky
                       %msg = new_message_with_text(
                         severity = if_abap_behv_message=>severity-error
                         text     = 'Amount must be greater than zero' )
                       %element-Amount = if_abap_behv=>mk-on ) TO reported-position.
      ENDIF.

      " Check if currency is filled
      IF cashflow-Currency IS INITIAL.
        APPEND VALUE #( %tky = cashflow-%tky ) TO failed-position.
        APPEND VALUE #( %tky = cashflow-%tky
                       %msg = new_message_with_text(
                         severity = if_abap_behv_message=>severity-error
                         text     = 'Currency is mandatory' )
                       %element-Currency = if_abap_behv=>mk-on ) TO reported-position.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD validateCashflowDate.
    " Read the cashflow data
    READ ENTITIES OF zkkr_i_position IN LOCAL MODE
      ENTITY Position BY \_Cashflow
        FIELDS ( ValueDate ) WITH CORRESPONDING #( keys )
      RESULT DATA(cashflows).

    LOOP AT cashflows INTO DATA(cashflow).
      " Check if value date is filled
      IF cashflow-ValueDate IS INITIAL.
        APPEND VALUE #( %tky = cashflow-%tky ) TO failed-position.
        APPEND VALUE #( %tky = cashflow-%tky
                       %msg = new_message_with_text(
                         severity = if_abap_behv_message=>severity-error
                         text     = 'Value date is mandatory' )
                       %element-ValueDate = if_abap_behv=>mk-on ) TO reported-position.
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
    SELECT SINGLE FROM ztrmpos
      FIELDS MAX( position_id ) AS last_id
      INTO @DATA(last_position_id).

    " Process positions without ID
    LOOP AT positions INTO DATA(position) WHERE PositionID IS INITIAL.
      " Generate new position ID
      DATA(new_position_id) = CONV string( last_position_id + 1 ).
      new_position_id = |POS{ new_position_id ALIGN = RIGHT PAD = '0' WIDTH = 10 }|.

      " Update position
      MODIFY ENTITIES OF zkkr_i_position IN LOCAL MODE
        ENTITY Position
          UPDATE FIELDS ( PositionID )
          WITH VALUE #( ( %tky = position-%tky
                         PositionID = new_position_id ) ).

      last_position_id = last_position_id + 1.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_instance_features.
    " Read the position data
    READ ENTITIES OF zkkr_i_position IN LOCAL MODE
      ENTITY Position
        FIELDS ( PositionID ) WITH CORRESPONDING #( keys )
      RESULT DATA(positions).

    " Set features per instance
    result = VALUE #( FOR position IN positions
                     ( %tky = position-%tky
                       %field-PositionID = if_abap_behv=>fc-f-read_only ) ).
  ENDMETHOD.

ENDCLASS.