CLASS zbp_trm_risk_position DEFINITION
  PUBLIC
  ABSTRACT
  FINAL
  FOR BEHAVIOR OF z_trm_risk_position_cds.

  PUBLIC SECTION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS zbp_trm_risk_position IMPLEMENTATION.

  METHOD validatePosition.
    " Read positions to be validated
    READ ENTITIES OF z_trm_risk_position_cds IN LOCAL MODE
      ENTITY RiskPosition
        FIELDS ( PositionID CompanyCode RiskType PositionDate )
        WITH CORRESPONDING #( keys )
      RESULT DATA(positions).

    " Check for errors
    LOOP AT positions INTO DATA(position).
      " Validate Position ID format
      IF NOT position-PositionID CP 'POS*'.
        APPEND VALUE #( %tky = position-%tky ) TO failed-riskposition.
        APPEND VALUE #( %tky = position-%tky
                       %msg = new_message_with_text(
                         severity = if_abap_behv_message=>severity-error
                         text     = 'Position ID must start with POS' )
                     ) TO reported-riskposition.
      ENDIF.

      " Validate date is not in the future
      IF position-PositionDate > cl_abap_context_info=>get_system_date( ).
        APPEND VALUE #( %tky = position-%tky ) TO failed-riskposition.
        APPEND VALUE #( %tky = position-%tky
                       %msg = new_message_with_text(
                         severity = if_abap_behv_message=>severity-error
                         text     = 'Position date cannot be in the future' )
                     ) TO reported-riskposition.
      ENDIF.

      " Validate risk type
      IF position-RiskType NP 'MKT' AND
         position-RiskType NP 'CRD' AND
         position-RiskType NP 'LIQ'.
        APPEND VALUE #( %tky = position-%tky ) TO failed-riskposition.
        APPEND VALUE #( %tky = position-%tky
                       %msg = new_message_with_text(
                         severity = if_abap_behv_message=>severity-error
                         text     = 'Invalid risk type' )
                     ) TO reported-riskposition.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD validateAmount.
    " Read positions to validate amounts
    READ ENTITIES OF z_trm_risk_position_cds IN LOCAL MODE
      ENTITY RiskPosition
        FIELDS ( PositionAmount PositionCurrency )
        WITH CORRESPONDING #( keys )
      RESULT DATA(positions).

    " Check for errors
    LOOP AT positions INTO DATA(position).
      " Amount must be positive
      IF position-PositionAmount <= 0.
        APPEND VALUE #( %tky = position-%tky ) TO failed-riskposition.
        APPEND VALUE #( %tky = position-%tky
                       %msg = new_message_with_text(
                         severity = if_abap_behv_message=>severity-error
                         text     = 'Position amount must be positive' )
                     ) TO reported-riskposition.
      ENDIF.

      " Currency must be valid
      SELECT SINGLE @abap_true
        FROM I_Currency
        WHERE Currency = @position-PositionCurrency
        INTO @DATA(currency_exists).

      IF currency_exists = abap_false.
        APPEND VALUE #( %tky = position-%tky ) TO failed-riskposition.
        APPEND VALUE #( %tky = position-%tky
                       %msg = new_message_with_text(
                         severity = if_abap_behv_message=>severity-error
                         text     = 'Invalid currency code' )
                     ) TO reported-riskposition.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD calculateRiskAmount.
    " Read positions to calculate risk
    READ ENTITIES OF z_trm_risk_position_cds IN LOCAL MODE
      ENTITY RiskPosition
        FIELDS ( PositionAmount RiskType ConfidenceLevel TimeHorizon )
        WITH CORRESPONDING #( keys )
      RESULT DATA(positions).

    LOOP AT positions INTO DATA(position).
      " Calculate risk amount based on risk type
      CASE position-RiskType.
        WHEN 'MKT'.
          " Market Risk - Using VaR calculation
          DATA(risk_amount) = position-PositionAmount *
                             SQRT( position-TimeHorizon ) *
                             ( 1 - position-ConfidenceLevel ).

        WHEN 'CRD'.
          " Credit Risk - Using exposure calculation
          risk_amount = position-PositionAmount *
                       ( 1 - get_credit_rating_factor( position-CounterpartyID ) ).

        WHEN 'LIQ'.
          " Liquidity Risk - Using liquidity factor
          risk_amount = position-PositionAmount *
                       get_liquidity_factor( position-PositionCurrency ).

        WHEN OTHERS.
          " Default calculation
          risk_amount = position-PositionAmount * '0.1'.
      ENDCASE.

      " Update risk amount
      MODIFY ENTITIES OF z_trm_risk_position_cds IN LOCAL MODE
        ENTITY RiskPosition
          UPDATE FIELDS ( RiskAmount )
          WITH VALUE #( ( %tky    = position-%tky
                         RiskAmount = risk_amount ) ).
    ENDLOOP.
  ENDMETHOD.

  METHOD checkLimitBreach.
    " Read positions to check limits
    READ ENTITIES OF z_trm_risk_position_cds IN LOCAL MODE
      ENTITY RiskPosition
        FIELDS ( CounterpartyID RiskAmount RiskType )
        WITH CORRESPONDING #( keys )
      RESULT DATA(positions).

    " Get risk limits
    SELECT counterparty_id,
           risk_type,
           limit_amount
      FROM z_trm_risk_limit
      FOR ALL ENTRIES IN @positions
      WHERE counterparty_id = @positions-CounterpartyID
        AND risk_type = @positions-RiskType
      INTO TABLE @DATA(limits).

    " Check for limit breaches
    LOOP AT positions INTO DATA(position).
      READ TABLE limits INTO DATA(limit)
        WITH KEY counterparty_id = position-CounterpartyID
                 risk_type = position-RiskType.

      IF sy-subrc = 0 AND position-RiskAmount > limit-limit_amount.
        " Generate alert for limit breach
        generateAlert( VALUE #( %tky = position-%tky
                              alert_type = 'LIMIT_BREACH'
                              alert_text = |Risk amount exceeds limit for { position-CounterpartyID }| ) ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD generateAlert.
    " Create alert record
    MODIFY ENTITIES OF z_trm_risk_alert_cds
      ENTITY RiskAlert
        CREATE FIELDS ( AlertType AlertText ReferenceID Status )
        WITH VALUE #( ( AlertType = alert_parameter-alert_type
                       AlertText = alert_parameter-alert_text
                       ReferenceID = alert_parameter-%param-PositionID
                       Status = 'NEW' ) ).

    " Send email notification
    DATA(lo_notification) = cl_trm_notification_factory=>create_notification( ).
    lo_notification->send_alert(
      iv_subject = 'Risk Position Alert'
      iv_text    = alert_parameter-alert_text
    ).
  ENDMETHOD.

ENDCLASS.