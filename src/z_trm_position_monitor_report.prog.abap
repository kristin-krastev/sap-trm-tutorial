*&---------------------------------------------------------------------*
*& Report Z_TRM_POSITION_MONITOR_REPORT
*& Daily Risk Position Monitoring Report
*&---------------------------------------------------------------------*
REPORT z_trm_position_monitor_report.

************************************************************************
* Selection Screen
************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  PARAMETERS: p_date   TYPE datum DEFAULT sy-datum,
             p_ccode  TYPE bukrs,
             p_rtype  TYPE tfm_risk_type AS LISTBOX
                      VISIBLE LENGTH 20,
             p_alert  TYPE abap_bool AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK b1.

************************************************************************
* Global Data Declarations
************************************************************************
TYPES: BEGIN OF ty_position_summary,
         risk_type        TYPE tfm_risk_type,
         position_count   TYPE i,
         total_amount     TYPE tfm_amount,
         total_risk       TYPE tfm_amount,
         currency         TYPE waers,
         limit_breaches   TYPE i,
       END OF ty_position_summary.

DATA: gt_summary TYPE STANDARD TABLE OF ty_position_summary,
      go_salv    TYPE REF TO cl_salv_table.

************************************************************************
* Main Processing
************************************************************************
START-OF-SELECTION.
  PERFORM get_position_data.
  PERFORM calculate_summary.
  PERFORM display_report.

************************************************************************
* Form Routines
************************************************************************
FORM get_position_data.
  " Get all positions for the selected date and company code
  SELECT FROM z_trm_risk_position_cds
    FIELDS RiskType,
           COUNT(*) AS position_count,
           SUM( PositionAmount ) AS total_amount,
           SUM( RiskAmount ) AS total_risk,
           PositionCurrency AS currency
    WHERE PositionDate = @p_date
      AND CompanyCode = @p_ccode
      AND RiskType = @p_rtype
    GROUP BY RiskType, PositionCurrency
    INTO CORRESPONDING FIELDS OF TABLE @gt_summary.

  " Get limit breach count
  LOOP AT gt_summary ASSIGNING FIELD-SYMBOL(<summary>).
    SELECT COUNT(*)
      FROM z_trm_risk_position_cds AS pos
      INNER JOIN z_trm_risk_limit AS lim
        ON pos~CounterpartyID = lim~counterparty_id
       AND pos~RiskType = lim~risk_type
      WHERE pos~PositionDate = @p_date
        AND pos~CompanyCode = @p_ccode
        AND pos~RiskType = @<summary>-risk_type
        AND pos~RiskAmount > lim~limit_amount
      INTO @<summary>-limit_breaches.
  ENDLOOP.
ENDFORM.

FORM calculate_summary.
  " Additional risk metrics could be calculated here
  " For example: VaR aggregation, correlation effects, etc.
ENDFORM.

FORM display_report.
  TRY.
      " Create ALV grid
      cl_salv_table=>factory(
        IMPORTING
          r_salv_table = go_salv
        CHANGING
          t_table      = gt_summary ).

      " Configure ALV display
      DATA(lo_functions) = go_salv->get_functions( ).
      lo_functions->set_all( abap_true ).

      DATA(lo_columns) = go_salv->get_columns( ).
      lo_columns->set_optimize( abap_true ).

      " Set column titles
      PERFORM set_column_titles.

      " Display the report
      go_salv->display( ).

      " Generate alerts if requested
      IF p_alert = abap_true.
        PERFORM generate_alerts.
      ENDIF.

    CATCH cx_salv_msg INTO DATA(lx_error).
      MESSAGE lx_error TYPE 'E'.
  ENDTRY.
ENDFORM.

FORM set_column_titles.
  DATA(lo_columns) = go_salv->get_columns( ).

  TRY.
      DATA(lo_column) = CAST cl_salv_column_table(
        lo_columns->get_column( 'RISK_TYPE' ) ).
      lo_column->set_short_text( 'Risk Type' ).
      lo_column->set_medium_text( 'Risk Type' ).
      lo_column->set_long_text( 'Risk Type' ).

      lo_column = CAST cl_salv_column_table(
        lo_columns->get_column( 'POSITION_COUNT' ) ).
      lo_column->set_short_text( 'Pos Cnt' ).
      lo_column->set_medium_text( 'Position Count' ).
      lo_column->set_long_text( 'Position Count' ).

      lo_column = CAST cl_salv_column_table(
        lo_columns->get_column( 'TOTAL_AMOUNT' ) ).
      lo_column->set_short_text( 'Total Amt' ).
      lo_column->set_medium_text( 'Total Amount' ).
      lo_column->set_long_text( 'Total Position Amount' ).

      lo_column = CAST cl_salv_column_table(
        lo_columns->get_column( 'TOTAL_RISK' ) ).
      lo_column->set_short_text( 'Total Risk' ).
      lo_column->set_medium_text( 'Total Risk' ).
      lo_column->set_long_text( 'Total Risk Amount' ).

      lo_column = CAST cl_salv_column_table(
        lo_columns->get_column( 'LIMIT_BREACHES' ) ).
      lo_column->set_short_text( 'Breaches' ).
      lo_column->set_medium_text( 'Limit Breaches' ).
      lo_column->set_long_text( 'Number of Limit Breaches' ).

    CATCH cx_salv_not_found.
      " Handle column not found
  ENDTRY.
ENDFORM.

FORM generate_alerts.
  " Generate alerts for limit breaches
  LOOP AT gt_summary INTO DATA(ls_summary) WHERE limit_breaches > 0.
    DATA(lo_notification) = cl_trm_notification_factory=>create_notification( ).

    lo_notification->send_alert(
      iv_subject = |Risk Position Alert: { ls_summary-risk_type }|
      iv_text    = |{ ls_summary-limit_breaches } limit breaches detected for { ls_summary-risk_type }| &&
                   | with total risk amount { ls_summary-total_risk } { ls_summary-currency }|
    ).
  ENDLOOP.
ENDFORM.