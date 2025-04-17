CLASS zcl_trm_matching_handler DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES:
      BEGIN OF ty_match_result,
        statement_id    TYPE tfm_stmt_id,
        payment_id     TYPE tfm_payment_id,
        match_rule_id  TYPE tfm_rule_id,
        match_score    TYPE int4,
        matched_amount TYPE tfm_amount,
        matched_date   TYPE datum,
      END OF ty_match_result,

      tt_match_results TYPE STANDARD TABLE OF ty_match_result WITH KEY statement_id payment_id.

    METHODS:
      constructor,

      match_statement
        IMPORTING
          iv_statement_id TYPE tfm_stmt_id
        RETURNING
          VALUE(rt_matches) TYPE tt_match_results
        RAISING
          cx_tfm_matching_error,

      apply_matching_rules
        IMPORTING
          iv_statement_id TYPE tfm_stmt_id
          iv_payment_id  TYPE tfm_payment_id
        RETURNING
          VALUE(rv_match_score) TYPE int4
        RAISING
          cx_tfm_matching_error,

      confirm_match
        IMPORTING
          is_match TYPE ty_match_result
        RAISING
          cx_tfm_matching_error.

  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS:
      get_active_rules
        RETURNING
          VALUE(rt_rules) TYPE STANDARD TABLE OF z_trm_matching_rules,

      check_amount_tolerance
        IMPORTING
          iv_amount1          TYPE tfm_amount
          iv_amount2          TYPE tfm_amount
          iv_tolerance_amount TYPE tfm_amount
        RETURNING
          VALUE(rv_within_tolerance) TYPE abap_bool,

      check_date_tolerance
        IMPORTING
          iv_date1          TYPE datum
          iv_date2          TYPE datum
          iv_tolerance_days TYPE int4
        RETURNING
          VALUE(rv_within_tolerance) TYPE abap_bool.
ENDCLASS.

CLASS zcl_trm_matching_handler IMPLEMENTATION.

  METHOD constructor.
    " Initialize any necessary resources
  ENDMETHOD.

  METHOD match_statement.
    " Get statement details
    SELECT SINGLE *
      FROM z_trm_statement_recon
      WHERE statement_id = @iv_statement_id
      INTO @DATA(ls_statement).

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_tfm_matching_error
        MESSAGE ID 'TFM_MATCH'
        NUMBER '001'
        WITH iv_statement_id.
    ENDIF.

    " Get unmatched payments within date range
    DATA(lv_date_from) = ls_statement-statement_date - 30.  " Configurable range
    DATA(lv_date_to) = ls_statement-statement_date + 30.

    SELECT *
      FROM z_trm_payment_trans
      WHERE payment_date BETWEEN @lv_date_from AND @lv_date_to
        AND status = 'COMP'  " Only completed payments
      INTO TABLE @DATA(lt_payments).

    " Apply matching rules to each payment
    LOOP AT lt_payments INTO DATA(ls_payment).
      DATA(lv_match_score) = apply_matching_rules(
        iv_statement_id = iv_statement_id
        iv_payment_id  = ls_payment-payment_id
      ).

      " If score above threshold, add to matches
      IF lv_match_score >= 80.  " Configurable threshold
        APPEND VALUE #(
          statement_id    = iv_statement_id
          payment_id     = ls_payment-payment_id
          match_score    = lv_match_score
          matched_amount = ls_payment-amount
          matched_date   = ls_payment-payment_date
        ) TO rt_matches.
      ENDIF.
    ENDLOOP.

    " Sort matches by score descending
    SORT rt_matches BY match_score DESCENDING.
  ENDMETHOD.

  METHOD apply_matching_rules.
    DATA(lt_rules) = get_active_rules( ).
    DATA(lv_total_score) = 0.
    DATA(lv_max_score) = 100.

    " Get statement and payment details
    SELECT SINGLE *
      FROM z_trm_statement_recon
      WHERE statement_id = @iv_statement_id
      INTO @DATA(ls_statement).

    SELECT SINGLE *
      FROM z_trm_payment_trans
      WHERE payment_id = @iv_payment_id
      INTO @DATA(ls_payment).

    " Apply each rule
    LOOP AT lt_rules INTO DATA(ls_rule).
      CASE ls_rule-match_type.
        WHEN 'EXACT'.
          " Exact amount match
          IF ls_statement-closing_balance = ls_payment-amount.
            lv_total_score = lv_total_score + 50.
          ENDIF.

        WHEN 'FUZZY'.
          " Check amount within tolerance
          IF check_amount_tolerance(
               iv_amount1 = ls_statement-closing_balance
               iv_amount2 = ls_payment-amount
               iv_tolerance_amount = ls_rule-tolerance_amount
             ).
            lv_total_score = lv_total_score + 30.
          ENDIF.

          " Check date within tolerance
          IF check_date_tolerance(
               iv_date1 = ls_statement-statement_date
               iv_date2 = ls_payment-payment_date
               iv_tolerance_days = ls_rule-tolerance_days
             ).
            lv_total_score = lv_total_score + 20.
          ENDIF.

        WHEN 'RANGE'.
          " Implement range matching logic
          " ...
      ENDCASE.
    ENDLOOP.

    rv_match_score = lv_total_score.
  ENDMETHOD.

  METHOD confirm_match.
    " Update statement with match information
    UPDATE z_trm_statement_recon
      SET matched_payment_id = @is_match-payment_id,
          match_rule_id = @is_match-match_rule_id,
          match_date = @sy-datum
      WHERE statement_id = @is_match-statement_id.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_tfm_matching_error
        MESSAGE ID 'TFM_MATCH'
        NUMBER '002'
        WITH is_match-statement_id.
    ENDIF.
  ENDMETHOD.

  METHOD get_active_rules.
    SELECT *
      FROM z_trm_matching_rules
      WHERE active = @abap_true
      ORDER BY priority
      INTO TABLE @rt_rules.
  ENDMETHOD.

  METHOD check_amount_tolerance.
    DATA(lv_difference) = abs( iv_amount1 - iv_amount2 ).
    rv_within_tolerance = xsdbool( lv_difference <= iv_tolerance_amount ).
  ENDMETHOD.

  METHOD check_date_tolerance.
    DATA(lv_days_diff) = abs( iv_date1 - iv_date2 ).
    rv_within_tolerance = xsdbool( lv_days_diff <= iv_tolerance_days ).
  ENDMETHOD.

ENDCLASS.