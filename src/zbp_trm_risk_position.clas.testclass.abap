*"* use this source file for your ABAP unit test classes
CLASS ltcl_risk_position DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    CLASS-DATA:
      mo_osql_environment TYPE REF TO if_osql_test_environment.

    CLASS-METHODS:
      class_setup,
      class_teardown.

    METHODS:
      setup,
      teardown,

      " Test methods for Position validations
      test_dates_valid FOR TESTING RAISING cx_static_check,
      test_dates_no_from FOR TESTING RAISING cx_static_check,
      test_dates_invalid FOR TESTING RAISING cx_static_check,
      test_position_valid FOR TESTING RAISING cx_static_check,
      test_position_invalid_id FOR TESTING RAISING cx_static_check,
      test_position_future FOR TESTING RAISING cx_static_check,
      test_position_invalid_type FOR TESTING RAISING cx_static_check,

      " Test methods for Cashflow validations
      test_cf_amount_valid FOR TESTING RAISING cx_static_check,
      test_cf_amount_negative FOR TESTING RAISING cx_static_check,
      test_cf_date_valid FOR TESTING RAISING cx_static_check,
      test_cf_date_missing FOR TESTING RAISING cx_static_check,
      test_cf_currency FOR TESTING RAISING cx_static_check,

      " Test methods for ID determinations
      test_determine_pos_id FOR TESTING RAISING cx_static_check,
      test_determine_cf_id FOR TESTING RAISING cx_static_check,

      " Test methods for Amount validation
      test_amount_valid FOR TESTING RAISING cx_static_check,
      test_amount_negative FOR TESTING RAISING cx_static_check,
      test_amount_invalid_curr FOR TESTING RAISING cx_static_check,

      " Test methods for Risk calculation
      test_calc_market_risk FOR TESTING RAISING cx_static_check,
      test_calc_credit_risk FOR TESTING RAISING cx_static_check,
      test_calc_liquid_risk FOR TESTING RAISING cx_static_check,

      " Test methods for Limit checks
      test_limit_no_breach FOR TESTING RAISING cx_static_check,
      test_limit_with_breach FOR TESTING RAISING cx_static_check,

      " Test methods for draft handling
      test_create_draft FOR TESTING RAISING cx_static_check,
      test_activate_draft FOR TESTING RAISING cx_static_check,
      test_discard_draft FOR TESTING RAISING cx_static_check,

      " Test methods for associations
      test_create_pos_with_cf FOR TESTING RAISING cx_static_check,
      test_add_cf_to_position FOR TESTING RAISING cx_static_check,

      " Test methods for authorization
      test_position_auth FOR TESTING RAISING cx_static_check.

    DATA:
      cut TYPE REF TO zbp_trm_risk_position.

ENDCLASS.

CLASS ltcl_risk_position IMPLEMENTATION.

  METHOD class_setup.
    " Create OSQL test environment
    mo_osql_environment = cl_osql_test_environment=>create(
        i_dependency_list = VALUE #(
          ( 'Z_TRM_RISK_POSITION' )
          ( 'Z_TRM_RISK_LIMIT' )
          ( 'Z_TRM_POSITION_DRAFT' )
          ( 'Z_TRM_CASHFLOW' ) ) ).
  ENDMETHOD.

  METHOD class_teardown.
    mo_osql_environment->destroy( ).
  ENDMETHOD.

  METHOD setup.
    " Prepare test data
    mo_osql_environment->clear_doubles( ).

    " Insert test position
    mo_osql_environment->insert_test_data(
      i_data = VALUE z_trm_risk_position_tab( (
        position_uuid = '1234567890'
        position_id = 'POS001'
        company_code = '1000'
        risk_type = 'MKT'
        position_amount = '100000.00'
        position_currency = 'EUR'
        position_date = sy-datum
        confidence_level = '0.99'
        time_horizon = 10
      ) ) ).

    " Insert test limit data
    mo_osql_environment->insert_test_data(
      i_data = VALUE z_trm_risk_limit_tab( (
        limit_uuid = '9876543210'
        limit_type = 'MKT'
        object_id = 'BP001'
        limit_amount = '1000000.00'
        limit_currency = 'EUR'
        valid_from = sy-datum
        valid_to = '99991231'
      ) ) ).

    cut = NEW #( ).
  ENDMETHOD.

  METHOD teardown.
    mo_osql_environment->clear_doubles( ).
  ENDMETHOD.

  METHOD test_dates_valid.
    " Given
    DATA(ls_position) = VALUE z_trm_risk_position(
      position_id = 'POS001'
      valid_from = sy-datum
      valid_to   = sy-datum + 30
    ).

    " When
    cut->validateDates(
      EXPORTING
        keys = VALUE #( ( %key = ls_position ) )
    ).

    " Then
    cl_abap_unit_assert=>assert_initial(
      act = failed-riskposition
      msg = 'Date validation should succeed for valid date range' ).
  ENDMETHOD.

  METHOD test_dates_no_from.
    " Given
    DATA(ls_position) = VALUE z_trm_risk_position(
      position_id = 'POS001'
      valid_to   = sy-datum + 30
    ).

    " When
    cut->validateDates(
      EXPORTING
        keys = VALUE #( ( %key = ls_position ) )
    ).

    " Then
    cl_abap_unit_assert=>assert_not_initial(
      act = failed-riskposition
      msg = 'Date validation should fail when valid_from is missing' ).
  ENDMETHOD.

  METHOD test_dates_invalid.
    " Given
    DATA(ls_position) = VALUE z_trm_risk_position(
      position_id = 'POS001'
      valid_from = sy-datum + 30
      valid_to   = sy-datum
    ).

    " When
    cut->validateDates(
      EXPORTING
        keys = VALUE #( ( %key = ls_position ) )
    ).

    " Then
    cl_abap_unit_assert=>assert_not_initial(
      act = failed-riskposition
      msg = 'Date validation should fail when valid_to is before valid_from' ).
  ENDMETHOD.

  METHOD test_position_valid.
    " Given
    DATA(ls_position) = VALUE z_trm_risk_position(
      position_id = 'POS001'
      company_code = '1000'
      risk_type = 'MKT'
      position_date = sy-datum
    ).

    " When
    cut->validatePosition(
      EXPORTING
        keys = VALUE #( ( %key = ls_position ) )
    ).

    " Then
    cl_abap_unit_assert=>assert_initial(
      act = failed-riskposition
      msg = 'Validation should succeed for valid position' ).
  ENDMETHOD.

  METHOD test_position_invalid_id.
    " Given
    DATA(ls_position) = VALUE z_trm_risk_position(
      position_id = 'INVALID001'
      company_code = '1000'
      risk_type = 'MKT'
      position_date = sy-datum
    ).

    " When
    cut->validatePosition(
      EXPORTING
        keys = VALUE #( ( %key = ls_position ) )
    ).

    " Then
    cl_abap_unit_assert=>assert_not_initial(
      act = failed-riskposition
      msg = 'Validation should fail for invalid position ID' ).
  ENDMETHOD.

  METHOD test_position_future.
    " Given
    DATA(ls_position) = VALUE z_trm_risk_position(
      position_id = 'POS001'
      company_code = '1000'
      risk_type = 'MKT'
      position_date = sy-datum + 1
    ).

    " When
    cut->validatePosition(
      EXPORTING
        keys = VALUE #( ( %key = ls_position ) )
    ).

    " Then
    cl_abap_unit_assert=>assert_not_initial(
      act = failed-riskposition
      msg = 'Validation should fail for future date' ).
  ENDMETHOD.

  METHOD test_position_invalid_type.
    " Given
    DATA(ls_position) = VALUE z_trm_risk_position(
      position_id = 'POS001'
      company_code = '1000'
      risk_type = 'INVALID'
      position_date = sy-datum
    ).

    " When
    cut->validatePosition(
      EXPORTING
        keys = VALUE #( ( %key = ls_position ) )
    ).

    " Then
    cl_abap_unit_assert=>assert_not_initial(
      act = failed-riskposition
      msg = 'Validation should fail for invalid risk type' ).
  ENDMETHOD.

  METHOD test_cf_amount_valid.
    " Given
    DATA(ls_cashflow) = VALUE z_trm_cashflow(
      cashflow_id = 'CF001'
      amount = '1000.00'
      currency = 'EUR'
    ).

    " When
    cut->validateCashflowAmount(
      EXPORTING
        keys = VALUE #( ( %key = ls_cashflow ) )
    ).

    " Then
    cl_abap_unit_assert=>assert_initial(
      act = failed-cashflow
      msg = 'Cashflow amount validation should succeed for valid amount' ).
  ENDMETHOD.

  METHOD test_cf_amount_negative.
    " Given
    DATA(ls_cashflow) = VALUE z_trm_cashflow(
      cashflow_id = 'CF001'
      amount = '-1000.00'
      currency = 'EUR'
    ).

    " When
    cut->validateCashflowAmount(
      EXPORTING
        keys = VALUE #( ( %key = ls_cashflow ) )
    ).

    " Then
    cl_abap_unit_assert=>assert_not_initial(
      act = failed-cashflow
      msg = 'Cashflow amount validation should fail for negative amount' ).
  ENDMETHOD.

  METHOD test_cf_date_valid.
    " Given
    DATA(ls_cashflow) = VALUE z_trm_cashflow(
      cashflow_id = 'CF001'
      value_date = sy-datum + 10
    ).

    " When
    cut->validateCashflowDate(
      EXPORTING
        keys = VALUE #( ( %key = ls_cashflow ) )
    ).

    " Then
    cl_abap_unit_assert=>assert_initial(
      act = failed-cashflow
      msg = 'Cashflow date validation should succeed for valid date' ).
  ENDMETHOD.

  METHOD test_cf_date_missing.
    " Given
    DATA(ls_cashflow) = VALUE z_trm_cashflow(
      cashflow_id = 'CF001'
    ).

    " When
    cut->validateCashflowDate(
      EXPORTING
        keys = VALUE #( ( %key = ls_cashflow ) )
    ).

    " Then
    cl_abap_unit_assert=>assert_not_initial(
      act = failed-cashflow
      msg = 'Cashflow date validation should fail for missing date' ).
  ENDMETHOD.

  METHOD test_cf_currency.
    " Given
    DATA(ls_cashflow) = VALUE z_trm_cashflow(
      cashflow_id = 'CF001'
      currency = 'XXX'
    ).

    " When
    cut->validateCashflowCurrency(
      EXPORTING
        keys = VALUE #( ( %key = ls_cashflow ) )
    ).

    " Then
    cl_abap_unit_assert=>assert_not_initial(
      act = failed-cashflow
      msg = 'Cashflow currency validation should fail for invalid currency' ).
  ENDMETHOD.

  METHOD test_determine_pos_id.
    " Given
    DATA(ls_position) = VALUE z_trm_risk_position(
      company_code = '1000'
      risk_type = 'MKT'
    ).

    " When
    cut->determinePositionId(
      EXPORTING
        keys = VALUE #( ( %key = ls_position ) )
    ).

    " Then
    cl_abap_unit_assert=>assert_not_initial(
      act = ls_position-position_id
      msg = 'Position ID should be determined' ).
  ENDMETHOD.

  METHOD test_determine_cf_id.
    " Given
    DATA(ls_cashflow) = VALUE z_trm_cashflow(
      position_id = 'POS001'
    ).

    " When
    cut->determineCashflowId(
      EXPORTING
        keys = VALUE #( ( %key = ls_cashflow ) )
    ).

    " Then
    cl_abap_unit_assert=>assert_not_initial(
      act = ls_cashflow-cashflow_id
      msg = 'Cashflow ID should be determined' ).
  ENDMETHOD.

  METHOD test_amount_valid.
    " Given
    DATA(ls_position) = VALUE z_trm_risk_position(
      position_amount = '1000.00'
      position_currency = 'EUR'
    ).

    " When
    cut->validateAmount(
      EXPORTING
        keys = VALUE #( ( %key = ls_position ) )
    ).

    " Then
    cl_abap_unit_assert=>assert_initial(
      act = failed-riskposition
      msg = 'Validation should succeed for valid amount' ).
  ENDMETHOD.

  METHOD test_amount_negative.
    " Given
    DATA(ls_position) = VALUE z_trm_risk_position(
      position_amount = '-1000.00'
      position_currency = 'EUR'
    ).

    " When
    cut->validateAmount(
      EXPORTING
        keys = VALUE #( ( %key = ls_position ) )
    ).

    " Then
    cl_abap_unit_assert=>assert_not_initial(
      act = failed-riskposition
      msg = 'Validation should fail for negative amount' ).
  ENDMETHOD.

  METHOD test_amount_invalid_curr.
    " Given
    DATA(ls_position) = VALUE z_trm_risk_position(
      position_amount = '1000.00'
      position_currency = 'XXX'
    ).

    " When
    cut->validateAmount(
      EXPORTING
        keys = VALUE #( ( %key = ls_position ) )
    ).

    " Then
    cl_abap_unit_assert=>assert_not_initial(
      act = failed-riskposition
      msg = 'Validation should fail for invalid currency' ).
  ENDMETHOD.

  METHOD test_calc_market_risk.
    " Given
    DATA(ls_position) = VALUE z_trm_risk_position(
      position_amount = '100000.00'
      position_currency = 'EUR'
      risk_type = 'MKT'
      confidence_level = '0.99'
      time_horizon = 10
    ).

    " When
    cut->calculateRiskAmount(
      EXPORTING
        keys = VALUE #( ( %key = ls_position ) )
    ).

    " Then
    cl_abap_unit_assert=>assert_not_initial(
      act = ls_position-risk_amount
      msg = 'Market risk calculation should produce a value' ).
  ENDMETHOD.

  METHOD test_calc_credit_risk.
    " Given
    DATA(ls_position) = VALUE z_trm_risk_position(
      position_amount = '100000.00'
      position_currency = 'EUR'
      risk_type = 'CRD'
      confidence_level = '0.99'
      time_horizon = 10
    ).

    " When
    cut->calculateRiskAmount(
      EXPORTING
        keys = VALUE #( ( %key = ls_position ) )
    ).

    " Then
    cl_abap_unit_assert=>assert_not_initial(
      act = ls_position-risk_amount
      msg = 'Credit risk calculation should produce a value' ).
  ENDMETHOD.

  METHOD test_calc_liquid_risk.
    " Given
    DATA(ls_position) = VALUE z_trm_risk_position(
      position_amount = '100000.00'
      position_currency = 'EUR'
      risk_type = 'LIQ'
      confidence_level = '0.99'
      time_horizon = 10
    ).

    " When
    cut->calculateRiskAmount(
      EXPORTING
        keys = VALUE #( ( %key = ls_position ) )
    ).

    " Then
    cl_abap_unit_assert=>assert_not_initial(
      act = ls_position-risk_amount
      msg = 'Liquidity risk calculation should produce a value' ).
  ENDMETHOD.

  METHOD test_limit_no_breach.
    " Given
    DATA(ls_position) = VALUE z_trm_risk_position(
      position_amount = '100000.00'
      position_currency = 'EUR'
      risk_type = 'MKT'
      counterparty_id = 'BP001'
    ).

    " When
    cut->checkLimitBreach(
      EXPORTING
        keys = VALUE #( ( %key = ls_position ) )
    ).

    " Then
    cl_abap_unit_assert=>assert_initial(
      act = failed-riskposition
      msg = 'No limit breach should be detected' ).
  ENDMETHOD.

  METHOD test_limit_with_breach.
    " Given
    DATA(ls_position) = VALUE z_trm_risk_position(
      position_amount = '2000000.00'  " Amount exceeds limit
      position_currency = 'EUR'
      risk_type = 'MKT'
      counterparty_id = 'BP001'
    ).

    " When
    cut->checkLimitBreach(
      EXPORTING
        keys = VALUE #( ( %key = ls_position ) )
    ).

    " Then
    cl_abap_unit_assert=>assert_not_initial(
      act = failed-riskposition
      msg = 'Limit breach should be detected' ).
  ENDMETHOD.

  METHOD test_create_draft.
    " Given
    DATA(ls_position) = VALUE z_trm_risk_position(
      position_id = 'POS002'
      company_code = '1000'
      risk_type = 'MKT'
      position_amount = '100000.00'
      position_currency = 'EUR'
    ).

    " When
    cut->createDraft(
      EXPORTING
        keys = VALUE #( ( %key = ls_position ) )
      IMPORTING
        result = DATA(lt_result) ).

    " Then
    cl_abap_unit_assert=>assert_not_initial(
      act = lt_result
      msg = 'Draft creation should return a result' ).
  ENDMETHOD.

  METHOD test_activate_draft.
    " Given - Create a draft first
    DATA(ls_draft) = VALUE z_trm_risk_position(
      position_id = 'POS003'
      is_draft = abap_true
    ).

    " When
    cut->activateDraft(
      EXPORTING
        keys = VALUE #( ( %key = ls_draft ) )
      IMPORTING
        result = DATA(lt_result) ).

    " Then
    cl_abap_unit_assert=>assert_not_initial(
      act = lt_result
      msg = 'Draft activation should return a result' ).
  ENDMETHOD.

  METHOD test_discard_draft.
    " Given - Create a draft first
    DATA(ls_draft) = VALUE z_trm_risk_position(
      position_id = 'POS004'
      is_draft = abap_true
    ).

    " When
    cut->discardDraft(
      EXPORTING
        keys = VALUE #( ( %key = ls_draft ) )
      IMPORTING
        result = DATA(lt_result) ).

    " Then
    cl_abap_unit_assert=>assert_initial(
      act = failed-riskposition
      msg = 'Draft should be discarded without errors' ).
  ENDMETHOD.

  METHOD test_create_pos_with_cf.
    " Given
    DATA(ls_position) = VALUE z_trm_risk_position(
      position_id = 'POS005'
      company_code = '1000'
      risk_type = 'MKT'
    ).

    DATA(ls_cashflow) = VALUE z_trm_cashflow(
      cashflow_id = 'CF001'
      amount = '50000.00'
      currency = 'EUR'
      value_date = sy-datum + 30
    ).

    " When
    cut->createPositionWithCashflow(
      EXPORTING
        position = ls_position
        cashflow = ls_cashflow
      IMPORTING
        result = DATA(lt_result) ).

    " Then
    cl_abap_unit_assert=>assert_not_initial(
      act = lt_result
      msg = 'Position with cashflow should be created' ).
  ENDMETHOD.

  METHOD test_add_cf_to_position.
    " Given
    DATA(ls_cashflow) = VALUE z_trm_cashflow(
      cashflow_id = 'CF002'
      amount = '75000.00'
      currency = 'EUR'
      value_date = sy-datum + 60
    ).

    " When
    cut->addCashflow(
      EXPORTING
        position_id = 'POS001'
        cashflow = ls_cashflow
      IMPORTING
        result = DATA(lt_result) ).

    " Then
    cl_abap_unit_assert=>assert_not_initial(
      act = lt_result
      msg = 'Cashflow should be added to position' ).
  ENDMETHOD.

  METHOD test_position_auth.
    " Given
    DATA(ls_position) = VALUE z_trm_risk_position(
      position_id = 'POS001'
      company_code = '1000'
    ).

    " When
    cut->checkAuthorization(
      EXPORTING
        keys = VALUE #( ( %key = ls_position ) )
      IMPORTING
        result = DATA(lt_result) ).

    " Then
    cl_abap_unit_assert=>assert_initial(
      act = failed-riskposition
      msg = 'Authorization check should pass' ).
  ENDMETHOD.

ENDCLASS.