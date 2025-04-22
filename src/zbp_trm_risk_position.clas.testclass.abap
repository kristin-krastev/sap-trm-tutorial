CLASS ltcl_risk_position DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    CLASS-DATA:
      environment TYPE REF TO if_cl_test_environment.

    CLASS-METHODS:
      class_setup,
      class_teardown.

    METHODS:
      setup,
      teardown,

      " Test methods for validations
      test_validate_position_success FOR TESTING,
      test_validate_position_invalid_id FOR TESTING,
      test_validate_position_future_date FOR TESTING,
      test_validate_position_invalid_type FOR TESTING,

      " Test methods for amount validation
      test_validate_amount_success FOR TESTING,
      test_validate_amount_negative FOR TESTING,
      test_validate_amount_invalid_curr FOR TESTING,

      " Test methods for risk calculation
      test_calculate_market_risk FOR TESTING,
      test_calculate_credit_risk FOR TESTING,
      test_calculate_liquidity_risk FOR TESTING,

      " Test methods for limit checks
      test_check_limit_no_breach FOR TESTING,
      test_check_limit_with_breach FOR TESTING,

      " Helper methods
      create_test_data,
      cleanup_test_data.

    DATA:
      cut TYPE REF TO zbp_trm_risk_position.
ENDCLASS.

CLASS ltcl_risk_position IMPLEMENTATION.

  METHOD class_setup.
    " Create test environment
    environment = cl_test_environment=>create(
      i_package_name = 'Z_TRM_RISK'
    ).

    " Insert test data into relevant tables
    environment->insert_test_data(
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
      ) )
    ).
  ENDMETHOD.

  METHOD class_teardown.
    environment->destroy( ).
  ENDMETHOD.

  METHOD setup.
    " Create instance of the class under test
    cut = NEW zbp_trm_risk_position( ).
    create_test_data( ).
  ENDMETHOD.

  METHOD teardown.
    cleanup_test_data( ).
  ENDMETHOD.

  METHOD test_validate_position_success.
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
      msg = 'Validation should succeed for valid position'
    ).
  ENDMETHOD.

  METHOD test_validate_position_invalid_id.
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
      msg = 'Validation should fail for invalid position ID'
    ).
  ENDMETHOD.

  METHOD test_calculate_market_risk.
    " Given
    DATA(ls_position) = VALUE z_trm_risk_position(
      position_amount = '100000.00'
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
    DATA(expected_risk) = ls_position-position_amount *
                         sqrt( ls_position-time_horizon ) *
                         ( 1 - ls_position-confidence_level ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_position-risk_amount
      exp = expected_risk
      msg = 'Market risk calculation incorrect'
    ).
  ENDMETHOD.

  METHOD test_check_limit_with_breach.
    " Given
    DATA(ls_position) = VALUE z_trm_risk_position(
      counterparty_id = 'BP001'
      risk_type = 'MKT'
      risk_amount = '1000000.00'
    ).

    " When
    cut->checkLimitBreach(
      EXPORTING
        keys = VALUE #( ( %key = ls_position ) )
    ).

    " Then
    cl_abap_unit_assert=>assert_not_initial(
      act = reported-riskposition
      msg = 'Limit breach should be detected'
    ).
  ENDMETHOD.

  METHOD create_test_data.
    " Insert test data into relevant tables
    INSERT INTO z_trm_risk_limit VALUES @(
      VALUE #(
        counterparty_id = 'BP001'
        risk_type = 'MKT'
        limit_amount = '500000.00'
        valid_from = sy-datum
        valid_to = '99991231'
      )
    ).
  ENDMETHOD.

  METHOD cleanup_test_data.
    " Clean up test data
    DELETE FROM z_trm_risk_limit WHERE counterparty_id = 'BP001'.
  ENDMETHOD.

ENDCLASS.