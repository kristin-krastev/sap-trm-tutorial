CLASS ltcl_limit DEFINITION FINAL FOR TESTING
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

      " Test methods for limit validation
      test_valid_cpty_limit FOR TESTING,
      test_invalid_limit_type FOR TESTING,
      test_invalid_limit_category FOR TESTING,
      test_missing_counterparty FOR TESTING,

      " Test methods for date validation
      test_valid_dates FOR TESTING,
      test_past_valid_from FOR TESTING,
      test_invalid_date_range FOR TESTING,

      " Test methods for amount validation
      test_valid_amounts FOR TESTING,
      test_negative_limit FOR TESTING,
      test_negative_utilization FOR TESTING,

      " Test methods for approval authority
      test_small_limit_no_approver FOR TESTING,
      test_large_limit_needs_approver FOR TESTING,

      " Test methods for utilization calculation
      test_normal_utilization FOR TESTING,
      test_over_utilized_limit FOR TESTING,

      " Test methods for actions
      test_approve_limit FOR TESTING,
      test_reject_limit FOR TESTING,
      test_request_increase FOR TESTING,
      test_reset_utilization FOR TESTING,

      " Helper methods
      create_test_data,
      cleanup_test_data.

    DATA:
      cut TYPE REF TO zbp_trm_limit.

ENDCLASS.

CLASS ltcl_limit IMPLEMENTATION.

  METHOD class_setup.
    environment = cl_test_environment=>create(
      i_package_name = 'Z_TRM_RISK'
    ).
  ENDMETHOD.

  METHOD class_teardown.
    environment->destroy( ).
  ENDMETHOD.

  METHOD setup.
    cut = NEW zbp_trm_limit( ).
    create_test_data( ).
  ENDMETHOD.

  METHOD teardown.
    cleanup_test_data( ).
  ENDMETHOD.

  METHOD test_valid_cpty_limit.
    " Given
    DATA(ls_limit) = VALUE z_trm_risk_limit(
      limit_uuid = '1234567890'
      limit_type = 'CPTY'
      limit_category = 'TRADING'
      object_id = 'BP001'
      limit_amount = '1000000.00'
      limit_currency = 'USD'
      valid_from = cl_abap_context_info=>get_system_date( )
    ).

    " When
    cut->validateLimit(
      EXPORTING
        keys = VALUE #( ( %key = ls_limit ) )
    ).

    " Then
    cl_abap_unit_assert=>assert_initial(
      act = failed-limit
      msg = 'Validation should succeed for valid counterparty limit'
    ).
  ENDMETHOD.

  METHOD test_invalid_limit_type.
    " Given
    DATA(ls_limit) = VALUE z_trm_risk_limit(
      limit_uuid = '1234567890'
      limit_type = 'INVALID'
      limit_category = 'TRADING'
      object_id = 'BP001'
    ).

    " When
    cut->validateLimit(
      EXPORTING
        keys = VALUE #( ( %key = ls_limit ) )
    ).

    " Then
    cl_abap_unit_assert=>assert_not_initial(
      act = failed-limit
      msg = 'Validation should fail for invalid limit type'
    ).
  ENDMETHOD.

  METHOD test_large_limit_needs_approver.
    " Given
    DATA(ls_limit) = VALUE z_trm_risk_limit(
      limit_uuid = '1234567890'
      limit_amount = '2000000.00'
      approver_id = ''  " No approver
    ).

    " When
    cut->checkApprovalAuthority(
      EXPORTING
        keys = VALUE #( ( %key = ls_limit ) )
    ).

    " Then
    cl_abap_unit_assert=>assert_not_initial(
      act = failed-limit
      msg = 'Large limit should require approver'
    ).
  ENDMETHOD.

  METHOD test_approve_limit.
    " Given
    DATA(ls_limit) = VALUE z_trm_risk_limit(
      limit_uuid = '1234567890'
      limit_type = 'CPTY'
      limit_category = 'TRADING'
      limit_amount = '1000000.00'
      approval_status = 'PENDING'
    ).

    " When
    cut->approveLimitChange(
      EXPORTING
        keys = VALUE #( ( %key = ls_limit ) )
      IMPORTING
        result = DATA(result)
    ).

    " Then
    READ TABLE result INTO DATA(ls_result) INDEX 1.
    cl_abap_unit_assert=>assert_equals(
      act = ls_result-%param-ApprovalStatus
      exp = 'APPROVED'
      msg = 'Limit should be approved'
    ).
  ENDMETHOD.

  METHOD test_reset_utilization.
    " Given
    DATA(ls_limit) = VALUE z_trm_risk_limit(
      limit_uuid = '1234567890'
      utilized_amount = '500000.00'
    ).

    " When
    cut->resetUtilization(
      EXPORTING
        keys = VALUE #( ( %key = ls_limit ) )
      IMPORTING
        result = DATA(result)
    ).

    " Then
    READ TABLE result INTO DATA(ls_result) INDEX 1.
    cl_abap_unit_assert=>assert_equals(
      act = ls_result-%param-UtilizedAmount
      exp = 0
      msg = 'Utilization should be reset to zero'
    ).
  ENDMETHOD.

  METHOD create_test_data.
    " Insert test data
    INSERT INTO z_trm_risk_limit VALUES @(
      VALUE #(
        limit_uuid = '1234567890'
        limit_type = 'CPTY'
        limit_category = 'TRADING'
        object_id = 'BP001'
        limit_amount = '1000000.00'
        limit_currency = 'USD'
        valid_from = cl_abap_context_info=>get_system_date( )
        warning_threshold = 80
        utilized_amount = '500000.00'
        approval_status = 'PENDING'
      )
    ).
  ENDMETHOD.

  METHOD cleanup_test_data.
    " Clean up test data
    DELETE FROM z_trm_risk_limit WHERE limit_uuid = '1234567890'.
  ENDMETHOD.

ENDCLASS.