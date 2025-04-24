# Crude Oil Trading Implementation Example

## 1. Business Scenario
```abap
" Configuration table for crude oil grades
@EndUserText.label : 'Crude Oil Grades'
@AbapCatalog.enhancement.category : #NOT_EXTENSIBLE
@AbapCatalog.tableCategory : #TRANSPARENT
@AbapCatalog.deliveryClass : #A
@AbapCatalog.dataMaintenance : #RESTRICTED
define table ztcomm_crude_grades {
  key grade_id     : abap.char(10);
  description      : abap.char(60);
  api_gravity      : abap.dec(5,2);
  sulfur_content   : abap.dec(5,2);
  base_price_diff  : abap.curr(12,2);
  currency         : abap.cuky(5);
}

" Position handling class
CLASS zcl_crude_position_handler DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      create_position
        IMPORTING
          is_position_data TYPE zscomm_crude_position
        RAISING
          zcx_position_error,

      validate_constraints
        IMPORTING
          is_position TYPE zscomm_crude_position
        RAISING
          zcx_constraint_error.

ENDCLASS.

CLASS zcl_crude_position_handler IMPLEMENTATION.
  METHOD create_position.
    TRY.
        " 1. Validate physical constraints
        validate_constraints( is_position_data ).

        " 2. Calculate risk metrics
        DATA(lo_risk_calc) = NEW zcl_integrated_risk_calculator( ).
        DATA(ls_risk) = lo_risk_calc->calculate_integrated_risk(
          is_position        = is_position_data
          is_physical_const  = get_physical_constraints( is_position_data )
          it_historical_data = get_historical_data( is_position_data )
        ).

        " 3. Create position with risk metrics
        INSERT INTO ztcomm_crude_position VALUES @( VALUE #(
          position_id     = is_position_data-position_id
          grade_id       = is_position_data-grade_id
          volume         = is_position_data-volume
          price         = is_position_data-price
          delivery_date = is_position_data-delivery_date
          risk_score    = ls_risk-risk_score
          var_value     = ls_risk-var_adjusted
        ) ).

    CATCH cx_root INTO DATA(lx_root).
      RAISE EXCEPTION TYPE zcx_position_error
        EXPORTING
          previous = lx_root.
    ENDTRY.
  ENDMETHOD.

  METHOD validate_constraints.
    " Check storage capacity
    SELECT SINGLE FROM ztcomm_storage_capacity
      FIELDS available_capacity
      WHERE storage_location = @is_position-storage_loc
        AND valid_on        <= @is_position-delivery_date
        AND valid_to        >= @is_position-delivery_date
      INTO @DATA(lv_available_capacity).

    IF is_position-volume > lv_available_capacity.
      RAISE EXCEPTION TYPE zcx_constraint_error
        EXPORTING
          textid = zcx_constraint_error=>storage_capacity_exceeded.
    ENDIF.

    " Additional validations...
  ENDMETHOD.
ENDCLASS.
```

## 2. Test Cases

### Unit Tests
```abap
CLASS ltcl_crude_position_handler DEFINITION
  FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA:
      mo_position_handler TYPE REF TO zcl_crude_position_handler,
      mo_environment     TYPE REF TO if_osql_test_environment.

    METHODS:
      setup,
      teardown,
      test_create_valid_position FOR TESTING,
      test_storage_constraint FOR TESTING,
      test_risk_calculation FOR TESTING.
ENDCLASS.

CLASS ltcl_crude_position_handler IMPLEMENTATION.
  METHOD setup.
    " Create test double environment
    mo_environment = cl_osql_test_environment=>create(
      i_dependency_list = VALUE #(
        ( 'ZTCOMM_CRUDE_GRADES' )
        ( 'ZTCOMM_STORAGE_CAPACITY' )
        ( 'ZTCOMM_CRUDE_POSITION' )
      )
    ).

    " Insert test data
    mo_environment->insert_test_data(
      i_data = VALUE ztcomm_storage_capacity_tab(
        ( storage_location = 'TANK01'
          available_capacity = 1000
          valid_on = '20240101'
          valid_to = '20241231' )
      )
    ).

    " Create object under test
    mo_position_handler = NEW #( ).
  ENDMETHOD.

  METHOD test_create_valid_position.
    " Given
    DATA(ls_position) = VALUE zscomm_crude_position(
      position_id   = '1000000001'
      grade_id     = 'WTI'
      volume       = 500
      price       = 75.50
      storage_loc = 'TANK01'
      delivery_date = '20240301'
    ).

    " When
    TRY.
        mo_position_handler->create_position( ls_position ).
        cl_abap_unit_assert=>assert_subrc( ).

        " Then
        SELECT SINGLE FROM ztcomm_crude_position
          FIELDS position_id
          WHERE position_id = @ls_position-position_id
          INTO @DATA(lv_position_id).

        cl_abap_unit_assert=>assert_equals(
          act = lv_position_id
          exp = ls_position-position_id
        ).

    CATCH zcx_position_error INTO DATA(lx_error).
      cl_abap_unit_assert=>fail( lx_error->get_text( ) ).
    ENDTRY.
  ENDMETHOD.

  METHOD test_storage_constraint.
    " Given
    DATA(ls_position) = VALUE zscomm_crude_position(
      position_id   = '1000000002'
      grade_id     = 'WTI'
      volume       = 1500  " Exceeds capacity
      price       = 75.50
      storage_loc = 'TANK01'
      delivery_date = '20240301'
    ).

    " When & Then
    TRY.
        mo_position_handler->create_position( ls_position ).
        cl_abap_unit_assert=>fail( 'Exception expected' ).

    CATCH zcx_constraint_error INTO DATA(lx_error).
        cl_abap_unit_assert=>assert_equals(
          act = lx_error->textid
          exp = zcx_constraint_error=>storage_capacity_exceeded
        ).
    ENDTRY.
  ENDMETHOD.

  METHOD test_risk_calculation.
    " Given
    DATA(ls_position) = VALUE zscomm_crude_position(
      position_id   = '1000000003'
      grade_id     = 'WTI'
      volume       = 500
      price       = 75.50
      storage_loc = 'TANK01'
      delivery_date = '20240301'
    ).

    " When
    TRY.
        mo_position_handler->create_position( ls_position ).

        " Then
        SELECT SINGLE FROM ztcomm_crude_position
          FIELDS risk_score,
                var_value
          WHERE position_id = @ls_position-position_id
          INTO @DATA(ls_result).

        " Verify risk calculations
        cl_abap_unit_assert=>assert_not_initial( ls_result-risk_score ).
        cl_abap_unit_assert=>assert_not_initial( ls_result-var_value ).

    CATCH zcx_position_error INTO DATA(lx_error).
      cl_abap_unit_assert=>fail( lx_error->get_text( ) ).
    ENDTRY.
  ENDMETHOD.

  METHOD teardown.
    mo_environment->destroy( ).
  ENDMETHOD.
ENDCLASS.
```

## 3. Key Implementation Points

### Business Logic
1. Position creation with integrated risk calculation
2. Physical constraints validation
3. Risk metrics calculation and storage
4. Grade-specific handling

### Test Coverage
1. Valid position creation
2. Storage constraint validation
3. Risk calculation verification
4. Error handling validation

### Best Practices
1. Use test doubles for database dependencies
2. Comprehensive error handling
3. Clear separation of concerns
4. Proper transaction handling