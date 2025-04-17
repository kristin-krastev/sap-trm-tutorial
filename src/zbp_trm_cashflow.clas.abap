CLASS zbp_trm_cashflow DEFINITION PUBLIC ABSTRACT FINAL FOR BEHAVIOR OF z_trm_cashflow_cds.
  PUBLIC SECTION.
    INTERFACES if_trm_validation_strategy.
    INTERFACES if_trm_risk_calculation.

    TYPES: BEGIN OF ty_validation_config,
             validation_type TYPE string,
             strategy_class TYPE REF TO if_trm_validation_strategy,
           END OF ty_validation_config.

    TYPES tt_validation_config TYPE STANDARD TABLE OF ty_validation_config WITH KEY validation_type.

    CLASS-METHODS: create_validation_factory
      RETURNING VALUE(ro_factory) TYPE REF TO zcl_trm_validation_factory.

  PROTECTED SECTION.
    METHODS: get_validation_strategy
      IMPORTING iv_validation_type        TYPE string
      RETURNING VALUE(ro_val_strategy)    TYPE REF TO if_trm_validation_strategy.

  PRIVATE SECTION.
    CLASS-DATA: mo_validation_factory TYPE REF TO zcl_trm_validation_factory.
    DATA: mt_validation_config TYPE tt_validation_config.

    METHODS: initialize_validation_config.
ENDCLASS.

CLASS zbp_trm_cashflow IMPLEMENTATION.

  METHOD create_validation_factory.
    IF mo_validation_factory IS NOT BOUND.
      mo_validation_factory = NEW zcl_trm_validation_factory( ).
    ENDIF.
    ro_factory = mo_validation_factory.
  ENDMETHOD.

  METHOD initialize_validation_config.
    " Initialize validation strategies using factory
    DATA(lo_factory) = create_validation_factory( ).

    mt_validation_config = VALUE #(
      ( validation_type = 'DATE'     strategy_class = lo_factory->create_date_validator( ) )
      ( validation_type = 'AMOUNT'   strategy_class = lo_factory->create_amount_validator( ) )
      ( validation_type = 'CURRENCY' strategy_class = lo_factory->create_currency_validator( ) )
    ).
  ENDMETHOD.

  METHOD validateDates.
    DATA(lo_date_validator) = get_validation_strategy( 'DATE' ).

    " Read instance data
    READ ENTITIES OF z_trm_cashflow_cds IN LOCAL MODE
      ENTITY CashFlow
        FIELDS ( BusinessDate ValueDate )
        WITH CORRESPONDING #( keys )
      RESULT DATA(cashflows).

    " Delegate validation to strategy
    lo_date_validator->validate(
      EXPORTING
        it_cashflows = cashflows
      IMPORTING
        et_failed   = failed-cashflow
        et_reported = reported-cashflow
    ).
  ENDMETHOD.

  METHOD validateAmount.
    DATA(lo_amount_validator) = get_validation_strategy( 'AMOUNT' ).

    " Read instance data
    READ ENTITIES OF z_trm_cashflow_cds IN LOCAL MODE
      ENTITY CashFlow
        FIELDS ( FlowAmount FlowType )
        WITH CORRESPONDING #( keys )
      RESULT DATA(cashflows).

    " Delegate validation to strategy
    lo_amount_validator->validate(
      EXPORTING
        it_cashflows = cashflows
      IMPORTING
        et_failed   = failed-cashflow
        et_reported = reported-cashflow
    ).
  ENDMETHOD.

  METHOD calculateRiskAdjustment.
    " Use Strategy pattern for risk calculations
    DATA(lo_risk_calculator) = CAST if_trm_risk_calculation(
      create_validation_factory( )->create_risk_calculator( )
    ).

    " Read instance data
    READ ENTITIES OF z_trm_cashflow_cds IN LOCAL MODE
      ENTITY CashFlow
        FIELDS ( FlowAmount FlowType )
        WITH CORRESPONDING #( keys )
      RESULT DATA(cashflows).

    " Delegate risk calculation to strategy
    lo_risk_calculator->calculate_risk(
      EXPORTING
        it_cashflows = cashflows
      IMPORTING
        et_adjustments = DATA(lt_adjustments)
    ).

    " Apply calculated adjustments
    MODIFY ENTITIES OF z_trm_cashflow_cds IN LOCAL MODE
      ENTITY CashFlow
        UPDATE FIELDS ( RiskAdjustedAmount )
        WITH lt_adjustments.
  ENDMETHOD.

  METHOD get_validation_strategy.
    READ TABLE mt_validation_config WITH KEY validation_type = iv_validation_type
      INTO DATA(ls_config).
    IF sy-subrc = 0.
      ro_val_strategy = ls_config-strategy_class.
    ELSE.
      " Fall back to default validation if strategy not found
      ro_val_strategy = create_validation_factory( )->create_default_validator( ).
    ENDIF.
  ENDMETHOD.

ENDCLASS.