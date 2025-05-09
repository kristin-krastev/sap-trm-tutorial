# SAP RAP Development Rules

## General Conventions
- Always follow SAP RAP naming conventions (Z or Y namespace)
- Use meaningful and consistent suffixes (_R_, _I_, _C_)
- Include proper documentation and comments
- Method names must not exceed 30 characters
- Use abbreviated but clear method names (e.g., test_pos_valid instead of test_validate_position_success)

## Required Features
- Always implement draft handling for transactional apps
- Use managed scenarios where possible
- Implement proper authorization checks
- Use late numbering for IDs
- Include ETag handling

## CDS View Patterns
- Basic views should use suffix _R_
- Interface views should use suffix _I_
- Consumption views should use suffix _C_
- Root views must use "define root view entity"
- Child views must use "define view entity"
- Always include @EndUserText.label annotations
- Always include @ObjectModel annotations for semantic keys

## Behavior Definitions
- Include strict mode (2)
- Use corresponding fields mapping
- Mark key fields as readonly
- Include proper validations
- Include proper determinations
- Include draft actions for root entities
- Add associations with draft support

## UI Annotations
- Include headerInfo
- Include proper lineItem positions
- Include identification annotations
- Include semantic field annotations
- Include proper facet definitions

## Database Tables
- Include administrative fields (created/changed)
- Use proper data elements
- Include proper key fields
- Draft tables must include all draft-specific fields

## Implementation Classes
- Follow handler pattern (create/update/delete)
- Implement proper validations
- Handle determinations efficiently
- Include proper error messages
- Follow clean code principles

## Testing
- Create unit tests for validations
- Create integration tests for scenarios
- Test draft handling
- Test error scenarios
- Method names must not exceed 30 characters
- Use clear abbreviations in test method names:
  * pos = position
  * cf = cashflow
  * inst = instrument
  * val = validate/validation
  * calc = calculate
  * auth = authorization
  * draft = draft handling
  * assoc = association

## Test Class Structure and Best Practices

### Test Class Setup
```abap
CLASS ltcl_<entity> DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    CLASS-DATA:
      sql_environment TYPE REF TO if_osql_test_environment.

    DATA:
      cut TYPE REF TO lhc_<entity>.  "Class Under Test

    CLASS-METHODS:
      class_setup,    "Setup test environment once for all tests
      class_teardown. "Cleanup after all tests

    METHODS:
      setup,         "Setup before each test method
      teardown,      "Cleanup after each test method
      test_method1 FOR TESTING RAISING cx_static_check,
      test_method2 FOR TESTING RAISING cx_static_check.
```

### Test Environment Setup
- Use `if_osql_test_environment` for database table isolation
- Define dependent tables in class_setup
- Clear test data after each test
- Use ROLLBACK ENTITIES after each test

### Test Method Structure
1. Given (Test Data Setup)
   - Prepare test data structures
   - Insert test data into test environment
   - Create test entities if needed

2. When (Execute Test)
   - Call the method under test
   - Pass required parameters
   - Capture results

3. Then (Verify Results)
   - Use assertions to verify expected outcomes
   - Check error messages if testing negative scenarios
   - Verify entity states

### RAP-Specific Testing Guidelines
- Use proper RAP response structures (failed/reported)
- Include %cid for entity creation
- Use %tky for entity keys
- Test both draft and active instances
- Test validation methods
- Test determination methods
- Test action methods

### Example Test Method
```abap
METHOD test_validation.
  " Given
  DATA: lt_create TYPE TABLE FOR CREATE zi_entity,
        lt_failed TYPE RESPONSE FOR FAILED LATE zi_entity.

  lt_create = VALUE #(
    ( %cid = 'TEST1'
      field1 = 'value1'
      field2 = 'value2' )
  ).

  " When
  MODIFY ENTITIES OF zi_entity
    ENTITY Entity
      CREATE FROM lt_create
    MAPPED DATA(mapped)
    FAILED DATA(failed)
    REPORTED DATA(reported).

  " Then
  cl_abap_unit_assert=>assert_initial(
    act = failed-entity
    msg = 'Entity creation should succeed'
  ).
END METHOD.
```

### Best Practices
1. Use meaningful test method names
2. Test both positive and negative scenarios
3. Keep tests independent
4. Clean up test data properly
5. Use proper assertions
6. Document complex test scenarios
7. Test boundary conditions
8. Include error message testing