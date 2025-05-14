# SAP RAP Development Rules and Best Practices

## Testing Parent-Child Entity Relationships

### Parent-Child Test Pattern
```abap
" 1. Create parent entity first
MODIFY ENTITIES OF zi_parent
  ENTITY Parent
    CREATE FIELDS ( field1 field2 )
    WITH VALUE #( ( %cid = 'PARENT1'
                   field1 = 'value1'
                   field2 = 'value2' ) )
  MAPPED DATA(mapped_parent)
  FAILED DATA(failed_parent)
  REPORTED DATA(reported_parent).

" 2. Create child using association
MODIFY ENTITIES OF zi_parent
  ENTITY Parent
    CREATE BY \_Child
    FIELDS ( field1 field2 )
    WITH VALUE #(
      ( %tky = mapped_parent-parent[ 1 ]-%tky  "Use mapped key
        %target = VALUE #(
          ( %cid = 'CHILD1'
            field1 = 'value1'
            field2 = 'value2' ) ) ) )
  MAPPED DATA(mapped_child)
  FAILED DATA(failed_child)
  REPORTED DATA(reported_child).
```

### Validation Test Best Practices
1. Always test both valid and invalid scenarios
2. For child entities:
   - Create parent first
   - Use proper association (\_ChildEntity)
   - Reference parent using %tky from mapped results
   - Use %target for child data
3. For read-only fields:
   - Never try to set them directly
   - Let determinations handle them
   - Test after creation using READ ENTITIES
4. Test structure:
   - Given: Setup test data and entities
   - When: Execute the validation
   - Then: Assert results

### Common Validation Test Cases
1. Required Fields
```abap
" Missing required field
( %cid = 'TEST1'
  required_field = '' )     "Should fail
```

2. Numeric Validations
```abap
" Amount validation
( %cid = 'TEST1'
  amount = '0.00' )         "Should fail
( %cid = 'TEST2'
  amount = '-100.00' )      "Should fail
( %cid = 'TEST3'
  amount = '100.00' )       "Should pass
```

3. Date Validations
```abap
" Date range validation
( %cid = 'TEST1'
  valid_from = '20240101'
  valid_to = '20231231' )   "Should fail (to < from)
```

### Test Method Template for Validations
```abap
METHOD test_validation.
  " Given
  DATA: failed   TYPE RESPONSE FOR FAILED LATE zi_parent,
        reported TYPE RESPONSE FOR REPORTED LATE zi_parent.

  " When - Create test data
  MODIFY ENTITIES OF zi_parent
    ENTITY Parent
      CREATE FIELDS ( field1 field2 )
      WITH VALUE #( ( %cid = 'TEST1'
                     field1 = 'valid_value'
                     field2 = 'valid_value' ) )
    MAPPED DATA(mapped)
    FAILED DATA(failed_create)
    REPORTED DATA(reported_create).

  " Then - Assert results
  cl_abap_unit_assert=>assert_initial(
    act = failed_create
    msg = 'Creation with valid data should succeed' ).

  " Test invalid case
  MODIFY ENTITIES OF zi_parent
    ENTITY Parent
      CREATE FIELDS ( field1 field2 )
      WITH VALUE #( ( %cid = 'TEST2'
                     field1 = 'invalid_value'
                     field2 = '' ) )
    MAPPED DATA(mapped_invalid)
    FAILED DATA(failed_invalid)
    REPORTED DATA(reported_invalid).

  " Assert invalid case
  cl_abap_unit_assert=>assert_not_initial(
    act = failed_invalid
    msg = 'Creation with invalid data should fail' ).
ENDMETHOD.
```

### Testing Tips
1. Use meaningful test data that reflects real business scenarios
2. Test boundary conditions
3. Include error message validation
4. Clean up test data in teardown
5. Keep tests independent
6. Use proper assertion messages
7. Test both draft and active instances
8. Consider authorization checks