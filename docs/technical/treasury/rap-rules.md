## Testing Patterns

### Date Validation Testing
When testing date validations in RAP:
1. Use `cl_abap_context_info=>get_system_date()` as reference point
2. For future dates: `reference_date + 1`
3. For past dates: `reference_date - 1`
4. Test through parent entity if testing child entity validations

Example for cashflow date validation:
```abap
" Test Case 1: Valid future date
DATA(future_date) = cl_abap_context_info=>get_system_date( ) + 1.

MODIFY ENTITIES OF zkkr_i_position
  ENTITY Position
    CREATE BY \_Cashflow
    FIELDS ( ValueDate )
    WITH VALUE #(
      ( %tky = mapped_pos-position[ 1 ]-%tky
        %target = VALUE #(
          ( %cid = 'CF1'
            ValueDate = future_date ) ) ) )
```

### Testing Child Entity Validations
When testing validations for child entities (like cashflows):
1. Create parent entity first
2. Use composition relationship (`\_ChildEntity`)
3. Test both valid and invalid scenarios
4. Assert using FAILED and REPORTED structures

Example pattern:
```abap
" Step 1: Create parent
MODIFY ENTITIES OF zkkr_i_position
  ENTITY Position
    CREATE FIELDS ( InstrumentID ValidFrom )
    WITH VALUE #( ( %cid = 'POS1'
                   InstrumentID = 'INST01'
                   ValidFrom = system_date ) )
  MAPPED DATA(mapped_pos)
  FAILED DATA(failed_pos)
  REPORTED DATA(reported_pos).

" Step 2: Create child through parent
MODIFY ENTITIES OF zkkr_i_position
  ENTITY Position
    CREATE BY \_Cashflow
    FIELDS ( ... )
    WITH VALUE #(
      ( %tky = mapped_pos-position[ 1 ]-%tky
        %target = VALUE #( ( %cid = 'CF1' ... ) ) ) )
  MAPPED DATA(mapped_cf)
  FAILED DATA(failed_cf)
  REPORTED DATA(reported_cf).

" Step 3: Assert results
cl_abap_unit_assert=>assert_initial/not_initial(
  act = failed_cf-cashflow
  msg = 'Test case description' ).
```