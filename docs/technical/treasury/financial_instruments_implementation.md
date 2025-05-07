# Financial Instruments Technical Implementation

## Directory Structure
```
src/financial_instruments/
├── z_trm_fin_instrument.tabl.xml       # Main financial instrument table
├── z_trm_instrument_cond.tabl.xml      # Instrument conditions table
├── z_trm_fin_instrument_cds.ddls.asddls # CDS view for financial instruments
└── z_trm_fin_instrument_bdef.bdef.asbdef # Behavior definition
```

## Database Tables

### Z_TRM_FIN_INSTRUMENT
Main table for storing financial instrument master data.

#### Key Fields:
- `INSTRUMENT_ID` (CHAR20): Primary key
- `INSTRUMENT_TYPE` (CHAR4): Type of instrument (BOND/LOAN/DEPO/SWAP)
- `DESCRIPTION` (CHAR60): Instrument description
- `NOMINAL_AMOUNT` (DEC23.4): Principal amount
- `CURRENCY` (CUKY5): Currency key
- `START_DATE` (DATS): Validity start date
- `END_DATE` (DATS): Maturity/end date
- `INTEREST_TYPE` (CHAR2): Fixed/Variable interest indicator
- `INTEREST_RATE` (DEC5.2): Interest rate percentage
- `PAYMENT_FREQUENCY` (CHAR2): Payment frequency code
- `COUNTERPARTY_ID` (CHAR10): Business partner ID
- `STATUS` (CHAR2): Instrument status

#### Administrative Fields:
- `CREATED_BY`, `CREATED_AT`
- `LAST_CHANGED_BY`, `LAST_CHANGED_AT`

### Z_TRM_INSTRUMENT_COND
Table for storing instrument conditions and payment schedules.

#### Key Fields:
- `INSTRUMENT_ID` (CHAR20): Foreign key to main table
- `CONDITION_TYPE` (CHAR4): Type of condition
- `VALID_FROM` (DATS): Condition validity start
- `VALID_TO` (DATS): Condition validity end
- `RATE_TYPE` (CHAR4): Interest rate type
- `BASE_RATE_ID` (CHAR10): Reference rate ID
- `SPREAD` (DEC5.2): Additional spread percentage
- `FIXED_RATE` (DEC5.2): Fixed interest rate
- `PAYMENT_CALENDAR` (CHAR4): Calendar ID for payments
- `DAY_CONVENTION` (CHAR2): Day count convention

## CDS View (Z_TRM_FIN_INSTRUMENT_CDS)

### Annotations
```abap
@AbapCatalog.sqlViewName: 'ZTRMFININS'
@AccessControl.authorizationCheck: #CHECK
@Analytics: {
  dataCategory: #FACT,
  dataExtraction.enabled: true
}
@ObjectModel: {
  modelCategory: #BUSINESS_OBJECT,
  compositionRoot: true,
  transactionalProcessingEnabled: true
}
```

### Associations
- `_Conditions`: To instrument conditions
- `_BusinessPartner`: To business partner master

### Virtual Elements
- `InstrumentTypeText`: Human-readable instrument type
- `InterestTypeText`: Interest type description
- `FrequencyText`: Payment frequency description
- `StatusText`: Status description

## Behavior Definition

### Implementation Details
- Implementation Class: `zbp_trm_fin_instrument`
- Strict Mode: 2
- Persistent Table: `z_trm_fin_instrument`

### Operations
1. Standard Operations:
   - Create
   - Update
   - Delete

2. Actions:
   - `activate`: Activates the instrument
   - `terminate`: Terminates the instrument
   - `close`: Closes the instrument

3. Validations:
   - Date validation
   - Amount validation
   - Counterparty validation
   - Interest validation

4. Determinations:
   - Interest calculation
   - Initial status setting

### Field Behaviors
- Mandatory Fields:
  - `InstrumentID`
  - `InstrumentType`
  - `NominalAmount`
  - `Currency`
  - `StartDate`

- Read-only Fields:
  - `CreatedBy`
  - `CreatedAt`
  - `LastChangedBy`
  - `LastChangedAt`

## Usage Examples

### Creating a New Instrument
```abap
DATA(instrument) = NEW zcl_trm_fin_instrument( ).
instrument->create_instrument(
  instrument_type = 'BOND'
  nominal_amount = 1000000
  currency       = 'USD'
  start_date     = '20240101'
  end_date       = '20250101'
).
```

### Updating Interest Rate
```abap
DATA(instrument) = zcl_trm_fin_instrument=>get_by_id( '1234567890' ).
instrument->update_interest_rate(
  interest_type = 'FX'
  interest_rate = '5.25'
).
```

## Error Handling

### Common Error Scenarios
1. Invalid date combinations
2. Missing mandatory fields
3. Invalid currency/amount combinations
4. Authorization errors

### Error Messages
- `ZTM_001`: Invalid date range
- `ZTM_002`: Missing mandatory field
- `ZTM_003`: Invalid amount
- `ZTM_004`: Invalid currency
- `ZTM_005`: Invalid status transition

## Position and Cashflow Behavior Definition

### Implementation Overview
- Implementation Class: `zbp_kkr_i_position`
- Strict Mode: 2
- Draft Enabled
- Authorization: Instance-based (master/dependent pattern)

### Position Behavior
```abap
define behavior for ZKKR_I_POSITION alias Position
persistent table ztrmpos
draft table zkkr_dpos
lock master total etag ChangedAt
authorization master ( instance )
etag master ChangedAt
{
  create;
  update;
  delete;

  // Field controls
  field ( readonly ) PositionID, CreatedBy, CreatedAt, ChangedBy, ChangedAt;
  field ( mandatory ) InstrumentID, ValidFrom;

  // Position validations
  validation validateDates on save {
    create;
    update;
    field ValidFrom;
    field ValidTo;
  }

  validation validateInstrument on save {
    create;
    update;
    field InstrumentID;
  }

  // Cashflow validations
  validation validateCashflowAmount on save {
    create;
    update;
  }

  validation validateCashflowDate on save {
    create;
    update;
  }

  // Draft actions
  draft action Edit;
  draft action Activate optimized;
  draft action Discard;
  draft action Resume;
  draft determine action Prepare {
    validation validateDates;
    validation validateInstrument;
    validation validateCashflowAmount;
    validation validateCashflowDate;
  }

  // Associations
  association _Cashflow { create; with draft; }

  // Determinations
  determination calculatePositionID on save { create; }

  mapping for ztrmpos corresponding;
}
```

### Cashflow Behavior
```abap
define behavior for ZKKR_I_CASHFLOW alias Cashflow
implementation in class zbp_kkr_i_position unique
persistent table ztrmcf
draft table zkkr_dcf
lock dependent by _Position
authorization dependent by _Position
etag master ChangedAt
{
  update;
  delete;

  // Field controls
  field ( readonly ) CashflowID, PositionID, CreatedBy, CreatedAt, ChangedBy, ChangedAt;
  field ( mandatory ) Amount, Currency, ValueDate;

  // Associations
  association _Position { with draft; }

  // Determinations
  determination calculateCashflowID on save { create; }

  mapping for ztrmcf corresponding;
}
```

### Key Features
1. **Draft Handling**:
   - Full draft support for both Position and Cashflow
   - Optimized activation
   - Prepare action with validations

2. **Authorization**:
   - Instance-based authorization for Position (master)
   - Dependent authorization for Cashflow
   - Proper lock handling

3. **Field Control**:
   - Proper readonly fields for system-managed fields
   - Mandatory field definitions
   - ETag handling for optimistic locking

4. **Validations and Determinations**:
   - Comprehensive date validations
   - Instrument validations
   - Cashflow amount and date validations
   - Automatic ID calculations

5. **Associations**:
   - Bidirectional association between Position and Cashflow
   - Draft-enabled associations
   - Proper parent-child relationship

### Secondary Key Handling in Determinations

For RAP business object implementations, when dealing with entity determinations, we should use proper READ ENTITIES and MODIFY ENTITIES patterns:

```abap
METHOD calculatePositionID.
  " Read the position data
  READ ENTITIES OF zkkr_i_position IN LOCAL MODE
    ENTITY Position
      FIELDS ( PositionID ) WITH CORRESPONDING #( keys )
    RESULT DATA(positions).

  " Get last used position number
  SELECT SINGLE
    FROM ztrmpos
    FIELDS MAX( position_id ) AS last_id
    INTO @DATA(last_position_id).

  " Handle initial value
  IF last_position_id IS INITIAL.
    last_position_id = 0.
  ENDIF.

  " Read positions that need ID assignment
  READ ENTITIES OF zkkr_i_position IN LOCAL MODE
    ENTITY Position
      FIELDS ( PositionID )
      WITH CORRESPONDING #( keys )
      RESULT DATA(positions_to_update)
      FAILED DATA(read_failed).

  DATA updates TYPE TABLE FOR UPDATE zkkr_i_position\\Position.

  " Process positions without ID
  LOOP AT positions_to_update REFERENCE INTO DATA(position_ref).
    IF position_ref->PositionID IS NOT INITIAL.
      CONTINUE.
    ENDIF.

    " Generate new position ID
    last_position_id += 1.
    DATA(new_position_id) = |POS{ last_position_id ALIGN = RIGHT PAD = '0' WIDTH = 10 }|.

    " Collect updates
    APPEND VALUE #( %tky = position_ref->%tky
                   PositionID = new_position_id ) TO updates.
  ENDLOOP.

  " Update positions
  MODIFY ENTITIES OF zkkr_i_position IN LOCAL MODE
    ENTITY Position UPDATE FROM updates.
ENDMETHOD.
```

The same pattern applies to the Cashflow determination:

```abap
METHOD calculateCashflowID.
  " Read the cashflow data
  READ ENTITIES OF zkkr_i_position IN LOCAL MODE
    ENTITY Cashflow
      FIELDS ( CashflowID ) WITH CORRESPONDING #( keys )
    RESULT DATA(cashflows).

  " Get last used cashflow number
  SELECT SINGLE
    FROM ztrmcf
    FIELDS MAX( cashflow_id ) AS last_id
    INTO @DATA(last_cashflow_id).

  " Handle initial value
  IF last_cashflow_id IS INITIAL.
    last_cashflow_id = 0.
  ENDIF.

  " Read cashflows that need ID assignment
  READ ENTITIES OF zkkr_i_position IN LOCAL MODE
    ENTITY Cashflow
      FIELDS ( CashflowID )
      WITH CORRESPONDING #( keys )
      RESULT DATA(cashflows_to_update)
      FAILED DATA(read_failed).

  DATA updates TYPE TABLE FOR UPDATE zkkr_i_position\\Cashflow.

  " Process cashflows without ID
  LOOP AT cashflows_to_update REFERENCE INTO DATA(cashflow_ref).
    IF cashflow_ref->CashflowID IS NOT INITIAL.
      CONTINUE.
    ENDIF.

    " Generate new cashflow ID
    last_cashflow_id += 1.
    DATA(new_cashflow_id) = |CF{ last_cashflow_id ALIGN = RIGHT PAD = '0' WIDTH = 10 }|.

    " Collect updates
    APPEND VALUE #( %tky = cashflow_ref->%tky
                   CashflowID = new_cashflow_id ) TO updates.
  ENDLOOP.

  " Update cashflows
  MODIFY ENTITIES OF zkkr_i_position IN LOCAL MODE
    ENTITY Cashflow UPDATE FROM updates.
ENDMETHOD.
```

#### Notes on Entity Handling
1. Use READ ENTITIES to fetch data instead of direct WHERE clauses
2. Process records using LOOP with CONTINUE for filtering
3. Collect updates in a separate table
4. Use MODIFY ENTITIES with collected updates
5. This approach:
   - Avoids secondary key warnings
   - Follows RAP best practices
   - Improves performance by batching updates
   - Maintains proper entity handling