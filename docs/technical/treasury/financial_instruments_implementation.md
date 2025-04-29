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