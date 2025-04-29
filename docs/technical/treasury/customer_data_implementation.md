# Customer Data Technical Implementation

## Directory Structure
```
src/customer_data/
├── z_trm_customer.tabl.xml           # Main customer master table
├── z_trm_customer_bank.tabl.xml      # Bank details table
├── z_trm_customer_trading.tabl.xml   # Trading partner details
├── z_trm_customer_cds.ddls.asddls    # CDS view for customer data
└── z_trm_customer_bdef.bdef.asbdef   # Behavior definition
```

## Database Tables

### Z_TRM_CUSTOMER
Main table for storing customer master data.

#### Key Fields:
- `CUSTOMER_ID` (CHAR10): Primary key
- `CUSTOMER_TYPE` (CHAR2): Type of customer (BP/BK/TR/IN)
- `NAME` (CHAR40): Customer name
- `SEARCH_TERM` (CHAR20): Search help field
- `COUNTRY` (CHAR2): Country key
- `LANGUAGE` (LANG): Language key
- `TAX_NUMBER` (CHAR20): Tax identification number
- `INDUSTRY` (CHAR4): Industry sector
- `CREDIT_RATING` (CHAR3): Credit rating
- `STATUS` (CHAR2): Customer status

#### Administrative Fields:
- `CREATED_BY`, `CREATED_AT`
- `LAST_CHANGED_BY`, `LAST_CHANGED_AT`

### Z_TRM_CUSTOMER_BANK
Table for storing bank-specific details.

#### Key Fields:
- `CUSTOMER_ID` (CHAR10): Foreign key to main table
- `BANK_ID` (CHAR20): Bank identification
- `SWIFT_CODE` (CHAR11): BIC/SWIFT code
- `BANK_COUNTRY` (CHAR2): Bank country
- `BANK_ACCOUNT` (CHAR34): IBAN/Account number
- `ACCOUNT_CURRENCY` (CUKY5): Account currency
- `ROUTING_CODE` (CHAR20): Bank routing code

### Z_TRM_CUSTOMER_TRADING
Table for storing trading partner details.

#### Key Fields:
- `CUSTOMER_ID` (CHAR10): Foreign key to main table
- `TRADING_LIMIT` (DEC23.4): Trading limit amount
- `LIMIT_CURRENCY` (CUKY5): Limit currency
- `RISK_CATEGORY` (CHAR2): Risk classification
- `TRADING_BLOCK` (CHAR1): Trading block indicator
- `VALID_FROM` (DATS): Validity start date
- `VALID_TO` (DATS): Validity end date

## CDS View (Z_TRM_CUSTOMER_CDS)

### Annotations
```abap
@AbapCatalog.sqlViewName: 'ZTRMCUST'
@AccessControl.authorizationCheck: #CHECK
@Analytics: {
  dataCategory: #DIMENSION,
  dataExtraction.enabled: true
}
@ObjectModel: {
  modelCategory: #BUSINESS_OBJECT,
  compositionRoot: true,
  transactionalProcessingEnabled: true,
  semanticKey: ['CUSTOMER_ID']
}
```

### Associations
- `_BankDetails`: To bank details
- `_TradingDetails`: To trading details
- `_CountryText`: To country descriptions
- `_IndustryText`: To industry descriptions

### Virtual Elements
- `CustomerTypeText`: Human-readable customer type
- `RiskCategoryText`: Risk category description
- `StatusText`: Status description
- `FullAddress`: Computed complete address

## Behavior Definition

### Implementation Details
- Implementation Class: `zbp_trm_customer`
- Strict Mode: 2
- Persistent Table: `z_trm_customer`

### Operations
1. Standard Operations:
   - Create
   - Update
   - Delete

2. Actions:
   - `activate`: Activates the customer
   - `block`: Blocks customer for trading
   - `unblock`: Removes trading block
   - `updateCreditRating`: Updates credit rating

3. Validations:
   - Address validation
   - Bank details validation
   - Trading limit validation
   - Credit rating validation

4. Determinations:
   - Risk category calculation
   - Initial status setting
   - Trading limit calculation

### Field Behaviors
- Mandatory Fields:
  - `CustomerID`
  - `CustomerType`
  - `Name`
  - `Country`
  - `Language`

- Read-only Fields:
  - `CreatedBy`
  - `CreatedAt`
  - `LastChangedBy`
  - `LastChangedAt`

## Usage Examples

### Creating a New Customer
```abap
DATA(customer) = NEW zcl_trm_customer( ).
customer->create_customer(
  customer_type = 'BP'
  name         = 'ACME Corporation'
  country      = 'US'
  language     = 'E'
  tax_number   = 'US123456789'
).
```

### Updating Trading Limits
```abap
DATA(customer) = zcl_trm_customer=>get_by_id( '1000000123' ).
customer->update_trading_limit(
  limit_amount = 5000000
  currency     = 'USD'
  risk_category = 'A1'
).
```

## Error Handling

### Common Error Scenarios
1. Invalid customer data
2. Duplicate records
3. Missing mandatory fields
4. Authorization errors

### Error Messages
- `ZCM_001`: Invalid customer data
- `ZCM_002`: Duplicate customer record
- `ZCM_003`: Missing mandatory field
- `ZCM_004`: Invalid bank details
- `ZCM_005`: Invalid trading limit