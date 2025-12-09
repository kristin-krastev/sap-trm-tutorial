# Fiori Elements - Column Sorting by ID and Name

## Overview
This guide explains how to enable "Sort by ID" and "Sort by Name" options in the column context menu for Fiori Elements analytical list reports.

---

## Pattern for Making Fields Sortable by ID and Name

To enable separate sorting options for both ID and Name in the column context menu, follow these steps:

### 1. Add Text/Name Field to CDS View

In your consumption view, add the text field and associate it with the ID field using `@ObjectModel.text.element`:

```abap
define root view C_CmmdtyHedgeTradeOrderCockpit
  with parameters
    ...
  as select distinct from I_CmmdtyHedgeTradeOrderCockpit(...)
  association [0..1] to I_CompanyCode as _CompanyCode 
    on $projection.CmmdtyHdgPlnExpsrCompanyCode = _CompanyCode.CompanyCode
  ...
{
  @ObjectModel.text.element: ['CompanyCodeName']
  key CmmdtyHdgPlnExpsrCompanyCode,
  
  @Semantics.text: true
  _CompanyCode.CompanyCodeName as CompanyCodeName,
  
  // ... rest of your fields
}
```

### 2. Update Metadata Extension

Add text arrangement and annotations for both ID and Name fields:

```abap
@UI.textArrangement: #TEXT_LAST
@EndUserText.label: 'Company Code'
CmmdtyHdgPlnExpsrCompanyCode;

@Semantics.text: true
@Consumption.filter.hidden: true
@UI.dataFieldDefault: [{hidden: true}]
CompanyCodeName;
```

### 3. Add Name Field to `groupBy`

Include the name field in the `groupBy` array so it becomes a sortable dimension:

```abap
@UI.presentationVariant: [{
  visualizations: [{ type: #AS_LINEITEM }],
  requestAtLeast: [ 'CmmdtyHdgPlnExpsrCompanyCode', 'CompanyCodeName', ... ],
  groupBy: [ 'CmmdtyHdgPlnExpsrCompanyCode',
             'CompanyCodeName',              // Add name field here
             'CmmdtyHedgePlanExposureDCSID',
             'DerivativeContrSpecName',
             'CmmdtyHdgPlanExposureHedgeBook',
             'CmmdtyHedgeBookDescription',
             ... ]
}]
```

### 4. Add Name Field to `requestAtLeast`

Ensure the name field is always loaded by including it in `requestAtLeast`:

```abap
requestAtLeast: [ 'CmmdtyHdgPlnExpsrCompanyCode', 
                  'CompanyCodeName',         // Add name field here
                  'CmmdtyHedgePlanExposureDCSID',
                  'DerivativeContrSpecName',
                  ... ]
```

---

## Complete Example for Multiple Fields

### CDS Consumption View

```abap
define root view C_CmmdtyHedgeTradeOrderCockpit
  with parameters
    ...
  as select distinct from I_CmmdtyHedgeTradeOrderCockpit(...)
  association [0..1] to I_CompanyCode as _CompanyCode 
    on $projection.CmmdtyHdgPlnExpsrCompanyCode = _CompanyCode.CompanyCode
{
  // Company Code
  @ObjectModel.text.element: ['CompanyCodeName']
  key CmmdtyHdgPlnExpsrCompanyCode,
  @Semantics.text: true
  _CompanyCode.CompanyCodeName as CompanyCodeName,
  
  // DCS (Derivative Contract Specification)
  @ObjectModel.text.element: ['DerivativeContrSpecName']
  key CmmdtyHedgePlanExposureDCSID,
  @Semantics.text: true
  DerivativeContrSpecName,
  
  // Hedge Book
  @ObjectModel.text.element: ['CmmdtyHedgeBookDescription']
  key CmmdtyHdgPlanExposureHedgeBook,
  @Semantics.text: true
  CmmdtyHedgeBookDescription,
  
  // ... rest of your fields
}
```

### Metadata Extension

```abap
@Metadata.layer: #CORE
@UI: {
  headerInfo: { typeName: 'Trade Order',
                typeNamePlural: 'Trade Orders' },
  lineItem: [{ criticality: 'CmmdtyHdgSpecOrderCriticality' }]
}

@UI.presentationVariant: [{
  visualizations: [{ type: #AS_LINEITEM }],
  
  requestAtLeast: [ 
    'CmmdtyHdgPlnExpsrCompanyCode', 'CompanyCodeName',
    'CmmdtyHedgePlanExposureDCSID', 'DerivativeContrSpecName',
    'CmmdtyHdgPlanExposureHedgeBook', 'CmmdtyHedgeBookDescription',
    // ... other fields
  ],
  
  groupBy: [ 
    'CmmdtyHdgPlnExpsrCompanyCode', 'CompanyCodeName',
    'CmmdtyHedgePlanExposureDCSID', 'DerivativeContrSpecName',
    'CmmdtyHdgPlanExposureHedgeBook', 'CmmdtyHedgeBookDescription',
    // ... other fields
  ]
}]

annotate view C_CmmdtyHedgeTradeOrderCockpit with
{
  // Company Code
  @UI.textArrangement: #TEXT_LAST
  @EndUserText.label: 'Company Code'
  CmmdtyHdgPlnExpsrCompanyCode;
  
  @Semantics.text: true
  @Consumption.filter.hidden: true
  @UI.dataFieldDefault: [{hidden: true}]
  CompanyCodeName;
  
  // DCS
  @UI.textArrangement: #TEXT_LAST
  @EndUserText.label: 'DCS'
  CmmdtyHedgePlanExposureDCSID;
  
  @Semantics.text: true
  @Consumption.filter.hidden: true
  @UI.dataFieldDefault: [{hidden: true}]
  DerivativeContrSpecName;
  
  // Hedge Book
  @UI.textArrangement: #TEXT_LAST
  @EndUserText.label: 'Hedge Book'
  CmmdtyHdgPlanExposureHedgeBook;
  
  @Semantics.text: true
  @Consumption.filter.hidden: true
  @UI.dataFieldDefault: [{hidden: true}]
  CmmdtyHedgeBookDescription;
}
```

---

## Key Annotations Explained

### `@ObjectModel.text.element`
- Associates an ID field with its text/name field
- Placed on the ID field
- Value is the name of the text field (in square brackets)

### `@UI.textArrangement`
- Controls how ID and text are displayed
- Options:
  - `#TEXT_LAST` - Shows "ID (Name)"
  - `#TEXT_FIRST` - Shows "Name (ID)"
  - `#TEXT_ONLY` - Shows only "Name"
  - `#TEXT_SEPARATE` - Shows ID and Name in separate columns

### `@Semantics.text: true`
- Marks a field as a text/description field
- Placed on the name/description field

### `groupBy` (in presentationVariant)
- Defines which fields can be used for grouping and sorting in analytical tables
- Must include both ID and Name fields for separate sort options

### `requestAtLeast` (in presentationVariant)
- Ensures specified fields are always loaded
- Improves performance by pre-loading necessary data

---

## Checklist

When adding sortable ID/Name fields:

- [ ] Add text field to CDS view select list
- [ ] Add `@ObjectModel.text.element` annotation to ID field
- [ ] Add `@Semantics.text: true` to name field
- [ ] Add `@UI.textArrangement` to ID field in metadata extension
- [ ] Add name field to `groupBy` array
- [ ] Add name field to `requestAtLeast` array
- [ ] Hide name field from UI if needed (`@UI.dataFieldDefault: [{hidden: true}]`)

---

## Result

After implementing these changes, the column context menu will display:
- **Sort Ascending** (by ID)
- **Sort Descending** (by ID)
- **Sort Ascending** (by Name)
- **Sort Descending** (by Name)

---

## Common Associations for Standard Fields

| Field Type | Standard View Association | Text Field |
|------------|---------------------------|------------|
| Company Code | `I_CompanyCode` | `CompanyCodeName` |
| Currency | `I_Currency` | `CurrencyName` |
| Country | `I_Country` | `CountryName` |
| Plant | `I_Plant` | `PlantName` |
| Customer | `I_Customer` | `CustomerName` |
| Supplier | `I_Supplier` | `SupplierName` |

---

*Document created: December 8, 2025*
