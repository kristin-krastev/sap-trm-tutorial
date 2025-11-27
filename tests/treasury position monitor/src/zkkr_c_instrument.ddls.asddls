@EndUserText.label: 'Instrument Consumption View'
@AccessControl.authorizationCheck: #NOT_REQUIRED
@Metadata.allowExtensions: true

@Search.searchable: true
define view entity ZKKR_C_INSTRUMENT
  as projection on ZKKR_I_INSTRUMENT
{
      @Search.defaultSearchElement: true
      @Search.fuzzinessThreshold: 0.8
  key instrument_id,

      @Search.defaultSearchElement: true
      InstrumentType,

      @Search.defaultSearchElement: true
      InstrumentDescription,

      @Semantics.amount.currencyCode: 'Currency'
      NominalAmount,

      @Consumption.valueHelpDefinition: [{
        entity: {
          name: 'I_Currency',
          element: 'Currency'
        }
      }]
      Currency,

      InterestRate,
      StartDate,
      EndDate,
      Issuer,

      /* Administrative fields */
      CreatedBy,
      CreatedAt,
      ChangedBy,
      ChangedAt
}
