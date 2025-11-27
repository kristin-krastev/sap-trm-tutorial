@EndUserText.label: 'Position Consumption View'
@AccessControl.authorizationCheck: #NOT_REQUIRED
@Metadata.allowExtensions: true

@Search.searchable: true
define root view entity ZKKR_C_POSITION
  provider contract transactional_query
  as projection on ZKKR_I_POSITION
{
      @Search.defaultSearchElement: true
      @Search.fuzzinessThreshold: 0.8
  key position_id,

      @Search.defaultSearchElement: true
      position_descr,

      @Search.defaultSearchElement: true
      @Consumption.valueHelpDefinition: [{
        entity: {
          name: 'ZKKR_C_INSTRUMENT',
          element: 'instrument_id'
        },
        additionalBinding: [{
          localElement: 'currency',
          element: 'Currency',
          usage: #RESULT
        }]
      }]
      instrument_id,

      @Semantics.amount.currencyCode: 'currency'
      amount,

      @Consumption.valueHelpDefinition: [{
        entity: {
          name: 'I_Currency',
          element: 'Currency'
        }
      }]
      currency,

      valid_from,
      valid_to,

      /* Administrative fields */
      created_by,
      created_at,
      changed_by,
      changed_at,

      /* Virtual fields */
      AmountCriticality,

      /* Associations */
      _Cashflow : redirected to composition child ZKKR_C_CASHFLOW,
      _Instrument : redirected to ZKKR_C_INSTRUMENT
}
