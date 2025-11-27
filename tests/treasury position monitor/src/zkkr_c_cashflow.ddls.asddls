@EndUserText.label: 'Cashflow Consumption View'
@AccessControl.authorizationCheck: #NOT_REQUIRED
@Metadata.allowExtensions: true

@Search.searchable: true
define view entity ZKKR_C_CASHFLOW
  as projection on ZKKR_I_CASHFLOW
{
      @Search.defaultSearchElement: true
  key CashflowID,

      @Search.defaultSearchElement: true
  key PositionID,

      @Search.defaultSearchElement: true
      CashflowType,

      @Semantics.amount.currencyCode: 'Currency'
      Amount,

      @Consumption.valueHelpDefinition: [{
        entity: {
          name: 'I_Currency',
          element: 'Currency'
        }
      }]
      Currency,

      ValueDate,

      /* Administrative fields */
      CreatedBy,
      CreatedAt,
      ChangedBy,
      ChangedAt,

      /* Virtual fields */
      AmountCriticality,

      /* Associations */
      _Position : redirected to parent ZKKR_C_POSITION
}
