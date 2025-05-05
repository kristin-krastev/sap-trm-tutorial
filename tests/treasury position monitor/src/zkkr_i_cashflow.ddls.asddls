@AbapCatalog.sqlViewName: 'ZKKRICF'
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Treasury Cashflow Interface View'
@VDM.viewType: #COMPOSITE
@Metadata.allowExtensions: true

@ObjectModel: {
    semanticKey: ['CashflowID', 'PositionID'],
    representativeKey: 'CashflowID'
}

@UI: {
    headerInfo: {
        typeName: 'Cashflow',
        typeNamePlural: 'Cashflows',
        title: { type: #STANDARD, value: 'CashflowID' }
    }
}

define view entity ZKKR_I_CASHFLOW
  as select from ZKKR_R_CASHFLOW as Cashflow
  association [1..1] to ZKKR_I_POSITION as _Position on $projection.PositionID = _Position.PositionID
{
      @UI.facet: [
          {
              id: 'CashflowDetails',
              purpose: #STANDARD,
              type: #IDENTIFICATION_REFERENCE,
              label: 'Cashflow Details',
              position: 10
          }
      ]

      @UI.lineItem: [{ position: 10 }]
      @UI.identification: [{ position: 10 }]
  key CashflowID,

      @UI.lineItem: [{ position: 20 }]
      @UI.identification: [{ position: 20 }]
      @Consumption.valueHelpDefinition: [{entity: {name: 'ZKKR_I_POSITION', element: 'PositionID'}}]
  key PositionID,

      @UI.lineItem: [{ position: 30 }]
      @UI.identification: [{ position: 30 }]
      CashflowType,

      @UI.lineItem: [{ position: 40 }]
      @UI.identification: [{ position: 40 }]
      @Semantics.amount.currencyCode: 'Currency'
      Amount,

      @UI.lineItem: [{ position: 50 }]
      @UI.identification: [{ position: 50 }]
      Currency,

      @UI.lineItem: [{ position: 60 }]
      @UI.identification: [{ position: 60 }]
      ValueDate,

      // Admin fields
      @UI.hidden: true
      CreatedBy,
      @UI.hidden: true
      CreatedAt,
      @UI.hidden: true
      ChangedBy,
      @UI.hidden: true
      ChangedAt,

      // Associations
      _Position
}