@AbapCatalog.sqlViewName: 'ZKKRIPOS'
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Treasury Position Interface View'
@VDM.viewType: #COMPOSITE
@ObjectModel.compositionRoot: true
@ObjectModel.transactional: true
@Metadata.allowExtensions: true

@ObjectModel: {
    semanticKey: ['PositionID'],
    representativeKey: 'PositionID'
}

@UI: {
    headerInfo: {
        typeName: 'Treasury Position',
        typeNamePlural: 'Treasury Positions',
        title: { type: #STANDARD, value: 'PositionDescription' }
    }
}

define view ZKKR_I_POSITION
  as select from ZKKR_R_POSITION as Position
  composition [0..*] of ZKKR_I_CASHFLOW   as _Cashflow
  association [1..1] to ZKKR_I_INSTRUMENT as _Instrument on $projection.InstrumentID = _Instrument.InstrumentID
{
      @UI.facet: [
          {
              id: 'PositionDetails',
              purpose: #STANDARD,
              type: #IDENTIFICATION_REFERENCE,
              label: 'Position Details',
              position: 10
          },
          {
              id: 'Cashflows',
              purpose: #STANDARD,
              type: #LINEITEM_REFERENCE,
              label: 'Cashflows',
              position: 20,
              targetElement: '_Cashflow'
          }
      ]

      @UI.lineItem: [{ position: 10 }]
      @UI.identification: [{ position: 10 }]
  key PositionID,

      @UI.lineItem: [{ position: 20 }]
      @UI.identification: [{ position: 20 }]
      PositionDescription,

      @UI.lineItem: [{ position: 30 }]
      @UI.identification: [{ position: 30 }]
      @Consumption.valueHelpDefinition: [{entity: {name: 'ZKKR_I_INSTRUMENT', element: 'InstrumentID'}}]
      InstrumentID,

      @UI.lineItem: [{ position: 40 }]
      @UI.identification: [{ position: 40 }]
      @Semantics.amount.currencyCode: 'Currency'
      Amount,

      @UI.lineItem: [{ position: 50 }]
      @UI.identification: [{ position: 50 }]
      Currency,

      @UI.lineItem: [{ position: 60 }]
      @UI.identification: [{ position: 60 }]
      ValidFrom,

      @UI.lineItem: [{ position: 70 }]
      @UI.identification: [{ position: 70 }]
      ValidTo,

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
      _Cashflow,
      _Instrument
}