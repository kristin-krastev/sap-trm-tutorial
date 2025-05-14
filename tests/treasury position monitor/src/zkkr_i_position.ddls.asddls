@AbapCatalog.sqlViewName: 'ZKKRIPOS'
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Treasury Position Interface View'
@VDM.viewType: #COMPOSITE
@ObjectModel.compositionRoot: true
@ObjectModel.transactional: true
@Metadata.allowExtensions: true

@ObjectModel: {
    semanticKey: ['position_id'],
    representativeKey: 'position_id'
}

@UI: {
    headerInfo: {
        typeName: 'Treasury Position',
        typeNamePlural: 'Treasury Positions',
        title: { type: #STANDARD, value: 'position_descr' }
    }
}

define view ZKKR_I_POSITION
  as select from ZKKR_R_POSITION as Position
  composition [0..*] of ZKKR_I_CASHFLOW as _Cashflow
  association [1..1] to ZKKR_I_INSTRUMENT as _Instrument on $projection.instrument_id = _Instrument.instrument_id
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
  key position_id,

      @UI.lineItem: [{ position: 20 }]
      @UI.identification: [{ position: 20 }]
      position_descr,

      @UI.lineItem: [{ position: 30 }]
      @UI.identification: [{ position: 30 }]
      @Consumption.valueHelpDefinition: [{entity: {name: 'ZKKR_I_INSTRUMENT', element: 'instrument_id'}}]
      instrument_id,

      @UI.lineItem: [{ position: 40 }]
      @UI.identification: [{ position: 40 }]
      @Semantics.amount.currencyCode: 'currency'
      position_amount as amount,

      @UI.lineItem: [{ position: 50 }]
      @UI.identification: [{ position: 50 }]
      currency,

      @UI.lineItem: [{ position: 60 }]
      @UI.identification: [{ position: 60 }]
      valid_from,

      @UI.lineItem: [{ position: 70 }]
      @UI.identification: [{ position: 70 }]
      valid_to,

      // Admin fields
      @UI.hidden: true
      created_by,
      @UI.hidden: true
      created_at,
      @UI.hidden: true
      changed_by,
      @UI.hidden: true
      changed_at,

      // Associations
      _Cashflow,
      _Instrument
}