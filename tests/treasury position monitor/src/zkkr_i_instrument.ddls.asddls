@AbapCatalog.sqlViewName: 'ZKKRIINST'
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Treasury Instrument Interface View'
@VDM.viewType: #COMPOSITE
@Metadata.allowExtensions: true

@ObjectModel: {
    semanticKey: ['InstrumentID'],
    representativeKey: 'InstrumentID'
}

@UI: {
    headerInfo: {
        typeName: 'Financial Instrument',
        typeNamePlural: 'Financial Instruments',
        title: { type: #STANDARD, value: 'InstrumentDescription' }
    }
}

define view ZKKR_I_INSTRUMENT
  as select from ZKKR_R_INSTRUMENT as Instrument
{
      @UI.facet: [
          {
              id: 'InstrumentDetails',
              purpose: #STANDARD,
              type: #IDENTIFICATION_REFERENCE,
              label: 'Instrument Details',
              position: 10
          }
      ]

      @UI.lineItem: [{ position: 10 }]
      @UI.identification: [{ position: 10 }]
  key InstrumentID,

      @UI.lineItem: [{ position: 20 }]
      @UI.identification: [{ position: 20 }]
      InstrumentType,

      @UI.lineItem: [{ position: 30 }]
      @UI.identification: [{ position: 30 }]
      InstrumentDescription,

      @UI.lineItem: [{ position: 40 }]
      @UI.identification: [{ position: 40 }]
      @Semantics.amount.currencyCode: 'Currency'
      NominalAmount,

      @UI.lineItem: [{ position: 50 }]
      @UI.identification: [{ position: 50 }]
      Currency,

      @UI.lineItem: [{ position: 60 }]
      @UI.identification: [{ position: 60 }]
      InterestRate,

      @UI.lineItem: [{ position: 70 }]
      @UI.identification: [{ position: 70 }]
      StartDate,

      @UI.lineItem: [{ position: 80 }]
      @UI.identification: [{ position: 80 }]
      EndDate,

      @UI.lineItem: [{ position: 90 }]
      @UI.identification: [{ position: 90 }]
      Issuer,

      // Admin fields
      @UI.hidden: true
      CreatedBy,
      @UI.hidden: true
      CreatedAt,
      @UI.hidden: true
      ChangedBy,
      @UI.hidden: true
      ChangedAt
}