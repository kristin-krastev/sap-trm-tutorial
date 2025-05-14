@AbapCatalog.sqlViewName: 'ZTRMPOSITION'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Treasury Position'
@Metadata.allowExtensions: true
@ObjectModel.semanticKey: ['PositionID']
@Search.searchable: true

define root view Z_TRM_POSITION_CDS
  as select from ztrmpos
  association [0..*] to Z_TRM_CASHFLOW_CDS    as _Cashflows     on _Cashflows.PositionID = $projection.PositionID
  association [1..1] to Z_TRM_FIN_INSTRUMENT_CDS as _Instrument on _Instrument.InstrumentID = $projection.InstrumentID
{
    @ObjectModel.text.element: ['PositionDescription']
    @Search.defaultSearchElement: true
    key position_id as PositionID,

    @Search.defaultSearchElement: true
    @Search.fuzzinessThreshold: 0.7
    position_descr as PositionDescription,

    @Consumption.valueHelp: '_Instrument'
    instrument_id as InstrumentID,

    @Semantics.amount.currencyCode: 'Currency'
    position_amount as Amount,

    @Semantics.currencyCode: true
    currency as Currency,

    @Semantics.businessDate.at: true
    valid_from as ValidFrom,

    @Semantics.businessDate.to: true
    valid_to as ValidTo,

    @Semantics.user.createdBy: true
    created_by as CreatedBy,

    @Semantics.systemDateTime.createdAt: true
    created_at as CreatedAt,

    @Semantics.user.lastChangedBy: true
    changed_by as ChangedBy,

    @Semantics.systemDateTime.lastChangedAt: true
    changed_at as ChangedAt,

    // Associations
    _Cashflows,
    _Instrument
}
where
  valid_to >= $session.system_date