@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Interface View for Treasury Positions'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
  serviceQuality: #X,
  sizeCategory: #S,
  dataClass: #MASTER
}
define view entity ZI_TRM_Position
  as select from ztrmpos
  association [1..1] to ZI_TRM_Instrument as _Instrument on $projection.InstrumentID = _Instrument.InstrumentID
{
  key position_id    as PositionID,
      position_descr as PositionDescription,
      @ObjectModel.foreignKey.association: '_Instrument'
      instrument_id  as InstrumentID,
      @Semantics.amount.currencyCode: 'Currency'
      position_amount as Amount,
      currency       as Currency,
      @Semantics.businessDate.from: true
      valid_from     as ValidFrom,
      @Semantics.businessDate.to: true
      valid_to       as ValidTo,
      @Semantics.user.createdBy: true
      created_by     as CreatedBy,
      @Semantics.systemDateTime.createdAt: true
      created_at     as CreatedAt,
      @Semantics.user.lastChangedBy: true
      changed_by     as ChangedBy,
      @Semantics.systemDateTime.lastChangedAt: true
      changed_at     as ChangedAt,

      // Associations
      _Instrument
}