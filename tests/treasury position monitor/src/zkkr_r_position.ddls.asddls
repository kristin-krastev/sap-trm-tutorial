@AbapCatalog.sqlViewName: 'ZKKRRPOS'
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Treasury Position Basic View'
@VDM.viewType: #BASIC

define view ZKKR_R_POSITION
  as select from ztrmpos as Position
  association [0..*] to ZKKR_R_CASHFLOW   as _Cashflow   on $projection.PositionID = _Cashflow.PositionID
  association [1..1] to ZKKR_R_INSTRUMENT as _Instrument on $projection.InstrumentID = _Instrument.InstrumentID
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
      _Cashflow,
      _Instrument
}