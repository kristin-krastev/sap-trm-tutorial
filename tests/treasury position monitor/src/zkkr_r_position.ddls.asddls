@AbapCatalog.sqlViewName: 'ZKKRRPOS'
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Treasury Position Basic View'
@VDM.viewType: #BASIC

define view ZKKR_R_POSITION
  as select from ztrmpos as Position
  association [0..*] to ZKKR_R_CASHFLOW   as _Cashflow   on $projection.position_id = _Cashflow.position_id
  association [1..1] to ZKKR_R_INSTRUMENT as _Instrument on $projection.instrument_id = _Instrument.instrument_id
{
  key position_id,
      position_descr,
      @ObjectModel.foreignKey.association: '_Instrument'
      instrument_id,
      @Semantics.amount.currencyCode: 'currency'
      position_amount,
      currency,
      @Semantics.businessDate.from: true
      valid_from,
      @Semantics.businessDate.to: true
      valid_to,
      @Semantics.user.createdBy: true
      created_by,
      @Semantics.systemDateTime.createdAt: true
      created_at,
      @Semantics.user.lastChangedBy: true
      changed_by,
      @Semantics.systemDateTime.lastChangedAt: true
      changed_at,

      // Associations
      _Cashflow,
      _Instrument
}