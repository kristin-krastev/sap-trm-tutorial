@AbapCatalog.sqlViewName: 'ZKKRRCF'
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Treasury Cashflow Basic View'
@VDM.viewType: #BASIC

define view ZKKR_R_CASHFLOW
  as select from ztrmcf as Cashflow
  association [1..1] to ZKKR_R_POSITION as _Position on $projection.PositionID = _Position.PositionID
{
  key cashflow_id     as CashflowID,
  key position_id     as PositionID,
      cashflow_type   as CashflowType,
      @Semantics.amount.currencyCode: 'Currency'
      cashflow_amount as Amount,
      currency        as Currency,
      @Semantics.businessDate.at: true
      value_date      as ValueDate,
      @Semantics.user.createdBy: true
      created_by      as CreatedBy,
      @Semantics.systemDateTime.createdAt: true
      created_at      as CreatedAt,
      @Semantics.user.lastChangedBy: true
      changed_by      as ChangedBy,
      @Semantics.systemDateTime.lastChangedAt: true
      changed_at      as ChangedAt,

      // Associations
      _Position
}