@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Interface View for Treasury Cashflows'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
  serviceQuality: #X,
  sizeCategory: #S,
  dataClass: #TRANSACTIONAL
}
define view entity ZI_TRM_Cashflow
  as select from ztrmcf
  association [1..1] to ZI_TRM_Position as _Position on $projection.PositionID = _Position.PositionID
{
  key cashflow_id    as CashflowID,
      @ObjectModel.foreignKey.association: '_Position'
      position_id    as PositionID,
      cashflow_type  as CashflowType,
      @Semantics.amount.currencyCode: 'Currency'
      cashflow_amount as Amount,
      currency       as Currency,
      @Semantics.businessDate.at: true
      value_date     as ValueDate,
      @Semantics.user.createdBy: true
      created_by     as CreatedBy,
      @Semantics.systemDateTime.createdAt: true
      created_at     as CreatedAt,
      @Semantics.user.lastChangedBy: true
      changed_by     as ChangedBy,
      @Semantics.systemDateTime.lastChangedAt: true
      changed_at     as ChangedAt,

      // Associations
      _Position
}