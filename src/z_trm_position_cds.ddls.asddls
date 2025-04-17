@AbapCatalog.sqlViewName: 'ZTRMPOSITION'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Treasury Position View'
@VDM.viewType: #BASIC

define view Z_TRM_POSITION_CDS
  as select from tfm_pos as Position   // Treasury Position table
    inner join   t001    as CompanyCode on CompanyCode.bukrs = Position.bukrs
{
  key Position.position_uuid as PositionUUID,
      Position.bukrs        as CompanyCode,
      Position.position_id  as PositionID,
      Position.instr_type   as InstrumentType,
      Position.amount       as Amount,
      Position.currency    as Currency,
      Position.value_date  as ValueDate,

      // Virtual Elements for Risk Calculation
      @Semantics.amount.currencyCode: 'Currency'
      cast(0 as abap.curr(23,2)) as RiskAmount,

      // Administrative Data
      @Semantics.user.createdBy: true
      Position.created_by    as CreatedBy,
      @Semantics.systemDateTime.createdAt: true
      Position.created_at    as CreatedAt,
      @Semantics.user.lastChangedBy: true
      Position.last_changed_by as LastChangedBy,
      @Semantics.systemDateTime.lastChangedAt: true
      Position.last_changed_at as LastChangedAt
}