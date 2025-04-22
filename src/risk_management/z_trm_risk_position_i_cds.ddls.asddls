@AbapCatalog.sqlViewName: 'ZTRMRISKPOSI'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Risk Position Interface View'

define view Z_TRM_RISK_POSITION_I_CDS
  as select from z_trm_risk_position
{
    key position_uuid,
        position_id,
        company_code,
        risk_type,
        counterparty_id,
        @Semantics.amount.currencyCode: 'position_currency'
        position_amount,
        @Semantics.currencyCode: true
        position_currency,
        position_date,
        @Semantics.amount.currencyCode: 'position_currency'
        risk_amount,
        confidence_level,
        time_horizon,
        @Semantics.user.createdBy: true
        created_by,
        @Semantics.systemDateTime.createdAt: true
        created_at,
        @Semantics.user.lastChangedBy: true
        last_changed_by,
        @Semantics.systemDateTime.lastChangedAt: true
        last_changed_at
}