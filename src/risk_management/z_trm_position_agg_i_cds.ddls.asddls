@AbapCatalog.sqlViewName: 'ZTRMPOSAGGI'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Position Aggregation Interface View'

define view Z_TRM_POSITION_AGG_I_CDS
  as select from z_trm_position
  association [0..1] to z_trm_risk_limit as _Limit on $projection.object_id = _Limit.object_id
                                                  and $projection.risk_type = _Limit.limit_type
{
    key position_uuid,
        business_date,
        risk_type,           // 'MKT', 'CPTY', 'CTY'
        object_id,           // Counterparty, Country, etc.
        portfolio_id,        // Trading portfolio
        @Semantics.amount.currencyCode: 'position_currency'
        position_amount,
        @Semantics.currencyCode: true
        position_currency,
        @Semantics.amount.currencyCode: 'position_currency'
        risk_amount,         // VaR, CVaR, etc.
        confidence_level,    // For risk calculations
        time_horizon,        // Risk calculation horizon
        position_status,     // 'ACTIVE', 'SETTLED', 'CANCELLED'
        @Semantics.user.createdBy: true
        created_by,
        @Semantics.systemDateTime.createdAt: true
        created_at,
        @Semantics.user.lastChangedBy: true
        last_changed_by,
        @Semantics.systemDateTime.lastChangedAt: true
        last_changed_at,

        // Associations
        _Limit
}