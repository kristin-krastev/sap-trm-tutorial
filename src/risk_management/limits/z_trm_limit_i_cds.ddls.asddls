@AbapCatalog.sqlViewName: 'ZTRMLIMITI'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Limit Monitoring Interface View'

define view Z_TRM_LIMIT_I_CDS
  as select from z_trm_risk_limit
{
    key limit_uuid,
        limit_type,        // 'CPTY' (Counterparty), 'MKT' (Market), 'CTY' (Country)
        limit_category,    // 'TRADING', 'SETTLEMENT', 'TOTAL'
        object_id,         // Counterparty ID, Country Code, etc.
        @Semantics.amount.currencyCode: 'limit_currency'
        limit_amount,
        @Semantics.currencyCode: true
        limit_currency,
        valid_from,
        valid_to,
        warning_threshold, // Percentage for early warning
        @Semantics.amount.currencyCode: 'limit_currency'
        utilized_amount,
        approval_status,   // 'PENDING', 'APPROVED', 'REJECTED'
        approver_id,
        approval_timestamp,
        @Semantics.user.createdBy: true
        created_by,
        @Semantics.systemDateTime.createdAt: true
        created_at,
        @Semantics.user.lastChangedBy: true
        last_changed_by,
        @Semantics.systemDateTime.lastChangedAt: true
        last_changed_at
}