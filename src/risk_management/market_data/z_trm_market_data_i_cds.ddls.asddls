@AbapCatalog.sqlViewName: 'ZTRMMARKETDI'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Market Data Interface View'

define view Z_TRM_MARKET_DATA_I_CDS
  as select from z_trm_market_data
{
    key market_data_uuid,
        market_data_type,  // 'FX', 'IR', 'COMM'
        instrument_id,
        @Semantics.amount.currencyCode: 'price_currency'
        price_value,
        @Semantics.currencyCode: true
        price_currency,
        valid_from,
        valid_to,
        data_source,      // Bloomberg, Reuters, Manual, etc.
        confidence_score, // Data quality indicator
        is_validated,     // Validation status
        validation_timestamp,
        validation_user,
        @Semantics.user.createdBy: true
        created_by,
        @Semantics.systemDateTime.createdAt: true
        created_at,
        @Semantics.user.lastChangedBy: true
        last_changed_by,
        @Semantics.systemDateTime.lastChangedAt: true
        last_changed_at
}