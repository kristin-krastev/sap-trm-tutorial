@AbapCatalog.sqlViewName: 'ZTRMRISKPOS'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Risk Position Monitoring'
@Analytics: {
    dataCategory: #CUBE,
    dataExtraction.enabled: true
}
@ObjectModel: {
    modelCategory: #BUSINESS_OBJECT,
    compositionRoot: true,
    transactionalProcessingEnabled: true,
    semanticKey: ['PositionID']
}

define view Z_TRM_RISK_POSITION_CDS
  as select from z_trm_risk_position as Position
  association [1..1] to I_CompanyCode      as _CompanyCode      on $projection.CompanyCode = _CompanyCode.CompanyCode
  association [1..1] to I_Currency         as _Currency         on $projection.PositionCurrency = _Currency.Currency
  association [0..1] to z_trm_counterparty as _Counterparty     on $projection.CounterpartyID = _Counterparty.counterparty_id
{
    key position_uuid          as PositionUUID,
        position_id           as PositionID,
        company_code          as CompanyCode,
        risk_type            as RiskType,
        counterparty_id      as CounterpartyID,

        @Semantics.amount.currencyCode: 'PositionCurrency'
        position_amount      as PositionAmount,

        @Semantics.currencyCode: true
        position_currency    as PositionCurrency,

        position_date       as PositionDate,

        @Semantics.amount.currencyCode: 'PositionCurrency'
        risk_amount        as RiskAmount,

        confidence_level   as ConfidenceLevel,
        time_horizon      as TimeHorizon,

        // Risk categorization
        case risk_type
            when 'MKT' then 'Market Risk'
            when 'CRD' then 'Credit Risk'
            when 'LIQ' then 'Liquidity Risk'
            else 'Other'
        end               as RiskTypeText,

        // Risk level calculation
        case
            when risk_amount <= position_amount * 0.1 then 'LOW'
            when risk_amount <= position_amount * 0.25 then 'MEDIUM'
            else 'HIGH'
        end               as RiskLevel,

        // Administrative data
        @Semantics.user.createdBy: true
        created_by        as CreatedBy,
        @Semantics.systemDateTime.createdAt: true
        created_at        as CreatedAt,
        @Semantics.user.lastChangedBy: true
        last_changed_by   as LastChangedBy,
        @Semantics.systemDateTime.lastChangedAt: true
        last_changed_at   as LastChangedAt,

        // Associations
        _CompanyCode,
        _Currency,
        _Counterparty
}