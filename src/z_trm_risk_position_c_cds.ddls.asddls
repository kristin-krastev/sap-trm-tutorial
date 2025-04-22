@AbapCatalog.sqlViewName: 'ZTRMRISKPOSC'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Risk Position Composite View'
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

define view Z_TRM_RISK_POSITION_C_CDS
  as select from Z_TRM_RISK_POSITION_I_CDS as Position
  association [1..1] to I_CompanyCode      as _CompanyCode  on $projection.CompanyCode = _CompanyCode.CompanyCode
  association [1..1] to I_Currency         as _Currency     on $projection.PositionCurrency = _Currency.Currency
  association [0..1] to z_trm_counterparty as _Counterparty on $projection.CounterpartyID = _Counterparty.counterparty_id
{
    key Position.position_uuid          as PositionUUID,
        Position.position_id           as PositionID,
        Position.company_code          as CompanyCode,
        Position.risk_type            as RiskType,
        Position.counterparty_id      as CounterpartyID,

        @Semantics.amount.currencyCode: 'PositionCurrency'
        Position.position_amount      as PositionAmount,

        @Semantics.currencyCode: true
        Position.position_currency    as PositionCurrency,

        Position.position_date       as PositionDate,

        @Semantics.amount.currencyCode: 'PositionCurrency'
        Position.risk_amount        as RiskAmount,

        Position.confidence_level   as ConfidenceLevel,
        Position.time_horizon      as TimeHorizon,

        // Risk categorization
        case Position.risk_type
            when 'MKT' then 'Market Risk'
            when 'CRD' then 'Credit Risk'
            when 'LIQ' then 'Liquidity Risk'
            else 'Other'
        end               as RiskTypeText,

        // Risk level calculation
        case
            when Position.risk_amount <= Position.position_amount * 0.1 then 'LOW'
            when Position.risk_amount <= Position.position_amount * 0.25 then 'MEDIUM'
            else 'HIGH'
        end               as RiskLevel,

        // Administrative data
        Position.created_by        as CreatedBy,
        Position.created_at        as CreatedAt,
        Position.last_changed_by   as LastChangedBy,
        Position.last_changed_at   as LastChangedAt,

        // Associations
        _CompanyCode,
        _Currency,
        _Counterparty
}