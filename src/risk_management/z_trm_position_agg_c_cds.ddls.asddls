@AbapCatalog.sqlViewName: 'ZTRMPOSAGGC'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Position Aggregation Composite View'
@Analytics: {
    dataCategory: #CUBE,
    dataExtraction.enabled: true
}

define view Z_TRM_POSITION_AGG_C_CDS
  as select from Z_TRM_POSITION_AGG_I_CDS as Position
  association [1..1] to I_Currency      as _Currency  on $projection.PositionCurrency = _Currency.Currency
  association [0..1] to z_trm_risk_limit as _Limit     on Position.object_id = _Limit.object_id
                                                      and Position.risk_type = _Limit.limit_type
{
    key Position.position_uuid     as PositionUUID,
        Position.business_date     as BusinessDate,
        Position.risk_type        as RiskType,
        Position.object_id        as ObjectID,
        Position.portfolio_id     as PortfolioID,

        @Semantics.amount.currencyCode: 'PositionCurrency'
        @DefaultAggregation: #SUM
        Position.position_amount  as PositionAmount,

        @Semantics.currencyCode: true
        Position.position_currency as PositionCurrency,

        @Semantics.amount.currencyCode: 'PositionCurrency'
        @DefaultAggregation: #SUM
        Position.risk_amount     as RiskAmount,

        Position.confidence_level as ConfidenceLevel,
        Position.time_horizon    as TimeHorizon,
        Position.position_status as PositionStatus,

        // Risk type description
        case Position.risk_type
            when 'MKT' then 'Market Risk'
            when 'CPTY' then 'Counterparty Risk'
            when 'CTY' then 'Country Risk'
            else 'Other'
        end                      as RiskTypeText,

        // Position status description
        case Position.position_status
            when 'ACTIVE' then 'Active Position'
            when 'SETTLED' then 'Settled Position'
            when 'CANCELLED' then 'Cancelled Position'
            else 'Unknown Status'
        end                      as PositionStatusText,

        // Risk level based on limit utilization
        case
            when _Limit.limit_amount > 0
                and cast(Position.position_amount as abap.dec(15,2)) /
                    cast(_Limit.limit_amount as abap.dec(15,2)) * 100 >= 100
                then 'HIGH'
            when _Limit.limit_amount > 0
                and cast(Position.position_amount as abap.dec(15,2)) /
                    cast(_Limit.limit_amount as abap.dec(15,2)) * 100 >= 80
                then 'MEDIUM'
            else 'LOW'
        end                      as RiskLevel,

        // Limit utilization percentage
        @Semantics.quantity.unitOfMeasure: '%'
        case when _Limit.limit_amount > 0
            then division(Position.position_amount, _Limit.limit_amount, 2) * 100
            else 0
        end                      as LimitUtilization,

        // Available limit amount
        @Semantics.amount.currencyCode: 'PositionCurrency'
        case when _Limit.limit_amount > Position.position_amount
            then _Limit.limit_amount - Position.position_amount
            else 0
        end                      as AvailableLimit,

        // Risk metrics
        @Semantics.quantity.unitOfMeasure: '%'
        division(Position.risk_amount, Position.position_amount, 4) * 100
                                as RiskRatio,

        Position.created_by      as CreatedBy,
        Position.created_at      as CreatedAt,
        Position.last_changed_by as LastChangedBy,
        Position.last_changed_at as LastChangedAt,

        // Associations
        _Currency,
        _Limit
}
where
    Position.position_status = 'ACTIVE'