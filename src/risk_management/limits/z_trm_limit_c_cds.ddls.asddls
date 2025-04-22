@AbapCatalog.sqlViewName: 'ZTRMLIMITC'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Limit Monitoring Composite View'
@Analytics: {
    dataCategory: #CUBE,
    dataExtraction.enabled: true
}

define view Z_TRM_LIMIT_C_CDS
  as select from Z_TRM_LIMIT_I_CDS as Limit
  association [1..1] to I_Currency      as _Currency     on $projection.LimitCurrency = _Currency.Currency
  association [0..1] to I_BusinessUser  as _Approver     on $projection.ApproverID = _Approver.UserID
{
    key Limit.limit_uuid           as LimitUUID,
        Limit.limit_type          as LimitType,
        Limit.limit_category      as LimitCategory,
        Limit.object_id           as ObjectID,

        @Semantics.amount.currencyCode: 'LimitCurrency'
        Limit.limit_amount        as LimitAmount,

        @Semantics.currencyCode: true
        Limit.limit_currency      as LimitCurrency,

        Limit.valid_from          as ValidFrom,
        Limit.valid_to            as ValidTo,
        Limit.warning_threshold   as WarningThreshold,

        @Semantics.amount.currencyCode: 'LimitCurrency'
        Limit.utilized_amount     as UtilizedAmount,

        // Calculate utilization percentage
        @Semantics.quantity.unitOfMeasure: '%'
        cast(
          case when Limit.limit_amount > 0
            then division(Limit.utilized_amount, Limit.limit_amount, 2) * 100
            else 0
          end as abap.dec(5,2)
        )                         as UtilizationPercentage,

        // Limit status based on utilization
        case
            when Limit.utilized_amount >= Limit.limit_amount then 'BREACH'
            when Limit.utilized_amount >= (Limit.limit_amount * Limit.warning_threshold / 100) then 'WARNING'
            else 'NORMAL'
        end                       as LimitStatus,

        // Available amount
        @Semantics.amount.currencyCode: 'LimitCurrency'
        case
            when Limit.limit_amount > Limit.utilized_amount
                then Limit.limit_amount - Limit.utilized_amount
            else 0
        end                       as AvailableAmount,

        // Limit type description
        case Limit.limit_type
            when 'CPTY' then 'Counterparty Limit'
            when 'MKT'  then 'Market Risk Limit'
            when 'CTY'  then 'Country Limit'
            else 'Other'
        end                       as LimitTypeText,

        // Limit category description
        case Limit.limit_category
            when 'TRADING'    then 'Trading Limit'
            when 'SETTLEMENT' then 'Settlement Limit'
            when 'TOTAL'      then 'Total Exposure Limit'
            else 'Other'
        end                       as LimitCategoryText,

        // Validity status
        case
            when Limit.valid_to < $session.system_date then 'Expired'
            when Limit.valid_from > $session.system_date then 'Future'
            else 'Active'
        end                       as ValidityStatus,

        Limit.approval_status    as ApprovalStatus,
        Limit.approver_id        as ApproverID,
        Limit.approval_timestamp as ApprovalTimestamp,
        Limit.created_by         as CreatedBy,
        Limit.created_at         as CreatedAt,
        Limit.last_changed_by    as LastChangedBy,
        Limit.last_changed_at    as LastChangedAt,

        // Associations
        _Currency,
        _Approver
}