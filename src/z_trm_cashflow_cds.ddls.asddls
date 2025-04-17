@AbapCatalog.sqlViewName: 'ZTRMCASHFLOW'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Treasury Cash Flow Analysis'
@Analytics: {
    dataCategory: #CUBE,
    dataExtraction.enabled: true
}
@VDM.viewType: #COMPOSITE

define view Z_TRM_CASHFLOW_CDS
  as select from tfm_cash_flow as CashFlow
    inner join   t001         as CompanyCode on CompanyCode.bukrs = CashFlow.bukrs
{
    // Dimensions
    key CashFlow.flow_uuid          as FlowUUID,
        CashFlow.bukrs             as CompanyCode,
        CashFlow.flow_type         as FlowType,
        CashFlow.business_date     as BusinessDate,
        CashFlow.value_date        as ValueDate,

    // Treasury-specific date handling
    case
        when dats_days_between($session.system_date, CashFlow.value_date) <= 0
            then 'PAST'
        when dats_days_between($session.system_date, CashFlow.value_date) <= 30
            then 'NEAR_TERM'
        when dats_days_between($session.system_date, CashFlow.value_date) <= 90
            then 'MEDIUM_TERM'
        else 'LONG_TERM'
    end                            as TimeBucket,

    // Amounts with currency conversion
    @Semantics.amount.currencyCode: 'FlowCurrency'
    CashFlow.flow_amount          as FlowAmount,

    @Semantics.currencyCode: true
    CashFlow.flow_currency        as FlowCurrency,

    // Converted amount in company code currency
    @Semantics.amount.currencyCode: 'CompanyCodeCurrency'
    currency_conversion(
        amount             => CashFlow.flow_amount,
        source_currency    => CashFlow.flow_currency,
        target_currency    => CompanyCode.waers,
        exchange_rate_date => CashFlow.value_date
    )                             as AmountInLocalCurrency,

    @Semantics.currencyCode: true
    CompanyCode.waers            as CompanyCodeCurrency,

    // Risk-related calculations
    @Semantics.amount.currencyCode: 'FlowCurrency'
    case CashFlow.flow_type
        when 'EXPECTED' then CashFlow.flow_amount * 0.9  // 90% probability
        when 'PLANNED'  then CashFlow.flow_amount * 0.7  // 70% probability
        else CashFlow.flow_amount
    end                          as RiskAdjustedAmount,

    // Administrative fields
    @Semantics.user.createdBy: true
    CashFlow.created_by         as CreatedBy,
    @Semantics.systemDateTime.createdAt: true
    CashFlow.created_at         as CreatedAt,
    @Semantics.user.lastChangedBy: true
    CashFlow.last_changed_by    as LastChangedBy,
    @Semantics.systemDateTime.lastChangedAt: true
    CashFlow.last_changed_at    as LastChangedAt,

    // Associations
    _CompanyCode
}