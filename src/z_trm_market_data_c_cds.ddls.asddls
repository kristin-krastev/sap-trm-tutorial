@AbapCatalog.sqlViewName: 'ZTRMMARKETDC'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Market Data Composite View'
@Analytics: {
    dataCategory: #CUBE,
    dataExtraction.enabled: true
}

define view Z_TRM_MARKET_DATA_C_CDS
  as select from Z_TRM_MARKET_DATA_I_CDS as MarketData
  association [1..1] to I_Currency as _Currency on $projection.PriceCurrency = _Currency.Currency
{
    key MarketData.market_data_uuid as MarketDataUUID,
        MarketData.market_data_type as MarketDataType,
        MarketData.instrument_id    as InstrumentID,

        @Semantics.amount.currencyCode: 'PriceCurrency'
        MarketData.price_value     as PriceValue,

        @Semantics.currencyCode: true
        MarketData.price_currency  as PriceCurrency,

        MarketData.valid_from      as ValidFrom,
        MarketData.valid_to        as ValidTo,
        MarketData.data_source     as DataSource,
        MarketData.confidence_score as ConfidenceScore,
        MarketData.is_validated    as IsValidated,

        // Data quality status
        case
            when MarketData.confidence_score >= 0.95 then 'HIGH'
            when MarketData.confidence_score >= 0.80 then 'MEDIUM'
            else 'LOW'
        end                        as DataQualityStatus,

        // Validation status text
        case MarketData.is_validated
            when 'X' then 'Validated'
            else 'Pending Validation'
        end                        as ValidationStatus,

        // Market data type description
        case MarketData.market_data_type
            when 'FX' then 'Foreign Exchange Rate'
            when 'IR' then 'Interest Rate'
            when 'COMM' then 'Commodity Price'
            else 'Other'
        end                        as MarketDataTypeText,

        // Validity check
        case
            when MarketData.valid_to < $session.system_date then 'Expired'
            when MarketData.valid_from > $session.system_date then 'Future'
            else 'Active'
        end                        as ValidityStatus,

        MarketData.validation_timestamp as ValidationTimestamp,
        MarketData.validation_user     as ValidationUser,
        MarketData.created_by          as CreatedBy,
        MarketData.created_at          as CreatedAt,
        MarketData.last_changed_by     as LastChangedBy,
        MarketData.last_changed_at     as LastChangedAt,

        // Associations
        _Currency
}