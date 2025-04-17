@AbapCatalog.sqlViewName: 'ZTRMBANKACCT'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Bank Account Management'
@Analytics: {
    dataCategory: #FACT,
    dataExtraction.enabled: true
}

define view Z_TRM_BANK_ACCOUNT_CDS
  as select from tfm_bank_account as BankAccount
    inner join   t012           as BankMaster on BankMaster.bankl = BankAccount.bank_id
    inner join   t012k          as BankKeys   on  BankKeys.bankl = BankMaster.bankl
                                             and BankKeys.banks = BankAccount.country
{
    // Bank Account Details
    key BankAccount.account_uuid     as AccountUUID,
    key BankAccount.company_code     as CompanyCode,
        BankAccount.account_id       as AccountID,
        BankAccount.bank_id          as BankID,
        BankAccount.country          as Country,

    // Bank Master Data
    @Semantics.text: true
    BankMaster.banka                as BankName,
    BankMaster.ort01                as BankCity,

    // Account Properties
    @Semantics.currencyCode: true
    BankAccount.currency            as AccountCurrency,
    BankAccount.account_type        as AccountType,

    // IBAN and SWIFT details
    BankKeys.iban                   as IBAN,
    BankKeys.swift                  as SWIFT,

    // Balance Information
    @Semantics.amount.currencyCode: 'AccountCurrency'
    @DefaultAggregation: #SUM
    BankAccount.current_balance     as CurrentBalance,

    @Semantics.amount.currencyCode: 'AccountCurrency'
    @DefaultAggregation: #SUM
    BankAccount.projected_balance   as ProjectedBalance,

    // Value Date Balance
    @Semantics.amount.currencyCode: 'AccountCurrency'
    @DefaultAggregation: #SUM
    case
        when dats_days_between($session.system_date, BankAccount.value_date) <= 0
            then BankAccount.current_balance
        else BankAccount.projected_balance
    end                            as ValueDateBalance,

    // Status Information
    BankAccount.account_status      as AccountStatus,
    BankAccount.last_statement_date as LastStatementDate,

    // Administrative Data
    @Semantics.user.createdBy: true
    BankAccount.created_by         as CreatedBy,
    @Semantics.systemDateTime.createdAt: true
    BankAccount.created_at         as CreatedAt,
    @Semantics.user.lastChangedBy: true
    BankAccount.last_changed_by    as LastChangedBy,
    @Semantics.systemDateTime.lastChangedAt: true
    BankAccount.last_changed_at    as LastChangedAt,

    // Virtual Elements
    @EndUserText.label: 'Days Since Last Statement'
    cast(
      dats_days_between(BankAccount.last_statement_date, $session.system_date)
      as abap.int4
    )                             as DaysSinceLastStatement,

    // Balance Status Indicator
    case
        when BankAccount.current_balance < 0 then 3  // Red - Negative Balance
        when BankAccount.current_balance < BankAccount.min_balance then 2  // Yellow - Below Minimum
        else 1  // Green - Healthy Balance
    end                          as BalanceStatusIndicator
}
where
  BankAccount.account_status <> 'CLOSED'