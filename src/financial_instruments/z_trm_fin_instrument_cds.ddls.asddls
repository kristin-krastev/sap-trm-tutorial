@AbapCatalog.sqlViewName: 'ZTRMFININS'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Treasury Financial Instruments'
@Analytics: {
    dataCategory: #FACT,
    dataExtraction.enabled: true
}
@ObjectModel: {
    modelCategory: #BUSINESS_OBJECT,
    compositionRoot: true,
    transactionalProcessingEnabled: true,
    writeActivePersistence: 'z_trm_fin_instrument',
    createEnabled: true,
    updateEnabled: true,
    deleteEnabled: true
}

define view Z_TRM_FIN_INSTRUMENT_CDS
  as select from z_trm_fin_instrument as Instrument
  association [0..*] to z_trm_instrument_cond as _Conditions
    on $projection.InstrumentID = _Conditions.instrument_id
  association [0..1] to but000 as _BusinessPartner
    on $projection.CounterpartyID = _BusinessPartner.partner
{
  key instrument_id as InstrumentID,
      @ObjectModel.text.element: ['InstrumentTypeText']
      instrument_type as InstrumentType,
      description as Description,
      @Semantics.amount.currencyCode: 'Currency'
      nominal_amount as NominalAmount,
      @Semantics.currencyCode: true
      currency as Currency,
      start_date as StartDate,
      end_date as EndDate,
      @ObjectModel.text.element: ['InterestTypeText']
      interest_type as InterestType,
      @Semantics.quantity.unitOfMeasure: '%'
      interest_rate as InterestRate,
      @ObjectModel.text.element: ['FrequencyText']
      payment_frequency as PaymentFrequency,
      @ObjectModel.foreignKey.association: '_BusinessPartner'
      counterparty_id as CounterpartyID,
      @ObjectModel.text.element: ['StatusText']
      status as Status,

      // Administrative Data
      @Semantics.user.createdBy: true
      created_by as CreatedBy,
      @Semantics.systemDateTime.createdAt: true
      created_at as CreatedAt,
      @Semantics.user.lastChangedBy: true
      last_changed_by as LastChangedBy,
      @Semantics.systemDateTime.lastChangedAt: true
      last_changed_at as LastChangedAt,

      // Virtual Elements
      case instrument_type
        when 'BOND' then 'Bond'
        when 'LOAN' then 'Loan'
        when 'DEPO' then 'Deposit'
        when 'SWAP' then 'Interest Rate Swap'
        else 'Unknown'
      end as InstrumentTypeText,

      case interest_type
        when 'FIX' then 'Fixed Rate'
        when 'VAR' then 'Variable Rate'
        when 'ZERO' then 'Zero Coupon'
        else 'Unknown'
      end as InterestTypeText,

      case payment_frequency
        when 'M' then 'Monthly'
        when 'Q' then 'Quarterly'
        when 'S' then 'Semi-Annual'
        when 'Y' then 'Yearly'
        else 'Unknown'
      end as FrequencyText,

      case status
        when 'ACTV' then 'Active'
        when 'DRAF' then 'Draft'
        when 'CLSD' then 'Closed'
        when 'TERM' then 'Terminated'
        else 'Unknown'
      end as StatusText,

      // Business Partner Name
      _BusinessPartner.name1 as CounterpartyName,

      // Associations
      _Conditions,
      _BusinessPartner
}