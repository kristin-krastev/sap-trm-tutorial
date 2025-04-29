@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Interface View for Financial Instruments'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
  serviceQuality: #X,
  sizeCategory: #S,
  dataClass: #MASTER
}
define view entity ZI_TRM_Instrument
  as select from ztrminst
{
  key instrument_id   as InstrumentID,
      instrument_type as InstrumentType,
      instrument_descr as InstrumentDescription,
      @Semantics.amount.currencyCode: 'Currency'
      nominal_amount  as NominalAmount,
      @Semantics.currencyCode
      currency       as Currency,
      interest_rate  as InterestRate,
      @Semantics.businessDate.from: true
      start_date     as StartDate,
      @Semantics.businessDate.to: true
      end_date       as EndDate,
      issuer         as Issuer,
      @Semantics.user.createdBy: true
      created_by     as CreatedBy,
      @Semantics.systemDateTime.createdAt: true
      created_at     as CreatedAt,
      @Semantics.user.lastChangedBy: true
      changed_by     as ChangedBy,
      @Semantics.systemDateTime.lastChangedAt: true
      changed_at     as ChangedAt
}