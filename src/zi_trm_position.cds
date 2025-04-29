@AbapCatalog.sqlViewName: 'ZITRMPOS'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Interface View for Treasury Position'

// Basic interface view for treasury positions
define view ZI_TRM_Position
  as select from ztrmpos
{
  key position_id      as PositionID,
      position_descr   as PositionDescription,
      instrument_id    as InstrumentID,
      @Semantics.amount.currencyCode: 'Currency'
      position_amount  as Amount,
      @Semantics.currencyCode: true
      currency        as Currency,
      @Semantics.businessDate.from: true
      valid_from      as ValidFrom,
      @Semantics.businessDate.to: true
      valid_to        as ValidTo,
      @Semantics.user.createdBy: true
      created_by      as CreatedBy,
      @Semantics.systemDateTime.createdAt: true
      created_at      as CreatedAt,
      @Semantics.user.lastChangedBy: true
      changed_by      as ChangedBy,
      @Semantics.systemDateTime.lastChangedAt: true
      changed_at      as ChangedAt
}
where
  valid_to >= $session.system_date