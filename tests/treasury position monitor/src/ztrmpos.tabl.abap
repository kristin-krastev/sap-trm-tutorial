@EndUserText.label : 'Treasury Position Master Data'
@EndUserText.description : 'Database table for storing treasury positions including financial instruments and amounts'
@AbapCatalog.enhancement.category : #NOT_EXTENSIBLE
@AbapCatalog.tableCategory : #TRANSPARENT
@AbapCatalog.deliveryClass : #A
@AbapCatalog.dataMaintenance : #RESTRICTED
define table ztrmpos {
  key client        : abap.clnt not null;
  key position_id   : abap.char(20) not null;
  position_descr    : abap.char(100);
  instrument_id     : abap.char(20);
  @Semantics.amount.currencyCode : 'ztrmpos.currency'
  position_amount   : abap.curr(23,2);
  currency          : abap.cuky;
  valid_from        : abap.dats;
  valid_to          : abap.dats;
  created_by        : syuname;
  created_at        : timestampl;
  changed_by        : syuname;
  changed_at        : timestampl;
}