@EndUserText.label : 'Treasury Financial Instrument'
@EndUserText.description : 'Database table for storing financial instruments master data'
@AbapCatalog.enhancement.category : #NOT_EXTENSIBLE
@AbapCatalog.tableCategory : #TRANSPARENT
@AbapCatalog.deliveryClass : #A
@AbapCatalog.dataMaintenance : #RESTRICTED
define table ztrminst {

  key client          : abap.clnt not null;
  key instrument_id   : abap.char(20) not null;
  instrument_type     : abap.char(4);
  instrument_descr    : abap.char(100);
  @Semantics.amount.currencyCode : 'ztrminst.currency'
  nominal_amount      : abap.curr(23,2);
  currency           : abap.cuky;
  interest_rate      : abap.dec(5,2);
  start_date         : abap.dats;
  end_date           : abap.dats;
  issuer             : abap.char(10);
  created_by         : syuname;
  created_at         : timestampl;
  changed_by         : syuname;
  changed_at         : timestampl;

}