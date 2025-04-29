@EndUserText.label : 'Treasury Position Cashflow'
@EndUserText.description : 'Database table for storing cashflows related to treasury positions'
@AbapCatalog.enhancement.category : #NOT_EXTENSIBLE
@AbapCatalog.tableCategory : #TRANSPARENT
@AbapCatalog.deliveryClass : #A
@AbapCatalog.dataMaintenance : #RESTRICTED
define table ztrmcf {

  key client        : abap.clnt not null;
  key cashflow_id   : abap.char(20) not null;
  key position_id   : abap.char(20) not null;
  cashflow_type     : abap.char(4);
  @Semantics.amount.currencyCode : 'ztrmcf.currency'
  cashflow_amount   : abap.curr(23,2);
  currency          : abap.cuky;
  value_date        : abap.dats;
  created_by        : syuname;
  created_at        : timestampl;
  changed_by        : syuname;
  changed_at        : timestampl;

}