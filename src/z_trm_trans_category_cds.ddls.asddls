@AbapCatalog.sqlViewName: 'ZTRMTRANSCAT'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Treasury Transaction Categories'
@ObjectModel: {
    representativeKey: 'CategoryID',
    semanticKey: ['CategoryID'],
    hierarchyCategory: #PARENT_CHILD_RECURSIVE
}

define view Z_TRM_TRANS_CATEGORY_CDS
  as select from z_trm_trans_category
  association [0..1] to z_trm_trans_category as _ParentCategory
    on $projection.ParentCategory = _ParentCategory.category_id
{
  key category_id as CategoryID,
      @ObjectModel.association.type: [#PARENT]
      parent_category as ParentCategory,
      @Semantics.text: true
      description as Description,
      posting_key as PostingKey,
      gl_account as GLAccount,
      @ObjectModel.text.element: ['RiskCategoryText']
      risk_category as RiskCategory,
      active as Active,

      // Virtual Elements
      case risk_category
        when 'LOW' then 'Low Risk'
        when 'MED' then 'Medium Risk'
        when 'HIGH' then 'High Risk'
        else 'Unknown'
      end as RiskCategoryText,

      // Associations
      _ParentCategory
}