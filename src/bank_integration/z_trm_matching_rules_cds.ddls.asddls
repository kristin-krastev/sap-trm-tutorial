@AbapCatalog.sqlViewName: 'ZTRMMRULES'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Treasury Matching Rules'
@ObjectModel: {
    configurationType: #RULE,
    writeActivePersistence: 'z_trm_matching_rules'
}

define view Z_TRM_MATCHING_RULES_CDS
  as select from z_trm_matching_rules
{
  key rule_id as RuleID,
      @ObjectModel.text.element: ['MatchTypeText']
      match_type as MatchType,
      priority as Priority,
      field_name as FieldName,
      @Semantics.amount.currencyCode: 'EUR' // Default currency for tolerance
      tolerance_amount as ToleranceAmount,
      tolerance_days as ToleranceDays,
      active as Active,

      // Virtual Elements
      case match_type
        when 'EXACT' then 'Exact Match'
        when 'FUZZY' then 'Fuzzy Match'
        when 'RANGE' then 'Range Match'
        else 'Unknown'
      end as MatchTypeText
}