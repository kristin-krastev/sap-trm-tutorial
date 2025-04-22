@AbapCatalog.sqlViewName: 'ZTRMPOSAGGU'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Position Aggregation UI View'
@Metadata.allowExtensions: true
@Search.searchable: true
@UI: {
    headerInfo: {
        typeName: 'Risk Position',
        typeNamePlural: 'Risk Positions',
        title: { type: #STANDARD, value: 'ObjectID' }
    }
}

define view Z_TRM_POSITION_AGG_UI_CDS
  as select from Z_TRM_POSITION_AGG_C_CDS as Position
{
    @UI.facet: [
        { id: 'Position',
          purpose: #STANDARD,
          type: #IDENTIFICATION_REFERENCE,
          label: 'Position Details',
          position: 10 },
        { id: 'Risk',
          purpose: #STANDARD,
          type: #FIELDGROUP_REFERENCE,
          label: 'Risk Information',
          position: 20,
          targetQualifier: 'RiskGroup' },
        { id: 'Limits',
          purpose: #STANDARD,
          type: #FIELDGROUP_REFERENCE,
          label: 'Limit Information',
          position: 30,
          targetQualifier: 'LimitGroup' }
    ]

    @UI.hidden: true
    key PositionUUID,

    @UI: {
        lineItem: [{ position: 10 }],
        identification: [{ position: 10 }]
    }
    @Search.defaultSearchElement: true
    ObjectID,

    @UI: {
        lineItem: [{ position: 20 }],
        identification: [{ position: 20 }]
    }
    BusinessDate,

    @UI: {
        lineItem: [{ position: 30 }],
        identification: [{ position: 30 }],
        fieldGroup: [{ qualifier: 'RiskGroup', position: 10 }]
    }
    @Search.defaultSearchElement: true
    RiskType,

    @UI: {
        lineItem: [{ position: 40 }],
        identification: [{ position: 40 }]
    }
    RiskTypeText,

    @UI: {
        lineItem: [{ position: 50 }],
        identification: [{ position: 50 }]
    }
    PortfolioID,

    @UI: {
        lineItem: [{ position: 60, criticality: 'RiskLevel' }],
        fieldGroup: [{ qualifier: 'RiskGroup', position: 20 }]
    }
    PositionAmount,

    @UI.hidden: true
    PositionCurrency,

    @UI: {
        lineItem: [{ position: 70, criticality: 'RiskLevel' }],
        fieldGroup: [{ qualifier: 'RiskGroup', position: 30 }]
    }
    RiskAmount,

    @UI: {
        fieldGroup: [{ qualifier: 'RiskGroup', position: 40 }]
    }
    ConfidenceLevel,

    @UI: {
        fieldGroup: [{ qualifier: 'RiskGroup', position: 50 }]
    }
    TimeHorizon,

    @UI: {
        lineItem: [{ position: 80 }],
        identification: [{ position: 80 }]
    }
    PositionStatus,

    @UI: {
        lineItem: [{ position: 90 }],
        identification: [{ position: 90 }]
    }
    PositionStatusText,

    @UI: {
        lineItem: [{ position: 100, criticality: 'RiskLevel' }],
        fieldGroup: [{ qualifier: 'LimitGroup', position: 10 }]
    }
    RiskLevel,

    @UI: {
        lineItem: [{ position: 110, criticality: 'RiskLevel' }],
        fieldGroup: [{ qualifier: 'LimitGroup', position: 20 }]
    }
    LimitUtilization,

    @UI: {
        fieldGroup: [{ qualifier: 'LimitGroup', position: 30 }]
    }
    AvailableLimit,

    @UI: {
        fieldGroup: [{ qualifier: 'RiskGroup', position: 60 }]
    }
    RiskRatio,

    @UI.hidden: true
    CreatedBy,

    @UI.hidden: true
    CreatedAt,

    @UI.hidden: true
    LastChangedBy,

    @UI.hidden: true
    LastChangedAt
}