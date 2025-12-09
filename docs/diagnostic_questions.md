# Diagnostic Questions for DCS/Hedge Book Sort Issue

## Critical Question:
**Which other fields ARE working with sort by name?**

If other text fields work but DCS and Hedge Book don't, there must be something SPECIFIC about these two fields.

## Things to Check:

### 1. Are the working text fields already in groupBy?
Look at your current groupBy array and check if the text fields that ARE working are listed there.

### 2. Source of the text fields
**For DCS:**
- Where does `DerivativeContrSpecName` come from in the view hierarchy?
- Is it from a direct field or association?
- Check: `I_CmmdtyHedgeTradeOrderCockpit` - how is this field sourced?

**For Hedge Book:**
- Where does `CmmdtyHedgeBookDescription` come from?
- Is it from a direct field or association?

### 3. Field Type and Length
- Are DCS and Hedge Book text fields different types (STRING vs CHAR)?
- Are they exceptionally long?
- Do they have any special domain/data element restrictions?

### 4. Value Help Configuration
In your consumption view:
```abap
@Consumption.valueHelpDefinition:[{ entity : { name: 'C_CommodityHedgeDCSValueHelp',
                                                element: 'DerivativeContrSpecification' }}]
CmmdtyHedgePlanExposureDCSID,
```

**Question:** Is the value help pointing to the ID field, not the text field?
Compare to Define Hedge Book which has:
```abap
@Consumption.valueHelpDefinition: [{ entity: { name: 'C_CommodityHedgeDCSValueHelp', 
                                                element: 'DerivativeContrSpecName'} }]
DerivativeContrSpecName,
```

### 5. Check the actual View Definition
Look at `I_CmmdtyHedgeTradeOrderCockpit` and search for:
- How is `DerivativeContrSpecName` defined?
- How is `CmmdtyHedgeBookDescription` defined?
- Do they have any special annotations there?
- Are they calculated fields or direct fields?

### 6. Presentation Variant Visualization Type
Your Trade Order Cockpit has:
```abap
@UI.presentationVariant: [{
visualizations: [{ type: #AS_LINEITEM }],
```

Is this maybe forcing a specific table type that doesn't support text sorting for certain fields?

### 7. Hidden Fields Issue
In your metadata, both text fields are hidden:
```abap
@UI.dataFieldDefault: [{hidden: true}]
DerivativeContrSpecName;

@UI.dataFieldDefault: [{hidden: true}]
CmmdtyHedgeBookDescription;
```

**Question:** Are the working text fields also hidden, or are they visible in the UI?

## Hypothesis:

If other text fields work without being in groupBy, then either:

1. **Your view is NOT actually analytical** (despite having groupBy in presentationVariant)
2. **The working text fields ARE in groupBy** and you didn't realize it
3. **There's something specific about how DCS and Hedge Book text is sourced** that makes them different from the working fields

## Action Items:

1. Share an example of a field that DOES work with sort by text
2. Check if that field's text element is in your current groupBy
3. Compare how that working field's text is defined vs DCS/Hedge Book text
4. Check the interface view `I_CmmdtyHedgeTradeOrderCockpit` to see how these fields are sourced
