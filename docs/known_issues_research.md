# Known Issues: Text Sorting in SAP Fiori Analytical Queries

## Common Causes Why Text Fields Don't Show Sort Options

### 1. **Text Field Not in groupBy (Analytical Views)**
**Symptom:** Sort by text option missing in column context menu  
**Cause:** In analytical queries with aggregation, only fields in `groupBy` are dimensions  
**Solution:** Add text field to `groupBy` array in `@UI.presentationVariant`

**Our Status:** We suspect this is the issue, but user says other fields work without it

---

### 2. **Text Field is a Measure, Not a Dimension**
**Symptom:** Text field has aggregation annotation making it a measure  
**Cause:** If text field has `@Aggregation.default: #SUM` or similar, it's treated as measure  
**Solution:** Use `@Aggregation.default: #MAX` or `#MIN` for text fields (or no aggregation in transactional views)

**Our Status:** We're using `#MAX` which should be correct

---

### 3. **@ObjectModel.sort.enabled: false**
**Symptom:** Sorting explicitly disabled  
**Cause:** Field has `@ObjectModel.sort.enabled: false` annotation  
**Solution:** Remove annotation or set to `true`

**Example from Define Hedge Book:**
```abap
@ObjectModel.sort.enabled: false
CommodityHedgeDeliveryPeriod,
```

**Action:** CHECK if DCS and Hedge Book text fields have this annotation!

---

### 4. **Text Field Not Exposed in OData Service**
**Symptom:** Field not visible to frontend  
**Cause:** Field might be filtered out by service definition or entity projection  
**Solution:** Verify field is in service binding and entity set

**Our Status:** Fields are in requestAtLeast, should be exposed

---

### 5. **Semantic Annotations Conflict**
**Symptom:** Wrong combination of semantic annotations  
**Cause:** `@Semantics.text: true` + wrong other annotations  
**Solution:** Ensure proper semantic layering

**Possible Conflicts:**
- `@Semantics.text: true` with `@Semantics.quantity` or `@Semantics.amount`
- Missing or incorrect `@EndUserText.label`

---

### 6. **Value Help Pointing to Wrong Element**
**Symptom:** Sort works on ID but not text  
**Cause:** Value help definition doesn't include text element properly

**Working Example (Define Hedge Book):**
```abap
// ID field has VH to ID
@Consumption.valueHelpDefinition: [{ entity: { name: 'C_CommodityHedgeDCSValueHelp', 
                                                element: 'DerivativeContrSpecification'} }]
DerivativeContrSpecification,

// TEXT field ALSO has VH to TEXT
@Consumption.valueHelpDefinition: [{ entity: { name: 'C_CommodityHedgeDCSValueHelp', 
                                                element: 'DerivativeContrSpecName'} }]
DerivativeContrSpecName,
```

**Action:** Check if Trade Order Cockpit text fields have their own value help definitions

---

### 7. **Hidden Fields in LineItem**
**Symptom:** Hidden fields might not be sortable  
**Cause:** UI framework doesn't enable sorting for hidden columns  
**Solution:** Make field visible or check if `@UI.hidden: true` vs `@UI.dataFieldDefault: [{hidden: true}]` matters

**Our Status:** Text fields are hidden in Trade Order Cockpit, visible in Define Hedge Book

---

### 8. **DISTINCT in View Definition**
**Symptom:** DISTINCT clause interferes with proper dimension handling  
**Cause:** `select distinct` can cause framework to lose track of individual field sortability  
**Solution:** Remove DISTINCT if not needed, or handle deduplication differently

**Our Status:** Trade Order Cockpit uses `select distinct`, Define Hedge Book doesn't

---

### 9. **Text Field Source Issues**
**Symptom:** Text comes from complex calculation or association  
**Cause:** If text field is calculated or comes from multi-level association, it might not be directly sortable  
**Solution:** Check source of text field in interface view

**Action:** Examine `I_CmmdtyHedgeTradeOrderCockpit` to see how text fields are sourced

---

### 10. **Parameter-Based View**
**Symptom:** View with parameters might have restrictions  
**Cause:** Parameterized views can have limitations on sorting  
**Solution:** Check if parameters affect field availability

**Our Status:** Trade Order Cockpit has parameters, Define Hedge Book doesn't

---

### 11. **Provider Contract Difference**
**Symptom:** Different provider contracts enable different features  
**Cause:** 
- `provider contract transactional_query` = full transactional features
- `provider contract analytical_query` = analytical features
- No provider contract = basic view

**Comparison:**
- Define Hedge Book: `provider contract transactional_query`
- Trade Order Cockpit: No provider contract specified

**This might be significant!**

---

### 12. **@UI.textArrangement on Text Field**
**Symptom:** Only ID sortable, not text  
**Cause:** Text field itself needs `@UI.textArrangement` annotation  
**Solution:** Add `@UI.textArrangement` to text field in metadata extension

**Define Hedge Book has:**
```abap
@UI.textArrangement: #TEXT_LAST
DerivativeContrSpecName;
```

**Trade Order Cockpit has:**
```abap
@Consumption.filter.hidden: true
@UI.dataFieldDefault: [{hidden: true}]
DerivativeContrSpecName;   // No textArrangement!
```

**Action:** Add `@UI.textArrangement: #TEXT_LAST` to text fields!

---

## Priority Actions to Try

### HIGH PRIORITY:
1. ✅ Remove `@ObjectModel.sort.enabled: false` if present on text fields
2. ✅ Add `@UI.textArrangement: #TEXT_LAST` to text fields in metadata extension
3. ✅ Add value help definitions to text fields (not just ID fields)
4. ✅ Make text fields visible (remove hidden: true) for testing

### MEDIUM PRIORITY:
5. Add text fields to groupBy in presentationVariant
6. Check source of text fields in interface view
7. Consider removing DISTINCT from view

### LOW PRIORITY:
8. Investigate provider contract differences
9. Test with parameters vs without

---

## Immediate Test:

Try this in your metadata extension:

```abap
@UI.textArrangement: #TEXT_LAST
@Consumption.filter.hidden: true
// Remove: @UI.dataFieldDefault: [{hidden: true}]
DerivativeContrSpecName;

@UI.textArrangement: #TEXT_LAST
@Consumption.filter.hidden: true
// Remove: @UI.dataFieldDefault: [{hidden: true}]
CmmdtyHedgeBookDescription;
```

If hidden fields can't be sorted, making them visible (even if not in lineItem) might enable sorting.
