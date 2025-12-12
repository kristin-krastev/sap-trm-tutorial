# Quick Reference - Treasury Position Monitor

## 🚀 What's New in Version 2.0

### Major Improvements
1. ✅ **Consumption Layer** - Clean separation of UI and business logic
2. ✅ **Metadata Extensions** - Flexible UI customization
3. ✅ **Enhanced Search** - Fuzzy search with multiple criteria
4. ✅ **Visual Indicators** - Color-coded amounts for quick insights
5. ✅ **Better UX** - Organized field groups and facets
6. ✅ **Service Layer** - OData V4 exposure for modern apps

---

## 📋 Quick Start

### Activate the Application

```abap
1. Activate all new CDS views:
   - ZKKR_C_POSITION
   - ZKKR_C_CASHFLOW
   - ZKKR_C_INSTRUMENT

2. Activate metadata extensions:
   - ZKKR_C_POSITION (DDLX)
   - ZKKR_C_CASHFLOW (DDLX)
   - ZKKR_C_INSTRUMENT (DDLX)

3. Activate behavior definition:
   - ZKKR_C_POSITION (BDEF)

4. Activate and publish service:
   - Service Definition: ZKKR_UI_POSITION
   - Service Binding: ZKKR_UI_POSITION_O4
```

### Access the Application

**Option 1: Fiori Launchpad**
```
Transaction: /UI2/FLP
App ID: (to be configured)
```

**Option 2: Direct URL**
```
/sap/bc/ui5_ui5/sap/zkkr_ui_position_o4/index.html
```

**Option 3: Preview in ADT**
```
Right-click on Service Binding → Preview
```

---

## 🎨 UI Features

### List Report (Position Overview)

**Filter Bar Fields**:
- Position ID
- Description
- Instrument ID
- Currency
- Valid From Date
- Valid To Date

**List Columns**:
- Position ID
- Description
- Instrument
- Amount (with color indicator)
- Currency
- Valid From
- Valid To

**Actions**:
- ➕ Create - Create new position
- ✏️ Edit - Edit selected position
- 🗑️ Delete - Delete selected position

### Object Page (Position Details)

**Header**:
- Position Amount (with criticality)
- Position Description

**Sections**:

1. **General Information**
   - Position ID (read-only)
   - Description
   - Amount (with color)
   - Currency

2. **Instrument Information**
   - Instrument ID (with value help)
   - View instrument details

3. **Validity Period**
   - Valid From (mandatory)
   - Valid To (optional)

4. **Cashflows**
   - Table of all related cashflows
   - Inline create/edit/delete
   - Sorted by value date

5. **Administrative Data**
   - Created By
   - Created At
   - Changed By
   - Changed At

---

## 🔍 Search Features

### Basic Search
Type in the search bar to find across:
- Position IDs
- Position descriptions
- Instrument IDs

**Example**: `"bond"` finds all positions with "bond" in description

### Advanced Filters

**By Currency**:
```
Currency = USD → Show only USD positions
```

**By Date Range**:
```
Valid From >= 01.01.2024
Valid To <= 31.12.2024
```

**By Instrument**:
```
Instrument ID = INST001
```

### Fuzzy Search
Enabled with 80% threshold:
- `"position"` matches "possition", "postion"
- `"instrument"` matches "instrumnt"

---

## 🎨 Color Indicators

### Amount Criticality

| Color | Meaning | Condition |
|-------|---------|-----------|
| 🟢 Green | Positive | Amount > 0 |
| 🔴 Red | Negative | Amount < 0 |
| ⚪ Grey | Zero/Empty | Amount = 0 or null |

**Applies to**:
- Position amounts (list & details)
- Cashflow amounts (table)

---

## ⌨️ Keyboard Shortcuts

| Shortcut | Action |
|----------|--------|
| `Ctrl + N` | Create new position |
| `Ctrl + S` | Save draft |
| `Enter` | Activate draft |
| `Escape` | Discard changes |
| `F4` | Open value help |
| `/` | Focus search bar |

---

## 📊 Data Model

```
POSITION (Root)
├── ID: Auto-generated (POS0000000001)
├── Description: Free text
├── Instrument: FK to Instrument
├── Amount: Decimal (23,2)
├── Currency: 3-char code
└── Valid From/To: Date range

    └── CASHFLOWS (Children)
        ├── ID: Auto-generated (CF0000000001)
        ├── Type: 4-char code
        ├── Amount: Decimal (23,2)
        ├── Currency: 3-char code
        └── Value Date: Date

INSTRUMENT (Master Data)
├── ID: 20-char key
├── Type: 4-char code
├── Description: Free text
├── Nominal Amount: Decimal (23,2)
├── Currency: 3-char code
├── Interest Rate: Decimal (5,2)
├── Start/End Date: Date range
└── Issuer: 10-char code
```

---

## 🛠️ Developer Guide

### Adding a New Field

**Step 1**: Add to database table
```abap
// In ztrmpos.tabl.abap
new_field : abap.char(10);
```

**Step 2**: Add to basic view
```sql
-- In zkkr_r_position.ddls.asddls
new_field as NewField,
```

**Step 3**: Add to interface view
```sql
-- In zkkr_i_position.ddls.asddls
@Semantics.text: true
NewField,
```

**Step 4**: Add to consumption view
```sql
-- In zkkr_c_position.ddls.asddls
@Search.defaultSearchElement: true
NewField,
```

**Step 5**: Add UI annotations
```abap
// In zkkr_c_position.ddlx.asddlx
@UI.lineItem: [{ position: 75 }]
@UI.fieldGroup: [{
  qualifier: 'GeneralInfo',
  position: 50
}]
NewField;
```

### Changing UI Layout

**To change field order**:
Edit `position` values in metadata extension

**To add a new facet**:
```abap
{
  id: 'MyNewSection',
  purpose: #STANDARD,
  type: #FIELDGROUP_REFERENCE,
  label: 'My New Section',
  targetQualifier: 'MyNewGroup',
  position: 25
}
```

**To change default sorting**:
```abap
@UI.presentationVariant: [{
  sortOrder: [{
    by: 'created_at',
    direction: #DESC
  }]
}]
```

---

## 🧪 Testing Scenarios

### Scenario 1: Create Position
1. Click "Create"
2. Enter description
3. Select instrument (use F4)
4. Enter amount and currency
5. Set valid from date
6. Save

**Expected**: Position ID auto-generated

### Scenario 2: Add Cashflows
1. Open a position
2. Go to Cashflows section
3. Click "+"
4. Enter type, amount, currency, date
5. Save

**Expected**: Cashflow ID auto-generated

### Scenario 3: Validation Testing
1. Try to save position without instrument
   - **Expected**: Error "Instrument is mandatory"

2. Try to set Valid To before Valid From
   - **Expected**: Error "Valid-to must be after valid-from"

3. Try to enter negative amount
   - **Expected**: Allowed, but shown in red

### Scenario 4: Search Testing
1. Search for "bond"
   - **Expected**: All positions with "bond" in description

2. Filter by currency "EUR"
   - **Expected**: Only EUR positions

3. Use fuzzy search "instrumnt"
   - **Expected**: Matches "instrument"

---

## ❓ FAQ

**Q: Why is the instrument field showing no results?**
A: Ensure instrument master data exists in table `ztrminst`

**Q: How do I delete a cashflow?**
A: Click the trash icon in the cashflow line item (object page)

**Q: Can I edit multiple positions at once?**
A: Not yet - planned for future release (mass editing)

**Q: Why don't I see the color indicators?**
A: Check that `AmountCriticality` field is exposed in consumption view

**Q: How do I export to Excel?**
A: Use the "Export" button in the list report toolbar

---

## 🐛 Troubleshooting

### Issue: "View not found" error
**Solution**: Activate all CDS views in correct order:
1. Basic views (R_)
2. Interface views (I_)
3. Consumption views (C_)

### Issue: UI annotations not showing
**Solution**: 
1. Check metadata extension is activated
2. Clear browser cache
3. Re-publish service binding

### Issue: Value help not working
**Solution**:
1. Verify `@Consumption.valueHelpDefinition` in consumption view
2. Ensure target entity is exposed in service
3. Check authorization for target entity

### Issue: Criticality colors not appearing
**Solution**:
1. Verify `AmountCriticality` field in interface view
2. Check it's exposed in consumption view
3. Confirm `criticality` property in metadata extension

---

## 📚 Additional Resources

- **Detailed Documentation**: See `FIORI_IMPROVEMENTS.md`
- **Architecture Guide**: See `README.md` (if exists)
- **Test Cases**: See `ltcl_position.class.abap`
- **SAP Help**: https://help.sap.com/rap

---

## 📞 Support

For issues or questions:
1. Check this quick reference
2. Review `FIORI_IMPROVEMENTS.md`
3. Check SAP Help documentation
4. Contact your ABAP development team

---

**Last Updated**: November 27, 2025  
**Version**: 2.0  
**Status**: ✅ Production Ready
