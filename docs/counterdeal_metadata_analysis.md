# Counter Deal Request Metadata Analysis

## Service: `UI_CMMDTYHDGCNTRDEALREQ` (cds_ui_cmmdtyhdgcntrdealreq)

---

## KEY FINDINGS

### 1. **DCS Field Configuration**

**ID Field:**
```xml
<Property Name="CmmdtyHedgePlanExposureDCSID" 
         Type="Edm.String" 
         MaxLength="20" 
         sap:text="DerivativeContrSpecName"
         sap:label="DCS"/>
```

**Text Field:**
```xml
<Property Name="DerivativeContrSpecName" 
         Type="Edm.String" 
         MaxLength="40" 
         sap:label="DCS Text"/>
```

✅ **Text association is correctly configured via `sap:text="DerivativeContrSpecName"`**

❌ **NO `sap:sortable="false"` attribute on `DerivativeContrSpecName`**

---

### 2. **Hedge Book Field Configuration**

**ID Field:**
```xml
<Property Name="CmmdtyHdgPlanExposureHedgeBook" 
         Type="Edm.String" 
         MaxLength="10" 
         sap:text="CmmdtyHedgeBookDescription"
         sap:label="Hedge Book"/>
```

**Text Field:**
```xml
<Property Name="CmmdtyHedgeBookDescription" 
         Type="Edm.String" 
         MaxLength="80" 
         sap:label="Hedge Book Description"/>
```

✅ **Text association is correctly configured via `sap:text="CmmdtyHedgeBookDescription"`**

❌ **NO `sap:sortable="false"` attribute on `CmmdtyHedgeBookDescription`**

---

### 3. **Company Code Field Configuration (WORKING)**

**ID Field:**
```xml
<Property Name="CmmdtyHdgPlnExpsrCompanyCode" 
         Type="Edm.String" 
         MaxLength="4" 
         sap:text="CmmdtyHdgPlnExpsrCoCodeText"
         sap:label="Company"/>
```

**Text Field:**
```xml
<Property Name="CmmdtyHdgPlnExpsrCoCodeText" 
         Type="Edm.String" 
         MaxLength="25" 
         sap:label="Company Name"/>
```

✅ **Text association is correctly configured**

❌ **NO `sap:sortable="false"` attribute on text field**

---

### 4. **Capabilities.SortRestrictions Analysis**

```xml
<Annotation Term="Capabilities.SortRestrictions">
  <Record>
    <PropertyValue Property="NonSortableProperties">
      <Collection>
        <PropertyPath>Approve_ac</PropertyPath>
        <PropertyPath>Cancel_ac</PropertyPath>
        <!-- ... other action/control fields ... -->
        <PropertyPath>CmmdtyHdgPlnExpsrQuantityUnit</PropertyPath>
        <PropertyPath>CmmdtyHdgPlanExposureDirection</PropertyPath>
        <PropertyPath>CmmdtyHdgPlnExpsrHedgingArea</PropertyPath>
        <!-- ... other calculated/virtual fields ... -->
      </Collection>
    </PropertyValue>
  </Record>
</Annotation>
```

### ✅ **CRITICAL: Text fields ARE NOT in NonSortableProperties!**

- ❌ `DerivativeContrSpecName` - **NOT listed** (should be sortable)
- ❌ `CmmdtyHedgeBookDescription` - **NOT listed** (should be sortable)
- ❌ `CmmdtyHdgPlnExpsrCoCodeText` - **NOT listed** (should be sortable)

---

## CRITICAL DIFFERENCE: Service Type

### Counter Deal Request Service:
```xml
<Annotation Term="Common.DraftRoot">
  <!-- TRANSACTIONAL service with Draft -->
</Annotation>
```
- ✅ **TRANSACTIONAL** (Draft-enabled)
- ✅ NO groupBy required
- ✅ All fields sortable by default (unless explicitly restricted)

### Trade Order Cockpit (Your Service):
- ❌ **ANALYTICAL** (with `select distinct` and aggregations)
- ❌ Requires `groupBy` for sortable dimensions
- ❌ Text fields must be explicitly included in `groupBy`

---

## METADATA VERDICT: ✅ PERFECT

**The Counter Deal Request metadata shows:**

1. ✅ Text associations configured (`sap:text` attribute)
2. ✅ Text fields NOT marked as non-sortable
3. ✅ Text fields NOT in `NonSortableProperties` collection
4. ✅ Transactional service (no groupBy required)

**Result:** All fields (ID and text) should be sortable in this service! 🎉

---

## WHY THIS MATTERS FOR YOUR ISSUE

### The Problem with Your Trade Order Cockpit:

Your service is **ANALYTICAL** (not transactional like Counter Deal Request), which means:

1. **Different sorting rules apply**
2. **Text fields MUST be in `groupBy`** to be sortable
3. **The metadata alone doesn't make them sortable** - the CDS view structure does

### What You Need to Check in YOUR Service:

```plaintext
1. Is your service ANALYTICAL? (uses select distinct, aggregations)
   ➜ If YES: Text fields MUST be in groupBy

2. Is your presentationVariant using groupBy?
   ➜ If YES: Add text fields to groupBy array

3. Are text fields marked with @Semantics.text: true?
   ➜ Check your CDS view annotations

4. Are text fields using @Aggregation.default: #MAX?
   ➜ Required for text fields in analytical queries
```

---

## NEXT STEPS FOR YOUR TRADE ORDER COCKPIT

The Counter Deal Request metadata is **perfect** because it's a **transactional** service.

Your Trade Order Cockpit needs **different handling** because it's **analytical**.

**We already fixed the CDS annotations - the issue is likely:**
1. Service not re-activated after CDS changes
2. Gateway cache not cleared
3. Metadata not regenerated
4. App cache not cleared in browser

Would you like me to create a **troubleshooting checklist** for getting your analytical service metadata updated?
