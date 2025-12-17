# Quick Reference: Adding Determination to Behavior Definition

## 🎯 What You Need to Add

In your behavior definition file `R_CMMDTYHDGCNTRDEALREQUESTTP.bdef`, add this single line:

```abap
determination prepare_overhedge_items on save { create; update; }
```

---

## 📍 Where to Add It

### Location in Your Behavior Definition:

```abap
managed implementation in class zbp_r_cmmdtyhdgcntrdealreq unique;
strict ( 2 );

define behavior for R_CMMDTYHDGCNTRDEALREQUESTTP alias CommodityCounterDealRequest
persistent table FIN_CMM_CDREQ
draft table FIN_CMM_CDREQ_D
lock master
total etag LastChangedAt
authorization master ( instance )
etag master LocalLastChangedAt
{
  // Basic operations
  create;
  update;
  delete;
  
  // ========== ADD THIS LINE HERE ==========
  determination prepare_overhedge_items on save { create; update; }
  // ========================================
  
  // Validations (if any)
  validation validateXYZ on save { create; update; field SomeField; }
  
  // Field properties
  field ( readonly ) CounterDealRequestUUID;
  field ( mandatory ) CommodityHedgePlanExposureID;
  
  // Associations
  association _CntrdealItem { create; with draft; }
  
  // Actions
  action Approve result [1] $self;
  
  // Mapping
  mapping for FIN_CMM_CDREQ { /* ... */ }
}
```

---

## 📝 Syntax Explanation

### Full Syntax:

```abap
determination <method_name> on <timing> { <trigger_operations> }
```

### Components:

| Component | Value | Meaning |
|-----------|-------|---------|
| **method_name** | `prepare_overhedge_items` | Must match your method name in handler class |
| **timing** | `on save` | Runs during SAVE phase (before late save) ✅ |
| **trigger_operations** | `{ create; update; }` | Triggers on create and update operations |

---

## ⚙️ Common Variations

### Option 1: Trigger on Create Only
```abap
determination prepare_overhedge_items on save { create; }
```

### Option 2: Trigger on Update Only
```abap
determination prepare_overhedge_items on save { update; }
```

### Option 3: Trigger on Create and Update (Recommended for your case)
```abap
determination prepare_overhedge_items on save { create; update; }
```

### Option 4: Trigger on Specific Field Changes
```abap
determination prepare_overhedge_items on save { 
  create; 
  update; 
  field CommodityHedgePlanExposureID, CounterDealRequestDate; 
}
```

### Option 5: Trigger on Any Modify (Create or Update)
```abap
determination prepare_overhedge_items on modify { }
```

---

## 🎯 Recommended for Your Case

Use this:

```abap
determination prepare_overhedge_items on save { create; update; }
```

**Why?**
- `on save` = Runs in SAVE phase, before late save ✅
- `{ create; update; }` = Covers both new and modified requests ✅
- Allows READ ENTITIES (buffer still accessible) ✅

---

## ✅ Verification

After adding the line:

### 1. Save and Activate
- Save: Ctrl+S
- Activate: Ctrl+F3

### 2. Check for Errors
Look for these common errors:

**Error 1:** Method not found
```
Determination method prepare_overhedge_items not found in class zbp_r_cmmdtyhdgcntrdealreq
```
**Fix:** Check method name matches exactly (case-sensitive)

**Error 2:** Wrong entity
```
Determination prepare_overhedge_items not defined for entity CommodityCounterDealRequest
```
**Fix:** Check entity alias name

**Error 3:** Syntax error
```
Syntax error in determination declaration
```
**Fix:** Check syntax: `determination <name> on save { create; update; }`

### 3. Confirm in Handler Class

Your handler class should have this method signature:

```abap
METHODS prepare_overhedge_items FOR DETERMINE ON SAVE
  IMPORTING keys FOR CommodityCounterDealRequest~prepare_overhedge_items.
```

**Key points:**
- `FOR DETERMINE` = Marks it as determination method
- `ON SAVE` = Must match behavior definition timing
- `CommodityCounterDealRequest~prepare_overhedge_items` = Entity alias + method name

---

## 🔄 Complete Flow

### 1. User Action
```
User creates/updates counter deal with items
```

### 2. SAVE Phase (Determination Runs)
```abap
determination prepare_overhedge_items on save { create; update; }
↓
METHOD prepare_overhedge_items.
  READ ENTITIES ... " ✅ Allowed here
  " Store in buffer: mt_cntrdeal_items
ENDMETHOD.
```

### 3. LATE SAVE Phase
```abap
METHOD save_modified.
  " Read from buffer
  READ TABLE mt_cntrdeal_items ...
  
  " Call helper with items
  cl_cmm_counterdeal_helper=>calculate_overhedge(
    EXPORTING
      is_overhedge     = ...
      it_cntrdeal_item = <fs_items>-items  " From buffer
    IMPORTING
      es_overhedge = ... ).
  
  " No READ ENTITIES here ✅
ENDMETHOD.
```

---

## 🚀 Testing After Adding

### Quick Test:

1. **Activate** behavior definition
2. **Create** counter deal with items
3. **Set breakpoint** in `prepare_overhedge_items`
4. **Save** the counter deal
5. **Verify:** Breakpoint hits → Determination working ✅

---

## 📋 Other Determination Examples (Reference)

### Calculate Totals
```abap
determination calculateTotals on modify { field Amount, Quantity; }
```

### Set Default Values
```abap
determination setDefaults on save { create; }
```

### Validate Related Data
```abap
determination checkRelatedData on save { create; update; field ForeignKeyField; }
```

### Update Counters
```abap
determination updateCounters on save { update; }
```

---

## ⚡ One-Line Summary

**Add this to your behavior definition:**

```abap
determination prepare_overhedge_items on save { create; update; }
```

**That's it!** 🎉

---

## 🆘 Quick Troubleshooting

| Problem | Solution |
|---------|----------|
| Can't find behavior definition | Search for `R_CMMDTYHDGCNTRDEALREQUESTTP` in Eclipse/ADT |
| Activation fails | Check method exists in handler class with `FOR DETERMINE ON SAVE` |
| Determination not triggering | Check operations: `{ create; update; }` matches your use case |
| Wrong timing | Use `on save` (not `on modify`) for your scenario |
| Method name mismatch | Ensure exact match: `prepare_overhedge_items` (case-sensitive) |

---

## 📞 Need More Help?

See the full guide: `DETERMINATION_IMPLEMENTATION_GUIDE.md`

