# Solution Architecture: RAP-Compliant Overhedge Calculation

## 🎯 The Problem We Solved

**Original Issue:** READ ENTITIES in late save phase violates RAP contracts

**Solution:** Move READ ENTITIES to SAVE phase, pass data via buffer

---

## 📊 Complete Flow Diagram

```
┌────────────────────────────────────────────────────────────────────────┐
│  USER ACTION: Create/Update Counter Deal with Items                   │
└────────────────────┬───────────────────────────────────────────────────┘
                     │
                     ▼
┌────────────────────────────────────────────────────────────────────────┐
│  RAP SAVE SEQUENCE                                                     │
├────────────────────────────────────────────────────────────────────────┤
│                                                                        │
│  ┌──────────────────────────────────────────────────────────────────┐ │
│  │ PHASE 1: ADJUST                                                  │ │
│  │ - Modify entity values                                           │ │
│  │ - Auto-numbering, etc.                                           │ │
│  └──────────────────────────────────────────────────────────────────┘ │
│                     │                                                  │
│                     ▼                                                  │
│  ┌──────────────────────────────────────────────────────────────────┐ │
│  │ PHASE 2: SAVE (DETERMINATIONS & VALIDATIONS)                     │ │
│  │                                                                  │ │
│  │ ✅ Transactional buffer ACCESSIBLE                               │ │
│  │ ✅ READ ENTITIES ALLOWED                                         │ │
│  │                                                                  │ │
│  │ ┌────────────────────────────────────────────────────────────┐  │ │
│  │ │ Behavior Definition:                                       │  │ │
│  │ │ determination prepare_overhedge_items on save             │  │ │
│  │ │                      { create; update; }                   │  │ │
│  │ └────────────────┬───────────────────────────────────────────┘  │ │
│  │                  │ TRIGGERS                                      │ │
│  │                  ▼                                                │ │
│  │ ┌────────────────────────────────────────────────────────────┐  │ │
│  │ │ Handler Class: lhc_commoditycounterdealrequest            │  │ │
│  │ │ METHOD prepare_overhedge_items                            │  │ │
│  │ │                                                            │  │ │
│  │ │ 1. LOOP AT keys (each counter deal request)               │  │ │
│  │ │                                                            │  │ │
│  │ │ 2. READ ENTITIES ... BY _CntrdealItem                     │  │ │
│  │ │    ✅ Allowed here (SAVE phase)                           │  │ │
│  │ │    RESULT DATA(lt_cntrdeal_item)                          │  │ │
│  │ │                                                            │  │ │
│  │ │ 3. Store in buffer:                                       │  │ │
│  │ │    APPEND VALUE #(                                        │  │ │
│  │ │      counterdealrequestuuid = ls_key-uuid                 │  │ │
│  │ │      items = CORRESPONDING #( lt_cntrdeal_item )          │  │ │
│  │ │    ) TO mt_cntrdeal_items.  ← Static buffer               │  │ │
│  │ │                                                            │  │ │
│  │ │ ENDLOOP.                                                  │  │ │
│  │ └────────────────────────────────────────────────────────────┘  │ │
│  │                                                                  │ │
│  │         Static Buffer: mt_cntrdeal_items                        │ │
│  │         ┌─────────────────────────────────────────┐             │ │
│  │         │ UUID_1 → [ Item1, Item2, Item3 ]        │             │ │
│  │         │ UUID_2 → [ Item1, Item2 ]                │             │ │
│  │         │ UUID_3 → [ Item1, Item2, Item3, Item4 ]  │             │ │
│  │         └─────────────────────────────────────────┘             │ │
│  │                                                                  │ │
│  └──────────────────────────────────────────────────────────────────┘ │
│                     │                                                  │
│                     ▼                                                  │
│  ┌──────────────────────────────────────────────────────────────────┐ │
│  │ PHASE 3: CLEANUP_FINALIZE                                        │ │
│  │ - Final data adjustments                                         │ │
│  └──────────────────────────────────────────────────────────────────┘ │
│                     │                                                  │
│                     ▼                                                  │
│  ┌──────────────────────────────────────────────────────────────────┐ │
│  │ PHASE 4: LATE SAVE (SAVE_MODIFIED)                               │ │
│  │                                                                  │ │
│  │ ❌ Transactional buffer CLOSED                                   │ │
│  │ ❌ READ ENTITIES NOT ALLOWED                                     │ │
│  │ ✅ Direct DB access OK (SELECT)                                  │ │
│  │                                                                  │ │
│  │ ┌────────────────────────────────────────────────────────────┐  │ │
│  │ │ Handler Class: lhc_commoditycounterdealrequest            │  │ │
│  │ │ METHOD save_modified                                      │  │ │
│  │ │                                                            │  │ │
│  │ │ 1. LOOP AT create-commoditycounterdealrequest             │  │ │
│  │ │                                                            │  │ │
│  │ │ 2. READ TABLE mt_cntrdeal_items                           │  │ │
│  │ │    WITH KEY counterdealrequestuuid = uuid                 │  │ │
│  │ │    ASSIGNING <fs_items>.                                  │  │ │
│  │ │    ✅ Read from buffer (no EML needed)                    │  │ │
│  │ │                                                            │  │ │
│  │ │ 3. Call helper with items from buffer:                    │  │ │
│  │ │    cl_cmm_counterdeal_helper=>calculate_overhedge(        │  │ │
│  │ │      EXPORTING                                            │  │ │
│  │ │        is_overhedge     = ls_overhedge                    │  │ │
│  │ │        it_cntrdeal_item = <fs_items>-items  ← From buffer │  │ │
│  │ │      IMPORTING                                            │  │ │
│  │ │        es_overhedge = ls_result ).                        │  │ │
│  │ │    ✅ No READ ENTITIES needed!                            │  │ │
│  │ │                                                            │  │ │
│  │ │ 4. Process result (update DB, etc.)                       │  │ │
│  │ │                                                            │  │ │
│  │ │ ENDLOOP.                                                  │  │ │
│  │ │                                                            │  │ │
│  │ │ 5. CLEAR mt_cntrdeal_items.  ← Clean up buffer           │  │ │
│  │ └────────────────┬───────────────────────────────────────────┘  │ │
│  │                  │                                                │ │
│  │                  ▼                                                │ │
│  │ ┌────────────────────────────────────────────────────────────┐  │ │
│  │ │ Helper Class: CL_CMM_COUNTERDEAL_HELPER                   │  │ │
│  │ │ METHOD calculate_overhedge                                │  │ │
│  │ │                                                            │  │ │
│  │ │ IMPORTING                                                 │  │ │
│  │ │   is_overhedge      TYPE ty_is_overhedge                  │  │ │
│  │ │   it_cntrdeal_item  TYPE ty_t_cntrdeal_item  ← NEW!      │  │ │
│  │ │ EXPORTING                                                 │  │ │
│  │ │   es_overhedge      TYPE ty_es_overhedge                  │  │ │
│  │ │                                                            │  │ │
│  │ │ ❌ NO READ ENTITIES ANYMORE!                              │  │ │
│  │ │                                                            │  │ │
│  │ │ "Use items passed as parameter                            │  │ │
│  │ │ LOOP AT it_cntrdeal_item INTO DATA(ls_item).              │  │ │
│  │ │   lv_requestquantity +=                                   │  │ │
│  │ │     ls_item-financialtransactionquantity.                 │  │ │
│  │ │ ENDLOOP.                                                  │  │ │
│  │ │                                                            │  │ │
│  │ │ "Calculate overhedge                                      │  │ │
│  │ │ cl_cmm_hedgereq_helper=>calculate_after_parameters(       │  │ │
│  │ │   EXPORTING iv_requestquantity = lv_requestquantity       │  │ │
│  │ │             ...                                           │  │ │
│  │ │   IMPORTING es_after_selection = ls_result ).             │  │ │
│  │ │                                                            │  │ │
│  │ │ "Set output                                               │  │ │
│  │ │ es_overhedge-cntrdealaftovrhedgecriticality = ...         │  │ │
│  │ └────────────────────────────────────────────────────────────┘  │ │
│  │                                                                  │ │
│  └──────────────────────────────────────────────────────────────────┘ │
│                     │                                                  │
│                     ▼                                                  │
│  ┌──────────────────────────────────────────────────────────────────┐ │
│  │ COMMIT TO DATABASE                                               │ │
│  │ - All changes persisted                                          │ │
│  └──────────────────────────────────────────────────────────────────┘ │
│                                                                        │
└────────────────────┬───────────────────────────────────────────────────┘
                     │
                     ▼
┌────────────────────────────────────────────────────────────────────────┐
│  RESULT: Counter Deal Saved with Overhedge Calculation                │
│  ✅ No ATC violations                                                  │
│  ✅ RAP-compliant                                                      │
│  ✅ Same functionality                                                 │
└────────────────────────────────────────────────────────────────────────┘
```

---

## 🔄 Data Flow

```
┌─────────────────┐
│   User Input    │
│  Counter Deal   │
│   + 3 Items     │
└────────┬────────┘
         │
         ▼
┌─────────────────────────────────────────┐
│   SAVE PHASE                            │
│   determination prepare_overhedge_items │
├─────────────────────────────────────────┤
│                                         │
│   READ ENTITIES BY _CntrdealItem        │
│   ┌───────────────────────────────┐    │
│   │ Item 1: Qty = 100             │    │
│   │ Item 2: Qty = 200             │    │
│   │ Item 3: Qty = 300             │    │
│   └───────────────────────────────┘    │
│           │                             │
│           ▼                             │
│   Store in Buffer:                      │
│   mt_cntrdeal_items                     │
│   ┌───────────────────────────────┐    │
│   │ UUID_ABC → [100, 200, 300]    │    │
│   └───────────────────────────────┘    │
│                                         │
└─────────────────────────────────────────┘
         │
         ▼
┌─────────────────────────────────────────┐
│   LATE SAVE PHASE                       │
│   save_modified                         │
├─────────────────────────────────────────┤
│                                         │
│   READ TABLE mt_cntrdeal_items          │
│   ┌───────────────────────────────┐    │
│   │ UUID_ABC → [100, 200, 300]    │    │
│   └───────────────────────────────┘    │
│           │                             │
│           ▼                             │
│   calculate_overhedge(                  │
│     it_cntrdeal_item = [100,200,300] )  │
│           │                             │
│           ▼                             │
│   ┌───────────────────────────────┐    │
│   │ Total Qty: 600                │    │
│   │ Overhedge: 15%                │    │
│   │ Criticality: 2 (Warning)      │    │
│   └───────────────────────────────┘    │
│           │                             │
│           ▼                             │
│   Update Counter Deal entity            │
│                                         │
└─────────────────────────────────────────┘
         │
         ▼
┌─────────────────┐
│  DB: Persisted  │
│  Overhedge: 15% │
└─────────────────┘
```

---

## 🏗️ Component Structure

### 1. Behavior Definition (`R_CMMDTYHDGCNTRDEALREQUESTTP.bdef`)

```
┌──────────────────────────────────────────────┐
│  define behavior for                         │
│    R_CMMDTYHDGCNTRDEALREQUESTTP               │
│                                              │
│  {                                           │
│    create;                                   │
│    update;                                   │
│                                              │
│    ┌────────────────────────────────────┐   │
│    │ determination                      │   │
│    │   prepare_overhedge_items          │   │
│    │   on save { create; update; }      │   │
│    └────────────────────────────────────┘   │
│                                              │
│    association _CntrdealItem;                │
│  }                                           │
└──────────────────────────────────────────────┘
```

### 2. Handler Class (`lhc_commoditycounterdealrequest`)

```
┌──────────────────────────────────────────────────────────┐
│  CLASS lhc_commoditycounterdealrequest                   │
├──────────────────────────────────────────────────────────┤
│  PRIVATE SECTION.                                        │
│                                                          │
│  ┌────────────────────────────────────────────────────┐ │
│  │ STATIC BUFFER                                      │ │
│  │ CLASS-DATA: mt_cntrdeal_items TYPE ...             │ │
│  │                                                    │ │
│  │ Structure:                                         │ │
│  │ ┌──────────────────────────────────────┐          │ │
│  │ │ UUID_1 → [ items ]                   │          │ │
│  │ │ UUID_2 → [ items ]                   │          │ │
│  │ │ UUID_3 → [ items ]                   │          │ │
│  │ └──────────────────────────────────────┘          │ │
│  └────────────────────────────────────────────────────┘ │
│                                                          │
│  ┌────────────────────────────────────────────────────┐ │
│  │ SAVE PHASE METHOD                                  │ │
│  │ METHODS prepare_overhedge_items                    │ │
│  │   FOR DETERMINE ON SAVE.                           │ │
│  │                                                    │ │
│  │ • Reads items via READ ENTITIES ✅                 │ │
│  │ • Stores in buffer                                 │ │
│  └────────────────────────────────────────────────────┘ │
│                                                          │
│  ┌────────────────────────────────────────────────────┐ │
│  │ LATE SAVE METHOD                                   │ │
│  │ METHODS save_modified FOR MODIFY.                  │ │
│  │                                                    │ │
│  │ • Reads from buffer                                │ │
│  │ • Calls helper with items                          │ │
│  │ • No READ ENTITIES ✅                              │ │
│  └────────────────────────────────────────────────────┘ │
│                                                          │
└──────────────────────────────────────────────────────────┘
```

### 3. Helper Class (`CL_CMM_COUNTERDEAL_HELPER`)

```
┌──────────────────────────────────────────────────────────┐
│  CLASS CL_CMM_COUNTERDEAL_HELPER                         │
├──────────────────────────────────────────────────────────┤
│                                                          │
│  ┌────────────────────────────────────────────────────┐ │
│  │ METHOD calculate_overhedge                         │ │
│  │                                                    │ │
│  │ IMPORTING                                          │ │
│  │   is_overhedge     TYPE ty_is_overhedge            │ │
│  │   it_cntrdeal_item TYPE ty_t_cntrdeal_item ← NEW   │ │
│  │ EXPORTING                                          │ │
│  │   es_overhedge     TYPE ty_es_overhedge            │ │
│  │                                                    │ │
│  │ ❌ No READ ENTITIES                                │ │
│  │ ✅ Uses passed items                               │ │
│  │                                                    │ │
│  │ Logic:                                             │ │
│  │ 1. Get utilization details                         │ │
│  │ 2. Calculate "before" parameters                   │ │
│  │ 3. Sum quantities from it_cntrdeal_item            │ │
│  │ 4. Calculate "after" parameters                    │ │
│  │ 5. Return overhedge result                         │ │
│  └────────────────────────────────────────────────────┘ │
│                                                          │
└──────────────────────────────────────────────────────────┘
```

---

## ⚡ Key Benefits

### Before (Violated RAP):

```
┌────────────────────────────────────┐
│ LATE SAVE                          │
├────────────────────────────────────┤
│ calculate_overhedge()              │
│   │                                │
│   ├─ READ ENTITIES ❌              │
│   │  (Buffer closed)               │
│   │                                │
│   └─ Calculate                     │
│                                    │
│ Result: ATC Violation              │
└────────────────────────────────────┘
```

### After (RAP-Compliant):

```
┌────────────────────────────────────┐
│ SAVE PHASE                         │
├────────────────────────────────────┤
│ prepare_overhedge_items()          │
│   │                                │
│   ├─ READ ENTITIES ✅              │
│   │  (Buffer accessible)           │
│   │                                │
│   └─ Store in buffer               │
└────────────────────────────────────┘
         │
         ▼
┌────────────────────────────────────┐
│ LATE SAVE                          │
├────────────────────────────────────┤
│ save_modified()                    │
│   │                                │
│   ├─ Read from buffer ✅           │
│   │                                │
│   ├─ calculate_overhedge(items)    │
│   │   (No READ ENTITIES)           │
│   │                                │
│   └─ Update entity                 │
│                                    │
│ Result: 0 ATC Violations ✅        │
└────────────────────────────────────┘
```

---

## 📊 Comparison Matrix

| Aspect | Old Approach | New Approach |
|--------|--------------|--------------|
| **READ ENTITIES Location** | Late save ❌ | SAVE phase ✅ |
| **ATC Violations** | 5 high ❌ | 0 ✅ |
| **RAP Compliant** | No ❌ | Yes ✅ |
| **Data Access** | Direct (forbidden) | Via buffer ✅ |
| **Code Separation** | Tight coupling | Clean separation ✅ |
| **Testability** | Harder | Easier ✅ |
| **Performance** | Same | Same ✅ |
| **Functionality** | Works | Works ✅ |

---

## 🎯 Summary

### What Changed:

1. **Behavior Definition**
   - Added: `determination prepare_overhedge_items on save { create; update; }`

2. **Handler Class**
   - Added: Static buffer `mt_cntrdeal_items`
   - Added: Method `prepare_overhedge_items` (reads items in SAVE phase)
   - Modified: Method `save_modified` (uses buffer instead of READ ENTITIES)

3. **Helper Class**
   - Modified: `calculate_overhedge` now accepts `it_cntrdeal_item` parameter
   - Removed: All READ ENTITIES statements

### Result:

✅ **RAP-compliant**  
✅ **0 ATC violations**  
✅ **Same functionality**  
✅ **Clean architecture**  
✅ **Maintainable**  

---

## 🚀 Next: Apply to Other Helper Classes

Apply the same pattern to:

1. `CL_CMM_DESIGNATIONREQ_HELPER`
2. `CL_CMM_MIGRATIONREQUEST_HELPER`
3. `CL_CMM_RECLASSIFICATION_HELPER`
4. (5th class from ATC results)

Each follows the same structure:
- Add determination to behavior definition
- Create buffer in handler
- Move READ ENTITIES to SAVE phase
- Pass data to helper via parameter

