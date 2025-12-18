# ⚡ Quick Reference Checklist - ATC Fix

**Print this! Keep it next to you while working!**

---

## 🎯 THE 7-PHASE METHOD

```
1. Helper Class     → Remove READ ENTITIES, add parameter
2. Handler Class    → Add determination + buffer
3. Behavior Def     → Declare determination
4. Calc Class ⚠️   → Update ALL calls (DON'T FORGET!)
5. Test Classes     → Update calls, fix UUIDs
6. Transport        → Include ALL objects, release
7. Test System      → Import, WAIT 10 min, test
```

---

## 📋 TRANSPORT OBJECTS CHECKLIST

**⚠️ ALL of these MUST be in your transport:**

```
☐ CL_*_HELPER              (Helper class)
☐ CL_BP_*                  (Handler/Saver class)
☐ R_*.bdef                 (Behavior definition)
☐ CL_*_CALC                (Calculation/Exit class) ⚠️ CRITICAL!
☐ Any other callers        (Search for references!)
```

**How to verify:** SE09/SE10 → Your transport → "Objects" tab

---

## 🔍 FIND ALL CALLERS (Do this FIRST!)

```
1. Open helper class
2. Right-click method name → "References" → "Workspace"
3. Note EVERY class shown
4. Especially look for:
   - CL_*_CALC classes
   - Classes with "SADL" or "EXIT" in name
   - Any if_sadl_exit_calc_element_read implementations
```

**If you miss a caller → Runtime error in test system! ⚠️**

---

## ⚠️ CRITICAL: BATCHED EML (NO LOOPS!)

**❌ WRONG (causes "EML in loop" error):**
```abap
LOOP AT keys INTO DATA(ls_key).
  READ ENTITIES OF r_something  "← BAD!
    ...
ENDLOOP.
```

**✅ CORRECT (batched):**
```abap
" Collect UUIDs first
DATA lt_uuids TYPE STANDARD TABLE OF sysuuid_x16.
LOOP AT keys INTO DATA(ls_key).
  APPEND ls_key-uuid TO lt_uuids.
ENDLOOP.

" ONE READ ENTITIES for all
READ ENTITIES OF r_something
  ENTITY entity BY \_items
    FIELDS ( ... )
    WITH VALUE #( FOR uuid IN lt_uuids 
                  ( %tky-uuid = uuid ... ) )
  RESULT DATA(lt_all_items).
```

---

## 🎯 TYPE DEFINITION TEMPLATE

**In helper class (can be PRIVATE or PUBLIC):**
```abap
TYPES: BEGIN OF ty_item,
         itemuuid    TYPE sysuuid_x16,
         quantity    TYPE ftr_quan,
         requestuuid TYPE sysuuid_x16,  "← If calc class needs grouping
       END OF ty_item,
       ty_t_item TYPE STANDARD TABLE OF ty_item WITH DEFAULT KEY.
```

---

## 🎯 METHOD SIGNATURE TEMPLATE

**Helper class method:**
```abap
CLASS-METHODS calculate_something
  IMPORTING
    !is_input   TYPE ty_input
    !it_items   TYPE ty_t_item      "← NEW!
  EXPORTING
    !es_output  TYPE ty_output
  RAISING
    cx_sadl_exit.
```

---

## 🎯 DETERMINATION METHOD TEMPLATE

**In handler class (lhc_*):**

```abap
METHOD prepare_items.
  
  CLEAR mt_items.
  
  DATA lt_uuids TYPE STANDARD TABLE OF sysuuid_x16.
  LOOP AT keys INTO DATA(ls_key).
    APPEND ls_key-uuid TO lt_uuids.
  ENDLOOP.
  
  CHECK lt_uuids IS NOT INITIAL.
  
  " Try draft
  READ ENTITIES OF r_something
    ENTITY entity BY \_items
      FIELDS ( itemuuid quantity requestuuid )
      WITH VALUE #( FOR uuid IN lt_uuids 
                    ( %tky-uuid = uuid %tky-%is_draft = if_abap_behv=>mk-on ) )
    RESULT DATA(lt_all_items).
  
  " Fallback to active
  IF lt_all_items IS INITIAL.
    READ ENTITIES OF r_something
      ENTITY entity BY \_items
        FIELDS ( itemuuid quantity requestuuid )
        WITH VALUE #( FOR uuid IN lt_uuids ( %tky-uuid = uuid ) )
      RESULT lt_all_items.
  ENDIF.
  
  " Group by UUID
  LOOP AT lt_uuids INTO DATA(lv_uuid).
    DATA(lt_items_for_req) = VALUE ty_t_item(
      FOR item IN lt_all_items WHERE ( requestuuid = lv_uuid )
      ( itemuuid = item-itemuuid quantity = item-quantity requestuuid = item-requestuuid ) ).
    APPEND VALUE #( requestuuid = lv_uuid items = lt_items_for_req ) TO mt_items.
  ENDLOOP.
  
ENDMETHOD.
```

---

## 🎯 BUFFER STRUCTURE TEMPLATE

**In handler class PUBLIC SECTION:**

```abap
PUBLIC SECTION.

  TYPES: BEGIN OF ty_item,
           itemuuid    TYPE sysuuid_x16,
           quantity    TYPE ftr_quan,
           requestuuid TYPE sysuuid_x16,
         END OF ty_item,
         ty_t_item TYPE STANDARD TABLE OF ty_item WITH DEFAULT KEY.

  TYPES: BEGIN OF ty_items_by_req,
           requestuuid TYPE sysuuid_x16,
           items       TYPE ty_t_item,
         END OF ty_items_by_req,
         ty_tt_items_by_req TYPE STANDARD TABLE OF ty_items_by_req WITH DEFAULT KEY.

  CLASS-DATA: mt_items TYPE ty_tt_items_by_req.
```

---

## 🎯 SAVE_MODIFIED UPDATE TEMPLATE

**In saver class (lsc_*):**

```abap
METHOD save_modified.
  
  " ... existing code ...
  
  " Get items from buffer
  READ TABLE lhc_entity=>mt_items INTO DATA(ls_entry)
    WITH KEY requestuuid = ls_input-requestuuid.
  
  DATA(lt_items) = COND #( WHEN sy-subrc = 0 
                           THEN ls_entry-items
                           ELSE VALUE cl_xxx_helper=>ty_t_item( ) ).
  
  " Call helper WITH items
  cl_xxx_helper=>calculate_something(
    EXPORTING is_input = ls_input it_items = lt_items
    IMPORTING es_output = ls_output ).
  
  " ... existing code ...
  
  " At the end:
  CLEAR lhc_entity=>mt_items.
  
ENDMETHOD.
```

---

## 🎯 BEHAVIOR DEFINITION UPDATE

**In .bdef file:**

```
define behavior for Entity alias Entity
{
  create;
  update;
  
  determination prepare_items on save { create; update; }  "← ADD THIS
  
  ...
}
```

---

## 🎯 TEST CLASS UPDATE TEMPLATE

**BEFORE:**
```abap
cl_xxx_helper=>calculate_something(
  EXPORTING is_input = ls_input
  IMPORTING es_output = ls_output ).
```

**AFTER:**
```abap
DATA(lt_test_items) = VALUE cl_xxx_helper=>ty_t_item(
  ( itemuuid = cl_system_uuid=>create_uuid_x16_static( )
    quantity = 100
    requestuuid = ls_input-requestuuid ) ).

cl_xxx_helper=>calculate_something(
  EXPORTING is_input = ls_input it_items = lt_test_items
  IMPORTING es_output = ls_output ).
```

---

## 🎯 CALC CLASS UPDATE - KEY POINTS

1. **Build map structure** at top
2. **Collect all UUIDs** from it_original_data
3. **ONE batched READ ENTITIES** (outside any loop)
4. **Group items by UUID** into map
5. **Main loop:** Lookup items from map, call helper
6. **Pass it_items** to helper method

**See full template in COMPLETE_ATC_FIX_PLAYBOOK.md → Phase 4**

---

## ⏱️ TEST SYSTEM VERIFICATION

**WAIT 10 MINUTES after import! ⏱️**

Then check:
```
☐ SE24 → CL_*_HELPER → Check method signature (2 importing params?)
☐ SE24 → CL_*_CALC → Ctrl+F2 (syntax check clean?)
☐ Fiori app → Create request (works without ST22 dump?)
```

**If error appears immediately after import → WAIT! Give it 10 minutes!**

---

## 🚨 COMMON MISTAKES TO AVOID

```
❌ Forgetting calc class in transport
❌ READ ENTITIES in a loop
❌ Types not in PUBLIC section
❌ Using '1' or '001' for UUIDs in tests
❌ Not waiting after import (testing too soon)
❌ Missing requestuuid in type definition
❌ Not clearing buffer after use
❌ Syntax checking before 10-min wait
```

---

## 🎯 UUID GENERATION

**❌ WRONG:**
```abap
DATA(lv_uuid) = '1'.              " Syntax error!
DATA(lv_uuid) = '001'.            " Syntax error!
```

**✅ CORRECT:**
```abap
DATA(lv_uuid) = cl_system_uuid=>create_uuid_x16_static( ).
```

---

## ✅ SUCCESS CRITERIA

**You're done when ALL of these are ✅:**

```
ERX System:
☐ No syntax errors
☐ No activation errors
☐ ATC: No READ_IN_LATE_SAVE errors
☐ ATC: No "EML in loop" errors
☐ Unit tests pass
☐ Transport released

QM7/Test System (after 10-min wait):
☐ Transport imported (RC 0 or 4)
☐ Syntax check clean
☐ Method signature correct (2 params)
☐ Fiori app works
☐ No ST22 dumps
☐ ATC violations gone
```

---

## 📞 QUICK TROUBLESHOOTING

| Problem | Quick Fix |
|---------|-----------|
| "EML in loop" | Batch READ ENTITIES outside loop |
| "Missing parameter IT_ITEMS" | Update that caller! (probably calc class) |
| "Private attribute not accessible" | Move to PUBLIC SECTION |
| Test fails with "Actual 0" | Use create_uuid_x16_static() |
| ST22 right after import | WAIT 10 minutes! |
| Syntax error in test system | Check if helper class imported |

---

## 📞 EMERGENCY CHECKLIST

**If test system fails after import:**

```
1. ⏱️ WAIT 10 MINUTES (seriously!)
2. ☕ Get coffee/tea
3. 🔄 Try again
4. Still failing? Check:
   ☐ Is CL_*_HELPER in transport objects?
   ☐ Is CL_*_CALC in transport objects?
   ☐ Did import show RC 0 or 4 (not 8)?
   ☐ Can you syntax-check both classes?
   ☐ Does helper have 2 importing parameters?
5. If yes to all → Wait another 5 minutes
6. Still failing → Check import log in STMS
```

---

## 🎯 PHASE COMPLETION TRACKING

**For current class: _______________________**

```
☐ Phase 1: Helper       (30 min)
☐ Phase 2: Handler      (45 min)
☐ Phase 3: BDEF         (5 min)
☐ Phase 4: Calc ⚠️      (45 min)
☐ Phase 5: Tests        (30 min)
☐ Phase 6: Transport    (20 min)
☐ Phase 7: Test System  (30 min)

Started:  __________
Finished: __________
```

---

## 📚 FULL DOCUMENTATION

**For detailed explanations, see:**
- `/workspace/docs/COMPLETE_ATC_FIX_PLAYBOOK.md`
- `/workspace/docs/LESSONS_LEARNED_CALC_CLASS.md`
- `/workspace/docs/COUNTERDEAL_ATC_FIX_COMPLETE_GUIDE.md`

---

**Version:** 1.0  
**Date:** December 17, 2025  
**Status:** Production-Ready ✅

**Print this and keep it visible while coding!** 📄
