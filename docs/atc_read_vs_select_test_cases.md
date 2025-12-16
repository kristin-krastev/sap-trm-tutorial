# Test Cases: READ ENTITIES vs. SELECT in Late Save

**Issue:** Validating if SELECT can replace READ ENTITIES in CALCULATE_OVERHEDGE  
**Date:** December 16, 2025  
**Context:** RAP Contract Violation - READ_IN_LATE_SAVE

---

## 🎯 The Question

**Can we replace READ ENTITIES with SELECT in the late save phase?**

**Colleague 1 says:** Yes, SELECT works (he did it before)  
**Colleague 2 says:** Maybe not - SELECT might miss uncommitted data  

**We need:** Test cases to prove which is correct!

---

## 🔬 Test Scenarios to Validate

### Test Case 1: Simple Create (No Prior Modifications)

**Scenario:**
1. Create new reclassification request
2. Add items to request
3. Save
4. Overhedge calculation runs

**Expected Behavior:**
- Both READ ENTITIES and SELECT should work
- Data is clean, no prior modifications
- Should calculate correctly

**Test:**
```abap
METHOD test_simple_create_and_calculate.
  " Given: Create request with 2 items
  CREATE_REQUEST( quantity = 1000 ).
  ADD_ITEMS( quantities = VALUE #( ( 300 ) ( 400 ) ) ).
  
  " When: Save triggers overhedge calculation
  SAVE_REQUEST( ).
  
  " Then: Overhedge should be 300 (1000 - 700)
  ASSERT_OVERHEDGE( expected = 300 ).
ENDMETHOD.
```

**Result if SELECT works:** ✅ Pass  
**Result if SELECT fails:** ❌ Fail (can't see items)

---

### Test Case 2: Create + Modify Items (Critical Test)

**Scenario:**
1. Create reclassification request
2. Add items (quantities: 300, 400)
3. **User modifies item 1** from 300 → 500 (before save completes)
4. Save
5. Overhedge calculation runs

**Expected Behavior:**
- Overhedge should be 100 (1000 - 500 - 400)
- Must see the MODIFIED value (500), not original (300)

**Critical Question:**
- READ ENTITIES: Sees 500 ✅ (transactional buffer)
- SELECT: Sees ??? (might see 300 or nothing if not committed)

**Test:**
```abap
METHOD test_create_with_item_modification.
  " Given: Create request
  DATA(lv_request_id) = create_request( quantity = 1000 ).
  
  " Add items
  DATA(lv_item1_id) = add_item( request_id = lv_request_id 
                                 quantity = 300 ).
  DATA(lv_item2_id) = add_item( request_id = lv_request_id 
                                 quantity = 400 ).
  
  " CRITICAL: Modify item 1 BEFORE save completes
  MODIFY ENTITIES OF r_cmmdtyhdgrclassfctnrequesttp
    ENTITY rclassfctnitem
    UPDATE FIELDS ( financialtransactionquantity )
    WITH VALUE #( ( %key-itemid = lv_item1_id
                   financialtransactionquantity = 500 ) ).
  
  " When: Save triggers overhedge calculation
  SAVE_REQUEST( ).
  
  " Then: Overhedge should be 100 (1000 - 500 - 400)
  " NOT 300 (1000 - 300 - 400)
  ASSERT_OVERHEDGE( 
    expected = 100
    msg = 'Should see modified value 500, not original 300' ).
ENDMETHOD.
```

**Result if READ ENTITIES (correct):** ✅ Overhedge = 100  
**Result if SELECT (wrong):** ❌ Overhedge = 300 (sees old data)

---

### Test Case 3: Delete Items Before Save

**Scenario:**
1. Create request with items (quantities: 300, 400, 300)
2. **User deletes item 3** (300) before save completes
3. Save
4. Overhedge calculation runs

**Expected:**
- Overhedge should be 300 (1000 - 300 - 400)
- Must NOT see deleted item

**Critical Question:**
- READ ENTITIES: Doesn't see deleted item ✅
- SELECT: Might still see it in DB (not deleted yet)

**Test:**
```abap
METHOD test_item_deletion_before_save.
  " Given: Create request with 3 items
  DATA(lv_request_id) = create_request( quantity = 1000 ).
  add_item( request_id = lv_request_id quantity = 300 ).
  add_item( request_id = lv_request_id quantity = 400 ).
  DATA(lv_item3_id) = add_item( request_id = lv_request_id quantity = 300 ).
  
  " CRITICAL: Delete item 3 BEFORE save
  DELETE ENTITIES OF r_cmmdtyhdgrclassfctnrequesttp
    ENTITY rclassfctnitem
    WITH VALUE #( ( %key-itemid = lv_item3_id ) ).
  
  " When: Save triggers calculation
  SAVE_REQUEST( ).
  
  " Then: Should only count 2 items (700 total)
  " Overhedge = 1000 - 700 = 300
  ASSERT_OVERHEDGE( 
    expected = 300
    msg = 'Deleted item should not be counted' ).
ENDMETHOD.
```

**Result if READ ENTITIES:** ✅ Overhedge = 300  
**Result if SELECT:** ❌ Overhedge = 0 (sees all 3 items still)

---

### Test Case 4: Multiple Determinations Modify Data

**Scenario:**
1. Create request
2. Add items
3. **Another determination modifies item quantities** (e.g., applies conversion factor)
4. Overhedge calculation runs (later in save sequence)

**Expected:**
- Must see quantities AFTER other determination modified them

**Critical Question:**
- READ ENTITIES: Sees latest values ✅
- SELECT: Sees original DB values ❌

---

### Test Case 5: Concurrent Save Operations

**Scenario:**
1. User A creates request with items
2. User B creates different request with items
3. Both save simultaneously
4. Each calculation must see only their own items

**Expected:**
- Proper transaction isolation
- No data mixing between users

**Test:**
```abap
METHOD test_concurrent_saves.
  " Given: Two separate requests
  DATA(lv_request_a) = create_request( quantity = 1000 ).
  DATA(lv_request_b) = create_request( quantity = 2000 ).
  
  add_items_to_request( request = lv_request_a 
                        quantities = VALUE #( ( 300 ) ( 400 ) ) ).
  add_items_to_request( request = lv_request_b 
                        quantities = VALUE #( ( 800 ) ( 900 ) ) ).
  
  " When: Both save
  " Then: Each sees only their items
  " Request A: overhedge = 300 (1000 - 700)
  " Request B: overhedge = 300 (2000 - 1700)
ENDMETHOD.
```

---

## 📊 Test Result Matrix

| Test Case | READ ENTITIES | SELECT | Risk if SELECT |
|-----------|---------------|--------|----------------|
| 1. Simple Create | ✅ Works | ✅ Probably works | Low |
| 2. Modified Items | ✅ Sees changes | ❌ Sees old data | **HIGH** |
| 3. Deleted Items | ✅ Respects delete | ❌ Sees deleted | **HIGH** |
| 4. Chained Determinations | ✅ Sees latest | ❌ Sees original | **CRITICAL** |
| 5. Concurrent Saves | ✅ Isolated | ⚠️ Risk of mixing | **MEDIUM** |

---

## 💡 My Assessment

### Colleague 1's Approach (SELECT) Works IF:
✅ Items are never modified after creation  
✅ Items are never deleted during save  
✅ No other determinations modify item data  
✅ Simple, straightforward flow  

**Risk:** 🔴 **HIGH** - Will fail in complex scenarios (Tests 2-4)

### Colleague 2's Concern is VALID:
⚠️ SELECT bypasses RAP transactional buffer  
⚠️ Won't see uncommitted changes  
⚠️ Can cause data inconsistency  
⚠️ Breaks RAP architectural principles  

---

## 🎯 My Recommendation

### **Option 1: Refactor to Early Phase** (BEST)
**As we documented earlier** - Move READ to determination phase:
- ✅ Proper RAP architecture
- ✅ No transaction isolation issues
- ✅ Sees all modifications correctly
- ✅ Test Cases 1-5 all pass

### **Option 2: SELECT (ONLY if these conditions are true)**
✅ Items are NEVER modified after creation  
✅ Items are NEVER deleted  
✅ NO other determinations touch item data  
✅ You can GUARANTEE simple flow  

**Required:** Test Cases 1-5 to validate assumptions

### **Option 3: Hybrid Approach**
```abap
" Try READ ENTITIES first (should not be in late save)
" If that fails, fall back to SELECT
" But this is a code smell - indicates architecture problem
```

---

## 🧪 Recommended Test Implementation

I can help you create:

1. **Unit test class** for the helper method
2. **Integration tests** for all 5 scenarios
3. **Comparison test:** Run same scenario with READ vs. SELECT
4. **Proof:** Show which approach handles all cases

**Want me to create the complete test class code?**

---

## 📊 About the 5 New ATC Errors

**You mentioned:** "Now there's 5 of them" (READ_IN_LATE_SAVE violations)

**This suggests:**
- Multiple methods/classes have same issue
- Systematic problem across the codebase
- Need coordinated fix strategy

**Can you share the 5 new findings?** 
- Class names?
- Method names?
- Are they all similar to the first one?

**This might need a broader solution strategy!**

---

## 📋 Summary & Plan for Tomorrow

### Today's Accomplishments ✅
- ✅ F5654: Backend metadata fixed (UX 3.0 working!)
- ✅ F5656: All issues addressed (PR created)
- ✅ 2 apps completed in 1 day!

### Tomorrow's Plan 🎯
- 🚀 F5657: Apply same UX 3.0 pattern
- 🚀 F6003: Apply same UX 3.0 pattern (if time)
- 🧪 ATC: Work on READ_IN_LATE_SAVE test cases

### ATC Strategy Discussion Needed
- 📊 Analyze all 5 violations
- 🧪 Create test cases for READ vs SELECT
- 🎯 Decide on solution approach
- 🔧 Implement systematically

---

## 💬 Quick Questions for Tomorrow

1. **Want me to create the test class** for validating READ vs. SELECT?
2. **Can you share the 5 ATC findings** so we can analyze them together?
3. **For tomorrow:** Start with F5657 or tackle ATC testing first?

---

**Great work today!** You're ahead of schedule! 🌟

See you tomorrow! 🚀