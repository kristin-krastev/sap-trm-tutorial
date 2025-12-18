# Functional Test Case: Counter Deal Overhedge Calculation

## Test Information

**Test ID:** TC_CD_001  
**Test Name:** Counter Deal Creation with Overhedge Calculation  
**Component:** Counter Deal Request - Overhedge Calculation  
**Test Type:** Functional, Regression  
**Priority:** High  
**Tester:** [Your Name]  
**Date:** [Test Date]  

---

## Test Objective

Verify that counter deal requests can be created with items and that the overhedge calculation works correctly after implementing the READ ENTITIES fix.

---

## Prerequisites

### System Requirements:
- ✅ Development system access
- ✅ Authorization for counter deal creation (`CMM_CHCD` with activity 01)
- ✅ Valid exposure ID available
- ✅ Valid company code, DCS ID, and commodity data

### Data Requirements:
- ✅ At least one valid hedge plan exposure
- ✅ Financial transactions available for selection
- ✅ Valid pricing dates within exposure period

---

## Test Case 1: Basic Counter Deal Creation with Items

### Test Steps:

#### Step 1: Navigate to Counter Deal Creation
1. Open transaction or Fiori app for counter deal creation
2. Click "Create" or press the create button

**Expected Result:**  
✅ Counter deal creation screen opens

---

#### Step 2: Fill Header Data

**Input Values:**
| Field | Value | Example |
|-------|-------|---------|
| Exposure ID | [Select from value help] | `EXP-2024-001` |
| Counter Deal Date | Today's date or valid date | `17.12.2024` |
| Company Code | Valid company code | `1000` |
| Hedging Area | Valid hedging area | `001` |

**Expected Result:**  
✅ All fields accept input  
✅ Exposure details populate automatically  
✅ No validation errors  

---

#### Step 3: Add Counter Deal Items

**Add 3 items with the following quantities:**

| Item # | Financial Transaction | Quantity | Unit |
|--------|---------------------|----------|------|
| 1 | [Select from candidates] | 100 | MT |
| 2 | [Select from candidates] | 200 | MT |
| 3 | [Select from candidates] | 300 | MT |

**Actions:**
1. Click "Add Item" or select from proposed candidates
2. Enter/verify quantity for each item
3. Verify unit of measure

**Expected Result:**  
✅ All 3 items added successfully  
✅ Total quantity visible: **600 MT**  
✅ No errors during item addition  

---

#### Step 4: Save Counter Deal (Triggers Determination)

**Action:**
1. Click "Save" button
2. Wait for save process to complete

**Expected Result:**  
✅ Save successful - no errors  
✅ Counter deal ID generated  
✅ Status: "Created"  
✅ Success message displayed  

---

#### Step 5: Verify Overhedge Calculation

**Check these fields:**

| Field | Description | Expected Behavior |
|-------|-------------|-------------------|
| **Before - Utilization** | Current hedged quantity before this deal | Should show existing value + unit |
| **Target Quota** | Management quota from exposure | Should show target value |
| **Before - Overhedge** | Overhedge before this counter deal | Should show value + percentage |
| **After - Utilization** | Hedged quantity after this deal | Should show: Before + 600 MT |
| **After - Overhedge** | Overhedge after this counter deal | Should show calculated value + % |

**Sample Expected Values:**
```
Before Utilization:   1,000 MT (50%)
Target Quota:         2,000 MT
Before Overhedge:     0 MT (0%)

After Utilization:    1,600 MT (80%)  ← Before + 600
After Overhedge:      0 MT (0%)       ← Still within quota
```

**Verification Points:**
✅ "Before" section displays values  
✅ "After" section displays values  
✅ After Utilization = Before Utilization + 600  
✅ Overhedge calculation mathematically correct  
✅ Criticality indicators correct (green/yellow/red)  

---

## Test Case 2: Counter Deal Creation with Overhedge

### Objective:
Test overhedge scenario where total utilization exceeds target quota

### Test Steps:

#### Step 1-2: Repeat TC1 Steps 1-2
(Same as Test Case 1)

#### Step 3: Add Items that Exceed Target Quota

**Scenario:** Target Quota = 2,000 MT, Current Utilization = 1,800 MT

**Add items totaling 500 MT:**
| Item # | Quantity |
|--------|----------|
| 1 | 200 MT |
| 2 | 300 MT |

**Expected Calculation:**
```
Before Utilization:   1,800 MT (90%)
Target Quota:         2,000 MT
Before Overhedge:     0 MT (0%)

After Utilization:    2,300 MT (115%)  ← Exceeds quota!
After Overhedge:      300 MT (15%)     ← 2,300 - 2,000
```

#### Step 4: Save and Verify

**Expected Result:**  
✅ Save successful  
✅ Overhedge value shows 300 MT (15%)  
✅ Criticality indicator: **Red** (overhedge detected)  
✅ Warning message may display  

---

## Test Case 3: Counter Deal with No Items

### Objective:
Verify handling when no items are added

### Test Steps:

1. Create counter deal header (Steps 1-2 from TC1)
2. **Do NOT add any items**
3. Attempt to save

**Expected Result:**  
❌ Save fails with validation error  
❌ Error message: "At least one Counter Deal Item should exist"  
✅ User can add items and retry  

---

## Test Case 4: Draft Handling

### Objective:
Verify determination works with draft scenarios

### Test Steps:

1. Create counter deal header
2. Add items (as in TC1)
3. **Save as DRAFT** (do not activate)
4. Edit draft
5. Modify quantities
6. Save draft again
7. Activate draft

**Expected Result:**  
✅ Draft saves successfully  
✅ Overhedge calculation works in draft mode  
✅ Activation successful  
✅ Final overhedge values correct  

---

## Test Case 5: Update Existing Counter Deal

### Objective:
Verify UPDATE scenario (not just CREATE)

### Test Steps:

1. Open existing counter deal (Status: Created)
2. Add one more item (Quantity: 150 MT)
3. Save

**Expected Result:**  
✅ Save successful  
✅ Overhedge recalculated with new total  
✅ After Utilization updated correctly  
✅ Criticality updated if needed  

---

## Test Case 6: Edge Cases

### Test 6A: Large Quantities

**Input:** Items with very large quantities (e.g., 999,999 MT)

**Expected Result:**  
✅ System handles large numbers  
✅ No overflow errors  
✅ Calculation correct  

### Test 6B: Decimal Quantities

**Input:** Items with decimal quantities (e.g., 100.5 MT)

**Expected Result:**  
✅ Decimals accepted based on unit configuration  
✅ Calculation maintains precision  

### Test 6C: Multiple Save Operations

**Steps:**
1. Create counter deal with items
2. Save (first time)
3. Add more items
4. Save (second time)
5. Modify quantities
6. Save (third time)

**Expected Result:**  
✅ All saves successful  
✅ Overhedge recalculated each time  
✅ No buffer conflicts  

---

## Debugging Test Case

### Objective:
Verify the technical implementation during save

### Debug Points:

#### Breakpoint 1: In `prepare_overhedge_items`

**Location:** Handler class, `prepare_overhedge_items` method

**Set Breakpoint:** First line after method declaration

**Action:** Create counter deal with items and save

**Verify:**
1. ✅ Breakpoint hits during save
2. ✅ `keys` table contains counter deal UUID
3. ✅ READ ENTITIES executes successfully
4. ✅ `lt_cntrdeal_item` contains correct items:
   ```
   Item 1: UUID = xxx, Quantity = 100
   Item 2: UUID = yyy, Quantity = 200
   Item 3: UUID = zzz, Quantity = 300
   ```
5. ✅ Items appended to `mt_cntrdeal_items` buffer
6. ✅ Buffer structure:
   ```
   counterdealrequestuuid = ABC-123
   items = [ {100}, {200}, {300} ]
   ```

---

#### Breakpoint 2: In `save_modified`

**Location:** Saver class, `save_modified` method, before `READ TABLE`

**Verify:**
1. ✅ Breakpoint hits
2. ✅ `lhc_counterdealrequest=>mt_cntrdeal_items` is populated
3. ✅ Buffer contains data from determination
4. ✅ READ TABLE finds matching UUID
5. ✅ `<fs_items_create>` is assigned
6. ✅ `lt_items_create` contains correct items

---

#### Breakpoint 3: In `calculate_overhedge`

**Location:** Helper class, `calculate_overhedge` method, first line

**Verify:**
1. ✅ Parameter `it_cntrdeal_item` is NOT empty
2. ✅ Items passed from buffer:
   ```
   it_cntrdeal_item has 3 entries
   Entry 1: Quantity = 100
   Entry 2: Quantity = 200
   Entry 3: Quantity = 300
   ```
3. ✅ Loop processes all items
4. ✅ `lv_requestquantity` = 600
5. ✅ Calculation completes successfully
6. ✅ Result returned to caller

---

## Performance Test

### Objective:
Verify no significant performance degradation

### Test Steps:

1. Note system time before save
2. Create counter deal with 10 items
3. Save
4. Note system time after save
5. Calculate duration

**Expected Result:**  
✅ Save completes within acceptable time  
✅ No noticeable delay compared to original implementation  
✅ Duration: < 5 seconds for normal case  

---

## Regression Test Checklist

### Verify these existing functions still work:

- [ ] Counter deal creation (without items) - Should fail validation
- [ ] Counter deal approval workflow
- [ ] Counter deal cancellation
- [ ] Counter deal release
- [ ] Counter deal status changes
- [ ] Counter deal reporting
- [ ] Integration with exposure management
- [ ] Authorization checks
- [ ] Draft functionality
- [ ] Copy function (if exists)
- [ ] Mass processing (if exists)

---

## Test Results Template

### Test Execution Summary:

| Test Case | Status | Tester | Date | Comments |
|-----------|--------|--------|------|----------|
| TC1: Basic Creation | ⬜ Pass / ⬜ Fail | | | |
| TC2: With Overhedge | ⬜ Pass / ⬜ Fail | | | |
| TC3: No Items | ⬜ Pass / ⬜ Fail | | | |
| TC4: Draft Handling | ⬜ Pass / ⬜ Fail | | | |
| TC5: Update Existing | ⬜ Pass / ⬜ Fail | | | |
| TC6: Edge Cases | ⬜ Pass / ⬜ Fail | | | |
| Debug Test | ⬜ Pass / ⬜ Fail | | | |
| Performance Test | ⬜ Pass / ⬜ Fail | | | |
| Regression Tests | ⬜ Pass / ⬜ Fail | | | |

---

## Defect Reporting Template

**If any test fails, use this template:**

### Defect Report:

**Defect ID:** [Auto-generated or assigned]  
**Test Case:** [TC number]  
**Severity:** Critical / High / Medium / Low  
**Priority:** P1 / P2 / P3  

**Description:**  
[Describe what happened]

**Steps to Reproduce:**
1. [Step 1]
2. [Step 2]
3. [Step 3]

**Expected Result:**  
[What should happen]

**Actual Result:**  
[What actually happened]

**Screenshots/Logs:**  
[Attach if available]

**Environment:**  
- System: [DEV/QA/PRD]
- User: [User ID]
- Date: [Date/Time]

---

## Sign-Off

### Test Completion:

**Tested By:** _________________________  
**Date:** _________________________  
**Status:** ⬜ PASSED / ⬜ FAILED / ⬜ BLOCKED  

**Comments:**
```
[Add any additional comments or observations]
```

---

### Approval:

**Approved By:** _________________________  
**Role:** Technical Lead / QA Manager  
**Date:** _________________________  
**Signature:** _________________________  

---

## Quick Reference: Expected vs Actual Values

### Formula for Verification:

```
After Utilization = Before Utilization + Sum(All Item Quantities)

After Overhedge = MAX(0, After Utilization - Target Quota)

After Overhedge % = (After Overhedge / Target Quota) × 100

Criticality:
- Green (1): Overhedge = 0
- Yellow (2): 0 < Overhedge < 10%
- Red (3): Overhedge >= 10%
```

### Sample Calculation Table:

| Before Util | Target Quota | Items Sum | After Util | After Overhedge | Criticality |
|------------|--------------|-----------|------------|-----------------|-------------|
| 1,000 | 2,000 | 600 | 1,600 | 0 (0%) | 🟢 Green |
| 1,800 | 2,000 | 600 | 2,400 | 400 (20%) | 🔴 Red |
| 1,900 | 2,000 | 150 | 2,050 | 50 (2.5%) | 🟡 Yellow |

---

**Test Case Status:** ✅ Ready for Execution  
**Last Updated:** December 2024  
**Version:** 1.0
