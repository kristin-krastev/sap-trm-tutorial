# ATC Error: RAP Contract Violation - READ_IN_LATE_SAVE

**Issue Date:** December 15, 2025  
**Priority:** High (Priority 2)  
**Check:** RAP Contract Check - Provider Violation  
**Cannot be suppressed** with pragma or pseudo-comment

---

## Issue Details

### ATC Finding
```
RAP Contract Check: Provider Violation (high)
CC/P:READ_IN_LATE_SAVE:R_CMMDTYHDGRCLASSFCTNREQUESTTP:R_CMMDTYHDGRCLASSFCTNREQUESTTP

Package: FIN_CMM_CMDTY_HEDGE_REQUESTS
Class: CL_CMM_RECLASSIFICATION_HELPER
Method: CALCULATE_OVERHEDGE
Line: 21
Priority: Priority 2
```

### What This Means
The code is attempting to **READ ENTITIES during the LATE SAVE phase**, which is a **RAP contract violation**.

---

## RAP Save Sequence Phases

RAP has strict rules about what operations are allowed in each phase:

### 1. **Interaction Phase**
- ✅ Create, Update, Delete operations
- ✅ Validations
- ✅ Determinations
- ✅ READ ENTITIES allowed
- ✅ MODIFY ENTITIES allowed

### 2. **Save Sequence - EARLY SAVE**
- ✅ Additional validations
- ✅ Final data preparations
- ✅ READ ENTITIES allowed (limited)
- ⚠️ MODIFY operations trigger new save cycle

### 3. **Save Sequence - LATE SAVE**
- ✅ Final persistence operations
- ✅ Database commits
- ❌ **READ ENTITIES NOT ALLOWED** ← **This is the violation**
- ❌ **MODIFY ENTITIES NOT ALLOWED**
- ❌ No data retrieval allowed

### 4. **After Save**
- ✅ READ ENTITIES allowed again
- ✅ Logging, notifications
- ❌ Cannot modify data anymore

---

## Why is READ_IN_LATE_SAVE a Violation?

### Technical Reasons
1. **Data Consistency:** During late save, data is in transit to database
2. **Transaction State:** Transaction is partially committed
3. **Locking Issues:** Can cause deadlocks
4. **Performance:** Can block other operations
5. **Predictability:** Violates RAP framework guarantees

### Business Impact
- **Data Integrity Risk:** Reading partially committed data
- **System Stability:** Potential deadlocks
- **Unpredictable Behavior:** May work sometimes, fail others
- **Upgrade Risk:** SAP may enforce stricter checks in future

---

## The Specific Violation

### Class/Method Details
```abap
Class: CL_CMM_RECLASSIFICATION_HELPER
Method: CALCULATE_OVERHEDGE
Line: 21

" The code is likely doing something like:
READ ENTITIES OF R_CmmdtyHdgRclassfctnRequestTP
  ENTITY ReclassificationRequest
  FIELDS ( ... )
  WITH VALUE #( ... )
  RESULT DATA(lt_requests).
```

### Entity Involved
- **Entity:** `R_CMMDTYHDGRCLASSFCTNREQUESTTP`
- **Full Name:** Commodity Hedge Reclassification Request
- **Type:** RAP Business Object (Root Entity)
- **Package:** FIN_CMM_CMDTY_HEDGE_REQUESTS

---

## Root Cause Analysis

### Why is This Happening?

The `CALCULATE_OVERHEDGE` method is likely:
1. Called during save sequence (possibly from a determination)
2. Needs to read reclassification request data
3. Trying to read that data during late save phase
4. This violates RAP contract

### Typical Scenario
```abap
" Method is probably being called from:
" - A determination that runs in late save
" - A save_modified method implementation
" - A finalize method in behavior implementation

METHOD calculate_overhedge.
  " This is the violation - reading during late save:
  READ ENTITIES OF R_CmmdtyHdgRclassfctnRequestTP
    ENTITY ReclassificationRequest
    ALL FIELDS WITH VALUE #( ( %key = ... ) )
    RESULT DATA(lt_requests).
    
  " ... calculation logic ...
ENDMETHOD.
```

---

## Solution Strategies

### Strategy 1: Move READ to Earlier Phase ✅ RECOMMENDED

**Principle:** Read data during interaction phase or early save, not late save.

**Implementation:**
```abap
" BAD: Reading in late save
METHOD save_modified.
  " This runs in late save - TOO LATE!
  READ ENTITIES OF R_CmmdtyHdgRclassfctnRequestTP ...
ENDMETHOD.

" GOOD: Read earlier, pass data forward
METHOD determine_overhedge.  " Runs in interaction phase
  " Read here - SAFE!
  READ ENTITIES OF R_CmmdtyHdgRclassfctnRequestTP
    ENTITY ReclassificationRequest
    ALL FIELDS WITH VALUE #( ( %key = ... ) )
    RESULT DATA(lt_requests).
    
  " Calculate and store result
  MODIFY ENTITIES OF R_CmmdtyHdgRclassfctnRequestTP
    ENTITY ReclassificationRequest
    UPDATE FIELDS ( overhedge_amount )
    WITH VALUE #( FOR req IN lt_requests
                  ( %key = req-%key
                    overhedge_amount = calculate_overhedge( req ) ) ).
ENDMETHOD.
```

### Strategy 2: Use Buffer/Cache Pattern ⚠️ WORKAROUND

**Principle:** Read data once in early phase, cache it, use cache in late save.

**Implementation:**
```abap
CLASS lhc_reclassification DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.
    " Cache at class level
    CLASS-DATA: gt_cached_requests TYPE TABLE OF ...
    
    METHODS determine_overhedge FOR DETERMINE ON SAVE ...
    METHODS save_modified FOR SAVE-MODIFIED ...
    
ENDCLASS.

CLASS lhc_reclassification IMPLEMENTATION.

  METHOD determine_overhedge.
    " Read and cache in early phase
    READ ENTITIES OF R_CmmdtyHdgRclassfctnRequestTP
      ENTITY ReclassificationRequest
      ALL FIELDS WITH keys
      RESULT DATA(lt_requests).
      
    " Cache for later use
    gt_cached_requests = lt_requests.
    
    " Do calculations
    " ...
  ENDMETHOD.
  
  METHOD save_modified.
    " Use cached data instead of reading
    LOOP AT gt_cached_requests INTO DATA(ls_request).
      " Use cached data - no READ ENTITIES needed
    ENDLOOP.
    
    " Clear cache
    CLEAR gt_cached_requests.
  ENDMETHOD.
  
ENDCLASS.
```

### Strategy 3: Refactor to Remove READ Dependency ✅ BEST PRACTICE

**Principle:** Redesign so late save doesn't need to read data.

**Options:**
1. **Calculate earlier:** Move calculation to determination (before save)
2. **Pass data via parameters:** Use ABAP buffer/memory
3. **Use BOPF events:** Trigger calculation in appropriate phase
4. **Restructure flow:** Change when/how overhedge is calculated

---

## Who Can Fix This?

### Case 1: SAP Standard Code ⚠️
**If class is in SAP namespace (CL_...):**
- **You cannot modify** SAP standard code directly
- **Action:** Report to SAP via SAP Support ticket
- **Include:** ATC details, system info, business impact
- **Workaround:** May need custom implementation to avoid this code path

### Case 2: Custom Code in Your Namespace (Z/Y) ✅
**If class is custom (ZCL_/YCL_...):**
- **You can modify** the code
- **Action:** Apply one of the solution strategies above
- **Test thoroughly** after changes

### Case 3: Partner/Vendor Code 🤝
**If class is from partner (starts with CL_ but not SAP):**
- **Contact vendor** for fix
- **May need update/patch**
- **Coordinate with vendor support**

### Case 4: Code in FIN_CMM Package (This Case) ⚠️
**Package FIN_CMM_CMDTY_HEDGE_REQUESTS suggests:**
- **Likely SAP standard** (FIN = Financial, CMM = Commodity Management)
- **But could be custom** in customer namespace
- **Check:** Look at class components (VOEROES suggests possible username?)

---

## Immediate Actions

### Step 1: Determine Code Ownership
```
Check in SE24 or SE80:
1. Open class: CL_CMM_RECLASSIFICATION_HELPER
2. Check package: FIN_CMM_CMDTY_HEDGE_REQUESTS
3. Check if class starts with:
   - CL_* = Probably SAP standard
   - ZCL_* or YCL_* = Custom code
4. Check original system/original language
```

### Step 2: Analyze the Code
```abap
" Open method CALCULATE_OVERHEDGE
" Look at line 21
" Identify:
" 1. Is there a READ ENTITIES statement?
" 2. Where is this method called from?
" 3. Is it called during save sequence?
" 4. Can we move the READ earlier?
```

### Step 3: Choose Resolution Path

#### If SAP Standard:
```
1. Create SAP OSS ticket
2. Component: BC-ESI-RAP-SRV (RAP Services)
3. Title: "RAP Contract Violation in CL_CMM_RECLASSIFICATION_HELPER"
4. Include:
   - Full ATC message
   - System details (QM7)
   - Business impact
   - Request for fix in next support package
```

#### If Custom Code:
```
1. Assign to original developer (VOEROES?)
2. Apply solution strategy (preferably #1 or #3)
3. Test thoroughly
4. Document the fix
5. Run ATC again to verify
```

---

## Example Fix (If Custom Code)

### Before (Violates RAP Contract)
```abap
CLASS zcl_cmm_reclassification_helper IMPLEMENTATION.

  METHOD calculate_overhedge.
    " VIOLATION: Reading in late save
    READ ENTITIES OF r_cmmdtyhdgrclassfctnrequesttp
      ENTITY reclassificationrequest
      ALL FIELDS WITH VALUE #( ( requestid = iv_request_id ) )
      RESULT DATA(lt_requests).
      
    " Calculate overhedge
    DATA(lv_overhedge) = lt_requests[ 1 ]-hedged_amount - 
                         lt_requests[ 1 ]-exposure_amount.
    
    rv_overhedge = lv_overhedge.
  ENDMETHOD.

ENDCLASS.
```

### After (Fixed - Read Earlier)
```abap
CLASS zcl_cmm_reclassification_helper IMPLEMENTATION.

  METHOD calculate_overhedge.
    " FIXED: Assume data is passed as parameter instead of reading
    " The calling method should read in early phase and pass data
    
    " Calculate overhedge from passed parameters
    rv_overhedge = is_request-hedged_amount - 
                   is_request-exposure_amount.
  ENDMETHOD.

ENDCLASS.

" Calling method (in behavior implementation):
CLASS lhc_reclassification IMPLEMENTATION.

  METHOD determine_overhedge_amount.
    " Read here (early phase) - SAFE
    READ ENTITIES OF r_cmmdtyhdgrclassfctnrequesttp
      ENTITY reclassificationrequest
      ALL FIELDS WITH keys
      RESULT DATA(lt_requests).
    
    " Calculate for each request
    LOOP AT lt_requests INTO DATA(ls_request).
      DATA(lv_overhedge) = zcl_cmm_reclassification_helper=>calculate_overhedge(
        is_request = ls_request ).
      
      " Update the entity with calculated value
      MODIFY ENTITIES OF r_cmmdtyhdgrclassfctnrequesttp
        ENTITY reclassificationrequest
        UPDATE FIELDS ( overhedge_amount )
        WITH VALUE #( ( %key = ls_request-%key
                       overhedge_amount = lv_overhedge ) ).
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
```

---

## Testing After Fix

### Test Checklist
- [ ] ATC check passes (violation resolved)
- [ ] Unit tests pass
- [ ] Functional testing: Create reclassification request
- [ ] Verify overhedge calculation still works
- [ ] Performance testing (no degradation)
- [ ] End-to-end scenario testing
- [ ] Regression testing of related functionality

### Test Scenarios
1. **Normal flow:** Create request with proper hedging
2. **Overhedge scenario:** Create request exceeding exposure
3. **Underhedge scenario:** Create request below exposure
4. **Boundary conditions:** Zero amounts, negative amounts
5. **Concurrent access:** Multiple users accessing same data

---

## Prevention Guidelines

### For Future Development

#### ✅ DO:
1. **Read early:** Always read in interaction or early determination phase
2. **Cache data:** If needed later, cache during early phase
3. **Pass data:** Use parameters instead of repeated reads
4. **Use determinations:** For calculations before save
5. **Follow RAP contract:** Understand save sequence phases

#### ❌ DON'T:
1. **Read in late save:** Never use READ ENTITIES in late save
2. **Read in save_modified:** Avoid reads in finalize methods
3. **Complex logic in late save:** Keep late save simple
4. **Assume data availability:** Plan data access for each phase
5. **Ignore ATC warnings:** Fix contract violations immediately

---

## Related Documentation

### SAP Resources
- SAP Help: RAP Save Sequence
- RAP Contract Checks Documentation
- Best Practices for RAP Development

### Internal Documentation
- `/workspace/docs/rap-rules.md` - General RAP guidelines
- `/workspace/docs/atc_use_side_effects_guide.md` - Related ATC fix
- `/workspace/docs/sprint_plan_cluster_c_ux30.md` - Sprint context

---

## Decision Matrix

| Situation | Action | Timeline | Priority |
|-----------|--------|----------|----------|
| SAP Standard Code | Report to SAP | Long term | Medium |
| Custom Code (Your Team) | Fix immediately | This sprint | High |
| Custom Code (Other Team) | Coordinate fix | 1-2 weeks | High |
| Partner Code | Contact vendor | Vendor timeline | Medium |
| Blocker Issue | Implement workaround | Immediate | Critical |

---

## Impact Assessment

### If Not Fixed

#### Short Term
- System continues to work (mostly)
- Occasional unexplained errors
- ATC finding remains

#### Medium Term
- Potential data inconsistencies
- Performance degradation
- Deadlock situations

#### Long Term
- May fail on SAP upgrade
- Technical debt accumulation
- Support difficulty

### If Fixed

#### Benefits
- ✅ RAP contract compliant
- ✅ ATC clean
- ✅ Stable, predictable behavior
- ✅ Upgrade-safe
- ✅ Better performance
- ✅ Easier maintenance

---

## Summary & Recommendations

### The Problem
- Code is reading entities during late save phase
- This violates RAP contract rules
- High-priority ATC finding
- Cannot be suppressed

### The Solution
1. **Determine ownership:** Is this SAP or custom code?
2. **If SAP:** Report via OSS ticket, request fix
3. **If Custom:** Refactor to read in earlier phase
4. **Test thoroughly:** Ensure fix doesn't break functionality

### Recommended Approach
```
Priority: HIGH
Effort: Medium (if custom) / Long (if SAP)
Risk: Medium (but higher if not fixed)

Immediate: Determine code ownership
This Week: Report to SAP or assign to developer
This Sprint: Fix if custom code
```

### Key Takeaway
**RAP contract violations are serious.** They indicate architectural issues that can cause stability problems. Always fix these, don't suppress or ignore them.

---

**Created:** December 15, 2025  
**Class:** CL_CMM_RECLASSIFICATION_HELPER  
**Method:** CALCULATE_OVERHEDGE  
**Violation:** READ_IN_LATE_SAVE  
**Status:** Requires investigation of code ownership  
**Priority:** High (Priority 2)  
**Cannot be suppressed:** True
