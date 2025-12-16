# ATC Check: CDS Performance Test Parameters Not Maintained

**Check Type:** PFCDS_QLTY (CDS Views: Check of Data Quality and Usage)  
**Issue:** Parameters not maintained in performance test system  
**Priority:** 2  
**Date:** December 15, 2025

---

## Issue Details

### Example Finding
```
Package: FIN_CMM_CMDTY_HEDGE_CONST
DDLS: I_CMMDTYHDGCNSTLTNEXTINTMIRROR
Title: Cmmdty Hdg Cnstltn Ext Int Mirror Rcrds
Line: 0
Check: CDS Views: Check of Data Quality and Usage (PFCDS_QLTY)
Message: CDS view I_CMMDTYHDGCNSTLTNEXTINTMIRROR: parameters not maintained 
         in performance test system CCQ/910 (200)
Priority: 2
Status: Not Passed
```

---

## What This Check Means

### Purpose of PFCDS_QLTY Check
This ATC check verifies that CDS views have **proper test data maintained** in designated performance test systems (like CCQ/910). This is important for:

1. ✅ **Performance Testing** - Ensure views can be tested with realistic data
2. ✅ **Code Coverage** - Views can be executed in test scenarios
3. ✅ **Quality Assurance** - Validate view logic before production
4. ✅ **Continuous Integration** - Automated tests can run successfully

### What "Parameters Not Maintained" Means

For **parameterized CDS views** (views with input parameters), the check verifies:
- ❌ Test parameter values are not defined in performance test system CCQ/910
- ❌ View cannot be executed/tested without these parameters
- ❌ Performance measurements cannot be taken

---

## Understanding the CDS View

### I_CMMDTYHDGCNSTLTNEXTINTMIRROR

**Type:** Interface CDS View (I_*)  
**Domain:** Commodity Hedge Constellation External/Internal Mirror Records  
**Package:** FIN_CMM_CMDTY_HEDGE_CONST

**Likely Structure:**
```sql
@AbapCatalog.sqlViewName: 'ICMHDGCNSTMIR'
define view I_CmmdtyHdgCnstltnExtIntMirror
  with parameters
    P_Parameter1 : <type>,
    P_Parameter2 : <type>
  as select from <datasource>
{
  // field definitions
}
where ...
```

---

## Why This Happens

### Common Reasons

1. **New CDS View** - Recently created, test data not yet defined
2. **New Parameters Added** - Parameters added to existing view, test values not updated
3. **Test System Not Updated** - View exists but CCQ/910 system missing test configuration
4. **Missing Test Data Setup** - Required underlying tables empty in test system

---

## Solution Options

### ✅ Option 1: Maintain Test Parameters in CCQ/910 (RECOMMENDED)

**Who Can Do This:** Test Data Administrator, Performance Test Team

**Steps:**
1. Access test system CCQ/910
2. Navigate to performance test configuration
3. Define test parameter values for this view
4. Run performance test to verify
5. Re-run ATC check

**Tool:** Transaction `SCTS_PFCG` or similar performance test configuration

**Example Configuration:**
```
CDS View: I_CMMDTYHDGCNSTLTNEXTINTMIRROR
Parameters:
  - P_HedgeConstellation: '00001'
  - P_ValidFrom: '20250101'
  - P_ValidTo: '20251231'
Expected Results: > 100 rows
Performance Threshold: < 500ms
```

**Effort:** ⏱️ 30 minutes - 1 hour  
**Risk:** ✅ Low (just test data configuration)

---

### ⚠️ Option 2: Request Exemption

**When to Use:** View is rarely used, performance testing not critical

**Steps:**
1. Document business justification
2. Request exemption from ATC check
3. Note: Your finding shows "Not authorized to request exemptions"
4. May need manager approval

**Justification Examples:**
- View used only for data migration (one-time)
- View for admin purposes only (low usage)
- View being deprecated
- Test system doesn't have representative data

**Effort:** ⏱️ 15-30 minutes (paperwork)  
**Risk:** ⚠️ Medium (bypasses quality check)

---

### 🔧 Option 3: Remove Parameters (If Possible)

**When to Use:** Parameters are not actually needed

**Before:**
```sql
define view I_CmmdtyHdgCnstltnExtIntMirror
  with parameters
    P_ValidFrom : abap.dats,
    P_ValidTo   : abap.dats
  as select from ztable
{
  key field1,
  field2
}
where valid_from >= $parameters.P_ValidFrom
  and valid_to   <= $parameters.P_ValidTo
```

**After:**
```sql
define view I_CmmdtyHdgCnstltnExtIntMirror
  as select from ztable
{
  key field1,
  field2,
  valid_from,  // Expose for filtering
  valid_to     // Expose for filtering
}
// Let consumer apply filters
```

**Pros:** Eliminates the ATC finding  
**Cons:** Changes API, may affect consumers  
**Risk:** ⚠️ High (breaking change)

---

### 📊 Option 4: Populate Test System with Data

**When to Use:** Test system has no data for this view

**Steps:**
1. Identify source tables used by view
2. Copy representative data from DEV/PRD to CCQ/910
3. Ensure data volume is realistic (for performance testing)
4. Define parameter values based on available data
5. Run performance test

**Tools:**
- Transaction `SE16N` or `SE14` for data copy
- Client copy utilities
- Data provisioning frameworks

**Effort:** ⏱️ 2-4 hours (depending on data complexity)  
**Risk:** ⚠️ Medium (test data maintenance)

---

## Is This Actually a Problem?

### Priority Assessment

**Priority 2** suggests this is **medium-high importance** but not critical.

### Impact Analysis

| Aspect | Impact | Severity |
|--------|--------|----------|
| **Production** | None (test check only) | ✅ Low |
| **Development** | ATC finding blocks release? | ⚠️ Medium |
| **Testing** | Cannot performance test view | ⚠️ Medium |
| **Quality** | Reduced test coverage | ⚠️ Medium |
| **CI/CD** | Automated tests may fail | ⚠️ Medium |

### When to Address

**Address Now If:**
- ✅ View is critical for performance
- ✅ View used in high-volume transactions
- ✅ Release blocked by ATC finding
- ✅ CI/CD pipeline failing

**Can Defer If:**
- ⏸️ View rarely used
- ⏸️ Not performance-critical
- ⏸️ Other priorities more urgent
- ⏸️ Exemption can be granted

---

## Multiple Similar Findings

**Your Comment:** "There are several like this one"

### Batch Resolution Strategy

If you have **many similar findings** (e.g., 10+ CDS views with same issue):

#### Strategy 1: Coordinate with Test Team
```
1. List all affected CDS views
2. Group by package/domain
3. Create bulk test data request
4. Test team configures all at once
5. Batch re-run ATC checks
```

**Effort:** ⏱️ Half day (coordination)  
**Best For:** Large number of findings (10+)

#### Strategy 2: Template Approach
```
1. Define standard test parameters for domain
2. Create template configuration
3. Apply template to all similar views
4. Adjust specific values as needed
```

**Example Template for Hedge Constellation Views:**
```
Domain: Commodity Hedge Constellation
Standard Parameters:
  - HedgeConstellationID: '00001', '00002', '00003'
  - ValidFrom: First day of last year
  - ValidTo: Last day of current year
  - CompanyCode: '1000', '2000'
Expected Data Volume: 100-1000 rows per parameter set
Performance Threshold: < 1 second
```

#### Strategy 3: Prioritize by Usage
```
1. Identify high-usage views (check usage logs)
2. Configure those first
3. Request exemptions for low-usage views
4. Defer rarely-used views
```

**Prioritization Criteria:**
- 🔥 **High:** Used in critical transactions, high volume
- ⚠️ **Medium:** Used regularly, moderate volume
- ⏸️ **Low:** Rarely used, low volume
- 🚫 **Defer:** Being deprecated, admin only

---

## How to Find Affected Views

### Query to List All Findings

In ATC results, filter by:
```
Check ID: PFCDS_QLTY
Message Pattern: "parameters not maintained in performance test system"
System: CCQ/910
Priority: 2
```

### Typical Patterns

Views affected are usually:
- ✅ **Interface views** (I_*)  with parameters
- ✅ **Consumption views** (C_*) with parameters
- ✅ **Views with date range parameters** (ValidFrom, ValidTo)
- ✅ **Views with selection parameters** (CompanyCode, etc.)

---

## Recommended Action Plan

### For Your Situation

Since you mentioned "several like this one", here's what I recommend:

#### Step 1: Assess Scope (30 min)
```
1. Export all PFCDS_QLTY findings from ATC
2. Count affected views
3. Group by package
4. Identify common patterns
```

#### Step 2: Prioritize (30 min)
```
1. Mark critical views (P1)
2. Mark important views (P2)
3. Mark nice-to-have (P3)
4. Mark for exemption (P4)
```

#### Step 3: Coordinate with Test Team (1 day)
```
1. Create list of P1 + P2 views
2. Define test parameters for each
3. Request test data configuration
4. Provide business context
```

#### Step 4: Implement (Variable)
```
- Test team configures: 1-2 days
- Your verification: 2-4 hours
- ATC re-check: 30 min
```

#### Step 5: Handle Remaining (Variable)
```
- P3 views: Configure when time permits
- P4 views: Request exemptions
```

---

## Example: Resolving One View

### For I_CMMDTYHDGCNSTLTNEXTINTMIRROR

**Step 1: Understand the View**
```sql
-- Check view definition in Eclipse/ADT
-- Note all parameters and their types
-- Example:
--   P_HedgeConstellationID : type
--   P_ValidFromDate : abap.dats
```

**Step 2: Define Test Values**
```
Test Case 1:
  P_HedgeConstellationID = '00000001'
  P_ValidFromDate = '20240101'
  Expected Result: Mirror records for constellation 00000001

Test Case 2:
  P_HedgeConstellationID = '00000002'
  P_ValidFromDate = '20240101'
  Expected Result: Mirror records for constellation 00000002

Test Case 3 (Performance):
  P_HedgeConstellationID = '*' (if supported)
  P_ValidFromDate = '20240101'
  Expected Result: All mirror records (performance test)
  Threshold: < 2 seconds for 1000 records
```

**Step 3: Provide to Test Team**
```
To: CCQ/910 Test Data Team
Subject: Test Parameters for I_CMMDTYHDGCNSTLTNEXTINTMIRROR

Please configure the following test parameters:

CDS View: I_CMMDTYHDGCNSTLTNEXTINTMIRROR
Package: FIN_CMM_CMDTY_HEDGE_CONST

Test Parameters:
  - HedgeConstellationID: ['00000001', '00000002']
  - ValidFromDate: '20240101'

Expected Results: > 50 records per parameter set
Performance Target: < 2 seconds

Business Justification: Required for commodity hedge constellation 
monitoring and performance testing of mirror record synchronization.

Contact: [Your Name]
```

**Step 4: Verify**
```
1. Wait for test team confirmation
2. Access CCQ/910 system
3. Execute view with test parameters
4. Verify results
5. Re-run ATC check
6. Confirm finding resolved ✅
```

---

## FAQ

### Q: Can I fix this myself?
**A:** Depends on authorization. You need access to:
- CCQ/910 test system
- Performance test configuration transaction
- Test data maintenance authorization

### Q: Is this blocking production?
**A:** No, this is a test quality check. Production is not affected.

### Q: Can I ignore this?
**A:** Depends on your organization's ATC policy. Priority 2 suggests it should be addressed, but may not block release.

### Q: How long to fix all findings?
**A:** 
- Single view: 30 min - 1 hour
- 10 views (batch): Half day to 1 day
- 50+ views: 2-3 days (with test team)

### Q: Who typically handles this?
**A:** Usually:
- **Test Data Team** - Configures parameters
- **Performance Test Team** - Validates performance
- **Developers** - Provides parameter definitions
- **QA Team** - Verifies resolution

### Q: What if test system has no data?
**A:** Options:
1. Copy data from DEV/PRD
2. Generate synthetic test data
3. Request exemption (with justification)

---

## Decision Matrix

| Situation | Recommended Action | Effort | Priority |
|-----------|-------------------|--------|----------|
| 1-2 views, critical | Configure immediately | 1-2 hours | 🔥 High |
| 5-10 views, mixed importance | Batch configure P1/P2, defer others | Half day | ⚠️ Medium |
| 10+ views, mostly low-usage | Prioritize, configure critical only | 1-2 days | ⏸️ Low |
| Views being deprecated | Request exemption | 30 min | ⏸️ Low |
| No test data available | Coordinate data copy | 2-4 hours | ⚠️ Medium |

---

## Summary

**This ATC Check:**
- ✅ Is about test quality, not code quality
- ✅ Doesn't affect production
- ✅ Requires test data configuration
- ✅ Can often be batch-resolved
- ⚠️ May need test team coordination

**Typical Resolution:**
- ⏱️ 30 min - 1 hour per view (if test data exists)
- 🤝 Coordinate with test team for batch resolution
- 📋 Request exemption for low-priority views

**Your Next Steps:**
1. List all affected views
2. Prioritize by importance
3. Coordinate with test team (CCQ/910)
4. Provide test parameter definitions
5. Verify after configuration
6. Re-run ATC check

---

**Document Created:** December 15, 2025  
**Check Type:** PFCGS_QLTY  
**System:** CCQ/910  
**Status:** Analysis complete, awaiting resolution strategy decision
