# Jira Ticket Summary: DCS and Hedge Book Sort Investigation

## Issue Summary
DCS and Hedge Book columns in Fiori apps only show "Sort by ID" option. "Sort by Name" option missing in deployed apps.

## Root Cause
Custom UI fragment `CustomColumns.fragment.xml` hardcodes `sortProperty` to ID field only, overriding standard Fiori Elements multi-sort behavior.

## Technical Details
- **Location:** `CustomColumns.fragment.xml` (referenced in `manifest.json`)
- **Problem:** `<table:Column sortProperty="ID_FIELD_ONLY">` limits sorting to single property
- **Impact:** Affects Trade Order Cockpit, Counter Deal Request, and similar apps
- **Backend:** Correctly configured (CDS annotations, OData metadata verified)
- **Frontend:** Custom fragment overrides standard behavior

## Investigation Results
✅ Root cause identified  
✅ Backend configuration verified as correct  
✅ Service Binding Preview works (uses standard rendering)  
✅ Deployed app limitation explained (uses custom fragment)  
✅ Solution options documented  

## Solution Options
1. **Remove Custom Columns** - Easiest, restores both sort options, loses custom navigation
2. **Accept Limitation** - Zero risk, preserves current functionality
3. **Custom JavaScript** - Complex, achieves all features, high effort/risk
4. **Switch to Name Sort** - Quick, swaps which field is sortable

## Status
Awaiting team decision on solution approach.

## Documentation
- `/workspace/docs/ROOT_CAUSE_FOUND.md` - Technical details
- `/workspace/docs/SESSION_SUMMARY_DEC_8_2025.md` - Full investigation summary
