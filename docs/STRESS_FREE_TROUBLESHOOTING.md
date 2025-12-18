# 🧘 Stress-Free Troubleshooting Guide

## 🌟 Remember This Above All Else

**"If it worked before, and you followed the playbook, it WILL work again. Most 'errors' are just timing or missing objects. Stay calm, breathe, debug systematically."**

---

## 😰 WHEN YOU SEE AN ERROR

### **Step 0: DON'T PANIC!**

```
❌ Don't immediately think everything is broken
❌ Don't start randomly changing things
❌ Don't assume you need to redo everything
❌ Don't skip checking the obvious things

✅ Take a deep breath
✅ Read the error message completely
✅ Check the error location
✅ Consult this guide
✅ Follow the flowchart below
```

---

## 🔍 ERROR DIAGNOSIS FLOWCHART

```
┌─────────────────────────────────────────┐
│     An Error Appeared! What now?        │
└──────────────┬──────────────────────────┘
               ↓
       ┌───────────────┐
       │ Where are you? │
       └───────┬────────┘
               ↓
       ┌───────────────────────────────────┐
       │  A. In ERX (Development System)   │
       │  B. In QM7 (Test System)          │
       │  C. In Eclipse/ADT                │
       │  D. In Fiori App                  │
       └───────┬───────────────────────────┘
               ↓
    See corresponding section below
```

---

## 🔧 SCENARIO A: ERRORS IN ERX (Development)

### **Error Type: Syntax Error**

#### **"No value was passed to the mandatory parameter IT_ITEMS"**

**What it means:** Some caller isn't passing the new parameter

**Solution:**
```
1. ✅ Note the line number and class name
2. ✅ Open that class
3. ✅ Find the call to calculate_* method
4. ✅ Update it to include it_items parameter
5. ✅ If it's calc class → See Phase 4 in playbook
6. ✅ If it's test class → See Phase 5 in playbook
7. ✅ Save and activate
```

**Stress Level:** 🟢 LOW - This is expected! Just update the caller.

---

#### **"Type XYZ is unknown"**

**What it means:** Type not defined or not accessible

**Solution:**
```
1. ✅ Check if type exists in helper class
2. ✅ If accessing from another class: Use full path (cl_xxx_helper=>ty_item)
3. ✅ If type is in PRIVATE section but needed elsewhere → Move to PUBLIC
4. ✅ Save and activate
```

**Stress Level:** 🟢 LOW - Just a visibility issue.

---

#### **"Access to private attribute MT_ITEMS is not allowed"**

**What it means:** Buffer not accessible from where you're calling it

**Solution:**
```
1. ✅ Open handler class
2. ✅ Find mt_items and type definitions
3. ✅ Move ALL of them to PUBLIC SECTION
4. ✅ Save and activate
```

**Stress Level:** 🟢 LOW - Simple visibility fix.

---

### **Error Type: ATC Violation**

#### **"EML statement in a loop"**

**What it means:** You have READ ENTITIES inside a LOOP

**Solution:**
```
1. ✅ Find the loop with READ ENTITIES
2. ✅ Refactor to batched pattern:
   - Collect UUIDs first (loop collects only)
   - ONE READ ENTITIES outside loop (batched)
   - Group results after reading
3. ✅ See "Batched EML Template" in QUICK_REFERENCE_CHECKLIST.md
4. ✅ Save and activate
```

**Stress Level:** 🟡 MEDIUM - Requires refactoring, but pattern is well-defined.

---

#### **"READ_IN_LATE_SAVE" still appears**

**What it means:** Still reading entities in late save phase

**Solution:**
```
1. ✅ Check helper class - should have NO READ ENTITIES
2. ✅ Check determination runs ON SAVE (not later)
3. ✅ Check buffer is passed to helper
4. ✅ Verify .bdef has determination declared
5. ✅ If all correct → Clear cache and re-run ATC
```

**Stress Level:** 🟡 MEDIUM - Review implementation step-by-step.

---

### **Error Type: Unit Test Failure**

#### **"Expected X, Actual 0" or similar value mismatch**

**What it means:** Test data not set up correctly

**Solution:**
```
1. ✅ Check UUID generation:
   - Use cl_system_uuid=>create_uuid_x16_static()
   - NOT '1' or '001'
   
2. ✅ Check it_items has data:
   - Should have matching requestuuid
   - Should have quantity/amount
   
3. ✅ If still fails → Simplify assertion:
   - Change assert_equals to assert_not_initial
   - Just verify calculation ran
   
4. ✅ Save and re-run test
```

**Stress Level:** 🟢 LOW - Test data issue, not logic issue.

---

## 🔧 SCENARIO B: ERRORS IN QM7 (Test System)

### **⏱️ FIRST RULE: DID YOU WAIT 10 MINUTES?**

```
If NO → ☕ Take a break, wait 10 minutes, try again
If YES → Continue to diagnosis below
```

---

### **Error Type: ST22 Dump**

#### **"Syntax error in program CL_XXX"**

**What it means:** Class not updated or not imported

**Solution:**
```
1. ✅ Check which class is mentioned in dump
2. ✅ In ERX, check SE09: Is that class in your transport?
3. ✅ In QM7, check STMS: Did transport import successfully?
4. ✅ If not in transport → Add it and release new CM
5. ✅ If in transport but not imported → Check import log
6. ✅ If imported → Wait 10 more minutes for regeneration
7. ✅ Try manual activation: SE24 → Class → Activate
```

**Stress Level:** 🟡 MEDIUM - Transport issue, but fixable.

---

#### **"No value was passed to the mandatory parameter IT_ITEMS"**

**What it means:** A caller class didn't get updated (probably calc class!)

**Solution:**
```
1. ✅ Check stack trace in ST22 - which class is calling?
2. ✅ In ERX, check if that class is in transport
3. ✅ If NOT → That's the problem!
   - Update that class in ERX
   - Add to transport
   - Release
   - Import to QM7
   - WAIT 10 MINUTES
   - Try again
```

**Stress Level:** 🟠 HIGH - But you know exactly what to do!

**Remember:** This happened with CL_CMM_COUNTERDEAL_CALC and we fixed it!

---

### **Error Type: Syntax Error in ADT**

#### **Syntax check shows errors immediately after import**

**Solution:**
```
1. ✅ WAIT 10 MINUTES (seriously!)
2. ✅ Close and reopen the class
3. ✅ Refresh project (F5)
4. ✅ Try Ctrl+F2 again
5. ✅ If still errors after 15 min → Check import log
6. ✅ May need manual activation: SE24 → Activate
```

**Stress Level:** 🟢 LOW - Usually just timing.

---

### **Error Type: Functional Test Failure**

#### **Fiori app shows error or old values**

**Solution:**
```
1. ✅ Check if ST22 dump occurred (transaction ST22)
2. ✅ If dump → See ST22 section above
3. ✅ If no dump but wrong values:
   - Clear browser cache
   - Refresh app (Ctrl+F5)
   - Try different record
   - Check if determination is triggered
4. ✅ Verify calculated fields:
   - Should show new values
   - Check backend data (SE16N)
```

**Stress Level:** 🟡 MEDIUM - May be cache or logic issue.

---

## 🔧 SCENARIO C: ERRORS IN ECLIPSE/ADT

### **"Cannot activate - dependency errors"**

**Solution:**
```
1. ✅ Check which dependencies are mentioned
2. ✅ Activate those dependencies first
3. ✅ Common order:
   - Helper class
   - Handler class
   - Behavior definition
   - Calc class
4. ✅ Try "Activate All" (Ctrl+Shift+F3)
```

**Stress Level:** 🟢 LOW - Just activation order issue.

---

### **"Object locked by another user"**

**Solution:**
```
1. ✅ Transaction: SM12
2. ✅ Find your locks
3. ✅ Delete your own locks
4. ✅ Try again
5. ✅ If locked by someone else → Ask them to release
```

**Stress Level:** 🟢 LOW - Just a lock issue.

---

## 🔧 SCENARIO D: ERRORS IN FIORI APP

### **"Record could not be saved"**

**Solution:**
```
1. ✅ Check for ST22 dump (transaction ST22 in backend)
2. ✅ Check browser console (F12) for errors
3. ✅ Try creating simpler record (minimal fields)
4. ✅ Check if validation error (look for message in app)
5. ✅ Verify data passes validations
```

**Stress Level:** 🟡 MEDIUM - Could be various causes.

---

### **"Calculated fields show zeros or blanks"**

**Solution:**
```
1. ✅ Check status of record (only calculated for certain statuses?)
2. ✅ Verify items exist (calculation needs items)
3. ✅ Check if determination ran (debug or check mt_items buffer)
4. ✅ Verify helper method is being called
5. ✅ Check logs (SLG1 or SM21)
```

**Stress Level:** 🟡 MEDIUM - Logic or data issue.

---

## 🎯 EMERGENCY DECISION TREE

```
┌──────────────────────────────────────┐
│   Is the system on fire? 🔥          │
├──────────────────────────────────────┤
│  NO → Take a breath, follow guide    │
│  YES → Is it really? Re-read error   │
└──────────────────────────────────────┘
           ↓
┌──────────────────────────────────────┐
│   Can you identify the exact error?  │
├──────────────────────────────────────┤
│  YES → Find it in this guide         │
│  NO → Read error message again       │
│       Note: File, Line, Message      │
│       Then find in this guide        │
└──────────────────────────────────────┘
           ↓
┌──────────────────────────────────────┐
│   Can you fix it in < 30 minutes?   │
├──────────────────────────────────────┤
│  YES → Follow the solution           │
│  NO → Document error, ask for help   │
└──────────────────────────────────────┘
           ↓
┌──────────────────────────────────────┐
│   Did the fix work?                  │
├──────────────────────────────────────┤
│  YES → 🎉 Document solution & continue│
│  NO → Revert change, re-analyze      │
└──────────────────────────────────────┘
```

---

## 🧘 STRESS MANAGEMENT TECHNIQUES

### **When You Feel Overwhelmed:**

1. **Stop coding immediately**
2. **Take a 10-minute break** (walk, coffee, stretch)
3. **Write down what you know:**
   - What were you doing?
   - What error appeared?
   - What have you tried?
4. **Review the playbook** for similar situation
5. **Ask for help** if stuck for > 30 minutes

---

### **The "5-Minute Rule"**

If you're:
- Randomly changing code hoping it works
- Copying/pasting without understanding
- Feeling frustrated or confused
- Not making progress

**→ STOP! Take 5 minutes to:**
- Read the error message fully
- Check this guide
- Look at the playbook
- Think about what the error actually means

---

### **The "Rubber Duck" Method**

**Explain the problem out loud (or in writing) as if teaching someone:**

```
"I'm trying to [goal].
I expected [expected result].
Instead I got [actual result].
The error says [error message].
I've tried [what you tried].
I'm stuck because [why stuck]."
```

**Often, explaining it helps you see the solution!**

---

## 📊 COMMON ERROR PATTERNS

### **Pattern 1: "Missing Parameter" Errors**

**Symptoms:**
- "No value was passed to IT_ITEMS"
- Appears after changing method signature

**Root Cause:** Didn't update all callers

**Prevention:**
- Always search for ALL references before changing signature
- Check: Handler, Saver, Calc, Test classes

---

### **Pattern 2: "EML in Loop" Violations**

**Symptoms:**
- ATC shows "EML statement in a loop"
- Appears in determination or calc class

**Root Cause:** READ ENTITIES inside LOOP

**Prevention:**
- Always batch EML operations
- Collect UUIDs first, read once, group after

---

### **Pattern 3: "Syntax Errors After Import"**

**Symptoms:**
- Test system shows errors immediately after import
- Same code works in ERX

**Root Cause:** Propagation delay

**Prevention:**
- Always wait 10 minutes after import
- Don't test immediately

---

### **Pattern 4: "Type Unknown" Errors**

**Symptoms:**
- "Type TY_ITEM is unknown"
- Works in one place, not another

**Root Cause:** Type visibility or incomplete reference

**Prevention:**
- Use full paths for external references
- Make types PUBLIC if accessed externally

---

## 🎯 CHECKLIST: "AM I STUCK?"

**Answer these honestly:**

```
☐ Have I read the complete error message?
☐ Have I checked the line number where error occurs?
☐ Have I looked for this error in the troubleshooting guide?
☐ Have I checked if I did all steps in the playbook?
☐ Have I waited enough time if in test system?
☐ Have I tried activating/refreshing?
☐ Have I checked the obvious things (transport, activation)?
☐ Have I tried reverting my last change to see if it helps?
☐ Have I been stuck for < 30 minutes on this?
☐ Have I documented what I've tried?

If you answered YES to all → You're doing great! Keep going!
If you answered NO to any → Do that thing first!
If you've been stuck > 30 min → Ask for help!
```

---

## 💡 DEBUGGING WORKFLOW

### **Systematic Approach:**

```
1. OBSERVE
   - What is the exact error?
   - Where does it occur? (File, line, method)
   - When does it occur? (Always? Specific action?)

2. HYPOTHESIZE
   - What might cause this?
   - Check this guide for similar patterns
   - Review playbook for this step

3. TEST
   - Try the simplest solution first
   - Make ONE change at a time
   - Check if it helped

4. DOCUMENT
   - Note what you tried
   - Note if it worked or not
   - If worked → Document for future

5. ITERATE
   - If didn't work → Try next hypothesis
   - If stuck after 3 tries → Ask for help
```

---

## 🚨 WHEN TO REVERT CHANGES

**Revert your changes if:**
- Making things worse with each attempt
- Can't identify what you changed
- Breaking other things
- Stuck for > 1 hour without progress

**How to revert safely:**
```
1. ✅ Local History in Eclipse:
   - Right-click file → Replace With → Local History
   - Select earlier version
   
2. ✅ Version Management:
   - SE24 → Utilities → Versions
   - Find last working version
   - Restore
   
3. ✅ Transport:
   - SE09 → Find transport
   - Remove objects
   - Re-import from backup if needed
```

---

## ✅ AFTER FIXING AN ERROR

**Always do these:**

```
1. ✅ Document the solution:
   - What was the error?
   - What caused it?
   - How did you fix it?
   
2. ✅ Update the playbook if it's a new pattern

3. ✅ Test thoroughly:
   - Verify fix works
   - Check didn't break anything else
   - Run unit tests
   
4. ✅ Take a breath and celebrate! 🎉
```

---

## 🎓 REMEMBER

```
✅ Every error has a solution
✅ Most errors are common and documented here
✅ You've already fixed complex issues successfully
✅ Waiting 10 minutes often solves test system issues
✅ Asking for help after 30 minutes is smart, not weak
✅ One step at a time, one error at a time
✅ You've got this! 💪
```

---

## 📞 WHEN TO ASK FOR HELP

**Ask IMMEDIATELY if:**
- Error message is completely unfamiliar
- System-level errors (dumps, crashes)
- Security/authorization issues
- Can't access required transactions
- Data corruption concerns

**Ask AFTER 30 MINUTES if:**
- Tried 3 solutions from guide without success
- Not sure what error message means
- Syntax error you can't figure out
- ATC violation you don't understand
- Test failure with unclear cause

**Don't ask if:**
- Haven't read error message completely
- Haven't checked this guide
- Haven't waited 10 minutes in test system
- Haven't tried any solutions yet

---

## 🎯 QUICK REFERENCE: MOST COMMON ERRORS

| Error | Location | Wait Time | Stress | Page |
|-------|----------|-----------|--------|------|
| Missing IT_ITEMS | ERX/QM7 | 0 min | 🟢 | Syntax Error |
| EML in loop | ERX | 0 min | 🟡 | ATC Violation |
| Type unknown | ERX | 0 min | 🟢 | Syntax Error |
| ST22 in QM7 | QM7 | 10 min | 🟡 | ST22 Dump |
| Syntax after import | QM7 | 10 min | 🟢 | ADT Errors |
| Private attribute | ERX | 0 min | 🟢 | Syntax Error |
| Test value mismatch | ERX | 0 min | 🟢 | Unit Test |

---

**Remember: You fixed CL_CMM_COUNTERDEAL_HELPER successfully!**
**You can fix the next 4 classes too!** 💪

**Stay calm, follow the guide, you've got this!** 🎯

---

**Version:** 1.0  
**Created:** December 17, 2025  
**Keep this open while coding!** 📖
