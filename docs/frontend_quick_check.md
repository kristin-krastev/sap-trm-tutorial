# Frontend Quick Check - Text Field Sorting

## Where to Look

### 1️⃣ **manifest.json** (Main Configuration)

**Location:** 
- Trade Order Cockpit app folder: `webapp/manifest.json`
- Counter Deal Request app folder: `webapp/manifest.json`

**What to check:**

```json
{
  "sap.ui5": {
    // Check SAPUI5 version
    "dependencies": {
      "minUI5Version": "1.XX.X",  // ← Is this old? (< 1.84 might have issues)
      "libs": {
        "sap.fe.templates": {}
      }
    },
    
    "routing": {
      "targets": {
        "YourListReport": {
          "options": {
            "settings": {
              // Check table settings
              "controlConfiguration": {
                "@com.sap.vocabularies.UI.v1.LineItem": {
                  "tableSettings": {
                    "type": "ResponsiveTable",  // ← Check this!
                    // OR "GridTable"
                    // OR "AnalyticalTable"
                    
                    "enableExport": true,
                    "personalization": true,
                    "quickVariantSelection": {
                      "paths": [/* ... */]
                    }
                  }
                }
              },
              
              // Check for column overrides
              "views": {
                "paths": [/* ... */]
              }
            }
          }
        }
      }
    }
  }
}
```

---

## 2️⃣ **Key Questions**

### Table Type:
```json
"tableSettings": {
  "type": "???"  // What value is here?
}
```

**Different table types have different sorting capabilities:**
- `ResponsiveTable` - Limited sorting (mobile-first)
- `GridTable` - Full sorting support
- `AnalyticalTable` - Full sorting support
- If **missing** - uses default (might not support text sorting)

### SAPUI5 Version:
```json
"minUI5Version": "1.XX.X"
```

**Text field sorting support:**
- SAPUI5 < **1.71** - Very limited text field sorting
- SAPUI5 < **1.84** - Partial text field sorting
- SAPUI5 >= **1.84** - Full text field sorting support
- SAPUI5 >= **1.90** - Improved text field sorting

### Column Personalization:
```json
"tableSettings": {
  "personalization": true,  // ← Should be true
  "selectionMode": "Multi"
}
```

---

## 3️⃣ **Annotation Overrides in Manifest**

Check if manifest is **overriding** backend annotations:

```json
{
  "sap.ui5": {
    "routing": {
      "targets": {
        "YourListReport": {
          "options": {
            "settings": {
              "controlConfiguration": {
                "@com.sap.vocabularies.UI.v1.LineItem": {
                  "columns": {
                    // Check if DCS/Hedge Book columns have restrictions
                    "DataField::CmmdtyHedgePlanExposureDCSID": {
                      "availability": "Default",
                      "properties": {
                        "sortable": false  // ← Would disable sorting!
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}
```

---

## 4️⃣ **Custom Extensions/Controllers**

Check for custom code that might interfere:

**Files to check:**
- `webapp/ext/controller/*.controller.js`
- `webapp/ext/fragment/*.fragment.xml`
- `webapp/changes/*.change` (UI Adaptation changes)

**Look for:**
```javascript
// Custom sorting logic that might interfere
onBeforeRebindTable: function(oEvent) {
  var mBindingParams = oEvent.getParameter("bindingParams");
  // Check if custom code removes text field sorting
}

// Custom column definitions
onInit: function() {
  // Check if columns are being customized
}
```

---

## 5️⃣ **UI Adaptation/Flexbox Changes**

**Location:** 
- Check in Fiori Launchpad: App Settings → Adapt UI
- Or: `/sap/bc/lrep/changes/` (LREP repository)

**What to check:**
- Are there saved "Views" that hide columns?
- Are there personalization settings that disable sorting?
- Check if someone created a "Standard" variant that limits sorting

---

## 6️⃣ **Compare Company Code vs DCS in Manifest**

If Company Code sorting works but DCS doesn't, check for differences:

```json
{
  "controlConfiguration": {
    "@com.sap.vocabularies.UI.v1.LineItem": {
      "columns": {
        // Company Code column
        "DataField::CmmdtyHdgPlnExpsrCompanyCode": {
          // What properties are here?
        },
        
        // DCS column  
        "DataField::CmmdtyHedgePlanExposureDCSID": {
          // What properties are here?
          // Are they different?
        }
      }
    }
  }
}
```

---

## 🔍 **Quick Discovery Commands**

### Find the app folder:
```bash
# In Eclipse/SAP WebIDE or via file system
# Look for structure:
# - webapp/
#   - manifest.json
#   - Component.js
#   - ext/ (custom code)
```

### Search manifest for relevant settings:
```bash
# Search for:
- "tableSettings"
- "type"
- "minUI5Version"
- "controlConfiguration"
- "sortable"
- "personalization"
```

---

## 📋 **Frontend Checklist**

```plaintext
☐ 1. Find manifest.json for both apps
☐ 2. Check SAPUI5 version (minUI5Version)
☐ 3. Check table type (ResponsiveTable vs GridTable)
☐ 4. Check for column overrides in controlConfiguration
☐ 5. Check personalization settings
☐ 6. Look for custom controllers/extensions
☐ 7. Check for UI Adaptation changes
☐ 8. Compare working column (Company Code) vs non-working (DCS)
```

---

## 🎯 **Most Likely Frontend Causes**

### #1: Wrong Table Type
```json
"tableSettings": {
  "type": "ResponsiveTable"  // ← ResponsiveTable has limited sorting!
}
```

**Fix:** Change to `"GridTable"` or `"AnalyticalTable"`

### #2: Old SAPUI5 Version
```json
"minUI5Version": "1.65.0"  // ← Too old!
```

**Fix:** Upgrade to at least 1.84+ (if possible)

### #3: Column Sorting Disabled
```json
"columns": {
  "DataField::CmmdtyHedgePlanExposureDCSID": {
    "properties": {
      "sortable": false  // ← Explicitly disabled!
    }
  }
}
```

**Fix:** Remove or set to `true`

### #4: Missing Personalization
```json
"tableSettings": {
  "personalization": false  // ← Disables sorting menu!
}
```

**Fix:** Set to `true`

---

## 🚀 **How to Check Right Now**

### Option 1: Browser DevTools
1. Open Counter Deal Request app
2. Press **F12** (DevTools)
3. Console tab, run:
```javascript
// Check SAPUI5 version
sap.ui.version

// Check table type
sap.ui.getCore().byId("listReport")
  .getContent()[0]
  .getAggregation("content")
  .getTableType()

// Check if column is sortable
var oTable = sap.ui.getCore().byId("fe::table::CounterdealRequest::LineItem");
oTable.getColumns().forEach(col => {
  console.log(col.getHeader().getText() + " - Sortable: " + col.getSortIndicator());
});
```

### Option 2: Network Tab
1. Open app
2. F12 → Network tab
3. Look for `manifest.json` request
4. Click it → Preview → check settings

### Option 3: Eclipse/WebIDE
1. Open project
2. Navigate to `webapp/manifest.json`
3. Search for keywords above

---

## ❓ **Questions for You**

1. **Can you access the manifest.json file?**
   - Via Eclipse project?
   - Via SAP WebIDE?
   - Via BSP application?

2. **Can you open browser DevTools and check SAPUI5 version?**
   - Open app → F12 → Console → Type: `sap.ui.version`

3. **What do you see in the table?**
   - Is it a responsive mobile-style table?
   - Or a fixed desktop grid table?

---

## 💡 **This Could Be It!**

If the issue is:
- ✅ **Old SAPUI5 version** → Document as limitation
- ✅ **Wrong table type** → **EASY FIX!** Just change manifest.json
- ✅ **Disabled in manifest** → **EASY FIX!** Just remove the restriction

**This is worth 5 minutes to check before we close it!** 🎯
