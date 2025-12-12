# Issue 10: Extensibility - Quick Reference Card

---

## 🎯 **What to Add**

### **F5659, F5666, F5665** (3 apps):
Add to end of `sap.ui5` section:
```json
"flexEnabled": true
```

### **F5658** (1 app):
**Change 1** - Add to end of `sap.ui5` section:
```json
"flexEnabled": true
```

**Change 2** - Add to `sap.fiori` section:
```json
"cloudDevAdaptationStatus": "released"
```

---

## 📝 **Exact Location**

**Find this at end of sap.ui5:**
```json
"contentDensities": {
    "compact": true,
    "cozy": true
}
```

**Change to:**
```json
"contentDensities": {
    "compact": true,
    "cozy": true
},
"flexEnabled": true
```

**Don't forget the comma!** ⚠️

---

## ⚡ **Quick Steps**

1. Open BAS → Open each app project
2. Open `webapp/manifest.json`
3. Find `sap.ui5` → Scroll to end
4. Add comma after `contentDensities`
5. Add `"flexEnabled": true`
6. (F5658 only: Also add to sap.fiori)
7. Save (Ctrl+S)
8. Repeat for all 4 apps

**Time:** 5 min per app = 20 minutes total

---

## ✅ **Checklist**

- [ ] F5659: flexEnabled added
- [ ] F5666: flexEnabled added
- [ ] F5665: flexEnabled added
- [ ] F5658: flexEnabled added
- [ ] F5658: cloudDevAdaptationStatus added
- [ ] All files saved
- [ ] No JSON syntax errors
- [ ] Ready to commit!

---

## 🚀 **Commit & Push**

```bash
git checkout -b feature/enable-extensibility-all-apps
git add */webapp/manifest.json
git commit -m "Enable extensibility for all apps (Issue 10)"
git push origin feature/enable-extensibility-all-apps
# Create PR
```

---

**Full Guide:** `/workspace/docs/ISSUE_10_IMPLEMENTATION_GUIDE.md`
