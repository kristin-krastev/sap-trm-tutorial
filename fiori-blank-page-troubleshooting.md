# Fix: Fiori App Shows Blank Page

## Problem
After running `npm run start`, the browser opens but shows a blank page.

## Common Causes & Solutions

### 1. Check Browser Console for Errors (MOST IMPORTANT)

Open your browser's Developer Tools (F12) and check the Console tab for errors. Common errors include:

- **404 errors**: Missing files or wrong paths
- **CORS errors**: Backend connection issues
- **JavaScript errors**: Code issues in Component.js or controllers
- **UI5 loading errors**: Wrong UI5 version or CDN issues

### 2. Verify manifest.json Configuration

Check your `manifest.json` file for:

#### a) Correct sap.app section:
```json
{
  "sap.app": {
    "id": "ibso.commodity.observer.request",
    "type": "application",
    "applicationVersion": {
      "version": "1.0.0"
    },
    "title": "{{appTitle}}",
    "description": "{{appDescription}}",
    "dataSources": {
      "mainService": {
        "uri": "/sap/opu/odata/sap/YOUR_SERVICE/",
        "type": "OData",
        "settings": {
          "odataVersion": "2.0",
          "localUri": "localService/metadata.xml"
        }
      }
    }
  }
}
```

#### b) Correct sap.ui5 section:
```json
{
  "sap.ui5": {
    "rootView": {
      "viewName": "ibso.commodity.observer.request.view.App",
      "type": "XML",
      "id": "app"
    },
    "routing": {
      "config": {
        "routerClass": "sap.m.routing.Router",
        "viewType": "XML",
        "viewPath": "ibso.commodity.observer.request.view",
        "controlId": "app",
        "controlAggregation": "pages",
        "async": true
      },
      "routes": [
        {
          "pattern": "",
          "name": "RequestsOverview",
          "target": "RequestsOverview"
        }
      ],
      "targets": {
        "RequestsOverview": {
          "viewName": "RequestsOverview",
          "viewLevel": 1
        }
      }
    }
  }
}
```

### 3. Check index.html

Your `index.html` should have the UI5 bootstrap:

```html
<!DOCTYPE html>
<html>
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Requests Overview</title>
    <script
        id="sap-ui-bootstrap"
        src="https://sapui5.hana.ondemand.com/resources/sap-ui-core.js"
        data-sap-ui-theme="sap_horizon"
        data-sap-ui-libs="sap.m,sap.ui.core,sap.f,sap.suite.ui.generic.template"
        data-sap-ui-resourceroots='{
            "ibso.commodity.observer.request": "./"
        }'
        data-sap-ui-compatVersion="edge"
        data-sap-ui-async="true"
        data-sap-ui-frameOptions="trusted"
        data-sap-ui-xx-waitForTheme="true">
    </script>
    <script id="locate-reuse-libs" src="./utils/locate-reuse-libs.js"></script>
</head>
<body class="sapUiBody" id="content">
    <div data-sap-ui-component 
         data-name="ibso.commodity.observer.request" 
         data-id="container" 
         data-settings='{"id" : "ibso.commodity.observer.request"}'>
    </div>
</body>
</html>
```

### 4. Verify Component.js

Make sure your `Component.js` has proper initialization:

```javascript
sap.ui.define([
    "sap/ui/core/UIComponent",
    "sap/ui/Device",
    "ibso/commodity/observer/request/model/models"
], function (UIComponent, Device, models) {
    "use strict";

    return UIComponent.extend("ibso.commodity.observer.request.Component", {
        metadata: {
            manifest: "json"
        },

        init: function () {
            // Call the base component's init function
            UIComponent.prototype.init.apply(this, arguments);

            // Set the device model
            this.setModel(models.createDeviceModel(), "device");

            // Create the views based on the url/hash
            this.getRouter().initialize();
        }
    });
});
```

### 5. Check if Views Exist

Ensure your view file exists at the correct path:
- Path should be: `webapp/view/RequestsOverview.view.xml`
- The view should have proper XML structure:

```xml
<mvc:View
    controllerName="ibso.commodity.observer.request.controller.RequestsOverview"
    xmlns:mvc="sap.ui.core.mvc"
    xmlns="sap.m"
    displayBlock="true">
    <Page title="Requests Overview">
        <content>
            <Text text="Hello World!" />
        </content>
    </Page>
</mvc:View>
```

### 6. Test with a Simple View First

Create a minimal test to see if anything loads:

**webapp/test.html:**
```html
<!DOCTYPE html>
<html>
<head>
    <meta charset="UTF-8">
    <title>Test Page</title>
    <script
        id="sap-ui-bootstrap"
        src="https://sapui5.hana.ondemand.com/resources/sap-ui-core.js"
        data-sap-ui-theme="sap_horizon"
        data-sap-ui-libs="sap.m"
        data-sap-ui-compatVersion="edge"
        data-sap-ui-async="true">
    </script>
    <script>
        sap.ui.getCore().attachInit(function() {
            new sap.m.Text({text: "Test - UI5 is loading!"}).placeAt("content");
        });
    </script>
</head>
<body class="sapUiBody">
    <div id="content"></div>
</body>
</html>
```

Access this at: `http://localhost:XXXX/test.html`

If this works, the issue is in your app configuration, not the UI5 setup.

### 7. Check ui5.yaml Configuration

Your `ui5.yaml` should be properly configured:

```yaml
specVersion: '2.6'
metadata:
  name: ibso.commodity.observer.request
type: application
server:
  customMiddleware:
    - name: fiori-tools-proxy
      afterMiddleware: compression
      configuration:
        backend:
          - path: /sap
            url: https://your-backend-system.com
            client: '900'
        ui5:
          path:
            - /resources
            - /test-resources
          url: https://sapui5.hana.ondemand.com
    - name: fiori-tools-appreload
      afterMiddleware: compression
      configuration:
        port: 35729
        path: webapp
```

### 8. Common Quick Fixes

Try these in order:

```bash
# 1. Clear browser cache and hard reload (Ctrl+Shift+R or Cmd+Shift+R)

# 2. Stop the server and restart
# Press Ctrl+C to stop, then:
npm run start

# 3. Clear npm cache and reinstall
rm -rf node_modules package-lock.json .cache
npm cache clean --force
npm install
npm run start

# 4. Try a different port
npm run start -- --port 8081

# 5. Check if another process is using the port
lsof -i :8080  # Linux/Mac
netstat -ano | findstr :8080  # Windows
```

### 9. Enable Debug Mode

Add these parameters to your URL to get more debugging info:
```
http://localhost:8080/index.html?sap-ui-debug=true&sap-ui-xx-debugModuleLoading=true
```

### 10. Check Network Tab

In browser DevTools, go to Network tab and check:
- Are all files loading (200 status)?
- Any 404 or 500 errors?
- Is the manifest.json loading?
- Is Component.js loading?

## Step-by-Step Debugging Process

1. **Open DevTools** (F12)
2. **Check Console** for red errors
3. **Check Network** for failed requests
4. **Verify** manifest.json, Component.js, and index.html are loaded
5. **Look for** the specific error message and search for solutions

## Need More Help?

If you're still seeing a blank page, please share:
1. Any error messages from the browser console
2. Your manifest.json file
3. Your Component.js file
4. The URL you're trying to access
5. Screenshot of Network tab showing what files loaded/failed
