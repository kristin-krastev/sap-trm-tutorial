# Fix: Fiori Command Not Found Error

## Problem
When running `npm run start`, you get the error:
```
sh: 1: fiori: not found
```

## Solution

### Step 1: Navigate to your Fiori app directory
```bash
cd path/to/ibso.commodity.observer.request
```

### Step 2: Install SAP Fiori tools
Run the following command to install the required SAP Fiori tools:

```bash
npm install --save-dev @sap/ux-ui5-tooling @ui5/cli @sap/fiori-tools-proxy @sap/fiori-tools-appreload
```

### Step 3: Verify package.json
Make sure your `package.json` has the following in the `devDependencies` section:

```json
{
  "devDependencies": {
    "@sap/ux-ui5-tooling": "^1",
    "@ui5/cli": "^3",
    "@sap/fiori-tools-proxy": "^1",
    "@sap/fiori-tools-appreload": "^1"
  }
}
```

### Step 4: Install all dependencies
```bash
npm install
```

### Step 5: Try running the app again
```bash
npm run start
```

## Alternative: Check if package.json exists

If you don't have a `package.json` file in your Fiori app directory, you need to create one first:

```bash
npm init -y
```

Then follow steps 2-5 above.

## Alternative: Global installation (not recommended for projects)

If you want to install the Fiori tools globally (though this is not recommended for project work):

```bash
npm install -g @sap/fiori-tools
```

## Common Issues

1. **npm not found**: Make sure Node.js and npm are installed
2. **Permission errors**: Try using `sudo` (on Linux/Mac) or run as administrator (on Windows)
3. **Network errors**: If you're behind a corporate proxy, configure npm proxy settings

## Additional Notes for BAS (Business Application Studio)

In BAS, the Fiori tools should typically be pre-installed. If you're getting this error:

1. Check if you're in the correct directory (your Fiori app root)
2. Try deleting `node_modules` folder and `package-lock.json`, then run `npm install` again
3. Restart your BAS workspace

```bash
rm -rf node_modules package-lock.json
npm install
```
