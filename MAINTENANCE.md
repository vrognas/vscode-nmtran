# NMTRAN Extension Maintenance Guide

This guide helps you maintain and update the NMTRAN VSCode extension safely and efficiently.

## Quick Start Commands

```bash
# Test everything works
npm test

# Build the extension
npm run compile

# Check for code issues
npm run lint

# Package for distribution
npm run vscode:prepublish
```

## Common Maintenance Tasks

### 1. Adding New NMTRAN Control Records

When NONMEM adds new control records, update them here:

**File: `server/src/constants.ts`**
- Add the new control record to the `allowedControlRecords` array
- Keep alphabetically sorted
- Include both full names and common abbreviations

**File: `server/src/hoverInfo.ts`**  
- Add explanation in the `explainControlRecordHover` function
- Follow the existing pattern: `case '$NEWRECORD': return 'Description here';`

**Test your changes:**
```bash
npm test  # Run validation tests
```

### 2. Fixing Validation Issues

If users report control records being incorrectly flagged:

1. Check if the record is in `server/src/constants.ts`
2. If missing, add it following step 1 above
3. If present, the issue might be in `server/src/utils/validateControlRecords.ts`
4. Run tests after any changes: `npm test`

### 3. Updating Hover Information

To fix or improve hover explanations:

**File: `server/src/hoverInfo.ts`**
- Find the control record in the switch statement
- Update the description
- Keep explanations concise but informative

### 4. Debugging Extension Issues

**View extension logs:**

*Language Server Logs (hover, validation, diagnostics):*
1. In Extension Development Host: `Cmd+Shift+P`
2. Type "Developer: Show Logs"
3. Select **"NMTRAN Language Server"**
4. Look for emoji-prefixed messages:
   - 🚀 Server initializing...
   - 🔍 Validating filename.mod...
   - 📄 Found X control records: $PROBLEM, $THETA, etc.
   - ⚠️  Issue with $INVALID: Invalid control record
   - ✅ Server ready and listening!

*Extension/Client Logs (activation, folding):*
1. In Extension Development Host: `Cmd+Shift+P`  
2. Type "Developer: Show Logs"
3. Select **"Extension Host"**
4. Look for extension activation and client-side issues

**Debug the language server:**
1. Press F5 to launch Extension Development Host
2. Use "Debug Extension + Server" for full breakpoint debugging
3. Server runs on debug port 6009 (see `client/src/extension.ts`)
4. Set breakpoints in both client (`extension.ts`) and server (`server.ts`) code

### 5. Testing Changes

**Run validation tests:**
```bash
npm test
```

**Manual testing:**
1. Open any `.mod` file in VSCode  
2. Test hover on control records like `$THETA`, `$OMEGA`
3. Try invalid records like `$INVALID` - should show red squiggle
4. Try abbreviations like `$EST` - should suggest `$ESTIMATION`

### 6. Code Quality Checks

**Before committing changes:**
```bash
npm run lint     # Check code style
npm run compile  # Ensure TypeScript compiles
npm test         # Run validation tests
```

## File Organization

```
├── client/src/extension.ts          # VSCode integration, folding
├── server/src/
│   ├── server.ts                    # Main language server logic  
│   ├── constants.ts                 # Valid control records list
│   ├── hoverInfo.ts                 # Control record explanations
│   └── utils/validateControlRecords.ts  # Validation logic
└── test/                            # Sample NMTRAN files
```

## Release Process

1. Update version in `package.json`
2. Update `CHANGELOG.md` with changes
3. Run quality checks: `npm run lint && npm test && npm run compile`
4. Package: `npm run vscode:prepublish`
5. Test the packaged extension manually
6. Publish via VSCode Marketplace

## Common Issues & Solutions

**Issue: Extension not loading**
- Check `client/out/extension.js` exists (run `npm run compile`)
- Check VSCode extension host logs for errors

**Issue: Hover not working**  
- Verify control record is in `constants.ts`
- Check `hoverInfo.ts` has corresponding explanation
- Test with `npm test`

**Issue: False validation errors**
- Check if record is properly spelled in `constants.ts`
- Verify regex pattern in `validateControlRecords.ts` matches correctly

**Issue: TypeScript compilation errors**
- Run `npm run compile` to see detailed errors
- Check imports are correct in affected files

## Getting Help

1. Check existing issues: https://github.com/vrognas/vscode-nmtran/issues
2. Run `npm test` to verify core functionality  
3. Use VSCode Developer Tools for debugging
4. Create minimal reproduction case with sample NMTRAN file