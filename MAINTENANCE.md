# NMTRAN Extension Maintenance Guide

This guide helps you maintain and update the NMTRAN VSCode extension safely and efficiently.

## Quick Start Commands

```bash
# Test everything works (both client and server tests)
npm test

# Run server-side unit tests specifically
npm run test:server

# Build the extension (TypeScript compilation)
npm run compile

# Build for production (esbuild bundling)
npm run build

# Check for code issues
npm run lint

# Package for distribution
npm run vscode:prepublish

# Validate everything before committing
npm run validate
```

## Common Maintenance Tasks

### 1. Adding New NMTRAN Control Records

When NONMEM adds new control records, update them in multiple locations:

**File: `server/src/constants.ts`**
- Add the new control record to the `allowedControlRecords` array
- Keep alphabetically sorted
- Include both full names and common abbreviations

**File: `server/src/hoverInfo.ts`**  
- Add explanation in the `explainControlRecordHover` function
- Follow the existing pattern: `case '$NEWRECORD': return 'Description here';`

**Optional: Add specific constants**
**File: `server/src/constants/parameters.ts`**
- Add parameter-specific constants if the new record introduces new parameter types

**Test your changes:**
```bash
npm run test:server  # Run server unit tests
npm test            # Run full test suite
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

*Language Server Logs (hover, validation, diagnostics, services):*
1. In Extension Development Host: `Cmd+Shift+P`
2. Type "Developer: Show Logs"
3. Select **"NMTRAN Language Server"**
4. Look for emoji-prefixed messages:
   - 🚀 Server initializing...
   - 📝 Document cached: filename.mod
   - 🔍 Hover request for parameter THETA(3)
   - 🎨 Format document request
   - ⚠️  Error in service: [error details]
   - ✅ Server ready and listening!

*Extension/Client Logs (activation, folding, language server management):*
1. In Extension Development Host: `Cmd+Shift+P`  
2. Type "Developer: Show Logs"
3. Select **"Extension Host"**
4. Look for extension activation, folding provider, and client-side issues

**Debug the language server:**
1. Press F5 to launch Extension Development Host
2. Use "Debug Extension + Server" configuration for full breakpoint debugging
3. Server runs on configurable debug port (default 6009)
4. Set breakpoints in:
   - Client code: `client/src/extension.ts`, `client/src/features/`
   - Server code: `server/src/server.ts`, `server/src/services/`

**Testing individual services:**
```bash
# Run specific test files
cd server
npm test -- parameterScanner.test.ts
npm test -- hoverService.test.ts
npm test -- definitionService.test.ts
```

### 5. Testing Changes

**Run automated tests:**
```bash
# Full test suite (client + server)
npm test

# Server unit tests only (Jest-based)
npm run test:server

# Run tests in watch mode during development
cd server && npm run test:watch

# Run specific test files
cd server && npm test -- definitionService.test.ts
```

**Manual testing:**
1. Open any `.mod` file in VSCode  
2. Test hover on control records like `$THETA`, `$OMEGA`
3. Try invalid records like `$INVALID` - should show red squiggle
4. Try abbreviations like `$EST` - should suggest `$ESTIMATION`
5. Test Go to Definition on parameters like `THETA(3)`
6. Test Find References on parameters
7. Test code folding on control record sections
8. Test document formatting with different indentation settings

### 6. Code Quality Checks

**Before committing changes:**
```bash
npm run validate # Run all quality checks (lint + compile + test)
```

**Individual quality checks:**
```bash
npm run lint     # Check code style (ESLint)
npm run compile  # Ensure TypeScript compiles
npm run build    # Test esbuild bundling
npm test         # Run all tests
npm run test:server # Run server tests with coverage
```

**Jest test coverage:**
- Coverage thresholds: 80% for branches, functions, lines, statements
- Coverage reports generated in `server/coverage/`
- View detailed coverage: `cd server && npm test -- --coverage`

## File Organization

```
├── client/src/
│   ├── extension.ts                 # VSCode extension entry point
│   ├── config.ts                    # Configuration management  
│   ├── logger.ts                    # Client-side logging
│   └── features/
│       ├── foldingProvider.ts       # Code folding implementation
│       └── languageServer.ts        # Language server management
├── server/src/
│   ├── server.ts                    # Main language server logic
│   ├── types.ts                     # Shared TypeScript types
│   ├── constants.ts                 # Valid control records list
│   ├── hoverInfo.ts                 # Control record explanations
│   ├── services/                    # Service-based architecture
│   │   ├── documentService.ts       # Document lifecycle management
│   │   ├── diagnosticsService.ts    # Error checking and validation
│   │   ├── hoverService.ts          # Hover information provider
│   │   ├── definitionService.ts     # Go to definition/references
│   │   ├── completionService.ts     # Auto-completion
│   │   ├── formattingService.ts     # Code formatting
│   │   └── ParameterScanner.ts      # Parameter scanning logic
│   ├── utils/                       # Utility functions
│   │   ├── validateControlRecords.ts # Control record validation
│   │   ├── NMTRANMatrixParser.ts    # Matrix parsing utilities
│   │   ├── parameterParser.ts       # Parameter parsing
│   │   ├── parameterValidator.ts    # Parameter validation
│   │   ├── errorHandler.ts          # Error handling
│   │   └── performanceMonitor.ts    # Performance tracking
│   ├── factories/                   # Object creation patterns
│   ├── parsers/                     # Parsing utilities
│   └── test/                        # Jest unit and integration tests
├── syntaxes/                        # TextMate grammar files
├── snippets/                        # Code completion snippets
├── test/                            # Sample NMTRAN files for manual testing
└── dist/                            # esbuild bundled output
```

## Release Process

1. Update version in `package.json`
2. Update `CHANGELOG.md` with changes
3. Run quality checks: `npm run validate`
4. Package: `npm run vscode:prepublish`
5. Test the packaged extension manually
6. Use pre-release script: `./scripts/pre-release.sh` (optional)
7. Publish via VSCode Marketplace

## Common Issues & Solutions

**Issue: Extension not loading**
- Check `dist/extension.js` exists (run `npm run build`)
- Check `client/out/extension.js` exists (run `npm run compile`)
- Check VSCode extension host logs for errors
- Verify `package.json` main entry points to correct file

**Issue: Hover not working**  
- Verify control record is in `constants.ts`
- Check `hoverService.ts` is properly calling `hoverInfo.ts`
- Test with `npm run test:server -- hoverService.test.ts`

**Issue: False validation errors**
- Check if record is properly spelled in `constants.ts`
- Verify regex pattern in `validateControlRecords.ts` matches correctly
- Test validation with unit tests

**Issue: TypeScript compilation errors**
- Run `npm run compile` to see detailed errors
- Check imports are correct in affected files
- Ensure service dependencies are properly injected

**Issue: Jest tests failing**
- Check `tsconfig.test.json` configuration
- Verify test file naming follows `*.test.ts` pattern
- Run individual tests: `cd server && npm test -- filename.test.ts`

**Issue: Language server not responding**
- Check for server crashes in Language Server logs
- Verify services are properly initialized in `server.ts`
- Test document caching with DocumentService

**Issue: Performance problems**
- Check ParameterScanner caching effectiveness
- Monitor diagnostics debouncing (500ms)
- Review performance metrics in logs

## Adding New Language Features

### Service Development Pattern

The extension uses a service-based architecture. To add new features:

1. **Create a new service** in `server/src/services/`:
   ```typescript
   export class NewFeatureService {
     constructor(private connection: Connection) {}
     
     provideNewFeature(document: TextDocument, position: Position) {
       // Implementation here
     }
   }
   ```

2. **Register in server.ts**:
   ```typescript
   const newFeatureService = new NewFeatureService(connection);
   connection.onNewFeatureRequest(({ textDocument, position }) => {
     const doc = services.document.getDocument(textDocument.uri);
     return newFeatureService.provideNewFeature(doc, position);
   });
   ```

3. **Add tests** in `server/src/test/`:
   - Unit tests for service logic
   - Integration tests for complete workflows

4. **Update package.json capabilities** if needed

### Development Workflow

```bash
# Start development
npm run compile:watch  # TypeScript compilation in watch mode
npm run bundle:watch   # esbuild bundling in watch mode

# Test as you develop
cd server && npm run test:watch

# Validate before committing
npm run validate
```

## Getting Help

1. Check existing issues: https://github.com/vrognas/vscode-nmtran/issues
2. Run `npm run validate` to verify everything works
3. Use VSCode Developer Tools for debugging
4. Review service tests for examples: `server/src/test/`
5. Create minimal reproduction case with sample NMTRAN file