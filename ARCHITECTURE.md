# NMTRAN Language Extension Architecture

## Overview

This VS Code extension provides language support for NMTRAN (NONMEM Translator) files. It follows a client-server architecture using the Language Server Protocol (LSP).

## Directory Structure

```
vscode-nmtran/
├── client/                 # VS Code extension client
│   └── src/
│       ├── extension.ts    # Extension entry point
│       ├── config.ts       # Configuration management
│       ├── logger.ts       # Client-side logging
│       └── features/       # Client features
│           └── languageServer.ts
├── server/                 # Language server
│   └── src/
│       ├── server.ts       # Server entry point
│       ├── types/          # Shared TypeScript types
│       ├── constants/      # Shared constants
│       ├── services/       # Core services
│       │   ├── definitionService.ts    # Go to Definition
│       │   ├── diagnosticsService.ts   # Error checking
│       │   ├── hoverService.ts         # Hover information
│       │   ├── formattingService.ts    # Code formatting
│       │   ├── completionService.ts    # Auto-completion
│       │   └── ParameterScanner.ts     # Parameter scanning
│       ├── utils/          # Utility functions
│       │   ├── NMTRANMatrixParser.ts   # Matrix calculations
│       │   ├── validation.ts           # Input validation
│       │   ├── errorHandler.ts         # Error handling
│       │   └── performanceMonitor.ts   # Performance tracking
│       ├── parsers/        # Parsing utilities
│       └── factories/      # Object factories
├── syntaxes/              # TextMate grammars
├── snippets/              # Code snippets
├── test/                  # Test NMTRAN files
└── dist/                  # Bundled output

```

## Architecture Principles

### 1. Service-Based Architecture
Each language feature is implemented as a separate service with a clear interface:
- **DefinitionService**: Handles "Go to Definition" requests
- **DiagnosticsService**: Validates NMTRAN syntax
- **HoverService**: Provides hover information
- **FormattingService**: Formats NMTRAN code
- **CompletionService**: Provides auto-completion

### 2. Separation of Concerns
- **Client**: Manages VS Code integration and UI
- **Server**: Handles all language intelligence
- **Services**: Each service has a single responsibility
- **Utils**: Reusable utilities shared across services

### 3. Performance Considerations
- **Caching**: Parameter scan results are cached to avoid re-parsing
- **Debouncing**: Diagnostics are debounced to avoid excessive validation
- **Compiled Patterns**: Regex patterns are compiled once and reused

### 4. Error Handling
- All service methods are wrapped with error handlers
- Errors are logged but don't crash the server
- Graceful fallbacks for all operations

## Key Components

### Parameter Scanner
The `ParameterScanner` is responsible for finding all parameter definitions in a document:
- Handles simple parameters (THETA, OMEGA, SIGMA)
- Processes BLOCK matrices including multi-line definitions
- Handles SAME keyword references
- Tracks diagonal vs off-diagonal elements in matrices

### NMTRAN Matrix Parser
Handles the mathematical aspects of NMTRAN matrices:
- Calculates diagonal positions in lower triangular matrices
- Validates matrix structures
- Extracts matrix elements

### Definition Service
Provides "Go to Definition" functionality:
- Finds parameter definitions (e.g., clicking on THETA(3) jumps to its definition)
- Handles multiple locations for SAME keywords
- Supports BLOCK matrix navigation

## Data Flow

1. **User Action**: User clicks "Go to Definition" on a parameter
2. **Client**: VS Code client sends request to language server
3. **Server**: Routes request to DefinitionService
4. **DefinitionService**: 
   - Identifies parameter at cursor position
   - Uses ParameterScanner to find all parameter locations
   - Returns matching definition locations
5. **Client**: VS Code navigates to the definition

## Build System

The project uses two build systems:

1. **TypeScript Compilation** (`tsc`):
   - Compiles to `client/out` and `server/out`
   - Used for type checking and development

2. **esbuild Bundling**:
   - Bundles everything to `dist/`
   - This is what the extension actually uses
   - Much faster than webpack

**Important**: Always run `npm run build` before testing changes!

## Testing Strategy

1. **Unit Tests**: Test individual components (ParameterScanner, NMTRANMatrixParser)
2. **Integration Tests**: Test complete workflows (definition flow)
3. **Manual Testing**: Use test files in `test/` directory

## Configuration

The extension respects VS Code settings:
- Indentation size for formatting
- Debug mode for additional logging
- File associations for NMTRAN extensions

## Privacy

This extension does not collect any telemetry or usage data. All operations are performed locally.

## Future Improvements

1. **Incremental Parsing**: Only re-parse changed portions of documents
2. **Workspace-wide Features**: Find references across multiple files
3. **Semantic Highlighting**: Enhanced syntax highlighting based on semantic analysis
4. **Refactoring Support**: Rename parameters across the document