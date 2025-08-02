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
│       ├── features/       # Client features
│       │   ├── foldingProvider.ts      # Code folding support
│       │   └── languageServer.ts       # Language server management
│       └── test/           # Client tests
│           ├── runTest.ts
│           └── suite/
├── server/                 # Language server
│   └── src/
│       ├── server.ts       # Server entry point
│       ├── types.ts        # Shared TypeScript types
│       ├── constants.ts    # Main constants file
│       ├── hoverInfo.ts    # Hover information content
│       ├── constants/      # Additional constants
│       │   └── parameters.ts
│       ├── services/       # Core services
│       │   ├── definitionService.ts    # Go to Definition
│       │   ├── diagnosticsService.ts   # Error checking
│       │   ├── hoverService.ts         # Hover information
│       │   ├── formattingService.ts    # Code formatting
│       │   ├── completionService.ts    # Auto-completion
│       │   ├── documentService.ts      # Document lifecycle management
│       │   └── ParameterScanner.ts     # Parameter scanning
│       ├── utils/          # Utility functions
│       │   ├── NMTRANMatrixParser.ts   # Matrix calculations
│       │   ├── validation.ts           # Input validation
│       │   ├── errorHandler.ts         # Error handling
│       │   ├── performanceMonitor.ts   # Performance tracking
│       │   ├── parameterParser.ts      # Parameter parsing utilities
│       │   ├── parameterValidator.ts   # Parameter validation
│       │   └── validateControlRecords.ts # Control record validation
│       ├── parsers/        # Parsing utilities
│       │   └── parameterParser.ts
│       ├── factories/      # Object factories
│       │   └── parameterFactory.ts
│       └── test/           # Server tests
│           ├── *.test.ts   # Unit tests
│           └── integration/ # Integration tests
├── syntaxes/              # TextMate grammars
├── snippets/              # Code snippets
├── test/                  # Sample NMTRAN files for testing
└── dist/                  # Bundled output (esbuild)

```

## Architecture Principles

### 1. Service-Based Architecture
Each language feature is implemented as a separate service with a clear interface:
- **DocumentService**: Manages document lifecycle and caching
- **DefinitionService**: Handles "Go to Definition" and "Find References" requests
- **DiagnosticsService**: Validates NMTRAN syntax and control records
- **HoverService**: Provides hover information for control records and parameters
- **FormattingService**: Formats NMTRAN code with configurable indentation
- **CompletionService**: Provides auto-completion for control records and parameters
- **ParameterScanner**: Specialized service for scanning and tracking THETA, ETA, EPS parameters

### 2. Separation of Concerns
- **Client**: Manages VS Code integration, UI, and client-side features like folding
- **Server**: Handles all language intelligence via Language Server Protocol
- **Services**: Each service has a single responsibility and clear interface
- **Utils**: Reusable utilities shared across services (validation, parsing, error handling)
- **Factories**: Object creation patterns for parameters and other entities

### 3. Performance Considerations
- **Document Caching**: Documents are cached in DocumentService to avoid re-reading
- **Debounced Diagnostics**: Diagnostics are debounced (500ms) to avoid excessive validation during rapid typing
- **Compiled Patterns**: Regex patterns are compiled once and reused for better performance
- **Incremental Updates**: Uses incremental text document synchronization for efficient change handling

### 4. Error Handling
- All service methods are wrapped with error handlers
- Errors are logged but don't crash the server
- Graceful fallbacks for all operations

## Key Components

### Document Service
The `DocumentService` manages document lifecycle and provides centralized document operations:
- Caches documents in memory for fast access
- Handles document open/change/close events
- Provides document creation and retrieval methods
- Tracks cache statistics for debugging and performance monitoring

### Parameter Scanner
The `ParameterScanner` is responsible for finding all parameter definitions in a document:
- Handles simple parameters (THETA, OMEGA, SIGMA) 
- Processes BLOCK matrices including multi-line definitions
- Handles SAME keyword references
- Tracks diagonal vs off-diagonal elements in matrices
- Uses compiled regex patterns for efficient scanning

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

### Document Lifecycle
1. **Document Open**: Client sends document open event to server
2. **DocumentService**: Creates and caches TextDocument instance
3. **DiagnosticsService**: Performs initial validation (debounced)

### Go to Definition Example
1. **User Action**: User clicks "Go to Definition" on a parameter
2. **Client**: VS Code client sends definition request to language server
3. **Server**: Routes request to DefinitionService
4. **DocumentService**: Retrieves cached document
5. **DefinitionService**: 
   - Identifies parameter at cursor position
   - Uses ParameterScanner to find all parameter locations
   - Returns matching definition locations
6. **Client**: VS Code navigates to the definition

### Document Changes
1. **User Edit**: User types in document
2. **Client**: Sends incremental change to server
3. **DocumentService**: Updates cached document with changes
4. **Server**: Schedules debounced diagnostics validation

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

The project uses Jest for comprehensive testing with the following structure:

### 1. Unit Tests (server/src/test/)
- **Component Tests**: ParameterScanner, NMTRANMatrixParser, validation utilities
- **Service Tests**: Individual service testing (HoverService, DefinitionService)
- **Parser Tests**: Parameter parsing, control record validation
- **Edge Case Tests**: Boundary conditions and error scenarios

### 2. Integration Tests (server/src/test/integration/)
- **Definition Flow**: Complete "Go to Definition" workflows
- **End-to-End**: Full feature testing with realistic NMTRAN files

### 3. Client Tests (client/src/test/)
- **Extension Tests**: VS Code extension activation and lifecycle
- **Integration**: Client-server communication testing

### 4. Manual Testing
- **Sample Files**: Use realistic NMTRAN files in `test/` directory
- **Development Testing**: Real-world scenarios with various NMTRAN constructs

### Test Configuration
- **Jest**: Primary test runner for server-side tests
- **VS Code Test API**: For client-side extension testing
- **TypeScript**: Full type checking in test files

## Client-Side Features

### Folding Provider
The client implements a custom folding provider (`NMTRANFoldingProvider`) that:
- Creates foldable regions between NMTRAN control records (lines starting with `$`)
- Only creates folds when there's meaningful content between control records
- Uses VS Code's FoldingRangeKind.Region for proper styling
- Provides enhanced code navigation for large NMTRAN files

## Configuration

The extension respects VS Code settings through structured configuration:
- **Formatting**: Configurable indentation size (2 or 4 spaces)
- **Diagnostics**: Maximum number of problems to report
- **Debug Mode**: Additional logging for development and troubleshooting
- **Server Settings**: Timeout and debug port configuration
- **File Associations**: Automatic detection of NMTRAN file extensions

## Privacy

This extension does not collect any telemetry or usage data. All operations are performed locally.

## Current Capabilities

The extension currently provides comprehensive NMTRAN language support:
- **Code Folding**: Client-side folding between control records
- **Syntax Highlighting**: TextMate grammar-based tokenization
- **Hover Information**: Detailed explanations for control records and parameters
- **Diagnostics**: Real-time validation with debounced updates
- **Go to Definition**: Navigate to parameter definitions including BLOCK matrices
- **Find References**: Locate all parameter usages
- **Code Completion**: Smart suggestions for control records and parameters
- **Document Formatting**: Configurable indentation and structure formatting
- **Document Outline**: Symbol provider for navigation sidebar

## Future Improvements

1. **Workspace-wide Features**: Find references and definitions across multiple files
2. **Semantic Highlighting**: Enhanced syntax highlighting based on semantic analysis  
3. **Refactoring Support**: Rename parameters across the document with validation
4. **Advanced Diagnostics**: Cross-record validation and parameter consistency checking
5. **Enhanced Matrix Support**: Better visualization and navigation for complex BLOCK structures