# NMTRAN Extension Architecture

## Overview

VS Code extension providing NMTRAN language support using the Language Server Protocol (LSP).

## Architecture

```
┌─────────────────┐         LSP          ┌─────────────────┐
│     Client      │ ◄──────────────────► │     Server      │
│  (VS Code API)  │                      │ (Language Intel)│
└─────────────────┘                      └─────────────────┘
        │                                        │
        ▼                                        ▼
┌─────────────────┐                      ┌─────────────────┐
│ FoldingProvider │                      │    Services     │
│ LanguageServer  │                      │  (DI pattern)   │
└─────────────────┘                      └─────────────────┘
```

### Client (`client/src/`)

- **extension.ts**: Entry point, initializes language client
- **features/foldingProvider.ts**: Code folding between control records
- **features/languageServer.ts**: Language server lifecycle management

### Server (`server/src/`)

- **server.ts**: LSP server, routes requests to services
- **services/**: Feature implementations (dependency injection pattern)
- **utils/**: Shared utilities
- **constants.ts**: Valid control records and abbreviations
- **hoverInfo.ts**: Control record explanations

## Services

| Service | Responsibility |
|---------|---------------|
| DocumentService | Document caching and lifecycle |
| DiagnosticsService | Validation with 500ms debounce |
| HoverService | Control record and parameter explanations |
| DefinitionService | Go-to-definition, find-references |
| CompletionService | Auto-completion suggestions |
| FormattingService | Code formatting |
| ParameterScanner | THETA/ETA/EPS tracking, BLOCK matrix support |

## Data Flow

### Document Lifecycle
1. Document opened → DocumentService caches it
2. Changes → Incremental sync → DiagnosticsService (debounced)
3. Document closed → Cache cleared

### Go to Definition
1. User triggers "Go to Definition" on `THETA(3)`
2. Client sends request → Server routes to DefinitionService
3. DefinitionService uses ParameterScanner to find definitions
4. Returns location(s) → Client navigates

## Build System

| Build | Output | Purpose |
|-------|--------|---------|
| `tsc -b` | `client/out/`, `server/out/` | Type checking, development |
| `esbuild` | `dist/` | Production bundles (extension uses this) |

## Key Design Decisions

1. **Service-based architecture**: Each feature isolated, testable, maintainable
2. **Debounced diagnostics**: 500ms delay prevents validation spam during typing
3. **Document caching**: Avoid re-parsing for repeated operations
4. **Incremental sync**: Only send changed text, not entire document
5. **Compiled regex patterns**: Reused for performance

## Directory Structure

```
vscode-nmtran/
├── client/src/
│   ├── extension.ts
│   └── features/
├── server/src/
│   ├── server.ts
│   ├── services/
│   ├── utils/
│   └── test/
├── syntaxes/          # TextMate grammar
├── snippets/          # Code snippets
├── test/              # Sample NMTRAN files
└── dist/              # Bundled output
```

## Privacy

No telemetry or usage data collected. All operations local.
