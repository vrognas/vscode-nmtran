# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is a VSCode extension providing comprehensive language support for NMTRAN (NONMEM Translator) files used in pharmacometric modeling.

### About NMTRAN

NMTRAN is a specialized language for pharmacokinetic/pharmacodynamic modeling:
- **Control stream files** (`.mod`, `.ctl`) contain modeling instructions
- **Control records** start with `$` (e.g., `$THETA`, `$OMEGA`, `$PK`)
- **Parameter types** with strict sequential numbering:
  - `THETA(1)`, `THETA(2)`, etc. - Fixed effects (no gaps allowed)
  - `ETA(1)`, `ETA(2)`, etc. - Random inter-individual effects  
  - `EPS(1)`, `EPS(2)`, etc. - Residual variabilities

### Key Extension Features

- Syntax highlighting and code folding for control records
- IntelliSense: hover info, go-to-definition, auto-completion
- Real-time validation of control records and parameter sequences
- Support for BLOCK matrices and SAME keyword references

**Reference**: Full NMTRAN documentation at <https://nmhelp.tingjieguo.com>

## Architecture

**Client-Server LSP Design** with service-based architecture for maintainability.

### Core Pattern
- **Client** (`client/src/`): VSCode integration, folding provider, extension lifecycle
- **Server** (`server/src/`): Language intelligence via specialized services
- **Services** (`server/src/services/`): Each feature (hover, diagnostics, completion) in separate service
- **Document Lifecycle**: Centralized caching and debounced validation (500ms)

### Key Services
- **DocumentService**: Document caching and lifecycle management
- **DiagnosticsService**: Control record and parameter validation
- **HoverService**: Explanations for control records and parameters
- **DefinitionService**: Go-to-definition and find-references for parameters
- **ParameterScanner**: THETA/ETA/EPS tracking with BLOCK matrix support

### Important Files
- `server/src/constants.ts`: Valid control records and abbreviations
- `server/src/hoverInfo.ts`: Control record explanations
- `server/src/utils/validateControlRecords.ts`: Validation logic
- `syntaxes/nmtran.tmLanguage.json`: Syntax highlighting grammar

## Development Commands

### Essential Commands
```bash
npm run build           # Production build (esbuild to dist/)
npm test               # Run all tests (client + server)
npm run test:server    # Jest unit tests only
npm run validate       # All quality checks (lint + compile + test)
npm run lint           # ESLint code style check
```

### Development Workflow
```bash
npm run compile:watch   # TypeScript compilation in watch mode
npm run bundle:watch    # esbuild bundling in watch mode  
cd server && npm run test:watch  # Jest tests in watch mode
```

**Reference**: See `MAINTENANCE.md` for detailed debugging and troubleshooting procedures.

## Key File Locations

### Client Code
- `client/src/extension.ts` - Extension entry point
- `client/src/features/foldingProvider.ts` - Code folding implementation

### Server Code  
- `server/src/server.ts` - Language server main entry
- `server/src/services/` - All language feature services
- `server/src/constants.ts` - Valid control records list
- `server/src/hoverInfo.ts` - Control record explanations

### Configuration & Assets
- `syntaxes/nmtran.tmLanguage.json` - Syntax highlighting
- `snippets/snippets.json` - Code completion templates
- `test/` - Sample NMTRAN files for testing

**Reference**: See `ARCHITECTURE.md` for complete directory structure.

## Language Features

**Supported file extensions**: `.mod`, `.ctl`, `.lst`, `.modt`, `.phi`, `.coi`, `.cor`, `.cov`, `.cnv`, `.scm`, `.ext`

### Core Capabilities
- **Syntax highlighting** and **code folding** for control records
- **Hover explanations** for control records and parameters  
- **Go-to-definition** and **find-references** for THETA/ETA/EPS parameters
- **Auto-completion** for control records and common patterns
- **Real-time validation** with error highlighting and quick fixes
- **Document formatting** with configurable indentation
- **Code snippets** for common NMTRAN structures

### Parameter Intelligence
- **THETA/ETA/EPS tracking** with strict sequential numbering validation
- **BLOCK matrix support** including multi-line definitions
- **SAME keyword resolution** for parameter references
- **Parameter navigation** throughout the document

## Development Guidelines

### Key Patterns to Follow

**Service-Based Architecture**: Each language feature is a separate service with dependency injection
```typescript
export class NewService {
  constructor(private connection: Connection) {}
  
  provideFeature(document: TextDocument, position: Position) {
    // Implementation following existing patterns
  }
}
```

**Testing Approach**: Jest-based test-driven development with 80% coverage requirement
- Write tests first, avoid mocks for actual functionality
- Use `npm run test:watch` during development
- Follow existing test patterns in `server/src/test/`

### Important Constraints

**NMTRAN Parameter Rules**:
- THETA/ETA/EPS must have strict sequential numbering (no gaps)
- Control records start with `$` and have specific validation rules
- BLOCK matrices require special parsing for diagonal/off-diagonal elements

**Performance Requirements**:
- Use debounced validation (500ms) for diagnostics
- Implement document caching through DocumentService
- Follow incremental text synchronization patterns

### Common Development Tasks

**Adding Control Records**: Update `constants.ts` and `hoverInfo.ts`
**Adding Services**: Follow dependency injection pattern, register in `server.ts`
**Validation Changes**: Modify `utils/validateControlRecords.ts` with tests

**References**: See `MAINTENANCE.md` for step-by-step procedures and `ARCHITECTURE.md` for detailed design patterns.
