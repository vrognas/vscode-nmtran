# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is a VSCode extension providing comprehensive language support for NMTRAN (NONMEM Translator) files used in pharmacometric modeling.

All notable changes to the NMTRAN VSCode extension will be documented in CHANGELOG.md.
The format of the CHANGELOG adheres to [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and the project versioning adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

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

### NONMEM Workflow (v7.6.0+)

NONMEM executes through six fundamental program steps:
1. **Simulation Step**: Generate simulated data under specified model
2. **Initial Estimation Step**: Compute initial parameter estimates
3. **Estimation Step**: Obtain final parameter estimates via objective function minimization
4. **Covariance Step**: Calculate parameter estimate covariance matrix
5. **Tables Step**: Generate output tables of data items and predictions
6. **Scatterplot Step**: Create diagnostic plots and visualizations

**Data Formatting Heritage**: NMTRAN inherits FORTRAN I/O formatting (E, F, X format codes) for `$DATA` record specifications, enabling precise control over numeric data reading and formatting.

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

**NMTRAN Language Constraints**:
- **Abbreviated Code**: NMTRAN is essentially FORTRAN with special semantics
- **Left-hand quantities**: Mandatory (Y) and optional (COM(n)) reserved variables
- **Right-hand quantities**: Data items, THETA(n), ETA(n), EPS(n), reserved variables
- **Random variables**: Any variable depending on ETA/EPS becomes a random variable
- **Sequential numbering**: THETA/ETA/EPS must be sequential with no gaps
- **Conditional restrictions**: Random variables cannot be defined in nested conditionals
- **ICALL contexts**: Different execution blocks (0=run init, 1=problem init, 2=analysis, 3=finalization, 4=simulation, 5=expectation, 6=data average)
- **BLOCK matrices**: Multi-line matrix definitions with SAME keyword support

**Performance Requirements**:
- Use debounced validation (500ms) for diagnostics
- Implement document caching through DocumentService
- Follow incremental text synchronization patterns

### NMTRAN Syntax Rules

**Abbreviated Code Structure**:
- Nearly every statement is syntactically FORTRAN with special semantics
- Assignment statements, conditional statements, arithmetic/logical expressions allowed
- Limited FORTRAN constructs: IF/THEN/ELSE/ENDIF, DO WHILE/ENDDO, CALL, WRITE, PRINT
- No GOTO, READ, FORMAT statements allowed
- Continuation lines use `&` character (FORTRAN 95 style)

**Reserved Elements**:
- **ICALL**: Execution context (0-6 for different phases)
- **NEWIND**: Individual record tracking (0=first, 1=new individual, 2=continuation)
- **ERR(n)**: Alternative to ETA(n)/EPS(n) for clarity
- **Verbatim code**: FORTRAN code inserted with `"` prefix (bypasses NM-TRAN processing)

### Common Development Tasks

**Adding Control Records**: Update `constants.ts` and `hoverInfo.ts`
**Adding Services**: Follow dependency injection pattern, register in `server.ts`
**Validation Changes**: Modify `utils/validateControlRecords.ts` with tests

**NMTRAN Validation Patterns**:
- **Random Variable Tracking**: Variables using ETA/EPS become random variables
- **Function Restrictions**: Some functions (INT, MOD, MIN, MAX) don't compute derivatives
- **Protective Functions**: PLOG, PEXP, etc. prevent domain violations (NONMEM 7.4+)
- **SAME Keyword Resolution**: Reference previous parameter blocks in validation

**References**: See `MAINTENANCE.md` for step-by-step procedures and `ARCHITECTURE.md` for detailed design patterns.
