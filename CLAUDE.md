# CLAUDE.md

Guidance for Claude Code when working with this repository.

## Project Overview

VSCode extension providing NMTRAN (NONMEM Translator) language support for pharmacometric modeling.

**Key files:**
- `server/src/constants.ts` - Valid control records
- `server/src/hoverInfo.ts` - Control record explanations
- `server/src/services/` - Language feature services
- `syntaxes/nmtran.tmLanguage.json` - Syntax highlighting

**Commands:**
```bash
npm run build      # Production build
npm run dev        # Watch mode
npm test           # All tests
npm run validate   # Lint + compile + test
```

See [CONTRIBUTING.md](CONTRIBUTING.md) for development workflow and [ARCHITECTURE.md](ARCHITECTURE.md) for design details.

## NMTRAN Language

NMTRAN is FORTRAN-based with special semantics for pharmacokinetic modeling:
- Control records start with `$` (e.g., `$THETA`, `$OMEGA`, `$PK`)
- Parameters: `THETA(n)`, `ETA(n)`, `EPS(n)` - must be sequential, no gaps
- BLOCK matrices with SAME keyword references
- Continuation lines use `&` character

**Reference**: https://nmhelp.tingjieguo.com

## Architecture

Client-server LSP design with service-based architecture:
- **Client** (`client/src/`): VSCode integration, folding
- **Server** (`server/src/`): Language intelligence
- **Services**: DocumentService, DiagnosticsService, HoverService, DefinitionService, CompletionService

## Common Tasks

**Add control record**: Update `constants.ts` and `hoverInfo.ts`
**Add service**: Follow DI pattern in `server/src/services/`, register in `server.ts`
**Fix validation**: Modify `utils/validateControlRecords.ts`

## Constraints

- Debounced validation (500ms)
- Document caching via DocumentService
- 80% test coverage requirement
- esbuild bundles to `dist/` (not TypeScript output)
