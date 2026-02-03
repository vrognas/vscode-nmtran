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

See [CONTRIBUTING.md](CONTRIBUTING.md) for development workflow.

## NMTRAN Language

NMTRAN is FORTRAN-based with special semantics for pharmacokinetic modeling:
- Control records start with `$` (e.g., `$THETA`, `$OMEGA`, `$PK`)
- Parameters: `THETA(n)`, `ETA(n)`, `EPS(n)` - must be sequential, no gaps
- BLOCK matrices with SAME keyword references
- Continuation lines use `&` character

**Reference**: https://nmhelp.tingjieguo.com

## Architecture

```
┌─────────────────┐         LSP          ┌─────────────────┐
│     Client      │ ◄──────────────────► │     Server      │
│  (VS Code API)  │                      │ (Language Intel)│
└─────────────────┘                      └─────────────────┘
```

**Client** (`client/src/`): VSCode integration, folding
**Server** (`server/src/`): Language intelligence via services

| Service | Responsibility |
|---------|---------------|
| DocumentService | Document caching and lifecycle |
| DiagnosticsService | Validation with 500ms debounce |
| HoverService | Control record and parameter explanations |
| DefinitionService | Go-to-definition, find-references |
| CompletionService | Auto-completion suggestions |
| FormattingService | Document and range formatting |
| ParameterScanner | THETA/ETA/EPS tracking, BLOCK matrix support |

**Data flow:** Document opened → cached → changes trigger debounced diagnostics → closed clears cache

**Build:** `tsc -b` for type checking, `esbuild` bundles to `dist/` for production

## Common Tasks

**Add control record**: Update `constants.ts` and `hoverInfo.ts`
**Add service**: Follow DI pattern in `server/src/services/`, register in `server.ts`
**Fix validation**: Modify `utils/validateControlRecords.ts`

## Constraints

- Debounced validation (500ms)
- Document caching via DocumentService
- 80% test coverage requirement
- esbuild bundles to `dist/` (not TypeScript output)
- No telemetry collected
