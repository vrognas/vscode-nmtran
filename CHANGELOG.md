## [0.2.17] 4 Feb, 2026

### Changed

* **Test Framework Migration**: Migrated server tests from Jest to Vitest
  - Replaced Jest with Vitest v3.0.0 for faster test execution
  - Added @vitest/coverage-v8 for coverage reporting
  - Created `vitest.config.ts` with 80% coverage thresholds
  - Removed Jest dependencies (`jest`, `ts-jest`, `@types/jest`)

* **Test Type Safety**: Added typed mock infrastructure
  - Created `mocks/mockConnection.ts` with `MockConnection` type and factory
  - Replaced all `any` typed mocks with proper TypeScript types
  - Added `getHoverValue()` helper for type-safe hover content extraction
  - Zero ESLint warnings in test files (was 24)

### Added

* **DocumentService Tests**: New comprehensive test suite for document caching
  - LRU eviction behavior tests
  - Cache statistics tests

## [0.2.16] 3 Feb, 2026

### Changed

* **TypeScript Configuration**: Consolidated tsconfig with shared base config
  - Created `tsconfig.base.json` with unified strict settings
  - Upgraded target from ES2020 to ES2022
  - Added stricter checks: `noUnusedLocals`, `noUnusedParameters`, `noImplicitOverride`
  - Added `forceConsistentCasingInFileNames`, `incremental`, `esModuleInterop`
  - Client and server now extend base config for consistency

### Fixed

* **Dead Code Removal**: Removed unused variables and methods flagged by stricter checks

## [0.2.15] 3 Feb, 2026

### Fixed

* **Snippet Syntax**: Fixed VSCode warning about confusing snippet variables and placeholders
  - Corrected extra brace in Surge model snippet (`THETA({${3:...})` → `THETA(${3:...})`)
  - Fixed `$TM_FILENAME_BASE` variable syntax in FOCEI snippet
  - Escaped literal `$TABLE` in Xpose TABLEs snippet comment

## [0.2.14] 3 Feb, 2026

### Fixed

* **Security Vulnerabilities**: Resolved all npm audit vulnerabilities across root, client, and server packages
* **ESLint Migration**: Upgraded from ESLint v8 to v9 with flat config format (eslint.config.mjs)

### Updated

* **eslint**: Updated from v8.57.0 to v9.18.0
* **typescript-eslint**: Migrated from separate @typescript-eslint/* packages to unified typescript-eslint v8.38.0
* **brace-expansion**: Patched ReDoS vulnerability
* **diff**: Patched DoS vulnerability in parsePatch/applyPatch
* **js-yaml**: Patched prototype pollution vulnerability

### Changed

* **Package Exclusions**: Improved .vscodeignore to exclude dev files from vsix package

## [0.2.13] 2 Aug, 2025

### Added

* **NONMEM 7.6.0 Support**: Added function highlighting for new NONMEM 7.6.0 functions (PLOG, PEXP, PSQRT, PSIGN, PSIND, PCOSD, PSIND1, PCOSD1)
* **Parameter Bounds Validation**: Comprehensive validation for THETA, OMEGA, and SIGMA parameter bounds syntax
* **Continuation Marker Validation**: Proper validation of `&` continuation characters in NMTRAN code
* **BLOCK Matrix Validation**: Enhanced validation for BLOCK matrix syntax in OMEGA and SIGMA records
* **NMTRAN Parameter Examples**: Added comprehensive parameter syntax reference examples for documentation
* **ERR/EPS Equivalence**: Full support for ERR() as synonym for EPS() parameters throughout the extension

### Enhanced

* **Syntax Highlighting**: Improved highlighting for generated subroutines (ADVAN/TRANS combinations)
* **Performance Optimization**: Enhanced ERR/EPS equivalence processing for better responsiveness
* **FormattingService**: Refactored for improved maintainability and code organization
* **Error Handling**: Added structured error context and enhanced parameter validation utilities
* **Test Coverage**: Comprehensive test suites for NONMEM 7.6.0 features, edge cases, and validation scenarios
* **Code Organization**: Better utility class structure and improved parameter scanning architecture

### Fixed

* **Parameter Bounds**: Resolved OMEGA/SIGMA parameter bounds validation edge cases
* **THETA Hover**: Fixed hover functionality for THETA parameters in complex syntax scenarios
* **TypeScript Compliance**: Resolved strict mode compliance issues for reliable builds
* **Test Reliability**: Fixed various test cases and TypeScript compilation errors
* **Build Management**: Added dist/ directory to .gitignore for cleaner repository state
* **Code Quality**: Fixed unused parameter lint error in ParameterScanner service

### Updated

* **Reserved Variables**: Updated for NONMEM 7.6.0 compatibility
* **Documentation**: Enhanced CLAUDE.md, MAINTENANCE.md, and ARCHITECTURE.md with current practices
* **Development Tools**: Improved pre-release scripts and build configuration

## [0.2.12] 31 Jul, 2025

### Enhanced

* **Architecture Overhaul**: Refactored extension to service-based architecture with proper dependency injection
* **Modern Bundling**: Implemented ESBuild bundling for improved performance and reliable dependency packaging
* **Configuration Management**: Added centralized configuration service with user-configurable debug settings
* **Structured Logging**: Professional logging service with configurable levels and consistent formatting
* **Code Quality**: Comprehensive improvements across maintainability, readability, performance, and testability
* **Modular Design**: Extracted features into dedicated services (folding, language server, parameter parsing)
* **TypeScript Strict Mode**: Enhanced type safety with proper null checking and strict compilation
* **Error Handling**: Improved error handling and recovery throughout the extension

### Fixed

* **TypeScript Compilation**: Fixed strict mode errors in parameter parser for reliable GitHub CI builds
* **Dependency Updates**: Updated TypeScript ESLint packages to v8.x for better code quality enforcement
* **Import Statements**: Converted legacy require() calls to modern ES6 imports for better bundling
* **ESLint Configuration**: Enhanced rules to properly handle unused variables and caught errors

### Updated

* **@typescript-eslint/eslint-plugin**: Updated from v6.21.0 to v8.38.0
* **@typescript-eslint/parser**: Updated from v6.21.0 to v8.38.0  
* **@types/vscode**: Updated from v1.80.0 to v1.102.0
* **@types/node**: Updated from v20.19.9 to v22.10.2

## [0.2.11] 31 Jul, 2025

### Fixed

* Bugfix

## [0.2.10] 31 Jul, 2025

### Fixed

* **Extension Activation**: Fixed TypeScript compilation error that prevented extension from activating properly
* **Go to Definition**: Resolved "Cannot find module 'vscode-languageclient/node'" error that blocked parameter navigation features

## [0.2.9] 30 Jul, 2025

### Enhanced

* **Improved Parameter Navigation**: Enhanced precision and reliability for THETA, ETA, and EPS parameter navigation
* **Precise Value Positioning**: Go to Definition now points to exact parameter values (e.g., initial values in bounded THETA syntax)
* **SAME Constraint Support**: Multiple definition locations for OMEGA SAME constraints showing both declaration and referenced value
* **Performance Optimization**: Added document parsing cache with automatic cleanup for faster repeated operations
* **Type Safety**: Improved TypeScript typing with ParameterInfo, CharacterRange, and ParameterType definitions
* **Code Quality**: Unified ETA/OMEGA and EPS/SIGMA handling, removed unused code, consolidated patterns

## [0.2.8] 29 Jul, 2025

### Added

* **NMTRAN Parameter Navigation**: Go to Definition and Find All References support for THETA, ETA, and EPS parameters
* Right-click on `THETA(1)` → "Go to Definition" jumps to corresponding `$THETA` line
* Right-click on `THETA(1)` or `$THETA` definition → "Find All References" shows all parameter usages
* Support for BLOCK syntax in `$OMEGA` and `$SIGMA` parameter counting

## [0.2.6] 29 Jul, 2025

* Smoother user experience with debounced diagnostics (500ms delay) to prevent excessive validation
* Significantly better performance with large NMTRAN files through LSP incremental sync
* Added TypeScript strict features: Better code quality and fewer runtime errors
* Better memory management with proper cleanup

## [0.2.5] 26 Jul, 2025

* Bugfix auto-release

## [0.2.4] 26 Jul, 2025

* Initiate Claude Code
* Refactor server to service-based architecture
* Add ESLint with TypeScript support and clean up server code

## [0.2.3] 19 Dec, 2024

### Changed

* Refactored validation logic for better maintainability
* Updated snippet placeholder syntax for improved usability
* Enhanced performance through code optimizations
* Improved overall code maintainability

## [0.2.2] 10 Oct, 2024

### Changed

* Optimized performance for better extension responsiveness

## [0.2.1] 10 Oct, 2024

### Added

* Additional code snippets for common NM-TRAN patterns
* Syntax highlighting support for `VARCALC` option in `$TABLE` records

## [0.2.0] 5 Sep, 2024

### Changed

* Syntax-highlighting overhaul
* Added test and demo model code
* Updates to documentation
* New logo

## [0.1.6] 1 Sep, 2024

### Changed

* Minor bug fixes and stability improvements

## [0.1.5] 13 Jul, 2023

### Added

* Expanded collection of code snippets for common NM-TRAN patterns

## [0.1.4] 13 Jul, 2023

### Added

* More snippets

## [0.1.3] 13 Jul, 2023

### Fixed

* Updated underlying framework for better stability and performance

## [0.1.2] 13 Jul, 2023

### Added

* Code folding support for control records

## [0.1.1] 13 Jul, 2023

### Added

* Added snippets demonstration GIF to documentation

### Removed

* Removed changelog section from README to maintain single source of truth

## [0.1.0] 13 Jul, 2023

### Added

* Code snippets for PREDPP subroutines (ADVANs)

## [0.0.4] 13 Jul, 2023

### Added

* Word pattern matching for improved syntax highlighting

### Fixed

* Enhanced exponentiation operator highlighting
* Improved auto-closing behavior for symbol pairs

### Removed

* Removed duplicate extension category from marketplace listing

## [0.0.3] 12 Jul, 2023

### Changed

* Updated README with improved documentation and examples

## [0.0.2] 12 Jul, 2023

### Added

* Added extension logo for marketplace presence

## [0.0.1] 12 Jul, 2023

### Added

* Initial release of the extension
