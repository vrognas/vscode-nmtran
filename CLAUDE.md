# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is a Visual Studio Code extension for NMTRAN (NONMEM) language support. It provides syntax highlighting, snippets, diagnostics, hover information, and other language features for NMTRAN control stream files used in pharmacometric modeling.

### About NMTRAN

NMTRAN (NM-TRAN stands for NONMEM Translator) is a Fortran-flavor language that is custom built to work with a program called NONMEM. It is a preprocessor to NONMEM which translates user-inputs into:

1. A NONMEM data set
2. A NONMEM control stream  
3. Various subroutines which must be included in a NONMEM load module

NMTRAN is a separate computer program written in FORTRAN 90/95, and one precedes a NONMEM run by first running it. The language combines Fortran syntax with specialized control records and pharmacometric modeling constructs for population pharmacokinetic and pharmacodynamic analysis.

NONMEM is a computer program written in FORTRAN to analyze population type pharmacokinetic (PopPK) data using a nonlinear mixed effects model. The control records (eg `$PK`) contain the instructions to NONMEM. The sequence of control records is called the "control stream" (this is essentially the model file, ".mod").

In the NMTRAN language:
- **Fixed effects** are denoted by `THETA(1)`, `THETA(2)`, etc. The numbering sequence is strict - there cannot be a gap in the numbering (eg `THETA(1)`, `THETA(3)` without a `THETA(2)` in the control stream).
- **Random interindividual (IIV) effects** are denoted by `ETA(1)`, `ETA(2)`, etc. The numbering sequence is strict with no gaps allowed. Their variance-covariance matrix is denoted `OMEGA` in the NONMEM printout.
- **Residual variabilities** are denoted by `EPS(1)`, `EPS(2)`, etc. The numbering sequence is strict with no gaps allowed. Their variance-covariance matrix is denoted by `SIGMA` in the NONMEM printout.

## Architecture

The extension follows a client-server architecture using the Language Server Protocol (LSP):

### Client (`client/src/extension.ts`)
- VSCode extension entry point that activates when NMTRAN files are opened
- Registers a folding range provider for control records (lines starting with `$`)
- Starts and manages the language server connection
- Handles extension lifecycle (activate/deactivate)

### Server (`server/src/server.ts`)
- Language server that provides IntelliSense features via LSP
- Implements hover, diagnostics, code actions, and document symbols
- Validates NMTRAN control records against known valid records
- Suggests corrections for abbreviated or invalid control records

### Key Components
- `server/src/hoverInfo.ts`: Contains detailed explanations for each NMTRAN control record
- `server/src/constants.ts`: Defines allowed control records and their abbreviations
- `server/src/utils/validateControlRecords.ts`: Validation logic for control records
- `syntaxes/nmtran.tmLanguage.json`: TextMate grammar for syntax highlighting
- `snippets/snippets.json`: Code snippets for common NMTRAN patterns

## Development Commands

### Build and Compile
```bash
npm run compile          # Compile TypeScript for both client and server
npm run watch           # Watch mode for development
```

### Linting
```bash
npm run lint            # Run ESLint on client and server source files
```

### Package Management
```bash
npm run postinstall     # Install dependencies for both client and server
```

### Extension Packaging
```bash
npm run vscode:prepublish   # Prepare extension for publishing (runs compile)
```

## File Structure

- `client/`: VSCode extension client code
- `server/`: Language server implementation
- `syntaxes/`: TextMate grammar files for syntax highlighting
- `snippets/`: Code completion snippets
- `test/`: Sample NMTRAN files for testing
- `images/`: Extension icons and demo images

## Language Features

The extension supports these NMTRAN file extensions: `.mod`, `.ctl`, `.lst`, `.modt`, `.phi`, `.coi`, `.cor`, `.cov`, `.cnv`, `.scm`, `.ext`

### Key Features
- **Syntax Highlighting**: TextMate grammar-based tokenization
- **Folding**: Automatic folding by control records (lines starting with `$`)
- **Hover Info**: Detailed explanations for control records
- **Diagnostics**: Validation of control record names
- **Code Actions**: Quick fixes for abbreviated control records
- **Document Symbols**: Outline view of control records
- **Snippets**: Pre-built templates for common NMTRAN patterns

## Development Notes

- The project uses TypeScript with strict mode enabled
- Client and server are separate npm packages with their own dependencies
- The language server validates control records against a predefined list in `constants.ts`
- Hover information is maintained in `hoverInfo.ts` with detailed explanations for each control record
- The extension uses VSCode's Language Server Protocol for optimal performance and separation of concerns
