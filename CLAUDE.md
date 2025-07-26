# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is a Visual Studio Code extension for NMTRAN (NONMEM) language support. It provides syntax highlighting, snippets, diagnostics, hover information, and other language features for NMTRAN control stream files used in pharmacometric modeling.

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
