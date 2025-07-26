# Contributing to NMTRAN Extension

Thank you for your interest in contributing to the NMTRAN VSCode extension! This document provides guidelines for contributing to the project.

## Getting Started

### Prerequisites

- Node.js 20.8.0 or higher
- VSCode 1.80.0 or higher
- Git

### Development Setup

1. **Clone the repository**
   ```bash
   git clone https://github.com/vrognas/vscode-nmtran.git
   cd vscode-nmtran
   ```

2. **Install dependencies**
   ```bash
   npm install
   ```

3. **Build the extension**
   ```bash
   npm run compile
   ```

4. **Run tests**
   ```bash
   npm test
   ```

5. **Debug the extension**
   - Open the project in VSCode
   - Press F5 to launch Extension Development Host
   - Test your changes with NMTRAN files in the `test/` folder

## Architecture Overview

The extension follows a client-server architecture using the Language Server Protocol:

- **Client** (`client/src/extension.ts`): VSCode extension entry point, handles folding
- **Server** (`server/src/server.ts`): Language server providing smart features
- **Services** (`server/src/services/`): Modular services for maintainability
  - `DiagnosticsService`: Validation and error reporting
  - `HoverService`: Hover information for control records
  - `DocumentService`: Document lifecycle management
  - `FormattingService`: Code formatting
  - `CompletionService`: Code completion

## How to Contribute

### Reporting Issues

1. Check existing issues to avoid duplicates
2. Use the issue template (if available)
3. Include:
   - VSCode version
   - Extension version
   - Sample NMTRAN file demonstrating the issue
   - Expected vs actual behavior

### Adding New Control Records

To add support for new NMTRAN control records:

1. **Add to constants** (`server/src/constants.ts`):
   ```typescript
   export const allowedControlRecords = [
     // ... existing records
     '$NEWRECORD',
   ];
   ```

2. **Add hover explanation** (`server/src/hoverInfo.ts`):
   ```typescript
   case '$NEWRECORD':
     return 'Description of what this control record does';
   ```

3. **Run tests**:
   ```bash
   npm test
   ```

### Adding New Features

1. **Create a feature branch**
   ```bash
   git checkout -b feature/your-feature-name
   ```

2. **Implement the feature**
   - Follow the service-based architecture
   - Add proper error handling
   - Include logging for debugging

3. **Add tests**
   - Update existing tests if needed
   - Add new test cases for your feature

4. **Update documentation**
   - Update MAINTENANCE.md if needed
   - Add comments to complex code

5. **Submit a pull request**

### Code Style Guidelines

- **TypeScript**: Use strict mode, proper typing
- **Comments**: Explain why, not what
- **Error Handling**: Always use try/catch in service methods
- **Logging**: Use emoji-prefixed console.log for easy identification
- **Naming**: Use descriptive names, follow existing patterns

### Testing

- **Run all tests**: `npm test`
- **Manual testing**: Use F5 debug mode with test files
- **Test edge cases**: Empty files, large files, invalid syntax

### Debugging

1. **View Language Server logs**:
   - Extension Development Host → Cmd+Shift+P → "Developer: Show Logs" → "NMTRAN Language Server"

2. **Set breakpoints**: 
   - In TypeScript files for both client and server
   - Use "Debug Extension + Server" configuration

3. **Common issues**:
   - Check MAINTENANCE.md for troubleshooting guide

## Release Process

1. **Update version** in `package.json`
2. **Update CHANGELOG.md** with changes
3. **Create tag**: `git tag v1.x.x`
4. **Push tag**: `git push origin v1.x.x`
5. **GitHub Actions** automatically publishes to both marketplaces

## Code of Conduct

- Be respectful and inclusive
- Focus on constructive feedback
- Help newcomers learn the codebase
- Follow the existing code patterns

## Questions?

- Check MAINTENANCE.md for development guidelines
- Open an issue for questions about contributing
- Review existing code for patterns and examples

Thank you for contributing to make NMTRAN development better! 🚀