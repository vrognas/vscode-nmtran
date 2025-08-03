# Contributing to NMTRAN Extension

Thank you for your interest in contributing to the NMTRAN VSCode extension! 🎉

We really appreciate that you'd like to contribute to this project. Whether you're fixing bugs, adding features, improving documentation, or sharing feedback, your contributions help make NMTRAN development better for the entire pharmacometrics community.

This document provides guidelines for contributing to the project effectively.

## Getting Started

### Prerequisites

Before you begin, ensure you have the following installed:

- **Node.js**: v20.8.0 or higher ([Download here](https://nodejs.org/))
- **VSCode**: v1.102.0 or higher ([Download here](https://code.visualstudio.com/))
- **Git**: For version control ([Download here](https://git-scm.com/))

Optional but recommended:
- **VSCode Extensions**: Install the TypeScript and ESLint extensions for better development experience
- **NONMEM Knowledge**: Familiarity with NMTRAN syntax and pharmacometric modeling concepts

### Development Setup

1. **Fork and Clone**
   ```bash
   # Fork the repository on GitHub first, then:
   git clone https://github.com/YOUR_USERNAME/vscode-nmtran.git
   cd vscode-nmtran
   ```

2. **Install Dependencies**
   ```bash
   npm install
   ```
   This installs dependencies for both client and server components.

3. **Build the Extension**
   ```bash
   npm run build
   ```
   **Important**: Always use `npm run build` (not `npm run compile`) as the extension loads from `dist/`, not TypeScript output directories.

4. **Run Tests**
   ```bash
   npm test                    # Run all tests
   npm run test:server        # Run server tests only
   cd server && npm run test:watch  # Watch mode for server tests
   ```

5. **Launch Development Environment**
   - Open the project in VSCode
   - Press **F5** to launch the Extension Development Host
   - Open NMTRAN files from the `test/` folder to test features
   - Check "Developer: Show Logs" → "NMTRAN Language Server" for debugging

6. **Development Workflow**
   ```bash
   npm run bundle:watch       # Auto-rebuild on file changes
   npm run compile:watch      # TypeScript compilation with watching
   npm run validate          # Run linting, compilation, and tests
   ```

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

### Testing Strategy

We use a comprehensive testing approach to ensure code quality:

#### Unit Tests

```bash
npm run test:server        # Run Jest unit tests
cd server && npm run test:watch  # Watch mode during development
```

Our tests cover:
- Parameter parsing and validation
- NMTRAN syntax validation
- Control record recognition
- BLOCK matrix handling
- SAME keyword resolution

#### Integration Tests

```bash
npm test                   # Runs both unit and integration tests
```

#### Manual Testing

1. **Extension Development Host**
   - Press **F5** to launch test environment
   - Open files from `test/` directory
   - Test specific features with real NMTRAN files

2. **Test Coverage Areas**
   - **Basic functionality**: Syntax highlighting, code folding
   - **Language features**: Hover, go-to-definition, diagnostics
   - **Edge cases**: Empty files, large files (>1000 lines), invalid syntax
   - **NONMEM versions**: Test with different NONMEM syntax versions
   - **Performance**: Test with large models and rapid typing

3. **Sample Test Files**
   Use files in `test/` directory for manual testing:
   - `demo.mod` - Basic PopPK model
   - `maximal.mod` - Complex model with many features
   - `test-data.mod` - Edge cases and validation scenarios

#### Writing New Tests

When adding features, include tests that cover:
- **Happy path**: Normal usage scenarios
- **Edge cases**: Boundary conditions and error states
- **Regression**: Ensure existing functionality isn't broken
- **Performance**: Test with realistic file sizes

Example test structure:
```typescript
describe('New Feature', () => {
  test('should handle normal case', () => {
    // Test implementation
  });
  
  test('should handle edge case', () => {
    // Test edge cases
  });
});
```

### Debugging

1. **View Language Server logs**:
   - Extension Development Host → Cmd+Shift+P → "Developer: Show Logs" → "NMTRAN Language Server"

2. **Set breakpoints**: 
   - In TypeScript files for both client and server
   - Use "Debug Extension + Server" configuration

3. **Common issues**:
   - Check MAINTENANCE.md for troubleshooting guide

## Release Process

For maintainers releasing new versions:

1. **Prepare Release**
   ```bash
   npm run validate           # Ensure all tests pass
   npm run build             # Verify production build
   ```

2. **Update Version and Documentation**
   - Update version in `package.json`
   - Update `CHANGELOG.md` with new features, fixes, and breaking changes
   - Commit changes: `git commit -m "Bump version to X.Y.Z"`

3. **Create and Push Tag**
   ```bash
   git tag vX.Y.Z
   git push origin vX.Y.Z
   ```

4. **Automated Release**
   - GitHub Actions automatically builds and publishes to VSCode Marketplace
   - Monitor the release workflow for any issues

## Getting Help

If you need assistance:

1. **Check Documentation**
   - [ARCHITECTURE.md](ARCHITECTURE.md) - Technical architecture details
   - [MAINTENANCE.md](MAINTENANCE.md) - Debugging and troubleshooting
   - [CLAUDE.md](CLAUDE.md) - Development guidelines and patterns

2. **Ask Questions**
   - [GitHub Discussions](https://github.com/vrognas/vscode-nmtran/discussions) - General questions
   - [GitHub Issues](https://github.com/vrognas/vscode-nmtran/issues) - Bug reports and feature requests

3. **Review Existing Code**
   - Look at existing services in `server/src/services/` for patterns
   - Check test files for examples of testing approaches
   - Review recent commits for coding style examples

## Code of Conduct

We are committed to providing a welcoming and inclusive environment for all contributors:

- **Be respectful and inclusive** - Welcome people of all backgrounds and experience levels
- **Focus on constructive feedback** - Provide helpful suggestions and critique ideas, not people
- **Help newcomers learn** - Share knowledge and guide new contributors through the codebase
- **Follow existing patterns** - Maintain consistency with established code conventions
- **Stay professional** - Keep discussions focused on technical matters and project improvement

## Recognition

Contributors who help improve this extension will be:
- Acknowledged in release notes for significant contributions
- Listed as contributors on the GitHub repository
- Recognized for their expertise in the pharmacometrics community

## Thank You! 🎉

Thank you for taking the time to contribute to the NMTRAN extension! Your efforts help make pharmacometric modeling more accessible and efficient for researchers worldwide.

Whether you're fixing a typo, implementing a major feature, or helping other users, every contribution matters and is greatly appreciated.

**Happy coding, and may your NONMEM models converge quickly!** 🧬💊