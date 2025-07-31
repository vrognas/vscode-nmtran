/**
 * NMTRAN VSCode Extension - Client Side
 * 
 * This is the "entry point" of the extension that VSCode directly talks to.
 * Main responsibilities:
 * 1. Register language features (like code folding)
 * 2. Start and manage the language server connection
 * 3. Handle extension lifecycle (activate/deactivate)
 */

import * as vscode from 'vscode';
import * as path from 'path';
import {
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
  TransportKind
} from 'vscode-languageclient/node';

// Global variable to hold the language server client connection
let client: LanguageClient;

/**
 * Called when the extension is activated (when NMTRAN files are opened)
 */
export function activate(context: vscode.ExtensionContext) {
  console.log('🚀 NMTRAN Extension: Starting activation...');
  console.log('📍 Extension path:', context.extensionPath);
  console.log('📦 Extension version:', require(context.extensionPath + '/package.json').version);
  
  // =================================================================
  // FEATURE 1: Code Folding
  // =================================================================
  // This allows users to collapse/expand sections between control records
  // Example: Everything between $PK and $DES can be folded
  const foldingProvider = vscode.languages.registerFoldingRangeProvider(
    { language: 'nmtran', scheme: 'file' },
    {
      provideFoldingRanges(document) {
        // Look for lines that start with $ (control records like $THETA, $OMEGA)
        const controlRecordPattern = /^\$/;
        let currentSectionStart = -1;
        const foldingRanges = [];

        // Helper function to check if there's meaningful content between lines
        const hasContentBetween = (startLine: number, endLine: number): boolean => {
          for (let i = startLine + 1; i <= endLine; i++) {
            const line = document.lineAt(i).text.trim();
            // If we find a non-empty line that isn't just whitespace
            if (line.length > 0) {
              return true;
            }
          }
          return false;
        };

        // Scan through each line of the document
        for (let lineNumber = 0; lineNumber < document.lineCount; lineNumber++) {
          const lineText = document.lineAt(lineNumber).text;

          // If this line starts with $, it's a new control record section
          if (controlRecordPattern.test(lineText)) {
            // If we were already tracking a section, close it here
            if (currentSectionStart >= 0) {
              const sectionEnd = lineNumber - 1;
              // Only create folding range if there's actual content (not just blank lines)
              if (sectionEnd > currentSectionStart && hasContentBetween(currentSectionStart, sectionEnd)) {
                foldingRanges.push(
                  new vscode.FoldingRange(currentSectionStart, sectionEnd, vscode.FoldingRangeKind.Region)
                );
              }
            }
            // Start tracking this new section
            currentSectionStart = lineNumber;
          }
        }

        // Don't forget the last section (from last $ to end of file)
        if (currentSectionStart >= 0) {
          const sectionEnd = document.lineCount - 1;
          // Only create folding range if there's actual content (not just blank lines)
          if (sectionEnd > currentSectionStart && hasContentBetween(currentSectionStart, sectionEnd)) {
            foldingRanges.push(
              new vscode.FoldingRange(currentSectionStart, sectionEnd, vscode.FoldingRangeKind.Region)
            );
          }
        }

        return foldingRanges;
      }
    }
  );

  // =================================================================
  // FEATURE 2: Language Server Setup
  // =================================================================
  // This starts the "brain" of the extension that provides smart features
  // like hover hints, error checking, and quick fixes
  
  // Find the bundled server file (server.js built by ESBuild)
  const serverModule = context.asAbsolutePath(
    path.join('dist', 'server.js')
  );
  console.log('🗂️ Server module path:', serverModule);
  console.log('📄 Server file exists:', require('fs').existsSync(serverModule));

  // Debug configuration (for development/troubleshooting)
  const debugOptions = { execArgv: ['--nolazy', '--inspect=6009'] };

  // Server startup options (how to run the language server)
  const serverOptions: ServerOptions = {
    // Normal mode: just run the server
    run: { module: serverModule, transport: TransportKind.ipc },
    // Debug mode: run with debugging enabled on port 6009
    debug: {
      module: serverModule,
      transport: TransportKind.ipc,
      options: debugOptions
    }
  };

  // Client configuration (what files should the server handle)
  const clientOptions: LanguageClientOptions = {
    // Only handle NMTRAN files (*.mod, *.ctl, etc.)
    documentSelector: [{ scheme: 'file', language: 'nmtran' }],
    // Watch for configuration changes (not really used currently)
    synchronize: {
      fileEvents: vscode.workspace.createFileSystemWatcher('**/.clientrc')
    }
  };

  // Create the connection between client and server
  client = new LanguageClient(
    'NMTRANLanguageServer',        // Internal ID
    'NMTRAN Language Server',      // Human-readable name
    serverOptions,
    clientOptions
  );

  // Start the language server
  console.log('🔄 NMTRAN Extension: Starting language server...');
  client.start().then(() => {
    console.log('✅ NMTRAN Extension: Language server started successfully');
  }).catch((error) => {
    console.error('❌ NMTRAN Extension: Failed to start language server:', error);
  });
  
  // Auto-show language server logs when debugging
  if (process.env.VSCODE_DEBUG_MODE === 'true') {
    // Wait a moment for the language server to initialize, then show its output
    setTimeout(() => {
      vscode.commands.executeCommand('workbench.action.output.show.NMTRAN Language Server');
    }, 2000);
  }
  
  // Register our features with VSCode so they get cleaned up properly
  context.subscriptions.push(foldingProvider);
  
  console.log('✨ NMTRAN Extension: Activation completed successfully');
}

/**
 * Called when the extension is deactivated (VSCode closes or extension is disabled)
 * Properly shuts down the language server to free resources
 */
export function deactivate(): Thenable<void> | undefined {
  if (!client) {
    // No server was started, nothing to clean up
    return undefined;
  }
  // Stop the language server gracefully
  return client.stop();
}
