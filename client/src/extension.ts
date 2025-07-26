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
  // =================================================================
  // FEATURE 1: Code Folding
  // =================================================================
  // This allows users to collapse/expand sections between control records
  // Example: Everything between $PK and $DES can be folded
  const foldingProvider = vscode.languages.registerFoldingRangeProvider(
    { language: 'nmtran', scheme: 'file' },
    {
      provideFoldingRanges(document) {
        // Look for lines that start with $ (control records like $PK)
        const controlRecordPattern = /^\$/;
        let currentSectionStart = -1;
        const foldingRanges = [];

        // Scan through each line of the document
        for (let lineNumber = 0; lineNumber < document.lineCount; lineNumber++) {
          const lineText = document.lineAt(lineNumber).text;

          // If this line starts with $, it's a new control record section
          if (controlRecordPattern.test(lineText)) {
            // If we were already tracking a section, close it here
            if (currentSectionStart >= 0) {
              foldingRanges.push(
                new vscode.FoldingRange(currentSectionStart, lineNumber - 1, vscode.FoldingRangeKind.Region)
              );
            }
            // Start tracking this new section
            currentSectionStart = lineNumber;
          }
        }

        // Don't forget the last section (from last $ to end of file)
        if (currentSectionStart > 0) {
          foldingRanges.push(
            new vscode.FoldingRange(currentSectionStart, document.lineCount - 1, vscode.FoldingRangeKind.Region)
          );
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
  
  // Find the compiled server file (server.js built from server.ts)
  const serverModule = context.asAbsolutePath(
    path.join('server', 'out', 'server.js')
  );

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
  client.start();
  
  // Auto-show language server logs when debugging
  if (process.env.VSCODE_DEBUG_MODE === 'true') {
    // Wait a moment for the language server to initialize, then show its output
    setTimeout(() => {
      vscode.commands.executeCommand('workbench.action.output.show.NMTRAN Language Server');
    }, 2000);
  }
  
  // Register our features with VSCode so they get cleaned up properly
  context.subscriptions.push(foldingProvider);
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
