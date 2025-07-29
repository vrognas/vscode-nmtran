/**
 * NMTRAN Language Server - Modernized Architecture
 * 
 * This is the smart part that provides all the NMTRAN language features:
 * - Hover explanations (when you hover over $THETA, shows what it means)
 * - Error checking (highlights invalid control records in red)
 * - Quick fixes (suggests $ESTIMATION when you type $EST)
 * - Document outline (shows all control records in sidebar)
 * 
 * ARCHITECTURE:
 * - Service-based architecture for better maintainability
 * - Proper error handling throughout
 * - Centralized document management
 * - Separated concerns (hover, diagnostics, symbols)
 */

import {
  createConnection,
  CodeAction,
  CodeActionKind,
  InitializeParams,
  InitializeResult,
  ProposedFeatures,
  SymbolInformation,
  SymbolKind,
  TextDocumentSyncKind
} from 'vscode-languageserver/node';

import { TextDocument } from 'vscode-languageserver-textdocument';

// Import our services
import { DocumentService } from './services/documentService';
import { DiagnosticsService } from './services/diagnosticsService';
import { HoverService } from './services/hoverService';
import { FormattingService } from './services/formattingService';
import { CompletionService } from './services/completionService';

// Import types and utilities
import { DEFAULT_SETTINGS, NMTRANSettings } from './types';
import {
  locateControlRecordsInText,
  getFullControlRecordName
} from './utils/validateControlRecords';

// =================================================================
// SERVER SETUP
// =================================================================

const connection = createConnection(ProposedFeatures.all);

// Initialize services
const services = {
  document: new DocumentService(connection),
  diagnostics: new DiagnosticsService(connection),
  hover: new HoverService(connection),
  formatting: new FormattingService(connection),
  completion: new CompletionService(connection)
};

// Settings management - currently only used for maxNumberOfProblems in diagnostics
const documentSettings: Map<string, Thenable<NMTRANSettings>> = new Map();

// Debounced diagnostics to prevent excessive validation during rapid text changes
const diagnosticsTimeouts: Map<string, NodeJS.Timeout> = new Map();
const DIAGNOSTICS_DEBOUNCE_MS = 500;

/**
 * Schedules debounced diagnostics validation for a document
 * Prevents excessive validation during rapid text changes
 */
function scheduleDebouncedDiagnostics(uri: string, doc: TextDocument): void {
  // Clear existing timeout for this document
  const existingTimeout = diagnosticsTimeouts.get(uri);
  if (existingTimeout) {
    clearTimeout(existingTimeout);
  }

  // Schedule new validation after debounce period
  const timeout = setTimeout(() => {
    try {
      services.diagnostics.validateDocument(doc);
      diagnosticsTimeouts.delete(uri);
    } catch (error) {
      connection.console.error(`❌ Error in debounced diagnostics: ${error}`);
    }
  }, DIAGNOSTICS_DEBOUNCE_MS);

  diagnosticsTimeouts.set(uri, timeout);
}

function getDocumentSettings(resource: string): Thenable<NMTRANSettings> {
  if (!documentSettings.has(resource)) {
    const result = Promise.all([
      connection.workspace.getConfiguration({
        scopeUri: resource,
        section: 'nmtranServer'
      }),
      connection.workspace.getConfiguration({
        scopeUri: resource,
        section: 'nmtran'
      })
    ]).then(([serverConfig, nmtranConfig]) => {
      return {
        maxNumberOfProblems: serverConfig?.maxNumberOfProblems ?? DEFAULT_SETTINGS.maxNumberOfProblems,
        formatting: {
          indentSize: Math.max(2, Math.min(4, nmtranConfig?.formatting?.indentSize ?? DEFAULT_SETTINGS.formatting?.indentSize ?? 2))
        }
      };
    });
    documentSettings.set(resource, result);
    return result;
  }
  return documentSettings.get(resource)!;
}


// =================================================================
// SERVER CAPABILITIES
// =================================================================

connection.onInitialize((_params: InitializeParams): InitializeResult => {
  // _params prefixed with underscore to indicate intentionally unused
  // (required by LSP interface but our simple implementation doesn't need it)
  
  // Debug logging
  connection.console.log('🚀 NMTRAN Language Server initializing...');
  connection.console.log('📁 Workspace folder: ' + (_params.workspaceFolders?.[0]?.uri || 'none'));
  connection.console.log('🔧 Using service-based architecture for better maintainability');
  
  return {
    capabilities: {
      textDocumentSync: {
        openClose: true,
        change: TextDocumentSyncKind.Incremental
      },
      hoverProvider: true,
      codeActionProvider: true,
      documentSymbolProvider: true,
      completionProvider: {
        triggerCharacters: ['$', ' ']
      },
      documentFormattingProvider: true,
      documentRangeFormattingProvider: true
    }
  };
});

// =================================================================
// LANGUAGE FEATURES
// =================================================================

/**
 * Provides hover information for NMTRAN control records
 * Shows explanations when users hover over control records like $THETA, $OMEGA, etc.
 */
connection.onHover(({ textDocument, position }) => {
  try {
    const doc = services.document.getDocument(textDocument.uri);
    if (!doc) {
      connection.console.error(`❌ Document not found: ${textDocument.uri}`);
      return null;
    }

    return services.hover.provideHover(doc, position);
  } catch (error) {
    connection.console.error(`❌ Error in hover handler: ${error}`);
    return null;
  }
});

/**
 * Provides code actions (quick fixes) for NMTRAN files
 * Suggests replacing abbreviated control records with full names (e.g., $EST → $ESTIMATION)
 */
connection.onCodeAction(({ textDocument, range: _range, context }) => {
  // _range prefixed with underscore to indicate intentionally unused
  // (required by LSP interface but we use diagnostics range instead)
  try {
    const doc = services.document.getDocument(textDocument.uri);
    if (!doc) {
      connection.console.error(`❌ Document not found for code actions: ${textDocument.uri}`);
      return null;
    }

    const codeActions: CodeAction[] = [];

    for (const diag of context.diagnostics) {
      if (diag.message.startsWith("Did you mean")) {
        const fullRecord = diag.message.replace("Did you mean ", "").replace("?", "");

        const replaceAction: CodeAction = {
          title: `Replace with ${fullRecord}`,
          kind: CodeActionKind.QuickFix,
          diagnostics: [diag],
          edit: {
            changes: {
              [textDocument.uri]: [
                {
                  range: diag.range,
                  newText: fullRecord
                }
              ]
            }
          }
        };

        codeActions.push(replaceAction);
      }
    }

    return codeActions;
  } catch (error) {
    connection.console.error(`❌ Error in code action handler: ${error}`);
    return [];
  }
});

/**
 * Provides document outline (symbols) for NMTRAN files
 * Lists all control records in the sidebar for easy navigation
 */
connection.onDocumentSymbol((params) => {
  try {
    const doc = services.document.getDocument(params.textDocument.uri);
    if (!doc) {
      connection.console.error(`❌ Document not found for symbols: ${params.textDocument.uri}`);
      return null;
    }

    const text = doc.getText();
    const controlRecords = locateControlRecordsInText(text);
    const symbols: SymbolInformation[] = [];

    for (const match of controlRecords) {
      const fullControlRecord = getFullControlRecordName(match[0]);
      const symbolInfo: SymbolInformation = {
        name: fullControlRecord,
        kind: SymbolKind.Module,
        location: {
          uri: params.textDocument.uri,
          range: {
            start: doc.positionAt(match.index),
            end: doc.positionAt(match.index + match[0].length)
          }
        }
      };
      symbols.push(symbolInfo);
    }

    return symbols;
  } catch (error) {
    connection.console.error(`❌ Error in document symbol handler: ${error}`);
    return [];
  }
});

/**
 * Provides code completion suggestions for NMTRAN files
 * Suggests control records and common parameter names
 */
connection.onCompletion(({ textDocument, position }) => {
  try {
    const doc = services.document.getDocument(textDocument.uri);
    if (!doc) {
      connection.console.error(`❌ Document not found for completion: ${textDocument.uri}`);
      return [];
    }

    return services.completion.provideCompletions(doc, position);
  } catch (error) {
    connection.console.error(`❌ Error in completion handler: ${error}`);
    return [];
  }
});

/**
 * Provides document formatting for NMTRAN files
 * Formats control records and ensures proper indentation
 */
connection.onDocumentFormatting(async ({ textDocument }, token) => {
  try {
    // Check for cancellation
    if (token.isCancellationRequested) {
      return [];
    }

    const doc = services.document.getDocument(textDocument.uri);
    if (!doc) {
      connection.console.error(`❌ Document not found for formatting: ${textDocument.uri}`);
      return [];
    }

    // Always get fresh settings for formatting to pick up changes
    documentSettings.delete(textDocument.uri);
    const settings = await getDocumentSettings(textDocument.uri);
    
    // Check for cancellation after async operation
    if (token.isCancellationRequested) {
      return [];
    }
    
    const indentSize = settings.formatting?.indentSize || DEFAULT_SETTINGS.formatting?.indentSize || 2;
    connection.console.log(`🎨 Format document request for: ${textDocument.uri}`);
    connection.console.log(`⚙️ Using ${indentSize}-space indentation`);
    
    return services.formatting.formatDocument(doc, indentSize);
  } catch (error) {
    connection.console.error(`❌ Error in formatting handler: ${error}`);
    return [];
  }
});

/**
 * Provides range formatting for NMTRAN files
 * Formats only the selected range of text
 */
connection.onDocumentRangeFormatting(async ({ textDocument, range }, token) => {
  try {
    // Check for cancellation
    if (token.isCancellationRequested) {
      return [];
    }

    const doc = services.document.getDocument(textDocument.uri);
    if (!doc) {
      connection.console.error(`❌ Document not found for range formatting: ${textDocument.uri}`);
      return [];
    }

    // Always get fresh settings for formatting to pick up changes
    documentSettings.delete(textDocument.uri);
    const settings = await getDocumentSettings(textDocument.uri);
    
    // Check for cancellation after async operation
    if (token.isCancellationRequested) {
      return [];
    }
    
    const indentSize = settings.formatting?.indentSize || DEFAULT_SETTINGS.formatting?.indentSize || 2;
    connection.console.log(`🎨 Format range request for: ${textDocument.uri}`);
    connection.console.log(`⚙️ Using ${indentSize}-space indentation`);
    
    return services.formatting.formatRange(doc, range, indentSize);
  } catch (error) {
    connection.console.error(`❌ Error in range formatting handler: ${error}`);
    return [];
  }
});

/**
 * Handles configuration changes
 * Clears settings cache to ensure fresh configuration is loaded
 */
connection.onDidChangeConfiguration((_change) => {
  // Clear document settings cache when configuration changes
  documentSettings.clear();
  connection.console.log('🔄 Configuration changed, cleared settings cache');
});

// =================================================================
// DOCUMENT LIFECYCLE
// =================================================================

// Listen for document open events
connection.onDidOpenTextDocument((params) => {
  try {
    const doc = services.document.createDocument(
      params.textDocument.uri,
      'nmtran',
      params.textDocument.version,
      params.textDocument.text
    );
    
    services.document.setDocument(doc);
    services.diagnostics.validateDocument(doc);
  } catch (error) {
    connection.console.error(`❌ Error handling document open: ${error}`);
  }
});

// Listen for document change events (incremental)
connection.onDidChangeTextDocument((change) => {
  try {
    // Get the current document from cache
    let doc = services.document.getDocument(change.textDocument.uri);
    
    if (!doc) {
      connection.console.warn(`⚠️  Document not found in cache: ${change.textDocument.uri}`);
      return;
    }
    
    // Apply incremental changes
    for (const contentChange of change.contentChanges) {
      if ('range' in contentChange) {
        // Incremental change
        doc = TextDocument.update(doc, [contentChange], change.textDocument.version);
      } else {
        // Full document change (fallback)
        doc = services.document.createDocument(
          change.textDocument.uri,
          'nmtran',
          change.textDocument.version,
          contentChange.text
        );
      }
    }
    
    services.document.setDocument(doc);
    scheduleDebouncedDiagnostics(change.textDocument.uri, doc);
  } catch (error) {
    connection.console.error(`❌ Error handling document change: ${error}`);
  }
});

// Listen for document close events
connection.onDidCloseTextDocument((params) => {
  try {
    services.document.removeDocument(params.textDocument.uri);
    
    // Clear any pending diagnostics timeout
    const timeout = diagnosticsTimeouts.get(params.textDocument.uri);
    if (timeout) {
      clearTimeout(timeout);
      diagnosticsTimeouts.delete(params.textDocument.uri);
    }
    
    // Clear diagnostics for closed document
    connection.sendDiagnostics({ 
      uri: params.textDocument.uri, 
      diagnostics: [] 
    });
  } catch (error) {
    connection.console.error(`❌ Error handling document close: ${error}`);
  }
});

// =================================================================
// SERVER LIFECYCLE
// =================================================================

// Handle shutdown gracefully
connection.onShutdown(() => {
  connection.console.log('🛑 NMTRAN Language Server shutting down...');
  const stats = services.document.getCacheStats();
  connection.console.log(`📊 Final stats: ${stats.documentCount} documents, ${stats.totalSize} chars`);
});

// Start listening for requests
connection.listen();

// Startup confirmation
connection.console.log('✅ NMTRAN Language Server is ready and listening for requests!');
connection.console.log('🏗️  Service-based architecture initialized for better maintainability');