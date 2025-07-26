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
  TextDocumentSyncKind,
  CompletionItem
} from 'vscode-languageserver/node';

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
const documentService = new DocumentService(connection);
const diagnosticsService = new DiagnosticsService(connection);
const hoverService = new HoverService(connection);
const formattingService = new FormattingService(connection);
const completionService = new CompletionService(connection);

// Settings management
let globalSettings: NMTRANSettings = DEFAULT_SETTINGS;

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
      textDocumentSync: TextDocumentSyncKind.Full,
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

// Provide hover info by matching control records under the cursor
connection.onHover(({ textDocument, position }) => {
  try {
    const doc = documentService.getDocument(textDocument.uri);
    if (!doc) {
      connection.console.error(`❌ Document not found: ${textDocument.uri}`);
      return null;
    }

    return hoverService.provideHover(doc, position);
  } catch (error) {
    connection.console.error(`❌ Error in hover handler: ${error}`);
    return null;
  }
});

// Provide code actions for replacing abbreviated control records
connection.onCodeAction(({ textDocument, range: _range, context }) => {
  // _range prefixed with underscore to indicate intentionally unused
  // (required by LSP interface but we use diagnostics range instead)
  try {
    const doc = documentService.getDocument(textDocument.uri);
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

// Provide document symbols (like an outline) by listing control records
connection.onDocumentSymbol((params) => {
  try {
    const doc = documentService.getDocument(params.textDocument.uri);
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

// Provide code completion
connection.onCompletion(({ textDocument, position }) => {
  try {
    const doc = documentService.getDocument(textDocument.uri);
    if (!doc) {
      connection.console.error(`❌ Document not found for completion: ${textDocument.uri}`);
      return [];
    }

    return completionService.provideCompletions(doc, position);
  } catch (error) {
    connection.console.error(`❌ Error in completion handler: ${error}`);
    return [];
  }
});

// Provide document formatting
connection.onDocumentFormatting(({ textDocument }) => {
  try {
    const doc = documentService.getDocument(textDocument.uri);
    if (!doc) {
      connection.console.error(`❌ Document not found for formatting: ${textDocument.uri}`);
      return [];
    }

    return formattingService.formatDocument(doc);
  } catch (error) {
    connection.console.error(`❌ Error in formatting handler: ${error}`);
    return [];
  }
});

// Provide range formatting
connection.onDocumentRangeFormatting(({ textDocument, range }) => {
  try {
    const doc = documentService.getDocument(textDocument.uri);
    if (!doc) {
      connection.console.error(`❌ Document not found for range formatting: ${textDocument.uri}`);
      return [];
    }

    return formattingService.formatRange(doc, range);
  } catch (error) {
    connection.console.error(`❌ Error in range formatting handler: ${error}`);
    return [];
  }
});

// =================================================================
// DOCUMENT LIFECYCLE
// =================================================================

// Listen for document open events
connection.onDidOpenTextDocument((params) => {
  try {
    const doc = documentService.createDocument(
      params.textDocument.uri,
      'nmtran',
      params.textDocument.version,
      params.textDocument.text
    );
    
    documentService.setDocument(doc);
    diagnosticsService.validateDocument(doc);
  } catch (error) {
    connection.console.error(`❌ Error handling document open: ${error}`);
  }
});

// Listen for document change events
connection.onDidChangeTextDocument((change) => {
  try {
    const doc = documentService.createDocument(
      change.textDocument.uri,
      'nmtran',
      change.textDocument.version,
      change.contentChanges[0].text
    );
    
    documentService.setDocument(doc);
    diagnosticsService.validateDocument(doc);
  } catch (error) {
    connection.console.error(`❌ Error handling document change: ${error}`);
  }
});

// Listen for document close events
connection.onDidCloseTextDocument((params) => {
  try {
    documentService.removeDocument(params.textDocument.uri);
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
  const stats = documentService.getCacheStats();
  connection.console.log(`📊 Final stats: ${stats.documentCount} documents, ${stats.totalSize} chars`);
});

// Start listening for requests
connection.listen();

// Startup confirmation
connection.console.log('✅ NMTRAN Language Server is ready and listening for requests!');
connection.console.log('🏗️  Service-based architecture initialized for better maintainability');