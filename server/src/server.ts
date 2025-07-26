/**
 * NMTRAN Language Server - The "Brain" of the Extension
 * 
 * This is the smart part that provides all the NMTRAN language features:
 * - Hover explanations (when you hover over $THETA, shows what it means)
 * - Error checking (highlights invalid control records in red)
 * - Quick fixes (suggests $ESTIMATION when you type $EST)
 * - Document outline (shows all control records in sidebar)
 * 
 * HOW IT WORKS:
 * 1. Client sends file content when you open/edit NMTRAN files
 * 2. Server scans for control records (words starting with $)
 * 3. Server validates each one against the known list
 * 4. Server sends back hover info, errors, suggestions, etc.
 * 
 * MAINTENANCE NOTES:
 * - Most NMTRAN knowledge is in hoverInfo.ts and constants.ts
 * - Validation logic is in utils/validateControlRecords.ts
 * - This file just orchestrates the features using LSP protocol
 */

import {
  createConnection,
  CodeAction,
  CodeActionKind,
  Diagnostic,
  DocumentSymbolParams,
  Hover,
  InitializeParams,
  InitializeResult,
  MarkupContent,
  MarkupKind,
  ProposedFeatures,
  SymbolInformation,
  SymbolKind,
  TextDocumentSyncKind
} from 'vscode-languageserver/node';
import { TextDocument } from 'vscode-languageserver-textdocument';

// Import our NMTRAN-specific knowledge and utilities
import { allowedControlRecords } from './constants'; // Referenced by validation utilities
import { explainControlRecordHover } from './hoverInfo';
import {
  locateControlRecordsInText,
  generateDiagnosticForControlRecord,
  getFullControlRecordName
} from './utils/validateControlRecords';

// =================================================================
// SERVER SETUP
// =================================================================

// Create connection to communicate with the VSCode client
const connection = createConnection(ProposedFeatures.all);

// Document storage - keeps track of all open NMTRAN files
// This is like a cache so we don't have to re-read files constantly
const documents = new (class extends Map<string, TextDocument> {
  get(uri: string) {
    return super.get(uri);
  }
  setDocument(document: TextDocument) {
    this.set(document.uri, document);
  }
})();

// Extension settings (currently just limits how many problems to report)
interface NMTRANSettings {
  maxNumberOfProblems: number;
}
const defaultSettings: NMTRANSettings = { maxNumberOfProblems: 100 };
let globalSettings: NMTRANSettings = defaultSettings; // Reserved for future configuration features

connection.onInitialize((_params: InitializeParams): InitializeResult => {
  // _params prefixed with underscore to indicate intentionally unused
  // (required by LSP interface but our simple implementation doesn't need it)
  
  // Debug logging
  connection.console.log('🚀 NMTRAN Language Server initializing...');
  connection.console.log('📁 Workspace folder: ' + (_params.workspaceFolders?.[0]?.uri || 'none'));
  
  return {
    capabilities: {
      textDocumentSync: TextDocumentSyncKind.Full,
      hoverProvider: true,
      codeActionProvider: true,
      documentSymbolProvider: true
    }
  };
});

// Provide hover info by matching control records under the cursor
connection.onHover(({ textDocument, position }) => {
  const uri = textDocument.uri;
  const doc = documents.get(uri);
  if (!doc) {
    connection.console.error(`Document not found: ${uri}`);
    return null;
  }

  const text = doc.getText();
  const offset = doc.offsetAt(position);
  const controlRecordRegex = /\$[A-Z]+\b/g;
  let match: RegExpExecArray | null;

  while ((match = controlRecordRegex.exec(text)) !== null) {
    const start = match.index;
    const end = match.index + match[0].length;
    if (start <= offset && offset <= end) {
      const fullControlRecord = getFullControlRecordName(match[0]);
      const hoverInfo: MarkupContent = {
        kind: MarkupKind.Markdown,
        value: explainControlRecordHover(match[0], fullControlRecord)
      };

      return {
        contents: hoverInfo,
        range: {
          start: doc.positionAt(start),
          end: doc.positionAt(end)
        }
      } as Hover;
    }
  }

  return null;
});

// Provide code actions for replacing abbreviated control records
connection.onCodeAction(({ textDocument, range: _range, context }) => {
  // _range prefixed with underscore to indicate intentionally unused
  // (required by LSP interface but we use diagnostics range instead)
  const uri = textDocument.uri;
  const doc = documents.get(uri);
  if (!doc) {
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
            [uri]: [
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
});

// Provide document symbols (like an outline) by listing control records
connection.onDocumentSymbol((params: DocumentSymbolParams) => {
  const uri = params.textDocument.uri;
  const doc = documents.get(uri);
  if (!doc) {
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
        uri: uri,
        range: {
          start: doc.positionAt(match.index),
          end: doc.positionAt(match.index + match[0].length)
        }
      }
    };
    symbols.push(symbolInfo);
  }

  return symbols;
});

// Validate a document by finding and reporting invalid control records
async function validateNMTRANFile(textDocument: TextDocument): Promise<void> {
  const fileName = textDocument.uri.split('/').pop();
  connection.console.log(`🔍 Validating ${fileName}...`);
  
  const text = textDocument.getText();
  const matches = locateControlRecordsInText(text);
  const diagnostics: Diagnostic[] = [];

  connection.console.log(`📄 Found ${matches.length} control records: ${matches.map(m => m[0]).join(', ')}`);

  for (const m of matches) {
    const diagnostic = generateDiagnosticForControlRecord(m, textDocument);
    if (diagnostic) {
      diagnostics.push(diagnostic);
      connection.console.log(`⚠️  Issue with ${m[0]}: ${diagnostic.message}`);
    }
  }

  connection.console.log(`📊 Sending ${diagnostics.length} diagnostics for ${fileName}`);
  connection.sendDiagnostics({ uri: textDocument.uri, diagnostics });
}

// Listen for changes to documents
connection.onDidOpenTextDocument((params) => {
  const doc = TextDocument.create(params.textDocument.uri, 'nmtran', params.textDocument.version, params.textDocument.text);
  documents.setDocument(doc);
  validateNMTRANFile(doc);
});

connection.onDidChangeTextDocument((change) => {
  const doc = TextDocument.create(
    change.textDocument.uri,
    'nmtran',
    change.textDocument.version,
    change.contentChanges[0].text
  );
  documents.setDocument(doc);
  validateNMTRANFile(doc);
});

connection.listen();

// Startup confirmation
connection.console.log('✅ NMTRAN Language Server is ready and listening for requests!');
