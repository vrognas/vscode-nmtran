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
import { allowedControlRecords } from './constants';
import { explainControlRecordHover } from './hoverInfo';
import {
  locateControlRecordsInText,
  generateDiagnosticForControlRecord,
  getFullControlRecordName
} from './utils/validateControlRecords';

// This server uses the Language Server Protocol to provide IntelliSense, diagnostics,
// and other language features for NMTRAN files. It listens on stdin/stdout for messages
// from the client, and responds with code actions, hovers, and diagnostics.
//
// Why:
// By keeping the server logic simple and distinct, we enhance maintainability. The code is structured
// so that logic for hover, diagnostics, and code actions is divided into small functions in separate files.

const connection = createConnection(ProposedFeatures.all);
const documents = new (class extends Map<string, TextDocument> {
  get(uri: string) {
    return super.get(uri);
  }
  setDocument(document: TextDocument) {
    this.set(document.uri, document);
  }
})();

// Default settings for NMTRAN extension
interface NMTRANSettings {
  maxNumberOfProblems: number;
}
const defaultSettings: NMTRANSettings = { maxNumberOfProblems: 100 };
let globalSettings: NMTRANSettings = defaultSettings;

connection.onInitialize((params: InitializeParams): InitializeResult => {
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
connection.onCodeAction(({ textDocument, range, context }) => {
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
  const text = textDocument.getText();
  const matches = locateControlRecordsInText(text);
  const diagnostics: Diagnostic[] = [];

  for (const m of matches) {
    const diagnostic = generateDiagnosticForControlRecord(m, textDocument);
    if (diagnostic) {
      diagnostics.push(diagnostic);
    }
  }

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
