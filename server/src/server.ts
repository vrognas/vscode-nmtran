import {
  createConnection,
  TextDocuments,
  Diagnostic,
  DiagnosticSeverity,
  ProposedFeatures,
  InitializeParams,
  InitializeResult,
  TextDocumentSyncKind
} from 'vscode-languageserver/node';
import { TextDocument } from 'vscode-languageserver-textdocument';

// Create the connection and the text document manager
let connection = createConnection(ProposedFeatures.all);
let documents: TextDocuments<TextDocument> = new TextDocuments(TextDocument);

// Initialize the server
connection.onInitialize((params: InitializeParams): InitializeResult => {
  return {
    capabilities: {
      textDocumentSync: TextDocumentSyncKind.Full
    }
  };
});

// Validate an NMTRAN document
async function validateNMTRANDocument(textDocument: TextDocument): Promise<void> {
  const text = textDocument.getText();
  const controlRecordPattern = /\$[A-Z]+\b/g;
  let m: RegExpExecArray | null;
  const diagnostics: Diagnostic[] = [];

  while ((m = controlRecordPattern.exec(text)) !== null) {
    const diagnostic: Diagnostic = {
      severity: DiagnosticSeverity.Information,
      range: {
        start: textDocument.positionAt(m.index),
        end: textDocument.positionAt(m.index + m[0].length)
      },
      message: `Control record: ${m[0]}`,
      source: 'nmtran-ls'
    };
    diagnostics.push(diagnostic);
  }

  // Send diagnostics to the client
  connection.sendDiagnostics({ uri: textDocument.uri, diagnostics });
}

// Listen for changes in the document
documents.onDidChangeContent(async (change) => {
  validateNMTRANDocument(change.document);
});

// Make the text document manager listen on the connection
documents.listen(connection);

// Listen on the connection
connection.listen();
