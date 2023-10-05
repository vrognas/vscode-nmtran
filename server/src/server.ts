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

// Create the connection for the server
let connection = createConnection(ProposedFeatures.all);

// Create a manager for text documents
let documents: TextDocuments<TextDocument> = new TextDocuments(TextDocument);

// Initialize the server capabilities
connection.onInitialize((params: InitializeParams): InitializeResult => {
  return {
    capabilities: {
      textDocumentSync: TextDocumentSyncKind.Full
    }
  };
});

// Function to validate an NMTRAN document
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
      source: 'NMTRAN Language Server'
    };
    diagnostics.push(diagnostic);
  }

  // Send the diagnostics to the client
  connection.sendDiagnostics({ uri: textDocument.uri, diagnostics });
}

// Listen for content changes in text documents
documents.onDidChangeContent(async (change) => {
  // Wait for validation to complete before proceeding
  await validateNMTRANDocument(change.document);
});

// Make the text document manager listen to the connection for changes
documents.listen(connection);

// Start listening on the connection
connection.listen();
