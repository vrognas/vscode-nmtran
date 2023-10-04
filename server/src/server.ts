import {
  createConnection,
  ProposedFeatures,
  InitializeParams,
  InitializeResult,
  TextDocuments,
  TextDocumentSyncKind,
  Diagnostic,
  DiagnosticSeverity,
} from 'vscode-languageserver/node';

import {
  TextDocument
} from 'vscode-languageserver-textdocument';


// Create a connection for the server.
let connection = createConnection(ProposedFeatures.all);

// Create a manager for open text documents.
let documents: TextDocuments<TextDocument> = new TextDocuments(TextDocument);

// Initialize the server
connection.onInitialize((params: InitializeParams) => {

  const result: InitializeResult = {
    capabilities: {
      textDocumentSync: TextDocumentSyncKind.Incremental
    }
  };

  return result;
});

documents.listen(connection);
connection.listen();

