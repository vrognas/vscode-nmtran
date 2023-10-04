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

documents.onDidChangeContent(async (change) => {
  let textDocument = change.document;

  let text = textDocument.getText();
  let pattern = /\$[A-Z]+\b/g;  // Regular expression to match NMTRAN control records.
  let m: RegExpExecArray | null;

  let diagnostics: Diagnostic[] = [];
  while ((m = pattern.exec(text))) {  
    let diagnostic: Diagnostic = {
      severity: DiagnosticSeverity.Information,  // Using Information severity for now
      range: {
        start: textDocument.positionAt(m.index),
        end: textDocument.positionAt(m.index + m[0].length)
      },
      message: `Found NMTRAN control record: ${m[0]}.`,
      source: 'nmtran-ls'
    };
    diagnostics.push(diagnostic);
  }

  // Send the computed diagnostics to VS Code.
  connection.sendDiagnostics({ uri: textDocument.uri, diagnostics });
});


// Make the text document manager listen on the connection
// for open, change and close text document events
documents.listen(connection);

// Listen on the connection
connection.listen();
