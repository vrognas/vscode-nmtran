import {
  createConnection,
  TextDocuments,
  Diagnostic,
  DiagnosticSeverity,
  Hover,
  MarkupContent,
  MarkupKind,
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

// Define interface for settings
interface NMTRANSettings {
  maxNumberOfProblems: number;
}

// Global and Document Settings
const defaultSettings: NMTRANSettings = { maxNumberOfProblems: 100 };
let globalSettings: NMTRANSettings = defaultSettings;

// Initialize the server capabilities
connection.onInitialize((params: InitializeParams): InitializeResult => {
  return {
    capabilities: {
      textDocumentSync: TextDocumentSyncKind.Full,
      // Indicate that the server provides hover support
      hoverProvider: true
    }
  };
});

// Implement hover logic
connection.onHover(({ textDocument, position }) => {
  const uri = textDocument.uri;
  const document = documents.get(uri);
  if (!document) {
    return null;
  }

  const text = document.getText();
  const offset = document.offsetAt(position);
  const controlRecordPattern = /\$[A-Z]+\b/g;
  let match: RegExpExecArray | null;

  while ((match = controlRecordPattern.exec(text)) !== null) {
    const start = match.index;
    const end = match.index + match[0].length;
    if (start <= offset && offset <= end) {
      const hoverInfo: MarkupContent = {
        kind: MarkupKind.Markdown,
        value: getHoverInfoForControlRecord(match[0])
      };

      return {
        contents: hoverInfo,
        range: {
          start: document.positionAt(start),
          end: document.positionAt(end)
        }
      } as Hover;
    }
  }

  return null;
});

// Function to return hover information for a given control record
function getHoverInfoForControlRecord(controlRecord: string): string {
  switch (controlRecord) {
    case '$PROBLEM':
      return 'This is a PROBLEM control record. It indicates ...';
    case '$ESTIMATION':
      return 'This is an ESTIMATION control record. It specifies ...';
    default:
      return `This is a ${controlRecord} control record.`;
  }
}

// Function to validate an NMTRAN document
// List of valid control records. Add more as you need
const validControlRecords = [
  '$ABBREVIATED',
  '$AES',
  '$AESINITIAL',
  '$ANNEAL',
  '$BIND',
  '$CHAIN',
  '$CONTR',
  '$COVARIANCE',
  '$DATA',
  '$DEFAULT',
  '$DES',
  '$DESIGN',
  '$ERROR',
  '$ESTIMATION',
  '$ETAS',
  '$PHIS',
  '$FORMAT',
  '$INDEX',
  '$INDXS',
  '$INFN',
  '$INPUT',
  '$LEVEL',
  '$MIX',
  '$MODEL',
  '$MSFI',
  '$NONPARAMETRIC',
  '$OLKJDF',
  '$OMEGAP',
  '$OMEGAPD',
  '$OMIT',
  '$OVARF',
  '$PK',
  '$PRED',
  '$PRIOR',
  '$PROBLEM',
  '$RCOV',
  '$RCOVI',
  '$SCATTERPLOT',
  '$SIGMA',
  '$SIGMAP',
  '$SIGMAPD',
  '$SIMULATION',
  '$SIZES',
  '$SLKJDF',
  '$SUBROUTINES',
  '$SUPER',
  '$SVARF',
  '$TABLE',
  '$THETA',
  '$THETAI',
  '$THETAP',
  '$THETAPV',
  '$THETAR',
  '$TOL',
  '$TTDF',
  '$WARNINGS'
];

// Function to validate an NMTRAN document
async function validateNMTRANDocument(textDocument: TextDocument): Promise<void> {
  const text = textDocument.getText();
  const controlRecordPattern = /\$[A-Z]+\b/g;
  let m: RegExpExecArray | null;
  const diagnostics: Diagnostic[] = [];
  let problems = 0;

  while ((m = controlRecordPattern.exec(text)) !== null && problems < globalSettings.maxNumberOfProblems) {
    problems++;
    
    // Check if it's a valid control record
    const isValid = isValidControlRecord(m[0]);
    
    // If the control record is invalid, send a diagnostic message
    if (!isValid) {
      const diagnostic: Diagnostic = {
        severity: DiagnosticSeverity.Error,  // Setting severity to Error
        range: {
          start: textDocument.positionAt(m.index),
          end: textDocument.positionAt(m.index + m[0].length)
        },
        message: `Invalid control record: ${m[0]}`,
        source: 'NMTRAN Language Server'
      };
      diagnostics.push(diagnostic);
    }
  }

  // Send the diagnostics to the client
  connection.sendDiagnostics({ uri: textDocument.uri, diagnostics });
}

// Function to check if a string is a valid control record
function isValidControlRecord(record: string): boolean {
  for (const validRecord of validControlRecords) {
    if (validRecord.startsWith(record)) {
      return true;
    }
  }
  return false;
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
