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
import { validControlRecords } from './constants';
import { getHoverInfoForControlRecord } from './hoverInfo';

// ------------ Initial Setup -------------

const connection = createConnection(ProposedFeatures.all);
const documents: TextDocuments<TextDocument> = new TextDocuments(TextDocument);

// ------------ Settings -------------

// Define interface for settings
interface NMTRANSettings {
  maxNumberOfProblems: number;
}

// Global and Document Settings
const defaultSettings: NMTRANSettings = { maxNumberOfProblems: 100 };
let globalSettings: NMTRANSettings = defaultSettings;

// ------------ Server Capabilities -------------

connection.onInitialize((params: InitializeParams): InitializeResult => {
  return {
    capabilities: {
      textDocumentSync: TextDocumentSyncKind.Full,
      hoverProvider: true
    }
  };
});

// ------------ Helper Functions -------------

// Function to get the full form of a control record
function getFullControlRecord(record: string): string {
  for (const validRecord of validControlRecords) {
    if (validRecord.startsWith(record)) {
      return validRecord; // Return the full form if a match is found
    }
  }
  return record; // Return the original string if no match is found
}

// Function to check validity of a control record, if it's an abbreviation, and find the closest valid match
function checkControlRecordValidity(record: string): { isValid: boolean, isAbbreviation: boolean, closestMatch?: string } {
  let closestMatch: string | undefined;
  let isAbbreviation = false;

  for (const validRecord of validControlRecords) {
    if (validRecord === record) {
      return { isValid: true, isAbbreviation: false };
    }

    if (validRecord.startsWith(record)) {
      isAbbreviation = true;
      closestMatch = validRecord;
      return { isValid: true, isAbbreviation, closestMatch };
    }

    // Logic to find the closest match
    if (!closestMatch && validRecord.startsWith(record.substring(0, 3))) {
      closestMatch = validRecord;
    }
  }

  return { isValid: false, isAbbreviation: false, closestMatch };
}

// This function just extracts control records from a text
function findControlRecordsInText(text: string): RegExpExecArray[] {
  const controlRecordPattern = /\$[A-Z]+\b/g;
  const matches: RegExpExecArray[] = [];
  let match: RegExpExecArray | null;

  while ((match = controlRecordPattern.exec(text)) !== null) {
    matches.push(match);
  }

  return matches;
}

// This function will validate individual control records and return a diagnostic
function createDiagnosticForControlRecord(match: RegExpExecArray, textDocument: TextDocument): Diagnostic | null {
  const { isValid, isAbbreviation, closestMatch } = checkControlRecordValidity(match[0]);

  if (isValid) {
    if (isAbbreviation) {
      return {
        severity: DiagnosticSeverity.Information,
        range: {
          start: textDocument.positionAt(match.index),
          end: textDocument.positionAt(match.index + match[0].length)
        },
        message: `Did you mean ${closestMatch}?`,
        source: 'NMTRAN Language Server'
      };
    }
    return null;  // It's a fully spelled out valid control record, so return null
  } else {
    let message = `Invalid control record: ${match[0]}`;
    if (closestMatch) {
      message += `. Did you mean ${closestMatch}?`;
    }
    return {
      severity: DiagnosticSeverity.Error,
      range: {
        start: textDocument.positionAt(match.index),
        end: textDocument.positionAt(match.index + match[0].length)
      },
      message,
      source: 'NMTRAN Language Server'
    };
  }
}

// ------------ Main Functionalities -------------

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
      const fullControlRecord = getFullControlRecord(match[0]);  // Assuming you have this function
      const hoverInfo: MarkupContent = {
        kind: MarkupKind.Markdown,
        value: getHoverInfoForControlRecord(match[0], fullControlRecord)
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

async function validateNMTRANDocument(textDocument: TextDocument): Promise<void> {
  const text = textDocument.getText();
  const controlRecords = findControlRecordsInText(text);
  const diagnostics: Diagnostic[] = [];

  for (const match of controlRecords) {
    const diagnostic = createDiagnosticForControlRecord(match, textDocument);
    if (diagnostic) {
      diagnostics.push(diagnostic);
    }
  }

  connection.sendDiagnostics({ uri: textDocument.uri, diagnostics });
}

// ------------ Event Listeners -------------

// Listen for content changes in text documents
documents.onDidChangeContent(async (change) => {
  // Wait for validation to complete before proceeding
  await validateNMTRANDocument(change.document);
});

// ------------ Start Server -------------

// Make the text document manager listen to the connection for changes
documents.listen(connection);

// Start listening on the connection
connection.listen();
