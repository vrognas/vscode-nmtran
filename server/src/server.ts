import {
  createConnection,
  CodeAction,
  CodeActionKind,
  Diagnostic,
  DiagnosticSeverity,
  DocumentSymbolParams,
  Hover,
  InitializeParams,
  InitializeResult,
  MarkupContent,
  MarkupKind,
  ProposedFeatures,
  SymbolInformation,
  SymbolKind,
  TextDocuments,
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
      hoverProvider: true,
      codeActionProvider: true,
      documentSymbolProvider: true
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

  // Remove lines that start with a semicolon (comments)
  const filteredText = text.split('\n')
                            .filter(line => !line.trim().startsWith(';'))
                            .join('\n');

  while ((match = controlRecordPattern.exec(filteredText)) !== null) {
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
        code: "replace-abbreviation",
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

// Implement Hover logic
connection.onHover(({ textDocument, position }) => {
  try {
    const uri = textDocument.uri;
    const document = documents.get(uri);
    if (!document) {
      connection.console.error(`Document not found: ${uri}`);
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
  } catch (error) {
    connection.console.error(`Error during onHover: ${(error as Error).message}`);
    return null;
  }
});

// Implement CodeAction logic
connection.onCodeAction(({ textDocument, range, context }) => {
  const uri = textDocument.uri;
  const document = documents.get(uri);
  if (!document) {
    return null;
  }

  const codeActions: CodeAction[] = [];
  
  for (const diagnostic of context.diagnostics) {
    // Check if the diagnostic is related to an abbreviation
    if (diagnostic.message.startsWith("Did you mean")) {
      const fullControlRecord = diagnostic.message.replace("Did you mean ", "").replace("?", "");
      
      const replaceAbbreviationAction: CodeAction = {
        title: `Replace with ${fullControlRecord}`,
        kind: CodeActionKind.QuickFix,
        diagnostics: [diagnostic],
        edit: {
          changes: {
            [uri]: [
              {
                range: diagnostic.range,
                newText: fullControlRecord
              }
            ]
          }
        }
      };

      codeActions.push(replaceAbbreviationAction);
    }
  }

  return codeActions;
});

// Implement Document Symbol logic
connection.onDocumentSymbol((params: DocumentSymbolParams) => {
  const uri = params.textDocument.uri;
  const document = documents.get(uri);
  if (!document) {
    return null;
  }

  const text = document.getText();
  const controlRecords = findControlRecordsInText(text);
  const symbols: SymbolInformation[] = [];

  for (const match of controlRecords) {
    const fullControlRecord = getFullControlRecord(match[0]);
    const symbolInfo: SymbolInformation = {
      name: fullControlRecord,
      kind: SymbolKind.Module,
      location: {
        uri: uri,
        range: {
          start: document.positionAt(match.index),
          end: document.positionAt(match.index + match[0].length)
        }
      }
    };

    symbols.push(symbolInfo);
  }

  return symbols;
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
