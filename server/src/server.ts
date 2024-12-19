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
  TextDocuments,
  TextDocumentSyncKind
} from 'vscode-languageserver/node';
import { TextDocument } from 'vscode-languageserver-textdocument';
import { validControlRecords } from './constants';
import { getHoverInfoForControlRecord } from './hoverInfo';
import {
  findControlRecordsInText,
  createDiagnosticForControlRecord,
  getFullControlRecord
} from './utils/validateControlRecords';

// ------------ Initial Setup -------------
const connection = createConnection(ProposedFeatures.all);
const documents: TextDocuments<TextDocument> = new TextDocuments(TextDocument);

// ------------ Settings -------------
interface NMTRANSettings {
  maxNumberOfProblems: number;
}
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

// ------------ Hover Logic -------------
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
        const fullControlRecord = getFullControlRecord(match[0]);
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

// ------------ Code Action Logic -------------
connection.onCodeAction(({ textDocument, range, context }) => {
  const uri = textDocument.uri;
  const document = documents.get(uri);
  if (!document) {
    return null;
  }

  const codeActions: CodeAction[] = [];

  for (const diagnostic of context.diagnostics) {
    // Check if the diagnostic is related to an abbreviation suggestion
    if (diagnostic.message.startsWith("Did you mean")) {
      const fullControlRecord = diagnostic.message.replace("Did you mean ", "").replace("?", "");

      const replaceAction: CodeAction = {
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

      codeActions.push(replaceAction);
    }
  }

  return codeActions;
});

// ------------ Document Symbol Logic -------------
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

// ------------ Validation Logic -------------
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
documents.onDidChangeContent(async (change) => {
  await validateNMTRANDocument(change.document);
});

// ------------ Start Server -------------
documents.listen(connection);
connection.listen();
