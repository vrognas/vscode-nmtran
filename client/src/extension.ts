// Import the required modules
import * as vscode from 'vscode';
import * as path from 'path';
import {
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
  TransportKind
} from 'vscode-languageclient/node';

// Declare the LanguageClient at the top level
let client: LanguageClient;

// This method is called when your extension is activated
export function activate(context: vscode.ExtensionContext) {
  // Register a Folding Range Provider for the 'nmtran' language
  let disposable = vscode.languages.registerFoldingRangeProvider(
    { language: 'nmtran', scheme: 'file' },
    {
      provideFoldingRanges(document, context, token) {
        const re = /^\$/;  // Regex to detect start of a region
        let sectionStart = 0;
        let foldingRanges = [];

        for (let i = 0; i < document.lineCount; i++) {
          if (re.test(document.lineAt(i).text)) {
            if (sectionStart >= 0) {
              const newFoldingRange = new vscode.FoldingRange(
                sectionStart,
                i - 1,
                vscode.FoldingRangeKind.Region
              );
              foldingRanges.push(newFoldingRange);
            }
            sectionStart = i;
          }
        }

        if (sectionStart > 0) {
          const newFoldingRange = new vscode.FoldingRange(
            sectionStart,
            document.lineCount - 1,
            vscode.FoldingRangeKind.Region
          );
          foldingRanges.push(newFoldingRange);
        }

        return foldingRanges;
      }
    }
  );

  // Language Client Setup
  let serverModule = context.asAbsolutePath(
    path.join('server', 'out', 'server.js')
  );

  let debugOptions = { execArgv: ['--nolazy', '--inspect=6009'] };

  let serverOptions: ServerOptions = {
    run: { module: serverModule, transport: TransportKind.ipc },
    debug: {
      module: serverModule,
      transport: TransportKind.ipc,
      options: debugOptions
    }
  };

  let clientOptions: LanguageClientOptions = {
    documentSelector: [{ scheme: 'file', language: 'nmtran' }],
    synchronize: {
      fileEvents: vscode.workspace.createFileSystemWatcher('**/.clientrc')
    }
  };

  // Initialize the Language Client
  client = new LanguageClient(
    'NMTRANLanguageServer',
    'NMTRAN Language Server',
    serverOptions,
    clientOptions
  );

  client.start();

  // Register the folding range provider to the extension's subscriptions
  context.subscriptions.push(disposable);
}

// This method is called when your extension is deactivated
export function deactivate(): Thenable<void> | undefined {
  if (!client) {
    return undefined;
  }
  return client.stop();
}
