import * as vscode from 'vscode';
import * as path from 'path';
import {
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
  TransportKind
} from 'vscode-languageclient/node';

let client: LanguageClient;

export function activate(context: vscode.ExtensionContext) {
  // Folding range provider to group lines between $CONTROL_RECORDS
  // Why: This lets users quickly fold sections by control record.
  const foldingProvider = vscode.languages.registerFoldingRangeProvider(
    { language: 'nmtran', scheme: 'file' },
    {
      provideFoldingRanges(document) {
        const re = /^\$/;
        let sectionStart = -1;
        const ranges = [];

        for (let i = 0; i < document.lineCount; i++) {
          const lineText = document.lineAt(i).text;
          if (re.test(lineText)) {
            if (sectionStart >= 0) {
              ranges.push(new vscode.FoldingRange(sectionStart, i - 1, vscode.FoldingRangeKind.Region));
            }
            sectionStart = i;
          }
        }

        if (sectionStart > 0) {
          ranges.push(new vscode.FoldingRange(sectionStart, document.lineCount - 1, vscode.FoldingRangeKind.Region));
        }

        return ranges;
      }
    }
  );

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

  client = new LanguageClient(
    'NMTRANLanguageServer',
    'NMTRAN Language Server',
    serverOptions,
    clientOptions
  );

  client.start();
  context.subscriptions.push(foldingProvider);
}

export function deactivate(): Thenable<void> | undefined {
  if (!client) {
    return undefined;
  }
  return client.stop();
}
