// The module 'vscode' contains the VS Code extensibility API
// Import the module and reference it with the alias vscode in your code below
import * as vscode from 'vscode';
import * as path from 'path';
import {
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
  TransportKind
} from 'vscode-languageclient/node';

let client: LanguageClient; // Declare client at the top level

// this method is called when your extension is activated
// your extension is activated the very first time the command is executed
export function activate(context: vscode.ExtensionContext){

  let disposable = vscode.languages.registerFoldingRangeProvider('nmtran',
  {
    provideFoldingRanges(document, context, token) {
      // console.log('folding range invoked'); // comes here on every character edit
      let sectionStart = 0, FR = [], re = /^\$/;  // regex to detect start of region

      for (let i = 0; i < document.lineCount; i++) {
    
        if (re.test(document.lineAt(i).text)) {
          if (sectionStart >= 0) {
            FR.push(new vscode.FoldingRange(sectionStart, i - 1, vscode.FoldingRangeKind.Region));
          }
          sectionStart = i;
        }
      }
      if (sectionStart > 0) { FR.push(new vscode.FoldingRange(sectionStart, document.lineCount - 1, vscode.FoldingRangeKind.Region)); }
    
      return FR;
    }
  });

  // Adding Language Client Setup
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

  // Register the folding range provider
  context.subscriptions.push(disposable);
}

export function deactivate(): Thenable<void> | undefined {
  if (!client) {
    return undefined;
  }
  return client.stop();
}
