// The module 'vscode' contains the VS Code extensibility API
// Import the module and reference it with the alias vscode in your code below
import * as vscode from 'vscode';

// this method is called when your extension is activated
// your extension is activated the very first time the command is executed
export function activate(context: vscode.ExtensionContext){
  let disposable = vscode.languages.registerFoldingRangeProvider('nmtran', {
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

  context.subscriptions.push(disposable);
}
