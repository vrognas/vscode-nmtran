/**
 * Verbatim FORTRAN decorator
 *
 * Paints a subtle background on lines beginning with a double-quote.
 * Per NMTRAN: such lines contain verbatim FORTRAN code. The theme-aware
 * background makes embedded code visually distinct from NMTRAN syntax.
 */

import * as vscode from 'vscode';

export class VerbatimDecorator implements vscode.Disposable {
  private readonly decorationType: vscode.TextEditorDecorationType;
  private readonly subscriptions: vscode.Disposable[] = [];

  constructor() {
    this.decorationType = vscode.window.createTextEditorDecorationType({
      isWholeLine: true,
      light: { backgroundColor: 'rgba(0, 0, 0, 0.05)' },
      dark: { backgroundColor: 'rgba(255, 255, 255, 0.05)' },
    });

    this.subscriptions.push(
      vscode.window.onDidChangeVisibleTextEditors(() => this.updateAllVisible()),
      vscode.workspace.onDidChangeTextDocument((e) => this.updateForDocument(e.document)),
      vscode.workspace.onDidOpenTextDocument((doc) => this.updateForDocument(doc))
    );

    this.updateAllVisible();
  }

  private updateAllVisible(): void {
    for (const editor of vscode.window.visibleTextEditors) {
      if (editor.document.languageId === 'nmtran') {
        this.updateEditor(editor);
      }
    }
  }

  private updateForDocument(doc: vscode.TextDocument): void {
    if (doc.languageId !== 'nmtran') return;
    for (const editor of vscode.window.visibleTextEditors) {
      if (editor.document === doc) this.updateEditor(editor);
    }
  }

  private updateEditor(editor: vscode.TextEditor): void {
    const ranges: vscode.Range[] = [];
    const lineCount = editor.document.lineCount;
    for (let i = 0; i < lineCount; i++) {
      const line = editor.document.lineAt(i);
      if (line.text.startsWith('"')) {
        ranges.push(line.range);
      }
    }
    editor.setDecorations(this.decorationType, ranges);
  }

  dispose(): void {
    this.decorationType.dispose();
    for (const sub of this.subscriptions) sub.dispose();
  }
}
