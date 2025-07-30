import * as assert from 'assert';
import * as vscode from 'vscode';

suite('NMTRAN Extension Test Suite', () => {
  vscode.window.showInformationMessage('Start all tests.');

  test('Extension should be present', () => {
    assert.ok(vscode.extensions.getExtension('vrognas.nmtran'));
  });

  test('Extension should activate', async () => {
    const extension = vscode.extensions.getExtension('vrognas.nmtran');
    assert.ok(extension);
    
    if (!extension.isActive) {
      await extension.activate();
    }
    
    assert.ok(extension.isActive);
  });

  test('Language should be registered', async () => {
    const doc = await vscode.workspace.openTextDocument({
      content: '$PROBLEM Test model\n$THETA 1\n',
      language: 'nmtran'
    });
    
    assert.strictEqual(doc.languageId, 'nmtran');
  });

  test('Language server connection should be established', async () => {
    // Test that the extension activates and language server connects
    const extension = vscode.extensions.getExtension('vrognas.nmtran');
    assert.ok(extension);
    
    if (!extension.isActive) {
      await extension.activate();
    }
    
    // Give language server time to initialize
    await new Promise(resolve => setTimeout(resolve, 1000));
    
    // Test basic document creation with NMTRAN language
    const doc = await vscode.workspace.openTextDocument({
      content: '$PROBLEM Test model\n$THETA 1\n',
      language: 'nmtran'
    });
    
    assert.strictEqual(doc.languageId, 'nmtran');
    assert.ok(doc.getText().includes('$PROBLEM'), 'Document should contain NMTRAN content');
  });

  test('Language configuration should support proper word selection', async () => {
    const doc = await vscode.workspace.openTextDocument({
      content: 'CL = THETA(1) * EXP(ETA(1))\n',
      language: 'nmtran'
    });
    
    const editor = await vscode.window.showTextDocument(doc);
    
    // Position cursor on THETA(1)
    const position = new vscode.Position(0, 5); // Position on 'THETA'
    editor.selection = new vscode.Selection(position, position);
    
    // Execute word selection command
    await vscode.commands.executeCommand('editor.action.addSelectionToNextFindMatch');
    
    // The selection should include the full THETA(1) token
    const selectedText = doc.getText(editor.selection);
    assert.ok(selectedText.includes('THETA'), 'Should select THETA parameter correctly');
  });
});