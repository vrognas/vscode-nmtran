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

  test('Language server should provide hover information', async () => {
    const doc = await vscode.workspace.openTextDocument({
      content: '$PROBLEM Test model\n$THETA 1\n',
      language: 'nmtran'
    });
    
    await vscode.window.showTextDocument(doc);
    
    // Test hover on $PROBLEM
    const position = new vscode.Position(0, 1); // Position on '$PROBLEM'
    const hovers = await vscode.commands.executeCommand<vscode.Hover[]>(
      'vscode.executeHoverProvider',
      doc.uri,
      position
    );
    
    assert.ok(hovers && hovers.length > 0, 'Should provide hover information for $PROBLEM');
  });

  test('Language server should provide diagnostics', async () => {
    const doc = await vscode.workspace.openTextDocument({
      content: '$INVALID_RECORD\n', // Invalid control record
      language: 'nmtran'
    });
    
    await vscode.window.showTextDocument(doc);
    
    // Wait for diagnostics to be processed
    await new Promise(resolve => setTimeout(resolve, 1000));
    
    const diagnostics = vscode.languages.getDiagnostics(doc.uri);
    assert.ok(diagnostics.length > 0, 'Should provide diagnostics for invalid control record');
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