const vscode = require('vscode')

function activate(context){
	let functionFoldingRanges = vscode.languages.registerFoldingRangeProvider('nmtran', {
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

	context.subscriptions.push(functionFoldingRanges);
}

function deactivate(){}

exports.activate = activate;
exports.deactivate = deactivate;

module.exports = {
  activate,
  deactivate
}