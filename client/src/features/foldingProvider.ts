/**
 * Folding Range Provider
 * 
 * Provides code folding functionality for NMTRAN control records.
 * Allows users to collapse/expand sections between control records.
 */

import * as vscode from 'vscode';

export class NMTRANFoldingProvider implements vscode.FoldingRangeProvider {
  
  public provideFoldingRanges(
    document: vscode.TextDocument,
    _context: vscode.FoldingContext,
    _token: vscode.CancellationToken
  ): vscode.ProviderResult<vscode.FoldingRange[]> {
    
    return this.createFoldingRanges(document);
  }

  private createFoldingRanges(document: vscode.TextDocument): vscode.FoldingRange[] {
    const controlRecordPattern = /^\$/;
    let currentSectionStart = -1;
    const foldingRanges: vscode.FoldingRange[] = [];

    // Scan through each line of the document
    for (let lineNumber = 0; lineNumber < document.lineCount; lineNumber++) {
      const lineText = document.lineAt(lineNumber).text;

      // If this line starts with $, it's a new control record section
      if (controlRecordPattern.test(lineText)) {
        // If we were already tracking a section, close it here
        if (currentSectionStart >= 0) {
          const sectionEnd = lineNumber - 1;
          // Only create folding range if there's actual content (not just blank lines)
          if (sectionEnd > currentSectionStart && this.hasContentBetween(document, currentSectionStart, sectionEnd)) {
            foldingRanges.push(
              new vscode.FoldingRange(currentSectionStart, sectionEnd, vscode.FoldingRangeKind.Region)
            );
          }
        }
        // Start tracking this new section
        currentSectionStart = lineNumber;
      }
    }

    // Don't forget the last section (from last $ to end of file)
    if (currentSectionStart >= 0) {
      const sectionEnd = document.lineCount - 1;
      // Only create folding range if there's actual content (not just blank lines)
      if (sectionEnd > currentSectionStart && this.hasContentBetween(document, currentSectionStart, sectionEnd)) {
        foldingRanges.push(
          new vscode.FoldingRange(currentSectionStart, sectionEnd, vscode.FoldingRangeKind.Region)
        );
      }
    }

    return foldingRanges;
  }

  /**
   * Helper function to check if there's meaningful content between lines
   */
  private hasContentBetween(document: vscode.TextDocument, startLine: number, endLine: number): boolean {
    for (let i = startLine + 1; i <= endLine; i++) {
      const line = document.lineAt(i).text.trim();
      // If we find a non-empty line that isn't just whitespace
      if (line.length > 0) {
        return true;
      }
    }
    return false;
  }
}