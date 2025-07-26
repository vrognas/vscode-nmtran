/**
 * Formatting Service
 * 
 * Provides document formatting capabilities for NMTRAN files.
 * Modern LSP feature for better code consistency.
 */

import { Connection, TextEdit, Range } from 'vscode-languageserver/node';
import { TextDocument } from 'vscode-languageserver-textdocument';

export class FormattingService {
  private connection: Connection;

  constructor(connection: Connection) {
    this.connection = connection;
  }

  /**
   * Formats the entire document
   */
  formatDocument(document: TextDocument): TextEdit[] {
    try {
      const text = document.getText();
      const lines = text.split('\n');
      const edits: TextEdit[] = [];

      for (let i = 0; i < lines.length; i++) {
        const line = lines[i];
        const trimmedLine = line.trim();
        
        // Skip empty lines
        if (trimmedLine.length === 0) continue;

        // Format control records (ensure they start at column 0)
        if (trimmedLine.startsWith('$')) {
          if (line !== trimmedLine) {
            edits.push({
              range: Range.create(i, 0, i, line.length),
              newText: trimmedLine
            });
          }
        }
        // Format continuation lines (ensure proper indentation)
        else if (trimmedLine.length > 0 && !trimmedLine.startsWith(';')) {
          const expectedIndent = '  '; // 2 spaces for continuation
          const expectedLine = expectedIndent + trimmedLine;
          
          if (line !== expectedLine) {
            edits.push({
              range: Range.create(i, 0, i, line.length),
              newText: expectedLine
            });
          }
        }
      }

      this.connection.console.log(`🎨 Formatted document with ${edits.length} changes`);
      return edits;

    } catch (error) {
      this.connection.console.error(`❌ Error formatting document: ${error}`);
      return [];
    }
  }

  /**
   * Formats a specific range in the document
   */
  formatRange(document: TextDocument, _range: Range): TextEdit[] {
    try {
      // For simplicity, format the entire document
      // Could be optimized to only format the specified range
      return this.formatDocument(document);
    } catch (error) {
      this.connection.console.error(`❌ Error formatting range: ${error}`);
      return [];
    }
  }
}