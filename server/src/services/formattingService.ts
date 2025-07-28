/**
 * Formatting Service
 * 
 * Provides basic document formatting capabilities for NMTRAN files.
 * Only handles control record positioning and basic continuation line indentation.
 */

import { Connection, TextEdit, Range } from 'vscode-languageserver/node';
import { TextDocument } from 'vscode-languageserver-textdocument';

export class FormattingService {
  private connection: Connection;
  private static readonly DEFAULT_INDENT_SIZE = 2;

  constructor(connection: Connection) {
    this.connection = connection;
  }

  /**
   * Formats the entire document with basic formatting only
   * @param document The text document to format
   * @param indentSize Number of spaces for indentation (2 or 4)
   */
  formatDocument(document: TextDocument, indentSize: number = FormattingService.DEFAULT_INDENT_SIZE): TextEdit[] {
    try {
      const text = document.getText();
      const lines = text.split('\n');
      const edits: TextEdit[] = [];
      
      // Create indentation strings (always use spaces for NMTRAN)
      const baseIndent = ' '.repeat(indentSize);

      for (let i = 0; i < lines.length; i++) {
        const line = lines[i];
        const trimmedLine = line.trim();
        
        // Skip empty lines and comments
        if (trimmedLine.length === 0 || trimmedLine.startsWith(';')) continue;

        let expectedLine = '';
        let needsEdit = false;

        // Format control records (ensure they start at column 0)
        if (trimmedLine.startsWith('$')) {
          expectedLine = trimmedLine;
          needsEdit = (line !== expectedLine);
        }
        // Format continuation lines with basic indentation
        else if (trimmedLine.length > 0) {
          expectedLine = baseIndent + trimmedLine;
          needsEdit = (line !== expectedLine);
        }

        if (needsEdit) {
          edits.push({
            range: Range.create(i, 0, i, line.length),
            newText: expectedLine
          });
        }
      }

      this.connection.console.log(`🎨 Formatted document with ${edits.length} changes using ${indentSize}-space indentation`);
      return edits;

    } catch (error) {
      this.connection.console.error(`❌ Error formatting document: ${error}`);
      return [];
    }
  }

  /**
   * Formats a specific range in the document
   * @param document The text document to format
   * @param _range The range to format (currently formats entire document)
   * @param indentSize Number of spaces for indentation (2 or 4)
   */
  formatRange(document: TextDocument, _range: Range, indentSize: number = FormattingService.DEFAULT_INDENT_SIZE): TextEdit[] {
    try {
      // For simplicity, format the entire document
      return this.formatDocument(document, indentSize);
    } catch (error) {
      this.connection.console.error(`❌ Error formatting range: ${error}`);
      return [];
    }
  }
}