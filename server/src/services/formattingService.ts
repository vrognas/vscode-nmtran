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
      const continuationIndent = baseIndent; // For line continuations

      let currentIndentLevel = 0;
      let inContinuation = false;

      for (let i = 0; i < lines.length; i++) {
        const line = lines[i];
        if (!line) continue;
        const trimmedLine = line.trim();
        
        // Skip empty lines and comments
        if (trimmedLine.length === 0 || trimmedLine.startsWith(';')) continue;

        let expectedLine = '';
        let needsEdit = false;

        // Format control records (ensure they start at column 0)
        if (trimmedLine.startsWith('$')) {
          expectedLine = trimmedLine;
          needsEdit = (line !== expectedLine);
          currentIndentLevel = 0; // Reset indentation for new control record
        }
        // Handle IF/THEN/ELSE/ENDIF indentation
        else if (trimmedLine.length > 0) {
          // Check if this line decreases indentation (ELSE, ELSEIF, ENDIF)
          if (/^(ELSE|ELSEIF|ENDIF)\b/i.test(trimmedLine)) {
            if (currentIndentLevel > 0) currentIndentLevel--;
          }

          // Calculate expected indentation
          const indent = ' '.repeat(Math.max(0, currentIndentLevel) * indentSize + indentSize); // Base + conditional indent
          expectedLine = indent + trimmedLine;
          needsEdit = (line !== expectedLine);

          // Check if this line increases indentation (IF...THEN, ELSEIF...THEN)
          if (/^(IF|ELSEIF)\b.*\bTHEN\s*$/i.test(trimmedLine)) {
            currentIndentLevel++;
          }
          // Handle ELSE (decreases then increases)
          else if (/^ELSE\s*$/i.test(trimmedLine)) {
            currentIndentLevel++;
          }
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