/**
 * Hover Service
 * 
 * Handles hover information for NMTRAN control records.
 * Separated from main server for better maintainability.
 */

import { Connection, Hover, MarkupContent, MarkupKind } from 'vscode-languageserver/node';
import { TextDocument } from 'vscode-languageserver-textdocument';
import { explainControlRecordHover } from '../hoverInfo';
import { getFullControlRecordName } from '../utils/validateControlRecords';

export class HoverService {
  private connection: Connection;
  private readonly CONTROL_RECORD_REGEX = /\$[A-Z]+\b/g;

  constructor(connection: Connection) {
    this.connection = connection;
  }

  /**
   * Provides hover information for control records at the given position
   */
  provideHover(document: TextDocument, position: { line: number; character: number }): Hover | null {
    try {
      const text = document.getText();
      const offset = document.offsetAt(position);
      
      // Reset regex state
      this.CONTROL_RECORD_REGEX.lastIndex = 0;
      let match: RegExpExecArray | null;

      while ((match = this.CONTROL_RECORD_REGEX.exec(text)) !== null) {
        const start = match.index;
        const end = match.index + match[0].length;
        
        if (start <= offset && offset <= end) {
          const controlRecord = match[0];
          const fullControlRecord = getFullControlRecordName(controlRecord);
          
          const hoverInfo: MarkupContent = {
            kind: MarkupKind.Markdown,
            value: explainControlRecordHover(controlRecord, fullControlRecord)
          };

          return {
            contents: hoverInfo,
            range: {
              start: document.positionAt(start),
              end: document.positionAt(end)
            }
          };
        }
      }

      return null;

    } catch (error) {
      this.connection.console.error(`❌ Error providing hover: ${error}`);
      return null;
    }
  }
}