/**
 * Diagnostics Service
 * 
 * Handles document validation and diagnostic generation for NMTRAN files.
 * Separated from main server for better maintainability.
 */

import { Connection, Diagnostic } from 'vscode-languageserver/node';
import { TextDocument } from 'vscode-languageserver-textdocument';
import {
  locateControlRecordsInText,
  generateDiagnosticForControlRecord
} from '../utils/validateControlRecords';

export class DiagnosticsService {
  private connection: Connection;

  constructor(connection: Connection) {
    this.connection = connection;
  }

  /**
   * Validates an NMTRAN document and sends diagnostics
   */
  async validateDocument(document: TextDocument): Promise<void> {
    try {
      const fileName = this.getFileName(document.uri);
      this.connection.console.log(`🔍 Validating ${fileName}...`);
      
      const text = document.getText();
      const controlRecords = locateControlRecordsInText(text);
      const diagnostics: Diagnostic[] = [];

      this.connection.console.log(
        `📄 Found ${controlRecords.length} control records: ${controlRecords.map(m => m[0]).join(', ')}`
      );

      for (const match of controlRecords) {
        const diagnostic = generateDiagnosticForControlRecord(match, document);
        if (diagnostic) {
          diagnostics.push(diagnostic);
          this.connection.console.log(`⚠️  Issue with ${match[0]}: ${diagnostic.message}`);
        }
      }

      this.connection.console.log(`📊 Sending ${diagnostics.length} diagnostics for ${fileName}`);
      this.connection.sendDiagnostics({ 
        uri: document.uri, 
        diagnostics 
      });

    } catch (error) {
      this.connection.console.error(`❌ Error validating document: ${error}`);
      // Don't crash the server on validation errors
      this.connection.sendDiagnostics({ 
        uri: document.uri, 
        diagnostics: [] 
      });
    }
  }

  private getFileName(uri: string): string {
    return uri.split('/').pop() || 'unknown';
  }
}