/**
 * Diagnostics Service
 * 
 * Handles document validation and diagnostic generation for NMTRAN files.
 * Separated from main server for better maintainability.
 */

import { Connection, Diagnostic, DiagnosticSeverity } from 'vscode-languageserver/node';
import { TextDocument } from 'vscode-languageserver-textdocument';
import {
  locateControlRecordsInText,
  generateDiagnosticForControlRecord
} from '../utils/validateControlRecords';
import { ParameterScanner } from './ParameterScanner';

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

      // Perform comprehensive parameter validation (single document scan)
      const parameters = ParameterScanner.scanDocument(document);
      
      // Validate sequential parameter numbering
      const parameterValidation = ParameterScanner.validateSequentialNumbering(parameters);
      if (!parameterValidation.isValid) {
        for (const error of parameterValidation.errors) {
          const diagnostic: Diagnostic = {
            severity: DiagnosticSeverity.Error,
            range: {
              start: { line: 0, character: 0 },
              end: { line: 0, character: 0 }
            },
            message: error,
            source: 'nmtran'
          };
          diagnostics.push(diagnostic);
          this.connection.console.log(`⚠️  Parameter validation: ${error}`);
        }
      }

      // Validate parameter references using existing scanned parameters
      const referenceValidation = ParameterScanner.validateParameterReferencesWithParameters(document, parameters);
      if (!referenceValidation.isValid) {
        for (const error of referenceValidation.errors) {
          const diagnostic: Diagnostic = {
            severity: DiagnosticSeverity.Error,
            range: {
              start: { line: error.line, character: error.startChar },
              end: { line: error.line, character: error.endChar }
            },
            message: error.message,
            source: 'nmtran'
          };
          diagnostics.push(diagnostic);
          this.connection.console.log(`⚠️  Parameter reference validation: ${error.message}`);
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