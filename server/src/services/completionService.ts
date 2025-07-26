/**
 * Completion Service
 * 
 * Provides intelligent code completion for NMTRAN control records and parameters.
 * Modern LSP feature for improved developer experience.
 */

import { Connection, CompletionItem, CompletionItemKind, InsertTextFormat } from 'vscode-languageserver/node';
import { TextDocument } from 'vscode-languageserver-textdocument';
import { allowedControlRecords } from '../constants';

export class CompletionService {
  private connection: Connection;

  constructor(connection: Connection) {
    this.connection = connection;
  }

  /**
   * Provides completion items at the given position
   */
  provideCompletions(document: TextDocument, position: { line: number; character: number }): CompletionItem[] {
    try {
      const text = document.getText();
      const lines = text.split('\n');
      const line = lines[position.line] || '';
      const linePrefix = line.substring(0, position.character);

      // Complete control records when user types $
      if (linePrefix.endsWith('$') || linePrefix.match(/\$[A-Z]*$/)) {
        return this.getControlRecordCompletions();
      }

      // Complete common NMTRAN parameters
      const currentWord = this.getCurrentWord(linePrefix);
      if (currentWord.length > 0) {
        return this.getParameterCompletions(currentWord);
      }

      return [];

    } catch (error) {
      this.connection.console.error(`❌ Error providing completions: ${error}`);
      return [];
    }
  }

  /**
   * Gets completion items for control records
   */
  private getControlRecordCompletions(): CompletionItem[] {
    return allowedControlRecords.map(record => ({
      label: record,
      kind: CompletionItemKind.Keyword,
      detail: 'NMTRAN Control Record',
      documentation: `Insert ${record} control record`,
      insertText: record,
      sortText: record
    }));
  }

  /**
   * Gets completion items for common parameters
   */
  private getParameterCompletions(prefix: string): CompletionItem[] {
    const commonParams = [
      { name: 'METHOD', detail: 'Estimation method', snippet: 'METHOD=${1:1}' },
      { name: 'MAXEVAL', detail: 'Maximum evaluations', snippet: 'MAXEVAL=${1:9999}' },
      { name: 'PRINT', detail: 'Print level', snippet: 'PRINT=${1:1}' },
      { name: 'POSTHOC', detail: 'Post-hoc estimation', snippet: 'POSTHOC' },
      { name: 'LAPLACE', detail: 'Laplace method', snippet: 'LAPLACE' },
      { name: 'INTERACTION', detail: 'Interaction', snippet: 'INTERACTION' },
      { name: 'NOABORT', detail: 'No abort on error', snippet: 'NOABORT' }
    ];

    return commonParams
      .filter(param => param.name.toLowerCase().startsWith(prefix.toLowerCase()))
      .map(param => ({
        label: param.name,
        kind: CompletionItemKind.Property,
        detail: param.detail,
        insertText: param.snippet,
        insertTextFormat: InsertTextFormat.Snippet,
        sortText: param.name
      }));
  }

  /**
   * Extracts the current word being typed
   */
  private getCurrentWord(linePrefix: string): string {
    const match = linePrefix.match(/[A-Za-z_][A-Za-z0-9_]*$/);
    return match ? match[0] : '';
  }
}
