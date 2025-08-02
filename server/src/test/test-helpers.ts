/**
 * Test Helper Functions
 * 
 * Common utilities for creating test documents and scenarios
 */

import { TextDocument } from 'vscode-languageserver-textdocument';

/**
 * Create a TextDocument for testing
 */
export function createDocument(content: string, uri = 'test://test.mod'): TextDocument {
  return TextDocument.create(uri, 'nmtran', 1, content);
}

/**
 * Create a document from a file-like structure with line numbers
 */
export function createDocumentFromLines(lines: string[]): TextDocument {
  return createDocument(lines.join('\n'));
}

/**
 * Helper to get line content from document
 */
export function getLine(document: TextDocument, lineNumber: number): string {
  const lines = document.getText().split('\n');
  return lines[lineNumber] || '';
}

/**
 * Helper to count lines in document
 */
export function getLineCount(document: TextDocument): number {
  return document.getText().split('\n').length;
}