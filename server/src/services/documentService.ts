/**
 * Document Service
 * 
 * Manages document lifecycle and provides document-related services.
 * Centralized document management for better maintainability.
 */

import { TextDocument } from 'vscode-languageserver-textdocument';
import { Connection } from 'vscode-languageserver/node';

export class DocumentService {
  private documents: Map<string, TextDocument> = new Map();
  private connection: Connection;

  constructor(connection: Connection) {
    this.connection = connection;
  }

  /**
   * Adds or updates a document in the cache
   */
  setDocument(document: TextDocument): void {
    this.documents.set(document.uri, document);
    this.connection.console.log(`📝 Document cached: ${this.getFileName(document.uri)}`);
  }

  /**
   * Retrieves a document from the cache
   */
  getDocument(uri: string): TextDocument | undefined {
    return this.documents.get(uri);
  }

  /**
   * Removes a document from the cache
   */
  removeDocument(uri: string): boolean {
    const removed = this.documents.delete(uri);
    if (removed) {
      this.connection.console.log(`🗑️  Document removed: ${this.getFileName(uri)}`);
    }
    return removed;
  }

  /**
   * Gets all cached document URIs
   */
  getAllDocumentUris(): string[] {
    return Array.from(this.documents.keys());
  }

  /**
   * Creates a new TextDocument instance
   */
  createDocument(uri: string, languageId: string, version: number, content: string): TextDocument {
    return TextDocument.create(uri, languageId, version, content);
  }

  /**
   * Gets the file name from a URI
   */
  private getFileName(uri: string): string {
    return uri.split('/').pop() || 'unknown';
  }

  /**
   * Gets cache statistics for debugging
   */
  getCacheStats(): { documentCount: number; totalSize: number } {
    let totalSize = 0;
    for (const doc of this.documents.values()) {
      totalSize += doc.getText().length;
    }
    
    return {
      documentCount: this.documents.size,
      totalSize
    };
  }
}