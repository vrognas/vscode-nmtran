/**
 * Document Service
 *
 * Manages document lifecycle and provides document-related services.
 * Centralized document management for better maintainability.
 * Uses LRU eviction to prevent unbounded memory growth.
 */

import { TextDocument } from 'vscode-languageserver-textdocument';
import { Connection } from 'vscode-languageserver/node';

export class DocumentService {
  private documents: Map<string, TextDocument> = new Map();
  private linesCache = new Map<string, { version: number; lines: string[] }>();
  private accessOrder: string[] = []; // Track access order for LRU
  private readonly maxCacheSize: number;

  constructor(_connection: Connection, maxCacheSize = 50) {
    this.maxCacheSize = maxCacheSize;
  }

  /**
   * Adds or updates a document in the cache with LRU eviction
   */
  setDocument(document: TextDocument): void {
    const uri = document.uri;

    // Update access order
    this.updateAccessOrder(uri);

    // Evict oldest if over capacity
    while (this.documents.size >= this.maxCacheSize && this.accessOrder.length > 0) {
      const oldest = this.accessOrder.shift();
      if (oldest && oldest !== uri) {
        this.documents.delete(oldest);
        this.linesCache.delete(oldest);
      }
    }

    this.documents.set(uri, document);
  }

  /**
   * Updates access order for LRU tracking
   */
  private updateAccessOrder(uri: string): void {
    const idx = this.accessOrder.indexOf(uri);
    if (idx !== -1) {
      this.accessOrder.splice(idx, 1);
    }
    this.accessOrder.push(uri);
  }

  /**
   * Retrieves a document from the cache and updates access order
   */
  getDocument(uri: string): TextDocument | undefined {
    const doc = this.documents.get(uri);
    if (doc) {
      this.updateAccessOrder(uri);
    }
    return doc;
  }

  /**
   * Returns pre-split lines for a document, cached by version.
   */
  getLines(uri: string): string[] | undefined {
    const doc = this.documents.get(uri);
    if (!doc) return undefined;

    const cached = this.linesCache.get(uri);
    if (cached && cached.version === doc.version) {
      return cached.lines;
    }

    const lines = doc.getText().split('\n');
    this.linesCache.set(uri, { version: doc.version, lines });
    return lines;
  }

  /**
   * Removes a document from the cache
   */
  removeDocument(uri: string): boolean {
    const removed = this.documents.delete(uri);
    this.linesCache.delete(uri);
    if (removed) {
      const idx = this.accessOrder.indexOf(uri);
      if (idx !== -1) {
        this.accessOrder.splice(idx, 1);
      }
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