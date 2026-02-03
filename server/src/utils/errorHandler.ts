/**
 * Standardized error handling utilities for the NMTRAN language server
 */

import { Connection } from 'vscode-languageserver';

export interface ErrorContext {
  operation: string;
  lineNumber?: number;
  fileName?: string;
  parameterType?: string;
  additionalInfo?: Record<string, unknown>;
}

export class ErrorHandler {
  constructor(private connection?: Connection) {}

  /**
   * Log a warning with context
   */
  logWarning(message: string, context?: ErrorContext): void {
    const contextStr = context ? this.formatContext(context) : '';
    const fullMessage = `[NMTRAN Warning] ${message}${contextStr}`;
    
    if (this.connection) {
      this.connection.console.warn(fullMessage);
    } else {
      console.warn(fullMessage);
    }
  }

  /**
   * Log an error with context
   */
  logError(message: string, context?: ErrorContext): void {
    const contextStr = context ? this.formatContext(context) : '';
    const fullMessage = `[NMTRAN Error] ${message}${contextStr}`;
    
    if (this.connection) {
      this.connection.console.error(fullMessage);
    } else {
      console.error(fullMessage);
    }
  }

  /**
   * Log debug information with context
   */
  logDebug(message: string, context?: ErrorContext): void {
    const contextStr = context ? this.formatContext(context) : '';
    const fullMessage = `[NMTRAN Debug] ${message}${contextStr}`;
    
    if (this.connection) {
      this.connection.console.log(fullMessage);
    } else {
      console.log(fullMessage);
    }
  }

  /**
   * Handle and log an exception with context
   */
  handleException(error: Error, context?: ErrorContext): void {
    const contextStr = context ? this.formatContext(context) : '';
    const fullMessage = `[NMTRAN Exception] ${error.message}${contextStr}`;
    
    if (this.connection) {
      this.connection.console.error(fullMessage);
      if (error.stack && process.env.NODE_ENV === 'development') {
        this.connection.console.error(error.stack);
      }
    } else {
      console.error(fullMessage);
      if (error.stack) {
        console.error(error.stack);
      }
    }
  }

  /**
   * Create a safe error result for operations that can fail
   */
  createSafeResult<T>(
    operation: () => T,
    fallback: T,
    context?: ErrorContext
  ): T {
    try {
      return operation();
    } catch (error) {
      this.handleException(error as Error, context);
      return fallback;
    }
  }

  /**
   * Create a safe async result for operations that can fail
   */
  async createSafeAsyncResult<T>(
    operation: () => Promise<T>,
    fallback: T,
    context?: ErrorContext
  ): Promise<T> {
    try {
      return await operation();
    } catch (error) {
      this.handleException(error as Error, context);
      return fallback;
    }
  }

  /**
   * Wrap a function with error handling (legacy method)
   * @deprecated Use createSafeResult instead for better type safety
   */
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  wrap<T extends (...args: any[]) => any>(
    fn: T,
    context: string
  ): T {
    return ((...args: Parameters<T>): ReturnType<T> => {
      try {
        return fn(...args);
      } catch (error) {
        this.handleException(error as Error, { operation: context });
        // Return appropriate default value based on function return type
        // eslint-disable-next-line @typescript-eslint/no-explicit-any
        return this.getDefaultReturnValue(fn as (...args: any[]) => any);
      }
    }) as T;
  }

  /**
   * Format error context for logging
   */
  private formatContext(context: ErrorContext): string {
    const parts: string[] = [];
    
    if (context.operation) {
      parts.push(`operation=${context.operation}`);
    }
    if (context.fileName) {
      parts.push(`file=${context.fileName}`);
    }
    if (context.lineNumber !== undefined) {
      parts.push(`line=${context.lineNumber + 1}`); // Convert to 1-based
    }
    if (context.parameterType) {
      parts.push(`type=${context.parameterType}`);
    }
    if (context.additionalInfo) {
      const additionalParts = Object.entries(context.additionalInfo)
        .map(([key, value]) => `${key}=${JSON.stringify(value)}`);
      parts.push(...additionalParts);
    }

    return parts.length > 0 ? ` [${parts.join(', ')}]` : '';
  }

  /**
   * Get appropriate default return value based on function signature (legacy method)
   */
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  private getDefaultReturnValue(fn: (...args: any[]) => any): any {
    const fnString = fn.toString();
    
    // Check return type hints in function signature
    if (fnString.includes('Location[]') || fnString.includes('Array')) {
      return [];
    }
    if (fnString.includes('Location') || fnString.includes('object')) {
      return null;
    }
    if (fnString.includes('number')) {
      return 0;
    }
    if (fnString.includes('string')) {
      return '';
    }
    if (fnString.includes('boolean')) {
      return false;
    }
    
    return null;
  }

  // Static methods for standalone usage
  static logWarning(message: string, context?: ErrorContext): void {
    const handler = new ErrorHandler();
    handler.logWarning(message, context);
  }

  static logError(message: string, context?: ErrorContext): void {
    const handler = new ErrorHandler();
    handler.logError(message, context);
  }

  static logDebug(message: string, context?: ErrorContext): void {
    const handler = new ErrorHandler();
    handler.logDebug(message, context);
  }

  static handleException(error: Error, context?: ErrorContext): void {
    const handler = new ErrorHandler();
    handler.handleException(error, context);
  }
}