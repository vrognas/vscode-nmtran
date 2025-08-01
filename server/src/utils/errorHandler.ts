/**
 * Error handling utilities for NMTRAN Language Server
 */

import { Connection } from 'vscode-languageserver';

export class ErrorHandler {
  constructor(private connection: Connection) {}

  /**
   * Wrap a function with error handling
   */
  wrap<T extends (...args: any[]) => any>(
    fn: T,
    context: string
  ): T {
    return ((...args: Parameters<T>): ReturnType<T> => {
      try {
        return fn(...args);
      } catch (error) {
        this.handleError(error, context);
        // Return appropriate default value based on function return type
        return this.getDefaultReturnValue(fn);
      }
    }) as T;
  }

  /**
   * Handle an error with appropriate logging
   */
  handleError(error: unknown, context: string): void {
    const message = error instanceof Error ? error.message : String(error);
    const stack = error instanceof Error ? error.stack : undefined;
    
    this.connection.console.error(`❌ Error in ${context}: ${message}`);
    if (stack && process.env.NODE_ENV === 'development') {
      this.connection.console.error(`Stack trace: ${stack}`);
    }
  }

  /**
   * Get appropriate default return value based on function signature
   */
  private getDefaultReturnValue(fn: Function): any {
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
}