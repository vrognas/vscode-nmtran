/**
 * Input validation utilities for NMTRAN Language Server
 */

/**
 * Validates that a line number is within document bounds
 */
export function isValidLineNumber(lineNum: number, totalLines: number): boolean {
  return lineNum >= 0 && lineNum < totalLines && Number.isInteger(lineNum);
}

/**
 * Validates that a character position is within line bounds
 */
export function isValidCharPosition(charPos: number, lineLength: number): boolean {
  return charPos >= 0 && charPos <= lineLength && Number.isInteger(charPos);
}

/**
 * Validates parameter index (must be positive integer)
 */
export function isValidParameterIndex(index: number): boolean {
  return index > 0 && Number.isInteger(index) && index <= 9999; // Reasonable upper limit
}

/**
 * Sanitizes file paths to prevent directory traversal
 */
export function sanitizeFilePath(path: string): string {
  // Remove any directory traversal attempts
  return path.replace(/\.\./g, '').replace(/^\//, '');
}