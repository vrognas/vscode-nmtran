/**
 * NMTRANMatrixParser - Handles parsing of NMTRAN matrix structures
 * 
 * This class encapsulates the logic for parsing BLOCK matrices, diagonal elements,
 * and SAME references in NMTRAN parameter definitions.
 */

export interface MatrixElement {
  value: string;
  position: number;
  isDiagonal: boolean;
}

export interface DiagonalPosition {
  arrayIndex: number;  // Position in the flattened array
  matrixRow: number;   // Row in the matrix (1-based)
  matrixCol: number;   // Column in the matrix (1-based)
}

export class NMTRANMatrixParser {
  /**
   * Calculate the position of diagonal elements in a lower triangular matrix
   * stored as a flattened array.
   * 
   * For a BLOCK(n) matrix, diagonal elements are at positions:
   * - (1,1): position 0
   * - (2,2): position 2
   * - (3,3): position 5
   * - (4,4): position 9
   * - etc.
   * 
   * @param paramIndex The parameter index (1-based)
   * @returns The array position of the diagonal element
   */
  static getDiagonalPosition(paramIndex: number): number {
    // For lower triangular matrix, diagonal position of parameter n is: n*(n-1)/2 + (n-1) = n*(n+1)/2 - 1
    return (paramIndex * (paramIndex + 1)) / 2 - 1;
  }

  /**
   * Parse a BLOCK matrix line and extract all numeric values
   * @param line The line containing BLOCK matrix values
   * @returns Array of numeric values found
   */
  static parseBlockValues(line: string): string[] {
    // Remove comments
    const codePartEnd = line.indexOf(';');
    const codePart = codePartEnd !== -1 ? line.substring(0, codePartEnd) : line;
    
    // Remove control record and BLOCK(n) pattern
    let contentPart = codePart.replace(/^\s*\$\w+\s*/i, '');
    contentPart = contentPart.replace(/^BLOCK\(\d+\)\s*/i, '');
    
    // Extract all numeric values
    const numericPattern = /[\d\-+][\d\-+.eE]*/g;
    const values: string[] = [];
    let match;
    
    while ((match = numericPattern.exec(contentPart)) !== null) {
      values.push(match[0]);
    }
    
    return values;
  }

  /**
   * Extract diagonal elements from a BLOCK matrix
   * @param blockSize The size of the BLOCK matrix
   * @param values All values in the lower triangular matrix
   * @returns Array of diagonal elements
   */
  static extractDiagonalElements(blockSize: number, values: string[]): string[] {
    const diagonals: string[] = [];
    
    for (let i = 1; i <= blockSize; i++) {
      const pos = this.getDiagonalPosition(i);
      if (pos < values.length && values[pos] !== undefined) {
        diagonals.push(values[pos]!);
      }
    }
    
    return diagonals;
  }

  /**
   * Determine if a given position in a flattened array represents a diagonal element
   * @param position Position in the flattened array
   * @returns The parameter index if diagonal, null otherwise
   */
  static isDiagonalElement(position: number): number | null {
    // Diagonal positions follow formula: pos = n*(n+1)/2 - 1
    // Solving for n: n = floor((sqrt(8*pos + 9) - 1) / 2)
    // We add 1 to position since getDiagonalPosition returns n*(n+1)/2 - 1
    const n = Math.floor((Math.sqrt(8 * (position + 1) + 1) - 1) / 2);

    // Verify this n actually produces the given position
    if (n >= 1 && this.getDiagonalPosition(n) === position) {
      return n;
    }

    return null;
  }

  /**
   * Calculate the total number of elements in a lower triangular matrix
   * @param size The size of the square matrix
   * @returns Total number of elements including diagonal
   */
  static getTriangularMatrixSize(size: number): number {
    return (size * (size + 1)) / 2;
  }

  /**
   * Convert a flat array position to matrix coordinates
   * @param position Position in flattened array
   * @returns Matrix coordinates (1-based) or null if invalid
   */
  static getMatrixCoordinates(position: number): DiagonalPosition | null {
    // This is a simplified implementation
    // In practice, you'd solve: position = row*(row-1)/2 + col - 1
    let row = 1;
    let elementsBeforeRow = 0;
    
    while (elementsBeforeRow + row <= position) {
      elementsBeforeRow += row;
      row++;
    }
    
    const col = position - elementsBeforeRow + 1;
    
    return {
      arrayIndex: position,
      matrixRow: row,
      matrixCol: col
    };
  }

  /**
   * Parse a complete NMTRAN matrix block (potentially multi-line)
   * @param lines Array of lines containing the matrix definition
   * @param startLine The line index where the BLOCK starts
   * @returns Parsed matrix information
   */
  static parseMatrixBlock(lines: string[], startLine: number): {
    blockSize: number;
    values: string[];
    diagonalElements: string[];
    endLine: number;
  } | null {
    const firstLine = lines[startLine];
    if (!firstLine) return null;
    
    // Extract block size
    const blockMatch = firstLine.match(/BLOCK\((\d+)\)/i);
    if (!blockMatch || !blockMatch[1]) return null;
    
    const blockSize = parseInt(blockMatch[1], 10);
    const expectedElements = this.getTriangularMatrixSize(blockSize);
    const values: string[] = [];
    
    // Check for inline values first
    const inlineValues = this.parseBlockValues(firstLine);
    values.push(...inlineValues);
    
    // If we don't have all values, look at continuation lines
    let currentLine = startLine + 1;
    while (values.length < expectedElements && currentLine < lines.length) {
      const line = lines[currentLine];
      if (!line || line.trim().startsWith('$')) break;
      
      const lineValues = this.parseBlockValues(line);
      values.push(...lineValues);
      currentLine++;
    }
    
    return {
      blockSize,
      values,
      diagonalElements: this.extractDiagonalElements(blockSize, values),
      endLine: currentLine - 1
    };
  }

  /**
   * Validate that a BLOCK matrix has the correct number of elements
   * @param blockSize The declared size of the BLOCK
   * @param values The values found
   * @returns True if valid, false otherwise
   */
  static validateBlockMatrix(blockSize: number, values: string[]): boolean {
    const expected = this.getTriangularMatrixSize(blockSize);
    return values.length === expected;
  }
}