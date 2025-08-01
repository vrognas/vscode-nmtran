/**
 * ParameterScanner - Encapsulates parameter scanning logic
 * 
 * Breaks down the complex scanAllParameters method into smaller,
 * focused methods for better maintainability and testability.
 */

import { TextDocument } from 'vscode-languageserver-textdocument';
import { NMTRANMatrixParser } from '../utils/NMTRANMatrixParser';
import { ParameterFactory } from '../factories/parameterFactory';
import { LIMITS } from '../constants/parameters';

export interface ParameterLocation {
  type: 'THETA' | 'ETA' | 'EPS';
  index: number;
  line: number;
  startChar?: number;
  endChar?: number;
}

export interface ScannerState {
  currentBlockType: 'THETA' | 'ETA' | 'EPS' | null;
  inBlockMatrix: boolean;
  blockMatrixRemaining: number;
  blockMatrixSize: number;
  blockElementsSeen: number;
  blockDiagonalsSeen: number;
  blockElements: string[];
  counters: { THETA: number; ETA: number; EPS: number };
}

interface BlockMatrixState {
  inBlockMatrix: boolean;
  blockMatrixRemaining: number;
}

// Parameter patterns - compile once for better performance
const PARAMETER_PATTERNS = {
  THETA: /^\$THETA(\s|$)/i,
  OMEGA: /^\$OMEGA(\s|$)/i, 
  SIGMA: /^\$SIGMA(\s|$)/i,
  BLOCK: /BLOCK\((\d+)\)/i,
  SAME: /\bSAME\b/i,
  NUMERIC: /[\d\-+][\d\-+.eE]*/g,
  CONTROL_RECORD: /^\s*\$\w+\s*/i,
  COMMENT: /;.*$/
} as const;

export class ParameterScanner {
  /**
   * Scan document for all parameter definitions
   */
  static scanDocument(document: TextDocument): ParameterLocation[] {
    const locations: ParameterLocation[] = [];
    const lines = document.getText().split('\n');
    const state = ParameterFactory.createScannerState();
    
    for (let lineNum = 0; lineNum < lines.length; lineNum++) {
      const line = lines[lineNum];
      if (!line) continue;
      
      const trimmed = line.trim();
      if (this.shouldSkipLine(trimmed)) continue;
      
      // Update state based on control record
      this.updateStateForControlRecord(trimmed, state);
      
      // Process parameters if in a parameter block
      if (state.currentBlockType) {
        const lineLocations = this.processParameterLine(
          line, 
          lineNum, 
          state, 
          document
        );
        locations.push(...lineLocations);
      }
    }
    
    return locations;
  }

  /**
   * Check if line should be skipped
   */
  private static shouldSkipLine(trimmed: string): boolean {
    return trimmed.startsWith(';') || trimmed.length === 0;
  }

  /**
   * Update scanner state based on control record
   */
  private static updateStateForControlRecord(trimmed: string, state: ScannerState): void {
    if (PARAMETER_PATTERNS.THETA.test(trimmed)) {
      state.currentBlockType = 'THETA';
      state.inBlockMatrix = false;
      state.blockMatrixRemaining = 0;
    } else if (PARAMETER_PATTERNS.OMEGA.test(trimmed)) {
      state.currentBlockType = 'ETA';
      const matrixState = this.detectBlockMatrix(trimmed);
      state.inBlockMatrix = matrixState.inBlockMatrix;
      state.blockMatrixRemaining = matrixState.blockMatrixRemaining;
      // Extract block size from BLOCK(n)
      const blockMatch = trimmed.match(PARAMETER_PATTERNS.BLOCK);
      state.blockMatrixSize = blockMatch && blockMatch[1] ? parseInt(blockMatch[1], 10) : 0;
      state.blockElementsSeen = 0;  // Reset element counter for new block
      state.blockDiagonalsSeen = 0; // Reset diagonal counter for new block
    } else if (PARAMETER_PATTERNS.SIGMA.test(trimmed)) {
      state.currentBlockType = 'EPS';
      const matrixState = this.detectBlockMatrix(trimmed);
      state.inBlockMatrix = matrixState.inBlockMatrix;
      state.blockMatrixRemaining = matrixState.blockMatrixRemaining;
      // Extract block size from BLOCK(n)
      const blockMatch = trimmed.match(PARAMETER_PATTERNS.BLOCK);
      state.blockMatrixSize = blockMatch && blockMatch[1] ? parseInt(blockMatch[1], 10) : 0;
      state.blockElementsSeen = 0;  // Reset element counter for new block
      state.blockDiagonalsSeen = 0; // Reset diagonal counter for new block
    } else if (trimmed.startsWith('$')) {
      // Different control record - reset state
      state.currentBlockType = null;
      state.inBlockMatrix = false;
      state.blockMatrixRemaining = 0;
    }
  }

  /**
   * Detect BLOCK matrix from line
   */
  private static detectBlockMatrix(line: string): BlockMatrixState {
    const blockMatch = line.match(PARAMETER_PATTERNS.BLOCK);
    if (blockMatch && blockMatch[1]) {
      const blockSize = parseInt(blockMatch[1], 10);
      
      // BLOCK(1) should always be treated as regular parameters
      // because it's just a single diagonal element, not a true matrix
      if (blockSize === 1) {
        return {
          inBlockMatrix: false,
          blockMatrixRemaining: 0
        };
      }
      
      return {
        inBlockMatrix: true,
        blockMatrixRemaining: blockSize
      };
    }
    return {
      inBlockMatrix: false,
      blockMatrixRemaining: 0
    };
  }

  /**
   * Check if line has inline values after BLOCK declaration
   */
  private static hasInlineValues(line: string): boolean {
    const afterBlock = line.replace(/^\s*\$\w+\s+BLOCK\(\d+\)\s*/i, '');
    return afterBlock.trim().length > 0 && !/^;/.test(afterBlock.trim());
  }

  /**
   * Process a line containing parameters
   */
  private static processParameterLine(
    line: string, 
    lineNum: number, 
    state: ScannerState,
    document: TextDocument
  ): ParameterLocation[] {
    const locations: ParameterLocation[] = [];
    const trimmed = line.trim();
    
    // Count parameters on this line
    const paramCount = this.countParametersOnLine(trimmed, state);
    
    
    // For BLOCK matrices, first count all numeric values on this line
    let allValuesOnLine: string[] = [];
    if (state.inBlockMatrix) {
      const cleanLine = trimmed.replace(PARAMETER_PATTERNS.CONTROL_RECORD, '')
                               .replace(/BLOCK\(\d+\)\s*/i, '')
                               .replace(PARAMETER_PATTERNS.COMMENT, '');
      const matches = cleanLine.match(PARAMETER_PATTERNS.NUMERIC);
      allValuesOnLine = matches || [];
      
    }
    
    // For BLOCK matrices, we need to determine which values are diagonal elements
    if (state.inBlockMatrix && allValuesOnLine.length > 0) {
      // Figure out which elements these values represent in the overall matrix
      const startElementIndex = state.blockElementsSeen;
      
      // For each diagonal parameter on this line
      let valuesProcessed = 0;
      for (let i = 0; i < paramCount; i++) {
        const blockType = state.currentBlockType;
        if (!blockType) continue;
        
        state.counters[blockType]++;
        
        const location: ParameterLocation = {
          type: blockType,
          index: state.counters[blockType],
          line: lineNum
        };
        
        // Which diagonal parameter is this within the current BLOCK?
        // For BLOCK matrices, we need to know which diagonal within the block this is
        // ETA(3) is the 1st diagonal in its BLOCK(2)
        // ETA(4) is the 2nd diagonal in its BLOCK(2)
        const diagonalWithinBlock = state.blockDiagonalsSeen + i + 1;
        
        // For the Nth diagonal, we need to find which element position it's at
        // and then find which value on this line corresponds to that position
        const { NMTRANMatrixParser } = require('../utils/NMTRANMatrixParser');
        const diagonalElementPosition = NMTRANMatrixParser.getDiagonalPosition(diagonalWithinBlock);
        
        // Which value on THIS line corresponds to this diagonal?
        const positionOnLine = diagonalElementPosition - startElementIndex;
        
        
        
        if (positionOnLine >= 0 && positionOnLine < allValuesOnLine.length) {
          // Find the position of this specific value
          const targetValue = allValuesOnLine[positionOnLine];
          
          
          if (targetValue) {
            const cleanLine = line.replace(/;.*$/, '');
            const prevValue = positionOnLine > 0 ? allValuesOnLine[positionOnLine - 1] : undefined;
            const searchStart = prevValue ? cleanLine.lastIndexOf(prevValue) : 0;
            const valueStart = cleanLine.indexOf(targetValue, searchStart);
            
            if (valueStart !== -1) {
              location.startChar = valueStart;
              location.endChar = valueStart + targetValue.length;
            }
          }
        }
        
        locations.push(location);
        valuesProcessed++;
      }
      
      // Update total elements seen
      state.blockElementsSeen += allValuesOnLine.length;
    } else {
      // Regular (non-block) parameter processing
      for (let i = 0; i < paramCount; i++) {
        const blockType = state.currentBlockType;
        if (!blockType) continue;
        
        state.counters[blockType]++;
        
        const location: ParameterLocation = {
          type: blockType,
          index: state.counters[blockType],
          line: lineNum
        };
        
        const valuePosition = this.findParameterValuePosition(
          line,
          lineNum,
          blockType,
          1,
          false,
          i + 1,
          document
        );
        
        if (valuePosition) {
          location.startChar = valuePosition.start;
          location.endChar = valuePosition.end;
        }
        
        locations.push(location);
      }
    }
    
    // Update block matrix state
    if (state.inBlockMatrix) {
      // Don't update here - we already updated in the loop above
      state.blockDiagonalsSeen += paramCount;
      state.blockMatrixRemaining -= paramCount;
      if (state.blockMatrixRemaining <= 0) {
        state.inBlockMatrix = false;
        state.blockElementsSeen = 0;
        state.blockDiagonalsSeen = 0;
      }
    }
    
    return locations;
  }

  /**
   * Count parameters on a line based on current state
   */
  private static countParametersOnLine(trimmed: string, state: ScannerState): number {
    if (state.inBlockMatrix && state.blockMatrixRemaining > 0) {
      return this.countBlockMatrixParameters(trimmed, state);
    } else {
      return this.countRegularParameters(trimmed, state.currentBlockType);
    }
  }

  /**
   * Count parameters in a BLOCK matrix context
   */
  private static countBlockMatrixParameters(trimmed: string, state: ScannerState): number {
    if (trimmed.match(PARAMETER_PATTERNS.BLOCK)) {
      if (PARAMETER_PATTERNS.SAME.test(trimmed)) {
        // SAME constraint - defines 1 parameter
        return 1;
      } else {
        // Check for inline values
        const afterBlock = trimmed.replace(/^\$OMEGA\s+BLOCK\(\d+\)\s*/i, '')
                                  .replace(/^\$SIGMA\s+BLOCK\(\d+\)\s*/i, '');
        const hasValues = afterBlock.trim().length > 0 && !/^;/.test(afterBlock.trim());
        
        if (hasValues) {
          // Count diagonal parameters in inline values
          const numValues = this.countNumericValues(afterBlock);
          return Math.min(state.blockMatrixRemaining, numValues);
        }
        // BLOCK header without values
        return 0;
      }
    } else {
      // Matrix data line - one diagonal parameter
      return 1;
    }
  }

  /**
   * Count regular (non-matrix) parameters
   */
  private static countRegularParameters(line: string, blockType: 'THETA' | 'ETA' | 'EPS' | null): number {
    if (!blockType) return 0;
    
    // Check for SAME keyword first - it counts as 1 parameter
    if (PARAMETER_PATTERNS.SAME.test(line)) {
      return 1;
    }
    
    const cleanContent = this.removeKeywords(line);
    if (!cleanContent) return 0;
    
    return this.countNumericValues(cleanContent);
  }

  /**
   * Remove keywords from parameter line
   */
  private static removeKeywords(line: string): string {
    // Remove comments
    const commentIndex = line.indexOf(';');
    const contentPart = commentIndex !== -1 ? line.substring(0, commentIndex) : line;
    
    // Remove control record prefix
    let cleanedPrefix = contentPart.replace(/^\s*\$\w+\s*/i, '');
    
    // Remove BLOCK(n) pattern for BLOCK(1) parameters that are treated as regular
    cleanedPrefix = cleanedPrefix.replace(/^BLOCK\(\d+\)\s*/i, '');
    
    // Remove parameter keywords but keep numeric values
    return cleanedPrefix.replace(/\b(FIX|FIXED|STANDARD|VARIANCE|CORRELATION|CHOLESKY|DIAGONAL|SAME|VALUES|NAMES)\b/gi, '').trim();
  }

  /**
   * Count numeric values in a string
   */
  private static countNumericValues(content: string): number {
    const numericPattern = /[\d\-+][\d\-+.eE]*/g;
    const matches = content.match(numericPattern);
    return matches ? matches.length : 0;
  }

  /**
   * Find the position of a parameter value
   * Delegates to appropriate finder based on parameter type
   */
  private static findParameterValuePosition(
    line: string,
    lineNum: number,
    paramType: 'THETA' | 'ETA' | 'EPS',
    paramIndex: number,
    inBlockMatrix: boolean,
    positionInLine: number,
    document: TextDocument
  ): { start: number; end: number } | null {
    const trimmed = line.trim();
    
    
    // Handle SAME keyword
    if (PARAMETER_PATTERNS.SAME.test(trimmed)) {
      const match = trimmed.match(PARAMETER_PATTERNS.SAME);
      if (match && match.index !== undefined) {
        const start = line.indexOf(match[0]);
        return {
          start: start,
          end: start + match[0].length
        };
      }
    }
    
    // For BLOCK(1) or regular parameters, find the first numeric value
    // Remove control record prefix and BLOCK(n) pattern
    let searchText = trimmed;
    const controlMatch = searchText.match(/^\s*\$\w+\s*/i);
    if (controlMatch) {
      searchText = searchText.substring(controlMatch[0].length);
    }
    
    const blockMatch = searchText.match(/^BLOCK\(\d+\)\s*/i);
    if (blockMatch) {
      searchText = searchText.substring(blockMatch[0].length);
    }
    
    // Remove comment part
    const commentIndex = searchText.indexOf(';');
    if (commentIndex !== -1) {
      searchText = searchText.substring(0, commentIndex);
    }
    
    // For BLOCK matrices, find the specific diagonal element
    if (inBlockMatrix) {
      // For multi-line BLOCK matrices, each continuation line has one diagonal parameter
      // The position within the row equals the parameter index within the block
      // E.g., for BLOCK(2): line 1 has 1 value (diagonal pos 1), line 2 has 2 values (diagonal pos 2)
      
      // Find all numeric values on this line
      const numericPattern = /[\d\-+][\d\-+.eE]*/g;
      const matches = [];
      let match;
      
      while ((match = numericPattern.exec(searchText)) !== null) {
        matches.push({
          value: match[0],
          index: match.index
        });
      }
      
      // For multi-line blocks, the diagonal position equals the parameter index
      // For single-line blocks with all values, use the diagonal position calculation
      const targetPosition = matches.length === 1 ? 0 : paramIndex - 1;
      
      // Debug: Remove after testing
      // console.log(`BLOCK: Found ${matches.length} values, using position ${targetPosition} for paramIndex ${paramIndex}`);
      
      if (matches.length > targetPosition && targetPosition >= 0) {
        const targetMatch = matches[targetPosition];
        if (targetMatch) {
          const numericValue = targetMatch.value;
          const absoluteStart = line.indexOf(numericValue, targetMatch.index > 0 ? line.indexOf(searchText) : 0);
        
          if (absoluteStart !== -1) {
            // Debug: Remove after testing
            // console.log(`Found: "${numericValue}" at ${absoluteStart}-${absoluteStart + numericValue.length}`);
            return {
              start: absoluteStart,
              end: absoluteStart + numericValue.length
            };
          }
        }
      }
    } else {
      // For regular parameters or first diagonal element, find first numeric value
      const numericPattern = /[\d\-+][\d\-+.eE]*/;
      const match = searchText.match(numericPattern);
      
      if (match && match.index !== undefined) {
        // Find the absolute position in the original line by searching for the numeric value
        const numericValue = match[0];
        const absoluteStart = line.indexOf(numericValue);
        
        if (absoluteStart !== -1) {
          return {
            start: absoluteStart,
            end: absoluteStart + numericValue.length
          };
        }
      }
    }
    
    return null;
  }
}