/**
 * ParameterScanner - Encapsulates parameter scanning logic
 * 
 * Breaks down the complex scanAllParameters method into smaller,
 * focused methods for better maintainability and testability.
 */

import { TextDocument } from 'vscode-languageserver-textdocument';
import { NMTRANMatrixParser } from '../utils/NMTRANMatrixParser';
import { ParameterFactory } from '../factories/parameterFactory';
import { ParameterValidator } from '../utils/parameterValidator';
import { ErrorHandler } from '../utils/errorHandler';

export interface ParameterLocation {
  type: 'THETA' | 'ETA' | 'EPS';
  index: number;
  line: number;
  startChar?: number;
  endChar?: number;
  additionalRanges?: Array<{ startChar: number; endChar: number; line?: number }>; // For FIXED keyword, etc.
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
  blockFixedKeywords: Array<{ startChar: number; endChar: number; line: number }>; // FIXED keywords for current block
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
  FIXED: /\b(FIX|FIXED)\b/gi,
  FIXED_CASE_INSENSITIVE: /\b(FIX|FIXED)\b/i,
  FIXED_START: /^(FIX|FIXED)\b/i,
  NUMERIC: /[\d\-+][\d\-+.eE]*/g,
  NUMERIC_SINGLE: /[\d\-+][\d\-+.eE]*/,
  CONTROL_RECORD: /^\s*\$\w+\s*/i,
  COMMENT: /;.*$/,
  COMMENT_END: /;.*$/,
  PARAMETER_REFERENCE: /\b(THETA|ETA|EPS|ERR)\((\d+)\)/g,
  WHITESPACE: /\s/,
  WHITESPACE_OR_PAREN: /[\s(]/,
  BLOCK_INLINE: /^\s*\$\w+\s+BLOCK\(\d+\)\s*/i,
  OMEGA_BLOCK_PREFIX: /^\$OMEGA\s+BLOCK\(\d+\)\s*/i,
  SIGMA_BLOCK_PREFIX: /^\$SIGMA\s+BLOCK\(\d+\)\s*/i,
  BLOCK_PREFIX: /^BLOCK\(\d+\)\s*/i,
  PARAMETER_KEYWORDS: /\b(FIX|FIXED|STANDARD|VARIANCE|CORRELATION|CHOLESKY|DIAGONAL|SAME|VALUES|NAMES)\b/gi,
  COMMENT_START: /^;/
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
      
      // Validate line before processing
      const lineValidation = ParameterValidator.validateParameterLine(line);
      if (!lineValidation.isValid) {
        ErrorHandler.logWarning(
          `Skipping invalid line: ${lineValidation.errors.join(', ')}`,
          { operation: 'scanDocument', lineNumber: lineNum }
        );
        continue;
      }
      
      const trimmed = line.trim();
      if (this.shouldSkipLine(trimmed)) continue;
      
      // Update state based on control record
      this.updateStateForControlRecord(trimmed, state, lineNum);
      
      // Validate state consistency
      const stateValidation = ParameterValidator.validateScannerState(state);
      if (!stateValidation.isValid) {
        ErrorHandler.logWarning(
          `Scanner state validation failed: ${stateValidation.errors.join(', ')}`,
          { operation: 'scanDocument', lineNumber: lineNum }
        );
      }
      
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
  private static updateStateForControlRecord(trimmed: string, state: ScannerState, lineNum: number): void {
    // Remove inline comments before checking control records
    const commentIndex = trimmed.indexOf(';');
    const lineWithoutComment = commentIndex !== -1 ? trimmed.substring(0, commentIndex).trim() : trimmed;
    
    if (PARAMETER_PATTERNS.THETA.test(lineWithoutComment)) {
      state.currentBlockType = 'THETA';
      state.inBlockMatrix = false;
      state.blockMatrixRemaining = 0;
    } else if (PARAMETER_PATTERNS.OMEGA.test(lineWithoutComment)) {
      state.currentBlockType = 'ETA';
      const matrixState = this.detectBlockMatrix(lineWithoutComment);
      state.inBlockMatrix = matrixState.inBlockMatrix;
      state.blockMatrixRemaining = matrixState.blockMatrixRemaining;
      // Extract block size from BLOCK(n)
      const blockMatch = lineWithoutComment.match(PARAMETER_PATTERNS.BLOCK);
      state.blockMatrixSize = blockMatch && blockMatch[1] ? parseInt(blockMatch[1], 10) : 0;
      state.blockElementsSeen = 0;  // Reset element counter for new block
      state.blockDiagonalsSeen = 0; // Reset diagonal counter for new block
      state.blockFixedKeywords = []; // Reset FIXED keywords for new block
      
      // Check for FIXED keywords on the BLOCK declaration line
      if (state.inBlockMatrix) {
        this.detectBlockFixedKeywords(lineWithoutComment, lineNum, state);
      }
    } else if (PARAMETER_PATTERNS.SIGMA.test(lineWithoutComment)) {
      state.currentBlockType = 'EPS';
      const matrixState = this.detectBlockMatrix(lineWithoutComment);
      state.inBlockMatrix = matrixState.inBlockMatrix;
      state.blockMatrixRemaining = matrixState.blockMatrixRemaining;
      // Extract block size from BLOCK(n)
      const blockMatch = lineWithoutComment.match(PARAMETER_PATTERNS.BLOCK);
      state.blockMatrixSize = blockMatch && blockMatch[1] ? parseInt(blockMatch[1], 10) : 0;
      state.blockElementsSeen = 0;  // Reset element counter for new block
      state.blockDiagonalsSeen = 0; // Reset diagonal counter for new block
      state.blockFixedKeywords = []; // Reset FIXED keywords for new block
      
      // Check for FIXED keywords on the BLOCK declaration line
      if (state.inBlockMatrix) {
        this.detectBlockFixedKeywords(lineWithoutComment, lineNum, state);
      }
    } else if (lineWithoutComment.startsWith('$')) {
      // Different control record - reset state
      state.currentBlockType = null;
      state.inBlockMatrix = false;
      state.blockMatrixRemaining = 0;
    }
  }

  /**
   * Detect and store FIXED keywords from BLOCK declaration line
   */
  private static detectBlockFixedKeywords(line: string, lineNum: number, state: ScannerState): void {
    let match;
    while ((match = PARAMETER_PATTERNS.FIXED.exec(line)) !== null) {
      state.blockFixedKeywords.push({
        startChar: match.index,
        endChar: match.index + match[0].length,
        line: lineNum
      });
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
      const processedLocations = this.processBlockMatrixLine(
        line,
        lineNum,
        state,
        allValuesOnLine
      );
      locations.push(...processedLocations);
    } else {
      // Regular (non-block) parameter processing - use sophisticated parser for THETA
      if (state.currentBlockType === 'THETA') {
        const expressions = this.parseParameterExpressions(line);
        
        for (let i = 0; i < Math.min(paramCount, expressions.length); i++) {
          state.counters.THETA++;
          
          const expr = expressions[i];
          const location: ParameterLocation = {
            type: 'THETA',
            index: state.counters.THETA,
            line: lineNum
          };
          
          if (expr) {
            location.startChar = expr.valueRange.startChar;
            location.endChar = expr.valueRange.endChar;
          }
          
          // Add FIXED keyword range if present
          if (expr?.fixedRange) {
            location.additionalRanges = [expr.fixedRange];
          }
          
          locations.push(location);
        }
      } else {
        // For OMEGA/SIGMA, use simpler processing (they typically don't have complex FIXED syntax)
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
          
          // Check for FIXED keyword on this line (simpler approach for OMEGA/SIGMA)
          PARAMETER_PATTERNS.FIXED.lastIndex = 0; // Reset global regex
          const fixedMatches = [];
          let match;
          while ((match = PARAMETER_PATTERNS.FIXED.exec(line)) !== null) {
            fixedMatches.push({
              startChar: match.index,
              endChar: match.index + match[0].length
            });
          }
          if (fixedMatches.length > 0) {
            location.additionalRanges = fixedMatches;
          }
          
          locations.push(location);
        }
      }
    }
    
    // Update block matrix state
    if (state.inBlockMatrix && state.blockMatrixRemaining <= 0) {
      state.inBlockMatrix = false;
      state.blockElementsSeen = 0;
      state.blockDiagonalsSeen = 0;
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
      // Matrix data line - could have multiple diagonal parameters if all values are on one line
      const numValues = this.countNumericValues(trimmed);
      // Return the minimum of remaining parameters needed and values found
      return Math.min(state.blockMatrixRemaining, numValues);
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
    
    // For THETA parameters, count expressions (not individual numeric values)
    if (blockType === 'THETA') {
      const expressions = this.parseParameterExpressions(line);
      return expressions.length;
    }
    
    // For OMEGA/SIGMA, use the old numeric counting method
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
    return cleanedPrefix.replace(PARAMETER_PATTERNS.PARAMETER_KEYWORDS, '').trim();
  }

  /**
   * Count numeric values in a string
   */
  private static countNumericValues(content: string): number {
    const matches = content.match(PARAMETER_PATTERNS.NUMERIC);
    return matches ? matches.length : 0;
  }

  /**
   * Process BLOCK matrix line to find diagonal parameters
   */
  private static processBlockMatrixLine(
    line: string,
    lineNum: number,
    state: ScannerState,
    allValuesOnLine: string[]
  ): ParameterLocation[] {
    const locations: ParameterLocation[] = [];
    
    // Figure out which elements these values represent in the overall matrix
    const startElementIndex = state.blockElementsSeen;
    
    // Find which diagonal elements are on this line
    let diagonalsFound = 0;
    
    // Check each position on this line to see if it's a diagonal element
    for (let positionOnLine = 0; positionOnLine < allValuesOnLine.length; positionOnLine++) {
      const absolutePosition = startElementIndex + positionOnLine;
      const parameterIndex = NMTRANMatrixParser.isDiagonalElement(absolutePosition);
      
      if (parameterIndex !== null) {
        // This position contains a diagonal element
        const blockType = state.currentBlockType;
        if (!blockType) continue;
        
        state.counters[blockType]++;
        
        const location: ParameterLocation = {
          type: blockType,
          index: state.counters[blockType],
          line: lineNum
        };
        
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
        
        // Add FIXED keyword ranges to each parameter in the block
        if (state.blockFixedKeywords.length > 0) {
          location.additionalRanges = state.blockFixedKeywords.map(keyword => ({
            startChar: keyword.startChar,
            endChar: keyword.endChar,
            line: keyword.line
          }));
        }
        
        locations.push(location);
        diagonalsFound++;
      }
    }
    
    // Update total elements seen
    state.blockElementsSeen += allValuesOnLine.length;
    
    // Update diagonal count and block remaining  
    state.blockDiagonalsSeen += diagonalsFound;
    state.blockMatrixRemaining -= diagonalsFound;
    
    return locations;
  }

  /**
   * Parse THETA parameter expressions from a line
   * Handles: (0,3), 2 FIXED, (0,.6,1), 10, (-INF,-2.7,0), (37 FIXED), 4 FIX
   */
  private static parseParameterExpressions(line: string): Array<{
    valueRange: { startChar: number; endChar: number };
    fixedRange?: { startChar: number; endChar: number };
  }> {
    const expressions = [];
    
    // Remove control record prefix and comments
    const controlRecordMatch = line.match(/^\s*\$\w+\s*/i);
    const controlRecordLength = controlRecordMatch ? controlRecordMatch[0].length : 0;
    
    // Remove comment part
    const commentIndex = line.indexOf(';');
    const lineWithoutComment = commentIndex !== -1 ? line.substring(0, commentIndex) : line;
    
    // Get content after control record
    const contentWithSpaces = lineWithoutComment.substring(controlRecordLength);
    const content = contentWithSpaces.trim();
    
    // Find where the trimmed content starts in the original line
    const trimmedContentStart = lineWithoutComment.indexOf(content, controlRecordLength);
    let currentPos = trimmedContentStart >= 0 ? trimmedContentStart : controlRecordLength;
    
    let i = 0;
    while (i < content.length) {
      // Skip whitespace
      while (i < content.length && PARAMETER_PATTERNS.WHITESPACE.test(content.charAt(i))) {
        i++;
        currentPos++;
      }
      if (i >= content.length) break;
      
      const startPos = i;
      const absStartPos = currentPos;
      
      if (content.charAt(i) === '(') {
        // Bounded expression: (low,init,up) or (value FIXED)
        let depth = 1;
        i++; // Skip opening paren
        while (i < content.length && depth > 0) {
          if (content.charAt(i) === '(') depth++;
          else if (content.charAt(i) === ')') depth--;
          i++;
        }
        
        const expr = content.substring(startPos, i);
        const fixedMatchInside = expr.match(PARAMETER_PATTERNS.FIXED_CASE_INSENSITIVE);
        
        const expression: {
          valueRange: { startChar: number; endChar: number };
          fixedRange?: { startChar: number; endChar: number };
        } = {
          valueRange: { startChar: absStartPos, endChar: absStartPos + expr.length }
        };
        
        if (fixedMatchInside && fixedMatchInside.index !== undefined) {
          // Has FIXED inside parentheses
          expression.fixedRange = { 
            startChar: absStartPos + fixedMatchInside.index, 
            endChar: absStartPos + fixedMatchInside.index + fixedMatchInside[0].length 
          };
        } else {
          // Check for FIXED keyword after the bounded expression
          // Skip whitespace after the closing parenthesis
          let afterParenPos = i;
          while (afterParenPos < content.length && PARAMETER_PATTERNS.WHITESPACE.test(content.charAt(afterParenPos))) {
            afterParenPos++;
          }
          
          // Check for FIXED/FIX keyword after the bounded expression
          const remainingAfterParen = content.substring(afterParenPos);
          const fixedMatchAfter = remainingAfterParen.match(PARAMETER_PATTERNS.FIXED_START);
          
          if (fixedMatchAfter) {
            expression.fixedRange = {
              startChar: absStartPos + (afterParenPos - startPos),
              endChar: absStartPos + (afterParenPos - startPos) + fixedMatchAfter[0].length
            };
            i = afterParenPos + fixedMatchAfter[0].length;
          }
        }
        
        expressions.push(expression);
      } else {
        // Simple value, possibly followed by FIXED
        // Read until next whitespace or parenthesis
        while (i < content.length && !PARAMETER_PATTERNS.WHITESPACE_OR_PAREN.test(content.charAt(i))) {
          i++;
        }
        
        // Check if followed by FIXED keyword
        const afterValue = i;
        
        // Skip whitespace
        while (i < content.length && PARAMETER_PATTERNS.WHITESPACE.test(content.charAt(i))) {
          i++;
        }
        
        // Check for FIXED/FIX keyword
        const remaining = content.substring(i);
        const fixedMatch = remaining.match(PARAMETER_PATTERNS.FIXED_START);
        
        const expression: {
          valueRange: { startChar: number; endChar: number };
          fixedRange?: { startChar: number; endChar: number };
        } = {
          valueRange: { startChar: absStartPos, endChar: absStartPos + (afterValue - startPos) }
        };
        
        if (fixedMatch) {
          expression.fixedRange = {
            startChar: absStartPos + (i - startPos),
            endChar: absStartPos + (i - startPos) + fixedMatch[0].length
          };
          i += fixedMatch[0].length;
        }
        
        expressions.push(expression);
      }
      
      currentPos = absStartPos + (i - startPos);
    }
    
    return expressions;
  }

  /**
   * Validate sequential parameter numbering (THETA/ETA/EPS must be 1,2,3... with no gaps)
   */
  static validateSequentialNumbering(parameters: ParameterLocation[]): { isValid: boolean; errors: string[] } {
    const errors: string[] = [];
    const groups: Record<'THETA' | 'ETA' | 'EPS', number[]> = { THETA: [], ETA: [], EPS: [] };
    
    // Group parameters by type and collect indices
    for (const param of parameters) {
      groups[param.type].push(param.index);
    }
    
    // Check each parameter type for sequential numbering
    for (const [type, indices] of Object.entries(groups)) {
      if (indices.length === 0) continue;
      
      const sorted = [...indices].sort((a, b) => a - b);
      const expected = Array.from({ length: sorted.length }, (_, i) => i + 1);
      
      for (let i = 0; i < expected.length; i++) {
        if (sorted[i] !== expected[i]) {
          errors.push(`Missing ${type}(${expected[i]}) - parameters must be sequential with no gaps`);
        }
      }
    }
    
    return { isValid: errors.length === 0, errors };
  }

  /**
   * Validate parameter references against definitions (optimized version with pre-scanned parameters)
   */
  static validateParameterReferencesWithParameters(
    document: TextDocument, 
    parameters: ParameterLocation[]
  ): { 
    isValid: boolean; 
    errors: Array<{ message: string; line: number; startChar: number; endChar: number }> 
  } {
    const errors: Array<{ message: string; line: number; startChar: number; endChar: number }> = [];
    const text = document.getText();
    const lines = text.split('\n');
    
    // Use pre-scanned parameters to get counts (no document re-scanning)
    const maxCounts = { THETA: 0, ETA: 0, EPS: 0 };
    
    for (const param of parameters) {
      maxCounts[param.type] = Math.max(maxCounts[param.type], param.index);
    }
    
    // Track which parameters are actually referenced
    const referencedParams = new Set<string>();
    
    // Then scan document for parameter references
    // Reset global regex for reuse
    PARAMETER_PATTERNS.PARAMETER_REFERENCE.lastIndex = 0;
    
    for (let lineNum = 0; lineNum < lines.length; lineNum++) {
      const line = lines[lineNum];
      if (!line) continue;
      
      // Remove comments before scanning for references
      const commentIndex = line.indexOf(';');
      const lineWithoutComment = commentIndex !== -1 ? line.substring(0, commentIndex) : line;
      
      let match;
      while ((match = PARAMETER_PATTERNS.PARAMETER_REFERENCE.exec(lineWithoutComment)) !== null) {
        const rawParamType = match[1] as 'THETA' | 'ETA' | 'EPS' | 'ERR';
        const paramIndex = parseInt(match[2]!, 10);
        
        // Normalize ERR to EPS since they are synonymous in NMTRAN
        const paramType = rawParamType === 'ERR' ? 'EPS' : rawParamType;
        
        // Track this parameter as referenced (use normalized type)
        referencedParams.add(`${paramType}:${paramIndex}`);
        
        if (paramIndex > maxCounts[paramType]) {
          // Use appropriate type name in error message
          const definitionType = paramType === 'EPS' ? 'EPS' : paramType;
          errors.push({
            message: `${rawParamType}(${paramIndex}) referenced but only ${maxCounts[paramType]} ${definitionType} parameters defined`,
            line: lineNum,
            startChar: match.index!,
            endChar: match.index! + match[0].length
          });
        }
      }
    }
    
    // Check for unused parameters
    for (const param of parameters) {
      const key = `${param.type}:${param.index}`;
      if (!referencedParams.has(key)) {
        errors.push({
          message: `${param.type}(${param.index}) defined but never referenced`,
          line: param.line,
          startChar: param.startChar || 0,
          endChar: param.endChar || 0
        });
      }
    }
    
    return { isValid: errors.length === 0, errors };
  }

  /**
   * Validate parameter references against definitions (backward compatibility wrapper)
   */
  static validateParameterReferences(document: TextDocument): { 
    isValid: boolean; 
    errors: Array<{ message: string; line: number; startChar: number; endChar: number }> 
  } {
    // Use optimized version with fresh parameter scan
    const parameters = this.scanDocument(document);
    return this.validateParameterReferencesWithParameters(document, parameters);
  }

  /**
   * Find the position of a parameter value
   * Delegates to appropriate finder based on parameter type
   */
  private static findParameterValuePosition(
    line: string,
    _lineNum: number,
    _paramType: 'THETA' | 'ETA' | 'EPS',
    paramIndex: number,
    inBlockMatrix: boolean,
    _positionInLine: number,
    _document: TextDocument
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
      PARAMETER_PATTERNS.NUMERIC.lastIndex = 0; // Reset global regex
      const matches = [];
      let match;
      
      while ((match = PARAMETER_PATTERNS.NUMERIC.exec(searchText)) !== null) {
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
      const match = searchText.match(PARAMETER_PATTERNS.NUMERIC_SINGLE);
      
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

  /**
   * Validate BLOCK matrix syntax and structure
   */
  static validateBlockMatrixSyntax(document: TextDocument): { 
    isValid: boolean; 
    errors: Array<{ message: string; line: number; startChar: number; endChar: number }> 
  } {
    const errors: Array<{ message: string; line: number; startChar: number; endChar: number }> = [];
    const lines = document.getText().split('\n');
    let currentBlockType: 'OMEGA' | 'SIGMA' | null = null;
    let currentBlockSize = 0;
    let currentBlockStartLine = 0;
    let expectedElements = 0;
    let actualElements = 0;
    let inBlockMatrix = false;

    for (let lineNum = 0; lineNum < lines.length; lineNum++) {
      const line = lines[lineNum];
      if (!line) continue;
      
      const trimmed = line.trim();
      if (trimmed.startsWith(';')) continue; // Skip comments
      
      // Remove inline comments
      const commentIndex = trimmed.indexOf(';');
      const lineWithoutComment = commentIndex !== -1 ? trimmed.substring(0, commentIndex).trim() : trimmed;
      
      // Check for OMEGA/SIGMA BLOCK declarations
      const omegaBlockMatch = lineWithoutComment.match(/^\$OMEGA\s+BLOCK\((\d+)\)/i);
      const sigmaBlockMatch = lineWithoutComment.match(/^\$SIGMA\s+BLOCK\((\d+)\)/i);
      
      if (omegaBlockMatch || sigmaBlockMatch) {
        const blockMatch = omegaBlockMatch || sigmaBlockMatch;
        if (!blockMatch || !blockMatch[1]) continue;
        const blockSize = parseInt(blockMatch[1], 10);
        currentBlockType = omegaBlockMatch ? 'OMEGA' : 'SIGMA';
        currentBlockSize = blockSize;
        currentBlockStartLine = lineNum;
        inBlockMatrix = true;
        actualElements = 0;
        
        // Validate BLOCK size
        if (blockSize < 1) {
          errors.push({
            message: `Invalid BLOCK size: BLOCK(${blockSize}). Size must be >= 1`,
            line: lineNum,
            startChar: blockMatch!.index! + blockMatch![0].indexOf(`(${blockSize})`),
            endChar: blockMatch!.index! + blockMatch![0].indexOf(`(${blockSize})`) + `(${blockSize})`.length
          });
          continue;
        }
        
        // BLOCK(1) is valid NONMEM syntax - no warning needed
        
        // Calculate expected elements for symmetric matrix: n*(n+1)/2
        expectedElements = (blockSize * (blockSize + 1)) / 2;
        
        // Check for SAME keyword on declaration line
        if (PARAMETER_PATTERNS.SAME.test(lineWithoutComment)) {
          // SAME is valid - skip element counting for this block
          inBlockMatrix = false;
          continue;
        }
        
        // Check for inline values on BLOCK declaration line
        const afterBlock = lineWithoutComment.replace(/^\$\w+\s+BLOCK\(\d+\)\s*/i, '');
        if (afterBlock.trim().length > 0) {
          const inlineValues = afterBlock.match(PARAMETER_PATTERNS.NUMERIC);
          if (inlineValues) {
            actualElements += inlineValues.length;
          }
        }
        
      } else if (inBlockMatrix && currentBlockType) {
        // Processing matrix data lines
        if (lineWithoutComment.startsWith('$')) {
          // New control record - validate current block
          this.validateBlockElementCount(currentBlockType, currentBlockSize, expectedElements, actualElements, currentBlockStartLine, errors);
          inBlockMatrix = false;
          currentBlockType = null;
        } else {
          // Count numeric values on this line
          const numericValues = lineWithoutComment.match(PARAMETER_PATTERNS.NUMERIC);
          if (numericValues) {
            actualElements += numericValues.length;
          }
        }
      }
    }
    
    // Validate final block if document ends while in a block
    if (inBlockMatrix && currentBlockType) {
      this.validateBlockElementCount(currentBlockType, currentBlockSize, expectedElements, actualElements, currentBlockStartLine, errors);
    }
    
    return { isValid: errors.length === 0, errors };
  }

  /**
   * Validate BLOCK matrix element count
   */
  private static validateBlockElementCount(
    blockType: 'OMEGA' | 'SIGMA',
    blockSize: number,
    expectedElements: number,
    actualElements: number,
    startLine: number,
    errors: Array<{ message: string; line: number; startChar: number; endChar: number }>
  ): void {
    if (blockSize === 1) {
      // BLOCK(1) should have exactly 1 element
      if (actualElements !== 1) {
        errors.push({
          message: `${blockType} BLOCK(1) expects 1 element, found ${actualElements}`,
          line: startLine,
          startChar: 0,
          endChar: 0
        });
      }
    } else {
      // True matrix blocks
      if (actualElements < expectedElements) {
        errors.push({
          message: `${blockType} BLOCK(${blockSize}) incomplete: expected ${expectedElements} elements, found ${actualElements}`,
          line: startLine,
          startChar: 0,
          endChar: 0
        });
      } else if (actualElements > expectedElements) {
        errors.push({
          message: `${blockType} BLOCK(${blockSize}) has too many elements: expected ${expectedElements}, found ${actualElements}`,
          line: startLine,
          startChar: 0,
          endChar: 0
        });
      }
    }
  }

  /**
   * Validate SAME keyword usage in BLOCK context
   */
  static validateSameKeywordUsage(document: TextDocument): { 
    isValid: boolean; 
    errors: Array<{ message: string; line: number; startChar: number; endChar: number }> 
  } {
    const errors: Array<{ message: string; line: number; startChar: number; endChar: number }> = [];
    const lines = document.getText().split('\n');
    
    for (let lineNum = 0; lineNum < lines.length; lineNum++) {
      const line = lines[lineNum];
      if (!line) continue;
      
      const trimmed = line.trim();
      if (trimmed.startsWith(';')) continue; // Skip comments
      
      // Remove inline comments
      const commentIndex = trimmed.indexOf(';');
      const lineWithoutComment = commentIndex !== -1 ? trimmed.substring(0, commentIndex).trim() : trimmed;
      
      // Check for SAME keyword usage
      if (PARAMETER_PATTERNS.SAME.test(lineWithoutComment)) {
        const sameMatch = lineWithoutComment.match(PARAMETER_PATTERNS.SAME);
        if (sameMatch) {
          // Check if SAME is used in a valid BLOCK context
          const isInBlockDeclaration = /^\$\w+\s+BLOCK\(\d+\)\s+.*SAME/i.test(lineWithoutComment);
          const isStandaloneBlock = /^\$\w+\s+BLOCK\(\d+\)\s+SAME\s*$/i.test(lineWithoutComment);
          
          if (!isInBlockDeclaration && !isStandaloneBlock) {
            // SAME used outside proper BLOCK context
            const startPos = line.indexOf(sameMatch[0]);
            errors.push({
              message: `SAME keyword should only be used with BLOCK matrices to reference previous block structure`,
              line: lineNum,
              startChar: startPos,
              endChar: startPos + sameMatch[0].length
            });
          }
        }
      }
    }
    
    return { isValid: errors.length === 0, errors };
  }

  /**
   * Validate parameter bounds for THETA, OMEGA, and SIGMA parameters
   */
  static validateParameterBounds(document: TextDocument): { 
    isValid: boolean; 
    errors: Array<{ message: string; line: number; startChar: number; endChar: number }> 
  } {
    const errors: Array<{ message: string; line: number; startChar: number; endChar: number }> = [];
    const lines = document.getText().split('\n');
    let currentBlockType: 'THETA' | 'OMEGA' | 'SIGMA' | null = null;

    for (let lineNum = 0; lineNum < lines.length; lineNum++) {
      const line = lines[lineNum];
      if (!line) continue;
      
      const trimmed = line.trim();
      if (trimmed.startsWith(';')) continue; // Skip comments
      
      // Remove inline comments
      const commentIndex = trimmed.indexOf(';');
      const lineWithoutComment = commentIndex !== -1 ? trimmed.substring(0, commentIndex).trim() : trimmed;
      
      // Detect parameter block type
      if (PARAMETER_PATTERNS.THETA.test(lineWithoutComment)) {
        currentBlockType = 'THETA';
      } else if (PARAMETER_PATTERNS.OMEGA.test(lineWithoutComment)) {
        currentBlockType = 'OMEGA';
      } else if (PARAMETER_PATTERNS.SIGMA.test(lineWithoutComment)) {
        currentBlockType = 'SIGMA';
      } else if (lineWithoutComment.startsWith('$')) {
        currentBlockType = null; // Different control record
      }
      
      // Validate bounds if in a parameter block
      if (currentBlockType && lineWithoutComment.includes('(')) {
        const boundExpressions = this.extractBoundExpressions(lineWithoutComment);
        
        for (const expr of boundExpressions) {
          const validation = this.validateSingleParameterBound(expr.text, currentBlockType);
          if (!validation.isValid) {
            for (const error of validation.errors) {
              const absoluteStart = line.indexOf(expr.text, expr.startPos);
              if (absoluteStart !== -1) {
                errors.push({
                  message: error,
                  line: lineNum,
                  startChar: absoluteStart,
                  endChar: absoluteStart + expr.text.length
                });
              }
            }
          }
        }
      }
    }
    
    return { isValid: errors.length === 0, errors };
  }

  /**
   * Extract bound expressions from a parameter line
   */
  private static extractBoundExpressions(line: string): Array<{ text: string; startPos: number }> {
    const expressions = [];
    let i = 0;
    
    // Remove control record prefix
    const controlMatch = line.match(/^\s*\$\w+\s*/i);
    if (controlMatch) {
      i = controlMatch[0].length;
    }
    
    // Skip BLOCK declarations
    const blockMatch = line.substring(i).match(/^BLOCK\(\d+\)\s*/i);
    if (blockMatch) {
      i += blockMatch[0].length;
    }
    
    // Find parenthesized expressions
    while (i < line.length) {
      if (line.charAt(i) === '(') {
        const startPos = i;
        let depth = 1;
        i++; // Skip opening paren
        
        while (i < line.length && depth > 0) {
          if (line.charAt(i) === '(') depth++;
          else if (line.charAt(i) === ')') depth--;
          i++;
        }
        
        if (depth === 0) {
          const expr = line.substring(startPos, i);
          expressions.push({ text: expr, startPos });
        }
      } else {
        i++;
      }
    }
    
    return expressions;
  }

  /**
   * Validate a single parameter bound expression
   */
  private static validateSingleParameterBound(
    boundExpr: string, 
    paramType: 'THETA' | 'OMEGA' | 'SIGMA'
  ): { isValid: boolean; errors: string[] } {
    const errors: string[] = [];
    
    // Remove outer parentheses
    const content = boundExpr.substring(1, boundExpr.length - 1).trim();
    
    // Split by commas (but not inside nested parentheses)
    const parts = this.splitBoundComponents(content);
    
    if (parts.length === 1) {
      // Single value: (init) - validate the value
      const firstPart = parts[0];
      if (firstPart) {
        const value = this.parseNumericValue(firstPart.trim());
        if (value === null) {
          errors.push(`Invalid ${paramType} value: ${firstPart.trim()}`);
        }
      }
    } else if (parts.length === 3) {
      // Three values: (low, init, high)
      const lowStr = parts[0]?.trim() || '';
      const initStr = parts[1]?.trim() || '';
      const highStr = parts[2]?.trim() || '';
      
      const low = this.parseNumericValue(lowStr);
      const init = this.parseNumericValue(initStr);
      const high = this.parseNumericValue(highStr);
      
      // Validate individual values
      if (low === null && !this.isInfinity(lowStr)) {
        errors.push(`Invalid lower bound: ${lowStr}`);
      }
      // Initial value can be empty (omitted) only for THETA: (lower,,upper)
      // OMEGA and SIGMA always require initial values
      if (init === null && (paramType !== 'THETA' || initStr !== '')) {
        errors.push(`Invalid initial value: ${initStr}`);
      }
      if (high === null && !this.isInfinity(highStr)) {
        errors.push(`Invalid upper bound: ${highStr}`);
      }
      
      // Validate bound relationships
      // Always check lower vs upper bound
      if (low !== null && high !== null && low > high) {
        errors.push(`Lower bound (${low}) cannot be greater than upper bound (${high})`);
      }
      
      // Check initial value bounds only if initial value is provided
      if (init !== null) {
        if (low !== null && low > init) {
          errors.push(`Lower bound (${low}) cannot be greater than initial value (${init})`);
        }
        if (high !== null && init > high) {
          errors.push(`Initial value (${init}) cannot be greater than upper bound (${high})`);
        }
        
        // Special validation for OMEGA/SIGMA (variance parameters should generally be positive)
        if ((paramType === 'OMEGA' || paramType === 'SIGMA') && init < 0) {
          errors.push(`${paramType} initial value (${init}) should generally be positive (variance parameter)`);
        }
      }
      
    } else if (parts.length !== 2) {
      // Invalid number of components
      errors.push(`Invalid bound format: expected (value), (low,init,high), found ${parts.length} components`);
    }
    
    return { isValid: errors.length === 0, errors };
  }

  /**
   * Split bound components by commas, respecting nested parentheses
   */
  private static splitBoundComponents(content: string): string[] {
    const parts = [];
    let current = '';
    let depth = 0;
    
    for (let i = 0; i < content.length; i++) {
      const char = content.charAt(i);
      
      if (char === '(') {
        depth++;
        current += char;
      } else if (char === ')') {
        depth--;
        current += char;
      } else if (char === ',' && depth === 0) {
        parts.push(current);
        current = '';
      } else {
        current += char;
      }
    }
    
    if (current.trim()) {
      parts.push(current);
    }
    
    return parts;
  }

  /**
   * Parse a numeric value, handling NONMEM special values
   */
  private static parseNumericValue(valueStr: string): number | null {
    const trimmed = valueStr.trim();
    
    // Handle infinity values
    if (this.isInfinity(trimmed)) {
      return trimmed.startsWith('-') ? -Infinity : Infinity;
    }
    
    // Try to parse as number
    const num = parseFloat(trimmed);
    return isNaN(num) ? null : num;
  }

  /**
   * Check if a string represents infinity
   */
  private static isInfinity(valueStr: string): boolean {
    const trimmed = valueStr.trim().toUpperCase();
    return trimmed === 'INF' || trimmed === '-INF' || trimmed === '+INF' || 
           trimmed === 'INFINITY' || trimmed === '-INFINITY' || trimmed === '+INFINITY';
  }
}