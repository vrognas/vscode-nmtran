/**
 * Definition and Reference Service - Simplified Navigation
 * 
 * Provides basic "Go to Definition" and "Find All References" for NMTRAN parameters.
 * Simple implementation focused on core navigation functionality.
 */

import { Connection, Location, Position } from 'vscode-languageserver/node';
import { TextDocument } from 'vscode-languageserver-textdocument';
import { NMTRANMatrixParser } from '../utils/NMTRANMatrixParser';
import { ParameterScanner } from './ParameterScanner';
import { PerformanceMonitor } from '../utils/performanceMonitor';

// Constants for consistent parameter pattern matching
const PARAMETER_PATTERNS = {
  THETA: /^\$THETA(\s|$)/i,
  OMEGA: /^\$OMEGA(\s|$)/i, 
  SIGMA: /^\$SIGMA(\s|$)/i,
  BLOCK: /BLOCK\((\d+)\)/i,
  SAME: /\bSAME\b/i,
  PARAMETER_USAGE_SOURCE: '\\b(THETA|ETA|EPS|ERR)\\((\\d+)\\)' // Source pattern without flags
} as const;

// Factory function to create fresh regex instances to avoid state contamination
const createParameterUsageRegex = () => new RegExp(PARAMETER_PATTERNS.PARAMETER_USAGE_SOURCE, 'gi');


type ParameterType = 'THETA' | 'ETA' | 'EPS';

type CharacterRange = {
  start: number;
  end: number;
};

interface ParameterLocation {
  type: ParameterType;
  index: number;
  line: number;
  startChar?: number;  // Start character position of the specific value
  endChar?: number;    // End character position of the specific value
  additionalRanges?: Array<{ startChar: number; endChar: number; line?: number }>; // For FIXED keyword, etc.
}

interface ParameterInfo {
  type: string;
  index: number;
}

export class DefinitionService {
  private connection: Connection;
  private scanCache = new Map<string, ParameterLocation[]>();
  private performanceMonitor: PerformanceMonitor;

  constructor(connection: Connection) {
    this.connection = connection;
    this.performanceMonitor = new PerformanceMonitor(connection);
  }

  /**
   * Provides definition location for NMTRAN parameters
   * THETA(3) → jumps to 3rd $THETA line
   * ETA(2) → jumps to line defining 2nd ETA parameter  
   * EPS(1) → jumps to line defining 1st EPS parameter
   * For SAME constraints, shows both the SAME line and the referenced value
   */
  async provideDefinition(document: TextDocument, position: Position): Promise<Location[] | null> {
    return this.performanceMonitor.measure(
      'provideDefinition',
      async () => {
        try {
          const parameter = this.getParameterAtPosition(document, position);
          if (!parameter) {
            return null;
          }

          const definitionLocations = this.findAllDefinitionLocations(document, parameter);
          
          return definitionLocations.length > 0 ? definitionLocations : null;

        } catch (error) {
          this.connection.console.error(`❌ Error in definition provider: ${error}`);
          return null;
        }
      },
      { documentUri: document.uri, position }
    );
  }

  /**
   * Provides all reference locations for NMTRAN parameters
   * Shows everywhere THETA(3), ETA(2), etc. is used in the document
   */
  provideReferences(document: TextDocument, position: Position, includeDeclaration: boolean): Location[] | null {
    try {
      const parameter = this.getParameterAtPosition(document, position);
      if (!parameter) {
        return null;
      }

      const references = this.findAllReferences(document, parameter, includeDeclaration);
      return references;

    } catch (error) {
      this.connection.console.error(`❌ Error in references provider: ${error}`);
      return null;
    }
  }


  /**
   * Extracts parameter info from cursor position
   * Returns parameter info if cursor is on:
   * 1. A THETA(n), ETA(n), or EPS(n) pattern, OR  
   * 2. A parameter definition line ($THETA, $OMEGA, $SIGMA)
   */
  private getParameterAtPosition(document: TextDocument, position: Position): ParameterInfo | null {
    const line = document.getText({
      start: { line: position.line, character: 0 },
      end: { line: position.line, character: Number.MAX_VALUE }
    });

    // First, try to match THETA(1), ETA(2), EPS(3) patterns - numeric indices only
    // Create fresh RegExp instance to avoid shared state issues with global flag
    const parameterRegex = createParameterUsageRegex();
    let match: RegExpExecArray | null;

    while ((match = parameterRegex.exec(line)) !== null) {
      const start = match.index;
      const end = match.index + match[0].length;
      
      if (position.character >= start && position.character <= end) {
        const rawType = match[1]!.toUpperCase();
        // Map ERR to EPS for consistency
        const mappedType = rawType === 'ERR' ? 'EPS' : rawType;
        return {
          type: mappedType,
          index: parseInt(match[2]!, 10)
        };
      }
    }

    // If not on a parameter usage, check if on a definition line
    const trimmedLine = line.trim();
    if (this.isDefinitionLine(trimmedLine, 'THETA') || 
        this.isDefinitionLine(trimmedLine, 'ETA') || 
        this.isDefinitionLine(trimmedLine, 'EPS')) {
      
      return this.getParameterFromDefinitionLine(document, position.line, trimmedLine, position.character);
    }
    
    // Check if this is a continuation line within a parameter block
    return this.getParameterFromContinuationLine(document, position.line);
  }

  /**
   * Simple scanner that maps all parameter definitions in the document
   * Handles multi-line THETA/OMEGA/SIGMA blocks
   * Results are cached for performance
   */
  private scanAllParameters(document: TextDocument): ParameterLocation[] {
    // Check cache first
    const cacheKey = `${document.uri}:${document.version}`;
    const cached = this.scanCache.get(cacheKey);
    if (cached) {
      return cached;
    }
    
    // Use ParameterScanner for the actual scanning logic
    const locations = ParameterScanner.scanDocument(document);
    
    this.enhanceLocationsWithValuePositions(document, locations);
    
    // Cache the result
    this.scanCache.set(cacheKey, locations);
    
    // Limit cache size
    if (this.scanCache.size > 50) {
      const firstKey = this.scanCache.keys().next().value;
      if (firstKey) {
        this.scanCache.delete(firstKey);
      }
    }
    
    return locations;
  }

  /**
   * Enhance parameter locations with precise value positions
   * This is the refactored logic that was previously inline
   */
  private enhanceLocationsWithValuePositions(document: TextDocument, locations: ParameterLocation[]): void {
    const lines = document.getText().split('\n');
    
    for (const location of locations) {
      // Skip if positions are already set by ParameterScanner
      if (location.startChar !== undefined && location.endChar !== undefined) {
        continue;
      }
      
      const line = lines[location.line];
      if (!line) continue;
      
      // Find precise value position based on parameter type
      let valuePosition;
      if (location.type === 'THETA') {
        // For THETA, determine position within the line based on parameter order
        const paramPosition = this.getParameterPositionInLine(document, location.line, location.index, 'THETA');
        valuePosition = this.findThetaInitialValue(line, paramPosition);
      } else {
        // For ETA/EPS, determine the position within the specific BLOCK
        // For BLOCK(1), it's always position 1
        // For BLOCK(n), we need to calculate which diagonal element this represents
        const paramPositionInBlock = this.calculateParameterPositionInBlock(document, location.line, location.index);
        
        valuePosition = this.findOmegaParameterValue(
          document, 
          location.line, 
          paramPositionInBlock, 
          false // Let findOmegaParameterValue determine block matrix state internally
        );
      }
      
      if (valuePosition) {
        location.startChar = valuePosition.start;
        location.endChar = valuePosition.end;
      }
    }
  }

  /**
   * Get the position of a parameter within its definition line
   * For multi-parameter lines, determines which parameter on the line this refers to
   */
  private getParameterPositionInLine(document: TextDocument, lineNum: number, paramIndex: number, paramType: string): number {
    const allParams = this.scanAllParameters(document);
    
    // Find all parameters of the same type defined on or before this line
    const sameTypeParams = allParams.filter(param => 
      param.type === paramType && param.line <= lineNum
    );
    
    // Count how many parameters of this type are on the same line
    const paramsOnThisLine = sameTypeParams.filter(param => param.line === lineNum);
    
    // Find the position of our parameter within this line
    for (let i = 0; i < paramsOnThisLine.length; i++) {
      if (paramsOnThisLine[i]?.index === paramIndex) {
        return i + 1; // Return 1-based position
      }
    }
    
    return 1; // Default to first parameter on line
  }

  /**
   * Calculate the position of a parameter within its BLOCK
   * For BLOCK(1), always returns 1
   * For BLOCK(n), calculates which diagonal position this parameter represents
   */
  private calculateParameterPositionInBlock(document: TextDocument, lineNum: number, globalParamIndex: number): number {
    const lines = document.getText().split('\n');
    const line = lines[lineNum];
    if (!line) return 1;
    
    const trimmed = line.trim();
    const blockMatch = trimmed.match(PARAMETER_PATTERNS.BLOCK);
    
    if (blockMatch && blockMatch[1]) {
      const blockSize = parseInt(blockMatch[1], 10);
      
      if (blockSize === 1) {
        // For BLOCK(1), there's only one parameter, so position is always 1
        return 1;
      } else {
        // For BLOCK(n>1), we need to figure out which diagonal element this is
        // First, find the first parameter defined by this BLOCK
        const allParams = this.scanAllParameters(document);
        const blockStartParam = allParams.find(param => 
          param.line === lineNum && param.type === 'ETA'
        );
        
        if (blockStartParam) {
          // Calculate position within the block (1-based)
          const positionInBlock = globalParamIndex - blockStartParam.index + 1;
          return positionInBlock;
        }
      }
    }
    
    return 1; // Default to position 1
  }

  /**
   * Find the character position of the diagonal element in a BLOCK matrix line
   * For ETA(n) or EPS(n), find the nth numeric value on the line (the diagonal element)
   */
  private findDiagonalElementPosition(line: string, paramIndex: number): CharacterRange | null {
    // Remove comment part
    const codePartEnd = line.indexOf(';');
    const codePart = codePartEnd !== -1 ? line.substring(0, codePartEnd) : line;
    
    // Find all numeric values (including scientific notation)
    const numericPattern = /[\d\-+][\d\-+.eE]*/g;
    const matches = [];
    let match;
    
    while ((match = numericPattern.exec(codePart)) !== null) {
      matches.push({
        value: match[0],
        start: match.index,
        end: match.index + match[0].length
      });
    }
    
    // The diagonal element is the paramIndex-th numeric value on this line
    if (matches.length >= paramIndex && paramIndex > 0) {
      const diagonalMatch = matches[paramIndex - 1]; // Convert to 0-based index
      if (diagonalMatch) {
        return {
          start: diagonalMatch.start,
          end: diagonalMatch.end
        };
      }
    }
    
    return null;
  }

  /**
   * Find diagonal element in a text string with a given offset
   * Similar to findDiagonalElementPosition but works on a substring
   */
  private findDiagonalElementInText(text: string, paramIndex: number, startOffset: number): CharacterRange | null {
    // Remove comment part
    const codePartEnd = text.indexOf(';');
    const codePart = codePartEnd !== -1 ? text.substring(0, codePartEnd) : text;
    
    // Find all numeric values (including scientific notation)
    const numericPattern = /[\d\-+][\d\-+.eE]*/g;
    const matches = [];
    let match;
    
    while ((match = numericPattern.exec(codePart)) !== null) {
      matches.push({
        value: match[0],
        start: match.index + startOffset,
        end: match.index + match[0].length + startOffset
      });
    }
    
    // Use NMTRANMatrixParser to get the correct diagonal position
    const diagonalPosition = NMTRANMatrixParser.getDiagonalPosition(paramIndex);
    
    if (matches.length > diagonalPosition) {
      const diagonalMatch = matches[diagonalPosition];
      if (diagonalMatch) {
        return { start: diagonalMatch.start, end: diagonalMatch.end };
      }
    }
    
    return null;
  }

  /**
   * Find the character position of a parameter value in a regular OMEGA/SIGMA line
   * For the nth parameter on a line, find the nth numeric value
   */
  private findParameterValuePosition(line: string, paramPosition: number): CharacterRange | null {
    // Remove comment part
    const codePartEnd = line.indexOf(';');
    const codePart = codePartEnd !== -1 ? line.substring(0, codePartEnd) : line;
    
    // Remove control record prefix (e.g., $OMEGA) and BLOCK(n) pattern if present
    let contentPart = codePart.replace(/^\s*\$\w+\s*/i, '');
    contentPart = contentPart.replace(/^BLOCK\(\d+\)\s*/i, ''); // Remove BLOCK(n) to avoid matching the number
    
    // Find all numeric values (including scientific notation)
    const numericPattern = /[\d\-+][\d\-+.eE]*/g;
    const matches = [];
    let match;
    
    const searchOffset = codePart.length - contentPart.length; // Offset for prefix removal
    
    while ((match = numericPattern.exec(contentPart)) !== null) {
      matches.push({
        value: match[0],
        start: match.index + searchOffset,
        end: match.index + match[0].length + searchOffset
      });
    }
    
    // For BLOCK matrices, we need to find diagonal elements, not sequential values
    // Check if this line contains a BLOCK pattern
    const isBlockLine = /BLOCK\(\d+\)/i.test(codePart);
    
    if (isBlockLine) {
      // For BLOCK matrices, use NMTRANMatrixParser to calculate diagonal position
      const diagonalPosition = NMTRANMatrixParser.getDiagonalPosition(paramPosition);
      
      if (matches.length > diagonalPosition) {
        const diagonalMatch = matches[diagonalPosition];
        if (diagonalMatch) {
          return { start: diagonalMatch.start, end: diagonalMatch.end };
        }
      }
    }
    
    // Return the paramPosition-th numeric value for regular cases
    if (matches.length >= paramPosition && paramPosition > 0) {
      const targetMatch = matches[paramPosition - 1]; // Convert to 0-based index
      if (targetMatch) {
        return {
          start: targetMatch.start,
          end: targetMatch.end
        };
      }
    }
    
    return null;
  }

  /**
   * Find the initial value position for a THETA parameter, handling bounded syntax
   * Supports: (low, init, up), (low, init), init, and edge case (low, , up)
   */
  private findThetaInitialValue(line: string, paramPosition: number): CharacterRange | null {
    // Remove comment part
    const codePartEnd = line.indexOf(';');
    const codePart = codePartEnd !== -1 ? line.substring(0, codePartEnd) : line;
    
    // Remove control record prefix (e.g., $THETA)
    const contentPart = codePart.replace(/^\s*\$\w+\s*/i, '');
    const searchOffset = codePart.length - contentPart.length;
    
    // Find all parameter expressions (bounded or standalone)
    const paramExpressions = this.extractThetaExpressions(contentPart, searchOffset);
    
    if (paramExpressions.length >= paramPosition && paramPosition > 0) {
      const targetExpression = paramExpressions[paramPosition - 1];
      
      if (targetExpression && targetExpression.type === 'bounded') {
        // Parse bounded expression like (low, init, up)
        return this.findInitialValueInBoundedExpression(targetExpression);
      } else if (targetExpression) {
        // Standalone value
        return {
          start: targetExpression.start,
          end: targetExpression.end
        };
      }
    }
    
    return null;
  }

  /**
   * Extract THETA parameter expressions from a line, handling both bounded and standalone values
   */
  private extractThetaExpressions(content: string, offset: number): Array<{
    type: 'bounded' | 'standalone';
    text: string;
    start: number;
    end: number;
  }> {
    const expressions = [];
    let i = 0;
    
    while (i < content.length) {
      // Skip whitespace
      while (i < content.length && /\s/.test(content.charAt(i))) i++;
      if (i >= content.length) break;
      
      if (content[i] === '(') {
        // Find matching closing parenthesis
        let depth = 1;
        const start = i;
        i++; // Skip opening paren
        
        while (i < content.length && depth > 0) {
          if (content[i] === '(') depth++;
          else if (content[i] === ')') depth--;
          i++;
        }
        
        if (depth === 0) {
          expressions.push({
            type: 'bounded' as const,
            text: content.substring(start, i),
            start: start + offset,
            end: i + offset
          });
        }
      } else {
        // Standalone value (number, keyword like FIX, etc.)
        const start = i;
        while (i < content.length && !/\s/.test(content.charAt(i))) i++;
        
        const text = content.substring(start, i);
        // Only include if it looks like a parameter value (not keywords like FIX)
        if (!/^(FIX|FIXED)$/i.test(text)) {
          expressions.push({
            type: 'standalone' as const,
            text: text,
            start: start + offset,
            end: i + offset
          });
        }
      }
    }
    
    return expressions;
  }

  /**
   * Find the initial value within a bounded expression like (low, init, up)
   */
  private findInitialValueInBoundedExpression(expression: { text: string; start: number; end: number }): { start: number; end: number } | null {
    // Remove parentheses
    const innerContent = expression.text.slice(1, -1); // Remove ( and )
    const parts = innerContent.split(',');
    
    if (parts.length >= 2) {
      // Standard cases: (low, init) or (low, init, up)
      const initPart = parts[1]?.trim();
      if (initPart && initPart !== '') {
        // Find position of initial value within the bounded expression
        const initStart = expression.text.indexOf(initPart, parts[0]?.length || 0);
        if (initStart !== -1) {
          return {
            start: expression.start + initStart,
            end: expression.start + initStart + initPart.length
          };
        }
      } else {
        // Edge case: (low, , up) - no initial value, return entire expression
        return {
          start: expression.start,
          end: expression.end
        };
      }
    } else if (parts.length === 1) {
      // Single value in parentheses: (init)
      const initPart = parts[0]?.trim();
      if (initPart) {
        return {
          start: expression.start + 1, // Skip opening paren
          end: expression.start + 1 + initPart.length
        };
      }
    }
    
    // Fallback: return entire bounded expression
    return {
      start: expression.start,
      end: expression.end
    };
  }

  /**
   * Find the parameter value for an OMEGA or SIGMA parameter, handling various matrix syntax patterns
   * Supports BLOCK matrices, SAME constraints, and regular inline values
   */
  private findOmegaParameterValue(document: TextDocument, lineNum: number, paramIndex: number, inBlockMatrix?: boolean): CharacterRange | null {
    const lines = document.getText().split('\n');
    const line = lines[lineNum];
    if (!line) return null;
    
    const trimmed = line.trim();
    
    // Determine block matrix state if not provided
    if (inBlockMatrix === undefined) {
      inBlockMatrix = PARAMETER_PATTERNS.BLOCK.test(trimmed);
    }
    
    // Check for SAME keyword - return position of SAME keyword itself
    if (PARAMETER_PATTERNS.SAME.test(trimmed)) {
      return this.findSameKeywordPosition(line);
    }
    
    // Check if this is a BLOCK line
    if (PARAMETER_PATTERNS.BLOCK.test(trimmed)) {
      // First check if there are values on the same line as the BLOCK declaration
      const afterBlock = trimmed.replace(/^\$OMEGA\s+BLOCK\(\d+\)\s*/i, '').replace(/^\$SIGMA\s+BLOCK\(\d+\)\s*/i, '');
      const hasInlineValues = afterBlock.trim().length > 0 && !/^;/.test(afterBlock.trim());
      
      if (hasInlineValues) {
        // BLOCK with inline values - find the diagonal element in the afterBlock portion
        const blockStartIndex = line.indexOf(afterBlock.trim());
        return this.findDiagonalElementInText(afterBlock, paramIndex, blockStartIndex);
      } else if (inBlockMatrix) {
        // BLOCK without inline values - look for value on next non-comment line
        for (let i = lineNum + 1; i < lines.length; i++) {
          const nextLine = lines[i];
          if (!nextLine) continue;
          
          const nextTrimmed = nextLine.trim();
          if (nextTrimmed.startsWith(';') || nextTrimmed.length === 0) continue;
          if (nextTrimmed.startsWith('$')) break; // Hit next control record
          
          // This should be our diagonal element
          return this.findDiagonalElementPosition(nextLine, paramIndex);
        }
        return null;
      }
    }
    
    // Regular OMEGA/SIGMA with inline value
    // For BLOCK(1) matrices, there's only one value on the line - always position 1
    // For multi-value BLOCK matrices, this was handled above
    // For regular OMEGA/SIGMA lines, find the first parameter
    return this.findParameterValuePosition(line, 1);
  }

  /**
   * Find the position of the SAME keyword in a line
   */
  private findSameKeywordPosition(line: string): CharacterRange | null {
    const match = line.match(PARAMETER_PATTERNS.SAME);
    if (match && match.index !== undefined) {
      return {
        start: match.index,
        end: match.index + match[0].length
      };
    }
    return null;
  }


  /**
   * Check if current line is a parameter continuation line and get parameter info
   */
  private getParameterFromContinuationLine(document: TextDocument, lineNum: number): ParameterInfo | null {
    const allParams = this.scanAllParameters(document);
    
    // Find any parameter defined on this line
    for (const param of allParams) {
      if (param.line === lineNum) {
        return {
          type: param.type,
          index: param.index
        };
      }
    }
    
    return null;
  }

  /**
   * Determines which parameter is defined on a given definition line
   * by counting preceding definition lines of the same type and cursor position
   */
  private getParameterFromDefinitionLine(document: TextDocument, lineNum: number, line: string, cursorChar?: number): ParameterInfo | null {
    let parameterType: string;
    
    if (this.isDefinitionLine(line, 'THETA')) {
      parameterType = 'THETA';
    } else if (this.isDefinitionLine(line, 'ETA')) {
      parameterType = 'ETA';  
    } else if (this.isDefinitionLine(line, 'EPS')) {
      parameterType = 'EPS';
    } else {
      return null;
    }

    // Count preceding definition lines of the same type to determine parameter index
    const lines = document.getText().split('\n');
    let parameterCount = 0;

    for (let i = 0; i <= lineNum; i++) {
      const currentLine = lines[i];
      if (!currentLine) continue;
      
      const trimmedCurrentLine = currentLine.trim();
      if (trimmedCurrentLine.startsWith(';') || trimmedCurrentLine.length === 0) {
        continue;
      }

      if (this.isDefinitionLine(trimmedCurrentLine, parameterType)) {
        const parametersInLine = this.countParametersInLine(trimmedCurrentLine, parameterType);
        
        if (i === lineNum) {
          // This is our target line - determine which specific parameter based on cursor position
          if (cursorChar !== undefined) {
            const parameterIndex = this.getParameterIndexFromCursorPosition(document, lineNum, cursorChar, parameterType, parameterCount);
            if (parameterIndex !== null) {
              return {
                type: parameterType,
                index: parameterIndex
              };
            }
          }
          
          // Fallback: return the first parameter it defines if cursor position not provided
          return {
            type: parameterType,
            index: parameterCount + 1
          };
        }
        
        parameterCount += parametersInLine;
      }
    }

    return null;
  }

  /**
   * Determine which parameter index the cursor is positioned on based on parameter locations
   */
  private getParameterIndexFromCursorPosition(document: TextDocument, lineNum: number, cursorChar: number, parameterType: string, _baseParameterCount: number): number | null {
    // Get all scanned parameters for this line
    const allParams = this.scanAllParameters(document);
    const paramsOnLine = allParams.filter(param => 
      param.line === lineNum && param.type === parameterType
    );
    
    // Find which parameter range the cursor falls into
    for (const param of paramsOnLine) {
      if (param.startChar !== undefined && param.endChar !== undefined) {
        if (cursorChar >= param.startChar && cursorChar <= param.endChar) {
          return param.index;
        }
      }
    }
    
    return null;
  }

  /**
   * Finds all definition locations for a parameter, handling SAME constraints
   */
  private findAllDefinitionLocations(document: TextDocument, parameter: ParameterInfo): Location[] {
    const allParams = this.scanAllParameters(document);
    
    // Find the specific parameter definition
    const paramLocation = allParams.find(param => 
      param.type === parameter.type && param.index === parameter.index
    );
    
    if (!paramLocation) {
      return [];
    }
    
    const lines = document.getText().split('\n');
    const line = lines[paramLocation.line];
    const locations: Location[] = [];
    
    // Always add the primary definition location
    locations.push({
      uri: document.uri,
      range: {
        start: { 
          line: paramLocation.line, 
          character: paramLocation.startChar || 0 
        },
        end: { 
          line: paramLocation.line, 
          character: paramLocation.endChar || line?.length || 0 
        }
      }
    });
    
    // Add additional ranges (e.g., FIXED keywords)
    if (paramLocation.additionalRanges) {
      for (const range of paramLocation.additionalRanges) {
        locations.push({
          uri: document.uri,
          range: {
            start: { 
              line: range.line !== undefined ? range.line : paramLocation.line, 
              character: range.startChar 
            },
            end: { 
              line: range.line !== undefined ? range.line : paramLocation.line, 
              character: range.endChar 
            }
          }
        });
      }
    }
    
    // For ETA parameters with SAME constraints, add the referenced location
    if (parameter.type === 'ETA' && line && /\bSAME\b/i.test(line.trim())) {
      const referencedLocation = this.findSameReferenceLocation(document, paramLocation.line);
      if (referencedLocation) {
        locations.push(referencedLocation);
      }
    }
    
    return locations;
  }

  /**
   * Find the location that a SAME constraint references (for multiple definitions)
   */
  private findSameReferenceLocation(document: TextDocument, sameLineNum: number): Location | null {
    const lines = document.getText().split('\n');
    
    // Go backwards to find the preceding OMEGA BLOCK
    for (let i = sameLineNum - 1; i >= 0; i--) {
      const line = lines[i];
      if (!line) continue;
      
      const trimmed = line.trim();
      if (trimmed.startsWith(';') || trimmed.length === 0) continue;
      
      // Found a BLOCK that's not SAME
      if (/^\$OMEGA.*BLOCK\(\d+\)/i.test(trimmed) && !/\bSAME\b/i.test(trimmed)) {
        // First check if there are inline values on the same line
        const afterBlock = trimmed.replace(/^\$OMEGA\s+BLOCK\(\d+\)\s*/i, '');
        const hasInlineValues = afterBlock.trim().length > 0 && !/^;/.test(afterBlock.trim());
        
        if (hasInlineValues) {
          // BLOCK with inline values - find the first value (diagonal element)
          const blockStartIndex = line.indexOf(afterBlock.trim());
          const valuePosition = this.findDiagonalElementInText(afterBlock, 1, blockStartIndex);
          if (valuePosition) {
            return {
              uri: document.uri,
              range: {
                start: { line: i, character: valuePosition.start },
                end: { line: i, character: valuePosition.end }
              }
            };
          }
        } else {
          // Look for the diagonal element on continuation line
          for (let j = i + 1; j < lines.length; j++) {
            const nextLine = lines[j];
            if (!nextLine) continue;
            
            const nextTrimmed = nextLine.trim();
            if (nextTrimmed.startsWith(';') || nextTrimmed.length === 0) continue;
            if (nextTrimmed.startsWith('$')) break;
            
            // Found the diagonal element - return its location
            const valuePosition = this.findDiagonalElementPosition(nextLine, 1);
            if (valuePosition) {
              return {
                uri: document.uri,
                range: {
                  start: { line: j, character: valuePosition.start },
                  end: { line: j, character: valuePosition.end }
                }
              };
            }
          }
        }
        break;
      }
    }
    
    return null;
  }


  /**
   * Finds all references to a parameter in the document
   * @param includeDeclaration If true, includes definition locations; if false, only usage locations
   */
  private findAllReferences(document: TextDocument, parameter: ParameterInfo, includeDeclaration: boolean = true): Location[] {
    const references: Location[] = [];
    const lines = document.getText().split('\n');
    
    // Create regex pattern for exact parameter match
    // Note: \b doesn't work well with parentheses, so use more specific pattern
    // For EPS parameters, also search for ERR (synonym)
    const typePattern = parameter.type === 'EPS' ? '(EPS|ERR)' : parameter.type;
    const searchPattern = new RegExp(`\\b${typePattern}\\(${parameter.index}\\)`, 'gi');
    
    // Use scanner to find definition lines (including continuation lines)
    const allParams = this.scanAllParameters(document);

    for (let lineNum = 0; lineNum < lines.length; lineNum++) {
      const line = lines[lineNum];
      if (!line) continue;
      
      // Check if this is a definition line for our parameter (including continuation lines)
      const paramLocation = allParams.find(param => 
        param.type === parameter.type && param.index === parameter.index && param.line === lineNum
      );
      
      if (paramLocation && includeDeclaration) {
        // Add primary definition range
        references.push({
          uri: document.uri,
          range: {
            start: { 
              line: lineNum, 
              character: paramLocation.startChar || 0 
            },
            end: { 
              line: lineNum, 
              character: paramLocation.endChar || line.length 
            }
          }
        });
        
        // Add additional ranges (e.g., FIXED keywords)
        if (paramLocation.additionalRanges) {
          for (const range of paramLocation.additionalRanges) {
            references.push({
              uri: document.uri,
              range: {
                start: { 
                  line: range.line !== undefined ? range.line : lineNum, 
                  character: range.startChar 
                },
                end: { 
                  line: range.line !== undefined ? range.line : lineNum, 
                  character: range.endChar 
                }
              }
            });
          }
        }
      }
      
      // Find usage references (THETA(n) patterns) - but exclude commented-out ones
      let match: RegExpExecArray | null;
      while ((match = searchPattern.exec(line)) !== null) {
        // Check if this match is inside a comment (after a semicolon)
        const commentStart = line.indexOf(';');
        const isInComment = commentStart !== -1 && match.index > commentStart;
        
        if (!isInComment) {
          references.push({
            uri: document.uri,
            range: {
              start: { line: lineNum, character: match.index },
              end: { line: lineNum, character: match.index + match[0].length }
            }
          });
        }
      }

      // Reset regex for next line
      searchPattern.lastIndex = 0;
    }

    return references;
  }

  /**
   * Checks if a line defines parameters of the specified type
   * Must match exactly $THETA, $OMEGA, $SIGMA (not $THETAP, $OMEGAPD, etc.)
   */
  private isDefinitionLine(line: string, parameterType: string): boolean {
    switch (parameterType) {
      case 'THETA':
        return PARAMETER_PATTERNS.THETA.test(line);
      case 'ETA':
        return PARAMETER_PATTERNS.OMEGA.test(line);
      case 'EPS':
        return PARAMETER_PATTERNS.SIGMA.test(line);
      default:
        return false;
    }
  }

  /**
   * Counts how many parameters are defined in a single line
   * Handles both header lines ($THETA) and continuation lines with parameter values
   */
  private countParametersInLine(line: string, parameterType: string): number {
    // For ETA/EPS: Check for BLOCK(n) syntax first
    if (parameterType !== 'THETA') {
      const blockMatch = line.match(PARAMETER_PATTERNS.BLOCK);
      if (blockMatch && blockMatch[1]) {
        return parseInt(blockMatch[1], 10);
      }
    }

    // Remove control record prefix and comments, then count parameter values
    const contentPart = line.replace(/^\$\w+/i, '').replace(/;.*$/, '').trim();
    
    if (!contentPart) {
      return 0; // Header-only line with no parameter values
    }

    // Handle bounded values that may have spaces inside them
    // Count complete parameter units: standalone numbers or complete bounded expressions
    let paramCount = 0;
    
    // Remove common keywords first
    const cleanContent = contentPart.replace(/\b(FIX|FIXED|STANDARD|VARIANCE|CORRELATION|CHOLESKY|DIAGONAL|SAME|VALUES|NAMES)\b/gi, '').trim();
    
    if (!cleanContent) {
      return 0;
    }
    
    // Count bounded expressions like (0,1,2) or (0, 1, 2) as single parameters
    const boundedExpressions = cleanContent.match(/\([^)]*\)/g);
    if (boundedExpressions) {
      paramCount += boundedExpressions.length;
      // Remove bounded expressions to count remaining standalone numbers
      const withoutBounded = cleanContent.replace(/\([^)]*\)/g, '').trim();
      if (withoutBounded) {
        // Count remaining standalone numeric values
        const standaloneNumbers = withoutBounded.split(/\s+/).filter(token => 
          token.length > 0 && /^[\d\-.]/.test(token)
        );
        paramCount += standaloneNumbers.length;
      }
    } else {
      // No bounded expressions, just count standalone numbers
      const tokens = cleanContent.split(/\s+/).filter(token => 
        token.length > 0 && /^[\d\-.]/.test(token)
      );
      paramCount = tokens.length;
    }

    return paramCount;
  }

  /**
   * Helper method to handle BLOCK matrix detection and eliminate code duplication
   */
  private handleBlockMatrixDetection(line: string, callback: (state: { inBlockMatrix: boolean; blockMatrixRemaining: number }) => void): void {
    const blockMatch = line.match(PARAMETER_PATTERNS.BLOCK);
    if (blockMatch && blockMatch[1]) {
      callback({
        inBlockMatrix: true,
        blockMatrixRemaining: parseInt(blockMatch[1], 10)
      });
    } else {
      callback({
        inBlockMatrix: false,
        blockMatrixRemaining: 0
      });
    }
  }
}