/**
 * Definition and Reference Service - Simplified Navigation
 * 
 * Provides basic "Go to Definition" and "Find All References" for NMTRAN parameters.
 * Simple implementation focused on core navigation functionality.
 */

import { Connection, Location, Position } from 'vscode-languageserver/node';
import { TextDocument } from 'vscode-languageserver-textdocument';

// Constants for consistent parameter pattern matching
const PARAMETER_PATTERNS = {
  THETA: /^\$THETA(\s|$)/i,
  OMEGA: /^\$OMEGA(\s|$)/i, 
  SIGMA: /^\$SIGMA(\s|$)/i,
  BLOCK: /BLOCK\((\d+)\)/i,
  SAME: /\bSAME\b/i,
  PARAMETER_USAGE_SOURCE: '\\b(THETA|ETA|EPS)\\((\\d+)\\)' // Source pattern without flags
} as const;

// Factory function to create fresh regex instances to avoid state contamination
const createParameterUsageRegex = () => new RegExp(PARAMETER_PATTERNS.PARAMETER_USAGE_SOURCE, 'gi');

const PARAMETER_TYPE_MAP = {
  THETA: 'THETA' as const,
  OMEGA: 'ETA' as const,
  SIGMA: 'EPS' as const
};

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
}

interface ParameterInfo {
  type: string;
  index: number;
}

export class DefinitionService {
  private connection: Connection;
  private scanCache = new Map<string, ParameterLocation[]>();

  constructor(connection: Connection) {
    this.connection = connection;
  }

  /**
   * Provides definition location for NMTRAN parameters
   * THETA(3) → jumps to 3rd $THETA line
   * ETA(2) → jumps to line defining 2nd ETA parameter  
   * EPS(1) → jumps to line defining 1st EPS parameter
   * For SAME constraints, shows both the SAME line and the referenced value
   */
  provideDefinition(document: TextDocument, position: Position): Location[] | null {
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
        return {
          type: match[1]!.toUpperCase(),
          index: parseInt(match[2]!, 10)
        };
      }
    }

    // If not on a parameter usage, check if on a definition line
    const trimmedLine = line.trim();
    if (this.isDefinitionLine(trimmedLine, 'THETA') || 
        this.isDefinitionLine(trimmedLine, 'ETA') || 
        this.isDefinitionLine(trimmedLine, 'EPS')) {
      
      return this.getParameterFromDefinitionLine(document, position.line, trimmedLine);
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
    const locations: ParameterLocation[] = [];
    const lines = document.getText().split('\n');
    
    let currentBlockType: 'THETA' | 'ETA' | 'EPS' | null = null;
    let inBlockMatrix = false;
    let blockMatrixRemaining = 0;
    const counters = { THETA: 0, ETA: 0, EPS: 0 };
    
    for (let lineNum = 0; lineNum < lines.length; lineNum++) {
      const line = lines[lineNum];
      if (!line) continue;
      
      const trimmed = line.trim();
      if (trimmed.startsWith(';') || trimmed.length === 0) continue;
      
      // Check if starting a new parameter block
      if (PARAMETER_PATTERNS.THETA.test(trimmed)) {
        currentBlockType = 'THETA';
        inBlockMatrix = false;
        blockMatrixRemaining = 0;
      } else if (PARAMETER_PATTERNS.OMEGA.test(trimmed)) {
        currentBlockType = 'ETA';
        this.handleBlockMatrixDetection(trimmed, state => {
          inBlockMatrix = state.inBlockMatrix;
          blockMatrixRemaining = state.blockMatrixRemaining;
        });
      } else if (PARAMETER_PATTERNS.SIGMA.test(trimmed)) {
        currentBlockType = 'EPS';
        this.handleBlockMatrixDetection(trimmed, state => {
          inBlockMatrix = state.inBlockMatrix;
          blockMatrixRemaining = state.blockMatrixRemaining;
        });
      } else if (trimmed.startsWith('$')) {
        currentBlockType = null; // Different control record
        inBlockMatrix = false;
        blockMatrixRemaining = 0;
      }
      
      // If we're in a parameter block, count parameters on this line
      if (currentBlockType) {
        let paramCount = 0;
        
        if (inBlockMatrix && blockMatrixRemaining > 0) {
          // For BLOCK matrices, handle different cases
          if (trimmed.match(PARAMETER_PATTERNS.BLOCK)) {
            // Check if this is a SAME constraint or regular BLOCK
            if (PARAMETER_PATTERNS.SAME.test(trimmed)) {
              // SAME constraint - this defines 1 ETA parameter
              paramCount = 1;
              blockMatrixRemaining--;
              if (blockMatrixRemaining === 0) {
                inBlockMatrix = false;
              }
            } else {
              // Regular BLOCK header - don't assign parameters here, wait for matrix lines
              paramCount = 0;
            }
          } else {
            // Matrix data line - assign one ETA parameter to this line (the diagonal element)
            paramCount = 1;
            blockMatrixRemaining--;
            if (blockMatrixRemaining === 0) {
              inBlockMatrix = false; // End of this BLOCK matrix
            }
          }
        } else {
          // Regular parameter counting for THETA and diagonal OMEGA/SIGMA
          paramCount = this.countParametersInLine(trimmed, currentBlockType);
        }
        
        for (let i = 0; i < paramCount; i++) {
          counters[currentBlockType]++;
          const location: ParameterLocation = {
            type: currentBlockType,
            index: counters[currentBlockType],
            line: lineNum
          };
          
          // Add precise character positions for parameter values
          let valuePosition;
          
          if (currentBlockType === 'THETA') {
            // For THETA parameters, find the specific initial value (handle bounded syntax)
            const paramPosition = i + 1; // Which parameter in this line (1-based)
            valuePosition = this.findThetaInitialValue(line, paramPosition);
          } else {
            // For ETA/EPS parameters, handle different OMEGA/SIGMA patterns the same way
            valuePosition = this.findOmegaParameterValue(document, lineNum, counters[currentBlockType], inBlockMatrix);
          }
          
          if (valuePosition) {
            location.startChar = valuePosition.start;
            location.endChar = valuePosition.end;
          }
          
          locations.push(location);
        }
      }
    }
    
    // Cache the results
    this.scanCache.set(cacheKey, locations);
    
    // Limit cache size to prevent memory leaks
    if (this.scanCache.size > 50) {
      const firstKey = this.scanCache.keys().next().value;
      if (firstKey) {
        this.scanCache.delete(firstKey);
      }
    }
    
    return locations;
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
    const numericPattern = /[\d\-+][\d\-+\.eE]*/g;
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
   * Find the character position of a parameter value in a regular OMEGA/SIGMA line
   * For the nth parameter on a line, find the nth numeric value
   */
  private findParameterValuePosition(line: string, paramPosition: number): CharacterRange | null {
    // Remove comment part
    const codePartEnd = line.indexOf(';');
    const codePart = codePartEnd !== -1 ? line.substring(0, codePartEnd) : line;
    
    // Remove control record prefix (e.g., $OMEGA)
    const contentPart = codePart.replace(/^\s*\$\w+\s*/i, '');
    
    // Find all numeric values (including scientific notation)
    const numericPattern = /[\d\-+][\d\-+\.eE]*/g;
    const matches = [];
    let match;
    
    let searchOffset = codePart.length - contentPart.length; // Offset for control record removal
    
    while ((match = numericPattern.exec(contentPart)) !== null) {
      matches.push({
        value: match[0],
        start: match.index + searchOffset,
        end: match.index + match[0].length + searchOffset
      });
    }
    
    // Return the paramPosition-th numeric value
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
        let start = i;
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
        let start = i;
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
  private findOmegaParameterValue(document: TextDocument, lineNum: number, paramIndex: number, inBlockMatrix: boolean): CharacterRange | null {
    const lines = document.getText().split('\n');
    const line = lines[lineNum];
    if (!line) return null;
    
    const trimmed = line.trim();
    
    // Check for SAME keyword - return position of SAME keyword itself
    if (PARAMETER_PATTERNS.SAME.test(trimmed)) {
      return this.findSameKeywordPosition(line);
    }
    
    // Check if this is a BLOCK with continuation line
    if (PARAMETER_PATTERNS.BLOCK.test(trimmed) && inBlockMatrix) {
      // Look for value on next non-comment line
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
    
    // Regular OMEGA with inline value: $OMEGA 0 FIX
    // For BLOCK matrix lines, find the diagonal element at position paramIndex
    // For regular OMEGA/SIGMA lines, find the first parameter
    const paramPosition = inBlockMatrix ? paramIndex : 1;
    return this.findParameterValuePosition(line, paramPosition);
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
   * by counting preceding definition lines of the same type
   */
  private getParameterFromDefinitionLine(document: TextDocument, lineNum: number, line: string): ParameterInfo | null {
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
          // This is our target line - return the first parameter it defines
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
    const searchPattern = new RegExp(`\\b${parameter.type}\\(${parameter.index}\\)`, 'gi');
    
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