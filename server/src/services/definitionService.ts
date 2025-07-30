/**
 * Definition and Reference Service - Simplified Navigation
 * 
 * Provides basic "Go to Definition" and "Find All References" for NMTRAN parameters.
 * Simple implementation focused on core navigation functionality.
 */

import { Connection, Location, Position } from 'vscode-languageserver/node';
import { TextDocument } from 'vscode-languageserver-textdocument';

interface ParameterLocation {
  type: 'THETA' | 'ETA' | 'EPS';
  index: number;
  line: number;
}

export class DefinitionService {
  private connection: Connection;

  constructor(connection: Connection) {
    this.connection = connection;
  }

  /**
   * Provides definition location for NMTRAN parameters
   * THETA(3) → jumps to 3rd $THETA line
   * ETA(2) → jumps to line defining 2nd ETA parameter  
   * EPS(1) → jumps to line defining 1st EPS parameter
   */
  provideDefinition(document: TextDocument, position: Position): Location[] | null {
    try {
      const parameter = this.getParameterAtPosition(document, position);
      if (!parameter) {
        return null;
      }

      const definitionLocation = this.findDefinitionLocation(document, parameter);
      return definitionLocation ? [definitionLocation] : null;

    } catch (error) {
      this.connection.console.error(`❌ Error in definition provider: ${error}`);
      return null;
    }
  }

  /**
   * Provides all reference locations for NMTRAN parameters
   * Shows everywhere THETA(3), ETA(2), etc. is used in the document
   */
  provideReferences(document: TextDocument, position: Position, _includeDeclaration: boolean): Location[] | null {
    try {
      const parameter = this.getParameterAtPosition(document, position);
      if (!parameter) {
        return null;
      }

      const references = this.findAllReferences(document, parameter);
      
      // findAllReferences now includes both usages and definitions
      // The includeDeclaration parameter is handled within findAllReferences
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
  private getParameterAtPosition(document: TextDocument, position: Position): { type: string; index: number } | null {
    const line = document.getText({
      start: { line: position.line, character: 0 },
      end: { line: position.line, character: Number.MAX_VALUE }
    });

    // First, try to match THETA(1), ETA(2), EPS(3) patterns - numeric indices only
    const parameterRegex = /(THETA|ETA|EPS)\((\d+)\)/gi;
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
   */
  private scanAllParameters(document: TextDocument): ParameterLocation[] {
    const locations: ParameterLocation[] = [];
    const lines = document.getText().split('\n');
    
    let currentBlockType: 'THETA' | 'ETA' | 'EPS' | null = null;
    const counters = { THETA: 0, ETA: 0, EPS: 0 };
    
    for (let lineNum = 0; lineNum < lines.length; lineNum++) {
      const line = lines[lineNum];
      if (!line) continue;
      
      const trimmed = line.trim();
      if (trimmed.startsWith(';') || trimmed.length === 0) continue;
      
      // Check if starting a new parameter block
      if (/^\$THETA(\s|$)/i.test(trimmed)) {
        currentBlockType = 'THETA';
      } else if (/^\$OMEGA(\s|$)/i.test(trimmed)) {
        currentBlockType = 'ETA';
      } else if (/^\$SIGMA(\s|$)/i.test(trimmed)) {
        currentBlockType = 'EPS';
      } else if (trimmed.startsWith('$')) {
        currentBlockType = null; // Different control record
      }
      
      // If we're in a parameter block, count parameters on this line
      if (currentBlockType) {
        const paramCount = this.countParametersInLine(trimmed, currentBlockType);
        for (let i = 0; i < paramCount; i++) {
          counters[currentBlockType]++;
          locations.push({
            type: currentBlockType,
            index: counters[currentBlockType],
            line: lineNum
          });
        }
      }
    }
    
    return locations;
  }

  /**
   * Check if current line is a parameter continuation line and get parameter info
   */
  private getParameterFromContinuationLine(document: TextDocument, lineNum: number): { type: string; index: number } | null {
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
  private getParameterFromDefinitionLine(document: TextDocument, lineNum: number, line: string): { type: string; index: number } | null {
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
   * Finds where a parameter is defined in the document
   */
  private findDefinitionLocation(document: TextDocument, parameter: { type: string; index: number }): Location | null {
    const allParams = this.scanAllParameters(document);
    
    // Find the specific parameter definition
    const paramLocation = allParams.find(param => 
      param.type === parameter.type && param.index === parameter.index
    );
    
    if (!paramLocation) {
      return null;
    }
    
    const lines = document.getText().split('\n');
    const line = lines[paramLocation.line];
    
    return {
      uri: document.uri,
      range: {
        start: { line: paramLocation.line, character: 0 },
        end: { line: paramLocation.line, character: line?.length || 0 }
      }
    };
  }

  /**
   * Finds all references to a parameter in the document
   */
  private findAllReferences(document: TextDocument, parameter: { type: string; index: number }): Location[] {
    const references: Location[] = [];
    const lines = document.getText().split('\n');
    
    // Create regex pattern for exact parameter match
    // Note: \b doesn't work well with parentheses, so use more specific pattern
    const searchPattern = new RegExp(`\\b${parameter.type}\\(${parameter.index}\\)`, 'gi');
    
    // Use scanner to find definition lines (including continuation lines)
    const allParams = this.scanAllParameters(document);
    const definitionLines = allParams
      .filter(param => param.type === parameter.type && param.index === parameter.index)
      .map(param => param.line);

    for (let lineNum = 0; lineNum < lines.length; lineNum++) {
      const line = lines[lineNum];
      if (!line) continue;
      
      // Check if this is a definition line for our parameter (including continuation lines)
      if (definitionLines.includes(lineNum)) {
        references.push({
          uri: document.uri,
          range: {
            start: { line: lineNum, character: 0 },
            end: { line: lineNum, character: line.length }
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
        return /^\$THETA(\s|$)/i.test(line);
      case 'ETA':
        return /^\$OMEGA(\s|$)/i.test(line);
      case 'EPS':
        return /^\$SIGMA(\s|$)/i.test(line);
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
      const blockMatch = line.match(/BLOCK\((\d+)\)/i);
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
   * Simple heuristic to identify parameter values vs keywords/options
   */
  private looksLikeParameterValue(token: string): boolean {
    // Skip common keywords/options
    if (/^(FIX|FIXED|STANDARD|VARIANCE|CORRELATION|CHOLESKY|DIAGONAL|SAME|VALUES|NAMES)$/i.test(token)) {
      return false;
    }

    // Accept:
    // - Pure numbers: 10, 1.5, -2.3
    // - Bounded values: (0,1,2), (0,0.6,
    // - End of bounded values: 1), 2.5)
    // - Infinity patterns: INF, -INF
    return /^[\d\-.]+$|^\(|.*\)$|^-?INF$/i.test(token);
  }
}