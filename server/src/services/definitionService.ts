/**
 * Definition and Reference Service - Simplified Navigation
 * 
 * Provides basic "Go to Definition" and "Find All References" for NMTRAN parameters.
 * Simple implementation focused on core navigation functionality.
 */

import { Connection, Location, Position } from 'vscode-languageserver/node';
import { TextDocument } from 'vscode-languageserver-textdocument';

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
    const lines = document.getText().split('\n');
    let parameterCount = 0;

    for (let lineNum = 0; lineNum < lines.length; lineNum++) {
      const line = lines[lineNum];
      if (!line) continue;
      const trimmedLine = line.trim();
      
      // Skip comments and empty lines
      if (trimmedLine.startsWith(';') || trimmedLine.length === 0) {
        continue;
      }

      // Check if this line defines our parameter type
      if (!this.isDefinitionLine(trimmedLine, parameter.type)) {
        continue;
      }

      // Count parameters defined by this line
      const parametersInThisLine = this.countParametersInLine(trimmedLine, parameter.type);
      
      // Check if our target parameter is defined in this line
      if (parameterCount + parametersInThisLine >= parameter.index) {
        return {
          uri: document.uri,
          range: {
            start: { line: lineNum, character: 0 },
            end: { line: lineNum, character: lines[lineNum]?.length || 0 }
          }
        };
      }
      
      parameterCount += parametersInThisLine;
    }

    return null;
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
    

    for (let lineNum = 0; lineNum < lines.length; lineNum++) {
      const line = lines[lineNum];
      if (!line) continue;
      
      // Also check if this line is a definition line for this parameter
      const trimmedLine = line.trim();
      if (this.isDefinitionLine(trimmedLine, parameter.type)) {
        // Check if this definition line defines our target parameter
        const parameterInfo = this.getParameterFromDefinitionLine(document, lineNum, trimmedLine);
        if (parameterInfo && parameterInfo.index === parameter.index) {
          references.push({
            uri: document.uri,
            range: {
              start: { line: lineNum, character: 0 },
              end: { line: lineNum, character: line.length }
            }
          });
        }
      }
      
      let match: RegExpExecArray | null;

      while ((match = searchPattern.exec(line)) !== null) {
        references.push({
          uri: document.uri,
          range: {
            start: { line: lineNum, character: match.index },
            end: { line: lineNum, character: match.index + match[0].length }
          }
        });
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
   * THETA: Always 1 (simplified rule - one parameter per $THETA line)
   * ETA/EPS: 1 for diagonal, n for BLOCK(n)
   */
  private countParametersInLine(line: string, parameterType: string): number {
    if (parameterType === 'THETA') {
      // Simplified: One THETA parameter per $THETA line
      return 1;
    }

    // For ETA/EPS: Check for BLOCK(n) syntax first
    const blockMatch = line.match(/BLOCK\((\d+)\)/i);
    if (blockMatch && blockMatch[1]) {
      return parseInt(blockMatch[1], 10);
    }

    // For diagonal elements: count parameter-like tokens
    // Remove $OMEGA/$SIGMA and comments, then count values
    const contentPart = line.replace(/^\$\w+/i, '').replace(/;.*$/, '').trim();
    
    if (!contentPart) {
      return 1; // Default to 1 parameter if line exists
    }

    // Simple counting: split by whitespace and count numeric/bounded values
    const tokens = contentPart.split(/\s+/).filter(token => token.length > 0);
    let paramCount = 0;

    for (const token of tokens) {
      // Count tokens that look like parameter values (numbers, bounds, etc.)
      if (this.looksLikeParameterValue(token)) {
        paramCount++;
      }
    }

    return Math.max(1, paramCount); // At least 1 parameter per definition line
  }

  /**
   * Simple heuristic to identify parameter values vs keywords/options
   */
  private looksLikeParameterValue(token: string): boolean {
    // Skip common keywords/options
    if (/^(FIX|FIXED|STANDARD|VARIANCE|CORRELATION|CHOLESKY|DIAGONAL|SAME|VALUES|NAMES)$/i.test(token)) {
      return false;
    }

    // Accept numeric values, bounded values like (0,1,2), or infinity patterns
    return /^[\d\-.]|^\(|^-?INF$/i.test(token);
  }
}