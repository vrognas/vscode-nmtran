/**
 * Hover Service
 * 
 * Handles hover information for NMTRAN control records.
 * Separated from main server for better maintainability.
 */

import { Connection, Hover, MarkupContent, MarkupKind } from 'vscode-languageserver/node';
import { TextDocument } from 'vscode-languageserver-textdocument';
import { explainControlRecordHover } from '../hoverInfo';
import { getFullControlRecordName } from '../utils/validateControlRecords';
import { ParameterScanner } from './ParameterScanner';

export class HoverService {
  private connection: Connection;
  private readonly CONTROL_RECORD_REGEX = /\$[A-Z]+\b/g;
  private readonly PARAMETER_USAGE_REGEX = /\b(THETA|ETA|EPS|ERR)\((\d+)\)/g;

  constructor(connection: Connection) {
    this.connection = connection;
  }

  /**
   * Provides hover information for control records and parameter references at the given position
   */
  provideHover(document: TextDocument, position: { line: number; character: number }): Hover | null {
    try {
      const text = document.getText();
      const offset = document.offsetAt(position);
      
      // First check for parameter references (THETA(1), ETA(2), etc.)
      const parameterHover = this.getParameterReferenceHover(document, position, offset);
      if (parameterHover) {
        return parameterHover;
      }
      
      // Check for reserved variables
      const reservedVariableHover = this.getReservedVariableHover(text, offset);
      if (reservedVariableHover) {
        return reservedVariableHover;
      }
      
      // Then check for control records
      return this.getControlRecordHover(text, offset, document);

    } catch (error) {
      this.connection.console.error(`❌ Error providing hover: ${error}`);
      return null;
    }
  }

  /**
   * Get hover information for parameter references like THETA(1), ETA(2), etc.
   */
  private getParameterReferenceHover(document: TextDocument, position: { line: number; character: number }, offset: number): Hover | null {
    const text = document.getText();
    
    // Reset regex state
    this.PARAMETER_USAGE_REGEX.lastIndex = 0;
    let match: RegExpExecArray | null;

    while ((match = this.PARAMETER_USAGE_REGEX.exec(text)) !== null) {
      const start = match.index;
      const end = match.index + match[0].length;
      
      if (start <= offset && offset <= end) {
        const paramType = match[1] as 'THETA' | 'ETA' | 'EPS' | 'ERR';
        const paramIndex = parseInt(match[2] || '0', 10);
        
        // Convert ERR to EPS for consistency
        const normalizedType = paramType === 'ERR' ? 'EPS' : paramType;
        
        // Find the parameter definition
        const parameterLocations = ParameterScanner.scanDocument(document);
        const definition = parameterLocations.find(loc => 
          loc.type === normalizedType && loc.index === paramIndex
        );
        
        if (definition) {
          const hoverContent = this.buildParameterHoverContent(document, definition, paramType, paramIndex);
          
          return {
            contents: {
              kind: MarkupKind.Markdown,
              value: hoverContent
            },
            range: {
              start: document.positionAt(start),
              end: document.positionAt(end)
            }
          };
        }
      }
    }
    
    return null;
  }

  /**
   * Get hover information for control records
   */
  private getControlRecordHover(text: string, offset: number, document: TextDocument): Hover | null {
    // Reset regex state
    this.CONTROL_RECORD_REGEX.lastIndex = 0;
    let match: RegExpExecArray | null;

    while ((match = this.CONTROL_RECORD_REGEX.exec(text)) !== null) {
      const start = match.index;
      const end = match.index + match[0].length;
      
      if (start <= offset && offset <= end) {
        const controlRecord = match[0];
        const fullControlRecord = getFullControlRecordName(controlRecord);
        
        const hoverInfo: MarkupContent = {
          kind: MarkupKind.Markdown,
          value: explainControlRecordHover(controlRecord, fullControlRecord)
        };

        return {
          contents: hoverInfo,
          range: {
            start: document.positionAt(start),
            end: document.positionAt(end)
          }
        };
      }
    }

    return null;
  }

  /**
   * Build hover content for parameter references by extracting definition text
   */
  private buildParameterHoverContent(document: TextDocument, definition: any, paramType: string, paramIndex: number): string {
    const lines = document.getText().split('\n');
    const definitionLine = lines[definition.line];
    
    if (!definitionLine) {
      return `**${paramType}(${paramIndex})**: Definition not found`;
    }
    
    // Extract the parameter value from the main range
    let parameterValue = '';
    if (definition.startChar !== undefined && definition.endChar !== undefined) {
      parameterValue = definitionLine.substring(definition.startChar, definition.endChar).trim();
    }
    
    // Check if this is a SAME keyword and resolve it
    if (parameterValue === 'SAME') {
      const resolvedValue = this.resolveSameKeyword(document, definition, paramType, paramIndex);
      if (resolvedValue) {
        parameterValue = resolvedValue;
      }
    }
    
    // Extract FIXED keywords from additional ranges
    const fixedKeywords: string[] = [];
    if (definition.additionalRanges) {
      for (const range of definition.additionalRanges) {
        if (range.line !== undefined && range.line !== definition.line) {
          // FIXED keyword is on a different line (BLOCK declaration line)
          const fixedLine = lines[range.line];
          if (fixedLine) {
            const fixedText = fixedLine.substring(range.startChar, range.endChar);
            fixedKeywords.push(fixedText);
          }
        } else {
          // FIXED keyword is on the same line as the value
          const fixedText = definitionLine.substring(range.startChar, range.endChar);
          fixedKeywords.push(fixedText);
        }
      }
    }
    
    // Build the hover content
    let content = `**${paramType}(${paramIndex})**`;
    
    if (parameterValue || fixedKeywords.length > 0) {
      const parts = [];
      if (parameterValue) {
        parts.push(parameterValue);
      }
      if (fixedKeywords.length > 0) {
        parts.push(...fixedKeywords);
      }
      content += `: ${parts.join(' ')}`;
    }
    
    return content;
  }

  /**
   * Resolve SAME keyword by finding the previous parameter value
   */
  private resolveSameKeyword(document: TextDocument, definition: any, paramType: string, paramIndex: number): string | null {
    // Get all parameter locations for this type
    const parameterLocations = ParameterScanner.scanDocument(document);
    const sameTypeParams = parameterLocations.filter(loc => loc.type === paramType && loc.index < paramIndex);
    
    if (sameTypeParams.length === 0) {
      return null;
    }
    
    // Find the most recent parameter of the same type
    const previousParam = sameTypeParams[sameTypeParams.length - 1];
    if (!previousParam) {
      return null;
    }
    
    const lines = document.getText().split('\n');
    const previousLine = lines[previousParam.line];
    
    if (!previousLine || previousParam.startChar === undefined || previousParam.endChar === undefined) {
      return null;
    }
    
    let previousValue = previousLine.substring(previousParam.startChar, previousParam.endChar).trim();
    let originalParamIndex = previousParam.index;
    
    // If the previous value is also SAME, recursively resolve it to find the original value
    if (previousValue === 'SAME') {
      const resolvedInfo = this.resolveSameKeywordWithReference(document, previousParam, paramType, previousParam.index);
      if (resolvedInfo) {
        previousValue = resolvedInfo.value;
        originalParamIndex = resolvedInfo.originalIndex;
      } else {
        previousValue = 'SAME';
      }
    }
    
    return `${previousValue} SAME as ${paramType}(${originalParamIndex})`;
  }

  /**
   * Helper method to resolve SAME keyword and track the original parameter index
   */
  private resolveSameKeywordWithReference(document: TextDocument, definition: any, paramType: string, paramIndex: number): { value: string; originalIndex: number } | null {
    // Get all parameter locations for this type
    const parameterLocations = ParameterScanner.scanDocument(document);
    const sameTypeParams = parameterLocations.filter(loc => loc.type === paramType && loc.index < paramIndex);
    
    if (sameTypeParams.length === 0) {
      return null;
    }
    
    // Find the most recent parameter of the same type
    const previousParam = sameTypeParams[sameTypeParams.length - 1];
    if (!previousParam) {
      return null;
    }
    
    const lines = document.getText().split('\n');
    const previousLine = lines[previousParam.line];
    
    if (!previousLine || previousParam.startChar === undefined || previousParam.endChar === undefined) {
      return null;
    }
    
    const previousValue = previousLine.substring(previousParam.startChar, previousParam.endChar).trim();
    
    // If the previous value is also SAME, recursively resolve it
    if (previousValue === 'SAME') {
      return this.resolveSameKeywordWithReference(document, previousParam, paramType, previousParam.index);
    }
    
    return { value: previousValue, originalIndex: previousParam.index };
  }

  /**
   * Get hover information for reserved variables like ICALL, NEWIND, Y
   */
  private getReservedVariableHover(text: string, offset: number): Hover | null {
    // Check for reserved variables at the cursor position
    const reservedVariables = {
      'ICALL': 'Reserved variable - Execution context: 0=run init, 1=problem init, 2=analysis, 3=finalization, 4=simulation, 5=expectation, 6=data average',
      'NEWIND': 'Reserved variable - Individual record indicator: 0=first record, 1=new individual, 2=continuation record',
      'Y': 'Mandatory left-hand quantity for PRED - represents the predicted value or observation under the statistical model',
      'ERR': 'Reserved array - Alternative to ETA(n)/EPS(n) for random intra-individual effects'
    };

    // Find word boundaries around the offset
    const beforeOffset = Math.max(0, offset - 10);
    const afterOffset = Math.min(text.length, offset + 10);
    const searchText = text.substring(beforeOffset, afterOffset);
    
    for (const [variable, description] of Object.entries(reservedVariables)) {
      const regex = new RegExp(`\\b${variable}\\b`, 'i');
      const match = regex.exec(searchText);
      
      if (match) {
        const matchStart = beforeOffset + match.index;
        const matchEnd = matchStart + match[0].length;
        
        // Check if the cursor is within the match
        if (offset >= matchStart && offset <= matchEnd) {
          return {
            contents: {
              kind: MarkupKind.Markdown,
              value: description
            } as MarkupContent
          } as Hover;
        }
      }
    }

    return null;
  }
}