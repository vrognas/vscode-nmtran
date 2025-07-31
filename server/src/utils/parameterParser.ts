/**
 * Parameter Parser Factory
 * 
 * Unified parsing logic for THETA, ETA, and EPS parameters.
 * Reduces code repetition and provides consistent parsing behavior.
 */

// Constants for consistent parameter pattern matching
export const PARAMETER_PATTERNS = {
  THETA: /^\$THETA(\s|$)/i,
  OMEGA: /^\$OMEGA(\s|$)/i, 
  SIGMA: /^\$SIGMA(\s|$)/i,
  BLOCK: /BLOCK\((\d+)\)/i,
  SAME: /\bSAME\b/i,
  PARAMETER_USAGE_SOURCE: '\\b(THETA|ETA|EPS)\\((\\d+)\\)' // Source pattern without flags
} as const;

export type ParameterType = 'THETA' | 'ETA' | 'EPS';

export interface ParameterLocation {
  type: ParameterType;
  index: number;
  line: number;
  startChar?: number;  // Start character position of the specific value
  endChar?: number;    // End character position of the specific value
}

export interface ParameterInfo {
  type: string;
  index: number;
}

export interface CharacterRange {
  start: number;
  end: number;
}

/**
 * Factory for creating parameter parsers with consistent behavior
 */
export class ParameterParserFactory {
  
  /**
   * Create a fresh regex instance to avoid state contamination
   */
  static createParameterUsageRegex(): RegExp {
    return new RegExp(PARAMETER_PATTERNS.PARAMETER_USAGE_SOURCE, 'gi');
  }

  /**
   * Parse parameter reference from text
   */
  static parseParameterReference(text: string, position: number): ParameterInfo | null {
    const regex = this.createParameterUsageRegex();
    let match;
    
    while ((match = regex.exec(text)) !== null) {
      const matchStart = match.index;
      const matchEnd = match.index + match[0].length;
      
      if (position >= matchStart && position <= matchEnd) {
        const paramType = match[1];
        const paramIndex = match[2];
        if (paramType && paramIndex) {
          return {
            type: paramType, // THETA, ETA, or EPS
            index: parseInt(paramIndex, 10)
          };
        }
      }
    }
    
    return null;
  }

  /**
   * Find all parameter references in text
   */
  static findAllParameterReferences(text: string, excludeComments: boolean = true): ParameterInfo[] {
    const references: ParameterInfo[] = [];
    const lines = text.split('\n');
    
    for (const line of lines) {
      let processLine = line;
      
      // Remove comments if requested
      if (excludeComments) {
        const commentIndex = line.indexOf(';');
        if (commentIndex !== -1) {
          processLine = line.substring(0, commentIndex);
        }
      }
      
      const regex = this.createParameterUsageRegex();
      let match;
      
      while ((match = regex.exec(processLine)) !== null) {
        const paramType = match[1];
        const paramIndex = match[2];
        if (paramType && paramIndex) {
          references.push({
            type: paramType,
            index: parseInt(paramIndex, 10)
          });
        }
      }
    }
    
    return references;
  }

  /**
   * Check if line is a parameter definition line
   */
  static isParameterDefinitionLine(line: string, parameterType: ParameterType): boolean {
    const patterns = {
      'THETA': PARAMETER_PATTERNS.THETA,
      'ETA': PARAMETER_PATTERNS.OMEGA,
      'EPS': PARAMETER_PATTERNS.SIGMA
    };
    
    return patterns[parameterType].test(line.trim());
  }

  /**
   * Extract BLOCK size from line
   */
  static extractBlockSize(line: string): number | null {
    const match = line.match(PARAMETER_PATTERNS.BLOCK);
    if (match && match[1]) {
      return parseInt(match[1], 10);
    }
    return null;
  }

  /**
   * Check if line contains SAME constraint
   */
  static hasSameConstraint(line: string): boolean {
    return PARAMETER_PATTERNS.SAME.test(line);
  }

  /**
   * Parse bounded THETA value: (low, init, up) or (low, init) or init
   */
  static parseThetaValue(valueText: string): CharacterRange | null {
    const trimmed = valueText.trim();
    
    // Handle bounded format: (low, init, up) or (low, init)
    const boundedMatch = trimmed.match(/^\s*\(\s*([^,]+)\s*,\s*([^,)]+)(?:\s*,\s*([^)]+))?\s*\)/);
    if (boundedMatch && boundedMatch[0] && boundedMatch[2]) {
      // Find the initial value (second parameter)
      const fullMatch = boundedMatch[0];
      const initialValue = boundedMatch[2];
      const initialStart = fullMatch.indexOf(initialValue);
      
      return {
        start: initialStart,
        end: initialStart + initialValue.length
      };
    }
    
    // Handle simple numeric value
    const numericMatch = trimmed.match(/^\s*([\d.-]+)/);
    if (numericMatch && numericMatch[1]) {
      return {
        start: 0,
        end: numericMatch[1].length
      };
    }
    
    return null;
  }

  /**
   * Get corresponding parameter type mapping
   */
  static getParameterTypeMapping(paramType: string): ParameterType | null {
    const mapping: Record<string, ParameterType> = {
      'THETA': 'THETA',
      'ETA': 'ETA',
      'OMEGA': 'ETA',
      'EPS': 'EPS',
      'SIGMA': 'EPS'
    };
    
    return mapping[paramType.toUpperCase()] || null;
  }
}