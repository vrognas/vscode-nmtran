/**
 * Parameter parsing utilities
 * Handles extraction and parsing of NMTRAN parameter values
 */

export interface ParsedParameter {
  value: string;
  position: { start: number; end: number };
  constraints?: {
    lowerBound?: string;
    upperBound?: string;
    isFixed?: boolean;
  };
}

/**
 * Parse a parameter value with optional bounds
 * e.g., "(0,27.6)" -> { lowerBound: "0", value: "27.6" }
 */
export function parseParameterWithBounds(text: string): ParsedParameter | null {
  const boundsPattern = /\(([^,]+),([^,)]+)(?:,([^)]+))?\)/;
  const match = text.match(boundsPattern);
  
  if (match) {
    const result: ParsedParameter = {
      value: match[2]!.trim(),
      position: { start: 0, end: text.length },
      constraints: {
        lowerBound: match[1]!.trim()
      }
    };
    
    if (match[3]) {
      result.constraints!.upperBound = match[3].trim();
    }
    
    return result;
  }
  
  // Simple value without bounds
  const valueMatch = text.match(/[\d\-+][\d\-+.eE]*/);
  if (valueMatch) {
    return {
      value: valueMatch[0],
      position: { 
        start: valueMatch.index || 0, 
        end: (valueMatch.index || 0) + valueMatch[0].length 
      }
    };
  }
  
  return null;
}

/**
 * Extract all parameter values from a line
 */
export function extractParameterValues(line: string): string[] {
  // Remove comments and keywords
  const cleanLine = line.replace(/;.*$/, '')
                       .replace(/^\$\w+\s*/i, '')
                       .replace(/BLOCK\(\d+\)\s*/i, '')
                       .replace(/\b(FIX|FIXED|SAME)\b/gi, '');
  
  const values: string[] = [];
  const pattern = /[\d\-+][\d\-+.eE]*/g;
  let match;
  
  while ((match = pattern.exec(cleanLine)) !== null) {
    values.push(match[0]);
  }
  
  return values;
}