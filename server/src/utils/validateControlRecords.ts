import { Diagnostic, DiagnosticSeverity, TextDocument } from 'vscode-languageserver/node';
import { allowedControlRecords } from '../constants';

/**
 * Finds the full allowed control record name if the given record is a recognized abbreviation.
 * 
 * Why:
 * We want to handle cases where a user abbreviates a control record. This function returns the
 * full allowed control record if an abbreviation is recognized, improving consistency.
 */
function getFullControlRecordName(record: string): string {
  for (const validRecord of allowedControlRecords) {
    if (validRecord.startsWith(record)) {
      return validRecord;
    }
  }
  return record;
}

/**
 * Checks the validity and abbreviation status of a control record.
 * 
 * Why:
 * Providing a single function to determine if a record is valid or abbreviated centralizes logic
 * and reduces confusion throughout the codebase.
 */
function evaluateControlRecord(record: string): { 
  isValid: boolean; 
  isAbbreviation: boolean; 
  closestMatch?: string 
} {
  let closestMatch: string | undefined;
  let isAbbreviation = false;

  for (const validRecord of allowedControlRecords) {
    if (validRecord === record) {
      return { isValid: true, isAbbreviation: false };
    }

    if (validRecord.startsWith(record)) {
      isAbbreviation = true;
      closestMatch = validRecord;
      return { isValid: true, isAbbreviation, closestMatch };
    }

    if (!closestMatch && validRecord.startsWith(record.substring(0, 3))) {
      closestMatch = validRecord;
    }
  }

  return { 
    isValid: false, 
    isAbbreviation: false, 
    ...(closestMatch ? { closestMatch } : {})
  };
}

/**
 * Retrieves all control records from the given document text, ignoring commented lines.
 * 
 * Why:
 * We must find control records within the text while ignoring commented lines to ensure that
 * diagnostics are only produced for actual code lines.
 */
function locateControlRecordsInText(text: string): RegExpExecArray[] {
  const controlRecordRegex = /\$[A-Z]+\b/g;
  const findings: RegExpExecArray[] = [];
  let match: RegExpExecArray | null;

  // Replace commented lines with whitespace so indexes remain correct
  const sanitizedText = text
    .split('\n')
    .map(line => (line.trim().startsWith(';') ? ' '.repeat(line.length) : line))
    .join('\n');

  while ((match = controlRecordRegex.exec(sanitizedText)) !== null) {
    findings.push(match);
  }

  return findings;
}

/**
 * Creates a diagnostic for an invalid or abbreviated control record.
 * 
 * Why:
 * This encapsulates diagnostic creation logic, ensuring that all control record diagnostics are
 * constructed consistently and can be easily maintained.
 */
function generateDiagnosticForControlRecord(match: RegExpExecArray, textDocument: TextDocument): Diagnostic | null {
  const { isValid, isAbbreviation, closestMatch } = evaluateControlRecord(match[0]);

  if (isValid) {
    if (isAbbreviation && closestMatch) {
      return {
        severity: DiagnosticSeverity.Information,
        range: {
          start: textDocument.positionAt(match.index),
          end: textDocument.positionAt(match.index + match[0].length)
        },
        message: `Did you mean ${closestMatch}?`,
        code: "replace-abbreviation",
        source: 'NMTRAN Language Server'
      };
    }
    return null;
  } else {
    let message = `Invalid control record: ${match[0]}`;
    if (closestMatch) {
      message += `. Did you mean ${closestMatch}?`;
    }
    return {
      severity: DiagnosticSeverity.Error,
      range: {
        start: textDocument.positionAt(match.index),
        end: textDocument.positionAt(match.index + match[0].length)
      },
      message,
      source: 'NMTRAN Language Server'
    };
  }
}

/**
 * Validates continuation marker (&) usage in NMTRAN files.
 * 
 * Why:
 * NMTRAN uses FORTRAN-style continuation markers which have specific rules:
 * - & must appear at the end of lines (after content, before comments)
 * - Continuation lines should be properly structured
 * - No orphaned & markers
 */
function validateContinuationMarkers(document: TextDocument): { 
  isValid: boolean; 
  errors: Array<{ message: string; line: number; startChar: number; endChar: number }> 
} {
  const errors: Array<{ message: string; line: number; startChar: number; endChar: number }> = [];
  const lines = document.getText().split('\n');
  
  for (let lineNum = 0; lineNum < lines.length; lineNum++) {
    const line = lines[lineNum];
    if (!line) continue;

    // Find the position of the first semicolon (start of comment)
    // Any ampersand after this position should be ignored
    const commentStart = line.indexOf(';');

    // Skip lines that are entirely comments
    if (line.trim().startsWith(';')) continue;

    // Find all & characters in the line (before any comment)
    for (let charPos = 0; charPos < line.length; charPos++) {
      // Skip ampersands that appear within comments
      if (commentStart !== -1 && charPos >= commentStart) break;

      if (line.charAt(charPos) === '&') {
        // Check if & is at the end of the line (ignoring trailing whitespace and comments)
        const afterAmpersand = line.substring(charPos + 1);
        const isAtLineEnd = /^\s*(;.*)?$/.test(afterAmpersand);
        
        if (!isAtLineEnd) {
          // & is not at the end of line - this is invalid
          errors.push({
            message: 'Continuation marker (&) must appear at the end of the line',
            line: lineNum,
            startChar: charPos,
            endChar: charPos + 1
          });
        } else {
          // Valid & at end of line - check if there's a continuation line
          if (lineNum === lines.length - 1) {
            // & at end of last line - orphaned continuation marker
            errors.push({
              message: 'Orphaned continuation marker (&) at end of file',
              line: lineNum,
              startChar: charPos,
              endChar: charPos + 1
            });
          } else {
            // Check if next line exists and is not empty/comment-only
            const nextLine = lines[lineNum + 1];
            if (!nextLine || nextLine.trim() === '' || nextLine.trim().startsWith(';')) {
              errors.push({
                message: 'Continuation marker (&) not followed by continuation content',
                line: lineNum,
                startChar: charPos,
                endChar: charPos + 1
              });
            }
          }
        }
      }
    }
  }
  
  return { isValid: errors.length === 0, errors };
}

export {
  locateControlRecordsInText,
  generateDiagnosticForControlRecord,
  getFullControlRecordName,
  validateContinuationMarkers
};
