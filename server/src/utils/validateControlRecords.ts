import { Diagnostic, DiagnosticSeverity, TextDocument } from 'vscode-languageserver/node';
import { validControlRecords } from '../constants';

/**
 * Returns the fully spelled-out control record if the abbreviation is recognized.
 */
function getFullControlRecord(record: string): string {
  for (const validRecord of validControlRecords) {
    if (validRecord.startsWith(record)) {
      return validRecord;
    }
  }
  return record;
}

/**
 * Checks the validity of a control record. 
 * Returns whether it is valid, whether it is an abbreviation,
 * and optionally a closest match if relevant.
 */
function checkControlRecordValidity(record: string): { 
  isValid: boolean; 
  isAbbreviation: boolean; 
  closestMatch?: string 
} {
  let closestMatch: string | undefined;
  let isAbbreviation = false;

  for (const validRecord of validControlRecords) {
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

  return { isValid: false, isAbbreviation: false, closestMatch };
}

/**
 * Extracts all control records from a given text.
 */
function findControlRecordsInText(text: string): RegExpExecArray[] {
  const controlRecordPattern = /\$[A-Z]+\b/g;
  const matches: RegExpExecArray[] = [];
  let match: RegExpExecArray | null;

  const filteredText = text.split('\n')
    .filter(line => !line.trim().startsWith(';'))
    .join('\n');

  while ((match = controlRecordPattern.exec(filteredText)) !== null) {
    matches.push(match);
  }

  return matches;
}

/**
 * Creates a diagnostic for a given control record match, if it is invalid or abbreviated.
 */
function createDiagnosticForControlRecord(match: RegExpExecArray, textDocument: TextDocument): Diagnostic | null {
  const { isValid, isAbbreviation, closestMatch } = checkControlRecordValidity(match[0]);

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

export {
  findControlRecordsInText,
  createDiagnosticForControlRecord,
  getFullControlRecord
};
