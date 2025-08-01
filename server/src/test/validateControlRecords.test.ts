/**
 * Tests for NMTRAN Control Record Validation
 * 
 * These tests verify that our validation logic correctly:
 * 1. Identifies valid control records
 * 2. Suggests corrections for abbreviations  
 * 3. Flags invalid control records
 * 4. Ignores commented lines
 * 
 * Run these tests whenever you modify validation logic.
 */

import {
  locateControlRecordsInText,
  generateDiagnosticForControlRecord,
  getFullControlRecordName
} from '../utils/validateControlRecords';
import { TextDocument } from 'vscode-languageserver-textdocument';

describe('NMTRAN Control Record Validation', () => {

  /**
   * Creates a mock TextDocument for testing
   */
  function createTestDocument(content: string): TextDocument {
    return TextDocument.create('test://test.mod', 'nmtran', 1, content);
  }

  test('Should find control records in simple text', () => {
  const text = '$PROBLEM Test Problem\n$THETA 1\n$OMEGA 0.1\n$ESTIMATION';
  const matches = locateControlRecordsInText(text);
  
  expect(matches).toHaveLength(4);
  expect(matches[0]?.[0]).toBe('$PROBLEM');
  expect(matches[1]?.[0]).toBe('$THETA');
  expect(matches[2]?.[0]).toBe('$OMEGA');
  expect(matches[3]?.[0]).toBe('$ESTIMATION');
  });

  test('Should ignore control records in comments', () => {
  const text = `$PROBLEM Test Problem
; This is a comment with $THETA
$THETA 1
; Another comment with $OMEGA
$ESTIMATION`;
  
  const matches = locateControlRecordsInText(text);
  expect(matches).toHaveLength(3);
  expect(matches[0]?.[0]).toBe('$PROBLEM');
  expect(matches[1]?.[0]).toBe('$THETA');
  expect(matches[2]?.[0]).toBe('$ESTIMATION');
  });

  test('Should expand abbreviations correctly', () => {
    expect(getFullControlRecordName('$EST')).toBe('$ESTIMATION');
    expect(getFullControlRecordName('$ESTM')).toBe('$ESTM'); // $ESTM is itself a valid abbreviation
    expect(getFullControlRecordName('$ESTIMATION')).toBe('$ESTIMATION');
    expect(getFullControlRecordName('$THETA')).toBe('$THETA');
    expect(getFullControlRecordName('$INVALID')).toBe('$INVALID'); // Should return as-is if no match
  });

  test('Should not flag valid control records', () => {
  const document = createTestDocument('$THETA 1');
  const matches = locateControlRecordsInText(document.getText());
  const diagnostic = matches[0] ? generateDiagnosticForControlRecord(matches[0], document) : null;
  
  expect(diagnostic).toBeNull();
  });

  test('Should suggest full name for abbreviations', () => {
  const document = createTestDocument('$EST METHOD=1');
  const matches = locateControlRecordsInText(document.getText());
  const diagnostic = matches[0] ? generateDiagnosticForControlRecord(matches[0], document) : null;
  
  expect(diagnostic).not.toBeNull();
  expect(diagnostic!.message).toContain('$ESTIMATION');
  });

  test('Should flag invalid control records', () => {
  const document = createTestDocument('$INVALID test');
  const matches = locateControlRecordsInText(document.getText());
  const diagnostic = matches[0] ? generateDiagnosticForControlRecord(matches[0], document) : null;
  
  expect(diagnostic).not.toBeNull();
  expect(diagnostic!.message).toContain('Invalid control record');
  });

  test('Should handle realistic NMTRAN file', () => {
  const text = `$PROBLEM Test Pharmacokinetic Model

$INPUT ID TIME DV MDV AMT EVID CMT

$DATA data.csv IGNORE=@

$SUBROUTINES ADVAN1 TRANS2

$PK
CL = THETA(1) * EXP(ETA(1))
V  = THETA(2) * EXP(ETA(2))

$ERROR
IPRED = F
Y = IPRED + IPRED*EPS(1)

$THETA
(0, 1)    ; CL
(0, 10)   ; V

$OMEGA
0.1       ; IIV CL
0.1       ; IIV V

$SIGMA
0.1       ; Residual error

$ESTIMATION METHOD=1 MAXEVAL=9999`;

  const matches = locateControlRecordsInText(text);
  
  // Should find all the control records
  const expectedRecords = ['$PROBLEM', '$INPUT', '$DATA', '$SUBROUTINES', '$PK', '$ERROR', '$THETA', '$OMEGA', '$SIGMA', '$ESTIMATION'];
  expect(matches).toHaveLength(expectedRecords.length);
  
  // Verify each record is found correctly
  matches.forEach((match, index) => {
    expect(match[0]).toBe(expectedRecords[index]);
  });
  });
});