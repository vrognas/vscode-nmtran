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

// =================================================================
// HELPER FUNCTIONS
// =================================================================

/**
 * Creates a mock TextDocument for testing
 */
function createTestDocument(content: string): TextDocument {
  return TextDocument.create('test://test.mod', 'nmtran', 1, content);
}

/**
 * Simple test runner - replace with proper framework if needed
 */
function runTest(testName: string, testFn: () => void) {
  try {
    testFn();
    console.log(`✅ ${testName}`);
  } catch (error) {
    console.log(`❌ ${testName}: ${error}`);
  }
}

function assertEqual(actual: unknown, expected: unknown, message?: string) {
  if (actual !== expected) {
    throw new Error(message || `Expected ${expected}, got ${actual}`);
  }
}

function assertTrue(condition: boolean, message?: string) {
  if (!condition) {
    throw new Error(message || 'Expected true, got false');
  }
}

// =================================================================
// TESTS
// =================================================================

// Test: Find control records in text
runTest('Should find control records in simple text', () => {
  const text = '$PROBLEM Test Problem\n$THETA 1\n$OMEGA 0.1\n$ESTIMATION';
  const matches = locateControlRecordsInText(text);
  
  assertEqual(matches.length, 4, 'Should find 4 control records');
  assertEqual(matches[0][0], '$PROBLEM');
  assertEqual(matches[1][0], '$THETA');
  assertEqual(matches[2][0], '$OMEGA');
  assertEqual(matches[3][0], '$ESTIMATION');
});

// Test: Ignore commented lines
runTest('Should ignore control records in comments', () => {
  const text = `$PROBLEM Test Problem
; This is a comment with $THETA
$THETA 1
; Another comment with $OMEGA
$ESTIMATION`;
  
  const matches = locateControlRecordsInText(text);
  assertEqual(matches.length, 3, 'Should find 3 control records (ignoring commented ones)');
  assertEqual(matches[0][0], '$PROBLEM');
  assertEqual(matches[1][0], '$THETA');
  assertEqual(matches[2][0], '$ESTIMATION');
});

// Test: Get full control record name
runTest('Should expand abbreviations correctly', () => {
  assertEqual(getFullControlRecordName('$EST'), '$ESTIMATION');
  assertEqual(getFullControlRecordName('$ESTM'), '$ESTM'); // $ESTM is itself a valid abbreviation
  assertEqual(getFullControlRecordName('$ESTIMATION'), '$ESTIMATION');
  assertEqual(getFullControlRecordName('$THETA'), '$THETA');
  assertEqual(getFullControlRecordName('$INVALID'), '$INVALID'); // Should return as-is if no match
});

// Test: Valid control record produces no diagnostic
runTest('Should not flag valid control records', () => {
  const document = createTestDocument('$THETA 1');
  const matches = locateControlRecordsInText(document.getText());
  const diagnostic = generateDiagnosticForControlRecord(matches[0], document);
  
  assertEqual(diagnostic, null, 'Valid control record should not produce diagnostic');
});

// Test: Abbreviated control record produces info diagnostic
runTest('Should suggest full name for abbreviations', () => {
  const document = createTestDocument('$EST METHOD=1');
  const matches = locateControlRecordsInText(document.getText());
  const diagnostic = generateDiagnosticForControlRecord(matches[0], document);
  
  assertTrue(diagnostic !== null, 'Should produce diagnostic for abbreviation');
  assertTrue(diagnostic!.message.includes('$ESTIMATION'), 'Should suggest $ESTIMATION');
});

// Test: Invalid control record produces error diagnostic  
runTest('Should flag invalid control records', () => {
  const document = createTestDocument('$INVALID test');
  const matches = locateControlRecordsInText(document.getText());
  const diagnostic = generateDiagnosticForControlRecord(matches[0], document);
  
  assertTrue(diagnostic !== null, 'Should produce diagnostic for invalid record');
  assertTrue(diagnostic!.message.includes('Invalid control record'), 'Should indicate invalid record');
});

// Test: Real NMTRAN file structure
runTest('Should handle realistic NMTRAN file', () => {
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
  assertEqual(matches.length, expectedRecords.length, `Should find ${expectedRecords.length} control records`);
  
  // Verify each record is found correctly
  matches.forEach((match, index) => {
    assertEqual(match[0], expectedRecords[index], `Record ${index} should be ${expectedRecords[index]}`);
  });
});

console.log('\n🧪 NMTRAN Validation Tests Complete\n');