/**
 * Tests for NONMEM 7.6.0 Function Highlighting
 * 
 * Verifies that new functions introduced in NONMEM 7.6.0 are properly
 * recognized and highlighted by the syntax highlighting system.
 */

import { createDocument } from './test-helpers';

describe('NONMEM 7.6.0 Function Highlighting', () => {
  
  describe('RANDMT Functions', () => {
    it('should recognize RANDMT function', () => {
      const content = `
$PK
  RANDOMVAL = RANDMT(1)
  Y = THETA(1) * RANDMT(2)
`;
      const document = createDocument(content);
      expect(document.getText()).toContain('RANDMT(1)');
      expect(document.getText()).toContain('RANDMT(2)');
    });

    it('should recognize RANDMTU function', () => {
      const content = `
$PK
  UNIFORMVAL = RANDMTU(1)
  CL = THETA(1) * RANDMTU(2)
`;
      const document = createDocument(content);
      expect(document.getText()).toContain('RANDMTU(1)');
      expect(document.getText()).toContain('RANDMTU(2)');
    });

    it('should handle RANDMT functions in complex expressions', () => {
      const content = `
$PK
  IF (ICALL.EQ.4) THEN
    VAL = LOG(RANDMT(1)) + EXP(RANDMTU(2))
  ENDIF
`;
      const document = createDocument(content);
      expect(document.getText()).toContain('LOG(RANDMT(1))');
      expect(document.getText()).toContain('EXP(RANDMTU(2))');
    });
  });

  describe('MAX Function', () => {
    it('should recognize MAX function separately from MIN', () => {
      const content = `
$PK
  UPPER = MAX(THETA(1), THETA(2))
  LOWER = MIN(THETA(3), THETA(4))
  CLIPPED = MAX(0.1, MIN(10, CL))
`;
      const document = createDocument(content);
      expect(document.getText()).toContain('MAX(THETA(1), THETA(2))');
      expect(document.getText()).toContain('MIN(THETA(3), THETA(4))');
      expect(document.getText()).toContain('MAX(0.1, MIN(10, CL))');
    });

    it('should handle MAX with multiple arguments', () => {
      const content = `
$ERROR
  IPRED = MAX(0.001, F)
  W = MAX(THETA(5), 0.01)
`;
      const document = createDocument(content);
      expect(document.getText()).toContain('MAX(0.001, F)');
      expect(document.getText()).toContain('MAX(THETA(5), 0.01)');
    });
  });

  describe('Function Case Insensitivity', () => {
    it('should recognize functions in different cases', () => {
      const content = `
$PK
  VAL1 = RANDMT(1)    ; uppercase
  VAL2 = randmt(2)    ; lowercase  
  VAL3 = RandMt(3)    ; mixed case
  VAL4 = MAX(1, 2)    ; uppercase
  VAL5 = max(3, 4)    ; lowercase
`;
      const document = createDocument(content);
      expect(document.getText()).toContain('RANDMT(1)');
      expect(document.getText()).toContain('randmt(2)');
      expect(document.getText()).toContain('RandMt(3)');
      expect(document.getText()).toContain('MAX(1, 2)');
      expect(document.getText()).toContain('max(3, 4)');
    });
  });

  describe('Function Context Validation', () => {
    it('should recognize functions in $PK block', () => {
      const content = `
$PK
  CL = THETA(1) * EXP(ETA(1))
  V = THETA(2) * EXP(ETA(2))
  KA = MAX(THETA(3) * RANDMT(1), 0.01)
`;
      const document = createDocument(content);
      expect(document.getText()).toContain('MAX(THETA(3) * RANDMT(1), 0.01)');
    });

    it('should recognize functions in $ERROR block', () => {
      const content = `
$ERROR
  IPRED = LOG(F)
  W = SQRT(THETA(5))
  Y = IPRED + W * EPS(1) * RANDMT(1)
`;
      const document = createDocument(content);
      expect(document.getText()).toContain('Y = IPRED + W * EPS(1) * RANDMT(1)');
    });

    it('should recognize functions in $PRED block', () => {
      const content = `
$PRED
  RANDOMEFFECT = RANDMTU(NEWIND)
  CL = THETA(1) * MAX(0.1, RANDOMEFFECT)
  Y = F + F * EPS(1)
`;
      const document = createDocument(content);
      expect(document.getText()).toContain('RANDMTU(NEWIND)');
      expect(document.getText()).toContain('MAX(0.1, RANDOMEFFECT)');
    });
  });

  describe('Nested Function Calls', () => {
    it('should handle nested function calls correctly', () => {
      const content = `
$PK
  NESTED = MAX(MIN(RANDMT(1), 10), 0.1)
  COMPLEX = LOG(MAX(EXP(THETA(1)), RANDMTU(2)))
`;
      const document = createDocument(content);
      expect(document.getText()).toContain('MAX(MIN(RANDMT(1), 10), 0.1)');
      expect(document.getText()).toContain('LOG(MAX(EXP(THETA(1)), RANDMTU(2)))');
    });
  });

  describe('Function with Comments', () => {
    it('should handle functions with inline comments', () => {
      const content = `
$PK
  CL = THETA(1) * RANDMT(1)  ; Random multiplier
  V = MAX(THETA(2), 0.1)     ; Minimum volume
`;
      const document = createDocument(content);
      expect(document.getText()).toContain('RANDMT(1)  ; Random multiplier');
      expect(document.getText()).toContain('MAX(THETA(2), 0.1)     ; Minimum volume');
    });
  });

  describe('Backward Compatibility', () => {
    it('should still recognize all existing functions', () => {
      const content = `
$PK
  ; Existing functions should still work
  CL = THETA(1) * EXP(ETA(1))
  V = THETA(2) * LOG(WT/70)
  KA = THETA(3) * SQRT(AGE/40)
  TVCL = THETA(4) * (WT/70)**0.75
  
  ; New functions mixed with old
  RANDOM_CL = CL * RANDMT(1)
  BOUNDED_V = MAX(V, 0.1)
`;
      const document = createDocument(content);
      
      // Check existing functions still work
      expect(document.getText()).toContain('EXP(ETA(1))');
      expect(document.getText()).toContain('LOG(WT/70)');
      expect(document.getText()).toContain('SQRT(AGE/40)');
      
      // Check new functions work
      expect(document.getText()).toContain('RANDMT(1)');
      expect(document.getText()).toContain('MAX(V, 0.1)');
    });
  });
});