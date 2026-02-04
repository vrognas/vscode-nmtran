import { ParameterValidator } from '../utils/parameterValidator';
import { ParameterFactory } from '../factories/parameterFactory';

describe('ParameterValidator', () => {
  describe('validateScannerState', () => {
    it('should validate valid scanner state', () => {
      const state = ParameterFactory.createScannerState();
      state.counters.THETA = 5;
      state.counters.ETA = 3;
      state.counters.EPS = 2;

      const result = ParameterValidator.validateScannerState(state);

      expect(result.isValid).toBe(true);
      expect(result.errors).toHaveLength(0);
      expect(result.warnings).toHaveLength(0);
    });

    it('should detect counter limit violations', () => {
      const state = ParameterFactory.createScannerState();
      state.counters.THETA = 10000; // Exceeds limit of 9999
      
      const result = ParameterValidator.validateScannerState(state);

      expect(result.isValid).toBe(false);
      expect(result.errors).toContain('THETA count 10000 exceeds maximum 9999');
    });

    it('should detect invalid block matrix size', () => {
      const state = ParameterFactory.createScannerState();
      state.inBlockMatrix = true;
      state.blockMatrixSize = 0; // Invalid size

      const result = ParameterValidator.validateScannerState(state);

      expect(result.isValid).toBe(false);
      expect(result.errors).toContain('Invalid block matrix size');
    });

    it('should warn about negative block remaining count', () => {
      const state = ParameterFactory.createScannerState();
      state.inBlockMatrix = true;
      state.blockMatrixSize = 2;
      state.blockMatrixRemaining = -1; // Should warn

      const result = ParameterValidator.validateScannerState(state);

      expect(result.isValid).toBe(true);
      expect(result.warnings).toContain('Block matrix remaining count is negative');
    });
  });

  describe('validateParameterLine', () => {
    it('should validate normal parameter lines', () => {
      const validLines = [
        '$THETA 1.0 2.0 FIXED',
        '$OMEGA 0.1',
        '$SIGMA BLOCK(2) 0.04 0.027 0.05',
        '; Comment line',
        '0.1 0.2 0.3'
      ];

      validLines.forEach(line => {
        const result = ParameterValidator.validateParameterLine(line);
        expect(result.isValid).toBe(true);
      });
    });

    it('should reject invalid line input', () => {
      // eslint-disable-next-line @typescript-eslint/no-explicit-any -- testing invalid input handling
      const result = ParameterValidator.validateParameterLine(null as any);

      expect(result.isValid).toBe(false);
      expect(result.errors).toContain('Invalid line input');
    });

    it('should warn about extremely long lines', () => {
      const longLine = 'x'.repeat(1001);
      
      const result = ParameterValidator.validateParameterLine(longLine);
      
      expect(result.isValid).toBe(true);
      expect(result.warnings).toContain('Line is unusually long');
    });

    it('should warn about potentially invalid characters', () => {
      const lineWithInvalidChars = '$THETA 1.0 @ # %';
      
      const result = ParameterValidator.validateParameterLine(lineWithInvalidChars);
      
      expect(result.isValid).toBe(true);
      expect(result.warnings.length).toBeGreaterThan(0);
      expect(result.warnings[0]).toMatch(/potentially invalid characters/);
    });
  });

  describe('validateNumericValue', () => {
    it('should validate correct numeric formats', () => {
      const validNumbers = [
        '1.0',
        '-2.5',
        '+3.14',
        '1e-5',
        '2.3E+10',
        '0.001',
        '42'
      ];

      validNumbers.forEach(num => {
        const result = ParameterValidator.validateNumericValue(num);
        expect(result.isValid).toBe(true);
      });
    });

    it('should reject invalid numeric formats', () => {
      const invalidNumbers = [
        'not_a_number',
        '1.2.3',
        'e10',
        '1.0e',
        'FIXED'
      ];

      invalidNumbers.forEach(num => {
        const result = ParameterValidator.validateNumericValue(num);
        expect(result.isValid).toBe(false);
        expect(result.errors).toContain(`Invalid numeric format: ${num}`);
      });
    });

    it('should reject null/undefined input', () => {
      // eslint-disable-next-line @typescript-eslint/no-explicit-any -- testing invalid input handling
      const result = ParameterValidator.validateNumericValue(null as any);

      expect(result.isValid).toBe(false);
      expect(result.errors).toContain('Invalid numeric value input');
    });

    it('should warn about extreme values', () => {
      const veryLarge = '1e15';
      const verySmall = '1e-15';

      const largeResult = ParameterValidator.validateNumericValue(veryLarge);
      expect(largeResult.warnings).toContain(`Very large numeric value: ${veryLarge}`);

      const smallResult = ParameterValidator.validateNumericValue(verySmall);
      expect(smallResult.warnings).toContain(`Very small numeric value: ${verySmall}`);
    });

    it('should not warn about zero', () => {
      const result = ParameterValidator.validateNumericValue('0');
      
      expect(result.isValid).toBe(true);
      expect(result.warnings).toHaveLength(0);
    });
  });
});