import { ParameterParserFactory } from '../utils/parameterParser';

describe('ParameterParserFactory', () => {
  describe('parseParameterReference', () => {
    it('should parse THETA references correctly', () => {
      const result = ParameterParserFactory.parseParameterReference('CL = THETA(1) * EXP(ETA(1))', 10);
      expect(result).toEqual({ type: 'THETA', index: 1 });
    });

    it('should parse ETA references correctly', () => {
      const result = ParameterParserFactory.parseParameterReference('CL = THETA(1) * EXP(ETA(1))', 25);
      expect(result).toEqual({ type: 'ETA', index: 1 });
    });

    it('should parse EPS references correctly', () => {
      const result = ParameterParserFactory.parseParameterReference('Y = F + F*EPS(1)', 12);
      expect(result).toEqual({ type: 'EPS', index: 1 });
    });

    it('should return null when not on a parameter reference', () => {
      const result = ParameterParserFactory.parseParameterReference('CL = THETA(1)', 3);
      expect(result).toBeNull();
    });

    it('should handle multi-digit parameter indices', () => {
      const result = ParameterParserFactory.parseParameterReference('CL = THETA(15)', 10);
      expect(result).toEqual({ type: 'THETA', index: 15 });
    });
  });

  describe('findAllParameterReferences', () => {
    it('should find all parameter references in a line', () => {
      const line = 'CL = THETA(1) * EXP(ETA(1)) + THETA(2)';
      const result = ParameterParserFactory.findAllParameterReferences(line);
      
      expect(result).toHaveLength(3);
      expect(result).toContainEqual({ type: 'THETA', index: 1 });
      expect(result).toContainEqual({ type: 'ETA', index: 1 });
      expect(result).toContainEqual({ type: 'THETA', index: 2 });
    });

    it('should handle empty lines', () => {
      const result = ParameterParserFactory.findAllParameterReferences('');
      expect(result).toEqual([]);
    });

    it('should handle lines with no parameters', () => {
      const result = ParameterParserFactory.findAllParameterReferences('Y = F + IPRED');
      expect(result).toEqual([]);
    });

    it('should handle commented parameters', () => {
      const line = 'CL = THETA(1) ; Using THETA(2) for clearance';
      // By default excludes comments
      const result = ParameterParserFactory.findAllParameterReferences(line);
      expect(result).toHaveLength(1);
      expect(result).toContainEqual({ type: 'THETA', index: 1 });
      
      // Include comments
      const resultWithComments = ParameterParserFactory.findAllParameterReferences(line, false);
      expect(resultWithComments).toHaveLength(2);
      expect(resultWithComments).toContainEqual({ type: 'THETA', index: 1 });
      expect(resultWithComments).toContainEqual({ type: 'THETA', index: 2 });
    });
  });

  describe('parseThetaValue', () => {
    it('should parse simple THETA value', () => {
      const result = ParameterParserFactory.parseThetaValue('0.5');
      expect(result).toEqual({ start: 0, end: 3 });
    });

    it('should parse bounded THETA value (low, init, up)', () => {
      const result = ParameterParserFactory.parseThetaValue('(0.01, 0.5, 10)');
      expect(result?.start).toBeGreaterThan(0); // Should point to 0.5
    });

    it('should parse bounded THETA value (low, init)', () => {
      const result = ParameterParserFactory.parseThetaValue('(0.01, 0.5)');
      expect(result?.start).toBeGreaterThan(0); // Should point to 0.5
    });

    it('should handle FIX keyword', () => {
      const result = ParameterParserFactory.parseThetaValue('0.5 FIX');
      expect(result).toEqual({ start: 0, end: 3 });
    });

    it('should handle scientific notation', () => {
      const result = ParameterParserFactory.parseThetaValue('1.5E-3');
      expect(result).toEqual({ start: 0, end: 6 });
    });

    it('should return null for empty value', () => {
      const result = ParameterParserFactory.parseThetaValue('');
      expect(result).toBeNull();
    });
  });

  describe('extractBlockSize', () => {
    it('should extract BLOCK size correctly', () => {
      const result = ParameterParserFactory.extractBlockSize('$OMEGA BLOCK(2)');
      expect(result).toBe(2);
    });

    it('should handle BLOCK with values on same line', () => {
      const result = ParameterParserFactory.extractBlockSize('$OMEGA BLOCK(3) 0.1 0.05 0.2');
      expect(result).toBe(3);
    });

    it('should return null for non-BLOCK lines', () => {
      const result = ParameterParserFactory.extractBlockSize('$OMEGA 0.1');
      expect(result).toBeNull();
    });

    it('should be case insensitive', () => {
      const result = ParameterParserFactory.extractBlockSize('$omega block(4)');
      expect(result).toBe(4);
    });
  });
});