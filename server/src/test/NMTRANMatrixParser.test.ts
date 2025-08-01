import { NMTRANMatrixParser } from '../utils/NMTRANMatrixParser';

describe('NMTRANMatrixParser', () => {
  describe('getDiagonalPosition', () => {
    it('should calculate correct diagonal positions', () => {
      expect(NMTRANMatrixParser.getDiagonalPosition(1)).toBe(0);  // (1,1)
      expect(NMTRANMatrixParser.getDiagonalPosition(2)).toBe(2);  // (2,2)
      expect(NMTRANMatrixParser.getDiagonalPosition(3)).toBe(5);  // (3,3)
      expect(NMTRANMatrixParser.getDiagonalPosition(4)).toBe(9);  // (4,4)
      expect(NMTRANMatrixParser.getDiagonalPosition(5)).toBe(14); // (5,5)
    });
  });

  describe('parseBlockValues', () => {
    it('should parse inline BLOCK values', () => {
      const line = '$OMEGA BLOCK(2) 0.0444 0.027 0.0241';
      const values = NMTRANMatrixParser.parseBlockValues(line);
      
      expect(values).toEqual(['0.0444', '0.027', '0.0241']);
    });

    it('should ignore comments', () => {
      const line = '$OMEGA BLOCK(2) 0.0444 0.027 0.0241 ; IIV on CL and V';
      const values = NMTRANMatrixParser.parseBlockValues(line);
      
      expect(values).toEqual(['0.0444', '0.027', '0.0241']);
    });

    it('should handle scientific notation', () => {
      const line = '$OMEGA BLOCK(2) 1.5E-3 2E+2 3.14E0';
      const values = NMTRANMatrixParser.parseBlockValues(line);
      
      expect(values).toEqual(['1.5E-3', '2E+2', '3.14E0']);
    });

    it('should handle negative values', () => {
      const line = '$OMEGA BLOCK(2) -0.5 +1.2 -3E-2';
      const values = NMTRANMatrixParser.parseBlockValues(line);
      
      expect(values).toEqual(['-0.5', '+1.2', '-3E-2']);
    });
  });

  describe('extractDiagonalElements', () => {
    it('should extract diagonal elements from BLOCK(2)', () => {
      const values = ['0.0444', '0.027', '0.0241'];
      const diagonals = NMTRANMatrixParser.extractDiagonalElements(2, values);
      
      expect(diagonals).toEqual(['0.0444', '0.0241']);
    });

    it('should extract diagonal elements from BLOCK(3)', () => {
      const values = ['0.1', '0.05', '0.2', '0.01', '0.03', '0.15'];
      const diagonals = NMTRANMatrixParser.extractDiagonalElements(3, values);
      
      expect(diagonals).toEqual(['0.1', '0.2', '0.15']);
    });

    it('should handle insufficient values gracefully', () => {
      const values = ['0.1', '0.05'];
      const diagonals = NMTRANMatrixParser.extractDiagonalElements(3, values);
      
      expect(diagonals).toEqual(['0.1']); // Only first diagonal found
    });
  });

  describe('isDiagonalElement', () => {
    it('should identify diagonal positions correctly', () => {
      expect(NMTRANMatrixParser.isDiagonalElement(0)).toBe(1);   // (1,1)
      expect(NMTRANMatrixParser.isDiagonalElement(2)).toBe(2);   // (2,2)
      expect(NMTRANMatrixParser.isDiagonalElement(5)).toBe(3);   // (3,3)
      expect(NMTRANMatrixParser.isDiagonalElement(9)).toBe(4);   // (4,4)
      expect(NMTRANMatrixParser.isDiagonalElement(14)).toBe(5);  // (5,5)
    });

    it('should return null for off-diagonal positions', () => {
      expect(NMTRANMatrixParser.isDiagonalElement(1)).toBeNull();  // (2,1)
      expect(NMTRANMatrixParser.isDiagonalElement(3)).toBeNull();  // (3,1)
      expect(NMTRANMatrixParser.isDiagonalElement(4)).toBeNull();  // (3,2)
    });
  });

  describe('getTriangularMatrixSize', () => {
    it('should calculate correct matrix sizes', () => {
      expect(NMTRANMatrixParser.getTriangularMatrixSize(1)).toBe(1);
      expect(NMTRANMatrixParser.getTriangularMatrixSize(2)).toBe(3);
      expect(NMTRANMatrixParser.getTriangularMatrixSize(3)).toBe(6);
      expect(NMTRANMatrixParser.getTriangularMatrixSize(4)).toBe(10);
      expect(NMTRANMatrixParser.getTriangularMatrixSize(5)).toBe(15);
    });
  });

  describe('parseMatrixBlock', () => {
    it('should parse inline BLOCK matrix', () => {
      const lines = ['$OMEGA BLOCK(2) 0.0444 0.027 0.0241'];
      const result = NMTRANMatrixParser.parseMatrixBlock(lines, 0);
      
      expect(result).toEqual({
        blockSize: 2,
        values: ['0.0444', '0.027', '0.0241'],
        diagonalElements: ['0.0444', '0.0241'],
        endLine: 0
      });
    });

    it('should parse multi-line BLOCK matrix', () => {
      const lines = [
        '$OMEGA BLOCK(3)',
        '0.1',
        '0.05 0.2',
        '0.01 0.03 0.15'
      ];
      const result = NMTRANMatrixParser.parseMatrixBlock(lines, 0);
      
      expect(result).toEqual({
        blockSize: 3,
        values: ['0.1', '0.05', '0.2', '0.01', '0.03', '0.15'],
        diagonalElements: ['0.1', '0.2', '0.15'],
        endLine: 3
      });
    });

    it('should stop at next control record', () => {
      const lines = [
        '$OMEGA BLOCK(2)',
        '0.1',
        '0.05 0.2',
        '$SIGMA 0.01'
      ];
      const result = NMTRANMatrixParser.parseMatrixBlock(lines, 0);
      
      expect(result?.values).toEqual(['0.1', '0.05', '0.2']);
      expect(result?.endLine).toBe(2);
    });
  });

  describe('validateBlockMatrix', () => {
    it('should validate correct BLOCK(2) matrix', () => {
      const isValid = NMTRANMatrixParser.validateBlockMatrix(2, ['0.1', '0.05', '0.2']);
      expect(isValid).toBe(true);
    });

    it('should invalidate incomplete BLOCK(3) matrix', () => {
      const isValid = NMTRANMatrixParser.validateBlockMatrix(3, ['0.1', '0.05', '0.2']);
      expect(isValid).toBe(false); // Expects 6 values, got 3
    });

    it('should invalidate excess values', () => {
      const isValid = NMTRANMatrixParser.validateBlockMatrix(2, ['0.1', '0.05', '0.2', '0.3']);
      expect(isValid).toBe(false); // Expects 3 values, got 4
    });
  });
});