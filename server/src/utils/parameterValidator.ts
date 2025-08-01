/**
 * Parameter validation utilities for NMTRAN parameter scanning
 */

import { ScannerState } from '../services/ParameterScanner';
import { LIMITS } from '../constants/parameters';

export interface ValidationResult {
  isValid: boolean;
  errors: string[];
  warnings: string[];
}

export class ParameterValidator {
  /**
   * Validate scanner state for consistency
   */
  static validateScannerState(state: ScannerState): ValidationResult {
    const errors: string[] = [];
    const warnings: string[] = [];

    // Check counter limits
    if (state.counters.THETA > LIMITS.MAX_PARAMETER_INDEX) {
      errors.push(`THETA count ${state.counters.THETA} exceeds maximum ${LIMITS.MAX_PARAMETER_INDEX}`);
    }
    if (state.counters.ETA > LIMITS.MAX_PARAMETER_INDEX) {
      errors.push(`ETA count ${state.counters.ETA} exceeds maximum ${LIMITS.MAX_PARAMETER_INDEX}`);
    }
    if (state.counters.EPS > LIMITS.MAX_PARAMETER_INDEX) {
      errors.push(`EPS count ${state.counters.EPS} exceeds maximum ${LIMITS.MAX_PARAMETER_INDEX}`);
    }

    // Check block matrix consistency
    if (state.inBlockMatrix) {
      if (state.blockMatrixSize <= 0) {
        errors.push('Invalid block matrix size');
      }
      if (state.blockElementsSeen < 0) {
        errors.push('Invalid block elements count');
      }
      if (state.blockMatrixRemaining < 0) {
        warnings.push('Block matrix remaining count is negative');
      }
    }

    return {
      isValid: errors.length === 0,
      errors,
      warnings
    };
  }

  /**
   * Validate line content for parameter parsing
   */
  static validateParameterLine(line: string): ValidationResult {
    const errors: string[] = [];
    const warnings: string[] = [];

    if (!line || typeof line !== 'string') {
      errors.push('Invalid line input');
      return { isValid: false, errors, warnings };
    }

    // Check for extremely long lines that might cause performance issues
    if (line.length > 1000) {
      warnings.push('Line is unusually long');
    }

    // Check for valid numeric patterns
    const numericPattern = /[^\d\-+.eE\s(),;$A-Z]/gi;
    const invalidChars = line.match(numericPattern);
    if (invalidChars && invalidChars.length > 0) {
      warnings.push(`Line contains potentially invalid characters: ${[...new Set(invalidChars)].join(', ')}`);
    }

    return {
      isValid: errors.length === 0,
      errors,
      warnings
    };
  }

  /**
   * Validate numeric value format
   */
  static validateNumericValue(value: string): ValidationResult {
    const errors: string[] = [];
    const warnings: string[] = [];

    if (!value || typeof value !== 'string') {
      errors.push('Invalid numeric value input');
      return { isValid: false, errors, warnings };
    }

    // Check if it's a valid number format
    const numericPattern = /^[+-]?(\d+\.?\d*|\.\d+)([eE][+-]?\d+)?$/;
    if (!numericPattern.test(value)) {
      errors.push(`Invalid numeric format: ${value}`);
    }

    // Check for extreme values
    const numValue = parseFloat(value);
    if (!isNaN(numValue)) {
      if (Math.abs(numValue) > 1e10) {
        warnings.push(`Very large numeric value: ${value}`);
      }
      if (Math.abs(numValue) < 1e-10 && numValue !== 0) {
        warnings.push(`Very small numeric value: ${value}`);
      }
    }

    return {
      isValid: errors.length === 0,
      errors,
      warnings
    };
  }
}