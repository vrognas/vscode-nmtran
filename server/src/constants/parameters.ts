/**
 * Constants for NMTRAN parameter handling
 */

export const PARAMETER_TYPES = {
  THETA: 'THETA',
  ETA: 'ETA',
  EPS: 'EPS'
} as const;

export type ParameterType = typeof PARAMETER_TYPES[keyof typeof PARAMETER_TYPES];

export const KEYWORDS = {
  FIX: 'FIX',
  FIXED: 'FIXED',
  SAME: 'SAME',
  BLOCK: 'BLOCK',
  DIAGONAL: 'DIAGONAL',
  VARIANCE: 'VARIANCE',
  CORRELATION: 'CORRELATION',
  CHOLESKY: 'CHOLESKY',
  STANDARD: 'STANDARD'
} as const;

export const LIMITS = {
  MAX_PARAMETER_INDEX: 9999,
  MAX_BLOCK_SIZE: 100,
  MAX_LINE_LENGTH: 10000,
  SCAN_CACHE_SIZE: 50
} as const;

export const DEBOUNCE_DELAYS = {
  DIAGNOSTICS: 500,
  FORMATTING: 100
} as const;