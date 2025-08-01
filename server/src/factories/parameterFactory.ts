/**
 * Factory for creating parameter-related objects
 */

import { ParameterLocation, ScannerState } from '../services/ParameterScanner';
import { PARAMETER_TYPES } from '../constants/parameters';

export class ParameterFactory {
  /**
   * Create a new parameter location
   */
  static createLocation(
    type: keyof typeof PARAMETER_TYPES,
    index: number,
    line: number,
    startChar?: number,
    endChar?: number
  ): ParameterLocation {
    const location: ParameterLocation = {
      type: PARAMETER_TYPES[type] as 'THETA' | 'ETA' | 'EPS',
      index,
      line
    };
    
    if (startChar !== undefined) {
      location.startChar = startChar;
    }
    if (endChar !== undefined) {
      location.endChar = endChar;
    }
    
    return location;
  }

  /**
   * Create initial scanner state
   */
  static createScannerState(): ScannerState {
    return {
      currentBlockType: null,
      inBlockMatrix: false,
      blockMatrixRemaining: 0,
      blockMatrixSize: 0,
      blockElementsSeen: 0,
      blockDiagonalsSeen: 0,
      blockElements: [],
      counters: { 
        THETA: 0, 
        ETA: 0, 
        EPS: 0 
      },
      blockFixedKeywords: []
    };
  }

  /**
   * Reset block-related state
   */
  static resetBlockState(state: ScannerState): void {
    state.inBlockMatrix = false;
    state.blockMatrixRemaining = 0;
    state.blockMatrixSize = 0;
    state.blockElementsSeen = 0;
    state.blockDiagonalsSeen = 0;
    state.blockElements = [];
    state.blockFixedKeywords = [];
  }
}