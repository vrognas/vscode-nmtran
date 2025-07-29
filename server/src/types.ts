/**
 * Shared types and interfaces for the NMTRAN Language Server
 * 
 * Centralizes type definitions to improve maintainability and consistency
 */

export interface NMTRANSettings {
  maxNumberOfProblems: number;
  formatting?: {
    indentSize: number;
  };
}

export interface ControlRecordValidationResult {
  isValid: boolean;
  isAbbreviation: boolean;
  closestMatch?: string | undefined;
}

export interface NMTRANDocumentInfo {
  uri: string;
  languageId: string;
  version: number;
  content: string;
}

export const DEFAULT_SETTINGS: NMTRANSettings = {
  maxNumberOfProblems: 100,
  formatting: {
    indentSize: 2
  }
};