/**
 * Test to understand BLOCK(1) highlighting behavior with different spacing patterns
 * 
 * This test focuses on the specific issue where:
 * Line 54: `$OMEGA  BLOCK(1) 0.0444    ; IIV (CL-V)` - works correctly
 * Line 57: `$OMEGA  BLOCK(1) 0.0165           ; IOV CL` - highlights entire line  
 * Line 59: `$OMEGA  BLOCK(1)  0.495           ; IOV KA` - highlights entire line
 * 
 * The difference appears to be in spacing patterns.
 */

import { DefinitionService } from '../services/definitionService';
import { Connection } from 'vscode-languageserver/node';
import { TextDocument } from 'vscode-languageserver-textdocument';

describe('BLOCK(1) Highlighting Test', () => {
  let mockConnection: Connection;
  let definitionService: DefinitionService;

  beforeEach(() => {
    mockConnection = {
      console: {
        log: jest.fn(),
        error: jest.fn(),
        warn: jest.fn(),
        info: jest.fn()
      }
    } as any;
    
    definitionService = new DefinitionService(mockConnection);
  });

  // Helper function to create a test document
  function createTestDocument(content: string): TextDocument {
    return TextDocument.create('test://test.mod', 'nmtran', 1, content);
  }

  // Test the private findParameterValuePosition method by accessing it via reflection
  function testFindParameterValuePosition(line: string, paramPosition: number) {
    // Access private method for testing
    const method = (definitionService as any).findParameterValuePosition;
    return method.call(definitionService, line, paramPosition);
  }

  describe('findParameterValuePosition with exact lines from test file', () => {
    it('should correctly handle BLOCK(1) with normal spacing (working case - line 2)', () => {
      const line = '$OMEGA  BLOCK(1) 3.0           ; IIV KA';
      
      const result = testFindParameterValuePosition(line, 1);
      
      expect(result).not.toBeNull();
      expect(result?.start).toBeDefined();
      expect(result?.end).toBeDefined();
      
      // Extract the highlighted text to verify it's correct
      const highlightedText = line.substring(result?.start || 0, result?.end || 0);
      expect(highlightedText).toBe('3.0');
      
      console.log(`Working case (line 2) - Line: "${line}"`);
      console.log(`Working case (line 2) - Highlighted: "${highlightedText}" at positions ${result?.start}-${result?.end}`);
    });

    it('should correctly handle BLOCK(1) with extra spaces before comment (failing case - line 4)', () => {
      const line = '$OMEGA  BLOCK(1) 0.0165           ; IOV CL';
      
      const result = testFindParameterValuePosition(line, 1);
      
      expect(result).not.toBeNull();
      expect(result?.start).toBeDefined();
      expect(result?.end).toBeDefined();
      
      // Extract the highlighted text to verify it's correct
      const highlightedText = line.substring(result?.start || 0, result?.end || 0);
      expect(highlightedText).toBe('0.0165');
      
      console.log(`Failing case (line 4) - Line: "${line}"`);
      console.log(`Failing case (line 4) - Highlighted: "${highlightedText}" at positions ${result?.start}-${result?.end}`);
    });

    it('should correctly handle BLOCK(1) with double space after BLOCK(1) (failing case - line 7)', () => {
      const line = '$OMEGA  BLOCK(1)  0.495           ; IOV KA';
      
      const result = testFindParameterValuePosition(line, 1);
      
      expect(result).not.toBeNull();
      expect(result?.start).toBeDefined();
      expect(result?.end).toBeDefined();
      
      // Extract the highlighted text to verify it's correct
      const highlightedText = line.substring(result?.start || 0, result?.end || 0);
      expect(highlightedText).toBe('0.495');
      
      console.log(`Failing case (line 7) - Line: "${line}"`);
      console.log(`Failing case (line 7) - Highlighted: "${highlightedText}" at positions ${result?.start}-${result?.end}`);
    });
  });

  describe('Content parsing analysis', () => {
    it('should show how content parsing differs between cases', () => {
      const cases = [
        '$OMEGA  BLOCK(1) 0.0444    ; IIV (CL-V)',     // Working
        '$OMEGA  BLOCK(1) 0.0165           ; IOV CL',   // Failing
        '$OMEGA  BLOCK(1)  0.495           ; IOV KA'    // Failing
      ];

      cases.forEach((line, index) => {
        console.log(`\n=== Case ${index + 1}: ${index === 0 ? 'WORKING' : 'FAILING'} ===`);
        console.log(`Full line: "${line}"`);
        
        // Simulate the content parsing logic from findParameterValuePosition
        const codePartEnd = line.indexOf(';');
        const codePart = codePartEnd !== -1 ? line.substring(0, codePartEnd) : line;
        console.log(`Code part: "${codePart}"`);
        
        // Remove control record prefix
        let contentPart = codePart.replace(/^\s*\$\w+\s*/i, '');
        console.log(`After removing control record: "${contentPart}"`);
        
        // Remove BLOCK(n) pattern
        contentPart = contentPart.replace(/^BLOCK\(\d+\)\s*/i, '');
        console.log(`After removing BLOCK pattern: "${contentPart}"`);
        
        // Find all numeric values
        const numericPattern = /[\d\-+][\d\-+.eE]*/g;
        const matches = [];
        let match;
        
        const searchOffset = codePart.length - contentPart.length;
        console.log(`Search offset: ${searchOffset}`);
        
        while ((match = numericPattern.exec(contentPart)) !== null) {
          matches.push({
            value: match[0],
            start: match.index + searchOffset,
            end: match.index + match[0].length + searchOffset
          });
        }
        
        console.log(`Numeric matches:`, matches);
        
        if (matches.length > 0) {
          const firstMatch = matches[0];
          const highlightedText = line.substring(firstMatch?.start || 0, firstMatch?.end || 0);
          console.log(`Would highlight: "${highlightedText}" at positions ${firstMatch?.start}-${firstMatch?.end}`);
        }
      });
    });
  });

  describe('Regex testing', () => {
    it('should test the BLOCK pattern regex behavior', () => {
      const lines = [
        '$OMEGA  BLOCK(1) 0.0444    ; IIV (CL-V)',
        '$OMEGA  BLOCK(1) 0.0165           ; IOV CL',
        '$OMEGA  BLOCK(1)  0.495           ; IOV KA'
      ];

      const blockPattern = /^BLOCK\(\d+\)\s*/i;
      
      lines.forEach((line, index) => {
        console.log(`\n=== Testing regex on line ${index + 1} ===`);
        console.log(`Line: "${line}"`);
        
        // First get the content part after removing control record
        const codePart = line.substring(0, line.indexOf(';'));
        const afterControlRecord = codePart.replace(/^\s*\$\w+\s*/i, '');
        console.log(`After control record removal: "${afterControlRecord}"`);
        
        const match = afterControlRecord.match(blockPattern);
        if (match) {
          console.log(`BLOCK match: "${match[0]}" at index ${match.index}`);
          const afterBlock = afterControlRecord.replace(blockPattern, '');
          console.log(`After BLOCK removal: "${afterBlock}"`);
        } else {
          console.log(`No BLOCK pattern found`);
        }
      });
    });
  });

  describe('Full document test with actual definition service workflow', () => {
    it('should test the full document with all problematic lines', () => {
      const documentContent = `$OMEGA  BLOCK(2) 0.0444 0.027 0.0241    ; IIV (CL-V)
$OMEGA  BLOCK(1) 3.0           ; IIV KA

$OMEGA  BLOCK(1) 0.0165           ; IOV CL
$OMEGA  BLOCK(1)  SAME         ; IOV CL

$OMEGA  BLOCK(1)  0.495           ; IOV KA
$OMEGA  BLOCK(1)  SAME         ; IOV`;
      
      const document = createTestDocument(documentContent);
      
      // Test line 1 (line 0 in 0-indexed): should find ETA(3) on the BLOCK(2) line
      const line1Position = { line: 1, character: 10 }; // Position in BLOCK(1) 3.0
      const line1Result = definitionService.provideDefinition(document, line1Position);
      console.log(`\nLine 1 (working case) result:`, line1Result);
      
      // Test line 3 (line 3 in 0-indexed): should find ETA parameter on the BLOCK(1) 0.0165 line
      const line3Position = { line: 3, character: 20 }; // Position in BLOCK(1) 0.0165
      const line3Result = definitionService.provideDefinition(document, line3Position);
      console.log(`\nLine 3 (failing case) result:`, line3Result);
      
      // Test line 6 (line 6 in 0-indexed): should find ETA parameter on the BLOCK(1) 0.495 line
      const line6Position = { line: 6, character: 20 }; // Position in BLOCK(1) 0.495
      const line6Result = definitionService.provideDefinition(document, line6Position);
      console.log(`\nLine 6 (failing case) result:`, line6Result);
    });

    it('should test the enhanceLocationsWithValuePositions method', () => {
      const documentContent = `$OMEGA  BLOCK(2) 0.0444 0.027 0.0241    ; IIV (CL-V)
$OMEGA  BLOCK(1) 3.0           ; IIV KA

$OMEGA  BLOCK(1) 0.0165           ; IOV CL
$OMEGA  BLOCK(1)  SAME         ; IOV CL

$OMEGA  BLOCK(1)  0.495           ; IOV KA
$OMEGA  BLOCK(1)  SAME         ; IOV`;
      
      const document = createTestDocument(documentContent);
      const lines = documentContent.split('\n');
      
      // Access the private scanAllParameters method to see what it returns
      const scanMethod = (definitionService as any).scanAllParameters;
      const allParams = scanMethod.call(definitionService, document);
      
      console.log(`\nAll scanned parameters:`, allParams);
      
      // Look for the specific parameters we're interested in
      const etaParams = allParams.filter((p: any) => p.type === 'ETA');
      console.log(`\nETA parameters:`, etaParams);
      
      // Verify the highlighted text for each parameter
      etaParams.forEach((param: any) => {
        const line = lines[param.line];
        if (line) {
          const highlightedText = line.substring(param.startChar, param.endChar);
          console.log(`ETA(${param.index}) on line ${param.line}: "${highlightedText}" (positions ${param.startChar}-${param.endChar})`);
          console.log(`  Full line: "${line}"`);
        }
      });
      
      // Test the enhanceLocationsWithValuePositions method
      const enhanceMethod = (definitionService as any).enhanceLocationsWithValuePositions;
      enhanceMethod.call(definitionService, document, etaParams);
      
      console.log(`\nETA parameters after enhancement:`, etaParams);
    });
  });
});