import { ParameterScanner } from '../services/ParameterScanner';
import { TextDocument } from 'vscode-languageserver-textdocument';

// Test the exact scenario from moxonidine.mod
const testContent = `$OMEGA  BLOCK(2) 0.0444 0.027 0.0241    ; IIV (CL-V)

$OMEGA  BLOCK(2) 0.1 0.027 0.04    ; IIV (CL-V)

$OMEGA  BLOCK(1) 0.0165           ; IOV CL
$OMEGA  BLOCK(1)  SAME         ; IOV CL

$OMEGA  BLOCK(1)  0.495           ; IOV KA`;

describe('ParameterScanner BLOCK Matrix Tests', () => {
  let document: TextDocument;

  beforeEach(() => {
    document = TextDocument.create('test://test.mod', 'nmtran', 1, testContent);
  });

  test('should correctly identify all ETA parameters', () => {
    const locations = ParameterScanner.scanDocument(document);
    
    // Should find 7 ETA parameters
    const etaLocations = locations.filter(loc => loc.type === 'ETA');
    console.log('Found ETA locations:', etaLocations.map(loc => `ETA(${loc.index}) at line ${loc.line}, chars ${loc.startChar}-${loc.endChar}`));
    
    expect(etaLocations).toHaveLength(7);
    
    // Check each ETA parameter
    expect(etaLocations[0]).toEqual(expect.objectContaining({ type: 'ETA', index: 1, line: 0 }));
    expect(etaLocations[1]).toEqual(expect.objectContaining({ type: 'ETA', index: 2, line: 0 }));
    expect(etaLocations[2]).toEqual(expect.objectContaining({ type: 'ETA', index: 3, line: 2 }));
    expect(etaLocations[3]).toEqual(expect.objectContaining({ type: 'ETA', index: 4, line: 2 }));
    expect(etaLocations[4]).toEqual(expect.objectContaining({ type: 'ETA', index: 5, line: 4 }));
    expect(etaLocations[5]).toEqual(expect.objectContaining({ type: 'ETA', index: 6, line: 5 }));
    expect(etaLocations[6]).toEqual(expect.objectContaining({ type: 'ETA', index: 7, line: 7 }));
  });

  test('should find correct positions for numeric values', () => {
    const locations = ParameterScanner.scanDocument(document);
    const etaLocations = locations.filter(loc => loc.type === 'ETA');
    
    // ETA(1) should highlight "0.0444" at line 0
    const eta1 = etaLocations.find(loc => loc.index === 1);
    expect(eta1?.startChar).toBeDefined();
    expect(eta1?.endChar).toBeDefined();
    
    // ETA(3) should highlight "0.1" at line 2  
    const eta3 = etaLocations.find(loc => loc.index === 3);
    expect(eta3?.startChar).toBeDefined();
    expect(eta3?.endChar).toBeDefined();
    
    // ETA(5) should highlight "0.0165" at line 4
    const eta5 = etaLocations.find(loc => loc.index === 5);
    expect(eta5?.startChar).toBeDefined(); 
    expect(eta5?.endChar).toBeDefined();
    
    console.log('ETA(1) position:', eta1?.startChar, '-', eta1?.endChar);
    console.log('ETA(3) position:', eta3?.startChar, '-', eta3?.endChar);
    console.log('ETA(5) position:', eta5?.startChar, '-', eta5?.endChar);
  });
});