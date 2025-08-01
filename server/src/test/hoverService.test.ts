/**
 * Test hover service functionality
 */

import { HoverService } from '../services/hoverService';
import { TextDocument } from 'vscode-languageserver-textdocument';
import { Connection } from 'vscode-languageserver/node';

// Mock connection
const mockConnection = {
  console: {
    error: jest.fn(),
    log: jest.fn(),
    info: jest.fn(),
    warn: jest.fn()
  }
} as unknown as Connection;

describe('HoverService', () => {
  let hoverService: HoverService;

  beforeEach(() => {
    hoverService = new HoverService(mockConnection);
  });

  test('should provide hover for parameter reference with FIXED keyword', () => {
    const content = `$THETA  (0,3) 2 FIXED (0,.6,1) 10 (-INF,-2.7,0) (37 FIXED) 4 FIX
$PK
CL = THETA(1) * EXP(ETA(1))
V = THETA(2) * EXP(ETA(2))`;

    const document = TextDocument.create('test.mod', 'nmtran', 1, content);
    
    // Test hovering over THETA(2) which should show "2 FIXED"
    const position = { line: 3, character: 5 }; // Position of THETA(2) on line 3
    const result = hoverService.provideHover(document, position);
    
    expect(result).toBeTruthy();
    expect(result?.contents).toHaveProperty('kind', 'markdown');
    expect((result?.contents as any).value).toContain('THETA(2)');
    expect((result?.contents as any).value).toContain('2 FIXED');
  });

  test('should provide hover for parameter reference without FIXED keyword', () => {
    const content = `$THETA  (0,3) 2 FIXED (0,.6,1) 10 (-INF,-2.7,0) (37 FIXED) 4 FIX
$PK
CL = THETA(1) * EXP(ETA(1))
V = THETA(3) * EXP(ETA(2))`;

    const document = TextDocument.create('test.mod', 'nmtran', 1, content);
    
    // Test hovering over THETA(3) which should show "(0,.6,1)"
    const position = { line: 3, character: 5 }; // Position of THETA(3)
    const result = hoverService.provideHover(document, position);
    
    expect(result).toBeTruthy();
    expect(result?.contents).toHaveProperty('kind', 'markdown');
    expect((result?.contents as any).value).toContain('THETA(3)');
    expect((result?.contents as any).value).toContain('(0,.6,1)');
  });

  test('should provide hover for ETA parameter reference', () => {
    const content = `$OMEGA BLOCK(2) FIXED
0.0444
0.027 0.0241
$PK
CL = THETA(1) * EXP(ETA(1))
V = THETA(2) * EXP(ETA(2))`;

    const document = TextDocument.create('test.mod', 'nmtran', 1, content);
    
    // Test hovering over ETA(1) which should show "0.0444 FIXED"
    const position = { line: 4, character: 23 }; // Position of ETA(1) - inside the parentheses
    const result = hoverService.provideHover(document, position);
    
    expect(result).toBeTruthy();
    expect(result?.contents).toHaveProperty('kind', 'markdown');
    expect((result?.contents as any).value).toContain('ETA(1)');
    expect((result?.contents as any).value).toContain('0.0444');
    expect((result?.contents as any).value).toContain('FIXED');
  });

  test('should provide hover for ERR parameter reference', () => {
    const content = `$SIGMA 0.1
$ERROR
Y = F + F*ERR(1)`;

    const document = TextDocument.create('test.mod', 'nmtran', 1, content);
    
    // Test hovering over ERR(1) which should show "0.1"
    const position = { line: 2, character: 12 }; // Position of ERR(1)
    const result = hoverService.provideHover(document, position);
    
    expect(result).toBeTruthy();
    expect(result?.contents).toHaveProperty('kind', 'markdown');
    expect((result?.contents as any).value).toContain('ERR(1)');
    expect((result?.contents as any).value).toContain('0.1');
  });

  test('should still provide hover for control records', () => {
    const content = `$THETA  (0,3) 2 FIXED
$PK
CL = THETA(1)`;

    const document = TextDocument.create('test.mod', 'nmtran', 1, content);
    
    // Test hovering over $PK control record
    const position = { line: 1, character: 2 }; // Position of $PK
    const result = hoverService.provideHover(document, position);
    
    expect(result).toBeTruthy();
    expect(result?.contents).toHaveProperty('kind', 'markdown');
    expect((result?.contents as any).value).toContain('PK');
  });

  test('should return null for positions with no hover information', () => {
    const content = `$THETA  (0,3) 2 FIXED
$PK
CL = THETA(1)`;

    const document = TextDocument.create('test.mod', 'nmtran', 1, content);
    
    // Test hovering over a space/empty area
    const position = { line: 2, character: 1 }; // Position in empty space
    const result = hoverService.provideHover(document, position);
    
    expect(result).toBeNull();
  });
});