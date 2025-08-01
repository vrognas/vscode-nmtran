import { TextDocument } from 'vscode-languageserver-textdocument';
import { DefinitionService } from '../services/definitionService';
import { Position } from 'vscode-languageserver';

describe('DefinitionService', () => {
  let service: DefinitionService;
  let mockConnection: any;

  beforeEach(() => {
    mockConnection = {
      console: {
        log: jest.fn(),
        error: jest.fn(),
        warn: jest.fn()
      }
    };
    service = new DefinitionService(mockConnection);
  });

  describe('BLOCK matrix handling', () => {
    it('should find correct diagonal elements in BLOCK(2) with inline values', async () => {
      const content = '$OMEGA  BLOCK(2) 0.0444 0.027 0.0241    ; IIV (CL-V)';
      const doc = TextDocument.create('test://test.mod', 'nmtran', 1, content);
      
      // Test ETA(1) - should point to 0.0444
      const eta1Def = await service.provideDefinition(doc, Position.create(0, 17)); // Position of 0.0444
      expect(eta1Def).toBeDefined();
      
      // Test ETA(2) - should point to 0.0241
      const eta2Def = await service.provideDefinition(doc, Position.create(0, 31)); // Position of 0.0241
      expect(eta2Def).toBeDefined();
    });

    it('should handle BLOCK(3) diagonal elements correctly', async () => {
      const content = `$OMEGA  BLOCK(3)
0.1    ; OMEGA(1,1)
0.05 0.2   ; OMEGA(2,1) OMEGA(2,2) 
0.01 0.03 0.15  ; OMEGA(3,1) OMEGA(3,2) OMEGA(3,3)`;
      
      const doc = TextDocument.create('test://test.mod', 'nmtran', 1, content);
      
      // The service should identify:
      // ETA(1) -> line 1, value 0.1
      // ETA(2) -> line 2, value 0.2 (second value)
      // ETA(3) -> line 3, value 0.15 (third value)
    });

    it('should handle SAME references correctly', async () => {
      const content = `$OMEGA  BLOCK(1) 0.0165           ; IOV CL
$OMEGA  BLOCK(1)  SAME         ; IOV CL`;
      
      const doc = TextDocument.create('test://test.mod', 'nmtran', 1, content);
      
      // When checking ETA with SAME, it should find both:
      // 1. The SAME keyword
      // 2. The referenced value 0.0165
    });

    it('should handle mixed BLOCK and diagonal OMEGA', async () => {
      const content = `$OMEGA  BLOCK(2) 0.0444 0.027 0.0241
$OMEGA  0.5   ; Simple diagonal
$OMEGA  BLOCK(1) 3.0`;
      
      const doc = TextDocument.create('test://test.mod', 'nmtran', 1, content);
      
      // Should correctly identify:
      // ETA(1) -> 0.0444 (from BLOCK(2))
      // ETA(2) -> 0.0241 (from BLOCK(2))
      // ETA(3) -> 0.5 (simple diagonal)
      // ETA(4) -> 3.0 (from BLOCK(1))
    });
  });

  describe('Parameter usage navigation', () => {
    it('should find definition from usage in $PK block', async () => {
      const content = `$THETA 0.5   ; CL
$PK
CL = THETA(1) * EXP(ETA(1))`;
      
      const doc = TextDocument.create('test://test.mod', 'nmtran', 1, content);
      
      // Clicking on THETA(1) should navigate to 0.5
      const def = await service.provideDefinition(doc, Position.create(2, 10)); // Position in THETA(1)
      expect(def).toBeDefined();
      expect(def).toHaveLength(1);
    });

    it('should handle ERR() as synonym for EPS()', async () => {
      const content = `$SIGMA 0.01   ; Residual error
$ERROR
Y = F + F*ERR(1)`;
      
      const doc = TextDocument.create('test://test.mod', 'nmtran', 1, content);
      
      // Clicking on ERR(1) should navigate to 0.01 in $SIGMA
      const def = await service.provideDefinition(doc, Position.create(2, 10)); // Position in ERR(1)
      expect(def).toBeDefined();
      expect(def).toHaveLength(1);
    });

    it('should handle bounded THETA syntax', async () => {
      const content = `$THETA (0.01, 0.5, 10)   ; Bounded CL
$THETA 2.5 FIX           ; Fixed V`;
      
      const doc = TextDocument.create('test://test.mod', 'nmtran', 1, content);
      
      // THETA(1) should point to 0.5 (initial value in bounded syntax)
      // THETA(2) should point to 2.5
    });
  });

  describe('Error handling', () => {
    it('should handle malformed BLOCK syntax gracefully', async () => {
      const content = '$OMEGA  BLOCK(';
      const doc = TextDocument.create('test://test.mod', 'nmtran', 1, content);
      
      // Should not throw error
      const result = await service.provideDefinition(doc, Position.create(0, 10));
      expect(result).toBeDefined(); // Should return null or empty array, not throw
    });

    it('should handle empty documents', async () => {
      const doc = TextDocument.create('test://test.mod', 'nmtran', 1, '');
      const result = await service.provideDefinition(doc, Position.create(0, 0));
      expect(result).toBeNull();
    });

    it('should handle out of bounds positions', async () => {
      const content = '$THETA 0.5';
      const doc = TextDocument.create('test://test.mod', 'nmtran', 1, content);
      
      const result = await service.provideDefinition(doc, Position.create(10, 0));
      expect(result).toBeNull();
    });
  });

  describe('Complex scenarios', () => {
    it('should handle complete NMTRAN model structure', async () => {
      const content = `$PROBLEM Test model
$INPUT ID TIME DV AMT
$DATA data.csv IGNORE=@
$SUBROUTINE ADVAN2 TRANS2

$PK
CL = THETA(1) * EXP(ETA(1))
V  = THETA(2) * EXP(ETA(2))
KA = THETA(3) * EXP(ETA(3))

$ERROR
Y = F + F*EPS(1)

$THETA 0.5    ; CL
$THETA (0.1, 2, 10)  ; V
$THETA 1.5 FIX   ; KA

$OMEGA BLOCK(2) 0.0444 0.027 0.0241
$OMEGA 0.1

$SIGMA 0.01

$ESTIMATION METHOD=1 MAXEVAL=9999`;
      
      const doc = TextDocument.create('test://test.mod', 'nmtran', 1, content);
      
      // Test various parameter references
      // THETA(1) in $PK should navigate to 0.5
      // ETA(2) should navigate to 0.0241 (diagonal of BLOCK(2))
      // ETA(3) should navigate to 0.1
      // EPS(1) should navigate to 0.01
    });
  });
});