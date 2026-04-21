import { describe, it, expect, beforeAll } from 'vitest';
import * as fs from 'fs';
import * as path from 'path';
import * as oniguruma from 'vscode-oniguruma';
import * as vsctm from 'vscode-textmate';

const GRAMMAR_PATH = path.resolve(__dirname, '../../../syntaxes/nmtran.tmLanguage.json');
const WASM_PATH = path.resolve(__dirname, '../../node_modules/vscode-oniguruma/release/onig.wasm');

let registry: vsctm.Registry;
let grammar: vsctm.IGrammar;

beforeAll(async () => {
  const wasmBin = fs.readFileSync(WASM_PATH).buffer;
  await oniguruma.loadWASM(wasmBin);

  registry = new vsctm.Registry({
    onigLib: Promise.resolve({
      createOnigScanner: (patterns) => new oniguruma.OnigScanner(patterns),
      createOnigString: (s) => new oniguruma.OnigString(s),
    }),
    loadGrammar: async (scopeName) => {
      if (scopeName === 'source.nmtran') {
        const raw = fs.readFileSync(GRAMMAR_PATH, 'utf-8');
        return vsctm.parseRawGrammar(raw, GRAMMAR_PATH);
      }
      return null;
    },
  });

  const g = await registry.loadGrammar('source.nmtran');
  if (!g) throw new Error('Failed to load NMTRAN grammar');
  grammar = g;
});

function scopesForToken(line: string, token: string): string[] {
  const result = grammar.tokenizeLine(line, vsctm.INITIAL);
  const start = line.indexOf(token);
  if (start < 0) return [];
  const hit = result.tokens.find(t => t.startIndex <= start && t.endIndex >= start + token.length);
  return hit ? hit.scopes : [];
}

describe('Grammar reserved words', () => {
  it('highlights F_FLAG as a reserved variable', () => {
    const scopes = scopesForToken('F_FLAG = 1', 'F_FLAG');
    expect(scopes.some(s => s.startsWith('support.variable.reserved'))).toBe(true);
  });

  it('highlights PHI(X) as a function call', () => {
    const scopes = scopesForToken('Y = PHI(X)', 'PHI');
    expect(scopes).toContain('entity.name.function.nmtran');
  });

  it('does not highlight PHI inside a user identifier like MYPHI(X)', () => {
    const scopes = scopesForToken('MYPHI(X)', 'PHI');
    expect(scopes).not.toContain('entity.name.function.nmtran');
  });
});
