# Implementation Plan

**Audit Date:** 2025-11-13
**Focus:** Performance bottlenecks, code health, and technical debt

## Critical Findings Summary

**Performance Impact:**
- 5x document scans per validation (300-800ms on 2000-line files)
- No hover caching (150-400ms per hover)
- Excessive console logging (10-30ms overhead)

**Code Health:**
- Dead code: 73 lines in unused parser
- Pattern duplication across 3+ files (~100 lines)
- 2 dependency version conflicts
- ParameterScanner.ts: 1,404 lines (violates SRP)

**Documentation:**
- Significant duplication across CLAUDE.md, ARCHITECTURE.md, CONTRIBUTING.md, MAINTENANCE.md
- 1 obsolete file removed
- 2 data errors fixed

---

## Phase 1: Performance Optimization (Week 1-2)

**Goal:** 80%+ improvement in user-perceived performance

### 1.1 Hover Caching (Day 1-2)
**File:** `server/src/services/hoverService.ts`
**Impact:** 85-90% faster (400ms → 50ms)

```typescript
private scanCache = new Map<string, ParameterLocation[]>();

provideHover(document: TextDocument, position: Position): Hover | null {
  const cacheKey = `${document.uri}:${document.version}`;
  let parameters = this.scanCache.get(cacheKey);

  if (!parameters) {
    parameters = ParameterScanner.scanDocument(document);
    this.scanCache.set(cacheKey, parameters);

    // LRU eviction (keep 100 most recent)
    if (this.scanCache.size > 100) {
      const oldest = this.scanCache.keys().next().value;
      this.scanCache.delete(oldest);
    }
  }
  // Use cached parameters...
}
```

### 1.2 Single-Pass Validation (Day 3-5)
**File:** `server/src/services/diagnosticsService.ts`
**Impact:** 70-80% faster (600ms → 120ms)

**Current:** 5 separate full-document scans
- `ParameterScanner.scanDocument()` - 1st scan
- `validateBlockMatrixSyntax()` - 2nd scan
- `validateSameKeywordUsage()` - 3rd scan
- `validateContinuationMarkers()` - 4th scan
- `validateParameterBounds()` - 5th scan

**Solution:** Single-pass scan collecting all data, then validate

```typescript
private singlePassScan(lines: string[]): ValidationData {
  const data = { controlRecords: [], parameters: [],
                 blockMatrices: [], continuationMarkers: [], bounds: [] };

  for (let i = 0; i < lines.length; i++) {
    // Collect ALL data in one iteration
    this.scanLine(lines[i], i, data);
  }
  return data;
}
```

### 1.3 Conditional Logging (Day 6)
**Files:** Multiple services
**Impact:** 10-30ms saved per operation

```typescript
private shouldLog = process.env.NODE_ENV === 'development';

if (this.shouldLog) {
  this.connection.console.log(`Validating...`);
}
```

**Expected Results:**
- Typing lag: 800ms → 120-200ms (75-85% improvement)
- Hover response: 400ms → 50ms (87% improvement)
- Users with large files (1000-3000 lines) experience "snappy" extension

---

## Phase 2: Code Health & Maintainability (Week 3-4)

**Goal:** Reduce codebase by ~1,200 lines, eliminate duplication, improve architecture

### 2.1 Dead Code Removal (Day 1)
**Files to delete:**
- `server/src/parsers/parameterParser.ts` (73 lines, completely unused)
- Empty `server/src/parsers/` directory
- Update ARCHITECTURE.md to remove parsers/ reference

### 2.2 Consolidate Pattern Constants (Day 2-3)
**Impact:** Single source of truth, ~100 lines saved

**Current duplication:**
- `PARAMETER_PATTERNS` in ParameterScanner.ts (24 constants)
- Duplicate in definitionService.ts (subset)
- Duplicate in utils/parameterParser.ts

**Solution:**
1. Create `server/src/constants/patterns.ts`:
   ```typescript
   export const PARAMETER_PATTERNS = {
     THETA: /^\$THETA(\s|$)/i,
     OMEGA: /^\$OMEGA(\s|$)/i,
     // ... all 24 patterns
   } as const;
   ```
2. Import in all services: `import { PARAMETER_PATTERNS } from '../constants/patterns';`
3. Remove duplicates

### 2.3 Fix Dependency Conflicts (Day 4)
**Files:** `package.json`, `server/package.json`, `client/package.json`

**Critical fixes:**
```bash
# Align TypeScript version
npm install -D typescript@^5.8.3
cd server && npm install -D typescript@^5.8.3

# Sync languageserver-textdocument
cd server && npm install vscode-languageserver-textdocument@^1.0.12

# Remove unused Mocha
cd ../client && npm uninstall @types/mocha mocha
```

**Sync tsconfig strict options:**
```json
// Share base config across client/server
{
  "extends": "../../tsconfig.base.json",
  "compilerOptions": { /* overrides */ }
}
```

### 2.4 Refactor ParameterScanner (Day 5-8)
**File:** `server/src/services/ParameterScanner.ts` (1,404 → ~400 lines)

**Split into focused modules:**
1. `ParameterScanner.ts` (300 lines) - Core orchestration
2. `BlockMatrixHandler.ts` (200 lines) - BLOCK matrix logic
3. `ParameterLineProcessor.ts` (200 lines) - Line parsing
4. `ScannerStateManager.ts` (150 lines) - State management

**Benefits:**
- Easier testing (unit test each module)
- Better separation of concerns
- Reduced cognitive load
- Follows Single Responsibility Principle

### 2.5 Consolidate Documentation (Day 9-10)
**Goal:** Reduce duplication, single source of truth

**Actions:**
1. Merge MAINTENANCE.md → CONTRIBUTING.md
   - Combine development workflows
   - Keep release process in CONTRIBUTING
   - Single source for contributors

2. Streamline CLAUDE.md
   - Remove duplicated architecture details
   - Reference ARCHITECTURE.md instead
   - Keep NMTRAN-specific constraints

3. Clean ARCHITECTURE.md
   - Remove procedural content
   - Focus on design principles
   - Point to CONTRIBUTING for procedures

**Result:** 1,366 lines → ~1,000 lines (25% reduction)

**Expected Results:**
- Codebase reduced by ~1,200 lines
- Zero pattern duplication
- Zero dependency conflicts
- All files under 500 lines
- Clearer documentation structure

---

## Success Metrics

### Performance Targets
- [ ] Typing lag < 200ms (2000-line files)
- [ ] Hover response < 100ms
- [ ] Go-to-definition < 50ms
- [ ] Memory stable with caching (< 100MB heap growth)

### Code Health Targets
- [ ] Zero dead code
- [ ] Zero pattern duplication
- [ ] All files < 500 lines
- [ ] All tests passing (279+)
- [ ] Test coverage ≥ 80%

### Documentation Targets
- [ ] Zero duplicate sections
- [ ] Clear cross-references
- [ ] Single source of truth per topic

---

## Future Phases (Not in Current Plan)

### Phase 3: Advanced Performance (Future)
- Incremental validation (only changed lines)
- Optimize ParameterScanner.parseParameterExpressions (state machine)
- Background validation thread

### Phase 4: Architecture Modernization (Future)
- npm workspaces migration
- ESM support (when VSCode mature)
- Bundle size tracking and limits
- Dependency injection container

### Phase 5: Extended Testing (Future)
- Client unit tests
- E2E tests with @vscode/test-electron
- Performance benchmarks
- Large file stress tests

---

## Risk Assessment

### Low Risk (Phase 1)
- Caching: Isolated changes, easy rollback
- Logging: Feature flag controlled
- Impact: High user value, low code complexity

### Medium Risk (Phase 2)
- ParameterScanner refactor: Large but well-tested
- Pattern consolidation: Simple refactor with test coverage
- Documentation: No code changes
- Mitigation: Keep test coverage ≥ 80%, run full test suite

---

## Dependencies

**Phase 1 → Phase 2:** Independent (can run in parallel)
**Testing:** All changes require passing 279+ tests
**Documentation:** Update CHANGELOG.md after each phase

---

## Timeline Summary

| Week | Phase | Focus | Completion |
|------|-------|-------|------------|
| 1-2  | Phase 1 | Performance optimization | 6-10 days |
| 3-4  | Phase 2 | Code health & docs | 8-10 days |

**Total:** 3-4 weeks for both critical phases
