/**
 * Shared mock types and factories for tests
 */

import { vi } from 'vitest';
import { Connection } from 'vscode-languageserver/node';

/**
 * Mock console interface matching RemoteConsole
 */
export interface MockConsole {
  error: ReturnType<typeof vi.fn>;
  warn: ReturnType<typeof vi.fn>;
  info: ReturnType<typeof vi.fn>;
  log: ReturnType<typeof vi.fn>;
}

/**
 * Partial Connection type for testing - only includes console
 */
export interface MockConnection {
  console: MockConsole;
}

/**
 * Creates a fresh mock connection for tests
 */
export function createMockConnection(): MockConnection {
  return {
    console: {
      error: vi.fn(),
      warn: vi.fn(),
      info: vi.fn(),
      log: vi.fn()
    }
  };
}

/**
 * Casts mock connection to Connection type for service constructors
 */
export function asMockConnection(mock: MockConnection): Connection {
  return mock as unknown as Connection;
}
