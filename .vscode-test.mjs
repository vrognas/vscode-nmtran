import { defineConfig } from '@vscode/test-cli';

export default defineConfig({
  files: 'client/out/test/**/*.test.js',
  workspaceFolder: './test',
  mocha: {
    ui: 'tdd',
    timeout: 20000
  }
});