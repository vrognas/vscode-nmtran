/**
 * Simple Test Runner for NMTRAN Extension
 * 
 * This script compiles and runs our TypeScript tests.
 * Usage: node run-tests.js
 */

const { spawn } = require('child_process');
const path = require('path');

console.log('🧪 Running NMTRAN Extension Tests...\n');

// Step 1: Compile TypeScript
console.log('📦 Compiling TypeScript...');
const tscProcess = spawn('npx', ['tsc'], {
  cwd: __dirname,
  stdio: 'inherit'
});

tscProcess.on('close', (code) => {
  if (code !== 0) {
    console.log('❌ TypeScript compilation failed');
    process.exit(1);
  }
  
  console.log('✅ TypeScript compilation successful\n');
  
  // Step 2: Run the compiled test
  console.log('🏃 Running tests...\n');
  const testProcess = spawn('node', ['out/test/validateControlRecords.test.js'], {
    cwd: __dirname,
    stdio: 'inherit'
  });
  
  testProcess.on('close', (testCode) => {
    if (testCode !== 0) {
      console.log('\n❌ Tests failed');
      process.exit(1);
    } else {
      console.log('✅ All tests passed!');
    }
  });
});