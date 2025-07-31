#!/bin/bash

# Simple Pre-Release Check
# Just test the things that commonly fail in GitHub Actions

set -e

# Cleanup test files on exit
cleanup() {
    rm -f test-release.vsix
}
trap cleanup EXIT

echo "🔍 Quick Release Check..."

# Show current version
CURRENT_VERSION=$(node -p "require('./package.json').version")
echo "Version: v$CURRENT_VERSION"

# Check VSCode version consistency (most common failure)
VSCODE_ENGINES=$(node -p "require('./package.json').engines.vscode")
VSCODE_TYPES=$(node -p "require('./package.json').devDependencies['@types/vscode']")

echo "engines.vscode: $VSCODE_ENGINES"
echo "@types/vscode: $VSCODE_TYPES"

ENGINES_VERSION=$(echo $VSCODE_ENGINES | sed 's/[^0-9.]*//g' | cut -d. -f1,2)
TYPES_VERSION=$(echo $VSCODE_TYPES | sed 's/[^0-9.]*//g' | cut -d. -f1,2)

if [[ "$ENGINES_VERSION" != "$TYPES_VERSION" ]]; then
    echo "❌ Version mismatch! This will fail in GitHub Actions"
    echo "Fix: Update engines.vscode to match @types/vscode"
    exit 1
fi

# Quick build test
echo "🔨 Testing build..."
npm run lint && npm run compile && npm run vscode:prepublish

# Test VSIX packaging 
echo "📦 Testing package..."
npx vsce package --out test-release.vsix > /dev/null

echo "✅ Release looks good! GitHub Actions should pass."
echo "Next: git tag v$CURRENT_VERSION && git push origin v$CURRENT_VERSION"