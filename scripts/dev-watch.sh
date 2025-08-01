#!/bin/bash

# Development watch script that watches both TypeScript and bundles
# This ensures changes are both type-checked and bundled for testing

echo "🚀 Starting NMTRAN extension development watchers..."
echo ""
echo "This will watch for:"
echo "  - TypeScript changes (for type checking)"
echo "  - Bundle changes (for running the extension)"
echo ""
echo "Press Ctrl+C to stop all watchers"
echo ""

# Start TypeScript watcher in background
npm run compile:watch &
TSC_PID=$!

# Start bundle watcher in background
npm run bundle:watch &
BUNDLE_PID=$!

# Function to cleanup on exit
cleanup() {
    echo ""
    echo "Stopping watchers..."
    kill $TSC_PID 2>/dev/null
    kill $BUNDLE_PID 2>/dev/null
    exit
}

# Set up trap to cleanup on Ctrl+C
trap cleanup INT

# Wait for both processes
wait