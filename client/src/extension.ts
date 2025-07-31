/**
 * NMTRAN VSCode Extension - Client Side
 * 
 * This is the "entry point" of the extension that VSCode directly talks to.
 * Main responsibilities:
 * 1. Register language features (like code folding)
 * 2. Start and manage the language server connection
 * 3. Handle extension lifecycle (activate/deactivate)
 */

import * as vscode from 'vscode';
import * as path from 'path';
import { ConfigurationService } from './config';
import { Logger } from './logger';
import { NMTRANFoldingProvider } from './features/foldingProvider';
import { LanguageServerManager } from './features/languageServer';

// Service instances
let languageServerManager: LanguageServerManager;

/**
 * Called when the extension is activated (when NMTRAN files are opened)
 */
export async function activate(context: vscode.ExtensionContext): Promise<void> {
  const logger = Logger.getInstance();
  
  try {
    logger.activation('Starting activation...');
    logger.debug('Extension path:', context.extensionPath);
    logger.debug('Extension version:', getExtensionVersion(context));
    
    // Register language features
    await registerLanguageFeatures(context);
    
    // Start language server
    await startLanguageServer(context);
    
    // Setup configuration change handlers
    setupConfigurationHandlers();
    
    logger.completion('Activation completed successfully');
    
  } catch (error) {
    logger.error('Extension activation failed:', error);
    throw error;
  }
}

/**
 * Register language features like folding
 */
async function registerLanguageFeatures(context: vscode.ExtensionContext): Promise<void> {
  const logger = Logger.getInstance();
  
  logger.info('Registering language features...');
  
  // Register folding provider
  const foldingProvider = vscode.languages.registerFoldingRangeProvider(
    { language: 'nmtran', scheme: 'file' },
    new NMTRANFoldingProvider()
  );
  
  context.subscriptions.push(foldingProvider);
  logger.debug('Folding provider registered');
}

/**
 * Start the language server
 */
async function startLanguageServer(context: vscode.ExtensionContext): Promise<void> {
  languageServerManager = new LanguageServerManager();
  await languageServerManager.start(context);
}

/**
 * Setup configuration change handlers
 */
function setupConfigurationHandlers(): void {
  const config = ConfigurationService.getInstance();
  
  vscode.workspace.onDidChangeConfiguration((event) => {
    if (event.affectsConfiguration('nmtran')) {
      config.refresh();
    }
  });
}

/**
 * Get extension version safely
 */
function getExtensionVersion(context: vscode.ExtensionContext): string {
  try {
    const packageJsonPath = path.join(context.extensionPath, 'package.json');
    // eslint-disable-next-line @typescript-eslint/no-require-imports
    const packageJson = require(packageJsonPath);
    return packageJson.version || 'unknown';
  } catch (_error) {
    return 'unknown';
  }
}

/**
 * Called when the extension is deactivated (VSCode closes or extension is disabled)
 * Properly shuts down the language server to free resources
 */
export async function deactivate(): Promise<void> {
  const logger = Logger.getInstance();
  
  try {
    logger.info('Deactivating extension...');
    
    if (languageServerManager?.isRunning()) {
      await languageServerManager.stop();
    }
    
    logger.info('Extension deactivated successfully');
  } catch (error) {
    logger.error('Error during deactivation:', error);
  }
}
