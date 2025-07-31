/**
 * Configuration Service
 * 
 * Centralized configuration management for the NMTRAN extension.
 * Handles debug settings, paths, and other configurable options.
 */

import * as vscode from 'vscode';

export interface ExtensionConfig {
  debug: {
    enabled: boolean;
    logLevel: 'error' | 'warn' | 'info' | 'debug';
  };
  server: {
    port: number;
    timeout: number;
  };
  paths: {
    serverModule: string;
  };
}

export class ConfigurationService {
  private static instance: ConfigurationService;
  private config: ExtensionConfig;

  private constructor() {
    this.config = this.loadConfiguration();
  }

  public static getInstance(): ConfigurationService {
    if (!ConfigurationService.instance) {
      ConfigurationService.instance = new ConfigurationService();
    }
    return ConfigurationService.instance;
  }

  private loadConfiguration(): ExtensionConfig {
    const vsConfig = vscode.workspace.getConfiguration('nmtran');
    
    return {
      debug: {
        enabled: process.env.NODE_ENV === 'development' || vsConfig.get('debug.enabled', false),
        logLevel: vsConfig.get('debug.logLevel', 'info')
      },
      server: {
        port: vsConfig.get('server.debugPort', 6009),
        timeout: vsConfig.get('server.timeout', 2000)
      },
      paths: {
        serverModule: 'dist/server.js'
      }
    };
  }

  public get<K extends keyof ExtensionConfig>(key: K): ExtensionConfig[K] {
    return this.config[key];
  }

  public isDebugEnabled(): boolean {
    return this.config.debug.enabled;
  }

  public refresh(): void {
    this.config = this.loadConfiguration();
  }
}