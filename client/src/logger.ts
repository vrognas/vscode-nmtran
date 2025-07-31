/**
 * Logger Service
 * 
 * Centralized logging service with configurable levels and consistent formatting.
 * Provides structured logging for the NMTRAN extension.
 */

import { ConfigurationService } from './config';

type LogLevel = 'error' | 'warn' | 'info' | 'debug';

export class Logger {
  private static instance: Logger;
  private config: ConfigurationService;

  private constructor() {
    this.config = ConfigurationService.getInstance();
  }

  public static getInstance(): Logger {
    if (!Logger.instance) {
      Logger.instance = new Logger();
    }
    return Logger.instance;
  }

  private shouldLog(level: LogLevel): boolean {
    if (!this.config.isDebugEnabled()) {
      return level === 'error';
    }

    const configLevel = this.config.get('debug').logLevel;
    const levels: LogLevel[] = ['error', 'warn', 'info', 'debug'];
    const configIndex = levels.indexOf(configLevel);
    const messageIndex = levels.indexOf(level);
    
    return messageIndex <= configIndex;
  }

  private formatMessage(level: LogLevel, message: string, ...args: unknown[]): string {
    const timestamp = new Date().toISOString();
    const emoji = this.getLogEmoji(level);
    const prefix = `[${timestamp.slice(11, 19)}] ${emoji} NMTRAN`;
    
    if (args.length > 0) {
      return `${prefix}: ${message} ${args.map(arg => 
        typeof arg === 'object' ? JSON.stringify(arg) : String(arg)
      ).join(' ')}`;
    }
    
    return `${prefix}: ${message}`;
  }

  private getLogEmoji(level: LogLevel): string {
    switch (level) {
      case 'error': return '❌';
      case 'warn': return '⚠️';
      case 'info': return 'ℹ️';
      case 'debug': return '🔍';
      default: return '📝';
    }
  }

  public error(message: string, ...args: unknown[]): void {
    if (this.shouldLog('error')) {
      console.error(this.formatMessage('error', message, ...args));
    }
  }

  public warn(message: string, ...args: unknown[]): void {
    if (this.shouldLog('warn')) {
      console.warn(this.formatMessage('warn', message, ...args));
    }
  }

  public info(message: string, ...args: unknown[]): void {
    if (this.shouldLog('info')) {
      console.log(this.formatMessage('info', message, ...args));
    }
  }

  public debug(message: string, ...args: unknown[]): void {
    if (this.shouldLog('debug')) {
      console.log(this.formatMessage('debug', message, ...args));
    }
  }

  // Convenience methods for common use cases
  public activation(message: string): void {
    this.info(`🚀 ${message}`);
  }

  public server(message: string, ...args: unknown[]): void {
    this.info(`🗂️ ${message}`, ...args);
  }

  public completion(message: string): void {
    this.info(`✨ ${message}`);
  }
}