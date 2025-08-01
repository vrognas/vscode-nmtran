/**
 * Performance monitoring utilities (without telemetry)
 * All metrics are kept local for development/debugging purposes only
 */

import { Connection } from 'vscode-languageserver';

export interface PerformanceMetric {
  operation: string;
  duration: number;
  timestamp: Date;
  metadata?: Record<string, unknown>;
}

export class PerformanceMonitor {
  private metrics: PerformanceMetric[] = [];
  private readonly maxMetrics = 1000;
  private enabled = process.env.NODE_ENV === 'development';
  
  constructor(private connection: Connection) {}

  /**
   * Measure the performance of an operation
   */
  async measure<T>(
    operation: string,
    fn: () => T | Promise<T>,
    metadata?: Record<string, unknown>
  ): Promise<T> {
    if (!this.enabled) {
      return fn();
    }

    const startTime = performance.now();
    
    try {
      const result = await fn();
      const duration = performance.now() - startTime;
      
      const metric: PerformanceMetric = {
        operation,
        duration,
        timestamp: new Date()
      };
      
      if (metadata) {
        metric.metadata = metadata;
      }
      
      this.recordMetric(metric);
      
      return result;
    } catch (error) {
      const duration = performance.now() - startTime;
      
      this.recordMetric({
        operation,
        duration,
        timestamp: new Date(),
        metadata: { ...metadata, error: true }
      });
      
      throw error;
    }
  }

  /**
   * Record a performance metric locally
   */
  private recordMetric(metric: PerformanceMetric): void {
    this.metrics.push(metric);
    
    // Keep only recent metrics
    if (this.metrics.length > this.maxMetrics) {
      this.metrics.shift();
    }
    
    // Log slow operations in development
    if (metric.duration > 100) {
      this.connection.console.warn(
        `⚠️ Slow operation: ${metric.operation} took ${metric.duration.toFixed(2)}ms`
      );
    }
  }

  /**
   * Get performance statistics (local only)
   */
  getStats(operation?: string): {
    count: number;
    avgDuration: number;
    minDuration: number;
    maxDuration: number;
    p95Duration: number;
  } {
    const relevantMetrics = operation
      ? this.metrics.filter(m => m.operation === operation)
      : this.metrics;
    
    if (relevantMetrics.length === 0) {
      return {
        count: 0,
        avgDuration: 0,
        minDuration: 0,
        maxDuration: 0,
        p95Duration: 0
      };
    }
    
    const durations = relevantMetrics.map(m => m.duration).sort((a, b) => a - b);
    const sum = durations.reduce((a, b) => a + b, 0);
    const p95Index = Math.floor(durations.length * 0.95);
    
    return {
      count: durations.length,
      avgDuration: sum / durations.length,
      minDuration: durations[0]!,
      maxDuration: durations[durations.length - 1]!,
      p95Duration: durations[p95Index]!
    };
  }

  /**
   * Log current statistics to console (development only)
   */
  logStats(): void {
    if (!this.enabled) return;

    const operations = new Set(this.metrics.map(m => m.operation));
    
    this.connection.console.log('📊 Performance Statistics:');
    
    for (const op of operations) {
      const stats = this.getStats(op);
      this.connection.console.log(
        `  ${op}: ${stats.count} calls, avg ${stats.avgDuration.toFixed(2)}ms, p95 ${stats.p95Duration.toFixed(2)}ms`
      );
    }
  }

  /**
   * Clear all metrics
   */
  clear(): void {
    this.metrics = [];
  }

  /**
   * Enable/disable performance monitoring
   */
  setEnabled(enabled: boolean): void {
    this.enabled = enabled;
    if (!enabled) {
      this.clear();
    }
  }
}