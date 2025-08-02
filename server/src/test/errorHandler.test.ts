import { ErrorHandler, ErrorContext } from '../utils/errorHandler';

describe('ErrorHandler', () => {
  let errorHandler: ErrorHandler;
  beforeEach(() => {
    errorHandler = new ErrorHandler();
    jest.spyOn(console, 'warn').mockImplementation();
    jest.spyOn(console, 'error').mockImplementation();
    jest.spyOn(console, 'log').mockImplementation();
  });

  afterEach(() => {
    jest.restoreAllMocks();
  });

  describe('logWarning', () => {
    it('should log warning without context', () => {
      errorHandler.logWarning('Test warning');
      
      expect(console.warn).toHaveBeenCalledWith('[NMTRAN Warning] Test warning');
    });

    it('should log warning with context', () => {
      const context: ErrorContext = {
        operation: 'testOp',
        lineNumber: 5,
        fileName: 'test.mod',
        parameterType: 'THETA'
      };

      errorHandler.logWarning('Test warning', context);
      
      expect(console.warn).toHaveBeenCalledWith(
        '[NMTRAN Warning] Test warning [operation=testOp, file=test.mod, line=6, type=THETA]'
      );
    });

    it('should format additional info in context', () => {
      const context: ErrorContext = {
        operation: 'testOp',
        additionalInfo: { blockSize: 2, matrixType: 'BLOCK' }
      };

      errorHandler.logWarning('Test warning', context);
      
      expect(console.warn).toHaveBeenCalledWith(
        '[NMTRAN Warning] Test warning [operation=testOp, blockSize=2, matrixType="BLOCK"]'
      );
    });
  });

  describe('logError', () => {
    it('should log error without context', () => {
      errorHandler.logError('Test error');
      
      expect(console.error).toHaveBeenCalledWith('[NMTRAN Error] Test error');
    });

    it('should log error with context', () => {
      const context: ErrorContext = {
        operation: 'testOp',
        lineNumber: 10
      };

      errorHandler.logError('Test error', context);
      
      expect(console.error).toHaveBeenCalledWith(
        '[NMTRAN Error] Test error [operation=testOp, line=11]'
      );
    });
  });

  describe('logDebug', () => {
    it('should log debug without context', () => {
      errorHandler.logDebug('Test debug');
      
      expect(console.log).toHaveBeenCalledWith('[NMTRAN Debug] Test debug');
    });
  });

  describe('handleException', () => {
    it('should handle Error objects', () => {
      const error = new Error('Test error message');
      const context: ErrorContext = { operation: 'testOp' };

      errorHandler.handleException(error, context);
      
      expect(console.error).toHaveBeenCalledWith(
        '[NMTRAN Exception] Test error message [operation=testOp]'
      );
    });

    it('should include stack trace', () => {
      const error = new Error('Test error');
      error.stack = 'Error: Test error\\n    at test';

      errorHandler.handleException(error);
      
      expect(console.error).toHaveBeenCalledTimes(2);
      expect(console.error).toHaveBeenCalledWith('[NMTRAN Exception] Test error');
      expect(console.error).toHaveBeenCalledWith('Error: Test error\\n    at test');
    });
  });

  describe('createSafeResult', () => {
    it('should return result when operation succeeds', () => {
      const operation = () => 'success';
      const fallback = 'fallback';

      const result = errorHandler.createSafeResult(operation, fallback);
      
      expect(result).toBe('success');
    });

    it('should return fallback when operation throws', () => {
      const operation = () => { throw new Error('Test error'); };
      const fallback = 'fallback';

      const result = errorHandler.createSafeResult(operation, fallback);
      
      expect(result).toBe('fallback');
      expect(console.error).toHaveBeenCalled();
    });

    it('should include context when operation throws', () => {
      const operation = () => { throw new Error('Test error'); };
      const fallback = 'fallback';
      const context: ErrorContext = { operation: 'testOp' };

      errorHandler.createSafeResult(operation, fallback, context);
      
      expect(console.error).toHaveBeenCalledWith(
        '[NMTRAN Exception] Test error [operation=testOp]'
      );
    });
  });

  describe('createSafeAsyncResult', () => {
    it('should return result when async operation succeeds', async () => {
      const operation = async () => 'async success';
      const fallback = 'fallback';

      const result = await errorHandler.createSafeAsyncResult(operation, fallback);
      
      expect(result).toBe('async success');
    });

    it('should return fallback when async operation throws', async () => {
      const operation = async () => { throw new Error('Async error'); };
      const fallback = 'fallback';

      const result = await errorHandler.createSafeAsyncResult(operation, fallback);
      
      expect(result).toBe('fallback');
      expect(console.error).toHaveBeenCalled();
    });
  });

  describe('static methods', () => {
    it('should work without instance', () => {
      ErrorHandler.logWarning('Static warning');
      
      expect(console.warn).toHaveBeenCalledWith('[NMTRAN Warning] Static warning');
    });

    it('should handle exceptions statically', () => {
      const error = new Error('Static error');
      const context: ErrorContext = { operation: 'staticOp' };

      ErrorHandler.handleException(error, context);
      
      expect(console.error).toHaveBeenCalledWith(
        '[NMTRAN Exception] Static error [operation=staticOp]'
      );
    });
  });

  describe('formatContext', () => {
    it('should handle empty context', () => {
      const context: ErrorContext = { operation: '' };
      
      errorHandler.logWarning('Test', context);
      
      expect(console.warn).toHaveBeenCalledWith('[NMTRAN Warning] Test');
    });

    it('should convert line numbers to 1-based', () => {
      const context: ErrorContext = { operation: 'test', lineNumber: 0 };
      
      errorHandler.logWarning('Test', context);
      
      expect(console.warn).toHaveBeenCalledWith('[NMTRAN Warning] Test [operation=test, line=1]');
    });
  });
});