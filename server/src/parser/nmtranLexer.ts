import * as moo from "moo";
import * as fs from "fs/promises";
import {
  validControlRecords,
  validSizesOptions
} from "../constants";

const mainRules: moo.Rules = {
  WS: /[ \t]+/,
  comment: /;.*?$/,
  math_op: ['*', '/', '+', '-', '**'],
  logical_op: ['.NOT.', '.AND.', '.OR.', '.EQ.', '.NE.', '.EQN.', '.NEN.', '.LT.', '.LE.', '.GT.', '.GE.', '==', '/=', '<', '<=', '>', '>='],
  assign: '=',
  digit:  { match: /[0-9]+/, value: str => Number(str) as any },
  identifier: {
    match: /[a-zA-Z][a-zA-Z0-9_]*/,
    type: moo.keywords({
      'keyword-if': ['IF', 'THEN', 'ELSE', 'ELSEIF', 'ENDIF'],
      'keyword-while': ['DO WHILE', 'ENDDO'],
      'keyword-other': ['CALL', 'WRITE', 'PRINT', 'RETURN', 'OPEN', 'CLOSE', 'REWIND', 'EXIT'],
      'fortran_builtin': ['LOG10', 'LOG', 'EXP', 'SQRT', 'SIN', 'COS', 'TAN', 'ASIN', 'ACOS', 'ATAN', 'ABS', 'INT', 'MIN', 'MAX', 'MOD'],
      'nonmem_protected': ['PLOG', 'PEXP'],
      'nonmem_builtin': ['PHI', 'GAMLN'],
    })
  },
  lparen: '(',
  rparen: ')',
  NL: { match: /\n/, lineBreaks: true },
  myError: moo.error,
};

// Dynamically generate control records and switch lexical states
const controlRecords: moo.Rules = {
  ...Object.fromEntries(
    validControlRecords.map(record => [
      record.slice(1), // Remove the '$' for the key
      { match: new RegExp(`\\${record}`), next: record.slice(1) } // Escape the '$' for the regex
    ])
  ),
  invalidControlRecord: { match: /\$[a-zA-Z]+/, next: 'main' }
};

const customRules: { [key: string]: moo.Rules } = {
  ABBREVIATED: {
  },
  AES: {
  },
  AESINITIAL: {
  },
  ANNEAL: {
  },
  BIND: {
  },
  CHAIN: {
  },
  CONTR: {
  },
  COVARIANCE: {
  },
  COVR: {
  },
  DATA: {
  },
  DEFAULT: {
  },
  DES: {
  },
  DESIGN: {
  },
  ERROR: {
  },
  ESTIMATION: {
  },
  ESTIMATE: {
  },
  ESTM: {
  },
  ETAS: {
  },
  PHIS: {
  },
  FORMAT: {
  },
  INDEX: {
  },
  INDXS: {
  },
  INFN: {
  },
  INPUT: {
  },
  LEVEL: {
  },
  MIX: {
  },
  MODEL: {
  },
  MSFI: {
  },
  NONPARAMETRIC: {
  },
  OLKJDF: {
  },
  OMEGA: {
  },
  OMEGAP: {
  },
  OMEGAPD: {
  },
  OMIT: {
  },
  OVARF: {
  },
  PK: {
  },
  PRED: {
  },
  PRIOR: {
  },
  PROBLEM: {
    problem_text: {
      match: /(?<=\s)[^\n]{1,72}/,  // Captures up to 72 characters that are not a newline
      lineBreaks: false,     // We don't want to capture line breaks
    }
  },
  RCOV: {
  },
  RCOVI: {
  },
  SCATTERPLOT: {
  },
  SIGMA: {
  },
  SIGMAP: {
  },
  SIGMAPD: {
  },
  SIMULATION: {
  },
  SIMULATE: {
  },
  SIML: {
  },
  SIZES: {
    sizes_constant: {
      match: /[a-zA-Z_][a-zA-Z0-9_]*/,
      type: moo.keywords({
        'sizes_constant': validSizesOptions,
      })
    },

    sizes_value: {
      match: /-?\d+/, // Values (positive, zero, or negative)
      value: (str: any) => Number(str),
    } as any,
  },
  SLKJDF: {
  },
  SUBROUTINES: {
  },
  SUPER: {
  },
  SVARF: {
  },
  TABLE: {
  },
  THETA: {
  },
  THI: {
  },
  THETAI: {
  },
  THETAP: {
  },
  THETAPV: {
  },
  THETAR: {
  },
  THR: {
  },
  TOL: {
  },
  TTDF: {
  },
  WARNINGS: {
  },
};

// Initialize the lexer
const nmtranLexer = moo.states({
  main: {
    ...controlRecords,
    ...mainRules
  },
  // Dynamically generate states for each control record
  ...Object.fromEntries(
    validControlRecords.map(record => [
      record.slice(1), // State name
      {
        ...controlRecords,
        ...customRules[record.slice(1)],
        ...mainRules
      }
    ])
  )
})

// const nmtranLexer: moo.Lexer = moo.compile(lexerRules);

async function main(inputFile: string, outputFile: string) {
  let logText = '';
  try {
    const controlStream = (await fs.readFile(inputFile)).toString();
    nmtranLexer.reset(controlStream);
    while (true) {
      const token = nmtranLexer.next();
      if (!token) {
        break;
      }
      logText += JSON.stringify(token) + '\n';
    }
    await fs.writeFile(outputFile, logText);
  } catch (err) {
    if (err instanceof Error) {
      console.log(err.stack);
    } else {
      console.log('An unknown error occurred:', err);
    }
  }  
}

main(
  './test/minimal.mod',
  './test/lexer-log.json'
);
