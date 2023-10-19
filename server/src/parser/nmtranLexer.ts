import * as moo from "moo";
import * as fs from "fs/promises";
import {
  uniqueControlRecords,
  abbreviatableControlRecords,
  aliasControlRecords,
  validSizesOptions,
  reservedDataItemLabels,
  statementKeywords
} from "../constants";

const mainRules: moo.Rules = {
  WS: /[ \t]+/,
  comment: /;[^\n]*/,
  exponent_op: '**',
  logical_op: ['.NOT.', '.AND.', '.OR.'],
  compare_op: ['.EQ.', '.NE.', '.EQN.', '.NEN.', '.LT.', '.LE.', '.GT.', '.GE.', '==', '/=', '<', '<=', '>', '>='],
  arithmetic_op: ['*', '/', '+', '-'],
  assign: '=',
  number:  {
    match: /(?:\d+\.?\d*|\.\d+)/,
    value: str => parseFloat(str) as any
  },
  lparen: '(',
  rparen: ')',
  comma: ',',
  dot: '.',
  colon: ':',
  word: /[a-zA-Z][a-zA-Z0-9_]*/,
  NL: { match: /\n/, lineBreaks: true },
  unknown_char: /./,
  myError: moo.error,
};

// Dynamically generate control records and switch lexical states
const controlRecords: moo.Rules = {
  ...Object.fromEntries(
    uniqueControlRecords.map(record => [
      record, 
      { match: new RegExp(`\\$${record}`), next: record }
    ])
  ),
  ...Object.fromEntries(
    abbreviatableControlRecords.map(record => [
      record, 
      { match: new RegExp(`\\$${record.substring(0, 3)}\\w*`), next: record }
    ])
  ),
  ...Object.fromEntries(
    Object.keys(aliasControlRecords).map(alias => [
      alias, 
      {
        match: new RegExp(`\\$${alias}`),
        type: (x) => {
          const upperX = x.slice(1).toUpperCase(); // Remove $ and convert to uppercase
          return aliasControlRecords[upperX] || upperX;
        },
        next: aliasControlRecords[alias]
      }
    ])
  ),
  invalidControlRecord: { match: /\$[a-zA-Z]+/, next: 'main' }
};

const abbreviatedCodeRules: moo.Rules = {
  scientificNotation: /[eEdD](?=[\(\+\-\d])/,
  identifier: {
    match: /[a-zA-Z][a-zA-Z0-9_]*/,
    type: (x) => {
      const upperX = x.toUpperCase();
      const keywordType = moo.keywords(statementKeywords)(upperX);
      if (keywordType) {
        return keywordType;
      }
      // Check if it's a reserved data item label
      if (reservedDataItemLabels.includes(upperX)) {
        return 'reservedDataItemLabel';
      }
      // Default to 'identifier' if it's neither
      return 'identifier';
    }
  },
  subroutine_call: /CALL\s+[a-zA-Z_][a-zA-Z0-9_]*/,
  continuation: /&\s*$/,
};

const customRules: { [key: string]: moo.Rules } = {
  ABBREVIATED: {
  },
  AES: {
    ...abbreviatedCodeRules
  },
  AESINITIAL: {
    ...abbreviatedCodeRules
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
  DATA: {
    // For single character IGNORE or IGN like IGNORE=c, IGN=c, IGNORE='c' or IGNORE="c"
    data_ignore_char: {
      match: /IGN(?:ORE)?\s*=\s*(?:'[^'\n]'|"[^"\n]"|[a-zA-Z0-9@#])/,
      value: (str: any) => str.split('=')[1].trim().replace(/['"]/g, '')
    },
    // For alphabetic character IGNORE or IGN like IGNORE=@ or IGN=@
    data_ignore_alpha: {
      match: /IGN(?:ORE)?\s*=\s*@/,
      value: (str: any) => str.split('=')[1].trim()
    },
    // For IGNORE=(list) or ACCEPT=(list)
    data_ignore_accept_list: {
      match: /(?:IGN(?:ORE)?|ACC(?:EPT)?)\s*=?\s*\((?:[a-zA-Z0-9_'".NEQGLT\/=\-\s,<>=]+)\)/,
      value: (str: any) => {
        const type = str.startsWith('IGN') ? 'IGNORE' : 'ACCEPT';
        const listStr = str.match(/\((.*)\)/)![1];
        const list = listStr.split(',').map((s: string) => s.trim());
        return { type, list };
      }
    } as any,
    data_null: {
      match: /NULL\s*=\s*(?:[a-zA-Z0-9_]+)/, // NULL= followed by its value
      value: (str: any) => str.split('=')[1].trim() // Extract the value after the equal sign
    },
    data_pred_ignore: /PRED_IGNORE_DATA/, // Exact match for PRED_IGNORE_DATA
    data_nowide: /NOWIDE/, // Exact match for NOWIDE
    data_wide: /WIDE/, // Exact match for WIDE
    data_checkout: /CHECKOUT/, // Exact match for CHECKOUT
    // Match filenames that are surrounded by quotes or not, up to 80 characters.
    // Special characters are allowed if the filename is quoted.
    data_format: {
      match: /\((?:[^)]+)\)/, // Anything inside parentheses
      value: (str: any) => str.slice(1, -1).trim() // Remove parentheses and trim
    },
    data_filename: {
      match: /'[^'\n]{1,80}'|"[^"\n]{1,80}"|[^ ,;()='"\n]{1,80}/,
      value: (str: any) => {
        // Remove quotes if present
        if (str.startsWith("'") || str.startsWith('"')) {
          return str.slice(1, -1);
        }
        return str;
      }
    },
    unknown_option: {
      match: /[a-zA-Z0-9_\-\/=]+/, // Alphabets, numbers, underscores, hyphens, slashes, and equal signs
      value: (str: any) => str.trim() // Trim any extra spaces
    },
  },
  DEFAULT: {
  },
  DES: {
    ...abbreviatedCodeRules
  },
  DESIGN: {
  },
  ERROR: {
    ...abbreviatedCodeRules
  },
  ESTIMATION: {
  },
  ETAS: {
  },
  PHIS: {
  },
  FORMAT: {
  },
  INDEX: {
  },
  INFN: {
    ...abbreviatedCodeRules
  },
  INPUT: {
    inputDataItemLabelDropped: {
      match: /[a-zA-Z_][a-zA-Z0-9_]{0,19}(?=\s*=\s*(?:DROP|SKIP))/,
      type: "inputDataItemLabelDropped"
    } as unknown as moo.Rule, // Hack to avoid TypeScript error
    drop: {
      match: /DROP|SKIP/,
      type: moo.keywords({
        'DROP': ['DROP', 'SKIP']
      })
    },
    inputDataItemLabel: {
      match: /[a-zA-Z_][a-zA-Z0-9_]{0,19}/, // Up to 20 characters, starts with a letter
      type: moo.keywords({
        'reservedDataItemLabel': reservedDataItemLabels,
      })
    },
    input_item_form: {
      // This regex tries to capture both "A=B" and "B" forms
      // It assumes that the "=" sign and label names have no spaces around them
      match: /(?:[a-zA-Z_][a-zA-Z0-9_]{0,19})=(?:[a-zA-Z_][a-zA-Z0-9_]{0,19})|(?:[a-zA-Z_][a-zA-Z0-9_]{0,19})/,
    }
  },
  LEVEL: {
  },
  MIX: {
    ...abbreviatedCodeRules
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
    ...abbreviatedCodeRules
  },
  PRED: {
    ...abbreviatedCodeRules
  },
  PRIOR: {
  },
  PROBLEM: {
    problem_text: {
      match: /(?<=\s)[^\n]{1,72}/,  // Captures up to 72 characters that are not a newline
      lineBreaks: false,     // We don't want to capture line breaks
      next: 'main'           // Return to the main state after capturing the problem text
    },
    NL: {
      match: /\n/,
      lineBreaks: true ,
      next: 'main'
    },
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
    } as unknown as moo.Rule, // Hack to avoid TypeScript error
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
  THETAI: {
  },
  THETAP: {
  },
  THETAPV: {
  },
  THETAR: {
  },
  TOL: {
    ...abbreviatedCodeRules
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
    // Combine all types of control records into a single array for state generation
    [
      ...uniqueControlRecords,
      ...abbreviatableControlRecords,
      ...Object.values(aliasControlRecords)
    ].map(record => [
      record, // State name
      {
        ...controlRecords,
        ...customRules[record],
        ...mainRules
      }
    ])
  )
});

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
      // // Skip whitespace tokens
      // if (token.type === 'WS') {
      //   continue;
      // }
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
  './test/test.mod',
  './test/lexer-log.json'
);
