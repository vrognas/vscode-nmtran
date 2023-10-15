@{%
  "use strict";
  Object.defineProperty(exports, "__esModule", { value: true });
  const moo = require("moo");
  const fs = require("fs/promises");
  const constants_1 = require("../constants");
  const mainRules = {
      WS: /[ \t]+/,
      comment: /;.*?$/,
      math_op: ['*', '/', '+', '-', '**'],
      logical_op: ['.NOT.', '.AND.', '.OR.', '.EQ.', '.NE.', '.EQN.', '.NEN.', '.LT.', '.LE.', '.GT.', '.GE.', '==', '/=', '<', '<=', '>', '>='],
      assign: '=',
      number: {
          match: /-?(?:\d+\.?\d*|\.\d+)(?:E(?:-)?\d+)?/,
          value: str => parseFloat(str)
      },
      lparen: '(',
      rparen: ')',
      comma: ',',
      dot: '.',
      colon: ':',
      word: /[a-zA-Z][a-zA-Z0-9_]*/,
      NL: { match: /\n/, lineBreaks: true },
      myError: moo.error,
  };
  // Dynamically generate control records and switch lexical states
  const controlRecords = {
      ...Object.fromEntries(constants_1.uniqueControlRecords.map(record => [
          record,
          { match: new RegExp(`\\$${record}`), next: record }
      ])),
      ...Object.fromEntries(constants_1.abbreviatableControlRecords.map(record => [
          record,
          { match: new RegExp(`\\$${record.substring(0, 3)}\\w*`), next: record }
      ])),
      ...Object.fromEntries(Object.keys(constants_1.aliasControlRecords).map(alias => [
          alias,
          { match: new RegExp(`\\$${alias}`), next: constants_1.aliasControlRecords[alias] }
      ])),
      invalidControlRecord: { match: /\$[a-zA-Z]+/, next: 'main' }
  };
  const abbreviatedCodeRules = {
      identifier: {
          match: /[a-zA-Z][a-zA-Z0-9_]*/,
          type: (x) => {
              const upperX = x.toUpperCase();
              const keywordType = moo.keywords(constants_1.statementKeywords)(upperX);
              if (keywordType) {
                  return keywordType;
              }
              // Check if it's a reserved data item label
              if (constants_1.reservedDataItemLabels.includes(upperX)) {
                  return 'reservedDataItemLabel';
              }
              // Default to 'identifier' if it's neither
              return 'identifier';
          }
      },
      subroutine_call: /CALL\s+[a-zA-Z_][a-zA-Z0-9_]*/,
      continuation: /&\s*$/
  };
  const customRules = {
      ABBREVIATED: {},
      AES: {
          ...abbreviatedCodeRules
      },
      AESINITIAL: {
          ...abbreviatedCodeRules
      },
      ANNEAL: {},
      BIND: {},
      CHAIN: {},
      CONTR: {},
      COVARIANCE: {},
      COVR: {},
      DATA: {
          // For single character IGNORE or IGN like IGNORE=c, IGN=c, IGNORE='c' or IGNORE="c"
          data_ignore_char: {
              match: /IGN(?:ORE)?\s*=\s*(?:'[^'\n]'|"[^"\n]"|[a-zA-Z0-9@#])/,
              value: (str) => str.split('=')[1].trim().replace(/['"]/g, '')
          },
          // For alphabetic character IGNORE or IGN like IGNORE=@ or IGN=@
          data_ignore_alpha: {
              match: /IGN(?:ORE)?\s*=\s*@/,
              value: (str) => str.split('=')[1].trim()
          },
          // For IGNORE=(list) or ACCEPT=(list)
          data_ignore_accept_list: {
              match: /(?:IGN(?:ORE)?|ACC(?:EPT)?)\s*=?\s*\((?:[a-zA-Z0-9_'".NEQGLT\/=\-\s,<>=]+)\)/,
              value: (str) => {
                  const type = str.startsWith('IGN') ? 'IGNORE' : 'ACCEPT';
                  const listStr = str.match(/\((.*)\)/)[1];
                  const list = listStr.split(',').map((s) => s.trim());
                  return { type, list };
              }
          },
          data_null: {
              match: /NULL\s*=\s*(?:[a-zA-Z0-9_]+)/,
              value: (str) => str.split('=')[1].trim() // Extract the value after the equal sign
          },
          data_pred_ignore: /PRED_IGNORE_DATA/,
          data_nowide: /NOWIDE/,
          data_wide: /WIDE/,
          data_checkout: /CHECKOUT/,
          // Match filenames that are surrounded by quotes or not, up to 80 characters.
          // Special characters are allowed if the filename is quoted.
          data_format: {
              match: /\((?:[^)]+)\)/,
              value: (str) => str.slice(1, -1).trim() // Remove parentheses and trim
          },
          data_filename: {
              match: /'[^'\n]{1,80}'|"[^"\n]{1,80}"|[^ ,;()='"\n]{1,80}/,
              value: (str) => {
                  // Remove quotes if present
                  if (str.startsWith("'") || str.startsWith('"')) {
                      return str.slice(1, -1);
                  }
                  return str;
              }
          },
          unknown_option: {
              match: /[a-zA-Z0-9_\-\/=]+/,
              value: (str) => str.trim() // Trim any extra spaces
          },
      },
      DEFAULT: {},
      DES: {
          ...abbreviatedCodeRules
      },
      DESIGN: {},
      ERROR: {
          ...abbreviatedCodeRules
      },
      ESTIMATION: {},
      ESTIMATE: {},
      ESTM: {},
      ETAS: {},
      PHIS: {},
      FORMAT: {},
      INDEX: {},
      INDXS: {},
      INFN: {
          ...abbreviatedCodeRules
      },
      INPUT: {
          inputDataItemLabelDropped: {
              match: /[a-zA-Z_][a-zA-Z0-9_]{0,19}(?=\s*=\s*(?:DROP|SKIP))/,
              type: "inputDataItemLabelDropped"
          },
          inputDataItemLabel: {
              match: /[a-zA-Z_][a-zA-Z0-9_]{0,19}/,
              type: moo.keywords({
                  'reservedDataItemLabel': constants_1.reservedDataItemLabels,
              })
          },
          input_item_form: {
              // This regex tries to capture both "A=B" and "B" forms
              // It assumes that the "=" sign and label names have no spaces around them
              match: /(?:[a-zA-Z_][a-zA-Z0-9_]{0,19})=(?:[a-zA-Z_][a-zA-Z0-9_]{0,19})|(?:[a-zA-Z_][a-zA-Z0-9_]{0,19})/,
          }
      },
      LEVEL: {},
      MIX: {
          ...abbreviatedCodeRules
      },
      MODEL: {},
      MSFI: {},
      NONPARAMETRIC: {},
      OLKJDF: {},
      OMEGA: {},
      OMEGAP: {},
      OMEGAPD: {},
      OMIT: {},
      OVARF: {},
      PK: {
          ...abbreviatedCodeRules
      },
      PRED: {
          ...abbreviatedCodeRules
      },
      PRIOR: {},
      PROBLEM: {
          problem_text: {
              match: /(?<=\s)[^\n]{1,72}/,
              lineBreaks: false,
              next: 'main' // Return to the main state after capturing the problem text
          },
          NL: {
              match: /\n/,
              lineBreaks: true,
              next: 'main'
          },
      },
      RCOV: {},
      RCOVI: {},
      SCATTERPLOT: {},
      SIGMA: {},
      SIGMAP: {},
      SIGMAPD: {},
      SIMULATION: {},
      SIMULATE: {},
      SIML: {},
      SIZES: {
          sizes_constant: {
              match: /[a-zA-Z_][a-zA-Z0-9_]*/,
              type: moo.keywords({
                  'sizes_constant': constants_1.validSizesOptions,
              })
          },
          sizes_value: {
              match: /-?\d+/,
              value: (str) => Number(str),
          }, // Hack to avoid TypeScript error
      },
      SLKJDF: {},
      SUBROUTINES: {},
      SUPER: {},
      SVARF: {},
      TABLE: {},
      THETA: {},
      THI: {},
      THETAI: {},
      THETAP: {},
      THETAPV: {},
      THETAR: {},
      THR: {},
      TOL: {
          ...abbreviatedCodeRules
      },
      TTDF: {},
      WARNINGS: {},
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
          ...constants_1.uniqueControlRecords,
          ...constants_1.abbreviatableControlRecords,
          ...Object.values(constants_1.aliasControlRecords)
      ].map(record => [
          record,
          {
              ...controlRecords,
              ...customRules[record],
              ...mainRules
          }
      ]))
  });
%}

# Pass lexer object using the @lexer option
@lexer nmtranLexer

model -> statements {% id %}

statements
  -> _ statement _ {%
    data =>  [data[1]]
  %}
  | _ statement _ "\n" statements {%
    data => [data[1], ...data[4]]
  %}
  | _ "\n" statements {%
    data => [data[1], ...data[2]]
  %}

statement
  -> var_init {% id %}
  | print_statement {% id %}
  | expression {% id %}
  | if_statement {% id %}
  | dowhile_statement {% id %}

dowhile_statement -> "DO" _ "WHILE" _ "(" _ expression _ ")" "\n" statements "\n" "END" _ "DO" {%
  data => {
    return {
      type: "dowhile_statement",
      condition: data[6],
      body: data[10]
    }
  }
%}

if_statement -> "IF" _ "(" _ expression _ ")" _ "THEN" "\n" statements "\n" "END" _ "IF" {%
  data => {
    return {
      type: "if_statement",
      condition: data[4],
      body: data[10]
    }
  }
%}

print_statement -> "print" __ expression {%
  data => {
    return {
      type: "print_statement",
      expression: data[2]
    }
  }
%}

expression
  -> unary_expression {% id %}
  | binary_expression {% id %}

unary_expression
  -> number {% id %}
  | identifier {% id %}

binary_expression
  -> unary_expression _ operator _ expression {%
    data => {
      return {
        type: "binary_expression",
        left: data[0],
        operator: data[2],
        right: data[4]
      }
    }
  %}

operator
  -> "+" {% id %}
  | "-" {% id %}
  | "*" {% id %}
  | "/" {% id %}
  | "^" {% id %}
  | "**" {% id %}
  | ">" {% id %}
  | "<" {% id %}
  | ">=" {% id %}
  | "<=" {% id %}
  | "==" {% id %}
  | "/=" {% id %}
  | ".GT." {% id %}
  | ".LT." {% id %}
  | ".GE." {% id %}
  | ".LE." {% id %}
  | ".NE." {% id %}
  | ".EQ." {% id %}
  | ".EQN." {% id %}
  | ".NEQ." {% id %}
  | ".NEN." {% id %}
  | ".NOT." {% id %}
  | ".AND." {% id %}
  | ".OR." {% id %}

var_init -> identifier _ "=" _ expression {%
  data => {
    return {
      type: "var_init",
      name: data[0],
      value: data[4]
    }
  }
%}

identifier -> [a-z]:+ {%
  data => data[0].join("")
%}

number
  -> digits "." digits {%
    data => Number(data[0] + "." + data[2])
  %}
  | digits {%
      data => Number(data[0])
    %}

digits -> [0-9]:+ {%
  data => data[0].join("")
%}

_ -> [ ]:* # optional whitespace
__ -> [ ]:+ # required whitespace
