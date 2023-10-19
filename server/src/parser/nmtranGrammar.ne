@{%
  "use strict";
  Object.defineProperty(exports, "__esModule", { value: true });
  const moo = require("moo");
  const fs = require("fs/promises");
  const constants_1 = require("../constants");
  const mainRules = {
      WS: /[ \t]+/,
      comment: /;[^\n]*/,
      exponent_op: '**',
      logical_op: ['.NOT.', '.AND.', '.OR.'],
      compare_op: ['.EQ.', '.NE.', '.EQN.', '.NEN.', '.LT.', '.LE.', '.GT.', '.GE.', '==', '/=', '<', '<=', '>', '>='],
      arithmetic_op: ['*', '/', '+', '-'],
      assign: '=',
      number: {
          match: /(?:\d+\.?\d*|\.\d+)/,
          value: str => parseFloat(str)
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
          {
              match: new RegExp(`\\$${alias}`),
              type: (x) => {
                  const upperX = x.slice(1).toUpperCase(); // Remove $ and convert to uppercase
                  return constants_1.aliasControlRecords[upperX] || upperX;
              },
              next: constants_1.aliasControlRecords[alias]
          }
      ])),
      invalidControlRecord: { match: /\$[a-zA-Z]+/, next: 'main' }
  };
  const abbreviatedCodeRules = {
      scientificNotation: /[eEdD](?=[\(\+\-\d])/,
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
      continuation: /&\s*$/,
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
      ETAS: {},
      PHIS: {},
      FORMAT: {},
      INDEX: {},
      INFN: {
          ...abbreviatedCodeRules
      },
      INPUT: {
          inputDataItemLabelDropped: {
              match: /[a-zA-Z_][a-zA-Z0-9_]{0,19}(?=\s*=\s*(?:DROP|SKIP))/,
              type: "inputDataItemLabelDropped"
          },
          drop: {
              match: /DROP|SKIP/,
              type: moo.keywords({
                  'DROP': ['DROP', 'SKIP']
              })
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
      THETAI: {},
      THETAP: {},
      THETAPV: {},
      THETAR: {},
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

# -------------------- Entire model file (control stream) ----------------------

controlStream -> controlStreamPart:* {%
    data => {
        // 'controlStreamPart' will be an array; flatten it
        return data[0].flat(Infinity).filter(item => item !== null);;
    }
%}
  # controlRecord controlStream
  # | controlRecord {% id %}
  # | _ comment controlStream
  # | _ comment
  # | __ controlStream
  # | __
  # | NL controlStream
  # | NL

controlStreamPart ->
  controlRecord
  | comment
  | separator

# ------------------------- Control record contents ----------------------------

controlRecord ->
  sizesRecord {% id %}
  | problemRecord {% id %}
  | inputRecord {% id %}
  | dataRecord {% id %}
  | subroutinesRecord {% id %}
  | modelRecord {% id %}
  | abbreviatedRecord {% id %}
  | infnRecord {% id %}
  | predRecord {% id %}
  | pkRecord {% id %}
  | desRecord {% id %}
  | errorRecord {% id %}
  | thetaRecord {% id %}
  | omegaRecord {% id %}
  | sigmaRecord {% id %}
  | estimationRecord {% id %}
  | covarianceRecord {% id %}
  | etasRecord {% id %}
  | tableRecord {% id %}

# -------------------------- Abbreviated Code Rules ----------------------------

# Important that leading whitespace is consumed by the "_" rule
abbreviatedCode ->
  _ statement abbreviatedCode
  | _ statement
  | _ comment abbreviatedCode
  | _ NL abbreviatedCode

statement ->
  assignmentStatement
  | conditionalStatement
  | dowhileStatement
  | exitStatement
  # | specialStatement

## ---- Assignment -------------------------------------------------------------

assignmentStatement ->
  assignmentLHS _ "=" _ assignmentRHS {%
    data => {
      return {
        type: "assignment",
        left: data[0],
        right: data[4]
      };
    }
  %}

assignmentLHS -> identifier {% id %}
  | inputDataItemLabel {% id %}
  | reservedDataItemLabel {% id %}
  | inital_condition {% id %}
  | dadt {% id %}


assignmentRHS -> expression {% id %}

expression -> additive_expression {% id %}

additive_expression
  -> multiplicative_expression {% id %}
  | additive_expression _ "+" _ multiplicative_expression {%
      data => {
        return {
          type: "additive_expression",
          left: data[0],
          operator: '+',
          right: data[4]
        };
      }
  %}
  | additive_expression _ "-" _ multiplicative_expression {%
      data => {
        return {
          type: "subtractive_expression",
          left: data[0],
          operator: '-',
          right: data[4]
        };
      }
  %}

multiplicative_expression
  -> exponentiation_expression {% id %}
  | multiplicative_expression _ "*" _ exponentiation_expression {%
      data => {
        return {
          type: "multiplicative_expression",
          left: data[0],
          operator: '*',
          right: data[4]
        };
      }
  %}
  | multiplicative_expression _ "/" _ exponentiation_expression {%
      data => {
        return {
          type: "division_expression",
          left: data[0],
          operator: '/',
          right: data[4]
        };
      }
  %}

exponentiation_expression
  -> unary_expression {% id %}
  | unary_expression _ "**" _ exponentiation_expression {%
      data => {
        return {
          type: "exponentiation_expression",
          left: data[0],
          operator: '**',
          right: data[4]
        };
      }
  %}

unary_expression -> primary_expression {% id %}

primary_expression
  -> assignmentLHS {% id %}
  | compartment_amt {% id %}
  | number {% id %}
  | theta {% id %}
  | eta {% id %}
  | epsilon {% id %}
  | identifier {% id %}
  | function_call {% id %}
  | "(" _ expression _ ")" {%
      data => {
        return {
          type: "paren_expression",
          expression: data[2]
        };
      }
  %}

function_call
  -> function_name "(" _ expression _ ")" {%
      data => {
        return {
          type: "function_call",
          name: data[0],
          argument: data[3]
        };
      }
  %}

function_name
  -> %nonmem_protect
  | %nonmem_builtin
  | %fortran_builtin

## ---- Conditional (IF) statements --------------------------------------------

conditionalStatement ->
  simpleIfStatement
  | complexIfStatement

simpleIfStatement ->
  "IF" _ "(" _ logical_expression _ ")" _ assignmentStatement

complexIfStatement -> "IF" _ "(" _ logical_expression _ ")" _ "THEN" NL _ abbreviatedCode:? NL elseIfBlock:* elseBlock:? _ "ENDIF"

elseIfBlock ->
  "ELSEIF" _ "(" _ logical_expression _ ")" _ "THEN" NL abbreviatedCode
  | "ELSE" _ "IF" _ "(" _ logical_expression _ ")" _ "THEN" NL abbreviatedCode

elseBlock ->
  "ELSE" NL abbreviatedCode

logical_expression ->
  compare_binary_expression
  | compare_binary_expression _ %logical_op _ logical_expression {%
    data => {
      return {
        type: "logical_expression",
        left: data[0],
        operator: data[2],
        right: data[4]
      };
    }
%}

compare_binary_expression ->
  compare_term _ %compare_op _ compare_term {%
    data => {
      return {
        type: "compare_binary_expression",
        left: data[0],
        operator: data[2],
        right: data[4]
      };
    }
%}

compare_term ->
  identifier
  | number
  | reservedDataItemLabel
  | inputDataItemLabel
  | inputDataItemLabelDropped

## ---- DO WHILE statements ----------------------------------------------------

dowhileStatement ->
  "DO" _ "WHILE" _ "(" _ logical_expression_while _ ")" _ NL _ abbreviatedCode:? NL _ "END" _ "DO"
  | "DOWHILE" _ "(" _ logical_expression_while _ ")" _ NL _ abbreviatedCode:? NL _ "END" _ "DO"
  | "DO" _ "WHILE" _ "(" _ logical_expression_while _ ")" _ NL _ abbreviatedCode:? NL _ "ENDDO"
  | "DOWHILE" _ "(" _ logical_expression_while _ ")" _ NL _ abbreviatedCode:? NL _ "ENDDO"

logical_expression_while -> logical_expression | "DATA" {% id %}

## ---- EXIT statements --------------------------------------------------------

  exitStatement -> "EXIT" _ [02]:? _ [0-999]:? _ NL

## ---- Special statements -----------------------------------------------------

  # specialStatement ->

# --------------------- Custom rules for Control Records -----------------------

## ---- $SIZES -----------------------------------------------------------------
sizesRecord -> %SIZES _ optNL

## ---- $PROBLEM ---------------------------------------------------------------
problemRecord -> %PROBLEM __ %problem_text NL

## ---- $INPUT -----------------------------------------------------------------

inputRecord -> %INPUT separator inputItems

inputItems ->
  _ inputItem inputItems
  | _ inputItem
  | _ inputItem _ comment
  | _ comment inputItems
  | _ comment
  | _ NL inputItems
  | _ NL

inputItem -> 
    inputDataItemLabel {% id %}
  | inputDataItemLabelDropped "=" %DROP
  | %DROP "=" inputDataItemLabelDropped
  | reservedDataItemLabel {% id %}

inputDataItemLabel -> %inputDataItemLabel {% id %}
inputDataItemLabelDropped -> %inputDataItemLabelDropped {% id %}
reservedDataItemLabel -> %reservedDataItemLabel {% id %}

## ---- $DATA ------------------------------------------------------------------
dataRecord -> %DATA separator %data_filename separator filterList

filterList ->
  _ filter separator filterList
  | filter {% id %}
  | _ comment filterList
  | _ comment {% id %}
  | NL filterList
  | NL {% id %}

filter ->
    %data_ignore_char {% id %}
  | %data_ignore_accept_list {% id %}

# ---- $SUBROUTINES ------------------------------------------------------------
subroutinesRecord -> %SUBROUTINES _ "ADVAN13" __ %word "=" %number NL

# ---- $MODEL ------------------------------------------------------------------
modelRecord -> %MODEL _ optNL

## ---- $INFN ------------------------------------------------------------------
infnRecord -> %INFN _ optNL abbreviatedCode

# ---- $ABBREVIATED ------------------------------------------------------------
abbreviatedRecord -> %ABBREVIATED _ ("PROTECT"):? optNL

## ---- $PK --------------------------------------------------------------------
pkRecord -> %PK _ optNL abbreviatedCode

## ---- $PRED -----------------------------------------------------------------
predRecord -> %PRED _ optNL abbreviatedCode

## ---- $DES -------------------------------------------------------------------
desRecord -> %DES separator abbreviatedCode

## ---- $ERROR -----------------------------------------------------------------
errorRecord -> %ERROR _ optNL abbreviatedCode

## ---- $THETA -----------------------------------------------------------------
thetaRecord -> %THETA separator initalTheta_format:*

initalTheta_format -> _ (%THETA __ ):? initalTheta _ ("FIX" _):? (comment):? NL

initalTheta ->
  number {%
    data => {
      return {
        type: "theta-init",
        init: data[0]
      };
    }
  %}
  | "(" _ number _ "," _ number _ ")" {%
    data => {
      return {
        type: "theta-init",
        init: data[6]
      };
    }
  %}
  | "(" _ "," _ number _ "," _ number _ ")" {%
    data => {
      return {
        type: "theta-init",
        init: data[4]
      };
    }
  %}
  | "(" _ number _ "," _ "," _ number _ ")" {%
    data => {
      return {
        type: "theta-init",
        init: null
      };
    }
  %}
  | "(" _ number _ "," _ number _ "," _ number _ ")" {%
    data => {
      return {
        type: "theta-init",
        init: data[6]
      };
    }
  %}

## ---- $OMEGA -----------------------------------------------------------------
omegaRecord -> %OMEGA _ optNL

## ---- $SIGMA -----------------------------------------------------------------
sigmaRecord -> %SIGMA _ optNL

## ---- $ESTIMATION ------------------------------------------------------------
estimationRecord -> %ESTIMATION _ optNL

## ---- $COVARIANCE ------------------------------------------------------------
covarianceRecord -> %COVARIANCE _ optNL

## ---- $ETAS ------------------------------------------------------------------
etasRecord -> %ETAS _ optNL

## ---- $TABLE -----------------------------------------------------------------
tableRecord -> %TABLE separator

# ---------------------------- Terminal symbols --------------------------------

theta -> %theta "(" [0-9]:+ ")" {%
  data => {
    return {
      type: "theta",
      index: data[2]
    };
  }
%}

eta -> %eta "(" [0-9]:+ ")" {%
  data => {
    return {
      type: "eta",
      index: data[2]
    };
  }
%}

epsilon -> %epsilon "(" [0-9]:+ ")" {%
  data => {
    return {
      type: "epsilon",
      index: data[2]
    };
  }
%}

operator
  -> %logical_op
  | %compare_op
  | %arithmetic_op
  | %exponent_op

identifier ->
  %identifier {% id %}
  | "+" _ %identifier
  | "-" _ %identifier
  
dadt -> "DADT" "(" [0-9]:+ ")" {%
  data => {
    // Combine the array of digits into a single integer
    const compartmentNumber = parseInt(data[2].map(d => d.text).join(''), 10);

    // Update the existing object for the number token
    data[2][0].value = compartmentNumber;

    return {
      type: "dadt",
      compartment: data[2][0] // This now holds the integer and original lexer metadata
    };
  }
%}

compartment_amt -> "A" "(" [0-9]:+ ")" {%
  data => {
    // Combine the array of digits into a single integer
    const compartmentNumber = parseInt(data[2].map(d => d.text).join(''), 10);

    // Update the existing object for the number token
    data[2][0].value = compartmentNumber;

    return {
      type: "compartment_amt",
      compartment: data[2][0] // This now holds the integer and original lexer metadata
    };
  }
%}

inital_condition -> "A_0" "(" [0-9]:+ ")" {%
  data => {
    // Combine the array of digits into a single integer
    const compartmentNumber = parseInt(data[2].map(d => d.text).join(''), 10);

    // Update the existing object for the number token
    data[2][0].value = compartmentNumber;

    return {
      type: "initial_condition",
      compartment: data[2][0] // This now holds the integer and original lexer metadata
    };
  }
%}

number ->
  baseNumber exponentPart {%
    (data) => {
      const [base, exponent] = data;
      return base * 10 ** exponent;
    }
  %}
  | baseNumber {%
    (data) => data[0].value
  %}
  | "+" _ baseNumber {%
    (data) => data[2].value
  %}
  | "-" _ baseNumber {%
    (data) => -data[2]
  %}

baseNumber -> %number {% id %}

exponentPart ->
  %scientificNotation signedNumber {%
    (data) => data[1]
  %}
  | %scientificNotation "(" _ signedNumber _ ")" {%
    (data) => data[3]
  %}

signedNumber -> 
  %number {% id %}
  | "+" _ %number {%
    (data) => data[2]
  %}
  | "-" _ %number {%
    (data) => -data[2]
  %}

comment -> %comment {% d => { return null } %}

separator -> __ | NL

_ -> %WS:* {% d => { return null } %} # optional whitespace
__ -> %WS:+ {% d => { return null } %} # required whitespace

optNL -> %NL:* {% d => { return null } %} # optional newlines
NL -> %NL:+ {% d => { return null } %} # required newline
