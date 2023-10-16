@{%
"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const moo = require("moo");
const fs = require("fs/promises");
const constants_1 = require("../constants");
const mainRules = {
    WS: /[ \t]+/,
    comment: /;[^\n]*/,
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

# Entire model file (control stream)
controlStream ->
  controlRecord controlStream
  | controlRecord {% id %}
  | comment controlStream
  | comment {% id %}
  | %NL controlStream
  | %NL {% id %}

controlRecord ->
  sizesRecord {% id %}
  | problemRecord {% id %}
  | inputRecord {% id %}
  | dataRecord {% id %}
  | subroutinesRecord {% id %}
  | modelRecord {% id %}
  | abbreviatedRecord {% id %}
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

# All control records

# $SIZES
sizesRecord -> %SIZES _ optNL

# $PROBLEM
problemRecord -> %PROBLEM __ %problem_text _ optNL

# $INPUT
inputRecord -> %INPUT separator inputItems

inputItems ->
  _ inputItem separator inputItems
  | inputItem {% id %}
  | %NL inputItems
  | %NL {% id %}

inputItem -> 
    inputDataItemLabel {% id %}
  | inputDataItemLabelDropped "=" %DROP
  | %DROP "=" inputDataItemLabelDropped
  | reservedDataItemLabel {% id %}

inputDataItemLabel -> %inputDataItemLabel {% id %}
inputDataItemLabelDropped -> %inputDataItemLabelDropped {% id %}
reservedDataItemLabel -> %reservedDataItemLabel {% id %}

dataRecord -> %DATA separator %data_filename separator filterList

filterList ->
  _ filter separator filterList
  | filter {% id %}
  | %NL filterList
  | %NL {% id %}

filter -> 
    %data_ignore_char {% id %}

subroutinesRecord -> %SUBROUTINES _ optNL

modelRecord -> %MODEL _ optNL

abbreviatedRecord -> %ABBREVIATED _ optNL

pkRecord -> %PK _ optNL

predRecord -> %PRED _ optNL

desRecord -> %DES _ optNL

errorRecord -> %ERROR _ optNL

thetaRecord -> %THETA _ optNL

omegaRecord -> %OMEGA _ optNL

sigmaRecord -> %SIGMA _ optNL

estimationRecord -> %ESTIMATION _ optNL

covarianceRecord -> %COVARIANCE _ optNL

etasRecord -> %ETAS _ optNL

tableRecord -> %TABLE _ optNL

comment -> %comment {% id %}

separator -> __ | NL

_ -> %WS:* {% id %} # optional whitespace
__ -> %WS:+ {% id %} # required whitespace

optNL -> NL:* {% id %} # optional newlines
NL -> %NL:+ {% id %} # required newline