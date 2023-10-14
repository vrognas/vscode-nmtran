# @{%
#   import nmtranLexer from "./nmtranLexer.js";
# %}

# # Pass lexer object using the @lexer option
# @lexer nmtranLexer

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
