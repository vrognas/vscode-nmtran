model
  -> var_init {% id %}
  | number {% id %}

var_init -> var_name "=" number {%
  data => {
    return {
      type: "var_init",
      name: data[0],
      value: data[2]
    }
  }
%}

var_name -> [a-z]:+ {% id %}

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
