@builtin "whitespace.ne"

MAIN ->
  _ LEAF _ {% d => d[1] %}
| _ PAIR_LEFT _ {% d => d[1] %}
| _ PAIR_RIGHT _ {% d => d[1] %}
| _ ASSOCIATION _ {% d => d[1] %}

LEAF ->
  REFERENCE {% d => d[0] %}
| PARENS {% d => d[0] %}

ASSOCIATION ->
  ARGUMENTS _ "=" _ PARENS {% d => d[0].reverse().reduce((body, head) => abs(head)(body), d[4]) %}

ARGUMENTS ->
  WORD {% d => [d[0]] %}
| ARGUMENTS __ WORD {% d => d[0].concat([d[2]]) %}

PARENS ->
  "(" _ PAIR_LEFT _ ")" {% d => d[2] %}
| "(" _ PAIR_RIGHT _ ")" {% d => d[2] %}
| "(" _ PARENS _ ")" {% d => d[2] %}
| "(" _ REFERENCE _ ")" {% d => d[2] %}
| "(" _ ASSOCIATION _ ")" {% d => d[2] %}

PAIR_RIGHT ->
  LEAF _ "," _ LEAF {% d => app(d[0])(d[4]) %}
| LEAF _ "," _ PAIR_RIGHT {% d => app(d[0])(d[4]) %}
| LEAF _ "," _ PAIR_LEFT {% d => app(d[0])(d[4]) %}
| PAIR_LEFT _ "," _ LEAF {% d => app(d[0])(d[4]) %}
| PAIR_LEFT _ "," _ PAIR_LEFT {% d => app(d[0])(d[4]) %}
| PAIR_LEFT _ "," _ PAIR_RIGHT {% d => app(d[0])(d[4]) %}
| LEAF _ "," {% d => d[0] %}

PAIR_LEFT ->
  PARENS _ PARENS {% d => app(d[0])(d[2]) %}
| PARENS _ REFERENCE {% d => app(d[0])(d[2]) %}
| REFERENCE _ PARENS {% d => app(d[0])(d[2]) %}
| REFERENCE __ REFERENCE {% d => app(d[0])(d[2]) %}
| PAIR_LEFT _ PARENS {% d => app(d[0])(d[2]) %}
| PAIR_LEFT __ REFERENCE {% d => app(d[0])(d[2]) %}

REFERENCE -> WORD {% d => ref(d[0]) %}

WORD -> [^\s\n(),=]:+ {% d => d[0].join('') %}
