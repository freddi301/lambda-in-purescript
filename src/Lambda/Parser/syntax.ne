@builtin "whitespace.ne"

@{%
  const moo = require("moo");
  const lexer = moo.compile({
    ws: /[ \t]+/,
    word: /[a-zA-Z0-9]+/,
    punctuation:  /\(|\)|,|=/
  });
%}
@lexer lexer

@{%
  const Ast = require("../Lambda.Data.Ast");
  const { app, abs, ref } = Ast;
  const named = Ast.Named.create;
%}

MAIN ->
  _ FUNCTION _ {% ([lspace, fun, rspace]) => fun %}

LEAF ->
  REFERENCE {% ([reference]) => reference %}
| PARENS {% ([parens]) => parens %}

FUNCTION ->
  ARGUMENTS "=" _ BODY {% ([[name, ...args], equal, space, body]) => named(name)(args.reverse().reduce((body, head) => abs(head)(body), body)) %}

ARGUMENTS ->
  (WORD __):+ {% ([words, space]) => words.map(([word]) => word) %}

@{%
  const body = ([term]) => term
%}
BODY ->
  PARENS {% body %}
| PAIR_LEFT {% body %}
| PAIR_RIGHT {% body %}
| REFERENCE {% body %}

@{%
  const parens = ([lparens, lspace, term, rspace, rparens]) => term
%}
PARENS ->
  "(" _ PAIR_LEFT _ ")" {% parens %}
| "(" _ PAIR_RIGHT _ ")" {% parens %}
| "(" _ PARENS _ ")" {% parens %}
| "(" _ REFERENCE _ ")" {% parens %}

@{%
  const pairRight = ([left, lspace, comma, rspace, right]) => app(left)(right)
%}
PAIR_RIGHT ->
  LEAF _ "," _ LEAF {% pairRight %}
| LEAF _ "," _ PAIR_RIGHT {% pairRight %}
| LEAF _ "," _ PAIR_LEFT {% pairRight %}
| PAIR_LEFT _ "," _ LEAF {% pairRight %}
| PAIR_LEFT _ "," _ PAIR_LEFT {% pairRight %}
| PAIR_LEFT _ "," _ PAIR_RIGHT {% pairRight %}
| LEAF _ "," {% pairRight %}

@{%
  const pairLeft = ([left, space, right])=> app(left)(right)
%}
PAIR_LEFT ->
  PARENS _ PARENS {% pairLeft %}
| PARENS _ REFERENCE {% pairLeft %}
| REFERENCE _ PARENS {% pairLeft %}
| REFERENCE __ REFERENCE {% pairLeft %}
| PAIR_LEFT _ PARENS {% pairLeft %}
| PAIR_LEFT __ REFERENCE {% pairLeft %}

REFERENCE -> WORD {% ([reference]) => ref(reference) %}

WORD -> %word {% ([token]) => token.text %}
