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
  const ParserData = require("../Lambda.Data.Parser");
  const named = ParserData.Named.create;
  const ref = Ast.Reference.create;
  const app = Ast.Application.create;
  const abs = Ast.Abstraction.create;
  const pos = (start, end) => ParserData.Position.create({
    file: "",
    startLine: 0, endLine: 0,
    startColumn: start, endColumn: end
  });
  const fakePos = ParserData.fakePos;
%}

MAIN ->
  _ FUNCTION _ {% ([lspace, fun, rspace]) => fun %}

LEAF ->
  REFERENCE {% ([reference]) => reference %}
| PARENS {% ([parens]) => parens %}

FUNCTION ->
  ARGUMENTS "=" _ BODY {% ([[name, ...args], equal, space, body]) => named(name)(fakePos)(args.reverse().reduce((body, head) => abs(head)(body)(fakePos)(fakePos), body)) %}

ARGUMENTS ->
  (%word __):+ {% ([words, space]) => words.map(([word]) => word.value) %}

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
  const pairRight = ([left, lspace, comma, rspace, right]) => app(left)(right)(fakePos)
%}
PAIR_RIGHT ->
  LEAF _ "," _ LEAF {% pairRight %}
| LEAF _ "," _ PAIR_RIGHT {% pairRight %}
| LEAF _ "," _ PAIR_LEFT {% pairRight %}
| PAIR_LEFT _ "," _ LEAF {% pairRight %}
| PAIR_LEFT _ "," _ PAIR_LEFT {% pairRight %}
| PAIR_LEFT _ "," _ PAIR_RIGHT {% pairRight %}

@{%
  const pairLeft = ([left, space, right]) => { return app(left)(right)(fakePos) }
%}
PAIR_LEFT ->
  PARENS _ PARENS {% pairLeft %}
| PARENS _ REFERENCE {% pairLeft %}
| REFERENCE _ PARENS {% pairLeft %}
| REFERENCE __ REFERENCE {% pairLeft %}
| PAIR_LEFT _ PARENS {% pairLeft %}
| PAIR_LEFT __ REFERENCE {% pairLeft %}

REFERENCE -> %word {% ([reference]) => ref(reference.value)(pos(reference.col - 1, reference.col - 1 + reference.text.length)) %}