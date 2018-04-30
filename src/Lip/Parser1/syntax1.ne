@{%
  const moo = require("moo");
  const lexer = moo.compile({
    ws: { match: /[ \t\n]+/, lineBreaks: true },
    word: /[a-zA-Z0-9]+/,
    punctuation:  /\(|\)|=/
  });
%}
@lexer lexer

@{%
  const Ast = require("../Lip.Data.Ast");
  const ParserData = require("../Lip.Parser.Parser1.Data");
  const ref = Ast.Reference.create;
  const app = Ast.Application.create;
  const abs = Ast.Abstraction.create;
  const pos = ParserData.Position.create;
%}

@{% const main = ([lspace, body, rspace]) => body.value %}
MAIN -> 
  %ws:? FUNCTION %ws:? {% main %}
| %ws:? PAIR_LEFT %ws:? {% main %}
| %ws:? REFERENCE %ws:? {% main %}

FUNCTION ->
  ARGUMENTS "=" %ws:? BODY {%
    ([[args], equal, space, body]) => {
      const text = args.reduce((m, [i, s]) => m + i.text + s.text, "") + equal.text + (space || { text: "" }).text + body.text;
      const ast = args.reverse().reduce((body, [head, space]) => {
        const headPos = pos({
          startLine: head.line - 1, endLine: head.line - 1 + head.lineBreaks,
          startColumn: head.col - 1, endColumn: head.col - 1 + head.text.length
        });
        const text = head.text + body.text;
        return { value: abs(head.value)(body.value)(headPos), text };
      }, Object.assign({}, body, { text: equal.text + space.text + body.text }));
      return ast;
    }
  %}

ARGUMENTS -> (%word %ws):+

@{%
  const body = ([term]) => term
%}
BODY ->
  PARENS {% body %}
| PAIR_LEFT {% body %}
| REFERENCE {% body %}

@{%
  const parens = ([lparens, lspace, term, rspace, rparens]) => {
    const text = lparens.text + (lspace || { text: "" }).text + term.text + (rspace || { text: "" }).text + rparens.text;
    const ast = term.value;
    return { col: lparens.col - 1, text, value: ast };
  }
%}
PARENS ->
  "(" %ws:? PAIR_LEFT %ws:? ")" {% parens %}
| "(" %ws:? PARENS %ws:? ")" {% parens %}
| "(" %ws:? REFERENCE %ws:? ")" {% parens %}
| "(" %ws:? FUNCTION %ws:? ")" {% parens %}

@{%
  const pairLeft = ([left, space, right]) => {
    const text = left.text + (space || { text: "" }).text + right.text;
    const ast = app(left.value)(right.value)(pos({
      startLine: left.line - 1, endLine: right.line - 1 + right.lineBreaks,
      startColumn: left.col - 1, endColumn: right.col - 1 + right.text.length
    }));
    return { col: left.col, text, value: ast };
  }
%}
PAIR_LEFT ->
  PARENS %ws:? PARENS {% pairLeft %}
| PARENS %ws:? REFERENCE {% pairLeft %}
| REFERENCE %ws:? PARENS {% pairLeft %}
| REFERENCE %ws REFERENCE {% pairLeft %}
| PAIR_LEFT %ws:? PARENS {% pairLeft %}
| PAIR_LEFT %ws REFERENCE {% pairLeft %}

REFERENCE -> %word {%
  ([reference]) => {
    const ast = ref(reference.value)(pos({
      startLine: reference.line - 1, endLine: reference.line - 1 + reference.lineBreaks,
      startColumn: reference.col - 1, endColumn: reference.col - 1 + reference.text.length
    }));
    return Object.assign({}, reference, { value: ast });
  }
%}