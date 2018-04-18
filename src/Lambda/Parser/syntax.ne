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
%}

MAIN -> %ws:? FUNCTION %ws:? {% ([lspace, fun, rspace]) => fun.value %}

LEAF ->
  REFERENCE {% ([reference]) => reference %}
| PARENS {% ([parens]) => parens %}

FUNCTION ->
  ARGUMENTS "=" %ws:? BODY {%
    ([[name, ...args], equal, space, body]) => {
      const text = name.text + args.reduce((m, i) => m + i.text, "") + equal.text + (space || { text: "" }).text + body.text;
      const namePos = pos(name.col - 1, name.col - 1 + name.value.length);
      const namedAst = args.reverse().reduce((body, head) => {
        const headPos = pos(head.col - 1, head.col - 1 + head.value.length);
        const text = head.text + body.text;
        const bodyPos = pos(head.col - 1, head.col - 1 + text.length);
        return { value: abs(head.value)(body.value)(headPos)(bodyPos), text };
      }, Object.assign({}, body, { text: equal.text + space.text + body.text }));
      const ast = named(name.value)(namePos)(namedAst.value);
      return { col: name.col, text, value: ast };
    }
  %}

ARGUMENTS ->
  (%word %ws):+ {% ([words]) => words.map(([word, space]) => Object.assign({}, word, { text: word.text + space.text })) %}

@{%
  const body = ([term]) => term
%}
BODY ->
  PARENS {% body %}
| PAIR_LEFT {% body %}
| PAIR_RIGHT {% body %}
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
| "(" %ws:? PAIR_RIGHT %ws:? ")" {% parens %}
| "(" %ws:? PARENS %ws:? ")" {% parens %}
| "(" %ws:? REFERENCE %ws:? ")" {% parens %}

@{%
  const pairRight = ([left, lspace, comma, rspace, right]) => {
    const text = left.text + (lspace || { text: "" }).text + comma.text + (rspace || { text: "" }).text + right.text;
    const ast = app(left.value)(right.value)(pos(left.col - 1, left.col - 1 + text.length));
    return { col: left.col, text, value: ast }
  }
%}
PAIR_RIGHT ->
  LEAF %ws:? "," %ws:? LEAF {% pairRight %}
| LEAF %ws:? "," %ws:? PAIR_RIGHT {% pairRight %}
| LEAF %ws:? "," %ws:? PAIR_LEFT {% pairRight %}
| PAIR_LEFT %ws:? "," %ws:? LEAF {% pairRight %}
| PAIR_LEFT %ws:? "," %ws:? PAIR_LEFT {% pairRight %}
| PAIR_LEFT %ws:? "," %ws:? PAIR_RIGHT {% pairRight %}

@{%
  const pairLeft = ([left, space, right]) => {
    const text = left.text + (space || { text: "" }).text + right.text;
    const ast = app(left.value)(right.value)(pos(left.col - 1, left.col -1 + text.length));
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
    const ast = ref(reference.value)(pos(reference.col - 1, reference.col - 1 + reference.text.length));
    return Object.assign({}, reference, { value: ast });
  }
%}