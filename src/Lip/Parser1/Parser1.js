"use strict";

const nearley = require("nearley");
const grammar = nearley.Grammar.fromCompiled(require("./grammar1"));

exports.parse = function parse(text) {
  const parser = new nearley.Parser(grammar);
  parser.feed(text);
  if (parser.results.length === 1) {
    // console.dir(parser.results[0], { colors: true, depth: null });
    return parser.results[0];
  }
  if (parser.results.length === 0) throw new Error("unecpected end of input");
  if (parser.results.length > 1) {
    // console.dir(parser.results, { colors: true, depth: null });
    throw new Error("ambigous syntax");
  }
};
