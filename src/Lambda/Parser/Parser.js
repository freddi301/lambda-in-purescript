"use strict";

const nearleyMake = require("nearley-make");
const fs = require("fs");
var path = require("path");

const grammar = fs.readFileSync(path.join(__dirname, "syntax.ne"), "utf-8");

const Ast = require("Lambda.Data.Ast");

function parse(text) {
  const parser = nearleyMake(grammar, {
    require: require,
    ref: Ast.ref,
    app: Ast.app,
    abs: Ast.abs,
    named: Ast.Named.create
  });
  parser.feed(text);
  if (parser.results.length === 0) throw new Error("unecpected end of input");
  if (parser.results.length > 1) {
    console.dir(parser.results, { colors: true, depth: null });
    throw new Error("ambigous syntax");
  }
  return parser.results[0];
}

exports.parse = parse;
