module Test.Lambda.Parser.Parser where

import Lambda.Data.Ast (Ast(..), ref, (!), (\))
import Lambda.Data.Parser (Block(..), IndentLevel(..), Named(..), Position(..), fakePos)
import Lambda.Parser.Parser (blocksToAst, parse, parseBlocks, parseIndent, parseProgram, parseUnit, prettify)
import Prelude (Unit, discard, unit, ($))
import Test.Lambda.Sources.Booleanic (booleanic)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Runner (RunnerEffects)

test :: ∀ e . Spec (RunnerEffects e) Unit
test = describe "Parse" do
  describe "parseUnit" do
    let check string ast = shouldEqual (parseUnit string) ast
    it "works" do
      check "hello = (hello)" $ Named "hello" unit (ref "hello")
      check "ex1 = (a b)" $ Named "ex1" unit ("a" ! "b")
      check "ex2 = (a b c d)" $ Named "ex2" unit $ "a" ! "b" ! "c" ! "d"
      check "ex3 = (a, b, c, d)" $ Named "ex3" unit $ "a" ! ("b" ! ("c" ! "d"))
      check "ex4 ab cd = (ef gh)" $ Named "ex4" unit ("ab" \ "cd" \ ("ef" ! "gh"))
  describe "parseIndent" do
    let check strings = shouldEqual (parseIndent strings)
    it "works" do
      check "a" (IndentLevel 0 "a")
      check "  b" (IndentLevel 1 "b")
      check "    c" (IndentLevel 2 "c")
      check "      d" (IndentLevel 3 "d")
  describe "parseBlocks" do
    let check string structure = shouldEqual (parseBlocks string) structure
    it "works" do
      check "a\nb" [Block "a" [], Block "b" []]
      check """
a
b"""    [Block "a" [], Block "b" []]
      check """
a
  b
c
  d
  e
    f
  g""" [
      Block "a" [
        Block "b" []
      ],
      Block "c" [
        Block "d" [],
        Block "e" [
          Block "f" []
        ],
        Block "g" []
      ]
    ]
  describe "blocksToAst" do
    let check string ast = shouldEqual (blocksToAst (ref "main") (parseBlocks string)) ast
    it "works" do
      check "main = a" $ (("main" \ "main") ! "a")
      check "main = a a" $ (("main" \ "main") ! ("a" ! "a"))
      check "main x = a" $ (("main" \ "main") ! ("x" \ "a"))
      check "main x = a a" $ (("main" \ "main") ! ("x" \ ("a" ! "a")))
      check "main x y = (a)" $ (("main" \ "main") ! ("x" \ "y" \ "a"))
      check "main x y = a" $ (("main" \ "main") ! ("x" \ "y" \ "a"))
      check "main x y = (a)\n  a z = (z)" $ ("main" \ "main") ! ("x" \ "y" \ (("a" \ "a") ! ("z" \ "z")))
  describe "parseProgram" do
    it "works for booleanic" do
      shouldEqual (parseProgram booleanic) (parseProgram booleanic)
  describe "prettify" do
    let program = """firstline = 1
secondline
  urca = haha ha
    eureka
  lol
    lol = niu
"""
    it "left untouched a pretty source" do
      (prettify program) `shouldEqual` program
    let uglyLine = "a   =  b c c   x   "
    let prettyLine = "a = b c c x\n"
    it "prettify line" do
      (prettify uglyLine) `shouldEqual` prettyLine
  describe "parse" do
    let check string ast = shouldEqual (parse string) ast
    let pos start end = Position { file: "", startLine: 0, endLine: 0, startColumn: start, endColumn: end }
    it "works" do
      check "hello = (hello)" $ Named "hello" (pos 0 5) (Reference "hello" (pos 9 14))
      check "ex1 = a b" $ Named "ex1" (pos 0 3) (Application (Reference "a" (pos 6 7)) (Reference "b" (pos 8 9)) (pos 6 9))
      check "ex2 = (a b c )" $ Named "ex2" (pos 0 3) (Application (Application (Reference "a" (pos 7 8)) (Reference "b" (pos 9 10)) (pos 7 10)) (Reference "c" (pos 11 12)) (pos 7 12))
      check "ex3 = (a, b, c)" $ Named "ex3" (pos 0 3) (Application (Reference "a" (pos 7 8)) (Application (Reference "b" (pos 10 11)) (Reference "c" (pos 13 14)) (pos 10 14)) (pos 7 14))
      check "ex4 ab cd = ef" $ Named "ex4" (pos 0 3) (Abstraction "ab" (Abstraction "cd" (Reference "ef" (pos 12 14)) (pos 7 9) (pos 7 14)) (pos 4 6) (pos 4 14))