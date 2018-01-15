module Test.Lambda.Parser.Parser where

import Lambda.Data.Ast (ref, (\), (!), Named(..))
import Lambda.Parser.Parser (Block(..), IndentLevel(..), blocksToAst, parse, parseBlocks, parseIndent, parseProgram, prettify)
import Prelude (Unit, discard, ($))
import Test.Lambda.Sources.Booleanic (booleanic)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Runner (RunnerEffects)

test :: ∀ e . Spec (RunnerEffects e) Unit
test = describe "Parse" do
  describe "parse" do
    let check string ast = shouldEqual (parse string) ast
    it "works" do
      check "hello = (hello)" $ Named "hello" (ref "hello")
      check "ex1 = (a b)" $ Named "ex1" ("a" ! "b")
      check "ex2 = (a b c d)" $ Named "ex2" $ "a" ! "b" ! "c" ! "d"
      check "ex3 = (a, b, c, d)" $ Named "ex3" $ "a" ! ("b" ! ("c" ! "d"))
      check "ex4 ab cd = (ef gh)" $ Named "ex4" ("ab" \ "cd" \ ("ef" ! "gh"))
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
      check "main y = (f x = x)" $ (("main" \ "main") ! ("y" \ "x" \ "x"))
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

