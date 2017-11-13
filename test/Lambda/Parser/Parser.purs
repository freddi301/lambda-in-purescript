module Test.Lambda.Parser.Parser where

import Lambda.Data.Ast (ref, (\), (!))
import Lambda.Parser.Parser (Block(..), IndentLevel(..), parse, parseBlocks, parseIndent)
import Prelude (Unit, discard, ($))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Runner (RunnerEffects)

test :: ∀ e . Spec (RunnerEffects e) Unit
test = describe "Parse" do
  describe "parse" do
    let check string ast = shouldEqual (parse string) ast
    it "works" do
      check "hello" $ ref "hello"
      check "a b" ("a" ! "b")
      check "a b c d" $ "a" ! "b" ! "c" ! "d"
      check "a, b, c, d" $ "a" ! ("b" ! ("c" ! "d"))
      check "ab cd = (ef gh)" ("ab" \ "cd" \ ("ef" ! "gh"))
      check "ab cd = (ef gh (x = (x)))" ("ab" \ "cd" \ ("ef" ! "gh" ! ("x" \ "x")))
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
    f""" [
      Block "a" [
        Block "b" []
      ],
      Block "c" [
        Block "d" [],
        Block "e" [
          Block "f" []
        ]
      ]
    ]
