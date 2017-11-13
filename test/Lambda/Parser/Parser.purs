module Test.Lambda.Parser.Parser where

import Lambda.Data.Ast (ref, (\), (!))
import Lambda.Parser.Parser (parse)
import Prelude (discard, ($))
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)

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
