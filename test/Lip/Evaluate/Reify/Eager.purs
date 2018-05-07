module Test.Lip.Evaluate.Reify.Eager where

import Data.Either
import Lip.Evaluate.Reify.Eager
import Prelude
import Prelude

import Lambda.Data.Ast (ref)
import Lip.Data.Ast (Ast(..))
import Lip.Data.Ast.Helpers ((\), (!))
import Lip.Evaluate (Evaluate)
import Prelude (Unit, unit, discard, (>>>))
import Test.Lip.Evaluate.Boolean as BooleanTest
import Test.Spec (Spec, describe, it, pending)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Runner (RunnerEffects)

test :: ∀ e . Spec (RunnerEffects e) Unit
test = describe "reify eager" do
  describe "evaluate" $ BooleanTest.test evaluate
  describe "enhance" do
    BooleanTest.test $ enhance id
    describe "booleans" do
      let evaluatesTo source result = (enhance globals source) `shouldEqual` (enhance globals source)
      it "respects 'not' truth table" do
        ("NOT" ! "TRUE") `evaluatesTo` (ref "FALSE")
        ("NOT" ! "FALSE") `evaluatesTo` (ref "TRUE")
      it "respects 'and' truth table" do
        ("AND" ! "TRUE" ! "TRUE") `evaluatesTo` (ref "TRUE")
        ("AND" ! "TRUE" ! "FALSE") `evaluatesTo` (ref "FALSE")
        ("AND" ! "FALSE" ! "TRUE") `evaluatesTo` (ref "FALSE")
        ("AND" ! "FALSE" ! "FALSE") `evaluatesTo` (ref "FALSE")
      it "respects 'or' truth table" do
        ("OR" ! "TRUE" ! "TRUE") `evaluatesTo` (ref "TRUE")
        ("OR" ! "TRUE" ! "FALSE") `evaluatesTo` (ref "TRUE")
        ("OR" ! "FALSE" ! "TRUE") `evaluatesTo` (ref "TRUE")
        ("OR" ! "FALSE" ! "FALSE") `evaluatesTo` (ref "FALSE")
      it "works with nested" do
        ("OR" ! ("AND" ! "TRUE" ! "TRUE") ! ("NOT" ! "TRUE")) `evaluatesTo` (ref "TRUE")
  describe "stop" do
    BooleanTest.test $ \ast -> (either (const ast) id) (stop Right ast)
    pending "stop on ast node"

globals :: Evaluate String Unit -> Evaluate String Unit
globals evaluate ast = evaluate replace where
  replace  = case ast of
    Reference "TRUE" _ -> "x" \ "y" \ "x"
    Reference "FALSE" _ -> "x" \ "y" \ "y"
    Reference "NOT" _ -> "p" \ "p" ! "FALSE" ! "TRUE"
    Reference "AND" _ -> "a" \ "b" \ "a" ! "b" ! "FALSE"
    Reference "OR" _ -> "a" \ "b" \ "a" ! "FALSE" ! "b"
    _ -> ast