module Test.Lip.Evaluate.Reify.Eager where

import Data.Either
import Lip.Evaluate.Reify.Eager
import Prelude

import Lip.Data.Ast (Ast(..))
import Lip.Data.Ast.Helpers ((\), (!), ref)
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
    let evaluatesTo source result = (enhance globals source) `shouldEqual` (enhance globals source)
    booleanTestSuite evaluatesTo
  describe "stop" do
    BooleanTest.test $ \ast -> (either (const ast) id) (stop Right ast)
    it "stops on ast node" do
      let stopsOn ast result = (stop stops ast) `shouldEqual` result
      (("a" \ "b" \ ("STOP" ! "a")) ! ("x" \ "x") ! ("x" \ "y" \ "y")) `stopsOn` (Left ("x" \ "x"))
    describe "as enhance" do
      let se = \ast -> (either (const ast) id) (stop (globals >>> Right) ast)
      let evaluatesTo source result = (se source) `shouldEqual` (se source)
      booleanTestSuite evaluatesTo
  describe "step" do
    BooleanTest.test $ (step >>> runStep)
  describe "stepEnhance" do
    BooleanTest.test $ ((stepEnhance id) >>> runStep)
    let se = (stepEnhance globals) >>> runStep
    let evaluatesTo source result = (se source) `shouldEqual` (se source)
    booleanTestSuite evaluatesTo
  describe "intermediate" do
    BooleanTest.test $ (intermediate >>> runIntermediate id)
    let se = intermediate >>> runIntermediate globals
    let evaluatesTo source result = (se source) `shouldEqual` (se source)
    booleanTestSuite evaluatesTo

globals :: Evaluate String Unit
globals ast = case ast of
  Reference "TRUE" _ -> "x" \ "y" \ "x"
  Reference "FALSE" _ -> "x" \ "y" \ "y"
  Reference "NOT" _ -> "p" \ "p" ! "FALSE" ! "TRUE"
  Reference "AND" _ -> "a" \ "b" \ "a" ! "b" ! "FALSE"
  Reference "OR" _ -> "a" \ "b" \ "a" ! "FALSE" ! "b"
  _ -> ast

stops :: Ast String Unit -> Either (Ast String Unit) (Ast String Unit)
stops ast = case ast of
  Application (Reference "STOP" _) body _ -> Left body
  _ -> Right ast

booleanTestSuite evaluatesTo = describe "booleans enhanced" do
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