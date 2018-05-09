module Test.Lip.Evaluate.Reify.Eager where

import Data.Either
import Lip.Evaluate.Reify.Eager
import Prelude

import Lip.Data.Ast (Ast(..))
import Lip.Data.Ast.Helpers ((\), (!), ref)
import Lip.Evaluate.Reify.Pauseable as Pauseable
import Lip.Evaluate (Evaluate)
import Prelude (Unit, unit, discard, (>>>))
import Test.Lip.Evaluate.Boolean as BooleanTest
import Test.Spec (Spec, describe, it, pending)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Runner (RunnerEffects)

test :: ∀ e . Spec (RunnerEffects e) Unit
test = describe "reify eager" do
  testGlobals
  describe "evaluate" $ BooleanTest.test evaluate
  describe "enhance" do
    BooleanTest.test $ enhance id
    let evaluatesTo source result = (enhance globals source) `shouldEqual` (enhance globals result)
    booleanTestSuite evaluatesTo
  describe "stop" do
    BooleanTest.test $ \ast → (either (const ast) id) (stop Right ast)
    it "stops on ast node" do
      let stopsOn ast result = (stop stops ast) `shouldEqual` result
      (("a" \ "b" \ ("STOP" ! "a")) ! ("x" \ "x") ! ("x" \ "y" \ "y")) `stopsOn` (Left ("x" \ "x"))
    describe "as enhance" do
      let se = \ast → (either (const ast) id) (stop (globals >>> Right) ast)
      let evaluatesTo source result = (se source) `shouldEqual` (globals result)
      booleanTestSuite evaluatesTo
  describe "step" do
    BooleanTest.test $ (step >>> runStep)
  describe "stepEnhance" do
    BooleanTest.test $ ((stepEnhance id) >>> runStep)
    let se = (stepEnhance globals) >>> runStep
    let evaluatesTo source result = (se source) `shouldEqual` (globals result)
    booleanTestSuite evaluatesTo
  describe "intermediate" do
    BooleanTest.test $ (intermediate >>> runIntermediate id)
    let se = intermediate >>> runIntermediate globals
    let evaluatesTo source result = (se source) `shouldEqual` (globals result)
    booleanTestSuite evaluatesTo
    it "steps" do
      let sample = (("x" \ "x") ! ("y" \ "y")) ! ("z" \ "z")
      let inte = getResult >>> shouldEqual
      let step1 = intermediate sample
      inte step1 $ (("x" \ "x") ! ("y" \ "y")) ! ("z" \ "z")
      let step2 = nextInter step1
      inte step2 $ ("x" \ "x") ! ("y" \ "y")
      let step3 = nextInter step2
      inte step3 $ ("y" \ "y")
      let step4 = nextInter step3
      inte step4 $ ("y" \ "y") ! ("z" \ "z")
      let step5 = nextInter step4
      inte step5 ("z" \ "z")
      let step6 = nextInter step5
      step6 `shouldEqual` (End ("z" \ "z"))
  describe "puaseable eager" do
    BooleanTest.test $ (Pauseable.eager >>> Pauseable.runIntermediate id)
    let se = Pauseable.eager >>> Pauseable.runIntermediate globals
    let evaluatesTo source result = (se source) `shouldEqual` (globals result)
    booleanTestSuite evaluatesTo
    it "steps" do
      let sample = (("x" \ "x") ! ("y" \ "y")) ! ("z" \ "z")
      let inte = Pauseable.getResult >>> shouldEqual
      let step1 = Pauseable.eager sample
      inte step1 $ ("x" \ "x") ! ("y" \ "y")
      let step2 = Pauseable.next step1
      inte step2 $ ("y" \ "y")
      let step3 = Pauseable.next step2
      inte step3 $ ("z" \ "z")
      let step4 = Pauseable.next step3
      inte step4 $ ("y" \ "y") ! ("z" \ "z")
      let step5 = Pauseable.next step4
      inte step5 ("z" \ "z")
      let step6 = Pauseable.next step5
      step6 `shouldEqual` (Pauseable.End ("z" \ "z"))
  describe "puaseable lazy" do
    BooleanTest.test $ (Pauseable.lazy >>> Pauseable.runIntermediate id)
    let se = Pauseable.lazy >>> Pauseable.runIntermediate globals
    let evaluatesTo source result = (se source) `shouldEqual` (globals result)
    booleanTestSuite evaluatesTo
    it "steps" do
      let sample = (("x" \ "x") ! ("y" \ "y")) ! ("z" \ "z")
      let inte = Pauseable.getResult >>> shouldEqual
      let step1 = Pauseable.lazy sample
      inte step1 $ ("x" \ "x") ! ("y" \ "y")
      let step2 = Pauseable.next step1
      inte step2 $ ("y" \ "y")
      let step3 = Pauseable.next step2
      inte step3 $ ("y" \ "y") ! ("z" \ "z")
      let step4 = Pauseable.next step3
      inte step4 $ ("z" \ "z")
      let step5 = Pauseable.next step4
      step5 `shouldEqual` (Pauseable.End ("z" \ "z"))
      
testGlobals = describe "globals" do
  it "works" do
    (globals (ref "TRUE")) `shouldEqual` ("x" \ "y" \ "x")
    (globals (ref "u")) `shouldEqual` (ref "u")

globals :: Evaluate String Unit
globals ast = case ast of
  Reference "TRUE" _ → "x" \ "y" \ "x"
  Reference "FALSE" _ → "x" \ "y" \ "y"
  Reference "NOT" _ → "p" \ "p" ! "FALSE" ! "TRUE"
  Reference "AND" _ → "a" \ "b" \ "a" ! "b" ! "FALSE"
  Reference "OR" _ → "a" \ "b" \ "a" ! "TRUE" ! "b"
  _ → ast

stops :: Ast String Unit → Either (Ast String Unit) (Ast String Unit)
stops ast = case ast of
  Application (Reference "STOP" _) body _ → Left body
  _ → Right ast

nextInter (End result) = (End result)
nextInter (Intermediate result task) = task result

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