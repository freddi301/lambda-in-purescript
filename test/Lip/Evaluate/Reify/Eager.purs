module Test.Lip.Evaluate.Reify.Eager where

import Prelude

import Control.Monad.Aff (Aff)
import Data.Either (Either(..), either)
import Lip.Data.Ast (Ast(..))
import Lip.Data.Ast.Helpers ((\), (!), ref)
import Lip.Data.Ast.Reference as MapReference
import Lip.Evaluate (Evaluate)
import Lip.Evaluate.Pauseable as Pauseable
import Lip.Evaluate.Reify.Eager (Intermediate(..), enhance, evaluate, getResult, intermediate, runIntermediate, runStep, step, stepEnhance, stop)
import Lip.Evaluate.Reify.Pauseable as ReifyPauseable
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
    BooleanTest.test $ (ReifyPauseable.eager >>> Pauseable.runIntermediate id)
    let se = ReifyPauseable.eager >>> Pauseable.runIntermediate globals
    let evaluatesTo source result = (se source) `shouldEqual` (globals result)
    booleanTestSuite evaluatesTo
    it "steps" do
      let sample = (("x" \ "x") ! ("y" \ "y")) ! ("z" \ "z")
      let inte = Pauseable.getResult >>> shouldEqual
      let step1 = ReifyPauseable.eager sample
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
    BooleanTest.test $ (ReifyPauseable.lazy >>> Pauseable.runIntermediate id)
    let se = ReifyPauseable.lazy >>> Pauseable.runIntermediate globals
    let evaluatesTo source result = (se source) `shouldEqual` (globals result)
    booleanTestSuite evaluatesTo
    it "steps" do
      let sample = (("x" \ "x") ! ("y" \ "y")) ! ("z" \ "z")
      let inte = Pauseable.getResult >>> shouldEqual
      let step1 = ReifyPauseable.lazy sample
      inte step1 $ ("x" \ "x") ! ("y" \ "y")
      let step2 = Pauseable.next step1
      inte step2 $ ("y" \ "y")
      let step3 = Pauseable.next step2
      inte step3 $ ("y" \ "y") ! ("z" \ "z")
      let step4 = Pauseable.next step3
      inte step4 $ ("z" \ "z")
      let step5 = Pauseable.next step4
      step5 `shouldEqual` (Pauseable.End ("z" \ "z"))
  describe "puaseable symbolic" do
    let lift = MapReference.map (\name -> (ReifyPauseable.Symbol name 0))
    let symbolic = ReifyPauseable.symbolic 0
    let unlift = MapReference.map (\ref -> case ref of (ReifyPauseable.Symbol name _) -> name)
    BooleanTest.test (lift >>> symbolic >>> (Pauseable.runIntermediate id) >>> unlift)
    let se = lift >>> symbolic >>> (Pauseable.runIntermediate id) >>> unlift
    let evaluatesTo source result = (se source) `shouldEqual` (globals result)
    pending "booleanTestSuite evaluatesTo"

testGlobals :: ∀ e . Spec (RunnerEffects e) Unit     
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

nextInter :: ∀ result . Intermediate result -> Intermediate result
nextInter (End result) = (End result)
nextInter (Intermediate result task) = task result

booleanTestSuite :: ∀ e . (Ast String Unit -> Ast String Unit -> Aff (RunnerEffects e) Unit) -> Spec (RunnerEffects e) Unit   
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