module Test.Main where

import Main

import Control.Monad.Eff (Eff)
import Data.Map as Map
import Data.Set as Set
import Prelude (Unit, discard, show, unit, (==), ($))
import Test.Spec (describe, it, pending)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (RunnerEffects, run)

eval :: (Ast Unit) -> { scope :: Scope (Ast Unit), term :: (Ast Unit) }
eval term = evaluate { scope: Map.empty, term }

ev :: (Ast Unit) -> (Ast Unit)
ev term = (evaluate { scope: Map.empty, term }).term

main :: Eff (RunnerEffects ()) Unit
main = run [consoleReporter] do
  describe "show Ast" do
    it "works" do
      let action = show (Abstraction "a" (Application (Reference "a" unit) (Reference "a" unit) unit) unit)
      let result = "(a => (a a))"
      action `shouldEqual` result
      shouldEqual (show ("x" \ "y" \ "x")) "(x => (y => x))"
      shouldEqual (show ("x" \ ("x" ! "x" ! "x"))) "(x => ((x x) x))"
  describe "eq Ast" do
    it "works" do
      ((Reference "a" unit) == (Reference "a" unit)) `shouldEqual` true
      ((Reference "a" unit) == (Reference "b" unit)) `shouldEqual` false
      let true_ = "x" \ "y" \ "x"
      let false_ = "x" \ "y" \ "y"
      (true_ == false_) `shouldEqual` false
  describe "collectFreeReferences" do
    it "works" do
      let check scope term expected = shouldEqual (collectFreeReferences { scope: Set.fromFoldable scope, free: Set.empty, term }) (Set.fromFoldable expected)
      check [] (Reference "x" unit) ["x"]
      check ["x"] (Reference "x" unit) []
      check [] ("x" \ "y" \ "c") ["c"]
      check [] ("x" \ "y" \ ("c" ! "x")) ["c"]
      let false_ = "x" \ "y" \ "y"
      check [] false_ []
  describe "evaluate" do
    it "identity" do
      (eval (("x" \ "x") ! ("y" \ "y"))).term `shouldEqual` ("y" \ "y")
    describe "booleans" do
      let true_ = "x" \ "y" \ "x"
      let false_ = "x" \ "y" \ "y"
      let not_ = "p" \ "p" ! false_ ! true_
      let and_ = "a" \ "b" \ "a" ! "b" ! false_
      let or_ = "a" \ "b" \ "a" ! true_ ! "b"
      it "respects 'not' truth table" do
        (eval (not_ ! true_)).term `shouldEqual` false_
        (eval (not_ ! false_)).term `shouldEqual` true_
      it "respects 'and' truth table" do
        ev (and_ ! true_ ! true_) `shouldEqual` true_
        ev (and_ ! true_ ! false_) `shouldEqual` false_
        ev (and_ ! false_ ! true_) `shouldEqual` false_
        ev (and_ ! false_ ! false_) `shouldEqual` false_
      pending "or truth table"
      pending "nested example"
    pending "list"
    pending "church numerals"
    pending "Y combinator"
  describe "garbage collection" do
    it "remove garbage works" do
      let false_ = "x" \ "y" \ "y"
      shouldEqual (removeGarbage Map.empty false_) Map.empty
      shouldEqual (removeGarbage (Map.singleton "y" false_) false_) Map.empty
    it "works" do
      let true_ = "x" \ "y" \ "x"
      let false_ = "x" \ "y" \ "y"
      let not_ = "p" \ "p" ! false_ ! true_
      let and_ = "a" \ "b" \ "a" ! "b" ! false_
      let or_ = "a" \ "b" \ "a" ! true_ ! "b"
      let result = eval (and_ ! false_ ! true_)
      shouldEqual result.term false_
      shouldEqual (removeGarbage result.scope result.term) Map.empty
  describe "addCapturedReferencesDecorator" do
    it "works" do
      shouldEqual (addCapturedReferencesDecorator (Reference "a" unit)) (Reference "a" (Set.singleton "a"))
      shouldEqual (addCapturedReferencesDecorator ("x" \ "x")) (Abstraction "x" (Reference "x" (Set.singleton "x")) Set.empty)
      shouldEqual (addCapturedReferencesDecorator ("x" \ "y" \ "y")) (Abstraction "x" (Abstraction "y" (Reference "y" (Set.singleton "y")) Set.empty) Set.empty)
  describe "evaluate captured references ast" do
    let acrd = addCapturedReferencesDecorator
    let true_ = "x" \ "y" \ "x"
    let false_ = "x" \ "y" \ "y"
    let not_ = "p" \ "p" ! false_ ! true_
    let and_ = "a" \ "b" \ "a" ! "b" ! false_
    let or_ = "a" \ "b" \ "a" ! true_ ! "b"
    let falseAndTrue = and_ ! false_ ! true_
    let result = evaluate { scope: Map.empty, term: acrd falseAndTrue }
    it "works" do      
      shouldEqual result.term (acrd false_)
      -- shouldEqual result.scope Map.empty
  describe "reifyEvaluate" do
    let re = reifyEvaluate
    it "identity" do
      (re (("x" \ "x") ! ("y" \ "y"))) `shouldEqual` ("y" \ "y")
    describe "booleans" do
      let true_ = "x" \ "y" \ "x"
      let false_ = "x" \ "y" \ "y"
      let not_ = "p" \ "p" ! false_ ! true_
      let and_ = "a" \ "b" \ "a" ! "b" ! false_
      let or_ = "a" \ "b" \ "a" ! true_ ! "b"
      it "respects 'not' truth table" do
        (re (not_ ! true_)) `shouldEqual` false_
        (re (not_ ! false_)) `shouldEqual` true_
      it "respects 'and' truth table" do
        re (and_ ! true_ ! true_) `shouldEqual` true_
        re (and_ ! true_ ! false_) `shouldEqual` false_
        re (and_ ! false_ ! true_) `shouldEqual` false_
        re (and_ ! false_ ! false_) `shouldEqual` false_
      it "respects 'or' truth table" do
        re (or_ ! true_ ! true_) `shouldEqual` true_
        re (or_ ! true_ ! false_) `shouldEqual` true_
        re (or_ ! false_ ! true_) `shouldEqual` true_
        re (or_ ! false_ ! false_) `shouldEqual` false_
      it "works with nested" do
        re (or_ ! (and_ ! true_ ! true_) ! (not_ ! true_)) `shouldEqual` true_
    pending "list"
    pending "church numerals"
    pending "Y combinator"