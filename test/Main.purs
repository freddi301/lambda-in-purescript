module Test.Main where

import Main

import Control.Monad.Eff (Eff)
import Data.Map (empty)
import Data.Set as Set
import Prelude (Unit, discard, show, unit, (=<<), (==))
import Test.Spec (describe, it, pending, pending')
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (RunnerEffects, run)

eval :: (Ast Unit) -> { scope :: Scope (Ast Unit), term :: (Ast Unit) }
eval term = evaluate { scope: empty, term }

ev :: (Ast Unit) -> (Ast Unit)
ev term = (evaluate { scope: empty, term }).term

main :: Eff (RunnerEffects ()) Unit
main = run [consoleReporter] do
  describe "show Ast" do
    it "works" do
      let action = show (Abstraction "a" (Application (Reference "a" unit) (Reference "a" unit) unit) unit)
      let result = "(a => (a a))"
      action `shouldEqual` result
      shouldEqual (show ("x" \ "y" \ "x")) "(x => y => x)"
      shouldEqual (show ("x" \ ("x" ! "x" ! "x"))) "(x => (x x x))"
  describe "eq Ast" do
    it "works" do
      ((Reference "a" unit) == (Reference "a" unit)) `shouldEqual` true
      ((Reference "a" unit) == (Reference "b" unit)) `shouldEqual` false
      let true_ = "x" \ "y" \ "x"
      let false_ = "x" \ "y" \ "y"
      (true_ == false_) `shouldEqual` false
  describe "checkFreeVariables" do
    it "works" do
      let check scope term expected = shouldEqual (checkFreeVariables { scope: Set.fromFoldable scope, free: Set.empty, term }) (Set.fromFoldable expected)
      check [] (Reference "x" unit) ["x"]
      check ["x"] (Reference "x" unit) []
      check [] ("x" \ "y" \ "c") ["c"]
      check [] ("x" \ "y" \ ("c" ! "x")) ["c"]
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
    let gref d name = Reference name (Set.fromFoldable d)
    let gabs d head body = Abstraction head body (Set.fromFoldable d)
    -- let gapp d left right = Application left right (Set.fromFoldable d)
    describe "collect garbage" do
      it "works" do
        let check naked decorated = shouldEqual (garbageCollect { scope: Set.empty, term: naked }) decorated
        let true_ = "x" \ "y" \ "x"
        let true_garbage = gabs [] "x" (gabs ["y"] "y" (gref [] "x"))
        check true_ true_garbage
        let false_ = "x" \ "y" \ "y"
        let false_garbage = gabs ["x"] "x" (gabs [] "y" (gref [] "y"))
        check false_ false_garbage
      pending "more examples"
    -- describe "evaluate with garbage" do
    --   it "works" do
    --     let true_ = "x" \ "y" \ "x"
    --     let false_ = "x" \ "y" \ "y"
    --     let not_ = "p" \ "p" ! false_ ! true_
    --     let false_garbage = gabs ["x"] "x" (gabs [] "y" (gref [] "y"))
    --     let result = (evaluate { scope: empty, term: (garbageCollect { scope: Set.empty, term: (not_ ! true_) }) })
    --     let expected = { scope: empty, term: false_garbage }
    --     shouldEqual result.term expected.term -- TODO: maintain garbage info
    --     shouldEqual result.scope expected.scope
      pending "more samples"