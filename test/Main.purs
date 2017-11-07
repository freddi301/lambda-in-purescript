module Test.Main where

import Main

import Control.Monad.Eff (Eff)
import Data.Map (empty)
import Prelude (Unit, discard, show, (==))
import Test.Spec (pending, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (RunnerEffects, run)

eval :: Ast -> { scope :: Scope Ast, term :: Ast }
eval term = evaluate { scope: empty, term }

ev :: Ast -> Ast
ev term = (evaluate { scope: empty, term }).term

main :: Eff (RunnerEffects ()) Unit
main = run [consoleReporter] do
  describe "show Ast" do
    it "works" do
      let action = show (Abstraction "a" (Application (Reference "a") (Reference "a")))
      let result = "(a => (a a))"
      action `shouldEqual` result
      shouldEqual (show ("x" \ "y" \ "x")) "(x => y => x)"
      shouldEqual (show ("x" \ ("x" ! "x" ! "x"))) "(x => (x x x))"
  describe "eq Ast" do
    it "works" do
      ((Reference "a") == (Reference "a")) `shouldEqual` true
      ((Reference "a") == (Reference "b")) `shouldEqual` false
      let true_ = "x" \ "y" \ "x"
      let false_ = "x" \ "y" \ "y"
      (true_ == false_) `shouldEqual` false
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