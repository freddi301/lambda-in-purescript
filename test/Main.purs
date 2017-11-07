module Test.Main where

import Main

import Control.Monad.Eff (Eff)
import Data.Map (empty)
import Prelude (Unit, discard, show, (==), ($))
import Test.Spec (pending, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (RunnerEffects, run)

eval :: Ast -> { scope :: Scope Ast, term :: Ast }
eval term = evaluate { scope: empty, term } 

main :: Eff (RunnerEffects ()) Unit
main = run [consoleReporter] do
  describe "show Ast" do
    it "works" do
      let action = show (Abstraction "a" (Application (Reference "a") (Reference "a")))
      let result = "(a => (a a))"
      action `shouldEqual` result
  describe "eq Ast" do
    it "works" do
      ((Reference "a") == (Reference "a")) `shouldEqual` true
      ((Reference "a") == (Reference "b")) `shouldEqual` false
  describe "evaluate" do
    it "evaluates identity" do
      (eval $ "x" \ r"x" ! "y" \ r"y").term `shouldEqual` ("y" \ r"y")
    pending "more evaluations"