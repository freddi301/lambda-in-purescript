module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Test.Spec (pending, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (RunnerEffects, run)

import Main

test = do

  describe "show Ast" do
    it "works" do
      let action = show (Abstraction "a" (Application (Reference "a") (Reference "a")))
      let result = "(a => (a a))"
      action `shouldEqual` result
  describe "eq Ast" do
    it "works" do
      ((Reference "a") == (Reference "a")) `shouldEqual` true
      ((Reference "a") == (Reference "b")) `shouldEqual` false

main :: Eff (RunnerEffects ()) Unit
main = run [consoleReporter] test