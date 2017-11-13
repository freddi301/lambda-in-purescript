module Test.Lambda.Control.Other where

import Prelude (Unit, discard)
import Test.Spec (Spec, describe, it, pending)
import Test.Spec.Runner (RunnerEffects)
import Test.Spec.Assertions (shouldEqual)

import Data.Map as Map

import Lambda.Data.Ast (Ast, (!), (\))
import Lambda.Control.Other (evaluate)

eval :: (Ast String Unit) -> { scope :: Map.Map String (Ast String Unit), term :: (Ast String Unit) }
eval term = evaluate { scope: Map.empty, term }

ev :: (Ast String Unit) -> (Ast String Unit)
ev term = (evaluate { scope: Map.empty, term }).term

test :: ∀ e . Spec (RunnerEffects e) Unit
test = describe "evaluate" do
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
    it "respects 'or' truth table" do
      ev (or_ ! true_ ! true_) `shouldEqual` true_
      ev (or_ ! true_ ! false_) `shouldEqual` true_
      ev (or_ ! false_ ! true_) `shouldEqual` true_
      ev (or_ ! false_ ! false_) `shouldEqual` false_
    it "works with nested" do
      ev (or_ ! (and_ ! true_ ! true_) ! (not_ ! true_)) `shouldEqual` true_
  pending "list"
  pending "church numerals"
  pending "Y combinator"
