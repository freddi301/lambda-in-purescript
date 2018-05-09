module Test.Lip.Evaluate.Boolean where

import Lip.Evaluate (Evaluate)
import Lip.Data.Ast.Helpers ((\), (!))
import Prelude (Unit, discard)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Runner (RunnerEffects)


test :: ∀ e . Evaluate String Unit → Spec (RunnerEffects e) Unit
test evaluate = describe "curch booleans truth table" do
  let true_ = "x" \ "y" \ "x"
  let false_ = "x" \ "y" \ "y"
  let not_ = "p" \ "p" ! false_ ! true_
  let and_ = "a" \ "b" \ "a" ! "b" ! false_
  let or_ = "a" \ "b" \ "a" ! true_ ! "b"
  it "respects 'not' truth table" do
    (evaluate (not_ ! true_)) `shouldEqual` false_
    (evaluate (not_ ! false_)) `shouldEqual` true_
  it "respects 'and' truth table" do
    evaluate (and_ ! true_ ! true_) `shouldEqual` true_
    evaluate (and_ ! true_ ! false_) `shouldEqual` false_
    evaluate (and_ ! false_ ! true_) `shouldEqual` false_
    evaluate (and_ ! false_ ! false_) `shouldEqual` false_
  it "respects 'or' truth table" do
    evaluate (or_ ! true_ ! true_) `shouldEqual` true_
    evaluate (or_ ! true_ ! false_) `shouldEqual` true_
    evaluate (or_ ! false_ ! true_) `shouldEqual` true_
    evaluate (or_ ! false_ ! false_) `shouldEqual` false_
  it "works with nested" do
    evaluate (or_ ! (and_ ! true_ ! true_) ! (not_ ! true_)) `shouldEqual` true_