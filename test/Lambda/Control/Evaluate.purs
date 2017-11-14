module Test.Lambda.Control.Evaluate where

import Data.Set as Set
import Lambda.Control.Evaluate (collectFreeReferences, reifyEvaluate, reifyEvaluateSymbolic)
import Lambda.Data.Ast (Ast(..), (!), (\))
import Lambda.Parser.Parser (parseProgram)
import Prelude (Unit, unit, discard, (>>>))
import Test.Lambda.Sources.Booleanic (booleanic, booleanicMCSE)
import Test.Spec (Spec, describe, it, pending)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Runner (RunnerEffects)

test :: ∀ e . Spec (RunnerEffects e) Unit
test = describe "Evaluate" do
  describe "collectFreeReferences" do
    it "works" do
      let check scope term expected = shouldEqual (collectFreeReferences { scope: Set.fromFoldable scope, free: Set.empty, term }) (Set.fromFoldable expected)
      check [] (Reference "x" unit) ["x"]
      check ["x"] (Reference "x" unit) []
      check [] ("x" \ "y" \ "c") ["c"]
      check [] ("x" \ "y" \ ("c" ! "x")) ["c"]
      let false_ = "x" \ "y" \ "y"
      check [] false_ []
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
    -- describe "program checks" do
    --   it "works for booleanic" do
    --     shouldEqual ((parseProgram >>> re) booleanic) ("a" \ "b" \ "a")
      -- it "works for Y fixed point" do
      --     shouldEqual ((parseProgram >>> re) combinatorY) ("a" \ "a")

  describe "reifyEvaluateSymbolic" do
    let re = reifyEvaluateSymbolic 0
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
    describe "symbolic evaluation" do
      describe "cases" do
        let identity = "x" \ "x"
        it "works for identity" do
          re (identity) `shouldEqual` identity
        let idenityInIdentity = "y" \ identity ! "y"
        it "works for idenityIdentity" do
          re (idenityInIdentity) `shouldEqual` ("y" \ "y")
          re ("y" \ ("x" \ "x") ! "y") `shouldEqual` ("y" \ "y")
          re ("x" \ ("x" \ "x") ! "x") `shouldEqual` ("x" \ "x")
        it "works for x => y => (x => x) x" do
          re ("x" \ "y" \ (("x" \ "x") ! "x")) `shouldEqual` ("x" \ "y" \ "x")
        it "works for x => y => (y => y) x" do
          re ("x" \ "y" \ (("y" \ "y") ! "x")) `shouldEqual` ("x" \ "y" \ "x")
        it "works for x => y => (z => z) x" do
          re ("x" \ "y" \ (("z" \ "z") ! "x")) `shouldEqual` ("x" \ "y" \ "x")
        it "works for x => y => (x => x) y" do
          re ("x" \ "y" \ (("x" \ "x") ! "y")) `shouldEqual` ("x" \ "y" \ "y")
        it "works for x => y => (y => x) y" do
          re ("x" \ "y" \ (("y" \ "y") ! "y")) `shouldEqual` ("x" \ "y" \ "y")
        it "works for x => y => (z => z) y" do
          re ("x" \ "y" \ (("z" \ "z") ! "y")) `shouldEqual` ("x" \ "y" \ "y")
        it "works for x => y => x ((z => z) y)" do
          re ("x" \ "y" \ ("x" ! (("z" \ "z") ! "y"))) `shouldEqual` ("x" \ "y" \ ("x" ! "y"))
        it "works for x => y => x (z => z) y" do
          re ("x" \ "y" \ (("x" ! ("z" \ "z")) ! "y")) `shouldEqual` ("x" \ "y" \ (("x" ! ("z" \ "z")) ! "y"))
        it "works for x => y => x ((z => w => z) y)" do
          re ("x" \ "y" \ ("x" ! (("z" \ "w" \ "z") ! "y"))) `shouldEqual` ("x" \ "y" \ ("x" ! ("w" \ "y")))
        -- it "works for x => f x" do -- | TODO: η-conversion
        --   re ("x" \ "f" ! "x") `shouldEqual` (Reference "f" unit)
      describe "program checks" do
        it "works for symbolic execution from sub-block" do
          shouldEqual ((parseProgram >>> re) "main x y = a x\n  a z = z") ("x" \ "y" \ "x")
        it "works for booleanic" do
          shouldEqual ((parseProgram >>> re) booleanic) ("a" \ "b" \ "a")
          shouldEqual ((parseProgram >>> re) booleanicMCSE) ("a" \ "b" \ "a")
        -- it "works for Y fixed point" do
        --   shouldEqual ((parseProgram >>> re) combinatorY) ("a" \ "a")
