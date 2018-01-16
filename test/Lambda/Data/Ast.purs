module Test.Lambda.Data.Ast where

import Prelude (Unit, discard, show, (==), unit)
import Data.Map as Map
import Test.Spec (Spec, describe, it)
import Test.Spec.Runner (RunnerEffects)
import Test.Spec.Assertions (shouldEqual)

import Lambda.Data.Ast

test :: ∀ e . Spec (RunnerEffects e) Unit
test = describe "Ast" do
  describe "show" do
    it "works" do
      -- shouldEqual (show ("x" \ "y" \ "x")) "unit(\"x\" => unit(\"y\" => unit\"x\"))"
      -- shouldEqual (show ("x" \ ("x" ! "x" ! "x"))) "unit(\"x\" => unit(unit(unit\"x\" unit\"x\") unit\"x\"))"
      shouldEqual (show ("x" \ "y" \ "x")) "(\"x\" => (\"y\" => \"x\"))"
      shouldEqual (show ("x" \ ("x" ! "x" ! "x"))) "(\"x\" => ((\"x\" \"x\") \"x\"))"
  describe "prettyPrint" do
    it "works" do
      shouldEqual (prettyPrint ("x" \ "y" \ "x")) "(x => y => x)"
      shouldEqual (prettyPrint ("x" \ ("x" ! "x" ! "x"))) "(x => x x x)"
  describe "eq" do
    it "works" do
      ((Reference "a" unit) == (Reference "a" unit)) `shouldEqual` true
      ((Reference "a" unit) == (Reference "b" unit)) `shouldEqual` false
      let true_ = "x" \ "y" \ "x"
      let false_ = "x" \ "y" \ "y"
      (true_ == false_) `shouldEqual` false
  describe "αConversion" do
    let f = \ast -> (αConversion { ast, map: Map.empty, symbol: 0 }).ast
    describe "gives comparable results for:" do
      it "x => x and y => y" do
        (f ("x" \ "x")) `shouldEqual` (f ("y" \ "y"))
        (f ("x" \ "x")) `shouldEqual` (Abstraction 0 (Reference 0 unit) unit unit)
      it "a => b => a and c => d => c" do
        (f ("a" \ "b" \ "a")) `shouldEqual` (f ("c" \ "d" \ "c"))
        (f ("a" \ "b" \ "a")) `shouldEqual` (Abstraction 0 (Abstraction 1 (Reference 0 unit) unit unit) unit unit)
