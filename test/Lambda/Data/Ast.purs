module Test.Lambda.Data.Ast where

import Prelude (discard, show, unit, (==))
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)

import Lambda.Data.Ast

test = describe "Ast" do
  describe "show" do
    it "works" do
      shouldEqual (show ("x" \ "y" \ "x")) "unit(\"x\" => unit(\"y\" => unit\"x\"))"
      shouldEqual (show ("x" \ ("x" ! "x" ! "x"))) "unit(\"x\" => unit(unit(unit\"x\" unit\"x\") unit\"x\"))"
  describe "prettyPrint" do
    it "works" do
      shouldEqual (prettyPrint ("x" \ "y" \ "x")) "(x => y => x)"
      shouldEqual (prettyPrint ("x" \ ("x" ! "x" ! "x"))) "(x => (x x x))"
  describe "eq" do
    it "works" do
      ((Reference "a" unit) == (Reference "a" unit)) `shouldEqual` true
      ((Reference "a" unit) == (Reference "b" unit)) `shouldEqual` false
      let true_ = "x" \ "y" \ "x"
      let false_ = "x" \ "y" \ "y"
      (true_ == false_) `shouldEqual` false
