module Test.Lambda.Control.Common where

import Lambda.Control.Evaluate (collectFreeReferences, reifyEvaluateEager, reifyEvaluateEagerSingleStep, reifyEvaluateLazy, reifyEvaluateLazySingleStep, reifyEvaluateSymbolic, ηConversion)
import Test.Spec (Spec, describe, it, pending)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Runner (RunnerEffects)
import Prelude (Unit, discard)
import Test.Lambda.Data.Common (u, y, z)
import Lambda.Data.Ast

test ∷ ∀ e . Spec (RunnerEffects e) Unit
test = describe "Common" do
  describe "U fixed point combinator" do
    it "works eager" do
      (reifyEvaluateEager u) `shouldEqual` u
      (reifyEvaluateEager (u ! ("sample" \ "t" \ "t"))) `shouldEqual` ("t" \ "t")
    it "works lazy" do
      (reifyEvaluateLazy u) `shouldEqual` u
      (reifyEvaluateLazy (u ! ("sample" \ "t" \ "t"))) `shouldEqual` ("t" \ "t")
    it "works symbolic" do
      (reifyEvaluateSymbolic 1 u) `shouldEqual` u
      (reifyEvaluateSymbolic 1 (u ! ("sample" \ "t" \ "t"))) `shouldEqual` ("t" \ "t")
  describe "Y fixed point combinator" do
    it "works eager" do
      (reifyEvaluateEager y) `shouldEqual` y
      -- TODO
    it "works lazy" do
      (reifyEvaluateLazy y) `shouldEqual` y
      (reifyEvaluateLazy (y ! ("sample" \ "t" \ "t"))) `shouldEqual` ("t" \ "t")
    it "works symbolic" do
      (reifyEvaluateSymbolic 1 y) `shouldEqual` y
      (reifyEvaluateSymbolic 1 (y ! ("sample" \ "t" \ "t"))) `shouldEqual` ("t" \ "t")
  describe "Z fixed point combinator" do
    it "works eager" do
      (reifyEvaluateEager z) `shouldEqual` z
      (reifyEvaluateEager (z ! ("sample" \ "t" \ "t"))) `shouldEqual` ("t" \ "t")
    it "works lazy" do
      (reifyEvaluateLazy z) `shouldEqual` z
      -- TODO
    it "works symbolic" do
      (reifyEvaluateSymbolic 1 z) `shouldEqual` z
      (reifyEvaluateSymbolic 1 (z ! ("sample" \ "t" \ "t"))) `shouldEqual` ("t" \ "t")
