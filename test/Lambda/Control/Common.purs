module Test.Lambda.Control.Common where

import Lambda.Control.Evaluate (collectFreeReferences, reifyEvaluateEager, reifyEvaluateEagerSingleStep, reifyEvaluateLazy, reifyEvaluateLazySingleStep, reifyEvaluateSymbolic, ηConversion)
import Test.Spec (Spec, describe, it, pending)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Runner (RunnerEffects)
import Prelude (Unit, discard)
import Test.Lambda.Data.Common (u, y, z)

test :: ∀ e . Spec (RunnerEffects e) Unit
test = describe "Common" do
  describe "U fixed point combinator" do
    it "works eager" do
      (reifyEvaluateEager u) `shouldEqual` u
    it "works lazy" do
      (reifyEvaluateLazy u) `shouldEqual` u
    it "works symbolic" do
      (reifyEvaluateSymbolic 1 u) `shouldEqual` u
  describe "Y fixed point combinator" do
    it "works eager" do
      (reifyEvaluateEager y) `shouldEqual` y
    it "works lazy" do
      (reifyEvaluateLazy y) `shouldEqual` y
    -- it "works symbolic" do -- TODO
    --   (reifyEvaluateSymbolic 1 y) `shouldEqual` y
  describe "Z fixed point combinator" do
    it "works eager" do
      (reifyEvaluateEager z) `shouldEqual` z
    it "works lazy" do
      (reifyEvaluateLazy z) `shouldEqual` z
    -- it "works symbolic" do -- TODO
    --   (reifyEvaluateSymbolic 1 z) `shouldEqual` y
