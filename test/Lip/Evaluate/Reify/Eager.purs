module Test.Lip.Evaluate.Reify.Eager where

import Lip.Evaluate.Reify.Eager
import Prelude
import Lip.Data.Ast.Helpers ((\), (!))
import Lip.Evaluate (Evaluate)
import Prelude (Unit, unit, discard, (>>>))
import Test.Lip.Evaluate.Boolean as BooleanTest
import Test.Spec (Spec, describe, it, pending)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Runner (RunnerEffects)

test :: ∀ e . Spec (RunnerEffects e) Unit
test = describe "reify eager" do
  describe "evaluate" $ BooleanTest.test evaluate
  describe "enhance" $ BooleanTest.test $ enhance id