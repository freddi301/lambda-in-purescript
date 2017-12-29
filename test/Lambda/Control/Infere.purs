module Test.Lambda.Control.Infere where

import Data.Map as Map
import Data.Maybe as Maybe
import Lambda.Control.Infere (Constraint(..), infere)
import Lambda.Data.Ast ((\))
import Prelude (Unit, const, discard, ($), (>>=))
import Test.Spec (Spec, describe, it, pending)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Runner (RunnerEffects)

test :: ∀ e . Spec (RunnerEffects e) Unit
test = describe "infere" do
  let inf ast = infere { ast, nextType: 0, typScope: Map.empty, constraints: Map.empty }
  let testit ast typ constraints = let result = inf ast in (shouldEqual result.typ typ) >>= const (shouldEqual result.constraints constraints)
  it "x => x is 0 = 1 -> 1" do
    let result = inf ("x" \ "x")
    result.typ `shouldEqual` 0
    (Map.lookup result.typ result.constraints) `shouldEqual` (Maybe.Just $ [IsAbstraction 1 1])
  it "x => y is 0 = 1 -> 2" do
    let result = inf ("x" \ "y")
    result.typ `shouldEqual` 0
    (Map.lookup result.typ result.constraints) `shouldEqual` (Maybe.Just $ [IsAbstraction 1 2])
  it "x => y => x is 0 = 1 -> 3 -> 1" do
    let result = inf ("x" \ "y" \ "x")
    result.typ `shouldEqual` 0
    (Map.lookup result.typ result.constraints) `shouldEqual` (Maybe.Just $ [IsAbstraction 1 2])
    (Map.lookup 2 result.constraints) `shouldEqual` (Maybe.Just $ [IsAbstraction 3 1])
  it "x => y => y is 0 = 1 -> 3 -> 3" do
    let result = inf ("x" \ "y" \ "y")
    result.typ `shouldEqual` 0
    (Map.lookup result.typ result.constraints) `shouldEqual` (Maybe.Just $ [IsAbstraction 1 2])
    (Map.lookup 2 result.constraints) `shouldEqual` (Maybe.Just $ [IsAbstraction 3 3])
  pending "nested cases cases"
  pending "recursive cases"
