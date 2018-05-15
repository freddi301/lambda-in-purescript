module Lambda.Control.Infere where

import Data.Array as Array
import Data.Map as Map
import Data.Maybe as Maybe
import Data.Ord as Ord
import Lambda.Data.Ast (Ast(..))
import Prelude (class Eq, class Show, show, (+), (<>), map)
import Data.Record (unionMerge)

infere :: ∀ reference decoration . Ord.Ord reference ⇒
  { ast :: Ast reference { | decoration }, nextType :: Int, typScope :: Map.Map reference Int, constraints :: Constraints } →
  { typ :: Int, nextType :: Int, constraints :: Constraints, ast :: Ast reference { typ :: Int | decoration } }
infere { ast, nextType, typScope, constraints } = case ast of
  Reference name decoration → case Map.lookup name typScope of
    Maybe.Just typ → { typ, nextType, constraints, ast: Reference name (unionMerge decoration { typ }) }
    Maybe.Nothing → { typ: nextType, nextType: nextType + 1, constraints, ast: Reference name (unionMerge decoration { typ: nextType }) } -- | TODO: manage free variable case (pass arround typScope?)
  Abstraction head body headDecoration decoration →
    let thisAbsType = nextType in
    let thisAbsHeadType = nextType + 1 in
    let inferred = infere { ast: body, nextType: nextType + 2, typScope: Map.insert head thisAbsHeadType typScope, constraints } in
    let newConstraints = addConstraint thisAbsType (IsAbstraction thisAbsHeadType inferred.typ) inferred.constraints in
    { typ: thisAbsType, nextType: inferred.nextType, constraints: newConstraints, ast: Abstraction head inferred.ast (unionMerge decoration { typ: thisAbsHeadType }) (unionMerge decoration { typ: thisAbsType }) }
  Application (Abstraction leftHead leftBody _ _) right@(Abstraction _ _ _ _) _ →
    let inferred = infere { ast: right, nextType, typScope, constraints } in
    let newTypeScope = Map.insert leftHead inferred.typ typScope in
    infere { ast: leftBody, nextType: inferred.nextType, constraints: inferred.constraints, typScope: newTypeScope }
  Application left right decoration → 
    let inferredRigth = infere { ast: right, nextType, typScope, constraints } in
    let inferredLeft = infere { ast: left, nextType: inferredRigth.nextType + 1, typScope, constraints: inferredRigth.constraints } in
    let newConstraints = addConstraint inferredLeft.typ (IsAbstraction inferredRigth.typ inferredRigth.nextType) inferredLeft.constraints in
    { typ: inferredRigth.nextType, nextType: inferredLeft.nextType, constraints: newConstraints, ast: Application inferredLeft.ast inferredRigth.ast (unionMerge decoration { typ: inferredRigth.nextType }) }

-- | TODO: add typechecking (trace)
-- | TODO: add user definable constraints

data Constraint = IsAbstraction Int Int -- | IsEqual Int Int
instance showContraint :: Show Constraint where show (IsAbstraction head body) = show head <> " → " <> show body
derive instance eqConstraint :: Eq Constraint

type Constraints = Map.Map Int (Array Constraint)

addConstraint :: Int → Constraint → Constraints → Constraints
addConstraint typ constraint constraints =
  case Map.lookup typ constraints of
    Maybe.Nothing → Map.insert typ (Array.singleton constraint) constraints
    Maybe.Just cons → Map.insert typ (Array.cons constraint cons) constraints

showType :: Constraints → Int → String
showType constraints typ = case Map.lookup typ constraints of
  Maybe.Nothing → show typ
  Maybe.Just [cons] → case cons of
    IsAbstraction head body → "(" <> showType constraints head <> " → " <> showType constraints body <> ")"
  Maybe.Just _ → show typ