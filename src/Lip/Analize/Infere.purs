module Lip.Analize.Infere where

import Data.Maybe
import Data.Array as Array
import Data.Map as Map
import Data.Ord as Ord
import Data.Record as Record
import Lip.Data.Ast (Ast(..))
import Prelude (class Eq, class Show, show, (+), (<>), (==))

infere :: ∀ reference decoration . Ord.Ord reference ⇒
  { ast :: Ast reference { | decoration }, nextType :: Int, typScope :: Map.Map reference Int, constraints :: Constraints } →
  { typ :: Int, nextType :: Int, constraints :: Constraints, ast :: Ast reference { typ :: Int | decoration } }
infere { ast, nextType, typScope, constraints } = case ast of
  Reference name decoration → case Map.lookup name typScope of
    Just typ → { typ, nextType, constraints, ast: Reference name (Record.unionMerge decoration { typ }) }
    Nothing → { typ: nextType, nextType: nextType + 1, constraints, ast: Reference name (Record.unionMerge decoration { typ: nextType }) } -- | TODO: manage free variable case (pass arround typScope?)
  Abstraction head body decoration →
    let thisAbsType = nextType in
    let thisAbsHeadType = nextType + 1 in
    let inferred = infere { ast: body, nextType: nextType + 2, typScope: Map.insert head thisAbsHeadType typScope, constraints } in
    let newConstraints = addConstraint thisAbsType (IsAbstraction thisAbsHeadType inferred.typ) inferred.constraints in
    { typ: thisAbsType, nextType: inferred.nextType, constraints: newConstraints, ast: Abstraction head inferred.ast (Record.unionMerge decoration { typ: thisAbsType}) }
  Application (Abstraction leftHead leftBody _ ) right@(Abstraction _ _ _ ) _ →
    let inferred = infere { ast: right, nextType, typScope, constraints } in
    let newTypeScope = Map.insert leftHead inferred.typ typScope in
    infere { ast: leftBody, nextType: inferred.nextType, constraints: inferred.constraints, typScope: newTypeScope }
  Application left right decoration → 
    let inferredRigth = infere { ast: right, nextType, typScope, constraints } in
    let inferredLeft = infere { ast: left, nextType: inferredRigth.nextType + 1, typScope, constraints: inferredRigth.constraints } in
    let newConstraints = addConstraint inferredLeft.typ (IsAbstraction inferredRigth.typ inferredRigth.nextType) inferredLeft.constraints in
    { typ: inferredRigth.nextType, nextType: inferredLeft.nextType, constraints: newConstraints, ast: Application inferredLeft.ast inferredRigth.ast (Record.unionMerge decoration { typ: inferredRigth.nextType }) }

data Constraint = IsAbstraction Int Int
instance showContraint :: Show Constraint where show (IsAbstraction head body) = show head <> " → " <> show body
derive instance eqConstraint :: Eq Constraint

type Constraints = Map.Map Int (Array Constraint)

addConstraint :: Int → Constraint → Constraints → Constraints
addConstraint typ constraint constraints =
  case Map.lookup typ constraints of
    Nothing → Map.insert typ (Array.singleton constraint) constraints
    Just cons → Map.insert typ (Array.cons constraint cons) constraints

showType :: Constraints → Int → String
showType constraints typ = case Map.lookup typ constraints of
  Nothing → show typ
  Just [cons] → case cons of
    IsAbstraction head body | head == typ → show head <> "*(" <> show head <> " → " <> showType constraints body <> ")"
    IsAbstraction head body → "(" <> showType constraints head <> " → " <> showType constraints body <> ")"
  Just _ → show typ <> "#"