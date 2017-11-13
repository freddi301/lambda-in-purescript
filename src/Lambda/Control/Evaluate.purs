module Lambda.Control.Evaluate where

import Prelude (class Eq, class Ord, ($), (==), (+))
import Data.Set as Set

import Lambda.Data.Ast (Ast(..), mapReference)

-- | `reify` takes a reference and substitutes every occurrence of it respecting lexical scoping rules
-- | it corresponds to β-reduction
-- | `ref` the reference
-- | `value` the value being copied in the ast
-- | `term` the ast where the value will be substituted
reify :: ∀ reference decoration . Eq reference => reference -> Ast reference decoration -> Ast reference decoration -> Ast reference decoration
reify ref value term = case term of
  Reference name decoration -> if ref == name then value else term
  Abstraction head body decoration -> if ref == head then term else Abstraction head (reify ref value body) decoration
  Application left right decoration -> Application (reify ref value left) (reify ref value right) decoration

-- | `reifyEvaluate` evaluates a lambda term using the reify mechanism
-- | the execution is eager
-- | there is no scope, as soon variable gets bound, every occurrence is substituted with its value
reifyEvaluate :: ∀ reference decoration . Eq reference => Ast reference decoration -> Ast reference decoration
reifyEvaluate term = case term of
  Application (Abstraction head body _) right@(Abstraction _ _ _) _ -> reifyEvaluate $ reify head right body
  Application left right decoration -> reifyEvaluate $ Application (reifyEvaluate left) (reifyEvaluate right) decoration
  _ -> term

-- | `reifyEvaluateSymbolic` same as `reifyEvaluate` enhanced with symbolic execution
-- | obtained by recursive α-conversion in abstraction bodies
-- | https://en.wikipedia.org/wiki/Symbolic_execution
reifyEvaluateSymbolic :: ∀ reference decoration . Eq reference =>
  Int -> Ast reference decoration -> Ast reference decoration
reifyEvaluateSymbolic nextSymbol term = let rec = reifyEvaluateSymbolic in case term of
  Application (Abstraction head body _) right@(Abstraction _ _ _) _ -> rec nextSymbol $ reify head right body
  Application (Abstraction head body _) right _ -> rec nextSymbol $ reify head right body
  Application (Reference _ _) (Reference _ _) _ -> term
  Application left right@(Reference _ _) decoration -> Application (rec nextSymbol left) right decoration
  Application left@(Reference _ _) right decoration -> Application left (rec nextSymbol right) decoration
  Application left right decoration -> rec nextSymbol $ Application (rec nextSymbol left) (rec nextSymbol right) decoration
  Abstraction head body decoration ->
    let symbolicHead = Symbolic head nextSymbol in
    let symbolicBody = reify (Concrete head) (Reference symbolicHead decoration) (mapReference Concrete body) in
    let computedSymbolicBody = rec (nextSymbol + 1) symbolicBody in
    let computedConcreteBody = mapReference extractReference $ reify symbolicHead (Reference (Concrete head) decoration) computedSymbolicBody in
    Abstraction head computedConcreteBody decoration
  Reference _ _ -> term

data Symbol reference variation = Symbolic reference variation | Concrete reference
derive instance eqSymbol :: (Eq reference, Eq variation) => Eq (Symbol reference variation)
extractReference :: ∀ reference variation . Symbol reference variation -> reference
extractReference symbol = case symbol of
  Concrete reference -> reference
  Symbolic reference _ -> reference

-- | `collectFreeReferences` return the set of free variables in the ast 
-- | use this to check undeclared variables
-- | `free` list of free references
-- | `scope` current scope
-- | `term` the ast
collectFreeReferences :: ∀ reference decoration . Ord reference =>
  { free :: Set.Set reference, scope :: Set.Set reference, term :: Ast reference decoration } -> Set.Set reference
collectFreeReferences { free, scope, term } = case term of
  Reference name _ -> if Set.member name scope then free else Set.insert name free
  Application left right _ -> Set.union (collectFreeReferences { free, scope, term: left }) (collectFreeReferences { free, scope, term: right })
  Abstraction head body _ -> collectFreeReferences { free, scope: Set.insert head scope, term: body }

-- instance showSetReference :: (Show reference) => Show (Set.Set reference) where show = show <<< Foldable.foldr (Array.cons) []

-- | TODO: implement α-conversion with `de bruijn`` indices for α-equivalence
-- | toBrujin :: ∀ reference decoration . Ast reference decoration -> Ast Unit decoration

-- | TODO: implement η-conversion (eta-conversion)

-- | TODO: η-conversion + α-conversion + α-equivalence can be used to validate laws (example comonad laws)

-- | TODO: parallel execution

-- | TODO: distributed execution
