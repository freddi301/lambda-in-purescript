module Lambda.Control.Other where

import Prelude
import Data.Map as Map
import Data.Maybe as Maybe

import Lambda.Data.Ast (Ast(..))

-- | `evaluate` evaluates naively a lambda term
-- | eager with scope
type Evaluated reference decoration = { scope :: Map.Map reference (Ast reference decoration), term :: Ast reference decoration }
evaluate :: forall reference decoration . Ord reference => Evaluated reference decoration -> Evaluated reference decoration
evaluate { scope, term } = case term of
  Reference name _ -> { scope, term: Maybe.fromMaybe term (Map.lookup name scope) }
  Abstraction head body decoration -> { scope, term: Abstraction head body decoration }
  Application (Abstraction leftHead leftBody _) right@(Abstraction _ _ _) _ ->
    evaluate { scope: Map.insert leftHead right scope, term: leftBody }
  Application left@(Abstraction _ _ _) right decoration ->
    let rightSide = (evaluate { scope, term: right }).term in
    evaluate { scope, term: Application left rightSide decoration }   
  Application left right@(Abstraction _ _ _) decoration ->
    let leftSide = evaluate { scope, term: left } in
    evaluate { scope: leftSide.scope, term: Application leftSide.term right decoration }
  Application left right decoration ->
    let leftSide = evaluate { scope, term: left } in
    evaluate { scope: leftSide.scope, term: Application leftSide.term right decoration }
