module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Array as Array
import Data.Foldable as Foldable
import Data.Map as Map
import Data.Maybe as Maybe
import Data.Set as Set

data Ast d = Reference String d | Application (Ast d) (Ast d) d | Abstraction String (Ast d) d

reify :: forall k d . String -> Ast d -> Ast d -> Ast d
reify ref value term = case term of
  Reference name d -> if ref == name then value else term
  Abstraction head body d -> if ref == head then term else Abstraction head (reify ref value body) d
  Application left right d -> Application (reify ref value left) (reify ref value right) d

reifyEvaluate :: forall d . Ast d -> Ast d
reifyEvaluate term = case term of
  Application (Abstraction head body _) right@(Abstraction _ _ _) _ -> reifyEvaluate $ reify head right body
  Application left right d -> reifyEvaluate $ Application (reifyEvaluate left) (reifyEvaluate right) d
  _ -> term

reifyEvaluateSymbolic :: forall d . Int -> Ast d -> Ast d
reifyEvaluateSymbolic nextSymbol term = let rec = reifyEvaluateSymbolic in case term of
  Application (Abstraction head body _) right@(Abstraction _ _ _) _ -> rec nextSymbol $ reify head right body
  Application (Abstraction head body _) right _ -> rec nextSymbol $ reify head right body
  Application (Reference _ _) (Reference _ _) _ -> term
  Application left right@(Reference _ _) d -> Application (rec nextSymbol left) right d
  Application left@(Reference _ _) right d -> Application left (rec nextSymbol right) d
  Application left right d -> rec nextSymbol $ Application (rec nextSymbol left) (rec nextSymbol right) d
  Abstraction head body d ->
    let symbolicHead = head <> "#" <> show nextSymbol in
    let symbolicBody = reify head (Reference symbolicHead d) body in
    let computedSymbolicBody = rec (nextSymbol + 1) symbolicBody in
    let concreteBody = reify symbolicHead (Reference head d) computedSymbolicBody in
    Abstraction head concreteBody d
  Reference _ _ -> term

-- Lifting Section

-- operators for friendlier construction of the ast
-- λx.x x
-- "x" \ "x" ! "x"
ref :: String -> Ast Unit
ref name = Reference name unit
app :: Ast Unit -> Ast Unit -> Ast Unit
app left right = Application left right unit
abs :: String -> Ast Unit -> Ast Unit
abs head body = Abstraction head body unit
class ToAstAbs body where toAstAbs :: String -> body -> Ast Unit
instance toAstAbsString :: ToAstAbs String where toAstAbs head body = abs head (ref body)
instance toAstAbsAst :: ToAstAbs (Ast Unit) where toAstAbs head body = abs head body
class ToAstApp left right where toAstApp :: left -> right -> Ast Unit
instance toAstAppStringString :: ToAstApp String String where toAstApp left right = app (ref left) (ref right)
instance toAstAppStringAst :: ToAstApp String (Ast Unit) where toAstApp left right = app (ref left) right
instance toAstAppAstString :: ToAstApp (Ast Unit) String where toAstApp left right = app left (ref right)
instance toAstAppAstAst :: ToAstApp (Ast Unit) (Ast Unit) where toAstApp left right = app left right
infixr 8 toAstAbs as \
infixl 9 toAstApp as !

instance showAst :: (ShowD d) => Show (Ast d) where
  show (Reference name d) = showd d name
  show (Application left right d) = showd d $ "(" <> show left <> " " <> show right <> ")"
  show (Abstraction head body d) = showd d $ "(" <> head <> " => " <> show body <> ")"

class ShowD d where showd :: d -> String -> String

instance showdUnit :: ShowD Unit where showd _ string  = string
derive instance eqAst :: Eq (Ast Unit)

-- Other Stuff Section

instance functorAst :: Functor Ast where
  map f (Reference name d) = Reference name (f d)
  map f (Application left right d) = Application (map f left) (map f right) (f d)
  map f (Abstraction head body d) = Abstraction head (map f body) (f d)

getD :: forall d. Ast d -> d
getD (Reference _ d) = d
getD (Application _ _ d) = d
getD (Abstraction _ _ d) = d

type Scope term = Map.Map String term

class Evaluable term where
  evaluate :: { scope :: Scope term, term :: term } -> { scope :: Scope term, term :: term }

instance evaluableAstUnit :: Evaluable (Ast Unit) where
  evaluate { scope, term } = case term of
    Reference name _ -> { scope, term: Maybe.fromMaybe (Reference "" unit) (Map.lookup name scope) }
    Abstraction head body _ -> { scope, term: Abstraction head body unit } -- symbolic execution here
    Application (Abstraction leftHead leftBody _) right@(Abstraction _ _ _) _ ->
      evaluate { scope: Map.insert leftHead right scope, term: leftBody }
    Application left@(Abstraction _ _ _) right _ ->
      let rightSide = (evaluate { scope, term: right }).term in
      evaluate { scope, term: Application left rightSide unit }   
    Application left right@(Abstraction _ _ _) _ ->
      let leftSide = evaluate { scope, term: left } in
      evaluate { scope: leftSide.scope, term: Application leftSide.term right unit }
    Application left right _ ->
      let leftSide = evaluate { scope, term: left } in
      evaluate { scope: leftSide.scope, term: Application leftSide.term right unit }

type CapturedReferences = Set.Set String
instance showdSetString :: ShowD (Set.Set String) where showd set string = "[" <> show (Foldable.foldr (Array.cons) [] set) <> string <> "]"
derive instance eqAstSetString :: Eq (Ast (Set.Set String))
-- use this to check undeclared variables
collectFreeReferences :: forall d . { free :: CapturedReferences, scope :: Set.Set String, term :: Ast d } -> CapturedReferences
collectFreeReferences { free, scope, term } = case term of
  Reference name _ -> if Set.member name scope then free else Set.insert name free
  Application left right _ -> Set.union (collectFreeReferences { free, scope, term: left }) (collectFreeReferences { free, scope, term: right })
  Abstraction head body _ -> collectFreeReferences { free, scope: Set.insert head scope, term: body }

removeGarbage :: forall d . Scope (Ast d) -> Ast d -> Scope (Ast d)
removeGarbage scope term =
  let used = collectFreeReferences { free: Set.empty, scope: Set.empty, term: term } in
  let isUsed key = Set.member key used in
  Map.filterKeys (isUsed) scope

addCapturedReferencesDecorator :: Ast Unit -> Ast CapturedReferences
addCapturedReferencesDecorator ast =
  let getFreeReferences term = collectFreeReferences { free: Set.empty, scope: Set.empty, term: term } in
  let rec = addCapturedReferencesDecorator in
  case ast of
    Reference name _ -> Reference name (getFreeReferences ast)
    Abstraction head body _ -> Abstraction head (rec body) (getFreeReferences ast)
    Application left right _ -> Application (rec left) (rec right) (getFreeReferences ast)

carryScope :: forall d . CapturedReferences -> Scope (Ast d) -> Scope (Ast d)
carryScope references scope = Map.filterKeys ((flip Set.member) references) scope

instance evaluableAstCapturedReferences :: Evaluable (Ast (Set.Set String)) where
  evaluate { scope, term } = case term of
    Reference name _ -> { scope, term: Maybe.fromMaybe (Reference "" Set.empty) (Map.lookup name scope) }
    Abstraction _ _ refs -> { scope: carryScope refs scope, term } -- TODO: symbolic execution here
    Application (Abstraction leftHead leftBody _) right@(Abstraction _ _ _) _ ->      
      evaluate { scope: Map.insert leftHead right scope, term: leftBody }
    Application left@(Abstraction _ _ _) right _ ->
        -- TODO: do not interpret right side if not used in left (not in leftRefs)
      let rightSide = (evaluate { scope, term: right }).term in
      evaluate { scope, term: Application left rightSide Set.empty }   
    Application left right@(Abstraction _ _ _) _ ->
      let leftSide = evaluate { scope, term: left } in
      evaluate { scope: leftSide.scope, term: Application leftSide.term right Set.empty }
    Application left right _ ->
      let leftSide = evaluate { scope, term: left } in
      evaluate { scope: leftSide.scope, term: Application leftSide.term right Set.empty }

-- TODO: symbolic evaluate

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log "hello"