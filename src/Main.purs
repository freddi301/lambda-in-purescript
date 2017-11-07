module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Foldable (class Foldable)
import Data.Foldable as Foldable
import Data.Map as Map
import Data.Maybe as Maybe
import Data.Set as Set

data Ast d = Reference String d | Application (Ast d) (Ast d) d | Abstraction String (Ast d) d

instance functorAst :: Functor Ast where
  map f (Reference name d) = Reference name (f d)
  map f (Application left right d) = Application (map f left) (map f right) (f d)
  map f (Abstraction head body d) = Abstraction head (map f body) (f d)  

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

instance showAst :: Show (Ast d) where
  show (Reference name _) = name
  show (Application (Application leftLeft leftRight _) right _) = "(" <> show leftLeft <> " " <> show leftRight <> " " <> show right <> ")"
  show (Application left right _) = "(" <> show left <> " " <> show right <> ")"
  show (Abstraction head (Abstraction headRight bodyRight _) _) = "(" <> head <> " => " <> headRight <> " => " <> show bodyRight <> ")"
  show (Abstraction head body _) = "(" <> head <> " => " <> show body <> ")"

derive instance eqAst :: Eq (Ast Unit)

type Scope term = Map.Map String term

class (Show term, Eq term) <= Term term where
  evaluate :: { scope :: Scope term, term :: term } -> { scope :: Scope term, term :: term }

instance termAstUnit :: Term (Ast Unit) where
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

-- use this to check undeclared variables
checkFreeVariables :: { free :: Set.Set String, scope :: Set.Set String, term :: Ast Unit } -> Set.Set String
checkFreeVariables { free, scope, term } = case term of
  Reference name _ -> if Set.member name scope then free else Set.insert name free
  Application left right _ -> Set.union (checkFreeVariables { free, scope, term: left }) (checkFreeVariables { free, scope, term: right })
  Abstraction head body _ -> checkFreeVariables { free, scope: Set.insert head scope, term: body }

-- TODO: finish garbage collect (remove not used variables from scope)

type Garbage = Set.Set String
derive instance eqAstGarbage :: Eq (Ast (Set.Set String))
garbageCollect :: { scope :: Set.Set String, term :: Ast Unit } -> Ast Garbage
garbageCollect { scope, term } = case term of
  Reference name _ -> Reference name Set.empty
  Application left right _ -> Application (garbageCollect { scope, term: left }) (garbageCollect { scope, term: right }) Set.empty
  Abstraction head body _ ->
    let newScope = Set.insert head scope in
    let garbage = Set.difference newScope (checkFreeVariables { free: Set.empty, scope: Set.empty, term: body  }) in
    let cleanScope = Set.difference newScope garbage in
    Abstraction head (garbageCollect { scope: cleanScope, term: body }) garbage

-- TODO: test
removeGarbage :: Garbage -> Scope (Ast Garbage) -> Scope (Ast Garbage)
removeGarbage garbage scope = Foldable.foldl (flip Map.delete) scope garbage

-- TODO: finish all cases
instance termAstGarbage :: Term (Ast (Set.Set String)) where
  evaluate { scope, term } = case term of
    Reference name _ -> { scope, term: Maybe.fromMaybe (Reference "" Set.empty) (Map.lookup name scope) }
    Abstraction head body garbage -> { scope: removeGarbage garbage scope, term: Abstraction head body Set.empty }
    Application (Abstraction leftHead leftBody _) right@(Abstraction _ _ _) _ ->
      evaluate { scope: Map.insert leftHead right scope, term: leftBody }
    Application left@(Abstraction _ _ _) right _ ->
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