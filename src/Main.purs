module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
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

class ShowD d where showd :: d -> String -> String
instance showdUnit :: ShowD Unit where showd _ string  = string

instance showAst :: (ShowD d) => Show (Ast d) where
  show (Reference name d) = showd d name
  show (Application left right d) = showd d $ "(" <> show left <> " " <> show right <> ")"
  show (Abstraction head body d) = showd d $ "(" <> head <> " => " <> show body <> ")"

derive instance eqAst :: Eq (Ast Unit)

type Scope term = Map.Map String term

class (Show term, Eq term) <= Term term where
  evaluate :: { scope :: Scope term, term :: term } -> { scope :: Scope term, term :: term }

instance termAstUnit :: Term (Ast Unit) where
  evaluate { scope, term } = case term of
    Reference name _ -> evaluate { scope, term: Maybe.fromMaybe (Reference "" unit) (Map.lookup name scope) }
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
checkFreeVariables :: forall d . { free :: Set.Set String, scope :: Set.Set String, term :: Ast d } -> Set.Set String
checkFreeVariables { free, scope, term } = case term of
  Reference name _ -> if Set.member name scope then free else Set.insert name free
  Application left right _ -> Set.union (checkFreeVariables { free, scope, term: left }) (checkFreeVariables { free, scope, term: right })
  Abstraction head body _ -> checkFreeVariables { free, scope: Set.insert head scope, term: body }

removeGarbage :: forall d . Scope (Ast d) -> Ast d -> Scope (Ast d)
removeGarbage scope term =
  let used = checkFreeVariables { free: Set.empty, scope: Set.empty, term: term } in
  let isUsed key = Set.member key used in
  Map.filterKeys (isUsed) scope

-- TODO: symbolic evaluate

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log "hello"