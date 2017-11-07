module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Map as Map
import Data.Maybe as Maybe
import Data.Set as Set

data Ast = Reference String | Application Ast Ast | Abstraction String Ast

-- operators for friendlier construction of the ast
-- λx.x x
-- "x" \ "x" ! "x"
class ToAstAbs body where toAstAbs :: String -> body -> Ast
instance toAstAbsString :: ToAstAbs String where toAstAbs head body = Abstraction head (Reference body) 
instance toAstAbsAst :: ToAstAbs Ast where toAstAbs = Abstraction
class ToAstApp left right where toAstApp :: left -> right -> Ast
instance toAstAppStringString :: ToAstApp String String where toAstApp left right = Application (Reference left) (Reference right)
instance toAstAppStringAst :: ToAstApp String Ast where toAstApp left right = Application (Reference left) right
instance toAstAppAstString :: ToAstApp Ast String where toAstApp left right = Application left (Reference right)
instance toAstAppAstAst :: ToAstApp Ast Ast where toAstApp left right = Application left right
infixr 8 toAstAbs as \
infixl 9 toAstApp as !

instance showAst :: Show Ast where
  show (Reference name) = name
  show (Application (Application leftLeft leftRight) right) = "(" <> show leftLeft <> " " <> show leftRight <> " " <> show right <> ")"
  show (Application left right) = "(" <> show left <> " " <> show right <> ")"
  show (Abstraction head (Abstraction headRight bodyRight)) = "(" <> head <> " => " <> headRight <> " => " <> show bodyRight <> ")"
  show (Abstraction head body) = "(" <> head <> " => " <> show body <> ")"

derive instance eqAst :: Eq Ast

type Scope term = Map.Map String term

class (Show term, Eq term) <= Term term where
  evaluate :: { scope :: Scope term, term :: term } -> { scope :: Scope term, term :: term }

checkFreeVariables :: { free :: Set.Set String, scope :: Set.Set String, term :: Ast } -> Set.Set String
checkFreeVariables { free, scope, term } = case term of
  Reference name -> if Set.member name scope then free else Set.insert name free
  Application left right -> Set.union (checkFreeVariables { free, scope, term: left }) (checkFreeVariables { free, scope, term: right })
  Abstraction head body -> checkFreeVariables { free, scope: Set.insert head scope, term: body }

-- TODO: garbage collect (remove not used variables from scope)

instance termAst :: Term Ast where
  evaluate { scope, term } = case term of
    Reference name -> { scope, term: Maybe.fromMaybe (Reference "") (Map.lookup name scope) }
    Abstraction head body -> { scope, term: Abstraction head body } -- symbolic execution here
    Application (Abstraction leftHead leftBody) right@(Abstraction _ _) ->
      evaluate { scope: Map.insert leftHead right scope, term: leftBody }
    Application left@(Abstraction _ _) right ->
      let rightSide = (evaluate { scope, term: right }).term in
      evaluate { scope, term: Application left rightSide }   
    Application left right@(Abstraction _ _) ->
      let leftSide = evaluate { scope, term: left } in
      evaluate { scope: leftSide.scope, term: Application leftSide.term right }
    Application left right ->
      let leftSide = evaluate { scope, term: left } in
      evaluate { scope: leftSide.scope, term: Application leftSide.term right }

-- TODO: symbolic evaluate

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log "hello"
