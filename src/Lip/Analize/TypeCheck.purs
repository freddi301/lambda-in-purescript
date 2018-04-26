module Lip.Analize.TypeCheck where

import Prelude

import Data.Either as Either
import Data.Foldable (find)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Lip.Analize.Infere (Constraints, Constraint(..))

type Criteria = Context -> TypeAlias -> Either.Either TypeError Context

type TypeAlias = Int

data TypeError = 
  ExpectedToBeAbstraction TypeAlias |
  ExpectedToBeType TypeAlias TypeAlias -- | expected got

type Context = {
  constraints :: Constraints,
  aliases :: Map.Map String TypeAlias
}

makeContext :: Constraints -> Context
makeContext constraints = { constraints, aliases: Map.empty }

abs :: Criteria -> Criteria -> Criteria
abs head body context typ = do
  abstraction <- findAbstraction context typ
  context <- head context abstraction.head
  body context abstraction.body

ref :: String -> Criteria
ref name context typ =
  unify name typ context

unify :: String -> TypeAlias -> Context -> Either.Either TypeError Context
unify alias typ context = case Map.lookup alias context.aliases of
  Maybe.Nothing -> Either.Right context { aliases = Map.insert alias typ context.aliases }
  Maybe.Just existing ->
    if typ == existing then Either.Right context
    else Either.Left (ExpectedToBeType existing typ)

findAbstraction :: Context -> TypeAlias -> Either.Either TypeError { head :: TypeAlias, body :: TypeAlias }
findAbstraction context typ = case Map.lookup typ context.constraints of
  Maybe.Nothing -> Either.Left (ExpectedToBeAbstraction typ)
  Maybe.Just constraints -> 
    let abstraction = find (\constraint -> case constraint of IsAbstraction _ _ -> true) constraints in
    case abstraction of
      Maybe.Just (IsAbstraction head body) -> Either.Right { head, body }
      Nothing -> Either.Left (ExpectedToBeAbstraction typ)

instance showTypeError :: Show TypeError where
  show (ExpectedToBeAbstraction t) = "ExpectedToBeAbstraction " <> show t
  show (ExpectedToBeType et gt) = "ExpectedToBeAbstraction " <> show et <> " " <> show gt
derive instance eqTypeError :: Eq TypeError