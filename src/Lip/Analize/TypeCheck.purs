module Lip.Analize.TypeCheck where

import Prelude (class Eq, class Show, bind, show, (<>), (==))

import Data.Either (Either(..), note)
import Data.Foldable (find)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Lip.Analize.Infere (Constraints, Constraint(..))

type Criteria = Context → TypeAlias → Either TypeError Context

type TypeAlias = Int

data TypeError = 
  ExpectedToBeAbstraction TypeAlias |
  ExpectedToBeType TypeAlias TypeAlias | -- | expected got
  ExpectedToBeUncallable TypeAlias |
  UndefinedAlias String

type Context = {
  constraints :: Constraints,
  aliases :: Map.Map String TypeAlias
}

makeContext :: Constraints → Context
makeContext constraints = { constraints, aliases: Map.empty }

abs :: Criteria → Criteria → Criteria
abs head body context typ = do
  abstraction ← note (ExpectedToBeAbstraction typ) (findAbstraction context typ)
  context ← head context abstraction.head
  body context abstraction.body

ref :: String → Criteria
ref name context typ =
  unify name typ context

uncallable :: Criteria
uncallable context typ = case findAbstraction context typ of
  Nothing → Right context
  Just _ → Left (ExpectedToBeUncallable typ)

onAlias :: String → Criteria → Criteria
onAlias alias criteria context typ = do
  aliasType ← note (UndefinedAlias alias) (Map.lookup alias context.aliases)
  criteria context aliasType

unify :: String → TypeAlias → Context → Either TypeError Context
unify alias typ context = case Map.lookup alias context.aliases of
  Nothing → Right context { aliases = Map.insert alias typ context.aliases }
  Just existing →
    if typ == existing then Right context
    else Left (ExpectedToBeType existing typ)

findAbstraction :: Context → TypeAlias → Maybe { head :: TypeAlias, body :: TypeAlias }
findAbstraction context typ = do
  constraints ← Map.lookup typ context.constraints
  abstraction ← find (\constraint → case constraint of IsAbstraction _ _ → true) constraints
  case abstraction of (IsAbstraction head body) → Just { head, body }

derive instance eqTypeError :: Eq TypeError
instance showTypeError :: Show TypeError where
  show (ExpectedToBeAbstraction t) = "ExpectedToBeAbstraction " <> show t
  show (ExpectedToBeType et gt) = "ExpectedToBeType " <> show et <> " " <> show gt
  show (ExpectedToBeUncallable t) = "ExpectedToBeUncallable " <> show t
  show (UndefinedAlias alias) = "UndefinedAlias " <> alias

composeCriteria :: Criteria → Criteria → Criteria
composeCriteria a b context typ = do
  context ← b context typ
  a context typ

composeCriteriaFlipped :: Criteria → Criteria → Criteria
composeCriteriaFlipped a b context typ = do
  context ← a context typ
  b context typ

infixr 9 composeCriteria as <<<
infixr 9 composeCriteriaFlipped as >>>