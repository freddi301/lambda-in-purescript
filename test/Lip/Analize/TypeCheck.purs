module Test.Lip.Analize.TypeCheck where

import Prelude

import Data.Either as Either
import Data.Map as Map
import Lip.Analize.Infere as Infere
import Lip.Analize.TypeCheck (Context, Criteria, TypeError(..), abs, makeContext, ref)
import Lip.Data.Ast (Ast)
import Lip.Data.Ast.Decoration as AstDecoration
import Lip.Data.Ast.Helpers ((\), (!))
import Test.Spec (Spec, describe, it, pending)
import Test.Spec.Assertions (fail, shouldEqual)
import Test.Spec.Runner (RunnerEffects)

infere ::
  ∀ reference decoration . 
  Ord reference => 
  Ast reference decoration -> 
  { typ :: Int, nextType :: Int, constraints :: Infere.Constraints, ast :: Ast reference { typ :: Int } }
infere ast = Infere.infere { ast: AstDecoration.map (const {}) ast, nextType: 0, typScope: Map.empty, constraints: Map.empty } 

re :: Ast String Unit -> Criteria -> Either.Either TypeError Context
re ast criteria = criteria (makeContext inferred.constraints) inferred.typ where
  inferred :: { typ :: Int, nextType :: Int, constraints :: Infere.Constraints, ast :: Ast String { typ :: Int } }
  inferred = infere ast

test :: ∀ e . Spec (RunnerEffects e) Unit
test = describe "TypeCheck" do
  describe "identity" do
    it "works" do
      case (re ("x" \ "x") (abs (ref "a") (ref "a"))) of
        Either.Left err -> fail ""
        Either.Right dat -> pure unit
  describe "true" do
    let true_ = re ("x" \ "y" \ "x")
    it "works" do
      case (true_ (abs (ref "a") (abs (ref "b") (ref "a")))) of
        Either.Left err -> fail ""
        Either.Right dat -> pure unit
      case (true_ (abs (ref "a") (abs (ref "b") (ref "b")))) of
        Either.Left err -> err `shouldEqual` (ExpectedToBeType 3 1)
        Either.Right dat -> fail ""
