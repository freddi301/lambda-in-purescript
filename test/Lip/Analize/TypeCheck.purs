module Test.Lip.Analize.TypeCheck where

import Data.Either

import Data.Map as Map
import Lip.Analize.Infere as Infere
import Lip.Analize.TypeCheck (Context, Criteria, TypeError(..), abs, makeContext, onAlias, ref, uncallable, (>>>))
import Lip.Data.Ast (Ast)
import Lip.Data.Ast.Decoration as AstDecoration
import Lip.Data.Ast.Helpers ((\), (!))
import Prelude (class Ord, Unit, const, discard, pure, show, unit, ($))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Test.Spec.Runner (RunnerEffects)

infere ::
  ∀ reference decoration . 
  Ord reference ⇒ 
  Ast reference decoration → 
  { typ :: Int, nextType :: Int, constraints :: Infere.Constraints, ast :: Ast reference { typ :: Int } }
infere ast = Infere.infere { ast: AstDecoration.map (const {}) ast, nextType: 0, typScope: Map.empty, constraints: Map.empty } 

re :: Ast String Unit → Criteria → Either TypeError Context
re ast criteria = criteria (makeContext inferred.constraints) inferred.typ where
  inferred :: { typ :: Int, nextType :: Int, constraints :: Infere.Constraints, ast :: Ast String { typ :: Int } }
  inferred = infere ast

test :: ∀ e . Spec (RunnerEffects e) Unit
test = describe "TypeCheck" do
  describe "identity" do
    it "works" do
      case (re ("x" \ "x") (abs (ref "a") (ref "a"))) of
        Left err → fail ""
        Right dat → pure unit
  describe "true" do
    let true_ = re ("x" \ "y" \ "x")
    it "works" do
      case (true_ (abs (ref "a") (abs (ref "b") (ref "a")))) of
        Left err → fail ""
        Right dat → pure unit
      case (true_ (abs (ref "a") (abs (ref "b") (ref "b")))) of
        Left err → err `shouldEqual` (ExpectedToBeType 3 1)
        Right dat → fail ""
  describe "compose criteria" do
    let apply = re ("f" \ "x" \ "f" ! "x")
    it "works for (f ⇒ x ⇒ f x)" do
      -- fail $ case infere ("f" \ "x" \ "f" ! "x") of { typ, constraints } → Infere.showType constraints typ
      case (apply $ (abs (ref "a") $ abs (ref "b") (ref "c")) >>> (onAlias "a" (abs (ref "b") (ref "c")))) of
        Left err → fail (show err)
        Right dat → pure unit
      case (apply $ (abs (ref "a") $ abs (ref "b") (ref "c")) >>> (onAlias "a" (abs (ref "b") (ref "b")))) of
        Left err → err `shouldEqual` (ExpectedToBeType 3 4)
        Right dat → fail ""
  describe "uncallable" do
    let called = re ("x" \ "f" \ ("y" \ "f") ! ("f" ! "x"))
    let identity = re ("x" \ "x")
    it "works for (x ⇒ x) :: (x → x | uncallable a)" do
      case (identity $ (abs (ref "a") (ref "a")) >>> (onAlias "a" uncallable)) of
        Left err → fail (show err)
        Right dat → pure unit
    it "throws for (x ⇒ f ⇒ (y ⇒ f) (f x)) :: (a → b → c | uncallable b)"
      case (called $ (abs (ref "a") (abs (ref "b") (ref "c"))) >>> (onAlias "b" uncallable)) of
        Left err → err `shouldEqual` (ExpectedToBeUncallable 3)
        Right dat → fail ""