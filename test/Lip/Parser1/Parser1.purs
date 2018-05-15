module Test.Lip.Parser.Parser1 where

import Data.Either
import Lip.Parser.Parser1.Data

import Data.Map as Map
import Lip.Data.Ast (Ast(..))
import Lip.Data.Ast.Decoration as AstDecoration
import Lip.Data.Ast.Helpers ((\), (!))
import Lip.Parser.Parser1 (parse)
import Prelude (class Ord, Unit, const, discard, pure, show, unit, ($), (<<<), (>>>))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Test.Spec.Runner (RunnerEffects)

ref name sL sC eL eC = Reference name $ pos sL sC eL eC
app left right sL sC eL eC = Application left right $ pos sL sC eL eC
abs head body sL sC eL eC = Abstraction head body $ pos sL sC eL eC

test ∷ ∀ e . Spec (RunnerEffects e) Unit
test = describe "Parser1" do
  it "works" do
    (parse "x") `shouldEqual` (ref "x" 0 0 0 1)
    (parse "x y") `shouldEqual` (app (ref "x" 0 0 0 1) (ref "y" 0 2 0 3) 0 0 0 3)
    (parse "x = y") `shouldEqual` (abs "x" (ref "y" 0 4 0 5) 0 0 0 1)
    (parse "x\n y") `shouldEqual` (app (ref "x" 0 0 0 1) (ref "y" 1 1 1 2) 0 0 1 2)