module Test.Main where
  
import Control.Monad.Eff (Eff)
import Prelude (Unit, discard)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (RunnerEffects, run)

import Test.Lambda.Data.Ast as Ast
import Test.Lambda.Control.Evaluate as Evaluate
import Test.Lambda.Parser.Parser as Parser

main :: Eff (RunnerEffects ()) Unit
main = run [consoleReporter] do
  Ast.test
  Evaluate.test
  Parser.test
