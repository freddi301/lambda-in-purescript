module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Lambda.Control.Evaluate (reifyEvaluateSymbolic)
import Lambda.Data.Ast (Ast)
import Lambda.Parser.Parser (parseProgram)

reifySource :: String -> Ast String Unit
reifySource = parseProgram >>> reifyEvaluateSymbolic 0

booleanic :: String
booleanic = """
true a b = a
false a b = b
not p = p false true
and l r = l r false
or l r = (l true r)
main = doKill
  alive = true
  human = false
  alien = and alive (not human)
  doKill = or alien human
"""

main :: Eff (console :: CONSOLE) Unit
main = do
  log "hello world"
  log $ show $ reifySource booleanic

-- | TODO: implement and test https://en.wikipedia.org/wiki/Lambda_calculus#Standard_terms

-- | TODO: see https://en.wikipedia.org/wiki/Lambda_calculus#Optimal_reduction

-- | TODO: introduce types and metaprogramming

-- | TODO: mix test normal and symbolic evaluation

-- | TODO: debugger

-- | TODO: source maps

-- | TODO: syntax highlight
