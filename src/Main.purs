module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Map as Map
import Lambda.Control.Evaluate (reifyEvaluate, reifyEvaluateSymbolic)
import Lambda.Control.Other (evaluate)
import Lambda.Data.Ast (Ast, prettyPrint)
import Lambda.Parser.Parser (parseProgram)





main :: Eff (console :: CONSOLE) Unit
main = do
  log "hello world"

-- | TODO: implement and test https://en.wikipedia.org/wiki/Lambda_calculus#Standard_terms

-- | TODO: see https://en.wikipedia.org/wiki/Lambda_calculus#Optimal_reduction

-- | TODO: introduce types and metaprogramming

-- | TODO: mix test normal and symbolic evaluation

-- | TODO: debugger

-- | TODO: source maps

-- | TODO: syntax highlight

-- | TODO: resolve some non terminating samples

-- | TODO: make available function variables to indented block
