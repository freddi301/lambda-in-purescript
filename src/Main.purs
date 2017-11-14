module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

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

-- | TODO: analyze program for code duplication

-- | TODO: check for undeclared variables

-- | TODO: import system

-- | TODO: support lazy execution in symbolic execution

-- | TODO: make possible recursive function by name  

-- | TODO: make possible recursive function by name (for inline functions)
