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


-- | TODO: lazy, eager, symbolic evaluation

-- | LAZY: pro: most efficient execution cons: unpredictable performance

-- | EAGER: pro: good for debugging, can indicate when execute cons: some unused arguments could be evaluated

-- | SYMBOLIC: pro: good for suggestins and metaprogramming cons: executes unneded code

-- | symbolic at compile time, eager at debug, lazy at runtime

-- | a naive implementation of and execution broker could be 3 queues
-- | lazy, eager, symbolic, where the evaluations will be partitioned
-- | naively the all the jobs from lazy would be executed before proceeding to eager and so on
-- | but a worker could get a task from eager before all from lazy are executed based on data locality (aka cost)

-- | TODO: hoisting and mutually recursive function

-- | TODO: memoization

-- | TODO: inference

-- | TODO: single step evaluation

-- | TODO: infix operators

-- | TODO: autocompletion

-- | TODO: wrap syntax errors

-- | TODO: prettify

-- | TODO: import system

-- | TODO: atoms

-- | TODO: FFI

-- | TODO: implement standard data structures

-- | TODO: access to code by path (use names hierarchy)

-- | TODO: documentation

-- | TODO: documentation generation

-- | TODO: playground

-- | TODO: named parameters

-- | TODO: common design patterns (ex: dependency injection)

-- | TODO: common functional data structures

-- | TODO: implement unit testing

-- | TODO: good practices assistan (clean code, code complete, TDD, DDD, PEA, GOF)

-- | TODO: implemet other languages features

-- | TODO: refactoring tools

-- | TODO: code navigation and view tools

-- | TODO: refactor

-- | TODO: bundling for browser and node with npm and bower

-- | TODO: finish tests
