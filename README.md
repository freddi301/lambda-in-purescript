# Lambda in PureScript

Is a lambda claculus implemented in purescript

It supports 3 evaluation types:

- LAZY:
  - pro: most efficient execution
  - cons: unpredictable performance

- EAGER:
  - pro: good for debugging, can indicate when execute
  - cons: some unused arguments could be evaluated

- SYMBOLIC:
  - pro: good for suggestions and metaprogramming
  - cons: executes unneeded code

**optimal use cases** symbolic at compile time, eager at debug, lazy at runtime

a naive implementation of and execution broker could be 3 queues
lazy, eager, symbolic, where the evaluations will be partitioned
naively the all the jobs from lazy would be executed before proceeding to eager and so on
but a worker could get a task from eager before all from lazy are executed based on data locality (aka cost)

## TODO

- implement and test https://en.wikipedia.org/wiki/Lambda_calculus#Standard_terms

- see https://en.wikipedia.org/wiki/Lambda_calculus#Optimal_reduction

- introduce types and metaprogramming

- debugger

- source maps

- syntax highlight (partially implemented as vscode language support)

- resolve some non terminating samples

- analyze program for code duplication (n-conversion + mangled references)

- check for undeclared variables

- make possible recursive function by name  

- make possible recursive function by name (for inline functions)

- hoisting and mutually recursive function

- memoization

- inference

- single step evaluation (missing for symbolic)

- infix operators

- autocompletion

- wrap syntax errors

- prettify

- atoms

- FFI

- implement standard data structures

- documentation

- documentation generation

- playground

- named parameters

- common design patterns (ex: dependency injection)

- common functional data structures

- implement unit testing

- good practices assistant (clean code, code complete, TDD, DDD, PEA, GOF)

- implement other languages features

- refactoring tools

- code navigation and view tools

- refactor

- bundling for browser and node with npm and bower

- finish tests

- implement η-conversion (eta-conversion)

- η-conversion + α-conversion + α-equivalence can be used to validate laws (example comonad laws)

- parallel execution

- distributed execution

- rename symbol (F2 in vscode) with reify

- import system (throught url + git)

- derive like in typeclasses

- stackless interpret (lazy, eager, symbolic)

- extract value (using symbolic execution)