module Lip.Evaluate.Reify where

import Prelude (class Eq, (==))

import Lip.Data.Ast (Ast(..))

-- | `reify` takes a reference and substitutes every occurrence of it respecting lexical scoping rules,
-- | it corresponds to β-reduction.
-- | - `ref` the reference
-- | - `value` the value being copied in the ast
-- | - `term` the ast where the value will be substituted
reify ∷ ∀ reference decoration . Eq reference ⇒ reference → Ast reference decoration → Ast reference decoration → Ast reference decoration
reify ref value term = case term of
  Reference name decoration → if ref == name then value else term
  Abstraction head body decoration → if ref == head then term else Abstraction head (reify ref value body) decoration
  Application left right decoration → Application (reify ref value left) (reify ref value right) decoration