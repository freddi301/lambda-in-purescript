module Lip.Data.Ast where

import Prelude

-- | `Ast` is the representation of untyped lambda calculus in `first-order abstract syntax` form
-- | - `reference` data type used to represent variable names (ex `String`)
-- | - `decoration` an optional additional data (ex `Unit`) 
data Ast reference decoration
  = Reference reference decoration
  | Application (Ast reference decoration) (Ast reference decoration) decoration
  | Abstraction reference (Ast reference decoration) decoration

derive instance eqAst ∷ (Eq reference, Eq decoration) ⇒ Eq (Ast reference decoration)

instance showAst ∷ (Show reference, Show decoration) ⇒ Show (Ast reference decoration) where
  show (Reference name decoration) = show name <> "[" <> show decoration <> "]"
  show (Application left right decoration) = "(" <> show left <> " " <> show right <> ")" <> "[" <> show decoration <> "]" 
  show (Abstraction head body decoration) = "(" <> show head <> " ⇒ " <> show body <> ")" <> "[" <> show decoration <> "]"