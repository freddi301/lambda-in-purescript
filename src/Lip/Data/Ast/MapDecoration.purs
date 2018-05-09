module Lip.Data.Ast.Decoration where

import Lip.Data.Ast

map :: ∀ r a b. (a → b) → Ast r a → Ast r b
map f (Reference name d) = Reference name (f d)
map f (Application left right d) = Application (map f left) (map f right) (f d)
map f (Abstraction head body d) = Abstraction head (map f body) (f d)