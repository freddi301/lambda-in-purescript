module Lip.Evaluate.Reify.Pauseable where

import Prelude (class Eq, flip)

import Lip.Data.Ast (Ast(..))

import Lip.Evaluate.Reify (reify)

import Lip.Evaluate.Pauseable

bind = flip wait

eager :: ∀ container reference decoration . Eq reference ⇒ Pauseable container ⇒
  Ast reference decoration → container (Ast reference decoration)
eager (Application (Abstraction head body _) right@(Abstraction _ _ _) _) =
  eager |> reify head right body
eager (Application left right decoration) = do
  left <- eager |> left
  right <- eager |> right
  eager |> Application left right decoration
eager term = end term

lazy :: ∀ container reference decoration . Eq reference ⇒ Pauseable container ⇒
  Ast reference decoration → container (Ast reference decoration)
lazy (Application (Abstraction head body _) right _) =
  lazy |> reify head right body
lazy (Application left right decoration) = do
  left <- lazy |> left
  lazy |> Application left right decoration
lazy term = end term