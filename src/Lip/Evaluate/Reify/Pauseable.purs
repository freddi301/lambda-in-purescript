module Lip.Evaluate.Reify.Pauseable where

import Prelude (class Eq, class Show, (<>), (==), show, ($), (&&), id, flip, (>>>))

import Lip.Data.Ast (Ast(..))
import Lip.Evaluate.Reify (reify)

data Intermediate result = End result | Intermediate result (result → Intermediate result)

instance showIntermediate :: Show result ⇒ Show (Intermediate result) where
  show (End result) = show "End " <> show result
  show (Intermediate result _) = show "Intermediate " <> show result

instance eqIntermediate :: Eq result ⇒ Eq (Intermediate result) where
  eq (End a) (End b) = a == b
  eq a@(Intermediate ar at) b@(Intermediate br bt) = (ar == br) && ((runIntermediate id a) == (runIntermediate id b))
  eq _ _ = false

getResult :: ∀ result . Intermediate result → result
getResult (End result) = result
getResult (Intermediate result _) = result

next (End result) = (End result)
next (Intermediate result task) = task result

runIntermediate :: ∀ result . (result → result) → Intermediate result → result
runIntermediate enhancer (End result) = result
runIntermediate enhancer (Intermediate result next) = runIntermediate enhancer $ next $ enhancer $ result

class Pauseable container where
  recur :: ∀ content . (content → container content) → content → container content
  wait :: ∀ content . (content → container content) → container content →  container content
  end :: ∀ content . content → container content

infixr 0 recur as |>

instance pauseableIntermediate :: Pauseable Intermediate where
  wait next (Intermediate result task) = Intermediate result $ task >>> wait next
  wait next (End result) = next result
  recur = flip Intermediate
  end = End

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