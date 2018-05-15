module Lip.Evaluate.Pauseable where

import Prelude (class Eq, class Show, (<>), (==), show, ($), (&&), id, flip, (>>>))

data Intermediate result = End result | Intermediate result (result → Intermediate result)

instance showIntermediate ∷ Show result ⇒ Show (Intermediate result) where
  show (End result) = show "End " <> show result
  show (Intermediate result _) = show "Intermediate " <> show result

instance eqIntermediate ∷ Eq result ⇒ Eq (Intermediate result) where
  eq (End a) (End b) = a == b
  eq a@(Intermediate ar at) b@(Intermediate br bt) = (ar == br) && ((runIntermediate id a) == (runIntermediate id b))
  eq _ _ = false

getResult ∷ ∀ result . Intermediate result → result
getResult (End result) = result
getResult (Intermediate result _) = result

next ∷ ∀ result . Intermediate result → Intermediate result
next (End result) = (End result)
next (Intermediate result task) = task result

runIntermediate ∷ ∀ result . (result → result) → Intermediate result → result
runIntermediate enhancer (End result) = result
runIntermediate enhancer (Intermediate result next) = runIntermediate enhancer $ next $ enhancer $ result

class Pauseable container where
  recur ∷ ∀ content . (content → container content) → content → container content
  wait ∷ ∀ content . (content → container content) → container content →  container content
  end ∷ ∀ content . content → container content

infixr 0 recur as |>

instance pauseableIntermediate ∷ Pauseable Intermediate where
  wait next (Intermediate result task) = Intermediate result $ task >>> wait next
  wait next (End result) = next result
  recur = flip Intermediate
  end = End
