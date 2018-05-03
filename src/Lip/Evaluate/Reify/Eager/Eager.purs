module Lip.Evaluate.Reify.Eager where

import Prelude

import Lip.Data.Ast (Ast(..))
import Lip.Evaluate
import Lip.Evaluate.Reify (reify)

-- | `reifyEvaluateEager` evaluates a lambda term using the reify mechanism,
-- | the execution is eager,
-- | there is no scope, as soon variable gets bound, every occurrence is substituted with its value.
evaluate :: ∀ reference decoration . Eq reference => Evaluate reference decoration
evaluate term = case term of
  Application (Abstraction head body _) right@(Abstraction _ _ _) _ -> evaluate $ reify head right body
  Application left right decoration -> evaluate $ Application (evaluate left) (evaluate right) decoration
  _ -> term

-- | evaluate == enhance id
enhance ::
  ∀ reference decoration . Eq reference =>
  (Evaluate reference decoration -> Evaluate reference decoration) ->
  Evaluate reference decoration
enhance enhancer = enhancer evaluate where
  evaluate (Application (Abstraction head body _) right@(Abstraction _ _ _) _) = (enhancer evaluate) $ reify head right body
  evaluate (Application left right decoration) = (enhancer evaluate) $ Application ((enhancer evaluate) left) ((enhancer evaluate) right) decoration
  evaluate term = term