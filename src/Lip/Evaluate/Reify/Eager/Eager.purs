module Lip.Evaluate.Reify.Eager where

import Prelude
import Data.Either

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

-- | evaluate == (stop Right) >>> fromRight
stop ::
  ∀ reference decoration inspection .
  Eq reference => 
  (Ast reference decoration -> Either inspection (Ast reference decoration)) ->
  Ast reference decoration ->
  Either inspection (Ast reference decoration)
stop inspect term = do
  let recursive = stop inspect
  ast <- inspect term
  case ast of
    Application (Abstraction head body _) right@(Abstraction _ _ _) _ -> recursive $ reify head right body
    Application left right decoration -> do
      leftAst <- recursive left
      rightAst <- recursive right
      recursive $ Application leftAst rightAst decoration
    _ -> inspect term

-- | evaluate == step >>> runStep
step ::
  ∀ reference decoration .
  Eq reference =>
  Ast reference decoration ->
  Step (Ast reference decoration)
step term = case term of
  Application (Abstraction head body _) right@(Abstraction _ _ _) _ ->
    Continue $ \_ -> step $ reify head right body
  Application left right decoration -> do
    leftAst <- step left
    rightAst <- step right
    Continue $ \_ -> step $ Application leftAst rightAst decoration
  _ -> Done term

data Step result = Done result | Continue (Unit -> Step result)

instance bindStep :: Bind Step where
  bind (Done result) next = next result
  bind (Continue task) next = bind (task unit) next

instance applyStep :: Apply Step where
  apply (Done f) step = map f step
  apply (Continue task) step = Continue $ \_ -> apply (task unit) step

instance functorStep :: Functor Step where
  map f (Done result) = Done $ f result
  map f (Continue task) = Continue $ \_ -> map f $ task unit

runStep :: ∀ result . Step result -> result
runStep (Done result) = result
runStep (Continue task) = runStep $ task unit