module Lambda.Data.Ast where

import Prelude

-- | `Ast` is the representation of untyped lambda calculus in `first-order abstract syntax` form
-- | `reference` data type used to represent variable names (ex `String`)
-- | `decoration` an optional additional data (ex `Unit`) 
data Ast reference decoration
  = Reference reference decoration
  | Application (Ast reference decoration) (Ast reference decoration) decoration
  | Abstraction reference (Ast reference decoration) decoration

-- | `Functor` instance for decoration part
instance functorAst :: Functor (Ast reference) where
  map f (Reference name d) = Reference name (f d)
  map f (Application left right d) = Application (map f left) (map f right) (f d)
  map f (Abstraction head body d) = Abstraction head (map f body) (f d)

-- | `Functor.map` implementation for reference part
mapReference :: forall reference toReference decoration .
  (reference -> toReference) -> Ast reference decoration -> Ast toReference decoration
mapReference f (Reference name decoration) = Reference (f name) decoration
mapReference f (Application left right decoration) = Application (mapReference f left) (mapReference f right) decoration
mapReference f (Abstraction head body decoration) = Abstraction (f head) (mapReference f body) decoration

instance showAst :: (Show reference, Show decoration) => Show (Ast reference decoration) where
  show (Reference name decoration) = show decoration <> show name
  show (Application left right decoration) = show decoration <> "(" <> show left <> " " <> show right <> ")"
  show (Abstraction head body decoration) = show decoration <> "(" <> show head <> " => " <> show body <> ")"

prettyPrint :: Ast String Unit -> String
prettyPrint (Reference name _) = name
prettyPrint (Application (Application leftLeft leftRight _) right _) = "(" <> prettyPrint leftLeft <> " " <> prettyPrint leftRight <> " " <> prettyPrint right <> ")"
prettyPrint (Application left right _) = "(" <> prettyPrint left <> " " <> prettyPrint right <> ")"
prettyPrint (Abstraction head (Abstraction headRight bodyRight _) _) = "(" <> head <> " => " <> headRight <> " => " <> prettyPrint bodyRight <> ")"
prettyPrint (Abstraction head body _) = "(" <> head <> " => " <> prettyPrint body <> ")"

-- | operators for friendlier construction of the ast
-- | λx.x x
-- | "x" \ "x" ! "x"
infixr 8 toAstAbs as \
infixl 9 toAstApp as !
class ToAstAbs body where toAstAbs :: String -> body -> Ast String Unit
class ToAstApp left right where toAstApp :: left -> right -> Ast String Unit
instance toAstAbsString :: ToAstAbs String where toAstAbs head body = abs head (ref body)
instance toAstAbsAst :: ToAstAbs (Ast String Unit) where toAstAbs head body = abs head body
instance toAstAppStringString :: ToAstApp String String where toAstApp left right = app (ref left) (ref right)
instance toAstAppStringAst :: ToAstApp String (Ast String Unit) where toAstApp left right = app (ref left) right
instance toAstAppAstString :: ToAstApp (Ast String Unit) String where toAstApp left right = app left (ref right)
instance toAstAppAstAst :: ToAstApp (Ast String Unit) (Ast String Unit) where toAstApp left right = app left right
ref :: String -> Ast String Unit
ref name = Reference name unit
app :: Ast String Unit -> Ast String Unit -> Ast String Unit
app left right = Application left right unit
abs :: String -> Ast String Unit -> Ast String Unit
abs head body = Abstraction head body unit
-- derive instance showAstStringUnit :: Show (Ast String Unit)
derive instance eqAstStringUnit :: Eq (Ast String Unit)