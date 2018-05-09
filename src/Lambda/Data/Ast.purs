module Lambda.Data.Ast where

import Prelude
import Data.String (take, length, drop)
import Data.Map as Map

-- | `Ast` is the representation of untyped lambda calculus in `first-order abstract syntax` form
-- | `reference` data type used to represent variable names (ex `String`)
-- | `decoration` an optional additional data (ex `Unit`) 
data Ast reference decoration
  = Reference reference decoration
  | Application (Ast reference decoration) (Ast reference decoration) decoration
  | Abstraction reference (Ast reference decoration) decoration decoration

-- | `Functor` instance for decoration part
instance functorAst :: Functor (Ast reference) where
  map f (Reference name d) = Reference name (f d)
  map f (Application left right d) = Application (map f left) (map f right) (f d)
  map f (Abstraction head body hd d) = Abstraction head (map f body) (f hd) (f d)

-- | `Functor.map` implementation for reference part
mapReference :: ∀ reference toReference decoration .
  (reference → toReference) → Ast reference decoration → Ast toReference decoration
mapReference f (Reference name decoration) = Reference (f name) decoration
mapReference f (Application left right decoration) = Application (mapReference f left) (mapReference f right) decoration
mapReference f (Abstraction head body headDecoration decoration) = Abstraction (f head) (mapReference f body) headDecoration decoration

instance showAst :: (Show reference, Show decoration) ⇒ Show (Ast reference decoration) where
  show (Reference name decoration) = show name <> "[" <> show decoration <> "]"
  show (Application left right decoration) = "(" <> show left <> " " <> show right <> ")" <> "[" <> show decoration <> "]" 
  show (Abstraction head body headDecoration decoration) = "(" <> show head <> " ⇒ " <> show body <> ")" <> "[" <> show headDecoration <> "]" <> "[" <> show decoration <> "]"

prettyPrint :: Ast String Unit → String
prettyPrint (Reference name _) = name
prettyPrint (Application (Application leftLeft leftRight _) right _) = "(" <> prettyPrint leftLeft <> " " <> prettyPrint leftRight <> " " <> prettyPrint right <> ")"
prettyPrint (Application left right _) = "(" <> prettyPrint left <> " " <> prettyPrint right <> ")"
prettyPrint (Abstraction head (Abstraction headRight bodyRight _ _) _ _) = "(" <> head <> " ⇒ " <> headRight <> " ⇒ " <> prettyPrint bodyRight <> ")"
prettyPrint (Abstraction head body@(Application _ _ _) _ _) = "(" <> head <> " ⇒ " <> removeParens (prettyPrint body) <> ")" where
  removeParens string = (take ((length string) - 2)) (drop 1 string) 
prettyPrint (Abstraction head body _ _) = "(" <> head <> " ⇒ " <> prettyPrint body <> ")"

-- | operators for friendlier construction of the ast
-- | λx.x x
-- | "x" \ "x" ! "x"
infixr 8 toAstAbs as \
infixl 9 toAstApp as !
class ToAstAbs body where toAstAbs :: String → body → Ast String Unit
class ToAstApp left right where toAstApp :: left → right → Ast String Unit
instance toAstAbsString :: ToAstAbs String where toAstAbs head body = abs head (ref body)
instance toAstAbsAst :: ToAstAbs (Ast String Unit) where toAstAbs head body = abs head body
instance toAstAppStringString :: ToAstApp String String where toAstApp left right = app (ref left) (ref right)
instance toAstAppStringAst :: ToAstApp String (Ast String Unit) where toAstApp left right = app (ref left) right
instance toAstAppAstString :: ToAstApp (Ast String Unit) String where toAstApp left right = app left (ref right)
instance toAstAppAstAst :: ToAstApp (Ast String Unit) (Ast String Unit) where toAstApp left right = app left right
ref :: String → Ast String Unit
ref name = Reference name unit
app :: Ast String Unit → Ast String Unit → Ast String Unit
app left right = Application left right unit
abs :: String → Ast String Unit → Ast String Unit
abs head body = Abstraction head body unit unit
derive instance eqAst :: (Eq reference, Eq decoration) ⇒ Eq (Ast reference decoration)

-- | α-conversion for α-equivalence
αConversion ::
  ∀ reference decoration .
  Eq reference ⇒
  { ast :: Ast reference decoration, map :: Map.Map Int reference, symbol :: Int } →
  { ast :: Ast Int decoration, map :: Map.Map Int reference, symbol :: Int }
αConversion { ast, map, symbol } = unwrapAst $ rec { ast: liftedAst, map, symbol } where
  liftedAst = mapReference Unmangled ast
  -- TODO: care for free references
  rec { ast, map, symbol } = case ast of
    (Reference (Mangled name) decoration) → { ast, map, symbol }
    (Reference (Unmangled name) decoration) →
      let resultAst = Reference (Mangled symbol) decoration in
      let resultMap = Map.insert symbol name map in
      let resultSymbol = symbol + 1 in
      { ast: resultAst, map: resultMap, symbol: resultSymbol }
    (Application left right decoration) →
      let leftResult = rec { ast: left, map, symbol } in
      let rightResult = rec { ast: right, map: leftResult.map, symbol: leftResult.symbol } in
      let resultAst = Application leftResult.ast rightResult.ast decoration in
      { ast: resultAst, map: rightResult.map, symbol: rightResult.symbol }
    (Abstraction (Mangled head) body headDecoration decoration) → { ast, map, symbol }
    (Abstraction (Unmangled head) body headDecoration decoration) →
      let nextMap = Map.insert symbol head map in
      let nextSymbol = symbol + 1 in
      let resultHead = Mangled symbol in
      let reifiedBody = replaceReference (Unmangled head) resultHead body in
      let bodyResult = rec { ast: reifiedBody, map: nextMap, symbol: nextSymbol } in
      let resultAst = Abstraction resultHead bodyResult.ast headDecoration decoration in
      { ast: resultAst, map: bodyResult.map, symbol: bodyResult.symbol }
  unwrapAst { ast, map, symbol } = { ast: mapReference unwrap ast, map, symbol }
  unwrap (Mangled symbol) = symbol
  unwrap (Unmangled name) = 666

data Mangled reference symbol = Mangled symbol | Unmangled reference
derive instance eqMangled :: (Eq reference, Eq symbol) ⇒ Eq (Mangled reference symbol)

αEquals :: 
  ∀ reference decoration .
  Eq reference ⇒
  Ast reference decoration →
  Ast reference decoration →
  Boolean
αEquals left right = (α left) == (α right) where
  α ast = (αConversion { ast: (const unit) <$> left, map: Map.empty, symbol: 0 }).ast

replaceReference :: ∀ reference decoration . Eq reference ⇒ reference → reference → Ast reference decoration → Ast reference decoration
replaceReference ref value term = case term of
  Reference name decoration → if ref == name then (Reference value decoration) else term
  Abstraction head body headDecoration decoration → if ref == head then term else Abstraction head (replaceReference ref value body) headDecoration decoration
  Application left right decoration → Application (replaceReference ref value left) (replaceReference ref value right) decoration
