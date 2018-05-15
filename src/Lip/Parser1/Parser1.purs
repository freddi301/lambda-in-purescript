module Lip.Parser.Parser1 where

import Prelude

import Data.Array (filter, head, snoc, tail, fromFoldable)
import Data.Foldable (find, foldl)
import Data.Function (flip)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.String (trim, length)
import Data.String.Regex (match, replace, split)
import Data.String.Regex.Flags (global, noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Lip.Data.Ast (Ast(..))
import Lip.Parser.Parser1.Data

foreign import parse ∷ String → Ast String Position

showProgram ∷ Ast String Position → String
showProgram = show

--getSourceMap ∷ Ast String Position → Map.Map Position (Ast String Position)
getSourceMap ast column = filter isJust found where
  found = map (flip Map.lookup sourceMap) positions
  positions = findSource sourceMap
  sourceMap = getSourceMap_ ast Map.empty
  findSource map = filter (\(Position pos) → (pos.startColumn ⇐ column) && (column ⇐ pos.endColumn)) (fromFoldable $ Map.keys map)
  getSourceMap_ ∷ Ast String Position → Map.Map Position (Ast String Position) → Map.Map Position (Ast String Position)
  getSourceMap_ ast map = case ast of
    Reference _ position → Map.insert position ast map
    Application left right position → Map.insert position ast (getSourceMap_ left (getSourceMap_ right map))
    Abstraction head body position → Map.insert position ast (getSourceMap_ body map)