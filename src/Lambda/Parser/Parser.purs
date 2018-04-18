module Lambda.Parser.Parser where

import Prelude

import Data.Map as Map
import Data.Array (filter, head, snoc, tail, fromFoldable)
import Data.Function (flip)
import Data.Foldable (find, foldl)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.String (trim, length)
import Data.String.Regex (match, replace, split)
import Data.String.Regex.Flags (global, noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Lambda.Data.Ast (Ast(..))
import Lambda.Data.Parser (Block(Block), IndentLevel(IndentLevel), Named(..), Position(Position), fakePos)

foreign import parse :: String -> Named String Position
parseUnit :: String -> Named String Unit
parseUnit text = (const unit) <$> (parse text)

parseIndent :: String -> IndentLevel
parseIndent string =
  let token = unsafeRegex "^ *" noFlags in
  let level = fromMaybe 0 $ (match token string) >>= find isJust >>= id <#> (length >>> flip div 2) in
  IndentLevel level string

parseBlocks :: String -> Array Block
parseBlocks = splitLines >>> parseIndentation >>> filterEmpty >>> toBlocks where
  splitLines = split (unsafeRegex "\n" noFlags)
  parseIndentation = map parseIndent
  filterEmpty = filter (\(IndentLevel _ text) -> text /= "")
  toBlocks lines = (reduceLines [] 0 lines).blocks
  reduceLines blocks level lines = case head lines of
    Just (IndentLevel lv text) | lv == level -> 
        let nested = reduceLines [] (level + 1) (rest lines) in
        reduceLines (snoc blocks (Block text nested.blocks)) level nested.lines
    Just (IndentLevel lv text) | lv > level -> reduceLines [] (level + 1) (rest lines)
    Just (IndentLevel lv text) | lv < level -> { blocks, lines }
    _ -> { blocks, lines }
  rest lines = fromMaybe [] (tail lines)

blocksToAst :: Ast String Position -> Array Block -> Ast String Position
blocksToAst upper blocks = subIfAbstraction upper where
  foldAll innermost = foldl reduce innermost (map parseRec blocks)
  reduce body (Named name decoration term) = Application (Abstraction name body decoration fakePos) term fakePos
  parseRec (Block text nested) = case parse text of (Named name decoration body) -> (Named name decoration (blocksToAst body nested))
  subIfAbstraction (Abstraction head body headDecoration decoration) = Abstraction head (subIfAbstraction body) headDecoration decoration
  subIfAbstraction term = foldAll term

parseProgram :: String -> Ast String Position
parseProgram = parseBlocks >>> blocksToAst (Reference "main" fakePos)

parseProgramUnit :: String -> Ast String Unit
parseProgramUnit text = (const unit) <$> parseProgram text

-- | TODO: do it better
prettyPrint :: String -> Array Block -> String
prettyPrint indentation blocks = foldl foldBlocks "" blocks where
  foldBlocks acc (Block line sublines) = acc <> indentation <> (trim >>> prettyLine $ line) <> "\n" <> prettyPrint (indentation <> "  ") sublines
  prettyLine = replace (unsafeRegex "\\s+" global) (" ")

prettify :: String -> String
prettify = parseBlocks >>> prettyPrint ""

--getSourceMap :: Ast String Position -> Map.Map Position (Ast String Position)
getSourceMap ast column = filter isJust found where
  found = map (flip Map.lookup sourceMap) positions
  positions = findSource sourceMap
  sourceMap = getSourceMap_ ast Map.empty
  findSource map = filter (\(Position pos) -> (pos.startColumn <= column) && (column <= pos.endColumn)) (fromFoldable $ Map.keys map)
  getSourceMap_ :: Ast String Position -> Map.Map Position (Ast String Position) -> Map.Map Position (Ast String Position)
  getSourceMap_ ast map = case ast of
    Reference _ position -> Map.insert position ast map
    Application left right position -> (Map.insert position ast (getSourceMap_ left (getSourceMap_ right map)))
    Abstraction head body headPosition bodyPostion -> 
      Map.insert bodyPostion ast (Map.insert headPosition ast (getSourceMap_ body map))
