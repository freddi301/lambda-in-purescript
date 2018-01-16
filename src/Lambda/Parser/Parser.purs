module Lambda.Parser.Parser where

import Prelude

import Data.Array (filter, head, snoc, tail)
import Data.Foldable (find, foldl)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.String (trim, length)
import Data.String.Regex (match, replace, split)
import Data.String.Regex.Flags (global, noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Lambda.Data.Ast (Ast(..))
import Lambda.Data.Parser (Block(Block), IndentLevel(IndentLevel), Position, Named(..))

-- | TODO: keep source info
foreign import parse :: String -> Named String Position
parseUnit :: String -> Named String Unit
parseUnit text = (const unit) <$> (parse text)

-- | TODO: keep source info
parseIndent :: String -> IndentLevel
parseIndent string =
  let token = unsafeRegex "^ *" noFlags in
  let level = fromMaybe 0 $ (match token string) >>= find isJust >>= id <#> (length >>> flip div 2) in
  IndentLevel level (trim string) -- | TODO: remove trim

-- | TODO: keep source info
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

-- | TODO: keep source info
blocksToAst :: Ast String Unit -> Array Block -> Ast String Unit
blocksToAst upper blocks = subIfAbstraction upper where
  foldAll innermost = foldl reduce innermost (map parseRec blocks)
  reduce body (Named name _ term) = Application (Abstraction name body unit unit) term unit
  parseRec (Block text nested) = case parseUnit text of (Named name unit body) -> (Named name unit (blocksToAst body nested))
  subIfAbstraction (Abstraction head body _ _) = Abstraction head (subIfAbstraction body) unit unit
  subIfAbstraction term = foldAll term

-- | TODO: keep source info
parseProgram :: String -> Ast String Unit
parseProgram = parseBlocks >>> blocksToAst (Reference "main" unit)

-- | TODO: do it better
prettyPrint :: String -> Array Block -> String
prettyPrint indentation blocks = foldl foldBlocks "" blocks where
  foldBlocks acc (Block line sublines) = acc <> indentation <> (trim >>> prettyLine $ line) <> "\n" <> prettyPrint (indentation <> "  ") sublines
  prettyLine = replace (unsafeRegex "\\s+" global) (" ")

prettify :: String -> String
prettify = parseBlocks >>> prettyPrint ""
