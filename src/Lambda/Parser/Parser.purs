module Lambda.Parser.Parser where


import Prelude

import Data.Array (filter, head, snoc, tail)
import Data.Foldable (find, foldl)
import Data.Maybe (Maybe(..), fromJust, fromMaybe, isJust)
import Data.String (trim, length)
import Data.String.Regex (match, split)
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Lambda.Data.Ast (Ast)

foreign import parse :: String -> Ast String Unit


data IndentLevel = IndentLevel Int String 
instance showIndentLevel :: Show IndentLevel where show (IndentLevel level text) = (show level) <> " " <> text
derive instance eqIndentLevel :: Eq IndentLevel

parseIndent :: String -> IndentLevel
parseIndent string =
  let token = unsafeRegex "^ *" noFlags in
  let level = fromMaybe 0 $ (match token string) >>= find isJust >>= id <#> (length >>> flip div 2) in
  IndentLevel level (trim string)

data Block = Block String (Array Block) 
instance showBlock :: Show Block where show (Block line block) = show line <> " " <> show block
derive instance eqBlock :: Eq Block

parseBlocks :: String -> Array Block
parseBlocks = splitLines >>> parseIndentation >>> filterEmpty >>> toBlocks where
  splitLines = split (unsafeRegex "\n" noFlags)
  parseIndentation = map parseIndent
  filterEmpty = filter (\(IndentLevel _ text) -> text /= "")
  toBlocks lines = (reduceLines [] 0 lines).blocks
  reduceLines blocks level lines = case head lines of
    Nothing -> { blocks, lines }
    Just (IndentLevel lv text) | lv == level -> 
        let nested = reduceLines [] (level + 1) (rest lines) in
        reduceLines (snoc blocks (Block text nested.blocks)) level nested.lines
    Just (IndentLevel lv text) | lv > level -> reduceLines [] (level + 1) (rest lines)
    Just (IndentLevel lv text) | lv < level -> { blocks, lines }
    _ -> { blocks, lines }
  rest lines = fromMaybe [] (tail lines)

