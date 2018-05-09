module Lip.Parser.Parser1.Data where

import Prelude

data Position
  = Position { startLine :: Int, endLine :: Int, startColumn :: Int, endColumn :: Int }
  | NoPosition

derive instance eqPosition :: Eq Position
derive instance ordPosition :: Ord Position
instance showPosition :: Show Position where
  show (Position { startLine, endLine, startColumn, endColumn }) =
    show startLine <> ":" <> show startColumn <> "-" <> show endLine <> ":" <> show endColumn
  show NoPosition = ":-:"

pos :: Int → Int → Int → Int → Position
pos startLine startColumn endLine endColumn = Position { startLine, startColumn, endLine, endColumn }