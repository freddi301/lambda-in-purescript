module Lambda.Data.Parser where

import Lambda.Data.Ast (Ast)
import Prelude (class Eq, class Functor, class Show, show, (<$>), (<>))

data Position = Position { file :: String, startLine :: Int, endLine :: Int, startColumn :: Int, endColumn :: Int }
derive instance eqPosition :: Eq Position
instance showPosition :: Show Position where
  show (Position { startColumn, endColumn }) = show startColumn <> "," <> show endColumn
fakePos :: Position
fakePos = Position { file: "", startLine: 0, endLine: 0, startColumn: 0, endColumn: 0 }

data IndentLevel = IndentLevel Int String 
instance showIndentLevel :: Show IndentLevel where show (IndentLevel level text) = (show level) <> " " <> text
derive instance eqIndentLevel :: Eq IndentLevel

data Block = Block String (Array Block) 
instance showBlock :: Show Block where show (Block line block) = show line <> " " <> show block
derive instance eqBlock :: Eq Block

data Named reference decoration = Named reference decoration (Ast reference decoration)
derive instance eqNamed :: (Eq reference, Eq decoration) => Eq (Named reference decoration)
instance showNamed :: (Show reference, Show decoration) => Show (Named reference decoration) where
  show (Named name decoration ast) = show name <> " = " <> show ast
instance functorNamed :: Functor (Named reference) where
  map f (Named name decoration ast) = Named name (f decoration) (f <$> ast)
