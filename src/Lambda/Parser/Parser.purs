module Lambda.Parser.Parser where

import Prelude (Unit)
import Lambda.Data.Ast (Ast)

foreign import parse :: String -> Ast String Unit
