module Lip.Data.Ast.Helpers where

import Prelude
import Lip.Data.Ast (Ast(..))

-- | operators for friendlier construction of the ast
-- | λx.x x
-- | "x" \ "x" ! "x"

infixr 8 toAstAbs as \
infixl 9 toAstApp as !

ref ∷ String → Ast String Unit
ref name = Reference name unit

app ∷ Ast String Unit → Ast String Unit → Ast String Unit
app left right = Application left right unit

abs ∷ String → Ast String Unit → Ast String Unit
abs head body = Abstraction head body unit

class ToAstAbs body where toAstAbs ∷ String → body → Ast String Unit
class ToAstApp left right where toAstApp ∷ left → right → Ast String Unit

instance toAstAbsString ∷ ToAstAbs String where toAstAbs head body = abs head (ref body)
instance toAstAbsAst ∷ ToAstAbs (Ast String Unit) where toAstAbs head body = abs head body
instance toAstAppStringString ∷ ToAstApp String String where toAstApp left right = app (ref left) (ref right)
instance toAstAppStringAst ∷ ToAstApp String (Ast String Unit) where toAstApp left right = app (ref left) right
instance toAstAppAstString ∷ ToAstApp (Ast String Unit) String where toAstApp left right = app left (ref right)
instance toAstAppAstAst ∷ ToAstApp (Ast String Unit) (Ast String Unit) where toAstApp left right = app left right