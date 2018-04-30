module Test.Lambda.Data.Common where

import Prelude (Unit)
import Lambda.Data.Ast (Ast, (!), (\))

u :: Ast String Unit
u = ("g" \ ("g" ! "g"))

y :: Ast String Unit
y = ("f" \ ("x" \ "f" ! ("x" ! "x")) ! ("x" \ "f" ! ("x" ! "x")))

z :: Ast String Unit
z = "g" \ (("x" \ "g" ! ("v" \ (("x" ! "x") ! "v"))) ! ("x" \ "g" ! ("v" \ (("x" ! "x") ! "v"))))

id :: Ast String Unit
id = ("x" \ "x")
