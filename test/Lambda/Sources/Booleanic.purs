module Test.Lambda.Sources.Booleanic where

booleanic :: String
booleanic = """
true a b = a
false a b = b
not p = p false true
and l r = l r false
or l r = (l true r)
main = doKill
  alive = true
  human = false
  alien = and alive (not human)
  doKill = or alien human
"""

booleanicMCSE :: String
booleanicMCSE = """
true a b = a
false a b = b
not p = p false true
and l r = l r false
or l r = l true r
main = doKill
  alive = true
  human = false
  alien = and alive (not human)
  doKill = or alien human
"""

combinatorY :: String
combinatorY = """
Y f = (side f) (side f)
  side f x = f (x x)
main = Y
"""

combinatorX :: String
combinatorX = """
X f = (left f) right
  left x = x x
  right f x = f (x x)
main = X
"""
