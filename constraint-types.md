id = x => x
true = x => y => x
callback (x => y) => x => y

idType = a => abs(a)(a)
trueType = a => b => abs(a)(abs(b)(a))

1
1 abs 2 2

abs head body context type =
  if (context type) has (abstraction head body)
  else error... 

1
1 abs 2 3
3 abs 4 2

type context type = type x | error x
ref context type = if (context type) has (reference type)

abs a a
idType context type =
  abs = find abstraction (context type)
  abs.head === abs.body

abs a (abs b a)
trueType context type =
  abs1 = find abstraction (context type)
  abs2 = find abstraction (context abs1.body)
  abs1.head === abs2.body

abs (abs a b) (abs a b)
callbackType context type =
  abs1 = find abstraction (context type)
  abs2 = find abstraction (context abs1.head)
  abs3 = find abstraction (context abs1.body)
  abs2.head === abs3.head
  abs2.body === abs3.body


abs head body context type =
  abs = find abstraction (context type)
  headCheck = head
    reference -> unify head abs.head context
    abstraction -> head context abs.head
  bodyCheck = body
    reference -> unify body abs.body headCheck
    abstraction -> body headCheck abs.body

ref name context type =
  unify name type context

------


abs head body context type =
  abs = find abstraction (context type)
  body (head context abs.head) abs.body

ref name context type =
  unify name type context

unify alias existing context =
  if alias already associated in context
    then existing must be equal to that
    else associate

