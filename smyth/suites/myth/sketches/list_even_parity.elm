type Boolean
  = T ()
  | F ()

type BooleanList
  = Nil ()
  | Cons (Boolean, BooleanList)

list_even_parity : BooleanList -> Boolean
list_even_parity xs =
  ??
