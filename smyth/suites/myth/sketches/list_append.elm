type Nat
  = Z ()
  | S Nat

type NatList
  = Nil ()
  | Cons (Nat, NatList)

list_append : NatList -> NatList -> NatList
list_append xs ys =
  ??
