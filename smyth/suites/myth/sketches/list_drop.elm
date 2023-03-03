type Nat
  = Z ()
  | S Nat

type NatList
  = Nil ()
  | Cons (Nat, NatList)

list_drop : NatList -> Nat -> NatList
list_drop xs n =
  ??
