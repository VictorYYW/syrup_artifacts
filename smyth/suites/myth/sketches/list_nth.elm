type Nat
  = Z ()
  | S Nat

type NatList
  = Nil ()
  | Cons (Nat, NatList)

list_nth : NatList -> Nat -> Nat
list_nth xs n =
  ??
