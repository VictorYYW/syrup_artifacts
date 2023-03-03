type Nat
  = Z ()
  | S Nat

type NatList
  = Nil ()
  | Cons (Nat, NatList)

list_snoc : NatList -> Nat -> NatList
list_snoc xs n =
  ??
