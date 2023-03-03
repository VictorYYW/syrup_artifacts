type Nat
  = Z ()
  | S Nat

type NatList
  = Nil ()
  | Cons (Nat, NatList)

list_tl : NatList -> NatList
list_tl xs =
  ??
