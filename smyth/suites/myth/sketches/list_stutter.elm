type Nat
  = Z ()
  | S Nat

type NatList
  = Nil ()
  | Cons (Nat, NatList)

list_stutter : NatList -> NatList
list_stutter xs =
  ??
