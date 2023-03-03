type Nat
  = Z ()
  | S Nat

type NatList
  = Nil ()
  | Cons (Nat, NatList)

list_take : Nat -> NatList -> NatList
list_take n xs =
  ??
