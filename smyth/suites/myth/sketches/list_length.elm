type Nat
  = Z ()
  | S Nat

type NatList
  = Nil ()
  | Cons (Nat, NatList)

list_length : NatList -> Nat
list_length xs =
  ??
