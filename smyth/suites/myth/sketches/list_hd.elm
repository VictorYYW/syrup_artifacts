type Nat
  = Z ()
  | S Nat

type NatList
  = Nil ()
  | Cons (Nat, NatList)

list_hd : NatList -> Nat
list_hd xs =
  ??
