type Nat
  = Z ()
  | S Nat

type NatList
  = Nil ()
  | Cons (Nat, NatList)

type NatOpt
  = None ()
  | Some Nat

list_last : NatList -> NatOpt
list_last xs =
  ??
