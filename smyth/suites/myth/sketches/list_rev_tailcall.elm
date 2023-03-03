type Nat
  = Z ()
  | S Nat

type NatList
  = Nil ()
  | Cons (Nat, NatList)

list_rev_tailcall : NatList -> NatList -> NatList
list_rev_tailcall xs acc =
  ??
