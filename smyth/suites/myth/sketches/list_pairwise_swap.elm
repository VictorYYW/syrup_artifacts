type Nat
  = Z ()
  | S Nat

type NatList
  = Nil ()
  | Cons (Nat, NatList)

list_pairwise_swap : NatList -> NatList
list_pairwise_swap xs =
  ??
