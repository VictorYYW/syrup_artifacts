type Nat
  = Z ()
  | S Nat

type NatList
  = Nil ()
  | Cons (Nat, NatList)

zero : Nat -> Nat
zero n = Z ()

inc : Nat -> Nat
inc n = S n

list_map : (Nat -> Nat) -> NatList -> NatList
list_map f =
  let
    listMapFix : NatList -> NatList
    listMapFix xs =
      ??
  in
    listMapFix
