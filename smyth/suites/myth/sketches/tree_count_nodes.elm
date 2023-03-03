type Nat
  = Z ()
  | S Nat

type NatTree
  = Leaf ()
  | Node (NatTree, Nat, NatTree)

sum : Nat -> Nat -> Nat
sum n1 n2 =
  case n1 of
    Z _ -> n2
    S m -> S (sum m n2)

tree_count_nodes : NatTree -> Nat
tree_count_nodes tree =
  ??
