type Boolean
  = F ()
  | T ()

type BooleanTree
  = Leaf ()
  | Node (BooleanTree, Boolean, BooleanTree)

type Nat
  = Z ()
  | S Nat

sum : Nat -> Nat -> Nat
sum n1 n2 =
  case n1 of
    Z _ -> n2
    S m -> S (sum m n2)

tree_count_leaves : BooleanTree -> Nat
tree_count_leaves tree =
  ??
