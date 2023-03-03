type Nat
  = Z ()
  | S Nat

type Boolean
  = F ()
  | T ()

type Cmp
  = LT ()
  | EQ ()
  | GT ()

compare : Nat -> Nat -> Cmp
compare n1 n2 =
  case n1 of
    Z _ ->
      case n2 of
        Z _ -> EQ ()
        S _ -> LT ()
    S m1 ->
      case n2 of
        Z _  -> GT ()
        S m2 -> compare m1 m2

nat_max : Nat -> Nat -> Nat
nat_max m n =
  ??
