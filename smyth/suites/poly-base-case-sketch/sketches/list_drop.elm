type Nat
  = Z ()
  | S Nat

type List a
  = Nil ()
  | Cons (a, List a)

listDrop : forall a . List a -> Nat -> List a
listDrop <a> xs n =
  case n of
    Z _ ->
      xs

    S n_ ->
      ??
