open Lang

type 'a t = 'a -> exp

let bool : bool t = function
  | true  -> EOp (True, [])
  | false -> EOp (False, [])

let rec int : int t = function
  | 0 -> EOp (Zero, [])
  | n -> EOp (Succ, [ int (n - 1) ])

let var : string t = fun x -> EVar x

let opt : 'a t -> 'a option t =
 fun da x_opt ->
  match x_opt with
  | None   -> EOp (No, [])
  | Some x -> EOp (Just, [ da x ])

let rec list : 'a t -> 'a list t =
 fun da -> function
  | []       -> EOp (Nil, [])
  | hd :: tl -> EOp (Cons, [ da hd; list da tl ])

let nested_list : 'a t -> 'a list list t = fun d -> list (list d)

let tree : 'a t -> 'a Tree2.t t =
 fun da ->
  let rec helper t =
    match t with
    | Tree2.Leaf -> EOp (Leaf, [])
    | Tree2.Node (left, x, right) ->
        EOp (Node, [ da x; helper left; helper right ])
  in
  helper

let args2 : 'a1 t -> 'a2 t -> ('a1 * 'a2) t =
 fun da1 da2 (x1, x2) -> ETuple [ da1 x1; da2 x2 ]

let args3 : 'a1 t -> 'a2 t -> 'a3 t -> ('a1 * 'a2 * 'a3) t =
 fun da1 da2 da3 (x1, x2, x3) -> ETuple [ da1 x1; da2 x2; da3 x3 ]

(* let rec poly_to_mono : exp -> exp = function (* Main cases *)

   | EApp (_, e1, EAType _) -> poly_to_mono e1

   | ECtor (ctor_name, [TData ("List", _)], arg) -> ECtor ("L" ^ ctor_name, [],
   poly_to_mono arg)

   | ECtor (ctor_name, _, arg) -> ECtor (ctor_name, [], poly_to_mono arg)

   (* Other cases *)

   | EFix (f, x, body) -> EFix (f, x, poly_to_mono body)

   | EApp (special, e1, EAExp e2) -> EApp (special, poly_to_mono e1, EAExp
   (poly_to_mono e2))

   | EVar x -> EVar x

   | ETuple components -> ETuple (List.map poly_to_mono components)

   | EProj (n, i, arg) -> EProj (n, i, poly_to_mono arg)

   | ECase (scrutinee, branches) -> ECase ( poly_to_mono scrutinee , List.map
   (Pair2.map_snd (Pair2.map_snd poly_to_mono)) branches )

   | EHole hole_name -> EHole hole_name

   | EAssert (e1, e2) -> EAssert (poly_to_mono e1, poly_to_mono e2)

   | ETypeAnnotation (e, tau) -> ETypeAnnotation (poly_to_mono e, tau)

   let poly : 'a t -> 'a -> exp = fun (da, _) x -> da x *)

let mono : 'a t -> 'a -> exp = fun d x -> d x
