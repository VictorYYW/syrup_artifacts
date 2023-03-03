open MyStdLib
open Lang

type 'a t = 'a -> Expr.t list

type 'a expr_t = 'a -> Expr.t

let bool : bool expr_t = fun b ->
  if b then
    Expr.mk_ctor (Id.create "T") (Expr.mk_unit)
      else 
    Expr.mk_ctor (Id.create "F") (Expr.mk_unit)

let int : int expr_t = fun n -> Expr.from_int n

let var : string expr_t = fun x ->
  match x with
  (* | "isEven" -> Expr.mk_var (Id.create "is_even") *)
  (* | "isNonzero" -> Expr.mk_var (Id.create "is_nonzero") *)
  (* | "countOdd" -> Expr.mk_var (Id.create "count_odd") *)
  (* | "add" -> Expr.mk_var (Id.create "sum") *)
  (* | "isOdd" -> Expr.mk_var (Id.create "is_odd") *)
  | _ -> Expr.mk_var (Id.create x)

let opt : 'a expr_t -> 'a option expr_t =
 fun da x_opt ->
  match x_opt with
  | None   ->
    Expr.mk_ctor (Id.create "None") (Expr.mk_unit)
  | Some x ->
    Expr.mk_ctor (Id.create "Some") (Expr.mk_tuple [da x])

let rec list : 'a expr_t -> 'a list expr_t =
  fun da xs ->
  match xs with
  | [] ->
    Expr.mk_ctor (Id.create "Nil") (Expr.mk_unit)
  | x::xs -> 
    Expr.mk_ctor (Id.create "Cons") (Expr.mk_tuple [da x; list da xs])
    
let rec nested_list : 'a expr_t -> 'a list list expr_t = fun d -> function
  | [] -> 
    Expr.mk_ctor (Id.create "LNil") (Expr.mk_unit)
  | x::xs -> 
    Expr.mk_ctor (Id.create "LCons") (Expr.mk_tuple [list d x; nested_list d xs])

let tree : 'a expr_t -> 'a Tree2.t expr_t = fun (da) ->
  ( let rec helper t = match t with
        | Tree2.Leaf -> Expr.mk_ctor (Id.create "Leaf") (Expr.mk_unit)

        | Tree2.Node (left, x, right) ->
    Expr.mk_ctor (Id.create "Node") (Expr.mk_tuple [helper left; da x; helper right])
    in helper )

let arg1 : 'a expr_t -> 'a t =
  fun da a -> [da a]

let args2 : 'a1 expr_t -> 'a2 expr_t -> ('a1 * 'a2) t =
 fun da1 da2 (x1, x2) -> [ da1 x1; da2 x2 ]

let args3 : 'a1 expr_t -> 'a2 expr_t -> 'a3 expr_t -> ('a1 * 'a2 * 'a3) t =
 fun da1 da2 da3 (x1, x2, x3) -> [ da1 x1; da2 x2; da3 x3 ]

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
