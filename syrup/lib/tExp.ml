open Core
open Util
open Lang

type t = texp [@@deriving sexp, compare, hash]

let rec is_nat : texp -> bool = function
  | TEOp (Zero, [], _)    -> true
  | TEOp (Succ, [ e ], _) -> is_nat e
  | _                     -> false

let rec to_int : texp -> int = function
  | TEOp (Zero, [], _)    -> 0
  | TEOp (Succ, [ e ], _) -> to_int e + 1
  | _                     -> failwith "to_int expects expression of num type"

let rec to_exp : texp -> exp = function
  | TEVar (x, _) -> EVar x
  | TETuple (texps, _) -> ETuple (List.map texps ~f:to_exp)
  | TEOp (op, args, _) -> EOp (op, List.map args ~f:to_exp)
  | TEMatch (scrut, rules, _) ->
      EMatch (to_exp scrut, List.map rules ~f:branch_of_tbranch)
  | TELet (pat, t_opt, defn, body, _) ->
      ELet (pat, t_opt, to_exp defn, to_exp body)
  | TEFun (pat, body, _) -> EFun (pat, to_exp body)
  | TEFix (id, pat, body, _) -> EFix (id, pat, to_exp body)
  | TEApp (f, arg, _) -> EApp (to_exp f, to_exp arg)

and branch_of_tbranch (p, e) = (p, to_exp e)

let rec of_exp : typ -> exp -> texp =
 fun ty -> function
  | EVar x                    -> TEVar (x, ty)
  | EOp (ctor, [])            -> TEOp (ctor, [], ty)
  | EOp (Just, [ e ])         -> (
      match ty with
      | TOpt ty' -> TEOp (Just, [ of_exp ty' e ], ty)
      | _        -> failwith "type inconsistency: Some")
  | EOp (Succ, [ e ])         -> (
      match ty with
      | TNum -> TEOp (Succ, [ of_exp ty e ], ty)
      | _    -> failwith "type inconsistency: Succ")
  | EOp (Cons, [ hd; tl ])    -> (
      match ty with
      | TList ty' -> TEOp (Cons, [ of_exp ty' hd; of_exp ty tl ], ty)
      | _         -> failwith "type inconsistency: Cons")
  | EOp (Node, [ v; t1; t2 ]) -> (
      match ty with
      | TTree ty' ->
          TEOp (Node, [ of_exp ty' v; of_exp ty t1; of_exp ty t2 ], ty)
      | _         -> failwith "type inconsistency: Cons")
  | ETuple es                 -> (
      match ty with
      | TProd tys ->
          List.map2_exn tys es ~f:of_exp |> fun es' -> TETuple (es', ty)
      | _         -> failwith "type inconsistency: Tuple")
  | e                         ->
      failwith
        (Printf.sprintf "%s %s unimplemented" (Typ.typ_to_string ty)
           (Exp.to_string e))

let to_typ = function
  | TEVar (_, ty)
  | TETuple (_, ty)
  | TEOp (_, _, ty)
  | TEMatch (_, _, ty)
  | TELet (_, _, _, _, ty)
  | TEFun (_, _, ty)
  | TEFix (_, _, _, ty)
  | TEApp (_, _, ty) -> ty

(** take a tuple and return an expression list *)
let to_list : t -> t list = function
  | TETuple (es, _ty) -> es
  | e                 -> [ e ]

(** take a list of arguments and transform into one tuplized expression *)
let of_list : t list -> t = function
  | []         -> failwith
                    "cannot transform empty expression list to an expression"
  | [ single ] -> single
  | exps       ->
      let tys = List.map exps ~f:to_typ in
      TETuple (exps, Typ.of_list tys)

let is_arrow texpr =
  match to_typ texpr with
  | TArr _ -> true
  | _      -> false

let cost = Fn.compose Exp.cost to_exp
let size = cost

let rec eval (vctx : VCtx.t) (texp : texp) : texp =
  let eval_all (texps : texp list) : texp list =
    List.map texps ~f:(eval vctx)
  in
  match texp with
  | TEVar (x, ty) -> of_exp ty (Ctx.lookup_exn vctx x)
  | TETuple (texps, ty) -> TETuple (eval_all texps, ty)
  | TEOp (ctor, args, ty) -> TEOp (ctor, eval_all args, ty)
  | TEApp (TEVar (fun_name', ty_arr), arg, ty)
    when String.equal fun_name fun_name' -> (
      let ios =
        match VCtx.lookup_exn vctx fun_name with
        | EFPar ios -> ios
        | _         -> failwith "impossible"
      in
      List.find_map ios ~f:(fun (input, output) ->
          if Exp.equal input (to_exp arg) then
            Some output
          else
            None)
      |> function
      | Some o -> of_exp ty o
      | None   -> TEApp (TEVar (fun_name', ty_arr), eval vctx arg, ty))
  | TEApp
      ( (TEVar
           ( ( "equal_to"
             | "less_than"
             | "not"
             | "add"
             | "isEven"
             | "isNonzero"
             | "inc"
             | "zero"
             | "append"
             | "append_bool"
             | "snoc"
             | "countOdd"
             | "insert" ),
             _ ) as f),
        arg,
        ty ) -> TEApp (f, eval vctx arg, ty)
  | TEApp (TEVar (("fold" | "filter" | "map"), _), _, _) -> texp
  | TEApp (TEVar (id, ty_f), arg, ty) -> (
      match VCtx.lookup vctx id with
      | Some (EVar id) -> eval vctx (TEApp (TEVar (id, ty_f), arg, ty))
      | Some _ | None  ->
          failwith (Printf.sprintf "%s not found in texp_to_z3" id))
  | texp -> failwith (sprintf !"TExp.eval unimplemented on %{sexp:texp}\n" texp)
