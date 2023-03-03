open Core
open Util
include Lang

let count = ref 0

let new_count () =
  incr count;
  !count

type t = typ [@@deriving sexp, compare, hash]

let to_z3sort : t -> Solver.Sort.sort = function
  | TBool          -> Solver.bool_sort
  | TNum           -> Solver.int_sort
  | TList TBool    -> Solver.list_of_bool_sort
  | TList TNum     -> Solver.list_of_int_sort
  | TOpt TNum      -> Solver.opt_of_int_sort
  | TList (TVar x) -> (
      match !x with
      | Link TNum  -> Solver.list_of_int_sort
      | Link TBool -> Solver.list_of_bool_sort
      | _          -> failwith "to_z3sort tvar unimplemented")
  | ty             ->
      failwith
        (Printf.sprintf "%s unimplemented in solver"
           (Sexp.to_string (sexp_of_t ty)))

let to_z3const : t -> string -> Solver.Expr.expr = function
  | TBool       -> Solver.mk_bool
  | TNum        -> Solver.mk_int
  | TList TBool -> Solver.mk_list_of_bool
  | TList TNum  -> Solver.mk_list_of_int
  | TOpt TNum   -> Solver.mk_opt_of_int
  | TTree TNum  -> raise Solver.Unimplemented
  | ty          ->
      failwith
        (Printf.sprintf "%s unimplemented in solver"
           (Sexp.to_string (sexp_of_t ty)))

let typ_to_string = Fn.compose Sexp.to_string sexp_of_t

let to_list : t -> t list = function
  | TProd tys -> tys
  | ty        -> [ ty ]

let of_list : t list -> t = function
  | []         -> failwith "cannot transform empty typ list to a product type"
  | [ single ] -> single
  | exps       -> TProd exps

let arity : t -> int = function
  | TProd tys -> List.length tys
  | _         -> 1

let input_typ = function
  | TArr (ty, _) -> ty
  | _            -> failwith "cannot find the input type for a non-arrow type"

let output_typ = function
  | TArr (_, ty) -> ty
  | _            -> failwith "cannot find the output type for a non-arrow type"

let is_arr = function
  | TArr _ -> true
  | _      -> false

let rec is_relevant_to_tree = function
  | TProd tys -> List.exists tys ~f:is_relevant_to_tree
  | TTree _   -> true
  | _         -> false

exception Inconsistent of string

exception TypeError of string

let fresh_free level = TVar (ref (Free (new_count (), level)))

let normalize typ =
  let count = ref (-1) in
  let fresh_name () =
    incr count;
    "t" ^ Int.to_string !count
  in
  let rec norm ctx typ =
    match typ with
    | TNum | TBool | TVar { contents = Free _ } -> typ
    | TList t -> TList (norm ctx t)
    | TTree t -> TTree (norm ctx t)
    | TProd ts -> TProd (List.map ts ~f:(norm ctx))
    | TVar { contents = Link typ' } -> norm ctx typ'
    | TVar { contents = Quant name } -> (
        match Ctx.lookup ctx name with
        | Some name' -> TVar (ref (Quant name'))
        | None       ->
            let name' = fresh_name () in
            Ctx.update ctx name name';
            TVar (ref (Quant name')))
    | TArr (t_i, t_o) -> TArr (norm ctx t_i, norm ctx t_o)
    | TOpt t -> TOpt (norm ctx t)
  in
  norm (Ctx.empty ()) typ

let rec occurs id level typ =
  match typ with
  | TNum | TBool | TVar { contents = Quant _ } -> ()
  | TVar ({ contents = Free (id', level') } as typ') ->
      if id' = id then
        raise
        @@ TypeError
             (sprintf "Failed occurs check: ft%d in %s" id (typ_to_string typ))
      else if
        (* The other type is being claimed by the let binding, if it is owned by
           a lower let. This prevents the free variable from being prematurely
           generalized. *)
        level' > level
      then
        typ' := Free (id', level)
      else
        ()
  | TVar { contents = Link typ' } -> occurs id level typ'
  | TList t -> occurs id level t
  | TTree t -> occurs id level t
  | TProd ts -> List.iter ts ~f:(occurs id level)
  | TArr (t_i, t_o) ->
      occurs id level t_i;
      occurs id level t_o
  | TOpt t -> occurs id level t

(** The level is associated with the let expression that "owns" a particular
    free type variable. When that let expression is completely typed, its free
    type variables can be generalized. *)
let rec generalize level typ =
  match typ with
  | TVar { contents = Free (id, level') } when level' > level ->
      TVar (ref (Quant ("t" ^ Int.to_string id)))
  | TVar { contents = Link typ' } -> generalize level typ'
  | TArr (t_i, t_o) -> TArr (generalize level t_i, generalize level t_o)
  | TList t -> TList (generalize level t)
  | TTree t -> TTree (generalize level t)
  | TProd ts -> TProd (List.map ts ~f:(generalize level))
  | TOpt t -> TOpt (generalize level t)
  | TNum | TBool | TVar { contents = Quant _ } | TVar { contents = Free _ } ->
      typ

(** Instantiating a type replaces all quantified type variables with fresh free
    type variables. *)
let instantiate ?(ctx = Ctx.empty ()) level typ =
  let rec inst ctx typ =
    match typ with
    | TNum | TBool | TVar { contents = Free _ } -> typ
    | TVar { contents = Quant name } -> (
        match Ctx.lookup ctx name with
        | Some typ' -> typ'
        | None      ->
            let typ' = fresh_free level in
            Ctx.update ctx name typ';
            typ')
    | TVar { contents = Link typ' } -> inst ctx typ'
    | TArr (t_i, t_o) -> TArr (inst ctx t_i, inst ctx t_o)
    | TList t -> TList (inst ctx t)
    | TTree t -> TTree (inst ctx t)
    | TProd ts -> TProd (List.map ts ~f:(inst ctx))
    | TOpt t -> TOpt (inst ctx t)
  in
  inst ctx typ

let rec unify_exn t1 t2 =
  let error () =
    raise
    @@ TypeError
         (sprintf "Failed to unify %s and %s." (typ_to_string t1)
            (typ_to_string t2))
  in
  if compare t1 t2 = 0 then
    ()
  else
    match (t1, t2) with
    | TBool, TBool | TNum, TNum -> ()
    | TVar { contents = Link t1' }, t2' | t1', TVar { contents = Link t2' } ->
        unify_exn t1' t2'
    | TVar { contents = Free (id1, _) }, TVar { contents = Free (id2, _) }
      when id1 = id2 ->
        raise (TypeError "Free variable occurred in both types.")
    | TVar ({ contents = Free (id, level) } as t'), t
    | t, TVar ({ contents = Free (id, level) } as t') ->
        occurs id level t;
        t' := Link t
    | TArr (t1_i, t1_o), TArr (t2_i, t2_o) ->
        unify_exn t1_i t2_i;
        unify_exn t1_o t2_o
    | TList t1, TList t2 -> unify_exn t1 t2
    | TTree t1, TTree t2 -> unify_exn t1 t2
    | TOpt t1, TOpt t2 -> unify_exn t1 t2
    | TProd ts1, TProd ts2 -> (
        match List.zip ts1 ts2 with
        | Ok t1t2s        -> List.iter t1t2s ~f:(fun (t1, t2) -> unify_exn t1 t2)
        | Unequal_lengths -> error ())
    | _ -> error ()

let unify t1 t2 =
  try
    Some
      (unify_exn t1 t2;
       t1)
  with TypeError _ -> None

let is_unifiable t1 t2 =
  Option.is_some (unify (instantiate 0 t1) (instantiate 0 t2))

let equal t1 t2 = compare t1 t2 = 0
