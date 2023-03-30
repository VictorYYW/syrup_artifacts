open Core
open Util
include Lang
module VCtxSet = Set.Make (VCtx)

let ( let* ) o f =
  match o with
  | None -> None
  | Some x -> f x

(** Parameterized program space *)
module ParameterizedSpace = struct
  type mode = [ `SyRup | `Height ] [@@deriving sexp]

  type t = {
    mode : mode;
    atomicSize : int;
    matchDepth : int;
    ctorDepth : int;
    condWidth : int;
    totalHeight : int;
  }
  [@@deriving sexp]

  let empty mode =
    {
      mode;
      atomicSize = 0;
      matchDepth = 0;
      ctorDepth = 0;
      condWidth = 0;
      totalHeight = 0;
    }

  let ps_syrup atomicSize matchDepth condWidth =
    {
      mode = `SyRup;
      atomicSize;
      matchDepth;
      ctorDepth = 0;
      condWidth;
      totalHeight = 0;
    }

  let ps_height totalHeight = { (empty `Height) with totalHeight }

  let get_height ps =
    match ps.mode with
    | `SyRup -> failwith "get_height on SyRup mode"
    | `Height -> ps.totalHeight

  let get_enum_bound ps =
    match ps.mode with
    | `SyRup -> ps.atomicSize
    | `Height -> ps.totalHeight

  let get_var_index ps =
    match ps.mode with
    | `SyRup -> ps.matchDepth
    | `Height -> ps.totalHeight

  (** bound on `Trace` during intersection of VSes *)
  let get_cond_bound ps =
    match ps.mode with
    | `SyRup -> ps.condWidth
    | `Height -> 3

  let update action (ps : t) : t option =
    match ps.mode with
    | `SyRup -> (
        match action with
        | `Ctor ->
            { ps with ctorDepth = ps.ctorDepth - 1 }
            |> Option.some_if (ps.ctorDepth > 0)
        | `MatchBase ->
            { ps with matchDepth = ps.matchDepth - 1 }
            |> Option.some_if (ps.matchDepth > 0)
        | `MatchInd ->
            {
              ps with
              matchDepth = ps.matchDepth - 1;
              ctorDepth = ps.ctorDepth + 1;
            }
            |> Option.some_if (ps.matchDepth > 0)
        (* when extracting program from VS, SyRup also performs unification, we
           do not shrink the search space of predicates as we move towards lower
           level in the AST *)
        | `Walkdown -> Some ps)
    | `Height ->
        { ps with totalHeight = ps.totalHeight - 1 }
        |> Option.some_if (ps.totalHeight > 0)

  let gen : mode -> typ -> t list = function
    | `SyRup ->
        fun ty ->
          let input_ty = Typ.input_typ ty in
          let arity =
            Typ.to_list input_ty |> List.count ~f:(Fn.compose not Typ.is_arr)
          in
          let init_n =
            if Typ.is_relevant_to_tree input_ty then
              5
            else
              3
          in
          let allowance_n = 4 in
          let max_n =
            if Typ.is_relevant_to_tree input_ty then
              7
            else
              5
          in
          let init_d = 1 in
          let max_d = 2 in
          List.concat_map
            (List.range ~start:`inclusive ~stop:`inclusive init_d arity)
            ~f:(fun d ->
              List.map
                (List.range ~start:`inclusive ~stop:`inclusive init_n
                   (init_n + allowance_n - (2 * (d - init_d))))
                ~f:(fun n -> ps_syrup n d 1))
          @ List.map
              (List.range ~start:`inclusive ~stop:`inclusive init_d max_d)
              ~f:(fun d -> ps_syrup init_n d 2)
          @ List.concat_map
              (List.range ~start:`inclusive ~stop:`inclusive (arity + 1) max_d)
              ~f:(fun d ->
                List.map
                  (List.range ~start:`inclusive ~stop:`inclusive init_n
                     (init_n + allowance_n - (2 * (d - init_d))))
                  ~f:(fun n -> ps_syrup n d 1))
          @ List.map
              (List.range ~start:`inclusive ~stop:`inclusive init_d max_d)
              ~f:(fun d -> ps_syrup init_n d 3)
    | `Height ->
        fun _ ->
          List.map
            (List.range ~start:`inclusive ~stop:`inclusive 1 10)
            ~f:ps_height
end

module PS = ParameterizedSpace

module Example = struct
  type t = VCtx.t * Exp.t option * Exp.t [@@deriving sexp_of]

  let of_io sigma (i, o) : t = (sigma, Some i, o)
  let to_vctx : t -> VCtx.t = Tuple3.get1

  (** assuming the example represent a function, otherwise raise exception *)
  let to_input ex : Exp.t = Option.value_exn (Tuple3.get2 ex)

  let to_output : t -> Exp.t = Tuple3.get3
  let map_vctx = Tuple3.map_fst
  let map_output : t -> f:(Exp.t -> Exp.t) -> t = Tuple3.map_trd
  let is_func ex = ex |> snd3 |> Option.is_some
  let vctx_lookup_exn ex = VCtx.lookup_exn (to_vctx ex)

  let vctx_bind_alist ex bindings : t =
    map_vctx ex ~f:(fun sigma -> VCtx.bind_alist sigma bindings)

  let is_consistent (e : Exp.t) ((vctx, input_opt, output) : t) : bool =
    try
      Exp.eval ~fpar_error:true ~ctx:vctx
        (match input_opt with
        | Some input -> EApp (e, input)
        | None -> e)
      |> Exp.equal output
    with
    | Exp.TypeCheckError msg -> failwith msg
    | Exp.RuntimeError _ -> false
    | Ctx.UnboundError _ ->
        failwith
          (sprintf "unbound error in expression: %s"
             (Sexp.to_string (Exp.sexp_of_t e)))
end

module Examples = struct
  type t = Example.t list [@@deriving sexp_of]

  let is_empty = List.is_empty

  module Asm = struct
    module ExpMap = Map.Make (Exp)

    type t = Exp.t ExpMap.t [@@deriving sexp, compare]

    exception ConflictAssumption

    let is_empty : t -> bool = ExpMap.is_empty
    let empty : t = ExpMap.empty
    let singleton i o : t = ExpMap.singleton i o
    let of_alist : (exp * exp) list -> t = ExpMap.of_alist_exn

    let add ~key ~data asm : t =
      match ExpMap.add ~key ~data asm with
      | `Ok asm' -> asm'
      | `Duplicate ->
          let data' = ExpMap.find_exn asm key in
          if Exp.equal data data' then
            asm
          else
            raise ConflictAssumption

    let merge_exn : t -> t -> t =
      ExpMap.merge ~f:(fun ~key:k -> function
        | `Left v | `Right v -> Some v
        | `Both (e1, e2) ->
            if Exp.equal e1 e2 then
              Some e1
            else
              raise ConflictAssumption)

    let merge (asm1 : t) (asm2 : t) : t option =
      try Some (merge_exn asm1 asm2) with ConflictAssumption -> None

    let multi_merge asms : t option =
      try asms |> List.fold ~init:ExpMap.empty ~f:merge_exn |> Some
      with ConflictAssumption -> None

    let to_examples vctx (asm : t) =
      asm |> ExpMap.to_alist |> List.map ~f:(fun (i, o) -> (vctx, Some i, o))

    let to_alist = ExpMap.to_alist
  end

  let to_vctx_set (exs : t) : VCtxSet.t =
    List.map exs ~f:fst3 |> VCtxSet.of_list

  let num_of_ptbs = ref 5

  exception ExpectValues

  let function_check (examples : t) : bool =
    not
      (List.exists examples ~f:(fun (vctx, input, output) ->
           List.exists examples ~f:(fun (vctx', input', output') ->
               Ctx.equal Exp.equal vctx vctx'
               && Option.equal Exp.equal input input'
               && not (Exp.equal output output'))))

  exception NoExample

  (* TODO: check out Hoogle+ and others to see how the input type and output
     type are parametrized **)
  let infer (exs : t) : Typ.t =
    match exs with
    | [] -> raise NoExample
    | (_, None, output) :: _ ->
        failwith "call Examples.infer on examples without input"
    | (_, Some input, output) :: tl ->
        let ty_in, ty_out =
          List.fold tl
            ~init:(Exp.infer input, Exp.infer output)
            ~f:
              (fun (ty_in, ty_out) -> function
                | _, Some input, output ->
                    Typ.unify_exn ty_in (Exp.infer input);
                    Typ.unify_exn ty_out (Exp.infer output);
                    (ty_in, ty_out)
                | _, None, output ->
                    failwith "call Examples.infer on examples without input")
        in
        TArr (ty_in, ty_out)

  let is_consistent (e : Exp.t) (exs : t) : bool =
    try
      List.fold exs ~init:true ~f:(fun so_far (vctx, input_opt, output) ->
          so_far
          &&
          let output' =
            Exp.eval ~fpar_error:true ~ctx:vctx
              (match input_opt with
              | Some input -> EApp (e, input)
              | None -> e)
          in
          Exp.equal output' output)
    with
    | Exp.TypeCheckError msg -> failwith msg
    | Exp.RuntimeError _ -> false
    | Ctx.UnboundError _ ->
        failwith
          (sprintf "unbound error in expression: %s"
             (Sexp.to_string (Exp.sexp_of_t e)))
end

module rec VSA : sig
  type op =
    | Fix of string * pat
    | Fun of pat
    | MatchX of string * pat list
    | Op of Op.t
    | ITE of int

  val op_name : op -> string
  val apply : op -> exp list -> exp

  type t =
    | Empty
    (* atomic expression *)
    | Direct of TCtx.t * VCtxSet.t * exp
    | Union of VSASet.t
    | Join of TCtx.t * VCtxSet.t * op * t list
    (* two lemmas, may be proved by construction, i.e., the definition of intersect*)
    (* lemma1: any VSA in Trace is either Direct or Join *)
    (* lemma2: no VSAs in Trace can share the same top-level operator *)
    | Trace of VSASet.t
    | Top of Typ.t

  val of_exp : TCtx.t -> VCtxSet.t -> exp -> t
  val print : ?d:int -> t -> unit
  val compare : t -> t -> int
  val compare_op : op -> op -> int
  val sexp_of_t : t -> Sexp.t
  val t_of_sexp : Sexp.t -> t
  val is_empty : t -> bool
  val union : t -> t -> t
  val union_list : t list -> t
  val multi_union_fix : t list -> t
  val intersect : ?max_branches:int -> t -> t -> t
end = struct
  type op =
    | Fix of string * pat
    | Fun of pat (* TODO: introduce it during "learn"*)
    | MatchX of string * pat list
    | Op of Op.t
    (* arg of ITE indicates the number of branching *)
    | ITE of int
  [@@deriving sexp, compare]

  let op_name = function
    | Fix _ -> "fix"
    | Fun _ -> "fun"
    | MatchX _ -> "match"
    | Op op -> Op.str op
    | ITE _ -> "ite"

  let is_fix : op -> bool = function
    | Fix _ -> true
    | _ -> false

  exception JoinArityError

  let apply (op : op) (es : exp list) : exp =
    match (op, es) with
    | Fix (id, p), [ e ] -> EFix (id, p, e)
    | Fun p, [ e ] -> EFun (p, e)
    | MatchX (x, pats), es -> (
        match List.zip pats es with
        | Ok branches -> EMatch (EVar x, branches)
        | Unequal_lengths -> raise JoinArityError)
    | Op op, es -> EOp (op, es)
    | ITE 2, [ pred; e1; e2 ] -> EOp (Ite, [ pred; e1; e2 ])
    | ITE 3, [ pred1; pred2; e1; e2; e3 ] ->
        EOp (Ite, [ pred1; e1; EOp (Ite, [ pred2; e2; e3 ]) ])
    | _ -> raise JoinArityError

  type t =
    | Empty
    (* atomic expression *)
    | Direct of TCtx.t * VCtxSet.t * Exp.t
    | Union of VSASet.t
    | Join of TCtx.t * VCtxSet.t * op * t list
    (* two lemmas, may be proved by construction, i.e., the definition of intersect*)
    (* lemma1: any VSA in Trace is either Direct or Join *)
    (* lemma2: no VSAs in Trace can share the same top-level operator *)
    | Trace of VSASet.t
    | Top of Typ.t
  [@@deriving sexp, compare]

  let rec of_exp (gamma : TCtx.t) (sigmas : VCtxSet.t) (e : Exp.t) : t =
    let of_exp_all = List.map ~f:(of_exp gamma sigmas) in
    match e with
    | EVar _ | ETuple _ | EApp _ | EOp (_, []) -> Direct (gamma, sigmas, e)
    | EOp (op, es) -> Join (gamma, sigmas, Op op, of_exp_all es)
    | _ -> failwith (sprintf !"VSA.of_exp unimplemented: %{sexp:exp}" e)

  let print_space (d : int) : unit =
    List.range 0 d |> List.iter ~f:(fun _ -> print_string "  ")

  let rec print ?(d = 0) (vsa : t) : unit =
    Out_channel.flush stdout;
    match vsa with
    | Empty ->
        print_space d;
        print_string "Empty\n"
    | Direct (_, _, e) ->
        print_space d;
        print_s (Exp.sexp_of_t e)
    | Union set ->
        print_space d;
        print_string "Union (\n";
        VSASet.iter set ~f:(fun vsa -> print ~d:(d + 1) vsa);
        print_space d;
        print_string ")\n"
    | Join (_, _, op, vsas) ->
        print_space d;
        print_string "Join (";
        print_s (sexp_of_op op);
        List.iter vsas ~f:(fun vsa -> print ~d:(d + 1) vsa);
        print_space d;
        print_string ")\n"
    | Trace set ->
        print_space d;
        print_string "Trace (\n";
        VSASet.iter set ~f:(fun vsa -> print ~d:(d + 1) vsa);
        print_space d;
        print_string ")\n"
    | Top _ ->
        print_space d;
        print_string "Top\n"

  let compare_vctxset sigmas1 sigmas2 =
    let l1 = Set.length sigmas1 in
    let l2 = Set.length sigmas2 in
    let diff = l2 - l1 in
    if diff <> 0 then
      diff
    else
      VCtxSet.compare sigmas1 sigmas2

  let compare (vsa1 : t) (vsa2 : t) =
    match (vsa1, vsa2) with
    | Union set1, Union set2 | Trace set1, Trace set2 ->
        VSASet.compare set1 set2
    | Direct (_, sigmas1, e1), Direct (_, sigmas2, e2) ->
        let size_cmp = Int.compare (Exp.size e1) (Exp.size e2) in
        if size_cmp = 0 then
          if Exp.compare e1 e2 = 0 then
            -VCtxSet.compare sigmas1 sigmas2
          else
            Exp.compare e1 e2
        else
          size_cmp
    | Join (_, sigmas1, op1, vsas1), Join (_, sigmas2, op2, vsas2) ->
        if compare_op op1 op2 <> 0 then
          compare_op op1 op2
        else
          let cmp = (* Poly.compare *) compare_vctxset sigmas1 sigmas2 in
          if cmp = 0 then
            List.compare (* Poly.compare *) compare vsas1 vsas2
          else
            cmp
    (* | Join (_, _, op1, _), Join (_, _, op2, _) -> compare_op op1 op2 *)
    | Empty, Empty | Top _, Top _ -> 0
    | Empty, (Direct _ | Join _ | Union _ | Trace _ | Top _)
    | Direct _, (Join _ | Union _ | Trace _ | Top _)
    | Join _, (Union _ | Trace _ | Top _)
    | Union _, (Trace _ | Top _)
    | Trace _, Top _ -> -1
    | Top _, (Empty | Direct _ | Join _ | Union _ | Trace _)
    | Trace _, (Empty | Direct _ | Join _ | Union _)
    | Union _, (Empty | Direct _ | Join _)
    | Join _, (Empty | Direct _)
    | Direct _, Empty -> 1

  exception UnexpectedVSAInTrace

  (* TODO: collapse empty VSA *)
  let rec is_empty : t -> bool = function
    | Union vsas -> VSASet.for_all ~f:is_empty vsas
    | Trace vsas -> VSASet.for_all ~f:is_empty vsas
    | Join (_, _, _op, vsa_list) -> List.exists ~f:is_empty vsa_list
    | Direct _ | Top _ -> false
    | Empty -> true

  let union (vsa1 : t) (vsa2 : t) : t =
    match (vsa1, vsa2) with
    | Empty, vsa | vsa, Empty -> vsa
    | Top ty, _ | _, Top ty -> Top ty
    | Union set1, Union set2 -> Union (VSASet.union set1 set2)
    | ((Join _ | Direct _ | Trace _) as vsa), Union set
    | Union set, ((Join _ | Direct _ | Trace _) as vsa) ->
        Union (VSASet.add set vsa) (* Union (add set vsa) *)
    | (Join _ | Direct _ | Trace _), Trace _ | Trace _, (Join _ | Direct _) ->
        Union (VSASet.of_list [ vsa1; vsa2 ])
    | Join (gamma1, sigmas1, op1, vsas1), Join (gamma2, sigmas2, op2, vsas2) ->
        Union (VSASet.of_list [ vsa1; vsa2 ])
    | Direct (gamma1, sigmas1, e1), Direct (gamma2, sigmas2, e2) ->
        Union (VSASet.of_list [ vsa1; vsa2 ])
    (* Join and Direct won't overlap as their top-level operators are disjoint,
       so we simply build a set from them *)
    | Join _, Direct _ | Direct _, Join _ ->
        Union (VSASet.of_list [ vsa1; vsa2 ])

  let union_list : t list -> t = List.fold ~init:Empty ~f:union

  let multi_union_fix (vsas : t list) : t =
    List.filter vsas ~f:(function
      | Empty -> false
      | _ -> true)
    |> function
    | [] -> Empty
    | Join (gamma, _sigmas, (Fix _ as op), _) :: tl as vsas ->
        let sigmas =
          List.map vsas ~f:(function
            | Join (_, sigmas, (Fix _ as op), _) -> sigmas
            | _ -> failwith "multi_union_fix sigmas wrong usage")
          |> VCtxSet.union_list
        in
        let vsa_body =
          List.fold vsas ~init:VSASet.empty ~f:(fun set -> function
            | Join (_, _, (Fix _ as op), [ vsa_body ]) -> (
                match vsa_body with
                | Union set' -> VSASet.union set set'
                (* assert ( *)
                (*   VSASet.length set + VSASet.length set' *)
                (*   = VSASet.length new_set); *)
                | _ ->
                    VSASet.add set vsa_body
                    (* assert (VSASet.length set + 1 = VSASet.length
                       new_set); *))
            | _ -> failwith "multi_union_fix vsa_body wrong usage")
          |> VSA.Union
        in
        Join (gamma, sigmas, op, [ vsa_body ])
    | vsa :: tl ->
        failwith
          (Printf.sprintf "multi_union_fix wrong usage: %s"
             (Sexp.to_string (sexp_of_t vsa)))

  (* match tvsa_list with *)
  (* | [] -> failwith "VSA.join on empty vsa list" *)
  (* | (gamma, _, _)::_ -> *)
  (*   let sigma = List. *)

  let rec intersect ?(max_branches = 3) (vsa1 : t) (vsa2 : t) : t =
    match (vsa1, vsa2) with
    | Empty, _ | _, Empty -> Empty
    | Top _, vsa | vsa, Top _ -> vsa
    | Direct (gamma1, sigmas1, e1), Direct (gamma2, sigmas2, e2) ->
        if Exp.compare e1 e2 = 0 then
          Direct (gamma1, VCtxSet.union sigmas1 sigmas2, e1)
        else if max_branches = 1 then
          Empty
        else
          Trace (VSASet.of_list [ vsa1; vsa2 ])
    (* Join and Direct won't overlap as their top-level operators are disjoint,
       so we simply unify them within a Trace *)
    | Join _, Direct _ | Direct _, Join _ ->
        if max_branches = 1 then
          Empty
        else
          Trace (VSASet.of_list [ vsa1; vsa2 ])
    | Join (gamma1, sigmas1, op1, vsas1), Join (gamma2, sigmas2, op2, vsas2) ->
        let get_fpar sigmas =
          VCtxSet.fold sigmas ~init:[] ~f:(fun acc sigma ->
              (match VCtx.lookup sigma fun_name with
              | Some (EFPar defn) -> defn
              | Some _ -> failwith "recursive function's value is not FPar"
              | None -> failwith "get_fpar on non-fpar")
              @ acc)
        in
        if
          (not (is_fix op1))
          && List.exists (get_fpar sigmas1) ~f:(fun (i, o) ->
                 List.Assoc.find ~equal:Exp.equal (get_fpar sigmas2) i
                 |> Option.map ~f:(fun o' -> not (Exp.equal o o'))
                 |> fun opt -> Option.is_some opt && Option.value_exn opt)
        then
          Empty
        else if compare_op op1 op2 = 0 then
          match
            List.map2 vsas1 vsas2 ~f:(fun vsa1' vsa2' ->
                intersect ~max_branches vsa1' vsa2')
          with
          | Ok vsas' -> Join (gamma1, VCtxSet.union sigmas1 sigmas2, op1, vsas')
          | Unequal_lengths -> raise JoinArityError
        else if max_branches = 1 then
          Empty
        else
          Trace (VSASet.of_list [ vsa1; vsa2 ])
    (* TODO: ? taking intersection between VSAs may result in loss of "join
       match" when they share the same sigmas *)
    | Union set, vsa | vsa, Union set ->
        Union
          (VSASet.fold set ~init:VSASet.empty ~f:(fun acc vsa' ->
               match intersect ~max_branches vsa vsa' with
               | Empty -> acc
               | vsa' -> VSASet.add acc vsa'))
    (* compared to Union case in "union", duality between Trace and Union? *)
    | Trace set, ((Join _ | Direct _) as vsa)
    | ((Join _ | Direct _) as vsa), Trace set ->
        let vsa_set = aux ~max_branches set vsa in
        if VSASet.length vsa_set > max_branches || VSASet.mem vsa_set Empty then
          Empty
        else
          Trace vsa_set
    | Trace set1, Trace set2 ->
        let vsa_set = VSASet.fold set2 ~init:set1 ~f:(aux ~max_branches) in
        if VSASet.length vsa_set > max_branches || VSASet.mem vsa_set Empty then
          Empty
        else
          Trace vsa_set

  and aux ~max_branches (set : VSASet.t) (vsa : t) : VSASet.t =
    match vsa with
    | Join (_, _, op, _) -> (
        match
          VSASet.find set ~f:(function
            | Join (_, _, op', _) -> compare_op op op' = 0
            | _ -> false)
        with
        | Some vsa' ->
            (set |> fun set -> VSASet.remove set vsa') |> fun set ->
            VSASet.add set (intersect ~max_branches vsa vsa')
        | None -> VSASet.add set vsa)
    | Direct (_, _, e) -> (
        match
          VSASet.find set ~f:(function
            | Direct (_, _, e') -> compare_exp e e' = 0
            | _ -> false)
        with
        | Some vsa' ->
            (set |> fun set -> VSASet.remove set vsa') |> fun set ->
            VSASet.add set (intersect ~max_branches vsa vsa')
        | None -> VSASet.add set vsa)
    | _ -> failwith "impossible"
end

and VSASet : (Set.S with type Elt.t = VSA.t) = Set.Make (VSA)

module TypMemoizer = Sstream.Memoizer (Typ) (TExp)

module Result = struct
  module AsmMap = Map.Make (Examples.Asm)

  type t = VSA.t AsmMap.t [@@deriving sexp]

  (* let empty : t = AsmMap.singleton Examples.Asm.empty VSA.Empty *)

  let empty : t = AsmMap.empty

  (** TODO: add a Variant Top : typ -> Result.t *)
  let top (ty : Typ.t) : t = AsmMap.singleton Examples.Asm.empty (VSA.Top ty)

  let singleton : Examples.Asm.t -> VSA.t -> t = AsmMap.singleton

  let to_alist result =
    if AsmMap.mem result Examples.Asm.empty then
      AsmMap.to_alist ~key_order:`Decreasing result
    else
      (Examples.Asm.empty, VSA.Empty) :: AsmMap.to_alist result

  let add (result : t) (asm : Examples.Asm.t) (vsa : VSA.t) : t =
    (* asm is not bound infers that its VSA is empty *)
    AsmMap.update result asm ~f:(function
      | Some vsa0 -> VSA.union vsa0 vsa
      | None -> vsa)

  let union (results : t list) : t =
    results
    |> List.fold ~init:empty ~f:(fun acc result ->
           (* Map.merge acc result ~f:(fun ~key -> function *)
           (*   | `Both (vsa1, vsa2)     -> Some (VSA.union vsa1 vsa2) *)
           (*   | `Left vsa | `Right vsa -> Some vsa) *)
           Map.merge_skewed result acc ~combine:(fun ~key:_ -> VSA.union))

  let join gamma sigmas (op : VSA.op) (results : t list) : t =
    results |> List.map ~f:to_alist |> List.cprod
    |> List.filter_map ~f:(fun results ->
           let asms = List.map results ~f:fst in
           let vsas = List.map results ~f:snd in
           asms |> Examples.Asm.multi_merge
           |> Option.map ~f:(fun asm ->
                  (asm, VSA.Join (gamma, sigmas, op, vsas))))
    |> AsmMap.of_alist_reduce ~f:VSA.union

  let get_concret (result : t) : VSA.t option =
    AsmMap.find result Examples.Asm.empty
end

let z3var = "x"

(** raise error if recursive function "f" would be applied to different input.
    Typ.t is the return type of "f", Exp.t is the input *)
let rec texp_to_z3 (texp : texp) :
    (exp * Solver.Expr.expr) list * Solver.Expr.expr =
  match texp with
  | TEOp ((Zero | Succ), _, _) when TExp.is_nat texp ->
      ([], Solver.mk_numeral_i (TExp.to_int texp))
  | TEApp (TEVar (fun_name', _), arg, ty) when String.equal fun_name fun_name'
    ->
      let arg : exp = TExp.to_exp arg in
      let hole =
        Typ.to_z3const ty (sprintf "%s" (Sexp.to_string_mach (sexp_of_exp arg)))
      in
      ([ (arg, hole) ], hole)
  | TEApp
      ( TEVar
          ( (( "equal_to"
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
             | "insert"
             | "div2" ) as id),
            _ ),
        TETuple (args, _),
        ty ) ->
      apply_op
        (let open Solver in
        match id with
        | "equal_to" -> mk_eq
        | "less_than" -> mk_lt
        | "not" -> mk_not
        | "add" -> mk_add
        | "isEven" -> mk_is_even
        | "isNonzero" -> mk_is_nonzero
        | "inc" -> mk_succ
        | "zero" -> mk_zero
        | "append" -> mk_append
        | "append_bool" -> mk_append_bool
        | "snoc" -> mk_snoc
        | "countOdd" -> mk_count_odd
        | "insert" -> mk_insert
        | "div2" -> mk_div2
        | _ -> failwith "impossible")
        args
  | TEApp
      ( TEVar
          ( (( "equal_to"
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
             | "insert"
             | "div2" ) as id),
            _ ),
        arg,
        ty ) ->
      apply_op
        (let open Solver in
        match id with
        | "equal_to" -> mk_eq
        | "less_than" -> mk_lt
        | "not" -> mk_not
        | "add" -> mk_add
        | "isEven" -> mk_is_even
        | "isNonzero" -> mk_is_nonzero
        | "inc" -> mk_succ
        | "zero" -> mk_zero
        | "append" -> mk_append
        | "append_bool" -> mk_append_bool
        | "snoc" -> mk_snoc
        | "countOdd" -> mk_count_odd
        | "insert" -> mk_insert
        | "div2" -> mk_div2
        | _ -> failwith "impossible")
        [ arg ]
  | TEApp (TEVar (("fold" | "filter" | "map"), _), _, _) ->
      raise Solver.Unimplemented
  | TEOp (op, args, ty) ->
      apply_op
        (let open Solver in
        match op with
        | True -> mk_true
        | False -> mk_false
        | Zero -> mk_zero
        | Succ -> mk_succ
        | No when Poly.( = ) ty (TOpt TNum) -> mk_none_int
        | Just when Poly.( = ) ty (TOpt TNum) -> mk_some_int
        | Nil -> mk_nil (Typ.to_z3sort ty)
        | Cons -> mk_cons (Typ.to_z3sort ty)
        | Leaf | Node | No | Just | Ite -> mk_unimplemented)
        args
  | texp ->
      failwith
        (Printf.sprintf "texp_to_z3 unimplemented on %s\n"
           (Sexp.to_string (TExp.sexp_of_t texp)))

and apply_op (app : Solver.Expr.expr list -> Solver.Expr.expr)
    (args : TExp.t list) : (exp * Solver.Expr.expr) list * Solver.Expr.expr =
  let holes_expr_pairs = args |> List.map ~f:texp_to_z3 in
  (* only need to keep track of function calls with distinct inputs, as function
     call on the same input should evaluate to the same output *)
  let holes' =
    List.map holes_expr_pairs ~f:fst
    |> List.concat
    |> List.dedup_and_sort ~compare:(fun (e1, _) (e2, _) -> Exp.compare e1 e2)
  in
  let expr' = app @@ List.map holes_expr_pairs ~f:snd in
  (holes', expr')

module TExpPair = struct
  type t = TExp.t * TExp.t [@@deriving sexp_of, compare, hash]
end

(** `output`, the value of `texp`; `texp`, an atomic expression without free var
    but contain recursive function calls; return all the possible evaluation of
    the recursive function calls *)
let inverse_eval ~hole_typ : texp -> texp -> (Exp.t * Exp.t) list list =
  let memo f =
    let cache = Hashtbl.create (module TExpPair) ~size:20 in
    fun output texp ->
      match Hashtbl.find cache (output, texp) with
      | None ->
          let res = f output texp in
          Hashtbl.add_exn cache ~key:(output, texp) ~data:res;
          res
      | Some res -> res
  in
  let aux output texp =
    (* printf "%s\n" (Solver.Solver.to_string !Solver.solver); *)
    match texp with
    | TEApp (TEVar (fun_name', _), arg, ty) when String.equal fun_name fun_name'
      -> [ [ (TExp.to_exp arg, TExp.to_exp output) ] ]
    | _ -> (
        try
          Solver.push ();
          let arg_const_pairs, texp_z3 = texp_to_z3 texp in
          let _, output_z3 = texp_to_z3 output in
          Solver._assert @@ Solver.mk_eq [ texp_z3; output_z3 ];
          (match hole_typ with
          | TNum ->
              List.iter arg_const_pairs ~f:(fun (_, const) ->
                  Solver._assert @@ Solver.mk_ge [ const; Solver.mk_zero [] ])
          | _ -> ());
          (* let assertions = *)
          (*   Solver.mk_eq [ texp_z3; output_z3 ] *)
          (*   :: *)
          (*   (match hole_typ with *)
          (*   | TNum -> *)
          (*       List.map arg_const_pairs ~f:(fun (_, const) -> *)
          (*           Solver.mk_ge [ const; Solver.mk_zero [] ]) *)
          (*   | _    -> []) *)
          (* in *)
          let results =
            Sequence.unfold ~init:() ~f:(fun () ->
                (* List.iter assertions ~f:(fun assertion -> *)
                (*     printf "assert:%s\n" (Solver.Expr.to_string assertion)); *)
                match Solver.check_sat () with
                | UNSATISFIABLE -> None
                | UNKNOWN -> failwith "solver unknown"
                | SATISFIABLE ->
                    Some
                      ( List.fold arg_const_pairs ~init:[]
                          ~f:(fun rec_assumptions (input0, const_z3) ->
                            let output0_z3 =
                              Solver.Model.get_const_interp_e
                                (Solver.get_model ()) const_z3
                              |> Option.value_exn
                            in
                            (* printf "z3val:%s\n" *)
                            (*   (Solver.Expr.to_string output0_z3); *)
                            (* let output0 = Exp.of_z3expr hole_typ output0_z3
                               in *)
                            Solver._assert
                            @@ Solver.mk_neq [ const_z3; output0_z3 ];
                            (input0, output0_z3) :: rec_assumptions),
                        () ))
            |> Sequence.to_list
          in
          Solver.pop ();
          List.map results ~f:(fun result ->
              List.map result ~f:(fun (input0, output0_z3) ->
                  (input0, Exp.of_z3expr hole_typ output0_z3)))
        with
        | Solver.Unimplemented ->
            Solver.pop ();
            []
        | Solver.NoModelExists ->
            Solver.pop ();
            print_s (TExp.sexp_of_t texp);
            print_string "no model exists\n";
            [])
  in
  memo aux

let enumerate ?(allow_asm = true) (gamma : TCtx.t) (exs : Examples.t)
    (ty : Typ.t) (ps : PS.t) : Result.t =
  let sigmas = Examples.to_vctx_set exs in
  let default_init : TExp.t list =
    [
      TEOp (No, [], TOpt (Typ.fresh_free 0));
      TEOp (Zero, [], TNum);
      TEOp (True, [], TBool);
      TEOp (False, [], TBool);
      TEOp (Nil, [], TList (Typ.fresh_free 0));
      TEOp (Leaf, [], TTree (Typ.fresh_free 0));
    ]
  in

  (* copied from `enumerate' from search.ml, build a matrix with row index
     denoting size of expressions contained in the row *)
  let rec enum ?(memo = TypMemoizer.empty ()) (init : TExp.t list) (typ : Typ.t)
      : TExp.t Sstream.matrix =
    let open Sstream in
    (* copied from `flattened_matrix_of_texps_typ' from search.ml *)
    let init_matrix : TExp.t matrix =
      [
        List.filter init ~f:(fun texp ->
            Typ.is_unifiable typ (TExp.to_typ texp));
      ]
      |> Sstream.of_list
    in
    let rec_args_matrix (arg_typs : Typ.t list) : TExp.t list matrix =
      let arity = List.length arg_typs in
      let choose ((prev_args, if_dec) : TExp.t list * bool) :
          (TExp.t list * bool) matrix =
        let cur = List.length prev_args in
        let current_typ : Typ.t = List.nth_exn arg_typs cur in
        slazy (fun () ->
            map_matrix
              (let dec_vars =
                 TCtx.filter_typ_spec gamma current_typ (TCtx.Dec cur)
                 |> TCtx.to_alist
                 |> List.map ~f:(fun (id, _) -> (TEVar (id, current_typ), true))
               in
               let arg_vars =
                 TCtx.filter_typ_spec gamma current_typ (TCtx.Arg cur)
                 |> TCtx.to_alist
                 |> List.map ~f:(fun (id, _) ->
                        (TEVar (id, current_typ), false))
               in
               (* dec_vars *)
               (if cur + 1 = arity && not if_dec then
                 dec_vars
               else
                 dec_vars @ arg_vars)
               |> (* fun x ->
                   * Printf.printf "%s\n"
                   *   (Sexp.to_string
                   *      (sexp_of_list (sexp_of_pair TExp.sexp_of_t sexp_of_bool) x)); *)
               singleton)
              ~f:(fun (arg, if_dec') ->
                (prev_args @ [ arg ], if_dec || if_dec')))
      in
      (* equivalent to choose^{num_args-1} [] *)
      match arg_typs with
      | _ :: xs ->
          ([], false)
          |> List.fold xs ~init:choose ~f:(fun acc _ ->
                 compose ~mode:ps.mode acc choose)
          |> map_matrix ~f:(fun (e, if_dec) ->
                 assert if_dec;
                 e)
      | [] -> failwith "Need larger than 0 arguments to generate args matrix"
    in
    let args_matrix (arg_typs : Typ.t list) : TExp.t list matrix =
      let choose (prev_args : TExp.t list) : TExp.t list matrix =
        let current_typ : Typ.t =
          List.nth_exn arg_typs (List.length prev_args)
        in
        map_matrix
          (TypMemoizer.get memo current_typ (fun () ->
               enum ~memo init current_typ))
          (* (enum ~memo init current_typ) *)
          ~f:(fun arg -> prev_args @ [ arg ])
      in
      (* equivalent to choose^{num_args-1} [] *)
      match arg_typs with
      | _ :: xs ->
          []
          |> List.fold xs ~init:choose ~f:(fun acc _ ->
                 compose ~mode:ps.mode acc choose)
      | [] -> failwith "Need larger than 0 arguments to generate args matrix"
    in
    let callable_matrix cost apply typ is_rec =
      match typ with
      | TArr (arg_typ, ret_typ) ->
          let arg_typs = Typ.to_list arg_typ in
          let num_args = List.length arg_typs in
          let num_fun_args =
            List.count arg_typs ~f:(function
              | TArr _ -> true
              | _ -> false)
          in
          let prefix =
            repeat_n
              (match ps.mode with
              | `SyRup -> cost + num_args - num_fun_args - 1
              | `Height -> 1)
              []
          in
          let matrix =
            slazy (fun () ->
                map_matrix
                  ((if is_rec then
                     rec_args_matrix
                   else
                     args_matrix)
                     arg_typs)
                  ~f:(apply ret_typ))
          in
          concat prefix matrix
      | _ ->
          failwith
            (sprintf "Cannot generate callable matrix given none-arrow type: %s"
               (Sexp.to_string (Typ.sexp_of_t typ)))
    in
    let op_matrix op op_typ =
      callable_matrix (Op.cost op)
        (fun ret_typ args -> TEOp (op, args, ret_typ))
        op_typ false
    in
    let apply_matrix func func_typ is_rec =
      callable_matrix 1
        (fun ret_typ args -> TEApp (func, TExp.of_list args, ret_typ))
        func_typ is_rec
    in
    let op_matrices : TExp.t matrix list =
      List.map Op.constructors ~f:(fun op -> (op, Op.typ op))
      |> List.filter ~f:(fun (_op, op_typ) ->
             match op_typ with
             | TArr (_, ret_typ) -> Typ.is_unifiable typ ret_typ
             | _ -> false)
      |> List.map ~f:(fun (op, op_typ) ->
             match (Typ.instantiate 0 typ, Typ.instantiate 0 op_typ) with
             | typ', (TArr (_, ret_typ) as op_typ') ->
                 Typ.unify_exn typ' ret_typ;
                 (op, Typ.normalize (Typ.generalize (-1) op_typ'))
             | _ -> failwith "op should have arrow type")
      |> List.map ~f:(fun (op, op_typ) -> op_matrix op op_typ)
    in
    let apply_matrices : TExp.t matrix list =
      init
      |> List.filter ~f:(fun texpr ->
             match TExp.to_typ texpr with
             | TArr (_, ret_typ) -> Typ.is_unifiable typ ret_typ
             | _ -> false)
      |> List.map ~f:(fun texpr ->
             match
               (Typ.instantiate 0 typ, Typ.instantiate 0 (TExp.to_typ texpr))
             with
             | typ', (TArr (_, ret_typ) as func_typ) ->
                 Typ.unify_exn typ' ret_typ;
                 (texpr, Typ.normalize (Typ.generalize (-1) func_typ))
             | _ -> failwith "function should have arrow type")
      |> List.map ~f:(fun (func, func_typ) -> apply_matrix func func_typ false)
    in
    let apply_rec_matrices : TExp.t matrix list =
      TCtx.filter_spec gamma TCtx.Rec
      |> TCtx.to_alist
      |> List.filter ~f:(fun (id, (func_typ, _rec)) ->
             match func_typ with
             | TArr (_, ret_typ) -> Typ.is_unifiable typ ret_typ
             | _ -> false)
      |> List.map ~f:(fun (id, (func_typ, _rec)) ->
             match (Typ.instantiate 0 typ, Typ.instantiate 0 func_typ) with
             | typ', (TArr (_, ret_typ) as func_typ) ->
                 Typ.unify_exn typ' ret_typ;
                 ( TEVar (id, func_typ),
                   Typ.normalize (Typ.generalize (-1) func_typ) )
             | _ -> failwith "function should have arrow type")
      |> List.map ~f:(fun (func, func_typ) -> apply_matrix func func_typ true)
    in
    merge (init_matrix :: (op_matrices @ apply_matrices @ apply_rec_matrices))
    (* |> map *)
    (*      ~f: *)
    (*        (List.filter ~f:(fun texp -> *)
    (*             let e = TExp.to_exp texp in *)
    (*             let e' = Rewrite.simplify e in *)
    (*             Exp.cost e' >= Exp.cost e)) *)
  in

  let check_consistency (texp : TExp.t) : (Exp.t * Examples.Asm.t list) option =
    let exp = TExp.to_exp texp in
    (* eprintf !"%{sexp:exp}\n" exp; *)
    (* let s = Exp.to_string exp in *)
    try
      if Examples.is_consistent exp exs then
        (* print_string "consistent\n"; *)
        Some (exp, [ Examples.Asm.empty ])
      else
        (* print_string "inconsistent\n"; *)
        None
    with Exp.PartialFunction _ -> (
      if not allow_asm then
        None
      else
        let vctx, output =
          match exs with
          | [ (vctx, None, output) ] -> (vctx, output)
          | _ -> failwith "expect exactly one example when allow_asm is enabled"
        in
        let recfun_ret_typ =
          TCtx.lookup_exn gamma fun_name |> fst |> Typ.output_typ
        in
        let assumption_set =
          texp |> TExp.eval vctx
          |> inverse_eval ~hole_typ:recfun_ret_typ (TExp.of_exp ty output)
        in
        (* printf !"exp:%{sexp:exp}\n" exp; *)
        (* printf !"output:%{sexp:exp}\n" output; *)
        (* List.iter assumption_set ~f:(fun asm -> *)
        (*     (\* printf !"%{sexp:((exp * exp) list)}\n" asm; *\) *)
        (*     let asm = *)
        (*       match VCtx.get_fpar vctx with *)
        (*       | EFPar asm' -> asm @ asm' *)
        (*       | _          -> failwith "fpar" *)
        (*     in *)
        (*     let vctx' = VCtx.bind vctx fun_name (EFPar asm) in *)
        (*     assert (compare_exp (Exp.eval ~ctx:vctx' exp) output = 0)); *)
        (* printf !"inv:%{sexp:(exp * exp) list list}\n" assumption_set; *)
        match assumption_set with
        | [] -> None
        | _ -> Some (exp, List.map assumption_set ~f:Examples.Asm.of_alist))
  in

  let rec accum ?(acc = Result.empty) (n : int)
      (matrix : (Exp.t * Examples.Asm.t list) option Sstream.matrix) : Result.t
      =
    if n > 0 then
      let exps = Sstream.next matrix in
      let acc =
        List.fold exps ~init:acc ~f:(fun acc -> function
          | Some (exp, asm_list) ->
              (* printf !"%{sexp:exp}\n" exp; *)
              List.fold asm_list ~init:acc ~f:(fun acc asm ->
                  let sigmas' =
                    VCtxSet.map sigmas ~f:(fun sigma ->
                        match VCtx.lookup_exn sigma fun_name with
                        | EFPar defn ->
                            EFPar (Examples.Asm.to_alist asm @ defn)
                            |> VCtx.bind sigma fun_name
                        | _ -> failwith "impossible")
                  in
                  Result.add acc asm (VSA.of_exp gamma sigmas exp))
          | None -> acc)
      in
      accum ~acc (n - 1) matrix
    else
      acc
  in

  enum
    (default_init
    @ (TCtx.to_alist gamma
      |> List.filter_map ~f:(function
           | x, (ty, Rec) -> None
           | x, (ty, _) -> Some (TEVar (x, ty)))))
    ty
  |> Sstream.map_matrix ~f:check_consistency
  |> accum @@ PS.get_enum_bound ps

let rec intro_match (gamma : TCtx.t) (ex : Example.t) (ty : Typ.t) (ps : PS.t) :
    Result.t =
  let sigmas = ex |> Example.to_vctx |> VCtxSet.singleton in
  let get_result (ps_base, ps_ind) =
    gamma |> TCtx.to_alist
    |> List.filter_map ~f:(fun (x, ((typ, _) as typ_spec)) ->
           match typ_spec with
           | TTree elem_typ, (Arg i | Dec i) ->
               (* Printf.printf "%s, intro_match\n" x; *)
               let x_v = "v" ^ Int.to_string @@ PS.get_var_index ps in
               let x_l = "l" ^ Int.to_string @@ PS.get_var_index ps in
               let x_r = "r" ^ Int.to_string @@ PS.get_var_index ps in
               let gamma_leaf = TCtx.bind gamma x (typ, Matched i) in
               let gamma_node =
                 TCtx.bind_alist gamma
                   [
                     (x, (typ, Matched i));
                     (x_v, (elem_typ, Null));
                     (x_l, (typ, Dec i));
                     (x_r, (typ, Dec i));
                   ]
               in
               let result_leaf, result_node =
                 match Example.vctx_lookup_exn ex x with
                 | EOp (Leaf, []) ->
                     (learn gamma_leaf ex ty ps_base, Result.top ty)
                 | EOp (Node, [ v; l; r ]) ->
                     let ex' =
                       Example.vctx_bind_alist ex
                         [ (x_v, v); (x_l, l); (x_r, r) ]
                     in
                     (Result.top ty, learn gamma_node ex' ty ps_ind)
                 | e -> raise (Typ.Inconsistent (Exp.to_string e))
               in
               let result_match =
                 Result.join gamma sigmas
                   (MatchX (x, [ PLeaf; PNode (PVar x_v, PVar x_l, PVar x_r) ]))
                   [ result_leaf; result_node ]
               in
               Some result_match
           | TList elem_typ, (Arg i | Dec i) ->
               (* Printf.printf "%s, intro_match\n" x; *)
               let x_hd = "hd" ^ Int.to_string @@ PS.get_var_index ps in
               let x_tl = "tl" ^ Int.to_string @@ PS.get_var_index ps in
               let gamma_nil = TCtx.bind gamma x (typ, Matched i) in
               let gamma_cons =
                 TCtx.bind_alist gamma
                   [
                     (x, (typ, Matched i));
                     (x_hd, (elem_typ, Null));
                     (x_tl, (typ, Dec i));
                   ]
               in
               let result_nil, result_cons =
                 match Example.vctx_lookup_exn ex x with
                 | EOp (Nil, []) ->
                     (learn gamma_nil ex ty ps_base, Result.top ty)
                 | EOp (Cons, [ hd; tl ]) ->
                     let ex' =
                       Example.vctx_bind_alist ex [ (x_hd, hd); (x_tl, tl) ]
                     in
                     (Result.top ty, learn gamma_cons ex' ty ps_ind)
                 | e -> raise (Typ.Inconsistent (Exp.to_string e))
               in
               let result_match =
                 Result.join gamma sigmas
                   (MatchX (x, [ PNil; PCons (PVar x_hd, PVar x_tl) ]))
                   [ result_nil; result_cons ]
               in
               Some result_match
           | TNum, (Arg i | Dec i) ->
               let x_n = "n" ^ Int.to_string @@ PS.get_var_index ps in
               let gamma_zero = TCtx.bind gamma x (typ, Matched i) in
               let gamma_succ =
                 TCtx.bind_alist gamma
                   [ (x, (typ, Matched i)); (x_n, (typ, Dec i)) ]
               in
               let result_zero, result_succ =
                 match Example.vctx_lookup_exn ex x with
                 | EOp (Zero, []) ->
                     (learn gamma_zero ex ty ps_base, Result.top ty)
                 | EOp (Succ, [ pred ]) ->
                     let ex' = Example.vctx_bind_alist ex [ (x_n, pred) ] in
                     (Result.top ty, learn gamma_succ ex' ty ps_ind)
                 | e -> raise (Typ.Inconsistent (Exp.to_string e))
               in
               let result_match =
                 Result.join gamma sigmas
                   (MatchX (x, [ PZero; PSucc (PVar x_n) ]))
                   [ result_zero; result_succ ]
               in
               Some result_match
           | TBool, (Arg i | Dec i) ->
               let gamma_true = TCtx.bind gamma x (typ, Matched i) in
               let gamma_false = TCtx.bind gamma x (typ, Matched i) in
               let result_true, result_false =
                 match Example.vctx_lookup_exn ex x with
                 | EOp (True, []) ->
                     (learn gamma_true ex ty ps_base, Result.top ty)
                 | EOp (False, []) ->
                     (Result.top ty, learn gamma_false ex ty ps_base)
                 | e -> raise (Typ.Inconsistent (Exp.to_string e))
               in
               let result_match =
                 Result.join gamma sigmas
                   (MatchX (x, [ PTrue; PFalse ]))
                   [ result_true; result_false ]
               in
               Some result_match
           | _ -> None)
    |> Result.union
  in
  Option.both (PS.update `MatchBase ps) (PS.update `MatchInd ps)
  |> Option.value_map ~default:Result.empty ~f:get_result

and decompose (gamma : TCtx.t) (ex : Example.t) (ty : Typ.t) (ps : PS.t) :
    Result.t =
  let sigmas = ex |> Example.to_vctx |> VCtxSet.singleton in
  let get_result ps' =
    match ty with
    | TTree elem_typ -> (
        match Example.to_output ex with
        | EOp (Leaf, []) -> Result.empty
        | EOp (Node, [ v; l; r ]) ->
            let ex_v = Example.map_output ex ~f:(const v) in
            let ex_l = Example.map_output ex ~f:(const l) in
            let ex_r = Example.map_output ex ~f:(const r) in
            let result_v = learn gamma ex_v elem_typ ps' in
            let result_l = learn gamma ex_l ty ps' in
            let result_r = learn gamma ex_r ty ps' in
            Result.join gamma sigmas (VSA.Op Node)
              [ result_v; result_l; result_r ]
        | e -> raise (Typ.Inconsistent (Exp.to_string e)))
    | TList elem_typ -> (
        match Example.to_output ex with
        | EOp (Nil, []) -> Result.empty
        | EOp (Cons, [ hd; tl ]) ->
            let ex_hd = Example.map_output ex ~f:(const hd) in
            let ex_tl = Example.map_output ex ~f:(const tl) in
            let result_hd = learn gamma ex_hd elem_typ ps' in
            let result_tl = learn gamma ex_tl ty ps' in
            Result.join gamma sigmas (VSA.Op Cons) [ result_hd; result_tl ]
        | e -> raise (Typ.Inconsistent (Exp.to_string e)))
    | TNum -> (
        match Example.to_output ex with
        | EOp (Zero, []) -> Result.empty
        | EOp (Succ, [ pred ]) ->
            let ex_pred = Example.map_output ex ~f:(const pred) in
            let result_pred = learn gamma ex_pred TNum ps' in
            Result.join gamma sigmas (VSA.Op Succ) [ result_pred ]
        | e -> raise (Typ.Inconsistent (Exp.to_string e)))
    | TOpt elem_typ -> (
        match Example.to_output ex with
        | EOp (No, []) -> Result.empty
        | EOp (Just, [ e ]) ->
            let ex_elem = Example.map_output ex ~f:(const e) in
            let result_elem = learn gamma ex_elem elem_typ ps' in
            Result.join gamma sigmas (VSA.Op Just) [ result_elem ]
        | e -> raise (Typ.Inconsistent (Exp.to_string e)))
    | _ -> Result.empty
  in
  PS.update `Ctor ps |> Option.value_map ~default:Result.empty ~f:get_result

and learn ?(fpar = []) (gamma : TCtx.t) (ex : Example.t) (ty : Typ.t)
    (ps : PS.t) : Result.t =
  if
    Example.is_func ex
    &&
    (* ex is not consistent with examples in fpar *)
    match List.Assoc.find ~equal:Exp.equal fpar (Example.to_input ex) with
    | Some output' -> not (Exp.equal (Example.to_output ex) output')
    | None -> false
  then
    Result.empty
  else if Example.is_func ex then (
    let sigma = Example.to_vctx ex in
    let input = Example.to_input ex in
    let output = Example.to_output ex in
    let pat = Exp.to_pat input in

    (* construct typing context *)
    let emitted_gamma : (string * (Typ.t * TCtx.bindspec)) list =
      match (pat, Typ.input_typ ty) with
      | PVar x, ty_in -> [ (x, (ty_in, Arg 0)) ]
      | PTuple pats, TProd tys ->
          List.zip_exn pats tys
          |> List.mapi ~f:(fun i -> function
               | PVar x, ty_in -> (x, (ty_in, TCtx.Arg i))
               | _ -> failwith "impossible")
      | p, ty_in ->
          failwith
            (Printf.sprintf "%s %s"
               (Sexp.to_string (Pat.sexp_of_t p))
               (Sexp.to_string (Typ.sexp_of_t ty_in)))
    in
    let gamma_defn = TCtx.bind_alist gamma emitted_gamma in
    TCtx.update gamma_defn fun_name (ty, Rec);

    (* construct environment of the evaluation *)
    let partial_fun : Exp.t = EFPar ((input, output) :: fpar) in
    let emitted_sigma = Option.value_exn (Exp.pattern_match input pat) in
    let sigma_defn = TCtx.merge sigma emitted_sigma ~f:TCtx.shadow_duplicate in
    VCtx.update sigma_defn fun_name partial_fun;

    learn gamma_defn (sigma_defn, None, output) (Typ.output_typ ty) ps
    (* comment out rest as we prepend header at the end *)
    (* let vsa_defn = *)
    (*   learn gamma_defn (sigma_defn, None, output) (Typ.output_typ ty) ps *)
    (* in *)
    (* Result.join gamma (VCtxSet.singleton sigma) *)
    (*   (Fix (fun_name, pat)) *)
    (*   [ vsa_defn ] *)
  ) else
    let result_enum = enumerate gamma [ ex ] ty ps in
    let result_decompose = decompose gamma ex ty ps in
    let result_match = intro_match gamma ex ty ps in
    Result.union [ result_enum; result_decompose; result_match ]

let rec intro_match_pred (gamma : TCtx.t) (exs : Examples.t) (ty : Typ.t)
    (ps : PS.t) : Result.t =
  let sigmas = Examples.to_vctx_set exs in
  let get_result (ps_base, ps_ind) =
    gamma |> TCtx.to_alist
    |> List.filter_map ~f:(fun (x, ((typ, _) as typ_spec)) ->
           match typ_spec with
           | TTree elem_typ, (Arg i | Dec i) ->
               (* Printf.printf "%s, intro_match\n" x; *)
               let x_v = "v'" ^ Int.to_string @@ PS.get_var_index ps in
               let x_l = "l'" ^ Int.to_string @@ PS.get_var_index ps in
               let x_r = "r'" ^ Int.to_string @@ PS.get_var_index ps in
               let gamma_leaf = TCtx.bind gamma x (typ, Matched i) in
               let gamma_node =
                 TCtx.bind_alist gamma
                   [
                     (x, (typ, Matched i));
                     (x_v, (elem_typ, Null));
                     (x_l, (typ, Dec i));
                     (x_r, (typ, Dec i));
                   ]
               in
               (* split examples by inputs' constructors *)
               let exs_leaf, exs_node =
                 List.fold exs ~init:([], []) ~f:(fun (exs_leaf, exs_node) ->
                   function
                   | (sigma, None, output) as ex -> (
                       match VCtx.lookup_exn sigma x with
                       | EOp (Leaf, []) -> (ex :: exs_leaf, exs_node)
                       | EOp (Node, [ v; l; r ]) ->
                           let sigma_node =
                             VCtx.bind_alist sigma
                               [ (x_v, v); (x_l, l); (x_r, r) ]
                           in
                           (exs_leaf, (sigma_node, None, output) :: exs_node)
                       | e -> raise (Typ.Inconsistent (Exp.to_string e)))
                   | _ -> failwith "intro_match on a function synthesis problem")
               in
               let result_leaf = learn_pred gamma_leaf exs_leaf ty ps_base in
               let result_node = learn_pred gamma_node exs_node ty ps_ind in
               let result_match =
                 Result.join gamma sigmas
                   (MatchX (x, [ PLeaf; PNode (PVar x_v, PVar x_l, PVar x_r) ]))
                   [ result_leaf; result_node ]
               in
               Some result_match
           | TList elem_typ, (Arg i | Dec i) ->
               (* Printf.printf "%s, intro_match\n" x; *)
               let x_hd = "hd'" ^ Int.to_string @@ PS.get_var_index ps in
               let x_tl = "tl'" ^ Int.to_string @@ PS.get_var_index ps in
               let gamma_nil = TCtx.bind gamma x (typ, Matched i) in
               let gamma_cons =
                 TCtx.bind_alist gamma
                   [
                     (x, (typ, Matched i));
                     (x_hd, (elem_typ, Null));
                     (x_tl, (typ, Dec i));
                   ]
               in
               (* split examples by inputs' constructors *)
               let exs_nil, exs_cons =
                 List.fold exs ~init:([], []) ~f:(fun (exs_nil, exs_cons) ->
                   function
                   | (sigma, None, output) as ex -> (
                       match VCtx.lookup_exn sigma x with
                       | EOp (Nil, []) -> (ex :: exs_nil, exs_cons)
                       | EOp (Cons, [ hd; tl ]) ->
                           let sigma_cons =
                             VCtx.bind_alist sigma [ (x_hd, hd); (x_tl, tl) ]
                           in
                           (exs_nil, (sigma_cons, None, output) :: exs_cons)
                       | e -> raise (Typ.Inconsistent (Exp.to_string e)))
                   | _ -> failwith "intro_match on a function synthesis problem")
               in
               let result_nil = learn_pred gamma_nil exs_nil ty ps_base in
               let result_cons = learn_pred gamma_cons exs_cons ty ps_ind in
               let result_match =
                 Result.join gamma sigmas
                   (MatchX (x, [ PNil; PCons (PVar x_hd, PVar x_tl) ]))
                   [ result_nil; result_cons ]
               in
               Some result_match
           | TNum, (Arg i | Dec i) ->
               let x_n = "n'" ^ Int.to_string @@ PS.get_var_index ps in
               let gamma_zero = TCtx.bind gamma x (typ, Matched i) in
               let gamma_succ =
                 TCtx.bind_alist gamma
                   [ (x, (typ, Matched i)); (x_n, (typ, Dec i)) ]
               in
               (* split examples by inputs' constructors *)
               let exs_zero, exs_succ =
                 List.fold exs ~init:([], []) ~f:(fun (exs_zero, exs_succ) ->
                   function
                   | (sigma, None, output) as ex -> (
                       match TCtx.lookup_exn sigma x with
                       | EOp (Zero, []) -> (ex :: exs_zero, exs_succ)
                       | EOp (Succ, [ pred ]) ->
                           let sigma_succ =
                             TCtx.bind_alist sigma [ (x_n, pred) ]
                           in
                           (exs_zero, (sigma_succ, None, output) :: exs_succ)
                       | _ -> raise (Typ.Inconsistent ""))
                   | _ ->
                       failwith "call decompose on a function synthesis problem")
               in
               let result_zero = learn_pred gamma_zero exs_zero ty ps_base in
               let result_succ = learn_pred gamma_succ exs_succ ty ps_ind in
               let result_match =
                 Result.join gamma sigmas
                   (MatchX (x, [ PZero; PSucc (PVar x_n) ]))
                   [ result_zero; result_succ ]
               in
               Some result_match
           | _ -> None)
    |> Result.union
  in
  Option.both (PS.update `MatchBase ps) (PS.update `MatchInd ps)
  |> Option.value_map ~default:Result.empty ~f:get_result

(** in contrast to `learn`, `learn_pred` can takes /multiple/ examples
    as spec because it disallows angelic semantics during
    enumeration *)
and learn_pred (gamma : TCtx.t) (exs : Examples.t) (ty : Typ.t) (ps : PS.t) :
    Result.t =
  let sigmas = Examples.to_vctx_set exs in
  let result_enum = enumerate ~allow_asm:false gamma exs ty ps in
  let result_match = intro_match_pred gamma exs ty ps in
  Result.union [ result_enum; result_match ]

module ExpPair = struct
  type t = Exp.t * Exp.t [@@deriving sexp_of, compare, hash]
end

let traces gamma sigma fpar ty ps =
  let trace (io : ExpPair.t) : VSA.t =
    let memo f =
      let cache = Hashtbl.create (module ExpPair) ~size:20 in
      fun (v : ExpPair.t) ->
        match Hashtbl.find cache v with
        | None ->
            let res = f v in
            Hashtbl.add_exn cache ~key:v ~data:res;
            res
        | Some res -> res
    in
    let memo_rec f_norec =
      let f = ref (fun _ -> assert false) in
      let f_rec_memo = memo (fun v -> f_norec !f v) in
      f := f_rec_memo;
      f_rec_memo
    in
    let trace_norec f io =
      Fresh.refresh ();
      let result = learn ~fpar gamma (Example.of_io sigma io) ty ps in
      result |> Result.to_alist
      |> List.map ~f:(fun (asm, vsa) ->
             (* printf !"asm: %{sexp:Examples.Asm.t}\n" asm; *)
             (* VSA.print vsa; *)
             match Examples.Asm.to_alist asm with
             | [] -> vsa
             | ios ->
                 let tr =
                   List.fold_right ios ~init:[] ~f:(fun io acc -> f io :: acc)
                   |> List.reduce_exn
                        ~f:(VSA.intersect ~max_branches:(PS.get_cond_bound ps))
                 in
                 let vsa' =
                   VSA.intersect ~max_branches:(PS.get_cond_bound ps) vsa tr
                 in
                 vsa')
      |> VSA.union_list
    in
    memo_rec trace_norec io
  in
  List.fold_right fpar ~init:[] ~f:(fun (i, o) acc ->
      let cur = trace (i, o) in
      (* VSA.print cur; *)
      cur :: acc)
  |> List.reduce_exn ~f:(VSA.intersect ~max_branches:(PS.get_cond_bound ps))

(* let traces gamma sigma fpar ty n d b dcmp = *)
(*   let rec trace ?(i = 0) io = *)
(*     Fresh.refresh (); *)
(*     let result = learn ~fpar gamma (Example.of_io sigma io) ty n d dcmp in *)
(*     result |> Result.to_alist *)
(*     |> List.map ~f:(fun (asm, vsa) -> *)
(*            (\* printf !"%dasm: %{sexp:Examples.Asm.t}\n" i asm; *\) *)
(*            (\* VSA.print vsa; *\) *)
(*            match Examples.Asm.to_alist asm with *)
(*            | []  -> vsa *)
(*            | ios -> *)
(*                let tr = *)
(*                  List.fold_right ios ~init:[] ~f:(fun io acc -> *)
(*                      trace ~i:(i + 1) io :: acc) *)
(*                  |> List.reduce_exn ~f:(VSA.intersect ~max_branches:b) *)
(*                in *)
(*                let vsa' = VSA.intersect ~max_branches:b vsa tr in *)
(*                vsa') *)
(*     |> VSA.union_list *)
(*   in *)
(*   List.fold_right fpar ~init:[] ~f:(fun (i, o) acc -> *)
(*       let cur = trace (i, o) in *)
(*       (\* printf !"input: %{sexp:exp}\n vsa: %{sexp:VSA.t}\n" i cur; *\) *)
(*       (\* VSA.print cur; *\) *)
(*       cur :: acc) *)
(*   |> List.reduce_exn ~f:(fun acc vsa -> *)
(*          let acc' = VSA.intersect ~max_branches:b acc vsa in *)
(*          (\* VSA.print acc'; *\) *)
(*          acc') *)

type pick = Random | AvoidUnification | AvoidNestedMatch

let rec pick_random (vsa : VSA.t) : Exp.t option =
  match vsa with
  | Union vsas ->
      Set.fold vsas ~init:None ~f:(fun acc vsa ->
          match acc with
          | Some e -> Some e
          | None -> pick_random vsa)
  | Join (_, _, op, vsa_list) ->
      List.fold_right vsa_list ~init:(Some []) ~f:(fun vsa acc ->
          Option.map2 (pick_random vsa) acc ~f:List.cons)
      |> Option.map ~f:(VSA.apply op)
  | Direct (_, _, exp) -> Some exp
  | Empty -> None
  (* TODO: select any expression of the expected type *)
  | Top ty -> Some (Exp.default ty)
  | Trace _ -> None

let rec pick_avoid_nested_match ?(d = 0) (vsa : VSA.t) : Exp.t option =
  let flag_exhausted = ref false in
  let rec aux ~d (vsa : VSA.t) =
    match vsa with
    | Union vsas ->
        Set.fold vsas ~init:None ~f:(fun acc vsa ->
            match acc with
            | Some e -> Some e
            | None -> aux ~d vsa)
    | Join (_, _, (MatchX _ as op), vsa_list) when d = 0 ->
        flag_exhausted := true;
        None
    | Join (_, _, (MatchX _ as op), vsa_list) ->
        List.fold_right vsa_list ~init:(Some []) ~f:(fun vsa acc ->
            Option.map2 (aux ~d:(d - 1) vsa) acc ~f:List.cons)
        |> Option.map ~f:(VSA.apply op)
    | Join (_, _, op, vsa_list) ->
        List.fold_right vsa_list ~init:(Some []) ~f:(fun vsa acc ->
            Option.map2 (aux ~d vsa) acc ~f:List.cons)
        |> Option.map ~f:(VSA.apply op)
    | Direct (_, _, exp) -> Some exp
    | Empty -> None
    (* TODO: select any expression of the expected type *)
    | Top ty -> Some (Exp.default ty)
    | Trace _ -> None
  in
  let vsa_opt = aux ~d vsa in
  if not !flag_exhausted then
    vsa_opt
  else
    match vsa_opt with
    | Some x -> Some x
    | None -> pick_avoid_nested_match ~d:(d + 1) vsa

let naive_compare (vsa1 : VSA.t) (vsa2 : VSA.t) : int =
  match (vsa1, vsa2) with
  | Union set1, Union set2 | Trace set1, Trace set2 -> VSASet.compare set1 set2
  | Direct (_, sigmas1, e1), Direct (_, sigmas2, e2) ->
      let size_cmp = Int.compare (Exp.size e1) (Exp.size e2) in
      if size_cmp = 0 then
        if Exp.compare e1 e2 = 0 then
          -VCtxSet.compare sigmas1 sigmas2
        else
          Exp.compare e1 e2
      else
        size_cmp
  | ( Join (_, sigmas1, (MatchX _ as op1), vsas1),
      Join (_, sigmas2, (MatchX _ as op2), vsas2) ) ->
      (* | Join (_, sigmas1, op1, vsas1), Join (_, sigmas2, op2, vsas2) -> *)
      if VSA.compare_op op1 op2 <> 0 then
        VSA.compare_op op1 op2
      else if VCtxSet.length sigmas1 < VCtxSet.length sigmas2 then
        -1
      else if VCtxSet.length sigmas1 > VCtxSet.length sigmas2 then
        1
      else
        List.compare (* Poly.compare *) VSA.compare vsas1 vsas2
  | Join (_, sigmas1, op1, vsas1), Join (_, sigmas2, op2, vsas2) ->
      if VSA.compare_op op1 op2 <> 0 then
        VSA.compare_op op1 op2
      else if VCtxSet.length sigmas1 < VCtxSet.length sigmas2 then
        -1
      else if VCtxSet.length sigmas1 > VCtxSet.length sigmas2 then
        1
      else
        List.compare (* Poly.compare *) VSA.compare vsas1 vsas2
  (* | Join (_, _, op1, _), Join (_, _, op2, _) -> compare_op op1 op2 *)
  | Empty, Empty | Top _, Top _ -> 0
  | Empty, (Direct _ | Join _ | Union _ | Trace _ | Top _)
  | Direct _, (Join _ | Union _ | Trace _ | Top _)
  | Join _, (Union _ | Trace _ | Top _)
  | Union _, (Trace _ | Top _)
  | Trace _, Top _ -> -1
  | Top _, (Empty | Direct _ | Join _ | Union _ | Trace _)
  | Trace _, (Empty | Direct _ | Join _ | Union _)
  | Union _, (Empty | Direct _ | Join _)
  | Join _, (Empty | Direct _)
  | Direct _, Empty -> 1

let rec pick_pred : VSA.t -> exp option = function
  | Union vsas ->
      VSASet.to_list vsas
      |> List.foldi ~init:None ~f:(fun i acc vsa ->
             match acc with
             | Some e -> Some e
             | None -> pick_pred vsa)
  | Trace set -> None
  | Join (_, _, op, vsa_list) ->
      List.fold_right vsa_list ~init:(Some []) ~f:(fun vsa acc ->
          Option.map2 (pick_pred vsa) acc ~f:List.cons)
      |> Option.map ~f:(VSA.apply op)
  | Direct (_, _, exp) -> Some exp
  | Empty -> None
  (* TODO: select any expression of the expected type *)
  | Top ty -> Some (Exp.default ty)

let rec pick_first ?(semantic = true) (vsa : VSA.t) (ps : PS.t) =
  match ps.mode with
  | `SyRup ->
      let vsas =
        match vsa with
        | Union vsas -> Set.to_list vsas
        | vsa -> [ vsa ]
      in
      if semantic then
        (* List.range ~start:`inclusive ~stop:`inclusive 0 1 *)
        (* |> List.find_map ~f:(fun d -> *)
        (*     PS.ps_syrup ps.atomicSize d 0 |> aux vsa) *)
        List.range ~start:`inclusive ~stop:`inclusive 0
          (Int.min ps.matchDepth 1)
        |> List.find_map ~f:(fun d ->
               List.find_map vsas ~f:(fun vsa ->
                   List.range ~start:`inclusive ~stop:`inclusive 2
                     ((ps.matchDepth * 2) + ps.atomicSize + 1)
                   |> List.find_map ~f:(fun h ->
                          let ps_pred = PS.ps_syrup ps.atomicSize d 0 in
                          PS.ps_height h |> aux ~ps_pred:(Some ps_pred) vsa)))
      else
        (* use height as the only criteria to pick program *)
        List.range ~start:`inclusive ~stop:`inclusive 2
          ((ps.matchDepth * 2) + ps.atomicSize + 1)
        |> List.find_map ~f:(fun h ->
               List.find_map (List.permute vsas) ~f:(fun vsa ->
                   PS.ps_height h |> aux vsa))
  | `Height ->
      List.range ~start:`inclusive ~stop:`inclusive 2 ps.totalHeight
      |> List.find_map ~f:(fun h -> PS.ps_height h |> aux vsa)

and aux ?(ps_pred = None) (vsa : VSA.t) (ps : PS.t) =
  match vsa with
  | Union vsas ->
      VSASet.to_list vsas
      |> List.foldi ~init:None ~f:(fun i acc vsa ->
             match acc with
             | Some e -> Some e
             | None -> aux ~ps_pred vsa ps)
  | Trace set -> unify ~ps_pred set ps
  | Join (_, _, op, vsa_list) ->
      let* ps' = PS.update `Walkdown ps in
      List.fold_right vsa_list ~init:(Some []) ~f:(fun vsa acc ->
          Option.map2 (aux ~ps_pred vsa ps') acc ~f:List.cons)
      |> Option.map ~f:(VSA.apply op)
  | Direct (_, _, exp) -> Some exp
  | Empty -> None
  (* TODO: select any expression of the expected type *)
  | Top ty -> Some (Exp.default ty)

and unify ~ps_pred (vsa_set : VSASet.t) (ps : PS.t) : Exp.t option =
  (* when ps.mode == `Height, we will not perform unification if the height
     allowance is already zero *)
  let* ps' = PS.update `Walkdown ps in
  let ps_pred' = ps_pred |> Option.value ~default:ps' in
  match VSASet.to_list vsa_set with
  | [] | [ _ ] -> failwith "a trace should composed by at least two vsa"
  | [
   ((Direct (gamma1, sigmas1, _) | Join (gamma1, sigmas1, _, _)) as vsa1);
   ((Direct (gamma2, sigmas2, _) | Join (gamma2, sigmas2, _, _)) as vsa2);
  ] -> (
      let exs1_false =
        sigmas1 |> VCtxSet.to_list
        |> List.map ~f:(fun sigma -> (sigma, None, Exp.exp_false))
      in
      let exs1_true =
        sigmas1 |> VCtxSet.to_list
        |> List.map ~f:(fun sigma -> (sigma, None, Exp.exp_true))
      in
      let exs2_true =
        sigmas2 |> VCtxSet.to_list
        |> List.map ~f:(fun sigma -> (sigma, None, Exp.exp_true))
      in
      let exs2_false =
        sigmas2 |> VCtxSet.to_list
        |> List.map ~f:(fun sigma -> (sigma, None, Exp.exp_false))
      in
      match
        ( learn_pred gamma1 (exs1_true @ exs2_false) TBool ps_pred'
          |> Result.get_concret
          |> Option.bind ~f:(fun vsa -> pick_pred vsa),
          learn_pred gamma1 (exs1_false @ exs2_true) TBool ps_pred'
          |> Result.get_concret
          |> Option.bind ~f:(fun vsa -> pick_pred vsa) )
      with
      | Some pred_1_2, Some pred_2_1 when Exp.compare pred_1_2 pred_2_1 <= 0 ->
          Option.map2 (aux ~ps_pred vsa1 ps') (aux ~ps_pred vsa2 ps')
            ~f:(fun e1 e2 -> VSA.apply (ITE 2) [ pred_1_2; e1; e2 ])
      | Some pred_1_2, None ->
          Option.map2 (aux ~ps_pred vsa1 ps') (aux ~ps_pred vsa2 ps')
            ~f:(fun e1 e2 -> VSA.apply (ITE 2) [ pred_1_2; e1; e2 ])
      (* Exp.compare pred_1_2 pred_2_1 >0 *)
      | Some _, Some pred_2_1 | None, Some pred_2_1 ->
          Option.map2 (aux ~ps_pred vsa1 ps') (aux ~ps_pred vsa2 ps')
            ~f:(fun e1 e2 -> VSA.apply (ITE 2) [ pred_2_1; e2; e1 ])
      | None, None -> None)
  | [
   ((Direct (gamma1, sigmas1, _) | Join (gamma1, sigmas1, _, _)) as vsa1);
   ((Direct (gamma2, sigmas2, _) | Join (gamma2, sigmas2, _, _)) as vsa2);
   ((Direct (gamma3, sigmas3, _) | Join (gamma3, sigmas3, _, _)) as vsa3);
  ] -> (
      let exs_true1 =
        sigmas1 |> VCtxSet.to_list
        |> List.map ~f:(fun sigma -> (sigma, None, Exp.exp_true))
      in
      let exs_false1 =
        VCtxSet.to_list sigmas2 @ VCtxSet.to_list sigmas3
        |> List.map ~f:(fun sigma -> (sigma, None, Exp.exp_false))
      in
      let exs_true2 =
        sigmas2 |> VCtxSet.to_list
        |> List.map ~f:(fun sigma -> (sigma, None, Exp.exp_true))
      in
      let exs_false2 =
        sigmas3 |> VCtxSet.to_list
        |> List.map ~f:(fun sigma -> (sigma, None, Exp.exp_false))
      in
      match
        ( learn_pred gamma1 (exs_true1 @ exs_false1) TBool ps_pred'
          |> Result.get_concret
          |> Option.bind ~f:(fun vsa -> pick_pred vsa),
          learn_pred gamma1 (exs_true2 @ exs_false2) TBool ps_pred'
          |> Result.get_concret
          |> Option.bind ~f:(fun vsa -> pick_pred vsa) )
      with
      | Some pred1, Some pred2 -> (
          match
            (aux ~ps_pred vsa1 ps', aux ~ps_pred vsa2 ps', aux ~ps_pred vsa3 ps')
          with
          | Some e1, Some e2, Some e3 ->
              VSA.apply (ITE 3) [ pred1; pred2; e1; e2; e3 ] |> Some
          | _ -> None)
      | _ -> None)
  | vsas ->
      failwith
        (Printf.sprintf
           "unification on more than three branches unimplemented:\n%s"
           (Sexp.to_string_hum (sexp_of_list VSA.sexp_of_t vsas)))

let main ?(gamma = TCtx.empty ()) ?(sigma = VCtx.empty ()) (oc : Out_channel.t)
    ?(mode = `SyRup) ?(semantic = true) ty exs : Exp.t option =
  let exs =
    List.sort exs ~compare:(fun (i1, _) (i2, _) -> -Exp.compare_input i1 i2)
  in
  let header =
    exs |> List.hd_exn |> Tuple2.get1 |> Exp.to_pat |> Exp.fix fun_name
  in
  (* Printf.fprintf oc "examples: %s\n" *)
  (*   (Sexp.to_string *)
  (*      (sexp_of_list (sexp_of_pair Exp.sexp_of_t Exp.sexp_of_t) exs)); *)
  PS.gen mode ty
  |> List.find_map ~f:(fun ps ->
         (* Printf.fprintf oc !"start synthesis with %{sexp:PS.t}\n" ps; *)
         (* Out_channel.flush oc; *)
         let vsa = traces gamma sigma exs ty ps in
         (* (match vsa with *)
         (* | Union vsas -> *)
         (*     VSASet.to_list vsas *)
         (*     |> List.iter ~f:(fun vsa -> *)
         (*            match pick_first ~semantic vsa ps with *)
         (*            | Some e -> SmartPrint.to_out_channel 25 2 oc (Exp.pp e) *)
         (*            | _ -> ()) *)
         (* | _ -> failwith "not a union"); *)
         (* print_s *)
         (*   (VSA.sexp_of_t *)
         (*      (match vsa with *)
         (*      | Join (_, _, _, [ Union vsa_set ]) -> *)
         (*          VSASet.nth vsa_set 1 |> Option.value_exn *)
         (*      | _ -> failwith "impossible")); *)
         (* VSA.print vsa; *)
         (* Printf.fprintf oc "VSA done\n"; *)
         match pick_first ~semantic vsa ps with
         | Some e ->
             let e = header e in
             SmartPrint.to_out_channel 25 2 oc (Exp.pp e);
             Some e
         | None ->
             (* Printf.fprintf oc "fail to find a solution\n"; *)
             None)
