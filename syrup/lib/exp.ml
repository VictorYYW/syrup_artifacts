open Core
open Util
include Lang

type t = exp [@@deriving sexp, compare, hash]

let nil = EOp (Nil, [])
let cons hd tl = EOp (Cons, [ hd; tl ])
let none = EOp (No, [])
let some elem = EOp (Just, [ elem ])
let zero = EOp (Zero, [])
let succ num = EOp (Succ, [ num ])
let leaf = EOp (Leaf, [])
let node v t1 t2 = EOp (Node, [ v; t1; t2 ])
let app f arg = EApp (f, arg)
let ematch scrut rules = EMatch (scrut, rules)
let fix id arg body = EFix (id, arg, body)
let exp_false = EOp (False, [])
let exp_true = EOp (True, [])

let rec is_nat : exp -> bool = function
  | EOp (Zero, []) -> true
  | EOp (Succ, [ e ]) -> is_nat e
  | _ -> false

let rec to_int : exp -> int = function
  | EOp (Zero, []) -> 0
  | EOp (Succ, [ e ]) -> to_int e + 1
  | _ -> failwith "to_int expects expression of num type"

let rec exps_of_list : t -> t list = function
  | EOp (Nil, []) -> []
  | EOp (Cons, [ hd; tl ]) -> hd :: exps_of_list tl
  | _ -> failwith "exps_of_list expects expression of list type"

let rec exps_of_tree : t -> t list = function
  | EOp (Leaf, []) -> []
  | EOp (Node, [ v; t1; t2 ]) -> (v :: exps_of_tree t1) @ exps_of_tree t2
  | _ -> failwith "exps_of_tree expects expression of tree type"

let rec compare_input i1 i2 =
  match (i1, i2) with
  | EOp ((Nil | Cons), _), EOp ((Nil | Cons), _) ->
      let es1 = exps_of_list i1 in
      let es2 = exps_of_list i2 in
      let diff = List.length es1 - List.length es2 in
      if diff > 0 then
        1
      else if diff < 0 then
        -1
      else
        List.compare compare_input es1 es2
  | EOp ((Leaf | Node), _), EOp ((Leaf | Node), _) ->
      let es1 = exps_of_tree i1 in
      let es2 = exps_of_tree i2 in
      let diff = List.length es1 - List.length es2 in
      if diff > 0 then
        1
      else if diff < 0 then
        -1
      else
        List.compare compare_input es1 es2
  | ETuple es1, ETuple es2 ->
      List.compare compare_input es1 es2
      (* let diffs = List.map2_exn es1 es2 ~f:(fun e1 e2 -> compare_input e1 e2) in *)
      (* if *)
      (*   List.for_all diffs ~f:(fun diff -> diff >= 0) *)
      (*   && List.exists diffs ~f:(fun diff -> diff > 0) *)
      (* then *)
      (*   1 *)
      (* else if *)
      (*   List.for_all diffs ~f:(fun diff -> diff <= 0) *)
      (*   && List.exists diffs ~f:(fun diff -> diff < 0) *)
      (* then *)
      (*   -1 *)
      (* else *)
      (*   compare_exp i1 i2 *)
  | _ -> compare_exp i1 i2

let space_string d : string =
  List.range 0 d |> List.map ~f:(const "  ") |> String.concat

let rec to_string ?(d = 0) (e : t) : string =
  space_string d
  ^
  match e with
  | EVar id -> id
  | ETuple es ->
      "(" ^ (es |> List.map ~f:to_string |> String.concat ~sep:", ") ^ ")"
  | EOp (op, es) ->
      "(" ^ Op.str op ^ " "
      ^ (es |> List.map ~f:to_string |> String.concat ~sep:" ")
      ^ ")"
  | EApp (f, arg) -> "(" ^ to_string f ^ " " ^ to_string arg ^ ")"
  | EMatch (scrut, branches) ->
      "match (" ^ to_string scrut ^ ")\n"
      ^ (branches
        |> List.map ~f:(fun (p, e) ->
               space_string d ^ "| " ^ Pat.to_string p ^ " -> \n"
               ^ to_string ~d:(d + 1) e
               ^ "\n")
        |> String.concat)
  | EFix (f_name, p, e) ->
      "fix " ^ f_name ^ ".fun " ^ Pat.to_string p ^ ".\n"
      ^ to_string ~d:(d + 1) e
  | _ -> failwith "unimplemented"

let rec pp : t -> SmartPrint.t =
  let open SmartPrint in
  function
  | EVar id -> !^id
  | ETuple es ->
      nest
        (parens
           (space ^^ separate (!^"," ^^ space) (List.map es ~f:pp) ^^ space))
  | EOp (Ite, [ pred; e1; e2 ]) ->
      nest
        (nest (!^"if" ^^ pp pred)
        ^^ nest (!^"then" ^^ pp e1)
        ^^ nest (!^"else" ^^ pp e2))
  | EOp (op, es) ->
      nest
        (parens
           (space
           ^^ !^(Op.str op)
           ^^ separate space (List.map es ~f:pp)
           ^^ space))
  | EApp (f, arg) -> nest (parens (pp f ^^ pp arg))
  | EMatch (scrut, branches) ->
      nest (!^"match" ^^ parens (pp scrut) ^^ !^"with" ^^ newline)
      ^^ concat
           (List.map branches ~f:(fun (p, e) ->
                nest
                  (!^"|"
                  ^^ !^(Pat.to_string p)
                  ^^ !^"->"
                  ^^ nest (pp e)
                  ^^ newline)))
  | EFix (f_name, p, e) ->
      nest
        (!^"fix" ^^ !^f_name ^-^ !^".fun"
        ^^ !^(Pat.to_string p)
        ^-^ !^"." ^^ pp e ^^ newline)
  | _ -> failwith "unimplemented"

let rec to_pat (value : t) : pat =
  match value with
  | ETuple exps ->
      let pats = List.map exps ~f:to_pat in
      PTuple pats
  | _ -> PVar (Fresh.name 'x')

let rec default : Typ.t -> t = function
  | TNum -> EOp (Zero, [])
  | TBool -> EOp (False, [])
  | TList (TList _ as ty) -> EOp (Cons, [ default ty; EOp (Nil, []) ])
  | TList _ -> EOp (Nil, [])
  | TTree _ -> EOp (Leaf, [])
  | TOpt _ -> EOp (No, [])
  | _ -> failwith "Exp.default unimplemented"

exception Z3NegativeInteger

let rec of_z3expr (ty : Typ.t) (expr : Solver.Expr.expr) : t =
  (* Printf.printf "of_z3expr ty %s \n" (Sexp.to_string (Typ.sexp_of_t ty)); *)
  (* Printf.printf "of_z3expr expr %s \n" *)
  (*   (Solver.Expr.ast_of_expr expr |> Solver.AST.to_sexpr); *)
  match ty with
  | TNum ->
      let n = Solver.get_int expr in
      if n < 0 then
        raise Z3NegativeInteger
      else
        Denotation.int n
  | TBool -> Denotation.bool (Solver.get_bool expr)
  | TList TBool ->
      if Solver.is_nil_list_of_bool expr then
        EOp (Nil, [])
      else
        let hd = Solver.hd_list_of_bool expr in
        let tl = Solver.tl_list_of_bool expr in
        EOp (Cons, [ of_z3expr TBool hd; of_z3expr ty tl ])
  | TList TNum ->
      if Solver.is_nil_list_of_int expr then
        EOp (Nil, [])
      else
        let hd = Solver.hd_list_of_int expr in
        let tl = Solver.tl_list_of_int expr in
        EOp (Cons, [ of_z3expr TNum hd; of_z3expr ty tl ])
  | TOpt TNum ->
      if Solver.is_some_int expr then
        EOp (Just, [ of_z3expr TNum (Solver.get_some_int expr) ])
      else
        EOp (No, [])
  | _ ->
      failwith
        (Printf.sprintf "%s unimplemented in solver"
           (Sexp.to_string (Typ.sexp_of_t ty)))

let equal e1 e2 =
  match (e1, e2) with
  | ETop _, _ | _, ETop _ -> true
  | _ -> compare_exp e1 e2 = 0

let rec cost = function
  | EVar _ | EFPar _ | ETop _ -> 1
  | ETuple exps -> List.fold exps ~init:0 ~f:(fun c exp -> c + cost exp)
  | EOp (op, exps) ->
      List.fold exps ~init:(Op.cost op) ~f:(fun c exp -> c + cost exp)
  | EMatch (scrut, branches) ->
      List.fold branches ~init:(cost scrut) ~f:(fun c b -> c + cost_of_branch b)
  | ELet (_p, _t, defn, body) -> 1 + cost defn + cost body
  | EFun (_pat, defn) -> 1 + cost defn
  | EFix (_id, _pat, defn) -> 1 + cost defn
  | EApp (f, arg) -> cost f + cost arg

and cost_of_branch (_pat, body) = 1 + cost body

(** take a tuple and return an expression list *)
let to_list : t -> t list = function
  | ETuple es -> es
  | e -> [ e ]

(** take a list of arguments and transform into one tuplized expression *)
let of_list : t list -> t = function
  | [] -> failwith "cannot transform empty expression list to an expression"
  | [ single ] -> single
  | exps -> ETuple exps

(** get nth element in a tuple *)
let nth (e : t) (n : int) : t =
  match (e, n) with
  | ETuple es, n -> List.nth_exn es n
  | _, 0 -> e
  | _, n ->
      (* n > 0 *)
      failwith
        "unable to retrieve nth element from a non-tuple expression when n > 0"

let size = cost

let rec infer (exp : t) : Typ.t =
  match exp with
  | ETuple es -> List.map es ~f:infer |> TProd
  | EOp ((True | False), []) -> TBool
  | EOp (Just, [ e ]) -> TOpt (infer e)
  | EOp (No, []) -> TOpt (Typ.fresh_free 0)
  | EOp ((Zero | Succ), _) -> TNum
  | EOp (Nil, []) -> TList (Typ.fresh_free 0)
  | EOp (Cons, [ hd; _ ]) -> TList (infer hd)
  | EOp (Leaf, []) -> TTree (Typ.fresh_free 0)
  | EOp (Node, [ v; _; _ ]) -> TTree (infer v)
  (* TODO: workaround to infer type for background function when synthesizing
     higher-order function *)
  | EVar "isEven" -> TArr (TNum, TBool)
  | EVar "isNonzero" -> TArr (TNum, TBool)
  | EVar "inc" -> TArr (TNum, TNum)
  | EVar "zero" -> TArr (TNum, TNum)
  | EVar "countOdd" -> TArr (TProd [ TNum; TNum ], TNum)
  | EVar "add" -> TArr (TProd [ TNum; TNum ], TNum)
  | EVar "div2" -> TArr (TNum, TNum)
  | _ -> failwith "unimplemented"

let stdlib_vctx : t Ctx.t =
  [
    ( "map",
      EFix
        ( "map",
          PTuple [ PVar "xs"; PVar "f" ],
          EMatch
            ( EVar "xs",
              [
                (PNil, nil);
                ( PCons (PVar "hd", PVar "tl"),
                  cons
                    (EApp (EVar "f", EVar "hd"))
                    (EApp (EVar "map", ETuple [ EVar "tl"; EVar "f" ])) );
              ] ) ) );
    ( "filter",
      EFix
        ( "filter",
          PTuple [ PVar "xs"; PVar "f" ],
          EMatch
            ( EVar "xs",
              [
                (PNil, nil);
                ( PCons (PVar "hd", PVar "tl"),
                  EOp
                    ( Ite,
                      [
                        EApp (EVar "f", EVar "hd");
                        EOp
                          ( Cons,
                            [
                              EVar "hd";
                              EApp
                                (EVar "filter", ETuple [ EVar "tl"; EVar "f" ]);
                            ] );
                        EApp (EVar "filter", ETuple [ EVar "tl"; EVar "f" ]);
                      ] ) );
              ] ) ) );
    ( "foldl",
      EFix
        ( "foldl",
          PTuple [ PVar "xs"; PVar "b"; PVar "f" ],
          EMatch
            ( EVar "xs",
              [
                (PNil, EVar "b");
                ( PCons (PVar "hd", PVar "tl"),
                  EApp
                    ( EVar "foldl",
                      ETuple
                        [
                          EVar "tl";
                          EApp (EVar "f", ETuple [ EVar "b"; EVar "hd" ]);
                          EVar "f";
                        ] ) );
              ] ) ) );
    ( "foldr",
      EFix
        ( "foldr",
          PTuple [ PVar "xs"; PVar "b"; PVar "f" ],
          EMatch
            ( EVar "xs",
              [
                (PNil, EVar "b");
                ( PCons (PVar "hd", PVar "tl"),
                  EApp
                    ( EVar "f",
                      ETuple
                        [
                          EApp
                            ( EVar "foldr",
                              ETuple [ EVar "tl"; EVar "b"; EVar "f" ] );
                          EVar "hd";
                        ] ) );
              ] ) ) );
  ]
  |> Ctx.of_alist_exn

let background : (string * (typ * t)) list =
  [
    ( "less_than",
      ( TArr (TProd [ TNum; TNum ], TBool),
        fix "less_than"
          (PTuple [ PVar "a"; PVar "b" ])
          (ematch (EVar "a")
             [
               ( PZero,
                 ematch (EVar "b")
                   [ (PZero, exp_false); (PSucc (PVar "n2"), exp_true) ] );
               ( PSucc (PVar "n1"),
                 ematch (EVar "b")
                   [
                     (PZero, exp_false);
                     ( PSucc (PVar "n2"),
                       app (EVar "less_than") (ETuple [ EVar "n1"; EVar "n2" ])
                     );
                   ] );
             ]) ) );
    ( "equal_to",
      ( TArr (TProd [ TNum; TNum ], TBool),
        fix "equal_to"
          (PTuple [ PVar "a"; PVar "b" ])
          (ematch (EVar "a")
             [
               ( PZero,
                 ematch (EVar "b")
                   [ (PZero, exp_true); (PSucc (PVar "n2"), exp_false) ] );
               ( PSucc (PVar "n1"),
                 ematch (EVar "b")
                   [
                     (PZero, exp_false);
                     ( PSucc (PVar "n2"),
                       app (EVar "equal_to") (ETuple [ EVar "n1"; EVar "n2" ])
                     );
                   ] );
             ]) ) );
    ( "not",
      ( TArr (TBool, TBool),
        EFun
          ( PVar "x",
            ematch (EVar "x") [ (PTrue, exp_false); (PFalse, exp_true) ] ) ) );
    ( "isEven",
      ( TArr (TNum, TBool),
        fix "isEven" (PVar "x")
          (ematch (EVar "x")
             [
               (PZero, exp_true);
               ( PSucc (PVar "n"),
                 ematch
                   (app (EVar "isEven") (EVar "n"))
                   [ (PTrue, exp_false); (PFalse, exp_true) ] );
             ]) ) );
    ( "div2",
      ( TArr (TNum, TNum),
        fix "div2" (PVar "x")
          (ematch (EVar "x")
             [
               (PZero, Denotation.int 0);
               ( PSucc (PVar "x'"),
                 ematch (EVar "x'")
                   [
                     (PZero, Denotation.int 0);
                     (PSucc (PVar "x''"), succ (app (EVar "div2") (EVar "x''")));
                   ] );
             ]) ) );
    ( "isNonzero",
      ( TArr (TNum, TBool),
        EFun
          ( PVar "x",
            ematch (EVar "x")
              [ (PZero, exp_false); (PSucc (PVar "n"), exp_true) ] ) ) );
    ("inc", (TArr (TNum, TNum), EFun (PVar "x", EOp (Succ, [ EVar "x" ]))));
    ("zero", (TArr (TNum, TNum), EFun (PVar "x", zero)));
    ( "add",
      ( TArr (TProd [ TNum; TNum ], TNum),
        fix "add"
          (PTuple [ PVar "x"; PVar "y" ])
          (ematch (EVar "x")
             [
               (PZero, EVar "y");
               ( PSucc (PVar "x'"),
                 EOp
                   (Succ, [ app (EVar "add") (ETuple [ EVar "x'"; EVar "y" ]) ])
               );
             ]) ) );
    ( "countOdd",
      ( TArr (TProd [ TNum; TNum ], TNum),
        EFun
          ( PTuple [ PVar "x"; PVar "y" ],
            EOp
              ( Ite,
                [
                  app
                    (fix "isEven" (PVar "x")
                       (ematch (EVar "x")
                          [
                            (PZero, exp_true);
                            ( PSucc (PVar "n"),
                              ematch
                                (app (EVar "isEven") (EVar "n"))
                                [ (PTrue, exp_false); (PFalse, exp_true) ] );
                          ]))
                    (EVar "y");
                  EVar "x";
                  EOp (Succ, [ EVar "x" ]);
                ] ) ) ) );
    ( "append_bool",
      ( TArr (TProd [ TList TBool; TList TBool ], TList TBool),
        EFix
          ( "append",
            PTuple [ PVar "x"; PVar "y" ],
            EMatch
              ( EVar "x",
                [
                  (PNil, EVar "y");
                  ( PCons (PVar "hd", PVar "tl"),
                    EOp
                      ( Cons,
                        [
                          EVar "hd";
                          EApp
                            (EVar "append_bool", ETuple [ EVar "tl"; EVar "y" ]);
                        ] ) );
                ] ) ) ) );
    ( "append",
      ( TArr (TProd [ TList TNum; TList TNum ], TList TNum),
        EFix
          ( "append",
            PTuple [ PVar "x"; PVar "y" ],
            EMatch
              ( EVar "x",
                [
                  (PNil, EVar "y");
                  ( PCons (PVar "hd", PVar "tl"),
                    EOp
                      ( Cons,
                        [
                          EVar "hd";
                          EApp (EVar "append", ETuple [ EVar "tl"; EVar "y" ]);
                        ] ) );
                ] ) ) ) );
    ( "snoc",
      ( TArr (TProd [ TList TNum; TNum ], TList TNum),
        EFix
          ( "snoc",
            PTuple [ PVar "x"; PVar "y" ],
            EMatch
              ( EVar "x",
                [
                  (PNil, cons (EVar "y") nil);
                  ( PCons (PVar "hd", PVar "tl"),
                    EOp
                      ( Cons,
                        [
                          EVar "hd";
                          EApp (EVar "snoc", ETuple [ EVar "tl"; EVar "y" ]);
                        ] ) );
                ] ) ) ) );
    ( "insert",
      ( TArr (TProd [ TList TNum; TNum ], TList TNum),
        EFix
          ( "insert",
            PTuple [ PVar "x"; PVar "y" ],
            EMatch
              ( EVar "x",
                [
                  (PNil, cons (EVar "y") nil);
                  ( PCons (PVar "hd", PVar "tl"),
                    EOp
                      ( Ite,
                        [
                          app
                            (fix "less_than"
                               (PTuple [ PVar "a"; PVar "b" ])
                               (ematch (EVar "a")
                                  [
                                    ( PZero,
                                      ematch (EVar "b")
                                        [
                                          (PZero, exp_false);
                                          (PSucc (PVar "n2"), exp_true);
                                        ] );
                                    ( PSucc (PVar "n1"),
                                      ematch (EVar "b")
                                        [
                                          (PZero, exp_false);
                                          ( PSucc (PVar "n2"),
                                            app (EVar "less_than")
                                              (ETuple [ EVar "n1"; EVar "n2" ])
                                          );
                                        ] );
                                  ]))
                            (ETuple [ EVar "hd"; EVar "y" ]);
                          EOp
                            ( Cons,
                              [
                                EVar "hd";
                                EApp
                                  (EVar "insert", ETuple [ EVar "tl"; EVar "y" ]);
                              ] );
                          EOp
                            ( Ite,
                              [
                                app
                                  (fix "equal_to"
                                     (PTuple [ PVar "a"; PVar "b" ])
                                     (ematch (EVar "a")
                                        [
                                          ( PZero,
                                            ematch (EVar "b")
                                              [
                                                (PZero, exp_true);
                                                (PSucc (PVar "n2"), exp_false);
                                              ] );
                                          ( PSucc (PVar "n1"),
                                            ematch (EVar "b")
                                              [
                                                (PZero, exp_false);
                                                ( PSucc (PVar "n2"),
                                                  app (EVar "equal_to")
                                                    (ETuple
                                                       [ EVar "n1"; EVar "n2" ])
                                                );
                                              ] );
                                        ]))
                                  (ETuple [ EVar "hd"; EVar "y" ]);
                                EVar "x";
                                EOp (Cons, [ EVar "y"; EVar "x" ]);
                              ] );
                        ] ) );
                ] ) ) ) );
    (* fold_right *)
    ( "fold",
      ( TArr
          ( TProd
              [
                TArr (TProd [ TList TNum; TNum ], TList TNum);
                TList TNum;
                TList TNum;
              ],
            TList TNum ),
        EFix
          ( "fold",
            PTuple [ PVar "f"; PVar "b"; PVar "xs" ],
            EMatch
              ( EVar "xs",
                [
                  (PNil, EVar "b");
                  ( PCons (PVar "hd", PVar "tl"),
                    EApp
                      ( EVar "f",
                        ETuple
                          [
                            EApp
                              ( EVar "fold",
                                ETuple [ EVar "f"; EVar "b"; EVar "tl" ] );
                            EVar "hd";
                          ] ) );
                ] ) ) ) );
    ( "map",
      ( TArr (TProd [ TArr (TNum, TNum); TList TNum ], TList TNum),
        EFix
          ( "map",
            PTuple [ PVar "f"; PVar "xs" ],
            EMatch
              ( EVar "xs",
                [
                  (PNil, nil);
                  ( PCons (PVar "hd", PVar "tl"),
                    EOp
                      ( Cons,
                        [
                          EApp (EVar "f", EVar "hd");
                          EApp (EVar "map", ETuple [ EVar "f"; EVar "tl" ]);
                        ] ) );
                ] ) ) ) );
  ]

exception TypeCheckError of string
(** Error thrown during evaluation because the expression synthesized is not
    well-typed, e.g., type inconsistency and non-exhaustive match *)

exception RuntimeError of string
(** Runtime error that is allowed to happen, e.g., division by zero, and
    recursion steps exceeding limit *)

let rec pattern_match (value : t) (pat : pat) : t Ctx.t Option.t =
  match (value, pat) with
  | _, PVar x -> Some (Ctx.of_alist_exn [ (x, value) ])
  (* nat *)
  | EOp (Zero, []), PZero -> Some (Ctx.empty ())
  | EOp (Succ, [ e ]), PSucc p -> pattern_match e p
  | EOp (Zero, []), PSucc _ | EOp (Succ, [ _ ]), PZero -> None
  (* bool *)
  | EOp (True, []), PTrue -> Some (Ctx.empty ())
  | EOp (False, []), PFalse -> Some (Ctx.empty ())
  | EOp (True, []), PFalse | EOp (False, []), PTrue -> None
  (* list *)
  | EOp (Nil, []), PNil -> Some (Ctx.empty ())
  | EOp (Cons, [ hd; tl ]), PCons (p_hd, p_tl) -> (
      match (pattern_match hd p_hd, pattern_match tl p_tl) with
      | Some ctx1, Some ctx2 ->
          Some (Ctx.merge ctx1 ctx2 ~f:Ctx.prohibite_duplicate)
      | _ -> None)
  | EOp (Nil, []), PCons _ | EOp (Cons, [ _; _ ]), PNil -> None
  (* tree *)
  | EOp (Leaf, []), PLeaf -> Some (Ctx.empty ())
  | EOp (Node, [ e; t1; t2 ]), PNode (p, p1, p2) -> (
      match (pattern_match e p, pattern_match t1 p1, pattern_match t2 p2) with
      | Some ctx1, Some ctx2, Some ctx3 ->
          Some (Ctx.join [ ctx1; ctx2; ctx3 ] ~f:Ctx.prohibite_duplicate)
      | _ -> None)
  | EOp (Leaf, []), PNode _ | EOp (Node, [ _; _; _ ]), PLeaf -> None
  (* tuple *)
  | ETuple exps, PTuple pats -> (
      match
        List.fold2 exps pats
          ~init:(Some (Ctx.empty ()))
          ~f:(fun ctx_opt exp pat ->
            Option.bind ctx_opt ~f:(fun ctx ->
                Option.map (pattern_match exp pat)
                  ~f:(Ctx.merge ctx ~f:Ctx.prohibite_duplicate)))
      with
      | Ok res -> res
      | Unequal_lengths ->
          raise
            (TypeCheckError
               (sprintf
                  !"Cannot match value %{sexp:exp} against pattern %{sexp:pat} \
                    due to type inconsistency"
                  value pat)))
  (* TODO: option type *)
  | _ ->
      raise
        (TypeCheckError
           (sprintf
              !"Cannot match value %{sexp:exp} against pattern %{sexp:pat} due \
                to type inconsistency"
              value pat))

(** apply substitution on `exp` according to bindings, i.e., `ctx` *)
let rec subst (ctx : t Ctx.t) (exp : t) : t =
  let subst_all = List.map ~f:(subst ctx) in
  match exp with
  | EVar x -> (
      match Ctx.lookup ctx x with
      | Some v -> v
      | None -> exp)
  | EOp (op, args) -> EOp (op, subst_all args)
  | ETuple xs -> ETuple (subst_all xs)
  | EFun (pat, defn) ->
      let ids = Pat.get_ids pat in
      let ctx =
        Ctx.filter_keys ctx
          ~f:(Fn.compose not (List.mem ids ~equal:String.( = )))
      in
      EFun (pat, subst ctx defn)
  | EFix (id, pat, defn) ->
      let ids = Pat.get_ids pat in
      let ctx =
        Ctx.filter_keys ctx
          ~f:(Fn.compose not (List.mem ids ~equal:String.( = )))
      in
      EFix (id, pat, subst ctx defn)
  | EApp (f, arg) -> EApp (subst ctx f, subst ctx arg)
  | ELet (pat, ty, defn, exp) ->
      let ids = Pat.get_ids pat in
      let ctx =
        Ctx.filter_keys ctx
          ~f:(Fn.compose not (List.mem ids ~equal:String.( = )))
      in
      ELet (pat, ty, subst ctx defn, subst ctx exp)
  | EMatch (scrut, branches) ->
      EMatch
        ( subst ctx scrut,
          List.map branches ~f:(fun (pat, exp) ->
              let ids = Pat.get_ids pat in
              let ctx =
                Ctx.filter_keys ctx
                  ~f:(Fn.compose not (List.mem ids ~equal:String.( = )))
              in
              (pat, subst ctx exp)) )
  | EFPar _ | ETop _ -> exp

exception PartialFunction of t

(** The evaluation will stop after "lim" recursion, fpar_error indicate if we
    would raise error when a partial function is applied on some undefined
    input *)
let rec eval ?(fpar_error = false) ?(lim = 100) ?(ctx : t Ctx.t = Ctx.empty ())
    (exp : t) : t =
  if lim = 0 then
    raise (RuntimeError (sprintf !"Exceeded recursion limit: %{sexp:exp}" exp))
  else
    let eval_all = List.map ~f:(eval ~fpar_error ~lim ~ctx) in
    match exp with
    | EFPar _ | ETop _ -> exp
    | EVar x -> (
        match Ctx.lookup_exn ctx x with
        | EVar x -> Ctx.lookup_exn ctx x
        | v -> v)
    | ETuple xs -> ETuple (eval_all xs)
    | EOp (op, args) -> (
        let args_error op args =
          raise
            (TypeCheckError
               (sprintf
                  !"Eval: %{sexp:VCtx.t}, Bad arguments to %{sexp:op}: \
                    %{sexp:exp list}."
                  ctx op args))
        in
        let open Op in
        match op with
        | Ite -> (
            match args with
            | [ pred; e1; e2 ] -> (
                match eval ~fpar_error ~lim ~ctx pred with
                | EOp (True, []) -> eval ~fpar_error ~lim ~ctx e1
                | EOp (False, []) -> eval ~fpar_error ~lim ~ctx e2
                | _ -> args_error op args)
            | _ -> args_error op args)
        | ctor -> EOp (ctor, eval_all args))
    | EFun (pats, defn) -> EFun (pats, defn)
    | EFix (id, pats, defn) -> EFix (id, pats, defn)
    | EApp (((EFun _ | EFix _) as f), arg) -> (
        let arg = eval ~fpar_error ~lim ~ctx arg in
        let (id_opt : string option), pat, defn =
          match f with
          | EFun (pat, defn) -> (None, pat, defn)
          | EFix (id, pat, defn) -> (Some id, pat, defn)
          | _ -> failwith "impossible"
        in
        let emitted_ctx =
          match pattern_match arg pat with
          | Some ctx -> ctx
          | None ->
              failwith
                (sprintf "match failure during function application: %s"
                   (to_string exp))
        in

        match id_opt with
        | Some id ->
            let emitted_ctx = Ctx.bind emitted_ctx id (EFix (id, pat, defn)) in
            eval ~fpar_error ~lim:(lim - 1) ~ctx (subst emitted_ctx defn)
        | None -> eval ~fpar_error ~lim ~ctx (subst emitted_ctx defn))
    | EApp (EFPar in_out_s, arg) -> (
        let arg = eval ~fpar_error ~lim ~ctx arg in
        List.find_map in_out_s ~f:(fun (input, output) ->
            if equal input arg then
              Some output
            else
              None)
        |> function
        | Some o -> o
        | None ->
            if fpar_error then
              raise (PartialFunction arg)
            else
              ETop arg)
    | EApp (f, arg) -> (
        match eval ~fpar_error ~lim ~ctx f with
        | (EFun _ | EFix _ | EFPar _) as f ->
            eval ~fpar_error ~lim ~ctx (EApp (f, arg))
        | _ ->
            raise
              (TypeCheckError
                 (sprintf "Cannot apply expression other than function: %s"
                    (to_string exp))))
    | ELet (pat, _ty, defn, body) ->
        let new_ctx =
          pattern_match (eval ~fpar_error ~lim ~ctx defn) pat
          |> Option.value
               ~default:
                 (raise
                    (TypeCheckError
                       (sprintf "Match failure in let binding: %s"
                          (to_string exp))))
        in
        eval ~fpar_error ~lim ~ctx (subst new_ctx body)
    | EMatch (scrut, rules) ->
        let scrut = eval ~fpar_error ~lim ~ctx scrut in
        let emitted_ctx, subexp =
          (* match scrut against each rules in order *)
          List.fold rules
            ~init:(None : 'a option)
            ~f:(function
              (* match already succeed *)
              | Some success_result -> Fn.const (Some success_result)
              (* match haven't succeeded, therefore attempt to match on the
                 current rule *)
              | None ->
                  fun (pat, subexp) ->
                    pattern_match scrut pat
                    |> Option.map ~f:(fun ctx -> (ctx, subexp)))
          (* match failure after traversing all the rules *)
          |> Option.value_exn
        in
        eval ~fpar_error ~lim ~ctx (subst emitted_ctx subexp)
