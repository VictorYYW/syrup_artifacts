open Z3
module Solver = Z3.Solver
module Model = Z3.Model
module Symbol = Z3.Symbol
module Int = Z3.Arithmetic.Integer
module Sort = Z3.Sort
module Expr = Z3.Expr
module AST = Z3.AST
module Datatype = Z3.Datatype

exception Incon of string
exception Unimplemented

let mk_new_ctx () =
  let cfg = [ ("model", "true"); ("proof", "false") ] in
  mk_context cfg

let ctx = ref @@ mk_new_ctx ()
let solver = ref @@ Solver.mk_solver !ctx None
let ctx_to_string () = Solver.to_string !solver

let reset () =
  ctx := mk_new_ctx ();
  solver := Solver.mk_solver !ctx None

(*
 * Z3 API for the current ctx
 *)

(* let option_para_sort = Sort.mk_uninterpreted_s !ctx "opt0" *)
(* let option_sort = Datatype.mk_sort_s !ctx "option" [Datatype.mk] *)

let sym s = Symbol.mk_string !ctx s
let mk_const_s str sort = Expr.mk_const_s !ctx str sort
let mk_app f args = Expr.mk_app !ctx f args
let int_sort = Int.mk_sort !ctx
let bool_sort = Boolean.mk_sort !ctx
let list_of_int_sort = Z3List.mk_list_s !ctx "int list" int_sort
let list_of_bool_sort = Z3List.mk_list_s !ctx "bool list" bool_sort

let opt_of_int_sort =
  Datatype.mk_sort_s !ctx "int opt"
    [
      Datatype.mk_constructor_s !ctx "Some" (sym "is_some")
        [ sym "content" ]
        [ Some int_sort ] [ 1 ];
      Datatype.mk_constructor_s !ctx "None" (sym "is_none") [] [] [];
    ]

let tree_of_int_sort =
  Datatype.mk_sort_s !ctx "int tree"
    [
      Datatype.mk_constructor_s !ctx "Leaf" (sym "is_leaf") [] [] [];
      Datatype.mk_constructor_s !ctx "Node" (sym "is_node")
        [ sym "data"; sym "left"; sym "right" ]
        [ Some int_sort; None; None ]
        [ 1; 0; 0 ];
    ]

let mk_numeral_i i = Int.mk_numeral_i !ctx i
let mk_uninterpreted_s s = Sort.mk_uninterpreted_s !ctx s
let mk_bool str = Boolean.mk_const_s !ctx str
let get_bool expr = Boolean.is_true expr
let mk_int str = Int.mk_const_s !ctx str
let get_int expr = Z.to_int @@ Int.get_big_int expr
let mk_list_of_int str = mk_const_s str list_of_int_sort
let mk_list_of_bool str = mk_const_s str list_of_bool_sort
let mk_opt_of_int str = mk_const_s str opt_of_int_sort
let mk_tree_of_int str = mk_const_s str tree_of_int_sort
let mk_constructor_s a b c d e = Datatype.mk_constructor_s !ctx a b c d e
let mk_sort_s a b = Datatype.mk_sort_s !ctx a b

let mk_func_decl_s name arg_sorts res_sort =
  FuncDecl.mk_func_decl_s !ctx name arg_sorts res_sort

let mk_and conjs = Boolean.mk_and !ctx conjs
let mk_or conjs = Boolean.mk_or !ctx conjs

let mk_eq es =
  match es with
  | [ x; y ] -> Boolean.mk_eq !ctx x y
  | _ -> failwith "eq expect two arguments"

let mk_neq es =
  match es with
  | [ x; y ] -> Boolean.mk_not !ctx (Boolean.mk_eq !ctx x y)
  | _ -> failwith "eq expect two arguments"

let mk_gt es =
  match es with
  | [ x; y ] -> Arithmetic.mk_gt !ctx x y
  | _ -> failwith "gt expect two arguments"

let mk_lt es =
  match es with
  | [ x; y ] -> Arithmetic.mk_lt !ctx x y
  | _ -> failwith "lt expect two arguments"

let mk_ge es =
  match es with
  | [ x; y ] -> Arithmetic.mk_ge !ctx x y
  | _ -> failwith "ge expect two arguments"

let mk_le es =
  match es with
  | [ x; y ] -> Arithmetic.mk_le !ctx x y
  | _ -> failwith "le expect two arguments"

let mk_not es =
  match es with
  | [ e ] -> Boolean.mk_not !ctx e
  | _ -> failwith "not expect one argument"

let mk_true es =
  match es with
  | [] -> Boolean.mk_true !ctx
  | _ -> failwith "true expect zero argument"

let mk_false es =
  match es with
  | [] -> Boolean.mk_false !ctx
  | _ -> failwith "true expect zero argument"

let mk_ite e1 e2 e3 = Boolean.mk_ite !ctx e1 e2 e3
let mk_distinct es = Boolean.mk_distinct !ctx es
let mk_add es = Arithmetic.mk_add !ctx es
let mk_succ es = Arithmetic.mk_add !ctx (mk_numeral_i 1 :: es)
let mk_sub es = Arithmetic.mk_sub !ctx es
let mk_mul es = Arithmetic.mk_mul !ctx es

let mk_div es =
  match es with
  | [ x; y ] -> Arithmetic.mk_div !ctx x y
  | _ -> failwith "div expect two arguments"

let mk_div2 es =
  match es with
  | [ e ] -> Arithmetic.mk_div !ctx e (mk_numeral_i 2)
  | _ -> failwith "div2 expect one argument"

let mk_mod es =
  match es with
  | [ x; y ] -> Int.mk_mod !ctx x y
  | _ -> failwith "mod expect two arguments"

let mk_nil sort es =
  match es with
  | [] -> Z3List.nil sort
  | _ -> failwith "nil expect zero argument"

let mk_cons sort es =
  match es with
  | [ x; y ] -> FuncDecl.apply (Z3List.get_cons_decl sort) [ x; y ]
  | _ -> failwith "cons expect two arguments"

let mk_some_int es =
  match es with
  | [ e ] ->
      let const_some = Datatype.get_constructors opt_of_int_sort |> List.hd in
      FuncDecl.apply const_some [ e ]
  | _ -> failwith "some_int expect one argument"

let mk_none_int es =
  match es with
  | [] ->
      let const_none = List.nth (Datatype.get_constructors opt_of_int_sort) 1 in
      FuncDecl.apply const_none []
  | _ -> failwith "none_int expect one argument"

let mk_leaf_int es =
  match es with
  | [] ->
      let const_leaf = Datatype.get_constructors tree_of_int_sort |> List.hd in
      FuncDecl.apply const_leaf []
  | _ -> failwith "leaf_int expect zero argument"

let mk_node_int es =
  match es with
  | [ v; l; r ] ->
      let const_node =
        List.nth (Datatype.get_constructors tree_of_int_sort) 1
      in
      FuncDecl.apply const_node [ v; l; r ]
  | _ -> failwith "node_int expect three arguments"

let _assert e = Solver.add !solver [ e ]
let _assert_all e = Solver.add !solver e
let push () = Solver.push !solver
let pop () = Solver.pop !solver 1
let check_sat () = Solver.check !solver []

exception NoModelExists

let get_model () =
  match Solver.get_model !solver with
  | Some model -> model
  | None -> raise NoModelExists

let is_nil_list_of_int expr = Expr.equal expr (mk_nil list_of_int_sort [])

let hd_list_of_int expr =
  let x = mk_int "hd" in
  let hd = FuncDecl.apply (Z3List.get_head_decl list_of_int_sort) [ expr ] in
  match Solver.check !solver [ mk_eq [ x; hd ] ] with
  | UNSATISFIABLE | UNKNOWN -> failwith "hd_list_of_int failure"
  | SATISFIABLE -> (
      match Model.get_const_interp_e (get_model ()) x with
      | Some expr -> expr
      | None -> failwith "impossible")

let tl_list_of_int expr =
  let x = mk_list_of_int "tl" in
  let tl = FuncDecl.apply (Z3List.get_tail_decl list_of_int_sort) [ expr ] in
  match Solver.check !solver [ mk_eq [ x; tl ] ] with
  | UNSATISFIABLE | UNKNOWN -> failwith "tl_list_of_int failure"
  | SATISFIABLE -> (
      match Model.get_const_interp_e (get_model ()) x with
      | Some expr -> expr
      | None -> failwith "impossible")

let is_nil_list_of_bool expr = Expr.equal expr (mk_nil list_of_bool_sort [])

let hd_list_of_bool expr =
  let x = mk_bool "hd" in
  let hd = FuncDecl.apply (Z3List.get_head_decl list_of_bool_sort) [ expr ] in
  match Solver.check !solver [ mk_eq [ x; hd ] ] with
  | UNSATISFIABLE | UNKNOWN -> failwith "hd_list_of_bool failure"
  | SATISFIABLE -> (
      match Model.get_const_interp_e (get_model ()) x with
      | Some expr -> expr
      | None -> failwith "impossible")

let tl_list_of_bool expr =
  let x = mk_list_of_bool "tl" in
  let tl = FuncDecl.apply (Z3List.get_tail_decl list_of_bool_sort) [ expr ] in
  match Solver.check !solver [ mk_eq [ x; tl ] ] with
  | UNSATISFIABLE | UNKNOWN -> failwith "tl_list_of_bool failure"
  | SATISFIABLE -> (
      match Model.get_const_interp_e (get_model ()) x with
      | Some expr -> expr
      | None -> failwith "impossible")

let is_some_int expr = not @@ Expr.equal expr (mk_none_int [])

(* let is_some_decl = Datatype.get_recognizers opt_of_int_sort |> List.hd in *)
(* FuncDecl.apply is_some_decl [ expr ] |> Boolean.is_true *)

let get_some_int expr =
  let get_some_decl =
    Datatype.get_accessors opt_of_int_sort |> List.hd |> List.hd
  in
  Expr.simplify (FuncDecl.apply get_some_decl [ expr ]) None

let mk_is_nonzero exprs =
  match exprs with
  | [ expr ] -> mk_not [ mk_eq [ expr; mk_numeral_i 0 ] ]
  | _ -> failwith "nonzero expect one argument"

let mk_is_even exprs =
  match exprs with
  | [ expr ] -> mk_eq [ mk_mod [ expr; mk_numeral_i 2 ] ]
  | _ -> failwith "is_even expect one argument"

let mk_zero exprs = mk_numeral_i 0

let mk_count_odd exprs =
  match exprs with
  | [ a; x ] ->
      mk_ite
        (mk_eq [ mk_mod [ x; mk_numeral_i 2 ]; mk_numeral_i 0 ])
        a (mk_succ [ a ])
  | _ -> failwith "count_odd expects two arguments"

(* let func = *)
(* FuncDecl.mk_rec_func_decl_s !ctx "count_odd" [ list_of_int_sort ] int_sort *)
(* in *)
(* let xs = mk_list_of_int "xs" in *)
(* let () = *)
(*   FuncDecl.add_rec_def !ctx func [ xs ] *)
(*     (mk_ite *)
(*        (FuncDecl.apply (Z3List.get_is_nil_decl list_of_int_sort) [ xs ]) *)
(*        (mk_numeral_i 0) *)
(*        (let hd = *)
(*           FuncDecl.apply (Z3List.get_head_decl list_of_int_sort) [ xs ] *)
(*         in *)
(*         let tl = *)
(*           FuncDecl.apply (Z3List.get_tail_decl list_of_int_sort) [ xs ] *)
(*         in *)
(*         mk_ite *)
(*           (mk_eq [ mk_mod [ hd; mk_numeral_i 2 ]; mk_numeral_i 0 ]) *)
(*           (FuncDecl.apply func [ tl ]) *)
(*           (mk_succ [ FuncDecl.apply func [ tl ] ]))) *)
(* in *)
(* FuncDecl.apply func exprs *)

let mk_snoc exprs =
  let func =
    FuncDecl.mk_rec_func_decl_s !ctx "snoc"
      [ list_of_int_sort; int_sort ]
      list_of_int_sort
  in
  let xs = mk_list_of_int "xs" in
  let x = mk_int "x" in
  let () =
    FuncDecl.add_rec_def !ctx func [ xs; x ]
      (mk_ite
         (FuncDecl.apply (Z3List.get_is_nil_decl list_of_int_sort) [ xs ])
         (mk_cons list_of_int_sort [ x; mk_nil list_of_int_sort [] ])
         (let hd =
            FuncDecl.apply (Z3List.get_head_decl list_of_int_sort) [ xs ]
          in
          let tl =
            FuncDecl.apply (Z3List.get_tail_decl list_of_int_sort) [ xs ]
          in
          mk_cons list_of_int_sort [ hd; FuncDecl.apply func [ tl; x ] ]))
  in
  FuncDecl.apply func exprs

let mk_append exprs =
  let func =
    FuncDecl.mk_rec_func_decl_s !ctx "append"
      [ list_of_int_sort; list_of_int_sort ]
      list_of_int_sort
  in
  let xs = mk_list_of_int "xs" in
  let ys = mk_list_of_int "ys" in
  let () =
    FuncDecl.add_rec_def !ctx func [ xs; ys ]
      (mk_ite
         (FuncDecl.apply (Z3List.get_is_nil_decl list_of_int_sort) [ xs ])
         ys
         (let hd =
            FuncDecl.apply (Z3List.get_head_decl list_of_int_sort) [ xs ]
          in
          let tl =
            FuncDecl.apply (Z3List.get_tail_decl list_of_int_sort) [ xs ]
          in
          mk_cons list_of_int_sort [ hd; FuncDecl.apply func [ tl; ys ] ]))
  in
  FuncDecl.apply func exprs

let mk_append_bool exprs =
  let func =
    FuncDecl.mk_rec_func_decl_s !ctx "append_bool"
      [ list_of_bool_sort; list_of_bool_sort ]
      list_of_bool_sort
  in
  let xs = mk_list_of_bool "xs" in
  let ys = mk_list_of_bool "ys" in
  let () =
    FuncDecl.add_rec_def !ctx func [ xs; ys ]
      (mk_ite
         (FuncDecl.apply (Z3List.get_is_nil_decl list_of_bool_sort) [ xs ])
         ys
         (let hd =
            FuncDecl.apply (Z3List.get_head_decl list_of_bool_sort) [ xs ]
          in
          let tl =
            FuncDecl.apply (Z3List.get_tail_decl list_of_bool_sort) [ xs ]
          in
          mk_cons list_of_bool_sort [ hd; FuncDecl.apply func [ tl; ys ] ]))
  in
  FuncDecl.apply func exprs

let mk_insert exprs =
  let func =
    FuncDecl.mk_rec_func_decl_s !ctx "insert"
      [ list_of_int_sort; int_sort ]
      list_of_int_sort
  in
  let xs = mk_list_of_int "xs" in
  let x = mk_int "x" in
  let () =
    FuncDecl.add_rec_def !ctx func [ xs; x ]
      (mk_ite
         (FuncDecl.apply (Z3List.get_is_nil_decl list_of_int_sort) [ xs ])
         (mk_cons list_of_int_sort [ x; mk_nil list_of_int_sort [] ])
         (let hd =
            FuncDecl.apply (Z3List.get_head_decl list_of_int_sort) [ xs ]
          in
          let tl =
            FuncDecl.apply (Z3List.get_tail_decl list_of_int_sort) [ xs ]
          in
          mk_ite
            (mk_lt [ hd; x ])
            (mk_cons list_of_int_sort [ hd; FuncDecl.apply func [ tl; x ] ])
            (mk_ite (mk_eq [ hd; x ]) xs (mk_cons list_of_int_sort [ x; xs ]))))
  in
  FuncDecl.apply func exprs

let get_list_of_int expr = ()
let mk_unimplemented exprs = raise Unimplemented
