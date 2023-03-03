open Core
open Util
open Lang

module OpMap = Map.Make (struct
  type t = op

  let t_of_sexp = op_of_sexp

  let sexp_of_t = sexp_of_op

  let compare = compare_op
end)

type t = op [@@deriving sexp, compare, hash]

type metadata = { typ : typ; str : string; cost : int }

let ops =
  [
    (Zero, { typ = TNum; str = "Zero"; cost = 1 });
    (Succ, { typ = TArr (TNum, TNum); str = "Succ"; cost = 1 });
    (False, { typ = TBool; str = "False"; cost = 1 });
    (True, { typ = TBool; str = "True"; cost = 1 });
    (No, { typ = TOpt (Typ.fresh_free 0); str = "None"; cost = 1 });
    ( Just,
      {
        typ = TArr (TVar (ref (Quant "a")), TOpt (TVar (ref (Quant "a"))));
        str = "Some";
        cost = 1;
      } );
    (Nil, { typ = TList (Typ.fresh_free 0); str = "Nil"; cost = 1 });
    ( Cons,
      {
        typ =
          TArr
            ( TProd [ TVar (ref (Quant "a")); TList (TVar (ref (Quant "a"))) ],
              TList (TVar (ref (Quant "a"))) );
        str = "Cons";
        cost = 1;
      } );
    (Leaf, { typ = TTree (Typ.fresh_free 0); str = "Leaf"; cost = 1 });
    ( Node,
      {
        typ =
          TArr
            ( TProd
                [
                  TVar (ref (Quant "a"));
                  TTree (TVar (ref (Quant "a")));
                  TTree (TVar (ref (Quant "a")));
                ],
              TTree (TVar (ref (Quant "a"))) );
        str = "Node";
        cost = 1;
      } );
    ( Ite,
      {
        typ =
          TArr
            ( TProd [ TBool; TVar (ref (Quant "a")); TVar (ref (Quant "a")) ],
              TVar (ref (Quant "a")) );
        str = "if";
        cost = 1;
      } );
  ]

let op_map = OpMap.of_alist_exn ops

let all = OpMap.keys op_map

let constructors = [ Zero; Succ; False; True; Nil; Cons; No; Just; Leaf; Node ]

let empty = []

(* List.filter_map ops ~f:(fun (op, meta) ->
 *     if meta.count > 0 then Some op else None) *)

let meta : t -> metadata = OpMap.find_exn op_map

let typ (op : t) : typ = (meta op).typ

let arity op =
  match (meta op).typ with
  | TArr (TProd ts, _) -> List.length ts
  | TArr (_, _)        -> 1
  | _                  -> 0

let str op = (meta op).str

let cost (op : t) : int = (meta op).cost
