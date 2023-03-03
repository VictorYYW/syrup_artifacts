open Core
open Util

let fun_name = Fresh.name 'f'
let var_count = ref 0

type level = int [@@deriving sexp, compare, hash]

type typ =
  | TVar of (var_t ref[@hash.ignore])
  | TBool
  | TNum
  | TOpt of typ
  | TList of typ
  | TTree of typ
  | TProd of typ list
  | TArr of typ * typ

and var_t = Free of int * level | Link of typ | Quant of string
[@@deriving sexp, compare, hash]

type pat =
  | PVar of string
  | PTrue
  | PFalse
  | PZero
  | PSucc of pat
  | PNil
  | PCons of pat * pat
  | PLeaf
  | PNode of pat * pat * pat
  | PTuple of pat list
[@@deriving sexp, compare]

type op =
  | No
  | Just
  | True
  | False
  | Zero
  | Succ
  | Nil
  | Cons
  | Leaf
  | Node
  | Ite
(* (\* comparison *\) *)
(* | Lt *)
(* | Eq *)
(* | Leq *)
(* | Neq *)
(* (\* boolean *\) *)
(* | Not *)
(* | And *)
(* | Or *)
(* (\* arithmetic *\) *)
(* | Plus *)
(* | Minus *)
(* | Mul *)
(* | Div *)
(* | Mod *)
[@@deriving sexp, compare, hash]

type exp =
  | EVar of string
  | EOp of op * exp list
  | EApp of exp * exp
  | ETuple of exp list
  | EMatch of exp * branch list
  | ELet of (pat[@hash.ignore]) * (typ option[@hash.ignore]) * exp * exp
  | EFun of (pat[@hash.ignore]) * exp
  | EFix of string * (pat[@hash.ignore]) * exp
  (* partial function *)
  | EFPar of (exp * exp) list
  (* equal to any expression *)
  | ETop of exp

and branch = (pat[@hash.ignore]) * exp [@@deriving sexp, compare, hash]

type texp =
  | TEVar of string * typ
  | TETuple of texp list * typ
  | TEOp of op * texp list * typ
  | TEMatch of texp * tbranch list * typ
  | TELet of (pat[@hash.ignore]) * typ option * texp * texp * typ
  | TEFun of (pat[@hash.ignore]) * texp * typ
  | TEFix of string * (pat[@hash.ignore]) * texp * typ
  | TEApp of texp * texp * typ

and tbranch = (pat[@hash.ignore]) * texp [@@deriving sexp, compare, hash]
