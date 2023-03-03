open Core
open Util
open Lang

type t = pat [@@deriving sexp, compare]

(** take a tuple pattern and return a pattern list *)
let to_list : t -> t list = function
  | PTuple pats -> pats
  | pat         -> [ pat ]

(** take a list of patterns and transform into one tuplized pattern *)
let of_list : t list -> t = function
  | []         -> failwith "cannot transform empty pattern list to a pattern"
  | [ single ] -> single
  | pats       -> PTuple pats

let rec to_string : t -> string = function
  | PVar id           -> id
  | PTrue             -> "T"
  | PFalse            -> "F"
  | PNil              -> "[]"
  | PCons (hd, tl)    -> to_string hd ^ "::" ^ to_string tl
  | PZero             -> "Z"
  | PSucc p           -> "S" ^ to_string p
  | PLeaf             -> "Leaf"
  | PNode (p, t1, t2) ->
      "Node" ^ "(" ^ to_string p ^ "," ^ to_string t1 ^ "," ^ to_string t2 ^ ")"
  | PTuple pats       ->
      "(" ^ (pats |> List.map ~f:to_string |> String.concat ~sep:", ") ^ ")"

let new_var () : t = PVar (Fresh.name 'p')

let new_vars (n : int) : t list =
  Fresh.names 'p' n |> List.map ~f:(fun x -> PVar x)

let get_id : t -> string = function
  | PVar id -> id
  | _       -> "get_id does not work on non-var patterns"

let rec get_ids : t -> string list = function
  | PVar id -> [ id ]
  | PTrue | PFalse | PNil | PZero | PLeaf -> []
  | PSucc x -> get_ids x
  | PCons (x, y) -> get_ids x @ get_ids y
  | PNode (p, t1, t2) -> get_ids p @ get_ids t1 @ get_ids t2
  | PTuple pats -> List.fold pats ~init:[] ~f:(fun acc pat -> acc @ get_ids pat)

let rec get_ids_typs (pat : t) (typ : Typ.t) : (string * typ) list =
  match pat with
  | PVar id -> [ (id, typ) ]
  | PTrue | PFalse | PNil | PZero | PLeaf -> []
  | PSucc x -> get_ids_typs x typ
  | PCons (x, y) -> (
      match typ with
      | TList typ' -> get_ids_typs x typ' @ get_ids_typs y typ
      | _          ->
          failwith
            "found Cons pattern is not bound with list type in 'get_ids_typs'")
  | PNode (p, x, y) -> (
      match typ with
      | TTree typ' ->
          get_ids_typs p typ' @ get_ids_typs x typ @ get_ids_typs y typ
      | _          ->
          failwith
            "found Cons pattern is not bound with list type in 'get_ids_typs'")
  | PTuple pats -> (
      match typ with
      | TProd typs -> (
          match
            List.fold2 pats typs ~init:[] ~f:(fun acc pat typ ->
                acc @ get_ids_typs pat typ)
          with
          | Ok res          -> res
          | Unequal_lengths ->
              failwith
                "Tuple pattern and product type has different arities in \
                 `get_ids_typs'")
      | _          ->
          failwith
            "found Tuple pattern is not bound with product type in \
             `get_ids_typs'")
