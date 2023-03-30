(** the file is mostly borrowed from Smyth *)
open Core

(*******************************************************************************
 * Parameters
 *)

let max_nat : int = 3

let max_nat_list_length : int = 4

let max_nested_nat_list_length : int = 4

let max_nested_nat_inner_list_length : int = 2

let max_bool_list_length : int = 4

let max_nat_tree_size : int = 4

let max_bool_tree_size : int = 6

(*******************************************************************************
 * Enumeration Sampling
 *)

(* Generic *)

type 'a gen = ?exclude_base:bool -> unit -> 'a

let weight : int -> int -> float = fun _elementSize _size -> 1.0
(* or: elementSize ^ size *)

let all ?(exclude_base = false) :
    'a -> (int -> 'a list) -> int -> int -> (float * 'a) * (float * 'a) list =
 fun base shapes element_size max_size ->
  ( ( (if exclude_base then
        0.0
      else
        1.0),
      base ),
    List.concat_map
      ~f:(fun size ->
        size |> shapes
        |> List.map ~f:(fun shape -> (weight element_size size, shape)))
      (List.range 1 max_size) )

let constant : 'a -> 'a gen = fun x ?(exclude_base = false) () -> x

let from : 'a * 'a list -> 'a gen =
 fun (x, xs) ?(exclude_base = false) () ->
  let choice = Random.int (List.length xs + 1) in
  if Int.equal choice 0 then
    x
  else
    List.nth_exn xs (choice - 1)

let pair : 'a gen -> 'b gen -> ('a * 'b) gen =
 fun x y ?(exclude_base = false) () -> (x ~exclude_base (), y ~exclude_base ())

let triple : 'a gen -> 'b gen -> 'c gen -> ('a * 'b * 'c) gen =
 fun x y z ?(exclude_base = false) () ->
  (x ~exclude_base (), y ~exclude_base (), z ~exclude_base ())

(* Semi-Generic *)

type list_shape = Nil | Cons of list_shape

let list_base : list_shape = Nil

let rec list_shapes : int -> list_shape list =
 fun n ->
  if n = 0 then
    [ list_base ]
  else
    List.map ~f:(fun s -> Cons s) (list_shapes (n - 1))

let rec list_fill : 'a gen -> list_shape -> 'a list =
 fun gen shape ->
  match shape with
  | Nil       -> []
  | Cons rest -> gen () :: list_fill gen rest

let list : int -> int -> 'a gen -> 'a list gen =
 fun max_list_size element_size element_gen ?(exclude_base = false) () ->
  all ~exclude_base list_base list_shapes element_size max_list_size
  |> Util.uncurry Random2.weighted
  |> list_fill element_gen

let nested_list : int -> int -> int -> 'a gen -> 'a list list gen =
 fun max_list_length max_inner_list_length element_size element_gen
     ?(exclude_base = false) () ->
  let inner_list_size =
    List.range 0 max_inner_list_length
    |> List.map ~f:(fun len -> Int.pow element_size len)
    |> List.fold ~f:( + ) ~init:0
  in
  all ~exclude_base list_base list_shapes inner_list_size max_list_length
  |> Util.uncurry Random2.weighted
  |> list_fill (list max_inner_list_length element_size element_gen)

type tree_shape = Leaf | Node of tree_shape * tree_shape

let tree_base : tree_shape = Leaf

let rec tree_shapes : int -> tree_shape list =
 fun n ->
  if n = 0 then
    [ tree_base ]
  else
    List.concat_map
      ~f:(fun k ->
        List.map ~f:(fun (left, right) -> Node (left, right))
        @@ List.cartesian_product (tree_shapes k) (tree_shapes @@ (n - 1 - k)))
      (List.range 0 n)

let rec tree_fill : 'a gen -> tree_shape -> 'a Tree2.t =
 fun gen t ->
  match t with
  | Leaf               -> Tree2.Leaf
  | Node (left, right) ->
      Tree2.Node (tree_fill gen left, gen (), tree_fill gen right)

let tree : int -> int -> 'a gen -> 'a Tree2.t gen =
 fun max_tree_size element_size element_gen ?(exclude_base = false) () ->
  all ~exclude_base tree_base tree_shapes element_size max_tree_size
  |> (fun (x, y) -> Random2.weighted x y)
  |> tree_fill element_gen

(* Particular *)

let nat : int gen = fun ?(exclude_base = false) () -> Random.int (max_nat + 1)

let bool : bool gen = fun ?(exclude_base = false) () -> Random.bool ()

let nat_list : int list gen = list max_nat_list_length (max_nat + 1) nat

let nested_nat_list : int list list gen =
  nested_list max_nested_nat_list_length max_nested_nat_inner_list_length
    (max_nat + 1) nat

let bool_list : bool list gen = list max_bool_list_length 2 bool

let nat_tree : int Tree2.t gen = tree max_nat_tree_size (max_nat + 1) nat

let bool_tree : bool Tree2.t gen = tree max_bool_tree_size 2 bool

(*******************************************************************************
 * IO Sampling
 *)

let io : ('a -> 'b) -> 'a gen -> ('a * 'b) gen =
 fun f gen ?(exclude_base = false) () ->
  let x = gen ~exclude_base () in
  (x, f x)

let io_trial :
    ?exclude_base:bool ->
    n:int ->
    k:int ->
    ('a -> 'b) ->
    'a gen ->
    ('a * 'b) list list =
 fun ?(exclude_base = false) ~n ~k ref input ->
  List.init n ~f:(fun _ ->
      let amounts = [ (k, io ref input ~exclude_base) ] in
      Random2.sample_unique amounts)
