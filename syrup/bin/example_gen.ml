open Core
open Lib
open Lang

let max_card = 10

let trials = 50

let rec val_to_str : exp -> string =
  let all = List.map ~f:val_to_str in
  function
  | EVar x -> x
  | ETuple es -> "(" ^ String.concat ~sep:"," (all es) ^ ")"
  | EOp (True, _) -> "T ()"
  | EOp (False, _) -> "F ()"
  | EOp ((Zero | Succ), _) as nat -> Int.to_string @@ Exp.to_int nat
  | EOp ((Nil | Cons), _) as list ->
      "[" ^ String.concat ~sep:"," (all (Exp.exps_of_list list)) ^ "]"
  | EOp (Leaf, []) -> "Leaf ()"
  | EOp (Node, [ v; l; r ]) ->
      "Node (" ^ val_to_str l ^ "," ^ val_to_str v ^ "," ^ val_to_str r ^ ")"
  | EOp (No, []) -> "None"
  | EOp (Just, [ e ]) -> "Some (" ^ val_to_str e ^ ")"
  | _ -> failwith "exp_to_str failure"

let ex_to_str (i, o) = val_to_str i ^ " -> " ^ val_to_str o ^ ";"

let exs_to_str exs = List.map exs ~f:ex_to_str |> String.concat ~sep:"\n"

let () =
  List.iter
    (References.all
       (Fuzz.random_examples_proj ~exclude_base:true ~n:50 ~max_k:max_card))
    ~f:(fun (name, delayed_exs_list_list) ->
      let fun_name, exs_list_list = delayed_exs_list_list () in
      assert (String.equal name fun_name);
      (* pointless :) *)
      List.iteri exs_list_list ~f:(fun i exs_list ->
          Out_channel.with_file
            ("experiment/io-random-nobase/" ^ name ^ Int.to_string (i + 1))
            ~f:(fun oc ->
              List.iter exs_list ~f:(fun exs ->
                  fprintf oc "%s\n" (exs_to_str exs)))));
  List.iter
    (References.all
       (Fuzz.random_examples_proj ~exclude_base:false ~n:50 ~max_k:max_card))
    ~f:(fun (name, delayed_exs_list_list) ->
      let fun_name, exs_list_list = delayed_exs_list_list () in
      assert (String.equal name fun_name);
      (* pointless :) *)
      List.iteri exs_list_list ~f:(fun i exs_list ->
          Out_channel.with_file
            ("experiment/io-random/" ^ name ^ Int.to_string (i + 1))
            ~f:(fun oc ->
              List.iter exs_list ~f:(fun exs ->
                  fprintf oc "%s\n" (exs_to_str exs)))))
