open Core
open Util
include Lang

module Input = struct
  type t = {
    inits : (texp * TCtx.bindspec) list;
    goal_type : typ;
    goal_bindspec : TCtx.bindspec;
    size : int;
  }
  [@@deriving sexp, compare, hash]
end

let rec size_partition (size : int) (arity : int) : int list list =
  match arity with
  | 0 ->
      if size = 0 then
        [ [] ]
      else
        []
  | 1 ->
      if size > 0 then
        [ [ size ] ]
      else
        []
  | n ->
      List.fold
        (List.range ~start:`inclusive ~stop:`inclusive 1 size)
        ~init:[]
        ~f:(fun partitions cur ->
          (size_partition (size - cur) (arity - 1)
          |> List.map ~f:(List.cons cur))
          @ partitions)

let cache : (Input.t, texp list) Hashtbl.t =
  Hashtbl.create (module Input) ~size:100

let rec run ({ inits; goal_type; goal_bindspec; size } as input : Input.t) :
    texp list =
  match Hashtbl.find cache input with
  | Some exps -> exps
  | None      ->
      if size = 1 then
        List.filter_map inits ~f:(fun (texp, bindspec) ->
            if
              Typ.is_unifiable (TExp.to_typ texp) goal_type
              && (TCtx.compare_bindspec goal_bindspec Null = 0
                 || TCtx.compare_bindspec bindspec goal_bindspec = 0)
            then
              Some texp
            else
              None)
      else
        List.concat_map (List.range 1 size) ~f:(fun size ->
            run { inits; goal_type; goal_bindspec; size })
        @ List.concat_map inits ~f:(fun (texp, bindspec) ->
              let exp = TExp.to_exp texp in
              let typ = TExp.to_typ texp in
              match typ with
              | TArr (_, output_type)
                when Typ.is_unifiable output_type goal_type -> (
                  let goal_type' = Typ.instantiate 0 goal_type in
                  let typ' = Typ.instantiate 0 typ in
                  let output_type' = Typ.output_typ typ' in
                  Typ.unify_exn goal_type' output_type';

                  (* let typ' = Typ.normalize (Typ.generalize (-1) typ') in *)
                  let arg_typs = typ' |> Typ.input_typ |> Typ.to_list in
                  let arity = List.length arg_typs in
                  match bindspec with
                  | Rec ->
                      if arity + 1 = size then
                        List.range 0 arity
                        |> List.concat_map ~f:(fun i ->
                               List.mapi arg_typs ~f:(fun i' arg_typ ->
                                   if i = i' then
                                     run
                                       {
                                         inits;
                                         goal_type = arg_typ;
                                         goal_bindspec = Dec i;
                                         size = 1;
                                       }
                                   else
                                     run
                                       {
                                         inits;
                                         goal_type = arg_typ;
                                         goal_bindspec = Dec i;
                                         size = 1;
                                       }
                                     @ run
                                         {
                                           inits;
                                           goal_type = arg_typ;
                                           goal_bindspec = Arg i;
                                           size = 1;
                                         })
                               |> List.cprod
                               |> List.map ~f:(fun args ->
                                      TEApp
                                        (texp, TExp.of_list args, output_type')))
                      else
                        []
                  | _   ->
                      size_partition (size - 1) arity
                      |> List.concat_map ~f:(fun arg_sizes ->
                             List.map2_exn arg_typs arg_sizes
                               ~f:(fun arg_typ arg_size ->
                                 run
                                   {
                                     inits;
                                     goal_type = arg_typ;
                                     goal_bindspec = Null;
                                     size = arg_size;
                                   })
                             |> List.cprod
                             |> List.map ~f:(fun args ->
                                    TEApp (texp, TExp.of_list args, output_type')))
                  )
              | _ -> [])
        @ List.concat_map Op.constructors ~f:(fun op ->
              let typ = Op.typ op in
              match typ with
              | TArr (_, output_type)
                when Typ.is_unifiable output_type goal_type ->
                  let goal_type' = Typ.instantiate 0 goal_type in
                  let typ' = Typ.instantiate 0 typ in
                  let output_type' = Typ.output_typ typ' in
                  Typ.unify_exn goal_type' output_type';

                  (* let typ' = Typ.normalize (Typ.generalize (-1) typ') in *)
                  let arg_typs = typ' |> Typ.input_typ |> Typ.to_list in
                  let arity = List.length arg_typs in
                  size_partition (size - 1) arity
                  |> List.concat_map ~f:(fun arg_sizes ->
                         List.map2_exn arg_typs arg_sizes
                           ~f:(fun arg_typ arg_size ->
                             run
                               {
                                 inits;
                                 goal_type = arg_typ;
                                 goal_bindspec = Null;
                                 size = arg_size;
                               })
                         |> List.cprod
                         |> List.map ~f:(fun args ->
                                TEOp (op, args, output_type')))
              | _ -> [])

(* Enum.run *)
(*   { *)
(*     inits = *)
(*       List.map default_init ~f:(fun x -> (x, TCtx.Null)) *)
(*       @ List.map (TCtx.to_alist gamma) ~f:(fun (id, (ty, spec)) -> *)
(*             (TEVar (id, ty), spec)); *)
(*     goal_type = ty; *)
(*     goal_bindspec = Null; *)
(*     size = n; *)
(*   } *)
(* |> fun texps -> *)
(* List.iter texps ~f:(printf !"%{sexp:texp}\n"); *)
(* List.fold texps ~init:Result.empty ~f:(fun acc texp -> *)
(*     match check_consistency texp with *)
(*     | Some (exp, asm_list) -> *)
(*         List.fold asm_list ~init:acc ~f:(fun acc asm -> *)
(*             let sigmas' = *)
(*               VCtxSet.map sigmas ~f:(fun sigma -> *)
(*                   match VCtx.lookup_exn sigma fun_name with *)
(*                   | EFPar defn -> *)
(*                       EFPar (Examples.Asm.to_alist asm @ defn) *)
(*                       |> VCtx.bind sigma fun_name *)
(*                   | _          -> failwith "impossible") *)
(*             in *)
(*             Result.add acc asm (VSA.of_exp gamma sigmas exp)) *)
(*     | None                 -> acc) *)
