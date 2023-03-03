open References
open Core
open Lang

(** list of list of list of random examples generated from sample2.ml, the top
    level list stores `n` sets of `k` examples at `k`th position *)
let random_examples_proj :
    exclude_base:bool ->
    n:int ->
    max_k:int ->
    (unit -> string * (Exp.t * Exp.t) list list list) reference_projection =
 fun ~exclude_base ~n ~max_k ->
  let runner = Denotation.mono in
  {
    proj =
      (fun {
             function_name;
             k_max;
             d_in;
             d_out;
             expert;
             assertion;
             input;
             func;
             background;
           } () ->
        ( function_name,
          List.map
            ~f:(fun k ->
              List.map
                ~f:
                  (List.map ~f:(fun (input_val, output_val) ->
                       (runner d_in input_val, runner d_out output_val)))
                (Sample2.io_trial ~exclude_base ~n ~k func input))
            (List.range ~stop:`inclusive 1 (min k_max max_k)) ));
  }

(* a set of expert examples hard-coded in references.ml *)
let expert_proj :
    (unit ->
    (TCtx.t * VCtx.t) * Typ.t * (Exp.t * Exp.t) list * (Exp.t * Exp.t) list)
    reference_projection =
  let runner = Denotation.mono in
  {
    proj =
      (fun {
             function_name;
             k_max;
             d_in;
             d_out;
             expert;
             assertion;
             input;
             func;
             background;
           } () ->
        let gamma, sigma =
          List.fold background
            ~init:(TCtx.empty (), VCtx.empty ())
            ~f:(fun (gamma, sigma) id ->
              let ty, defn =
                List.Assoc.find_exn ~equal:String.equal Exp.background id
              in
              match function_name with
              | "list_filter" | "list_fold" | "list_map" ->
                  (gamma, VCtx.bind sigma id defn)
              | _ ->
                  (TCtx.bind gamma id (ty, TCtx.Null), VCtx.bind sigma id defn))
        in
        let assertion =
          List.map assertion ~f:(fun (i, o) -> (runner d_in i, runner d_out o))
        in
        let expert =
          List.map expert ~f:(fun (i, o) -> (runner d_in i, runner d_out o))
        in
        ( (gamma, sigma),
          TArr
            ( Exp.infer (fst (List.hd_exn assertion)),
              Exp.infer (snd (List.hd_exn assertion)) ),
          expert,
          assertion ));
  }

let parse_with_errors (type a)
    (parser : (Lexing.lexbuf -> Parser.token) -> Lexing.lexbuf -> a)
    (s : string) : a =
  let lexbuf = Lexing.from_string ~with_positions:true s in
  try parser Lexer.token lexbuf
  with _ ->
    failwith
      ("Unexpected token between positions: ("
      ^ Int.to_string lexbuf.lex_start_p.pos_lnum
      ^ ","
      ^ Int.to_string (lexbuf.lex_start_p.pos_cnum - lexbuf.lex_start_p.pos_bol)
      ^ ") and ("
      ^ Int.to_string lexbuf.lex_curr_p.pos_lnum
      ^ ","
      ^ Int.to_string (lexbuf.lex_curr_p.pos_cnum - lexbuf.lex_curr_p.pos_bol)
      ^ ")")

(* parse random examples from input *)
let parse_io_proj :
    exs_json_str:string ->
    (unit ->
    (TCtx.t * VCtx.t) * Typ.t * (Exp.t * Exp.t) list * (Exp.t * Exp.t) list)
    reference_projection =
 fun ~exs_json_str ->
  let runner = Denotation.mono in
  {
    proj =
      (fun {
             function_name;
             k_max;
             d_in;
             d_out;
             expert;
             assertion;
             input;
             func;
             background;
           } () ->
        let gamma, sigma =
          List.fold background
            ~init:(TCtx.empty (), VCtx.empty ())
            ~f:(fun (gamma, sigma) id ->
              let ty, defn =
                List.Assoc.find_exn ~equal:String.equal Exp.background id
              in
              match function_name with
              | "list_filter" | "list_fold" | "list_map" ->
                  (gamma, VCtx.bind sigma id defn)
              | _ ->
                  (TCtx.bind gamma id (ty, TCtx.Null), VCtx.bind sigma id defn))
        in
        let assertion =
          List.map assertion ~f:(fun (i, o) -> (runner d_in i, runner d_out o))
        in
        (* let json = Yojson.Basic.from_string exs_json_str in *)
        ( (gamma, sigma),
          TArr
            ( Exp.infer (fst (List.hd_exn assertion)),
              Exp.infer (snd (List.hd_exn assertion)) ),
          parse_with_errors Parser.examples exs_json_str,
          (* (let open Yojson.Basic.Util in *)
          (* json |> to_list *)
          (* |> List.map ~f:(fun io -> *)
          (*        match to_list io with *)
          (*        | [ i; o ] -> *)
          (*            let input_val = p_in i in *)
          (*            let output_val = func input_val in *)
          (*            (runner d_in input_val, runner d_out output_val) *)
          (*        | _        -> failwith "json not well-formated")), *)
          assertion ));
  }
(* let newline_regexp = *)
(*   Str.regexp " *\n+ *" *)

(* let left_paren_space_regexp = *)
(*   Str.regexp "( " *)

(* let space_right_paren_regexp = *)
(*   Str.regexp " )" *)

(* let space_comma_regexp = *)
(*   Str.regexp " ," *)

(* let clean_string : string -> string = *)
(*   fun str -> *)
(*     str *)
(*       |> Str.global_replace newline_regexp " " *)
(*       |> Str.global_replace left_paren_space_regexp "(" *)
(*       |> Str.global_replace space_right_paren_regexp ")" *)
(*       |> Str.global_replace space_comma_regexp "," *)

(* let specification_proj : poly:bool -> string reference_projection = fun ~poly
   -> let runner = if poly then Denotation.poly else Denotation.mono in { proj =
   fun { function_name; k_max; d_in; d_out; input; base_case; poly_args ; func }
   -> match Sample2.io_trial ~n:1 ~k:k_max func input base_case with | [ios] ->
   let (arg_lens, inners) = ios |> List.map ( fun (input_val, output_val) -> let
   args = match runner d_in input_val with | Lang.ECtor ("args", [], Lang.ETuple
   args) -> args

   | arg -> [arg] in ( List.length args , "(" ^ ( args @ [runner d_out
   output_val] |> List.map (Pretty.exp >> clean_string) |> String.concat ", " )
   ^ ")" ) ) |> List.split in let arg_len = match List.collapse_equal arg_lens
   with (* Need to replace List2.collapse_equal*) | Some n -> n

   | None -> failwith "Unequal arg lengths in specification_proj" in let
   n_string = if Int.equal arg_len 1 then "" else string_of_int arg_len in let
   first_line = if poly_args = [] then "specifyFunction" ^ n_string ^ " " ^
   function_name else "specifyFunction" ^ n_string ^ " (" ^ function_name ^ " <"
   ^ String.concat ", " (List.map Pretty.typ poly_args) ^ ">)"

   in first_line ^ "\n [ " ^ String.concat "\n , " inners ^ "\n ]"

   | _ -> failwith "Sample2.io_trial didn't return singleton in
   specification_proj" } *)
