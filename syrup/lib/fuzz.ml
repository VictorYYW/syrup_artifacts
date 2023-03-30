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

let parse_io_proj :
    exs_str:string ->
    (unit ->
    (TCtx.t * VCtx.t) * Typ.t * (Exp.t * Exp.t) list * (Exp.t * Exp.t) list)
    reference_projection =
 fun ~exs_str ->
  let runner = Denotation.mono in
  {
    proj =
      (fun {
             function_name;
             k_max;
             d_in;
             d_out;
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
        ( (gamma, sigma),
          TArr
            ( Exp.infer (fst (List.hd_exn assertion)),
              Exp.infer (snd (List.hd_exn assertion)) ),
          parse_with_errors Parser.examples exs_str,
          assertion ));
  }
