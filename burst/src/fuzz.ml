open MyStdLib
open References
open Lang

let parse_with_errors (type a)
    (parser : (Lexing.lexbuf -> Parser2.token) -> Lexing.lexbuf -> a)
    (s : string) : a =
  let lexbuf = Lexing.from_string ~with_positions:true s in
  try parser Lexer2.token lexbuf
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

(* parse random examples from json *)
let parse_io_proj :
  fname:String.t ->
    exs_str:String.t ->
    Problem.t ->
    (unit ->
    (Value.t * Value.t) list 
    * (Value.t * Value.t) list)
    reference_projection =
 fun ~fname ~exs_str problem ->
  {
    proj =
      (fun {
             function_name;
             k_max;
             d_in;
             d_out;
             assertion;
             func;
             _;
           } () ->
        let i_e = problem.full_eval_context in
        let rec concat_L_input (e: Expr.t) : Expr.t =
          match Expr.node e with
              | Ctor (Id "Nil", _) -> Expr.mk_ctor (Id.create "LNil") Expr.mk_unit
              | Ctor (Id "Cons", hd_tl) ->
                (match Expr.node hd_tl with
                 | Tuple [hd; tl] ->
                   Expr.mk_ctor (Id.create "LCons") (Expr.mk_tuple [hd; concat_L_input tl])
                | _ -> failwith "impossible0")
              | _ -> failwith (Printf.sprintf "impossible: %s" (Expr.show e))
            in
        let io_expr_to_val (es, e) = 
              let vs =
                List.map
                  ~f:(Eval.evaluate_with_holes ~eval_context:i_e)
                  es
              in
              let v =
                Eval.evaluate_with_holes
                  ~eval_context:i_e
                  e
              in
              (Value.mk_tuple vs,v) in
        let assertion =
          List.map assertion ~f:(fun (i, o) -> io_expr_to_val (d_in i, d_out o))
        in
        let exs = parse_with_errors Parser2.examples exs_str in
        List.map exs ~f:(fun (es, o) -> io_expr_to_val ((if String.equal fname "list_concat" then List.map es ~f:concat_L_input else es), o) ), assertion
        );
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
