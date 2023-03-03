open MyStdLib
open Burst
open Lang

module Time = struct
  open Core
  include Time

  let timeout = 60

  exception Timeout

  let delayed_fun f =
    let (_ : unit) =
      Stdlib.Sys.set_signal Stdlib.Sys.sigalrm
        (Stdlib.Sys.Signal_handle (fun _ -> raise Timeout))
    in
    ignore (Unix.alarm timeout : int);
    try
      let r = f () in
      ignore (Unix.alarm 0 : int);
      r
    with e ->
      ignore (Unix.alarm 0 : int);
      raise e

  let timed f : 'a * float =
    let start_time = now () in
    (* let result = delayed_fun f in *)
    let result = f () in
    let end_time = now () in
    let time = Span.to_sec (Time.diff end_time start_time) in
    (result, time)
end
module A = SmythSynthesizer


let rec import_imports
    (acc:Problem.t_unprocessed)
  : Problem.t_unprocessed =
  begin match Problem.extract_file acc with
    | None -> acc
    | Some (fname,acc) ->
      let (news,newd) =
        ParserContainer.import_decls_start (MyStdLib.SimpleFile.read_from_file ~fname)
      in
      let news = Problem.update_import_base news fname in
      import_imports
        (Problem.merge_unprocessed
           acc
           (news,newd))
  end

module Crazy = CrazyFTASynthesizer.Create(Automata.TimbukBuilder)
module Simple = CrazyFTASynthesizer.Create(Automata.TimbukBuilder)
module VEQ = Synthesizers.VerifiedEquiv.Make(Synthesizers.IOSynth.OfPredSynth(Crazy))(EnumerativeVerifier.T)
module SimpleVEQ = Synthesizers.VerifiedEquiv.Make(Synthesizers.IOSynth.OfPredSynth(Simple))(EnumerativeVerifier.T)

let get_ioe_synthesizer
    ~(use_myth:bool)
    ~(use_smyth:bool)
    ~(use_simple:bool)
    ~(use_l2:bool)
    ~(tc_synth:bool)
  : (module Synthesizers.IOSynth.S) =
  let synth =
    if use_myth then
      (module MythSynthesisCaller : Synthesizers.IOSynth.S)
    else if use_l2 then
      (module L2SynthesisCaller : Synthesizers.IOSynth.S)
    else if use_smyth then
      (module SmythSynthesizer.T : Synthesizers.IOSynth.S)
    else if use_simple then
      let builder =
        (module Automata.TimbukBuilder : Automata.AutomatonBuilder)
      in
      (module Synthesizers.IOSynth.OfPredSynth(SimpleFTASynthesizer.Create(val builder)) : Synthesizers.IOSynth.S)
    else
      let builder =
        (module Automata.TimbukBuilder : Automata.AutomatonBuilder)
      in
      (module Synthesizers.IOSynth.OfPredSynth(CrazyFTASynthesizer.Create(val builder)) : Synthesizers.IOSynth.S)
  in
  if tc_synth then
    (module Synthesizers.IOSynth.TCToNonTC(val synth) : Synthesizers.IOSynth.S)
  else
    synth


let synthesize_satisfying_verified_equiv
    ~(problem:Problem.t)
    ~(context:Context.t)
    ~(tin:Type.t)
    ~(tout:Type.t)
    ~(equiv:Value.t -> Value.t)
    ~(use_myth:bool)
    ~(use_smyth:bool)
    ~(use_simple:bool)
    ~(use_l2:bool)
    ~(tc_synth:bool)
  : Expr.t =
  let synth = get_ioe_synthesizer ~use_myth ~use_smyth ~use_simple ~use_l2 ~tc_synth in
  let module S = Synthesizers.VerifiedEquiv.Make(val synth)(EnumerativeVerifier.T) in
  S.synth ~problem ~context ~tin ~tout equiv

let get_predicate_synthesizer
    ~(use_simple:bool)
  : (module Synthesizers.PredicateSynth.S) =
  if use_simple then
    let builder =
      (module Automata.TimbukBuilder : Automata.AutomatonBuilder)
    in
    (module SimpleFTASynthesizer.Create(val builder) : Synthesizers.PredicateSynth.S)
  else
    let builder =
      (module Automata.TimbukBuilder : Automata.AutomatonBuilder)
    in
    (module CrazyFTASynthesizer.Create(val builder) : Synthesizers.PredicateSynth.S)

let synthesize_satisfying_postcondition
    ~(problem:Problem.t)
    ~(context:Context.t)
    ~(tin:Type.t)
    ~(tout:Type.t)
    ~(post:Value.t -> Value.t -> bool)
    ~(use_myth:bool)
    ~(use_smyth:bool)
    ~(use_simple:bool)
    ~(use_l2:bool)
    ~(tc_synth:bool)
  : Expr.t =
  if use_myth then failwith "invalid synthesizer for postconditions";
  if use_l2 then failwith "invalid synthesizer for postconditions";
  if tc_synth then failwith "invalid synthesizer for postconditions";
  let synth = get_predicate_synthesizer ~use_simple in
  let module S = Synthesizers.VerifiedPredicate.Make(val synth)(EnumerativeVerifier.T) in
  S.synth ~problem ~context ~tin ~tout post

let synthesize_satisfying_ioes
    ~(problem:Problem.t)
    ~(context:Context.t)
    ~(tin:Type.t)
    ~(tout:Type.t)
    ~(ioes:(Value.t * Value.t) list)
    ~(use_myth:bool)
    ~(use_simple:bool)
    ~(use_smyth:bool)
    ~(use_l2:bool)
    ~(tc_synth:bool)
  : Expr.t =
  let synth =
    if use_myth then
      (module MythSynthesisCaller : Synthesizers.IOSynth.S)
    else if use_smyth then
      (module SmythSynthesizer.T : Synthesizers.IOSynth.S)
    else
      begin
        let synth = get_predicate_synthesizer ~use_simple in
        (module Synthesizers.VerifiedPredicate.ToIOSynth(Synthesizers.VerifiedPredicate.Make(val synth)))
      end
  in
  let module S = (val synth) in
  snd (S.synth (S.init ~problem ~context ~tin ~tout) ioes)

let check_equivalence
    ~(fname:string)
    ~(ce1:string)
    ~(ce2:string)
  : unit =
  let p_unprocessed =
    ParserContainer.unprocessed_problem (MyStdLib.SimpleFile.read_from_file ~fname)
  in
  let p_unprocessed = Problem.update_all_import_bases p_unprocessed fname in
  let p_unprocessed = import_imports p_unprocessed in
  let problem = Problem.process p_unprocessed in
  let context : Context.t = Problem.extract_context problem in
  let ref_e =
    ParserContainer.exp
      (MyStdLib.SimpleFile.read_from_file ~fname:ce1)
  in
  let cand =
    Parser.exp
      Lexer.token
      (Lexing.from_string
         (MyStdLib.SimpleFile.read_from_file ~fname:ce2))
  in
  let (tin,tout) = problem.synth_type in
  let checker inp outp =
    let inpe = Lang.Value.to_exp inp in
    let apped = Expr.mk_app ref_e inpe in
    let evaled = Eval.evaluate_with_holes ~eval_context:context.evals apped in
    Value.equal evaled outp
  in
  let ceo =
    EnumerativeVerifier.T.satisfies_post
      ~context
      ~tin
      ~tout
      ~cand
      ~checker
  in
  begin match ceo with
    | None -> ()
    | Some _ -> failwith "not equiv"
  end

let synthesize_solution
    ~(fname:string)
    ~(use_myth:bool)
    ~(use_smyth:bool)
    ~(use_simple:bool)
    ~(use_l2:bool)
    ~(log:bool)
    ~(no_experiments:bool)
    ~(print_times:bool)
    ~(tc_synth:bool)
  : unit =
  Consts.logging := log;
  let p_unprocessed =
    ParserContainer.unprocessed_problem
      ((MyStdLib.SimpleFile.read_from_file ~fname))
  in
  let p_unprocessed = Problem.update_all_import_bases p_unprocessed fname in
  let p_unprocessed = import_imports p_unprocessed in
  let problem = Problem.process p_unprocessed in
  (*print_endline (Expr.show (Crazy.simple_synth ~problem));*)
  (*let synth =
    if use_myth then
      (module MythSynthesisCaller : Synthesizer.S)
    else if use_l2 then
      (module L2SynthesisCaller : Synthesizer.S)
    else
      let builder =
        if use_timbuk then
          (module Automata.TimbukBuilder : Automata.AutomatonBuilder)
        else
          (module TimbukVataBuilder.Make : Automata.AutomatonBuilder)
      in
      if tc_synth then
        (module (TraceCompleteFTASynthesizer.Create(val builder))
          : Synthesizer.S)
      else
        (module (FTASynthesizer.Create(val builder))
          : Synthesizer.S)
    in
    let module Synthesizer = (val synth) in
    let e = Synthesizer.synth ~problem in*)
  let e =
    begin match problem.spec with
      | IOEs ioes ->
        let context = Problem.extract_context problem in
        let (tin,tout) = problem.synth_type in
        synthesize_satisfying_ioes
          ~problem
          ~context
          ~tin
          ~tout
          ~ioes
          ~use_myth
          ~use_smyth
          ~use_simple
          ~use_l2
          ~tc_synth
      | Post post ->
        let context = Problem.extract_context problem in
        let (tin,tout) = problem.synth_type in
        synthesize_satisfying_postcondition
          ~problem
          ~context
          ~tin
          ~tout
          ~post
          ~use_myth
          ~use_smyth
          ~use_simple
          ~use_l2
          ~tc_synth
      | Equiv equiv ->
        let context = Problem.extract_context problem in
        let (tin,tout) = problem.synth_type in
        synthesize_satisfying_verified_equiv
          ~problem
          ~context
          ~tin
          ~tout
          ~equiv
          ~use_myth
          ~use_simple
          ~use_smyth
          ~use_l2
          ~tc_synth
    end
  in
  if no_experiments then
    begin
      print_endline (Expr.show e);
      print_endline ";";
      print_endline (Float.to_string (Consts.total Consts.isect_times));
      print_endline ";";
      print_endline (Float.to_string (Consts.max Consts.isect_times));
      print_endline ";";
      print_endline (Float.to_string (Consts.total Consts.minify_times));
      print_endline ";";
      print_endline (Float.to_string (Consts.max Consts.minify_times));
      print_endline ";";
      print_endline (Float.to_string (Consts.total Consts.min_elt_times));
      print_endline ";";
      print_endline (Float.to_string (Consts.max Consts.min_elt_times));
      print_endline ";";
      print_endline (Float.to_string (Consts.total Consts.initial_creation_times));
      print_endline ";";
      print_endline (Float.to_string (Consts.max Consts.initial_creation_times));
      print_endline ";";
      print_endline (Float.to_string (Consts.total Consts.accepts_term_times));
      print_endline ";";
      print_endline (Float.to_string (Consts.max Consts.accepts_term_times));
      print_endline ";";
      print_endline (Int.to_string (!Consts.loop_count));
      print_endline ";";
      print_endline (Float.to_string (Consts.total Consts.full_synth_times));
      print_endline ";";
      print_endline (Float.to_string (Consts.max Consts.full_synth_times));
    end
  else
    begin
      print_endline (Expr.show e);
      if print_times then
        begin
          print_endline ("Total Intersection Time: " ^ (Float.to_string (Consts.total Consts.isect_times)));
          print_endline ("Max Intersection Time: " ^ (Float.to_string (Consts.max Consts.isect_times)));
          print_endline ("Total Minify Time: " ^ (Float.to_string (Consts.total Consts.minify_times)));
          print_endline ("Max Minify Time: " ^ (Float.to_string (Consts.max Consts.minify_times)));
          print_endline ("Total Min-elt Time: " ^ (Float.to_string (Consts.total Consts.min_elt_times)));
          print_endline ("Max Min-elt Time: " ^ (Float.to_string (Consts.max Consts.min_elt_times)));
          print_endline ("Total Initial Creation Time: " ^ (Float.to_string (Consts.total Consts.initial_creation_times)));
          print_endline ("Max Initial Creation Time: " ^ (Float.to_string (Consts.max Consts.initial_creation_times)));
          print_endline ("Total Accepts Term Time: " ^ (Float.to_string (Consts.total Consts.accepts_term_times)));
          print_endline ("Max Accepts Term Time: " ^ (Float.to_string (Consts.max Consts.accepts_term_times)));
          print_endline ("Total Copy Time: " ^ (Float.to_string (Consts.total Consts.copy_times)));
          print_endline ("Max Copy Time: " ^ (Float.to_string (Consts.max Consts.copy_times)));
        end
    end

let handle_inputs
    ~(fname:string)
    ~(use_myth:bool)
    ~(use_smyth:bool)
    ~(use_simple:bool)
    ~(check_equiv1:string option)
    ~(check_equiv2:string option)
    ~(use_l2:bool)
    ~(log:bool)
    ~(no_experiments:bool)
    ~(print_times:bool)
    ~(tc_synth:bool)
    ~(use_random:bool)
  : unit =
  Consts.use_random := use_random;
  begin match (check_equiv1,check_equiv2) with
    | (Some ce1,Some ce2) ->
      check_equivalence
        ~fname
        ~ce1
        ~ce2
    | (Some _, None) | (None, Some _) -> failwith "need both check equivs given"
    | _ ->
      synthesize_solution
        ~fname
        ~use_myth
        ~use_smyth
        ~use_simple
        ~use_l2
        ~log
        ~no_experiments
        ~print_times
        ~tc_synth
  end

let benchmarks = [
  "list_compress";
  "list_concat";
  "list_hd";
  "list_last";
  "list_length";
  "list_rev_snoc";
  "list_snoc";
  "list_stutter";
  "list_tl";
  "list_append";
  "list_even_parity";
  "list_inc";
  "list_map";
  "list_nth";
  "list_filter";
  "list_sum";
  "list_drop";
  "list_take";
  "list_pairwise_swap";
  "list_sort_sorted_insert";
  "list_sorted_insert";
  "list_fold";
  "list_rev_tailcall";
  "list_rev_append";
  "list_rev_fold";
]

open MyStdLib.Command.Let_syntax
let param =
  MyStdLib.Command.basic ~summary:"..."
    [%map_open
      let input_spec  = anon ("input_spec" %: string)
      and use_myth   = flag "use-myth" no_arg ~doc:"Solve using the myth synthesis engine"
      and use_smyth   = flag "use-smyth" no_arg ~doc:"Solve using the smyth synthesis engine"
      and use_simple   = flag "use-simple" no_arg ~doc:"Solve using the simple synthesis engine"
      and log   = flag "log" no_arg ~doc:"log process"
      and use_l2   = flag "use-l2" no_arg ~doc:"Solve using the l2 synthesis engine"
      and print_times   = flag "print-times" no_arg ~doc:"print the times to run various components"
      and no_experiments   = flag "no-experiments" no_arg ~doc:"print the times to run various components"
      and check_equiv1   = flag "check-equiv1" (optional string) ~doc:"check equivalence of two synthesized solutions"
      and check_equiv2   = flag "check-equiv2" (optional string) ~doc:"check equivalence of two synthesized solutions"
      and tc_synth   = flag "tc-synth" no_arg ~doc:"use the FTA synthesizer with trace complete examples"
      and use_random   = flag "use-random" no_arg ~doc:"print timbuk to vata mapping"
      and json_fuzz = flag "json-fuzz" no_arg ~doc:"use json as the source for sets IO examples to perform stress testing"
      and exs_opt = flag "-exs" (optional string) ~doc:"take examples in a different format"
      and trial_count = flag "trial-count" (optional_with_default 50 int) ~doc:"the number of trials for each k"
      and max_k = flag "max-k" (optional_with_default 5 int) ~doc:"the maximum size of example sets"
      (*and no_grammar_output   = flag "no-grammar-output" no_arg ~doc:"do not output the discovered grammar"
        and log_progress   = flag "log-progress" no_arg ~doc:"output the progress log"
        and print_runtime_specs  = flag "print-runtime-specs" no_arg ~doc:"output the runtime specs"
        and no_experiments  = flag "no-experiments" no_arg ~doc:"output experient info"
        and positive_examples  = flag "pos" (listed string) ~doc:"path positive example path"
        and negative_examples  = flag "neg" (listed string) ~doc:"path negative example path"
        and pos_ndfo  = flag "pos-ndf" (optional string) ~doc:"path newline delimited positive example path"
        and neg_ndfo  = flag "neg-ndf" (optional string) ~doc:"path newline delimited negative example path"*)
      in
      let no_experiments = not no_experiments in
      fun () ->
        let checker (ctx: Context.t) ref_e inp outp =
          let inpe = Lang.Value.to_exp inp in
          let apped = Expr.mk_app ref_e inpe in
          try 
          let evaled = Eval.evaluate_with_holes ~eval_context:ctx.evals apped in
          Value.equal evaled outp
          with
          | _ -> false
        in
        let is_consistent ctx ref_e ios =
          List.for_all ios ~f:(fun (i, o) -> checker ctx ref_e i o)
        in
          let synth =
            let synth = get_predicate_synthesizer ~use_simple in
            (module Synthesizers.VerifiedPredicate.ToIOSynth(Synthesizers.VerifiedPredicate.Make(val synth)) : Synthesizers.IOSynth.S)
          in
          let module S = (val synth) in
        if Option.is_some exs_opt then
              (let p_unprocessed =
                ParserContainer.unprocessed_problem
                  ((MyStdLib.SimpleFile.read_from_file ~fname:("benchmarks/io/"^input_spec^".mls")))
              in
              let p_unprocessed = Problem.update_all_import_bases p_unprocessed input_spec in
              let p_unprocessed = import_imports p_unprocessed in
              let problem = Problem.process p_unprocessed in
              let context = Problem.extract_context problem in
              let (tin,tout) = problem.synth_type in
              let ((exs : (Value.t * Value.t) list ),
                   (assertion : (Value.t * Value.t) list) ) =
                match
                  List.Assoc.find
                    (References.all (Fuzz.parse_io_proj ~fname:input_spec ~exs_str:(Option.value_exn exs_opt) problem))
                    input_spec ~equal:String.equal
                with
                | Some benchmark_thunk -> benchmark_thunk ()
                | None                 ->
                  prerr_endline ("Unknown built-in function '" ^ input_spec ^ "'.");
                  exit 1
              in
              try
                let exp, time_spent =
                  Time.timed (fun _ -> snd (S.synth (S.init ~problem ~context ~tin ~tout) exs))
                in
                Printf.printf "%s\n\n%.4f %b\n" (Expr.show exp)time_spent
                  (is_consistent context exp assertion);
              with
              | Stack_overflow -> Printf.printf "STACKOVERFLOW\n"
              |  Time.Timeout ->
                Printf.printf "TIMEOUT\n";

)        else 
          handle_inputs
            ~fname:input_spec
            ~use_myth
            ~use_smyth
            ~use_simple
            ~use_l2
            ~log
            ~print_times
            ~no_experiments
            ~tc_synth
            ~check_equiv1
            ~check_equiv2
            ~use_random
    ]

let () =
  Core.Command.run
    param
