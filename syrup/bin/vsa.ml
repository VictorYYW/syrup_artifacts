open Core
open Lib
open Syn

let () =
  let argv = Sys.get_argv () in
  let mode_str = argv.(1) in
  let builtin = argv.(2) in
  let exs_str = argv.(3) in
  let mode =
    match mode_str with
    | "syrup" -> `SyRup
    | "height" -> `Height
    | "naive" -> `SyRup
    | _ ->
        prerr_endline ("Unknown mode '" ^ mode_str ^ "'.");
        exit 1
  in
  let ( (gamma, sigma),
        ty,
        (exs : (Exp.t * Exp.t) list),
        (assertion : (Exp.t * Exp.t) list) ) =
    match
      List.Assoc.find
        (References.all (Fuzz.parse_io_proj ~exs_str))
        builtin ~equal:String.equal
    with
    | Some benchmark_thunk -> benchmark_thunk ()
    | None ->
        prerr_endline ("Unknown built-in function '" ^ builtin ^ "'.");
        exit 1
  in
  try
    let exp_opt, time_spent =
      Time.timed (fun _ ->
          main Out_channel.stdout ~gamma ~sigma ~mode ty exs
            ~semantic:(not @@ String.equal mode_str "naive"))
    in
    (* assert ( *)
    (*   Examples.is_consistent exp *)
    (*     (List.map exs ~f:(fun (i, o) -> (sigma, Some i, o)))); *)
    Printf.printf "%.4f %b\n" time_spent
      (Option.value_map exp_opt ~default:false ~f:(fun exp ->
           Examples.is_consistent exp
             (List.map assertion ~f:(fun (i, o) -> (sigma, Some i, o)))))
  with Time.Timeout -> Printf.printf "TIMEOUT\n"
