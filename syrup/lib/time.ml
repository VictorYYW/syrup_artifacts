open Core
include Time

let timeout = 600

exception Timeout

let delayed_fun f =
  let (_ : unit) =
    Stdlib.Sys.set_signal Stdlib.Sys.sigalrm
      (Stdlib.Sys.Signal_handle (fun _ -> raise Timeout))
  in
  ignore (Core_unix.alarm timeout : int);
  try
    let r = f () in
    ignore (Core_unix.alarm 0 : int);
    r
  with e ->
    ignore (Core_unix.alarm 0 : int);
    raise e

let timed f : 'a * float =
  let start_time = now () in
  let result = delayed_fun f in
  let end_time = now () in
  let time = Span.to_sec (Time.diff end_time start_time) in
  (result, time)
