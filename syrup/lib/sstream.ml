open Core
include Stream

type 'a matrix = 'a list t

let concat s1 s2 =
  from (fun _ ->
      match peek s1 with
      | Some _ -> Some (next s1)
      | None -> (
          match peek s2 with
          | Some _ -> Some (next s2)
          | None -> None))

let map s ~f = from (fun _ -> try Some (f (next s)) with Failure -> None)
let map_matrix m ~f = map m ~f:(List.map ~f)
let filter_matrix m ~f = map m ~f:(List.filter ~f)
let filter_map_matrix m ~f = map m ~f:(List.filter_map ~f)
let singleton v = of_list [ v ]
let repeat v = from (fun _ -> Some v)
let repeat_n n v = List.range 0 n |> List.map ~f:(fun _ -> v) |> of_list

let trans : 'a t list -> 'a list t = function
  | [] -> repeat []
  | ss -> from (fun _ -> Some (List.map ss ~f:next))

(** when the row index and column index is two cost metric
    respectively, the sum of cost can be annotated as following,
    `diag` returns stream of list of expressions, where expression in
    the same list has the same cost
    3 | 4 5 6
    2 | 3 4 5
    1 | 2 3 4
   ---|-------
      | 1 2 3 *)
let diag (s : 'a t t) : 'a list t =
  from (fun i -> Some (List.map (npeek (i + 1) s) ~f:next))

(** 3 | 3 3 3
    2 | 2 2 3
    1 | 1 2 3
   ---|-------
      | 1 2 3 *)
let square (s : 'a t t) : 'a list t =
  from (fun i ->
      let sq = List.rev @@ npeek (i + 1) s in
      match sq with
      | [] -> raise Failure
      | hd :: tl ->
          List.append (List.init i ~f:(fun _ -> next hd)) (List.map tl ~f:next)
          |> Some)

(* calculate correct cost matrix from the nested cost matrix *)
let join ~mode (x : 'a matrix matrix) : 'a matrix =
  x |> map ~f:trans
  |> (match mode with
     | `SyRup -> diag
     | `Height -> square)
  |> map ~f:(fun y -> y |> List.concat |> List.concat)

let compose ~mode (f : 'a -> 'b matrix) (g : 'b -> 'c matrix) (x : 'a) :
    'c matrix =
  x |> f |> map ~f:(List.map ~f:g) |> join ~mode

let group s ~break =
  from (fun _ ->
      let rec collect () =
        match npeek 2 s with
        | [] -> None
        | [ _ ] -> Some [ next s ]
        | [ x; y ] ->
            if break x y then
              Some [ next s ]
            else
              collect ()
        | _ -> failwith "Stream.npeek returned a larger list than expected."
      in
      collect ())

(* stream of the concatenation of all ith list in the input matrices, where i is
   the stream count *)
let merge (ss : 'a matrix list) : 'a matrix =
  from (fun _ ->
      Some
        (ss
        |> List.filter_map ~f:(fun s ->
               try Some (next s) with Failure -> None)
        |> List.concat))

let rec drop_while s ~f =
  match peek s with
  | Some x ->
      if f x then (
        junk s;
        drop_while s ~f
      ) else
        ()
  | None -> ()

let flatten (m : 'a matrix) : 'a t =
  let current = ref [] in
  from (fun _ ->
      match !current with
      | x :: xs ->
          current := xs;
          Some x
      | [] -> (
          drop_while m ~f:List.is_empty;
          try
            match next m with
            | [] -> failwith "Failed to drop empty rows."
            | x :: xs ->
                current := xs;
                Some x
          with Failure -> None))

let multiply (x : int) (m : 'a matrix) : 'a matrix =
  from (fun i ->
      if (i + 1) mod x = 0 then
        Some (next m)
      else
        Some [])

module Memoizer
    (Key : Map.Key) (Value : sig
      type t
    end) =
struct
  module KMap = Map.Make (Key)

  (* basically, head just stored the value that we have already retrieved from
     stream*)
  (* index simply refers to the where the last value is stored, and is the
     largest valid index in the table *)
  type memo_stream = {
    index : int ref;
    head : Value.t list Int.Table.t;
    stream : Value.t matrix;
  }

  type t = memo_stream KMap.t ref

  let empty () = ref KMap.empty

  (* Get access to a stream of results for 'typ'. *)
  let get memo typ stream : Value.t matrix =
    (* find value -- matrix along with its metadata -- by key (typ), if not
       exist, then map the key to a default value*)
    let mstream =
      match KMap.find !memo typ with
      | Some s -> s
      | None ->
          let s =
            { index = ref 0; head = Int.Table.create (); stream = stream () }
          in
          memo := KMap.set !memo ~key:typ ~data:s;
          s
    in
    from (fun i ->
        let sc = i + 1 in
        if sc <= !(mstream.index) then
          Some (Int.Table.find_exn mstream.head sc)
        else (
          List.range ~stop:`inclusive (!(mstream.index) + 1) sc
          |> List.iter ~f:(fun j ->
                 try
                   Int.Table.add_exn mstream.head ~key:j
                     ~data:(next mstream.stream);
                   incr mstream.index
                 with Failure -> ());
          if sc = !(mstream.index) then
            Some (Int.Table.find_exn mstream.head sc)
          else
            None
        ))
end
