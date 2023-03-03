open Core

(* From Elm 0.19 *)
let rec get_by_weight : float * 'a -> (float * 'a) list -> float -> 'a =
 fun (weight, value) others countdown ->
  match others with
  | []                     -> value
  | second :: other_others ->
      if Float.( <= ) countdown (Float.abs weight) then
        value
      else
        get_by_weight second other_others (countdown -. Float.abs weight)

(* From Elm 0.19 *)
let weighted : float * 'a -> (float * 'a) list -> 'a =
 fun first others ->
  let normalize (weight, _) = Float.abs weight in
  let total =
    normalize first
    +. List.fold ~f:Float.( + ) ~init:0.0 (List.map ~f:normalize others)
  in
  get_by_weight first others (Random.float total)

let rec sample_unique_helper : 'a list -> (int * (unit -> 'a)) list -> 'a list =
 fun acc info ->
  match info with
  | []                       -> acc
  | (size, gen) :: rest_info ->
      if List.length acc >= size then
        sample_unique_helper acc rest_info
      else
        let x = gen () in
        if Option.is_some (List.find ~f:(Poly.( = ) x) acc) then
          sample_unique_helper acc info
        else
          sample_unique_helper (x :: acc) info

let sample_unique : (int * (unit -> 'a)) list -> 'a list =
 fun info -> sample_unique_helper [] info |> List.rev
