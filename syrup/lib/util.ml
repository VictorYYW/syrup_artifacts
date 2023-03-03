let uncurry f (x, y) = f x y

module List = struct
  include Core.List

  let remove_nth i xs =
    match split_n xs i with
    | pre, _ :: arg -> pre @ arg
    | _, []         -> failwith "remove_nth failed"

  let replace_nth xs i x =
    match split_n xs i with
    | pre, _ :: arg -> pre @ (x :: arg)
    | _, []         -> failwith "replace_nth failed"

  let update_nth xs i f =
    match split_n xs i with
    | pre, x :: arg -> pre @ (f x :: arg)
    | _, []         -> failwith "update_nth failed"

  (** return all the elements except the first one satisfying the predicate,
      return none if there is no such element *)
  let rec remove_first (xs : 'a t) ~(f : 'a -> bool) : 'a t option =
    match xs with
    | []       -> None
    | hd :: tl ->
        if f hd then
          Some tl
        else
          remove_first tl ~f |> Option.map (fun tl' -> hd :: tl')

  (** return if the second list can be obtained by simply permuting the first
      list *)
  let equal_perm (xs : 'a t) (ys : 'a t) ~(equal : 'a -> 'a -> bool) : bool =
    let rec equal_perm' xs ys equal =
      match xs with
      | []       -> is_empty ys
      | x :: xs' -> (
          match remove_first ys ~f:(equal x) with
          | Some ys' -> equal_perm' xs' ys' equal
          | None     -> false)
    in
    List.length xs = List.length ys && equal_perm' xs ys equal

  let rec cprod (l : 'a t t) =
    (* We need to do the cross product of our current list and all the others
     * so we define a helper function for that *)
    let rec aux ~acc l1 l2 =
      match (l1, l2) with
      | [], _ | _, []      -> acc
      | h1 :: t1, h2 :: t2 ->
          let acc = (h1 :: h2) :: acc in
          let acc = aux ~acc t1 l2 in
          aux ~acc [ h1 ] t2
      (* now we can do the actual computation *)
    in
    match l with
    | []       -> []
    | [ l1 ]   -> List.map (fun x -> [ x ]) l1
    | l1 :: tl ->
        let tail_product = cprod tl in
        aux ~acc:[] l1 tail_product

  let perm (xs : 'a t) (p : int t) =
    zip_exn p xs
    |> sort ~compare:(fun (i, _) (j, _) -> Int.compare i j)
    |> map ~f:snd

  let rand_perm (xs : 'a t) =
    xs
    |> map ~f:(fun x -> (Random.bits (), x))
    |> sort ~compare:(fun (i, _) (j, _) -> Int.compare i j)
    |> map ~f:snd

  let sample n (xs : 'a t) = take (rand_perm xs) n

  let rec permutations l =
    let n = List.length l in
    if n = 1 then
      [ l ]
    else
      let rec sub e = function
        | []     -> failwith "index_permutations:sub"
        | h :: t ->
            if h = e then
              t
            else
              h :: sub e t
      in
      let rec aux k =
        let e = List.nth l k in
        let subperms = permutations (sub e l) in
        let t = List.map (fun a -> e :: a) subperms in
        if k < n - 1 then
          List.rev_append t (aux (k + 1))
        else
          t
      in
      aux 0

  (** n is the length of the list to be permuted *)
  let index_permutations n = permutations (range 0 n)

  let indices (xs : 'a t) = range 0 (length xs)
end

(** Module for creating fresh names and numbers. *)
module Fresh = struct
  open Core
  module CharMap = Map.Make (Char)

  let count_map : int CharMap.t ref = ref CharMap.empty

  let refresh () = count_map := CharMap.empty

  let get_int (prefix : Char.t) =
    count_map :=
      CharMap.update !count_map prefix ~f:(function
        | Some n -> n + 1
        | None   -> 0);
    CharMap.find_exn !count_map prefix

  let name prefix = Printf.sprintf "%c%d" prefix (get_int prefix)

  let names prefix num = List.range 0 num |> List.map ~f:(fun _ -> name prefix)
end
