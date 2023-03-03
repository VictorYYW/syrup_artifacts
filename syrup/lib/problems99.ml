(* return element(s) from a list *)

(* when the output is of `option` type, and the result do not needs to be
   accumulated between recursions, the solution normally would need three cases
   corresponding to `None`, `Some`, and recursive function call respectively *)

(** 1. return the last element of a list *)
let rec last xs =
  match xs with
  | []     -> None
  | [ x ]  -> Some x
  | _ :: t -> last t

(** 2. find the last but one elements of a list *)
let rec last_two xs =
  match xs with
  | [] | [ _ ] -> None
  | [ x; y ]   -> Some (x, y)
  | _ :: t     -> last_two t

(** 3. find the K'th element of a list *)
let rec at k xs =
  match xs with
  | []     -> None
  | h :: t ->
      if k = 0 then
        Some h
      else
        at (k - 1) t

(* neither add nor drop elements as building list *)

(** 9. pack consecutive duplicates of list elements into sublists *)
let pack xs =
  let rec aux current acc xs =
    match xs with
    | []            -> []
    | [ x ]         -> (x :: current) :: acc
    | h1 :: h2 :: t ->
        if h1 = h2 then
          aux (h1 :: current) acc (h2 :: t)
        else
          aux [] ((h1 :: current) :: acc) t
  in
  List.rev (aux [] [] xs)

(** 10. run-length encoding of a list *)
let encode xs =
  let rec aux count acc xs =
    match xs with
    | []            -> []
    | [ x ]         -> (count + 1, x) :: acc
    | h1 :: h2 :: t ->
        if h1 = h2 then
          aux (count + 1) acc (h2 :: t)
        else
          aux 0 ((count + 1, h1) :: acc) t
  in
  List.rev (aux 0 [] xs)

(** 17. Split a list into two parts; the length of the first part is given *)
let rec split n xs =
  match xs with
  | []     -> ([], [])
  | h :: t ->
      if n = 0 then
        ([], xs)
      else
        let l, r = split (n - 1) t in
        (* TODO: why having let binding under "else" branch instead of above
           "if-then-else" *)
        (h :: l, r)

(* drop elements as building list, an superset of those problems using `filter`.
   We can identify -- an "if-then-else" structure featuring dropping and
   including the current element in respective branch -- for this problem
   category *)

(** 8. eliminate consecutive duplicates of list elements *)
let rec compress xs =
  match xs with
  | [] | [ _ ]    -> xs
  | h1 :: h2 :: t ->
      if h1 = h2 then
        compress (h2 :: t)
      else
        h1 :: compress (h2 :: t)

(* the one would be synthesized *)
let rec compress' xs =
  match xs with
  | []       -> []
  | h1 :: t1 -> (
      match t1 with
      | []       -> h1 :: compress' t1
      | h2 :: t2 ->
          if h1 = h2 then
            compress' t1
          else
            h1 :: compress' t1)

(* when given [2;2;4;1;1] -> [2;4;1], the synthesizer's output is *)
let rec compress1 xs =
  match xs with
  | []       -> [ 1; 2; 3; 4; 5; 6 ] (* T *)
  | hd :: tl ->
      if
        match tl with
        | []         -> false
        | hd' :: tl' -> hd == hd'
      then
        compress1 tl
      else
        hd
        ::
        (if
         match tl with
         | []         -> true
         | hd' :: tl' -> hd == hd'
        then
          tl
        else
          compress1 tl)

(* which is equivalent to *)
let rec compress1' xs =
  match xs with
  | []       -> [ 1; 2; 3; 4; 5; 6 ] (* T *)
  | hd :: tl -> (
      match tl with
      | []         -> tl
      | hd' :: tl' ->
          if hd == hd' then
            compress1' tl
          else
            hd :: compress1' tl)

(** 16. drop every N'th element from a list*)
let drop xs n =
  let rec aux xs i =
    match xs with
    | []     -> []
    | h :: t ->
        if i = n then
          aux t 1
        else
          h :: aux t (i + 1)
  in
  aux xs 1

(** 18. Extract a slice from a list *)
let rec slice xs s t =
  match xs with
  | []       -> []
  | hd :: tl ->
      if s > 0 then
        slice tl (s - 1) (t - 1)
      else if t > 0 then
        hd :: slice tl 0 (t - 1)
      else
        []

(** 20. remove the k'th element from a list *)
let rec remove_at n xs =
  match xs with
  | []     -> []
  | h :: t ->
      if n = 0 then
        t
      else
        h :: remove_at (n - 1) t

(* add element(s) as building list *)

(** 21. Insert an element at a given position into a list *)
let rec insert_at x n = function
  | []          -> [ x ]
  | h :: t as l ->
      if n = 0 then
        x :: l
      else
        h :: insert_at x (n - 1) t

(* expand elements as building list *)

(** 12. decode a run-length encoded list *)
let rec decode xs =
  match xs with
  | []          -> []
  | (0, x) :: t -> decode t
  | (n, x) :: t -> x :: decode ((n - 1, x) :: t)

(** 14. duplicate the elements of a list *)
let rec duplicate xs =
  match xs with
  | []     -> []
  | h :: t -> h :: h :: duplicate t

(** 15. Replicate the elements of a list a given number of times *)
let replicate xs n =
  let rec aux xs i =
    match xs with
    | []     -> []
    | h :: t ->
        if i = n then
          aux t 0
        else
          h :: aux xs (i + 1)
  in
  aux xs 0

(* obtain a conclusion about a list *)

(** 4. find the number of elements of a list *)
let rec length xs =
  match xs with
  | []     -> 0
  | _ :: t -> 1 + length t

(** 6. find out whether a list is a palindrome *)
let is_palindrome xs = List.rev xs = xs

(* list reordering *)

(** 5. reverse a list *)
let rec reverse xs =
  match xs with
  | []     -> []
  | h :: t -> reverse t @ [ h ]

(** 19. Rotate a list N places to the left, there is a much elegant solution
    using `split`; if it is "rotate to the right", then `split` becomes almost a
    must *)
let rec rotate xs n =
  match xs with
  | []     -> []
  | h :: t -> rotate (t @ [ h ]) (n - 1)

(* ... *)

(** 7. Flatten a nested list structure. *)
type 'a node = One of 'a | Many of 'a node list

let rec flatten xs =
  match xs with
  | []           -> []
  | One x :: t   -> x :: flatten t
  | Many ys :: t -> flatten ys @ flatten t

(** 22. Create a list containing all integers within a given range *)
let rec range s t =
  if s < t then
    s :: range (s + 1) t
  else
    []

(** 26. Generate the combinations of K distinct objects chosen from the N
    elements of a list *)
let rec extract k xs =
  if k <= 0 then
    [ [] ]
  else
    match xs with
    | []     -> []
    | h :: t -> List.map (fun l -> h :: l) (extract (k - 1) t) @ extract k t

(* when there is an extra integer argument, it could have different meanings --
   a value, like elements in the input list, an index, or a counter. When it
   comes to integers, synthesizer is likely to provide overfitting solutions.
   Certain relational perturbation properties may help distinguish between its
   different uses, and thus help with the synthesis.

   We can categorize list manipulation benchmarks on our hands using this metric
   as follows.

   - value: add(l2), member(l2)

   - index: at, split, slice, remove_at, insert_at, range

   - counter: drop, decode, replicate, rotate, extract

   Then we move on to see how differentiating them may help with the synthesis.

   - First, integers representing values won't be involved in arithmetic
   operations or comparisons with only constants (should involve elements from
   the input data structure).

   - Secondly, we notice that when the input of the synthesis problem consists
   of a data structure and extra argument(s) representing index, i.e. excluding
   "range", there is clearly a structural pattern in the solutions for those
   synthesis problems -- other than the empty list base case, the `cons` list
   case needs to branch on whether the index `i` is 0, and recursive function
   call(s) under branching would take `i-1` and the tail list `t` as arguments.

   - Finally, the solution for synthesis problem involving argument(s)
   representing counters seems chaotic. However, when a counter argument would
   be used multiple times in a program, the solution often uses auxiliary
   function to reset counter by introducing a local counter, e.g., "drop" and
   "replicate".

   Now we explore which and how properties may differentiate the three use cases
   of integer arguments.

   - output is also list(s): if left extension is not preserved, and right
   extension is preserved, it is very likely that the extra integer argument
   represents an index.

   - otherwise: if left extension invariance does not apply, and right extension
   does apply, it is very likely that the extra integer argument represents an
   index.

   Knowing the effect of incrementing integer input to the length of the output
   would expose the semantics of that integer in the program as well, but it
   remains unclear how that could be leveraged. *)
