open Core

type bindspec = Null | Rec | Arg of int | Dec of int | Matched of int
[@@deriving sexp, compare, hash]

include Ctx

type t = (Typ.t * bindspec) Ctx.t [@@deriving sexp, compare]

let is_rec ctx id =
  match lookup_exn ctx id with
  | _ty, Rec -> true
  | _        -> false

let is_arg ctx id =
  match lookup_exn ctx id with
  | _ty, Arg _ -> true
  | _          -> false

let is_dec ctx id =
  match lookup_exn ctx id with
  | _ty, Dec _ -> true
  | _          -> false

let filter_spec ctx spec =
  filter ctx ~f:(fun (_typ', spec') -> compare_bindspec spec spec' = 0)

let filter_typ_spec ctx typ spec =
  filter ctx ~f:(fun (typ', spec') ->
      Typ.compare typ typ' = 0 && compare_bindspec spec spec' = 0)
