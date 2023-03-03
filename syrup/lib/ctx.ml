open Core
module SMap = Map.Make (String)

type 'a t = 'a SMap.t ref

let sexp_of_t f ctx = SMap.sexp_of_t f !ctx
let t_of_sexp f sexp = ref (SMap.t_of_sexp f sexp)
let compare f ctx1 ctx2 = SMap.compare f !ctx1 !ctx2

exception UnboundError of string

let empty () : 'a t = ref SMap.empty
let is_empty ctx = SMap.is_empty !ctx
let singleton key data = ref (SMap.singleton key data)

(** Look up an id in a context. *)
let lookup ctx id = SMap.find !ctx id

let lookup_exn ctx id =
  match lookup ctx id with
  | Some v -> v
  | None   -> raise (UnboundError id)

(** Bind a type or value to an id, returning a new context. *)
let bind ctx id data = ref (SMap.set !ctx ~key:id ~data)

let bind_alist ctx alist =
  List.fold alist ~init:ctx ~f:(fun ctx' (id, data) -> bind ctx' id data)

(** Remove a binding from a context, returning a new context. *)
let unbind ctx id = ref (SMap.remove !ctx id)

(** Bind a type or value to an id, updating the context in place. *)
let update ctx id data = ctx := SMap.set !ctx ~key:id ~data

(** Remove a binding from a context, updating the context in place. *)
let remove ctx id = ctx := SMap.remove !ctx id

(** prohibite duplicate binding from happening, e.g., duplicate pattern variable
    being used *)
let prohibite_duplicate ~key:bind = function
  | `Left v | `Right v -> Some v
  | `Both _            -> failwith "confliction found during context merging"

(** shadow existing binding when the same identifies is used, e.g., try binding
    a variable that is already in the context *)
let shadow_duplicate ~key:bind = function
  | `Left v | `Right v | `Both (_, v) -> Some v

let merge c1 c2 ~f = ref (SMap.merge !c1 !c2 ~f)
let join ctxs ~f = List.fold ctxs ~init:(empty ()) ~f:(merge ~f)

(** merge two optional context *)
let merge_opt c1_opt c2_opt ~f =
  match (c1_opt, c2_opt) with
  | Some c1, Some c2 -> Some (ref (SMap.merge !c1 !c2 ~f))
  | _                -> None

let join_opt ctx_opts ~f =
  List.fold ctx_opts ~init:(Some (empty ())) ~f:(merge_opt ~f)

let map ctx ~f = ref (SMap.map !ctx ~f)
let mapi ctx ~f = ref (SMap.mapi !ctx ~f)
let filter ctx ~f = ref (SMap.filter !ctx ~f)
let filter_keys ctx ~f = ref (SMap.filter_keys !ctx ~f)
let filter_mapi ctx ~f = ref (SMap.filter_mapi !ctx ~f)
let equal cmp c1 c2 = SMap.equal cmp !c1 !c2
let keys ctx = SMap.keys !ctx
let data ctx = SMap.data !ctx
let of_alist alist = ref (SMap.of_alist alist)
let of_alist_exn alist = ref (SMap.of_alist_exn alist)
let to_alist ctx = SMap.to_alist !ctx

let to_string ctx str =
  to_alist ctx
  |> List.map ~f:(fun (key, value) -> key ^ ": " ^ str value)
  |> String.concat ~sep:", "
  |> fun s -> "{ " ^ s ^ " }"

let fold ctx ~init ~f = SMap.fold !ctx ~init ~f
