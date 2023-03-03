include Ctx
include Lang

type t = exp Ctx.t [@@deriving sexp, compare]

let get_fpar sigma = Ctx.lookup_exn sigma fun_name
