open SL
open Context

let apply_ctx ctx = match SL.view ctx.phi with
  | GuardedNeg (lhs, rhs) ->
    if lhs === rhs then {ctx with phi = SL.ff}
    else ctx
  | _ -> ctx
