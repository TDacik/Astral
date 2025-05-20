(* Inlining of non-recursive inductive definitions.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2024 *)

let inline phi = SL.map_view (function
  | Predicate (name, xs, structs) -> match SID.inline name xs with
    | Some res -> `Modify res
    | None -> `Skip
) phi

let inline phi =
  let phi' = inline phi in
  if SL.equal phi phi' then phi
  else inline phi'

let inline_ctx ctx =
  let open Context in
  {ctx with phi = inline ctx.phi}
