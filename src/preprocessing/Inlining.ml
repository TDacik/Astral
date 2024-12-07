(* Inlining of non-recursive inductive definitions.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2024 *)

let inline phi = SL.map_view (function
  | Predicate (name, xs, structs) -> match SID.inline name xs with
    | None -> SL.mk_predicate name xs ~structs
    | Some res -> res
) phi

let inline phi =
  let phi' = inline phi in
  if SL.equal phi phi' then phi
  else inline phi'
