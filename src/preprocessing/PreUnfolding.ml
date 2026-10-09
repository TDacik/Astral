(* Pass which unfolds user-supplied minimal depths of inductive predicates.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2026 *)

open Context

let unfold sid = SL.map_view (function
  | Predicate (name, xs, n, _) when n > 0 -> `Modify (SID.unfold_non_empty sid name xs n)
  | _ -> `Skip
)

let apply phi =
  let sid = GlobalSID.get () in
  unfold sid phi
  (* TODO: PNF *)

let apply_ctx ctx = {ctx with phi = apply ctx.phi}
