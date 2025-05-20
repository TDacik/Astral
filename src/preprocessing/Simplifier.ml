(* Simple simplification by applying smart constructors.
 *
 * TODO: add predicate simplication rules.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2024 *)

let simplify ?(dont_care=[]) phi = SL.map_view (function
  | SL.And psis -> `Modify (SL.mk_and psis)
  | SL.Or psis -> `Modify (SL.mk_or psis)
  | SL.Star psis -> `Modify (SL.mk_star psis)
  | SL.Eq xs -> `Modify (SL.mk_eq xs)
  | SL.Distinct xs -> `Modify (SL.mk_distinct xs)
  | SL.Not (psi) -> `Modify (SL.mk_not psi)
  | SL.GuardedNeg (psi1, psi2) -> `Modify (SL.mk_gneg psi1 psi2)
  | SL.Ite (c, t, e) -> `Modify (SL.mk_ite c t e)
  | SL.Exists (xs, psi) -> `Modify (SL.mk_exists xs psi)
  | _ -> `Skip
) phi

let simplify_ctx ctx =
  let open Context in
  {ctx with phi = simplify ctx.phi}
