(* Simple simplification by applying smart constructors.
 *
 * TODO: add predicate simplication rules.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2024 *)

let simplify ?(dont_care=[]) phi = SL.map_view (function
  | SL.And psis -> SL.mk_and psis
  | SL.Or psis -> SL.mk_or psis
  | SL.Star psis -> SL.mk_star psis
  | SL.Eq xs -> SL.mk_eq xs
  | SL.Distinct xs -> SL.mk_distinct xs
  | SL.Not (psi) -> SL.mk_not psi
  | SL.GuardedNeg (psi1, psi2) -> SL.mk_gneg psi1 psi2
  | SL.Exists (xs, psi) -> SL.mk_exists xs psi
) phi
