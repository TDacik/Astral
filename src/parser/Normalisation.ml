(* Normalisation pass of parser.
 *
 * This preprocessing pass is part of parser because it is required to ensure invariants that are
 * needed when working with SSL formulae and hard to catch by parser:
 *  - Equality is always used only over location variables. When used over boolean terms as 'iff'
 *    it is replaced by it.
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2023 *)

let all_sort xs sort = List.for_all (fun x -> Sort.equal sort @@ SSL.get_sort x) xs
let all_bool xs = all_sort xs Sort.Bool

let rec apply = function
  | SSL.Var x -> SSL.Var x
  | SSL.Eq xs ->
      (* Transform equality of boolean terms to iff *)
      if all_bool xs then SSL.mk_iff (List.map apply xs)
      else SSL.Eq xs
  | SSL.Distinct xs -> SSL.Distinct xs
  | SSL.And (psi1, psi2) -> SSL.And (apply psi1, apply psi2)
  | SSL.Or (psi1, psi2) -> SSL.Or (apply psi1, apply psi2)
  | SSL.Not psi -> SSL.Not (apply psi)
  | SSL.GuardedNeg (psi1, psi2) -> SSL.GuardedNeg (apply psi1, apply psi2)
  | SSL.Star psis -> SSL.mk_star @@ List.map apply psis
  | SSL.Septraction (psi1, psi2) -> SSL.mk_septraction (apply psi1) (apply psi2)
  | SSL.Forall (xs, psi) -> SSL.Forall (xs, apply psi)
  | SSL.Exists (xs, psi) -> SSL.Exists (xs, apply psi)

  | other -> other
