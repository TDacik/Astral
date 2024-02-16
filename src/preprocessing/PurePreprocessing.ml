(* Preprocessing of pure terms in SSL formulae.
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2022 *)

open SMT

let rec purify = function
  | SSL.Var (name, sort) -> SMT.Variable.mk name sort
  | SSL.Pure t -> t
  | SSL.Eq xs -> Boolean.mk_eq_list (List.map purify xs)
  | SSL.Distinct xs -> Boolean.mk_distinct_list (List.map purify xs)
  | SSL.And (psi1, psi2) -> Boolean.mk_and [purify psi1; purify psi2]
  | SSL.Or (psi1, psi2) -> Boolean.mk_or [purify psi1; purify psi2]
  | SSL.GuardedNeg (psi1, psi2) -> Boolean.mk_and [purify psi1; Boolean.mk_not @@ purify psi2]
  | SSL.Not psi -> Boolean.mk_not @@ purify psi (* TODO: not sound, but needed for Broom *)
  | other -> Utils.internal_error @@ Format.asprintf "Not a pure formula: %a" SSL.pp other

let purify phi = SSL.mk_pure @@ purify phi

let apply =
  SSL.map
    (fun phi -> match phi with
      | SSL.Eq xs when List.for_all SSL.is_pure_smt xs -> purify phi
      | SSL.Distinct xs -> if List.for_all SSL.is_pure_smt xs then purify phi else phi
      | SSL.And (psi1, psi2) when SSL.is_pure_smt psi1 && SSL.is_pure_smt psi2 -> purify phi
      | SSL.Or (psi1, psi2) when SSL.is_pure_smt psi1 && SSL.is_pure_smt psi2 -> purify phi
      | SSL.Not psi when SSL.is_pure_smt psi -> purify phi
      | SSL.GuardedNeg (psi1, psi2) when SSL.is_pure_smt psi1 && SSL.is_pure_smt psi2 ->
        purify phi
      | phi -> phi
    )
