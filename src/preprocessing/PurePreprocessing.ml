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
  | SSL.Not psi -> Boolean.mk_not @@ purify psi
  | other -> failwith @@ SSL.show other

let purify phi = SSL.mk_pure @@ purify phi

let rec apply phi = match phi with
  | SSL.Eq xs -> if List.for_all SSL.is_pure xs then purify phi else phi
  | SSL.Distinct xs -> if List.for_all SSL.is_pure xs then purify phi else phi
  | SSL.And (psi1, psi2) ->
      if SSL.is_pure psi1 && SSL.is_pure psi2 then purify phi
      else SSL.mk_and [apply psi1; apply psi2]
  | SSL.Or (psi1, psi2) ->
      if SSL.is_pure psi1 && SSL.is_pure psi2 then purify phi
      else SSL.mk_or [apply psi1; apply psi2]
  | SSL.Not psi ->
      if SSL.is_pure psi then purify phi
      else SSL.mk_not (apply psi)
  | SSL.GuardedNeg (psi1, psi2) ->
      if SSL.is_pure psi1 && SSL.is_pure psi2 then purify phi
      else SSL.mk_gneg (apply psi1) (apply psi2)
  | SSL.Star (psi1, psi2) -> SSL.mk_star [apply psi1; apply psi2]
  | SSL.Septraction (psi1, psi2) -> SSL.mk_septraction (apply psi1) (apply psi2)
  | atom -> atom
