(* Preprocessing of pure terms in SSL formulae.
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2022 *)

open SSL
open SMT

let rec purify = function
  | SSL.Pure t -> t
  | SSL.Eq (Pure x, Pure y) -> Boolean.mk_eq x y
  | SSL.Neq (Pure x, Pure y) -> Boolean.mk_neq x y
  | SSL.And (psi1, psi2) -> Boolean.mk_and [purify psi1; purify psi2]
  | SSL.Or (psi1, psi2) -> Boolean.mk_or [purify psi1; purify psi2]
  | SSL.GuardedNeg (psi1, psi2) -> Boolean.mk_and [purify psi1; Boolean.mk_not @@ purify psi2]
  | SSL.Not psi -> Boolean.mk_not @@ purify psi
  | other -> failwith @@ SSL.show other

let purify phi = SSL.mk_pure @@ purify phi

let rec preprocess phi = match phi with
  | SSL.Eq (Pure x, Pure y) -> purify phi
  | SSL.Neq (Pure x, Pure y) -> purify phi
  | SSL.And (psi1, psi2) ->
      if SSL.is_pure psi1 && SSL.is_pure psi2 then purify phi
      else SSL.mk_and [preprocess psi1; preprocess psi2]
  | SSL.Or (psi1, psi2) ->
      if SSL.is_pure psi1 && SSL.is_pure psi2 then purify phi
      else SSL.mk_or [preprocess psi1; preprocess psi2]
  | SSL.Not psi ->
      if SSL.is_pure psi then purify phi
      else SSL.mk_not (preprocess psi)
  | SSL.GuardedNeg (psi1, psi2) ->
      if SSL.is_pure psi1 && SSL.is_pure psi2 then purify phi
      else SSL.mk_gneg (preprocess psi1) (preprocess psi2)
  | SSL.Star (psi1, psi2) -> SSL.mk_star [preprocess psi1; preprocess psi2]
  | SSL.Septraction (psi1, psi2) -> SSL.mk_septraction (preprocess psi1) (preprocess psi2)
  | atom -> atom
