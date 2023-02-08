(* Preprocessing of formulas from SL competition
 *
 * 1. Astral use semantics where (dis)equalities can be satisfied on empty heaps only.
 *    For example formula x != y /\ x -> y is not satisfiable under this semantics.
 *
 *    For SL-comp we use heurstic that replace all concjunctions where one of their
 *    operand is pure (equality or disequality) by separating conjunction.
 *
 * Author: Tomas Dacik (xdacik00@fit.vutbr.cz), 2022 *)

open SSL

let is_pure phi = match phi with
  | Eq _ | Neq _ -> true
  | _ -> false

let rec preprocess phi = match phi with
  | And (psi1, psi2) ->
    if is_pure psi1 then Star (preprocess psi1, preprocess psi2)
    else if is_pure psi2 then Star (preprocess psi1, preprocess psi2)
    else And (preprocess psi1, preprocess psi2)
  | Or (psi1, psi2) -> Or (preprocess psi1, preprocess psi2)
  | Not psi -> Not (preprocess psi)
  | GuardedNeg (psi1, psi2) -> GuardedNeg (preprocess psi1, preprocess psi2)
  | Star (psi1, psi2) -> Star (preprocess psi1, preprocess psi2)
  | Septraction (psi1, psi2) -> Septraction (preprocess psi1, preprocess psi2)
  | Eq _ | Neq _ | LS _ | PointsTo _ -> phi
