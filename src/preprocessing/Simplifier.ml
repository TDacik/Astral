(* Simplifcation
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2023 *)

open SSL

let rec preprocess phi = match phi with
  | Var x -> phi
  | Pure t -> phi (* TODO: Simplifcation using SMT? *)
  | Eq (x, y) -> if equal x y then mk_emp () else phi
  | Neq (x, y) -> if equal x y then mk_not @@ mk_emp () else phi
  | PointsTo (x, ys) -> phi
  | LS (x, y) -> if equal x y then mk_emp () else phi
  | DLS (x, y, f, l) -> phi (* TODO: ... *)
  | SkipList (depth, x, y) -> phi
  | And (psi1, psi2) ->
    let psi1 = preprocess psi1 in
    let psi2 = preprocess psi2 in
    if equal psi1 psi2 then psi1
    else if is_true psi1 then psi2
    else if is_true psi2 then psi1
    else if is_false psi1 || is_false psi2 then mk_false ()
    else And (psi1, psi2)

  | Or (psi1, psi2) ->
    let psi1 = preprocess psi1 in
    let psi2 = preprocess psi2 in
    if equal psi1 psi2 then psi1
    else if is_false psi1 then psi2
    else if is_false psi2 then psi1
    else if is_true psi1 || is_true psi2 then mk_true ()
    else Or (psi1, psi2)

  | GuardedNeg (psi1, psi2) ->
    let psi1 = preprocess psi1 in
    let psi2 = preprocess psi2 in
    if equal psi1 psi2 then mk_false ()
    else if is_false psi1 then mk_false ()
    else if is_true psi2 then mk_false ()
    else if is_true psi2 then mk_false ()
    else if is_false psi2 then psi1
    else GuardedNeg (psi1, psi2)

  | Star (psi1, psi2) ->
    let psi1 = preprocess psi1 in
    let psi2 = preprocess psi2 in
    if is_false psi1 || is_false psi2 then mk_false ()
    else if is_emp psi1 then psi2
    else if is_emp psi2 then psi1
    else Star (psi1, psi2)

  | Not psi -> phi
  | Exists (xs, psi) -> Exists (xs, preprocess psi)
  | Forall (xs, psi) -> Forall (xs, preprocess psi)
  | Septraction (psi1, psi2) -> Septraction (preprocess psi1, preprocess psi2) (* TODO *)

