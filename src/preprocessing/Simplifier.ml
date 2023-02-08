(* Basic simplifcation of SSL formulae
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2023 *)

open SSL

let rec simplify phi = match phi with
  | Var x -> phi
  | Pure t -> phi (* TODO: simplification using SMT *)
  | Eq xs -> if List_utils.all_equal SSL.equal xs then mk_emp () else phi
  | Distinct xs -> if List_utils.all_equal SSL.equal xs then mk_false () else phi
  | PointsTo (x, ys) -> phi
  | LS (x, y) -> if equal x y then mk_emp () else phi
  | DLS (x, y, f, l) -> if equal x l || equal y f then mk_emp () else phi
  | And (psi1, psi2) ->
    let psi1 = simplify psi1 in
    let psi2 = simplify psi2 in
    if is_false psi1 || is_false psi2 then mk_false ()
    else if equal psi1 psi2 then psi1
    else if is_true psi1 then psi2
    else if is_true psi2 then psi1
    else And (psi1, psi2)

  | Or (psi1, psi2) ->
    let psi1 = simplify psi1 in
    let psi2 = simplify psi2 in
    if is_true psi1 || is_true psi2 then mk_true ()
    else if equal psi1 psi2 then psi1
    else if is_false psi1 then psi2
    else if is_false psi2 then psi1
    else Or (psi1, psi2)

  | GuardedNeg (psi1, psi2) ->
    let psi1 = simplify psi1 in
    let psi2 = simplify psi2 in
    if equal psi1 psi2 then mk_false ()
    else if is_false psi1 then mk_false ()
    else if is_true psi2 then mk_false ()
    else if is_true psi1 then mk_not psi2
    else if is_false psi2 then psi1
    else GuardedNeg (psi1, psi2)

  | Star (psi1, psi2) ->
    let psi1 = simplify psi1 in
    let psi2 = simplify psi2 in
    if is_false psi1 || is_false psi2 then mk_false ()
    else if is_emp psi1 then psi2
    else if is_emp psi2 then psi1
    else Star (psi1, psi2)

  | Not psi -> phi
  | Exists (xs, psi) -> Exists (xs, simplify psi)
  | Forall (xs, psi) -> Forall (xs, simplify psi)
  | Septraction (psi1, psi2) -> Septraction (simplify psi1, simplify psi2) (* TODO *)
