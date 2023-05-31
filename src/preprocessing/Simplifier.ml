(* Basic simplifcation of SSL formulae
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2023 *)

open SSL

let is_emp phi = match phi with
  | SSL.Pure (SMT.True) -> true
  | phi -> SSL.is_emp phi

let is_false phi = match phi with
  | SSL.Pure (SMT.False) -> true
  | phi -> SSL.is_false phi

let fold psis =
  let stars, others = List.partition (function Star _ -> true | _ -> false) psis in
  let stars_operands = List.concat @@ List.map (function Star psis -> psis) stars in
  stars_operands @ others

let rec fold_exists = function
  | Exists (xs, psi) ->
    let xs', phi' = fold_exists psi in
    (xs @ xs', phi')
  | phi -> ([], phi)

(** Remove duplicated pure operands of star *)
let remove_duplicate_pure psis =
  snd @@ List.fold_left
    (fun (pures, res) psi ->
      if SSL.is_pure psi && BatList.mem_cmp SSL.compare psi pures then (pures, res)
      else if SSL.is_pure psi then (psi :: pures, psi :: res)
      else (pures, psi :: res)
    ) ([], []) psis

(** Remove useless variables in quantifiers *)
let remove_useless =
  let filter xs phi =
    let free = SSL.free_vars phi in
    List.filter (fun x -> BatList.mem_cmp SSL.compare x free) xs
  in
  SSL.map
    (fun phi -> match phi with
      | SSL.Forall (xs, psi) -> SSL.mk_forall (filter xs psi) psi
      | SSL.Exists (xs, psi) -> SSL.mk_exists (filter xs psi) psi
      | phi -> phi
    )

(** Exported simplification functions **)

let fold_stars = SSL.map (function Star psis -> SSL.mk_star @@ fold psis | psi -> psi)


let rec simplify phi = match phi with
  | Emp -> Emp
  | Var x -> phi
  | Pure t -> phi (* TODO: simplification using SMT *)
  | Eq xs -> if List_utils.all_equal SSL.equal xs then mk_emp () else phi
  | Distinct xs -> if List_utils.all_equal SSL.equal xs then mk_false () else phi
  | PointsTo (x, ys) -> phi
  | LS (x, y) -> if equal x y then mk_emp () else phi
  | DLS (x, y, f, l) ->
    if equal x l && equal y f then mk_emp ()
    else if equal x l then SSL.mk_eq y f
    else if equal y f then SSL.mk_eq x l
    else phi
  | NLS (x, y, z) -> SSL.mk_nls x y z
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
    (*if equal psi1 psi2 then mk_false ()
    else*) if is_false psi1 then mk_false ()
    else if is_true psi2 then mk_false ()
    else if is_true psi1 then mk_not psi2
    else if is_false psi2 then psi1
    else GuardedNeg (psi1, psi2)

  | Star psis ->
    let psis =
      fold @@ List.map simplify psis
      |> remove_duplicate_pure
    in
    if List.exists is_false psis then mk_false ()
    else SSL.mk_star @@ List.filter (fun psi -> not @@ is_emp psi) psis
  | Not psi -> phi

  | Exists (xs, psi) ->
    let phi = remove_useless phi in
    let xs, psi = fold_exists phi in
    let psi = simplify psi in
    if is_true psi || is_false psi then psi
    else SSL.mk_exists xs psi

  | Forall (xs, psi) ->
    let phi = remove_useless phi in
    let psi = simplify psi in
    if is_true psi || is_false psi then psi
    else SSL.mk_forall xs psi
  | Septraction (psi1, psi2) -> Septraction (simplify psi1, simplify psi2) (* TODO *)
