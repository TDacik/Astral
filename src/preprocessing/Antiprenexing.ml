(* Pushing quantifiers deeper to formula to reduce their scope
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2023 *)

let partition x =
  List.partition (fun psi ->
    let free = SL.free_vars psi in
    BatList.mem_cmp SL.Variable.compare x free
  )

let push_stars xs psis =
  let phi, rest = List.fold_left (fun (acc, psis) x ->
    let with_x, without_x = partition x psis in
    let lhs = SL.mk_exists [x] @@ SL.mk_star with_x in
    let phi = SL.mk_star [acc; lhs] in
    (phi, without_x)
  ) (SL.emp, psis) xs
  in
  SL.mk_star (phi :: rest)

let apply_forall xs phi = match SL.view phi with
  | SL.And psis -> SL.mk_and @@ List.map (SL.mk_forall xs) psis
  | _ -> phi

let apply_exists xs phi = match SL.view phi with
  | SL.Or psis -> SL.mk_or @@ List.map (SL.mk_exists xs) psis
  | SL.Star psis -> push_stars xs psis

let apply =
  SL.map_view (function
    | SL.Forall (xs, psi) -> apply_forall xs psi
    | SL.Exists (xs, psi) -> apply_exists xs psi
  )
