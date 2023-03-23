(* Pushing quantifiers deeper to formula to reduce their scope
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2023 *)

let partition x =
  List.partition
    (fun psi ->
      let free = SSL.free_vars psi in
      BatList.mem_cmp SSL.compare x free
    )

let rec push_star_aux xs psis = match xs with
  | [] -> SSL.mk_star psis
  | x :: xs ->
    match partition x psis with
    | plus, [] -> SSL.mk_exists [x] (push_star_aux xs plus)
    | [], minus -> push_star_aux xs minus
    | plus, minus ->
      let phi_minus = push_star_aux xs minus in
      let phi_plus = push_star_aux xs plus in
      SSL.mk_star [apply @@ SSL.mk_exists [x] phi_plus; phi_minus]

and apply phi = match phi with
  | SSL.Var _ | SSL.Pure _ | SSL.Eq _ | SSL.Distinct _ -> phi
  | SSL.PointsTo _ | SSL.LS _ | SSL.DLS _ | SSL.NLS _ -> phi

  (* Forall over conjunction *)
  | SSL.Forall (xs, And (psi1, psi2)) ->
    SSL.mk_and [apply @@ SSL.mk_forall xs psi1; apply @@ SSL.mk_forall xs psi2]

  (* Exists over disjunction *)
  | SSL.Exists (xs, Or (psi1, psi2)) ->
    SSL.mk_or [apply @@ SSL.mk_exists xs psi1; apply @@ SSL.mk_exists xs psi2]

  (* Exists over star *)
  | SSL.Exists (xs, Star psis) -> push_star_aux xs psis

  (* Forall over star
  | SSL.Forall (xs, Star (psi1, psi2)) ->
    SSL.mk_star [apply @@ SSL.mk_forall xs psi1; apply @@ SSL.mk_forall xs psi2]
  *)

  | SSL.And (psi1, psi2) -> SSL.mk_and [apply psi1; apply psi2]
  | SSL.Or (psi1, psi2) -> SSL.mk_or [apply psi1; apply psi2]
  | SSL.Not psi -> SSL.mk_not (apply psi)
  | SSL.GuardedNeg (psi1, psi2) -> SSL.mk_gneg (apply psi1) (apply psi2)
  | SSL.Star psis -> SSL.mk_star @@ List.map apply psis
  | SSL.Septraction (psi1, psi2) -> SSL.mk_septraction (apply psi1) (apply psi2)
  | SSL.Forall (xs, psi) -> SSL.mk_forall xs (apply psi)
  | SSL.Exists (xs, psi) -> SSL.mk_exists xs (apply psi)
