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

and apply phi =
  SSL.map
    (fun phi -> match phi with
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
      | phi -> phi
    ) phi
