(* Introduction of the if-then-else operator instead of disjunctions.
 *
 * TODO: consider n-ary disjunctions?
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2024 *)

let is_contradiction atom1 atom2 = match SL.view atom1, SL.view atom2 with
  | Eq xs, Distinct ys
  | Distinct xs, Eq ys -> List.equal SL.Term.equal xs ys
  | _ -> false

let rec as_atoms phi = match SL.view phi with
  | SL.Star atoms -> [], atoms
  | SL.Exists (xs, body) ->
    let qs, atoms = as_atoms body in
    xs @ qs, atoms
  | _ when SL.is_atom phi -> [], [phi]

let apply =
  SL.map_view (function
    | Or [lhs; rhs] when List.for_all SL.is_symbolic_heap [lhs; rhs] ->
      let qs1, atoms1 = as_atoms lhs in
      let qs2, atoms2 = as_atoms rhs in
      try
        let c1, c2 =
          BatList.cartesian_product atoms1 atoms2
          |> List.find (fun (a1, a2) -> is_contradiction a1 a2)
        in
        let lhs' = SL.mk_exists qs1 @@ SL.mk_star @@ BatList.remove_if (SL.equal c1) atoms1 in
        let rhs' = SL.mk_exists qs2 @@ SL.mk_star @@ BatList.remove_if (SL.equal c2) atoms2 in
        SL.mk_ite c1 lhs' rhs'
      with Not_found -> SL.mk_or [lhs; rhs]
  )

let apply_ctx ctx =
  let open Context in
  {ctx with phi = apply ctx.phi}
