(* Introduction of the if-then-else operator instead of disjunctions.
 *
 * TODO: consider n-ary disjunctions?
 *
 * Author: Tomas Dacik (idacik@fit.vut.cz), 2024 *)

let is_conflict atom1 atom2 = match SL.view atom1, SL.view atom2 with
  | SL.Eq [x1; x2], SL.Distinct [x1'; x2'] -> SL.Term.equal x1 x1' && SL.Term.equal x2 x2'
  | SL.Distinct [x1; x2], SL.Eq [x1'; x2'] -> SL.Term.equal x1 x1' && SL.Term.equal x2 x2'

let drop_non_atoms phi = match SL.view phi with
  | Or xs -> SL.mk_or @@ List.filter SL.is_atom xs

let apply =
  SL.map_view (function
    | Or [lhs; rhs] ->
      let pure1, spatial1 = SL.as_symbolic_heap @@ drop_non_atoms lhs in
      let pure2, spatial2 = SL.as_symbolic_heap @@ drop_non_atoms rhs in
      let conflicts =
        BatList.cartesian_product pure1 pure2
        |> List.filter (fun (x1, x2) -> is_conflict x1 x2)
      in
      if List.is_empty conflicts
      then SL.mk_or [lhs; rhs] (* No change *)
      else
        let p1 = List.filter (fun x -> not @@ BatList.mem_cmp SL.compare x @@ List.map fst conflicts) pure1 in
        let p2 = List.filter (fun x -> not @@ BatList.mem_cmp SL.compare x @@ List.map snd conflicts) pure2 in
        let lhs = SL.mk_star (p1 @ spatial1) in
        let rhs = SL.mk_star (p2 @ spatial2) in
        SL.mk_ite (SL.mk_or @@ List.map fst conflicts) lhs rhs
  )
